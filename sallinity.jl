
# set up packages:
using Pkg
Pkg.activate(".")
Pkg.add("RCall")
#Pkg.add(PackageSpec(url="https://github.com/esa-esdl/ESDL.jl"))
#Pkg.add(PackageSpec(url="https://github.com/esa-esdl/ESDLPlots.jl"))
Pkg.add("ColorSchemes");
Pkg.add("Plots");
Pkg.add("Statistics")
Pkg.add("NetCDF")
Pkg.add("FFTW")
Pkg.add("DataFrames")
Pkg.add("Polynomials")

using RCall, Plots, ColorSchemes;
using Statistics;
using NetCDF;
using FFTW;
using DataFrames
using ESDL;
using Polynomials: fit


path = "Documents/Projects/DATA/ESA_ECV_ocean_salinity/"

Pkg.installed()["RCall"]

pwd()
cd("/Users/juanrocha/Documents/Projects/DATA/ESA_ECV_ocean_salinity/")

R"""
library(tidyverse)
fls <- fs::dir_ls(recurse = TRUE) |>
    str_subset( pattern = "nc$")
"""

@rget fls

## This formally reduces the computation to weekly observations:
fls = fls[1:7:length(fls)]

# this only gives me the files on the current directory, not recursively
# fls = readdir()
#
# fls = run(`ls -R `)
# read(fls, String)

## This is a lazy AbstractArray that does not load data on memory
c = NetCDF.open(fls[1])
ncinfo(fls[1])
## This is the data:
t = @timed dat = ncread(fls[1], "sss"); # 0.14s
t.time
## Extract lon and lat
lon = ncread(fls[1], "lon");
lat = ncread(fls[1], "lat");
size(dat)

m = map(!isnan, dat)

# it will be useful to set missing values for later
function set_missing(x)
    ## this one is for boolean, but I need one for nans.
    if x == 0
        x = missing
    else
        x
    end
    return(x)
end

function set_missing_nan(x)
    ## this one is for boolean, but I need one for nans.
    if isnan(x)
        x = missing
    else
        x
    end
    return(x)
end


m1 = map(set_missing, m)
m[250,250] == 1

## The bottle neck is reading over and over the files.  For each pixel time series, reading all 522 files takes over 11s. An alternative approach is to create the cube on memory:

# initialize one pixel
# pxl_ts = [0.0]
# # read the pixel in all the NetCDF files
# t = @timed for i in 1:length(fls)
#   pxl = ncread(fls[i], "sss", start = [250,250,1], count = [1,1,1])
#   pxl_ts = push!(pxl_ts, pxl[1,1,1])
# end
# @time deleteat!(pxl_ts, 1)
# @time fft(pxl_ts)

# initiallize the array
A = Array{Union{Float32,Missing}}(missing, 1388, 584, 522)

# 14secs, A is 1.9Gb! varinfo()
t = @timed for i in 1:length(fls)
    A[:,:,i] = ncread(fls[i], "sss")
end

A = map(set_missing_nan, A)



## testing: the problem with mapslices is that the result is a matrix
# mapslices(skipmissing ∘ )
T = A[20:30, 20:30, :]



@time l = size(A)[3]
@time outar = Matrix(undef, l, 4)

## plans for the Fourier transform
testar = zeros(Complex{Base.nonmissingtype(eltype(A[250,250,:]))}, l)
fftplan = plan_fft!(testar)
ifftplan = inv(fftplan)

#### fourier transform: inspiration from ESDL::filterTSFFT
## https://github.com/esa-esdl/ESDL.jl/blob/master/src/TSDecomposition.jl
function linreg(x,y)
  b = cov(x,y)/var(x)
  a = mean(y) - b*mean(x)
  a,b
end

function detrendTS!(outar::AbstractMatrix,xin::AbstractVector)
    x=collect(eachindex(xin))
    a,b=linreg(x,xin)
    for i in eachindex(xin)
        outar[i,1]=a+b*x[i]
    end
end

function tscale2ind(b::Float64,l::Int)
    i=round(Int,l/b+1)
    return i
end
mirror(i,l)=l-i+2

function filterFFT(outar, y, annfreq::Int=52, nharm::Int=3)
    detrendTS!(outar, y)
    fy = Complex{Base.nonmissingtype(eltype(y))}[y[i] - outar[i,1] for i=1:l]
    fftplan * fy # changes fy inplace
    fyout = similar(fy) # creates a new element array with same size
    czero = zero(eltype(fy))

    # remove annual cycle
    fill!(fyout, zero(eltype(fyout)))

    for jharm = 1:nharm
           iup=tscale2ind(annfreq*1.1/jharm,l)

           idown=tscale2ind(annfreq*0.9/jharm,l)

           for i=iup:idown
               fyout[i]  = fy[i]
               i2        = mirror(i,l)
               fyout[i2] = fy[i2]
               fy[i]     = czero
               fy[i2]    = czero
           end
       end

    ifftplan * fyout
    for i=1:l
        outar[i,3]=real(fyout[i])
    end

    iup   = tscale2ind(annfreq*1.1,l)
    idown = tscale2ind(annfreq*0.9,l)
    #Now split the remaining parts
    fill!(fyout,czero)
    for i=2:iup-1
        i2        = mirror(i,l)
        fyout[i]  = fy[i]
        fyout[i2] = fy[i2]
        fy[i]     = czero
        fy[i2]    = czero
    end
    ifftplan * fyout
    ifftplan * fy

    for i=1:l
        outar[i,2]=real(fyout[i])
        outar[i,4]=real(fy[i])
    end

end


@time filterFFT(outar, A[250,250,:])
#
# t = @timed df_all = DataFrame(lon = lon[250], lat = lat[250],
#    sss_slope = linreg(1:l, pxl_ts)[2],
#    sss_mean = mean(pxl_ts),
#    sss_var = var(pxl_ts),
#    sss_std_longterm = std(outar[:,2], mean=0),
#    sss_std_annual_cycle = std(outar[:,3], mean = 0),
#    sss_std_fast_oscillations = std(outar[:,4], mean = 0)
#    )
idx = 1 # counter
# precompute the computations worth doing, with few missing values
z = mapslices(sum, ismissing.(A), dims = [3])
# old conditional: ((!ismissing(T[i,j,:])) & (T[i,j,:] .|> ismissing |> sum < 100))
sum(z .< 20 )
response = Matrix(undef,  sum(z .< 20 ), 8)

t = @timed for i in 1:size(A)[1], j in 1:size(A)[2] # i = lons, j = lats
  if z[i,j] < 20
        outar = Matrix(undef, l, 4)
        x = gapfillpoly2(A[i,j,:])

        filterFFT(outar, x)

        response[idx,1] = lon[i] # longitud
        response[idx,2] = lat[j] # latitude
        response[idx,3] = linreg(1:l, x)[2] # sss_slope
        response[idx,4] = (mean ∘ skipmissing)(x) # sss_mean
        response[idx,5] = (var ∘ skipmissing)(x) # sss_var
        response[idx,6] = (std ∘ skipmissing)(outar[:,2]) # sss_std_longterm
        response[idx,7] = (std ∘ skipmissing)(outar[:,3]) # sss_std_annual_cycle
        response[idx,8] = (std ∘ skipmissing)(outar[:,4]) # sss_std_fast_oscillations
        idx += 1
  end
end
## It took ~24h to compllete almost 12k out of 505k. Cannot afford that
## with new approach

## save the results

R"""
df_all <- tibble(
    lon = unlist( $response[,1]),
    lat = unlist( $response[,2]),
    sss_slope = unlist( $response[,3]),
    sss_mean = unlist( $response[,4]),
    sss_var = unlist( $response[,5]),
    sss_std_longterm = unlist( $response[,6]),
    sss_std_annual_cycle = unlist( $response[,7]),
    sss_std_fast_oscillations = unlist( $response[,8])
)
"""

R"""
setwd("/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL")
save(
  df_all,
  file = "Results/sampled_FFT_variables/sea_surface_salinity_FFT_.RData")
"""



x = Matrix(undef, size(A)[1], size(A)[2])
for i in 1:size(A)[1], j in 1:size(A)[2]
    if ismissing(A[i,j,:])
        x[i,j] = missing
    else
        x[i,j] = A[i,j,:] .|> ismissing |> sum
    end
end




## Additional unused code:

#### Some R manipulations
#
# R"tibble(ts = $pxl_ts, time = seq_along($pxl_ts)) %>% ggplot(aes(time, ts)) + geom_line() "
#
# R"""
# x <- as_tibble($dat)
# colnames(x) <- $lat
# x <- x %>% add_column(lon = $lon, .before = 1)
# x <- x |> pivot_longer(cols = 2:last_col(), names_to = "lat", values_to = "sss") |>
# mutate(lat = as.numeric(lat))
# """
#
# R"x"
#
# R"x |> ggplot(aes(lon,lat)) + geom_tile(aes(fill = sss))"
#
# # This reduces the dataset to 502k pixels
# R"x <- x |> filter(!is.nan(sss))"
#
# #### index of the lon and lat
# R"which(unique(x$lat) %in% $lat)"


function gapfillpoly2(x;max_gap=30,nbefore_after=10, polyorder = 2)
    x = replace(i->(!ismissing(i) && isfinite(i)) ? i : missing,x)
    a = copy(x)
    workx = Float64[]
    worky = Float64[]
    # find first nonmissing value
    idxstart = findfirst(!ismissing,a)
    idxstart === nothing && return a
    while true
        #find next missing value
        gapstart = findnext(ismissing,a,idxstart)
        gapstart === nothing && break
        gapstop = findnext(!ismissing,a,gapstart)
        gapstop === nothing && break
        if gapstop-gapstart < max_gap
            idxfirst = max(1,gapstart - nbefore_after)
            idxlast  = min(length(a), gapstop + nbefore_after - 1)
            idxr = idxfirst:idxlast
            for (ii,idx) in enumerate(idxr)
                if !ismissing(x[idx])
                    push!(workx,idxr[ii])
                    push!(worky,x[idx])
                end
            end
            if length(workx)>polyorder
                p = fit(workx,worky,polyorder)
                for idx in gapstart:(gapstop-1)
                    a[idx] = p(idx)
                end
            end
        end
        idxstart = gapstop
        empty!(workx)
        empty!(worky)
    end
    a
    # added this line to correct NAs on the ends of the vector
    y = replace(i->(!ismissing(i) && isfinite(i)) ? i : (mean ∘ skipmissing)(a),a)
    return y
end

@time x = gapfillpoly!(T[5,5,:])

mapslices(sum, ismissing.(T) ; dims = [3])

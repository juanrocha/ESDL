# Here are leftovers from  my previous attempts to run the EWS from Julia, including programming the gaussian filter rolling windows.


#Program the gaussian filter as a function and run it before calculating the EWS. Note here that the \sigma is the standard deviation of complete time series. The following code doesn't work!
# import Statistics: var, mean, std
# function gaussian_filter(xin, xout)
#     sd = std(skipmissing(xin))

#     if sd > 0 # std non-zero
#         xout[:].=e^(.-xin^2 ./(2*sd^2))./ (sd*sqrt(2*pi))
#     else # time series is probably constant / sd == 0, so avoiding division by zero
#         xout[:].=0
#     end
# end


## Define input and output dimensions
indims = InDims("Time")
outdims = OutDims("Time")

# test the function
test = mapCube(
    gaussian_filter,
    subsetcube(d2, Scale = "Fast Oscillations"),
    indims=indims, outdims=outdims)

    # Not sure this is needed. The TS are normalized to 0-mean and 1-variance
# import Statistics: var, mean, std
# std_dev = mapslices(
#     std ∘ skipmissing,
#     subsetcube(d2, Scale = "Fast Oscillations"),
#     dims = "Time")

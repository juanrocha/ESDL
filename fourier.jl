## Fourier transform for vectors inspired on the ESDL version of the function
## https://github.com/esa-esdl/ESDL.jl/blob/master/src/TSDecomposition.jl

using FFTW: plan_fft!
using Statistics: mean, cov, var
using Distributed: workers, remotecall, fetch, myid

## Setting up Julia
## some help from here: https://www.rdocumentation.org/packages/JuliaCall/versions/0.17.4
#install.packages("JuliaCall")
#devtools::install_github("Non-Contradiction/JuliaCall")

library(JuliaCall)
help(package = "JuliaCall")
# export R_LD_LIBRARY_PATH = "$R_LD_LIBRARY_PATH:/Applications/Julia-1.6.app/Contents/Resources/julia/lib/libjulia.dylib"

# options(
#   JULIA_HOME = "/Applications/Julia-1.6.app/Contents/Resources/julia/bin/"
# )

# dyn.load("/Library/Frameworks/R.framework/Versions/4.1/Resources/library/JuliaCall/libs/JuliaCall.so")

## works with Julia 1.1. but not more recent versions.
## see issue here:https://github.com/Non-Contradiction/JuliaCall/issues/129
julia <- julia_setup(
  JULIA_HOME = "/Applications/Julia-1.1.app/Contents/Resources/julia/bin/",
  verbose = TRUE,
  installJulia = FALSE
  #rebuild = TRUE
)

julia$command("a = sqrt(2);"); julia$eval("a")

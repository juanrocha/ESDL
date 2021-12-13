## Carpenter code to calculate time changing eigenvalues from time series
## source: https://github.com/juanrocha/Demo_DynamicLinearModels/blob/master/MLE-AR-DLM_2017-07-01.R
## 
# Function to fit an AR(p) model as a Dynamic Linear Model (DLM) by Maximum Likelihood
# Copyright 2017 by Stephen R. Carpenter
# Modified by Juan Rocha 2021 to skip plotting and speed up a bit the computation
# The DLM, a form of dynamic linear regression, is defined by

# Observation equation is
# Y_t = F_t'*theta_t + eta_t where
# Y_t is the prediction
# F_t is a vector of predictors at the beginning of the time step
# theta_t is the parameter vector
# eta_t is an individual observation error

# System equation is:
# theta_t = theta_t-1 + omega_t
# where theta is defined above and omega_t is an individual process error

# This model converts directly into MARSS format as follows: 
#  x(t) = B(t)*x(t-1) + w(t)
#  y(t) = Z(t)*x(t) + v(t)
# where x(t) = theta_t, B(t) = G(t) = I, Z(t) = F_t, Q=Q, R=r, and
#   u=0, C=0, c=0 a=0, D=0, d=0
# For more information about MARSS see the user guide; the version I used was
#
# Holmes EE, Ward EJ, Scheuerell MD, 2014, Analysis of Multivariate Time Series Using the
# MARSS package. V 3.9. Northwest Fisheries Science Center, NOAA, Seattle, WA, USA
#
# Function call: DLM.MLE(nl,nobs,timevec,X,title) where
#  nl is the number of AR lags, nobs is the number of observations, timevec is the time steps,
#  X is the time series to be analyzed, and title is the title for the plots

# Outputs are a list containing:
#  1 = matrix containing: time step, Y, yhat (one-step prediction); 
#       Dimension is (nobs-nl)x3 where nobs is number of observations and nl is number of lags
#  2 = (nobs-nl)x4 matrix containing eigenvalue, sd of eigenvalue, eigenvalue + sd, eigenvalue - sd
#  3 = (nl+1)x(nobs-nl) matrix of AR parameter estimates; col 1 is intercept, col 2 is AR(1) coef, etc.
#  4 = (nl+1)x(nobs-nl) matrix of AR parameter standard deviations
#  5 = the full MARSS structure returned by MARSS


library(MARSS)

## J211102: Entering manual values to test line by line the code. 
## Read one csv file an use one lat lon vector
# nl = 1
# nobs = length(time)
# X = matrix(y$gpp, ncol = length(time))

# MLE fit of DLM using MARSS: input nl, nobs, X, timevec
# Modified: dd is the data frame with one time series (one pxl). First lines
# recover nobs, timevec and X
#  *************************************************
DLM.MLE <-  function(nl,dd)  { 
    ## dd <- dat[[1]] # this was just for testing
    nobs = nrow(dd)
    timevec = seq_along(dd$time)
    X = matrix(dd$gpp_1d, nrow = 1) # MARSS expect data as matrix with time on cols
    
    
    if(nl>8) {
        nl=8
        print('Number of lags nl must be 8 or less; nl set to 8',quote=F)
        print('High order AR fits are likely to have convergence problems',quote=F)
    }
    
    # number of parameters
    p = nl+1 # number of parameters counting intercept
    
    # AR variates
    X.design = matrix(1,nr=(nl+1),nc=(nobs-nl)) # matrix to hold predictors
    for(i in 1:nl) {
        X.design[(i+1),] = X[i:(nobs-nl+i-1)]
    }
    Y = matrix(X[(1+nl):nobs],nr=1,nc=(nobs-nl)) # response
    
    # LS regression
    invXX = solve(X.design%*%t(X.design))
    lm.par = invXX%*%X.design%*%t(Y)
    # Force initials inside unit circle
    lm.inits=lm.par
    lm.inits[2:p] = ifelse(lm.par[2:p]^2 < 1,lm.par[2:p],0.9)
    
    # process equation
    B = diag(p)
    U = matrix(0,nr=p,nc=1) 
    Q = matrix(list(0),p,p)
    vec.of.qs = c('q1','q2','q3','q4','q5','q6','q7','q8','q9')
    for(i in 1:p) {
        Q[i,i] = vec.of.qs[i]
    }
    
    # Observation equation
    Z = array(NA,c(1,p,(nobs-nl))) # first dimension is time, second is parameters, third is time steps
    for(i in 1:p) {
        Z[1,i,] = X.design[i,]
    }
    A = matrix(0)
    R = matrix('r')
    
    # initials
    inits.list = list(x0=matrix(lm.inits, nrow=p))
    
    # model list
    mod.list = list(B=B, U=U, Q=Q, Z=Z, A=A, R=R)
    
    # run DLM
    #tic()
    dlm1 = MARSS(Y, inits=inits.list, model=mod.list,control=list(maxit=5000,trace=1))
    #toc() # this is the bottleneck of the function, takes 386s = 6.5mins
    # extract state estimates
    B.ests = dlm1$states
    # s.e. of state estimates
    B.se = dlm1$states.se
    
    # one-step predictions
    Yhat=rep(0,(nobs-nl))
    for(i in 1:(nobs-nl)){
        Yhat[i] = X.design[,i]%*%B.ests[,i]
    }
    
    epsilon = Yhat-Y # prediction errors
    
    # compute eigenvalues
    lamda = rep(0,(nobs-nl))
    for(i in 1:(nobs-nl)) {
        armat = matrix(0,nr=nl,nc=nl)
        subdiag = rep(1,(nl-1))
        armat[row(armat) == col(armat)+1] = subdiag
        armat[1,] = B.ests[2:p,i]
        eigvals = eigen(armat,only.values=T)
        lamda[i] = max(Mod(eigvals$values))
    }
    
    # Bootstrap eigenvalue errors
    NT = nobs-nl  # number of time steps
    nboot = 100 # number of bootstrap iterations
    lamda.se = rep(0,NT) # vector to hold time series of eigenvalue s.e.
    lamda.temp = rep(0,nboot)  # vector to hold temporary sample of eigenvalues
    # make a matrix with subdiagonal 1
    armat = matrix(0,nr=nl,nc=nl)
    subdiag = rep(1,(nl-1))
    armat[row(armat) == col(armat)+1] = subdiag
    for(i in 1:NT) {  # loop over time steps
        # make a matrix of normal random numbers with nboot rows and nl columns, each column a sample for an AR coef
        ARnorm = mapply(
            function(mu,sigma){
                rnorm(n=nboot,mean=mu,sd=sigma)},
            mu=B.ests[2:p,i],sigma=B.se[2:p,i])
        for(j in 1:nboot)  {  # loop over bootstrap iterations
            armat[1,] = ARnorm[j,]
            eigvals = eigen(armat,only.values=T)
            lamda.temp[j] = max(Mod(eigvals$values))
        }  # end loop over bootstrap iterations
        lamda.se[i] = sd(lamda.temp)
    }  # end loop over timesteps
    
    # Time vector for plotting
    T.ar = 1:(length(time)-nl) #timevec[1:(nobs-nl)]
    
    # Plot 1: general results
    # windows(width=6,height=12)
    # par(mfrow=c(4,1),mar=c(4, 4.3, 2, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
    # plot(T.ar,Yhat,type='l',lwd=3,col='deepskyblue',xlab='DOY',ylab='Y and Yhat',main=title)
    # points(T.ar,Y,type='p',pch=19,col='darkblue')
    # plot(T.ar,epsilon,type='b',pch=19,lwd=2,col='red',xlab='DOY',ylab='one-step error')
    # plot(T.ar,B.ests[1,],type='b',pch=19,lwd=2,col='forestgreen',xlab='DOY',ylab='intercept')
    # eigrange = range(lamda,1)
    # plot(T.ar,lamda,type='b',pch=19,lwd=2,col='blue',xlab='DOY',ylab='eigenvalue',ylim=eigrange)
    # abline(h=1,lty=2)
    
    # # Plot 2: plot the ar coefficients
    # quartz(width=6,height=12)
    # par(mfrow=c((nl),1),mar=c(4, 4.3, 2, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
    # for(i in 2:p) {  # plot AR coefs
    #     plot(T.ar,B.ests[i,],type='l',lwd=2,col='magenta',
    #          ylab=bquote('AR'~ .(i-1) ~'coef'),
    #          xlab='DOY')
    # }
    
    # Plot 3: eigenvalue plus error
    # lamda.plus = lamda+lamda.se
    # lamda.minus = lamda-lamda.se
    
    # par(mfrow=c(1,1),mar=c(4, 4.3, 2, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
    # yrange = range(lamda.plus,lamda.minus,0,1)
    # plot(T.ar,lamda,type='l',lwd=2,col='blue',ylim=yrange,
    #      xlab='DOY',ylab='Eigenvalue +/- SE',main=title)
    # points(T.ar,lamda.plus,type='l',lwd=2,lty=2,col='deepskyblue')
    # points(T.ar,lamda.minus,type='l',lwd=2,lty=2,col='deepskyblue')
    # abline(h=1,lty=2)
    
    # Construct output list
    #Yyhat = matrix(c(T.ar,Y,Yhat),nr=(nobs-nl),nc=3)
    #LamdaMat = matrix(c(lamda,lamda.se,lamda.plus,lamda.minus),nr=(nobs-nl),nc=4)
    lambda_df <- tibble(
        time = dd$time[p:nobs],
        lamda = lamda,
        lamda.se = lamda.se
    )
    #outlist = list(Yyhat,LamdaMat,B.ests,B.se,dlm1)
    dd <- dd |> 
        left_join(lambda_df)
    
    return(dd)
    #J211104: this reduces the size of the output object from 0.5Mb to something more manageable at scale
    
}  # End function for fitting DLM with MLE **********************************************************************

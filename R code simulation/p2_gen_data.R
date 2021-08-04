# this script contains function to create data
# Xtrain 
# Xtest
# Ytrain
# Ytest
# Xscales.train
# Xscales.test
chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))

p2_gen_data <- function(nitems, nscales, r12, sig_items, sig_comps, loadings,
                        meas, exp, ntotal, R2 =.4, ntrain, ntest = 10^4, sig_scales = 1) {

  er <- 1-loadings^2
  cov.z <- matrix(r12, nscales, nscales)
  diag(cov.z) <- 1
  cov.e <- diag(er)
  
  # Get covariance matrix X
  V <- matrix(0,ntotal, nscales)
  for (k in 1:nscales) {
    V[((k-1)*nitems + 1):(k*nitems), k] <- loadings[((k-1)*nitems + 1):(k*nitems)] 
  }
  
  cov.x <- V %*% cov.z %*% t(V) + cov.e
  
  bvalues <- p2_gen_betas(cov.z, cov.x, nitems, nscales, sig_items, sig_comps, sig_scales, R2, ntotal,exp)   # generate bvalues
  
  Ztrain <- mvrnorm(ntrain, mu = rep(0, nscales), Sigma = cov.z) 
  Etrain <- mvrnorm(n = ntrain, mu = rep(0, ntotal), Sigma = cov.e)
  Xtrain <-  Ztrain %*% t(V)   + Etrain 
  colnames(Xtrain) <- paste0("x", 1:ntotal)
  
  EYtrain <- rnorm(ntrain, sd = sqrt(1-R2))
  if(exp == "exp1") {
    Ytruepredtrain <- Ztrain%*%bvalues 
    Ytrain <- Ytruepredtrain + EYtrain
  } else {
    Ytruepredtrain <- Xtrain%*%bvalues
    Ytrain <- Ytruepredtrain+ EYtrain
  }
  
  set.seed(1)
  Ztest <- mvrnorm(ntest, mu = rep(0, nscales), Sigma = cov.z) 
  Etest <- mvrnorm(n = ntest, mu = rep(0, ntotal), Sigma = cov.e)
  Xtest <-  Ztest%*%t(V)  + Etest 
  colnames(Xtest) <- paste0("x", 1:ntotal)
  
  EYtest <- rnorm(ntest, sd = sqrt(1-R2))
  if(exp == "exp1") {
    Ytruepredtest <- Ztest%*%bvalues 
    Ytest <- Ytruepredtest + EYtest
  } else {
    Ytruepredtest <- Xtest%*%bvalues 
    Ytest <- Ytruepredtest+ EYtest 
  }
  
  # subscales
  Xscales.train <- matrix(NA, ntrain, nscales)
  Xscales.test <- matrix(NA, ntest, nscales)
  
  for(i in 1:nscales){
    
    Xscales.train[,i] <- rowSums(Xtrain[,((i-1)*nitems +1):(i*nitems)])
    Xscales.test[,i] <- rowSums(Xtest[,((i-1)*nitems +1):(i*nitems)])
    
  }
  
  #  names
  colnames(Xscales.train) <- paste0("X", 1:nscales)
  colnames(Xscales.test) <- paste0("X", 1:nscales)

  return(list(Xtrain = Xtrain, Ytrain = Ytrain, Xtest = Xtest, Ytrain = Ytrain, Ytruepredtrain = Ytruepredtrain,
              Ytest = Ytest,  Ytruepredtest = Ytruepredtest,  Xscales.train = Xscales.train, Xscales.test = Xscales.test))
  
}


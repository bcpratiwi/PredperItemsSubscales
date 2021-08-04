# Rule 1 and 2 using PCovR
# returns : selected features ncomps, alpha level, and MSE
# This method does not perform any feature selection. I control the the space of the alpha parameter to choose from since we know the true structure of the
# predictors

sdn <- function(x) sd(x)*sqrt((length(x)-1))/sqrt(length(x))
p2_pcovr <- function(SimData, aa, nfold = 10){
  
  list2env(SimData, globalenv())

  X.train <- data.frame(Xtrain)
  Y.train <- data.frame(Ytrain)
  m_pcovr <- pcovr(X.train, Y.train, modsel = "sim", weight = aa, fold = nfold)
  
  std <- apply(X.train,2, sdn)
  meanXtrain <- apply(X.train, 2, mean)
  
  Xtrain.std <- scale(X.train, scale = std)
  Ytrain.std <- scale(Y.train, scale = sdn(as.matrix(Y.train)))
  
  Ytest.std <- scale(Ytest, center = mean(as.matrix(Y.train)), scale = sdn(as.matrix(Y.train)))
  Xtest.std <- scale(Xtest, center = meanXtrain, scale = std)
  
  est_pcovr<- pcovr_est(Xtrain.std,
                        Ytrain.std,  
                        r = m_pcovr$R, a = m_pcovr$alpha)
  
  ypred_items   <- (Xtest.std %*% est_pcovr$W %*% est_pcovr$Py) * sdn(as.matrix(Y.train)) + mean(as.matrix(Y.train))
  result_items <- c(m_pcovr$alpha, m_pcovr$R)

  return(list(hyper_PCovR = result_items,
              MSE_PCovR = mean((ypred_items - Ytest)^2),
              ypred_PCovR = ypred_items))
}

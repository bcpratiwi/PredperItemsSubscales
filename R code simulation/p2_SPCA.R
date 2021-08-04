# returns : selected features ncomps and vector of ypredval
# superpc might result in a solution of zero dimensions, 
# to avoid this I will repeat the process of cross validation until I get a non-zero dimensional solution
# the repeats of the cross validation for rule 1 and 2 is saved in the final output
sdn <- function(x) sd(x)*sqrt((length(x)-1))/sqrt(length(x))
p2_SPCA<- function(SimData, nfold = 10) {
  
  list2env(SimData, globalenv())
  
  mydata.train <- list(x = t(scale(Xtrain, scale = apply(Xtrain,2,sdn))), y = Ytrain)
  mydata.test <- list(x = t(scale(Xtest, center = apply(Xtest,2,mean), scale = apply(Xtest,2,sdn))), y = Ytest)
  m_spca <- superpc.train(mydata.train, type = "regression")
  
  cv.m_spca <- superpc.cv(m_spca, mydata.train, n.threshold = 20, n.fold = nfold)  
  thresh <- cv.m_spca$thresholds
  LR <- cv.m_spca$scor 
    
  cv.sol <- t(LR)  
  ind <- which.max(cv.sol)
  
  opt.thresh <- thresh[ifelse(ind%%20 == 0, 20, ind %%20)]
  ncomp <- ifelse( ind <= 20, 1, ifelse( ind <= 40, 2, 3))
  
  comps.train <- superpc.predict(m_spca, data = mydata.train, newdata = mydata.train, 
                                 threshold = opt.thresh, 
                                 n.components =  ncomp)  # compute the component scores?
  
  model.spca <- superpc.fit.to.outcome(m_spca, mydata.train, comps.train$v.pred)  
  coefs <- model.spca$coeftable[, 1]
  
  comps.test <- superpc.predict(m_spca, data = mydata.train, newdata = mydata.test, 
                                threshold = opt.thresh, 
                                n.components =  ncomp) 
  ypred_items <- cbind(1, comps.test$v.pred) %*% coefs 
  return(list(hyper_SPCA =c(opt.thresh, ncomp), 
              which.items = comps.test$which.features,
              MSE_SPCA = mean((ypred_items - Ytest)^2),
              ypred_SPCA = ypred_items))
  
}

























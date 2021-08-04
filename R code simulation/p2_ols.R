# perform ordinary least squares using items or subscales as predictors
# return coefficients using items or subscales
# prediction error based on rule using items and subscales


p2_ols <- function(SimData) {
  
    list2env(SimData, globalenv())
  
    mydata.train <- cbind.data.frame(Ytrain, Xscales.train)
    names(mydata.train)[1] <- "y"
    mydata.test <- cbind.data.frame(Ytest, Xscales.test)
    names(mydata.test)[1] <- "y"
    
    m_scales_ols <- lm(y ~ ., mydata.train)
    ypred_scales_ols <- predict(m_scales_ols, newdata = mydata.test)

  
    return(list(coefs_scales_ols = coef(m_scales_ols),
                MSE_ols_scales = mean((ypred_scales_ols - Ytest)^2),
                ypred_ols_scales = ypred_scales_ols))
}
















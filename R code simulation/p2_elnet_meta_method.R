p2_elnet_meta_method <-function(SimData, nfold = 10, alphas = seq(0,1,.1)){
  
  list2env(SimData, globalenv())
  
  # Subscale rules
  set.seed(1)
  results_utils <- cva.glmnet(x = Xscales.train, y = Ytrain, alpha = alphas, nfolds = nfold)
  utils_lambda <- lapply(results_utils$modlist, `[[`, "lambda")
  utils_cvm <- lapply(results_utils$modlist, `[[`, "cvm")
  utils_alpha <- as.list(seq(0,1,.1))
  j <- 1
  temp <- data.frame(alpha = 0, lambda = 0, cvm = 0)
  while(j <= length(alphas)) {
    dat <- cbind.data.frame(alpha = utils_alpha[[j]],
                            lambda = utils_lambda[[j]],
                            cvm = utils_cvm[[j]])
    temp <- rbind.data.frame(temp,dat)
    j <- j + 1
  }
  utils_scales <- temp[-1,]
  utils_scales$input <- "Subscales"
  winnerModel.scales <- utils_scales[which.min(utils_scales$cvm),]
  m_elnet_scales <- glmnet(Xscales.train, Ytrain, alpha = winnerModel.scales$alpha)
  ypredelnet_scales <- predict(m_elnet_scales, s = winnerModel.scales$lambda, newx = Xscales.test)
  
  # Item rules
  set.seed(1)
  results_utils <- cva.glmnet(x = Xtrain, y = Ytrain, alpha = alphas, nfolds = nfold)
  utils_lambda <- lapply(results_utils$modlist, `[[`, "lambda")
  utils_cvm <- lapply(results_utils$modlist, `[[`, "cvm")
  utils_alpha <- as.list(seq(0,1,.1))
  j <- 1
  temp <- data.frame(alpha = 0, lambda = 0, cvm = 0)
  while(j <= length(alphas)) {
    dat <- cbind.data.frame(alpha = utils_alpha[[j]],
                            lambda = utils_lambda[[j]],
                            cvm = utils_cvm[[j]])
    temp <- rbind.data.frame(temp,dat)
    j <- j + 1
  }
  utils_items <- temp[-1,]
  utils_items$input <- "Items"
  winnerModel.items <- utils_items[which.min(utils_items$cvm),]
  m_elnet_items <- glmnet(Xtrain, Ytrain, alpha = winnerModel.items$alpha)
  
  ypredelnet_items <- predict(m_elnet_items, s = winnerModel.items$lambda, newx = Xtest)
  
  
  # meta method simply choose which of the above solution has minimum error
  winnerModel <- rbind.data.frame(winnerModel.scales, winnerModel.items)
  winnerModel <- winnerModel[which.min(winnerModel$cvm),]
  if(winnerModel$input == "Items") {
    ypred_metamethod <- ypredelnet_items
  } else {
    ypred_metamethod <- ypredelnet_scales
  }
  
  # what is the output?
  return(list(hyper_elnet = rbind.data.frame(winnerModel.scales, winnerModel.items), 
              hyper_elnet_items = winnerModel.items,
              coef_elnet_items = predict(m_elnet_items, s = winnerModel.items$lambda, newx = Xtest, type= "coefficients"),
              MSE_elnet_items = mean((ypredelnet_items - Ytest)^2),
              ypred_elnet_items  = ypredelnet_items, 
              hyper_elnet_scales = winnerModel.scales,
              coef_elnet_scales = predict(m_elnet_scales, s = winnerModel.scales$lambda, newx = Xscales.test, type= "coefficients"),
              MSE_elnet_scales = mean((ypredelnet_scales - Ytest)^2),
              ypred_elnet_scales  = ypredelnet_scales,
              hyper_meta = winnerModel,
              MSE_metamethod = mean((ypred_metamethod - Ytest)^2),
              ypred_metamethod = ypred_metamethod))
}
























# Title : Prediction rules for Empirical Examples
# Description : This script contains the analyses performed on the empirical examples
# packages --------
library(ggpubr)
library(RColorBrewer)
library(ggplot2)
library(glmnet)
library(superpc)
library(PCovR)
library(reshape2)
library(superpc)
library(glmnetUtils)

sdn <- function(x) sd(x)*sqrt((length(x)-1))/sqrt(length(x))

# load data  -------
load("thedata")
Y <- thedata[, "Y"]  # criterion
X <- as.matrix(thedata[, "subscale names"]) # X1, X2, X3 .... X_K
items <- as.matrix(thedata[, "item names"]) # x1, x2, x3, ... x_P, P = J*K
N <- nrow(mydata)

reps <- 100
kfold <- 10


# Classcical Approach -----------
MSE_Y_CA_ols <- expand.grid(fold = 1:kfold, reps = 1:reps, 
                                             Method = "ols",  
                                             MSE = 0)
MSE_Y_CA_ols$n <- 0
MSE_Y_CA_ols <- cbind.data.frame(MSE_Y_CA_ols, matrix(NA, nrow(MSE_Y_CA_ols), ncol(X) + 1))
colnames(MSE_Y_CA_ols)[6:ncol(MSE_Y_CA_ols)] <- c("Intercept", colnames(X))

ypredlm = 0
mydata <- cbind.data.frame(Y = Y, X)
for (r in 1:reps){
  set.seed(100*r)
  
  folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  
  for(k in 1:kfold) {
    
    cat("rep = ", r, "fold = ", k, "\r")  
    
    # least squares
    m_lm <- lm(Y ~ ., data = mydata[folds!=k, ] )
    ypredlm[folds==k] <- predict(m_lm, newdata = mydata[folds==k, ])
    
    MSE_Y_CA_ols[MSE_Y_CA_ols$fold == k & 
                                    MSE_Y_CA_ols$reps == r &
                                    MSE_Y_CA_ols$Method == "ols", "MSE"] <- 
      mean((ypredlm[folds==k]- mydata[folds==k, "Y"])^2)
    
    MSE_Y_CA_ols[MSE_Y_CA_ols$fold == k & 
                                    MSE_Y_CA_ols$reps == r &
                                    MSE_Y_CA_ols$Method == "ols", "n"] <- sum(folds==k)
    
    MSE_Y_CA_ols[MSE_Y_CA_ols$fold == k & 
                                    MSE_Y_CA_ols$reps == r &
                                    MSE_Y_CA_ols$Method == "ols", 6:ncol(MSE_Y_CA_ols)] <- coef(m_lm) 
  } #  kfold
  
} # reps 

# OLS on items
MSE_Y_SLT_ols <- expand.grid(fold = 1:kfold, reps = 1:reps, 
                                       Method = "ols",  
                                       MSE = 0, n = 0)

MSE_Y_SLT_ols <- cbind.data.frame(MSE_Y_SLT_ols, matrix(NA, nrow(MSE_Y_SLT_ols), ncol(items)+1))
colnames(MSE_Y_SLT_ols)[6:ncol(MSE_Y_SLT_ols)] <- c("Intercept", colnames(items))


ypredlm = 0
mydata <- cbind.data.frame(Y, items)

for (r in 1:reps){
  set.seed(100*r)
  
  folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  
  for(k in 1:kfold) {
    
    cat("rep = ", r, "fold = ", k, "\r")  
    
    # least squares
    m_lm <- lm(Y ~ ., data = mydata[folds!=k, ] )
    ypredlm[folds==k] <- predict(m_lm, newdata = mydata[folds==k, ])
    
    MSE_Y_SLT_ols[MSE_Y_SLT_ols$fold == k & 
                              MSE_Y_SLT_ols$reps == r &
                              MSE_Y_SLT_ols$Method == "ols", "MSE"] <- 
      mean((ypredlm[folds==k] - mydata[folds==k, "Y"])^2)
    
    MSE_Y_SLT_ols[MSE_Y_SLT_ols$fold == k & 
                              MSE_Y_SLT_ols$reps == r &
                              MSE_Y_SLT_ols$Method == "ols", "n"] <- sum(folds==k) 
    
    MSE_Y_SLT_ols[MSE_Y_SLT_ols$fold == k & 
                              MSE_Y_SLT_ols$reps == r &
                              MSE_Y_SLT_ols$Method == "ols", 6:ncol(MSE_Y_SLT_ols)] <- coef(m_lm) 
  } # kfold
} # reps 

# Elastic net -----------------------
alphas <- seq(0,1,.1)
MSE_Y_CA_elnet <- expand.grid(fold = 1:kfold, reps = 1:reps, Method = "elnet",
                                          MSE = 0, n = 0, alpha = 0, lambda = 0)
MSE_Y_CA_elnet <- cbind.data.frame(MSE_Y_CA_elnet, matrix(NA, nrow(MSE_Y_CA_elnet), ncol(X)+1))
colnames(MSE_Y_CA_elnet)[8:ncol(MSE_Y_CA_elnet)] <- c("Intercept", colnames(X))

ypredelnet <- 0

for (r in 1:reps){
    set.seed(100*r)
    
    folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
    
    for(k in 1:kfold) {
      
      cat("rep = ", r, "fold = ", k,"\r")  
      
      # inner loop  select best lambda min
      set.seed(1)
      results_utils <- cva.glmnet(X[folds!=k,], Y[folds!=k], 
                                  alpha = alphas, nfolds = kfold)
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
      temp <- temp[-1,]
      winnerModel <-  temp[which.min(temp$cvm),]
      m_elnet <- glmnet(X[folds!=k,], Y[folds!=k], alpha = winnerModel$alpha)
      
      ypredelnet <- predict(m_elnet, s = winnerModel$lambda, newx = X[folds==k, ])
      
      MSE_Y_CA_elnet[MSE_Y_CA_elnet$fold == k &
                                  MSE_Y_CA_elnet$reps == r, "MSE"] <- 
        mean((ypredelnet- Y[folds==k])^2)
      MSE_Y_CA_elnet[MSE_Y_CA_elnet$fold == k &
                                  MSE_Y_CA_elnet$reps == r, "n"] <- sum(folds == k)
      MSE_Y_CA_elnet[MSE_Y_CA_elnet$fold == k &
                                  MSE_Y_CA_elnet$reps == r, c("alpha", "lambda")] <- winnerModel[,c("alpha", "lambda")]
      MSE_Y_CA_elnet[MSE_Y_CA_elnet$fold == k &
                                  MSE_Y_CA_elnet$reps == r, 8:ncol(MSE_Y_CA_elnet)] <- predict(m_elnet, s = winnerModel$lambda, newx = X[folds==k, ], type = "coefficients")
      
      } # kfold
} # reps

# SLT Approach ------------------
MSE_Y_SLT_ols <- expand.grid(fold = 1:kfold, reps = 1:reps, 
                                   Method = "ols",  
                                   MSE = 0, n = 0)

MSE_Y_SLT_ols <- cbind.data.frame(MSE_Y_SLT_ols, matrix(NA, nrow(MSE_Y_SLT_ols), ncol(items)+1))
colnames(MSE_Y_SLT_ols)[6:ncol(MSE_Y_SLT_ols)] <- c("Intercept", colnames(items))


ypredlm = 0
mydata <- cbind.data.frame(Y = Y, items)

for (r in 1:reps){
  set.seed(100*r)
  
  folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  
  for(k in 1:kfold) {
    
    cat("rep = ", r, "fold = ", k, "\r")  
    
    # least squares
    m_lm <- lm(Y ~ ., data = mydata[folds!=k, ] )
    ypredlm[folds==k] <- predict(m_lm, newdata = mydata[folds==k, ])
    
    MSE_Y_SLT_ols[MSE_Y_SLT_ols$fold == k & 
                          MSE_Y_SLT_ols$reps == r &
                          MSE_Y_SLT_ols$Method == "ols", "MSE"] <- 
      mean((ypredlm[folds==k] - mydata[folds==k, "Y"])^2)
    
    MSE_Y_SLT_ols[MSE_Y_SLT_ols$fold == k & 
                              MSE_Y_SLT_ols$reps == r &
                              MSE_Y_SLT_ols$Method == "ols", "n"] <- sum(folds==k) 
    
    MSE_Y_SLT_ols[MSE_Y_SLT_ols$fold == k & 
                          MSE_Y_SLT_ols$reps == r &
                          MSE_Y_SLT_ols$Method == "ols", 6:ncol(MSE_Y_SLT_ols)] <- coef(m_lm) 
  } # kfold
} # reps 

save(MSE_Y_SLT_ols, file = "MSE_Y_SLT_ols.Rdata")

# Elastic net on items
MSE_Y_SLT_elnet <- expand.grid(fold = 1:kfold, reps = 1:reps, Method = "elnet",
                                        MSE = 0, n = 0, alpha = 0, lambda = 0)
MSE_Y_SLT_elnet <- cbind.data.frame(MSE_Y_SLT_elnet, matrix(NA, nrow(MSE_Y_SLT_elnet), ncol(items)+1))
colnames(MSE_Y_SLT_elnet)[8:ncol(MSE_Y_SLT_elnet)] <- c("Intercept", colnames(items))

ypredelnet <- 0
for (r in 1:reps){
  set.seed(100*r)
  
  folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  
for(k in 1:kfold) {
    
    cat("rep = ", r, "fold = ", k,"\r")  
    
    # inner loop  select best lambda min
    set.seed(1)
    results_utils <- cva.glmnet(items[folds!=k,], Y[folds!=k], 
                                alpha = alphas, nfolds = kfold)
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
    temp <- temp[-1,]
    winnerModel <-  temp[which.min(temp$cvm),]
    m_elnet <- glmnet(items[folds!=k,], Y[folds!=k], alpha = winnerModel$alpha)
    
    ypredelnet <- predict(m_elnet, s = winnerModel$lambda, newx = items[folds==k, ])
    
    MSE_Y_SLT_elnet[MSE_Y_SLT_elnet$fold == k &
                               MSE_Y_SLT_elnet$reps == r, "MSE"] <- 
      mean((ypredelnet - Y[folds==k])^2)
    MSE_Y_SLT_elnet[MSE_Y_SLT_elnet$fold == k &
                               MSE_Y_SLT_elnet$reps == r, "n"] <- sum(folds == k)
    MSE_Y_SLT_elnet[MSE_Y_SLT_elnet$fold == k &
                               MSE_Y_SLT_elnet$reps == r, c("alpha", "lambda")] <- winnerModel[,c("alpha", "lambda")]
    MSE_Y_SLT_elnet[MSE_Y_SLT_elnet$fold == k &
                               MSE_Y_SLT_elnet$reps == r, 8:ncol(MSE_Y_SLT_elnet)] <- predict(m_elnet, s = winnerModel$lambda, newx = items[folds==k, ], type = "coefficients")
    
  } # kfold
} # reps


# SPCA --------------
MSE_Y_SLT_SPCA <- expand.grid(fold = 1:kfold, reps = 1:reps, 
                                        Method = c("SPCA"),  
                                        MSE = 0, n = 0, threshold = 0, ncomp = 0, varexp = 0, nitems = 0)
MSE_Y_SLT_SPCA <- cbind(MSE_Y_SLT_SPCA, matrix(NA, nrow(MSE_Y_SLT_SPCA), ncol(items)))
colnames(MSE_Y_SLT_SPCA)[10:ncol(MSE_Y_SLT_SPCA)] <- colnames(items)


for (r in 1:reps){
  set.seed(100*r)
  
  folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  
  for(k in 1:kfold) {
    
    cat("rep = ", r, "fold = ", k, "\r")  
    
    mydata.train <- list(x = t(scale(items[folds!=k, ], scale = apply(items[folds!=k, ],2,sdn))), y = Y[folds!=k])
    mydata.test <- list(x = t(scale(items[folds==k, ], center = apply(items[folds!=k, ],2,mean), scale = apply(items[folds!=k, ],2,sdn))), y = Y[folds==k])
    
    m_spca <- superpc.train(mydata.train, type = "regression")
    feat.score <- t(m_spca$feature.scores)
    cv.m_spca <- superpc.cv(m_spca, mydata.train, n.threshold = 20, n.fold = kfold)  
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
    svditemstrain <- svd(mydata.train$x[comps.train$which.features,])
    ypredict <- cbind(1, comps.test$v.pred) %*% coefs 
    
    MSE_Y_SLT_SPCA[MSE_Y_SLT_SPCA$reps == r& MSE_Y_SLT_SPCA$fold == k, "MSE"] <-
      mean((ypredict - mydata.test$y)^2)
    MSE_Y_SLT_SPCA[MSE_Y_SLT_SPCA$reps == r& MSE_Y_SLT_SPCA$fold == k, "n"] <- sum(folds == k)
    MSE_Y_SLT_SPCA[MSE_Y_SLT_SPCA$reps == r& MSE_Y_SLT_SPCA$fold == k, "ncomp"] <-
      ncomp
    MSE_Y_SLT_SPCA[MSE_Y_SLT_SPCA$reps == r& MSE_Y_SLT_SPCA$fold == k, "threshold"] <-
      opt.thresh
    MSE_Y_SLT_SPCA[MSE_Y_SLT_SPCA$reps == r&  MSE_Y_SLT_SPCA$fold == k, "varexp"] <- sum((svditemstrain$d^2/sum(svditemstrain$d^2))[1:ncomp])
    
    MSE_Y_SLT_SPCA[MSE_Y_SLT_SPCA$reps == r& MSE_Y_SLT_SPCA$fold == k, "nitems"] <-
      sum(comps.train$which.features)
    MSE_Y_SLT_SPCA[MSE_Y_SLT_SPCA$reps == r& MSE_Y_SLT_SPCA$fold == k, 10:ncol(MSE_Y_SLT_SPCA)] <- 
      comps.train$which.features
  } # fold
}#reps


# PCovR --------------
MSE_Y_SLT_PCoVR <- expand.grid(fold = 1:kfold, reps = 1:reps, 
                                         Method = c("PCovR"),  
                                         MSE = 0, n = 0, alpha = 0, ncomp = 0)
MSE_Y_SLT_PCoVR <- cbind(MSE_Y_SLT_PCoVR, matrix(NA, nrow(MSE_Y_SLT_PCoVR), ncol(items)))
colnames(MSE_Y_SLT_PCoVR)[8:ncol(MSE_Y_SLT_PCoVR)] <- colnames(items)


for (r in 1:reps){
  set.seed(100*r)
 
  kfold <- 10
  folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  
  for(k in 1:kfold) {
    cat("rep = ", r, "fold = ", k, "\r")  
    tmp <- proc.time()
    X.train <- data.frame(items[folds!= k, ])
    Y.train <- data.frame(Y[folds!=k])
    m_pcovr <- pcovr(X.train, Y.train, modsel = "sim", weight =  seq(from=.01, to=.99, by=.02), fold = kfold)
    std <- apply(X.train,2, sdn)
    meanXtrain <- apply(X.train, 2, mean)
    
    Xtrain.std <- scale(X.train, scale = std)
    Ytrain.std <- scale(Y.train, scale = sdn(as.matrix(Y.train)))
    
    Ytest.std <- scale(Y[folds==k], center = mean(as.matrix(Y.train)), scale = sdn(as.matrix(Y.train)))
    Xtest.std <- scale(items[folds==k,], center = meanXtrain, scale = std)
    
    est_pcovr<- pcovr_est(Xtrain.std,
                          Ytrain.std,  
                          r = m_pcovr$R, a = m_pcovr$alpha)
    
    ypredict   <- (Xtest.std %*% est_pcovr$W %*% est_pcovr$Py) * sdn(Y[folds != k]) + mean(Y[folds != k])

    MSE_Y_SLT_PCoVR[MSE_Y_SLT_PCoVR$reps == r& MSE_Y_SLT_PCoVR$fold == k, "MSE"] <-
      mean((ypredict - Y[folds==k])^2)
    MSE_Y_SLT_PCoVR[MSE_Y_SLT_PCoVR$reps == r& MSE_Y_SLT_PCoVR$fold == k, "n"] <- sum(folds == k)
    
    MSE_Y_SLT_PCoVR[MSE_Y_SLT_PCoVR$reps == r& MSE_Y_SLT_PCoVR$fold == k, "alpha"] <-
      m_pcovr$alpha
    
    MSE_Y_SLT_PCoVR[MSE_Y_SLT_PCoVR$reps == r& MSE_Y_SLT_PCoVR$fold == k, "ncomp"] <-
      m_pcovr$R

  } #folds
}#reps

# Meta method ------
n_outer_folds <- 10
reps <- 100
n <- nrow(mydata)
alphas <- seq(0,1,.1)
N <- nrow(mydata)
Results_hypar_elnet <- expand.grid(fold = 1:n_outer_folds, reps= 1:reps, Method = "elnet", 
                                        input = factor(NA, levels = c("Items", "Subscales")),
                                        alpha = 0, lambda = 0)

MSE_per_rep <- numeric(reps)
for (i in 1:reps) {
  set.seed(100*i)
  folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  outer_partition <- folds
  preds_outer <- numeric(n)
  
  for (outer_index in 1:n_outer_folds) {
    cat("rep = ", i, "outer_id = ", outer_index,"\r")
    idx_out_test <- outer_index == outer_partition
    n_inner <- sum(!idx_out_test)
    outer_train <- thedata[!idx_out_test, ]
    
    n_models <- 2
    # inner results
    # item ------------------------------------
    set.seed(1)
    inner_results_items <- cva.glmnet(as.matrix(outer_train[grep("x",colnames(outer_train))]),
                                      outer_train$Y, alpha = alphas, nfolds = kfold)
    utils_lambda <- lapply(inner_results_items$modlist, `[[`, "lambda")
    utils_cvm <- lapply(inner_results_items$modlist, `[[`, "cvm")
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
    temp <- temp[-1,]
    temp$input <- "Items"
    inner_results_items <- temp
    # subscales  --------------------------
    set.seed(1)
    inner_results_subscales <- cva.glmnet(as.matrix(outer_train[grep("X",colnames(outer_train))]),
                                          outer_train$Y, alpha = alphas, nfolds = kfold)
    utils_lambda <- lapply(inner_results_subscales$modlist, `[[`, "lambda")
    utils_cvm <- lapply(inner_results_subscales$modlist, `[[`, "cvm")
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
    temp <- temp[-1,]
    temp$input <- "Subscales"
    inner_results_subscales <- temp
    inner_results <- rbind.data.frame(inner_results_items, inner_results_subscales)   
    
    winnerModel <- inner_results[which.min(inner_results$cvm), ]
    Results_hypar_elnet[Results_hypar_elnet$reps == i & Results_hypar_elnet$fold== outer_index, c("input", "alpha", "lambda")] <- winnerModel[, c("input", "alpha", "lambda")]
    if (winnerModel$input == "Items") {
      outer_X <- as.matrix(outer_train[,grep("x", colnames(outer_train))]) 
    } else 
    {outer_X <- as.matrix(outer_train[,grep("X", colnames(outer_train))])}
    trainedModel <- glmnet(outer_X, outer_train$Y, alpha = winnerModel$alpha)
    outer_test_X <- as.matrix(thedata[idx_out_test, colnames(outer_X)])
    preds_outer[idx_out_test] <- predict(trainedModel, s= winnerModel$lambda, newx = outer_test_X)
    
  }
  MSE_per_rep[i] <- mean((preds_outer - thedata$Y)^2)
  cat("finish = ", i)
}

# Factor Analysis Regression -----------------------------------------
source("fac_regression.R")
MSE_Y_CA_FAREG <- expand.grid(fold = 1:kfold, reps = 1:reps, 
                                        Method = "FAREG",  
                                        MSE = 0, n = 0)
nscale <- 9
scalenames <- unlist(lapply(strsplit(colnames(X_TOTAL), split = "_"), function(x) x[1]))

MSE_Y_CA_FAREG <- cbind.data.frame(MSE_Y_CA_FAREG, matrix(NA, nrow(MSE_Y_CA_FAREG), nscale+1))
colnames(MSE_Y_CA_FAREG)[6:ncol(MSE_Y_CA_FAREG)] <- c("Intercept", scalenames)


ypred = rep(0, N)
N <- nrow(items)

mydata <- cbind.data.frame(Y = Y, items)

for (r in 1:reps){
  set.seed(100*r)
  
  folds <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  
  for(k in 1:kfold) {
    
    cat("rep = ", r, "fold = ", k, "\r")  
    
    # Factor regression
    CERQ.model <- syntaxmod(colnames(items), nscale, scalenames = scalenames)
    facreg <- fa.reg(mydata$Y[folds!=k], items[folds!=k, ],
                     CERQ.model)
    ypred[folds==k] <- predictfareg(facreg, items[folds==k, ])
    
    
    MSE_Y_CA_FAREG[MSE_Y_CA_FAREG$fold == k & 
                               MSE_Y_CA_FAREG$reps == r &
                               MSE_Y_CA_FAREG$Method == "FAREG", "MSE"] <- 
      mean((ypred[folds==k] - mydata[folds==k, "Y"])^2)
    
    MSE_Y_CA_FAREG[MSE_Y_CA_FAREG$fold == k & 
                               MSE_Y_CA_FAREG$reps == r &
                               MSE_Y_CA_FAREG$Method == "FAREG", "n"] <- sum(folds==k) 
    
    MSE_Y_CA_FAREG[MSE_Y_CA_FAREG$fold == k & 
                               MSE_Y_CA_FAREG$reps == r &
                               MSE_Y_CA_FAREG$Method == "FAREG", 6:ncol(MSE_Y_CA_FAREG)] <- facreg$regcoefs 
  } # kfold
} # reps 

save(MSE_Y_CA_FAREG, file = "MSE_Y_CA_FAREG.Rdata")

# Merge objects  ------------------
files <- dir(pattern = ".Rdata")
n <- length(files)
for(i in 1:n)  load(files[i])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
sdn <- function(x) sd(x)*sqrt(length(x)-1)/sqrt(length(x))
MSE_Y_CA_FSR <- MSE_Y_CA_FAREG
# CERQ ------------------
# OLS
MSE_Y_CA_ols$p <- 9
MSE_Y_CA_ols$nitems <- 36

MSE_Y_SLT_ols$p <- 36
MSE_Y_SLT_ols$nitems <- 36

# FSR
MSE_Y_CA_FSR$p <- 9
MSE_Y_CA_FSR$nitems <- 36

# elastic net 
MSE_Y_CA_elnet[, 
                         9:ncol(MSE_Y_CA_elnet)] <- ifelse(
                           MSE_Y_CA_elnet[,9:ncol(MSE_Y_CA_elnet) ] != 0, TRUE, FALSE)
MSE_Y_CA_elnet$p <- rowSums(MSE_Y_CA_elnet[, 9:ncol(MSE_Y_CA_elnet)])

MSE_Y_CA_elnet$nitems <- MSE_Y_CA_elnet$p *4


MSE_Y_SLT_elnet[, 
                          9:ncol(MSE_Y_SLT_elnet)] <- ifelse(
                            MSE_Y_SLT_elnet[,9:ncol(MSE_Y_SLT_elnet) ] != 0, TRUE, FALSE)
MSE_Y_SLT_elnet$p <- rowSums(MSE_Y_SLT_elnet[, 9:ncol(MSE_Y_SLT_elnet)])
MSE_Y_SLT_elnet$nitems <- MSE_Y_SLT_elnet$p 

# SPCA
MSE_Y_SLT_SPCA$p <- MSE_Y_SLT_SPCA$ncomp

# PCovR
MSE_Y_SLT_PCovR$p <- MSE_Y_SLT_PCovR$ncomp
MSE_Y_SLT_PCovR$nitems <- 36

vars <- c("fold", "reps", "Method", "MSE", "n", "p", "nitems")
MSE_Y_CA <- rbind.data.frame(MSE_Y_CA_ols[,vars],
                                       MSE_Y_CA_elnet[,vars])
MSE_Y_CA$input <- "Subscales"
MSE_Y_SLT <- rbind.data.frame(MSE_Y_SLT_ols[,vars],
                                        MSE_Y_SLT_elnet[,vars],
                                        MSE_Y_SLT_SPCA[,vars],
                                        MSE_Y_SLT_PCovR[,vars],
                                        MSE_Y_CA_FSR[,vars])
MSE_Y_SLT$input <- "Items"
MSE_Y <- rbind.data.frame(MSE_Y_CA, MSE_Y_SLT)
MSE_Y$SS <- MSE_Y$MSE * MSE_Y$n

N <- 240
results <- aggregate(SS ~ reps + Method + input, sum,data = MSE_Y)
results$MSE <- results$SS/N
results_meta <- data.frame(reps = 1:100, Method = "meta-method", input = "-",
                           SS = CERQ_mse_per_rep*N, MSE = CERQ_mse_per_rep)
results <- rbind.data.frame(results,results_meta)
results$input <- factor(results$input, labels = c("Both", "Items", "Subscales"))
results$rule <- paste0(results$Method, "_", results$input)
results$Method <- factor(results$Method, 
                         levels = c("ols", "FAREG", "elnet", "meta-method", "SPCA", "PCovR"),
                         labels = c("OLS", "FSR", "Elastic net", "Meta-method", " SPCA", "PCovR"))
results$rule <- factor(results$rule, levels = c("ols_Subscales", "ols_Items", "FAREG_Items", "elnet_Subscales", "elnet_Items",
                                                "meta-method_Both", "SPCA_Items", "PCovR_Items"),
                       labels = c("OLS Subscales", "OLS Items", "FSR Items","Elastic net Subscales", "Elastic net Items",
                                  "Meta-method", "SPCA Items", "PCovR Items"))
gg <- ggboxplot(results, x = "rule", y = "MSE", fill = "input", 
                palette = "Pastel1", bxp.errorbar = T,
                xlab = "", 
                ylab =  expression(Estimated ~ Prediction ~ Error ~ italic(MSE[pr])))
gg +theme(legend.position = "none",
          text = element_text(family="serif",size = 15),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),)

ggsave("p2_MSE_CERQlogdepr.pdf", width = 8, height = 7)

# table
ave.results <- aggregate(MSE ~ Method + input, 
                         mean, data = results)

# sd is the standard error in this scenario
sd.results <- aggregate(MSE ~ Method + input,sdn, data = results)
MSE_Y$Method <- factor(MSE_Y$Method,levels = c("ols", "FSR", "elnet", "meta-method", "SPCA", "PCovR"),
                                 labels = c("OLS", "FSR", "Elastic net", "Meta-method", " SPCA", "PCovR"))
nitems.results <- aggregate(nitems ~ Method + input, getmode, data =MSE_Y)
p.results <- aggregate(p ~ Method + input, getmode, data =MSE_Y)
tabs <- cbind.data.frame(ave.results[-1,], SE= sd.results$MSE[-1], nitems = nitems.results$nitems,p = p.results$p)
meta <- cbind.data.frame(ave.results[1,], SE= sd.results$MSE[1],  nitems = '-',p = '-')
tabs <- rbind.data.frame(tabs, meta)
xtable(tabs[order(tabs$MSE),], digits = 3)

# variance explained from the components in SPCA --
spca.ncomp <- aggregate(cbind(nitems,p) ~ fold, getmode, data=MSE_Y_SLT_SPCA)
ave.var <- aggregate(varexp ~ reps+ p + nitems, 
                     mean, data = MSE_Y_SLT_SPCA)
ave.var %>%
  ggplot(aes(x=nitems, y=varexp, fill=factor(p))) + 
  geom_boxplot() + ylim(0,1) + theme_bw()

aggregate(cbind(varexp, nitems)~p, summary, data=ave.var)

MSE_Y_SLT_SPCA %>%
  filter(ncomp == 1 & nitems == 9) %>%
  summarize(mean_exp=mean(varexp))

sum3 <-function(X) round(c(min(X), mean(X), max(X))*100,2)
tab <- aggregate(varexp~p, sum3, data=MSE_Y_SLT_SPCA)
table(MSE_Y_SLT_SPCA$p)/1000 *100

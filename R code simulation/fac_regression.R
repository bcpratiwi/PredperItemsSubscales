
#library(lavaan)

# function to create model syntax for cfa model
syntaxmod <- function(itemnames, nscale, scalenames=NULL) {
  if(is.null(scalenames)){scalenames<- paste0("f", 1:nscale)}
  MODEL <- as.list(rep(0, nscale))
  splititems <- split(itemnames, sort(1:length(itemnames)%%nscale))
  names(MODEL) <- scalenames
  for (i in 1:nscale) {
    MODEL[i] <- paste(names(MODEL[i]), paste(splititems[[i]], collapse="+"), sep="=~")
  }
  return(paste(MODEL,collapse="\n"))
}

fa.reg <- function(Y, Items, model, estimator="ML", ridge=1, stdlav=TRUE) 
# Performs CFA on item scores
# Performs a regression analysis using factor scores
# returns CFA model and regression coefficients  
{
  if (ncol(Items) > nrow(Items)){
    cfamod <- lavaan(model=model, data=Items, ridge=ridge, estimator="GLS",
                     std.lv=stdlav)
  } else {
    cfamod <- lavaan(model=model, data=Items, estimator=estimator,
                     std.lv=stdlav)
  }
  facscores <- lavPredict(cfamod)
  mydata <- cbind.data.frame(Y, facscores)
  lmmodel <- lm(Y ~ ., data=mydata)
  return(list(cfamod=cfamod, regcoefs=coef(lmmodel)))
}


predictfareg <- function(facreg,Items)
# Compute predictions 
{
  facscores <- lavPredict(facreg$cfamod, Items)
  return(cbind(1, facscores) %*% facreg$regcoefs)
}

#HS.model <- ' visual  =~ x1 + x2 + x3
#              textual =~ x4 + x5 + x6
#             speed   =~ x7 + x8 + x9 '
#HS.model <- syntaxmod(paste0("x" , 1:9),3, c("vis", "text", "speed"))

#facreg <- fa.reg(Y=HolzingerSwineford1939$grade, 
#                 Items=HolzingerSwineford1939[,paste0("x",1:9)],
#                 model=HS.model,estimator="ML")

#facreg$regcoefs
#ypred <- predictfareg(facreg, HolzingerSwineford1939[,paste0("x",1:9)])


#train <- sample(1:300, 8)
#fit <- cfa(HS.model, sample.cov = cov(HolzingerSwineford1939[paste0("x",1:9)]),
#           sample.nobs = 7)


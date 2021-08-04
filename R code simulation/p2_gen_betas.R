# this script contains a function that generate coefficients for the simulations for project 2
# variance of 2 weighted random variables is b1 var(x1) + b2 var(x2) + 2b1b2cov(x1,x2) 
# Prediction scenarios are controlled by the number of signal items within a subscale and signal subscales

chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))

p2_gen_betas <- function(cov.z, cov.x, nitems, nscales, sig_items, sig_comps, sig_scales = 1, R2, ntotal, exp) 
# nitems : items per subscale
# nscales : number of subscales
# ntotal : nitems*nscales
# sig_comps : percentage of comps that are predictive of Y
# sig_items : percentage of items of subscales that are predictive of Y
# cov.x : covariance matrix of item scores X
# cov.z : covariance matrix of component scores   
  {
  if(exp == "exp1") {
    bvalues <- matrix(0, nscales,1)
    
    idx <- 1:(nscales*sig_comps)
    cov.true <- cov.z[idx,idx]
    cov.true <- cov.true[lower.tri(cov.true)]
    b <- sqrt(R2/(length(idx) +  2* sum(cov.true)))
    
    bvalues[idx] <- b 
    
  } else {
    bvalues <- matrix(0, ntotal, 1) 
    idx <- 1:(ntotal*sig_scales)
    idx <- chunk(idx,nscales*sig_scales)
    idx <- unlist(lapply(idx, function(x) x[1:(sig_items*nitems)]))
    attributes(idx) <- NULL
    
    cov.true <- cov.x[idx, idx]
    cov.true <- cov.true[lower.tri(cov.true)]
    b <- sqrt(R2/(length(idx) +  2* sum(cov.true)))
    
    bvalues[idx] <- b 
    
  }
  
  
  return(bvalues)  
}














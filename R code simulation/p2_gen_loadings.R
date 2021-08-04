# Script to generate item loadings
# Weak, medium, strong measurement
# n items per subscale, 4 or 8
# return = loadings, omega


p2_gen_loadings <- function(ntotal = ntotal, meas = meas) {
  if (meas == "ap.equal") {
    loadings <- runif(ntotal,.6,.7)
  }
  if (meas == "unequal") {
    loadings <- runif(ntotal, .4,.8)
  } 
  omega <- sum(loadings)^2/(sum(loadings)^2 + sum(1-loadings^2))
  
  return(list(omega = omega, loadings = loadings))
}



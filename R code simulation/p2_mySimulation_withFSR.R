args <- commandArgs(TRUE)
args <- as.numeric(args)


r <- args[1]  # repeat
i <- args[2]  # row of the design


# libraries and functions -----------------------
library(lavaan, lib.loc="/home/bpratiwi/R/x86_64-pc-linux-gnu-library/4.0")
library(MASS, lib.loc = "/home/bpratiwi/R/x86_64-pc-linux-gnu-library/4.0")
source("fac_regression.R")
source("p2_gen_data.R")
source("p2_gen_betas.R")
source("p2_gen_loadings.R")

# create objects ----------------------------------
nitems <- c(4, 8)
nscales <- c(8, 20)
meas <- c('ap.equal', 'unequal')
r12 <- c( .3, .5, .7)
ntrain <- c(100,300,500)
sig_comps <- seq(.25, 1, by = .25)
sig_items <- seq(.25, 1, by = .25)
# load loadings
load("meas.design.loadings.Rdata")

# design matrix of measurement
meas.design.mat <- expand.grid(nitems = nitems,
                           nscales = nscales,
                           meas = meas)
meas.design.mat$ntotal <- meas.design.mat$nitems * meas.design.mat$nscales
meas.design.mat$meas.design <- paste0("meas.design", 1:nrow(meas.design.mat))

exp1.design <- expand.grid(ntrain = ntrain, r12= r12, meas.design = meas.design.mat$meas.design,
                           sig_comps = sig_comps, exp = "exp1")    
exp1.design <- merge(exp1.design, meas.design.mat, by = "meas.design")
exp1.design$ndesign <- paste0("ndesign", 1:nrow(exp1.design))

run_design <- i
reps <- r

exp1_MSE <- cbind.data.frame(reps = reps, ndesign = run_design, exp = "exp1", Method = 0,
                      input = 0,
                      MSE = 0)
exp1_MSE$input <- "Items"
exp1_MSE$Method <- "FAREG"
exp1_MSE$rules <-paste0(exp1_MSE$Method, "_", exp1_MSE$input)

# Ytest and Ypredtest                        
ntest <- 10^4
exp1_ytruepredtest <- data.frame(reps = r, ndesign = run_design, exp = "exp1", 
                          Ytruepredtest = rep(NA,ntest)) 
exp1_ypreds <- data.frame(reps = r, ndesign = run_design, exp = "exp1", Ypred = numeric(ntest * length(exp1_MSE$rules))) 
exp1_ypreds$rule <- rep(exp1_MSE$rules, each = ntest)

# coefficients different test lengths
exp1_coefs_FAREG_scales <- data.frame(reps = r, ndesign = run_design, exp = "exp1",
                                    matrix(NA, ncol = exp1.design[i, "nscales"] + 1))
colnames(exp1_coefs_FAREG_scales)[-c(1:3)] <- c("Intercept", paste0("X", 1:exp1.design[i, "nscales"]))


# run one repetition and one design
set.seed((r + 1000)*i)
    cat("design = ", i, "replication = " , r, "\n")
    loadings <- meas.design.loadings[[exp1.design$meas.design[i]]]
    list2env(as.list(exp1.design[i,]), globalenv())
    
    # generate data experiment 1 --
    SimData_exp1 <- p2_gen_data(nitems = nitems,
                           nscales = nscales, 
                           r12 = r12, loadings,
                           sig_items = sig_items,
                           sig_comps = sig_comps,
                           exp = "exp1",
                           ntotal = ntotal,
                           R2 = .4, ntrain = ntrain)
    exp1_ytruepredtest$Ytruepredtest <- SimData_exp1$Ytruepredtest
    # analyze data --
    modsyntax <- syntaxmod(colnames(SimData_exp1$Xtrain), nscale=nscales)
    results_FAREG <- fa.reg(SimData_exp1$Ytrain, SimData_exp1$Xtrain,
                     modsyntax)
    ypred_FAREG <- predictfareg(results_FAREG, SimData_exp1$Xtest)
    # move results to objects ----
    ## MSE
    exp1_MSE[, "MSE"] <- mean((SimData_exp1$Ytest-ypred_FAREG)^2)
   
    ## Y predictions in test set
    exp1_ypreds[, "Ypred"] <- ypred_FAREG
    ## extra parameters
    exp1_coefs_FAREG_scales[,-c(1:3)] <- results_FAREG$regcoefs

# Ytest 
# MSE objects
# experiment 1
assign(paste0("exp1_MSE_design", i,"_rep", reps), exp1_MSE)
rm(exp1_MSE)
save(list = ls(pattern = "exp1_MSE"), file = paste0("exp1_MSE_design",i,"_rep",reps, ".Rdata"))




# experiment 2.2
exp2.2_MSE <- cbind.data.frame(reps = reps, ndesign = run_design, exp = "exp2.2", Method = 0,
                               input = 0,
                               MSE = 0)
exp2.2_MSE$input <- "Items"
exp2.2_MSE$Method <- "FAREG"
exp2.2_MSE$rules <-paste0(exp2.2_MSE$Method, "_", exp2.2_MSE$input)

# Ytest and Ypredtest                        
ntest <- 10^4
exp2.2_ytruepredtest <- data.frame(reps = r, ndesign = run_design, exp = "exp2.2", 
                                   Ytruepredtest = rep(NA,ntest)) 
exp2.2_ypreds <- data.frame(reps = r, ndesign = run_design, exp = "exp2.2", Ypred = numeric(ntest * length(exp2.2_MSE$rules))) 
exp2.2_ypreds$rule <- rep(exp2.2_MSE$rules, each = ntest)


# coefficients different test lengths
# ols
exp2.2_coefs_FAREG_scales <- data.frame(reps = r, ndesign = run_design, exp = "exp2.2",
                                        matrix(NA, ncol = exp2.2.design[i, "nscales"] + 1))
colnames(exp2.2_coefs_FAREG_scales)[-c(1:3)] <- c("Intercept", paste0("X", 1:exp2.2.design[i, "nscales"]))


# run one repetition and one design
set.seed((r + 1000)*i)
cat("design = ", i, "replication = " , r, "\n")
loadings <- meas.design.loadings[[exp2.2.design$meas.design[i]]]
list2env(as.list(exp2.2.design[i,]), globalenv())

# generate data experiment 1 --
SimData_exp2.2 <- p2_gen_data(nitems = nitems,
                              nscales = nscales, 
                              r12 = r12, loadings,
                              sig_items = sig_items,
                              sig_comps = sig_comps,
                              exp = "exp2.2",
                              ntotal = ntotal,
                              R2 = .4, sig_scales = .5, ntrain = ntrain)
exp2.2_ytruepredtest$Ytruepredtest <- SimData_exp2.2$Ytruepredtest
# analyze data --
modsyntax <- syntaxmod(colnames(SimData_exp2.2$Xtrain), nscale=nscales)
results_FAREG <- fa.reg(SimData_exp2.2$Ytrain, SimData_exp2.2$Xtrain,
                        modsyntax)
ypred_FAREG <- predictfareg(results_FAREG, SimData_exp2.2$Xtest)
# move results to objects ----
## MSE
exp2.2_MSE[, "MSE"] <- mean((SimData_exp2.2$Ytest-ypred_FAREG)^2)

## Y predictions in test set
exp2.2_ypreds[, "Ypred"] <- ypred_FAREG
## extra parameters
exp2.2_coefs_FAREG_scales[,-c(1:3)] <- results_FAREG$regcoefs


# MSE objects
# experiment 2.2
assign(paste0("exp2.2_MSE_design", i,"_rep", reps), exp2.2_MSE)
rm(exp2.2_MSE)
save(list = ls(pattern = "exp2.2_MSE"), file = paste0("exp2.2_MSE_design",i,"_rep",reps, ".Rdata"))



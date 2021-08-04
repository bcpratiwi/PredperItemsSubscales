args <- commandArgs(TRUE)
args <- as.numeric(args)


r <- args[1]  # repeat
i <- args[2]  # row of the design


# libraries and functions -----------------------
library(glmnet)
library(superpc)
library(caret)
library(MASS)
library(glmnetUtils)

source("p2_gen_data.R")
source("p2_gen_betas.R")
source("p2_gen_loadings.R")
source("p2_ols.R")
source("p2_elnet_meta_method.R")
source("p2_SPCA.R")
source("p2_pcovr.R")

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

exp1_MSE <- cbind.data.frame(reps = rep(reps,6), ndesign = rep(run_design,6), exp = "exp1", Method = 0,
                      input = 0,
                      MSE = 0)
exp1_MSE$input <- c("Subscales", "Items", "Subscales", "both", "Items", "Items")
exp1_MSE$Method <- c("ols", "elnet", "elnet", "Meta_method", "SPCA", "PCovR")
rules <-paste0(exp1_MSE$Method, "_", exp1_MSE$input)

# Ytest and Ypredtest                        
ntest <- 10^4
exp1_ytruepredtest <- data.frame(reps = r, ndesign = run_design, exp = "exp1", 
                          Ytruepredtest = rep(NA,ntest)) 
exp1_ypreds <- data.frame(reps = r, ndesign = run_design, exp = "exp1", Ypred = numeric(ntest * length(rules))) 
exp1_ypreds$rule <- rep(rules, each = ntest)



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
    results_ols <- p2_ols(SimData_exp1)
    results_elnet_meta_method <- p2_elnet_meta_method(SimData_exp1)
    results_SPCA <- p2_SPCA(SimData_exp1)
    results_PCovR <- p2_pcovr(SimData_exp1, aa = seq(.05, .99, length.out = 20)) ## aa as in the paper vervloet (2016)
    
    # move results to objects ----
    ## MSE
    exp1_MSE[exp1_MSE$input == "Subscales" & exp1_MSE$Method == "ols", "MSE"] <- results_ols$MSE_ols_scales
    exp1_MSE[exp1_MSE$input == "Subscales" & exp1_MSE$Method == "elnet", "MSE"] <- results_elnet_meta_method$MSE_elnet_scales
    exp1_MSE[exp1_MSE$input == "Items" & exp1_MSE$Method == "elnet", "MSE"] <- results_elnet_meta_method$MSE_elnet_items
    exp1_MSE[exp1_MSE$Method == "Meta_method", "MSE"] <- results_elnet_meta_method$MSE_metamethod
    exp1_MSE[exp1_MSE$Method == "SPCA", "MSE"] <- results_SPCA$MSE_SPCA
    exp1_MSE[exp1_MSE$Method == "PCovR", "MSE"] <- results_PCovR$MSE_PCovR
    
# MSE objects
assign(paste0("exp1_MSE_design", i,"_rep", reps), exp1_MSE)
rm(exp1_MSE)
setwd("/exports/fsw/bpratiwi/project_2/V10/exp1/MSE")
save(list = ls(pattern = "exp1_MSE"), file = paste0("exp1_MSE_design",i,"_rep",reps, ".Rdata"))


# Experiment 2 -----------------------------------------------------------
exp2.2_MSE <- cbind.data.frame(reps = rep(reps,6), ndesign = rep(run_design,6), 
                               exp = "exp2.2", Method = 0, input = 0,
                               MSE = 0)
exp2.2_MSE$input <- c("Subscales", "Items", "Subscales", "both", "Items", "Items")
exp2.2_MSE$Method <- c("ols", "elnet", "elnet", "Meta_method", "SPCA", "PCovR")

rules <-paste0(exp2.2_MSE$Method, "_", exp2.2_MSE$input)

# run one repetition and one design
set.seed((r + 1000)*i)
cat("design = ", i, "replication = " , r, "\n")

  # generate data experiment 2 --
  loadings <- meas.design.loadings[[exp2.2.design$meas.design[i]]]
  list2env(as.list(exp2.2.design[i,]), globalenv())
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
  results_ols <- p2_ols(SimData_exp2.2)
  results_elnet_meta_method <- p2_elnet_meta_method(SimData_exp2.2)
  results_SPCA <- p2_SPCA(SimData_exp2.2)
  results_PCovR <- p2_pcovr(SimData_exp2.2, aa = seq(.05, .99, length.out = 20)) ## aa as in the paper vervloet (2016)
  
  # Move results ---
  ## MSE 
  exp2.2_MSE[exp2.2_MSE$input == "Subscales" & exp2.2_MSE$Method == "ols", "MSE"] <- results_ols$MSE_ols_scales
  exp2.2_MSE[exp2.2_MSE$input == "Subscales" & exp2.2_MSE$Method == "elnet", "MSE"] <- results_elnet_meta_method$MSE_elnet_scales
  exp2.2_MSE[exp2.2_MSE$input == "Items" & exp2.2_MSE$Method == "elnet", "MSE"] <- results_elnet_meta_method$MSE_elnet_items
  exp2.2_MSE[exp2.2_MSE$Method == "Meta_method", "MSE"] <- results_elnet_meta_method$MSE_metamethod
  exp2.2_MSE[exp2.2_MSE$Method == "SPCA", "MSE"] <- results_SPCA$MSE_SPCA
  exp2.2_MSE[exp2.2_MSE$Method == "PCovR", "MSE"] <- results_PCovR$MSE_PCovR
  

# experiment 2
assign(paste0("exp2.2_MSE_design", i,"_rep", reps), exp2.2_MSE)
rm(exp2.2_MSE)
save(list = ls(pattern = "exp2.2_MSE"), file = paste0("exp2.2_MSE_design",i,"_rep",reps, ".Rdata"))


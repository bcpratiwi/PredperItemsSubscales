files <- dir(path = "working directory",
             pattern = ".Rdata", full.names = T)
for(i in 1:length(files)) load(files[i])


exp1_MSE <- do.call(rbind, lapply(ls(pattern = "MSE"), get)) 
rm(list = ls(pattern = "design"))
save(exp1_MSE, file = "exp1_MSE.Rdata")

exp2.2_MSE <- do.call(rbind, lapply(ls(pattern = "MSE"), get)) 
rm(list = ls(pattern = "design"))
save(exp2.2_MSE, file = "exp2.2_MSE.Rdata")
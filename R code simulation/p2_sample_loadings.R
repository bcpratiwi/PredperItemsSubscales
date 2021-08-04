# FULL DESIGN POPULATION
nitems <- c(4, 8)
nscales <- c(8, 20)
meas <- c('ap.equal', 'unequal')
r12 <- c( .3, .5, .7)
ntrain <- c(100,300,500)
sig_comps <- seq(.25, 1, by = .25)
sig_items <- seq(.25, 1, by = .25)

meas.design <- expand.grid(nitems = nitems,
                           nscales = nscales,
                           meas = meas)
meas.design$ntotal <- meas.design$nitems * meas.design$nscales
meas.design$meas.design <- paste0("meas.design", 1:nrow(meas.design))
meas.design.loadings <- vector(mode = "list", length = nrow(meas.design))
meas.design.omega <- vector(mode = "list", length = nrow(meas.design))

source("p2_gen_loadings.R")
set.seed(1234)
for(i in 1:nrow(meas.design)) {
  res_loadings <- p2_gen_loadings(meas.design[i,"ntotal"], meas = meas.design[i, "meas"])
  meas.design.loadings[[i]] <- res_loadings$loadings
  meas.design.omega[[i]] <- res_loadings$omega
}
names(meas.design.loadings) <- meas.design$meas.design
names(meas.design.loadings)

save(meas.design.loadings, file = "meas.design.loadings.Rdata")


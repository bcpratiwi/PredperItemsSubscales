install.packages(c("MASS", "glmnet", "superpc", "lattice",
                   "ThreeWay", "GPArotation","lava", "ensr",
                   "Matrix", "ggplot2", "crayon", "vctrs", "caret", "PCovR"),
                   Sys.getenv("R_LIBS_USER"), 
                   repos = "http://cran.case.edu",
                   dependencies = TRUE)

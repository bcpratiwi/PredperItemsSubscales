## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(RColorBrewer)
library(reshape2)
library(haven)
library(ggpubr)
library(openxlsx)
library(xtable)
load("exp_MSE.Rdata")

exp1_MSE$rule <- factor(exp1_MSE$rule, levels = c("ols_Subscales", "elnet_Items", "elnet_Subscales",
                                                  "Meta_method_both", "SPCA_Items", "PCovR_Items"),
                        labels = c("OLS Subscales", "Elnet Items", "Elnet Subscales",
                                   "Meta-method", "SPCA Items", "PCovR Items"))


exp1_MSE$Method <- factor(exp1_MSE$Method, levels = c("ols", "elnet", 
                                                      "Meta_method", "SPCA", "PCovR"),
                          labels = c("OLS", "Elastic net", 
                                     "Meta-method", "SPCA", "PCovR"))



exp2.2_MSE$rule <- factor(exp2.2_MSE$rule, levels = c("ols_Subscales", "elnet_Items", "elnet_Subscales",
                                                      "Meta_method_both", "SPCA_Items", "PCovR_Items"),
                          labels = c("OLS Subscales", "Elnet Items", "Elnet Subscales",
                                     "Meta-method", "SPCA Items", "PCovR Items"))

exp2.2_MSE$Method <- factor(exp2.2_MSE$Method, levels = c("ols", "elnet", 
                                                          "Meta_method", "SPCA", "PCovR"),
                            labels = c("OLS", "Elastic net", 
                                       "Meta-method", "SPCA", "PCovR"))
# average over repetitions
## Experiment 1
exp1_MSE_wide<- aggregate(MSE ~ rule + nitems + ntrain + r12 + sig_comps + sig_items+  nscales + meas + ntotal, mean, data = exp1_MSE)

## Experiment 2.2
exp2.2_MSE_wide<- aggregate(MSE ~rule + nitems + ntrain + r12 + sig_items + sig_comps+ nscales + meas + ntotal, mean, data = exp2.2_MSE)


## Experiment 1
exp1_MSE_wide<- dcast(exp1_MSE_wide, 
                      nitems + ntrain + r12 + sig_comps + sig_items + nscales + meas + ntotal 
                      ~  rule, value.var = "MSE")



## Experiment 2.2
exp2.2_MSE_wide<- dcast(exp2.2_MSE_wide,
                        nitems + ntrain + r12 + sig_items + sig_comps + nscales + meas + ntotal 
                        ~  rule, value.var = "MSE")


# which method wins?
win.rule <- apply(exp1_MSE_wide[,9:14], 1, function(x) which.min(x))
exp1_MSE_wide$win.rule <- colnames(exp1_MSE_wide)[9:14][win.rule]

#win.rule <- apply(exp2_MSE_wide[,9:14], 1, function(x) which.min(x))
#exp2_MSE_wide$win.rule <- colnames(exp2_MSE_wide)[9:14][win.rule]

win.rule <- apply(exp2.2_MSE_wide[,9:14], 1, function(x) which.min(x))
exp2.2_MSE_wide$win.rule <- colnames(exp2.2_MSE_wide)[9:14][win.rule]

table(exp1_MSE_wide$win.rule)
#table(exp2_MSE_wide$win.rule)
table(exp2.2_MSE_wide$win.rule)

# transform to factors
exp1_MSE_wide$win.rule <- factor(exp1_MSE_wide$win.rule, levels = levels(exp1_MSE$rule))
exp2.2_MSE_wide$win.rule <- factor(exp2.2_MSE_wide$win.rule,levels = levels(exp1_MSE$rule))


# add method column and input column
exp1_MSE_wide$Method <- ifelse(str_detect(exp1_MSE_wide$win.rule, "OLS"), "OLS", 
                               ifelse(str_detect(exp1_MSE_wide$win.rule, "Elnet"), "Elastic net", 
                                      ifelse(str_detect(exp1_MSE_wide$win.rule, "Meta-method"), "Meta-method",
                                             ifelse(str_detect(exp1_MSE_wide$win.rule, "SPCA"), "SPCA", "PCovR"))))

exp2.2_MSE_wide$Method <- ifelse(str_detect(exp2.2_MSE_wide$win.rule, "OLS"), "OLS", 
                                 ifelse(str_detect(exp2.2_MSE_wide$win.rule, "Elnet"), "Elastic net", 
                                        ifelse(str_detect(exp2.2_MSE_wide$win.rule, "Meta-method"), "Meta-method",
                                               ifelse(str_detect(exp2.2_MSE_wide$win.rule, "SPCA"), "SPCA", "PCovR"))))


exp1_MSE_wide$Method <- factor(exp1_MSE_wide$Method, levels = levels(exp1_MSE$Method))
exp2.2_MSE_wide$Method <- factor(exp2.2_MSE_wide$Method,levels = levels(exp1_MSE$Method))


exp1_MSE_wide$input <- ifelse(str_detect(exp1_MSE_wide$win.rule, "Items"), "Items", 
                              ifelse(str_detect(exp1_MSE_wide$win.rule, "Subscales"), "Subscales", "Both"))

exp2.2_MSE_wide$input <- ifelse(str_detect(exp2.2_MSE_wide$win.rule, "Items"), "Items", 
                                ifelse(str_detect(exp2.2_MSE_wide$win.rule, "Subscales"), "Subscales", "Both"))

exp1_MSE$input <- factor(exp1_MSE$input, levels = c("Items", "Subscales", "both"), labels = c("Items", "Subscales", "Both"))
exp2.2_MSE$input <- factor(exp2.2_MSE$input, levels = c("Items", "Subscales", "both"), labels = c("Items", "Subscales", "Both"))

exp1_MSE_wide$input <- factor(exp1_MSE_wide$input, levels = levels(exp1_MSE$input))
exp2.2_MSE_wide$input <- factor(exp2.2_MSE_wide$input,levels = levels(exp1_MSE$input))

exp_MSE_wide <- rbind.data.frame(exp1_MSE_wide,
                                 # exp2_MSE_wide,
                                 exp2.2_MSE_wide)
vars <- c("nitems", "ntrain", "r12", "sig_items", "nscales", "meas", "sig_comps")
exp_MSE_wide[,vars] <- apply(exp_MSE_wide[,vars], 2, as.factor)
exp_MSE_wide$exp <- rep(c("exp1", "exp2.2"), each = nrow(exp1_MSE_wide))

exp1_MSE_wide[,vars] <- apply(exp1_MSE_wide[,vars], 2, as.factor)
exp2.2_MSE_wide[,vars] <- apply(exp2.2_MSE_wide[,vars], 2, as.factor)

## ---- fig.cap="Overall means of the Mean Squared Error of prediction against rule aggregated by experiment.\\label{fig:exp_effect}"----
method_colors <- c("OLS" = "red", "Elastic net" = "dodgerblue", "Meta-method" = "darkblue", 
                   "SPCA" = "green", "PCovR" = "purple")
results_m <- aggregate(MSE ~ rule + exp + nscales + nitems + meas + r12 + ntrain+ sig_comps + sig_items , 
                       mean, data = exp_MSE)

## ---------------------------------------------------------------------------------------------------------------------
# Experiment 1
# Percentage of wins --------------------------
input_col <- c("#B3CDE3","#CCEBC5","#FBB4AE")
gg <- ggplot(exp1_MSE_wide, aes(x = win.rule,
                                fill = input, y = (..count..)/sum(..count..))) +
  geom_bar(position = position_dodge2(reverse = T, width = 1, preserve = "total")) + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat= "count", vjust = -.5)  +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  scale_x_discrete(labels = c("OLS Subscales", "Elastic net Items", "Elastic net Subscales","Meta-method", 
                              "SPCA Items", "PCovR Items"), drop =F) +
  scale_fill_manual(values = input_col)+
  labs(fill = " ", y = "Percentage of data sets", x = "")

gg + theme(legend.position = "none", 
           legend.key = element_rect(fill = "white", colour = "white"),
           text = element_text(family="serif", size = 15),
           strip.background = element_rect(fill = "white", colour = "black"),
           panel.background = element_rect(fill = "white", colour= "black"),
           panel.grid.minor = element_blank())

## -----Rule x nscales x ntrain----------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule + Method + nscales + ntrain + input, mean, data = exp1_MSE)
results_m$nscales <- factor(results_m$nscales)
results_m$input <- factor(results_m$input)
gg <- ggplot(results_m, aes(x = ntrain, y = MSE, color =Method, linetype = input)) + geom_point() +geom_line() + facet_grid(.~ nscales) +
      ylim(c(0.6,0.8)) + scale_color_manual(values = method_colors) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
      scale_x_continuous(breaks = c(100, 300, 500)) + 
      guides(linetype = FALSE) + 
      labs(x = expression(italic(n[train])), y = expression(Estimated~Prediction~Error~italic(MSE[pr])), 
           color = "")
gg +theme(legend.position = "top", 
          legend.key = element_rect(fill = "white", colour = "white"),
          text = element_text(family="serif", size = 15),
          strip.background = element_rect(fill = "white", colour = "black"),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave("p2_exp1_nscales_ntrain_rule.pdf", width = 8, height = 6)

## ------ Errorplot Rule exclude OLS Subscales (not necessary) ---------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ Method + reps + input, mean, data = subset(exp1_MSE, rule != "OLS Subscales"))
results_m$input <- factor(results_m$input, labels = c("Both", "Items", "Subscales"))
results_m$input <- factor(results_m$input, labels = c("Both", "Items", "Subscales"))
input_col <- c("#B3CDE3","#CCEBC5","#FBB4AE")
gg <-  ggerrorplot(results_m, x = "rule", y = "MSE", 
                   col = "input", size = 1.5,
                   palette = input_col,
                   desc_stat = "mean_ci", error.plot = "errorbar",
                   add = "mean", 
                   ylab =  expression(Estimated ~ Prediction ~ Error ~ italic(MSE[pr])),
                   bxp.errorbar = T, xlab = "")

gg + theme(legend.position = "none",
          text = element_text(family="serif",size = 15),
          axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),)

ggsave("p2_exp1_rule_exc_ols_err_reps.pdf", width = 8, height = 6)

# Experiment 2.2 ---------------------------------------------------------------------
# Percentage of wins --------------------------------------
gg <- ggplot(exp2.2_MSE_wide, aes(x = win.rule, fill = input, y = (..count..)/sum(..count..))) +
  geom_bar(position = position_dodge2(reverse = T, width = 1, preserve = "total")) + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat= "count", vjust = -.5)  +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  scale_x_discrete(labels = c("OLS Subscales", "Elastic net Items", "Elastic net Subscales","Meta-method", 
                              "SPCA Items", "PCovR Items"), drop =F) +
  scale_fill_manual(values = input_col )+
  labs(fill = " ", y = "Percentage of data sets", x = "")

gg + theme(legend.position = "none", 
           legend.key = element_rect(fill = "white", colour = "white"),
           text = element_text(family="serif", size = 15),
           strip.background = element_rect(fill = "white", colour = "black"),
           panel.background = element_rect(fill = "white", colour= "black"),
           panel.grid.minor = element_blank())

## ----Rule x nscales x ntrain -----------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule+ Method +  input+ nscales +ntrain, mean, data = exp2.2_MSE)
# paper -------------------------------------------------
gg <- ggplot(results_m, aes(x = ntrain, y = MSE, color =Method, linetype = input)) + geom_point() +geom_line() + facet_grid(.~ nscales) +
  ylim(c(0.6,0.8)) + scale_color_manual(values = method_colors) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid"),
                        labels = c("Both", "Items", "Subscales"))+
  guides(linetype = FALSE, color = FALSE) +
  scale_x_continuous(breaks = c(100, 300, 500)) + 
  labs(x = expression(italic(n[train])), y = expression(Estimated~Prediction~Error~italic(MSE[pr])))
gg + theme(text = element_text(family="serif", size = 15),
           strip.background = element_rect(fill = "white", colour = "black"),
           panel.background = element_rect(fill = "white", colour= "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())


ggsave("p2_exp2_2_nscales_ntrain_rule.pdf", width = 8, height = 6)


## -----Rule x signal items ----------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule+ Method + sig_items + input, mean, data = exp2.2_MSE)
results_m$sig_items <- factor(results_m$sig_items, labels = paste0(seq(25,100,25), "%"))
# Paper -------------------
gg <- ggplot(results_m, aes(x = sig_items, y = MSE, group = interaction(input,Method),
                            color =Method, linetype = input)) + 
  geom_point() +geom_line() +  ylim(c(0.6,0.8)) + scale_color_manual(values = method_colors) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
  guides(linetype = FALSE) +
  labs(x = "% Signal Items", y = expression(Estimated~Prediction~Error~ italic(MSE[pr])),
       color = "")

gg +theme(legend.position = "top", 
          legend.key = element_rect(fill = "white", colour = "white"),
          text = element_text(family="serif", size = 15),
          strip.background = element_rect(fill = "white", colour = "black"),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),)



## Exclude OLS -------------------------------------------
## Rule x sig_items -------------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule+ Method + sig_items + input, mean, data = exp2.2_MSE[exp2.2_MSE$Method != "OLS",])
results_m$sig_items <- factor(results_m$sig_items, labels = paste0(seq(25,100,25), "%"))
# Paper -------------------
gg <- ggplot(results_m, aes(x = sig_items, y = MSE, group = interaction(input,Method),
                            color =Method, linetype = input)) + 
  geom_point() +geom_line() +  ylim(c(0.6,0.8)) + scale_color_manual(values = method_colors) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
  guides(linetype = FALSE) +
  labs(x = "% Signal Items", y = expression(Estimated~Prediction~Error~ italic(MSE[pr])),
       color = "")
gg +theme(legend.position = "top", 
          legend.key = element_rect(fill = "white", colour = "white"),
          text = element_text(family="serif", size = 15),
          strip.background = element_rect(fill = "white", colour = "black"),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("p2_exp2_2_sig_items_rule_exc_ols.pdf", width = 7, height = 6)

## ----Rule x ntrain -----------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule+ Method + ntrain + input, mean, data = exp2.2_MSE[exp2.2_MSE$Method != "OLS",])
# Paper -------------------
gg <- ggplot(results_m, aes(x = ntrain, y = MSE, group = interaction(input,Method),
                            color =Method, linetype = input)) + 
  geom_point() +geom_line() +  ylim(c(0.6,0.8)) + scale_color_manual(values = method_colors) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
  scale_x_continuous(breaks = c(100,300,500)) +
  guides(linetype = FALSE, color = FALSE) +
  labs(x = expression(italic(n[train])),
       y = expression(Estimated~Prediction~Error~italic(MSE[pr])))
gg +theme(text = element_text(family="serif", size = 15),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

## ------ Rule x nscales ------------
results_m <- aggregate(MSE ~ rule+ Method + nscales + input, mean, data = exp2.2_MSE[exp2.2_MSE$Method != "OLS",])
results_m$nscales <- as.factor(results_m$nscales)
# Paper -------------------
gg <- ggplot(results_m, aes(x = nscales, y = MSE, group = interaction(input,Method),
                            color =Method, linetype = input)) + 
  geom_point() +geom_line() +  ylim(c(0.6,0.8)) + scale_color_manual(values = method_colors) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
  guides(linetype = FALSE, color = FALSE) +
  labs(x = expression(Number~of~Components~italic(K)), color = "", y = expression(Estimated~Prediction~Error~italic(MSE[pr])))
gg +theme(text = element_text(family="serif", size = 15),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

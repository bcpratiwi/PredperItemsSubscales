# solution includes FAREG Items
library(RColorBrewer)
library(reshape2)
library(haven)
library(stringr)
library(ggpubr)
library(openxlsx)
library(xtable)
load("p2_exp_MSE.Rdata")

exp_MSE$rule <- factor(exp_MSE$rule, levels =  c("ols_Subscales", "FAREG_Items",  "elnet_Subscales", "elnet_Items",
                                                  "Meta_method_both", "SPCA_Items", "PCovR_Items"),
                       labels = c("OLS Subscales", "FSR Items",  "Elastic net Subscales", "Elastic net Items",
                                  "Meta-method", "SPCA Items", "PCovR Items"))

exp_MSE$Method <- factor(exp_MSE$Method, levels = c("ols","FAREG", "elnet", 
                                                      "Meta_method", "SPCA", "PCovR"),
                          labels = c("OLS", "FSR", "Elastic net", 
                                     "Meta-method", "SPCA", "PCovR"))

exp1_MSE$rule <- factor(exp1_MSE$rule, levels = c("ols_Subscales", "FAREG_Items", "elnet_Subscales", "elnet_Items", 
                                                  "Meta_method_both", "SPCA_Items", "PCovR_Items"),
                        labels = c("OLS Subscales", "FSR Items",  "Elastic net Subscales", "Elastic net Items",
                                   "Meta-method", "SPCA Items", "PCovR Items"))


exp1_MSE$Method <- factor(exp1_MSE$Method, levels = c("ols","FAREG", "elnet", 
                                                      "Meta_method", "SPCA", "PCovR"),
                          labels = c("OLS", "FSR", "Elastic net", 
                                     "Meta-method", "SPCA", "PCovR"))



exp2.2_MSE$rule <- factor(exp2.2_MSE$rule, levels = c("ols_Subscales", "FAREG_Items", "elnet_Subscales", "elnet_Items",
                                                      "Meta_method_both", "SPCA_Items", "PCovR_Items"),
                          labels = c("OLS Subscales", "FSR Items","Elastic net Subscales", "Elastic net Items",
                                     "Meta-method", "SPCA Items", "PCovR Items"))
exp2.2_MSE$Method <- factor(exp2.2_MSE$Method, levels = c("ols","FAREG", "elnet", 
                                                          "Meta_method", "SPCA", "PCovR"),
                            labels = c("OLS", "FAREG", "Elastic net", 
                                       "Meta-method", "SPCA", "PCovR"))
# average over repetitions
## Experiment 1
exp1_MSE_wide<- aggregate(MSE ~ rule + nitems + ntrain + r12 + sig_items+ sig_comps +  nscales +ntotal+ meas, mean, data = exp1_MSE)


## Experiment 2.2
exp2.2_MSE_wide<- aggregate(MSE ~rule + nitems + ntrain + r12 + sig_items + sig_comps+ nscales + meas + ntotal, mean, data = exp2.2_MSE)


## Experiment 1
exp1_MSE_wide<- dcast(exp1_MSE_wide, 
                      nitems + ntrain + r12 + sig_comps + sig_items + nscales + meas + ntotal 
                      ~  rule, value.var = "MSE")

## Experiment 2.2
exp2.2_MSE_wide<- dcast(exp2.2_MSE_wide,
                        nitems + ntrain + r12 +  sig_comps+ sig_items + nscales + meas + ntotal 
                        ~  rule, value.var = "MSE")


# which method wins? [9:15] idx rule
win.rule <- apply(exp1_MSE_wide[,9:15], 1, function(x) which.min(x))
exp1_MSE_wide$win.rule <- colnames(exp1_MSE_wide)[9:15][win.rule]

win.rule <- apply(exp2.2_MSE_wide[,9:15], 1, function(x) which.min(x))
exp2.2_MSE_wide$win.rule <- colnames(exp2.2_MSE_wide)[9:15][win.rule]

table(exp1_MSE_wide$win.rule)
table(exp2.2_MSE_wide$win.rule)

# transform to factors
exp1_MSE_wide$win.rule <- factor(exp1_MSE_wide$win.rule, levels = levels(exp1_MSE$rule))
exp2.2_MSE_wide$win.rule <- factor(exp2.2_MSE_wide$win.rule,levels = levels(exp1_MSE$rule))


# add method column and input column
exp1_MSE_wide$Method <- ifelse(str_detect(exp1_MSE_wide$win.rule, "ols"), "OLS", 
                               ifelse(str_detect(exp1_MSE_wide$win.rule, "Elnet"), "Elastic net", 
                                      ifelse(str_detect(exp1_MSE_wide$win.rule, "Meta-method"), "Meta-method",
                                             ifelse(str_detect(exp1_MSE_wide$win.rule, "SPCA"), "SPCA", 
                                                    ifelse(str_detect(exp1_MSE_wide$win.rule, "PCovR"), "PCovR", "FSR")))))

exp2.2_MSE_wide$Method <- ifelse(str_detect(exp2.2_MSE_wide$win.rule, "OLS"), "OLS", 
                                 ifelse(str_detect(exp2.2_MSE_wide$win.rule, "Elnet"), "Elastic net", 
                                        ifelse(str_detect(exp2.2_MSE_wide$win.rule, "Meta-method"), "Meta-method",
                                               ifelse(str_detect(exp2.2_MSE_wide$win.rule, "SPCA"), "SPCA", 
                                                      ifelse(str_detect(exp2.2_MSE_wide$win.rule, "PCovR"), "PCovR", "FSR")))))


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
                                 exp2.2_MSE_wide)
vars <- c("nitems", "ntrain", "r12", "sig_items", "nscales", "meas", "sig_comps")
exp_MSE_wide[,vars] <- apply(exp_MSE_wide[,vars], 2, as.factor)
exp_MSE_wide$exp <- rep(c("exp1", "exp2.2"), each = nrow(exp1_MSE_wide))

exp1_MSE_wide[,vars] <- apply(exp1_MSE_wide[,vars], 2, as.factor)
exp2.2_MSE_wide[,vars] <- apply(exp2.2_MSE_wide[,vars], 2, as.factor)


### Mixed ANOVA using SPSS -------------------------------------------------------------------------------
exp1_mixed_anova <- read.xlsx("MixedAnova_withinSubs_exp1.xlsx", sheet = 1)
exp1_mixed_anova_exc_ols <- read.xlsx("MixedAnova_withinSubs_exp1_exc_ols.xlsx", sheet = 1)

exp2_2_mixed_anova <- read.xlsx("MixedAnova_withinSubs_exp2_2.xlsx", sheet = 1)
exp2_2_mixed_anova_exc_ols <- read.xlsx("MixedAnova_withinSubs_exp2_2_exc_ols.xlsx", sheet = 1)
vars <- c("Source", "SS", "df","F", "EtaSquared")

tab <- exp1_mixed_anova[rev(order(exp1_mixed_anova$EtaSquared)),]
xtable(tab[c(2:11,1), vars], digits = 3)
tab <- exp1_mixed_anova_exc_ols[rev(order(exp1_mixed_anova_exc_ols$EtaSquared)),]
xtable(tab[c(2:11,1), vars], digits = 3)

tab <- exp2_2_mixed_anova[rev(order(exp2_2_mixed_anova$EtaSquared)),]
xtable(tab[c(2:11,1), vars], digits = 3)
tab <- exp2_2_mixed_anova_exc_ols[rev(order(exp2_2_mixed_anova_exc_ols$EtaSquared)),]
xtable(tab[c(2:11,1), vars], digits = 3)



## ---- ----
input_colors <- c("Items"= "#B3CDE3","Subscales"="#CCEBC5","Both"= "#FBB4AE")
rule_colors <- c("OLS Subscales" = 'red', "FSR Items" = "purple", "Elastic net Subscales"= 'blue',
                "Elastic net Items"= 'lightblue',"Meta-method"='dodgerblue',
                "SPCA Items"='lightgreen', "PCovR Items" = 'darkgreen')

## ---------------------------------------------------------------------------------------------------------------------
# Experiment 1
# Percentage of wins --------------------------
gg <- ggplot(exp1_MSE_wide, aes(x = win.rule,
                                fill = input, y = (..count..)/sum(..count..))) +
  geom_bar(position = position_dodge2(reverse = T, width = 1, preserve = "total")) + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat= "count", vjust = -.5, size = 5)  +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  scale_x_discrete(drop =F) +
  scale_fill_manual(values = input_colors)+
  labs(fill = " ", y = "Percentage of won conditions", x = "")

gg + theme(legend.position = "none", 
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.key = element_rect(fill = "white", colour = "white"),
           text = element_text(family="serif", size = 18),
           strip.background = element_rect(fill = "white", colour = "black"),
           panel.background = element_rect(fill = "white", colour= "black"),
           panel.grid.minor = element_blank())
ggsave("p2_exp1_rule_input_wins.pdf", width = 9, height = 6)
## -----Rule x nscales x ntrain----------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule +  nscales + ntrain , mean, data = exp1_MSE)
results_m$nscales <- factor(results_m$nscales)
gg <- ggplot(results_m, aes(x = ntrain, y = MSE, color = rule)) + 
  geom_point(size=4) +geom_line(size=1.5) + 
  facet_grid(.~ nscales, 
             labeller = as_labeller(c(`8` = "nr components = 8",
                                      `20`= "nr components = 20")))+
  ylim(c(0.6,0.8)) + scale_color_manual(values = rule_colors) +
  scale_x_continuous(breaks = c(100, 300, 500)) + 
  labs(x = expression(Size~of~training~sample~italic(n[train])), y = expression(Estimated~Prediction~Error~italic(MSE[pr])), 
           color = "")

gg +theme(legend.position = "right", 
          legend.key = element_rect(fill = "white", colour = "white"),
          text = element_text(family="serif", size = 18),
          strip.background = element_rect(fill = "white", colour = "black"),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave("p2_exp1_nscales_ntrain_rule.pdf", width = 10, height = 6)

# Rule (exclude OLS) x ntrain x nscales --------------------------------------------
exc_ols <- subset(results_m, rule != "OLS Subscales")
gg <- ggplot(exc_ols, aes(x = ntrain, y = MSE, color = rule)) + 
  geom_point(size=4) +geom_line(size=1.5) + 
  facet_grid(.~ nscales, 
             labeller = as_labeller(c(`8` = "nr components = 8",
                                      `20`= "nr components = 20")))+
  ylim(c(0.6,0.8)) + scale_color_manual(values = rule_colors) +
  scale_x_continuous(breaks = c(100, 300, 500)) + 
  labs(x = expression(Size~of~training~sample~italic(n[train])), y = expression(Estimated~Prediction~Error~italic(MSE[pr])), 
       color = "")
## ------ Errorplot Rule exclude OLS Subscales and FSR  ---------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule +  nscales + ntrain + sig_comps+r12+meas, mean, data = subset(exp1_MSE, !(rule %in% c("OLS Subscales", "FSR Items"))))
gg <-  ggerrorplot(results_m, x = "rule", y = "MSE", size = 1,
                   desc_stat = "mean_ci", error.plot = "errorbar",
                   add = "mean", 
                   ylab =  expression(Estimated ~ Prediction ~ Error ~ italic(MSE[pr])),
                   bxp.errorbar = T, xlab = "")

gg + theme(legend.position = "none",
           axis.text.x = element_text(angle = 45, hjust = 1),
           text = element_text(family="serif",size = 18),
           axis.line = element_line(colour = "black"),
           panel.background = element_rect(fill = "white", colour= "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),)

ggsave("p2_exp1_rule_exc_ols_FSR_err_reps.pdf", width = 7, height = 6)
# Experiment 2.2 ---------------------------------------------------------------------
# Percentage of wins --------------------------------------
gg <- ggplot(exp2.2_MSE_wide, aes(x = win.rule, fill = input, y = (..count..)/sum(..count..))) +
  geom_bar(position = position_dodge2(reverse = T, width = 1, preserve = "total")) + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat= "count", vjust = -.5, size = 5)  +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  scale_x_discrete( drop =F) +
  scale_fill_manual(values = input_colors)+
  labs(fill = " ", y = "Percentage of won conditions", x = "")

gg + theme(legend.position = "none", 
           legend.key = element_rect(fill = "white", colour = "white"),
           axis.text.x = element_text(angle = 45, hjust = 1),
           text = element_text(family="serif", size = 18),
           strip.background = element_rect(fill = "white", colour = "black"),
           panel.background = element_rect(fill = "white", colour= "black"),
           panel.grid.minor = element_blank())
ggsave("p2_exp2_2_rule_input_wins.pdf", width =9, height = 6)

## Experiment 2.2 -------------------------------------
## ----Rule x nscales x ntrain -----------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule+  nscales +ntrain, mean, data = exp2.2_MSE)
gg <- ggplot(results_m, aes(x = ntrain, y = MSE, color =rule)) + 
  geom_point(size=4) + geom_line(size=1.5) + 
  facet_grid(.~ nscales, 
             labeller = as_labeller(c(`8` = "nr components = 8",
                                      `20`= "nr components = 20"))) +
  ylim(c(0.6,0.8)) + scale_color_manual(values = rule_colors) +
  guides( color = "none") +
  scale_x_continuous(breaks = c(100, 300, 500)) + 
  labs(x = expression(Size~of~training~sample~italic(n[train])), y = expression(Estimated~Prediction~Error~italic(MSE[pr])))
gg + theme(text = element_text(family="serif", size = 18),
           strip.background = element_rect(fill = "white", colour = "black"),
           panel.background = element_rect(fill = "white", colour= "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())

ggsave("p2_exp2_2_nscales_ntrain_rule.pdf", width = 9.5, height = 6)

## -----Rule x signal items ----------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule+  sig_items , mean, data = exp2.2_MSE)
gg <- ggplot(results_m, aes(x = sig_items, y = MSE,
                            color =rule)) + 
  geom_point(size=4) +geom_line(size=1.5) +  ylim(c(0.6,0.8)) + scale_color_manual(values = rule_colors) +
  scale_x_continuous(breaks = seq(.25,1,.25),labels = scales::percent)+
  labs(x = "% Signal Items", y = expression(Estimated~Prediction~Error~ italic(MSE[pr])),
       color = "")

gg +theme(legend.position = "right", 
          legend.key = element_rect(fill = "white", colour = "white"),
          text = element_text(family="serif", size = 18),
          strip.background = element_rect(fill = "white", colour = "black"),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),)
ggsave("p2_exp2_2_sig_items_rule.pdf", width = 8.5, height = 6)


## Exclude OLS and FSR -------------------------------------------
## Rule x sig_items -------------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule+  sig_items , mean, data = subset(exp2.2_MSE,!(rule %in% c("OLS Subscales", "FSR Items"))))
gg <- ggplot(results_m, aes(x = sig_items, y = MSE, 
                            color = rule)) + 
  geom_point(size=4) +geom_line(size=1.5) +  ylim(c(0.6,0.72)) + scale_color_manual(values = rule_colors[-c(1:2)]) +
  scale_x_continuous(breaks = seq(.25,1,.25),labels = scales::percent)+
  labs(x = "% Signal Items", y = expression(Estimated~Prediction~Error~ italic(MSE[pr])),
       color = "")
gg +theme(legend.position = "right", 
          legend.key = element_rect(fill = "white", colour = "white"),
          text = element_text(family="serif", size = 18),
          strip.background = element_rect(fill = "white", colour = "black"),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("p2_exp2_2_sig_items_rule_exc_ols_FSR.pdf", width = 8.5, height = 6)

## ----Rule x ntrain -----------------------------------------------------------------------------------------------------------------
results_m <- aggregate(MSE ~ rule+  ntrain, mean, data = subset(exp2.2_MSE,!(rule %in% c("OLS Subscales", "FSR Items"))))
gg <- ggplot(results_m, aes(x = ntrain, y = MSE, 
                            color =rule)) + 
  geom_point(size=4) +geom_line(size=1.5) +  ylim(c(0.6,0.72)) + scale_color_manual(values = rule_colors[-1]) +
  scale_x_continuous(breaks = c(100,300,500)) +
  guides(color = "none") +
  labs(x = expression(Size~of~training~sample~italic(n[train])),
       y = expression(Estimated~Prediction~Error~italic(MSE[pr])))
gg +theme(text = element_text(family="serif", size = 18),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave("p2_exp2_2_ntrain_rule_exc_ols_FSR.pdf", width = 7, height = 6)

## ------ Rule x nscales ------------
results_m <- aggregate(MSE ~ rule + nscales, mean, data = subset(exp2.2_MSE,!(rule %in% c("OLS Subscales", "FSR Items"))))
results_m$nscales <- as.factor(results_m$nscales)
gg <- ggplot(results_m, aes(x = nscales, y = MSE, group=rule,
                            color =rule)) + 
  geom_point(size=4) + geom_line(size=1.5) + ylim(c(0.6,0.72)) + scale_color_manual(values = rule_colors[-1]) +
  guides(color = 'none') +
  labs(x = expression(Number~of~Components~italic(K)), color = "", y = expression(Estimated~Prediction~Error~italic(MSE[pr])))
gg +theme(text = element_text(family="serif", size = 18),
          panel.background = element_rect(fill = "white", colour= "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("p2_exp2_2_nscales_rule_exc_ols_FSR.pdf", width = 7, height = 6)

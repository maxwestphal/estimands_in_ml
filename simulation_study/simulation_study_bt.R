###
#   Project: 
#   Estimands in ML algorithm evaluation (estimands_in_ml)
###
#   Author: 
#   Max Westphal (max.westphal@mevis.fraunhofer.de)
###
#   Date: 
#   2022-03-26
###
#   Script:
#   simulation_study_bt.R
###
#   Remarks:
#
###


# PACKAGES ------------------------------------------------------------------------------------
library(batchtools)
library(data.table)
library(parallel)
library(dplyr)
library(readr)
library(tictoc)


# PREPERATION ---------------------------------------------------------------------------------
# rm(list = ls())                           

## simulation name, 
sim <- "EstiML_SIM_v1"                            
nsim <- 5000                     
cr <- 0.8                                 
req.packages <- c("dplyr", "tidyselect", "propagate", "mvtnorm", "glmnet") 

## setup folders: 
main.dir <- file.path("E:/EstiML")              
reg.dir <- file.path(main.dir, sim)  
out.dir <- file.path(main.dir, "EstiML_SIM_results")
dir.create(main.dir)
dir.create(out.dir)

## setup prob parameters: 
f <- 2 # factor
prob <- list()
prob[["ml_study"]] <- 
  data.frame(n_cell = 100,
             beta_0 = -2, 
             sd_b = 1*f,
             sd_s = rep(c(1.00, 0.50, 0.50), each=2)*f,
             sd_t = rep(c(0.50, 0.50, 0.25), each=2)*f,
             rho_s = rep(c(0.25, 0.50), 3),
             rho_t = rep(c(0.50, 0.75), 3)
  )

prob


## setup algo parameters:
algo <- list()
algo[["null"]] <- CJ()

message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", nrow(algo[[1]]))


# REGISTRY SETUP ------------------------------------------------------------------------------
## create/load registry:
reg <- makeExperimentRegistry(file.dir = reg.dir, seed = 1)
reg <- loadRegistry(reg.dir, writeable = TRUE)

reg$packages <- req.packages
reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))
reg$source <- file.path("2_simulation_study", "simulation_study_funs.R")

## add problems/algorithms:
addProblem(name="ml_study", data=NULL, 
           fun=ml_study, reg=reg) 
addAlgorithm(name= "null", 
             fun= NULL, reg=reg) 

## add experiments:
addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls=nsim,
               reg=reg)

summarizeExperiments(reg=reg)


# Test jobs -----------------------------------------------------------------------------------
# j <- findExperiments(algo.pars = adjustment == "mbeta")
j <- 6001
tic()
r <- testJob(j, reg=reg)
toc()
r
unwrap(getJobPars(j, reg=reg))


# Execute jobs --------------------------------------------------------------------------------
tic()
# jj <- findExperiments(prob.pars = m <=10) %>% ijoin(findNotDone())
jj <- findNotDone(reg=reg)
submitJobs(jj, reg=reg)
toc()


# Optional steps ------------------------------------------------------------------------------
getStatus(reg=reg)
# findExpired(reg=reg)
# removeExperiments(..., reg=reg)
# waitForJobs(reg=reg)
# getErrorMessages()
# killJobs(reg=reg)
# clearRegistry(reg=reg)


# Save results --------------------------------------------------------------------------------
# options(batchtools.progress = TRUE)
JP <- getJobPars(findDone(), reg=reg) %>% unwrap()

tic()
R <- reduceResultsList(findDone(), reg=reg, fun = NULL) 
toc()

JP <- JP %>% mutate(splt = as.character(interaction(sd_s, sd_t, rho_s, rho_t, sep="|")))

length(unique(JP$splt))
ss <- split(1:nrow(JP), JP$splt)

results_bias_abs <- lapply(ss, function(s){
  out_list <- lapply(R[s], function(x) {
    return(x$results$D) # absolute bias
  }) 
  Reduce("+", out_list)/length(out_list)
})

results_bias_abs_sse <- lapply(ss, function(s){
  out_array <- lapply(R[s], function(x) {
    return(x$results$D) # absolute bias
  }) %>% simplify2array()
  sqrt(apply(out_array, 1:2, var)/dim(out_array)[3])
})


results_bias_rel <- lapply(ss, function(s){
  out_list <- lapply(R[s], function(x) {
    return(x$results$D /x$results$P) # relative bias
  }) 
  Reduce("+", out_list)/length(out_list)
})

results_bias_rel_sse <- lapply(ss, function(s){
  out_array <- lapply(R[s], function(x) {
    return(x$results$D /x$results$P) # relative bias
  }) %>% simplify2array()
  sqrt(apply(out_array, 1:2, var)/dim(out_array)[3])
})



results_bias_abs %>% lapply(function(x) {x[1:4, 1:4]})
results_bias_abs %>% lapply(function(x) {x[5:10, 5:10]})
results_bias_rel %>% lapply(function(x) {x[1:4, 1:4]})
results_bias_rel %>% lapply(function(x) {x[5:10, 5:10]})

results_bias_abs_sse %>% sapply(mean)
results_bias_rel_sse %>% sapply(mean)

## save results
saveRDS(JP, file.path(out.dir, "EstiML_SIM_V1_JP.rds"))
saveRDS(R, file.path(out.dir, "EstiML_SIM_V1_R.rds"))
saveRDS(results_bias_abs, file.path(out.dir, "EstiML_SIM_V1_results_bias_abs.rds"))
saveRDS(results_bias_rel, file.path(out.dir, "EstiML_SIM_V1_results_bias_rel.rds"))
saveRDS(results_bias_abs_sse, file.path(out.dir, "EstiML_SIM_V1_results_bias_abs_sse.rds"))
saveRDS(results_bias_rel_sse, file.path(out.dir, "EstiML_SIM_V1_results_bias_rel_sse.rds"))


# VIZ -----------------------------------------------------------------------------------------

library(ggplot2)

M <- results_bias_rel[[1]]

# https://dgopstein.github.io/articles/spot-matrix/



?reshape2::melt

results_bias_abs_long <- reshape2::melt(results_bias_abs, value.name="bias")  %>% 
  rename(estimator = Var1, estimand = Var2) %>% 
  rename(scenario = "L1") %>% 
  mutate(sd_s = sapply(strsplit(scenario, split="|", fixed=TRUE), function(x)x[1]) %>% as.numeric()) %>% 
  mutate(sd_t = sapply(strsplit(scenario, split="|", fixed=TRUE), function(x)x[2]) %>% as.numeric()) %>% 
  mutate(rho_s = sapply(strsplit(scenario, split="|", fixed=TRUE), function(x)x[3]) %>% as.numeric()) %>% 
  mutate(rho_t = sapply(strsplit(scenario, split="|", fixed=TRUE), function(x)x[4]) %>% as.numeric()) %>% 
  mutate(variability = sd_s + sd_t) %>% 
  mutate(variability = recode(variability, "3.0"="strong", "2.0"= "medium", "1.5" = "weak") %>% 
           ordered(levels = c("weak", "medium", "strong"))) %>% 
  mutate(heterogeneity = rho_s + rho_t) %>% 
  mutate(heterogeneity = recode(heterogeneity, "0.75" = "large", "1.25" = "small")  %>% 
           ordered(levels = c("small", "large"))) %>% 
  mutate(dir = case_when(estimator == estimand ~ "zero", bias > 0 ~ "pos", bias < 0 ~ "neg"))
  

results_bias_rel_long <- reshape2::melt(results_bias_rel, value.name="bias") %>% 
  rename(estimator = Var1, estimand = Var2) %>% 
  rename(scenario = "L1") %>% 
  mutate(sd_s = sapply(strsplit(scenario, split="|", fixed=TRUE), function(x)x[1]) %>% as.numeric()) %>% 
  mutate(sd_t = sapply(strsplit(scenario, split="|", fixed=TRUE), function(x)x[2]) %>% as.numeric()) %>% 
  mutate(rho_s = sapply(strsplit(scenario, split="|", fixed=TRUE), function(x)x[3]) %>% as.numeric()) %>% 
  mutate(rho_t = sapply(strsplit(scenario, split="|", fixed=TRUE), function(x)x[4]) %>% as.numeric()) %>% 
  mutate(variability = sd_s + sd_t) %>% 
  mutate(variability = recode(variability, "3.0"="strong", "2.0"= "medium", "1.5" = "weak") %>% 
           ordered(levels = c("weak", "medium", "strong"))) %>% 
  mutate(heterogeneity = case_when(rho_s + rho_t == 1.25 ~ "small", rho_s + rho_t == 0.75 ~ "large")  %>% 
           ordered(levels = c("small", "large"))) %>%
  mutate(dir = case_when(estimator == estimand ~ "zero", bias > 0 ~ "pos", bias < 0 ~ "neg"))


head(results_bias_abs_long)
unique(results_bias_abs_long$scenario)
names(results_bias_abs) 

results_bias_rel_long$variability %>%  table()
results_bias_rel_long$heterogeneity %>% table()

# esti_labels <- c(
#   "HOV", bquote("T" <= 4), bquote("S" != ""), bquote("S" != ""),
#   "CV", bquote({"S" * symbol("\306") *" | T" } != ""  ), "c",
#   "d", "e", "f"
# )

## PREP
esti_labels <- c(
  "C-HOV",
  "C-T(4)",
  "C-S(d)",
  "C-T(4)-S(d)",
  "U-CV5",
  "U-T(2)",
  "U-S(d)",
  "U-S(i)",
  "U-T(2)-S(d)",
  "U-T(2)-S(i)"
)

owntheme <- theme(legend.position="none",
                  text = element_text(size = 18),
                  title = element_text(size = 21, face = "bold"),
                  strip.text = element_text(size = 18),
                  axis.text = element_text(size = 18, face = "bold"),
                  axis.title = element_text(size = 21, face = "bold"))

f <- 1

## PLOT 1: conditional | single scenario 

sel <- 1:4
tt <- "Relative bias (%) for estimation of conditional performance (balanced accuracy)"
png("figures/EstiML_SIM_rel_bias_cond_sel.png", width=1600, height=900, res=100)

results_bias_rel_long %>%
  filter(estimand %in% sel, estimator %in% sel) %>% 
  filter(variability == "medium", heterogeneity=="small") %>% 
  ggplot(aes(factor(estimand), factor(estimator))) + 
  geom_point(aes(size = abs(bias)^(f), col=dir)) + 
  geom_text(aes(label = round(bias*100, 2)), vjust=2, size=5) +
  ggtitle(tt) +
  scale_x_discrete(name="Estimand", breaks = sel, labels = esti_labels[sel]) +
  scale_y_discrete(name="Estimator", breaks = sel, labels = esti_labels[sel], limits=rev) +
  scale_color_manual(values=c("#56B4E9", "#E69F00", "#999999" )) +
  scale_size_continuous(range = c(0,5)) +
  owntheme

dev.off()

## PLOT 2: conditional | all scenarios
sel <- 1:4
tt <- "Relative bias (%) for estimation of conditional performance (balanced accuracy)"
png("figures/EstiML_SIM_rel_bias_cond_all.png", width=1600, height=900, res=100)

results_bias_rel_long %>%
  filter(estimand %in% sel, estimator %in% sel) %>% 
  #filter(variability == "medium", heterogeneity=="small") %>% 
  ggplot(aes(factor(estimand), factor(estimator))) + 
  geom_point(aes(size = abs(bias)^(f), col=dir)) + 
  geom_text(aes(label = round(bias*100, 2)), vjust=2, size=5) +
  ggtitle(tt) +
  scale_x_discrete(name="Estimand", breaks = sel, labels = esti_labels[sel]) +
  scale_y_discrete(name="Estimator", breaks = sel, labels = esti_labels[sel], limits=rev) +
  scale_color_manual(values=c("#56B4E9", "#E69F00", "#999999" )) +
  scale_size_continuous(range = c(0,5)) +
  owntheme +
  facet_wrap(heterogeneity ~ variability, 
             labeller = label_bquote(atop(.(as.character(heterogeneity)) ~ "heterogeneity ",
                                          .(as.character(variability)) ~ "variability"))) 
dev.off()


## PLOT 3: UNconditional | single scenario 
sel <- 5:10
tt <- "Relative bias (%) for estimation of unconditional performance (balanced accuracy)"
png("figures/EstiML_SIM_rel_bias_uncond_sel.png", width=1600, height=900, res=100)

results_bias_rel_long %>%
  filter(estimand %in% sel, estimator %in% sel) %>% 
  filter(variability == "medium", heterogeneity=="small") %>% 
  ggplot(aes(factor(estimand), factor(estimator))) + 
  geom_point(aes(size = abs(bias)^(f), col=dir)) + 
  geom_text(aes(label = round(bias*100, 2)), vjust=2, size=5) +
  ggtitle(tt) +
  scale_x_discrete(name="Estimand", breaks = sel, labels = esti_labels[sel]) +
  scale_y_discrete(name="Estimator", breaks = sel, labels = esti_labels[sel], limits=rev) +
  scale_color_manual(values=c("#56B4E9", "#E69F00", "#999999" )) + 
  scale_size_continuous(range = c(0,10)) +
  owntheme 

dev.off()


  
  
## PLOT 4: UNconditional | all scenarios
sel <- 5:10
tt <- "Relative bias (%) for estimation of unconditional performance (balanced accuracy)"
png("figures/EstiML_SIM_rel_bias_uncond_all.png", width=1600, height=900, res=100)

results_bias_rel_long %>%
  filter(estimand %in% sel, estimator %in% sel) %>% 
  #filter(variability == "medium", heterogeneity=="small") %>% 
  ggplot(aes(factor(estimand), factor(estimator))) + 
  geom_point(aes(size = abs(bias)^(f), col=dir)) + 
  geom_text(aes(label = round(bias*100, 2)), vjust=2, size=5) +
  ggtitle(tt) +
  scale_x_discrete(name="Estimand", breaks = sel, labels = esti_labels[sel]) +
  scale_y_discrete(name="Estimator", breaks = sel, labels = esti_labels[sel], limits=rev) +
  scale_color_manual(values=c("#56B4E9", "#E69F00", "#999999" )) + 
  scale_size_continuous(range = c(0,10)) +
  owntheme +
  facet_wrap(heterogeneity ~ variability, 
             labeller = label_bquote(atop(.(as.character(heterogeneity)) ~ "heterogeneity ",
                                          .(as.character(variability)) ~ "variability"))) 
dev.off()

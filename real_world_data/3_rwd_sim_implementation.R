# Dependencies ---------------------------------------------------------------------------------
library(dplyr)
library(data.table)
library(parallel)
library(batchtools)
library(mlr3) 
library(mlr3tuningspaces)
library(mlr3learners)
library(mlr3pipelines)

# Preparation -------------------------------------------------------------
rm(list = ls())                           # clear environment
source("_setup.R")
source(file.path("real_world_data", "0_rwd_functions.R"))

Nsim <- 20                                      # number of simulations
cr <- 0.8                                       # fraction of cores used
pkgs <- c("dplyr", "mlr3", "mlr3tuningspaces", "mlr3learners", "mlr3pipelines",
          "glmnet", "ranger", "xgboost", "e1071", "data.table") # required packages
main_dir <- file.path(rwd_dir)                  # main folder
reg_dir <- file.path(main_dir, "sim_registry")        # registry folder
out_dir <- file.path(rwd_dir, "sim_results")    # results folder
dir.create(out_dir)

# Design Parameters -------------------------------------------------------
prob <- list()
prob[["sample_hp"]] <- CJ(
  algo = c(
    "classif.glmnet.default",
    "classif.rpart.default",
    "classif.svm.default",
    "classif.ranger.default",
    "classif.xgboost.default"
  ),
  method = "random",
  n_hp = 5
)

# TODO: for country diff: reduce number of included countries to 5-10
# TODO: ADD uk_cv_5_a
# TODO: ADD c7 analysis (7 most countires)
# TODO: same country
algo <- list()
algo[["conduct_ml_study"]] <- CJ(
  outcome = c("FDEAD"), # "ID14
  missing = c("cc", "cc05", "mode"),
  splits_name = c("uk_cv_10", "uk_cv_5", "uk_sd_y3", "uk_ss_y3", "uk_sd_y0"), 
  subsample = c(100, Inf), 
  n_trn_min = 10,
  n_val_min = 1,
  rwd_dir = rwd_dir
)

message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", nrow(algo[[1]]))


# Registry setup ----------------------------------------------------------
reg <- makeExperimentRegistry(file.dir = reg_dir, seed = 1)
reg <- loadRegistry(reg_dir, writeable = TRUE)

reg$packages <- pkgs
reg$source <- file.path("real_world_data", "0_rwd_functions.R")
reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))


addProblem(name = "sample_hp", fun = sample_hp, reg = reg, seed = 1)
addAlgorithm(name = "conduct_ml_study", fun = conduct_ml_study, reg = reg)

addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls = Nsim,
               reg = reg)

summarizeExperiments(reg = reg)

# Run jobs ------------------------------------------------------------------------------------
# Test job:
j <- findNotDone()[1]
j
r <- testJob(j, reg=reg)
r
unwrap(getJobPars(j))

Sys.time()
submitJobs(findNotDone(reg=reg), reg=reg) 
Sys.time()

#Sys.sleep(180)
getStatus(reg=reg)

# Optional steps ------------------------------------------------------------------------------
#findExpired(reg=reg)
#removeExperiments(findNotDone(reg=reg), reg=reg)
#waitForJobs(reg=reg)
#getErrorMessages()
#killJobs(reg=reg)
#clearRegistry(reg=reg)



# Export results --------------------------------------------------------
## jobs
P <- getJobPars(findDone(reg = reg),reg = reg) %>% 
  unwrap() %>% 
  select(-algorithm, -problem, -rwd_dir)

## results
rlist <- reduceResultsList(findDone(reg = reg), reg = reg)

R <- lapply(seq_along(rlist), function(job.id){
  hp <- rlist[[job.id]]$info$hp; hp <- rename(hp, hp=model); 
  hp$hpchr <- apply(hp, 1, paste0, collapse="|"); hp <- select(hp, hp, hpchr);
  
  cbind(job.id=job.id,
        full_join(rlist[[job.id]]$meta,
                  rlist[[job.id]]$results, by = "split") %>% 
          full_join(hp, by="hp"))
       
}) %>% data.table::rbindlist() %>% 
  select(-month_val)
 

C <- full_join(P, R, by="job.id") %>% 
  mutate(algo = recode(algo,
                       classif.glmnet.default = "glmnet",
                       classif.ranger.default = "ranger",
                       classif.rpart.default = "rpart",
                       classif.svm.default = "svm",
                       classif.xgboost.default = "xgboost"))

readr::write_csv(C, file.path(out_dir, "estiml_sim_results.csv"))

## test: 
library(ggplot2)

A <- C %>% 
  mutate(label = as.numeric(label == "TRUE.")) %>% 
  group_by(outcome, missing, splits_name, subsample, algo, hpchr, label) %>%
  filter(!is.na(correct)) %>% 
  summarize(acc=mean(correct)) %>% 
  tidyr::pivot_wider(values_from = "acc", names_from="label", names_prefix="acc") %>% 
  mutate(bacc = 0.5*acc0 + 0.5*acc1)



dim(A)
head(A)

W <- A %>% 
  select(-acc0, -acc1) %>% 
  tidyr::pivot_wider(values_from = "bacc", names_from="splits_name", names_prefix="bacc_") %>% 
  select(-bacc_NA) %>%  tidyr::drop_na() %>% 
  mutate(rel_bias_sd_y0 = (bacc_uk_cv_5 - bacc_uk_sd_y0)/bacc_uk_sd_y0 ) %>% 
  mutate(rel_bias_sd_y3 = (bacc_uk_cv_5 - bacc_uk_sd_y3)/bacc_uk_sd_y3 ) %>% 
  mutate(rel_bias_ss_y3 = (bacc_uk_cv_5 - bacc_uk_ss_y3)/bacc_uk_ss_y3 )
  
dim(W)
W

C$correct %>% is.na %>% mean
A$acc %>% is.na %>% mean

dim(W)
head(W)

W %>% 
  # filter(hp %in% as.character((11:15)/10)) %>% 
  filter(algo == "classif.glmnet.default", missing=="cc", subsample==100) 


W %>% 
  ungroup() %>% 
  select(-hpchr) %>% 
  group_by(outcome, missing, subsample, algo) %>% 
  summarize_all(median) %>%
  filter(subsample == 100) %>% 
  print(n=Inf)
  
  

A %>% filter(missing == "mode", subsample == 100) %>% 
  ggplot(aes(algo, bacc)) + 
  geom_boxplot() +
  facet_wrap(~splits_name) 

W %>% 
  filter(subsample==100) %>% 
  ggplot(aes(algo, rel_bias_ss_y3)) + 
  geom_hline(yintercept=0, color="blue", lwd=1.1) +
  geom_boxplot() +
  stat_summary(fun=mean, geom = "point", color="red", pch=18, size=2)+
  facet_wrap(subsample~missing,
             labeller = label_bquote("subset = " ~ .(subsample) ~ " | " ~ "missing =" ~ .(missing) ))+
  ylim(-0.1, 0.4) +
  xlab("") + 
  ylab(bquote(bold("Relative Bias: [CV5] - [same site, next year]")))

ggsave(file.path(rwd_dir, "rel_bias_ss_y3.jpg"), device="jpg",
        width=1600, height=900, units = "px", dpi=300, scale=1.5)

W %>% 
  filter(subsample==100) %>% 
  ggplot(aes(algo, rel_bias_sd_y3)) + 
  geom_hline(yintercept=0, color="blue", lwd=1.1) +
  geom_boxplot() +
  stat_summary(fun=mean, geom = "point", color="red", pch=18, size=2)+
  facet_wrap(subsample~missing,
             labeller = label_bquote("subset = " ~ .(subsample) ~ " | " ~ "missing =" ~ .(missing) ))+
  ylim(-0.1, 0.4) +
  xlab("") + 
  ylab(bquote(bold("Relative Bias: [CV5] - [different site, next year]")))

ggsave(file.path(rwd_dir, "rel_bias_sd_y3.jpg"), device="jpg",
       width=1600, height=900, units = "px", dpi=300, scale=1.5)

W %>% 
  filter(subsample==100) %>% 
  ggplot(aes(algo, rel_bias_sd_y0)) + 
  geom_hline(yintercept=0, color="blue", lwd=1.1) +
  geom_boxplot() +
  stat_summary(fun=mean, geom = "point", color="red", pch=18, size=2)+
  facet_wrap(subsample~missing,
             labeller = label_bquote("subset = " ~ .(subsample) ~ " | " ~ "missing =" ~ .(missing) ))+
  ylim(-0.1, 0.4) +
  xlab("") + 
  ylab(bquote(bold("Relative Bias: [CV5] - [different site, same year]")))
ggsave(file.path(rwd_dir, "rel_bias_sd_y0.jpg"), device="jpg",
       width=1600, height=900, units = "px", dpi=300, scale=1.5)





C %>% filter(missing == "cc05", subsample == 100)




# EXPERIMENTAL AREA ---------------------------------------------------------------------------
getErrorMessages() %>% getJobPars() %>% unwrap()

splits <- read_splits(rwd_dir, "ID14", "mode", "uk_ss_yd")
data <- import_data("ID14", "mode", rwd_dir)

sapply(splits, function(s){
  data[s$trn, "ID14"] %>% unique()
})


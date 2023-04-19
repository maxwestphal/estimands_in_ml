library(batchtools)
library(data.table)
library(parallel)

# Preparation -------------------------------------------------------------
setwd("~")
rm(list = ls())                           # clear environment


Nsim <- 100                            # number of simulations
cr <- 0.8                                 # fraction of cores used
req.packages <- c("caret", "dplyr")        # required packages
main.dir <- file.path(getwd(), "BatchtoolSimulationsMW")  # main folder
reg.dir <- paste0(main.dir, "/IST_training") # registry folder
out.dir <- paste0(main.dir, "/IST_training_RESULTS")    # results folder
# Data

df <- read.csv2("BatchtoolSimulationsMW/IST_final_data.csv")[,-1] %>% 
  mutate_at(c(1:5, 7:12, 14:28, 30:51), as.factor) %>% 
  mutate("ID14" = make.names(ID14))



# Design Parameters -------------------------------------------------------
prob <- list()
prob[["hyperparams"]] <- CJ(method = c("glmnet"))

algo <- list()
algo[["training_models"]] <- CJ(estimand = c("random", "geographic", "temporal"))

message("Number of problem parameters: ", nrow(prob[[1]]))
message("Number of algorithm arguments: ", nrow(algo[[1]]))


# Registry setup ----------------------------------------------------------
reg <- makeExperimentRegistry(file.dir = reg.dir,
                              source = "simulation_functions.R",
                              packages = req.packages, seed = 1)

reg$cluster.functions <- makeClusterFunctionsSocket(round(cr*detectCores()))
reg <- loadRegistry(reg.dir, writeable = TRUE)

addProblem(name = "hyperparams", data = df, fun = get_hyperparameters, reg = reg, seed = 8787)
addAlgorithm(name = "training_models", fun = training_and_prediction, reg = reg)

addExperiments(prob.designs = prob,
               algo.designs = algo,
               repls = Nsim,
               reg = reg)

summarizeExperiments(reg = reg)

## run jobs
submitJobs(reg = reg)
getErrorMessages()



# Evaluate results --------------------------------------------------------
## problems
P <- getJobPars(findDone(reg = reg),reg = reg) %>% unwrap() %>% select(-algorithm, -problem)

## results
R <- reduceResultsList(1, reg = reg) %>% bind_rows()
R <- mutate(R, job = rep(1, nrow(R)), .before = estimand)
for (i in 2:300) {
  R_new <- reduceResultsList(i, reg = reg) %>% bind_rows()
  R_new <- mutate(R_new, job = rep(i, nrow(R_new)), .before = estimand)
  R <- rbind(R, R_new)
}

accuracy_per_fold <- R %>% group_by(hyperparams, estimand, fold) %>% 
  summarize(sensitivity = sum(observed == "X1" & predicted == "X1")/sum(observed == "X1"),
            specificity = sum(observed == "X0" & predicted == "X0")/sum(observed == "X0"),
            balanced_accuracy = mean(c(sensitivity, specificity)))

accuracy_per_job <- accuracy_per_fold %>% group_by(hyperparams, estimand) %>% 
  summarize(mean_balanced_accuracy = mean(balanced_accuracy, na.rm = T),
            var_balanced_accuracy_between_folds = var(balanced_accuracy, na.rm = T))

accuracy_per_estimand <- accuracy_per_job %>% group_by(estimand) %>% 
  summarize(mean_mean_balanced_accuracy = mean(mean_balanced_accuracy, na.rm = T),
            var_mean_balanced_accuracy_between_jobs = var(mean_balanced_accuracy),
            mean_var_balanced_accuracy_between_folds = mean(var_balanced_accuracy_between_folds),
            var_var_balanced_accuracy_between_folds = var(var_balanced_accuracy_between_folds))

write.csv2(accuracy_per_fold, paste0(out.dir, "/accuracy_per_fold.csv"))
write.csv2(accuracy_per_job, paste0(out.dir, "/accuracy_per_job.csv"))
write.csv2(accuracy_per_estimand, paste0(out.dir, "/accuracy_per_estimand.csv"))

# pairwise difference
difference_per_job <- accuracy_per_job %>% group_by(hyperparams) %>% 
  summarize(random_geographic = mean_balanced_accuracy[estimand == "random"] - mean_balanced_accuracy[estimand == "geographic"],
            random_temporal = mean_balanced_accuracy[estimand == "random"] - mean_balanced_accuracy[estimand == "temporal"],
            temporal_geographic = mean_balanced_accuracy[estimand == "temporal"] - mean_balanced_accuracy[estimand == "geographic"])

overall_difference <- difference_per_job %>% 
  summarize(mean_diff_random_geographic = mean(random_geographic),
            var_diff_random_geographic = var(random_geographic),
            min_diff_random_geographic = min(random_geographic),
            max_diff_random_geographic = max(random_geographic),
            mean_diff_random_temporal = mean(random_temporal),
            var_diff_random_temporal = var(random_temporal),
            min_diff_random_temporal = min(random_temporal),
            max_diff_random_temporal = max(random_temporal),
            mean_diff_temporal_geographic = mean(temporal_geographic),
            var_diff_temporal_geographic = var(temporal_geographic),
            min_diff_temporal_geographic = min(temporal_geographic),
            max_diff_temporal_geographic = max(temporal_geographic))

par(mfrow = c(2,2))
hist(difference_per_job$random_geographic, 
     main = "random - geographic", 
     xlab = "Difference of balanced accurcy with same hyperparameters", xlim = c(-0.1, 0.1))
hist(difference_per_job$random_temporal, 
     main = "random - temporal", 
     xlab = "Difference of balanced accurcy with same hyperparameters", xlim = c(-0.1, 0.1))
hist(difference_per_job$temporal_geographic, 
     main = "temporal - geographic", 
     xlab = "Difference of balanced accurcy with same hyperparameters", xlim = c(-0.1, 0.1))


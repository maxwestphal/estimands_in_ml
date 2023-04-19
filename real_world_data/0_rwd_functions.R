# Functions for data splitting ----------------------------------------------------------------

## function to generate and save single split:
save_splits <- function(rwd_dir, outcome, missing, split_type, seed=NULL, suffix=NULL){
  
  ## load data as specified by missing
  data <- import_data(outcome, missing, rwd_dir)
  
  ## derive splits:
  splits <- split_data(data, split_type, seed=seed)
  
  ## prepare saving:
  splits_name <- paste0(split_type, ifelse(!is.null(seed), paste0("_", suffix), ""))
  target_dir <- file.path(rwd_dir, "data", outcome, missing, "splits")
  dir.create(target_dir, showWarnings = FALSE)
  
  ## save:
  saveRDS(splits, file.path(target_dir, paste0(splits_name, ".rds")))
  
  return(invisible(NULL))
}

## function to generate and save multiple splits:
save_splits_list <- function(rwd_dir, outcome, missing, splits_desc){
  
  lapply(splits_desc, function(x){
    save_splits(rwd_dir,
                outcome,
                missing, 
                split_type = x$split_type,
                seed = x$seed,
                suffix = x$suffix)
  })
  
  return(invisible(NULL))
}




# Functions for simulation study --------------------------------------------------------------
# TODO (MW): check whether we should switch to e.g. mlr3
# methods I tried:
# "ranger" -> uses is.factor(y) and nrow/ncol(x)
# "rpartCost", "rpart2" -> performs an initial fit on x and y inside grid function
# "knn" -> uses is.factor(y) and nrow(x)
# "svmLinearWeights2" -> does not provide probability model, can look this up via caret::modelLookup("svmLinearWeights2")

export_data <- function(df){
  
  nn <- strsplit(df, "_")[[1]]
  outcome <- nn[2]
  missing <- nn[3]
  out_dir <- file.path(rwd_dir, "data", outcome, missing)
  dir.create(file.path(out_dir, "splits"),  recursive = TRUE, showWarnings=FALSE)
  saveRDS(eval(parse(text=df)), file.path(out_dir, "data.rds"))
  
  #readr::write_csv(data, file.path(out_dir, "data.csv"))
}

sample_hp <- function(algo = "classif.glmnet.default", #
                      method = "random",
                      n_hp = 10,
                      data = NULL,
                      job = NULL){
  
  
  
  
  # algo <- "classif.glmnet.default"
  # algo <- "classif.rpart.default"
  # algo <- "classif.svm.default"
  # algo <- "classif.ranger.default"
  # algo <- "classif.xgboost.default"
  
  ts <- mlr3tuningspaces::lts(algo)
  learner <- ts$get_learner()
  learner$predict_type <- "prob"
  
  if(algo == "classif.svm.default"){
    learner$param_set$values$type = "C-classification"
  }
  
  ps <- learner$param_set$search_space()
  design <- generate_design_random(ps, n_hp)
  
  instance <- list(learner=learner, design=design)
  
  return(instance)
  
  
  #hp <- caret::getModelInfo(method, regex=FALSE)[[method]]$grid(len = 1, search = "random")
  #return(list(method = method, hp = hp))
}

# TODO: implement downsample argument for conduct_ml_study: 
# FALSE: do nothing (current impl)
# TRUE: read all training set sizes n_trn of current split, even those are unequal (usual case),
# then randomly draw a subsample of each training idx vector, each of size min(n_trn), i.e.
# all training data sets have same size if downsample = TRUE

get_trn_sets <- function(splits){
  lapply(splits, function(x) x$trn)
}

get_val_sets <- function(splits){
  lapply(splits, function(x) x$val)
}

conduct_ml_study <- function(outcome = c("ID14", "FDEAD"), 
                             missing = "cc",
                             splits_name = "cv_4_a", 
                             subsample = Inf, 
                             n_trn_min = 10,
                             n_val_min = 1,
                             rwd_dir, 
                             data = NULL,
                             instance = NULL,
                             job = NULL){
  
  outcome <- match.arg(outcome)
  
  data <- prep_data(rwd_dir, missing, outcome)
  task <- prep_task(data, outcome) 
  y <- (task$data() %>% as.data.frame() %>% {.[,outcome, drop=TRUE]}) == "TRUE."
  
  
  splits <- read_splits(rwd_dir, outcome, missing, splits_name) %>% 
    subsample_splits(n_trn_new = subsample) %>% 
    filter_splits(n_trn_min = n_trn_min, 
                  n_val_min = n_val_min, 
                  y=y,
                  n1_trn_min = 5, n0_trn_min=5) 
  #length(splits)
  
  # TODO: remove
  #message("[estiml]")
  #message(n_val_min)
  
  exp_design <- rsmp("custom")
  exp_design$instantiate(task,
                         train_sets = get_trn_sets(splits), 
                         test_sets = get_val_sets(splits)) 
  #exp_design$instance %>% str() # TODO: remove
  
  ## prep output:
  #results <- data.frame() # stores predictions, labels (nrow = nobs)
  #meta <- data.frame() # stores n_trn, n_val (nrow = length(split))
  
  
  #as.data.table(mlr_pipeops) %>% filter(key=="encode")
  
  #?po
  learner <- po("encode") %>>% instance$learner
  #learner$param_set
  
  # gr = pipeline_robustify(task, learner) %>>% po("learner", learner)
  # ede = ?resample(task, GraphLearner$new(gr), rsmp("holdout"))
  # tsk_regr1 = ede$task$clone()
  
  # TODO: https://mlr3book.mlr-org.com/pipelines.html
  
  design <- instance$design$data
  names(design) <- paste0(instance$learner$id, ".", names(design))

  #?mlr3tuning::tune
  
  ## training:
  learner_tuned <- mlr3tuning::tune(
    method = "design_points",
    task = task,
    learner = learner, #GraphLearner$new(gr), #learner,
    resampling = exp_design,
    measures = msrs("classif.acc"),
    design = design
    #batch_size = 1
  )
  
  #learner_tuned %>% str(1)
  
  L <- nrow(instance$design$data)
  
  pred_list <- lapply(1:L, function(l) {learner_tuned$archive$predictions(i=l)} )
  
  ## predictions:
  results <- lapply(1:L, function(l){
    lapply(1:length(pred_list[[l]]), function(s){
      cbind(hp=l, split=s, 
            data.table::as.data.table(pred_list[[l]][[s]])
      ) 
    }) %>% 
      data.table::rbindlist()
  }) %>% 
    data.table::rbindlist() %>% 
    rename(pred = response, label=truth, pred_pr = prob.TRUE.) %>% 
    select(- prob.FALSE.) %>% 
    mutate(correct = pred == label) 
  
  #sapply(get_trn_sets(splits), length)
  
  ## metadata:
  meta <- get_meta(splits, data, "val")

  
  
  ## stores info on method, hp (nrow = 1):
  info <- list(method = instance$learner$id, 
               hp = data.table(model = 1:L, instance$design$data))
  
  return(list(results = results, meta=meta, info=info))
  
}

read_splits <- function(rwd_dir, outcome, missing, splits_name){
  readRDS(file.path(rwd_dir, "data", outcome, missing, "splits", paste0(splits_name, ".rds")))
}

import_data <- function(outcome, missing, rwd_dir){
  readRDS(file.path(rwd_dir, "data", outcome, missing, "data.rds"))
  # readr::read_csv(file.path(rwd_dir, "data", paste0("data_", outcome, "_", missing, ".csv")),
  #                 show_col_types = FALSE)
}

get_meta <- function(splits, data, part="val"){

  meta1 <- data.table::data.table(
    split = 1:length(splits),
    n_trn = sapply(get_trn_sets(splits), length),
    n_val = sapply(get_val_sets(splits), length)
  )
  
  meta2 <- lapply(splits, function(s){
    data[s[[part]], c("HOSPNUM", "COUNTRY", "MONTH", "YEAR") ] %>% 
      apply(2, unique) %>% 
      lapply(function(x) paste0(x, collapse = "|")) %>% 
      do.call(cbind, .) %>% 
      as.data.table() %>% 
      setnames(new=c("hosp_val", "country_val", "month_val", "year_val"))
  }) %>% 
    data.table::rbindlist()
  
  return(cbind(meta1, meta2))
}

prep_data <- function(rwd_dir, missing, outcome){

  if(outcome == "ID14"){
    data <- import_data(outcome, missing, rwd_dir) %>% 
      mutate(ID14 = make.names(ID14)) %>% 
      #dplyr::select(-all_of(vars_excl)) %>% 
      as.data.frame()
  }
  if(outcome == "FDEAD"){
    data <- import_data(outcome, missing, rwd_dir) %>% 
      mutate(FDEAD = make.names(FDEAD)) %>% 
      #dplyr::select(-all_of(vars_excl)) %>% 
      as.data.frame()
  }

  return(data)
}

prep_task <- function(data, outcome){
  
  vars_excl <- c("HOSPNUM", "COUNTRY", "MONTH", "YEAR") 
  
  task <- as_task_classif(data %>% dplyr::select(-all_of(vars_excl)),
                          target = outcome,
                          positive = "TRUE.", 
                          id = "esti_task")
  
}



hp2char <- function(hp){
  paste(do.call(paste, list(names(hp),
                            round(as.numeric(hp), 5),
                            sep="=")), collapse="_")
}

# TODO: this is needed for split_data_year_lag1 and prob for other furture splits as well
# Question: how to define n_trn_min
filter_splits <- function(splits, n_trn_min=10, n_val_min=1, 
                          y = NULL,
                          n1_trn_min = 1, n0_trn_min=1,
                          n1_val_min = 0, n0_val_min=0){
  valid <- sapply(splits, function(x){
    (length(x$trn) >= n_trn_min) & (length(x$val) >= n_val_min)
  })
  if(!is.null(y)){
    valid_01 <- sapply(splits, function(x){
      (sum(y[x$trn]) >= n1_trn_min) & 
        (sum(!y[x$trn] ) >= n0_trn_min) & 
        (sum(y[x$val] ) >= n1_val_min) & 
        (sum(!y[x$val] ) >= n0_val_min) 
    })
    valid <- valid & valid_01
  }
  
  return(splits[valid])
}

# TODO: implement
subsample_splits <- function(splits, n_trn_new = Inf){
  
  stopifnot(is.finite(n_trn_new) | is.infinite(n_trn_new))
  n_trn_new <- round(n_trn_new)
  stopifnot(n_trn_new > 0)
  
  lapply(splits, subsample_split, n_trn_new=n_trn_new)
}

subsample_split <- function(split, n_trn_new = Inf){
  n_trn <- length(split$trn)
  n_val <- length(split$val)
  
  if(n_trn <= n_trn_new){
    if(is.finite(n_trn_new)){
      message("[estiml] Cannot subsample, too few training samples.")
    }
    return(split)
  }
  
  split_new <- list(trn=base::sample(split$trn, size=n_trn_new, replace=FALSE),
                    val=split$val)
  return(split_new)
  
} 

check_splits <- function(splits){
  all(sapply(splits, check_split))
}

check_split <- function(split){
  length(intersect(split$trn, split$val)) == 0
}

# subsample_splits(splits, n_trn_new = 50000) %>% str(2)

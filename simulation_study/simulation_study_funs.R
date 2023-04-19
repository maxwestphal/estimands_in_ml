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
#   simulation_study_funs.R
###
#   Remarks:
#
###

ml_study <- function(n_cell = 250,
                     n_cell_pop = 1000,
                     n_vars = 200,
                     n_vars_infl = 40, 
                     n_vars_rdm = 20, 
                     n_time = 10,
                     n_site = 5,
                     beta_0 = -2, # intercept
                     sd_b = 0.5, # variation of base coefficients
                     sd_s = 0.25, # variation between sides
                     rho_s = 0.5, # correlation between sites
                     sd_t = 0.05, # variation between time periods
                     rho_t = 0.75, # correlation between periods
                     metric = "bacc",
                     algo = "en",
                     equal_trn = FALSE,
                     data = NULL,
                     job = NULL
){
  
  ## argument checks:
  stopifnot(n_vars >= n_vars_infl)
  stopifnot(n_vars_infl >= n_vars_rdm)
  
  message("ml_study: generating data...")
  ## attribute labels:
  sites <- LETTERS[1:n_site]
  times <- 1:n_time
  
  ## intercept parameter:
  beta_0 <- -2
  
  ## slope parameters:
  beta <- rep(0, n_vars)
  
  ## slope with influence:
  beta[1:n_vars_infl] <- rnorm(n_vars_infl, 0, sd_b) 
  
  random_effects <- gen_random_effects(n_vars_rdm=n_vars_rdm, intercept_rdm=TRUE, 
                                       n_site=n_site, sites=sites, sd_s=sd_s, rho_s=sd_s,
                                       n_time=n_time, times=times, sd_t=sd_t, rho_t=rho_t)
  
  
  ## define generative model:
  gen_model <- define_gen_model(beta_0 = beta_0, beta=beta, random_effects=random_effects) 
  
  ## generate development data:
  attr_dev <- gen_attr_data(n_cell, times = times, sites=sites)
  n_dev <- nrow(attr_dev)
  feat_dev <- gen_feature_data(n = n_dev, n_vars= n_vars)
  outp_dev <- gen_output_data(attr_data=attr_dev, feat_data=feat_dev, gen_model=gen_model)
  
  ## generate population test data:
  attr_pop <- gen_attr_data(n_cell_pop, times = times, sites=sites)
  n_pop <- nrow(attr_pop)
  feat_pop <- gen_feature_data(n = n_pop, n_vars= n_vars)
  outp_pop <- gen_output_data(attr_data=attr_pop, feat_data=feat_pop, gen_model=gen_model)
  
  ## data splits for development:
  splits_dev <- create_splits_dev(attr_dev, attr_pop)
  
  ## subsample training set if needed:
  if(equal_trn){
    n_trn_min <- sapply(splits_dev, function(split){
      sapply(split, function(sets) length(sets$trn) ) %>% min()
    }) %>% min()
    splits_dev <- lapply(splits_dev, function(split){ 
      lapply(split, function(sets){
        sets$trn <- sample(sets$trn, n_trn_min)
        return(sets)
      })
    })
  }
  
  n_esti <- length(splits_dev)
  models <- list()
  
  ## model training:
  message("ml_study: training models...")
  for(i in 1:n_esti){
    
    # splt <- splits_dev[[i]][[1]]
    models[[i]] <- lapply(splits_dev[[i]], function(splt){
      training(idx_trn=splt$trn, feat=feat_dev, outp=outp_dev, algo=algo)
    })
    
  }
  
  ## create matrices of parameters and estimates
  P <- matrix(NA, n_esti, n_esti)
  H <- matrix(NA, n_esti, n_esti)
  
  ## performance estimation:
  message("ml_study: estimating performance...")
  for(i in 1:n_esti){
    
    mods <- models[[i]]
    hh <- list()
    pp <- list()
    
    for(m in 1:length(mods)){
      
      mod <- mods[[m]]
      
      idx_val <- splits_dev[[i]][[m]]$val
      idx_pop <- splits_dev[[i]][[m]]$pop
      
      yhat_val <- inference(model=mod, idx_inf=idx_val, feat=feat_dev, outp=outp_dev, algo=algo)
      y_val <- outp_dev[idx_val, "response"]
      
      yhat_pop <- inference(model=mod, idx_inf=idx_pop, feat=feat_pop, outp=outp_pop, algo=algo)
      y_pop <- outp_pop[idx_pop, "response"]
      
      hh[[m]] <- get_metrics(yhat_val, y_val)
      pp[[m]] <- get_metrics(yhat_pop, y_pop)
      
    }
    
    H[i,  ] <- sapply(hh, function(x) x[, metric]) %>% mean()
    P[ , i] <- sapply(pp, function(x) x[, metric]) %>% mean()
    
  }
  
  stopifnot(all(is.finite(P)) & all(is.finite(H)))
  
  ## calculate deviation (bias):
  D <- H-P
  
  ## output:
  out <- list(info = list(esti_table = create_esti_table(),
                          gm_metrics = get_metrics(outp_pop$pred_gm, outp_pop$response)),
              results = list(H=H, P=P, D=D))
  return(out)
  
}


# Functions -------------------------------------------------------------------------------------
training <- function(idx_trn, feat, outp, algo="en"){
  
  model <- glmnet::glmnet(x = feat[idx_trn, ],
                          y = outp[idx_trn, "response"],
                          family = "binomial",
                          intercept = FALSE)
  
  return(model)
}

inference <- function(model, idx_inf, feat, outp, algo="en"){
  pred <- 
    predict(model, newx=feat[idx_inf, ], type="class",
            s = model$lambda[round(length(model$lambda)/2)]) %>% 
    as.numeric()
  
  return(pred)
}

create_esti_table <- function(){
  data.frame(
    esti_id = paste0("esti_", 1:10),
    esti_type = c(rep("cond", 4), rep("uncond", 6)),
    esti_site = c("none", "none", "diff", "diff", "none", "none", "diff", "diff", "equal", "equal"),
    esti_time = c("none", "leq4", "none", "leq4", "none", "leq2", "none", "leq2", "none", "leq2")
    # TODO: complete just for description ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #sites_dev = c("HO: A,B,C,D", "A,B,C", "A,B,C,D", "A,B,C", "CV5: A,B,C,D", "A,B,C,D" ), 
    #times_dev = c("HO: 1:5", "1:5", "1:4", "1:4", "CV5: 1:5", "", "", "", "", ""), 
    #fraction_dev = c("0.8", "1", "1", "1", "0.8")
  )
}

get_idx <- function(attr_data, site_sel=LETTERS[1:4], time_sel=1:5){
  which(attr_data$site %in% site_sel & attr_data$time %in% time_sel)
}


get_split_e8 <- function(attr_dev, attr_pop, frac_trn = 0.8){
  list(
    get_sets_e8(attr_dev, attr_pop, "A", frac_trn),
    get_sets_e8(attr_dev, attr_pop, "A", frac_trn),
    get_sets_e8(attr_dev, attr_pop, "A", frac_trn),
    get_sets_e8(attr_dev, attr_pop, "A", frac_trn)
  )
}

get_sets_e8 <- function(attr_dev, attr_pop, site_sel="A", frac_trn = 0.8){
  idx <- get_idx(attr_dev, site_sel=site_sel, time_sel=1:5)
  idx_trn <-  sample(idx, round(frac_trn*length(idx)))
  list(
    trn = idx_trn,
    val = setdiff(idx, idx_trn),
    pop = get_idx(attr_pop, site_sel, 1:5)
  )
}


get_sets <- function(attr_dev, attr_pop,
                     site_from, site_to,
                     time_from, time_to){
  list(
    trn = get_idx(attr_dev, site_from, time_from),
    val = get_idx(attr_dev, site_to, time_to),
    pop = get_idx(attr_pop, site_to, time_to)
  )
}

get_sets_rwa <- function(attr_data, 
                         site_sel=LETTERS[1:4], time_sel=1:5, 
                         new_site="E", new_year=8){
  list(
    lrn = NA, # TODO: later extension
    app = NA
    #rw_i1 = get_idx(attr_data, site_sel, last_year+1),
    #rw_i5 = get_idx(attr_data, site_sel, last_year+1),
    #rw_n1 = get_idx(attr_data, new_site, last_year+5),
    #rw_n5 = get_idx(attr_data, new_site, last_year+5)
  ) 
}



get_split_ho <- function(attr_dev, attr_pop, frac_trn=0.8){
  n <- nrow(attr_dev)
  idx_trn <- sample(1:n, round(n * frac_trn))
  list(
    list(trn = idx_trn,
         val = setdiff(1:n, idx_trn),
         pop = 1:nrow(attr_pop))
  )
}

get_split_cv <- function(attr_dev, attr_pop, folds=5){
  n <- nrow(attr_dev)
  idx_rdm <- sample(n, n)
  
  nk <- rep(round(n/folds), folds-1)
  nk[folds] <- n-sum(nk)
  
  idx_val_list <- split(idx_rdm, rep(1:folds, nk))
  
  lapply(idx_val_list, function(idx_val){
    list(trn = setdiff(1:n, idx_val),
         val = idx_val,
         pop = 1:nrow(attr_pop))
  }) 
}

get_metrics <- function(yhat, y){
  
  stopifnot(length(yhat) == length(y))
  
  data.frame(n = length(y),
             n1 = sum(y),
             n0 = sum(1-y),
             prev = mean(y), 
             se = mean(yhat[y==1]==1),
             sp = mean(yhat[y==0]==0),
             acc = mean(yhat==y)) %>% 
    mutate(bacc = 0.5*se + 0.5*sp)
}

gen_output_data <- function(attr_data, feat_data, gen_model){
  
  input_data <- cbind(attr_data, feat_data) %>% 
    mutate(st = as.character(interaction(site, time, sep="|")))
  
  output_data <- lapply(unique(input_data$st), function(i) {
    info <- strsplit(i, "|", fixed=TRUE)[[1]]
    gen_model(feat_data = feat_data[input_data$st == i, ],
              site_sel = info[1], time_sel = as.numeric(info[2]))
  }) %>% 
    do.call(rbind, .)
  
  return(output_data)
  
}

define_gen_model <- function(beta_0, beta, random_effects){
  gen_model <- function(feat_data, time_sel=5, site_sel="D"){
    
    re <- random_effects %>% 
      dplyr::filter(site == site_sel, time == time_sel) %>% 
      dplyr::select(tidyselect::starts_with("beta_")) %>% 
      as.numeric()
    
    beta_vec <- c(beta_0, beta)
    beta_vec[1:length(re)] <- beta_vec[1:length(re)] + re
    
    score <- as.numeric(feat_data %*% beta_vec)
    prob <- sigmoid(score)
    response <- rbinom(length(prob), 1, prob %>% as.numeric())
    pred_gm <- as.numeric(prob > 0.5)
    
    outcome_data <- data.frame(score=score, prob=prob, response=response, pred_gm=pred_gm)
    outcome_data
  }
}

sigmoid <- function(x){
  1/(1+exp(-x))
}

gen_feature_data <- function(n = 10, n_vars= 20){
  X <- mvtnorm::rmvnorm(n, mean = rep(0, n_vars))
  X <- cbind(1, X)
  colnames(X) <- paste0("x_", 0:n_vars)
  return(X)
}

gen_attr_data <- function(n_cell = 1, times = 1:10, sites=LETTERS[1:5]){
  expand.grid(
    obs = 1:n_cell, 
    time = times,
    site = sites
  ) %>% 
    mutate(subject = 1:n())
}

create_cov_ar1 <- function(d = 10, sd=rep(1, d), rho=0.5){
  delta <- abs(matrix(1:d - 1, nrow = d, ncol = d, byrow = TRUE) - 
                 (1:d - 1))
  propagate::cor2cov(rho^delta, sd^2) 
}

create_cov_equi <- function(d = 10, sd=rep(1, d), rho=0.5){
  R <- matrix(rho, d, d)
  diag(R) <- rep(1, d)
  propagate::cor2cov(R, sd^2) 
}

gen_random_effect <- function(n_site=5, sites=LETTERS[1:n_site], sd_s=0.5, rho_s=0.75,
                              n_time=10, times=1:n_time, sd_t=0.1, rho_t=0.5){
  
  gamma_site <- mvtnorm::rmvnorm(1, 
                                 mean=rep(0, n_site), 
                                 sigma=create_cov_equi(d=n_site, sd=rep(sd_s, n_site), rho=rho_s))
  
  gamma_rdm <- lapply(gamma_site, function(x){
    mvtnorm::rmvnorm(1, 
                     mean=rep(x, n_time), 
                     sigma=create_cov_ar1(d=n_time, sd=rep(sd_t, n_time), rho=rho_t))
  }) 
  
  return(Reduce(c, gamma_rdm))
}


gen_random_effects <- function(n_vars_rdm=20, intercept_rdm=TRUE,
                               n_site=5, sites=LETTERS[1:n_site], sd_s=0.5, rho_s=0.75,
                               n_time=10, times=1:n_time, sd_t=0.1, rho_t=0.5){
  
  j <- 0:n_vars_rdm
  B <- sapply(j, function(x) {
    gen_random_effect(n_site=n_site, sites=sites, sd_s=sd_s, rho_s=rho_s,
                      n_time=n_time, times=times, sd_t=sd_t, rho_t=rho_t)
  }
  ) %>% 
    as.data.frame()
  if(!intercept_rdm){B[,1] <- 0}
  names(B) <- paste0("beta_", j)
  cbind(expand.grid(time=times, site=sites), B)
}

create_splits_dev <- function(attr_dev, attr_pop, add_rw = TRUE){
  list(
    esti_1 = get_split_ho(attr_dev, attr_pop, 0.8),
    esti_2 = list(
      get_sets(attr_dev, attr_pop, c("A", "B", "C", "D"), c("A", "B", "C", "D"), 1:4, 5)
    ),
    esti_3 = list(
      get_sets(attr_dev, attr_pop, c("A", "B", "C"), "D", 1:5, 1:5)
    ),
    esti_4 = list(
      get_sets(attr_dev, attr_pop, c("A", "B", "C"), "D", 1:4, 5)
    ),
    esti_5 = get_split_cv(attr_dev, attr_pop, 5),
    esti_6 = list(
      get_sets(attr_dev, attr_pop, c("A", "B", "C", "D"), c("A", "B", "C", "D"), 1:2, 3),
      get_sets(attr_dev, attr_pop, c("A", "B", "C", "D"), c("A", "B", "C", "D"), 2:3, 4),
      get_sets(attr_dev, attr_pop, c("A", "B", "C", "D"), c("A", "B", "C", "D"), 3:4, 5)
    ),
    esti_7 = list(
      get_sets(attr_dev, attr_pop, c("A", "B", "C"), "D", 1:5, 1:5),
      get_sets(attr_dev, attr_pop, c("A", "B", "D"), "C", 1:5, 1:5),
      get_sets(attr_dev, attr_pop, c("A", "C", "D"), "B", 1:5, 1:5),
      get_sets(attr_dev, attr_pop, c("B", "C", "D"), "A", 1:5, 1:5)
    ),
    esti_8 = get_split_e8(attr_dev, attr_pop, 0.8),
    esti_9 = list(
      get_sets(attr_dev, attr_pop, c("A", "B", "C"), "D", 1:2, 3),
      get_sets(attr_dev, attr_pop, c("A", "B", "D"), "C", 1:2, 3),
      get_sets(attr_dev, attr_pop, c("A", "C", "D"), "B", 1:2, 3),
      get_sets(attr_dev, attr_pop, c("B", "C", "D"), "A", 1:2, 3),
      get_sets(attr_dev, attr_pop, c("A", "B", "C"), "D", 1:2+1, 3+1),
      get_sets(attr_dev, attr_pop, c("A", "B", "D"), "C", 1:2+1, 3+1),
      get_sets(attr_dev, attr_pop, c("A", "C", "D"), "B", 1:2+1, 3+1),
      get_sets(attr_dev, attr_pop, c("B", "C", "D"), "A", 1:2+1, 3+1),
      get_sets(attr_dev, attr_pop, c("A", "B", "C"), "D", 1:2+2, 3+2),
      get_sets(attr_dev, attr_pop, c("A", "B", "D"), "C", 1:2+2, 3+2),
      get_sets(attr_dev, attr_pop, c("A", "C", "D"), "B", 1:2+2, 3+2),
      get_sets(attr_dev, attr_pop, c("B", "C", "D"), "A", 1:2+2, 3+2)
    ),
    esti_10 = list(
      get_sets(attr_dev, attr_pop, "D", "D", 1:2, 3),
      get_sets(attr_dev, attr_pop, "C", "C", 1:2, 3),
      get_sets(attr_dev, attr_pop, "B", "B", 1:2, 3),
      get_sets(attr_dev, attr_pop, "A", "A", 1:2, 3),
      get_sets(attr_dev, attr_pop, "D", "D", 1:2+1, 3+1),
      get_sets(attr_dev, attr_pop, "C", "C", 1:2+1, 3+1),
      get_sets(attr_dev, attr_pop, "B", "B", 1:2+1, 3+1),
      get_sets(attr_dev, attr_pop, "A", "A", 1:2+1, 3+1),
      get_sets(attr_dev, attr_pop, "D", "D", 1:2+2, 3+2),
      get_sets(attr_dev, attr_pop, "C", "C", 1:2+2, 3+2),
      get_sets(attr_dev, attr_pop, "B", "B", 1:2+2, 3+2),
      get_sets(attr_dev, attr_pop, "A", "A", 1:2+2, 3+2)
    )
  ) 
}


# experimentation -----------------------------------------------------------------------------
# mls <- ml_study(n_cell = 100, n_cell_pop=1000,
#                 beta_0 = -2,
#                 sd_b = 2, sd_s = 0, sd_t= 0,
#                 rho_s = 0.5, rho_t= 0.75,
#                 equal_trn=TRUE)
# plot(mls$results$P[1, ])
# plot(mls$results$D %>% diag()); abline(h=0)
# mls$info$gm_metrics
# mls$results$D

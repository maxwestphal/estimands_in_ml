get_hyperparameters <- function(method, data = NULL, job = NULL){
  hyperparams <- getModelInfo(method)[[as.character(parse(text = method))]]$grid(len = 1, search = "random")
  return(list("method" = method, "hyperparams" = hyperparams))
}


training_and_prediction <- function(estimand, data = NULL, instance = NULL, job = NULL){
  testing <- readRDS(paste0("//gaia/fme/home/ralpers/public/Estimands/IST/",
                            estimand, "_split_test.RDS"))
  training <- readRDS(paste0("//gaia/fme/home/ralpers/public/Estimands/IST/",
                             estimand, "_split_train.RDS"))
  
  results <- data.frame()
  
  # training for random split
  for (i in 1:length(training)) {
    model <- train(ID14 ~ ., data[training[[i]],-c(1:3)], 
                   method = instance$method, trControl = trainControl(method = "none", classProbs = TRUE),
                   tuneGrid = instance$hyperparams)
    pred <- predict(model, data[testing[[i]],-c(1:3)])
    prediction <- predict(model, data[testing[[i]],-c(1:3)], type = "prob")
    results <- rbind(results, data.frame(estimand = rep(estimand,length(testing[[i]])),
                                         fold = rep(i, length(testing[[i]])),
                                         n_train = rep(length(training[[i]]), length(testing[[i]])),
                                         n_test = rep(length(testing[[i]]), length(testing[[i]])),
                                         observed = data[testing[[i]],"ID14"],
                                         predicted = pred,
                                         pred_prob_0 = prediction[,1], 
                                         pred_prob_1 = prediction[,2]))
  }
  
  results <- mutate(results, 
                    hyperparams = paste(paste(names(instance$hyperparams), collapse = ", "), "=", paste(instance$hyperparams, collapse = ", ")), 
                    .before = estimand)
  return(results)
}

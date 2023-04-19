
# ranger --------------------------------------------------------------------------------------

# model <- ranger::ranger(response ~ ., 
#                         data = cbind(response=outp_dev$response, feat_dev)[idx_trn, ],
#                         classification = TRUE)
# yhat <- predict(model, feat_dev[idx_val, ])$predictions
# table(yhat)
# ?ranger::predictions()
# ?ranger::ranger

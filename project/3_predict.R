# 3_predict


# environment and package loading

library(tidyverse) # dplyr, ggplot
library(doParallel) # registerDoParallel
library(xgboost) # xgboost

cl = makeCluster(detectCores() - 1)
registerDoParallel(cl)


# 최종 선택 모델

xgb_model

# 최종 예측 모델 학습

pre_data_matrix = model.matrix(model_formula, data = pre_data)
pre_data_label = pre_data %>% select(credit) %>% as.matrix() %>%  as.vector()

pre_data_xgb_matrix = xgb.DMatrix(data = pre_data_matrix, label = pre_data_label)

set.seed(20152410)
final_xgb_model = xgboost(data = pre_data_xgb_matrix,
                          objective = 'multi:softprob',
                          eval_metric = 'mlogloss',
                          num_class = xgb_model$params$num_class,
                          nrounds = xgb_model$best_iteration,
                          eta = xgb_model$params$eta)

final_xgb_model


# 예측값 생성

pred_test = predict(final_xgb_model, newdata = X_test) %>% 
    matrix(nrow = final_xgb_model$params$num_class, ncol = dim(X_test)[1]) %>% 
    t() %>% 
    as.data.frame() %>% 
    dplyr::rename('0' = V1, '1' = V2, '2' = V3)

submission = pre_test_data %>% 
    select(index) %>% 
    cbind(pred_test)

submission %>% head()

submission %>% 
    select(-index) %>% 
    apply(1, function(x){which.max(x) - 1}) %>% 
    table()


# 예측 결과 저장
# write_csv(submission, './submission/xgboost_1.csv')


# 혹시 몰라서 오버샘플링한 모델 제출

xgb_model_resampled

pre_data_resampled = resample_data(pre_data)

pre_data_resampled_matrix = model.matrix(model_formula, data = pre_data_resampled)
pre_data_resampled_label = pre_data_resampled %>% select(credit) %>% as.matrix() %>%  as.vector()

pre_data_xgb_matrix = xgb.DMatrix(data = pre_data_resampled_matrix, label = pre_data_resampled_label)

set.seed(20152410)
final_xgb_model_resampled = xgboost(data = pre_data_xgb_matrix,
                                    objective = 'multi:softprob',
                                    eval_metric = 'mlogloss',
                                    num_class = xgb_model_resampled$params$num_class,
                                    nrounds = xgb_model_resampled$best_iteration,
                                    eta = xgb_model_resampled$params$eta)

final_xgb_model_resampled

pred_test_resampled = predict(final_xgb_model_resampled, newdata = X_test) %>% 
    matrix(nrow = final_xgb_model_resampled$params$num_class, ncol = dim(X_test)[1]) %>% 
    t() %>% 
    as.data.frame() %>% 
    dplyr::rename('0' = V1, '1' = V2, '2' = V3)

submission_resampled = pre_test_data %>% 
    select(index) %>% 
    cbind(pred_test_resampled)

submission_resampled %>% head()

submission_resampled %>% 
    select(-index) %>% 
    apply(1, function(x){which.max(x) - 1}) %>% 
    table()

# 예측 결과 저장
# write_csv(submission_resampled, './submission/xgboost_resampled_2.csv')


# save.image(file='environment.RData')


# 2_modeling


# environment and package loading

library(dlookr) # diagnose
library(tidyverse) # dplyr, ggplot
library(doParallel) # registerDoParallel

library(caret)
library(MLmetrics) # MultiLogLoss
library(xgboost) # xgboost
library(catboost) # catboost
# devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')

cl = makeCluster(detectCores() - 1)
registerDoParallel(cl)
# stopCluster(cl)


# xgboost 모델을 이용하여 kfold = 4, number of hyperparameter = 200 조건으로 grid search를 진행했는데 4시간으론 턱없이 부족하길래 포기했습니다.
# 그리드 서치말고 단일 validation만 이용하는 대신 다양한 방법으로 성능을 비교해보고 최종 모델을 선택해보려고 합니다.

# 데이터 준비

# 데이터의 종속변수가 불균형하므로 층화추출법을 이용한 train validation 분할

pre_data = pre_data %>% 
    mutate(edu_type = as.factor(edu_type),
           child_num_category = as.factor(child_num_category))

pre_test_data = pre_test_data %>% 
    mutate(edu_type = as.factor(edu_type),
           child_num_category = as.factor(child_num_category)) 

set.seed(20152410)

train_number = createDataPartition(pre_data[['credit']], p = 0.8)
train_data = pre_data %>% dplyr::slice(train_number$Resample1)
validation_data = pre_data %>% dplyr::slice(-train_number$Resample1)

# 불균형 데이터를 위한 리샘플링 (오버샘플링)

resample_data = function(df){
    
    ### 불균형 데이터에서 빈도수가 적은 종속변수의 샘플을 2배로 오버샘플링
    resampled_0 = df %>%
        filter(credit == 0) %>%
        rbind(df %>% filter(credit == 0))
    
    resampled_1 = df %>%
        filter(credit == 1) %>%
        rbind(df %>% filter(credit == 1))
    
    resampled_df = df %>%
        filter(credit == 2) %>%
        rbind(resampled_0) %>%
        rbind(resampled_1) %>%
        arrange(index)
    
    return(resampled_df)
}

resampled_data = resample_data(train_data)


# 학습데이터 준비
# 행렬 형태로 변환, 변수로 factor 변수의 2차 상호작용항 추가

model_formula = formula(credit ~ -1 + income_total + family_size + begin_month + age + career_year + parent_num +
                            (gender + car + reality + income_type + family_type + house_type + work_phone + phone + email + child_num_category)^2)

X_train = model.matrix(model_formula, data = train_data)
X_train_resampled = model.matrix(model_formula, data = resampled_data)
X_val = model.matrix(model_formula, data = validation_data)
X_test = model.matrix(model_formula, data = pre_test_data)

y_train = train_data %>% select(credit) %>% as.matrix() %>%  as.vector()
y_train_resampled = resampled_data %>% select(credit) %>% as.matrix() %>%  as.vector() 
y_val = validation_data %>% select(credit) %>% as.matrix() %>%  as.vector() 


# 모델 학습
# 일반데이터, 리샘플데이터를 xgboost, catboost 2개의 모델에 적용해보도록 하겠음

# xgboost

xgb_train_matrix = xgb.DMatrix(data = X_train, label = y_train)
xgb_train_resampled_matrix = xgb.DMatrix(data = X_train_resampled, label = y_train_resampled)
xgb_val_matrix = xgb.DMatrix(data = X_val, label = y_val)

set.seed(20152410)
xgb_model = xgb.train(data = xgb_train_matrix,
                      objective = 'multi:softprob',
                      eval_metric = 'mlogloss',
                      num_class = 3,
                      nrounds = 1000,
                      eta = 0.3,
                      watchlist = list(val = xgb_val_matrix),
                      early_stopping_rounds = 50)

set.seed(20152410)
xgb_model_resampled = xgb.train(data = xgb_train_resampled_matrix,
                                objective = 'multi:softprob',
                                eval_metric = 'mlogloss',
                                num_class = 3,
                                nrounds = 1000,
                                eta = 0.3,
                                watchlist = list(val = xgb_val_matrix),
                                early_stopping_rounds = 50)

xgb_predict = function(model, newdata){
    ### xgb 모델의 예측 확률, 예측 클래스 생성
    probs = predict(model, newdata = newdata) %>% 
        matrix(nrow = model$call$num_class, ncol = dim(newdata)[1]) %>% 
        t()
    
    classes = probs %>% 
        apply(1, function(x){which.max(x) - 1}) %>% 
        as.factor()
    
    return(list(prob = probs,
                class = classes))
}

prediction_xgb_model = xgb_predict(xgb_model, xgb_val_matrix)
prediction_xgb_model_resampled = xgb_predict(xgb_model_resampled, xgb_val_matrix)


# catboost
# xgboost에서 오버샘플링 데이터의 validation 성능이 더 좋지 않기때문에
# catboost에선 오버샘플링하지 않은 데이터만 이용하도록 하겠음

cat_train_matrix = catboost.load_pool(data = X_train,
                                      label = as.integer(y_train),
                                      cat_features = seq(6, 274))

cat_val_matrix = catboost.load_pool(data = X_val,
                                    label = as.integer(y_val),
                                    cat_features = seq(6, 274))

cat_test_matrix = catboost.load_pool(data = X_test,
                                     cat_features = seq(6, 274))

cat_model = catboost.train(learn_pool = cat_train_matrix,
                           test_pool = cat_val_matrix,
                           params = list(
                               loss_function = 'MultiClass',
                               eval_metric = 'MultiClass',
                               classes_count = 3,
                               iterations = 500,
                               learning_rate = 0.5,
                               random_seed = 20152410,
                               metric_period = 10))

cat_model2 = catboost.train(learn_pool = cat_train_matrix,
                            test_pool = cat_val_matrix,
                            params = list(
                                loss_function = 'MultiClassOneVsAll',
                                eval_metric = 'MultiClassOneVsAll',
                                classes_count = 3,
                                iterations = 500,
                                learning_rate = 0.5,
                                random_seed = 20152410,
                                metric_period = 10))

cat_predict = function(model, newdata){
    ### catboost 모델의 예측 확률, 예측 클래스 생성
    probs = catboost.predict(cat_model,
                             pool = cat_val_matrix,
                             prediction_type = 'Probability')
    
    classes = catboost.predict(cat_model,
                               pool = cat_val_matrix,
                               prediction_type = 'Class') %>% 
        as.factor()
    
    return(list(prob = probs,
                class = classes))
}

prediction_cat_model = cat_predict(cat_model, cat_val_matrix)
prediction_cat_model2 = cat_predict(cat_model, cat_val_matrix)


# evaluate model performance

eval_df = data.frame(model = c("xgboost",
                               "xgboost_resampled",
                               'catboost_MultiClass_loss',
                               'catboost_MultiClassOneVsAll_loss'),
                     accuracy = c(Accuracy(prediction_xgb_model$class, as.factor(y_val)),
                                  Accuracy(prediction_xgb_model_resampled$class, as.factor(y_val)),
                                  Accuracy(prediction_cat_model$class, as.factor(y_val)),
                                  Accuracy(prediction_cat_model2$class, as.factor(y_val))),
                     logloss = c(MultiLogLoss(prediction_xgb_model$prob, as.factor(y_val)),
                                 MultiLogLoss(prediction_xgb_model_resampled$prob, as.factor(y_val)),
                                 MultiLogLoss(prediction_cat_model$prob, as.factor(y_val)),
                                 MultiLogLoss(prediction_cat_model2$prob, as.factor(y_val))))

eval_df


# save.image(file='environment.RData')
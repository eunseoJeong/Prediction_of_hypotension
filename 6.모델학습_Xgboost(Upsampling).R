####---------------------------------------------------------4. 모델 학습---------------------------------------------------------####


final_list <- read.csv("H:/Dropbox/연구실세미나/이건/저혈압예측(0927)/bucheonvr_NotNAList.csv")
# final_data <- read.csv("H:/Dropbox/연구실세미나/이건/저혈압예측(0927)/bucheonvr_final.csv") # 9848
final_data <- read.csv("H:/Dropbox/연구실세미나/이건/저혈압예측(0927)/bucheonvr_final2.csv") # 10116

final_data %>% is.na() %>% sum() # 286(1) 306(2)

colSums(is.na(final_data))[which(colSums(is.na(final_data))!=0)] # IBP_p2_std, PLETH_skewness, PLETH_kurtosis, PLETH_p2_std, PLETH_cf

set.seed(3)
Tindex <- createDataPartition(final_data$class, p = 0.7, list = F)

train_data <- final_data[Tindex,] # 6894(1) 7082(2)
test_data <- final_data[-Tindex,] # 2954 3034(2)

train_data <- as.data.frame(lapply(train_data,na_replace))
test_data <- as.data.frame(lapply(test_data,na_replace))

x_train <- train_data %>% dplyr::select(-class)
x_test <- test_data %>%  dplyr::select(-class)

y_train <- as.factor(train_data$class)
y_test <- as.factor(test_data$class)

# 5. Upsampling
train_data <- ovun.sample(class~., data = train_data, method = 'over', N=10000)$data

x_train <- train_data %>% select(-class)
x_test <- test_data %>% select(-class)

y_train <- as.factor(train_data$class)
y_test <- as.factor(test_data$class)

set.seed(1)

datable <- train_data$class %>% table()
(scale_pos_weight <- datable[1]/datable[2] %>% as.numeric())


xgboost_model <- xgboost(
  data = as.matrix(x_train),
  label = as.numeric(y_train)-1,
  eta = 0.05,
  max_depth = 6,
  nrounds = 200,
  n_estimator = 100,
  booster = "gbtree",
  objective = "binary:logistic",
  early_stopping_rounds = 100,
  set.seed = 1
)

xgb_pred <- xgboost_model %>% predict(as.matrix(x_test))
head(xgb_pred)
b<-xgb_pred>=0.4
confusionMatrix (b %>% as.integer() %>% as.factor(), y_test)

b <- ifelse(b,1,0)

Accuracy(y_test,b)
Precision(y_test,b,positive="1")
Recall(y_test,b,positive="1")
F1_Score(y_test,b,positive = "1") # 최대 0.35까지(NA처리를 살짝 수정)(하지만 크게 변하진 않음)

xgb.importance(model=xgboost_model)

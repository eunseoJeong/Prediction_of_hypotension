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

# 1. xgboost
set.seed(1)

xgboost_model <- xgboost(
  data = as.matrix(x_train),
  label = as.numeric(y_train)-1,
  eta = 0.01,
  depth = 100,
  nround = 1000,
  n_estimator = 200,
  booster = "gbtree",
  objective = "binary:logistic",
  early_stopping_rounds = 100,
  scale_pos_weight = 3,
  set.seed = 1
)


xgb_pred <- xgboost_model %>% predict(as.matrix(x_test))
head(xgb_pred)
b<-xgb_pred>=0.5
crst_xb <- CrossTable(y_test, b)

b <- ifelse(b,1,0)

Accuracy(y_test,b)
Precision(y_test,b,positive="1")
Recall(y_test,b,positive="1")
F1_Score(y_test,b,positive = "1")
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

# 2. random forest
table(as.character(train_data$class),useNA="always")

train_data$class<-as.factor(train_data$class)
m <- randomForest(class~., data = train_data, na.rm=T)

xgb_pred <- m %>% predict(x_test,class=TRUE)
crst_xb <- CrossTable(y_test, xgb_pred)

Accuracy(y_test,xgb_pred)
Precision(y_test,xgb_pred,positive="1")
Recall(y_test,xgb_pred,positive="1")
F1_Score(y_test,xgb_pred,positive = "1")
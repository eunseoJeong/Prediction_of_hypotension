# 패키지 불러오기
library(dplyr)
library(car) # vif 확인
library(e1071) # svm모델 생성
library(Metrics) # rmse계산


# --- < 4. SVR "rbf" > ---
# - SBP -
# 데이터셋 나누기
# 8:2 round(nrow(data) * 0.8) = 39089
colnames(data)
data <- data %>% select(-c(ENT_RD_EMG_20,ENT_RD_EMG_21,ENT_RD_EMG_22,ENT_RD_EMG_23,ENT_RD_EMG_24))
data <- data %>% select(-c(f))
# data <- data %>% select(c("SBP","SBP_20","SBP_21","MBP_20","SBP_22","MBP_21","SBP_23","MBP_22","SBP_24","MBP_23","MBP_24",
#                           "REMIFENTANIL_CT_20","ETCO2_20","REMIFENTANIL_CT_21","ETCO2_21","REMIFENTANIL_CT_22","ETCO2_22","ETCO2_23","REMIFENTANIL_CT_23","ETCO2_24","REMIFENTANIL_CT_24",
#                           "REMIFENTANIL_CE_20","REMIFENTANIL_CE_21","HR_24","HR_23","HR_22","HR_21","HR_20","REMIFENTANIL_CE_22","REMIFENTANIL_CE_23","REMIFENTANIL_CE_24",
#                           "REMIFENTANIL_CP_20","REMIFENTANIL_CP_21","REMIFENTANIL_CP_22","REMIFENTANIL_CP_23","REMIFENTANIL_CP_24"))
#data[39385:39386,]$f # 38785:39385
# index_train <- c(1:21170, 21772:39986)
# index_test <- c(21171:21771, 39987:nrow(data))

index_train <- c(1:39385)
index_test <- c(39386:nrow(data))

train <- data[index_train,]
test <- data[index_test,]

## 모델생성
m <- svm(SBP ~ ., data=train, kernel="radial")
summary(m)
# 변수별 coefficients
coef(m)
coef_sbp <- coef(m) %>% data.frame() %>% arrange(desc(abs(.)))
write.csv(coef_sbp, "D://은서//은서대학교생활//2021 은서 연구생//vital//coef_sbp.csv")
## 모델 평가
str(train)
str(test)
pred <- predict(m, test)
test <- na.omit(test)
# plot그리기
plot(x=test$SBP, y=pred, 
     xlim=c(55,205), ylim=c(55,205), 
     main="SBP", xlab="실제값", ylab="예측값")
# h = 가로선, v = 세로선
abline(h = 90, v = 90, col = "red")
max(test$SBP)
max(pred)
min(test$SBP)
min(pred)
# RMSE
RMSE <- rmse(test$SBP, pred)
# MAPE
MAPE <- Metrics::mape(test$SBP, pred)

### 저혈압 예측일때 (SBP 65이하일 때)
result_sbp <- cbind(test$SBP, pred) %>% data.frame()
result_sbp <- result_sbp %>% filter(V1 <= 90) 
# MAPE
MAPE <- Metrics::mape(result_sbp$V1, result_sbp$pred)



# - MBP -
data <- data %>% select(-c(f,ENT_RD_EMG_20,ENT_RD_EMG_21,ENT_RD_EMG_22,ENT_RD_EMG_23))
colnames(data)
# data <- data %>% select(c("MBP","MBP_20","MBP_21","MBP_22","MBP_23","MBP_24","SBP_20","SBP_21","SBP_22","SBP_23","SBP_24",
#                           "HR_22","HR_23","HR_21","HR_24","HR_20",
#                           "ETCO2_20","REMIFENTANIL_CT_20","ETCO2_21","REMIFENTANIL_CT_21","ETCO2_22","REMIFENTANIL_CT_22","ETCO2_23","REMIFENTANIL_CT_23","ETCO2_24","REMIFENTANIL_CT_24",
#                           "REMIFENTANIL_CE_20","REMIFENTANIL_CE_21","PIP_20","PIP_21","PIP_22"))

# 데이터셋 나누기
# 8:2 round(nrow(data) * 0.8) = 39089
index_train <- 1:39385
index_test <- 39386:nrow(data)
train <- data[index_train,]
test <- data[index_test,]

## 모델생성
m <- svm(MBP ~ ., data=train, kernel="radial")
summary(m)
# 변수별 coefficients
coef_mbp <- coef(m) %>% data.frame() %>% arrange(desc(abs(.)))
write.csv(coef_mbp, "D://은서//은서대학교생활//2021 은서 연구생//vital//coef_mbp.csv")
## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)
# plot그리기
plot(x=test$MBP, y=pred, 
     xlim=c(55,170), ylim=c(55,170), 
     main="MBP", xlab="실제값", ylab="예측값")
# h = 가로선, v = 세로선
abline(h = 65, v = 65, col = "red")
max(test$MBP)
max(pred)
min(test$MBP)
min(pred)
# RMSE 
RMSE <- rmse(test$MBP, pred)
# MAPE 
MAPE <- Metrics::mape(test$MBP, pred)

### 저혈압 예측일때 (SBP 65이하일 때)
result_mbp <- cbind(test$MBP, pred) %>% data.frame()
result_mbp <- result_mbp %>% filter(V1 <= 65) 
# MAPE 
MAPE <- Metrics::mape(result_mbp$V1, result_mbp$pred)


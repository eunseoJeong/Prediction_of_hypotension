# 패키지
#install.packages("corrplot")
library(corrplot)
library(dplyr)

## 주요변수 데이터셋 만들기
#디렉토리 설정하기
setwd("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\Data")
d <- getwd()
fls <- dir(d, recursive=TRUE)

SBP <- data.frame()
MBP <- data.frame()
HR <- data.frame()
ETCO2 <- data.frame()
SEF <- data.frame()
REMIFENTANIL_CT <- data.frame()
REMIFENTANIL_CE <- data.frame()
REMIFENTANIL_CP <- data.frame()
PROPOFOL_CE <- data.frame()
dataset <- data.frame()

for(i in fls){
  # 파일 불러오기
  temp <- read.csv(paste0(d,"\\",i))
  SBP <- temp$Bx50.NIBP_SBP
  MBP <- temp$Bx50.NIBP_MBP
  HR <- temp$Bx50.HR
  ETCO2 <- temp$Bx50.ETCO2
  SEF <- temp$BIS.SEF
  REMIFENTANIL_CT <- temp$Orchestra.REMIFENTANIL_CT
  REMIFENTANIL_CE <- temp$Orchestra.REMIFENTANIL_CE
  REMIFENTANIL_CP <- temp$Orchestra.REMIFENTANIL_CP
  PROPOFOL_CE <- temp$Orchestra.PROPOFOL_CE
  onedata <- bind_cols(SBP,MBP,HR,ETCO2,SEF,REMIFENTANIL_CT,REMIFENTANIL_CE,REMIFENTANIL_CP,PROPOFOL_CE)
  dataset <- bind_rows(dataset,onedata)}
colnames(dataset) <- c("SBP","MBP","HR","ETCO2","SEF","REMIFENTANIL_CT","REMIFENTANIL_CE","REMIFENTANIL_CP","PROPOFOL_CE")

# 상관관계 분석
cor <- cor(dataset, use="pairwise.complete.obs")

# 시각화
corrplot(cor, method='shade', shade.col=NA, tl.col='black', tl.srt=45)
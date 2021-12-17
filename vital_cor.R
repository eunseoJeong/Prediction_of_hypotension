rm(list=ls())
gc()
memory.size()
memory.limit(1000000)

library(stringr)
library(dplyr)
#------------------------ cor ------------------------------#
setwd("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL")
# d <- getwd()
# flist <- dir(d, recursive=TRUE)
# 
# # 상관분석 계산
# for (file in flist){
#   a <- file.path(str_c(d,"/", file))
#   temp <- read.csv(a)
#   cor <- cor(temp[,-1], use="pairwise.complete.obs")
#   write.csv(cor, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_COR\\","cor_",file,".csv"))
# }

c <- ("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_COR")
corlist <- dir(c, recursive=TRUE)
SBPdf <- data.frame()
MBPdf <- data.frame()
# # MBP, SBP의 상관관계 값만 불러오기
# for (i in corlist){
#   a <- file.path(str_c(c,"/", i))
#   temp <- read.csv(a)
#   temp <- temp[,-1]
#   rownames(temp) <- colnames(temp)
#   SBP <- data.frame(temp[,6])
#   MBP <- data.frame(temp[,7])
#   rownames(SBP) <- rownames(temp)
#   rownames(MBP) <- rownames(temp)
#   SBPdf <- rbind(SBPdf, SBP)
#   MBPdf <- rbind(MBPdf, MBP)
# #  print(a)
# }
# SBPdf[order(abs(SBPdf$temp...6.),decreasing=TRUE),]

# SBPdf <- data.frame()
# MBPdf <- data.frame()
# MBP, SBP의 상관관계 값만 불러오기
for (i in corlist){
  a <- file.path(str_c(c,"/", i))
  temp <- read.csv(a)
  temp <- temp[,-1]
  rownames(temp) <- names(temp)
  SBP <- data.frame(temp$Bx50.NIBP_SBP)
  MBP <- data.frame(temp$Bx50.NIBP_MBP)
  SBP$group <- colnames(temp)
  MBP$group <- colnames(temp)
  SBPdf <- rbind(SBPdf, SBP)
  MBPdf <- rbind(MBPdf, MBP)
  #  print(a)
}
names(SBPdf)[1] <- "SBP"
names(MBPdf)[1] <- "MBP"

# 장비다른 room12데이터 따로 추출
room12 <- read.csv("D:/은서/은서대학교생활/2021 은서 연구생/cor_Room 12_180513_171443.csv_tag_filled.csv.csv")
room12 <- room12[,-1]
head(room12)
# room12의 SBP
solar.SBP <- data.frame(room12$Solar.8000M.NIBP_SBP)
solar.group <- data.frame(colnames(room12)) 
room_12 <- cbind(solar.SBP, solar.group)
colnames(room_12) <- c('SBP', 'group')
# room12의 MBP
solar.MBP <- data.frame(room12$Solar.8000M.NIBP_MBP)
solar.group <- data.frame(colnames(room12)) 
room_12m <- cbind(solar.MBP, solar.group)
colnames(room_12m) <- c('MBP', 'group')
table(finalSBP$group)
# 하나로 합치기
finalSBP <- rbind(SBPdf, room_12)
write.csv(finalSBP, "D:/은서/은서대학교생활/2021 은서 연구생/vital_cor_SBP_total.csv")
finalMBP <- rbind(MBPdf, room_12m)
write.csv(finalMBP, "D:/은서/은서대학교생활/2021 은서 연구생/vital_cor_MBP_total.csv")
unique(finalSBP$group)
library(dplyr)
# SBP _ cor 절대값이 0.7이상인 것들 추출
finalSBP <- na.omit(finalSBP)
SBP_0.7 <- filter(finalSBP, abs(SBP) > 0.7 & abs(SBP) <1)
SBP_order<- SBP_0.7[order(abs(SBP_0.7$SBP),decreasing=TRUE),]
SBP_order <- filter(SBP_order, group!="Bx50.NIBP_MBP")
write.csv(SBP_order, "D:/은서/은서대학교생활/2021 은서 연구생/vital_cor_SBP_0.7.csv")
S7 <- table(SBP_0.7$group) %>% data.frame()
S <- table(finalSBP$group) %>% data.frame()
join_cor_SBP <- right_join(S, S7, by="Var1")
join_cor_SBP$percent <- join_cor_SBP$Freq.y/join_cor_SBP$Freq.x
colnames(join_cor_SBP) <- c("Feature","전체 빈도","상위70% 빈도","상위70%빈도/전체빈도")

# MBP _ cor 절대값이 0.7이상인 것들 추출
MBP_0.7 <- filter(finalMBP, abs(MBP) > 0.7 & abs(MBP) <1)
MBP_order <- MBP_0.7[order(abs(MBP_0.7$MBP),decreasing=TRUE),]
MBP_order <- filter(MBP_order, group!="Bx50.NIBP_SBP")
write.csv(MBP_order, "D:/은서/은서대학교생활/2021 은서 연구생/vital_cor_MBP_0.7.csv")
M7 <- table(MBP_0.7$group) %>% data.frame()
M <- table(finalMBP$group) %>% data.frame()
join_cor_MBP <- right_join(M, M7, by="Var1")
join_cor_MBP$percent <- join_cor_MBP$Freq.y/join_cor_MBP$Freq.x
colnames(join_cor_MBP) <- c("Feature","전체 빈도","상위70% 빈도","상위70%빈도/전체빈도")

#--------------------- lag correlation ---------------------------------#
setwd("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL")
d <- getwd()
fls <- dir(d, recursive=TRUE)

for(f in fls){
  a <- file.path(str_c(d,"/",f))
  temp <- read.csv(a)
  assign(f,temp)
}


# c("Error in ts(x) : 'ts' 객체는 반드시 하나 또는 그 이상의 관찰값들을 가지고 있습니다."
# ,"Error in ts(x) : 'ts' object must have one or more observations"
# ,"Error in na.fail.default(as.ts(x)) : missing values in object"
# ,"Error in plot.window(...) : 유한한 값들만이 'ylim'에 사용될 수 있습니다
#           In addition: Warning messages:
#           1: In min(x) : no non-missing arguments to min; returning Inf
#           2: In max(x) : no non-missing arguments to max; returning -Inf")
# 
# custom_error = function(subclass, message, call = sys.call(-1), ...) {
#   structure(
#     class = c(subclass, 'condition'),
#     list(message = message, call = call, ...)
#   )
# }
# 
# error = custom_error(c("Error in ts(x) : 'ts' 객체는 반드시 하나 또는 그 이상의 관찰값들을 가지고 있습니다."
#                         ,"Error in ts(x) : 'ts' object must have one or more observations"
#                         ,"Error in na.fail.default(as.ts(x)) : missing values in object"
#                         ,"Error in plot.window(...) : 유한한 값들만이 'ylim'에 사용될 수 있습니다
#           In addition: Warning messages:
#           1: In min(x) : no non-missing arguments to min; returning Inf
#           2: In max(x) : no non-missing arguments to max; returning -Inf")
#                       , 'An Error occured while test')

# tryCatch(
#   {for(i in 1:length(fls)){
  #데이터 객체 가져오기
  temp <- get(fls[90])
    
  # 돌릴 때마다 데이터 초기화
  Bx50.HR <- data.frame()
  Bx50.PLETH_SPO2 <- data.frame()
  Bx50.ETCO2 <- data.frame()
  Bx50.CO2 <- data.frame()
  Bx50.AMB_PRES <- data.frame()
  Bx50.PLETH <- data.frame()
  Bx50.RR_VENT <- data.frame()
  Datex.Ohmeda.TV_EXP <- data.frame()
  Datex.Ohmeda.MV_EXP <- data.frame()
  Datex.Ohmeda.PIP <- data.frame()
  Datex.Ohmeda.COMPLIANCE <- data.frame()
  BIS.SEF <- data.frame()
  BIS.SQI <- data.frame()
  BIS.EMG <- data.frame()
  BIS.TOTPOW <- data.frame()
  Bx50.ENT_RD_EEG <- data.frame()
  Bx50.ENT_RD_EMG <- data.frame()
  Bx50.ENT_RD_BSR <- data.frame()
  Orchestra.PROPOFOL_CP <- data.frame()
  Orchestra.PROPOFOL_CE <- data.frame()
  Orchestra.PROPOFOL_CT <- data.frame()
  Orchestra.REMIFENTANIL_CP <- data.frame()
  Orchestra.REMIFENTANIL_CE <- data.frame()
  Orchestra.REMIFENTANIL_CT <- data.frame()
  Hypotension <- data.frame()
  
  Bx50.HR_ <- data.frame()
  Bx50.PLETH_SPO2_ <- data.frame()
  Bx50.ETCO2_ <- data.frame()
  Bx50.CO2_ <- data.frame()
  Bx50.AMB_PRES_ <- data.frame()
  Bx50.PLETH_ <- data.frame()
  Bx50.RR_VENT_ <- data.frame()
  Datex.Ohmeda.TV_EXP_ <- data.frame()
  Datex.Ohmeda.MV_EXP_ <- data.frame()
  Datex.Ohmeda.PIP_ <- data.frame()
  Datex.Ohmeda.COMPLIANCE_ <- data.frame()
  BIS.SEF_ <- data.frame()
  BIS.SQI_ <- data.frame()
  BIS.EMG_ <- data.frame()
  BIS.TOTPOW_ <- data.frame()
  Bx50.ENT_RD_EEG_ <- data.frame()
  Bx50.ENT_RD_EMG_ <- data.frame()
  Bx50.ENT_RD_BSR_ <- data.frame()
  Orchestra.PROPOFOL_CP_ <- data.frame()
  Orchestra.PROPOFOL_CE_ <- data.frame()
  Orchestra.PROPOFOL_CT_ <- data.frame()
  Orchestra.REMIFENTANIL_CP_ <- data.frame()
  Orchestra.REMIFENTANIL_CE_ <- data.frame()
  Orchestra.REMIFENTANIL_CT_ <- data.frame()
  Hypotension_ <- data.frame()
  
  HR <- data.frame()
  PLETH_SPO2 <- data.frame()
  ETCO2 <- data.frame()
  CO2 <- data.frame()
  AMB_PRES <- data.frame()
  PLETH <- data.frame()
  RR_VENT <- data.frame()
  TV_EXP <- data.frame()
  MV_EXP <- data.frame()
  PIP <- data.frame()
  COMPLIANCE <- data.frame()
  SEF <- data.frame()
  SQI <- data.frame()
  EMG <- data.frame()
  TOTPOW <- data.frame()
  ENT_RD_EEG <- data.frame()
  ENT_RD_EMG <- data.frame()
  ENT_RD_BSR <- data.frame()
  PROPOFOL_CP <- data.frame()
  PROPOFOL_CE <- data.frame()
  PROPOFOL_CT <- data.frame()
  REMIFENTANIL_CP <- data.frame()
  REMIFENTANIL_CE <- data.frame()
  REMIFENTANIL_CT <- data.frame()
  Hypo <- data.frame()
  
  HR_ <- data.frame()
  PLETH_SPO2_ <- data.frame()
  ETCO2_ <- data.frame()
  CO2_ <- data.frame()
  AMB_PRES_ <- data.frame()
  PLETH_ <- data.frame()
  RR_VENT_ <- data.frame()
  Ohmeda.TV_EXP_ <- data.frame()
  Ohmeda.MV_EXP_ <- data.frame()
  Ohmeda.PIP_ <- data.frame()
  Ohmeda.COMPLIANCE_ <- data.frame()
  SEF_ <- data.frame()
  SQI_ <- data.frame()
  EMG_ <- data.frame()
  TOTPOW_ <- data.frame()
  ENT_RD_EEG_ <- data.frame()
  ENT_RD_EMG_ <- data.frame()
  ENT_RD_BSR_ <- data.frame()
  PROPOFOL_CP_ <- data.frame()
  PROPOFOL_CE_ <- data.frame()
  PROPOFOL_CT_ <- data.frame()
  REMIFENTANIL_CP_ <- data.frame()
  REMIFENTANIL_CE_ <- data.frame()
  REMIFENTANIL_CT_ <- data.frame()
  Hypo_ <- data.frame()
  

  
  ## 변수명 그대로 = SBP의 lag-correlation / 변수명_ = MBP의 lag-correlation
  # 데이터의 lag-correlation값 h=음수인 부분만 추출(x에 y가 영향받음)
  HR <- ccf(temp$Bx50.HR,temp$Bx50.NIBP_SBP)
  Bx50.HR <- HR[[1]][1:24] %>% data.frame()
  colnames(Bx50.HR) <- "Bx50.HR"
  HR_ <- ccf(temp$Bx50.HR,temp$Bx50.NIBP_MBP)
  Bx50.HR_ <- HR_[[1]][1:24] %>% data.frame()
  colnames(Bx50.HR_) <- "Bx50.HR"
  
  PLETH_SPO2 <- ccf(temp$Bx50.PLETH_SPO2,temp$Bx50.NIBP_SBP)
  Bx50.PLETH_SPO2 <- PLETH_SPO2[[1]][1:24] %>% data.frame()
  colnames(Bx50.PLETH_SPO2) <- "Bx50.PLETH_SPO2"
  PLETH_SPO2_ <- ccf(temp$Bx50.PLETH_SPO2,temp$Bx50.NIBP_MBP)
  Bx50.PLETH_SPO2_ <- PLETH_SPO2_[[1]][1:24] %>% data.frame()
  colnames(Bx50.PLETH_SPO2_) <- "Bx50.PLETH_SPO2"

  ETCO2 <- ccf(temp$Bx50.ETCO2,temp$Bx50.NIBP_SBP)
  Bx50.ETCO2 <- ETCO2[[1]][1:24]%>% data.frame()
  colnames(Bx50.ETCO2) <- "Bx50.ETCO2"
  ETCO2_ <- ccf(temp$Bx50.ETCO2,temp$Bx50.NIBP_MBP)
  Bx50.ETCO2_ <- ETCO2_[[1]][1:24]%>% data.frame()
  colnames(Bx50.ETCO2_) <- "Bx50.ETCO2"
  
  CO2 <- ccf(temp$Bx50.CO2,temp$Bx50.NIBP_SBP)
  Bx50.CO2 <- CO2[[1]][1:24]%>% data.frame()
  colnames(Bx50.CO2) <- "Bx50.CO2"
  CO2_ <- ccf(temp$Bx50.CO2,temp$Bx50.NIBP_MBP)
  Bx50.CO2_ <- CO2_[[1]][1:24]%>% data.frame()
  colnames(Bx50.CO2_) <- "Bx50.CO2"
  
  AMB_PRES <- ccf(temp$Bx50.AMB_PRES,temp$Bx50.NIBP_SBP)
  Bx50.AMB_PRES <- AMB_PRES[[1]][1:24]%>% data.frame()
  colnames(Bx50.AMB_PRES) <- "Bx50.AMB_PRES"
  AMB_PRES_ <- ccf(temp$Bx50.AMB_PRES,temp$Bx50.NIBP_MBP)
  Bx50.AMB_PRES_ <- AMB_PRES_[[1]][1:24]%>% data.frame()
  colnames(Bx50.AMB_PRES_) <- "Bx50.AMB_PRES"
  
  PLETH <- ccf(temp$Bx50.PLETH,temp$Bx50.NIBP_SBP)
  Bx50.PLETH <- PLETH[[1]][1:24]%>% data.frame()
  colnames(Bx50.PLETH) <- "Bx50.PLETH"
  PLETH_ <- ccf(temp$Bx50.PLETH,temp$Bx50.NIBP_MBP)
  Bx50.PLETH_ <- PLETH_[[1]][1:24]%>% data.frame()
  colnames(Bx50.PLETH_) <- "Bx50.PLETH"
  
  RR_VENT <- ccf(temp$Bx50.RR_VENT,temp$Bx50.NIBP_SBP)
  Bx50.RR_VENT <- RR_VENT[[1]][1:24]%>% data.frame()
  colnames(Bx50.RR_VENT) <- "Bx50.RR_VENT"
  RR_VENT_ <- ccf(temp$Bx50.RR_VENT,temp$Bx50.NIBP_MBP)
  Bx50.RR_VENT_ <- RR_VENT_[[1]][1:24]%>% data.frame()
  colnames(Bx50.RR_VENT_) <- "Bx50.RR_VENT"
  
  TV_EXP <- ccf(temp$Datex.Ohmeda.TV_EXP,temp$Bx50.NIBP_SBP)
  Datex.Ohmeda.TV_EXP <- TV_EXP[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.TV_EXP) <- "Datex.Ohmeda.TV_EXP"
  TV_EXP_ <- ccf(temp$Datex.Ohmeda.TV_EXP,temp$Bx50.NIBP_MBP)
  Datex.Ohmeda.TV_EXP_ <- TV_EXP_[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.TV_EXP_) <- "Datex.Ohmeda.TV_EXP"
  
  MV_EXP <- ccf(temp$Datex.Ohmeda.MV_EXP,temp$Bx50.NIBP_SBP)
  Datex.Ohmeda.MV_EXP <- MV_EXP[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.MV_EXP) <- "Datex.Ohmeda.MV_EXP"
  MV_EXP_ <- ccf(temp$Datex.Ohmeda.MV_EXP,temp$Bx50.NIBP_MBP)
  Datex.Ohmeda.MV_EXP_ <- MV_EXP_[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.MV_EXP_) <- "Datex.Ohmeda.MV_EXP"

  PIP <- ccf(temp$Datex.Ohmeda.PIP,temp$Bx50.NIBP_SBP)
  Datex.Ohmeda.PIP <- PIP[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.PIP) <- "Datex.Ohmeda.PIP"
  PIP_ <- ccf(temp$Datex.Ohmeda.PIP,temp$Bx50.NIBP_MBP)
  Datex.Ohmeda.PIP_ <- PIP_[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.PIP_) <- "Datex.Ohmeda.PIP"
  
  COMPLIANCE <- ccf(temp$Datex.Ohmeda.COMPLIANCE,temp$Bx50.NIBP_SBP)
  Datex.Ohmeda.COMPLIANCE <- COMPLIANCE[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.COMPLIANCE) <- "Datex.Ohmeda.COMPLIANCE"
  COMPLIANCE_ <- ccf(temp$Datex.Ohmeda.COMPLIANCE,temp$Bx50.NIBP_MBP)
  Datex.Ohmeda.COMPLIANCE_ <- COMPLIANCE_[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.COMPLIANCE_) <- "Datex.Ohmeda.COMPLIANCE"

  SEF <- ccf(temp$BIS.SEF,temp$Bx50.NIBP_SBP)
  BIS.SEF <- SEF[[1]][1:24]%>% data.frame()
  colnames(BIS.SEF) <- "BIS.SEF"
  SEF_ <- ccf(temp$BIS.SEF,temp$Bx50.NIBP_MBP)
  BIS.SEF_ <- SEF_[[1]][1:24]%>% data.frame()
  colnames(BIS.SEF_) <- "BIS.SEF"
  
  SQI <- ccf(temp$BIS.SQI,temp$Bx50.NIBP_SBP)
  BIS.SQI <- SQI[[1]][1:24]%>% data.frame()
  colnames(BIS.SQI) <- "BIS.SQI"
  SQI_ <- ccf(temp$BIS.SQI,temp$Bx50.NIBP_MBP)
  BIS.SQI_ <- SQI_[[1]][1:24]%>% data.frame()
  colnames(BIS.SQI_) <- "BIS.SQI"
  
  EMG <- ccf(temp$BIS.EMG,temp$Bx50.NIBP_SBP)
  BIS.EMG <- EMG[[1]][1:24]%>% data.frame()
  colnames(BIS.EMG) <- "BIS.EMG"
  EMG_ <- ccf(temp$BIS.EMG,temp$Bx50.NIBP_MBP)
  BIS.EMG_ <- EMG_[[1]][1:24]%>% data.frame()
  colnames(BIS.EMG_) <- "BIS.EMG"
  
  TOTPOW <- ccf(temp$BIS.TOTPOW,temp$Bx50.NIBP_SBP)
  BIS.TOTPOW <- TOTPOW[[1]][1:24]%>% data.frame()
  colnames(BIS.TOTPOW) <- "BIS.TOTPOW"
  TOTPOW_ <- ccf(temp$BIS.TOTPOW,temp$Bx50.NIBP_MBP)
  BIS.TOTPOW_ <- TOTPOW_[[1]][1:24]%>% data.frame()
  colnames(BIS.TOTPOW_) <- "BIS.TOTPOW"
  
  ENT_RD_EEG <- ccf(temp$Bx50.ENT_RD_EEG,temp$Bx50.NIBP_SBP)
  Bx50.ENT_RD_EEG <- ENT_RD_EEG[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_EEG) <- "Bx50.ENT_RD_EEG"
  ENT_RD_EEG_ <- ccf(temp$Bx50.ENT_RD_EEG,temp$Bx50.NIBP_MBP)
  Bx50.ENT_RD_EEG_ <- ENT_RD_EEG_[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_EEG_) <- "Bx50.ENT_RD_EEG"
    
  ENT_RD_EMG <- ccf(temp$Bx50.ENT_RD_EMG,temp$Bx50.NIBP_SBP)
  Bx50.ENT_RD_EMG <- ENT_RD_EMG[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_EMG) <- "Bx50.ENT_RD_EMG"
  ENT_RD_EMG_ <- ccf(temp$Bx50.ENT_RD_EMG,temp$Bx50.NIBP_MBP)
  Bx50.ENT_RD_EMG_ <- ENT_RD_EMG_[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_EMG_) <- "Bx50.ENT_RD_EMG"

  ENT_RD_BSR <- ccf(temp$Bx50.ENT_RD_BSR,temp$Bx50.NIBP_SBP)
  Bx50.ENT_RD_BSR <- ENT_RD_BSR[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_BSR) <- "Bx50.ENT_RD_BSR"
  ENT_RD_BSR_ <- ccf(temp$Bx50.ENT_RD_BSR,temp$Bx50.NIBP_MBP)
  Bx50.ENT_RD_BSR_ <- ENT_RD_BSR_[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_BSR_) <- "Bx50.ENT_RD_BSR"
  
  PROPOFOL_CP <- ccf(temp$Orchestra.PROPOFOL_CP,temp$Bx50.NIBP_SBP)
  Orchestra.PROPOFOL_CP <- PROPOFOL_CP[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CP) <- "Orchestra.PROPOFOL_CP"
  PROPOFOL_CP_ <- ccf(temp$Orchestra.PROPOFOL_CP,temp$Bx50.NIBP_MBP)
  Orchestra.PROPOFOL_CP_ <- PROPOFOL_CP_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CP_) <- "Orchestra.PROPOFOL_CP"
  
  PROPOFOL_CE <- ccf(temp$Orchestra.PROPOFOL_CE,temp$Bx50.NIBP_SBP)
  Orchestra.PROPOFOL_CE <- PROPOFOL_CE[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CE) <- "Orchestra.PROPOFOL_CE"
  PROPOFOL_CE_ <- ccf(temp$Orchestra.PROPOFOL_CE,temp$Bx50.NIBP_MBP)
  Orchestra.PROPOFOL_CE_ <- PROPOFOL_CE_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CE_) <- "Orchestra.PROPOFOL_CE"
  
  PROPOFOL_CT <- ccf(temp$Orchestra.PROPOFOL_CT,temp$Bx50.NIBP_SBP)
  Orchestra.PROPOFOL_CT <- PROPOFOL_CT[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CT) <- "Orchestra.PROPOFOL_CT"
  PROPOFOL_CT_ <- ccf(temp$Orchestra.PROPOFOL_CT,temp$Bx50.NIBP_MBP)
  Orchestra.PROPOFOL_CT_ <- PROPOFOL_CT_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CT_) <- "Orchestra.PROPOFOL_CT"
  
  REMIFENTANIL_CP <- ccf(temp$Orchestra.REMIFENTANIL_CP,temp$Bx50.NIBP_SBP)
  Orchestra.REMIFENTANIL_CP <- REMIFENTANIL_CP[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CP) <- "Orchestra.REMIFENTANIL_CP"
  REMIFENTANIL_CP_ <- ccf(temp$Orchestra.REMIFENTANIL_CP,temp$Bx50.NIBP_MBP)
  Orchestra.REMIFENTANIL_CP_ <- REMIFENTANIL_CP_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CP_) <- "Orchestra.REMIFENTANIL_CP"
  
  REMIFENTANIL_CE <- ccf(temp$Orchestra.REMIFENTANIL_CE,temp$Bx50.NIBP_SBP)
  Orchestra.REMIFENTANIL_CE <- REMIFENTANIL_CE[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CE) <- "Orchestra.REMIFENTANIL_CE"
  REMIFENTANIL_CE_ <- ccf(temp$Orchestra.REMIFENTANIL_CE,temp$Bx50.NIBP_MBP)
  Orchestra.REMIFENTANIL_CE_ <- REMIFENTANIL_CE_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CE_) <- "Orchestra.REMIFENTANIL_CE"
  
  REMIFENTANIL_CT <- ccf(temp$Orchestra.REMIFENTANIL_CT,temp$Bx50.NIBP_SBP)
  Orchestra.REMIFENTANIL_CT <- REMIFENTANIL_CT[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CT) <- "Orchestra.REMIFENTANIL_CT"
  REMIFENTANIL_CT_ <- ccf(temp$Orchestra.REMIFENTANIL_CT,temp$Bx50.NIBP_MBP)
  Orchestra.REMIFENTANIL_CT_ <- REMIFENTANIL_CT_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CT_) <- "Orchestra.REMIFENTANIL_CT"
  
  Hypo <- ccf(temp$Hypotension,temp$Bx50.NIBP_SBP)
  Hypotension <- Hypo[[1]][1:24]%>% data.frame()
  colnames(Hypotension) <- "Hypotension"
  Hypo_ <- ccf(temp$Hypotension,temp$Bx50.NIBP_MBP)
  Hypotension_ <- Hypo_[[1]][1:24]%>% data.frame()
  colnames(Hypotension_) <- "Hypotension"
  
  
  
  colSums(is.na(temp))
  
  # 데이터별 SBP와의 lag-correlation값 하나의 프레임으로 합침
  lagcor <- bind_cols(
     Bx50.HR
   ,Bx50.PLETH_SPO2
    ,Bx50.ETCO2
    ,Bx50.CO2
    ,Bx50.AMB_PRES
     ,Bx50.PLETH
 ,Bx50.RR_VENT
  ,Datex.Ohmeda.TV_EXP
  ,Datex.Ohmeda.MV_EXP
  ,Datex.Ohmeda.PIP
  # ,Datex.Ohmeda.COMPLIANCE
 ,BIS.SEF
 ,BIS.SQI
 ,BIS.EMG
    ,BIS.TOTPOW
 #   ,Bx50.ENT_RD_EEG
 #  ,Bx50.ENT_RD_EMG
 # ,Bx50.ENT_RD_BSR
    ,Orchestra.PROPOFOL_CP
    ,Orchestra.PROPOFOL_CE
    ,Orchestra.PROPOFOL_CT
    ,Orchestra.REMIFENTANIL_CP
    ,Orchestra.REMIFENTANIL_CE
    ,Orchestra.REMIFENTANIL_CT
    # ,Hypotension
  )
  
  # 데이터별 MBP와의 lag-correlation값 하나로 합침
  lagcor_ <- bind_cols(
     Bx50.HR_
   ,Bx50.PLETH_SPO2_
    ,Bx50.ETCO2_
    ,Bx50.CO2_
    ,Bx50.AMB_PRES_
    ,Bx50.PLETH_
    ,Bx50.RR_VENT_
    ,Datex.Ohmeda.TV_EXP_
    ,Datex.Ohmeda.MV_EXP_
    ,Datex.Ohmeda.PIP_
    # ,Datex.Ohmeda.COMPLIANCE_
    ,BIS.SEF_
    ,BIS.SQI_
    ,BIS.EMG_
    ,BIS.TOTPOW_
    # ,Bx50.ENT_RD_EEG_
    # ,Bx50.ENT_RD_EMG_
    # ,Bx50.ENT_RD_BSR_
    ,Orchestra.PROPOFOL_CP_
    ,Orchestra.PROPOFOL_CE_
     ,Orchestra.PROPOFOL_CT_
    ,Orchestra.REMIFENTANIL_CP_
    ,Orchestra.REMIFENTANIL_CE_
    ,Orchestra.REMIFENTANIL_CT_
    # ,Hypotension_
  )
  
  
  write.csv(lagcor, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_lagCOR\\","lagSBP_",fls[90],".csv"))
  write.csv(lagcor_, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_lagCOR_\\","lagMBP_",fls[90],".csv"))

  #데이터 객체 가져오기
  temp <- get(fls[90])
  
  # 돌릴 때마다 데이터 초기화
  Bx50.HR <- data.frame()
  Bx50.PLETH_SPO2 <- data.frame()
  Bx50.ETCO2 <- data.frame()
  Bx50.CO2 <- data.frame()
  Bx50.AMB_PRES <- data.frame()
  Bx50.PLETH <- data.frame()
  Bx50.RR_VENT <- data.frame()
  Datex.Ohmeda.TV_EXP <- data.frame()
  Datex.Ohmeda.MV_EXP <- data.frame()
  Datex.Ohmeda.PIP <- data.frame()
  Datex.Ohmeda.COMPLIANCE <- data.frame()
  BIS.SEF <- data.frame()
  BIS.SQI <- data.frame()
  BIS.EMG <- data.frame()
  BIS.TOTPOW <- data.frame()
  Bx50.ENT_RD_EEG <- data.frame()
  Bx50.ENT_RD_EMG <- data.frame()
  Bx50.ENT_RD_BSR <- data.frame()
  Orchestra.PROPOFOL_CP <- data.frame()
  Orchestra.PROPOFOL_CE <- data.frame()
  Orchestra.PROPOFOL_CT <- data.frame()
  Orchestra.REMIFENTANIL_CP <- data.frame()
  Orchestra.REMIFENTANIL_CE <- data.frame()
  Orchestra.REMIFENTANIL_CT <- data.frame()
  Hypotension <- data.frame()
  
  Bx50.HR_ <- data.frame()
  Bx50.PLETH_SPO2_ <- data.frame()
  Bx50.ETCO2_ <- data.frame()
  Bx50.CO2_ <- data.frame()
  Bx50.AMB_PRES_ <- data.frame()
  Bx50.PLETH_ <- data.frame()
  Bx50.RR_VENT_ <- data.frame()
  Datex.Ohmeda.TV_EXP_ <- data.frame()
  Datex.Ohmeda.MV_EXP_ <- data.frame()
  Datex.Ohmeda.PIP_ <- data.frame()
  Datex.Ohmeda.COMPLIANCE_ <- data.frame()
  BIS.SEF_ <- data.frame()
  BIS.SQI_ <- data.frame()
  BIS.EMG_ <- data.frame()
  BIS.TOTPOW_ <- data.frame()
  Bx50.ENT_RD_EEG_ <- data.frame()
  Bx50.ENT_RD_EMG_ <- data.frame()
  Bx50.ENT_RD_BSR_ <- data.frame()
  Orchestra.PROPOFOL_CP_ <- data.frame()
  Orchestra.PROPOFOL_CE_ <- data.frame()
  Orchestra.PROPOFOL_CT_ <- data.frame()
  Orchestra.REMIFENTANIL_CP_ <- data.frame()
  Orchestra.REMIFENTANIL_CE_ <- data.frame()
  Orchestra.REMIFENTANIL_CT_ <- data.frame()
  Hypotension_ <- data.frame()
  
  HR <- data.frame()
  PLETH_SPO2 <- data.frame()
  ETCO2 <- data.frame()
  CO2 <- data.frame()
  AMB_PRES <- data.frame()
  PLETH <- data.frame()
  RR_VENT <- data.frame()
  TV_EXP <- data.frame()
  MV_EXP <- data.frame()
  PIP <- data.frame()
  COMPLIANCE <- data.frame()
  SEF <- data.frame()
  SQI <- data.frame()
  EMG <- data.frame()
  TOTPOW <- data.frame()
  ENT_RD_EEG <- data.frame()
  ENT_RD_EMG <- data.frame()
  ENT_RD_BSR <- data.frame()
  PROPOFOL_CP <- data.frame()
  PROPOFOL_CE <- data.frame()
  PROPOFOL_CT <- data.frame()
  REMIFENTANIL_CP <- data.frame()
  REMIFENTANIL_CE <- data.frame()
  REMIFENTANIL_CT <- data.frame()
  Hypo <- data.frame()
  
  HR_ <- data.frame()
  PLETH_SPO2_ <- data.frame()
  ETCO2_ <- data.frame()
  CO2_ <- data.frame()
  AMB_PRES_ <- data.frame()
  PLETH_ <- data.frame()
  RR_VENT_ <- data.frame()
  Ohmeda.TV_EXP_ <- data.frame()
  Ohmeda.MV_EXP_ <- data.frame()
  Ohmeda.PIP_ <- data.frame()
  Ohmeda.COMPLIANCE_ <- data.frame()
  SEF_ <- data.frame()
  SQI_ <- data.frame()
  EMG_ <- data.frame()
  TOTPOW_ <- data.frame()
  ENT_RD_EEG_ <- data.frame()
  ENT_RD_EMG_ <- data.frame()
  ENT_RD_BSR_ <- data.frame()
  PROPOFOL_CP_ <- data.frame()
  PROPOFOL_CE_ <- data.frame()
  PROPOFOL_CT_ <- data.frame()
  REMIFENTANIL_CP_ <- data.frame()
  REMIFENTANIL_CE_ <- data.frame()
  REMIFENTANIL_CT_ <- data.frame()
  Hypo_ <- data.frame()
  
  
  
  ## 변수명 그대로 = SBP의 lag-correlation / 변수명_ = MBP의 lag-correlation
  # room12 데이터(변수명이 아예 다름)의 lag-correlation값 h=음수인 부분만 추출(x에 y가 영향받음)
  temp <- get(fls[21])
  HR <- ccf(temp$Solar.8000M.HR, temp$Solar.8000M.NIBP_SBP)
  Bx50.HR <- HR[[1]][1:24] %>% data.frame()
  colnames(Bx50.HR) <- "Bx50.HR"
  HR_ <- ccf(temp$Solar.8000M.HR, temp$Solar.8000M.NIBP_MBP)
  Bx50.HR_ <- HR_[[1]][1:24] %>% data.frame()
  colnames(Bx50.HR_) <- "Bx50.HR"
  
  PLETH_SPO2 <- ccf(temp$Solar.8000M.PLETH_SPO2,temp$Solar.8000M.NIBP_SBP)
  Bx50.PLETH_SPO2 <- PLETH_SPO2[[1]][1:24] %>% data.frame()
  colnames(Bx50.PLETH_SPO2) <- "Bx50.PLETH_SPO2"
  PLETH_SPO2_ <- ccf(temp$Solar.8000M.PLETH_SPO2,temp$Solar.8000M.NIBP_MBP)
  Bx50.PLETH_SPO2_ <- PLETH_SPO2_[[1]][1:24] %>% data.frame()
  colnames(Bx50.PLETH_SPO2_) <- "Bx50.PLETH_SPO2"
  
  ETCO2 <- ccf(temp$Solar.8000M.ETCO2,temp$Solar.8000M.NIBP_SBP)
  Bx50.ETCO2 <- ETCO2[[1]][1:24]%>% data.frame()
  colnames(Bx50.ETCO2) <- "Bx50.ETCO2"
  ETCO2_ <- ccf(temp$Solar.8000M.ETCO2,temp$Solar.8000M.NIBP_MBP)
  Bx50.ETCO2_ <- ETCO2_[[1]][1:24]%>% data.frame()
  colnames(Bx50.ETCO2_) <- "Bx50.ETCO2"
  
  CO2 <- ccf(temp$Bx50.CO2,temp$Solar.8000M.NIBP_SBP)
  Bx50.CO2 <- CO2[[1]][1:24]%>% data.frame()
  colnames(Bx50.CO2) <- "Bx50.CO2"
  CO2_ <- ccf(temp$Bx50.CO2,temp$Solar.8000M.NIBP_MBP)
  Bx50.CO2_ <- CO2_[[1]][1:24]%>% data.frame()
  colnames(Bx50.CO2_) <- "Bx50.CO2"
  
  AMB_PRES <- ccf(temp$Bx50.AMB_PRES, temp$Solar.8000M.NIBP_SBP)
  Bx50.AMB_PRES <- AMB_PRES[[1]][1:24]%>% data.frame()
  colnames(Bx50.AMB_PRES) <- "Bx50.AMB_PRES"
  AMB_PRES_ <- ccf(temp$Bx50.AMB_PRES, temp$Solar.8000M.NIBP_MBP)
  Bx50.AMB_PRES_ <- AMB_PRES_[[1]][1:24]%>% data.frame()
  colnames(Bx50.AMB_PRES_) <- "Bx50.AMB_PRES"
  
  PLETH <- ccf(temp$SNUADC.PLETH, temp$Solar.8000M.NIBP_SBP)
  Bx50.PLETH <- PLETH[[1]][1:24]%>% data.frame()
  colnames(Bx50.PLETH) <- "Bx50.PLETH"
  PLETH_ <- ccf(temp$SNUADC.PLETH, temp$Solar.8000M.NIBP_MBP)
  Bx50.PLETH_ <- PLETH_[[1]][1:24]%>% data.frame()
  colnames(Bx50.PLETH_) <- "Bx50.PLETH"
  
  RR_VENT <- ccf(temp$Solar.8000M.RR,temp$Solar.8000M.NIBP_SBP)
  Bx50.RR_VENT <- RR_VENT[[1]][1:24]%>% data.frame()
  colnames(Bx50.RR_VENT) <- "Bx50.RR_VENT"
  RR_VENT_ <- ccf(temp$Solar.8000M.RR,temp$Solar.8000M.NIBP_MBP)
  Bx50.RR_VENT_ <- RR_VENT_[[1]][1:24]%>% data.frame()
  colnames(Bx50.RR_VENT_) <- "Bx50.RR_VENT"
  
  TV_EXP <- ccf(temp$Datex.Ohmeda.TV_EXP, temp$Solar.8000M.NIBP_SBP)
  Datex.Ohmeda.TV_EXP <- TV_EXP[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.TV_EXP) <- "Datex.Ohmeda.TV_EXP"
  TV_EXP_ <- ccf(temp$Datex.Ohmeda.TV_EXP, temp$Solar.8000M.NIBP_MBP)
  Datex.Ohmeda.TV_EXP_ <- TV_EXP_[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.TV_EXP_) <- "Datex.Ohmeda.TV_EXP"
  
  MV_EXP <- ccf(temp$Datex.Ohmeda.MV_EXP,temp$Solar.8000M.NIBP_SBP)
  Datex.Ohmeda.MV_EXP <- MV_EXP[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.MV_EXP) <- "Datex.Ohmeda.MV_EXP"
  MV_EXP_ <- ccf(temp$Datex.Ohmeda.MV_EXP,temp$Solar.8000M.NIBP_MBP)
  Datex.Ohmeda.MV_EXP_ <- MV_EXP_[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.MV_EXP_) <- "Datex.Ohmeda.MV_EXP"
  
  PIP <- ccf(temp$Datex.Ohmeda.PIP,temp$Solar.8000M.NIBP_SBP)
  Datex.Ohmeda.PIP <- PIP[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.PIP) <- "Datex.Ohmeda.PIP"
  PIP_ <- ccf(temp$Datex.Ohmeda.PIP,temp$Solar.8000M.NIBP_MBP)
  Datex.Ohmeda.PIP_ <- PIP_[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.PIP_) <- "Datex.Ohmeda.PIP"
  
  COMPLIANCE <- ccf(temp$Datex.Ohmeda.COMPLIANCE,temp$Solar.8000M.NIBP_SBP)
  Datex.Ohmeda.COMPLIANCE <- COMPLIANCE[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.COMPLIANCE) <- "Datex.Ohmeda.COMPLIANCE"
  COMPLIANCE_ <- ccf(temp$Datex.Ohmeda.COMPLIANCE,temp$Solar.8000M.NIBP_MBP)
  Datex.Ohmeda.COMPLIANCE_ <- COMPLIANCE_[[1]][1:24]%>% data.frame()
  colnames(Datex.Ohmeda.COMPLIANCE_) <- "Datex.Ohmeda.COMPLIANCE"
  
  SEF <- ccf(temp$BIS.SEF,temp$Solar.8000M.NIBP_SBP)
  BIS.SEF <- SEF[[1]][1:24]%>% data.frame()
  colnames(BIS.SEF) <- "BIS.SEF"
  SEF_ <- ccf(temp$BIS.SEF,temp$Solar.8000M.NIBP_MBP)
  BIS.SEF_ <- SEF_[[1]][1:24]%>% data.frame()
  colnames(BIS.SEF_) <- "BIS.SEF"
  
  SQI <- ccf(temp$BIS.SQI,temp$Solar.8000M.NIBP_SBP)
  BIS.SQI <- SQI[[1]][1:24]%>% data.frame()
  colnames(BIS.SQI) <- "BIS.SQI"
  SQI_ <- ccf(temp$BIS.SQI,temp$Solar.8000M.NIBP_MBP)
  BIS.SQI_ <- SQI_[[1]][1:24]%>% data.frame()
  colnames(BIS.SQI_) <- "BIS.SQI"
  
  EMG <- ccf(temp$BIS.EMG,temp$Solar.8000M.NIBP_SBP)
  BIS.EMG <- EMG[[1]][1:24]%>% data.frame()
  colnames(BIS.EMG) <- "BIS.EMG"
  EMG_ <- ccf(temp$BIS.EMG,temp$Solar.8000M.NIBP_MBP)
  BIS.EMG_ <- EMG_[[1]][1:24]%>% data.frame()
  colnames(BIS.EMG_) <- "BIS.EMG"
  
  TOTPOW <- ccf(temp$BIS.TOTPOW,temp$Solar.8000M.NIBP_SBP)
  BIS.TOTPOW <- TOTPOW[[1]][1:24]%>% data.frame()
  colnames(BIS.TOTPOW) <- "BIS.TOTPOW"
  TOTPOW_ <- ccf(temp$BIS.TOTPOW,temp$Solar.8000M.NIBP_MBP)
  BIS.TOTPOW_ <- TOTPOW_[[1]][1:24]%>% data.frame()
  colnames(BIS.TOTPOW_) <- "BIS.TOTPOW"
  
  ENT_RD_EEG <- ccf(temp$Bx50.ENT_RD_EEG,temp$Solar.8000M.NIBP_SBP)
  Bx50.ENT_RD_EEG <- ENT_RD_EEG[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_EEG) <- "Bx50.ENT_RD_EEG"
  ENT_RD_EEG_ <- ccf(temp$Bx50.ENT_RD_EEG,temp$Solar.8000M.NIBP_MBP)
  Bx50.ENT_RD_EEG_ <- ENT_RD_EEG_[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_EEG_) <- "Bx50.ENT_RD_EEG"
  
  ENT_RD_EMG <- ccf(temp$Bx50.ENT_RD_EMG,temp$Solar.8000M.NIBP_SBP)
  Bx50.ENT_RD_EMG <- ENT_RD_EMG[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_EMG) <- "Bx50.ENT_RD_EMG"
  ENT_RD_EMG_ <- ccf(temp$Bx50.ENT_RD_EMG,temp$Solar.8000M.NIBPMBP)
  Bx50.ENT_RD_EMG_ <- ENT_RD_EMG_[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_EMG_) <- "Bx50.ENT_RD_EMG"
  
  ENT_RD_BSR <- ccf(temp$Bx50.ENT_RD_BSR,temp$Solar.8000M.NIBP_SBP)
  Bx50.ENT_RD_BSR <- ENT_RD_BSR[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_BSR) <- "Bx50.ENT_RD_BSR"
  ENT_RD_BSR_ <- ccf(temp$Bx50.ENT_RD_BSR,temp$Solar.8000M.NIBP_MBP)
  Bx50.ENT_RD_BSR_ <- ENT_RD_BSR_[[1]][1:24]%>% data.frame()
  colnames(Bx50.ENT_RD_BSR_) <- "Bx50.ENT_RD_BSR"
  
  PROPOFOL_CP <- ccf(temp$Orchestra.PROPOFOL_CP,temp$Solar.8000M.NIBP_SBP)
  Orchestra.PROPOFOL_CP <- PROPOFOL_CP[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CP) <- "Orchestra.PROPOFOL_CP"
  PROPOFOL_CP_ <- ccf(temp$Orchestra.PROPOFOL_CP,temp$Solar.8000M.NIBP_MBP)
  Orchestra.PROPOFOL_CP_ <- PROPOFOL_CP_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CP_) <- "Orchestra.PROPOFOL_CP"
  
  PROPOFOL_CE <- ccf(temp$Orchestra.PROPOFOL_CE,temp$Solar.8000M.NIBP_SBP)
  Orchestra.PROPOFOL_CE <- PROPOFOL_CE[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CE) <- "Orchestra.PROPOFOL_CE"
  PROPOFOL_CE_ <- ccf(temp$Orchestra.PROPOFOL_CE,temp$Solar.8000M.NIBP_MBP)
  Orchestra.PROPOFOL_CE_ <- PROPOFOL_CE_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CE_) <- "Orchestra.PROPOFOL_CE"
  
  PROPOFOL_CT <- ccf(temp$Orchestra.PROPOFOL_CT,temp$Solar.8000M.NIBP_SBP)
  Orchestra.PROPOFOL_CT <- PROPOFOL_CT[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CT) <- "Orchestra.PROPOFOL_CT"
  PROPOFOL_CT_ <- ccf(temp$Orchestra.PROPOFOL_CT,temp$Solar.8000M.NIBP_MBP)
  Orchestra.PROPOFOL_CT_ <- PROPOFOL_CT_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.PROPOFOL_CT_) <- "Orchestra.PROPOFOL_CT"
  
  REMIFENTANIL_CP <- ccf(temp$Orchestra.REMIFENTANIL_CP,temp$Solar.8000M.NIBP_SBP)
  Orchestra.REMIFENTANIL_CP <- REMIFENTANIL_CP[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CP) <- "Orchestra.REMIFENTANIL_CP"
  REMIFENTANIL_CP_ <- ccf(temp$Orchestra.REMIFENTANIL_CP,temp$Solar.8000M.NIBP_MBP)
  Orchestra.REMIFENTANIL_CP_ <- REMIFENTANIL_CP_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CP_) <- "Orchestra.REMIFENTANIL_CP"
  
  REMIFENTANIL_CE <- ccf(temp$Orchestra.REMIFENTANIL_CE,temp$Solar.8000M.NIBP_SBP)
  Orchestra.REMIFENTANIL_CE <- REMIFENTANIL_CE[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CE) <- "Orchestra.REMIFENTANIL_CE"
  REMIFENTANIL_CE_ <- ccf(temp$Orchestra.REMIFENTANIL_CE,temp$Solar.8000M.NIBP_MBP)
  Orchestra.REMIFENTANIL_CE_ <- REMIFENTANIL_CE_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CE_) <- "Orchestra.REMIFENTANIL_CE"
  
  REMIFENTANIL_CT <- ccf(temp$Orchestra.REMIFENTANIL_CT,temp$Solar.8000M.NIBP_SBP)
  Orchestra.REMIFENTANIL_CT <- REMIFENTANIL_CT[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CT) <- "Orchestra.REMIFENTANIL_CT"
  REMIFENTANIL_CT_ <- ccf(temp$Orchestra.REMIFENTANIL_CT,temp$Solar.8000M.NIBP_MBP)
  Orchestra.REMIFENTANIL_CT_ <- REMIFENTANIL_CT_[[1]][1:24]%>% data.frame()
  colnames(Orchestra.REMIFENTANIL_CT_) <- "Orchestra.REMIFENTANIL_CT"
  
  Hypo <- ccf(temp$Hypotension,temp$Solar.8000M.NIBP_SBP)
  Hypotension <- Hypo[[1]][1:24]%>% data.frame()
  colnames(Hypotension) <- "Hypotension"
  Hypo_ <- ccf(temp$Hypotension,temp$Solar.8000M.NIBP_MBP)
  Hypotension_ <- Hypo_[[1]][1:24]%>% data.frame()
  colnames(Hypotension_) <- "Hypotension"
  
  
  
  colSums(is.na(temp))
  
  # 데이터별 SBP와의 lag-correlation값 하나의 프레임으로 합침
  lagcor <- bind_cols(
    Bx50.HR
    ,Bx50.PLETH_SPO2
    ,Bx50.ETCO2
    # ,Bx50.CO2
    # ,Bx50.AMB_PRES
    ,Bx50.PLETH
    ,Bx50.RR_VENT
    # ,Datex.Ohmeda.TV_EXP
    # ,Datex.Ohmeda.MV_EXP
    # ,Datex.Ohmeda.PIP
    # ,Datex.Ohmeda.COMPLIANCE
    ,BIS.SEF
    ,BIS.SQI
    ,BIS.EMG
    ,BIS.TOTPOW
    #   ,Bx50.ENT_RD_EEG
    #  ,Bx50.ENT_RD_EMG
    # ,Bx50.ENT_RD_BSR
    ,Orchestra.PROPOFOL_CP
    ,Orchestra.PROPOFOL_CE
    ,Orchestra.PROPOFOL_CT
    ,Orchestra.REMIFENTANIL_CP
    ,Orchestra.REMIFENTANIL_CE
    ,Orchestra.REMIFENTANIL_CT
    ,Hypotension
  )
  
  # 데이터별 MBP와의 lag-correlation값 하나로 합침
  lagcor_ <- bind_cols(
    Bx50.HR_
    ,Bx50.PLETH_SPO2_
    ,Bx50.ETCO2_
    # ,Bx50.CO2_
    # ,Bx50.AMB_PRES_
    ,Bx50.PLETH_
    ,Bx50.RR_VENT_
    # ,Datex.Ohmeda.TV_EXP_
    # ,Datex.Ohmeda.MV_EXP_
    # ,Datex.Ohmeda.PIP_
    # ,Datex.Ohmeda.COMPLIANCE_
    ,BIS.SEF_
    ,BIS.SQI_
    ,BIS.EMG_
    ,BIS.TOTPOW_
    # ,Bx50.ENT_RD_EEG_
    # ,Bx50.ENT_RD_EMG_
    # ,Bx50.ENT_RD_BSR_
    ,Orchestra.PROPOFOL_CP_
    ,Orchestra.PROPOFOL_CE_
    ,Orchestra.PROPOFOL_CT_
    ,Orchestra.REMIFENTANIL_CP_
    ,Orchestra.REMIFENTANIL_CE_
    ,Orchestra.REMIFENTANIL_CT_
    ,Hypotension_
  )
  
  
  write.csv(lagcor, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_lagCOR\\","lagSBP_",fls[21],".csv"))
  write.csv(lagcor_, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_lagCOR_\\","lagMBP_",fls[21],".csv"))
  # }},
  # 
  # error = function(e){ #e는 오류 내용
  #   # 위(실행할 구문)에서 오류(error) 발생 시 수행할 구문
  #   cat("->에러발생")
  # })

### SBP ###
l <- "D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_lagCOR"
li <- dir(l, recursive=TRUE)
df <- read.csv("D:\\은서\\은서대학교생활\\2021 은서 연구생\\bottom.csv")
class(df)
# 전체 데이터셋 합치기
library(stringr)
library(dplyr)
for (i in li){
  a <- file.path(str_c(l,"/", i))
  temp <- read.csv(a)
  df <- bind_rows(df, temp)
  df <- subset(df, select = -c(X, Bx50.NIBP_SBP, Bx50.NIBP_MBP))
}

# 변수마다 분리해서 저장
for (x in 1:length(df)){
  k <- df[x]
  write.csv(k, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_SBP\\","SBP",names(df[x]),".csv"))
}

# 변수별로 데이터 합쳐서 변수명 GROUP로 설정하기
kll <- "D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_SBP"
kl <- dir(kll, recursive=TRUE)
lagSBP <- data.frame()
for (z in kl){
  a <- file.path(str_c(kll,"/", z))
  temp <- read.csv(a)
  tmp <- temp[,2] %>% data.frame
  #tmp <- na.omit(tmp)
  tmp$group <- colnames(temp[2])
  lagSBP <- bind_rows(lagSBP,tmp)}
colnames(lagSBP) <- c("lagcor","group")
# 변수별 상관관계 0.7이상인 것들의 갯수 세기
lagSBP <- na.omit(lagSBP)
lagSBP_0.7 <- filter(lagSBP, abs(lagcor)>0.7 & abs(lagcor)<1)
lagSBP_0.7_table <- table(lagSBP_0.7$group) %>% data.frame()
lagSBP_table <- table(lagSBP$group) %>% data.frame()

library(dplyr)
join <- right_join(lagSBP_table, lagSBP_0.7_table, by="Var1")
join$percent <- join$Freq.y/join$Freq.x
colnames(join) <- c("Feature","전체 빈도","상위70% 빈도","상위70%빈도/전체빈도")
### MBP ###
y <- "D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_lagCOR_"
yi <- dir(y, recursive=TRUE)
df <- read.csv("D:\\은서\\은서대학교생활\\2021 은서 연구생\\bottom.csv")
class(df)
# 전체 데이터셋 합치기
library(stringr)
library(dplyr)
for (i in yi){
  a <- file.path(str_c(y,"/", i))
  temp <- read.csv(a)
  df <- bind_rows(df, temp)
  df <- subset(df, select = -c(X))
  }
  
# 변수마다 분리해서 저장
for (x in 1:length(df)){
  k <- df[x]
  write.csv(k, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_MBP\\","MBP",names(df[x]),".csv"))
}

# 변수별로 데이터 합쳐서 변수명 GROUP로 설정하기
kll <- "D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL_MBP"
kl <- dir(kll, recursive=TRUE)
lagMBP <- data.frame()
for (z in kl){
  a <- file.path(str_c(kll,"/", z))
  temp <- read.csv(a)
  tmp <- temp[,2] %>% data.frame
  #tmp <- na.omit(tmp)
  tmp$group <- colnames(temp[2])
  lagMBP <- bind_rows(lagMBP,tmp)}
colnames(lagMBP) <- c("lagcor_","group")
# 변수별 상관관계 0.7이상인 것들의 갯수 세기
lagMBP <- na.omit(lagMBP)
lagMBP_0.7 <- filter(lagMBP, abs(lagcor_)>0.7 & abs(lagcor_)<1)
lagMBP_0.7_table <- table(lagMBP_0.7$group) %>% data.frame()
lagMBP_table <- table(lagMBP$group) %>% data.frame()

library(dplyr)
joinM <- right_join(lagMBP_table, lagMBP_0.7_table, by="Var1")
joinM$percent <- joinM$Freq.y/joinM$Freq.x
colnames(joinM) <- c("Feature","전체 빈도","상위70% 빈도","상위70%빈도/전체빈도")



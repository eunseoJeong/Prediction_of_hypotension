# 패키지 불러오기
library(dplyr)
library(car) # vif 확인
library(e1071) # svm모델 생성
library(Metrics) # rmse계산



# --- < 1. lag-correlation값 계산 > ---

#디렉토리 설정하기
setwd("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\Data")
d <- getwd()
fls <- dir(d, recursive=TRUE)
lag <- data.frame(c(-24:-20))
# 파일 불러오기 및 lag-correlation 계산
for(i in fls){
  # 파일 불러오기
  temp <- read.csv(paste0(d,"\\",i))
  temp$Time <- NULL
    ##select_if(~sum(!is.na(.)) > 0) == 모두가 NA인 열 제외
  temp <- temp %>% select_if(~sum(!is.na(.)) > 0)

  # 돌릴 때마다 데이터 초기화
    ## SBP와의 lag-correlation
  Bx50.HR <- data.frame()
  Bx50.PLETH_SPO2 <- data.frame()
  Bx50.ETCO2 <- data.frame()
  Bx50.CO2 <- data.frame()
  Bx50.AMB_PRES <- data.frame()
  Bx50.NIBP_SBP <- data.frame()
  Bx50.NIBP_MBP <- data.frame()
  Bx50.PLETH <- data.frame()
  Bx50.RR_VENT <- data.frame()
  Datex.Ohmeda.TV_EXP <- data.frame()
  Datex.Ohmeda.MV_EXP <- data.frame()
  Datex.Ohmeda.PIP <- data.frame()
  Datex.Ohmeda.COMPLIANCE <- data.frame()
  BIS.SEF <- data.frame()
  BIS.SQI <- data.frame()
  BIS.EMG <- data.frame()
  BIS.BIS <- data.frame()
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
  
    ## MBP와의 lag-correlation
  Bx50.HR_ <- data.frame()
  Bx50.PLETH_SPO2_ <- data.frame()
  Bx50.ETCO2_ <- data.frame()
  Bx50.CO2_ <- data.frame()
  Bx50.AMB_PRES_ <- data.frame()
  Bx50.NIBP_SBP_ <- data.frame()
  Bx50.NIBP_MBP_ <- data.frame()
  Bx50.PLETH_ <- data.frame()
  Bx50.RR_VENT_ <- data.frame()
  Datex.Ohmeda.TV_EXP_ <- data.frame()
  Datex.Ohmeda.MV_EXP_ <- data.frame()
  Datex.Ohmeda.PIP_ <- data.frame()
  Datex.Ohmeda.COMPLIANCE_ <- data.frame()
  BIS.SEF_ <- data.frame()
  BIS.SQI_ <- data.frame()
  BIS.EMG_ <- data.frame()
  BIS.BIS_ <- data.frame()
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
  
    ## ccf 계산결과(SBP)
  HR <- data.frame()
  PLETH_SPO2 <- data.frame()
  ETCO2 <- data.frame()
  CO2 <- data.frame()
  AMB_PRES <- data.frame()
  SBP <- data.frame()
  MBP <- data.frame()
  PLETH <- data.frame()
  RR_VENT <- data.frame()
  TV_EXP <- data.frame()
  MV_EXP <- data.frame()
  PIP <- data.frame()
  COMPLIANCE <- data.frame()
  SEF <- data.frame()
  SQI <- data.frame()
  EMG <- data.frame()
  BIS <- data.frame()
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
  
    ## ccf계산결과(MBP)
  HR_ <- data.frame()
  PLETH_SPO2_ <- data.frame()
  ETCO2_ <- data.frame()
  CO2_ <- data.frame()
  AMB_PRES_ <- data.frame()
  SBP_ <- data.frame()
  MBP_ <- data.frame()
  PLETH_ <- data.frame()
  RR_VENT_ <- data.frame()
  Ohmeda.TV_EXP_ <- data.frame()
  Ohmeda.MV_EXP_ <- data.frame()
  Ohmeda.PIP_ <- data.frame()
  Ohmeda.COMPLIANCE_ <- data.frame()
  SEF_ <- data.frame()
  SQI_ <- data.frame()
  EMG_ <- data.frame()
  BIS_ <- data.frame()
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
  # 변수 내 값이 일정할 경우 에러발생(Error in plot.window(...) : 유한한 값들만이 'ylim'에 사용될 수 있습니다) -> 해결: length(unique(temp$변수)) > 1 == TRUE 조건 추가
  if("Bx50.NIBP_SBP" %in% colnames(temp) & "Bx50.NIBP_MBP" %in% colnames(temp) == TRUE){
    # 1. Bx50.HR
    if("HR" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.HR)) > 1){
      HR <- ccf(temp$Bx50.HR,temp$Bx50.NIBP_SBP)
      Bx50.HR <- HR[[1]][1:5] %>% data.frame()
      colnames(Bx50.HR) <- "Bx50.HR"
      HR_ <- ccf(temp$Bx50.HR,temp$Bx50.NIBP_MBP)
      Bx50.HR_ <- HR_[[1]][1:5] %>% data.frame()
      colnames(Bx50.HR_) <- "Bx50.HR"
    } else {
      Bx50.HR <- NA
      Bx50.HR_ <- NA      
    }
    # 2. Bx50.PLETH_SPO2
    if("Bx50.PLETH_SPO2" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.PLETH_SPO2)) > 1){
      PLETH_SPO2 <- ccf(temp$Bx50.PLETH_SPO2,temp$Bx50.NIBP_SBP)
      Bx50.PLETH_SPO2 <- PLETH_SPO2[[1]][1:5] %>% data.frame()
      colnames(Bx50.PLETH_SPO2) <- "Bx50.PLETH_SPO2"
      PLETH_SPO2_ <- ccf(temp$Bx50.PLETH_SPO2,temp$Bx50.NIBP_MBP)
      Bx50.PLETH_SPO2_ <- PLETH_SPO2_[[1]][1:5] %>% data.frame()
      colnames(Bx50.PLETH_SPO2_) <- "Bx50.PLETH_SPO2"
    } else {
      Bx50.PLETH_SPO2 <- NA
      Bx50.PLETH_SPO2_ <- NA
    }
    # 3. Bx50.ETCO2
    if("Bx50.ETCO2" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.ETCO2)) > 1){
      ETCO2 <- ccf(temp$Bx50.ETCO2,temp$Bx50.NIBP_SBP)
      Bx50.ETCO2 <- ETCO2[[1]][1:5]%>% data.frame()
      colnames(Bx50.ETCO2) <- "Bx50.ETCO2"
      ETCO2_ <- ccf(temp$Bx50.ETCO2,temp$Bx50.NIBP_MBP)
      Bx50.ETCO2_ <- ETCO2_[[1]][1:5]%>% data.frame()
      colnames(Bx50.ETCO2_) <- "Bx50.ETCO2"
    } else {
      Bx50.ETCO2 <- NA
      Bx50.ETCO2_ <- NA
    }
    # 4. Bx50.CO2
    if("Bx50.CO2" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.CO2)) > 1){
      CO2 <- ccf(temp$Bx50.CO2,temp$Bx50.NIBP_SBP)
      Bx50.CO2 <- CO2[[1]][1:5]%>% data.frame()
      colnames(Bx50.CO2) <- "Bx50.CO2"
      CO2_ <- ccf(temp$Bx50.CO2,temp$Bx50.NIBP_MBP)
      Bx50.CO2_ <- CO2_[[1]][1:5]%>% data.frame()
      colnames(Bx50.CO2_) <- "Bx50.CO2"
    } else {
      Bx50.CO2 <- NA
      Bx50.CO2_ <- NA
    }
    # 5. Bx50.AMB_PRES
    if("Bx50.AMB_PRES" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.AMB_PRES)) > 1){
      AMB_PRES <- ccf(temp$Bx50.AMB_PRES,temp$Bx50.NIBP_SBP)
      Bx50.AMB_PRES <- AMB_PRES[[1]][1:5]%>% data.frame()
      colnames(Bx50.AMB_PRES) <- "Bx50.AMB_PRES"
      AMB_PRES_ <- ccf(temp$Bx50.AMB_PRES,temp$Bx50.NIBP_MBP)
      Bx50.AMB_PRES_ <- AMB_PRES_[[1]][1:5]%>% data.frame()
      colnames(Bx50.AMB_PRES_) <- "Bx50.AMB_PRES"
    } else {
      Bx50.AMB_PRES <- NA
      Bx50.AMB_PRES_ <- NA
    }
    # 6. Bx50.NIBP_SBP
    if("Bx50.NIBP_SBP" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.NIBP_SBP)) > 1){
      SBP <- ccf(temp$Bx50.NIBP_SBP,temp$Bx50.NIBP_SBP)
      Bx50.NIBP_SBP <- SBP[[1]][1:5]%>% data.frame()
      colnames(Bx50.NIBP_SBP) <- "Bx50.NIBP_SBP"
      SBP_ <- ccf(temp$Bx50.NIBP_SBP,temp$Bx50.NIBP_MBP)
      Bx50.NIBP_SBP_ <- SBP_[[1]][1:5]%>% data.frame()
      colnames(Bx50.NIBP_SBP_) <- "Bx50.NIBP_SBP"
    } else {
      Bx50.NIBP_SBP <- NA
      Bx50.NIBP_SBP_ <- NA
    }
    # 7. Bx50.NIBP_MBP
    if("Bx50.NIBP_MBP" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.NIBP_MBP)) > 1){
      MBP <- ccf(temp$Bx50.NIBP_MBP,temp$Bx50.NIBP_SBP)
      Bx50.NIBP_MBP <- MBP[[1]][1:5]%>% data.frame()
      colnames(Bx50.NIBP_MBP) <- "Bx50.NIBP_MBP"
      MBP_ <- ccf(temp$Bx50.NIBP_MBP,temp$Bx50.NIBP_MBP)
      Bx50.NIBP_MBP_ <- MBP_[[1]][1:5]%>% data.frame()
      colnames(Bx50.NIBP_MBP_) <- "Bx50.NIBP_MBP"
    } else {
      Bx50.NIBP_MBP <- NA
      Bx50.NIBP_MBP_ <- NA
    }
    # 8. Bx50.PLETH
    if("Bx50.PLETH" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.PLETH)) > 1){
      PLETH <- ccf(temp$Bx50.PLETH,temp$Bx50.NIBP_SBP)
      Bx50.PLETH <- PLETH[[1]][1:5]%>% data.frame()
      colnames(Bx50.PLETH) <- "Bx50.PLETH"
      PLETH_ <- ccf(temp$Bx50.PLETH,temp$Bx50.NIBP_MBP)
      Bx50.PLETH_ <- PLETH_[[1]][1:5]%>% data.frame()
      colnames(Bx50.PLETH_) <- "Bx50.PLETH"
    } else {
      Bx50.PLETH <- NA
      Bx50.PLETH_ <- NA
    }
    # 9. Bx50.RR_VENT
    if("Bx50.RR_VENT" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.RR_VENT)) > 1){
      RR_VENT <- ccf(temp$Bx50.RR_VENT,temp$Bx50.NIBP_SBP)
      Bx50.RR_VENT <- RR_VENT[[1]][1:5]%>% data.frame()
      colnames(Bx50.RR_VENT) <- "Bx50.RR_VENT"
      RR_VENT_ <- ccf(temp$Bx50.RR_VENT,temp$Bx50.NIBP_MBP)
      Bx50.RR_VENT_ <- RR_VENT_[[1]][1:5]%>% data.frame()
      colnames(Bx50.RR_VENT_) <- "Bx50.RR_VENT"
    } else {
      Bx50.RR_VENT <- NA
      Bx50.RR_VENT_ <- NA
    }
    # 10. Datex.Ohmeda.TV_EXP
    if("Datex.Ohmeda.TV_EXP" %in% colnames(temp) == TRUE & length(unique(temp$Datex.Ohmeda.TV_EXP)) > 1){
      TV_EXP <- ccf(temp$Datex.Ohmeda.TV_EXP,temp$Bx50.NIBP_SBP)
      Datex.Ohmeda.TV_EXP <- TV_EXP[[1]][1:5]%>% data.frame()
      colnames(Datex.Ohmeda.TV_EXP) <- "Datex.Ohmeda.TV_EXP"
      TV_EXP_ <- ccf(temp$Datex.Ohmeda.TV_EXP,temp$Bx50.NIBP_MBP)
      Datex.Ohmeda.TV_EXP_ <- TV_EXP_[[1]][1:5]%>% data.frame()
      colnames(Datex.Ohmeda.TV_EXP_) <- "Datex.Ohmeda.TV_EXP"
    } else {
      Datex.Ohmeda.TV_EXP <- NA
      Datex.Ohmeda.TV_EXP_ <- NA
    }
    # 11. Datex.Ohmeda.MV_EXP
    if("Datex.Ohmeda.MV_EXP" %in% colnames(temp) == TRUE & length(unique(temp$Datex.Ohmeda.MV_EXP)) > 1){
      MV_EXP <- ccf(temp$Datex.Ohmeda.MV_EXP,temp$Bx50.NIBP_SBP)
      Datex.Ohmeda.MV_EXP <- MV_EXP[[1]][1:5]%>% data.frame()
      colnames(Datex.Ohmeda.MV_EXP) <- "Datex.Ohmeda.MV_EXP"
      MV_EXP_ <- ccf(temp$Datex.Ohmeda.MV_EXP,temp$Bx50.NIBP_MBP)
      Datex.Ohmeda.MV_EXP_ <- MV_EXP_[[1]][1:5]%>% data.frame()
      colnames(Datex.Ohmeda.MV_EXP_) <- "Datex.Ohmeda.MV_EXP"
    } else {
      Datex.Ohmeda.MV_EXP <- NA
      Datex.Ohmeda.MV_EXP_ <- NA
    }
    # 12. Datex.Ohmeda.PIP
    if("Datex.Ohmeda.PIP" %in% colnames(temp) == TRUE & length(unique(temp$Datex.Ohmeda.PIP)) > 1){
      PIP <- ccf(temp$Datex.Ohmeda.PIP,temp$Bx50.NIBP_SBP)
      Datex.Ohmeda.PIP <- PIP[[1]][1:5]%>% data.frame()
      colnames(Datex.Ohmeda.PIP) <- "Datex.Ohmeda.PIP"
      PIP_ <- ccf(temp$Datex.Ohmeda.PIP,temp$Bx50.NIBP_MBP)
      Datex.Ohmeda.PIP_ <- PIP_[[1]][1:5]%>% data.frame()
      colnames(Datex.Ohmeda.PIP_) <- "Datex.Ohmeda.PIP"
    } else {
      Datex.Ohmeda.PIP <- NA
      Datex.Ohmeda.PIP_ <- NA
    }
    # 13. Datex.Ohmeda.COMPLIANCE
    if("Datex.Ohmeda.COMPLIANCE" %in% colnames(temp) == TRUE & length(unique(temp$Datex.Ohmeda.COMPLIANCE)) > 1){
      COMPLIANCE <- ccf(temp$Datex.Ohmeda.COMPLIANCE,temp$Bx50.NIBP_SBP)
      Datex.Ohmeda.COMPLIANCE <- COMPLIANCE[[1]][1:5]%>% data.frame()
      colnames(Datex.Ohmeda.COMPLIANCE) <- "Datex.Ohmeda.COMPLIANCE"
      COMPLIANCE_ <- ccf(temp$Datex.Ohmeda.COMPLIANCE,temp$Bx50.NIBP_MBP)
      Datex.Ohmeda.COMPLIANCE_ <- COMPLIANCE_[[1]][1:5]%>% data.frame()
      colnames(Datex.Ohmeda.COMPLIANCE_) <- "Datex.Ohmeda.COMPLIANCE"
    } else {
      Datex.Ohmeda.COMPLIANCE <- NA
      Datex.Ohmeda.COMPLIANCE_ <- NA
    }
    # 14. BIS.SEF
    if("BIS.SEF" %in% colnames(temp) == TRUE & length(unique(temp$BIS.SEF)) > 1){
      SEF <- ccf(temp$BIS.SEF,temp$Bx50.NIBP_SBP)
      BIS.SEF <- SEF[[1]][1:5]%>% data.frame()
      colnames(BIS.SEF) <- "BIS.SEF"
      SEF_ <- ccf(temp$BIS.SEF,temp$Bx50.NIBP_MBP)
      BIS.SEF_ <- SEF_[[1]][1:5]%>% data.frame()
      colnames(BIS.SEF_) <- "BIS.SEF"
    } else {
      BIS.SEF <- NA
      BIS.SEF_ <- NA
    }
    # 15. BIS.SQI
    if("BIS.SQI" %in% colnames(temp) == TRUE & length(unique(temp$BIS.SQI)) > 1){
      SQI <- ccf(temp$BIS.SQI,temp$Bx50.NIBP_SBP)
      BIS.SQI <- SQI[[1]][1:5]%>% data.frame()
      colnames(BIS.SQI) <- "BIS.SQI"
      SQI_ <- ccf(temp$BIS.SQI,temp$Bx50.NIBP_MBP)
      BIS.SQI_ <- SQI_[[1]][1:5]%>% data.frame()
      colnames(BIS.SQI_) <- "BIS.SQI"
    } else {
      BIS.SQI <- NA
      BIS.SQI_ <- NA
    }
    # 16. BIS.EMG
    if("BIS.EMG" %in% colnames(temp) == TRUE & length(unique(temp$BIS.EMG)) > 1){
      EMG <- ccf(temp$BIS.EMG,temp$Bx50.NIBP_SBP)
      BIS.EMG <- EMG[[1]][1:5]%>% data.frame()
      colnames(BIS.EMG) <- "BIS.EMG"
      EMG_ <- ccf(temp$BIS.EMG,temp$Bx50.NIBP_MBP)
      BIS.EMG_ <- EMG_[[1]][1:5]%>% data.frame()
      colnames(BIS.EMG_) <- "BIS.EMG"
    } else {
      BIS.EMG <- NA
      BIS.EMG_ <- NA
    }
    # 17. BIS.BIS
    if("BIS.BIS" %in% colnames(temp) == TRUE & length(unique(temp$BIS.BIS)) > 1){
      BIS <- ccf(temp$BIS.BIS,temp$Bx50.NIBP_SBP)
      BIS.BIS <- BIS[[1]][1:5]%>% data.frame()
      colnames(BIS.BIS) <- "BIS.BIS"
      BIS_ <- ccf(temp$BIS.BIS,temp$Bx50.NIBP_MBP)
      BIS.BIS_ <- BIS_[[1]][1:5]%>% data.frame()
      colnames(BIS.BIS_) <- "BIS.BIS"
    } else {
      BIS.BIS <- NA
      BIS.BIS_ <- NA
    }
    # 18. BIS.TOTPOW
    if("BIS.TOTPOW" %in% colnames(temp) == TRUE & length(unique(temp$BIS.TOTPOW)) > 1){
      TOTPOW <- ccf(temp$BIS.TOTPOW,temp$Bx50.NIBP_SBP)
      BIS.TOTPOW <- TOTPOW[[1]][1:5]%>% data.frame()
      colnames(BIS.TOTPOW) <- "BIS.TOTPOW"
      TOTPOW_ <- ccf(temp$BIS.TOTPOW,temp$Bx50.NIBP_MBP)
      BIS.TOTPOW_ <- TOTPOW_[[1]][1:5]%>% data.frame()
      colnames(BIS.TOTPOW_) <- "BIS.TOTPOW"
    } else {
      BIS.TOTPOW <- NA
      BIS.TOTPOW_ <- NA
    }
    # 19. Bx50.ENT_RD_EEG
    if("Bx50.ENT_RD_EEG" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.ENT_RD_EEG)) > 1){
      ENT_RD_EEG <- ccf(temp$Bx50.ENT_RD_EEG,temp$Bx50.NIBP_SBP)
      Bx50.ENT_RD_EEG <- ENT_RD_EEG[[1]][1:5]%>% data.frame()
      colnames(Bx50.ENT_RD_EEG) <- "Bx50.ENT_RD_EEG"
      ENT_RD_EEG_ <- ccf(temp$Bx50.ENT_RD_EEG,temp$Bx50.NIBP_MBP)
      Bx50.ENT_RD_EEG_ <- ENT_RD_EEG_[[1]][1:5]%>% data.frame()
      colnames(Bx50.ENT_RD_EEG_) <- "Bx50.ENT_RD_EEG"
    } else {
      Bx50.ENT_RD_EEG <- NA
      Bx50.ENT_RD_EEG_ <- NA
    }
    # 20. Bx50.ENT_RD_EMG
    if("Bx50.ENT_RD_EMG" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.ENT_RD_EMG)) > 1){
      ENT_RD_EMG <- ccf(temp$Bx50.ENT_RD_EMG,temp$Bx50.NIBP_SBP)
      Bx50.ENT_RD_EMG <- ENT_RD_EMG[[1]][1:5]%>% data.frame()
      colnames(Bx50.ENT_RD_EMG) <- "Bx50.ENT_RD_EMG"
      ENT_RD_EMG_ <- ccf(temp$Bx50.ENT_RD_EMG,temp$Bx50.NIBP_MBP)
      Bx50.ENT_RD_EMG_ <- ENT_RD_EMG_[[1]][1:5]%>% data.frame()
      colnames(Bx50.ENT_RD_EMG_) <- "Bx50.ENT_RD_EMG"
    } else {
      Bx50.ENT_RD_EMG <- NA
      Bx50.ENT_RD_EMG_ <- NA
    }
    # 21. Bx50.ENT_RD_BSR
    if("Bx50.ENT_RD_BSR" %in% colnames(temp) == TRUE & length(unique(temp$Bx50.ENT_RD_BSR)) > 1){
      ENT_RD_BSR <- ccf(temp$Bx50.ENT_RD_BSR,temp$Bx50.NIBP_SBP)
      Bx50.ENT_RD_BSR <- ENT_RD_BSR[[1]][1:5]%>% data.frame()
      colnames(Bx50.ENT_RD_BSR) <- "Bx50.ENT_RD_BSR"
      ENT_RD_BSR_ <- ccf(temp$Bx50.ENT_RD_BSR,temp$Bx50.NIBP_MBP)
      Bx50.ENT_RD_BSR_ <- ENT_RD_BSR_[[1]][1:5]%>% data.frame()
      colnames(Bx50.ENT_RD_BSR_) <- "Bx50.ENT_RD_BSR"
    } else {
      Bx50.ENT_RD_BSR <- NA
      Bx50.ENT_RD_BSR_ <- NA
    }
    # 22. Orchestra.PROPOFOL_CP
    if("Orchestra.PROPOFOL_CP" %in% colnames(temp) == TRUE & length(unique(temp$Orchestra.PROPOFOL_CP)) > 1){
      PROPOFOL_CP <- ccf(temp$Orchestra.PROPOFOL_CP,temp$Bx50.NIBP_SBP)
      Orchestra.PROPOFOL_CP <- PROPOFOL_CP[[1]][1:5]%>% data.frame()
      colnames(Orchestra.PROPOFOL_CP) <- "Orchestra.PROPOFOL_CP"
      PROPOFOL_CP_ <- ccf(temp$Orchestra.PROPOFOL_CP,temp$Bx50.NIBP_MBP)
      Orchestra.PROPOFOL_CP_ <- PROPOFOL_CP_[[1]][1:5]%>% data.frame()
      colnames(Orchestra.PROPOFOL_CP_) <- "Orchestra.PROPOFOL_CP"
    } else {
      Orchestra.PROPOFOL_CP <- NA
      Orchestra.PROPOFOL_CP_ <- NA
    }
    # 23. Orchestra.PROPOFOL_CE
    if("Orchestra.PROPOFOL_CE" %in% colnames(temp) == TRUE & length(unique(temp$Orchestra.PROPOFOL_CE)) > 1){
      PROPOFOL_CE <- ccf(temp$Orchestra.PROPOFOL_CE,temp$Bx50.NIBP_SBP)
      Orchestra.PROPOFOL_CE <- PROPOFOL_CE[[1]][1:5]%>% data.frame()
      colnames(Orchestra.PROPOFOL_CE) <- "Orchestra.PROPOFOL_CE"
      PROPOFOL_CE_ <- ccf(temp$Orchestra.PROPOFOL_CE,temp$Bx50.NIBP_MBP)
      Orchestra.PROPOFOL_CE_ <- PROPOFOL_CE_[[1]][1:5]%>% data.frame()
      colnames(Orchestra.PROPOFOL_CE_) <- "Orchestra.PROPOFOL_CE"
    } else {
      Orchestra.PROPOFOL_CE <- NA
      Orchestra.PROPOFOL_CE_ <- NA
    }
    # 24. Orchestra.PROPOFOL_CT
    if("Orchestra.PROPOFOL_CT" %in% colnames(temp) == TRUE & length(unique(temp$Orchestra.PROPOFOL_CT)) > 1){
      PROPOFOL_CT <- ccf(temp$Orchestra.PROPOFOL_CT,temp$Bx50.NIBP_SBP)
      Orchestra.PROPOFOL_CT <- PROPOFOL_CT[[1]][1:5]%>% data.frame()
      colnames(Orchestra.PROPOFOL_CT) <- "Orchestra.PROPOFOL_CT"
      PROPOFOL_CT_ <- ccf(temp$Orchestra.PROPOFOL_CT,temp$Bx50.NIBP_MBP)
      Orchestra.PROPOFOL_CT_ <- PROPOFOL_CT_[[1]][1:5]%>% data.frame()
      colnames(Orchestra.PROPOFOL_CT_) <- "Orchestra.PROPOFOL_CT"
    } else {
      Orchestra.PROPOFOL_CT <- NA
      Orchestra.PROPOFOL_CT_ <- NA
    }
    # 25. Orchestra.REMIFENTANIL_CP
    if("Orchestra.REMIFENTANIL_CP" %in% colnames(temp) == TRUE & length(unique(temp$Orchestra.REMIFENTANIL_CP)) > 1){
      REMIFENTANIL_CP <- ccf(temp$Orchestra.REMIFENTANIL_CP,temp$Bx50.NIBP_SBP)
      Orchestra.REMIFENTANIL_CP <- REMIFENTANIL_CP[[1]][1:5]%>% data.frame()
      colnames(Orchestra.REMIFENTANIL_CP) <- "Orchestra.REMIFENTANIL_CP"
      REMIFENTANIL_CP_ <- ccf(temp$Orchestra.REMIFENTANIL_CP,temp$Bx50.NIBP_MBP)
      Orchestra.REMIFENTANIL_CP_ <- REMIFENTANIL_CP_[[1]][1:5]%>% data.frame()
      colnames(Orchestra.REMIFENTANIL_CP_) <- "Orchestra.REMIFENTANIL_CP"
    } else {
      Orchestra.REMIFENTANIL_CP <- NA
      Orchestra.REMIFENTANIL_CP_ <- NA
    }
    # 26. Orchestra.REMIFENTANIL_CE
    if("Orchestra.REMIFENTANIL_CE" %in% colnames(temp) == TRUE & length(unique(temp$Orchestra.REMIFENTANIL_CE)) > 1){
      REMIFENTANIL_CE <- ccf(temp$Orchestra.REMIFENTANIL_CE,temp$Bx50.NIBP_SBP)
      Orchestra.REMIFENTANIL_CE <- REMIFENTANIL_CE[[1]][1:5]%>% data.frame()
      colnames(Orchestra.REMIFENTANIL_CE) <- "Orchestra.REMIFENTANIL_CE"
      REMIFENTANIL_CE_ <- ccf(temp$Orchestra.REMIFENTANIL_CE,temp$Bx50.NIBP_MBP)
      Orchestra.REMIFENTANIL_CE_ <- REMIFENTANIL_CE_[[1]][1:5]%>% data.frame()
      colnames(Orchestra.REMIFENTANIL_CE_) <- "Orchestra.REMIFENTANIL_CE"
    } else {
      Orchestra.REMIFENTANIL_CE <- NA
      Orchestra.REMIFENTANIL_CE_ <- NA
    }
    # 27. Orchestra.REMIFENTANIL_CT
    if("Orchestra.REMIFENTANIL_CT" %in% colnames(temp) == TRUE & length(unique(temp$Orchestra.REMIFENTANIL_CT)) > 1){
      REMIFENTANIL_CT <- ccf(temp$Orchestra.REMIFENTANIL_CT,temp$Bx50.NIBP_SBP)
      Orchestra.REMIFENTANIL_CT <- REMIFENTANIL_CT[[1]][1:5]%>% data.frame()
      colnames(Orchestra.REMIFENTANIL_CT) <- "Orchestra.REMIFENTANIL_CT"
      REMIFENTANIL_CT_ <- ccf(temp$Orchestra.REMIFENTANIL_CT,temp$Bx50.NIBP_MBP)
      Orchestra.REMIFENTANIL_CT_ <- REMIFENTANIL_CT_[[1]][1:5]%>% data.frame()
      colnames(Orchestra.REMIFENTANIL_CT_) <- "Orchestra.REMIFENTANIL_CT"
    } else {
      Orchestra.REMIFENTANIL_CT <- NA
      Orchestra.REMIFENTANIL_CT_ <- NA
    }
    # 28. Hypotension
    # if("Hypotension" %in% colnames(temp) == TRUE & length(unique(temp$Hypotension)) > 1){
    #   Hypo <- ccf(temp$Hypotension,temp$Bx50.NIBP_SBP)
    #   Hypotension <- Hypo[[1]][1:5]%>% data.frame()
    #   colnames(Hypotension) <- "Hypotension"
    #   Hypo_ <- ccf(temp$Hypotension,temp$Bx50.NIBP_MBP)
    #   Hypotension_ <- Hypo_[[1]][1:5]%>% data.frame()
    #   colnames(Hypotension_) <- "Hypotension"
    # } else {
    #   Hypotension <- NA
    #   Hypotension_ <- NA
    # }
    
  # 데이터별 SBP와의 lag-correlation값 하나의 프레임으로 합침
  lagcor <- bind_cols(
    Bx50.HR
    ,Bx50.PLETH_SPO2
    ,Bx50.ETCO2
    ,Bx50.CO2
    ,Bx50.AMB_PRES
    ,Bx50.NIBP_SBP
    ,Bx50.NIBP_MBP
    ,Bx50.PLETH
    ,Bx50.RR_VENT
    ,Datex.Ohmeda.TV_EXP
    ,Datex.Ohmeda.MV_EXP
    ,Datex.Ohmeda.PIP
    ,Datex.Ohmeda.COMPLIANCE
    ,BIS.SEF
    ,BIS.SQI
    ,BIS.EMG
    ,BIS.BIS
    ,BIS.TOTPOW
    ,Bx50.ENT_RD_EEG
    ,Bx50.ENT_RD_EMG
    ,Bx50.ENT_RD_BSR
    ,Orchestra.PROPOFOL_CP
    ,Orchestra.PROPOFOL_CE
    ,Orchestra.PROPOFOL_CT
    ,Orchestra.REMIFENTANIL_CP
    ,Orchestra.REMIFENTANIL_CE
    ,Orchestra.REMIFENTANIL_CT
    # ,Hypotension
    ,lag
  )
  colnames(lagcor) <- c("Bx50.HR", "Bx50.PLETH_SPO2", "Bx50.ETCO2", "Bx50.CO2", "Bx50.AMB_PRES", "Bx50.NIBP_SBP", "Bx50.NIBP_MBP", "Bx50.PLETH", "Bx50.RR_VENT", 
                               "Datex.Ohmeda.TV_EXP", "Datex.Ohmeda.MV_EXP", "Datex.Ohmeda.PIP", "Datex.Ohmeda.COMPLIANCE", "BIS.SEF", "BIS.SQI", "BIS.EMG", "BIS.BIS", "BIS.TOTPOW",               
                               "Bx50.ENT_RD_EEG", "Bx50.ENT_RD_EMG", "Bx50.ENT_RD_BSR", "Orchestra.PROPOFOL_CP", "Orchestra.PROPOFOL_CE", "Orchestra.PROPOFOL_CT",    
                               "Orchestra.REMIFENTANIL_CP", "Orchestra.REMIFENTANIL_CE", "Orchestra.REMIFENTANIL_CT", "lag")
  
  # 데이터별 MBP와의 lag-correlation값 하나로 합침
  lagcor_ <- bind_cols(
    Bx50.HR_
    ,Bx50.PLETH_SPO2_
    ,Bx50.ETCO2_
    ,Bx50.CO2_
    ,Bx50.AMB_PRES_
    ,Bx50.NIBP_SBP_
    ,Bx50.NIBP_MBP_
    ,Bx50.PLETH_
    ,Bx50.RR_VENT_
    ,Datex.Ohmeda.TV_EXP_
    ,Datex.Ohmeda.MV_EXP_
    ,Datex.Ohmeda.PIP_
    ,Datex.Ohmeda.COMPLIANCE_
    ,BIS.SEF_
    ,BIS.SQI_
    ,BIS.EMG_
    ,BIS.BIS_
    ,BIS.TOTPOW_
    ,Bx50.ENT_RD_EEG_
    ,Bx50.ENT_RD_EMG_
    ,Bx50.ENT_RD_BSR_
    ,Orchestra.PROPOFOL_CP_
    ,Orchestra.PROPOFOL_CE_
    ,Orchestra.PROPOFOL_CT_
    ,Orchestra.REMIFENTANIL_CP_
    ,Orchestra.REMIFENTANIL_CE_
    ,Orchestra.REMIFENTANIL_CT_
    # ,Hypotension_
    ,lag
  )
  colnames(lagcor_) <- c("Bx50.HR", "Bx50.PLETH_SPO2", "Bx50.ETCO2", "Bx50.CO2", "Bx50.AMB_PRES", "Bx50.NIBP_SBP", "Bx50.NIBP_MBP", "Bx50.PLETH", "Bx50.RR_VENT", 
                               "Datex.Ohmeda.TV_EXP", "Datex.Ohmeda.MV_EXP", "Datex.Ohmeda.PIP", "Datex.Ohmeda.COMPLIANCE", "BIS.SEF", "BIS.SQI", "BIS.EMG", "BIS.BIS", "BIS.TOTPOW",               
                               "Bx50.ENT_RD_EEG", "Bx50.ENT_RD_EMG", "Bx50.ENT_RD_BSR", "Orchestra.PROPOFOL_CP", "Orchestra.PROPOFOL_CE", "Orchestra.PROPOFOL_CT",    
                               "Orchestra.REMIFENTANIL_CP", "Orchestra.REMIFENTANIL_CE", "Orchestra.REMIFENTANIL_CT", "lag")
  
  
  write.csv(lagcor, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\vital_lagcorrelation_SBP\\","lagSBP_",i))
  write.csv(lagcor_, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\vital_lagcorrelation_MBP\\","lagMBP_",i))
  } else {
      next}}



# --- < 2. 변수선택 by.lag-correlation > ---
# 기준: 변수 및 lag별 abs(lag-correlation)값의 평균

# - SBP -
s <- "D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\vital_lagcorrelation_SBP"
s_list <- dir(s, recursive=TRUE)

df <- data.frame()
# 전체 데이터셋 합치기
for (i in s_list){
  temp <- read.csv(paste0(s, "\\", i))
  temp <- temp [,-1]
  print(dim(temp))
  df <- bind_rows(df, temp)
}

# 변수마다 분리해서 저장
for (x in 1:length(df)-1){
  lag <- df[28]
  k <- df[x]
  sbp <- cbind(lag,k)
  write.csv(sbp, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\lagcor_feature_SBP\\",names(df[x]),".csv"))
}

# 변수별로 데이터 합쳐서 변수명 GROUP로 설정하기
kll <- "D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\lagcor_feature_SBP"
kl <- dir(kll, recursive=TRUE)
lagSBP <- data.frame()
for (z in kl){
  temp <- read.csv(paste0(kll,"\\", z))
  tmp <- temp[,2:3] %>% data.frame
  tmp$group <- colnames(temp[3])
  colnames(tmp) <- c("lag","lagcor","group")
  lagSBP <- rbind(lagSBP,tmp)
}

# 평균 lag-correlation값 구하기
lagSBP$group_lag <- paste0(lagSBP$group,"//",lagSBP$lag)
lagSBP$abs_lagcor <- abs(lagSBP$lagcor)
SBP_lagcor_mean <- aggregate(abs_lagcor~group_lag, lagSBP, mean) %>% arrange(desc(abs_lagcor))


# - MBP -
m <- "D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\vital_lagcorrelation_MBP"
m_list <- dir(m, recursive=TRUE)

df <- data.frame()
# 전체 데이터셋 합치기
for (i in m_list){
  temp <- read.csv(paste0(m, "\\", i))
  temp <- temp [,-1]
  df <- bind_rows(df, temp)
}

# 변수마다 분리해서 저장
for (x in 1:length(df)-1){
  lag <- df[28]
  k <- df[x]
  sbp <- cbind(lag,k)
  write.csv(sbp, paste0("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\lagcor_feature_MBP\\",names(df[x]),".csv"))
}

# 변수별로 데이터 합쳐서 변수명 GROUP로 설정하기
kll <- "D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\lagcor_feature_MBP"
kl <- dir(kll, recursive=TRUE)
lagMBP <- data.frame()
for (z in kl){
  temp <- read.csv(paste0(kll,"\\", z))
  tmp <- temp[,2:3] %>% data.frame
  tmp$group <- colnames(temp[3])
  colnames(tmp) <- c("lag","lagcor","group")
  lagMBP <- rbind(lagMBP,tmp)
}

# 평균 lag-correlation값 구하기
lagMBP$group_lag <- paste0(lagMBP$group,"//",lagMBP$lag)
lagMBP$abs_lagcor <- abs(lagMBP$lagcor)
MBP_lagcor_mean <- aggregate(abs_lagcor~group_lag, lagMBP, mean) %>% arrange(desc(abs_lagcor))


# --- < 3. 모델 데이터셋 만들기 > --- 

# - SBP -
#                      group_lag  abs_correlation
# 1               Bx50.NIBP_SBP//-20 0.72953527
# 2               Bx50.NIBP_SBP//-21 0.71771439
# 3               Bx50.NIBP_MBP//-20 0.70728038
# 4               Bx50.NIBP_SBP//-22 0.70589019
# 5               Bx50.NIBP_MBP//-21 0.69726820
# 6               Bx50.NIBP_SBP//-23 0.69409333
# 7               Bx50.NIBP_MBP//-22 0.68724754
# 8               Bx50.NIBP_SBP//-24 0.68228798
# 9               Bx50.NIBP_MBP//-23 0.67726184
# 10              Bx50.NIBP_MBP//-24 0.66728442
# 11  Orchestra.REMIFENTANIL_CT//-20 0.39461060
# 12                 Bx50.ETCO2//-20 0.39210414
# 13  Orchestra.REMIFENTANIL_CT//-21 0.39137298
# 14                 Bx50.ETCO2//-21 0.39015641
# 15  Orchestra.REMIFENTANIL_CT//-22 0.38829403
# 16                 Bx50.ETCO2//-22 0.38814095
# 17                 Bx50.ETCO2//-23 0.38653438
# 18  Orchestra.REMIFENTANIL_CT//-23 0.38526536
# 19                 Bx50.ETCO2//-24 0.38471122
# 20  Orchestra.REMIFENTANIL_CT//-24 0.38221055
# 21  Orchestra.REMIFENTANIL_CE//-20 0.37360040
# 22  Orchestra.REMIFENTANIL_CE//-21 0.37087451
# 23                    Bx50.HR//-24 0.37004348
# 24                    Bx50.HR//-23 0.37001563
# 25                    Bx50.HR//-22 0.36989912
# 26                    Bx50.HR//-21 0.36920309
# 27                    Bx50.HR//-20 0.36837149
# 28  Orchestra.REMIFENTANIL_CE//-22 0.36807734
# 29  Orchestra.REMIFENTANIL_CE//-23 0.36546861
# 30  Orchestra.REMIFENTANIL_CE//-24 0.36296446
# 31  Orchestra.REMIFENTANIL_CP//-20 0.35945868
# 32  Orchestra.REMIFENTANIL_CP//-21 0.35610392
# 33  Orchestra.REMIFENTANIL_CP//-22 0.35331525
# 34  Orchestra.REMIFENTANIL_CP//-23 0.35097856
# 35  Orchestra.REMIFENTANIL_CP//-24 0.34855286
# 36           Datex.Ohmeda.PIP//-20 0.33532831
# 37           Datex.Ohmeda.PIP//-21 0.33497898
# 38           Datex.Ohmeda.PIP//-22 0.33448878
# 39            Bx50.ENT_RD_EMG//-22 0.33443675
# 40            Bx50.ENT_RD_EMG//-21 0.33419603
# 41           Datex.Ohmeda.PIP//-23 0.33378026
# 42            Bx50.ENT_RD_EMG//-23 0.33343535
# 43           Datex.Ohmeda.PIP//-24 0.33310205
# 44            Bx50.ENT_RD_EMG//-20 0.33292496
# 45            Bx50.ENT_RD_EMG//-24 0.33165970
# 46      Orchestra.PROPOFOL_CE//-20 0.32464576
# 47      Orchestra.PROPOFOL_CE//-21 0.32288546
# 48      Orchestra.PROPOFOL_CE//-22 0.32100975
# 49      Orchestra.PROPOFOL_CE//-23 0.31885816
# 50                    BIS.SEF//-20 0.31784007
# 51                    BIS.SEF//-21 0.31701611
# 52      Orchestra.PROPOFOL_CE//-24 0.31659454
# 53                    BIS.SEF//-22 0.31610539
# 54                    BIS.SEF//-23 0.31466900
# 55                    BIS.SEF//-24 0.31337434
# 56                    BIS.BIS//-20 0.31048994
# 57                    BIS.BIS//-21 0.30956735
# 58                    BIS.BIS//-22 0.30852508
# 59                    BIS.BIS//-23 0.30753262
# 60                    BIS.BIS//-24 0.30656346
# 61      Orchestra.PROPOFOL_CT//-20 0.30543503
# 62      Orchestra.PROPOFOL_CT//-21 0.30443603
# 63      Orchestra.PROPOFOL_CT//-22 0.30359079
# 64      Orchestra.PROPOFOL_CT//-23 0.30234318
# 65      Orchestra.PROPOFOL_CT//-24 0.30110421

# 변수 만들기
setwd("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\Data")
d <- getwd()
fls <- dir(d, recursive=TRUE)

data <- data.frame()

## 총 row갯수 알아보기
# da <- 0
# dd <- 0
# for(f in fls){
#   temp <- read.csv(paste0(d,"\\",f))
#   n <- nrow(temp)
#   da <- da+n
#   dd <- dd+1}

for(f in fls){
  temp <- read.csv(paste0(d,"\\",f))
  SBP <- temp$Bx50.NIBP_SBP
  SBP_20 <- lag(SBP,20)
  SBP_21 <- lag(SBP,21)
  SBP_22 <- lag(SBP,22)
  SBP_23 <- lag(SBP,23)
  SBP_24 <- lag(SBP,24)
  MBP <- temp$Bx50.NIBP_MBP
  MBP_20 <- lag(MBP,20)
  MBP_21 <- lag(MBP,21)
  MBP_22 <- lag(MBP,22)
  MBP_23 <- lag(MBP,23)
  MBP_24 <- lag(MBP,24)
  REMIFENTANIL_CT <- temp$Orchestra.REMIFENTANIL_CT
  REMIFENTANIL_CT_20 <- lag(REMIFENTANIL_CT,20)
  REMIFENTANIL_CT_21 <- lag(REMIFENTANIL_CT,21)
  REMIFENTANIL_CT_22 <- lag(REMIFENTANIL_CT,22)
  REMIFENTANIL_CT_23 <- lag(REMIFENTANIL_CT,23)
  REMIFENTANIL_CT_24 <- lag(REMIFENTANIL_CT,24)
  if("Bx50.ETCO2" %in% colnames(temp) == TRUE){
    ETCO2 <- temp$Bx50.ETCO2
    ETCO2_20 <- lag(ETCO2,20)
    ETCO2_21 <- lag(ETCO2,21)
    ETCO2_22 <- lag(ETCO2,22)
    ETCO2_23 <- lag(ETCO2,23)
    ETCO2_24 <- lag(ETCO2,24)
  } else {
    ETCO2 <- temp$Primus.ETCO2
    ETCO2_20 <- lag(ETCO2,20)
    ETCO2_21 <- lag(ETCO2,21)
    ETCO2_22 <- lag(ETCO2,22)
    ETCO2_23 <- lag(ETCO2,23)
    ETCO2_24 <- lag(ETCO2,24)
  }
  REMIFENTANIL_CE <- temp$Orchestra.REMIFENTANIL_CE
  REMIFENTANIL_CE_20 <- lag(REMIFENTANIL_CE,20)
  REMIFENTANIL_CE_21 <- lag(REMIFENTANIL_CE,21)
  REMIFENTANIL_CE_22 <- lag(REMIFENTANIL_CE,22)
  REMIFENTANIL_CE_23 <- lag(REMIFENTANIL_CE,23)
  REMIFENTANIL_CE_24 <- lag(REMIFENTANIL_CE,24)
  BIS <- temp$BIS.BIS
  BIS_20 <- lag(BIS,20)
  BIS_21 <- lag(BIS,21)
  BIS_22 <- lag(BIS,22)
  BIS_23 <- lag(BIS,23)
  BIS_24 <- lag(BIS,24)
  SEF <- temp$BIS.SEF
  SEF_20 <- lag(SEF,20)
  SEF_21 <- lag(SEF,21)
  SEF_22 <- lag(SEF,22)
  SEF_23 <- lag(SEF,23)
  SEF_24 <- lag(SEF,24)
  ENT_RD_EMG <- temp$Bx50.ENT_RD_EMG
  ENT_RD_EMG_20 <- lag(ENT_RD_EMG,20)
  ENT_RD_EMG_21 <- lag(ENT_RD_EMG,21)
  ENT_RD_EMG_22 <- lag(ENT_RD_EMG,22)
  ENT_RD_EMG_23 <- lag(ENT_RD_EMG,23)
  ENT_RD_EMG_24 <- lag(ENT_RD_EMG,24)
  HR <- temp$Bx50.HR
  HR_20 <- lag(HR,20)
  HR_21 <- lag(HR,21)
  HR_22 <- lag(HR,22)
  HR_23 <- lag(HR,23)
  HR_24 <- lag(HR,24)
  if("Datex.Ohmeda.PIP" %in% colnames(temp) == TRUE){
    PIP <- temp$Datex.Ohmeda.PIP
    PIP_20 <- lag(PIP,20)
    PIP_21 <- lag(PIP,21)
    PIP_22 <- lag(PIP,22)
    PIP_23 <- lag(PIP,23)
    PIP_24 <- lag(PIP,24)
  } else {
    PIP <- temp$Primus.PIP_MBAR
    PIP_20 <- lag(PIP,20)
    PIP_21 <- lag(PIP,21)
    PIP_22 <- lag(PIP,22)
    PIP_23 <- lag(PIP,23)
    PIP_24 <- lag(PIP,24)
    }
  PROPOFOL_CE <- temp$Orchestra.PROPOFOL_CE
  PROPOFOL_CE_20 <- lag(PROPOFOL_CE,20)
  PROPOFOL_CE_21 <- lag(PROPOFOL_CE,21)
  PROPOFOL_CE_22 <- lag(PROPOFOL_CE,22)
  PROPOFOL_CE_23 <- lag(PROPOFOL_CE,23)
  PROPOFOL_CE_24 <- lag(PROPOFOL_CE,24)
  PROPOFOL_CT <- temp$Orchestra.PROPOFOL_CT
  PROPOFOL_CT_20 <- lag(PROPOFOL_CT,20)
  PROPOFOL_CT_21 <- lag(PROPOFOL_CT,21)
  PROPOFOL_CT_22 <- lag(PROPOFOL_CT,22)
  PROPOFOL_CT_23 <- lag(PROPOFOL_CT,23)
  PROPOFOL_CT_24 <- lag(PROPOFOL_CT,24)
  REMIFENTANIL_CP <- temp$Orchestra.REMIFENTANIL_CP
  REMIFENTANIL_CP_20 <- lag(REMIFENTANIL_CP,20)
  REMIFENTANIL_CP_21 <- lag(REMIFENTANIL_CP,21)
  REMIFENTANIL_CP_22 <- lag(REMIFENTANIL_CP,22)
  REMIFENTANIL_CP_23 <- lag(REMIFENTANIL_CP,23)
  REMIFENTANIL_CP_24 <- lag(REMIFENTANIL_CP,24)
  tmp <- bind_cols(f,SBP,SBP_20,SBP_21,MBP_20,SBP_22,MBP_21,SBP_23,MBP_22,SBP_24,MBP_23,MBP_24,
                   REMIFENTANIL_CT_20,ETCO2_20,REMIFENTANIL_CT_21,ETCO2_21,REMIFENTANIL_CT_22,ETCO2_22,ETCO2_23,REMIFENTANIL_CT_23,ETCO2_24,REMIFENTANIL_CT_24,
                   REMIFENTANIL_CE_20,REMIFENTANIL_CE_21,HR_24,HR_23,HR_22,HR_21,HR_20,REMIFENTANIL_CE_22,REMIFENTANIL_CE_23,REMIFENTANIL_CE_24,
                   REMIFENTANIL_CP_20,REMIFENTANIL_CP_21,REMIFENTANIL_CP_22,REMIFENTANIL_CP_23,REMIFENTANIL_CP_24,
                   PIP_20,PIP_21,PIP_22,ENT_RD_EMG_22,ENT_RD_EMG_21,PIP_23,ENT_RD_EMG_23,PIP_24,ENT_RD_EMG_20,ENT_RD_EMG_24,
                   PROPOFOL_CE_20,PROPOFOL_CE_21,PROPOFOL_CE_22,PROPOFOL_CE_23,SEF_20,SEF_21,PROPOFOL_CE_24,SEF_22,SEF_23,SEF_24,
                   BIS_20,BIS_21,BIS_22,BIS_23,BIS_24,PROPOFOL_CT_20,PROPOFOL_CT_21,PROPOFOL_CT_22,PROPOFOL_CT_23,PROPOFOL_CT_24
                   )
  data <- rbind(data,tmp)
}
colnames(data) <- c("f","SBP","SBP_20","SBP_21","MBP_20","SBP_22","MBP_21","SBP_23","MBP_22","SBP_24","MBP_23","MBP_24",
                    "REMIFENTANIL_CT_20","ETCO2_20","REMIFENTANIL_CT_21","ETCO2_21","REMIFENTANIL_CT_22","ETCO2_22","ETCO2_23","REMIFENTANIL_CT_23","ETCO2_24","REMIFENTANIL_CT_24",
                    "REMIFENTANIL_CE_20","REMIFENTANIL_CE_21","HR_24","HR_23","HR_22","HR_21","HR_20","REMIFENTANIL_CE_22","REMIFENTANIL_CE_23","REMIFENTANIL_CE_24",
                    "REMIFENTANIL_CP_20","REMIFENTANIL_CP_21","REMIFENTANIL_CP_22","REMIFENTANIL_CP_23","REMIFENTANIL_CP_24",
                    "PIP_20","PIP_21","PIP_22","ENT_RD_EMG_22","ENT_RD_EMG_21","PIP_23","ENT_RD_EMG_23","PIP_24","ENT_RD_EMG_20","ENT_RD_EMG_24",
                    "PROPOFOL_CE_20","PROPOFOL_CE_21","PROPOFOL_CE_22","PROPOFOL_CE_23","SEF_20","SEF_21","PROPOFOL_CE_24","SEF_22","SEF_23","SEF_24",
                    "BIS_20","BIS_21","BIS_22","BIS_23","BIS_24","PROPOFOL_CT_20","PROPOFOL_CT_21","PROPOFOL_CT_22","PROPOFOL_CT_23","PROPOFOL_CT_24") 

# - MBP -
#                     group_lag   abs_correlation
# 1               Bx50.NIBP_MBP//-20 0.73262961
# 2               Bx50.NIBP_MBP//-21 0.72077725
# 3               Bx50.NIBP_MBP//-22 0.70892856
# 4               Bx50.NIBP_MBP//-23 0.69712258
# 5               Bx50.NIBP_MBP//-24 0.68534370
# 6               Bx50.NIBP_SBP//-20 0.66991000
# 7               Bx50.NIBP_SBP//-21 0.65830242
# 8               Bx50.NIBP_SBP//-22 0.64669524
# 9               Bx50.NIBP_SBP//-23 0.63511928
# 10              Bx50.NIBP_SBP//-24 0.62354518
# 11                    Bx50.HR//-22 0.38938173
# 12                    Bx50.HR//-23 0.38922102
# 13                    Bx50.HR//-21 0.38897915
# 14                    Bx50.HR//-24 0.38882138
# 15                    Bx50.HR//-20 0.38872339
# 16                 Bx50.ETCO2//-20 0.37186437
# 17  Orchestra.REMIFENTANIL_CT//-20 0.37119590
# 18                 Bx50.ETCO2//-21 0.36900938
# 19  Orchestra.REMIFENTANIL_CT//-21 0.36823103
# 20                 Bx50.ETCO2//-22 0.36606042
# 21  Orchestra.REMIFENTANIL_CT//-22 0.36534712
# 22                 Bx50.ETCO2//-23 0.36342688
# 23  Orchestra.REMIFENTANIL_CT//-23 0.36264720
# 24                 Bx50.ETCO2//-24 0.36069232
# 25  Orchestra.REMIFENTANIL_CT//-24 0.36011794
# 26  Orchestra.REMIFENTANIL_CE//-20 0.35436366
# 27  Orchestra.REMIFENTANIL_CE//-21 0.35183696
# 28           Datex.Ohmeda.PIP//-20 0.35147559
# 29           Datex.Ohmeda.PIP//-21 0.35109075
# 30           Datex.Ohmeda.PIP//-22 0.35038602
# 31           Datex.Ohmeda.PIP//-23 0.34956016
# 32  Orchestra.REMIFENTANIL_CE//-22 0.34920325
# 33           Datex.Ohmeda.PIP//-24 0.34839315
# 34  Orchestra.REMIFENTANIL_CE//-23 0.34668858
# 35  Orchestra.REMIFENTANIL_CE//-24 0.34424689
# 36  Orchestra.REMIFENTANIL_CP//-20 0.33489705
# 37  Orchestra.REMIFENTANIL_CP//-21 0.33176528
# 38  Orchestra.REMIFENTANIL_CP//-22 0.32881199
# 39  Orchestra.REMIFENTANIL_CP//-23 0.32575254
# 40  Orchestra.REMIFENTANIL_CP//-24 0.32272738
# 41            Bx50.ENT_RD_EMG//-20 0.31959139
# 42                    BIS.SEF//-20 0.31749700
# 43                    BIS.SEF//-21 0.31641425
# 44      Orchestra.PROPOFOL_CE//-20 0.31639933
# 45                    BIS.SEF//-22 0.31526629
# 46      Orchestra.PROPOFOL_CE//-21 0.31496819
# 47            Bx50.ENT_RD_EMG//-21 0.31447111
# 48      Orchestra.PROPOFOL_CE//-22 0.31372479
# 49                    BIS.SEF//-23 0.31364100
# 50      Orchestra.PROPOFOL_CE//-23 0.31244407
# 51                    BIS.SEF//-24 0.31229151
# 52      Orchestra.PROPOFOL_CE//-24 0.31117963
# 53            Bx50.ENT_RD_EMG//-22 0.30916262
# 54            Bx50.ENT_RD_EMG//-23 0.30365263

# 변수 만들기
setwd("D:\\은서\\은서대학교생활\\2021 은서 연구생\\VITAL\\Data")
d <- getwd()
fls <- dir(d, recursive=TRUE)
data <- data.frame()
for(f in fls){
  temp <- read.csv(paste0(d,"\\",f))
  MBP <- temp$Bx50.NIBP_MBP
  MBP_20 <- lag(MBP,20)
  MBP_21 <- lag(MBP,21)
  MBP_22 <- lag(MBP,22)
  MBP_23 <- lag(MBP,23)
  MBP_24 <- lag(MBP,24)
  SBP <- temp$Bx50.NIBP_SBP
  SBP_20 <- lag(SBP,20)
  SBP_21 <- lag(SBP,21)
  SBP_22 <- lag(SBP,22)
  SBP_23 <- lag(SBP,23)
  SBP_24 <- lag(SBP,24)
  SEF <- temp$BIS.SEF
  SEF_20 <- lag(SEF,20)
  SEF_21 <- lag(SEF,21)
  SEF_22 <- lag(SEF,22)
  SEF_23 <- lag(SEF,23)
  SEF_24 <- lag(SEF,24)
  ENT_RD_EMG <- temp$Bx50.ENT_RD_EMG
  ENT_RD_EMG_20 <- lag(ENT_RD_EMG,20)
  ENT_RD_EMG_21 <- lag(ENT_RD_EMG,21)
  ENT_RD_EMG_22 <- lag(ENT_RD_EMG,22)
  ENT_RD_EMG_23 <- lag(ENT_RD_EMG,23)
  if("Bx50.ETCO2" %in% colnames(temp) == TRUE){
    ETCO2 <- temp$Bx50.ETCO2
    ETCO2_20 <- lag(ETCO2,20)
    ETCO2_21 <- lag(ETCO2,21)
    ETCO2_22 <- lag(ETCO2,22)
    ETCO2_23 <- lag(ETCO2,23)
    ETCO2_24 <- lag(ETCO2,24)
  } else {
    ETCO2 <- temp$Primus.ETCO2
    ETCO2_20 <- lag(ETCO2,20)
    ETCO2_21 <- lag(ETCO2,21)
    ETCO2_22 <- lag(ETCO2,22)
    ETCO2_23 <- lag(ETCO2,23)
    ETCO2_24 <- lag(ETCO2,24)
  }
  HR <- temp$Bx50.HR
  HR_20 <- lag(HR,20)
  HR_21 <- lag(HR,21)
  HR_22 <- lag(HR,22)
  HR_23 <- lag(HR,23)
  HR_24 <- lag(HR,24)
  if("Datex.Ohmeda.PIP" %in% colnames(temp) == TRUE){
    PIP <- temp$Datex.Ohmeda.PIP
    PIP_20 <- lag(PIP,20)
    PIP_21 <- lag(PIP,21)
    PIP_22 <- lag(PIP,22)
    PIP_23 <- lag(PIP,23)
    PIP_24 <- lag(PIP,24)
  } else {
    PIP <- temp$Primus.PIP_MBAR
    PIP_20 <- lag(PIP,20)
    PIP_21 <- lag(PIP,21)
    PIP_22 <- lag(PIP,22)
    PIP_23 <- lag(PIP,23)
    PIP_24 <- lag(PIP,24)
  }
  PROPOFOL_CE <- temp$Orchestra.PROPOFOL_CE
  PROPOFOL_CE_20 <- lag(PROPOFOL_CE,20)
  PROPOFOL_CE_21 <- lag(PROPOFOL_CE,21)
  PROPOFOL_CE_22 <- lag(PROPOFOL_CE,22)
  PROPOFOL_CE_23 <- lag(PROPOFOL_CE,23)
  PROPOFOL_CE_24 <- lag(PROPOFOL_CE,24)
  REMIFENTANIL_CP <- temp$Orchestra.REMIFENTANIL_CP
  REMIFENTANIL_CP_20 <- lag(REMIFENTANIL_CP,20)
  REMIFENTANIL_CP_21 <- lag(REMIFENTANIL_CP,21)
  REMIFENTANIL_CP_22 <- lag(REMIFENTANIL_CP,22)
  REMIFENTANIL_CP_23 <- lag(REMIFENTANIL_CP,23)
  REMIFENTANIL_CP_24 <- lag(REMIFENTANIL_CP,24)
  REMIFENTANIL_CE <- temp$Orchestra.REMIFENTANIL_CE
  REMIFENTANIL_CE_20 <- lag(REMIFENTANIL_CE,20)
  REMIFENTANIL_CE_21 <- lag(REMIFENTANIL_CE,21)
  REMIFENTANIL_CE_22 <- lag(REMIFENTANIL_CE,22)
  REMIFENTANIL_CE_23 <- lag(REMIFENTANIL_CE,23)
  REMIFENTANIL_CE_24 <- lag(REMIFENTANIL_CE,24)
  REMIFENTANIL_CT <- temp$Orchestra.REMIFENTANIL_CT
  REMIFENTANIL_CT_20 <- lag(REMIFENTANIL_CT,20)
  REMIFENTANIL_CT_21 <- lag(REMIFENTANIL_CT,21)
  REMIFENTANIL_CT_22 <- lag(REMIFENTANIL_CT,22)
  REMIFENTANIL_CT_23 <- lag(REMIFENTANIL_CT,23)
  REMIFENTANIL_CT_24 <- lag(REMIFENTANIL_CT,24)
  
  tmp <- bind_cols(f,MBP,MBP_20,MBP_21,MBP_22,MBP_23,MBP_24,SBP_20,SBP_21,SBP_22,SBP_23,SBP_24,
                   HR_22,HR_23,HR_21,HR_24,HR_20,
                   ETCO2_20,REMIFENTANIL_CT_20,ETCO2_21,REMIFENTANIL_CT_21,ETCO2_22,REMIFENTANIL_CT_22,ETCO2_23,REMIFENTANIL_CT_23,ETCO2_24,REMIFENTANIL_CT_24,
                   REMIFENTANIL_CE_20,REMIFENTANIL_CE_21,PIP_20,PIP_21,PIP_22,PIP_23,REMIFENTANIL_CE_22,PIP_24,REMIFENTANIL_CE_23,REMIFENTANIL_CE_24,
                   REMIFENTANIL_CP_20,REMIFENTANIL_CP_21,REMIFENTANIL_CP_22,REMIFENTANIL_CP_23,REMIFENTANIL_CP_24,
                   ENT_RD_EMG_20,SEF_20,SEF_21,PROPOFOL_CE_20,SEF_22,PROPOFOL_CE_21,ENT_RD_EMG_21,PROPOFOL_CE_22,SEF_23,PROPOFOL_CE_23,SEF_24,PROPOFOL_CE_24,ENT_RD_EMG_22,ENT_RD_EMG_23)
  data <- rbind(data,tmp)
}
colnames(data) <- c("f","MBP","MBP_20","MBP_21","MBP_22","MBP_23","MBP_24","SBP_20","SBP_21","SBP_22","SBP_23","SBP_24",
                    "HR_22","HR_23","HR_21","HR_24","HR_20",
                    "ETCO2_20","REMIFENTANIL_CT_20","ETCO2_21","REMIFENTANIL_CT_21","ETCO2_22","REMIFENTANIL_CT_22","ETCO2_23","REMIFENTANIL_CT_23","ETCO2_24","REMIFENTANIL_CT_24",
                    "REMIFENTANIL_CE_20","REMIFENTANIL_CE_21","PIP_20","PIP_21","PIP_22","PIP_23","REMIFENTANIL_CE_22","PIP_24","REMIFENTANIL_CE_23","REMIFENTANIL_CE_24",
                    "REMIFENTANIL_CP_20","REMIFENTANIL_CP_21","REMIFENTANIL_CP_22","REMIFENTANIL_CP_23","REMIFENTANIL_CP_24",
                    "ENT_RD_EMG_20","SEF_20","SEF_21","PROPOFOL_CE_20","SEF_22","PROPOFOL_CE_21","ENT_RD_EMG_21","PROPOFOL_CE_22","SEF_23","PROPOFOL_CE_23","SEF_24","PROPOFOL_CE_24","ENT_RD_EMG_22","ENT_RD_EMG_23") 


# --- < 4. SVR > ---
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
m <- svm(SBP ~ ., data=train, kernel="linear")
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
# RMSE = 12.28603(ENT_RD_EMG들제외)
RMSE <- rmse(test$SBP, pred)
# MAPE = 0.04289952(ENT_RD_EMG들제외)
MAPE <- Metrics::mape(test$SBP, pred)

### 저혈압 예측일때 (SBP 65이하일 때)
result_sbp <- cbind(test$SBP, pred) %>% data.frame()
result_sbp <- result_sbp %>% filter(V1 <= 90) 
# MAPE = 0.1894446
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
m <- svm(MBP ~ ., data=train, kernel="linear")
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
# RMSE = 10.13937(ENT_RD_EMG들제외)
RMSE <- rmse(test$MBP, pred)
# MAPE = 0.04164303(ENT_RD_EMG들제외)
MAPE <- Metrics::mape(test$MBP, pred)

### 저혈압 예측일때 (SBP 65이하일 때)
result_mbp <- cbind(test$MBP, pred) %>% data.frame()
result_mbp <- result_mbp %>% filter(V1 <= 65) 
# MAPE = 0.03397946
MAPE <- Metrics::mape(result_mbp$V1, result_mbp$pred)








# -SBP-
# 7:3 round(nrow(data) * 0.7) = 34203
index_train <- 1:34203
index_test <- 34204:48861

train <- data[index_train,]
test <- data[index_test,]
min(test$SBP)

## 모델생성
m <- svm(SBP ~ ., data=train)
summary(m)

## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)

plot(x=test$SBP, y=pred, xlim=c(70,205), ylim=c(70,205), main="SVR_lagcor_SBP(7:3)")
max(test$SBP)
max(pred)
min(test$SBP)
min(pred)

# 14.47322 - 비율 0.3까지(ENT_RD_EMG들제외)
# 12.64757 - 35번까지
# 7.1588 - 전체 (SBP_20+SBP_21+MBP_20+SBP_22+MBP_21+SBP_23+MBP_22+SBP_24+MBP_23+MBP_24)
# 7.1486 - MBP_24 제외 (SBP_20+SBP_21+MBP_20+SBP_22+MBP_21+SBP_23+MBP_22+SBP_24+MBP_23)
# 7.139176 - MBP_23, MBP_24 제외 (SBP_20+SBP_21+MBP_20+SBP_22+MBP_21+SBP_23+MBP_22+SBP_24)
# 7.142258 - SBP_24, MBP_23, MBP_24 제외 (SBP_20+SBP_21+MBP_20+SBP_22+MBP_21+SBP_23+MBP_22)
# 7.130813 - MBP_22, SBP_24, MBP_23, MBP_24 제외 (SBP_20+SBP_21+MBP_20+SBP_22+MBP_21+SBP_23)
# 7.136851 - SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외 (SBP_20+SBP_21+MBP_20+SBP_22+MBP_21)
# 7.112914 - MBP_21, SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외 (SBP_20+SBP_21+MBP_20+SBP_22)
# 7.122866 - SBP_22, MBP_21, SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외 (SBP_20+SBP_21+MBP_20)
# 7.073358 - MBP_20, SBP_22, MBP_21, SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외 (SBP_20+SBP_21)
# 7.079394 - SBP_21, MBP_20, SBP_22, MBP_21, SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외 (SBP_20)
RMSE <- rmse(test$SBP, pred)

# 0.07836176 - 비율 0.3 까지(ENT_RD_EMG들제외)
# 0.05164708 - 35번까지
# 0.02607336 - 전체
# 0.02612336 - MBP_24 제외
# 0.02620407 - MBP_23, MBP_24 제외
# 0.02614687 - SBP_24, MBP_23, MBP_24 제외 
# 0.0262579 - MBP_22, SBP_24, MBP_23, MBP_24 제외 
# 0.0264136 - SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외
# 0.02648691 - MBP_21, SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외
# 0.02651872 - SBP_22, MBP_21, SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외
# 0.02750179 - MBP_20, SBP_22, MBP_21, SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외
# 0.02757897 - SBP_21, MBP_20, SBP_22, MBP_21, SBP_23, MBP_22, SBP_24, MBP_23, MBP_24 제외
MAPE <- Metrics::mape(test$SBP, pred)


# 8:2 round(nrow(data) * 0.8) = 39089
index_train <- 1:39089
index_test <- 39090:48861
train <- data[index_train,]
test <- data[index_test,]


## 모델생성
m <- svm(SBP ~ ., data=train)
summary(m)

## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)

plot(x=test$SBP, y=pred, xlim=c(75,205), ylim=c(75,205), main="SVR_lagcor_SBP(8:2)")
max(test$SBP)
max(pred)
min(test$SBP)
min(pred)
# RMSE = 14.00221(ENT_RD_EMG들제외)
# 11.9967 - 35번까지
RMSE <- rmse(test$SBP, pred)
# MAPE = 0.06994905(ENT_RD_EMG들제외)
# 0.04555177 - 35번까지
MAPE <- Metrics::mape(test$SBP, pred)


# 9:1 round(nrow(data) * 0.9) = 43975
index_train <- 1:43975
index_test <- 43976:48861
train <- data[index_train,]
test <- data[index_test,]


## 모델생성
m <- svm(SBP ~ ., data=train)
summary(m)

## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)

plot(x=test$SBP, y=pred, xlim=c(75,205), ylim=c(75,205), main="SVR_lagcor_SBP(9:1)")
max(test$SBP)
max(pred)
min(test$SBP)
min(pred)
# 14.77588
# RMSE = 14.99958 - 35번까지
RMSE <- rmse(test$SBP, pred)
# 0.06839824
# MAPE = 0.05619822 - 35번까지
MAPE <- Metrics::mape(test$SBP, pred)

#-MBP-
# 7:3 round(nrow(data) * 0.7) = 34203
index_train <- 1:34203
index_test <- 34204:48861

train <- data[index_train,]
test <- data[index_test,]


## 모델생성
m <- svm(MBP ~ ., data=train)
summary(m)

## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)

plot(x=test$MBP, y=pred, xlim=c(55,170), ylim=c(55,170), main="SVR_lagcor_MBP(7:3)")
max(test$MBP)
max(pred)
min(test$MBP)
min(pred)

# RMSE = 5.732114(이전실험)
# 10.48081 - 0.3
# 9.381731 - 0.35
RMSE <- rmse(test$MBP, pred)
# MAPE = 0.02714988(이전실험)
# 0.07481577 - 0.3
# 0.05349616 - 0.35
MAPE <- Metrics::mape(test$MBP, pred)


# 8:2 round(nrow(data) * 0.8) = 39089
index_train <- 1:39089
index_test <- 39090:48861
train <- data[index_train,]
test <- data[index_test,]


## 모델생성
# 효과없었음 - tune.svm(factor(MBP)~MBP_20+MBP_21+MBP_22+MBP_23+MBP_24+SBP_20+SBP_21+SBP_22+SBP_23+SBP_24, data=train, gamma=2^(-1:1), cost=2:4)
m <- svm(MBP ~ ., data=train, kernel = linear)
summary(m)

## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)

plot(x=test$MBP, y=pred, xlim=c(50,170), ylim=c(50,170), main="SVR_lagcor_MBP(8:2)")
max(test$MBP)
max(pred)
min(test$MBP)
min(pred)

# RMSE = 3.100443 (이전실험)
# 10.15346 - 0.3
# 10.1774 - 0.35
RMSE <- rmse(test$MBP, pred)

# MAPE = 0.02005618 (이전실험)
# 0.0652975 - 0.3
# 0.04955419 - 0.35
MAPE <- Metrics::mape(test$MBP, pred)


# 9:1 round(nrow(data) * 0.9) = 43975
index_train <- 1:43975
index_test <- 43976:48861
train <- data[index_train,]
test <- data[index_test,]


## 모델생성
m <- svm(MBP ~ ., data=train)
summary(m)

## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)

plot(x=test$MBP, y=pred, xlim=c(55,160), ylim=c(55,160), main="SVR_lagcor_MBP(9:1)")
max(test$MBP)
max(pred)
min(test$MBP)
min(pred)

# RMSE = 3.489243
# 11.35324 - 0.3
# 13.62557 - 0.35
RMSE <- rmse(test$MBP, pred)
# MAPE = 0.02264685
# 0.06445823 - 0.3
# 0.06051575 - 0.35
MAPE <- Metrics::mape(test$MBP, pred)







### +) 유의미한 시점 찾기 ###

# - SBP -
# 9:1 round(nrow(data) * 0.9) = 10399 **
index_train <- 1:10399
index_test <- 10400:11554
train <- data[index_train,]
test <- data[index_test,]


## 모델생성
m <- svm(SBP ~ SBP_20, data=train)
summary(m)

## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)

RMSE <- rmse(test$SBP, pred)
MAPE <- Metrics::mape(test$SBP, pred)

# 결과
# 1) SBP_20
# > RMSE
# [1] 3.920558
# > MAPE
# [1] 0.01936255
# 2) SBP_21
# > RMSE
# [1] 4.047933
# > MAPE
# [1] 0.01989297
# 3) SBP_22
# > RMSE
# [1] 4.170206
# > MAPE
# [1] 0.0203703
# 4) SBP_23
# > RMSE
# [1] 4.281714
# > MAPE
# [1] 0.0208054
# 5) SBP_24
# > RMSE
# [1] 4.397466
# > MAPE
# [1] 0.02126118

# - MBP -
# 8:2 round(nrow(data_MBP) * 0.8) = 9243
index_train <- 1:9243
index_test <- 9244:11554
train <- data[index_train,]
test <- data[index_test,]

## 모델생성
m <- svm(MBP ~ MBP_24, data=train)
summary(m)

## 모델 평가
pred <- predict(m, test)
test <- na.omit(test)

RMSE <- rmse(test$MBP, pred)
MAPE <- Metrics::mape(test$MBP, pred)

# 결과
# 1) MBP_20 
# > RMSE
# [1] 3.066966
# > MAPE
# [1] 0.02115823
# 2) MBP_21
# > RMSE
# [1] 3.156076
# > MAPE
# [1] 0.02166457
# 3) MBP_22
# > RMSE
# [1] 3.222744
# > MAPE
# [1] 0.02185227
# 4) MBP_23
# > RMSE
# [1] 3.32497
# > MAPE
# [1] 0.02269024
# 5) MBP_24 
# > RMSE
# [1] 3.412702
# > MAPE
# [1] 0.02316298
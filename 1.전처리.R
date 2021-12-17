####-----------------------------------------------1. 패키지 + 사용자 함수 + DB연결-----------------------------------------------####


setwd("./Github/Aline100_Time_series_forecasting") # 상대경로로 지정(공유 이후 사용 가능하는게 목적)

# 1. 패키지
# library(RMySQL)
library(tidyverse)
library(data.table)
library(foreach) # Loop와 lapply 함수를 섞어 만든 패키지
# %do% 순차적으로 실시
# %dopar% 동시실행(병렬 처리시 사용)
library(doSNOW) # 병렬 처리하는 클러스터 내에서 백엔드 역할
library(doParallel) # loop를 사용한 병렬 처리 시
library(parallel) # 대부분 상황의 병렬 처리 시
library(progress)

library(tidyverse)
library(stringr)
library(zoo)
library(data.table)
library(pracma)
library(changepoint)
library(seewave)
library(wavScalogram)
library(e1071)

library(signal)
library(ggplot2)
library(gridExtra)
library(forecast)
library(xgboost)
library(RWeka)
library(class)
library(randomForest)
library(gmodels)

library(ROSE)
library(caret)
library(MLmetrics)

# 신경망 구축용
library(keras)
# install_keras(method='conda', tensorflow='gpu')
# install_tensorflow(method = 'conda', envname = 'r-reticulate')
library(reticulate)
use_condaenv('r-reticulate')
library(tensorflow)
# tf$debugging$set_log_device_placement(TRUE)
# install_tensorflow(version = "gpu")

# 2. 사용자 함수
# makeDBtable <- function(start,limitlength){
#   DB_part <- dbGetQuery(jusoDB,
#                         paste0("SELECT * FROM vr1m_21 LIMIT ",start,",",limitlength,";"))
#   vr1m_21 <<-rbind(vr1m_21,DB_part)
# 
#   print(paste0(start,"에서 ",limitlength, "행만큼 불러옴."))
# }

DataListOnlyCsv <- function(datalocation){
  datalist <- dir(datalocation,recursive = T)
  datalist <- datalist[str_detect(datalist,"\\.csv")] # csv파일만 불러오기
  return(datalist)
}

ReadDataFromDataLocation <- function(datalist){
  
  for(i in 1:length(datalist)){
    
    cat(paste0(
      round(i*100/length(datalist),digits = 3),
      "%"),"\r") # 퍼센트 체크
    
    assign(datalist[i],
           fread(paste0(datalocation,"/",datalist[i]),colClasses = ),envir = parent.frame())
  }
}

CountNaNumberOfData <- function(datalist){
  
  dataInformation <- data.frame()
  
  for(i in 1:length(datalist)){
    get(datalist[i]) %>% is.na() %>% as.data.frame() %>% sapply(sum) %>% t() %>% as.data.frame() -> one_data_info
    per_data_info <- round(one_data_info[,-1]/nrow(get(datalist[i])),digits=5)
    
    colnames(per_data_info) <- paste0("NA_percentage_of_",colnames(per_data_info))
    colnames(one_data_info) <- paste0("NA_number_of_",colnames(one_data_info))
    
    one_data_info <- cbind(one_data_info,per_data_info)
    
    one_data_info <- cbind(Case_ID = str_split(datalist[i],"\\.csv")[[1]][1],
                           one_data_info)
    
    dataInformation <- rbind(dataInformation,one_data_info)
  }
  
  return(dataInformation)
}

CountTimeNumber <- function(ts_data){
  
  test_data <- ts_data$Time %>% table() %>% as.data.frame
  colnames(test_data) <- c("Time","count")
  test_data$Time %>% str_split("\\.",simplify = TRUE) %>% .[,1] -> test_data$Time
  test_data %>% group_by(Time) %>% summarize(count=sum(count)) -> summarize_test_data
  
  return(summarize_test_data)
  
}

My_na.approx <- function(data){
  
  NotNaStartLocation <- which(!is.na(data))
  
  if(NotNaStartLocation[length(NotNaStartLocation)] != length(data)){
    data[1:NotNaStartLocation[length(NotNaStartLocation)]] <- na.approx(data[1:NotNaStartLocation[length(NotNaStartLocation)]])
    data[(NotNaStartLocation[length(NotNaStartLocation)]+1):length(data)] <- data[NotNaStartLocation[length(NotNaStartLocation)]]
    
    return(data)
  }else{
    return(na.approx(data))
  }
}

PlotForNaCheck <- function(NaDataFrame,FileName){
  
  CheckName <- NaDataFrame$Case_ID
  
  ColumnName <- str_split(FileName,"_")[[1]][1]
  
  pdf(paste0("./Pictures/",str_split(ColumnName,"/")[[1]][2],"_NA_Data/",str_split(FileName,"/")[[1]][2],".pdf"),width = 14.4, height = 10.4)
  
  for(x in CheckName){
    DataForPlot <- get(paste0(x,".csv"))
    
    NotNaStartLocation <- which(!is.na(select(DataForPlot,ColumnName)[[1]]))[1]
    
    Plot1 <- ggplot(data = DataForPlot, aes(x = 1:nrow(DataForPlot),y = get(ColumnName)))+
      geom_line()+theme_bw()+
      labs(title = paste(ColumnName," Plot by",x),
           x = "Index (100Hz)",
           y = ColumnName)
    
    Plot2 <- ggplot(data = DataForPlot[NotNaStartLocation:(NotNaStartLocation+100),], aes(x = 1:101,y = get(ColumnName)))+
      geom_line()+theme_bw()+
      labs(title = paste(ColumnName, " Plot by ",x,"100 data"),
           x = "Index (100Hz)",
           y = ColumnName)
    
    Plot3 <- ggplot(data = DataForPlot[NotNaStartLocation:(NotNaStartLocation+100),], aes(x = 1:101,y = My_na.approx(get(ColumnName))))+
      geom_line()+theme_bw()+
      labs(title = paste(ColumnName," Plot by",x,"100 data with na.approx"),
           x = "Index (100Hz)",
           y = ColumnName)
    
    grid.arrange(Plot2,Plot3,Plot1,layout_matrix = rbind(c(1,2), c(3,3)))
  }
  
  dev.off()
  
}

skewness<-function(x){
  (sum((x - mean(x))^3)/length(x))/((sum((x - mean(x))^2)/length(x)))^(3/2)
}

rss<-function(x,na.rm=F){
  if(na.rm==T) x<-na.omit(x)
  rms(x)*length(x)
}

asff<-function(d,colname){
  gc()
  x<-data.frame(d)
  colnames(x)<-colname
  gc()
  return(x)
}

# IBP와 PLETH에 대한 feature생성 코드를 각자 다르게 하자(10/12)
IBP_feature <- function(x){
  p <-findpeaks((-1)*x,minpeakheight=-65) # minpeakheight : 그 값 이하로는 안나옴
  p[,1]<- (-1)*p[,1]
  cf<-crest((-1)*x,100, plot=FALSE)
  
  p2 <-findpeaks((-1)*x,minpeakheight=-50) 
  p2[,1]<- (-1)*p2[,1]
  
  cm <-cpt.mean(x)
  cpt1<-cpts(cm)
  cv <-cpt.var(x)
  cpt2<-cpts(cv)
  cmv <-cpt.meanvar(x)
  cpt3<-cpts(cmv)
  
  r.spec<-spectrum(x,span=10,plot=FALSE)
  d.spec<-data.frame(r.freq=r.spec$freq/0.01, r.spec=r.spec$spec*2)
  d.spec<-arrange(d.spec,desc(r.spec))
  
  r.scal<-scalogram(signal = x,dt=0.01,makefigure=FALSE)
  d.scal<-data.frame(r.scal$scalog,r.scal$scales)
  d.scal<-arrange(d.scal,desc(r.scal.scalog))
  
  
  r.scal2<-scalogram(signal = x,dt=0.01,makefigure=FALSE, wname="HAAR")
  d.scal2<-data.frame(r.scal2$scalog,r.scal2$scales)
  d.scal2<-arrange(d.scal2,desc(r.scal2.scalog))
  
  # 추가(10/12)
  cof <- lm(x~c(1:length(x)))$coefficients["c(1:length(x))"]
  min_dfx <- min(diff(x))
  
  rslt<-c(mean(x),min(x),max(x),sd(x),skewness(x),rms(x),rss(x),IQR(x),kurtosis(x), 
          ifelse(!is.null(p), dim(p)[1],0), 
          ifelse(!is.null(p),ifelse(dim(p)[1]>2, mean(diff(p[,2])),0),0),
          ifelse(!is.null(p),ifelse(dim(p)[1]>2, std(diff(p[,2])),0), 0),
          ifelse(!is.null(p),ifelse(dim(p)[1]>2, min(diff(p[,2])),0),0),
          ifelse(!is.null(p),ifelse(dim(p)[1]>2, max(p[,2]),0),0),
          
          
          ifelse(!is.null(p),mean(p[,1]),0),
          ifelse(!is.null(p),max(p[,1]),0),
          ifelse(!is.null(p),min(p[,1]),0),
          ifelse(!is.null(p),ifelse(dim(p)[1]>2,std(p[,1]),0),0),
          ifelse(!is.null(p2), dim(p2)[1],0), 
          ifelse(!is.null(p2),ifelse(dim(p2)[1]>2, mean(diff(p2[,2])),0),0),
          ifelse(!is.null(p2),ifelse(dim(p2)[1]>2, std(diff(p2[,2])),0), 0),
          
          ifelse(!is.null(p2),ifelse(dim(p2)[1]>2, min(diff(p2[,2])),0),0),
          ifelse(!is.null(p2),ifelse(dim(p2)[1]>2, max(p2[,2]),0),0),
          
          
          ifelse(!is.null(p2),mean(p2[,1]),0),
          ifelse(!is.null(p2),max(p2[,1]),0),
          ifelse(!is.null(p2),min(p2[,1]),0),
          ifelse(!is.null(p2),ifelse(dim(p)[1]>2,std(p2[,1]),0),0),
          cf$C,
          length(cpt1),length(cpt2),length(cpt3), 
          d.spec[1:3,1], d.spec[1:3,2], 
          length(which(d.scal$r.scal.scalog>=0.1)),d.scal[1:10,2],
          length(which(d.scal2$r.scal2.scalog>=0.7)),d.scal2[1:10,2],
          cof,min_dfx) 
  return(rslt)
  
}

PLETH_feature <- function(x){
  p <-findpeaks((-1)*x,minpeakheight=0.15) # minpeakheight : 그 값 이하로는 안나옴
  p[,1]<- (-1)*p[,1]
  cf<-crest(x,100, plot=FALSE) 
  
  p2 <-findpeaks((-1)*x,minpeakheight=0.3) 
  p2[,1]<- (-1)*p2[,1]
  
  cm <-cpt.mean(x)
  cpt1<-cpts(cm)
  cv <-cpt.var(x)
  cpt2<-cpts(cv)
  cmv <-cpt.meanvar(x)
  cpt3<-cpts(cmv)
  
  r.spec<-spectrum(x,span=10,plot=FALSE)
  d.spec<-data.frame(r.freq=r.spec$freq/0.01, r.spec=r.spec$spec*2)
  d.spec<-arrange(d.spec,desc(r.spec))
  
  r.scal<-scalogram(signal = x,dt=0.01,makefigure=FALSE)
  d.scal<-data.frame(r.scal$scalog,r.scal$scales)
  d.scal<-arrange(d.scal,desc(r.scal.scalog))
  
  
  r.scal2<-scalogram(signal = x,dt=0.01,makefigure=FALSE, wname="HAAR")
  d.scal2<-data.frame(r.scal2$scalog,r.scal2$scales)
  d.scal2<-arrange(d.scal2,desc(r.scal2.scalog))
  
  cof <- lm(x~c(1:length(x)))$coefficients["c(1:length(x))"]
  min_dfx <- min(diff(x))
  
  
  rslt<-c(mean(x),min(x),max(x),sd(x),skewness(x),rms(x),rss(x),IQR(x),kurtosis(x), 
          ifelse(!is.null(p), dim(p)[1],0), 
          ifelse(!is.null(p),ifelse(dim(p)[1]>2, mean(diff(p[,2])),0),0),
          ifelse(!is.null(p),ifelse(dim(p)[1]>2, std(diff(p[,2])),0), 0),
          ifelse(!is.null(p),ifelse(dim(p)[1]>2, min(diff(p[,2])),0),0),
          ifelse(!is.null(p),ifelse(dim(p)[1]>2, max(p[,2]),0),0),
          
          
          ifelse(!is.null(p),mean(p[,1]),0),
          ifelse(!is.null(p),max(p[,1]),0),
          ifelse(!is.null(p),min(p[,1]),0),
          ifelse(!is.null(p),ifelse(dim(p)[1]>2,std(p[,1]),0),0),
          ifelse(!is.null(p2), dim(p2)[1],0), 
          ifelse(!is.null(p2),ifelse(dim(p2)[1]>2, mean(diff(p2[,2])),0),0),
          ifelse(!is.null(p2),ifelse(dim(p2)[1]>2, std(diff(p2[,2])),0), 0),
          
          ifelse(!is.null(p2),ifelse(dim(p2)[1]>2, min(diff(p2[,2])),0),0),
          ifelse(!is.null(p2),ifelse(dim(p2)[1]>2, max(p2[,2]),0),0),
          
          
          ifelse(!is.null(p2),mean(p2[,1]),0),
          ifelse(!is.null(p2),max(p2[,1]),0),
          ifelse(!is.null(p2),min(p2[,1]),0),
          ifelse(!is.null(p2),ifelse(dim(p)[1]>2,std(p2[,1]),0),0),
          cf$C,
          length(cpt1),length(cpt2),length(cpt3), 
          d.spec[1:3,1], d.spec[1:3,2], 
          length(which(d.scal$r.scal.scalog>=0.1)),d.scal[1:10,2],
          length(which(d.scal2$r.scal2.scalog>=0.7)),d.scal2[1:10,2],
          cof,min_dfx) 
  return(rslt)
  
}

ma <- function(x, n = 5){
  stats::filter(x, rep(1 / n, n), sides = 2,na.rm=T)
}

splitWithOverlap <- function(vec, seg.length, overlap) {
  starts = seq(1, length(vec), by=seg.length-overlap)
  ends   = starts + seg.length - 1
  ends[ends > length(vec)] = length(vec)
  
  lapply(1:length(starts), function(i) vec[starts[i]:ends[i]])
}

make_rslt <- function(td1,td2){
  td <- data.frame()
  
  for(i in 1:length(td1)){
    val1 <- unlist(td1[i])
    val2 <- unlist(td2[i])
    
    if(all(is.na(val1))|all(is.nan(val1))){}
    else if(length(val1)>=SRATE*(20+(1+MINUTES_AHEAD)*60)-1){
      segx <- val1[1:(SRATE*20)]
      segy <- val1[(1+SRATE*(20+MINUTES_AHEAD*60)):(1+SRATE*(20+(1+MINUTES_AHEAD)*60)-1)]
      segxd <- val1[1:(SRATE*(20+MINUTES_AHEAD*60))]
      
      if (all(is.na(segx))| all(is.na(segxd))|all(is.na(segy))){
        
      }else if (is.na(mean(segx,na.rm=T)) | 
                is.na(mean(segy,na.rm=T))|max(segx,na.rm=T)>200 | min(segx,na.rm=T)<20 |
                max(segy,na.rm=T)>200 | max(segy,na.rm=T)<20 |
                max(segx,na.rm=T) - min(segx,na.rm=T) < 30 |
                max(segy,na.rm=T) - min(segy,na.rm=T) < 30 |
                max(rollmean(segx, 2*SRATE,na.rm=T),na.rm=T) <= 65 |
                # max(ma(segxd, 2*SRATE), na.rm=T) <= 65 |
                any(abs(diff(segx,na.rm=T))>30,na.rm = T) |
                any(abs(diff(segy,na.rm=T))>30,na.rm=T) ){   
      } else { 
        segy <- rollmean(segy, 2*SRATE,na.rm=T)
        event <- ifelse(max(segy, na.rm=T) <= 65, 1, ifelse(min(segy,na.rm=T)>65,0,2))
        segx_p <- val2[1:(SRATE*20)]
        
        if(event!=2){
          ft1<-IBP_feature(segx)
          ft2 <- PLETH_feature(segx_p)
          
          td <- rbind(td,as.data.frame(t(c(ft1,ft2,event))))
        }
      }
    }
  }
  return(td)
}

MakeFeatureDataFrame <- function(datalist){
  percentage <- 0
  total <- length(datalist)
  
  for(j in datalist){
    percentage <- percentage+1
    f <- get(j)
    
    if(!all(is.na(f$`Bx50/IBP1`))){
      ts_IBP <- My_na.approx(f$`Bx50/IBP1`)
      ts_PLETH <- My_na.approx(f$`Bx50/PLETH`)
      
      if(all(is.na(ts_IBP))==FALSE){
        td_IBP <- splitWithOverlap(ts_IBP, SRATE*(20+(1+MINUTES_AHEAD)*60), 1*SRATE)
        td_PLETH <- splitWithOverlap(ts_PLETH,SRATE*(20+(1+MINUTES_AHEAD)*60), 1*SRATE)
      }
      
      if(!is.null(td_IBP)) {
        # rslt<-sapply(td_IBP,event)
        rslt <- make_rslt(td_IBP,td_PLETH)
        
        if(!is.null(rslt)){
          if(nrow(rslt)>0){
            
            rslt<-asff(rslt, c(paste0("IBP_",
                                      c("mean","min","max","sd","skewness","rms","rss","IQR","kurtosis",
                                        "f_n","p_interval","p_interval_std","p_interval_min","p_interval_recency","p_mean","p_max","p_min","p_std",
                                        "p2","p2_interval","p2_interval_std","p2_interval_min","p2_interval_recency","p2_mean","p2_max","p2_min","p2_std",
                                        "cf",
                                        "cp1","cp2","cp3",
                                        "freq1","freq2","freq3","po1","po2","po3",
                                        "scalog20",paste0("scale",1:10),"scalog220",paste0("scale2",1:10),
                                        "coefficient",
                                        "diff_Top_1")),
                               paste0("PLETH_",
                                      c("mean","min","max","sd","skewness","rms","rss","IQR","kurtosis",
                                        "f_n","p_interval","p_interval_std","p_interval_min","p_interval_recency","p_mean","p_max","p_min","p_std",
                                        "p2","p2_interval","p2_interval_std","p2_interval_min","p2_interval_recency","p2_mean","p2_max","p2_min","p2_std",
                                        "cf",
                                        "cp1","cp2","cp3",
                                        "freq1","freq2","freq3","po1","po2","po3",
                                        "scalog20",paste0("scale",1:10),"scalog220",paste0("scale2",1:10),
                                        "cofficient",
                                        "diff_Top_1")),
                               "class"))
            
            return(rslt)}}}}
  }
}

na_replace <- function(x){
  ifelse(is.na(x),0,x)
}
# # 3. DB연결 준비
# jusoDB <- dbConnect(
#   MySQL(),
#   user='dbuser',
#   password='database1234!@',
#   host='114.71.219.39',
#   dbname="bucheonvr"
# )
# 
# dbSendQuery(jusoDB,'set character set "utf8"')
# 
# # 4. DB연결 해제
# dbDisconnect(jusoDB)


####----------------------------------------------------2. DB(vr1m_21)불러오기----------------------------------------------------####


datalocation <-"H:/Dropbox/Dataset/bucheon_vr100/bcDB_2103_2106_100"

datalist <- DataListOnlyCsv(datalocation)
datalist %>% length() # 3519

# ReadDataFromDataLocation(datalist)


####------------------------------------------------------3. 분할 처리 적용-------------------------------------------------------####


## IBP의 NA개수 확인
cores <- detectCores() - 1
DBCluster <- makeCluster(spec = cores)
registerDoSNOW(cl = DBCluster)

pb <- txtProgressBar(max = length(datalist), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# %do% 순차적으로 실시
# %dopar% 동시실행(병렬 처리시 사용)
NotNAIBPlist <- foreach(i=1:length(datalist), .combine = rbind,
                        .packages=c("tidyverse","data.table","zoo","stringr","pracma","changepoint","seewave","wavScalogram","e1071"),
                        .options.snow=opts) %dopar%{
                          
                          ReadDataFromDataLocation(datalist[i])
                          get(datalist[i]) %>% is.na() %>% as.data.frame() %>% sapply(sum) %>% t() %>% as.data.frame() -> one_data_info
                          one_data_info <- round(one_data_info[,-1]/nrow(get(datalist[i])),digits=5)
                          colnames(one_data_info) <- paste0("NA_percentage_of_",colnames(one_data_info))
                          
                          rm(list=datalist[i])
                          
                          if (one_data_info$`NA_percentage_of_Bx50/IBP1`!=1){
                            cbind(datalist[i],one_data_info$`NA_percentage_of_Bx50/IBP1`)
                          }
                        }

stopCluster(DBCluster)
NotNAIBPlist %>% nrow() # 3519개 중 447개만 IBP가 존재 (12.7%)
NotNAIBPdataframe <- data.frame(Case_ID=NotNAIBPlist[,1],
                                Percentage_of_NA=as.numeric(NotNAIBPlist[,2]))
rm(NotNAIBPlist)
NotNAIBPdataframe[order(NotNAIBPdataframe$Percentage_of_NA,decreasing = T),] %>% dplyr::filter(Percentage_of_NA<=0.7) -> NotNAIBPdataframe #그 중 NA 비율이 70% 이하인 데이터들은 435개 존재 (12.3%)
NotNAIBPdataframe # 전처리 기준이 IBP인데 이거 어케하지용..
# write.csv(NotNAIBPdataframe,file = "bucheonvr_NotNAList.csv",row.names = F)


## 전처리
NotNAIBPdataframe <- read.csv("H:/Dropbox/연구실세미나/이건/저혈압예측(0927)/bucheonvr_NotNAList.csv")
cores <- detectCores() - 1
DBCluster <- makeCluster(spec = cores)
registerDoSNOW(cl = DBCluster)

pb <- txtProgressBar(max = nrow(NotNAIBPdataframe), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# %do% 순차적으로 실시
# %dopar% 동시실행(병렬 처리시 사용)
final_data <- foreach(i=1:nrow(NotNAIBPdataframe), .combine = rbind,
                      .packages=c("tidyverse","data.table","zoo","stringr","pracma","changepoint","seewave","wavScalogram","e1071"),
                      .options.snow=opts) %dopar%{
                        
                        ReadDataFromDataLocation(NotNAIBPdataframe[i,1])
                        SRATE <- 100 # 100Hz
                        MINUTES_AHEAD <- 5 # 5분 후 데이터 예측
                        one_prepro_data <- MakeFeatureDataFrame(NotNAIBPdataframe[i,1])
                        rm(list=NotNAIBPdataframe[i,1])
                        one_prepro_data
                      }
stopCluster(DBCluster)
final_data$class %>% table()

## 전처리 데이터 저장
# write.csv(final_data,file = "bucheonvr_final.csv",row.names = F)
# write.csv(final_data, file = "bucheonvr_final2.csv",row.names = F)

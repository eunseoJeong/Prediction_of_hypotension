####-------------------------------6.AI blog - Time Series Forecasting with Recurrent Neural Networks-----------------------------####


make_rslt <- function(td1,td2){
  td_list <- list()
  result <- data.frame()
  
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
          seg_res <- data.frame(segx,segx_p)
          td_list <- c(td_list,list(as.matrix(seg_res)))
          result <- rbind(result,event)
        }
      }
    }
  }
  if (nrow(result)!=0){
    colnames(result) <- "result"
    return(list(td_list,result))
  }
}

ReadDataFromDataLocation <- function(datalist){
  
  for(i in 1:length(datalist)){
    assign(datalist[i],
           fread(paste0(datalocation,"/",datalist[i]),colClasses = ),envir = parent.frame())
  }
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

splitWithOverlap <- function(vec, seg.length, overlap) {
  starts = seq(1, length(vec), by=seg.length-overlap)
  ends   = starts + seg.length - 1
  ends[ends > length(vec)] = length(vec)
  
  lapply(1:length(starts), function(i) vec[starts[i]:ends[i]])
}


# https://blogs.rstudio.com/ai/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/
datalocation <-"H:/Dropbox/Dataset/bucheon_vr100/bcDB_2103_2106_100"
NotNAIBPdataframe <- read.csv("H:/Dropbox/연구실세미나/이건/저혈압예측(0927)/bucheonvr_NotNAList.csv")
NotNAIBPdataframe # IBP만 사용할 예정

SRATE <- 100
MINUTES_AHEAD <- 5

## 전처리
final_data <- list()
final_result <- data.frame()
count <- 0

for(i in 1:nrow(NotNAIBPdataframe)){
  
  ReadDataFromDataLocation(NotNAIBPdataframe[i,1])
  f <- get(NotNAIBPdataframe[i,1])
  ts_IBP <- My_na.approx(f$`Bx50/IBP1`)
  ts_PLETH <- My_na.approx(f$`Bx50/PLETH`)
  
  if(all(is.na(ts_IBP))==FALSE){
    td_IBP <- splitWithOverlap(ts_IBP, SRATE*(20+(1+MINUTES_AHEAD)*60), 1*SRATE)
    td_PLETH <- splitWithOverlap(ts_PLETH, SRATE*(20+(1+MINUTES_AHEAD)*60), 1*SRATE)
  }
  
  final_data <- c(final_data,make_rslt(td_IBP,td_PLETH)[[1]])
  final_result <- rbind(final_result,make_rslt(td_IBP,td_PLETH)[[2]])
  
  rm(list=NotNAIBPdataframe[i,1])
  count <- count+1
  cat(paste0(round(count/nrow(NotNAIBPdataframe),digits = 3)*100,"%\r"))
}

# save.image("H:/Dropbox/GitHub_RData/Aline100_Time_series_forecasting/AIblog(1130).RData")
load("H:/Dropbox/GitHub_RData/Aline100_Time_series_forecasting/AIblog(1130).RData")


####-----------------------------------------------------7. apply keras models----------------------------------------------------####

set.seed(12345)
train_num <- createDataPartition(final_result$result, p = 0.7, list = FALSE)

train_data <- final_data[train_num]
test_data <- final_data[-train_num]

train_array <- array(0, dim = c(length(train_data), dim(train_data[[1]])))
test_array <- array(0, dim = c(length(test_data), dim(test_data[[1]])))

for(i in 1:length(train_data)){
  train_array[i,,] <- train_data[[i]]
}

for(i in 1:length(test_data)){
  test_array[i,,] <- test_data[[i]]
}

train_result <- final_result[train_num,]
test_result <- final_result[-train_num,]

model <- keras_model_sequential() %>%
  layer_gru(units = 32, input_shape = dim(train_array)[-1]) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "adam", # 최적화기 : adam
  loss = "binary_crossentropy", # 손실함수 : binary_crossentropy(교차 엔트로피)
  metrics = c("accuracy") # 훈련 도중 정확도 관측
)

history <- model %>% fit(
  train_array,
  train_result,
  epochs = 50,
)

model %>% predict(test_array) %>% table()
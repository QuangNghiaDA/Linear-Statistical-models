#LE THI NGOC THOM-19110458
#TRAN QUANG NGHIA - 19110392
#LE THI QUYNH NHI-19110401
#######


####### Cau 3
setwd("C:/Users/ADMIN/OneDrive - VNU-HCMUS/NAM_4/HK1/LinearModel")
data3<-read.csv("data3.csv",header=TRUE)
attach(data3)
dim(data3) #DATA co 177 dong va 15 cot( 15 bien)
str(data3) 
head(data3)


# Ham loai bo outlier
find_outliers_iqr <- function(data,x) {
  # Tinh IQR
  q25 <- quantile(x)[2]
  q75 <- quantile(x)[4]
  iqr = q75 - q25 
  # Tinh cca gia tri Upper/Lower xac dinh cac outliers
  upper = q75 + iqr * 1.5
  lower = q25 - iqr * 1.5
  # xac dinh outliers
  ouliers_index <- which(x > upper | x < lower)
  data_new_iqr<-data[-ouliers_index, ]
  # thong bao cac outliers 
  if (length(ouliers_index) > 0 ) {
    message(paste("Number of outliers:", length(ouliers_index)))
    message(paste("Number of Non-outliers:", length(x)-length(ouliers_index)))
    # return the data with the outliers removed
    return(data[-ouliers_index,])
  } else {
    message("Not outliers")
  }
}

#kiem tra du lieu khuyet
f=function(x){any(is.na(x))}
apply(data3,2,f)


#loai bien khong lien quan
data3=data3[,-c(1, 7, 8, 10)]
dim(data3)# so chieu la 526 va 20
str(data3)


# Tom tat model
summary(lm(lwage~., data=data3))


#ma tran he so tuong quan
library(corrplot)
corrplot(cor(data3[sapply(data3,is.numeric)]),method="number",order="hclust")


#ngoai lai
str(data3)
boxplot(data3)
dim(data3)
data3=data3[, -c(19, 20)]
str(data3)

#loai outlier 
data3<-find_outliers_iqr(data3,data3$educ)
data3<-find_outliers_iqr(data3,data3$tenure)
data3<-find_outliers_iqr(data3,data3$lwage)

#vif 
car::vif(lm(lwage~.,data=data3),data=data3)


MASS::stepAIC(lm(lwage~.,data=data3),k=log(length(data3)))

model3<-lm(formula = lwage ~ educ + exper + tenure + female + smsa + 
             west + trade + services + profocc + servocc, data = data3)
summary(model3)

###############
#ve 4 do thi de quan sat tinh chuan, psai...
par(mfrow=c(2,2))
plot(model3)

#kiem tra bang test
#kiem tra mean(resid)
mean(model3$resid) #bang 0

#kiem tra phuong sai khong doi
car::ncvTest(model3)
#H0: psai la hang so, khong doi
#ket qua cho thay pvalue>0.05 chap nhan Ho
#vay psai mo hinh nay khong doi 

#kiem tra tinh chuan cho dataset
shapiro.test(model3$residuals)
# H0: resid co pp chuan
#ket qua: resid nay co pp chuan

#kiem dinh su doc lap giua cac exilon i,j
car::durbinWatsonTest(model3)
#H0: he so tuong quan bang 0 i.e la cac exilon i doc lap
#ket qua co p value >0.05
#khong du co so bac bo H0, nghia la cac exilon i doc lap






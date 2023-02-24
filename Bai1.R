#LE THI NGOC THOM-19110458
#TRAN QUANG NGHIA - 19110392
#LE THI QUYNH NHI-19110401
#######


####### Cau 1

setwd("C:/Users/ADMIN/OneDrive - VNU-HCMUS/NAM_4/HK1/LinearModel")
data1<-read.csv("data1.csv",header=TRUE)
attach(data1)
dim(data1) #DATA co 177 dong va 15 cot( 15 bien)
str(data1) 
head(data1)


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
apply(data1,2,f)


#loai bien khong lien quan
data1=data1[,-c(1, 2, 7, 9)]
dim(data1)# so chieu la 177 va 11
str(data1)


# tom tat model
summary(lm(lsalary~., data=data1))

#do thi
pairs(data1)


#ngoai lai
boxplot(data1$lsalary)
boxplot(data1)

#loai outlier 
data1<-find_outliers_iqr(data1,data1$lsalary)
data1<-find_outliers_iqr(data1,data1$profits)
data1<-find_outliers_iqr(data1,data1$profmarg)
data1<-find_outliers_iqr(data1,data1$comtensq)

#vif 
car::vif(lm(lsalary~.,data=data1),data=data1)

#loai bien comten, comtensq
str(data1)
data1=data1[,-c(3, 9)]
str(data1)
dim(data1)
# con 142 quan sat va 9 bien

library(leaps)
corrplot(cor(data1[sapply(data1,is.numeric)]),method="number",order="hclust")
boxplot(data1)

### loai profits va ceotensq
data1=data1[,-c(4, 8)]
str(data1)


# Dung tieu chuan BIC de chon mo hinh phu hop nhat
MASS::stepAIC(lm(lsalary~.,data=data1),k=log(length(data1)))

## Model
model1<-lm(lsalary ~ college + ceoten + lmktval + profmarg, 
data = data1)
summary(model1)

###############
#ve 4 do thi de quan sat tinh chuan, psai...
par(mfrow=c(2,2))
plot(model1)

#kiem tra bang test
#kiem tra mean(resid)
mean(model1$resid) #bang 0
#kiem tra phuong sai khong doi
car::ncvTest(model1)

#H0: psai la hang so, khong doi
#ket qua cho thay pvalue>0.05 nen khong bac bo Ho

#kiem tra tinh chuan cho dataset
shapiro.test(model1$residuals)
# H0: resid co pp chuan
#ket qua: resid nay co pp chuan

#kiem dinh su doc lap giua cac epsilon i,j
car::durbinWatsonTest(model1)
#H0: he so tuong quan bang 0 i.e la cac epxilon i doc lap
#ket qua co p value =0.598>0.05
#khong du co so bac bo H0, nghia la cac epxilon i doc lap






#LE THI NGOC THOM-19110458
#TRAN QUANG NGHIA - 19110392
#LE THI QUYNH NHI-19110401
#######
####### Cau 2
setwd("C:/Users/ADMIN/OneDrive - VNU-HCMUS/NAM_4/HK1/LinearModel")
data2 <- read.csv("data2.csv", header = TRUE) 
dim(data2) #DATA co 21597 dong va 21 cot( 21 bien)
str(data2)
head(data2)

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
apply(data2,2,f)


#loai bien khong lien quan
data2=data2[,-c(1, 2, 6, 10, 18, 19)]
dim(data2) #DATA co 21597 dong va 15 cot( 15 bien)
str(data2)


#cac thong ke cua du lieu
summary(lm(price~.,data=data2))


#ma tran he so tuong quan
library(corrplot)
corrplot(cor(data2[sapply(data2,is.numeric)]),method="number",order="hclust")

#ngoai lai
boxplot(data2)

# Loai bo outliers
data2<-find_outliers_iqr(data2,data2$price)
data2<-find_outliers_iqr(data2,data2$sqft_lot)
data2<-find_outliers_iqr(data2,data2$sqft_lot15)

# Kiem tra lai
boxplot(data2)

#vif 
model2<-lm(price~.,data=data2)
car::vif(model2,data=data2)

# Bo sqft_above, sqft_lot15
str(data2)
data2=data2[, -c(9, 15)]
str(data2)


# Tieu chuan BIC
MASS::stepAIC(lm(price~.,data=data2),k=log(length(data2)))

model2<-lm(formula = price ~ bathrooms + sqft_lot + floors + waterfront + 
             condition + grade + sqft_basement + yr_built + yr_renovated + 
             zipcode + sqft_living15, data = data2)

summary(model2)


###############
#ve 4 do thi de quan sat tinh chuan, psai...
par(mfrow=c(2,2))
plot(model2)

#kiem tra bang test
#kiem tra mean(resid)
mean(model2$resid) #bang 0


#kiem tra phuong sai khong doi
car::ncvTest(model2)
#H0: psai la hang so, khong doi
#ket qua cho thay pvalue<0.05 bac bo Ho
#vay psai mo hinh nay thay doi cu the o phan cuoi normal QQ bi lech



#kiem dinh su doc lap giua cac exilon i,j
car::durbinWatsonTest(model2)
#H0: he so tuong quan bang 0 i.e la cac exilon i doc lap
#ket qua co p value =0.896>0.05
#khong du co so bac bo H0, nghia la cac exilon i doc lap



######## Khac phuc phuong sai thay doi bang mo hinh log log
lprice<-log(data2$price)
lbathrooms<-log(data2$bathrooms)
lsqft_lot<-log(data2$sqft_lot)
lfloors<-log(data2$floors)
lcondition<-log(data2$condition)
lgrade<-log(data2$grade)
lyr_built<-log(data2$yr_built)
lsqft_living15<-log(data2$sqft_living15)


model2.1<-lm(lprice ~ lbathrooms + lsqft_lot + data2$floors + data2$waterfront + 
             lcondition + lgrade + data2$sqft_basement + lyr_built + data2$yr_renovated + 
             data2$zipcode + lsqft_living15)
summary(model2.1)

## Tieu chuan BIC
MASS::stepAIC(model2.1,k=log(length(data2)))

model2.2<-lm(formula = lprice ~ lbathrooms + lsqft_lot + data2$floors + 
     data2$waterfront + lcondition + lgrade + data2$sqft_basement + 
     lyr_built + data2$zipcode + lsqft_living15)
summary(model2.2)

#ve 4 do thi de quan sat tinh chuan, psai...
par(mfrow=c(2,2))
plot(model2.2)

#kiem tra bang test
#kiem tra mean(resid)
mean(model2.2$resid) #bang 0


#kiem tra phuong sai khong doi
car::ncvTest(model2.2)
#H0: psai la hang so, khong doi
#ket qua cho thay pvalue<0.05 bac bo Ho


#kiem dinh su doc lap giua cac exilon i,j
car::durbinWatsonTest(model2.2)
#H0: he so tuong quan bang 0 i.e la cac exilon i doc lap
#ket qua co p value>0.05
#khong du co so bac bo H0, nghia la cac exilon i doc lap


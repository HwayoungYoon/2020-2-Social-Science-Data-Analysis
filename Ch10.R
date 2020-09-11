setwd("D:/Work/Class/Survey Data Analysis/사회과학 통계분석/실습")
### 데이터 불러들이기
load("spssdata.RData")

### 데이터 만들기
myvar <- c("q33a01w1", "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1",
           "q33a06w1", "q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1"  )
PCA1   <- spssdata[myvar]
# names(spssdata)
# PCA1 <- spssdata[,c(9:18)]
PCA1_1 <- na.omit(PCA1) #결측값 제거

#################### 요인분석 #####################
### Factor의 수 결정 : 주성분분석
fit1  <- princomp(PCA1_1, cor=TRUE)
summary(fit1)
plot(fit1, type="lines") #요인분석의 고유값 도표
loadings(fit1) #요인들의 적재량
#sum(loadings(fit1)[,1]^2)

### Factor에 따른 변수결정
######### OPTION 1 #########
fit <- factanal(PCA1_1, 2, rotation="varimax") 
#요인수 2개의 요인분석 결과를 varimax 방법을 사용하여 회전
print(fit, digits=3, sort=TRUE) #입력된 문항 순서대로 출력: sort=TRUE 옵션
#summary(fit)
load <- fit$loadings[,1:2]
plot(load) #도표출력
text(load, labels=names(PCA1_1), cex=0.7)

######### OPTION 2 ########
install.packages("psych")
library(psych)
fit1_1 <- principal(PCA1_1, nfactors=2, rotate="varimax")
fit1_1

#################### 신뢰도분석 ####################
# 데이터 쪼개기
myvar1a <- c("q33a01w1", "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1", "q33a06w1")
myvar1b <- c("q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1")
# myvar1a <- myvar[1:6] ; myvar1b <- myvar[7:10]
PCA1a <- spssdata[myvar1a]
PCA1b <- spssdata[myvar1b]

###신뢰도분석
library(psych)
alpha(PCA1a, na.rm=TRUE)
alpha(PCA1b, na.rm=TRUE)

###########################################################################
# sjPlot 패키지를 이용한 요인분석과 신뢰도분석
install.packages("sjPlot")
library(sjPlot)
tab_pca(PCA1, title="부모에 대한 애착", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 비율")


##############################
## 새로운 변수 생성
attach(spssdata)
spssdata$gattach <- q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
spssdata$outatt  <- q33a07w1+q33a08w1+q33a09w1+q33a10w1
detach(spssdata)



###########################################################################
# sjPlot 패키지를 이용한 요인분석과 신뢰도분석
library(Hmisc)
test <- spss.get("D:/Work/Class/Survey Data Analysis/사회과학 통계분석/실습/[중2패널] 1차년도_6차년도 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
                 use.value.labels=FALSE)
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, 
                  select=c("q48a01w1","q48a02w1","q48a03w1","q48a04w1","q48a05w1","q48a06w1",
                           "q48a07w1","q48a08w1","q48a09w1","q48a10w1","q48a11w1","q48a12w1"))
library(sjPlot)
tab_pca(newdata, title="자기만족도", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 비율")

############################################################################

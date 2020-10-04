# 작업공간 설정
setwd("C:/R/Social Science Data Analysis")

# 데이터 불러들이기
load("spssdata.RData")

# 변수 확인
names(spssdata)

################################################################################

# 33번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar <- c("q33a01w1", "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1",
           "q33a06w1", "q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1"  )
PCA1 <- spssdata[myvar]
#names(spssdata)
#PCA1 <- spssdata[,c(9:18)]

# 결측값 제거 : 결측값이 하나라도 있는 행 전체 제거
PCA1_1 <- na.omit(PCA1)

################################### 요인분석 ###################################

# Factor의 수 결정 : 주성분분석
## cor=TRUE : correlation matrix 사용(FALSE : covariance matrix 사용)
fit1  <- princomp(PCA1_1, cor=TRUE)
# 각 요인의 표준편차(Standard deviation), 분산의 비율(Proportion of Variance), 누적 비율(Cumulative Proportion) 출력
# 고유값 : 표준편차의 제곱, 1이상이면 유의미
summary(fit1)

# 요인분석의 고유값 도표
plot(fit1, type="lines")

# 요인들의 적재량 : 요인 회전 이전의 적재량
loadings(fit1)
#sum(loadings(fit1)[,1]^2)

# Factor에 따른 변수결정

######### OPTION 1 #########
# 2개의 요인에 대한 변수 결정
## rotation="" : 회전 방법 지정
fit <- factanal(PCA1_1, 2, rotation="varimax")

# Uniquenesses : 선택된 요인으로 설명되지 않는 각 문항에서의 분산값
# SS loadings : 요인별 적재량의 제곱합, 회전 후의 고유값
# Chi squre statistic : 요인의 수에 대한 가설 검정(표본의 크기가 커야함)
# -> 귀무가설 : 요인분석을 통해 추출한 요인이 문항 사이의 관계를 정확히 설명한다
## digits= : 소숫점 자릿수 지정
## sort=TRUE : 입력된 문항 순서대로 출력
print(fit, digits=3, sort=TRUE)

#summary(fit)
load <- fit$loadings[,1:2]
# 도표출력
plot(load)
# 각 점에 대한 라벨 붙이기
text(load, labels=names(PCA1_1), cex=0.7)

######### OPTION 2 #########
install.packages("psych")
library(psych)
# 주성분 분석(요인분석)
# h2 : 추출된 요인으로 설명되는 해당 변수의 분산
# u2 : 설명되지 않은 분산
fit1_1 <- principal(PCA1_1, nfactors=2, rotate="varimax")
fit1_1

################################## 신뢰도분석 ##################################

# 데이터 쪼개기
myvar1a <- c("q33a01w1", "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1", "q33a06w1")
myvar1b <- c("q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1")
#myvar1a <- myvar[1:6] ; myvar1b <- myvar[7:10]
PCA1a <- spssdata[myvar1a]
PCA1b <- spssdata[myvar1b]

# 신뢰도분석
## na.rm=TRUE : 결측값 제거
# raw_alpha : Cronbach's alpha 값
# std.alpha : 표준화된 Cronbach's alpha 값
library(psych)
alpha(PCA1a, na.rm=TRUE)
alpha(PCA1b, na.rm=TRUE)

################################################################################

# sjPlot 패키지 install 및 load
install.packages("sjPlot")
library(sjPlot)

# sjPlot 패키지를 이용한 요인분석과 신뢰도분석
## wrap.labels= : 변수값 설명에 쓰일 글자 수
## show.cronb=TRUE : Cronbach's alpha 값 출력
## show.var=TRUE : 각 요인의 분산 비율과 누적 분산 비율 출력
tab_pca(PCA1, title="부모에 대한 애착", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 분산 비율")

################################################################################

# 새로운 변수 생성
attach(spssdata)
spssdata$gattach <- q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
spssdata$outatt  <- q33a07w1+q33a08w1+q33a09w1+q33a10w1
detach(spssdata)

################################################################################

# sjPlot 패키지를 이용한 요인분석과 신뢰도분석
library(Hmisc)
test <- spss.get("C:/R/Social Science Data Analysis/[중2패널] 1차년도_6차년도 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
                 use.value.labels=FALSE)
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, 
                  select=c("q48a01w1","q48a02w1","q48a03w1","q48a04w1","q48a05w1","q48a06w1",
                           "q48a07w1","q48a08w1","q48a09w1","q48a10w1","q48a11w1","q48a12w1"))

library(sjPlot)
tab_pca(newdata, title="자기만족도", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 비율")

# 데이터 쪼개기
myvar1a_1 <- c("q48a01w1","q48a02w1","q48a03w1")
myvar1b_1 <- c("q48a04w1","q48a05w1","q48a06w1")

PCA1a_1 <- newdata[myvar1a_1]
PCA1b_1 <- newdata[myvar1b_1]

# 신뢰도 분석
library(psych)
alpha(PCA1a_1, na.rm=TRUE)
alpha(PCA1b_1, na.rm=TRUE)

# 역방향 코딩
attach(newdata)
newdata$rq48a04w1[q48a04w1==1] <- 5
newdata$rq48a04w1[q48a04w1==2] <- 4
newdata$rq48a04w1[q48a04w1==3] <- 3
newdata$rq48a04w1[q48a04w1==4] <- 2
newdata$rq48a04w1[q48a04w1==5] <- 1

newdata$rq48a05w1[q48a05w1==1] <- 5
newdata$rq48a05w1[q48a05w1==2] <- 4
newdata$rq48a05w1[q48a05w1==3] <- 3
newdata$rq48a05w1[q48a05w1==4] <- 2
newdata$rq48a05w1[q48a05w1==5] <- 1

newdata$rq48a06w1[q48a06w1==1] <- 5
newdata$rq48a06w1[q48a06w1==2] <- 4
newdata$rq48a06w1[q48a06w1==3] <- 3
newdata$rq48a06w1[q48a06w1==4] <- 2
newdata$rq48a06w1[q48a06w1==5] <- 1

# 새로운 데이터셋 생성
myvar <- c("q48a01w1", "q48a02w1", "q48a03w1" , "rq48a04w1", "rq48a05w1", "rq48a06w1")
PCA1aa <- newdata[myvar]

# 신뢰도 분석
alpha(PCA1aa, na.rm=TRUE)

################################################################################

# 데이터 저장하기
save(spssdata, file="spssdata.RData")
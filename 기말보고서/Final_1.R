# 작업공간 설정
setwd("C:/R/Social Science Data Analysis")

# SPSS 파일 불러오기 위한 패키지 install 및 load
library(Hmisc)

# 데이터 불러오기
## use.value.labels=TRUE : factor 형태의 데이터(분석 어려움)
raw_data <- spss.get("기말보고서/(HRC) 1-1. 학업중단 청소년의 삶과 의식에 대한 종단조사_1차년도 데이터_776명(180326).sav",
                 use.value.labels=FALSE)
# 저장된 변수명 확인
names(raw_data)

################################################################################

# 변수 선택 - 숫자형 벡터 만들기
select_variables <- c(1,2,14,20,73:82,87:93,263,280,297,314,
                      339:394,401:403,497:514,530)

# 선택된 변수만 가지는 새로운 데이터셋 select_raw_data 생성
select_raw_data <- raw_data[,select_variables]

################################################################################

# sjmisc, sjlabelled 패키지 install 및 load
library(sjmisc)
library(sjlabelled)

# label을 가진 중2패널 데이터 불러오기
test.labels <- spss.get("기말보고서/(HRC) 1-1. 학업중단 청소년의 삶과 의식에 대한 종단조사_1차년도 데이터_776명(180326).sav",
                        use.value.labels=TRUE)

# 선택된 변수만 가지는 새로운 데이터셋 test.lavels1 생성
test.labels1 <- test.labels[select_variables]

# 라벨로 저장
labels.spss.values <- get_labels(test.labels1)
# 관측값과 라벨 일치시킴(각 관측값들이 라벨 가짐)
mydata <- set_labels(select_raw_data, labels=labels.spss.values, force.values=FALSE, force.labels=TRUE)

# 데이터 저장하기
save(mydata, file="mydata.RData")

#########################################################

# 결측값 확인
attach(mydata)
table(SEX, useNA="ifany")
table(T1.Q4.1, useNA="ifany")
table(T1.Q6, useNA="ifany")
table(T1.Q16.1, useNA="ifany")
table(T1.Q16.2, useNA="ifany")
table(T1.Q16.3, useNA="ifany")
table(T1.Q16.4, useNA="ifany")
table(T1.Q16.5, useNA="ifany")
table(T1.Q16.6, useNA="ifany")
table(T1.Q16.7, useNA="ifany")
table(T1.Q16.8, useNA="ifany")
table(T1.Q16.9, useNA="ifany")
table(T1.Q16.10, useNA="ifany")
table(T1.Q18.1, useNA="ifany")
table(T1.Q18.2, useNA="ifany")
table(T1.Q18.3, useNA="ifany")
table(T1.Q18.4, useNA="ifany")
table(T1.Q18.5, useNA="ifany")
table(T1.Q18.6, useNA="ifany")
table(T1.Q18.7, useNA="ifany")
table(T1.Q20.1.99M, useNA="ifany")
table(T1.Q20.2.99M, useNA="ifany")
table(T1.Q20.3.99M, useNA="ifany")
table(T1.Q20.4.99M, useNA="ifany")
table(T1.Q27.1, useNA="ifany")
table(T1.Q27.2, useNA="ifany")
table(T1.Q27.3, useNA="ifany")
table(T1.Q27.4, useNA="ifany")
table(T1.Q27.5, useNA="ifany")
table(T1.Q27.6, useNA="ifany")
table(T1.Q28.1, useNA="ifany")
table(T1.Q28.2, useNA="ifany")
table(T1.Q28.3, useNA="ifany")
table(T1.Q28.4, useNA="ifany")
table(T1.Q28.5, useNA="ifany")
table(T1.Q28.6, useNA="ifany")
table(T1.Q28.7, useNA="ifany")
table(T1.Q28.8, useNA="ifany")
table(T1.Q28.9, useNA="ifany")
table(T1.Q28.10, useNA="ifany")
table(T1.Q29.1, useNA="ifany")
table(T1.Q29.2, useNA="ifany")
table(T1.Q29.3, useNA="ifany")
table(T1.Q29.4, useNA="ifany")
table(T1.Q29.5, useNA="ifany")
table(T1.Q29.6, useNA="ifany")
table(T1.Q29.7, useNA="ifany")
table(T1.Q29.8, useNA="ifany")
table(T1.Q29.9, useNA="ifany")
table(T1.Q29.10, useNA="ifany")
table(T1.Q29.11, useNA="ifany")
table(T1.Q29.12, useNA="ifany")
table(T1.Q29.13, useNA="ifany")
table(T1.Q29.14, useNA="ifany")
table(T1.Q29.15, useNA="ifany")
table(T1.Q30.1, useNA="ifany")
table(T1.Q30.2, useNA="ifany")
table(T1.Q30.3, useNA="ifany")
table(T1.Q30.4, useNA="ifany")
table(T1.Q30.5, useNA="ifany")
table(T1.Q30.6, useNA="ifany")
table(T1.Q30.7, useNA="ifany")
table(T1.Q30.8, useNA="ifany")
table(T1.Q31.1, useNA="ifany")
table(T1.Q31.2, useNA="ifany")
table(T1.Q31.3, useNA="ifany")
table(T1.Q31.4, useNA="ifany")
table(T1.Q31.5, useNA="ifany")
table(T1.Q31.6, useNA="ifany")
table(T1.Q31.7, useNA="ifany")
table(T1.Q31.8, useNA="ifany")
table(T1.Q32.1, useNA="ifany")
table(T1.Q32.2, useNA="ifany")
table(T1.Q32.3, useNA="ifany")
table(T1.Q32.4, useNA="ifany")
table(T1.Q32.5, useNA="ifany")
table(T1.Q32.6, useNA="ifany")
table(T1.Q32.7, useNA="ifany")
table(T1.Q32.8, useNA="ifany")
table(T1.Q32.9, useNA="ifany")
table(T1.Q34.4, useNA="ifany")
table(T1.Q34.5, useNA="ifany")
table(T1.Q34.6, useNA="ifany")

table(T1.Q48, useNA="ifany")

################################################################################

# 역방향 코딩 : 응답의 방향성을 맞추기 위해
attach(mydata)
mydata$T1.Q27.4[T1.Q27.4==1] <- 4
mydata$T1.Q27.4[T1.Q27.4==2] <- 3
mydata$T1.Q27.4[T1.Q27.4==3] <- 2
mydata$T1.Q27.4[T1.Q27.4==4] <- 1

mydata$T1.Q27.6[T1.Q27.6==1] <- 4
mydata$T1.Q27.6[T1.Q27.6==2] <- 3
mydata$T1.Q27.6[T1.Q27.6==3] <- 2
mydata$T1.Q27.6[T1.Q27.6==4] <- 1

mydata$T1.Q32.1[T1.Q32.1==1] <- 4
mydata$T1.Q32.1[T1.Q32.1==2] <- 3
mydata$T1.Q32.1[T1.Q32.1==3] <- 2
mydata$T1.Q32.1[T1.Q32.1==4] <- 1

# 결측값 대체
mydata[is.na(mydata)] <- 1
attach(mydata)

# 재부호화
mydata$T1.Q20.1.99M[T1.Q20.1.99M==99] <- 0
mydata$T1.Q20.2.99M[T1.Q20.2.99M==99] <- 0
mydata$T1.Q20.3.99M[T1.Q20.3.99M==99] <- 0
mydata$T1.Q20.4.99M[T1.Q20.4.99M==99] <- 0

mydata$T1.Q42.1.1[T1.Q42.1.1==1] <- 0
mydata$T1.Q42.1.1[T1.Q42.1.1==2] <- 1
mydata$T1.Q42.1.2[T1.Q42.1.2==1] <- 0
mydata$T1.Q42.1.2[T1.Q42.1.2==2] <- 1
mydata$T1.Q42.2.1[T1.Q42.2.1==1] <- 0
mydata$T1.Q42.2.1[T1.Q42.2.1==2] <- 1
mydata$T1.Q42.2.2[T1.Q42.2.2==1] <- 0
mydata$T1.Q42.2.2[T1.Q42.2.2==2] <- 1
mydata$T1.Q42.3.1[T1.Q42.3.1==1] <- 0
mydata$T1.Q42.3.1[T1.Q42.3.1==2] <- 1
mydata$T1.Q42.3.2[T1.Q42.3.2==1] <- 0
mydata$T1.Q42.3.2[T1.Q42.3.2==2] <- 1
mydata$T1.Q42.4.1[T1.Q42.4.1==1] <- 0
mydata$T1.Q42.4.1[T1.Q42.4.1==2] <- 1
mydata$T1.Q42.4.2[T1.Q42.4.2==1] <- 0
mydata$T1.Q42.4.2[T1.Q42.4.2==2] <- 1
mydata$T1.Q42.5.1[T1.Q42.5.1==1] <- 0
mydata$T1.Q42.5.1[T1.Q42.5.1==2] <- 1
mydata$T1.Q42.5.2[T1.Q42.5.2==1] <- 0
mydata$T1.Q42.5.2[T1.Q42.5.2==2] <- 1
mydata$T1.Q42.6.1[T1.Q42.6.1==1] <- 0
mydata$T1.Q42.6.1[T1.Q42.6.1==2] <- 1
mydata$T1.Q42.6.2[T1.Q42.6.2==1] <- 0
mydata$T1.Q42.6.2[T1.Q42.6.2==2] <- 1
mydata$T1.Q42.7.1[T1.Q42.7.1==1] <- 0
mydata$T1.Q42.7.1[T1.Q42.7.1==2] <- 1
mydata$T1.Q42.7.2[T1.Q42.7.2==1] <- 0
mydata$T1.Q42.7.2[T1.Q42.7.2==2] <- 1
mydata$T1.Q42.8.1[T1.Q42.8.1==1] <- 0
mydata$T1.Q42.8.1[T1.Q42.8.1==2] <- 1
mydata$T1.Q42.8.2[T1.Q42.8.2==1] <- 0
mydata$T1.Q42.8.2[T1.Q42.8.2==2] <- 1
mydata$T1.Q42.9.1[T1.Q42.9.1==1] <- 0
mydata$T1.Q42.9.1[T1.Q42.9.1==2] <- 1
mydata$T1.Q42.9.2[T1.Q42.9.2==1] <- 0
mydata$T1.Q42.9.2[T1.Q42.9.2==2] <- 1

################################################################################

attach(mydata)

# 16번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar1 <- c("T1.Q16.1","T1.Q16.2","T1.Q16.3","T1.Q16.4","T1.Q16.5",
            "T1.Q16.6","T1.Q16.7","T1.Q16.8","T1.Q16.9","T1.Q16.10")
PCA1 <- mydata[myvar1]
# 18번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar2 <- c("T1.Q18.1","T1.Q18.2","T1.Q18.3","T1.Q18.4","T1.Q18.5","T1.Q18.6","T1.Q18.7")
PCA2 <- mydata[myvar2]
# 27번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar3 <- c("T1.Q27.1","T1.Q27.2","T1.Q27.3","T1.Q27.4","T1.Q27.5","T1.Q27.6")
PCA3 <- mydata[myvar3]
# 28번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar4 <- c("T1.Q28.1","T1.Q28.2","T1.Q28.3","T1.Q28.4","T1.Q28.5",
            "T1.Q28.6","T1.Q28.7","T1.Q28.8","T1.Q28.9","T1.Q28.10")
PCA4 <- mydata[myvar4]
# 29번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar5 <- c("T1.Q29.1","T1.Q29.2","T1.Q29.3","T1.Q29.4","T1.Q29.5",
            "T1.Q29.6","T1.Q29.7","T1.Q29.8","T1.Q29.9","T1.Q29.10",
            "T1.Q29.11","T1.Q29.12","T1.Q29.13","T1.Q29.14","T1.Q29.15")
PCA5 <- mydata[myvar5]
# 30번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar6 <- c("T1.Q30.1","T1.Q30.2","T1.Q30.3","T1.Q30.4",
            "T1.Q30.5","T1.Q30.6","T1.Q30.7","T1.Q30.8")
PCA6 <- mydata[myvar6]
# 31번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar7 <- c("T1.Q31.1","T1.Q31.2","T1.Q31.3","T1.Q31.4",
            "T1.Q31.5","T1.Q31.6","T1.Q31.7","T1.Q31.8")
PCA7 <- mydata[myvar7]
# 32번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar8 <- c("T1.Q32.1","T1.Q32.2","T1.Q32.3","T1.Q32.4","T1.Q32.5",
            "T1.Q32.6","T1.Q32.7","T1.Q32.8","T1.Q32.9")
PCA8 <- mydata[myvar8]
# 34번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar9 <- c("T1.Q34.4","T1.Q34.5","T1.Q34.6")
PCA9 <- mydata[myvar9]
# 42번 문항의 딸린 문항들로 새로운 데이터 만들기
myvar10 <- c("T1.Q42.1.1","T1.Q42.1.2","T1.Q42.2.1","T1.Q42.2.2",
             "T1.Q42.3.1","T1.Q42.3.2","T1.Q42.4.1","T1.Q42.4.2",
             "T1.Q42.5.1","T1.Q42.5.2","T1.Q42.6.1","T1.Q42.6.2",
             "T1.Q42.7.1","T1.Q42.7.2","T1.Q42.8.1","T1.Q42.8.2",
             "T1.Q42.9.1","T1.Q42.9.2")
PCA10 <- mydata[myvar10]

################################################################################

library(sjPlot)
library(psych)
# PCA1
tab_pca(PCA1, title="학업중단 이전 학교생활", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 분산 비율")
# PCA2
fit2  <- princomp(PCA2, cor=TRUE)
summary(fit2)
plot(fit2, type="lines")
loadings(fit2)
alpha(PCA2, na.rm=TRUE)
# PCA3
tab_pca(PCA3, title="학업중단 관련 심리-정서", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 분산 비율")
# PCA4
tab_pca(PCA4, title="자아정체감", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 분산 비율")
alpha(PCA4, na.rm=TRUE)
# PCA5
tab_pca(PCA5, title="우울/충동성", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 분산 비율")
myvar5a <- c("T1.Q29.1","T1.Q29.2","T1.Q29.3","T1.Q29.4","T1.Q29.5",
             "T1.Q29.6","T1.Q29.7","T1.Q29.8","T1.Q29.9","T1.Q29.10","T1.Q29.12")
myvar5b <- c("T1.Q29.11","T1.Q29.13","T1.Q29.14","T1.Q29.15")
alpha(mydata[myvar5a], na.rm=TRUE)
alpha(mydata[myvar5b], na.rm=TRUE)
# PCA6
fit6  <- princomp(PCA6, cor=TRUE)
summary(fit6)
plot(fit6, type="lines")
loadings(fit6)
alpha(PCA6, na.rm=TRUE)
# PCA7
tab_pca(PCA7, title="부모애착", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 분산 비율")
alpha(PCA7, na.rm=TRUE)
# PCA8
tab_pca(PCA8, title="부모의 방임 및 학대", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 분산 비율")
alpha(PCA8, na.rm=TRUE)
# PCA9
fit9  <- princomp(PCA9, cor=TRUE)
summary(fit9)
plot(fit9, type="lines")
loadings(fit9)
alpha(PCA9, na.rm=TRUE)
# PCA10
tab_pca(PCA10, title="문제행동 가해경험", wrap.labels=20, show.cronb=TRUE,
        show.var=TRUE, string.pov="분산비율", string.cpov="누적 분산 비율")
alpha(PCA10, na.rm=TRUE)

################################################################################

attach(mydata)

# 변수생성
mydata$teacher.rel = T1.Q16.1+T1.Q16.2+T1.Q16.3
mydata$friendship = T1.Q16.4+T1.Q16.5+T1.Q16.6+T1.Q16.7
mydata$maladaptive = T1.Q16.8+T1.Q16.9+T1.Q16.10
mydata$violation = T1.Q18.1+T1.Q18.2+T1.Q18.3+T1.Q18.4+T1.Q18.5+T1.Q18.6+T1.Q18.7
mydata$delinquent = T1.Q20.1.99M+T1.Q20.2.99M+T1.Q20.3.99M+T1.Q20.4.99M
mydata$stop.emotion = T1.Q27.3+T1.Q27.4+T1.Q27.5+T1.Q27.6
mydata$self.identity = T1.Q28.1+T1.Q28.2+T1.Q28.3+T1.Q28.4+T1.Q28.5+T1.Q28.6+T1.Q28.7+T1.Q28.8+T1.Q28.9+T1.Q28.10
mydata$game.addiction = T1.Q30.1+T1.Q30.2+T1.Q30.3+T1.Q30.4+T1.Q30.5+T1.Q30.6+T1.Q30.7+T1.Q30.8
mydata$p.attachment = T1.Q31.1+T1.Q31.2+T1.Q31.3+T1.Q31.4+T1.Q31.5+T1.Q31.6+T1.Q31.7+T1.Q31.8
mydata$p.abuse = T1.Q32.1+T1.Q32.2+T1.Q32.3+T1.Q32.4+T1.Q32.5+T1.Q32.6+T1.Q32.7+T1.Q32.8+T1.Q32.9
mydata$f.delinquent = T1.Q34.4+T1.Q34.5+T1.Q34.6
mydata$harm = T1.Q42.1.1+T1.Q42.1.2+T1.Q42.2.1+T1.Q42.2.2+T1.Q42.3.1+T1.Q42.3.2+T1.Q42.4.1+T1.Q42.4.2+T1.Q42.5.1+T1.Q42.5.2+T1.Q42.6.1+T1.Q42.6.2+T1.Q42.7.1+T1.Q42.7.2+T1.Q42.8.1+T1.Q42.8.2+T1.Q42.9.1+T1.Q42.9.2
mydata$impulse = T1.Q29.11+T1.Q29.13+T1.Q29.14+T1.Q29.15
mydata$depressed = T1.Q29.1+T1.Q29.2+T1.Q29.3+T1.Q29.4+T1.Q29.5+T1.Q29.6+T1.Q29.7+T1.Q29.8+T1.Q29.9+T1.Q29.10+T1.Q29.12

################################################################################

attach(mydata)

# column명 변경
names(mydata) [names(mydata) == "T1.Q4.1"] <- c("stop.reason")
names(mydata) [names(mydata) == "T1.Q6"] <- c("grade")
names(mydata) [names(mydata) == "T1.Q48"] <- c("economy")

#########################################################

# 데이터 저장하기
save(mydata, file="mydata.RData")
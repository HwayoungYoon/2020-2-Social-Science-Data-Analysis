# 작업공간 설정
setwd("C:/R/Social Science Data Analysis")

# SPSS 파일 불러오기 위한 패키지 install 및 load
install.packages("Hmisc")
library(Hmisc)

# 중2패널 데이터 불러오기
## use.value.labels=TRUE : factor 형태의 데이터(분석 어려움)
test <- spss.get("[중2패널] 1차년도_6차년도 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
                 use.value.labels=FALSE)
## names(test) : test 데이터셋에 저장된 변수들의 이름

# 변수 선택 - 숫자형 벡터 만들기
select_variables <- c(1,4,9,10,19,101,102,103,328:337,339:348,372,375,377,379,
                      484:489, 496:504, 532)

# 선택된 변수만 가지는 새로운 데이터셋 test1 생성
## test1 <- test[select_variables]
test1 <- test[,select_variables]

# 학교 위치에 대한 값이 100이상, 200미만인 데이터(서울)만 선택
spssdata <- test1[which(test1$scharew1 >= 100 & test1$scharew1 <200), ]

# 데이터 저장하기
save(spssdata, file="spssdata.RData")

################################################################################

# sjmisc, sjlabelled 패키지 install 및 load
install.packages("sjmisc")
install.packages("sjlabelled")
library(sjmisc)
library(sjlabelled)

# label을 가진 중2패널 데이터 불러오기
test.labels <- spss.get("[중2패널] 1차년도_6차년도 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
                        use.value.labels=TRUE)

# 선택된 변수만 가지는 새로운 데이터셋 test.lavels1 생성
test.labels1 <- test.labels[select_variables]

# 숫자로 변경한 뒤 학교 위치에 대한 데이터 중 26 미만인 데이터(서울)만 선택
## as.numeric : 문자를 숫자로 변경
spssdata1 <- test.labels1[which(as.numeric(test.labels1$scharew1) < 26),]

# 라벨로 저장
labels.spss.values <- get_labels(spssdata1)
# 관측값과 라벨 일치시킴(각 관측값들이 라벨 가짐)
spssdata1 <- set_labels(spssdata, labels=labels.spss.values, force.values=FALSE, force.labels=TRUE)

################################################################################

# 변수생성
attach(spssdata)
spssdata$attachment = q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
#spssdata <- transform(spssdata, attachment=q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1)
spssdata$grade = q18a1w1+q18a2w1+q18a3w1
#detach(spssdata)

# 학교성적 변수를 3집단으로 분류
install.packages("epiDisplay")
library(epiDisplay)
tab1(spssdata$grade, cum.percent = TRUE)
attach(spssdata)
spssdata$grp.grade[grade >= min(grade) & grade <= 8] <- 1
spssdata$grp.grade[grade >= 9 & grade <= 10] <- 2
spssdata$grp.grade[grade >= 11 & grade <= max(grade)] <- 3
#detach(spssdata)
tab1(spssdata$grp.grade, cum.percent = TRUE)

# q50w1의 속성을 부정/중립/긍정으로
attach(spssdata)
spssdata$satisfaction[q50w1==1|q50w1==2] <- 1 #만족하지 못하는 편
spssdata$satisfaction[q50w1==3] <- 2          #보통
spssdata$satisfaction[q50w1==4|q50w1==5] <- 3 #만족하는 편
#detach(spssdata)

# 역방향 코딩
attach(spssdata)
spssdata$rq48a04w1[q48a04w1==1] <- 5
spssdata$rq48a04w1[q48a04w1==2] <- 4
spssdata$rq48a04w1[q48a04w1==3] <- 3
spssdata$rq48a04w1[q48a04w1==4] <- 2
spssdata$rq48a04w1[q48a04w1==5] <- 1

spssdata$rq48a05w1[q48a05w1==1] <- 5
spssdata$rq48a05w1[q48a05w1==2] <- 4
spssdata$rq48a05w1[q48a05w1==3] <- 3
spssdata$rq48a05w1[q48a05w1==4] <- 2
spssdata$rq48a05w1[q48a05w1==5] <- 1

spssdata$rq48a06w1[q48a06w1==1] <- 5
spssdata$rq48a06w1[q48a06w1==2] <- 4
spssdata$rq48a06w1[q48a06w1==3] <- 3
spssdata$rq48a06w1[q48a06w1==4] <- 2
spssdata$rq48a06w1[q48a06w1==5] <- 1
#detach(spssdata)

# 두 변수의 변수값을 교차시켜 하나의 집단변수 만들기
#attach(spssdata)
spssdata$grp.sex.grade[sexw1==1 & grp.grade==1] <- 11
spssdata$grp.sex.grade[sexw1==1 & grp.grade==2] <- 12
spssdata$grp.sex.grade[sexw1==1 & grp.grade==3] <- 13
spssdata$grp.sex.grade[sexw1==2 & grp.grade==1] <- 21
spssdata$grp.sex.grade[sexw1==2 & grp.grade==2] <- 22
spssdata$grp.sex.grade[sexw1==2 & grp.grade==3] <- 23
#detach(spssdata)

# 결측값은 NA로 지정
q33a07w1
table(q33a07w1)
table(q33a07w1, useNA="ifany")

# 데이터 저장하기
save(spssdata, file="spssdata.RData")

# workspace 저장하기(전부 저장하므로 저장할 파일 선택X)
save.image(file="myspssdata.RData")

################################################################################
# 데이터 병합하기 
second <- spss.get("[중2패널] 1차년도_6차년도 데이터(SPSS)/04-2 중2 패널 2차년도 데이터(SPSS).sav",
                 use.value.labels=FALSE)
mergedata <- merge(spssdata, second, by="id")

################################################################################
# 데이터 분할하기 
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, 
                  select=c("id", "sexw1", "scharew1", "areaw1", "q2w1", "q18a1w1", 
                           "q18a2w1", "q18a3w1"))

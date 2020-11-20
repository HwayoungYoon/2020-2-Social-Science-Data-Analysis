# T검정

## 성별/모든 연속형 변수
mydata$SEX.factor <- factor(mydata$SEX, levels=c(1,2), labels=c("남자","여자"))

#################### 충동성
## 분산 동질성 검정
var.test(impulse ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(impulse ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$impulse, group=mydata$SEX.factor)

#################### 교사관계
## 분산 동질성 검정
var.test(teacher.rel ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(teacher.rel ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$teacher.rel, group=mydata$SEX.factor)

#################### 친구관계
## 분산 동질성 검정
var.test(friendship ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(friendship ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$friendship, group=mydata$SEX.factor)

#################### 학습 부적응
## 분산 동질성 검정
var.test(maladaptive ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(maladaptive ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$maladaptive, group=mydata$SEX.factor)
#################### 학교규범 위반 경험
## 분산 동질성 검정
var.test(violation ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(violation ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$violation, group=mydata$SEX.factor)

#################### 학습중단 관련 심리-정서
## 분산 동질성 검정
var.test(stop.emotion ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(stop.emotion ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$stop.emotion, group=mydata$SEX.factor)

#################### 자아정체감
## 분산 동질성 검정
var.test(self.identity ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(self.identity ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$self.identity, group=mydata$SEX.factor)

#################### 우울
## 분산 동질성 검정
var.test(depressed ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(depressed ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$depressed, group=mydata$SEX.factor)

#################### 게임중독
## 분산 동질성 검정
var.test(game.addiction ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(game.addiction ~ SEX.factor, var.equal=FALSE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$game.addiction, group=mydata$SEX.factor)

#################### 부모애착
## 분산 동질성 검정
var.test(p.attachment ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(p.attachment ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$p.attachment, group=mydata$SEX.factor)

#################### 부모의 방임 및 학대
## 분산 동질성 검정
var.test(p.abuse ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(p.abuse ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$p.abuse, group=mydata$SEX.factor)

#################### 친구의 비행성향
## 분산 동질성 검정
var.test(f.delinquent ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(f.delinquent ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$f.delinquent, group=mydata$SEX.factor)

#################### 문제행동 가해경험
## 분산 동질성 검정
var.test(harm ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(harm ~ SEX.factor, var.equal=FALSE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$harm, group=mydata$SEX.factor)







#################### 교사관계
## 분산 동질성 검정
var.test(teacher.rel ~ SEX.factor, data=mydata)
## 연구가설 검증
t.test(teacher.rel ~ SEX.factor, var.equal=TRUE, data=mydata)
## 집단에 따른 기술 통계량
library(psych)
describeBy(mydata$teacher.rel, group=mydata$SEX.factor)

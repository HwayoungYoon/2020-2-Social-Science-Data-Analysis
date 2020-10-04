# 작업공간 설정
setwd("C:/R/Social Science Data Analysis")

# 데이터 불러들이기
load("spssdata.RData")
attach(spssdata)

################################################################################

# 1.빈도표 출력: 삶의 만족도
## table 함수 이용
table(q50w1)

## plyr 패키지의 count 함수 이용
library(plyr)
count(spssdata, "q50w1")

## table, cbind 함수 이용
tab <- table(q50w1)
cbind(Freq=tab, #빈도
      Cumul=cumsum(tab), #누적빈도
      Relative=prop.table(tab), #빈도별 비율
      Cum.prop = cumsum(prop.table(tab))) #누적비율

## round 함수를 이용해 소숫점 아래 셋째자리까지만 출력
cbind(Freq=table(q50w1),
      percentage=100*prop.table(table(q50w1)),
      relative=round(100*prop.table(table(q50w1)),3))

## Hmisc 패키지의 describe 함수 이용
library(Hmisc)
describe(q50w1)

## summarytools 패키지의 freq 함수 이용
install.packages("summarytools")
library(summarytools)
view(freq(q50w1))
## plain.ascii=FALSE, style="rmarkdown" : 표 스타일 변경
freq(q50w1, plain.ascii=FALSE, style="rmarkdown")

################################################################################

# 2.기술통계량 출력
## 변수에 대한 기본적인 정보 확인
str(q50w1)

## summary 함수 이용
summary(q50w1)

## psych 패키지의 describe 함수 이용
library(psych)
describe(q50w1)

## 동일한 변수를 범주형/연속형으로 취급했을 때 비교
var1 <- c("q33a01w1", "q50w1")
tab1 <- spssdata[var1]
library(summarytools)
freq(tab1)
library(psych)
describe(tab1)

## sjmisc 패키지의 descr 함수 이용
## encoding="EUC-KR" : 인코딩
## out="viewer" : viewer에 출력
library(sjmisc)
descr(tab1, encoding="EUC-KR", out="viewer")

################################################################################

# 3.여러 형태의 도표 그리기
## label 지정
lab.val <- c("전혀 만족하지 못한다", "만족하지 못하는 편이다", "보통이다", 
             "만족하는 편이다", "매우 만족한다")
# 3.1.바 도표
## barplot 함수 이용
## names.arg=lab.val : 카테고리명 지정
## space= : 바 사이의 거리
## border=NA : 바의 경계 없음
## cex.names= : 카테고리별 이름의 크기
## ylim=c(a,b) : y값의 높이 범위
par(mfrow=c(2,1))
barplot(table(q50w1),  #빈도
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 350))
barplot(prop.table(table(q50w1)), #비율
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 1))

par(mfrow=c(1,1))

## 도표의 색 변경
library(RColorBrewer)
## 선택 가능한 색상 확인
display.brewer.all()
## Set2에서 임의로 5개의 색상 선택
display.brewer.pal(5, "Set2")
pal1 <- brewer.pal(5, "Set2")
## col= : 바의 색상 지정
barplot(prop.table(table(q50w1)), col=pal1, 
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 1))

## sjPlot 패키지의 plot_frq 함수 이용
library(sjPlot)
plot_frq(q50w1)
plot_frq(q50w1, geom.size=0.3, ylim=c(0,350), geom.colors="grey47", title="",)
tab.val <- factor(q50w1, levels=c(1:5), labels=lab.val)
plot_frq(tab.val, axis.title="삶의 만족도", geom.colors="yellow", geom.size=0.5)

# 3.2.히스토그램
## 기술통계량 확인
library(psych)
describe(attachment)
## hist 함수 이용
par(mfrow=c(1,2))
hist(attachment)
library(RColorBrewer) #색깔
pal1 <- brewer.pal(7, "Set2")
## breaks= : 나누는 범위의 갯수
hist(attachment, breaks=20, col=pal1)
par(mfrow=c(1,1))

## density 함수 이용
par(mfrow=c(1,2))
plot(density(attachment, na.rm=TRUE))
d <- density(attachment, na.rm=TRUE)
plot(d)
## density 함수에 색 지정
polygon(d, col="red", border="blue")
par(mfrow=c(1,1))

## hist + density
## prob = TRUE : 무조건 비율에 대한 hist여야 함
## lines : plot 위에 line 생성
hist(attachment, color="blue", border="black", prob=TRUE, main="부모에 대한 애착")
lines(density(attachment, na.rm=TRUE), lwd=2, col="red")

# 3.3.1.파이 도표
tab <- table(q50w1)
pie(tab, labels=lab.val)

## 빈도표*100으로 %에 대한 값 생성(소숫점 첫째자리까지)
pct <- round(100*prop.table(tab), 1)
## paste : 해당 값을 문자로 연결하고 벡터로 변환하는 함수
## label + %값 lbls에 저장
lbls <- paste(lab.val, pct)
## lbls 뒤에 % 기호 추가
lbls <- paste(lbls, "%")
## init.angle=180 : 180도 회전
## radius= : pie chart의 반지름 길이
pie(pct, labels=lbls, col=rainbow(5),
    main="50. 학생은 학생의 삶에 전반적으로 얼마나 만족하고 있습니까?",
    init.angle=180, radius=1.0)

# 3.3.2.3D 파이도표
install.packages("plotrix")
library(plotrix)
## labelcex= : 라벨의 크기
## explode= : 파이 조각들 사이의 간격
## theta= : pie chart의 기울기 값
## shade= : 명암 조절
pie3D(pct, labels = lbls, col=rainbow(5),
      main="50. 학생은 학생의 삶에 전반적으로 얼마나 만족하고 있습니까?",
      labelcex=1.1, explode=0.1, theta=1.1, shade=0.3)

library(RColorBrewer) #색깔
pal1 <- brewer.pal(5, "Set2")
pie3D(pct, labels=lbls, col=pal1,
      main="50. 학생은 학생의 삶에 전반적으로 얼마나 만족하고 있습니까?",
      labelcex=1.1, explode = 0.1, theta=1.1, shade=0.3)

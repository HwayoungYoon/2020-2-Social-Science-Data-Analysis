# 작업공간 설정
setwd("C:/R/Social Science Data Analysis")

# 데이터 불러들이기
load("mydata.RData")
attach(mydata)

# 반응 # 충동성 impulse

# 이분 # 성별 SEX

# 명목 # 학업중단 이유 stop.reason

# 순서 # 학교 성적 grade
# 순서 # 비행경험 delinquent
# 순서 # 가정 경제수준 economy

## 교사관계 teacher.rel
## 친구관계 friendship
## 학습 부적응 maladaptive
## 학교규범 위반 경험 violation
## 학습중단 관련 심리-정서 stop.emotion
## 자아정체감 self.identity
## 우울 depressed
## 게임중독 game.addiction
## 부모애착 p.attachment
## 부모의 방임 및 학대 p.abuse
## 친구의 비행성향 f.delinquent
## 문제행동 가해경험 harm

##########################################################################
library(sjlabelled)
mydata$impulse <- set_label(mydata$impulse, "충동성")
mydata$SEX <- set_label(mydata$SEX, "성별")
mydata$stop.reason <- set_label(mydata$stop.reason, "학업중단 이유")
mydata$grade <- set_label(mydata$grade, "학교성적")
mydata$delinquent <- set_label(mydata$delinquent, "비행경험")
mydata$economy <- set_label(mydata$economy, "가정 경제수준")
mydata$teacher.rel <- set_label(mydata$teacher.rel, "교사관계")
mydata$friendship <- set_label(mydata$friendship, "친구관계")
mydata$maladaptive <- set_label(mydata$maladaptive, "학습 부적응")
mydata$violation <- set_label(mydata$violation, "학교규범 위반 경험")
mydata$stop.emotion <- set_label(mydata$stop.emotion, "학습중단 관련 심리-정서")
mydata$self.identity <- set_label(mydata$self.identity, "자아정체감")
mydata$depressed <- set_label(mydata$depressed, "우울")
mydata$game.addiction <- set_label(mydata$game.addiction, "게임중독")
mydata$p.attachment <- set_label(mydata$p.attachment, "부모애착")
mydata$p.abuse <- set_label(mydata$p.abuse, "부모의 방임 및 학대")
mydata$f.delinquent <- set_label(mydata$f.delinquent, "친구의 비행성향")
mydata$harm <- set_label(mydata$harm, "문제행동 가해경험")

##########################################################################
# 빈도표
library(summarytools)
## 성별
view(freq(SEX))
## 학업중단 이유
view(freq(grp.stop.reason))
## 학교 성적
view(freq(grade))
## 비행경험
view(freq(delinquent))
## 가정 경제수준
view(freq(economy))

##########################################################################

# 교차분석
## 성별/학업중단 이유
library(sjPlot)
tab_xtab(mydata$grp.stop.reason, mydata$SEX, show.col.prc=TRUE, 
         var.labels=c("학업중단 이유", "성별"), encoding="UTF-8")

set_theme(geom.label.size=4.5, axis.textsize=1.1, legend.pos="bottom")
plot_xtab(mydata$grp.stop.reason, mydata$SEX, type="bar", y.offset=0.01,
          margin="col", coord.flip=TRUE, wrap.labels=7, geom.colors="Set2",
          show.summary=TRUE)

##########################################################################

# 상관분석
## 모든 순서형 변수, 모든 연속형 변수
cor.var <- mydata[c("grade","delinquent","economy","teacher.rel",
                       "friendship","maladaptive","violation",
                       "stop.emotion","self.identity","depressed",
                       "game.addiction","p.attachment","p.abuse",
                       "f.delinquent","harm","impulse")]
library(sjPlot)
tab_corr(cor.var, corr.method="spearman", na.deletion="pairwise", 
         p.numeric=TRUE, triangle="lower", encoding="EUC-KR")
set_theme(axis.textsize=1.0)
sjp.corr(cor.var, corr.method="spearman", wrap.labels=5, na.deletion="pairwise")

##########################################################################

# 데이터 저장하기
save(mydata, file="mydata.RData")

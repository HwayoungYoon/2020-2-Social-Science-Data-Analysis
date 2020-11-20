# 일원분산분석

## 학업중단 이유/모든 연속형 변수
mydata$stop.reason.factor <- factor(mydata$stop.reason, levels=c(1:21),
                                    labels=c("건강상의 이유","심리/정신적인 문제","가정불화",
                                             "집안 경제사정","공부하기 싫음","학교의 불필요성",
                                             "친구들이 싫음","교칙이 엄함","학업중단 친구를 따라서",
                                             "선생님이 싫음","검정고시","특기/소질 개발",
                                             "교칙 위반으로 인한 징계","비행으로 보호/형사처분 받음",
                                             "기타","귀찮아서/잠/출성 불성실","거리/내신성적/종교/학교분위기",
                                             "학교부적응","유학/이민/대안학교/전학","학교의 거부","비행문제"))

mydata$grp.stop.reason[stop.reason==6] <- 1
mydata$grp.stop.reason[stop.reason==5 | stop.reason==9] <- 2
mydata$grp.stop.reason[stop.reason==12] <- 3
mydata$grp.stop.reason[stop.reason==11 | stop.reason==17 | stop.reason==19] <- 4
mydata$grp.stop.reason[stop.reason==8 | stop.reason==13 | stop.reason==14 | stop.reason==16 | stop.reason==21] <- 5
mydata$grp.stop.reason[stop.reason==7 | stop.reason==10 | stop.reason==18] <- 6
mydata$grp.stop.reason[stop.reason==1 | stop.reason==2] <- 7
mydata$grp.stop.reason[stop.reason==3 | stop.reason==4] <- 8

mydata$grp.stop.reason <- set_label(mydata$grp.stop.reason, "학업중단 이유")
mydata$grp.stop.reason <- set_labels(mydata$grp.stop.reason, levels=c(1:8),
                                 labels=c("학교의 불필요","공부 부적응","특기/소질 개발","검정고시/기타 진학",
                                          "징계 및 비행","학교 부적응","정신/건강상의 이유","가정상황"))
mydata$grp.stop.reason.factor <- factor(mydata$grp.stop.reason, levels=c(1:8),
                                    labels=c("학교의 불필요","공부 부적응","특기/소질 개발","검정고시/기타 진학",
                                             "징계 및 비행","학교 부적응","정신/건강상의 이유","가정상황"))

###############
library(sjstats)
# 학업중단 이유에 따른 집단간의 충동성 수준은 다를 것이다.
means_by_group(mydata, dv=impulse, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 교사관계 수준은 다를 것이다.
means_by_group(mydata, dv=teacher.rel, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 친구관계 수준은 다를 것이다.
means_by_group(mydata, dv=friendship, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 학습 부적응 수준은 다를 것이다.
means_by_group(mydata, dv=maladaptive, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 학교규범 위반 경험 수준은 다를 것이다.
means_by_group(mydata, dv=violation, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 학습중단 관련 심리-정서 수준은 다를 것이다.
means_by_group(mydata, dv=stop.emotion, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 자아정체감 수준은 다를 것이다.
means_by_group(mydata, dv=self.identity, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 우울 수준은 다를 것이다.
means_by_group(mydata, dv=depressed, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 게임중독 수준은 다를 것이다.
means_by_group(mydata, dv=game.addiction, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 부모애착 수준은 다를 것이다.
means_by_group(mydata, dv=p.attachment, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 부모의 방임 및 학대 수준은 다를 것이다.
means_by_group(mydata, dv=p.abuse, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 친구의 비행성향 수준은 다를 것이다.
means_by_group(mydata, dv=f.delinquent, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

# 학업중단 이유에 따른 집단간의 문제행동 가해경험 수준은 다를 것이다.
means_by_group(mydata, dv=harm, grp=grp.stop.reason, encoding="EUC-KR", out="viewer")

##########################################################################
# 이원분산분석
## 성별/학업중단 이유/모든 연속형 변수

# 성별/학업중단 이유에 따른 집단간의 충동성 수준
tw.ano1 <- aov(impulse ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano1)

# 성별/학업중단 이유에 따른 집단간의 교사관계 수준
tw.ano2 <- aov(teacher.rel ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano2)

# 성별/학업중단 이유에 따른 집단간의 친구관계 수준
tw.ano3 <- aov(friendship ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano3)

# 성별/학업중단 이유에 따른 집단간의 학습 부적응 수준
tw.ano4 <- aov(maladaptive ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano4)

# 성별/학업중단 이유에 따른 집단간의 학교규범 위반 경험 수준
tw.ano5 <- aov(violation ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano5)

# 성별/학업중단 이유에 따른 집단간의 학습중단 관련 심리-정서 수준
tw.ano6 <- aov(stop.emotion ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano6)

# 성별/학업중단 이유에 따른 집단간의 자아정체감 수준
tw.ano7 <- aov(self.identity ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano7)

# 성별/학업중단 이유에 따른 집단간의 우울 수준
tw.ano8 <- aov(depressed ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano8)

# 성별/학업중단 이유에 따른 집단간의 게임중독 수준
tw.ano9 <- aov(game.addiction ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano9)

# 성별/학업중단 이유에 따른 집단간의 부모애착 수준
tw.ano10 <- aov(p.attachment ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano10)

# 성별/학업중단 이유에 따른 집단간의 부모의 방임 및 학대 수준
tw.ano11 <- aov(p.abuse ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano11)

# 성별/학업중단 이유에 따른 집단간의 친구의 비행성향 수준
tw.ano12 <- aov(f.delinquent ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano12)

# 성별/학업중단 이유에 따른 집단간의 문제행동 가해경험 수준
tw.ano13 <- aov(harm ~ SEX.factor*grp.stop.reason.factor, data=mydata)
anova(tw.ano13)

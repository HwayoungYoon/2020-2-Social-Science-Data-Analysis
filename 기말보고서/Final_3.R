# 작업공간 설정
setwd("C:/R/Social Science Data Analysis")

# 데이터 불러들이기
load("mydata.RData")
attach(mydata)

################################################################################
# 변수 생성
mydata$sex.re[SEX == 1] <- 0
mydata$sex.re[SEX == 2] <- 1
mydata$sex.re <- set_label(mydata$sex.re, "성별")
mydata$sex.re <- set_labels(mydata$sex.re, labels=c("남자","여자"))

mydata$stop.reason.f <- factor(mydata$grp.stop.reason)
mydata$stop.reason.f <- set_label(mydata$stop.reason.f, "학업중단 이유")

# 결측값 제거
mydata.na.omit <- na.omit(mydata[c("impulse","sex.re","stop.reason.f","grade",
                                   "delinquent","economy","teacher.rel",
                                   "friendship","maladaptive","violation",
                                   "stop.emotion","self.identity","depressed",
                                   "game.addiction","p.attachment","p.abuse",
                                   "f.delinquent","harm")])

# Full model 1
regression.f1 <- lm(impulse ~ sex.re+stop.reason.f+sex.re:stop.reason.f+
                      grade+delinquent+economy+teacher.rel+friendship+
                      maladaptive+violation+stop.emotion+self.identity+
                      depressed+game.addiction+p.attachment+p.abuse+
                      f.delinquent+harm, data=mydata.na.omit)
summary(regression.f1)
tab_model(regression.f1, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

# mctest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression.f1)
imcdiag(regression.f1)

# 표준화 계수값
tab_model(regression.f1, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# 변수선택
# 선택제거
step3 <- step(regression.f1, direction="both")
summary(step3)

# 변수선택 결과 모형
reg.mod <- lm(impulse ~ economy + friendship + violation + stop.emotion + 
                self.identity + depressed + game.addiction + p.attachment + 
                p.abuse + f.delinquent + harm, data = mydata.na.omit)
summary(reg.mod)
tab_model(reg.mod, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

# mctest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(reg.mod)
imcdiag(reg.mod)

# 표준화 계수값
tab_model(reg.mod, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# Partial plots
library(ggplot2)
effect_plot(reg.mod, pred = economy, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = friendship, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = violation, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = stop.emotion, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = self.identity, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = depressed, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = game.addiction, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = p.attachment, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = p.abuse, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = f.delinquent, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = harm, interval = TRUE, plot.points = TRUE)

############################
# 잔차분석
# 더빈왓슨통계량 : 잔차의 독립성
library(car)
durbinWatsonTest(reg.mod)

# 표준화된 잔차의 잔차그림
id <- c(1:nrow(mydata.na.omit))
resid <- rstandard(reg.mod)
par(mfrow=c(1,1))
plot(id, resid, main="잔차의 독립성", ylab="표준화잔차",pch=21)

# 잔차그림 : 오차의 정규성 및 이상점, 영향점
pred <- predict(reg.mod)
plot(pred, resid, main="잔차 vs 적합값", pch=21, col="red", ylab="표준화잔차", 
     xlab="적합값")
abline(0,0)

# 잔차그림 : 반응값의 설명력
library(sjPlot)
plot_residuals(reg.mod)

# 잔차그림 : 회귀분석에서 5개의 잔차그림
par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod, i)
par(mfrow=c(1,1))

# 잔차그림 : 회귀분석에서 5개의 잔차그림 중 4가지
library(ggfortify)
autoplot(reg.mod)

# Partial residuals plots
effect_plot(reg.mod, pred = economy, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = friendship, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = violation, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = stop.emotion, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = self.identity, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = depressed, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = game.addiction, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = p.attachment, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = p.abuse, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = f.delinquent, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = harm, interval = TRUE, partial.residuals = TRUE)

############################
# 이상점 제거
remove <- c("395","443")
mydata.out <- mydata.na.omit[!row.names(mydata.na.omit)%in%remove,]

# full model
regression.fout <- lm(impulse ~ sex.re+stop.reason.f+sex.re:stop.reason.f+
                        grade+delinquent+economy+teacher.rel+friendship+
                        maladaptive+violation+stop.emotion+self.identity+
                        depressed+game.addiction+p.attachment+p.abuse+
                        f.delinquent+harm, data=mydata.out)
summary(regression.fout)
tab_model(regression.fout, show.se=TRUE, show.fstat=TRUE,
          auto.label=TRUE)

# mctest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression.fout)
imcdiag(regression.fout)

# 표준화 계수값
tab_model(regression.fout, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# 변수선택
# 선택제거
step3out <- step(regression.fout, direction="both")
summary(step3out)

# 변수선택 결과 모형
reg.mod2 <- lm(impulse ~ friendship + violation + stop.emotion + 
                 self.identity + depressed + game.addiction + p.abuse + f.delinquent + 
                 harm, data = mydata.out)
summary(reg.mod2)
tab_model(reg.mod2, show.se=TRUE, show.fstat=TRUE, auto.label=TRUE)

############################
# 잔차분석
# 더빈왓슨통계량 : 잔차의 독립성
library(car)
durbinWatsonTest(reg.mod2)

id2 <- c(1:nrow(mydata.out))
resid2 <- rstandard(reg.mod2)
par(mfrow=c(1,1))
plot(id2, resid2, main="잔차의 독립성", ylab="표준화잔차",pch=21)

# 잔차그림 : 오차의 정규성 및 이상점, 영향점
pred2 <- predict(reg.mod2)
plot(pred2, resid2, main="잔차 vs 적합값", pch=21, col="red", ylab="표준화잔차", 
     xlab="적합값")
abline(0,0)

library(sjPlot)
plot_residuals(reg.mod2)

par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod2, i)
par(mfrow=c(1,1))

############################
# 이상점 제거
remove2 <- c("589")
mydata.out2 <- mydata.out[!row.names(mydata.out)%in%remove2,]

# full model
regression.fout2 <- lm(impulse ~ sex.re+stop.reason.f+sex.re:stop.reason.f+
                         grade+delinquent+economy+teacher.rel+friendship+
                         maladaptive+violation+stop.emotion+self.identity+
                         depressed+game.addiction+p.attachment+p.abuse+
                         f.delinquent+harm, data=mydata.out2)
summary(regression.fout2)
tab_model(regression.fout2, show.se=TRUE, show.fstat=TRUE, auto.label=TRUE)

# mctest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression.fout2)
imcdiag(regression.fout2)

# 표준화 계수값
tab_model(regression.fout2, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# 변수선택
# 선택제거
step3out2 <- step(regression.fout2, direction="both")
summary(step3out2)

# 변수선택 결과 모형
reg.mod4 <- lm(impulse ~ friendship + violation + stop.emotion + 
                 self.identity + depressed + game.addiction + p.abuse + 
                 f.delinquent + harm, data = mydata.out2)
summary(reg.mod4)
tab_model(reg.mod4, show.se=TRUE, show.fstat=TRUE, auto.label=TRUE)

# mctest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(reg.mod4)
imcdiag(reg.mod4)

# 표준화 계수값
tab_model(reg.mod4, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

############################
# 잔차분석
# 더빈왓슨통계량 : 잔차의 독립성
library(car)
durbinWatsonTest(reg.mod4)

id4 <- c(1:nrow(mydata.out2))
resid4 <- rstandard(reg.mod4)
par(mfrow=c(1,1))
plot(id4, resid4, main="잔차의 독립성", ylab="표준화잔차",pch=21)

# 잔차그림 : 오차의 정규성 및 이상점, 영향점
pred4 <- predict(reg.mod4)
plot(pred4, resid4, main="잔차 vs 적합값", pch=21, col="red", ylab="표준화잔차", 
     xlab="적합값")
abline(0,0)

library(sjPlot)
plot_residuals(reg.mod4)

par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod4, i)
par(mfrow=c(1,1))

############################
# 최종모형 : 이상점 3개 제거 후 적합한 모형
reg.final <- lm(impulse ~ friendship + violation + stop.emotion + 
                 self.identity + depressed + game.addiction + p.abuse + 
                 f.delinquent + harm, data = mydata.out2)
tab_model(reg.final, show.se=TRUE, show.fstat=TRUE, show.std=TRUE, auto.label=TRUE)

# 표준화 계수값
tab_model(reg.mod4, show.se=TRUE, auto.label=TRUE)

##########################################################################

# 데이터 저장하기
save(mydata, file="mydata.RData")
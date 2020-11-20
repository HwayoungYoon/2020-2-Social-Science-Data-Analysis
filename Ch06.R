# �۾����� ����
setwd("C:/R/Social Science Data Analysis")

# ������ �ҷ����̱�
load("spssdata.RData")
attach(spssdata)

##########################################################################
## �Ͽ��л�м� : 
 # �б� ������ ���� ���ܰ��� �ھ����߰� ������ �ٸ� ���̴�.
############################
# �������� 
# �б����� ���� grade ����
spssdata$grade <- q18a1w1 + q18a2w1 + q18a3w1
attach(spssdata)

# �б����� ���� grade�� ����
library(summarytools)
freq(grade, plain.ascii=FALSE,style="rmarkdown")

 # �б����� ������ 3�������� �з�
spssdata$grp.grade[grade >= min(grade) & grade <= 8] <- 1
spssdata$grp.grade[grade >= 9 & grade <= 10] <- 2
spssdata$grp.grade[grade >= 11 & grade <= max(grade)] <- 3

 # ��������
library(sjlabelled)
spssdata$grp.grade <- set_label(spssdata$grp.grade, "�б�����")
 # ������ ����
spssdata$grp.grade <- set_labels(spssdata$grp.grade, levels=c(1,2,3),
      labels=c("���� �б����� ����", "�߰� �б����� ����", "���� �б����� ����"))
 # ������ ������ ��ȯ
spssdata$grp.grade.factor <- factor(spssdata$grp.grade, levels=c(1,2,3),
      labels=c("���� �б����� ����", "�߰� �б����� ����", "���� �б����� ����"))

############################
# ���Ӻ��� 
# ������ �ڵ�
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

attach(spssdata)
 # �ھ����߰� ���� 
spssdata$self.esteem <- q48a01w1+q48a02w1+q48a03w1+rq48a04w1+rq48a05w1+rq48a06w1
 # ��������
spssdata$self.esteem <- set_label(spssdata$self.esteem ,"�ھ����߰�")

############################
# �Ͽ��л�м� : 
# �б� ������ ���� ���ܰ��� �ھ����߰� ������ �ٸ� ���̴�.
ano1 <- aov(self.esteem ~ grp.grade.factor, data=spssdata)
anova(ano1)
#ano <- aov(self.esteem ~ as.factor(grp.grade), data=spssdata)
#anova(ano)

# ���ܺ� ��հ�
library(psych)
describeBy(spssdata$self.esteem, group=spssdata$grp.grade)

# sjstats ��Ű���� �̿��� �л�м� ���̺�
library(sjstats)
means_by_group(spssdata, dv=self.esteem, grp=grp.grade, encoding="EUC-KR", out="viewer")
 
# sjstats ��Ű���� �̿��� �л�м� �׷���
library(sjPlot)
set_theme(geom.label.size=4.5, axis.textsize=1.2)
sjp.aov1(spssdata$self.esteem, spssdata$grp.grade, geom.size=0.5, 
         wrap.labels=7, axis.lim=c(17.5,22), meansums = TRUE, show.summary = TRUE)

# gplots ��Ű���� �̿��� �����跮 �� ��ǥ
library(gplots)
plotmeans(self.esteem~grp.grade.factor, data=spssdata, xlab="�б�����",
          ylab="�ھ����߰�", ci.label=TRUE, mean.label=TRUE, ylim=c(16,23),
          barwidth=5, digits=2, col="brown", pch=1, barcol="red",
          main="�б������� ���� ���ܺ� �ھ����߰� ����")

############################
# �Ͽ��л�м� : ���ߺ�
install.packages("agricolae")
library(agricolae)
scheffe.test(ano1, "grp.grade.factor", alpha=0.05, console=TRUE)
  #Scheffe�� ���İ���
LSD.test(ano1, "grp.grade.factor", alpha=0.05, console=TRUE)
  #Fisher�� LSD(Least Significant Difference) ���İ���
duncan.test(ano1, "grp.grade.factor", alpha=0.05, console=TRUE)
  #Duncan�� ���� ����
## ���İ��� ��� �б����� ������ ���� 3���� ���� ������̴� ���躰������ 0.05����
## ��� ��������� ������ ���̰� �ִ� ������ ��Ÿ����.


##########################################################################
## �̿��л�м� : 
# ������ ���� �б����� ���� ���� �θ� ���� ���� ������ �ٸ� ���̴�. (��ȣ�ۿ� ȿ��)
# ���� ���� ���� �θ� ���� ���� ������ �ٸ� ���̴�. (���� ��ȿ��)
# �б������� ���� ���� ���� �θ� ���� ���� ������ �ٸ� ���̴�. (���� ��ȿ��)
##########################################################################
attach(spssdata)
 # �θ� ���� ���� ����
spssdata$attachment <- q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
library(sjlabelled)
spssdata$attachment <- set_label(spssdata$attachment, "�θ� ���� ����")

  #������ ������ ��ȯ
spssdata$sexw1.factor <- factor(spssdata$sexw1, levels=c(1,2),
                                labels=c("����","����"))

## �̿��л�м�
tw.ano1a <- aov(attachment ~ sexw1.factor + grp.grade.factor + 
                sexw1.factor:grp.grade.factor, data=spssdata)
anova(tw.ano1a)

tw.ano1b <- aov(attachment ~ sexw1.factor*grp.grade.factor, data=spssdata)
anova(tw.ano1b)

############################
# �̿��л�м� : ���ߺ�
library(agricolae)
scheffe.test(tw.ano1a, "grp.grade.factor", alpha=0.05, console=TRUE)
#Scheffe�� ���İ���
LSD.test(tw.ano1a, "grp.grade.factor", alpha=0.05, console=TRUE)
#Fisher�� LSD(Least Significant Difference) ���İ���
duncan.test(tw.ano1a, "grp.grade.factor", alpha=0.05, console=TRUE)
#Duncan�� ���� ����
## ��ȣ�ۿ�� ���� ��ȿ���� ������ �̿��л�м����� �б������� ���� ���ߺ� ��� 
## �б����� ������ ���� 3���� ���� ������̴� ���躰������ 0.05����
## ��� ��������� ������ ���̰� �ִ� ������ ��Ÿ����.

###################################
# ��ȣ�ۿ� ȿ��: ���ܺ� ��� ��
library(psych)
 # describeBy�Լ� ���: ���ܺ� ������跮 �� ���
describeBy(spssdata$attachment, list(spssdata$sexw1,spssdata$grp.grade),
           mat=TRUE, digits=2)
 # FUN='mean'���� ��� ��� ����
aggregate(attachment ~ sexw1+grp.grade, data=spssdata, FUN='mean')
tapply(spssdata$attachment, spssdata[,c("sexw1","grp.grade")], mean)
tapply(spssdata$attachment, list(spssdata$sexw1, spssdata$grp.grade), mean)

### �� ���������� ������ ������ �ϳ��� ������ ��ȯ
attach(spssdata)
spssdata$grp.sex.grade[sexw1==1 & grp.grade==1] <- 11
spssdata$grp.sex.grade[sexw1==1 & grp.grade==2] <- 12
spssdata$grp.sex.grade[sexw1==1 & grp.grade==3] <- 13
spssdata$grp.sex.grade[sexw1==2 & grp.grade==1] <- 21
spssdata$grp.sex.grade[sexw1==2 & grp.grade==2] <- 22
spssdata$grp.sex.grade[sexw1==2 & grp.grade==3] <- 23
detach(spssdata)

library(sjlabelled)
  # ��������
spssdata$grp.sex.grade <- set_label(spssdata$grp.sex.grade, "���� �б�����")
  # ������ ����
spssdata$grp.sex.grade <- set_labels(spssdata$grp.sex.grade,
      labels=c("����/���� �б����� ����", "����/�߰� �б����� ����", 
               "����/���� �б����� ����", "����/���� �б����� ����", 
               "����/�߰� �б����� ����", "����/���� �б����� ����"))

## ���ܿ� ���� �����跮 
# ���ܺ� �����跮 ǥ
library(sjstats)
means_by_group(spssdata, dv=attachment, grp=grp.sex.grade, encoding="EUC-KR", out="viewer")

# gplots ��Ű���� �̿��� �����跮 �� ��ǥ
# ������ ������ ��ȯ
spssdata$grp.sex.grade.factor <- factor(spssdata$grp.sex.grade,
            labels=c("����/���� �б����� ����", "����/�߰� �б����� ����", 
                     "����/���� �б����� ����", "����/���� �б����� ����", 
                     "����/�߰� �б����� ����", "����/���� �б����� ����"))
library(gplots)
plotmeans(attachment~grp.sex.grade.factor, data=spssdata, xlab="���� �б�����",
          ylab="�θ����", ci.label=TRUE, mean.label=TRUE, ylim=c(17,24.5),
          barwidth=5, digits=2, col="brown", pch=1, barcol="red",
          main="���� �б������� ���� ���ܺ� �θ���� ����")

# ��ȣ�ۿ� �׸�
interaction.plot(spssdata$grp.grade.factor, spssdata$sexw1.factor, 
                 spssdata$attachment , col=c(2,4),
                 trace.label="����", xlab="�б�����", ylab="�θ� ���� ����",
                 ylim=c(17,24), type="b", 
                 fun=function(x) mean(x,na.rm=TRUE))

################################################################################

# ������ �����ϱ�
save(spssdata, file="spssdata.RData")
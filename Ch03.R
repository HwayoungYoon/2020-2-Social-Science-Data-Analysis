# �۾����� ����
setwd("C:/R/Social Science Data Analysis")

# ������ �ҷ����̱�
load("spssdata.RData")
attach(spssdata)

################################################################################

# 1.��ǥ ���: ���� ������
## table �Լ� �̿�
table(q50w1)

## plyr ��Ű���� count �Լ� �̿�
library(plyr)
count(spssdata, "q50w1")

## table, cbind �Լ� �̿�
tab <- table(q50w1)
cbind(Freq=tab, #��
      Cumul=cumsum(tab), #������
      Relative=prop.table(tab), #�󵵺� ����
      Cum.prop = cumsum(prop.table(tab))) #��������

## round �Լ��� �̿��� �Ҽ��� �Ʒ� ��°�ڸ������� ���
cbind(Freq=table(q50w1),
      percentage=100*prop.table(table(q50w1)),
      relative=round(100*prop.table(table(q50w1)),3))

## Hmisc ��Ű���� describe �Լ� �̿�
library(Hmisc)
describe(q50w1)

## summarytools ��Ű���� freq �Լ� �̿�
install.packages("summarytools")
library(summarytools)
view(freq(q50w1))
## plain.ascii=FALSE, style="rmarkdown" : ǥ ��Ÿ�� ����
freq(q50w1, plain.ascii=FALSE, style="rmarkdown")

################################################################################

# 2.�����跮 ���
## ������ ���� �⺻���� ���� Ȯ��
str(q50w1)

## summary �Լ� �̿�
summary(q50w1)

## psych ��Ű���� describe �Լ� �̿�
library(psych)
describe(q50w1)

## ������ ������ ������/���������� ������� �� ��
var1 <- c("q33a01w1", "q50w1")
tab1 <- spssdata[var1]
library(summarytools)
freq(tab1)
library(psych)
describe(tab1)

## sjmisc ��Ű���� descr �Լ� �̿�
## encoding="EUC-KR" : ���ڵ�
## out="viewer" : viewer�� ���
library(sjmisc)
descr(tab1, encoding="EUC-KR", out="viewer")

################################################################################

# 3.���� ������ ��ǥ �׸���
## label ����
lab.val <- c("���� �������� ���Ѵ�", "�������� ���ϴ� ���̴�", "�����̴�", 
             "�����ϴ� ���̴�", "�ſ� �����Ѵ�")
# 3.1.�� ��ǥ
## barplot �Լ� �̿�
## names.arg=lab.val : ī�װ����� ����
## space= : �� ������ �Ÿ�
## border=NA : ���� ��� ����
## cex.names= : ī�װ����� �̸��� ũ��
## ylim=c(a,b) : y���� ���� ����
par(mfrow=c(2,1))
barplot(table(q50w1),  #��
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 350))
barplot(prop.table(table(q50w1)), #����
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 1))

par(mfrow=c(1,1))

## ��ǥ�� �� ����
library(RColorBrewer)
## ���� ������ ���� Ȯ��
display.brewer.all()
## Set2���� ���Ƿ� 5���� ���� ����
display.brewer.pal(5, "Set2")
pal1 <- brewer.pal(5, "Set2")
## col= : ���� ���� ����
barplot(prop.table(table(q50w1)), col=pal1, 
        names.arg=lab.val,
        space=1.5, border=NA, cex.names=0.7, ylim=c(0, 1))

## sjPlot ��Ű���� plot_frq �Լ� �̿�
library(sjPlot)
plot_frq(q50w1)
plot_frq(q50w1, geom.size=0.3, ylim=c(0,350), geom.colors="grey47", title="",)
tab.val <- factor(q50w1, levels=c(1:5), labels=lab.val)
plot_frq(tab.val, axis.title="���� ������", geom.colors="yellow", geom.size=0.5)

# 3.2.������׷�
## �����跮 Ȯ��
library(psych)
describe(attachment)
## hist �Լ� �̿�
par(mfrow=c(1,2))
hist(attachment)
library(RColorBrewer) #����
pal1 <- brewer.pal(7, "Set2")
## breaks= : ������ ������ ����
hist(attachment, breaks=20, col=pal1)
par(mfrow=c(1,1))

## density �Լ� �̿�
par(mfrow=c(1,2))
plot(density(attachment, na.rm=TRUE))
d <- density(attachment, na.rm=TRUE)
plot(d)
## density �Լ��� �� ����
polygon(d, col="red", border="blue")
par(mfrow=c(1,1))

## hist + density
## prob = TRUE : ������ ������ ���� hist���� ��
## lines : plot ���� line ����
hist(attachment, color="blue", border="black", prob=TRUE, main="�θ� ���� ����")
lines(density(attachment, na.rm=TRUE), lwd=2, col="red")

# 3.3.1.���� ��ǥ
tab <- table(q50w1)
pie(tab, labels=lab.val)

## ��ǥ*100���� %�� ���� �� ����(�Ҽ��� ù°�ڸ�����)
pct <- round(100*prop.table(tab), 1)
## paste : �ش� ���� ���ڷ� �����ϰ� ���ͷ� ��ȯ�ϴ� �Լ�
## label + %�� lbls�� ����
lbls <- paste(lab.val, pct)
## lbls �ڿ� % ��ȣ �߰�
lbls <- paste(lbls, "%")
## init.angle=180 : 180�� ȸ��
## radius= : pie chart�� ������ ����
pie(pct, labels=lbls, col=rainbow(5),
    main="50. �л��� �л��� � ���������� �󸶳� �����ϰ� �ֽ��ϱ�?",
    init.angle=180, radius=1.0)

# 3.3.2.3D ���̵�ǥ
install.packages("plotrix")
library(plotrix)
## labelcex= : ���� ũ��
## explode= : ���� ������ ������ ����
## theta= : pie chart�� ���� ��
## shade= : ���� ����
pie3D(pct, labels = lbls, col=rainbow(5),
      main="50. �л��� �л��� � ���������� �󸶳� �����ϰ� �ֽ��ϱ�?",
      labelcex=1.1, explode=0.1, theta=1.1, shade=0.3)

library(RColorBrewer) #����
pal1 <- brewer.pal(5, "Set2")
pie3D(pct, labels=lbls, col=pal1,
      main="50. �л��� �л��� � ���������� �󸶳� �����ϰ� �ֽ��ϱ�?",
      labelcex=1.1, explode = 0.1, theta=1.1, shade=0.3)
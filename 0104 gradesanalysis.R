setwd("~/R Scripts/instagram/data")
library(MatchIt)
library(gplots)
library(ggplot2)
library(dplyr)
library(psych)
library(plyr)
library(reshape2)
library(ggcorrplot)
library(lme4)
library(lmerTest)

df.roster<- read.csv("roster.csv",header = T)
df.q0<- read.csv("q0n.csv",header = T)
df.tests<- read.csv("p1p2q1q2.csv",header = T)
df.canvas<- read.csv("canvas1.csv",header = T)


df.tests$treat
df.tests[df.tests=="#N/A"]<-NA
df.tests$treatreal<- as.numeric(df.tests$treatreal)
df.tests$treatreal<-ifelse(!is.na(df.tests$Q6p)|!is.na(df.tests$Q12p),1,NA)

df.q0$Q5<-as.numeric(mapvalues(df.q0$Q5, from = c("Never", "Once a month", "Once a week","Once a day","More than once a day"), 
                               to = c(1, 2, 3,4,5)))
# Work_sat
for (i in 10:12) {
  df.q0[,i]<-as.numeric(mapvalues(df.q0[,i], from = c("Extremely dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied","Somewhat satisfied","Extremely satisfied"), 
                                  to = c(1, 2, 3,4,5)))
}
df.q0$work_sat<- rowMeans(cbind(df.q0[,10],df.q0[,11],df.q0[,12]),na.rm = TRUE)

# need_to_belong
for (i in 13:22) {
  df.q0[,i]<-as.numeric(mapvalues(df.q0[,i], from = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree","Somewhat agree","Strongly agree"), 
                                  to = c(1, 2, 3,4,5)))
}

df.q0$need_to<- rowMeans(cbind(-df.q0[,13],df.q0[,14],-df.q0[,15],df.q0[,16],df.q0[,17],df.q0[,18],-df.q0[,19],df.q0[,20],df.q0[,21],df.q0[,22]),na.rm = TRUE)
# col_learning
for (i in 23:29) {
  df.q0[,i]<-as.numeric(mapvalues(df.q0[,i], from = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree","Somewhat agree","Strongly agree"), 
                                  to = c(1, 2, 3,4,5)))
}
df.q0$col_learning<- rowMeans(cbind(df.q0[,23],df.q0[,24],df.q0[,25],df.q0[,26],df.q0[,27],-df.q0[,28],df.q0[,29]),na.rm = TRUE)

# satisf
for (i in 30:35) {
  df.q0[,i]<-as.numeric(mapvalues(df.q0[,i], from = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree","Somewhat agree","Strongly agree"), 
                                  to = c(1, 2, 3,4,5)))
}
df.q0$satisf<- rowMeans(cbind(df.q0[,30],df.q0[,31],df.q0[,32],df.q0[,33],df.q0[,34],df.q0[,35]),na.rm = TRUE)

###############################
df.roster<-df.roster[,c(1:8,12)]
df.canvas<-df.canvas[,c(2,9)]
colnames(df.canvas)<-c("NAME","Canvas")
df.combine<-left_join(df.roster,df.canvas,by="NAME")
colnames(df.combine)[4]<-"netid"
df.combine<-left_join(df.combine,df.q0,by="netid")
df.combine<-left_join(df.combine,df.tests,by="netid")
colnames(df.combine)[which(colnames(df.combine) == "Q29p")]<-"avetime"
df.combine[df.combine=="#N/A"]<-NA
for (i in which(colnames(df.combine) == "avetime") : dim(df.combine)[2]) {
  df.combine[,i]<- as.numeric(df.combine[,i])
}
df.combine[,65:84]<- 6 - df.combine[,65:84]
df.combine[,86:105]<- 6 - df.combine[,86:105]
df.combine$socilpre_t2<-rowMeans(cbind(df.combine[,65],df.combine[,66],df.combine[,67],df.combine[,68],df.combine[,69],df.combine[,70],df.combine[,71],df.combine[,72],df.combine[,73]),na.rm = TRUE)
df.combine$cogpre_t2<-rowMeans(cbind(df.combine[,74],df.combine[,75],df.combine[,76],df.combine[,77],df.combine[,78],df.combine[,79],df.combine[,80],df.combine[,81],df.combine[,82],df.combine[,83],df.combine[,84]),na.rm = TRUE)
df.combine$socilpre_t1<-rowMeans(cbind(df.combine[,86],df.combine[,87],df.combine[,88],df.combine[,89],df.combine[,90],df.combine[,91],df.combine[,92],df.combine[,93],df.combine[,94]),na.rm = TRUE)
df.combine$cogpre_t1<-rowMeans(cbind(df.combine[,95],df.combine[,96],df.combine[,97],df.combine[,98],df.combine[,99],df.combine[,100],df.combine[,101],df.combine[,102],df.combine[,103],df.combine[,104],df.combine[,105]),na.rm = TRUE)

df.p3<-read.csv("allgrades.csv",header = T)
colnames(df.p3)[1]<-"netid"
colnames(df.p3)[4]<-"p3"
df.p3<-subset(df.p3,select=c(netid,p3))
df.combine<-left_join(df.combine,df.p3,by="netid")
data1<-df.combine
data1$dif_total<-data1$p2-data1$p1
data1$treat_real<-ifelse(data1$treatreal == 1,data1$treat, 0)
data1$treat_post<-as.factor(ifelse(data1$treat_real == 2|data1$treat_real ==3,1,0))
data1$treat_real<-as.factor(data1$treat_real)
data1$selection<-as.factor(ifelse(data1$treatreal == "1",1, 0))

#remove outliers who missed p1 or p2
outliers<-which(is.na(data1$p1) | is.na(data1$p2) | is.na(data1$p3) | data1$p2 == 0)
data2<-data1[-outliers,]
data2[data2=="#N/A"]<-NA
# Divide groups to G0:non-participants, G1: treat1, G2: treat2, G3: treat3, G4: participants but dropped 
G4<-which(is.na(data2$treat_real) & !is.na(data2$treat))
G1<-which(data2$treat_real == "treat1")
G2<-which(data2$treat_real == "treat2")
G3<-which(data2$treat_real == "treat3")
G0<-which(is.na(data2$treat_real) & is.na(data2$treat))

G<-factor(c(1:358),levels = c("G0","G1","G2","G3","G4"))
G[G1]<-factor(c("G1"))   
G[G2]<-factor(c("G2"))   
G[G3]<-factor(c("G3"))   
G[G4]<-factor(c("G4"))   
G[G0]<-factor(c("G0"))   
sum(is.na(G))
data2$G<-G
colnames(data2)[7] <- "Credits"
colnames(data2)[8] <- "Years"
colnames(data2)[9] <- "Sex"
colnames(data2)[15] <- "Stu_know"
colnames(data2)[18] <- "Ins_fam"

data2$Sex2[data2$Sex=="male"]<-1
data2$Sex2[data2$Sex=="female"]<-0

data2$Years2<-as.numeric(data2$Years)
data2$Years2[271]<-61
data2$Years2 <- round(data2$Years2/10,digits = 0)

data2$Stu_know<-as.numeric(data2$Stu_know)
data2$Ins_fam<-as.numeric(data2$Ins_fam)

which(colnames(data2)=='treat_post')
data2<-subset(data2,select = c(netid,Credits,Years2,Sex2,Canvas,Stu_know,Q3,Q4,Ins_fam,work_sat,need_to,col_learning,satisf,p1,p2,p3,avetime,selection,dif_total,socilpre_t1,socilpre_t2,cogpre_t1,cogpre_t2,G))
data2[data2=="NaN"]<-NA

describe(data2$Sex2)
describeBy(data2$Sex2, G)

describe(data2$Years2)
describeBy(data2$Years2, G)

describe(data2$Credits)
describeBy(data2$Credits, G)

describe(data2$Stu_know)
describeBy(data2$Stu_know, G)

describe(data2$Ins_fam)
describeBy(data2$Ins_fam, G)

describe(data2$need_to)
describeBy(data2$need_to, G)

describe(data2$col_learning)
describeBy(data2$col_learning, G)

describe(data2$satisf)
describeBy(data2$satisf, G)

describe(data2$p1)
describeBy(data2$p1, G)

describe(data2$p2)
describeBy(data2$p2, G)


describeBy(data2$socilpre_t1, G)
describeBy(data2$socilpre_t2, G)
describeBy(data2$cogpre_t1, G)
describeBy(data2$cogpre_t2, G)

data3<-subset(data2,select = c(netid,p1,p2,p3,Sex2,G))
Tr<-factor(c(1:358),levels = c("T1","T2","T0"))
T1<-which(data3$G == "G1" | data3$G == "G2")
T2<-which(data3$G == "G3")
T0<-which(data3$G == "G0" | data3$G == "G4")
Tr[T1]<-factor(c("T1"))   
Tr[T2]<-factor(c("T2")) 
Tr[T0]<-factor(c("T0")) 
data3$Tr<-Tr
data3<-subset(data3,select = c(netid,p1,p2,p3,Tr,Sex2))
data3<-melt(data3,id.vars = c("netid","Sex2","Tr"))

View(data2)
summary(data3$Tr)
ggplot(data3, aes(x=variable, y=value, fill=Tr)) + 
  geom_boxplot(alpha=0.2)+ 
  coord_cartesian(ylim = c(60, 100))+
  stat_summary(fun = mean, color = "darkred", position = position_dodge(0.75),
               geom = "point", show.legend = FALSE)+
  stat_summary(fun = mean, geom="line",aes(group = Tr,color=Tr),size = 1.2, show.legend = FALSE)+
  ggtitle("Plot of prelim grades by group") +
  xlab("Prelim") + 
  ylab("Grade")+ 
  labs(fill = "Treatment")


#PSM
data2$treated<-as.factor(ifelse(data2$G == "G1"|data2$G == "G2",1,0))
data2$interacted<-as.factor(ifelse(data2$G == "G1",1,0))
data2$group <- as.logical(data2$treated == "1")
match.it <- matchit(group ~ p1 + Sex2 + Canvas , data = data2, method="nearest", ratio=1)
plot(match.it, type = 'jitter', interactive = FALSE)
df.match <- match.data(match.it)[1:ncol(data2)]
ggplot(data = df.match,mapping = aes(x=p1,y=p2,group=treated,color=treated))+
  geom_point()+
  geom_smooth(method = "lm",se=TRUE)


describeBy(data2[data2$treated == "0",]$p1)
describeBy(df.match[df.match$treated == "0",]$p1)
describeBy(data2[data2$treated == "1",]$p1)

#######################################Analyze p1 and p2
ggplot(data = data2,mapping = aes(x=p1,y=p2,group=treated,color=treated))+
  geom_point()+
  geom_smooth(method = "lm",se=TRUE)

fit.d<-lm((p2-p1)~treated,data=df.match)
summary(fit.d)
fit.p.2<-lm(p2~p1*treated,data=data2)
summary(fit.p.2)
fit.p.2.match<-lm(p2~p1*treated,data=df.match)
summary(fit.p.2.match)
fit.p.1<-lm(p2~p1+ treated,data=data2)
summary(fit.p.1)
fit.p.1.match<-lm(p2~p1+treated,data=df.match)
summary(fit.p.1.match)

#DID
data2.did<-subset(data2,select = c(p1,p2,treated))
data2.did<-melt(data2.did,id.vars = c("treated"))
colnames(data2.did)[2]<-"time"
data2.did$time<-ifelse(data2.did$time == "p1",0,1)
data2.did$time<-as.factor(data2.did$time)
fit.1d<-lm(value~treated * time,data = data2.did)
summary(fit.1d)

data2.dids<-subset(data2,select = c(G,treated,p1,p2))
data2.dids$selection<-as.factor(ifelse(data2.dids$G=="G0"|data2.dids$G=="G4",0,1))
data2.dids<-subset(data2.dids,select = c(treated,p1,p2,selection))
data2.dids<-melt(data2.dids,id.vars = c("treated","selection"))
data2.dids$time<-as.factor(ifelse(data2.dids$variable == "p1",0,1))
fit.1e<-lm(value~time + treated + time:treated +time:selection ,data = data2.dids)
summary(fit.1e)

data2.did.m<-subset(df.match,select = c(p1,p2,treated))
data2.did.m<-melt(data2.did.m,id.vars = c("treated"))
colnames(data2.did.m)[2]<-"time"
data2.did.m$time<-ifelse(data2.did.m$time == "p1",0,1)
data2.did.m$time<-as.factor(data2.did.m$time)
fit.1d.m<-lm(value~treated * time,data = data2.did.m)
summary(fit.1d.m)

data2.dids.m<-subset(df.match,select = c(G,treated,p1,p2))
data2.dids.m$selection<-as.factor(ifelse(data2.dids.m$G=="G0"|data2.dids.m$G=="G4",0,1))
data2.dids.m<-subset(data2.dids.m,select = c(treated,p1,p2,selection))
data2.dids.m<-melt(data2.dids.m,id.vars = c("treated","selection"))
data2.dids.m$time<-as.factor(ifelse(data2.dids.m$variable == "p1",0,1))
fit.1e.m<-lm(value~time + treated + time:treated +time:selection ,data = data2.dids.m)
summary(fit.1e.m)






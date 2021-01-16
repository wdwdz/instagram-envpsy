setwd("~/R Scripts/instagram")
data <- read.csv("participants.csv",na.strings = "#N/A",header = F)
newdata<- data
colnames(newdata)<-c("time","netid","program","grade","year","sex","discussion","test0")
table(newdata$discussion)
newdata$treatment <- c("0")
newdata$treatment[newdata$discussion=="0"]<- sample(c(rep("treat1",44),rep("treat2",44),rep("treat3",45)))
newdata$treatment[newdata$discussion=="1"]<- sample(c(rep("treat1",22),rep("treat2",22),rep("treat3",22)))
newdata$treatment[newdata$discussion=="2"]<- sample(c(rep("treat1",3),rep("treat2",3),rep("treat3",2)))
table(newdata$discussion,newdata$treatment)


write.table (newdata,file ="treatment.csv", sep =",", row.names = FALSE) 


setwd("~/Github/instagram-envpsy/data")
library(tidyverse)
library(plyr)
library(emmeans)


df.roster<- read.csv("roster.csv",header = T)
df.s1<-read.csv("survey1.csv", header = T)
df.s2<-read.csv("survey2.csv", header = T)
df.s3<-read.csv("survey3.csv", header = T)



for (i in 4:(ncol(df.s1)-1)) {
  df.s1[,i]<-as.numeric(mapvalues(df.s1[,i], from = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree","Somewhat agree","Strongly agree"), 
                                  to = c(1, 2, 3,4,5)))
}
df.s1$soc1<- rowMeans(cbind(df.s1$Q6,df.s1$Q7,df.s1$Q8,df.s1$Q9,df.s1$Q10,df.s1$Q11,df.s1$Q12,df.s1$Q13,df.s1$Q14),na.rm = TRUE)
df.s1$cog1<- rowMeans(cbind(df.s1$Q17,df.s1$Q18,df.s1$Q19,df.s1$Q20,df.s1$Q21,df.s1$Q22,df.s1$Q23,df.s1$Q24,df.s1$Q25,df.s1$Q26,df.s1$Q27),na.rm = TRUE)

#alpha(df.s1[,4:10])

for (i in 4:(ncol(df.s2)-1)) {
  df.s2[,i]<-as.numeric(mapvalues(df.s2[,i], from = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree","Somewhat agree","Strongly agree"), 
                                  to = c(1, 2, 3,4,5)))
}
df.s2$soc2<- rowMeans(cbind(df.s2$Q6,df.s2$Q7,df.s2$Q8,df.s2$Q9,df.s2$Q10,df.s2$Q11,df.s2$Q12,df.s2$Q13,df.s2$Q14),na.rm = TRUE)
df.s2$cog2<- rowMeans(cbind(df.s2$Q17,df.s2$Q18,df.s2$Q19,df.s2$Q20,df.s2$Q21,df.s2$Q22,df.s2$Q23,df.s2$Q24,df.s2$Q25,df.s2$Q26,df.s2$Q27),na.rm = TRUE)

for (i in 7:(ncol(df.s3)-1)) {
  df.s3[,i]<-as.numeric(mapvalues(df.s3[,i], from = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree","Somewhat agree","Strongly agree"), 
                                  to = c(1, 2, 3,4,5)))
}
df.s3$soc3<- rowMeans(cbind(df.s3$Q6,df.s3$Q7,df.s3$Q8,df.s3$Q9,df.s3$Q10,df.s3$Q11,df.s3$Q12,df.s3$Q13,df.s3$Q14),na.rm = TRUE)
df.s3$cog3<- rowMeans(cbind(df.s3$Q17,df.s3$Q18,df.s3$Q19,df.s3$Q20,df.s3$Q21,df.s3$Q22,df.s3$Q23,df.s3$Q24,df.s3$Q25,df.s3$Q26,df.s3$Q27),na.rm = TRUE)


df.s3.sc<-subset(df.s3,select = c(Q15,group,soc3,cog3))
df.s2.sc<-subset(df.s2,select = c(Q15,soc2,cog2))
df.s1.sc<-subset(df.s1,select = c(Q15,soc1,cog1))

df.s2.sc$Q15<-tolower(df.s2.sc$Q15)
df.s1.sc$Q15<-tolower(df.s1.sc$Q15)
df.s3.sc$Q15<-tolower(df.s3.sc$Q15)

df.s2.sc<-df.s2.sc[!duplicated(df.s2.sc$Q15),]
df.s1.sc<-df.s1.sc[!duplicated(df.s1.sc$Q15),]


df.combine<-merge(df.s3.sc, df.s2.sc, by = "Q15",all.x = TRUE)
df.combine.sc<-left_join(df.combine, df.s1.sc, by = "Q15",all.x = TRUE)

# we created a new csv based on the data above
# from here, we can use df.new as the raw data and remove other dataframes
# we add another 3 columns to indicate the instagram followers, following, and condition
# we may only filter the subjects whose condition is 3 as they are real participants
# df.combine.sc is the dataframes contain treatment, social presence and cognitive presence.
# Below are did models using the full data in one model.
# firstly, lets look at the social presence




df.new<-read.csv("participants_details.csv",header = T)
colnames(df.new)[1]<-"netid"

################THIS PART WE USE ONE MODEL
###########################################
view(social_tidy)
df.combine.s<-subset(df.new,select = c(netid,soc1,soc2,soc3,group,condition)) 
social_tidy<-df.combine.s %>% 
  filter(condition==3) %>% 
  pivot_longer(c(soc3,soc2,soc1), names_to = 'time', values_to = 'socialp') 

model.soc.did<- lmerTest::lmer(data=social_tidy,socialp~group*time+(1|netid))
summary(model.soc.did)

social.emm<-emmeans(model.soc.did, specs =  ~ group * time)
#predicted test score for each group in each test
#write down the order list here
emmip(model.soc.did, group ~ time,CIs = TRUE)
#model.emm<-emmeans(0 = T2S2 - T2S1  + T3S2 - T3S1  - 2*T1S2 + 2*T1S1)
LF<-contrast(social.emm,
             list(CMO = c(2, -1, -1, -2, 1, 1, 0, 0, 0))
  
)
LF


df.combine.c<-subset(df.new,select = c(netid,cog1,cog2,cog3,group,condition)) 
cog_tidy<-df.combine.c %>% 
  filter(condition==3) %>% 
  pivot_longer(c(cog3,cog2,cog1), names_to = 'time', values_to = 'cogp') 

model.cog.did<- lmerTest::lmer(data=cog_tidy,cogp~group*time+(1|netid))
summary(model.cog.did)
emmip(model.cog.did, group ~ time,CIs = TRUE)



#################################################
#################################################

df.combine.s<-subset(df.combine.sc,select = c(Q15,soc1,soc2,soc3,group))
df.combine.s$group<-as.factor(df.combine.s$group)

data3<-melt(df.combine.s,id.vars = c("Q15","group"))

ggplot(data3, aes(x=variable, y=value, fill=group)) + 
  geom_boxplot(alpha=0.2)+ 
  coord_cartesian(ylim = c(1, 5))+
  stat_summary(fun = mean, color = "darkred", position = position_dodge(0.75),
               geom = "point", show.legend = FALSE)+
  stat_summary(fun = mean, geom="line",aes(group = group,color=group),size = 1.2)



df.combine.c<-subset(df.combine.sc,select = c(Q15,cog1,cog2,cog3,group))
df.combine.c$group<-as.factor(df.combine.c$group)

data3<-melt(df.combine.c,id.vars = c("Q15","group"))

ggplot(data3, aes(x=variable, y=value, fill=group)) + 
  geom_boxplot(alpha=0.2)+ 
  coord_cartesian(ylim = c(1, 5))+
  stat_summary(fun = mean, color = "darkred", position = position_dodge(0.75),
               geom = "point", show.legend = FALSE)+
  stat_summary(fun = mean, geom="line",aes(group = group,color=group),size = 1.2)



#################################ANCOVA ANALYSIS
##########df.combine.s df.combine.c 
# In this analysis, we use the pretest score as the covariate and are interested in 
# possible differences between group with respect to the soc3 scores.
# guide from https://www.datanovia.com/en/lessons/ancova-in-r/
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
df.new<-read.csv("participants_details.csv",header = T)



##
#first, we compare treat group(treat1 + treat2) with control group(treat3)
#we remove those not involved students(condition=1:no post/lack many posts;
#condition=2:no followers/followings)
# the test showed significant difference between groups
cogs1<-as_tibble(df.new)
cogs1 <- cogs1 %>% 
  filter(condition==3) %>% 
  select(?..netid,group,cog1,cog2) %>% 
  mutate(group2=(ifelse(group=="treat3","control","treat"))) %>% 
  dplyr::rename(id = ?..netid,pretest=cog1,posttest=cog2)

ggscatter(
  cogs1, x = "pretest", y = "posttest",
  color = "group2", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group)
  )

cogs1 %>% anova_test(posttest ~ group2*pretest)

# Fit the model, the covariate goes first
model <- lm(posttest ~ pretest + group2, data = cogs1)
# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)
# ANCOVA assumes that the variance of the residuals is equal for all groups. This can be checked using the Levene's test
model.metrics %>% levene_test(.resid ~ group2)

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
#the outliers jnu4 will be removed
#social<-social %>% 
#  filter(id != "jnu4")

#Computation
res.aov <- social %>% anova_test(posttest ~ pretest + group2)
get_anova_table(res.aov)

library(emmeans)
pwc <- cogs1 %>% 
  emmeans_test(
    posttest ~ group2, covariate = pretest,
    p.adjust.method = "bonferroni"
  )
pwc

pwc <- pwc %>% add_xy_position(x = "group2", fun = "mean_se")
ggline(get_emmeans(pwc), x = "group2", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

##
#second, we compare treat group(treat1) with control group(treat2+treat3)
#we remove those not involved students(condition=1:no post/lack many posts;
#condition=2:no followers/followings)
# the effect is positive but not significant difference between groups

socs1<-as_tibble(df.new)
socs1 <- socs1 %>% 
  filter(condition==3) %>% 
  select(?..netid,group,soc1,soc2) %>% 
  mutate(group2=(ifelse(group=="treat1","treat","control"))) %>% 
  dplyr::rename(id = ?..netid,pretest=soc1,posttest=soc2)

ggscatter(
  socs1, x = "pretest", y = "posttest",
  color = "group2", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group2)
  )

socs1 %>% anova_test(posttest ~ group2*pretest)

# Fit the model, the covariate goes first
model <- lm(posttest ~ pretest + group2, data = socs1)
# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)
# ANCOVA assumes that the variance of the residuals is equal for all groups. This can be checked using the Levene's test
model.metrics %>% levene_test(.resid ~ group2)

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
#the outliers jnu4 will be removed
#social<-social %>% 
#  filter(id != "jnu4")

#Computation
res.aov <- socs1 %>% anova_test(posttest ~ pretest + group2)
get_anova_table(res.aov)

library(emmeans)
pwc <- cogs1 %>% 
  emmeans_test(
    posttest ~ group2, covariate = pretest,
    p.adjust.method = "bonferroni"
  )
pwc

pwc <- pwc %>% add_xy_position(x = "group2", fun = "mean_se")
ggline(get_emmeans(pwc), x = "group2", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

### very active users
socs1<-as_tibble(df.new)
socs1 <- socs1 %>% 
  filter(condition==3) %>% 
  select(?..netid,group,soc1,soc2) %>% 
  mutate(group2=(ifelse(group=="treat1","treat","control"))) %>% 
  dplyr::rename(id = ?..netid,pretest=soc1,posttest=soc2)

ggscatter(
  socs1, x = "pretest", y = "posttest",
  color = "group2", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group2)
  )

socs1 %>% anova_test(posttest ~ group2*pretest)

# Fit the model, the covariate goes first
model <- lm(posttest ~ pretest + group2, data = socs1)
# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)
# ANCOVA assumes that the variance of the residuals is equal for all groups. This can be checked using the Levene's test
model.metrics %>% levene_test(.resid ~ group2)

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
#the outliers jnu4 will be removed
#social<-social %>% 
#  filter(id != "jnu4")

#Computation
res.aov <- socs1 %>% anova_test(posttest ~ pretest + group2)
get_anova_table(res.aov)

library(emmeans)
pwc <- cogs1 %>% 
  emmeans_test(
    posttest ~ group2, covariate = pretest,
    p.adjust.method = "bonferroni"
  )
pwc

pwc <- pwc %>% add_xy_position(x = "group2", fun = "mean_se")
ggline(get_emmeans(pwc), x = "group2", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

































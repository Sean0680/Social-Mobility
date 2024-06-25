#############################################
##Concentration and cultural change study 1##
#############################################

library(broom)
library("car")
library("phia")
library("interactions")

d<-Berger_synthpop_para

d$Condition<-ifelse(is.na(d$Concentrated)==T,0,1)

#transformations
d$Petition_1_dummy<-ifelse(is.na(d$Petition_1)==T,0,1)
d$Petition_2_dummy<-ifelse(is.na(d$Petition_2)==T,0,1)
d$Petition_3_dummy<-ifelse(is.na(d$Petition_3)==T,0,1)
d$Petition_4_dummy<-ifelse(is.na(d$Petition_4)==T,0,1)
d$Petition_5_dummy<-ifelse(is.na(d$Petition_5)==T,0,1)
d$Petition_6_dummy<-ifelse(is.na(d$Petition_6)==T,0,1)
d$Petition_7_dummy<-ifelse(is.na(d$Petition_7)==T,0,1)
d$Petition_8_dummy<-ifelse(is.na(d$Petition_8)==T,0,1)

#exclude people who didn't finish the study
d2<-d[is.na(d$Gender)==F,]
table(d2$Gender)
mean(d2$Age,na.rm=T)
sd(d2$Age,na.rm=T)

results = bind_rows(
  lm1 = tidy(fit1<-glm(Petition_8_dummy~Condition,family=binomial(link="logit"),data=d2)),
  lm2 = tidy(lm(Petition_8_dummy~Condition,data=d2)),
  .id = "model"
)

write_csv(results, "SocialMobility/Berger/gen_data/results/Berger_results_synthpop_para.csv")

setwd("~/Dropbox/PhD/Zoology/Manuscripts/Thermal mutualism/for Evolution Letters")
library(lme4)
library(car)
#1.Effect of mites and beetle body temperature on contests between rival females
data=read.csv("Effect of mites and beetle body temperature on contests between rival females.csv")
#1.1contest with mites vs without mites
datacontest=data[data$experimental.treatment=="with mites vs. without mites",]
datacontest$trial.ID<-as.factor(datacontest$trial.ID)
datacontest$block<-as.factor(datacontest$block)
model=glmer(contest.outcome~mite.treatment+carcass.mass+relative.difference.in.body.size+block+(1|trial.ID),family=binomial,data=datacontest)
Anova(model)
#1.2 temperature differences between beeltes with mites and without mites before and after the fight
data=read.csv("Effect of mites on temperature difference before and after the fight.csv")
data$ID<-as.factor(data$ID)
data$aggressive.act<-as.factor(data$aggressive.act)
data$block<-as.factor(data$block)
#1.2.1before the fight
databefore=data[data$before.or.after.the.fight=="before",]
model=glmer(difference.in.beetle.temperature~mite.treatment+relative.difference.in.body.size+block+carcass.mass+(1|ID/aggressive.act),family=gaussian,data=databefore)
Anova(model)
#1.2.2after the fight
dataafter=data[data$before.or.after.the.fight=="after",]
model=glmer(difference.in.beetle.temperature~mite.treatment*difference.in.beetle.temperature.before.fight+relative.difference.in.body.size+block+carcass.mass+(1|ID/aggressive.act),family=gaussian,data=dataafter)
Anova(model)
#1.3contest without mites (warming) vs without mites
data=read.csv("Effect of mites and beetle body temperature on contests between rival females.csv")
datawarm=data[data$experimental.treatment=="without mites/warmed vs. without mites",]
datawarm$trial.ID<-as.factor(datawarm$trial.ID)
model=glmer(contest.outcome~mite.treatment+carcass.mass+relative.difference.in.body.size+(1|trial.ID),family=binomial,data=datawarm)
Anova(model)
#1.4contest with mites (cooling) vs with mites
datacool=data[data$experimental.treatment=="with mites/cooled vs. with mites",]
datacool$trial.ID<-as.factor(datacool$trial.ID)
model=glmer(contest.outcome~mite.treatment+carcass.mass+relative.difference.in.body.size+(1|trial.ID),family=binomial,data=datacool)
Anova(model)
#2.Effect of mite density on beetle body temperature
data=read.csv("Effect of mite density on beetle body temperature.csv")
data$individual.ID<-as.factor(data$individual.ID)
#2.1Mite density effect on beetle's temperature difference
model=glmer(difference.in.temperature~sequence.of.mite.association+mite.number.treatment+body.size+(1|individual.ID),family=gaussian,data=data)
Anova(model)
#2.2thermal benefits conferred by mites differed with body size
datapropincrease=data[data$mite.number.treatment!="zero",]
model=glmer(proportional.increase.in.temperature~sequence.of.mite.association+mite.number.treatment*poly(body.size,degree=2)+(1|individual.ID),family=gaussian,data=datapropincrease)
Anova(model)
#2.3greater increase in body temperature in smaller beetles carrying 30 mites
datathirty=data[data$mite.number.treatment=="thirty",]
model=glm(proportional.increase.in.temperature~sequence.of.mite.association+poly(body.size,degree=2),family=gaussian,data=datathirty)
Anova(model)
#3.Are mites a source of heat?
data=read.csv("Are mites a source of heat.csv")
model=glmer(difference.in.temperature~sequence.of.mite.association+mite.number.treatment+body.size+(1|individual.ID),family=gaussian,data=data)
Anova(model)
#4.Treadmill experiments
data=read.csv("Treadmill experiment.csv")
data$individual.ID<-as.factor(data$individual.ID)
data$timing<-as.factor(data$timing)
datalarge=data[data$beetle.size=="L",]
datasmall=data[data$beetle.size=="S",]
#4.1Walking period
model=glmer(temperature.difference.during.walking~treatment*beetle.size*timing+(1|individual.ID),gaussian,data=data)
Anova(model)
#4.1.1Walking period for large beetles
model=glmer(temperature.difference.during.walking~treatment*timing+(1|individual.ID),gaussian,data=datalarge)
Anova(model)
#4.1.2Walking period for small beetles
model=glmer(temperature.difference.during.walking~treatment*timing+(1|individual.ID),gaussian,data=datasmall)
Anova(model)
#4.2Resting period
model=glmer(temperature.difference.during.resting~treatment*beetle.size*timing+(1|individual.ID),gaussian,data=data)
Anova(model)
#4.2.1Resting period for large beetles
model=glmer(temperature.difference.during.resting~treatment*timing+(1|individual.ID),gaussian,data=datalarge)
Anova(model)
#4.2.2Resting period for small beetles
model=glmer(temperature.difference.during.resting~treatment*timing+(1|individual.ID),gaussian,data=datasmall)
Anova(model)
#5.Measurement of beetle surface area, surface area covered by mites,and surface area:volume ratio
#5.1 Proportion of surface acrea covered by mites
data=read.csv("Beetle surface area covered and surface area volume ratio .csv")
data$individual.ID<-as.factor(data$individual.ID)
model=glmer((log.surface.covered)~treatment*bodysize+(1|individual.ID),gaussian,data=data)
Anova(model)
#5.1 Surface acrea:volume ratio 
model=glmer(surfacevolumeratio~treatment*bodysize+(1|individual.ID),gaussian,data=data)
Anova(model)
#6.Effect of mites on contests between beetles differed in size
data=read.csv("Effect of mites on contests between beetles differed in size.csv")
model=glm(contest.outcome~body.size*mite.treatment+relative.difference.in.body.size+carcass.mass,family=binomial,data=data)
Anova(model)
#7.Effect of mites on burying beetle reproductive success, with respect to beetle body size
data=read.csv("Effect of mites on burying beetle reproductive success with respect to beetle body size.csv")
model=glmer(broodsize~beetle.size*mite.treatment+carcass.mass+(1|block),family=poisson,data=data)
Anova(model)
#8.Effect of mite density on mite reproductive success
data=read.csv("Effect of mite density on mite reproductive success.csv")
data$number.of.mites <- as.numeric(data$number.of.mites)
y<-cbind(data$number.of.moulted.mites,data$number.of.unmoulted.mites)
model=glm(y~poly(number.of.mites,degree=2),family=binomial,data=data)
Anova(model)
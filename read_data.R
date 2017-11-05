#Replication data
library(readr)
library(foreign)
library(sandwich)
library(lmtest)

setwd('/Users/yanaotlan/Desktop/200D/Replication')
dta_mid <- read.dta('HorowitzStamLeadersIOMIDReplication.dta')
dta_war <- read.dta('HorowitzStamLeadersIOWarReplication.dta')
#run the models as they are in the paper
#model 1
#why is age so significant? Did I do something wrong?..
model1 <- glm(cwinit ~ milnoncombat + combat + rebel + warwin + warloss + rebelwin + 
                rebelloss + age + aut + cinc + tau_lead + officetenure1000 + 
                fiveyearchallengelag + cwpceyrs1 + cwpceyrs2 + cwpceyrs3, data = dta_mid, family=binomial(link="logit"))
summary(model1)
coeftest(model1, function(x) vcovHC(x, method="arellano",type="HC1",cluster="leaderid"))


#model 2
model2 <- glm(initiation ~ milnoncombat + combat + rebel + warwin + warloss + rebelwin + 
                rebelloss + age + aut + cinc + tau_lead + officetenure1000 + 
                fiveyearchallengelag + cwpceyrs1 + cwpceyrs2 + cwpceyrs3, data = dta_war, family=binomial(link="logit"))
summary(model2)
coeftest(model2, function(x) vcovHC(x, method="arellano",type="HC1",cluster="leaderid"))

#model 3
model3 <- glm(cwinit ~ milnoncombat + combat + rebel + milpolity + combatpolity + rebelpolity + 
                milrebel + combatrebel + warwin + warloss + rebelwin + rebelloss + age + 
                polity21 + cinc + tau_lead + officetenure1000 + fiveyearchallengelag + 
                cwpceyrs1 + cwpceyrs2 + cwpceyrs3, data = dta_mid, family=binomial(link="logit")) 
summary(model3)
coeftest(model3, function(x) vcovHC(x, method="arellano",type="HC1",cluster="leaderid"))


#model 4 
model4 <- glm(cwinit ~ milnoncombat + combat + rebel + milaut + combataut + rebelaut + 
                milrebel + combatrebel + warwin + warloss + rebelwin + rebelloss + age + 
                aut +  cinc + tau_lead + officetenure1000 + fiveyearchallengelag + 
                cwpceyrs1 + cwpceyrs2 + cwpceyrs3, data = dta_mid, family=binomial(link="logit")) 
summary(model4)
coeftest(model4, function(x) vcovHC(x, method="arellano",type="HC1",cluster="leaderid"))

#model 5 
new_dta_mid <- dta_mid %>% 
  filter(year > 1945) %>%
  filter(year < 2000)
 
model5 <- glm(cwinit ~ milnoncombat + combat + rebel + milmil + combatmil + rebelmil + 
                 milrebel + combatrebel + warwin + warloss + rebelwin + rebelloss + age + 
                 cinc + cgvmildict + tau_lead + officetenure1000 + fiveyearchallengelag + 
                 cwpceyrs1 + cwpceyrs2 + cwpceyrs3, data = new_dta_mid, family=binomial(link="logit")) 

summary(model5)
coeftest(model5, function(x) vcovHC(x, method="arellano",type="HC1",cluster="leaderid"))

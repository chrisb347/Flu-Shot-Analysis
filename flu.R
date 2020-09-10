
library(dplyr)
library(tidyverse)
library(Hmisc)
library(twang)
library(survey)
library(tmle)
library(SuperLearner)
library(dbarts)
install.packages('tmle')
install.packages("SuperLearner")
install.packages("dbarts")

#install.packages('survey')
BRFSS<- sasxport.get('C:\\Users\\Christopher Barry\\Desktop\\fordham\\Obserational Studies\\Project\\BRFSS.XPT')

# Set options for allowing a single observation per stratum
options(survey.lonely.psu = "adjust")
# Create survey design
brfssdsgn <- svydesign(
  id=~1,
  strata = ~x.ststr,
  weights = ~x.llcpwt,
  data = BRFSS) 


##mean for flu shot
svymean(~factor(flushot6),
        brfssdsgn, na.rm = TRUE)

# ###mean for flu by inc
# ##create outcome that is most similar to flu like symptoms
# flu_sub<-subset(brfssdsgn,((flushot6==1) & (genhlth == 1))|((flushot6==1)& (genhlth == 2))|((flushot6==1)& (genhlth == 3))|((flushot6==1)& (genhlth == 4)))
# x_flu_sub<-subset(brfssdsgn,((flushot6==2) & (genhlth == 1))|((flushot6==2)& (genhlth == 2))|((flushot6==2)& (genhlth == 3))|((flushot6==2)& (genhlth == 4)))
# flu_sub<-subset(flu_sub, imonth=='12' | imonth=='01' | imonth=='02' | imonth=='03' |imonth=='04')
# x_flu_sub<-subset(x_flu_sub, imonth=='12' | imonth=='01' | imonth=='02' | imonth=='03' |imonth=='04')


###check subsets
unique(flu_sub$variables$genhlth)
unique(x_flu_sub$variables$genhlth)
unique(flu_sub$variables$flushot6)
unique(x_flu_sub$variables$flushot6)
unique(flu_sub$variables$imonth)
unique(x_flu_sub$variables$imonth) 

summary(flu_sub$flushot6)

mean_flu_inc<-svymean(~factor(income2),
                      design=flu_sub, na.rm = TRUE)

mean_xFlu_inc<-svymean(~factor(income2),
                       design=x_flu_sub, na.rm = TRUE)

diff_mean_flu_inc<-(mean_flu_inc - mean_xFlu_inc)/mean_xFlu_inc
barplot(diff_mean_flu_inc,
        names.arg=c('Less than $10,000','$10,000 to less than $15,000','($15,000 to less than $20,000','$20,000 to less than $25,000',
                    '$25,000 to less than $35,000','($35,000 to less than $50,000','($50,000 to less than $75,000','$75,000 or more','Dont know/Not sure','Refused'),horiz=TRUE, cex.names=0.8)
title('Perecnt Difference in Group Size between \nVaccinated and NonVaccinated Participants', adj=0)

###marital

mean_flu_mar<-svymean(~factor(marital),
                      design=flu_sub, na.rm = TRUE)

mean_xFlu_mar<-svymean(~factor(marital),
                       design=x_flu_sub, na.rm = TRUE)

diff_mean_flu_mar<-(mean_flu_mar - mean_xFlu_mar)/mean_xFlu_mar
barplot(diff_mean_flu_mar,
        names.arg=c('Married','Divorced','Widowed','Separated',
                    'Never married','A member of an unmarried couple','Refused'),horiz=TRUE, cex.names=0.8)
title('Perecnt Difference in Group Size between \nVaccinated and NonVaccinated Participants', adj=0)



#####race

mean_flu_race<-svymean(~factor(x.race),
                       design=flu_sub, na.rm = TRUE)

mean_xFlu_race<-svymean(~factor(x.race),
                        design=x_flu_sub, na.rm = TRUE)

diff_mean_flu_race<-(mean_flu_race - mean_xFlu_race)/mean_xFlu_race

par(las=2)
par(mar=c(5,16,4,2))
barplot(diff_mean_flu_race,
        names.arg=c('White only','Black only','American Indian or Alaskan Native only','Asian only',
                    'Native Hawaiian or other Pacific Islander only','Other race only','Multiracial','Hispanic
','Dont know'),horiz=TRUE, cex.names=0.8)
title('Perecnt Difference in Group Size between \nVaccinated and NonVaccinated Participants', adj=0)

###education level

mean_flu_ed<-svymean(~factor(educa),
                     design=flu_sub, na.rm = TRUE)

mean_xFlu_ed<-svymean(~factor(educa),
                      design=x_flu_sub, na.rm = TRUE)

diff_mean_flu_ed<-(mean_flu_ed - mean_xFlu_ed)/mean_xFlu_ed
barplot(diff_mean_flu_ed,names.arg=c('Never attended school or only kindergarten','Grades 1 through 8 (Elementary)',
                                     'Grades 9 through 11 (Some high school)','Grade 12 or GED (High school graduate)',
                                     'College 1 year to 3 years (Some college or
technical school)','College 4 years or more (College graduate)','Refused'),horiz=TRUE, cex.names=0.8)
title('Perecnt Difference in Group Size between \nVaccinated and NonVaccinated Participants', adj=0)



###########################################################################################################################
#MODELING
##########################################################################################################################



###subset months and health

flu_sub_t<-subset(brfssdsgn,( (genhlth == 1))|( (genhlth == 2))|( (genhlth == 3))|( (genhlth == 4)))

flu_sub_t<-subset(flu_sub_t, imonth=='12' | imonth=='01' | imonth=='02' | imonth=='03' |imonth=='04')
flu_sub_t<-subset(flu_sub_t,flushot6==1|flushot6==2)
flu_sub_t<-subset(flu_sub_t,!is.na(flushot6))
flu_sub_t<-subset(flu_sub_t,!is.na(x.race))
flu_sub_t<-subset(flu_sub_t,!is.na(educa))
flu_sub_t<-subset(flu_sub_t,!is.na(marital))

##check subsets for model

unique(flu_sub_t$variables$imonth) 
unique(flu_sub_t$variables$genhlth)

flu_data_twang<-flu_sub_t$variables
flu_data_twang$health_outcome<-ifelse(flu_data_twang$physhlth>4&flu_data_twang$physhlth<31,1,0)


unique(flu_data_twang$x.race)
ps_mod <- ps(health_outcome~flushot6 +marital + x.race + educa+income2,
             data = flu_data_twang, 
             n.trees = 3000, interaction.depth =2, shrinkage = 0.1, # For GBM
             perm.test.iters = 0, # p-values for Kolmogorov-Smirnov test statistic; number of Monte Carlo trials
             stop.method = c("es.mean", "ks.max"), 
             estimand = "ATT")
plot(ps_mod)
summary(ps_mod$gbm.obj, n.trees = ps_mod$desc$es.mean.ATT$n.trees)


bal_tab <- bal.table(ps_mod)
bal_tab

library(xtable)
my_tab <- cbind(bal_tab$ks.max.ATT[, c("tx.mn", "ct.mn", "ks")], bal_tab$unw[, "ct.mn"])
names(my_tab) <- c("Treatment", "Weighted Controls", "KS test", "Unweighted Controls")
xtable(my_tab, caption = "Covariate Balance", label = "tab:balance", digits = c(0, 2, 2, 2, 2), 
       align = c("l", "r", "r", "r", "r"))  # paste this into Word document


plot(ps_mod, plots = 2)
##   Standardized effect sizes for covariates
plot(ps_mod, plots = 3)
##   p-values from t-tests for group difference in means
plot(ps_mod, plots = 4)
##   p-values from Kolmogorov-Smirnov tests for group difference in distributions
plot(ps_mod, plots = 5)

###no weights
ps_glm <- glm(health_outcome~flushot6+marital + x.race + educa +income2, 
              data = flu_data_twang, family = "binomial")

summary(ps_glm)

###brfss Weights
flu_data_twang<-flu_sub_t$variables
flu_sub_t$variables$health_outcome<-ifelse(flu_data_twang$physhlth>4&flu_data_twang$physhlth<31,1,0)

mod<-summary(svyglm(health_outcome~flushot6+marital + x.race + educa +income2,design = flu_sub_t,family='binomial'))

mod<-svyglm(health_outcome~flushot6+marital + x.race + educa +income2,design = flu_sub_t,family='binomial')
exp(mod$coefficients*10)-1
################################################################################################################################

###BART
# install.packages('BART')
 library(BART)
library(MASS)
library(BayesTree)
###############################################################################################################################
flu_data_twang<-flu_sub_t$variables
flu_sub_t$variables$health_outcome<-ifelse(flu_data_twang$physhlth>4&flu_data_twang$physhlth<31,1,0)


flu_data_twang<-flu_data_twang[,c('flushot6','marital','x.race','educa','income2',"health_outcome")]
flu_data_twang<-flu_data_twang[complete.cases(flu_data_twang),]

y<-flu_data_twang$health_outcome
xt<-flu_data_twang[,c('flushot6','marital','x.race','educa','income2')]
xp1<-xp2<-flu_data_twang[flu_data_twang$flushot6==1,c('flushot6','marital','x.race','educa','income2')]
xt<-as.data.frame(xt)


xp2$flushot6<-2
xp <- rbind(xp1, xp2)
bart_mod <- bart(x.train = xt, y.train = y, x.test = xp)
#   xt<-unique(complete.cases(xt))
# y<-unique(complete.cases(y))
# xp<-unique(complete.cases(xp))


y
bart_mod$

  
  bart_mod_off1 <- bart(x.train = xt, y.train = y, x.test = xp,binaryOffset = 1)
bart_mod_off2 <- bart(x.train = xt, y.train = y, x.test = xp,binaryOffset = 2)
#############
# Fit BART ##
#############

# BART
bart_mod <- bart(x.train = xt, y.train = y, x.test = xp)

nt<-subset(flu_data_twang,flushot6==1)
nt<-length(nt$health_outcome)

att <- mean(bart_mod$yhat.test[1:nt]) - 
  mean(bart_mod$yhat.test[(nt+1):(2*nt)])
att

#ndraws <- nrow(bart_mod$yhat.test)

tmp <- apply(bart_mod$yhat.test[,1:nt] -
               bart_mod$yhat.test[, (nt+1):(2*nt)], 1, mean)
stdev <- sqrt(var(tmp)) 

# Confidence interval for ATT
c(att - 1.96*stdev, att + 1.96*stdev)

############################################################################################################################

flu_data_twang<-flu_sub_t$variables
flu_sub_t$variables$health_outcome<-ifelse(flu_data_twang$physhlth>4&flu_data_twang$physhlth<31,1,0)


flu_data_twang<-flu_data_twang[,c('flushot6','marital','x.race','educa','income2',"health_outcome")]
flu_data_twang<-flu_data_twang[complete.cases(flu_data_twang),]
y<-flu_data_twang$health_outcome
xt<-flu_data_twang[,c('flushot6','marital','x.race','educa','income2')]
xp1<-xp2<-flu_data_twang[flu_data_twang$flushot6==2,c('flushot6','marital','x.race','educa','income2')]
xt<-as.data.frame(xt)


xp2$flushot6<-1
xp <- rbind(xp1, xp2)
bart_mod_ATNT <- bart(x.train = xt, y.train = y, x.test = xp)


nt<-subset(flu_data_twang,flushot6==2)
nt<-length(nt$health_outcome)

att <- mean(bart_mod_ATNT$yhat.test[1:nt]) - 
  mean(bart_mod_ATNT$yhat.test[(nt+1):(2*nt)])
att



tmp <- apply(bart_mod_ATNT$yhat.test[,1:nt] -
               bart_mod_ATNT$yhat.test[, (nt+1):(2*nt)], 1, mean)
stdev <- sqrt(var(tmp)) 

# Confidence interval for ATT
c(att - 1.96*stdev, att + 1.96*stdev)


####################################################################################################################################
##TMLE
####################################################################################################################################


###############################
flu_data_twang<-flu_sub_t$variables
flu_sub_t$variables$health_outcome<-ifelse(flu_data_twang$physhlth>4&flu_data_twang$physhlth<31,1,0)
flu_data_twang<-flu_data_twang[,c('flushot6','marital','x.race','educa','income2',"health_outcome")]

flu_data_twang<-flu_data_twang[complete.cases(flu_data_twang),]
flu_data_twang$flushot6<-ifelse(flu_data_twang$flushot6==1,1,0)

TMLE2 <- tmle(Y = flu_data_twang$health_outcome , A = flu_data_twang$flushot6, 
              W = flu_data_twang[,c('marital','x.race','educa','income2')],
              family = "binomial")



summary(TMLE2)
TMLE2$estimates$ATE


TMLE2 <- tmle(Y = ObsData$Y, A = ObsData$A, 
              W = ObsData[,c("w1", "w2", "w3", "w4")],
              family = "binomial")
ATEtmle2 <- TMLE2$estimates$ATE$psi
ATEtmle2
TMLE2$estimates$ATE$CI
ATEtmle1
True_ATE

MORtmle2 <- TMLE2$estimates$OR$psi
MORtmle2
TMLE2$estimates$OR$CI
MORtmle1
True_MOR




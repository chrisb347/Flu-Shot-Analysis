# Flu Shot Analysis

This set of files belongs to a working paper I wrote with a professor while taking his observational studies class. The data set for this paper comes from the Behavioral Health Risk Factor Surveillance System (BRFSS). This is a survey that is conducted by the CDC where the goal is to collect information about the US population about their health behaviors, for example: "Do you smoke?" or "Do you exercise?". An example of a question can be seen below. 

![Alt text](/images/flu_survey_example.PNG?raw=true "Optional Title")

## Study Design & EDA

After observing the survey question above, I wanted to create an observational study design that would allow me to test differences between populations of recipients of flu shot vaccinations and non recipients. The working hypthesis was that there would be differences when we looked across different income groups, races, education level, marital status, or race. The first obstacle was the outcome, the BRFSS provides flu shot data (whether or not someone received a flushot) but the survey does not provide whether or not the flushot actually worked. Data like this may be difficult to actually obtain without doing clinical studies, however it is possible to create a proxy of this outcome with some assumptions. The BRFSS does collect detailed information of contraction and existing diseases in the survey's particiants. The BRFSS asks participants if they have been ill in the past 30 days, and this binary variable was the variable of interest. The survey was filtered to remove participants who had an chronic illnesses, in addition a filter was applied to only accept responses within the CDC defined flu season time period. Below code is written that shows the percent difference between groups that were administered flu shots, and were not to see if there were differences by any of the demographic categories. 

```

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
```
![Alt text](/images/EDA.PNG?raw=true "Optional Title")


## Modeling



```

###BART
# install.packages('BART')
 library(BART)
library(MASS)
library(BayesTree)
###############################################################################################################################
##data processing
flu_data_twang<-flu_sub_t$variables
flu_sub_t$variables$health_outcome<-ifelse(flu_data_twang$physhlth>4&flu_data_twang$physhlth<31,1,0)


flu_data_twang<-flu_data_twang[,c('flushot6','marital','x.race','educa','income2',"health_outcome")]
flu_data_twang<-flu_data_twang[complete.cases(flu_data_twang),]

y<-flu_data_twang$health_outcome
xt<-flu_data_twang[,c('flushot6','marital','x.race','educa','income2')]
xp1<-xp2<-flu_data_twang[flu_data_twang$flushot6==1,c('flushot6','marital','x.race','educa','income2')]
xt<-as.data.frame(xt)

##ATE
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
##ATT
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
##ATNT
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



```
Here is a snippet from my paper on how BART works

## Bayesian Additive Regression Trees (BART) 

To estimate the outcome, of whether or not a participant was sick or not sick the modeling method of Bayesian Additive Regression Trees was employed. This method uses a conditional distribution of the treatment either flu shot vaccination or no flu shot vaccination, and the covariates, with an additive error term attached to predict the Y response values. This conditional distribution is dependent on an ignorability assumption that assumes values of Y are orthogonal to values of treatments given individual values of covariates. 

BART begins by separating treatment and nontreatment groups. Once this is done it uses the covariates to establish cut points which are represented by the different branches of the trees. Once the cut points are established a final stepwise function to fit the data for different values of covariates. Within BART there is a regularization parameter that helps control 1 variable from contributing too much to the fit when trees are constructed. This regularization parameter also ensures there are not too many nodes in the tree to fit the model. In order to find the model which has the best fit, BART uses a Monte Carlo Markov Chain to search through different tree parameters and variance to minimize the residual variance. Once a model is fit for an individual tree, this model explanatory power is subtracted from the initial y value. This model generates residuals and the next model is fit on the residual values only, treating these residual values as the y value. Once all of the sub trees have been fit,they are added up to give the yhat prediction value from BART. The idea is that weak learners do not perform on their own but adding them up together can provide accurate estimates. The individual tree contributions are not able to be realized by the model because the joint probability remains fixed at the initial tree calculations, however the tree structure is subject to change when each subsequent tree is calculated.  

![Alt text](/images/bart.PNG?raw=true "Optional Title")

## Results

BART 

The results that are output from BART include the train data predicted values and the test data predicted values for y. These values are calculated using a probit connection function because they are binary and this is the default function for BART when considering a binary outcome. Typically in medicine the group of interest is the experimental group or the treated group. Within this particular study, the treatment group is less interesting because there is fairly substantial belief in the medical community that flu shots are helpful preventative measures when attempting to lower a patient's chances of contracting the flu. Surprisingly the average treatment effect (ATE) is quite small, smaller than 0.01, between treated and non treated individuals in this study. This does not necessarily mean that flu shots do not work. With multiple covariates, they may have different treatment effects, exploring the average effect of treatment on the non-treated (ATNT), average treatment against the treatment (ATT) and the heterogeneity treatment effects (HTE) will give more insight into differences between covariates in this sample.  

A potentially interesting method to view results is building the model, and constructing a holdout sample of the data, where the holdout dataset is two duplicate datasets of the observations that received either a treatment or no treatment stacked on top of each other. However the duplicate would have the opposite value assigned to the duplicate observations. This will allow the researcher to view the counterfactual prediction that the model would yield. 

 In this study, the ATT is small, because giving participants no flu shot who did receive a flu shot’s health outcome could easily be explained by other factors. People who receive flu shots could practice healthier lifestyles in general and live in communities of others who also receive flu shots. This would drastically reduce their risk of contracting the flu. The ATNT is significantly more interesting, because this taxonomy will vaccinate participants that did not receive a vaccination. However this treatment effect was found to be small, less than 0.01.  

The next window to view the ATNT effects through is the heterogeneity treatment effect. This will take the ATNT from the non vaccinated observations that were duplicated and  assigned a treatment, and subtract them from a non treatment. Then the individual differences are a participant level can be observed if they had or had not received a vaccination. All of the predicted values in this model were negative and in this probit model framework, a large absolute negative value is associated with a lower probability of sickness. For example if the no vaccination group is larger than the counterfactual, when the two values are subtracted from each other respectively a negative value will be obtained. A boxplot is a good tool for noting the differences between these differential values, and was used to identify any differences in distributions for covariates. For many covariates there is no discernible difference. The only difference that can be seen somewhat clearly is the race covariate. Below it can be seen that the boxplot for hispanic participants is higher than the others. Positive differences mean that counterfactual flu shot that was administered had a positive effect on health outcome. Another way to say this is that if a person who did not receive a flu shot, had actually received that flu shot they would have decreased their chances of contracting an illness. A potentially interesting suggestive finding of this study is that hispanic and black populations have a slightly positive skewed distribution. This means that these races may have benefitted from receiving a flu shot. The tree cut points help account for differences among the covariates, so these racial differences are independent of marital status, education level, and income. Potential sources of these differences could be racial biases that exist in the medical system or cultural characteristics that may influence less healthy lifestyles, or less vaccination.  These are all difficult population characteristics to collect data on, and these findings remain solely speculative. 

![Alt text](/images/race.PNG?raw=true "Optional Title")



















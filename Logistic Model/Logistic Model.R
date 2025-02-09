#Homework 4
#Name: Alessio Baldini
#Student ID: 617281

library(car)
library(sandwich)
library(lmtest)
library(readr)
library(AER)
library(stargazer)
library(bucky)

setwd(choose.dir())
getwd()
baldini<-read.csv("Data.csv")

#a) Create the new variable dambexp
dambexp<-ifelse(baldini$ambexp>0, 1, 0)
pos_exp<-mean(dambexp)

#We want to create a binary variable starting from the variable ambexp which 
#associates to each positive value the number 1 and to any non positive value 
#the number 0. So that we have create the variable dambexp, that is dummy (made 
#of 0,1) and such that to the value 1 corresponds a positive ambexp.
#Since it is dummy and constituted of 0 and 1, the sum of all the values give the
#number of individual with positive values of ambexp, while doing the ratio
#between the sum and the total number of individuals (which in fact is just a 
#decomposition of the mean of dambex in two stages), we get the share of
#individuals with positive expediture. This holds because dambexp takes value 
#0 and 1, otherwise we couldn't simply consider the sum as the number of 
#individuals with positive expenditure.

tot_pos_exp<-table(dambexp)[2]
pos_exp_2<-tot_pos_exp/(nrow(baldini))

#This is an alternative way to count the positive expenditure share through the
#function table that gives the amount of individuals associated with value o and 1.

#b) Estimate a LPM

lmp<-lm(dambexp ~ totchr+age+educ, data=baldini)
summary(lmp, vcov=vcovHC)
coeftest(lmp, vcov=vcovHC)
linearHypothesis(lmp, c("totchr","age","educ"), vcov=vcovHC)
AME_totchr_lpm<-(mean(lmp$coefficients[2]))

#We can create a linear model that represents the probability of the event dambexp=1.
#We test the null hypothesis that the coefficients are zero by using the function 
#coeftest (or looking at the summary), and we see that the p-values are very low
#for all the coefficients, so that we reject the null hypothesis for all the
#coefficients (almost 0). We do not reject the null hypothesis for the intercept.
#We test also the null hypothesis that coefficients are all equal to zero with
#the linear.hypothesis function and since the p-value is very low we reject it.
#When we test parameters in this framework, we need to do computation taking care of
#using tests robust to heteroskedasticity, since in the linear probability model
#the error term is always heteroskedastic by construction, whenever coefficients
#are not all equal to zero.
#In this model we can interpreter results as in the standard Ols: since the model
#is lin-lin, then positive coefficients can be interpreted as the increment of 
#the probability of positive expense when the variable associated to the parameter 
#increases by 1 unit. The intercept has not a direct interpretation, since when 
#all the parameters are zero, in particular age, the probability of expense should 
#be zero, by definition. Moreover, we can see from the dataset that the minimum
#value of age is 21 so that we don't consider lower ages. This can cause a omitted 
#variable bias in the computation of the parameters as well as the fact that the
#maximum age is 64, not considering those individuals for which the ambulatory 
#medical expenses could be high and for which chronic diseases are often relevant.


#c) Fitted values
dambexp_fitted<-lmp$coefficients[1]+lmp$coefficients[2]*baldini$totchr+lmp$coefficients[3]*baldini$age+lmp$coefficients[4]*baldini$educ
dambexp_fitted_aut<-fitted(lmp)
summary(dambexp_fitted)

#We can evaluate fitted values both by using the function fitted and applying
#the definition: the vector dambex_fitted is built by putting the values of educ,
#age and totchr in the estimated linear model. We get the vector of fitted dambexp
#and by using the summary we can evaluate it. In particular using the function 
#summary we can see the limit of the linear model applied to a binomial variable
#to evaluate the probability of a outcome: while probability takes value between
#0 and 1, we can see that the maximum of fitted values is 1,475, which can't 
#represent the probability properly. However, we can also see that up to the third 
#quartile values are below one: this result can be explained by the fact that
#linear models represents a good estimate of parameters for values of the independent 
#variable near to the centre of their distribution, but may be useless for extreme 
#values. Moreover, in general, the lpm approximates a behavior that is not always
#linear, so that it can bring to underestimate or overestimate the size of the marginal
#effects, that in this framework are evaluated by the coefficients.

#d) Probit model
probit <- glm(dambexp ~ totchr+age+educ, data=baldini, family=binomial(link = "probit"))
summary(probit)

#Since the linear probability model is not very representative, we could use the 
#probit model to estimate the probability that dambexp is positive. We create the
#model using the function glm. 
#With probit and logit we assume homskedasticity of the error term, so that we can
#use the classic formula for the summary. We test the null hypothesis that the 
#coefficients are equal to zero, and we reject it since p values are almost zero
#for all the coefficients. Also in this case we can avoid to consider the intercept
#in the analysis. In this framework we cannot interpret directly the coefficients 
#of the regression, but we can study their sign: since all the parameters are 
#positive, we can just conclude that if the independent variables increase so does
#the probability given by the model (ind. varibales have positive effect on 
#the probability of having positive ambexp).
#In particular, the fact that totchr has positive effect on the probability of
#having positive ambulatory expense seems to be natural: a person with chronic
#diseases requires more visits and assistance so that the person has to spend more,
#especially in the US where the Health System is expensive and in large part private.

#In general, if homoskedasticity doesn't hold the model is inconsistent.


#e) AME and MEM of probit
lin_pred_probit<-probit$linear.predictors
ME_totchr_probit<-dnorm(lin_pred_probit)*coefficients(probit)["totchr"]
AME_totchr_probit<-mean(ME_totchr_probit)
MEM_totchr_probit<-dnorm(mean(lin_pred_probit))*coefficients(probit)["totchr"]
MEM_totchr_probit

#We have applied the formulas to get the Ame and the Mem. Linear predictors are
#used to compute the x*beta we have seen in the formula presented during lessons.
#As we said before, the parameter itself has no interpretation in magnitude effect.
#By evaluating these two parameters we can get also the effect in terms of value
#of a change of totchr on predicted probability and not only in term of sign.
#The MEM gives the marginal effect of the mean value of linear predictors, while 
#the AME gives the average of the marginal effects of totchr evaluated for each observation.
#We can see that they are very similar, in fact they are just alternative ways of 
#evaluating the same effect. We could interpret the MEM as the effect of a change 
#in mean of the value of the independent variable on the predicted probability,
#while the AME gives the mean effect of a unitary change of the independent variable.

#f) Goodness of fit and share of correctly predicted outcomes
lrtest<-probit$null.deviance-probit$deviance
pchisq(lrtest, df=3, lower.tail = F)

LLModel<-logLik(probit)[1]
LLNull<- -(lrtest/2 - LLModel)

Pseudo_R2_probit<-1-(LLModel/LLNull)
logLik(probit)

#To evaluate the goodness of the model we first compute the likelihood ratio test.
#If the constraints are good, then the likelihood ratio test should be small.
#In fact, the null model is built considering all the coefficients 0, except of
#the intercept. So that, if the two likelihood are almost equal, and the lrtest
#almost zero, it means that restrictions are valid and our model is not good.
#Knowing that lrtest is distributed like a chi-square with 3 degrees of freedom 
#(they are the number of constraint, which are the null coefficients) we can compute
#the pvalue and since the results is almost zero we can reject the null 
#hypothesis that all the coefficients except the intercept are zero.
#Alternatively we can use the adjusted R-squared to evaluate the model: in this 
#case, we are comparing the ratio between the likelihoods. This means that if
#the two likelihoods are equal and our model is not good, then the ratio equals to
#1 and the pseudo R-squared (McFadden method) which is 1 minus the ratio, equals
#to zero. In our case,the result is very close to zero, which means that the two 
#models are quite similar, so that the estimated model is not very good, even if 
#the null model has been rejected by the previous test.

phat_probit<-probit$fitted.values
yhat_probit<-(phat_probit>0.5)
(t <- prop.table(table(yhat_probit,dambexp)))
correct_predict_probit<-t[1,1]+t[2,2]
correct_predict_probit

share_pos_exp<-pos_exp
share_neg_exp<-1-(pos_exp)

test_1<-t[1,1]+t[2,1]
#To evaluate the goodness of the model we could use also the percentage of correctly
#predicted outcome. We can see from the results that we estimated correctly the 
#outcome in the (almost) 74% of the cases. We could compare this measure with the 
#percentage of success for a model that always predicts 0 as result (test_1=27%).
#We see that the probability of success of the simplified model is far lower than 
#the one of our model. This gives no information about the goodness of our model:
#we could have said that the model was inefficient if the naive model had higher
#probability, but we cannot say anything in this case.

#g) Logit Model
logit <- glm(dambexp ~ totchr+age+educ, data=baldini, family=binomial(link = "logit"))
lin_pred_logit<-logit$linear.predictors
ME_totchr_logit<-dlogis(lin_pred_logit)*coefficients(logit)["totchr"]
AME_totchr_logit<-mean(ME_totchr_logit)

stargazer(AME_totchr_logit, AME_totchr_probit, AME_totchr_lpm, type="text")

#We evaluate also the logit model since it is an alternative model to the probit
#and it is often used as it is easier to compute.
#As we can see, comparing the three models, probit and logit give similar results 
#for the AME, and this is quite understandable from the fact that they have similar
#behavior, especially in the centre of the distribution of the independent variable.
#The linear model differs because it approximates the model differently with
#the problems we exposed before, and since its Ame is lower we could say that 
#it underestimate the effect.

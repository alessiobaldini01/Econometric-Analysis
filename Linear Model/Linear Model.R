#Homework 3
#Name: Alessio Baldini
#Student ID: 617281

library(car)
library(sandwich)
library(lmtest)
library(readr)
library(AER)
library(stargazer)

setwd(choose.dir())
getwd()
baldini<-read.csv("baldini.csv")


#Question 1 - OLS

ols1<-lm(children ~ educ+age+I(age^2), data=baldini)
summary(ols1)

#a) Discussion about coefficients
beta_intercept_1<-ols1$coefficients[1]
beta_education_1<-ols1$coefficients[2]
beta_age_1<-ols1$coefficients[3]
beta_sqrage_1<-ols1$coefficients[4]

APE<-mean(beta_age_1+2*beta_sqrage_1*baldini$age)   #applying deifnition of APE
APE2<-beta_age_1+2*beta_sqrage_1*mean(baldini$age)  #applying properties of excpected value
turning_point_1<-(-beta_age_1)/(2*beta_sqrage_1)

#We are studying a linear model where, in general, the coefficients (at least those
#referring to linear variables) represent the partial effect of the independent variables
#on the dependent variable. 
#In our case we have the dependent variable (children) and in the first stage we do
#the regression using education and age. In particular, we can see that age has both
#a linear and a non-linear component. This affect the way we interpret parameters beta:
#-intercept: it shows the presence of a basline component, which has not direct meaning
#in the regression (the direct interpretation would be that the number of child when
#all the parameters are null is negative, which is clearly unreasonable). We could
#imagine that the intercept in our case takes count of possible measurement errors
#or inefficiencies of the regression model, which could be not properly defined;
#-education: the parameter of educ represents the partial effect and, since the model
#is linear, it shows the marginal effect of the education on children or, in other words,
#how much the variable children varies when the level of education increase by 
#one unit. In this case the partial effect is negative so that higher level of education
#are associated with a lower number of children;
#-age: the coefficient of age doesn't represent the partial effect of age since it is only the
#linear component of the variable age. It represents the linear contribution to 
#the total effect of age, and in this sense we can see that it is positive (as age increases
#so does the number of children) but we need also to analyze sqrage and the APE;
#-sqrage: this coefficient gives more information about the impact of age. In fact it is negative,
#which means that even if children is positively affected by age (according to the
#previous coefficient), this impact reduces in terms of magnitude over time. 
#In other words, as the age increases the number of children increases non linearly 
#less than proportionally up to the turning point, that gives us the age at which the 
#effect of the age on the number of children is the highest. It is basically 
#the vertex of the parabola described by the linear combination of the terms where
#the variable age appears. These results about age are coherent with the fertility 
#of women, that reduces overtime until menopause and with the fact that the older 
#the woman is, the more children she could have had in the previous years.
#-APE: to better estimate the effect of age on children we need to use the average
#partial effects that gives us the expected partial effect of the combination
#of the two components (linear and non linear);

#b) Significance of coefficients
summary(ols1)
coeftest(ols1)                    #same results of summary
coeftest(ols1, vcov=vcovHC)       #robust formula

#We evaluate the significance of the parameters in case of homoskedasticity and 
#using the robust formula. As we can see, there is no difference between using 
#the command summary or coeftest when evaluating for homoskedasticity and, in all
#the cases, all the parameters are statistically significant since we reject the 
#null hypothesis for all the coefficients (the null hypothesis run by these tests 
#are that coeffcients are zero), given by p-values close to 0 or the(***).
#We could exclude the presence of heteroskedasticity in the model since the results 
#are quite similar both using the classic formula and the robust one;
#Coeftest and summary uses t as test statistic, distributed a student's t.

#c) Test for the significance of age on children
linearHypothesis(ols1, matchCoefs(ols1, "age"))

#While in the previous point we have analyzed the parameters individually, when 
#we consider age as a unique variable (with a linear and non linear component),
#we need to assess that both the coefficients are different from 0 at the same time.
#This requires the use of linearHypothesis command that, differently from coeftest,
#tests the parameters jointly. Also in this case the null hypothesis is that 
#parameters are jointly zero and the distribution of the test statistic is F. In this case
#we can see that the p-value is close to zero enough (2,2e-16) to reject the null hypothesis
#(also in this case we could alternatively note the (***) which assesses the statistical 
#significance of the parameters). Therefore, according to the test we can assess 
#the significance of the variable age in the model.

#d) Regression with parameter for urban
ols2<-lm(children ~ educ+age+I(age^2)+urban, data=baldini)

beta_intercept_2<-ols2$coefficients[1]
beta_education_2<-ols2$coefficients[2]
beta_age_2<-ols2$coefficients[3]
beta_sqrage_2<-ols2$coefficients[4]
beta_urban_2<-ols2$coefficients[5]

APE<-mean(beta_age_2+2*beta_sqrage_2*baldini$age)   #applying definition of APE
APE2<-beta_age_2+2*beta_sqrage_2*mean(baldini$age)  #applying properties of expected value
turning_point_2<-(-beta_age_2)/(2*beta_sqrage_2)

#I did the regression adding the variable urban as regressor.
#We can attribute to the coefficients the same results as before in terms of
#partial effect or average partial effect. In this case we added the variable 
#urban and we can consider its coefficient as a partial effect, so that we can
#see it has negative effect on the number of children (-0,387). In this case, living
#in urban areas shows a negative effect on the number of children. This may be due
#to higher cost of living but also to larger opportunities in terms of jobs and
#education for women, that could prefer to focus on their career rather than having
#many children. Moreover, women who live in the countryside are often members of 
#families which have farms or work as farmers, so that having many children could 
#be an advantage for the entire family;

#e) Significance of urban coefficient
summary(ols2)
coeftest(ols2)
coeftest(ols2, vcov=vcovHC)

#As we have seen before, we run the test on coefficients individually using both 
#the formula under homoskedasticity and robust to heteroskedasticity. The results
#also in this case are quite similar (we could exclude heteroskedaticity), and
#for all the coefficients, including urban, we reject the null hypothesis that
#coefficients are zero (we can both look to p-values, that are close to zero,
#or to the symbol (***) as before). The test statistic is distributed as student's t.
#In case of homoskedasticity we can also evaluate the statistical significance 
#using the function summary.

#f) Comparison of the two models
stargazer(ols1, ols2, type="text")

change_beta_intercept<-(beta_intercept_2-beta_intercept_1)/(abs(beta_intercept_1))*100
change_beta_education<-(beta_education_2-beta_education_1)/(abs(beta_education_1))*100
change_beta_age<-(beta_age_2-beta_age_1)/(abs(beta_age_1))*100
change_beta_sqrage<-(beta_sqrage_2-beta_sqrage_1)/(abs(beta_sqrage_1))*100

cov_urb_educ<-cov(baldini$educ,baldini$urban)

ols_OV<-lm(urban ~ educ+age+I(age^2), data=baldini)
summary(ols_OV)

#We can empirically assess the presence of an omitted variable bias comparing the 
#results about coefficients:adding the variable urban to the model there is a 
#difference in the estimate of the parameter education (almost 12%).
#When there is an omitted variable bias, the ols becomes biased and inconsistent: 
#as we can see with our data, adding the variable urban to the model, there is a 
#change in the coefficient of education and this may be due to the bias 
#coming from the exclusion of the variable urban, which is significant for the 
#number of children, in the model ols1 (pvalue=3,134e-05).
#In general, the direction of the bias of a coefficient depends on the correlation 
#between the dependent variable and the omitted one and on the covariance between the 
#omitted variable and the variable that refers to the biased coefficient: if their
#product is positive, the bias is positive and if it is negative so is the bias. 
#We can see that in our case the covariance (urban,edcu) is positive. Doing the linear 
#regression of all the variables on urban, we can see from the summary of the 
#regression (summary(ols_OV)) that education is significant for urban and the 
#coefficient is positive (0,03495), giving to the same results for the sign of 
#the bias we obtained using the covariance. However, the variable urban negatively
#affects children, so that the bias should be negative, since it signs is affected
#by the product between the cov_urb_educ (positive) and the parameter of urban 
#on children (negative). Our empirical analysis is coherent with theoretical 
#results: omitting urban, the beta_education is underestimated, so that the sign
#of the bias is negative (when urban doesn't appear in the regression, the
#coefficient of beta_education is lower),



#Question 2 - IV

#a) Discussion on instruments validity
#If we suspect that education is endogenous, so that it is correlated to the
#error term in the model, we could find some variables that may describe it.
#In general an instrument is valid when it is both an exogenous variable (not
#correlated to the error term) and relevant (strongly related to the endogenous variable).
#If we compare firsthalf and bicycle, in particular looking at the relation with
#the education, I think that bicycle is a more valuable instrument: while the 
#fact that the woman is born in the first half of the year doesn't tell so much 
#about the level of education, bicycle can describe it better, especially in poor 
#countries where public transports are not affordable or neither present and having 
#a bicycle can make the difference in order to be able to move from countryside 
#towards city centers, where schools are usually located. For sure it isn't the 
#only explanatory variable of the level of education, but it may be a useful 
#instrument to evaluate it.
#To evaluate the exogeneity of the instruments we need to test them, so that we
#cannot say anything in advance.

#b) Test on instruments validity
#To test if one variable is a valid instrument we need that it is both exogenous
#and strongly relevant. So that we first evaluate relevance and then we do Sargan test
#to evaluate exogeneity. First I do regression of x on all the exogenous variable 
#and all the instruments (it is the first stage of 2SLS method), evaluating the 
#significance of the coefficients resulting, and then we perform the F-test.

st1_b<-lm(educ ~ age+I(age^2)+urban+frsthalf+bicycle, data=baldini)
coeftest(st1_b, vcov=vcovHC)                                   #we use robust formula
linearHypothesis(st1_b, c("frsthalf", "bicycle"), vcov=vcovHC) #we use robust formula

ivreg_b<-ivreg(children ~ educ+age+I(age^2)+urban | frsthalf+bicycle+age+I(age^2)+urban, data=baldini)
summary(ivreg_b, diagnostics=TRUE)

#Instruments are valid if they are relevant and exogenous:
#-Significant: we can see from the coeftest that both frsthalf and bicycle have p-value
#close enough to zero (respectively 9,32e-06 and 4,791e-05) to reject the null hypothesis (coefficients individually 
#equal to zero). We could also consider the significance code (***);
#-Relevant: performing the linearHypothesis test, we have seen that their p-value is close enough
#to 0 (4,916e-08) to reject the null hypothesis that they are both equal to zero, so that they
#are relevant and since F value 17>10, they are also strong instruments;
#In coeftest the test statistic is distributed ad student's t, while in F test
#as Fisher's F.
#-Exogenous: we are also interested to check if they are both valid or if there are some
#not exogenous instruments. To check this, we use the Sargan test, using the
#ivreg function.

#We can see in the summary that the Sargan test strongly rejects the null hypothesis (pvalue=8,74e-05).
#In the Sargan test the null hypothesis is that the instruments are jointly exogenous, 
#so that in our case we can't say that the instruments are both exogenous and then are not valid.
#However, through this test we can't neither assess which instrument is valid and
#which one is not exogenous.
#In this case the test statistic is distributed as a chi-square with df=2.

#c) Frsthalf relevant instrument
#We can test if it is Relevant as we did in the previous point, but considering 
#frsthalf the only instrument.
st1_c<-lm(educ ~ age+I(age^2)+urban+frsthalf, data=baldini)
coeftest(st1_c)
summary(st1_c)
linearHypothesis(st1_c, c("frsthalf"))

#As we can see in the results, coeftest tells that the coefficient of frsthalf
#is statistically significant (pvalue=1,845e-05) and since it is the only instrument 
#it is also relevant. So that frsthalf and education have a correlation (we reject 
#Ho for which parameter's value is zero), and the F test says that the instrument 
#is strong (18,475>10).

#d) 2SLS Model
st1_d<-lm(educ ~ age+I(age^2)+urban+frsthalf, data=baldini)
educhat<-fitted(st1_d)
st2_d<-lm(children ~ educhat+age+I(age^2)+urban, data=baldini)
summary(st2_d)

#We estimate the coefficients of the model using the 2SLS Model.
#About validity of frsthalf: we can consider frsthalf as a valid instrumental variable
#since it is relevant (from previous test) and endogenous (not correlated to the 
#error term) by assumption, 
#Note that if the last assumption doesn't hold we cannot say anything about the
#validity of the test since we cannot assess its exogeneity using Sargman test
#because it is not a over-identified case(Sargan test is valid only in over-identified case).

ivreg_d<-ivreg(children ~ educ+age+I(age^2)+urban | frsthalf+age+I(age^2)+urban, data=baldini)
summary(ivreg_d)
#This application is just to show that the same coefficients of the model can be
#obtained running the method of moments instead of the 2SLS. Note that the two models
#are equivalent for what concerns the estimate but not for s.e. and tests.

#e) Hausman Test for educ endogeneity
#We want to test if the variable education is endogenous or not. We are assuming 
#as null hypothesis that educ is exogenous. If we reject Ho we should use IV/2SLS,
#to run regressions as OLS would be biased and inconsistent, while if we don't 
#reject the null hypothesis, the OLS is the best choice.
#We use the result of the previous point. Then we evaluate residuals, which represents
#the endogenous part of the model, running the linear regression using residuals
#as a regressor of children:
st1_d
vhat<-residuals(st1_d)
ols_hausman<-lm(children ~ educ+age+I(age^2)+urban+vhat, data=baldini)
summary(ols_hausman)         
linearHypothesis(ols_hausman, c("vhat"))

#Once we have run the regression we test coefficient of the residuals (vhat). 
#In this case the p-value (obtained with the summary) is 0,1296 so that we don't 
#reject the null hypothesis. The test statistic in this case is distirubted as 
#student's t.
#We can also use the command linearHypothesis, valid in presence of more coefficients,
#and the p-value is the same, so that also in this case we don't reject H0.
#With linear hypothesis the test statistic is distributed as Fisher's F.
#Since we don't reject Ho, we don't have statistical evidence for the fact that
#education is endogenous: we can't assess endogeneity of education.

#f) Preferred specification
#During the previous points we got the following results:
#-validity of frsthalf: the variable frsthalf is a valid instrument for education
#if assumed as exogenous;
#-non validity of frsthalf and bicycle: according to the Sargan test, the joint
#use of these iv is not valid;
#-non-endogeneity of education: the result of Hausman test in the last point
#gives statistical evidence against the hypothesis of endogeneity.
#According to the Hausman test it could be reasonable to think the ols is a better
#model than IV. However, we need to notice that in the first exercise there was
#a relevant bias in the coefficient of education that cannot be ignored. 
#Since we cannot be sure about the exogenenity of education and empirical data
#seem to enforce the idea of endogeneity, we could still use instrumental variables,
#but choosing more than one. In the second exercise frsthalf and bicycle seemed
#to be valid instruments, but their use is not compatible with the model according
#to the Sargan test. Using more instruments could lead to better analysis of their 
#exogeneity as we could test with Sargan's test (we have assumed it on frsthalf 
#but not tested), and it could explain better the endogeneity of education. 
#If we think to Africa, evidently the possibility to live in urban areas is both 
#a cause and a consequence of the education level, but it is not the only 
#explanatory variable. Also bicycle, at least theoretically, could be a good 
#instrumental variable for its implications on education. There could be many 
#others and the use of many iv a could enforce the quality of the model.

#To conclude, we cannot avoid to consider the strong possibility of presence of 
#omitted variables in the model, and even the possibility that a model such this
#one is not qualitatively representative of the phenomenon of maternity in Africa, but
#I think that among those anlyzed, the 2SLS, using more variables, could lead to
#the best qualitative analysis and to better explaining results.

## Setup

### Load packages

library(ggplot2)
library(bootstrap)
library(mediation)
library(bda)
library(ggpubr)
library(statsr)
library(dplyr)


### Load data


getwd()
load("C:/Users/sanjay/Desktop/Extras/_5db435f06000e694f6050a2d43fc7be3_gss/gss.Rdata")


## Part 1: Data

##Generalizability :

# - The GSS sample is drawn using an area probability design.
# - Using NORCs sample frame of United States addresses, a mix of urban, suburban, and rural National Frame Areas (NFAs) are scientifically selected to represent the nation. 
# - As of 2012, a sample with 57061 respondents and 100+ variables have been collected.
# - Random Sampling was used for the better understanding of the overall US population with certain conditions like to interview/ conduct survey only after 3:00 pm on weekdays  or on during the weekends or holidays to overcome the biases mainly due to not-at-homes respondents.

# Causality: 

# Random assignments were not used. 


## Part 2: Research question

# Black_Lives_Matter
# 
# Based on the incident happened on May 25, 2020 at Minneapolis, US centering the death of the victim named George Floyd, an African-American while being detained by the white american cop. I preferred to study upon the racial discrimination of the African-Americans in US from 1975 to 2012  
# 
# The study will focus on the Variable heads such as,
#   racdif1 <- Differnces due to discrimination 
#   polviews<- Political views of the Respondents
#   race <- To what race the Respondents belongs to 
#   joblose <- Is R likely to lose job
#   degree<- Respondents highest degree
# 
# 
# Two research questions have been analysed, 
# 
# 
# 1. How does the political stance of a respondent influence their acceptance of racial discrimination ? 
# 2. Is the degree alone responsible for the job loss or does race of the respondent act as a mediator  ? 




## Part 1.3: Exploratory data analysis

##1.3- Research Question 1 | polviews-Political Views and racdif1-Differences due to racial discrimination


gss%>%
  filter(!is.na(polviews) &!is.na(racdif1))%>%
  select(polviews, racdif1)-> gss_racpolyea
ggplot(gss_racpolyea, 
       aes(x = polviews, 
           fill = racdif1)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")+
  ggtitle("Conservatives & Liberals on the existence Racial Discrimination")

# The above stacked bar plot represents the proportion of acceptance of racial discrimination across various political stance of the US respondents 
# 
# observation:
# 
# Liberal viewed respondents (mostly Democrats) proposes that there exists strong racial discrimination in US, whereas Conservative viewed respondents(mostly Republicans) proposes there exists very little racial discrimination
# 
# ## 1.4 Inference :
# 
# Null Hypothesis: Political stance of an respondent is not related on figuring out the presence of racial discrimination in US
# 
# Alternate Hypothesis: Political stance of an respondent is related on figuring out the presence of racial discrimination in US
# 
# Conditions:
# 1. The observations are independent, GSS specified that the data is through random sampling and that the records are believed to be independent
# 
# 2.The variable under study are categorical, Polview and racdif1 are categorical variables
# 3. Expected counts: Every cell must posses an expected count more than 5
# 
# Method:
# chi-square goodness of fit and Independence test 
# 
# 
# chisq.test(gss_racpolyea$polviews, gss_racpolyea$racdif1)$expected
# 
# 
# The above exected Chi-squared goodness of fit clarifies that the expected count on every cell is above 5
# Now, moving on to check the chi-squared test of Indepence,
# 
# 
# chisq.test(gss_racpolyea$racdif1, gss_racpolyea$polviews)
# 
# P-value is much less than 0.05, hence forth we reject the Null hypothesis and conclude that the Political stance of the respondent is related on figuring out the presence of racial discrimination in US.
# 
# Ads were made on Republican's( Conservative ) like George W.Bush and Donald J Trump calling them as racist at several occasions. So, itseems like where there is "No Racism" there actually is "Racism".
# 
# 
## Part 2.3: Exploratory data analysis

## 2.3 Exp| Research Question-2, jobloss, degree, race


gss%>%
  filter(!is.na(race)&!is.na(joblose)&!is.na(degree))%>%
  select(race, joblose, degree)-> gss_racjobdeg
gss_racjobdeg[sapply(gss_racjobdeg, is.factor)] <- data.matrix(gss_racjobdeg[sapply(gss_racjobdeg, is.factor)])
model.m <- lm(race~degree, gss_racjobdeg)
model.y <- lm(joblose ~ race + degree, gss_racjobdeg)
effect1<-mediate(model.m, model.y, treat = "degree", mediator = "race", boot = TRUE, sims = 50)
summary(effect1)



##2.4 Inferences:

# Null Hypothesis : Race of the respondent does not significantly mediates the relationship between Degree of the respondent(independent variable) and Job lose (dependent variable)
# 
# Alternate Hypothesis : Race of the respondent significantly mediates the relationship between degree of the respondent(independent variable) and Job lose (dependent variable)
# 
# Conditions:
# 
# 1. The observations are independent, GSS specified that the data is through random sampling and that the records are believed to be independent
# 2. The sample size is more than 30, here it is 14,894
# 
# Methods:
# 
# Did the Bootstrap causal mediation analysis test to identify if there existed any mediation effects due to the race of the respondent in losing a job despite his degree. As the sample size was large, bootstrap method was considered the right choice
# 
# For simplification, the categorical variables were converted into numerical variables by using data.frame and is.factor functions. 
# After converting the factor variables into numerical variables linear regression models model.m, model.y are constructed, in such a way that model.m predicts the mediator variable from Indepdent variable and model.y predicts Dependent variable from mediator and independent variable. Following this, a mediate function is used to figure out the effect of IV and Mediator variables on DV by turning on the bootstrap and simulations.
# 
# Inference: 
# 
# Looking at the output it is clear that P<.05 for ACME( Average Causation Mediation Indirect effect) and ADE( Average Direct Effect ), thus signifying that IV actually affects DV and thus the mediator variable is considered to be responsible for partial mediation of the DV. Meaning that, just like the degree(IV), the race(Mediator) of the respondent too had its fair share in determining the job loss(DV).
# But when we look at the confidence interal we get much more information as to how well a variable mediates the DV, which is explained below. 


plot(effect1)

# Confidence Interval:

# The above plot indicates the CI of Control-treatment group, the ACME for the job lose is estimated to be approximately 0.00372, with the 95% confidence interval ranging from 0.00290 to 0.01 points and the ADE is estimated to be approximately 0.06967, with the 95% confidence interval ranging from .06143 to 0.08. 
# This implies that degree of the US citizen changes account for about 5.07% of the total effect and the race does the remaining mediation. 


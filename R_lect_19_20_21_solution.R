#homework19 20 21;
#author: Ashwin


#Problem 1. Use the regression model you have built in R. Lecture 18 and submit a report including:
 
#identifying outliers (if there is any)
#identifying influential observations (if there is any)
#selecting the best regression model

install.packages('ISLR')
library('ISLR')
library(car)
library(leaps)


mydata = ISLR::Wage
head(mydata)

table(mydata$jobclass)


mydata_cleaned = mydata[,-6]  #here, we delete region column since it includes only mid-atlantic. Hence it is of no use in regression

head(mydata_cleaned)


full_model = lm(formula = wage ~ age + factor(maritl) +factor(race) +factor(education)
           + factor(jobclass) +factor(health) +factor(health_ins) +logwage  , data = mydata_cleaned)

summary(full_model)

# Assessing outliers
# |standardized residual| > 2 (or 3)

qqPlot(full_model, simulate=TRUE, labels=row.names(mydata_cleaned),
       id=list(method="identify"), main="Q-Q Plot")

outlierTest(full_model)



# Identifying influential observations

# Cooks Distance D
# identify D values > 4/(n-k-1) 
# where n=sample size
# k=# of predictor variables

( cutoff <- 4/(nrow(mydata_cleaned)-length(full_model$coefficients)-2) )

plot(full_model, which=4, cook.levels=cutoff)

abline(h=cutoff, lty=2, col="red")

# All subsets regression
#library(leaps)

#leaps <-regsubsets( wage ~ age + factor(maritl) +factor(race) +factor(education)
                  #  + factor(jobclass) +factor(health) +factor(health_ins) +logwage  , data = mydata_cleaned, nbest=4)
#plot(leaps, scale="adjr2")






#Qno2
#Problem 2. Develop an ANOVA using the education level ('education') to predict wage 
#('wage') and discuss the overall and pairwise comparisons.

library(dplyr)
library(ggplot2)
library(multcomp)
library(car)
library(effects)


fit <- aov(wage ~ education, data=mydata_cleaned)                                  
summary(fit)



# Tukey HSD pairwise group comparisons
pairwise <- TukeyHSD(fit)
pairwise


plotdata <- as.data.frame(pairwise[[1]])
plotdata$conditions <- row.names(plotdata)
plotdata
head(plotdata)

ggplot(data=plotdata, aes(x=conditions, y=diff)) + 
 geom_point(size=3, color="red") +
 geom_errorbar(aes(ymin=lwr, ymax=upr, width=.2)) +
 geom_hline(yintercept=0, color="red", linetype="dashed") +
 theme_bw() +
 labs(y="Difference in mean levels", x="", 
      title="95% family-wise confidence level") +
 coord_flip()




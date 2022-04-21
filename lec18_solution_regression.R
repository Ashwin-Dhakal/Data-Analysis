# stat4110/7110 Lecture 18 -- Regression 


#homework18;
mydata = ISLR::Wage
head(mydata)
table(mydata$maritl)
table(mydata$race)
table(mydata$education)
table(mydata$region)
table(mydata$health)
table(mydata$health_ins)

mydata_cleaned = mydata[,-6]  #here, we delete region column since it includes only mid-atlantic. Hence it is of no use in regression

head(mydata_cleaned)


lm(formula = wage ~ factor(race), data = mydata_cleaned)
model = lm(formula = wage ~ age + factor(maritl) +factor(race) +factor(education)
           +factor(health) +factor(health_ins) +logwage  , data = mydata_cleaned)

summary(model)

library(olsrr)
ols_step_forward_p(model)
#These are the selected predictor variables
# 1    logwage            
# 2    factor(education)  
# 3    factor(health_ins) 
# 4    age                
# 5    factor(maritl)




ols_step_backward_p(model)
#These are the deleted variables:
# 1    factor(race)   
# 2    factor(health) 
# 3    age



ols_step_both_p(model)
#These are the stepwise selected variables
#1         logwage       
#2    factor(education)  
#3    factor(health_ins) 
#4           age         

#-----------------------------------
#Hence, we conclude that these are the highly influencial predictor variables:
 # 1    logwage            
 # 2    factor(education)  
 # 3    factor(health_ins) 
 # 4    age                
 # 5    factor(maritl)
 
final_model = lm(formula = wage ~ age + factor(maritl)  +factor(education)
            +factor(health_ins) +logwage  , data = mydata_cleaned)

summary(final_model)



cor(mydata_cleaned)
library(ggcorrplot)
install.packages("ggcorrplot")


install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%




model.matrix(~0+., data=mydata_cleaned) %>% 
 cor(use="pairwise.complete.obs") %>% 
 ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)



plot(final_model)
par(mfrow=c(2,2))

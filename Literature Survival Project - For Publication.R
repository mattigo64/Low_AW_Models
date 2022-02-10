library(readxl)
library(car)
library(olsrr)
#####Import Salmonella Data
low_awSal = Low_Aw_Survival_Modelling_Extraction <- read_excel("Low Aw Survival Modelling Extraction.xlsx", sheet = "Sal_For_R")

#####Creates Factor Levels for Categorical Variables                                                                                                                             
ST_sal = factor(low_awSal$`Serovar/Strain`)
BT_sal = factor(low_awSal$`Buffer Type`)
MT_sal = factor(low_awSal$`Media Type`)
Inoc = factor(low_awSal$Inoculation)
GR_sal = factor(low_awSal$Growth)

#####Sets Baseline Factor Level
ST_sal = relevel(ST_sal, 'Cocktail')
Inoc = relevel(Inoc, 'Wet')
BT_sal = relevel(BT_sal, 'PEP')

#####Creates Model From All Factors
Sal_model_full = lm(Rate ~ Temperature +Initial + Aw + ST_sal +Inoc +GR_sal + MT_sal + BT_sal, data=low_awSal)
summary(Sal_model_full)

#####Checks Variance Inflation Factor
car::vif(Sal_model_full)

#####Buffer Type and Media Type Removed Due to Multicollinearity
#####Model Run Again w/o These
Sal_model = lm(Rate ~ Temperature +Initial + Aw + ST_sal +Inoc +GR_sal, data=low_awSal)
aovSal = aov(Sal_model)
summary(aovSal)
car::vif(Sal_model)

#####Stepwise Reduction Procedure
step(Sal_model, direction = "both", k=2)

#####Model After Paramter Reduction
subset_model_Sal = lm(formula = Rate ~ Temperature   + Aw+ Inoc+  ST_sal+ GR_sal, data = low_awSal)
summary(subset_model_Sal)
aov_sub_Sal = aov(subset_model_Sal)
summary(aov_sub_Sal)
car::vif(subset_model_Sal)

#####Creates Best Subset Models
ols_step_best_subset(subset_model_Sal, IC='AIC')

#####Linear Models for Individual Parameters
summary(lm(Rate~Temperature, data = low_awSal))
summary(lm(Rate~Aw, data = low_awSal))
summary(lm(RR~Temperature, data = low_awSal))

#####
#####
#####Import Listeria Data
low_awLis <- read_excel("Low Aw Survival Modelling Extraction.xlsx", sheet = "Listeria_For_R")

#####Creates Factor Levels for Categorical Variables                                                                                                                             
ST_lis = factor(low_awLis$Organism)
BT_lis = factor(low_awLis$`Buffer Type`)
MT_lis = factor(low_awLis$`Media Type`)
GR_lis = factor(low_awLis$Growth)

#####Sets Baseline Factor Level
ST_lis = relevel(ST_lis, "Cocktail")
BT_lis=relevel(BT_lis,'PEP')

#####Creates Model From All Factors
Lis_model_full = lm(Rate ~ Temperature  + Aw + ST_lis  + GR_lis + MT_lis + BT_lis + Initial   , data=low_awLis)
summary(Lis_model_full)

#####Checks Variance Inflation Factor
car::vif(Lis_model_full)

#####Buffer Type and Media Type Removed Due to Multicollinearity
#####Model Run Again w/o These
Lis_model = lm(Rate ~ Temperature  + Aw + ST_lis  + GR_lis + Initial   , data=low_awLis)
summary(Lis_model)
aovLis = aov(Lis_model)
summary(aovLis)

#####Stepwise Reduction Procedure
step(Lis_model, direction = "both", k=2)

#####Model After Paramter Reduction
subset_model_Lis = lm(Rate~ Temperature + ST_lis + GR_lis  , data = low_awLis)
summary(subset_model_Lis)
aov_sub_Lis = aov(subset_model_Lis)
summary(aov_sub_Lis)

#####Creates Best Subset Models
ols_step_best_subset(subset_model_Lis, IC='AIC')

#####Linear Models for Individual Parameters
summary(lm(Rate~Temperature, data = low_awLis))
summary(lm(Rate~Aw, data = low_awLis))
summary(lm(RR~Temperature, data = low_awLis))


#####
#####
#####Import E. coli Data
low_awEcoli<- read_excel("Low Aw Survival Modelling Extraction.xlsx", sheet = "Ecoli_For_R")

#####Creates Factor Levels for Categorical Variables                                                                                                                             
ST_Ecoli = factor(low_awEcoli$Organism)
BT_Ecoli = factor(low_awEcoli$`Buffer Type`)
MT_Ecoli = factor(low_awEcoli$`Media Type`)
GR_Ecoli = factor(low_awEcoli$Growth)

#####Sets Baseline Factor Level
ST_Ecoli = relevel(ST_Ecoli, 'O157:H7 Cocktail')
BT_Ecoli=relevel(BT_Ecoli,'PEP')

#####Creates Model From All Factors
Ecoli_model_full = lm(Rate ~ Temperature + Initial + Aw + ST_Ecoli + GR_Ecoli + BT_Ecoli + MT_Ecoli , data=low_awEcoli)
summary(Ecoli_model_full)

#####Checks Variance Inflation Factor
car::vif(Ecoli_model_full)

#####Buffer Type and Media Type Removed Due to Multicollinearity
#####Model Run Again w/o These
Ecoli_model = lm(Rate ~ Temperature + Initial  + Aw   + ST_Ecoli + GR_Ecoli , data=low_awEcoli)
summary(Ecoli_model)
summary(aov(Ecoli_model))
car::vif(Ecoli_model)

#####Stepwise Reduction Procedure
step(Ecoli_model, direction = "both", k=2)

#####Model After Paramter Reduction
subset_model_Ecoli = lm(Rate~ Temperature + GR_Ecoli + Aw , data = low_awEcoli)
summary(subset_model_Ecoli)
aov_sub_Ecoli = aov(subset_model_Ecoli)
summary(aov_sub_Ecoli)
car::vif(subset_model_Ecoli)

#####Creates Best Subset Models
ols_step_best_subset(subset_model_Ecoli, IC = 'AIC')

#####Linear Models for Individual Parameters
summary(lm(Rate~Temperature, data = low_awEcoli))
summary(lm(Rate~Aw, data = low_awEcoli))
summary(lm(RR~Temperature, data = low_awEcoli))










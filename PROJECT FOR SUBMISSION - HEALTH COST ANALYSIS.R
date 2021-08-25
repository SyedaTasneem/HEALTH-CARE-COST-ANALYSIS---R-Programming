###################    "R" PROGRAMMING - PROJECT FOR SUBMISSION     #########################
###################          HEALTH CARE COST ANALYSIS                 ######################
####### DOMAIN : HEALTH CARE
######    DATA : "hospitalcosts.xlsx"
## DESCRIPTION : A nationwide survey of hospital costs conducted by the US Agency for Healthcare
# consists of hospital records of inpatient samples. The given data is restricted to the city 
# of Wisconsin and relates to patients in the age group 0-17 years. 
# The agency wants to analyze the data to research on healthcare costs and their utilization.

##########   LOADING THE DATASET ###########################################################

library(readxl)
hospital_data <- read_excel("C:/Users/imtiyaz/Desktop/Datasets/hospitalcosts.xlsx")
View(hospital_data)


###########   GETTING A LOOK OF THE DATASET   ############################################## 

head(hospital_data)
summary(hospital_data)

############# DATA CLEANING and MANIPULATION   ###########################################

sum(is.na(hospital_data))
names(hospital_data)

##### 1.There is one null value "NA" in the data  .... we need to remove it.
#### 2. The gender column is named as "FEMALE"... we have to change it to "GENDER".
### 3. We can also rename column "TOTCHG" to "EXPENDITURE" for ease of understanding.
## 4. Column "APRDRG" can be changed to "DIAGNOSIS GOURP".
# 5. column "LOS" can also be changed to "STAY"

library(dplyr)
data <- hospital_data %>%
  na.omit() %>% 
  rename(GENDER = FEMALE) %>% 
  rename(EXPENDITURE = TOTCHG) %>% 
  rename(DIAGNOSIS.GROUP = APRDRG) %>% 
  rename(STAY = LOS)

str(data)

# 6."GENDER" and "RACE" columns are encoded as "num"...we need change them to "factor" datatype.

data$RACE <- as.factor(data$RACE)
levels(data$RACE)

data$GENDER <- as.factor(data$GENDER)
levels(data$GENDER)

# OUTPUT: [1] "0" (for male), "1" (for female)


####################### DATA ANALYSIS THROUGH VISUALIZATION    ###############################
### STEP.1 : 
# Find the age category of people who frequently visit the hospital and have max expenditure.

library(ggplot2)
ggplot(data, aes(x = AGE))+
  geom_bar(fill = "blue", alpha = 0.5)+
  labs(title = "AGE WISE COUNT OF PATIENTS",
       x = "AGE",
       y = "COUNT")

#### INFERENCE FROM AGE PLOT : 
### a. Maximum number of patients are "NEONATAL"(NEW BORNS, age = 0, approx 310),
## b. 2nd most patients belong to age "17" (approx 40 patients)
# c. 3rd most patients are from ages "16" n "15" (approx 30 patients each)

ggplot(data, aes(x = AGE, y = EXPENDITURE))+
  geom_point(colour = "red")+
  labs(title = "AGE - EXPENDITURE",
       x = "AGE",
       Y = "EXPENDITURE")

##### INFERENCE FROM AGE-TOTAL CHARGE PLOT:
#### a. Total expenditure incurred upon inpatients usually ranges b/w $0 t0  $20,000.
### b. A couple of data points for age "0" show an expenditure of more than $25,000.
## c. A single outlier is detected at age "17" with an expenditure $ 49,000 approx.
# d. We may assume that age does not influence the expenditure incurred by patients.
 

#### STEP.2 : 
# The agency wants to find the diagnosis-related group that has maximum hospitalization and 
# expenditure.

ggplot(data,aes(x = DIAGNOSIS.GROUP))+
  geom_histogram(colour ="black", fill = "green",alpha = 0.7)+
  theme_bw()+ 
  labs(title = "DIAGNOSIS RELATED GROUPS",
       X = "DIAGNOSIS GROUP",
       Y = "COUNT")

ggplot(data, aes(x = DIAGNOSIS.GROUP, y = EXPENDITURE, size = EXPENDITURE))+
  geom_point(alpha = 0.5)+
  labs(title = " DIAGNOSIS RELATED GROUP  Vs EXPENDITURE",
       x = "DIAGNOSIS RELATED GROUP",
       y= "EXPENDITURE")
       #geom_smooth(method = lm))

##### INFERENCE FROM DIAGNOSIS.GROUP Vs EXPENDITURE :
# a. The plot shows most of the data distributed in the range of $0 to $20000, barring a few .
# b. Diagnosis groups do not seem to effect the expenditure.


#### STEP.3:
# 3. The agency wants to analyze if the race of the patient is related to the hospitalization 
# costs to detect any Malpractice.

ggplot(data, aes(x = RACE, y = EXPENDITURE ))+
  geom_col(fill = "pink")+
  theme_bw()+
  labs(title = "EXPENDITURE Vs RACE",
       x = "RACE",
       y = "EXPENDITURE")

#### INFERENCE FROM RACE Vs EXPENDITURE :
# a.The plot reveals that people of RACE"1" seem to incur maximum EXPENDITURE on hospital bills.
# b. There seems to be a large gap betweem RACE "1" and other races.
# c. This could be a case of Malpractice, need more analysis to confirm.

#### STEP.4 :
#4. The agency wants to analyze the severity of the hospital costs by age and gender for 
# proper allocation of resources. 
  
ggplot(data, aes(x = AGE, y = EXPENDITURE))+
  geom_col(aes(fill = GENDER, alpha = 0.5))+
  facet_wrap(~GENDER)+
  labs(title = "EFFECT OF AGE & GENDER ON EXPENDITURE")

#### INFERENCE FROM AGE & GENDER ON EXPENDITURE PLOT :
# a. There is not much difference between the expenditure incurred by Male or Female patients.
# b. New born Males seem to incur a little more than their Female counterparts
# c. Gender does not effect the Expenditure

#### STEP.5 : 
# The length of stay is the crucial factor for inpatients, the agency wants to find if the 
# length of stay can be predicted from age, gender, and race.

attach(data)
Result <- lm(formula = STAY~AGE + GENDER + RACE)
Result
summary(Result)

##### INFERENCE FROM RESULT:

# The "Y" intercept "Bo" and slpoes foe concerned IDVs are as follows:

#                     Estimate      P-Value    
# Y Intercept(Bo) =   2.85687      <2e-16 ***
#  SLOPES => AGE  =  -0.0393       0.0818 .  
#        GENDER1  =  0.35391       0.2586    
#          RACE2  = -0.37501       0.7883    
#          RACE3  =  0.78922       0.8158    
#          RACE4  =  0.59493       0.7613    
#          RACE5  = -0.85687       0.6626    
#          RACE6  = -0.71879       0.7640    


#           R-squared =  0.008699,	
# Adjusted R-squared  = -0.005433   => accuracy of Model is only "-0.54%" ,
#     Overall p-value = 0.7432      => P-value > 0.05 (Accept NULL HYPOTHESIS)
 
### the P-value > 0.05 (for all Predictor variables n also for the Model)
# HENCE we cannot reject the "NULL HYPOTHESIS " and the above predictors "AGE", "GENDER" and
# "RACE" can not be used to predict "STAY "

#### STEP.6: 
# 6. To perform a complete analysis, the agency wants to find the variable that mainly affects 
# hospital costs.
  
attach(data)
Final_result <- lm(formula = EXPENDITURE ~ AGE + GENDER + RACE + STAY + DIAGNOSIS.GROUP )
Final_result
summary(Final_result)


##### INFERENCE FROM RESULT:
# The "Y" intercept "Bo" and slpoes foe concerned IDVs are as follows:

#                   Estimate       P-Value   
#  "Y"Intercept =  5024.9610      < 2e-16 ***
#           AGE =   133.2207      2.29e-13 ***
#       GENDER1 =  -392.5778      0.116    
#         RACE2 =   458.2427      0.673    
#         RACE3 =   330.5184      0.900    
#         RACE4 =  -499.3818      0.743    
#         RACE5 = -1784.5776      0.245    
#         RACE6 = -594.2921       0.749    
#          STAY =  742.9637       < 2e-16 ***
#DIAGNOSIS.GROUP=   -7.8175       < 2e-16 ***

#          R-squared  = 0.5544,	
# Adjusted R-squared  = 0.5462     => accuracy of the model is 54% .
#            p-value  = < 2.2e-16  => reject NULL HYPOTHESIS

# The P-Value is less than 0.01 for AGE, STAY and DIAGNOSIS RELATED GROUP.
# HENCE AGE, STAY & DIAGNOSIS GROUP are good predictors of EXPENDITURE.


























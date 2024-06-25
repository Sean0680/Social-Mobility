#### TABLE OF CONTENTS ####
# 1. Setting up the workspace 
# 2. Computing variables
#   2.1. Computing variables
#   2.2. Cleaning data base 
# 3. Explore data
#   3.1. Sociodemographic Characteristics 
#   3.2. Descriptive analysis 
# 4. Confirmatory Factorial Analysis (CFA)
# 5. Correlations with other constructs
# 6. Bidimensional Social Mobility Scale
# 7. Supplementary Material


library(broom)
#-------------------------------------#
#### 1. Setting the workspace ####
#-------------------------------------#
# Install and load packages 
pacman::p_load (tidyverse, psych, lavaan, dplyr, car, plyr, performance, sjPlot, 
        sjmisc, GGally, gtsummary, rstatix, Routliers, sjlabelled, ggplot2, apaTables, semTools, 
        install = F)


## Load data base
data <- Matamoros_2_original

data$ID <- 1:nrow(data) 

## Measures: 
# Bidimensional Social Mobility Beliefs Scale (BSMBS)
# Meritocratic Beliefs Scale (MBS: Spanish adaptation by García-Sánchez et al., 2022; Zimmerman and Reyna, 2013)
# Economic System Justification Scale (ESJS: Spanish adaptation by Jaume et al., 2012; Jost & Thompson, 2000)
# Status Anxiety Scale (SAS: Spanish adaptation by Melita et al., 2020; Keshabyan & Day, 2020)
# Subjective Socioeconomic Status (SSS: Adler et al., 2000)



#-----------------------------------------------------#
#### 2. Computing variables and cleaning data base ####
#-----------------------------------------------------#
##### 2.1. Computing variables ####
## Transform variables
data <- as.data.frame(apply(data, 2, as.numeric))  

## Created Variables
data$Income_part <- data$Household_Income / data$Household_members_1

data$Mean_UP <- rowMeans(cbind (data$BSMBS_4u_I1, data$BSMBS_8u_I2,           
                                data$BSMBS_9u_I3, data$BSMBS_10u_I4), na.rm = T)

data$Mean_DOWN <- rowMeans(cbind (data$BSMBS_11d_I5, data$BSMBS_13d_I6,
                                   data$BSMBS_14d, data$BSMBS_18d), na.rm = T)

data$Mean_MBS <- rowMeans(cbind (data$MBS_1, data$MBS_2, data$MBS_3, data$MBS_4,  
                                 data$MBS_5, data$MBS_6), na.rm = T)

data$Mean_ESJS <- rowMeans(cbind (data$ESJS_1, data$ESJS_2, data$ESJS_3, data$ESJS_4,
                                   data$ESJS_5, data$ESJS_6, data$ESJS_7), na.rm = T) 

data$Mean_SAS <- rowMeans(cbind (data$SAS_1, data$SAS_2, data$SAS_3, 
                                  data$SAS_4, data$SAS_5), na.rm = T)

data$Household_Income <- as.numeric(data$Household_Income)
data$Mean_UP <- as.numeric(data$Mean_UP)
data$Mean_DOWN <- as.numeric(data$Mean_DOWN)
data$Mean_MBS <- as.numeric(data$Mean_MBS)
data$Mean_ESJS <- as.numeric(data$Mean_ESJS)
data$Mean_SAS <- as.numeric(data$Mean_SAS)

##### 2.2. Cleaning data base ####
## Exclusions
'Data from participants who have not Spanish nationality, or are under 18 will 
be excluded from the analyses, as well as, data from participants who fail to correctly 
answer the attention check items (e.g., “This is a question to check the 
participants attention. To answer this item correctly check the nuMBSer five”). 

Participants who score more than 3 times the absolute deviation from the median 
in Beliefs about Type of Social Mobility Scale, MBScratic beliefs, Economic System ESJSfication
and Status anxiety will be considered outliers and their scores in those variables 
will not be included in the analysis.'


# Missing data
data1 <- data %>% dplyr::na_if(NA)


# Outliers with MAD
outliers_mad(data$Mean_UP, b = 1.4826, 3, na.rm = TRUE)
outliers_mad(data$Mean_DOWN, b = 1.4826, 3, na.rm = TRUE)
outliers_mad(data$Mean_MBS, b = 1.4826, 3, na.rm = TRUE)
outliers_mad(data$Mean_ESJS, b = 1.4826, 3, na.rm = TRUE)
outliers_mad(data$Mean_SAS, b = 1.4826, 3, na.rm = TRUE)


# Filter with outliers: N = 400
data1 <- subset(data %>%
                  dplyr::na_if(NA) %>% 
                  filter(Age >= 18, Nacionality == 1, AC_1 == 5, AC_2 == 5, 
                         Mean_DOWN < 6.83585)) 

data2<- subset(data1 %>%
                            select(Age, Gender, Marital_Status, Education, Occupation, 
                                   Income_part, SSS, PoliticalOrientation))

BSMBS <- subset(data1 %>%
                  dplyr::na_if(NA) %>% 
                  select(BSMBS_4u_I1, BSMBS_8u_I2,
                         BSMBS_9u_I3, BSMBS_10u_I4, 
                         BSMBS_11d_I5, BSMBS_13d_I6, 
                         BSMBS_14d_I7, BSMBS_18d_I8)) 



#-------------------------------------#
#### 3. Explore Data  ####
#-------------------------------------#
##### 3.1. Sociodemographic Characteristics  ####
data2 %>% 
  select(Age, Income_part, Gender, Marital_Status, Education, Occupation, 
         SSS, PoliticalOrientation) %>%  
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"), 
              #digits = all_continuous() ~ 2, 
              digits = all_categorical() ~ 2,
              label = list(Income_part ~ "Participant’ Income", Marital_Status ~ "Marital Status", Education ~ "Educational Attainment",
                           SSS ~ "Subjective Socio-economic Status", PoliticalOrientation ~ "Political Orientation"), 
              missing_text = "(Missing)") %>%
  modify_caption("**Table SXX. Sociodemografic variable (Study 2)**") %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_footnote(
    all_stat_cols() ~ "Note: N, Total sample size; Mean (SD); Total number of participants (%)") %>%
  bold_labels()



##### 3.2. Descriptive analysis ####
BSMBS %>% 
  dplyr::na_if(NA)%>% 
  psych::describe()  
  
# Multivariate normality
#install.packages("MVN")
library("MVN")

result_mnorm <- mvn(data = BSMBS, mvnTest = "hz") 

result_mnorm <- mvn(data = BSMBS, mvnTest = "hz", univariatePlot = "qqplot") 

result_mnorm <- mvn(data = BSMBS, mvnTest = "hz", univariatePlot = "histogram") 


## Inter-items correlations
################################################################################################################
cor1 <- data.frame()
columns <- colnames(BSMBS)
for (i in 1:(ncol(BSMBS) - 1)) {
  for (j in (i + 1):ncol(BSMBS)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(BSMBS[[col1]], BSMBS[[col2]]))
    
    cor1 <- bind_rows(cor1, test_result)
  }
}
################################################################################################################
## Reliability scales
data1 %>% 
  select("MBS_1", "MBS_2", "MBS_3", "MBS_4",
         "MBS_5","MBS_6") %>% 
  psych::alpha(na.rm = T) # alpha = .92 


data1 %>% 
  select("ESJS_1", "ESJS_2", "ESJS_3", "ESJS_4", "ESJS_5",
         "ESJS_6", "ESJS_7") %>% 
  psych::alpha(na.rm = T) # alpha = .83


data1 %>% 
  select("SAS_1", "SAS_2", "SAS_3", "SAS_4",
         "SAS_5") %>% 
  psych::alpha(na.rm = T) # alpha = .87



#----------------------------------------------------#
#### 4. Confirmatory Factorial Analysis (CFA) ####
#----------------------------------------------------#
## Uni-factorial model ##
################################################################################################################
cfa_a <- 'Mobility =~ BSMBS_4u_I1 +  BSMBS_8u_I2 +
                        BSMBS_9u_I3 + BSMBS_10u_I4 + 
                        BSMBS_11d_I5 + BSMBS_13d_I6 +  
                        BSMBS_14d_I7 + BSMBS_18d_I8'


cfa_fit_a <- cfa(cfa_a, 
                 estimator = "MLR",
                 data = BSMBS)
cfa1 <- cfa_fit_a

## Bi-factorial model ##
cfa_b <- 'Upward =~ BSMBS_4u_I1 +  BSMBS_8u_I2 +
                    BSMBS_9u_I3 + BSMBS_10u_I4 
          
          Downward =~ BSMBS_11d_I5 + BSMBS_13d_I6 +  
                      BSMBS_14d_I7 + BSMBS_18d_I8'


cfa_fit_b <- cfa(cfa_b, 
                estimator = "MLR",
                data = BSMBS)

cfa2 <- cfa_fit_b
################################################################################################################
# Factor reliability
semTools::reliability(cfa_fit_b,
                       what = c("alpha", "omega"))
# Model comparison tests
################################################################################################################
anova(cfa_fit_a, cfa_fit_b,
      method = "satorra.2000") 

################################################################################################################
## Create fit index table
ajuste <- c("chisq","df","pvalue","cfi","tli","srmr", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")

tabla_ajuste <- matrix(NA, nrow = 2, ncol = 10)

colnames(tabla_ajuste) <- c("Model", ajuste)

tabla_ajuste[1,] <- c("Unifactorial", round(fitMeasures(cfa_fit_a, ajuste), digits = 3))

tabla_ajuste[2,] <- c("Bifactorial", round(fitMeasures(cfa_fit_b, ajuste), digits = 3))


tabla_ajuste

## Discrimination index (corrected item-total correlation) and reliability
BSMBS %>% 
  select("BSMBS_4u_I1","BSMBS_8u_I2",     
         "BSMBS_9u_I3", "BSMBS_10u_I4") %>%  
  psych::alpha(na.rm = T) # alpha = .81 (Upward dimension)


BSMBS %>% 
  select("BSMBS_11d_I5","BSMBS_13d_I6",     
         "BSMBS_14d_I7", "BSMBS_18d_I8") %>%  
  psych::alpha(na.rm = T) # alpha = .83 (Downward dimension)


#-----------------------------------------------------------#
#### 5. Correlations between BSMBS and other constructs  ####
#-----------------------------------------------------------#
## Descriptive statistics  
data1 %>% 
  select(Mean_UP, Mean_DOWN, Mean_MBS, Mean_ESJS, Mean_SAS, SSS, PoliticalOrientation) %>%
  dplyr::na_if(NA)%>%
  psych::describe() 


## Correlations
################################################################################################################

cor2df <- subset(data1, select = c(Mean_UP, Mean_DOWN, Mean_MBS, Mean_ESJS, Mean_SAS, SSS))
cor2 <- data.frame()
columns <- colnames(cor2df)
for (i in 1:(ncol(cor2df) - 1)) {
  for (j in (i + 1):ncol(cor2df)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor2df[[col1]], cor2df[[col2]]))
    
    cor2 <- bind_rows(cor2, test_result)
  }
}
################################################################################################################

#-----------------------------------#
#### 7. Supplementary Material ####
#-----------------------------------#
##### Pre-registered Hypothesis ####
###  Hypothesis 1
r.test(400,.534,-.241, twotailed = F) 

## Hypothesis 2
r.test(400,.435,-.182, twotailed = F) 

## Hypothesis 3
r.test(400,-.172,.260, twotailed = F) 



##### Invariance: gender ####
data1$Gender <- as.numeric(data1$Gender)

invariance_gender <- subset(data1 %>%
                  dplyr::na_if(NA) %>% 
                  filter(Gender != 3)) 

# Table 

ajuste <- c("chisq","df","pvalue","cfi.robust","tli.robust", "srmr",
"rmsea.robust","rmsea.ci.lower", "rmsea.ci.upper")

tabla_ajuste <- matrix(NA, nrow = 4, ncol = 10)

colnames(tabla_ajuste) <- c("Invariance", ajuste)
# Model 1: (configural)
################################################################################################################
m1_fit <- cfa(cfa_b, 
              estimator = "MLR",
              data = invariance_gender,
              group = "Gender",
              check.gradient = FALSE)  

tabla_ajuste[1,] <- c("Configural", round(fitMeasures(m1_fit, ajuste), digits = 3))

cfa3 <- m1_fit
# Model 2: (metric) equal factor loadings
m2_fit <- cfa(cfa_b, 
              estimator = "MLR",
              data = invariance_gender,
              group = "Gender",
              group.equal = c("loadings"))

tabla_ajuste[2,] <- c("Metric", round(fitMeasures(m2_fit, ajuste), digits = 3))

cfa4 <- m2_fit
# Model 3: (scalar) equal intercepts
m3_fit <- cfa(cfa_b, 
              estimator = "MLR",
              data = invariance_gender,
              group = "Gender",
              group.equal = c("loadings", "intercepts"))

cfa5 <- m3_fit
# Model 4: (residual): equal factor loadings
m4_fit <- cfa(cfa_b, 
              estimator = "MLR",
              data = invariance_gender,
              group = "Gender",
              group.equal = c("loadings", "intercepts", "residuals"))

cfa6 <- m4_fit
################################################################################################################

##### Invariance: Subjective Socioeconomic Status ####
data1$SSS <- as.numeric(data1$SSS)

data1$SSS_cate <- cut(data1$SSS,
                      na.rm = T,
                      breaks = c(1, 5, 10),
                      labels = c("SSS_low", "SSS_high")) 

# Table 
ajuste <- c("chisq","df","pvalue","cfi.robust","tli.robust", "srmr",
"rmsea.robust","rmsea.ci.lower", "rmsea.ci.upper")

tabla_ajuste2 <- matrix(NA, nrow = 4, ncol = 10)

colnames(tabla_ajuste2) <- c("Invariance", ajuste)

tabla_ajuste2

################################################################################################################
# Model 1: (configural)
m1_fit <- cfa(cfa_b, 
              estimator = "MLR",
              data = data1,
              group = "SSS_cate")  

tabla_ajuste2[1,] <- c("Configural", round(fitMeasures(m1_fit, ajuste), digits = 3))

cfa7 <- m1_fit
# Model 2: (metric) equal factor loadings
m2_fit <- cfa(cfa_b, 
              estimator = "MLR",
              data = data1,
              group = "SSS_cate",
              group.equal = c("loadings"))

tabla_ajuste2[2,] <- c("Metric", round(fitMeasures(m2_fit, ajuste), digits = 3))

cfa8 <- m2_fit
# Model 3: (scalar) equal intercepts
m3_fit <- cfa(cfa_b, 
              estimator = "MLR",
              data = data1,
              group = "SSS_cate",
              group.equal = c("loadings", "intercepts"))


tabla_ajuste2[3,] <- c("Scalar", round(fitMeasures(m3_fit, ajuste), digits = 3))

cfa9 <- m3_fit
# Model 4: (residual): equal factor loadings
m4_fit <- cfa(cfa_b, 
              estimator = "MLR",
              data = data1,
              group = "SSS_cate",
              group.equal = c("loadings", "intercepts", "residuals"))


tabla_ajuste2[4,] <- c("Residual", round(fitMeasures(m4_fit, ajuste), digits = 3))

cfa10 <- m4_fit
################################################################################################################

results = bind_rows(
  cfa11 = tidy(cfa1),
  cfa22 = tidy(cfa2),
  cfa33 = tidy(cfa3),
  cfa44 = tidy(cfa4),
  cfa55 = tidy(cfa5),
  cfa66 = tidy(cfa6),
  cfa77 = tidy(cfa7),
  cfa88 = tidy(cfa8),
  cfa99 = tidy(cfa9),
  cfa101 = tidy(cfa10),
  cor.test1 = cor1,
  cor.test2 = cor2,
  .id = "model"
)

write_csv(results, "SocialMobility/Matamoros-lima_2/results/Matamoros-lima_2_results_original.csv")



#-------------------------------------#
#### 1. Setting the workspace ####
#-------------------------------------#
# Install and load packages
library(broomExtra)
pacman::p_load (tidyverse, psych, lavaan, dplyr, car, plyr, performance, sjPlot, 
        sjmisc, GGally, gtsummary, rstatix, sjlabelled, ggplot2, apaTables, GPArotation,
        install = T)


## Load data base
data <- Matamoros_lima_1_synthpop
data$ID <- 1:nrow(data)


## Measures Labels: 
# Bidimensional Social Mobility Beliefs Scale (BSMBS)
# Social Mobility Beliefs Scale (SMBS: own Spanish translation from Browman et al., 2017)
# Support for Economic Inequality Scale (SEIS: Spanish adaptation by Montoya-Lozano et al., 2022; Wiwad et al., 2019)
# Subjective Socioeconomic Status (SSS; Adler et al., 2000)



#-----------------------------------------------------#
#### 2. Computing variables and cleaning data base ####
#-----------------------------------------------------#
##### 2.1. Computing variables ####
## Transform variables
data <- as.data.frame(apply(data, 2, as.numeric))  
#data$Income_part <- as.numeric(data$Income_part)
#data$Mean_SMBS <- as.numeric(data$Mean_SMBS)
#data$Mean_SEIS <- as.numeric(data$Mean_SEIS)

#data$Gender <- factor(data3_sociodemo$Gender, levels = c("1", "2", "3"),
#                         labels = c("Male", "Female", "Other")) 

#data$Marital_Status <- factor(data$Marital_Status, levels = c("1", "2", "3", "4", "5"),
#                                 labels = c("Withoutgle", "With partner", "Married",
#                                            "Divorced", "Widowed"))

#data$Education <- factor(data$Education, levels = c("1", "2", "3", "4", "5", "6", "7"),
#                            labels = c("No schooling", "Primary education",
#                                       "Secondary education obligatory", "Secondary education no obligatory", 
#                                       "Professional training", "University studies", "Postgraduate"))

#data$Occupation <- factor(data$Occupation, levels = c("1", "2", "3", "4", "5", "6"),
#                      labels = c("Unemployed", "Student", "Student and part-time worker ",
#                                 "Part-time worker", "Full-time worker", "Retired"))

#data$PoliticalOrientation <- factor(data$PoliticalOrientation, levels = c("1", "2", "3", "4", "5", "6", "7"),
#                                       labels = c("Far-left", "Left", "Center-left",
#                                                "Center", "Center-right", "Right", "Far-right"))


## Reversed items
# Social Mobility Beliefs Scale (SMBS; Browman et al., 2017)
data$SMBS_1_R <- dplyr::recode(data$SMBS_1, '1' = 7L, '2' = 6L, '3' = 5L, 
                                 '4' =  4L, '5' = 3L, '6' = 2L, '7' = 1L)

data$SMBS_3_R <- dplyr::recode(data$SMBS_3, '1' = 7L, '2' = 6L, '3' = 5L, 
                               '4' =  4L, '5' = 3L, '6' = 2L, '7' = 1L)

data$SMBS_7_R <- dplyr::recode(data$SMBS_7, '1' = 7L, '2' = 6L, '3' = 5L, 
                               '4' =  4L, '5' = 3L, '6' = 2L, '7' = 1L)

data$SMBS_8_R <- dplyr::recode(data$SMBS_8, '1' = 7L, '2' = 6L, '3' = 5L, 
                               '4' =  4L, '5' = 3L, '6' = 2L, '7' = 1L)


# Support for Economic Inequality Scale (SEIS; Wiwad et al., 2019). 
data$SEIS_2_R <- dplyr::recode(data$SEIS_2, '1' = 7L, '2' = 6L, '3' = 5L, 
                                 '4' =  4L, '5' = 3L, '6' = 2L, '7' = 1L)

data$SEIS_3_R <- dplyr::recode(data$SEIS_3, '1' = 7L, '2' = 6L, '3' = 5L, 
                                '4' =  4L, '5' = 3L, '6' = 2L, '7' = 1L)

data$SEIS_5_R <- dplyr::recode(data$SEIS_5, '1' = 7L, '2' = 6L, '3' = 5L, 
                               '4' =  4L, '5' = 3L, '6' = 2L, '7' = 1L)



## Created Variables
data$Income_part <- (data$Household_Income / data$Household_members_1) 


data$Mean_SMBS <- rowMeans(cbind (data$SMBS_1_R, data$SMBS_2, data$SMBS_3_R, data$SMBS_4,
                                  data$SMBS_5, data$SMBS_6, data$SMBS_7_R, data$SMBS_8_R),
                           na.rm = T)

data$Mean_SEIS <- rowMeans(cbind (data$SEIS_1, data$SEIS_2_R, data$SEIS_3_R, data$SEIS_4,
                                  data$SEIS_5_R),
                           na.rm = T)


data$Mean_Up <- rowMeans(cbind (data$BSMBS_4u, data$BSMBS_8u, data$BSMBS_9u, 
                                 data$BSMBS_10u),
                            na.rm = T)


data$Mean_Down <- rowMeans(cbind (data$BSMBS_11d, data$BSMBS_13d, data$BSMBS_14d, 
                                 data$BSMBS_18d),
                            na.rm = T)


##### 2.2. Cleaning data base ####
## Exclusions
'Data from participants who have not Spanish nationality, or are under 18 will be 
excluded from the analyses, as well as data from participants who fail to 
correctly answer the attention check item (e.g., “To answer this item correctly 
                                           check the number five”).' 

# Missing data 
data <- data %>% dplyr::na_if(NA) 

# Filter with outliers: N = 164
data1 <- subset(data %>%
                  select(ID, BSMBS_1u, BSMBS_2u, BSMBS_3u, BSMBS_4u, BSMBS_5u,       
                         BSMBS_6u, BSMBS_7u, BSMBS_8u, BSMBS_9u, BSMBS_10u, BSMBS_11d,       
                         BSMBS_12d, BSMBS_13d, BSMBS_14d, BSMBS_15d, BSMBS_16d, BSMBS_17d,       
                         BSMBS_18d, BSMBS_19d, BSMBS_20d, SMBS_1, SMBS_2, SMBS_3, 
                         SMBS_4, SMBS_5, SMBS_6, SMBS_7, SMBS_8, SEIS_1, SEIS_2, SEIS_3, SEIS_4, SEIS_5,                
                         AC_1, Gender, Age, Nacionality, Marital_Status, Education, Occupation, 
                         Household_Income, Household_members_1, SSS, PoliticalOrientation) %>%
                  dplyr::na_if(NA) %>% 
                  filter(Age >= 18, Nacionality == 1, AC_1 == 5)) 


data2 <- subset(data %>%
                  select(ID, AC_1, Gender, Age, Nacionality, Marital_Status, 
                         Education, Occupation, Income_part, SSS, PoliticalOrientation, 
                         Mean_SMBS, Mean_SEIS, Mean_Up, Mean_Down) %>%
                  dplyr::na_if(NA) %>% 
                  filter(Age >= 18, Nacionality == 1, AC_1 == 5)) 


data3 <- subset(data2 %>%
                           select(Age, Gender, Marital_Status, Education, Occupation, 
                                  Income_part, SSS, PoliticalOrientation))


Scale_20items <- subset(data1 %>% 
                          select(BSMBS_1u, BSMBS_2u,       
                                 BSMBS_3u, BSMBS_4u, BSMBS_5u, 
                                 BSMBS_6u, BSMBS_7u, BSMBS_8u,       
                                 BSMBS_9u, BSMBS_10u, BSMBS_11d, 
                                 BSMBS_12d, BSMBS_13d, BSMBS_14d,       
                                 BSMBS_15d, BSMBS_16d, BSMBS_17d, 
                                 BSMBS_18d, BSMBS_19d, BSMBS_20d))



#----------------------------------#
#### 3. Explore data ####
#----------------------------------#
##### 3.1. Sociodemographic Characteristics  ####
data3 %>% 
  select(Age, Income_part, Gender, Marital_Status, Education, Occupation, 
         SSS, PoliticalOrientation) %>%  
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"), 
              #digits = all_continuous() ~ 2, 
              digits = all_categorical() ~ 2,
              label = list(Income_part ~ "Participant’ Income", Marital_Status ~ "Marital Status", Education ~ "Educational Attainment", 
                           SSS ~ "Subjective Socio-economic Status", PoliticalOrientation ~ "Political Orientation"), 
              missing_text = "(Missing)") %>%
  modify_caption("**Table SXX. Sociodemografic variable (Study 1)**") %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_footnote(
    all_stat_cols() ~ "Note: N, Total sample size; Mean (SD); Total number of participants (%)") %>%
  bold_labels()


##### 3.2. Descriptive Statistics  ####
Scale_20items %>% 
  select(BSMBS_1u, BSMBS_2u,       
         BSMBS_3u, BSMBS_4u, BSMBS_5u, 
         BSMBS_6u, BSMBS_7u, BSMBS_8u,       
         BSMBS_9u, BSMBS_10u, BSMBS_11d, 
         BSMBS_12d, BSMBS_13d, BSMBS_14d,       
         BSMBS_15d, BSMBS_16d, BSMBS_17d, 
         BSMBS_18d, BSMBS_19d, BSMBS_20d) %>% 
  dplyr::na_if(NA)%>% 
  psych::describe() 
        
######################################################################################################
# Inter-Items Correlations
cor1 <- data.frame()
columns <- colnames(Scale_20items)
for (i in 1:(ncol(Scale_20items) - 1)) {
  for (j in (i + 1):ncol(Scale_20items)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(Scale_20items[[col1]], Scale_20items[[col2]]))
    
    cor1 <- bind_rows(cor1, test_result)
  }
}
#Scale_20items %>% 
#  sjPlot::tab_corr(na.deletion = c("listwise"),
#                   corr.method = c("pearson"),
#                   title = "Correlations Inter-item from Bidimensional Social Mobility Beliefs Scale",
#                   var.labels = NULL,
#                   show.p = T,
#                   p.numeric = F,
#                   fade.ns = T,
#                   val.rm = NULL,
#                   digits = 3,
#                   triangle = "lower",
#                   string.diag = NULL,
#                   use.viewer = F,
#                   remove.spaces = T)
######################################################################################################

## Reliability Measures
data1 %>% 
  select("SMBS_1", "SMBS_2", "SMBS_3", "SMBS_4", "SMBS_5", 
         "SMBS_6", "SMBS_7", "SMBS_8") %>% 
  psych::alpha(na.rm = T, 
               check.keys = T) # alfa = .89


data1 %>% 
  select("SEIS_1", "SEIS_2", "SEIS_3", "SEIS_4", "SEIS_5") %>% 
  psych::alpha(na.rm = T,
               check.keys = T) # alfa = .82



#---------------------------------#
#### 4. Factorial models ####
#---------------------------------#
##### 4.1. Check data ####
## Bartlett’s sphericity index 
psych::cortest.bartlett(Scale_20items, n = 164) # (Chi-square = 1771.58, df = 190, p < .001) 

## Kaiser-Meyer-Olkin index (KMO)
psych::KMO(Scale_20items) # KMO index (.90)


##### 4.2. Exploratory Factorial Analysis  ####
##### 4.2.1. Scale 20 items ####
# Identify factors with Parallel Analysis (Horn, 1965)
parallel_20 <- psych::fa.parallel(Scale_20items, fm = 'ml', fa = 'fa') # 3 factors

# Factor solution
efa_m1 <- fa(Scale_20items, nfactors = 3, rotate = "oblimin", fm="pa")

print.psych(efa_m1, cut = 0.3, sort = T)

"Items BSMBS_7u and BSMBS_17d do not load on any factor and 
Items BSMBS_5u, BSMBS_12d and BSMBS_15d load on two different factors"


##### 4.2.2. Scale 15 items ####
# Filter (Without items: BSMBS_7u, BSMBS_17d, BSMBS_5u, BSMBS_12d, BSMBS_15d)
Scale_15items <- subset(data1 %>% 
                          select(BSMBS_1u, BSMBS_2u, BSMBS_3u, BSMBS_4u,       
                                 BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u,      
                                 BSMBS_11d, BSMBS_13d, BSMBS_14d, 
                                 BSMBS_16d, BSMBS_18d, BSMBS_19d, BSMBS_20d))


parallel_15 <- fa.parallel(Scale_15items, fm = 'ml', fa = 'fa') # 2 factors

efa_m2 <- fa(Scale_15items, nfactors = 2, rotate = "oblimin", fm="pa")

print.psych(efa_m2, cut = 0.3, sort = T)


# Inter-Items Correlations
######################################################################################################
cor2 <- data.frame()
columns <- colnames(Scale_15items)
for (i in 1:(ncol(Scale_15items) - 1)) {
  for (j in (i + 1):ncol(Scale_15items)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(Scale_15items[[col1]], Scale_15items[[col2]]))
    
    cor2 <- bind_rows(cor2, test_result)
  }
}

#sjPlot::tab_corr(Scale_15items %>%
#                   select (BSMBS_1u, BSMBS_2u, BSMBS_3u, BSMBS_4u, 
#                           BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u, 
#                           BSMBS_11d, BSMBS_13d, BSMBS_14d,       
#                           BSMBS_16d, BSMBS_18d, BSMBS_19d, BSMBS_20d), 
#                 triangle = "lower")
######################################################################################################

# Realibility 
Scale_15items %>% # (Without items: "BSMBS_5u", "BSMBS_7u")
  select("BSMBS_1u", "BSMBS_2u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u") %>%  
  psych::alpha(na.rm = T) # (Factor: M = 3.9; SD = 1.3)


Scale_15items %>% # (Without items: "BSMBS_12d", "BSMBS_15d", "BSMBS_17d")
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d") %>%  
  psych::alpha(na.rm = T) # (Factor: M = 3.4; SD = 1.1)


# Descriptive Statistics 
Scale_15items %>% 
  dplyr::na_if(NA)%>% 
  psych::describe() 


"Item BSMBS_2u shows a high mean ((M = 4.3; SD = 1.6)) in comparison to the mean of its factor (Factor: M = 3.9; SD = 1.3)"



##### 4.2.3. Scale 14 items ####
# Filter (Without items: BSMBS_7u, BSMBS_17d, BSMBS_5u, BSMBS_15d, BSMBS_2u and BSMBS_12d)
Scale_14items <- subset(data1 %>% 
                          select(BSMBS_1u,        
                                 BSMBS_3u, BSMBS_4u, 
                                 BSMBS_6u, BSMBS_8u,       
                                 BSMBS_9u, BSMBS_10u, BSMBS_11d, 
                                 BSMBS_13d, BSMBS_14d,       
                                 BSMBS_16d, 
                                 BSMBS_18d, BSMBS_19d, BSMBS_20d))


parallel_14 <- fa.parallel(Scale_14items, fm = 'ml', fa = 'fa') # 2 factors

efa_m3 <- fa(Scale_14items, nfactors = 2, rotate = "oblimin", fm="pa")



##### 4.3. Descriptive Statistics and Discrimination and Reliability Index ####
###### 4.3.1. Scale 14 items ####
# Reliability
Scale_14items %>% # (Without "BSMBS_2u", "BSMBS_5u", "BSMBS_7u")
  select("BSMBS_1u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u") %>%  
  psych::alpha(na.rm = T) # alfa = .91


Scale_14items %>% # (Without "BSMBS_12u", "BSMBS_15u", "BSMBS_17u")
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d") %>%  
  psych::alpha(na.rm = T) # alfa = .87


## Difference between the correlation item- belonging factor and item-opposite factor
#FACTOR UP --> items DOWN 
Scale_14items %>% 
  select("BSMBS_1u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_11d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .59; r.drop (opposite factor) = .41 --> .59 - .41 = .18

Scale_14items %>% 
  select("BSMBS_1u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_13d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .68; r.drop (opposite factor) = .37 --> .68 - .37 = .31 


Scale_14items %>% 
  select("BSMBS_1u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_14d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .67; r.drop (opposite factor) = .40 --> .67 - .40 = .27 


Scale_14items %>% 
  select("BSMBS_1u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_16d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .64; r.drop (opposite factor) = .49 --> .64 - .49 = .15


Scale_14items %>% 
  select("BSMBS_1u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_18d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .72; r.drop (opposite factor) = .47 --> .72 - .47 = .25


Scale_14items %>% 
  select("BSMBS_1u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_19d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .55; r.drop (opposite factor) = .31 --> .55 - .31 = .24


Scale_14items %>% 
  select("BSMBS_1u", "BSMBS_3u", "BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_20d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .70; r.drop (opposite factor) = .44 --> .70 - .44 = .26


# FACTOR DOWN --> items UP
Scale_14items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d",
         "BSMBS_1u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .76; r.drop (opposite factor) = .45 --> .76 - .45 = .31


Scale_14items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d",
         "BSMBS_3u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .74; r.drop (opposite factor) = .40 --> .74 - .40 = .34 


Scale_14items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d",
         "BSMBS_4u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .72; r.drop (opposite factor) = .44 --> .72 - .44 = .28


Scale_14items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d",
         "BSMBS_6u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .67; r.drop (opposite factor) = .40 --> .67 - .40 = .27


Scale_14items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d",
         "BSMBS_8u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .75; r.drop (opposite factor) = .38 --> .75 - .38 = .37 


Scale_14items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d",
         "BSMBS_9u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .74; r.drop (opposite factor) = .49--> .74 - .49 = .25


Scale_14items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_19d", "BSMBS_20d",
         "BSMBS_10u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .74; r.drop (opposite factor) = .54 --> .74 - .54 = .20 



# Inter-Items Correlations   
# UP
######################################################################################################
#sjPlot::tab_corr(Scale_14items %>%
#                   select (BSMBS_1u, BSMBS_3u, BSMBS_4u,       
#                           BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u), 
#                 triangle = "lower")

# DOWN
#sjPlot::tab_corr(Scale_14items %>%
#                   select (BSMBS_11d, BSMBS_13d, BSMBS_14d,       
#                           BSMBS_16d, BSMBS_18d, BSMBS_19d, BSMBS_20d), 
#                 triangle = "lower")

# BOTH
#sjPlot::tab_corr(Scale_14items %>%
#                   select (BSMBS_1u, BSMBS_3u, BSMBS_4u,       
#                           BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u,
#                           BSMBS_11d, BSMBS_13d, BSMBS_14d,       
#                           BSMBS_16d, BSMBS_18d, BSMBS_19d, BSMBS_20d), 
#                 triangle = "lower")

cor3dfup <- subset(Scale_14items, select = c(BSMBS_1u, BSMBS_3u, BSMBS_4u, BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u))
cor3 <- data.frame()
columns <- colnames(cor3dfup)
for (i in 1:(ncol(cor3dfup) - 1)) {
  for (j in (i + 1):ncol(cor3dfup)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor3dfup[[col1]], cor3dfup[[col2]]))
    
    cor3 <- bind_rows(cor3, test_result)
  }
}

cor4dfdown <- subset(Scale_14items, select = c(BSMBS_11d, BSMBS_13d, BSMBS_14d, BSMBS_16d, BSMBS_18d, BSMBS_19d, BSMBS_20d))
cor4 <- data.frame()
columns <- colnames(cor4dfdown)
for (i in 1:(ncol(cor4dfdown) - 1)) {
  for (j in (i + 1):ncol(cor4dfdown)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor4dfdown[[col1]], cor4dfdown[[col2]]))
    
    cor4 <- bind_rows(cor4, test_result)
  }
}

cor5 <- data.frame()
columns <- colnames(Scale_14items)
for (i in 1:(ncol(Scale_14items) - 1)) {
  for (j in (i + 1):ncol(Scale_14items)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(Scale_14items[[col1]], Scale_14items[[col2]]))
    
    cor5 <- bind_rows(cor5, test_result)
  }
}
######################################################################################################
"Items BSMBS_1u, BSMBS_3u and BSMBS_19d were deleated"



###### 4.3.2. Scale 11 items ####
Scale_11items <- subset(data1 %>% 
                          select(BSMBS_4u, 
                                 BSMBS_6u, BSMBS_8u,       
                                 BSMBS_9u, BSMBS_10u, BSMBS_11d, 
                                 BSMBS_13d, BSMBS_14d,       
                                 BSMBS_16d, 
                                 BSMBS_18d, BSMBS_20d))

## Reliability  
Scale_11items %>% # (Without items: "BSMBS_2u", "BSMBS_5u", "BSMBS_7u", BSMBS_1u, BSMBS_3u)
  select("BSMBS_4u", "BSMBS_6u", "BSMBS_8u",        
         "BSMBS_9u", "BSMBS_10u") %>%  
  psych::alpha(na.rm = T) # alpha = .87


Scale_11items %>% # (Without items: "BSMBS_12d", "BSMBS_15d", "BSMBS_17d", BSMBS_19d)
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_20d") %>%  
  psych::alpha(na.rm = T) # alpha = .87


## Difference between the correlation item- belonging factor and item-opposite factor
# FACTOR UP --> items DOWN 
Scale_11items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_11d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .61; r.drop (opposite factor) = .40 --> .61 - .40 = .21 


Scale_11items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_13d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .68; r.drop (opposite factor) = .37 --> .68 - .37 = .31 


Scale_11items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_14d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .69; r.drop (opposite factor) = .41 --> .69 - .41 = .28 


Scale_11items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_16d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .60; r.drop (opposite factor) = .54 --> .60 - .49 = .11 


Scale_11items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_18d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .71; r.drop (opposite factor) = .48 --> .71 - .48 = .23 


Scale_11items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_20d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .69; r.drop (opposite factor) = .46 --> .69 - .46 = .23 



# FACTOR DOWN --> items UP
Scale_11items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_20d",
         "BSMBS_4u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .68; r.drop (opposite factor) = .45 --> .68 - .45 = .23 


Scale_11items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_20d",
         "BSMBS_6u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .67; r.drop (opposite factor) = .39 --> .67 - .39 = .28 

Scale_11items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_20d",
         "BSMBS_8u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .73; r.drop (opposite factor) = .39 --> .73 - .39 = .34 


Scale_11items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_20d",
         "BSMBS_9u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .72; r.drop (opposite factor) = .50 --> .72 - .50 = .22 


Scale_11items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_16d", "BSMBS_18d", "BSMBS_20d",
         "BSMBS_10u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .69; r.drop (opposite factor) = .55 --> .69 - .55 = .14 



# Inter-Items Correlations
######################################################################################################
# UP
#sjPlot::tab_corr(Scale_11items %>%
#                   select (BSMBS_4u, BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u), 
#                 triangle = "lower")

# DOWN
#sjPlot::tab_corr(Scale_11items %>%
#                   select (BSMBS_11d, BSMBS_13d, BSMBS_14d,       
#                           BSMBS_16d, BSMBS_18d, BSMBS_20d), 
#                 triangle = "lower")

# BOTH
#sjPlot::tab_corr(Scale_11items %>%
#                   select (BSMBS_4u, BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u,
#                           BSMBS_11d, BSMBS_13d, BSMBS_14d,       
#                           BSMBS_16d, BSMBS_18d, BSMBS_20d), 
#                 triangle = "lower")

cor6dfup <- subset(Scale_11items, select = c(BSMBS_4u, BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u))
cor6 <- data.frame()
columns <- colnames(cor6dfup)
for (i in 1:(ncol(cor6dfup) - 1)) {
  for (j in (i + 1):ncol(cor6dfup)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor6dfup[[col1]], cor6dfup[[col2]]))
    
    cor6 <- bind_rows(cor6, test_result)
  }
}

cor7dfdown <- subset(Scale_11items, select = c(BSMBS_11d, BSMBS_13d, BSMBS_14d, BSMBS_16d, BSMBS_18d, BSMBS_20d))
cor7 <- data.frame()
columns <- colnames(cor7dfdown)
for (i in 1:(ncol(cor7dfdown) - 1)) {
  for (j in (i + 1):ncol(cor7dfdown)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor7dfdown[[col1]], cor7dfdown[[col2]]))
    
    cor7 <- bind_rows(cor7, test_result)
  }
}

cor8 <- data.frame()
columns <- colnames(Scale_11items)
for (i in 1:(ncol(Scale_11items) - 1)) {
  for (j in (i + 1):ncol(Scale_11items)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(Scale_11items[[col1]], Scale_11items[[col2]]))
    
    cor8 <- bind_rows(cor8, test_result)
  }
}
######################################################################################################
"Item BSMBS_16d were deleated"


###### 4.3.3. Scale 10 items ####
Scale_10items <- subset(data1 %>% 
                          select(BSMBS_4u, BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u, 
                                 BSMBS_11d, BSMBS_13d, BSMBS_14d, BSMBS_18d, BSMBS_20d))
                                  
## Reliability  
Scale_10items %>% # (Without items: "BSMBS_2u", "BSMBS_5u", "BSMBS_7u", BSMBS_1u, BSMBS_3u)
  select("BSMBS_4u", "BSMBS_6u", "BSMBS_8u",        
         "BSMBS_9u", "BSMBS_10u") %>%  
  psych::alpha(na.rm = T) # alpha = .87


Scale_10items %>% # (Without items: "BSMBS_12d", "BSMBS_15d", "BSMBS_16d", "BSMBS_17d", BSMBS_19d)
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_18d", "BSMBS_20d") %>%  
  psych::alpha(na.rm = T) # alpha = .85

                                 
## Difference between the correlation item- belonging factor and item-opposite factor
# FACTOR UP --> items DOWN 
Scale_10items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_11d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .63; r.drop (opposite factor) = .40 --> .63 - .40 = .23 


Scale_10items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_13d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .65; r.drop (opposite factor) = .37 --> .65 - .37 = .28


Scale_10items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_14d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .68; r.drop (opposite factor) = .41 --> .68 - .41 = .27


Scale_10items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_18d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .67; r.drop (opposite factor) = .48 --> .67 - .48 = .19


Scale_10items %>% 
  select("BSMBS_4u",       
         "BSMBS_6u", "BSMBS_8u", "BSMBS_9u", "BSMBS_10u",
         "BSMBS_20d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .71; r.drop (opposite factor) = .46 --> .71 - .46 = .25


# FACTOR DOWN --> items UP
Scale_10items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_18d", "BSMBS_20d",
         "BSMBS_4u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .68; r.drop (opposite factor) = .43 --> .68 - .43 = .25


Scale_10items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_18d", "BSMBS_20d",
         "BSMBS_6u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .67; r.drop (opposite factor) = .36 --> .67 - .36 = .31


Scale_10items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_18d", "BSMBS_20d",
         "BSMBS_8u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .73; r.drop (opposite factor) = .37 --> .73 - .37 = .36


Scale_10items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_18d", "BSMBS_20d",
         "BSMBS_9u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .72; r.drop (opposite factor) = .47 --> .72 - .47 = .25


Scale_10items %>% 
  select("BSMBS_11d", "BSMBS_13d", "BSMBS_14d",       
         "BSMBS_18d", "BSMBS_20d",
         "BSMBS_10u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .69; r.drop (opposite factor) = .54 --> .69 - .54 = .15


# Inter-Items Correlations
######################################################################################################
# UP
#sjPlot::tab_corr(Scale_10items %>%
#                   select (BSMBS_4u, BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u), 
#                 triangle = "lower")
#
# DOWN
#sjPlot::tab_corr(Scale_10items %>%
#                   select (BSMBS_11d, BSMBS_13d, BSMBS_14d, BSMBS_18d, BSMBS_20d), 
#                 triangle = "lower")
#
# BOTH
#sjPlot::tab_corr(Scale_10items %>%
#                   select (BSMBS_4u, BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u,
#                           BSMBS_11d, BSMBS_13d, BSMBS_14d,       
#                           BSMBS_18d, BSMBS_20d), 
#                 title = "Table 1. Correlations",
#                 triangle = "lower")

cor9dfup <- subset(Scale_10items, select = c(BSMBS_4u, BSMBS_6u, BSMBS_8u, BSMBS_9u, BSMBS_10u))
cor9 <- data.frame()
columns <- colnames(cor9dfup)
for (i in 1:(ncol(cor9dfup) - 1)) {
  for (j in (i + 1):ncol(cor9dfup)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor9dfup[[col1]], cor9dfup[[col2]]))
    
    cor9 <- bind_rows(cor9, test_result)
  }
}

cor10dfdown <- subset(Scale_10items, select = c(BSMBS_11d, BSMBS_13d, BSMBS_14d, BSMBS_18d, BSMBS_20d))
cor10 <- data.frame()
columns <- colnames(cor10dfdown)
for (i in 1:(ncol(cor10dfdown) - 1)) {
  for (j in (i + 1):ncol(cor10dfdown)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor10dfdown[[col1]], cor10dfdown[[col2]]))
    
    cor10 <- bind_rows(cor10, test_result)
  }
}

cor11 <- data.frame()
columns <- colnames(Scale_10items)
for (i in 1:(ncol(Scale_10items) - 1)) {
  for (j in (i + 1):ncol(Scale_10items)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(Scale_10items[[col1]], Scale_10items[[col2]]))
    
    cor11 <- bind_rows(cor11, test_result)
  }
}
######################################################################################################

"Items BSMBS_6u, BSMBS_20d because of inter-item correlation were deleated"


###### 4.3.4. Scale 8 items ####
Scale_08items <- subset(data1 %>% 
                          select(BSMBS_4u, BSMBS_8u,
                                 BSMBS_9u, BSMBS_10u, 
                                 BSMBS_11d, BSMBS_13d, 
                                 BSMBS_14d, BSMBS_18d))


## Reliability 
Scale_08items %>% 
  select("BSMBS_4u", "BSMBS_8u",        
         "BSMBS_9u", "BSMBS_10u") %>%  
  psych::alpha(na.rm = T) # alpha = .86


Scale_08items %>% 
  select("BSMBS_11d", "BSMBS_13d",        
         "BSMBS_14d", "BSMBS_18d") %>%  
  psych::alpha(na.rm = T) # alpha = .81


## Difference between the correlation item- belonging factor and item-opposite factor
Scale_08items %>% 
  select("BSMBS_4u", "BSMBS_8u",       
         "BSMBS_9u", "BSMBS_10u",
         "BSMBS_11d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .61; r.drop (opposite factor) = .42 --> .61 - .42 = .19


Scale_08items %>% 
  select("BSMBS_4u", "BSMBS_8u",       
         "BSMBS_9u", "BSMBS_10u",
         "BSMBS_13d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .65; r.drop (opposite factor) = .38 --> .65 - .38 = .27


Scale_08items %>% 
  select("BSMBS_4u", "BSMBS_8u",       
         "BSMBS_9u", "BSMBS_10u",
         "BSMBS_14d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .64; r.drop (opposite factor) = .44 --> .64 - .44 = .20


Scale_08items %>% 
  select("BSMBS_4u", "BSMBS_8u",       
         "BSMBS_9u", "BSMBS_10u",
         "BSMBS_18d") %>% 
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .62; r.drop (opposite factor) = .49 --> .62 - .49 = .13


# FACTOR DOWN --> items UP
Scale_08items %>% 
  select("BSMBS_11d", "BSMBS_13d",        
         "BSMBS_14d", "BSMBS_18d", 
         "BSMBS_4u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .68; r.drop (opposite factor) = .42 --> .68 - .42 = .26



Scale_08items %>%
  select("BSMBS_11d", "BSMBS_13d",        
         "BSMBS_14d", "BSMBS_18d", 
         "BSMBS_8u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .69; r.drop (opposite factor) = .36 --> .69 - .36 = .33


Scale_08items %>%
  select("BSMBS_11d", "BSMBS_13d",        
         "BSMBS_14d", "BSMBS_18d", 
         "BSMBS_9u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .70; r.drop (opposite factor) = .49 --> .70 - .49 = .21


Scale_08items %>%
  select("BSMBS_11d", "BSMBS_13d",        
         "BSMBS_14d", "BSMBS_18d", 
         "BSMBS_10u") %>%  
  psych::alpha(na.rm = T, check.keys = T) # raw.drop (belonging factor) = .69; r.drop (opposite factor) = .53 --> .69 - .53 = .16


## Descriptive Statistics 
Scale_08items %>% 
  dplyr::na_if(NA)%>% 
  psych::describe() 


## Inter-Items Correlations
######################################################################################################
#sjPlot::tab_corr(Scale_08items %>%
#                   select (BSMBS_4u, BSMBS_8u,       
#                          BSMBS_9u, BSMBS_10u, 
#                           BSMBS_11d, BSMBS_13d,
#                           BSMBS_14d, BSMBS_18d), 
#                 triangle = "both")

cor12 <- data.frame()
columns <- colnames(Scale_08items)
for (i in 1:(ncol(Scale_08items) - 1)) {
  for (j in (i + 1):ncol(Scale_08items)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(Scale_08items[[col1]], Scale_08items[[col2]]))
    
    cor12 <- bind_rows(cor12, test_result)
  }
}
######################################################################################################


#---------------------------------------------------------------------------#
#### 5. Exploratory analysis: Correlations between BSMBS and other constructs ####
#---------------------------------------------------------------------------#
## Descriptive Statistics 
data2 %>% 
  dplyr::na_if(NA)%>%
  select(Mean_Up, Mean_Down, Mean_SMBS, Mean_SEIS, SSS, PoliticalOrientation) %>%
  psych::describe() 


## Correlations analysis
######################################################################################################
#data2 %>% 
#  dplyr::na_if(NA)%>%
#  select(Mean_Up, Mean_Down, Mean_SMBS, Mean_SEIS, SSS, PoliticalOrientation) %>%
#  tab_corr(na.deletion = c("listwise"),
#           corr.method = c("pearson"),
#           title = "Tabla 1. EFA",
#           var.labels = NULL,
#           show.p = T,
#           p.numeric = F,
#           fade.ns = T,
#           val.rm = NULL,
#           digits = 3,
#           triangle = "lower",
#           string.diag = NULL,
#           encoding = NULL,
#           file = NULL,
#           use.viewer = T,
#           remove.spaces = T)

cor13df <- subset(data2, select = c(Mean_Up, Mean_Down, Mean_SMBS, Mean_SEIS, SSS, PoliticalOrientation))
cor13 <- data.frame()
columns <- colnames(cor13df)
for (i in 1:(ncol(cor13df) - 1)) {
  for (j in (i + 1):ncol(cor13df)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor13df[[col1]], cor13df[[col2]]))
    
    cor13 <- bind_rows(cor13, test_result)
  }
}
###################################################################################################### 


#--------------------------------------------------------------#
#### 6. Bidimensional Social Mobility Beliefs Scale (BSMBS) ####
#--------------------------------------------------------------#

## Upward social mobility dimension:
# Item1 (BSMS_4u): En España, es frecuente que los(as) hijos(as) consigan un estatus socioeconómico superior al del hogar en el que crecieron (In Spain, children often achieve a higher socio-economic status than the household in which they grew up).
# Item2 (BSMS_8u): Los/as hijos/as de las personas españolas llegan a pertenecer a una clase social más alta en comparación con la clase de la que provienen (The children of Spanish people come to belong to a higher social class compared to the class they come from).
# Item3 (BSMS_9u): La mayoría de la población española mejora su estatus socioeconómico a lo largo de su vida (The majority of the Spanish population improves their socio-economic status throughout their lives).
# Item4 (BSMS_10u): Generalmente, en España, los(as) hijos(as) tienen mejores puestos de trabajo de una generación a otra (In Spain, children in general have better Occupations from one generation to the next).

## Downward social mobility dimension:
# Item5 (BSMS_11d): En la sociedad española, la mayoría de las personas tienen ingresos más bajos de una generación a otra (In Spanish society, most people have lower incomes from one generation to the next).
# Item6 (BSMS_13d): La mayoría de las familias españolas ocupan posiciones sociales inferiores a las de la generación anterior (The majority of Spanish families have lower social positions than the previous generation).
# Item7 (BSMS_14d): En España, es frecuente que los(as) hijos(as) consigan un estatus socioeconómico inferior al del hogar en el que crecieron (In Spain, children often achieve a lower socio-economic status than the household in which they grew up).
# Item8 (BSMS_18d): Los/as hijos/as de las personas españolas llegan a pertenecer a una clase social más baja en comparación con la clase de la que provienen (The children of Spanish people come to belong to a lower social class compared to the class they come from).

"IMPORTANT: The English version has not been validated. It is presented for illustration purposes"



#-----------------------------------#
#### 7. Supplementary Material ####
#-----------------------------------#
###### Exploratory analyses between items from BSMBS and SMBS ####
DSMBS_and_SMBS <- subset(data1 %>% 
                           select(BSMBS_4u, BSMBS_8u, BSMBS_9u, BSMBS_10u,       
                                  BSMBS_11d, BSMBS_13d, BSMBS_14d, BSMBS_18d, 
                                  SMBS_1, SMBS_2, SMBS_3, SMBS_4, 
                                  SMBS_5, SMBS_6, SMBS_7, SMBS_8))


parallel_DSMBS_and_SMBS <- fa.parallel(DSMBS_and_SMBS, fm = 'ml', fa = 'fa') # 3 factors

efa_m_DSMBS_and_SMBS <- fa(DSMBS_and_SMBS, nfactors = 3, rotate = "oblimin", fm="pa")

print.psych(efa_m_DSMBS_and_SMBS, cut = 0.3, sort = T)



##### Exploratory Analyses for BSMBS ####
# EFA for 8 items scale
efa_m_Scale_08items <- psych::fa(Scale_08items,
                                 nfactors = 2,
                                 rotate = "oblimin",
                                 fm ="pa")

# with correlation poly
efa_m_cor_poly_Scale_08items <- psych::fa(Scale_08items, 
                                        nfactors = 2, 
                                        rotate = "oblimin", 
                                        fm ="pa",
                                        cor = "poly")


# with bootstrapping (= 5000)
#efa_m_Bootstrapping_Scale_08items <- psych::fa(Scale_08items, 
                                              # nfactors = 2, 
                                              # rotate = "oblimin", 
                                              # fm ="pa",
                                              # n.iter=5000)


# print
#print.psych(efa_m_Scale_08items, cut = 0.3, sort = T)
#print.psych(efa_m_cor_poly_Scale_08items, cut = 0.3, sort = T)
#print.psych(efa_m_Bootstrapping_Scale_08items, cut = 0.3, sort = T)

results <- bind_rows(
  cor.test1 = cor1,
  cor.test2 = cor2,
  cor.test3 = cor3,
  cor.test4 = cor4,
  cor.test5 = cor5,
  cor.test6 = cor6,
  cor.test7 = cor7,
  cor.test8 = cor8,
  cor.test9 = cor9,
  cor.test10 = cor10,
  cor.test11 = cor11,
  cor.test12 = cor12,
  cor.test13 = cor13,
  .id = "model"
)

write_csv(results, "SocialMobility/Matamoros-lima_1/gen_data/results/Matamoros-lima_1_results_synthpop.csv")


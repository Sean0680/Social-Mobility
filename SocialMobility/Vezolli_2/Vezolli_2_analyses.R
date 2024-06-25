# Read data
library(readxl)
library(broom)
library(dplyr)
library(lavaan)

data_use <- Vezolli_2_synthpop

split_df <- split(data_use, data_use$country)

uk <- split_df$uk
ita <- split_df$ita
za <- split_df$za

ccias.model <- 'rich =~ ccias.rich
               poor =~ ccias.poor
              fatal =~ ccias.fatal'
### UK  
set.seed(1234)
cfa.ccias.uk <- cfa(ccias.model,
                    data = uk,
                    std.lv=TRUE,
                    estimator = "ML")

### Italy  
set.seed(1234)
cfa.ccias.ita <- cfa(ccias.model,
                     data = ita,
                     std.lv=TRUE,
                     estimator = "ML")

### South Africa  
set.seed(1234)
cfa.ccias.za <- cfa(ccias.model,
                    data = za,
                    std.lv=TRUE,
                    estimator = "ML")

# Correlational pattern 
cor1df <- subset(ita, select = c(ccias.rich,ccias.poor,ccias.fatal,Between.Inequality.Wide,Between.Redistribution,Between.Migration,Between.Fairness,Moralization,
                                 Moral.Outrage,Position.Own.Country,General.Trust.Inst,trust.institution,Horizontal.Trust,zero.sum,meritocracy,social.dominance,eco.system.just,
                                 identif.country,identif.world,subjective.ses,Pol.Orientation,Age,Gender,Education))

cor1 <- data.frame()
columns <- colnames(cor1df)
for (i in 1:(ncol(cor1df) - 1)) {
  for (j in (i + 1):ncol(cor1df)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor1df[[col1]], cor1df[[col2]]))
    
    cor1 <- bind_rows(cor1, test_result)
  }
}


## UK  
cor2df <- subset(uk, select = c(ccias.rich,ccias.poor,ccias.fatal,Between.Inequality.Wide,Between.Redistribution,Between.Migration,Between.Fairness,Moralization,Moral.Outrage,
                                Position.Own.Country,General.Trust.Inst,trust.institution,Horizontal.Trust,zero.sum,meritocracy,social.dominance,eco.system.just,identif.country,
                                identif.world,subjective.ses,Pol.Orientation,Age,Gender,Education))

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

## South Africa  
cor3df <- subset(za, select = c(ccias.rich,ccias.poor,ccias.fatal,Between.Inequality.Wide,Between.Redistribution,Between.Migration,Between.Fairness,Moralization,Moral.Outrage,
                                Position.Own.Country,General.Trust.Inst,trust.institution,Horizontal.Trust,zero.sum,meritocracy,social.dominance,eco.system.just,identif.country,identif.world,
                                subjective.ses,Pol.Orientation,Age,Gender,Education))

cor3 <- data.frame()
columns <- colnames(cor3df)
for (i in 1:(ncol(cor3df) - 1)) {
  for (j in (i + 1):ncol(cor3df)){
    col1 <- columns[i]
    col2 <- columns[j]
    
    test_result <- tidy(cor.test(cor3df[[col1]], cor3df[[col2]]))
    
    cor3 <- bind_rows(cor3, test_result)
  }
}

data <- rbind(uk[,c("country","ccias.rich","ccias.poor","ccias.fatal","Between.Inequality.Wide","Between.Redistribution","Between.Migration",
                    "Moralization","Moral.Outrage","Gender_fac","Age")],ita[,c("country","ccias.rich","ccias.poor","ccias.fatal","Between.Inequality.Wide",
                                                                               "Between.Redistribution","Between.Migration","Moralization","Moral.Outrage","Gender_fac","Age")])

data <- rbind(data[,c("country","ccias.rich","ccias.poor","ccias.fatal","Between.Inequality.Wide","Between.Redistribution","Between.Migration",
                      "Moralization","Moral.Outrage","Gender_fac","Age")],za[,c("country","ccias.rich","ccias.poor","ccias.fatal","Between.Inequality.Wide","Between.Redistribution",
                                                                                "Between.Migration","Moralization","Moral.Outrage","Gender_fac","Age")])

mod.wide <- lm(Between.Inequality.Wide~ccias.rich+ccias.poor+ccias.fatal+country, data = data)
mod.wide.demo <- lm(Between.Inequality.Wide~ccias.rich+ccias.poor+ccias.fatal+country+Gender_fac+Age, data = data)

mod.redr <- lm(Between.Redistribution~ccias.rich+ccias.poor+ccias.fatal+country, data = data)
mod.redr.demo <- lm(Between.Redistribution~ccias.rich+ccias.poor+ccias.fatal+country+Gender_fac+Age, data = data)

mod.migr <- lm(Between.Migration~ccias.rich+ccias.poor+ccias.fatal+country, data = data)
mod.migr.demo <- lm(Between.Migration~ccias.rich+ccias.poor+ccias.fatal+country+Gender_fac+Age, data = data)

mod.moral <- lm(Moralization~ccias.rich+ccias.poor+ccias.fatal+country, data = data)
mod.moral.demo <- lm(Moralization~ccias.rich+ccias.poor+ccias.fatal+country+Gender_fac+Age, data = data)

mod.rage <- lm(Moral.Outrage~ccias.rich+ccias.poor+ccias.fatal+country, data = data)
mod.rage.demo <- lm(Moral.Outrage~ccias.rich+ccias.poor+ccias.fatal+country+Gender_fac+Age, data = data)

results = bind_rows(
  lm1 = tidy(mod.moral),
  lm2 = tidy(mod.moral.demo),
  lm3 = tidy(mod.rage),
  lm4 = tidy(mod.rage.demo),
  lm5 = tidy(mod.redr),
  lm6 = tidy(mod.redr.demo),
  lm7 = tidy(mod.wide),
  lm8 = tidy(mod.wide.demo),
  
  cfa1 = tidy(cfa.ccias.uk),
  cfa2 = tidy(cfa.ccias.ita),
  cfa3 = tidy(cfa.ccias.za),
  
  cor.test1 = cor1,
  cor.test2 = cor2,
  cor.test3 = cor3,
  .id = "model"
)

write_csv(results, "C:/Users/srtui/Downloads/Master Thesis/SocialMobility/Vezolli_2/gen_data/results/Vezolli_2_results_synthpop.csv")

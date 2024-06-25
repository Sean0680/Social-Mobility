library(dplyr)

uk <- Vezolli_2_uk_original
# Load Italy
ita <- Vezolli_2_it_original
# Load South Africa
za <- Vezolli_2_sa_original

# Create a country variable
uk$country <- "uk"
ita$country <- "ita"
za$country <- "za"

combined <- rbind(uk, ita, za)

combined_sub <- subset(combined, select = -c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,27,28,29,30,37,38,39,40,61,62,63,64,91,92,93,94,107,108,109,110,124,125,126,127,
                                          138,139,140,141,157,158,159,160,161,162,163))

combined_sub <- combined_sub%>%
  mutate(working.status_fac=case_when(
    working.status_fac=="Employed.fulltime" ~ 1,
    working.status_fac=="Employed.parttime" ~ 2,
    working.status_fac=="Employed.less15hweek" ~ 3,
    working.status_fac=="Apprentice" ~ 4,
    working.status_fac=="Unemployed.lookforjob" ~ 5,
    working.status_fac=="Unable.to.work" ~ 6,
    working.status_fac=="Student" ~ 7,
    working.status_fac=="Housekeeping" ~ 8,
    working.status_fac=="Retired" ~ 9
  ))

combined_sub <- combined_sub%>%
  mutate(Position.Own.Country_fac=case_when(
    Position.Own.Country_fac=="poorest20%" ~ 1,
    Position.Own.Country_fac=="second.poorest20%" ~ 2,
    Position.Own.Country_fac=="middle20%" ~ 3,
    Position.Own.Country_fac=="second.richest20%" ~ 4,
    Position.Own.Country_fac=="richest20%" ~ 5,
  ))

combined_sub <- combined_sub%>%
  mutate(Gender_fac=case_when(
    Gender_fac=="male" ~ 1,
    Gender_fac=="female" ~ 2,
    Gender_fac=="non-binary" ~ 3,
  ))



write_csv(combined_sub, "C:/Users/srtui/Downloads/Master Thesis/SocialMobility/Vezolli_2/Vezolli_2_complete_original.csv")


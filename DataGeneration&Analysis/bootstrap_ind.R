set.seed(111)
library(dplyr)

data_lw <- Vezolli_2_complete_original
resampled_data_frame <- list()

for (i in colnames(data_lw)) {
  resampled_data_frame[[i]] <- sample(data_lw[[i]], length(data_lw[[i]]), replace = TRUE)
  resampled_data_frame <- as.data.frame(resampled_data_frame)
}

write.csv(resampled_data_frame, "SocialMobility/Vezolli_2/gen_data/Vezolli_2_bootstrap_ind.csv")

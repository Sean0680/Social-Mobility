set.seed(111)
library(dplyr)

data_lw <- Vezolli_2_complete_original

num_bootstraps <- 1

bootstrapped_datasets <- list()

for (i in 1:num_bootstraps) {
  bootstrap_sample <- data_lw[sample(nrow(data_lw), replace = TRUE), ]
  
  bootstrapped_datasets[[i]] <- bootstrap_sample
}

write.csv(bootstrap_sample, "SocialMobility/Vezolli_2/gen_data/Vezolli_2_bootstrap.csv")

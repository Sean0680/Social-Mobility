set.seed(111)
library(synthpop)
library(readr)

original_data = Vezolli_2_complete_original

gen_data <- syn(data = original_data)

summary(gen_data)

write.syn(gen_data, file = paste0("C:/Users/srtui/Downloads/Master Thesis/SocialMobility/Vezolli_2/gen_data/", "Vezolli_2_synthpop"), 
          filetype ="csv",convert.factors = "numeric", data.labels = NULL, save.complete = TRUE)

compare(gen_data, original_data)

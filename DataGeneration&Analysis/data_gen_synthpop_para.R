set.seed(111)
library(synthpop)
library(readr)

original_data = Vezolli_2_complete_original

gen_data <- syn(data = original_data, method = "parametric", default.method = c("normrank", "logreg", "polyreg", "polr"))

write.syn(gen_data, file = paste0("C:/Users/srtui/Downloads/Master Thesis/SocialMobility/Vezolli_2/gen_data/", "Vezolli_2_synthpop_para"), 
          filetype ="csv",convert.factors = "numeric", data.labels = NULL, save.complete = TRUE)


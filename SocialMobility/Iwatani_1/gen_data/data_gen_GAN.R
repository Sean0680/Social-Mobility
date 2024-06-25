library(readr)
library(RGAN)

data <- read_csv("SocialMobility/Iwatani_1/Iwatani_1_original.csv")
data_lw <- na.omit(data) 
data_lw <- as.data.frame(data_lw)

use_cuda <- torch::cuda_is_available()
device <- ifelse(use_cuda, "cuda", "cpu")

synthetic_examples <- nrow(data_lw)

transformer <- data_transformer$new()
transformer$fit(data_lw)
transformed_data <- transformer$transform(data_lw)

#Training on transformed_data, generates table with only NaN.
#Training on original data, generates a table with values
set.seed(111)
data_lw <- as.matrix(data_lw)

res <- gan_trainer(
  data_lw,
  synthetic_examples = synthetic_examples)


synth_data <- sample_synthetic_data(res, transformer)

#Optimize GAN
res_cont <- gan_trainer(data_lw,
                        generator = res$generator,
                        discriminator = res$discriminator,
                        generator_optimizer = res$generator_optimizer,
                        discriminator_optimizer = res$discriminator_optimizer,
                        epochs = 150,
                        synthetic_examples = synthetic_examples
)

synth_data_cont <- sample_synthetic_data(res_cont, transformer)

write.csv(synth_data, "SocialMobility/Iwatani_1/gen_data/Iwatani_1_gen_GAN.csv")
write.csv(synth_data, "SocialMobility/Iwatani_1/gen_data/Iwatani_1_gen_GAN_optimized.csv")

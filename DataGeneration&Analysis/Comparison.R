library(tidyverse)
library(dplyr)

#load datasets
original = Berger_results_original
bstrap = Berger_results_bootstrap
bstrap_ind = Berger_results_bootstrap_ind
spop = Berger_results_synthpop
spop_para = Berger_results_synthpop_para

#select the columns needed
original <- subset(original, select = c(model, estimate,std.error))
bstrap <- subset(bstrap, select = c(model, estimate,std.error))
bstrap_ind <- subset(bstrap_ind, select = c(model, estimate, std.error))
spop <- subset(spop, select = c(model, estimate, std.error))
spop_para <- subset(spop_para, select = c(model, estimate, std.error))

#choose a value for the study and the gen methods
study <- "Berger"
gen_methods <- list(bstrap, bstrap_ind, spop, spop_para)
gen_list <- c("bstrap", "bstrap_ind", "spop", "spop_para")

#creates a list with 4 dataframes and adds columns "study" and "gen_method" + the needed columns from the original dataset
#to each dataframe in the list
for (i in 1:length(gen_methods)) {
  gen_methods[[i]]$study <- study
  gen_methods[[i]]$gen_method <- gen_list[i]
  gen_methods[[i]] <- cbind(gen_methods[[i]], orig_est = original$estimate)
  gen_methods[[i]] <- cbind(gen_methods[[i]], orig_std.error = original$std.error)
  #gen_methods[[i]] <- cbind(gen_methods[[i]], orig_conf.low = original$conf.low)
  #gen_methods[[i]] <- cbind(gen_methods[[i]], orig_conf.high = original$conf.high)
}

#CI overlap
for (i in 1:length(gen_methods)) {
  for (j in 1:nrow(gen_methods[[i]])){
    
    min_upper <- pmin(gen_methods[[i]]$orig_conf.high, gen_methods[[i]]$conf.high, na.rm = TRUE)
    max_lower <- pmax(gen_methods[[i]]$orig_conf.low, gen_methods[[i]]$conf.low, na.rm = TRUE)
    
    ci_numerator <- min_upper - max_lower
    
    int_orig <- gen_methods[[i]]$orig_conf.high - gen_methods[[i]]$orig_conf.low
    int_synthd <- gen_methods[[i]]$conf.high - gen_methods[[i]]$conf.low
    
    ci_orig <- ci_numerator/int_orig
    ci_synthd <- ci_numerator/int_synthd
    
    ci_total <-ci_orig + ci_synthd
    
    CIO <- 0.5 * ci_total
    
    gen_methods[[i]]$CI_overlap <- CIO
  }
}

#mean CI Overlap
for (i in 1:length(gen_methods)) {
  for (j in 1:nrow(gen_methods[[i]])){
    gen_methods[[i]]$mean_CI_overlap <- mean(gen_methods[[i]]$CI_overlap, na.rm = TRUE)
  }
}

#SE
for (i in 1:length(gen_methods)) {
  for (j in 1:nrow(gen_methods[[i]])){
    test <- gen_methods[[i]]$orig_est - gen_methods[[i]]$estimate
    #test <- test / gen_methods[[i]]$orig_std.error
    test <- test^2
    test[test == Inf] <- NA
    gen_methods[[i]]$SE <- test
  }
}

#MSE
for (i in 1:length(gen_methods)) {
  for (j in 1:nrow(gen_methods[[i]])){
    gen_methods[[i]]$MSE <- mean(gen_methods[[i]]$SE, na.rm = TRUE)
  }
}
#make a df of the list of dataframes
combined_df <- bind_rows(gen_methods, .id = "study")

#extra stuff to clean up dataframe
combined_df$study <- study
combined_df$model <- gsub("[0-9]", "", combined_df$model)

#write to CSV and after combine all the csv
write.csv(combined_df, "SocialMobility/combined/combined_df6.csv")
combined_df <- list.files(path = "SocialMobility/combined/", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
write.csv(combined_df, "SocialMobility/combined/combined_df_complete.csv")

############################################################################################
#for everything above this, results from all the studies are needed
#The lower part can be ran with combined_df_complete.csv
############################################################################################

combined_df <- combined_df_complete
combined_df <- subset(combined_df, select = -c(1,2,3,4,5,6))

options(repr.plot.width = 1, repr.plot.height = 1)

#plots
#MSE PLOT
grouped_df_MSE <- aggregate(MSE ~ model + gen_method,
                            data = combined_df,
                            function(x) {
                              c(MSE=mean(x))
                            })

#write.csv(grouped_df_MSE, "SocialMobility/combined/MSE_study.csv")

mse_p <- ggplot(grouped_df_MSE, aes(x = gen_method, y = MSE)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(colour = model),
              position = position_jitter(height = 0, width = 0)) +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype=1),
        axis.ticks = element_line(linewidth = 1, colour = "black"),
        axis.ticks.length = unit(.2, "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans='log10') +
  ggtitle("MSE Scores of the Different Generation Methods") +
  xlab("Synthetic Data Generation Methods")
mse_p

#CI PLOT
grouped_df_ci <- aggregate(mean_CI_overlap ~ model + gen_method,
                            data = combined_df,
                            function(x) {
                              c(CI_overlap=mean(x))
                            })

#write.csv(grouped_df_ci, "SocialMobility/combined/CI_study.csv")

ci_p <- ggplot(grouped_df_ci, aes(x = gen_method, y = mean_CI_overlap)) + 
  geom_boxplot() +
  geom_jitter(aes(color = model),
              position = position_jitter(height = 0, width = 0)) +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype=1),
        axis.ticks = element_line(linewidth = 1, colour = "black"),
        axis.ticks.length = unit(.2, "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Mean CI Overlap Scores")

ci_p


ggsave("SocialMobility/plot4.png", plot = ci_p, width = 10, height = 6, scaling = 2)

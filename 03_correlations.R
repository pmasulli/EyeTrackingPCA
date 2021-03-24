clinical_scores <- read.csv(file.path(data_dir, "clinical_scores.csv"))
clinical_scores$Name <- as.character(clinical_scores$Name)

regression_data <- as.data.frame(mydata.pca$x)
regression_data$Name <- as.character(participants)
regression_data <- inner_join(clinical_scores, regression_data, by = "Name")
regression_data$Name <- NULL

# Correlations to report ----
cor.test(regression_data$PC2, regression_data$AQ_SUM)
cor.test(regression_data$PC2, regression_data$bdi_sum)
cor.test(regression_data$PC2, regression_data$wurs_sum)

cor.test(regression_data$bdi_sum, regression_data$AQ_SUM)


l <- lm(regression_data$PC2 ~ regression_data$bdi_sum + regression_data$AQ_SUM)   # run linear regression
l
summary(l)

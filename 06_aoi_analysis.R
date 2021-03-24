aoi_metrics_avg <- read.csv(file.path(data_dir, "aoi_metrics.csv"))
aoi_metrics_avg$Name <- as.character(aoi_metrics_avg$Name)

aoi_data <- inner_join(clinical_scores, aoi_metrics_avg, by = "Name")

aoi_data <- aoi_data[!is.na(aoi_data$AQ_SUM), ]
aoi_data <- aoi_data[!is.na(aoi_data$bdi_sum), ]


y <- aoi_data[, c("Central", "EyeRegion", "LeftFace", "MouthRegion", "RightFace")]

for (variable in c("AQ_SUM", "bdi_sum", "wurs_sum")) {
    print(variable)
    t <- corr.test(aoi_data[, variable], y)
    print(t, short = T, digits = 4)
    print("-----------")
}

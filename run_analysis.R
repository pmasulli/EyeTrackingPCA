library(plyr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(psych)
library(snow)
library(caret)


options(digits = 4)

readRenviron(".Renviron")

data_dir <- Sys.getenv("DATA_DIR")
output_dir <- file.path(data_dir, "output")
dir.create(output_dir, showWarnings = FALSE)


screenSize <- c(1920, 1080)

##### This is a small area around the rectangle
colstart <- 625
colend <- 1300
rowstart <- 250
rowend <- 850

dependent_variables <- c("AQ_SUM", "bdi_sum", "wurs_sum")



source("functions.R")


zz <- file(file.path(output_dir, "output.txt"), open = "wt")
sink(zz)
sink(zz, type = "message")

# Compute participant heatmaps ----
message("\n\nSTEP 1: Kernel convolution")
source("01_kernel_convolution.R")

# Compute PCA ----
message("\n\nSTEP 2: PCA")
source("02_pca.R")

# Correlations ----
message("\n\nSTEP 3: Correlations")
source("03_correlations.R", echo = T)

# Feature selection ----
message("\n\nSTEP 4: Feature selection")
source("04_feature_selection.R", echo=T)

# Compute linear models and draw heat maps ----
message("\n\nSTEP 5: Compute linear models")
source("05_linear_model.R", echo = T)

# Compare with AOI-based analysis ----
message("\n\nSTEP 6: Compare with AOI-based analysis")
source("06_aoi_analysis.R", echo = T)

sink(type='message')
sink()



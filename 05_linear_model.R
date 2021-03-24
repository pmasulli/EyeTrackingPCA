dependent_variables <- c("AQ_SUM", "bdi_sum", "wurs_sum")

for (dependent_variable in dependent_variables) {
    
    # only keep the chosen dependent variable and remove NAs
    r <- regression_data[, c(dependent_variable, setdiff(colnames(regression_data), dependent_variables))]
    r <- r[!is.na(r[, dependent_variable]), ]
    
    if (dependent_variable == "AQ_SUM") {
        linear_model <- lm(as.formula(paste0(dependent_variable, " ~ PC2 + PC3 + PC7")), r)
        possible_scores <- c(1, 15, 30, 40, 50)
    } else if (dependent_variable == "bdi_sum") {
        linear_model <- lm(as.formula(paste0(dependent_variable, " ~ PC2 + PC5")), r)
        possible_scores <- c(1, 15, 30, 45, 60)
    } else if (dependent_variable == "wurs_sum") {
        linear_model <- lm(as.formula(paste0(dependent_variable, " ~ PC17")), r)
        possible_scores <- c(1, 25, 50, 75, 90)
    }

    summary(linear_model)
    
    score_heatmaps <- matrix(0, nrow = nrow(components), ncol = length(possible_scores))
    score_plots <- list()
    
    predictor_names <- names(linear_model$coefficients)[2 : length(linear_model$coefficients)]
    chosen_components <- which(colnames(r) %in% predictor_names) - 1
    
    selected_eigen_heatmaps <- components[, predictor_names]
    
    
    i <- 1

        for (target_score in possible_scores) {
        message(target_score)
        
        alpha <- (target_score - linear_model$coefficients[1]) /
            sum(linear_model$coefficients[2 : length(linear_model$coefficients)]^2)
        message(paste("alpha =", alpha))
        target_coefficients <- alpha * linear_model$coefficients[2 : length(linear_model$coefficients)]
        
        target_coefficients <- as.data.frame(matrix(target_coefficients, nrow = 1))
        colnames(target_coefficients) <- predictor_names
        check_score <- stats::predict.lm(linear_model, newdata = target_coefficients)
        
        generated_heatmap <- as.numeric(as.matrix(selected_eigen_heatmaps) %*% t(as.matrix(target_coefficients)))
        generated_heatmap <- generated_heatmap + d_center
        
        generated_heatmap <- ((generated_heatmap - min(generated_heatmap)) / (max(generated_heatmap) - min(generated_heatmap)))
    
        if (i > 2) {
            ii <- generated_heatmap < quantile(generated_heatmap, 0.8)
            generated_heatmap[ii] <- 0.5 * generated_heatmap[ii]
        }
        
        score_heatmaps[, i] <- generated_heatmap
        
        if (dependent_variable == "AQ_SUM") {
            t <- "AQ = %d"
        } else if (dependent_variable == "bdi_sum") {
            t <- "BDI = %d"
        } else {
            t <- "WURS = %d"
        }
        
        i <- i + 1
    }
}
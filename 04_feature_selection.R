variance_percentage <- 90
outer_k <- nrow(regression_data)
inner_k <- 10 # inner CV is k-fold
nvmax <- 80
n_features_to_use <- 18 # based on 90% variance


perform_feature_selection <- function(regression_data, dependent_variable, save_graphs = TRUE, dependent_variable_max = NULL,
                                      random_seed = 4321, output_dir_fs = ".") {

    
    message("Starting feature selection algorithm.")
    message("PARAMETERS")
    message(sprintf("dependent_variable=%s", dependent_variable))
    message(sprintf("variance_percentage=%s", variance_percentage))
    message(sprintf("output_dir_fs=%s", output_dir_fs))
    message(sprintf("outer_k=%d", outer_k))
    message(sprintf("inner_k=%d", inner_k))
    message(sprintf("nvmax=%d", nvmax))
    
    # only keep the chosen dependent variable and remove NAs
    regression_data <- regression_data[, c(dependent_variable, setdiff(colnames(regression_data), dependent_variables))]
    regression_data <- regression_data[!is.na(regression_data[, dependent_variable]), ]
    
    regression_data <- regression_data[, 1:(n_features_to_use + 1)]
    
    
    # Set seed for reproducibility
    set.seed(random_seed)
    
    message(paste("Performing feature selection for variable:", dependent_variable))
    message(sprintf("There are %d features in the data set.", ncol(regression_data) - 1))
    
    logit <- function(x) log(x / ( 1 - x ))
    if (is.null(dependent_variable_max)) {
        dependent_variable_max <- 1.2 * max(regression_data[, dependent_variable])
    }

    predictor_names <- colnames(regression_data)[2:ncol(regression_data)]
    message(paste(predictor_names, collapse = ", "))
    
    outer_folds <- createFolds(regression_data[, dependent_variable], k = outer_k,
                               list = TRUE, returnTrain = TRUE)
    
    message(sprintf("Number of outer CV folds: %d", outer_k))
    
    # get the number of cores from cluster
    num_cores <- as.numeric(Sys.getenv('LSB_DJOB_NUMPROC'))

    # if unavailable, use a default value
    if (is.na(num_cores)) {
        num_cores <- 8
    }

    message(sprintf("Running computation in parallel on %d cores...", num_cores))
    
    # for UNIX:
    cl <- makeCluster(num_cores)
    
    clusterEvalQ(cl, library(caret))
    clusterExport(cl, "dependent_variable")
    clusterExport(cl, "output_dir_fs")
    clusterExport(cl, "inner_k")
    clusterExport(cl, "nvmax")
    clusterSetupRNGstream(cl, seed = rep(random_seed, 6))
    
    outer_fold_models <- clusterApply(cl, names(outer_folds), function(outer_fold_name) {
        message(paste("Outer fold:", outer_fold_name))
        
        outer_training_set <- regression_data[outer_folds[[outer_fold_name]], ]
        outer_test_set <- regression_data[ - outer_folds[[outer_fold_name]],]
        
        inner_folds <- createFolds(outer_training_set[, dependent_variable],
                                   k = inner_k, list = TRUE, returnTrain = TRUE)
        
        
        model_minimal <- lm(as.formula(paste(dependent_variable, "~ 1")), data = regression_data)
        model_step <- model_minimal
        current_formula <- formula(model_step)
        
        inner_fold_models <- list()
        previous_rmse <- 1000
        for (num_predictors in 1:nvmax) {
            predictor_scores <- as.data.frame(matrix(nrow = length(predictor_names), ncol = inner_k))
            predictor_scores <- cbind(predictor_names, predictor_scores)
            colnames(predictor_scores) <- c("name", names(inner_folds))

            predictor_scores <- lapply(predictor_names, function(predictor_name) {
                if (predictor_name %in% attr(model_step$terms, "term.labels")) {
                    return(NA)
                }
                candidate_formula <- modelr::add_predictors(current_formula,
                                                            as.formula(paste0("~", predictor_name)))
                predictor_scores <- rep(0, length(inner_folds))
                for (inner_fold_id in seq_along(inner_folds)) {
                    
                    inner_fold_name <- names(inner_folds)[inner_fold_id]
                    # message(paste("Fold:", fold_name))
                    inner_fold_training <- outer_training_set[inner_folds[[inner_fold_id]],]
                    inner_fold_test <- outer_training_set[- inner_folds[[inner_fold_id]], ]
                    
                    candidate_model <- lm(candidate_formula, data = inner_fold_training)
                    predictor_scores[inner_fold_id] <- modelr::rmse(candidate_model, inner_fold_test)
                }
                predictor_scores
            })
            predictor_scores_avg <- unlist(lapply(predictor_scores, mean))
            message(paste(predictor_scores_avg, collapse = ", "))
            selected_predictor <- which.min(predictor_scores_avg)
            message(paste("Selected predictor:", predictor_names[selected_predictor], 
                          " - avg RMSE = ", predictor_scores_avg[selected_predictor]))
            current_formula <- modelr::add_predictors(current_formula,
                                                      as.formula(paste0("~", predictor_names[selected_predictor])))
            model_step <- lm(current_formula, data = outer_training_set)
            model_step_rmse <- predictor_scores_avg[selected_predictor]
            inner_fold_models[[num_predictors]] <- list(
                rmse = model_step_rmse,
                predictors = attr(model_step$terms, "term.labels"),
                formula = current_formula,
                model = model_step
            )
            
            # if the test error didn't decrease, stop adding predictors
            if (model_step_rmse < previous_rmse) {
                previous_rmse <- model_step_rmse
            } else {
                message("RMSE did not decrease. Stop adding predictors.")
                message(paste(model_step_rmse, previous_rmse))
                break()
            }
            
        }
        
        min_validation_error_index <- which.min(lapply(inner_fold_models, function(x) x$rmse))
        outer_fold_model <- inner_fold_models[[min_validation_error_index]]$model
        outer_fold_formula <- inner_fold_models[[min_validation_error_index]]$formula
        outer_fold_model <- lm(outer_fold_formula, data = outer_training_set)
        outer_fold_predictors <- inner_fold_models[[min_validation_error_index]]$predictors
        message(paste("Outer fold predictors:", paste0(outer_fold_predictors, collapse = ",")))
        message(sprintf("RMSE = %.3f", inner_fold_models[[min_validation_error_index]]$rmse))
        res <- list(
            predictors = predictor_names %in% outer_fold_predictors,
            model = outer_fold_model
        )
    })
    stopCluster(cl)
    
    outer_fold_models_predictors <- lapply(outer_fold_models, function(x) x[[1]])
    outer_fold_models_models <- lapply(outer_fold_models, function(x) x[[2]])
    
    predictors_num_times_selected <- 
        colSums(matrix(unlist(outer_fold_models_predictors), ncol = length(predictor_names), byrow = TRUE))
    names(predictors_num_times_selected) <- predictor_names
    
    rmse_values <- 
        as.data.frame(matrix(nrow = max(predictors_num_times_selected), ncol = length(outer_folds)))
    colnames(rmse_values) <- names(outer_folds)
    
    num_predictors <- rep(0, max(predictors_num_times_selected))
    
    for (n in 1:max(predictors_num_times_selected)) {
        message(paste("Best model with variables chosen in at least", n, "folds."))
        predictors <- predictor_names[predictors_num_times_selected >= n]
        message(sprintf("%d predictors: %s", length(predictors), paste0(predictors, collapse = ", ")))
        num_predictors[n] <- length(predictors)
        fold_rmse_values <- c()

        for (outer_fold_id in seq_along(outer_folds)) {
            outer_fold_name <- names(outer_folds)[outer_fold_id]

            outer_training_set <- regression_data[outer_folds[[outer_fold_id]], ]
            outer_test_set <- regression_data[ - outer_folds[[outer_fold_id]],]
            
            fit <- lm(as.formula(paste(dependent_variable, "~ ", paste0(predictors, collapse = " + "))),
                      data = outer_training_set)
            rmse_value <- modelr::rmse(fit, outer_test_set)
            rmse_values[n, outer_fold_name] <- rmse_value
            fold_rmse_values <- c(fold_rmse_values, rmse_value)
        }
        message(sprintf("Avg. RMSE = %.3f", mean(fold_rmse_values)))
    }
    rmse_values$avg <- rowMeans(rmse_values)
    
    png(file.path(output_dir_fs, "num_predictors_rmse.png"), height = 800)
    par(mfrow=c(2,1))
    plot(rmse_values$avg, type = "l", main = "Test error", xlab = "# times predictors were selected", ylab = "RMSE")
    plot(num_predictors, type = "p", main = "Num. of predictors", xlab = "# times predictors were selected", ylab = "# predictors")
    dev.off()
    chosen_num_predictors_threshold <- which.min(rmse_values$avg)
    
    best_predictors <- predictor_names[predictors_num_times_selected >= chosen_num_predictors_threshold]
    message(paste("Best predictors:", paste0(best_predictors, collapse = ", ")))
    
    fit <- lm(as.formula(paste(dependent_variable, "~ ", paste0(best_predictors, collapse = " + "))),
              data = regression_data)
    
    list(
        outer_fold_models_predictors = outer_fold_models_predictors,
        outer_fold_models = outer_fold_models_models,
        rmse_values = rmse_values,
        final_fit = fit, 
        predictors_num_times_selected = predictors_num_times_selected
    )
}

for (dependent_variable in dependent_variables) {
    output_dir_fs <- file.path(output_dir, sprintf("fs_output_%s_%s", dependent_variable, format(Sys.time(), "%Y%m%d-%H%M%S")))
    dir.create(output_dir_fs, showWarnings = FALSE)
    
    regularized_fit <- perform_feature_selection(regression_data, dependent_variable, output_dir_fs = output_dir_fs)
    
    save(regularized_fit, file = file.path(output_dir_fs, "regularized_model.RData"))
}



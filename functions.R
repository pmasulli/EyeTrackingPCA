customIterate <- function(l, fun) {
    len <- if (is.data.frame(l)) {
        nrow(l)
    } else {
        length(l)
    }
    mycounter <- 1
    do.call(rbind, by(l, 1:len, function(x) {
        message(paste0("[", mycounter, "/", len, "] ", x[1]))
        mycounter <<- mycounter + 1
        fun(x)
    }))
}


vector_to_matrix <- function(input_vector, ncol, ...) {
    matrix(input_vector, ncol = ncol, byrow = F, ...)
}

edit_matrix_border <- function(component_matrix, toprows, bottomrows, leftcols, rightcols, replacement_value = 0) {
    output_matrix <- component_matrix
    output_matrix[1:toprows, ] <- replacement_value
    output_matrix[(nrow(component_matrix) - bottomrows + 1):nrow(component_matrix), ] <- replacement_value
    output_matrix[, 1:leftcols] <- replacement_value
    output_matrix[, (ncol(component_matrix) - rightcols + 1):ncol(component_matrix)] <- replacement_value
    output_matrix
}

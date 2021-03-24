# area dimensions at full size
area_cols_full <- colend - colstart + 1
area_rows_full <- rowend - rowstart + 1

# padding rows and columns at full size
padding_left_full <- colstart - 1
padding_right_full <- screenSize[1] - colend
padding_top_full <- rowstart - 1
padding_bottom_full <- screenSize[2] - rowend


dim(participant_matrices)

participant_matrices_matrix <- participant_matrices[, 2:ncol(participant_matrices)]

zero_columns <- apply(participant_matrices_matrix, 2, function(x) all(x == 0))

message(paste("There are", sum(zero_columns), "zero columns."))

mydata <- participant_matrices_matrix[, !zero_columns]


mydata.pca <- prcomp(mydata, center = TRUE, scale. = FALSE)

components <- mydata.pca$rotation
dim(components)

std_dev <- mydata.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

pca_numfeatures <- list()
for (threshold in c(.90, .95, .99)) {
    nf <- min(which(cumsum(prop_varex) >= threshold))
    message(sprintf("It takes %d components to express %.2f of the variance.",
                    nf, threshold))
    pca_numfeatures[[100 * threshold]] <- nf
}

d_center <- colMeans(mydata)
d_scale <- rep(1, length(d_center))
d_scaled <- mydata - matrix(d_center, nrow = nrow(mydata), ncol = ncol(mydata), byrow = T)

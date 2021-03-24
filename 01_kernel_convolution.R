participants <- gsub(".csv", "", dir(data_dir, pattern = "^[0123456789].*csv"), fixed = T)

gaussian_kernel <- spatialEco::gaussian.kernel(10, 70)

cl <- makeCluster(8)

clusterExport(cl, c("gaussian_kernel", "data_dir", "screenSize", "rowstart", "rowend", "colstart", "colend", "edit_matrix_border"))

participant_matrices <- clusterApply(cl, participants, function(participant) {
  
  message(participant)
  
  participant_data <- read.csv(file.path(data_dir, sprintf("%s.csv", participant)))
  participant_data$count <- 1
  
  
  # Create the gaze point matrix
  # Same size as the screen. Each entry is the count of gaze points at that location
  gaze_points_matrix <- matrix(0, ncol = screenSize[1], nrow = screenSize[2])
  for (n in 1:nrow(participant_data)) {
    gaze_points_matrix[participant_data[n, "GazeY"], participant_data[n, "GazeX"]] <-
      gaze_points_matrix[participant_data[n, "GazeY"], participant_data[n, "GazeX"]] + 1
  }
  
  # sanity check
  stopifnot(sum(gaze_points_matrix) == nrow(participant_data))
  
  # Convolve the kernel
  message("Computing convolution...")
  start.time <- Sys.time()
  convolved_matrix <- OpenImageR::convolution(gaze_points_matrix, gaussian_kernel)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message(paste("Convolution computed in", format(time.taken)))
  
  convolved_matrix <- edit_matrix_border(convolved_matrix[rowstart:rowend, colstart:colend], 6, 6, 15, 15)
  
  # convolved_matrix
  c(as.numeric(as.character(participant)), as.numeric(convolved_matrix))
})

stopCluster(cl)

participant_matrices <- do.call(rbind, participant_matrices)

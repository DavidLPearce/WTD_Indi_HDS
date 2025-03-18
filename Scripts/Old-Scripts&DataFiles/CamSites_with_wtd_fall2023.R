

# Subsetting the data to for just Mule Deer and White-tailed Deer
deer_data <- fall_cams[(which(fall_cams$common_name == "White-tailed Deer")),]

# Table to hold detections and group size
deer_table <- data.frame(matrix(NA, nrow = length(unique(deer_data$deployment_id)), ncol = 2))

deer_table[1:25,1] <- unique(deer_data[,2])

# Column names
colnames(deer_table) <- c("Site_ID", "Group_Size")

# Getting number of detections
deer_data$det<-ifelse (deer_data$common_name == "White-tailed Deer", 1,0)

# Iterate through each unique site
for (site in unique(deer_data$deployment_id)) {
  
  # Subset the data based on the current site
  subset <- deer_data[deer_data$deployment_id == site, ]
  
  # Find the row index in jav_table corresponding to the current site
  row_index <- which(deer_table$Site_ID == site)
  
  # Update jav_table with values from the subset
  deer_table[row_index, 1] <- subset[1, 2]    # Assuming site id is in the first row of the subset
  deer_table[row_index, 2] <- sum(subset[,27])  # detentions

  
  # You may want to add more columns here if needed
}

write.csv(deer_table, "LaCop_deer_Cam_Det.csv")

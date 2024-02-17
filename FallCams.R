
fall_cams <- read.csv("LaCopita_Cams_Fall2023.csv")


spp_sum <- function(data, name_col){
  
  # Check if the specified column exists in the dataframe
  if (!(name_col %in% names(data))) {
    stop("The specified column does not exist in the dataframe.")
  }
  
  # Count the number of observations for each unique name
  observation_counts <- table(data[[name_col]])
  
  # Convert the result to a data frame
  result <- data.frame(Name = names(observation_counts), Observations = as.numeric(observation_counts))
  
  return(result)
}


cam_summary <- spp_sum(fall_cams, name_col = "common_name")

unique(fall_cams$deployment_id)


#### Javelina ####

jav_cams <- fall_cams[(which(fall_cams$common_name == "Collared Peccary" )),]


# Table to hold detections and group size
jav_table <- data.frame(matrix(NA, nrow = length(unique(jav_cams$deployment_id)), ncol = 5))

jav_table[1:14,1] <- unique(jav_cams[,2])

# Column names
colnames(jav_table) <- c("Site_ID", "Lat", "Long", "Number_of_Det", "Max_Group_Size")

# Getting number of detections
jav_cams$jav_det<-ifelse (jav_cams$common_name == "Collared Peccary", 1,0)


# Iterate through each unique site
for (site in unique(jav_cams$deployment_id)) {
  
  # Subset the data based on the current site
  subset <- jav_cams[jav_cams$deployment_id == site, ]
  
  # Find the row index in jav_table corresponding to the current site
  row_index <- which(jav_table$Site_ID == site)
  
  # Update jav_table with values from the subset
  jav_table[row_index, 1] <- subset[1, 2]    # Assuming site id is in the first row of the subset
  jav_table[row_index, 4] <- sum(subset[,27])  # detentions
  jav_table[row_index, 5] <- max(subset[,16])  # group size
  
  # You may want to add more columns here if needed
}

write.csv(jav_table, "LaCop_Jav_Cam_Det.csv")

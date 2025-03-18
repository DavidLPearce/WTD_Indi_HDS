


Snapshot24 <- read.csv("./Data/Survey_Data/Camera_Data/2024_sequences.csv")



F24_cams <- Snapshot24 %>%
  filter(grepl("^TX_Grassland_Rural_LaCopita", deployment_id))

unique(F24_cams$deployment_id)

write.csv(F24_cams, "./Data/Survey_Data/Camera_Data/Raw_Data/LaCopita_Cams_Fall2024.csv")

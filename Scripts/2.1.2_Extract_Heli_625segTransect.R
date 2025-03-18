# ------------------------------------------------------------------------------
#
#                               Load Packages
#
# ------------------------------------------------------------------------------

# Install packages (if needed)
# install.packages("sf")
# install.packages("raster")
# install.packages("ggplot2")
# install.packages("viridis")
# install.packages("terra")
# install.packages(igraph)
# install.packages("lidR")
# install.packages("RCSF")
# install.packages("landscapemetrics")
# install.packages("progress")
# install.packages("psych")

# Load packages
library(sf)
library(sp)
library(raster)
library(ggplot2)
library(viridis)
library(terra)
library(igraph)
library(lidR)
library(RCSF)
library(landscapemetrics)
library(progress)
library(psych)

# Set seed, scientific notation, and workplace
set.seed(123)
options(scipen = 9999)
setwd(".")

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Read in raster data
lulc_rast <- stack("D:/LaCopita_GIS_Data/LaCopitaLULC/LaCopitaLULC_60cm_SVM_Buffered/LaCopitaLULC_60cm_SVM_Buffered.tif")
print(lulc_rast)

# Read in site locations
transects <- st_read("./Data/Spatial_Data/Helicopter_Transects/Helicopter_1250SegmentedTransects.shp")
print(transects)

# Directory for plots
output_dir <- "./Figures/LULC/625segTransect/"

# Extracting each class to a new raster
woody_class <- lulc_rast == 0   # Woody
herb_class <- lulc_rast == 1    # Herbaceous
brgnd_class <- lulc_rast == 2   # Bare Ground 
# Water = 3
# Developed = 4

# Convert to SpatRaster
lulc_rast <- rast(lulc_rast)
woody_class <- rast(woody_class) 
herb_class <- rast(herb_class)
brgnd_class <- rast(brgnd_class)

# Plots
# plot(lulc_rast, main = "La Copita LULC")
# plot(woody_class, main = "Woody")
# plot(herb_class, main = "Herbaceous")
# plot(baregnd_class, main = "Bare Ground")
# plot(water_class, main = "Water")
# plot(dev_class, main = "Developed")


# ------------------------------------------------------------------------------
#
#                                 Extracting
#
# ------------------------------------------------------------------------------

# Dataframe for site covariates
trans_site_covs <- as.data.frame(transects)


# Initialize the progress bar
pb <- progress_bar$new(
  format = "  Processing [:bar] :percent in :elapsed, remaining: :eta",
  total = NROW(transects),
  clear = FALSE,
  width = 100
)


# row = 1

# Loop
for (row in 1:NROW(transects)) {
  
  ## list_lsm() list of available metrics ##
  
  # Subset the site
  site_sub <- transects[row, ]
  
  # Getting siteID
  SiteID <- as.data.frame(site_sub[,4])
  SiteID <- SiteID[,-2] # remove geometry

  # Create a buffer around the site
  site_buffer <- st_buffer(site_sub[, "geometry"], dist = 100, endCapStyle="FLAT")
  site_buffer_terra <- vect(site_buffer) # SpatVector
  site_buffer_terra <- terra::project(site_buffer_terra, terra::crs(lulc_rast))# CRS of lulc
  
  # Extract and crop the raster for the buffer
  lulc_clip <- terra::crop(lulc_rast, site_buffer_terra)
  lulc_clip <- terra::mask(lulc_clip, site_buffer_terra)
  
  # Subset to class
  woody_clip <- lulc_clip == 0
  herb_clip <- lulc_clip == 1
  brgnd_clip <- lulc_clip == 2
  
  # Plot & Export
  lulc_Plotfile <- paste0(output_dir, SiteID, "_LULC.png")
  lulc_df <- as.data.frame(lulc_clip, xy = TRUE)
  lulc_plot <- ggplot(lulc_df) +
    geom_raster(aes(x = x, y = y, fill = factor(Classvalue))) +
    scale_fill_manual(name = "Land Cover Type",
                      values =  c("1" = "forestgreen",
                                  "2" = "lightgreen",
                                  "3" = "saddlebrown"),
                      labels = c("1" = "Woody",
                                 "2" = "Herbaceous",
                                 "3" = "Bareground")) +
    coord_fixed() +
    labs(title = paste("LULC Site", SiteID)) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  ggsave(filename = lulc_Plotfile, plot = lulc_plot, width = 8, height = 6, dpi = 300)
  
  
  # ------------------------------
  # Proportion of Class
  # ------------------------------
  # Extract raster cover type within buffer
  woody_prp <- terra::extract(woody_class, site_buffer_terra, fun = mean, na.rm = TRUE)
  herb_prp <- terra::extract(herb_class, site_buffer_terra, fun = mean, na.rm = TRUE)
  brgnd_prp <- terra::extract(brgnd_class, site_buffer_terra, fun = mean, na.rm = TRUE)
  trans_site_covs[row, "woody_prp"] <- woody_prp[1, 'layer']          
  trans_site_covs[row, "herb_prp"] <- herb_prp[1, 'layer']              
  trans_site_covs[row, "open_prp"] <- herb_prp[1, 'layer'] + brgnd_prp[1, 'layer'] 
  
  
  # ------------------------------
  # Mean Patch Area
  # ------------------------------
  # lsm_p_area: Calculates the area of each patch.(m^2)
  p_area <- landscapemetrics::calculate_lsm(lulc_clip, what = "lsm_p_area")
  woody_p_area <- p_area[which(p_area$class == 0),] # Woody
  woody_p_area_mean <- mean(woody_p_area$value, na.rm = TRUE)
  herb_p_area <- p_area[which(p_area$class == 1),] # Herbaceous
  herb_p_area_mean <- mean(herb_p_area$value, na.rm = TRUE) 
  trans_site_covs[row, "woody_mnParea"] <- woody_p_area_mean    
  trans_site_covs[row, "herb_mnParea"] <- herb_p_area_mean
  
  
  # ------------------------------
  # Clumpy Index
  # ------------------------------
  # lsm_c_clumpy: Quantifies the clumpiness (spatial aggregation)
  # Equals -1 for maximally disaggregated, 0 for randomly distributed and 1 for maximally aggregated classes.
  c_clumpy <- landscapemetrics::calculate_lsm(lulc_clip, what = "lsm_c_clumpy")
  woody_c_clumpy <- c_clumpy[which(c_clumpy$class == 0),] # Woody
  herb_c_clumpy <- c_clumpy[which(c_clumpy$class == 1),] # Herbaceous
  trans_site_covs[row, "woody_ClmIdx"] <- woody_c_clumpy[1, 'value'] 
  trans_site_covs[row, "herb_ClmIdx"] <- herb_c_clumpy[1, 'value']
  
  
  # ------------------------------
  # Mean Shape Index
  # ------------------------------
  # lsm_l_shape_mn: summarised as the mean of all patches in the landscape
  # Equals SHAPE_MN = 1 if all patches are squares. Increases, without limit, as the shapes of patches become more complex.
  woody_shape_mn <- landscapemetrics::calculate_lsm(woody_clip, what = "lsm_l_shape_mn") # Woody
  herb_shape_mn <- landscapemetrics::calculate_lsm(herb_clip, what = "lsm_l_shape_mn") # Herbaceous
  trans_site_covs[row, "woody_ShpInx"] <- woody_shape_mn[1, 'value']   
  trans_site_covs[row, "herb_ShpInx"] <- herb_shape_mn[1, 'value']  
  
  
  # ------------------------------
  # Largest Patch Index
  # ------------------------------
  # lsm_c_lpi: an 'Area and edge metric'. Percentage of landscape covered by the corresponding largest patch of each class i
  # Approaches LPI = 0 when the largest patch is becoming small and equals LPI = 100 when only one patch is present
  c_lpi <- landscapemetrics::calculate_lsm(lulc_clip, what = "lsm_c_lpi")
  woody_c_lpi <- c_lpi[which(c_lpi$class == 0),] # Woody
  herb_c_lpi <- c_lpi[which(c_lpi$class == 1),] # Herbaceous
  trans_site_covs[row, "woody_lrgPInx"] <- woody_c_lpi[1, 'value']   
  trans_site_covs[row, "herb_lrgPInx"] <- herb_c_lpi[1, 'value']  
  
  
  # ------------------------------
  # Aggregation Index
  # ------------------------------
  # lsm_c_ai: equals the number of like adjacencies divided by the theoretical maximum possible number of like adjacencies for that class
  # Equals 0 for maximally disaggregated and 100 for maximally aggregated classes.
  c_ai <- landscapemetrics::calculate_lsm(lulc_clip, what = "lsm_c_ai")
  woody_c_ai <- c_ai[which(c_ai$class == 0),] # Woody
  herb_c_ai <- c_ai[which(c_ai$class == 1),] # Herbaceous
  trans_site_covs[row, "woody_AggInx"] <- woody_c_ai[1, 'value']     
  trans_site_covs[row, "herb_AggInx"] <- herb_c_ai[1, 'value']  
  
  
  # ------------------------------
  # Edge Density 
  # ------------------------------
  # lsm_c_ed: equals the sum of all edges of class i in relation to the landscape area
  # = 0 if only one patch is present (and the landscape boundary is not included) and increases, without limit, as the landscapes becomes more patchy
  c_ed <- landscapemetrics::calculate_lsm(lulc_clip, what = "lsm_c_ed")
  woody_c_ed <- c_ed[which(c_ed$class == 0),] # Woody
  herb_c_ed <- c_ed[which(c_ed$class == 1),] # Herbaceous
  trans_site_covs[row, "woody_EdgDens"] <- woody_c_ed[1, 'value']   
  trans_site_covs[row, "herb_EdgDens"] <- herb_c_ed[1, 'value'] 
  
  
  # ------------------------------
  # Patch Density
  # ------------------------------
  # lsm_c_pd: Number per 100 hectares
  # Increases as the landscape gets more patchy. Reaches its maximum if every cell is a different patch.
  c_pd <- landscapemetrics::calculate_lsm(lulc_clip, what = "lsm_c_pd")
  woody_c_pd <- c_pd[which(c_pd$class == 0),] # Woody
  herb_c_pd <- c_pd[which(c_pd$class == 1),] # Herbaceous
  trans_site_covs[row, "woody_Pdens"] <- woody_c_pd[1, 'value']
  trans_site_covs[row, "herb_Pdens"] <- herb_c_pd[1, 'value']
  
  
  # ------------------------------
  # Clumpy Patches
  # ------------------------------
  
  # Classify patches
  woody_rast <- raster(woody_clip)
  herb_rast <- raster(herb_clip)
  woody_clumps <- clump(woody_rast, directions = 8) # Id clumps
  herb_clumps <- clump(herb_rast, directions = 8)  
  
  # Plot and Export Woody Patches
  woodyclumps_Plotfile <- paste0(output_dir, SiteID, "_WoodyClumps.png")
  png(woodyclumps_Plotfile, width = 8, height = 6, units = "in", res = 300)  # Set resolution to 300 DPI
  plot(woody_clumps, main = paste("Woody Clumps Site", SiteID ), col = turbo(cellStats(woody_clumps, stat = "max"), direction = 1))
  dev.off()
  
  # Number of patches
  trans_site_covs[row, "woody_Npatches"] <- cellStats(woody_clumps, stat = "max")
  trans_site_covs[row, "herb_Npatches"] <- cellStats(herb_clumps, stat = "max")
  

  # ------------------------------
  # Update the progress bar
  # ------------------------------
  pb$tick()
  
} # -------------------- End Extraction Loop -----------------------------


# Take a look
str(trans_site_covs)
print(trans_site_covs)

# Removing Rnded_Lgth, Nsegs, RowN, sgT_lgh_m, sgT_Lgh_km, geometry
site_covs <- trans_site_covs[,-c(1:3, 5:7)]
print(site_covs)
 
# Export data
write.csv(site_covs, "./Data/Survey_Data/Helicopter_Data/Heli_625segTransect_siteCovs.csv")



# ----------------------------- End of Script -----------------------------
sub_las
plot(sub_las, color = "Classification", bg = "white", size = 5)


chm <- rasterize_canopy(sub_las, 0.5, pitfree(subcircle = 0.2))
??rasterize_canopy
ttops <- locate_trees(sub_las, lmf(ws = 5))


plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)


f <- function(x) {x * 0.1 + 3}

ttops <- locate_trees(sub_las, lmf(f))

plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)


f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 5
  return(y)
}

ttops <- locate_trees(sub_las, lmf(f))

plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

# Point-to-raster 2 resolutions
chm_p2r_05 <- rasterize_canopy(sub_las, 0.5, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1 <- rasterize_canopy(sub_las, 1, p2r(subcircle = 0.2), pkg = "terra")

# Pitfree with and without subcircle tweak
chm_pitfree_05_1 <- rasterize_canopy(sub_las, 0.5, pitfree(), pkg = "terra")
chm_pitfree_05_2 <- rasterize_canopy(sub_las, 0.5, pitfree(subcircle = 0.2), pkg = "terra")

# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)


ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(5))
ttops_chm_p2r_1 <- locate_trees(chm_p2r_1, lmf(5))
ttops_chm_pitfree_05_1 <- locate_trees(chm_pitfree_05_1, lmf(5))
ttops_chm_pitfree_05_2 <- locate_trees(chm_pitfree_05_2, lmf(5))
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))
ttops_chm_p2r_1_smoothed <- locate_trees(chm_p2r_1_smoothed, lmf(5))

?segment_trees

asegment_treesalgo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
las <- segment_trees(sub_las, algo) # segment point cloud
plot(las, bg = "white", size = 4, color = "treeID") # visualize trees

?dalponte2016

tree110 <- filter_poi(las, treeID == 110)
plot(tree110, size = 8, bg = "white")


??crown_metrics

crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "convex")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")

num_Crowns <- NROW(crowns) 


sum_conhull_area <- mean(crowns$convhull_area)

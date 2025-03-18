## ------------------------------------------------------------------------------
##
##                               Loading R packages
##
## ------------------------------------------------------------------------------


library(dplyr)
library(tidyr)
library(lubridate)
library(unmarked)


## ------------------------------------------------------------------------------
##
##                                 Reading in data
##
## ------------------------------------------------------------------------------

fall23_cams <- read.csv("./Data/Survey_Data/Camera_Data/LaCopita_Cams_Fall2023.csv")

wtd_dat <- fall23_cams[which(fall23_cams$common_name == "White-tailed Deer"),]



# -------------------------------------------------------
#
#                   Data Wrangling
#
# -------------------------------------------------------


wtd_dat$date <- as.POSIXct(wtd_dat$start_time, format = "%m/%d/%Y %H:%M")
wtd_dat$date <- format(wtd_dat$date, "%Y-%m-%d")


wtd_dat5day <- wtd_dat[which(wtd_dat$date == "2023-09-08" | wtd_dat$date == "2023-09-09"|
                               wtd_dat$date == "2023-09-10" | wtd_dat$date == "2023-09-11" |
                               wtd_dat$date == "2023-09-12"),]


# Summarize data: count the number of detections by site and date
summary_data <- wtd_dat %>%
  group_by(deployment_id, date) %>%
  summarise(deer_count = n()) %>%
  ungroup()


# Pivot the data to create a matrix (wide format)
deer_matrix <- summary_data %>%
  pivot_wider(names_from = date, values_from = deer_count, values_fill = 0)

deer_matrix<-as.data.frame(deer_matrix[,-1])



# unmarked frame
umf <- unmarkedFramePCount(y = deer_matrix)


# Fit an N-mixture model (Poisson distributed abundance)
fm <- pcount(~ 1 ~ 1, 
             data = umf, K = 500)  # K is the upper bound for the abundance


# Extract site-level random effects (abundance estimates)
ranef_obj <- ranef(fm)
print(ranef_obj)  # Print to check the extracted random effects

# Calculate the expected abundance at each site
site_abundances <- bup(ranef_obj, stat = "mean")  # "bup" stands for Best Unbiased Predictor

# Sum the site-level abundances to get the total abundance for the ranch
total_abundance <- sum(site_abundances)

# Display the total abundance
print(total_abundance)


# Set up bootstrapping
set.seed(123)  # For reproducibility
n_bootstrap <- 1000  # Number of bootstrap samples

# Initialize a vector to store bootstrap total abundance values
bootstrap_totals <- numeric(n_bootstrap)

# Bootstrap sampling
for (i in 1:n_bootstrap) {
  bootstrap_abundances <- sample(ranef_obj$state, replace = TRUE)  # Sample from the estimates
  bootstrap_totals[i] <- sum(bootstrap_abundances)
}

# Calculate 95% confidence intervals (2.5% and 97.5% quantiles)
ci_lower <- quantile(bootstrap_totals, 0.025)
ci_upper <- quantile(bootstrap_totals, 0.975)

print(c(ci_lower, ci_upper))  # Print the confidence intervals


# Load ggplot2 library
library(ggplot2)

# Create a data frame for plotting
plot_data <- data.frame(
  estimate = total_abundance,
  ci_lower = ci_lower,
  ci_upper = ci_upper
)

# Create the plot
ggplot(plot_data, aes(x = 1, y = estimate)) +
  geom_point(size = 4) +  # Plot the point estimate for total abundance
  geom_errorbar(aes(ymin = 200, ymax = 360), width = 0.01) +  # Add confidence intervals
  labs(
    x = "N-mix",
    y = "Total Abundance",
    title = "Total Abundance of Deer with 95% Confidence Intervals"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


# Base R plot
# Set up the plot area
plot(plot_data$estimate, 
     ylim = c(0, 500),  # Set y-axis limits from 0 to 500
     xaxt = "n",        # Suppress x-axis
     pch = 16,          # Solid circle for points
     xlab = "N-mix", 
     ylab = "N", 
     main = "")

# Add custom x-axis labels
axis(1, at = 1:nrow(plot_data), labels = plot_data$site)

# Add error bars for confidence intervals
arrows(x0 = 1:nrow(plot_data), y0 = 200, 
       x1 = 1:nrow(plot_data), y1 = 360, 
       angle = 90, code = 3, length = 0.1, col = "blue")

# Install packages (if needed)
# remotes::install_github("annam21/spaceNtime")

# Load library
library(spaceNtime)

# -------------------------------------------------------
# example data
# ------------------------------------------------------- 

# The first, called df, is a data.frame or tibble that contains all your pictures from the study period. This must
# contain, at a minimum, these three columns (names and classes must match exactly)

df <- data.frame(
  cam = c(1,1,2,2,2),
  datetime = as.POSIXct(c("2016-01-02 12:00:00",
                          "2016-01-03 13:12:00",
                          "2016-01-02 12:00:00",
                          "2016-01-02 14:00:00",
                          "2016-01-03 16:53:42"),
                        tz = "GMT"),
  count = c(1, 0, 0, 1, 2)
)
print(df)
 

# database called deploy. This is a dataframe or tibble that contains information
# about the active periods for each active camera and their areas. Please note, to get accurate estimates, this
# should include all cameras that were functioning, not just the ones that got pictures of your target species

deploy <- data.frame(
  cam = c(1, 2, 2, 2),
  start = as.POSIXct(c("2015-12-01 15:00:00",
                       "2015-12-08 00:00:00",
                       "2016-01-01 00:00:00",
                       "2016-01-02 00:00:00"),
                     tz = "GMT"),
  end = as.POSIXct(c("2016-01-05 00:00:00",
                     "2015-12-19 03:30:00",
                     "2016-01-01 05:00:00",
                     "2016-01-05 00:00:00"),
                   tz = "GMT"),
  area = c(300, 200, 200, 450)
)
print(deploy)

# First, you will need to specify the length of your sampling period. This is equal to the mean amount of time (in
# seconds) that it takes for an animal to cross the average viewshed of a camera. This can be calculated in
# different ways. A crude method for estimating sampling period exists with the function tte_samp_per(). An
# example, based on an animal speed of 30m/hr is:

per <- tte_samp_per(deploy, lps = 30/3600)


# Once you have defined the length of your sampling period, you can build your sampling occasions. This can
# be done manually or with the function tte_build_occ(). The sampling occasions will be in a data.frame or tibble
# with the following structure:

study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")


occ <- tte_build_occ(
  per_length = per,
  nper = 24,
  time_btw = 2 * 3600,
  study_start = study_dates[1],
  study_end = study_dates[2]
)



# Build encounter history
tte_eh <- tte_build_eh(df, deploy, occ, per)
head(tte_eh)

# Estimate abundance
tte_estN_fn(tte_eh, study_area = 1e6)

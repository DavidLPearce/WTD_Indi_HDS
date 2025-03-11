# Author: David L. Pearce
# Description:
#             This script extends the script and vignette written by Jeff Doser
#             and Marc Kéry for hierarcical distance sampling.  Which can be 
#             found here: 
#             https://doserlab.com/files/spabundance-web/articles/distancesampling
#             This script analyses 4 helicopter surveys with two replicates per
#             survey to estimate white-tailed deer abundance.             

# Citation: 
#           TBD



# ------------------------------------------------------------------------------
#
#                               Load Packages
#
# ------------------------------------------------------------------------------

# Install packages (if needed)
# install.packages("tidyverse")
# install.packages("spAbundance")

# Load library
library(tidyverse)
library(spAbundance)

# Set seed, scientific notation options, and working directory
set.seed(123)
options(scipen = 9999)
setwd(".")

# -------------------------------------------------------
#
#                    Load Data
#
# -------------------------------------------------------
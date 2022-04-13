# Install library===============================================================
library(dplyr)

# Set seed for reproducability
set.seed(234)
#===============================================================================
# Utilise various machine learning methods, both supervised and unsuperivsed
# to predict onces change of having metabolic syndrom; OR if metabolic syndrom
# clusters together.
#
#
# STEPS PROPOSED
# STEP 1:
# Data prep
# Check for nulls & decide on imputation
# Create new data frames, one for supervised and one for unsupervised
#


# DATA PREP=====================================================================

# Load in data==================================================================
metabolic_df <- read.csv("metabolic.csv")
str(metabolic_df)

# Ensure any characters are converted to NA
metabolic_df <- metabolic_df %>% 
  mutate_all(na_if, "")

sum(is.na(metabolic_df)) # We have 436 varibles missing
sapply(metabolic_df, function(x) sum(is.na(x))) # Column wise summary of null values

# Data Imputation ==============================================================
# Based on above we have the following varibles containing NULL values
# Maritial: If Null then we will replace with unkown
# Income
# Waist Circumfrance

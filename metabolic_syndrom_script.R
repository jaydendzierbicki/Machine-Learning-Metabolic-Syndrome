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
# Ensure each varible is converted to apporiate data type
# Create new data frames, one for supervised and one for unsupervised
# Implement first model - naive, LDA, QDA
# Implement second model - PCA
# Implement third model - Kmeans clustering


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
# https://towardsdatascience.com/7-ways-to-handle-missing-values-in-machine-learning-1a6326adf79e 
# There are serveral methods we can employ to deal with various missing data 
# Martial: We will imputate maritial if NULL -> unkown
# Income: 
# Waist Circumstance

metabolic_df$Marital[is.na(metabolic_df$Marital)] <- "Unkown"
  

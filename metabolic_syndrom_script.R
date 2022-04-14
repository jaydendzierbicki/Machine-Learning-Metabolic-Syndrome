# Install library===============================================================
library(dplyr)
library(VIM) # Used for imputation

# Set seed for reproducability - arbitrarily selected
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


# Ensure any characters are converted to NA
metabolic_df <- metabolic_df %>% 
  mutate_all(na_if, "")

sum(is.na(metabolic_df)) # We have 436 varibles missing


sapply(metabolic_df, function(x) sum(is.na(x))) # Column wise summary of null values

# Data Imputation ==============================================================
# https://towardsdatascience.com/7-ways-to-handle-missing-values-in-machine-learning-1a6326adf79e 
# There are serveral methods we can employ to deal with various missing data 
# Martial: We will imputate maritial if NULL -> unkown
# Income: Utilise VIM package kNN()
# Waist Circumstance: Utilise VIM package kNN()


metabolic_df$Marital[is.na(metabolic_df$Marital)] <- "Unkown"

# There are some issues with imputation and machine learning, as such we
# will make two df, one removing the remidning NA values and one containg
# imuptated values 

metabolic_df_ni <- metabolic_df # Retain DF without k-means
metabolic_df <- kNN(metabolic_df, imp_var = T) # Default k = 5, retain imputation flag

# Overall data coercion =========================================================
# Ensure data is of correct type
str(metabolic_df) # We observe many factor varibles are of type character 


# For our unsupervised learning method we will only select continuous varibles, at this
# stage we will retain seqn and metabolic syndrom status
metabolic_df_unsupervised <- metabolic_df %>% 
  select(seqn,
         MetabolicSyndrome,
         Age,
         Income,
         WaistCirc,
         BMI,
         UrAlbCr,
         UricAcid,
         BloodGlucose,
         HDL,
         Triglycerides)

# Since we are dealing with units in different measurements we will apply
# a scaling algorithm prior to completing PCA analysis 

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

# !!!
# There are some issues with imputation and machine learning, as such we
# will need to include this in disscusion section of paper
# !!!

metabolic_df_ni <- metabolic_df # Retain DF without k-means
metabolic_df <- kNN(metabolic_df, imp_var = F) # Default k = 5, retain imputation flag

# Overall data coercion =========================================================
# Ensure data is of correct type
str(metabolic_df) # We observe many factor varibles are of type character 

metabolic_df <- metabolic_df %>% 
  mutate(Sex = as.factor(Sex),
         Marital = as.factor(Marital),
         Race = as.factor(Race),
         MetabolicSyndrome = as.factor(MetabolicSyndrome))
str(metabolic_df) # All varibles are now printing correctly

# Supervised data prep =========================================================
metabolic_df_supervised <- metabolic_df

# Unsupervised data prep =======================================================
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








# SUPERVISED LEARNING ==========================================================




# Assumption testing ===========================================================
# Will look at the following and pick the one that has best assumptions
# Naive Bayes
# LDA
# QDA
# Logistic
# K-means
# Logit

library(caret) # used to split data

# NAIVE BAYES ==================================================================
metabolic_df_supervised_nb <- metabolic_df_supervised %>% 
  select(-seqn)
split_nb <- createDataPartition(metabolic_df_supervised_nb$MetabolicSyndrome, p = 0.8, list = F)
train_nb <- metabolic_df_supervised_nb[split, ]
test_nb <- metabolic_df_supervised_nb[-split, ]
c(nrow(train_nb), nrow(test_nb)) # 1922 TRAIN, 479 TEST

# Check balance of data
prop.table(table(train_nb$MetabolicSyndrome)) # Data is somewhat imbalanced, 35% NMS, 65% MBS

# ASSUMPTION TESTING - Independence NB
library(psych)
pairs.panels(metabolic_df_supervised_nb)

# !!! We observe some varibles are correlated to one another - violation !!!

# DATA DISTRUBTUION - Gaussian Distribution 
par(mfrow=c(3,2))
hist(metabolic_df$Age) # Not normal
hist(metabolic_df$Income) # Not normal
hist(metabolic_df$Triglycerides) # Skewed left
hist(metabolic_df$BloodGlucose) # Not normal

# !!! We observe does not follow Gaussian Distrubtion, set kernel  = T

# Implement NAIVE BAYES
library(naivebayes)
nb_g <- naive_bayes(MetabolicSyndrome ~ ., data = train_nb, usekernel = F)
get_cond_dist(nb_g) # Observe distrubtion 
nb_gf <- naive_bayes(MetabolicSyndrome ~ ., data = train_nb, usekernel = T)
get_cond_dist(nb_gf) # Observe distrubtion used for non paramtertic version 
plot(nb_g, which = "BloodGlucose") # Test discriminatory power

# Produce confusion matrix for NAIVE BAYES
confusionMatrix(predict(nb_g), train_nb$MetabolicSyndrome)
confusionMatrix(predict(nb_gf), train_nb$MetabolicSyndrome)
# Based on test data we observe that the non-parametric model is slightly better

pred_g <- predict(nb_g, newdata = test_nb)
confusionMatrix(pred_g, test_nb$MetabolicSyndrome)

pred_gf <- predict(nb_gf, newdata = test_nb)
confusionMatrix(pred_gf, test_nb$MetabolicSyndrome)

# We observe that using kernal density plots in place of Gaussian improves our model
# this was expected based on the assumption testing

# NAIVE BAYES AURO
library(pROC)
roc_gaussian <- roc(as.numeric(test_nb$MetabolicSyndrome), as.numeric(pred_g)) # AUC 0.7264
roc_kernal <- roc(as.numeric(test_nb$MetabolicSyndrome), as.numeric(pred_gf)) # AUC 0.7526

################################################################################
# NAIVE BAYES DISC
# We observe that our assumptions have been violated, such as conditional independence 
# amongst predictor variables, though Naive bayes can handle this generally 
# We observe that our data is not Gausian Distrubtion - as such we can apply
# a non-paramatric feture in the naivebayes() function - this resulted in a 
# improved model, both in accuracy and AUC
################################################################################

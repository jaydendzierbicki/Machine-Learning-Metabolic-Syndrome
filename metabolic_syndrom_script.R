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








# UNSUPERVISED =================================================================
# Will explore the following topics
# 1 - PCA unsupervised & the benefits
# 2 - K-means clustering






# PCA UNSUPERVISED =============================================================
# 
# Will utilise PCA to reduce dimensionality of our data and apply it to our
# clustering technique
################################################################################

# Remove seqn and MetabolicSyndrom
metabolic_df_unsupervised_pca <- metabolic_df_unsupervised %>% 
  select(-seqn, -MetabolicSyndrome)

# Scale as units are diff measurements/values
metabolic_df_unsupervised_pca <- as.data.frame(scale(metabolic_df_unsupervised_pca))
#metabolic_df_unsupervised_pca <- as.data.frame((metabolic_df_unsupervised_pca))



summary(metabolic_df_unsupervised_pca) # Confrim scaled
diagnosis <- as.numeric(metabolic_df_unsupervised$MetabolicSyndrome == "MetSyn") # MetSyn = 1, NO METSYN = 0

# Commence PCA and obtain some info
pca <- prcomp(metabolic_df_unsupervised_pca, scale = F) # Already scaled
summary(pca) # Summary PC values
PVE <- round((pca$sdev^2)/sum(pca$sdev^2), 2)

# Scree Plot
par(mfrow = c(1,2))
plot(PVE, xlab = "Principal Component", ylab = "Prop of variance Explained", type = "b", ylim = c(0,1))
# Shows prop of variance explained cumualaivte of each PCA as we go through list
plot(cumsum(PVE), xlab = "Principal component", ylab = "cummulative prop of variance Explained", type = "b")

# Elbow method select PC values for model
library(factoextra)
fviz_eig(pca) # Select first 2 PC values for our model

# Show visually our model - we can see some speration between our model
library(pca3d)
pca2d(pca, group = diagnosis)

pca_3d_graph <- pca3d(pca, group=diagnosis, show.ellipses=TRUE,
                      ellipse.ci=0.95, show.plane=FALSE, legend = "topleft")



# What varibles contribute most to our PCA model?
loading_pc1 <- abs(pca$rotation[,1])
loading_pc2 <- abs(pca$rotation[,2])
print(head(sort(loading_pc1, decreasing = T)))
print(head(sort(loading_pc2, decreasing = T)))

################################################################################
# What the loading tells us is the most impact a var has on the PC
# PCA1: WaistCirc > BMI > BMI > UricAcid > Trig > BloodGlucose
# PCA2: Age > UrAlCr > Blood Glucose > HDL > BMI > Waist Circ
################################################################################


# K-means clustering ===========================================================
metabolic_df_unsupervised_clustering <- pca$x[,1:2] # Select first 2 PC
metabolic_df_unsupervised_clustering <- as.data.frame(cbind(metabolic_df_unsupervised_clustering, diagnosis)) # MetSyn = 1, NO METSYN = 0
k2 <- kmeans(metabolic_df_unsupervised_clustering[1:2], centers = 2, nstart = 25)

metabolic_df_unsupervised_clustering$cluster <- factor(k2$cluster)

# Produce table
table <- metabolic_df_unsupervised_clustering %>% 
  mutate(diagnosis = case_when(diagnosis == 1 ~ "MetSyn",
                               diagnosis == 0 ~ "No MetSyn"))

cont_table_kmeans <- table(table$diagnosis, table$cluster)
(accuracy_kmeans <- sum(diag(cont_table_kmeans)/sum(cont_table_kmeans)))

################################################################################
# Obtained accuracy of 77% via unsupervised k-means clustering with PCA
# This is on par with supervised method
################################################################################

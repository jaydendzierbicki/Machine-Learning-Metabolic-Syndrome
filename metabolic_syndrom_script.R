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



# https://www.frontiersin.org/articles/10.3389/fmed.2021.626580/full
# Similair studies have applied LDA/NB & Logstic regression to similair studies
# Based on other studies some important variables required to predict Mets are:
# WC, BMI, Obesity, DBp, SBp, Creatinine, Sex, UA, T_Billirbimum, Albumin, Escore
# CAPScore, TG, rGT, ALKp, GPT, GOT, HbA1C, GlucoseC, AFP, BUN, Age, TSH, HDL, 
# MDRD, Cholesterol and LDL

# The above could suggest that categorical varibles such as income, maritial statues
# do not play a risk

# https://www.cdc.gov/pcd/issues/2017/16_0287.htm#:~:text=When%20stratified%20by%20race%2Fethnicity,%E2%80%932012%20(Figure%202).
# The above study found that metabolic syndrom has increased amongts non-hispanic white women
# non-hispanic black women and people of low socioeconomic status

# The above suggests that we could also probably use Race and Income in our model(s)
# if assumptions allowed them to be included.

#===============================================================================



#===============================================================================
# DATA PREP - overal
#
#===============================================================================

metabolic_df <- read.csv("metabolic.csv")


# Ensure any characters are converted to NA
metabolic_df <- metabolic_df %>% 
  mutate_all(na_if, "")

sum(is.na(metabolic_df)) # We have 436 varibles missing


sapply(metabolic_df, function(x) sum(is.na(x))) # Column wise summary of null values

#===============================================================================
# IMPUTATION
# https://towardsdatascience.com/7-ways-to-handle-missing-values-in-machine-learning-1a6326adf79e 
# There are serveral methods we can employ to deal with various missing data 
# Martial: We will imputate maritial if NULL -> unkown
# Income: Utilise VIM package kNN()
# Waist Circumstance: Utilise VIM package kNN()
#===============================================================================

metabolic_df$Marital[is.na(metabolic_df$Marital)] <- "Unkown"

# !!!
# There are some issues with imputation and machine learning, as such we
# will need to include this in disscusion section of paper
# !!!

metabolic_df_ni <- metabolic_df # Retain DF without k-means

#===============================================================================
# IMPUTATION
# Income / Waist Circumfrance / BMI
#===============================================================================

library(caret) # For imputation
imput_df <- metabolic_df %>% 
  select(-seqn, -MetabolicSyndrome)

dummy_vars <- dummyVars(~ ., data = imput_df)
train_dummy <- predict(dummy_vars, imput_df)

pre_process <- preProcess(train_dummy, method = "bagImpute")
imputed_data <- as.data.frame(predict(pre_process, train_dummy))


metabolic_df$Income <- imputed_data$Income
metabolic_df$WaistCirc <- imputed_data$WaistCirc
metabolic_df$BMI <- imputed_data$BMI
sapply(metabolic_df, function(x) sum(is.na(x))) # Confrim imputation worked

#===============================================================================
# DATA TRANSFOMRATION
# Ensure data is of correct type
#===============================================================================

str(metabolic_df) # We observe many factor varibles are of type character 

metabolic_df <- metabolic_df %>% 
  mutate(Sex = as.factor(Sex),
         Marital = as.factor(Marital),
         Race = as.factor(Race),
         MetabolicSyndrome = as.factor(MetabolicSyndrome),
         Income = (as.integer(Income)),
         WaistCirc = as.integer(WaistCirc),
         BMI = as.integer(BMI),
         UrAlbCr = as.integer(UrAlbCr),
         UricAcid = as.integer(UricAcid))
str(metabolic_df) # All varibles are now printing correctly


#===============================================================================
# DATA EXPLORATION
# Box plots
#===============================================================================
library(ggplot2)
# Look for outliers

(age_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=Age)) +
  geom_boxplot() ) # Different between groups

(income_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=Income)) +
    geom_boxplot() ) # Different between groups

(waist_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=WaistCirc)) +
    geom_boxplot() ) # Different between groups

(bmi_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=BMI)) +
    geom_boxplot() ) # Different between gorups

(urAlbCr_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=UrAlbCr)) +
    geom_boxplot() ) # Unable to gain any insight from this varible - transformation?

(uric_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=UricAcid)) +
    geom_boxplot() ) # Almost identical distribution - DROP - refer to literature, we do see some outliers on higher end of MS & longer whisker on bottom end of no metsynd

(blood_glucos_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=BloodGlucose)) +
    geom_boxplot() ) # Lot of outliers

(hdl_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=HDL)) +
    geom_boxplot() ) # Higher in those without metabolic syndrome (good cholesterol )

(tri_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=Triglycerides)) +
    geom_boxplot() ) # Higher in those without metabolic syndrome 


# Based on the boxplot the following transformation was conducted]
metabolic_df <- metabolic_df %>% 
  mutate(UrAlbCr = log(as.integer(UrAlbCr)))
(uric_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=UricAcid)) +
    geom_boxplot() ) # Can now see almost identical distrubtion


barplot(prop.table(table(metabolic_df$Race)))
barplot((table(metabolic_df$Race)))
# We observe that a large portion of our data is: White, Black & Asian
# We are unsure the ethenicity of other, and since accounts for <10% of data
# we elected to remove it

barplot(prop.table(table(metabolic_df$Sex))) # Almost identical 50/50 gender split
barplot((table(metabolic_df$Sex)))

barplot(prop.table(table(metabolic_df$Marital))) # Most maried/single
barplot((table(metabolic_df$Marital)))
# Previous studies have shown that relationship status can influence health status
# https://pubmed.ncbi.nlm.nih.gov/29976034/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6166117/#:~:text=%5B6%5D%20have%20reported%20that%20women,physical%20and%20psychological%20support%2C%20especially

barplot(prop.table(table(metabolic_df$Albuminuria))) # Unsure what 0,1 & 2 refer to, might have to drop this variable if no data
barplot((table(metabolic_df$Albuminuria)))           # dict can be found - limitation of study and source data.
                                                     # Though, most obseervations are classed as 0 and could result in noise in our model


barplot(prop.table(table(metabolic_df$MetabolicSyndrome)))# Unbalanced data
barplot((table(metabolic_df$MetabolicSyndrome))) 
#===============================================================================
# REMOVE/CREATE VARS
# Since we do not know what other stands for we will remove it, also accounts
# for less then 10% of all observations
#===============================================================================


n_row_initial <- nrow(metabolic_df)

metabolic_df <- metabolic_df %>% 
  filter(Race != "Other") %>% 
  filter(Marital !=  "Unkown") %>% 
  select(-Albuminuria) %>% 
  unique() # Remove any duplicated rows - none detected anyhow

n_row_filter <- nrow(metabolic_df)
total_removed = n_row_initial - n_row_filter # 61 observations lost



#===============================================================================
# DATA EXPLOR SUMMARY
# Based on exploration of data we observe most varibles have a clear difference
# between those with MS and those without. Some identified issues:
# Remove: Race (Other)
# Remove Column: Unsure what Albuinuria refers to - possible limitation - will review litrature to decide
# Transfomration: log UrAlbCr
# Unbalanced data: Metabolic syndrom unbalance - approx 800 MetSyn and approx 1540 No MetSyn
#===============================================================================

#===============================================================================
# Supervised data prep 
#===============================================================================
metabolic_df_supervised <- metabolic_df

#===============================================================================
# Unsupervised data prep
# For our unsupervised learning method we will only select continuous varibles, at this
# stage we will retain seqn and metabolic syndrom status
#===============================================================================

metabolic_df_unsupervised <- metabolic_df %>% 
  select(#seqn,
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





#===============================================================================
#
# SUPERVISED LEARNING 
#
#===============================================================================

#===============================================================================
# Assumption testing 
# Will look at the following and pick the one that has best assumptions
# Naive Bayes
# LDA
# QDA
# Logistic
# K-means
# Logit
#===============================================================================

library(caret) # used to split data

#===============================================================================
# NAIVE BAYES 
#===============================================================================

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
par(mfrow=c(2,2))
hist(metabolic_df$Age) # Not normal
hist(metabolic_df$Income) # Not normal
hist(metabolic_df$Triglycerides) # Skewed left
hist(metabolic_df$BloodGlucose) # Not normal
par(mfrow=c(1,1)) # Reset

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
roc_kernal <- roc(as.numeric(test_nb$MetabolicSyndrome), as.numeric(pred_gf)) # AUC 0.7526 (0.8336)

#===============================================================================
# NAIVE BAYES DISC
# We observe that our assumptions have been violated, such as conditional independence 
# amongst predictor variables, though Naive bayes can handle this generally 
# We observe that our data is not Gausian Distrubtion - as such we can apply
# a non-paramatric feture in the naivebayes() function - this resulted in a 
# improved model, both in accuracy and AUC
#===============================================================================

#===============================================================================
# Cross Validation Naive Bayes
#===============================================================================



#===============================================================================
# KNN Supervised learning 
#===============================================================================

metabolic_df_knn <- metabolic_df_supervised %>% 
  select(-seqn, - MetabolicSyndrome)

diagnosis <- metabolic_df_supervised$MetabolicSyndrome

dummy_vars_knn <- dummyVars(~ ., data = metabolic_df_knn)
train_dummy_knn <- predict(dummy_vars_knn, metabolic_df_knn)
train_dummy_knn <- as.data.frame(train_dummy_knn)
train_dummy_knn <- cbind(train_dummy_knn, diagnosis)


train_dummy_knn$diagnosis <- ifelse(train_dummy_knn$diagnosis == "No MetSyn", 0 , 1)


metabolic_df_knn_norm <- preProcess(train_dummy_knn[,1:24], method = c("scale", "center")) # Scale all vars except predictor
metabolic_df_knn_ST <- predict(metabolic_df_knn_norm, train_dummy_knn[,1:24])
summary(metabolic_df_knn_ST)

# Train our model
knn_label <- train_dummy_knn$diagnosis
split <- createDataPartition(knn_label, p=0.8, list = F)
knn_train <- train_dummy_knn[split,]
knn_test <- train_dummy_knn[-split,]
knn_label <- knn_train$diagnosis

library(class)
knn3_class <- knn(knn_train, knn_test, knn_label, k=4)
accuracy <- function(predictions, ground_truth){
  mean(predictions == ground_truth)
}
accuracy(knn3_class, train_dummy_knn$diagnosis)



#===============================================================================
# UNSUPERVISED 
# Will explore the following topics
# 1 - PCA unsupervised & the benefits
# 2 - K-means clustering
#===============================================================================





#===============================================================================
# PCA UNSUPERVISED 
# 
# Will utilise PCA to reduce dimensionality of our data and apply it to our
# clustering technique
#===============================================================================

# Remove seqn and MetabolicSyndrom
metabolic_df_unsupervised_pca <- metabolic_df_unsupervised %>% 
  select( -MetabolicSyndrome)

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
par(mfrow = c(1,1))
plot(PVE, xlab = "Principal Component", ylab = "Prop of variance Explained", type = "b", ylim = c(0,1))
# Shows prop of variance explained cumualaivte of each PCA as we go through list
plot(cumsum(PVE), xlab = "Principal component", ylab = "cummulative prop of variance Explained", type = "b")

# Elbow method select PC values for model
library(factoextra)
fviz_eig(pca) # Select first 3 PC values for our model

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

#===============================================================================
# What the loading tells us is the most impact a var has on the PC
# PCA1: WaistCirc > BMI > HD: > Tri > Blood Gluc > UricAcid
# PCA2: UrAlbCr > Age > Blood Glucose > HDL > BMI > Income
#===============================================================================

#===============================================================================
# K-means clustering
#===============================================================================

metabolic_df_unsupervised_clustering <- pca$x[,1:2] # Select first 2 PC
metabolic_df_unsupervised_clustering <- as.data.frame(cbind(metabolic_df_unsupervised_clustering, diagnosis)) # MetSyn = 1, NO METSYN = 0
k2 <- kmeans(metabolic_df_unsupervised_clustering[1:2], centers = 2, nstart = 25) # We know k=2 domain knowledge

metabolic_df_unsupervised_clustering$cluster <- factor(k2$cluster)

# Produce table
table <- metabolic_df_unsupervised_clustering %>% 
  mutate(diagnosis = case_when(diagnosis == 1 ~ "MetSyn",
                               diagnosis == 0 ~ "No MetSyn"))

cont_table_kmeans <- (table(table$diagnosis, table$cluster))
(accuracy_kmeans <- sum(diag(cont_table_kmeans)/sum(cont_table_kmeans)))

#===============================================================================
# Obtained accuracy of 77% via unsupervised k-means clustering with PCA
# This is on par with supervised method
#===============================================================================




































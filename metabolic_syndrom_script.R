# Install library===============================================================

library(VIM) # Used for imputation
library(MASS)
library(class)
#library(klaR)
library(dplyr)
library(pROC)

# Set seed for reproducability - arbitrarily selected
set.seed(2343)


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

metabolic_df$Marital[is.na(metabolic_df$Marital)] <- "unknown"

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
  dplyr::select(-seqn  , -MetabolicSyndrome)

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

library(grid)
library(gridExtra)
grid.arrange(age_boxplot, income_boxplot, waist_boxplot, bmi_boxplot, urAlbCr_boxplot, uric_boxplot, blood_glucos_boxplot, hdl_boxplot, tri_boxplot)

# Based on the boxplot the following transformation was conducted]
metabolic_df <- metabolic_df %>% 
  mutate(UrAlbCr = log(as.integer(UrAlbCr)))
(uric_boxplot <- ggplot(metabolic_df, aes(x=MetabolicSyndrome, y=UricAcid)) +
    geom_boxplot() ) # Can now see almost identical distrubtion


(race_bar <- ggplot(metabolic_df, aes(x=Race)) +
    geom_bar() ) 
# We observe that a large portion of our data is: White, Black & Asian
# We are unsure the ethenicity of other, and since accounts for <10% of data
# we elected to remove it

(sex_bar <- ggplot(metabolic_df, aes(x=Sex)) +
    geom_bar() ) 


(relo_bar <- ggplot(metabolic_df, aes(x=Marital)) +
    geom_bar() ) 
# Previous studies have shown that relationship status can influence health status
# https://pubmed.ncbi.nlm.nih.gov/29976034/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6166117/#:~:text=%5B6%5D%20have%20reported%20that%20women,physical%20and%20psychological%20support%2C%20especially


(alb_bar <- ggplot(metabolic_df, aes(x=Albuminuria)) +
    geom_bar() ) 
# Unsure what 0,1 & 2 refer to, might have to drop this variable if no data
# dict can be found - limitation of study and source data.
# Though, most obseervations are classed as 0 and could result in noise in our model


(ms_bar <- ggplot(metabolic_df, aes(x=MetabolicSyndrome)) +
    geom_bar() ) 

grid.arrange(race_bar, sex_bar, relo_bar, alb_bar, ms_bar)

#===============================================================================
# REMOVE/CREATE VARS
# Since we do not know what other stands for we will remove it, also accounts
# for less then 10% of all observations
#===============================================================================


n_row_initial <- nrow(metabolic_df)

metabolic_df <- metabolic_df %>% 
  filter(Race != "Other") %>% 
  filter(Marital !=  "unknown") %>% 
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
# Testing significance 
# We elected to use a non parametric test as we know from previous steps that our data
# has many outliers, normality is often violated etc.
# For numerical varibles we elected to do a Two sample Wilcoxon test for independent samples
#===============================================================================


metabolic_df_sig <- metabolic_df %>% 
  select(-seqn)

# For p less then 0.05 we find evidence to support that varible is different between
# metabolic syndrome status 
age_zw <- wilcox.test(Age ~ MetabolicSyndrome, data = metabolic_df_sig, alternative="two.sided",mu=0) # P < 0.05
income_zw <- wilcox.test(Income ~ MetabolicSyndrome, data = metabolic_df_sig, alternative="two.sided",mu=0) # P< 0.05
waist_zw <- wilcox.test(WaistCirc ~ MetabolicSyndrome, data = metabolic_df_sig, alternative="two.sided",mu=0) # P < 0.05
BMI_zw <- wilcox.test(BMI ~ MetabolicSyndrome, data = metabolic_df_sig, alternative="two.sided",mu=0) # P < 0.05
uric_a_zw <- wilcox.test(UricAcid ~ MetabolicSyndrome, data = metabolic_df_sig, alternative="two.sided",mu=0) # P < 0.05
glucose_zw <- wilcox.test(BloodGlucose ~ MetabolicSyndrome, data = metabolic_df_sig, alternative="two.sided",mu=0) # P < 0.05
HDL_zw <- wilcox.test(HDL ~ MetabolicSyndrome, data = metabolic_df_sig, alternative="two.sided",mu=0) # P < 0.05
tri_zw <- wilcox.test(Triglycerides ~ MetabolicSyndrome, data = metabolic_df_sig, alternative="two.sided",mu=0) # P < 0.05


# For categorical varibles we elected to do chi sqaure test
# Ho Two vars independent of one another in relation to MS status
# H1 Two vars are related to one another
chisq.test(y= metabolic_df_sig$MetabolicSyndrome, x=metabolic_df_sig$Sex) # P 0.351 - reject H1
chisq.test(y= metabolic_df_sig$MetabolicSyndrome, x=metabolic_df_sig$Marital) # P < 0.05 - reject H0
chisq.test(y= metabolic_df_sig$MetabolicSyndrome, x=metabolic_df_sig$Race) # P < 0.05 - reject H0



#===============================================================================
#
# SUPERVISED LEARNING 
#
# Naive Bayes - 4 different models
#===============================================================================

library(caret) # used to split data

#===============================================================================
# NAIVE BAYES: Model 1 & 2
#===============================================================================
set.seed(2343)
metabolic_df_supervised_nb <- metabolic_df_supervised %>% 
  select(-seqn)

library(psych)
pairs.panels(metabolic_df_supervised_nb[,-13]) # Observe  cor feature <- assumption violation



split_nb <- createDataPartition(metabolic_df_supervised_nb$MetabolicSyndrome, p = 0.8, list = F)
train_nb <- metabolic_df_supervised_nb[split_nb, ]
test_nb <- metabolic_df_supervised_nb[-split_nb, ]
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
# NAIVE BAYES: Model 3
# Note we only use the non-gausian distribution in the report
#===============================================================================
# At this stage we elect to remove waist circumstance to see if it improves our model and
# to ensure all assumptions are satisfied 


#metabolic_df_supervised_nb <- metabolic_df_supervised %>% 
#  select(-seqn)

#metabolic_df_supervised_nb_2 <- metabolic_df_supervised_nb %>% 
#  select(-BMI)

#split_nb_2 <- createDataPartition(metabolic_df_supervised_nb_2$MetabolicSyndrome, p = 0.8, list = F)
train_nb_2 <- train_nb %>% 
  select(-BMI)
test_nb_2 <- test_nb %>% 
  select(-BMI)

c(nrow(train_nb_2), nrow(test_nb_2)) 

# Produce models
nb_g_2 <- naive_bayes(MetabolicSyndrome ~ ., data = train_nb_2, usekernel = F)
nb_gf_2 <- naive_bayes(MetabolicSyndrome ~ ., data = train_nb_2, usekernel = T)



# Produce confusion matrix
pred_g_2 <- predict(nb_g_2, newdata = test_nb_2)
confusionMatrix(pred_g_2, test_nb_2$MetabolicSyndrome)

pred_gf_2 <- predict(nb_gf_2, newdata = test_nb_2)
confusionMatrix(pred_gf_2, test_nb_2$MetabolicSyndrome)


(roc_gaussian_2 <- roc(as.numeric(test_nb_2$MetabolicSyndrome), as.numeric(pred_g_2))) # AUC 0.7569
(roc_kernal_2 <- roc(as.numeric(test_nb_2$MetabolicSyndrome), as.numeric(pred_gf_2))) # AUC 0.8167 

# Findings: 
# AUC 0.85 remove BMI
# AUC 0.82 remove waist cir
# AUC 0.85 remove HDL
# AUC 0.82 remove Tri
# AUC 0.85 remove BMI and HDL


#===============================================================================
# NAIVE BAYES DISC
# We observe that our assumptions have been violated, such as conditional independence 
# amongst predictor variables, though Naive bayes can handle this generally 
# We observe that our data is not Gausian Distrubtion - as such we can apply
# a non-paramatric feture in the naivebayes() function - this resulted in a 
# improved model, both in accuracy and AUC
#===============================================================================

#===============================================================================
# NAIVE BAYES: Model 4
# Attempt to improve our model we use 10-fold cross validation and SMOTE
#===============================================================================

ctrl <- trainControl(sampling = "smote")
nb_cv_smote <- train(MetabolicSyndrome ~ ., data = train_nb_2,
                               method = "nb",
                               trControl = ctrl)

print(nb_cv_smote)
pred_cv_smote <- predict(nb_cv_smote, newdata = test_nb_2)
confusionMatrix(pred_cv_smote, test_nb_2$MetabolicSyndrome)

roc_cv_smote <- roc(as.numeric(test_nb_2$MetabolicSyndrome), as.numeric(pred_cv_smote))
auc(roc_cv_smote) # 0.8221


#===============================================================================
# logit Supervised learning 
# We retain BMI
# We test assumptions last - we will only test assumption on best
# performing model 
#===============================================================================
set.seed(2343) # Ensure when you run model we run all of it with sed seed, otherwise it will not work if you do it in chunks.
# Convert categorical variables into dummy variables
metabolic_df_logit <- metabolic_df_supervised %>% 
  select(-seqn, - MetabolicSyndrome)

diagnosis <- metabolic_df_supervised$MetabolicSyndrome

dummy_vars_logit <- dummyVars(~ ., data = metabolic_df_logit)
train_dummy_logit <- predict(dummy_vars_logit, metabolic_df_logit)
train_dummy_logit <- as.data.frame(train_dummy_logit)
train_dummy_logit <- cbind(train_dummy_logit, diagnosis)

# Bind diagnosis back to data frame with dummy varibles
#train_dummy_logit$diagnosis <- ifelse(train_dummy_logit$diagnosis == "No MetSyn", 0 , 1)
train_dummy_logit$diagnosis <- train_dummy_logit$diagnosis
str(train_dummy_logit)

# Convert variables to factor level
sum(train_dummy_logit$Marital.unknown) # 0, must be stored as memory as removed
sum(train_dummy_logit$Race.Other) # , must be stored as memory as removed before

train_dummy_logit <- train_dummy_logit %>% 
  mutate(Sex.Female = as.factor(Sex.Female),
         Sex.Male = as.factor(Sex.Male),
         Marital.Divorced = as.factor(Marital.Divorced),
         Marital.Married = as.factor(Marital.Married),
         Marital.Separated = as.factor(Marital.Separated),
         Marital.Single = as.factor(Marital.Single),
         Marital.Widowed = as.factor(Marital.Widowed),
         Race.Asian = as.factor(Race.Asian),
         Race.Black = as.factor(Race.Black),
         Race.Hispanic = as.factor(Race.Hispanic),
         Race.MexAmerican = as.factor(Race.MexAmerican),
         Race.White = as.factor(Race.White),
         diagnosis = as.factor(diagnosis)) %>% 
  select(-Marital.unknown, -Race.Other)
str(train_dummy_logit)

# Our data frame is now in the required format, we have set our binary dummy varibles to factors with 2 level
# and all other data types are correct
# !! We do assumption testing after our model is developed !!
# The only main assumption to test first is that our response (diagnosis) is binary, which it has been converted

split_logit <- createDataPartition(train_dummy_logit$diagnosis, p=0.8, list = F)
train_logit <- train_dummy_logit[split_logit, ]
test_logit <- train_dummy_logit[-split_logit, ]
c(nrow(train_logit), nrow(test_logit))

#===============================================================================
# LOGIT MODEL: Basic
#===============================================================================
logit_fit <- glm(diagnosis ~ ., data = train_logit,
                 family = binomial(link = "logit"))

logs_odd_ratio <- predict(logit_fit, newdata = test_logit, type = "link")
proba <- predict(logit_fit, newdata = test_logit, type = "response")
pred_on_lodds <- ifelse(logs_odd_ratio > 0, "MetSyn", "No MetSyn")
pred_on_proba <- ifelse(proba > 0.5, "MetSyn", "No MetSyn")
all(pred_on_lodds == pred_on_proba)
confusionMatrix(as.factor(pred_on_proba), test_logit$diagnosis)

roc_basic <- roc(as.numeric(test_logit$diagnosis), as.numeric(as.factor(pred_on_proba)))
auc(roc_basic) # Terrible confusion matrix


#===============================================================================
# LOGIT MODEL: Leave one out
#===============================================================================

# Cross validation: Leave one out cross validation
ctrl <- trainControl(method = "LOOCV")
logit_fit_loocv <- train(diagnosis ~ ., data = train_logit,
                         method = "glm",
                         family = "binomial",
                         trControl = ctrl)

pred_loocv <- predict(logit_fit_loocv, newdata = test_logit)
confusionMatrix(pred_loocv, test_logit$diagnosis)

roc_loocv <- roc(as.numeric(test_logit$diagnosis), as.numeric(pred_loocv))
auc(roc_loocv) # 0.84



#===============================================================================
# LOGIT MODEL: Cross Validation 10-fold
#===============================================================================
ctrl <- trainControl(method = "repeatedcv", number = 10,
                     savePredictions = TRUE, repeats = 5)
logit_fit_cv <- train(diagnosis ~ ., data = train_logit,
                      method = "glm",
                      family = "binomial",
                      trControl = ctrl)
print(logit_fit_cv)
pred_cv <- predict(logit_fit_cv, newdata = test_logit)
confusionMatrix(pred_cv, test_logit$diagnosis)

roc_cv <- roc(as.numeric(test_logit$diagnosis), as.numeric(pred_cv))
auc(roc_cv) # 0.84

#===============================================================================
# Based on the intial logit models we observe that we might have an issue 
# with unblanced nature impacting our model - we will now compare and contrast
# by including smote into our model
# Could not find ROSE used in the context of health science. 
#
# Based on confusion matrix we observe LOOCV had best outcome, thus we will
# see if we can improve it by accounting for an unblaanced data set
#===============================================================================

#===============================================================================
# LOGIT MODEL: Leave one out - SMOTE applied
#===============================================================================

ctrl <- trainControl(method = "LOOCV", sampling = "smote")
logit_fit_loocv_smote <- train(diagnosis ~ ., data = train_logit,
                         method = "glm",
                         family = "binomial",
                         trControl = ctrl)

pred_loocv_smote <- predict(logit_fit_loocv_smote, newdata = test_logit)
confusionMatrix(pred_loocv_smote, test_logit$diagnosis)

roc_loocv_smote <- roc(as.numeric(test_logit$diagnosis), as.numeric(pred_loocv_smote))
auc(roc_loocv_smote) # 0.8277

#===============================================================================
# FINDINGS: 
# We find when we apply the SMOTE method for LOOCV that we have an increase
# in false positives - this results in a reduction in our AUC slightly
# though, we have an increase by 0.10 in sensitivity and a 0.9 reduction in
# specificity - we also have a reduction in accuracy in our model.
# This is because our model has a reduction in original biased towards no met 
# syndrome.
# For the purpose of health applications we want to maxmimise sensitivity 
# as failure to do so can lead to delayed treatment
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




































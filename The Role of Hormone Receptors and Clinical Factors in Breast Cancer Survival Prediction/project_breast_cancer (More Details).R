# Libraries
library(dplyr)
library(ggplot2)
library(MASS)
library(corrplot) # correlation plot
library(car) # for "scatterplotMatrix" function.
library(tree) ## for tree
library(class)

# Load the Dataset
breast_cancer_df <- read.csv('nkiproject.csv')

#change the tumor size variable name more readable
breast_cancer_df <- breast_cancer_df %>%
  rename(Cl03_Size_mm = Cl03_Size_.mm.)

##### EDA #####
breast_cancer_df$M02G_TP53 <- NULL  # has 81 missing value
breast_cancer_df$I06_CMYC_2007 <- NULL # has 60 missing value
breast_cancer_df$M01B_PIK3CA <- NULL # has 33 missing value

colSums(is.na(breast_cancer_df))
# Check missing values row!
which(is.na(breast_cancer_df$I01_ER_2007))
# 54  74  90 102 113 151 191 207 209
which(is.na(breast_cancer_df$I02_PR_2007))
# 54  74  90 102 113 151 191 209 283
which(is.na(breast_cancer_df$I03_HER2_2007))
# 1  11  33  37  44  54  60  74  90 102 111 113 123 124 140 151 155 179 182 191 234 283

# These 3 variables missing values in same rows
which(is.na(breast_cancer_df$H04_centralcoll))
which(is.na(breast_cancer_df$H05_matrix))
which(is.na(breast_cancer_df$H06_necrosis))

### Check which of them missing!
which(is.na(breast_cancer_df$I02B_ER_PR_clinical))
which(is.na(breast_cancer_df$I03_HER2_2007))
which(is.na(breast_cancer_df$I11_P53_2007))

### Covert categorical variables to factor if necessary!
breast_cancer_df$B01_Profile_70_genes <- as.factor(breast_cancer_df$B01_Profile_70_genes)
breast_cancer_df$E02_Event_DMFS_2005 <- as.factor(breast_cancer_df$E02_Event_DMFS_2005)
breast_cancer_df$I03_HER2_2007 <- as.factor(breast_cancer_df$I03_HER2_2007)
breast_cancer_df$H02A_Type2007 <- as.factor(breast_cancer_df$H02A_Type2007)
breast_cancer_df$H03_ANGIOINV2001 <- as.factor(breast_cancer_df$H03_ANGIOINV2001)
breast_cancer_df$H04_centralcoll <- as.factor(breast_cancer_df$H04_centralcoll)
breast_cancer_df$H05_matrix <- as.factor(breast_cancer_df$H05_matrix)
breast_cancer_df$H06_necrosis <- as.factor(breast_cancer_df$H06_necrosis)
breast_cancer_df$H08D_grade_07 <- as.factor(breast_cancer_df$H08D_grade_07)
breast_cancer_df$H09A_lymphinf2005 <- as.factor(breast_cancer_df$H09A_lymphinf2005)
breast_cancer_df$I02B_ER_PR_clinical <- as.factor(breast_cancer_df$I02B_ER_PR_clinical)

summary(breast_cancer_df)


# New dataframe for clinical data
clinical_df <- breast_cancer_df[, 2:18]
str(clinical_df)
#View(breast_cancer_df)
dim(clinical_df)
summary(clinical_df)

## correlation!
# Select only numeric columns from the clinical_df
numeric_df <- clinical_df[sapply(clinical_df, is.numeric)]
cor_matrix <- cor(numeric_df,  use =  "pairwise.complete.obs")
corrplot(cor_matrix, method = 'square', type = 'full', insig='blank',
         addCoef.col ='black', number.cex = 0.6, diag=FALSE)

## Handling missing Values
colSums(is.na(clinical_df))

# removed the missing values
reduced_clinical <- subset(clinical_df, subset = !is.na(I01_ER_2007) & !is.na(I02_PR_2007) & !is.na(I03_HER2_2007))

reduced_clinical <- na.omit(clinical_df)

## Handling missing Values
colSums(is.na(reduced_clinical))
dim(reduced_clinical)

str(reduced_clinical)
### Visualisations
# Create a boxplot using ggplot2
ggplot(reduced_clinical, aes(x = as.factor(E02_Event_DMFS_2005), y = Cl01_Age)) +
  geom_boxplot() +
  labs(x = "E02 Event DMFS 2005", y = "Age", 
       title = "Boxplot of Age by E02 Event DMFS 2005") +
  theme_minimal()

ggplot(reduced_clinical, aes(x = as.factor(E02_Event_DMFS_2005), y = Cl02_pN_pos)) +
  geom_boxplot() +
  labs(x = "E02 Event DMFS 2005", y = "Cl02_pN_pos", 
       title = "Boxplot of Cl02_pN_pos by E02 Event DMFS 2005") +
  theme_minimal()

ggplot(reduced_clinical, aes(x = as.factor(E02_Event_DMFS_2005), y = Cl03_Size_mm)) +
  geom_boxplot() +
  labs(x = "E02 Event DMFS 2005", y = "Cl03_Size_.mm.", 
       title = "Boxplot of Cl03_Size_.mm. by E02 Event DMFS 2005") +
  theme_minimal()

ggplot(reduced_clinical, aes(x = as.factor(E02_Event_DMFS_2005), y = I01_ER_2007)) +
  geom_boxplot() +
  labs(x = "E02 Event DMFS 2005", y = "I01_ER_2007", 
       title = "Boxplot of I01_ER_2007 by E02 Event DMFS 2005") +
  theme_minimal()

ggplot(reduced_clinical, aes(x = as.factor(E02_Event_DMFS_2005), y = I02_PR_2007)) +
  geom_boxplot() +
  labs(x = "E02 Event DMFS 2005", y = "I02_PR_2007", 
       title = "Boxplot of I02_PR_2007 by E02 Event DMFS 2005") +
  theme_minimal()

ggplot(reduced_clinical, aes(x = as.factor(E02_Event_DMFS_2005), y = I11_P53_2007)) +
  geom_boxplot() +
  labs(x = "E02 Event DMFS 2005", y = "I11_P53_2007", 
       title = "Boxplot of I11_P53_2007 by E02 Event DMFS 2005") +
  theme_minimal()

#mean table 
library(dplyr)

# Summarize the mean of each variable based on E02_Event_DMFS_2005
mean_table <- reduced_clinical %>%
  group_by(E02_Event_DMFS_2005) %>%
  summarise(
    Mean_Age = mean(Cl01_Age, na.rm = TRUE),
    Mean_pN_pos = mean(Cl02_pN_pos, na.rm = TRUE),
    Mean_Size_mm = mean(Cl03_Size_mm, na.rm = TRUE),
    Mean_ER_2007 = mean(I01_ER_2007, na.rm = TRUE),
    Mean_PR_2007 = mean(I02_PR_2007, na.rm = TRUE),
    Mean_P53_2007 = mean(I11_P53_2007, na.rm = TRUE)
  )
mean_table

#Median table

median_table <- reduced_clinical %>%
  group_by(E02_Event_DMFS_2005) %>%
  summarise(
    Median_Age = median(Cl01_Age, na.rm = TRUE),
    Median_pN_pos = median(Cl02_pN_pos, na.rm = TRUE),
    Median_Size_mm = median(Cl03_Size_mm, na.rm = TRUE),
    Median_ER_2007 = median(I01_ER_2007, na.rm = TRUE),
    Median_PR_2007 = median(I02_PR_2007, na.rm = TRUE),
    Median_P53_2007 = median(I11_P53_2007, na.rm = TRUE)
  )

median_table


table(breast_cancer_df$E02_Event_DMFS_2005)

# I02B_ER_PR_clinical: ER/PR Group: 1=ER+/PR+, 2=ER+/PR-, 3=ER-/PR-, 4=ER-/PR+
table(breast_cancer_df$E02_Event_DMFS_2005, breast_cancer_df$I02B_ER_PR_clinical)

# Table of DMFS vs HER2
table(breast_cancer_df$E02_Event_DMFS_2005, breast_cancer_df$I03_HER2_2007)

table(breast_cancer_df['B01_Profile_70_genes'])


## Function for fitting an ellipse. ##


fit.ellipse <- function (x, y = NULL) 
{
  EPS <- 1.0e-8 
  dat <- xy.coords(x, y) 
  
  D1 <- cbind(dat$x * dat$x, dat$x * dat$y, dat$y * dat$y) 
  D2 <- cbind(dat$x, dat$y, 1) 
  S1 <- t(D1) %*% D1 
  S2 <- t(D1) %*% D2 
  S3 <- t(D2) %*% D2 
  T <- -solve(S3) %*% t(S2) 
  M <- S1 + S2 %*% T 
  M <- rbind(M[3, ] / 2, -M[2, ], M[1, ] / 2) 
  evec <- eigen(M)$vec 
  cond <- 4 * evec[1, ] * evec[3, ] - evec[2, ] ^ 2 
  a1 <- evec[, which(cond > 0)] 
  f <- c(a1, T %*% a1) 
  names(f) <- letters[1 : 6] 
  
  A <- matrix(c(2 * f[1], f[2], f[2], 2 * f[3]), nrow = 2, ncol = 2, byrow = T )
  b <- matrix(c(-f[4], -f[5]), nrow = 2, ncol = 1, byrow = T)
  soln <- solve(A) %*% b
  
  b2 <- f[2] ^ 2 / 4
  
  center <- c(soln[1], soln[2]) 
  names(center) <- c("x", "y") 
  
  num  <- 2 * (f[1] * f[5] ^ 2 / 4 + f[3] * f[4] ^ 2 / 4 + f[6] * b2 - f[2] * f[4] * f[5] / 4 - f[1] * f[3] * f[6]) 
  den1 <- (b2 - f[1] * f[3]) 
  den2 <- sqrt((f[1] - f[3]) ^ 2 + 4 * b2) 
  den3 <- f[1] + f[3] 
  
  semi.axes <- sqrt(c( num / (den1 * (den2 - den3)),  num / (den1 * (-den2 - den3)) )) 
  
  # calculate the angle of rotation 
  term <- (f[1] - f[3]) / f[2] 
  angle <- atan(1 / term) / 2 
  
  list(coef = f, center = center, major = max(semi.axes), minor = min(semi.axes), angle = unname(angle)) 
}


# Function that returns x- and 
#y-coordinates for an ellipse.


get.ellipse <- function(fit, n = 360) 
{
  tt <- seq(0, 2 * pi, length = n) 
  sa <- sin(fit$angle) 
  ca <- cos(fit$angle) 
  ct <- cos(tt) 
  st <- sin(tt) 
  
  x <- fit$center[1] + fit$maj * ct * ca - fit$min * st * sa 
  y <- fit$center[2] + fit$maj * ct * sa + fit$min * st * ca 
  
  cbind(x = x, y = y) 
}


# Scatter plot of er vs pr
plot(reduced_clinical$I01_ER_2007, reduced_clinical$I02_PR_2007, 
     xlab = "I01_ER_2007", 
     ylab = "I02_PR_2007", 
     main = "Scatterplot of I01_ER_2007 vs. \nPI02_PR_2007", 
     col = reduced_clinical$E02_Event_DMFS_2005 + 1,
     pch = 19, cex = 1.1)
legend(x ='right', legend = c("Negative", "Pozitive"), pch = 19, col = 1 : 2)
group.1.ellipse <- fit.ellipse(reduced_clinical$I01_ER_2007[reduced_clinical$E02_Event_DMFS_2005 == 0], reduced_clinical$I02_PR_2007[reduced_clinical$E02_Event_DMFS_2005 == 0])
group.2.ellipse <- fit.ellipse(reduced_clinical$I01_ER_2007[reduced_clinical$E02_Event_DMFS_2005 == 1], reduced_clinical$I02_PR_2007[reduced_clinical$E02_Event_DMFS_2005 == 1])

points(get.ellipse(group.1.ellipse), type = "l", lty = 2, lwd = 1.5)
points(get.ellipse(group.2.ellipse), type = "l", col = 2, lty = 2, lwd = 1.5)

# Scatter plot of age vs Size(mm)
plot(reduced_clinical$Cl01_Age, reduced_clinical$Cl03_Size_mm, 
     xlab = "Age", 
     ylab = "Tumor Size(mm)", 
     main = "Scatterplot of Age vs. Tumor Size (mm)", 
     col = reduced_clinical$E02_Event_DMFS_2005 + 1,
     pch = 19, cex = 1.1)
legend(x ='topleft', legend = c("Negative", "Pozitive"), pch = 19, col = 1 : 2)

# Scatter plot of Number of positive lymph nodes vs Size(mm)
plot(reduced_clinical$Cl02_pN_pos, reduced_clinical$Cl03_Size_mm, 
     xlab = "Number of positive lymph nodes", 
     ylab = "Tumor Size(mm)", 
     main = "Scatterplot of Number of positive lymph nodes vs. Tumor Size (mm)", 
     col = reduced_clinical$E02_Event_DMFS_2005 + 1,
     pch = 19, cex = 1.1)
legend(x ='topright', legend = c("Negative", "Pozitive"), pch = 19, col = 1 : 2)

# Scatter plot of age vs Number of positive lymph nodes
plot(reduced_clinical$Cl01_Age, reduced_clinical$Cl02_pN_pos, 
     xlab = "Age", 
     ylab = "Size(mm)", 
     main = "Scatterplot of Age vs. Number of positive lymph nodes", 
     col = reduced_clinical$E02_Event_DMFS_2005 + 1,
     pch = 19, cex = 1.1)
legend(x ='topright', legend = c("Negative", "Pozitive"), pch = 19, col = 1 : 2)

# Scatter plot of er vs pr on HER2 protein
plot(reduced_clinical$I01_ER_2007, reduced_clinical$I02_PR_2007, 
     xlab = "I01_ER_2007", 
     ylab = "I02_PR_2007", 
     main = "Scatterplot of I01_ER_2007 vs. \nPI02_PR_2007", 
     col = reduced_clinical$I03_HER2_2007,
     pch = 19, cex = 1.1)
legend(x ='left', legend = c("Negative", "Pozitive"), pch = 19, col = 1 : 2)

# Scatter plot of er vs pr on HER2 protein
plot(reduced_clinical$I01_ER_2007, reduced_clinical$I02_PR_2007, 
     xlab = "I01_ER_2007", 
     ylab = "I02_PR_2007", 
     main = "Scatterplot of I01_ER_2007 vs. \nPI02_PR_2007", 
     col = reduced_clinical$I02B_ER_PR_clinical,
     pch = 19, cex = 1.1)
legend(x ='left', legend = c("ER+/PR+", "ER+/PR-", "ER-/PR-", "ER-/PR+"), pch = 19, col = 1 : 4)

#pair plot

library(GGally)
pair_data <- reduced_clinical[, c("E02_Event_DMFS_2005", "Cl01_Age", "Cl02_pN_pos", "Cl03_Size_mm", "I01_ER_2007", "I02_PR_2007", "I11_P53_2007")]

ggpairs(pair_data, aes(color = as.factor(E02_Event_DMFS_2005))) +
  labs(title = "Pair Plot of Clinical Variables by Distant Metastasis Status")


#log transformation 

##### LDA #####
scatterplotMatrix(~Cl01_Age + Cl02_pN_pos + Cl03_Size_mm + I01_ER_2007 + I02_PR_2007, data = reduced_clinical, subset = reduced_clinical$E02_Event_DMFS_2005 == 0, main = "NO")
scatterplotMatrix(~Cl01_Age + Cl02_pN_pos + Cl03_Size_mm + I01_ER_2007 + I02_PR_2007, data = reduced_clinical, subset = reduced_clinical$E02_Event_DMFS_2005 == 1, main = "YES")

#log transform
scatterplotMatrix(~log(Cl01_Age) + log(Cl02_pN_pos + 1) + sqrt(Cl03_Size_mm), data = reduced_clinical, subset = reduced_clinical$E02_Event_DMFS_2005 == 0, main = "NO")
scatterplotMatrix(~log(Cl01_Age) + log(Cl02_pN_pos + 1) + sqrt(Cl03_Size_mm), data = reduced_clinical, subset = reduced_clinical$E02_Event_DMFS_2005 == 1, main = "YES")


## LDA 3 variables
breas_cancer_lda <- lda(E02_Event_DMFS_2005 ~ log(Cl01_Age) + log(Cl02_pN_pos + 1) + log(Cl03_Size_mm + 1), data = reduced_clinical)
breas_cancer_lda

confusion_matrix <- table(predict(breas_cancer_lda)$class, reduced_clinical$E02_Event_DMFS_2005)
confusion_matrix

hit_rate <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
hit_rate

## LDA 5 
breas_cancer_lda_5 <- lda(E02_Event_DMFS_2005 ~ log(Cl01_Age) + log(Cl02_pN_pos + 1) + log(Cl03_Size_mm + 1) + I01_ER_2007 + I02_PR_2007, data = reduced_clinical)
breas_cancer_lda_5

confusion_matrix_5 <- table(predict(breas_cancer_lda_5)$class, reduced_clinical$E02_Event_DMFS_2005)
confusion_matrix_5

hit_rate_5 <- sum(diag(confusion_matrix_5)) / sum(confusion_matrix_5)
hit_rate_5

# LDA with CV
breas_cancer_lda_cv <- lda(E02_Event_DMFS_2005 ~ log(Cl01_Age) + log(Cl02_pN_pos + 1) + log(Cl03_Size_mm + 1), data = reduced_clinical, CV = T)
breas_cancer_lda_cv

confusion_matrix_cv <- table(breas_cancer_lda_cv$class, reduced_clinical$E02_Event_DMFS_2005)
confusion_matrix_cv

hit_rate_cv <- sum(diag(confusion_matrix_cv)) / sum(confusion_matrix_cv)
hit_rate_cv
table(reduced_clinical$E02_Event_DMFS_2005)
#~~~~~ Cross-validation

# Specify a vector that will consider a sequence of potential prior probabilities.
new_priors <- seq(0.001, 0.999, by = 0.001)
new_priors
# Construct an empty vector that will store hit rates corresponding to the various priors.
hit_rate_vec <- rep(NA, length(new_priors))

# Use a "for" loop to cycle through the various priors.
for(i in 1 : length(new_priors))
{
  # Carry out leave-one-out cross-validation for given priors.
  breas_cancer_lda_cv <- lda(E02_Event_DMFS_2005 ~ log(Cl01_Age) + log(Cl02_pN_pos + 1) + log(Cl03_Size_mm + 1), data = reduced_clinical, CV = TRUE, prior = c(new_priors[i], 1 - new_priors[i]))
  # Calculate the hit rate from the confusion matrix.
  hit_rate_vec[i] <- sum(diag(table(breas_cancer_lda_cv$class, clinical_df$E02_Event_DMFS_2005))) / sum(table(breas_cancer_lda_cv$class, clinical_df$E02_Event_DMFS_2005))
}

# Output the index that maximizes the hit rate.
order(hit_rate_vec, decreasing = TRUE)

# Store the index that maximises the hit rate.
optimal_index <- order(hit_rate_vec, decreasing = TRUE)[1]
optimal_index
# Output the prior corresponding to this index.
new_priors[optimal_index]

# Carry out the linear discriminant analysis based on this prior.
breas_cancer_lda_cv <- lda(E02_Event_DMFS_2005 ~ log(Cl01_Age) + log(Cl02_pN_pos + 1) + log(Cl03_Size_mm + 1), data = clinical_df, CV = T, prior = c(new_priors[optimal_index], 1 - new_priors[optimal_index]))

# Output the corresponding confusion matrix.
confusion_matrix_cv <- table(breas_cancer_lda_cv$class, clinical_df$E02_Event_DMFS_2005)
confusion_matrix_cv

hit_rate_cv <- sum(diag(confusion_matrix_cv)) / sum(confusion_matrix_cv)
hit_rate_cv

# LDA5 with CV
breas_cancer_lda_cv_5 <-  lda(E02_Event_DMFS_2005 ~ log(Cl01_Age) + log(Cl02_pN_pos + 1) + log(Cl03_Size_mm + 1) + I01_ER_2007 + I02_PR_2007, data = reduced_clinical, CV = T)
breas_cancer_lda_cv_5

confusion_matrix_cv_5 <- table(breas_cancer_lda_cv_5$class, reduced_clinical$E02_Event_DMFS_2005)
confusion_matrix_cv_5

hit_rate_cv <- sum(diag(confusion_matrix_cv_5)) / sum(confusion_matrix_cv_5)
hit_rate_cv
table(reduced_clinical$E02_Event_DMFS_2005)
#~~~~~ Cross-validation for LDA 

# Specify a vector that will consider a sequence of potential prior probabilities.
new_priors <- seq(0.001, 0.999, by = 0.001)

# Construct an empty vector that will store hit rates corresponding to the various priors.
hit_rate_vec <- rep(NA, length(new_priors))

# Use a "for" loop to cycle through the various priors.
for(i in 1 : length(new_priors))
{
  # Carry out leave-one-out cross-validation for given priors.
  breas_cancer_lda_cv_5 <- lda(E02_Event_DMFS_2005 ~ log(Cl01_Age) + log(Cl02_pN_pos + 1) + log(Cl03_Size_mm + 1) + I01_ER_2007 + I02_PR_2007, data = reduced_clinical, CV = TRUE, prior = c(new_priors[i], 1 - new_priors[i]))
  # Calculate the hit rate from the confusion matrix.
  hit_rate_vec[i] <- sum(diag(table(breas_cancer_lda_cv_5$class, reduced_clinical$E02_Event_DMFS_2005))) / sum(table(breas_cancer_lda_cv_5$class, reduced_clinical$E02_Event_DMFS_2005))
}

# Output the index that maximises the hit rate.
order(hit_rate_vec, decreasing = TRUE)

# Store the index that maximises the hit rate.
optimal_index <- order(hit_rate_vec, decreasing = TRUE)[1]

# Output the prior corresponding to this index.
new_priors[optimal_index]

# Carry out the linear discriminant analysis based on this prior.
breas_cancer_lda_cv_5 <- lda(E02_Event_DMFS_2005 ~ log(Cl01_Age) + log(Cl02_pN_pos + 1) + log(Cl03_Size_mm + 1) + I01_ER_2007 + I02_PR_2007, data = reduced_clinical, CV = T, prior = c(new_priors[optimal_index], 1 - new_priors[optimal_index]))

# Output the corresponding confusion matrix.
confusion_matrix_cv_5 <- table(breas_cancer_lda_cv_5$class, reduced_clinical$E02_Event_DMFS_2005)
confusion_matrix_cv_5

hit_rate_cv <- sum(diag(confusion_matrix_cv_5)) / sum(confusion_matrix_cv_5)
hit_rate_cv



##### LDA CV with costs
costs <- c(1, 2)

clinic.lda.cv <- lda(E02_Event_DMFS_2005 ~ Cl01_Age + log(Cl02_pN_pos +1) + log(Cl03_Size_mm +1) + I01_ER_2007 + I02_PR_2007, data = reduced_clinical)
clinic.lda.cv
# Produce the confusion matrix based on both linear discriminant functions.
confusion.matrix <- table(clinic.lda.cv$class, reduced_clinical$E02_Event_DMFS_2005)
confusion.matrix

# Calculate the loss based on costs and misclassifications.
loss <- 0 # Initialise the loss to be 0

for(i in 1 : ncol(confusion.matrix))
{
  loss <- loss + costs[i] * sum(confusion.matrix[-i, i])
}

# Print the loss to the screen
loss

#~~~~~ previous priors ~~~~~#
table(reduced_clinical$E02_Event_DMFS_2005)/sum(table(reduced_clinical$E02_Event_DMFS_2005))

#~~~~~ Updated priors based on costs ~~~~~#
costs * table(reduced_clinical$E02_Event_DMFS_2005)
# Create new priors based on costs.
new.prior <- costs * table(reduced_clinical$E02_Event_DMFS_2005) / sum(costs * table(reduced_clinical$E02_Event_DMFS_2005))
new.prior

# Linear discriminant analysis of 
clinic.lda.cv <- lda(E02_Event_DMFS_2005 ~ Cl01_Age + Cl02_pN_pos + Cl03_Size_mm + I01_ER_2007 + I02_PR_2007, data = reduced_clinical, CV = T, prior = new.prior)
clinic.lda.cv

# Produce the confusion matrix based on both linear discriminant functions.
confusion.matrix <- table(clinic.lda.cv$class, reduced_clinical$E02_Event_DMFS_2005)
confusion.matrix

hit_rate_cv <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
hit_rate_cv

# Calculate the loss based on costs and misclassifications.
loss <- 0 # Initialise the loss to be 0

for(i in 1 : ncol(confusion.matrix))
{
  loss <- loss + costs[i] * sum(confusion.matrix[-i, i])
}

# Print the loss to the screen
loss

#ldahist(data = predict(clinic.lda.cv)$x[,1], g = factor(clinic.lda.cv$E02_Event_DMFS_2005))

lda_values <- predict(clinic.lda.cv)$x[, 1]
grouping <- as.factor(reduced_clinical$E02_Event_DMFS_2005)

# Plot the histogram 
ldahist(data = lda_values, g = grouping)



###### DT #####
reduced_clinical$E02_Event_DMFS_2005 <- as.factor(reduced_clinical$E02_Event_DMFS_2005)
reduced_clinical$I03_HER2_2007 <- as.factor(reduced_clinical$I03_HER2_2007)

set.seed(1)
pt <- 0.80 # Train set
inTrain <- sample(1:nrow(reduced_clinical), pt * nrow(reduced_clinical))
train_set <- reduced_clinical[inTrain,]
test_set <- reduced_clinical[-inTrain,]

# DT Model
tree_model <- tree(E02_Event_DMFS_2005 ~ ., data=train_set)

# Pruning with CV
cv_tree <- cv.tree(tree_model, FUN = prune.misclass)

plot(cv_tree$size, cv_tree$dev, type = "b", xlab = "Number of Terminal Nodes (Size)", ylab = "Misclassification Deviance", main = "Cross-validation: Tree Size vs Deviance")

which.min(cv_tree$dev) # 4

# New DT model
optimal_tree <- prune.misclass(tree_model, best = 4)

# new tree
plot(optimal_tree)
text(optimal_tree, pretty = 0)

predicted <- predict(optimal_tree, test_set, type = "class")

# Confusion Matrix
conf_matrix <- table(predicted, test_set$E02_Event_DMFS_2005)
print(conf_matrix)

# Hit Rate
hit_rate <- sum(diag(conf_matrix)) / sum(conf_matrix)
hit_rate

##### KNN ####

reduced_clinical_6 <- reduced_clinical[, c("E02_Event_DMFS_2005", "Cl01_Age", "Cl02_pN_pos", "Cl03_Size_mm", "I01_ER_2007", "I02_PR_2007")]

knn.cv(train = scale(reduced_clinical_6[, 2:6]), cl = reduced_clinical_6$E02_Event_DMFS_2005, k = 1)

#set the random no generator seed 
set.seed(1)

#specifiy the maximum no of the nearest neighbour to consider 
n <- 20

#initialize vector that will store misclassification rate corrensponding each of the three candidate model 
missclassifications.vec.1 <- rep(NA, n)
missclassifications.vec.2 <- rep(NA, n)
missclassifications.vec.3 <- rep(NA, n)
missclassifications.vec.4 <- rep(NA, n)
missclassifications.vec.5 <- rep(NA, n)
missclassifications.vec.6 <- rep(NA, n)

for(i in 1:n){
  
  class.vec.1 <- knn.cv(train = scale(reduced_clinical_7[, 2:3]), cl = reduced_clinical_7$E02_Event_DMFS_2005, k = i)
  class.vec.2 <- knn.cv(train = scale(reduced_clinical_7[, 3:4]), cl = reduced_clinical_7$E02_Event_DMFS_2005, k = i)
  class.vec.3 <- knn.cv(train = scale(reduced_clinical_7[, c(2, 4)]), cl = reduced_clinical_7$E02_Event_DMFS_2005, k = i)
  class.vec.4 <- knn.cv(train = scale(reduced_clinical_7[, 2:4]), cl = reduced_clinical_7$E02_Event_DMFS_2005, k = i)
  class.vec.5 <- knn.cv(train = scale(reduced_clinical_7[, 2:5]), cl = reduced_clinical_7$E02_Event_DMFS_2005, k = i)
  class.vec.6 <- knn.cv(train = scale(reduced_clinical_7[, 2:6]), cl = reduced_clinical_7$E02_Event_DMFS_2005, k = i)
  
  
  #calculate the classifications rate 
  missclassifications.vec.1[i] <- mean(class.vec.1 != reduced_clinical_7$E02_Event_DMFS_2005)
  missclassifications.vec.2[i] <- mean(class.vec.2 != reduced_clinical_7$E02_Event_DMFS_2005)
  missclassifications.vec.3[i] <- mean(class.vec.3 != reduced_clinical_7$E02_Event_DMFS_2005)
  missclassifications.vec.4[i] <- mean(class.vec.4 != reduced_clinical_7$E02_Event_DMFS_2005)
  missclassifications.vec.5[i] <- mean(class.vec.5 != reduced_clinical_7$E02_Event_DMFS_2005)
  missclassifications.vec.6[i] <- mean(class.vec.6 != reduced_clinical_7$E02_Event_DMFS_2005)
  
}


# Plot the misclassification rates for the four models
plot(missclassifications.vec.1, ylim = range(missclassifications.vec.1, missclassifications.vec.2, missclassifications.vec.3, missclassifications.vec.4, missclassifications.vec.5, missclassifications.vec.6),
     xlab = "Nearest neighbours", ylab = "Misclassification rate",
     main = "Plot of Misclassification Rate by Number of Nearest Neighbours", type = "b", lwd = 2, col = 1)
points(missclassifications.vec.2, type = "b", lty = 2, lwd = 2, col = 2)
points(missclassifications.vec.3, type = "b", lty = 3, lwd = 2, col = 3)
points(missclassifications.vec.4, type = "b", lty = 4, lwd = 2, col = 4)
points(missclassifications.vec.5, type = "b", lty = 4, lwd = 2, col = 5)
points(missclassifications.vec.6, type = "b", lty = 4, lwd = 2, col = 3)

legend("topright", legend = c("Model 1: Columns 2 & 3", "Model 2: Columns 3 & 4", "Model 3: Columns 2 & 4", "Model 4: Columns 2, 3 & 4", "Model 4: Columns 2, 3, 4, 5", "Model 4: Columns 2, 3, 4, 5, 6"),
       title = "Candidate Model", pch = 1, lty = 1:6, col = c(1, 2, 3, 4,5,6), lwd = 2)

k <- 9  # Set this to your desired k value

# Make predictions using k-NN
predictions <- knn(train = scale(reduced_clinical_6[, -1]), 
                   test = scale(reduced_clinical_6[, -1]), 
                   cl = reduced_clinical_6$E02_Event_DMFS_2005, 
                   k = k)

# Create the confusion matrix
confusion_mat <- table(Predicted = predictions, Actual = reduced_clinical_6$E02_Event_DMFS_2005)

# Print the confusion matrix
print(confusion_mat)

# Calculate accuracy
accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
accuracy


##### Question B #####

clinical_df <- clinical_df[complete.cases(clinical_df),]
table(clinical_df$B01_Profile_70_genes, clinical_df$E02_Event_DMFS_2005)

# Cross-tabulation between ER status and survival outcome
er_table <- table(reduced_clinical$I01_ER_2007, reduced_clinical$E02_Event_DMFS_2005)
colnames(er_table) <- c("No", "Yes")
er_table_with_percent <- cbind(er_table, Survived_Percentage = round(er_table[, "No"] / rowSums(er_table) * 100, 2))
print(er_table_with_percent)
# Patients with higher ER expression (e.g., 80-100) tend to have a lower rate of distant metastasis compared to those with lower ER expression.
#This suggests that ER-positive breast cancers might be associated with a better prognosis.

# Cross-tabulation for PR status and survival outcome
pr_table <- table(reduced_clinical$I02_PR_2007, reduced_clinical$E02_Event_DMFS_2005)
colnames(pr_table) <- c("No", "Yes")
pr_table_with_percent <- cbind(pr_table, Survived_Percentage = round(pr_table[, "No"] / rowSums(pr_table) * 100, 2))
print(pr_table_with_percent)
# Similar to ER, patients with higher PR expression seem to have a lower rate of distant metastasis.
# This further reinforces the potential association between hormone receptor positivity and improved survival.

# Repeat for HER2 status and survival outcome
her2_table <- table(reduced_clinical$I03_HER2_2007, reduced_clinical$E02_Event_DMFS_2005)
colnames(her2_table) <- c("No", "Yes")
her2_table_with_percent <- cbind(her2_table, Survived_Percentage = round(her2_table[, "No"] / rowSums(her2_table) * 100, 2))
print(her2_table_with_percent)
# The data for HER2 is less clear-cut. While there might be a slight trend towards lower distant metastasis rates in HER2-negative patients, the difference is not as pronounced as with ER and PR.

# Bar plot for ER status
ggplot(clinical_df, aes(x = factor(I01_ER_2007), fill = factor(E02_Event_DMFS_2005))) +
  geom_bar(position = "dodge") +
  labs(title = "Breast Cancer Survival by ER Status", x = "ER Status (Percentage)", fill = "Distant Metastasis (0 = No, 1 = Yes)") +
  theme_minimal()

# Bar plot for PR status
ggplot(clinical_df, aes(x = factor(I02_PR_2007), fill = factor(E02_Event_DMFS_2005))) +
  geom_bar(position = "dodge") +
  labs(title = "Breast Cancer Survival by PR Status", x = "PR Status (Percentage)", fill = "Distant Metastasis (0 = No, 1 = Yes)") +
  theme_minimal()

# Bar plot for HER2 status
ggplot(clinical_df, is.na = T, aes(x = factor(I03_HER2_2007), fill = factor(E02_Event_DMFS_2005))) +
  geom_bar(position = "dodge") +
  labs(title = "Breast Cancer Survival by HER2 Status", x = "HER2 Status (0 = Negative, 1 = Positive)", fill = "Distant Metastasis (0 = No, 1 = Yes)") +
  theme_minimal()

#### KNN ####

## Find best nearest neighbor and lowest missclassification
# Scatterplot of ER vs.Pr with different colours for each group.
plot(reduced_clinical$I01_ER_2007, reduced_clinical$I02_PR_2007, xlab = "er", ylab = "pr", main = "Scatterplot of er vs. pr", pch = 19, col = as.numeric(reduced_clinical$E02_Event_DMFS_2005), cex = 1.1)
legend(x ="left", legend = c("No", "Yes"), pch = 19, col = 1 : 2)

# Set the random number generator seed
set.seed(1)

# Specify the maximum number of nearest neighbours to consider.
n <- 30

# Initialise vectors that will store misclassification rates corresponding to each of the three candidate models.
misclassifications.vec.1 <- rep(NA, n)
misclassifications.vec.2 <- rep(NA, n)
misclassifications.vec.3 <- rep(NA, n)
misclassifications.vec.4 <- rep(NA, n)

# Create a "for" loop to consider each possible number of nearest neighbours.
for(i in 1 : n)
{
  # Obtain classifications for a given observation for each of the three candidate models
  class.vec.1 <- knn.cv(train = scale(reduced_clinical[, c("I01_ER_2007", "I02_PR_2007")]), cl = reduced_clinical$E02_Event_DMFS_2005, k = i)
  class.vec.2 <- knn.cv(train = scale(reduced_clinical[, c("I01_ER_2007", "I02_PR_2007","I03_HER2_2007")]), cl = reduced_clinical$E02_Event_DMFS_2005, k = i)
  class.vec.3 <- knn.cv(train = scale(reduced_clinical[, c("I01_ER_2007")]), cl = reduced_clinical$E02_Event_DMFS_2005, k = i)
  class.vec.4 <- knn.cv(train = scale(reduced_clinical[, c("I02_PR_2007")]), cl = reduced_clinical$E02_Event_DMFS_2005, k = i)
  # Calculate the misclassification rate for each model for a given number of nearest neighbours.
  misclassifications.vec.1[i] <- mean(class.vec.1 != reduced_clinical$E02_Event_DMFS_2005)
  misclassifications.vec.2[i] <- mean(class.vec.2 != reduced_clinical$E02_Event_DMFS_2005)
  misclassifications.vec.3[i] <- mean(class.vec.3 != reduced_clinical$E02_Event_DMFS_2005)
  misclassifications.vec.4[i] <- mean(class.vec.4 != reduced_clinical$E02_Event_DMFS_2005)
}

# Plot misclassification rates by number of nearest neighbours for each of the three classification models.
plot(misclassifications.vec.1, ylim = range(misclassifications.vec.1, misclassifications.vec.2, misclassifications.vec.3, misclassifications.vec.4), xlab = "Nearest neighbours", ylab = "Misclassification rate", main = "Plot of Misclassification Rate by Number of Nearest Neighbours", type = "b", lwd = 2)
points(misclassifications.vec.2, type = "b", lty = 2, lwd = 2, col = 2)
points(misclassifications.vec.3, type = "b", lty = 2, lwd = 2, col = 3)
points(misclassifications.vec.4, type = "b", lty = 2, lwd = 2, col = 4)
legend(x ='topright', legend = c("Model 1", "Model 2", "Model 3", "Model 4"), title = "Candidate Models", pch = 1, lty = 1 : 4, col = c(1, 2, 3,4), lwd = 2)

# Find the number of nearest neighbours that minimises the misclassification rate.
order(misclassifications.vec.3)

confusion.matrix <- table(class.vec.2, reduced_clinical$E02_Event_DMFS_2005)
confusion.matrix

#hit rate
(176) /(271)

# Scatterplot of ER vs. PR
plot(reduced_clinical$I01_ER_2007, reduced_clinical$I02_PR_2007, 
     xlab = "ER", 
     ylab = "PR", 
     main = "Scatterplot of ER vs. PR", 
     pch = 19, 
     col = as.numeric(knn.cv(train = scale(reduced_clinical[, c("I01_ER_2007", "I02_PR_2007")]), 
                      cl = reduced_clinical$E02_Event_DMFS_2005, k = 4)), cex = 1.1)
legend(x = 'left', legend = c("NO", "YES"), pch = 19, col = 1 : 2)                                 






#### Cluster 2 splits ####
# Calculate distances and create cluster dendrogram.
immu_distance <- dist(reduced_clinical[, c("I01_ER_2007", "I02_PR_2007","I03_HER2_2007")])
immu_hc <- hclust(immu_distance, method = "ward.D2")

# Find classifications based on two means.
immu_2_means <- kmeans(reduced_clinical[, c("I01_ER_2007", "I02_PR_2007","I03_HER2_2007")], centers = 2)

# Visualise on dendrogram.
plot(immu_hc)
groups.2 <- cutree(immu_hc, k = 2)
rect.hclust(immu_hc, k = 2, border = 1)

# Produce confusion matrix.
table(groups.2, reduced_clinical$E02_Event_DMFS_2005)

# hitrate
(117 + 35) /(117 + 51 + 68 + 35)

#### k-means ####
wss.plot <- function(reduced_clinical, nc = ncol(reduced_clinical), seed = 1)
{
  wss <- (nrow(reduced_clinical) - 1) * sum(apply(reduced_clinical, 2, var))
  for (i in 2 : nc)
  {
    set.seed(seed)
    wss[i] <- sum(kmeans(reduced_clinical, centers = i)$withinss)
  }
  
  plot(1 : nc, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares", main = "Plot of Within Group Sum of Squares \nvs. Number of Clusters", lwd = 2)
}

wss.plot(scale(reduced_clinical[, c("I01_ER_2007", "I02_PR_2007","I03_HER2_2007")]), nc = 10) 

# Calculate k-means classifications for 2 means.
classes.2 <- kmeans(scale(reduced_clinical[, c("I01_ER_2007", "I02_PR_2007","I03_HER2_2007")]), centers = 2)

# Produce a confusion matrix based on classifications from 2 means.
table(classes.2$cluster, reduced_clinical$E02_Event_DMFS_2005)

# Hitrate
172/(151+65+34+21)

#### DT ####

set.seed(1)
pt   <- 0.6 # percentage of training set
inTrain   <- sample(1:nrow(reduced_clinical), pt * nrow(reduced_clinical))
train_set <- reduced_clinical[inTrain,]
test_set  <- reduced_clinical[-inTrain,]

## fit the tree
tree_model <- tree(E02_Event_DMFS_2005 ~ + I01_ER_2007 + I02_PR_2007 + I03_HER2_2007, data=train_set[, c("E02_Event_DMFS_2005", "I01_ER_2007", "I02_PR_2007","I03_HER2_2007")])
tree_model

summary(tree_model)
##plot the tree with labels 
plot(tree_model)
text(tree_model, pretty = 0)

# Pruning with CV
cv_tree <- cv.tree(tree_model, FUN = prune.misclass)

plot(cv_tree$size, cv_tree$dev, type = "b", xlab = "Number of Terminal Nodes (Size)", ylab = "Misclassification Deviance", main = "Cross-validation: Tree Size vs Deviance")

which.min(cv_tree$dev) # 4

# New DT model
optimal_tree <- prune.misclass(tree_model, best = 3)

# new tree
plot(optimal_tree)
text(optimal_tree, pretty = 0)

predicted <- predict(optimal_tree, test_set, type = "class")

# Confussion Metrix
conf_matrix <- table(predicted, test_set$E02_Event_DMFS_2005)
print(conf_matrix)

# Hit Rate
hit_rate <- sum(diag(conf_matrix)) / sum(conf_matrix)
hit_rate
#####################


## CV pruning
cv.clinic <- cv.tree(tree_model, FUN = prune.misclass)

plot(cv.clinic$size, cv.clinic$dev, type = 'b')
cv.clinic

prune.oj <- prune.misclass(cv.clinic, best = 14)
plot(prune.oj)
text(prune.oj, pretty =0 )

optimal_tree <- prune.misclass(tree_model, best = 4) 
plot(optimal_tree)
text(optimal_tree, pretty = 0)

## confusion matrix
prune.pred <- predict(prune.oj, type = 'class', newdata =  OJ.test)

table(prune.pred, OJ.test$Purchase)

(160+66)/length(prune.pred)

table(reduced_clinical$E02_Event_DMFS_2005)

###### Question C #####
str(breast_cancer_df)
gene_data <- breast_cancer_df[, c(3:6, 14:15, 22:121)]
colSums(is.na(gene_data))
# reduced_gene <- subset(gene_data, subset = !is.na(HAS3) & !is.na(C16orf7) & !is.na(C16orf7) * is.na(GNG12))
reduced_gene <- na.omit(gene_data)
colSums(is.na(reduced_gene))
reduced_gene$E02_Event_DMFS_2005 <- as.factor(reduced_gene$E02_Event_DMFS_2005)

str(reduced_gene)

# Set the seed for reproducibility
set.seed(1)

# Define the proportion of the training set
train_proportion <- 0.7

# Split the indices into training and test sets
n_samples <- nrow(reduced_gene)
inTrain <- sample(1:n_samples, size = train_proportion * n_samples)


# Principal Component Analysis (PCA) on gene expression data
gene_pca <- prcomp(reduced_gene[, -4], scale = TRUE)

gene_pca


# Extract the principal components for training and test sets
train_pca <- gene_pca$x[inTrain, ]
test_pca <- gene_pca$x[-inTrain, ]


# ggbiplot to visualize PCA with DMFS Event groups
library(ggbiplot)
g <- ggbiplot(gene_pca, obs.scale = 1, var.scale = 1, var.axes = FALSE, groups = reduced_gene$E02_Event_DMFS_2005, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = 'DMFS Event') + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

# PCA Özeti
summary(gene_pca)

# Variance Percentage
cumsum(gene_pca$sdev^2 / sum(gene_pca$sdev^2))


# Set the number of components to use based on the desired level of variance explained
n_components <- 24  # This captures around 89% of the variance

# Specify the maximum number of nearest neighbours to consider.
n <- 20  # Consider up to 10 nearest neighbors

# Initialise a vector that will store misclassification rates corresponding to each number of nearest neighbours
misclassifications.vec <- rep(NA, n)

# Set the random number generator seed
set.seed(1)

# Use a "for" loop to evaluate different values of k (number of nearest neighbours)
for (i in 1:n) {
  # Obtain classifications using KNN for the selected number of components
  class.vec <- knn.cv(train = gene_pca$x[, 1:n_components], cl = reduced_gene$E02_Event_DMFS_2005, k = i)
  
  # Calculate the misclassification rate for each value of k
  misclassifications.vec[i] <- mean(class.vec != reduced_gene$E02_Event_DMFS_2005)
}

# Plot the misclassification rates for different values of k
plot(1:n, misclassifications.vec, type = "b", lwd = 2, xlab = "Number of Nearest Neighbours", ylab = "Misclassification Rate", main = "KNN Misclassification Rate with Reduced Components")

# Set k to 6, which is the optimal number of neighbors found earlier
optimal_k <- 6

# Obtain final classifications using KNN for the selected number of components and k = 6
set.seed(1)  # Ensuring reproducibility
final_classifications <- knn.cv(train = gene_pca$x[, 1:n_components], cl = reduced_gene$E02_Event_DMFS_2005, k = optimal_k)

# Print out the classifications to check
confusion_matrix <- table(final_classifications, reduced_gene$E02_Event_DMFS_2005)
confusion_matrix

# Calculate the misclassification rate for the final model with k = 6
misclassification_rate <- mean(final_classifications != reduced_gene$E02_Event_DMFS_2005)
misclassification_rate

# Calculate accuracy
hit_rate <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
hit_rate

# Calculate sensitivity and specificity if the dataset is binary (0 or 1 outcomes)
if (length(unique(reduced_gene$E02_Event_DMFS_2005)) == 2) {
  true_positive <- confusion_matrix[2, 2]
  false_negative <- confusion_matrix[1, 2]
  false_positive <- confusion_matrix[2, 1]
  true_negative <- confusion_matrix[1, 1]
  
  sensitivity <- true_positive / (true_positive + false_negative)
  specificity <- true_negative / (true_negative + false_positive)
  
  print(paste("Sensitivity:", sensitivity))
  print(paste("Specificity:", specificity))
}

# Create a scatterplot using the first two principal components to visualize classification results
g <- ggbiplot(gene_pca, obs.scale = 1, var.scale = 1, var.axes = FALSE, groups = as.factor(final_classifications), ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = 'KNN Classification')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

library(caret)
set.seed(1)

# Define cross-validation method
train_control <- trainControl(method = "cv", number = 10)

# Train the KNN model using cross-validation
knn_model_cv <- train(
  gene_pca$x[, 1:n_components], reduced_gene$E02_Event_DMFS_2005,
  method = "knn",
  trControl = train_control,
  tuneGrid = data.frame(k = optimal_k)
)

# Print out cross-validation results
print(knn_model_cv)

# Assuming you have split data into train and test sets
test_classifications <- knn(train = gene_pca$x[inTrain, 1:n_components],
                            test = gene_pca$x[-inTrain, 1:n_components],
                            cl = reduced_gene$E02_Event_DMFS_2005[inTrain],
                            k = optimal_k)

# Generate confusion matrix for the test set
confusion_matrix_test <- table(test_classifications, reduced_gene$E02_Event_DMFS_2005[-inTrain])
print(confusion_matrix_test)

# Calculate accuracy for the test set
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
accuracy_test

##### LDA #####

## LDA 

# Extract the first 24 PCs from the PCA results
top_24_pcs <- gene_pca$x[, 1:24]

dim(reduced_gene)


# Combine the 24 PCs with the target variable
pca_data <- data.frame(E02_Event_DMFS_2005 = reduced_gene$E02_Event_DMFS_2005, top_24_pcs)


# Fit the LDA model using the first 24 PCs as predictors
breast_cancer_lda_24_pcs <- lda(
  E02_Event_DMFS_2005 ~ .,  
  data = pca_data
)

# Print the LDA model summary
print(breast_cancer_lda_24_pcs)

confusion_matrix_5 <- table(predict(breas_cancer_lda_5)$class, reduced_clinical$E02_Event_DMFS_2005)
confusion_matrix_5

hit_rate_5 <- sum(diag(confusion_matrix_5)) / sum(confusion_matrix_5)
hit_rate_5

# Predict the class using the LDA model
lda_predictions <- predict(breast_cancer_lda_24_pcs)$class

# Create the confusion matrix between predicted values and actual values
confusion_matrix_24_pcs <- table(lda_predictions, pca_data$E02_Event_DMFS_2005)
print(confusion_matrix_24_pcs)

# Calculate the hit rate (accuracy)
hit_rate_24_pcs <- sum(diag(confusion_matrix_24_pcs)) / sum(confusion_matrix_24_pcs)
hit_rate_24_pcs






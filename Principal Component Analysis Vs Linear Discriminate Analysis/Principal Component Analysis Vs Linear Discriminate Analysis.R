#' ---
#' title: "Assignment 3"
#' author: "Sabbir Dewan"
#' date: "`r Sys.Date()`"
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' #Question 1(a)
#' 
#' Read the dataset first 
## --------------------------------------------------------------------------------------------------------------------------------------
horse = read.csv("Horse.csv")
names(horse)

## --------------------------------------------------------------------------------------------------------------------------------------
#check the horse dataset datatypes 
str(horse)

#' # Answer Question 1(a) - 
#' 
#' The dataset consists of 98 observations and 13 variables, including 10 numerical (e.g., Age, Weight_carried, Race_speed) and 3 categorical variables (e.g., Sex, Track & Outcome). For the principal component analysis (PCA), only numerical variables were selected, as PCA requires numerical data for accurate dimensionality reduction. Categorical variables, such as Sex (Filly or Gelding) and Track (indicating race tracks 0, 1, or 2), were excluded, as they represent categorical behaviors that PCA cannot handle. Additionally, the Outcome variable, which reflects injury status and is categorical, was excluded. Remaining variables were used throughout the analysis. 
#' 
#' # Question 1(b) - 
#' 
#' Check the null value for all columns. If get any NA, will remove that. 
## --------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(horse))


#' No NA values in any column. 
#' 
#' Exclude the categorical variables from the dataset and find the co-variance between variables.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
# Exclude the sex, Track and outcome column
cov.horse.variables <- cov(horse[ , !(names(horse) %in% c("Sex", "Track","Outcome"))])
cov.horse.variables


#' All the covariances are non-zero, indicating some level of dependency between the variables. However, since covariance is sensitive to the scale of the variables, interpreting the strength of these relationships directly from the covariance values can be misleading. For example, Age ranges between 2 and 4 years, while Days_in_this_racing_prep ranges from 0 to 178 days, highlighting the different scales of the variables. 
#' 
#' 
#' Let's do more clearer understanding of the relationship between variables, it's ideal to standardize them so that they are on the same scale. I will use standardize values for Principal Component Analysis. This allows for a more interpretable measure of dependence among the variables.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
# Calculate correlation excluding  Sex, Track and Outcome column
cor.horse.variables <- cor(horse[ , !(names(horse) %in% c("Sex", "Track", "Outcome"))])
cor.horse.variables


#' 
#' Now,will run the Principal component analysis on the reduced horse dataset. I use prcomp function and there Scale =TRUE parameter will ensure the scaling of all variables. 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
horse_reduced <-  horse[, !names(horse) %in% c("Sex", "Track", "Outcome")]  
horse.pca <- prcomp(horse_reduced , scale = TRUE)
horse.pca
                  

#' This result is the square root of eigenvalues and eigenvectors, on other words the standard deviation and rotation.  
#' Eigenvalues represent the total variance captured by a PCA. On the other hand, Eigenvector values represent the contribution of each variable to the principal components which means rotating the data into new dimensions(Orthogonal) to achieve uncorrelated variables. 
#' 
#' 
#' I will plot a Scree plot to find optimal Principal component.
## --------------------------------------------------------------------------------------------------------------------------------------
plot(horse.pca$sdev^2/sum(horse.pca$sdev^2), xaxt = "n", xlab = "Principle Component", ylab = "Percentage of Total variance", main = "scre plot of Variance accounted for by principle compoent", type = "b")
axis(side = 1, at = 1:10, labels = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"))

#' Add a more reader firnedly scree plot from ggplot. 
## --------------------------------------------------------------------------------------------------------------------------------------
library(MASS) 
library(factoextra)
library(ggplot2)

fviz_eig(horse.pca, addlabels = TRUE, ylim = c(0,30))

#' 
## --------------------------------------------------------------------------------------------------------------------------------------
#Summary of principle component analysis
summary(horse.pca)

#' 
#' # Ans 1(b) - 
#' 
#' As per Elbow method from Scree plot - few principle components contribute highest in terms of explain variability. First four PCA explain 71% of variability and after this four PCA, contribution of PCA drastically getting lower which means not contributing for the variability. So, ideally choosing 4 PCA as per elbow method. 
#' 
#' # Ans 1(b) - 
#' 
#' Considering 70% of total variation, first 4 PCA is enough as we can see from the Cumulative Proportion which is 71% upto 4 PCA. 
#' 
#' # Qsn 1(b) -
#' To get the PC using 1 cut off let check the eigenvalues first. 
#' 
#' Get the eigenvalues (variance explained by each principal component)
## --------------------------------------------------------------------------------------------------------------------------------------
# Get the eigenvalues (variance explained by each principal component)
eigenvalues <- (horse.pca$sdev)^2
print(eigenvalues)

#' 
## --------------------------------------------------------------------------------------------------------------------------------------
components_above_1 <- which(eigenvalues >= 1)
components_above_1

#' #Answer 1(b) - 
#' As we consider 1 as a cut-off, we will select 4 PCA. 
#' 
#' 
#' #Question 1(c) - 
#' 
#' Produce a biplot of first 2 PCA 
## --------------------------------------------------------------------------------------------------------------------------------------
biplot(horse.pca, xlim = c(-0.5, 0.5), xlab= " 1st PCA", ylab = "2nd PCA")

#' 
## --------------------------------------------------------------------------------------------------------------------------------------
horse_reduced_2 <-  horse[, !names(horse) %in% c("Sex", "Track", "Outcome", "Days_in_this_racing_prep")]  
horse.pca_2 <- prcomp(horse_reduced_2 , scale = TRUE)
horse.pca_2

#' 
## --------------------------------------------------------------------------------------------------------------------------------------
library(ggbiplot)
# Create the biplot
g <- ggbiplot(horse.pca_2, 
              obs.scale = 1, var.scale = 1, 
               ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')

print(g)

#' 
#' 
#' Print the more reader friend biplot from ggbiplot package
## --------------------------------------------------------------------------------------------------------------------------------------
library(ggbiplot)
# Create the biplot
g <- ggbiplot(horse.pca, 
              obs.scale = 1, var.scale = 1, 
               ellipse = TRUE, circle = TRUE)

g <- g + scale_color_discrete(name = '')

g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')

print(g)

#' get the loading of PCA1 & PCA2
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
# Get the loading for first 2 PCA
loadings <- horse.pca$rotation[, 1:2]
loadings

#' PC1 = -0.377 × Age - 0.158 × Weight_carried - 0.089 × Place_finish - 0.051 × Distance_from_winner + 0.059 × Race_speed - 0.551 × Starts_this_prep - 0.527 × Days_in_this_racing_prep - 0.458 × Race_distance - 0.128 × Number_starters + 0.106 × Temp
#' 
#' PC2 = 0.124 × Age - 0.211 × Weight_carried + 0.644 × Place_finish + 0.634 × Distance_from_winner + 0.064 × Race_speed - 0.152 × Starts_this_prep - 0.127 × Days_in_this_racing_prep + 0.008 × Race_distance + 0.219 × Number_starters - 0.179 × Temp
#' 
#' #Ans 1(c)
#' 
#' The vectors for Days_in_this_racing_prep, Starts_this_prep, and Race_distance extend further from the origin in the horizontal direction (corresponding to the first principal component) compared to other variables, indicating their higher loading on the first principal component.
#' 
#' On the other hand, the vectors for Place_finish and Distance_from_winner extend further from the origin in the vertical direction (corresponding to the second principal component) than other variables, consistent with their far greater loading on the second principal component.
#' 
#' #Qsn 1(d)
#' 
#' Check the variance percentage
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
loadings_pc1 <- horse.pca_2$rotation[, 1:1]

# Square the loading for PC1
squared_loadings_pc1 <- loadings_pc1^2

# Calculate the total sum of squared loading for PC1
total_squared_pc1 <- sum(squared_loadings_pc1)

# Calculate percentage contributions
percentage_contributions_pc1 <- (squared_loadings_pc1 / total_squared_pc1) * 100

print(percentage_contributions_pc1)

#' 
#' #Ans 1(d)- 
#' 
#' The variable Starts_this_prep explains the largest proportion of variability, contributing approximately 30.4%, followed closely by Days_in_this_racing_prep at 27.8%. Age also makes a notable contribution of 14.2%. Race_distance contributes 21.0%, while Weight_carried has a moderate contribution of 2.49%. Number_starters and Temp each contribute 1.65% and 1.13%, respectively. Variables such as Place_finish and Distance_from_winner contribute more modestly, at 0.79% and 0.26%, respectively, while Race_speed contributes the least at 0.34%.
#' 
#' #Qsn2 (a) - 
#' Assumption of LDA 
#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
library(GGally)

# Subset the data for each class of Outcome, excluding 'Sex', 'Track', and 'Outcome'
horse_class_0 <- subset(horse, Outcome == "0")[, !(names(horse) %in% c('Sex', 'Track', 'Outcome'))]
horse_class_1 <- subset(horse, Outcome == "1")[, !(names(horse) %in% c('Sex', 'Track', 'Outcome'))]
horse_class_2 <- subset(horse, Outcome == "2")[, !(names(horse) %in% c('Sex', 'Track', 'Outcome'))]

# Create pair plots for each class
ggpairs(horse_class_0,  progress = FALSE) + ggtitle("Pair Plot for Outcome Class 0")

ggpairs(horse_class_1, progress = FALSE) + ggtitle("Pair Plot for Outcome Class 1")

ggpairs(horse_class_2, progress = FALSE) + ggtitle("Pair Plot for Outcome Class 2")


#' This distributions for Outcome level 0 indicate Race_speed, Race_distance, and Number_starters are appear close to normal distribution, but others like Days_in_this_racing_prep and Age show significant skewness or multimodality.Also its clear that the relationships between some variables show linear patterns, while others (like Days_in_this_racing_prep vs. Race_speed) show a less clear, nonlinear relationship.
#' 
#' This distributions for Outcome level 1 indicate that Age, Days_in_this_racing_prep and Place_finish has multimodal,Right Skewed distribution indicating a violation of the normality assumption. Heteroscedasticity is observed in relationships like Race_distance vs. Days_in_this_racing_prep, where variance increases with the values of one variable. Furthermore, strong correlations between some variables, such as Days_in_this_racing_prep and Starts_this_prep, suggest that the assumption of independence may not hold. 
#' 
#' This distributions for Outcome level 2 indicate that Age and Weight_carried  with Weight_carried showing multimodal,Right Skewed distribution. Heteroscedasticity is also present, particularly in the relationship between Starts_this_prep and Race_distance. Additionally, strong correlations, such as between Days_in_this_racing_prep and Starts_this_prep (0.994) suggest a violation of independence assumptions.
#' 
#' 
#' #Qsn 2(b) - 
#' I will not use the Days_in_this_racing_prep this variable as this variable have a high co-relation (0.94) with Starts_this_prep column. 
#' 
#' Let's apply the LDA rest of the numeric columns. 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
# Subset the data excluding 'Sex', 'Track', and 'Days_in_this_racing_prep' columns
horse_reduced_4 <- horse[ , !(names(horse) %in% c("Sex", "Track","Days_in_this_racing_prep"))]


lda_model_3 <- lda(Outcome ~ ., data = horse_reduced_4)
print(lda_model_3)

# Predict using the LDA model
lda_predictions_3 <- predict(lda_model_3)

#' 
#' let's calculate the Hit Rate 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
actual_classes <- horse_reduced_4$Outcome

predicted_classes <- lda_predictions_3$class

# Calculate hit rate
hit_rate <- mean(predicted_classes == actual_classes)

print(paste("Hit Rate:", hit_rate))

#' Now, I will check the hit rate without excluding the Days_in_this_racing_prep (highly co-related) column
## --------------------------------------------------------------------------------------------------------------------------------------
horse_reduced_3 <-  horse[, !names(horse) %in% c("Sex", "Track")]

lda_model_2 <- lda(Outcome ~ ., data = horse_reduced_3)
print(lda_model_2)

# Predict using the LDA model
lda_predictions_2 <- predict(lda_model_2)


#' 
#' Let's check the Hit Rate 
## --------------------------------------------------------------------------------------------------------------------------------------
actual_classes <- horse_reduced_3$Outcome

# Predicted Outcome values
predicted_classes_2 <- lda_predictions_2$class

# Calculate hit rate 
hit_rate_2 <- mean(predicted_classes_2 == actual_classes)

print(paste("Hit Rate:", hit_rate_2))

#' Print the confusion matrix 
## --------------------------------------------------------------------------------------------------------------------------------------
confusion.matrix <- table(lda_predictions_2$class, horse_reduced_3$Outcome)
print(confusion.matrix)

#' 
#' 
#' #Question 2(b) - Ans
#' Excluding the variable "Days_in_this_racing_prep" due to its high correlation with Starts_this_preparation variable, doesn't improved the hit rate. Consequently, both model hit rate stands at 61%.
#' 
#' #Question 2(c) - Ans
#' 
#' Variables such as age, Weight_carried, Race_speed, Number_of_race_starts, and Number_of_starters showing  almost same among all the 3 outcome levels. These variables do not significantly differ between horses Outcome Classes that experience no injuries, minor injuries, or moderate to severe injuries.
#' 
#' Horses that experience moderate to severe injuries (Outcome 2) tend to perform significantly better in races, with an average 2.53 in Place_finish and a distance from the winner is lowest 0.98 comparing to other 2 outcome. In comparison, horses experiencing no injuries (Outcome 0) finish with place finish of 3.85 and a distance of 2.10 from the winner, indicating they finish far behind. Horses with minor injuries (Outcome 1) have a place finish of 4.06 and a distance of 2.91. 
#' 
#' Average higher temperate can cause the Horses with moderate to severe injuries (Outcome 2) with 24.93. 
#' On the other hand, horses with minor injuries (Outcome 1) race in lowest temperature (23.09) compared to other two outcome groups. Horses with no injuries (Outcome 0) race on average in the middle temperature (24.06) are faced by other 2 group.
#' 
#' Horses with injuries, especially those with minor or moderate/severe injuries, participate in slightly longer races like race distances is 1246 and 1257 respectively. Horses with no injuries (Outcome 0) race shorter distances, averaging a 1196. 
#' 
#' #Question 2(d) -  
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
priors <- lda_model_2$prior
costs <- c(1, 20, 100)

# Initialize vector to store new priors
new_priors <- numeric(length(priors))
for (i in 1:length(priors)) {
  new_priors[i] <- (priors[i] * costs[i]) / sum(priors * costs)
}
names(new_priors) <- c(0, 1, 2)

new_priors



#' 
#' #Question 2(d) -i (Ans)
#' 
#' New Priors are for Outcome 0 is 3%, for Outcome 1 is 30% and for Outcome 2 is 67%. 
#' 
#' Calculate New hit rate and misclassification rate 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
# Fit LDA with the new priors
lda_model_3 <- lda(Outcome ~ ., data = horse_reduced_3, prior = new_priors)
lda_predictions_4 <- predict(lda_model_3)

# Confusion matrix to evaluate performance
confusion.matrix.2 <- table(lda_predictions_4$class, horse_reduced_3$Outcome)
print(confusion.matrix.2)


#' The hit Rate of Outcome 2 increased to (13/13) 100% comparing to previous without any cost which was (2/13) 15%. The Hit rate of Outcome 1 also increased (16/30) 53% comparing to previous (7/30) 23%. But decreased drastically in Outcome 1 as it's hit rate now 0% (0/55) comparing to previous (51/55) 92%. 
#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
# Calculate the hit rate 
correct_predictions <- sum(diag(confusion.matrix.2))
total_predictions <- sum(confusion.matrix.2)
hit_rate <- correct_predictions / total_predictions

misclassification_rate <- 1 - hit_rate

print(hit_rate)
print(misclassification_rate)


#' #Ans 2(d) -ii 
#' New Hit rate is 30% and Misclassification rate is 70%
#' 
#' #Qsn 2(e) - 
#' 
#' Plot the LDA histogram to check how well it's separated (feasibility of LDA)
## --------------------------------------------------------------------------------------------------------------------------------------
lda_scores <- lda_predictions_4$x 
#for the first LDA
ldahist(data = lda_scores[, 1], g = horse_reduced_3$Outcome)


#' Let's check the second LDA 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------
lda_scores <- lda_predictions_4$x 
#for the second LDA
ldahist(data = lda_scores[, 2], g = horse_reduced_3$Outcome)


#' Both LDA1 and LDA2 can't separate more accurately the outcome of horse injury as observations are overlapped between groups. 
#' 
#' #Qsn 2(e) - ans 
#' 
#' LDA is not effective in this context for predicting injury outcomes. The high misclassification rate, significant overlap in the LDA components, and poor separation between the injury categories suggest that LDA is not suitable for this problem.
#' 
#' 
#' #Question 3(a) -  Ans -
#' 
#' Principal Components Regression (PCR) is suitable for predictive modeling when there are  highly correlated predictor variables, which can lead to multicollinearity and inaccuracy in standard regression. PCR addresses these issues by transforming the predictors into principal components. Since each principal component is orthogonal to the previous ones, Principal Components Regression (PCR) is used to avoid errors caused by dependencies between the assumed independent variables in regression (Hadi and Ling, 1998). This orthogonality ensures that the transformed components are independent, thereby addressing multicollinearity issues.
#' 
#' Also, PCR particularly useful for high-dimensional datasets where the number of predictors is large or when predictors are strongly correlated. It is most suitable when the focus is on maximizing predictive accuracy rather than interpreting the relationships between individual predictors and the response variable.
#' 
#' #Question 3(b) - Ans 
#' 
#' Principal Component Regression (PCR) has some key assumptions. It assumes that there is a linear relationship between the principal components and the outcome. It also assumes the residuals (errors) have constant variance, are independent, and are normally distributed. To check these, we can use scatter plots to verify linearity, and residual plots for constant variance. We can use residual plots to test for independence and Q-Q plots or histograms for checking normality. 
#' 
#' PCR also needs the right number of components to be chosen, which can be done using scree plots or cross-validation. 
#' 
#' #Question 3(c) - ans
#' 
#' For Principal Component Regression (PCR), the output can be present several way.  First, explained variance would be shown using a scree plot, which helps illustrate how much each principal component captures the original data’s variability. This allows stakeholders to understand which components are most important. 
#' 
#' Additionally, model performance metrics such as PRESS, MSE or RMSE can be used to show how well/worse the model fits and how accurate it is. Residual analysis, including residual plots, can be used to verify any issues (i.e - difference from actual value, any particular group,overlap data etc). Finally, a clear interpretation summary would explain the key findings, predictors, and their impact on the outcome in plain English. 
#' 
#' 
#' #Question 4(a) - 
#' 
#' Supervised learning have both the input (predictors) and output (response) variables. The primary objective is to find a relationship between the inputs and outputs to make predictions for new data and/or understand the influence of predictors on the response variable. Regression and classification are supervised learning models.  For example, predicting an spam email based on features like subject line and content is a supervised learning.
#' 
#' In terms of KNN classification, its a supervised machine learning algorithm. It works by looking at the closest k data points (neighbors) to the new point. Mostly its find distance based on  Euclidean distance. The new point is assigned to the class that has the the most vote among other neighbors.If there is a tie, more than k neighbors might be considered. In some cases, closer neighbors are given more importance by using weight.To choose the best number of neighbors k, cross-validation methods are used. Also leave-one-out cross-validation is also used to calculate the best k. The best k is the one that give the highest accuracy or lowest error rate.
#' 
#' 
#' In Unsupervised learning, we do not have labeled data. This means we do not know the group each data point belongs to. The goal is to group the data based on how similar or close they are. Using a k-NN method for clustering, the algorithm groups points that are near each other, based on their distance, without needing labels.This is helpful in situations like market segmentation, where we want to group customers based on their behaviors (such as shopping habits), even though we don’t know their spending category ahead of time.
#' 
#' In terms of k-Means clustering, its group the data points into k clusters based on how close they are to each other. The process starts by randomly assigning data points to k clusters. The average position of points in each cluster is called the centroid.Each data point is moved to the cluster with the nearest centroid, and the centroids are updated. This continues until no points need to be moved further. The goal of k-Means is to make the points in each cluster as close to their centroid as possible (Less variance within cluster). To decide the best number of clusters k, we can use the "elbow" method, where we plot the within-cluster sum of squares (WSS) against different numbers of clusters. The best k is where the curve forms an "elbow," showing a good balance.
#' 
#' 

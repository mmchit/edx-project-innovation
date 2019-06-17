#Edx-CYO Project RScript
#Myint Moe Chit (mmchit@hotmail.com) 
# R version: 3.6.0

#Download and load required packages (if required)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(curl)) install.packages("curl", repos = "http://cran.us.r-project.org")

#Downloading data file from Github (source: https://raw.githubusercontent.com/mmchit/edx-project-innovation/master/wbes.csv)
#The full data set is available from the World Bank Enterprise Surveys - https://www.enterprisesurveys.org/

wbes <- read.csv(curl("https://raw.githubusercontent.com/mmchit/edx-project-innovation/master/wbes.csv"))

wbes %>% as_tibble() 


#Summary statistics of firms across countries and industries

wbes %>% group_by(country) %>% 
    summarise(n = n(), Innovation = mean(innovation =="Yes"), 
              Average_Size = mean(employee), Exporter = mean(exporter =="Yes"),
              ISO = mean(iso == "Yes"), Training = mean(training == "Yes"),
              Foreign_Technology = mean(foreignTech == "Yes")) %>% 
    knitr::kable(digits = c(0, 0, 2, 2, 2, 2, 2, 2))

wbes %>% group_by(industry) %>% 
    summarise(n = n(), Innovation = mean(innovation =="Yes"), 
              Average_Size = mean(employee), Exporter = mean(exporter =="Yes"),
              ISO = mean(iso == "Yes"), Training = mean(training == "Yes"),
              Foreign_Technology = mean(foreignTech == "Yes")) %>% 
    knitr::kable(digits = c(0, 0, 2, 2, 2, 2, 2, 2)) 


# Data visualisation plots

wbes %>% group_by(size) %>% summarize(Innovation = mean(innovation == "Yes")) %>% 
    ggplot(aes(size, Innovation)) + geom_bar(stat = "identity", aes(fill = size)) +
    theme(axis.text.x = element_text(hjust = 1), 
          legend.position = "none") + ggtitle ("Proportion of innovative firms across different sizes")

wbes %>% group_by(industry) %>% summarize(Innovation = mean(innovation == "Yes")) %>% 
    ggplot(aes(industry, Innovation)) + geom_bar(stat = "identity", aes(fill = industry)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "none") + ggtitle ("Proportion of innovative firms across different industries")

wbes %>% group_by(legalStatus) %>% summarize(Innovation = mean(innovation == "Yes")) %>% 
    ggplot(aes(legalStatus, Innovation)) + geom_bar(stat = "identity", aes(fill = legalStatus)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "none") + ggtitle ("Proportion of innovative firms across different Legal Status")

wbes %>% group_by(foreignTech) %>% summarize(Innovation = mean(innovation == "Yes")) %>% 
    ggplot(aes(foreignTech, Innovation)) + geom_bar(stat = "identity", aes(fill = foreignTech)) +
    theme(axis.text.x = element_text(hjust = 1), 
          legend.position = "none") + ggtitle ("Innovative Firms and the use of Foreign Technology")

wbes %>% group_by(exporter) %>% summarize(Innovation = mean(innovation == "Yes")) %>% 
    ggplot(aes(exporter, Innovation)) + geom_bar(stat = "identity", aes(fill = exporter)) +
    theme(axis.text.x = element_text(hjust = 1), 
          legend.position = "none") + ggtitle ("Innovative Firms and the Exporters")

wbes %>% group_by(iso) %>% summarize(Innovation = mean(innovation == "Yes")) %>% 
    ggplot(aes(iso, Innovation)) + geom_bar(stat = "identity", aes(fill = iso)) +
    theme(axis.text.x = element_text(hjust = 1), 
          legend.position = "none") + ggtitle ("Innovative Firms and ISO certificate")

wbes %>% group_by(training) %>% summarize(Innovation = mean(innovation == "Yes")) %>% 
    ggplot(aes(training, Innovation)) + geom_bar(stat = "identity", aes(fill = training)) +
    theme(axis.text.x = element_text(hjust = 1), 
          legend.position = "none") + ggtitle ("Innovative Firms and Training")


#Creating a training set (90%) and a test set (10%)

y <- wbes$innovation

mean(y == "Yes")

set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.1, list = FALSE)
test_set <- wbes[test_index, ]
train_set <- wbes[-test_index, ]

# Function to calculate RMSE

RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}


#Baseline prediction: Selecting firms in random

y_hat <- sample(c("Yes", "No"), length(test_index), replace = TRUE) %>%
    factor(levels = levels(test_set$innovation))

mean(y_hat == test_set$innovation)

confusionMatrix(data = y_hat, reference = test_set$innovation, positive = "Yes")

accuracy_random <- confusionMatrix(data = y_hat, reference = test_set$innovation, 
                                   positive = "Yes")$overall["Accuracy"]

rmse_random <- RMSE(as.numeric(test_set$innovation), as.numeric(y_hat))


Results <- data_frame(Method = "Random Selection", Accuracy = accuracy_random, RMSE = rmse_random)


Results %>% knitr::kable(digits = c(0, 2, 2))


#Logitic Regression 

logit_fit <- glm(innovation ~ age + country + size + legalStatus + industry + foreignTech 
                 + exporter + iso + training, data = train_set, family = "binomial")

p_h_logit <- predict(logit_fit, test_set)

y_h_logit <- factor(ifelse(p_h_logit > 0.5, "Yes", "No"))

confusionMatrix(data = y_h_logit, reference = test_set$innovation, positive = "Yes")

accuracy_logit <- confusionMatrix(data = y_h_logit, reference = test_set$innovation, 
                                   positive = "Yes")$overall["Accuracy"]

rmse_logit <- RMSE(as.numeric(test_set$innovation), as.numeric(y_h_logit))

Results1 <- data.frame(Method = "Logitic Regression", 
                                Accuracy = accuracy_logit, RMSE = rmse_logit)

Results1 %>% knitr::kable(digits = c(0, 2, 2))


#k-Nearest Neigbours

knn_fit <- knn3(innovation ~ age + country + size + legalStatus + industry + foreignTech 
                + exporter + iso + training, data = train_set, k = 13)

y_h_knn <- predict(knn_fit, test_set, type = "class")

confusionMatrix(data = y_h_knn, reference = test_set$innovation, positive = "Yes")

accuracy_knn <- confusionMatrix(data = y_h_knn, reference = test_set$innovation, 
                positive = "Yes")$overall["Accuracy"]

rmse_knn <- RMSE(as.numeric(test_set$innovation), as.numeric(y_h_knn))

Results1 <- data.frame(Method = "k-Nearest Neigbour", 
                                         Accuracy = accuracy_knn, RMSE = rmse_knn)

Results1 %>% knitr::kable(digits = c(0, 2, 2))


# Naive Bayes approach

naive_fit <- train(innovation ~ age + country + size + legalStatus + industry + foreignTech 
                   + exporter + iso + training, data = train_set, method = "naive_bayes")

y_h_naive <- predict(naive_fit, test_set)

confusionMatrix(data = y_h_naive, reference = test_set$innovation, positive = "Yes")


accuracy_naive <- confusionMatrix(data = y_h_naive, reference = test_set$innovation, 
                                  positive = "Yes")$overall["Accuracy"]

rmse_naive <- RMSE(as.numeric(test_set$innovation), as.numeric(y_h_naive))

Results1 <- data.frame(Method = "Naive Bayes", 
                                         Accuracy = accuracy_naive, RMSE = rmse_naive)

Results1 %>% knitr::kable(digits = c(0, 2, 2))


# Linear Discriminant Analysis 

lda_fit <- train(innovation ~ age + country + size + legalStatus +
                     industry +  foreignTech + exporter + iso +
                     training , data = train_set, method = "lda")

y_h_lda <- predict(lda_fit, test_set)

confusionMatrix(data = y_h_lda, reference = test_set$innovation, positive = "Yes")

accuracy_lda <- confusionMatrix(data = y_h_lda, reference = test_set$innovation, 
                                positive = "Yes")$overall["Accuracy"]

rmse_lda <- RMSE(as.numeric(test_set$innovation), as.numeric(y_h_lda))

Results1 <- data.frame(Method = "Linear Descriminant Analysis", 
                                         Accuracy = accuracy_lda, RMSE = rmse_lda)

Results1 %>% knitr::kable(digits = c(0, 2, 2))


#Classification Trees

train_rpart <- train(innovation ~ age + country + size + legalStatus + iso 
                     + industry +  foreignTech + exporter + training,
                     method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_set)

plot(train_rpart)

y_h_rpart <- predict(train_rpart, test_set)

confusionMatrix(data = y_h_rpart, reference = test_set$innovation, positive = "Yes")

accuracy_rpart <- confusionMatrix(data = y_h_rpart, reference = test_set$innovation, 
                                  positive = "Yes")$overall["Accuracy"] 

rmse_rpart <- RMSE(as.numeric(test_set$innovation), as.numeric(y_h_rpart))

Results1 <- data.frame(Method = "Classification Trees", 
                                         Accuracy = accuracy_rpart, RMSE = rmse_rpart)

Results1 %>% knitr::kable(digits = c(0, 2, 2))


#Random Forests (Rborist) Method
# This analysis takes about an hour to complete.

train_rb <- train(innovation ~ age + country + size + legalStatus + iso 
                 + industry +  foreignTech + exporter +
                     training , data = train_set, method = "Rborist")

y_h_rb <- predict(train_rb, test_set)

confusionMatrix(data = y_h_rb, reference = test_set$innovation, positive = "Yes")

accuracy_rb <- confusionMatrix(data = y_h_rb, reference = test_set$innovation, 
                                  positive = "Yes")$overall["Accuracy"] 

rmse_rb <- RMSE(as.numeric(test_set$innovation), as.numeric(y_h_rb))

Results1 <- data.frame(Method = "Random Forest", 
                                         Accuracy = accuracy_rb, RMSE = rmse_rb)

Results1 %>% knitr::kable(digits = c(0, 2, 2))


#Summary of results

Results <- bind_rows(Results, data.frame(Method = "Logitic Regression", 
                                Accuracy = accuracy_logit, RMSE = rmse_logit))

Results <- bind_rows(Results, data.frame(Method = "k-Nearest Neigbours", 
                                Accuracy = accuracy_knn, RMSE = rmse_knn))

Results <- bind_rows(Results, data.frame(Method = "Naive Bayes", 
                                         Accuracy = accuracy_naive, RMSE = rmse_naive))

Results <- bind_rows(Results, data.frame(Method = "Linear Discriminant Analysis", 
                                         Accuracy = accuracy_lda, RMSE = rmse_lda))

Results <- bind_rows(Results, data.frame(Method = "Classification Trees", 
                                         Accuracy = accuracy_rpart, RMSE = rmse_rpart))

Results <- bind_rows(Results, data.frame(Method = "Random Forest", 
                                         Accuracy = accuracy_rb, RMSE = rmse_rb))

Results %>% knitr::kable(digits = c(0, 2, 2))


#Variable Importance obtained from LDA

varImp(lda_fit, scale = FALSE) 

#The estimation is conducted using R3.6.0 

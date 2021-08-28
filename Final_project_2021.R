rm(list=ls())

#Almaza Fares 315734285
#Sol Krengel 208950568 

library(tidyverse)
library(reshape2)
library("recommenderlab")
library(data.table)


#Q1
#Loading the data
ratings <- read.csv("rating.csv")
# remove duplicates
ratings <- unique(ratings, by = c("user_id", "anime_id"))

# remove -1 ratings
ratings <- ratings[ratings$rating > 0,]

# selecting just 20k records
ratings <- ratings[1:20000,]

#naming the items
anime <- read.csv("anime.csv")
length(anime)
ItemNames=anime$name[1:length(unique(ratings$rating))]
levels(ratings$rating)=ItemNames

#Q2
#reformatting the ratings matrix, naming the users

Mat_ratings <- ratings %>%
  # Spread into user-item format
  spread(anime_id, rating) %>%
  select(-user_id) %>%
  # Convert to matrix
  as.matrix()
 

#Q3

library(recommenderlab)

#Q4

Real_mat_ratings=Mat_ratings %>%
  as("realRatingMatrix")
Real_mat_ratings

#Q5

object.size(Real_mat_ratings)#compact storage of sparse matrices
## 441808 bytes
object.size(as(Real_mat_ratings, "matrix"))
## 6711144 bytes
object.size(as(Real_mat_ratings, "matrix")) / object.size(Real_mat_ratings)
##compression ratio: 15.2 bytes

#.What is the advantage of saving a matrix as a realRatingMatrix object? The length of the answer to the question should not exceed two sentences
#`It will binarize for an object of class "realRatingMatrix" which makes computation of determinant of a matrix has been made more efficient by saving unnecessary data storage  `
#.Does this ratio match what you expected and why?
#`The ratio matched because we are expecting the matrix should gather alot less memory than sparse matrix`
#.in which matrices the advantage of a realRatingMatrix type object is not significant?
#`the sparse matrices are the matrices where advantage of a realRatingMatrix type object is not significant`

#Q6

Real_mat_ratings_filtered <- Real_mat_ratings[rowCounts(Real_mat_ratings)>100,
                                              colCounts(Real_mat_ratings) > 40 ]
Real_mat_ratings_filtered

#Q7
##for all similarity and distance methods:


Real_mat_ratings40 <- Real_mat_ratings[rowCounts(Real_mat_ratings) >40,]
Real_mat_ratings40
similarity_users <- similarity(Real_mat_ratings40, method =
                                 "Euclidean", which = "users")

Real_mat_ratings_item40 <- Real_mat_ratings[,colCounts(Real_mat_ratings) >40]
Real_mat_ratings_item40

similarity_items <- similarity(Real_mat_ratings_item40, method =
                                 "Euclidean", which = "users")

#Q8
#8.in the body of the code is stored the object recommender_models 
#which contains the different types of recommendation models. 
#It is then demonstrated how to search for these parameters and each model contains 
#all the models: IBCF and UBCF. In the same way, select two other models from the model
#list and print the parameters that the models can have.



recommender_models <- recommenderRegistry$get_entries(dataType =
                                                        "realRatingMatrix")
#display the model applicable to the realRatingMatrix
recommender_models
names(recommender_models)


###IBCF and UBCF default parameters:
recommender_models$IBCF_realRatingMatrix$parameters#IBCF parameters
recommender_models$UBCF_realRatingMatrix$parameters#IBCF parameters

############PART A##############
################################
#Q9

#Defining the training and test sets 
#random indices (in our case randomly sampling 75-25 division)
set.seed(23)
which_train <- sample(x = c(TRUE, FALSE), size = nrow(Real_mat_ratings_filtered),
                      replace = TRUE, prob = c(0.8, 0.2))

#train-test data
reco_data_train <- Real_mat_ratings_filtered[which_train, ]
reco_data_test <- Real_mat_ratings_filtered[!which_train, ]

#Q10

reco_model <- Recommender(data = reco_data_train, method = "SVD")
reco_model
#Recommender of type 'SVD' for 'realRatingMatrix' 
#learned using 42 users.

class(reco_model)

##Exploring the recommender model##
model_details <- getModel(reco_model)
model_details$description
model_details$k

#Q11

n_recommended <- 8

reco_predicted <- predict(object = reco_model, newdata = reco_data_test,
                          n = n_recommended)
reco_predicted
#Recommendations as 'topNList' with n = 8 for 19 users.
preds_ids <- as(reco_predicted, "list")


#Q12

reco_matrix <- sapply(reco_predicted@items, function(x){
  colnames(reco_data_test)[x]
})
Random_3_users <- reco_matrix[1:3,]
Random_3_users

############PART B##############
################################
#Q13
#defined for you:

percentage_training <- 0.8 

which_set <- sample(x = 1:8, size = nrow(Real_mat_ratings_filtered), replace =TRUE)
for(Fold in 1:8) {
  which_train <- which_set == Fold
  recc_data_train <- Real_mat_ratings_filtered[which_train, ]
  recc_data_test <- Real_mat_ratings_filtered[!which_train, ]
  # build the recommender
}

min(rowCounts(Real_mat_ratings_filtered)) 
items_to_keep <- 9

rating_threshold <- 3.5

n_eval <- 1

eval_sets <- evaluationScheme(data = Real_mat_ratings_filtered, 
                              method = "split",
                              train = percentage_training,
                              given = items_to_keep, 
                              goodRating = rating_threshold,
                              k = n_eval) 
eval_sets 

#Q14

n_fold <- 4
eval_sets <- evaluationScheme(data = Real_mat_ratings_filtered,
                              method = "cross-validation",
                              k = n_fold,
                              given = items_to_keep,
                              goodRating = rating_threshold)

#building model
model_1_to_evaluat <- "UBCF"
model_parameters <- NULL 
eval_recommender_1 <- Recommender(data = getData(eval_sets, "train"),
                                  method = model_1_to_evaluat,
                                  parameter = model_parameters) 


model_2_to_evaluat <- "LIBMF"
eval_recommender_2 <- Recommender(data = getData(eval_sets, "train"),
                                  method = model_2_to_evaluat,
                                  parameter = model_parameters) 



model_3_to_evaluat <- "IBCF"
eval_recommender_3 <- Recommender(data = getData(eval_sets, "train"),
                                  method = model_3_to_evaluat,
                                  parameter = model_parameters)

#Q15

items_to_recommend <- 10 

eval_prediction_1 <- predict(object = eval_recommender_1,
                             newdata = getData(eval_sets, "known"),
                             n = items_to_recommend,
                             type = "ratings")
eval_prediction_2 <- predict(object = eval_recommender_2,
                             newdata = getData(eval_sets, "known"),
                             n = items_to_recommend,
                             type = "ratings")
eval_prediction_3 <- predict(object = eval_recommender_3,
                             newdata = getData(eval_sets, "known"),
                             n = items_to_recommend,
                             type = "ratings")

#Q16


eval_accuracy1 <- calcPredictionAccuracy(x = eval_prediction_1,
                                         data = getData(eval_sets, "unknown"),
                                         byUser = FALSE)
eval_accuracy1
RMSE_1=eval_accuracy1[1]
RMSE_1

eval_accuracy2 <- calcPredictionAccuracy(x = eval_prediction_2,
                                         data = getData(eval_sets, "unknown"),
                                         byUser = FALSE)
eval_accuracy2
RMSE_2=eval_accuracy2[1]
RMSE_2

eval_accuracy3 <- calcPredictionAccuracy(x = eval_prediction_3,
                                         data = getData(eval_sets, "unknown"),
                                         byUser = FALSE)
eval_accuracy3
RMSE_3=eval_accuracy3[1]
RMSE_3


#The LIBMF gives the lowest error.

#Q17

models_to_evaluate <- list(Model_1 = list(name = "IBCF", param = list(method = "cosine")),
                           Model_2 = list(name = "IBCF", param = list(method = "pearson")),
                           Model_3 = list(name = "SVD", param = list(method = "cosine")),
                           Model_4 = list(name = "SVD", param = list(method = "pearson")),
                           Model_5 = list(name = "UBCF", param = list(method = "cosine")),
                           Model_6 = list(name = "RANDOM", param=NULL) )


#Q18

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets,
                         method = models_to_evaluate,
                         n = n_recommendations)
class(list_results) 

# average matrices
avg_matrices <- lapply(list_results, avg) 
head(avg_matrices$Model_1[, 5:8])

# plotting
plot(list_results, annotate = 1,
     legend = "topleft");title("ROC curve")


#########
plot(list_results, 
     "prec/rec", annotate = 1,
     legend = "bottomright");title("Precision-recall")



#Q19

list_results[["Model_4"]]@results

#Q20

vector_nn <- c(5, 10, 20, 40,80,100) 

models_to_evaluate <- lapply(vector_nn, function(nn){ 
  list(name = "SVD", param = list(method = "pearson", nn = nn)) }) 

names(models_to_evaluate) <- paste0("SVD_nn_", vector_nn)

list_results <- evaluate(x = eval_sets,
                         method = models_to_evaluate,
                         n = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")





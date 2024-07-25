library(C50)
library(caret)
library(ipred)
library(neuralnet)

RNGversion("3.5.2")
set.seed(500)


# STEP 1: Collect Data

colleges <- read.csv(
  "C:/path/to/my/csv/file/colleges.csv",
  stringsAsFactors = FALSE
  )

colleges$Elite <- cut(
  colleges$Top10perc, 
  breaks = c(-Inf, 50, Inf),
  labels = c("No", "Yes")
  )

# STEP 2: explore and prepare data
  # preparing data
    # I should probably normalize the data

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

colleges_n <- as.data.frame(lapply(colleges[3:19], normalize))

colleges_n$Private <- colleges$Private
colleges_n$Elite <- colleges$Elite

train_sample <- sample(nrow(colleges_n), 0.85 * nrow(colleges_n))

training_data <- colleges_n[train_sample, ]
testing_data <- colleges_n[-train_sample, ]

  # exploring data

eliteColleges_n <- colleges_n[colleges_n$Elite == "Yes", ]


# STEP 3: Train a model

  #Build C5 model

c5_model <- C5.0(
  Elite ~  Top25perc + Outstate + Expend,
  data = training_data,
  trials = 30,
  rules = FALSE
)
  #Build neural network model model
nnet_model <- neuralnet(
  Elite ~ Top25perc + Outstate + Expend,
  data = training_data,
  hidden = 1
)


# STEP 4: Evaluate Model performance

  # evaluate C5
predictions_c5 <- predict(c5_model, testing_data)

confusion_matrix_c5 <- table(testing_data$Elite, predictions_c5)
print(confusion_matrix_c5)

  # Evaluate nnet
prediction_nnet <- predict(nnet_model, testing_data)
predicted_classes <- ifelse(prediction_nnet[,1] > 0.5, "Yes", "No")
predicted_classes <- factor(predicted_classes, levels = levels(testing_data$Elite))

confusion_matrix_nnet <- table(testing_data$Elite, predicted_classes)
print(confusion_matrix_nnet)

# STEP 5: Improving Model Performance

  # improve C5
ctrl <- trainControl(method = "cv", number = 10)
c5_model <- train(Elite ~ Top25perc + Outstate + Expend, data = colleges_n,
                  method = "treebag",
                  trControl = ctrl
                  )

improved_predictions_c5 <- predict(c5_model, testing_data)

confusion_matrix <- table(testing_data$Elite, improved_predictions_c5)
print(confusion_matrix)

  # improve neurla network
nnet_model <- train(Elite ~ Top25perc + Outstate + Expend,
                    data = colleges_n,
                    method = "avNNet",
                    trControl = ctrl,
                    trace = FALSE,
                    lineout = TRUE
                    )


improved_prediction_nnet <- predict(nnet_model, testing_data)
predicted_classes <- ifelse(prediction_nnet[,1] > 0.5, "Yes", "No")
predicted_classes <- factor(predicted_classes, levels = levels(testing_data$Elite))


confusion_matrix_nnet <- table(
  testing_data$Elite,
  improved_prediction_nnet
)

print(confusion_matrix_nnet)

savehistory("finalproject.RHistory")
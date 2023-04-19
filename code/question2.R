# housekeeping ####
library(tidyverse)
library('caret')
library(mlbench)

# データ読み込み####
data <- read_csv('./df_cat.csv')
data <- data %>% mutate(Wilderness = as_factor(Wilderness_Areas),
                        Soil = as_factor(Soil_Types),
                        Cover = as.factor(Cover_Type)) %>% 
  select(-c(Soil_Types, Wilderness_Areas, Cover_Type))


# データ分割 ####
set.seed(1013)
trainIndex <- caret::createDataPartition(data$Cover, p = 0.8, list = FALSE)
training <- data[trainIndex, ]
testing <- data[-trainIndex, ]


# ハイパラチューニング ####
# モデルを定義する
model <- train(
  Cover~.,
  data = training,
  method = 'multinom',
  trControl = trainControl(method = "repeatedcv", number = 10),
  tuneLength = 10,
  metric = "Accuracy",
)

# チューニングした結果を表示する
print(model)


# テストデータ精度を確認する ####
pred <- predict(model, newdata = testing)
mean(pred == testing$Cover)

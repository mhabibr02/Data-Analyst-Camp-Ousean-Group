
# Import library
library(ggplot2)
library(caret)
library(dplyr)

# Load dataset 
library(readxl)
data <- read_excel("D:/Course/Ousean/data.xlsx")

# Split data menjadi training dan testing
set.seed(123)
trainIndex <- createDataPartition(data$stress_level, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# 1. Membuat tiga model regresi
model1 <- lm(student_id ~ stress_level + sleep_hours, data = trainData)
model2 <- lm(student_id ~ stress_level + sleep_hours + anxiety_level, data = trainData)
model3 <- lm(student_id ~ stress_level + sleep_hours + anxiety_level + mood_score, data = trainData)

# 2. Fungsi untuk menghitung R^2 dan RMSE
calculate_metrics <- function(model, testData) {
  predictions <- predict(model, testData)
  actuals <- testData$student_id
  r2 <- cor(predictions, actuals)^2
  rmse <- sqrt(mean((predictions - actuals)^2))
  return(data.frame(R2 = r2, RMSE = rmse))
}

# 3. Menghitung metrics untuk setiap model
metrics1 <- calculate_metrics(model1, testData)
metrics2 <- calculate_metrics(model2, testData)
metrics3 <- calculate_metrics(model3, testData)

metrics_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  R2 = c(metrics1$R2, metrics2$R2, metrics3$R2),
  RMSE = c(metrics1$RMSE, metrics2$RMSE, metrics3$RMSE)
)

# 4. Visualisasi perbandingan metrics
ggplot(metrics_df, aes(x = Model)) +
  geom_bar(aes(y = R2), stat = "identity", fill = "blue", alpha = 0.6) +
  geom_bar(aes(y = RMSE), stat = "identity", fill = "red", alpha = 0.6) +
  labs(title = "Perbandingan R^2 dan RMSE", y = "Nilai") +
  theme_minimal()

# 5. Menentukan model terbaik (berdasarkan R^2 terbesar dan RMSE terkecil)
best_model <- metrics_df[which.max(metrics_df$R2), "Model"]
print(paste("Model terbaik adalah:", best_model))

# 6. Visualisasi prediksi model terbaik
best_model_object <- switch(
  best_model,
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3
)

testData$Predicted <- predict(best_model_object, testData)

ggplot(testData, aes(x = mpg, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Prediksi vs Aktual", x = "Nilai Aktual", y = "Prediksi") +
  theme_minimal()

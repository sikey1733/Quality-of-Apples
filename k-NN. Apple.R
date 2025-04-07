# Загрузка библиотек
library(tidyverse)
library(readr)
library(class)
library(gmodels)

# Загрузка данных
Apple_Qu <- read_csv("apple_quality.csv")

# Очистка и преобразование
Apple_Qu <- Apple_Qu[-1] 
Apple_Qu$Quality <- factor(Apple_Qu$Quality, levels = c("bad", "good"))

Apple_Qu <- Apple_Qu %>%
  mutate(across(where(is.character), ~ gsub("[\"']", "", .))) %>%
  na.omit() %>% 
  mutate(Acidity = as.numeric(Acidity))

# Проверка баланса классов
round(prop.table(table(Apple_Qu$Quality)) * 100, digits = 1)
summary(Apple_Qu)

# Стандартизация по z-оценке
Apple_Qu_z <- as.data.frame(lapply(Apple_Qu[1:7], scale))
Apple_Qu_z$Quality <- Apple_Qu$Quality

# Разделение на обучающую и тестовую выборки
set.seed(120)
idx_Apple <- sample(1:nrow(Apple_Qu_z), size = 0.7 * nrow(Apple_Qu_z))

train <- Apple_Qu_z[idx_Apple, ]
test <- Apple_Qu_z[-idx_Apple, ]

# Извлечение признаков и меток
train_features <- train[, -which(names(train) == "Quality")]
test_features <- test[, -which(names(test) == "Quality")]
train_labels <- train$Quality
test_labels <- test$Quality

# Подбор k 
optimal_k <- floor(sqrt(nrow(train_features)))
if(optimal_k %% 2 == 0)
  optimal_k <- optimal_k - 1 
cat("Оптимальное k:", optimal_k, "\n")

# Обучение модели
Apple_test_pred <- knn(train = train_features,
                       test = test_features,
                       cl = train_labels,
                       k = optimal_k)


# Оценка эффективности модели
CrossTable(x = test_labels, y = Apple_test_pred, prop.chisq = FALSE)

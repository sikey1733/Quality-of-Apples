library(e1071)
library(gmodels)

# Обучение 
Apple_naiveBayes <- naiveBayes(Quality ~ ., data = train)

# Оценка эффективности модели
Apple_test_pred <- predict(Apple_naiveBayes, test)

CrossTable(Apple_test_pred,
           test_labels,
           prop.chisq = FALSE,
           prop.c = FALSE,
           prop.r = FALSE, 
           dnn = c("predicted_Apple", "actual_Apple"))

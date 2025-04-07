library(C50)
library(gmodels)

# Обучение
c50_Apple <- C5.0(train[-8], train$Quality, trials = 10)
summary(c50_Apple)

# Оценка эффективности модели
Apple_pred <- predict(c50_Apple, test)
CrossTable(Apple_pred,
           test$Quality,              
           prop.chisq = FALSE,
           prop.c = FALSE, 
           prop.r = FALSE,   
           dnn = c('Predicted', 'Actual')) 
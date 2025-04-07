library(pROC)
library(ggplot2)

# Вероятности для всех моделей
nb_probs <- predict(Apple_naiveBayes, test, type = "raw")[, "good"]

c50_probs <- predict(c50_Apple, test, type = "prob")[, "good"]

knn_pred <- knn(train = train_features,
                       test = test_features,
                       cl = train_labels,
                       k = optimal_k,
                       prob = TRUE)

knn_probs <- attr(knn_pred, "prob")
knn_probs <- ifelse(knn_pred == "good", knn_probs, 1 - knn_probs)

# ROC-кривые
roc_nb <- roc(test$Quality, nb_probs)
roc_c50 <- roc(test$Quality, c50_probs)
roc_knn <- roc(test$Quality, knn_probs) 

# 3. Сравнительный график
plot(roc_nb, col = "blue", main = "Сравнение ROC-кривых", lwd = 2)
plot(roc_c50, col = "red", add = TRUE, lwd = 2)
plot(roc_knn, col = "green", add = TRUE, lwd = 2) 

# Легенда
legend("bottomright",
       legend = c(paste0("Naive Bayes (AUC = ", round(auc(roc_nb), 3), ")"),
                  paste0("C5.0 (AUC = ", round(auc(roc_c50), 3), ")"),
                  paste0("KNN (AUC = ", round(auc(roc_knn), 3), ")")),
       col = c("blue", "red", "green"),
       lwd = 2)

# Сравнение AUC 
roc_test <- roc.test(roc_nb, roc_c50, roc_knn, method = "delong")
print(roc_test)
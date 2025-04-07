# Загрузка необходимых библиотек
library(shiny)
library(tidyverse)
library(class)
library(gmodels)
library(readr)
library(pROC)
library(plotly)

# UI 
ui <- fluidPage(
  titlePanel("Прогноз качества яблок с использованием k-NN"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Загрузите CSV файл", accept = ".csv"),
      numericInput("k_value", "Выберите значение k для модели", value = 5, min = 1),
      actionButton("train", "Обучить модель")
    ),
    
    mainPanel(
      h3("Результаты предсказания:"),
      hr(),
      h4("Точность модели:"),
      textOutput("model_accuracy"),
      h4("ROC Кривая:"),
      plotlyOutput("roc_curve") 
    )
  )
)

# Server
server <- function(input, output, session) {
  
  apple_data <- reactive({
    req(input$file)
    apple_data <- read_csv(input$file$datapath)
    apple_data <- apple_data[-1]  
    apple_data$Quality <- factor(apple_data$Quality, levels = c("bad", "good"))
    
    apple_data <- apple_data %>%
      mutate(across(where(is.character), ~ gsub("[\"']", "", .))) %>%
      na.omit() %>% 
      mutate(Acidity = as.numeric(Acidity))
    
    apple_data
  })
  
  # Обучение модели
  model_data <- eventReactive(input$train, {
    data <- apple_data()
    
    # Стандартизация данных
    data_z <- as.data.frame(lapply(data[1:(ncol(data)-1)], scale))
    data_z$Quality <- data$Quality
    
    # Разделение на обучающую и тестовую выборки
    set.seed(120)
    idx_Apple <- sample(1:nrow(data_z), size = 0.7 * nrow(data_z))
    train <- data_z[idx_Apple, ]
    test <- data_z[-idx_Apple, ]
    
    # Извлечение признаков и меток
    train_features <- train[, -ncol(train)]
    test_features <- test[, -ncol(test)]
    train_labels <- train$Quality
    test_labels <- test$Quality
    
    # Подбор оптимального k
    optimal_k <- input$k_value
    if(optimal_k %% 2 == 0) optimal_k <- optimal_k - 1  
    
    # Обучение модели 
    test_pred <- knn(train = train_features,
                     test = test_features,
                     cl = train_labels,
                     k = optimal_k)
    
    # Оценка эффективности модели
    cm <- CrossTable(x = test_labels, y = test_pred, prop.chisq = FALSE)
    accuracy <- cm$prop.tbl[2, 2] * 100
    
    # Рассчитываем вероятности для ROC-кривой
    prob_pred <- attr(knn(train = train_features, test = test_features, cl = train_labels, k = optimal_k, prob = TRUE), "prob")
    
    # Создаем ROC кривую
    roc_curve <- roc(test_labels, prob_pred)
    
    list(accuracy = accuracy, roc_curve = roc_curve)    
    
  })
  
  # Отображение точности модели
  output$model_accuracy <- renderText({
    req(model_data())
    paste("Точность модели: ", round(model_data()$accuracy, 2), "%")
  })
  
  output$roc_curve <- renderPlotly({
    req(model_data())
    
    # Строим график ROC-кривой
    roc_plot <- ggroc(model_data()$roc_curve) + 
      labs(title = "ROC Кривая") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(roc_plot)
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)
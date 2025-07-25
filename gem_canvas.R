# ===================================================================
# Proje: Interaktif ve Seffaf Musteri Odeme Riski Analizi
# Versiyon: 5.0
# Yeni Ozellikler: Veri Sizintisi Giderildi (Payment.Plan kaldirildi)
# ===================================================================

# -------------------------------------------------------------------
# ADIM 1: Gerekli Kutuphanelerin Yuklenmesi
# -------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(shinyjs)
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(echarts4r)
library(lime)

# -------------------------------------------------------------------
# ADIM 2: Veri Yukleme ve Model Egitimi - GELISTIRILDI
# -------------------------------------------------------------------
cat("Veri yukleniyor ve model egitiliyor... Lutfen bekleyin.\n")
tryCatch({
  sales_data <- read_excel("red30.xlsx")
}, error = function(e) {
  stop("HATA: red30.xlsx dosyasi bulunamadi. Lutfen app.R ile ayni klasorde oldugundan emin olun.")
})
names(sales_data) <- make.names(names(sales_data), unique=TRUE)

# --- Veri On Isleme (Payment.Plan sutunu kaldirildi) ---
data_ml <- sales_data %>%
  select(
    -EmpID, -Employee.Name, -Employee.Job.Title, -CustName, 
    -OrderNum, -ProdNumber, -ProdName, -Payment.Plan # <-- DEGISIKLIK BURADA
  )

data_ml$OrderDate <- as.Date(data_ml$OrderDate)
data_ml$DateCustAdded <- as.Date(data_ml$DateCustAdded)
data_ml$CustomerTenure <- as.numeric(difftime(data_ml$OrderDate, data_ml$DateCustAdded, units = "days")) / 365.25
data_ml <- data_ml %>% select(-OrderDate, -DateCustAdded)
data_ml$Payment.Status <- as.factor(ifelse(data_ml$Payment.Status %in% c("Paid", "Current"), "Odenmis", "Gecikmis"))
data_ml <- data_ml %>% mutate_if(is.character, as.factor)
preproc_values <- preProcess(data_ml, method = "knnImpute")
data_ml_processed <- predict(preproc_values, data_ml)
train_data <- data_ml_processed

# --- Model Egitimi (Yeni veri seti ile) ---
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary, sampling = "up")
set.seed(42)
rf_model <- train(Payment.Status ~ ., data = train_data, method = "rf", trControl = ctrl, metric = "ROC", preProcess = c("zv", "nzv"))

# --- TAHMIN ACKLAYICI (LIME EXPLAINER) OLUSTURMA ---
explainer <- lime(
  x = train_data[,-which(names(train_data) == "Payment.Status")],
  model = rf_model,
  bin_continuous = FALSE
)

# Arayuzden kaldirilan alanlar icin varsayilan degerler
get_mode <- function(v) {uniqv <- unique(v); uniqv[which.max(tabulate(match(v, uniqv)))]}
default_values <- list(
  Sales.Region = get_mode(train_data$Sales.Region),
  OrderType = get_mode(train_data$OrderType),
  CustomerType = get_mode(train_data$CustomerType)
  # Payment.Plan buradan da kaldirildi
)
cat("Model ve Aciklayici hazir. Uygulama baslatiliyor.\n")

# -------------------------------------------------------------------
# ADIM 3: Kullanici Arayuzu (UI)
# -------------------------------------------------------------------
# Arayuzde degisiklik yapmaya gerek yok cunku bu alan zaten kaldirilmisti.
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  useShinyjs(),
  titlePanel("Seffaf Musteri Odeme Riski Analizi (v5)"),
  sidebarLayout(
    sidebarPanel(
      h4("Sadece En Etkili Bilgileri Girin"),
      selectInput("cust_state", "Musteri Sehri:", choices = levels(train_data$CustState)),
      selectInput("prod_category", "Urun Kategorisi:", choices = levels(train_data$ProdCategory)),
      numericInput("quantity", "Miktar:", value = 1, min = 1),
      numericInput("price", "Birim Fiyat:", value = 0, min = 0),
      numericInput("discount", "Indirim:", value = 0, min = 0),
      numericInput("order_total", "Siparis Toplami:", value = 0, min = 0),
      numericInput("customer_tenure", "Musteri Gecmisi (Yil):", value = 0, min = 0),
      actionButton("analyze", "Riski Analiz Et", class = "btn-primary btn-lg", icon = icon("search"))
    ),
    mainPanel(
      h3("Analiz Sonucu"),
      echarts4rOutput("risk_gauge", width = "100%", height = "250px"),
      uiOutput("result_panel"),
      hr(),
      h4("Bu Tahmini Etkileyen En Onemli Faktorler"),
      plotOutput("explanation_plot")
    )
  )
)

# -------------------------------------------------------------------
# ADIM 4: Sunucu Tarafi (Server Logic) - GELISTIRILDI
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  shinyjs::disable("order_total")
  
  observe({
    total <- (input$quantity * input$price) - input$discount
    if (total < 0) { total <- 0 }
    updateNumericInput(session, "order_total", value = total)
  })
  
  observeEvent(input$analyze, {
    
    # Tahmin icin veri seti olusturma (Payment.Plan kaldirildi)
    new_sale_data <- data.frame(
      CustState = as.factor(input$cust_state),
      ProdCategory = as.factor(input$prod_category),
      Quantity = as.numeric(input$quantity),
      Price = as.numeric(input$price),
      Discount = as.numeric(input$discount),
      Order.Total = as.numeric(input$order_total),
      CustomerTenure = as.numeric(input$customer_tenure)
    )
    new_sale_data <- new_sale_data %>%
      mutate(
        Sales.Region = default_values$Sales.Region,
        OrderType = default_values$OrderType,
        CustomerType = default_values$CustomerType
        # Payment.Plan buradan da kaldirildi
      )
    new_sale_data <- new_sale_data[, names(train_data)[-which(names(train_data) == "Payment.Status")]]
    
    # Tahmin ve sonuc gosterimi
    tahmin_olasiliklari <- predict(rf_model, newdata = new_sale_data, type = "prob")
    risk_skoru <- tahmin_olasiliklari$Gecikmis * 100
    karar <- ifelse(risk_skoru > 50, "Yuksek Risk (Odeme Gecikebilir)", "Dusuk Risk (Odeme Bekleniyor)")
    
    output$risk_gauge <- renderEcharts4r({
      e_charts() %>%
        e_gauge(round(risk_skoru, 1), "Risk Skoru", 
                axisLine = list(lineStyle = list(color = list(c(0.5, "#5cb85c"), c(0.75, "#f0ad4e"), c(1, "#d9534f")))),
                pointer = list(show = TRUE), 
                detail = list(fontSize = 25, offsetCenter = c('0', '40%'))) %>%
        e_tooltip(formatter = "{b}: {c}%")
    })
    output$result_panel <- renderUI({
      panel_color <- ifelse(risk_skoru > 50, "panel-danger", "panel-success")
      header_text <- ifelse(risk_skoru > 50, "Yuksek Risk Tespit Edildi", "Dusuk Risk Tespit Edildi")
      tags$div(class = "panel", class = panel_color, style = "margin-top: 20px;",
               tags$div(class = "panel-heading", tags$h3(class = "panel-title", header_text)),
               tags$div(class = "panel-body", style = "font-size: 16px;",
                        tags$p(tags$strong("Tahmin:") , karar),
                        tags$p(tags$strong("Hesaplanan Risk Skoru: "), sprintf("%.2f %%", risk_skoru))
               )
      )
    })
    
    # Tahmin aciklamasi
    explanation <- lime::explain(
      x = new_sale_data,
      explainer = explainer,
      n_labels = 1,
      n_features = 5
    )
    output$explanation_plot <- renderPlot({
      plot_features(explanation) +
        labs(title = "Karar Destek Grafigi",
             subtitle = "Bu grafikteki cubuklar, her bir ozelligin skoru nasil etkiledigini gosterir.",
             x = "Etki Agirligi", y = "Ozellik")
    })
  })
}

# -------------------------------------------------------------------
# ADIM 5: Uygulamayi Calistirma
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
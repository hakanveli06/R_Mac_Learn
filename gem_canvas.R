# ===================================================================
# Proje: Interaktif Musteri Odeme Riski Analizi Web Uygulamasi
# Kutuphaneler: Shiny, readxl, caret, randomForest
# Kural: Hicbir yerde Turkce karakter kullanilmayacak.
# ===================================================================

# -------------------------------------------------------------------
# ADIM 1: Gerekli Kutuphanelerin Yuklenmesi
# -------------------------------------------------------------------
install.packages(c("shiny", "shinythemes", "readxl", "dplyr", "caret", "randomForest", "flexdashboard"))

library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(flexdashboard) # Gauge gostergesi icin

# -------------------------------------------------------------------
# ADIM 2: Veri Yukleme ve Model Egitimi (Uygulama Baslarken Sadece 1 Kere Calisir)
# -------------------------------------------------------------------
cat("Veri yukleniyor ve model egitiliyor... Lutfen bekleyin.\n")

# Excel dosyasindan veri okuma (readxl paketi ile)
tryCatch({
  sales_data <- read_excel("red30.xlsx")
}, error = function(e) {
  stop("HATA: red30.xlsx dosyasi bulunamadi. Lutfen app.R ile ayni klasorde oldugundan emin olun.")
})

# --- Veri On Isleme ---
names(sales_data) <- make.names(names(sales_data), unique=TRUE)
data_ml <- sales_data %>%
  select(-EmpID, -Employee.Name, -Employee.Job.Title, -CustName, -OrderNum, -ProdNumber, -ProdName)

data_ml$OrderDate <- as.Date(data_ml$OrderDate)
data_ml$DateCustAdded <- as.Date(data_ml$DateCustAdded)
data_ml$CustomerTenure <- as.numeric(difftime(data_ml$OrderDate, data_ml$DateCustAdded, units = "days")) / 365.25
data_ml <- data_ml %>% select(-OrderDate, -DateCustAdded)

# Hedef degiskeni olusturma (Turkce karakter olmadan)
data_ml$Payment.Status <- as.factor(ifelse(data_ml$Payment.Status %in% c("Paid", "Current"), "Odenmis", "Gecikmis"))

# Kategorik degiskenleri faktore cevirme
data_ml <- data_ml %>% mutate_if(is.character, as.factor)

# Eksik verileri doldurma
preproc_values <- preProcess(data_ml, method = "knnImpute")
data_ml_processed <- predict(preproc_values, data_ml)

# Egitim verisini hazirlama (Tum veri kullaniliyor cunku uygulama tahmin amacli)
train_data <- data_ml_processed

# --- Model Egitimi ---
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary, sampling = "up")
set.seed(42)
rf_model <- train(Payment.Status ~ .,
                  data = train_data,
                  method = "rf",
                  trControl = ctrl,
                  metric = "ROC",
                  preProcess = c("zv", "nzv"))

cat("Model hazir. Uygulama baslatiliyor.\n")

# -------------------------------------------------------------------
# ADIM 3: Kullanici Arayuzu (UI - User Interface)
# -------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Customer Payment Risk Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter New Sale Information"),
      
      # Metinsel veriler icin acilir menu (ComboBox)
      selectInput("sales_region", "Sales Region:", choices = levels(train_data$Sales.Region)),
      selectInput("order_type", "Order Type:", choices = levels(train_data$OrderType)),
      selectInput("customer_type", "Customer Type:", choices = levels(train_data$CustomerType)),
      selectInput("cust_state", "Customer State:", choices = levels(train_data$CustState)),
      selectInput("prod_category", "Product Category:", choices = levels(train_data$ProdCategory)),
      selectInput("payment_plan", "Payment Plan:", choices = levels(train_data$Payment.Plan)),
      
      # Sayisal veriler icin veri giris alanlari
      numericInput("quantity", "Quantity:", value = 1, min = 1),
      numericInput("price", "Price:", value = 0, min = 0),
      numericInput("discount", "Discount:", value = 0, min = 0),
      numericInput("order_total", "Order Total:", value = 0, min = 0),
      numericInput("customer_tenure", "Customer Tenure (Years):", value = 0, min = 0),
      
      # Analizi baslatacak buton
      actionButton("analyze", "Analyze Risk", class = "btn-primary", icon = icon("search"))
    ),
    
    mainPanel(
      h3("Analysis Result"),
      # Sonuc paneli ve gosterge icin ayrilan alanlar
      gaugeOutput("risk_gauge", width = "100%", height = "150px"),
      uiOutput("result_panel")
    )
  )
)

# -------------------------------------------------------------------
# ADIM 4: Sunucu Tarafi (Server Logic)
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Butona tiklandiginda tetiklenecek olay
  observeEvent(input$analyze, {
    
    # Arayuzden girilen verileri bir data frame icinde toplama
    new_sale_data <- data.frame(
      Sales.Region = as.factor(input$sales_region),
      OrderType = as.factor(input$order_type),
      CustomerType = as.factor(input$customer_type),
      CustState = as.factor(input$cust_state),
      ProdCategory = as.factor(input$prod_category),
      Quantity = as.numeric(input$quantity),
      Price = as.numeric(input$price),
      Discount = as.numeric(input$discount),
      Order.Total = as.numeric(input$order_total),
      Payment.Plan = as.factor(input$payment_plan),
      CustomerTenure = as.numeric(input$customer_tenure)
    )
    
    # Modeli kullanarak tahmin yapma
    tahmin_olasiliklari <- predict(rf_model, newdata = new_sale_data, type = "prob")
    risk_skoru <- tahmin_olasiliklari$Gecikmis * 100
    karar <- ifelse(risk_skoru > 50, "High Risk (Payment May Be Delayed)", "Low Risk (Payment Expected)")
    
    # Risk gostergesini (gauge) olusturma
    output$risk_gauge <- renderGauge({
      gauge(round(risk_skoru, 1), min = 0, max = 100, symbol = '%', label = "Risk Score",
            gaugeSectors(success = c(0, 49.9), warning = c(50, 74.9), danger = c(75, 100)))
    })
    
    # Dinamik sonuc panelini olusturma
    output$result_panel <- renderUI({
      
      panel_color <- ifelse(risk_skoru > 50, "panel-danger", "panel-success")
      header_text <- ifelse(risk_skoru > 50, "High Risk Detected", "Low Risk Detected")
      
      tags$div(class = "panel", class = panel_color,
               tags$div(class = "panel-heading",
                        tags$h3(class = "panel-title", header_text)
               ),
               tags$div(class = "panel-body",
                        tags$p(tags$strong("Prediction:") , karar),
                        tags$p(tags$strong("Calculated Risk Score: "), sprintf("%.2f %%", risk_skoru))
               )
      )
    })
  })
}

# -------------------------------------------------------------------
# ADIM 5: Uygulamayi Calistirma
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)


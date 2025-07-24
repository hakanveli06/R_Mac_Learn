# ===================================================================
# Proje: Interaktif Musteri Odeme Riski Analizi Web Uygulamasi
# Versiyon: 2.0
# Yeni Ozellikler: Turkce Arayuz, Otomatik Hesaplama, Gelismis Gosterge
# ===================================================================

# -------------------------------------------------------------------
# ADIM 1: Gerekli Kutuphanelerin Yuklenmesi
# -------------------------------------------------------------------

install.packages(c("shiny", "shinythemes", "readxl", "dplyr", "caret", "randomForest", "flexdashboard"))
install.packages(c("shinyjs", "echarts4r"))
library(shiny)
library(shinythemes)
library(shinyjs)      # Otomatik hesaplama icin eklendi
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(echarts4r)    # Gelismis gosterge icin eklendi

# -------------------------------------------------------------------
# ADIM 2: Veri Yukleme ve Model Egitimi (Uygulama Baslarken Sadece 1 Kere Calisir)
# -------------------------------------------------------------------
# Bu blokta bir degisiklik yok, ayni sekilde calisiyor.
cat("Veri yukleniyor ve model egitiliyor... Lutfen bekleyin.\n")
tryCatch({
  sales_data <- read_excel("red30.xlsx")
}, error = function(e) {
  stop("HATA: red30.xlsx dosyasi bulunamadi. Lutfen app.R ile ayni klasorde oldugundan emin olun.")
})
names(sales_data) <- make.names(names(sales_data), unique=TRUE)
data_ml <- sales_data %>%
  select(-EmpID, -Employee.Name, -Employee.Job.Title, -CustName, -OrderNum, -ProdNumber, -ProdName)
data_ml$OrderDate <- as.Date(data_ml$OrderDate)
data_ml$DateCustAdded <- as.Date(data_ml$DateCustAdded)
data_ml$CustomerTenure <- as.numeric(difftime(data_ml$OrderDate, data_ml$DateCustAdded, units = "days")) / 365.25
data_ml <- data_ml %>% select(-OrderDate, -DateCustAdded)
data_ml$Payment.Status <- as.factor(ifelse(data_ml$Payment.Status %in% c("Paid", "Current"), "Odenmis", "Gecikmis"))
data_ml <- data_ml %>% mutate_if(is.character, as.factor)
preproc_values <- preProcess(data_ml, method = "knnImpute")
data_ml_processed <- predict(preproc_values, data_ml)
train_data <- data_ml_processed
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary, sampling = "up")
set.seed(42)
rf_model <- train(Payment.Status ~ ., data = train_data, method = "rf", trControl = ctrl, metric = "ROC", preProcess = c("zv", "nzv"))
cat("Model hazir. Uygulama baslatiliyor.\n")

# -------------------------------------------------------------------
# ADIM 3: Kullanici Arayuzu (UI - User Interface) - GELISTIRILDI
# -------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("spacelab"), # Temayi degistirebilirsiniz: "cerulean", "superhero", "yeti" vs.
  useShinyjs(),  # shinyjs kutuphanesini aktive et
  
  titlePanel("Musteri Odeme Riski Analizi"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Yeni Satis Bilgilerini Girin"),
      
      # 1.) Menu dili Turkce'ye cevrildi (karaktersiz)
      selectInput("sales_region", "Satis Bolgesi:", choices = levels(train_data$Sales.Region)),
      selectInput("order_type", "Siparis Tipi:", choices = levels(train_data$OrderType)),
      selectInput("customer_type", "Musteri Tipi:", choices = levels(train_data$CustomerType)),
      selectInput("cust_state", "Musteri Sehri:", choices = levels(train_data$CustState)),
      selectInput("prod_category", "Urun Kategorisi:", choices = levels(train_data$ProdCategory)),
      selectInput("payment_plan", "Odeme Plani:", choices = levels(train_data$Payment.Plan)),
      
      numericInput("quantity", "Miktar:", value = 1, min = 1),
      numericInput("price", "Birim Fiyat:", value = 0, min = 0),
      numericInput("discount", "Indirim:", value = 0, min = 0),
      
      # 2.) "Order Total" alani artik otomatik hesaplanacak ve degistirilemeyecek
      numericInput("order_total", "Siparis Toplami:", value = 0, min = 0),
      
      numericInput("customer_tenure", "Musteri Gecmisi (Yil):", value = 0, min = 0),
      
      actionButton("analyze", "Riski Analiz Et", class = "btn-primary btn-lg", icon = icon("search"))
    ),
    
    mainPanel(
      h3("Analiz Sonucu"),
      # 3.) Gosterge alani yeni kutuphane icin guncellendi
      echarts4rOutput("risk_gauge", width = "100%", height = "250px"),
      uiOutput("result_panel")
    )
  )
)

# -------------------------------------------------------------------
# ADIM 4: Sunucu Tarafi (Server Logic) - GELISTIRILDI
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 2.) "Siparis Toplami" kutusunu uygulama baslarken devre disi birak
  shinyjs::disable("order_total")
  
  # 2.) Miktar, Fiyat veya Indirim degistiginde "Siparis Toplami" alanini otomatik guncelle
  observe({
    total <- (input$quantity * input$price) - input$discount
    # Negatif olmamasi icin kontrol
    if (total < 0) {
      total <- 0
    }
    updateNumericInput(session, "order_total", value = total)
  })
  
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
    karar <- ifelse(risk_skoru > 50, "Yuksek Risk (Odeme Gecikebilir)", "Dusuk Risk (Odeme Bekleniyor)")
    
    # 3.) Yeni, gosterge cubuklu risk gostergesini olusturma
    output$risk_gauge <- renderEcharts4r({
      e_charts() %>%
        e_gauge(round(risk_skoru, 1), "Risk Skoru", 
                axisLine = list(lineStyle = list(
                  color = list(c(0.5, "#5cb85c"), c(0.75, "#f0ad4e"), c(1, "#d9534f")) # Renkler: Yesil, Sari, Kirmizi
                )),
                pointer = list(show = TRUE), # Gosterge cubugunu goster
                detail = list(fontSize = 25, offsetCenter = c('0', '40%'))) %>%
        e_tooltip(formatter = "{b}: {c}%")
    })
    
    # Dinamik sonuc panelini olusturma (metinler Turkcelestirildi)
    output$result_panel <- renderUI({
      
      panel_color <- ifelse(risk_skoru > 50, "panel-danger", "panel-success")
      header_text <- ifelse(risk_skoru > 50, "Yuksek Risk Tespit Edildi", "Dusuk Risk Tespit Edildi")
      
      tags$div(class = "panel", class = panel_color, style = "margin-top: 20px;",
               tags$div(class = "panel-heading",
                        tags$h3(class = "panel-title", header_text)
               ),
               tags$div(class = "panel-body", style = "font-size: 16px;",
                        tags$p(tags$strong("Tahmin:") , karar),
                        tags$p(tags$strong("Hesaplanan Risk Skoru: "), sprintf("%.2f %%", risk_skoru))
               )
      )
    })
  })
}

# -------------------------------------------------------------------
# ADIM 5: Uygulamayi Calistirma
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
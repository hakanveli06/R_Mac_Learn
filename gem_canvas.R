# ===================================================================
# Proje: MC<Eteri Cdeme Riski Tahmini ve SkorlamasD1
# Model: Random Forest
# ===================================================================

# -------------------------------------------------------------------
# ADIM 1: Gerekli KC<tC<phanelerin YC<klenmesi
# -------------------------------------------------------------------
# Gerekirse bu satD1rD1 C'alD1EtD1rD1n:
# install.packages(c("readr", "dplyr", "lubridate", "caret", "randomForest", "ggplot2"))
library(readr)
library(dplyr)
library(lubridate)
library(caret)
library(randomForest)
library(ggplot2)
library(readxl)

# -------------------------------------------------------------------
# ADIM 2: Veri YC<kleme ve Cn D0Eleme
# -------------------------------------------------------------------
sales_data <-read_excel("red30.xlsx")
cat("Veri seti baEarD1yla yC<klendi.\n")

names(sales_data) <- make.names(names(sales_data), unique=TRUE)

data_ml <- sales_data %>%
  select(-EmpID, -Employee.Name, -Employee.Job.Title, -CustName, -OrderNum, -ProdNumber, -ProdName)

data_ml$OrderDate <- as.Date(data_ml$OrderDate)
data_ml$DateCustAdded <- as.Date(data_ml$DateCustAdded)
data_ml$CustomerTenure <- as.numeric(difftime(data_ml$OrderDate, data_ml$DateCustAdded, units = "days")) / 365.25
data_ml <- data_ml %>% select(-OrderDate, -DateCustAdded)

data_ml$Payment.Status <- as.factor(ifelse(data_ml$Payment.Status %in% c("Paid", "Current"), "Odendi", "Gecikti"))

data_ml <- data_ml %>% mutate_if(is.character, as.factor)

# Eksik verileri, hataya sebep olmayan "knnImpute" metodu ile doldurma
preproc_values <- preProcess(data_ml, method = "knnImpute")
data_ml_processed <- predict(preproc_values, data_ml)

set.seed(42)
trainIndex <- createDataPartition(data_ml_processed$Payment.Status, p = .75, list = FALSE, times = 1)
train_data <- data_ml_processed[trainIndex, ]
test_data  <- data_ml_processed[-trainIndex, ]
cat("Veri hazD1rlama tamamlandD1.\n")

# -------------------------------------------------------------------
# ADIM 3: Random Forest Modelinin EDitimi
# -------------------------------------------------------------------
cat("Random Forest modeli eDitiliyor...\n")
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary, sampling = "up")

set.seed(42)
rf_model <- train(Payment.Status ~ .,
                  data = train_data,
                  method = "rf",
                  trControl = ctrl,
                  metric = "ROC",
                  preProcess = c("zv", "nzv"))
cat("Model baEarD1yla eDitildi!\n")

# -------------------------------------------------------------------
# ADIM 4: Modelin PerformansD1nD1n DeDerlendirilmesi
# -------------------------------------------------------------------
cat("\n--- MODELD0N TEST VERD0SD0 PERFORMANSI ---\n")
rf_predictions <- predict(rf_model, newdata = test_data)
rf_cm <- confusionMatrix(rf_predictions, test_data$Payment.Status, positive = "Gecikti")
print(rf_cm)

# -------------------------------------------------------------------
# ADIM 4.5: Model KararlarD1nD1n GC6rselleEtirilmesi (Czellik Cnemi)
# -------------------------------------------------------------------
cat("\nModelin C6zellik C6nem dC<zeyi grafiDi oluEturuluyor...\n")
importance <- varImp(rf_model, scale = FALSE)
importance_df <- data.frame(Feature = row.names(importance$importance), Importance = importance$importance$Overall)
top_10_importance <- importance_df %>% arrange(desc(Importance)) %>% head(10)

# --- GCRSELLEETD0RME (DCZELTD0LMD0E KISIM) ---
# TC<rkC'e karakterler yerine standart karakterler kullanD1larak hata giderildi.
importance_plot <- ggplot(data = top_10_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#4a90e2") +
  coord_flip() +
  labs(title = "Top 10 Most Important Features", # <-- DEDD0ED0KLD0K
       x = "Features",                           # <-- DEDD0ED0KLD0K
       y = "Importance") +                        # <-- DEDD0ED0KLD0K
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12))

ggsave("ozellik_onem_duzeyi.png", plot = importance_plot, width = 10, height = 6)
print(importance_plot)
cat("Grafik baEarD1yla oluEturuldu ve 'ozellik_onem_duzeyi.png' adD1yla kaydedildi.\n")

# -------------------------------------------------------------------
# ADIM 5: Yeni SatD1E iC'in Tahmin ve Risk Skoru Fonksiyonu
# -------------------------------------------------------------------
tahmin_ve_risk_skoru <- function(Sales.Region, OrderType, CustomerType, CustState,
                                 ProdCategory, Quantity, Price, Discount,
                                 Order.Total, Payment.Plan, CustomerTenure) {
  
  yeni_satis <- data.frame(
    Sales.Region = as.factor(Sales.Region), OrderType = as.factor(OrderType),
    CustomerType = as.factor(CustomerType), CustState = as.factor(CustState),
    ProdCategory = as.factor(ProdCategory), Quantity = as.numeric(Quantity),
    Price = as.numeric(Price), Discount = as.numeric(Discount),
    Order.Total = as.numeric(Order.Total), Payment.Plan = as.factor(Payment.Plan),
    CustomerTenure = as.numeric(CustomerTenure)
  )
  
  tahmin_olasiliklari <- predict(rf_model, newdata = yeni_satis, type = "prob")
  risk_skoru <- tahmin_olasiliklari$Gecikti * 100
  karar <- ifelse(risk_skoru > 50, "Riskli (Gecikebilir)", "GC<venli (Cdenecek)")
  
  cat("\n--- Yeni Satis Risk Analizi Sonucu ---\n")
  cat(sprintf("Analiz Sonucu: %s\n", karar))
  cat(sprintf("Risk Skoru (100 uzerinden): %.2f\n", risk_skoru))
  cat("---------------------------------------\n")
  
  return(invisible(list(Karar = karar, Risk_Skoru = risk_skoru)))
}

# -------------------------------------------------------------------
# Crnek KullanD1m: Fonksiyonu test etme
# -------------------------------------------------------------------
cat("\n--- Ornek bir musteri icin risk analizi yapiliyor... ---\n")
tahmin_ve_risk_skoru(
  Sales.Region = "S Central East", OrderType = "Wholesale", CustomerType = "Business",
  CustState = "Texas", ProdCategory = "Drone Kits", Quantity = 2, Price = 125,
  Discount = 0, Order.Total = 250, Payment.Plan = "Yes", CustomerTenure = 0.1
)


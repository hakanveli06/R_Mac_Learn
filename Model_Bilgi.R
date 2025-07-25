# ===================================================================
# MODELIN IC YAPISINI GORSELLESTIREN ANALIZ KODU (v4 - Akilli Kontrol)
# Amac: Egitilmis Random Forest modelini gorsellerle anlamak.
# YENILIK: Kod, calismadan once gerekli sutunlarin varligini kontrol eder.
# ===================================================================

# --- BOLUM 1: GEREKLI KUTUPHANELERIN YUKLENMESI ---
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)

cat("==================================================================\n")
cat("MODEL GORSEL ANALIZ RAPORU\n")
cat("==================================================================\n\n")

# --- BOLUM 2: VERI HAZIRLAMA ---
cat(">>> ADIM 1: Veri yukleniyor ve kontrol ediliyor...\n")

# Veriyi Excel'den okuma
tryCatch({
  sales_data <- read_excel("red30.xlsx")
}, error = function(e) {
  stop("HATA: 'red30.xlsx' dosyasi bulunamadi. Lutfen script ile ayni klasorde oldugundan emin olun.")
})


# Sutun adlarini R'in sevecegi formata cevirme (bosluk yerine nokta vb.)
names(sales_data) <- make.names(names(sales_data), unique=TRUE)


# --- YENI AKILLI KONTROL MEKANIZMASI ---
# Modelin tahmin etmesi gereken "Payment.Status" sutunu var mi?
if (!"Payment.Status" %in% names(sales_data)) {
  cat("\n!!! ONEMLI HATA !!!\n")
  cat("Excel dosyasinda 'Payment.Status' adinda bir sutun bulunamadi.\n")
  cat("Modelin neyi tahmin edecegini bilemiyoruz. Lutfen Excel dosyanizi kontrol edin.\n\n")
  cat("Mevcut Sutun Adlari Sunlardir:\n")
  print(names(sales_data))
  stop("Islem durduruldu. Lutfen odeme durumunu belirten sutunun adini dogrulayin.")
}
# --- KONTROL SONU ---


cat(">>> Gerekli 'Payment.Status' sutunu bulundu. Islemlere devam ediliyor...\n\n")

# Veri On Isleme
data_ml <- sales_data %>%
  select(
    -CustName, -OrderNum, -ProdNumber, -ProdName
  )

data_ml$OrderDate <- as.Date(data_ml$OrderDate)
data_ml$DateCustAdded <- as.Date(data_ml$DateCustAdded)
data_ml$CustomerTenure <- as.numeric(difftime(data_ml$OrderDate, data_ml$DateCustAdded, units = "days")) / 365.25
data_ml <- data_ml %>% select(-OrderDate, -DateCustAdded)
data_ml$Payment.Status <- as.factor(ifelse(data_ml$Payment.Status %in% c("Paid", "Current"), "Odenmis", "Gecikmis"))
data_ml <- data_ml %>% mutate_if(is.character, as.factor)

preproc_values <- preProcess(data_ml, method = "knnImpute")
train_data <- predict(preproc_values, data_ml)

# --- BOLUM 3: MODELLERIN EGITILMESI ---
cat(">>> ADIM 2: Modeller egitiliyor...\n")
set.seed(42)
final_model_object <- randomForest(Payment.Status ~ ., data = train_data, ntree = 500)
set.seed(42)
tree_for_plot <- rpart(Payment.Status ~ ., data = train_data)
cat(">>> Modeller basariyla egitildi. Simdi gorsellere gecelim.\n\n")


# --- BOLUM 4 ve 5: GORSELLER (Bu kisimlarda degisiklik yok) ---
# ... (Grafik kodlari önceki versiyonla aynı olduğu için buraya eklenmedi,
#      ama tam script içinde mevcutlar)
# OZELLIK ONEM DUZEYI GRAFIGI
cat("==================================================================\n")
cat("GORSEL 1: OZELLIK ONEM DUZEYI (FEATURE IMPORTANCE)\n")
cat("==================================================================\n")
importance_scores <- importance(final_model_object)
importance_df <- as.data.frame(importance_scores)
importance_df$Feature <- row.names(importance_df)
importance_plot <- ggplot(data = importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "#4a90e2") +
  coord_flip() +
  labs(title = "Model Icin Ozellik Onem Duzeyleri", x = "Ozellikler", y = "Onem Skoru (MeanDecreaseGini)") +
  theme_minimal()
ggsave("1_ozellik_onemi_grafigi.png", plot = importance_plot, width = 10, height = 7)
print(importance_plot)
cat("-> Grafik '1_ozellik_onemi_grafigi.png' olarak kaydedildi.\n\n\n")

# OGRENME EGRISI
cat("==================================================================\n")
cat("GORSEL 2: MODELIN OGRENME EGRISI (HATA ORANI)\n")
cat("==================================================================\n")
png("2_ogrenme_egrisi_grafigi.png", width = 800, height = 600)
plot(final_model_object, main = "Agac Sayisina Gore Modelin Hata Orani")
legend("topright", colnames(final_model_object$err.rate), cex=0.8, fill=1:4)
dev.off()
plot(final_model_object, main = "Agac Sayisina Gore Modelin Hata Orani")
legend("topright", colnames(final_model_object$err.rate), cex=0.8, fill=1:4)
cat("-> Grafik '2_ogrenme_egrisi_grafigi.png' olarak kaydedildi.\n\n\n")

# KARAR AGACI SEMASI
cat("==================================================================\n")
cat("GORSEL 3: TEMSILI KARAR AGACI AKIS SEMASI\n")
cat("==================================================================\n")
png("3_karar_agaci_semasi.png", width = 1200, height = 800, res = 100)
rpart.plot(tree_for_plot, box.palette = "RdBu", shadow.col = "gray", nn = TRUE,
           main = "Temsili Karar Agaci Modeli")
dev.off()
rpart.plot(tree_for_plot, box.palette = "RdBu", shadow.col = "gray", nn = TRUE,
           main = "Temsili Karar Agaci Modeli")
cat("-> Grafik '3_karar_agaci_semasi.png' olarak kaydedildi.\n\n")

cat("\n==================================================================\n")
cat("RAPOR TAMAMLANDI.\n")
cat("==================================================================\n")

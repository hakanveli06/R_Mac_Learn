# ===================================================================
# "EN KOTU SENARYO" ANALIZ KODU (v5 - Nihai Duzeltme)
# Amac: Modelin en yuksek risk skorunu hangi durumda uretecegini bulmak.
# YENILIK: Veri yapi uyuşmazligi hatasini kesin olarak cozmek icin
#          ozel bir "align_factors" fonksiyonu eklendi.
# ===================================================================

# --- BOLUM 1: GEREKLI KUTUPHANELERIN YUKLENMESI ---
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(lubridate)
library(RANN)

cat("==================================================================\n")
cat("MODEL ICIN EN KOTU SENARYO ANALIZ RAPORU\n")
cat("==================================================================\n\n")

# --- BOLUM 2: VERI HAZIRLAMA VE MODEL EGITIMI ---
cat(">>> ADIM 1: Veri hazirlaniyor ve model egitiliyor...\n")

sales_data <- read_excel("red30.xlsx")
names(sales_data) <- make.names(names(sales_data), unique=TRUE)
data_ml <- sales_data %>%
  select(-CustName, -OrderNum, -ProdNumber, -ProdName)

data_ml$OrderDate <- as.Date(data_ml$OrderDate)
data_ml$DateCustAdded <- as.Date(data_ml$DateCustAdded)
data_ml$CustomerTenure <- as.numeric(difftime(data_ml$OrderDate, data_ml$DateCustAdded, units = "days")) / 365.25
data_ml <- data_ml %>% select(-OrderDate, -DateCustAdded)
data_ml <- data_ml %>% mutate_if(is.character, as.factor)

required_cols <- c("Payment.Status")
if (!all(required_cols %in% names(data_ml))) {
  stop("Veri setinde gerekli sutunlar (ornek: Payment.Status) bulunamadi.")
}

data_ml$Payment.Status <- as.factor(ifelse(data_ml$Payment.Status %in% c("Paid", "Current"), "Odenmis", "Gecikmis"))
preproc_values <- preProcess(data_ml, method = "knnImpute", na.remove = TRUE)
train_data <- predict(preproc_values, data_ml)

set.seed(42)
final_model_object <- randomForest(Payment.Status ~ ., data = train_data, ntree = 500, na.action = na.omit)
cat(">>> Model egitimi tamamlandi.\n\n")


# --- BOLUM 3: EN RISKLI PROFILIN PARCALARINI BULMA ---

# ----- YENI HIZALAMA FONKSIYONU -----
# Bu fonksiyon, bir veri cercevesindeki faktor seviyelerini
# baska bir sablon veri cercevesiyle esitler.
align_factors <- function(df_to_align, df_template) {
  template_factors <- names(which(sapply(df_template, is.factor)))
  for (col_name in template_factors) {
    if (col_name %in% names(df_to_align)) {
      df_to_align[[col_name]] <- factor(df_to_align[[col_name]], levels = levels(df_template[[col_name]]))
    }
  }
  return(df_to_align)
}
# ------------------------------------

cat(">>> ADIM 2: En riskli ozellikler analiz ediliyor...\n\n")

# Referans olarak kullanilacak temel profil
base_profile <- train_data[1, -which(names(train_data) == "Payment.Status")]
for (col in names(base_profile)) {
  if (is.numeric(base_profile[[col]])) {
    base_profile[[col]] <- median(train_data[[col]], na.rm = TRUE)
  } else {
    base_profile[[col]] <- levels(train_data[[col]])[1]
  }
}

# Kategorik ozellik analizi
categorical_features <- names(which(sapply(base_profile, is.factor)))
worst_categorical_values <- list()

if (length(categorical_features) > 0) {
  for (feature in categorical_features) {
    scores <- c()
    levels_to_test <- levels(train_data[[feature]])
    for (level in levels_to_test) {
      test_profile <- base_profile
      test_profile[[feature]] <- level
      # Hizalama fonksiyonunu cagir
      test_profile <- align_factors(test_profile, train_data)
      pred_prob <- predict(final_model_object, newdata = test_profile, type = "prob")
      scores[level] <- pred_prob[, "Gecikmis"]
    }
    worst_level <- names(which.max(scores))
    worst_categorical_values[[feature]] <- worst_level
    cat(sprintf("-> En Riskli '%s': %s\n", feature, worst_level))
  }
}

# Sayisal ozellik analizi
numeric_features <- names(which(sapply(base_profile, is.numeric)))
worst_numeric_values <- list()

if (length(numeric_features) > 0) {
  for(feature in numeric_features) {
    low_val <- quantile(train_data[[feature]], 0.05, na.rm = TRUE)
    test_profile_low <- base_profile
    test_profile_low[[feature]] <- low_val
    # Hizalama fonksiyonunu cagir
    test_profile_low <- align_factors(test_profile_low, train_data)
    score_low <- predict(final_model_object, newdata = test_profile_low, type = "prob")[, "Gecikmis"]
    
    high_val <- quantile(train_data[[feature]], 0.95, na.rm = TRUE)
    test_profile_high <- base_profile
    test_profile_high[[feature]] <- high_val
    # Hizalama fonksiyonunu cagir
    test_profile_high <- align_factors(test_profile_high, train_data)
    score_high <- predict(final_model_object, newdata = test_profile_high, type = "prob")[, "Gecikmis"]
    
    if (score_high > score_low) {
      worst_numeric_values[[feature]] <- high_val
      cat(sprintf("-> En Riskli '%s' Durumu: Yuksek Deger (%.2f civari)\n", feature, high_val))
    } else {
      worst_numeric_values[[feature]] <- low_val
      cat(sprintf("-> En Riskli '%s' Durumu: Dusuk Deger (%.2f civari)\n", feature, low_val))
    }
  }
}
cat("\n")

# --- BOLUM 4: "EN KOTU SENARYO" PROFILININ OLUSTURULMASI ---
cat("==================================================================\n")
cat("RAPOR: EN KOTU SENARYO PROFILI VE RISK SKORU\n")
cat("==================================================================\n")

worst_case_profile <- base_profile
for(feature in names(worst_categorical_values)) {
  worst_case_profile[[feature]] <- worst_categorical_values[[feature]]
}
for(feature in names(worst_numeric_values)) {
  worst_case_profile[[feature]] <- worst_numeric_values[[feature]]
}

# Son kez hizalama yap
worst_case_profile <- align_factors(worst_case_profile, train_data)

final_prediction <- predict(final_model_object, newdata = worst_case_profile, type = "prob")
final_risk_score <- final_prediction[, "Gecikmis"] * 100

print(worst_case_profile)
cat("\n------------------------------------------------------------------\n")
cat(sprintf(">>> BU PROFIL ICIN HESAPLANAN MAKSIMUM RISK SKORU: %.2f %% <<<\n", final_risk_score))
cat("------------------------------------------------------------------\n")

cat("\n\n==================================================================\n")
cat("ANALIZ TAMAMLANDI.\n")
cat("==================================================================\n")


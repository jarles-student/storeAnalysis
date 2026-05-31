# ==============================================================================
# PEŁNY WORKFLOW ANALITYCZNY - Pakiet storeAnalysis (Zaliczenie Poziom 1, 2 i 3)
# ==============================================================================

# 1. Załaduj swój autorski pakiet i bibliotekę do rysowania
library(storeAnalysis)
library(ggplot2)

# 2. Wczytanie prawdziwych danych z Kaggle (w tym kalendarza świąt)
message("1/9 Wczytywanie surowych danych (transakcje, sklepy, święta)...")
prawdziwe_dane <- load_sales_data(
  train_path = "train.csv",
  stores_path = "stores.csv",
  holidays_path = "holidays_events.csv"
)

# 3. Walidacja jakości danych
message("\n2/9 Walidacja danych (braki, duplikaty, luki w czasie)...")
validate_sales_ts(prawdziwe_dane)

# 4. Czyszczenie i agregacja do poziomu dziennego
message("\n3/9 Czyszczenie i agregacja danych (z uwzględnieniem dni świątecznych)...")
czyste_dane <- clean_sales_ts(prawdziwe_dane, aggregate_by = "day")

# 5. Obliczenie głównych metryk biznesowych
message("\n4/9 Generowanie głównych metryk (wolumen, zmienność, wpływ promocji)...")
metryki <- compute_sales_metrics(czyste_dane)
print(metryki)

# 6. Podsumowanie dla zarządu (Rankingi i dynamika)
message("\n5/9 Generowanie raportu biznesowego...")
podsumowanie <- create_management_summary(czyste_dane)

print("--- Liderzy wolumenu (TOP 5 Sklepów) ---")
print(podsumowanie$Top_5_Stores)

print("--- Dynamika z ostatnich 7 dni ---")
message(sprintf("Najszybciej rosnąca kategoria: %s", podsumowanie$Fastest_Growing_Category))
message(sprintf("Największy spadek (do optymalizacji): %s", podsumowanie$Biggest_Drop_Category))

# 7. Wizualizacje analityczne (Wymogi Poziom 2 i 3)
message("\n6/9 Generowanie pakietu wykresów (okno Plots)...")
print(plot_sales_trends(czyste_dane))
print(plot_category_trends(czyste_dane))
print(plot_promotion_impact(czyste_dane))
plot_top_rankings(podsumowanie)

# 8. Prognozowanie modeli predykcyjnych (Wymóg Poziom 3)
message("\n7/9 Tworzenie prognozy modelem Prophet (z uwzględnieniem świąt)...")
prognoza_prophet <- create_prognosis(czyste_dane, periods = 30, method = "prophet", zoom_days = 150)

message("\n8/9 Tworzenie prognozy modelem ARIMA...")
prognoza_arima <- create_prognosis(czyste_dane, periods = 30, method = "arima", zoom_days = 150)

# 9. Testowanie logiki wyższego rzędu (Wymóg Poziom 3)
message("\n9/9 Testowanie logiki na wyizolowanym podzbiorze (Sklep nr 44)...")
metryki_sklep_44 <- sales_ts_logic(czyste_dane, FUN = compute_sales_metrics, store_id = 44)
print("--- Metryki operacyjne dla lidera wolumenu (nr 44) ---")
print(metryki_sklep_44)

message("\n=== Workflow zakończony sukcesem! Pakiet gotowy do analizy. ===")

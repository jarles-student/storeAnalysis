# storeAnalysis

Pakiet R do analizy, wizualizacji i prognozowania szeregów czasowych w sprzedaży detalicznej. Automatyzuje proces przekształcania surowych danych w gotowe metryki biznesowe i raporty dla zarządu.

## Instalacja

```R
# install.packages("devtools")
devtools::install_github("jarles-student/storeAnalysis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(storeAnalysis)

# 1. Wczytanie, walidacja i czyszczenie danych
surowe_dane <- load_sales_data(
  train_path = "train.csv", 
  stores_path = "stores.csv", 
  holidays_path = "holidays_events.csv"
)
validate_sales_ts(surowe_dane)
czyste_dane <- clean_sales_ts(surowe_dane, aggregate_by = "day")

# 2. Obliczenie głównych metryk i generowanie podsumowania (m.in. TOP 5)
metryki <- compute_sales_metrics(czyste_dane)
podsumowanie <- create_management_summary(czyste_dane)

# 3. Wykresy i raportowanie biznesowe
plot_top_rankings(podsumowanie)
plot_sales_trends(czyste_dane)
plot_promotion_impact(czyste_dane)
plot_category_trends(czyste_dane)

# 4. Prognozowanie wolumenu (np. model Prophet na 30 dni)
prognoza <- create_prognosis(czyste_dane, periods = 30, method = "prophet")
```


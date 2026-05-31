#' Wczytywanie danych sprzedażowych
#'
#' @description Wczytuje pliki CSV i opcjonalnie łączy je (sklepy, święta) w jedną ramkę danych.
#' @param train_path Ścieżka do pliku z danymi treningowymi.
#' @param stores_path Ścieżka do pliku z metadanymi sklepów (opcjonalnie).
#' @param holidays_path Ścieżka do pliku z kalendarzem świąt (opcjonalnie).
#' @return Ramka danych (tibble).
#' @import dplyr
#' @import readr
#' @export
load_sales_data <- function(train_path, stores_path = NULL, holidays_path = NULL) {
  message("Wczytywanie danych głównych...")
  data <- readr::read_csv(train_path, show_col_types = FALSE)

  if (!is.null(stores_path)) {
    stores <- readr::read_csv(stores_path, show_col_types = FALSE)
    data <- data %>% dplyr::left_join(stores, by = "store_nbr")
  }

  # NOWE: Integracja świąt
  if (!is.null(holidays_path)) {
    holidays <- readr::read_csv(holidays_path, show_col_types = FALSE)

    # Oczyszczamy święta: odrzucamy przeniesione i bierzemy tylko unikalne daty,
    # aby uniknąć sztucznego zduplikowania wolumenu sprzedaży w głównej tabeli!
    holidays_clean <- holidays %>%
      dplyr::filter(transferred == FALSE) %>%
      dplyr::select(date) %>%
      dplyr::mutate(is_holiday = TRUE) %>%
      dplyr::distinct()

    data <- data %>%
      dplyr::left_join(holidays_clean, by = "date") %>%
      dplyr::mutate(is_holiday = ifelse(is.na(is_holiday), FALSE, is_holiday))
  }

  return(data)
}

#' Walidacja danych szeregów czasowych
#'
#' @description Sprawdza jakość danych: braki, duplikaty, poprawność dat oraz luki w szeregu.
#' @param data Ramka danych.
#' @return Wypisuje raport walidacyjny w konsoli i zwraca wejściową ramkę.
#' @export
validate_sales_ts <- function(data) {
  message("--- Raport Walidacji Danych ---")

  nas <- sum(is.na(data))
  message(sprintf("Liczba brakujących wartości (NA): %d", nas))

  dups <- nrow(data) - nrow(dplyr::distinct(data))
  message(sprintf("Liczba zduplikowanych wierszy: %d", dups))

  if("date" %in% names(data)) {
    if(!inherits(data$date, "Date")) {
      warning("Kolumna 'date' nie jest formatu Date!")
    }
    # NOWE: Sprawdzanie niespójnej częstotliwości
    dates <- sort(unique(data$date))
    date_diffs <- as.numeric(diff(dates))
    if(length(unique(date_diffs)) > 1) {
      message("Ostrzeżenie: Wykryto niespójną częstotliwość dat (luki w dniach).")
    } else {
      message("Częstotliwość dat jest spójna.")
    }
  } else {
    stop("Brak kolumny 'date' w zbiorze danych.")
  }

  if("sales" %in% names(data)) {
    neg_sales <- sum(data$sales < 0, na.rm = TRUE)
    message(sprintf("Liczba ujemnych wartości sprzedaży: %d", neg_sales))
  }

  return(invisible(data))
}

#' Czyszczenie danych sprzedażowych
#'
#' @description Usuwa braki, agreguje sprzedaż, promocje oraz flagi świąteczne.
#' @param data Ramka danych.
#' @param aggregate_by Okres agregacji: "day", "week", "month".
#' @import dplyr
#' @import lubridate
#' @export
clean_sales_ts <- function(data, aggregate_by = "day") {
  data <- data %>%
    dplyr::filter(!is.na(sales), sales >= 0) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::arrange(date)

  if (aggregate_by == "week") {
    data <- data %>% dplyr::mutate(date = lubridate::floor_date(date, "week"))
  } else if (aggregate_by == "month") {
    data <- data %>% dplyr::mutate(date = lubridate::floor_date(date, "month"))
  }

  has_promo <- "onpromotion" %in% names(data)
  has_holiday <- "is_holiday" %in% names(data)
  has_groups <- "store_nbr" %in% names(data) & "family" %in% names(data)

  if(has_groups) {
    data <- data %>% dplyr::group_by(date, store_nbr, family)
  } else {
    data <- data %>% dplyr::group_by(date)
  }

  data <- data %>%
    dplyr::summarise(
      sales = sum(sales, na.rm = TRUE),
      onpromotion = if(has_promo) sum(onpromotion, na.rm = TRUE) else NA,
      is_holiday = if(has_holiday) any(is_holiday, na.rm = TRUE) else FALSE,
      .groups = "drop"
    )

  return(data)
}

#' Obliczanie metryk biznesowych
#'
#' @description Oblicza m.in. sprzedaż całkowitą, średnią, zmienność, wpływ promocji oraz odstępy między szczytami.
#' @param data Oczyszczona ramka danych.
#' @import dplyr
#' @export
compute_sales_metrics <- function(data) {

  # Podstawowe metryki
  total_sales <- sum(data$sales, na.rm = TRUE)
  avg_sales <- mean(data$sales, na.rm = TRUE)
  var_sales <- var(data$sales, na.rm = TRUE)

  # NOWE: Udział promocji (suma promowanych jednostek)
  prom_share <- NA
  if("onpromotion" %in% names(data)) {
    prom_share <- sum(data$onpromotion, na.rm = TRUE)
  }

  # NOWE: Długość pomiędzy szczytami sprzedaży
  days_between_peaks <- NA
  if("date" %in% names(data)) {
    daily_sales <- data %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(sales = sum(sales, na.rm=TRUE), .groups="drop")

    threshold <- quantile(daily_sales$sales, 0.90, na.rm = TRUE)
    peak_dates <- daily_sales$date[daily_sales$sales > threshold]

    if(length(peak_dates) > 1) {
      days_between_peaks <- mean(as.numeric(diff(peak_dates)))
    }
  }

  metrics <- data.frame(
    Total_Sales = total_sales,
    Average_Sales = avg_sales,
    Sales_Variance = var_sales,
    Total_Promoted_Items = prom_share,
    Avg_Days_Between_Peaks = days_between_peaks
  )

  return(metrics)
}

#' Wizualizacja trendów sprzedażowych
#'
#' @description Generuje estetyczny wykres liniowy trendu wolumenu.
#' @param data Ramka danych.
#' @import ggplot2
#' @import dplyr
#' @export
plot_sales_trends <- function(data) {
  agg_data <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(sales = sum(sales, na.rm = TRUE))

  p <- ggplot(agg_data, aes(x = date, y = sales)) +
    geom_line(color = "#2c3e50", linewidth = 0.7) +
    geom_smooth(method = "loess", color = "#e74c3c", se = FALSE, linetype = "dashed") +
    theme_minimal(base_size = 12) +
    labs(
      title = "Historyczny Trend Wolumenu Sprzedaży",
      subtitle = "Zestawienie dziennego przepływu towarów z wygładzoną linią trendu (czerwona)",
      x = "Data",
      y = "Liczba sprzedanych jednostek",
      caption = "Dane zagregowane dla całej sieci"
    ) +
    theme(legend.position = "bottom")

  return(p)
}

#' Tworzenie podsumowania biznesowego
#'
#' @description Generuje raport, identyfikując ekstrema, TOP/BOTTOM sklepów oraz kategorii.
#' @param data Ramka danych.
#' @import dplyr
#' @import tidyr
#' @export
create_management_summary <- function(data) {
  summary_list <- list()

  if("store_nbr" %in% names(data)) {
    store_sales <- data %>%
      dplyr::group_by(store_nbr) %>%
      dplyr::summarise(total_volume = sum(sales, na.rm=TRUE)) %>%
      dplyr::arrange(desc(total_volume))

    summary_list$Best_Store <- store_sales$store_nbr[1]
    summary_list$Worst_Store <- store_sales$store_nbr[nrow(store_sales)]
    summary_list$Top_5_Stores <- head(store_sales, 5)
    summary_list$Bottom_5_Stores <- tail(store_sales, 5) %>% dplyr::arrange(total_volume)
  }

  # NOWE: TOP 5 i BOTTOM 5 Kategorii
  if("family" %in% names(data)) {
    cat_sales <- data %>%
      dplyr::group_by(family) %>%
      dplyr::summarise(total_volume = sum(sales, na.rm=TRUE)) %>%
      dplyr::arrange(desc(total_volume))

    summary_list$Top_5_Categories <- head(cat_sales, 5)
    summary_list$Bottom_5_Categories <- tail(cat_sales, 5) %>% dplyr::arrange(total_volume)
  }

  if("date" %in% names(data)) {
    max_date <- max(data$date, na.rm=TRUE)
    last_period <- max_date - 7

    last_period_data <- data %>% dplyr::filter(date > last_period)
    summary_list$Last_7_Days_Avg_Sales <- mean(last_period_data$sales, na.rm=TRUE)

    if("family" %in% names(data)) {
      prev_period <- last_period - 7
      cat_growth <- data %>%
        dplyr::mutate(period = dplyr::case_when(
          date > last_period ~ "current",
          date > prev_period & date <= last_period ~ "previous",
          TRUE ~ "old"
        )) %>%
        dplyr::filter(period %in% c("current", "previous")) %>%
        dplyr::group_by(family, period) %>%
        dplyr::summarise(sales = sum(sales, na.rm=TRUE), .groups="drop") %>%
        tidyr::pivot_wider(names_from = period, values_from = sales, values_fill = list(sales = 0)) %>%
        dplyr::mutate(growth = (current - previous) / (previous + 1))

      summary_list$Fastest_Growing_Category <- cat_growth$family[which.max(cat_growth$growth)]
      summary_list$Biggest_Drop_Category <- cat_growth$family[which.min(cat_growth$growth)]
    }
  }
  return(summary_list)
}

#' Funkcja wyższego rzędu do logiki biznesowej
#'
#' @description Umożliwia zaaplikowanie innej funkcji (np. plot, metrics) na wyfiltrowanym podzbiorze danych.
#' @param data Ramka danych.
#' @param FUN Funkcja do wykonania (np. compute_sales_metrics).
#' @param store_id Wybrany numer sklepu.
#' @param category Wybrana kategoria (family).
#' @import dplyr
#' @export
sales_ts_logic <- function(data, FUN, store_id = NULL, category = NULL) {
  filtered_data <- data

  if (!is.null(store_id)) {
    filtered_data <- filtered_data %>% filter(store_nbr == store_id)
  }
  if (!is.null(category)) {
    filtered_data <- filtered_data %>% filter(family == category)
  }

  # Wywołanie przekazanej funkcji
  result <- FUN(filtered_data)
  return(result)
}

#' Prognozowanie szeregów czasowych
#'
#' @description Generuje prognozy (Prophet z uwzględnieniem świąt i ARIMA).
#' @param data Oczyszczona ramka danych.
#' @param periods Liczba okresów do przodu.
#' @param method Wybór metody: "arima" lub "prophet".
#' @param zoom_days Liczba ostatnich dni do wyświetlenia na wykresie.
#' @import forecast
#' @import prophet
#' @import ggplot2
#' @import dplyr
#' @export
create_prognosis <- function(data, periods = 30, method = "prophet", zoom_days = 150) {
  ts_data <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(sales = sum(sales, na.rm = TRUE),
                     is_holiday = any(is_holiday, na.rm = TRUE)) %>%
    dplyr::arrange(date)

  if (method == "arima") {
    ts_obj <- ts(ts_data$sales, frequency = 7)
    fit <- forecast::auto.arima(ts_obj)
    prog <- forecast::forecast(fit, h = periods)

    p <- forecast::autoplot(prog, include = zoom_days) +
      theme_minimal(base_size = 12) +
      labs(title = "Prognoza Wolumenu: ARIMA", x = "Czas (Tygodnie)", y = "Wolumen") +
      theme(legend.position = "bottom")
    print(p)
    return(prog)

  } else if (method == "prophet") {
    df_prophet <- ts_data %>% dplyr::rename(ds = date, y = sales)

    # Wyciągamy same daty świąteczne dla modelu
    holidays_df <- NULL
    if ("is_holiday" %in% names(ts_data) && any(ts_data$is_holiday)) {
      holidays_df <- ts_data %>%
        dplyr::filter(is_holiday == TRUE) %>%
        dplyr::select(date) %>%
        dplyr::rename(ds = date) %>%
        dplyr::mutate(holiday = "national_holiday")
    }

    # Model uczy się teraz specyfiki dni wolnych od pracy
    m <- prophet::prophet(holidays = holidays_df, daily.seasonality = TRUE)
    m <- prophet::fit.prophet(m, df_prophet)

    future <- prophet::make_future_dataframe(m, periods = periods)
    forecast_res <- predict(m, future)

    zoom_start <- as.POSIXct(max(ts_data$date) - zoom_days)
    zoom_end <- as.POSIXct(max(ts_data$date) + periods)

    p <- plot(m, forecast_res) +
      ggplot2::coord_cartesian(xlim = c(zoom_start, zoom_end)) +
      theme_minimal(base_size = 12) +
      labs(title = "Prognoza Wolumenu: Prophet (z kalendarzem świąt)", x = "Data", y = "Wolumen")
    print(p)
    return(forecast_res)
  } else {
    stop("Nieznana metoda.")
  }
}

#' Wizualizacja rankingów biznesowych
#'
#' @description Generuje wykresy słupkowe dla TOP 5 sklepów i kategorii na podstawie podsumowania.
#' @param summary_data Lista wygenerowana przez funkcję create_management_summary().
#' @import ggplot2
#' @export
plot_top_rankings <- function(summary_data) {
  if(!is.null(summary_data$Top_5_Stores)) {
    p1 <- ggplot(summary_data$Top_5_Stores, aes(x = reorder(factor(store_nbr), total_volume), y = total_volume)) +
      geom_col(fill = "#3498db", width = 0.6) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      labs(title = "TOP 5 Sklepów", x = "Numer Sklepu", y = "Liczba sprzedanych jednostek") +
      scale_y_continuous(labels = scales::comma_format(big.mark = " ", decimal.mark = ","))
    print(p1)
  }

  if(!is.null(summary_data$Top_5_Categories)) {
    p2 <- ggplot(summary_data$Top_5_Categories, aes(x = reorder(family, total_volume), y = total_volume)) +
      geom_col(fill = "#2ecc71", width = 0.6) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      labs(title = "TOP 5 Kategorii", x = "Kategoria", y = "Liczba sprzedanych jednostek") +
      scale_y_continuous(labels = scales::comma_format(big.mark = " ", decimal.mark = ","))
    print(p2)
  }
}

#' Wizualizacja trendów dla najlepszych kategorii
#'
#' @description Generuje wykres trendów (product trends) dla 5 najpopularniejszych kategorii w czasie.
#' @param data Oczyszczona ramka danych.
#' @import ggplot2
#' @import dplyr
#' @export
plot_category_trends <- function(data) {
  if(!("family" %in% names(data))) stop("Brak informacji o kategoriach (family).")

  # Wyłapujemy 5 kategorii o największym wolumenie
  top_cats <- data %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(total = sum(sales, na.rm=TRUE)) %>%
    dplyr::arrange(desc(total)) %>%
    head(5) %>%
    dplyr::pull(family)

  # Agregujemy dane tylko dla tych kategorii
  cat_data <- data %>%
    dplyr::filter(family %in% top_cats) %>%
    dplyr::group_by(date, family) %>%
    dplyr::summarise(sales = sum(sales, na.rm=TRUE), .groups="drop")

  p <- ggplot(cat_data, aes(x = date, y = sales, color = family)) +
    geom_line(alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
    theme_minimal(base_size = 12) +
    labs(title = "Trendy Sprzedażowe: TOP 5 Produktów (Kategorii)",
         subtitle = "Porównanie przepływu wolumenu w czasie",
         x = "Data",
         y = "Liczba sprzedanych jednostek",
         color = "Kategoria") +
    theme(legend.position = "bottom")

  return(p)
}

#' Analiza skuteczności promocji
#'
#' @description Bada korelację między akcjami promocyjnymi a wygenerowanym wolumenem.
#' @param data Oczyszczona ramka danych.
#' @import ggplot2
#' @import dplyr
#' @export
plot_promotion_impact <- function(data) {
  if(!("onpromotion" %in% names(data))) stop("Brak kolumny 'onpromotion'.")

  agg_data <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(sales = sum(sales, na.rm=TRUE),
                     onpromotion = sum(onpromotion, na.rm=TRUE))

  p <- ggplot(agg_data, aes(x = onpromotion, y = sales)) +
    geom_point(alpha = 0.4, color = "#8e44ad") +
    geom_smooth(method = "lm", color = "#f39c12", linetype = "dashed", linewidth = 1) +
    theme_minimal(base_size = 12) +
    labs(title = "Skuteczność Promocji na Wolumen",
         subtitle = "Korelacja: Zależność między liczbą objętych promocją jednostek a całkowitą sprzedażą",
         x = "Liczba promowanych jednostek",
         y = "Całkowity wolumen (jednostki)")

  return(p)
}

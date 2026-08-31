#FINAL: BULGARIA DAG:EXCESS MORTALITY (WEEKLY WINTER, 2000-2025)
# + SENTINEL VALIDASI (WEEKLY WINTER, 2021-2025)
# + VACCINE COVERAGE (INTERPOLATED, TANPA NA)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(gridExtra)
library(broom)
library(zoo)

folder_path <- "~/Desktop/DATA_1"

cat("========================================\n")
cat("ANALISIS DAG LENGKAP - BULGARIA\n")
cat("EXCESS MORTALITY (2000-2025)\n")
cat("+ SENTINEL VALIDASI (2021-2025)\n")
cat("+ VACCINE COVERAGE (INTERPOLATED)\n")
cat("========================================\n\n")

# ============================================
# 1. LOAD ALL DATA
# ============================================

cat("1. LOADING DATA\n")
cat("========================================\n\n")

# 1.1 CBR Data (Original for Bulgaria)
cbr_all <- read.csv(file.path(folder_path, "birth_with_cbr_europe_2000_fixed.csv"))
bulgaria_cbr <- cbr_all %>%
  filter(country == "Bulgaria") %>%
  arrange(year)

cat("✓ CBR Bulgaria (original):", nrow(bulgaria_cbr), "rows\n")
cat("  Range:", min(bulgaria_cbr$year), "-", max(bulgaria_cbr$year), "\n")
print(bulgaria_cbr)

# 1.2 Temperature Data
load(file.path(folder_path, "temperature_europe_average.RData"))
cat("✓ Temperature data loaded\n")

# 1.3 Excess Mortality Data (Weekly)
load(file.path(folder_path, "eurostat_deaths_combined_final.RData"))
cat("✓ Excess mortality data loaded\n")

# 1.4 Sentinel Data
excel_file <- file.path(folder_path, "SENTINEL DATA.xlsx")
if(file.exists(excel_file)) {
  df_sentinel <- read_excel(excel_file, sheet = "Sheet 1", skip = 1)
  cat("✓ Sentinel data loaded:", nrow(df_sentinel), "rows\n")
} else {
  cat("⚠️ Sentinel file not found\n")
  df_sentinel <- NULL
}

# 1.5 Vaccine Coverage Data
vaccine_file <- file.path(folder_path, "VACCINE COVERAGE.xlsx")
if(file.exists(vaccine_file)) {
  df_vaccine_raw <- read_excel(vaccine_file, sheet = "Sheet 1", skip = 7, col_names = FALSE)
  cat("✓ Vaccine data loaded\n")
} else {
  cat("⚠️ Vaccine file not found\n")
  df_vaccine_raw <- NULL
}

# ============================================
# 2. PROCESS VACCINE DATA WITH INTERPOLATION - BULGARIA
# ============================================

cat("\n========================================\n")
cat("2. PROCESSING VACCINE DATA (WITH INTERPOLATION) - BULGARIA\n")
cat("========================================\n\n")

vaccine_bulgaria <- NULL
vaccine_interp <- NULL

if(!is.null(df_vaccine_raw)) {
  
  # Extract years (row 1)
  years <- as.numeric(df_vaccine_raw[1, 2:ncol(df_vaccine_raw)])
  years <- years[!is.na(years)]
  
  # Find Bulgaria row
  bulgaria_row <- NULL
  for(i in 1:nrow(df_vaccine_raw)) {
    row_vals <- as.character(df_vaccine_raw[i, ])
    if(any(grepl("Bulgaria", row_vals, ignore.case = TRUE))) {
      bulgaria_row <- i
      break
    }
  }
  
  if(!is.null(bulgaria_row)) {
    coverage_raw <- df_vaccine_raw[bulgaria_row, 2:ncol(df_vaccine_raw)]
    coverage <- as.numeric(coverage_raw[1, ])
    
    vaccine_original <- data.frame(
      year = years[1:length(coverage)],
      coverage = coverage
    ) %>%
      filter(!is.na(year) & !is.na(coverage)) %>%
      arrange(year)
    
    cat("✓ Bulgaria vaccine coverage (original):\n")
    print(vaccine_original)
    
    # ============================================
    # INTERPOLASI: ISI TAHUN YANG HILANG
    # ============================================
    
    # Buat data frame lengkap 2000-2025
    vaccine_complete <- data.frame(year = 2000:2025) %>%
      left_join(vaccine_original, by = "year")
    
    # Interpolasi dengan zoo::na.approx
    vaccine_complete <- vaccine_complete %>%
      mutate(
        coverage_interp = na.approx(coverage, rule = 2, na.rm = FALSE)
      )
    
    # Untuk tahun sebelum data pertama, gunakan nilai pertama
    first_value <- vaccine_original$coverage[1]
    vaccine_complete <- vaccine_complete %>%
      mutate(
        coverage_final = ifelse(is.na(coverage_interp), first_value, coverage_interp)
      )
    
    vaccine_interp <- vaccine_complete %>%
      dplyr::select(year, coverage = coverage_final) %>%
      filter(!is.na(coverage))
    
    cat("\n✓ Bulgaria vaccine coverage (interpolated):\n")
    print(vaccine_interp)
    
    # Tampilkan tahun yang diinterpolasi
    interpolated_years <- vaccine_interp %>%
      left_join(vaccine_original, by = "year") %>%
      filter(is.na(coverage.y)) %>%
      dplyr::select(year, coverage = coverage.x)
    
    if(nrow(interpolated_years) > 0) {
      cat("\n  Years interpolated:\n")
      print(interpolated_years)
    }
    
    # Simpan data interpolasi
    write.csv(vaccine_interp, 
              file.path(folder_path, "vaccine_bulgaria_interpolated.csv"),
              row.names = FALSE)
    cat("\n✅ Interpolated vaccine data saved to: vaccine_bulgaria_interpolated.csv\n")
  } else {
    cat("⚠️ Bulgaria not found in vaccine data\n")
  }
}

# ============================================
# 3. PROCESS EXCESS MORTALITY (WEEKLY WINTER) - BULGARIA
# ============================================

cat("\n========================================\n")
cat("3. PROCESSING EXCESS MORTALITY (WEEKLY WINTER) - BULGARIA\n")
cat("========================================\n\n")

# Filter Bulgaria
bulgaria_deaths <- all_data_final %>%
  mutate(
    year = as.numeric(substr(Week, 1, 4)),
    week = as.numeric(gsub(".*-W", "", Week)),
    country = `GEO (Labels)`
  ) %>%
  filter(country == "Bulgaria" & year >= 2000 & year <= 2025)

cat("✓ Bulgaria deaths:", nrow(bulgaria_deaths), "rows\n")

# Define Winter (weeks 40-52 and 1-9)
bulgaria_deaths <- bulgaria_deaths %>%
  mutate(
    is_winter = ifelse(week >= 40 | week <= 9, TRUE, FALSE)
  )

cat("  Winter weeks:", sum(bulgaria_deaths$is_winter, na.rm = TRUE), "\n")

# Calculate baseline mortality for Winter
baseline_winter <- bulgaria_deaths %>%
  filter(is_winter & !is.na(week)) %>%
  group_by(week) %>%
  summarise(
    baseline_deaths = mean(Deaths, na.rm = TRUE),
    sd_deaths = sd(Deaths, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate excess mortality
excess_winter <- bulgaria_deaths %>%
  filter(is_winter & !is.na(week)) %>%
  left_join(baseline_winter, by = "week") %>%
  mutate(
    excess_mortality = Deaths - baseline_deaths,
    flu_proxy = pmax(excess_mortality / baseline_deaths, 0),
    is_epidemic = ifelse(excess_mortality > (baseline_deaths + 2 * sd_deaths), 1, 0)
  )

cat("\n✓ Excess mortality (Winter):", nrow(excess_winter), "weeks\n")
cat("  Years:", min(excess_winter$year), "-", max(excess_winter$year), "\n")

# Get winter weeks
winter_weeks <- excess_winter %>%
  dplyr::select(year, week) %>%
  distinct() %>%
  arrange(year, week)

cat("  Winter weeks unique:", nrow(winter_weeks), "\n")

# ============================================
# 4. PROCESS SENTINEL (WEEKLY WINTER, 2021-2025) - BULGARIA
# ============================================

cat("\n========================================\n")
cat("4. PROCESSING SENTINEL (WEEKLY WINTER) - BULGARIA\n")
cat("========================================\n\n")

sentinel_weekly <- data.frame()

if(!is.null(df_sentinel)) {
  
  bulgaria_sentinel <- df_sentinel %>%
    filter(countryname == "Bulgaria" & 
             pathogen == "Influenza" & 
             pathogentype == "Influenza") %>%
    mutate(
      year = as.numeric(substr(yearweek, 1, 4)),
      week = as.numeric(gsub(".*-W", "", yearweek))
    ) %>%
    filter(year >= 2021 & year <= 2025)
  
  cat("✓ Bulgaria sentinel:", nrow(bulgaria_sentinel), "rows\n")
  
  # Define Winter weeks
  bulgaria_sentinel <- bulgaria_sentinel %>%
    mutate(
      is_winter = ifelse(week >= 40 | week <= 9, TRUE, FALSE)
    ) %>%
    filter(is_winter & !is.na(week))
  
  cat("  Winter weeks:", nrow(bulgaria_sentinel), "\n")
  
  if(nrow(bulgaria_sentinel) > 0) {
    sentinel_wide <- bulgaria_sentinel %>%
      dplyr::select(year, week, indicator, value) %>%
      pivot_wider(
        names_from = indicator,
        values_from = value,
        values_fn = sum
      )
    
    sentinel_weekly <- sentinel_wide %>%
      group_by(year, week) %>%
      summarise(
        detections = sum(detections, na.rm = TRUE),
        positivity = mean(positivity, na.rm = TRUE),
        tests = sum(tests, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(year, week)
    
    cat("\n✓ Sentinel weekly (winter):", nrow(sentinel_weekly), "weeks\n")
    print(head(sentinel_weekly, 10))
  }
}

# ============================================
# 5. CREATE VACCINE WEEKLY DATA (WITH INTERPOLATION) - BULGARIA
# ============================================

cat("\n========================================\n")
cat("5. CREATING VACCINE WEEKLY DATA (INTERPOLATED) - BULGARIA\n")
cat("========================================\n\n")

if(!is.null(vaccine_interp) && nrow(vaccine_interp) > 0) {
  
  vaccine_weekly <- winter_weeks %>%
    left_join(vaccine_interp %>% dplyr::select(year, coverage), by = "year")
  
  cat("✓ Vaccine data merged to weekly:\n")
  cat("  - Total weeks:", nrow(vaccine_weekly), "\n")
  cat("  - Weeks with coverage:", sum(!is.na(vaccine_weekly$coverage)), "\n")
  cat("  - Weeks WITHOUT coverage (NA):", sum(is.na(vaccine_weekly$coverage)), "\n")
  
  cat("\n  Preview:\n")
  print(head(vaccine_weekly, 20))
  
} else {
  vaccine_weekly <- winter_weeks %>%
    mutate(coverage = NA)
  cat("⚠️ No vaccine data available\n")
}

# ============================================
# 6. MERGE ALL DATA - BULGARIA
# ============================================

cat("\n========================================\n")
cat("6. MERGING ALL DATA - BULGARIA\n")
cat("========================================\n\n")

# Temperature weekly
bulgaria_temp_weekly <- temp_avg_all %>%
  mutate(
    year = year(date),
    week = week(date)
  ) %>%
  filter(year >= 2000 & year <= 2025) %>%
  group_by(year, week) %>%
  summarise(
    temp_weekly = mean(temperature, na.rm = TRUE),
    .groups = 'drop'
  )

# --- PART 1: Full Dataset (Excess Mortality + Vaccine) ---
full_data <- excess_winter %>%
  left_join(bulgaria_temp_weekly, by = c("year", "week")) %>%
  left_join(bulgaria_cbr %>% dplyr::select(year, cbr), by = "year") %>%
  left_join(vaccine_weekly %>% dplyr::select(year, week, coverage), by = c("year", "week")) %>%
  filter(!is.na(cbr) & !is.na(flu_proxy) & !is.na(temp_weekly)) %>%
  arrange(year, week) %>%
  mutate(
    trend = row_number(),
    flu_proxy_lag1 = lag(flu_proxy, 1),
    flu_proxy_lag2 = lag(flu_proxy, 2),
    flu_proxy_lag3 = lag(flu_proxy, 3),
    flu_proxy_lag4 = lag(flu_proxy, 4),
    flu_proxy_ma4 = rollmean(flu_proxy, k = 4, fill = NA, align = "right"),
    coverage_lag1 = lag(coverage, 1)
  ) %>%
  filter(!is.na(flu_proxy_lag1))

cat("✓ Full dataset (Excess Mortality + Vaccine):\n")
cat("  - Observations:", nrow(full_data), "weeks\n")
cat("  - Years:", min(full_data$year), "-", max(full_data$year), "\n")
cat("  - Weeks with vaccine data:", sum(!is.na(full_data$coverage)), "\n")
cat("  - Weeks with vaccine lag:", sum(!is.na(full_data$coverage_lag1)), "\n")
cat("  - Weeks WITHOUT vaccine lag (NA):", sum(is.na(full_data$coverage_lag1)), "\n")

# --- PART 2: Validation Dataset (Sentinel) ---
if(nrow(sentinel_weekly) > 0) {
  validation_data <- sentinel_weekly %>%
    left_join(bulgaria_temp_weekly, by = c("year", "week")) %>%
    left_join(bulgaria_cbr %>% dplyr::select(year, cbr), by = "year") %>%
    left_join(vaccine_weekly %>% dplyr::select(year, week, coverage), by = c("year", "week")) %>%
    filter(!is.na(cbr) & !is.na(temp_weekly)) %>%
    arrange(year, week) %>%
    mutate(
      trend = row_number(),
      detections_lag1 = lag(detections, 1),
      positivity_lag1 = lag(positivity, 1),
      tests_lag1 = lag(tests, 1),
      coverage_lag1 = lag(coverage, 1)
    ) %>%
    filter(!is.na(detections_lag1))
  
  cat("\n✓ Validation dataset (Sentinel + Vaccine):\n")
  cat("  - Observations:", nrow(validation_data), "weeks\n")
  cat("  - Years:", min(validation_data$year), "-", max(validation_data$year), "\n")
} else {
  validation_data <- data.frame()
  cat("\n⚠️ No validation data available\n")
}

# ============================================
# 7. REGRESSION - EXCESS MORTALITY (WITH INTERPOLATED VACCINE) - BULGARIA
# ============================================

cat("\n========================================\n")
cat("7. REGRESSION - EXCESS MORTALITY - BULGARIA\n")
cat("========================================\n\n")

if(nrow(full_data) >= 50) {
  
  # Model 1: Naive
  m1 <- lm(cbr ~ flu_proxy_lag1 + trend, data = full_data)
  cat("MODEL 1: Naive (Flu + Trend)\n")
  cat("========================================\n")
  print(summary(m1))
  
  # Model 2: DAG Adjusted
  m2 <- lm(cbr ~ flu_proxy_lag1 + temp_weekly + trend, data = full_data)
  cat("\nMODEL 2: DAG Adjusted (Flu + Temperature + Trend)\n")
  cat("========================================\n")
  print(summary(m2))
  
  # Model 3: DAG + Vaccine
  full_data_vaccine <- full_data %>%
    filter(!is.na(coverage_lag1))
  
  cat("\n✓ Data with vaccine (after interpolation):", nrow(full_data_vaccine), "weeks\n")
  
  if(nrow(full_data_vaccine) >= 30) {
    m3 <- lm(cbr ~ flu_proxy_lag1 + coverage_lag1 + temp_weekly + trend, 
             data = full_data_vaccine)
    cat("\nMODEL 3: DAG Adjusted + Vaccine\n")
    cat("========================================\n")
    print(summary(m3))
    
    # Model 4: Full DAG with Lags + Vaccine
    m4 <- lm(cbr ~ flu_proxy_lag1 + flu_proxy_lag2 + flu_proxy_lag3 + 
               flu_proxy_lag4 + coverage_lag1 + temp_weekly + trend, 
             data = full_data_vaccine)
    cat("\nMODEL 4: Full DAG (Multiple Lags + Vaccine)\n")
    cat("========================================\n")
    print(summary(m4))
    
    # Model 5: Moving Average + Vaccine
    m5 <- lm(cbr ~ flu_proxy_ma4 + coverage_lag1 + temp_weekly + trend, 
             data = full_data_vaccine)
    cat("\nMODEL 5: Moving Average + Vaccine\n")
    cat("========================================\n")
    print(summary(m5))
    
    models_excess <- list(
      "Naive" = m1,
      "DAG_Adjusted" = m2,
      "DAG_With_Vaccine" = m3,
      "Full_DAG_Vaccine" = m4,
      "MA4_Vaccine" = m5
    )
    
  } else {
    cat("\n⚠️ Insufficient data with vaccine\n")
    cat("  Need >= 30 weeks, have", nrow(full_data_vaccine), "\n")
    models_excess <- list(
      "Naive" = m1,
      "DAG_Adjusted" = m2
    )
  }
  
  # Model Comparison
  cat("\n========================================\n")
  cat("MODEL COMPARISON - EXCESS MORTALITY - BULGARIA\n")
  cat("========================================\n\n")
  
  comparison <- data.frame(
    Model = names(models_excess),
    R2 = sapply(models_excess, function(m) if(!is.null(m)) summary(m)$r.squared else NA),
    Adj_R2 = sapply(models_excess, function(m) if(!is.null(m)) summary(m)$adj.r.squared else NA),
    AIC = sapply(models_excess, function(m) if(!is.null(m)) AIC(m) else NA)
  )
  print(comparison)
  
  # Extract effects
  effects_excess <- data.frame()
  for(model_name in names(models_excess)) {
    m <- models_excess[[model_name]]
    if(!is.null(m)) {
      coefs <- summary(m)$coefficients
      for(i in 1:nrow(coefs)) {
        var_name <- rownames(coefs)[i]
        if(grepl("flu|coverage", var_name, ignore.case = TRUE)) {
          effects_excess <- rbind(effects_excess, data.frame(
            Model = model_name,
            Variable = var_name,
            Estimate = coefs[i, 1],
            P_Value = coefs[i, 4],
            Significant = ifelse(coefs[i, 4] < 0.05, "YES", "NO")
          ))
        }
      }
    }
  }
  
  cat("\n========================================\n")
  cat("EFFECTS - EXCESS MORTALITY - BULGARIA\n")
  cat("========================================\n\n")
  print(effects_excess)
  
} else {
  cat("⚠️ Insufficient data for regression\n")
}

# ============================================
# 8. REGRESSION - SENTINEL VALIDATION - BULGARIA
# ============================================

cat("\n========================================\n")
cat("8. REGRESSION - SENTINEL VALIDATION - BULGARIA\n")
cat("========================================\n\n")

if(nrow(validation_data) >= 20) {
  
  validation_vaccine <- validation_data %>%
    filter(!is.na(coverage_lag1))
  
  if(nrow(validation_vaccine) >= 10) {
    
    # Model 1: Detections + Vaccine
    m6 <- lm(cbr ~ detections_lag1 + coverage_lag1 + temp_weekly + trend, 
             data = validation_vaccine)
    cat("MODEL 1: Detections + Vaccine + Temperature + Trend\n")
    print(summary(m6))
    
    # Model 2: Positivity + Vaccine
    m7 <- lm(cbr ~ positivity_lag1 + coverage_lag1 + temp_weekly + trend, 
             data = validation_vaccine)
    cat("\nMODEL 2: Positivity + Vaccine + Temperature + Trend\n")
    print(summary(m7))
    
    # Model 3: Tests + Vaccine
    m8 <- lm(cbr ~ tests_lag1 + coverage_lag1 + temp_weekly + trend, 
             data = validation_vaccine)
    cat("\nMODEL 3: Tests + Vaccine + Temperature + Trend\n")
    print(summary(m8))
    
    # Model 4: All Indicators + Vaccine
    m9 <- lm(cbr ~ detections_lag1 + positivity_lag1 + tests_lag1 + 
               coverage_lag1 + temp_weekly + trend, data = validation_vaccine)
    cat("\nMODEL 4: All Indicators + Vaccine + Temperature + Trend\n")
    print(summary(m9))
    
    # Extract effects
    models_sentinel <- list(
      "Detections_Vaccine" = m6,
      "Positivity_Vaccine" = m7,
      "Tests_Vaccine" = m8,
      "All_Indicators_Vaccine" = m9
    )
    
    effects_sentinel <- data.frame()
    for(model_name in names(models_sentinel)) {
      m <- models_sentinel[[model_name]]
      coefs <- summary(m)$coefficients
      for(i in 1:nrow(coefs)) {
        var_name <- rownames(coefs)[i]
        if(grepl("detections|positivity|tests|coverage", var_name, ignore.case = TRUE)) {
          effects_sentinel <- rbind(effects_sentinel, data.frame(
            Model = model_name,
            Variable = var_name,
            Estimate = coefs[i, 1],
            P_Value = coefs[i, 4],
            Significant = ifelse(coefs[i, 4] < 0.05, "YES", "NO")
          ))
        }
      }
    }
    
    cat("\n========================================\n")
    cat("EFFECTS - SENTINEL VALIDATION - BULGARIA\n")
    cat("========================================\n\n")
    print(effects_sentinel)
    
  } else {
    cat("⚠️ Insufficient data with vaccine for sentinel validation\n")
  }
  
} else {
  cat("⚠️ Insufficient sentinel data\n")
}

# ============================================
# 9. FINAL CONCLUSION - BULGARIA
# ============================================

cat("\n========================================\n")
cat("9. FINAL CONCLUSION - BULGARIA\n")
cat("========================================\n\n")

cat("ANALYSIS: Bulgaria\n")
cat("Primary: Excess Mortality (2000-2025)\n")
cat("Validation: Sentinel (2021-2025)\n")
cat("Control: Vaccine Coverage (INTERPOLATED)\n")
cat("========================================\n\n")

if(exists("effects_excess") && nrow(effects_excess) > 0) {
  
  flu_effects <- effects_excess %>%
    filter(grepl("flu", Variable, ignore.case = TRUE))
  
  vaccine_effects <- effects_excess %>%
    filter(grepl("coverage", Variable, ignore.case = TRUE))
  
  cat("INFLUENZA EFFECTS (with interpolated vaccine control):\n")
  cat("------------------------------------------------------\n")
  
  if(nrow(flu_effects) > 0) {
    sig_flu <- flu_effects %>% filter(Significant == "YES")
    if(nrow(sig_flu) > 0) {
      cat("✅ SIGNIFICANT\n")
      print(sig_flu)
    } else {
      cat("❌ NOT SIGNIFICANT\n")
      best <- flu_effects %>% arrange(P_Value) %>% slice(1)
      cat(sprintf("   Best: β = %.6f, p = %.4f\n", best$Estimate, best$P_Value))
    }
  }
  
  if(nrow(vaccine_effects) > 0) {
    cat("\nVACCINE EFFECTS:\n")
    cat("----------------\n")
    sig_vaccine <- vaccine_effects %>% filter(Significant == "YES")
    if(nrow(sig_vaccine) > 0) {
      cat("✅ SIGNIFICANT\n")
      print(sig_vaccine)
    } else {
      cat("❌ NOT SIGNIFICANT\n")
      best <- vaccine_effects %>% arrange(P_Value) %>% slice(1)
      cat(sprintf("   Best: β = %.6f, p = %.4f\n", best$Estimate, best$P_Value))
    }
  }
  
  cat("\n========================================\n")
  cat("OVERALL CONCLUSION - BULGARIA\n")
  cat("========================================\n\n")
  
  if(nrow(flu_effects) > 0) {
    any_flu_sig <- any(flu_effects$Significant == "YES")
    if(any_flu_sig) {
      cat("✅ INFLUENZA MEMPENGARUHI FERTILITAS\n")
      cat("   (dengan interpolated vaccine control)\n")
    } else {
      cat("❌ INFLUENZA TIDAK MEMPENGARUHI FERTILITAS\n")
      cat("   (dengan interpolated vaccine control)\n")
      best <- flu_effects %>% arrange(P_Value) %>% slice(1)
      cat(sprintf("\n   Closest: β = %.6f, p = %.4f\n", best$Estimate, best$P_Value))
    }
  }
  
} else {
  cat("⚠️ No results to summarize\n")
}

cat("\n========================================\n")
cat("ANALYSIS COMPLETE - BULGARIA!\n")
cat("========================================\n")

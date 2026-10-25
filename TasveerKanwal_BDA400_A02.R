# ---- 1. Packages ----
required_packages <- c("quantmod", "TTR", "ggplot2")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# ---- 2. Importing the portfolio ----
read_portfolio <- function(path = "portfolio.txt") {
  if (!file.exists(path)) stop("Portfolio file not found: ", path)
  symbols <- toupper(trimws(readLines(path, warn = FALSE)))
  unique(symbols[nzchar(symbols)])
}

load_stock_data <- function(path = "portfolio.txt", from = "2023-01-01", to = "2023-07-01") {
  stocks <- list()
  for (symbol in read_portfolio(path)) {
    df <- tryCatch({
      raw <- getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)
      out <- data.frame(Date = index(raw), coredata(raw))
      colnames(out) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      out <- na.omit(out)
      rownames(out) <- NULL
      out
    }, error = function(e) {
      warning("Could not load ", symbol, ": ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (!is.null(df) && nrow(df) > 0) stocks[[symbol]] <- df
  }
  stocks
}

# ---- 3. Computing statistics ----
calculate_mode <- function(x, digits = 0) {
  rounded <- round(x, digits)
  counts <- table(rounded)
  as.numeric(names(counts)[which.max(counts)])
}

add_moving_average <- function(df, window = 20) {
  df[[paste0("MA", window)]] <- as.numeric(SMA(df$Close, n = window))
  df
}

calculate_statistics <- function(df, symbol, window = 20) {
  with_ma <- add_moving_average(df, window)
  latest_ma <- tail(na.omit(with_ma[[paste0("MA", window)]]), 1)
  data.frame(
    Symbol = symbol,
    Days = nrow(df),
    Mean = mean(df$Close),
    Median = median(df$Close),
    Mode = calculate_mode(df$Close),
    StdDev = sd(df$Close),
    Min = min(df$Close),
    Max = max(df$Close),
    MovingAvg = if (length(latest_ma) == 0) NA_real_ else latest_ma,
    stringsAsFactors = FALSE
  )
}

calculate_all_statistics <- function(stocks, window = 20) {
  rows <- lapply(names(stocks), function(s) calculate_statistics(stocks[[s]], s, window))
  result <- do.call(rbind, rows)
  names(result)[names(result) == "MovingAvg"] <- paste0("MA", window, "_Latest")
  result
}

# ---- 4. Display utilities ----
display_stock_table <- function(df, symbol, rows = 6) {
  line <- strrep("=", 62)
  cat("\n", line, "\n", symbol, ": ", nrow(df), " trading days (",
      format(min(df$Date)), " to ", format(max(df$Date)), ")\n", line, "\n", sep = "")
  cat("First", rows, "rows\n")
  print(head(df, rows), row.names = FALSE)
  cat("\nLast", rows, "rows\n")
  print(tail(df, rows), row.names = FALSE)
}

display_summary <- function(df, symbol) {
  cat("\nSummary statistics for", symbol, "\n")
  print(summary(df[, c("Open", "High", "Low", "Close", "Volume")]))
}

display_statistics <- function(stats) {
  cat("\nPortfolio statistics (closing prices)\n")
  print(stats, row.names = FALSE, digits = 4)
}

display_candlestick <- function(df, symbol, window = 20) {
  series <- xts(df[, c("Open", "High", "Low", "Close", "Volume")], order.by = df$Date)
  chartSeries(series, name = symbol, theme = "white",
              TA = paste0("addSMA(n=", window, ");addVolume()"))
}

# ---- 5. Visualizations ----
plot_price_with_ma <- function(df, symbol, window = 20) {
  df <- add_moving_average(df, window)
  ma_label <- paste0(window, "-day MA")
  ggplot(df, aes(x = Date)) +
    geom_line(aes(y = Close, colour = "Close"), linewidth = 0.8) +
    geom_line(aes(y = .data[[paste0("MA", window)]], colour = ma_label), linewidth = 0.9, na.rm = TRUE) +
    scale_colour_manual(values = setNames(c("#1565c0", "#ef6c00"), c("Close", ma_label))) +
    labs(title = paste(symbol, "closing price with moving average"),
         x = NULL, y = "Price (USD)", colour = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

plot_volume <- function(df, symbol) {
  ggplot(df, aes(x = Date, y = Volume / 1e6)) +
    geom_col(fill = "#78909c") +
    labs(title = paste(symbol, "trading volume"), x = NULL, y = "Volume (millions)") +
    theme_minimal(base_size = 13)
}

plot_close_distribution <- function(df, symbol) {
  ggplot(df, aes(x = Close)) +
    geom_histogram(bins = 20, fill = "#90caf9", colour = "white") +
    geom_vline(aes(xintercept = mean(Close), linetype = "Mean"), colour = "#c62828", linewidth = 0.9) +
    geom_vline(aes(xintercept = median(Close), linetype = "Median"), colour = "#2e7d32", linewidth = 0.9) +
    scale_linetype_manual(values = c(Mean = "solid", Median = "dashed")) +
    labs(title = paste(symbol, "distribution of closing prices"),
         x = "Closing price (USD)", y = "Number of days", linetype = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

plot_portfolio_comparison <- function(stocks) {
  rebased <- lapply(names(stocks), function(s) {
    df <- stocks[[s]]
    data.frame(Date = df$Date, Symbol = s, Index = df$Close / df$Close[1] * 100)
  })
  combined <- do.call(rbind, rebased)
  ggplot(combined, aes(x = Date, y = Index, colour = Symbol)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
    labs(title = "Portfolio comparison (first day = 100)", x = NULL, y = "Rebased price", colour = NULL) +
    theme_minimal(base_size = 13)
}

plot_statistics_comparison <- function(stats) {
  long <- rbind(
    data.frame(Symbol = stats$Symbol, Measure = "Mean", Value = stats$Mean),
    data.frame(Symbol = stats$Symbol, Measure = "Median", Value = stats$Median),
    data.frame(Symbol = stats$Symbol, Measure = "Std Dev", Value = stats$StdDev)
  )
  ggplot(long, aes(x = Symbol, y = Value, fill = Measure)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(Mean = "#1565c0", Median = "#2e7d32", `Std Dev` = "#ef6c00")) +
    labs(title = "Mean, median and standard deviation by stock", x = NULL, y = "USD", fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

# ---- 6. Run the analysis ----
portfolio <- load_stock_data("portfolio.txt", from = "2023-01-01", to = "2023-07-01")
list2env(portfolio, envir = globalenv())

for (symbol in names(portfolio)) {
  display_stock_table(portfolio[[symbol]], symbol)
  display_summary(portfolio[[symbol]], symbol)
}

stats_table <- calculate_all_statistics(portfolio, window = 20)
display_statistics(stats_table)

dir.create("plots", showWarnings = FALSE)

for (symbol in names(portfolio)) {
  df <- portfolio[[symbol]]
  charts <- list(
    price_ma = plot_price_with_ma(df, symbol),
    volume = plot_volume(df, symbol),
    distribution = plot_close_distribution(df, symbol)
  )
  for (name in names(charts)) {
    print(charts[[name]])
    ggsave(file.path("plots", paste0(symbol, "_", name, ".png")), charts[[name]],
           width = 9, height = 5, dpi = 150)
  }
}

comparison <- plot_portfolio_comparison(portfolio)
print(comparison)
ggsave(file.path("plots", "portfolio_comparison.png"), comparison, width = 9, height = 5, dpi = 150)

stats_chart <- plot_statistics_comparison(stats_table)
print(stats_chart)
ggsave(file.path("plots", "statistics_comparison.png"), stats_chart, width = 9, height = 5, dpi = 150)

display_candlestick(portfolio[[1]], names(portfolio)[1])

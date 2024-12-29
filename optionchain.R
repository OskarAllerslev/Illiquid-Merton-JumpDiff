
optionchain <- function(){
  library(httr)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
  
  parse_option_identifier <- function(identifier) {
    ticker <- substr(identifier, 1, 4)
    expiration_date <- as.Date(substr(identifier, 5, 10), format = "%y%m%d")
    option_type <- substr(identifier, 11, 11)
    strike_price <- as.numeric(substr(identifier, 12, nchar(identifier))) / 1000
    
    data.frame(
      ticker = ticker,
      expiration_date = expiration_date,
      option_type = ifelse(option_type == "P", "Put", "Call"),
      strike_price = strike_price,
      stringsAsFactors = FALSE
    )
  }
  # API request
  url <- "https://data.alpaca.markets/v1beta1/options/snapshots/GERN"
  
  queryString <- list(
    feed = "indicative",
    limit = "1000"
  )
  
  response <- VERB(
    "GET",
    url,
    query = queryString,
    add_headers(
      'APCA-API-KEY-ID' = 'PKCYJWOETNBK6VB6KQ0L',
      'APCA-API-SECRET-KEY' = 'T4rUiEMgAhnSvrNpAnjMoQL1m4A7yJlR61vUSXnT'
    ),
    content_type("application/octet-stream"),
    accept("application/json")
  )
  
  # Parse JSON response
  options_data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Extract snapshots
  snapshots <- options_data$snapshots
  
  # Safe extraction function
  safe_extract <- function(field, default = NA) {
    if (is.null(field)) return(default)
    return(field)
  }
  
  # Updated flatten_option to include parsed fields
  flatten_option <- function(option_key, option_data) {
    # Parse the option identifier
    parsed <- parse_option_identifier(option_key)
    
    # Extract fields safely
    dailyBar <- option_data$dailyBar
    greeks <- option_data$greeks
    latestQuote <- option_data$latestQuote
    latestTrade <- option_data$latestTrade
    minuteBar <- option_data$minuteBar
    prevDailyBar <- option_data$prevDailyBar
    
    # Combine all data into a single row
    data.frame(
      ticker = parsed$ticker,
      expiration_date = parsed$expiration_date,
      option_type = parsed$option_type,
      strike_price = parsed$strike_price,
      # Daily Bar
      daily_close = safe_extract(dailyBar$c),
      daily_high = safe_extract(dailyBar$h),
      daily_low = safe_extract(dailyBar$l),
      daily_volume = safe_extract(dailyBar$v),
      daily_vw = safe_extract(dailyBar$vw),
      # Greeks
      delta = safe_extract(greeks$delta),
      gamma = safe_extract(greeks$gamma),
      rho = safe_extract(greeks$rho),
      theta = safe_extract(greeks$theta),
      vega = safe_extract(greeks$vega),
      implied_volatility = safe_extract(option_data$impliedVolatility),
      # Latest Quote
      ask_price = safe_extract(latestQuote$ap),
      ask_size = safe_extract(latestQuote$as),
      bid_price = safe_extract(latestQuote$bp),
      bid_size = safe_extract(latestQuote$bs),
      # Latest Trade
      trade_price = safe_extract(latestTrade$p),
      trade_size = safe_extract(latestTrade$s),
      trade_time = safe_extract(latestTrade$t),
      # Minute Bar
      minute_open = safe_extract(minuteBar$o),
      minute_close = safe_extract(minuteBar$c),
      minute_high = safe_extract(minuteBar$h),
      minute_low = safe_extract(minuteBar$l),
      minute_volume = safe_extract(minuteBar$v),
      # Previous Daily Bar
      prev_close = safe_extract(prevDailyBar$c),
      prev_high = safe_extract(prevDailyBar$h),
      prev_low = safe_extract(prevDailyBar$l),
      prev_volume = safe_extract(prevDailyBar$v),
      stringsAsFactors = FALSE
    )
  }
  
  
  
  
  # Iterate over snapshots and flatten all options
  options_list <- lapply(names(snapshots), function(key) {
    flatten_option(key, snapshots[[key]])
  })
  
  # Combine into a single data frame
  options_df <- do.call(rbind, options_list)
  
  options <- data.frame(
    ticker = options_df$ticker,
    expiration = options_df$expiration_date,
    type = options_df$option_type,
    strike = options_df$strike_price,
    ask = options_df$ask_price,
    bid = options_df$bid_price,
    expiration_date = options_df$expiration
  )
  

  
  return(options)
  
}


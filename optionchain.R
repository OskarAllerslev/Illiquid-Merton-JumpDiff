optionchain <- function(){
  library(httr)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
  
  # 1) Flexible parse function for OCC symbols
  #    (If length < 15, we skip detailed parsing and just store the symbol in "ticker".)
  parse_option_identifier <- function(sym) {
    len <- nchar(sym)
    
    # If length < 15, can't parse standard OCC format (YYMMDD + C/P + strike).
    # We'll just store the raw symbol as ticker, and set everything else to NA.
    if (len < 15) {
      return(data.frame(
        ticker           = sym,
        expiration_date  = as.Date(NA),
        option_type      = NA,
        strike_price     = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    
    # Otherwise, parse as OCC:
    # [up to 6 chars for ticker][6 chars YYMMDD][1 char C/P][8 chars for strike*1000]
    ticker     <- substr(sym, 1, len - 15)
    yymmdd     <- substr(sym, len - 14, len - 9)
    cp_flag    <- substr(sym, len - 8, len - 8)
    strike_str <- substr(sym, len - 7, len)
    
    # Convert date
    expiration_date <- as.Date(yymmdd, format = "%y%m%d")
    
    # Convert C/P
    if (cp_flag == "C") {
      option_type <- "Call"
    } else if (cp_flag == "P") {
      option_type <- "Put"
    } else {
      option_type <- NA
    }
    
    # Convert strike
    strike_price <- as.numeric(strike_str) / 1000
    
    data.frame(
      ticker           = ticker,
      expiration_date  = expiration_date,
      option_type      = option_type,
      strike_price     = strike_price,
      stringsAsFactors = FALSE
    )
  }
  
  # 2) Safe extraction function
  safe_extract <- function(field, default = NA) {
    if (is.null(field)) return(default)
    return(field)
  }
  
  # 3) Flatten each option
  flatten_option <- function(option_key, option_data) {
    # Parse the option identifier (OCC format, or fallback)
    parsed <- parse_option_identifier(option_key)
    
    # Extract fields safely
    dailyBar    <- option_data$dailyBar
    greeks      <- option_data$greeks
    latestQuote <- option_data$latestQuote
    latestTrade <- option_data$latestTrade
    minuteBar   <- option_data$minuteBar
    prevDailyBar<- option_data$prevDailyBar
    
    data.frame(
      ticker = parsed$ticker,
      expiration_date = parsed$expiration_date,
      option_type = parsed$option_type,
      strike_price = parsed$strike_price,
      
      # Daily Bar
      daily_close   = safe_extract(dailyBar$c),
      daily_high    = safe_extract(dailyBar$h),
      daily_low     = safe_extract(dailyBar$l),
      daily_volume  = safe_extract(dailyBar$v),
      daily_vw      = safe_extract(dailyBar$vw),
      
      # Greeks
      delta               = safe_extract(greeks$delta),
      gamma               = safe_extract(greeks$gamma),
      rho                 = safe_extract(greeks$rho),
      theta               = safe_extract(greeks$theta),
      vega                = safe_extract(greeks$vega),
      implied_volatility  = safe_extract(option_data$impliedVolatility),
      
      # Latest Quote
      ask_price           = safe_extract(latestQuote$ap),
      ask_size            = safe_extract(latestQuote$as),
      bid_price           = safe_extract(latestQuote$bp),
      bid_size            = safe_extract(latestQuote$bs),
      
      # Latest Trade
      trade_price         = safe_extract(latestTrade$p),
      trade_size          = safe_extract(latestTrade$s),
      trade_time          = safe_extract(latestTrade$t),
      
      # Minute Bar
      minute_open         = safe_extract(minuteBar$o),
      minute_close        = safe_extract(minuteBar$c),
      minute_high         = safe_extract(minuteBar$h),
      minute_low          = safe_extract(minuteBar$l),
      minute_volume       = safe_extract(minuteBar$v),
      
      # Previous Daily Bar
      prev_close          = safe_extract(prevDailyBar$c),
      prev_high           = safe_extract(prevDailyBar$h),
      prev_low            = safe_extract(prevDailyBar$l),
      prev_volume         = safe_extract(prevDailyBar$v),
      
      stringsAsFactors = FALSE
    )
  }
  
  # 4) Make the API request
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
      'APCA-API-KEY-ID'     = 'PKCYJWOETNBK6VB6KQ0L',   # <- your keys
      'APCA-API-SECRET-KEY' = 'T4rUiEMgAhnSvrNpAnjMoQL1m4A7yJlR61vUSXnT'
    ),
    content_type("application/octet-stream"),
    accept("application/json")
  )
  
  # 5) Parse JSON response
  options_data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # 6) Extract snapshots
  snapshots <- options_data$snapshots
  
  if (is.null(snapshots) || length(snapshots) == 0) {
    warning("No snapshots returned. Returning NULL.")
    return(NULL)
  }
  
  # 7) Flatten all options
  options_list <- lapply(names(snapshots), function(key) {
    flatten_option(key, snapshots[[key]])
  })
  
  # 8) Combine
  options_df <- do.call(rbind, options_list)
  
  # 9) Final "tidy" frame to return
  options <- data.frame(
    ticker          = options_df$ticker,
    expiration      = options_df$expiration_date,
    type            = options_df$option_type,
    strike          = options_df$strike_price,
    ask             = options_df$ask_price,
    bid             = options_df$bid_price,
    expiration_date = options_df$expiration_date  # duplicates 'expiration' if you like
  )
  
  return(options)
}
optionchain()
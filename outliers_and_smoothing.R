# Removes NAs and takes value next to missing value - this may make the score go up slightly?
correct_outliers <- function(df, th = 2) {
  # df = df[tripID==tripID][["dist_pro"]]
  # Created to shift lat/lngs up/down and to fill last/start values with 0
  df[, c("latDeg_pre", "latDeg_pro", "lngDeg_pre", "lngDeg_pro") := .(
    shift(LatitudeDegrees, fill = 0, type = "lag"),
    shift(LatitudeDegrees, fill = 0, type = "lead"),
    shift(LongitudeDegrees, fill = 0, type = "lag"),
    shift(LongitudeDegrees, fill = 0, type = "lead")
  )]
  
  # Calculates the distance between each continuous point from point before (pre) and point after (pro)
  df[, 'dist_pre'] = calc_haversine(df[, 'latDeg_pre'], df[, 'lngDeg_pre'],
                                    df[, 'LatitudeDegrees'], df[, 'LongitudeDegrees'])
  df[, 'dist_pro'] = calc_haversine(df[, 'LatitudeDegrees'], df[, 'LongitudeDegrees'], df[, 'latDeg_pro'], df[, 'lngDeg_pro'])
  
  # Replace distance for first/last point since there was a 0
  df[1,][, 'dist_pre'] <- 0
  df[nrow(df),][, 'dist_pro'] <- 0
  
  # Looks for distances 2x sd, and take mean of values before/after point.
  pro_95 = mean(df[["dist_pro"]], na.rm = T) + sd(df[["dist_pro"]], na.rm = T) * th
  pre_95 = mean(df[["dist_pre"]], na.rm = T) + sd(df[["dist_pre"]], na.rm = T) * th
  
  # ADDED is.na(), | is.na(df$LatitudeDegrees), removed and score was same
  ind = which((df[, dist_pro] > pro_95 &
                 df[, dist_pre] > pre_95) | is.na(df$LatitudeDegrees), arr.ind = TRUE)
  # ind = c(ind, which(is.na(df$LatitudeDegrees)))
  for (i in ind) {
    df[i, 'LatitudeDegrees']  = mean(df[i - 1, LatitudeDegrees],  df[i + 1, LatitudeDegrees], na.rm = T, trim = 0)
    df[i, 'LongitudeDegrees'] = mean(df[i - 1, LongitudeDegrees], df[i + 1, LongitudeDegrees], na.rm = T, trim = 0)
  }
  return(df)
}

# pracma also has savgol, but results may not match, per stackoverflow
# Smooths data
apply_savgol_filter <- function(tripID, df, wl, poly) {
  # df = df[tripID == tripID]
  df[, 'LatitudeDegrees'] = signal::sgolayfilt(df[, LatitudeDegrees], n = wl, p = poly)
  df[, 'LongitudeDegrees'] = signal::sgolayfilt(df[, LongitudeDegrees], n = wl, p = poly)
  return(df)
}

optimize <- function(params) {
  score_list = data.frame()
  
  threshold = unlist(params[1])
  window_len = unlist(params[2])
  poly_order = unlist(params[3])
  # print(threshold)
  # window_len = as.integer(window_len)
  # must be odd
  if (window_len %% 2 == 0) {
    window_len = window_len + 1
  }
  for (i in 1:length(trip_ids)) {
    trip = trip_ids[i]
    
    pred_df = blh_df[tripID == trip]
    
    # We are using BLH, not ECEF to optimize
    pred_df = correct_outliers(pred_df, threshold)# |> setDT() # ??
    pred_df = apply_savgol_filter(tripID, df = pred_df, wl = window_len, poly_order)
    # print(pred_df)
    score = calc_score(trip, pred_df)
    
    score_list = bind_rows(score_list, score)
    
  }
  # score_list = bind_rows(score_list, score)
  # }
  # mean_score = mean(score_list$score)
  return (mean(score_list[["score"]]))
  # score
}
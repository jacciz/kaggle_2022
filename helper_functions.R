# Calculates score for each trip. Returns df with tripID and score.
calc_score <- function(trip_id, pred_df) {
  pred_df = pred_df[tripID == trip_id] # So we can map_df() this function
  gt_df = open_gt(trip_id)
  d = pandas_haversine_distance(pred_df, gt_df)
  score = mean(c(quantile(d, .5, na.rm = T), quantile(d, .95, na.rm = T)), na.rm = T)
  return (data.frame(trip_id, score))
}
open_gt <- function(trip_id){
  read.csv(paste0("data/train/", trip_id, "/ground_truth.csv"))
}
open_data <- function(dirname){
  # Combines cleaned and ground_truth into a single df
  name = gsub("data/train/", '', dirname)
  tripID  = unlist(str_split(name, "/"))[1:2] |> paste(collapse = '/')
  gnss_df = read.csv(dirname) |> setDT()
  gt_df   = read.csv(gsub("device_gnss", "ground_truth", dirname)) |> setDT()
  gt_df[, tripID := tripID]
  
  # Adjusted to then lat lat/lng
  pred_df = ecef_to_lat_lng(tripID, gnss_df, gt_df[, 'UnixTimeMillis']) |> setDT()
  # pred_df[ , MessageType :="Raw"]
  # gt_df[ , c(colnames(pred_df))]
  # gt_df = gt_df |> select(colnames(pred_df))
  # 
  # bind_rows( pred_df, gt_df)
  pred_df
}

# Use this to batch open and clean data
# file_list = list.files(path="data/train", pattern = "device_gnss.csv$", recursive = TRUE, full.names=TRUE)#[1]
# dirname = x

# Then save as an RDS
# all = purrr::map_df(file_list, open_data)
# write_rds(all, "data/train/cleaned_to_blh.rds")

# Don't use anymore
# score_list <- function(tripID, cleaned_data) {
#   cleaned_data = cleaned_data[tripID == tripID]
#   gt_df = filter(cleaned_data, MessageType == "Fix")
#   pred_df = filter(cleaned_data, MessageType == "Raw")
#   score = calc_score(tripID, pred_df, gt_df)
#   # print(f'{tripID:<45}: score = {score:.3f}')
#   # score_list = bind_rows(score_list, score)
#   score
# }
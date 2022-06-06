library(tidyverse)
library(data.table)
# options("scipen"=100, "digits"=4)
path = "data/train/2020-05-28-US-MTV-2/GooglePixel4XL/"
gnss_df = read.csv(paste0(path, "device_gnss.csv")) |> setDT()
i = read.csv(paste0(path, "device_imu.csv"))|> setDT()
truth = read.csv(paste0(path, "ground_truth.csv"))|> setDT()
# l = readLines(paste0(path, "supplemental/gnss_log.txt"))

s = head(i, 1000) |> filter(MeasurementY  < -10)
ggplot(s) +
  geom_point(aes(x = MeasurementX, y = MeasurementY))

# ggplot(truth) +
#   geom_point(aes(x = LatitudeDegrees, y = LongitudeDegrees))

gi = full_join(gnss_df, i , by = "utcTimeMillis")
gi[21, .(utcTimeMillis)]
colnames(i)

# list.files(path="data/train/", recursive = TRUE)
list.files(path="data/train/", pattern="imu.csv", full.names=FALSE, recursive = TRUE)
# myfiles[grepl("94", myfiles)]

## SMOOTHING
carr_th = 2.0 # carrier phase jump threshold [m]
pr_th =  20.0 # pseudorange jump threshold [m]

gnss_df |> mutate( ch = (AccumulatedDeltaRangeMeters - lag(AccumulatedDeltaRangeMeters, n = 1))/1 )
AccumulatedDeltaRangeMeters 

# Carrier Smoothing
# prsmooth = np.full_like(gnss_df['RawPseudorangeMeters'], np.nan) # return matrix filled with NA

for (i in group_by(gnss_df, Svid, SignalType)){
  print(i)
}
gnss_df[ .()]


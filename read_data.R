library(tidyverse)
library(data.table)
# options("scipen"=100, "digits"=4)
path = "data/train/2020-05-28-US-MTV-2/GooglePixel4XL/"
gnss_df = read.csv(paste0(path, "device_gnss.csv")) |> setDT()
i = read.csv(paste0(path, "device_imu.csv"))|> setDT()
truth = read.csv(paste0(path, "ground_truth.csv"))|> setDT()
# l = readLines(paste0(path, "supplemental/gnss_log.txt"))


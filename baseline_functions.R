WGS84_SEMI_MAJOR_AXIS = 6378137.0
WGS84_SEMI_MINOR_AXIS = 6356752.314245
WGS84_SQUARED_FIRST_ECCENTRICITY  = 6.69437999013e-3
WGS84_SQUARED_SECOND_ECCENTRICITY = 6.73949674226e-3
HAVERSINE_RADIUS = 6371000

# Convert into matrix with X, Y, Z. Input df with raw 'WlsPosition' for colnames.
ECEF <- function(gnss_df) {
  xyz_col = gnss_df[, grep("WlsPosition", colnames(gnss_df), value = TRUE)]
  ECEF = gnss_df[, ..xyz_col] |> as.matrix()
  colnames(ECEF) <- c("x", "y", "z")
  return(ECEF)
}

# Converts x,y,z to WGS84 BLH. Needs xyz for colnames. Outputs matrix with lat/lng/hgt colnames.
ECEF_to_BLH <- function(ecef){
  a = WGS84_SEMI_MAJOR_AXIS
  b = WGS84_SEMI_MINOR_AXIS
  e2  = WGS84_SQUARED_FIRST_ECCENTRICITY
  e2_ = WGS84_SQUARED_SECOND_ECCENTRICITY
  x = ecef[, "x"]
  y = ecef[, "y"]
  z = ecef[, "z"]
  r = sqrt(x**2 + y**2)
  t = atan2(z * (a/b), r)
  B = atan2(z + (e2_*b)*sin(t)**3, r - (e2*a)*cos(t)**3)
  L = atan2(y, x)
  n = a / sqrt(1 - e2*sin(B)**2)
  H = (r / cos(B)) - n
  # return BLH(lat=B, lng=L, hgt=H)
  fixed = matrix(c(B,L,H), ncol = 3)
  colnames(fixed) <- c("lat", "lng", "hgt")
  return(fixed)
}

# Finds distance bwn predicted and ground truth points
haversine_distance <- function(blh_1, blh_2) {
  dlat = blh_2[, "lat"] - blh_1[, "lat"]
  dlng = blh_2[, "lng"] - blh_1[, "lng"]
  a = sin(dlat / 2) ** 2 + cos(blh_1[, "lat"]) * cos(blh_2[, "lat"]) * sin(dlng /
                                                                             2) ** 2
  dist = 2 * HAVERSINE_RADIUS * asin(sqrt(a)) #was arcsin()
  return (dist)
}

#' Get haversine distance. This is used for score_card().
#'
#' @param df1 raw with 'LatitudeDegrees/LongitudeDegrees'
#' @param df2 gt with 'LatitudeDegrees/LongitudeDegrees'
#'
#' @return list of numbers of distances with 'lat/lng'.
pandas_haversine_distance <- function(df1, df2) {
  blh1 = matrix(c(pracma::deg2rad(unlist(df1[, 'LatitudeDegrees'])),
                  pracma::deg2rad(unlist(df1[, 'LongitudeDegrees']))),
                ncol = 2) # REMOVE hgt
  blh1 = cbind(blh1, hgt = 0)
  colnames(blh1) <- c("lat", "lng", "hgt")
  blh2 = matrix(c(pracma::deg2rad(unlist(df2[, 'LatitudeDegrees'])),
                  pracma::deg2rad(unlist(df2[, 'LongitudeDegrees']))),
                ncol = 2)
  blh2 = cbind(blh2, hgt = 0)
  colnames(blh2) <- c("lat", "lng", "hgt")
  return(haversine_distance(blh1, blh2))
}

#' Converts raw gnss ECEF (raw 'WlsPosition' for colnames) into lat/lng, removes duplicated utcTimeMillis.
#' Convert to BLH and lat/lng using ECEF_to_BLH(). Output colnames is for score_card.
#'
#' @param tripID character string
#' @param gnss_df 
#' @param UnixTimeMillis from gt[ , 'UnixTimeMillis']
#'
#' @return df with tripID, UnixTimeMillis, LatitudeDegrees, LongitudeDegrees
# TODO I don't know purpose of interpolation: interp1()
# TODO some files have missing Lats/Longs - did do a 'take value before/after'
ecef_to_lat_lng <- function(tripID, gnss_df, UnixTimeMillis){
  ecef_columns = c('WlsPositionXEcefMeters', 'WlsPositionYEcefMeters', 'WlsPositionZEcefMeters')
  columns = c('utcTimeMillis', ecef_columns)
  
  ecef_df = gnss_df[!is.na(utcTimeMillis)][!duplicated(utcTimeMillis), ..columns] ## dropna??
  
  ecef = ecef_df[, ..ecef_columns]
  ecef = ECEF(ecef)
  blh  = ECEF_to_BLH(ecef)
  
  TIME = ecef_df[, 'utcTimeMillis'] |> as.matrix() |> as.numeric()
  # lat = InterpolatedUnivariateSpline(TIME, blh.lat, ext=3)(UnixTimeMillis)
  # lng = InterpolatedUnivariateSpline(TIME, blh.lng, ext=3)(UnixTimeMillis)
  
  # Fill in na ??
  lat = pracma::interp1(TIME, blh[, "lat"], method = "cubic")
  lng = pracma::interp1(TIME, blh[, "lng"], method = "cubic")
  
  # lat |> as.data.frame() |> filter(is.na(lat))
  
  # What does it interpolate ? I see difference between raw and adjusted is 0
  # lng = pracma::interp1(TIME, ecef_df$LongitudeDegrees, method = "constant")
  # both = cbind(ecef_df$LongitudeDegrees, lng)
  # both = cbind(both,  diff =  both[,1] - both[, 2])
  # both[ both[, 'diff'] != 0 ]
  
  data.frame('tripID' = tripID,
             'UnixTimeMillis' = UnixTimeMillis,
             'LatitudeDegrees' = pracma::rad2deg(lat),
             'LongitudeDegrees'= pracma::rad2deg(lng))
}


# TODO Same a and dist, but different radius as pandas_haversine_distance(). Could just use geosphere::distHaversine()
calc_haversine <- function(lat1, lon1, lat2, lon2){
  # """Calculates the great circle distance between two points
  # on the earth. Inputs are array-like and specified in decimal degrees.
  # """
  RADIUS = 6367000
  # lat1, lon1, lat2, lon2 = map(np.radians, [lat1, lon1, lat2, lon2])
  coor = sapply(c(lat1, lat2, lon1, lon2), pracma::deg2rad)
  
  colnames(coor) <- c("lat1", "lat2", "lon1", "lon2")
  # print(coor)
  # pracma::deg2rad(lat1)
  # sapply(c(lat1, lat2), pracma::deg2rad)
  dlat = coor[, 'lat2'] - coor[, 'lat1']
  dlon = coor[, 'lon2'] - coor[, 'lon1']
  # dlat = c["lat2.LatitudeDegrees"] - c["lat1.latDeg_pre"]
  # dlon = c["lon2.LongitudeDegrees"] - c["lon1.lngDeg_pre"]
  a = sin(dlat/2)**2 + cos(coor[, 'lat1']) * cos(coor[, 'lat2']) * sin(dlon/2)**2
  dist = 2 * RADIUS * asin(a**0.5)
  return (dist)
}
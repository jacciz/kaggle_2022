# def exclude_interpolate_outlier(r.r.x_wls, r.r.v_wls, r.r.cov_x, r.r.cov_v):
# Up velocity / height threshold
v_up_th = 2.6# m/s2.0 -> 2.6
height_th = 200.0 # m
v_out_sigma = 3.0 # m/s
x_out_sigma = 30.0 # m

# Coordinate conversion
x_llh = np.array(pm.ecef2geodetic(r.x_wls[:, 0], r.x_wls[:, 1], r.x_wls[:, 2])).T
x_llh_mean = np.nanmean(x_llh, axis=0)
v_enu = np.array(pm.ecef2enuv(
r.v_wls[:, 0], r.v_wls[:, 1], r.v_wls[:, 2], x_llh_mean[0], x_llh_mean[1])).T

# Up velocity jump detection
# Cars don't jump suddenly!
idx_v_out = np.abs(v_enu[:, 2]) > v_up_th
idx_v_out |= np.isnan(v_enu[:, 2])
print(idx_v_out)
r.v_wls[idx_v_out, :] = np.nan
# r.cov_v[idx_v_out] = v_out_sigma**2 * np.eye(3)
# print(f'Number of velocity outliers {np.count_nonzero(idx_v_out)}')
# 
# # Height check
# hmedian = np.nanmedian(x_llh[:, 2])
# idx_x_out = np.abs(x_llh[:, 2] - hmedian) > height_th
# idx_x_out |= np.isnan(x_llh[:, 2])
# r.x_wls[idx_x_out, :] = np.nan
# r.cov_x[idx_x_out] = x_out_sigma**2 * np.eye(3)
# print(f'Number of position outliers {np.count_nonzero(idx_x_out)}')
# 
# # Interpolate NaNs at beginning and end of array
# x_df = pd.DataFrame({'x': r.x_wls[:, 0], 'y': r.x_wls[:, 1], 'z': r.x_wls[:, 2]})
# x_df = x_df.interpolate(limit_area='outside', limit_direction='both')
# 
# # Interpolate all NaN data
# v_df = pd.DataFrame({'x': r.v_wls[:, 0], 'y': r.v_wls[:, 1], 'z': r.v_wls[:, 2]})
# v_df = v_df.interpolate(limit_area='outside', limit_direction='both')
# v_df = v_df.interpolate('spline', order=3)
# 
# # return x_df.to_numpy(), v_df.to_numpy(), r.cov_x, r.cov_v

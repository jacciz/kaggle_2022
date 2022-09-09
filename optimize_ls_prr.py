if len(r.df_prr) >= 4:    # Normal WLS
  if np.all(r.v0 == 0):
    opt_prr = least_squares(r.prr_residuals, r.v0, r.jac_prr_residuals, args=(r.vsat, r.prr, r.x0, r.xsat_prr, r.Wv))
    v0 = opt_prr.x
            # Robust WLS for position estimation
  opt_prr = least_squares(r.prr_residuals, v0, r.jac_prr_residuals, args=(r.vsat, r.prr, r.x0, r.xsat_prr, r.Wv))
  if opt_prr.status < 1:
    print(f'i = {i} position lsq status = {opt_prr.status}')
  else: # Covariance estimation
    cov_prr = np.linalg.inv(opt_prr.jac.T @ r.Wv @ opt_prr.jac)

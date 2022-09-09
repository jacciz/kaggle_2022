if len(r.df_pr) >= 4:    # Normal WLS
  if np.all(r.x0 == 0):
    opt = least_squares(r.pr_residuals, r.x0, r.jac_pr_residuals, args=(r.xsat_pr, r.pr, r.Wx))
    x0 = opt.x
            # Robust WLS for position estimation
  opt = least_squares(r.pr_residuals, x0, r.jac_pr_residuals, args=(r.xsat_pr, r.pr, r.Wx))
  if opt.status < 1 or opt.status == 2:
    print(f'i = {i} position lsq status = {opt.status}')
  else: # Covariance estimation
    cov = np.linalg.inv(opt.jac.T @ r.Wx @ opt.jac)

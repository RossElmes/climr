#' Fit basic statistical models to climate data
#'
#' @param obj An object of class \code{climr} from \code{\link{load_clim}}
#' @param optim_method The optimisation algorithm to be used
#'
#' @return Returns a list of class \code{gp_fit} which includes the model details as well as the data set and fit type used
#' @seealso \code{\link{load_clim}}, \code{\link{plot.climr_gp_fit}}
#' @export
#' @importFrom stats "optim"
#' @importFrom mvtnorm "dmvnorm"
#' @importFrom tibble "tibble"
#'
#' @examples
#' ans1 = load_clim('SH')
#' ans2 = gp_fit(ans1, optim_method = "BFGS")
gp_fit = function(obj, optim_method = c("Nelder-Mead", "BFGS", "SANN", "Brent")) {
  UseMethod('gp_fit')
}

#' @export
gp_fit.climr = function(obj, optim_method = c("Nelder-Mead", "BFGS", "SANN", "Brent")) {

  # Create global variables to avoid annoying CRAN notes
  DJF = Dec = `J-D` = Jan = SON = Year = month = pred = quarter = temp = x = year = NULL


  gp_criterion = function(par,x,y) {

    sig_sq = exp(par[1])
    rho_sq = exp(par[2])
    tau_sq = exp(par[3])
    Mu = rep(0, length(x))
    Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
    ll = dmvnorm(y, Mu, Sigma, log = TRUE)
    return(-ll)
  }

  year = obj$clim_year$year
  temp = obj$clim_year$temp

  x = scale(obj$clim_year[,1])
  y = scale(obj$clim_year[,2])
  x_g = pretty(as.vector(x),n=100)

  x_vec = as.vector(x)
  y_vec = as.vector(y)


  #x_rep = matrix(rep(x_vec, p), ncol = p, nrow = length(x_vec))
  #X = sweep(x_rep, 2, 1:p, '^') # 1:p here ensures no intercept
  #X_g_rp = matrix(rep(x_g, p), ncol = p, nrow = length(x_g))
  #X_g = sweep(X_g_rp, 2, 1:p, '^')
  #pred  = X_g %*% solve(t(X)%*%X, t(X)%*%y_vec)

  answer = optim(rep(0, 3), gp_criterion,x=x_vec,y=y_vec, method = paste0(optim_method))

  sig_sq = exp(answer$par[1]) #Optimised values for sig_sq
  rho_sq = exp(answer$par[2]) #Optimised values for rho_sq
  tau_sq = exp(answer$par[3]) #Optimised values for tau_sq

  # Create covariance matrices
  C = sig_sq * exp( - rho_sq * outer(x_g, x_vec, '-')^2 )
  Sigma = sig_sq * exp( - rho_sq * outer(x_vec, x_vec, '-')^2 ) + tau_sq * diag(length(x_vec))

  # Now create predictions
  pred2 = C %*% solve(Sigma, y_vec)

  x_g_unscale <- as.vector(x_g)*attr(x,'scaled:scale')+attr(x, 'scaled:center')
  pred2_unscale <-as.vector(pred2)*attr(y,'scaled:scale')+attr(y, 'scaled:center')

  results <- list()

  results[["data"]] <- tibble(year,temp)

  results[["fit"]] <-tibble(x_g_unscale,pred2_unscale)

  class(results) = 'climr_gp_fit'

  invisible(results)

  }




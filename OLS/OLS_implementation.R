#OLS Implementation
#John Spaw

#Perform OLS estimation for a linear model using QR decomposition
#Input: Design matrix X, Response vector Y
#Output: List containing fitted betas, predicted values, residuals, 
#        estimated error variance, and variance-covariance matrix of fitted betas


OLS_fit <- function(X, Y) {
  n <- nrow(X)
  p <- ncol(X)
  
  #Compute QR decomposition of X ...  improves speed of computations
  qr_X <- qr(X)
  Q <- qr.Q(qr_X)
  R <- qr.R(qr_X)
  
  #Compute fitted beta values
  beta <- backsolve(qr_X$qr,qr.qty(qr_X, Y))
  
  #Compute residuals
  fitted_values <- as.vector(Q%*%R%*%beta)
  residuals <- as.vector(Y - fitted_values)
  
  #Compute error variance
    #Using residuals: 
    sigma2_hat <- sum(residuals^2)/(nrow(X) - qr_X$rank)
    
    #using QR decomposition and hat matrix representation
    sigma2_hat <- ((t(Y)%*%Y - t(Y)%*%Q%*%R%*%chol2inv(R)%*%t(R)%*%t(Q)%*%(Y))/(n-qr_X$rank))[[1]]
    
  #Compute estimate for var(beta_hat)
    var_beta_hat <- (sigma2_hat*chol2inv(R))
    
  #Combine all of the fitted quantities into a single list for output
  fit_list <- list(beta, fitted_values, residuals, sigma2_hat, var_beta_hat)
  
  return(fit_list)
}
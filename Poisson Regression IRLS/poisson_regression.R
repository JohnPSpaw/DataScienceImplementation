#Poisson Regression Implementation
#John Spaw

#Fit a poisson regression (generalized linear model) using iteratively reweighted least squares
#Input: Design matrix X, Response vector Y
#Output: List containing fitted thetas and iterations


library(boot)
data(breslow)

y <- breslow$y
X <- model.matrix(y ~ age + smoke, data = breslow)
dim(X)
poisson_regression <- function(X, y,
                               B = 1000, #max iterations
                               tol = 1e-07
                               ) {
  notdone <- TRUE
  b <- 1                      #iteration counter
  tol <- 1e-07                #tolerance for ending iteration
  theta <- rep(1,dim(X)[2])   #initial estimate
  iterates <- theta
  
  while(notdone) {
    eta <- c(X %*% theta)                 #Linear predictor for each observation
    lambda <- exp(eta)                    #mean of poisson ... log link ... inverse of log is exp
    score <- y/lambda - 1                 #score function
    
    Win <- diag(lambda)                   #inverse weight matrix (fisher scoring) ... diagonals are inverse of unit info
    D <- matrix(NA,nrow(X),ncol(X))       #dlambda_i/dtheta_j --- based on design matrix and link function
    for(i in 1:nrow(X)) {
      for(j in 1:ncol(X)) {
        ifelse(j == 1,
               D[i,j] <- exp(X[i,]%*%theta),
               D[i,j] <- X[i,j]*exp(X[i,]%*%theta)
        )
      }
    }
    
    #Weighted least squares regression
    ytilde <- D%*%theta + Win%*%score               #pseudo - response
    tmp <- lm(ytilde ~ D - 1, weights=(1/lambda))   
    theta.new <- tmp$coef
    err <- max( abs( theta-theta.new) )
    b <- b+1
    notdone <- (b < B) & (err > tol)                #stopping rule
    theta <- theta.new
    iterates <- cbind( iterates, theta )            #track iterations for reference
    output <- list(coefficients = theta, iterations = iterates)
    
  }
  return(output)
}

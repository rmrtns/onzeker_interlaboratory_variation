# Function for ordinal metrics

# Mean absolute error (MAE)
MAE <- function(vecA, VecB){
  
  MAE <- mean(abs(vecA - VecB))
  return(MAE)
}


# mean square error (MSE)
MSE <- function(vecA, VecB){
  MSE <- mean((vecA - VecB)^2)
  return(MSE)
}

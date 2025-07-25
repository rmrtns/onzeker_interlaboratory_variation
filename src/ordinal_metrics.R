# Function for ordinal metrics
# input control
is_numeric_vec <- function(...){
  inputs <- list(...)
  for(i in seq_along(inputs)){ 
    if (class(unlist(inputs[i])) == "numeric"){
       invisible(NULL)
    } else {
      stop(paste("input element ",i," is not a numeric vector"))
    }
  }
}  

# Mean absolute error (MAE)
MAE <- function(vec_true, vec_pred){
  is_numeric_vec(vec_true, vec_pred)
  return(mean(abs(vec_true - vec_pred)))
}

# Macro averaged mean absolute error (MAMAE)
MAMAE <- function(vec_true,vec_pred){
  is_numeric_vec(vec_true, vec_pred)
  
    classes <- sort(unique(vec_true))
    mae_per_class <- sapply(classes, function(cls) {
      idx <- which(vec_true == cls)
      mean(abs(vec_pred[idx] - vec_true[idx]))
    })
    return(mean(mae_per_class))
}

# mean square error (MSE)
MSE <- function(vec_true, vec_pred){
  is_numeric_vec(vec_true, vec_pred)
  return(mean((vec_true - vec_pred)^2))
}

# Macro averaged mean sqaured error (MAMAE)
MAMSE <- function(vec_true,vec_pred){
  is_numeric_vec(vec_true, vec_pred)
  
    classes <- sort(unique(vec_true))
    mse_per_class <- sapply(classes, function(cls) {
      idx <- which(vec_true == cls)
      mean((vec_pred[idx] - vec_true[idx])^2)
    })
    mean(mse_per_class)
  }
  
# root mean sqaure error (RMSE)
RMSE <- function(vec_true, vec_pred){
  is_numeric_vec(vec_true, vec_pred)
  return(sqrt(MSE(vec_true,vec_pred)))
}
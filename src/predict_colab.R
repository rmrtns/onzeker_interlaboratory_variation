# Functions to calculate the CoLab-score, either continous (linear predictor)
# ordinal (0 to 5) or binary (using a score of 5 as positive)

get_continuous_prediction_colab <- function(data, dots_arguments){
  
  RBC <- dots_arguments[["model_variables"]]["Erytrocyten_BV"]
  WBC <- dots_arguments[["model_variables"]]["Leukocyten_BV"]
  EOS <- dots_arguments[["model_variables"]]["EosAbs_BV"]
  BASO <- dots_arguments[["model_variables"]]["BasoAbs_BV"]
  BILI <- dots_arguments[["model_variables"]]["BilirubineTotaal_BV"]
  LD <- dots_arguments[["model_variables"]]["LD_BV"]
  AF <- dots_arguments[["model_variables"]]["AlkFosf_BV"]
  GGT <- dots_arguments[["model_variables"]]["GGT_BV"]
  ALB <- dots_arguments[["model_variables"]]["Albumine_BV"]
  CRP <- dots_arguments[["model_variables"]]["CRP_BV"]
  age <- dots_arguments[["model_variables"]]["age"]
  
  formula <- expression(
    -6.885000+
    data[[RBC]]*0.937900+
    data[[WBC]]*-0.129800+
    data[[EOS]]*-6.834000+
    data[[BASO]]*-47.70000+
    data[[BILI]]*-1.142000+
    data[[LD]]*5.369000+
    data[[AF]]*-3.114000+
    data[[GGT]]*0.360500+
    data[[ALB]]*-0.115600+
    data[[CRP]]*0.002560+
    data[[age]]*0.002275              
  )
  eval(formula)
}


get_ordinal_prediction_colab <- function(data, dots_arguments){
  cut_offs <- c(-5.83, -4.03, -3.29, -2.34, -1.64)
  
  cut(
    as.numeric(data[["continuous_prediction"]]),
    c(-Inf, cut_offs, Inf),
    labels = seq (0, length(cut_offs)),
    ordered_result = TRUE
  )
}


get_categorical_prediction_colab <- function(data, dots_arguments){
  as.numeric(data[["continuous_prediction"]]) >= -1.64
}


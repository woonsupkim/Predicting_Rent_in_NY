

to_factor_from_numeric <- function(current_var){
  x <- NULL
 
  if(is.numeric(current_var) && length(unique(current_var)) < 10){
    x <- as.factor(current_var)
  
  }else{
    x <- current_var
  }
  return(x)
}


to_logical_from_factor <- function(current_var){
  x <- NULL
  
  if(is.factor(current_var) && length(unique(current_var)) < 4){
    x <- as.logical(current_var)
    
  }else{
    x <- current_var
  }
  return(x)
}


char2na <- function(x) {
  x <- as.character(x)
  return(case_when(
    x == "" ~ NA_character_,
    x == "N/A" ~ NA_character_,
    TRUE ~ x
  ))
}


remove_columns <- function(current_var){
  x <- NULL
  
  if(is.factor(current_var) && length(levels(current_var)) < 1000){
    x <- current_var
    
  }else if(is.factor(current_var)){
    # x <- current_var
    }
  else{
    x <- current_var
  }
  return(x)
}

remove_nzv_columns <- function(current_var){
  x <- NULL
  if(nearZeroVar(current_var,saveMetrics = T)[4] == 'FALSE'){
    x <- current_var
  }else{
    
  }
  return(x)
}



remove_multi_cor <- function(df){
  x = NULL
  for (i in 1:ncol(df)){
    if(sum(df[,i] >= 0.9) >= 2)
    {
      x = append(x,i)
    }
  }
  return(x)
}



remove_multi_cor <- function(df){
  x = NULL
  for (i in 1:ncol(df)){
  if(sum(df[,i] >= 0.9) >= 2)
  {
    x = append(x,i)
  }
  }
  return(x)
}


remove_fact_1lvl <- function(current_var){
  x <- NULL
  
  if(is.factor(current_var) && length(levels(current_var)) > 1){
    x <- current_var
  }else if(is.factor(current_var)){
  }
  else{
    x <- current_var
  }
  return(x)
}


outlier_norm <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  return(x)
}

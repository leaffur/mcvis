cleanDigits = function(df, sig = 2){
  res = dplyr::mutate_if(.tbl = df,
                         is.double,
                         dplyr::funs(signif), digits = sig)
  return(res)
}

rangeTransform = function(x){
  if(min(x) == max(x)){
    return(0)
  } else {
    return((x - min(x)) / (max(x) - min(x)))
  }
}

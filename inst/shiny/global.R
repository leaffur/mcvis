cleanDigits = function(df, sig = 2){
  res = dplyr::mutate_if(.tbl = df,
                         is.double,
                         dplyr::funs(signif), digits = sig)
  return(res)
}

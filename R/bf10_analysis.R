#' Bayes Factor Test with Sensitivity Check
#' 
#' Tests data against three priors (hard-coded).
#' 
#' @param data data frame to be analysed
#' @param vars list of variables
#' 
#' @return Returns a tibble
#' @export

bf10_analysis <- function(data, vars) {
  
  bf10 <- purrr::map_dfr(vars, function(var) {
    P1_col <- paste0(var, "_1")
    P2_col <- paste0(var, "_2")
    
    valid_data <- data |>
      dplyr::select(tidyselect::all_of(c(P1_col, P2_col))) |>
      tidyr::drop_na()
    
    n_obs <- nrow(valid_data)
    if (n_obs<2) return (NULL)
    
    bf_narrow <- BayesFactor::ttestBF(
      x = valid_data[[P2_col]],
      y = valid_data[[P1_col]],
      paired = TRUE,
      rscale = 0.354
    )
    
    bf_default <- BayesFactor::ttestBF(
      x = valid_data[[P2_col]],
      y = valid_data[[P1_col]],
      paired = TRUE
    )
    
    bf_ultrawide <- BayesFactor::ttestBF(
      x = valid_data[[P2_col]],
      y = valid_data[[P1_col]],
      paired = TRUE,
      rscale = "ultrawide"
    )
    
    narrow_df <- BayesFactor::extractBF(bf_narrow)
    default_df <- BayesFactor::extractBF(bf_default)
    ultrawide_df <- BayesFactor::extractBF(bf_ultrawide)
    
    tibble::tibble(
      variable = var,
      n = nrow(valid_data),
      bf10_small = round(narrow_df$bf, 3),
      error_s = round(narrow_df$error, 5),
      bf10_medium = round(default_df$bf, 3),
      error_me = round(default_df$error, 5),
      bf10_massive = round(ultrawide_df$bf, 3),
      error_ma = round(ultrawide_df$error, 5)
    )
  })
  
  return(bf10)
}
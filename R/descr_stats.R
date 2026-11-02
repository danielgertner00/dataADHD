#' Descriptive Statistics
#' 
#' @param data data frame to be analysed
#' @param vars list of variables
#' 
#' @return Returns a tibble
#' @export

descr_stats <- function(data, vars) {
  
  descr <- purrr::map_dfr(vars, function(var) {
    P1_col <- paste0(var, "_1")
    P2_col <- paste0(var, "_2")
    
    valid_data <- data |> 
      dplyr::select(tidyselect::all_of(c(P1_col, P2_col))) |> 
      tidyr::drop_na()
    
    n_obs <- nrow(valid_data)
    if (n_obs<2) return (NULL)
    
    stats <- psych::describe(valid_data)
    
    tibble::tibble(
      variable = var,
      session = c("Session 1", "Session 2"),
      n = stats$n,
      mean = round(stats$mean, 2),
      sd = round(stats$sd, 2),
      min = stats$min,
      max = stats$max,
      skewness = round(stats$skew, 3),
      kurtosis = round(stats$kurtosis, 3),
      SE = round(stats$se, 3)
    ) |> 
    tidyr::pivot_wider(
      names_from = session,
      values_from = c(n, mean, sd, min, max, skewness, kurtosis, SE),
      names_sep = "_"
    ) |> 
      dplyr::select(variable, tidyselect::ends_with("Session 1"), tidyselect::ends_with("Session 2"))
  })
  
  return(descr)
}
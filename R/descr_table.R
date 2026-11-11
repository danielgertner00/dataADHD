#' Descriptive Statistics APA Table
#' 
#' Generates an APA table of descriptive statistics
#' 
#' @param descr data frame to be analysed
#' @param name_map mapping for variable names as they're supposed to appear on the plot
#' 
#' @return Returns a flextable
#' @export

descr_table <- function(descr, name_map) {
  
  descr <- descr |> dplyr::select(-`n_Session 2`) |> 
    dplyr::mutate(spacer = "") |> 
    dplyr::relocate(spacer, .before = `mean_Session 2`)
  
  bl_border <- officer::fp_border(width = 1, color = "black")
  
  descr$variable <- ifelse(descr$variable %in% names(name_map), name_map[descr$variable], descr$variable)
  
  descr_table <- flextable::flextable(descr) |> 
    flextable::theme_apa() |> 
    flextable::set_header_labels(
      variable = "Variable",
      `n_Session 1` = "n",
      `mean_Session 1` = "M",
      `sd_Session 1` = "SD",
      `min_Session 1` = "Min",
      `max_Session 1` = "Max",
      `skewness_Session 1` = "Skew",
      `kurtosis_Session 1` = "Kurtosis",
      `SE_Session 1` = "SE",
      
      spacer = "",
      
      `mean_Session 2` = "M",
      `sd_Session 2` = "SD",
      `min_Session 2` = "Min",
      `max_Session 2` = "Max",
      `skewness_Session 2` = "Skew",
      `kurtosis_Session 2` = "Kurtosis",
      `SE_Session 2` = "SE"
    ) |> 
    flextable::add_header_row(
      values = c("Variable", "n", "Session 1", "", "Session 2"),
      colwidths = c(1, 1, 7, 1, 7)
    ) |> 
    
    flextable::italic(j = "n_Session 1", part = "header") |> 
    flextable::colformat_num(j = "n_Session 1", digits = 0) |> 
    
    flextable::merge_v(part = "header") |> 
    
    flextable::hline(i = 1, part = "header", border = officer::fp_border(width = 0)) |> 
    flextable::hline(i = 1, j = 3:9, part = "header", border = bl_border) |> 
    flextable::hline(i = 1, j = 11:17, part = "header", border = bl_border) |> 
    
    flextable::italic(i = 2, j = c(3, 4, 9, 11, 12, 17), part = "header") |> 
    flextable::font(fontname = "Times New Roman", part = "all") |> 
    flextable::fontsize(size = 11, part = "all") |>
    flextable::valign(valign = "bottom", "part" = "header") |> 
    
    flextable::align(j = 1, align = "left", part = "all") |> 
    flextable::bg(bg = "white", part = "all") |> 
    flextable::autofit() 
  
  return(descr_table)
}
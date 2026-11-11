#' Monte Carlo Permutation Test APA Table
#'
#' Generates an APA table of MCPT results
#'
#' @param mcpt data frame to be analysed
#' @param name_map mapping for variable names as they're supposed to appear on the plot
#'
#' @return Returns a flextable
#' @export
mcpt_table <- function(mcpt, name_map) {

  mcpt$variable <- ifelse(mcpt$variable %in% names(name_map), name_map[mcpt$variable], mcpt$variable)

  mcpt$p_value_mc <- paste0(
    sprintf("%.3f", mcpt$p_value_mc),
    ifelse(mcpt$p_value_mc < 0.01, "**", ifelse(mcpt$p_value_mc < 0.05, "*", ""))
  )

  mcpt_tbl <- flextable::flextable(mcpt) |>
    flextable::theme_apa() |>
    flextable::set_header_labels(
      variable = "Variable",
      estimate = "Estimate",
      statistic = "Statistic",
      p_value_mc = "p"
    ) |>
    flextable::align(j = "variable", align = "left", part = "all") |>
    flextable::italic(j = c("n", "p_value_mc"), part = "header") |>
    flextable::colformat_num(j = "n", digits = 0) |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::bg(bg = "white", part = "all") |>
    flextable::autofit()

  return(mcpt_tbl)
}

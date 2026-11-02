#' Monte Carlo Permutation Test APA Table
#'
#' Generates an APA table of MCPT results
#'
#' @param mcpt data frame to be analysed
#' @param name_map mapping for variable names as they're supposed to appear on the plot
#' @param table_num string, title header row, eg. "Table 1."
#' @param abbreviations string, listing abbreviations as they're supposed to appear in the footer of the table, eg. "Abbreviations. MCPT - Monte Carlo Permutation Test."
#'
#' @return Returns a flextable
#' @export
mcpt_table <- function(mcpt, name_map, table_num, abbreviations) {

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
    flextable::add_header_lines(c(table_num, "Monte Carlo Permutation Tests for Variables After Intervention.")) |>
    flextable::add_footer_lines(c("Note.", "\u2020 Standardized test scores.", "", abbreviations)) |>
    flextable::compose(
      i = 3, j = 1, part = "footer",
      value = flextable::as_paragraph("*", flextable::as_i("p"), " < 0.05; **", flextable::as_i("p"), " < 0.01.")
    ) |>
    flextable::bold(i = 1, part = "header") |>
    flextable::italic(i = 2, part = "header") |>
    flextable::align(i = 1:2, align = "left", part = "header") |>
    flextable::italic(i = 1, part = "footer") |>
    flextable::align(part = "footer", align = "left") |>
    flextable::hline(i = 1, part = "header", border = officer::fp_border(width = 0)) |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::bg(bg = "white", part = "all") |>
    flextable::autofit()

  return(mcpt_tbl)
}

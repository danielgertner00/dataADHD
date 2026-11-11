#' Bayes Estimation Analysis APA Table
#'
#' Generates an APA table of Bayes estimation results
#'
#' @param b_estim data frame to be analysed
#' @param name_map mapping for variable names as they're supposed to appear on the plot
#'
#' @return Returns a flextable
#' @export

estimB_table <- function(b_estim, name_map) {
  b_estim$variable <- ifelse(
    b_estim$variable %in% names(name_map),
    name_map[b_estim$variable],
    b_estim$variable
  )

  b_estim$cohens_d <- paste0(
    sprintf("%.3f", b_estim$cohens_d),
    ifelse(
      (b_estim$cohens_d > 0.2 | b_estim$cohens_d < -0.2) &
        (b_estim$hdi_lower > 0 &
          b_estim$hdi_upper > 0 |
          b_estim$hdi_lower < 0 & b_estim$hdi_upper < 0),
      "**",
      ifelse(
        b_estim$hdi_lower > 0 &
          b_estim$hdi_upper > 0 |
          b_estim$hdi_lower < 0 & b_estim$hdi_upper < 0,
        "*",
        ""
      )
    )
  )

  b_estim_table <- flextable::flextable(b_estim) |>
    flextable::theme_apa() |>
    flextable::set_header_labels(
      variable = "Variable",
      mean_diff = "Mean Difference",
      hdi_lower = "HDI \u2193",
      hdi_upper = "HDI \u2191"
    ) |>
    flextable::compose(
      j = "cohens_d",
      part = "header",
      value = flextable::as_paragraph("Cohen's ", flextable::as_i("d"))
    ) |>
    flextable::align(j = "variable", align = "left", part = "all") |>
    flextable::italic(j = "n", part = "header") |>
    flextable::colformat_num(j = "n", digits = 0) |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::bg(bg = "white", part = "all") |>
    flextable::autofit()

  return(b_estim_table)
}

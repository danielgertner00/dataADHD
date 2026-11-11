#' Bayes Factor with Sensitivity Analysis APA Table
#'
#' Generates an APA table of BF10 results
#'
#' @param bf10 data frame to be analysed
#' @param name_map mapping for variable names as they're supposed to appear on the plot
#'
#' @return Returns a flextable
#' @export

bf10_table <- function(bf10, name_map) {
  bf10$variable <- ifelse(
    bf10$variable %in% names(name_map),
    name_map[bf10$variable],
    bf10$variable
  )

  bf10$bf10_small <- paste0(
    sprintf("%.1f", bf10$bf10_small),
    ifelse(
      bf10$bf10_small > 1 & bf10$bf10_small <= 3,
      "*",
      ifelse(
        bf10$bf10_small > 3 & bf10$bf10_small <= 10,
        "**",
        ifelse(
          bf10$bf10_small > 10 & bf10$bf10_small <= 30,
          "***",
          ifelse(bf10$bf10_small > 30, "****", "")
        )
      )
    )
  )

  bf10$bf10_medium <- paste0(
    sprintf("%.1f", bf10$bf10_medium),
    ifelse(
      bf10$bf10_medium > 1 & bf10$bf10_medium <= 3,
      "*",
      ifelse(
        bf10$bf10_medium > 3 & bf10$bf10_medium <= 10,
        "**",
        ifelse(
          bf10$bf10_medium > 10 & bf10$bf10_medium <= 30,
          "***",
          ifelse(bf10$bf10_medium > 30, "****", "")
        )
      )
    )
  )

  bf10$bf10_massive <- paste0(
    sprintf("%.1f", bf10$bf10_massive),
    ifelse(
      bf10$bf10_massive > 1 & bf10$bf10_massive <= 3,
      "*",
      ifelse(
        bf10$bf10_massive > 3 & bf10$bf10_massive <= 10,
        "**",
        ifelse(
          bf10$bf10_massive > 10 & bf10$bf10_massive <= 30,
          "***",
          ifelse(bf10$bf10_massive > 30, "****", "")
        )
      )
    )
  )

  bf10$error_s <- ifelse(
    bf10$error_s < 0.001,
    "<0.001",
    sprintf("%.3f", bf10$error_s)
  )

  bf10$error_me <- ifelse(
    bf10$error_s < 0.001,
    "<0.001",
    sprintf("%.3f", bf10$error_s)
  )

  bf10$error_ma <- ifelse(
    bf10$error_s < 0.001,
    "<0.001",
    sprintf("%.3f", bf10$error_s)
  )

  bf10_table <- flextable::flextable(bf10) |>
    flextable::theme_apa() |>
    flextable::set_header_labels(
      variable = "Variable",
      error_s = "Error",
      error_me = "Error",
      error_ma = "Error"
    ) |>
    flextable::compose(
      j = "bf10_small",
      part = "header",
      value = flextable::as_paragraph(
        "BF",
        flextable::as_sub("10"),
        " small",
        flextable::as_sup("a")
      )
    ) |>
    flextable::compose(
      j = "bf10_medium",
      part = "header",
      value = flextable::as_paragraph(
        "BF",
        flextable::as_sub("10"),
        " medium",
        flextable::as_sup("b")
      )
    ) |>
    flextable::compose(
      j = "bf10_massive",
      part = "header",
      value = flextable::as_paragraph(
        "BF",
        flextable::as_sub("10"),
        " large",
        flextable::as_sup("c")
      )
    ) |>
    flextable::align(j = "variable", align = "left", part = "all") |>
    flextable::italic(j = "n", part = "header") |>
    flextable::colformat_num(j = "n", digits = 0) |>
    flextable::add_footer_lines(c(
      "Note. ",
      "*BF between 1 and 3 - anecdotal evidence for alternative hypothesis; **BF between 3 and 10 - moderate evidence for alternative hypothesis; ***BF between 10 and 30 - strong evidence for alternative hypothesis.",
      ""
    )) |>
    flextable::compose(
      i = 3,
      j = 1,
      part = "footer",
      value = flextable::as_paragraph(
        flextable::as_sup("a"),
        " Small prior - 0.354; ",
        flextable::as_sup("b"),
        " Medium prior - 0.707; ",
        flextable::as_sup("c"),
        " Very large prior - 1.414."
      )
    ) |>
    flextable::italic(i = 1, part = "footer") |>
    flextable::align(part = "footer", align = "left") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::bg(bg = "white", part = "all") |>
    flextable::autofit()

  return(bf10_table)
}

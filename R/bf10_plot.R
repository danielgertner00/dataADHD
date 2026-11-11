#' Bayes Factor with Sensitivity Analysis Plot
#'
#' Generates a comparison plot between variables and different priors
#'
#' @param bf10 data frame to be analysed
#' @param name_map mapping for variable names as they're supposed to appear on the plot
#'
#' @return Returns a ggplot2 object
#' @export
bf10_plot <- function(bf10, name_map) {

  bf10 <- bf10 |> dplyr::select(variable, bf10_small, bf10_medium, bf10_massive) |> tidyr::pivot_longer(
    cols = tidyselect::starts_with("bf10_"),
    names_to = "prior",
    values_to = "bf_value"
  ) |>
    dplyr::mutate(
      prior = stringr::str_remove(prior, "bf10_"),
      prior = ifelse(prior == "massive", "large", prior),
      prior = factor(prior, levels = c("small", "medium", "large")),
      variable = ifelse(variable %in% names(name_map), name_map[variable], variable)
    )

  bf10_plot <- ggplot2::ggplot(bf10, ggplot2::aes(x = prior, y = bf_value, group = variable, color = variable)) +
    ggplot2::geom_hline(yintercept = c(1, 3, 10, 30), linetype = "dotted", color = "black", alpha = 0.8) +

    ggplot2::geom_line(linewidth = 1, alpha = 0.8) +
    ggplot2::geom_point(size = 3) +

    ggplot2::facet_wrap(~variable, scales = "free", ncol = 2) +

    ggplot2::scale_y_log10(breaks = c(1, 2, 3, 5, 10, 20, 30)) +

    ggplot2::scale_color_viridis_d(option = "turbo") +

    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Prior Scale",
      y = expression(BF[10]),
      color = "Variable"
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif", color = "black"),

      strip.text = ggplot2::element_text(size = 10),
      strip.text.x = ggtext::element_textbox_simple(
        size = 8,
        margin = ggplot2::margin(t = 5, b = 5),
        halign = 0.5,
        hjust = 0.5,
        lineheight = 1.2,
        face = "bold"
      ),

      plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
      plot.subtitle = ggplot2::element_text(face = "italic", size = 12, hjust = 0, margin = ggplot2::margin(b = 15)),

      plot.caption = ggtext::element_textbox_simple(hjust = 0, size = 11, margin = ggplot2::margin(t = 15), lineheight = 1.2),
      plot.caption.position = "plot",

      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 9, color = "black"),
      axis.title = ggplot2::element_text(size = 11),

      legend.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "right"
    )
  return(bf10_plot)
}

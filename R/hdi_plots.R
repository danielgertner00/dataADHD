#' Highest Density Interval Plots
#'
#' Generates plots
#'
#' @param df data frame to be analysed
#' @param name_map mapping for variable names as they're supposed to appear on the plot
#'
#' @return Returns a ggplot2 object
#' @export

hdi_plots <- function(df, name_map) {
  df <- df |>
    dplyr::mutate(
      variable = ifelse(
        variable %in% names(name_map),
        name_map[variable],
        variable
      )
    )

  df_plot <- df |>
    ggplot2::ggplot(ggplot2::aes(x = mu_samples)) +
    ggplot2::geom_density(
      fill = "#34495e",
      alpha = 0.4,
      color = "#2c3e50",
      linewidth = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "darkred",
      linewidth = 0.6
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = hdi_lower, xend = hdi_upper, y = 0, yend = 0),
      color = "black",
      linewidth = 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = mean_diff, y = 0),
      color = "white",
      fill = "black",
      shape = 21,
      size = 2,
      stroke = 1
    ) +
    ggplot2::facet_wrap(~variable, scales = "free", ncol = 2) + # CHANGED FROM 3 TO 2 FOR HANIA
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = expression(Mean ~ Difference ~ (M[diff]) ~ with ~ 95 ~ "%" ~ HDI),
      y = "Probability Density") +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif", color = "black"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 9),
      axis.ticks.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      )
    )

  return(df_plot)
}

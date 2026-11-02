#' Highest Density Interval Plots
#'
#' Generates plots
#'
#' @param df data frame to be analysed
#' @param name_map mapping for variable names as they're supposed to appear on the plot
#' @param fig_num string, title header row, ex. "Figure 1."
#'
#' @return Returns a ggplot2 object
#' @export

hdi_plots <- function(df, name_map, fig_num) {

  df <- df |> dplyr::mutate(
    variable = ifelse(variable %in% names(name_map), name_map[variable], variable)
  )

  df_plot <- df |> ggplot2::ggplot(ggplot2::aes(x = mu_samples)) +
    ggplot2::geom_density(fill = "#34495e", alpha = 0.4, color = "#2c3e50", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", linewidth = 0.6) +
    ggplot2::geom_segment(
      ggplot2::aes(x = hdi_lower, xend = hdi_upper, y = 0, yend = 0),
      color = "black", linewidth = 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = mean_diff, y = 0),
      color = "white", fill = "black", shape = 21, size = 2, stroke = 1
    ) +
    ggplot2::facet_wrap(~variable, scales = "free", ncol = 2) + # CHANGED FROM 3 TO 2 FOR HANIA
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = expression(Mean ~ Difference ~ (M[diff]) ~ with ~ 95 ~ "%" ~ HDI),
      y = "Probability Density",
      title = fig_num,
      subtitle = "Posterior Distribution of Mean Difference with Highest Density Interval by Variable of Interest.",
      caption = "<i>Note.</i> Shaded regions represent the posterior distributions. Black horizontal bars at the base indicate the 95% probability Highest Density Intervals (HDIs) with circles marking the posterior means. Red dashed lines denote the point of zero effect. <br>\u2020 Standardized test scores. <br> Abbreviations: HDI - Highest Density Interval."
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif", color = "black"),
      plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
      plot.subtitle = ggplot2::element_text(face = "italic", size = 12, hjust = 0),
      plot.caption = ggtext::element_textbox_simple(hjust = 0, size = 10, margin = ggplot2::margin(t = 15), lineheight = 1.2),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 9),
      axis.ticks.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5)
    )

  return(df_plot)
}

#' Bayes Estimation Analysis with Highest Density Interval
#'
#' @param data data frame to be analysed
#' @param vars list of variables
#'
#' @return Returns a list with two tibbles - one is estimation results (for table), the other is estimation samples (for plot).
#' @export

estimB_analysis <- function(data, vars) {

  estim_samples <- purrr::map_dfr(vars, function(var) {
    P1_col <- paste0(var, "_1")
    P2_col <- paste0(var, "_2")

    valid_data <- data |>
      dplyr::select(tidyselect::all_of(c(P1_col, P2_col))) |>
      tidyr::drop_na()

    n_obs <- nrow(valid_data)
    if (n_obs<2) return (NULL)

    fit <- BayesianFirstAid::bayes.t.test(valid_data[[P2_col]], valid_data[[P1_col]], paired = TRUE)

    mcmc_matrix <- as.matrix(fit$mcmc)
    post_df <- as.data.frame(mcmc_matrix)

    mu_samples <- post_df$mu

    diff_scores <- valid_data[[P2_col]] - valid_data[[P1_col]]
    dz <- mean(diff_scores) / stats::sd(diff_scores)

    tibble::tibble(
      variable = var,
      n = nrow(valid_data),
      mean_diff = round(mean(mu_samples), 3),
      hdi_lower = round(stats::quantile(mu_samples, 0.025), 3),
      hdi_upper = round(stats::quantile(mu_samples, 0.975), 3),
      cohens_d = round(dz, 3),
      mu_samples = mu_samples
    )
  })

  b_estim <- estim_samples |> dplyr::select(-mu_samples) |>
    dplyr::distinct()

  return(list(b_estim = b_estim, estim_samples = estim_samples))
}

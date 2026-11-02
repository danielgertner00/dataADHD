#' Monte Carlo Permutation Test
#'
#' @param data data frame to be analysed
#' @param vars list of variables
#' @param seed for randomness; DEFAULT seed = 42
#' @param n_sim number of simulations; DEFAULT = 5000
#'
#' @return Returns a tibble
#' @export



mcpt_analysis <- function(data, vars, seed=42, n_sim=5000) {

  set.seed(seed)

  mcpt_r <- purrr::map_dfr(vars, function(var) {
    P1_col <- paste0(var, "_1")
    P2_col <- paste0(var, "_2")

    diff_scores <- stats::na.omit(data[[P2_col]] - data[[P1_col]])
    n_obs <- length(diff_scores)

    if (n_obs <2) return(NULL)

    std_test <- stats::t.test(diff_scores)
    obs_t <- std_test$statistic

    sim_t_stats <- replicate(n_sim, {
      random_signs <- sample(c(-1, 1), n_obs, replace = TRUE)
      sim_diffs <- diff_scores * random_signs

      mean(sim_diffs) / (stats::sd(sim_diffs) / sqrt(n_obs))
    })

    mc_p <- mean(abs(sim_t_stats) >= abs(obs_t))

    tibble::tibble(
      variable = var,
      n = n_obs,
      estimate = mean(diff_scores),
      statistic = obs_t,
      p_value_mc = mc_p
    )
  })

  mcpt_r <- mcpt_r |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(., 3)))

  return(mcpt_r)
}

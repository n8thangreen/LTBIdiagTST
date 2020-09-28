
#' Probability sensitivity analysis for initial population
#'
#' heemod Markov model taking decision tree subpopulations
#' as input.
#' Make sure inits in correct order.
#'
heemod_init_pop_PSA <- function(heemod_model,
                                init_states) {
  res_mod <- list()

  for (i in seq_len(nrow(init_states))) {

    res_mod[[i]] <-
      heemod_model(
        unname(unlist(init_states[i, ])))
  }

  res_mod
}

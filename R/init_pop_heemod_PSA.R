
#' Probability sensitivity analysis for initial population
#'
#' heemod Markov model taking decision tree sub-populations
#' as input.
#' Make sure inits in correct order.
#'
#' @param heemod_model function to run heemod with parameters set
#' @param init_states data.frame of scenario starting state population
#'                    scenario as rows
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


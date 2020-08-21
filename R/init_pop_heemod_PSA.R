
#'
init_pop_heemod_PSA <- function(heemod_model,
                                init_states,
                                N = 1) {
  res_mod <- list()
  model <- heemod_model()

  for (i in 1:nrow(init_states)) {

    res_mod[[i]] <-
      suppressMessages(
        run_model(
          init = N * init_states[i, ],  # population sizes
          method = "end",
          model$strat,
          parameters = model$param,
          cycles = 66,
          cost = model$cost,
          effect = model$utility
        ))
  }

  res_mod
}

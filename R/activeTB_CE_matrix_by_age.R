
#' costeff_matrix
#'
#' This replicates the population active TB calculation
#' in Manabu's Excel model.
#'
#' @param start_age single value starting age in years
#' @param n_pop initial total population size
#' @param n_LTBI initial number of LTBI cases
#' @param QALY0 startup QALY
#' @param cost0 startup cost
#'
#' @return dataframe
#' @export
#'
costeff_matrix <- function(start_age = 34,
                           n_pop = 1000,
                           n_LTBI = 105.739,
                           QALY0 = 924.77849, #0,
                           cost0 = 17051.77) { #0) {

  lifetable <- readr::read_csv(here::here("lifetable.csv"))

  active_tb_QoL <- 0.85
  active_tb_cost <- 5329

  dat <-
    lifetable %>%
    dplyr::filter(age >= start_age) %>%
    mutate(year = seq_len(n()) - 1)

  # initial conditions
  dat <-
    data.frame(dat,
               active_tb = 0,
               LTBI = n_LTBI,
               LTBI_free = n_pop - n_LTBI,
               death = 0,
               QALY = QALY0,
               cost = cost0)

  for (i in seq_len(nrow(dat))[-1]) {

    dat$active_tb[i] <- dat$LTBI[i - 1]*dat$pProgTB[i]*(1 - dat$pDeath_mean[i - 1])
    dat$LTBI[i] <- dat$LTBI[i - 1]*(1 - dat$pProgTB[i])*(1 - dat$pDeath_mean[i - 1])
    dat$LTBI_free[i] <-
      dat$LTBI_free[i - 1]*(1 - dat$pDeath_mean[i - 1]) +
      dat$active_tb[i - 1]*(1 - dat$pDeath_TB[i - 1])
  }

  dat <-
    dat %>%
    mutate(
      death = (LTBI_free + LTBI)*pDeath_mean + active_tb*pmin(1, pDeath_mean + pDeath_TB),
      QALY  = ifelse(year == 0, QALY, (LTBI_free + LTBI + active_tb*active_tb_QoL)*QOL_age),
      cost  = ifelse(year == 0, cost, active_tb*active_tb_cost),
      QALY_disc = QALY/(1 + 0.035)^year,
      cost_disc = cost/(1 + 0.035)^year)
}


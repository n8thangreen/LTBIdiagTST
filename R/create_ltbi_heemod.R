
#' Create LTBI heemod model
#'
#' Create a heemod Markov model for the LTBI test population.
#' Transitions happen at the beginning of each year (equivalent to transition happening at
#' the end + ignoring the first year) with `method = "beginning"`.
#' Since with this method the first year is actually the second,
#' costs should be discounted from the start with the argument `first = TRUE` in `discount()`.
#' See: (https://cran.r-project.org/web/packages/heemod/vignettes/d_non_homogeneous.html)
#'
#' @param age_init Starting age
#' @param pReact_comp TB after completed LTBI treatment
#' @param pReact_incomp TB after LTBI treatment drop-out
#' @param pReactB TB after no treatment
#' @param TB_cost Cost of TB treatment (£)
#' @param d Discount factor
#' @param N Number of runs; integer
#'
#' @return Returns a function with initial sub-populations as input.
#' @import heemod purrr dplyr
#'
#' @export
#'
#' @examples
#'
#' create_ltbi_heemod()
#'
create_ltbi_heemod <- function(age_init = 34,
                               pReact_comp = 0.0006779,
                               pReact_incomp = 0.0015301,
                               pReact = 0.0019369,
                               TB_cost = 4925.76,
                               d = 0.035,
                               N = 1) {
  # age-dependent probability of death, TB and QoL weighting
  pdeath_QoL <-
    read.csv(here::here("raw data", "pdeath_QoL.csv"))

  # define heemod model parameters
  param <- define_parameters(
    age_init = !!age_init,
    # age = age_init + markov_cycle,   # increment age annually
    age = !!age_init + model_time,   # increment age annually
    pReact_comp = !!pReact_comp,
    pReact_incomp = !!pReact_incomp,
    pReact = !!pReact,
    TB_cost = !!TB_cost,
    d = !!d,

    # match prob death to age
    pdeath = look_up(data = pdeath_QoL,
                     value = "pDeath",
                     age = age),
    pdeathTB = look_up(data = pdeath_QoL,
                       value = "pDeath_TB",
                       age = age),

    # match QoL weight to age
    QoL = look_up(data = pdeath_QoL,
                  value = "QOL_weight",
                  age = age)
  )

  # create transition matrix
  mat_trans <- define_transition(
    state_names = c(
      "noLTBI",
      "completeTx",
      "incompleteTx",
      "noTx",
      "activeTB",
      "dead"),

    # from-to probability matrix
    # C represent complements
    C, 0, 0, 0, 0,             pdeath,
    0, C, 0, 0, pReact_comp,   pdeath,
    0, 0, C, 0, pReact_incomp, pdeath,
    0, 0, 0, C, pReact,        pdeath,
    C, 0, 0, 0, 0,             pdeathTB,
    0, 0, 0, 0, 0,             1
  )

  # define cost and utility values associated with each state

  noLTBI <- define_state(
    cost = 0,
    utility = discount(QoL, d, first = TRUE)
  )

  completeTx <- define_state(
    cost = 0,
    utility = discount(QoL, d, first = TRUE)
  )

  incompleteTx <- define_state(
    cost = 0,
    utility = discount(QoL, d, first = TRUE)
  )

  noTx <- define_state(
    cost = 0,
    utility = discount(QoL, d, first = TRUE)
  )

  activeTB <- define_state(
    cost = discount(TB_cost, d, first = TRUE),
    utility = discount(QoL - 0.15, d, first = TRUE)
  )

  dead <- define_state(
    cost = 0,
    utility = 0
  )

  # combine all of the model elements to form
  # a 'strategy' consisting of a transition
  # matrix and states states with properties attached
  strat <- define_strategy(
    transition = mat_trans,
    noLTBI = noLTBI,
    completeTx = completeTx,
    incompleteTx = incompleteTx,
    noTx = noTx,
    activeTB = activeTB,
    dead = dead
  )

  save(strat, param, mat_trans, noLTBI, completeTx,
       incompleteTx, noTx, activeTB, dead,
       file = here::here("data", "ltbi_heemod.RData"))

  #
  function(init_states) {

    suppressMessages(
      run_model(
        init = N * init_states,  # population sizes
        method = "end",
        strat,
        parameters = param,
        cycles = 65,
        cost = cost,
        effect = utility
      ))
  }
}


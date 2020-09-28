
#' run_cedectree
#'
#' @param tree_dat
#' @param label_probs_distns
#' @param label_costs_distns
#' @param label_health_distns
#' @param state_list
#' @param n
#'
#' @return
#' @export
#'
run_cedectree <- function(tree_dat,
                          label_probs_distns = NULL,
                          label_costs_distns = NULL,
                          label_health_distns = NULL,
                          state_list = NULL,
                          n = 100) {

  cost <-
    tree_dat %>%
    rename(val = cost) %>%
    select(-name.health, -health) %>%
    dectree(label_probs_distns,
            label_costs_distns,
            state_list,
            n = n)

  health <-
    tree_dat %>%
    rename(val = health) %>%
    select(-name.cost, -cost) %>%
    dectree(label_probs_distns,
            label_health_distns,
            state_list,
            n = n)

  list(cost = cost,
       health = health)
}

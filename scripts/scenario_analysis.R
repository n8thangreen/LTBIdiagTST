# scenario analysis

library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(CEdecisiontree)


load(here::here("data", "params.RData")) #create_param_values()
load(here::here("data", "trees.RData"))  #create_trees()

scenario_vals <- readr::read_csv("inst/extdata/scenario_input_values.csv")

state_list <-
  list(
    no_LTBI  = c(16, 18, 19, 21, 26, 29),
    LTBI_complete_Tx  = 9,
    LTBI_incomplete_Tx = c(7, 10),
    LTBI_no_Tx = c(12, 24, 28),
    activeTB = c(),
    dead = c())

ev <- NULL

for (i in 1:nrow(scenario_vals)) {

  new_vals <- as.list(scenario_vals[i, -1])

  tree_dat <-
    create_ce_tree_long_df(
      tree_list = QFT_tree,
      label_probs = modifyList(label_probs, new_vals),
      label_costs = modifyList(label_costs, new_vals),
      label_health = label_health,
      pname_from_to = QFT_pname_from_to,
      cname_from_to = QFT_cname_from_to,
      hname_from_to = QFT_hname_from_to)

  res <- run_cedectree(tree_dat, state_list)

  ev <- rbind(ev, c(res$cost$ev_point[1],
                    res$health$ev_point[1]))

  heemod_model <-
    create_ltbi_heemod(pReact_comp = new_vals$pReact_comp,
                       pReact_incomp = new_vals$pReact_incomp,
                       pReact = new_vals$pReact,
                       TB_cost = new_vals$TB_cost)

  res_heemod <-
    heemod_model(
      unname(unlist(res$cost$term_pop_point)))
}


ev

netbenefit <- -30000*ev[, 2] - ev[, 1]

write.csv(tree_dat, file = "data/tree_dat_QFT_scenario.csv")
save(dt, file = "data/run_cedectree_QFT_scenario.RData")








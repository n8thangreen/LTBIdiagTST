# scenario analysis

library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(CEdecisiontree)


load(here::here("data", "params.RData")) #create_param_values()
load(here::here("data", "trees.RData"))  #create_trees()

scenario_vals <- read_csv("inst/extdata/scenario_input_values.csv")

state_list <-
  list(
    no_LTBI  = c(16, 18, 19, 21, 26, 29),
    LTBI_complete_Tx  = 9,
    LTBI_incomplete_Tx = c(7, 10),
    LTBI_no_Tx = c(12, 24, 28),
    activeTB = c(),
    dead = c())

#
for (i in 1:nrow(scenario_vals)) {

  new_vals <- as.list(scenario_input_values[i, -1])

  tree_dat <-
    create_ce_tree_long_df(
      tree_list = QFT_tree,
      label_probs = modifyList(label_probs, new_vals),
      label_costs = modifyList(label_costs, new_vals),
      label_health = label_health,
      pname_from_to = QFT_pname_from_to,
      cname_from_to = QFT_cname_from_to,
      hname_from_to = QFT_hname_from_to)

  dt[[i]] <-
    run_cedectree(tree_dat, state_list)
}


write.csv(tree_dat, file = "data/tree_dat_QFT_scenario.csv")
save(dt, file = "data/run_cedectree_QFT_scenario.RData")








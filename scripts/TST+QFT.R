
# test strategy:
# dual tests (TST followed by IGRA QFT)


library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(CEdecisiontree)


load(here::here("data", "params.RData")) #create_param_values()
load(here::here("data", "trees.RData"))  #create_trees()

load("data/state_lists.RData")

# decision tree ----

tree_dat <-
  create_ce_tree_long_df(
    tree_list = TST_QFT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_QFT_pname_from_to,
    cname_from_to = TST_QFT_cname_from_to,
    hname_from_to = TST_QFT_hname_from_to)

write.csv(tree_dat, file = "data/tree_dat_TST+QFT.csv")


## group together decision tree terminal states corresponding
## to initial states in the Markov model
## make sure names in same order as heemod model
## TODO: match order automatically
state_list <- state_lists$`TST_QFT`

dt <-
  run_cedectree(tree_dat,
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

# returns expected values at each node
# we're interested in node `1` (root)

save(dt, file = "data/run_cedectree_TST+QFT.RData")

#########################
# Markov model

heemod_model <- create_ltbi_heemod()

res_mm <-
  heemod_init_pop_PSA(
    heemod_model,
    init_states = dt$cost$term_pop_sa)

# extract the cost and utility values
c_mm <- map_df(res_mm, "run_model")$cost
h_mm <- map_df(res_mm, "run_model")$utility


## combine decision tree and Markov model output

res <-
  list(cost =
         c_mm + dt$cost$ev_sa[['1']],
       health =
         h_mm - dt$health$ev_sa[['1']])

saveRDS(res, file = "data/res_TST+QFT.RDS")


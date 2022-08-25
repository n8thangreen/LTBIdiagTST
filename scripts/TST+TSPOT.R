
# test strategy:
# TST -> T.SPOT


library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(CEdecisiontree)


load(here::here("data", "params.RData")) #create_param_values()
load(here::here("data", "trees.RData"))  #create_trees()


# decision tree ----

tree_dat <-
  create_ce_tree_long_df(
    tree_list = TST_TSPOT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_TSPOT_pname_from_to,
    cname_from_to = TST_TSPOT_cname_from_to,
    hname_from_to = TST_TSPOT_hname_from_to)

state_list <-
  list(
    no_LTBI  = c(19, 21, 22, 24, 29, 32, 37, 40, 43),
    LTBI_complete_Tx  = 12,
    LTBI_incomplete_Tx = c(10, 13),
    LTBI_no_Tx = c(15, 27, 31, 35, 39, 42),
    activeTB = c(),
    dead = c())

dt <-
  run_cedectree(tree_dat,
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

write.csv(tree_dat, file = "data/tree_dat_TST+TSPOT.csv")
save(dt, file = "data/run_cedectree_TST+TSPOT.RData")

# Markov model ----

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

saveRDS(res, file = "data/res_TST+TSPOT.RDS")



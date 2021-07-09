
# test strategy:
# T.SPOT


library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(treeSimR)
library(CEdecisiontree)

# input data
load(here::here("data", "params.RData")) #create_param_values()

# tree structures
load(here::here("data", "trees.RData"))  #create_trees()


# decision tree ----

tree_dat <-
  create_ce_tree_long_df(
    tree_list = TSPOT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TSPOT_pname_from_to,
    cname_from_to = TSPOT_cname_from_to,
    hname_from_to = TSPOT_hname_from_to)

# starting states of Markov model
state_list <-
  list(
    no_LTBI  = c(16, 18, 19, 21, 26, 29),
    LTBI_complete_Tx  = 9,
    LTBI_incomplete_Tx = c(7, 10),
    LTBI_no_Tx = c(12, 24, 28),
    activeTB = c(),
    dead = c())

dt <-
  run_cedectree(tree_dat,
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

write.csv(tree_dat, file = "data/tree_dat_TSPOT.csv")


# Markov model ----

heemod_model <- create_ltbi_heemod()

res_mm <-
  heemod_init_pop_PSA(
    heemod_model,
    init_states = dt$cost$term_pop_sa)

#TODO: replace function above?
# init_pop <- t(dt$cost$term_pop_sa)
# res_mm <- map(init_pop, heemod_model)

# extract the cost and utility values
c_mm <- map_df(res_mm, "run_model")$cost
h_mm <- map_df(res_mm, "run_model")$utility


## combine decision tree and Markov model output

res <-
  list(cost =
         c_mm + dt$cost$ev_sa[['1']],
       health =
         h_mm - dt$health$ev_sa[['1']])

saveRDS(res, file = "data/res_TSPOT.RDS")


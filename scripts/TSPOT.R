
# test strategy:
# T-SPOT


library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(CEdecisiontree)

# input data
load(here::here("data", "params.RData")) #create_param_values()

# tree structures
load(here::here("data", "trees.RData"))  #create_trees()

load("data/state_lists.RData")

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
state_list <- state_lists$TSPOT

dt <-
  run_cedectree(tree_dat,
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

write.csv(tree_dat, file = "data/tree_dat_TSPOT.csv")


# Markov model

dt <- readRDS(file = "data/run_cedectree_TSPOT.RDS")

heemod_params <-
  list(pReact = label_probs$pReact,
       pReact_incomp = label_probs$pReact_incomp,
       pReact_comp = label_probs$pReact_comp,
       TB_cost = label_costs$TB_cost)

heemod_model <- do.call(create_ltbi_heemod,
                        args = heemod_params)
# # points values
# heemod_model(unname(unlist(dt$cost$term_pop_point)))

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
         c_mm + dt$cost$ev_sa[, 1],
       health =
         h_mm - dt$health$ev_sa[, 1])

saveRDS(res, file = "data/res_TSPOT.RDS")


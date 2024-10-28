
# test strategy:
# QFT only


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
    tree_list = QFT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = QFT_pname_from_to,
    cname_from_to = QFT_cname_from_to,
    hname_from_to = QFT_hname_from_to)

state_list <- state_lists$QFT

dt <-
  run_cedectree(tree_dat,
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

write.csv(tree_dat, file = "data/tree_dat_QFT.csv")
save(dt, file = "data/run_cedectree_QFT.RData")

###############################
# Markov model

dt <- readRDS(file = "data/run_cedectree_QFT.RDS")

heemod_params <-
  list(pReact = label_probs$pReact,
       pReact_incomp = label_probs$pReact_incomp,
       pReact_comp = label_probs$pReact_comp,
       TB_cost = label_costs$TB_cost)

heemod_model <- do.call(create_ltbi_heemod,
                        args = heemod_params)

# # from Excel for testing
# # check same results
# term_pop_point <-
#   list(
#     no_LTBI = 738.45,
#     LTBI_complete_Tx = 155.44,
#     LTBI_incomplete_Tx = 39.25,
#     LTBI_no_Tx = 66.87,
#     activeTB = 0,
#     dead = 0)
#
# # points values
# heemod_model(
#   unname(unlist(term_pop_point)/1000))

# # point values
# starting_state_props <- unname(unlist(dt$cost$term_pop_point))
# res_mm_pt <- heemod_model(starting_state_props)

res_mm <-
  heemod_init_pop_PSA(
    heemod_model,
    init_states = dt$cost$term_pop_sa)

# extract the cost and utility values
c_mm <- map_df(res_mm, "run_model")$cost
h_mm <- map_df(res_mm, "run_model")$utility

## combine decision tree and Markov model output

# summary_mm <- summary(res_mm_pt)$res_values
#
# res_pt <-
#   list(cost = summary_mm$cost + dt$cost$ev_point[[1]],
#        health = summary_mm$utility + dt$health$ev_point[[1]])

res <-
  list(cost =
         c_mm + dt$cost$ev_sa[, 1],
       health =
         h_mm - dt$health$ev_sa[, 1])

# map(res, median)
# summary(dt$cost$ev_sa[[1]])
# summary(dt$health$ev_sa[[1]])

saveRDS(res, file = "data/res_QFT.RDS")



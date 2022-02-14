
# test strategy:
# QFT only


library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(treeSimR)
library(CEdecisiontree)


load(here::here("data", "params.RData")) #create_param_values()
load(here::here("data", "trees.RData"))  #create_trees()


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

write.csv(tree_dat, file = "data/tree_dat_QFT.csv")
save(dt, file = "data/run_cedectree_QFT.RData")


# Markov model ----

heemod_model <- create_ltbi_heemod()

# points values
out_heemod <- heemod_model(unname(unlist(dt$cost$term_pop_point)))

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

saveRDS(res, file = "data/res_QFT.RDS")


# res_point <-
#   list(cost =
#          summary(out_heemod)$res_values$cost + dt$cost$ev_point[['1']],
#        health =
#          summary(out_heemod)$res_values$utility - dt$health$ev_point[['1']])



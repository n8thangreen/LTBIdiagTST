
# test strategy:
# TST only


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
    tree_list = TST_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_pname_from_to,
    cname_from_to = TST_cname_from_to,
    hname_from_to = TST_hname_from_to)

# Markov model starting states
state_list <-
  list(
    no_LTBI  = c(18, 20, 21, 23, 28, 31, 34),
    LTBI_complete_Tx  = 11,
    LTBI_incomplete_Tx = c(9, 12),
    LTBI_no_Tx = c(14, 26, 30, 33),
    activeTB = NULL,
    dead = NULL)

dt <-
  run_cedectree(tree_dat,
                # PSA
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

write.csv(tree_dat, file = "data/tree_dat_TST.csv")
save(dt, file = "data/run_cedectree_TST.RData")

# summary(dt$cost$ev_sa)

###################
# Markov model

heemod_model <- create_ltbi_heemod()

# points values
res_mm_pt <- heemod_model(
  unname(unlist(dt$cost$term_pop_point)))

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

saveRDS(res, file = "data/res_TST.RDS")



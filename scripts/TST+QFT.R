
# Dual test (TST + QFT) forward model


library(readr)
library(dplyr)
library(tibble)
library(reshape2)
library(treeSimR)
library(assertthat)
library(CEdecisiontree)
library(purrr)


load(here::here("data", "params.RData"))
load(here::here("data", "trees.RData"))


# decision tree ----

tree_dat <-
  create_ce_tree_long_df(
    tree_list = TST_IGRA_fwd_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    pname_from_to = TST_QFT_fwd_pname_from_to,
    cname_from_to = TST_QFT_fwd_cname_from_to)

##TODO: why not include nurse cost in model params?

state_list <-
  list(
    LTBI_complete_Tx  = 12,
    LTBI_incomplete_Txs = c(10,48),
    LTBI_no_Tx = c(14,16,18,20,22,24),
    no_LTBI  = c(33,35,37,39,41,43,45,47,49))

c_dt <-
  dectree(tree_dat,
          label_probs_distns,
          label_costs_distns,
          state_list,
          n = 100)

h_dt <-
  dectree(tree_dat,
          label_probs_distns,
          label_health_distns,
          state_list,
          n = 100)

# Markov model ----

res_mm <-
  init_pop_heemod_PSA(create_ltbi_heemod,
                      init_states = c_dt$term_pop_sa,
                      N = 1)

# extract the cost and utility values
c_mm <- map_df(res_mm, "run_model")$cost
h_mm <- map_df(res_mm, "run_model")$utility


## combine decision tree and Markov model

res <-
  list(cost =
         c_mm + c_dt$ev_sa,
       health =
         h_mm + h_dt$ev_sa)

# save(res, file = "res_TST+QFT.RData")


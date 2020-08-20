
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
    LTBI_incomplete_Txs = c(10,48),
    no_LTBI  = c(33,35,37,39,41,43,45,47,49))

dectree_res <-
  dectree(tree_dat,
          label_probs_distns,
          label_costs_distns,
          state_list,
          n = 100)


# Markov model ----

# set-up heemod model
load("data/ltbi_heemod.RData")

res_mod <- list()

##TODO:
init_states <- dectree_res$term_pop_sa

for (i in 1:nrow(init_states)) {

  res_mod[[i]] <-
    suppressMessages(
      run_model(
        init = 1000 * init_states[i, ],  # population sizes
        method = "end",
        strat,
        parameters = param,
        cycles = 66,
        cost = cost,
        effect = utility
      ))
}

# extract the cost and utility values
c1 <- map_df(res_mod, "run_model")$cost
h1 <- map_df(res_mod, "run_model")$utility

# save()


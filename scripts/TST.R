
# TST model


library(readr)
library(dplyr)
library(tibble)
library(reshape2)
library(treeSimR)
library(assertthat)
library(CEdecisiontree)
library(purrr)
library(treeSimR)


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

res <-
  dectree(tree_dat,
          label_probs_distns,
          label_costs_distns,
          state_list,
          n = 100)


# Markov model ----


# save()

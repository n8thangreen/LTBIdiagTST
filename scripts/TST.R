
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
    tree_list = TST_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_pname_from_to,
    cname_from_to = TST_cname_from_to,
    hname_from_to = TST_hname_from_to)

state_list <-
  list(
    no_LTBI  = c(17,19,20,22,27,30,33),
    LTBI_complete_Tx  = 10,
    LTBI_incomplete_Tx = c(8,11),
    LTBI_no_Tx = c(13,25,29,32),
    activeTB = c(),
    dead = c())

dt <-
  run_cedectree(tree_dat,
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

save(dt, file = here::here("data", "decision_tree_output_TST.RData"))


# Markov model ----


# save()

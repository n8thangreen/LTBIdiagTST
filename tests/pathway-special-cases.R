# test special cases of diagnostic pathways

library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(CEdecisiontree)


## read pathways data

# parameter data
load(here::here("data", "params.RData")) #create_param_values()

# tree structures
load(here::here("data", "trees.RData"))  #create_trees()

# markov model starting states
load("data/state_lists.RData")


##################################
# TST as TST/QFT with perfect TST


# set TST parameters

# run TST/QFT decision tree model
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

# compare against QFT pathway

# save
write.csv(tree_dat, file = "testdata/.csv")


##################################
# QFT as TST/QFT with perfect QFT

# read QFT pathway parameters

# set QFT parameters

# run TST/QFT decision tree model
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

# compare against TST pathway

# save
write.csv(tree_dat, file = "testdata/.csv")


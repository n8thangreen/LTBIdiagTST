# test special cases of diagnostic pathways
# against direectly defined versions

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

tree_dat_TSPOT <-
  create_ce_tree_long_df(
    tree_list = TSPOT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TSPOT_pname_from_to,
    cname_from_to = TSPOT_cname_from_to,
    hname_from_to = TSPOT_hname_from_to)

tree_dat_TST_TSPOT <-
  create_ce_tree_long_df(
    tree_list = TST_TSPOT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_TSPOT_pname_from_to,
    cname_from_to = TST_TSPOT_cname_from_to,
    hname_from_to = TST_TSPOT_hname_from_to)

tree_dat_TST <-
  create_ce_tree_long_df(
    tree_list = TST_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_pname_from_to,
    cname_from_to = TST_cname_from_to,
    hname_from_to = TST_hname_from_to)

########################################
# TST as TST/QFT with uninformative TST

# TSPOT only as special case of TST/TSPOT
tree_TSPOT_star <- tree_dat_TST_TSPOT

# set uninformative TST parameter values
tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "pAccept_TST"] <- 1
tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "pTSTread"] <- 1
tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "TST_pos"] <- 1
tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "PPV_TST"] <- 1
tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "NPV_TST"] <- 0

# match probabilities with TSPOT only decision tree
tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "pAccept_IGRA_TST+"] <-
  tree_dat_TSPOT$prob[tree_dat_TSPOT$name.prob == "pAccept_IGRA"]

tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "TSPOT_pos_TST+"] <-
  tree_dat_TSPOT$prob[tree_dat_TSPOT$name.prob == "TSPOT_pos"]

tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "PPV_TSPOT_TST+"] <-
  tree_dat_TSPOT$prob[tree_dat_TSPOT$name.prob == "PPV_TSPOT"]

tree_TSPOT_star$prob[tree_TSPOT_star$name.prob == "NPV_TSPOT_TST+"] <-
  tree_dat_TSPOT$prob[tree_dat_TSPOT$name.prob == "NPV_TSPOT"]

# run TST/QFT decision tree model

# starting states of Markov model
state_list <- state_lists$TSPOT

res_TSPOT_star <-
  run_cedectree(tree_TSPOT_star,
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

# compare against QFT pathway

# save
write.csv(tree_TSPOT_star, file = "testdata/tree_TSPOT_star.csv")


########################################
# QFT as TST/QFT with uninformative QFT

# TST only as special case of TST/TSPOT
tree_TST_star <- tree_dat_TST_TSPOT

# set uninformative IGRA parameters
tree_TST_star$prob[tree_TST_star$name.prob == "pAccept_IGRA_TST+"] <- 1
tree_TST_star$prob[tree_TST_star$name.prob == "TSPOT_pos_TST+"] <- 1
tree_TST_star$prob[tree_TST_star$name.prob == "PPV_TSPOT_TST+"] <- 1
tree_TST_star$prob[tree_TST_star$name.prob == "NPV_TSPOT_TST+"] <- 0

# starting states of Markov model
state_list <- state_lists$TSPOT

res_TST_star <-
  run_cedectree(tree_TST_star,
                label_probs_distns,
                label_costs_distns,
                label_health_distns,
                state_list)

# compare against TST pathway

# save
write.csv(tree_TST_star, file = "testdata/tree_TST_star.csv")


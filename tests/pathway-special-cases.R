# test special cases of diagnostic pathways
# against directly defined versions

library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(testthat)
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
# replace values
tree_TSPOT_star <-
  mutate(tree_dat_TST_TSPOT,
         prob = ifelse(is.na(name.prob), NA, prob),
         prob = case_when(
           name.prob == "pAccept_TST" ~ 1,
           name.prob == "pTSTread" ~ 1,
           name.prob == "TST_pos" ~ 1,
           name.prob == "PPV_TST" ~
             filter(tree_dat_TSPOT, name.prob == "pLTBI")$prob,
           name.prob == "NPV_TST" ~ 0,
           # match probabilities with TSPOT only decision tree
           name.prob == "TSPOT_pos_TST+" ~
             filter(tree_dat_TSPOT, name.prob == "TSPOT_pos")$prob,
           name.prob == "pAccept_IGRA_TST+" ~
             filter(tree_dat_TSPOT, name.prob == "pAccept_IGRA")$prob,
           name.prob == "PPV_TSPOT_TST+" ~
             filter(tree_dat_TSPOT, name.prob == "PPV_TSPOT")$prob,
           name.prob == "NPV_TSPOT_TST+" ~
             filter(tree_dat_TSPOT, name.prob == "NPV_TSPOT")$prob,
           .default = prob
         ),
         cost = case_when(
           name.cost == "TST" ~ 0,
           name.cost == "Ns_cost" ~ 0,
           .default = cost
         )
  ) |>
  fill_complementary_probs()

# run TST/QFT decision tree model
# point value case
res_TSPOT_star <-
  run_cedectree(dat_long = tree_TSPOT_star,
                state_list = state_lists$TST_TSPOT)

# compare against QFT pathway

# point value case
res_TSPOT <-
  run_cedectree(dat_long = tree_dat_TSPOT,
                state_list = state_lists$TSPOT)

# tests
expect_equal(res_TSPOT_star$cost$ev_point[1], res_TSPOT$cost$ev_point[1])

expect_equal(res_TSPOT_star$cost$term_pop_point, res_TSPOT$cost$term_pop_point)

expect_equal(res_TSPOT_star$health$ev_point[1], res_TSPOT$health$ev_point[1], tolerance = 0.00001)

expect_equal(res_TSPOT_star$health$term_pop_point, res_TSPOT$health$term_pop_point)

# save
write.csv(tree_TSPOT_star, file = "tests/testthat/testdata/tree_TSPOT_star.csv")


########################################
# QFT as TST/QFT with uninformative QFT

# replace values
tree_TST_star <-
  mutate(tree_dat_TST_TSPOT,
         prob = ifelse(is.na(name.prob), NA, prob),
         prob = case_when(
           name.prob == "pAccept_IGRA_TST+" ~ 1,
           name.prob == "TSPOT_pos_TST+" ~ 1,
           name.prob == "PPV_TSPOT_TST+" ~
             filter(tree_dat_TST, name.prob == "PPV_TST")$prob,
           name.prob == "NPV_TSPOT_TST+" ~ 0,
           .default = prob
         ),
         cost = case_when(
           name.cost == "TSPOT" ~ 0,
           .default = cost
         )
  ) |>
  fill_complementary_probs()

# run model
# point value case
res_TST_star <-
  run_cedectree(dat_long = tree_TST_star,
                state_list = state_lists$TST_TSPOT)

# compare against TST pathway

# point value case
res_TST <-
  run_cedectree(dat_long = tree_dat_TST,
                state_list = state_lists$TST)

# tests
expect_equal(res_TST_star$cost$ev_point[1], res_TST$cost$ev_point[1])

expect_equal(res_TST_star$cost$term_pop_point, res_TST$cost$term_pop_point)

expect_equal(res_TST_star$health$ev_point[1], res_TST$health$ev_point[1])

expect_equal(res_TST_star$health$term_pop_point, res_TST$health$term_pop_point)

# save
write.csv(tree_TST_star, file = "tests/testthat/testdata/tree_TST_star.csv")


# use sample samples across scenarios/diagnostic pathways


library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(CEdecisiontree)


load(here::here("data", "params.RData")) #create_param_values()
load(here::here("data", "trees.RData"))  #create_trees()

load("data/state_lists.RData")

# decision tree

tree_dat_tst <-
  create_ce_tree_long_df(
    tree_list = TST_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_pname_from_to,
    cname_from_to = TST_cname_from_to,
    hname_from_to = TST_hname_from_to)

tree_dat_qft <-
  create_ce_tree_long_df(
    tree_list = QFT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = QFT_pname_from_to,
    cname_from_to = QFT_cname_from_to,
    hname_from_to = QFT_hname_from_to)

tree_dat_tspot <-
  create_ce_tree_long_df(
    tree_list = TSPOT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TSPOT_pname_from_to,
    cname_from_to = TSPOT_cname_from_to,
    hname_from_to = TSPOT_hname_from_to)

tree_dat_tst_qft <-
  create_ce_tree_long_df(
    tree_list = TST_QFT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_QFT_pname_from_to,
    cname_from_to = TST_QFT_cname_from_to,
    hname_from_to = TST_QFT_hname_from_to)

tree_dat_tst_tspot <-
  create_ce_tree_long_df(
    tree_list = TST_TSPOT_tree,
    label_probs = label_probs,
    label_costs = label_costs,
    label_health = label_health,
    pname_from_to = TST_TSPOT_pname_from_to,
    cname_from_to = TST_TSPOT_cname_from_to,
    hname_from_to = TST_TSPOT_hname_from_to)

# Markov model starting states

out_tst <- list()
out_qft <- list()
out_tspot <- list()
out_tst_qft <- list()
out_tst_tspot <- list()

out_tst$cost$ev_sa <- NULL
out_qft$cost$ev_sa <- NULL
out_tspot$cost$ev_sa <- NULL
out_tst_qft$cost$ev_sa <- NULL
out_tst_tspot$cost$ev_sa <- NULL

out_tst$health$ev_sa <- NULL
out_qft$health$ev_sa <- NULL
out_tspot$health$ev_sa <- NULL
out_tst_qft$health$ev_sa <- NULL
out_tst_tspot$health$ev_sa <- NULL

out_tst$cost$term_pop_sa <- NULL
out_qft$cost$term_pop_sa <- NULL
out_tspot$cost$term_pop_sa <- NULL
out_tst_qft$cost$term_pop_sa <- NULL
out_tst_tspot$cost$term_pop_sa <- NULL

for (i in 1:100) {

  # sample parameter values

  sample_probs <-
    label_probs_distns |>
    mutate(
      prob = map_dbl(prob, ~sample_distributions(.)))

  sample_costs <-
    label_costs_distns |>
    mutate(
      cost = map_dbl(vals, ~sample_distributions(.))) |>
    select(-vals)

  sample_health <-
    label_health_distns |>
    mutate(
      health = map_dbl(vals, ~sample_distributions(.))) |>
    select(-vals)

  # merge with tree data

  dat_long_tst <-
    tree_dat_tst |>
    as_tibble() |>
    select(-prob, -cost, -health) |>
    dplyr::left_join(sample_probs,
                     by = "name.prob") |>
    dplyr::left_join(sample_health,
                     by = "name.health") |>
    dplyr::left_join(sample_costs,
                     by = "name.cost") |>
  fill_complementary_probs()

  dat_long_qft <-
    tree_dat_qft |>
    as_tibble() |>
    select(-prob, -cost, -health) |>
    dplyr::left_join(sample_probs,
                     by = "name.prob") |>
    dplyr::left_join(sample_health,
                     by = "name.health") |>
    dplyr::left_join(sample_costs,
                     by = "name.cost") |>
    fill_complementary_probs()

  dat_long_tspot <-
    tree_dat_tspot |>
    as_tibble() |>
    select(-prob, -cost, -health) |>
    dplyr::left_join(sample_probs,
                     by = "name.prob") |>
    dplyr::left_join(sample_health,
                     by = "name.health") |>
    dplyr::left_join(sample_costs,
                     by = "name.cost") |>
    fill_complementary_probs()

  dat_long_tst_qft <-
    tree_dat_tst_qft |>
    as_tibble() |>
    select(-prob, -cost, -health) |>
    dplyr::left_join(sample_probs,
                     by = "name.prob") |>
    dplyr::left_join(sample_health,
                     by = "name.health") |>
    dplyr::left_join(sample_costs,
                     by = "name.cost") |>
    fill_complementary_probs()

  dat_long_tst_tspot <-
    tree_dat_tst_tspot |>
    as_tibble() |>
    select(-prob, -cost, -health) |>
    dplyr::left_join(sample_probs,
                     by = "name.prob") |>
    dplyr::left_join(sample_health,
                     by = "name.health") |>
    dplyr::left_join(sample_costs,
                     by = "name.cost") |>
    fill_complementary_probs()

  # run decision trees

  dt_tst <-
    run_cedectree(dat_long = dat_long_tst,
                  state_list = state_lists$TST)
  dt_qft <-
    run_cedectree(dat_long = dat_long_qft,
                  state_list = state_lists$QFT)
  dt_tspot <-
    run_cedectree(dat_long = dat_long_tspot,
                  state_list = state_lists$TSPOT)
  dt_tst_qft <-
    run_cedectree(dat_long = dat_long_tst_qft,
                  state_list = state_lists$TST_QFT)
  dt_tst_tspot <-
    run_cedectree(dat_long = dat_long_tst_tspot,
                  state_list = state_lists$TST_TSPOT)

  # combine results

  out_tst$cost$ev_sa <- rbind(out_tst$cost$ev_sa, dt_tst$cost$ev_point)
  out_tst$health$ev_sa <- rbind(out_tst$health$ev_sa, dt_tst$health$ev_point)
  out_tst$cost$term_pop_sa <- rbind(out_tst$cost$term_pop_sa, unlist(dt_tst$cost$term_pop_point))

  out_qft$cost$ev_sa <- rbind(out_qft$cost$ev_sa, dt_qft$cost$ev_point)
  out_qft$health$ev_sa <- rbind(out_qft$health$ev_sa, dt_qft$health$ev_point)
  out_qft$cost$term_pop_sa <- rbind(out_qft$cost$term_pop_sa, unlist(dt_qft$cost$term_pop_point))

  out_tspot$cost$ev_sa <- rbind(out_tspot$cost$ev_sa, dt_tspot$cost$ev_point)
  out_tspot$health$ev_sa <- rbind(out_tspot$health$ev_sa, dt_tspot$health$ev_point)
  out_tspot$cost$term_pop_sa <- rbind(out_tspot$cost$term_pop_sa, unlist(dt_tspot$cost$term_pop_point))

  out_tst_qft$cost$ev_sa <- rbind(out_tst_qft$cost$ev_sa, dt_tst_qft$cost$ev_point)
  out_tst_qft$health$ev_sa <- rbind(out_tst_qft$health$ev_sa, dt_tst_qft$health$ev_point)
  out_tst_qft$cost$term_pop_sa <- rbind(out_tst_qft$cost$term_pop_sa, unlist(dt_tst_qft$cost$term_pop_point))

  out_tst_tspot$cost$ev_sa <- rbind(out_tst_tspot$cost$ev_sa, dt_tst_tspot$cost$ev_point)
  out_tst_tspot$health$ev_sa <- rbind(out_tst_tspot$health$ev_sa, dt_tst_tspot$health$ev_point)
  out_tst_tspot$cost$term_pop_sa <- rbind(out_tst_tspot$cost$term_pop_sa, unlist(dt_tst_tspot$cost$term_pop_point))
}

saveRDS(out_tst, file = "data/run_cedectree_TST.RDS")
saveRDS(out_qft, file = "data/run_cedectree_QFT.RDS")
saveRDS(out_tspot, file = "data/run_cedectree_TSPOT.RDS")
saveRDS(out_tst_qft, file = "data/run_cedectree_TST_QFT.RDS")
saveRDS(out_tst_tspot, file = "data/run_cedectree_TST_TSPOT.RDS")


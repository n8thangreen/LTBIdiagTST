# scenario analysis

library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(CEdecisiontree)


load(here::here("data", "params.RData")) #create_param_values()
load(here::here("data", "trees.RData"))  #create_trees()

scenario_vals <- readr::read_csv(here::here("inst/extdata/scenario_input_values.csv"))

state_list <-
  list(
    no_LTBI  = c(16, 18, 19, 21, 26, 29),
    LTBI_complete_Tx  = 9,
    LTBI_incomplete_Tx = c(7, 10),
    LTBI_no_Tx = c(12, 24, 28),
    activeTB = c(),
    dead = c())

ev_ce <- NULL
ev_heemod <- NULL

for (i in 1:nrow(scenario_vals)) {

  new_vals <- as.list(scenario_vals[i, -1])

  # decision tree
  tree_dat <-
    create_ce_tree_long_df(
      tree_list = QFT_tree,
      label_probs = modifyList(label_probs, new_vals),
      label_costs = modifyList(label_costs, new_vals),
      label_health = modifyList(label_health, new_vals),
      pname_from_to = QFT_pname_from_to,
      cname_from_to = QFT_cname_from_to,
      hname_from_to = QFT_hname_from_to)

  res_ce <- run_cedectree(dat_long = tree_dat,
                          state_list = state_list)

  # root node values
  ev_ce <- rbind(ev_ce,
                 c(res_ce$cost$ev_point[1],
                   -res_ce$health$ev_point[1]))
  # markov model
  heemod_model <-
    create_ltbi_heemod(pReact_comp = new_vals$pReact_comp,
                       pReact_incomp = new_vals$pReact_incomp,
                       pReact = new_vals$pReact,
                       TB_cost = new_vals$TB_cost)

  res_heemod <-
    heemod_model(
      unname(unlist(res_ce$cost$term_pop_point)))

  ev_heemod <- rbind(ev_heemod,
                     c(res_heemod$run_model$cost,
                       res_heemod$run_model$utility))
}

res <- ev_ce + ev_heemod

wtp <- 25000

netbenefit <- wtp*res[, 2] - res[, 1]

# against primary analysis
inmb <- round(netbenefit - (wtp*res[1, 2] - res[1, 1]), 2)

write.csv(tree_dat, file = "data/tree_dat_QFT_scenario.csv")
save(dt, file = "data/run_cedectree_QFT_scenario.RData")


# tornado plot

dat <- data.frame(scenario_vals, netbenefit, inmb)

library(ceplot)
library(magrittr)
library(reshape2)
library(plyr)
library(purrr)
library(dplyr)
library(ggplot2)

# psa_dat <-
#   dat |>
#   select(pReact, pReact_comp, pReact_incomp, pHep, pComp_chemo, Sp_cost, LTBIcompl_cost,
#            LTBIincompl_cost, pAccept_chemo, QFT, QFT_pos, Hep, PPV_QFT, NPV_QFT, TB_cost) |>
#   distinct() %>%
#   model.frame(
#   formula = inmb ~ pReact + pReact_comp + pReact_incomp + pHep + pComp_chemo + Sp_cost + LTBIcompl_cost +
#     LTBIincompl_cost + pAccept_chemo + QFT + QFT_pos + Hep + PPV_QFT + NPV_QFT + TB_cost,
#   data = .)

psa_dat <-
  dat |>
  select(inmb, pReact, pComp_chemo, Sp_cost, LTBIcompl_cost,
         LTBIincompl_cost,
         # pAccept_chemo,
         QFT, PPV_QFT, NPV_QFT, TB_cost) |>
  distinct(across(pReact:TB_cost), .keep_all = TRUE) %>%
  model.frame(
    formula = inmb ~ pReact + pComp_chemo + Sp_cost + LTBIcompl_cost +
      LTBIincompl_cost +
      # pAccept_chemo +
      QFT + PPV_QFT + NPV_QFT + TB_cost,
    data = .) |>
  dplyr::rename(
    "LTBI completion cost" = LTBIcompl_cost,
    "LTBI incompletion cost" = LTBIincompl_cost,
    "Negative predictive value" = NPV_QFT,
    "Probability of completing treatment" = pComp_chemo,
    "Positive predictive value" = PPV_QFT,
    "Reactivation probability" = pReact,
    "Cost of QFT test" = QFT,
    "Cost of positive screening" = Sp_cost,
    "Cost active TB treatment" = TB_cost)

psa_dat %>%
  create_tornado_data %>%
  ceplot:::ggplot_tornado.tornado(baseline_output = 0) +
  ylab("Net monetary benefit (NMB)") #+
# ylim(-500, 500)
# ggplot_tornado(baseline_output = 460463) +
# ylim(450000, 465400)




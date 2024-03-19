# deterministic scenario sensitivity analysis

library(dplyr)
library(reshape2)
library(assertthat)
library(purrr)
library(heemod)
library(magrittr)
library(CEdecisiontree)


load(here::here("data", "params.RData"))      #create_param_values()
load(here::here("data", "trees.RData"))       #create_trees()
load(here::here("data", "state_lists.RData")) #create_state_lists()

# select diagnostic pathway
test_name <- "QFT"
# test_name <- "TST_QFT"

##TODO:
# test_name <- "TST"


scenario_vals <-
  readr::read_csv(here::here(glue::glue("inst/extdata/scenario_input_values_{test_name}.csv")))

state_list <- state_lists[[test_name]]

# create variable names
tree_varname <- paste0(test_name, "_tree")
pname_from_to <- paste0(test_name, "_pname_from_to")
cname_from_to <- paste0(test_name, "_cname_from_to")
hname_from_to <- paste0(test_name, "_hname_from_to")

ev_ce <- NULL
ev_heemod <- NULL

for (i in 1:nrow(scenario_vals)) {

  new_vals <- as.list(scenario_vals[i, -1])

  # decision tree
  tree_dat <-
    create_ce_tree_long_df(
      tree_list = get(tree_varname),
      label_probs = modifyList(label_probs, new_vals),
      label_costs = modifyList(label_costs, new_vals),
      label_health = modifyList(label_health, new_vals),
      pname_from_to = get(pname_from_to),
      cname_from_to = get(cname_from_to),
      hname_from_to = get(hname_from_to))

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

ce_scenarios <- (ev_ce + ev_heemod) |> set_colnames(c("cost", "eff"))

write.csv(ce_scenarios, file = glue::glue("data/ce_scenarios_{test_name}.csv"))

##TODO:
## take difference against basecase TST
## make ce planes

###############
# tornado plot

library(ceplot)
library(magrittr)
library(reshape2)
library(plyr)
library(purrr)
library(dplyr)
library(ggplot2)


ce_scenarios <- read.csv(file = glue::glue("data/ce_scenarios_{test_name}.csv"))
scenario_vals <-
  readr::read_csv(here::here(glue::glue("inst/extdata/scenario_input_values_{test_name}.csv")))

wtp <- 25000

netbenefit <- wtp*ce_scenarios[, "eff"] - ce_scenarios[, "cost"]

# against primary analysis
inmb <- round(netbenefit - netbenefit[1], 2)

dat <- data.frame(scenario_vals, netbenefit, inmb)

psa_dat <-
  dat |>
  # select(inmb, pReact, pComp_chemo, Sp_cost, LTBIcompl_cost,
  #        LTBIincompl_cost,
  #        # pAccept_chemo,
  #        QFT, PPV_QFT, NPV_QFT, TB_cost) |>
  select(-scenario, -netbenefit, -Hep, -pHep,
         # -QFT_pos_TST.,
         -QFT_pos,
         -pAccept_chemo,
         -pReact_comp, -pReact_incomp) |>
  distinct(across(pReact:TB_cost), .keep_all = TRUE) %>%
  model.frame(
    formula = inmb ~ ., data = .) |>
  dplyr::rename(
    "LTBI completion cost" = LTBIcompl_cost,
    "LTBI incompletion cost" = LTBIincompl_cost,
    "Negative predictive value" = NPV_QFT,
    "Positive predictive value" = PPV_QFT,
    # "Negative predictive value" = NPV_QFT_TST.,
    # "Positive predictive value" = PPV_QFT_TST.,
    "Probability of completing treatment" = pComp_chemo,
    "Reactivation probability" = pReact,
    "Cost of QFT test" = QFT,
    "Cost of positive screening" = Sp_cost,
    "Cost active TB treatment" = TB_cost)

psa_dat %>%
  ceplot:::create_tornado_data() %>%
  ceplot:::ggplot_tornado.tornado(baseline_output = 0, annotate_nudge = 20) +
  ylab("Incremental net monetary benefit (INMB)") +
  theme(legend.position="none")#+
# ylim(-500, 500)
# ggplot_tornado(baseline_output = 460463) +
# ylim(450000, 465400)


ggsave(filename = glue::glue("plots/tornado_plot_{test_name}.png"),
       device = "png", width = 25, height = 15, units = "cm")

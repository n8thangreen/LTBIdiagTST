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
# test_name <- "QFT"
test_name <- "TST_QFT"
# test_name <- "TST"


# input parameter values
scenario_vals <-
  readr::read_csv(here::here(glue::glue("inst/extdata/scenario_input_values_{test_name}.csv")))

# decision tree state ids matching Markov model
state_list <- state_lists[[test_name]]

# create variable names
tree_varname <- paste0(test_name, "_tree")
pname_from_to <- paste0(test_name, "_pname_from_to")
cname_from_to <- paste0(test_name, "_cname_from_to")
hname_from_to <- paste0(test_name, "_hname_from_to")

# expected values from submodels
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

# combine model expected values
ce_scenarios <- (ev_ce + ev_heemod) |> set_colnames(c("cost", "eff"))

write.csv(ce_scenarios, file = glue::glue("data/ce_scenarios_{test_name}.csv"))


################
# tornado plots

library(ceplot)
library(plyr)
library(ggplot2)

ce_scenarios <- read.csv(file = glue::glue("data/ce_scenarios_{test_name}.csv"))

scenario_vals <-
  readr::read_csv(here::here(glue::glue("inst/extdata/scenario_input_values_{test_name}.csv")))

wtp <- 25000


## against own baseline _within_ pathway

netbenefit <- wtp*ce_scenarios[, "eff"] - ce_scenarios[, "cost"]

# against primary analysis
inmb <- round(netbenefit - netbenefit[1], 2)

dat <- data.frame(scenario_vals, netbenefit, inmb)

psa_dat <-
  dat |>
  select(-scenario, -netbenefit, -Hep, -pHep,
         # -NPV_TST,
         # -PPV_TST,
         # -TST,
         # -TST_pos,
         -QFT,
         # -NPV_QFT,   # QFT only pathway
         # -PPV_QFT,   # QFT only pathway
         # -QFT_pos,   # QFT only pathway
         -NPV_QFT_TST.,   # TST only pathway
         -PPV_QFT_TST.,   # TST only pathway
         -QFT_pos_TST.,   # TST only pathway
         -pAccept_chemo,
         -pReact_comp, -pReact_incomp) |>
  distinct(across(pReact:TB_cost), .keep_all = TRUE) %>%
  model.frame(
    formula = inmb ~ ., data = .) |>
  dplyr::rename(
    "LTBI completion cost" = LTBIcompl_cost,
    "LTBI incompletion cost" = LTBIincompl_cost,
    # "Negative predictive value" = NPV_QFT,
    # "Positive predictive value" = PPV_QFT,
    "Negative predictive value TST" = NPV_TST,
    "Positive predictive value TST" = PPV_TST,
    # "Probability positive QFT after TST" = QFT_pos_TST.,    # dual pathway
    "Probability positive TST" = TST_pos,
    # "Probability positive QFT" = QFT_pos,
    # "Negative predictive value QFT" = NPV_QFT_TST.,  # dual pathway
    # "Positive predictive value QFT" = PPV_QFT_TST.,  # dual pathway
    "Probability of completing treatment" = pComp_chemo,
    "Probability of reactivation" = pReact,
    # "QFT test cost" = QFT,
    "TST test cost" = TST,
    "Positive screening cost" = Sp_cost,
    "Active TB treatment cost" = TB_cost)

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

## against TST baseline _between_ pathways

ce_scenarios_TST <- read.csv(file = "data/ce_scenarios_TST.csv")
ce_scenarios_comp <- read.csv(file = glue::glue("data/ce_scenarios_{test_name}.csv"))

netbenefit_TST <- wtp*ce_scenarios_TST[, "eff"] - ce_scenarios_TST[, "cost"]
netbenefit_comp <- wtp*ce_scenarios_comp[, "eff"] - ce_scenarios_comp[, "cost"]

# against TST equivalent scenario
inmb <- round(netbenefit_comp - netbenefit_TST, 2)

dat <- data.frame(scenario_vals, netbenefit_TST, netbenefit_comp, inmb)

psa_dat <-
  dat |>
  select(-scenario, -netbenefit_TST, -netbenefit_comp, -Hep, -pHep,
         -NPV_TST,    # QFT only pathway
         # -PPV_TST,
         -TST,        # QFT only pathway
         -TST_pos,
         # -QFT,
         # -NPV_QFT,
         # -PPV_QFT,
         # -QFT_pos,
         # -NPV_QFT_TST.,
         # -PPV_QFT_TST.,
         # -QFT_pos_TST.,
         -pAccept_chemo,
         -pReact_comp, -pReact_incomp) |>
  distinct(across(pReact:TB_cost), .keep_all = TRUE) %>%
  model.frame(
    formula = inmb ~ ., data = .) |>
  dplyr::rename(
    "LTBI completion cost" = LTBIcompl_cost,
    "LTBI incompletion cost" = LTBIincompl_cost,
    # "Negative predictive value QFT" = NPV_QFT,
    # "Positive predictive value QFT" = PPV_QFT,
    # "Negative predictive value TST" = NPV_TST,      # QFT only pathway
    "Positive predictive value TST" = PPV_TST,
    "Negative predictive value combined" = NPV_QFT_TST.,
    "Positive predictive value combined" = PPV_QFT_TST.,
    # "Negative predictive value QFT" = NPV_QFT,
    # "Positive predictive value QFT" = PPV_QFT,
    "Probability of completing treatment" = pComp_chemo,
    "Probability of reactivation" = pReact,
    "QFT test cost" = QFT,
    # "TST test cost" = TST,                        # QFT only pathway
    # "Probability of positive TST test" = TST_pos,
    # "Probability of positive QFT test" = QFT_pos,
    "Probability of positive QFT after TST" = QFT_pos_TST.,
    "Positive screening cost" = Sp_cost,
    "Active TB treatment cost" = TB_cost)

psa_dat %>%
  ceplot:::create_tornado_data() %>%
  ceplot:::ggplot_tornado.tornado(annotate_nudge = 50) +
  ylab("Incremental net monetary benefit (INMB)") +
  theme(legend.position = "none") +
  # ylim(10200, 11000)       # QFT only pathway
  ylim(11300, 11800)         # dual pathway


ggsave(filename = glue::glue("plots/tornado_plot_TST_vs_{test_name}.png"),
       device = "png", width = 25, height = 15, units = "cm")


###########
# ce plane

library(ggrepel)
library(BCEA)

# load in all ce_scenarios data
ce_scenarios_TST <- read.csv(file = "data/ce_scenarios_TST.csv")
# ce_scenarios_QFT <- read.csv(file = "data/ce_scenarios_TST_QFT.csv")
ce_scenarios_QFT <- read.csv(file = "data/ce_scenarios_QFT.csv")

TST_eff <- t(ce_scenarios_TST$eff)
TST_cost <- t(ce_scenarios_TST$cost)
QFT_eff <- t(ce_scenarios_QFT$eff)
QFT_cost <- t(ce_scenarios_QFT$cost)

# incremental values vs TST
incr_eff <- t(c(0, QFT_eff - TST_eff))
incr_cost <- t(c(0, QFT_cost - TST_cost))

res_bcea <- bcea(eff = as.matrix(incr_eff)[c(1,1),],
                 cost = as.matrix(incr_cost)[c(1,1),], ref = 1)

# plot
ceplane.plot(res_bcea, graph = "ggplot", wtp = 25000)


# https://ggrepel.slowkow.com/articles/examples.html

labels <- c("dummy", "BASELINE",
            "pReact_high","pReact_low",
            "pHep_low","pHep_high",
            "pComp_chemo_low","pComp_chemo_high",
            "PPV_QFT_low", "PPV_QFT_high",
            "pos_screen_cost_low", "pos_screen_cost_high",
            "LTBIcompl_cost_low","LTBIcompl_cost_high",
            "LTBIincompl_cost_low","LTBIincompl_cost_high",
            "pAccept_chemo_low","pAccept_chemo_high",
            "QFT_cost_low","QFT_cost_high",
            "QFT_pos_low","QFT_pos_high",
            "Hep_low","Hep_high",
            "NPV_QFT_low","NPV_QFT_high",
            "TB_cost_low","TB_cost_high")

dat <- data.frame(scenario = 1:28,
                  param = rep(1:14, each = 2),
                  labels,
                  eff = t(incr_eff),
                  cost = t(incr_cost))

p <- ggplot(dat, aes(eff, cost, label = labels)) +
  geom_point(color = "red") +
  # geom_text_repel(max.overlaps = Inf, direction = "x") +
  geom_label_repel(fill = "white", box.padding = 0.5,
                   colour = dat$param,
                   max.overlaps = Inf) + #, direction = "x") +
  ylim(-190, -70) +
  xlim(0, 0.95)

p

# ggsave(filename = "plots/cepplane_scenario_analysis_TST_QFT.png",
ggsave(filename = "plots/cepplane_scenario_analysis_QFT.png",
       device = "png", width = 30, height = 30, units = "cm")


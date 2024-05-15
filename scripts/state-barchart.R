# Markov model starting state bar chart

library(dplyr)
library(ggplot2)
library(reshape2)

dt_tst <- readRDS(file = "data/run_cedectree_TST.RDS")
dt_qft <- readRDS(file = "data/run_cedectree_QFT.RDS")
dt_tspot <- readRDS(file = "data/run_cedectree_TSPOT.RDS")
dt_tst_qft <- readRDS(file = "data/run_cedectree_TST_QFT.RDS")
dt_tst_tspot <- readRDS(file = "data/run_cedectree_TST_TSPOT.RDS")


dat_raw <-
  data.frame(
    scenario = c("TST", "TST_QFT", "TST_TSPOT", "TSPOT", "QFT"),
    rbind(
      colMeans(dt_tst$cost$term_pop_sa),
      colMeans(dt_tst_qft$cost$term_pop_sa),
      colMeans(dt_tst_tspot$cost$term_pop_sa),
      colMeans(dt_tspot$cost$term_pop_sa),
      colMeans(dt_qft$cost$term_pop_sa)))

dat <-
  dat_raw |>
  melt() |>
  mutate(variable =
           ifelse(variable == "no_LTBI", "No LTBI",
                  ifelse(variable == "LTBI_complete_Tx", "LTBI complete treatment",
                         ifelse(variable == "LTBI_incomplete_Tx", "LTBI incomplete treatment",
                                ifelse(variable == "LTBI_no_Tx", "LTBI no treatment",
                                       "Active TB"))))) |>
  rename(State = variable,
         Probability = value,
         Scenario = scenario) |>
  filter(State != "No LTBI")

# grouped barchart of starting states using all data for all scenarios

ggplot(data = dat, aes(x = State, y = Probability,
                       col = Scenario, fill = Scenario, group = Scenario)) +
  geom_bar(position="dodge", stat="identity") +
  theme_minimal()

ggsave("plots/state_barchart.png", width = 25, height = 15, units = "cm", dpi = 640, bg = "white")

# radar plot

library(scales)
library(ggradar)

ggradar(dat_raw, grid.max = 0.1, group.line.width = 1, group.point.size = 3) +
  scale_color_brewer(palette = "Set2")

ggsave("plots/state_radar.png", width = 30, height = 20, units = "cm", dpi = 640, bg = "white")

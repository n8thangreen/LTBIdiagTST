
# plots of model outputs
# for all scenarios

library(BCEA)
library(heemod)
library(ggplot2)


# load data

# input data
load(here::here("data", "params.RData")) #create_param_values()

dat <- list()
dat$TST_QFT <- readRDS(file = "data/res_TST+QFT.RDS")
dat$TSPOT <- readRDS(file = "data/res_TSPOT.RDS")
dat$TST <- readRDS(file = "data/res_TST.RDS")
dat$QFT <- readRDS(file = "data/res_QFT.RDS")
dat$TST_TSPOT <- readRDS(file = "data/res_TST+TSPOT.RDS")


# filenames <- Sys.glob("data/res_*.RDS")
# dat <- map(filenames, readRDS)

#######################
# plot direct

## point values
heemod_params <-
  list(pReact = label_probs$pReact,
       pReact_incomp = label_probs$pReact_incomp,
       pReact_comp = label_probs$pReact_comp,
       TB_cost = label_costs$TB_cost)

heemod_model <- do.call(create_ltbi_heemod,
                        args = heemod_params)

# points values
##TODO


# incremental cost, qalys
# TST as baseline

delta_c_tstqft<- dat$TST_QFT$cost - dat$TST$cost
delta_h_tstqft <- dat$TST_QFT$health - dat$TST$health

delta_c_tspot <- dat$TSPOT$cost - dat$TST$cost
delta_h_tspot <- dat$TSPOT$health - dat$TST$health

delta_c_qft <- dat$QFT$cost - dat$TST$cost
delta_h_qft <- dat$QFT$health - dat$TST$health

delta_c_tsttspot <- dat$TST_TSPOT$cost - dat$TST$cost
delta_h_tsttspot <- dat$TST_TSPOT$health - dat$TST$health

plot(delta_h_tspot, delta_c_tspot,
     col = "orange",
     # xlim = c(-0.005, 0.005),
     xlim = c(-0.01, 0.02),
     ylim = c(-400, 0),
     xlab = "Incremental QALYs", ylab = "Incremental cost")
points(mean(delta_h_tspot), mean(delta_c_tspot), col = "orange", pch = 16, cex = 1.5)

points(delta_h_tstqft, delta_c_tstqft, col = "blue")
points(mean(delta_h_tstqft), mean(delta_c_tstqft), col = "blue", pch = 16, cex = 1.5)

points(delta_h_qft, delta_c_qft, col = "grey")
points(mean(delta_h_qft), mean(delta_c_qft), col = "grey", pch = 16, cex = 1.5)

points(delta_h_tsttspot, delta_c_tsttspot, col = "darkgreen")
points(mean(delta_h_tsttspot), mean(delta_c_tsttspot), col = "darkgreen", pch = 16, cex = 1.5)

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")


###############
# with BCEA

eff <- cbind(dat$TST_QFT$health,
             dat$TST_TSPOT$health,
             dat$TST$health,
             dat$TSPOT$health,
             dat$QFT$health)

cost <- cbind(dat$TST_QFT$cost,
              dat$TST_TSPOT$cost,
              dat$TST$cost,
              dat$TSPOT$cost,
              dat$QFT$cost)

# TST reference
m <- bcea(-eff, -cost,
          # Kmax = 30000,
          k = seq(0, 30000, 50),
          ref = 3,
          interventions = c("TST QFT", "TST TSPOT", "TST", "TSPOT", "QFT"))

ceplane.plot(m, graph = "base")#, xlim = c(0.15,0.17), pos = "bottomleft")
ceplane.plot(m, graph = "ggplot")#, pos = "bottomleft") + xlim(0.15,0.17)

mypalette <- RColorBrewer::brewer.pal(4, "Set2")
ceplane.plot(m, graph = "ggplot",
             icer = list(color = "black"),
             title = "",
             point = list(color = mypalette, size = 2),
             xlim = c(-0.007, 0.007), ylim = c(-220, 20),
             currency = "£",
             ref_first = FALSE)

contour2(m, graph = "ggplot",
        point = list(color = c("blue", "plum", "tomato", "springgreen"), size = 2),
        icer = list(color = c("darkblue", "black", "darkred", "darkgreen"), size = 5),
        contour = list(breaks = 0.25),
        title = "", currency = "£")

ggsave(filename = "plots/ceplane_main.png", device = "png", width = 15, height = 15, units = "cm")


ceac.plot(m, line = list(color = mypalette),
          title = "", currency = "£", graph = "ggplot2")

# simultaneous comparisons
m_simul <- multi.ce(m)
ceac.plot(m_simul)


#########
# tables

BCEA::tabulate_means(m)

ce_table2(m)

write.csv(ce_table2(m), file = "data/ce_table.csv")




#########
# plots #
#########

library(BCEA)
library(heemod)
library(ggplot2)


# load data

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
heemod_model <- create_ltbi_heemod()

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
     xlim = c(-0.005, 0.005),
     # xlim = c(-0.01, 0.17),
     xlab = "incremental qalys", ylab = "Incremental cost")
points(mean(delta_h_tspot), mean(delta_c_tspot), col = "black", pch = 16, cex = 1.5)

points(delta_h_tstqft, delta_c_tstqft, col = "green")
points(mean(delta_h_tstqft), mean(delta_c_tstqft), col = "green", pch = 16, cex = 1.5)

points(delta_h_qft, delta_c_qft, col = "blue")
points(mean(delta_h_qft), mean(delta_c_qft), col = "blue", pch = 16, cex = 1.5)

points(delta_h_tsttspot, delta_c_tsttspot, col = "red")
points(mean(delta_h_tsttspot), mean(delta_c_tsttspot), col = "red", pch = 16, cex = 1.5)

abline(h = 0)
abline(v = 0)


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

m <- bcea(-eff, -cost, Kmax = 30000, ref = 3,
          interventions = c("TST QFT", "TST TSPOT", "TST", "TSPOT", "QFT"))

ceplane.plot(m, graph = "base")#, xlim = c(0.15,0.17), pos = "bottomleft")
ceplane.plot(m, graph = "ggplot")#, pos = "bottomleft") + xlim(0.15,0.17)

mypalette <- RColorBrewer::brewer.pal(4, "Set2")
ceplane.plot(m, graph = "ggplot", pos = TRUE,
             ICER_sizes = 2,
             point_colors = mypalette,
             xlim = c(-0.007, 0.007), ylim = c(-220, 20))

ceac.plot(m)

# simultaneous comparisons
m_simul <- multi.ce(m)
ceac.plot(m_simul)

## tables

BCEA::tabulate_means(m)

ce_table(m)

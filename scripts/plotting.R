
#########
# plots #
#########

library(BCEA)

dat <- list()
dat$TST_QFT <- readRDS(file = "data/res_TST+QFT.RDS")
dat$TSPOT <- readRDS(file = "data/res_TSPOT.RDS")
dat$TST <- readRDS(file = "data/res_TST.RDS")
dat$QFT <- readRDS(file = "data/res_QFT.RDS")
dat$TST_TSPOT <- readRDS(file = "data/res_TST+TSPOT.RDS")


filenames <- Sys.glob("data/res_*.RDS")
dat <- map(filenames, readRDS)

delta_c1 <- dat$TST_QFT$cost - dat$TST$cost
delta_h1 <- dat$TST_QFT$health - dat$TST$health

delta_c2 <- dat$TSPOT$cost - dat$TST$cost
delta_h2 <- dat$TSPOT$health - dat$TST$health

delta_c3 <- dat$QFT$cost - dat$TST$cost
delta_h3 <- dat$QFT$health - dat$TST$health

delta_c4 <- dat$TST_TSPOT$cost - dat$TST$cost
delta_h4 <- dat$TST_TSPOT$health - dat$TST$health

plot(delta_h2, delta_c2)#, xlim = c(-0.01, 0.17))
points(mean(delta_h2), mean(delta_c2), col = "black", pch = 16)

points(delta_h1, delta_c1, col = "green")

points(delta_h3, delta_c3, col = "blue")
points(mean(delta_h3), mean(delta_c3), col = "blue", pch = 16)

points(delta_h4, delta_c4, col = "red")
points(mean(delta_h4), mean(delta_c4), col = "red", pch = 16)

abline(h = 0)
abline(v = 0)

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

m <- bcea(eff, cost, Kmax=500,
          interventions = c("TST QFT", "TST TSPOT", "TST", "TSPOT", "QFT"))

ceplane.plot(m, graph = "base", xlim = c(0.15,0.17), pos = "bottomleft")
ceplane.plot(m, graph = "ggplot", pos = "bottomleft") + xlim(0.15,0.17)

mypalette <- RColorBrewer::brewer.pal(4, "Set2")
ceplane.plot(m, graph = "ggplot", pos = TRUE, ICER_sizes = 2, point_colors = mypalette) + xlim(0.16, 0.17)

ceac.plot(m)

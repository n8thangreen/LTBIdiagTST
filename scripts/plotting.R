
#########
# plots #
#########

library(BCEA)

dat <- list()
dat$TST_QFT <- readRDS(file = "data/res_TST+QFT.RDS")
dat$TST <- readRDS(file = "data/res_TST.RDS")
dat$QFT <- readRDS(file = "data/res_QFT.RDS")
dat$TST_TSPOT <- readRDS(file = "data/res_TST+TSPOT.RDS")
dat$TSPOT <- readRDS(file = "data/res_TSPOT.RDS")





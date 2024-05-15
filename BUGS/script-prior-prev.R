
# run BUGS model script
# placing a prior directly on prevalence
# rather than function in terms of other parameters

library(R2jags)


dataJags <-
  list(
    N = 381,             # total screening test results adult close contacts
    n_accept_tst = 366,      # accept attend TST reading
    n_accept_igra = 378,     # accept IGRA
    n_tst_pos = 204,         # TST positive
    n_igra_pos = 87,         # IGRA positive
    #n_igra_pos_tst_pos = 78, # IGRA positive & TST positive
    # n_igra_pos_tst_neg = 9,  # IGRA positive & TST negative
    mu_sens_tst = 0.79,      # hyper-parameters
    mu_spec_tst = 0.59,
    mu_sens_qft = 0.886,  #0.80,
    mu_spec_qft = 0.995,  #0.97,
    sd_sens_tst = 0.03,
    sd_spec_tst = 0.05,
    sd_sens_qft = 0.029,  #0.015,
    sd_spec_qft = 0.0025)  #0.005)

filein <- "BUGS/model-prior-prev.txt"

# probabilities
params <- c("p_accept_tst",
            "p_accept_igra",
            "p_igra_pos",
            "p_tst_pos",
            "p_igra_pos_tst_pos",
            "PPV_tst",
            "PPV_igra",
            "NPV_tst",
            "NPV_igra",
            "sens_tst",
            "sens_qft",
            "spec_tst",
            "spec_qft",
            "PPV_igra_TST_pos",
            "NPV_igra_TST_pos",
            "prev")

n.iter <- 30000
n.burnin <- 1000
n.thin <- floor((n.iter - n.burnin)/500)

res <-
  jags(data = dataJags,
       inits = NULL,
       parameters.to.save = params,
       model.file = filein,
       n.chains = 1,
       n.iter,
       n.burnin,
       n.thin,
       DIC = TRUE)

R2WinBUGS::attach.bugs(res$BUGSoutput)

# save(res, file = "data/jags_output.RData")
x11()
# png("plots/posterior-densities.png", width = 800, height = 800)
par(mfrow = c(4,5))
dat <- as.mcmc(res)
dat[[1]] <- dat[[1]][, -1]
densplot(as.mcmc(dat), xlim = c(0,1))
# dev.off()

library(dplyr)

# posterior summary table for paper

tab <-
  summary(dat, quantiles = c(0.5, 0.025, 0.975))$quantiles %>%
  round(2)

tab <-
  as.data.frame(tab) |>
  tibble::rownames_to_column("label") |>
  mutate(label = as.factor(label))

# harmonize names
levels(tab$label) <-
  list(pAccept_TST = "p_accept_tst",
       pAccept_IGRA = "p_accept_igra",
       TST_pos = "p_tst_pos",
       IGRA_pos = "p_igra_pos",
       TSTIGRA_pos = "p_igra_pos_tst_pos",
       TSTIGRA_neg = "p_igra_pos_tst_neg",
       PPV_TST = "PPV_tst",
       NPV_TST = "NPV_tst",
       # PPV_QFT = "PPV_igra",
       # NPV_QFT = "NPV_igra",
       # PPV_TSPOT = "PPV_igra",
       # NPV_TSPOT = "NPV_igra",
       NPV_IGRA = "NPV_igra",
       PPV_IGRA = "PPV_igra",
       prev_TST = "prev")

tab

# reorder
tab <- arrange(tab, label)

tab$CrI <- paste0(tab$`2.5%`, ", ", tab$`97.5%`)

tab <- tab[, c('label', '50%', 'CrI')]

tab

write.csv(tab, file = "data/posterior_summary_table.csv")


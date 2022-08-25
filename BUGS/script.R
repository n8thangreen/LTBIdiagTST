
# run BUGS model script

library(R2jags)


dataJags <-
  list(
    N = 381,             # total screening test results adult close contacts
    n_accept_tst = 366,      # accept attend TST reading
    n_accept_igra = 378,     # accept IGRA
    n_tst_pos = 204,         # TST positive
    n_igra_pos = 87,         # TST negative
    n_igra_pos_tst_pos = 78, # IGRA positive & TST positive
    n_igra_pos_tst_neg = 9)  # IGRA positive & TST negative

filein <- "BUGS/model.txt"

# probabilities
params <- c("p_accept_tst",
            "p_accept_igra",
            "p_igra_pos",
            "p_tst_pos",
            "p_igra_pos_tst_pos",
            "p_igra_pos_tst_neg",
            "PPV_tst",
            "PPV_igra",
            # "PPV_qft",
            # "PPV_tspot",
            "NPV_tst",
            "NPV_igra",
            # "NPV_qft",
            # "NPV_tspot",
            "prev_tst",
            "prev_igra")

n.iter <- 1000
n.burnin <- 500
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

save(res, file = "data/jags_output.RData")

par(mfrow = c(3,4))
dat <- as.mcmc(res)
dat[[1]] <- dat[[1]][, -1]
densplot(as.mcmc(dat))


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
       prev_TST = "prev_tst",
       prev_IGRA = "prev_igra")

tab

# reorder
tab <- arrange(tab, label)

tab$CrI <- paste0(tab$`2.5%`, ", ", tab$`97.5%`)

tab <- tab[, c('label', '50%', 'CrI')]

tab

write.csv(tab, file = "data/posterior_summary_table.csv")


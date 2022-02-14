
# run BUGS model script

library(R2jags)


dataJags <-
  list(
    N = 381,
    n_accept_tst = 366,
    n_accept_igra = 378,
    n_tst_pos = 204,
    n_igra_pos = 87,
    n_igra_pos_tst_pos = 78,
    n_igra_pos_tst_neg = 9)

filein <- "BUGS/model.txt"

# probabilities
params <- c("p_accept_tst",
            "p_accept_igra",
            "p_igra_pos",
            "p_tst_pos",
            "p_igra_pos_tst_pos",
            "p_igra_pos_tst_neg")

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

par(mfrow = c(3,3))
densplot(as.mcmc(res))

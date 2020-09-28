
# Probability Sensitivity Analysis
#
#

library(purrr)

probs_new <-
  probs %>%
  transmat_to_long() %>%
  match_branch_to_label(pname_from_to) %>%
  as.tibble() %>%
  match_branchlabel_to_prob(label_probs_distns)


tree_dat_sa <- list()

for (i in 1:2) {

  tree_dat_sa[[i]] <-
    define_model(
      tree_dat =
        list(child = TST_TSPOT_tree,
             dat = data.frame(
               node = probs_new$to,
               from = probs_new$from,
               to = probs_new$to,
               prob = lapply(probs_new$prob, sample_distributions) %>% unlist(),
               cost = lapply(probs_new$prob, sample_distributions) %>% unlist())
        ))

  tree_dat_sa[[i]]$dat <-
    fill_complementary_probs(tree_dat_sa[[i]]$dat) %>%
    rename(vals = cost) %>%
    mutate(vals = ifelse(is.na(vals), 0, vals))
}


# wrappers for sampling in PSA

# recursive format
for (i in seq_along(tree_dat_sa)) {

  tree_dat_sa[[i]]$dat <- rbind(c(1,NA,1,NA,0),
                                tree_dat_sa[[i]]$dat)
}
lapply(tree_dat_sa, function(x) dectree_expected_values(define_model(tree_dat = x)))

# long dataframe format
dat_long <- map(map(tree_dat_sa, "dat"), select, -node)
lapply(dat_long, function(x) dectree_expected_values(define_model(dat_long = x)))

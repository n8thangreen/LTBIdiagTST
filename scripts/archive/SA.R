
# replicate Manabu's TB TST(+IGRA)
# cost-effectiveness model from Excel
#
#


library(readr)
library(dplyr)
library(tibble)
library(reshape2)
library(treeSimR)
library(assertthat)


load(here::here("../../MSc-MPH/projects/2017/LTBI-TST_Manabu/R/params.RData"))
load(here::here("../../MSc-MPH/projects/2017/LTBI-TST_Manabu/R/trees.RData"))

# probs <- child_list_to_transmat(TST_tree)
# probs <- child_list_to_transmat(QFT_tree)
# probs <- child_list_to_transmat(TSPOT_tree)
# probs <- child_list_to_transmat(TST_QFT_tree)
probs <- child_list_to_transmat(TST_TSPOT_tree)

# pname_from_to <- TST_pname_from_to
# pname_from_to <- QFT_pname_from_to
# pname_from_to <- TSPOT_pname_from_to
# pname_from_to <- TST_QFT_pname_from_to
pname_from_to <- TST_TSPOT_pname_from_to

# cname_from_to <- TST_cname_from_to
# cname_from_to <- QFT_cname_from_to
# cname_from_to <- TSPOT_cname_from_to
# cname_from_to <- TST_QFT_cname_from_to
cname_from_to <- TST_TSPOT_cname_from_to


#########
# probs #
#########

label_probs_long <-
  as_tibble(label_probs) %>%
  melt(value.name = "prob",
       variable.name = "name")

probs_new <-
  probs %>%
  transmat_to_long() %>%
  match_branch_to_label(pname_from_to) %>%
  match_branchlabel_to_prob(label_probs_long) %>%
  fill_complementary_probs()

probs <- insert_to_probmat(dat = probs_new, mat = probs)

write.csv(probs, here::here("../../MSc-MPH/projects/2017/LTBI-TST_Manabu/R/probs.csv"))


########
# cost #
########

label_cost_long <-
  as_tibble(label_costs) %>%
  melt(value.name = "cost",
       variable.name = "name")

costs_names <-
  merge(cname_from_to, label_cost_long,
        by = "name", all.x = TRUE) %>%
  mutate(from = as.numeric(as.character(from)),
         to = as.numeric(as.character(to)))

costs <- as.tibble(matrix(NA_real_,
                          nrow = nrow(probs), ncol = ncol(probs)))

costs <- insert_to_costmat(costs_names, costs)


# model -------------------------------------------------------------------

res <-
  dectree_expected_values(vals = costs,
                          p = probs)


res[1] + 44.31


# list of deterministic scenarios

all_long <-
  merge(costs_names, probs_new,
        all = TRUE, by = c("from", "to")) %>%
  rename(vals = cost) %>%
  select(-contains("name"))

dat <-
  list(all_long,
       all_long)

# wrapper for sampling in SA
dectree_expected_values(define_model(dat_long = all_long))
lapply(dat, function(x) dectree_expected_values(define_model(dat_long = x)))




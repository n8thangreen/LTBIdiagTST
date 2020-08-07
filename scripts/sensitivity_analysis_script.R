
library(readr)
library(CEdecisiontree)

probs <- read_csv("data-raw/TB_decision-tree-probs.csv")

probs[1, 2] <-  0.5

dectree_expected_values(vals = probs,
                        p = probs)



test_that("term_pop_sa sum to 1", {

  x <- rowSums(c_dt$term_pop_sa) %>% round(2)
  expect_equal(rep(1, nrow(c_dt$term_pop_sa)), x)
})




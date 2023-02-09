
# decision tree final state groups
# for Markov model starting states

create_state_lists <- function() {
  state_lists <-
    list(
      "QFT" =
        list(
          no_LTBI  = c(16, 18, 19, 21, 26, 29),
          LTBI_complete_Tx  = 9,
          LTBI_incomplete_Tx = c(7, 10),
          LTBI_no_Tx = c(12, 24, 28),
          activeTB = c(),
          dead = c()),
      "TSPOT" =
        list(
          no_LTBI  = c(16, 18, 19, 21, 26, 29),
          LTBI_complete_Tx  = 9,
          LTBI_incomplete_Tx = c(7, 10),
          LTBI_no_Tx = c(12, 24, 28),
          activeTB = c(),
          dead = c()),
      "TST_QFT" =
        list(
          no_LTBI = c(19, 21, 22, 24, 29, 32, 37, 40, 43),
          LTBI_complete_Tx = 12,
          LTBI_incomplete_Tx = c(10, 13),
          LTBI_no_Tx = c(15, 27, 31, 35, 39, 42),
          activeTB = c(),
          dead = c()),
      "TST_TSPOT" =
        list(
          no_LTBI  = c(19, 21, 22, 24, 29, 32, 37, 40, 43),
          LTBI_complete_Tx  = 12,
          LTBI_incomplete_Tx = c(10, 13),
          LTBI_no_Tx = c(15, 27, 31, 35, 39, 42),
          activeTB = c(),
          dead = c()),
      "TST" =
        list(
          no_LTBI  = c(18, 20, 21, 23, 28, 31, 34),
          LTBI_complete_Tx  = 11,
          LTBI_incomplete_Tx = c(9, 12),
          LTBI_no_Tx = c(14, 26, 30, 33),
          activeTB = NULL,
          dead = NULL))

  save(state_lists,
       file = here::here("data", "state_lists.RData"))
}

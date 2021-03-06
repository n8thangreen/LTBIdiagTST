
#' all trees structures for analysis
#'
create_trees <- function() {

  TST_IGRA_fwd_tree <-
    list(
      '1' = c(2,25),
      '2' = c(3,23),
      '3' = c(4,21),
      '4' = c(5,19),
      '5' = c(6,17),
      '6' = c(7,15),
      '7' = c(8,13),
      '8' = c(9,11),
      '9' = 10,
      '10' = NULL,
      '11' = c(12,48),
      '12' = NULL,
      '13' = 14,
      '14' = NULL,
      '15' = 16,
      '16' = NULL,
      '17' = 18,
      '18' = NULL,
      '19' = 20,
      '20' = NULL,
      '21' = 22,
      '22' = NULL,
      '23' = 24,
      '24' = NULL,
      '25' = c(26,46),
      '26' = c(27,44),
      '27' = c(28,42),
      '28' = c(29,40),
      '29' = c(30,38),
      '30' = c(31,36),
      '31' = c(32,34),
      '32' = 33,
      '33' = NULL,
      '34' = c(35,49),
      '35' = NULL,
      '36' = 37,
      '37' = NULL,
      '38' = 39,
      '39' = NULL,
      '40' = 41,
      '41' = NULL,
      '42' = 43,
      '43' = NULL,
      '44' = 45,
      '45' = NULL,
      '46' = 47,
      '47' = NULL,
      '48' = NULL,
      '49' = NULL
    )


  IGRA_tree <-
    list(
      '1' = c(2,27),
      '2' = c(3,22),
      '3' = c(4,13),
      '4' = c(5,11),
      '5' = c(6,8),
      '6' = c(7),
      '7' = NULL,
      '8' = c(9,10),
      '9' = NULL,
      '10' = NULL,
      '11' = c(12),
      '12' = NULL,
      '13' = c(14,20),
      '14' = c(15,17),
      '15' = c(16),
      '16' = NULL,
      '17' = c(18,19),
      '18' = NULL,
      '19' = NULL,
      '20' = c(21),
      '21' = NULL,
      '22' = c(23,25),
      '23' = c(24),
      '24' = NULL,
      '25' = c(26),
      '26' = NULL,
      '27' = c(28,29),
      '28' = NULL,
      '29' = NULL
    )

  QFT_tree <- IGRA_tree
  TSPOT_tree <- IGRA_tree

  TST_tree <-
    list(
      '1' = 2,
      '2' = c(3,32),
      '3' = c(4,29),
      '4' = c(5,24),
      '5' = c(6,15),
      '6' = c(7,13),
      '7' = c(8,10),
      '8' = c(9),
      '9' = NULL,
      '10' = c(11,12),
      '11' = NULL,
      '12' = NULL,
      '13' = c(14),
      '14' = NULL,
      '15' = c(16,22),
      '16' = c(17,19),
      '17' = c(18),
      '18' = NULL,
      '19' = c(20,21),
      '20' = NULL,
      '21' = NULL,
      '22' = c(23),
      '23' = NULL,
      '24' = c(25,27),
      '25' = c(26),
      '26' = NULL,
      '27' = c(28),
      '28' = NULL,
      '29' = c(30,31),
      '30' = NULL,
      '31' = NULL,
      '32' = c(33,34),
      '33' = NULL,
      '34' = NULL
    )

  TST_IGRA_tree <-
    list(
      '1' = c(2,41),
      '2' = c(3,38),
      '3' = c(4,33),
      '4' = c(5,30),
      '5' = c(6,25),
      '6' = c(7,16),
      '7' = c(8,14),
      '8' = c(9,11),
      '9' = c(10),
      '10' = NULL,
      '11' = c(12,13),
      '12' = NULL,
      '13' = NULL,
      '14' = c(15),
      '15' = NULL,
      '16' = c(17,23),
      '17' = c(18,20),
      '18' = c(19),
      '19' = NULL,
      '20' = c(21,22),
      '21' = NULL,
      '22' = NULL,
      '23' = c(24),
      '24' = NULL,
      '25' = c(26,28),
      '26' = c(27),
      '27' = NULL,
      '28' = c(29),
      '29' = NULL,
      '30' = c(31,32),
      '31' = NULL,
      '32' = NULL,
      '33' = c(34,36),
      '34' = c(35),
      '35' = NULL,
      '36' = c(37),
      '37' = NULL,
      '38' = c(39,40),
      '39' = NULL,
      '40' = NULL,
      '41' = c(42,43),
      '42' = NULL,
      '43' = NULL
    )

  TST_QFT_tree <- TST_IGRA_tree
  TST_TSPOT_tree <- TST_IGRA_tree

  save(QFT_tree,
       TSPOT_tree,
       TST_tree,
       TST_QFT_tree,
       TST_TSPOT_tree,
       TST_IGRA_fwd_tree,
       file = here::here("data", "trees.RData"))
}


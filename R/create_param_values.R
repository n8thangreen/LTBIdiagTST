
#' all parameter values required
#' for the decision tree model
#'
#' Excel model with structure and labelling:
#' G:/DIDE-PC_2019/MSc-MPH/projects/2017/LTBI-TST_Manabu/decision tree Excel/all_decision_trees.xls
#' @export
#'
#' @examples
#' create_param_values()
#'
create_param_values <- function() {

  # assign values to labels ------------------------------------------

  label_costs <-
    list(
      "Contact tracing per contact" = 368.9,
      "Mean number of contacts examined per primary case"	= 6.5,
      "Total Contact tracing" = 2397.85,

      "Cost of inpatient episode for acute TB" = 3325.15,
      "Proportion of patients with acute TB who are admitted"	= 0.53,
      "Total Inpatient care" = 1762.3295,

      "Cost of culture test" = 8.06,
      "Culture tests per case" = 4,
      "CXR per case" = 2,
      "LFT per case" = 4,
      "Total Cost of tests" = 104.74,

      "HRZE tablet (60)" = 39.5,
      "_tablets per month"	= 150,
      "HRZE per month" = 98.75,
      "HR tablet 300/150 (56)" = 25.22,
      "tablets per month" = 60,
      "HR per month" = 27.02142857,
      "Duration of initial therapy" = 2,
      "Duration of continious therapy" = 4,
      "Total Cost of chemotherapy" = 305.5857143,

      "Cost of outpatient consultation (first visit)" = 208,
      "Cost of outpatient consultation (follow-up visit)" = 94,
      "Number of outpatient clinical visits per case" = 4,
      "Visits from TB nurse per case" = 6,
      "Total Out patient care" = 755.86,

      "TST" = 18.62,
      "QFT" = 23.65,
      "TB special nurse visit" = 44.31,
      "TSPOT" = 35.12,

      "Out patient consultation (first visit)" = 208,
      "CXR" = 30.21,
      "LFT" = 3.02,
      "Number of outpatient consultation" = 1,
      "Number of CXR" = 1,
      "Number of LFT" = 1,
      "Total Cost of positive screening" = 241.23,

      "Follow-up via nurses" = 44.31, #22.15, 66.46,
      "HR tablet (300/150) (56)" = 25.22,
      "drug cost per month" = 27.02,
      "Number of TB nurse appointments"	= 2,
      "Duration of HR" = 3,

      "Total (complete)" = 169.68	,
      "Total (incomplete)" = 84.84,

      "Hep" = 732.13)

  label_costs_distns <-
    tribble(
      ~name.cost,   ~vals,
      # "TST",   list(distn = "pert", params = c(mode = 18.62, min = 9.31, max = 37.24)),    # Pooran 2010
      "TST",   list(distn = "unif", params = c(min=9.31, max=37.24)),
      # "QFT",   list(distn = "pert", params = c(mode = 23.65, min = 12.33, max = 45.29)),
      "QFT",   list(distn = "unif", params = c(min=35, max=80)),
      # "TSPOT", list(distn = "pert", params = c(mode = 35.12, min = 18.06, max = 68.23)),
      "TSPOT", list(distn = "unif", params = c(min=18.06, max=68.23)),                     # Imperial
      "CXR",   list(distn = "pert", params = c(mode = 30.21, min = 23.16, max = 35.25)),
      "LFT",   list(distn = "pert", params = c(mode = 3.02, min = 2.01, max = 4.03)),
      # "Hep",   list(distn = "pert", params = c(mode = 732.13, min = 366.06, max = 1464.25)),
      "Hep",   list(distn = "unif", params = c(min=366.06, max=1464.25)),                   # Pooran 2010
      "TB",    list(distn = "pert", params = c(mode = 4925.76, min = 2462.88, max = 9851.52)),
      #
      "Total (complete)", list(distn = "unif", params = c(min=169.68, max=169.68)),
      "Total (incomplete)", list(distn = "unif", params = c(min=84.84,  max=84.84)),
      "Total Cost of positive screening",  list(distn = "unif", params = c(min=233.17, max=247.28)),
      # "TB special nurse visit",    list(distn = "pert", params = c(mode = 44.31, min = 22.15, max = 66.46)),
      "TB special nurse visit", list(distn = "unif", params = c(min=22.15, max=66.23))
    )

  label_probs_distns <-
    tribble(
      ~name.prob,         ~prob,
      # "TST_sens",    list(distn = "pert", params = c(mode = 0.79, min = 0.69, max = 0.89)),
      # "TST_spec",    list(distn = "pert", params = c(mode = 0.59, min = 0.46, max = 0.73)),
      "TST_sens",    list(distn = "unif", params = c(min=0.73, max=1.0)),                     # Warwick evidence
      "TST_spec",    list(distn = "unif", params = c(min=0.12, max=0.53)), # Kik (2010) lower, Harstad (2010) upper from Warwick evidence
      # "QFT_sens",    list(distn = "pert", params = c(mode = 0.77, min = 0.8, max = 0.84)),
      # "QFT_spec",    list(distn = "pert", params = c(mode = 0.97, min = 0.94, max = 0.99)),
      "QFT_sens",    list(distn = "unif", params = c(min=0.81, max=0.87)),                    # Diel (2010)
      "QFT_spec",    list(distn = "unif", params = c(min=0.98, max=1.0)),
      # "TSPOT_sens",  list(distn = "pert", params = c(mode = 0.9, min = 0.87, max = 0.93)),
      # "TSPOT_spec",  list(distn = "pert", params = c(mode = 0.95, min = 0.92, max = 0.98)),
      "TSPOT_sens",   list(distn = "unif", params = c(min=0.85, max=0.93)),                 # NICE appendix (2010)
      "TSPOT_spec",   list(distn = "unif", params = c(min=0.86, max=1.0)),
      # "pAccept_chemo", list(distn = "pert", params = c(mode = 0.95, min = 0.5, max = 1)),
      "pAccept_chemo",  list(distn = "unif", params = c(min=0.5,   max=1)),                   # Pareek 2013
      # "pAccept_chemo",list(distn = "unif", params = c(min=0.53, max=0.98)),                 # Alsdurf (2016) table 2
      # "pComp_chemo", list(distn = "pert", params = c(mode = 0.8, min = 0.5, max =  0.9)),
      "pComp_chemo",    list(distn = "unif", params = c(min=0.5,   max=0.9)),                 # Kowada 2013
      # "pComp_chemo",  list(distn = "unif", params = c(min=0.28, max=0.97)),                   # Alsdurf (2016) table 1
      "pHep",        list(distn = "unif", params = c(min=0.001, max=0.003)),                  # Kunst, Pareek
      # "pHep",        list(distn = "pert", params = c(mode = 0.002, min = 0.001, max = 0.003)),
      "Eff_comp",    list(distn = "pert", params = c(mode = 0.65, min = 0.5, max = 0.8)),
      "Eff_incomp",  list(distn = "pert", params = c(mode = 0.21, min = 0.1, max = 0.3)),
      "pTB",         list(distn = "pert", params = c(mode = 0.12, min = 0.08,	max = 0.19)),
      #
      # "pAccept_TST",  list(distn = "pert", params = c(mode = 0.982, min = 0.982, max = 0.982)),
      "pAccept_TST",  list(distn = "unif", params = c(min=0.5,   max=1)),                      # Campbell (2017)
      # "pTSTread",     list(distn = "pert", params = c(mode = 0.979, min = 0.979, max = 0.979)),
      "pTSTread",     list(distn = "unif", params = c(min=0.979, max=0.979)),
      # "pTSTread",     list(distn = "unif", params = c(min=0.90, max=0.99)),                  # Alsdurf (2016) table S3
      "pIGRAread",    list(distn = "pert", params = c(mode = 1, min = 1, max = 1)),
      "pAccept_IGRA", list(distn = "pert", params = c(mode = 0.992, min = 0.992, max = 0.992)),
      # "pAccept_IGRA_TST+", list(distn = "pert", params = c(mode = 0.995, min = 0.995, max = 0.995)),
      "pAccept_IGRA_TST+", list(distn = "unif", params = c(min=0.995, max=0.995)),
      "TSTIGRA_pos",  list(distn = "pert", params = c(mode = 0.214, min = 0.214, max = 0.214)),
      "Dual_sens",    list(distn = "pert", params = c(mode = 0.632, min = 0.632, max = 0.632)),
      "Dual_spec",    list(distn = "pert", params = c(mode = 0.988, min = 0.988, max = 0.988)),
      # "pLTBI",        list(distn = "pert", params = c(mode = 0.326, min = 0.326, max = 0.326)),
      "pLTBI",        list(distn = "unif", params = c(min=0.326, max=0.326)),
      # "pLTBI",        list(distn = "unif", params = c(min=0.10, max=0.40)),                   # Pooran (2010)
      # "TST_pos",      list(distn = "pert", params = c(mode = 0.534, min = 0.534, max = 0.534)),
      "TST_pos",      list(distn = "unif", params = c(min=0.534, max=0.534)),
      # "PPV_TST",      list(distn = "pert", params = c(mode = 0.482, min = 0.482, max = 0.482)),
      "PPV_TST",      list(distn = "unif", params = c(min=0.482, max=0.482)),
      # "NPV_TST",      list(distn = "pert", params = c(mode = 0.853, min = 0.853, max = 0.853)),
      "NPV_TST",      list(distn = "unif", params = c(min=0.853, max=0.853)),
      "QFT_pos",      list(distn = "pert", params = c(mode = 0.281, min = 0.281, max = 0.281)),
      "PPV_QFT",      list(distn = "pert", params = c(mode = 0.928, min = 0.928, max = 0.928)),
      "NPV_QFT",      list(distn = "pert", params = c(mode = 0.909, min = 0.909, max = 0.909)),
      "TSPOT_pos",    list(distn = "pert", params = c(mode = 0.327, min = 0.327, max = 0.327)),
      "PPV_TSPOT",    list(distn = "pert", params = c(mode = 0.897, min = 0.897, max = 0.897)),
      "NPV_TSPOT",    list(distn = "pert", params = c(mode = 0.952, min = 0.952, max = 0.952)),
      "QFT_pos_TST+", list(distn = "pert", params = c(mode = 0.401, min = 0.401, max = 0.401)),
      "PPV_QFT_TST+", list(distn = "pert", params = c(mode = 0.961, min = 0.961, max = 0.961)),
      "NPV_QFT_TST+", list(distn = "pert", params = c(mode = 0.839, min = 0.839, max = 0.839)),
      # "TSPOT_pos_TST+", list(distn = "pert", params = c(mode = 0.460, min = 0.460, max = 0.460)),
      "TSPOT_pos_TST+", list(distn = "unif", params = c(min=0.46,  max=0.46)),
      # "PPV_TSPOT_TST+", list(distn = "pert", params = c(mode = 0.944, min = 0.944, max = 0.944)),
      "PPV_TSPOT_TST+", list(distn = "unif", params = c(min=0.944, max=0.944)),
      # "NPV_TSPOT_TST+", list(distn = "pert", params = c(mode = 0.911, min = 0.911, max = 0.911)),
      "NPV_TSPOT_TST+", list(distn = "unif", params = c(min=0.911, max=0.911)),
      "pReact",       list(distn = "pert", params = c(mode = 0.001936869, min = 0.001936869, max = 0.001936869)),
      "pReact_comp",  list(distn = "pert", params = c(mode = 0.000677904, min = 0.000677904, max = 0.000677904)),
      "pReact_incomp", list(distn = "pert", params = c(mode = 0.001530127, min = 0.001530127, max = 0.001530127))
    )

  label_probs <-
    list(
      "pAccept_TST" = 0.982,
      "pTSTread" = 0.979,
      "pIGRAread" = 1,
      "pAccept_IGRA" = 0.992,
      "pAccept_IGRA_TST+" = 0.995,
      "TSTIGRA_pos" = 0.214,

      "Dual_sens" = 0.632,
      "Dual_spec" = 0.988,
      "TST_sens" = 0.8,  ##TODO: better numbers here
      "TST_spec" = 0.9,
      "QFT_sens" = 0.9,
      "QFT_spec" = 0.95,
      "TSPOT_sens" = 0.91,
      "TSPOT_spec" = 0.96,

      "pLTBI"	= 0.326,
      "TST_pos"	= 0.534,
      "PPV_TST"	= 0.482,
      "NPV_TST"	= 0.853,
      "QFT_pos"	= 0.281,
      "PPV_QFT"	= 0.928,
      "NPV_QFT"	= 0.909,
      "TSPOT_pos" = 0.327,
      "PPV_TSPOT" = 0.897,
      "NPV_TSPOT" = 0.952,
      "QFT_pos_TST+" = 0.401,
      "PPV_QFT_TST+" = 0.961,
      "NPV_QFT_TST+" = 0.839,
      "TSPOT_pos_TST+" = 0.460,
      "PPV_TSPOT_TST+" = 0.944,
      "NPV_TSPOT_TST+" = 0.911,
      "pAccept_chemo" = 0.95,
      "pComp_chemo" = 0.8,
      "pHep" = 0.002,
      "pReact" = 0.001936869,
      "pReact_comp" = 0.000677904,
      "pReact_incomp" = 0.001530127)

  hsuv <-
    list(
      "loss_chemo" = 0.01,
      "loss_hep" = 0.22,
      "loss_tb" = 0.15)


  # assign labels to branches -----------------------------------------------

  #########
  # costs #
  #########

  TST_cname_from_to <-
    rbind.data.frame(
      c("TB special nurse visit", 1, 2),
      c("TST", 2, 3),
      c("TB special nurse visit", 3, 4),
      c("Total Cost of positive screening", 4, 5),
      c("Hep", 7, 8),
      c("Total (incomplete)", 8, 9),
      c("Total (complete)", 10, 11),
      c("Total (incomplete)", 10, 12),
      c("Hep", 16, 17),
      c("Total (incomplete)", 17, 18),
      c("Total (complete)", 19, 20),
      c("Total (incomplete)", 19, 21)
    ) %>%
    setNames(c("name", "from", "to"))

  QFT_cname_from_to <-
    rbind.data.frame(
      c("QFT", 1, 2),
      c("Total Cost of positive screening", 2, 3),
      c("Hep", 5, 6),
      c("Total (incomplete)", 6, 7),
      c("Total (complete)", 8, 9),
      c("Total (incomplete)", 8, 10),
      c("Hep", 14, 15),
      c("Total (incomplete)", 15, 16),
      c("Total (complete)", 17, 18),
      c("Total (incomplete)", 17, 19)
    ) %>%
    setNames(c("name", "from", "to"))

  TSPOT_cname_from_to <-
    rbind.data.frame(
      c("TSPOT", 1, 2),
      c("Total Cost of positive screening", 2, 3),
      c("Hep", 5, 6),
      c("Total (incomplete)", 6, 7),
      c("Total (complete)", 8, 9),
      c("Total (incomplete)", 8, 10),
      c("Hep", 14, 15),
      c("Total (incomplete)", 15, 16),
      c("Total (complete)", 17, 18),
      c("Total (incomplete)", 17, 19)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_QFT_cname_from_to <-
    rbind.data.frame(
      c("TST", 1, 2),
      c("TB special nurse visit", 2, 3),
      c("QFT", 4, 5),
      c("Total Cost of positive screening", 5, 6),
      c("Hep", 8, 9),
      c("Total (incomplete)", 9, 10),
      c("Total (complete)", 11, 12),
      c("Total (incomplete)", 11, 13),
      c("Total (incomplete)", 18, 19),
      c("Total (complete)", 20, 21),
      c("Total (incomplete)", 20, 22)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_TSPOT_cname_from_to <-
    rbind.data.frame(
      c("TST", 1, 2),
      c("TB special nurse visit", 2, 3),
      c("TSPOT", 4, 5),
      c("Total Cost of positive screening", 5, 6),
      c("Hep", 8, 9),
      c("Total (incomplete)", 9, 10),
      c("Total (complete)", 11, 12),
      c("Total (incomplete)", 11, 13),
      c("Total (incomplete)", 18, 19),
      c("Total (complete)", 20, 21),
      c("Total (incomplete)", 20, 22)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_QFT_fwd_cname_from_to <-
    rbind.data.frame(
      c("TST", 2, 3),
      c("TST", 25, 26),
      c("TB special nurse visit", 3, 4),
      c("TB special nurse visit", 26, 27),
      c("QFT", 5, 6),
      c("QFT", 28, 29),
      c("Total Cost of positive screening", 6, 7),
      c("Total Cost of positive screening", 29, 30),
      c("Hep", 8, 9),
      c("Hep", 31, 32),
      c("Total (incomplete)", 9, 10),
      c("Total (incomplete)", 32, 33),
      c("Total (complete)", 11, 12),
      c("Total (complete)", 34, 35)
    ) %>%
    setNames(c("name", "from", "to"))


  #########
  # probs #
  #########

  TST_pname_from_to <-
    rbind.data.frame(
      c("pAccept_TST", 2, 3),
      c("pTSTread", 3, 4),
      c("TST_pos", 4, 5),
      c("PPV_TST", 5, 6),
      c("pAccept_chemo", 6, 7),
      c("pHep", 7, 8),
      c("pComp_chemo", 10, 11),
      c("pAccept_chemo", 15, 16),
      c("pHep", 16, 17),
      c("pComp_chemo", 19, 20),
      c("NPV_TST", 24, 27),
      c("pLTBI", 29, 30),
      c("pLTBI", 32, 33)
    ) %>%
    setNames(c("name", "from", "to"))

  QFT_pname_from_to <-
    rbind.data.frame(
      c("pAccept_IGRA", 1, 2),
      c("QFT_pos", 2, 3),
      c("PPV_QFT", 3, 4),
      c("pAccept_chemo", 4, 5),
      c("pHep", 5, 6),
      c("pComp_chemo", 8, 9),
      c("pAccept_chemo", 13, 14),
      c("pHep", 14, 15),
      c("pComp_chemo", 17, 18),
      c("NPV_QFT", 22, 25),
      c("pLTBI", 27, 28)
    ) %>%
    setNames(c("name", "from", "to"))

  TSPOT_pname_from_to <-
    rbind.data.frame(
      c("pAccept_IGRA", 1, 2),
      c("TSPOT_pos", 2, 3),
      c("PPV_TSPOT", 3, 4),
      c("pAccept_chemo", 4, 5),
      c("pHep", 5, 6),
      c("pComp_chemo", 8, 9),
      c("pAccept_chemo", 13, 14),
      c("pHep", 14, 15),
      c("pComp_chemo", 17, 18),
      c("NPV_TSPOT", 22, 25),
      c("pLTBI", 27, 28)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_TSPOT_pname_from_to <-
    rbind.data.frame(
      c("pAccept_TST", 1, 2),
      c("pTSTread", 2, 3),
      c("TST_pos", 3, 4),
      c("pAccept_IGRA_TST+", 4, 5),
      c("TSPOT_pos_TST+", 5, 6),
      c("PPV_TSPOT_TST+", 6, 7),
      c("pAccept_chemo", 7, 8),
      c("pHep", 8, 9),
      c("pComp_chemo", 11, 12),
      c("pAccept_chemo", 16, 17),
      c("pHep", 17, 18),
      c("pComp_chemo", 20, 21),
      c("NPV_TSPOT_TST+", 25, 28),
      c("PPV_TST", 30, 31),
      c("NPV_TST", 33, 36),
      c("pLTBI", 38, 39),
      c("pLTBI", 41, 42)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_QFT_pname_from_to <-
    rbind.data.frame(
      c("pAccept_TST", 1, 2),
      c("pTSTread", 2, 3),
      c("TST_pos", 3, 4),
      c("pAccept_IGRA_TST+", 4, 5),
      c("QFT_pos_TST+", 5, 6),
      c("PPV_QFT_TST+", 6, 7),
      c("pAccept_chemo", 7, 8),
      c("pHep", 8, 9),
      c("pComp_chemo", 11, 12),
      c("pAccept_chemo", 16, 17),
      c("pHep", 17, 18),
      c("pComp_chemo", 20, 21),
      c("NPV_QFT_TST+", 25, 28),
      c("PPV_TST", 30, 31),
      c("NPV_TST", 33, 36),
      c("pLTBI", 38, 39),
      c("pLTBI", 41, 42)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_QFT_fwd_pname_from_to <-
    rbind.data.frame(
      c("pLTBI", 1, 2),
      c("pAccept_TST", 2, 3),
      c("pAccept_TST", 25, 26),
      c("pTSTread", 3, 4),
      c("pTSTread", 26, 27),
      c("TST_sens", 4, 5),
      c("TST_spec", 27, 42),
      c("pAccept_IGRA", 5, 6),
      c("pAccept_IGRA", 28, 29),
      c("QFT_sens", 6, 7),
      c("QFT_spec", 29, 38),
      c("pAccept_chemo", 7, 8),
      c("pAccept_chemo", 30, 31),
      c("pHep", 8, 9),
      c("pHep", 31, 32),
      c("pComp_chemo", 11, 12),
      c("pComp_chemo", 34, 35)
    ) %>%
    setNames(c("name", "from", "to"))


  #############
  # QALY loss #
  #############

  t_chemo <- 3/12  #years
  t_hep <- 1.5/12

  label_health <-
    list(
      "Hep" = 0.22*t_hep,
      "Total (complete)" = 0.01*t_chemo,
      "Total (incomplete)" = 0.01*t_chemo/2)

  label_health_distns <-
    tribble(
      ~name.health,   ~vals,
      "Hep",
        list(distn = "unif", params = c(min=0.22*t_hep, max=0.22*t_hep)),
      "Total (complete)",
        list(distn = "unif", params = c(min= 0.01*t_chemo, max= 0.01*t_chemo)),
      "Total (incomplete)",
        list(distn = "unif", params = c(min= 0.01*t_chemo/2, max= 0.01*t_chemo/2)))

  TST_hname_from_to <-
    rbind.data.frame(
      c("Hep", 7, 8),
      c("Hep", 16, 17),
      c("Total (incomplete)", 8, 9),
      c("Total (incomplete)", 17, 18),
      c("Total (incomplete)", 10, 12),
      c("Total (incomplete)", 19, 21),
      c("Total (complete)", 10, 11),
      c("Total (complete)", 19, 20)) %>%
    setNames(c("name", "from", "to"))

  QFT_hname_from_to <-
    rbind.data.frame(
      c("Hep", 5, 6),
      c("Hep", 14, 15),
      c("Total (incomplete)", 6, 7),
      c("Total (incomplete)", 8, 10),
      c("Total (incomplete)", 15, 16),
      c("Total (incomplete)", 17, 19),
      c("Total (complete)", 8, 9),
      c("Total (complete)", 17, 18)) %>%
    setNames(c("name", "from", "to"))

  TSPOT_hname_from_to <-
    rbind.data.frame(
      c("Hep", 5, 6),
      c("Hep", 14, 15),
      c("Total (incomplete)", 6, 7),
      c("Total (incomplete)", 8, 10),
      c("Total (incomplete)", 15, 16),
      c("Total (incomplete)", 17, 19),
      c("Total (complete)", 8, 9),
      c("Total (complete)", 17, 18)) %>%
    setNames(c("name", "from", "to"))

  TST_QFT_hname_from_to <-
    rbind.data.frame(
      c("Hep", 8, 9),
      c("Hep", 17, 18),
      c("Total (incomplete)", 9, 10),
      c("Total (incomplete)", 11, 13),
      c("Total (incomplete)", 18, 19),
      c("Total (incomplete)", 20, 22),
      c("Total (complete)", 11, 12),
      c("Total (complete)", 20, 21)) %>%
    setNames(c("name", "from", "to"))

  TST_TSPOT_hname_from_to <-
    rbind.data.frame(
      c("Hep", 8, 9),
      c("Hep", 17, 18),
      c("Total (incomplete)", 9, 10),
      c("Total (incomplete)", 11, 13),
      c("Total (incomplete)", 18, 19),
      c("Total (incomplete)", 20, 22),
      c("Total (complete)", 11, 12),
      c("Total (complete)", 20, 21)) %>%
    setNames(c("name", "from", "to"))

  TST_QFT_fwd_hname_from_to <-
    rbind.data.frame(
      c("Hep", 8, 9),
      c("Hep", 31, 32),
      c("Total (incomplete)", 9, 10),
      c("Total (incomplete)", 11, 48),
      c("Total (incomplete)", 32, 33),
      c("Total (incomplete)", 34, 49),
      c("Total (complete)", 11, 12),
      c("Total (complete)", 34, 35)) %>%
    setNames(c("name", "from", "to"))


  save(label_costs,
       label_health,
       label_probs,
       label_costs_distns,
       label_health_distns,
       label_probs_distns,
       TST_cname_from_to,
       TST_hname_from_to,
       TST_pname_from_to,
       TST_QFT_cname_from_to,
       TST_QFT_hname_from_to,
       TST_QFT_pname_from_to,
       TST_QFT_fwd_cname_from_to,
       TST_QFT_fwd_pname_from_to,
       TST_QFT_fwd_hname_from_to,
       TSPOT_cname_from_to,
       TSPOT_hname_from_to,
       TSPOT_pname_from_to,
       TST_TSPOT_cname_from_to,
       TST_TSPOT_hname_from_to,
       TST_TSPOT_pname_from_to,
       QFT_cname_from_to,
       QFT_hname_from_to,
       QFT_pname_from_to,
       hsuv,
       file = here::here("data/params.RData"))
}


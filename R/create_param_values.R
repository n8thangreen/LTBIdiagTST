
#' Create parameter values
#'
#' All parameter values required for the decision tree model.
#'
#' Excel model with structure and labelling:
#' G:/DIDE-PC_2019/MSc-MPH/projects/2017/LTBI-TST_Manabu/decision tree Excel/all_decision_trees.xls
#' @export
#'
#' @examples
#' create_param_values()
#'
#' ## save lists as csv
#' # write.csv(cbind(label_costs), file = "data/cost_inputs.csv")
#' # write.csv(cbind(label_health), file = "data/health_inputs.csv")
#' # write.csv(cbind(label_probs), file = "data/probs_inputs.csv")
#'
create_param_values <- function(save = TRUE) {

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
      "HR per month" = 27.02,
      "Duration of initial therapy" = 2,
      "Duration of continious therapy" = 4,
      "Total Cost of chemotherapy" = 305.58,

      "Cost of outpatient consultation (first visit)" = 208,
      "Cost of outpatient consultation (follow-up visit)" = 94,
      "Number of outpatient clinical visits per case" = 4,
      "Visits from TB nurse per case" = 6,
      "Total Out patient care" = 755.86,

      "TST" = 25.02,
      "QFT" = 29.11,
      "TSPOT" = 43.17,

      "Ns_cost" = 53,
      "Out patient consultation (first visit)" = 224,
      "Follow-up via nurses" = 166,

      "CXR" = 46,
      "LFT" = 3.95,
      "Number of CXR" = 1,
      "Number of LFT" = 1,
      "Sp_cost" = 241.23,

      "HR tablet (300/150) (56)" = 25.22,
      "drug cost per month" = 27.02,
      "Duration of HR" = 3,

      "Number of outpatient consultation" = 1,
      "Number of TB nurse appointments"	= 2,

      "LTBIcompl_cost" = 169.68	,
      "LTBIincompl_cost" = 84.84,

      "Hep" = 727.04,
      "TB_cost" = 4909.12)

  label_costs_distns <-
    tribble(
      ~name.cost,   ~vals,
      "TST",   list(distn = "pert", params = c(mode = 25.02, min = 12.51, max = 50.05)),   # Pooran 2010

      "QFT",   list(distn = "pert", params = c(mode = 29.11, min = 15.16, max = 55.67)),   # Imperial NHS

      "TSPOT", list(distn = "pert", params = c(mode = 43.17, min = 22.21, max = 83.87)),   # Imperial NHS

      "CXR",   list(distn = "pert", params = c(mode = 46, min = 40, max = 52)),            # Reference cost 2016
      "LFT",   list(distn = "pert", params = c(mode = 3.95, min = 2.63, max = 5.27)),      # Reference cost 2016

      "Hep",   list(distn = "pert", params = c(mode = 732.04, min = 363.52, max = 1454.07)),     # Pooran 2010

      "TB_cost",    list(distn = "pert", params = c(mode = 4909.12, min = 2454.56, max = 9818.24)),

      "LTBIcompl_cost", list(distn = "unif", params = c(min = 169.68, max = 169.68)),
      "LTBIincompl_cost", list(distn = "unif", params = c(min = 84.84, max = 84.84)),

      "Sp_cost", list(distn = "pert", params = c(mode = 241, min = 233.17, max = 247.28)),
      "Ns_cost", list(distn = "pert", params = c(mode = 53, min = 43, max = 63)),
    )

  label_probs_distns <-
    tribble(
      ~name.prob,         ~prob,
      "TST_sens",    list(distn = "pert", params = c(mode = 0.79, min = 0.69, max = 0.89)),  # Kahwati (2016)
      "TST_spec",    list(distn = "pert", params = c(mode = 0.59, min = 0.46, max = 0.73)),  # Pai (2008)
      "QFT_sens",    list(distn = "pert", params = c(mode = 0.886, min = 0.812, max = 0.944)),  # Zhang et al. BMC Infectious Diseases (2023) 23:40
      "QFT_spec",    list(distn = "pert", params = c(mode = 0.995, min = 0.959, max = 1)),      # Zhang et al. BMC Infectious Diseases (2023) 23:40
      "TSPOT_sens",  list(distn = "pert", params = c(mode = 0.872, min = 0.643, max = 0.991)),  # Zhang et al. BMC Infectious Diseases (2023) 23:40
      "TSPOT_spec",  list(distn = "pert", params = c(mode = 0.998, min = 0.996, max = 1)),          # Zhang et al. BMC Infectious Diseases (2023) 23:40

      "pAccept_chemo", list(distn = "pert", params = c(mode = 0.95, min = 0.5, max = 1)),  # August, Pareek 2013
      "pComp_chemo", list(distn = "pert", params = c(mode = 0.8, min = 0.5, max = 0.9)),   # Kowada (2013)

      "pHep",        list(distn = "unif", params = c(min = 0.001, max = 0.003)),           # Kunst, Pareek

      "Eff_comp",    list(distn = "pert", params = c(mode = 0.65, min = 0.5, max = 0.8)),  # Pareek (2013)
      "Eff_incomp",  list(distn = "pert", params = c(mode = 0.21, min = 0.1, max = 0.3)),  # Pareek (2013)

      "pTB",         list(distn = "pert", params = c(mode = 0.12, min = 0.08,	max = 0.19)),

      "pAccept_TST",  list(distn = "pert", params = c(mode = 0.96, min = 0.93, max = 0.98)),  # posterior
      "pTSTread",     list(distn = "unif", params = c(min = 0.979, max = 0.979)),
      "pIGRAread",    list(distn = "pert", params = c(mode = 1, min = 1, max = 1)),

      "pAccept_IGRA", list(distn = "pert", params = c(mode = 0.99, min = 0.98, max = 1)),  # posterior

      "pAccept_IGRA_TST+", list(distn = "unif", params = c(min = 0.995, max = 0.995)),
      "Dual_sens",    list(distn = "pert", params = c(mode = 0.632, min = 0.632, max = 0.632)),
      "Dual_spec",    list(distn = "pert", params = c(mode = 0.988, min = 0.988, max = 0.988)),
      "pLTBI",        list(distn = "unif", params = c(min = 0.326, max = 0.326)),

      "TSTIGRA_pos",  list(distn = "pert", params = c(mode = 0.38, min = 0.32, max = 0.45)),  # posterior
      "TST_pos",      list(distn = "pert", params = c(mode = 0.56, min = 0.51, max = 0.61)),  # posterior
      "TSPOT_pos",    list(distn = "pert", params = c(mode = 0.23, min = 0.19, max = 0.27)),  # posterior
      "QFT_pos",      list(distn = "pert", params = c(mode = 0.23, min = 0.19, max = 0.27)),  # posterior
      "PPV_TST",      list(distn = "pert", params = c(mode = 0.55, min = 0.20, max = 0.75)),  # posterior
      "NPV_TST",      list(distn = "pert", params = c(mode = 0.82, min = 0.68, max = 0.95)),  # posterior
      "PPV_QFT",      list(distn = "pert", params = c(mode = 0.90, min = 0.86, max = 0.95)),  # posterior
      "NPV_QFT",      list(distn = "pert", params = c(mode = 0.93, min = 0.91, max = 0.95)),  # posterior
      "PPV_TSPOT",    list(distn = "pert", params = c(mode = 0.90, min = 0.86, max = 0.95)),  # posterior
      "NPV_TSPOT",    list(distn = "pert", params = c(mode = 0.93, min = 0.91, max = 0.95)),  # posterior
      "QFT_pos_TST+", list(distn = "pert", params = c(mode = 0.38, min = 0.32, max = 0.45)),
      "PPV_QFT_TST+", list(distn = "pert", params = c(mode = 0.961, min = 0.961, max = 0.961)),
      "NPV_QFT_TST+", list(distn = "pert", params = c(mode = 0.839, min = 0.839, max = 0.839)),
      "TSPOT_pos_TST+", list(distn = "pert", params = c(mode = 0.460, min = 0.460, max = 0.460)),
      "PPV_TSPOT_TST+", list(distn = "pert", params = c(mode = 0.944, min = 0.944, max = 0.944)),
      "NPV_TSPOT_TST+", list(distn = "pert", params = c(mode = 0.911, min = 0.911, max = 0.911)),

      "pReact",       list(distn = "pert", params = c(mode = 0.0012, min = 0.0012, max = 0.0012)),  # Pareek (2013)
      "pReact_comp",  list(distn = "pert", params = c(mode = 0.0004, min = 0.0004, max = 0.0004)),  # Horsburgh
      "pReact_incomp", list(distn = "pert", params = c(mode = 0.0009, min = 0.0009, max = 0.0009))
    )

  label_probs <-
    list(
      "TST_pos"	= 0.56,            # posterior
      "QFT_pos"	= 0.23,            # posterior
      "TSPOT_pos" = 0.23,          # posterior
      "TSTIGRA_pos" = 0.38,        # posterior
      "TSPOT_pos_TST+" = 0.38,     # posterior
      "QFT_pos_TST+" = 0.38,       # posterior

      "pTSTread" = 0.979,
      "pIGRAread" = 1,
      "pAccept_TST" = 0.96,        # posterior
      "pAccept_IGRA" = 0.99,       # posterior
      "pAccept_IGRA_TST+" = 0.99,

      "Dual_sens" = 0.632,
      "Dual_spec" = 0.988,
      "TST_sens" = 0.79,
      "TST_spec" = 0.59,
      "QFT_sens" = 0.8,
      "QFT_spec" = 0.97,
      "TSPOT_sens" = 0.90,
      "TSPOT_spec" = 0.95,

      "pLTBI"	= 0.326,
      "PPV_TST"	= 0.55,             # posterior
      "NPV_TST"	= 0.82,             # posterior
      "PPV_QFT"	= 0.9,              # posterior
      "NPV_QFT"	= 0.93,             # posterior
      "PPV_TSPOT" = 0.9,            # posterior
      "NPV_TSPOT" = 0.93,           # posterior
      "PPV_QFT_TST+" = 0.961,
      "NPV_QFT_TST+" = 0.839,
      "PPV_TSPOT_TST+" = 0.944,
      "NPV_TSPOT_TST+" = 0.911,
      "pAccept_chemo" = 0.95,
      "pComp_chemo" = 0.8,
      "pHep" = 0.002,
      "pReact" = 0.0012,            # Pareek (2013)
      "pReact_comp" = 0.0004,       # Horsburg
      "pReact_incomp" = 0.0009)

  hsuv <-
    list(
      "loss_chemo" = 0.001,         # Auguste (2016)
      "loss_hep" = 0.14,            # Woo (2012)
      "loss_tb" = 0.15)             # Auguste (2016)


  # assign labels to branches -----------------------------------------------

  #########
  # costs #
  #########

  TST_cname_from_to <-
    rbind.data.frame(
      # c("Ns_cost", 1, 2), # remove this since all trees start with nurse visit
      c("TST", 2, 3),
      c("Ns_cost", 3, 4),
      c("Sp_cost", 4, 5),
      c("Hep", 7, 8),
      c("LTBIincompl_cost", 8, 9),
      c("LTBIcompl_cost", 10, 11),
      c("LTBIincompl_cost", 10, 12),
      c("Hep", 16, 17),
      c("LTBIincompl_cost", 17, 18),
      c("LTBIcompl_cost", 19, 20),
      c("LTBIincompl_cost", 19, 21)
    ) %>%
    setNames(c("name", "from", "to"))

  QFT_cname_from_to <-
    rbind.data.frame(
      c("QFT", 1, 2),
      c("Sp_cost", 2, 3),
      c("Hep", 5, 6),
      c("LTBIincompl_cost", 6, 7),
      c("LTBIcompl_cost", 8, 9),
      c("LTBIincompl_cost", 8, 10),
      c("Hep", 14, 15),
      c("LTBIincompl_cost", 15, 16),
      c("LTBIcompl_cost", 17, 18),
      c("LTBIincompl_cost", 17, 19)
    ) %>%
    setNames(c("name", "from", "to"))

  TSPOT_cname_from_to <-
    rbind.data.frame(
      c("TSPOT", 1, 2),
      c("Sp_cost", 2, 3),
      c("Hep", 5, 6),
      c("LTBIincompl_cost", 6, 7),
      c("LTBIcompl_cost", 8, 9),
      c("LTBIincompl_cost", 8, 10),
      c("Hep", 14, 15),
      c("LTBIincompl_cost", 15, 16),
      c("LTBIcompl_cost", 17, 18),
      c("LTBIincompl_cost", 17, 19)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_QFT_cname_from_to <-
    rbind.data.frame(
      c("TST", 1, 2),
      c("Ns_cost", 2, 3),
      c("QFT", 4, 5),
      c("Sp_cost", 5, 6),
      c("Hep", 8, 9),
      c("Hep", 17, 18),
      c("LTBIincompl_cost", 9, 10),
      c("LTBIcompl_cost", 11, 12),
      c("LTBIincompl_cost", 11, 13),
      c("LTBIincompl_cost", 18, 19),
      c("LTBIincompl_cost", 20, 21),
      c("LTBIincompl_cost", 20, 22)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_TSPOT_cname_from_to <-
    rbind.data.frame(
      c("TST", 1, 2),
      c("Ns_cost", 2, 3),
      c("TSPOT", 4, 5),
      c("Sp_cost", 5, 6),
      c("Hep", 8, 9),
      c("Hep", 17, 18),
      c("LTBIincompl_cost", 9, 10),
      c("LTBIcompl_cost", 11, 12),
      c("LTBIincompl_cost", 11, 13),
      c("LTBIincompl_cost", 18, 19),
      c("LTBIcompl_cost", 20, 21),
      c("LTBIincompl_cost", 20, 22)
    ) %>%
    setNames(c("name", "from", "to"))

  TST_QFT_fwd_cname_from_to <-
    rbind.data.frame(
      c("TST", 2, 3),
      c("TST", 25, 26),
      c("Ns_cost", 3, 4),
      c("Ns_cost", 26, 27),
      c("QFT", 5, 6),
      c("QFT", 28, 29),
      c("Sp_cost", 6, 7),
      c("Sp_cost", 29, 30),
      c("Hep", 8, 9),
      c("Hep", 31, 32),
      c("LTBIincompl_cost", 9, 10),
      c("LTBIincompl_cost", 32, 33),
      c("LTBIcompl_cost", 11, 12),
      c("LTBIcompl_cost", 34, 35)
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

  t_chemo <- 3/12  # years
  t_hep <- 1.5/12

  label_health <-
    list(
      "Hep" = hsuv$loss_hep*t_hep,
      "Total (complete)" = hsuv$loss_chemo*t_chemo,
      "Total (incomplete)" = hsuv$loss_chemo*t_chemo/2)      # assume drop-out half way through

  label_health_distns <-
    tribble(
      ~name.health,   ~vals,
      "Hep",
      list(distn = "unif", params = c(min = 0.13, max = 0.15)),             # Woo (2012)
      "Total (complete)",
      list(distn = "unif", params = c(min = hsuv$loss_chemo*t_chemo, max = hsuv$loss_chemo*t_chemo)),      # Auguste (2016)
      "Total (incomplete)",
      list(distn = "unif", params = c(min = hsuv$loss_chemo*t_chemo/2, max = hsuv$loss_chemo*t_chemo/2)))  # Auguste (2016)

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

  if (save) {
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
         file = here::here("data/params.RData"))}
}


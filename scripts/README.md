## README

This folder contains separate files for each test combination decision tree analysis.

The scenarios are:

1.  TST only
2.  IGRA QFT only
3.  IGRA T.SPOT only
4.  TST positive followed by QFT
5.  TST positive followed by T.SPOT

The steps of each analysis are:

-   Define the decision tree using `create_ce_tree_long_df()`
-   Define the terminal states of the decision tree that match starting states for the Markov model
-   Run the decision tree model using `run_cedectree()`
-   Define and run the Markov model using `create_ltbi_heemod()` and `heemod_init_pop_PSA()`
-   Combine decision tree and Markov model output into a list

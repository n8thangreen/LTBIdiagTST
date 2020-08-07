
#' create_ce_tree_long_df
#'
#' @param tree_list 
#' @param tree_mat 
#' @param label_costs 
#' @param pname_from_to 
#' @param cname_from_to 
#'
#' @return
#' @export
#'
#' @examples
#' 
create_ce_tree_long_df <- function(tree_list = NA,
                                   tree_mat = NA,
                                   label_probs,
                                   label_costs,
                                   pname_from_to,
                                   cname_from_to) {
  
  if (!is.na(tree_list)) {
    probs <- child_list_to_transmat(tree_list)
  }
  if (!is.na(tree_mat)) {
    probs <- tree_mat
  } else {
    stop("Require a tree structure object.")
  }
  
  if (inherits(label_probs, "list")) {
    
    label_probs <-
      as_tibble(label_probs) %>%
      melt(value.name = "prob",
           variable.name = "name")
  }
  
  if (inherits(label_costs, "list")) {
    
    label_costs <-
      as_tibble(label_costs) %>%
      melt(value.name = "cost",
           variable.name = "name")
  }
  
  probs_names <-
    probs %>%
    transmat_to_long() %>%
    match_branch_to_label(pname_from_to) %>%
    match_branchlabel_to_prob(label_probs) %>%
    fill_complementary_probs()
  
  costs_names <-
    merge(cname_from_to, label_cost,
          by = "name", all.x = TRUE) %>%
    mutate(from = as.numeric(as.character(from)),
           to = as.numeric(as.character(to)))
  
  all_long <-
    merge(costs_names, probs_names,
          all.y = TRUE, by = c("from", "to"),
          suffixes = c(".cost", ".prob")) %>%
    rename(vals = cost)
  
  return(all_long)
}

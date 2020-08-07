
#' pathway_joint_prob
#'
#' @param df long format decision tree
#' @param terminal_node vector or integers
#'
#' @return
#' @export
#'
#' @examples
#' df <- all_long
#' terminal_node <- 10
#' 
pathway_joint_prob <- function(df,
                               terminal_node){
  
  out <- list()
  
  for (i in seq_along(terminal_node)) {
    
    to_node <- terminal_node[i]
    p_total <- 1
    
    while (to_node %in% df$to) {
      
      p_total <- c(p_total, df$prob[df$to == to_node])
      to_node <- df$from[df$to == to_node]
    }
    
    out[[i]] <- p_total 
  }  
  
  return(out) 
}
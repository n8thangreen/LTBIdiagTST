

#' @export
#'
ce_table2 <- function (he, wtp = 25000, ...)
{
  data.frame(
    cost = paste(round(-colMeans(he$c)[c(he$ref, he$comp)],2),
                 apply(he$c, 2,
                       function(x) paste0("(",
                                          paste(round(-quantile(x, probs = c(0.975, 0.025)),2), collapse = ", "),
                                          ")"))[c(he$ref, he$comp)]),
    eff = paste(round(-colMeans(he$e)[c(he$ref,
                           he$comp)],3),
    apply(he$e, 2,
                function(x) paste0("(",
                                   paste(round(-quantile(x, probs = c(0.975, 0.025)),3), collapse = ", "),
                                   ")"))[c(he$ref, he$comp)]),
    delta.c = c(NA, colMeans(he$delta_c)),
    delta.e = c(NA,
                colMeans(he$delta_e)),
    ICER = c(NA, he$ICER),
    INB = round(c(NA,
            he$eib[he$k == wtp,]),2)
  )
}

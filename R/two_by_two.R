#' Two-by-two comparisons
#' @import dplyr
#' @export
two_by_two = function(data_did, g, e, g_prime = 1, e_prime = 1) {
  n = nrow(data_did)
  data_did = data_did |>
    filter(group == g_prime & W == e_prime | group == g & W == e) |>
    mutate(
      indt = 1 * (group == g_prime & W == e_prime),
      indc = 1 - indt
    )

  indt = data_did |> pull(indt)
  Y_diff = data_did |> pull(Y_diff)
  X_mat = stats::model.matrix( ~ X, stats::model.frame( ~ X, data = data_did, na.action =
                                                          na.pass))

  ps = as.vector(tcrossprod(coef(glm.fit(X_mat, indt, family = binomial())), X_mat))
  ps =  pmin(ps, 1 - 1e-6)
  m = predict(lm(Y_diff~X, data = data_did |> filter(indt == 0)), data_did)

  wt = indt / (sum(indt)/n)
  wc = (1-indt) * (ps/(1-(ps))) / (sum((1-indt) * (ps/(1-(ps))))/n)
  psi = wc * Y_diff + (wt - wc) * m
  return(as.numeric(psi))
}

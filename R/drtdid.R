#' Double robust TDID estimation
#'
#' PUT Desc here
#'
#' @param data The data
#'
#' @return DID and TDID estimates
#'
#' @export
drtdid <-function(data){
  # prep data
  data_did = prep_data(data) |>
    mutate(
      indt_A = 1 * (group == 1 & W == 1),
      indt_B = 1 * (group == 0 & W == 1),
      indc_A = 1 * (group == 1 & W == 0),
      indc_B = 1 * (group == 0 & W == 0)
    )
  # linear scores for each component
  data_did$psi_A_c = 0
  data_did$psi_A_c[data_did$indc_A == 1 | data_did$indt_A == 1] = two_by_two(data_did, 1, 0)
  data_did$psi_B_c = 0
  data_did$psi_B_c[data_did$indc_B == 1 | data_did$indt_A == 1] = two_by_two(data_did, 0, 0)
  data_did$psi_B_t = 0
  data_did$psi_B_t[data_did$indt_B == 1 | data_did$indt_A == 1] = two_by_two(data_did, 0, 1)
  data_did = data_did |> mutate(psi_A_t = (indt_A / mean(indt_A)) * Y_diff)
  data_did = data_did |> mutate(psi_B_t_alt = (indt_B / mean(indt_B)) * Y_diff)
  data_did$psi_B_c_alt = 0
  data_did$psi_B_c_alt[data_did$indc_B == 1 | data_did$indt_B == 1] = two_by_two(data_did, 0, 0, 0, 1)
  # linear scores
  psi_dr_A_est = data_did$psi_A_t - data_did$psi_A_c
  psi_dr_B_est = data_did$psi_B_t_alt - data_did$psi_B_c_alt
  psi_wdr_A_B_est = data_did$psi_B_t - data_did$psi_B_c
  psi_tdid = psi_dr_A_est - psi_wdr_A_B_est
  # if of the tdid estimator
  eta = psi_tdid - mean(psi_tdid)*(data_did$indt_A / mean(data_did$indt_A))
  # final output
  ret <- data.frame(
    tau = mean(psi_tdid),
    tau.se = sd(eta)/sqrt(length(psi_tdid)),
    dr_A = mean(psi_dr_A_est),
    dr_B = mean(psi_dr_B_est),
    wdr = mean(psi_wdr_A_B_est)
  )

  return(ret)

}

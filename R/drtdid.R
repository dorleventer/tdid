#' Double robust TDID estimation
#'
#' PUT Desc here
#'
#' @param data The data
#' @param outcome_name Name of outcome variable
#' @param time_name Name of time variable
#' @param id_name Name of id variable
#' @param group_name Name of group variable
#' @param treat_name Name of treatment variable
#' @param control_formula Covariates to use in estimation of propensity score and outcome regression
#' @param type Type of model for outcome regression and propensity score estimation. Options are "parametric" (default) for OLS and logisitic regressions, "rf" for Random Forest, and "nn" for Neural Network
#' @param crossfit If true, estimation is done using 2-fold cross fitting
#'
#' @return DID and TDID estimates
#'
#' @export
drtdid <- function(data,
                   group_name = "group",
                   time_name = "time",
                   id_name = "id",
                   outcome_name = "Y",
                   treat_name = "W",
                   control_formula = "X",
                   type = "parametric",
                   crossfit = FALSE) {
  # Prepare data using the improved data_prep2 function
  prep <- data_prep(data, group_name, time_name, id_name, outcome_name, treat_name, control_formula)
  data_did <- prep$data_did
  ids <- data_did$ids

  # Create treatment/control indicators for each group
  indt_A <- as.numeric(data_did$W == 1 & data_did$group == 1)
  indc_A <- as.numeric(data_did$W == 0 & data_did$group == 1)
  indt_B <- as.numeric(data_did$W == 1 & data_did$group == 0)
  indc_B <- as.numeric(data_did$W == 0 & data_did$group == 0)
  Y_diff <- data_did$y_diff
  X_mat <- prep$X_mat

  # Estimate propensity scores using the learning_function
  ps_A_t <- learning_function(Y = indt_A, X = X_mat, ids_out = ids, ids_in = ids,
                              type = type, crossfit = crossfit)
  ps_B_t <- learning_function(Y = indt_B, X = X_mat, ids_out = ids, ids_in = ids,
                              type = type, crossfit = crossfit)
  ps_A_c <- learning_function(Y = indc_A, X = X_mat, ids_out = ids, ids_in = ids,
                              type = type, crossfit = crossfit)
  ps_B_c <- learning_function(Y = indc_B, X = X_mat, ids_out = ids, ids_in = ids,
                              type = type, crossfit = crossfit)

  ps_B_t = pmax(ps_B_t, 1e-4)
  ps_B_c = pmax(ps_B_c, 1e-4)
  ps_A_c = pmax(ps_B_t, 1e-4)

  # Outcome regressions
  m_A_c <- learning_function(Y = Y_diff, X = X_mat, ids_out = ids,
                             ids_in = ids[indc_A == 1], type = type)
  m_B_t <- learning_function(Y = Y_diff, X = X_mat, ids_out = ids,
                             ids_in = ids[indt_B == 1], type = type, crossfit = crossfit)
  m_B_c <- learning_function(Y = Y_diff, X = X_mat, ids_out = ids,
                             ids_in = ids[indc_B == 1], type = type, crossfit = crossfit)

  # Construct weights
  w_A_t       <- (indt_A) / mean(indt_A)
  w_B_t       <- (indt_B) / mean(indt_B)
  w_A_t_A_c   <- (indc_A * (ps_A_t / ps_A_c)) / mean(indt_A)
  w_A_t_B_t   <- (indt_B * (ps_A_t / ps_B_t)) / mean(indt_A)
  w_A_t_B_c   <- (indc_B * (ps_A_t / ps_B_c)) / mean(indt_A)
  w_B_t_B_c   <- (indc_B * (ps_B_t / ps_B_c)) / mean(indt_B)

  # Calculate score functions
  psi_dr_A    <- (w_A_t - w_A_t_A_c) * (Y_diff - m_A_c)
  psi_dr_B    <- (w_B_t - w_B_t_B_c) * (Y_diff - m_B_c)
  psi_wdr_A_B <- (w_A_t_B_t - w_A_t_B_c) * Y_diff +
    (w_A_t - w_A_t_B_t) * m_B_t -
    (w_A_t - w_A_t_B_c) * m_B_c
  psi_diff_dr <- psi_dr_A - psi_dr_B
  psi_tdid    <- psi_dr_A - psi_wdr_A_B

  # Influence function adjustment for standard error estimation
  eta         <- psi_tdid - mean(psi_tdid) * w_A_t
  eta_diff_dr <- psi_diff_dr - mean(psi_diff_dr) * w_A_t
  eta_wdr     <- psi_wdr_A_B - mean(psi_wdr_A_B) * w_A_t
  eta_dr_A    <- psi_dr_A - mean(psi_dr_A) * w_A_t
  eta_dr_B    <- psi_dr_B - mean(psi_dr_B) * w_B_t

  # Return a summary data frame of estimates
  return(data.frame(
    est        = mean(psi_tdid),
    se         = stats::sd(eta) / sqrt(length(eta_diff_dr)),
    diff_dr    = mean(psi_diff_dr),
    diff_dr.se = stats::sd(eta_diff_dr) / sqrt(length(eta_diff_dr)),
    wdr        = mean(psi_wdr_A_B),
    wdr.se     = stats::sd(psi_wdr_A_B) / sqrt(length(psi_wdr_A_B)),
    dr_A       = mean(psi_dr_A),
    dr_A.se    = stats::sd(eta_dr_A) / sqrt(length(eta_dr_A)),
    dr_B       = mean(psi_dr_B),
    dr_B.se    = stats::sd(eta_dr_B) / sqrt(length(eta_dr_B)),
    N          = length(psi_tdid)
  ))
}

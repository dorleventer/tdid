#' Data prepartion
#'
#' @param data The data
#' @param outcome_name Name of outcome variable
#' @param time_name Name of time variable
#' @param id_name Name of id variable
#' @param group_name Name of group variable
#' @param control_formula Covariates to use in estimation of propensity score and outcome regression
#'
#' @export
data_prep <- function(data,
                      group_name = "group",
                      time_name = "time",
                      id_name = "id",
                      outcome_name = "Y",
                      treat_name = "W",
                      control_formula = "X") {

  # drop NAs
  data = data |> tidyr::drop_na()

  # make panel
  data = BMisc::makeBalancedPanel(data, id_name, time_name)

  time_vec  <- data[[time_name]]
  id_vec    <- data[[id_name]]
  y_vec     <- data[[outcome_name]]
  w_vec     <- data[[treat_name]]
  group_vec <- data[[group_name]]

  # Check that group_vec contains exactly the expected two groups
  if(!all(sort(unique(group_vec)) == sort(group_levels))){
    stop("Group variable does not match expected levels: c(a, b)")
  }

  # Determine time points
  t1 <- min(time_vec)
  t2 <- max(time_vec)

  # Subset observations for the final time period
  ids   <- id_vec[time_vec == t2]
  y1    <- y_vec[time_vec == t1]
  y2    <- y_vec[time_vec == t2]
  y_diff <- y2 - y1
  W     <- w_vec[time_vec == t2]
  # Define group indicator: 1 if group equals the first level (e.g., "A"), 0 otherwise.
  group <- as.numeric(data[[group_name]][time_vec == t2] == "a")

  # Allow full formula specification or a character string for control covariates.
  if (inherits(control_formula, "formula")) {
    formula_obj <- control_formula
  } else {
    formula_obj <- stats::as.formula(paste0("~", control_formula))
  }

  X <- stats::model.matrix(
    formula_obj,
    stats::model.frame(formula_obj, data = data[time_vec == t2, ], na.action = na.pass)
  )

  return(list(
    data_did = data.frame(ids = ids, y1 = y1, y2 = y2, y_diff = y_diff, W = W, group = group),
    X_mat = X[, -1, drop = FALSE]
  ))
}

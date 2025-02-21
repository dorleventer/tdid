#' Example DGP
#'
#' The data generating process of the example
#'
#' @param n Number of units
#' @param seed The random seed
#'
#' @return A 2-by-2-by-2 dataset
#'
#' @export
example_dgp <- function(n,seed = NULL) {
  if (!is.null(seed)) set.seed(seed) else set.seed(42)

  # Generate group membership: randomly assign "A" or "B"
  group <- sample(c("A", "B"), size = n, replace = TRUE)

  # Draw the time-invariant covariate X based on group membership
  X <- ifelse(group == "A",
              rnorm(n, mean = 1, sd = 1),
              rnorm(n, mean = 3, sd = 1))

  # Treatment assignments:
  W <- rbinom(n, size = 1, prob = .5)

  # Generate error terms for each period
  e1 <- rnorm(n, mean = 0, sd = 1)
  e2 <- rnorm(n, mean = 0, sd = 1)

  # Potential outcomes for the untreated (0) state
  Y1_0 <- X + e1                      # Period 1 outcome (only untreated)
  # For period 2, the untreated outcome is defined with an extra term: W2 * X
  # (note: even though W2 is the treatment indicator, here it appears as part of the potential outcome function)
  Y2_0 <- X + W * X + e2

  # Treatment effects: tau(g, x)
  tau <- ifelse(group == "A", 4 * X, X)

  # Potential outcome for period 2 if treated (i.e., shift by tau)
  Y2_1 <- Y2_0 + tau

  # Observed outcomes
  # Period 1: always untreated, so Y1 = Y1_0
  Y1_obs <- Y1_0
  # Period 2: if treated then Y2 = Y2_1; otherwise, Y2 = Y2_0
  Y2_obs <- ifelse(W == 1, Y2_1, Y2_0)

  data1 = tibble(
    id = 1:n,
    time = 1,
    group = group,
    W = W,
    X = X,
    Y = Y1_obs
  )

  data2 = tibble(
    id = 1:n,
    time = 2,
    group = group,
    W = W,
    X = X,
    Y = Y2_obs
  )

  data = bind_rows(data1, data2)

  return(data)
}

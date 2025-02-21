#' Data prepartion
#'
#' @export
prep_data = function(data) {
  data = data |> arrange(id, time)
  ids = data$id[data$time == 2]
  ypre = data$Y[data$time == 1]
  ypost = data$Y[data$time == 2]
  W = data$W[data$time == 2]
  X = data$X[data$time == 2]
  group = 1*(data$group[data$time == 2]=="A")

  return(data.frame(ids, Y_diff = ypost - ypre, W, X, group))
}

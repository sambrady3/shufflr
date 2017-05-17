#' P_Value of a model performance metric
#'
#' @param true_score numeric metric score on non-shuffled data
#' @param shuffled_scores numeric vector of metrics on shuffled data.
#'   Will have length equal to the number of simulations used during
#'   target shuffling.
#' @return numeric proportion of shuffled scores that are lower than true score.
#'   This can be interpreted as a p_value measuring the significance
#'   of the model's performance.
#' @export
#' @examples
#' p_value(true_precision, shuffled_precisions)

p_value <- function(true_score, shuffled_scores, report = FALSE) {
  len_shuffled_scores = length(shuffled_scores)
  len_lower_scores = length(shuffled_scores[shuffled_scores <= true_score])
  p_val = len_lower_scores / len_shuffled_scores
  if (report) {
    cat("P-Value: ", p_val, "\n", sep = "")
  }
  return(p_val)
}

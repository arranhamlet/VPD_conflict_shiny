#' Split a Vector into Bins and Sum Each Bin
#'
#' This function divides a numeric vector into approximately equal-sized groups (bins),
#' then computes the sum of values within each bin.
#'
#' @param vec A numeric vector to be grouped and summed.
#'
#' @param n_bins An integer specifying the number of bins to split the vector into.
#'
#' @return A numeric vector of length `n_bins` (or fewer if `vec` has fewer elements),
#'   where each element is the sum of values in a bin.
#'
#' @examples
#' # Split and sum a vector of length 10 into 3 bins
#' split_and_sum(1:10, 3)
#'
#' @export
split_and_sum <- function(vec, n_bins) {
  group_size <- ceiling(length(vec) / n_bins)
  groups <- (seq_along(vec) - 1) %/% group_size + 1
  tapply(vec, groups, sum)
}

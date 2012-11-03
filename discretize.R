discretize <- function(x) {
  if ((median(x) - 2*sd(x) < min(x)) & (median(x) + 2*sd(x) > max(x))) {
    d <- c(min(x),median(x) - sd(x),median(x),median(x) + sd(x),max(x))
  }
  if ((median(x) - 2*sd(x) > min(x)) & (median(x) + 2*sd(x) > max(x))) {
    d <- c(min(x),median(x) - 2*sd(x),median(x) - sd(x),median(x),median(x) + sd(x),max(x))
  }
  if ((median(x) - 2*sd(x) < min(x)) & (median(x) + 2*sd(x) < max(x))) {
    d <- c(min(x),median(x) - sd(x),median(x),median(x) + sd(x),median(x) + 2*sd(x),max(x))
  }
  if ((median(x) - 2*sd(x) > min(x)) & (median(x) + 2*sd(x) < max(x))) {
    d <- c(min(x),median(x) - 2*sd(x), median(x) - sd(x),median(x),median(x) + sd(x),median(x) + 2*sd(x),max(x))
  }
}
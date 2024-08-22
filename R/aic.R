loglik_matrix <- function(likert_levels, alpha) {
  # Distance matrix: element i, j denotes how many "notches" an observation,
  # level i, is from a mode, level j, where the last row and column denote
  # the null/blank/don't know level.
  x <- matrix(0, nrow=likert_levels + 1, ncol=likert_levels + 1)

  # An observation at Likert level i is abs(i-j) notches away from a mode at
  # Likert level j
  for (i in 1:likert_levels) {
    for (j in 1:likert_levels) {
      x[i, j] <- abs(i - j)
    }
  }

  # A observation at a Likert level is 1 notch away from a mode at the null
  for (i in 1:likert_levels) {
    x[i, likert_levels + 1] <- 1
  }

  # A null observation is 1 notch away from a Likert mode
  for (j in 1:likert_levels) {
    x[likert_levels + 1, j] <- 1
  }

  # Likelihood matrix: element i, j denotes the likelihood of an observation,
  # level i, given a mode, level j
  exp_minus_alpha_x <- exp(-alpha * x)
  lik <- scale(exp_minus_alpha_x, center=FALSE,
               scale=colSums(exp_minus_alpha_x))

  # Log likelihood: element i, j denotes the log-likelihood contribution of
  # an observation, level i, given a mode, level j.
  log(lik)
  }

loglik_of_bubble_plot_given_alpha <- function(alpha, bubble_plot,
                                              detail=FALSE) {
  likert_levels <- ncol(bubble_plot) - 1
  loglik_for_each_mode <- bubble_plot %*% loglik_matrix(likert_levels, alpha)

  optimal_modes <- apply(loglik_for_each_mode, 1, which.max)
  optimal_logliks <- apply(loglik_for_each_mode, 1, max)

  if(detail) {
    list(optimal_modes=optimal_modes,
         full=loglik_for_each_mode,
         optimal_logliks=optimal_logliks,
         loglik=sum(optimal_logliks))
  } else {
    sum(optimal_logliks)
  }
}

loglik_of_bubble_plot <- function(bubble_plot, alpha_max=100) {
  # Note that even this example that's extremely close to perfect separation
  # gives an optimal alpha of 9.4; alpha_max=100 is unlikely to be too low
  # for any realistic survey dataset.
  # matrix(c(1000, 0, 1, 0, 2000, 0, 0, 0, 3000), nrow=3, ncol=3, byrow=TRUE)

  # First, work out the special case alpha = Inf:
  if(all(apply(bubble_plot, 1, max) == apply(bubble_plot, 1, sum))) {
    # Every bubble is purely one level. Optimal alpha is infinity, and the
    # likelihood of every observation is 1. So the total loglik is a perfect 0.
    return(list(alpha=Inf,
                optimal_modes=apply(bubble_plot, 1, which.max),
                loglik=0))
  }

  optimum <- optimize(loglik_of_bubble_plot_given_alpha, c(0, alpha_max),
                      bubble_plot, maximum=TRUE)

  optimal_alpha <- optimum$maximum
  optimal_loglik <- optimum$objective
  optimal_modes <- loglik_of_bubble_plot_given_alpha(optimal_alpha, bubble_plot,
                                                     detail=TRUE)$optimal_modes
  list(alpha=optimal_alpha,
       optimal_modes=optimal_modes,
       loglik=optimal_loglik)
}

aic_of_bubble_plot <- function(bubble_plot, alpha_max=100) {
  lik_results <- loglik_of_bubble_plot(bubble_plot, alpha_max)

  # TODO Number of parameters is number of bubbles plus one for alpha. But
  # a discrete parameter like the mode of a bubble's histogram carries less
  # information than e.g. a cofficient in a linear regression. A refinement
  # of this workflow could conceivably take this into account, e.g., by
  # estimating an effective number of parameters feeding into a Deviance
  # Information Criterion, but for likely a much more complex and CPU-intensive
  # computation. Effective number of parameters seems to require posterior
  # distributions and thus a MCMC-type sample.
  2 * (length(lik_results$optimal_modes) + 1) - 2 * lik_results$loglik
}

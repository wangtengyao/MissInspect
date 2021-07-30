rms <- function(v){
  sqrt(mean(v^2))
}

mae <- function(v){
  median(abs(v))
}

vector.soft.thresh <- function (x, lambda){
  sign(x) * pmax(0, (abs(x) - lambda))
}

rescale.variance <- function (x){
  p <- dim(x)[1]
  n <- dim(x)[2]
  for (j in 1:p) {
    scale <- mad(diff(x[j, ]))/sqrt(2)
    x[j, ] <- x[j, ]/scale
  }
  return(x)
}

colorise <- function(v){
  uniq <- unique(v)
  ind <- match(v, uniq)
  palet <- putils::matplotlib_palette(length(uniq))
  col <- palet[ind]
  attr(col, 'palette') <- palet
  attr(col, 'legend') <- uniq
  return(col)
}

rep_along <- function(a, v){
  head(rep(a, ceiling(length(v) / length(a))), length(v))
}

stylise <- function(v){
  uniq <- unique(v)
  ind <- match(v, uniq)
  lty <- seq_along(uniq)
  style <- lty[ind]
  attr(style, 'lty') <- lty
  attr(style, 'legend') <- uniq
  return(style)
}

myplot <- function(x, y, col=NULL, style=NULL, data=.GlobalEnv, legend.position='topright', ...){
  #browser()
  xval <- data[[x]]
  yval <- data[[y]]
  legend.col <- legend.lty <- legend.txt <- c()

  if (is.null(col)) {
    plot.col <- legend.col <- 'black'
  } else {
    plot.col <- colorise(data[[col]])
    legend.col <- attr(plot.col, 'palet')
    legend.txt <- paste(col, '=', attr(plot.col, 'legend'))
  }
  if (is.null(style)){
    plot.lty <- legend.lty <- 1
  } else {
    plot.lty <- stylise(data[[style]])
    legend.lty <- attr(plot.lty, 'lty')
    legend.txt <- c(legend.txt, paste(style, '=', attr(plot.lty, 'legend')))
  }
  if (!is.null(col) && !is.null(style)){
    tmp1 <- legend.col; tmp2 <- legend.lty
    legend.col <- c(tmp1, rep_along('black', tmp2))
    legend.lty <- c(rep_along(1, tmp1), tmp2)
  }
  tmp <- data.frame(xval=xval, yval=yval, col=plot.col, lty=plot.lty)
  agg <- aggregate(cbind(xval, yval) ~ col + lty, data=tmp, FUN=list)
  plot(range(xval), range(yval), xlab=x, ylab=y, type='n', ...)
  for (i in 1:nrow(agg)){
    points(agg[i, 3][[1]], agg[i, 4][[1]], col=agg[i, 1], lty=agg[i, 2], type='b', ...)
  }
  legend(legend.position, legend=legend.txt, col=legend.col, lty=legend.lty)
}
# function to draw envelope

envelope <- function(results, xscale, show_all=TRUE, new_plot=TRUE, col='black', lty=2){
  z <- which(xscale==0)
  if (new_plot){
    plot(0,0,xlim=range(xscale), ylim=max(abs(results))*c(-1,1), type='n', xlab='index', ylab='Delta')
  }
  if (show_all){
    for (i in 1:nrow(results)){
      points(xscale, results[i, ], col=putils::matplotlib_palette(20)[i %% 20 + 1], type='l')
    }
  }
  env <- apply(abs(results), 2, max)
  for (t in (z+1):length(env)){env[t] <- max(env[t], env[t-1])}
  for (t in (z-1):1){env[t] <- max(env[t], env[t+1])}
  points(xscale, env, type='l', col=col, lty=lty)
  points(xscale, -env, type='l', col=col, lty=lty)
  return(invisible(env))
}


#
# palet <- putils::matplotlib_palette(20)
# # when probability of observation q is very small, E_z - E_t does not scale with z-t.
# q <- 0.001
# results <- rep(0, 40000)
# for (i in 30000:40000) {
#   set.seed(i)
#   x_true <- x <- rnorm(1000)
#   x[sample(c(T,F), 1000, replace=T, prob=c(1-q, q))] <- NA
#   z <- 500
#   t <- 490
#   x.cusum <- cusum(x)
#   #plot(x.cusum, pch=20, cex=0.6)
#   results[i] <- x.cusum[z] - x.cusum[t]
# }
#
# max(abs(results))
#
# set.seed(33754)
# x_true <- x <- rnorm(1000)
# x[sample(c(T,F), 1000, replace=T, prob=c(1-q, q))] <- NA
# x.cusum <- cusum(x)
# plot(x.cusum - x.cusum[500])
# plot(x)
#
#
# # envelope of Delta_t - Delta_z
#
# simulateD <- function(n, q=0.5, tau=0.4, vartheta=1, nrep=100){
#   z <- n * tau
#   results <- matrix(0, nrep, n-1)
#   for (i in 1:nrep){
#     mu_Omega <- mu <- rep(c(0,1), c(z, n-z)) * vartheta
#     Omega <- sample(c(T,F), n, replace=T, prob=c(1-q, q))
#     mu_Omega[Omega] <- NA
#
#     A <- cusum(mu)
#     A_Omega <- cusum(mu_Omega)
#     Delta <- A_Omega - A * sqrt(q)
#     results[i, ] <- Delta - Delta[z]
#   }
#   return(results)
# }
#
# n <- 1000; q <- 0.5; tau <- 0.4; vartheta <- 1
# envelope(simulateD(n, q, tau, vartheta), xscale=(1:(n-1))/n-tau)
#
# plot(0, xlim=c(-tau, 1-tau), ylim=c(-1.5, 1.5), xlab='index', ylab='Delta')
# ns <- c(1000, 2000, 3000)
# qs <- c(0.01, 0.5)
# for (s in seq_along(qs)){
#   for (r in seq_along(ns)){
#     q <- qs[s]
#     n <- ns[r]
#     envelope(simulateD(n, q), (1:(n-1)/n-tau)/sqrt(q*2), F, F, col=putils::matplotlib_palette()[r], lty=s)
#   }
# }
#
# # envelope of E_t - E_z
# simulateE <- function(n, q=0.5, tau=0.4, nrep=100){
#   z <- n * tau
#   results <- matrix(0, nrep, n-1)
#   for (i in 1:nrep){
#     eps_Omega <- rnorm(n)
#     Omega <- sample(c(T,F), n, replace=T, prob=c(1-q, q))
#     eps_Omega[Omega] <- NA
#     E_Omega <- cusum(eps_Omega)
#     results[i, ] <- E_Omega - E_Omega[z]
#   }
#   return(results)
# }
#
# n <- 1000; tau <- 0.4;
# envelope(simulateE(n), xscale=(1:(n-1))/n-tau)
#
# plot(0, xlim=c(-tau, 1-tau), ylim=c(-5,5), xlab='index', ylab='Delta')
# ns <- c(1000, 5000, 10000)
# qs <- c(0.1, 0.5, 1)
# for (s in seq_along(qs)){
#   for (r in seq_along(ns)){
#     q <- qs[s]
#     n <- ns[r]
#     envelope(simulateE(n, q), 1:(n-1)/n-tau, F, F, col=putils::matplotlib_palette()[r], lty=s)
#   }
# }


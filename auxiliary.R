library(putils) # devtools::install_github('wangtengyao/putils')

rms <- function(v){
  sqrt(mean(v^2))
}

mae <- function(v){
  median(abs(v))
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

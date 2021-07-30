source('auxiliary.R')
lines <- readLines('25Jul.out')
lines_spl <- strsplit(lines, ', | = ')
mx <- unname(t(as.data.frame(lines_spl)))
df <- as.data.frame(mx[,c(F,T)])
colnames(df) <- mx[1,c(T,F)]
for (j in 2:ncol(df)) df[,j] <- as.numeric(df[,j])
write.csv(df, '25Jul.dat')

df <- read.csv('25Jul.dat')
df$err <- abs(400 - df$cp)

agg <- aggregate(cbind(err,  angle) ~ p + q1 + vartheta+sigma, data=df, FUN=mean)
agg$denom <- agg$q1^(1/2) * agg$vartheta
filter <- with(agg, p==1000 &  vartheta %in% c(0.5,1,2) & sigma %in% c(0.2,0.4,0.8, 1.6) & q1 %in% c(0.1,0.2,0.4,0.8))

pchs <- c(16,17,15,18)
postscript('fig3.eps', width=12, height=5)
old_par <- par(oma = c(1,1,1,5), mfrow = c(1, 2), mar = c(3, 4, 0.2, 0.2), mgp=c(2,0.5,0), tcl=-0.2)
# left panel
xval <- agg[filter, 'denom']
yval <- agg[filter, 'angle']

plot.col <- colorise(agg[filter, 'vartheta'])
legend.col <- attr(plot.col, 'palet')
legend.txt <- paste('v', '=', attr(plot.col, 'legend'))

plot.lty <- stylise(agg[filter, 'sigma'])
legend.lty <- attr(plot.lty, 'lty')
legend.txt <- c(legend.txt, paste('s', '=', attr(plot.lty, 'legend')))

tmp1 <- legend.col; tmp2 <- legend.lty
legend.col <- c(tmp1, rep_along('black', tmp2))
legend.lty <- c(rep_along(1, tmp1), tmp2)

tmp <- data.frame(xval=xval, yval=yval, col=plot.col, lty=plot.lty)
curves <- aggregate(cbind(xval, yval) ~ col + lty, data=tmp, FUN=list)
plot(log(range(xval)), log(range(yval)), xlab='log denom', ylab='log sin angle', type='n')
for (i in 1:nrow(curves)){
  points(log(curves[i, 3][[1]]), log(curves[i, 4][[1]]), col=curves[i, 1], lty=curves[i, 2], type='b', pch=pchs, cex=c(1,1,1,1.3))
}
legend('bottomleft', legend=legend.txt, col=legend.col, lty=legend.lty, cex=0.9)


# right panel
xval <- log(agg[filter, 'denom'])
yval <- log(agg[filter, 'err'])

plot.col <- colorise(agg[filter, 'vartheta'])
legend.col <- attr(plot.col, 'palet')
legend.txt <- paste('v', '=', attr(plot.col, 'legend'))

plot.lty <- stylise(agg[filter, 'sigma'])
legend.lty <- attr(plot.lty, 'lty')
legend.txt <- c(legend.txt, paste('s', '=', attr(plot.lty, 'legend')))

tmp1 <- legend.col; tmp2 <- legend.lty
legend.col <- c(tmp1, rep_along('black', tmp2))
legend.lty <- c(rep_along(1, tmp1), tmp2)

tmp <- data.frame(xval=xval, yval=yval, col=plot.col, lty=plot.lty)
curves <- aggregate(cbind(xval, yval) ~ col + lty, data=tmp, FUN=list)
plot(range(xval), range(yval), xlab='log denom', ylab='log loss', type='n')
for (i in 1:nrow(curves)){
  points(curves[i, 3][[1]], curves[i, 4][[1]], col=curves[i, 1], lty=curves[i, 2], type='b', pch=pchs, cex=c(1,1,1,1.3))
}
legend('bottomleft', legend=legend.txt, col=legend.col, lty=legend.lty, cex=0.9, pt.cex=0.9)

dev.off()

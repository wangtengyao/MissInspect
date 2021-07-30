##### Figure 1 #####
source('algorithm.R')
set.seed(120)
n <- 250
p <- 100
k <- 10
z <- 100
vartheta <- 2
q1 <- 0.2
q2 <- 0.2
q <- rep(c(q1, q2), c(k, p-k))
tmp <- InspectChangepoint::single.change(n, p, k, z, vartheta/sqrt(k), shape=0)
x <- tmp$x
mu <- tmp$mu
mask <- matrix(random.bernoulli(p * n, rep(q, n)) == 0, p)
x[mask] <- NA
x.cusum <- cusum(x)

cp <- locate.change(x)
vhat <- attr(cp, 'vhat')
x.cusum.proj <- colSums(vhat * x.cusum)

postscript('fig1.eps', width=10, height=6)
old_par <- par(oma = c(1,1,1,5), mfrow = c(2, 2), mar = c(2.5, 3, 0.2, 0.2), mgp=c(1.2,0.2,0), tcl=-0.2)

# fig 1a
visualise(x)

# fig 1b
visualise(abs(x.cusum))

# fig 1c
palet <- putils::matplotlib_palette(10)
ind <- 1:5
plot(c(0,250), range(c(-x.cusum[ind,], x.cusum.proj)), type='n', xlab='Time', ylab='CUSUM')
for (i in ind){
  points(-x.cusum[i,], type='b', pch=20, cex=0.5, col=palet[i])
}
points((x.cusum.proj), type='l', col='black', lwd=2)
abline(v=cp, lty=2, lwd=2, col='black')
abline(v=100, lty=1, lwd=2, col='darkgray')


# fig 1d

cps <- rep(0, 1000)
for (i in 1:1000){
  set.seed(i)
  tmp <- InspectChangepoint::single.change(n, p, k, z, vartheta/sqrt(k), shape=0)
  x <- tmp$x
  mask <- matrix(random.bernoulli(p * n, rep(q, n)) == 0, p)
  x[mask] <- NA
  cps[i] <- locate.change(x)
  printPercentage(i, 1000)
}
hist(cps, breaks=seq(0,250,4), freq=F, main='', xlab='Estimated changepoint')
library(logcondens)
tmp <- activeSetLogCon(sort(cps))
points(tmp$x, exp(tmp$phi), type='l', col=palet[4], lwd=2)

par(old_par)
dev.off()

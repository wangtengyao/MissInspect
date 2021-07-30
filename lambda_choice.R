source('algorithm.R')
library(putils)

machine = paste0(Sys.info()["nodename"])
println('machine ', machine, ' started.')
sink(paste0('outfiles/', machine, '.out'))

# simulation 1
# seed <- strtoi(substr(digest::digest(machine), 1, 6), 16)
# 
# df <- sim.params(n=1000, p=500, k=c(3,10,50), z=400, vartheta=c(1,1.5,2,2.5,3), q=0.2, a=seq(0, 2, by=0.1), rep=1:3)
# 
# for (i in 1:nrow(df)){
#   n <- df$n[i]; p <- df$p[i]; k <- df$k[i]; vartheta <- df$vartheta[i]; q1 <- df$q[i]; q2 <- df$q[i]; z <- df$z[i]
#   a <- df$a[i]
#   q <- rep(c(q1, q2), c(k, p-k))
#   x <- InspectChangepoint::single.change(n, p, k, z, vartheta/sqrt(k), shape=0)$x
#   mask <- matrix(random.bernoulli(p * n, rep(q, n)) == 0, p)
#   x[mask] <- NA
#   cp <- locate.change(x, lambda=sqrt(n*log(p*n))*a)
#   vhat <- attr(cp, 'vhat')
#   v.oracle <- vector.normalise(rep(c(1,0), c(k, p-k)))
#   angle <- acos(abs(sum(vhat * v.oracle))) / pi * 180
#   println(show.params(machine, n, p, k, z, vartheta, q1, q2, a, cp, angle))
# }

# simulation 2
seed <- strtoi(substr(digest::digest(machine), 1, 6), 16)

df <- sim.params(n=1000, p=500, k=3, z=400, vartheta=2, q1=c(0.1,0.2,0.3,0.4,0.5), q2=c(0.1,0.2,0.3,0.4,0.5), a=seq(0, 2, by=0.1), rep=1:3)

for (i in 1:nrow(df)){
  n <- df$n[i]; p <- df$p[i]; k <- df$k[i]; vartheta <- df$vartheta[i]; q1 <- df$q1[i]; q2 <- df$q2[i]; z <- df$z[i]
  a <- df$a[i]
  q <- rep(c(q1, q2), c(k, p-k))
  x <- InspectChangepoint::single.change(n, p, k, z, vartheta/sqrt(k), shape=0)$x
  mask <- matrix(random.bernoulli(p * n, rep(q, n)) == 0, p)
  x[mask] <- NA
  cp <- locate.change(x, lambda=sqrt(n*log(p*n))*a)
  vhat <- attr(cp, 'vhat')
  v.oracle <- vector.normalise(rep(c(1,0), c(k, p-k)))
  angle <- acos(abs(sum(vhat * v.oracle))) / pi * 180
  println(show.params(machine, n, p, k, z, vartheta, q1, q2, a, cp, angle))
  #printPercentage(i, nrow(df))
}

sink()




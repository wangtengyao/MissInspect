source('algorithm.R')
library(putils)

machine = paste0(Sys.info()["nodename"])
println('machine ', machine, ' started.')
sink(paste0('outfiles/', machine, '.out'))

seed <- strtoi(substr(digest::digest(machine), 1, 6), 16)

df <- sim.params(n=1200, p=2000, k=c(1,3,10,-1,-2), z=400, vartheta=c(0.5,1, 1.5,2, 2.5,3), q=c(0.1,0.5), rep=1:4)
df$k[df$k==-1] <- floor(sqrt(df$p[df$k==-1]))
df$k[df$k==-2] <- df$p[df$k==-2]

for (i in 1:nrow(df)){
  n <- df$n[i]; p <- df$p[i]; k <- df$k[i]; vartheta <- df$vartheta[i]; q1 <- df$q[i]; q2 <- df$q[i]; z <- df$z[i]
  q_mu <- rep(c(q1, q2), c(k, p-k))
  q <- rbeta(p, 10*q_mu, 10*(1-q_mu))
  x <- InspectChangepoint::single.change(n, p, k, z, vartheta/sqrt(k), shape=3)$x
  mask <- matrix(random.bernoulli(p * n, rep(q, n)) == 0, p)
  x[mask] <- NA
  v.oracle <- vector.normalise(c((1:k)^(-1/2), rep(0, p-k)) * q)
  
  cp <- locate.change(x)
  vhat <- attr(cp, 'vhat')
  angle <- acos(abs(sum(vhat * v.oracle))) / pi * 180
  
  cp.soft <- imputeInspect(x)
  vhat.soft <- attr(cp.soft, 'vhat')
  angle.soft <- acos(abs(sum(vhat.soft * v.oracle))) / pi * 180
  
  println(show.params(machine, n, p, k, z, vartheta, q1, q2, cp, cp.soft, angle, angle.soft))
  #printPercentage(i, nrow(df))
}

sink()




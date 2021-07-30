library(putils)
source('algorithm.R')

machine = paste0(Sys.info()["nodename"])
println('machine ', machine, ' started.')
seed <- strtoi(substr(digest::digest(machine), 1, 6), 16)
df <- sim.params(n=1200, p=c(500,1000), k=3, z=400, vartheta=c(0.5,1,1.5,2), q=seq(0.1,1,by=0.1), sigma=seq(0, 2, by=0.2), rep=1:4)

sink(paste0('outfiles/', machine, '.out'))
for (i in 1:nrow(df)){
    n <- df$n[i]; p <- df$p[i]; k <- df$k[i]; vartheta <- df$vartheta[i]; q1 <- q2 <- df$q[i]; z <- df$z[i]; sigma <- df$sigma[i]
    q <- rep(c(q1, q2), c(k, p-k))
    x <- InspectChangepoint::single.change(n, p, k, z, vartheta/sqrt(k), sigma, shape=0)$x
    mask <- matrix(random.bernoulli(p * n, rep(q, n)) == 0, p)
    x[mask] <- NA
    v.oracle <- vector.normalise(rep(c(1,0), c(k, p-k)))
    
    cp <- locate.change(x, lambda=sigma * sqrt(n*log(p*log(n)))/2)
    vhat <- attr(cp, 'vhat')
    angle <- sin(acos(abs(sum(vhat * v.oracle))))
    
    println(show.params(machine, p, q1, vartheta, sigma, cp, angle))
}
sink()




# analysis 1
postscript('fig2.eps', width=12, height=5)
old_par <- par(oma = c(1,1,1,5), mfrow = c(1, 2), mar = c(2.5, 3, 0.2, 0.2), mgp=c(1.2,0.2,0), tcl=-0.2)

palet <- putils::matplotlib_palette(10)
lines <- readLines('lambda_choice1.out')
lines_spl <- strsplit(lines, ', | = ')
mx <- unname(t(as.data.frame(lines_spl)))
df <- as.data.frame(mx[,c(F,T)])
colnames(df) <- mx[1,c(T,F)]
for (j in 2:ncol(df)) df[,j] <- as.numeric(df[,j])

agg <- aggregate(angle ~ n + p + k + z + vartheta + q1 + q2 + a, data=df, FUN=mean)
agg0 <- aggregate(angle ~ n + p + k + z + vartheta + q1 + q2, data=agg, FUN=list)

plot(c(0, 2), c(0, 90), xlab='a', ylab='angle', type='n')
k.uniq <- unique(agg0$k)
vth.uniq <- unique(agg0$vartheta)
for (i in 1:nrow(agg0)){
  points(seq(0,2,by=0.1), agg0[i, 'angle'][[1]], col=palet[match(agg0$vartheta[i], vth.uniq)], lty=match(agg0$k[i], k.uniq), type='l', lwd=2)
}
legend('bottomright', legend=c(paste0('vth = ', vth.uniq), paste0('k = ', k.uniq)), 
       lty=c(rep(1, length(vth.uniq)), 1:length(k.uniq)), 
       col=c(palet[1:length(vth.uniq)], rep('black', 3)), bg='transparent', lwd=2, cex=0.7)

# analysis 2

palet <- putils::matplotlib_palette(10)
lines <- readLines('lambda_choice2.out')
lines_spl <- strsplit(lines, ', | = ')
mx <- unname(t(as.data.frame(lines_spl)))
df <- as.data.frame(mx[,c(F,T)])
colnames(df) <- mx[1,c(T,F)]
for (j in 2:ncol(df)) df[,j] <- as.numeric(df[,j])

agg <- aggregate(angle ~ n + p + k + z + vartheta + q1 + q2 + a, data=df, FUN=mean)
# agg <- agg[agg$q1 %in% c(0.1,0.3,0.5) & agg$q2 %in% c(0.1,0.3,0.5), ]
agg0 <- aggregate(angle ~ n + p + k + z + vartheta + q1 + q2, data=agg, FUN=list)

plot(c(0, 2), c(0, 90), xlab='a', ylab='angle', type='n')
q1.uniq <- unique(agg0$q1)
q2.uniq <- unique(agg0$q2)
for (i in 1:nrow(agg0)){
  points(seq(0,2,by=0.1), agg0[i, 'angle'][[1]], col=palet[match(agg0$q1[i], q1.uniq)], lty=match(agg0$q2[i], q2.uniq), type='l')
}
legend('topright', legend=c(paste0('q1 ~= ', q1.uniq), paste0('q2 ~= ', q2.uniq)), 
       lty=c(rep(1, length(q1.uniq)), 1:length(q1.uniq)), 
       col=c(palet[1:length(q1.uniq)], rep('black', length(q2.uniq))), bg='transparent', lwd=2, cex=0.7)
dev.off()
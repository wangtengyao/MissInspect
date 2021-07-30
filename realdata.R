library(putils)
source('algorithm.R')
NA_core <- read.table('real_data/NA_finalCore.txt')
PO_core <- read.table('real_data/PO_finalCore.txt')
SO_core <- read.table('real_data/SO_finalCore.txt')
NA_core[,3] <- paste0('NA', NA_core[,3])
PO_core[,3] <- paste0('PO', PO_core[,3])
SO_core[,3] <- paste0('SO', SO_core[,3])
cores <- rbind(NA_core, PO_core, SO_core)
colnames(cores) <- c('Ma', 'delta13C', 'core')
myplot('Ma', 'delta13C', col='core', data=cores, pch=20, cex=0.7)

core.uniq <- unique(cores$core)
Ma.uniq <- unique(cores$Ma)
dat <- matrix(NA, length(core.uniq), length(Ma.uniq),
              dimnames=list(core.uniq, Ma.uniq))

dat[cbind(as.character(cores[,3]), cores[,1])] <- cores[,2]
snippet(dat)

mu <- apply(dat, 1, mean, na.rm=T)
sd <- apply(dat, 1, sd, na.rm=T)
dat_norm <- (dat - mu) / sd
cp <- locate.change(dat_norm) # 3732
abline(v=Ma.uniq[cp])
tmp <- BinSeg(dat_norm)
tmp <- as.data.frame(tmp)
cps <- Ma.uniq[sort(tmp, by=2, decreasing=T)[1:10,1]]

palet <- putils::matplotlib_palette(16)
pdf('fig4.pdf', width=10, height=12)
old_par <- par(mfrow = c(16, 1), oma = c(3,3,3,3), mar = c(0, 3, 0, 0.2), mgp=c(1.2,0.2,0), tcl=-0.2, yaxt='n')
for (i in 1:16) {
  plot(Ma.uniq, dat[i,], type='p', pch=20, ylab=rownames(dat)[i])
  abline(v=cps[-1], col=palet[4], lty=2)
  abline(v=cps[1], col=palet[4], lty=1)
}
dev.off()
plot(Ma.uniq[-length(Ma.uniq)], tmp$max.val)



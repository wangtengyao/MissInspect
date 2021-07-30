source('auxiliary.R')
lines <- readLines('26Jul.out')
lines_spl <- strsplit(lines, ', | = ')
mx <- unname(t(as.data.frame(lines_spl)))
df <- as.data.frame(mx[,c(F,T)])
colnames(df) <- mx[1,c(T,F)]
for (j in 2:ncol(df)) df[,j] <- as.numeric(df[,j])
write.csv(df, '26Jul.dat')

df$err <- abs(df$z - df$cp)
df$err.soft <- abs(df$z - df$cp.soft)
agg <- aggregate(cbind(err, err.soft, angle, angle.soft) ~ n + p + k + z + vartheta + q1 + q2, data=df, FUN=sd)
filter <- with(agg, k %in% c(3,44,2000) & vartheta %in% c(1,2,3))
ret <- round(agg[filter, -c(1,2,4,6)], 1)

ret <- sort(ret, by=c(3, 1, 2))
ret <- ret[, c(3,1,2,6,7,4,5)]
putils::write.latextable(ret)


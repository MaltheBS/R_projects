# variance test to select timepoints for further analysis
# to avoid mulitple compatisons

# gaussian noise function
gaus_noise <- function(x, sd) {
  m <- mean(x)
  rnorm(x, m, sd)
}

# generate data1
samples <- 10000
trialsInCondition <- 30
condition1 <- rep(1,samples)

for (trial in 1:trialsInCondition){
  trial <- smooth(gaus_noise(rep(0,samples),.5))
  condition1 <- rbind(condition1,trial)
}
condition1 <- condition1[2:trialsInCondition+1,]
condition1[,200:300] <- condition1[,200:300] + 1

# generate data2
trialsInCondition <- 30
condition2 <- rep(1,samples)

for (trial in 1:trialsInCondition){
  trial <- smooth(gaus_noise(rep(0,samples),.5))
  condition2 <- rbind(condition2,trial)
}
condition2 <- condition2[2:trialsInCondition+1,]

# all data
all <- rbind(condition1,condition2)

# calulate sd
var_vect <- rep(0,samples)
for (n in 1:length(var_vect)){
  var_vect[n] <- sd(all[,n])
}

hist(var_vect,100)
plot(var_vect,type='l',xlim=c(100,400))

varmean <- mean(var_vect)
varsd <- sd(var_vect)
sum(var_vect > 5*varsd+varmean)
plot(var_vect > 2*varsd+varmean,type='l')
# condition means
c1mean <- apply(condition1,2,mean)
c2mean <- apply(condition2,2,mean)
allmean <- apply(all,2,mean)
plot(c1mean,type='l')
lines(c2mean,type='l',col=2)
lines(allmean,type='l',col=4)

# t tests
tvect <- rep(0,samples)
for (n in 1:samples){
  tvect[n] <- t.test(condition1[,n],condition2[,n])$statistic
}

plot(tvect,type='l')

### fooo

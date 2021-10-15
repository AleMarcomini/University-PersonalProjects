#Define function to sample single random variable, recalling Erlang distribution properties
y_gen <- function(m, beta){
  u = runif(m)
  y <- -log(prod(u))/beta    
}

#Define function for sampling n random values  
rerlg <- function(n, m, beta){
  M = rep(m,N)
  Y <- Vectorize(y_gen)(M, beta)
}



set.seed(2024286)

#set function params
n.samples = 3000
m1 = 1
m2 = 100
m3 = 5
beta1 = .8
beta2 = 15
beta3 = 1.3

#Create histograms
histo_data_1 <- rerlg(n.samples,m1,beta1)
histo_data_2 <- rerlg(n.samples,m2,beta2)
histo_data_3 <- rerlg(n.samples,m3,beta3)

#Plot
x <- seq(0,10,0.1)

col1 <- rgb(255, 0, 0, max = 255, alpha = 20, names = "red50")
col2 <- rgb(0, 255, 0, max = 255, alpha = 20, names = "green50")
col3 <- rgb(0, 0, 255, max = 255, alpha = 20, names = "blue50")

PDF_data_1 = dgamma(x,shape=m1,rate=beta1)
PDF_data_2 = dgamma(x,shape=m2,rate=beta2)
PDF_data_3 = dgamma(x,shape=m3,rate=beta3)
plot(x,PDF_data_1, pch=20, cex=0, col='red', xlab = 'x',ylab='Prob', 
     main = 'Random samples from Erlang distributions', ylim=c(0,1.1))
grid()
breaks = seq(0,20,0.5)
hist(histo_data_1, breaks = breaks, prob = TRUE, add=TRUE, col = col1)
hist(histo_data_2, breaks = breaks, prob = TRUE, add=TRUE, col = col2)
hist(histo_data_3, breaks = breaks, prob = TRUE, add=TRUE, col = col3)

lines(x,PDF_data_1, col='red', lty = 2, lw=3)
lines(x,PDF_data_2, col='chartreuse3', lty = 2, lw=3)
lines(x,PDF_data_3, col='blue', lty = 2, lw=3)

leg_names <- c('m1 = 1','beta1 = 0.8','m2 = 100',
               'beta2 = 15','m3 = 5','beta3 = 1.3')
legend('topright', leg_names, cex=0.8, text.col=c('red','red','chartreuse4',
                                             'chartreuse4','blue','blue'))
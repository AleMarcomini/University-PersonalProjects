#Ask for introductory data
N = strtoi(readline(prompt = "How many random numbers shall I generate? : "))
x = as.numeric(readline(prompt = "Insert input number: "))

#Get length of numbers to be generated
Lx = floor(log10(x))+1
#Set iteration counter
n = 1
#Do you want to visualize data before and after cropping to check the central region is saved?
info = FALSE

#Store output
random <- rep(0,N)

#Algorithm
while(n<=N){
  #Square value
  y = x*x
  if (info){print(y)}
  Ly = floor(log10(y))+1
  
  #Consider exception for pathological values
  if (Ly<Lx){                    
    x = as.numeric(readline(prompt = "Algorithm has failed, provide new input: "))
    next
  }
  
  #Keep only the central region (lower-central, for odd number of digits)
  margin = floor((Ly-Lx)/2)
  y = floor(y/(10**margin))
  y = ( (y/(10**Lx)) - floor(y/(10**Lx)) ) * 10**Lx
  if (info){print(y)}
  
  #Update
  random[[n]] <- y
  x = y
  n = n+1
}

#Show vector of generated data numbers and autocorrelation plot for analysis
View(matrix(random))
acf(random)
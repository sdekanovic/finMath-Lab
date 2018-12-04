# Glajenje časovnih vrst
library(readr)


# Naloga 1: Uvoz in predstavitev podatkov

srebro <- read.csv("srebro.csv")
srebro <- srebro[1:123, c(1, 5)]

cene <- numeric(length(srebro[, 2]))
for (i in 1:length(srebro[, 2])){
  cene[i] <- as.numeric(gsub("[\\$,]", "", srebro[, 2][i]))
}

cene <- rev(cene)

plot(ts(cene), main = "Srebro", ylab = "$")
points(ts(cene), p = 20)

# Naloga 2: Glajenje z drsečim povprečjem reda k

G <- function(vrsta, k){
  
  y <- numeric(length(vrsta))
  for (i in 1:k){
    y[i] <- NA
  }
  for (i in k:length(vrsta)){
    # t = k:k + (k-1) + ... + 1
    # t = k+1: k+1 + k + (k-1) + ... 2
    y[i+1] <- sum(vrsta[c((i-k+1):(i))])/k
  }
  return(y)
}

zglajena <- G(cene, 5)
napoved <- zglajena[length(zglajena)]

par(mfrow = c(2, 2))

vrsta1 <- ts(cene)
vrsta2 <- ts(zglajena)
ts.plot(vrsta1, vrsta2, col = c("black", "red"), main = "Drsno povprečje reda 5", ylab = "$")
points(vrsta1, p = 20)

MSE <- function(vrsta, k = 1){
  zglajena <- G(vrsta, k)
  vsota <- 0
  for (i in k:(length(vrsta) - 1)){
    vsota <- vsota + (vrsta[i+1]-zglajena[i+1])**2
  }
  vsota <- vsota / (length(vrsta) - k)
  return(vsota)
}

MSE(cene, 5)

zglajena15 <- G(cene, 15)
napoved15 <- zglajena15[length(zglajena15)]
vrsta15 <- ts(zglajena15)
ts.plot(vrsta1, vrsta15, col = c("black", "red"), main = "Drsno povprečje reda 15", ylab = "$")
points(vrsta1, p = 20)
MSE(cene, 15)

zglajena30 <- G(cene, 30)
napoved30 <- zglajena30[length(zglajena30)]
vrsta30 <- ts(zglajena30)
ts.plot(vrsta1, vrsta30, col = c("black", "red"), main = "Drsno povprečje reda 30", ylab = "$")
points(vrsta1, p = 20)
MSE(cene, 30)


 
# Naloga 3: Eksponentno glajenje

EG <- function(vrsta, alpha){
  # y^_T+1 = l_t
  l <- numeric(length(vrsta))
  l[1] <- vrsta[1]
  for (i in 2:(length(vrsta))){
    l[i] <- alpha*vrsta[i] + (1 - alpha)*l[i-1]
  }
  return(l)
}

alfa <- 0.1
zglajenaE <- EG(cene, alfa)
napovedE <- zglajenaE[length(zglajenaE)]
napovedE

par(mfrow=c(1,1))

vrsta1 <- ts(cene)
vrstaE <- ts(zglajenaE)
ts.plot(vrsta1, vrstaE, col = c("black", "red"), main = "Eksponentno glajenje", ylab = "$")
points(ts(cene), p = 20)

MSEa <- function(alfa){
  vrsta <- EG(cene, alfa)
  return(MSE(vrsta, k = 1))
}

optimize(MSEa, c(0.1, 0.3))

alfaOpt <- optimize(MSEa, c(0, 1))
alfaOpt <- as.double(alfaOpt)[1]
zglajenaE <- EG(cene, 0.1)
vrsta1 <- ts(cene)
vrstaE <- ts(zglajenaE)
ts.plot(vrsta1, vrstaE, col = c("black", "red"), main = "Eksponentno glajenje, minimalen MSE", ylab = "$")
points(ts(cene), p = 20)

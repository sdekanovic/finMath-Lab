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
  
  y <- numeric(length(vrsta) - k)
  # for i in k+1: length(y) + 1
  for (i in 1:length(y)){
    # k + (k-1) + ... + 1
    # k+1 + k + (k-1) + ... 2
    vrednost <- 0
    vrednost <- sum(vrsta[c(i:(k+i-1))])/k
    y[i] <- vrednost
  }
  return(y)
}

zglajena <- G(cene, 5)
napoved <- sum(cene[c(119:123)])/5

par(new = TRUE)
plot(ts(zglajena), from = 5, to = 123, col = "red")

a<-ts(zglajena)
b<-ts(cene)
ts.plot(b,a)


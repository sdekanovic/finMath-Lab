library(combinat)
library(Rlab)

# Naloga 1

u = 1.05
d = 0.95
T = 5
R = 3/100
W = c(1:6)

S0 = c(rep(50, 5))
S1 = c(52.50, 52.50, 47.50, 47.50, 52.50)
S2 = c(49.88, 55.12, 49.88, 45.12, 49.88)
S3 = c(47.38, 57.88, 47.38, 47.38, 52.37)
S4 = c(45.01, 60.78, 45.01, 45.01, 54.99)
S5 = c(47.26, 63.81, 42.76, 47.26, 52.24)

M <- matrix(c(S0, S1, S2, S3, S4, S5), nrow = 5, ncol = 6)

X <- c()
Y <- c()
for (i in 1:5){
  X <- c(X, max(0, S5[i] - weighted.mean(M[i, ], W)))
  Y <- c(Y, max(weighted.mean(M[i, ] - S5[i], W), 0))
}

M <- cbind(M, X, Y)
colnames(M) <- c("S0", "S1", "S2", "S3", "S4", "S5", "Izplačilo X", "Izplačilo Y")

izplacilo <- function(vrsta, W, type){
  if (type == "call"){
    return(max(0, vrsta[length(vrsta)] - weighted.mean(vrsta, W)))
  }
  else if (type == "put"){
    return(max(weighted.mean(vrsta, W) - vrsta[length(vrsta)], 0))
  }
  else{
    return(NULL)
  }
}

# Naloga 2

binomski <- function(S0,u,d,R,T,W,type){
  
  q <- (1 + R - d)/(u - d)
  r <- hcube(rep(2,T)) - 1
  
  razvoji <- rowSums(r)
  
  P <- q^(T - razvoji)*(1 - q)^razvoji #  vektor verjetnosti
  poti <- u^(1 - r)*d^r
  drevo <- cbind(rep(S0, 2^T), poti)
  vrednosti <- t(apply(drevo, 1, cumprod))
  izplacila <- apply(vrednosti, 1, function(x) izplacilo(x, W, type))
  
  povprecjeQ <- sum(izplacila*P)
  cena <- povprecjeQ/((1 + R)^T)
  
  return(cena)
}

monte <- function(S0, u, d, R, T, W, type, N){
  
  poti <- matrix(nrow = N, ncol = T)
  p <- (1 + R - d)/(u - d)
  
  i = 1
  while(i <= N) {
    vektor_0in1 <- rbinom(T,1,p)
    poti[i, ] <- vektor_0in1
    i <- i + 1
  }
  
  razvoji <- rowSums(poti)
  
  P <- p^razvoji*(1 - p)^(T - razvoji) # vektor verjetnosti
  
  smeri <- u^(poti)*d^(1 - poti)
  smeri <- cbind(rep(S0, nrow(poti)), smeri)
  
  vrednosti <- t(apply(smeri, 1, cumprod))
  izplacila <- apply(vrednosti, 1, izplacilo, W=W, type=type)
  
  povprecjeQ <- sum(izplacila)/N
  
  cena <- povprecjeQ/((1+R)^T)
  
  return(cena)
}

# N = 10;100;1000

monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), "put", 10)
monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), "put", 100)
monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), "put", 1000)

# Naloga 3





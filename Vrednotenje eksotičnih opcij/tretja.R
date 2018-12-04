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

N1 = 10
N2 = 100
N3 = 1000

n1 <- numeric(100)
n2 <- numeric(100)
n3 <- numeric(100)

for (i in 1:100){
  n1[i] <- monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), "put", N1)
  n2[i] <- monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), "put", N2)
  n3[i] <- monte(60, 1.05, 0.95, 0.01, 15, rep(1, 16), "put", N3)
}

B1 <- binomski(60, 1.05, 0.95, 0.01, 15, rep(1, 16), "put")

H1 <- hist(n1, main = "Monte Carlo: N=10", xlim = c(0, 5), ylim = NULL, col = "yellow", xlab = "Premija")
arrows(x0 = mean(n1), y0 = 0, x1 = mean(n1), y1 = 24, lty = 1, col = "green")
arrows(x0 = mean(n1), y0 = 0, x1 = mean(n1) - sd(n1), y1 = 0, lty = 1, col = "green")
arrows(x0 = mean(n1), y0 = 0, x1 = mean(n1) + sd(n1), y1 = 0, lty = 1, col = "green")
arrows(x0 = B1, y= 0, x1 = B1, y1 = 24, lty = 3, col = "red")

H2 <- hist(n2, main = "Monte Carlo: N=100", xlim = c(0, 5), ylim = NULL, col = "yellow", xlab = "Premija")
arrows(x0 = mean(n2), y0 = 0, x1 = mean(n2), y1 = 35, lty = 1, col = "green")
arrows(x0 = mean(n2), y0 = 0, x1 = mean(n2) - sd(n2), y1 = 0, lty = 1, col = "green")
arrows(x0 = mean(n2), y0 = 0, x1 = mean(n2) + sd(n2), y1 = 0, lty = 1, col = "green")
arrows(x0 = B1, y= 0, x1 = B1, y1 = 35, lty = 3, col = "red")

H3 <- hist(n3, main = "Monte Carlo: N=1000", xlim = c(0, 5), ylim = NULL, col = "yellow", xlab = "Premija")
arrows(x0 = mean(n3), y0 = 0, x1 = mean(n3), y1 = 30, lty = 1, col = "green")
arrows(x0 = mean(n3), y0 = 0, x1 = mean(n3) - sd(n3), y1 = 0, lty = 1, col = "green")
arrows(x0 = mean(n3), y0 = 0, x1 = mean(n3) + sd(n3), y1 = 0, lty = 1, col = "green")
arrows(x0 = B1, y= 0, x1 = B1, y1 = 30, lty = 3, col = "red")

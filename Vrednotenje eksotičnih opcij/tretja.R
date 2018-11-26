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


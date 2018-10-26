library(actuar)
library(readr)

# 1.naloga

# S = sum Yi from 1 to N
# E(S) = E(E(S|N)) = E(N)E(Y)
# var(S) = var(Y)E(N) + E(Y^2)var(N)
# var(Y) = E(Y^2) - E(Y)^2
# N ~ bin

vzorec <- scan("vzorec2.txt")
parametra <- mde(vzorec, pweibull, start = list(shape = 1, scale = 1), measure = "CvM")
par1 <- parametra$estimate[1]
par2 <- parametra$estimate[2]
histogram <- hist(vzorec, main = "Histogram odskodnin", xlab = "Visina odskodnine", col = "lightskyblue", probability = TRUE)
curve(dweibull(x, shape = par1, scale = par2, from = 0, to = max(vzorec), col="red", add = TRUE))

WB <- pweibull(vzorec, shape = par1, scale = par2)
BIN <- rbinom(n = 20, size = 195, prob = 0.5)

upanje <- mean(BIN)*mean(WB)
disperzija <- var(WB)*mean(BIN) + (var(WB) + mean(WB)^2)*var(BIN)

# 2.naloga

h <- 0.25
n <- 26
Y <- discretize(pweibull(x, shape = par1, scale = par2), from = 0, to = n*h, step = h, method = c("upper"))
curve(pweibull(x, shape = par1, scale = par2), from = 0, to = n*h)

plot(Y)

library(actuar)
library(readr)

# 1.naloga: Porazdelitev individualnih ?kodnih zahtevkov

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
curve(dweibull(x, shape = par1, scale = par2), from = 0, to = max(vzorec), col="red", add = TRUE)
legend("topright", legend = "Weibullova porazdelitev", col = "red", lty = 1)

curve(pweibull(x, shape = par1, scale = par2), from = 0, to = max(vzorec), col = "blue")
plot(ecdf(vzorec), add = TRUE)
legend("bottomright", legend = c("Empiricna porazdelitev", "Weibullova porazdelitev"), col = c("black", "blue"), pch = c(16, NA), lty = c(1, 1))

WB <- pweibull(vzorec, shape = par1, scale = par2)
BIN <- rbinom(n = 20, size = 195, prob = 0.5)

upanje <- mean(BIN)*mean(WB)
disperzija <- var(WB)*mean(BIN) + (var(WB) + mean(WB)^2)*var(BIN)


# 2.naloga: Dolo?anje porazdelitve kumulativne ?kode s Panjerjevim algoritmom

h <- 0.25
n <- 26
#M <- max(vzorec)
Y <- discretize(pweibull(x, shape = par1, scale = par2), method = c("unbiased"), lev = levweibull(x, 1), from = 0, to = n*h, step = h)

curve(pweibull(x, shape = par1, scale = par2), to = n*h, col = "blue")
plot(stepfun(1:26, Y), pch = NA, col = "green", add = TRUE)

#plot(stepfun(x, diffinv(fb)), pch = 19, add = TRUE)

Pan <- aggregateDist(method = "recursive", model.freq = "binomial", model.sev = Y, size = 195, prob = 0.5)
knots(Pan)
upanjePan <- mean(Pan)
disperzijaPan <- var(Pan)

# 3. naloga

S <- c()
while (length(S) < 10000) {
  N <- rbinom(n = 1, size = 195, prob = 0.5);
  Y <- rweibull(n = N, shape = par1, scale = par2)
  S <- c(S, sum(Y))
}

upanjeMC <- mean(S)
odklonMC <- sqrt(var(S))

plot(ecdf(S))




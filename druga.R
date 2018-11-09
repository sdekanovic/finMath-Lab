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

curve(pweibull(x, shape = par1, scale = par2), main = "Porazdelitvena funkcija odskodnin", from = 0, to = max(vzorec), col = "blue", xlab = "Visina odskodnine", ylab = "Porazdelitvena funkcija")
plot(ecdf(vzorec), add = TRUE)
legend("bottomright", legend = c("Empiricna porazdelitev", "Weibullova porazdelitev"), col = c("black", "blue"), pch = c(16, NA), lty = c(1, 1))

# upanje weibullove
eY <- par2 * gamma(1 + (1 / par1))
# upanje binomske
eBIN <- 20 * 0.5

varY <- par2^2 * (gamma(1 + 2/par1) - gamma(1 + 1/par1)^2)
varBIN <- 20 * 0.5 * 0.5

upanje <- eY * eBIN
disperzija <- varY * eBIN + (eY^2)*varBIN


# 2.naloga: Dolo?anje porazdelitve kumulativne ?kode s Panjerjevim algoritmom

razdalja <- function(a, b){
      vector <- c(a)
      while (a<b) {
        a <- a + 0.25
        vector <- c(vector, a)
      }
      return(vector)
}

verjetnosti <- function(vec) {
  a <- rep(NA, 27)
  for (i in (0 : 27)){
    a[i] <- sum(vec[0:i])
  }
  return(a)
}


h <- 0.25
n <- 28
#M <- max(vzorec)
Y <- discretize(pweibull(x, shape = par1, scale = par2), method = c("rounding"), from = 0, to = n*h, step = h)


plot(xlab = "x", ylab = "Porazdelitvena funkcija", stepfun(razdalja(0, 6.5), c(0, verjetnosti(Y))),
    col = "orange", lwd = 3, main = "Weibullova porazdelitev")
curve(pweibull(x, shape = par1, scale = par2), to = n*h, col = "black", add = TRUE)


Pan <- aggregateDist(method = "recursive", model.freq = "binomial", model.sev = verjetnosti(Y), size = 20, prob = 0.5)
knots(Pan)
upanjePan <- mean(Pan)
disperzijaPan <- var(knots(Pan))

# 3. naloga

S <- c()
while (length(S) < 10000) {
  N <- rbinom(n = 1, size = 20, prob = 0.5);
  Y <- rweibull(n = N, shape = par1, scale = par2)
  S <- c(S, sum(Y))
}

upanjeMC <- mean(S)
variancaMC <- var(S)

plot(Pan)
plot(ecdf(S), col = "green", add = TRUE)
legend(1, 1, legend = c("Panjerjev algoritem", "Monte Carlo simulacija"), col = c("black", "green"), lty = c(1, 1), box.lty = 0)




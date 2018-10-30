library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")

# Obdobje: 2015-2017
# Tip terminske obrestne mere: 3x6

# Uvoz podatkov

eu15 <- read_csv("EURIBOR_2015.csv")
eu16 <- read_csv("EURIBOR_2016.csv")
eu17 <- read_csv("EURIBOR_2017.csv")

# Čiščenje in urejanje podatkov

#eu15

mesec <- 1
k <- 2
v <- 1
while (mesec < 13) {
  if (as.numeric(unlist(strsplit(colnames(eu15)[k], split = "/"))[2]) == mesec) {mesec <- mesec+1; v <- c(v, k)};
  k <- k+1
}

eu15 <- eu15[v]
eu15 <- t(eu15)
colnames(eu15) <- c("1w", "2w", "1m", "2m", "3m", "6m", "9m", "12m")
eu15 <- eu15[-1,]
eu15 <- data.frame(eu15)

#eu16

mesec <- 1
k <- 2
v <- 1
while (mesec < 13) {
  if (as.numeric(unlist(strsplit(colnames(eu16)[k], split = "/"))[2]) == mesec) {mesec <- mesec+1; v <- c(v, k)};
  k <- k+1
}

eu16 <- eu16[v]
eu16 <- t(eu16)
colnames(eu16) <- c("1w", "2w", "1m", "2m", "3m", "6m", "9m", "12m")
eu16 <- eu16[-1,]
eu16 <- data.frame(eu16)

#eu17

mesec <- 1
k <- 2
v <- 1
while (mesec < 13) {
  if (as.numeric(unlist(strsplit(colnames(eu17)[k], split = "/"))[2]) == mesec) {mesec <- mesec+1; v <- c(v, k)};
  k <- k+1
}

eu17 <- eu17[v]
eu17 <- t(eu17)
colnames(eu17) <- c("1w", "2w", "1m", "2m", "3m", "6m", "9m", "12m")
eu17 <- eu17[-1,]
eu17 <- data.frame(eu17)

# Graf: sprememba 3x6 obrestne mere glede na čas

X <- eu15[,5:6]
Y <- eu16[,5:6]
Z <- eu17[,5:6]
XYZ <- rbind(X, Y, Z)
XYZ <- data.frame(XYZ)

graf <- ts.plot(ts(data=as.matrix(XYZ), start=c(2015,1,2), end=c(2017,12,1), frequency = 12), gpars=list(xlab="Time", ylab="%"), col=c("red", "blue"));
        title(main="Euribor"); 
        legend("topright", legend = c("3m", "6m"), col=c("red", "blue"), lty = c(1,1))

# 2.naloga

# Izbrani datumi: 2.1.2015, 1.12.2015 (6 se ustavi in naraste za kratek čas), 1.2.2017 (6 strmo pade in naraste)


podatki <- as.data.frame(t(eu15[1,]))
mesci <- c(0.23, 0.46, 1, 2, 3, 6, 9, 12)
podatki["01/12/2015"] <- as.data.frame(t(eu15[12,]))
podatki["01/02/2017"] <- as.data.frame(t(eu17[2,]))
podatki["Dospetje"] <- mesci

graf2 <- ggplot() + geom_point(data = podatki, aes(x = podatki$Dospetje, y = podatki$`02/01/2015`),
                               colour = "dodgerblue", size = 2) + 
  geom_line(data = podatki, aes(y = podatki$`02/01/2015`, x = podatki$Dospetje, group = 1), colour = "dodgerblue") +
  geom_text(aes(y = 3, x=11.0), label = "2/1/2015", colour = "dodgerblue") +
  geom_point(data = podatki, aes(x = podatki$Dospetje, y = podatki$`01/12/2015`),
             colour = "firebrick", size = 2) +
  geom_line(data = podatki, aes(y = podatki$`01/12/2015`, x = podatki$Dospetje, group = 1), colour = "firebrick") + 
  geom_text(aes(y=2, x = 11.0), label = "1/12/2015", colour = "firebrick") + 
  geom_point(data = podatki, aes(x = podatki$Dospetje, y = podatki$`01/02/2017`),
             colour = "springgreen", size = 2) + 
  geom_line(data = podatki, aes(y = podatki$`01/02/2017`, x = podatki$Dospetje, group = 1), colour = "springgreen") +
  geom_text(aes(y = 1, x = 11.0), label = "1/2/2017", colour = "springgreen") +
  ggtitle("Časovna struktura Euribor") + labs(x = "Dospetje (mesec)", y = "%")



# 3.naloga
<<<<<<< HEAD
=======

# Funkcija za izracun terminske obrestne mere v tabeli X

termX <- function(m){
  matrika <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), nrow = 12, ncol = 1)
  matrika <- as.data.frame(matrika)
  for (i in m){
    x <- 4*((1 + 0.75 * X[i, 2])/(1 + 0.5*X[i, 1]) - 1)
    matrika[i, ] <- x
  }
  
  return(matrika)
}

# x <- c(1:12)

# Funkcija za izracun terminske obrestne mere v tabeli Y

termY <- function(m){
  matrika <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), nrow = 12, ncol = 1)
  matrika <- as.data.frame(matrika)
  for (i in m){
    x <- 4*((1 + 0.75 * Y[i, 2])/(1 + 0.5*Y[i, 1]) - 1)
    matrika[i, ] <- x
  }
  
  return(matrika)
}

# Funkcija za izracun terminske obrestne mere v tabeli Z

termZ <- function(m){
  matrika <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), nrow = 12, ncol = 1)
  matrika <- as.data.frame(matrika)
  for (i in m){
    x <- 4*((1 + 0.75 * Z[i, 2])/(1 + 0.5*Z[i, 1]) - 1)
    matrika[i, ] <- x
  }
  
  return(matrika)
}

x = c(1:12)

# 3 b)

# Leto 2015

m2015 <- termX(x)
tb2015 <- eu15[5]
tb2015["napoved3m"] <- termX(x)
tb2015[1,2] <- NA
tb2015[2,2] <- NA
tb2015[3,2] <- NA
tb2015[4,2] <- NA
tb2015[5,2] <- NA
tb2015[6,2] <- NA
tb2015[7,2] <- m2015[1,1]
tb2015[8,2] <- m2015[2,1]
tb2015[9,2] <- m2015[3,1]
tb2015[10,2] <- m2015[4,1]
tb2015[11,2] <- m2015[5,1]
tb2015[12,2] <- m2015[6,1]

# Leto 2016

m2016 <- termY(x)
tb2016 <- eu16[5]
tb2016["napoved3m"] <- termY(x)
tb2016[1,2] <- NA
tb2016[2,2] <- NA
tb2016[3,2] <- NA
tb2016[4,2] <- NA
tb2016[5,2] <- NA
tb2016[6,2] <- NA
tb2016[7,2] <- m2016[1,1]
tb2016[8,2] <- m2016[2,1]
tb2016[9,2] <- m2016[3,1]
tb2016[10,2] <- m2016[4,1]
tb2016[11,2] <- m2016[5,1]
tb2016[12,2] <- m2016[6,1]

# Leto 2017

m2017 <- termZ(x)
tb2017 <- eu17[5]
tb2017["napoved3m"] <- termZ(x)
tb2017[1,2] <- NA
tb2017[2,2] <- NA
tb2017[3,2] <- NA
tb2017[4,2] <- NA
tb2017[5,2] <- NA
tb2017[6,2] <- NA
tb2017[7,2] <- m2017[1,1]
tb2017[8,2] <- m2017[2,1]
tb2017[9,2] <- m2017[3,1]
tb2017[10,2] <- m2017[4,1]
tb2017[11,2] <- m2017[5,1]
tb2017[12,2] <- m2017[6,1]

tabela <- rbind(tb2015, tb2016, tb2017)
>>>>>>> 1efae44a806f047fb6c5698d0105e20032af12c9

# Funkcija za izracun terminske obrestne mere v tabeli X

termX <- function(m){
  matrika <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), nrow = 12, ncol = 1)
  matrika <- as.data.frame(matrika)
  for (i in m){
    x <- 4*((1 + 0.75 * X[i, 2])/(1 + 0.5*X[i, 1]) - 1)
    matrika[i, ] <- x
  }
  
  return(matrika)
}

# x <- c(1:12)

# Funkcija za izracun terminske obrestne mere v tabeli Y

termY <- function(m){
  matrika <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), nrow = 12, ncol = 1)
  matrika <- as.data.frame(matrika)
  for (i in m){
    x <- 4*((1 + 0.75 * Y[i, 2])/(1 + 0.5*Y[i, 1]) - 1)
    matrika[i, ] <- x
  }
  
  return(matrika)
}

# Funkcija za izracun terminske obrestne mere v tabeli Z

termZ <- function(m){
  matrika <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), nrow = 12, ncol = 1)
  matrika <- as.data.frame(matrika)
  for (i in m){
    x <- 4*((1 + 0.75 * Z[i, 2])/(1 + 0.5*Z[i, 1]) - 1)
    matrika[i, ] <- x
  }
  
  return(matrika)
}

x = c(1:12)

# 3 b)

# Leto 2015

m2015 <- termX(x)
tb2015 <- eu15[5]
tb2015["napoved3m"] <- termX(x)
tb2015[1,2] <- NA
tb2015[2,2] <- NA
tb2015[3,2] <- NA
tb2015[4,2] <- NA
tb2015[5,2] <- NA
tb2015[6,2] <- NA
tb2015[7,2] <- m2015[1,1]
tb2015[8,2] <- m2015[2,1]
tb2015[9,2] <- m2015[3,1]
tb2015[10,2] <- m2015[4,1]
tb2015[11,2] <- m2015[5,1]
tb2015[12,2] <- m2015[6,1]

# Leto 2016

m2016 <- termY(x)
tb2016 <- eu16[5]
tb2016["napoved3m"] <- termY(x)
tb2016[1,2] <- NA
tb2016[2,2] <- NA
tb2016[3,2] <- NA
tb2016[4,2] <- NA
tb2016[5,2] <- NA
tb2016[6,2] <- NA
tb2016[7,2] <- m2016[1,1]
tb2016[8,2] <- m2016[2,1]
tb2016[9,2] <- m2016[3,1]
tb2016[10,2] <- m2016[4,1]
tb2016[11,2] <- m2016[5,1]
tb2016[12,2] <- m2016[6,1]

# Leto 2017

m2017 <- termZ(x)
tb2017 <- eu17[5]
tb2017["napoved3m"] <- termZ(x)
tb2017[1,2] <- NA
tb2017[2,2] <- NA
tb2017[3,2] <- NA
tb2017[4,2] <- NA
tb2017[5,2] <- NA
tb2017[6,2] <- NA
tb2017[7,2] <- m2017[1,1]
tb2017[8,2] <- m2017[2,1]
tb2017[9,2] <- m2017[3,1]
tb2017[10,2] <- m2017[4,1]
tb2017[11,2] <- m2017[5,1]
tb2017[12,2] <- m2017[6,1]

tabela <- rbind(tb2015, tb2016, tb2017)

# Ni mi uspelo dokoncati naloge, v cimkrajsem casu jo dokoncam in vam poslem novo verzijo

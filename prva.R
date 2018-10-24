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

graf <- ts.plot(ts(data=XYZ, start=c(2015,1,2), end=c(2017,12,1), frequency = 12), gpars=list(xlab="Time", ylab="%"), col=c("red", "blue"));
        title(main="Euribor"); 
        legend("topleft", legend = c("3m", "6m"), col=c("red", "blue"), lty = c(1,1))




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
XYZ <- type.convert(XYZ, "matrix")

graf <- ts.plot(ts(data=XYZ, start=c(2015,1,2), end=c(2017,12,1), frequency = 12), gpars=list(xlab="Time", ylab="%"), col=c("red", "blue"));
        title(main="Euribor"); 
        legend("topright", legend = c("3m", "6m"), col=c("red", "blue"), lty = c(1,1))

# 2.naloga

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



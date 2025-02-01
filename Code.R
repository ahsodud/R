wd <- getwd()
setwd(wd) 



# R1: Beispiel Festplatte 

disk <- read.csv("disk.csv", header = TRUE)
head(disk)
mean(disk$time)
median(disk$time)
sd(disk$time)
plot(time ~ length, data = disk)

# R2: Beispiel Cats

library(MASS)
head(cats)
hist(cats$Hwt, freq = FALSE)

# R3: Beispiel CPU

cpu <- c(1.86, 3.49, 2.63, 3.49, 1.69, 1.83, 0.81, 4.70,
         0.85, 4.24, 3.49, 2.75, 1.65, 0.92, 0.62)
sort(cpu)
n <- length(cpu)
p <- c(0.25, 0.5, 0.75)
sort(cpu)[floor(n * p) + 1]

# R4: Lokationsmasse CPU

cpu
mean(cpu)
mean(cpu, trim = 0.25)

# R5: Funktionen zur Erstellung explorativer Grafiken

plot(ecdf(cpu))
hist(cats$Hwt, freq = FALSE)
hist(cats$Hwt, breaks = 6:21, freq = FALSE)
boxplot(Bwt ~ Sex, data = cats)

height <- survey$Height[which(survey$Sex == "Male")]
qqnorm(height)
qqline(height)
plot(Hwt ~ Bwt, data = cats)

# R6: Simulation explorativer Grafiken 

set.seed(123)
n <- 100

sample <- rnorm(n, mean = 0, sd = 1)
grid <- seq(-10, 10, length.out = 1000)
par(mfrow = c(2, 2))
hist(sample, freq = FALSE)
lines(grid, dnorm(grid, mean = 0, sd = 1))
plot(ecdf(sample))
lines(grid, pnorm(grid, mean = 0, sd = 1))
boxplot(sample)
qqnorm(sample)
qqline(sample)
dev.off()

# R7: t-Test 

x <- c(0.63,  1.56,  1.26, -0.31,  3.87,  0.03,  4.92,  0.90)
alpha <- 0.05
n <- length(x)
t <- sqrt(n) * (mean(x) - 3)/sd(x)
t < qt(1-alpha, df = n - 1)
pt(t, df = n - 1)

t.test(x, alternative = "less", mu = 3)

# R8: Beispiel Mietpreise

x <- c(14.57, 17.61, 15.34, 9.88, 12.75,
       13.71, 13.45, 14.88, 11.67, 12.26)
y <- c(13.14, 11.48, 13.79, 8.85, 9.57,
       12.74, 9.84, 12.51)
n <- length(x)
m <- length(y)
sigma_x <- 2.2
sigma_y <- 2
z <- (mean(x)-mean(y))/sqrt(sigma_x^2/n + sigma_y^2/m)
p.value <- 2 * pnorm(abs(z), lower.tail = FALSE)
p.value

# R9: Einleitung zur linearen Regression

x = 100 * 1:10
y = c(43.98, 98.11, 116.53, 137.31, 186.24,
      213.17, 255.67, 273.97, 318.05, 360.31)
disk <- data.frame(time = y, length = x)
disk

plot(disk$length, disk$time, ylim = c(0, max(disk$time)), xlim = c(0, 1000),
     xlab = "Datenlänge (kB)", ylab = "Lesezeit (ms)")
lm <- lm(time ~ length, disk)
abline(a = lm$coefficients[1], b = lm$coefficients[2])

# R10: Beispiel Festplatte, Lineares Modell

lm.disk <- lm(time ~ length, disk)
summary(lm.disk)

# R11: Beispiel Festplatte, Konfidenz- und Prädiktionsintervall 

x.h <- data.frame(length = 250)
predict(lm.disk, x.h, interval = "confidence", level = 0.95)
predict(lm.disk, x.h, interval = "predict", level = 0.95)

# R12: Mooresches Gesetz 

moore <- read.csv("moore.csv")
head(moore)

# Einfaches lineares Regressionsmodell
plot(moore$time, moore$speed)
fit0 <- lm(speed ~ time, data = moore)
summary(fit0)
abline(fit0)

# Modell bei logarithmierter Response
plot(moore$time, log(moore$speed, 2))
fit1 <- lm(I(log(speed, 2)) ~ time, data = moore)
summary(fit1)
abline(fit1)

# Regressionskurve
plot(moore$time, moore$speed)
lines(moore$time, exp(fit1$fitted.values))

# R13: Beispiel Brandschäden

fire <- read.csv("fire.csv", header = TRUE)
head(fire)

plot(fire$distance, fire$damage, xlim = c(0, 7), ylim = c(0, 45))
title(ylab = "Schadenssumme")
title(xlab = "Distanz")

lm.fire <- lm(damage ~ distance, data = fire)
summary(lm.fire)

confint(lm.fire, level = 0.95)
newdata <- data.frame(distance = 3.5)
predict(lm.fire, newdata, interval = "predict", level = 0.95)

plot(fire$distance, fire$damage, xlim = c(0, 7), ylim = c(0, 45),
     ylab = "Schadenssumme", xlab = "Distanz")
abline(a = lm.fire$coefficients[1],
       b = lm.fire$coefficients[2])

# R14: Mietpreis

home <- read.csv("home.csv")
head(home)

plot(home$area, home$price, xlab = "Fläche (m2)", ylab = "Mietkosten (EUR)")
lm.home <- lm(price ~ area, home)
summary(lm.home)

dwelling <- data.frame(area = 55)
predict(lm.home, dwelling, interval = "confidence", level = 0.95)
predict(lm.home, dwelling, interval = "prediction", level = 0.95)

plot(lm.home)

# Ohne Ausreißer

home2 <- home[-c(5,32),]
lm.home2 <- lm(price ~ area, home2)
summary(lm.home2)

dwelling <- data.frame(area = 55)
predict(lm.home2, dwelling, interval = "confidence", level = 0.95)
predict(lm.home2, dwelling, interval = "prediction", level = 0.95)

plot(home$area, home$price, ann = FALSE)
title(ylab = "Mietkosten (EUR)")
title(xlab = "Fläche (m2)")
abline(a = lm.home$coefficients[1],
       b = lm.home$coefficients[2],)
abline(a = lm.home2$coefficients[1],
       b = lm.home2$coefficients[2], lty = 2)

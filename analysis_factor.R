us_health<- read.csv('us_healthfix.csv',header=TRUE)
us_health
us_healthused<- us_health[,3:12]
us_healthused

# fungsi untuk mendapatkan perkiraan faktor
# Tujuan utama dari faktor analisis adalah untuk mencari loadings Q dan 
# spesifik variansi (Uniquenesses)
us_healthused.fa<- factanal(us_healthused,factors = 4)
us_healthused.fa

# mencari communalities dengan loadings^2
apply(us_healthused.fa$loadings^2,1,sum)

# menampilkan spesifik variansi (Uniquenesses)
us_healthused.fa$uniquenesses
1 - apply(us_healthused.fa$loadings^2,1,sum) 

# Penerapan  Principal Component Method
# membuat matriks diagonal dari uniquenesses berukuran k x k
# dan membuat variabel baru
Q = us_healthused.fa$loadings
Psi = diag(us_healthused.fa$uniquenesses)
Sigma <- Q %*% t(Q) + Psi
Sigma

# akan dicari sample matriks korelasi terdiagonalisasi
S <- us_healthused.fa$correlation
S

# akan dicari matriks residual untuk menentukan
# bagus tidaknya sebuah aproksimasi
# bulatkan sebanyak 6 digit
round(S - Sigma,6)
# Semakin mendekati 0 mengindikasikan model faktornya
# merupakan representasi yang bagus

####################################################

# faktor k = 2
# interpretasi dari faktor analisis dengan 4 faktor
us_healthused.fa.none<- factanal(us_healthused,factors = 4,
                                 rotation = "none")
us_healthused.fa.none

# rotasi ortogonal (varimax rotation method)
us_healthused.fa.varimax<- factanal(us_healthused,factors = 4,
                                 rotation = "varimax")
us_healthused.fa.varimax

# membuat subplot dengan faktor k = 4
# *Score Plot* Faktor 1 Metode Tidak Rotasi
par(mfrow = c(1,3))
plot(us_healthused.fa.none$loadings[,1],
     us_healthused.fa.none$loadings[,2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation (1,2)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,1]-0.08,
     us_healthused.fa.none$loadings[,2]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

plot(us_healthused.fa.none$loadings[,1],
     us_healthused.fa.none$loadings[,3],
     xlab = "Factor 1",
     ylab = "Factor 3",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation (1,3)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,1]-0.08,
     us_healthused.fa.none$loadings[,3]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

plot(us_healthused.fa.none$loadings[,1],
     us_healthused.fa.none$loadings[,4],
     xlab = "Factor 1",
     ylab = "Factor 4",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation (1,4)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,1]-0.08,
     us_healthused.fa.none$loadings[,4]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

# *Score Plot* Faktor 2 Metode Tidak Rotasi
par(mfrow = c(1,3))
plot(us_healthused.fa.none$loadings[,2],
     us_healthused.fa.none$loadings[,1],
     xlab = "Factor 2",
     ylab = "Factor 1",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(2,1)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,2]-0.08,
     us_healthused.fa.none$loadings[,1]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

plot(us_healthused.fa.none$loadings[,2],
     us_healthused.fa.none$loadings[,3],
     xlab = "Factor 2",
     ylab = "Factor 3",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(2,3)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,2]-0.08,
     us_healthused.fa.none$loadings[,3]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

plot(us_healthused.fa.none$loadings[,2],
     us_healthused.fa.none$loadings[,4],
     xlab = "Factor 2",
     ylab = "Factor 4",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(2,4)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,2]-0.08,
     us_healthused.fa.none$loadings[,4]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

# *Score Plot* Faktor 3 Metode Tidak Rotasi
par(mfrow = c(1,3))
plot(us_healthused.fa.none$loadings[,3],
     us_healthused.fa.none$loadings[,1],
     xlab = "Factor 3",
     ylab = "Factor 1",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(3,1)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,3]-0.08,
     us_healthused.fa.none$loadings[,1]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

plot(us_healthused.fa.none$loadings[,3],
     us_healthused.fa.none$loadings[,2],
     xlab = "Factor 3",
     ylab = "Factor 2",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(3,2)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,3]-0.08,
     us_healthused.fa.none$loadings[,2]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)


plot(us_healthused.fa.none$loadings[,3],
     us_healthused.fa.none$loadings[,4],
     xlab = "Factor 3",
     ylab = "Factor 4",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(3,4)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,3]-0.08,
     us_healthused.fa.none$loadings[,4]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

# *Score Plot* Faktor 4 Metode Tidak Rotasi
par(mfrow = c(1,3))
plot(us_healthused.fa.none$loadings[,4],
     us_healthused.fa.none$loadings[,1],
     xlab = "Factor 4",
     ylab = "Factor 1",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(4,1)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,4]-0.08,
     us_healthused.fa.none$loadings[,1]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

plot(us_healthused.fa.none$loadings[,4],
     us_healthused.fa.none$loadings[,2],
     xlab = "Factor 4",
     ylab = "Factor 2",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(4,2)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,4]-0.08,
     us_healthused.fa.none$loadings[,2]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

plot(us_healthused.fa.none$loadings[,4],
     us_healthused.fa.none$loadings[,3],
     xlab = "Factor 4",
     ylab = "Factor 3",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "no rotation(4,3)")
abline(h = 0, v = 0)

text(us_healthused.fa.none$loadings[,4]-0.08,
     us_healthused.fa.none$loadings[,3]+0.08,
     colnames(us_healthused),
     col="red")
abline(h = 0, v = 0)

# *Score Plot* Faktor 1 Metode Rotasi Varimax
par(mfrow = c(1,3))
plot(us_healthused.fa.varimax$loadings[,1],
     us_healthused.fa.varimax$loadings[,2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(1,2)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,1]-0.08,
     us_healthused.fa.varimax$loadings[,2]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

plot(us_healthused.fa.varimax$loadings[,1],
     us_healthused.fa.varimax$loadings[,3],
     xlab = "Factor 1",
     ylab = "Factor 3",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(1,3)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,1]-0.08,
     us_healthused.fa.varimax$loadings[,3]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

plot(us_healthused.fa.varimax$loadings[,1],
     us_healthused.fa.varimax$loadings[,4],
     xlab = "Factor 1",
     ylab = "Factor 4",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(1,4)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,1]-0.08,
     us_healthused.fa.varimax$loadings[,4]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

# *Score Plot* Faktor 2 Metode Rotasi Varimax
par(mfrow = c(1,3))
plot(us_healthused.fa.varimax$loadings[,2],
     us_healthused.fa.varimax$loadings[,1],
     xlab = "Factor 2",
     ylab = "Factor 1",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(2,1)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,2]-0.08,
     us_healthused.fa.varimax$loadings[,1]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

plot(us_healthused.fa.varimax$loadings[,2],
     us_healthused.fa.varimax$loadings[,3],
     xlab = "Factor 2",
     ylab = "Factor 3",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(2,3)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,2]-0.08,
     us_healthused.fa.varimax$loadings[,3]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

plot(us_healthused.fa.varimax$loadings[,2],
     us_healthused.fa.varimax$loadings[,4],
     xlab = "Factor 2",
     ylab = "Factor 4",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(2,4)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,2]-0.08,
     us_healthused.fa.varimax$loadings[,4]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

# *Score Plot* Faktor 3 Metode Rotasi Varimax
par(mfrow = c(1,3))
plot(us_healthused.fa.varimax$loadings[,3],
     us_healthused.fa.varimax$loadings[,1],
     xlab = "Factor 3",
     ylab = "Factor 1",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(3,1)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,3]-0.08,
     us_healthused.fa.varimax$loadings[,1]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

plot(us_healthused.fa.varimax$loadings[,3],
     us_healthused.fa.varimax$loadings[,2],
     xlab = "Factor 3",
     ylab = "Factor 2",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(3,2)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,3]-0.08,
     us_healthused.fa.varimax$loadings[,2]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

plot(us_healthused.fa.varimax$loadings[,3],
     us_healthused.fa.varimax$loadings[,4],
     xlab = "Factor 3",
     ylab = "Factor 4",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(3,4)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,3]-0.08,
     us_healthused.fa.varimax$loadings[,4]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

# *Score Plot* Faktor 4 Metode Rotasi Varimax
par(mfrow = c(1,3))
plot(us_healthused.fa.varimax$loadings[,4],
     us_healthused.fa.varimax$loadings[,1],
     xlab = "Factor 4",
     ylab = "Factor 1",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(4,1)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,4]-0.08,
     us_healthused.fa.varimax$loadings[,1]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

plot(us_healthused.fa.varimax$loadings[,4],
     us_healthused.fa.varimax$loadings[,2],
     xlab = "Factor 4",
     ylab = "Factor 2",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(4,2)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,4]-0.08,
     us_healthused.fa.varimax$loadings[,2]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

plot(us_healthused.fa.varimax$loadings[,4],
     us_healthused.fa.varimax$loadings[,3],
     xlab = "Factor 4",
     ylab = "Factor 3",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "Varimax rotation(4,3)")
abline(h = 0, v = 0)

text(us_healthused.fa.varimax$loadings[,4]-0.08,
     us_healthused.fa.varimax$loadings[,3]+0.08,
     colnames(us_healthused),
     col="blue")
abline(h = 0, v = 0)

# Perhitungan *loadings* dan *Uniquenesses* Dengan *factors* 5 dan Tidak Rotasi
us_healthused.fa.none5<- factanal(us_healthused,factors = 5,
                                 rotation = "none")
us_healthused.fa.none5

# Perhitungan *loadings* dan *Uniquenesses* Dengan *factors* 5 dan Rotasi Menggunakan Metode Varimax
us_healthused.fa.varimax5<- factanal(us_healthused,factors = 5,
                                    rotation = "varimax")
us_healthused.fa.varimax5
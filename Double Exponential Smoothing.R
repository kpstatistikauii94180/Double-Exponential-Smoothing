vaksin=read.csv(file.choose(),header = TRUE, sep = ";")
View(vaksin)

vaksin.dosis2=vaksin$dosis2
vaksin.tanggal=vaksin$tanggal

#analisis deskriptif 
plot(vaksin.dosis2, main = "Plot Data Vaksinasi Dosis Lengkap di Indonesia", lwd = 1, col = "blue", xlim = vaksin.tanggal, 
     type = "o")

summary(vaksin.dosis2)
#mengurutkan data terlama ke data terbaru
#vaksin.rev=vaksin[order(nrow(vaksin):1),]
#vaksin.rev

#ambil data dosis2 dari objek vaksin, lalu ubah menjadi time series
vaksin.ts=ts(vaksin$dosis2, start = c(2021,1),frequency=365)
vaksin.ts
plot(vaksin.ts)

#lakukan analisis data dengan metode holt dengan nilai alpa dan beta optimum (sehingga nilainya NULL) dengan perintah Holtwinter
holtb.vaksin=HoltWinters(vaksin.ts,alpha=NULL,beta=NULL,gamma=FALSE)


#tampilkan nilai fit value atau nilai prediksi
holtb.vaksin$fitted
#tampilkan SSE, MSE, RMSE, MAPE untuk menjadi pembanding mana metode yang terbaik
sse = holtb.vaksin$SSE
mse = holtb.vaksin$SSE/NROW(holtb.vaksin$fitted)
rmse = sqrt (mse)
mape = mean(abs((vaksin.ts-holtb.vaksin$fitted[,1])/vaksin.ts),na.rm = TRUE)*100
sse
mse
mape

#melakukan peramalan untuk 155 periode kedepan
pred.holtb = predict(holtb.vaksin, 161)
pred.holtb
plot(pred.holtb)

#Plot data aktual dan hasil peramalan 
plot(vaksin.ts, main = "", lwd = 1, col = "blue", xlim = c(2021,2022), 
     type = "o")
limitDate = end(vaksin.ts)[1]+(end(vaksin.ts)[2]-1)/frequency(vaksin.ts)
abline(v=limitDate ,lty=4)
lines(holtb.vaksin$fitted[,1], lwd = 2, col = "red", type = "o")
lines(pred.holtb, col = "green", type = "o", pch = 5)
legend("topright", legend = c("Data aktual", "Fitted value", "Peramalan"), 
       col=c("blue", "red","green"), lty=1, cex = 0.6, inset=0.02)

##########Menggunakan package forecast 
library(forecast)

holt.vaksin = holt(vaksin.ts, h = 5, damped = TRUE, alpha = NULL, beta = NULL, phi = NULL)
holt.vaksin$model
holt.vaksin
holt.vaksin$fitted

mse = mean(holt.vaksin$residuals^2)
rmse = sqrt(mse)
mape = mean(abs(holt.vaksin$residuals)/vaksin.ts, na.ra = TRUE)*100
mse
rmse
mape

plot(holt.vaksin, main="Plot Data Aktual dan Hasil Peramalan", col ="purple", lwd=2)
lines(holt.vaksin$fitted, col = "green")
legend("topleft", legend = c("Data Aktual", "Fitted value"), col = c("purple", "green"), lty = 1, cex = 0.8, inset = 0.02)

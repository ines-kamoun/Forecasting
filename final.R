# Install Required Libraries
install.packages(c("tseries", "ggplot2" , "forecast" ,"rugarch"))

# Load Libraries
library(tseries)
library(ggplot2)

# view summary
summary(CPILFESL)

# Check for missing values in the "CPI" column
any(is.na(CPILFESL$CPI))


# Check the Time Series Plot
ggplot(data = CPILFESL, aes(x = observation_date, y = CPI)) +
  geom_line() +
  labs(title = "Time Series Plot", x = "DATE", y = "CPI")

# Make First Difference
CPILFESL$CPI_diff1 <- c(NA, diff(CPILFESL$CPI))


# Check the Time Series Plot of the First Difference
ggplot(data = CPILFESL, aes(x = observation_date, y = CPI_diff1)) +
  geom_line() +
  labs(title = "Time Series Plot of First Difference", x = "Date", y = "CPI first Difference")

# Make First Difference
CPILFESL$CPI_diff1 <- c(NA, diff(CPILFESL$CPI))

# Make Second Difference
CPILFESL$CPI_diff2 <- c(NA, diff(CPILFESL$CPI_diff1))

# Check the Time Series Plot of the Second Difference
ggplot(data = CPILFESL, aes(x = observation_date, y = CPI_diff2)) +
  geom_line() +
  labs(title = "Time Series Plot of Second Difference", x = "Date", y = "CPI Second Difference")

any(is.na(CPILFESL$CPI_diff2))
# Mean imputation
CPILFESL$CPI_diff2[is.na(CPILFESL$CPI_diff2)] <- mean(CPILFESL$CPI_diff2, na.rm = TRUE)

#perform unit root and stationarity test 
# ADF Test
adf.test(CPILFESL$CPI_diff2)

# PP test
pp.test(CPILFESL$CPI_diff2)

# KPSS Test
kpss.test(CPILFESL$CPI_diff2, null="Trend")

View(CPILFESL)
class(CPILFESL)
#we have to convert the data into time series first!
cpitime=ts(CPILFESL$CPI_diff2,start = 1990,end = 2023, frequency = 1)
class(cpitime)

#choosing model specification
library(forecast)
library(tseries)
par(mar = c(1, 1, 1, 1)) # make the margins smaller
plot(cpitime)
acf(cpitime)
pacf(cpitime)
adf.test(cpitime)
#Parameter estimation
cpimodel=auto.arima(cpitime,ic="aic",trace = TRUE)
cpimodel
#plotting with residuals
acf(ts(cpimodel$residuals))
pacf(ts(cpimodel$residuals))

#validate forecast using box test
Box.test(cpitime, lag=5, type= "Ljung-Box")
Box.test(CPI_diff2, lag=15, type= "Ljung-Box")
Box.test(CPI_diff2, lag=25, type= "Ljung-Box")

#forecast part
mycpiforecast=forecast(cpimodel,level = c(95),h=10*1)
mycpiforecast
plot(mycpiforecast)

#validate forecast using box test
Box.test(mycpiforecast$residuals, lag=5, type= "Ljung-Box")
Box.test(mycpiforecast$residuals, lag=15, type= "Ljung-Box")
Box.test(mycpiforecast$residuals, lag=25, type= "Ljung-Box")


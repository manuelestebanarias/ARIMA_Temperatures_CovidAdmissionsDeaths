################################################################################
##########  TiMe Series analysis for Covid 19 Prediction #######################
################################################################################
################################## By Manuel Esteban Arias #####################
################################################################################

## Pakage import
library(readr)
library(forecast)
library(dplyr)
library(lubridate)
library(tseries)
library(stargazer)
library(ggplot2)
library(ggpubr)

################### Data Import ###############################################
##Charging the database

covid19 <- read_csv("C:/Users/manue/Downloads/covid19.csv", skip = 1)

temperatures <- read_csv("C:/Users/manue/Downloads/temperatures.csv",skip = 1)
##Generate date row
covid19$date<-paste(covid19$jj,"/",covid19$mm,"/",covid19$aaaa, sep = "")
temperatures$date<-paste(temperatures$jj,"/",temperatures$mm,"/",
                         temperatures$aaaa, sep = "")
##merging the datasets
data0 <- merge(covid19,temperatures,by="date")
data0 <- full_join(covid19,temperatures,by="date")
data0$date <- as.Date(data0$date, "%d/%m/%Y")

## order the data in cronological order
data0 <- data0[order(data0$date,decreasing=FALSE),]


################################################################################
################################################################################
############Creating the data for ADMISSIONS time series analysis ##############
################################################################################
################################################################################

############## Data celaning #########################################################
# Extract the desired observations from columns "admis," "temp_max," and "temp_min"
admis_subset <- data0[19:100,c("date","admissions")]
temp_max_subset <- data0[11:92, c("date","temp_max")]
temp_min_subset <- data0[11:92, c("temp_min")]

# Create a new dataframe with aligned observations
data_admis <- data.frame(admis = admis_subset, temp_max = temp_max_subset, temp_min = temp_min_subset)

############### Define Time series #############################################
#Define the admis as TS
starting_date <- ymd("2020-03-19")# = 79th day of the year
ending_date <- ymd("2020-06-08")# = 160th day of the year
num_days <- as.numeric(ending_date - starting_date + 1)
admis <- ts(data_admis$admis.admissions, start = c(2020,79),end = c(2020,160),
            frequency = 365)


#Define the temperatures as TS
starting_date <- ymd("2020-03-11")#= 71th of the year
ending_date <- ymd("2020-05-31")#= 152th dqy of the yeqr
num_days <- as.numeric(ending_date - starting_date + 1)
temp_max <- ts(data_admis$temp_max.temp_max, start = c(2020,71),end = c(2020,152),
            frequency = 365)
temp_min <- ts(data_admis$temp_min.temp_min, start = c(2020,71),end = c(2020,152),
               frequency = 365)
############## Generating train and datasets ###################################

start_date <- time(admis)[1]  # Get the start date of the time series
end_date <- start_date + (0.7 * (time(admis)[length(admis)] - start_date))  # Calculate the desired end date
end_date2 <- start_date + (0.701 * (time(admis)[length(admis)] - start_date))

start_dateT <- time(temp_max)[1]
end_dateT <- start_dateT + (0.70 * (time(temp_max)[length(temp_max)] - start_dateT))
end_date2T <- start_dateT + (0.701 * (time(temp_max)[length(temp_max)] - start_dateT))
# For admis
train_admis <- window(admis, end = end_date)  # 70% of the data
test_admis <- window(admis, start = end_date2)  # 30% of the data
# For temperatures
train_temp_max <- window(temp_max, end = end_dateT)
test_temp_max <- window(temp_max, start = end_date2T)
train_temp_min <- window(temp_min, end = end_dateT)
test_temp_min <- window(temp_min, start = end_date2T)
################################################################################
################Box Jenkings methdoology #######################################
################################################################################

# 1. Plot the time series
plot(train_admis, xlab = "Date", ylab = "Admissions", main = "Time Series - Admissions")

# 2. verify stationarity
# Augmented dickey-fuller 
adf.test(train_admis)

#3. make the series stationary
train_admis_diff <- diff(train_admis, differences =3)
plot(train_admis_diff, ylab= "Diff3 train_admissions")

#3. Model identifying.
#Auctocorrelation
acf(train_admis, lag.max = 20)
# Partial autocorreilation funciton
pacf_values <- pacf(train_admis_diff)
plot(pacf_values, xlab = "Lag", ylab = "Partial Autocorrelation", main = "Partial Autocorrelation Function (PACF)")

#4. Model estimations
arimaModel_01=arima(train_admis, order=c(1,0,0))
arimaModel_02=arima(train_admis, order=c(1,0,1))
arimaModel_1=arima(train_admis, order=c(2,0,0))
arimaModel_2=arima(train_admis, order=c(2,0,1))
arimaModel_3=arima(train_admis, order=c(3,0,0))
arimaModel_4=arima(train_admis, order=c(3,0,1))
arimaModel_5=arima(train_admis, order=c(4,0,0))
arimaModel_6=arima(train_admis, order=c(4,0,1))
arimaModel_7=arima(train_admis, order=c(5,0,0))
stargazer(arimaModel_01, title = "ARIMA Model", out = "C:/Users/manue/Downloads/admis_arima.docx", type = "text")

###ARIMAX

lag_temp_max <- stats::lag(temp_max, k = 8)
lag_temp_min <- stats::lag(temp_min, k = 8)

# Ensure equal lengths of series
n <- min(length(admis), length(temp_max), length(temp_min))
admis_trial <- admis[1:n]
lag_temp_max <- lag_temp_max[1:n]
lag_temp_min <- lag_temp_min[1:n]



ArimaxModel=arima(admis, order=c(1,0,0), xreg = cbind(lag_temp_max, lag_temp_min))
stargazer(ArimaxModel , title = " Arimax Model Admissions - temperatures lagged 8 ", out = "C:/Users/manue/Downloads/resid_arimax_trial.docx", type = "text")



#5. Verification
residuals <- residuals(arimaModel_01)
lb_test <- Box.test(residuals, lag = 12, type = "Ljung-Box")
#6 prediction
final_admis <- Arima(train_admis, order = c(1,0,0))
final_admisX <- Arima(train_admis, order = c(1,0,0), xreg = cbind(train_temp_max, train_temp_min))
#foecast
forecast_admis <- forecast(final_admis)
plot(forecast_admis, ylim = c(0,350), xlim = c(2020.22,2020.43))
lines(test_admis, col = "blue")

#foecast
forecast_admisX <- forecast(final_admisX, xreg = cbind(train_temp_max, train_temp_min), h = length(test_admis))
forecast_admis_train <- forecast(final_admis, xreg = cbind(train_temp_max, train_temp_min))#, h = length(test_admis))
accuracy(forecast_admis, test_admis)
plot(forecast_admisX, ylim = c(0,350), xlim = c(2020.22,2020.43))
lines(test_admis, col = "blue")
###############################################################################
################## Full model #################################################
arimaxModel_full0=arima(admis, order=c(1,0,0), xreg = cbind(temp_max, temp_min))
stargazer(arimaxModel_full0, title = "ARIMAX Model full series", out = "C:/Users/manue/Downloads/admis_arimax-full.docx", type = "text")
residuals_full0 <- residuals(arimaxModel_full0)
lb_test <- Box.test(residuals0, lag = 12, type = "Ljung-Box")

################## Modell for asseging correlations
arimaxModel_full=arima(admis, order=c(1,0,0))
residuals_full <- residuals(arimaxModel_full)
################################################################################
#######################Measuring Correlations       ############################
################################################################################


########## Testing the correlations suggested by the paper

# Calculate the Pearson correlation coefficient and p-value
cor_result <- cor.test(residuals_full, lag_temp_max)

# Create the scatter plot
p3 <- ggplot(data.frame(residuals_full, lag_temp_max), aes(x = residuals_full, y = lag_temp_max)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = lm, color = "red", fill = "#69b3a2", se = TRUE) +
  labs(title = "Scatter Plot of temp_max and residuals_full", 
       x = "residuals_full", 
       y = "lag_temp_max") +
  theme_minimal()

# Add correlation coefficient and p-value as annotations
p3_with_annotations <- p3 +
  annotate("text", x = Inf, y = Inf, 
           label = paste("Correlation:", round(cor_result$estimate, 3)),
           hjust = 1, vjust = 1, size = 5) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("p-value:", format(cor_result$p.value, scientific = TRUE, digits = 3)),
           hjust = 1, vjust = 3, size = 5)

p3_with_annotations





# Calculate the Pearson correlation coefficient and p-value
cor_result <- cor.test(residuals_full, lag_temp_min)

# Create the scatter plot
p4 <- ggplot(data.frame(residuals_full, lag_temp_min), aes(x = residuals_full, y = lag_temp_min)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = lm, color = "red", fill = "#69b3a2", se = TRUE) +
  labs(title = "Scatter Plot of temp_min and residuals_full", 
       x = "residuals_full", 
       y = "lag_temp_min") +
  theme_minimal()

# Add correlation coefficient and p-value as annotations
p4_with_annotations <- p4 +
  annotate("text", x = Inf, y = Inf, 
           label = paste("Correlation:", round(cor_result$estimate, 3)),
           hjust = 1, vjust = 1, size = 5) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("p-value:", format(cor_result$p.value, scientific = TRUE, digits = 3)),
           hjust = 1, vjust = 3, size = 5)

p4_with_annotations

#Regression
RegResid <- lm(residuals_full ~ temp_max + temp_min, data = data.frame(residuals_full, temp_min, temp_max))
stargazer(RegResid , title = "OLS regresion fullresiduals - temperatures ", out = "C:/Users/manue/Downloads/resid_arimax-full.docx", type = "text")


plot(residuals_full)

# Calculate the cross-correlation between admis and temp_max
cross_corr <- ccf(residuals_full, temp_max)

# Plot the cross-correlation function
plot(cross_corr, main = "Cross-Correlation: admis_residuals vs temp_max", xlab = "Lag", ylab = "Correlation")

# Find the lag with the maximum correlation
max_corr_lag <- which.min(cross_corr$acf)
max_corr <- cross_corr$acf[max_corr_lag]
lag <- cross_corr$lag[max_corr_lag]

# Print the lag and the maximum correlation
cat("Lag with maximum correlation:", lag* 365, "\n")
cat("Maximum correlation:", max_corr, "\n")
cor.test(residuals_full,lag_temp_max)


# Calculate the cross-correlation between admis and temp_min
cross_corr2 <- ccf(residuals_full, temp_min)

# Plot the cross-correlation function
plot(cross_corr2, main = "Cross-Correlation: admis_residuals vs temp_mmin", xlab = "Lag", ylab = "Correlation")

# Find the lag with the maximum correlation
min_corr_lag <- which.max(cross_corr2$acf)
min_corr <- cross_corr2$acf[min_corr_lag]
lag2 <- cross_corr2$lag[min_corr_lag]

# Print the lag and the maximum correlation
cat("Lag with mminimum correlation:", lag2* 365, "\n")
cat("Maximum correlation:", min_corr, "\n")
cor.test(residuals_full,lag_temp_min)


lag_temp_max <- stats::lag(temp_max, k = 8)
lag_temp_min <- stats::lag(temp_min, k = 8)

# Ensure equal lengths of series
n <- min(length(admis), length(temp_max), length(temp_min))
admis_trial <- admis[1:n]
lag_temp_max <- lag_temp_max[1:n]
lag_temp_min <- lag_temp_min[1:n]



ArimaxModel_trial=arima(admis_trial, order=c(1,0,0), xreg = cbind(lag_temp_max, lag_temp_min))
stargazer(ArimaxModel_trial , title = " Arimax Model Admissions - temperatures lagged ", out = "C:/Users/manue/Downloads/resid_arimax_trial.docx", type = "text")



























####################Model comparison ###########################################
# Fit the final ARIMAX model
final_admisX <- Arima(train_admis, order = c(1,0,0), xreg = cbind(train_temp_max, train_temp_min))
final_admis_auto <- Arima(train_admis, order = arima_order, xreg = cbind(train_temp_max, train_temp_min))

#foecast
forecast_admisX <- forecast(final_admisX, xreg = cbind(train_temp_max, train_temp_min), h = length(test_admis))
forecast_admis_train <- forecast(final_admis, xreg = cbind(train_temp_max, train_temp_min))
accuracy(forecast_admis, test_admis)
plot(forecast_admisX, ylim = c(0,350), xlim = c(2020.22,2020.43))
lines(test_admis, col = "blue")
#lines(temp_max, col = "#00FF00")
#lines(temp_min, col = "turquoise3")
# Plot the temperature variables on the right y-axis
par(new = TRUE)
plot(temp_max, type = "l", col = "#00FF00", axes = FALSE, xlab = "", ylab = "", ylim = c(min(temp_min), max(temp_max)), yaxt = "n")
axis(side = 4, col = "#00FF00")
mtext("Temperature (°C)", side = 4, line = 3)

par(new = TRUE)
plot(temp_min, type = "l", col = "turquoise3", axes = FALSE, xlab = "", ylab = "", ylim = c(min(temp_min), max(temp_max)), yaxt = "n")
axis(side = 4, col = "turquoise3")
mtext("Temperature (°C)", side = 4, line = 2.5)

legend("topright", legend = c("Forecast", "Test Data", "Temp Max", "Temp Min"),
       col = c("black", "blue", "#00FF00", "turquoise3"), lty = c(1, 1,1,1), cex = 0.8)




forecast_admis_auto <- forecast(final_admis_auto, xreg = cbind(train_temp_max, train_temp_min), h = length(test_admis)/100)
accuracy(forecast_admis_auto, test_admis)
plot(forecast_admis_auto)
lines(test_admis, col = "blue")



































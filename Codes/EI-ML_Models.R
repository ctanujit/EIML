################### Exogenous Variables ###################
library(readxl)
data <- read_excel("real data.xlsx")
datats = ts(data$`Total Cases (Weekly)`)
train = datats[1:60]
test =  datats[61:72]
library(Metrics) 

# Evaluation Matrix
performance = data.frame()
model_summary = function(test, output, model){
  MAPE = mape(as.vector(test),output)*100
  SMAPE = smape(as.vector(test),output)*100
  MAE = mae(as.vector(test),output)
  MASE = mase(as.vector(test),output)
  RMSE = rmse(as.vector(test),output)
  Evaluation = data.frame(Model = model, MAPE = MAPE, SMAPE = SMAPE, MAE = MAE, MASE = MASE, RMSE = RMSE)
  return(Evaluation)
}
n = length(test)
library(forecast)


xreg_df  = read_excel("Updated/SIBR_model_fit.xlsx")
reg_datats <- xreg_df$`Fitted Infected Population`
train_xreg = reg_datats[1:60]
test_xreg <- reg_datats[61:72]

#' ARIMAX
fitARIMAX = auto.arima(train, xreg = train_xreg) 
predARIMAX = forecast::forecast(fitARIMAX,h=n, xreg = test_xreg)
ARIMAX_summary = model_summary(test, predARIMAX$mean, model = "EI-ARIMA")
performance = rbind(performance, ARIMAX_summary)

set.seed(100)
#' ARNNX
fit_ARNNX = nnetar(train, xreg = train_xreg)
predARNNX=forecast::forecast(fit_ARNNX, h= n, xreg = test_xreg)
ARNNX_summary = model_summary(test, predARNNX$mean, model = "EI-ARNN")
performance = rbind(performance, ARNNX_summary)

write.csv(predARIMAX$mean, "Updated/EI-ARIMA_Forecast.csv")
write.csv(predARNNX$mean, "Updated/EI-ARNN_Forecast.csv")
write.csv(fitARIMAX$mean, "Updated/EI-ARIMA_Prediction.csv")
write.csv(fit_ARNNX$fitted, "Updated/EI-ARNN_Predicction.csv")

library(ggplot2)

Time_tr = 1:60
Time_ts = 61 : 72
dat_tr = data.frame("Train" = train, "Fitted" = round(fit_ARNNX$fitted), Time_tr, "SIBR" = train_xreg)
dat_ts = data.frame("Test" = test, "Forecast" = round(predARNNX$mean), Time_ts, "SIBR" = test_xreg)
new_plot = ggplot() +    # Create default ggplot2 line plot
  geom_line(aes(x = dat_tr$Time_tr, y = dat_tr$Train), linewidth = 1.2, color = 'blue')+
  geom_line(aes(x = dat_tr$Time_tr, y = dat_tr$Fitted), linewidth = 1, color = 'lavender')+
  geom_point(aes(x = dat_tr$Time_tr, y = dat_tr$Fitted), size = 2.5, color = 'coral')+
  geom_line(aes(x = dat_tr$Time_tr, y = dat_tr$SIBR), linewidth = 1.2, color = 'forestgreen')+
  labs(
    title = "", subtitle = "", 
    x = "Time (weeks)", y = "Cases") + theme_bw()+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.y = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=16))
#+
#scale_x_continuous(limits = c(61,72),breaks = seq(61,72, 3))+
#scale_y_continuous(limits = c(0,200),breaks = seq(0,200, 50))

#geom_line(dat_tr, aes(x = Time_tr, y = Train), linewidth = 1.2, color = 'blue')+

ggplot() +
  geom_line(aes(x = dat_ts$Time_ts, y = dat_ts$Forecast), linewidth = 1, color = 'slateblue')+
  geom_point(aes(x = dat_ts$Time_ts, y = dat_ts$Forecast), size = 3, color = 'yellow3')+
  geom_line(aes(x = dat_ts$Time_ts, y = dat_ts$Test), linewidth = 1.2, color = 'red')+
  geom_line(aes(x = dat_ts$Time_ts, y = dat_ts$SIBR), linewidth = 1.2, color = 'forestgreen')+
  scale_x_continuous(limits = c(61,72),breaks = seq(61,72, 2))+
  scale_y_continuous(limits = c(60000,66500),breaks = seq(60000,66500, 3000))+
  labs(
    title = "", subtitle = "", 
    x = "Time (weeks)", y = "Cases", size = 16)+ theme_bw()+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.y = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=16))

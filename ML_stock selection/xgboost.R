# https://blog.quantinsti.com/forecasting-markets-using-extreme-gradient-boosting-xgboost/
# Forecasting Markets using eXtreme Gradient Boosting (XGBoost) ----
install.packages("xgboost")
# Load the relevant libraries
library(quantmod); library(TTR); library(xgboost);

# Read the stock data 
# symbol = "ACC"
# fileName = paste(getwd(),"/",symbol,".csv",sep="") ; 
# df = as.data.frame(read.csv(fileName))
# colnames(df) = c("Date","Time","Close","High", "Low", "Open","Volume")

getSymbols("AMZN", from  = '2008-01-01')
str(AMZN)
df <- AMZN %>% data.frame %>% rownames_to_column()

# Define the technical indicators to build the model 
rsi = RSI(df$Close, n=14, maType="WMA")
adx = data.frame(ADX(df[,c("High","Low","Close")]))
sar = SAR(df[,c("High","Low")], accel = c(0.02, 0.2))
trend = df$Close - sar
# create a lag in the technical indicators to avoid look-ahead bias 
rsi = c(NA,head(rsi,-1)) 
adx$ADX = c(NA,head(adx$ADX,-1)) 
trend = c(NA,head(trend,-1))



























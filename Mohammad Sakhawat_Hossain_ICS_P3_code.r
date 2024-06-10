library(ggplot2)
library(plyr) 
library(ggpubr)
library(car)
library(gridExtra)

# read the file
df <- read.csv(file = 'Bikedata.csv', sep = ",")
df

summary(df)
str(df)


head(df)

#check missing data
apply(df, 2, function(x) any(is.na(x)))




# summary of dataset

t1 <- get_summary_stats(df, show = c("n", "min", "max", "mean", "median", "iqr", "sd")) # to appendix
t1



# Full Model

bike_count <- lm(log.Rented.Bike.Count ~ ., data = df)
summary(bike_count)




#Descriptive analysis

data_f <- df
data_f <- rename(data_f,c('log.Rented.Bike.Count'='LogBC in hour', 'Hour'='Hour', 'Temperature'='Temperature in celsius',  'Humidity'='Humidity in percentage (%)', 'Wind.speed'='Windspeed (m/s)', 'Visibility'='Visibility (10m)', 'Solar.Radiation'=' Solar radiation in MJ/m^2', 'Rainfall'='Rainfall in mm', 'Snowfall'='Snowfall in cm', 'Seasons'='Seasons (Autumn = 1, Spring = summer = winter = 0)', "Holiday" = "Holiday (yes = 1 , no = 0)"))
head (data_f)




# Scatter plot excluding Log.Rented.Bike.Count itself


scatterplots <- lapply(names(data_f)[c(-1, -10, -11)], function(x) {
  ggplot(data_f, aes(y = `LogBC in hour`, x = .data[[x]])) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("LogBC in hour") +
    xlab(x) +
    geom_point() 
})

figure <- ggarrange(
  scatterplots[[1]], scatterplots[[2]], scatterplots[[3]], scatterplots[[4]],
  scatterplots[[5]], scatterplots[[6]], scatterplots[[7]], scatterplots[[8]],
   nrow = 4, ncol = 2
)

figure




# correlation 


cor(df$log.Rented.Bike.Count,df$Hour)
cor(df$log.Rented.Bike.Count,df$Temperature)
cor(df$log.Rented.Bike.Count,df$Humidity)
cor(df$log.Rented.Bike.Count,df$Wind.speed)
cor(df$log.Rented.Bike.Count,df$Visibility)
cor(df$log.Rented.Bike.Count,df$Solar.Radiation)
cor(df$log.Rented.Bike.Count,df$Rainfall)
cor(df$log.Rented.Bike.Count,df$Snowfall)





# Best Subset Selection for all the subsets of the predicates

data_lm <- df
data_lm$log.Rented.Bike.Count <- NULL

mod_headers <- names(data_lm[1:ncol(data_lm)])
models <- list()

  
  
k <- 1
for (i in 1:length(mod_headers)) {
  tab <- combn(mod_headers, i)
  tab
  for(j in 1:ncol(tab)) {
    mod_tab_new <- c(tab[, j], "log.Rented.Bike.Count")
    models[[k]]  <- lm(log.Rented.Bike.Count ~ ., data = df[mod_tab_new])
    k <- k + 1
  }
}

# Best model by AIC
lm_AIC <- models[[which.min(sapply(models, AIC))]] # hour, temperature, humidity, wind speed, rainfall, season spring, season summer, season winter, no holiday
summary(lm_AIC )
AIC(models[[which.min(sapply(models, AIC))]]) # 6521.45





#qq AIC

res <- residuals(lm_AIC)
res
qqplot <- ggplot(data = lm_AIC, aes(sample = lm_AIC$residuals)) + 
  geom_qq(color = "black") +
  geom_qq_line(color = "blue") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20), title = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5)) + 
  ylab("Residuals") + xlab("Theoratical Quantiles") + ggtitle("Q-Q Plot (AIC model)")

qqplot

#residual vs fitted values

residual_plot <- ggplot(df, aes(y = lm_AIC$res, x = lm_AIC$fitted.values)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20), title = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5)) +
  geom_point() + ylab("Residuals") + xlab("Fitted values") + ggtitle("Residual vs Fitted Plot (AIC)")+
  
  geom_abline(intercept = 0, slope = 0, col="blue")

residual_plot

combined_plot <- grid.arrange(qqplot, residual_plot, nrow = 2)
combined_plot


# Variance inflation factor (VIF)

vif(lm_AIC)


# confidence intervals

conf = confint(lm_AIC, level=0.95)
conf

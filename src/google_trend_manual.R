library(plotly)
setwd('/home/enrico/Dropbox/Tesi SSST/Modelling the geographical diffusion of IT trends/src')
#####################
### PREPROCESSING ###
#####################

keyword <- "Big Data"

start_date <- 213
start_date_to_int <- start_date
path <- paste("../Data/",keyword,sep="")


#import data from csv file
braz_bigdata <- read.csv(file = paste(path,"/brazil.csv",sep=""), head = TRUE,
                         row.names=NULL, stringsAsFactors = F)

#remove first and last lines, dfo not contain useful info
braz_bigdata <- braz_bigdata[start_date:629,] #start from 2008

#drop the old column
braz_bigdata$row.names <- NULL
#turn the numbers into numeric
braz_bigdata$Interest <- as.numeric(braz_bigdata[,1])
#drop the old column
braz_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
braz_bigdata <- braz_bigdata[-c(120,440),]
#turn into a time series object
braz_bigdata.ts <- ts(braz_bigdata, start = start_date_to_int, frequency = 52)

braz_dec <- decompose(braz_bigdata.ts)
braz_loess <- stl(braz_bigdata.ts, "periodic")
braz_loess_series <- braz_loess$time.series
braz_loess_trend <- braz_loess_series[,2]

#import data from csv file
italy_bigdata <- read.csv(file = paste(path,"/italy.csv",sep=""), head = TRUE,
                          row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
italy_bigdata <- italy_bigdata[start_date:629,] #start from 2008
#turn the first column into a date
#drop the old column
italy_bigdata$row.names <- NULL
#turn the numbers into numeric
italy_bigdata$Interest <- as.numeric(italy_bigdata[,1])
#drop the old column
italy_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
italy_bigdata <- italy_bigdata[-c(120,440),]
#turn into a time series object
italy_bigdata.ts <- ts(italy_bigdata, start = start_date_to_int, frequency = 52)
italy_dec <- decompose(italy_bigdata.ts)
#plot(italy_dec$trend)

#import data from csv file
france_bigdata <- read.csv(file = paste(path,"/france.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
france_bigdata <- france_bigdata[start_date:629,] #start from 2008
#turn the first column into a date
#drop the old column
france_bigdata$row.names <- NULL
#turn the numbers into numeric
france_bigdata$Interest <- as.numeric(france_bigdata[,1])
#drop the old column
france_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
france_bigdata <- france_bigdata[-c(120,440),]
#turn into a time series object
france_bigdata.ts <- ts(france_bigdata, start = start_date_to_int, frequency = 52)
france_dec <- decompose(france_bigdata.ts)

#import data from csv file
india_bigdata <- read.csv(file = paste(path,"/india.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
india_bigdata <- india_bigdata[start_date:629,] #start from 2008
#turn the first column into a date
#braz_bigdata$Date <- as.Date(braz_bigdata$row.names)
#drop the old column
india_bigdata$row.names <- NULL
#turn the numbers into numeric
india_bigdata$Interest <- as.numeric(india_bigdata[,1])
#drop the old column
india_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
india_bigdata <- india_bigdata[-c(120,440),]
#turn into a time series object
india_bigdata.ts <- ts(india_bigdata, start = start_date_to_int, frequency = 52)
india_dec <- decompose(india_bigdata.ts)

#import data from csv file
germany_bigdata <- read.csv(file = paste(path,"/germany.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
germany_bigdata <- germany_bigdata[start_date:629,] #start from 2008
#drop the old column
germany_bigdata$row.names <- NULL
#turn the numbers into numeric
germany_bigdata$Interest <- as.numeric(germany_bigdata[,1])
#drop the old column
germany_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
germany_bigdata <- germany_bigdata[-c(120,440),]
#turn into a time series object
germany_bigdata.ts <- ts(germany_bigdata, start = start_date_to_int, frequency = 52)
germany_dec <- decompose(germany_bigdata.ts)

#import data from csv file
uk_bigdata <- read.csv(file = paste(path,"/uk.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
uk_bigdata <- uk_bigdata[start_date:629,] #start from 2008
#drop the old column
uk_bigdata$row.names <- NULL
#turn the numbers into numeric
uk_bigdata$Interest <- as.numeric(uk_bigdata[,1])
#drop the old column
uk_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
uk_bigdata <- uk_bigdata[-c(120,440),]
#turn into a time series object 
uk_bigdata.ts <- ts(uk_bigdata, start = start_date_to_int, frequency = 52)
uk_dec <- decompose(uk_bigdata.ts)

#import data from csv file
us_bigdata <- read.csv(file = paste(path,"/us.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
us_bigdata <- us_bigdata[start_date:629,] #start from 2008
#drop the old column
us_bigdata$row.names <- NULL
#turn the numbers into numeric
us_bigdata$Interest <- as.numeric(us_bigdata[,1])
#drop the old column
us_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
us_bigdata <- us_bigdata[-c(120,440),]
#turn into a time series object
us_bigdata.ts <- ts(us_bigdata, start = start_date_to_int, frequency = 52)
us_dec <- decompose(us_bigdata.ts)

japan_bigdata <- read.csv(file = paste(path,"/japan.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
japan_bigdata <- japan_bigdata[start_date:629,] #start from 2008
#drop the old column
japan_bigdata$row.names <- NULL
#turn the numbers into numeric
japan_bigdata$Interest <- as.numeric(japan_bigdata[,1])
#drop the old column
japan_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
japan_bigdata <- japan_bigdata[-c(120,440),]
#turn into a time series object
japan_bigdata.ts <- ts(japan_bigdata, start = start_date_to_int, frequency = 52)
japan_dec <- decompose(japan_bigdata.ts)

canada_bigdata <- read.csv(file = paste(path,"/canada.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
canada_bigdata <- canada_bigdata[start_date:629,] #start from 2008
#drop the old column
canada_bigdata$row.names <- NULL
#turn the numbers into numeric
canada_bigdata$Interest <- as.numeric(canada_bigdata[,1])
#drop the old column
canada_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
canada_bigdata <- canada_bigdata[-c(120,440),]
#turn into a time series object
canada_bigdata.ts <- ts(canada_bigdata, start = start_date_to_int, frequency = 52)
canada_dec <- decompose(canada_bigdata.ts)

china_bigdata <- read.csv(file = paste(path,"/china.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
china_bigdata <- china_bigdata[start_date:629,] #start from 2008
#drop the old column
china_bigdata$row.names <- NULL
#turn the numbers into numeric
china_bigdata$Interest <- as.numeric(china_bigdata[,1])
#drop the old column
china_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
china_bigdata <- china_bigdata[-c(120,440),]
#turn into a time series object
china_bigdata.ts <- ts(china_bigdata, start = start_date_to_int, frequency = 52)
china_dec <- decompose(china_bigdata.ts)

russia_bigdata <- read.csv(file = paste(path,"/russia.csv",sep=""), head = TRUE, row.names=NULL, stringsAsFactors = F)
#remove first and last lines, do not contain useful info
russia_bigdata <- russia_bigdata[start_date:629,] #start from 2008
#drop the old column
russia_bigdata$row.names <- NULL
#turn the numbers into numeric
russia_bigdata$Interest <- as.numeric(russia_bigdata[,1])
#drop the old column
russia_bigdata[,1] <- NULL
#drop one row in order to have exactly 52 weeks per year
russia_bigdata <- russia_bigdata[-c(120,440),]
#turn into a time series object
russia_bigdata.ts <- ts(russia_bigdata, start = start_date_to_int, frequency = 52)
russia_dec <- decompose(russia_bigdata.ts)

#dump the plot to pdf
#plot_name <- paste("plot_",keyword,args[2],".pdf",sep="")
#pdf(plot_name)

#layout(1,width = 2, height = 1)
#plot(us_dec$trend,lwd = 2, ylim = c(0,100), ylab = keyword)
#lines(braz_dec$trend, col = 'red',lwd = 2)
#lines(uk_dec$trend, col = 'blue',lwd = 2)
#lines(italy_dec$trend, col = 'green',lwd = 2)
#lines(france_dec$trend, col = 'purple',lwd = 2)
#lines(germany_dec$trend, col = 'yellow', lwd = 2)
#lines(india_dec$trend, col = 'grey', lwd = 2)
#lines(china_dec$trend, col = 'orange', lwd = 2)
#lines(canada_dec$trend, col = rgb(0,1,1,0.5), lwd = 2)
#lines(japan_dec$trend,  col =rgb(1,0,1,0.3),lwd = 2)
#lines(russia_dec$trend,  col =rgb(1,1,0,0.3),lwd = 2)

#change the name of the trends for later

#cut away first and last 26 data, which are NAN
data_length <- length(us_dec$trend) -26

us_trend <- us_dec$trend[27:data_length]
uk_trend <- uk_dec$trend[27:data_length]
ge_trend <- germany_dec$trend[27:data_length]
it_trend <- italy_dec$trend[27:data_length]
fr_trend <- france_dec$trend[27:data_length]
br_trend <- braz_dec$trend[27:data_length]
in_trend <- india_dec$trend[27:data_length]
ca_trend <- canada_dec$trend[27:data_length]
ch_trend <- china_dec$trend[27:data_length]
ja_trend <- japan_dec$trend[27:data_length]
ru_trend <- russia_dec$trend[27:data_length]

############
### PLOT ###
############

a <-read.csv(file = paste(path,"/china.csv",sep=""), head = TRUE, row.names=NULL)

dates <- a$row.names[start_date:629]

start_date_Date <- as.Date("06-01-2008","%d-%m-%Y")

x_iter <- 0:(length(dates)-1)

x_labels <- start_date_Date + 7*x_iter

#the first and last 26 values are Nan
last_date <- length(x_labels) - 26
x_labels <- x_labels[27:last_date]

f <- list(
  family = "Courier New, monospace",
  size = 25,
  color = "Black"
)


f1 <- list(
  family = "Courier New, monospace",
  size = 15,
  color = "Black"
)

x <- list(
  title = "Date",
  titlefont = f,
  tickfont =f1
  #ticktext=list(x_labels)
)
y <- list(
  title = "Interest",
  titlefont = f,
  tickfont =f1
)

leg <- list(
  font = f1
)
p<- plot_ly(x = x_labels, y = ca_trend, name = "CAN", line = list(width = 3)) %>%
  layout(xaxis = x, yaxis = y, legend = leg) %>%
  add_trace(x = x_labels, y = us_trend, name = "USA", line = list(width = 3))%>%
  add_trace(x = x_labels, y = uk_trend, name = "GBR", line = list(width = 3))%>%
  add_trace(x = x_labels, y = it_trend, name = "ITA", line = list(width = 3))%>%
  add_trace(x = x_labels, y = fr_trend, name = "FRA", line = list(width = 3))%>%
  add_trace(x = x_labels, y = ge_trend, name = "DEU", line = list(width = 3))%>%
  add_trace(x = x_labels, y = ja_trend, name = "JPN", line = list(width = 3))%>%
  add_trace(x = x_labels, y = ch_trend, name = "CHN", line = list(width = 3))%>%
  add_trace(x = x_labels, y = br_trend, name = "BRA", line = list(width = 3))%>%
  add_trace(x = x_labels, y = in_trend, name = "IND", line = list(width = 3))%>%
  add_trace(x = x_labels, y = ru_trend, name = "RUS", line = list(width = 3))
p
Sys.setenv("plotly_username" = "enrico_palumbo")
Sys.setenv("plotly_api_key" = "hp4ptpwpdx")

plotly_IMAGE(p, format = "png", width = 1800, height = 1000, out_file = "plot.png")
p
###########
### FIT ###
###########

times <- 1:length(us_trend)

Data_us <- data.frame(Times= times, us_trend = us_trend)

fit_us <- nls(us_trend ~ SSlogis(Times, Asym, xmid, scal), Data_us)

Data_it <- data.frame(Times= times, it_trend = it_trend)

fit_it <- nls(it_trend ~ SSlogis(Times, Asym, xmid, scal),Data_it)

Data_uk <- data.frame(Times= times, uk_trend = uk_trend)

fit_uk <- nls(uk_trend ~ SSlogis(Times, Asym, xmid, scal), Data_uk)

Data_br <- data.frame(Times= times, br_trend = br_trend)

fit_br <- nls(br_trend ~ SSlogis(Times, Asym, xmid, scal), Data_br)

Data_fr <- data.frame(Times= times, fr_trend = fr_trend)

fit_fr <- nls(fr_trend ~ SSlogis(Times, Asym, xmid, scal), Data_fr)

Data_ge <- data.frame(Times= times, ge_trend = ge_trend)

fit_ge <- nls(ge_trend ~ SSlogis(Times, Asym, xmid, scal), Data_ge)

Data_in <- data.frame(Times= times, in_trend = in_trend)

fit_in <- nls(in_trend ~ SSlogis(Times, Asym, xmid, scal), Data_in)

Data_ja <- data.frame(Times= times, ja_trend = ja_trend)

fit_ja <- nls(ja_trend ~ SSlogis(Times, Asym, xmid, scal), Data_ja)

Data_ch <- data.frame(Times= times, ch_trend = ch_trend)

fit_ch <- nls(ch_trend ~ SSlogis(Times, Asym, xmid, scal), Data_ch)

Data_ca <- data.frame(Times= times, ca_trend = ca_trend)

fit_ca <- nls(ca_trend ~ SSlogis(Times, Asym, xmid, scal), Data_ca)

#no RUSSIA, it doesn't fit with the model

xmid_us <- coef(fit_us)[2] #value of x0
xmid_uk <- coef(fit_uk)[2]
xmid_br <- coef(fit_br)[2]
xmid_in <- coef(fit_in)[2]
xmid_ge <- coef(fit_ge)[2]
xmid_it <- coef(fit_it)[2]
xmid_fr <- coef(fit_fr)[2]
xmid_ch <- coef(fit_ch)[2]
xmid_ca <- coef(fit_ca)[2]
xmid_ja <- coef(fit_ja)[2]
#xmid_ru <- coef(fit_ru)[2]

xmid_us_err <- summary(fit_us)$coefficients['xmid','Std. Error'] #error on the parameter
xmid_uk_err <- summary(fit_uk)$coefficients['xmid','Std. Error']
xmid_br_err <- summary(fit_br)$coefficients['xmid','Std. Error']
xmid_in_err <- summary(fit_in)$coefficients['xmid','Std. Error']
xmid_ge_err <- summary(fit_ge)$coefficients['xmid','Std. Error']
xmid_it_err <- summary(fit_it)$coefficients['xmid','Std. Error']
xmid_fr_err <- summary(fit_fr)$coefficients['xmid','Std. Error']
xmid_ja_err <- summary(fit_ja)$coefficients['xmid','Std. Error']
xmid_ch_err <- summary(fit_ch)$coefficients['xmid','Std. Error']
xmid_ca_err <- summary(fit_ca)$coefficients['xmid','Std. Error']

mid_times <- data.frame(time_us = c(xmid_us,xmid_us_err), time_uk = c(xmid_uk,xmid_uk_err), 
                        time_it = c(xmid_it,xmid_it_err), 
                        time_ge = c(xmid_ge,xmid_ge_err), time_in = c(xmid_in,xmid_in_err),
                        time_br = c(xmid_br,xmid_br_err), 
                        time_fr = c(xmid_fr,xmid_fr_err),
                        time_ca = c(xmid_ca,xmid_ca_err),
                        time_ja = c(xmid_ja,xmid_ja_err),
                        time_ch = c(xmid_ch,xmid_ch_err))

#now we compute the mid_times with respect to japan that is the first

square_error <- function(x,y)return(sqrt(x**2+y**2)) 

mid_times_relative <- data.frame(time_us = c(xmid_us-xmid_ja,square_error(xmid_us_err,xmid_ja_err)),
                                 time_uk = c(xmid_uk-xmid_ja,square_error(xmid_uk_err,xmid_ja_err)), 
                                 time_it = c(xmid_it-xmid_ja,square_error(xmid_it_err,xmid_ja_err)), 
                                 time_ge = c(xmid_ge-xmid_ja,square_error(xmid_ge_err,xmid_ja_err)), 
                                 time_in = c(xmid_in-xmid_ja,square_error(xmid_in_err,xmid_ja_err)),
                                 time_br = c(xmid_br-xmid_ja,square_error(xmid_br_err,xmid_ja_err)), 
                                 time_fr = c(xmid_fr-xmid_ja,square_error(xmid_fr_err,xmid_ja_err)),
                                 time_ca = c(xmid_ca-xmid_ja,square_error(xmid_ca_err,xmid_ja_err)),
                                 time_ja = c(xmid_ja-xmid_ja,0),
                                 time_ch = c(xmid_ch-xmid_ja,square_error(xmid_ch_err,xmid_ja_err)))

write.csv(mid_times_relative,"onset_times_relative.csv")
names <- c('time_us','time_uk','time_br','time_in', 'time_ge','time_it','time_fr','time_ca','time_ja','time_ch') 

delays <-data.frame()

for (i in names){
  
  value_i <- mid_times[1,][i] #427
  #compute the difference list    
  row <- c()
  
  for (j in names){   
    value_j <- mid_times[1,][j] #437
    #print (abs(value_j - value_i))
    row <- c(row, (-value_j + value_i))    
  }
  delays <- rbind(delays, row) 
}

write.csv(delays,paste("delays_",keyword,".csv",sep=""), row.names = FALSE)

errors <- data.frame()

for (i in names){
  
  err_i <- mid_times[2,][i] #0.2
  
  #compute the difference list    
  row <- c()
  
  for (j in names){   
    err_j <- mid_times[2,][j] #437
    row <- c(row, sqrt(err_j**2 + err_i**2))    
  }
  
  errors <- rbind(errors, row ) 
}

write.csv(errors, paste("errors_",keyword,".csv",sep=""), row.names = FALSE)

short_names <- c("USA","GBR","BRA","IND","DEU","ITA","FRA","CAN","JPN","CHN")

f_ticks <- list(
  family = "Courier New, monospace",
  size = 35,
  color = "Black"
)

x_delays <- list(
  title = "",
  ticktext=short_names,
  tickfont = f_ticks,
  ticks = '')
y_delays <- list(
  title = "",
  ticktext=short_names,
  tickfont = f_ticks,
  ticks = ''
)

leg_2 <- list(
  font = f_ticks
)

#################
### HEATMAP #####
#################

delay <- as.matrix(delays)
short_names_rev = rev(short_names)
heatmap <- plot_ly(z = delay,
        x = short_names, y = short_names,
        colorscale = blue2red(100),
        type = "heatmap", colorbar = list(title = "Delay", tickfont = f_ticks, titlefont = f_ticks))%>%layout(xaxis = x_delays, yaxis = y_delays)
heatmap
plotly_IMAGE(heatmap, format = "png", width = 1800, height = 1800, out_file = "heatmap.png")

#############
### MAP #####
#############

# light grey boundaries
l <- list(color = toRGB("white"), width = 0.5)

g <- list(
  showframe = FALSE)

mid_times_new <- data.frame(times = c(xmid_us-xmid_ja,xmid_uk-xmid_ja,xmid_br-xmid_ja,xmid_in-xmid_ja,xmid_ge-xmid_ja,xmid_it-xmid_ja,xmid_fr-xmid_ja,xmid_ca-xmid_ja,xmid_ja-xmid_ja,xmid_ch-xmid_ja), names = short_names)

mappa <- plot_ly(mid_times_new, z = mid_times_new$times, locations = mid_times_new$names, type = 'choropleth',
        color =mid_times_new$times, colors = 'Reds',marker = list(line = l),
        colorbar = list(ticksuffix = ' weeks ', title = 'Delay w.r.t. Japan',tickfont = f_ticks, titlefont = f_ticks)) %>%
  layout(title = 'Interest in Big Data',xaxis = x, geo = g, titlefont = f_ticks)
Sys.setenv("plotly_username" = "enrico_palumbo")
Sys.setenv("plotly_api_key" = "hp4ptpwpdx")

plotly_IMAGE(mappa, format = "png", width = 1800, height = 1000, out_file = "map.png")

#########################
## effect of smoothing ##
#########################

f <- list(
  family = "Courier New, monospace",
  size = 35,
  color = "Black"
)

x <- list(
  title = "Date",
  titlefont = f,
  tickfont = f1
)
y <- list(
  title = "Interest Big Data",
  titlefont = f,
  tickfont = f1
)
leg_3 <- list(
  font = f,
  x = 0.8,
  y = 0.6
)

smooth <-plot_ly(x = x_labels, y = italy_dec$x[27:length(italy_dec$x)], name = "Italy Data",line = list(width = 5)) %>%
  layout(xaxis = x, yaxis = y, legend = leg_3) %>%
  add_trace(x = x_labels, y = it_trend, name = "Italy Trend",line = list(width = 5))
smooth

Sys.setenv("plotly_username" = "enrico_palumbo")
Sys.setenv("plotly_api_key" = "hp4ptpwpdx")

plotly_IMAGE(smooth, format = "png", width = 1800, height = 1000, out_file = "smoothing_effect.png")

###################
## fitting_plot ###
###################

asym_br <- coef(fit_br)[1]
xmid_br <- coef(fit_br)[2]
scal_br <- coef(fit_br)[3]

logistic_br <- function(x)return (asym_br / (1+ exp(-(x-xmid_br)/scal_br)))

fit <- plot_ly(x = x_labels, y = logistic_br(x_iter), name = "Brazil Fit",line = list(width = 5)) %>%
  layout(xaxis = x, yaxis = y,legend = leg_3) %>%
  add_trace(x = x_labels, y = br_trend, name = "Brazil Trend", line = list(width = 5))

plotly_IMAGE(fit, format = "png", width = 1800, height = 1000, out_file = "brazil_fit.png")

################
## clustering ##
################

data_cluster <- matrix(rbind(us_trend,uk_trend,br_trend,in_trend, ge_trend, it_trend, fr_trend,ca_trend, ja_trend, ch_trend),nrow = 10)
#clusters <- list()

#for(i in seq(1,1000)){
#clusters <- c(clusters,list(kmeans(data_cluster, centers = 3)$cluster))
#}
#clusters

kmeans(data_cluster, centers = data_cluster[c(1,6,10),], iter.max = 1000)

#plot coloring the clusters
prot <- plot_ly(x = x_labels, y = ca_trend, name = "CAN", line = list(color = "Blue", width = 5)) %>%
  layout(xaxis = x, yaxis = y, legend = leg) %>%
  add_trace(x = x_labels, y = us_trend, name = "USA", line = list(color = "Blue", width = 5))%>%
  add_trace(x = x_labels, y = uk_trend, name = "GBR", line = list(color = "Blue", width = 5))%>%
  add_trace(x = x_labels, y = it_trend, name = "ITA", line = list(color = "Red", width = 5))%>%
  add_trace(x = x_labels, y = fr_trend, name = "FRA", line = list(color = "Red", width = 5))%>%
  add_trace(x = x_labels, y = ge_trend, name = "DEU", line = list(color = "Blue", width = 5))%>%
  add_trace(x = x_labels, y = ja_trend, name = "JPN", line = list(color = "Red", width = 5))%>%
  add_trace(x = x_labels, y = ch_trend, name = "CHN", line = list(color = "Black", width = 5))%>%
  add_trace(x = x_labels, y = br_trend, name = "BRA", line = list(color = "Red", width = 5))%>%
  add_trace(x = x_labels, y = in_trend, name = "IND", line = list(color = "Blue", width = 5))
  #add_trace(x = x_labels, y = ru_trend, name = "RUS")
Sys.setenv("plotly_username" = "enrico_palumbo")
Sys.setenv("plotly_api_key" = "hp4ptpwpdx")

plotly_IMAGE(prot, format = "png", width = 1800, height = 1000, out_file = "plot_cluster.png")

###########
## peaks ##
###########

max_us_date_index <- which.max(us_trend)
max_uk_date_index <- which.max(uk_trend)
max_br_date_index <- which.max(br_trend)
max_in_date_index <- which.max(in_trend)
max_ge_date_index <- which.max(ge_trend)
max_it_date_index <- which.max(it_trend)
max_fr_date_index <- which.max(fr_trend)
max_ca_date_index <- which.max(ca_trend)
max_ja_date_index <- which.max(ja_trend)
max_ch_date_index <- which.max(ch_trend)

#index_lists <- c(max_us_date_index,max_uk_date_index,max_br_date_index, max_in_date_index, max_ge_date_index, max_it_date_index, max_fr_date_index, max_ca_date_index, max_ja_date_index,max_ch_date_index)
index_frame <- data.frame( us = max_us_date_index, uk = max_uk_date_index, br = max_br_date_index, ind = max_in_date_index, ge = max_ge_date_index, it = max_it_date_index, fr = max_fr_date_index, ca = max_ca_date_index, ja = max_ja_date_index, ch = max_ch_date_index)

peak_delay_japan <- index_frame - index_frame$ja

max_us_date <- x_labels[which.max(us_trend)]
max_uk_date <- x_labels[which.max(uk_trend)]
max_br_date <- x_labels[which.max(br_trend)]
max_in_date <- x_labels[which.max(in_trend)]
max_ge_date <- x_labels[which.max(ge_trend)]
max_it_date <- x_labels[which.max(it_trend)]
max_fr_date <- x_labels[which.max(fr_trend)]
max_ca_date <- x_labels[which.max(ca_trend)]
max_ja_date <- x_labels[which.max(ja_trend)]
max_ch_date <- x_labels[which.max(ch_trend)]

max_us_date
max_fr_date

#################
## renorm plot ##
#################

max_uk <- max(uk_trend)
max_us <- max(us_trend)
max_it <- max(it_trend)
max_fr <- max(fr_trend)
max_ge <- max(ge_trend)
max_br <- max(br_trend)
max_in <- max(in_trend)
max_ja <- max(ja_trend)
max_ca <- max(ca_trend)
max_ch <- max(ch_trend)

uk_trend_norm <- 100*uk_trend/max_uk
us_trend_norm <- 100*us_trend/max_us
br_trend_norm <- 100*br_trend/max_br
it_trend_norm <- 100*it_trend/max_it
ge_trend_norm <- 100*ge_trend/max_ge
fr_trend_norm <- 100*fr_trend/max_fr
ja_trend_norm <- 100*ja_trend/max_ja
ca_trend_norm <- 100*ca_trend/max_ca
uk_trend_norm <- 100*uk_trend/max_uk
us_trend_norm <- 100*us_trend/max_us
ch_trend_norm <- 100*ch_trend/max_ch
in_trend_norm <- 100*in_trend/max_in

plot_ly(x = x_labels, y = ca_trend_norm, name = "CAN") %>%
  layout(xaxis = x, yaxis = y, legend = leg) %>%
  add_trace(x = x_labels, y = us_trend_norm, name = "USA")%>%
  add_trace(x = x_labels, y = uk_trend_norm, name = "GBR")%>%
  add_trace(x = x_labels, y = it_trend_norm, name = "ITA")%>%
  add_trace(x = x_labels, y = fr_trend_norm, name = "FRA")%>%
  add_trace(x = x_labels, y = ge_trend_norm, name = "DEU")%>%
  add_trace(x = x_labels, y = ja_trend_norm, name = "JPN")%>%
  add_trace(x = x_labels, y = ch_trend_norm, name = "CHN")%>%
  add_trace(x = x_labels, y = br_trend_norm, name = "BRA")%>%
  add_trace(x = x_labels, y = in_trend_norm, name = "IND")
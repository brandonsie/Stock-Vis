
libcallStocks <- function(){
	library(stocks)
	library(quantmod)
	library(robinhoodr)
  
  # library(QuantTools)
	
	library(tidyquant)
	library(ggplot2)
	library(plotly)
	
	library(bdscale) #ignore weekends holidays
	
	library(data.table) #fread, fwrite
	library(stats) #fisher.test
	library(tidyr) # >%>
	library(magrittr) #%<>%
	library(kableExtra)
	library(stringr) #str_count
	library(tools) #texi2pdf
	library(readr) #parse_number
	
	library(lubridate)
	options(stringsAsFactors = FALSE)
}

getpct <- function(values, window = 20, method = "bbands"){
  # calculate percentile (0 to 1) of value within last 60 intervals
  if(length(values) < window) stop("Error: getpct: length(v) < w.")

  output <- data.frame(matrix(nrow = length(values), ncol = 3))
  names(output) <- c("window_upper", "window_lower", "window_pct")
  
  if(method == "absolute"){
    
    for(i in window:length(values)){
      window_values <- values[(i-window+1):i]
      output$window_upper[i] <- max(window_values)
      output$window_lower[i] <- min(window_values)
      output$window_pct[i] <- (values[i] - min(window_values)) /
        (max(window_values) - min(window_values))
    }
    
  } else if(method == "bbands"){
    bb <- TTR::BBands(values, window, maType = "EMA") %>% data.frame
    output$window_upper <- bb$up
    output$window_lower <- bb$dn
    output$window_pct <- bb$pct
    output$window_mavg <- bb$mavg
  }
  
  return(output)


}

getema <- function(values, window){
  TTR::EMA(values, window)
}
  
getcrossover <- function(diff){
  #takes difference vector
  #checks if difference has changed sign since last interval
  prevdiff <- c(NA, diff[1:(length(diff)-1)])
  crossover <- diff * prevdiff < 0
  direction <- ifelse(diff - prevdiff > 0 , "Up", "Dn")
  
  data.frame(prevdiff = prevdiff,
             crossover = crossover,
             direction = direction)
}

getfdata <- function(sym, period){
	fdata.xts <- quantmod::getSymbols.yahoo(
	  Symbols = sym, auto.assign = FALSE, periodicity = period)
	fdata <- data.frame(Date = index(fdata.xts), coredata(fdata.xts)) %>% 
		setnames(c("Date","Open","High","Low","Close","Volume","Adjusted"))
	
	fdata$Close_Dir <- getcrossover(fdata$Close)$direction
	fdata$Vol_Dir <- getcrossover(fdata$Volume)$direction
	
	return(fdata)
}

addbbands <- function(fdata){
	bb <- BBands(fdata[,c("High","Low","Close")], maType = "EMA") %>% data.frame
  colnames(bb) <- c("BB_Lower", "BB_Mavg", "BB_Upper", "BB_Pct")
  fdata <- cbind(fdata, bb)
  
  fdata$BB_Diff <- fdata$Close - fdata$BB_Mavg
  cross <- getcrossover(fdata$BB_Diff)
  fdata$BB_Cross <- cross$crossover
  fdata$BB_Dir <- cross$direction
  
  return(fdata)
}


addmacd <- function(fdata){
  macd <- MACD(fdata$Close) %>% data.frame
  fdata$MACD_Value <- macd$macd
  fdata$MACD_Signal <- macd$signal
  fdata$MACD_Diff <- macd$macd - macd$signal
  
  cross <- getcrossover(fdata$MACD_Diff)
  fdata$MACD_Cross <- cross$crossover
  fdata$MACD_Dir <- cross$direction
  
  return(fdata)
  
	# ta.macd$difference <- ta.macd$macd - ta.macd$signal
	# ta.macd$prevdiff <- c(NA,ta.macd$difference[1:(length(ta.macd$difference)-1)])
	# ta.macd$crossMACD <- ta.macd$difference * ta.macd$prevdiff < 0
	# ta.macd$macd_dir <- ifelse(ta.macd$difference - ta.macd$prevdiff > 0, "Up","Down")
	
	# 
	# colnames(ta.macd) <- c("MACD_Value", "MACD_Signal","MACD_Diff",
	#                        "MACD_PrevDiff", "MACD_Cross", "MACD_Dir")
	
	
	# cbind(fdata, ta.macd)
}

addsymbol <- function(fdata, sym){
  fdata$Symbol = sym
  return(fdata)
}

addema <- function(fdata){
  
  fdata$EMA_50 <- getema(fdata$Close, 50)
  fdata$EMA_50_Diff <- fdata$Close - fdata$EMA_50
  cross50 <- getcrossover(fdata$EMA_50_Diff)
  fdata$EMA_50_Cross <- cross50$crossover
  fdata$EMA_50_Dir <- cross50$direction
  
  fdata$EMA_200 <- getema(fdata$Close, 200)
  fdata$EMA_200_Diff <- fdata$Close - fdata$EMA_200
  cross200 <- getcrossover(fdata$EMA_200_Diff)
  fdata$EMA_200_Cross <- cross200$crossover
  fdata$EMA_200_Dir <- cross200$direction
  
  
  return(fdata)
}


#---
getcandlestick <- function(fdata){
	
	for (i in 1:length(fdata[,1])) {
		if (fdata$Close[i] >= fdata$Open[i]) {fdata$direction[i] = 'Increasing'
		} else {fdata$direction[i] = 'Decreasing'}
	}
	return(fdata)
}


taplot <- function(df){
	# -- -- -- -- -- -- -- -- -- --
	# Plot 1: Candlestick
	# -- -- -- -- -- -- -- -- -- --
	
	# colors column for increasing and decreasing
	inc <- list(line = list(color = '#26cf17'))
	dec <- list(line = list(color = '#d34141'))
	
	ay <- list(
		tickfont = list(color = "red"),
		overlaying = "y",
		side = "right",
		title = "second y axis"
	)
	
	# Plot Candlestick with Bollinger Bands and Volume
	
	plotc <- df %>% 
		#Volume (colors only work in plot_ly not add_trace?)
		plot_ly(x = ~Date, y =  ~Volume, type = "bar", 
						color = ~direction, colors = c("#26cf17","#d34141"), alpha = 0.8,
						yaxis = "y2") %>%
		add_trace(inherit = FALSE, data = df, x = ~Date, type="candlestick", 
							open = ~Open, close = ~Close, high = ~High, low = ~Low, 
							name = sym, increasing = inc, decreasing = dec, yaxis = "y1") %>%
		# Add Bollinger Bands
		add_ribbons(inherit = FALSE, data = df, x = ~Date, ymin = ~dn, ymax = ~up,
								line = list(color = 'rgba(7, 164, 181, 0.05)'),
								fillcolor = 'rgba(7, 164, 181, 0.2)',
								name = "Bollinger Bands", yaxis = "y1") %>%
		add_lines(inherit = FALSE, data = df, x = ~Date, y = ~mavg, name = "Bollinger EMA",
							line = list(color = '#5d98f7', width = 1),
							hoverinfo = "none", yaxis = "y1") %>%
		#Layout
		layout(xaxis = list(rangeslider = list(visible = F)),
					 yaxis = list(title = "Price"), yaxis = "y",
					 yaxis2 = list(overlaying = "y", side = "right", title = "Volume")) 
	
	
	
	# -- -- -- -- -- -- -- -- -- --
	# Plot 2: MACD
	# -- -- -- -- -- -- -- -- -- --
	
	# Add MACD subplot
	#
	# #(!) update crossover lines to be fewer, more recent, directional
	# line <- list(type = "line",line = list(color = "blue",width=.5),xref = "x", yref = "y")
	# 
	# macdline <- list()
	# for(i in format(df$Date[df$crossMACD]-1,"%Y-%m-%d")){
	# 	line[["x0"]] <- format(i)
	# 	line[["x1"]] <- format(i)
	# 	line[c("y0","y1")] <- c(-1,1) #c(df$macd %>% min,df$macd %>% max)
	# 	macdline <- c(macdline, list(line))
	# }
	
	
	#two axes
	ym1 <- list(showline = FALSE, side = "left", overlaying = "y1", 
	            title = "Label 1" ,color = "red")
	ym2 <- list(showline = FALSE, side = "right", overlaying = "y3",
	            title = "Label 2",color = "blue")
	
	pmacd <- df %>%
		plot_ly(x=~Date, y=~difference, type = 'bar', yaxis = "y2") %>%
		add_lines(x=~Date, y=~macd, type='scatter', mode='lines', yaxis = "y2") %>%
		add_lines(x=~Date, y=~signal, yaxis = "y2") %>%
		layout(xaxis = list(rangeslider = list(visible = F)),
					 yaxis = ym1, yaxis2 = ym2) 
	
	plotm <- df %>%
		plot_ly(x = ~Date, y = ~difference, type = "bar", yaxis = "y") %>%
		add_lines(x = ~Date, y = ~macd, type = "scatter", mode = "lines", 
							yaxis = "y4") %>%
		add_lines(x = ~Date, y = ~signal, yaxis = "y4") %>%
		layout(yaxis = list(title = "MACD Difference", side = "left"),
					 yaxis4 = list(title = "MACD", side = "right", overlaying = "y3"))
	
	
	# create rangeselector buttons
	rs <- list(visible = TRUE, x = 0.5, y = -0.055,
						 xanchor = 'center', yref = 'paper',
						 font = list(size = 9),
						 buttons = list(
						 	list(count=1, label='RESET', step='all'),
						 	list(count=1, label='1 YR', step='year', stepmode='backward'),
						 	list(count=3, label='3 MO', step='month', stepmode='backward'),
						 	list(count=1, label='1 MO', step='month', stepmode='backward')
						 ))
	
	# subplot with shared x axis
	subplot(plotc, plotm, heights = c(0.5, 0.3), nrows=2,
					shareX = TRUE, titleY = TRUE) %>%
		layout(title = paste("Apple: 2015-02-14 -",Sys.Date()),
					 xaxis = list(rangeselector = rs),
					 legend = list(orientation = 'h', x = 0.5, y = 1,
					 							xanchor = 'center', yref = 'paper',
					 							font = list(size = 10),
					 							bgcolor = 'transparent'))
	
}





# ==============================================================================
# Overview Plot
# Plot grid of closing price graphs for tickers of interest
# ==============================================================================

symbols <- c("SPY","QQQ","IWM","GLD","DIA","MSFT","AAPL",
						 "BRK.B","BRK-B","AMZN","SH","PSQ","SDOW","DXD","ILMN",
						 "NVDA","INTC")


getRHdata <- function(symbols, interval, span){
	data <- rh_historicals(
		symbols = symbols, interval = interval, span = span, 
		bounds = "regular", to_xts = FALSE) %>%	
		rbindlist(use.names = TRUE, fill = TRUE, idcol = "ID")
	data$ID %<>% factor(levels = data$ID %>% unique) #maintain order for plot
	
	#Add percentage
	for(i in symbols) if(i %in% data$ID) {
		data$Percent[data$ID == i] <- (data$close_price[data$ID == i]) / (data$close_price[data$ID == i][1])
	}
	
	return(data)
}

stockFacetPlot <- function(
	data, title, x = "begins_at", y = "close_price", scales = 'free_y', 
	xlab = "Date", ylab = "Close", color = NA){
	g <- ggplot(data, aes(x = eval(as.name(x)), y = eval(as.name(y)))) + geom_line() +
				facet_wrap(.~ID, scales = scales) + 
				xlab(xlab) + ylab(ylab) + ggtitle(title) +
				theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
	
	if(!is.na(color)){
		g %<>% + geom_line(aes(color = eval(as.name(color))))
	}
	return(g)
}


percFacetPlot <- function(
	data, title, x = "begins_at", y = "Percent", scales = 'fixed',
	xlab = "Date", ylab = "Close", color = NA){
	g <- ggplot(data, aes(x = eval(as.name(x)), y = eval(as.name(y)))) + geom_line() +
		facet_wrap(.~ID, scales = scales) + 
		xlab(xlab) + ylab(ylab) + ggtitle(title) +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
	
	if(!is.na(color)){
		g %<>% + geom_line(aes(color = eval(as.name(color))))
	}
	return(g)
}




StockOverview <- function(symbols){
	
	# ----------------------------------------------------------------------------
	# Intraday
	#(!) todo: convert close values to percentages relative to start of window
	#(!) keep price as secondary free y axis
	
	#Gather Intraday Data from RobinhoodR
	intra.data <- getRHdata(symbols, "10minute","week")
	
	#Facet Plot
	intra.facet <- stockFacetPlot(intra.data, "Summary: Intraday")
	intra.facet.perc <- percFacetPlot(intra.data, "Percentage Summary: Intraday")
	
	#Color Plot
	
	
	#(!) convert to percentage
	
	# ----------------------------------------------------------------------------
	# Daily & Weekly
	
	
	#Gather Daily & Weekly Data from RobinhoodR
	daily.data <- getRHdata(symbols,"day","year")
	weekly.data <- getRHdata(symbols,"week","year")
	merge.data <- rbindlist(list(daily.data,weekly.data) %>% 
														setNames(c("Daily","Weekly")),
													use.names = TRUE, fill = TRUE, idcol = "Period")
	
	merge.facet1 <- stockFacetPlot(merge.data, "Year Summary: Daily", color = "Period") + labs(col = "Period")
	merge.facet2 <- stockFacetPlot(merge.data[merge.data$begins_at > (today() - 60)], "Two Month Summary: Daily", color = "Period") + labs(col = "Period")
	
	merge.facet1.perc <- percFacetPlot(merge.data, "Percentage Year Summary: Daily", color = "Period") + labs(col = "Period")
	merge.facet2.perc <- percFacetPlot(merge.data[merge.data$begins_at > (today() - 60)], "Percentage Two Month Summary: Daily", color = "Period") + labs(col = "Period")
	

	
	
	#(!) do single plot with all stocks https://ntguardian.wordpress.com/2018/07/17/stock-data-analysis-python-v2/
	#(!) one percent and one price
	#(!) add zoom in last 60 days for daily
	
	#Load all, append symbol.intraday first
	
	#Intraday
	
	
	#Daily
	
	
	#Weekly
	
	
	#Monthly
	
}


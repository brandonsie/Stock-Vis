
libcallStocks <- function(){
	library(stocks)
	library(quantmod)
	
	library(tidyquant)
	library(ggplot2)
	library(plotly)
	
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


getfdata <- function(sym){
	fdata.xts <- getSymbols(Symbols = sym, src = "yahoo", env=NULL)
	fdata.df <- data.frame(Date = index(fdata.xts), coredata(fdata.xts)) %>% 
		setnames(c("Date","Open","High","Low","Close","Volume","Adjusted"))
	
}

getbbands <- function(fdata){
	BBands(fdata[,c("High","Low","Close")], maType = "EMA") %>% data.frame
}


getmacd <- function(fdata){
	ta.macd <- MACD(fdata$Close) %>% data.frame
	ta.macd$difference <- ta.macd$macd - ta.macd$signal
	ta.macd$prevdiff <- c(NA,ta.macd$difference[1:(length(ta.macd$difference)-1)])
	ta.macd$crossMACD <- ta.macd$difference * ta.macd$prevdiff < 0
	return(ta.macd)
}


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

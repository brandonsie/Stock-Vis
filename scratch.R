
sym <- "GOOG"

# Date, Open, High, Low, Close, Volume, Adjusted
fd_0 <- fd <- getfdata(sym, "daily")
fw_0 <- fw <- getfdata(sym, "weekly")

#reset from here
fd <- fd_0
fw <- fw_0

fd %<>% addbbands() %>% addmacd() %>% addsymbol(sym) %>% addema()
fw %<>% addbbands() %>% addmacd() %>% addsymbol(sym) %>% addema()


collapse_n <- function(vals, n = 5, col = ", "){
  vals %>% tail(n) %>% paste(collapse = col)
}

out_cols <- c("Symbol", "Close", "Close_Dir", "Volume", "Volume_Dir")
output <- data.frame(matrix(ncol = length(out_cols), nrow = 1))
names(output) <- out_cols
output$Symbol <- fd$Symbol %>% tail(1)
output$Close <- fd$Close %>% tail(1)
output$Close_Dir <- fd$Close_Dir %>% collapse_n
output$Volume <- fd$Volume %>% tail(1)
output$Volume_Dir <- fd$Vol_Dir %>% collapse_n

# Current, Directions, Boundary, Crossover

#(!) work on getpct?

# cs <- getcandlestick(fd) #add Increasing Decreasing for candle color
#(!) add back somehow for direction of various indicators

#(!) add rbind for multiple stocks

taplot(df)


#----


window = 100
today <- format(Sys.time(), "%Y-%m-%d") %>% as.character %>% as.Date #today
cutoff <- today - window


df <- tq_get("GOOG")
df[df$date > cutoff,] %>% ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close))

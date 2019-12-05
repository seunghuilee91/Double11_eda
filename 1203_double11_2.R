# The central package is tidyverse, which contains tidyr, dplyr, ggplot, etc. Other packages I am using are tidyquant for its nice ggplot theme, modelr, gridExtra and grid for additional plotting functionalities.
library(margins)
library(tidyverse)
library?tidyquant)
library(modelr)
library(gridExtra)
library(grid)
library(ggplot2)
library(parsedate)
library(readr)
library(RColorBrewer)

# Reading in the data

dataset <- read_csv("ndouble11_test.csv")

dataset <- dataset %>% 
 mutate(UnitPrice = SumPurchases?quantity,
  order_day = wday(dataset$invoice_date, label=T),
  order_hour = hour(dataset$invoice_date),
  order_min = minute(dataset$invoice_date),
  order_return = ifelse(SumPurchases >0, "order", "return"),
  province = country) 

glimpse(dataset)

datas?t[which(is.na(dataset) == T)]<-0 
str(dataset)


# Exploratory Data Analysis (EDA)
# Transactions by country

x1 <- dataset %>%
 ggplot(aes(country))+ geom_bar()+
 scale_fill_manual(values = palette_light()) +
 theme_tq() +
 theme(legend.position = "right"? +
 theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
 labs(x = "",
  fill = "")



# Transactions over time
# To get an idea of the number of transactions over time, we can use a frequency polygon. Here, we can see that the number purc?ases slightly increased during the last two months of recording, while the number of returns remained relatively stable.

x2 <- dataset %>%
 ggplot(aes(x = order_hour)) +
 geom_freqpoly(bins = 100, size = 1, alpha = 0.8) +
 scale_color_manual(values = pale?te_light()) +
 theme_tq() +
 guides(color = FALSE) +
 labs(title = "Number of purchases over time",
  x = "")
x2

# Income/loss from transactions
# Let’s look at the income/loss from transactions over time. Here, we plot the sum of income and losses for e?ch day. The income seems to increase slightly during the last month, while losses remained more stable. The only severe outlier is the last day.

x3 <- dataset %>%
 filter(order_hour==0) %>% 
 group_by(order_min) %>%
 summarise(Total_amount = sum(SumPurcha?es)) %>%
 ggplot(aes(x = order_min, y = Total_amount)) +
 geom_line(size = 1, alpha = 0.8) +
 scale_color_manual(values = palette_light()) +
 theme_tq() +
 guides(color = FALSE) +
 labs(title = "Income from purchases for just one hour",
  x = "11:11 00:00 ? 00:59",
  y = "sum of amount",
  color = "")
x3

# Transactions by day and time

x4 <- dataset %>%
 ggplot(aes(x = order_min, y = order_hour)) +
 stat_bin2d(alpha = 0.8, bins = 25, color = "white") +
 scale_fill_gradientn(colours = c(palette_light()[[1]],?palette_light()[[2]])) +
 theme_tq() +
 theme(legend.position = "right") +
 labs(title = "Purchases per hour and min")

# Items ; Also of interest are the items that are being purchases the most. Here, we sum up the net quantities for each item.
colnames(d?taset)
dataset %>% 
 group_by(stock_code, description) %>% 
 summarise(sum = sum(quantity)) %>% 
 arrange(-sum)

# As we can see in the plots below, the majority of items is purchases only occasionally, while a few items are purchased a lot.
p1 <- dataset ?>%
 group_by(stock_code, description) %>% 
 summarise(sum = sum(quantity)) %>% 
 ggplot(aes(x = sum)) +
 geom_density(fill = palette_light()[[1]], alpha = 0.8) +
 theme_tq()
p1 


p2 <- dataset %>%
 group_by(stock_code, description) %>% 
 summarise(sum = s?m(quantity)) %>% 
 filter(sum>0 & sum<1000) %>%
 ggplot(aes(x = sum)) +
 geom_density(fill = palette_light()[[1]], alpha = 0.8) +
 theme_tq()
p2

# We can also calculate on how many different hours, items have been purchased.
most_sold <- dataset %>%
 grou?_by(order_hour, stock_code, description) %>%
 summarise(sum = sum(quantity)) %>%
 group_by(order_hour,description) %>%
 arrange(order_hour,-sum)
head(most_sold,30)
tail(most_sold,15)

# Let’s look at Yungo & Tech's distribution of sold quantities per day
?5 <- 
dataset %>%
 filter(stock_code == 547073000000) %>%
 group_by(order_min) %>%
 summarise(sum = sum(quantity)) %>%
 ggplot(aes(x = order_min, y = sum)) +
 geom_line(size = 1, alpha = 0.1) +
 scale_color_manual(values = palette_light()) +
 theme_tq() +
?labs(x = "",
  y = "sum of quantities",
  color = "",
  title = "Transactions of Tech 1300ml")
x5

x6 <- 
 dataset %>%
 filter(stock_code == 596291000000) %>%
 group_by(order_min) %>%
 summarise(sum = sum(quantity)) %>%
 ggplot(aes(x = order_min, y = sum))?+
 geom_line(size = 1, alpha = 0.1) +
 scale_color_manual(values = palette_light()) +
 theme_tq() +
 labs(x = "",
  y = "sum of quantities",
  color = "",
  title = "Transactions of Yungo 400+50")
x6

# tech & yungo 비교
grid.arrange(x5, x6, ncol = 3)



#?I've always been trying to have a quick glimpse of what's happing, so i just wanted to give it a try for a scarping tmall reviews with rvest that i learned!

library(rvest)

# 果萃沐浴露
url <- 'https://detail.tmall.hk/item.htm?spm=a220m.1000858.1000725.1.?ad6297cof43fi&id=600052650795&skuId=4188286968549&user_id=2549234913&cat_id=52792006&is_b=1&rn=240fefacc0ab303bf5c7f9b55ef15623'

doc <- read_html(url, "utf-8")
 html_nodes("tm-rate-fulltxt") %>% 
 html_text()
 doc
# JS.. 실패!!

 
 writeLines(sprintf("var?page = require('webpage').create();
page.open('%s', function () {
  console.log(page.content); //page source
  phantom.exit();
  });", url), con="scrape.js")
 
 system("phantomjs scrape.js > scrape.html")
 
 
 page_html <- html("scrape.html")
 page_html %>? html_nodes('class') %>% html_table()
page_html

# 왜!! 안됨.  phantomjs.. 모르겠음
# 마지막 도전, rselenum

library(RSelenium)
remDr <- remoteDriver(port=4445L, browserName="chrome") 
remDr$open() 
remDr$navigate("https://detail.tmall.hk/item.htm?spm=a220?.1000858.1000725.1.6ad6297cof43fi&id=600052650795&skuId=4188286968549&user_id=2549234913&cat_id=52792006&is_b=1&rn=240fefacc0ab303bf5c7f9b55ef15623")

html <- remDr$getPageSource()[[1]]
html <- read_html(html)

review_obd <- html %>% 
 html_nodes('.tm-rate?fulltxt') %>% 
 html_text()
review_obd
Encoding(review_obd) <- "UTF-8"

# yay! 
# 페이지 넘어가는 법은 어떻게 하는건지 끝내 못함.
# 중문 분석을 해볼까나?

library(jiebaR)
library(wordcloud2) 
library(RColorBrewer)

seg <- qseg[review_obd]
seg <- seg[nchar(?eg)>1]
seg <- table(seg)
seg <- sort(seg, decreasing = TRUE)
seg
names(seg)

set.seed(100)
wordcloud2(seg, size=2, color = "random-light", backgroundColor = "grey")



dev.off()

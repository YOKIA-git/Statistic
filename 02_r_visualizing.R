library(tidyverse)
library(cowplot)


##---ggplot: data, aesthetics, geometric objects and others----------
## data
ggplot(data = mtcars)

## data, aesthetics, geometric 
ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(fill = 'grey', color = 'black', bins = 5) +
  geom_density(color = 'red')

## data, aesthetics, geometric
ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), fill = 'grey', color = 'black', bins =5) +
  geom_density(color = 'red', lwd =1)

## others
ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), fill = 'grey', color = 'black', bins = 5) +
  geom_density(color = 'red', lwd = 1) +
  ggtitle('Histogram with Imposed Density Curve \n (Miles Per Gallon)')

## others 
ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), fill = 'grey', color = 'black',bins = 5) +
  geom_density(color = 'red', lwd =1) +
  ggtitle('Histogram with Imposed Density Curve \n (Miles per Gallon)') +
  ylab('Density') +
  xlab('Miles per Gallon') +
  theme(plot.title = element_text(hjust = 0.5))

# others
ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), fill = 'grey', color = 'black',bins = 5) +
  geom_density(color = 'red', lwd =1) +
  ggtitle('Histogram with Imposed Density Curve \n (Miles per Gallon)') +
  ylab('Density') +
  xlab('Miles per Gallon') +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5))


## how save my plot 
fig1 <- ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), fill = 'grey', color = 'black',bins = 5) +
  geom_density(color = 'red', lwd =1) +
  ggtitle('Histogram with Imposed Density Curve \n (Miles per Gallon)') +
  ylab('Density') +
  xlab('Miles per Gallon') +
  theme(plot.title = element_text(hjust = 0.5))

fig1

ggsave("C:/Users/mi/SynologyDrive/Rclub/myfirstplot.pdf", fig1) 

## plot grid 1
mtcars$am <- factor(mtcars$am, labels = c('Mannual', 'Automatic'))
fig2 <- ggplot(data = mtcars, aes(y = mpg, x = am)) +
  geom_boxplot() +
  ggtitle('Boxplot for Miles per Gallon \n By Transmisstion Type') +
  xlab('Transmission Types') +
  ylab('Miles per Gallon')

plot_grid(fig1, fig2, nrow = 1)

## plot grid 2
plot_grid(fig1, fig2, nrow = 2)

## facet_plot 1
fig3 <- ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), fill = 'Green',
                 color = 'black', bins = 5) +
  facet_grid(am ~ .) +
  ggtitle('Histogram of Miles per Gallon \n By Transmission Type')
fig3 

## facet_plot 2
fig4 <- ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), fill = 'Green',
                 color = 'black', bins = 5)+
  facet_grid(. ~ am) +
  ggtitle('Histogram of Miles per Gallon \n By Transmission Type')
fig4

## -----------------------color--------------------------------------
fig8 <- ggplot(data =diamonds) +
  geom_boxplot(aes(x = color, y = price, fill = color)) +
  xlab('Color of Diamonds') +
  ylab('Price (USD)') +
  ggtitle('Diamond Price by Color')
fig8

fig8 <- fig8 + scale_fill_brewer(palette = 'Accent')
fig8

?scale_fill_brewer()

## ------------------------analysis----------------------------------
## ------------------------one variable------------------------------
## histogram
fig1 <- ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = 'blue') +
  geom_density(color = 'black') +
  xlab('Miles per Gallon') +
  ylab('') +
  ggtitle('Histogram of Miles per Gallon')
fig1

## boxplot
mtcars$type <- 'Automobiles'
fig2 <- ggplot(data = mtcars, aes(y = mpg, type)) +
  geom_boxplot(fill = 'blue', color = 'black') + 
  xlab('') +
  ylab('Miles per Gallon') +
  ggtitle('Boxplot of Miles per Gallon')
fig2

## Violinplot 
mtcars$type <- 'Automobiles'
fig3 <- ggplot(data = mtcars, aes(y = mpg, x = type)) +
  geom_violin(fill = 'blue', color = 'black',
              draw_quantiles = c(0.25, 0.5, 0.75),
              trim = F) +
  xlab('') +
  ylab('Miles per Gallon') +
  ggtitle('Violinplot of Miles per Gallon')
fig3

## bar chart 
fig4 <- ggplot(data = diamonds) +
  geom_bar(aes(x = cut, y = ..prop..,
               group = 1), fill = 'gold') + 
  xlab('Different Styles of Cut') +
  ylab('Percentage') +
  ggtitle('Barplot of Diamond Cuts')
fig4

## pie chart 
pie.data <- diamonds %>% 
  group_by(cut) %>% 
  summarise(perct = n()/nrow(diamonds))

fig5 <- ggplot(data = pie.data, aes(x = '')) +
  geom_bar(aes(y = perct, fill =cut), stat = 'identity') +
  coord_polar('y', start = 0) +
  xlab('') +
  ylab('') +
  ggtitle('Pie Chart of Diamond Cuts') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank())
fig5

## ------------------------two variables-----------------------------


######集中趋势
cars.new <- mtcars %>% 
  group_by(am) %>% 
  summarise(wt.avg =mean(wt))
cars.new$am <- factor(cars.new$am , labels = c('Mannual','Auto'))

fig6 <- ggplot(data = cars.new) +
  geom_bar(aes(x = am, y = wt.avg),stat = 'identity') +
  xlab('Transmission System') +
  ylab('Average Weight (tons)') +
  ggtitle('Automobile Average Weight by Transmission')
fig6

######离散趋势
fig7 <- ggplot(data = diamonds) +
  geom_violin(aes(x = color, y = price, fill = color),
              trim = F) +
  xlab('Color of Diamonds') +
  ylab('Price (USD)') +
  ggtitle('Diamond Price by Color')
fig7

######离散+均值
fig8 <- ggplot(data =diamonds) +
  geom_boxplot(aes(x = color, y = price, fill = color)) +
  xlab('Color of Diamonds') +
  ylab('Price (USD)') +
  ggtitle('Diamond Price by Color')
fig8


######相关
fig9 <- ggplot(data = diamonds, aes(x =carat, y = price )) +
  geom_point() +
  geom_smooth(method = 'auto', color = 'red', se = T)+ #增加一条相关曲线
  xlab('Diamond Weight (crat)') +
  ylab('Price (usd)') +
  ggtitle('diamond Price by Weight')
fig9

######相关
fig10 <- ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1))) + #切开之后看的更清楚
  xlab('Diamond Weight Group(carat)') +
  ylab('Price (USD)') +
  ggtitle('Diamond Price by Weight Group')

fig10

######相关

fig11 <- ggplot(data = diamonds, aes(x = cut, y = color)) +
  geom_count(shape = 'diamond') +
  xlab('Diamond Cut') +
  ylab('Diamond Color') +
  ggtitle('Diamond Color by Cut')
fig11



######相关
fig14 <- ggplot(data = diamonds, aes(x = carat, y = price , color = cut)) +
  geom_point() +
  geom_smooth(method = 'lm') +  
  xlab('Diamond Weight (carat)') +
  ylab('Diamond Price (USD)') +
  ggtitle('Diamond Price by Weight by Cut')
fig14

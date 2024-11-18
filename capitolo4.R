library(tidyverse)
library(here)
library(gapminder)
gapminder <- gapminder
gapminder %>% 
  slice(1:4)

glimpse(gapminder)

summary(gapminder)

#SOME PLOT
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = year, y = lifeExp)) +
  theme_bw()

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = year, 
                           y = lifeExp, 
                           colour = continent)) +
  scale_colour_grey() +
  theme_bw()

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = year, 
                           y = lifeExp, 
                           size = gdpPercap)) +
  theme_bw()

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = year, 
                           y = lifeExp, colour=continent,
                           size = gdpPercap)) +
  scale_color_grey()+
  theme_bw()



ggplot(data = gapminder) +
  geom_jitter(mapping = aes(x = year, 
                            y = lifeExp, 
                            shape = continent),
              width = 0.75, alpha = 0.5) +
  theme_bw()

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = year, y = lifeExp), 
             shape = 3) +
  theme_bw()

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = year, y = lifeExp)) + 
  facet_wrap(~ continent, nrow = 3) +
  theme_bw()

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = year, y = lifeExp)) + 
  facet_wrap(~ continent, nrow = 2) +
  theme_bw()

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp)) +
  theme_bw()

ggplot(data = gapminder) +
  geom_smooth(mapping = aes(x = gdpPercap, y = lifeExp)) +
  theme_bw()

ggplot(data = gapminder) +
  geom_point(mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_smooth(mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  theme_bw()

ggplot(data = gapminder, 
       mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

ggplot(data = gapminder, 
       mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(mapping = aes(shape = continent)) +
  geom_smooth() +
  theme_bw()

ggplot(data = gapminder, 
       mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point() +
  geom_smooth(mapping = aes(linetype = continent)) +
  theme_bw()

ggplot(data = gapminder, 
       mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(mapping = aes(shape = continent)) +
  geom_smooth(mapping = aes(colour = continent)) +
  scale_colour_grey() +
  theme_bw()

#BAR CHARTS
ggplot(data = gapminder) +
  geom_bar(mapping = aes(x = continent)) +
  scale_colour_grey() +
  theme_bw()
#FREQ /PRPORTIONS
ggplot(data = gapminder) +
  geom_bar(mapping = aes(x = continent, y = after_stat(count/sum(count)),
                         group = 1))
gapminder2 <- 
  gapminder %>% 
  count(continent) %>% 
  mutate(perc = n/sum(n) * 100)

pl <- gapminder2 %>% 
  ggplot(aes(x = continent, y = n, fill = continent)) 
pl <- pl + geom_col() + scale_fill_grey(start = 0, end = .9)
pl <- pl + geom_text(aes(x = continent, y = n,
                         label = paste0(n, " (", round(perc,1),"%)"),
                         vjust = -0.5))
pl <- pl + theme_classic()
pl <- pl + labs(title ="Bar chart showing counts and percentages")
pl

#TITLES

mypop <- ggplot(data = gapminder, 
                mapping = aes(x = log(gdpPercap), 
                              y = lifeExp,
                              shape = continent)) +
  geom_point(alpha = 0.4) +
  geom_smooth(mapping = aes(colour =  continent), se = FALSE) +
  scale_colour_grey() 
mypop

mypop + 
  ggtitle("GDP (in log) and life expectancy")

mypop <- mypop + ggtitle("GDP (in log) and life expectancy:
                \nData from Gapminder") 
mypop

#MIN MAX AXIS
mypop <- mypop + 
  scale_x_continuous(breaks = seq(0,12,1)) +
  scale_y_continuous(breaks = seq(0,90,10)) 
mypop

mypop + 
  ylab("Life Expentancy") + 
  xlab("Percapita GDP in log")


ggplot(data = gapminder, 
       mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(alpha = 0.4) +
  geom_smooth(mapping = aes(line =  continent), se = FALSE) +
  facet_wrap(~ continent) +
  ylab("Life Expentancy") + 
  xlab("Percapita GDP in log") +
  theme(legend.position="none") +
  theme_bw()


#SAVING
myplot <- 
  mypop + 
  ggtitle("GDP (in log) and life expectancy:
                \nData from Gapminder") + 
  ylab("Life Expentancy") + 
  xlab("Percapita GDP in log") +
  
  theme_bw()
myplot

ggsave(plot = myplot, 
       here("plots","my_pdf_plot.pdf"))

ggsave(plot = myplot, 
       here("plots","my_png_plot.png")) 

ggsave(plot = myplot, 
       here("plots","my_jpg_plot.jpg"))

ggsave(plot = myplot, 
       here('plots','my_pdf_plot2.pdf'), 
       width = 10, height = 6, units = "in",
       dpi = 150, device = 'pdf')

ggsave(plot = myplot, 
       here('plots','my_png_plot2.png'), 
       width = 10, height = 6, units = "cm", 
       dpi = 150, device = 'png')

ggsave(plot = myplot, 
       here('plots','my_png_plot2.png'), 
       width = 10, height = 6, units = "cm", 
       dpi = 150, device = 'png')
#
# U.S. Bureau of Labor Statistics
#

library(ggplot2)
library(ggrepel)
library(plyr)
library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))

employed.by.sex.2016 <- read.csv('data/cpsaat09_preprocessed.csv',
                            colClasses=c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
employed.by.sex.2005 <- read.csv('data/aat9_preprocessed.csv',
                            colClasses=c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
median.weekly.earnings <- read.csv('data/cpsaat39_preprocessed.csv',
                                   colClasses=c('character', 'numeric', 'numeric', 'numeric', 'numeric',
                                                'numeric', 'numeric'))

employment.by.sex.and.earnings <- join(employed.by.sex.2016, median.weekly.earnings, by='Occupation',
                                       type='inner') %>%
  join(employed.by.sex.2005, by='Occupation', type='inner')

employment.by.sex.and.earnings <- mutate(
  employment.by.sex.and.earnings,
  X2016.Women.Salary = X2016.Women.Median.weekly.earnings * 52,
  X2016.Percent.Women = Women.16.years.and.over.2016 / Total.16.years.and.over.2016 * 100,
  X2005.Percent.Women = Women.16.years.and.over.2005 / Total.16.years.and.over.2005 * 100,
  Growth.Percent.Women = X2005.Percent.Women / X2016.Percent.Women * 100
)

smallest <- floor(min(employment.by.sex.and.earnings$Total.16.years.and.over.2016) / 1000) * 1000
largest  <- ceiling(max(employment.by.sex.and.earnings$Total.16.years.and.over.2016) / 1000) * 1000
middle <- ceiling((largest - smallest) / 2 / 1000) * 1000

light.blue <- '#7788AA'
dark.blue <- '#152B55'
light.red <- '#EA9CAE'

highlights <- c('Computer and mathematical occupations',
                'Management, business, and financial operations occupations',
                'Healthcare support occupations',
                'Construction and extraction occupations'
)
highlight.data <- filter(employment.by.sex.and.earnings, Occupation %in% highlights)
nonhighlight.data <- filter(employment.by.sex.and.earnings, Occupation %!in% highlights)


ggplot(employment.by.sex.and.earnings,
       aes(x=X2016.Women.Salary, y=Growth.Percent.Women)) +
  geom_point(data=nonhighlight.data, aes(size=Total.16.years.and.over.2016), color=alpha(light.blue, 0.5),
             show.legend=FALSE) +
  geom_point(data=highlight.data, aes(size=Total.16.years.and.over.2016), color=alpha(light.red, 0.5),
             show.legend=FALSE) +
  scale_size_continuous(range=c(5, 20)) +
  geom_text_repel(data=highlight.data, aes(label=Occupation), size=3, nudge_y=-1) +
  geom_smooth(method='lm', se=FALSE, color=dark.blue) +
  xlab('Median Women\'s Salary (2016)') +
  ylab('Growth in Percentage of Women (2005 - 2016)') +
  ggtitle('Lower Salary Jobs See Higher Increase in Women') +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color=gray(0.8)),
    panel.grid.minor.y = element_line(color=gray(0.9)),
    panel.grid.major.x = element_line(color=gray(0.8)),
    panel.grid.minor.x = element_line(color=gray(0.9))
  )

circle.legend <- ggplot(employment.by.sex.and.earnings,
                        aes(x=X2016.Women.Salary, y=Growth.Percent.Women)) +
  geom_point(data=filter(employment.by.sex.and.earnings,
                         Total.16.years.and.over.2016 == min(Total.16.years.and.over.2016)),
             x=0.5, y=0.5, shape=1, aes(size=smallest), show.legend=FALSE) +
  geom_point(data=filter(employment.by.sex.and.earnings,
                         Total.16.years.and.over.2016 == max(Total.16.years.and.over.2016)),
             x=0.5, y=0.565, shape=1, aes(size=largest), show.legend=FALSE) +
  geom_point(data=filter(employment.by.sex.and.earnings,
                         Total.16.years.and.over.2016 == max(Total.16.years.and.over.2016)),
             x=0.5, y=0.545, shape=1, aes(size=middle), show.legend=FALSE) +
  geom_text(x=0.58, y=0.5, label=smallest / 1000) +
  geom_text(x=0.58, y=0.575, label=middle / 1000) +
  geom_text(x=0.58, y=0.65, label=largest / 1000) +
  scale_size_continuous(range=c(5,20)) +
  xlab('') +
  ylab('') +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank()
  )

ggplot(employment.by.sex.and.earnings,
       aes(x=X2016.Women.Salary, y=X2016.Percent.Women)) +
  geom_point(data=nonhighlight.data, aes(size=Total.16.years.and.over.2016), color=alpha(light.blue, 0.5),
             show.legend=FALSE) +
  geom_point(data=highlight.data, aes(size=Total.16.years.and.over.2016), color=alpha(light.red, 0.5),
             show.legend=FALSE) +
  scale_size_continuous(range=c(5, 20)) +
  geom_text_repel(data=highlight.data, aes(label=Occupation), size=3) +
  geom_smooth(method='lm', se=FALSE, color=dark.blue) +
  xlab('Median Women\'s Salary') +
  ylab('Percentage of Women in Industry') +
  ggtitle('Percentage of Women in Industry Shows no Effect on Salary (2016)') +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color=gray(0.8)),
    panel.grid.minor.y = element_line(color=gray(0.9)),
    panel.grid.major.x = element_line(color=gray(0.8)),
    panel.grid.minor.x = element_line(color=gray(0.9))
  )

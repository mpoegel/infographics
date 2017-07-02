#
# U.S. Bureau of Labor Statistics
#

library(ggplot2)
library(ggrepel)
library(grid)
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
  Growth.Percent.Women = X2016.Percent.Women - X2005.Percent.Women,
  X2016.Men.Salary = X2016.Men.Median.weekly.earnings * 52,
  X2016.Percent.Men = Men.16.years.and.over.2016 / Total.16.years.and.over.2016 * 100,
  X2005.Percent.Men = Men.16.years.and.over.2005 / Total.16.years.and.over.2005 * 100,
  Growth.Percent.Men = X2016.Percent.Men - X2005.Percent.Men
)

smallest <- floor(min(employment.by.sex.and.earnings$Total.16.years.and.over.2016) / 1000) * 1000
largest  <- ceiling(max(employment.by.sex.and.earnings$Total.16.years.and.over.2016) / 1000) * 1000
middle <- ceiling((largest - smallest) / 2 / 1000) * 1000

light.blue <- '#7788AA'
dark.blue <- '#152B55'
light.red <- '#EA9CAE'
dark.red <- '#75142B'
off.white <- '#D9D9EA'

highlights <- c('Computer and mathematical occupations',
                'Management, business, and financial operations occupations',
                'Healthcare support occupations',
                'Construction and extraction occupations'
)
highlight.data <- filter(employment.by.sex.and.earnings, Occupation %in% highlights)
nonhighlight.data <- filter(employment.by.sex.and.earnings, Occupation %!in% highlights)


growth.and.salary.women <- ggplot(employment.by.sex.and.earnings,
                                  aes(x=X2016.Women.Salary / 1000, y=Growth.Percent.Women)) +
  geom_point(aes(size=Total.16.years.and.over.2016, color=X2016.Percent.Women)) +
  scale_size_continuous(range=c(5, 20), guide=FALSE) +
  scale_color_continuous(low=alpha('#ffffe0', 0.5), high=alpha('#8b0000', 0.5), name='') +
  geom_text_repel(data=highlight.data, aes(label=Occupation), size=3, nudge_y=-1) +
  geom_smooth(method='lm', se=FALSE, color=dark.blue) +
  xlab('Median Women\'s Salary (thousands, 2016)') +
  ylab('Change in Percentage of Women (2005 - 2016)') +
  ggtitle('Higher Salary Jobs See Higher Increase in Women') +
  theme(
    plot.background = element_rect(fill = off.white, color = off.white),
    plot.title = element_text(size=20),
    panel.background = element_rect(fill = off.white, color = off.white),
    panel.grid.major.y = element_line(color=gray(0.8)),
    panel.grid.minor.y = element_line(color=gray(0.9)),
    panel.grid.major.x = element_line(color=gray(0.8)),
    panel.grid.minor.x = element_line(color=gray(0.9)),
    legend.background = element_rect(fill = off.white, color = off.white),
    legend.position = 'bottom',
    legend.key.size = unit(1, 'cm')
  )

growth.and.salary.men <- ggplot(employment.by.sex.and.earnings,
                                aes(x=X2016.Men.Salary, y=Growth.Percent.Men)) +
  geom_point(data=nonhighlight.data, aes(size=Total.16.years.and.over.2016), color=alpha(light.blue, 0.5),
             show.legend=FALSE) +
  geom_point(data=highlight.data, aes(size=Total.16.years.and.over.2016), color=alpha(light.red, 0.5),
             show.legend=FALSE) +
  scale_size_continuous(range=c(5, 20)) +
  geom_text_repel(data=highlight.data, aes(label=Occupation), size=3, nudge_y=-1) +
  geom_smooth(method='lm', se=FALSE, color=dark.blue) +
  xlab('Median Men\'s Salary (2016)') +
  ylab('Change in Percentage of Men (2005 - 2016)') +
  ggtitle('Higher Salary Jobs See Higher Decrease in Men') +
  theme(
    plot.background = element_rect(fill = off.white, color = off.white),
    panel.background = element_rect(fill = off.white, color = off.white),
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
  geom_text(x=0.57, y=0.5, label=smallest / 1000) +
  geom_text(x=0.57, y=0.575, label=middle / 1000) +
  geom_text(x=0.57, y=0.65, label=largest / 1000) +
  scale_size_continuous(range=c(5,20)) +
  xlab('') +
  ylab('') +
  theme(
    plot.background = element_rect(fill = 'transparent', colour = NA),
    panel.background = element_rect(fill = 'transparent', colour = NA)
  )

gender.pay.gap <- ggplot(employment.by.sex.and.earnings,
                                   aes(x=X2016.Men.Salary / 1000, y=X2016.Women.Salary / 1000)) +
  geom_point(aes(size=Total.16.years.and.over.2016, color=X2016.Percent.Women),
             show.legend=FALSE) +
  scale_size_continuous(range=c(5, 20)) +
  scale_color_continuous(low=alpha('#ffffe0', 0.5), high=alpha('#8b0000', 0.5)) +
  scale_x_continuous(limits=c(20, 100), breaks=c(20, 40, 60, 80, 100)) +
  scale_y_continuous(limits=c(20, 100), breaks=c(20, 40, 60, 80, 100)) +
  geom_text_repel(data=highlight.data, aes(label=Occupation), size=3, nudge_y = 10) +
  geom_abline(slope=1, intercept=0, color=alpha(dark.blue, 0.5)) +
  xlab('Median Men\'s Salary (thousands)') +
  ylab('Median Women\'s Salary (thousands)') +
  ggtitle('Yet the Gender Pay Gap is Not Effected By Percentage of\nWomen in Industry (2016)') +
  theme(
    plot.background = element_rect(fill = off.white, color = off.white),
    plot.title = element_text(size=20),
    panel.background = element_rect(fill = off.white, color = off.white),
    panel.grid.major.y = element_line(color=gray(0.8)),
    panel.grid.minor.y = element_line(color=gray(0.9)),
    panel.grid.major.x = element_line(color=gray(0.8)),
    panel.grid.minor.x = element_line(color=gray(0.9))
  )


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

png('./reports/figures/better_jobs_no_equality.png', width = 10, height = 15, units = "in",
    res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=11, ncol=5)))
grid.rect(gp = gpar(fill = off.white, col = off.white))
print(growth.and.salary.women + theme(plot.margin=margin(0, 2, -1, 2, unit="cm")),
      vp = vplayout(c(2,5), c(1,5)))
print(gender.pay.gap + theme(plot.margin=margin(-1, 2, 0, 2, unit="cm")),
      vp = vplayout(c(7,10), c(1,5)))
print(circle.legend + theme(plot.margin=margin(-0.5, -8.5, 0.5, 8.5, unit="cm")),
      vp = vplayout(c(5,7), c(1,5)))

grid.rect(
  x = unit(0, 'npc'), y = unit(1, 'npc'), just = 'left',
  width = unit(1, 'npc'), height = unit(0.03, 'npc'),
  gp = gpar(fill = '#9C344C', col = '#9C344C')
)
grid.rect(
  x = unit(0, 'npc'), y = unit(0.975, 'npc'), just = 'left',
  width = unit(0.07, 'npc'), height = unit(0.08, 'npc'),
  gp = gpar(fill = '#9C344C', col = '#9C344C')
)

grid.text('Better Jobs, But Still No Equality',
          x = unit(0.1, 'npc'), y = unit(0.96, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = gray(0.1), fontsize = 30))
grid.text('Millions of\nWorkers',
          x = unit(0.79, 'npc'), y = unit(0.535, 'npc'), just = 'right',
          gp = gpar(fontfamily = 'Segoe UI', col = gray(0.1), fontsize = 15))

# footer
grid.rect(
  x = unit(0, 'npc'), y = unit(0.025, 'npc'), just = 'left',
  width = unit(1, 'npc'), height = unit(0.05, 'npc'),
  gp = gpar(fill = dark.red, col = dark.red)
)
grid.text('Source: U.S. Bureau of Labor Statistics',
          x = unit(0.05, 'npc'), y = unit(0.025, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = gray(0.9), fontsize = 15))
grid.text('Matt Poegel',
          x = unit(0.95, 'npc'), y = unit(0.025, 'npc'), just = 'right',
          gp = gpar(fontfamily = 'Segoe UI', col = gray(0.9), fontsize = 15))

dev.off()

#
# In this script we look at economic data for the three countries in NAFTA
#
library(ggplot2)
library(reshape2)
library(tidyverse)

main.font <- 'Bookman Old Style'
black <- '#000000'
# Color theme: http://paletton.com/#uid=1400u0kllllaFw0g0qFqFg0w0aF
blue <- '#363377'
light.blue <- '#565595'
lighter.blue <- '#8180B2'
off.white <- '#D9D9EA'
dark.blue <- '#0A093B'
white <- '#FFFFFF'

# Define a graph theme
graph.theme <- function() {
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = white, color = white),
    panel.background = element_rect(fill = white),
    axis.text = element_text(color = blue),
    plot.title = element_text(color = dark.blue, face = "bold", size = 24, vjust = 1),
    axis.title = element_text(color = dark.blue, face = "bold", size = 18),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm")),
    panel.grid.major.y = element_line(color = off.white),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = off.white),
    panel.grid.minor.x = element_blank()
  )
}

# Add a line to a plot to show where NAFTA began
mark.nafta.start <- function(plot, y) {
  plot + 
    # NAFTA took effect on Jan. 1, 1994
    geom_vline(xintercept = 1994, color = blue, linetype = 2) +
    geom_text(aes(x = 1994, y = 22000, label = 'NAFTA'), hjust = -0.2, size = 4, color = blue)
}

economic.data <- read.csv('data/WEOOct2016all.csv')
nafta.countries <- c('United States', 'Canada', 'Mexico')
nafta.data <- filter(economic.data, Country %in% nafta.countries)

# Let's start by plotting the GDP of these three countries
gdp.data <- filter(nafta.data, WEO.Subject.Code == 'NGDPD') %>%
  melt(id.vars = 'Country',
       measure.vars = grep('\\d', names(nafta.data), value = TRUE)) %>%
  transmute(
    Country = Country,
    Year = as.numeric(gsub('X', '', variable)),
    GDP = as.numeric(gsub(',', '', value)))
gdp.plot <- ggplot(gdp.data,
                   aes(x = Year, y = GDP, group = Country)) +
  geom_line(data = filter(gdp.data, Year <= 2015), stat = 'identity', aes(color = Country),
            linetype = 1) +
  geom_line(data = filter(gdp.data, Year >= 2015), stat = 'identity', aes(color = Country),
            linetype = 2) +
  xlab('Year') +
  ylab('GDP (Billions of U.S. Dollars)') +
  ggtitle('GDP of NAFTA Member Countries') +
  graph.theme()
  
gdp.plot <- mark.nafta.start(gdp.plot, 22000)
gdp.plot

# Save this plot
png('reports/figures/nafta_countries_gdp.png')
gdp.plot
dev.off()

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
mark.nafta.start <- function(plot) {
  plot + 
    # NAFTA took effect on Jan. 1, 1994
    geom_vline(xintercept = 1994, color = blue, linetype = 2) +
    geom_text(aes(x = 1994, y = NAFTA.label.y, label = 'NAFTA'), hjust = -0.2, size = 4, color = blue)
}

# Load data from the World Economic Outlook report by the International Monetary Fund from
#   October 2016
economic.data <- read.csv('data/WEOOct2016all.csv')
nafta.countries <- c('United States', 'Canada', 'Mexico')
nafta.data <- filter(economic.data, Country %in% nafta.countries)
numeric.cols <- colnames(select(nafta.data, matches('\\d')))
nafta.data[numeric.cols] <- lapply(nafta.data[numeric.cols], 
                                   (function (x) as.numeric(as.character(gsub(',', '', x)))))


# Let's start by plotting the GDP of these three countries
gdp.data <- filter(nafta.data, WEO.Subject.Code == 'NGDPD') %>%
  melt(id.vars = 'Country',
       measure.vars = grep('\\d', names(nafta.data), value = TRUE)) %>%
  transmute(
    Country = Country,
    Year = as.numeric(gsub('X', '', variable)),
    GDP = value)
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

NAFTA.label.y <- 22000
gdp.plot <- mark.nafta.start(gdp.plot)
gdp.plot

# Save this plot
png('reports/figures/nafta_countries_gdp.png')
gdp.plot
dev.off()

# Maybe NAFTA has had an impact on growth?
growth.data <- filter(nafta.data, WEO.Subject.Code == 'NGDP_RPCH') %>%
  melt(id.vars = 'Country',
       measure.vars = grep('\\d', names(nafta.data), value = TRUE)) %>%
  transmute(
    Country = Country,
    Year = as.numeric(gsub('X', '', variable)),
    Growth = value)
growth.plot <- ggplot(growth.data,
                      aes(x = Year, y = Growth, group = Country)) +
  geom_line(data = filter(growth.data, Year <= 2015), stat = 'identity', aes(color = Country),
            linetype = 1) +
  geom_line(data = filter(growth.data, Year >= 2015), stat = 'identity', aes(color = Country),
            linetype = 2) +
  xlab('Year') +
  ylab('Growth (Percent Change in GDP)') +
  ggtitle('GDP Growth of NAFTA Member Countries') +
  graph.theme()

NAFTA.label.y <- 9
growth.plot <- mark.nafta.start(growth.plot)
growth.plot

# Save this plot
png('reports/figures/nafta_countries_gdp_growth.png')
growth.plot
dev.off()

# Mexico's GDP growth tanks in '94! Is this a coincidence or the start of a trend? Lets calculate
#   average growth before and after NAFTA was signed for each country
growth.avgs <- nafta.data %>%
  filter(WEO.Subject.Code == 'NGDP_RPCH')
before.cols <- growth.avgs %>% select(matches('198[0-9]|199[0-3]'))
after.cols  <- growth.avgs %>% select(matches('199[5-9]|200[0-9]|201[0-5]+'))
growth.avgs <- growth.avgs %>%
  transmute(
    Country = Country,
    '1980-1993' = rowSums(before.cols) / length(before.cols),
    '1994-2015' = rowSums(after.cols) / length(after.cols)
  ) %>%
  melt(id.vars = 1)

growth.avgs.plot <- ggplot(growth.avgs,
                      aes(x = Country, y = value)) +
  geom_bar(stat = 'identity', aes(fill = variable), position = 'dodge') +
  labs(fill = 'Date Range') +
  xlab('Country') +
  ylab('Average Percentage Growth of GDP') +
  ggtitle('Average GDP Growth Before and After NAFTA') +
  graph.theme()
growth.avgs.plot

# Save this plot
png('reports/figures/nafta_countries_gdp_growth_avgs.png')
growth.avgs.plot
dev.off()

# Let's look at some statistics that have more effect on the average person, starting with
#   purchasing power parity
ppp.data <- nafta.data %>%
  filter(WEO.Subject.Code == 'PPPEX') %>%
  melt(id.vars = 'Country',
       measure.vars = grep('\\d', names(nafta.data), value = TRUE)) %>%
  transmute(
    Country = Country,
    Year = as.numeric(gsub('X', '', variable)),
    PPP = value)
ppp.plot <- ggplot(ppp.data,
                   aes(x = Year, y = PPP, group = Country)) +
  geom_line(data = filter(ppp.data, Year <= 2015), stat = 'identity', aes(color = Country),
            linetype = 1) +
  geom_line(data = filter(ppp.data, Year >= 2015), stat = 'identity', aes(color = Country),
            linetype = 2) +
  xlab('Year') +
  ylab('PPP Conversion Rate') +
  ggtitle('PPP of NAFTA Member Countries') +
  graph.theme()
NAFTA.label.y <- 8.5
ppp.plot <- mark.nafta.start(ppp.plot)
ppp.plot

# Save this plot
png('reports/figures/nafta_countries_ppp.png')
ppp.plot
dev.off()

# Unemployment rate is probably irrelevent but let's look at it anyway
unemployment.data <- nafta.data %>%
  filter(WEO.Subject.Code == 'LUR') %>%
  melt(id.vars = 'Country',
       measure.vars = grep('\\d', names(nafta.data), value = TRUE)) %>%
  transmute(
    Country = Country,
    Year = as.numeric(gsub('X', '', variable)),
    Unemployment = value)
unemployment.plot <- ggplot(unemployment.data,
                   aes(x = Year, y = Unemployment, group = Country)) +
  geom_line(data = filter(unemployment.data, Year <= 2015), stat = 'identity', aes(color = Country),
            linetype = 1) +
  geom_line(data = filter(unemployment.data, Year >= 2015), stat = 'identity', aes(color = Country),
            linetype = 2) +
  xlab('Year') +
  ylab('Unemployment Rate') +
  ggtitle('Unemployment Rate of NAFTA Member Countries') +
  graph.theme()
NAFTA.label.y <- 12
unemployment.plot <- mark.nafta.start(unemployment.plot)
unemployment.plot

# Save this plot
png('reports/figures/nafta_countries_unemployment.png')
unemployment.plot
dev.off()


#
# Now let's focus on imports and exports between NAFTA countries. We will examine datasets from the
#   governments of the United States and Canada regarding imports and exports.
#

ie.us.data <- read.csv('data/us-imports-exports.csv')
ie.ca.data <- read.csv('data/ca-imports-exports.csv')

# We'll start with the U.S data
ie.us.plot <- ie.us.data %>%
  filter(CTYNAME == 'Mexico' | CTYNAME == 'Canada') %>%
  ggplot(aes(x = year, y = IYR, group = CTYNAME)) +
  geom_line(aes(x = year, y = IYR * 1e6, color = CTYNAME, linetype = '1')) +
  geom_line(aes(x = year, y = EYR * 1e6, color = CTYNAME, linetype = '5')) +
  scale_linetype_manual(name = 'Type', values = c(1, 5), labels = c('Imports', 'Exports')) +
  labs(color = 'Country') +
  xlab('Year') +
  ylab('Value of Import/Exports (USD)') +
  ggtitle('Imports and Exports with the U.S.') +
  graph.theme()
NAFTA.label.y <- 3e11 + 2e10
ie.us.plot <- mark.nafta.start(ie.us.plot)
ie.us.plot  

# Save this plot
png('reports/figures/nafta_us_imports_exports.png')
ie.us.plot
dev.off()

# Now for Canada
ie.ca.plot <- rbind(ie.ca.data %>%
  filter(Sections == 'Total merchandise trade') %>%
  select(matches('DE|I')) %>%
  melt(id.vars = c('Sections')) %>%
  transmute(
    year = as.numeric(gsub('DE|I', '', variable)),
    type = ifelse(grepl('DE', variable), 'B', 'A'),
    country = 'Mexico',
    value = value * 0.76  # this data is in CAD
  ),
  ie.us.data %>%
    filter(CTYNAME == 'Canada') %>%
    transmute(
      year = year,
      country = 'United States',
      B = IYR * 1e6,  # this data is in millions
      A = EYR * 1e6
    ) %>%
    melt(id.vars = c('year', 'country'), variable.name = 'type')
) %>%
  mutate(
    country.type = paste(country, type)
  ) %>%
  ggplot(aes(x = year, y = value, group = country.type)) +
  geom_line(aes(x = year, y = value, color = country, linetype = type)) +
  scale_linetype_manual(name = 'Type', values = c(1, 5), labels = c('Imports', 'Exports')) +
  scale_y_log10() +
  labs(color = 'Country') +
  xlab('Year') +
  ylab('Value of Import/Exports (USD)') +
  ggtitle('Imports and Exports with Canada') +
  graph.theme()
NAFTA.label.y <- 5e10
ie.ca.plot <- mark.nafta.start(ie.ca.plot)
ie.ca.plot

# Save this plot
png('reports/figures/nafta_ca_imports_exports.png')
ie.ca.plot
dev.off()

# And finally, Mexico
ie.mx.plot <- rbind(
  ie.ca.data %>%
    filter(Sections == 'Total merchandise trade') %>%
    select(matches('DE|I')) %>%
    melt(id.vars = c('Sections')) %>%
    transmute(
      year = as.numeric(gsub('DE|I', '', variable)),
      type = ifelse(grepl('DE', variable), 'A', 'B'),
      country = 'Canada',
      value = value * 0.76  # this data is in CAD
  ),
  ie.us.data %>%
    filter(CTYNAME == 'Mexico') %>%
    transmute(
      year = year,
      country = 'United States',
      B = IYR * 1e6,  # this data is in millions
      A = EYR * 1e6
    ) %>%
    melt(id.vars = c('year', 'country'), variable.name = 'type')
) %>%
  mutate(
    country.type = paste(country, type)
  ) %>%
  ggplot(aes(x = year, y = value, group = country.type)) +
  geom_line(aes(x = year, y = value, color = country, linetype = type)) +
  scale_linetype_manual(name = 'Type', values = c(1, 5), labels = c('Imports', 'Exports')) +
  scale_y_log10() +
  labs(color = 'Country') +
  xlab('Year') +
  ylab('Value of Import/Exports (USD)') +
  ggtitle('Imports and Exports with Mexico') +
  graph.theme()
NAFTA.label.y <- 3e11 - 1e10
ie.mx.plot <- mark.nafta.start(ie.mx.plot)
ie.mx.plot

# Save this plot
png('reports/figures/nafta_mx_imports_exports.png')
ie.mx.plot
dev.off()



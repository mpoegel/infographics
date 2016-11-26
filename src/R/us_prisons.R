library(ggplot2)
library(ggrepel)

#
# Load the compiled data
#
prison.data <- read.csv("./data/jd-private-prison-data.csv")


#
# Make sure the data is stored as the correct types
#
prison.data$private <- as.numeric(as.character(prison.data$private))
prison.data$local <- as.numeric(as.character(prison.data$local))
prison.data$other <- as.numeric(as.character(prison.data$other))
prison.data$total <- as.numeric(as.character(prison.data$total))


#
# Color themes
#
dark.gray  <- "#D2D2D3"
light.gray <- "#F3F3F3"


#
# Plot the U.S. Total Population of Private Prisons over time
#
png("./reports/figures/total_us_private_prison_pop.png")
p1 <- ggplot(prison.data[prison.data$jurisdiction == "U.S. Total", ], 
             aes(x = year, y = private, group = jurisdiction)) +
  geom_line(stat="identity", aes(color=jurisdiction)) +
  xlab("Year") +
  ylab("Population of Private Prisons") +
  ggtitle("U.S. Total Population of Private Prisons from 2000-2014") +
  theme(
    legend.position = "none"
  )
p1
dev.off()


#
# Plot all of the states' private prison populations over time
#
not.a.state <- c("U.S. Total", "Federal", "State", "Midwest", "Northeast", "South", "West")

state.prison.data <- prison.data[ !prison.data$jurisdiction %in% not.a.state, ]

png("./reports/figures/private_prison_pop_by_state.png")
p2 <- ggplot(state.prison.data,
             aes(x = year, y = private, group = jurisdiction)) +
  geom_line(stat = "identity", aes(color=jurisdiction)) + 
  xlab("Year") + 
  ylab("Population in Private Prisons") +
  ggtitle("Population in Private Prisons by State (2000-2014)")
p2
dev.off()

labeled.states = c("Texas", "Florida")

png("./reports/figures/private_prison_pop_by_state_highlighted.png")
p3 <- ggplot(state.prison.data,
             aes(x = year, y = private, group = jurisdiction)) +
  geom_line(stat = "identity", color = dark.gray) +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "Texas"),
            color = "red") +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "Florida"),
            color = "blue") +
  geom_label_repel(
    data = subset(state.prison.data, jurisdiction %in% labeled.states & year == 2014),
    aes(2014, private, label = jurisdiction),
    color = "black",
    fill = light.gray,
    box.padding = unit(0.3, "cm"),
    point.padding = unit(0.1, "cm"),
    segment.color = 'grey50',
    label.size = 0
  ) +
  xlab("Year") +
  ylab("Population in Private Prisons") +
  ggtitle("Population in Private Prisons by State (2000-2014)") +
  theme(
    legend.position = "none"
  )
p3
dev.off()


#
# Plot the proportion of prisoners held in private prisons
#

state.prison.data$fraction.private = state.prison.data$private / state.prison.data$total

png("./reports/figures/private_prison_fraction_pop_by_state.png")
p4 <-ggplot(state.prison.data,
            aes(x = year, y = fraction.private, group = jurisdiction)) +
  geom_line(stat = "identity", aes(color=jurisdiction)) + 
  xlab("Year") + 
  ylab("Fraction in Private Prisons") +
  ggtitle("Fraction of Prison Population in Private Prisons by State (2000-2014)")
p4
dev.off()

labeled.states = c("New Mexico", "Alaska", "New Hampshire")

png("./reports/figures/private_prison_fraction_pop_by_state_highlighted.png")
p5 <- ggplot(state.prison.data,
             aes(x = year, y = fraction.private, group = jurisdiction)) +
  geom_line(stat = "identity", color = dark.gray) +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "New Mexico"),
            color = "red") +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "Alaska"),
            color = "blue") +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "New Hampshire"),
            color = "green") +
  geom_label_repel(
    data = subset(state.prison.data, jurisdiction %in% labeled.states & year == 2014),
    aes(2014, fraction.private, label = jurisdiction),
    color = "black",
    fill = light.gray,
    box.padding = unit(0.3, "cm"),
    point.padding = unit(0.1, "cm"),
    segment.color = 'grey50',
    label.size = 0
  ) +
  xlab("Year") + 
  ylab("Fraction in Private Prisons") +
  ggtitle("Fraction of Prison Population in Private Prisons by State (2000-2014)")
  theme(
    legend.position = "none"
  )
p5
dev.off()


#
# Plot prison population vs total population
#

world.prison.pop <- read.csv("./data/prison-population-by-country.csv")
world.pop <- read.csv("./data/population-by-country.csv")
world.pop.data <- merge(x = world.prison.pop, y = world.pop, by = "Country")

labeled.countries = c("United States", "China", "Russia", "United Kingdom", "India")

png("./reports/figures/total_population_vs_prison_population_by_country.png")
p6 <- ggplot(world.pop.data,
       aes(x = Population, y = Prison.Population, group = Country)) +
  geom_point(data = world.pop.data) +
  geom_label_repel(
    data = subset(world.pop.data, Country %in% labeled.countries),
    aes(x = Population, y = Prison.Population, label = Country),
    color = "black",
    fill = light.gray,
    box.padding = unit(0.3, "cm"),
    point.padding = unit(0.1, "cm"),
    segment.color = 'grey50',
    label.size = 0
  ) +
  xlab("Total Population") +
  ylab("Prison Population") +
  ggtitle("Total Population vs. Prison Population by Country")
p6
dev.off()


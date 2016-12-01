library(extrafont)
library(ggplot2)
library(ggrepel)
library(grid)

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
black      <- "#000000"
dark.gray  <- "#D2D2D3"
light.gray <- "#F3F3F3"

# Pallet: http://paletton.com/#uid=50q0u0kllllaFw0g0qFqFg0w0aF
orange      <- "#AA6839"
dark.orange <- "#804115"
red         <- "#A43741"

main.font <- "Bookman Old Style"

orangeLineTheme <- function() {
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = light.gray, color = light.gray),
    panel.background = element_rect(fill = light.gray),
    axis.text = element_text(color = dark.orange, family = main.font),
    plot.title = element_text(color = dark.orange, face = "bold", size = 24, vjust = 1, 
                              family = main.font),
    axis.title = element_text(color = dark.orange, face = "bold", size = 18, family = main.font),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm")),
    panel.grid.major.y = element_line(color = orange),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = main.font, color = "white"),
    strip.background = element_rect(fill = orange),
    axis.ticks = element_line(color = orange)
  )
}


#
# Plot the U.S. Total Population of Private Prisons over time
#
percision <- 10000
min.pop <- round(min(prison.data[prison.data$jurisdiction == "U.S. Total", ]$private)/ percision) *
  percision
max.pop <- round(max(prison.data[prison.data$jurisdiction == "U.S. Total", ]$private)/ percision) *
  percision

png("./reports/figures/total_us_private_prison_pop.png")
p1 <- ggplot(prison.data[prison.data$jurisdiction == "U.S. Total", ], 
             aes(x = year, y = private, group = jurisdiction)) +
  geom_line(stat="identity", color=red, size=1.5) +
  scale_x_continuous(breaks=c(seq(2000, 2014, 2))) +
  scale_y_continuous(breaks=c(seq(min.pop, max.pop + percision, percision))) +
  expand_limits(x = 2000, y = max.pop) +
  xlab("Year") +
  ylab("Population of Private Prisons") +
  ggtitle("Rapid Growth in Total Population of Private Prisons\nfrom 2000-2014") +
  orangeLineTheme()
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



#
# Create the poster
#
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


pdf("./reports/figures/private_prisons.pdf", width = 30, height = 40, fonts = main.font)
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=5, ncol=6)))
grid.rect(gp = gpar(fill = light.gray, color = light.gray))

# header
grid.rect(gp = gpar(fill = orange, color = orange),
          x = unit(0.5, "npc"), y = unit(0.95, "npc"),
          width = unit(1, "npc"), height = unit(0.10, "npc"))
grid.text("Monetizing Prisons",
          x = unit(0.5, "npc"), y = unit(0.97, "npc"), 
          gp = gpar(fontfamily = main.font, color = dark.gray, cex = 10))
grid.text("HOW MONETIZING THE U.S. PRISON SYSTEM HAS AFFECTED SOCIETY",
          x = unit(0.5, "npc"), y = unit(0.93, "npc"),
          gp = gpar(fontfamily = main.font, color = light.gray, cex = 3))
grid.text("Andrew Batbouta and Matt Poegel",
          x = unit(0.5, "npc"), y = unit(0.91, "npc"),
          gp = gpar(fontfamily = main.font, color = light.gray, cex = 2))

header.gpar <- gpar(fontfamily = main.font, color = light.gray, cex = 4)
paragraph.gpar <- gpar(fontfamily = main.font, color = light.gray, cex = 2)

# first column
grid.text("Prisons for Profit", x = unit(0.12, "npc"), y = unit(0.88, "npc"), gp = header.gpar)
grid.text("Bacon ipsum dolor amet venison t-bone picanha meatball
shankle, swine pork. Frankfurter filet mignon picanha venison
shankle turkey pork loin. Tongue beef flank biltong, sausage
brisket cow capicola. Kevin sirloin meatball, spare ribs cow
cupim alcatra biltong pastrami.",
          x = unit(0.03, "npc"), y = unit(0.84, "npc"), gp = paragraph.gpar, just = "left")

print(p1 + theme(plot.margin=margin(1, 0, 0, 2, unit="cm")),
      vp = vplayout(2, 1:2))

# second column


# third column


# footer
grid.rect(gp = gpar(fill = orange, color = orange),
          x = unit(0.5, "npc"), y = unit(0, "npc"),
          width = unit(1, "npc"), height = unit(0.1, "npc"))
grid.text("American Politics in Crisis, Fall 2016",
          x = unit(0.89, "npc"), y = unit(0.01, "npc"),
          gp = gpar(fontfamily = main.font, color = light.gray, cex = 2))

dev.off()


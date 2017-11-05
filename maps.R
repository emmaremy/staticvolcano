# Graphing
library(ggplot2)

# Maps
library(maps)

# Read in raw data
Eruptions <- read.csv("GVP_Eruption_Results.csv")


# Various ways to wrangle this data
# (most of which I don't actually use in graphing)

# Produces table of the number of confirmed eruptions per year
Eruptions.year <- 
  Eruptions %>%
  filter(EruptionCategory=="Confirmed Eruption") %>%
  group_by(StartYear) %>%
  summarize(count=length(StartYear))  

# Removes unconfirmed eruptions and eruptions before 1900
# and results in a table of the number of confirmed eruptions per year
# as well as the average VEI (magnitude)
Eruptions.year.1900 <-
  Eruptions %>%
  filter(EruptionCategory=="Confirmed Eruption", StartYear>=1900) %>%
  group_by(StartYear) %>%
  summarize(count=length(StartYear), avg.VEI=mean(VEI, na.rm=TRUE))

# Removes unconfirmed eruptions, calculates duration of eruption,
# and removes eruptions with durations < 0
Eruptions.duration <- 
  Eruptions %>%
  filter(EruptionCategory=="Confirmed Eruption")  %>%
  mutate(StartDate= as.Date(paste(StartYear, StartMonth, StartDay, sep = "." )  , format = "%Y.%m.%d" )) %>%
  mutate(EndDate= as.Date(paste(EndYear, EndMonth, EndDay, sep = "." )  , format = "%Y.%m.%d" )) %>%
  mutate(Duration = as.numeric(EndDate-StartDate +1)) %>%
  filter(Duration>=0)

# Removes unconfirmed eruptions
# and results in table of average VEI (magnitude) for each volcano
Eruptions.VEI  <-
  Eruptions %>%
  filter(EruptionCategory=="Confirmed Eruption") %>%
  group_by(VolcanoNumber) %>%
  mutate(avg.VEI=mean(VEI, na.rm=TRUE), Longitide = mean(Longitude),
         Latitude = mean(Latitude))



# Actually mapping

# Very basic map (volcano locations)
world <- map_data("world")
ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), 
                        fill = "grey", color = "blue", size = .1) + 
  coord_fixed(1.3) +
  geom_point(data = Eruptions, aes(x = Longitude, y = Latitude), 
             color = "red", alpha = .5, size = 1) + 
  labs(title = "Where are volcanoes erupting?") + 
  xlab("Longitude") + ylab("Latitude")

# Map with color by avg VEI
ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), 
                        fill = "grey", color = "blue", size = .1) + 
  coord_fixed(1.3) +
  geom_point(data = Eruptions.VEI, aes(x = Longitude, y = Latitude, color = avg.VEI), 
             alpha = .5, size = 2) + 
  labs(title = "Are eruptions more intense in some areas?") + 
  xlab("Longitude") + ylab("Latitude") +
  scale_colour_gradient(low = "yellow", high = "red")

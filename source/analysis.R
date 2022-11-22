library(tidyverse)
library(dplyr)
library(ggplot2)
Assignment_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
max_aapi <- function() {
  max <- Assignment_data %>%
    select(year, state, aapi_jail_pop) %>%
    drop_na() %>%
    filter(aapi_jail_pop == max(aapi_jail_pop)) 
  return(max)
}

max_black <- function() {
  max <- Assignment_data %>%
    select(year, state, black_jail_pop) %>%
    drop_na() %>%
    filter(black_jail_pop == max(black_jail_pop)) 
  return(max)
}

max_latinx <- function() {
  max <- Assignment_data %>%
    select(year, state, latinx_jail_pop) %>%
    drop_na() %>%
    filter(latinx_jail_pop == max(latinx_jail_pop)) 
  return(max)
}

max_native <- function() {
  max <- Assignment_data %>%
    select(year, state, native_jail_pop) %>%
    drop_na() %>%
    filter(native_jail_pop == max(native_jail_pop)) 
  return(max)
}

max_white <- function() {
  max <- Assignment_data %>%
    select(year, state, white_jail_pop) %>%
    drop_na() %>%
    filter(white_jail_pop == max(white_jail_pop)) 
  return(max)
}

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function only returns the year and total jail population from the dataset... <todo:  update comment>
get_year_jail_pop <- function() {
  Assignment_data %>%
    select(year, total_jail_pop) %>%
    drop_na() %>%
  return(year, total_jail_pop)   
}

# This function plots the total jail population by year with the function above... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  jail_plot <- ggplot(data = get_year_jail_pop(), aes(x = year, y = total_jail_pop)) + 
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Increase of Jail Population in U.S. (1970-2018") 
  return(jail_plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  state_year <- Assignment_data %>%
    select(state, year) %>%
    drop_na() %>%
    filter(str_detect(state, states)) %>%
    pull(state, year)
  return(state_year)
}

plot_jail_pop_by_states <- function(states) {
  state_graph <- ggplot(data = get_jail_pop_by_states(), aes(x = state, y = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("States") +
    ggtitle("Prison Population from 1970 to 2018 in States") +
  return(state_graph)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
aapi_pop_in_jail <- function() {
  Assignment_data %>%
    select(year, aapi_jail_pop) %>%
    drop_na() %>%
  return(year, aapi_jail_pop)
}

black_pop_in_jail <- function() {
  Assignment_data %>%
    select(year, black_jail_pop) %>%
    drop_na() %>%
  return(year, black_jail_pop)
}

latinx_pop_in_jail <- function() {
  Assignment_data %>%
    select(year, latinx_jail_pop) %>%
    drop_na() %>%
  return(year, latinx_jail_pop)
}

native_pop_in_jail <- function() {
  Assignment_data %>%
    select(year, native_jail_pop) %>%
    drop_na() %>%
  return(year, native_jail_pop)
}

white_pop_in_jail <- function() {
  Assignment_data %>%
    select(year, white_jail_pop) %>%
    drop_na() %>%
  return(year, white_jail_pop)
}

# Creating charts with the functions above
plot_aapi_pop_in_jail <- function() {
  aapi_plot <- ggplot(data = aapi_pop_in_jail(), aes(x = year, y = aapi_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("AAPI (Asian American Pacific Islander) Jail Population")
  return(aapi_plot)
}

plot_black_pop_in_jail <- function() {
  black_plot <- ggplot(data = black_pop_in_jail(), aes(x = year, y = black_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Black Jail Population")
  return(black_plot)
}

plot_latinx_pop_in_jail <- function() {
  latinx_plot <- ggplot(data = latinx_pop_in_jail(), aes(x = year, y = latinx_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Latinx Jail Population")
  return(latinx_plot)
}

plot_native_pop_in_jail <- function() {
  native_plot <- ggplot(data = native_pop_in_jail(), aes(x = year, y = native_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Native Jail Population")
  return(native_plot)
}

plot_white_pop_in_jail <- function() {
  white_plot <- ggplot(data = white_pop_in_jail(), aes(x = year, y = white_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("White Jail Population")
  return(white_plot)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ----

# Step 1: libraries
library(maps)

# Step 2: Get most recent assignment_data year from dataset
recent_pop <- Assignment_data %>%
  select(year, fips, state,county_name, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop) %>%
  filter(year == max(year))

# Step 3: Use map_data function to join the 'assignment_data' dataset with the map_data for county
data_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Step 4: Merge map data and filtered jail data together
map_data <- data_shape %>%
  left_join(recent_pop, by = "fips")

# Step 5: Incorporate blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background =  element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank(), # remove border around plot
  )

# Step 6: Create the map
jail_map_aapi <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = aapi_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$aapi_jail_pop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("AAPI (Asian American Pacific Islander) Jailing Population")

jail_map_black <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Black Jailing Population")


jail_map_latinx <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = latinx_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$latinx_jail_pop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Latinx Jailing Population")


jail_map_native <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = native_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$native_jail_pop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Native Jailing Population")


jail_map_white <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = white_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$white_jail_pop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("White Jailing Population")


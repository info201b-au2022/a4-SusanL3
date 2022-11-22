
# Load the dplyr and ggplot2 package
library(ggplot2)
library(dplyr)

# Load the data from https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv
Assignment_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

# Section 3
# get_year_jail_pop() : This data wrangling function should 
# return a data frame that is suitable for visualization. 
# This function takes no parameters. 
get_year_jail_pop <- function() {
   Assignment_data %>%
    select(year, total_jail_pop) %>%
    drop_na()
}

# plot_jail_pop_for_us() : This plotting function should 
# return the chart. This function: (1) Takes no parameters; 
# and (2) Should call the data wrangling function.
plot_jail_pop_for_us <- function() {
  ggplot(data = get_year_jail_pop(), aes(x = year, y = total_jail_pop)) + 
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Increase of Jail Population in U.S. (1970-2018")
}

# Section 4
# get_jail_pop_by_states(states): This data wrangling function 
# should return a data frame that is suitable for visualization. 
# The parameter states should be a vector of states.
get_jail_pop_by_states <- function(states) {
  Assignment_data %>%
    select(state, year) %>%
    drop_na() %>%
    filter(str_detect(state, states))
    return(get_jail_pop_by_states())
}

# plot_jail_pop_by_states(states): This plotting function should 
# return the chart. The parameter states should be a vector of states. 
# This function should call the data wrangling function.
plot_jail_pop_by_states <- function(states) {
  ggplot(data = get_jail_pop_by_states(), aes(x = year, y = state)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("States") +
    ggtitle("Prison Population from 1970 to 2018 in States")
}

# Section 5
# Creating a function that shows the population of each groups imprisoned. 
# Takes no parameters.

aapi_pop_in_jail <- function() {
  Assignment_data %>%
    select(aapi_jail_pop, year) %>%
    drop_na()
}

black_pop_in_jail <- function() {
  Assignment_data %>%
    select(black_jail_pop, year) %>%
    drop_na()
}

latinx_pop_in_jail <- function() {
  Assignment_data %>%
    select(latinx_jail_pop, year) %>%
    drop_na()
}

native_pop_in_jail <- function() {
  Assignment_data %>%
    select(native_jail_pop, year) %>%
    drop_na()
}

white_pop_in_jail <- function() {
  Assignment_data %>%
    select(white_jail_pop, year) %>%
    drop_na()
}


# Creating charts with the functions above
plot_aapi_pop_in_jail <- function() {
  ggplot(data = aapi_pop_in_jail(), aes(x = year, y = aapi_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("AAPI (Asian American Pacific Islander) People Jail Population")
}

plot_black_pop_in_jail <- function() {
  ggplot(data = black_pop_in_jail(), aes(x = year, y = black_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Black People Jail Population")
}

plot_latinx_pop_in_jail <- function() {
  ggplot(data = latinx_pop_in_jail(), aes(x = year, y = latinx_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Latinx People Jail Population")
}

plot_native_pop_in_jail <- function() {
  ggplot(data = native_pop_in_jail(), aes(x = year, y = native_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Native People Jail Population")
}

plot_white_pop_in_jail <- function() {
  ggplot(data = white_pop_in_jail(), aes(x = year, y = white_jail_pop, fill = year)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("White People Jail Population")
}


# Section 6
# Produce a map that reveals a potential inequality.
# Libraries
library(tidyverse)
install.packages("maps")
library(maps)



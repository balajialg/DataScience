---
  title: "Some data about countries"
output:  
  flexdashboard::flex_dashboard:
  orientation: columns
vertical_layout: fill
theme: yeti
runtime: shiny
---
  
  
#include = FALSE mentions that this will not be displayed
# as before, we need to do all of our librarying and data importation in this document; make sure all of these are installed on your computer before librarying
library(flexdashboard) # including the flexdashboard package
library(shiny) # and shiny for interactive plots
library(ggplot2)
library(plotly)
library(dplyr)
library(texreg)
gapminder <- read.csv('C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/Spring/S022 - Introduction to Data Science/Working Directory/gapminder_sub.csv')

library(flexdashboard) # used to create a flexdashboard; this is a great way to create data visualizations using R, especially interactive visualizations
library(ggplot2) # need ggplot2 for our ggplots
library(viridis) # for coloring maps
library(ggmap) # for plotting maps using shape files
library(tidyverse)

load(url('https://www.dropbox.com/s/spgon1foi4whrqz/mapping.RData?dl=1')) # by default, this is in reference to where the .Rmd file is located
gapminder %>% filter(!is.na(continent), year == 2005) %>% ggplot(aes(x = gni, fill = continent)) + geom_density(alpha = .3) + 
  labs(x = 'GNI per-capita', y = 'Density', title = 'Density plot of per-capita GNI by continent in the year 2005', fill = 'Continent')

names(gapminder)
selectInput("map_type", label = "Race of residents:", # this will allow people to select the race of respondents that they want to map
            choices = c('Asian', 'Black', 'Latinx', 'White'), selected = 'Asian') # we'll be able to access it in renderPlot (below) by referring to input$map_type

selectInput("color_scheme", label = 'Color scheme:', # similarly, we'll refer to this variable as input$color_scheme
            choices = c('A', 'B', 'C', 'D', 'E'), selected = 'A')


renderPlot({
  census_dat$estimate <- census_dat[[paste0(tolower(input$map_type), '_income')]] # create a new column called estimate (for plotting); this will be equal to the paste0(tolower(input$map_type), '_income') column of census_dat. So if a viewer has selected 'Asian', then dat$estimate will be the dat$asian_income column
  ggplot(census_dat, aes(fill = estimate)) +
    geom_sf() +
    coord_sf(crs = 26914) +
    scale_fill_viridis(option = input$color_scheme) # if the viewer has selected, e.g., B for the color scheme, then the option for viridis will be B
})

length(unique(gapminder$country))` <!--- notice that we can embed R code directly into the document outside of a chunk ---> different countries in the <a href = "https://www.gapminder.org/">gapminder</a> <!--- this is an html document, so it allows us to insert html code. This creates an anchor in the document which links to https://www.gapminder.org/. If you know a lot about html, you can do really fancy things. If you don't know html, that's okay because R Markdown will make it relatively easy to use ---> dataset.

# we want the results of this code to display, but not the code itself. echo = FALSE will accomplish that. Warning messages may also be displayed, and we may want to explicitly turn them off. 
#Echo says hide the code, Warning = FALSE says no warning
gapminder %>% filter(!is.na(continent), year == 2005) %>% ggplot(aes(x = continent, y = gni, fill = continent)) + geom_violin() + 
  labs(x = 'Continent', y = 'GNI per-capita', title = 'Violin plot of per-capita GNI by continent in the year 2005', fill = 'Continent')



### Due date: 3/12/2020, by 11:59PM This document was created using [R Markdown](https://r4ds.had.co.nz/r-markdown.html), an extension of R that we'll learn later in the term. I'll post both the final version and the code used to create it.

### Purpose of the assignment In this pset we'll creating interactive visualizations using R. This will combine skills in R Markdown and Shiny.

### Logistics Feel free to collaborate on this assignment. Each person should submit a document representing their own work. When submitting your assignment, please make it clear which question each response is intended to address.  Also, make sure to include R code with clear commenting at the end of your assignment. TFs will use this to identify the source of any errors you may have made. If they're not able to follow your code, they won't be able to find the mistakes. You should assume that the TF is running this code in a folder which has a file called gapminder.csv, which is the subset of the gapminder dataset that we used in class. Your code needs to work on the TF's computer, so you should read in the data with `gapminder <- read.csv('gapminder.csv', stringsAsFactors = FALSE)`. Your only submission will be the .Rmd document, so please be sure that it works.

#### Background We'll continue to use data from gapminder for this pset. We're going to create a simple dashboard which will allow the user to interact directly with the data from Gapminder. A final project for this class could, in theory, be a more intricate version of this dashboard.
   
#### Step 1 Create a dashboard to store these results. Create a tab on your dashboard which plots a map of the world and allows the user to select a year and a quantitative variable and create a choropleth using data for the specific variable in the specific year. Try to give the user the ability to hover over text and see the value of the variable for that country, as well as the country name. Remember from week 3, plotly can plot countries without passing their geometries. As a challenge, see if you can allow the user to select a date range, and display the average of that variable for the country. Look at the end of unit 2 for an example of plotting a world map using gapminder in plotly. Note that plotly also doesn't love working with variable names, just like ggplot. However, we can get it to work in a couple of ways. Suppose I have a variable name, 'gni'. First, I can tell plotly exactly which data to use with

#```
#plot_ly(x = gapminder[['gni']])
#```

#Second, I can use the get() function along with the typical syntax, as in

#```
#plot_ly(gapminder, x = ~get('gni'))
#```

#This may be useful to you in working with user variable selections.

#### Step 2 Create a second tab which allows the user to select a year, a set of countries and a set of variables and creates a table showing values for those countries in that year. Consider using renderTable.

#### Step 3 Create a third tab which adds additional interactivity. Exactly what you want to do is up to you, but it should use the gapminder data and should not be identical to anything we've done in class or on this assignment.
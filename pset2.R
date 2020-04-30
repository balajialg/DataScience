### Due date: 2/13/2020, by 11:59PM

#This document was created using [R Markdown](https://r4ds.had.co.nz/r-markdown.html), an extension of R that we'll learn later in the term. I'll post both the final version and the code used to create it.

### Purpose of the assignment The goal of this pset is to have you use ggplot and plotly to produce visualizations. We'll make static figures with ggplot and interactive figures with plotly. We'll be doing this using data from gapminder, the same dataset as in class. We'll start by creating some new variables, renaming some variables, and making plots. Some of the plots may require you to make decisions; oftentimes there are multiple ways to do something, and you are free to select one. Some of these may ask you to do things we haven't covered in class; in these cases, consider searching for help online and looking at the ggplot and plotly cheatsheets. If you try those and they don't work, you're more than welcome to ask for help.

#Of course, these are only the very basics. You are strongly encouraged to play around with these ideas on your own. The only way to get better are writing code is to write code. So deep. So true.

### Logistics Feel free to collaborate on this assignment. Each person should submit a final assignment as a PDF which represents their own work. When submitting your assignment, please make it clear which question each response is intended to address.  Also, make sure to include R code with clear commenting at the end of your assignment. TFs will use this to identify the source of any errors you may have made. If they're not able to follow the code, they won't be able to find the mistakes. In this particular assignment, the TFs may need to run your code to get an interactive visualization, so be absolutely sure to include it.

#### Background

#As we've seen, the gapminder dataset has measurements on a ton of variables for countries around the world. In this particular analysis, we're not exploring any particular hypotheses, we're just looking at the data. As we'll see, a big part of data science involves exploring data visually.

library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
#### Step 1 We're going to do a few things here to make the data a little easier to work with. One frustrating thing about this dataset is how long some of the variable names are. Prior to doing anything else, we want to rename some of them. Run the following code after reading in the gapminder dataset (you need to first load the `dplyr` package): `gapminder <- 


# basic pattern is new_name = old_name; overwrite gapminder with the renamed variables`. This #will rename, e.g., `life_expectancy_years` to `life_exp`.

#Next, create an `income_level` variable as we did before, with breaks at \$2,000, \$5,000, \$15,000, and \$25,000 (that is, 0-2,000, 2,000-5,000, etc...), and with labels that you think are appropriate.

#Finaly, create a continent variable and a country variable, just like we did in class.

gapminder <- read.csv('https://www.dropbox.com/s/pylowxqa6ubi2h3/gapminder.csv?dl=1') 
# we'll be using data from www.gapminder.org for plotting. I downloaded these data from https://github.com/syntagmatic/gapminder-csv/blob/master/gapminder.csv. Click "download", then save the page somewhere on your computer with the extension ".csv". This is a fairly big dataset, so it's a good idea for you to save a copy on your computer to read in directly
gapminder <- dplyr::rename(gapminder, year = time, gini = gapminder_gini, life_exp = life_expectancy_years, gni = gnipercapita_ppp_current_international) 
# basic pattern is new_name = old_name;
gapminder$income_level <- cut(gapminder$gni, breaks = c(0, 2000, 5000, 15000, 25000, 2000000), labels = c('Very Low', 'Low','Medium' ,'High', 'Very High'))
table(gapminder$income_level) # this variable measures the income level of a country

library(countrycode)
# the gapminder country codes are stored as ISO3C country codes (most of them. A handful, like Northern Cyprus, and North and South Yemen, have non-standard codes)
gapminder$continent <- countrycode(toupper(gapminder$geo), 'iso3c', 'continent')
table(gapminder$continent)
unique(gapminder$geo)

library(ggthemes)
#### Step 2 Using `ggplot`, create a histogram of life expectancy for countries, using data from the year 2010. Use professional-looking labels and a title and use 40 bins. If you're up to the challenge, try to figure out how to add numbers above each bin indicating how many countries are in the bin.

ggplot(subset(gapminder,  year == 2010 &!is.na(continent)), 
       aes(x = life_exp)) + # modify the data to only include observations from the current year
  geom_histogram(bins = 40) + xlab("Life expectancy across continents")  + title("Life expectancy for countries") +stat_bin(bins = 40, geom = 'text', aes(y = ..count.. +1 ,label = ..count..), position =  position_stack(vjust = 1)) + theme_economist()


#### Step 3 Create density plots of the life expectency variable with separate plots based on income level. Make the fill depend on income level and make the fill partially transparent. Do not include a density plot for countries with missing income levels.

ggplot(subset(gapminder, year == 2010 & !is.na(continent) & !is.na(income_level)), 
       aes(x = life_exp,  fill = income_level)) + 
geom_density(alpha = 0.5) + xlab("Life Expectancy across continents") 

#### Step 3 Using `ggplot`, create a line-chart showing continent mean per-capita GNI against time. There should be one line for each continent and each line should have its own color and line-type (lty). Use professional-looking labels and do something with the colors (that is, don't just use the defaults).
library(dplyr)
gapminder$year

cont_dat <- gapminder %>% group_by(continent,year) %>% summarize(gni_val = weighted.mean(gni, na.rm = TRUE)) %>% na.omit()
density <- ggplot(cont_dat, aes(x = year, y = gni_val, color = continent, lty = continent, colors = 'accent'))  + geom_line() + xlim(1990, 2010) + xlab("Time") + ylab("Gross National Income") 

#### Step 4 Recreate the plot from step 3, except be sure that each continent has its own plotting region.
plot_density <- density + facet_wrap(~continent) 

#### Step 5 Create an interactive visualization using plotly. Create a scatterplot of life expectancy v.s gini-coefficient for countries from either Europe or the Americas based on data from 2005. Color the points by per-capita gni and use a color scheme other than the default. Make sure that the country name is displayed when you hover over a point. If possible, try to get plotly to also display the GNI of the country. Export the image to the working directory as a .html file. TFs will use your code to recreate this image so make sure the code works and include commenting!
library(plotly)
ggplotly(density)
ggplotly(plot_density)
selectedcontinents <- c('Europe')
subgapminder <- gapminder %>% filter(continent %in% selectedcontinents , year == 2005)
  giniplot <- plot_ly(subgapminder, x = ~life_exp, y = ~gini, color = ~gni, text = ~country, colors = 'Accent') %>% 
  layout(title = 'A plot of life expectancy against gini coefficient in Europe', xaxis = list(title = 'Life expectancy'), yaxis = list(title = 'Gini coefficient'))

library(htmlwidgets)
saveWidget(as_widget(giniplot), "C:/Users/LenovoT480s/Desktop/pset2visualization.html")



  
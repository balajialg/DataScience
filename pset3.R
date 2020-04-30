### Due date: 2/20/2020, by 11:59PM

#This document was created using [R Markdown](https://r4ds.had.co.nz/r-markdown.html), an extension of R that we'll learn later in the term. I'll post both the final version and the code used to create it.

### Purpose of the assignment

#In this pset, we'll be doing basic data cleaning and manipulation. Cool things like filtering, summarizing, renaming, recoding, selecting, etc.... I'm not gonna lie to you, this stuff is not as glamorous and exciting as making interactive choropleths. But, it's worth learning because

#1. In your life as a data analyst/PhD student, you will spend a *lot* more time doing this than you will fitting and interpreting models;
#2. Learning how to do this stuff effectively will make you *so* much more efficient and accurate than you would otherwise be; and
#3. You have to because you're in this class (although you still have a day to drop it).

### Logistics

#Feel free to collaborate on this assignment. Each person should submit a document representing their own work. When submitting your assignment, please make it clear which question each response is intended to address.  Also, make sure to include R code with clear commenting at the end of your assignment. TFs will use this to identify the source of any errors you may have made. If they're not able to follow your code, they won't be able to find the mistakes.

#### Background

#We'll be working with data from the Cooperative Congressional Election Survey (CCES), just like in the week 3 exercises. As you'll recall, the CCES has data on a large sample of 60,000 respondents from the US. We'll use R to organize these data and produce simple summaries as well as some plots. Inthis assignment, please try to use the tools we've introduced, including the pipe operator (`%>%`) as much as you can. There are always going to be multiple ways to do any of these things, but we want you to practice with the most powerful tools at your disposal.

#### Step 1

#As in the previous assignment, we need to do some setup before we begin. Use the codebook to find the variable regarding respondents' attitudes toward state spending on education. Rename it "state_ed" and reverse it so that higher values indicate more support for spending. Find the variable describing whether the respondent has a child under the age of 18. Rename it "has_child", and make it a labeled factor with levels of "No child" and "Has child". Rename the variable "countyname" as "county". (feel free to do all of the renaming in a single line of code; use the `%<>%` operator as much as possible).

#Import the required libraries that will help perform data manipulation 
library(dplyr)
library(magrittr)
library(stringr)

#Load the data from dropbox and store it as part of a variable
load(url('https://www.dropbox.com/s/k8xh3slsjwardaf/cces18_common_vv.RData?dl=1')) # any R objects can be saved as .Rdata files and then loaded using the load function
cces <- x
cces$state_ed <- cces$CC18_426_3
#Reverse the coding by subtracting values from 6 for all values so that higher the state_ed values indicates openness to higher education spending
cces$state_ed <- 6 - cces$state_ed 
summary(cces$state_ed)

#Refactor the child18 variable by labeling values 1 as Has child and 2 as No Child 
cces$has_child <- cces$child18 %>%factor(levels = (2:1), labels = c('No Child','Has Child'))
#Rename the countyname variable as county
cces %<>% rename(county = countyname)

#### Step 2
#Find the 5 counties with the greatest and least support for increasing spending on education among respondents who have children under the age of 18. As a challenge, try to only consider counties with at least 25 people in the sample. As an extra challenge, try to only consider counties with at least 25 *non-missing* responses. Here's some code to consider for the challenges:

#Group the values by county using groupby method, filter the values for people with child using filter method and find the mean of state education spending at county level using the summarize method. Pass the output from one method as an input for the next method using dplyr method %>%
county_list <- cces %>% group_by(county) %>% filter( has_child == 'Has Child', !is.na(county), !is.na(state_ed)) %>% summarize(mean_state = mean(state_ed, na.rm = TRUE), length_county = length(county), sum_county = sum(!is.na(state_ed))) %>% filter(length_county > 25, sum_county > 25) %>% arrange((mean_state)) 


#### Step 3
#Use tidycensus to download Census data on median incomes in various counties. The code below will accomplish that (but you'll also need to bring in code from lecture).

library(tidycensus)
#Store the individual census key from Census website. Pass the census key as part of census_api_key method to overwrite the existing key. Use the get_acs() to get the census data B06011_001 at a county level for the year 2018
census_key <- '94800a10e8b3f88accfbe645033db301a2dc9c7a'
census_api_key(census_key, overwrite = TRUE, install = TRUE)
census_dat <- get_acs(geography = 'county', variables = 'B06011_001', year = 2018, output = 'wide', geometry = TRUE,shift_geo = TRUE)
#Rename the census_dat variable with meaningful names
census_dat %<>% rename(county = NAME)
census_dat %<>% rename(countyfips = GEOID)
#Make the countyfips variable as part of census data a numeric variable
census_dat$countyfips %<>% as.numeric()

#Then collapse cces up to the county level and calculate the mean of state_ed in each county (use countyfips as a grouping variable; make sure to also capture county name and region). Merge this with the census dat. Use an inner join; we'll only be able to work with counties which appear in both datasets.

#Do an inner join between cces dataset and census dataset and use group_by() to identify the final output
cces_state <- cces %>% group_by(countyfips, region) %>% summarize(Education_Spending = mean(state_ed, na.rm = TRUE)) %>% inner_join(census_dat) %>% arrange(desc(Education_Spending))
summary(cces_state$Education_Spending)




#### Step 4

#Use plotly to create a scatter of mean support for educational spending against median income at the county level for counties in the south (`where region == 3`). If possible, make it an interactive plot which allows the user to identify the county by mousing over. (when doing this I ran into an absurd number of problems, so if you're encountering inexpicable errors, please let us know).

#Use the plotly library to create a plot between Median Income against the Mean Educational Spending 
library(plotly)
plot <- plot_ly(subset(cces_state, region == 3), x = ~B06011_001E, y = ~Education_Spending, color = ~Education_Spending, colors = 'Accent', text = ~paste0(county)) %>% layout(title = 'A plot of Peoples attitude to Education Spending against Median Income', xaxis = list(title = 'Median Income'), yaxis = list(title = 'Mean Education Spending'))

#Save the created plot at a specified location as .html file for future access
library(htmlwidgets)
saveWidget(as_widget(plot), "C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/Spring/S022 - Introduction to Data Science/Assignments/Assignment 3/pset3visualization.html")


#### Challenges

#Here are some other things you should try. They're not graded, but they're good practice.

#1. Create a dataset which tracks the mean *difference* in support for increased educational spending between people who have children under than age of 18 and those who don't. Identify the counties with the largest gaps.

#2. Create a variable which measures the mean conservatism (or liberalism) in each county. Scatter mean support for educational spending in counties v.s. mean conservatism#.

#3. Create a choropleth showing mean support for educational spending at either a state, county, or zip-code level.



matrix_mod <- matrix(NA, ncol = 2, nrow = 1)
rownames(matrix_mod) <- c('Regression of Age on Sex')
colnames(matrix_mod) <- c('RSquare','Slope')

matrix_mod2 <- matrix(NA, ncol = 2, nrow = 1)
rownames(matrix_mod2) <- c('Regression of Age on School')
colnames(matrix_mod2) <- c('RSquare','Slope')

matrix_mod <- matrix(NA, ncol = 2, nrow = 1)
rownames(matrix_mod) <- c('Regression of Age on Sex')
colnames(matrix_mod) <- c('RSquare','Slope')

matrix_mod2 <- matrix(NA, ncol = 2, nrow = 1)
rownames(matrix_mod2) <- c('Regression of Age on School')
colnames(matrix_mod2) <- c('RSquare','Slope')

matrix_mod['Regression of Age on Sex',1 ] <- mod$r.squared
matrix_mod['Regression of Age on Sex',2 ] <- mod$coefficients[2]

mod2 <- my_mod(lm(pums_sub$wagp ~ pums_sub$schl), pums_sub, puma.this = pumas)
matrix_mod2['Regression of Age on School', 1] <- mod2$r.squared
matrix_mod2['Regression of Age on School',2] <- mod2$coefficients[2]

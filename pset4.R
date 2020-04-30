
### Due date: 2/27/2020, by 11:59PM This document was created using [R Markdown](https://r4ds.had.co.nz/r-markdown.html), an extension of R that we'll learn later in the term. I'll post both the final version and the code used to create it.

### Purpose of the assignment In this pset we'll be writing loops and functions in R. These will make it our code more efficient and easier to maintain.. These are challenging skills, but ultimately very much worth learning. In many of these situations below you could accomplish something similar without a for loop or without writing your own functions. But do it anyway.

### Logistics Feel free to collaborate on this assignment. Each person should submit a document representing their own work. When submitting your assignment, please make it clear which question each response is intended to address.  Also, make sure to include R code with clear commenting at the end of your assignment. TFs will use this to identify the source of any errors you may have made. If they're not able to follow your code, they won't be able to find the mistakes.

#### Background We'll be working with data from Public Use Microsystem (PUMS) from the US Census. These contain a variety of information from American households and individuals. We're interested in making the data prep and analysis easier by writing our own functions and loops.

#### Step 1  We need to read in the data before we begin. I want to use data from the 2016 and 2017 ACS, focusing on responses from states in the American West. These are Alaska (AK), Arizona (AZ), California (CA), Colorado (CO), Hawaii (HI), Idaho (ID), Montana (MT), Nevada (NV), New Mexico (NM), Oregon (OR), Utah (UT), Washington (WA), and Wyoming (WY). Write a function which takes the state abbreviation as an argument and downloads and unzips the files from all three years. Use a for-loop and this function to download and unzip all of the files; make sure that the files from the same state don't overwrite each other. As a challenge, consider also passing the year as an argument and writing a nested for loop (a for loop inside a for loop, looping over both the years and the states to process all of the files). Alternately, try to make this a function that would work on someone else's computer: add an argument which allows the user to pass the file path so that the file can be downloaded to the correct drive. Make the default the location on your computer.After downloading and unzipping the data, read in the data from the various states and put them together into a single dataframe. As a response, submit the code you wrote to accomplish the downloading and the reading in.


library(stringr)
library(dplyr)
library(magrittr)
library(plotly)
library(ggplot2)


states <- c('id', 'mt', 'nv', 'nm', 'or', 'ut', 'wa', 'wy','ca','az','co', 'hi', 'ak') # western states

years <- c(2016, 2017)

#downloadpums method creates a directory and downloads files from census data, unzips it and deletes the zip file
download_pums <- function(my_dir, url.this, year.this, state.this){
  file.path(my_dir,year.this)
  dir.create(file.path(my_dir,year.this))
  download.file(url = url.this, destfile = paste0(my_dir, '/',year.this, '/',state.this, '.zip'))
  unzip(zipfile = paste0(my_dir,'/',year.this, '/', state.this,'.zip'),
        exdir = paste0(my_dir, '/', year.this))
  unlink(paste0(my_dir,'/',year.this,'/',state.this,'.zip'))
}

#Save the working directory for future access
my_dir <- 'C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/Spring/S022 - Introduction to Data Science/Pset4'

#Call the download pums method by looping across state and year variables to get the relevant files
for(state.this in states){ # loop over the states    # use download_pums in here
  for (year.this in years){
    download_pums(my_dir, url = paste0('https://www2.census.gov/programs-surveys/acs/data/pums/',year.this,'/1-Year/csv_p',state.this,'.zip'), year = year.this, state = state.this)
  }}

#Search for all the files in a directory. Search for .CSV files in the directories created seperately for 2016 and 2017
all_files <- c(dir(paste0(my_dir,'/',2016)), dir(paste0(my_dir,'/',2017)) )
all_files <- all_files[str_detect(all_files, '\\.csv')]

all_files_2016 <- c(dir(paste0(my_dir,'/',2016)[str_detect(all_files_2016, '\\.csv')])) 
all_files_2016 <- all_files_2016[str_detect(all_files_2016, '\\.csv')]

all_files_2017 <- c(dir(paste0(my_dir,'/',2017))) 
all_files_2017 <- all_files_2017[str_detect(all_files_2017, '\\.csv')]

states_list <- list()


# Read the files from respective folder and store it as part of a list
for(state_file in all_files_2016)
{
  states_list[[state_file]] <- read.csv(paste0(my_dir,'/',2016, '/', state_file))
  }

for(state_file in all_files_2017)
{
  states_list[[state_file]] <- read.csv(paste0(my_dir,'/',2017, '/', state_file))
}

# put the list into a single dataframe called pums
pums <- bind_rows(states_list)
head(pums)
#Conver the case from upper to lower characters   
names(pums) %<>% tolower()

#### Step 2 Find the five PUMS areas (in this subset) with the highest mean salary/wages for women. Use the PUMA variable to define the region. Then find the five PUMS areas with the highest ratio of mean salaries for women to mean salaries for men, and the five with the highest ratio of mean salaries for men to mean salaries for women (you can find the mean wage for men with `mean(wagp[sex == 1])`). Use data from both years. This step is testing dplyr skills and will probably not involve a for loop or a new function. To make this easier, take a subset of the PUMS which only includes the variables we mention in this pset; run the code `pums_sub <- pums[c('sex', 'st', 'wagp', 'puma', paste0('mlp', c('a', 'b', 'cd', 'e', 'fg', 'h', 'i', 'j', 'k')))]`.Submit the code and the PUMAs.

#Create a dataset to store all the relevant variables 
pums_sub <- pums[c('sex', 'schl', 'st', 'wagp', 'puma', paste0('mlp', c('a', 'b', 'cd', 'e', 'fg', 'h', 'i', 'j', 'k')))]
summary(pums_sub$puma)
table(pums_sub$sex)
#Calculate the mean wage, group it by puma values and arrange it in the descending order
pums_sub %>% group_by(puma) %>% summarize(mean_wage = mean(wagp[sex == 1], na.rm = TRUE)) %>% arrange(desc(mean_wage)) 

#Calculate the men_women_ratio by dividing the  mean wage of men by mean wage of women and storing it in a seperate variable by arranging it in descending order
Men_Women_Ratio <- pums_sub %>% group_by(puma ) %>% summarize(mean_wage_men = mean(wagp[sex == 1], na.rm = TRUE), mean_wage_women = mean(wagp[sex == 2], na.rm = TRUE), ratio_men_women = mean_wage_men/mean_wage_women) %>% arrange(desc(ratio_men_women))

#Calculate the women_men_ratio by dividing the  mean wage of women by mean wage of men and storing it in a seperate variable by arranging it in descending order
Women_Men_Ratio <- pums_sub %>% group_by(puma,st) %>% summarize(mean_wage_men = mean(wagp[sex == 1], na.rm = TRUE), mean_wage_women = mean(wagp[sex == 2], na.rm = TRUE), ratio_women_men = mean_wage_women/mean_wage_men) %>% arrange(desc(ratio_women_men)) 

# group the dataset by puma, requested means and ratios, and order them
# will require a couple of lines of code

#### Step 3 A number of variables ask about a respondent's military service during various periods of time (MLPA:MLPK). Use a for loop to create a factor version of each of these (i.e., create a MLPA_fac which has levels of "Did not serve this period" and "Served this period"). Count people with missing values as not having served (consider code along the lines of `dat[[var]][is.na(dat[[var]])] <- "some value"`). Then use a for loop to create a matrix where the rows are different periods and the columns are the mean ages of those who did and did not serve in that period. If you're using `group_by()` to accomplish this, you can use the `group_by_at()` function, which will accept a column name as a string. You may also find it helpful to 1. extract the desired column after summarizing (using the select function), and 2. converting it into a matrix (using as.matrix()). This will make it easier to store the information in a column.

#Store all the required variables
var_military <- c('mlpa','mlpb','mlpcd','mlpe', 'mlpfg', 'mlph', 'mlpi', 'mlpj', 'mlpk' )

#Get the subset of columns which are required and store in the pums_military subset 
pums_military <- pums[c('mlpa','mlpb','mlpcd','mlpe', 'mlpfg', 'mlph', 'mlpi', 'mlpj', 'mlpk', 'agep' )]
pums_military['mlpa']

#Loop over the variables and refactor the values in the dataset to Served or Not served and refactor NA to not served 
for (var in var_military){
  pums_military[var][is.na(pums_military[var])] <- 0
  pums_military[[var]] %<>% factor(levels = 0:1, labels = c('Did not serve this period','Served this period'))
}
# make a vector storing all of the military service variables
my_tab <- matrix("NA", ncol = 2, nrow = length(var_military))
rownames(my_tab) <- var_military
colnames(my_tab) <- levels(pums_military$mlpa)

#Adding a temporary statement to verify the working of the code 

#Loop over the values and store the mean values in a vector which gets stored in a matrix my_tab

for(var in var_military)
{
meanage <- pums_military %>% filter(pums_military[var] == 'Did not serve this period') %>% summarize(mean_age_notser = mean(agep , na.rm = TRUE)) %>% select(mean_age_notser)
my_tab[var, 1] <- meanage$mean_age_notser

meanage <- pums_military %>% filter(pums_military[var] == 'Served this period') %>% summarize(mean_age_ser = mean(agep,na.rm = TRUE)) %>% select(mean_age_ser)
my_tab[var,2] <- meanage$mean_age_ser                                                
}
  
#Submit the matrix/table and your code
 
#### Step 4  Write a function which takes a dataset and a formula (for a linear regression; something like y ~ x) as arguments. The function should output the R^2 from the regression (`summary(mod)$r.squared`). Loop over the various PUMAs in the dataset (`unique(pums_sub$puma)`), fitting the models WAGP ~ SEX and WAGP ~ SCHL in each one. Create two matrices, one storing the slopes and R^2s from the regression on SEX and one storing the same information from the regression on SCHL. Create a scatterplot of the R^2 values from the model with SEX on the R^2 values from the model with SCHL. If you can, make this plot interactive and have the PUMA label appear when you mouse over a point.Here is some code to get you started. You do not need to use it, but you may find it helpful.

#Function to calculate the Rsquare values for shared regression formula
my_mod <- function(formula, data = pums_sub, puma.this)
{
  data <- data %>% filter(puma == puma.this)
  rsquared <- summary(lm(formula, data))$r.squared
  return (rsquared)
}

#Create a dataframe to store the Rsquare values from the varied regression model
puma = unique(pums_sub$puma)

my_ests_sex <- data.frame(puma = unique(pums_sub$puma), sex_r2 = NA)
my_ests_schl <- data.frame(puma = unique(pums_sub$puma), school_r2 = NA)
reg_rsquare <- data.frame(puma = unique(pums_sub$puma), sex_r2 = NA, school_r2 = NA)

#Loop over the regression method to calculate regression values of wage against sex and wage against school and store it in the dataframe reg_rsquare
for (i in 1:length(puma))
{
my_ests_sex$r2[i] <- my_mod(wagp ~ sex, pums_sub, puma.this = puma[i])
my_ests_schl$r2[i] <- my_mod(wagp ~ schl, pums_sub, puma.this = puma[i])
reg_rsquare$sex_r2[i] <- my_ests_sex$r2[i]
reg_rsquare$school_r2[i] <- my_ests_schl$r2[i]
}

#Create a ggplot which takes the rsquare values for wage ~sex and wage ~ school
ggp <- ggplot(reg_rsquare, aes(x = sex_r2, y = school_r2)) + geom_point() + xlab("Regression of Wage on Sex") + ylab("Regression of Wage on School")

#Generate an interactable plot by calling the plotly method 
plot <- plot_ly(reg_rsquare, x = ~sex_r2, y = ~school_r2, color = ~sex_r2, colors = 'Accent') %>% layout(title = 'Scatterplot between Regression of Wage on Sex and Wage on School ', xaxis = list(title = 'Regression of Wage on Sex'), yaxis = list(title = 'Regression of Wage on Sex'))
plot

```


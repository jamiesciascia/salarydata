# LOADING DATA

# install and load all necessary packages
install.packages("googlesheets4")
library(googlesheets4)
library(tidyverse)
library(scales)
library(RColorBrewer)

# read data from google sheets
salary_survey <- read_sheet("1IPS5dBSGtwYVbjsfbaMCYIWnOuRmJcbequohNxCyGVw")

#CLEANING DATA

# rename columns and create data frame for clean data
salary_survey_clean <-
    rename(salary_survey,
      datetime = `Timestamp`,
      age = `How old are you?`,
      industry = `What industry do you work in?`, 
      job_title = `Job title`, 
      job_title_context = `If your job title needs additional context, please clarify here:`,
      annual_salary = `What is your annual salary? (You'll indicate the currency in a later question. If you are part-time or hourly, please enter an annualized equivalent -- what you would earn if you worked the job 40 hours a week, 52 weeks a year.)`,
      additional_salary = `How much additional monetary compensation do you get, if any (for example, bonuses or overtime in an average year)? Please only include monetary compensation here, not the value of benefits.`,
      currency = `Please indicate the currency`,
      other_currency = `If "Other," please indicate the currency here:`,
      income_context = `If your income needs additional context, please provide it here:`,
      country = `What country do you work in?`,
      us_state = `If you're in the U.S., what state do you work in?`,
      city = `What city do you work in?`,
      years_professional_experience = `How many years of professional work experience do you have overall?`,
      years_field_experience = `How many years of professional work experience do you have in your field?`,
      education = `What is your highest level of education completed?`,
      gender = `What is your gender?`,
      race = `What is your race? (Choose all that apply.)`
      ) %>%
  select(datetime, age, industry, job_title, job_title_context, annual_salary, additional_salary, currency, other_currency, income_context, education, gender)

# change data types
salary_survey_clean$additional_salary[salary_survey_clean$additional_salary == 'NULL'] <- 0
salary_survey_clean$other_currency[salary_survey_clean$other_currency == 'NULL'] <- 'NULL'
salary_survey_clean$additional_salary <- as.numeric(unlist(salary_survey_clean$additional_salary))
salary_survey_clean$other_currency <- as.character(unlist(salary_survey_clean$other_currency))

# add total salary and unique id columns, filter out bad data
salary_survey_clean <- salary_survey_clean %>%
  mutate(total_salary = annual_salary + additional_salary, 
         id = row_number()) %>%
  filter(! id %in% c(3606, 28022), age != 'under 18', education != is.na(education))

# cleaning/categorizing currency
salary_survey_clean$other_currency <- toupper(salary_survey_clean$other_currency)
salary_survey_clean$other_currency[salary_survey_clean$other_currency %in% c('NA', 'N/A', 'EQUITY')] <- 'NULL'
salary_survey_clean$currency <- ifelse(salary_survey_clean$currency == 'Other', salary_survey_clean$other_currency, salary_survey_clean$currency)
salary_survey_clean$currency[salary_survey_clean$currency %in% c('AMERICAN DOLLARS', 'US DOLLAR')] <- 'USD'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('EURO')] <- 'EUR'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('NZD', 'AUD', 'AUD AUSTRALIAN', 'AUSTRALIAN DOLLARS')] <- 'AUD/NZD'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('INDIAN RUPEES', 'INR (INDIAN RUPEE)', 'RUPEES')] <- 'INR'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('NORWEGIAN KRONER (NOK)')] <- 'NOK'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('DANISH KRONER')] <- 'DKK'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('CZECH CROWNS')] <- 'CZK'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('PHILIPPINE PESO', 'PHILIPPINE PESO (PHP)', 'PHP (PHILIPPINE PESO)', 'PHILIPPINE PESOS')] <- 'PHP'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('BR$', 'BRL (R$)')] <- 'BRL'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('TAIWANESE DOLLARS')] <- 'TWD'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('NIS (NEW ISRAELI SHEKEL)')] <- 'NIS'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('SINGAPORE DOLLARA')] <- 'SGD'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('THAI BAHT', 'THAI  BAHT')] <- 'THB'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('CHINA RMB', 'RMB (CHINESE YUAN)')] <- 'RMB'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('PESO ARGENTINO', 'ARGENTINIAN PESO (ARS)', 'ARGENTINE PESO')] <- 'ARS'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('ILS (SHEKEL)', 'ISRAELI SHEKELS')] <- 'ILS'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('MEXICAN PESOS')] <- 'MXN'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('PLN (POLISH ZLOTY)', 'POLISH ZŁOTY', 'PLN (ZWOTY)')] <- 'PLN'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('KRW (KOREAN WON)', 'KOREAN WON')] <- 'KRW'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('CROATIAN KUNA')] <- 'HRK'
salary_survey_clean$currency[salary_survey_clean$currency %in% c('RM')] <- 'MYR'

# cleaning/categorizing 'other' industry
salary_survey_clean$industry[! salary_survey_clean$industry %in% 
                               c('Accounting, Banking & Finance', 
                                 'Agriculture or Forestry',
                                 'Art & Design',
                                 'Business or Consulting',
                                 'Computing or Tech',
                                 'Education (Primary/Secondary)',
                                 'Education (Higher Education)',
                                 'Engineering or Manufacturing',
                                 'Entertainment',
                                 'Government and Public Administration',
                                 'Health care',
                                 'Hospitality & Events',
                                 'Insurance',
                                 'Law',
                                 'Law Enforcement & Security',
                                 'Leisure, Sport & Toursim',
                                 'Marketing, Advertising & PR',
                                 'Media & Digital',
                                 'Nonprofits',
                                 'Property or Construction',
                                 'Recruitment or HR',
                                 'Retail',
                                 'Sales',
                                 'Social Work',
                                 'Transport or Logistics',
                                 'Utilities & Telecommunications')] <- 'Other'

# create data frame filtered for USD
salary_survey_usd <- salary_survey_clean %>%
  filter(currency == 'USD')

# VISUALIZING DATA

# creating color palette
mycolors <- c("#8DD3C7", "#BFE6BE", "#F1F9B5", "#EAE8BF", "#CDCAD0", "#CAAEC5", "#E59497", "#F18379",
              "#BB99A4", "#84AFCF", "#B2B2A5", "#E8B374", "#E8BF63", "#C7D267", "#BEDB7C", "#DED3B3",
              "#FACDE4", "#EBD2DF", "#DBD8D9", "#CEB8CE", "#C191C2", "#BF99BE", "#C6C8C2", "#D2EBBA",
              "#E8EC94", "#FFED6F")

# industry
salary_survey_usd %>% 
  group_by(industry) %>% 
  summarize(averageUSD = mean(total_salary), min = min(total_salary), max = max(total_salary)) %>%
  ggplot(aes(x=averageUSD, y=reorder(industry, averageUSD), fill = industry)) + 
  geom_col() + 
  labs(title = "US Salary by Industry",
       x = "Average Yearly Salary",
       y = "Industry") +
  geom_text(aes(label = scales::dollar(round(averageUSD))), hjust=-.2, size = 5) +
  theme(plot.title = element_text(hjust = .5), legend.position = "none", text = element_text(size = 15)) +
  scale_x_continuous(labels = dollar_format(), expand = expansion(mult=c(0,.2))) +
  scale_fill_manual(values = mycolors)

# age  
salary_survey_usd %>% 
  group_by(age) %>% 
  summarize(averageUSD = mean(total_salary), min = min(total_salary), max = max(total_salary)) %>%
  ggplot(aes(x=averageUSD, y=age, fill=age)) + 
    geom_col() + 
    labs(title = "US Salary by Age",
         x = "Average Yearly Salary",
         y = "Age") +
    geom_text(aes(label = scales::dollar(round(averageUSD))), hjust=1.2, size = 10) +
    theme(plot.title = element_text(hjust = .5), legend.position = "none", text = element_text( size = 30)) +
    scale_x_continuous(labels = dollar_format(), expand = expansion(mult = c(0, .1))) +
    scale_fill_manual(values = mycolors)

# education
ranking <- c(3,1,4,5,6,2)

salary_survey_usd %>% 
  group_by(education) %>% 
  summarize(averageUSD = mean(total_salary), min = min(total_salary), max = max(total_salary), median = median(total_salary)) %>%
  mutate(ranking) %>%
  ggplot(aes(x=averageUSD, y=reorder(education, ranking), fill = education)) + 
    geom_col() + 
    labs(title = "US Salary by Edcuation Level",
         x = "Average Yearly Salary",
         y = "Education Level") +
    geom_text(aes(label = scales::dollar(round(averageUSD))), hjust=1.2, size = 10) +
    theme(plot.title = element_text(hjust = .5), legend.position = "none", text = element_text(size = 30)) +
    scale_x_continuous(labels = dollar_format(), expand = expansion(mult = c(0, .1))) +
    scale_y_discrete(labels = label_wrap(10)) +
    scale_fill_manual(values = mycolors)


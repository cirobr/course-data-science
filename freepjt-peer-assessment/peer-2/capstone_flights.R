###########################################################
# Capstone. Exercise 2. Airport Delays from flightradar.com
###########################################################

###########################################################
# Preparing the packages & Libraries in R (if required)
###########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(stringr)
library(recosystem)
library(kableExtra)
library(readxl)


#########################################################
# Reading the Excel File from datascrapping
#########################################################

BCN_September_2019 <- read_excel("C:/Users/Jordi Candela/Desktop/Edx_CP_Data Science_Harvard/9.- PH125.9x Data Science. Capstone/2.- own project/BCN_September_2019.xlsx")

head(BCN_September_2019)

#########################################################
# Data Tyding
#########################################################

# omiting NA values
BCN_September_2019<- na.omit(BCN_September_2019)

#########################################################
# Understanding Data
#########################################################

# Behaviour per airlines and delays
BCN_September_2019_airline <- BCN_September_2019 %>%
  group_by(AIRLINE) %>% 
  summarise (mean_delay = mean (fr24_dd), n=n())

# A lot of dispersion from many players, thus is required to filter
# Filter of Airlines with more than 100 monthly departures
BCN_September_2019_airline <- filter(BCN_September_2019_airline, n >= 100)
BCN_September_2019_airline


#Obtaining the major airlines with better performance
top_airlines_arranged <- arrange(BCN_September_2019_airline, mean_delay)
top_airlines_arranged
                 
#Barplot arplot delay of TOP airlines (n > 100 monthly departures)
top_airlines_arranged %>%
  ggplot(aes(AIRLINE, mean_delay))+
  geom_col(fill="lightblue") +
  labs(title="Barplot of delay per airline")+
  geom_text(aes(label = signif(mean_delay, digits = 2)), nudge_y = 4)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 


# Behavior of TOP3 Airlines
BCN_September_2019_TOP3 <- filter(BCN_September_2019, AIRLINE %in% c("easyJet", "Ryanair", "Vueling"))

BCN_September_2019_TOP3 %>%
  ggplot(aes(x=AIRLINE, y=fr24_dd, fill=AIRLINE)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0,250))+
  geom_jitter(color="darkblue", size=0.2, alpha=0.05) +
  theme(
    legend.position="none",
    plot.title = element_text(size=10)
  ) +
  ggtitle("TOP 3 Airlines Delays Boxplot") +
  xlab("") +
  ylab("delay in min")  


# Evolution of delays within the day

dailydelay <- ggplot(BCN_September_2019, aes(x=ATD, y= fr24_dd)) +
  geom_point(
    color="lightblue",
    fill="#69b3a2",
    shape=21,
    alpha=0.3,
    size=0.1,
    stroke = 2
  )+
  geom_smooth(method=lm , color="red", se=FALSE) +
  labs(x = "Hour",y = "Delay")+
  theme(axis.title.x = element_text(vjust=-0.5, size=rel(1))) +
  theme(axis.title.y = element_text(vjust=1.5, size=rel(1)))
  
dailydelay


# Evolution of delays within the day without considering early morning/late night
BCN_September_2019_nomorning <- cbind(BCN_September_2019, hour = hour(BCN_September_2019$ATD))

BCN_September_2019_nomorning <- BCN_September_2019_nomorning %>% filter (BCN_September_2019_nomorning$hour >= 6)

head(BCN_September_2019_nomorning)
str(BCN_September_2019_nomorning)



########################################################
# Implementation of 2 Linear Regression Models
#######################################################

### Linear regressions 
# Linear regression delay vs hour without considering early morning / late night

lmdelay = lm(fr24_dd ~ hour, data = BCN_September_2019_nomorning) 
summary(lmdelay) 

# Plot of Delays and Linear regression delay of departures vs hour without considering early morning / late night
dailydelay_nomorning <- ggplot(BCN_September_2019_nomorning, aes(x=ATD, y= fr24_dd)) +
  geom_point(
    color="darkblue",
    fill="#69b3a2",
    shape=21,
    alpha=0.3,
    size=0.1,
    stroke = 2
  )+
  geom_smooth(method=lm , color="red", se=FALSE) +
  labs(x = "Hour",y = "Delay")+
  theme(axis.title.x = element_text(vjust=-0.5, size=rel(1))) +
  theme(axis.title.y = element_text(vjust=1.5, size=rel(1)))

dailydelay_nomorning


### Linear regression 2
# Being Vueling the #1 Airlines of the Airport and having its major base in BCN Airport
# makes sense de deploy a particular
# Linear regression delay of Vueling vs hour without considering early morning / late night
BCN_September_2019_nomorning_vy <- BCN_September_2019_nomorning %>% filter (BCN_September_2019_nomorning$AIRLINE %in% c("Vueling"))
lmdelay_vy = lm(fr24_dd ~ hour, data = BCN_September_2019_nomorning_vy) 
summary(lmdelay_vy) 

# Plot of Delays and Linear regression delay of Vueling vs hour without considering early morning / late night
dailydelay_nomorning_vy <- ggplot(BCN_September_2019_nomorning_vy, aes(x=ATD, y= fr24_dd)) +
  geom_point(
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.3,
    size=0.1,
    stroke = 2
  )+
  geom_smooth(method=lm , color="red", se=FALSE) +
  labs(x = "Hour",y = "Delay")+
  theme(axis.title.x = element_text(vjust=-0.5, size=rel(1))) +
  theme(axis.title.y = element_text(vjust=1.5, size=rel(1)))

dailydelay_nomorning_vy


########################################################
# Implementation of a Machine Learning Algorithm
#######################################################

# Preparation of a Machine Learning Dataset (Test 10%)
# Validation set will be 10% of BCN_September_2019 data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(BCN_September_2019_nomorning$fr24_dd, p = 0.1, list = FALSE)
tester_bcn <- BCN_September_2019_nomorning[-test_index,]
validation_bcn <- BCN_September_2019_nomorning[test_index,]


### Simple Naive Prediction based on Mean Delay
mu <- mean(tester_bcn$fr24_dd)
mu
rmse_naive <- RMSE(tester_bcn$fr24_dd, mu)
rmse_naive

# Create a Data Frame and Save Results 
rmse_results = data_frame(method = "Naive Analysis by Mean", RMSE = rmse_naive)
rmse_results %>% knitr::kable()


### Simple model taking into account the airline effects (b_i)
mu <- mean(tester_bcn$fr24_dd)
airline_avgs <- tester_bcn %>%
  group_by(AIRLINE) %>%
  summarise(b_i = mean(fr24_dd - mu))
predicted_delays <- mu + validation_bcn %>%
  left_join(airline_avgs, by='AIRLINE') %>%
  pull(b_i)
rmse_model_airline_effects <- RMSE(predicted_delays, validation_bcn$fr24_dd)
rmse_model_airline_effects
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Airline Effects Model",
                                     RMSE = rmse_model_airline_effects))
rmse_results %>% knitr::kable()


### Simple model taking into account the hour effect (b_h)
mu <- mean(tester_bcn$fr24_dd)
airline_avgs <- tester_bcn %>%
  group_by(hour) %>%
  summarise(b_h = mean(fr24_dd - mu))
predicted_delays <- mu + validation_bcn %>%
  left_join(airline_avgs, by='hour') %>%
  pull(b_h)
rmse_model_hour_effects <- RMSE(predicted_delays, validation_bcn$fr24_dd)
rmse_model_hour_effects
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Hour Effects Model",
                                     RMSE = rmse_model_hour_effects))
rmse_results %>% knitr::kable()


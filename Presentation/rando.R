# Charles J. Arayata
# Healthcare Data Analyst Presentation
# Statistical Process Control on Brevet Finishing Times

setwd("C:/Users/carayata/Desktop/CHOP/SPC Resources")
rando <- read.csv("rando.csv", header=T, stringsAsFactors = F, na.strings = c("", "?"))

library(lubridate)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(gridExtra)

# Just 200k brevets
two.hundred <- rando[which(rando$Km == 200), ]

# Fixing Time and Dates
two.hundred$elapsed <- hm(two.hundred$Time)
two.hundred$Date <- mdy(two.hundred$Date)
two.hundred <- two.hundred[order(two.hundred$Date), ]

# Calculate Moving Range
two.hundred$lag <- lag(two.hundred$Time, n = 1)
two.hundred$lag.elapsed <- hm(two.hundred$lag)
two.hundred$moving.range <- abs(as.numeric(two.hundred$elapsed - two.hundred$lag.elapsed))/3600 # in hours

# Calculate Limits

# All in hours
x.mean <- mean(as.numeric(two.hundred$elapsed)/3600) # x-bar
sd.mean <- sd(as.numeric(two.hundred$elapsed)/3600) # standard deviation of x (not used)
mr.mean <- mean(two.hundred$moving.range, na.rm = T) # moving range mean


# From Control Chart Constants, d2 = 1.128, sample size 2 (taking differences from 2 consecutive ranges)
mr.mean/1.128 # 1.023022

# Control Limits = x-mean +/- [(moving range mean/d2) * moving range mean]
x.ucl <- x.mean + ((mr.mean/1.128)*mr.mean)
x.lcl <- x.mean - ((mr.mean/1.128)*mr.mean)



# Plot
one <- ggplot(two.hundred, aes(x = Date, y = as.numeric(elapsed)/3600), label = Date) +
  geom_hline(aes(yintercept = 13.5, colour = "red"), size = 1) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = Route), size = 3) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(8, 14, 0.5)) +
  coord_cartesian(ylim = c(8, 14)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Finishing Time (Hours)",
       x = "Ride Date") +
  guides(color = "none") +
  geom_hline(yintercept = mean(as.numeric(two.hundred$elapsed)/3600), linetype = 3) +

  geom_hline(yintercept = x.lcl, linetype = 5) +
  geom_hline(yintercept = x.ucl, linetype = 5) +
  annotate(geom="text", x = as.Date("2014-06-05"),
           y=13.7 ,label="200km Time Limit: 13.5hrs", fontface="bold") +
  annotate(geom="text", x = as.Date("2014-06-05"),
           y=8.3 ,label=paste0("Lower Control Limit: ", round(x.lcl, 1), "hrs")) +
  annotate(geom="text", x = as.Date("2014-06-05"),
           y=12.4 ,label=paste0("Upper Control Limit: ", round(x.ucl, 1), "hrs")) +
  ggtitle("X-Chart: Control Limits Using Moving Range")


# Calculate 95% Confidence Interval based on Standard Deviation of Observations
x.uci <- x.mean + (2*sd.mean)
x.lci <- x.mean - (2*sd.mean)

two <- ggplot(two.hundred, aes(x = Date, y = as.numeric(elapsed)/3600), label = Date) +
  geom_hline(aes(yintercept = 13.5, colour = "red"), size = 1) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = Route), size = 3) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(8, 14, 0.5)) +
  coord_cartesian(ylim = c(8, 14)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Finishing Time (Hours)",
       x = "Ride Date") +
  guides(color = "none") +
  geom_hline(yintercept = mean(as.numeric(two.hundred$elapsed)/3600), linetype = 3) +

  geom_hline(yintercept = x.lci, linetype = 5) +
  geom_hline(yintercept = x.uci, linetype = 5) +
  annotate(geom="text", x = as.Date("2014-06-05"),
           y=13.7 ,label="200km Time Limit: 13.5hrs", fontface="bold") +
  annotate(geom="text", x = as.Date("2014-06-05"),
           y=8.3 ,label=paste0("Lower Control Limit: ", round(x.lci, 1), "hrs")) +
  annotate(geom="text", x = as.Date("2014-06-05"),
           y=12.9 ,label=paste0("Upper Control Limit: ", round(x.uci, 1), "hrs")) +
  ggtitle("X-Chart: 95% Confidence Intervals")
# out of 22 200k's, one falls outside of range of 2 SD's. to be expected (95% = 1/20)

# Moving Range Chart
# From Control Chart Constants, D4 = 3.267,
mr.ucl <- 3.267*mr.mean

# plot moving range - need to grid.arrange it underneath
three <- ggplot(two.hundred, aes(x = Date, y = moving.range), label = Date) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = round(moving.range, 1), size = 1)) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Moving Range (Hours)",
       x = "Ride Date") +
  guides(size = "none") +
  geom_hline(yintercept = mr.mean, linetype = 3) +
  geom_hline(yintercept = mr.ucl, linetype = 5) +
  coord_cartesian(ylim = c(0, 4)) +
  annotate(geom="text", x = as.Date("2014-06-05"),
           y=4 ,label=paste0("Upper Control Limit: ", round(mr.ucl, 1), "hrs")) +
  annotate(geom="text", x = as.Date("2016-04-05"),
           y=1.3 ,label=paste0("Mean: ", round(mr.mean, 1), "hrs")) +
  ggtitle("MR-Chart")

grid.arrange(one, three, heights = 3:2)
grid.arrange(two, three, heights = 3:2)

# need a ggsave call

# make a slideshow about process variation
# limitation: not evenly spaced sampling
## also, autocorrelations between observations (using CI might be better)
## common cause/chance variation: inherent random variability (my stable performance)
## special cause: non-random (weather, climbing)
# how to calculate intervals? by using the mean times for 6 month chunks?

## make a fishbone chart
# rider fitness
# equipment
# weather
# terrain
# other riders, social aspect (stay with chatty people even if slow, push to ride with faster people)

## post code to github
rm(list=ls())

# load packages -----------------------------------------------------------

library(mcp)
library(changepoint)
library(strucchange)
library(segmented)
library(tidyverse)

update_geom_defaults("point", list(shape = 21, fill="grey"))

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

#make some data
data <- data.df %>%
  filter(outlier==FALSE) %>%
  drop_na(cluster) %>%
  arrange(year.mean) %>%
  mutate(y=d15N.permil,
         x=row_number()) %>%
  select(x,y) %>%
  drop_na()

str(data)

plot(data)

# mcp package -------------------------------------------------------------

# Define the model with a change point for the mean
model <- list(
  y ~ 1 + x,
  1 + (1|location) ~ 1 + x,
  1 + (1|location) ~ 1 + x
)

model <- list(
  y ~ 1 + x,
  ~ 1 + x,
  ~ 1
)

result <- mcp(model, data = data, par_x = "x", adapt = 10000)
#summary(result)
plot(result)
plot(result, facet_by = "location")
plot_pars(result)

# changepoint package -----------------------------------------------------

plot(data$y~data$x)

result <- cpt.mean(data = data$y, method = "PELT")  # detects change in mean
plot(result)


# segmented package -------------------------------------------------------

lm_model <- lm(y ~ x + location, data = data)

segmented_model <- segmented(lm_model, seg.Z = ~ x)
summary(segmented_model)
plot(lm_model)
plot(segmented_model)
points(data)


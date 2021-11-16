# loading packages
library(tidyverse)

# Loading data ----

# load data from clipboard -- I'm creating a dataframe from data I copied from a google sheet
data.in <- read_csv('data_files/CvC_test-data.csv', col_names = TRUE)
  # data.in <- read_delim(clipboard(), delim='\t') # temporary to read copied stuff


# Selecting test columns ----

subset_data <- data.in %>% 
  select(1:7)  # selects A-F Chlorella samples at OD 685 nm

# I still don't know how to use Growthcurver on multiple growth curves at once  
nested_data <- subset_data %>% 
  pivot_longer(data = ., cols = -Time, 
               values_to = 'OD', names_to = 'Samples') %>% 
  group_by(Samples) %>% 
  # mutate() # create new vector for each sample type
   
  
  # condensing all TIME OD into data frames for each sample
  nest (data = c(Time, OD)) %>% 
  # fit to each sample's growth data
  mutate(fits = map(data, 
                        ~ growthcurver::SummarizeGrowth(.x$Time, .x$OD))
  ) %>% 
  
  # Retrieve parameters from the fit
  mutate(growth_rate = map_dbl(fits, ~ .x$vals$r) )
  
# GOing to plot vectorized later--
  # plot each fit
  # mutate(plots_col = map(fits, plot)) # doesn't seem to plot when we call later

# Plotting the growth rates
ggplot(nested_data, 
       aes(x = Samples, y = growth_rate)) + 
  
  geom_point()


# Fit growth curve ----

# using growthcurver package
# https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html

# Controls
gc_fit1 <- growthcurver::SummarizeGrowth(subset_data$Time, subset_data$`OD 685-A (Chlorella)`)
gc_fit2 <- growthcurver::SummarizeGrowth(subset_data$Time, subset_data$`OD 685-B (Chlorella)`)
gc_fit3 <- growthcurver::SummarizeGrowth(subset_data$Time, subset_data$`OD 685-C (Chlorella)`)

# Samples
gc_fit4 <- growthcurver::SummarizeGrowth(subset_data$Time, subset_data$`OD 685-D (Chlorella)`)
gc_fit5 <- growthcurver::SummarizeGrowth(subset_data$Time, subset_data$`OD 685-E (Chlorella)`)
gc_fit6 <- growthcurver::SummarizeGrowth(subset_data$Time, subset_data$`OD 685-F (Chlorella)`)

# Plotting new growth curves ----

# Controls plotted
Growthcurve1 <- plot(gc_fit1)
Growthcurve2 <- plot(gc_fit2)
Growthcurve3 <- plot(gc_fit3)

# Samples plotted
Growthcurve4 <- plot(gc_fit4)
Growthcurve5 <- plot(gc_fit5)
Growthcurve6 <- plot(gc_fit6)

# Displaying Growth Rate and Error ----

# Control values
str(gc_fit1$vals)
str(gc_fit2$vals)
str(gc_fit3$vals)

# Sample values
str(gc_fit4$vals)
str(gc_fit5$vals)
str(gc_fit6$vals)

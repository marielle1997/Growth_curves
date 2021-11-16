# loading packages
library(tidyverse)

# Loading data ----

# load data from clipboard -- I'm creating a dataframe from data I copied from a google sheet
data.in <- read_delim(clipboard(), delim='\t')

# Selecting test columns ----

data.mid <- data.in %>% 
  select(1:7)  # selects A-F Chlorella samples at OD 685 nm

# I still don't know how to use Growthcurver on multiple growth curves at once  
data.new <- data.mid %>% 
  rename('OD' = starts_with('OD')) %>%
  pivot_longer(data.mid, cols = -Time, values_to = 'OD', names_to = 'Samples') %>% 
  group_by(Samples) %>% 
  nest (data = c(Time, OD)) 
data.model <- data.new %>%  
  mutate(data.new = growthcurver::SummarizeGrowth(data$Time, data$OD))


# Fit growth curve ----

# using growthcurver package
# https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html

# Controls
gc_fit1 <- growthcurver::SummarizeGrowth(data.mid$Time, data.mid$`OD 685-A (Chlorella)`)
gc_fit2 <- growthcurver::SummarizeGrowth(data.mid$Time, data.mid$`OD 685-B (Chlorella)`)
gc_fit3 <- growthcurver::SummarizeGrowth(data.mid$Time, data.mid$`OD 685-C (Chlorella)`)

# Samples
gc_fit4 <- growthcurver::SummarizeGrowth(data.mid$Time, data.mid$`OD 685-D (Chlorella)`)
gc_fit5 <- growthcurver::SummarizeGrowth(data.mid$Time, data.mid$`OD 685-E (Chlorella)`)
gc_fit6 <- growthcurver::SummarizeGrowth(data.mid$Time, data.mid$`OD 685-F (Chlorella)`)

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

# preliminaries ----

# loading packages
library(tidyverse) # general purpose for tidy data manipulation and plotting

# the name of the google sub-sheet to read
sheet_name_data <- 'C v P' 

# URL of the google sheet to read from
ggl_sheet_url <- 'https://docs.google.com/spreadsheets/d/1fnoj_f-QcrnEIDRB6UXDVqv4hy0gXGMayUZfIaWd5E4/edit#gid=418342985'

# Assign the A-F variables to 'control' 'co-culture' and such for plotting 
flask.letter_assignment_lookup <- 
  tibble(flask = LETTERS[1:6], # the letters correspond to FLASKs samples are in
         bio.replicate = rep(1:3, 2),
         experiment = rep(c('control', 'co-culture'), each = 3) 
         )

# Loading data ----

# load data from clipboard -- I'm creating a dataframe from data I copied from a google sheet
data.in <- 
  googlesheets4::read_sheet(
    ss = ggl_sheet_url,
    sheet = sheet_name_data)

# temporary to read test files
#data.in <- read_csv('data_files/CvC_test-data.csv', col_names = TRUE)
  # data.in <- read_delim(clipboard(), delim='\t') # temporary to read copied stuff


# Selecting test columns ----

subset_data <- data.in %>% 
  select(-Date, # selects A-F Chlorella samples at OD 685 nm
         -contains('Pseudomonas'))  # remove Pseudomonas data

# I still don't know how to use Growthcurver on multiple growth curves at once  
nested_data <- subset_data %>% 
  pivot_longer(data = ., cols = -Time, 
               values_to = 'OD', names_to = 'Samples') %>% 
  
  # separate the column samples into multiple columns for flask, organism etc.
  separate(col = Samples,
           into = c(NA, 'flask', 'organism'),
           sep = '-| \\(|\\)',
           remove = FALSE) %>% 
  
  # grouping each dataset
  group_by(Samples) %>% 
  # mutate() # create new vector for each sample type
  
  # condensing all TIME OD into data frames for each sample
  nest (data = c(Time, OD)) %>% 
  
  # fit to each sample's growth data
  mutate(fits = map(data, 
                    ~ growthcurver::SummarizeGrowth(.x$Time, .x$OD))
  ) %>% 
  
  # Retrieve parameters from the fit
  mutate(growth_rate = map_dbl(fits, ~ .x$vals$r),
         error = map_dbl(fits, ~ .x$vals$sigma/2))  %>%
  
  # Merge the flask names with the lookup table
  left_join(x = ., 
            y = flask.letter_assignment_lookup,
            by = 'flask') %>% 
  
  # just re-arranging the columns for easy quick glance
  select(organism, experiment, growth_rate, bio.replicate, everything())

# massage the data for plotting the raw growth data ; mean, SD as well
plotting_data <- 
  select(nested_data, 
         organism, experiment, growth_rate, bio.replicate, data) %>% 
  
  # unnest the OD vs Time data
  unnest(cols = data) %>% 

  # regroup - replicates within the same group -- for doing mean, sd
  ungroup() %>% group_by(organism, experiment, Time) %>% 
  
  # summarizing data : mean and SD of biological replicates
  mutate(
    across(c('OD', 'bio.replicate'), 
           lst(
             mean = ~ mean(.x, na.rm = TRUE),
             sd =  ~ sd(.x, na.rm = TRUE)) 
           )
    )
    
    # OD_mean = mean(OD))


# GOing to plot vectorized later--
  # plot each fit
  # mutate(plots_col = map(fits, plot)) # doesn't seem to plot when we call later

# How do I get all the growth curves to one plot? ----
#ggplot(nested_data[[2]][[,]],
 #       aes(x = Time, OD)) +
  #    geom_line() + 
   #     theme_classic()

# Plotting the growth rates ----
ggplot(nested_data, 
       aes(x = Samples, y = growth_rate)) + 
  geom_errorbar(aes(x=Samples, ymin=growth_rate-error, ymax=growth_rate+error)) +
  geom_point(aes(color=Samples), size=3) +
  labs(x = "Samples", y = "Growth Rate (r)") +
  scale_color_manual(name=NULL, 
                      breaks = c(	"OD 685-A (Chlorella)",
                                  "OD 685-B (Chlorella)",
                                  "OD 685-C (Chlorella)",
                                  "OD 685-D (Chlorella)",
                                  "OD 685-E (Chlorella)",
                                  "OD 685-F (Chlorella)" ),
                      values = c("pink",
                                 "pink",
                                 "pink",
                                 "orange",
                                 "orange",
                                 "orange"),
                      labels = c("Controls","Controls","Controls", "Samples","Samples","Samples")) +
  theme_classic() + 
  theme(legend.position = "none") +
  scale_x_discrete(labels=c("A","B","C","D","E","F")) +
  ylim(-.05,0.2) +
  theme(axis.text = element_text(size=12, color = "black"),
        axis.title = element_text(size=14)) 

ggsave("CvC.tiff", width = 5, height = 4)
ggsave("CvP.tiff", width = 5, height = 4)
ggsave("CvM.tiff", width = 5, height = 4)
ggsave("CvM+P.tiff", width = 5, height = 4)

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
Growthcurve1 <- plot(gc_fit1, ylim=range(0:1))
Growthcurve2 <- plot(gc_fit2, ylim=range(0:1))
Growthcurve3 <- plot(gc_fit3, ylim=range(0:1))

# Samples plotted
Growthcurve4 <- plot(gc_fit4, ylim=range(0:1))
Growthcurve5 <- plot(gc_fit5, ylim=range(0:1))
Growthcurve6 <- plot(gc_fit6, ylim=range(0:1))


# Displaying Growth Rate and Error ----

# Control values
str(gc_fit1$vals)
str(gc_fit2$vals)
str(gc_fit3$vals)

# Sample values
str(gc_fit4$vals)
str(gc_fit5$vals)
str(gc_fit6$vals)

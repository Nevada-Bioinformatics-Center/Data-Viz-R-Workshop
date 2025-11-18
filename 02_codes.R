# Visualizing Data with R: From Basics to Publication-Ready Graphics--Part 2
# Nevada Bioinformatics Center

# 1. Introduction ----

# 1.1 Recap ----

# 1.2 Overview ----

# View the built-in dataset
head(mtcars)

# The mtcars dataset contains data on 32 car models, with the following columns:
# - mpg: Miles per gallon (fuel efficiency).
# - cyl: Number of cylinders in the engine (4, 6, or 8).
# - disp: Engine displacement (in cubic inches).
# - hp: Gross horsepower.
# - drat: Rear axle ratio.
# - wt: Weight of the car (in 1000s of pounds).
# - qsec: 1/4 mile time (time in seconds to cover a quarter mile).
# - vs: Engine shape (0 = V-shaped, 1 = straight).
# - am: Transmission type (0 = automatic, 1 = manual).
# - gear: Number of forward gears.
# - carb: Number of carburetors.
 
# In the dataset, we have categorical data (cyl, vs, am, gear, carb) and 
# continuous data (mpg, disp, hp, drat, wt, qsec).


# 2. Data Summarization ----

# 2.1 Reviewing the Data ----

# Load libraries and data
library(ggplot2)
data("mtcars")

ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=1)

# Change the binwidth to 0.5
# Change the binwidth to 1.5
# Change the binwidth to 10


# ggplot(mtcars, aes(x=factor(cyl))) + geom_histogram(binwidth = 1)


ggplot(mtcars, aes(x=cyl)) + geom_histogram(binwidth = 1)


ggplot(mtcars, aes(x = factor(cyl))) +   # Factor again for categorical
  geom_bar()

# Look at the distribution of qsec in the code block below:



# 2.2 Plotting Summary Statistics with dplyr ----

# Install libraries if necessary
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Load libraries
library(dplyr)
library(tidyverse)                                 # data manipulation


mtcars_summary <- mtcars %>%
  group_by(cyl) %>%  # Group by the number of cylinders
  summarise(
    mpg_mean = mean(mpg, na.rm = TRUE),  # mpg mean
    mpg_sd = sd(mpg, na.rm = TRUE),      # mpg standard deviation
    mpg_n = n(),                         # mpg count
    qsec_mean = mean(qsec, na.rm = TRUE),  # qsec mean
    qsec_sd = sd(qsec, na.rm = TRUE),      # qsec standard deviation
    qsec_n = n()                           # qsec count
  ) %>%
  mutate(
    mpg_se = mpg_sd / sqrt(mpg_n),  # mpg standard error
    mpg_lower_ci = mpg_mean - qt(1 - (0.05 / 2), mpg_n - 1) * mpg_se,  # mpg lower 95% CI
    mpg_upper_ci = mpg_mean + qt(1 - (0.05 / 2), mpg_n - 1) * mpg_se,  # mpg upper 95% CI
    qsec_se = qsec_sd / sqrt(qsec_n),  # qsec standard error
    qsec_lower_ci = qsec_mean - qt(1 - (0.05 / 2), qsec_n - 1) * qsec_se,  # qsec lower 95% CI
    qsec_upper_ci = qsec_mean + qt(1 - (0.05 / 2), qsec_n - 1) * qsec_se   # qsec upper 95% CI
  )


ggplot(data = mtcars_summary, aes(x = factor(cyl), y = qsec_mean)) +
  geom_bar(stat = "identity")


ggplot(data = mtcars_summary, aes(x = factor(cyl), y = qsec_mean, color = factor(cyl))) +
  geom_bar(stat = "identity")

# Wait, why didn't that work?
# How do we use color appropriately in our geom_bar()?


# Manually adding error bars
ggplot(data = mtcars_summary, aes(x = factor(cyl), y = qsec_mean, fill = factor(cyl))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = qsec_lower_ci, ymax = qsec_upper_ci)) 


# 2.3 Plotting Summary Statistics with Hmisc ----

# Install and load the Hmisc package
if (!requireNamespace("Hmisc", quietly = TRUE)) {
  install.packages("Hmisc")
}
library(Hmisc)

# Use the regular dataset and not the summary we made
ggplot(data = mtcars, 
       aes(
         x = factor(cyl), 
         y = qsec, 
         fill = factor(cyl))) +
  stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") # Add error bars showing confidence intervals


ggplot(data = mtcars, 
       aes(
         x = factor(cyl), 
         y = qsec, 
         fill = factor(cyl))) +
  stat_summary(fun = mean, geom = "point", size = 5) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar")
  
# Change the colors back
# Make the error bars black and thinner
# Put the error bars behind the points


# 2.4 Why Bar Plots are Bad ----

set.seed(123)  # For reproducibility

# Red data
red_data <- rnorm(1000, mean = 100, sd = 15)

# Blue data (using exponential distribution scaled to match mean and SD of normal)
blue_data <- rexp(1000, rate = 0.1)
desired_mean <- mean(red_data)
desired_sd <- sd(red_data)
blue_data <- (blue_data - mean(blue_data)) / sd(blue_data) * desired_sd + desired_mean

# Combine into a data frame
data <- data.frame(
  group = rep(c("Red", "Blue"), each = length(red_data)),
  value = c(red_data, blue_data)
)

data$group <- factor(data$group, levels = c("Red", "Blue"))

# View our data
head(data)


ggplot(data, aes(x = value, fill = group)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ group) +
  labs(title = "Clearly Different Distributions")


ggplot(data, aes(x = group, y=value, fill = group)) +
  geom_boxplot() +
  labs(title = "Still Looks Different")


ggplot(data, aes(x = group, y = value, fill = group)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.4) +
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +
  labs(title = "They Look the Same!")

# Friends Don't Let Friends Make Barplots


# 3. Plotting Raw Data ----

# Boxplot
ggplot(data = mtcars, aes(x = factor(cyl), y = qsec, fill = factor(cyl))) 


# Violin Plot
ggplot(data = mtcars, aes(x = factor(cyl), y = qsec, fill = factor(cyl)))

# Can add draw_quantiles

# Raw Data Points
ggplot(data = mtcars, aes(x = factor(cyl), y = qsec, fill = factor(cyl)))  # Switch back to color


# Plotting Raw Data and Summary Stats
ggplot(data = mtcars, aes(x = factor(cyl), y = qsec, color = factor(cyl))) +
  geom_point(size = 5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") 


# Now we have a(n ugly) figure! 


# 4. Customizing Figures ----

# 4.1 Themes ----

# Add theme_classic

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec, color = factor(cyl))) +
  geom_point(size = 5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black")


# Much better! Try out theme_minimal() and theme_bw().

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec, color = factor(cyl))) +
  geom_point(size = 5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black")     # Add your theme here
  


# Set our theme to classic
theme_set(theme_classic())


ggplot(data = mtcars, aes(x = factor(cyl), y = qsec, color = factor(cyl))) +
  geom_point(size = 5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black")


# 4.2 Shapes and Colors ----

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec, color = factor(cyl))) +
  geom_point(size = 5, pch = 21) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") 
  

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(size = 5, pch = 21, color = "black", aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") 


ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(size = 5, pch = 21, color = "hotpink", stroke = 2, # Thicken border
             aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") 

# Pick some different shapes to test 


# Manually pick colors

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(size = 5, pch = 21, color = "black", aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") +
  scale_fill_manual() 


ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(size = 5, pch = 21, color = "black", aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") +
  scale_fill_brewer(palette = "Set2") 

RColorBrewer::display.brewer.all()


# We can also use a color pallet generator (https://coolors.co/) for custom color pallets. 

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(size = 5, pch = 21, color = "black", aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") +
  scale_fill_manual(values = c("#3A3238", "#6E4555", "#D282A6")) 


# Add position = position_jitter

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(position = position_jitter(),   # Slightly offset points
             size = 5, pch = 21, color = "black",
             aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") +
  scale_fill_manual(values = c("grey50", "grey40", "#0074D9"))


# 4.3 Labels and Axes ----

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(position = position_jitter(width = 0.05), size = 5, pch = 21, color = "black", aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") +
  scale_fill_manual(
    values = c("grey50", "grey40", "#0074D9"),
    labels = c("4" = "Slowest", 
               "6" = "Still Slow", 
               "8" = "Fast but Broke")) +        # Custom legend labels
  scale_x_discrete(labels = c("4" = "Slowest", 
                              "6" = "Still Slow", 
                              "8" = "Fast but Broke"))     # Custom x axis labels


# Add x, y, title, and legend labels

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(position = position_jitter(width = 0.05), size = 5, pch = 21, color = "black", aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") +
  scale_fill_manual(
    values = c("grey50", "grey40", "#0074D9"),
    labels = c("4" = "Slowest", 
               "6" = "Still Slow", 
               "8" = "Fast but Broke")) +        # Custom legend labels
  scale_x_discrete(labels = c("4" = "Slowest", 
                              "6" = "Still Slow", 
                              "8" = "Fast but Broke"))     # Custom x axis labels
  


# Set the y-axis breaks to 12, 18, at 24, and limits 12-24.
# Change the theme to have larger text, in Times New Roman, and remove the legend


# Try it for yourself:
# Edit the plot yourself: chose different colors, shapes, titles, and adjustments:

ggplot(data = mtcars, aes(x = factor(cyl), y = qsec)) +
  geom_point(position = position_jitter(width = 0.05), size = 5, pch = 21, color = "black", aes(fill = factor(cyl))) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
        color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point",
    size = 5, color = "black") +
  scale_fill_manual(
    values = c("grey50", "grey40", "#0074D9"),
    labels = c("4" = "Slowest", 
               "6" = "Still Slow", 
               "8" = "Fast but Broke")) +        # Custom legend labels
  scale_x_discrete(labels = c("4" = "Slowest", 
                              "6" = "Still Slow", 
                              "8" = "Fast but Broke")) 


# 5. Example--Bubble Plot ----

if (!requireNamespace("gapminder", quietly = TRUE)) {
  install.packages("gapminder")
}

library(gapminder)  
data("gapminder")


# Filter out 2007 data
data <- gapminder %>% 
  filter(year=="2007") %>% 
  dplyr::select(-year)
# Changing values, sorting, and setting factor levels for plotting
data %>%
mutate(pop=pop/1000000) %>%
arrange(desc(pop)) %>%
mutate(country = factor(country)) 


# Let's install a new package for a better color pallet

if (!requireNamespace("viridis", quietly = TRUE)) {
  install.packages("viridis")
}

library(viridis)                                   # color scales 


# Plot
ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_color_viridis(discrete=TRUE) +
  theme(legend.position="bottom")


# Plot
ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1, 15), guide = "none") + # set population sizes and remove legend
  scale_color_viridis(discrete=TRUE) +
  theme(legend.position="bottom")


# Packge for annotations with "geom_text_repel"
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel")
}

library(ggrepel)                                   # extra geoms for ggplot2


# Prepare data
tmp <- data %>%
mutate(annotation = case_when(
  gdpPercap > 5000 & lifeExp < 60 ~ "yes",
  gdpPercap > 40000 ~ "yes")) 


?geom_text_repel


# Plot
ggplot(tmp, aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19)) +
  scale_color_viridis(discrete=TRUE) +
  theme(legend.position="none") + # Remove both legends for aestetics
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=country), size=4 )


ggplot(tmp, aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1.4, 19), name = "Population (Millions)") +
  scale_color_viridis(discrete = TRUE) +
  labs(
    title = "Global Economic and Health Overview",
    subtitle = "GDP per capita vs. Life Expectancy",
    x = "GDP per Capita (USD)",
    y = "Life Expectancy (years)",
    caption = "Data sourced from Gapminder"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    plot.caption = element_text(size = 8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_text_repel(
    data = tmp %>% filter(annotation == "yes"),
    aes(label = country),
    size = 4
  )


# 6. Exercises ----

# Create this image: images/ex1.png

# Here is some helpful code
am0 <-subset(mtcars, am == 0)
am1 <-subset(mtcars, am == 1)
mn0 <- round(mean(am0$mpg), 2)
mn1 <- round(mean(am1$mpg), 2)

# Create plot here


# Create this image: images/ex2.png


# 7. Summary ----

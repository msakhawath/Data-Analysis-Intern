library(tidyverse)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(xtable)
library(cowplot)


# read the file
demographic <- read.csv(file = 'census2002_2022.csv')

head(demographic)

demographic <- demographic %>%
  group_by(Region, Subregion) %>%
  mutate(across(starts_with("Life.Expectancy"), ~ ifelse(is.na(.), mean(.[!is.na(.)]), .)),
         across(starts_with("Under.Age.5.Mortality"), ~ ifelse(is.na(.), mean(.[!is.na(.)]), .))) %>%
  ungroup()


demographic

# Check for any remaining missing values
sum(is.na(demographic))

head(demographic)

colSums(is.na(demographic))




### Question 1




Census_2002 <- demographic[demographic$Year == 2002, ]
Census_2022 <- demographic[demographic$Year == 2022, ] 



# Frequency distribution of Mortality rate of both sexes in 2022

Histogram_infant_mortality <-ggplot(Census_2022, aes(x =Under.Age.5.Mortality..Both.Sexes)) + 
  geom_histogram(aes(y=..density..), color="Black", fill="light green") +
  scale_x_continuous(name = "U5 mortality of both sexes") +
  scale_y_continuous(name = "density") 
Histogram_infant_mortality

# Frequency distribution of life expectancy of both sexes in 2022

Histogram_both_sexes <-ggplot(Census_2022, aes(x = Life.Expectancy.at.Birth..Both.Sexes)) +
  geom_histogram(aes(y=..density..), color="Black", fill="#00BFC4") +
  scale_x_continuous(name = "Life expectancy of both sexes") +
  scale_y_continuous(name = "density") 
Histogram_both_sexes

# Frequency distribution of Mortality rate of males in 2022

Histogram_males_mortality <-ggplot(Census_2022, aes(x =Under.Age.5.Mortality..Males)) +
  geom_histogram(aes(y=..density..), color="Black", fill="light green") +
  scale_x_continuous(name = "U5 mortality of males") +
  scale_y_continuous(name = "density") 
Histogram_males_mortality


# Frequency distribution of life expectancy of males in 2022


Histogram_males <-ggplot(Census_2022, aes(x =Life.Expectancy.at.Birth..Males)) +
  geom_histogram(aes(y=..density..), color="Black", fill="#00BFC4") +
  scale_x_continuous(name = "Life expectancy of males")+  
  scale_y_continuous(name = "density") + ylim(0, 0.08)
Histogram_males


# Frequency distribution of Mortality rate of females in 2022

Histogram_females_mortality <-ggplot(Census_2022, aes(x =Under.Age.5.Mortality..Females)) +
  geom_histogram(aes(y=..density..), color="Black", fill="light green") +
  scale_x_continuous(name = "U5 mortality of females") +
  scale_y_continuous(name = "density") 

Histogram_females_mortality


# Frequency distribution of life expectancy of females in 2022

Histogram_females <-ggplot(Census_2022, aes(x =Life.Expectancy.at.Birth..Females)) +
  geom_histogram(aes(y=..density..), color="Black", fill="#00BFC4") +
  scale_x_continuous(name = "Life expectancy of females") +
  scale_y_continuous(name = "density") 

Histogram_females


# All combined histogram

histogram <- grid.arrange(Histogram_infant_mortality, Histogram_both_sexes, Histogram_males_mortality, Histogram_males,Histogram_females_mortality , Histogram_females, nrow=3, ncol=2)
histogram



# finding mean, median, minimum, maximum and standard deviation for life expectancy and 
# Under 5 mortality rate of male, female and both sexes. 

mortality_rate_both_22 <- filter(demographic, Year == 2022)$Under.Age.5.Mortality..Both.Sexes

summary(mortality_rate_both_22)
sd(mortality_rate_both_22) 



mortality_rate_m_22 <-  filter(demographic, Year == 2022)$Under.Age.5.Mortality..Males

summary(mortality_rate_m_22)
sd(mortality_rate_m_22)



mortality_rate_f_22 <-  filter(demographic, Year == 2022)$Under.Age.5.Mortality..Females

summary(mortality_rate_f_22)
sd(mortality_rate_f_22)


life_expec_both_22 <- filter(demographic, Year == 2022)$Life.Expectancy.at.Birth..Both.Sexes

summary(life_expec_both_22)
sd(life_expec_both_22) 



life_expect_f_22 <- filter(demographic, Year == 2022)$Life.Expectancy.at.Birth..Females

summary(life_expect_f_22)
sd(life_expect_f_22) 




life_expec_m_22 <- filter(demographic, Year == 2022)$Life.Expectancy.at.Birth..Males

summary(life_expec_m_22)
sd(life_expec_m_22) 




# part2 of question 1


# Box plot to show the difference between region and life expectancy of both sexes in 2022. 


LE_both_sexes <- ggplot(data = Census_2022, aes(x = Life.Expectancy.at.Birth..Both.Sexes, y = Region)) +
  geom_point(aes(x = Life.Expectancy.at.Birth..Both.Sexes,y= Region, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("Life expectancy of both sexes") +
  ylab("regions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))


LE_both_sexes


# Box plot to show the difference between region and life expectancy of males in 2022. 


LE_males <- ggplot(data = Census_2022, aes(x =  Life.Expectancy.at.Birth..Males, y = Region)) +
  geom_point(aes(y= Region, x =  Life.Expectancy.at.Birth..Males, colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7,outlier.color = "Black",  aes(fill = Region)) +
  xlab(" Life expectancy of males") + 
  ylab("regions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))

LE_males


# Box plot to show the difference between region and life expectancy of females in 2022. 


LE_females <- ggplot(data = Census_2022, aes(x = Life.Expectancy.at.Birth..Females, y = Region)) +
  geom_point(aes(y= Region, x = Life.Expectancy.at.Birth..Females, colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("Life expectancy of females") + 
  ylab("regions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))
LE_females


# Box plot to show the difference between region and U5 mortality of both sexes in 2022. 


U5_Mortality <- ggplot(data = Census_2022, aes(x = Under.Age.5.Mortality..Both.Sexes, y = Region)) +
  geom_point(aes(y= Region, x = Under.Age.5.Mortality..Both.Sexes, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("U5 mortality of both sexes") + 
  ylab("regions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))
U5_Mortality


# Box plot to show the difference between region and U5 mortality of males in 2022. 


U5_Mortality_male <- ggplot(data = Census_2022, aes(x = Under.Age.5.Mortality..Males, y = Region)) +
  geom_point(aes(y= Region, x = Under.Age.5.Mortality..Males, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("U5 mortality of males") + 
  ylab("regions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))

U5_Mortality_male


# Box plot to show the difference between region and U5 mortality of females in 2022. 


U5_Mortality_female <- ggplot(data = Census_2022, aes(x = Under.Age.5.Mortality..Females, y = Region)) +
  geom_point(aes(y= Region, x = Under.Age.5.Mortality..Females, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("U5 mortality of females") + 
  ylab("regions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))

U5_Mortality_female


# Combined plots

grid.arrange(LE_males,LE_females, nrow = 2, ncol = 1)
grid.arrange(U5_Mortality,LE_both_sexes, nrow = 2, ncol = 1)
grid.arrange(U5_Mortality_male,U5_Mortality_female, nrow = 2, ncol = 1)






### Question 2





Census_2022_africa <- subset(Census_2022, Region == "Africa")

Census_2022_africa$Subregion <- factor(Census_2022_africa$Subregion, levels = unique(Census_2022_africa$Subregion[order(Census_2022_africa$Region)]))


#Life.Expectancy..Both.Sexes
box_both_sexes <- ggplot(data = Census_2022_africa, aes(x = Life.Expectancy.at.Birth..Both.Sexes, y = Subregion)) +
  geom_point(aes(y= Subregion, x = Life.Expectancy.at.Birth..Both.Sexes, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "red", aes(fill = Region), color = 'blue') +   scale_fill_manual(values = "gray") +
  xlab("Life expectancy of both sexes") + xlim(50,85)+
  ylab("Subregions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))

box_both_sexes




#Life.Expectancy..Males
box_males <- ggplot(data = Census_2022_africa, aes(x = Life.Expectancy.at.Birth..Males, y = Subregion)) +
  geom_point(aes(y= Subregion, x = Life.Expectancy.at.Birth..Males, colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "red", aes(fill = Region), color = 'blue') +   scale_fill_manual(values = "gray") +
  xlab("Life expectancy of males") + xlim(50,85)+ 
  ylab("Subregions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10)) 

box_males

#Life.Expectancy..Females
box_females <- ggplot(data = Census_2022_africa, aes(x = Life.Expectancy.at.Birth..Females, y = Subregion)) +
  geom_point(aes(y= Subregion, x = Life.Expectancy.at.Birth..Females, colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "red", aes(fill = Region), color = 'blue') +   scale_fill_manual(values = "gray") +
  xlab("Life expectancy of females") + xlim(50,85)+ 
  ylab("Subregions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))

box_females

#Mortality.Rate..Both.Sexes



#u5 
box_males_u5 <- ggplot(data = Census_2022_africa, aes(x = Under.Age.5.Mortality..Males
                                                      , y = Subregion)) +
  geom_point(aes(y= Subregion, x = Under.Age.5.Mortality..Males
                 , colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "red", aes(fill = Region), color = 'blue') +   scale_fill_manual(values = "gray") +
  xlab("U5 mortality of males") + xlim(0,160)+ 
  ylab("Subregions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))

box_males_u5

#u5 
box_females_u5 <- ggplot(data = Census_2022_africa, aes(x = Under.Age.5.Mortality..Females
                                                        , y = Subregion)) +
  geom_point(aes(y= Subregion, x = Under.Age.5.Mortality..Females
                 , colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "red", aes(fill = Region), color = 'blue') +   scale_fill_manual(values = "gray") +
  xlab("U5 mortality of females") + xlim(0,160)+ 
  ylab("Subregions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))

box_females_u5





box_Mortality <- ggplot(data = Census_2022_africa, aes(x = Under.Age.5.Mortality..Both.Sexes, y = Subregion)) +
  geom_point(aes(y= Subregion, x = Under.Age.5.Mortality..Both.Sexes, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "red", aes(fill = Region), color = 'blue') +
  scale_fill_manual(values = "gray") +
  xlab("U5 mortality of both sexes") + 
  ylab("Subregions") +
  theme(axis.title.x = element_text(colour="Black", size=10, face = "bold"),
        axis.title.y = element_text(colour="Black", size=10, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))







legend_box_males <- get_legend(box_males)
legend_box_females <- get_legend(box_females)



# Create the final plot with a common legend
final_plot_bothsex <- plot_grid(box_males + theme(legend.position = "top"),
                                box_females + theme(legend.position = "none"),
                                align = "h",
                                axis = "tb",
                                ncol = 1)

# Add the common legend to the final plot
final_plot_males <- ggdraw() +
  draw_plot(final_plot_bothsex) +
  draw_plot(legend_box_males, x = 0.78, y = 0.15) +
  draw_plot(legend_box_females, x = 0.78, y = 0.15) 

# Show the final plot
final_plot_males







# Define the legend separately for each plot
legend_box_males_u5 <- get_legend(box_males_u5)
legend_box_females_u5 <- get_legend(box_females_u5)


# Create the final plot with a common legend
final_plot <- plot_grid(box_males_u5 + theme(legend.position = "top"),
                              box_females_u5 + theme(legend.position = "none"),
                              align = "h",
                              axis = "tb",
                              ncol = 1)

# Add the common legend to the final plot
final_plot_females <- ggdraw() +
  draw_plot(final_plot) +
  draw_plot(legend_box_males_u5, x = 0.78, y = 0.15) +
  draw_plot(legend_box_females_u5, x = 0.9, y = 0.15)

# Show the final plot
final_plot_females








# Define the legend separately for each plot
legend_box_both_sexes <- get_legend(box_both_sexes)
legend_box_Mortality <- get_legend(box_Mortality)

# Create the final plot with a common legend
final_plot_b <- plot_grid(box_both_sexes + theme(legend.position = "top"),
                                box_Mortality + theme(legend.position = "none"),
                                align = "h",
                                axis = "tb",
                                ncol = 1)


# Add the common legend to the final plot
final_plot_both <- ggdraw() +
  draw_plot(final_plot_b) +
  draw_plot(legend_box_both_sexes , x = 0.78, y = 0.15) +
  draw_plot(legend_box_Mortality, x = 0.9, y = 0.15)

# Show the final plot
final_plot_both






# Under.Age.5.Mortality..Males
# Create an empty data frame to store the results
results_df_male <- data.frame()
# Subset the data for the Africa region
africa_df <- subset(Census_2022, Region == "Africa")

# Get unique subregions in Africa
africa_subregions <- unique(africa_df$Subregion)

# Loop through each subregion and calculate the descriptive statistics
for (subregion in africa_subregions) {
  subregion_df <- subset(africa_df, subregion == Subregion)
  subregion_mean <- mean(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_median <- median(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_var <- var(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_iqr <- IQR(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_min <- min(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_max <- max(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  
  # Save the results in individual variables for each subregion
  # Create a data frame to store the results of this subregion
  subregion_results <- data.frame(
    subregion = subregion,
    mean = round(subregion_mean,2),
    median = round(subregion_median,2),
    var = round(subregion_var,2),
    iqr = round(subregion_iqr,2),
    min = round(subregion_min,2),
    max = round(subregion_max,2)
    
  )
  
  # Append the results to the overall results data frame
  results_df_male <- rbind(results_df_male, subregion_results)
}

results_df_male








# Exporting the results



table_list <- list(results_df_male)
table_captions <- list("Descriptive statistics of U5 mortality of both sexes")
table_labels <- list("extra")
for (i in 1:length(table_list)) {
  file_name_tex = paste("extra", i, ".tex", sep="")
  print(xtable(table_list[[i]], caption = table_captions[[i]], label = table_labels[[i]], table.placement = "H"), file = file_name_tex, caption.placement = "top")
  
  file_name_pdf = paste("table_", i, ".pdf", sep="")
  pdf(file_name_pdf, height=11, width=10)
  grid.table(table_list[[i]])
  dev.off()
}



## Question 3
# Are there bi variate correlations between the variables?






# Correlations between life expectancy female and U5 mortality rate of box sexes.

gr12 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(Under.Age.5.Mortality..Both.Sexes, Life.Expectancy.at.Birth..Females)) +
  ylab("Life expectancy of females (years)") + xlab("U5 mortality of both sexes") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=8))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr12

cor(mortality_rate_both_22,life_expec_both_22,method = 'pearson') # -0.90
cor(mortality_rate_both_22,life_expec_m_22,method = 'pearson' ) # -0.88
cor(mortality_rate_both_22,life_expect_f_22,method = 'pearson' ) # -0.91
cor(life_expec_both_22,life_expec_m_22,method = 'pearson' ) # .99
cor(life_expec_both_22,life_expect_f_22,method = 'pearson') # .99
cor(life_expect_f_22,life_expec_m_22 ,method = 'pearson') #.97


# Correlations between life expectancy male and U5 mortality rate of both sexes.

gr22 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(Under.Age.5.Mortality..Both.Sexes, Life.Expectancy.at.Birth..Males)) +
  ylab("Life expectancy of males (years)") + xlab("U5 mortality of both sexes") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=8))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr22

# Correlations between life expectancy both sexes and U5 mortality rate of both sexes.


gr32 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(Under.Age.5.Mortality..Both.Sexes, Life.Expectancy.at.Birth..Both.Sexes)) +
  ylab("Life expectancy of both sexes (years)") + xlab("U5 mortality of both sexes") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=8))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr32





combined <- gr12 + gr22 + gr32 & theme(legend.position = "bottom")
comb_2 <- combined + plot_layout(guides = "collect")
comb_2







# Correlations between life expectancy female and both sexes.

gr13 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(Life.Expectancy.at.Birth..Both.Sexes, Life.Expectancy.at.Birth..Females)) +
  ylab("Life expectancy females (year)") + xlab("Life expectancy both sexes") +  theme(plot.title = element_text(size=15,hjust = 0.4,face = "bold"),text = element_text(size=7))+
  geom_point(position = position_jitter(w = 0.04, h = 0.01), size = 1.5)
gr13

# Correlations between life expectancy male and both sexes.

gr23 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(Life.Expectancy.at.Birth..Both.Sexes, Life.Expectancy.at.Birth..Males)) +
  ylab("Life expectancy males (year)") + xlab("Life expectancy both sexes") +  theme(plot.title = element_text(size=15,hjust = 0.4,face = "bold"),text = element_text(size=7))+
  geom_point(position = position_jitter(w = 0.04, h = 0.01), size = 1.5)
gr23

# Correlations between life expectancy male and life expectancy female.

gr33 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(Life.Expectancy.at.Birth..Males, Life.Expectancy.at.Birth..Females)) +
  ylab("Life expectancy females (year)") + xlab("Life expectancy males") +  theme(plot.title = element_text(size=15,hjust = 0.4,face = "bold"),text = element_text(size=7))+
  geom_point(position = position_jitter(w = 0.04, h = 0.01), size = 1.5)
gr33

combined <- gr13 + gr23 + gr33 & theme(legend.position = "bottom")
comb_3 <- combined + plot_layout(guides = "collect")
comb_3





###  Question 4


# Scatter plot to show the comparison of mortality rate in 2002 and 2022


plot17<-ggplot(Census_2022, aes(x= Census_2022$Under.Age.5.Mortality..Both.Sexes, y= Census_2002$Under.Age.5.Mortality..Both.Sexes)) +
  geom_point(size=2,aes(color = Region, shape = Region))+
labs(x="U5 mortality of both sexes in 2022", y="U5 mortality both sex 2002")
k<-plot17+ geom_abline()
k

# Scatter plot to show the comparison of life expectancy both sexes in 2002 and 2022

plot18<-ggplot(Census_2022, aes(x=Census_2022$Life.Expectancy.at.Birth..Both.Sexes, y=Census_2002$Life.Expectancy.at.Birth..Both.Sexes)) +
  geom_point(size=2,aes(color = Region, shape = Region))+ labs(x="Life expectancy of both sexes in 2022", y="Life expectancy both sexe 2002")
p<-plot18+ geom_abline()
p


grid.arrange(p,k)












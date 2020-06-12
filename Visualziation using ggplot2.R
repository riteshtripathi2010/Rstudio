
#Process of Visuaz,iztion
#Business Question > Data > Choose Visuazlaition > Data Preparation > Develop Visuazlization > Develop Insights > Next Steps
setwd("/Users/riteshtripathi/Desktop")
vehicle <- read.csv("vehicle.csv", header = TRUE)

library(dplyr)
library(ggplot2)
install.packages("ggExtra")
library(ggExtra)
install.packages("vehicle")
library(vehicle)

#vehicle <- read.csv(file.choose(), hheader = TRUE)
View(vehicle)
str(vehicle)
#Ask Business Question:
#How are labor hours and labor cost related in the states of CA, TX and FL
#Since this is finding relationship, we will use scatter plot
#lets filter out states thta we dont need
veh <- vehicle %>% filter(State == 'CA' |State == 'TX' | State == 'FL' )

#We have finsihed till Data Preparation
#Now we develop visualziation with 7 elements of grammar of graphics
#Data
#Aesthetics
#geometry
#Facets
#Statistics
#Coordinates
#Themes

#lh labor hours and lc labor costs, since costs depends on how much time was spent on labor, thats why putting lc on y axis
#lc is dependent variable, lh is independent variable
veh %>% ggplot(aes(x = lh, y = lc)) + #used data as veh, and aesthatics aes
  geom_point() + #geometry: to have scatter plot
  facet_wrap(~ State) + #facets: broke down to 3 different plots
  ggtitle("Labour Hours vs Labor Costs",
          subtitle = "Source Vehicle Failure") + 
  geom_smooth(method = 'lm',
              col = 'red') +#Statistics: lm, for trend line
  scale_x_continuous("Labour Hours", 
                     limits = c(0, 40)) + #Cordinates: limits for upper and lower bound
  scale_y_continuous("Labour costs in $", 
                     limits = c(0,3000)) +
  theme_bw() #final was theme
#its 0 to 40 hrs for all states
#therefore we have used all 7 elements of graphics grammer

#Third part of the video: Univariate Plots
#Bar Plots
#Pie Charts
#Histogram
#Density Plot()
#Process Capability Analysis

#Bar Plots are for one variable: Univariate Plot
#We use Business Question: what are top 3 states contributing to vehicle failurs
#Do these states differ in terms on failures in the 1st month vs 2nd month

# %>% is use for connecting to something
vehicle %>%
  ggplot(aes(x = State)) +
  geom_bar() + #here all the states are not visible properly
  coord_flip()

veh %>%
  ggplot(aes(x = State, fill = State)) +#this will add default colors
  geom_bar()

veh %>% filter(fm == 1 | fm == 2)  %>%#fm failure month
  ggplot(aes(x = State, fill = State)) +
  geom_bar() +
  facet_grid(fm ~.)#this will put failure month 1 and 2 vertically, this is veritcally
#to get horizontall
#facet_grid(. ~ fm)#didnt work, will try later
ggtitle("Failure in cities", 
        subtitle = "Failure 1 vs 2 in top 3 cities")

#Histogram
#Business Question: How is failure mileage distributed in top 3states
#histogram, bin size by default is 30
veh %>%
  ggplot(aes(x = Mileage, fill = State)) +
  #geom_histogram(), by default is 30 bin
  #since they are in thousands, lets use 5000
  #alpha is used for tranperncy
  geom_histogram(binwidth = 5000, 
                 col = 'blue', 
                 alpha = 0.8) +
  facet_wrap(~State) +
  ggtitle('Fialure Mileage Distribution', 
          'Top 3 States')#we see most of the failures occur early and then reducing as the mileage increases

#lets add in more details
veh %>%
  ggplot(aes(x = Mileage, fill = State)) +
  
  geom_histogram(binwidth = 5000, 
                 col = 'blue', 
                 alpha = 0.8,
                 aes(y = ..density..)) +#here we are converting y axis into density
  facet_wrap(~State) +
  ggtitle('Fialure Mileage Distribution', 
          'Top 3 States') +
  scale_fill_brewer(palette = 'Set1') + #slight different colors
  geom_density(alpha = 0.2)
#along with histogram we get density curves, all these have right skewed and have longer tail on the right side

#Lets use filter
veh %>% filter(fm == 1 | fm == 2) %>% 
  ggplot(aes(x = Mileage, fill = State)) +
  geom_histogram(binwidth = 2000, 
                 col = 'blue', 
                 alpha = 0.8,
                 aes(y = ..density..)) +
  facet_grid(State ~ fm) +
  ggtitle('Fialure Mileage Distribution', 
          'Top 3 States') +
  scale_fill_brewer(palette = 'Set1') 
#so we get state vs failure for 1 and 2 months

#Density Plot
veh %>% filter(fm == 1 | fm == 2) %>% 
  ggplot(aes(x = Mileage, fill = State)) +
  geom_density(binwidth = 2000, 
               col = 'blue', 
               alpha = 0.8,
               aes(y = ..density..)) +
  facet_grid(State ~ fm) +
  ggtitle('Fialure Mileage Distribution', 
          'Top 3 States') +
  scale_fill_brewer(palette = 'Set1') 
#therefore more failures are occuring with lower milage and this reduces with higher mileage

#Process Capability analysis of endshake
#use dataset of handshake
endshake <- read.csv(file.choose(), header = T)
endshake %>% ggplot(aes(x = endshake)) +
  geom_histogram(binwidth = 10,
                 color = 'blue', 
                 fill = 'darkgreen',
                 alpha = 0.8) +
  ggtitle('Process', 
          'Specification') +
  geom_vline(xintercept = 800, 
             color = 'red', 
             size = 1.5) +
  geom_vline(xintercept = 900, 
             color = 'red', 
             size = 1.5) +
  geom_vline(xintercept = 850, 
             color = 'red', 
             size = 1.5, 
             linetype = 'dashed') 

#Plots with 2 or more variables, MultiVariate Variable Analysis

#Box Plot
#Business qns:
#How is the failure mileage distribtued in months 1 and 2 for top three states
veh %>% ggplot(aes(x = State, y = Mileage, fill = State)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey', 'grey', 'pink'),
                    guide = F)#this adds our own color

#lets add in more, we want 6box plots which fm == 2 and three states
veh %>% filter(fm == 1 | fm == 2) %>%
  ggplot(aes(x = interaction(State, fm), y = Mileage)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey', 'grey', 'pink'),
                    guide = F)#we see failure mileage for three states, fm is 1, is lower, for two months mileage is much higher

#DOt Plot
#Business Qns:
#What is the average failure mileage in each state
#what is the labor cost per hour in eact state
veh_avg <- vehicle %>%
  group_by(State) %>%
  summarise(avg = mean(Mileage),
            CPH = sum(lc)/sum(lh))
veh_avg

veh_avg %>% ggplot(aes(x = avg, y = State)) +
  geom_point()

#lets make some changes
veh_avg %>% ggplot(aes(x = avg, 
                       y = reorder(State, avg))) +
  geom_point() +
  theme_bw()

#if we want to swap axis
veh_avg %>% ggplot(aes(y = avg, 
                       x = reorder(State, avg))) +
  geom_point() +
  theme_bw()

#lets add in more details
veh_avg %>% ggplot(aes(x = avg, 
                       y = reorder(State, avg))) +
  geom_point(color = 'red') +#since states are in discrete, following code
  scale_y_discrete('State') +
  theme_bw()

#Lets include CPH
veh_avg %>% ggplot(aes(x = CPH, 
                       y = reorder(State, CPH))) +
  geom_point(color = 'red') +
  scale_y_discrete('State') +
  scale_x_continuous('Labor cost per hour ') +
  theme_bw()

#labor cost is high in huwaie
#some states have higher and lower side
#as a manufacturer, want to reduce these expenses 
#what are the best practises in those states, the car repairs for same kind of problems is done at lower cost
#from other states

#Scatter Plot
#Businees Qns:
#Is there any relationship between failrue mileage and month in top 3 states
veh %>% 
  ggplot(aes(x = fm,
             y= Mileage, 
             color = State)) +
  geom_point() + #dots for three states are mixed up, so..lets add trend line
  stat_smooth(se = 0) +#we see here standard intervals, we can remove standard intervals by adding se = 
  facet_wrap('State')

#we see mileage at failure vs failure month relation for eahc of th three states more or less similar

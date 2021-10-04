library(ISLR)
library(tidyverse)

#1. The aesthetics defined are the title, labels, minimal theme. The geoms are defined as point and density. 
    # The aesthetic mapping is applied to Hits on the x-axis and HmRun on the y-axis.

#2. 
set.seed(1234)
student_grade  <- rnorm(32, 7)
student_number <- round(runif(32) * 2e6 + 5e6)
programme      <- sample(c("Science", "Social Science"), 32, replace = TRUE)


gg_students <- data.frame(student_grade, student_number, programme)

#3. 
homeruns_plot <- 
  ggplot(Hitters, aes(x = HmRun, y = Hits)) +
  geom_point() +
  labs(x = "Home runs", y = "Hits")

homeruns_plot

#4. 
homeruns_plot_aes <- 
  ggplot(Hitters, aes(x = HmRun, y = Hits, colour = League, size = Salary)) +
  geom_point() +
  labs(x = "Home runs", y = "Hits")

homeruns_plot_aes

#5. Check

#6. 
grades_plot <-
  ggplot(gg_students, aes(x=student_grade)) +
  geom_histogram(binwidth = 0.5)

grades_plot

#7. 
grades_plot_dens <-
  ggplot(gg_students, aes(x=student_grade)) +
  geom_density(fill = "light seagreen")

grades_plot_dens

#8. 
grades_plot_dens_rug <-
  ggplot(gg_students, aes(x=student_grade)) +
  geom_density(fill = "light seagreen") +
  geom_rug(colour = "purple", size = 1)

grades_plot_dens_rug

#9.
grades_plot_dens_rug_min <-
  ggplot(gg_students, aes(x=student_grade)) +
  geom_density(fill = "purple", outline.type = "lower") + 
  geom_rug(colour = "purple", size = 1) +
  xlim(0, 10) + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
    ) + 
  theme_minimal()

grades_plot_dens_rug_min

#10.
grades_boxsplot <-
  ggplot(gg_students, aes(x=programme, y=student_grade, colour = programme)) +
  geom_boxplot()

grades_boxsplot

#11. The horizontal lines indicate the quartiles. The vertical lines represent the spread of the data.

#12. 
grades_plot_dens_compare <-
  ggplot(gg_students, aes(x=student_grade, colour=programme)) +
  geom_density(fill = "light seagreen", alpha=0.5)

grades_plot_dens_compare

#13.
barplot_hitters <-
  ggplot(Hitters, aes(x=Years)) +
  geom_bar()

barplot_hitters

#14. I am very sorry but I do not understand what you want me to do here with the Day variable. 
    # Volume does already stand for the daily trades?
df_Smarket <- Smarket %>%
  mutate(Day = Volume)

lineplot <-
  ggplot(Smarket[1:200,], aes(x=Volume, y=Year)) +
  geom_line()

lineplot

#15. 
lineplot <-
  ggplot(Smarket[1:200,], aes(x=Volume, y=Year)) +
  geom_line(size=2, colour="purple") +
  geom_point(size=1, colour="black")

lineplot

#16.
which.max(Smarket$Volume[1:200]) #170

Smarket_max = max((Smarket$Volume[170])) #2.33083

#17. 
lineplot <-
  ggplot(Smarket[1:200,], aes(x=Volume, y=Year)) +
  geom_line(size=2, colour="purple") +
  geom_point(size=1, colour="black") +
  geom_label(aes(x = Smarket_max, y = Year, label = "Peak volume"))

lineplot

#18. 
# Get necessary info about Salary distribution
summary(Hitters$Salary)
# Cut-offs are Q1, Q2, Q3
labs <- c("Low", "Medium", "High")

baseball <- as.data.frame(Hitters)
baseball <- baseball %>%
  filter(!is.na(Salary)) %>%
  mutate(Salary_cat = cut_number(Salary, 3, labels = labs)) %>%
  mutate(Homerun_proportion = HmRun / Hits)

#19.
scatter_bb <- 
  ggplot(baseball, aes(x=CWalks, y=Homerun_proportion)) +
  geom_point() +
  xlab("Number of career walks") +
  xlim(0, 1600) +
  ylab("Homerun proportion of total hits") +
  ylim(0, 0.4) 
  
scatter_bb

#20. 
scatter_bb_grp <- 
  ggplot(baseball, aes(x=CWalks, y=Homerun_proportion)) +
  geom_point(aes(colour = Salary_cat)) +
  xlab("Number of career walks") +
  xlim(0, 1600) +
  ylab("Homerun proportion of total hits") +
  ylim(0, 0.4) 

scatter_bb_grp


#21.
# So the data set has, according to its information page, information on stores that sell child car seats

library(SmartEDA)
cs <- as.data.frame(Carseats)

# Perform some EDA to get an understanding of the data
View(cs)
summary(cs)
dim(cs)
head(cs)
anyNA(cs)

# Overview of Data
ed_1 <- ExpData(cs, type = 1)
ed_1

# Structure of Data
ed_2 <- ExpData(cs, type = 2)
ed_2

eda_plot <- ExpNumViz(cs,target=NULL,nlim=10,Page=c(2,2),sample=7)
eda_plot[[1]]

scatter_cs_grp <- 
  ggplot(cs, aes(x=Price, y=CompPrice)) +
  geom_jitter(aes(colour = Urban)) +
  stat_smooth(method = "lm",
              col = "#e33333",
              se = FALSE,
              size = 1) +
  xlab("Price of carseats at location") +
  ylab("Competitor's price")


scatter_cs_grp







# Load libraries that were previously installed)
library(ISLR)
library(tidyverse)
library(haven)
library(readxl)

#1
object_1 <- 1:5
object_2 <- 1L:5L
object_3 <- "-123.456"
object_4 <- as.numeric(object_2)
object_5 <- letters[object_1]
object_6 <- as.factor(rep(object_5, 2))
object_7 <- c(1, 2, 3, "4", "5", "6")

class(object_7)

#2 
as.numeric(object_7)

#3
objects <- list(object_1, object_2, object_3, object_4, object_5, object_6, object_7)

#4 
df <- data.frame(object_1, object_2, object_5)

#5
ncol(df)
nrow(df)

#6
apps <- read.csv("data/googleplaystore.csv", sep=",")

#7
str(apps)
  # Yes, Size, category, price, installs all as char which is sub-optimal

#8 
head(apps)

#9
students <- read_xlsx("data/students.xlsx")
str(students)
tail(students)

#10
summary(students)

#11
filter(students, grade < 5.5)

#12
filter(students, grade > 8, programme == "A")

#13
arrange(students, programme == "B", desc(grade))

#14
select(students, student_number, programme)

#15
char_vec <- students$programme
new_char_vec <- recode(char_vec, A="Science", B="Social Science")
students_recoded <- mutate(students, programme = new_char_vec)
students_recoded

#16 
popular_apps <-
  read.csv("data/googleplaystore.csv", sep=",") %>% 
  mutate(Downloads = parse_number(Installs)) %>% 
  filter(Downloads > 500000) %>% 
  select(App, Category, Rating, Downloads, Reviews) %>%
  arrange(desc(Rating)) %>%
  distinct(App, .keep_all = TRUE)
popular_apps

#17
popular_apps %>% 
  summarise(
    mean = mean(Reviews), 
    variance = var(Reviews), 
    min = min(Reviews), 
    max = max(Reviews)
  )

#18
mad <- function(x) {
  median(abs(x - median(x)))
}

popular_apps %>% 
  summarise(
    mean = mean(Reviews), 
    variance = var(Reviews), 
    min = min(Reviews), 
    max = max(Reviews),
    median = mad(Reviews)
  )

#19
popular_apps %>% 
  group_by(Category) %>% 
  summarise(
    mean = mean(Rating), 
    variance = var(Rating), 
    min = min(Rating), 
    max = max(Rating),
    median = mad(Rating)
  )

#20

  # Research questions about the Google Playstore Dataset: Which apps have the highest controversy regarding rating?
rating_controversy <- popular_apps %>% 
  group_by(Category) %>% 
  summarise(
    mean = mean(Rating), 
    variance = var(Rating), 
    min = min(Rating), 
    max = max(Rating)
  )

arrange(rating_controversy, desc(variance))














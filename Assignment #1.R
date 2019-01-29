# Alex Nestorov
# Econ 613
# Assignment #1

# Pull in data, summarize, and view/analyze to understand
datsss <- read.csv("~/Documents/ECON 613/dat/datsss.csv")
summary(datsss)
str(datsss)
View(datsss)
datstu <- read.csv("~/Documents/ECON 613/dat/datstu.csv")
summary(datstu)
str(datstu)
View(datstu)
datjss <- read.csv("~/Documents/ECON 613/dat/datjss.csv")
summary(datjss)
str(datjss)
View(datjss)
install.packages("stringr")
library(stringr)

# Exercise 1
# No. of students
length(datstu$X)
## I find that there are 340,823 students.

# No. of schools
length(unique(datsss$schoolcode))
## Initially, I get a result of 898 schools. However, there are schools in the data file with missing
## information or NAs. If I remove these lines of data (in a copied database so as to not affect
## the original) I find 689 schools. That means that 209 schools have missing or NA data. 
datsss_copy <- datsss[complete.cases(datsss[, 5:6]),]
length(unique(datsss_copy$schoolcode))

# No. of programs
uq_pgm <- unique(c(levels(datstu$choicepgm1), levels(datstu$choicepgm2), levels(datstu$choicepgm3), 
             levels(datstu$choicepgm4), levels(datstu$choicepgm5), levels(datstu$choicepgm6)))
View(uq_pgm)
length(uq_pgm)
## Initially, I find that there are 33 programs. However, upon closer inspection, there is one blank
## value being included in these 33, so I have to subtract 1 to get the final answer of 32 programs.

# No. of choices (school, program bundle)
datstu$choice1 <- paste(datstu$schoolcode1, datstu$choicepgm1, sep = "")
datstu$choice2 <- paste(datstu$schoolcode2, datstu$choicepgm2, sep = "")
datstu$choice3 <- paste(datstu$schoolcode3, datstu$choicepgm3, sep = "")
datstu$choice4 <- paste(datstu$schoolcode4, datstu$choicepgm4, sep = "")
datstu$choice5 <- paste(datstu$schoolcode5, datstu$choicepgm5, sep = "")
datstu$choice6 <- paste(datstu$schoolcode6, datstu$choicepgm6, sep = "")
all_choices <- unique(c(datstu$choice1, datstu$choice2, datstu$choice3, datstu$choice4,
                        datstu$choice5, datstu$choice6))
all_choices <- all_choices[!grepl('NA',all_choices)]
all_choices <- all_choices[!grepl('^\\d+$',all_choices)]
length(all_choices)
## First, I must concatenate all possible school code and program choices and then collapse this to
## all of the unique values. Once I do so, I find there are cases where there is either a "NA" or
## a blank in the school code and/or program choices. I need to remove these (given these were
## obviously some sort of data entry error) and arrive at 2,773 choices.

# No. missing test score
sum(is.na(datstu$score))
## I find that there are 179,887 missing test scores.

# No. apply to same school (different programs)
sum(apply(datstu[match("schoolcode1", names(datstu)):match("schoolcode6", names(datstu))], 
          1, function(x) any(duplicated(x))))
## I find that there are 133,668 individuals who apply to the same school code but different programs.

# No. apply to less than 6 choices
datstu$no_choices <- rowSums(is.na(datstu[match("schoolcode1", names(datstu)):
                                         match("schoolcode6", names(datstu))]))
sum(datstu$no_choices > 0)
## I sum the number of school codes showing up as "NA" in the data (which is a proxy and equivalent
## to the number of choices not being applied to). I sum the number of individuals with more than 0 
## choices that are "NA" to find that there are 17,734 individuals who apply to less than 6 choices.

# Exercise 2
# Create a school level dataset, where each row corresponds to a (school, program) choice
##  District, latitude, longitude
datstu$choiceadmit <- 
  with(datstu, 
       ifelse(rankplace == 1, choice1,
              ifelse(rankplace == 2, choice2,
                     ifelse(rankplace == 3, choice3,
                            ifelse(rankplace == 4, choice4,
                                   ifelse(rankplace == 5, choice5,
                                          ifelse (rankplace == 6, choice6,
                                                  ifelse(rankplace == 99, "Not Admit", "NA"))))))))
## I first create a column with the choices that students were admitted to from 1-6. 

school_data <- data.frame(choices = all_choices)
school_data$schoolcode <- str_extract(school_data$choices, "[0-9]+")
school_data$choicepgm <- gsub("[0-9]+","",school_data$choices)
school_data <- unique(merge(school_data, datsss_copy[, c("schoolcode", "sssdistrict", 
                                                         "ssslong", "ssslat")], by = "schoolcode"))
## I then create a school-level dataset and pull in the district, longitude, and latitude of each
## school.

install.packages("dplyr")
library(dplyr)
choice_calcs <- datstu %>%
  group_by(choiceadmit) %>%
  summarize(cutoff = min(score), quality = mean(score), size = length(score))
school_data <- merge(school_data, choice_calcs[, c("choiceadmit", "cutoff", "quality", "size")], 
                            by.x = "choices", by.y = "choiceadmit", all.x = TRUE)
school_data <- school_data %>%
  mutate(size = ifelse(is.na(size), 0, size))
## I then use a package named dplyr to calculate the cutoff, quality, and size of each choice, as
## defined by the problem, and merged the mathematical data with the school-level dataset from above.
## I found that there was a difference between the number of choices as originally found in (1) = 
## 2,733 and the number of choices for which we have actual score data, meaning the data was missing
## or nobody was admitted to that specific choice. Namely, there are 473 choices that did not have 
## score data and so did not have any cutoff or quality data (including those not admitted to any
## of their choices and "NA").

# Exercise 3
# Use given formula to calculate the distance between junior HS and senior HS
datjss_copy <- datjss
colnames(datjss_copy) <- c("X", "jssdistrict", "jsslong", "jsslat")
datstu_calcs <- datstu[,c("X", "score", "rankplace", "choiceadmit", "schoolcode1", "schoolcode2", 
                          "schoolcode3", "schoolcode4", "schoolcode5", "schoolcode6", "jssdistrict", 
                          "choice1", "choice2", "choice3", "choice4", "choice5", "choice6")]
datstu_calcs <- unique(merge(datstu_calcs, datjss_copy[, c("jssdistrict", "jsslong", "jsslat")],
                                          by = "jssdistrict", all.x = TRUE))
## First, I create a copy of datjss so that we don't make any changes to the raw data files. Next, 
## I am making a copy of "datstu" that contains fewer columns so that the code runs quicker. Finally,
## I am merging these two datasets.

sum(is.na(datstu_calcs$jsslong))
## As an aside, I found that there are 25 entries with a "NA" in jssdistrict in datstu.

datstu_calcs <- unique(merge(datstu_calcs, datsss_copy[, c("schoolcode", "ssslong", "ssslat")],
                             by.x = "schoolcode1", by.y = "schoolcode", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "ssslong"] <- "ssslong1"
  names(datstu_calcs)[names(datstu_calcs) == "ssslat"] <- "ssslat1"
datstu_calcs <- unique(merge(datstu_calcs, datsss_copy[, c("schoolcode", "ssslong", "ssslat")],
                         by.x = "schoolcode2", by.y = "schoolcode", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "ssslong"] <- "ssslong2"
  names(datstu_calcs)[names(datstu_calcs) == "ssslat"] <- "ssslat2"
datstu_calcs <- unique(merge(datstu_calcs, datsss_copy[, c("schoolcode", "ssslong", "ssslat")],
                         by.x = "schoolcode3", by.y = "schoolcode", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "ssslong"] <- "ssslong3"
  names(datstu_calcs)[names(datstu_calcs) == "ssslat"] <- "ssslat3"
datstu_calcs <- unique(merge(datstu_calcs, datsss_copy[, c("schoolcode", "ssslong", "ssslat")],
                         by.x = "schoolcode4", by.y = "schoolcode", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "ssslong"] <- "ssslong4"
  names(datstu_calcs)[names(datstu_calcs) == "ssslat"] <- "ssslat4"
datstu_calcs <- unique(merge(datstu_calcs, datsss_copy[, c("schoolcode", "ssslong", "ssslat")],
                         by.x = "schoolcode5", by.y = "schoolcode", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "ssslong"] <- "ssslong5"
  names(datstu_calcs)[names(datstu_calcs) == "ssslat"] <- "ssslat5"
datstu_calcs <- unique(merge(datstu_calcs, datsss_copy[, c("schoolcode", "ssslong", "ssslat")],
                         by.x = "schoolcode6", by.y = "schoolcode", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "ssslong"] <- "ssslong6"
  names(datstu_calcs)[names(datstu_calcs) == "ssslat"] <- "ssslat6"
## Next, I wanted to merge in the longitude and latitude of each individual school that was applied to
## so we can understand the location of each school for each of ranked choices 1-6. Ideally, I would 
## do this through a "for (i in 1:6) loop if there were many more choices than just 6, but when I 
## tried to do so it took much longer for R to process than having individual code for each choice.
  
datstu_calcs$dist1 <- 
  sqrt(((69.172*(datstu_calcs$ssslong1-datstu_calcs$jsslong)*cos(datstu_calcs$jsslat/57.3))^2)+
         (69.172*(datstu_calcs$ssslat1-datstu_calcs$jsslat))^2)
datstu_calcs$dist2 <- 
  sqrt(((69.172*(datstu_calcs$ssslong2-datstu_calcs$jsslong)*cos(datstu_calcs$jsslat/57.3))^2)+
         (69.172*(datstu_calcs$ssslat2-datstu_calcs$jsslat))^2)
datstu_calcs$dist3 <- 
  sqrt(((69.172*(datstu_calcs$ssslong3-datstu_calcs$jsslong)*cos(datstu_calcs$jsslat/57.3))^2)+
         (69.172*(datstu_calcs$ssslat3-datstu_calcs$jsslat))^2)
datstu_calcs$dist4 <- 
  sqrt(((69.172*(datstu_calcs$ssslong4-datstu_calcs$jsslong)*cos(datstu_calcs$jsslat/57.3))^2)+
         (69.172*(datstu_calcs$ssslat4-datstu_calcs$jsslat))^2)
datstu_calcs$dist5 <- 
  sqrt(((69.172*(datstu_calcs$ssslong5-datstu_calcs$jsslong)*cos(datstu_calcs$jsslat/57.3))^2)+
         (69.172*(datstu_calcs$ssslat5-datstu_calcs$jsslat))^2)
datstu_calcs$dist6 <- 
  sqrt(((69.172*(datstu_calcs$ssslong6-datstu_calcs$jsslong)*cos(datstu_calcs$jsslat/57.3))^2)+
         (69.172*(datstu_calcs$ssslat6-datstu_calcs$jsslat))^2)
## Finally, I calculated the distances for each school that was applied to by each individual using
## the given equation. Again, this would ideally be calculated through a loop but I found that this
## method took much more time than having individual code for each column.

# Exercise 4
# Report avg and SD for each ranked choice
datstu_calcs <- unique(merge(datstu_calcs, school_data[, c("choices", "cutoff", "quality")],
                             by.x = "choice1", by.y = "choices", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "cutoff"] <- "cutoff1"
  names(datstu_calcs)[names(datstu_calcs) == "quality"] <- "quality1"
datstu_calcs <- unique(merge(datstu_calcs, school_data[, c("choices", "cutoff", "quality")],
                             by.x = "choice2", by.y = "choices", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "cutoff"] <- "cutoff2"
  names(datstu_calcs)[names(datstu_calcs) == "quality"] <- "quality2"
datstu_calcs <- unique(merge(datstu_calcs, school_data[, c("choices", "cutoff", "quality")],
                               by.x = "choice3", by.y = "choices", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "cutoff"] <- "cutoff3"
  names(datstu_calcs)[names(datstu_calcs) == "quality"] <- "quality3"  
datstu_calcs <- unique(merge(datstu_calcs, school_data[, c("choices", "cutoff", "quality")],
                               by.x = "choice4", by.y = "choices", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "cutoff"] <- "cutoff4"
  names(datstu_calcs)[names(datstu_calcs) == "quality"] <- "quality4"
datstu_calcs <- unique(merge(datstu_calcs, school_data[, c("choices", "cutoff", "quality")],
                               by.x = "choice5", by.y = "choices", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "cutoff"] <- "cutoff5"
  names(datstu_calcs)[names(datstu_calcs) == "quality"] <- "quality5"
datstu_calcs <- unique(merge(datstu_calcs, school_data[, c("choices", "cutoff", "quality")],
                               by.x = "choice6", by.y = "choices", all.x = TRUE))
  names(datstu_calcs)[names(datstu_calcs) == "cutoff"] <- "cutoff6"
  names(datstu_calcs)[names(datstu_calcs) == "quality"] <- "quality6"
## First, I added in the cutoff and quality for each of the individual's 6 ranked choices by merging
## these values in from the individual school-level data. Again, this would ideally be calculated 
## through a loop but I found that this method took much more time than having individual code for 
## each column.
  
rank_choices <- data.frame("rankchoice" = c(1:6))
rank_choices$avgcutoff <- 
  c(mean(datstu_calcs$cutoff1, na.rm = TRUE), mean(datstu_calcs$cutoff2, na.rm = TRUE),
    mean(datstu_calcs$cutoff3, na.rm = TRUE), mean(datstu_calcs$cutoff4, na.rm = TRUE),
    mean(datstu_calcs$cutoff5, na.rm = TRUE), mean(datstu_calcs$cutoff6, na.rm = TRUE))
rank_choices$sdcutoff <- 
  c(sd(datstu_calcs$cutoff1, na.rm = TRUE), sd(datstu_calcs$cutoff2, na.rm = TRUE),
    sd(datstu_calcs$cutoff3, na.rm = TRUE), sd(datstu_calcs$cutoff4, na.rm = TRUE),
    sd(datstu_calcs$cutoff5, na.rm = TRUE), sd(datstu_calcs$cutoff6, na.rm = TRUE))
rank_choices$avgquality <- 
  c(mean(datstu_calcs$quality1, na.rm = TRUE), mean(datstu_calcs$quality2, na.rm = TRUE),
    mean(datstu_calcs$quality3, na.rm = TRUE), mean(datstu_calcs$quality4, na.rm = TRUE),
    mean(datstu_calcs$quality5, na.rm = TRUE), mean(datstu_calcs$quality6, na.rm = TRUE))
rank_choices$sdquality <- 
  c(sd(datstu_calcs$quality1, na.rm = TRUE), sd(datstu_calcs$quality2, na.rm = TRUE),
    sd(datstu_calcs$quality3, na.rm = TRUE), sd(datstu_calcs$quality4, na.rm = TRUE),
    sd(datstu_calcs$quality5, na.rm = TRUE), sd(datstu_calcs$quality6, na.rm = TRUE))
rank_choices$avgdistance <- 
  c(mean(datstu_calcs$dist1, na.rm = TRUE), mean(datstu_calcs$dist2, na.rm = TRUE),
    mean(datstu_calcs$dist3, na.rm = TRUE), mean(datstu_calcs$dist4, na.rm = TRUE),
    mean(datstu_calcs$dist5, na.rm = TRUE), mean(datstu_calcs$dist6, na.rm = TRUE))
rank_choices$sddistance <- 
  c(sd(datstu_calcs$dist1, na.rm = TRUE), sd(datstu_calcs$dist2, na.rm = TRUE),
    sd(datstu_calcs$dist3, na.rm = TRUE), sd(datstu_calcs$dist4, na.rm = TRUE),
    sd(datstu_calcs$dist5, na.rm = TRUE), sd(datstu_calcs$dist6, na.rm = TRUE))
print(rank_choices)
## Finally, I create a new data table to house the cutoff, quality, and distance average/standard
## deviations by each ranked choice. Again, this would ideally be calculated through a loop but
## given only 6 ranked choices was quicker to run as seen above.

# Redo same table, differentiating by student test score quantiles
datstu_calcs$scorequartile <- cut(datstu_calcs$score, 
                              breaks = quantile(datstu_calcs$score, seq(0, 1, by = 0.25), na.rm = TRUE), 
                              include.lowest = TRUE, labels = 1:4)
## First, I calculated the student test score quartiles and assigned labels of 1 through 4 for their
## respective quartile, where 1 = 0-25th %ile, 2 = 25th-50th %ile, 3 = 50th-75th %ile, and
## 4 = 75th-100 %ile.

datstu_firstq <- datstu_calcs %>%
  filter(scorequartile == 1)
first_quartile_choices <- data.frame("first quartile score by ranked choices" = c(1:6))
first_quartile_choices$avgcutoff <- 
  c(mean(datstu_firstq$cutoff1, na.rm = TRUE), mean(datstu_firstq$cutoff2, na.rm = TRUE),
    mean(datstu_firstq$cutoff3, na.rm = TRUE), mean(datstu_firstq$cutoff4, na.rm = TRUE),
    mean(datstu_firstq$cutoff5, na.rm = TRUE), mean(datstu_firstq$cutoff6, na.rm = TRUE))
first_quartile_choices$sdcutoff <- 
  c(sd(datstu_firstq$cutoff1, na.rm = TRUE), sd(datstu_firstq$cutoff2, na.rm = TRUE),
    sd(datstu_firstq$cutoff3, na.rm = TRUE), sd(datstu_firstq$cutoff4, na.rm = TRUE),
    sd(datstu_firstq$cutoff5, na.rm = TRUE), sd(datstu_firstq$cutoff6, na.rm = TRUE))
first_quartile_choices$avgquality <- 
  c(mean(datstu_firstq$quality1, na.rm = TRUE), mean(datstu_firstq$quality2, na.rm = TRUE),
    mean(datstu_firstq$quality3, na.rm = TRUE), mean(datstu_firstq$quality4, na.rm = TRUE),
    mean(datstu_firstq$quality5, na.rm = TRUE), mean(datstu_firstq$quality6, na.rm = TRUE))
first_quartile_choices$sdquality <- 
  c(sd(datstu_firstq$quality1, na.rm = TRUE), sd(datstu_firstq$quality2, na.rm = TRUE),
    sd(datstu_firstq$quality3, na.rm = TRUE), sd(datstu_firstq$quality4, na.rm = TRUE),
    sd(datstu_firstq$quality5, na.rm = TRUE), sd(datstu_firstq$quality6, na.rm = TRUE))
first_quartile_choices$avgdistance <- 
  c(mean(datstu_firstq$dist1, na.rm = TRUE), mean(datstu_firstq$dist2, na.rm = TRUE),
    mean(datstu_firstq$dist3, na.rm = TRUE), mean(datstu_firstq$dist4, na.rm = TRUE),
    mean(datstu_firstq$dist5, na.rm = TRUE), mean(datstu_firstq$dist6, na.rm = TRUE))
first_quartile_choices$sddistance <- 
  c(sd(datstu_firstq$dist1, na.rm = TRUE), sd(datstu_firstq$dist2, na.rm = TRUE),
    sd(datstu_firstq$dist3, na.rm = TRUE), sd(datstu_firstq$dist4, na.rm = TRUE),
    sd(datstu_firstq$dist5, na.rm = TRUE), sd(datstu_firstq$dist6, na.rm = TRUE))
print(first_quartile_choices)

datstu_secondq <- datstu_calcs %>%
  filter(scorequartile == 2)
second_quartile_choices <- data.frame("second quartile score by ranked choices" = c(1:6))
second_quartile_choices$avgcutoff <- 
  c(mean(datstu_secondq$cutoff1, na.rm = TRUE), mean(datstu_secondq$cutoff2, na.rm = TRUE),
    mean(datstu_secondq$cutoff3, na.rm = TRUE), mean(datstu_secondq$cutoff4, na.rm = TRUE),
    mean(datstu_secondq$cutoff5, na.rm = TRUE), mean(datstu_secondq$cutoff6, na.rm = TRUE))
second_quartile_choices$sdcutoff <- 
  c(sd(datstu_secondq$cutoff1, na.rm = TRUE), sd(datstu_secondq$cutoff2, na.rm = TRUE),
    sd(datstu_secondq$cutoff3, na.rm = TRUE), sd(datstu_secondq$cutoff4, na.rm = TRUE),
    sd(datstu_secondq$cutoff5, na.rm = TRUE), sd(datstu_secondq$cutoff6, na.rm = TRUE))
second_quartile_choices$avgquality <- 
  c(mean(datstu_secondq$quality1, na.rm = TRUE), mean(datstu_secondq$quality2, na.rm = TRUE),
    mean(datstu_secondq$quality3, na.rm = TRUE), mean(datstu_secondq$quality4, na.rm = TRUE),
    mean(datstu_secondq$quality5, na.rm = TRUE), mean(datstu_secondq$quality6, na.rm = TRUE))
second_quartile_choices$sdquality <- 
  c(sd(datstu_secondq$quality1, na.rm = TRUE), sd(datstu_secondq$quality2, na.rm = TRUE),
    sd(datstu_secondq$quality3, na.rm = TRUE), sd(datstu_secondq$quality4, na.rm = TRUE),
    sd(datstu_secondq$quality5, na.rm = TRUE), sd(datstu_secondq$quality6, na.rm = TRUE))
second_quartile_choices$avgdistance <- 
  c(mean(datstu_secondq$dist1, na.rm = TRUE), mean(datstu_secondq$dist2, na.rm = TRUE),
    mean(datstu_secondq$dist3, na.rm = TRUE), mean(datstu_secondq$dist4, na.rm = TRUE),
    mean(datstu_secondq$dist5, na.rm = TRUE), mean(datstu_secondq$dist6, na.rm = TRUE))
second_quartile_choices$sddistance <- 
  c(sd(datstu_secondq$dist1, na.rm = TRUE), sd(datstu_secondq$dist2, na.rm = TRUE),
    sd(datstu_secondq$dist3, na.rm = TRUE), sd(datstu_secondq$dist4, na.rm = TRUE),
    sd(datstu_secondq$dist5, na.rm = TRUE), sd(datstu_secondq$dist6, na.rm = TRUE))
print(second_quartile_choices)

datstu_thirdq <- datstu_calcs %>%
  filter(scorequartile == 3)
third_quartile_choices <- data.frame("third quartile %ile score by ranked choices" = c(1:6))
third_quartile_choices$avgcutoff <- 
  c(mean(datstu_thirdq$cutoff1, na.rm = TRUE), mean(datstu_thirdq$cutoff2, na.rm = TRUE),
    mean(datstu_thirdq$cutoff3, na.rm = TRUE), mean(datstu_thirdq$cutoff4, na.rm = TRUE),
    mean(datstu_thirdq$cutoff5, na.rm = TRUE), mean(datstu_thirdq$cutoff6, na.rm = TRUE))
third_quartile_choices$sdcutoff <- 
  c(sd(datstu_thirdq$cutoff1, na.rm = TRUE), sd(datstu_thirdq$cutoff2, na.rm = TRUE),
    sd(datstu_thirdq$cutoff3, na.rm = TRUE), sd(datstu_thirdq$cutoff4, na.rm = TRUE),
    sd(datstu_thirdq$cutoff5, na.rm = TRUE), sd(datstu_thirdq$cutoff6, na.rm = TRUE))
third_quartile_choices$avgquality <- 
  c(mean(datstu_thirdq$quality1, na.rm = TRUE), mean(datstu_thirdq$quality2, na.rm = TRUE),
    mean(datstu_thirdq$quality3, na.rm = TRUE), mean(datstu_thirdq$quality4, na.rm = TRUE),
    mean(datstu_thirdq$quality5, na.rm = TRUE), mean(datstu_thirdq$quality6, na.rm = TRUE))
third_quartile_choices$sdquality <- 
  c(sd(datstu_thirdq$quality1, na.rm = TRUE), sd(datstu_thirdq$quality2, na.rm = TRUE),
    sd(datstu_thirdq$quality3, na.rm = TRUE), sd(datstu_thirdq$quality4, na.rm = TRUE),
    sd(datstu_thirdq$quality5, na.rm = TRUE), sd(datstu_thirdq$quality6, na.rm = TRUE))
third_quartile_choices$avgdistance <- 
  c(mean(datstu_thirdq$dist1, na.rm = TRUE), mean(datstu_thirdq$dist2, na.rm = TRUE),
    mean(datstu_thirdq$dist3, na.rm = TRUE), mean(datstu_thirdq$dist4, na.rm = TRUE),
    mean(datstu_thirdq$dist5, na.rm = TRUE), mean(datstu_thirdq$dist6, na.rm = TRUE))
third_quartile_choices$sddistance <- 
  c(sd(datstu_thirdq$dist1, na.rm = TRUE), sd(datstu_thirdq$dist2, na.rm = TRUE),
    sd(datstu_thirdq$dist3, na.rm = TRUE), sd(datstu_thirdq$dist4, na.rm = TRUE),
    sd(datstu_thirdq$dist5, na.rm = TRUE), sd(datstu_thirdq$dist6, na.rm = TRUE))
print(third_quartile_choices)

datstu_fourthq <- datstu_calcs %>%
  filter(scorequartile == 4)
fourth_quartile_choices <- data.frame("fourth quartile score by ranked choices" = c(1:6))
fourth_quartile_choices$avgcutoff <- 
  c(mean(datstu_fourthq$cutoff1, na.rm = TRUE), mean(datstu_fourthq$cutoff2, na.rm = TRUE),
    mean(datstu_fourthq$cutoff3, na.rm = TRUE), mean(datstu_fourthq$cutoff4, na.rm = TRUE),
    mean(datstu_fourthq$cutoff5, na.rm = TRUE), mean(datstu_fourthq$cutoff6, na.rm = TRUE))
fourth_quartile_choices$sdcutoff <- 
  c(sd(datstu_fourthq$cutoff1, na.rm = TRUE), sd(datstu_fourthq$cutoff2, na.rm = TRUE),
    sd(datstu_fourthq$cutoff3, na.rm = TRUE), sd(datstu_fourthq$cutoff4, na.rm = TRUE),
    sd(datstu_fourthq$cutoff5, na.rm = TRUE), sd(datstu_fourthq$cutoff6, na.rm = TRUE))
fourth_quartile_choices$avgquality <- 
  c(mean(datstu_fourthq$quality1, na.rm = TRUE), mean(datstu_fourthq$quality2, na.rm = TRUE),
    mean(datstu_fourthq$quality3, na.rm = TRUE), mean(datstu_fourthq$quality4, na.rm = TRUE),
    mean(datstu_fourthq$quality5, na.rm = TRUE), mean(datstu_fourthq$quality6, na.rm = TRUE))
fourth_quartile_choices$sdquality <- 
  c(sd(datstu_fourthq$quality1, na.rm = TRUE), sd(datstu_fourthq$quality2, na.rm = TRUE),
    sd(datstu_fourthq$quality3, na.rm = TRUE), sd(datstu_fourthq$quality4, na.rm = TRUE),
    sd(datstu_fourthq$quality5, na.rm = TRUE), sd(datstu_fourthq$quality6, na.rm = TRUE))
fourth_quartile_choices$avgdistance <- 
  c(mean(datstu_fourthq$dist1, na.rm = TRUE), mean(datstu_fourthq$dist2, na.rm = TRUE),
    mean(datstu_fourthq$dist3, na.rm = TRUE), mean(datstu_fourthq$dist4, na.rm = TRUE),
    mean(datstu_fourthq$dist5, na.rm = TRUE), mean(datstu_fourthq$dist6, na.rm = TRUE))
fourth_quartile_choices$sddistance <- 
  c(sd(datstu_fourthq$dist1, na.rm = TRUE), sd(datstu_fourthq$dist2, na.rm = TRUE),
    sd(datstu_fourthq$dist3, na.rm = TRUE), sd(datstu_fourthq$dist4, na.rm = TRUE),
    sd(datstu_fourthq$dist5, na.rm = TRUE), sd(datstu_fourthq$dist6, na.rm = TRUE))
print(fourth_quartile_choices)
## I then split the datstu_calcs table into 4 tables, 1 for each quartile (for ease of creating)
## the summary statistics. Finally, as I did earlier in part (4), I created a table reporting the
## average and standard deviation for each ranked choice for each of the 3 variables differentiated
## by each student test quartile, i.e.a unique table for each student test score quartile. Again, 
## loops would have been ideal here but I found that it took R way too much time to process the
## same information through a loop rather than just copying & pasting and changing the references
## to the correct data.

# Exercise 5
# Group schools by decile of selectivity (cutoffs) and compute for each individual the # of groups
# in the application.
school_data$decile <- cut(school_data$cutoff, 
                          breaks = quantile(school_data$cutoff, seq(0, 1, by = 0.1), na.rm = TRUE),
                          include.lowest = TRUE, labels = 1:10)
## First, I have computed the deciles of cutoffs for each choice (school, program bundle).

datstu_groups <- datstu[,c("X", "score", "choice1", "choice2", "choice3", "choice4", "choice5", 
                           "choice6")]
datstu_groups <- unique(merge(datstu_groups, school_data[, c("choices", "decile")], by.x = "choice1",
                              by.y = "choices", all.x = TRUE))
  names(datstu_groups)[names(datstu_groups) == "decile"] <- "decile1"
datstu_groups <- unique(merge(datstu_groups, school_data[, c("choices", "decile")], by.x = "choice2",
                               by.y = "choices", all.x = TRUE))
  names(datstu_groups)[names(datstu_groups) == "decile"] <- "decile2"
datstu_groups <- unique(merge(datstu_groups, school_data[, c("choices", "decile")], by.x = "choice3",
                               by.y = "choices", all.x = TRUE))
  names(datstu_groups)[names(datstu_groups) == "decile"] <- "decile3"
datstu_groups <- unique(merge(datstu_groups, school_data[, c("choices", "decile")], by.x = "choice4",
                               by.y = "choices", all.x = TRUE))
  names(datstu_groups)[names(datstu_groups) == "decile"] <- "decile4"
datstu_groups <- unique(merge(datstu_groups, school_data[, c("choices", "decile")], by.x = "choice5",
                               by.y = "choices", all.x = TRUE))
  names(datstu_groups)[names(datstu_groups) == "decile"] <- "decile5"
datstu_groups <- unique(merge(datstu_groups, school_data[, c("choices", "decile")], by.x = "choice6",
                               by.y = "choices", all.x = TRUE))
  names(datstu_groups)[names(datstu_groups) == "decile"] <- "decile6"
## Next, I am merging these deciles into a copy of "datstu" that contains fewer columns so that the
## code runs quicker. Ideally, I'd use a loop, but it was taking R a long time to do so.
  
datstu_groups$nodeciles <- apply(datstu_groups[match("decile1", names(datstu_groups)):
                                              match("decile6", names(datstu_groups))], 1, 
                              function(x) length(unique(x)))
datstu_groups$nadeciles <- ifelse(rowSums(is.na(datstu_groups[match("decile1", names(datstu_groups)):
                                                   match("decile6", names(datstu_groups))]))>0,1,0)
datstu_groups$groups <- datstu_groups$nodeciles - datstu_groups$nadeciles
## The column "groups" will contain the number of groups of deciles to which each individual applied.

# Redo this, by student test score (quantile)
datstu_groups$scorequartile <- cut(datstu_groups$score, 
                                   breaks = quantile(datstu_groups$score, seq(0, 1, by = 0.25), 
                                                    na.rm = TRUE), include.lowest = TRUE, labels = 1:4)
## First, I add the quantile data to the groups dataset I created above.

datstu_groupstats <- datstu_groups %>%
  group_by(scorequartile) %>%
  summarize(MinGroups = min(groups, na.rm = TRUE), AvgGroups = mean(groups, na.rm = TRUE), 
            MaxGroups = max(groups, na.rm = TRUE), SDGroups = sd(groups, na.rm = TRUE))
datstu_groupstats <- na.omit(datstu_groupstats)
print(datstu_groupstats)

datstu_decilestats <- datstu_groups %>%
  group_by(scorequartile) %>%
  summarize(avgdecile1 = mean(as.numeric(decile1), na.rm = TRUE), sddecile1 = sd(decile1, na.rm = TRUE),
            avgdecile2 = mean(as.numeric(decile2), na.rm = TRUE), sddecile2 = sd(decile2, na.rm = TRUE),
            avgdecile3 = mean(as.numeric(decile3), na.rm = TRUE), sddecile3 = sd(decile3, na.rm = TRUE),
            avgdecile4 = mean(as.numeric(decile4), na.rm = TRUE), sddecile4 = sd(decile4, na.rm = TRUE),
            avgdecile5 = mean(as.numeric(decile5), na.rm = TRUE), sddecile5 = sd(decile5, na.rm = TRUE),
            avgdecile6 = mean(as.numeric(decile6), na.rm = TRUE), sddecile6 = sd(decile6, na.rm = TRUE))
datstu_decilestats <- na.omit(datstu_decilestats)
print(datstu_decilestats)
## I then created two summaries of the grouping data by quartile. First, I looked at some descriptive
## statistics of the groups themselves by quartile. In the last table, I look at descriptive statistics
## (specifically average and standard deviation) of the deciles that we calculated above, to get a
## sense of how students with scores in different quartiles made decisions on the strength of school
## to which they would apply.

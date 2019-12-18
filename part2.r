## -----------------------------------------------------------------------------
library("knitr")
library("caret")


## -----------------------------------------------------------------------------
purl("index.Rmd", output = "part1.r")
source("part1.r")


## -----------------------------------------------------------------------------
myvars <- c("grade_id", "height", "weight", "started", "sex")
grade_model_data <- hardestWithYear[myvars]
grade_model_data <- na.omit(grade_model_data)


## -----------------------------------------------------------------------------
set.seed(926)
sample <- sample.int(n = nrow(grade_model_data), 
                     size = floor(.90*nrow(grade_model_data)), # Selecting 90% of data
                     replace = F)

train_data <- grade_model_data[sample, ]
test_data  <- grade_model_data[-sample, ]


## -----------------------------------------------------------------------------
grade_model <- lm(train_data, formula = grade_id ~ height + weight + started + sex)
summary(grade_model)


## -----------------------------------------------------------------------------
grade_predictions <- grade_model %>% predict(test_data)
grade_predictions <- as.data.frame(grade_predictions)
colnames(grade_predictions)[colnames(grade_predictions) == "grade_predictions"] <- "grade_id"
grade_predictions["grade_id"] <- floor(grade_predictions["grade_id"])
RMSE(grade_predictions, test_data$grade_id)/mean(test_data$grade_id)


## -----------------------------------------------------------------------------
injuries = read.csv("climbInjuries.csv", header = TRUE)


## -----------------------------------------------------------------------------
new_per_year <- count(userInfo, year_started)
colnames(new_per_year)[colnames(new_per_year) == "year_started"] <- "year"
colnames(new_per_year)[colnames(new_per_year) == "n"] <- "num_new_climbers"
new_per_year <- new_per_year[!rowSums(new_per_year[1] <1981),]
new_per_year <- new_per_year[!rowSums(new_per_year[1] > 2013),]
new_per_year <- na.omit(new_per_year)
injuries <- merge(injuries,new_per_year, by="year")


## -----------------------------------------------------------------------------
colnames(injuries)


## -----------------------------------------------------------------------------
injury_plot <- ggplot(injuries, aes(x=year)) + geom_point(aes(y=num_new_climbers, colour ='blue')) +
    geom_point(aes(y=num_of_accidents, colour ='red')) + ylab("Number of Climbers") + xlab("Year") +    scale_color_manual(labels = c("# of New Climbers", "# of Accidents"), values = c("blue", "red"))

injury_plot


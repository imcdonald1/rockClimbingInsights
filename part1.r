## ----a------------------------------------------------------------------------
library("tidyverse")
library("RSQLite")
library("ggplot2")
library("RColorBrewer")


## ----x------------------------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), "database.sqlite")
dbListTables(con)


## ----b------------------------------------------------------------------------

ascents <- dbGetQuery(con, "SELECT * FROM ASCENT;")
grades <- dbGetQuery(con, "SELECT * FROM GRADE");
methods <- dbGetQuery(con, "SELECT * FROM METHOD;")
users <- dbGetQuery(con, "SELECT * FROM USER;")



## ----c------------------------------------------------------------------------
dim(users)
colnames(users)


## ----d------------------------------------------------------------------------
colnames(users)[colnames(users) == 'id'] <- 'user_id'
users[, 7:9][users[,7:9]==0] <- NA
users[, 10:20][users[,10:20]==""] <- NA
userInfo <- dplyr::tibble(id=users$user_id, height=users$height, weight=users$weight, city=users$city, country=users$country, year_started=users$started, occupation=users$occupation, birthday=users$birth)


## ----dd-----------------------------------------------------------------------
dim(ascents)
colnames(ascents)


## ----ddq----------------------------------------------------------------------
ascents[, 4][ascents[,4]==""] <- NA
ascents[, 14][ascents[,14]==""] <- NA
ascents[, 15][ascents[,15]==0] <- NA
ascents[, 16][ascents[,16]==""] <- NA
ascents[, 17][ascents[,17]==0] <- NA
ascents[, 18][ascents[,18]==""] <- NA
ascents[, 19][ascents[,19]==""] <- NA
ascents[, 20][ascents[,20]=="\n"] <- NA
ascents$raw_notes <- NULL


## ----ddx----------------------------------------------------------------------
dim(grades)
colnames(grades)


## ----ddaa---------------------------------------------------------------------
grades[, 1:14][grades[,1:14]==0] <- NA


## ----ddda---------------------------------------------------------------------
dim(methods)
colnames(methods)


## ----xq-----------------------------------------------------------------------
methods$name <- as.factor(methods$name)
levels(methods$name)


## ----qs-----------------------------------------------------------------------
plot <- ggplot(ascents, aes(x=grade_id)) + geom_bar() + xlab("Grade") + ylab("Number of Logged Climbs")
plot


## ----qss----------------------------------------------------------------------
hardestClimb <- ascents %>% group_by(user_id) %>% filter(grade_id == max(grade_id))
hardestWithYear <- merge(hardestClimb,users, by="user_id")
startPlot <- ggplot(hardestWithYear, aes(x=started,y=grade_id)) + geom_point() +  xlim(1950, 2019) + xlab("Year Started Climbing") + ylab("Highest Grade Climbed")
startPlot


## ----as1----------------------------------------------------------------------
usersPerCountry <- count(users, country)
usersPerCountry <- usersPerCountry[order(usersPerCountry$n, decreasing = TRUE),]
usersPerCountry$country <- factor(usersPerCountry$country, levels = usersPerCountry$country)
usersPerCountry <- head(usersPerCountry, 10)
plotP <- ggplot(usersPerCountry, aes(country, n)) + geom_col() + ylab("Number of Users") + xlab("Country")
plotP


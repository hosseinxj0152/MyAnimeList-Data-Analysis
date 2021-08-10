install.packages("plotly")
library(ggplot2)
library(plotly)
library(data.table)
library(RColorBrewer)


# Here we read the data using fread from the data.table package


anime_data <- fread("dataanime.csv")
str(anime_data)

# We change the categorical columns into factors

anime_data$Type <- as.factor(anime_data$Type)
anime_data$Status <- as.factor(anime_data$Status)
anime_data$`Starting season` <-
	as.factor(anime_data$`Starting season`)
anime_data$Type <- as.factor(anime_data$Type)

# We convert the date columns into date format

anime_data$`Start airing` <-
	as.Date(anime_data$`Start airing`, "%Y-%m-%d")
anime_data$`End airing` <-
	as.Date(anime_data$`End airing`, "%Y-%m-%d")

# We want to see what percentage of our data is missing(NA)

NAs_in_data <- apply(anime_data, MARGIN = 2,
		     function(x) {
		     	round(sum(x == "-") / NROW(x) * 100, 2)
		     })
x <- data.frame(
	keyName = names(NAs_in_data),
	value = NAs_in_data,
	row.names = NULL
)
x <- x[x$value > 0, ]
g <- ggplot(x, aes(reorder(keyName, -value), value)) +
	geom_bar(stat = 'identity', aes(fill = value)) +
	theme(
		axis.text.x = element_text(
			angle = 30,
			vjust = 0.5,
			hjust = 1
		),
		plot.title = element_text(hjust = 0.5)
	) +
	xlab(label = "Columns") +
	ylab(label = "Percentage of missing values") +
	ggtitle(label = "Percentage of missing values by column") +
	ylim(c(0, 100))


ggplotly(g)


## Which type of anime do we have more of?

prop_table <- as.matrix(table(anime_data$Type))
myFrame <- as.data.frame((prop_table))
myFrame$Type <- rownames(myFrame)
rownames(myFrame) <- c()
myFrame <- myFrame[order(-myFrame$V1), ]

g <- ggplot(myFrame, aes(
	x = reorder(Type, -V1),
	y = V1,
	fill = Type
)) +
	geom_bar(stat = "identity",
		 width = 1,
		 color = "white") +
	ylab(label = "Frequency") +
	xlab(label = "Type of media") +
	ggtitle(label = "Frequency of Anime based on type of media") +
	theme(plot.title = element_text(hjust = 0.5))


ggplotly(g)

# I wanted to have a pie chart here but was forced to use a barplot
# because there was a problem with coord_polar('y') for some reason
# apparently, it is currently an open issue in the ggplot2 package



## Which type of anime is more popular? series or movies?


anime_series_data <-
	anime_data[anime_data$Type == "TV" |
		   	anime_data$Type == "ONA" |
		   	anime_data$Type == "Special"][, -c("Producers",
		   					   "Licensors",
		   					   "Description")]

anime_movies_data <-
	anime_data[anime_data$Type == "Movie"][, -c(
		"Type",
		"Episodes",
		"Status",
		"End airing",
		"Starting season",
		"Broadcast time",
		"Producers",
		"Licensors",
		"Duration",
		"Description"
	)]

mean_score_type <- round(c(
	mean(anime_series_data$Score),
	mean(anime_movies_data$Score)
), 2)

mean_score_by_type <- as.data.frame(cbind(Type = c("Series",
						   "Movie"),
					  mean_score_type))

g <-
	ggplot(mean_score_by_type, aes(x = Type, y = mean_score_type)) +
	geom_bar(stat = "identity", fill = c("magenta", "blue")) +
	xlab(label = "Type of Anime") +
	ylab(label = "Mean Score") +
	ggtitle(label = "MyAnimeList Score based on anime type") +
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)


## Which status of anime series is more popular?


temp <- as.data.frame(table(anime_series_data$Status))
mean_score_status <- round(c(
	mean(anime_series_data[Status == "Currently Airing"]$Score),
	mean(anime_series_data[Status == "Finished Airing"]$Score)
), 2)

mean_score_by_status <-
	as.data.frame(cbind(
		Status = c("Currently Airing",
			   "Finished Airing"),
		mean_score_status,
		Number = temp[, 2]
	))

g <- ggplot(mean_score_by_status, aes(x = Status,
				      y = mean_score_status)) +
	geom_bar(stat = "identity",
		 fill = c("magenta", "blue")) +
	xlab(label = "Status of Anime") +
	ylab(label = "Mean Score") +
	ggtitle(label = "MyAnimeList Score based on anime series status") +
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)


## Which years were the best for starting an anime series?


temp <- as.data.frame(table(year(anime_series_data$`Start airing`)))
mean_score_start <-
	anime_series_data[, mean(Score), by = year(`Start airing`)]
mean_score_start <- round(mean_score_start, 2)
mean_score_start <- mean_score_start[order(year), ]
mean_score_start <- cbind(mean_score_start, temp[, 2])
names(mean_score_start) <-
	c("Year", "Mean_Score", "Number_of_series")

g <- ggplot(mean_score_start, aes(x = Year, y = Mean_Score)) +
	geom_line(stat = "identity", color = "#879cec") +
	geom_point(stat = "identity",
		   aes(size = Number_of_series),
		   alpha = .6) +
	geom_smooth(method = "lm", se = FALSE, color = "brown") +
	xlab(label = "Starting year of Anime") +
	ylab(label = "Mean Score") +
	ggtitle("MyAnimeList Score based on anime series starting year") +
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)

## Which years were the best for ending an anime series?


temp <- as.data.frame(table(year(anime_series_data$`End airing`)))
mean_score_end <-
	anime_series_data[, mean(Score), by = year(`End airing`)]
mean_score_end <- round(mean_score_end, 2)
mean_score_end <- mean_score_end[order(year), ]
mean_score_end <- cbind(mean_score_end, temp[, 2])
names(mean_score_end) <-
	c("Year", "Mean_Score", "Number_of_series")

g <- ggplot(mean_score_end, aes(x = Year, y = Mean_Score)) +
	geom_line(stat = "identity", color = "#879cec") +
	geom_point(stat = "identity",
		   aes(size = Number_of_series),
		   alpha = .6) +
	geom_smooth(method = "lm", se = FALSE, color = "brown") +
	xlab(label = "Ending year of Anime") +
	ylab(label = "Mean Score") +
	ggtitle("MyAnimeList Score based on anime series ending year") +
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)


## Which year was the best for releasing an anime movie?


temp <- as.data.frame(table(year(anime_movies_data$`Start airing`)))

mean_score_start <-
	anime_movies_data[, mean(Score), by = year(`Start airing`)]
mean_score_start <- round(mean_score_start, 2)
mean_score_start <- mean_score_start[order(year), ]
mean_score_start <- cbind(mean_score_start, temp[, 2])
names(mean_score_start) <-
	c("Year", "Mean_Score", "Number_of_movies")

g <- ggplot(mean_score_start, aes(x = Year, y = Mean_Score)) +
	geom_line(stat = "identity", color = "#879cec") +
	geom_point(stat = "identity",
		   aes(size = Number_of_movies),
		   alpha = 0.6) +
	geom_smooth(method = "lm", se = FALSE) +
	xlab(label = "Release year of Anime") +
	ylab(label = "Mean Score") +
	ggtitle(label = "MyAnimeList Score based on anime movies release year") +
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)


## Which season is most filled with anime?


temp <- as.data.frame(table(anime_series_data$`Starting season`))

prop_table <- as.matrix(table(anime_series_data$`Starting season`))
myFrame <- as.data.frame((prop_table))
myFrame$`Starting season` <- rownames(myFrame)
rownames(myFrame) <- c()
myFrame <- myFrame[order(-myFrame$V1), ]
myFrame <- myFrame[myFrame$`Starting season` != '-', ]



g <- ggplot(myFrame, aes(
	x = reorder(`Starting season`, -V1),
	y = V1
	,
	fill = `Starting season`
)) +
	geom_bar(stat = "identity",
		 width = 1,
		 color = "white") +
	ylab(label = "Frequency") +
	xlab(label = "Starting Season") +
	ggtitle(label = "Frequency of Anime based on starting season") +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "none")



ggplotly(g)


## Which season is best for starting an anime series?


temp <- as.data.frame(table((anime_series_data$`Starting season`)))

mean_score_start <-
	anime_series_data[, mean(Score), by = `Starting season`]
mean_score_start$V1 <- round(mean_score_start$V1, 2)
mean_score_start <- mean_score_start[order(`Starting season`), ]
mean_score_start <- cbind(mean_score_start, temp[, 2])
names(mean_score_start) <-
	c("Starting_Season", "Mean_Score", "Number_of_series")
mean_score_start <- mean_score_start[-(Starting_Season == '-'), , ]


g <- ggplot(mean_score_start,
	    aes(x = Starting_Season,
	        y = Mean_Score,
	        fill = Starting_Season)) +
	geom_bar(stat = "identity", color = "white") +
	xlab(label = "Starting season of Anime") +
	ylab(label = "Mean Score") +
	ggtitle("MyAnimeList Score based on anime series starting season") +
	theme(plot.title = element_text(hjust = 0.5)) +
	coord_cartesian(ylim = c(7.5, 8)) +
	theme(legend.position = "none")


ggplotly(g)


## Which day of the week is the most anime heavy?

anime_series_data$weekday <- anime_series_data$`Broadcast time`
anime_series_data <-
	anime_series_data[grepl(' at ', `Broadcast time`), ]

anime_series_data$weekday <-
	sapply(anime_series_data$`Broadcast time`,
	       function(x) {
	       	gsub('.{1}$', '',
	       	     strsplit(x, ' at ')[[1]][1])
	       })

temp <- as.data.frame(table(anime_series_data$weekday))

prop_table <- as.matrix(table(anime_series_data$weekday))
myFrame <- as.data.frame((prop_table))
myFrame$weekday <- rownames(myFrame)
rownames(myFrame) <- c()
myFrame <- myFrame[order(-myFrame$V1), ]
myFrame <- myFrame[myFrame$weekday != '-', ]



g <- ggplot(myFrame, aes(
	x = reorder(weekday, -V1),
	y = V1
	,
	fill = weekday
)) +
	geom_bar(stat = "identity",
		 width = 1,
		 color = "white") +
	ylab(label = "Frequency") +
	xlab(label = "Broadcast day") +
	ggtitle(label = "Frequency of Anime based on Broadcast day") +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "none")



ggplotly(g)







## Which day of the week is better for anime broadcast time?

anime_series_data$weekday <- anime_series_data$`Broadcast time`
anime_series_data <-
	anime_series_data[grepl(' at ', `Broadcast time`), ]

anime_series_data$weekday <-
	sapply(anime_series_data$`Broadcast time`,
	       function(x) {
	       	gsub('.{1}$', '',
	       	     strsplit(x, ' at ')[[1]][1])
	       })

temp <- as.data.frame(table((anime_series_data$weekday)))

mean_score_day <-
	anime_series_data[, mean(Score), by = weekday]
mean_score_day$V1 <- round(mean_score_day$V1, 2)
mean_score_day <- mean_score_day[order(weekday), ]
mean_score_day <- cbind(mean_score_day, temp[, 2])
names(mean_score_day) <-
	c("Weekday", "Mean_Score", "Number_of_series")


g <- ggplot(mean_score_day,
	    aes(x = Weekday,
	        y = Mean_Score,
	        fill = Weekday)) +
	geom_bar(stat = "identity", color = "white") +
	xlab(label = "Broadcast day of Anime") +
	ylab(label = "Mean Score") +
	ggtitle("MyAnimeList Score based on anime Broadcast day") +
	theme(plot.title = element_text(hjust = 0.5)) +
	coord_cartesian(ylim = c(7.8, 8.2)) +
	theme(legend.position = "none")


ggplotly(g)




## Which time slot has the most amount of anime?

anime_series_data$day_time <-
	sapply(anime_series_data$`Broadcast time`, function(x) {
		gsub('.{6}$', '', strsplit(x, ' at ')[[1]][2])
	})
anime_series_data <- anime_series_data[day_time != 'U',]

temp <- as.data.frame(table(anime_series_data$day_time))

prop_table <- as.matrix(table(anime_series_data$day_time))
myFrame <- as.data.frame((prop_table))
myFrame$day_time <- rownames(myFrame)
rownames(myFrame) <- c()
myFrame <- myFrame[order(-myFrame$V1), ]
myFrame <-
	myFrame[myFrame$day_time != '-', ][myFrame$V1 > mean(myFrame$V1),]



g <- ggplot(myFrame, aes(
	x = reorder(day_time, -V1),
	y = V1
	,
	fill = day_time
)) +
	geom_bar(stat = "identity",
		 width = 1,
		 color = "white") +
	ylab(label = "Frequency") +
	xlab(label = "Time Slot") +
	ggtitle(label = "Frequency of Anime based on Time Slot") +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "none",
	      axis.text = element_text(
	      	angle = 30,
	      	vjust = .5,
	      	hjust = 1
	      ))


ggplotly(g)




## Which time slot is the best for anime?

anime_series_data$day_time <-
	sapply(anime_series_data$`Broadcast time`, function(x) {
		gsub('.{6}$', '', strsplit(x, ' at ')[[1]][2])
	})
anime_series_data <- anime_series_data[day_time != 'U',]

temp <- as.data.frame(table((anime_series_data$day_time)))

mean_score_ts <-
	anime_series_data[, mean(Score), by = day_time]
mean_score_ts$V1 <- round(mean_score_ts$V1, 2)
mean_score_ts <- mean_score_ts[order(day_time), ]
mean_score_ts <- cbind(mean_score_ts, temp[, 2])
names(mean_score_ts) <-
	c("Time_Slot", "Mean_Score", "Number_of_series")
mean_score_ts <-
	mean_score_ts[mean_score_ts$Mean_Score > mean(mean_score_ts$Mean_Score),]

g <- ggplot(mean_score_ts,
	    aes(
	    	x = reorder(Time_Slot, -Mean_Score),
	    	y = Mean_Score,
	    	fill = Time_Slot
	    )) +
	geom_bar(stat = "identity", color = "white") +
	xlab(label = "Time Slot of Anime") +
	ylab(label = "Mean Score") +
	ggtitle("MyAnimeList Score based on anime Time Slot") +
	theme(plot.title = element_text(hjust = 0.5)) +
	coord_cartesian(ylim = c(7.8, 9.2)) +
	theme(legend.position = "none",
	      axis.text = element_text(
	      	angle = 90,
	      	vjust = .5,
	      	hjust = 1
	      ))

ggplotly(g)


## What percentage of anime come from a manga?

temp <- as.data.frame(table(anime_series_data$Sources))

prop_table <- as.matrix(table(anime_series_data$Sources))
myFrame <- as.data.frame((prop_table))
myFrame$Sources <- rownames(myFrame)
rownames(myFrame) <- c()
myFrame <- myFrame[order(-myFrame$V1), ]


g <- ggplot(myFrame, aes(
	x = reorder(Sources, -V1),
	y = V1
	,
	fill = Sources
)) +
	geom_bar(stat = "identity",
		 width = 1,
		 color = "white") +
	ylab(label = "Frequency") +
	xlab(label = "Source") +
	ggtitle(label = "Frequency of Anime based on its' source") +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(legend.position = "none",
	      axis.text = element_text(
	      	angle = 30,
	      	vjust = .5,
	      	hjust = 1
	      ))


ggplotly(g)


## How does the source affect an the popularity of an anime?

temp <- as.data.frame(table((anime_series_data$Sources)))

mean_score_s <-
	anime_series_data[, mean(Score), by = Sources]
mean_score_s$V1 <- round(mean_score_s$V1, 2)
mean_score_s <- mean_score_s[order(Sources), ]
mean_score_s <- cbind(mean_score_s, temp[, 2])
names(mean_score_s) <-
	c("Source", "Mean_Score", "Number_of_series")

g <- ggplot(mean_score_s,
	    aes(
	    	x = reorder(Source, -Mean_Score),
	    	y = Mean_Score,
	    	fill = Source
	    )) +
	geom_bar(stat = "identity", color = "white") +
	xlab(label = "Source of Anime") +
	ylab(label = "Mean Score") +
	ggtitle("MyAnimeList Score based on anime Source") +
	theme(plot.title = element_text(hjust = 0.5)) +
	coord_cartesian(ylim = c(7.8, 8.3)) +
	theme(legend.position = "none",
	      axis.text = element_text(
	      	angle = 90,
	      	vjust = .5,
	      	hjust = 1
	      ))

ggplotly(g)






## How does the rating effect popularity?

rating_means <-
	aggregate(anime_series_data$Score ~ anime_series_data$Rating, FUN = mean)
rating_means <- data.frame(as.matrix(rating_means))
rating_means$anime_series_data.Score <-
	as.numeric(rating_means$anime_series_data.Score)
names(rating_means) <- c("Rating", "Score")
g <-
	ggplot(rating_means, aes(
		x = reorder(Rating, -Score),
		y = Score,
		fill = Rating
	)) +
	geom_bar(stat = 'identity', color = 'white') +
	xlab(label = 'Anime rating') +
	ylab(label = 'Mean Score') +
	ggtitle(label = 'MyAnimeList Score based on Rating') +
	coord_cartesian(ylim = c(7.5, 8.3)) +
	theme(legend.position = "none",
	      plot.title = element_text(hjust = .5))

ggplotly(g)

## What are the top anime of all time?

top_anime <- anime_series_data[order(-anime_series_data$Score),]
top_anime <- head(top_anime, 10)

g <-
	ggplot(top_anime, aes(x = reorder(Title, -Score), Score, fill = Title)) +
	geom_bar(stat = 'identity', color = 'white') +
	xlab(label = 'Title') +
	ylab(label = 'Score') +
	ggtitle(label = 'Top 10 anime on MyAnimeList') +
	coord_cartesian(ylim = c(9, 9.3)) +
	theme(
		axis.text.x = element_text(
			angle = 30,
			vjust = 0.5,
			hjust = 1
		),
		legend.position = 'none',
		plot.title = element_text(hjust = .5)
	)

ggplotly(g)


## What are the top 10 most popular of all time?

pop_anime <- anime_series_data[order(-anime_series_data$Members),]
pop_anime <- head(pop_anime, 10)

g <-
	ggplot(pop_anime, aes(x = reorder(Title, -Score), Score, fill = Title)) +
	geom_bar(stat = 'identity', color = 'white') +
	xlab(label = 'Title') +
	ylab(label = 'Score') +
	ggtitle(label = 'Top 10 most popular anime on MyAnimeList') +
	coord_cartesian(ylim = c(7.5, 9.3)) +
	theme(
		axis.text.x = element_text(
			angle = 30,
			vjust = 0.5,
			hjust = 1
		),
		legend.position = 'none',
		plot.title = element_text(hjust = .5)
	)

ggplotly(g)


## Which series have been on air the longest?

anime_longevity <- anime_series_data
anime_longevity[is.na(anime_longevity$`End airing`),]$`End airing` <- Sys.Date()
anime_longevity$On_air <- anime_longevity$`End airing`- anime_longevity$`Start airing`
anime_longevity$On_air <- as.numeric(gsub('.{5}$', '', anime_longevity$On_air))

old_anime <- anime_longevity
old_anime <- anime_longevity[order(-anime_longevity$On_air),]
old_anime <- head(old_anime, 10)

g <-
	ggplot(old_anime, aes(x = reorder(Title, -Score), Score, fill = Title)) +
	geom_bar(stat = 'identity', color = 'white') +
	xlab(label = 'Title') +
	ylab(label = 'Score') +
	ggtitle(label = 'Top 10 anime with the longest record of being on TV on MyAnimeList') +
	coord_cartesian(ylim = c(7.5, 9.3)) +
	theme(
		axis.text.x = element_text(
			angle = 30,
			vjust = 0.5,
			hjust = 1
		),
		legend.position = 'none',
		plot.title = element_text(hjust = .5)
	)

ggplotly(g)



## Which series have the most number of episodes?

ep_anime <- anime_series_data
ep_anime$Episodes <- as.numeric(ep_anime$Episodes)
ep_anime <- ep_anime[order(-ep_anime$Episodes),]
ep_anime <- head(ep_anime, 10)

g <-
	ggplot(ep_anime, aes(x = reorder(Title, -Episodes), Episodes, fill = Title)) +
	geom_bar(stat = 'identity', color = 'white') +
	xlab(label = 'Title') +
	ylab(label = 'Number of Episodes') +
	ggtitle(label = 'Top 10 longest-running finished anime on MyAnimeList') +
	theme(
		axis.text.x = element_text(
			angle = 30,
			vjust = 0.5,
			hjust = 1
		),
		legend.position = 'none',
		plot.title = element_text(hjust = .5)
	)

ggplotly(g)


## Which anime are most favorited?

fav_anime <- anime_series_data[order(-anime_series_data$Favorites),]
fav_anime <- head(fav_anime, 10)

g <-
	ggplot(fav_anime, aes(x = reorder(Title, -Favorites), Favorites, fill = Title)) +
	geom_bar(stat = 'identity', color = 'white') +
	xlab(label = 'Title') +
	ylab(label = 'Number of Favorites') +
	ggtitle(label = 'Top 10 most-favorited anime on MyAnimeList') +
	theme(
		axis.text.x = element_text(
			angle = 30,
			vjust = 0.5,
			hjust = 1
		),
		legend.position = 'none',
		plot.title = element_text(hjust = .5)
	)

ggplotly(g)




## Multivariate regression model for anime score
## Future projects

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
anime_data$`Starting season` <- as.factor(anime_data$`Starting season`)
anime_data$Type <- as.factor(anime_data$Type)

# We convert the date columns into date format

anime_data$`Start airing` <- as.Date(anime_data$`Start airing`, "%Y-%m-%d")
anime_data$`End airing` <- as.Date(anime_data$`End airing`, "%Y-%m-%d")

# We specify 2 main genres for each anime

anime_data$Genres <- strsplit(anime_data$Genres,',')
anime_data$Genre_1 <- anime_data$Genres
anime_data$Genre_1 <- lapply(anime_data$Genres, function(x){x[1]})
anime_data$Genre_2 <- anime_data$Genres
anime_data$Genre_2 <- lapply(anime_data$Genre_2, function(x){x[2]})
anime_data$Genre_3 <- anime_data$Genres
anime_data$Genre_3 <- lapply(anime_data$Genre_3, function(x){x[3]})
anime_data$Genre_4 <- anime_data$Genres
anime_data$Genre_4 <- lapply(anime_data$Genre_4, function(x){x[4]})
anime_data$Genre_5 <- anime_data$Genres
anime_data$Genre_5 <- lapply(anime_data$Genre_5, function(x){x[5]})
anime_data$Genre_6 <- anime_data$Genres
anime_data$Genre_6 <- lapply(anime_data$Genre_6, function(x){x[6]})
anime_data$Genre_7 <- anime_data$Genres
anime_data$Genre_7 <- lapply(anime_data$Genre_7, function(x){x[7]})
anime_data$Genre_8 <- anime_data$Genres
anime_data$Genre_8 <- lapply(anime_data$Genre_8, function(x){x[8]})
anime_data$Genre_9 <- anime_data$Genres
anime_data$Genre_9 <- lapply(anime_data$Genre_9, function(x){x[9]})
anime_data$Genre_10 <- anime_data$Genres
anime_data$Genre_10 <- lapply(anime_data$Genre_10, function(x){x[10]})
# We want to see what percentage of our data is missing(NA)

NAs_in_data <- apply(anime_data, MARGIN = 2,
		     function(x) {round(sum(x=="-")/NROW(x)*100,2)})
x <- data.frame(keyName=names(NAs_in_data),
		value=NAs_in_data,
		row.names=NULL)
g <- ggplot(x, aes(reorder(keyName,-value), value))+
	geom_bar(stat = 'identity',aes(fill=value))+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
	      plot.title = element_text(hjust = 0.5))+
	xlab(label = "Columns")+
	ylab(label = "Percentage of missing values")+
	ggtitle(label = "Percentage of missing values by column")


ggplotly(g)


## Which type of anime do we have more of?

prop_table <- as.matrix(table(anime_data$Type))
myFrame <- as.data.frame((prop_table))
myFrame$Type <- rownames(myFrame) 
rownames(myFrame) <- c()
myFrame <- myFrame[order(-myFrame$V1),]

g <- ggplot(myFrame, aes(x=reorder(Type,-V1),y=V1, fill=Type)) +
	geom_bar(stat="identity", width=1, color="white") +
	ylab(label = "Frequency")+
	xlab(label = "Type of media")+
	ggtitle(label = "Frequency of Anime based on type of media")+
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)

# I wanted to have a pie chart here but was forced to use a barplot
# because there was a problem with coord_polar('y') for some reason
# apparently, it is currently an open issue in the package itself



## Which type of anime is more popular? series or movies?


anime_series_data <- 
	anime_data[
		anime_data$Type=="TV" |
		anime_data$Type=="ONA" |
		anime_data$Type=="Special"][,-c("Producers",
						"Licensors",
						"Description")]

anime_movies_data <- anime_data[anime_data$Type=="Movie"][,-c("Type","Episodes",
							      "Status",
							      "End airing",
							      "Starting season",
							      "Broadcast time",
							      "Producers",
							      "Licensors",
							      "Duration",
							      "Description")]

table(anime_series_data$Type)
mean_score_type <- round(c(mean(anime_series_data$Score),
		      mean(anime_movies_data$Score)),2)

mean_score_by_type <- as.data.frame(cbind(Type = c("Series",
						   "Movie"),
					  mean_score_type))

g <- ggplot(mean_score_by_type, aes(x=Type, y=mean_score_type))+
	geom_bar(stat = "identity", fill=c("magenta","blue"))+
	xlab(label = "Type of Anime")+
	ylab(label = "Mean Score")+
	ggtitle(label = "MyAnimeList Score based on anime type")+
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)


## What status of anime series is more popular?


temp <- as.data.frame(table(anime_series_data$Status))
mean_score_status <- round(c(
	mean(anime_series_data[Status == "Currently Airing"]$Score),
	mean(anime_series_data[Status == "Finished Airing"]$Score)),2)

mean_score_by_status <- as.data.frame(cbind(Status = c("Currently Airing",
						       "Finished Airing"),
					  mean_score_status,
					  Number=temp[,2]))

g <- ggplot(mean_score_by_status, aes(x=Status,
				      y=mean_score_status))+
	geom_bar(stat = "identity", fill=c("magenta","blue"),aes(size=Number))+
	xlab(label = "Status of Anime")+
	ylab(label = "Mean Score")+
	ggtitle(label = "MyAnimeList Score based on anime series status")+
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)


## Which years were the best for starting an anime series?


temp <- as.data.frame(table(year(anime_series_data$`Start airing`)))
mean_score_start <- anime_series_data[,mean(Score),by=year(`Start airing`)]
mean_score_start <- round(mean_score_start,2)
mean_score_start <- mean_score_start[order(year),]
mean_score_start <- cbind(mean_score_start, temp[,2])
names(mean_score_start) <- c("Year", "Mean_Score","Number_of_series")

g <- ggplot(mean_score_start, aes(x=Year, y=Mean_Score))+
	geom_line(stat = "identity",color="#879cec")+
	geom_point(stat = "identity",aes(size=Number_of_series),alpha=.6)+
	geom_smooth(method = "lm", se = FALSE)+
	xlab(label = "Starting year of Anime")+
	ylab(label = "Mean Score")+
	ggtitle("MyAnimeList Score based on anime series starting year")+
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)


## Which year was the best for realeasing an anime movie?


temp <- as.data.frame(table(year(anime_movies_data$`Start airing`)))

mean_score_start <- anime_movies_data[,mean(Score),by=year(`Start airing`)]
mean_score_start <- round(mean_score_start,2)
mean_score_start <- mean_score_start[order(year),]
mean_score_start <- cbind(mean_score_start,temp[,2])
names(mean_score_start) <- c("Year", "Mean_Score","Number_of_movies")

g <- ggplot(mean_score_start, aes(x=Year,y=Mean_Score))+
	geom_line(stat = "identity",color="#879cec")+
	geom_point(stat = "identity",aes(size=Number_of_movies),alpha=0.6)+
	geom_smooth(method = "lm", se = FALSE)+
	xlab(label = "Release year of Anime")+
	ylab(label = "Mean Score")+
	ggtitle(label = "MyAnimeList Score based on anime movies release year")+
	theme(plot.title = element_text(hjust = 0.5))

ggplotly(g)


## Which season is most filled with anime?


temp <- as.data.frame(table(anime_series_data$`Starting season`))

prop_table <- as.matrix(table(anime_series_data$`Starting season`))
myFrame <- as.data.frame((prop_table))
myFrame$`Starting season` <- rownames(myFrame) 
rownames(myFrame) <- c()
myFrame <- myFrame[order(-myFrame$V1),]
myFrame <- myFrame[myFrame$`Starting season`!='-',]


mean_score_start <- mean_score_start[-(Starting_Season=='-'),,]


g <- ggplot(myFrame, aes(x=reorder(`Starting season`,-V1),y=V1
			 , fill=`Starting season`)) +
	geom_bar(stat="identity", width=1, color="white") +
	ylab(label = "Frequency")+
	xlab(label = "Starting Season")+
	ggtitle(label = "Frequency of Anime based on starting season")+
	theme(plot.title = element_text(hjust = 0.5))


ggplotly(g)


## Which season is best for starting an anime series?


temp <- as.data.frame(table((anime_series_data$`Starting season`)))

mean_score_start <- anime_series_data[,mean(Score),by=`Starting season`]
mean_score_start$V1 <- round(mean_score_start$V1,2)
mean_score_start <- mean_score_start[order(`Starting season`),]
mean_score_start <- cbind(mean_score_start,temp[,2])
names(mean_score_start) <- c("Starting_Season", "Mean_Score","Number_of_series")
mean_score_start <- mean_score_start[-(Starting_Season=='-'),,]


g <- ggplot(mean_score_start, aes(x=Starting_Season,
				  y=Mean_Score,
				  fill=Starting_Season))+
	geom_bar(stat="identity", color="white") +
	xlab(label = "Starting season of Anime")+
	ylab(label = "Mean Score")+
	ggtitle("MyAnimeList Score based on anime series starting season")+
	theme(plot.title = element_text(hjust = 0.5))+
	coord_cartesian(ylim = c(7.5,8))

ggplotly(g)


## Which day of the week is the most anime heavy?

View(table(anime_series_data$`Broadcast time`))
anime_series_data$weekday <- anime_series_data$`Broadcast time`
anime_series_data <- anime_series_data[grepl(' at ',`Broadcast time`),]

anime_series_data$weekday <- sapply(anime_series_data$`Broadcast time`,
				    function(x){strsplit(x,' at ')[[1]][1]})
anime_series_data$day_time <- sapply(anime_series_data$`Broadcast time`, function(x){strsplit(x,' at ')[[1]][2]})






strsplit(anime_series_data$weekday[1],'at')[[1]][2]




## Which weekday is better for anime broadcast time?
## Which day of the week is better for anime broadcast time?
## Which studios have the best products?
## What percentage of anime come from a manga?
## How does the source affect an anime's popularity?
## Which genres are the most popular?
## What is the ideal duration for an anime episode?
## How does the rating effect popularity?
## What are the top anime of all time?
## Which studios are on the rise in the recent years?
## Which series are the longest-running?
## What is the relationship between genre and rating?
## Which anime are most favorited?
## future projects


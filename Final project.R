#---------------------------------------------------------------------------------#
# Shyamala Venkatakrishnan                                             11/10/2022 #
#                                                                                 #
#                 ALY6010: Final project - Milestone 1                            #
#---------------------------------------------------------------------------------#


install.packages(c("FSA","FSAdata","dplyr","plotrix","ggplot2","moments","stringr"
                   ,"data.table","sqldf","tidyverse","wordcloud","RColorBrewer", 
                   "devtools","plotly","gmodels","formattable","tidyr","rvest",
                   "magrittr","ggmap"))

loadlibrary <- c("FSA","FSAdata","dplyr","plotrix","ggplot2","moments",
                 "stringr","data.table","sqldf","tidyverse","wordcloud","RColorBrewer", 
                 "devtools","plotly","plotme","gmodels","formattable","tidyr",
                 "rvest","magrittr","ggmap")

lapply(loadlibrary,require,character.only=TRUE)

setwd("~/Downloads")

spotify_df <- read.csv("Spotify_tracks.csv",header=TRUE,stringsAsFactors = FALSE,
                       na.string = "")

#No. of rows and columns of a dataset
dim(spotify_df)

#Summary of the dataset
summary(spotify_df)

#First few rows from the dataset
head(spotify_df)

#Size of the dataset
file.info("Spotify_tracks.csv")$size

#Structure of the dataset
str(spotify_df)

install.packages('DataExplorer') 
library(DataExplorer)

#dropping unwanted columns - id, id_artists

spotify_df = select(spotify_df, -id, -id_artists)

#Correcting the format for the "releae_date" column.
library(lubridate)

spotify_df <- spotify_df %>% 
  mutate(release_date = as.character(release_date),
         release_month= if_else(grepl('-', release_date),
                          month(as.Date(release_date, origin = '1899-12-30')),
                          0),
         release_date = if_else(grepl('-', release_date),
                        as.Date(release_date, origin = '1899-12-30'),
                        as.Date(release_date, format = "%Y")),
         release_year = year(release_date))

remove_bracket_artists <- unlist(lapply(spotify_df$artists, function(x) 
  gsub("\\[|\\]", "", x)))

spotify_df$artists <- remove_bracket_artists

remove_quotes_artists <- unlist(lapply(spotify_df$artists, function(x) 
  gsub("\\'|\\'", "", x)))

spotify_df$artists <- remove_quotes_artists


#Num of Records with null/missing values:

na_count <-sapply(spotify_df, function(y) sum(length(which(is.na(y)))))
print(na_count)

plot_missing(spotify_df)

#Removing the records with missing values.

clean_spotify_df <- na.omit(spotify_df)

#Scaling the data - converting duration_ms to duration_mins:

clean_spotify_df$duration_ms <- round(clean_spotify_df$duration_ms/60000,2)
setnames(clean_spotify_df, "duration_ms", "duration_min")

boxplot(clean_spotify_df$release_year, 
        main="Box plot of release_year column")

#Let us do the analysis over the songs released after the year 1930

#hence deleting the records with release year less than 1930

spotify_dataset <- subset(clean_spotify_df, 
                           subset=(clean_spotify_df$release_year>=1930))

boxplot(spotify_dataset$release_year)

#Mapping key to its corresponding tone as per the pitch class notation:

spotify_dataset = spotify_dataset %>%
  mutate(
    key_class = case_when(
      key == 0 ~ "C",
      key == 1 ~ "C#",
      key == 2 ~ "D",
      key == 3 ~ "D#",
      key == 4 ~ "E",
      key == 5 ~ "F",
      key == 6 ~ "F#",
      key == 7 ~ "G",
      key == 8 ~ "G#",
      key == 9 ~ "A",
      key == 10 ~ "A#",
      key == 11 ~ "B"
    )
  )

spotify_dataset = spotify_dataset %>%
  mutate(
    Decade = case_when(
      startsWith(as.character(release_year),"193") ~ "1930s",
      startsWith(as.character(release_year),"194") ~ "1940s",
      startsWith(as.character(release_year),"195") ~ "1950s",
      startsWith(as.character(release_year),"196") ~ "1960s",
      startsWith(as.character(release_year),"197") ~ "1970s",
      startsWith(as.character(release_year),"198") ~ "1980s",
      startsWith(as.character(release_year),"199") ~ "1990s",
      startsWith(as.character(release_year),"200") ~ "2000s",
      startsWith(as.character(release_year),"201") ~ "2010s",
      startsWith(as.character(release_year),"202") ~ "2020s",
    )
  )

spotify_dataset = spotify_dataset %>%
  mutate(
    mode_class = case_when(
      mode == 1 ~ "Major",
      mode == 0 ~ "Minor"
    )
  )

#Descriptive summary statistics:

install.packages("psych")
library(psych) 

formattable(describe(spotify_dataset), 
            caption = "Descriptive statistics summary of the Spotify dataset")
describe(spotify_dataset)


#Computing the correlation matrix:

plot_correlation(spotify_dataset, type = 'continuous')

#EDA:

install.packages("GGally")
library(GGally)

corr_df <- select(spotify_dataset, -release_year,
                  -release_month,-time_signature,-tempo,-key,-mode,
                  -explicit,-duration_min,-speechiness)

ggcorr(corr_df, method = c("everything", "pearson")) 

corr_df <- sqldf("select * from corr_df where release_year >= 2010")

ggpairs(corr_df, title="correlogram with ggpairs()") 

#1. Popularity vs energy, danceability,loudness,acousticness - 
#heat maps with density
nrow(eliminated)

pop_energy = ggplot(spotify_dataset,aes(x=energy,y=popularity)) +
  ggtitle("Popularity vs Energy") +
  xlab("Energy") +
  ylab("Popularity")

pop_danceability = ggplot(spotify_dataset,aes(x=danceability,y=popularity)) +
  ggtitle("Popularity vs Danceability") +
  xlab("Danceability") +
  ylab("Popularity")

pop_loudness = ggplot(spotify_dataset,aes(x=loudness,y=popularity)) +
  ggtitle("Popularity vs Loudness") +
  xlab("Loudness") +
  ylab("Popularity")

pop_acoustic = ggplot(spotify_dataset,aes(x=acousticness,y=popularity)) +
  ggtitle("Popularity vs Acousticness") +
  xlab("Acousticness") +
  ylab("Popularity")

pop_instrument = ggplot(spotify_dataset,aes(x=instrumentalness,y=popularity)) +
  ggtitle("Popularity vs Instrumentalness") +
  xlab("Instrumentalness") +
  ylab("Popularity")

p1 = pop_energy + 
  geom_point(alpha = 0.01, colour="orange") + 
  geom_density2d() + 
  theme_bw()

p2 = pop_danceability + 
  geom_point(alpha = 0.01, colour="orange") + 
  geom_density2d() + 
  theme_bw()


p3 = pop_loudness + 
  geom_point(alpha = 0.01, colour="orange") + 
  geom_density2d() + 
  theme_bw()

p4 = pop_acoustic + 
  geom_point(alpha = 0.01, colour="orange") + 
  geom_density2d() + 
  theme_bw()


p5 = pop_instrument + 
  geom_point(alpha = 0.01, colour="orange") + 
  geom_density2d() + 
  theme_bw()

grid.arrange(p1,p2,p3,p4,p5,ncol=2,nrow=3)

#2. Energy-loudness - heat maps

energy_loudness = ggplot(spotify_dataset,aes(x=energy,y=loudness)) +
  ggtitle("Energy vs Loudness") +
  xlab("Energy") +
  ylab("Loudness")

p6 = energy_loudness + 
  geom_point(alpha = 0.01, colour="pink") + 
  geom_density2d() + 
  theme_bw()

#3. No of songs in Major mode vs minor mode - decade wise - grouped bar plot

mode_decade_no_of_songs <- sqldf("select mode_class, Decade,count(name) as count
                                from spotify_dataset
                        group by Decade,mode_class")
library(lattice)
library(viridis)

ggplot(mode_decade_no_of_songs,aes(x = Decade, y = count, fill = mode_class)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_viridis(discrete = T, option='E')+
  theme_bw()+
  ggtitle("No of songs in major and minor mode over the decades")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))+
  xlab("Decade")+ylab("No. of songs released")

#4. Percentage of songs in each key over the decades - stacked bar plot

key_decade_count <- sqldf("select Decade,key_class,count(name) as count
                          from spotify_dataset
                          group by Decade,key_class")

overall_key_count <- sqldf("select key_class,count(name) as count
                          from spotify_dataset
                          group by key_class")

overall_key_count <- overall_key_count %>%
  mutate(Decade = 'Overall')

combined <- rbind(key_decade_count,overall_key_count)

combined %>%
  group_by(Decade) %>%
  mutate(pct= prop.table(count) * 100) %>%
  ggplot() + aes(Decade, pct, fill=key_class) +
  geom_bar(stat="identity") +
  ylab("Percentage of songs") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Most preferred choice of key by the musicians - C and G") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))+
  xlab("Decade")+ylab("Percentage of songs")

#5. Top 5 popular songs in each decade,C major,G major. - tree map.

top_5_popular_songs_c <- sqldf("select * from
                             (select name,artists,release_year,Decade,
                             popularity, mode_class,key_class,
                             row_number() over (partition by 
                             Decade order by popularity desc) as n from
                             spotify_dataset where key_class == 'C' and 
                             mode_class = 'Major'
                              order by Decade desc) as x
                               where n <= 5 ") 

top_5_popular_songs <- sqldf("select * from
                             (select name,artists,release_year,Decade,
                             popularity,  mode_class,key_class,
                             row_number() over (partition by 
                             Decade,key_class order by popularity desc) as n from
                             spotify_dataset where key_class in ('C','G') and 
                             mode_class = 'Major'
                              order by Decade desc) as x
                               where n <= 5 ") 

install.packages("treemap")
library(treemap)

##Coloring the boxes by a measure##
treemap(top_5_popular_songs_c,
        index = c("Decade","mode_class","key_class","name"),
        vSize ="n",vColor = "popularity",type="value",
        title="Top 5 popular songs in each decade, in C major key")

##Coloring the boxes by a measure##
treemap(top_5_popular_songs_g,
        index = c("Decade","mode_class","key_class","name"),
        vSize ="n",vColor = "popularity",type="value",
        title="Top 5 popular songs in each decade, in G major key")

top_5_popular_songs %>% 
  count(Decade, mode_class, key_class, name,wt = popularity) %>% 
  count_to_treemap()

#8. Top 10 happy,cheerful songs 2000-2021:

df_20s <- sqldf("select * from spotify_dataset where release_year >= 2000")

happy_songs_top_10 <- sqldf("select name,artists,release_year,
                            valence, danceability,energy,loudness,
                            popularity from df_20s where
                            valence between 0.75 and 1 and
                            danceability between 0.75 and 1 and
                            energy between 0.75 and 1 and
                            loudness between -20 and 0
                            order by popularity desc limit 10")
happy_songs_top_10

formattable(happy_songs_top_10, caption = "Top 10 popular,happy,energetic,
            foot tapping songs 2000-2021")

#11. How is the duration of the songs changing in the years 1930-2021?

duration_year_df <- sqldf("select release_year, avg(duration_min) 
                          as Avg_duration_min from spotify_dataset
                        group by release_year order by Avg_duration_min desc")

ggplot(duration_year_df, aes(x=release_year, y=Avg_duration_min)) +
  geom_line( color="blue") + 
  geom_point() +
  theme_ipsum() +
  ylim(1,5)+
  scale_x_continuous(limits=c(1925, 2021))+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  xlab("Release year of songs") +ylab("Avg. duration of songs in mins")+
  ggtitle("How is the duration of the songs changing in the years 1930-2021?")


df_20s_more_popular <- sqldf("select * from df_20s
                             where popularity > 70")

df_20s_least_popular <- sqldf("select * from df_20s
                             where popularity > 0 and popularity <= 30")

df_20s_least_popular <- df_20s_least_popular[sample(nrow(df_20s_least_popular),
                                                   5500,replace = F),]

ggplot(data = df_20s_more_popular, mapping = aes(x = duration_min)) +
  geom_histogram(fill="orange")+
  xlab("Duration of songs in mins")+ylab("No. of songs")+
  xlim(0,5)+
  ggtitle("Histogram of duration of songs 2000-2021 with popularity of 70 to 100")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))+
  theme_bw()

ggplot(data = df_20s_least_popular, mapping = aes(x = duration_min)) +
  geom_histogram(fill="orange")+
  xlab("Duration of songs in mins")+ylab("No. of songs")+
  xlim(0,5)+
  ggtitle("Histogram of duration of songs 2000-2021 with popularity of 0 to 30")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))+
  theme_bw()

more_popular<- ggplot(data = df_20s_more_popular, mapping = aes(x = duration_min)) +
  geom_rug(color = 'hotpink') + 
  xlim(0,5)+
  xlab("Duration of songs in mins")+
  ggtitle("Average duration of songs with popularity of 70-100 in the years 2000-2021")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))+
  theme_bw()+
  stat_function(fun = ~dnorm(.x, 
                             mean = mean(df_20s_more_popular$duration_min,na.rm=TRUE),
                             sd = sd(df_20s_more_popular$duration_min,na.rm=TRUE))
                ,col = "hotpink")+
  geom_vline(xintercept = 3.49, linetype="dotted",
             color = "black", linewidth=1.5)+
  annotate("text", x=4, y=0.4, label= "Mean duration_mins = 3.49")

least_popular<- ggplot(data = df_20s_least_popular, mapping = aes(x = duration_min)) +
  geom_rug(color = 'hotpink') + 
  xlim(0,5)+
  xlab("Duration of songs in mins")+
  ggtitle("Average duration of songs with popularity of 0-30 in the years 2000-2021")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))+
  theme_bw()+
  stat_function(fun = ~dnorm(.x, 
                             mean = mean(df_20s_least_popular$duration_min,na.rm=TRUE),
                             sd = sd(df_20s_least_popular$duration_min,na.rm=TRUE))
                ,col = "hotpink")+
  geom_vline(xintercept = 3.9, linetype="dotted",
             color = "black", linewidth=1.5)+
  annotate("text", x=4.3, y=0.4, label= "Mean duration_mins = 3.90")

#How is the song valence distributed every year?

mood_songs_df <- sqldf("select release_year, avg(valence) as Avg_valence
                        from spotify_dataset
                        group by release_year")

ggplot()+ geom_boxplot(data = spotify_dataset, 
                       aes(x=Decade,y=valence, fill=Decade))+
  theme_bw()+
  ggtitle("Distribution of song valence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))+
  labs(subtitle = "0 = negative music, 1 = positive music")

#Music has become more electric and energetic over time

trend_en_da_ac <- sqldf("select release_year, avg(acousticness) as acousticness
,avg(energy) as avg_energy, avg(danceability) as avg_danceability
                       from spotify_dataset
                        group by release_year")

legend_colors <- c("Acousticness" = "blue", "Energy" = "#458B00", 
                   "Danceability" = "red")

ggplot()+
  geom_line(data = trend_en_da_ac,mapping = aes(x=release_year,
                                        y=acousticness,color="Acousticness"))+
  geom_line(data = trend_en_da_ac,mapping = aes(x=release_year,
                                        y=avg_energy,color="Energy"))+
  geom_line(data = trend_en_da_ac,mapping = aes(x=release_year,
                                  y=avg_danceability,color="Danceability"))+
  theme_ipsum() +
  scale_x_continuous(limits=c(1925, 2021))+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(color = "Song features") + 
  scale_color_manual(values = legend_colors) + 
  xlab("Release year of songs") +
  ggtitle("Music has become more electric and energetic over time")+
  geom_vline(xintercept = 1950, linetype="dotted",
             color = "orange", linewidth=1.5)+
  geom_vline(xintercept = 1980, linetype="dotted",
             color = "orange", linewidth=1.5)+
  annotate("text", x=1970, y=0.9, label= "Rise of Rock and Roll music")+
  annotate("text", x=2003, y=0.8,
           label= "Use of Drum machines,Synthesizers")

#Loudness of tracks

loudness <- sqldf("select release_year, avg(loudness) as loudness
                       from spotify_dataset
                        group by release_year")

ggplot()+
  geom_line(data = loudness,mapping = aes(x=release_year,
                                          y=loudness),color="#FF1493")+
  theme_ipsum() +
  ylim(-60,0)+
  scale_x_continuous(limits=c(1925, 2021))+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  xlab("Release year of songs") +
  ggtitle("Loudness of the songs over the years")

#Pattern of top 100 popular songs in the recent years 2000-2021

top_1000 <- sqldf("select * from spotify_dataset where release_year >= 2000
                  order by popularity desc limit 1000")

top_1000_pattern <- sqldf("select avg(valence),avg(danceability),avg(energy)
                ,avg(acousticness),avg(liveness),
                  avg(speechiness),avg(loudness),avg(tempo)
                       from top_1000")

install.packages("fmsb")
library(fmsb)

df <- data.frame(Valence=c(1, 0, 0.50),
                 Danceability=c(1, 0, 0.67),
                 Energy=c(1, 0, 0.63),
                 Acousticness=c(1, 0, 0.20),
                 Liveness=c(1, 0, 0.16),
                 Speechiness=c(1, 0, 0.10),
                 Loudness=c(0,-60,-6))
radarchart(df,
           axistype=1 , 
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,100,25), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 )

#Top 1000 popular songs - key %,mode % - donut, pie

key_table <- table(top_1000$key_class)

key_table_perc <- round((key_table / 1000) * 100, digits = 2)

key_table_df <- data.frame(key_table_perc)

colnames(key_table_df)[1] <- "key_class"
colnames(key_table_df)[2] <- "Percentage"

donut_key_top_1000 <- ggplot(data = key_table_df, aes(x=2,y=Percentage, 
                                                      fill = key_class))+
  geom_col(color = "black")+
  coord_polar("y", start=1)+
  
  geom_text(aes(label = paste(Percentage,"%", sep = "")), 
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  scale_fill_brewer(palette = "Paired")+
  xlim(.2,2.5)

mode_table <- table(top_1000$mode_class)

mode_table_perc <- round((mode_table / 1000) * 100, digits = 2)

mode_table_df <- data.frame(mode_table_perc)

colnames(mode_table_df)[1] <- "Mode_class"
colnames(mode_table_df)[2] <- "Percentage"

ggplot(mode_table_df, aes(x = "", y = Percentage, fill = Mode_class)) +
  geom_col() + geom_text(aes(label = paste(Percentage,"%", sep = ""))
                         ,position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Category")) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")+
  ggtitle("Percentage of mode class in top 1000 popular songs 2000 - 2021")


install.packages("caret",dep = TRUE)

library(caret)


install.packages("scales")
library("scales")

#Removing outliers from these variables:
#popularity,loudness, danceability,speechiness,liveness

#Popularity:
Q <- quantile(spotify_dataset$popularity, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(spotify_dataset$popularity)

up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr

eliminated<- subset(spotify_dataset, spotify_dataset$popularity > (Q[1] - 1.5*iqr) 
                    & spotify_dataset$popularity < (Q[2]+1.5*iqr))

#loudness:
Q <- quantile(eliminated$loudness, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(eliminated$loudness)

up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr

eliminated<- subset(eliminated, eliminated$loudness > (Q[1] - 1.5*iqr) 
                    & eliminated$loudness < (Q[2]+1.5*iqr))


#danceability:
Q <- quantile(eliminated$danceability, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(eliminated$danceability)

up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr

eliminated<- subset(eliminated, eliminated$danceability > (Q[1] - 1.5*iqr) 
                    & eliminated$danceability < (Q[2]+1.5*iqr))

#speechiness:
Q <- quantile(eliminated$speechiness, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(eliminated$speechiness)

up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr

eliminated<- subset(eliminated, eliminated$speechiness > (Q[1] - 1.5*iqr) 
                    & eliminated$speechiness < (Q[2]+1.5*iqr))

#liveness:
Q <- quantile(eliminated$liveness, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(eliminated$liveness)

up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr

eliminated<- subset(eliminated, eliminated$liveness > (Q[1] - 1.5*iqr) 
                    & eliminated$liveness < (Q[2]+1.5*iqr))

#Duration_mins
Q <- quantile(eliminated$duration_min, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(eliminated$duration_min)

up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr

eliminated<- subset(eliminated, eliminated$duration_min > (Q[1] - 1.5*iqr) 
                    & eliminated$duration_min < (Q[2]+1.5*iqr))

#Linear regression:
#Model 1: Predicting song popularity

devtools::install_github("kassambara/ggpubr")
library("ggpubr")

popularity <- ggdensity(eliminated$popularity,
                        xlab = "popularity")

popularity_cutoff <- 75

spotify_sample_1 <- 
  spotify_dataset[spotify_dataset$popularity >= popularity_cutoff,]

head(spotify_sample_1)

nrow(spotify_sample_1)

spotify_sample_2 <- 
  spotify_dataset[spotify_dataset$popularity < popularity_cutoff,]

nrow(spotify_sample_2_random)

spotify_sample_2_random <- spotify_sample_2[sample(nrow(spotify_sample_2),
                                                   3000,replace = F),]

spotify_sample_combined = 
  rbind(spotify_sample_1,spotify_sample_2_random)

nrow(spotify_sample_combined)

head(spotify_sample_combined)

set.seed(1)
training.samples <- createDataPartition(spotify_sample_combined$popularity,
                                        p = 0.8, list = FALSE)

head(training.samples,10)

train.data  <- spotify_sample_combined[training.samples, ]

head(train.data,10)
test.data <- spotify_sample_combined[-training.samples, ]

head(test.data, 10)

train.data$loudness <- rescale(train.data$loudness,0,1)
train.data$key <- rescale(train.data$key,0,1)

test.data$loudness <- rescale(test.data$loudness,0,1)
test.data$key <- rescale(test.data$key,0,1)

model_2 <- lm(popularity ~ energy + danceability + loudness + valence
              + acousticness + explicit + liveness + 
                speechiness + instrumentalness, 
            data = train.data)

# Summarize the model
summary(model_2)

plot(model_2$residuals, pch = 16, col = "red")

library(car)
avPlots(model_2)

# Make predictions
predictions <- model_2 %>% predict(test.data)


# Model performance
# (a) Prediction error, RMSE
RMSE(predictions, test.data$popularity)

# (b) R-square
R2(predictions, test.data$popularity)


newdata <- data.frame(
  energy = 0.8, danceability = 0.75
)

model_2 %>% predict(newdata)

#Model 2 :  Predicting song valence:
install.packages("leaps")
library(leaps)

spotify_numeric_df <- select(spotify_dataset,popularity, explicit,danceability,
                             energy,loudness,speechiness,acousticness,instrumentalness,
                             liveness,valence,key,mode)

model_1 <- regsubsets(valence~., data = spotify_numeric_df, 
                     nvmax = 5)
summary(model_1)

model1 <- lm(valence ~ danceability, data = spotify_numeric_df)
model2 <- lm(valence ~ danceability+energy, data = spotify_numeric_df)
model3 <- lm(valence ~ danceability+energy+popularity, data = spotify_numeric_df)
model4 <- lm(valence ~ danceability+energy+popularity+acousticness
             , data = spotify_numeric_df)


model5 <- lm(valence ~ danceability+energy+popularity+acousticness+explicit+liveness+
               instrumentalness+key+mode
             , data = spotify_dataset)

summary(model1) #0.27
summary(model2) #0.34
summary(model3) #0.37
summary(model4) #0.38
summary(model5) #0.40

avPlots(model5)

plot(model5$residuals, pch = 16, col = "red")

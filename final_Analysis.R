library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(Rtsne)
library(wordcloud)


setwd('C:/Users/Erin/Documents/DataViz/HungryRapper/')
options(stringsAsFactors = FALSE)


load("englyrics_CLEAN.Rda")
load("artists_progress.Rda")
numsongs <- read.csv("number_of_songs.csv")
foods <- read.csv("food.csv")


artists <- artists %>% select(artist_id, artist_name, followers)

#clean up lyrics
englyrics <- subset(englyrics, artist_id_song != 117146 & artist_id_section != 117146)
englyrics$artist_id_section[englyrics$artist_id_song == 12012 & englyrics$artist_id_section %in% c(178, 2963)] <- 12012 #hail mary mallon
englyrics$artist_id_section[englyrics$artist_id_song == 5360 & englyrics$artist_id_section %in% c(37290, 3075)] <- 5360 #strong arm steady
englyrics$artist_id_section[englyrics$artist_id_song == 124 & englyrics$artist_id_section %in% c(12214, 13243)] <- 124 #das racist
englyrics$artist_id_section[englyrics$artist_id_song == 80 & englyrics$artist_id_section %in% c(1, 79, 4301, 78	)] <- 80 #diplomats

#hungriest songs
hungriest_song <- englyrics %>% select(song_name, artist_id_song, song_id, canonical) %>% unique()
hungriest_song <- hungriest_song %>% group_by(song_name, artist_id_song, song_id) %>% tally()
hungriest_song <- hungriest_song[order(-hungriest_song$n), ]

hungriest_song <- inner_join(hungriest_song, artists, by = c("artist_id_song" = "artist_id"))
hungriest_song <- subset(hungriest_song, followers >= 1000)

write.csv(hungriest_song,  "./FINAL Results/hungriest_song.csv", row.names =F)

#top food mentions
top_foods <- englyrics %>% select(song_id, lyrics_line, canonical) %>% unique %>% group_by(canonical) %>% tally()
top_foods <- top_foods[order(-top_foods$n), ]
write.csv(top_foods, "./FINAL Results/top_foods.csv", row.names =F)


wordcloud(words = top_foods$canonical, freq = top_foods$n, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0)


#artist that mentions the most foods
total_foods <- englyrics %>% select(artist_id = artist_id_section, canonical) %>% unique %>% group_by(artist_id) %>% tally()
total_foods <- inner_join(total_foods, artists, by = "artist_id")
total_foods <- total_foods[order(-total_foods$n), ]

write.csv(total_foods, "./FINAL Results/most_food_mentions.csv", row.names =F)

#raw food words per artist
foodwords <- englyrics %>% select(artist_id = artist_id_section, canonical) %>% group_by(artist_id, canonical) %>% tally()
foodwords <- inner_join(total_foods, foodwords, by = "artist_id")
names(foodwords)[2] <- "total"
names(foodwords)[6] <- "n"

write.csv(foodwords, "./FINAL Results/all_food_words_per_artist.csv", row.names =F)

#hungriest song per artist
hungriest_song <- englyrics %>% select(artist_id = artist_id_section, song_id, canonical) %>% unique %>% group_by(artist_id, song_id) %>% tally()
hungriest_song <- inner_join(hungriest_song, select(artists, artist_id, artist_name), by = "artist_id")
hungriest_song <- inner_join(hungriest_song, unique(select(englyrics, song_id, song_name)), by = "song_id")
hungriest_song <- hungriest_song %>% group_by(artist_id) %>% top_n(1, n)

write.csv(hungriest_song, "./FINAL Results/hungriest_song.csv", row.names =F)

#average artist's food words
avgwords <- select(foodwords, artist_id, canonical, n)
allwords <- expand.grid(unique(englyrics$canonical), unique(avgwords$artist_id))
names(allwords) <- c("canonical", "artist_id")
allwords$canonical <- as.character(allwords$canonical)

allwords <- left_join(allwords, avgwords, by = c("artist_id", "canonical"))
allwords$n[is.na(allwords$n)] <- 0

avgs <- allwords %>% group_by(canonical) %>% summarize(avg = mean(n))

sum(avgs$avg)

#percent of songs that mention food
pct_songs <-  numsongs %>% subset(n_songs >= 20) 
pct_songs <- pct_songs[order(-pct_songs$pct), ]

write.csv(pct_songs, "./FINAL Results/pct_songs_mention_food.csv", row.names =F)

#average # of unique foods mentioned per song
foods_per_song <- englyrics %>% subset(artist_id_section == artist_id_song) %>% select(artist_id_section, song_id, canonical) %>% unique()   %>% 
                              group_by(artist_id_section, song_id) %>% tally() %>% ungroup()
foods_per_song <- foods_per_song %>% group_by(artist_id_section) %>% summarize(foods = sum(n))
foods_per_song <- foods_per_song %>% inner_join(pct_songs, by = "artist_id_section") %>% subset(!is.na(n_songs))
foods_per_song$avg_num_foods <- foods_per_song$foods/foods_per_song$n_songs
foods_per_song <- select(foods_per_song, artist_id = artist_id_section, artist_name, avg_num_foods)
foods_per_song <- foods_per_song[order(-foods_per_song$avg_num_foods), ]

write.csv(foods_per_song, "./FINAL Results/num_foods_per_song.csv", row.names =F)


#do food-named artists mention more foods per song?
search <- read.csv("search.csv") 
search$food <- paste0("\\b", search$food, "\\b")
search <- paste0(search$food, collapse = "|")

artists$foodname <- grepl(search, artists$artist_name, ignore.case =T)
foodartists <- subset(artists, foodname == T) %>% select(artist_id, foodname)

foods_per_song <- left_join(foods_per_song, foodartists, by = "artist_id")
foods_per_song$foodname[is.na(foods_per_song$foodname)] <- F

write.csv(foods_per_song, "./FINAL Results/food_name_artists_num_foods_per_song.csv", row.names =F)

#t.test(foods_per_song$avg_num_foods[foods_per_song$foodname == T],foods_per_song$avg_num_foods[foods_per_song$foodname == F])



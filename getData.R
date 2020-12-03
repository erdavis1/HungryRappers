library(dplyr)
library(geniusr)
library(purrr)
library(spotifyr)
library(stringr)
library(cld2)
library(cld3)

setwd('C:/Users/Erin/Desktop/')
options(stringsAsFactors = FALSE)


Sys.setenv(SPOTIFY_CLIENT_ID = 'CLIENTID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'CLIENTSECRET')
spotify_token <- get_spotify_access_token()
genius_token <- 'TOKENHERE'


Sys.setenv(SPOTIFY_CLIENT_ID = '2b718f4bd105417e89b021230524642c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a641502d2635419e91e902aa040a95fb')
spotify_token <- get_spotify_access_token()
genius_token <- 'Pkh61BYc-6fzqk5DywmpeoxCNWzXRSgUx5Kc8BQsiAtys4EbmTfSLGPewThg8liw'


#get plurals of foods
food <- read.csv("food.csv")
consonants <- letters[!(letters %in% c("a", "e", "i", "o", "u", "y"))]
terms <- select(food, canonical = Food, food = Food) %>% 
  bind_rows(select(food, canonical = Food, food = Alt)) %>%
  subset(nchar(food) > 0)

plurals1 <- data.frame(canonical = terms$food, food = paste0(terms$food, "s"))

plurals2 <- terms[(substr(terms$food, nchar(terms$food) - 1, nchar(terms$food)-1) %in% consonants & 
                     substr(terms$food, nchar(terms$food), nchar(terms$food)) == "y"), ]
plurals2$food <- substr(plurals2$food, 1, nchar(plurals2$food)-1) %>% paste0("ies")

plurals3 <- terms[substr(terms$food, nchar(terms$food) - 1, nchar(terms$food)) %in% c("ch", "sh"), ]
plurals3$food <- substr(plurals3$food, 1, nchar(plurals3$food)) %>% paste0("es")

plurals4 <- terms[substr(terms$food, nchar(terms$food), nchar(terms$food)) %in% c("z", "s", "x"), ]
plurals4$food <- substr(plurals4$food, 1, nchar(plurals4$food)) %>% paste0("es")

terms <- bind_rows(terms, plurals1) %>% bind_rows(plurals2)  %>% bind_rows(plurals3)  %>% bind_rows(plurals4) %>% unique()



#search spotify for each food
songs <- NULL

for (i in 1:nrow(terms)) {
  temp <- search_song(terms$food[i], n = 10000, genius_token)
  temp$term <- terms$food[i]
  songs <- bind_rows(songs, temp)
}

songs <- songs %>% 
  group_by(song_id) %>% 
  mutate(terms = paste0(term, collapse = ",")) %>%
  mutate(terms = strsplit(terms, ",")) %>%
  select(song_id, song_name, artist_id, artist_name, terms) %>%
  unique() %>% ungroup



#search for artists on spotify
artists <- songs %>% select(artist_id, artist_name) %>% unique()
artists <- artists[1:2]
artists$genre <- NA
artists$followers <- NA


for (i in 1:nrow(artists)) {
  temp <- search_spotify(artists$artist_name[i], type = "artist", authorization  = spotify_token)
  
  if (nrow(temp) > 0) {
    temp <- subset(temp, name == artists$artist_name[i])
    if (nrow(temp) > 0) {
      artists$genre[i] <- temp$genres[1]
      artists$followers[i] <- temp$followers.total[1]
    }
  }
  
  if (i%%1000==0) {
    print(i)
    spotify_token <- get_spotify_access_token()
    save(artists, file = "artists_progress.Rda")
  }
}
save(artists, file = "artists_progress.Rda")


#filter to just rappers
artists <- artists %>% group_by(artist_id) %>% 
  mutate(genre = map_chr(genre, paste0, collapse = ",")) %>% 
  ungroup()

artists$is_rap <- grepl("hip hop|\\brap\\b", artists$genre)
rapartists <- subset(artists, is_rap == TRUE) %>% select(artist_id)

save(rapartists, file = "rapartists.Rda")

rapartists <- artists

#get lyrics for all rap songs
foodlyrics <- NULL
rapsongs <- inner_join(songs, rapartists, by = "artist_id" )

search <- terms
search$food <- paste0("\\b", search$food, "\\b")
search <- paste0(search$food, collapse = "|")

for (i in 1:nrow(rapsongs)) {
  httr::set_config(httr::config(http_version = 0))
  temp <- NULL
  result <- tryCatch(temp <- get_lyrics_id(rapsongs$song_id[i], access_token = genius_token), error=function(e) e)
  
  if (!inherits(result, "error")) {
    if (nrow(temp) > 0) {
      temp$lyrics_line <- seq.int(nrow(temp))
      temp$food <- str_extract_all(temp$line, regex(search, ignore_case = TRUE))
      temp$num_foods <- lengths(temp$food)
      temp <- subset(temp, num_foods > 0)
      temp$artist_id <- rapsongs$artist_id[i]
      foodlyrics <- bind_rows(temp, foodlyrics)
    }
  }
  
  if (i%%100==0) {
    print(i)
    save(foodlyrics, file = "foodlyrics.Rda")
  }
}

save(foodlyrics, file = "foodlyrics.Rda")

#keep only English lyrics
foodlyrics <- unique(foodlyrics)
foodlyrics$lang1 <- cld2::detect_language(foodlyrics$line)
foodlyrics$lang2 <- cld3::detect_language(foodlyrics$line)
foodlyrics <- subset(foodlyrics, lang1 == "en" | lang2=="en")


#get section artist IDs to credit featured rappers
foodlyrics <- left_join(foodlyrics, select(rapartists, artist_name, artist_id), by = c("section_artist" = "artist_name"))
names(foodlyrics)[11:12] <- c("artist_id", "artist_id_section")

#at this point, I did a bunch of manual review on the lyrics to clean them up










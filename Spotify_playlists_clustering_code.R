library(spotifyr)
library(cluster)
library(fpc)
library(tidyverse)

########################################################################
##get the session ids needed for authorization
id <- "3b9c6772e1f54c4eb48eccd01f90c413"
secret <- "###############################"

Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

##get the access token
access_token <- get_spotify_access_token()

##save my spotify user id
my_id <- 'dtj7fdsb05eq0rswpflu3su6z'
########################################################################

##The package already has a function to get tracks from a playlist but the maximum
##number of tracks you can get at once is set to 100. So I made a custom function
##to get all of the tracks from playlists with higher than 100 t racks
get_playlist_tracks_custom <- function(playlist_id_1) {
  
  #save the playlist info
  pp  <- get_playlist(playlist_id_1)
  ##save number of songs in the paylist 
  number_of_songs <- pp$tracks$total  

  ##make a sequence from 0 to number of songs by 100
  s <- seq(0,number_of_songs, 100)
  temp_s <- list()
  ind <- 1
  
  for (i in s){
    ##offset defines the index from which track info should be downloaded.
    ##for example if the playlist has 150 tracks then we will first get the first 100,
    ##and then get the remaining 150 by setting the offset to 100
    temp_s[[ind]] <- get_playlist_tracks(playlist_id_1, offset = i)
    ind <- ind + 1
  }
  return(bind_rows(temp_s))
}

##following this I made a function to put all of the songs from multiple playlists into
##one dataframe. Not that this uses the get_playlist_tracks_custom function from earlier. 
put_songs_from_playlists_into_df <- function(user_id, pl_names){
  ##get all playlists and then filter for only selected filters
  my_plists <- as_tibble(get_user_playlists(user_id, limit = 50))
  my_plists.filtered <- my_plists %>% filter(name %in% pl_names)
  
  ##using map iterate over the my_plists.filtered and get tracks for each playlist
  tracks_from_chosen_playlists <-  as_tibble(map_df(my_plists.filtered$id,get_playlist_tracks_custom)) 
 
   ##remove duplicates
  tracks_from_chosen_playlists_filtered <- tracks_from_chosen_playlists %>% group_by(track.uri) %>% mutate(number = n()) %>% 
    dplyr::filter(number == 1)
  
  return(ungroup(tracks_from_chosen_playlists_filtered))
}

##save the songs from selected playlists into a dataframe
tracks_from_playlists <- put_songs_from_playlists_into_df(my_id, c("on the beach", "hang out", "electronic sounds 1"))


##once again the maximum number of songs you can get features from in one go is 100.
##so we need to make a function that allows us to donwload all track features at once
playlist_audio_features <- function(full_songs_df) {
  ##save the number of songs
  number_of_songs <- nrow(tracks_from_playlists)
  final_df <- data.frame()
  songs <- dplyr::select(full_songs_df, track.id, track.name)
  
##in this function we simply download audio features for one track at a time.
##and then combine them together in a dataframe
  for (i in 1:number_of_songs) {
    
    s <- get_track_audio_features(songs[i,1])
    ss <- bind_cols(songs[i,], s)
    
    final_df <- bind_rows(final_df, ss)
    print(ss$track.name)
    
    
  }
  return(final_df)
}

##save the track_features from the selected songs into a dataframe
track_features <- playlist_audio_features(tracks_from_playlists)
##remove the unnecessary columns
track_features <- track_features %>% dplyr::select(-type, -id, -track_href, -analysis_url, -duration_ms, -time_signature, -key,
                                 -mode)

##before we cluster we need to scale the aduio features as they are on slightly different scales
track_features_scaled <- scale(dplyr::select(track_features, -track.id, -track.name, -uri), center = TRUE, scale = TRUE)
track_features_scaled_full <- bind_cols(dplyr::select(track_features, track.id, track.name, uri), as_tibble(track_features_scaled))

##these are all the possible audio features stored by Spotify
# "danceability"     "energy"           "loudness"        
# "speechiness"      "acousticness"     "instrumentalness"
# "liveness"         "valence"          "tempo"   


##this is the clustering function
##here we select the audio features that we want to have a high value in our new playlists,
##and we select audio features that we want to have a low value in our new playlist.
##we also specify the minimum number of songs we want in the new playlist.

##The function then runs the kmeans algorithm 10 times.
##In each iteration it finds the cluster which has the biggest difference between the sum of
##the "high features" and the sum of the "low features". 
##The idea behind this is that the playlists with the highest difference will have 
##a large sum of the high features and a low sum (likely negative) of the low features.

#Since the function runs the kmeans cluster 10 times, then we are able to further select
#the 3 playlists with the biggest difference, which are then outputted by the function.
##This gives the user some choice into what playlist they want to pick.
clustering_function <- function(songs_data, high_features, low_features, min_number_of_songs) {
  ##make empty lists to store features and song info in
  features_list <- list()
  songs_list <- list()
  
  ##combine all selected features in a vector
  all_slected_features <- c(high_features, low_features)
  #filter the track feature dataframe to ony have the selected features
  songs_data.filtered <- songs_data %>%  dplyr::select(all_slected_features)
  number_of_clusters <- round(nrow(songs_data)/min_number_of_songs)
  ##run k means 10 times
  niter <- 10
  
  for (i in 1:niter) {
  ##save kmeans output
  kmeans_output <- kmeans(songs_data.filtered, number_of_clusters)
  
  means <- as_tibble(kmeans_output$centers)
  means$cluster_number <- 1:number_of_clusters
  means$size <-   kmeans_output$size
 
 ##first make sure the cluster has around the minimum number of songs.
  means_w_sums <- means %>% dplyr::filter(size >= min_number_of_songs - number_of_clusters) %>% 
           ##sum features that are selected to be high in the new playlist
    mutate(high_sum = rowSums(.[colnames(means) %in% high_features]), 
           ##sum features that are selected to be low in the new playlist
           low_sum = rowSums(.[colnames(means) %in% low_features]), 
           ##get the difference between the sum of high features and sum of low features
           playlist_score = high_sum - low_sum)
 
  ##find which cluster has the biggest difference and select it.
 chosen_cluster_index <- which.max(means_w_sums$playlist_score)
 chosen_cluster <-  means_w_sums$cluster_number[chosen_cluster_index]
 
 songs_data$cluster_number <- as.factor(kmeans_output$cluster)
 songs_data.chosen_cluster <- songs_data %>% filter(cluster_number == chosen_cluster)
 
 ##save the features of the songs in the selected cluster as a dataframe in a list
 features_list[[i]] <- means_w_sums %>% filter(cluster_number == chosen_cluster) %>% 
   select(all_slected_features, playlist_score)
 ##save the names and uris of the songs in the selected cluster as a dataframe in a list
 songs_list[[i]] <- dplyr::select(songs_data.chosen_cluster, track.name, uri)
 
  }
  
  ##bind the features list into a dataframe
  features_list_df <-  bind_rows(features_list) 
  features_list_df$iteration <- 1:niter
  ##keep only unique clusters
  features_list_df_unique <-distinct(features_list_df,playlist_score,  .keep_all = T)
  ##arrange in their playlist_score (difference between high features sum and low feature sum)
  features_list_df.arranged <- features_list_df_unique %>% arrange(desc(playlist_score)) 
  ##select the top 3 playlists
  features_list_df_top3 <- features_list_df.arranged[1:3, ]
  
  ##extract features and songs for the top 3 features
  playlist_1_features <- features_list_df[features_list_df_top3$iteration[1], ]
  playlist_1_songs <- as_tibble(songs_list[[features_list_df_top3$iteration[1]]])

  playlist_2_features <- features_list_df[features_list_df_top3$iteration[2], ]
  playlist_2_songs <- as_tibble(songs_list[[features_list_df_top3$iteration[2]]])
  
  playlist_3_features <- features_list_df[features_list_df_top3$iteration[3], ]
  playlist_3_songs <- as_tibble(songs_list[[features_list_df_top3$iteration[3]]])
  
  ##return the top 3 playlists data in a list
return(top_3_chosen_playlists <- c(c(playlist_1_features =list(playlist_1_features), playlist_1_songs=list(playlist_1_songs)),
      c(playlist_2_features =list(playlist_2_features), playlist_2_songs=list(playlist_2_songs)),
  c(playlist_3_features =list(playlist_3_features), playlist_3_songs=list(playlist_3_songs))))
}


##run the clustering function to get a playlist that is high in danceability and
#valence but is low in speechiness and loudness. With a minimum of 20 songs
clustering_function_output <- clustering_function(track_features_scaled_full, c("danceability","valence"),  c("speechiness","loudness"),
                    20)

##choose the one paylist out of the top three presented
playlist_I_chose <- clustering_function_output$playlist_1_songs

##just as before we can only add 100 songs to a playlist at a time. 
##so we need to make a custom function which allows to add as many songs at once as we 
##want
put_selected_songs_in_a_playlist <- function(user_id,chosen_playlst_df, name_of_the_new_playlist) {
  ##first make an empty new playlist
  new_playlist <-  create_playlist(user_id, name_of_the_new_playlist)
  new_playlist_id <- new_playlist$id
  
  tracks_from_chosen_playlists <- chosen_playlst_df
  ##here we split the songs dataframe into segments of 100
  ##for some reason it is impossible to add just one track to the playlist. 
  ##so if the chosen playlist has 101 songs than we need to split it into segments of 99
  ##so the second segment has 2 songs instead of just 1
  if (nrow(tracks_from_chosen_playlists) %% 100 == 1) {
  tracks_from_chosen_playlists.split <- split(tracks_from_chosen_playlists, (as.numeric(rownames(tracks_from_chosen_playlists)) - 1) %/% 99) 
  } else {
    tracks_from_chosen_playlists.split <- split(tracks_from_chosen_playlists, (as.numeric(rownames(tracks_from_chosen_playlists)) - 1) %/% 100) 
  }
  ##upload the chosen songs to the playlist in segments
    for (i in 1:(length(tracks_from_chosen_playlists.split))) {
      
      add_tracks_to_playlist(new_playlist_id,tracks_from_chosen_playlists.split[[i]]$uri)
      print(tracks_from_chosen_playlists.split[[i]]$track.name)
    }
  }

##upload the tracks to the playlist
put_selected_songs_in_a_playlist(user_id, playlist_I_chose, "Github_showcase")


library(httr)
library(Hmisc)
library(jsonlite)
library(dplyr)
library(devtools)
#devtools::install_github(repo='maksimhorowitz/nflscrapR')
library(nflscrapR)
library(stats)
## function defs ########################
clean_names <- function(dat){
   # Takes a data.frame, returns the same data frame with cleaned names
   old_names <- names(dat)
   new_names <- old_names %>%
      gsub("%", "percent", .) %>%
      make.names(.) %>%
      gsub("[.]+", "_", .) %>%
      tolower(.) %>%
      gsub("_$", "", .)
   setNames(dat, tolower(new_names))
}



## input params ###############################################################

year <- ## only needed for grabbing the pbp data if it's already loaded, then no need
	2018

rush_att_min <- ## needed for the rushing function. Min attempt filter for average curve
	10

player_groupby <-  ## needed for the rushing function. 
	'rusher_team' ## Note: Allowing for a different groupby just in case 
					  ## our name/team combo changes. eg... if we merge in the 
                 ## full player/team names to the pbp data, we can use that to groupby


player_name_team <- ## needed for rushing function. Players to look at
	c('M.Breida, SF', 'A.Jones, GB')

## Import/Format pbp data #####################################################
## grab data
pbp_data <-
	clean_names(season_play_by_play(year))
	
## make player team columns 
pbp_data <-
	pbp_data %>%
	mutate(rusher_team = paste0(rusher, ', ', posteam),
			 receiver_team = paste0(receiver, ', ', posteam), 
			 passer_team = paste0(passer, ', ', posteam)) %>%
	filter(posteam != '')


## rushing distribution function !!!! 

rushing_distirbution <- function(pbp_data, rush_att_min = 10 , 
											player_groupby = 'rusher_team', 
											player_name_team = 'M.Breida, SF'){
	
	## Filter data down to rushes only and non-penalty plays ######################
	rushing_cols <-
		c('rusher', player_groupby, 
		  'yards_gained', 'playtype', 
		  'penaltytype')
	
	rush_data <-
		pbp_data[, rushing_cols] %>%
		filter(playtype == 'Run', 
				 is.na(penaltytype)
		) %>%
		mutate(gb_col = player_groupby)
	
	## groupby sum all rushing plays and filter down to players with ypa min 
	## going to use this to establish baseline 
	rush_players <- 
		rush_data %>%
		group_by(gb_col) %>%
		summarise(rushatts = n(),
					 yards_gained = sum(yards_gained),
					 rushatt_min = rushatts > rush_att_min) %>%
		filter(rushatt_min == TRUE) %>%
		arrange(-rushatts) %>%
		distinct(gb_col)
	
	## calculate league average based on players from above
	avg_rush_points <-
		rush_data %>%
		filter(gb_col %in% rush_players$gb_col) %>%
		group_by(yards_gained) %>%
		summarise(count = n()) %>%
		mutate(total_rushes = sum(count),
				 percent_of_runs = 100*count/total_rushes,
				 player = 'League Average') %>%
		arrange(-yards_gained) %>%
		select(yards_gained, percent_of_runs, player, count)
	
	## calculate average for each player
	player_rush_points <- 
		NULL
	
	for(i in player_name_team){
		player_rush_points <- 
			rush_data %>%
			filter(rusher_team == i) %>%
			group_by(yards_gained) %>%
			summarise(count = n()) %>%
			mutate(total_rushes = sum(count),
					 percent_of_runs = 100*count/total_rushes,
					 player = i) %>%
			arrange(-yards_gained) %>%
			select(yards_gained, percent_of_runs, player, count) %>%
			bind_rows(player_rush_points)
		
	}
	
	
	## combine player and average to easily plot
	rush_plot_points <-
		avg_rush_points %>%
		bind_rows(player_rush_points)

	min_x <- ## min x axis value
		5*round(min(rush_plot_points$yards_gained)/5)
	
	max_x <- ## max x axis value
		5*round(max(rush_plot_points$yards_gained)/5)
	
	ggplot(rush_plot_points, 
			 aes(x = yards_gained,
			 	 y = percent_of_runs, 
			 	 color = player)) +
		geom_line() +
		labs(title = "Percentage of Running Back Runs by Yards Gained", 
			  y = "Percent of Runs", 
			  x = "Yards Gained") + 
		scale_x_continuous(limits = c(min_x, max_x), 
								 breaks = seq(min_x, max_x,5)) + 
		#geom_point(size = 0.5) + 
		annotate("text", x = Inf, y = Inf, label = "FFSTATISTICS",
					hjust=1.1, vjust=1.1, col="white", cex=6,
					fontface = "bold", alpha = 0.8)
}




rushing_distirbution(pbp_data, 
							rush_att_min = 10 , 
							player_groupby = player_groupby, 
							player_name_team = player_name_team)


options(stringsAsFactors = FALSE)
library(httr)
library(Hmisc)
library(jsonlite)
library(tidyr)
library(dplyr)

## quick function defs  ###########
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



na.is.zero <-
	function(X, value = 0) {
		
		X1 <-
			X
		
		X1[is.na(X1)] <-
			value
		
		return(X1)
		
	}


filter <-
	dplyr::filter

select <-
	dplyr::select


summarize <- 
	dplyr::summarize

summarise_all <-
	dplyr::summarize

count <- 
	dplyr::count

gather <-
	tidyr::gather



## Main function definition, baby!! #####################
weekly_split <- function(df_raw, player_pos_name, measure_vars, year_filter, 
								 week_split, player_pos_split, home_away_split, 
								 opponent_split, gameday_split, gamestart_time_split,
								 posrank_year = 2017, transpose_output = TRUE) {
	# Function that splits weekly data into in split and out split averages,totals, and projections
	# 
	# Args:
	#   df_raw: data.frame of weekly data
	#   player_pos_name: string input of player, position to 
	#   					look at (should be format 'First Last, POS' ie 'Marvin Jones, WR')
	#   measure_vars: vector of variables to average, total, and project
	#   year_filter: vectors of years to filter data down to 
	#   week_split: vector of weeks to split on 
	#   player_pos_split: vector of player(s) to split on (should be 
	#                 format c('First Last, POS') ie cc("Kenny Golladay, WR")). Can be NA 
	#                 or 'any' if no player split desired.
	#   home_away_split: vector of home/away classifiers. 1 is home, 0 is away (if you 
	#                 want home or away, then specify c(0:1) or 'Either' or NA) 
	#   opponent_split: vector of opponenets to split on. NA or 'any' is no opponent split
	#   gameday_split: vector of days to split on; format 'Mon', 'Tue', etc. NA or 'any' is no game day split. 
	#   gamestart_time_split: vector of times to split on; values should be in 1, 4, 7, 8, 9, 10, 12
	#                  specify NA or 'any' for no start time split.                   
	#   posrank_year: numeric value for year to determine projected position rank (default is 2017)
	#   transpose_output: TRUE or FALSE indicating if you want the result to be transposed 
	# Returns:
	#   data frame of in split and out split metrics for the given criteria
	
	measure_vars <- ## ensure lowercase values
		tolower(measure_vars)
	
	## create processed df with clean names, split column, and player_pos column
	
	df <-
		clean_names(df_raw) %>%
		mutate(player_pos = paste(player, position, sep = ', '),
				 split_col = paste0(year, week, home, opponent, gameday, gamestarttime)) %>%
		select(year, week, home, opponent, 
				 gameday, gamestarttime, position, 
				 measure_vars, player_pos, split_col) 
	
	
	## adjust player_pos_split based on NA input 
	if(any(tolower(player_pos_split) %in% c(NA, 'any'))){
		player_pos_split <- ## if NA then just grab player_pos_name
			player_pos_name
	}
	
	## adjust home_away_split
	if(any(tolower(home_away_split) %in% c('either', NA))){
		home_away_split <-
			c(0:1)
	}
	
	## adjust opponent_split
	if(any(opponent_split %in% c(NA, 'any'))){
		opponent_split <-
			unique(df$opponent)
	}
	
	## adjust gameday_split
	if(any(gameday_split %in% c(NA, 'any'))){
		gameday_split <-
			unique(df$gameday)
	}
	
	## adjust gamestart_time_split
	if(any(gamestart_time_split %in% c(NA, 'any'))){
		gamestart_time_split <-
			unique(df$gamestarttime)
	}
	
	
	split_groups <- ## get unique split criteria for the primary player
		df %>%
		filter(player_pos == player_pos_name, 
				 year %in% year_filter,
				 week %in% week_split,
				 home %in% home_away_split,
				 opponent %in% opponent_split,
				 gameday %in% gameday_split,
				 gamestarttime %in% gamestart_time_split) %>%
		distinct(split_col)
	
	
	df_split <- ## create DF of in split criteria 
		df %>%
		filter(split_col %in% split_groups$split_col, 
				 player_pos %in% player_pos_split,
				 year %in% year_filter) %>%
		mutate(in_split = 1) %>%
		select(split_col, in_split)
	
	df_player <- ## get player specific data
		df %>%
		filter(player_pos == player_pos_name,
				 year %in% year_filter)
	
	## merge split data w/ player data to figure out 
	## when primary player was in the split criteria
	df_player <- ## merge in split data 
		merge(df_player, df_split, by = c('split_col'), all.x = TRUE)
	
	df_player$in_split <- ## zero out the mis-matches (these are out of split values)
		na.is.zero(df_player$in_split)
	
	if(length(measure_vars) > 1){
		res <- ## create initial result - take means and totals, project ppr points
			df_player %>%
			group_by(in_split) %>%
			summarize_at(.vars = measure_vars,
							 .funs = c(mean="mean")) %>% #, total="sum")) %>%
			mutate_at(.funs = funs(projection = .*16), 
						 .vars = vars(paste0(measure_vars, "_mean"))) %>%
			mutate(player = player_pos_name)
	} else {
		res <- ## create initial result - take means and totals, project ppr points
			df_player %>%
			group_by(in_split) %>%
			summarize_at(.vars = measure_vars,
							 .funs = c(mean="mean")) %>% #, total="sum")) %>%
			mutate_at(.funs = funs(projection = .*16), 
						 .vars = vars("mean")) %>%
			mutate(player = player_pos_name)
		res[, paste0(measure_vars, "_mean")] <- res$mean
		res[, paste0(measure_vars, '_mean_projection')] <- res$projection
		#res[, paste0(measure_vars, "_total")] <- res$sum
		res$mean <- NULL
		res$projection <- NULL
	}
	
	## need to account for variables that don't need to be projected ie rams and tms
	for(i in c('rams' ,'tms')){
		if(i %in% measure_vars){
			res[, paste0(i, '_mean_projection')] <-
				res[, paste0(i, '_mean')]
		}
	}
	
	df_count <- ## data frame of total games 
		df_player %>%
		group_by(in_split) %>%
		summarize(games = n())
	
	res <- ## merge in total games 
		merge(res, df_count, by = 'in_split', all.x = TRUE)
	
	
	## scoring to rank c('ppr', 'halfppr', 'standard', 'fourpttd', 'sixpttd')
	## 
	## Rank the projected ppr values if ppr_mean_projection exists
	posrankvars <-
		c('ppr', 'halfppr', 'standard', 'fourpttd', 'sixpttd')
	
	for(i in intersect(posrankvars, measure_vars)){
		# i <- 'ppr'
		pos <- ## get player position
			unlist(strsplit(player_pos_name, ',', fixed = TRUE))[2]
		
		pos <- ## make sure we're getting the correct values 
			substr(pos, nchar(pos)-1, nchar(pos))
		
		i_mean_proj <-
			paste0(i, "_mean_projection")
		
		df$points <-
			df[, i]
		
		df_posrank <- ## calculate position rank for in/out split of projected points 
			df %>% ## fitler down to year, position, and not player 
			filter(year == posrank_year, 
					 position == pos, 
					 week %in% c(1:16),
					 player_pos %nin% c(player_pos_name)) %>%
			group_by(player_pos) %>% ## group by sum points 
			summarize(total_in_split_1 = sum(points),
						 total_in_split_0 = sum(points)) %>% ## bind player projected pts
			bind_rows(
				data.frame(player_pos = player_pos_name,
							  total_in_split_1 = res[res$in_split==1, i_mean_proj],
							  total_in_split_0 = res[res$in_split==0, i_mean_proj]
							  )
				) %>% ## add in rank columns
			mutate(
				pos_rank_in_split_1 = dense_rank(-total_in_split_1),
				pos_rank_in_split_0 = dense_rank(-total_in_split_0)) %>%
			filter(player_pos==player_pos_name) %>% ## filter down to player and select columns
			select(pos_rank_in_split_1, pos_rank_in_split_0) 
	
		df_posrank <- ## transpose df_posrank and turn into dataframe
			data.frame(t(df_posrank))
		
		names(df_posrank) <- ## rename df_posrank
			paste0(i, '_posrank_projection')
		
		df_posrank$in_split <- ## create split column for df_posrank based on names
			ifelse(row.names(df_posrank) == 'pos_rank_in_split_1', 1, 0)
		
		res <- ## merge df_posrank with res and we're done! 
			merge(res, df_posrank, by = 'in_split', all.x = TRUE) 
		
	}
	
	## fill in gaps for missing data 
	if(1 %nin% unique(res$in_split)){
		temp <- 
			res
		
		temp$in_split <- 
			1
		
		temp[, setdiff(names(temp), 'in_split')] <- 
			0 
		
		res <-
			rbind(res, temp)
	}
	
	## fill in gaps for missing data 
	
	if(0 %nin% unique(res$in_split)){
		temp <- 
			res
		
		temp$in_split <- 
			0
		
		temp[, setdiff(names(temp), 'in_split')] <- 
			0 
		
		res <-
			rbind(res, temp)
	}
	
	res <-
		res %>%
		mutate_if(is.numeric, round, 2)
	
	## transpose data frame for easy display if TRUE
	if(transpose_output){
		res <- ## arrange in descending order, then transpose, and turn into df
			data.frame(
				res %>%
					arrange(-in_split) %>%
					t(.)
			)
		
		
		names(res) <- ## rename 
			c('in_split', 'out_of_split')
		
		res$metric <- ## add metric column
			row.names(res)
		
		row.names(res) <- ## remove row names
			NULL
		
		res <- ## re-order columns so metric is 1st
			res %>%
			filter(metric != 'player') %>%
			mutate(player = player_pos_name) %>%
			select(player, metric, in_split, out_of_split)
	}
	
	## return result
	return(res)
	
}




######## Test function out ####################################################
###############################################################################
###############################################################################

## read data ###############

fp <-
	'/Users/zenomuscarella/Documents/FF_projs/data/ffstatistics/'


fn <-
	'finalWeeklyData.csv'

df_raw <-
	read.csv(paste0(fp, fn))

## definingg function inputs 

player_pos_name <- ## player to look at - aka primary player
	'Marvin Jones, WR'
	#'Kerryon Johnson, RB'

measure_vars <- ## variables to aggregate -- you can add any metrics from the data to this vector
	c('ppr', 'standard', 'tms')#,'receptions', 'rectds', 'targets', 'recyards')

year_filter <- ## year filter on data
	c(2017:2017)

week_split <- ## weeks to split on
	c(1:17)

player_pos_split <- ## player(s) to split on; NA or 'any' indicates no player split
	c("Kenny Golladay, WR") ## <-- example 
	#c(NA) ## <-- example (also can put 'any')

home_away_split <- ## game types to split on; 1=home, 0=away, 0:1 = either
	c(0:1) 

opponent_split <- ## vector of opponent abreviations to split on
	c(NA) ## <-- example (can also put 'any')
	#c('NO', 'ARI')  ## <-- example 

gameday_split <- ## vector of gameday values to split on; NA or 'any' is any gameday 
	c(NA) ## other values are 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'
	#c('Mon', 'Sat', 'Thu') ## <-- example (can list as many values are you want)


gamestart_time_split <- ## vector of game times to split on; NA is any gametime
	c(NA) ## other values are 1, 4, 7, 8, 9, 10, 12
	#c(1,12)


### testing/running the function !!!!!! 
## run function w/ transposed output 
res <- 
	weekly_split(df_raw = df_raw, 
					 player_pos_name = player_pos_name, 
					 measure_vars = measure_vars, 
					 year_filter = year_filter, 
					 week_split = week_split,
					 player_pos_split = player_pos_split, 
					 home_away_split  = home_away_split,
					 opponent_split = opponent_split,
					 gameday_split = gameday_split,
					 gamestart_time_split = gamestart_time_split,
					 posrank_year = 2017,
					 transpose_output = TRUE)


## run function w/ non transposed output
res2 <- 
	weekly_split(df_raw = df_raw, 
					 player_pos_name = player_pos_name, 
					 measure_vars = measure_vars, 
					 year_filter = year_filter, 
					 week_split = week_split,
					 player_pos_split = player_pos_split, 
					 home_away_split  = home_away_split,
					 posrank_year = 2017,
					 transpose_output = FALSE)

res
res2

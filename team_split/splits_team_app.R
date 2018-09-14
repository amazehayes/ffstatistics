
options(stringsAsFactors = FALSE)
library(httr)
library(Hmisc)
library(jsonlite)
library(dplyr)
require(tidyr)

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


## data parameters

fp <-
   '/Users/zenomuscarella/Documents/FF_projs/data/ffstatistics/'


fn <-
   'finalWeeklyData.csv'

df_raw <-
   read.csv(paste0(fp, fn))


## other params


player_pos_name <- ## player to look at - aka primary player
   #'Chris Hogan, WR'
   'Marvin Jones, WR'

year_filter <- ## year filter on data -- only one value allowed
   c(2017)


## data processing 
df <- 
   clean_names(df_raw) %>%
   mutate(player_pos = paste(player, position, sep = ', '),
          split_col = paste0(week, year, team)
          ) %>%
   filter(year %in% year_filter)

split_groups <-
   df %>%
   filter(player_pos == player_pos_name) %>%
   distinct(split_col, team)


df_split <-
   df %>%
   filter(team %in% unique(split_groups$team)) %>%
   mutate(in_split = ifelse(split_col %in% split_groups$split_col, 1,0))
   

## win % and average points 
win_split <-
   df_split %>%
   distinct(split_col, win, points, in_split) %>%
   group_by(in_split) %>%
   summarize(win_perc = mean(win),
             average_points = mean(points),
             games = n()
             )
   
   
## team tendencies 

team_split <-
   df_split %>%
   group_by(in_split) %>%
   summarize(
             run_pass_ratio = sum(rushatt)/sum(passatt),
             pass_perc  = sum(passatt)/(sum(passatt) + sum(rushatt)),
             run_perc  = sum(rushatt)/(sum(passatt) + sum(rushatt)),
             pass_tds = sum(passtds),
             rush_tds = sum(rushtds),
             pass_tds_perc = sum(passtds)/(sum(passtds) + sum(rushtds)),
             rush_tds_perc = sum(rushtds)/(sum(passtds) + sum(rushtds)),
             total_targets = sum(targets),
             total_touches = sum(rushatt) + sum(receptions),
             ypa = sum(recyards)/sum(passatt),
             ypc = sum(rushyards)/sum(rushatt),
             pass_yds_avg = mean(passyards),
             rush_yds_avg = mean(rushyards)
             )


## positional metrics
positional_split_targets <- 
	df_split %>%
	group_by(in_split, position) %>%
	summarize(targets = sum(targets)) %>%
	spread(position, targets)


positional_split_touches <-
	df_split %>%
	group_by(in_split, position) %>%
	summarize(touches = sum(receptions) + sum(rushatt)) %>%
	spread(position, touches)


names(positional_split_targets) <-
	gsub('in_split_targets', 'in_split', 
		  tolower(
		  	paste0(names(positional_split_targets), '_targets')
		  	)
	)

names(positional_split_touches) <-
	gsub('in_split_touches', 'in_split', 
		  tolower(
		  	paste0(names(positional_split_touches), '_touches')
		  )
	)

## merge everythign together and create ms metrics
positional_split <-
	merge(positional_split_targets, positional_split_touches, by = 'in_split', all = TRUE)

res <-
   merge(win_split, team_split, by = 'in_split', all =TRUE) %>%
	merge(., positional_split, by = 'in_split', all = TRUE) %>%
	mutate(rb_target_share = rb_targets/total_targets, 
			 wr_target_share = wr_targets/total_targets, 
			 te_target_share = te_targets/total_targets,
			 rb_touch_share = rb_targets/total_touches, 
			 wr_touch_share = wr_targets/total_touches, 
			 te_touch_share = te_targets/total_touches) %>%
	select(-qb_targets, 
			 -rb_targets, 
			 -wr_targets, 
			 -te_targets, 
			 -total_targets,
			 -qb_touches, 
			 -rb_touches, 
			 -wr_touches, 
			 -te_touches, 
			 -total_touches
			 ) %>%
	mutate_if(is.numeric, round, 2)


## ensure we have an in split and out of split
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


## transpose data frame for easy table use
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
	mutate(player = player_pos_name) %>%
	select(player, metric, in_split, out_of_split)





library(XML)
library(jsonlite)

parse.touchdown <- function(play) {
  play$TD <- FALSE
  
  td_regex <- paste0("(?<touchdown>touchdown|TD)|\\(", name.pattern, "(kick|PAT)( (good|missed|blocked|no good))?\\)")
  
  if (grepl(td_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    play$TD <- TRUE
  }
  
  return(play)
}

parse.timeout = function(pbp, play) {
  play$timeout = FALSE
  play$timeout_team = NA
  
  timeout_regex = "Timeout (?<team>[-a-zA-Z\\. ']+).* (?<min>\\d{1,2})?:(?<sec>\\d\\d)"
  
  if (grepl(timeout_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    play$timeout = TRUE
    
    match = regex(timeout_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    if (!is.na(match[1,'team'])) {
      teams = c(play$poss, play$def)
      costs = list(insertions=2, deletions=0, substitutions=1)
      t1.unlike = adist(teams[1], match[1,'team'], costs=costs, ignore.case=TRUE)
      t2.unlike = adist(teams[2], match[1,'team'], costs=costs, ignore.case=TRUE)
      if (t1.unlike != t2.unlike) {
        play$timeout_team = teams[which.min(c(t1.unlike, t2.unlike))]
      } else {play$timeout_team = match[1,'team']}
    }
  }
  
  return(play)
}

parse.special <- function(play) {
  play$kicker <- NA
  play$made <- NA
  play$returner <- NA
  play$kick_dist <- NA
  play$touchback <- FALSE
  play$kick_return <- NA
  
  play <- parse.kickoff(play)
  play <- parse.punt(play)
  play <- parse.pat(play)
  play <- parse.fg(play)
  
  return(play)
}

parse.sack <- function(pbp, play) {
  play$sack <- FALSE
  play$sack.credit <- NA
  
  sack_regex <- paste0("(?<passer>", name.pattern, ") (sack|sacked)( by (?<sackcredit>", name.pattern, "))? for ((?<gain>\\d+) ",
                       "(yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|(?<nogain>no gain))")
  
  if (grepl(sack_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match = regex(sack_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    play$sack <- TRUE
    play$pass <- TRUE
    play$passer <- format.name(match[1,'passer'])
    play$sack.credit <- format.name(match[1, 'sackcredit'])
    
    if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
    else if (!is.na(match[1,'loss'])) {play$gain <- -as.numeric(match[1,'loss'])}
    else if (!is.na(match[1,'nogain'])) {play$gain <- 0}
  }
  
  return(play)
}

parse.rush <- function(pbp, play) {
  play$rush <- FALSE
  play$kneeldown <- FALSE
  
  # regular expresions to identify rushing plays
  rush_regex1 <- paste0("(?<player>", name.pattern, ") (run|rush) [\\s\\w]*for ((?<gain>\\d+) ",
                        "(yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|(?<nogain>no gain))")
  rush_regex2 <- paste0("(?<player>", name.pattern, ") (?<gain>\\d+) (yd|yard)s? (run|rush)")
  
  
  if (grepl(rush_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match <- regex(rush_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    play$rush <- TRUE
    play$carrier <- format.name(match[1,'player'])
    
    if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
    else if (!is.na(match[1,'loss'])) {play$gain <- -as.numeric(match[1,'loss'])}
    else if (!is.na(match[1,'nogain'])) {play$gain <- 0}
  } else if (grepl(rush_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match <- regex(rush_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    play$rush <- TRUE
    play$carrier <- format.name(match[1,'player'])
    
    if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
  }
  
  # kneeldowns are coded as "team rush"
  if (play$rush && play$carrier %in% c('TEAM', 'team', 'Team')) {
    play$rush <- FALSE
    play$kneeldown <- TRUE
  }
  
  return(play)
}

parse.punt <- function(play) {
  play$punt <- FALSE
  play$faircatch <- NA
  
  punt_regex <- paste0("(?<punter>", name.pattern, ") punt for (?<kickdist>\\d{1,3}) ",
                       "(yd|yard)s?(.*(?<touchback>touchback).*|.*out[- ]of[- ]bounds at|.*fair catch by ",
                       "(?<catcher>", name.pattern, ") at|.*returned by (?<returner>", name.pattern, ") ",
                       "for (((?<retgain>\\d{1,3}) (yd|yard)s|(a )?loss of (?<retloss>\\d+) ",
                       "(yd|yard)s?|(?<retnogain>no gain)))?)?")
  
  if (grepl(punt_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match <- regex(punt_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    play$punt <- TRUE
    play$kicker <- format.name(match[1, 'punter'])
    play$returner <- format.name(match[1,'returner'])
    play$kick_dist <- match[1,'kickdist']
    
    if (!is.na(match[1,'touchback'])) {
      play$touchback <- TRUE
      play$kick_return <- 0
    } else {play$touchback <- FALSE}
    
    if (!is.na(match[1,'catcher'])) {
      play$faircatch <- TRUE
      play$returner <- format.name(match[1,'catcher'])
      play$kick_return <- 0
    } else {play$faircatch <- FALSE}
    
    if (!is.na(match[1,'retgain'])) {play$kick_return <- as.numeric(match[1,'retgain'])}
    else if (!is.na(match[1,'retloss'])) {play$kick_return <- -as.numeric(match[1,'retloss'])}
    else if (!is.na(match[1,'retnogain'])) {play$kick_return <- 0}
  }
  
  return(play)
}

parse.penalty <- function(play, meta) {
  play$penalty <- FALSE
  play$penalty_type <- NA
  play$penalized_player <- NA
  play$penalty_dist <- NA
  play$penalized_team <- NA
  
  pbp <- play$pbp
  team.pattern <- paste(meta$home.school, meta$home.abbrev, meta$away.school, meta$away.abbrev, sep='|')
  
  penalty_regex0 <- paste0("(?<team>", team.pattern, ") penalty,? (\\(?-?(?<dist>\\d{1,3}) (yd|yard)s?\\)? ",
                           "(?<penalty>[-a-zA-Z\\. ']+?)?( on \\(?(?<player>", name.pattern, ")\\)?)? ",
                           "(?<decision>accepted|declined))?")
  
  penalty_regex <- paste0("(?<team>", team.pattern, ") penalty,? (?<penalty>[-a-zA-Z\\. ']+) \\(-?(?<dist>\\d{1,3}) (yd|yard)s?\\)",
                          "( \\(?(?<player>", name.pattern, ")\\)?)?",
                          "( (?<decision>accepted|declined)?)?")
  
  penalty_regex2 <- paste0("penalty,? (?<team>", team.pattern, ") (?<penalty>[-a-zA-Z\\. ']+) \\(-?(?<dist>\\d{1,3}) (yd|yard)s?\\)",
                           "( \\(?(?<player>", name.pattern, ")\\)?)?",
                           "( (?<decision>accepted|declined)?)?")
  
  penalty_regex3 <- paste0("penalty,? (?<team>", team.pattern, ") (?<penalty>[-a-zA-Z\\. ']+)",
                           "( \\(?(?<player>", name.pattern, ")\\)?)?",
                           " \\(?-?(?<dist>\\d{1,3}) (yd|yard)s?\\)?",
                           "( (?<decision>accepted|declined)?)?")
  
  penalty_regex4 <- paste0("(?<team>", team.pattern, ") penalty,? (?<penalty>[-a-zA-Z\\. ']+)",
                           "( \\(?(?<player>", name.pattern, ")\\)?)?",
                           "( \\(?-?(?<dist>\\d{1,3}) (yd|yard)s?\\)?)?",
                           "( (?<decision>accepted|declined)?)?")
  
  patterns <- c(penalty_regex, penalty_regex2, penalty_regex3, penalty_regex4, penalty_regex0)
  penalty <- sapply(patterns, function(p) grepl(pattern=p, x=pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) 
  
  if (any(penalty)) {
    match <- regex(patterns[which(penalty)[1]], pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    play$penalty = TRUE
    play$penalty_type = match[1,'penalty']
    play$penalized_player = format.name(match[1,'player'])
    play$penalty_dist = as.numeric(match[1,'dist'])
    
    #Identify the penalized team
    play$penalized_team <- assign.team(match[1, 'team'], meta$home.abbrev, meta$away.abbrev)
  }
  
  return(play)        
}

parse.pat <- function(play) {
  play$PAT <- FALSE
  
  # pat_regex1 = paste0("(?<kicker>", name.pattern, ")",
  #     " (extra point|KICK|PAT)",
  #     "( (?<made>GOOD|MADE)| (?<missed>MISSED|NO GOOD|BLOCKED))")
  pat_regex = paste0("(TD|touchdown)[^[:alnum:]]* \\(?(?<kicker>", name.pattern, ")",
                     " (extra point|KICK|PAT)",
                     "( (?<made>GOOD|MADE)| (?<missed>MISSED|NO GOOD|BLOCKED))?\\)?")
  
  if (grepl(pat_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    play$PAT <- TRUE
    play$made <- TRUE
    
    match <- regex(pat_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    play$kicker <- format.name(match[1,'kicker'])     
    
    if (!is.na(match[1,'missed'])) {
      play$made <- FALSE
    }
  }
  
  return(play)
}


parse.pass <- function(pbp, play) {
  play$pass <- FALSE
  play$complete <- NA
  play$passer <- NA
  
  pass_regex1 <- paste0("(?<QB>", name.pattern, ") pass ((?<complete>complete)|",
                        "(?<incomplete>incomplete))(( to (?<receiver>",name.pattern, ").*(?(complete) for ",
                        "((?<gain>\\d+) (yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|",
                        "(?<nogain>no gain))))?)?", sep="")
  
  pass_regex2 <- paste0("(?<receiver>", name.pattern, ") ((?<gain>\\d+) (yd|yard)s? )?pass( (?<complete>complete))? .*from (?<QB>", name.pattern, ")")
  
  # test whether this play matches any off the pass patterns
  pass.patterns <- c(pass_regex1, pass_regex2)
  pass.match <- sapply(pass.patterns, function(p) grepl(pattern=p, x=pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))
  
  if (any(pass.match)) {
    match <- regex(pass.patterns[which(pass.match)[1]], pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    # match = regex(pass_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    play$pass <- TRUE
    play$passer <- format.name(match[1,'QB'])
    play$carrier <- format.name(match[1,'receiver'])
    
    if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
    else if (!is.na(match[1,'loss'])) {play$gain <- -as.numeric(match[1,'loss'])}
    else if (!is.na(match[1,'nogain'])) {play$gain <- 0}
    
    if (!is.na(match[1,'complete'])) {play$complete <- TRUE}
    else {
      play$complete <- FALSE
      play$gain <- 0
    }
  } # else if (grepl(pass_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
  #     match = regex(pass_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
  #     play$pass = TRUE
  #     play$passer = format.name(match[1,'QB'])
  #     play$carrier = format.name(match[1,'receiver'])
  # 
  #     if (!is.na(match[1,'gain'])) {
  #         play$gain = as.numeric(match[1,'gain'])
  #         play$complete = TRUE    
  #     }
  #     #else if (!is.na(match[1,'loss'])) {play$gain = -as.numeric(match[1,'loss'])}
  #     #else if (!is.na(match[1,'nogain'])) {play$gain = 0}
  # 
  #     #if (!is.na(match[1,'complete'])) {play$complete = TRUE}
  #     #else {
  #     #    play$complete = FALSE
  #     #    play$gain = 0
  #     #}
  # }
  
  return(play)
}

parse.kickoff <- function(play) {
  play$kickoff <- FALSE
  
  kickoff_regex <- paste("(?<kicker>", name.pattern, ") kickoff for (?<kickdist>\\d{1,3}) ",
                         "(yd|yard)s?\\s?,? (((?<returner1>", name.pattern, ") return|returned by (?<returner2>", name.pattern, ")) for ((?<retgain>\\d{1,3}) ",
                         "(yd|yard)s?|(a )?loss of (?<retloss>\\d+) (yd|yard)s?|(?<retnogain>no gain))",
                         "|.*(?<touchback>touchback))?", sep='')
  
  if (grepl(kickoff_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match <- regex(kickoff_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    play$kickoff <- TRUE
    play$kicker <- format.name(match[1,'kicker'])
    play$returner <- ifelse(!is.na(match[1,'returner1']), format.name(match[1,'returner1']), format.name(match[1,'returner2']))
    play$kick_dist <- match[1,'kickdist']
    
    #Define the kicking team to have possession and remove kickoffs from drives
    #If first play of a drive is a kickoff, then flip possession.
    if (play$playnum == 1) {
      tmp <- play$poss
      play$poss <- play$def
      play$def <- tmp
    }
    
    if (!is.na(match[1,'touchback'])) {
      play$touchback <- TRUE
      play$kick_return <- 0
    } else play$touchback <- FALSE
    
    if (!is.na(match[1,'retgain'])) {play$kick_return <- as.numeric(match[1,'retgain'])}
    else if (!is.na(match[1,'retloss'])) {play$kick_return <- -as.numeric(match[1,'retloss'])}
    else if (!is.na(match[1,'retnogain'])) {play$kick_return <- 0}
  }
  
  return(play)
}

parse.interception <- function(play) {
  play$INT <- NA
  play$intercepter <- NA
  play$int_return <- NA
  
  interception_regex <- paste0("pass intercept(ed|ion)?( by)? (?<intercepter>", name.pattern, ")",
                               "( at (the )?(?<side>[a-zA-Z]+) (?<yardline>\\d{1,2}))?[\\.,]? return(ed)? for ",
                               "((?<retgain>\\d{1,3}) (yd|yard)s?|(a )?loss of (?<retloss>\\d+) ",
                               "(yd|yard)s?|(?<retnogain>no gain))")   
  
  interception_regex2 <- paste0("(?<intercepter>", name.pattern, ")( (?<retgain>\\d{1,3}) (yd|yard)s?)? interception return")   
  
  # test whether this play matches any off the pass patterns
  int.patterns <- c(interception_regex, interception_regex2)
  int.match <- sapply(int.patterns, function(p) grepl(pattern=p, x=play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))
  
  
  if (any(int.match)) {
    match <- regex(int.patterns[which(int.match)[1]], play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    play$pass <- TRUE
    play$INT <- TRUE
    play$complete <- FALSE
    play$intercepter <- format.name(match[1,'intercepter'])
    
    if (!is.na(match[1,'retgain'])) {play$int_return <- as.numeric(match[1,'retgain'])}
    else if (int.match[[2]] && !is.na(match[1,'retloss'])) {play$int_return <- -as.numeric(match[1,'retloss'])}
    else if (int.match[[2]] && !is.na(match[1,'retnogain'])) {play$int_return <- 0}
  }
  
  return(play)
}


parse.fumble <- function(play, meta) {
  play$fumble <- FALSE
  
  team.pattern <- paste(meta$home.school, meta$home.abbrev, meta$away.school, meta$away.abbrev, sep='|')
  
  fumble_regex = paste0("fumbled?.*(forced by (?<forcer>", name.pattern, "))?.*",
                        "(recovered by (?<team>", team.pattern, ") (?<recoverer>", name.pattern, "))?", sep='')
  
  if (grepl(fumble_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match <- regex(fumble_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    play$fumble <- TRUE
  }
  
  return(play)
}

parse.first.down = function(pbp, play) {
  play$first.down = FALSE
  
  first_regex = "(?<first>1st down|first down)"
  
  if (grepl(first_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    play$first.down = TRUE
  }
  
  return(play)    
}

parse.fg <- function(play) {
  play$FG <- FALSE
  
  fg_regex <- paste0("(?<kicker>", name.pattern, ") (?<kickdist>\\d{1,3}) (yd|yard)s? (field goal|FG) ",
                     "((?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD)).*")
  
  if (grepl(fg_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match <- regex(fg_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    play$FG <- TRUE
    play$kicker <- format.name(match[1,'kicker'])
    play$kick_dist <- match[1,'kickdist']
    if (!is.na(match[1,'made'])) {play$made <- TRUE}
    else {play$made <- FALSE}
  }
  
  return(play)        
}

format.name <- function(name) {
  if (is.na(name))
    return(NA)
  
  junior <- c('Jr', 'Jr.', 'JR', 'JR.', 'III')
  
  split <- strsplit(name, ',', fixed=TRUE)[[1]]
  split <- rev(trim(split))
  if (any(split %in% junior)) {
    indx <- split %in% junior
    split <- c(split[!indx], split[indx])
  }
  trim(paste(split, collapse=' '))
}

assign.team <- function(string, A, B) {
  team <- NA
  
  if (!is.na(string)) {
    teams <- c(A, B)
    costs <- list(insertions=2, deletions=0, substitutions=1)
    A.unlike <- adist(teams[1], string, costs=costs, ignore.case=TRUE)
    B.unlike <- adist(teams[2], string, costs=costs, ignore.case=TRUE)
    if (A.unlike != B.unlike) {
      team <- teams[which.min(c(A.unlike, B.unlike))]
    } else {team <- string}
  }
  
  return(team)
}

regex <- function(pattern, str, perl=TRUE, fixed=FALSE, ignore.case=TRUE) {
  #Process the regex
  match = gregexpr(pattern, str, perl=perl, fixed=fixed, ignore.case=ignore.case)[[1]]
  
  #Get the named capture groups
  capts = attr(match, 'capture.names')
  starts = attr(match, 'capture.start')
  lengths = attr(match, 'capture.length')
  
  #Remove unnamed captures:
  capts = capts[capts != ""]
  
  #Initialize the table of results
  result = matrix(NA, nrow=0, ncol=length(capts))
  
  #Produce a table of results where each row is a complete match
  for (j in 1:length(match)) {
    row = vector()
    
    #Loop through the possible capture groups and find those that matched.
    for (capt in capts) {
      start = starts[j,capt]
      length = lengths[j,capt]
      
      #Uncaptured groups are returned NA.
      if (length<=0) {row = c(row, NA)}
      else {
        #Remove leading and trailing whitespace:
        item = substr(str, start, start+length-1)
        item = gsub("[\\s\\.]*$","", item, perl=TRUE)
        item = gsub("^[\\s\\.]*","", item, perl=TRUE)
        row = c(row, item)
      }
    }
    
    #Add this match to the table
    result = rbind(result, row)
  }
  
  #Annotate the table and return it
  colnames(result) = capts
  return(result)
}

ExtractPlays <- function(url) {
  tree <- htmlTreeParse(url, isURL=TRUE, useInternalNodes=TRUE)
  
  #Make sure to label the scores properly:
  costs <- list(insertions=2, deletions=0, substitutions=1)
  
  img.id.regex <- "\\/(?<id>[^\\/]+)\\.png"
  meta <- list()
  
  # home team metadata
  meta$home.id <-home.id <- regex(img.id.regex, getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::img[contains(@class, 'team-logo')]/@src")[[1]])
  meta$home.school <-home.school <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'long-name')]")[[1]])
  meta$home.name <-home.name <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'short-name')]")[[1]])
  meta$home.abbrev <-home.abbrev <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'abbrev')]")[[1]])
  meta$home.score <-home.score <- as.integer(xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::div[contains(@class, 'score')]")[[1]]))
  
  # away team metadata
  meta$away.id <- away.id <- regex(img.id.regex, getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::img[contains(@class, 'team-logo')]/@src")[[1]])
  meta$away.school <- away.school <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'long-name')]")[[1]])
  meta$away.name <- away.name <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'short-name')]")[[1]])
  meta$away.abbrev <- away.abbrev <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'abbrev')]")[[1]])
  meta$away.score <- away.score <- as.integer(xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::div[contains(@class, 'score')]")[[1]]))
  
  team.patterns <- paste(meta$home.school, meta$home.abbrev, meta$away.school, meta$away.abbrev, sep='|')
  
  playmeta.regex = paste0("^(?<down>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH) (and|AND) ",
                          "(?<togo>\\d{1,2}|goal|Goal|GOAL) at (?<field>", team.patterns, ")? ?",
                          "(?<yardline>\\d{1,2})$")
  
  postplay.regex <- "^\\(((?<OT>OT)|(?<min>\\d{1,2}):(?<sec>\\d\\d)\\s*-\\s*(?<quarter>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH))\\)\\s(?<pbp>.*)\\s*$"
  
  
  
  # drive metadata
  ndrives <- getNodeSet(tree, "count(//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')])")
  halftime <- 1 + getNodeSet(tree, "count(//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item') and contains(@class, 'half-time')]/preceding-sibling::*)")
  if (halftime == 1) {
    indx.drives <- 1:ndrives
  } else indx.drives <- (1:ndrives)[-halftime]
  
  # scores after each drive. ESPN's presentation has the ID of home/away reversed in the score column.
  drive.score.away <- as.integer(xpathSApply(tree, "//article[contains(@class, 'play-by-play')]/descendant::span[contains(@class, 'home')]/span[contains(@class,'team-score')]", xmlValue))
  drive.score.home <- as.integer(xpathSApply(tree, "//article[contains(@class, 'play-by-play')]/descendant::span[contains(@class, 'away')]/span[contains(@class,'team-score')]", xmlValue))
  
  drives <- list()
  
  # for each drive extract the team, half, pbp code, duration
  for (i in indx.drives) {
    # identify the team with possession for this drive
    src <- getNodeSet(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/div[contains(@class, 'accordion-header')]/descendant::span[contains(@class, 'home-logo')]/img[contains(@class, 'team-logo')]/@src"))[[1]]
    poss <- regex(img.id.regex, src)
    if (poss[1] == away.id) { off <- away.abbrev; def <- home.abbrev; }
    if (poss[1] == home.id) { off <- home.abbrev; def <- away.abbrev; }
    
    
    # extract data for each play
    playdata <- trim(xpathSApply(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/descendant::ul[contains(@class, 'drive-list')]/li[not(contains(@class, 'half-time')) and not(contains(@class, 'end-quarter'))]/p/span[contains(@class, 'post-play')]"), xmlValue))
    down.dist <- trim(xpathSApply(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/descendant::ul[contains(@class, 'drive-list')]/li[not(contains(@class, 'half-time')) and not(contains(@class, 'end-quarter'))]/h3"), xmlValue))
    
    # get the gametime out of the "post-play" code
    plays1 <- as.data.frame(t(sapply(playdata, function(x) regex(postplay.regex, x))))
    plays2 <- as.data.frame(t(sapply(down.dist, function(x) regex(playmeta.regex, x))))
    
    # adjust the data objects
    colnames(plays1) <- c('OT', 'min', 'sec', 'quarter', 'pbp')
    rownames(plays1) <- NULL
    
    colnames(plays2) <- c('down', 'togo', 'field', 'yardline')
    rownames(plays2) <- NULL
    
    # adjust the types of each column
    plays <- as.data.frame(cbind(plays2, pbp=plays1$pbp))
    plays <- within(plays, {down <- as.character(down);
    field <- as.character(field);
    yardline <- as.integer(yardline);
    pbp <- as.character(pbp)})
    plays1 <- within(plays1, {min <- as.integer(min);
    sec <- as.integer(sec);
    quarter <- as.character(quarter)})
    
    #################################
    # How far from the end zone did the play begin?
    
    # Figure out which side of the field the play began from
    offense_unlike <- ifelse(!is.na(plays$field), adist(off, plays$field, costs=costs, ignore.case=TRUE), NA)
    defense_unlike <- ifelse(!is.na(plays$field), adist(def, plays$field, costs=costs, ignore.case=TRUE), NA)
    
    #Now compute the distance from the goal line:
    plays$dist <- ifelse(plays$yardline == 50, 50, ifelse(!is.na(plays$field), ifelse(offense_unlike < defense_unlike, 100 - plays$yardline, plays$yardline), NA))
    
    #Replace 'goal' to go with the distance to the goal line:
    indx <- !is.na(plays$togo) & (substr(plays$togo, 1, 1) == 'g' | substr(plays$togo,1,1) == 'G')
    plays$togo[indx] <- plays$dist[indx]
    plays$togo <- as.numeric(plays$togo)
    
    ##########################
    # convert all times to seconds
    plays1$quarter <- ifelse(plays1$quarter %in% c('1st', '1ST'), 1,
                             ifelse(plays1$quarter %in% c('2nd', '2ND'), 2,
                                    ifelse(plays1$quarter %in% c('3rd', '3RD'), 3,
                                           ifelse(plays1$quarter %in% c('4th', '4TH'), 4, NA))))
    plays$time <- 60 * (15 * (4 - plays1$quarter) + plays1$min) + plays1$sec
    
    
    this.drive <- list(plays=plays)
    
    ####################
    # metadata for the drive
    this.drive$half <- ifelse(i < halftime[1], 1, 2)
    if (i > 1) { drives[[length(drives)]]$duration <- drives[[length(drives)]]$plays$time[1] - plays$time[1] }
    
    # get the home and away scores during this drive
    if (i == 1) {
      this.drive$plays$score.home <- this.drive$home.score <- 0
      this.drive$plays$score.away <- this.drive$away.score <- 0
    } else {
      this.drive$plays$score.home <- this.drive$home.score <- drive.score.home[which(indx.drives == i) - 1]
      this.drive$plays$score.away <- this.drive$away.score <- drive.score.away[which(indx.drives == i) - 1]
    }
    
    # attach metadata to the play table
    this.drive$plays$half <- this.drive$half
    this.drive$plays$poss <- off
    this.drive$plays$def <- def
    this.drive$plays$home <- home.abbrev
    this.drive$plays$away <- away.abbrev
    if (poss[1] == away.id) { this.drive$plays$score.offense <- this.drive$away.score; this.drive$plays$score.defense <- this.drive$home.score; }
    if (poss[1] == home.id) { this.drive$plays$score.defense <- this.drive$away.score; this.drive$plays$score.offense <- this.drive$home.score; }
    this.drive$plays$playnum <- 1:nrow(plays)
    this.drive$plays$drive <- which(indx.drives == i)
    
    rownames(this.drive$plays) <- NULL
    
    # add this drive to the list
    drives[[length(drives) + 1]] <- this.drive
  }
  
  # final drive takes up the remaining time
  drives[[length(drives)]]$duration <- drives[[length(drives)]]$plays$time[1]
  
  
  list(drives=drives, meta=meta)
}
parse.down.dist <- function(url){
  
  tree <- htmlTreeParse(url, isURL=TRUE, useInternalNodes=TRUE)
  
  #Make sure to label the scores properly:
  costs <- list(insertions=2, deletions=0, substitutions=1)
  
  img.id.regex <- "\\/(?<id>[^\\/]+)\\.png"
  meta <- list()
  
  # home team metadata
  meta$home.id <-home.id <- regex(img.id.regex, getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::img[contains(@class, 'team-logo')]/@src")[[1]])
  meta$home.school <-home.school <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'long-name')]")[[1]])
  meta$home.name <-home.name <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'short-name')]")[[1]])
  meta$home.abbrev <-home.abbrev <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'abbrev')]")[[1]])
  meta$home.score <-home.score <- as.integer(xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::div[contains(@class, 'score')]")[[1]]))
  
  # away team metadata
  meta$away.id <- away.id <- regex(img.id.regex, getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::img[contains(@class, 'team-logo')]/@src")[[1]])
  meta$away.school <- away.school <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'long-name')]")[[1]])
  meta$away.name <- away.name <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'short-name')]")[[1]])
  meta$away.abbrev <- away.abbrev <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'abbrev')]")[[1]])
  meta$away.score <- away.score <- as.integer(xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::div[contains(@class, 'score')]")[[1]]))
  
  team.patterns <- paste(meta$home.school, meta$home.abbrev, meta$away.school, meta$away.abbrev, sep='|')
  
  playmeta.regex = paste0("^(?<down>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH) (and|AND) ",
                          "(?<togo>\\d{1,2}|goal|Goal|GOAL) at (?<field>", team.patterns, ")? ?",
                          "(?<yardline>\\d{1,2})$")
  
  postplay.regex <- "^\\(((?<OT>OT)|(?<min>\\d{1,2}):(?<sec>\\d\\d)\\s*-\\s*(?<quarter>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH))\\)\\s(?<pbp>.*)\\s*$"
  
  
  
  # drive metadata
  ndrives <- getNodeSet(tree, "count(//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')])")
  halftime <- 1 + getNodeSet(tree, "count(//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item') and contains(@class, 'half-time')]/preceding-sibling::*)")
  if (halftime == 1) {
    indx.drives <- 1:ndrives
  } else indx.drives <- (1:ndrives)[-halftime]
  
  # scores after each drive. ESPN's presentation has the ID of home/away reversed in the score column.
  drive.score.away <- as.integer(xpathSApply(tree, "//article[contains(@class, 'play-by-play')]/descendant::span[contains(@class, 'home')]/span[contains(@class,'team-score')]", xmlValue))
  drive.score.home <- as.integer(xpathSApply(tree, "//article[contains(@class, 'play-by-play')]/descendant::span[contains(@class, 'away')]/span[contains(@class,'team-score')]", xmlValue))
  
  drives <- list()
  
  down.dist2 <- c()
  # for each drive extract the team, half, pbp code, duration
  for (i in indx.drives) {
    # identify the team with possession for this drive
    src <- getNodeSet(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/div[contains(@class, 'accordion-header')]/descendant::span[contains(@class, 'home-logo')]/img[contains(@class, 'team-logo')]/@src"))[[1]]
    poss <- regex(img.id.regex, src)
    if (poss[1] == away.id) { off <- away.abbrev; def <- home.abbrev; }
    if (poss[1] == home.id) { off <- home.abbrev; def <- away.abbrev; }
    
    
    # extract data for each play
    playdata <- trim(xpathSApply(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/descendant::ul[contains(@class, 'drive-list')]/li[not(contains(@class, 'half-time')) and not(contains(@class, 'end-quarter'))]/p/span[contains(@class, 'post-play')]"), xmlValue))
    down.dist <- trim(xpathSApply(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/descendant::ul[contains(@class, 'drive-list')]/li[not(contains(@class, 'half-time')) and not(contains(@class, 'end-quarter'))]/h3"), xmlValue))
    down.dist2 <- c(down.dist2,down.dist)
    
  }
  
  return(down.dist2)
  
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

parse.url <- function(url) {
  SaF <- options('stringsAsFactors')$stringsAsFactors
  options(stringsAsFactors = FALSE)
  
  data <- ExtractPlays(url)
  down.dist2 <- parse.down.dist(url)
  meta <- data$meta
  
  plays <- do.call('rbind', sapply(data$drives, function(x) x$plays, simplify=FALSE))
  # scores = pbp$scores
  # teams = colnames(pbp$scores)
  
  play_table = data.frame()
  raw_pbp = vector()
  
  for (k in 1:nrow(plays)) {
    #Get the already-established metadata:
    play <- plays[k,]
    play$carrier <- NA
    play$gain <- NA
    
    pbp <- play$pbp
    
    play <- parse.rush(pbp, play)
    play <- parse.pass(pbp, play)
    play <- parse.timeout(pbp, play)
    
    
    
    #Fumbles, penalties, touchdowns, first downs can happen on any play:
    play <- parse.sack(pbp, play)
    play <- parse.penalty(play, meta)
    play <- parse.fumble(play, meta)
    play <- parse.touchdown(play)
    play <- parse.special(play)
    play <- parse.first.down(pbp, play)
    play <- parse.interception(play)
    
    #Put scores in the table
    play$margin <- play$score.offense - play$score.defense
    
    play_table <- rbind(play_table, play)
  }
  
  #Sacks should count against passing, not rushing. Consider anyone who threw at least two passes a QB:
  QBs = vector()
  for (qb in unique(play_table$passer)) {
    if (!is.na(qb) && sum(play_table$passer==qb, na.rm=TRUE)) {
      QBs = c(QBs, qb)
    }
  }
  
  #Now any non-positive rush for a QB should be a sack.
  sack_indx <- (play_table$rush & play_table$gain<=0 & play_table$carrier %in% QBs)
  play_table$rush[sack_indx] <- FALSE
  play_table$pass[sack_indx] <- TRUE
  play_table$complete[sack_indx] <- FALSE
  play_table$sack[sack_indx] <- TRUE
  play_table$passer[sack_indx] <- play_table$carrier[sack_indx]
  
  #Remove kickoffs, penalties, and other non-scrimmage plays from play numbering.
  scrimmage <- (play_table$rush | play_table$pass | play_table$FG | play_table$punt)
  PAT_play <- (play_table$PAT & !play_table$TD)
  play_table$playnum[!scrimmage] <- NA
  play_table$playnum[PAT_play] <- NA
  
  #Also make kickoffs the first play of the ensuing drive.
  koind = which(play_table$kickoff)
  for (l in rev(koind)) {
    if (l != nrow(play_table) && !is.na(play_table$drive[l+1]))
      play_table$drive[l] = play_table$drive[l+1]
  }
  
  #Correct play numbering:
  for (l in unique(play_table$drive[!is.na(play_table$drive)])) {
    indx = (play_table$drive==l & !is.na(play_table$playnum))
    play_table$playnum[indx] = 1:sum(indx)
  }
  
  # If penalty yards weren't acquired form the pbp code, impute them:
  indx <- play_table$penalty &
    is.na(play_table$penalty_dist) & 
    is.na(play_table$gain) & 
    play_table$poss == play_table$poss[c(2:nrow(play_table), nrow(play_table))] &
    play_table$half == play_table$half[c(2:nrow(play_table), nrow(play_table))]
  indx <- head(indx, length(indx) - 1) # don't bother with the final play of the game
  play_table$penalty_dist[indx] <- abs(play_table$dist[c(2:nrow(play_table), nrow(play_table))] - play_table$dist)[indx]
  
  down <- c()
  togo <- c()
  field <- c()
  yrdline <- c()
  for(i in 1:nrow(play_table)){
    
    atl <- as.numeric(gregexpr("at ",down.dist2[i]))
    team.yrdline <- substring(down.dist2[i],atl+3)
    spacel <- as.numeric(gregexpr(" ",team.yrdline))
    yrdlineloop <- as.numeric(substring(team.yrdline,spacel+1))
    team.yrd <- substring(team.yrdline,1,spacel-1)
    downloop <- as.numeric(substring(down.dist2[i],1,1))
    ampl <- as.numeric(gregexpr("&",down.dist2[i]))
    distance <- as.numeric(substr(down.dist2[i],ampl+2,8))
    
    down <- c(down,downloop)
    togo <- c(togo,distance)
    field <- c(field,team.yrd)
    yrdline <- c(yrdline,yrdlineloop)
    
  }
  
  play_table$down <- down
  play_table$togo <- togo
  play_table$field <- field
  play_table$yardline <- yrdline
  
  return(play_table)
}

name.pattern <- "(\\d{1,2}-)?[-a-zA-Z,\\. ']+"

substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}

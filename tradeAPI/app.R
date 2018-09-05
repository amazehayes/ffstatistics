library(DT)
library(shiny)
library(httr)
library(jsonlite)
library(Hmisc)

options(stringsAsFactors = FALSE)

# PLAYER IDs API CALL 
player_url_pref <- 'https://www71.myfantasyleague.com/2018/export?TYPE=players&DETAILS=0&SINCE=&PLAYERS=&JSON=1'
r <- GET(player_url_pref)
player_content <- content(r)
player_data <- player_content$players$player
name_vec <- c('status', 'position', 'name', 'id', 'team')
player_data <- lapply(player_data, 
                      function(x) {
                        if('status' %nin% names(x)) {
                          x[['status']] <- 'NR'
                        } 
                        return(x[name_vec])
                      })

player_data_df <- data.frame(do.call(rbind, player_data))
char_names <- c(names(player_data_df))
for(i in char_names){
  player_data_df[, i] <-
    as.character(player_data_df[, i])
}
player_data_df <- player_data_df[385:2478,]

player_ids <- player_data_df$id
player_names <- player_data_df$name





ui <- fluidPage(
   
  titlePanel("MFL League Trade Logs"),
  br(),
  br(),
  
  fluidRow(
      column(4,textInput("leagueID", "Input 5-digit League ID", value = "54549"))
    ),
  fluidRow(DT::dataTableOutput("trade_log"))
   
)


server <- function(input, output, session) {
   
  output$trade_log <- renderDataTable({
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    
    # LEAGUE API CALL - GET YEARS ACTIVE AND FRANCHISE IDs
    leagueID <- as.numeric(input$leagueID)
    leagues_url <- paste0('http://www77.myfantasyleague.com/2018/export?TYPE=league&', 'L=', leagueID, '&APIKEY=&JSON=1')
    
    r <- GET(leagues_url)
    leagues_content <- content(r)
    leagues <- leagues_content$league$franchises$franchise
    
    name_vec <- c('logo', 'icon', 'bbidAvailableBalance', 'name', 'id', 'waiverSortOrder')
    
    leagues <- lapply(leagues, 
                      function(x) {
                        if('logo' %nin% names(x)) {
                          x[['logo']] <- 'NA'
                        } 
                        return(x[name_vec])
                      })
    leagues <- lapply(leagues, 
                      function(x) {
                        if('icon' %nin% names(x)) {
                          x[['icon']] <- 'NA'
                        } 
                        return(x[name_vec])
                      })
    
    leagues_df <- data.frame(do.call(rbind, leagues))
    
    history <- leagues_content$league$history$league
    history_df <- data.frame(do.call(rbind, history))
    
    char_names_leagues <- c(names(leagues_df))
    char_names_history <- c(names(history_df))
    
    for(i in char_names_leagues){
      leagues_df[, i] <-
        as.character(leagues_df[, i])
    }
    for(i in char_names_history){
      history_df[, i] <-
        as.character(history_df[, i])
    }
    
    years <- history_df$year
    no_years <- as.numeric(length(years))
    
    franchise_id <- leagues_df$id
    franchise_names <- leagues_df$name
    
    
    
    # TRADES LOG API CALL USING ABOVE INFO
    trans_url <- NULL
    for(i in 1:no_years) {
      x <- paste0('http://www77.myfantasyleague.com/',years[i],'/export?TYPE=transactions&L=',leagueID,'&APIKEY=&W=&TRANS_TYPE=TRADE&FRANCHISE=&DAYS=400&COUNT=&JSON=1')
      trans_url <- rbind(trans_url, x)
    }
    
    trans_final <- NULL 
    for(i in 1:length(trans_url)){
      r <- GET(trans_url[i])
      
      trans_content <- content(r)
      
      transactions <- trans_content$transactions$transaction
      
      transactions_df <- data.frame(do.call(rbind, transactions))
      
      char_names <- c(names(transactions_df))
      
      for(i in char_names){
        transactions_df[, i] <-
          as.character(transactions_df[, i])
      }
      trans_final <- rbind(trans_final, transactions_df)
    }
    
    for(j in 1:nrow(trans_final)){
      for(i in 1:length(franchise_id)){
        trans_final[j,] <- gsub(franchise_id[i], franchise_names[i], trans_final[j,], fixed = TRUE)
      }
    }
    trans_final <- data.frame(lapply(trans_final, function(x) {
      gsub("FP_", "", x)
    }))
    trans_final <- data.frame(lapply(trans_final, function(x) {
      gsub(",", ";", x)
    }))
    
    trans_final$timestamp <- as.POSIXct(as.numeric(as.character(trans_final$timestamp)),origin="1970-01-01")
    
    for(j in 1:nrow(trans_final)){
      for(k in 0:10){
        for(i in 0:16){
          trans_final[j,1] <- gsub(paste0("DP_",k,"_",i),paste0(k+1,".",i+1), trans_final[j,1], fixed = TRUE)
        }
      }
    }
    for(j in 1:nrow(trans_final)){
      for(k in 0:10){
        for(i in 0:16){
          trans_final[j,4] <- gsub(paste0("DP_",k,"_",i),paste0(k+1,".",i+1), trans_final[j,4], fixed = TRUE)
        }
      }
    }
    
    for(j in 1:nrow(trans_final)){
      for(i in 1:length(player_ids)){
        trans_final[j,1] <- gsub(player_ids[i], player_names[i], trans_final[j,1], fixed = TRUE)
      }
    }
    for(j in 1:nrow(trans_final)){
      for(i in 1:length(player_ids)){
        trans_final[j,4] <- gsub(player_ids[i], player_names[i], trans_final[j,4], fixed = TRUE)
      }
    }
    
    trans_finalA <- trans_final[,c(5,4,2,1,3)]
    trans_finalB <- trans_final[,c(2,1,5,4,3)]
    colnames(trans_finalA) <- c("Franchise1","Franchise1 Gave Up","Franchise2","Franchise2 Gave Up","Date")
    colnames(trans_finalB) <- c("Franchise1","Franchise1 Gave Up","Franchise2","Franchise2 Gave Up","Date")
    
    trans_final2 <- rbind(trans_finalA,trans_finalB)
    
  }, rownames = FALSE, filter = "top", options = list(lengthMenu = c(25,50,100)))
  
}

# Run the application 
shinyApp(ui = ui, server = server)


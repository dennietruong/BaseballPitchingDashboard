library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(lubridate)

# -------------------------
# LOAD DATA
# -------------------------
PitchData2025 <- read.csv("PitchingData2025.csv", stringsAsFactors = FALSE)
PitchData2026 <- read.csv("PitchingData2026.csv", stringsAsFactors = FALSE)

PitchData2025$Year <- 2025
PitchData2026$Year <- 2026

PitchData <- bind_rows(PitchData2025, PitchData2026)
PitchData$Date <- as.Date(PitchData$Date, format = "%m/%d/%y")

# -------------------------
# CONVERT INNINGS
# -------------------------
convert_inning <- function(x) {
  sapply(x, function(inn_str) {
    parts <- strsplit(as.character(inn_str), "\\.")[[1]]
    whole <- as.numeric(parts[1])
    outs <- ifelse(length(parts) > 1, as.numeric(parts[2]), 0)
    
    if (is.na(whole)) return(NA_real_)
    if (!outs %in% 0:2) stop(paste("Invalid inning format:", inn_str))
    
    whole + outs / 3
  })
}

PitchData$ConvertedInnings <- round(convert_inning(PitchData$InningPitched), 2)

# -------------------------
# STATS FUNCTIONS
# -------------------------
CalcERA <- function(data, player_col, player_name) {
  df <- data[data[[player_col]] == player_name, ]
  
  ip <- sum(df$ConvertedInnings, na.rm = TRUE)
  er <- sum(df$RunScored, na.rm = TRUE)
  
  if (ip == 0) return(NA_real_)
  
  round((er / ip) * 3, 2)
}

CalcWHIP <- function(data, player_col, player_name) {
  df <- data[data[[player_col]] == player_name, ]
  
  ip <- sum(df$ConvertedInnings, na.rm = TRUE)
  bb <- sum(df$BB, na.rm = TRUE)
  h  <- sum(df$Hit, na.rm = TRUE)
  
  if (ip == 0) return(NA_real_)
  
  round((bb + h) / ip, 2)
}

# -------------------------
# ERA COLOR STYLE
# -------------------------
era_style <- function(dt) {
  formatStyle(
    dt,
    "ERA",
    backgroundColor = styleInterval(
      c(2, 4),
      c("#c7f5c4", "#ffe7a3", "#f5b5b5")
    )
  )
}

# -------------------------
# PIE FUNCTION (PLAYER TAB)
# -------------------------
PieChart <- function(df) {
  
  if (nrow(df) == 0) return(NULL)
  
  par(mfrow = c(1,3), mar = c(4,4,4,2))
  
  # Pitch
  balls <- sum(df$Ball, na.rm = TRUE)
  strikes <- sum(df$Strike, na.rm = TRUE)
  total1 <- max(balls + strikes, 1)
  
  pie(c(balls, strikes),
      labels = paste0(c("Ball","Strike"), " ",
                      round(c(balls, strikes)/total1*100), "%"),
      col = c("skyblue","tomato"),
      main = "Pitch Profile")
  
  # Out
  go <- sum(df$GO, na.rm = TRUE)
  fo <- sum(df$FO, na.rm = TRUE)
  so <- sum(df$SO, na.rm = TRUE)
  total2 <- max(go + fo + so, 1)
  
  pie(c(go, fo, so),
      labels = paste0(c("GO","FO","SO"), " ",
                      round(c(go, fo, so)/total2*100), "%"),
      col = c("lightgreen","green","forestgreen"),
      main = "Out Profile")
  
  # Result
  bb <- sum(df$BB, na.rm = TRUE)
  hit <- sum(df$Hit, na.rm = TRUE)
  total3 <- max(go + fo + so + bb + hit, 1)
  
  pie(c(go, fo, so, bb, hit),
      labels = paste0(c("GO","FO","SO","BB","Hit"), " ",
                      round(c(go, fo, so, bb, hit)/total3*100), "%"),
      col = c("lightgreen","green","forestgreen","firebrick","tomato"),
      main = "Result Profile")
}

# -------------------------
# UI
# -------------------------
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("Pitching Dashboard"),
  
  tabsetPanel(
    
    # -------------------------
    tabPanel("Pitching Stats",
             selectInput("year_stats", "Select Year:",
                         choices = sort(unique(PitchData$Year))),
             DTOutput("table")
    ),
    
    # -------------------------
    tabPanel("Player Analysis",
             
             selectInput("year_player", "Select Year:",
                         choices = sort(unique(PitchData$Year))),
             
             selectInput("player_select", "Select Player:", choices = NULL),
             
             h3(textOutput("player_title")),
             
             DTOutput("player_table"),
             
             h4("Team Averages"),
             DTOutput("team_avg_table"),
             
             h4("Pitch Trend"),
             plotOutput("pitch_trend"),
             
             h4("Pitch Profiles"),
             plotOutput("pie_all")
    ),
    
    # -------------------------
    tabPanel("Game Breakdown",
             
             selectInput("year_game", "Select Year:",
                         choices = sort(unique(PitchData$Year))),
             
             selectInput("player_game", "Select Player:", choices = NULL),
             
             selectInput("game_date", "Select Game:", choices = NULL),
             
             h3("Game Stats"),
             DTOutput("game_table"),
             
             h4("Player Season Averages"),
             DTOutput("player_avg_table"),
             
             h4("Game Pitch & Result Profiles"),
             
             fluidRow(
               column(6, plotOutput("game_pitch_pie")),
               column(6, plotOutput("game_result_pie"))
             )
    ),
    
    # -------------------------
    tabPanel("Glossary",
             tags$ul(
               tags$li("IP: Innings Pitched"),
               tags$li("ERA: Earned Run Average (per 3 innings)"),
               tags$li("WHIP: Walks + Hits per Inning"),
               tags$li("GO: Groundout"),
               tags$li("FO: Flyout"),
               tags$li("SO: Strikeout"),
               tags$li("BB: Walk"),
               tags$li("Hit: Hits allowed")
             )
    )
  )
)

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  
  # Players (stats)
  observe({
    req(input$year_player)
    updateSelectInput(session, "player_select",
                      choices = unique(PitchData$Name[PitchData$Year == input$year_player]))
  })
  
  # Players (game)
  observe({
    req(input$year_game)
    updateSelectInput(session, "player_game",
                      choices = unique(PitchData$Name[PitchData$Year == input$year_game]))
  })
  
  # Dates
  observe({
    req(input$year_game, input$player_game)
    
    dates <- PitchData %>%
      filter(Year == input$year_game,
             Name == input$player_game) %>%
      arrange(Date) %>%
      pull(Date)
    
    updateSelectInput(session, "game_date",
                      choices = unique(dates))
  })
  
  # -------------------------
  # MAIN TABLE
  # -------------------------
  output$table <- renderDT({
    
    df <- PitchData %>% filter(Year == input$year_stats)
    
    res <- bind_rows(lapply(unique(df$Name), function(p) {
      pd <- df[df$Name == p, ]
      
      data.frame(
        Name = p,
        IP = round(sum(pd$ConvertedInnings), 2),
        Hits = sum(pd$Hit),
        Runs = sum(pd$RunScored),
        BB = sum(pd$BB),
        SO = sum(pd$SO),
        HR = sum(pd$HR),
        ERA = CalcERA(df, "Name", p),
        WHIP = CalcWHIP(df, "Name", p)
      )
    }))
    
    era_style(datatable(res))
  })
  
  # -------------------------
  # PLAYER TABLE
  # -------------------------
  output$player_table <- renderDT({
    
    req(input$player_select)
    
    df <- PitchData %>%
      filter(Year == input$year_player,
             Name == input$player_select)
    
    datatable(data.frame(
      Name = input$player_select,
      IP = round(sum(df$ConvertedInnings), 2),
      Hits = sum(df$Hit),
      Runs = sum(df$RunScored),
      BB = sum(df$BB),
      SO = sum(df$SO),
      HR = sum(df$HR),
      ERA = CalcERA(df, "Name", input$player_select),
      WHIP = CalcWHIP(df, "Name", input$player_select)
    ), options = list(dom = "t"))
  })
  
  # -------------------------
  # TEAM AVG
  # -------------------------
  output$team_avg_table <- renderDT({
    
    df <- PitchData %>% filter(Year == input$year_player)
    
    datatable(data.frame(
      Name = "Team Avg",
      IP = round(mean(df$ConvertedInnings), 2),
      Hits = round(mean(df$Hit), 2),
      Runs = round(mean(df$RunScored), 2),
      BB = round(mean(df$BB), 2),
      SO = round(mean(df$SO), 2),
      HR = round(mean(df$HR), 2),
      ERA = round(mean(df$RunScored), 2),
      WHIP = round(mean(df$BB + df$Hit), 2)
    ), options = list(dom = "t"))
  })
  
  # -------------------------
  # PITCH TREND
  # -------------------------
  output$pitch_trend <- renderPlot({
    
    req(input$player_select)
    
    df <- PitchData %>%
      filter(Year == input$year_player,
             Name == input$player_select) %>%
      arrange(Date)
    
    df$Game <- seq_len(nrow(df))
    
    total <- df$Ball + df$Strike
    total[total == 0] <- NA
    
    ball <- (df$Ball / total) * 100
    strike <- (df$Strike / total) * 100
    
    ball[is.na(ball)] <- 0
    strike[is.na(strike)] <- 0
    
    plot(df$Game, ball, type = "b", col = "tomato",
         ylim = c(0,100), pch = 16)
    
    lines(df$Game, strike, type = "b", col = "steelblue", pch = 16)
    
    legend("topright",
           legend = c("Ball %", "Strike %"),
           col = c("tomato","steelblue"),
           pch = 16, lty = 1)
  })
  
  # -------------------------
  # PLAYER PIES
  # -------------------------
  output$pie_all <- renderPlot({
    req(input$player_select)
    
    df <- PitchData %>%
      filter(Year == input$year_player,
             Name == input$player_select)
    
    PieChart(df)
  })
  
  # -------------------------
  # GAME TABLE
  # -------------------------
  output$game_table <- renderDT({
    
    req(input$player_game, input$game_date)
    
    df <- PitchData %>%
      filter(Year == input$year_game,
             Name == input$player_game,
             Date == input$game_date)
    
    datatable(data.frame(
      Name = input$player_game,
      Date = as.character(input$game_date),
      IP = round(sum(df$ConvertedInnings), 2),
      Hits = sum(df$Hit),
      Runs = sum(df$RunScored),
      BB = sum(df$BB),
      SO = sum(df$SO),
      HR = sum(df$HR),
      ERA = CalcERA(df, "Name", input$player_game),
      WHIP = CalcWHIP(df, "Name", input$player_game)
    ))
  })
  
  # -------------------------
  # PLAYER AVG (GAME TAB)
  # -------------------------
  output$player_avg_table <- renderDT({
    
    req(input$player_game)
    
    df <- PitchData %>%
      filter(Year == input$year_game,
             Name == input$player_game)
    
    datatable(data.frame(
      Name = input$player_game,
      Games = nrow(df),
      IP = round(mean(df$ConvertedInnings), 2),
      Hits = round(mean(df$Hit), 2),
      Runs = round(mean(df$RunScored), 2),
      BB = round(mean(df$BB), 2),
      SO = round(mean(df$SO), 2),
      HR = round(mean(df$HR), 2),
      ERA = CalcERA(df, "Name", input$player_game),
      WHIP = CalcWHIP(df, "Name", input$player_game)
    ), options = list(dom = "t"))
  })
  
  # -------------------------
  # GAME PIE CHARTS (FIXED + GUARANTEED)
  # -------------------------
  output$game_pitch_pie <- renderPlot({
    
    req(input$player_game, input$game_date)
    
    df <- PitchData %>%
      filter(Year == input$year_game,
             Name == input$player_game,
             Date == input$game_date)
    
    validate(need(nrow(df) > 0, "No data"))
    
    balls <- sum(df$Ball)
    strikes <- sum(df$Strike)
    total <- max(balls + strikes, 1)
    
    pie(c(balls, strikes),
        labels = paste0(c("Ball","Strike"), " ",
                        round(c(balls, strikes)/total*100), "%"),
        col = c("skyblue","tomato"),
        main = "Game Pitch Profile")
  })
  
  output$game_result_pie <- renderPlot({
    
    req(input$player_game, input$game_date)
    
    df <- PitchData %>%
      filter(Year == input$year_game,
             Name == input$player_game,
             Date == input$game_date)
    
    validate(need(nrow(df) > 0, "No data"))
    
    vals <- c(
      sum(df$GO),
      sum(df$FO),
      sum(df$SO),
      sum(df$BB),
      sum(df$Hit)
    )
    
    total <- max(sum(vals), 1)
    
    pie(vals,
        labels = paste0(c("GO","FO","SO","BB","Hit"), " ",
                        round(vals/total*100), "%"),
        col = c("lightgreen","green","forestgreen","firebrick","tomato"),
        main = "Game Result Profile")
  })
  
}

shinyApp(ui, server)
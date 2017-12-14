

library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(ggrepel)
theme_set(theme_classic())

##########################
#### IP Dot Plot Data ####
##########################

pitching <- read.csv("Pitching.csv")

# Add column for innings pitched
pitching["IP"] <- ""
pitching$IP <- (pitching$IPouts/3)

# Create list of names to reference for creation of dataframe
IPpitchernames <- c("kershcl01", "priceda01", "hernafe02", "salech01", "porceri01", "lestejo01", "tanakma01", "bumgama01", "syndeno01", "sabatcc01")
IPpitchingfavorites <- pitching %>% filter(playerID %in% IPpitchernames) %>% arrange(desc(playerID))
head(IPpitchingfavorites)

# Modify playerID to reflect players' actual name
IPpitchingfavorites$playerID <- gsub('kershcl01', 'Clayton Kershaw', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('priceda01', 'David Price', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('hernafe02', 'Felix Hernandez', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('salech01', 'Chris Sale', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('porceri01', 'Rick Porcello', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('lestejo01', 'John Lester', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('tanakma01', 'Masahiro Tanaka', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('bumgama01', 'Madison Bumgarner', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('syndeno01', 'Noah Syndergaard', IPpitchingfavorites$playerID)
IPpitchingfavorites$playerID <- gsub('sabatcc01', 'CC Sabathia', IPpitchingfavorites$playerID)

# Aggregate IP over career into a per season average
avg_IP <- IPpitchingfavorites %>% 
  group_by(playerID) %>%
  summarize(IP = mean(IP)) %>%
  ungroup

#####################
#### IP Dot Plot ####
#####################

g1 <- ggplot(IPpitchingfavorites, aes(x = playerID, y = IP)) + 
  theme(axis.text.x = element_text(angle=45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14)) +
  #geom_point(data=pitch.avg, col="blue", size = 4) +
  geom_point(col = "tomato2", size = 2) +
  geom_segment(aes(x = as.factor(playerID),
                   xend = as.factor(playerID),
                   y = min(IP),
                   yend = max(IP)),
               linetype= "dashed",
               size = 0.1) + 
  labs(title = "Dot Plot",
       subtitle = "Average Innings Pitched Per Season",
       caption = "Source: Lahman's Baseball Database",
       x = "Player",
       y = "Avg IP / Season") 

print(g1)

# Dot plot of average innings pitched per season, over course of career, superimposed over g1
g2 <- g1 + geom_point(data = avg_IP, col="blue", size = 5)

print(g2)


#############################
#### ERA Comparison Data ####
#############################

erapitchernames <- c("kershcl01", "priceda01", "hernafe02", "salech01", "porceri01", "lestejo01", "tanakma01", "bumgama01", "syndeno01", "sabatcc01")
erapitchingfavorites <- pitching %>% filter(playerID %in% erapitchernames) %>% arrange(desc(playerID))
head(erapitchingfavorites)

eracomp <- (erapitchingfavorites[,c("playerID", "yearID", "ERA")])

era_2011 <- 
  eracomp %>% filter(yearID == "2011")

# Delete year column
era_2011$yearID <- NULL

era_2016 <- 
  eracomp %>% filter(yearID == "2016")

# Delete year column
era_2016$yearID <- NULL


# Rename columns
colnames(era_2011) <- c("name", "era_2011")
era_2011

colnames(era_2016) <- c("name", "era_2016")
era_2016

# Remove players that didn't record an ERA in 2011 (Tanaka, Syndergaard)
era_2016 <- era_2016[-c(1, 2), ]
era_2016

# Cbind 2016 ERA column into the 2011 ERA data.
era_comparison <- cbind(era_2011, era_2016$era_2016)

colnames(era_comparison) <- c("name", "era_2011", "era_2016")

# Prep Data
df <- era_comparison

# If Season ERA goes down over time, green line. Else: red
df$color <- ifelse((df$era_2011 - df$era_2016) < 0, "Red", "Green")

# Modify playerID to reflect players' actual name
df$name <- gsub('kershcl01', 'Clayton Kershaw', df$name)
df$name <- gsub('priceda01', 'David Price', df$name)
df$name <- gsub('hernafe02', 'Felix Hernandez', df$name)
df$name <- gsub('salech01', 'Chris Sale', df$name)
df$name <- gsub('porceri01', 'Rick Porcello', df$name)
df$name <- gsub('lestejo01', 'John Lester', df$name)
df$name <- gsub('bumgama01', 'Madison Bumgarner', df$name)
df$name <- gsub('sabatcc01', 'CC Sabathia', df$name)

#############################
#### ERA Comparison Plot ####
#############################

# If Season ERA goes down over time, green line. Else: red
df$color <- ifelse((df$era_2011 - df$era_2016) < 0, "Red", "Green")

p <- ggplot(df) + 
  geom_segment(aes(x=rep(1,8), 
                   xend=rep(2,8),
                   y=era_2011,
                   yend= era_2016, col=color), 
               size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("Green"="#00ba38", "Red"="#f8766d")) +  # color of lines
  labs(title = "Season ERA Comparison, 2016 vs. 2011", x="", y="ERA") +  # Axis labels
  xlim(.5, 2.5) + ylim(1, 5) +
  theme(title = element_text(size = 16))

print(p)

# Prep labels
left_label <- paste(df$name, (df$era_2011),sep=", ")
right_label <- paste(df$name, (df$era_2016),sep=", ")

# Add texts / labels
p <- p + geom_text_repel(label=left_label, y=df$era_2011, x=rep(1, 8), hjust=1.1, size=3.5)
p <- p + geom_text_repel(label=right_label, y=df$era_2016, x=rep(2, 8), hjust=-0.1, size=3.5)
p <- p + geom_text(label="2011", x=1, y=5, hjust=1.2, size=5)  # title
p <- p + geom_text(label="2016", x=2, y=5, hjust=-0.1, size=5)  # title

print(p)

##########################
#### BOS Batting Data ####
##########################

batting <- read.csv("Batting.csv")
playernames <- c("ortizda01", "pedrodu01", "varitja01", "ramirma02", "damonjo01", "bettsmo01")
redsoxfavorites <- batting %>% filter(playerID %in% playernames) %>% arrange(desc(playerID))

# Rename playerID column to reflect players' actual name
redsoxfavorites$playerID <- gsub('ortizda01', 'David Ortiz', redsoxfavorites$playerID)
redsoxfavorites$playerID <- gsub('pedrodu01', 'Dustin Pedroia', redsoxfavorites$playerID)
redsoxfavorites$playerID <- gsub('varitja01', 'Jason Varitek', redsoxfavorites$playerID)
redsoxfavorites$playerID <- gsub('ramirma02', 'Manny Ramirez', redsoxfavorites$playerID)
redsoxfavorites$playerID <- gsub('damonjo01', 'Johnny Damon', redsoxfavorites$playerID)
redsoxfavorites$playerID <- gsub('bettsmo01', 'Mookie Betts', redsoxfavorites$playerID)


############################
#### BOS Hits Bar Chart ####
############################

f <- ggplot(redsoxfavorites, aes(x = playerID, y= H)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Career Hits", 
       caption="source: MLB Batting Data",
       x = "Player",
       y = "Hits") + 
  theme(axis.text.x = element_text(angle=45, vjust=0.6, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14))

print(f)


##########################
#### Steroid Era Data ####
##########################

roidnames <- c("palmera01", "mcgwima01", "sosasa01", "bondsba01", "rodrial01", "giambja01")
roidusers <- batting %>% filter(playerID %in% roidnames) %>% arrange(desc(playerID))
roidusers$yearID <- as.factor(roidusers$yearID)

data1 <- roidusers %>% 
  spread(playerID, HR) %>% 
  select(yearID, palmera01, mcgwima01, sosasa01, bondsba01, rodrial01, giambja01)

data2 <-  aggregate(. ~ yearID, data1, sum, na.rm = TRUE, na.action = "na.pass")

colnames(data2) <- c("Year", "Rafael_Palmeiro", "Mark_McGwire", "Sammy_Sosa", "Barry_Bonds", "Alex_Rodriguez", "Jason_Giambi")

# Replace zeroes with NAs to make graph cleaner
data2[data2 == 0] <- NA

# Generate 6 colors to use in plot
a <- palette(rainbow(6))


##########################
#### Steroid Era Plot ####
##########################

g <- ggplot(data2, aes(x=Year)) + 
  geom_line(aes(y=Rafael_Palmeiro, group = 1, col = "Rafael Palmeiro")) + 
  geom_line(aes(y=Mark_McGwire, group = 1, col = "Mark McGwire")) + 
  geom_line(aes(y=Sammy_Sosa, group = 1, col = "Sammy Sosa")) +
  geom_line(aes(y=Barry_Bonds, group = 1, col = "Barry Bonds")) +
  geom_line(aes(y=Alex_Rodriguez, group = 1, col = "Alex Rodriguez")) +
  geom_line(aes(y=Jason_Giambi, group = 1, col = "Jason Giambi")) +
  labs(title="Time Series of HRs hit", 
       caption="Source: Lahman's Baseball Database", y="HRs") +
  scale_color_manual(name="", 
                     values = c("Rafael Palmeiro" = "red", "Mark McGwire" = "orange", "Sammy Sosa" = "green", "Barry Bonds" = "cyan", "Alex Rodriguez" = "blue", "Jason Giambi" = "magenta")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

print(g)

#########################
#### WS Winners Maps ####
#########################

library(devtools)
library(ggmap)
library(ggalt)

# Get the east coast WS winning cities' coordinates (past 25 years)
# al_east_ws_winning_cities <- c("New York", "Boston", "Oak Lawn", "Kansas City", "Houston", "Toronto")
# nl_east_ws_winning_cities <- c("Miami", "St. Louis", "Philidelphia", "Evanston")
# al_east_city_loc <- geocode(al_east_ws_winning_cities)
# nl_east_city_loc <- geocode(nl_east_ws_winning_cities)

# Offline version of coordinates (Google's Map API only allows a certain number of queries in a day)
# Eastern US AL Winners
al_east_offline <- read.csv("AL East Offline Data.csv")
nl_east_offline <- read.csv("NL East Offline Data.csv")

# Western US AL Winners
al_west_offline <- read.csv("AL West Offline Data.csv")
nl_west_offline <- read.csv("NL West Offline Data.csv")

# Combine into one dataset
westcombinedleagues <- rbind(al_west_offline, nl_west_offline)
eastcombinedleagues <- rbind(al_east_offline, nl_east_offline)

# Get the Maps
# Google Terrain Map, eastern half of US

us_eastern_google_road_map <- qmap("knoxville", zoom = 5, source = "google", maptype = "terrain")

# Google Terrain Map, western half of US
us_western_google_road_map <- qmap("Price", zoom = 5, source = "google", maptype = "terrain")

# Determine size of point based on how many WS wins team has
point_size_west <- ifelse(westcombinedleagues$Team == "SF", 11, 5)
point_size_east <- ifelse(eastcombinedleagues$Team == 'NYY', 14,
                          (ifelse(eastcombinedleagues$Team == 'BOS',11,
                                  (ifelse(eastcombinedleagues$Team == 'TOR', 8,
                                          (ifelse(eastcombinedleagues$Team == 'STL',8,
                                                  (ifelse(eastcombinedleagues$Team == 'PHI',8,
                                                          (ifelse(eastcombinedleagues$Team == 'MIA',8,5)))))))))))

# Determine color of point based on team's league
point_color_west <- ifelse(westcombinedleagues$league == "NL", "tomato", "blue")
point_color_east <- ifelse(eastcombinedleagues$league == "NL", "tomato", "blue")

# Map of West Coast World Series winners
us_western_google_road_map + geom_point(aes(x = lon, y = lat),
                                        data = westcombinedleagues, 
                                        alpha = 0.7, 
                                        size = point_size_west, 
                                        color = point_color_west)

# Map of East Coast World Series winners
us_eastern_google_road_map + geom_point(aes(x = lon, y = lat),
                                        data = eastcombinedleagues, 
                                        alpha = 0.7, 
                                        size = point_size_east, 
                                        color = point_color_east)

#############################
#### Shiny Dashboard App ####
#############################

library(shiny)
library(shinythemes)
library(rsconnect)


ui <- fluidPage(
  theme = shinytheme("simplex"),
  titlePanel("MLB Statistics Snapshot"), #overall title that stays there the whole time
  hr(),
  
  tabsetPanel(
    tabPanel(title = "IP Per Season",
             br(),
             p(style = "font-family: Times New Roman",
               "Innings Pitched (IP) is often a good indicator of a pitcher's stamina over the course of the season."),
             hr(),
             mainPanel(
               fluidRow(
                 plotOutput(outputId = "IPplot")))),
    
    tabPanel(title = "Season ERA Comparison",
             br(),
             p(style = "font-family: Times New Roman",
               "ERA is a measure of average earned runs (ER) given up over a 9 inning span. The lower the ERA, the better."),
             hr(),
             mainPanel(
               fluidRow(
                 plotOutput(outputId = "ERAplot")))),
    
    tabPanel(title = "Red Sox Favorites: Career Hits",
             br(),
             p(style = "font-family: Times New Roman",
               "Career hits through the end of the 2016 season. Mookie Betts just completed his second year in the MLB."),
             hr(),
             mainPanel(
               fluidRow(
                 plotOutput(outputId = "CareerHitsplot")))),
    
    tabPanel(title = "Steroid Era Home Runs Hit",
             br(),
             p(style = "font-family: Times New Roman",
               "The 'Steroid Era' officially began when steroids made the banned substance list for MLB (1991), however official testing for steroids didn't begin until 2003."),
             hr(),
             mainPanel(
               fluidRow(
                 plotOutput(outputId = "RoidEraHRplot")))),
    
    tabPanel(title = "West Coast WS Winners",
             br(),
             p(style = "font-family: Times New Roman",
               "Red points denote NL teams, blue denotes AL. Point size is determined by WS wins in the last 25 years."),
             hr(),
             mainPanel(
               fluidRow(
                 plotOutput(outputId = "WCplot")))),
    
    tabPanel(title = "East Coast WS Winners",
             br(),
             p(style = "font-family: Times New Roman",
               "Red points denote NL teams, blue denotes AL. Point size is determined by WS wins in the last 25 years."),
             hr(),
             mainPanel(
               fluidRow(
                 plotOutput(outputId = "ECplot"))))))







server <- function(input, output) {
  
  
  
  output$IPplot <- renderPlot({
    
    theme_set(theme_classic())
    
    g1 <- ggplot(IPpitchingfavorites, aes(x = playerID, y = IP)) + 
      theme(axis.text.x = element_text(angle=45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 14)) +
      #geom_point(data=pitch.avg, col="blue", size = 4) +
      geom_point(col = "tomato2", size = 2) +
      geom_segment(aes(x = as.factor(playerID),
                       xend = as.factor(playerID),
                       y = min(IP),
                       yend = max(IP)),
                   linetype= "dashed",
                   size = 0.1) + 
      labs(title = "Dot Plot",
           subtitle = "Average Innings Pitched Per Season",
           caption = "Source: Lahman's Baseball Database",
           x = "Player",
           y = "Avg IP / Season") 
    
    print(g1)
    
    # Dot plot of average innings pitched per season, over course of career, superimposed over g1
    g2 <- g1 + geom_point(data = avg_IP, col="blue", size = 5)
    
    print(g2)
  })
  
  output$ERAplot <- renderPlot({
    
    df$color <- ifelse((df$era_2011 - df$era_2016) < 0, "Red", "Green")
    p <- ggplot(df) + 
      geom_segment(aes(x=rep(1,8), 
                       xend=rep(2,8),
                       y=era_2011,
                       yend= era_2016, col=color), 
                   size=.75, show.legend=F) + 
      geom_vline(xintercept=1, linetype="dashed", size=.1) + 
      geom_vline(xintercept=2, linetype="dashed", size=.1) +
      scale_color_manual(labels = c("Up", "Down"), 
                         values = c("Green"="#00ba38", "Red"="#f8766d")) +  # color of lines
      labs(title = "Season ERA Comparison, 2016 vs. 2011", x="", y="ERA") +  # Axis labels
      xlim(.5, 2.5) + ylim(1, 5) +
      theme(title = element_text(size = 16))
    
    print(p)
    
    # Prep labels
    left_label <- paste(df$name, (df$era_2011),sep=", ")
    right_label <- paste(df$name, (df$era_2016),sep=", ")
    
    # Add texts / labels
    p <- p + geom_text_repel(label=left_label, y=df$era_2011, x=rep(1, 8), hjust=1.1, size=3.5)
    p <- p + geom_text_repel(label=right_label, y=df$era_2016, x=rep(2, 8), hjust=-0.1, size=3.5)
    p <- p + geom_text(label="2011", x=1, y=5, hjust=1.2, size=5)  # title
    p <- p + geom_text(label="2016", x=2, y=5, hjust=-0.1, size=5)  # title
    
    print(p)
    
  })
  
  output$CareerHitsplot <- renderPlot({
    
    f <- ggplot(redsoxfavorites, aes(x = playerID, y= H)) + 
      geom_bar(stat="identity", width=.5, fill="tomato3") + 
      labs(title="Ordered Bar Chart", 
           subtitle="Career Hits", 
           caption="source: MLB Batting Data",
           x = "Player",
           y = "Hits") + 
      theme(axis.text.x = element_text(angle=45, vjust=0.6, size = 16),
            axis.text.y = element_text(size = 16),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 14))
    
    print(f)
    
  })
  
  output$RoidEraHRplot <- renderPlot({
    
    g <- ggplot(data2, aes(x=Year)) + 
      geom_line(aes(y=Rafael_Palmeiro, group = 1, col = "Rafael Palmeiro")) + 
      geom_line(aes(y=Mark_McGwire, group = 1, col = "Mark McGwire")) + 
      geom_line(aes(y=Sammy_Sosa, group = 1, col = "Sammy Sosa")) +
      geom_line(aes(y=Barry_Bonds, group = 1, col = "Barry Bonds")) +
      geom_line(aes(y=Alex_Rodriguez, group = 1, col = "Alex Rodriguez")) +
      geom_line(aes(y=Jason_Giambi, group = 1, col = "Jason Giambi")) +
      labs(title="Time Series of HRs hit", 
           caption="Source: Lahman's Baseball Database", y="HRs") +
      scale_color_manual(name="", 
                         values = c("Rafael Palmeiro" = "red", "Mark McGwire" = "orange", "Sammy Sosa" = "green", "Barry Bonds" = "cyan", "Alex Rodriguez" = "blue", "Jason Giambi" = "magenta")) + 
      theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 16),
            axis.text.y = element_text(size = 16),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20))
    
    print(g)
    
  })
  
  output$WCplot <- renderPlot({
    
    us_western_google_road_map + geom_point(aes(x = lon, y = lat),
                                            data = westcombinedleagues, 
                                            alpha = 0.7, 
                                            size = point_size_west, 
                                            color = point_color_west)
    
  })
  
  output$ECplot <- renderPlot({
    
    us_eastern_google_road_map + geom_point(aes(x = lon, y = lat),
                                            data = eastcombinedleagues, 
                                            alpha = 0.7, 
                                            size = point_size_east, 
                                            color = point_color_east)
    
  })}

shinyApp(ui = ui, server = server)


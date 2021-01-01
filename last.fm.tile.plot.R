# -----------------
# LESSONS
# -----------------

# https://www.r-bloggers.com/music-listener-statistics-last-fms-last-year-as-an-r-package/
# devtools::install_github("zappingseb/analyze_last_fm")

# https://towardsdatascience.com/visualising-temperatures-in-amsterdam-as-a-heatmap-in-r-part-ii-92db6b37a5e1
# https://stackoverflow.com/questions/20571306/multi-row-x-axis-labels-in-ggplot-line-chart
# https://stackoverflow.com/questions/57626853/labeling-x-axis-with-another-column-from-dataframe?noredirect=1&lq=1

# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales

# -----------------
# DEPENDENCIES
# -----------------

library(analyzelastfm)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(anytime)
library(lubridate)
library(zoo)
library(plotly)

# -----------------
# API
# -----------------

# Application name	Analytics
  # API key	3201a3f5635a9e01a9bcb836729e3463
  # Shared secret	bab39d5bd8c8d23526fb6bde6dd8c9ae
  # Registered to	Perkski

# Define API
api_key <- "3201a3f5635a9e01a9bcb836729e3463"

# -----------------
# DATA
# -----------------

# Read in song listen history
last.fm.data.2020 <- UserData$new("Perkski", api_key, 2020) 
last.fm.data.2019 <- UserData$new("Perkski", api_key, 2019) 
last.fm.data.2018 <- UserData$new("Perkski", api_key, 2018) 
last.fm.data.2017 <- UserData$new("Perkski", api_key, 2017)
last.fm.data.2016 <- UserData$new("Perkski", api_key, 2016)

# extract out data.table from last.fm environment
last.fm.df.2020 <- as.data.frame(as.list(last.fm.data.2020$data_table))
last.fm.df.2019 <- as.data.frame(as.list(last.fm.data.2019$data_table))
last.fm.df.2018 <- as.data.frame(as.list(last.fm.data.2018$data_table))
last.fm.df.2017 <- as.data.frame(as.list(last.fm.data.2017$data_table))
last.fm.df.2016 <- as.data.frame(as.list(last.fm.data.2016$data_table))

# bind 2017 and 2018
last.fm.df <- rbind(last.fm.df.2016, 
                    last.fm.df.2017, 
                    last.fm.df.2018, 
                    last.fm.df.2019,
                    last.fm.df.2020)

# Look at data-frame
str(last.fm.df)

# re-order 
last.fm.df <- last.fm.df[order(-last.fm.df$datetext),]

# -----------------
# DATA CLEAN
# -----------------

# remove non-music artists
last.fm.df <- last.fm.df[ !(last.fm.df$artist %in% 
                              c("<Sconosciuto>", "1 Hour of Relaxing Zelda", "2814", "Valhalla DSP",
                                "Valhalla DSP Plugin Presets", "inclair Broadcast Group",
                                "GoldenEye", "Larry David Is OK With Women Who Only Love Fame",
                                "Outlaw King", "Game Of Thrones Season 6 Episode 10 Music",
                                "ValhallaVintageVerb Ambient Guitar Jam", "Valhalla Shimmer Reverb",
                                "Garageband Quick Tip", "Perkot", "Ozzy Man Reviews: Iguana vs Snakes",
                                "The Simpsons")), ]

# Check unique artists
unique.artists <- unique(last.fm.df$artist)
unique.artists

# Need to remove duplicate song/artist combos in succession - known glitch in scrobbling

# remove duplicate tracks where scrobble has doubled
last.fm.df <- last.fm.df[!duplicated(last.fm.df[c("datetext")]),]
# combine artist and song into single column 
last.fm.df <- last.fm.df %>% 
  mutate(artist.track = str_c(artist," - ",track))

# this will delete first instance of any two tracks occurring in succession
last.fm.df <- as.data.table(last.fm.df)[, .SD[1], by = rleid(artist.track)]

# -----------------
# TIME METRICS
# -----------------

# CLEAN UP DATE & TIME

# Separate out date & time
last.fm.df <- separate(last.fm.df, datetext, 
                       into = c("Date.UT", "Time.UT"), 
                       sep = ",")

# Convert data into more suitable format
last.fm.df$Date.UT = as.Date(last.fm.df$Date.UT, "%d %B %Y")

# Combine date and time back together
last.fm.df <- last.fm.df %>% 
  unite(DateTime.UT, c(Date.UT, Time.UT), sep = " ", remove = FALSE)

# change to time format 
last.fm.df$DateTime.UT <- as.POSIXct(last.fm.df$DateTime.UT, 
                                     format = "%Y-%m-%d %H:%M")

# DETERMINE EST - ACTUAL LISTENING TIMES

# Change to EST - times are universal
last.fm.df$DateTime.EST <- last.fm.df$DateTime.UT + hours(11)

# Duplicate datetext
last.fm.df$DateTime.EST2 = last.fm.df$DateTime.EST

# Separate into time and date
last.fm.df <- separate(last.fm.df, DateTime.EST2, 
                       into = c("Date.EST", "Time.EST"), 
                       sep = " ")

# Deleting Columns 
last.fm.df <- subset(last.fm.df, select = -c(5))

# Change Names
colnames(last.fm.df) <- c("ID", "Artist", "Track", "Album", 
                          "DateTime.UT", "Date.UT", "Time.UT",
                          "DateTime.EST", "Date.EST", "Time.EST")

# ADDITIONAL TIME METRICS 

# Create week column 
last.fm.df$Week <- isoweek(ymd(last.fm.df$Date.EST))

# Extract month from date
last.fm.df$Month <- format(anydate(last.fm.df$Date.EST), "%m")

# Quarter from date
last.fm.df$Quarter <- as.yearqtr(last.fm.df$Date.EST, format = "%Y-%m-%d")

# Year from date
last.fm.df$Year <- year(last.fm.df$Date.EST)

# Merge "Year" Column with "Week" Column 
last.fm.df$Yr_Week <- paste(last.fm.df$Year, last.fm.df$Week, sep="-")

last.fm.df$Date.EST = as.Date(last.fm.df$Date.EST)

# Create "Year-Month" column
setDT(last.fm.df)[, yr_month := format(as.Date(Date.EST), "%Y-%m") ]

#Round clock-time to nearest hour

# Duplicate time of song scrobble columsn to create 'hour' column 
last.fm.df$Hour = last.fm.df$Time.EST
# Convert to time format
last.fm.df$Hour <- as.POSIXct(last.fm.df$Hour, format = "%H:%M:%S")
# Round to nearest hour
last.fm.df$Hour = format(round(last.fm.df$Hour, units = "hours"), format = "%H:%M")

# Day of the week
last.fm.df$DayofWeek <- weekdays(as.Date(last.fm.df$Date.EST))

# Weekend/Weekday
last.fm.df$DayType[
  last.fm.df$DayofWeek == "Saturday" |
    last.fm.df$DayofWeek == "Sunday"] <-
  "Weekend"
last.fm.df$DayType[is.na(last.fm.df$DayType)] <- "Weekday"

# -----------------
# EXPORT DATA
# -----------------

# Remove df that are not required 
rm(last.fm.df.2016, last.fm.df.2017, last.fm.df.2018, last.fm.df.2019, last.fm.df.2020)

# Standard .csv export 
write.csv(last.fm.df, file = "lastfm1620.csv",
          na = "", 
          row.names = FALSE)

# -----------------
# PREPARE FOR VISUALISATION 
# -----------------

# extract out the calendar 'day' from date 
last.fm.df$day <- substr(x = last.fm.df$Date.EST, 
                         start = 9, 
                         stop = 10)

# Aggregate total listens by date 
last.fm.calendar <- last.fm.df %>% 
  group_by(Date.EST) %>% 
  summarise(daily_listens = n())

# check df
str(last.fm.calendar)

# ensure in df format 
last.fm.calendar <- as.data.frame(last.fm.calendar)

# Crucial step 
  # on dates where I have not listened to music, they are absent from the df
  # as such we need to create "empty records" for these dates, so that they are
  # reflected in the visualisation 

# Complete out dates
last.fm.calendar <- 
last.fm.calendar %>%
  mutate(Date.EST = as.Date(Date.EST)) %>%
  complete(Date.EST = seq.Date(min(Date.EST), max(Date.EST), by="day"))

# Replace NA values with a 0, reflecting no listens on that date 
last.fm.calendar[is.na(last.fm.calendar)] <- 0

# Extract out day from date for visualisation 
last.fm.calendar$day <- substr(x = last.fm.calendar$Date.EST, 
                         start = 9, 
                         stop = 10)

# Create "Year-Month" column - y axis 
setDT(last.fm.calendar)[, yr_month := format(as.Date(Date.EST), "%Y-%m") ]

# Create year column - y axis labels 
last.fm.calendar$Year <- year(last.fm.calendar$Date.EST)

# -----------------
# TILE PLOT VISUALISATION 
# -----------------

# Determine the buckets we want for our legend
# Alternative legend display to the usual gradient 
sl <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110)

# Create tile-plot
p <- 
last.fm.calendar %>% 
  ggplot(aes(x = day,
             forcats::fct_rev(yr_month))) + # reverse order of y-axis
  
  geom_tile(aes(fill = daily_listens), # fill based upon total
            colour = "white", # border colour == white
            size = 0.2, # size of tile
            na.rm = FALSE) + # don't remove missing values 
  coord_equal(ratio = 1) +  # keep even squares
  
  scale_fill_gradient2(low = "#fffafa",
                       high = "#db0909",
                       midpoint = 2,
                       na.value = "#fcfbf7",
                       name = "daily listens",
                       guide = "legend", # can specify colour scale, or legend format
                       breaks = sl) + # specify breaks (this does not work here)
  
  labs(x = "",
       y = "",
       title = "last.fm scrobble history",
       subtitle = "Daily count of itunes song plays between 2016 & 2020",
       caption = "Data-source: last.fm") +
  
  # Guides - a useful way to format legend 
  guides(fill = guide_legend(title = "Daily listens", # title of legend
                             title.hjust = 0.5, # centre title
                             title.vjust = 0.5, # centre title
                             breaks = sl, # specify breaks for legend format rather than colour scale
                             reverse = TRUE)) + # descending rather than ascending order for scale

  # Specify custom axis labels 
  # modify to "year" view for y-axis 
  scale_y_discrete(
    breaks = c("2020-01", "2019-01", "2018-01", "2017-01", "2016-01"), # pick only first-month
    labels = c("2020", "2019", "2018", "2017", "2016")) + # label it as year - cleaner aesthetic
  # remove leading zero from x-axis 
  scale_x_discrete(
    breaks = c("01", "05", "10", "15", "20", "25", "31"), # pick only first-month
    labels = c("1", "5", "10", "15", "20", "25", "31")) + # label it as year - cleaner aesthetic
  
  # crucial - removes gray areas from empty cells (i.e. February 30)
  theme(panel.background = element_rect(fill = "#FFFDFA")) + 
  
  # Titles / Text - size, colours, vjust 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8,
                               vjust = 0.2,
                               colour = "#1C2226"),
        axis.text.x = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#1C2226"),
        plot.title = element_text(colour = "#3b474f",
                              hjust = 0,
                              size = 9,
                              face = "bold"),
        plot.subtitle = element_text(colour = "#5d6b75",
                                 hjust = 0,
                                 size = 8),
        plot.caption = element_text(colour = "#5d6b75",
                                hjust = 0,
                                vjust = 1,
                                size = 7,
                                face = "italic",
                                margin = margin(5,0,0,0)), # adjust position ... top, bottom, left, right
        
        # Remove ticks/titles/labels I don't want 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        
        # Aesthetic of legend 
        legend.position = "right", # position to right of plot
        legend.direction = "vertical", # vertical orientation 
        legend.title = element_text(colour = "#1C2226", # colour of title text
                                    size = 8), # size of text 
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        legend.text = element_text(colour = "#1C2226",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.box.just = "center",
        
        # Plot margins / border / fill-colour 
        plot.background = element_rect(fill = "#fffafa"),
        legend.background = element_rect(fill = "#fffafa"),
        panel.border = element_blank(),
        plot.margin = margin(0.7,0.7,0.7,0.7,"cm") # top, right, bottom, left
        ) 
p

# Save file 
ggsave(p,
       filename="lastfm_tile_plot2.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)

















# COLOUR PALETTE  
# https://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2

# gradient between colour [1] & colour [2]
colfunc <- colorRampPalette(c("#C72B26","#fffdfa"))
# plot gradient
plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)
# generate 11 colours ranging from colour [1] to colour [2]
colfunc(11)

# Alternative colour scheme 
colfunc <- colorRampPalette(c("#db2721","#ffffff"))
plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)
colfunc(11)

# CREATE BUCKETS

# Create buckets for avg_max
last.fm.calendar <- last.fm.calendar %>%
  mutate(bin = cut(daily_listens,
                   breaks=c(0,1,10,20,30,40,50,60,70,80,90,max(daily_listens,na.rm = T)),
                   labels=c("0", 
                            "10",
                            "20",
                            "30",
                            "40",
                            "50",
                            "60",
                            "70",
                            "80",
                            "90",
                            "100"))) %>%
  # change level order
  mutate(bin=factor(as.character(bin),levels=rev(levels(bin))))

last.fm.calendar$bin[is.na(last.fm.calendar$bin)] <- 0



listenpalette <- c("0" = "#FFFFFF", 
                   "10" = "#FBE9E8", 
                   "20" = "#F7D3D2", 
                   "30" = "#F4BEBC", 
                   "40" = "#F0A8A6", 
                   "50" = "#ED9390", 
                   "60" = "#E97D79", 
                   "70" = "#E56763",
                   "80" = "#E2524D",
                   "90" = "#DE3C37",
                   "100" = "#DB2721")

# Plot with bins

last.fm.calendar %>% 
  ggplot(aes(x = day,
             forcats::fct_rev(yr_month))) + # reverse order of y-axis
  geom_tile(aes(fill = bin),
            colour = "white",
            size = 0.2,
            na.rm = FALSE) + 
  coord_equal(ratio = 1) + 
  
  # scale_fill_gradient2(low = "#F1EDDE", 
  #                      high = "#C72B26", 
  #                      midpoint = 2,
  #                      na.value = "#fcfbf7") +
  
  scale_fill_manual(values=listenpalette,
                    na.value = "#FFFDFA") + 
  
  guides(fill = guide_legend(title = "Daily listens")) +
  scale_y_discrete(
    breaks = c("2019-01", "2018-01", "2017-01", "2016-01"),
    labels = c("2019", "2018", "2017", "2016")) +
  
  theme(panel.background = element_rect(fill = "#FFFDFA")) + # removes grey areas
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 8,
                               vjust = 0.2,
                               colour = "#1C2226"
    ),
    #   axis.text.y=element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(colour = "#1C2226",
                                size=8),
    legend.margin = margin(grid::unit(0,"cm")),
    legend.text = element_text(colour = "#1C2226",
                               size = 6,
                               face = "bold"),
    legend.key.height = grid::unit(0.6,"cm"),
    legend.key.width = grid::unit(0.6,"cm"),
    
    plot.background = element_rect(fill = "#FFFFFF"),
    legend.background = element_rect(fill = "#FFFFFF"),
    panel.border = element_blank(),
    plot.margin = margin(0.7,0.4,0.1,0.2,"cm")
  ) 
    


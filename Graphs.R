library(tidyverse)
library(ggrepel)
library(grid)
library(dplyr)
library(lubridate)

standings <- read_csv("EPLStandings.csv")
FinalRPMData <- read_csv("FinalRPMData.csv")
players <- gsub("`", "", FinalRPMData$PlayerNames)
FinalRPMData$PlayerNames <- players

## read in standings and final matrix outputs and clean apostrophes

TeamRPM <- FinalRPMData %>%
  filter(MinutesPlayed > 342) %>%
  group_by(Team) %>%
  summarize(DefRPM = sum(DefRPM))

TeamRPMatt <- FinalRPMData %>%
  filter(MinutesPlayed > 342) %>%
  group_by(Team) %>%
  summarize(AttRPM = sum(AttRPM))

## create team totals for only players who played at least 342 minutes (10%)

lineRpm <- merge(standings, TeamRPM, by.x = 'Club', by.y = "Team" )
lineRpm$Type <- "Defensive"
names(lineRpm)[names(lineRpm) == "DefRPM"] <- "RPM" 

lineRpmA <- merge(standings, TeamRPMatt, by.x = 'Club', by.y = "Team" )
lineRpmA$Type <- "Attacking"
names(lineRpmA)[names(lineRpmA) == "AttRPM"] <- "RPM" 

bar <- rbind(lineRpm, lineRpmA)

lineRpm$Rank <- -(lineRpm$Rank)

one <- lineRpm %>%
  ggplot(aes(x = Club, y = RPM, group = 1)) +
  geom_line(color = "darkblue") +
  ggtitle("Correlation between RPM and Final League Placement") +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))
  
two <- lineRpm %>%
  ggplot(aes(x = Club, y = -Rank, group = 1)) +
  geom_line(color = "darkblue") +
  scale_y_reverse() +
  ylab("League Placement") +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold"))

grid.newpage()
grid.draw(rbind(ggplotGrob(one), ggplotGrob(two), size = "last"))

## line graphs for final standings and club RPMs
  
colors <- c("red3", "blue3")

bar %>%
  ggplot(aes(x=Club, y=RPM, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Attacking vs. Defensive RPM by Club") +
  scale_fill_manual(values=colors) +
  theme(axis.title.x = element_blank()) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))
  
## bar graph for attacking vs defensive RPM by club


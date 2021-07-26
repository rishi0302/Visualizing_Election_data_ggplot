library("readxl")
library(ggplot2)
library(dplyr)
library(viridis)
library(devtools)
#devtools::install_github("wilkelab/cowplot")
#install.packages("colorspace", repos = "http://R-Forge.R-project.org")
#devtools::install_github("clauswilke/colorblindr")
library(colorblindr)

#Read the data into dataframe
galway_2016_df <- read.csv('GalwayWest2016.csv',encoding = "UTF-8",check.names=FALSE)
galway_2020_df <- read_excel('GalwayWest-2020.xlsx',skip = 1)
nation_2016_df <- read.csv('GE2016.csv',check.names=FALSE)
nation_2020_df <- read.csv('GE2020.csv',check.names=FALSE)

party_short <- c('AAA'="AAA",'Direct Democracy Ireland'="DDI",'Fianna Fail'="FF", 'Fine Gael'='FG','Green Party'="GP", 'Independent'="IND", 'Labour Party'="LAB",'Renua'="REN",'Sinn Fein'="SF",'Social Democrats'="SDP",'Aontú'="AON",'Solidarity-PBP'="S-PBP")

#Pre-processing Galway West 2016 Election data
galway_2016_df$Party <- trimws(galway_2016_df$Party, which = c("both"))

galway_2016_df <- galway_2016_df[galway_2016_df$`Count Number`==1,]
galway_2016_df$Party[galway_2016_df$Party == "Ind"] <- "Independent"
galway_2016_df$Party[galway_2016_df$Party == "Fine  Gael"] <- "Fine Gael"
galway_2016_df$Party[galway_2016_df$Party == "Social Democratic Party"] <- "Social Democrats"
galway_2016_df$Party[galway_2016_df$Party == "AAA"] <- "Solidarity-PBP"
galway_2016_df['Short'] <- party_short[galway_2016_df$Party]


#Pre-processing Galway West 2020 Election data
drops <- c("Party")
galway_2020_df <- galway_2020_df[ , !(names(galway_2020_df) %in% drops)]
galway_2020_df <- galway_2020_df[-c(16), ]
colnames(galway_2020_df)[1] <- "Party"
colnames(galway_2020_df)[4] <- "Votes"
galway_2020_df <- select(galway_2020_df, Party, Candidate, Votes)
galway_2020_df$Party[galway_2020_df$Party == "Fianna Fáil"] <- "Fianna Fail"
galway_2020_df$Party[galway_2020_df$Party == "Sinn Féin"] <- "Sinn Fein"
galway_2020_df['Short'] <- party_short[galway_2020_df$Party]

#Part1
#2016 grouping the total votes  based on party
galway_2016_votes <- galway_2016_df %>% group_by(Party,Short) %>% summarise(Votes = sum(Votes))

# Calculating the percentage of the votes received by each party
galway_2016_votes$Votes_Percent <- paste(round(galway_2016_votes$Votes*100/sum(
  galway_2016_votes$Votes),2), "%")

# Plot 1
party.colours <- c('FG' = '#1f78b4', 'FF' = '#33a02c', 'SF' = 'darkolivegreen', 'LAB' = '#e31a1c',  'GP' = '#b2df8a', 'SDP' ='#cab2d6', 'IND' = 'darkgrey', 'REN' = '#ff7f00', 'DDI' ='darkgrey','AAA' ='darkgrey',"AON"="#44532A","S-PBP"= '#fdbf6f')

theme_set(theme_classic())

plot_1_2016 <- ggplot(galway_2016_votes, (aes(x= reorder(Short, -Votes) , y=Votes, fill = Short))) + 
  geom_col( width=0.7) +
  scale_y_continuous(limits = c(0, 16000),
                     breaks = seq(0,16000, by = 2000),
                     name = "Votes") +
  geom_text(aes(label=Votes_Percent), size =2.5, vjust=-0.4) +
  scale_fill_manual(values = party.colours)+
  xlab("Parties") +
  guides(fill=guide_legend(title="Parties")) +
  ggtitle("Votes Per Party in General Election 2016 - Galway West") +
  
  theme(
    plot.margin = margin(6, 6, 3, 3),
    panel.background = element_blank(),
    panel.ontop = TRUE,
    plot.title = element_text(vjust=-0.5, hjust=0.2, face='bold', size = 13)
  )
plot_1_2016

#2020 grouping the total votes based on party
galway_2020_votes <- galway_2020_df %>% group_by(Party,Short) %>% summarise(Votes = sum(Votes))

# Calculating the percentage of the votes received by each party
galway_2020_votes$Votes_Percent <- paste(round(galway_2020_votes$Votes*100/sum(
  galway_2020_votes$Votes),2), "%")

# Plot 1 for 2020
plot_1_2020 <- ggplot(galway_2020_votes, (aes(x= reorder(Short, -Votes) , y=Votes, fill = Short))) + 
  geom_col( width=0.7) +
  scale_y_continuous(limits = c(0, 18000),
                     breaks = seq(0,18000, by = 2000),
                     name = "Votes") +
  geom_text(aes(label=Votes_Percent), size =2.5, vjust=-0.4) +
  scale_fill_manual(values = party.colours)+
  xlab("Parties") +
  guides(fill=guide_legend(title="Parties")) +
  ggtitle("Votes Per Party in General Election 2020 - Galway West") +
  
  theme(
    plot.margin = margin(6, 6, 3, 3),
    panel.background = element_blank(),
    
    panel.ontop = TRUE,
    plot.title = element_text(vjust=-0.5, hjust=0.2, face='bold', size = 13)
  )
plot_1_2020

#Part 2
galway_2016_votes <- galway_2016_votes %>% rename(Votes_2016 = Votes)
galway_2020_votes <- galway_2020_votes %>% rename(Votes_2020 = Votes)
drops <- c("Votes_Percent")
galway_2016_votes <- galway_2016_votes[ , !(names(galway_2016_votes) %in% drops)]
galway_2020_votes <- galway_2020_votes[ , !(names(galway_2020_votes) %in% drops)]



#Merging the data
votes_2016_2020_df <- full_join(galway_2016_votes, galway_2020_votes, by=c("Party", "Short")) 
votes_2016_2020_df[is.na(votes_2016_2020_df)] <- 0
votes_2016_2020_df_diff <- votes_2016_2020_df %>% 
  select(Party, Votes_2016, Votes_2020) %>%
  mutate(diff = Votes_2016-Votes_2020,
         above = ifelse(diff > 0, TRUE, FALSE))%>%
  arrange(diff)%>%
  mutate(Party = factor(Party, levels = .$Party))

mean_diff <- mean(votes_2016_2020_df_diff$diff)
#votes_2016_2020_df_diff$diff <- votes_2016_2020_df_diff$diff/mean_diff

plot_2 <- ggplot(votes_2016_2020_df_diff, aes(x=diff, y=Party, colour = above)) +
  geom_segment(aes(x = 0, y = Party, xend = diff, yend = Party), size = 0.9, color = "darkgrey") +
  geom_point(size =2.5) +
  xlab('Vote Difference Per Party from 2016 to 2020') +
  ylab('Party') +
  ggtitle("Vote Comparison for each Party: 2016 vs 2020") +
  scale_colour_viridis_d()+ 
  scale_x_continuous(limits = c(-3000, 5000),
                     expand = c(0, 0),
                     name = "2016 vs 2020 average difference in Votes Per Party",
                     breaks = seq(-3000,5000, by = 1000)) +
  
  theme(panel.grid.major.x = element_line(size=0.04, colour = "lightgrey"),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "None",
        plot.title = element_text(vjust=-0.5, hjust=0.2, face='bold', size = 13),
        axis.title.y = element_text(size = 9, face = 'bold'),
        axis.title.x = element_text(size = 9, face = 'bold'))
plot_2
cvd_grid(plot_2)
#Plot3
#2016
nation_2016_df$Party <- trimws(nation_2016_df$Party, which = c("both"))
colnames(nation_2016_df)[1] <- "Party"
colnames(nation_2016_df)[4] <- "Votes_Percent"
nation_2016_df$Party[nation_2016_df$Party =="People before Profit"] <- "Solidarity-PBP"
nation_2016_df$Party[nation_2016_df$Party =="Direct Democracy Ireland - National Citizens Movement"] <- "Direct Democracy Ireland"
nation_2016_df$Party[nation_2016_df$Party =="Fianna Fáil"] <- "Fianna Fail"
nation_2016_df$Party[nation_2016_df$Party =="Sinn Féin"] <- "Sinn Fein"
nation_2016_df$Party[nation_2016_df$Party =="Other Independents"] <- "Independent"
nation_2016_df$Party[nation_2016_df$Party =="Independent Alliance"] <- "Independent"
nation_2016_df$Party[nation_2016_df$Party =="Independents 4 Change"] <- "Independent"
nation_2016_df$Party[nation_2016_df$Party =="Labour"] <- "Labour Party"
nation_2016_df$Party[nation_2016_df$Party =="Renua Ireland"] <- "Renua"
nation_2016_df <- nation_2016_df[-c(22,23), ]
nation_2016_df['Short'] <- party_short[nation_2016_df$Party]
nation_2016_votes <- nation_2016_df %>% group_by(Party,Short) %>% summarise(Votes_Percent = sum(Votes_Percent))
# Calculating the percentage of the votes received by each party
galway_2016_votes$Votes_Percent <- paste(round(galway_2016_votes$Votes_2016*100/sum(
  galway_2016_votes$Votes_2016),2))
galway_2016_votes$Votes_Percent <- as.numeric(galway_2016_votes$Votes_Percent)
galway_2016_votes <- galway_2016_votes %>% select(Party, Short, Votes_Percent)
galway_2016_votes$Election_Name <- "GalwayWest"
nation_2016_votes$Election_Name <- "National"
overall_votes_2016 <- rbind(galway_2016_votes, nation_2016_votes) 
overall_votes_2016 <- overall_votes_2016[!is.na(overall_votes_2016$Short),]
plot_3_2016 <- ggplot(overall_votes_2016, (aes(x= Short, y=Votes_Percent, fill=Election_Name))) + 
  geom_col(position="dodge", alpha=0.85) +
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0,30, by = 5),
                     name = "Vote Percentage(%)") +
  xlab(label = "Parties") +
  ggtitle("Party's Vote Share Comparison for 2016 GE: GalwayWest vs National Average") +
  scale_fill_manual(values = c("#fc8d62","#66c2a5" ), name = NULL) +
  
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(size = 0.4, linetype = 'solid', colour = "white"),
    panel.ontop = TRUE,
    plot.title = element_text(vjust=-0.5, hjust=0.2, face='bold', size = 13),
    legend.position= c(0.8, 0.9), legend.direction="horizontal")
plot_3_2016

#2020
nation_2020_df$Party <- trimws(nation_2020_df$Party, which = c("both"))
colnames(nation_2020_df)[1] <- "Party"
colnames(nation_2020_df)[4] <- "Votes_Percent"
nation_2020_df$Party[nation_2020_df$Party == "Sinn Féin"] <- "Sinn Fein"
nation_2020_df$Party[nation_2020_df$Party == "Fianna Fáil"] <- "Fianna Fail"
nation_2020_df$Party[nation_2020_df$Party == "Labour"] <- "Labour Party"
nation_2020_df$Party[nation_2020_df$Party == "Solidarity-PBP (Solidarity- People Before Profit Alliance)"] <- "Solidarity-PBP"
nation_2020_df$Party[nation_2020_df$Party == "Independents"] <- "Independent"
nation_2020_df <- nation_2020_df[-c(22,23), ]
nation_2020_df['Short'] <- party_short[nation_2020_df$Party]
nation_2020_votes <- nation_2020_df %>% group_by(Party,Short) %>% summarise(Votes_Percent = sum(Votes_Percent))
# Calculating the percentage of the votes received by each party
galway_2020_votes$Votes_Percent <- paste(round(galway_2020_votes$Votes_2020*100/sum(
  galway_2020_votes$Votes_2020),2))
galway_2020_votes$Votes_Percent <- as.numeric(galway_2020_votes$Votes_Percent)
galway_2020_votes <- galway_2020_votes %>% select(Party, Short, Votes_Percent)
galway_2020_votes$Election_Name <- "GalwayWest"
nation_2020_votes$Election_Name <- "National"
overall_votes_2020 <- rbind(galway_2020_votes, nation_2020_votes) 
overall_votes_2020 <- overall_votes_2020[!is.na(overall_votes_2020$Short),]
plot_3_2020 <- ggplot(overall_votes_2020, (aes(x= Short, y=Votes_Percent, fill=Election_Name))) + 
  geom_col(position="dodge", alpha=0.85) +
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0,30, by = 5),
                     name = "Vote Percentage(%)") +
  xlab(label = "Parties") +
  ggtitle("Party's Vote Share Comparison for 2020 GE: GalwayWest vs National Average") +
  scale_fill_manual(values = c("#fc8d62","#66c2a5" ), name = NULL) +
  
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(size = 0.4, linetype = 'solid', colour = "white"),
    panel.ontop = TRUE,
    plot.title = element_text(vjust=-0.5, hjust=0.2, face='bold', size = 13),
    legend.position= c(0.8, 0.9), legend.direction="horizontal")
plot_3_2020

#Part 4
# create a new column Candidate with the two columns First and Last name collapsed together
cols <- c('Candidate First Name', 'Candidate surname')
galway_2016_df['Candidate'] <- apply( galway_2016_df[ , cols ] , 1 , paste , collapse = " " )
galway_2016_df <- galway_2016_df  %>% select(Party, Candidate, Votes, Short)
mean_votes <- mean(galway_2020_df$Votes)
#As mean votes is 4022 so we will only take the candidates whose votes are above the mean value for 2020 candidates
galway_2020_df <- galway_2020_df[galway_2020_df$Votes>4000,]
galway_2016_df$Candidate[galway_2016_df$Candidate == "Éamon O'Cuív"] <- "Éamon Ó Cuív"
galway_2016_df$Candidate[galway_2016_df$Candidate == "Catherine Martina Ann Connolly"] <- "Catherine Connolly"
galway_2016_df$Candidate[galway_2016_df$Candidate == "Sean Kyne"] <- "Seán Kyne"
candidates_list <- galway_2020_df$Candidate
galway_2016_df <- galway_2016_df[galway_2016_df$Candidate %in% candidates_list, ]
galway_2020_df <- galway_2020_df[galway_2020_df$Candidate %in% galway_2016_df$Candidate, ]
galway_2016_df$Year <- '2016'
galway_2020_df$Year <- '2020'
candidate_votes_df <- rbind(galway_2016_df, galway_2020_df)

#Plot4 
plot_4 <- ggplot(candidate_votes_df , aes(x = Votes, y= Candidate)) +
  geom_line(aes(group = Candidate), colour = "grey", size=0.5) +
  geom_point(aes(colour = Year), size = 3, alpha = 0.7) +
  
  geom_text(aes(label=Year, colour = Year), vjust=-1.2, size=2, na.rm = TRUE, show.legend = FALSE)+ 
  
  scale_colour_manual(values= c("#ce5a6c","#212f85"), name = "") +
  
  scale_x_continuous(limits = c(4000, 10000), 
                     expand = c(0, 0),
                     breaks = seq(4000, 10000, by = 1000),
                     name = "Votes") +
  ylab(label = "Candidate Name") +
  ggtitle("GalwayWest Candidate Performance Comparison: 2016 vs 2020") +
  
  theme(
    panel.grid.major.x =element_line(size=0.03),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position= c(0.85, 0.94),
    legend.text = element_text(size = 12), # legend text  was a little large
    legend.key.size = unit(0.9, "lines"),
    plot.title = element_text(vjust=-0.5, hjust=0.1, face='bold', size = 11),
    legend.title = element_blank())# legend keys were a little large)
plot_4
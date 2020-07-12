#Install any required libraries
install.packages('tidyverse')
install.packages('data.table')
install.packages('gsubfn')
install.packages('ggthemes')
install.packages('gghighlight')
install.packages('gganimate')
install.packages('transformr')
install.packages('gifski')
install.packages('png')
install.packages('magick')

#Load required libraries
library('tidyverse')
library('readr')
library('data.table')
library('gsubfn')
library('ggthemes')
library('gghighlight')
library('gganimate')
library('transformr')
library('gifski')
library('png')
library('magick')

#Clear workspace if needed
rm(list = ls())

##2018-19 season
#Set working directory
setwd('EPL weekly chart animation/2018-19 Season/Weekly Data')

#Read data into dataframe - get list and enter into proper dataframe
FileList <- list.files(pattern = '*.csv')
PrevSeasonDF <-   list.files(pattern = "*.csv") %>% 
                  map_df(~fread(.))

#Replace data as needed - clean up club names
PrevSeasonDF$Club <- PrevSeasonDF$Club %>% 
                      gsub("Man City", "Manchester City", .) %>% 
                      gsub("Man Utd", "Manchester United", .) %>%
                      gsub("West Ham", "West Ham United", .) %>%
                      gsub("Spurs", "Tottenham Hotspur", .) %>%
                      gsub("Wolves", "Wolverhampton", .) %>%
                      gsub("Leicester", "Leicester City", .) %>%
                      gsub("Cardiff", "Cardiff City", .)

#Change Clubs to factors
PrevSeasonDF$Club <- as.factor(PrevSeasonDF$Club)

#Subset only data that needs to be plotted 
Top2Data <- PrevSeasonDF %>% 
                filter((Club == 'Liverpool' | Club == 'Manchester City'))
                       
Top4Data <- PrevSeasonDF %>% 
                filter((Club == 'Liverpool' | Club == 'Manchester City' | Club == 'Chelsea' | Club == 'Tottenham Hotspur'))

NLData <- PrevSeasonDF %>% 
                filter((Club == 'Arsenal' | Club == 'Tottenham Hotspur'))

#Get final positions for geom point
PrevSeasonDFFinal <- PrevSeasonDF %>% 
                filter(Pl == 38)
ColourDataFinal <- ColourData %>% 
                filter(Pl == 38)

#Plot Top 2 Clubs
Top2Plot <- 
      ggplot() +
      geom_line(aes(x = Pl, y = Pts, group = Club), size = 1.5, data = PrevSeasonDF, colour = '#999999', alpha = 0.5) +
      #geom_point(aes(x = Pl, y = Pts, group = Club), data = PrevSeasonDF, colour = '#999999', alpha = 0.8, size = 5) +
      geom_line(aes(x = Pl, y = Pts, colour = Club), size = 1.5, data = Top2Data, alpha = 0.8) +
      #geom_point(aes(x = Pl, y = Pts, colour = Club),data = ColourData, alpha = 0.8, size = 5) +
      #geom_text(aes(x = 42, y = Pts, label = Club, colour = Club), data = ColourData, size = 5) +
      #geom_label(aes(x = Pl, y = Pts, colour = Club, label = Club), data = ColourDataFinal) +
      geom_abline(intercept = 0, slope = 3, colour = 'white', linetype = 'dashed') +
      theme_classic() +
      scale_x_continuous(name = 'Games Played', limits = c(0,40), breaks = seq(0, 38, 5), expand = c(0,0)) +
      scale_y_continuous(name = 'Points Scored', limits = c(0,115), breaks = seq(0, 115, 5), expand = c(0,0)) +
      theme(legend.position = 'bottom',
            legend.background = element_rect(fill = 'black'),
            legend.title =  element_text(size = 12, colour = 'white', face = 'bold'),
            legend.text = element_text(size = 12, colour = 'white'),
            plot.title = element_text(size = 20, hjust = 0.5, colour = 'white'),
            panel.background = element_rect(fill = 'black'),
            plot.background = element_rect(fill = 'black'),
            axis.line = element_line(colour = 'white'),
            axis.text.x = element_text(size = 12,angle = 0, vjust = 0.5, colour = 'white'), 
            axis.text.y = element_text(size = 12, colour = 'white'), 
            axis.title.x = element_text(size = 15, colour = 'white'), 
            axis.title.y = element_text(size = 15, colour = 'white')) +
      scale_color_manual(values = c("Liverpool" = "red2", "Manchester City" = "skyblue2")) +
      ggtitle('2018-19 EPL Season Title Race') +
      geom_text(aes(x = 30,y = 95,label = "Perfect season", vjust = 1), colour = 'white', angle = 30, size = 5) +
      geom_text(aes(x = 1,y = 100,label = "@rv94", vjust = -0.4, hjust = 0.2), colour = 'white', size = 9, alpha = 0.4) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)))

#Save entire plot
animate(Top2Plot + transition_reveal(Pl), 
        width = 974, 
        height = 614, 
        nframes = 200, 
        fps = 15, 
        end_pause = 20,
        renderer = gifski_renderer('Top2Plot.gif'))

#Save individual frames
animate(Top2Plot + transition_reveal(Pl), 
        nframes = 50,  
        width = 974, 
        height = 614, 
        device = 'png',
        renderer = file_renderer( prefix = 'Top2Plot',
                                  overwrite = T))

#Plot Top 4 Clubs
Top4Plot <- 
      ggplot() +
      geom_line(aes(x = Pl, y = Pts, group = Club), size = 1.5, data = PrevSeasonDF, colour = '#999999', alpha = 0.5) +
      #geom_point(aes(x = Pl, y = Pts, group = Club), data = PrevSeasonDF, colour = '#999999', alpha = 0.8, size = 5) +
      geom_line(aes(x = Pl, y = Pts, colour = Club), size = 1.5, data = Top4Data, alpha = 0.8) +
      #geom_point(aes(x = Pl, y = Pts, colour = Club),data = ColourData, alpha = 0.8, size = 5) +
      #geom_text(aes(x = 42, y = Pts, label = Club, colour = Club), data = ColourData, size = 5) +
      #geom_label(aes(x = Pl, y = Pts, colour = Club, label = Club), data = ColourDataFinal) +
      geom_abline(intercept = 0, slope = 3, colour = 'white', linetype = 'dashed') +
      theme_classic() +
      scale_x_continuous(name = 'Games Played', limits = c(0,40), breaks = seq(0, 38, 5), expand = c(0,0)) +
      scale_y_continuous(name = 'Points Scored', limits = c(0,115), breaks = seq(0, 115, 5), expand = c(0,0)) +
      theme(legend.position = 'bottom',
            legend.background = element_rect(fill = 'black'),
            legend.title =  element_text(size = 12, colour = 'white', face = 'bold'),
            legend.text = element_text(size = 12, colour = 'white'),
            plot.title = element_text(size = 20, hjust = 0.5, colour = 'white'),
            panel.background = element_rect(fill = 'black'),
            plot.background = element_rect(fill = 'black'),
            axis.line = element_line(colour = 'white'),
            axis.text.x = element_text(size = 12,angle = 0, vjust = 0.5, colour = 'white'), 
            axis.text.y = element_text(size = 12, colour = 'white'), 
            axis.title.x = element_text(size = 15, colour = 'white'), 
            axis.title.y = element_text(size = 15, colour = 'white')) +
      scale_color_manual(values = c("Liverpool" = "red2", "Manchester City" = "skyblue2", "Tottenham Hotspur" = "white", "Chelsea" = "royalblue3")) +
      ggtitle('2018-19 EPL Season Title Race') +
      geom_text(aes(x = 30,y = 95,label = "Perfect season", vjust = 1), colour = 'white', angle = 30, size = 5) +
      geom_text(aes(x = 1,y = 100,label = "@rv94", vjust = -0.4, hjust = 0.2), colour = 'white', size = 9, alpha = 0.4) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)))

#Save entire plot
animate(Top4Plot + transition_reveal(Pl), 
        width = 974, 
        height = 614, 
        nframes = 200, 
        fps = 15, 
        end_pause = 20,
        renderer = gifski_renderer('Top4Plot.gif'))

#Save individual frames
animate(Top4Plot + transition_reveal(Pl), 
        nframes = 50,  
        width = 974, 
        height = 614, 
        device = 'png',
        renderer = file_renderer( prefix = 'Top4Plot',
                                  overwrite = T))

#Plot North London duel
NLPlot <- 
      ggplot() +
      geom_line(aes(x = Pl, y = Pts, group = Club), size = 1.5, data = PrevSeasonDF, colour = '#999999', alpha = 0.5) +
      #geom_point(aes(x = Pl, y = Pts, group = Club), data = PrevSeasonDF, colour = '#999999', alpha = 0.8, size = 5) +
      geom_line(aes(x = Pl, y = Pts, colour = Club), size = 1.5, data = NLData, alpha = 0.8) +
      #geom_point(aes(x = Pl, y = Pts, colour = Club),data = ColourData, alpha = 0.8, size = 5) +
      #geom_text(aes(x = 42, y = Pts, label = Club, colour = Club), data = ColourData, size = 5) +
      #geom_label(aes(x = Pl, y = Pts, colour = Club, label = Club), data = ColourDataFinal) +
      geom_abline(intercept = 0, slope = 3, colour = 'white', linetype = 'dashed') +
      theme_classic() +
      scale_x_continuous(name = 'Games Played', limits = c(0,40), breaks = seq(0, 38, 5), expand = c(0,0)) +
      scale_y_continuous(name = 'Points Scored', limits = c(0,115), breaks = seq(0, 115, 5), expand = c(0,0)) +
      theme(legend.position = 'bottom',
            legend.background = element_rect(fill = 'black'),
            legend.title =  element_text(size = 12, colour = 'white', face = 'bold'),
            legend.text = element_text(size = 12, colour = 'white'),
            plot.title = element_text(size = 20, hjust = 0.5, colour = 'white'),
            panel.background = element_rect(fill = 'black'),
            plot.background = element_rect(fill = 'black'),
            axis.line = element_line(colour = 'white'),
            axis.text.x = element_text(size = 12,angle = 0, vjust = 0.5, colour = 'white'), 
            axis.text.y = element_text(size = 12, colour = 'white'), 
            axis.title.x = element_text(size = 15, colour = 'white'), 
            axis.title.y = element_text(size = 15, colour = 'white')) +
      scale_color_manual(values = c("Arsenal" = "red4", "Tottenham Hotspur" = "white")) +
      ggtitle('2018-19 EPL Season Title Race') +
      geom_text(aes(x = 30,y = 95,label = "Perfect season", vjust = 1), colour = 'white', angle = 30, size = 5) +
      geom_text(aes(x = 1,y = 100,label = "@rv94", vjust = -0.4, hjust = 0.2), colour = 'white', size = 9, alpha = 0.4) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)))

#Save entire plot
animate(NLPlot + transition_reveal(Pl), 
        width = 974, 
        height = 614, 
        nframes = 200, 
        fps = 15, 
        end_pause = 20,
        renderer = gifski_renderer('NLPlot.gif'))

#Save individual frames
animate(NLPlot + transition_reveal(Pl), 
        nframes = 50,  
        width = 974, 
        height = 614, 
        device = 'png',
        renderer = file_renderer( prefix = 'NLPlot',
                                  overwrite = T))

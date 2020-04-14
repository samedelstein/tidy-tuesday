library(tidyverse)
library(ggplot2)
library(gridExtra)

#introduce black theme (source: https://gist.github.com/jslefche/eff85ef06b4705e6efbc)
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.spacing = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

#We awarded 10 points for first ranked track, eight points for second ranked track, 
#and so on down to two points for fifth place. The song with the most points won. 
#We split ties by the total number of votes: songs with more votes ranked higher. 
#Any ties remaining after this were split by first place votes, followed by second place votes and so on: 
#songs with more critics placing them at higher up the lists up ranked higher.


polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

#Ranking By Year
byYear <- polls %>%
  mutate(points = case_when(rank == 1 ~ 10,
                            rank == 2 ~ 8,
                            rank == 3 ~ 6,
                            rank == 4 ~ 4,
                            rank == 5 ~ 2)) %>%
  group_by(title, year, gender) %>%
  summarise(sum_points = sum(points)) %>%
  arrange(-sum_points) %>%
  ggplot(aes(year, sum_points)) +
  geom_point(position = "jitter")  +
  gghighlight::gghighlight(sum_points > 90,  label_key = title) +
  labs(title = "Top Rated Songs",
       x = "Year",
       y= "Total Points") +
  theme_black()
byYear


#Ranking for collaborations
collaboration <- polls %>%
  mutate(first_artist = gsub( " ft.*$", "", artist ), #find if an artist had a collaborator
         first_artist = gsub( " &.*$", "", first_artist ), 
         collaboration = case_when(str_detect(artist, "ft") ~ "Yes",
                                   str_detect(artist, "&") ~ "Yes",
                                   TRUE ~ "No"),
         points = case_when(rank == 1 ~ 10, #assign points based on grading rubric
                            rank == 2 ~ 8,
                            rank == 3 ~ 6,
                            rank == 4 ~ 4,
                            rank == 5 ~ 2)) %>%
  group_by(first_artist, collaboration) %>%
  summarise(sum_points = sum(points),
            n = n(),
            avg_points = sum_points/n) %>%
  group_by(first_artist) %>% filter(n() >1) %>% #only find artists with both songs on their own and songs with collaboration
  arrange(-avg_points) %>%
  ggplot(aes(avg_points, first_artist, color = collaboration)) +
  geom_line(aes(group = first_artist)) +
  geom_point() +
  scale_color_manual(values = c("red", "green"), name = "Artist Collaboration?") +
  labs(title = "Do artists get higher rankings when they collaborate?",
       y = "",
       x = "Average Points Rating") +
  theme_black()
collaboration

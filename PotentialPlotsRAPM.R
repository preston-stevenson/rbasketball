library(tidyverse)
library(ggplot2)
library(showtext)
library(ggbeeswarm)

# Add your chosen Google font
#font_add_google("Poppins")
rapm <- read.csv('rapm_data.csv')

# Update theme
themes <- function() { 
  theme_minimal(base_size = 9, base_family = "Arial") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

rapm %>%
  ggplot(aes(x = season, y = RAPM, group = full_name)) +
  geom_line()

playerid = 203999

rapm %>%
  filter(selection =="5 Year") %>%
  ggplot(aes(x = season, y = RAPM, group = full_name)) +
  geom_line(data = rapm %>% filter(id == playerid, selection == '5 Year'), size = 2, color = "skyblue") +  # Highlighted player
  geom_line(data = rapm %>% filter(id != playerid, selection == '5 Year'), alpha = 0.15) +  # Other players with transparency
  theme_minimal() +
  labs(title = "Player RAPM Over Seasons",
       x = "Season",
       y = "RAPM",
       color = "Player") +
  scale_color_viridis_d() 




name = "Matisse Thybulle"

rapm %>%
  filter(selection == '3 Year') %>%
  ggplot(aes(x = factor(season), y = RAPM)) +  # Using 'factor' to treat seasons as categorical
  geom_boxplot(aes(group = season), alpha = 0.5, fill = "floralwhite", color = "#001F3F") +  # Boxplots for all players
  geom_point(data = rapm %>% filter(full_name == name, 
                                    selection == '3 Year'), 
             aes(x = season, y = RAPM), color = "#C04000", size = 2.5) + 
  geom_line(data = rapm %>% filter(full_name == name, selection == '3 Year'), 
            aes(x = season, y = RAPM), group = name, 
            color = "#C04000", size = .7, linetype = "dashed", alpha = .8) + 
# Highlighted player's points
  scale_y_continuous(breaks = seq(-6, 8, by = 2)) +
  themes() +
  labs(title = "RAPM Distribution by Season",
       x = "Season",
       y = "RAPM",
       subtitle = paste("Player:", name)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

rapm %>%
  filter(selection == '3 Year') %>%
  ggplot(aes(x = factor(season), y = RAPM, group = 1)) +  # Line graph treats seasons as categorical
  geom_line(aes(group = full_name), color = "gray80", size = 0.5, alpha = 0.2) +
  geom_line(aes(group = season), color = "floralwhite", size = 0.2, alpha = 0.4) +  # General background trend
  geom_point(data = rapm %>% filter(full_name == name, selection == '3 Year'), 
             aes(x = season, y = RAPM), color = "#C04000", size = 2.5) + 
  geom_line(data = rapm %>% filter(full_name == name, selection == '3 Year'), 
            aes(x = season, y = RAPM), color = "#C04000", size = 0.8, linetype = "dashed", alpha = .8) + 
  scale_y_continuous(breaks = seq(-6, 8, by = 2)) +  # Add more y-axis ticks
  themes() +
  labs(title = "RAPM Distribution by Season",
       x = "Season",
       y = "RAPM",
       subtitle = paste("Player:", name)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "#001F3F"),
    axis.title = element_text(size = 12, color = "#001F3F"), 
    plot.title = element_text(size = 20, hjust = 0.5, color = "#001F3F"),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#001F3F"),
  )



# ,
# # Add the plotOutput for the boxplot chart
# plotOutput("boxplot_chart", height = "400px") 

# output$boxplot_chart <- renderPlot({
#  selection1
# %>%
#     filter(selection == input$radio) %>%
#     ggplot(aes(x = factor(Season), y = RAPM)) +  # Using 'factor' to treat seasons as categorical
#     geom_boxplot(aes(group = Season), alpha = 0.5, fill = "floralwhite", color = "#001F3F") +  # Boxplots for all players
#     geom_point(data = rapm %>% filter(Name == selection1, 
#                                       Selection == input$radio), 
#                aes(x = Season, y = RAPM), color = "#C04000", size = 3) +  # Highlighted player's points
#     themes() +
#     labs(title = "RAPM Distribution by Season",
#          x = "Season",
#          y = "RAPM",
#          subtitle = paste("Player:", selection1)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#})

rapm <- rapm %>% 
  select(Name = full_name,
         RAPM, 
         'RAPM Rank' = RAPM_Rank,
         ORAPM = RAPM__Off,
         'ORAPM Rank' = RAPM__Off_Rank,
         DRAPM = RAPM__Def,
         'DRAPM Rank' = RAPM__Def_Rank,
         Selection = selection,
         Season = season,
         ID = id)

var1 <- '3 Year'
player_name <- 'Matisse Thybulle'


rapm %>%
  filter(Selection == var1) %>%
  ggplot(aes(x = factor(Season), y = RAPM)) +  # Using 'factor' to treat seasons as categorical
  geom_quasirandom(size = 1.5, alpha = 0.4, method = "smiley", color = "#001F3F") +   # Boxplots for all players
  geom_point(data = rapm %>% filter(Name == player_name, Selection == var1),  # Highlight specific player
             aes(x = factor(Season), y = RAPM), 
             color = "#C04000", size = 3.75) +  
  geom_line(data = rapm %>% filter(Name == player_name, Selection == var1), 
            aes(x = factor(Season), y = RAPM), group = player_name, 
            color = "#C04000", size = .7, linetype = "dashed", alpha = .8) + 
  # Highlighted player's points
  scale_y_continuous(breaks = seq(-6, 8, by = 2)) +# Highlighted player's points
  themes() +
  labs(title = "RAPM Distribution by Season",
       x = "Season",
       y = "RAPM",
       subtitle = paste(player_name)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "#001F3F"),  # X-axis text size and color
    axis.text.y = element_text(size = 12, color = "#001F3F"),  # Y-axis text size and color
    axis.title.x = element_text(size = 12, color = "#001F3F"),  # X-axis title size and color
    axis.title.y = element_text(size = 12, color = "#001F3F"),  # Y-axis title size and color
    plot.title = element_text(size = 20, color = "#001F3F", hjust = 0.5),  # Title size, color, and alignment
    plot.subtitle = element_text(size = 18, color = "#001F3F", hjust = 0.5)) 



#aes(color = RAPM), 

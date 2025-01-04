### RAPM and other things app

#### Packages for App ####
library(knitr)
library(DT)
library(colorRamps)
library(tidyverse)
library(colorspace)
#library(ggrepel)
library(rsconnect)
#library(kableExtra)
library(markdown)
library(htmlwidgets)
library(ggbeeswarm) 
#library(showtext)
#library(gt)

#rsconnect::deployApp('/Users/prestonstevenson/Documents/Projects/Basketball/rapm/data')
#setwd("~/Documents/Projects/Basketball/rapm/data")

drawCallback <- htmlwidgets::JS(
  "function (oSettings, json) {
        $('.dt-center').each(function(i) {
          var color = $(this).css('background-color');
          $(this).attr('style', 'background-color: '+color+' !important');
   })}"
)

#font_add_google("Poppins")
rapm <- read.csv('rapm_data.csv')

# set themew
theme_preston <- function () { 
  theme_minimal(base_size=9, base_family="mono") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

# Update theme
themes <- function() { 
  theme_minimal(base_size = 9, base_family = "roboto") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}



###look to include some box score stats and regular and or advanced stats in player bios maybe

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
brks_1 <- quantile(rapm$RAPM, probs = seq(.05, .95, .1), na.rm = TRUE)
clrs_1 <- sequential_hcl(length(brks_1) + 1, h = c(250,350), c = c(60,60), l = 58)

brks_2 <- quantile(rapm$ORAPM, probs = seq(.05, .95, .1), na.rm = TRUE)
clrs_2 <- sequential_hcl(length(brks_2) + 1, h = c(250,350), c = c(60,60), l = 58)

brks_3 <- quantile(rapm$DRAPM, probs = seq(.05, .95, .1), na.rm = TRUE)
clrs_3 <- sequential_hcl(length(brks_3) + 1, h = c(250,350), c = c(60,60), l = 58)


#### SETUP APPLICATION ####
ui <- fluidPage(
  tags$style(HTML("
  hr {
      border: 1px solid #1f2024;  /* Dark color for the line */
      margin-top: 20px;  /* Space above the line */
      margin-bottom: 20px;  /* Space below the line */
    }
    body {
      background-color: floralwhite;  /* Set body background to floralwhite */
    }
    .navbar {
      background-color: floralwhite;  /* Set navbar background to floralwhite */
      border-color: #DCDCDC;  /* Light gray border for the navbar */
    }
    .navbar-nav > li > a {
      color: #000033 !important;  /* Set navbar item font color (navy) */
    }
    .navbar-nav > li > a:hover {
      color: #8B4513 !important;  /* Change color on hover (e.g., terracotta) */
    }
    .navbar-header .navbar-brand {
      color: #000033 !important;  /* Set navbar title font color to navy */
    }
    .navbar-header .navbar-brand:hover {
      color: #000033 !important;  /* Darker navy on hover for title */
    }
    .container-fluid {
      background-color: floralwhite;  /* Set the container background to floralwhite */
    }
    .tab-content {
      background-color: floralwhite;  /* Set background for tab content */
    }
    .shiny-output-error {
      background-color: floralwhite;  /* Background color for error messages */
    }
    .shiny-output-error-text {
      color: #1f2024;  /* Error text color */
    }
  ")),
  # tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {font-size:125% !important; background-color: unset !important;}')),
  navbarPage(
    "Explore Basketball",
    tabPanel(
      "Overview",
      mainPanel(
        includeMarkdown("Overview.Rmd")
      )
    ),
    tabPanel(
      "RAPM",
      h5(a("Built by: @prestons2023",
        style = "color: #1f2024",
        href = "https://twitter.com/PrestonS2023",
        target = "_blank"
      )),
      downloadButton("download_rapm", "Download Data"),
      verticalLayout(
        radioButtons(
          inputId = "radio",
          label = "Number of Years",
          choices = c('5 Year', '3 Year', '1 Year'),
          inline = TRUE
        ),
        radioButtons(
          inputId = "radio1",
          label = "Season",
          choices = c('2024','2023', '2022', '2021',
                      '2020', '2019', '2018', '2017'),
          inline = TRUE
        ),
        # h6("Notes: Season indicates beginning of season (2024 is for 2024-25).",
        #    style = "color: #1f2024"
        # ),
        # h6("5 year goes back to 2019. 3 year goes back to 2017. Single year is not prior informed (yet).",
        #    style = "color: #1f2024"
        # ),
        h6(HTML("Notes: Season indicates beginning of season (2024 is for 2024-25).<br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;5 year goes back to 2019. 3 year goes back to 2017. Single year is not prior informed (yet)."),
           style = "color: #1f2024"
        ),
        
        DT::dataTableOutput("table"),
        hr(),
        
        # Add the plotOutput for the boxplot chart
        plotOutput("boxplot_chart", height = "400px")
      )
    )))
    


#### RENDER APPLICATION ####
server <- function(input, output, session) {


  #### Talent Tables ####
  output$download_rapm <- downloadHandler(
    filename <- function() {
      paste0("rapm.csv")
    },
    content <- function(file) {
      write.csv(rapm, file, row.names = FALSE)
    }
  )
  
  
  output$table <- renderDataTable({
    datatable(rapm %>% 
                select(-ID) %>%
        filter(Selection == input$radio) %>%
        filter(Season == input$radio1) %>%
          arrange(desc(RAPM))
       ,
       filter = "top",
    rownames = FALSE,
    selection = list(mode = "single", target = "row", selected = c(1)),
    options = list(
      drawCallback = drawCallback,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )) %>%
     formatStyle("RAPM",
                 backgroundColor = styleInterval(brks_1, clrs_1),
                 color = "white"
     ) %>%
     formatStyle("ORAPM",
                 backgroundColor = styleInterval(brks_2, clrs_2),
                 color = "white"
     ) %>%
      formatStyle("DRAPM",
                  backgroundColor = styleInterval(brks_3, clrs_3),
                  color = "white"
     )
  })
  
  # Reactive to get the correct selected player after filtering
  selected_player <- reactive({
    filtered_data <- rapm %>%
      select(-ID) %>%
      filter(Selection == input$radio) %>%
      filter(Season == input$radio1) %>%
      arrange(desc(RAPM))

    selected_row <- input$table_rows_selected

    if (length(selected_row) > 0) {
      return(filtered_data[selected_row, "Name"])
    }
    return(NULL)
  })
   
  output$boxplot_chart <- renderPlot({
    player_name <- selected_player()  # Get the selected player name

    if (is.null(player_name)) {
      return(NULL)  # No player selected, return nothing
    }

   rapm %>%
     filter(Selection == input$radio) %>%
     ggplot(aes(x = factor(Season), y = RAPM)) +  # Using 'factor' to treat seasons as categorical
     geom_quasirandom(size = 1.5, alpha = 0.4, method = "smiley", color = "#001F3F") +   # Boxplots for all players
     geom_point(data = rapm %>% filter(Name == player_name, Selection == input$radio),  # Highlight specific player
                aes(x = factor(Season), y = RAPM), 
                color = "#C04000", size = 3.75) +  
     geom_line(data = rapm %>% filter(Name == player_name, Selection == input$radio), 
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
       plot.subtitle = element_text(size = 18, color = "#001F3F", hjust = 0.5)  # Subtitle size and color
     )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

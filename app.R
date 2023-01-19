#Katarzyna Borkowska WCY20IJ2S1
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(dplyr)
library(lattice)
library(tidyr)
library(plotly)

data <- read.csv("christmas_movies.csv", header = TRUE)

#przygotowanie danych

#usuwanie duplikatów
data <- distinct(data)

data<- data %>% select(-"description",-"img_src") # usuwanie danych niepotrzebnych do analizy

# usuń niecyfrowe znaki i zostaw tylko liczby
data$release_year <- gsub("\\D", "", data$release_year)
data$release_year <- as.integer( data$release_year )

# Usun wiersze, w których wartość w kolumnie "release_year" jest większa niż 2022
data <- data %>%
  filter(release_year <= 2022)


data$votes <- gsub(",", "", data$votes) # zamiana na liczby
data$votes <- as.numeric( data$votes )


data$gross <- gsub("[$M]", "", data$gross)  # usuń znaki dolara i M
data$gross <- as.numeric(data$gross)  # zamień na liczbę zmiennoprzecinkową

data$gross <- ifelse(is.na(data$gross), 0, data$gross)

data$genre <- lapply(data$genre, function(x) unlist(strsplit(x, ", "))) # tworzę tablicę gatunków filmów z których składa się film

data$stars <- lapply(data$stars, function(x) unlist(strsplit(x, ", "))) # tworzę listę aktorów którzy grają w filmach


data <- data %>% filter(!is.na(data$imdb_rating)) # usuwanie gdzie nie ma wartości
data <- data %>% filter(!is.na(data$votes)) # usuwanie gdzie nie ma wartości


#data
#str(data)

# przygotowanie szczegółowych danych do wykresów

# Policz ilość wystąpień każdego elementu w kolumnie "genre"
data_genre <- data %>% 
  unnest(genre) %>%  # Rozwiń listy w kolumnie "genre" na osobne wiersze
  count(genre)       # Policz ilość wystąpień każdego elementu


#zastąpienie pustych miejsc
data <- data %>%
  mutate(rating = ifelse(is.na(rating) | rating == "", "Not Rated", rating))



# Wczytaj plik CSV do obiektu danych w R
ui <- navbarPage(
  
  titlePanel("Filmy świąteczne - Katarzyna Borkowska WCY20IJ2S1"),# wyświetlenie tytułu
  # wybieranie filmu wg czasu trwania, a na wykresie przedstawiamy ocene
  tabPanel("Liczba głosów a ocena",
           fluidPage(
             titlePanel("Czas i oceny filmów"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("slider3", "Wybierz liczbę ocen filmu:", min = 0, max = 900000, value = c(60, 100), step = 5),
                 sliderInput("slider4", "Wybierz oceny imdb które cię interesują:", min = 0, max = 10, value = c(6, 8.5), step = 0.1)
               ),
               mainPanel(
                 plotOutput("plot4"), # utworzenie miejsca na wykres
               )
             )
           )
  ),
  #--------------------------------------------
  tabPanel("Reżyser i jego filmy",
           fluidPage(
             titlePanel("Reżyser i jego filmy"), # Tytuł aplikacji
             sidebarLayout(
               sidebarPanel(
                 selectInput("wybor", "Wybierz reżysera:", sort(unique(data$director)), selected = "Chris Columbus")
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("punktowy", plotOutput("punktowy1", click = "plot_click1"),
                             verbatimTextOutput("info1")),
                             tabPanel("tekstowy", plotOutput("tekstowy1")),
                             tabPanel("zysk", plotOutput("zysk"))
                 )
               )  
             )
           )
  ),
  #--------------------------------------------
  tabPanel("Czas i oceny filmów",
           fluidPage(
             titlePanel("Czas i oceny filmów"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("slider", "Wybierz ile może trawć film świąteczny:", min = 5, max = 200, value = c(60, 100), step = 5),
                 sliderInput("slider2", "Wybierz oceny imdb które cię interesują:", min = 0, max = 10, value = c(6, 8.5), step = 0.1)
               ),
               mainPanel(
                 plotOutput("plot_co", click = "plot_click_co"),
                 verbatimTextOutput("info_co"), # utworzenie miejsca na wykres
               )
             )
           )
  ),
  #--------------------------------------------
  tabPanel("Gatunki filmów",
           fluidPage(
             titlePanel("Gatunki filmów"), # Tytuł aplikacji
               mainPanel(
                 plotOutput("plotG", width = "150%"), # utworzenie miejsca na wykres
               )  
             
           )
  ),
  #--------------------------------------------
  tabPanel("Kategorie wiekowe",
           fluidPage(
             titlePanel("Kategorie wiekowe"), 
             sidebarLayout(
               sidebarPanel(
                 radioButtons("type", label = h3("Wybierz typ wykresu"), 
                              choices = list(
                                "pie" = 1,
                                "hist"  = 2),
                              selected = 1
                 )
               ),
               mainPanel(
                 plotOutput("plotW")
               )  
             ) 
           )
  ),
  #--------------------------------------------
  tabPanel("Statysytczne oceny w latach",
           fluidPage(
             titlePanel("Statysytczne oceny w latach"), # Tytuł aplikacji
             
             fluidRow(
               mainPanel(plotOutput("plotB", click = "plot_click_B", width = "150%"),
                         verbatimTextOutput("info_B"), # utworzenie miejsca na wykres
               )
             )
           )
  )
)

server <- function(input, output){
  #-----------------------111111111111---------------------------
  output$plot4 <- renderPlot({
    min <- input$slider3[1]
    max <- input$slider3[2]
    min2 <- input$slider4[1]
    max2 <- input$slider4[2]
    plot(data$votes, data$imdb_rating, main = "Wykres", xlab = "Liczba głosów", ylab = "Ocena IMDB", type = "p", xlim = c(min, max), ylim = c(min2, max2))
  })
  
  #------------------------2222222222222222--------------------------
  
  output$punktowy1 <- renderPlot({
    selected_rows <- data %>% filter(director == input$wybor)
    selected_rows$release_year_int <- sort(as.integer(selected_rows$release_year))
    x_min <- min(selected_rows$release_year_int)
    x_max <- max(selected_rows$release_year_int)
    y_min <- min(selected_rows$imdb_rating)
    y_max <- max(selected_rows$imdb_rating)
    plot(selected_rows$release_year_int, selected_rows$imdb_rating , type ="p", main = "Wykres", xlab = "Rok wydania filmu", ylab = "Ocena IMDB",
         xlim = c(x_min, x_max), ylim = c(y_min, y_max))
  })
  
  
  output$info1 <- renderPrint({
    if (exists("plot_click")) {
      print(paste("Rok wydania filmu:", round(input$plot_click1$x)))
      print(paste("Ocena IMDB:", round(input$plot_click1$y,1)))
    } else {
      print("Kliknij na punkt aby wyświetlić szczegóły.")
    }
  })
  
  observeEvent(input$plot_click1, {
    output$info1 <- renderPrint({
      print(paste("Rok wydania filmu:", round(input$plot_click1$x)))
      print(paste("Ocena IMDB:", round(input$plot_click1$y, 1)))
    })
  })
  
  output$tekstowy1 <- renderPlot({
    selected_rows <- data %>% filter(director == input$wybor)
    selected_rows$release_year_int <- sort(as.integer(selected_rows$release_year))
    x_min <- min(selected_rows$release_year_int)
    x_max <- max(selected_rows$release_year_int)
    y_min <- min(selected_rows$imdb_rating)
    y_max <- max(selected_rows$imdb_rating)
    p <- plot(selected_rows$release_year_int, selected_rows$imdb_rating, pch ="" ,type ="p", main = "Wykres", xlab = "Rok wydania filmu", ylab = "Ocena IMDB",
              xlim = c(x_min, x_max), ylim = c(y_min, y_max))
    
    p <- p + text(selected_rows$release_year_int, selected_rows$imdb_rating,  labels = selected_rows$title, cex = 0.7)
    
  })
  
  output$zysk <- renderPlot({
    selected_rows <- data %>% filter(director == input$wybor)
    selected_rows$release_year_int <- sort(as.integer(selected_rows$release_year))
    plot(selected_rows$release_year_int, selected_rows$gross, type = "l", xlab = "Rok wydania", ylab = "Zysk")
    
    # Dodajemy punkty do wykresu
    points(selected_rows$release_year_int, selected_rows$gross, col = "red", pch = 19)
    
  })
  
  
  #-------------------------3333333333333333333333-------------
  
  output$plot_co <- renderPlot({
    min <- input$slider[1]
    max <- input$slider[2]
    min2 <- input$slider2[1]
    max2 <- input$slider2[2]
    
    
    
    plot(data$runtime, data$imdb_rating, main = "Wykres", xlab = "Czas trwania filmu", ylab = "Ocena IMDB", xlim = c(min, max), ylim = c(min2, max2))
    
  })
  
  output$info_co <- renderPrint({
    if (exists("plot_click_co")) {
      print(paste("Czas trwania filmu:", round(input$plot_click_co$x)))
      print(paste("Ocena IMDB:", round(input$plot_click_co$y, 1)))
    } else {
      print("Kliknij na punkt aby wyświetlić szczegóły.")
    }
  })
  
  observeEvent(input$plot_click_co, {
    output$info_co <- renderPrint({
      print(paste("Czas trwania filmu:", round(input$plot_click_co$x)))
      print(paste("Ocena IMDB:", round(input$plot_click_co$y, 1)))
      selected_row <- data[data$runtime == round(input$plot_click_co$x) & data$imdb_rating == round(input$plot_click_co$y, 1)& !is.na(data$title), ]
      selected_row <- selected_row %>% filter(!is.na(selected_row$title)) # usuwanie gdzie nie ma wartości
      print("Tytuł[y]:")
      if (nrow(selected_row) > 0) {
        print(paste(selected_row$title))
      } else {
        print("Brak informacji o tytule dla wybranego punktu")
      }
    })
  })
  
  #------------444444444444444444--------------------------
  
  output$plotG <- renderPlot({
    data_genre <- data_genre %>%
      mutate(genre = reorder(genre, n))
    ggplot(data_genre, aes(x = genre, y = n)) +
      geom_col() +
      coord_flip() +
      xlab("Gatunek filmu") +
      ylab("Ilość filmów")
    
  })
  
  #------------55555555555555555------------------
  output$plotW <- renderPlot({
    
    data_rating <- data %>%
      unnest(rating) %>%  
      count(rating) 
    
    data_rating <- data_rating %>%
      arrange(desc(n))
    
    if(input$type == 1){
      ggplot(data_rating, aes(x = "", y = n, fill = rating)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme(legend.position = "right")
    }
    else{
      ggplot(data_rating, aes(x = reorder(rating, n, FUN = desc), fill = rating, y = n)) +
        geom_col() +
        xlab("Kategoria wiekowa") + ylab(" Ilość filmów") +
        theme(legend.position = "right",
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
  })
  
  #------------6666666666666-------------------
  
  output$plotB <- renderPlot({
    
    boxplot(imdb_rating ~ release_year, data = data, vertical = TRUE, las = 2, xlab = "Rok premiery", ylab = "Ocena widzów")
    
  })
  
  output$info_B <- renderPrint({
    if (exists("plot_click_B")) {
      print(paste("Ocena IMDB:", round(input$plot_click_B$y, 1)))
    } else {
      print("Kliknij na punkt aby wyświetlić szczegóły.")
    }
  })
  
  observeEvent(input$plot_click_B, {
    output$info_B <- renderPrint({
      print(paste("Ocena IMDB:", round(input$plot_click_B$y, 1)))
    })
  })
 }

shinyApp(ui = ui, server = server)

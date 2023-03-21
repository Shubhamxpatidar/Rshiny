install.packages("vctrs", dependencies = TRUE, INSTALL_opts = "--no-lock")
install.packages("dbplyr", dependencies = TRUE, INSTALL_opts = "--no-lock")
install.packages("hms", dependencies = TRUE, INSTALL_opts = "--no-lock")


install.packages(c("shiny", "tidyverse", "wordcloud2", "tm", "tidytext", "stopwords", "leaflet", "ggpubr", "ggthemes", "vctrs"))
library(shiny)
library(tidyverse)
library(wordcloud2)
library(tm)
library(tidytext)
library(stopwords)
library(leaflet)
library(ggpubr)
library(ggthemes)

# Load dataset
listings <- read_csv("/Users/shubhampatidar/Downloads/Data-Analysis-Of-Boston-Airbnb-Using-R-main/dataset/listings.csv")
listings$price <- as.numeric(gsub("\\$", "", listings$price))
listings <- listings %>% 
  mutate(
    income_monthly = round(price * (365 - availability_365) / 12),
    highly_available = (availability_365 >= 60)
  )

# Define a function to get vibe word dataframe
vibe_word_df <- function(df = listings, n_words) {
  df %>% 
    select(id, neighborhood_overview) %>% 
    unnest_tokens(word, neighborhood_overview) %>% 
    anti_join(stop_words, by = "word") %>% # Join with stop words dataset
    filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%  
    count(word, sort = TRUE) %>% 
    na.omit() %>%  # remove rows with missing values
    filter(n > n_words) %>% 
    mutate(word = reorder(word, n))
}

# Define UI for application
ui <- navbarPage(
  # Application title
  title = "Analysis of Airbnb Listings in Boston",
  
  # Add tab for map view
  tabPanel(
    "Map",
    leafletOutput("map", width = "75%", height = "700px"),
    absolutePanel(
      id = 'controls',
      class = "",
      fixed = TRUE,
      draggable = FALSE,
      top = 130,
      left = "76%",
      right = "auto",
      bottom = "auto",
      width = "auto",
      height = "700px",
      verbatimTextOutput(h1("nlist")),
      sliderInput(
        "price_slide",
        "Price",
        min = 0,
        max = 1000,
        value = c(0, 1000),
        step = 1
      ),
      sliderInput(
        "reviews",
        "Ratings",
        min = 0,
        max = 5,
        value = c(0, 5),
        step = 0.1
      ),
      plotOutput("no_lists", height = "150px"),
      plotOutput("price_d", height = "200px"),
      plotOutput("monthly_income", height = "200px")
    )
  ),
  
  # Add tab for neighbourhood vibe
  tabPanel(
    "Neighbourhood Vibe",
    sliderInput(
      "no_words",
      "No. of times a word appears",
      min = 10,
      max = 100,
      value = 50,
      step = 5
    ),
    wordcloud2Output("word_cloud", width = "100%", height = "600px")
  ),
  
  # Add tab for analysis
  tabPanel(
    "Analysis",
    splitLayout(
      plotOutput("rtype_plot", width = "100%"), 
      plotOutput("dens_plot2", width = "100%")
    )
  )
)

# Define server logic


# Add tab for neighbourhood vibe

# Define server logic

# Define server logic required to draw a histogram
server <- function(input, output) {
  n_df <- reactive({if(input$n_hood != 'All'){
    n_df <- listings %>% filter(neighbourhood_cleansed == input$n_hood
    )
  }
    else {
      n_df <- listings 
    }})
  
  df <- reactive({
    if(input$n_hood != 'All'){
      df <- listing_map_hood %>% filter(neighbourhood_cleansed == input$n_hood)
    }
    else {
      df <- listing_map 
    }
    df
  })
  
  
  
  
  
  
  
  l_map <- reactive({
    
    lmap <- n_df() %>% filter(
      review_scores_rating <= input$reviews[2],
      review_scores_rating >= input$reviews[1],
      price <= input$price_slide[2],
      price >= input$price_slide[1]) %>% 
      group_by(room_type) %>% 
      dplyr::summarise("nb_bnb" = n())
    lmap 
    
  })
  
  dense_plot <- function(dat, cont_col,fill_col){
    mean_dat <- mean(dat[[cont_col]],na.rm = T)
    g <- ggplot(data=dat) +
      geom_density(mapping = aes(x=dat[[cont_col]],fill = dat[[fill_col]]),alpha = 0.6,color = NA) +
      theme_pubclean() + theme(legend.title = element_blank())+ scale_fill_brewer(palette="Set1")
    return (g)
  }
  
  dense_plot_mean <- function(dat, cont_col){
    mean_dat <- mean(dat[[cont_col]],na.rm = T)
    g <- ggplot(data = dat) +
      geom_density(mapping = aes(x=dat[[cont_col]])) + 
      theme_pubclean() + theme(legend.position = "none") + 
      geom_vline(mapping = aes(xintercept = mean_dat),linetype = 'dashed')  + scale_fill_brewer(palette="Set1")
    return (g)
  }
  
  output$dens_plot2 <- renderPlot({
    ndf <- n_df() 
    g <- dense_plot(ndf,"price",fill_col =   "room_type") +
      labs(x="Price/night",  
           title = "Price Distribution"
      ) 
    g
  })
  
  output$word_cloud <- renderWordcloud2({
    set.seed(1234)
    wordcloud_df <- vibe_word_df(n_df(),n_words = input$no_words)
    wordcloud2(wordcloud_df,fontFamily = 'helvitica nue',font = 1,color = 'random-dark',size = 2)
  })
  
  output$price_d <- renderPlot({
    ndf <- n_df() %>% filter(
      review_scores_rating <= input$reviews[2],
      review_scores_rating >= input$reviews[1],
      price <= input$price_slide[2],
      price >= input$price_slide[1])
    g <-dense_plot_mean(dat = ndf,'price')       +
      labs(x="Price/night",  
           title = "Price Distribution"
      ) 
    g
    
  })
  
  output$no_lists <- renderPlot({
    
    g <- ggplot(data = l_map()) +
      geom_col(mapping = aes(x=reorder(room_type,-nb_bnb),y=nb_bnb, fill = factor(room_type))) + 
      theme_pubclean() + theme(legend.position = "none") + 
      labs(x="Room Type", y = "Number of listings", 
           title = "Number of listings per room type"
      )+ scale_fill_brewer(palette="Set1")
    g
    
  })
  
  output$rtype_plot <- renderPlot({
    g <- ggplot(data = df()) +
      geom_col(mapping = aes(x=reorder(room_type,-avg_price),y=avg_price, fill = factor(room_type))) + 
      theme_pubclean() + theme(legend.position = "none") + 
      labs(x="Room Type", y = "Average Price per day", 
           title = "Avg. Price of listings according to Room Type"
      ) + scale_fill_brewer(palette="Set1")
    g
  })
  
  
  output$map <- renderLeaflet({
    # Filtering according to review scores 
    n_df <- n_df() %>% filter(review_scores_rating <= input$reviews[2],
                              review_scores_rating >= input$reviews[1],
                              price <= input$price_slide[2],
                              price >= input$price_slide[1])
    
    leaflet(n_df) %>% addTiles() %>% 
      fitBounds(~min(longitude),~min(latitude),~max(longitude),~max(latitude)) %>% 
      addMarkers(
        clusterOptions = markerClusterOptions(), 
        popup = ~paste(
          "<b>",name,"</b><br/>",
          "Type: ",room_type,"<br/>",
          "Price: $",round(price),sep = "","<br/>",
          "Availability per year: ",round(availability_365)," days","<br/>"
        )
      ) %>% 
      addProviderTiles(providers$CartoDB.Positron)})
  
  output$no_bnb <- renderPlot({
    if (input$n_hood == 'All'){
      n_df <- n_df() %>% filter(
        `neighbourhood_cleansed` %in% n_hood
      ) %>% 
        group_by(`neighbourhood_cleansed`) %>% 
        dplyr::summarise(
          "nb_bnb" = n()
        )
      g <- ggplot(data = n_df) +
        geom_col(mapping = aes(x=reorder(`neighbourhood_cleansed`,-nb_bnb),y=`nb_bnb`, fill = factor(`neighbourhood_cleansed`))) + 
        theme_pubclean() + theme(legend.position = "none") + 
        labs(x="Neighbourhood", y = "No. of listings", 
             title = "No. of listings in each neighbourhood") 
      g
    }
    
  })
  
  output$monthly_income <- renderPlot({
    ndf <- n_df() %>% filter(
      review_scores_rating <= input$reviews[2],
      review_scores_rating >= input$reviews[1],
      price <= input$price_slide[2],
      price >= input$price_slide[1]
    )
    
    g <-  dense_plot_mean(dat = ndf,cont_col = "income_monthly")  +
      labs(x="Monthly Income",  
           title = "Monthly Income Distribution"
      ) 
    g
    
  })
  
  output$nlist <- renderPrint({
    n_df <-  n_df() %>% filter(
      review_scores_rating <= input$reviews[2],
      review_scores_rating >= input$reviews[1],
      price <= input$price_slide[2],
      price >= input$price_slide[1])
    n <-  paste("No. of listings: ",as.character((nrow(n_df))),sep = "")
    print (n)
    
  })
  
}
rsconnect::deployApp('Shiny_app.R')

# Run the application 
shinyApp(ui = ui, server = server)



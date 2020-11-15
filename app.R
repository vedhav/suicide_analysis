library(shiny)
library(shinyWidgets)
library(tidyverse)
library(tidycharts)

load("data.RData")

age_groups <- unique(data$Age) %>% sort() %>% as.character()
continents <- unique(data$Continent) %>% sort()
all_countries <- unique(data$Country) %>% sort()
continent_country_mapping <- data %>% select(Continent, Country) %>% distinct()

ui <- fluidPage(
  titlePanel("Suicide Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("age_group_in", "Age Groups", age_groups, age_groups, multiple = TRUE),
      selectInput("gender_in", "Gender", c("male", "female"), c("male", "female"), multiple = TRUE),
      pickerInput(
        "continent_in", "Continents", continents, continents, multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
      ),
      uiOutput("countries_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Happiness vs Suicides",
          fluidRow(
            column(12, plotlyOutput("corr_plot_ui_1", height = "90vh"))
          )
        ),
        tabPanel(
          "GDP vs Suicides",
          fluidRow(
            column(12, plotlyOutput("corr_plot_ui_2", height = "90vh"))
          )
        ),
        tabPanel(
          "Alcohol Consumption vs Suicides",
          fluidRow(
            column(12, plotlyOutput("corr_plot_ui_3", height = "90vh"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$continent_in, {
    current_continent <- continent_country_mapping %>% filter(Continent %in% input$continent_in)
    countries <- current_continent$Country %>% sort()
    output$countries_ui <- renderUI({
      pickerInput(
        "country_in", "Country", countries, countries, multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
      )
    })
  })
  output$happiness_plot_ui <- renderUI({
    plot_data <- happiness_data %>%
      filter(Country %in% input$country_in)
    if (nrow(plot_data) == 0) return()
    output$happiness_plot <- renderPlotly({
      bar_chart(
        plot_data, HappinessScore, Country,
        plot_title = "Happiness Score for various Countries",
        x_axis_title = "Happiness Score", y_axis_title = "Country"
      )
    })
    plotlyOutput("happiness_plot", height = nrow(plot_data) * 20 + 150)
  })
  output$corr_plot_ui_1 <- renderPlotly({
    filter_data <- data %>% filter(Age %in% input$age_group_in) %>%
      filter(Gender %in% input$gender_in) %>% left_join(consumption) %>% filter(Country %in% input$country_in) %>%
      # filter(Year == input$year_in) %>%
      left_join(happiness_data) %>% mutate(suicides_per_100k = 100000 / Population * Suicides) %>%
      group_by(Country, Continent, HappinessScore, Year) %>% summarise(suicides_per_100k = round(mean(suicides_per_100k), 2))
    # numeric_scatter_chart(filter_data, HappinessScore, suicides_per_100k, Country)
    if (nrow(filter_data) == 0) return()
    g <- crosstalk::SharedData$new(filter_data, ~Continent)
    gg <- ggplot(g, aes(HappinessScore, suicides_per_100k, color = Continent, frame = Year)) +
      geom_point(aes(ids = Country)) +
      geom_smooth(se = FALSE, method = "lm") +
      scale_x_log10()
    ggplotly(gg) %>%
      highlight("plotly_hover")
  })
  output$corr_plot_ui_2 <- renderPlotly({
    filter_data <- data %>% filter(Age %in% input$age_group_in) %>%
      filter(Gender %in% input$gender_in) %>% left_join(consumption) %>% filter(Country %in% input$country_in) %>%
      left_join(happiness_data) %>% mutate(suicides_per_100k = 100000 / Population * Suicides) %>%
      group_by(Country, Continent, GDPperCapita, Year) %>% summarise(suicides_per_100k = round(mean(suicides_per_100k), 2))
    if (nrow(filter_data) == 0) return()
    g <- crosstalk::SharedData$new(filter_data, ~Continent)
    gg <- ggplot(g, aes(GDPperCapita, suicides_per_100k, color = Continent, frame = Year)) +
      geom_point(aes(ids = Country)) +
      geom_smooth(se = FALSE, method = "lm") +
      scale_x_log10()
    ggplotly(gg) %>%
      highlight("plotly_hover")
  })
  output$corr_plot_ui_3 <- renderPlotly({
    filter_data <- data %>% filter(Age %in% input$age_group_in) %>%
      filter(Gender %in% input$gender_in) %>% left_join(consumption) %>% filter(Country %in% input$country_in) %>%
      left_join(happiness_data) %>% mutate(suicides_per_100k = 100000 / Population * Suicides) %>%
      group_by(Country, Continent, LitresPerCapita, Year) %>% summarise(suicides_per_100k = round(mean(suicides_per_100k), 2))
    if (nrow(filter_data) == 0) return()
    g <- crosstalk::SharedData$new(filter_data, ~Continent)
    gg <- ggplot(g, aes(LitresPerCapita, suicides_per_100k, color = Continent, frame = Year)) +
      geom_point(aes(ids = Country)) +
      geom_smooth(se = FALSE, method = "lm") +
      scale_x_log10()
    ggplotly(gg) %>%
      highlight("plotly_hover")
  })
}

shinyApp(ui, server)


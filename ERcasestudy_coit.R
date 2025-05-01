library(shiny)
library(vroom)
library(tidyverse)

# upload data used in app
injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

# set names for product codes 
prod_codes <- setNames(products$prod_code, products$title)

# function for selecting top 5 most common injuries/body parts/locations
# fct_infreq() orders factor by most frequent to least frequent
# fct_lump() assigns 'other' to all factors outside n most frequent
# returns five most frequent + 'other' and their counts
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  # allows user to select product of interest from 'products' by name
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    # allows user to select metric to display on plot
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  # output three tables all of same width
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  # output one plot showing injuries from product by age and sex
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  # produces one of the narratives upon selection of the button
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  # reactive that saves user product input for outputs
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # outputs 5 most frequent of the following columns for the user selected product
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  # reactive that saves the count and rate of injuries by age and sex
  # provides data for what is selected by user input option input$y
  summary <- reactive({
    selected() %>%
      # count produces 'n' column
      count(age, sex, wt = weight) %>%
      # adds total population of each age/sex category as 'population'
      left_join(population, by = c("age", "sex")) %>%
      # calulates rate and saves as new column
      mutate(rate = n / population * 1e4)
  })
  
  # creates output plot of age/sex count or rate by product type
  output$age_sex <- renderPlot({
    # for loop let's user decide on y-axis 
    if (input$y == "count") {
      summary() %>%
        # n column contains count
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    # in this case else just means input$y == 'rate'
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  # creates reactive only when user clicks button
  narrative_sample <- eventReactive(
    # this means that the 'selected' reactive is called when button clicked
    list(input$story, selected()),
    # narratives are pulled and sampled from selected product
    selected() %>% pull(narrative) %>% sample(1)
  )
  
  # creates the actual output text from narrative reactive
  output$narrative <- renderText(narrative_sample())
}

shinyApp(ui, server)

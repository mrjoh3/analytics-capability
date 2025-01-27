

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(dbplyr)
library(purrr)
library(tidyr)
library(pool)
library(DBI)
library(ggplot2)
library(ggradar)
library(randomNames)
library(glue)

library(djprshiny)
library(djprtheme)


# setup db connection
db_pool <- dbPool(
    drv = RSQLite::SQLite(),
    db = "capability.sqlite"
)

# close db connection on exit
onStop(function() {
    cat('Closing Pool')
    pool::poolClose(db_pool)
})



methods <- tribble(~ capability,                  ~target,
                   "Machine Learning",            0.50,
                   "statistical modelling",       0.75,
                   "Natural Language Processing", 0.3,
                   "Text Mining",                 0.75,
                   "Web scraping",                0.6,
                   "Package Development",         0.75,
                   "Shiny Applications",          0.75,
                   "Time Series",                 0.5,
                   "Experimental Design",         0.5,
                   "Spatial Analysis",            0.5) %>%
  mutate(type = 'methods')

technology <- tribble(~ capability,   ~target,
                     "gitlab",        1,
                     "ci/cd",         0.75,
                     "docker",        0.5,
                     "databricks",    0.5,
                     "postgresql",    0.5,
                     "DRA",           0.75,
                     "Windows VM",    0.3,
                     "Linux VM",      0.3,
                     "shiny server",  0.3,
                     "shinyproxy",    0.5) %>%
  mutate(type = 'technology')

domain <- tribble(~ capability,            ~target,
                  "econimics",          0.5,
                  "environment",  0.3,
                  "science",       0.3,
                  "psychology",     1,
                  "statistics",    0.5) %>%
  mutate(type = 'domain')

targets <- bind_rows(methods, technology, domain)

team_data <- tbl(db_pool, 'capability_matrix') %>%
    collect() %>%
  group_by(user) %>% # need to account for accidental multiple entries (need ui fix)
  filter(timestamp == max(timestamp)) %>%
  ungroup()


# create fake data set
# team_data <- expand.grid(user = randomNames::randomNames(10, which.names = 'first'),
#                          timestamp = Sys.time(),
#                          capability = targets$capability) %>%
#   left_join(targets %>% select(type, capability)) 
# 
# team_data$value <- sample(0:2, nrow(team_data), replace = TRUE, prob = c(.25, .5, .25))



scale_n <- length(unique(team_data$user)) * 2 # score if everyone is an expert

summary_data <- team_data %>%
    group_by(type, capability) %>%
    summarise(score = sum(as.numeric(value))) %>%
    ungroup() %>%
    mutate(score = score / scale_n) %>%
    left_join(targets) 

plot_data <- summary_data %>%
    pivot_longer(cols = c('score', 'target'), names_to = 'value_type', values_to = 'score') %>%
    split(f = as.factor(.$type)) %>%
    map(~ pivot_wider(.x, names_from = 'capability', values_from = 'score'))

gap_data <- summary_data %>%
    mutate(gap = target - score) %>%
    filter(gap < 0) %>%
    select(capability, gap) %>%
    arrange(gap)


add_buttons <- function(button){
    radioGroupButtons(
        inputId = tolower(gsub(' ', '-', button)),
        label = tools::toTitleCase(button), 
        # choices = c(`<i class='fa fa-stack-overflow'></i>` = 0, `<i class='fa fa-dice-one'></i>` = 1, 
        #             `<i class='fa fa-dice-two'></i>` = 2),
        choices = 0:2,
        justified = TRUE
    )
}

description_text <- glue('Scores are scaled by the potential of each team member to be an expert. ',
                        'Targets are based on the desired proportion of total expertise is needed. ',
                        'Therefore a gap is identified as the difference between a target and the scored epertise')
users_text <- glue('Currently {length(unique(team_data$user))} people have completed the capcbility survey.')

selecter_height <- 325
inner_height <- selecter_height - 44

# Define UI for application that draws a histogram
ui <- function() {
  djprshiny::djpr_page(
    title = shiny::HTML("DJPR<BR>Data Analytics Capabilities"),
    tabPanel(title = 'dashboard',
             tags$head(
               tags$style(paste0(".multi-wrapper {height: ", selecter_height, "px;}")),
               tags$style(paste0(
                 ".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: ",
                 inner_height,
                 "px;}"
               ))
             ),
             value = "tab-dash",
             br(),
             br(),
             br(),
             fluidRow(
               column(8,
                      tabBox(width = '100%', id = 'capabilities',
                             tabPanel('Methods',
                                      shiny::plotOutput('methods_plot')),
                             tabPanel('Technology',
                                      shiny::plotOutput('technology_plot')),
                             tabPanel('Domain',
                                      shiny::plotOutput('domain_plot'))
                      )
               ),
               column(4,
                      p(description_text),
                      p(users_text),
                      p('This analysis has identified the following training gaps:'),
                      shiny::tableOutput('gaps')
               )
             )),
    tabPanel(title = 'capability',
             value = "tab-capability",
             br(),
             br(),
             br(),
             tabBox(width = '100%', id = 'scores', 
                    tabPanel(title = 'Methods', width = '100%', #solidHeader = TRUE,
                             shiny::uiOutput('methods')
                    ),
                    tabPanel(title = 'Technology', width = '100%', #solidHeader = TRUE,
                             shiny::uiOutput('technology')
                    ),
                    tabPanel(title = 'Domain', width = '100%', #solidHeader = TRUE,
                             shiny::uiOutput('domain')
                    )
             ),
             textInput('user', '', width = '100%', placeholder = 'Who are you?'),
             actionBttn('submit', label = 'Submit', block = FALSE, style = 'material-flat', size = 'xs', color = 'primary'))

  )
}


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$gaps <- renderTable(
        gap_data
    )
    
    output$methods <- renderUI({
        tagList(
            purrr::map(methods %>% pull(capability),
                       ~ add_buttons(.x))
        )
    })
    
    output$technology <- renderUI({
        tagList(
            purrr::map(technology %>% pull(capability),
                       ~ add_buttons(.x))
        )
    })
    
    output$domain <- renderUI({
        tagList(
            purrr::map(domain %>% pull(capability),
                       ~ add_buttons(.x))
        )
    })
    
    out_df <- reactive({
        
        m_df <- t_df <- d_df <- NULL
        
        if (!is.null(input[[tolower(gsub(' ', '-', pull(methods[1, 'capability'])))]])) {
            m_df <- purrr::map_dfr(methods %>% pull(capability), ~ tibble(type = 'methods',
                                                                    capability = .x,
                                                                    value = input[[tolower(gsub(' ', '-', .x))]]))
        }
        if (!is.null(input[[tolower(gsub(' ', '-', pull(technology[1, 'capability'])))]])) {
            t_df <- purrr::map_dfr(technology %>% pull(capability), ~ tibble(type = 'technology',
                                                                       capability = .x,
                                                                       value = input[[tolower(gsub(' ', '-', .x))]]))
        }
        
        if (!is.null(input[[tolower(gsub(' ', '-', pull(domain[1, 'capability'])))]])) {
            d_df <- purrr::map_dfr(domain %>% pull(capability), ~ tibble(type = 'domain',
                                                                   capability = .x,
                                                                   value = input[[tolower(gsub(' ', '-', .x))]]))
        }
        
        bind_rows(m_df, t_df, d_df)

    })
    
    output$tab <- renderTable(out_df())
    
    # add all the radar plots
    Map(function(p){
        output[[paste0(p, '_plot')]] <- renderPlot({
            plot_data[[p]] %>%
                select(-type) %>%
                ggradar()
        })
    }, names(plot_data))
        
    
    
    
    observeEvent(input$submit, {

        # add user and time stamp
        to_db <- out_df() %>%
            mutate(timestamp = Sys.time(),
                   user = input$user) %>%
            select(user, timestamp, everything())

        DBI::dbWriteTable(db_pool, 'capability_matrix', to_db, append = TRUE, row.names = FALSE)

        shiny::showNotification('Your data has been uploaded', duration = 10, closeButton = TRUE)


    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

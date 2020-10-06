library(shiny)
library(tidyverse)
library(shinythemes)
source("functions/functions.R")

ui <- fluidPage(theme = shinytheme("sandstone"),
    
    # Application title
    titlePanel("Trajectory Draw"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        ############################## Beginning of sidebar ##############################
        sidebarPanel(
            
            p(
                "Enter the initial height and either component-wise initial launch state or magnitude-wise initial launch state."
            ),
            br(),
            ############################## Component-wise input ##############################
            numericInput(
                "initial_height",
                "Initial Height of Launch:",
                min = 0,
                max = 9999,
                value = 0
            ),
            br(),
            helpText(
                "Initial horizontal and vertical velocity:"
            ),
            numericInput(
                "initial_x_velocity",
                "Initial Horizontal Velocity:",
                min = 0,
                max = 9999,
                value = 20
            ),
            numericInput(
                "initial_y_velocity",
                "Initial Vertical Velocity:",
                min = 0,
                max = 9999,
                value = 20
            ),
            actionButton(
                "submit_by_component",
                "Submit by Components"
            ),
            br(),
            br(),
            br(),
            ############################## Magnitude-wise input ##############################
            # some text
            helpText(
                "Initial angle & velocity magnitude:"
            ),
            numericInput(
                "initial_velocity",
                "Initial Velocity:",
                min = 0,
                max = 9999,
                value = 50
            ),
            numericInput(
                "initial_angle",
                "Initial Launch Angle:",
                min = 0,
                max = 89.9,
                value = 45
            ),
            actionButton(
                "submit_by_magnitude",
                "Submit by Magnitude"
            )
        ),
        ##############################       Main Panel        ##############################
        mainPanel(
            # Show a plot of the generated distribution
            plotOutput(
                "trajectory_plot"
            ),
            
            textOutput("distance_text"),
            br(),
            textOutput("height_text")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # initialize trajectory path with NA, to be updated later via click
    trajectory_object <- reactiveVal(list(NA))
    
    # update trajectory path using magnitude-based inputs on magnitude-based click
    observeEvent(input$submit_by_magnitude, {
        magnitude_object <- 
            traject_by_component(
                input$initial_velocity*cos(input$initial_angle*pi/180),
                input$initial_velocity*sin(input$initial_angle*pi/180),
                input$initial_height
            )
        trajectory_object(magnitude_object)
    })
    
    # update trajectory path using component-based inputs on magnitude-based click
    observeEvent(input$submit_by_component, {
        component_object <- 
            traject_by_component(
                input$initial_x_velocity,
                input$initial_y_velocity,
                input$initial_height
            )  
        trajectory_object(component_object)
    })
    
    # if the plot is still initialized, plot something empty
    # Otherwise, draw the path
    output$trajectory_plot <- renderPlot({
        if (anyNA(trajectory_object())) {
            ggplot() + theme_void()
        } else {
            ggplot(
                data = tibble(
                    x = 0
                ),
                aes(x = x)
            ) +
            geom_point(
                data = tibble(
                    x = c(
                        0,
                        trajectory_object()[["H"]]["x"],
                        trajectory_object()[["R"]]["x"]),
                    y = c(
                        trajectory_object()[["h0"]],
                        trajectory_object()[["H"]]["y"],
                        trajectory_object()[["R"]]["y"])
                ),
                aes(x, y),
                size = 4
            ) +
            stat_function(fun = trajectory_object()[["form"]]) +
            coord_cartesian(
                xlim = c(0, ceiling(trajectory_object()[["R"]][["x"]])),
                ylim = c(0, ceiling(trajectory_object()[["H"]][["y"]]))
            ) +
            labs(
               x = NULL, 
               y = NULL
            ) +
            geom_hline(yintercept = 0, alpha = 1/2) +
            geom_vline(xintercept = 0, alpha = 1/2) +
            theme(
                panel.background = element_rect(fill = "white"),
                axis.text = element_text(size = 15),
                panel.grid.major = element_line(color = "grey92")
            )
        }
    })
    
    # summary of max distance reached
    output$distance_text <- renderText({
        if(anyNA(trajectory_object())) {
            ""
        } else {
            paste(
                "The maximum range reached was ",
                format(round(trajectory_object()[["R"]][["x"]], 2), nsmall = 2, big.mark = ","),
                "meters, which occurred ",
                format(round(trajectory_object()[["R"]][["t"]], 2), nsmall = 2, big.mark = ","),
                "seconds after launch."
            )  
        }
    })
    
    # summary of max height reached
    output$height_text <- renderText({
        if(anyNA(trajectory_object())) {
            ""
        } else {
            paste(
                "The maximum height reached was ",
                format(round(trajectory_object()[["H"]][["y"]], 2), nsmall = 2, big.mark = ","),
                "meters, which occurred ",
                format(round(trajectory_object()[["H"]][["t"]], 2), nsmall = 2, big.mark = ","),
                "seconds after launch."
            )  
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

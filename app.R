# Shiny App to Explore NEISS Injury Data Set
# Author: Nils Gehlenborg (nils@hms.harvard.edu), Eric Yang (ericyang@hms.harvard.edu)

library(shiny)
library(tidyverse)
library(lubridate)
library(bmi713neiss)
library(scales)

data <- neiss_2008_2018

ui <- fluidPage(
    
    titlePanel("Shiny NEISS Explorer"),

    tabsetPanel(
        tabPanel("Product Details",
            sidebarLayout(
                sidebarPanel(
                    selectInput( 
                        "product", 
                        "Product",          
                        choices=sort(unique(data$Product_1)),
                        multiple=FALSE,
                        selected=" GOLF CARTS"
                    ),
                    
                    textInput(
                        "narrative_filter",
                        "Filter Narratives By"
                    ),
        
                    sliderInput(
                        "age",
                        "Filter Age By",
                        min=0,
                        max=99,
                        value=c(0, 99)
                    ),

                    selectInput(
                        "bodypart",
                        "Filter Bodypart By",
                        choices=str_to_title(unique(data$Body_Part)),
                        multiple=T
                    ),
                    
                    selectInput(
                        "group_by",
                        "Group By",
                        choices=c("Month", "Week", "Day", "Weekday", "Age Group", "Body Part")
                    ),
                    
                    # Filter Years
                    checkboxGroupInput(
                        "years",
                        "Filter Year by",
                        choices=unique(data$Year),
                        selected=unique(data$Year),
                        inline=TRUE
                    ),
                    
                    radioButtons( 
                        "isSexStratified", 
                        "Stratify by Sex?", 
                        choiceNames=list( "Yes", "No" ), 
                        choiceValues=list(TRUE,FALSE),
                        selected=FALSE
                    ),
                    
                    radioButtons( 
                        "isDiagnosisStratified", 
                        "Stratify by Diagnosis?", 
                        choiceNames=list( "Yes", "No" ), 
                        choiceValues=list(TRUE,FALSE),
                        selected=FALSE
                    ),
                    
                    textOutput("summaryText")
                ),
        
                mainPanel(
                   h2("Case Summary"),
                   plotOutput("summaryPlot"),
                   
                   h2("Case Details"),
                   DT::dataTableOutput("injuryTable"),
                )
            )
        ),
        tabPanel("Product Comparison",
            sidebarLayout(
                sidebarPanel(
                    selectInput( 
                        "products", 
                        "Products",          
                        choices=sort(unique(data$Product_1)),
                        multiple=TRUE,
                        selected=c(" GOLF CARTS"," GROCERY OR SHOPPING CARTS")
                    ),
                    
                    checkboxInput( 
                        "isNormalizeProduct", 
                        "Normalize by Product?"
                    )
                ),
                
                mainPanel(
                    h2("Injuries by Year"),
                    plotOutput("yearPlot"),
                    
                    h2("Injuries by Body Part"),
                    plotOutput("bodyptPlot"),
                )
            )
        )
    )
)

server <- function(input, output) {
    
    print( "Loading data ..." )
    
    # lazy loading, first access will be slow
    data <- neiss_2008_2018  

    print( "Data loaded!" )
    print( dim(data) )

    print( "Adding dimensions ..." )

    # add columns to the tibble
    data <- data %>%
        mutate(Week = as.factor(week(ymd(Treatment_Date)))) %>%
        mutate(Weekday = factor(weekdays(ymd(Treatment_Date)),
                                   levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") ))
    
    
    print( "Dimensions added!" )
    print( dim(data) )
    
    # reactive expression to filter selected cases
    cases <- reactive({
        selection <- data %>%
            filter(Product_1 == input$product) %>%
            filter(stringr::str_detect(Narrative_1, toupper(input$narrative_filter)))

        selection <- selection %>%
            filter(Age >= input$age[1] ) %>%
            filter(Age <= input$age[2] )
        
        # Filter year
        selection <- selection %>%
            filter(Year %in% input$years)
        
        # only apply body part filter if user made a selection
        # do not filter if no body part was selected
        if ( !is.null( input$bodypart ) ) {
            selection <- selection %>% filter( Body_Part %in% str_to_upper(input$bodypart) )
        }
        
        selection
    })
    
    # reactive to filter on selected products and all years
    cases_year <- reactive({
        selection <- data %>%
            filter(Product_1 %in% input$products) %>%
            select(Product_1, Year) %>%
            group_by(Product_1,Year) %>%
            summarise(count=n()) 
        
        if(input$isNormalizeProduct) {
            selection <- selection %>%
                mutate(count=rescale(count,to=c(0,1)))
        }
        
        selection
    })
    
    # reactive to filter on selected products and body part
    cases_bdypt <- reactive({
        selection <- data %>%
            filter(Product_1 %in% input$products) %>%
            select(Product_1, Body_Part) %>%
            group_by(Product_1,Body_Part) %>%
            summarise(count=n()) 
        if(input$isNormalizeProduct) {
            selection <- selection %>%
                mutate(count=rescale(count,to=c(0,1)))
        }
        selection
    })
    
    # For plotting below:
    # Make y-axis labels integers using scale package
    # adapted from github/jhrcook/integer_breaks_ggplot2
    int_breaks <- function(n = 5, ...) {
        fxn <- function(x) {
            breaks <- floor(pretty(x, n, ...))
        }
        return(fxn)
    }
    
    output$summaryPlot <- renderPlot({
        #transform inputs back to data format
        if (input$group_by == "Body Part") {
            input_group = "Body_Part"
        } else if (input$group_by == "Age Group") {
            input_group = "Age_Group"
        } else {
            input_group = input$group_by
        }
        
        plot <- ggplot(cases()) +
            xlab( input_group ) +
            ylab( "Cases" )
        
        if ( input$isSexStratified ) {
            plot <- plot + 
                geom_bar(aes(x=.data[[input_group]], fill=as.factor(Sex) ), position="dodge") + 
                ggtitle( paste0( "Injuries by ", input_group, " and sex" ) )
        } else {
            plot <- plot + 
                geom_bar(aes(x=.data[[input_group]] )) +
                ggtitle( paste0( "Injuries by ", input_group ) )
        }
        
        if ( input$isDiagnosisStratified ) {
            plot <- plot + facet_wrap(as.factor(cases()$Diagnosis))
        }
        
        
        plot <- plot +
            scale_y_continuous(breaks = int_breaks())
        
        # Rotate X axis by 45 degrees
        plot <- plot +
            theme(axis.text.x = element_text(angle=45,hjust=1))
        
        plot
    })

    output$injuryTable <- DT::renderDataTable(
        cases() %>% select(Treatment_Date, Narrative_1), options=list(pageLength=5)
    )
    
    output$summaryText <- renderText(
        paste0( "Selected: ", dim( cases() )[1], "/", dim( data )[1], " cases" )
    )
    
    output$yearPlot <- renderPlot({
        plot <- ggplot(cases_year(),aes(x=Year, y=count, group=Product_1)) +
            xlab( "Year" ) +
            ylab( "Cases" ) +
            geom_line(aes(color=Product_1)) +
            scale_x_continuous(breaks=2008:2018) +
            theme(axis.text.x = element_text(angle=45,hjust=1), legend.position = "bottom",
                  legend.direction = "vertical") +
            guides(color = guide_legend(title.position = "left"))
        
        if(!input$isNormalizeProduct) {
            plot <- plot + scale_y_continuous(breaks = int_breaks())
        }
        plot 
    })
        
    output$bodyptPlot <- renderPlot({
        plot <- ggplot(cases_bdypt(),aes(x=Body_Part,y=count,group=Product_1)) +
            xlab( "Body Part" ) +
            ylab( "Cases" ) +
            geom_bar(stat="identity", position="dodge", aes(fill=Product_1)) +
            theme(axis.text.x = element_text(angle=45,hjust=1), legend.position = "bottom",
                 legend.direction = "vertical") +
            labs(fill='Product_1') + 
            guides(fill = guide_legend(title.position = "left"))
        
        if(!input$isNormalizeProduct) {
            plot <- plot + scale_y_continuous(breaks = int_breaks())
        }
        plot 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

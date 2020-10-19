#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load("knitr", "tidyverse", "magrittr", "kableExtra", "dplyr", "ggplot2", "shiny", "rsconnect")


### This is in the cleaning.rds file as it is faster to run and it requires less code in the Shiny App.
# ag_data <- read_csv("berries.csv", col_names = TRUE)
# colnames(ag_data)
# NA_cols <- which(sapply(ag_data, function(x)all(is.na(x))))
# ag_data %<>% select(-all_of(NA_cols))
# colnames(ag_data)
# uni_cols <- sapply(ag_data, function(x) length(unique(x)))
# ag_data %<>% subset(select = uni_cols > 1)
# ag_data %<>% select(-`State ANSI`)
# colnames(ag_data)
# ras <- ag_data %>% filter((Commodity=="RASPBERRIES") & (Period=="YEAR"))
# ras %<>% select(-c(Period, Commodity))
# colnames(ras)
# ras$'Domain' %>% unique
# d_total <- filter(ras, Domain=="TOTAL")
# d_total$`Data Item` %>% unique()
# d_total$`Domain Category` %>% unique()
# ras <- filter(ras, Domain!="TOTAL")
# ras %<>% separate(`Data Item`, c("Bearing", "Production"), sep = "-")
# ras %<>% separate(Domain, c("D_left", "D_right"), sep = ", ")
# ras[is.na(ras)] <- ""
# ras$Chemical_Type <- ifelse(ras$D_left == "FERTILIZER", ras$D_left, ras$D_right)
# ras %<>% separate(`Domain Category`, c("DC_left", "DC_right"), sep = ":")
# ras[is.na(ras)] <- ""
# ras %<>% rename(Chemicals = DC_right)
# ras %<>% separate(Bearing, c("Berry", "Bearing"), sep = ",")
# ras %<>% select(-Bearing)
# ras %<>% separate(Production, c("Production", "Measurement"), sep = ",")
# ras %<>% filter(Measurement == " MEASURED IN LB")
# ras %<>% select(-c(D_right, D_left, DC_left, Berry))
# kable(head(ras)) %>% kable_styling(font_size = 12)
# ras_eda <- ras %>% select(Year, State, Measurement, Chemical_Type, Value)
# ras_eda$Value %<>% as.numeric()
# Year <- ras_eda$Year
# State <- ras_eda$State
# cols <- c("Chemical Type", "Year", "State")

# write_rds(ras_eda, "cleaning.rds")


test <- read_rds("cleaning.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Raspberries"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10)
            ),

            fluidRow(
                selectInput("group", "Grouping Variable", col)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                plotOutput("distPlot")
            ),
            fluidRow(
                tableOutput("summary")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observe({print(input$num1)})
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- test$Value
        bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab = "Number of Lbs", main = "Histogram of Lbs")
    })

    output$summary <- renderTable({
        if(isTRUE(input$group=="Chemical Type")){
            table <- test %<>% group_by(Chemical_Type) %>% summarize(mean(Value, na.rm = TRUE))
        }
        if(isTRUE(input$group=="Year")){
            table <- test %<>% group_by(Year) %>% summarize(mean(Value, na.rm = TRUE))
        }
        if(isTRUE(input$group=="State")){
            table <- test %<>% group_by(State) %>% summarize(mean(Value, na.rm = TRUE))
        }
        table
    })
  
    
}

# Run the application
shinyApp(ui = ui, server = server)

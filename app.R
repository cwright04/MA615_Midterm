
source(file = "Wrangling.R", echo = FALSE)
CHEMICALS_MEASURED_IN_LB <- subset(CHEM_CLEAN2, Measurements =="MEASURED IN LB") 

Carcinogens <- subset(CHEMICALS_MEASURED_IN_LB, Carcinogen != "")


Chem_Car_Group <- subset(Carcinogens, Carcinogens$Value1 >0  )

Chem_Car_Group1 <- Chem_Car_Group %>% group_by(State,Year, Carcinogen) %>% summarise(Total_Carcinogen = sum(Value1),.groups = "keep" )


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Carcinogen by State"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("stateInput", "State",
                                    choices = unique(Chem_Car_Group1$State))
        ), 
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("carcinogenplot")
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    d <- reactive({
        filtered <-
            Chem_Car_Group1 %>%
            filter(State == input$stateInput)    
    })
    output$carcinogenplot <- renderPlot({
        
        ggplot(d(), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
            geom_point(shape=25, size=2, stroke = 1)  + xlim(1990, 2020) + ylim(0, 300000) + scale_color_manual(breaks = c("known","possible","probable"), values = c("firebrick","lightpink2","mistyrose3"))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

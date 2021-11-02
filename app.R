library(shiny)

source(file = "Wrangling.R", echo = FALSE)
source(file = "plots.R", echo=FALSE)


# Define UI for application that draws a scatterplot
ui <- fluidPage(

    # Application title
    titlePanel("Carcinogen by State"),

    # Sidebar with a drop down input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("stateInput", "State",
                                    choices = unique(Chem_Car_Group1$State))
            
        ), 
        # Show a plot of the generated trends
        mainPanel(
           plotOutput("carcinogenplot", brush="plot_brush"),
           tableOutput("data")
        )
    )
    
    
)


# Define server logic required to draw a scatterplot
server <- function(input, output) {
    d <- reactive({
        filtered <-
            Chem_Car_Group1 %>%
            filter(State == input$stateInput)    
    })
    l<-reactive({(t2) %>% filter(State==input$stateInput)})
    output$carcinogenplot <- renderPlot({
        
        ggplot(d(), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
            geom_point(shape=25, size=2, stroke = 1)  + xlim(1990, 2020) + ylim(0, 300000) + scale_color_manual(breaks = c("known","possible","probable"), values = c("firebrick","lightpink2","mistyrose3"))+ggtitle(input$stateInput)
        
    })
    
    output$data<-renderTable({
        brushedPoints(l()[2:6], input$plot_brush)})  
    
}

# Run the application 
shinyApp(ui = ui, server = server)






library(shiny)
source(file = "Wrangling.R", echo = FALSE)

CHEMICALS_MEASURED_IN_LB <- subset(CHEM_CLEAN2, Measurements =="MEASURED IN LB") 

Carcinogens <- subset(CHEMICALS_MEASURED_IN_LB, Carcinogen != "")


Chem_Car_Group <- subset(Carcinogens, Carcinogens$Value1 >0  )

Chem_Car_Group1 <- Chem_Car_Group %>% group_by(State,Year, Carcinogen) %>% summarise(Total_Carcinogen = sum(Value1),.groups = "keep" )





ggplot(subset(Chem_Car_Group1, State == "CALIFORNIA"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_line() + ggtitle("Total Carcinogens CA") + xlim(1990, 2020) + ylim(0, 300000) + scale_color_manual(breaks = c("known","possible","probable"), values = c("red","blue","green")) 



ui <- fluidPage(
  selectInput("state", label = "States", choices =unique(Chem_Car_Group1$State)),
  plotOutput("plot")
)

dataf = reactive({
  a = subset(Chem_Car_Group1, State == input)
  return(a)
})


server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(dataf(), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
      geom_line() + ggtitle(cat("Total Carcinogens Per State")) + xlim(1990, 2020) + ylim(0, 300000) + scale_color_manual(breaks = c("known","possible","probable"), values = c("red","blue","green")) 
  })
} 

shinyApp(ui, server)
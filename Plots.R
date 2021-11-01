library(usmap)
library(ggplot2)
library(knitr)


###########################################################################################################################
#Plot Bar chart displaying the difference in the amount of carcinogens

#Subset down the data into just measured in lb
CHEMICALS_MEASURED_IN_LB <- subset(CHEM_CLEAN2, Measurements =="MEASURED IN LB") 


#Create flags to sum up 
#Note:  this process should be turned into a function, however we ran out of time to get this completed at this point
CHEMICALS_MEASURED_IN_LB$Carcinogen_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Carcinogen != "" & 
is.na(CHEMICALS_MEASURED_IN_LB$Carcinogen) != "TRUE" , 1, 0)

CHEMICALS_MEASURED_IN_LB$Hormone_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Hormone.Disruptor != "" & 
is.na(CHEMICALS_MEASURED_IN_LB$Hormone.Disruptor) != "TRUE" , 1, 0)

CHEMICALS_MEASURED_IN_LB$Neurotoxin_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Neurotoxins != "" & is.na(CHEMICALS_MEASURED_IN_LB$Neurotoxins) != "TRUE" , 1, 0)

CHEMICALS_MEASURED_IN_LB$Reprod_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Developmental.or.Reproductive.Toxins != "" & is.na(CHEMICALS_MEASURED_IN_LB$Developmental.or.Reproductive.Toxins) != "TRUE" , 1, 0)

CHEMICALS_MEASURED_IN_LB$Bee_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Bee.Toxins != "" & 
is.na(CHEMICALS_MEASURED_IN_LB$Bee.Toxins) != "TRUE" , 1, 0)

#Sum up the counts
CHEMICALS_MEASURED_IN_LB_CAR_COUNTS <- CHEMICALS_MEASURED_IN_LB %>%  summarise(Carcinogen = sum(Carcinogen_FLAG),
                                                                               Hormone_Disruptor =  sum(Hormone_FLAG), 
                                                                               Neurotoxin = sum(Neurotoxin_FLAG), 
                                                                               Reproductive = sum(Reprod_FLAG),
                                                                               Bee =  sum(Bee_FLAG))


#pivot the table for a bar chart
CHEMICALS_MEASURED_IN_LB_CAR_COUNTS_HIST <- pivot_longer(CHEMICALS_MEASURED_IN_LB_CAR_COUNTS, cols =c("Carcinogen","Hormone_Disruptor","Neurotoxin", "Reproductive", "Bee"))

#plot the bar chart
Toxin_bar <- ggplot(CHEMICALS_MEASURED_IN_LB_CAR_COUNTS_HIST, mapping = aes(x = name, y = value, fill = name)) +geom_bar(stat="identity") +ggtitle("Overall Toxin Counts") + xlab("Toxin") + ylab("Pounds") +  scale_fill_brewer(palette = "Reds") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust=.6),
        legend.position="none"
  )

###########################################################################################################################

#Subset to just carcinogens
Carcinogens <- subset(CHEMICALS_MEASURED_IN_LB, Carcinogen != "")

Chem_Car_Group <- subset(CHEMICALS_MEASURED_IN_LB, Value1 >0 & Carcinogen != "")

Chem_Car_Group1 <- Chem_Car_Group %>% group_by(State,Year, Carcinogen) %>% summarise(Total_Carcinogen = sum(Value1),.groups = "keep" )

#Create a function that can be fed different states and create differing plots
Carcinogen_Plots <- function(state){
 ggplot(subset(Chem_Car_Group1, State == `state`), mapping = aes(x = Year, y = Total_Carcinogen, color = Total_Carcinogen)) + 
  geom_point(shape=25, size=2, stroke = 2) + ggtitle(`state`) + xlim(1990, 2020) + ylim(0, 300000) + xlab("Pounds") + 
    scale_colour_viridis( option = "rocket", begin = .8, end = 0) +    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(face = "bold", size = (25)), 
          legend.title = element_text(face = "bold", size =(25)), 
          legend.text = element_text(size =(25)), 
          axis.title = element_text( size = (25)),
          axis.text = element_text(size = (25)),
          legend.position="none")
}

  
###########################################################################################################################

#Create a plot for overall Carcinogens over time in California
CA_Carcinogen <- ggplot(subset(Chem_Car_Group1, State == "CALIFORNIA"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_point(shape=25, size=2, stroke = 1) + ggtitle("Total Carcinogens CA") +ylab("Pounds")+ xlim(1990, 2020) + ylim(0, 300000) + 
  scale_color_manual(breaks = c("known","possible","probable"), values = c("firebrick","lightpink2","mistyrose3")) +
  theme(panel.background = element_rect(fill = "gray100",
                                        colour = "gray100",
                                        size = 0.5, linetype = "solid"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))

library(RColorBrewer)

#create our own color scheme, using the prescribed scale gave us white points which were not visible
my_color1 <- brewer.pal(n = 11, "RdGy")[1:4]
my_color2 <- brewer.pal(n = 7, "RdPu")[4:6]
my_color <- c(my_color1,my_color2)

#Create a scaled plot function - used in the PDF for comparison purposes
California_Plots_Scaled <- function(level, max){
ggplot(subset(Carcinogens, State == "CALIFORNIA" & Carcinogen == `level`), mapping = aes(x = Year, y = Value1,  color = Chemical )) +
  geom_point(shape=25, size=3, stroke = 1) + ggtitle(paste0("CA - ",`level`," carcinogen")) + xlim(1990, 2020) + ylim(0, 300000) + ylab("Pounds") +
    scale_colour_manual(name = "Chemical",values = my_color) +
  theme( panel.background = element_rect(fill = "gray100",
                                         colour = "gray100",
                                         size = 0.5, linetype = "solid"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold", size = (15)), 
        legend.title = element_text(face = "bold", size =(15)), 
        legend.text = element_text(size =(15)), 
        axis.title = element_text( size = (15)),
        axis.text = element_text(size = (15)))
}

#Create a non-scaled function used in the presentation
California_Plots <- function(level, max){
  ggplot(subset(Carcinogens, State == "CALIFORNIA" & Carcinogen == `level`), mapping = aes(x = Year, y = Value1,  color = Chemical )) +
    geom_point(shape=25, size=2, stroke = 1) + ggtitle(paste0("CA - ",`level`," carcinogen")) + xlim(1990, 2020)  + ylab("Pounds") +
    scale_colour_manual(name = "Chemical",values = my_color) +
    theme( panel.background = element_rect(fill = "gray100",
                                           colour = "gray100",
                                           size = 0.5, linetype = "solid"),
           panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.line = element_line(colour = "black"),
           plot.title = element_text(face = "bold", size = (8)), 
           legend.title = element_text(face = "bold", size =(8)), 
           legend.text = element_text(size =(8)), 
           axis.title = element_text( size = (8)),
           axis.text = element_text(size = (8)))
}
###########################################################################################################################


#Create a map displaying the difference in the amount of carcinogens across states
states<- statepop[,c("fips","abbr","full")]
states$full <- toupper(states$full)

Carcinogens_map_prep <- Chem_Car_Group %>% group_by(State) %>% summarise(Total_Carcinogen = sum(Value1),.groups = "keep" )

names(Carcinogens_map_prep)[1] <- "full"

Carcinogens_map <- full_join(states,Carcinogens_map_prep, by ="full" )

Carcinogen_usmap <- plot_usmap(data = Carcinogens_map, values = "Total_Carcinogen", color = "black") + 
  scale_fill_continuous(low="white", high = "firebrick", name = "Amount of Carcinogens", label = scales::comma) + 
  theme(legend.position = "right") 

###########################################################################################################################

#Creating a summary counts table for all different carcinogen chemicals
t<-subset(Carcinogens, Carcinogens$State=="CALIFORNIA")
table<-kable(table(t$Chemical, t$Carcinogen))

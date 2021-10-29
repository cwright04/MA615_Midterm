library(usmap)
library(ggplot2)


###########################################################################################################################
#Plot Bar chart displaying the difference in the amount of carcinogens

#Subset down the data into just measured in lb
CHEMICALS_MEASURED_IN_LB <- subset(CHEM_CLEAN2, Measurements =="MEASURED IN LB") 

# FLAGS <- function(var){
#     CHEMICALS_MEASURED_IN_LB$`{{var}}_FLAG` <- ifelse(CHEMICALS_MEASURED_IN_LB$`{{var}}` != "" & 
#                                                   is.na(CHEMICALS_MEASURED_IN_LB$`{{var}}`) != TRUE , 1, 0)
# }
# 
# FLAGS(Carcinogen)

#Create flags to sum up
CHEMICALS_MEASURED_IN_LB$Carcinogen_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Carcinogen != "" & 
                                                     is.na(CHEMICALS_MEASURED_IN_LB$Carcinogen) != "TRUE" , 1, 0)
CHEMICALS_MEASURED_IN_LB$Hormone_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Hormone.Disruptor != "" & 
                                                  is.na(CHEMICALS_MEASURED_IN_LB$Hormone.Disruptor) != "TRUE" , 1, 0)
CHEMICALS_MEASURED_IN_LB$Neurotoxin_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Neurotoxins != "" & 
                                                     is.na(CHEMICALS_MEASURED_IN_LB$Neurotoxins) != "TRUE" , 1, 0)
CHEMICALS_MEASURED_IN_LB$Reprod_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Developmental.or.Reproductive.Toxins != "" & 
                                                 is.na(CHEMICALS_MEASURED_IN_LB$Developmental.or.Reproductive.Toxins) != "TRUE" , 1, 0)
CHEMICALS_MEASURED_IN_LB$Bee_FLAG <- ifelse(CHEMICALS_MEASURED_IN_LB$Bee.Toxins != "" & 
                                              is.na(CHEMICALS_MEASURED_IN_LB$Bee.Toxins) != "TRUE" , 1, 0)

#Sum up the counts
CHEMICALS_MEASURED_IN_LB_CAR_COUNTS <- CHEMICALS_MEASURED_IN_LB %>%  summarise(Carcinogen = sum(Carcinogen_FLAG),
                                                                               Hormone_Disruptor =  sum(Hormone_FLAG), 
                                                                               Neurotoxin = sum(Neurotoxin_FLAG), 
                                                                               Reproductive = sum(Reprod_FLAG),
                                                                               Bee =  sum(Bee_FLAG))


#pivot the table for a bar chart
CHEMICALS_MEASURED_IN_LB_CAR_COUNTS_HIST <- pivot_longer(CHEMICALS_MEASURED_IN_LB_CAR_COUNTS, 
                                                         cols =c("Carcinogen","Hormone_Disruptor","Neurotoxin", "Reproductive", "Bee"))

#plot the bar chart
Toxin_bar <- ggplot(CHEMICALS_MEASURED_IN_LB_CAR_COUNTS_HIST, mapping = aes(x = name, y = value, fill = name)) +geom_bar(stat="identity") +
  ggtitle("Overall Toxin Counts") + xlab("Toxin") + ylab("Pounds") +  scale_fill_brewer(palette = "Reds") +
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


Chem_Car_Group <- subset(Carcinogens, Carcinogens$Value1 >0  )

Chem_Car_Group1 <- Chem_Car_Group %>% group_by(State,Year, Carcinogen) %>% summarise(Total_Carcinogen = sum(Value1),.groups = "keep" )

#Create a custom color scale

Carcinogen_Plots <- function(state){
 ggplot(subset(Chem_Car_Group1, State == `state`), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_point(shape=25, size=2, stroke = 1) + ggtitle(`state`) + xlim(1990, 2020) + ylim(0, 300000) + scale_color_manual(breaks = c("known","possible","probable"), values = c("firebrick","lightpink2","mistyrose3"))
}

###########################################################################################################################


CA_Carcinogen <- ggplot(subset(Chem_Car_Group1, State == "CALIFORNIA"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_point() + ggtitle("Total Carcinogens CA") +ylab("Pounds")+ xlim(1990, 2020) + ylim(0, 300000) + scale_color_manual(breaks = c("known","possible","probable"), values = c("firebrick","lightpink2","mistyrose3")) 


California_Plots <- function(level){
ggplot(subset(Carcinogens, State == "CALIFORNIA" & Carcinogen == `level`), mapping = aes(x = Year, y = Value1, color = Chemical )) +
  geom_point(shape=25, size=2, stroke = .5) + ggtitle(paste0("CA - ",`level`," carcinogen")) + xlim(1990, 2020) + ylim(0, 300000) + ylab("Pounds") +
  scale_color_brewer(palette = "RdGy") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
  )
}

###########################################################################################################################


#Create a map displaying the difference in the amount of carcinogens
states<- statepop[,c("fips","abbr","full")]
states$full <- toupper(states$full)

Carcinogens_map_prep <- Chem_Car_Group %>% group_by(State) %>% summarise(Total_Carcinogen = sum(Value1),.groups = "keep" )

names(Carcinogens_map_prep)[1] <- "full"

Carcinogens_map <- full_join(states,Carcinogens_map_prep, by ="full" )

Carcinogen_usmap <- plot_usmap(data = Carcinogens_map, values = "Total_Carcinogen", color = "black") + 
  scale_fill_continuous(low="white", high = "firebrick", name = "Amount of Carcinogens", label = scales::comma) + 
  theme(legend.position = "right") 


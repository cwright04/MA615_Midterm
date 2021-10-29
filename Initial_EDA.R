# source(file = "Wrangling_code.R")

table(CHEM_CLEAN2$Year)

CHEM_CLEAN2_Car <- subset(CHEM_CLEAN2, Carcinogen != "")

table(CHEM_CLEAN2_Car$Year)



table(CHEM_CLEAN2_Car$Chemical)
table(CHEM_CLEAN2_Car$Chemical_Type)

CHEM_CLEAN2_Car_Insec <- subset(CHEM_CLEAN2_Car, Bee.Toxins != "")

table(CHEM_CLEAN2_Car_Insec$Chemical_Type)

CHEM_CLEAN2_Car_LBS  <- subset(CHEM_CLEAN2_Car, Measurements =="MEASURED IN LB")

table(CHEM_CLEAN2_Car_LBS$Year)
table(CHEM_CLEAN2_Car_LBS$Item)

table(CHEM_CLEAN2_Car_LBS$State)

table(CHEM_CLEAN2_Car_LBS$Carcinogen)


#group the level of carcinogen by level by state
Chem_Car_Group <- subset(CHEM_CLEAN2_Car_LBS, CHEM_CLEAN2_Car_LBS$Value1 >0  )

Chem_Car_Group1 <- Chem_Car_Group %>% group_by(State,Year, Carcinogen) %>% summarise(Total_Carcinogen = sum(Value1),.groups = "keep" )




plot_CA <- ggplot(subset(Chem_Car_Group1, State == "CALIFORNIA"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_line() + ggtitle("CA") 
plot_FL <- ggplot(subset(Chem_Car_Group1, State == "FLORIDA"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_point() + ggtitle("FL")
plot_NY <- ggplot(subset(Chem_Car_Group1, State == "NEW YORK"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_point() + ggtitle("NY")
plot_NC <- ggplot(subset(Chem_Car_Group1, State == "NORTH CAROLINA"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) +
  geom_point() + ggtitle("NC")
plot_OR <- ggplot(subset(Chem_Car_Group1, State == "OREGON"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_point() + ggtitle("OR")
plot_WA <- ggplot(subset(Chem_Car_Group1, State == "WASHINGTON"), mapping = aes(x = Year, y = Total_Carcinogen, color = Carcinogen )) + 
  geom_point() + ggtitle("WA")

require(gridExtra)
grid.arrange(plot_CA, plot_FL, plot_NY, plot_NC, plot_OR, plot_WA, ncol = 2)

library(tidyverse)
library(magrittr)

## load data

strawb_raw <- read.csv("Strawberries_more_years.csv", fileEncoding = "latin1", header = TRUE)
pest_raw <- read.csv("Pesticides.csv", fileEncoding = "latin1", header = TRUE)

names(strawb_raw)[ncol(strawb_raw)] <- "CV"


##################################################
# CLEAN UP PESTICIDES DATA                       #
##################################################
#rename first column
names(pest_raw)[1] <- "Pesticide"

#remove missing obs
pest1 <- subset(pest_raw, Pesticide != "" )

pest1$Chemical <- toupper(pest1$Pesticide)

pest_clean <- pest1[,-1]


##################################################
## Drop the no-info columns
##################################################
## capture column names
cnames <- colnames(strawb_raw)
x <- 1:dim(strawb_raw)[2]
## set variable to collect values
T <- NULL
## collect number of unique rows in each column
for(i in x){T <- c(T, dim(unique(strawb_raw[i]))[1])}
## use T to select columns to drop
drop_cols <- cnames[which(T == 1)]

## drop no info columns
strawb_raw %>% select(!all_of(drop_cols))


########################################################################################################
#The following chunk of code breaks the data into subsets by domain. 
#This will make it easier to parse out the Data.Item and Domain.Category into separate columns.
#########################################################################################################


#CLEAN UP SUBSET OF DOMAIN =  TOTAL
TOTAL <- subset(strawb_raw, strawb_raw$Domain == "TOTAL")

#Investigate Data.Item and Domain.Category within this subset
(tot_item <- data.frame(unique(TOTAL$Data.Item)))
(tot_cat <- data.frame(unique(TOTAL$Domain.Category)))

#split Data.Item into separate columns
TOTAL %<>% separate(col=Data.Item,
                    into = c("Item", "Measurements"),
                    sep = ", ",
                    fill = "right")

TOTAL %<>% separate(col=Item,
                    into = c("Strawberries", "Item"),
                    sep = " - ",
                    fill = "right")

#Finalize subset data/ prepare for row binding with other subsets
TOTAL_CLEAN <- TOTAL[,c("State","Year", "Item", "Measurements", "Value")] 

#CLEAN UP SUBSET OF DOMAIN =  ORGANIC STATUS
ORGANIC <- subset(strawb_raw, strawb_raw$Domain == "ORGANIC STATUS")

#Investigate Data.Item and Domain.Category within this subset
(org_item <- data.frame(unique(ORGANIC$Data.Item)))
(org_cat <- data.frame(unique(ORGANIC$Domain.Category)))

ORGANIC %<>% separate(col=Data.Item,
                      into = c("Item", "Measurements"),
                      sep = "- ",
                      fill = "right")

ORGANIC %<>% separate(col=Item,
                      into = c("Strawberries", "Item", "Type"),
                      sep = ", ",
                      fill = "right")

ORGANIC$Value1 <- as.numeric(str_replace_all(ORGANIC$Value, ",", ""))  

ORGANIC_CLEAN <- ORGANIC[,c("State","Year", "Item","Type", "Measurements", "Value1")]

#CLEAN UP SUBSET OF DOMAIN =  CHEMICAL, X
CHEM <- subset(strawb_raw, strawb_raw$Domain == "CHEMICAL, OTHER" | strawb_raw$Domain == "CHEMICAL, FUNGICIDE" | 
                 strawb_raw$Domain == "CHEMICAL, HERBICIDE" | strawb_raw$Domain == "CHEMICAL, INSECTICIDE")

#Investigate Data.Item and Domain.Category within this subset
(CHEM_item <- data.frame(unique(CHEM$Data.Item)))
(CHEM_cat <- data.frame(unique(CHEM$Domain.Category)))

#split Data.Item into separate columns
CHEM %<>% separate(col=Data.Item,
                   into = c("Strawberries", "Item", "Measurements", "Unit"),
                   sep = ", ",
                   fill = "right")


#split Domain.Category into separate columns
CHEM %<>% separate(col=Domain.Category,
                   into = c("Chemical_Type", "Chemical"),
                   sep = ": ",
                   fill = "right")

CHEM$Chemical <- gsub("[()]", "", CHEM$Chemical)
CHEM$Chemical_Type <- gsub("CHEMICAL, ", "", CHEM$Chemical_Type)
CHEM$Domain <- gsub(", OTHER ", "", CHEM$Domain)

CHEM %<>% separate(col=Chemical,
                   into = c("Chemical", "Chemical_Code"),
                   sep = "=",
                   fill = "right")

CHEM$Chemical_Code <- gsub(" ", "", CHEM$Chemical_Code)
CHEM$Chemical <-str_trim(CHEM$Chemical, "right")

#Finalize subset data/ prepare for row binding with other subsets
CHEM<- CHEM[,c("State","Year", "Item", "Measurements", "Value","Chemical_Type", "Chemical","Chemical_Code", "Unit")] 

  
  CHEM$Chemical_Code <- gsub(" ", "", CHEM$Chemical_Code)
  CHEM$Chemical <-str_trim(CHEM$Chemical, "right")
  
  #Finalize subset data/ prepare for row binding with other subsets
  CHEM<- CHEM[,c("State","Year", "Item", "Measurements", "Value","Chemical_Type", "Chemical","Chemical_Code", "Unit")] 


#CLEAN UP SUBSET OF DOMAIN =  FERTILIZER

FERTILIZER <- subset(strawb_raw, strawb_raw$Domain == "FERTILIZER")

#Investigate Data.Item and Domain.Category within this subset
(FERTILIZER_item <- data.frame(unique(FERTILIZER$Data.Item)))
(FERTILIZER_cat <- data.frame(unique(FERTILIZER$Domain.Category)))

#split Data.Item into separate columns
FERTILIZER %<>% separate(col=Data.Item,
                         into = c("Strawberries", "Item", "Measurements", "Unit"),
                         sep = ", ",
                         fill = "right")

#split Domain.Category into separate columns
FERTILIZER %<>% separate(col=Domain.Category,
                         into = c("Chemical_Type", "Chemical"),
                         sep = ": ",
                         fill = "right")

FERTILIZER$Chemical <- gsub("[()]", "", FERTILIZER$Chemical)

#Finalize subset data/ prepare for row binding with other subsets
FERTILIZER<- FERTILIZER[,c("State","Year", "Item", "Measurements", "Value","Chemical_Type", "Chemical","Unit")] %>% mutate(Chemical_Code = NA)

#Stack CHEM cleaned data back together
CHEM_CLEAN <- rbind(FERTILIZER, CHEM) 


#CLEAN UP VALUE VARIABLE
#Convert to numeric
CHEM_CLEAN$Value1 <- as.numeric(str_replace_all(CHEM_CLEAN$Value, ",", ""))

#Remove character Value
CHEM_CLEAN <- CHEM_CLEAN[,c("State","Year", "Item", "Measurements", "Value1","Chemical_Type", "Chemical","Unit")]


#########################################
#Merge on pesticides

CHEM_CLEAN$Chemical <- toupper(CHEM_CLEAN$Chemical)

CHEM_CLEAN2 <- left_join(CHEM_CLEAN, pest_clean, by = "Chemical")


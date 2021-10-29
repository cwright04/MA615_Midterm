# source(file = "Wrangling_code.R")

table(CHEM_CLEAN2$Year)

CHEM_CLEAN2_Car <- subset(CHEM_CLEAN2, Carcinogen != "")

table(CHEM_CLEAN2_Car$Year)

ggplot(CHEM_CLEAN2_Car, mapping = aes)


table(CHEM_CLEAN2_Car$Chemical)
table(CHEM_CLEAN2_Car$Chemical_Type)

CHEM_CLEAN2_Car_Insec <- subset(CHEM_CLEAN2_Car, Bee.Toxins != "")

table(CHEM_CLEAN2_Car_Insec$Chemical_Type)

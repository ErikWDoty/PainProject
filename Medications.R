library(dplyr)

CV_tab <- read.csv("~/Harvard/Research/CV_table.csv")
MV_tab <- read.csv("~/Harvard/Research/MV_table.csv")
#Eliminate NAs
CV_tab <- CV_tab[-which(is.na(CV_tab$amount)),]
main.table <- read.csv("C:/Users/erikd/OneDrive/Documents/Harvard/Research/main.table.csv")
admit.times <- read.csv("C:/Users/erikd/OneDrive/Documents/Harvard/Research/admittimes.csv")

MV_tab$starttime <- as.POSIXlt(MV_tab$starttime)
CV_tab$charttime <- as.POSIXlt(CV_tab$charttime)
admit.times$admittime <- as.POSIXlt(admit.times$admittime)


# Create medication table
Med_Table <- data.frame(hadm_id = NA, opiates24 = NA, opiates_tot = NA, benzos24 = NA, benzos_tot = NA, propofol24 = NA, propofol_tot = NA)
Med_Table[1:844,] <- NA

# Add in HADM_ID from main cohort
Med_Table$hadm_id <- main.table$hadm_id[which(main.table$ventdur <= 24)]

# Following code will sort through CV and MV tables and calculate first day(24hr)
# Opiate (Morphine Equivalents), Benzo (Lorazepam Equivalent), and Propofol total

# Opiate For Loop
# Following ids are tied to various opiates
Fentanyl_IDs <- c(5464, 1355, 1361, 3432, 5261, 5077, 30150, 30308, 30118, 30149, 
                  43387, 221744, 225972, 225942)
Hydromorphone_IDs <- c(221833, 981, 30163)
Meperidine_IDs <- c(225973, 1252, 40582, 40579)
Morphine_IDs <- c(225154, 1813, 5613, 30153, 30126)

#Opiate totals
for(i in 1:844){
  if(Med_Table$hadm_id[i] %in% CV_tab$hadm_id == TRUE){
  Med_Table$opiates_tot[i] <- (sum(CV_tab$amount[which(CV_tab$hadm_id == Med_Table$hadm_id[i] & CV_tab$itemid %in% Fentanyl_IDs)])/10) +
                              (sum(CV_tab$amount[which(CV_tab$hadm_id == Med_Table$hadm_id[i] & CV_tab$itemid %in% Hydromorphone_IDs)])*6.7) +
                              (sum(CV_tab$amount[which(CV_tab$hadm_id == Med_Table$hadm_id[i] & CV_tab$itemid %in% Meperidine_IDs)])/7.5) + 
                              (sum(CV_tab$amount[which(CV_tab$hadm_id == Med_Table$hadm_id[i] & CV_tab$itemid %in% Morphine_IDs)]))
  }
  
  if(Med_Table$hadm_id[i] %in% MV_tab$hadm_id == TRUE){
    Med_Table$opiates_tot[i] <- (sum(MV_tab$amount[which(MV_tab$hadm_id == Med_Table$hadm_id[i] & MV_tab$itemid %in% Fentanyl_IDs)])/10) +
                                (sum(MV_tab$amount[which(MV_tab$hadm_id == Med_Table$hadm_id[i] & MV_tab$itemid %in% Hydromorphone_IDs)])*6.7) +
                                (sum(MV_tab$amount[which(MV_tab$hadm_id == Med_Table$hadm_id[i] & MV_tab$itemid %in% Meperidine_IDs)])/7.5) + 
                                (sum(MV_tab$amount[which(MV_tab$hadm_id == Med_Table$hadm_id[i] & MV_tab$itemid %in% Morphine_IDs)]))
  }
}

#Opiate First 24hr total
for(i in 1:844){
  if(Med_Table$hadm_id[i] %in% CV_tab$hadm_id == TRUE){
    Med_Table$opiates24[i] <- (sum(CV_tab$amount[which(CV_tab$hadm_id == Med_Table$hadm_id[i] & CV_tab$itemid %in% Fentanyl_IDs &
                                 CV_tab$charttime - admit.times$admittime[which(admit.times$hadm_id == Med_Table$hadm_id[i])] <= as.difftime(1, units = "days"))])/10) +
      (sum(CV_tab$amount[which(CV_tab$hadm_id == Med_Table$hadm_id[i] & CV_tab$itemid %in% Hydromorphone_IDs & 
                                 CV_tab$charttime - admit.times$admittime[which(admit.times$hadm_id == Med_Table$hadm_id[i])] <= as.difftime(1, units = "days"))])*6.7) +
      (sum(CV_tab$amount[which(CV_tab$hadm_id == Med_Table$hadm_id[i] & CV_tab$itemid %in% Meperidine_IDs & 
                                 CV_tab$charttime - admit.times$admittime[which(admit.times$hadm_id == Med_Table$hadm_id[i])] <= as.difftime(1, units = "days"))])/7.5) + 
      (sum(CV_tab$amount[which(CV_tab$hadm_id == Med_Table$hadm_id[i] & CV_tab$itemid %in% Morphine_IDs & 
                                 CV_tab$charttime - admit.times$admittime[which(admit.times$hadm_id == Med_Table$hadm_id[i])] <= as.difftime(1, units = "days"))]))
  }
  
  if(Med_Table$hadm_id[i] %in% MV_tab$hadm_id == TRUE){
    Med_Table$opiates24[i] <- (sum(MV_tab$amount[which(MV_tab$hadm_id == Med_Table$hadm_id[i] & MV_tab$itemid %in% Fentanyl_IDs & 
                                MV_tab$starttime - admit.times$admittime[which(admit.times$hadm_id == Med_Table$hadm_id[i])] <= as.difftime(1, units = "days"))])/10) +
      (sum(MV_tab$amount[which(MV_tab$hadm_id == Med_Table$hadm_id[i] & MV_tab$itemid %in% Hydromorphone_IDs & 
                                 MV_tab$starttime - admit.times$admittime[which(admit.times$hadm_id == Med_Table$hadm_id[i])] <= as.difftime(1, units = "days"))])*6.7) +
      (sum(MV_tab$amount[which(MV_tab$hadm_id == Med_Table$hadm_id[i] & MV_tab$itemid %in% Meperidine_IDs & 
                                 MV_tab$starttime - admit.times$admittime[which(admit.times$hadm_id == Med_Table$hadm_id[i])] <= as.difftime(1, units = "days"))])/7.5) + 
      (sum(MV_tab$amount[which(MV_tab$hadm_id == Med_Table$hadm_id[i] & MV_tab$itemid %in% Morphine_IDs &
                                 MV_tab$starttime - admit.times$admittime[which(admit.times$hadm_id == Med_Table$hadm_id[i])] <= as.difftime(1, units = "days"))]))
  }
}

# Opiate Average
Med_Table$Opiate_Avg <- NA
for(i in 1:844){
  Med_Table$Opiate_Avg[i] <- Med_Table$opiates_tot[i]/main.table$icu_los[which(main.table$hadm_id == Med_Table$hadm_id[i])]
}

# Join Meds to main table
main.table <- left_join(main.table, Med_Table, by = "hadm_id")

#ANOVA
summary(aov(opiates24~Pain.Scale, data = main.table[which(main.table$ventdur <= 24),]))
summary(aov(opiates_tot~Pain.Scale, data = main.table[which(main.table$ventdur <= 24),]))
summary(aov(Opiate_Avg~Pain.Scale, data = main.table[which(main.table$ventdur <= 24),]))

  
  
  
  
  
  
  
  
  


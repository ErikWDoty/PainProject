# Select input events (MEDS) from CABG patients, when carvue was used
meds_CV <- dbGetQuery(con, "select * from inputevents_cv where hadm_id in 
                            (select hadm_id from cptevents where cpt_number 
                            BETWEEN 33510 and 33516 or cpt_number 
                            between 33533 and 33548)")

# Select input events (MEDS) from CABG patients, when metavision was used
meds_MV <- dbGetQuery(con, "select * from inputevents_mv where hadm_id in 
                            (select hadm_id from cptevents where cpt_number 
                            BETWEEN 33510 and 33516 or cpt_number 
                            between 33533 and 33548)")


# creates table of all input events on CABG patients
CV_tab <- as.data.frame(table(meds_CV$itemid))
MV_tab <- as.data.frame(table(meds_MV$itemid))

CV_tab$med <- NA
MV_tab$med <- NA


# 2 for loops that go through and label input item based on 
# itemid. Looks through d_items table ot find label. This data
# can be used to find the exact itemid for the medication used
# in icu.
for(i in 1:nrow(CV_tab)){
  CV_tab$med[i] <- dbGetQuery(con, sprintf(
    "select label from mimiciii.d_items where itemid = %s;", 
                                           CV_tab$Var1[i]))
}

CV_tab$med <- tolower(CV_tab$med) 

for(i in 1:nrow(MV_tab)){
  MV_tab$med[i] <- dbGetQuery(con, sprintf(
    "select label from mimiciii.d_items where itemid = %s;", 
    MV_tab$Var1[i]))
}

MV_tab$med <- tolower(MV_tab$med)

colnames(CV_tab)[1] <- "itemid"
colnames(MV_tab)[1] <- "itemid"

# Following code will build table to of events for medications of interest
# These are sedating medications
# The specific medications we will look at are 
# fentanyl, midazolam, propofol, ativan, valium, dilaudid, vecuronium

medIDs <- rbind(CV_tab, MV_tab)
# Filter only meds of interest, will give us itemids
medIDs <- medIDs[c(grep("fentanyl", medIDs$med),
                   grep("midazolam",medIDs$med),
                   grep("propofol",medIDs$med),
                   grep("ativan",medIDs$med),
                   grep("valium",medIDs$med),
                   grep("dilaudid",medIDs$med),
                   grep("vecuronium",medIDs$med)),]
medIDs


#CV subject_id (2), hadm_id (3), icustay_id (4), charttime(5), itemid(6), amount(7)
# amountuom(8), rate(9), rateuom (10) originalroute (19)
#MV subject_id(2), hadm_id(2) icustay (4), starttime(5), itemid(7), amount (8), 
# amountuomrate(9), rate (10) rateuom(11) originalroute (19)

CV <- meds_CV[,c(2,3,4,5,6,7,8,9,10,19)]
MV <- meds_MV[,c(2,3,4,5,7,8,9,10,11,16)]
colnames(CV)[4] <- "date"
colnames(CV)[10] <- "route"
colnames(MV)[4] <- "date"
colnames(MV)[10] <- "route"

meds <- rbind(CV,MV)
meds$drug_name <- NA
# For loop designed to sort through sed.med table, which currently contains all
# input events, and filter out medications of interest
sed.meds <- NULL
for(i in 1:nrow(medIDs)){
  sed.meds <- rbind(sed.meds, meds[which(meds$itemid == medIDs$itemid[i]),])
  sed.meds$drug_name[which(sed.meds$itemid == medIDs$itemid[i])] <- medIDs$med[i] 
  }
sed.meds <- sed.meds[-1,]
# MV and CV use differing terminolgy, following code makes it same for both
sed.meds$route[c(grep("Bolus", sed.meds$route), grep("Push", sed.meds$route))] <- "Push"
sed.meds$route[grep("Drip", sed.meds$route)] <- "Drip"
table(sed.meds$rateuom)
# Sed meds is now a table that contains all input events (sedative medications)
# given to CABG patients during their entire length of stay
head(sed.meds)

## Following chunk will build a table to summarize the meds of interest per patient
# Summary information will include average dose of drug, number of days the drug was
# taken for, the proportion of the los that it was taken for
med.summary <- NULL
med.summary <- data.frame(hadm_id = NA, los = NA,  d.ativ.avg = NA, 
                          d.ativ.time = NA, d.fent.avg = NA, d.fent.time = NA, 
                          d.dil.avg = NA, d.dil.time = NA, d.midaz.avg = NA,
                          d.midaz.time = NA, d.pro.avg = NA, d.pro.time = NA, 
                          d.vec.avg = NA, d.vec.time = NA, p.ativ.avg = NA, 
                          p.ativ.time = NA, p.fent.avg = NA, p.fent.time = NA, 
                          p.dil.avg = NA, p.dil.time = NA, p.midaz.avg = NA,
                          p.midaz.time = NA, p.pro.avg = NA, p.pro.time = NA, 
                          p.vec.avg = NA, p.vec.time = NA, sed.days = NA,
                          proportion_los = NA)

hadm_dist <- unique(sed.meds$hadm_id)
med.summary[1:length(hadm_dist),] <-  0 
med.summary$hadm_id <- hadm_dist

for(i in 1:length(hadm_dist)){
  med.summary$los[i] <- adm_table$hosp_los[which(adm_table$hadm_id == hadm_dist[i])]

  # Following calculates average doses and time of Drips (continuous infusions)
  d.pt.meds <- sed.meds[which(sed.meds$hadm_id == hadm_dist[i] & sed.meds$route == "Drip"),]
  d.pt.meds <- d.pt.meds[-is.na(d.pt.meds$rate)]
  if(is.null(d.pt.meds$rate[grep("ativan", d.pt.meds$drug_name)]) == F){
  med.summary$d.ativ.avg[i] <- mean(d.pt.meds$rate[grep("ativan", d.pt.meds$drug_name)])
  med.summary$d.ativ.time[i] <- as.numeric(max(d.pt.meds$date[grep("ativan", d.pt.meds$drug_name)]) -
                                          min(d.pt.meds$date[grep("ativan", d.pt.meds$drug_name)]))}
  if(is.null(d.pt.meds$rate[grep("fentanyl", d.pt.meds$drug_name)]) == F){
  med.summary$d.fent.avg[i] <- mean(d.pt.meds$rate[grep("fentanyl", d.pt.meds$drug_name)])
  med.summary$d.fent.time[i] <- as.numeric(max(d.pt.meds$date[grep("fentanyl", d.pt.meds$drug_name)]) -
                                          min(d.pt.meds$date[grep("fentanyl", d.pt.meds$drug_name)]))}
  if(is.null(d.pt.meds$rate[grep("dilaudid", d.pt.meds$drug_name)]) == F){
  med.summary$d.dil.avg[i] <- mean(d.pt.meds$rate[grep("dilaudid", d.pt.meds$drug_name)])
  med.summary$d.dil.time[i] <- as.numeric(max(d.pt.meds$date[grep("dilaudid", d.pt.meds$drug_name)]) -
                                          min(d.pt.meds$date[grep("dilaudid", d.pt.meds$drug_name)]))}
  if(is.null(d.pt.meds$rate[grep("midaz", d.pt.meds$drug_name)]) == F){
  med.summary$d.midaz.avg[i] <- mean(d.pt.meds$rate[grep("midaz", d.pt.meds$drug_name)])
  med.summary$d.midaz.time[i] <- as.numeric(max(d.pt.meds$date[grep("midaz", d.pt.meds$drug_name)]) -
                                          min(d.pt.meds$date[grep("midaz", d.pt.meds$drug_name)]))}
  if(is.null(d.pt.meds$rate[grep("propofol", d.pt.meds$drug_name)]) == F){
  med.summary$d.pro.avg[i] <- mean(d.pt.meds$rate[grep("propofol", d.pt.meds$drug_name)])
  med.summary$d.pro.time[i] <- as.numeric(max(d.pt.meds$date[grep("propofol", d.pt.meds$drug_name)]) -
                                          min(d.pt.meds$date[grep("propofol", d.pt.meds$drug_name)]))}
  if(is.null(d.pt.meds$rate[grep("vecuronium", d.pt.meds$drug_name)]) == F){
  med.summary$d.vec.avg[i] <- mean(d.pt.meds$rate[grep("vecuronium", d.pt.meds$drug_name)])
  med.summary$d.vec.time[i] <- as.numeric(max(d.pt.meds$date[grep("vecuronium", d.pt.meds$drug_name)]) -
                                         min(d.pt.meds$date[grep("vecuronium", d.pt.meds$drug_name)]))}
  
  # Calculates number of days patient was on a sedative medication
  med.summary$sed.days[i] <-length(unique(sed.meds$date[which(sed.meds$hadm_id == hadm_dist[i])]))
  
  # Following calculates average doses and time of Drips (continuous infusions)
  p.pt.meds <- sed.meds[which(sed.meds$hadm_id == hadm_dist[i] & sed.meds$route == "Push"),]
  if(is.null(p.pt.meds$amount[grep("ativan", p.pt.meds$drug_name)]) == F){
    med.summary$p.ativ.avg[i] <- mean(p.pt.meds$amount[grep("ativan", p.pt.meds$drug_name)])
    med.summary$p.ativ.time[i] <- as.numeric(max(p.pt.meds$date[grep("ativan", p.pt.meds$drug_name)]) -
                                               min(p.pt.meds$date[grep("ativan", p.pt.meds$drug_name)]))}
  if(is.null(p.pt.meds$amount[grep("fentanyl", p.pt.meds$drug_name)]) == F){
    med.summary$p.fent.avg[i] <- mean(p.pt.meds$amount[grep("fentanyl", p.pt.meds$drug_name)])
    med.summary$p.fent.time[i] <- as.numeric(max(p.pt.meds$date[grep("fentanyl", p.pt.meds$drug_name)]) -
                                               min(p.pt.meds$date[grep("fentanyl", p.pt.meds$drug_name)]))}
  if(is.null(p.pt.meds$amount[grep("dilaudid", p.pt.meds$drug_name)]) == F){
    med.summary$p.dil.avg[i] <- mean(p.pt.meds$amount[grep("dilaudid", p.pt.meds$drug_name)])
    med.summary$p.dil.time[i] <- as.numeric(max(p.pt.meds$date[grep("dilaudid", p.pt.meds$drug_name)]) -
                                              min(p.pt.meds$date[grep("dilaudid", p.pt.meds$drug_name)]))}
  if(is.null(p.pt.meds$amount[grep("midaz", p.pt.meds$drug_name)]) == F){
    med.summary$p.midaz.avg[i] <- mean(p.pt.meds$amount[grep("midaz", p.pt.meds$drug_name)])
    med.summary$p.midaz.time[i] <- as.numeric(max(p.pt.meds$date[grep("midaz", p.pt.meds$drug_name)]) -
                                                min(p.pt.meds$date[grep("midaz", p.pt.meds$drug_name)]))}
  if(is.null(p.pt.meds$amount[grep("propofol", p.pt.meds$drug_name)]) == F){
    med.summary$p.pro.avg[i] <- mean(p.pt.meds$amount[grep("propofol", p.pt.meds$drug_name)])
    med.summary$p.pro.time[i] <- as.numeric(max(p.pt.meds$date[grep("propofol", p.pt.meds$drug_name)]) -
                                              min(p.pt.meds$date[grep("propofol", p.pt.meds$drug_name)]))}
  if(is.null(p.pt.meds$amount[grep("vecuronium", p.pt.meds$drug_name)]) == F){
    med.summary$p.vec.avg[i] <- mean(p.pt.meds$amount[grep("vecuronium", p.pt.meds$drug_name)])
    med.summary$p.vec.time[i] <- as.numeric(max(p.pt.meds$date[grep("vecuronium", p.pt.meds$drug_name)]) -
                                              min(p.pt.meds$date[grep("vecuronium", p.pt.meds$drug_name)]))}
  # Calculate proportion of days that patient was on sedative medication
  med.summary$proportion_los[i] <- med.summary$sed.days[i]/med.summary$los[i]
}

head(med.summary)

drip_table <- data.frame(hadm_id = NA, los = NA, sed.days = NA, proportion = NA)
drip_table[1:length(hadm_dist),] <- NA
drip_table$hadm_id <- hadm_dist

for(i in 1:nrow(drip_table)){
  drip_table$los[i] <- adm_table$hosp_los[which(adm_table$hadm_id == hadm_dist[i])]
  drip_table$sed.days[i] <- length(unique(sed.meds$date[which(sed.meds$hadm_id == 
                                               hadm_dist[i] & sed.meds$route == "Drip")]))
  drip_table$proportion[i] <- drip_table$sed.days[i]/drip_table$los[i] 
}

head(drip_table)
qplot(round(drip_table$proportion, 2), shape = "histogram",  xlim = c(0,1))
table(round(drip_table$proportion, 1))
qplot(round(med.summary$proportion, 2), shape = "histogram", xlim = c(0,1))
table(round(med.summary$proportion, 1))

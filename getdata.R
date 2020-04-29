# geographies
lrf_lad <- read.csv("data/Local_Authority_District_to_Local_Resilience_Forum_December_2019.csv")
ltla_utla <- read.csv("data/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_April_2019.csv")
lad_reg <- read.csv("data/Local_Authority_District_to_Region_April_2019_Lookup_in_England.csv")
lrf_lad_utla_reg <- cbind(lrf_lad, ltla_utla[match(lrf_lad[,"LAD19CD"],ltla_utla[,"LTLA19CD"]),3:4], lad_reg[match(lrf_lad[,"LAD19CD"],lad_reg[,"LAD19CD"]),3:5])

# setting up buttons
uni_reg <- sort(as.character(unique(lrf_lad_utla_reg[, "RGN19NM"])))
reg <- as.list(0:length(uni_reg)+1)
names(reg) <- c("All", uni_reg)

lrf <- utla <- ltla <- NULL
for (i in length(uni_reg):1+1){
  uni_lrf <- sort(as.character(unique(lrf_lad_utla_reg[lrf_lad_utla_reg[, "RGN19NM"]==names(reg)[[i]],"LRF19NM"])))
  lrf[[i]] <- as.list(0:length(uni_lrf)+1)
  names(lrf[[i]]) <- c("All", uni_lrf)
  for (j in length(uni_lrf):1+1){
    uni_utla <- sort(as.character(unique(lrf_lad_utla_reg[lrf_lad_utla_reg[, "LRF19NM"]==names(lrf[[i]])[j],"UTLA19NM"])))
    utla[[i]][[j]] <- as.list(0:length(uni_utla)+1)
    names(utla[[i]][[j]]) <- c("All", uni_utla)
    for (k in length(uni_utla):1+1){
      uni_ltla <- sort(as.character(unique(lrf_lad_utla_reg[lrf_lad_utla_reg[, "UTLA19NM"]==names(utla[[i]][[j]])[k],"LAD19NM"])))
      ltla[[i]][[j]][[k]] <- as.list(0:length(uni_ltla)+1)
      names(ltla[[i]][[j]][[k]]) <- c("All", uni_ltla)
    }
  }
}
uni_lrf <- sort(unique(lrf_lad_utla_reg[, "LRF19NM"]))
utla2 <- utla
names(utla[[8]][[3]])[2] <- paste(names(utla[[8]][[3]])[2], "and", names(utla[[8]][[3]])[4])
utla[[8]][[3]] <- utla[[8]][[3]][-4]


# load latest data

phe_cases <- read.csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv")
phe_date <- seq(min(as.Date(phe_cases[,"Specimen.date"])), max(as.Date(phe_cases[,"Specimen.date"])),1)

phe_data <- NULL
uni_utla <- sort(unique(phe_cases[phe_cases[, "Area.type"]=="Upper tier local authority",1]))
phe_data$utla <- matrix(0, ncol=length(phe_date), nrow=length(uni_utla), dimnames=list(uni_utla, as.character(phe_date)))
for (i in 1:length(uni_utla)){
  phe_data$utla[i,as.character(phe_cases[phe_cases[,"Area.name"]==uni_utla[i],"Specimen.date"])] <- 
    phe_cases[phe_cases[,"Area.name"]==uni_utla[i],"Daily.lab.confirmed.cases"]
}
phe_data$lrf <- matrix(0, ncol=length(phe_date), nrow=length(uni_lrf), dimnames=list(uni_lrf, as.character(phe_date)))
for (i in 1:length(uni_lrf)){
  lrf_tab <- phe_data$utla[lrf_lad_utla_reg[match(rownames(phe_data$utla), lrf_lad_utla_reg[, "UTLA19NM"]),"LRF19NM"]==uni_lrf[i],]
  if(!is.null(dim(lrf_tab))){
  phe_data$lrf[i,] <- colSums(lrf_tab, na.rm = T)
  }else{
    phe_data$lrf[i,] <- lrf_tab
  }
}
phe_data$lrf["Devon, Cornwall & Isle of Scilly",] <- phe_data$lrf["Devon, Cornwall & Isle of Scilly",]+phe_data$utla["Cornwall and Isles of Scilly",]
phe_data$reg <-  matrix(0, ncol=length(phe_date), nrow=length(uni_reg), dimnames=list(uni_reg, as.character(phe_date)))
for (i in 1:length(uni_reg)){
  phe_data$reg[i,as.character(phe_cases[phe_cases[,"Area.name"]==uni_reg[i],"Specimen.date"])] <- 
    phe_cases[phe_cases[,"Area.name"]==uni_reg[i],"Daily.lab.confirmed.cases"]
}
phe_data$eng <- matrix(0, ncol=length(phe_date), nrow=1, dimnames=list("England", as.character(phe_date)))
phe_data$eng[,as.character(phe_cases[which(phe_cases[,1]=="England"),4])] <- phe_cases[which(phe_cases[,1]=="England"),5]

# nhse

# ons
ons <- as.matrix(read_excel("data/lahbtablesweek16.xlsx", sheet="Occurrences - All data", skip=2))
ons_la <- ons[ons[,"Geography type"]=="Local Authority",]
week_n <- max(as.numeric(ons_la[, "Week number"]))
start_date <- as.Date("2020-01-03")
ons_date <- seq(start_date, start_date+(week_n-1)*7, 7)

ons_death <- NULL

all_loc <- aggregate(as.numeric(ons_la[, "Number of deaths"]), list(ons_la[, "Area name"], ons_la[, "Cause of death"], ons_la[, "Week number"]), sum, na.rm=T)
ons_death$ltla$covid <- matrix(all_loc[all_loc[,2]=="COVID 19",4],ncol=week_n, dimnames=list(unique(all_loc[,1]), as.character(ons_date)))
ons_death$ltla$all <- matrix(all_loc[all_loc[,2]=="All causes",4],ncol=week_n, dimnames=list(unique(all_loc[,1]), as.character(ons_date)))

uni_utla2 <- sort(unique(lrf_lad_utla_reg[!is.na(lrf_lad_utla_reg[,"RGN19NM"]), "UTLA19NM"]))
ons_death$utla$all <- ons_death$utla$covid <- matrix(ncol=week_n, nrow=length(uni_utla2), dimnames=list(uni_utla2, as.character(ons_date)))
for (i in 1:length(uni_utla2)){
  utla_tab <- NULL
  utla_tab$all <- ons_death$ltla$all[lrf_lad_utla_reg[match(rownames(ons_death$ltla$all), lrf_lad_utla_reg[, "LAD19NM"]),"UTLA19NM"]==uni_utla2[i],]
  utla_tab$covid<- ons_death$ltla$covid[lrf_lad_utla_reg[match(rownames(ons_death$ltla$covid), lrf_lad_utla_reg[, "LAD19NM"]),"UTLA19NM"]==uni_utla2[i],]
  if(!is.null(dim(utla_tab$all))){
    ons_death$utla$all[i,] <- colSums(utla_tab$all, na.rm = T)
    ons_death$utla$covid[i,] <- colSums(utla_tab$covid, na.rm = T)
  }else{
    ons_death$utla$all[i,] <- utla_tab$all
    ons_death$utla$covid[i,] <- utla_tab$covid
  }
  }
                                                     

ons_death$lrf$all <- ons_death$lrf$covid <- matrix(ncol=length(ons_date), nrow=length(uni_lrf), dimnames=list(uni_lrf, as.character(ons_date)))
for (i in 1:length(uni_lrf)){
  lrf_tab <- NULL
  lrf_tab$all <- ons_death$utla$all[lrf_lad_utla_reg[match(rownames(ons_death$utla$all), lrf_lad_utla_reg[, "UTLA19NM"]),"LRF19NM"]==uni_lrf[i],]
  lrf_tab$covid <- ons_death$utla$covid[lrf_lad_utla_reg[match(rownames(ons_death$utla$all), lrf_lad_utla_reg[, "UTLA19NM"]),"LRF19NM"]==uni_lrf[i],]
   if(!is.null(dim(lrf_tab$all))){
    ons_death$lrf$all[i,] <- colSums(lrf_tab$all, na.rm = T)
    ons_death$lrf$covid[i,] <- colSums(lrf_tab$covid, na.rm = T)
  }else{
    ons_death$lrf$all[i,] <- lrf_tab$all
    ons_death$lrf$covid[i,] <- lrf_tab$covid
  }
}

ons_death$reg$all <- ons_death$reg$covid <- matrix(ncol=length(ons_date), nrow=length(uni_reg), dimnames=list(uni_reg, as.character(ons_date)))
for (i in 1:length(uni_reg)){
  reg_tab <- NULL
  reg_tab$all <- ons_death$lrf$all[lrf_lad_utla_reg[match(rownames(ons_death$lrf$all), lrf_lad_utla_reg[, "LRF19NM"]),"RGN19NM"]==uni_reg[i],]
  reg_tab$covid <- ons_death$lrf$covid[lrf_lad_utla_reg[match(rownames(ons_death$lrf$all), lrf_lad_utla_reg[, "LRF19NM"]),"RGN19NM"]==uni_reg[i],]
  if(!is.null(dim(reg_tab$all))){
    ons_death$reg$all[i,] <- colSums(reg_tab$all, na.rm = T)
    ons_death$reg$covid[i,] <- colSums(reg_tab$covid, na.rm = T)
  }else{
    ons_death$reg$all[i,] <- reg_tab$all
    ons_death$reg$covid[i,] <- reg_tab$covid
  }
}

for (i in 1:2){
  ons_death$eng[[i]] <- t(as.matrix(colSums(ons_death$reg[[i]])))
  rownames(ons_death$eng[[i]]) <- "England"
}
names(ons_death$eng) <- c("covid", "all")

for (i in 1:5){
  ons_death[[i]]$non <- ons_death[[i]]$all - ons_death[[i]]$covid
}

# ONS population
# src2 <- "https://www.ons.gov.uk/file?uri=%2fpersonspopulationandcommunity%2fpopulationandmigration%2fpopulationprojections%2fdatasets%2flocalAuthoritiesinenglandtable2%2f2018based/table2.xls"
# fn <- basename(src2)
# download.file(url = src2, destfile = fn)
# 
# all <- as.matrix(read_excel("data/table2.xls", sheet=4, skip=6))
# write.csv(all, "data/ons_pop.csv")
all <- read.csv("data/ons_pop.csv")

bands <- unique(all[, "AGE.GROUP"])
age_band <- NULL
for (i in 1:length(bands)){
  age_band[[i]] <- as.numeric(all[all[, "AGE.GROUP"]==bands[i],"X2020"])
  names(age_band[[i]]) <- all[all[, "AGE.GROUP"]==bands[i],"AREA"]
  
  age_band[[i]] <- c(age_band[[i]], "Bournemouth, Christchurch and Poole"=age_band[[i]]["Bournemouth"]+age_band[[i]]["Christchurch"]+age_band[[i]]["Poole"])
  age_band[[i]] <- c(age_band[[i]], "Cornwall and Isles of Scilly"=age_band[[i]]["Cornwall"]+age_band[[i]]["Isles of Scilly"])
}
names(age_band) <- bands

age_all <- age_band$"All ages"
age60 <- age_band$"60-64"+age_band$"65-69"+age_band$"70-74"+age_band$"75-79"+age_band$"80-84"+age_band$"85-89"+age_band$"90+"
names(age_all)[names(age_all)=="East"] <- names(age60)[names(age60)=="East"] <- "East of England"

ons_pop <- NULL
ons_pop$utla$age_all <- ons_pop$ltla$age_all  <- age_all
ons_pop$utla$age60 <- ons_pop$ltla$age60  <- age60

lrf_age <- lrf_age60 <- NULL
for (i in 1:length(uni_lrf)){
    ind <- which(lrf_lad_utla_reg[match(names(age_all), lrf_lad_utla_reg[, "LAD19NM"]),"LRF19NM"]==uni_lrf[i])
    lrf_age <- c(lrf_age, sum(age_all[ind]))
    lrf_age60 <- c(lrf_age60, sum(age60[ind]))
}
i <- 34
lrf_age[i] <- age_all["Suffolk"]
lrf_age60[i] <- age60["Suffolk"]
names(lrf_age) <- names(lrf_age60) <- uni_lrf
ons_pop$lrf$age_all <- lrf_age
ons_pop$lrf$age60 <- lrf_age60

ons_pop$reg$age_all <- age_all[uni_reg]
ons_pop$reg$age60 <- age60[uni_reg]

ons_pop$eng$age_all <- age_all["England"]
ons_pop$eng$age60 <- age60["England"]
age_all[uni_reg]

for (i in 1:5){
  for (j in 1:2){
    ons_pop[[i]][[j]] <- ons_pop[[i]][[j]]/1e5
  }
}

# making plotting data
cases <- NULL
for (i in 4:1){
  for (j in 6:1){
  cases[[i]][[j]] <- data_transform(phe_data[[i]], j, ons_pop[[i+1]])
  }
}

deaths <- NULL
for (i in 5:1){
  for (j in 3:1){
  for (k in 6:1){
    deaths[[i]][[j]][[k]] <- data_transform(ons_death[[i]][[j]], k, ons_pop[[i]])
  }
  }
}
      

# colors
n_col <- 20
ana_cols <- as.character(read.table("data/ana-cols.txt", comment.char = "?")[,1])

col_cou <- c("red", "#0065BF", "#00ad36", "#FFCD01", "black")
multi_col <- rep(ana_cols, ceiling(nrow(ons_death$ltla$all)/n_col))

trans_grey <- rgb(.5,.5,.5,.25)


# Calculations====

servicedata$Sex <- str_to_upper(servicedata$Sex)
servicedata$Partners <- str_to_upper(servicedata$Partners)
visitdata$PartnersLast <- str_to_upper(visitdata$PartnersLast)

visitvector <- visitdata$URNO %in% servicedata$URNO
visitdata <- visitdata[visitvector,]

# Update SW Last ====
servicedata[is.na(servicedata$SW),"SW"] <- 9
servicedata[servicedata$SW == 2, "SW"] <- 3
servicedata[servicedata$SW == 1, "SW"] <- 2

swservicedata <- data.table(URNO = sort(na.exclude(unique(
                    visitdata[visitdata$SWLast == 1 | visitdata$SWLast == 3,"URNO"]
                    ))))

servicedata <- within(servicedata, SW[servicedata$URNO %in% swservicedata$URNO] <- 1)

# Update PartnersLast ====

servicedata$Partners <- "U"

plservicedataN <- data.table(URNO = sort(na.exclude(unique(
                     visitdata[visitdata$PartnersLast == "N","URNO"]
                     ))))

plservicedataO <- data.table(URNO = sort(na.exclude(unique(
                     visitdata[visitdata$PartnersLast == "O","URNO"]
                     ))))

plservicedataS <- data.table(URNO = sort(na.exclude(unique(
                     visitdata[visitdata$PartnersLast == "S","URNO"]
                     ))))

plservicedataB <- data.table(URNO = sort(na.exclude(unique(
                     visitdata[visitdata$PartnersLast == "B","URNO"]
                     ))))

plservicedata <- data.table(URNO = sort(na.exclude(unique(
                    visitdata[,"URNO"]
                    ))))


servicedata <- within(servicedata, Partners[servicedata$URNO %in% plservicedataN$URNO] <- "N")
servicedata <- within(servicedata, Partners[servicedata$URNO %in% plservicedataO$URNO] <- "O")
servicedata <- within(servicedata, Partners[servicedata$URNO %in% plservicedataS$URNO] <- "S")
servicedata <- within(servicedata, Partners[servicedata$URNO %in% plservicedataB$URNO] <- "B")

# Update IDULast ====
servicedata[is.na(servicedata$IDU),"IDU"] <- 9
servicedata[servicedata$IDU == 2, "IDU"] <- 3
servicedata[servicedata$IDU == 1, "IDU"] <- 2

IDUservicedata <- data.table(URNO = sort(na.exclude(unique(
  visitdata[visitdata$IDULast == 1,"URNO"]
))))

servicedata <- within(servicedata, IDU[servicedata$URNO %in% IDUservicedata$URNO] <- 1)

servicedata_lab <<- subset(servicedata, LabGroupCode==12)

servicedata_rapid <<- subset(servicedata, LabGroupCode==96)

servicedata_poct <<- subset(servicedata, LabNumber==96)

servicedata_poct_male <- subset(servicedata_poct, Sex=="M")

servicedata_poct_msm <- subset(servicedata_poct, Sex=="M" & {Partners=="S" | Partners=="B"})

###############################################################################

# LabTesting

###############################################################################

## Age

lab_age_01 <- c(
  nrow(subset(servicedata_lab, (is.na(Age)) & Sex=="M")),
  nrow(subset(servicedata_lab, (is.na(Age)) & Sex=="F" )),
  nrow(subset(servicedata_lab, (is.na(Age)))),
  nrow(subset(servicedata_lab, (is.na(Age)) & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, (is.na(Age)) & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, (is.na(Age)) & Colour==12)))

lab_age_02 <- c(
  nrow(subset(servicedata_lab, {Age<15} & Sex=="M" )),
  nrow(subset(servicedata_lab, {Age<15} & Sex=="F" )),
  nrow(subset(servicedata_lab, {Age<15})),
  nrow(subset(servicedata_lab, {Age<15} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age<15} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age<15} & Colour==12)))

lab_age_03 <- c(
  nrow(subset(servicedata_lab, {Age>=15 & Age<=19} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=15 & Age<=19} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=15 & Age<=19})),
  nrow(subset(servicedata_lab, {Age>=15 & Age<=19} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=15 & Age<=19} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=15 & Age<=19} & Colour==12)))

lab_age_04 <- c(
  nrow(subset(servicedata_lab, {Age>=20 & Age<=24} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=20 & Age<=24} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=20 & Age<=24})),
  nrow(subset(servicedata_lab, {Age>=20 & Age<=24} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=20 & Age<=24} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=20 & Age<=24} & Colour==12)))

lab_age_05 <- c(
  nrow(subset(servicedata_lab, {Age>=25 & Age<=29} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=25 & Age<=29} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=25 & Age<=29})),
  nrow(subset(servicedata_lab, {Age>=25 & Age<=29} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=25 & Age<=29} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=25 & Age<=29} & Colour==12)))

lab_age_06 <- c(
  nrow(subset(servicedata_lab, {Age>=30 & Age<=34} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=30 & Age<=34} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=30 & Age<=34})),
  nrow(subset(servicedata_lab, {Age>=30 & Age<=34} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=30 & Age<=34} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=30 & Age<=34} & Colour==12)))

lab_age_07 <- c(
  nrow(subset(servicedata_lab, {Age>=35 & Age<=39} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=35 & Age<=39} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=35 & Age<=39})),
  nrow(subset(servicedata_lab, {Age>=35 & Age<=39} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=35 & Age<=39} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=35 & Age<=39} & Colour==12)))

lab_age_08 <- c(
  nrow(subset(servicedata_lab, {Age>=40 & Age<=44} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=40 & Age<=44} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=40 & Age<=44})),
  nrow(subset(servicedata_lab, {Age>=40 & Age<=44} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=40 & Age<=44} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=40 & Age<=44} & Colour==12)))

lab_age_09 <- c(
  nrow(subset(servicedata_lab, {Age>=45 & Age<=49} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=45 & Age<=49} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=45 & Age<=49})),
  nrow(subset(servicedata_lab, {Age>=45 & Age<=49} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=45 & Age<=49} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=45 & Age<=49} & Colour==12)))

lab_age_10 <- c(
  nrow(subset(servicedata_lab, {Age>=50 & Age<=54} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=50 & Age<=54} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=50 & Age<=54})),
  nrow(subset(servicedata_lab, {Age>=50 & Age<=54} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=50 & Age<=54} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=50 & Age<=54} & Colour==12)))

lab_age_11 <- c(
  nrow(subset(servicedata_lab, {Age>=55 & Age<=59} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=55 & Age<=59} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=55 & Age<=59})),
  nrow(subset(servicedata_lab, {Age>=55 & Age<=59} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=55 & Age<=59} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=55 & Age<=59} & Colour==12)))

lab_age_12 <- c(
  nrow(subset(servicedata_lab, {Age>=60 & Age<=64} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=60 & Age<=64} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=60 & Age<=64})),
  nrow(subset(servicedata_lab, {Age>=60 & Age<=64} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=60 & Age<=64} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=60 & Age<=64} & Colour==12)))                

lab_age_13 <- c(
  nrow(subset(servicedata_lab, {Age>=65} & Sex=="M")),
  nrow(subset(servicedata_lab, {Age>=65} & Sex=="F")),
  nrow(subset(servicedata_lab, {Age>=65})),
  nrow(subset(servicedata_lab, {Age>=65} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=65} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_lab, {Age>=65} & Colour==12)))

lab_age <<- rbind(lab_age_01,
                  lab_age_02,
                  lab_age_03,
                  lab_age_04,
                  lab_age_05,
                  lab_age_06,
                  lab_age_07,
                  lab_age_08,
                  lab_age_09,
                  lab_age_10,
                  lab_age_11,
                  lab_age_12,
                  lab_age_13
                  )

rm(lab_age_01,
   lab_age_02,
   lab_age_03,
   lab_age_04,
   lab_age_05,
   lab_age_06,
   lab_age_07,
   lab_age_08,
   lab_age_09,
   lab_age_10,
   lab_age_11,
   lab_age_12,
   lab_age_13
   )

## ATSI

lab_atsi_1 <- c(nrow(subset(servicedata_lab, Indigenous == 1)),
                nrow(subset(servicedata_lab, Indigenous == 1 & Colour == 12)))
lab_atsi_2 <- c(nrow(subset(servicedata_lab, Indigenous == 2)),
                nrow(subset(servicedata_lab, Indigenous == 2 & Colour == 12)))
lab_atsi_3 <- c(nrow(subset(servicedata_lab, Indigenous == 3)),
                nrow(subset(servicedata_lab, Indigenous == 3 & Colour == 12)))
lab_atsi_4 <- c(nrow(subset(servicedata_lab, Indigenous == 4)),
                nrow(subset(servicedata_lab, Indigenous == 4 & Colour == 12)))
lab_atsi_9 <- c(nrow(subset(servicedata_lab, Indigenous == 9 | is.na(Indigenous))),
                nrow(subset(servicedata_lab, {Indigenous == 9 | is.na(Indigenous)} & Colour==12)))

lab_atsi <<- rbind(lab_atsi_1,
                   lab_atsi_2,
                   lab_atsi_3,
                   lab_atsi_4,
                   lab_atsi_9
                   )
rm(lab_atsi_1,
   lab_atsi_2,
   lab_atsi_3,
   lab_atsi_4,
   lab_atsi_9
   )

## IDU
lab_idu_1 <- c(nrow(subset(servicedata_lab, IDU == 1)),
               nrow(subset(servicedata_lab, IDU == 1 & Colour==12))                 )
lab_idu_2 <- c(nrow(subset(servicedata_lab, IDU == 2)),
               nrow(subset(servicedata_lab, IDU == 2 & Colour==12)))
lab_idu_3 <- c(nrow(subset(servicedata_lab, IDU == 3)),
               nrow(subset(servicedata_lab, IDU == 3 & Colour==12)))
lab_idu_9 <- c(nrow(subset(servicedata_lab, IDU == 9 | is.na(IDU))),
               nrow(subset(servicedata_lab, {IDU == 9 | is.na(IDU)} & Colour==12)))

lab_idu <<- rbind(lab_idu_1,
                  lab_idu_2,
                  lab_idu_3,
                  lab_idu_9
                  )

rm(lab_idu_1,
   lab_idu_2,
   lab_idu_3,
   lab_idu_9
   )

## Risk
lab_risk_1 <- c(
  nrow(subset(servicedata_lab, Partners== "O" & Sex=="M")),
  nrow(subset(servicedata_lab, Partners== "O"& Sex=="F")),
  nrow(subset(servicedata_lab, Partners== "O")),
  nrow(subset(servicedata_lab, Partners== "O" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_lab, Partners== "O"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_lab, Partners== "O" & Colour == 12)))
lab_risk_2 <- c(
  nrow(subset(servicedata_lab, Partners== "B" & Sex=="M")),
  nrow(subset(servicedata_lab, Partners== "B"& Sex=="F")),
  nrow(subset(servicedata_lab, Partners== "B")),
  nrow(subset(servicedata_lab, Partners== "B" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_lab, Partners== "B"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_lab, Partners== "B" & Colour == 12)))
lab_risk_3 <- c(
  nrow(subset(servicedata_lab, Partners== "S" & Sex=="M")),
  nrow(subset(servicedata_lab, Partners== "S"& Sex=="F")),
  nrow(subset(servicedata_lab, Partners== "S")),
  nrow(subset(servicedata_lab, Partners== "S" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_lab, Partners== "S"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_lab, Partners== "S" & Colour == 12)))
lab_risk_4 <- c(
  nrow(subset(servicedata_lab, Partners== "N" & Sex=="M")),
  nrow(subset(servicedata_lab, Partners== "N"& Sex=="F")),
  nrow(subset(servicedata_lab, Partners== "N")),
  nrow(subset(servicedata_lab, Partners== "N" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_lab, Partners== "N"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_lab, Partners== "N" & Colour == 12)))
lab_risk_5 <- c(
  nrow(subset(servicedata_lab, {Partners== "U" | is.na(Partners)} & Sex=="M")),
  nrow(subset(servicedata_lab, {Partners== "U" | is.na(Partners)} & Sex=="F")),
  nrow(subset(servicedata_lab, {Partners== "U" | is.na(Partners)})),
  nrow(subset(servicedata_lab, {Partners== "U" | is.na(Partners)} & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_lab, {Partners== "U" | is.na(Partners)} & Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_lab, {Partners== "U" | is.na(Partners)} & Colour == 12)))

lab_risk <<- rbind(lab_risk_1,
                   lab_risk_2,
                   lab_risk_3,
                   lab_risk_4,
                   lab_risk_5
                   )

rm(lab_risk_1,
   lab_risk_2,
   lab_risk_3,
   lab_risk_4,
   lab_risk_5
   )

## SW
lab_sw_1 <- c(nrow(subset(servicedata_lab, SW == 1)),
              nrow(subset(servicedata_lab, SW == 1 & Colour==12))                 )
lab_sw_2 <- c(nrow(subset(servicedata_lab, SW == 2)),
              nrow(subset(servicedata_lab, SW == 2 & Colour==12)))
lab_sw_3 <- c(nrow(subset(servicedata_lab, SW == 3)),
              nrow(subset(servicedata_lab, SW == 3 & Colour==12)))
lab_sw_9 <- c(nrow(subset(servicedata_lab, SW == 9 | is.na(SW))),
              nrow(subset(servicedata_lab, {SW == 9 | is.na(SW)} & Colour==12)))

lab_sw <<- rbind(lab_sw_1,
                 lab_sw_2,
                 lab_sw_3,
                 lab_sw_9
                 )

rm(lab_sw_1,
   lab_sw_2,
   lab_sw_3,
   lab_sw_9
   )



###############################################################################

# RapidTesting

###############################################################################

## Age

rapid_age_01 <- c(
  nrow(subset(servicedata_rapid, (is.na(Age)) & Sex=="M")),
  nrow(subset(servicedata_rapid, (is.na(Age)) & Sex=="F" )),
  nrow(subset(servicedata_rapid, (is.na(Age)))),
  nrow(subset(servicedata_rapid, (is.na(Age)) & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, (is.na(Age)) & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, (is.na(Age)) & Colour==12)))

rapid_age_02 <- c(
  nrow(subset(servicedata_rapid, {Age<15} & Sex=="M" )),
  nrow(subset(servicedata_rapid, {Age<15} & Sex=="F" )),
  nrow(subset(servicedata_rapid, {Age<15})),
  nrow(subset(servicedata_rapid, {Age<15} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age<15} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age<15} & Colour==12)))

rapid_age_03 <- c(
  nrow(subset(servicedata_rapid, {Age>=15 & Age<=19} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=15 & Age<=19} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=15 & Age<=19})),
  nrow(subset(servicedata_rapid, {Age>=15 & Age<=19} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=15 & Age<=19} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=15 & Age<=19} & Colour==12)))

rapid_age_04 <- c(
  nrow(subset(servicedata_rapid, {Age>=20 & Age<=24} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=20 & Age<=24} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=20 & Age<=24})),
  nrow(subset(servicedata_rapid, {Age>=20 & Age<=24} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=20 & Age<=24} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=20 & Age<=24} & Colour==12)))

rapid_age_05 <- c(
  nrow(subset(servicedata_rapid, {Age>=25 & Age<=29} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=25 & Age<=29} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=25 & Age<=29})),
  nrow(subset(servicedata_rapid, {Age>=25 & Age<=29} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=25 & Age<=29} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=25 & Age<=29} & Colour==12)))

rapid_age_06 <- c(
  nrow(subset(servicedata_rapid, {Age>=30 & Age<=34} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=30 & Age<=34} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=30 & Age<=34})),
  nrow(subset(servicedata_rapid, {Age>=30 & Age<=34} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=30 & Age<=34} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=30 & Age<=34} & Colour==12)))

rapid_age_07 <- c(
  nrow(subset(servicedata_rapid, {Age>=35 & Age<=39} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=35 & Age<=39} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=35 & Age<=39})),
  nrow(subset(servicedata_rapid, {Age>=35 & Age<=39} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=35 & Age<=39} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=35 & Age<=39} & Colour==12)))

rapid_age_08 <- c(
  nrow(subset(servicedata_rapid, {Age>=40 & Age<=44} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=40 & Age<=44} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=40 & Age<=44})),
  nrow(subset(servicedata_rapid, {Age>=40 & Age<=44} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=40 & Age<=44} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=40 & Age<=44} & Colour==12)))

rapid_age_09 <- c(
  nrow(subset(servicedata_rapid, {Age>=45 & Age<=49} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=45 & Age<=49} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=45 & Age<=49})),
  nrow(subset(servicedata_rapid, {Age>=45 & Age<=49} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=45 & Age<=49} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=45 & Age<=49} & Colour==12)))

rapid_age_10 <- c(
  nrow(subset(servicedata_rapid, {Age>=50 & Age<=54} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=50 & Age<=54} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=50 & Age<=54})),
  nrow(subset(servicedata_rapid, {Age>=50 & Age<=54} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=50 & Age<=54} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=50 & Age<=54} & Colour==12)))

rapid_age_11 <- c(
  nrow(subset(servicedata_rapid, {Age>=55 & Age<=59} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=55 & Age<=59} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=55 & Age<=59})),
  nrow(subset(servicedata_rapid, {Age>=55 & Age<=59} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=55 & Age<=59} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=55 & Age<=59} & Colour==12)))

rapid_age_12 <- c(
  nrow(subset(servicedata_rapid, {Age>=60 & Age<=64} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=60 & Age<=64} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=60 & Age<=64})),
  nrow(subset(servicedata_rapid, {Age>=60 & Age<=64} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=60 & Age<=64} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=60 & Age<=64} & Colour==12)))                

rapid_age_13 <- c(
  nrow(subset(servicedata_rapid, {Age>=65} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Age>=65} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Age>=65})),
  nrow(subset(servicedata_rapid, {Age>=65} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=65} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_rapid, {Age>=65} & Colour==12)))

rapid_age <<- rbind(rapid_age_01,
                  rapid_age_02,
                  rapid_age_03,
                  rapid_age_04,
                  rapid_age_05,
                  rapid_age_06,
                  rapid_age_07,
                  rapid_age_08,
                  rapid_age_09,
                  rapid_age_10,
                  rapid_age_11,
                  rapid_age_12,
                  rapid_age_13
)

rm(rapid_age_01,
   rapid_age_02,
   rapid_age_03,
   rapid_age_04,
   rapid_age_05,
   rapid_age_06,
   rapid_age_07,
   rapid_age_08,
   rapid_age_09,
   rapid_age_10,
   rapid_age_11,
   rapid_age_12,
   rapid_age_13
)

## ATSI

rapid_atsi_1 <- c(nrow(subset(servicedata_rapid, Indigenous == 1)),
                nrow(subset(servicedata_rapid, Indigenous == 1 & Colour == 12)))
rapid_atsi_2 <- c(nrow(subset(servicedata_rapid, Indigenous == 2)),
                nrow(subset(servicedata_rapid, Indigenous == 2 & Colour == 12)))
rapid_atsi_3 <- c(nrow(subset(servicedata_rapid, Indigenous == 3)),
                nrow(subset(servicedata_rapid, Indigenous == 3 & Colour == 12)))
rapid_atsi_4 <- c(nrow(subset(servicedata_rapid, Indigenous == 4)),
                nrow(subset(servicedata_rapid, Indigenous == 4 & Colour == 12)))
rapid_atsi_9 <- c(nrow(subset(servicedata_rapid, Indigenous == 9 | is.na(Indigenous))),
                nrow(subset(servicedata_rapid, {Indigenous == 9 | is.na(Indigenous)} & Colour==12)))

rapid_atsi <<- rbind(rapid_atsi_1,
                   rapid_atsi_2,
                   rapid_atsi_3,
                   rapid_atsi_4,
                   rapid_atsi_9
)
rm(rapid_atsi_1,
   rapid_atsi_2,
   rapid_atsi_3,
   rapid_atsi_4,
   rapid_atsi_9
)

## IDU
rapid_idu_1 <- c(nrow(subset(servicedata_rapid, IDU == 1)),
               nrow(subset(servicedata_rapid, IDU == 1 & Colour==12))                 )
rapid_idu_2 <- c(nrow(subset(servicedata_rapid, IDU == 2)),
               nrow(subset(servicedata_rapid, IDU == 2 & Colour==12)))
rapid_idu_3 <- c(nrow(subset(servicedata_rapid, IDU == 3)),
               nrow(subset(servicedata_rapid, IDU == 3 & Colour==12)))
rapid_idu_9 <- c(nrow(subset(servicedata_rapid, IDU == 9 | is.na(IDU))),
               nrow(subset(servicedata_rapid, {IDU == 9 | is.na(IDU)} & Colour==12)))

rapid_idu <<- rbind(rapid_idu_1,
                  rapid_idu_2,
                  rapid_idu_3,
                  rapid_idu_9
)

rm(rapid_idu_1,
   rapid_idu_2,
   rapid_idu_3,
   rapid_idu_9
)

## Risk
rapid_risk_1 <- c(
  nrow(subset(servicedata_rapid, Partners== "O" & Sex=="M")),
  nrow(subset(servicedata_rapid, Partners== "O"& Sex=="F")),
  nrow(subset(servicedata_rapid, Partners== "O")),
  nrow(subset(servicedata_rapid, Partners== "O" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_rapid, Partners== "O"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_rapid, Partners== "O" & Colour == 12)))
rapid_risk_2 <- c(
  nrow(subset(servicedata_rapid, Partners== "B" & Sex=="M")),
  nrow(subset(servicedata_rapid, Partners== "B"& Sex=="F")),
  nrow(subset(servicedata_rapid, Partners== "B")),
  nrow(subset(servicedata_rapid, Partners== "B" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_rapid, Partners== "B"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_rapid, Partners== "B" & Colour == 12)))
rapid_risk_3 <- c(
  nrow(subset(servicedata_rapid, Partners== "S" & Sex=="M")),
  nrow(subset(servicedata_rapid, Partners== "S"& Sex=="F")),
  nrow(subset(servicedata_rapid, Partners== "S")),
  nrow(subset(servicedata_rapid, Partners== "S" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_rapid, Partners== "S"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_rapid, Partners== "S" & Colour == 12)))
rapid_risk_4 <- c(
  nrow(subset(servicedata_rapid, Partners== "N" & Sex=="M")),
  nrow(subset(servicedata_rapid, Partners== "N"& Sex=="F")),
  nrow(subset(servicedata_rapid, Partners== "N")),
  nrow(subset(servicedata_rapid, Partners== "N" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_rapid, Partners== "N"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_rapid, Partners== "N" & Colour == 12)))
rapid_risk_5 <- c(
  nrow(subset(servicedata_rapid, {Partners== "U" | is.na(Partners)} & Sex=="M")),
  nrow(subset(servicedata_rapid, {Partners== "U" | is.na(Partners)} & Sex=="F")),
  nrow(subset(servicedata_rapid, {Partners== "U" | is.na(Partners)})),
  nrow(subset(servicedata_rapid, {Partners== "U" | is.na(Partners)} & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_rapid, {Partners== "U" | is.na(Partners)} & Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_rapid, {Partners== "U" | is.na(Partners)} & Colour == 12)))

rapid_risk <<- rbind(rapid_risk_1,
                   rapid_risk_2,
                   rapid_risk_3,
                   rapid_risk_4,
                   rapid_risk_5
)

rm(rapid_risk_1,
   rapid_risk_2,
   rapid_risk_3,
   rapid_risk_4,
   rapid_risk_5
)

## SW
rapid_sw_1 <- c(nrow(subset(servicedata_rapid, SW == 1)),
              nrow(subset(servicedata_rapid, SW == 1 & Colour==12))                 )
rapid_sw_2 <- c(nrow(subset(servicedata_rapid, SW == 2)),
              nrow(subset(servicedata_rapid, SW == 2 & Colour==12)))
rapid_sw_3 <- c(nrow(subset(servicedata_rapid, SW == 3)),
              nrow(subset(servicedata_rapid, SW == 3 & Colour==12)))
rapid_sw_9 <- c(nrow(subset(servicedata_rapid, SW == 9 | is.na(SW))),
              nrow(subset(servicedata_rapid, {SW == 9 | is.na(SW)} & Colour==12)))

rapid_sw <<- rbind(rapid_sw_1,
                 rapid_sw_2,
                 rapid_sw_3,
                 rapid_sw_9
)

rm(rapid_sw_1,
   rapid_sw_2,
   rapid_sw_3,
   rapid_sw_9
)






###############################################################################

# POCT Testing

###############################################################################

## Age====
poct_age_01 <- c(
  nrow(subset(servicedata_poct, (is.na(Age)) & Sex=="M")),
  nrow(subset(servicedata_poct, (is.na(Age)) & Sex=="F")),
  nrow(subset(servicedata_poct, (is.na(Age)))),
  nrow(subset(servicedata_poct, (is.na(Age)) & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, (is.na(Age)) & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, (is.na(Age)) & Colour==12)))

poct_age_02 <- c(
  nrow(subset(servicedata_poct, {Age<15} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age<15} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age<15})),
  nrow(subset(servicedata_poct, {Age<15} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age<15} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age<15} & Colour==12)))

poct_age_03 <- c(
  nrow(subset(servicedata_poct, {Age>=15 & Age<=19} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=15 & Age<=19} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=15 & Age<=19})),
  nrow(subset(servicedata_poct, {Age>=15 & Age<=19} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=15 & Age<=19} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=15 & Age<=19} & Colour==12)))

poct_age_04 <- c(
  nrow(subset(servicedata_poct, {Age>=20 & Age<=24} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=20 & Age<=24} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=20 & Age<=24})),
  nrow(subset(servicedata_poct, {Age>=20 & Age<=24} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=20 & Age<=24} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=20 & Age<=24} & Colour==12)))

poct_age_05 <- c(
  nrow(subset(servicedata_poct, {Age>=25 & Age<=29} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=25 & Age<=29} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=25 & Age<=29})),
  nrow(subset(servicedata_poct, {Age>=25 & Age<=29} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=25 & Age<=29} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=25 & Age<=29} & Colour==12)))

poct_age_06 <- c(
  nrow(subset(servicedata_poct, {Age>=30 & Age<=34} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=30 & Age<=34} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=30 & Age<=34})),
  nrow(subset(servicedata_poct, {Age>=30 & Age<=34} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=30 & Age<=34} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=30 & Age<=34} & Colour==12)))

poct_age_07 <- c(
  nrow(subset(servicedata_poct, {Age>=35 & Age<=39} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=35 & Age<=39} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=35 & Age<=39})),
  nrow(subset(servicedata_poct, {Age>=35 & Age<=39} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=35 & Age<=39} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=35 & Age<=39} & Colour==12)))

poct_age_08 <- c(
  nrow(subset(servicedata_poct, {Age>=40 & Age<=44} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=40 & Age<=44} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=40 & Age<=44})),
  nrow(subset(servicedata_poct, {Age>=40 & Age<=44} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=40 & Age<=44} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=40 & Age<=44} & Colour==12)))

poct_age_09 <- c(
  nrow(subset(servicedata_poct, {Age>=45 & Age<=49} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=45 & Age<=49} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=45 & Age<=49})),
  nrow(subset(servicedata_poct, {Age>=45 & Age<=49} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=45 & Age<=49} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=45 & Age<=49} & Colour==12)))

poct_age_10 <- c(
  nrow(subset(servicedata_poct, {Age>=50 & Age<=54} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=50 & Age<=54} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=50 & Age<=54})),
  nrow(subset(servicedata_poct, {Age>=50 & Age<=54} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=50 & Age<=54} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=50 & Age<=54} & Colour==12)))

poct_age_11 <- c(
  nrow(subset(servicedata_poct, {Age>=55 & Age<=59} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=55 & Age<=59} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=55 & Age<=59})),
  nrow(subset(servicedata_poct, {Age>=55 & Age<=59} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=55 & Age<=59} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=55 & Age<=59} & Colour==12)))

poct_age_12 <- c(
  nrow(subset(servicedata_poct, {Age>=60 & Age<=64} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=60 & Age<=64} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=60 & Age<=64})),
  nrow(subset(servicedata_poct, {Age>=60 & Age<=64} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=60 & Age<=64} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=60 & Age<=64} & Colour==12)))                

poct_age_13 <- c(
  nrow(subset(servicedata_poct, {Age>=65} & Sex=="M")),
  nrow(subset(servicedata_poct, {Age>=65} & Sex=="F")),
  nrow(subset(servicedata_poct, {Age>=65})),
  nrow(subset(servicedata_poct, {Age>=65} & Sex=="M" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=65} & Sex=="F" & Colour==12)),
  nrow(subset(servicedata_poct, {Age>=65} & Colour==12)))

poct_age <<- rbind(poct_age_01,
                   poct_age_02,
                   poct_age_03,
                   poct_age_04,
                   poct_age_05,
                   poct_age_06,
                   poct_age_07,
                   poct_age_08,
                   poct_age_09,
                   poct_age_10,
                   poct_age_11,
                   poct_age_12,
                   poct_age_13
                   )

rm(poct_age_01,
   poct_age_02,
   poct_age_03,
   poct_age_04,
   poct_age_05,
   poct_age_06,
   poct_age_07,
   poct_age_08,
   poct_age_09,
   poct_age_10,
   poct_age_11,
   poct_age_12,
   poct_age_13
   )

## ATSI====
poct_atsi_1 <- c(nrow(subset(servicedata_poct, Indigenous == 1)),
                 nrow(subset(servicedata_poct, Indigenous == 1 & Colour==12))                 )
poct_atsi_2 <- c(nrow(subset(servicedata_poct, Indigenous == 2)),
                 nrow(subset(servicedata_poct, Indigenous == 2 & Colour==12)))
poct_atsi_3 <- c(nrow(subset(servicedata_poct, Indigenous == 3)),
                 nrow(subset(servicedata_poct, Indigenous == 3 & Colour==12)))
poct_atsi_4 <- c(nrow(subset(servicedata_poct, Indigenous == 4)),
                 nrow(subset(servicedata_poct, Indigenous ==4 & Colour==12)))
poct_atsi_9 <- c(nrow(subset(servicedata_poct, Indigenous == 9 | is.na(Indigenous))),
                 nrow(subset(servicedata_poct, {Indigenous == 9 | is.na(Indigenous)} & Colour==12)))

poct_atsi <<- rbind(poct_atsi_1,
                    poct_atsi_2,
                    poct_atsi_3,
                    poct_atsi_4,
                    poct_atsi_9
                    )

rm(poct_atsi_1,
   poct_atsi_2,
   poct_atsi_3,
   poct_atsi_4,
   poct_atsi_9
   )

## IDU====
poct_idu_1 <- c(nrow(subset(servicedata_poct, IDU == 1)),
                nrow(subset(servicedata_poct, IDU == 1 & Colour==12))                 )
poct_idu_2 <- c(nrow(subset(servicedata_poct, IDU == 2)),
                nrow(subset(servicedata_poct, IDU == 2 & Colour==12)))
poct_idu_3 <- c(nrow(subset(servicedata_poct, IDU == 3)),
                nrow(subset(servicedata_poct, IDU == 3 & Colour==12)))
poct_idu_9 <- c(nrow(subset(servicedata_poct, IDU == 9 | is.na(IDU))),
                nrow(subset(servicedata_poct, {IDU == 9 | is.na(IDU)} & Colour==12)))

poct_idu <<- rbind(poct_idu_1,
                   poct_idu_2,
                   poct_idu_3,
                   poct_idu_9
                   )

rm(poct_idu_1,
   poct_idu_2,
   poct_idu_3,
   poct_idu_9
   )

## Risk====
poct_risk_1 <- c(
  nrow(subset(servicedata_poct, Partners== "O" & Sex=="M")),
  nrow(subset(servicedata_poct, Partners== "O"& Sex=="F")),
  nrow(subset(servicedata_poct, Partners== "O")),
  nrow(subset(servicedata_poct, Partners== "O" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_poct, Partners== "O"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_poct, Partners== "O" & Colour == 12)))
poct_risk_2 <- c(
  nrow(subset(servicedata_poct, Partners== "B" & Sex=="M")),
  nrow(subset(servicedata_poct, Partners== "B"& Sex=="F")),
  nrow(subset(servicedata_poct, Partners== "B")),
  nrow(subset(servicedata_poct, Partners== "B" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_poct, Partners== "B"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_poct, Partners== "B" & Colour == 12)))
poct_risk_3 <- c(
  nrow(subset(servicedata_poct, Partners== "S" & Sex=="M")),
  nrow(subset(servicedata_poct, Partners== "S"& Sex=="F")),
  nrow(subset(servicedata_poct, Partners== "S")),
  nrow(subset(servicedata_poct, Partners== "S" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_poct, Partners== "S"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_poct, Partners== "S" & Colour == 12)))
poct_risk_4 <- c(
  nrow(subset(servicedata_poct, Partners== "N" & Sex=="M")),
  nrow(subset(servicedata_poct, Partners== "N"& Sex=="F")),
  nrow(subset(servicedata_poct, Partners== "N")),
  nrow(subset(servicedata_poct, Partners== "N" & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_poct, Partners== "N"& Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_poct, Partners== "N" & Colour == 12)))
poct_risk_5 <- c(
  nrow(subset(servicedata_poct, {Partners== "U" | is.na(Partners)} & Sex=="M")),
  nrow(subset(servicedata_poct, {Partners== "U" | is.na(Partners)} & Sex=="F")),
  nrow(subset(servicedata_poct, {Partners== "U" | is.na(Partners)})),
  nrow(subset(servicedata_poct, {Partners== "U" | is.na(Partners)} & Sex=="M" & Colour == 12)),
  nrow(subset(servicedata_poct, {Partners== "U" | is.na(Partners)} & Sex=="F" & Colour == 12)),
  nrow(subset(servicedata_poct, {Partners == "U" | is.na(Partners)} & Colour == 12)))

poct_risk <<- rbind(poct_risk_1,
                    poct_risk_2,
                    poct_risk_3,
                    poct_risk_4,
                    poct_risk_5
                    )

rm(poct_risk_1,
   poct_risk_2,
   poct_risk_3,
   poct_risk_4,
   poct_risk_5
   )

## SW====
poct_sw_1 <- c(nrow(subset(servicedata_poct, SW == 1)),
               nrow(subset(servicedata_poct, SW == 1 & Colour==12))                 )
poct_sw_2 <- c(nrow(subset(servicedata_poct, SW == 2)),
               nrow(subset(servicedata_poct, SW == 2 & Colour==12)))
poct_sw_3 <- c(nrow(subset(servicedata_poct, SW == 3)),
               nrow(subset(servicedata_poct, SW == 3 & Colour==12)))
poct_sw_9 <- c(nrow(subset(servicedata_poct, SW == 9 | is.na(SW))),
               nrow(subset(servicedata_poct, {SW == 9 | is.na(SW)} & Colour==12)))

poct_sw <<- rbind(poct_sw_1,
                  poct_sw_2,
                  poct_sw_3,
                  poct_sw_9
                  )

rm(poct_sw_1,
   poct_sw_2,
   poct_sw_3,
   poct_sw_9
   )

## Condom Use====
poct_condom_use_1 <- c(
  nrow(subset(servicedata_poct_male, CondomRegularMaleAnal==1)),
  nrow(subset(servicedata_poct_male,  CondomCasualMaleAnal==1)),
  nrow(subset(servicedata_poct_male, CondomRegularFemale==1)),
  nrow(subset(servicedata_poct_male,  CondomCasualFemale==1)))

poct_condom_use_2 <- c(
  nrow(subset(servicedata_poct_male, CondomRegularMaleAnal==2)),
  nrow(subset(servicedata_poct_male,  CondomCasualMaleAnal==2)),
  nrow(subset(servicedata_poct_male, CondomRegularFemale==2)),
  nrow(subset(servicedata_poct_male,  CondomCasualFemale==2)))

poct_condom_use_3 <- c(
  nrow(subset(servicedata_poct_male, CondomRegularMaleAnal==3)),
  nrow(subset(servicedata_poct_male,  CondomCasualMaleAnal==3)),
  nrow(subset(servicedata_poct_male, CondomRegularFemale==3)),
  nrow(subset(servicedata_poct_male,  CondomCasualFemale==3)))

poct_condom_use_4 <- c(
  nrow(subset(servicedata_poct_male, CondomRegularMaleAnal==4)),
  nrow(subset(servicedata_poct_male,  CondomCasualMaleAnal==4)),
  nrow(subset(servicedata_poct_male, CondomRegularFemale==4)),
  nrow(subset(servicedata_poct_male,  CondomCasualFemale==4)))

poct_condom_use_5 <- c(
  nrow(subset(servicedata_poct_male, CondomRegularMaleAnal==5)),
  nrow(subset(servicedata_poct_male,  CondomCasualMaleAnal==5)),
  nrow(subset(servicedata_poct_male, CondomRegularFemale==5)),
  nrow(subset(servicedata_poct_male,  CondomCasualFemale==5)))


poct_condom_use <<- rbind(poct_condom_use_1,
                          poct_condom_use_2,
                          poct_condom_use_3,
                          poct_condom_use_4,
                          poct_condom_use_5
                          )

rm(poct_condom_use_1,
   poct_condom_use_2,
   poct_condom_use_3,
   poct_condom_use_4,
   poct_condom_use_5
   )

## Previous Diagnoses====
poct_syph <- c(nrow(subset(servicedata_poct, SyphilisDiag==1)),
               nrow(subset(servicedata_poct, SyphilisDiag==1 & Colour==12)))
poct_hcv <- c(nrow(subset(servicedata_poct,  HepCDiag==1)),
              nrow(subset(servicedata_poct, HepCDiag==1 & Colour==12)))

poct_diag <<- rbind(poct_hcv, poct_syph)

rm(poct_hcv, poct_syph)

## Partners====

poct_partners_1 <- c(
  nrow(subset(servicedata_poct_msm, PartnersMaleLast3 %in% 1:5)),
  nrow(subset(servicedata_poct_msm, PartnersMaleLast3 %in% 1:5 & Colour==12)))
poct_partners_2 <- c(
  nrow(subset(servicedata_poct_msm, PartnersMaleLast3 > 5)),
  nrow(subset(servicedata_poct_msm, PartnersMaleLast3 > 5 & Colour==12))) 

poct_partners<<- rbind(poct_partners_1,
                       poct_partners_2
                       )

rm(poct_partners_1,
   poct_partners_2
   )

## Testing History====

poct_testing_history_total <<- nrow(subset(servicedata_poct, HIVEverTested==1))

poct_testing_history_last <<- c(
  nrow(subset(servicedata_poct,HIVLastTested==1)),
  nrow(subset(servicedata_poct,HIVLastTested==2)),
  nrow(subset(servicedata_poct,HIVLastTested==3)),
  nrow(subset(servicedata_poct,HIVLastTested==4)),
  nrow(subset(servicedata_poct,HIVLastTested==5)),
  nrow(subset(servicedata_poct,HIVLastTested==6)))
poct_testing_history_type <<- c(
  nrow(subset(servicedata_poct,HIVLastTestType==1)),
  nrow(subset(servicedata_poct,HIVLastTestType==2)),
  nrow(subset(servicedata_poct,HIVLastTestType==3)),
  nrow(subset(servicedata_poct,HIVLastTestType==4)),
  nrow(subset(servicedata_poct,HIVLastTestType==5)))
poct_testing_history_where <<- c(
  nrow(subset(servicedata_poct,HIVWhereTested==1)),
  nrow(subset(servicedata_poct,HIVWhereTested==2)),
  nrow(subset(servicedata_poct,HIVWhereTested==3)),
  nrow(subset(servicedata_poct,HIVWhereTested==4)),
  nrow(subset(servicedata_poct,HIVWhereTested==5)),
  nrow(subset(servicedata_poct,HIVWhereTested==6)))


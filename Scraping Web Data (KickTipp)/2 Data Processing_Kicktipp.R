library(reshape2)
library(XML)
library(plyr)
library(dplyr)
library(stringr)

Spieltag          <- 16
Spieltage         <- seq(1, Spieltag, 1)
Ergebnisse_list   <- list()
Tipps_list        <- list()

for(i in 1:length(Spieltage)) {
      website     <- paste0("http://www.kicktipp.de/puma-buli/tippuebersicht?tippspieltagIndex=", Spieltage[i], "&rankingGruppeId=0&sortBy=gesamtpunkte&wertung=einzelwertung&teilnehmerSucheName=")
      Ergebnisse_list[[Spieltage[i]]]     <- cbind(Spieltage[i],
                                                   seq(1, 9, 1),
                                                   readHTMLTable(website, header = F, which = 2, stringsAsFactors = F))
      Tipps_list[[Spieltage[i]]]          <- cbind(Spieltage[i],
                                                   readHTMLTable(website, header = F, which = 3, stringsAsFactors = F))
}

Ergebnisse  <- ldply(Ergebnisse_list, data.frame) %>%
      select(-3) %>%
      rename(Spieltag = Spieltage.i., Spiel = seq.1..9..1., Heim = V2, Ausw = V3, Erg = V4)

Ergebnisse$Spieltag           <- ordered(as.character(Ergebnisse$Spieltag))
Ergebnisse$Spiel              <- ordered(as.character(Ergebnisse$Spiel))
Ergebnisse$Heim               <- factor(Ergebnisse$Heim)
Ergebnisse$Ausw               <- factor(Ergebnisse$Ausw)
Ergebnisse$Erg                <- as.character(Ergebnisse$Erg)

Ergebnisse                    <- cbind(Ergebnisse,
                                 str_split_fixed(Ergebnisse$Erg, ":", 2))
colnames(Ergebnisse)[c(6:7)]  <- c("HeimTore", "AuswTore")
Ergebnisse$HeimTore           <- as.numeric(as.character(Ergebnisse$HeimTore))
Ergebnisse$AuswTore           <- as.numeric(as.character(Ergebnisse$AuswTore))
Ergebnisse$TorDiff            <- Ergebnisse$HeimTore-Ergebnisse$AuswTore

Tipps       <- ldply(Tipps_list, data.frame) %>%
      subset(!is.na(V3), c(1, 4:13)) %>%
      rename(Spieltag = Spieltage.i., Tipper = V3, Sp1 = V4, Sp2 = V5, Sp3 = V6, Sp4 = V7, Sp5 = V8, Sp6 = V9, Sp7 = V10, Sp8 = V11, Sp9 = V12) %>%
      melt(id = c("Spieltag", "Tipper")) %>%
      rename(Spiel = variable, Tipp = value) %>%
      select(Spieltag, Spiel, Tipper, Tipp)

Tipps$Spieltag                <- ordered(as.character(Tipps$Spieltag))
Tipps$Spiel                   <- ordered(substr(Tipps$Spiel, 3, 3))
Tipps$Tipper                  <- factor(Tipps$Tipper)
Tipps$Tipp                    <- as.character(Tipps$Tipp)

Tipps                         <- cbind(Tipps,
                                       str_split_fixed(Tipps$Tipp, ":", 2))
colnames(Tipps)[c(5:6)]       <- c("HeimTipp", "AuswTipp")
Tipps$Pkt                     <- substring(Tipps$AuswTipp, 2)
Tipps$AuswTipp                <- substring(Tipps$AuswTipp, 1, 1)

Tipps$Pkt                     <- as.numeric(as.character(Tipps$Pkt))
Tipps$Pkt[is.na(Tipps$Pkt)]   <- 0
Tipps$HeimTipp                <- as.numeric(as.character(Tipps$HeimTipp))
Tipps$AuswTipp                <- as.numeric(as.character(Tipps$AuswTipp))
Tipps$TippDiff                <- Tipps$HeimTipp-Tipps$AuswTipp

Tipps$Tipp                    <- paste0(Tipps$HeimTipp, ":", Tipps$AuswTipp)
Tipps                         <- select(Tipps,
                                        Spieltag,
                                        Spiel,
                                        Tipper,
                                        Tipp,
                                        HeimTipp,
                                        AuswTipp,
                                        TippDiff,
                                        Pkt)

Data <- join(Tipps, Ergebnisse)

Data$Pkt_                                                         <- 0
Data$Pkt_[(Data$TippDiff >  0 & Data$TorDiff    >  0)|
          (Data$TippDiff <  0 & Data$TorDiff    <  0)|
          (Data$TippDiff == 0 & Data$TorDiff    == 0)]            <- 2 
Data$Pkt_[Data$TippDiff  != 0 & Data$TippDiff   == Data$TorDiff]  <- 3
Data$Pkt_[Data$Tipp==Data$Erg]                                    <- 4

Data$TorDiff_abs  <- abs(Data$TorDiff)
Data$TippDiff_abs <- abs(Data$TippDiff)

Data$Paarung <- paste(Data$Heim, "-", Data$Ausw)

Data <- select(Data,
               Spieltag,
               Spiel,
               Paarung,
               Heim,
               Ausw,
               Erg,
               Tipp,
               Tipper,
               Pkt,
               Pkt_,
               HeimTore,
               AuswTore,
               TorDiff,
               TorDiff_abs,
               HeimTipp,
               AuswTipp,
               TippDiff,
               TippDiff_abs
               )

setwd("C:/Users/Stefan/Desktop/KickTipp")
write.csv(Data, "Processed Data_Kicktipp.csv", row.names = FALSE)
#import data

install.packages("dplyr")
install.packages("tidyverse")
install.packages("plotly")
library(dplyr)
library(tidyverse)
library(plotly)

lebron <- read.csv("LebronGianni.csv") 
jordan <- read.csv("MicheleGiordano.csv")
MedieTiro <- read.csv("MedieTiro.csv")

PER_L_J <-read.csv("PER_GOAT_MOD.csv")
PER_PLAYERS <- read.csv("PERgiocatoriCOM.csv")

RECORD_L <- read.csv("RECORD_LEBRON.csv")
RECORD_J <- read.csv("RECORD_JORDAN.csv")

carriere <- c("Carriera Jordan" , "Carriera Lebron")
lebron$Year <- as.numeric(substr(lebron$Season, 1, 4))
jordan$Year <- as.numeric(substr(jordan$Season, 1, 4))
PER_PLAYERS$is_goat <- ifelse(PER_PLAYERS$giocatori == "Lebron James" | PER_PLAYERS$giocatori == "Michael Jordan", "yes", "no")

#-------------------------------------------------------------------------------------------------
#medie punti per le tutte le stagioni
lebronPTS <- ggplot(data = lebron, aes(x=Year, y=PTS,group = 1))+
  geom_line(size = 1.2, color = "grey")+
  geom_smooth()+
  geom_point(data = lebron, aes(color = Tm), size = 3)+
  
  labs(title = "Media punti Lebron james per ogni stagione",
       x = "Stagione",
       y = "Punti",
       color = "Squadra"
       )+
  theme_classic()

LPTS <- ggplotly(lebronPTS)
LPTS

jordanPTS <- ggplot(data = jordan, aes(x=Year, y=PTS, group = 1))+
  geom_line(size = 1.2, color = "grey")+
  geom_smooth()+
  geom_point(data = jordan, aes(color = Tm), size = 3)+
  labs(
    title = "media punti Michael Jordan per ogni stagione",
    x = "Stagione",
    y = "Punti",
    color = "Squadra"
    )+
  theme_classic()

JPTS<-ggplotly(jordanPTS)
JPTS
#-------------------------------------------------------------------------------------------------------------
#assist stagionali
lebronAST <- ggplot(data = lebron, aes(x=Year, y=AST,group = 1))+
  geom_line(size = 1.2, color = "grey")+
  geom_point(data = lebron, aes(color = Tm),size=3)+
  geom_smooth()+
  labs(
    title = "media assist Lebron James per ogni stagione",
    x = "Stagione",
    y = "Assist",
    color = "Squadra"
  )+
  theme_classic()

LAST <- ggplotly(lebronAST)
LAST

jordanAST <- ggplot(data = jordan, aes(x=Year, y=AST, color = Tm, group = 1))+
  geom_line(size = 1.2, color = "grey")+
  geom_point(data = jordan, size=3)+
  geom_smooth()+
  labs(
    title = "media assist Michael Jordan per ogni stagione",
    x = "Stagione",
    y = "Assist",
    color = "Squadra"
  )+
  theme_classic()

JAST <- ggplotly(jordanAST)
JAST

#-----------------------------------------------------------------------------------------------------------
#barplot tiri tentati lebron


tiriTotali_L <- ggplot()+
  geom_col(data = lebron, aes(x = Year, y = FGA), fill = "firebrick4")+
  geom_col(data = lebron, aes(x = Year, y = TPA), fill = "darkgoldenrod1")+
  labs(title = "Distribuzione tiri di Lebron",
       x = "Stagioni",
       y = "tiri tentati",
      )+
  theme_classic()

tiriTotali_L

TTL <- ggplotly(tiriTotali_L)
TTL
#------------------------------------------------------------------------------------------------
#barplot tiri tentati jordan
tiriTotali_J <- ggplot()+
  geom_col(data = jordan, aes(x = Year, y = FGA), fill = "red")+
  geom_col(data = jordan, aes(x = Year, y = TPA), fill = "black")+
  labs(title = "Distribuzione tiri di Jordan",
       x = "Stagioni",
       y = "tiri tentati",
       )+
  theme_classic()
tiriTotali_J

TTJ <- ggplotly(tiriTotali_J)
TTJ
#------------------------------------------------------------------------------------
#confronto medie
tiriTotali <- ggplot()+
  geom_col(data = MedieTiro, aes(x = carriere, y = FGA), fill = "orange")+
  geom_col(data = MedieTiro, aes(x = carriere, y = TPA), fill = "darkorange2")+
  labs(title = "Distribuzione tiri in carriera",
       x = "Giocatori",
       y = "tiri tentati"
       )+
  theme_classic()

tiriTotali

TT<-ggplotly(tiriTotali)
TT
#----------------------------------------------------------------------------------------
#P.E.R jordan e lebron
PER_LEBRON <- ggplot()+
  geom_col(data = PER_L_J, aes(x = SeasonL, y = PERL, fill = Team_L))+
  labs(
    x = "Stagione",
    y = "P.E.R",
    title ="Player Efficiency Rating Lebron:",
    fill = "Squadra"
  )+
theme_bw()
 

PRL<-ggplotly(PER_LEBRON)
PRL

PER_JORDAN <- ggplot()+
  geom_col(data = PER_L_J, aes(x = SeasonJ, y = PERJ, fill = Team_J))+
  labs( x = "Stagione",
        y = "P.E.R",
        title ="Player Efficiency Rating Jordan:",
        fill = "Squadra")+
theme_bw()


PRJ <- ggplotly(PER_JORDAN)
PRJ

#P.E.R tanti giocatori #da perfezionare
PER_PLAYERS$is_goat <- ifelse(PER_PLAYERS$giocatori == "Lebron James" | PER_PLAYERS$giocatori == "Michael Jordan", "yes", "no")
PER_01 <- ggplot(data = PER_PLAYERS, aes(x = partite, y = PER,  label = giocatori, fill = is_goat ))+
  geom_point( color = "black",
              shape = 21,
              stroke = 0.5,
              alpha = 0.8,
              size = 8)+
  scale_size(range = c(8, 20), name = "Punti totali:")+
  labs(
    title = "Player Efficiency Rating",
    x = "Partite disputate",
    y = "PER"
  )+
  theme_classic()

PER_CONFRONTO1 <-ggplotly(PER_01)
PER_CONFRONTO1

PER_02 <- ggplot(data = PER_PLAYERS, aes(x = partite, y = totPTS, size = PER, label = giocatori , fill = is_goat ))+
  geom_point(
              color = "black",
              shape = 21,
              stroke = 0.5,
              alpha = 0.8)+
  scale_size(range = c(5,20),name = "P.E.R")+
  labs(
    x= "Partite disputate",
    y= "Punti totali",
    size= "PER"
  )+
  theme_classic()

PER_CONFRONTO2 <- ggplotly(PER_02)
PER_CONFRONTO2
                  
#---------------------------------------------------------------------------------------
#record squadra lebron
TeamLebron <- ggplot(data = RECORD_L, aes(x = Season, y = W,  group = 1))+
  geom_line(color = "grey",size = 1.2)+
  geom_point(data = RECORD_L,aes(color = team), size = 3)+
  geom_smooth()+
  labs(
    title = "Record squadre di Lebron (82 partite stagionali",
    subtitle = "(stagioni 11-12, 19-20, 20-21 hanno avuto meno partite complessive)",
    x = "Stagione",
    y = "Partite vinte"
  )+
  theme_classic()

TL <- ggplotly(TeamLebron)
TL

#record squadre jordan
TeamJordan <- ggplot(data = RECORD_J, aes(x = Season, y = W, group = 1))+
  geom_line(color = "grey", size = 1.2)+
  geom_point(data = RECORD_J,aes(color = team),size = 3)+
  geom_smooth()+
  labs(
    title = "Record squadre di jordan (82 partite stagionali)",
    x = "Stagione",
    y = "Partite vinte"
  )+
  theme_classic()

 TJ <- ggplotly(TeamJordan)
 TJ

#confronto diretto
 

Team <- ggplot()+
  geom_point(data = RECORD_L, aes(x = L, y = W, color = player, size = percentage, label = Season))+
  geom_point(data = RECORD_J, aes(x = L, y = W, color = player, size = percentage, label = Season))+
  labs(
    x = "Sconfitte",
    y = "Vittorie"
  )+
  theme_classic()
  
Team

TEAMS <- ggplotly(Team)
TEAMS



#grafici fatti:
LPTS #medie punti stagionali di lebron
JPTS #medie punti stagionali di jordan
LAST #medie assist stagionali di lebron
JAST #medie assist stagionali di jordan
TTL  #tiri tentatati stagionali di lebron
TTJ  #tiri tentatati stagionali di jordan
TT   #confronto tiri tentati media carriere jordan e lebron
PRL  #PER stagionale lebron
PRJ  #PER stagionale jordan
PER_CONFRONTO1  #confronto PER lebron e jordan con altri giocatori 1
PER_CONFRONTO2  #confronto PER lebron e jordan con altri giocatori 2
TL #analisi record squadre di lebron
TJ #analisi record squadre di jordan
TEAMS #confronto record squadre di lebron e jordan


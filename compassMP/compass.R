# libraries
library(rgdal)
library(ggplot2)
library(dplyr)
library(scales)
library(ggsn)
library(png)
library(extrafont)
library(cowplot)
library(magick)
library(gridExtra)
library(RColorBrewer)

# setting colours
keskColor = '#01954B'
kokColor = '#006288'
psColor = '#FFDE55'
sdpColor = '#E11931' 
vihrColor = '#61BF1A'
vasColor = '#F00A64'
vasColor2 = '#BF1E24'
sfpColor = '#FFDD93'
kdColor = '#18359B'
sinColor = '#003680'
liikColor = '#ae2375'
puoluevärit <- c("Keskusta"=keskColor, "Kokoomus"=kokColor, "Perussuomalaiset"=psColor, "SDP"=sdpColor, "Vihreät"=vihrColor,
                 "Vasemmistoliitto"=vasColor, "Ruotsalainen kansanpuolue"=sfpColor, "Kristillisdemokraatit"=kdColor, "Liike Nyt"=liikColor,
                 "Aito suomalainen yhteislista"="grey", "Eläinoikeuspuolue"="grey", "Feministinen puolue"="grey", "Itsenäisyyspuolue"="grey",
                 "Kansalaispuolue"="grey", "Kansanliike Suomen Puolesta"="grey", "Kommunistinen työväenpuolue"="grey",
                 "Liberaalipuolue - Vapaus Valita"="grey", "Piraattipuolue"="grey", "Seitsemän tähden liike"="grey", "Sininen tulevaisuus"="grey",
                 "Suomen kansa ensin"="grey", "Suomen kommunistinen puolue"="grey", "Reformi"="grey", "Kansanliike Suomen puolesta"="grey")
palette <- brewer.pal(4, "Reds")

# alternate colours
värit <- c('#33a02c','#1f78b4','#6a3d9a','#fb9a99','#fdbf6f','#a6cee3','#ff7f00','#e31a1c','#cab2d6','#b2df8a')

# reading data and subsetting it a bit
data <- read.csv2(file="arvokartta.csv", header=TRUE, sep=";", encoding="UTF-8")
data2 <- subset(data, kansanedustaja == "yes")
piiri2 <- data2$X.U.FEFF.piiri
arvo2 <- data2$y
talous2 <- data2$x
puolue2 <- data2$puolue
arvolim2 <- data2$y2
talouslim2 <- data2$x2
votes2 <- data2$votes
votefrac2 <- data2$votefrac

# determining data categories and colours
## 4 classes
fill.breaks <- c(0,0.05,0.1,0.15,Inf)
fill.names <- levels(cut(seq(0, 120), breaks = fill.breaks, right = FALSE))
fill.labels <- c("0–5 %", "5–10 %", "10–15 %", ">15 % kaikista äänistä")
fill.colors <- c("#fff7ec", "#fdd49e", "#fc8d59", "#d7301f")
names(fill.labels) <- fill.names
names(fill.colors) <- fill.names

## 5 classes
fill.breaks5 <- c(0,0.05,0.1,0.15,0.2,Inf)
fill.names5 <- levels(cut(seq(0, 120), breaks = fill.breaks5, right = FALSE))
fill.labels5 <- c("0–5 %", "5–10 %", "10–15 %", "15–20 %", ">20 % kaikista äänistä")
fill.colors5 <- c("#fff7ec", "#fdcc8a", "#fc8d59", "#d7301f", "#7f0000")
names(fill.labels5) <- fill.names5
names(fill.colors5) <- fill.names5

# drawing the plot
graafi <- ggplot()+
  geom_point(data=data2, aes(x=talouslim2, y=arvolim2, size=votes, colour=puolue), alpha=1)+
  stat_summary_2d(data=data2, aes(talouslim2, arvolim2, z=votes/sum(votes)), alpha=0.2, binwidth = c(1.25, 1.25), fun = function(x) cut(sum(x),
        breaks = fill.breaks5, right = FALSE), drop = F, colour="grey")+
  scale_fill_manual(values=fill.colors5, labels=fill.labels5,
        name="Kaikille eduskuntavaaliehdokkaille\nannettujen äänien jakautuminen")+
  scale_color_manual(values=puoluevärit, name="Kansanedustajat",
        breaks=c("Kokoomus", "Perussuomalaiset", "SDP", "Keskusta", "Vihreät", "Vasemmistoliitto", "Liike Nyt", "Kristillisdemokraatit", "Ruotsalainen kansanpuolue"))+
  scale_size(guide="none")+
  labs(title='Suomalaiset äänestäjät "arvokartalla"', subtitle="Eduskuntavaalit 2019",
       caption="Databyro.fi\nEdustajien sijainnit poimittu https://alanmiesz.github.io/vaalikoneet-2019/")+
  guides(color = guide_legend(override.aes = list(size=5), order=2), fill=guide_legend(order=1, reverse = T))+
  geom_hline(aes(yintercept = 0, size=0.1))+
  geom_vline(aes(xintercept = 0, size=0.1))+
  theme_void()+
  theme(plot.title = element_text(color="black", size=36, family="Philosopher"))+
  theme(plot.subtitle = element_text(color="black", size=30, family="Philosopher"))+
  theme(text=element_text(family="Philosopher", size=28))+
  theme(plot.caption=element_text(family="Philosopher"))+
  theme(legend.text=element_text(size=20))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(legend.box.margin = margin(20,20,20,20))+
  theme(plot.caption = element_text(hjust = 0))+
  theme(plot.margin = margin(10,10,10,10))
  
graafi

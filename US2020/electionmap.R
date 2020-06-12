# What would the US states' map roughly look like with current polling average

#libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(extrafont)
library(reshape)
library(jsonlite)
library(fresh)
library(formattable)
library(forcats)
library(readxl)
library(rgdal)
library(cowplot)
library(ggrepel)
library(rgeos)
library(sp)

# setting the subtitle
average <- "RealClearPolitics polling average 11th June 2020:\nBiden (D-S&D): 49.8%\nTrump (R-ECR): 41.7%"

#reading the US shapefiles
states <- readOGR(dsn="C:/Users/Julius/Setit/Europe Elects/Shapefiles/USA_adm", layer="cb_2018_us_state_500k", stringsAsFactors = FALSE)

# determining the state centroids, labels and electoral votes
states@data <- cbind(states@data, rgeos::gCentroid(states, byid = T) %>% coordinates())
abbreviations <- c("Miss.", "N.C.", "Okla.", "Va.", "W.Va.", "La.", "Mich.", "Mass.", "Idaho", "Fla.", "Neb.", "Wash.", "N.M.", "0", "S.D.", "Texas",
                   "Calif.", "Ala.", "Ga.", "Pa.", "Mo.", "Colo.", "Utah", "Tenn.", "Wyo.", "N.Y.", "Kan.", "Alaska", "Nev.", "Ill.", "Vt.",
                   "Mont.", "Iowa", "S.C.", "N.H.", "Ariz.", "D.C.", "0", "0", "N.J.", "Md.", "Maine", "Hawaii", "Del.", "0", "0", "R.I.", "Ky.",
                   "Ohio", "Wis.", "Ore.", "N.D.", "Ark.", "Ind.", "Minn.", "Conn")
electoralvotes <- c(6, 15, 7, 13, 5, 8, 16, 11, 4, 29, 5, 12, 5, 0, 3, 38,
                    55, 9, 16, 20, 10, 9, 6, 11, 3, 29, 6, 3, 6, 20, 3,
                    3, 6, 9, 4, 11, 3, 0, 0, 14, 10, 4, 4, 3, 0, 0, 4, 8,
                    18, 10, 7, 3, 6, 11, 10, 7)
states@data <- cbind(states@data, abbreviations)
states@data <- cbind(states@data, electoralvotes)
states.df <- fortify(states, region = "NAME")

# reading the file for political leans of states and merging to the shapefile
leans <- as.data.frame(read_excel("leans.xlsx"))
df <- merge(states.df, leans, by="id")

# determining data categories
df$cat <- cut(df$repdiff20, c(-Inf, -0.1, 0, 0.1, Inf), labels=c("Democratic", "Lean Democratic", "Toss-up", "Lean Republican", "Republican"))

# drawing the mainland USA plot
mainland <- ggplot(arrange(df, order), aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=class20), colour="white")+
  geom_text(data=states@data, aes(x=x, y=y, label = abbreviations, group=abbreviations), size=5, check_overlap=T, nudge_y = 0.4, fontface = "bold", colour = "black")+
  geom_text(data=states@data, aes(x=x, y=y, label = electoralvotes, group=electoralvotes), size=5, check_overlap=T, nudge_y = -0.4, fontface = "bold", colour = "black")+
  labs(title="2020 United States presidential election", subtitle=average, caption="Databyro.fi")+
  scale_fill_manual(name="",
                    values=c("e_Democratic" = "#7070FF", "d_Lean Democratic" = "#adadff", "c_Toss-up" = "grey", "b_Lean Republican" = "#f5a3a7", "a_Republican" = "#ee5f65"),
                    labels=c("Democratic (> 8 pp.)", "Leans Democratic (8-3 pp.)", "Toss-up (< 3 pp.)", "Leans Republican (8-3 pp.)", "Republican (> 8 pp.)"),
                    breaks=c("e_Democratic", "d_Lean Democratic", "c_Toss-up", "b_Lean Republican", "a_Republican"))+
  coord_map("albers", lat0=30, lat1=40, xlim=c(-140,-67.5), ylim=c(25,47.5))+
  theme_void()+
  theme(plot.title = element_text(color="black", size=40, family="Philosopher", vjust=7))+
  theme(plot.subtitle = element_text(color="black", size=26, family="Philosopher", vjust=9))+
  theme(text=element_text(family="Philosopher", size=28))+
  theme(plot.caption=element_text(family="Philosopher"))+
  theme(legend.text=element_text(size=18))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(legend.box.margin = margin(0,0,0,0))+
  theme(legend.position = c(0.95,0.5))+
  theme(legend.spacing.y = unit(0.4, 'cm'))+
  theme(plot.caption = element_text(hjust = 1))+
  theme(plot.margin = margin(50,50,10,10))

# drawing alaska
alaska <- ggplot(arrange(df, order), aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=class20),colour="white")+
  scale_fill_manual(name="", values=c("e_Democratic" = "#7070FF", "d_Lean Democratic" = "#adadff", "c_Toss-up" = "grey", "b_Lean Republican" = "#f5a3a7", "a_Republican" = "#ee5f65"), guide=F)+
  labs(title="Alaska")+
  coord_map("albers", lat0=60, lat1=70, xlim=c(-180, -130), ylim=c(50,75))+
  theme_void()+
  theme(plot.title = element_text(color="black", size=28, family="Philosopher"))+
  theme(text=element_text(family="Philosopher", size=28))+
  theme(plot.margin = margin(10,10,10,10))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# drawing hawaii
hawaii <- ggplot(arrange(df, order), aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=class20),colour="white")+
  scale_fill_manual(name="", values=c("e_Democratic" = "#7070FF", "d_Lean Democratic" = "#adadff", "c_Toss-up" = "grey", "b_Lean Republican" = "#f5a3a7", "a_Republican" = "#ee5f65"), guide=F)+
  labs(title="Hawaii")+
  coord_map("albers", lat0=30, lat1=40, xlim=c(195, 210), ylim=c(15,25))+
  theme_void()+
  theme(plot.title = element_text(color="black", size=28, family="Philosopher"))+
  theme(text=element_text(family="Philosopher", size=28))+
  theme(plot.margin = margin(10,10,10,10))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# drawing a bar for electoral votes
bar <- ggplot(leans, aes(x=dummy, y=evs20, fill=totals20, label=evs20))+
  geom_bar(stat="identity", position = position_stack(reverse = F), width = 0.3)+
  geom_text(position = position_stack(vjust = 0.5, reverse = F), size=5)+
  scale_fill_manual(values=c("e_Democratic" = "#7070FF", "d_Lean Democratic" = "#adadff", "c_Toss-up" = "grey", "b_Lean Republican" = "#f5a3a7", "a_Republican" = "#ee5f65"),
                    labels=c("Democratic", "Leans Democratic", "Toss-up", "Leans Republican", "Republican"),
                    breaks=c("e_Democratic", "d_Lean Democratic", "c_Toss-up", "b_Lean Republican", "a_Republican"), name="", guide = guide_legend(direction = "horizontal", nrow = 1))+
  labs(title="")+
  xlab("")+
  ylab("")+
  coord_flip()+
  annotate("text", x=1.25, y=270, label="270", size=6)+
  annotate("segment", x=0.85, xend = 1.15, y=270, yend=270, size=2)+
  theme_void()+
  theme(plot.title = element_text(color="black", size=20, family="Philosopher", vjust = -10, hjust = 0.074))+
  theme(text=element_text(family="Philosopher"))+
  theme(legend.text=element_text(size=15))+
  theme(legend.key.size = unit(0.5, "cm"))+
  theme(legend.box.margin = margin(0,0,0,0))+
  theme(legend.position = c(0.5, 0.12))+
  theme(plot.caption = element_text(hjust = 1))+
  theme(legend.spacing.x = unit(0.4, 'cm'))+
  theme(plot.margin = margin(0,0,0,0))

# putting it all together
ggdraw(mainland)+
  draw_plot(alaska, scale=0.35, x=1, y=1, hjust=1.38, vjust=0.85)+
  draw_plot(hawaii, scale=0.3, x=1, y=1, hjust=1.38, vjust=1.15)+
  draw_plot(bar, scale=0.4, x=1, y=1, hjust=0.82, vjust=0.56)

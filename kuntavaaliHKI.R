library(tidyverse)
library(extrafont)
library(readxl)
library(rgdal)
library(cowplot)
library(sf)
library(cartogram)

kunta21 <- as.data.frame(read_excel("C:/Users/Julius/Setit/Databyro/excel ja R/excelit/kuntavaalit_helsinki.xlsx"))

helsinki <- readOGR(dsn="C:/Users/Julius/Setit/Databyro/shp_fin/Helsinki", layer="Aanestysaluejako_PKS_2021", stringsAsFactors = FALSE)
helsinki <- subset(helsinki, kunta=="HELSINGIN KAUPUNKI")
helsinki <- merge(helsinki, kunta21, by="nimi")
helsinki <- st_as_sf(helsinki)
helsinki <- st_transform(helsinki, crs=3857)
# #helsinki <- cartogram_ncont(helsinki, "total")
helsinki <- as(helsinki, "Spatial")
helsinki@data <- cbind(helsinki@data, rgeos::gCentroid(helsinki, byid = T) %>% coordinates())
helsinkidf <- fortify(helsinki, region="nimi")
helsinkidf <- merge(helsinkidf, kunta21, by="id")

kunnat <- readOGR(dsn="C:/Users/Julius/Setit/Databyro/shp_fin/Helsinki", layer="PKS_Postinumeroalueet_manner_2019", stringsAsFactors = FALSE)
kunnat <- subset(kunnat, Kunta=="Helsinki")
kunnat <- st_as_sf(kunnat)
kunnat <- st_transform(kunnat, crs=3857)
kunnat <- as(kunnat, "Spatial")
kunnat <- fortify(kunnat, region="Kunta")

hkiplot <- ggplot(helsinkidf, aes(x=long, y=lat, group=group))+
  geom_polygon(fill=NA, colour="grey", size=0.5)+
  geom_polygon(data=kunnat, fill=NA, colour="black", size=1)+
  #geom_point(data=helsinki@data, aes(x=x, y=y, group=nimi, fill=helsinki@data$plurality, alpha=helsinki@data$Yht2), size=7, shape=21, colour="black", stroke=1)+
  geom_point(data=helsinki@data, aes(x=x, y=y, group=nimi, fill=helsinki@data$second, size=(helsinki@data$Yht2)), alpha=0.8, shape=21, colour="black", stroke=1)+
  scale_size(name="Total voters", guide = guide_legend(reverse = T, order=1), range=c(0,10))+
  #scale_colour_fermenter(palette = "Blues", breaks=c(20,25,30), limits=c(0,50), direction=1, name="Ääniosuus", labels=scales::percent_format(scale=1, accuracy=1, suffix=" %"))+
  scale_fill_manual(values=c("#006288","#FFD500", "#FFDD93", "#FF0000", "#E94786", "#61BF1A"), labels=c("Kokoomus", "Perussuomalaiset", "RKP", "SDP", "Vasemmistoliitto", "Vihreät"), name="Toiseksi suurin\npuolue")+
  guides(fill = guide_legend(override.aes = list(size=10)), size=F, alpha=F)+
  coord_fixed(1)+
  theme_void()+
  theme(text=element_text(family="Philosopher", size=10))+
  theme(legend.title=element_text(size=40))+
  theme(legend.text=element_text(size=35))+
  theme(legend.key.size = unit(1.5, "cm"))+
  theme(legend.box.margin = margin(0,0,0,0))+
  theme(legend.position = c(-0.15,0.55))+
  theme(legend.box.background = element_rect(fill="white"))+
  theme(legend.box.margin = margin(20,20,20,20))+
  theme(plot.caption = element_text(hjust = 1))+
  theme(plot.margin = margin(50,50,10,10))

blank <- ggplot(data=helsinkidf, aes(x = long, y = lat, group = group))+
  geom_blank()+
  labs(title="Kuntavaalit 2021", subtitle="Toiseksi suurin puolue Helsingissä äänestysalueittain\nPallojen koko skaalattu alueen kokonaisäänimäärällä", caption="Julius Lehtinen / Databyro.fi\nData: Tilastokeskus\nÄänestysaluerajat: Helsinki Region Infoshare (CC BY 4.0)")+
  theme_void()+
  theme(plot.title = element_text(color="black", size=45, family="Philosopher", face = "bold", margin=margin(0,0,10,0)))+
  theme(plot.subtitle = element_text(color="black", size=25, family="Philosopher"))+
  theme(text = element_text(family="Philosopher", size=40))+
  theme(plot.caption= element_text(family="Philosopher", size=20, hjust = 0))+
  theme(plot.margin = margin(25,25,25,25))+
  theme(plot.background = element_rect(fill = '#fff8e5'))

ggdraw(blank)+
  draw_plot(hkiplot, scale=1.2, x=0, y=0, hjust=-0.1, vjust=0.03)
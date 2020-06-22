  # libraries
  library(extrafont)
  library(reshape)
  library(fresh)
  library(ggplot2)
  library(readxl)
  library(ggpubr)
  library(scales)
  
  #function for combining log and reverse scaling for ggplot
  reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv, 
              log_breaks(base = base), 
              domain = c(1e-100, Inf))
  }
  
  #setting colours, reading data and doin' some subsettin'
  colours <- c("#fc8d62", "#8da0cb", "#66c2a5")
  df <- as.data.frame(read_excel("futis.xlsx"))
  df$sum_cat <- as.factor(df$sum_cat)
  df2 <- subset(df, sum >= 5)
  
  # plot
  ggplot(df, aes(x=salarycoeffavg, y=positionavg, label=team2))+
    geom_point(aes(colour=sum_cat), size=5)+
    geom_text(nudge_y = 0.025, nudge_x = 0.02)+
    geom_smooth(method="lm", se=T, colour="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "\n")), output="text", method="pearson", p.digits = 3, r.digits = 2, label.x = 1.2, label.y =-0.35)+
    scale_colour_manual(name="Kausien määrä", breaks=c(1,2,3), labels=c("1-3", "4-10", "11-13"), values=colours)+
    #scale_x_log10(labels=scales::comma_format(big.mark = " ", decimal.mark = ","))+
    scale_y_continuous(trans=reverselog_trans(10), labels=c(1,2,3,4,5,6,7,8,9,10,11,12), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
    scale_x_continuous(labels=scales::comma_format(decimal.mark = ",", accuracy = 0.1))+
    #scale_y_reverse(labels=c(1,2,3,4,5,6,7,8,9,10,11,12), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
    guides(colour = guide_legend(override.aes = list(size=10), reverse = T))+
    labs(title = "Palkkakulujen ja sijoituksen suhde\nVeikkausliigassa kausilla 2007-2019", caption="Julius Lehtinen\nDatabyro.fi")+
    xlab("Keskimääräinen palkkakulujen kerroin\nsuhteessa kauden keskiarvoon")+
    ylab("Keskimääräinen sijoitus (log)")+
    theme_minimal()+
    theme(plot.title = element_text(color="black", size=40, family="Philosopher"))+
    theme(plot.subtitle = element_text(color="darkgrey", size=30, family="Philosopher"))+
    theme(text=element_text(color="black", family="Philosopher", size=20))+
    theme(plot.caption=element_text(hjust = 0.98, vjust = 1))+
    theme(plot.margin = margin(10,10,10,10))+
    theme(panel.grid.minor = element_blank())+
    theme(legend.position=c(.15,.85))

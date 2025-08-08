library(googlesheets4)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(extrafont)
library(shadowtext)
library(patchwork)

keskColor = '#01954B'
kokColor = '#006288'
psColor = '#FFDE55'
sdpColor = '#E11931' 
vihrColor = '#61BF1A'
vasColor = '#F00A64'
sfpColor = '#FFDD93'
kdColor = '#18359B'
sinColor = '#003680'
liikColor = '#ae2375'

colours <- c("SDP" = sdpColor, "PS" = psColor, "Kok" = kokColor, "Kesk" = keskColor, "Vihr" = vihrColor, "Vas" = vasColor, "RKP" = sfpColor,
             "KD" = kdColor, "Sin" = sinColor, "SKDL" = "#BF1E24", "Ed/KP/LKP" = "#ffd700", "TPSL" = "#DA2300", "SMP" ="#053170",
             "NSP" = "#3399FF", "SP" = "#3333FF", "Ed" = "#ffd700")

df1 <- read_sheet("1vevKF6OrpFhzxupIoMtFidsRQVDqYy2wN3ldTXDWz6c", sheet = "1917–1939")
nodes <- as.data.frame(unique(df1$from)) %>%
  rownames_to_column()
netw1 <- tbl_graph(edges = df1, directed = F)

df2 <- read_sheet("1vevKF6OrpFhzxupIoMtFidsRQVDqYy2wN3ldTXDWz6c", sheet = "1945–1991")
nodes <- as.data.frame(unique(df2$from)) %>%
  rownames_to_column()
netw2 <- tbl_graph(edges = df2, directed = F)

df3 <- read_sheet("1vevKF6OrpFhzxupIoMtFidsRQVDqYy2wN3ldTXDWz6c", sheet = "1991–2023")
nodes <- as.data.frame(unique(df3$from)) %>%
  rownames_to_column()
netw3 <- tbl_graph(edges = df3, directed = F)

df4 <- read_sheet("1vevKF6OrpFhzxupIoMtFidsRQVDqYy2wN3ldTXDWz6c", sheet = "total")
nodes <- as.data.frame(unique(df4$from)) %>%
  rownames_to_column()
netw4 <- tbl_graph(edges = df4, directed = F)

interwar <- ggraph(netw1, layout = "fr", maxiter=100000000)+
  geom_edge_link(aes(width=weight, alpha=weight, start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)))+
  geom_node_label(aes(label=name, fill=name), colour="white", size=6, label.padding = unit(0.45, "lines"))+
  geom_shadowtext(aes(x, y, label=name), size=6)+
  scale_edge_width_continuous(range = c(0,3), limits = c(0,max(df1$weight)))+
  scale_edge_alpha_continuous(range = c(0.1,1), limits = c(0,max(df1$weight)))+
  scale_fill_manual(values = colours)+
  labs(title="Suomalaisten puolueiden hallituskumppanit 1918–1939",
       subtitle = "Mitä paksumpi ja näkyvämpi viiva puolueiden välillä,\nsitä useamman päivän samassa hallituksessa",
       caption = "Julius Lehtinen / Databyro"
  )+
  coord_cartesian(clip="off")+
  theme_graph()+
  theme(
    #panel.background = element_rect(colour="black"),
    legend.position = "none",
    plot.title = element_text(size=32, face = "bold"),
    plot.subtitle = element_text(size=26),
    plot.margin = margin(10,10,10,10)
  )

postwar <- ggraph(netw2, layout = "fr", maxiter=100000)+
  geom_edge_link(aes(width=weight, alpha=weight, start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)))+
  geom_node_label(aes(label=name, fill=name), colour="white", size=6, label.padding = unit(0.45, "lines"))+
  geom_shadowtext(aes(x, y, label=name), size=6)+
  scale_edge_width_continuous(range = c(0,3), limits = c(0,max(df2$weight)))+
  scale_edge_alpha_continuous(range = c(0.1,1), limits = c(0,max(df2$weight)))+
  scale_fill_manual(values = colours)+
  labs(title="Suomalaisten puolueiden hallituskumppanit 1945–1991",
       subtitle = "Mitä paksumpi ja näkyvämpi viiva puolueiden välillä,\nsitä useamman päivän samassa hallituksessa",
       caption = "Julius Lehtinen / Databyro"
  )+
  coord_cartesian(clip="off")+
  theme_graph()+
  theme(
    #panel.background = element_rect(colour="black"),
    legend.position = "none",
    plot.title = element_text(size=32, face = "bold"),
    plot.subtitle = element_text(size=26),
    plot.margin = margin(10,10,10,10)
  )

coop <- ggraph(netw3, layout = "fr", maxiter=1000000)+
  geom_edge_link(aes(width=weight, alpha=weight, start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)))+
  geom_node_label(aes(label=name, fill=name), colour="white", size=6, label.padding = unit(0.45, "lines"))+
  geom_shadowtext(aes(x, y, label=name), size=6)+
  scale_edge_width_continuous(range = c(0,3), limits = c(0,max(df3$weight)))+
  scale_edge_alpha_continuous(range = c(0.1,1), limits = c(0,max(df3$weight)))+
  scale_fill_manual(values = colours)+
  labs(title="Suomalaisten puolueiden hallituskumppanit 1991–2025",
       subtitle = "Mitä paksumpi ja näkyvämpi viiva puolueiden välillä,\nsitä useamman päivän samassa hallituksessa",
       caption = "Julius Lehtinen / Databyro"
  )+
  coord_cartesian(clip="off")+
  theme_graph()+
  theme(
    #panel.background = element_rect(colour="black"),
    legend.position = "none",
    plot.title = element_text(size=32, face = "bold"),
    plot.subtitle = element_text(size=26),
    plot.margin = margin(10,10,10,10)
  )
coop

total <- ggraph(netw4, layout = "fr", maxiter=1000000)+
  geom_edge_link(aes(width=weight, alpha=weight, start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)))+
  geom_node_label(aes(label=name, fill=name), colour="white", size=6, label.padding = unit(0.45, "lines"))+
  geom_shadowtext(aes(x, y, label=name), size=6)+
  scale_edge_width_continuous(range = c(0.1,3), limits = c(0,max(df4$weight)))+
  scale_edge_alpha_continuous(range = c(0.1,1), limits = c(0,max(df4$weight)))+
  scale_fill_manual(values = colours)+
  labs(title="Suomalaisten puolueiden hallituskumppanit rauhan aikana 1918–2025",
       subtitle = "Mitä paksumpi ja näkyvämpi viiva puolueiden välillä,\nsitä useamman päivän samassa hallituksessa",
       caption = "Julius Lehtinen / Databyro"
  )+
  coord_cartesian(clip="off")+
  theme_graph()+
  theme(
    #panel.background = element_rect(colour="black"),
    legend.position = "none",
    plot.title = element_text(size=32, face = "bold"),
    plot.subtitle = element_text(size=26),
    plot.margin = margin(10,10,10,10)
  )

ggsave("network_interwar.png", dpi = 400, plot = interwar, height = 225, width = 400, units = "mm")
ggsave("network_postwar.png", dpi = 400, plot = postwar, height = 225, width = 400, units = "mm")
ggsave("network_coop.png", dpi = 400, plot = coop, height = 225, width = 400, units = "mm")
ggsave("network_total.png", dpi = 400, plot = total, height = 225, width = 400, units = "mm")

(interwar + postwar) / (coop + total) +
  plot_annotation(
    title="Suomalaisten puolueiden hallituskumppanit",
    subtitle = "Mitä paksumpi ja näkyvämpi viiva puolueiden välillä, sitä useamman päivän samassa hallituksessa",
    theme =   theme(
      plot.title = element_text(size=40, face = "bold"),
      plot.subtitle = element_text(size=30),
      plot.margin = margin(10,10,10,10)
    )
  )

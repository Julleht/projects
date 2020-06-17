library(extrafont)
library(reshape)
library(plotly)
library(shiny)
library(fresh)
library(formattable)
library(ggplot2)

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
rrColor = '#FFD600'
nsColor = '#3399FF'
smpColor = '#053170'
ekoColor = 'darkgreen '
lkpColor = '#F1AF44'
skdlColor ='#BF1E24'
devaColor = '#8B0000'
tpslColor = '#DA2300'
kpColor = '#47C7E6'
keColor = '#ffd700'
iklColor = '#000008'

keskFill = '#01954B64'
kokFill = '#00628864'
psFill = '#FFDE5564'
sdpFill = '#E1193164' 
vihrFill = '#61BF1A64'
vasFill = '#F00A6464'
sfpFill = '#FFDD9364'
kdFill = '#18359B64'
sinFill = '#00368064'
liikFill = '#ae237564'
rrFill = '#FFD60064'
nsFill = '#3399FF64'
smpFill = '#05317064'
ekoFill = '#01322064'
lkpFill = '#F1AF4464'
skdlFill ='#BF1E2464'
devaFill = '#8B000064'
tpslFill = '#DA230064'
kpFill = '#47C7E664'
keFill = '#ffd70064'
iklFill = '#00000864'

colours <- c("Suomen Sosialidemokraattinen Puolue" = sdpColor, "Kokoomus" = kokColor, "Keskusta" = keskColor, "Perussuomalaiset" = psColor, "Vihr" = vihrColor,
             "Vasemmistoliitto" = vasColor, "Ruotsalainen Kansanpuolue" = sfpColor, "Kristillisdemokraatit" = kdColor, "Liike Nyt" = liikColor,
             "RR" = rrColor, "Nuorsuomalaiset" = nsColor, "Suomen Maaseudun Puolue" = smpColor, "Eko-Vihr" = ekoColor,
             "Liberaali Kansanpuolue" = lkpColor, "Suomen Kansan Demokraattinen Liitto" = skdlColor, "Demokraattinen Vaihtoehto" = devaColor,
             "Perustuslaillinen Oikeistopuolue" = "#BEBEBE", "SKYP" = "#BEBEBE",
             "TPSL" = tpslColor, "Suomen Kansanpuolue" = kpColor, "Vapaamielisten Liitto" = "#BEBEBE",
             "Kansallinen Edistyspuolue" = keColor, "IKL" = iklColor, "PMP" = "#BEBEBE",
             "SPP" = "grey", "Kansanpuolue" = "grey", "Ruotsalainen Vasemmisto" = "#BEBEBE", "SSTP" = "#BEBEBE",
             "KrTL" = "#BEBEBE")

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme="style.css",
    use_googlefont("Philosopher"),
    # Application title
    titlePanel("Suomen vaaliarkisto"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            radioButtons("datamuoto",
                         h3("Datan muoto"),
                         choices = list(
                             "Graafi" = "graph",
                             "Taulukko" = "table"
                         ),
                         selected = c("graph")
            ),
            
            # conditionalPanel(condition = "input.datamuoto == 'graph'",
            #                  radioButtons("grafiikkamuoto",
            #                               h3("Grafiikan muoto"),
            #                               choices = list(
            #                                   "Kannatushistoria" = "eduskunta",
            #                                   "Vaaleittain" = "euro"
            #                               ),
            #                               selected = c("eduskunta")
            #                  )
            #                  ),
            
            radioButtons("vaalityyppi",
                               h3("Vaalit"),
                               choices = list(
                                   "Eduskuntavaalit" = "eduskunta",
                                   "Eurovaalit" = "euro"
                               ),
                               selected = c("eduskunta")
                         ),
            
            conditionalPanel(
                condition = "input.vaalityyppi == 'eduskunta'",
                sliderInput("vuosi",
                    h3("Vuodet"),
                    min=1919,
                    max=2019,
                    value=c(1919, 2019),
                    sep=""
                        )),
            
            conditionalPanel(
                condition = "input.vaalityyppi == 'euro'",
                sliderInput("vuosi2",
                            h3("Vuodet"),
                            min=1996,
                            max=2019,
                            value=c(1996, 2019),
                            sep=""
                ))
            ),

        mainPanel(
            conditionalPanel(condition = "input.datamuoto == 'graph'",
                             plotlyOutput("history")
                             ),
            conditionalPanel(condition = "input.datamuoto == 'table'",
                             formattableOutput("taulukko"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$history <- renderPlotly({
        
        df <- as.data.frame(read.csv("data/vaaliarkisto.csv", dec = ".", check.names = F, na.strings = c("", 0), encoding = "UTF-8"))
        
        df2 <- subset(df, type==input$vaalityyppi[1] | type==input$vaalityyppi[2])
        
        molten.df <- melt(df2, id=c("Vuosi", "type", "votes", "seats"))
        
        years <- switch(input$vaalityyppi,
            "eduskunta" = c(input$vuosi[1]-1, input$vuosi[2]+1),
            "euro" = c(input$vuosi2[1]-1, input$vuosi2[2]+1)
            )
        
        plot_ly(data=molten.df, mode="lines+markers", type="scatter", x = ~Vuosi, y = ~value, color= ~variable, colors=colours, text = ~variable, marker=list(size=8), height=600) %>%
            layout(hovermode="x unified",
                   margin = list(l=0, r=0, t= 0, b=0, pad=5),
                   showlegend = T,
                   title = list(text = "",
                                font = list(family="Advent Pro", size=40), x=0),
                   legend = list(y=-0.1, orientation = "h",
                                 font=list(family="Advent Pro"),
                                 title = list(text = "",
                                              font = list(size=20, family="Advent Pro"))),
                   yaxis = list(ticksuffix = "%", title="", range=c(0,42)),
                   xaxis = list(title="", range=years, dtick=10)
                   ) %>%
            config(displayModeBar = F)
    })
    
    output$taulukko <- renderFormattable({
        
        df <- as.data.frame(read.csv("data/vaaliarkisto.csv", dec = ".", check.names = F, na.strings = c(""), encoding = "UTF-8"))
        
        df2 <- subset(df, type==input$vaalityyppi[1] | type==input$vaalityyppi[2])
        
        df2 <- switch(input$vaalityyppi,
                      "eduskunta" = subset(df2, Vuosi >= input$vuosi[1] & Vuosi <= input$vuosi[2]),
                      "euro" = subset(df2, Vuosi >= input$vuosi2[1] & Vuosi <= input$vuosi2[2])
                      )
        df2 <- df2[,c(1,5:33)]
        df2 <- df2[, colSums(is.na(df2)) != nrow(df2)]
        
        formattable(df2, align= c("l", rep("l", ncol(df2)-1)),
                    list(`Suomen Sosialidemokraattinen Puolue` = color_tile("transparent", sdpFill),
                         `Perussuomalaiset` = color_tile("transparent", psFill),
                         `Kokoomus` = color_tile("transparent", kokFill),
                         `Keskusta` = color_tile("transparent", keskFill),
                         `Vihr` = color_tile("transparent", vihrFill),
                         `Vasemmistoliitto` = color_tile("transparent", vasFill),
                         `Ruotsalainen Kansanpuolue` = color_tile("transparent", sfpFill),
                         `Kristillisdemokraatit` = color_tile("transparent", kdFill),
                         `Liike Nyt` = color_tile("transparent", liikFill),
                         `RR` = color_tile("transparent", rrFill),
                         `Nuorsuomalaiset` = color_tile("transparent", nsFill),
                         `Suomen Maaseudun Puolue` = color_tile("transparent", smpFill),
                         `Eko-Vihr` = color_tile("transparent", ekoFill),
                         `Liberaali Kansanpuolue` = color_tile("transparent", lkpFill),
                         `Suomen Kansan Demokraattinen Liitto` = color_tile("transparent", skdlFill),
                         `Demokraattinen Vaihtoehto` = color_tile("transparent", devaFill),
                         `Perustuslaillinen Oikeistopuolue` = color_tile("transparent", "#BEBEBE16"),
                         `SKYP` = color_tile("transparent", "#BEBEBE16"),
                         `TPSL` = color_tile("transparent", "#BEBEBE16"),
                         `Suomen Kansanpuolue` = color_tile("transparent", "#BEBEBE16"),
                         `Vapaamielisten Liitto` = color_tile("transparent", "#BEBEBE16"),
                         `Kansallinen Edistyspuolue` = color_tile("transparent", keFill),
                         `IKL` = color_tile("transparent", iklFill),
                         `PMP` = color_tile("transparent", "#BEBEBE16"),
                         `SPP` = color_tile("transparent", "#BEBEBE16"),
                         `Kansanpuolue` = color_tile("transparent", "#BEBEBE16"),
                         `Ruotsalainen Vasemmisto` = color_tile("transparent", "#BEBEBE16"),
                         `SSTP` = color_tile("transparent", "#BEBEBE16"),
                         `KrTL` = color_tile("transparent", "#BEBEBE16")))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

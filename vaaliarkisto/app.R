#libraries
library(extrafont)
library(reshape)
library(plotly)
library(shiny)
library(fresh)

#colours
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
colours <- c("SDP" = sdpColor, "Kok" = kokColor, "Kesk" = keskColor, "PS" = psColor, "Vihr" = vihrColor, "Vas" = vasColor, "SFP" = sfpColor, "KD" = kdColor,
             "Liik" = liikColor, "RR" = rrColor, "NuSu" = nsColor, "SMP" = smpColor, "Eko" = ekoColor, "LKP" = lkpColor,
             "SKDL" = skdlColor, "DeVa" = devaColor, "POP" = "grey", "SKYP" = "grey", "TPSL" = tpslColor, "KP" = kpColor, "VL" = "grey", "KE" = keColor,
             "IKL" = iklColor, "PMP" = "grey", "SPP" = "grey", "SK" = "grey", "RVP" = "grey", "SSTP" = "grey", "KrTL" = "grey")

#reading data
df <- as.data.frame(read.csv("data/vaaliarkisto.csv", dec = ".", check.names = F))

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme="style.css",
    use_googlefont("Philosopher"),
    # Application title
    titlePanel("Suomen vaaliarkisto"),

    # Sidebar with selection dashboard
    sidebarLayout(
        sidebarPanel(
            radioButtons("tyyppi",
                               h3("Vaalit"),
                               choices = list(
                                   "Eduskuntavaalit" = "eduskunta",
                                   "Eurovaalit" = "euro"
                               ),
                               selected = c("eduskunta")
                         ),
            conditionalPanel(
                condition = "input.tyyppi == 'eduskunta'",
                sliderInput("vuosi",
                    h3("Vuodet"),
                    min=1919,
                    max=2019,
                    value=c(1919, 2019),
                    sep=""
                        )),
            
            conditionalPanel(
                condition = "input.tyyppi == 'euro'",
                sliderInput("vuosi2",
                            h3("Vuodet"),
                            min=1996,
                            max=2019,
                            value=c(1996, 2019),
                            sep=""
                ))
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a plot
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        
        df2 <- subset(df, type==input$tyyppi[1] | type==input$tyyppi[2])
        
        molten.df <- melt(df2, id=c("year", "type", "votes", "seats"))
        
        years <- switch(input$tyyppi,
            "eduskunta" = c(input$vuosi[1]-1, input$vuosi[2]+1),
            "euro" = c(input$vuosi2[1]-1, input$vuosi2[2]+1)
            )
        
        plot_ly(data=molten.df, x = ~year, y = ~value*100, color= ~variable, colors=colours, text = ~variable, marker=list(size=8)) %>%
            add_lines() %>%
            layout(margin = list(l=0, r=0, t= 0, b=0, pad=5), height=600, showlegend = T,
                   title = list(text = "", font = list(family="Advent Pro", size=40), x=0),
                   legend = list(y=-0.1, orientation = "h", title = list(text = "", font = list(size=20, family="Advent Pro")), font=list(family="Advent Pro")),
                   hovermode = "compare", yaxis = list(ticksuffix = "%", title="", range=c(0,42)), xaxis = list(title="", range=years, dtick=10)) %>%
            config(displayModeBar = F)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

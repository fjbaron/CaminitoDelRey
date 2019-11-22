
library(shiny)
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "openxlsx","lubridate","ggplot2","parallel","rhandsontable","ggridges")




numCores=function(nCores){
    if(Sys.info()[['sysname']]=="Windows") 1 else nCores 
}


dondeEsta=function(id,t){
    id=as.character(id)
    if( t<=0 | listaIdtiempo[[id]] %>% .[["tiempo"]] < t) return(NA)
    listaIdSector[[id]] %>% filter(t>=tiempoAcum) %>% summarise_all(last) %>% mutate(posicion=longitudAcum+(t-tiempoAcum)*velocidad) %>% pull() 
}


dondeDf=function(dfSimul,t){
    dfSimul$donde=map2_dbl(dfSimul %>% .[["Id"]], dfSimul %>% .[["entrada"]],
                           function(id,entrada) dondeEsta(id,t-entrada))
    dfSimul %>% filter(!is.na(donde))
}


dondeDfparalelo=function(dfSimul,t,nucleos=numCores(6)){
    transpuesto=dfSimul %>% transpose() 
    dfSimul$metros=mclapply(transpuesto,
                            function(x) dondeEsta(x$Id,t-x$entrada),mc.cores=nucleos) %>% unlist()
    dfSimul %>% filter(!is.na(metros))
}

load("datosInterpretados.RData")

dfIdSector=dfReferencia%>% group_by(Id) %>% mutate(longitudAcum=lag(longitudAcum), tiempoAcum=lag(tiempoAcum)) %>% 
    mutate(longitudAcum=ifelse(is.na(longitudAcum),0,longitudAcum),
           tiempoAcum=ifelse(is.na(tiempoAcum),0,tiempoAcum)) %>%
    select(Id:tiempoAcum)


dfIdTiempo=dfReferencia %>% group_by(Id) %>% filter(sector==3) %>% mutate(tiempo=tiempoAcum) %>% select(Id,tiempo) %>% ungroup()

listaIdSector=dfIdSector %>% split(dfIdSector %>% select(Id))
listaIdtiempo=dfIdTiempo %>% split(dfIdTiempo %>% select(Id))





#map_dbl( dfReferencia %>% .[["Id"]], ~ dondeEsta(.x,10))







DF <- tribble(~Hora, ~frecMinuto,
              8,1,
              9,3,
              10,5,
              11,5,
              12,5,
              13,3,
              14,1,
              15,1,
              16,3,
              17,2,
              18,1
)

              
values <- reactiveValues()



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simulador de Caminito del Rey"),

    # Sidebar with a slider input for number of  bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("numReplicas",
                        "Número de simulaciones:",
                        min = 1,
                        max = 400,
                        value = 6),
            sliderInput("horaSim2",
                        "Hora:",
                        min = today()+dhours(8),
                        max = today()+dhours(21),
                        value=today()+dhours(10),
                        timezone="Europe/Madrid",
                        timeFormat = "%H:%M",
        ),
            actionButton("simular", "Simular"),
            rHandsontableOutput("hot")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            p("El número de personas estimado en cada sector, y un intervalo de confianza al 90%."),
            tableOutput("tablaEstimacion"),
            p("Posible numéro de personas en cada sector"),
           plotOutput("grafico",height="300px")
        )
    )
)






# Define server logic required to draw a histogram
server <- function(input, output) {
  entradas <- reactive({
      DF <- values[["DF"]] %>% filter(frecMinuto>0)
      transpose(DF) %>% map( ~ seq(.x$Hora,.x$Hora+1,by = 1/.x$frecMinuto/60)) %>%
          unlist() %>% `-`(8)  %>% `*`(60)
  })

    
     simulacion<-reactive({
         input$simular
      dfReplicas=1:(input$numReplicas) %>% map_df( ~{
      dfSimul=data.frame(entrada=entradas()) %>% 
        mutate(Id=sample(dfIdTiempo$Id,length(entrada),replace = TRUE)) %>%
        as_tibble() %>%
        mutate(simulacion=.x)
#      tiempo=(difftime(input$horaSim2,today()+dhours(8),units="hours")-8)*60
      tiempo=as.integer(difftime(input$horaSim2,today()+dhours(8),units="mins"))
#      message(tiempo)
      dondeDfparalelo(dfSimul,
              tiempo)
      }
      ) %>% mutate(sector=as.integer(ifelse(metros<=1172,1L,ifelse(metros<=1594,2L,3L)))) %>%
          mutate(sector=str_c("s",sector))
     })
    
    
     
    frecSector <- reactive({
        simulacion() %>% nest(-simulacion) %>% mutate(res=map(.[["data"]],
                                                            ~ .x %>% count(sector) %>% spread(sector,n))) %>%
                     select(simulacion,res) %>% unnest()  %>%filter(complete.cases(.))
    })
     
    estimacionSector <- reactive({
        #simulacion()[["sector"]] %>% table() %>% `/`(input$numReplicas) %>% round()
        #simulacion() %>% count(sector) %>% mutate(n=as.integer(round(n/input$numReplicas)))
        media=frecSector() %>% select(-simulacion) %>% summarise_all(~as.integer(mean(.x))) %>% mutate("Estadistico"="Media")
        p05=frecSector() %>% select(-simulacion) %>% summarise_all(~as.integer(quantile(.x,.05))) %>% mutate("Estadistico"="Desde(p5)")
        p95=frecSector() %>% select(-simulacion) %>% summarise_all(~ as.integer(quantile(.x,.95))) %>% mutate("Estadistico"="Hasta (p95)")
        bind_rows(media,p05,p95) %>% select(Estadistico,everything()) %>% gather(Sector,Personas,-Estadistico) %>%
          spread(Estadistico,Personas)
    })   
    
     
     
    
    ## Handsontable
    observe({
        if (!is.null(input$hot)) {
            DF = hot_to_r(input$hot)
        } else {
            if (is.null(values[["DF"]]))
                DF <- DF
            else
                DF <- values[["DF"]]
        }
        values[["DF"]] <- DF 
    })

        
    output$hot <- renderRHandsontable({
        DF <- values[["DF"]]
        if (!is.null(DF))
            rhandsontable(DF, useTypes = TRUE, digits = 2, stretchH = "all") %>% hot_col(c(2),format="0.0")
    })
    
    
    output$tablaEstimacion <- renderTable({
        estimacionSector()
    })
    
    
     output$graficoold <- renderPlot({
         df=frecSector() %>% gather(sector, frecuencia,-simulacion) 
        ggplot(df,aes(x=frecuencia,color=sector,fill=sector))+geom_density(alpha=0.75)+theme_classic()+
            coord_cartesian(ylim=c(0,0.02),xlim=c(0,350))+
            xlab("Personas")+
          scale_x_continuous(breaks=seq(0,350,by=10),limits = c(0,350))+
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())+
           theme(legend.position="top")
        })



output$grafico1 <- renderPlot({
  df=frecSector() %>% gather(sector, frecuencia,-simulacion) 
  ggplot(df,aes(y=frecuencia,x=sector,group=sector,fill=sector))+geom_boxplot()+
    theme_classic()+
    coord_cartesian(ylim=c(0,320))+
    xlab("Personas")+
    coord_flip()+
    scale_y_continuous(breaks=seq(0,350,by=10),limits = c(0,350))+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    theme(legend.position="top")
})


output$grafico <- renderPlot({
  df=frecSector() %>% gather(sector, frecuencia,-simulacion) 
  ggplot(df,aes(x=frecuencia,y=sector,group=sector,fill=sector))+
      geom_density_ridges(
        aes(point_color = sector, point_fill = sector, point_shape = sector),
        alpha = .2, point_alpha = 1, jittered_points = TRUE
      ) +
    theme_classic()+
    coord_cartesian(xlim=c(0,320))+
    xlab("Personas")+
    scale_x_continuous(breaks=seq(0,350,by=10),limits = c(0,350))+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    theme(legend.position="top")
})

}



# Run the application 
shinyApp(ui = ui, server = server)

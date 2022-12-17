#------ App.Shiny: Consulta Parametros reales, 
#----------------- Consulta construccion de movil real,
#----------------- Generadir de curvas Reales. 

#------- Author:   MarlonBello.



library(shiny)
#library(tidyverse)
library(DT)
library(readxl)
library(writexl)




DATOS <- read_excel("Historico bloques.xlsx", sheet = "BD")
DATOS_PARAMETROS <- read_excel("Parametros actualizables (productos comerciales).xlsx")

tball_parametros <- data.frame(
  Finca              = DATOS_PARAMETROS$Finca,
  Producto_Vegetal   = DATOS_PARAMETROS$ProductoVegetal,
  Var_Cultivo        = DATOS_PARAMETROS$Variedad,
  CI.Forecast        = DATOS_PARAMETROS$CicloInicio,
  CP.Forecast        = DATOS_PARAMETROS$CicloPico,
  CT.Forecast        = DATOS_PARAMETROS$CicloTotal,
  AprV.Forecast      = round(DATOS_PARAMETROS$Aprovechamiento,2)
)

tball <- data.frame(Finca             =DATOS$Finca,
                    Producto_Vegetal  =DATOS$Producto,
                    Periodo           =as.numeric(DATOS$PeriodoSiembra),
                    Camas             =DATOS$Camas,
                    Ciclo_Inicio_Real =DATOS$CicloInicioReal,
                    Ciclo_Pico_Real   =DATOS$CicloPicoReal,
                    Ciclo_Total_Real  =DATOS$CicloTotalReal,
                    Var_Cultivo       =DATOS$`Var. Comercial`,
                    Aprovechamiento   =DATOS$`Prod.Real%`,
                    Productv_movil     =DATOS$`Prod.Movil%`
)

tball$ID <- paste( tball$Finca,tball$Var_Cultivo,sep = "")
tball_parametros$Var_Cultivo <- tolower(tball_parametros$Var_Cultivo)
tball_parametros$ID <- paste(tball_parametros$Finca,tball_parametros$Var_Cultivo,sep = "")

tball <- tball[!apply(tball[,5:6]<=0, 1, all),]





ui <- (fluidPage( 
  
  tabsetPanel(
    
    tabPanel("All Data",
             titlePanel("Cierre de bloques DIS Chita"),
             fluidRow(column(DT::dataTableOutput("RawData"), 
                             width = 12))),
    
    tabPanel("FiltroVariedad", 
             titlePanel("Filtros y Medianas"),
             sidebarLayout(
               
               
               sidebarPanel(
                 selectInput(    "Var","Variedad a consultar", choices = c("All",unique(DATOS$`Var. Comercial`))),
                 selectInput(    "Finca","Finca a consultar", choices = c("All",unique(DATOS$Finca))),
                 textInput(      "fecha1","ingrese fecha en formato 202201", value = 0),
                 textInput(      "fecha2","ingrese fecha en formato 202102", value = 0),
                 
                 
                 width = 2),
               
               
               mainPanel(
                 
                 dataTableOutput  ("tb3"),
                 dataTableOutput  ("tbl2"),
                 
                 
                 width = 10)
               
             )),
    
    
    
    tabPanel("FiltroFinca", 
             
             sidebarLayout(
               
               
               
               sidebarPanel(selectInput( "Fn","Finca a consultar", choices = c("Seleccione Finca",unique(tball$Finca))),
                            numericInput("Bq", "Ultimos bloques cerrados a calcular", value = 5),
                            width = 2),
               mainPanel(
                 dataTableOutput  ("tbl4"),
                 width = 10)
             )
             
    ),
    
    
    tabPanel("Ciclos Forecast Vs Real",
             fluidRow(
               column(4, 
                      selectInput( "Fr","Finca a consultar", 
                                   choices = c("Seleccione Finca",unique(tball$Finca)))
                      
               ),
               column(4,
                      numericInput("Br", "Ultimos bloques cerrados a calcular", value = 200)
               ),),
             
             
             dataTableOutput  ("Vs")
    ),
  )))



server <- function(input, output) {
  
  
  #########------SALIDA 1 - TABLA CON TODOS LOS DATOS-------############
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      
      tball }, filter = "top"))
  
  #########------SALIDA 2 - TABLA CONFILTROS-------############
  output$tbl2 <- DT::renderDataTable(DT::datatable({
    
    dat2 <- tball
    bloqAux <- (as.integer(input$Bloq))
    period1 <- input$fecha1
    period2 <- input$fecha2
    
    
    auxdat2 <- data.frame(
      
      "Finca"                = dat2$Finca,
      "Periodo"              = dat2$Periodo,
      "Mediana_CI"           = dat2$Ciclo_Inicio_Real,
      "Mediana_CP"           = dat2$Ciclo_Pico_Real,
      "Mediana_CT"           = dat2$Ciclo_Total_Real,
      "Var_Comercial"        = dat2$Var_Cultivo,
      "Aprovechamiento"      = dat2$Aprovechamiento
    )
    
    
    if (input$Var != "All") {
      
      dat2 <- dat2[dat2$Var_Cultivo == input$Var,]
      
      auxdat2 <- data.frame(
        
        "Finca"                = dat2$Finca,
        "Periodo"              = dat2$Periodo,
        "Mediana_CI"           = dat2$Ciclo_Inicio_Real,
        "Mediana_CP"           = dat2$Ciclo_Pico_Real,
        "Mediana_CT"           = dat2$Ciclo_Total_Real,
        "Var_Comercial"        = dat2$Var_Cultivo,
        "Aprovechamiento"      = dat2$Aprovechamiento
      )}
    
    if (input$Finca != "All") {
      
      dat2 <- dat2[dat2$Finca == input$Finca,]
      
      auxdat2 <- data.frame(
        
        "Finca"                = dat2$Finca,
        "Periodo"              = dat2$Periodo,
        "Mediana_CI"           = dat2$Ciclo_Inicio_Real,
        "Mediana_CP"           = dat2$Ciclo_Pico_Real,
        "Mediana_CT"           = dat2$Ciclo_Total_Real,
        "Var_Comercial"        = dat2$Var_Cultivo,
        "Aprovechamiento"      = dat2$Aprovechamiento
      )}
    
    
    if (period1 != 0 & period2 != 0) {
      
      
      
      dat2 <- dat2[dat2$Periodo <= period2 & dat2$Periodo >= period1,]
      
      auxdat2 <- data.frame(
        
        "Finca"                = dat2$Finca,
        "Periodo"              = dat2$Periodo,
        "Mediana_CI"           = dat2$Ciclo_Inicio_Real,
        "Mediana_CP"           = dat2$Ciclo_Pico_Real,
        "Mediana_CT"           = dat2$Ciclo_Total_Real,
        "Var_Comercial"        = dat2$Var_Cultivo,
        "Aprovechamiento"      = dat2$Aprovechamiento
      )}
    auxdat2 <- na.omit(auxdat2)
    
    auxdat2
    
    
    
  }))
  
  ###############------SALIDA 3 - TABLA CON FILTROS ARROJA MEDIANAS-------#################
  output$tb3 <- DT::renderDataTable(DT::datatable({
    
    dat <- tball
    period1 <- input$fecha1
    period2 <- input$fecha2
    auxdesde <- max(dat$Periodo)
    bloqAux <- (as.integer(input$Bloq))
    
    
    if (input$Finca != "All") {
      
      dat <- dat[dat$Finca == input$Finca,]
      
      
      auxdat <- data.frame(
        "Finca"                = input$Finca,
        "Desde"                = min(as.numeric(dat$Periodo)),
        "Hasta"                = max(as.numeric(dat$Periodo)),
        "Mediana_CI"           = median(dat$Ciclo_Inicio_Real),
        "Mediana_CP"           = median(dat$Ciclo_Pico_Real),
        "Mediana_CT"           = median(dat$Ciclo_Total_Real),
        "Var_Comercial"        = input$Var,
        "Mediana_Aprv"         = median(dat$Aprovechamiento)
      )}
    
    if (input$Var != "All") {
      
      dat <- dat[dat$Var_Cultivo == input$Var,] 
      dat <- na.omit(dat)
      
      auxdat <- data.frame(
        
        "Finca"                = input$Finca,
        "Desde"                = min(as.numeric(dat$Periodo)),
        "Hasta"                = max(as.numeric(dat$Periodo)),
        "Mediana_CI"           = median(dat$Ciclo_Inicio_Real),
        "Mediana_CP"           = median(dat$Ciclo_Pico_Real),
        "Mediana_CT"           = median(dat$Ciclo_Total_Real),
        "Var_Comercial"        = input$Var,
        "Mediana_Aprv"         = median(dat$Aprovechamiento)
      )}                           
    
    if (period1 != 0 & period2 != 0) {
      
      
      dat <- dat[dat$Periodo <= period2 & dat$Periodo > period1,]
      dat <- na.omit(dat)
      
      auxdat <- data.frame(
        "Finca"                = input$Finca,
        "desde"                = min(as.numeric(dat$Periodo)),
        "Hasta"                = max(as.numeric(dat$Periodo)),
        "Mediana_CI"           = median(dat$Ciclo_Inicio_Real),
        "Mediana_CP"           = median(dat$Ciclo_Pico_Real),
        "Mediana_CT"           = median(dat$Ciclo_Total_Real),
        "Var_Comercial"        = input$Var,
        "Mediana_Aprv"         = median(dat$Aprovechamiento)
      )}
    
    auxdat
  }, options = list(searching = FALSE
                    ,lengthChange = FALSE)))
  
  
  ################------SALIDA 4- TABLA PARA EXPORTAR PARAMETROS-------#################
  output$tbl4 <- DT::renderDataTable(DT::datatable({
    
    
    dat4 <- tball
    
    
    tabla_aux <-   data.frame(
      "Finca"                = c(),
      "desde"                = c(),
      "Hasta"                = c(),
      "Mediana_CI"           = c(),
      "Mediana_CP"           = c(),
      "Mediana_CT"           = c(),
      "Var_Comercial"        = c())
    
    if (input$Fn != "Seleccione Finca") {
      
      dat4 <- dat4[dat4$Finca == input$Fn,]
      
      
      Vec_Variedad <- unique(dat4$Var_Cultivo)
      
      
      for (i in Vec_Variedad) {
        
        V <- i
        
        dat4 <- tball
        
        dat4 <- dat4[dat4$Finca == input$Fn,]
        dat4 <- dat4[dat4$Var_Cultivo == V,]
        dat4 <- na.omit(dat4)
        Vec_fechas <- c(unique(dat4$Periodo))
        Vec_fechas <- sort(Vec_fechas)
        resta <- length(Vec_fechas)-as.numeric(input$Bq)
        
        if (resta >= 1 ) {
          
          Aux_Vector <- Vec_fechas[resta:length(Vec_fechas)]
          dat4 <- dat4[dat4$Periodo <= max(Aux_Vector) & dat4$Periodo > min(Aux_Vector),]
          
          a <-        data.frame(
            "Finca"                = input$Fn,
            "desde"                = min(dat4$Periodo),
            "hasta"                = max(dat4$Periodo),
            "Mediana_CI"           = median(dat4$Ciclo_Inicio_Real),
            "Mediana_CP"           = median(dat4$Ciclo_Pico_Real),
            "Mediana_CT"           = median(dat4$Ciclo_Total_Real),
            "variedad"             = i,
            "Mediana_Aprov"        = median(dat4$Aprovechamiento)
          )
          
          tabla_aux <- rbind(tabla_aux,a)
          
        }
        
      }
    } 
    
    tabla_aux
    
    
    
    
  }, filter = "top"))
  
  ################--------- Salida Ciclos Forecast vs Real-------------###################3
  
  
  output$Vs <- DT::renderDataTable(DT::datatable({
    
    datvs <- tball
    
    tabla_aux_vs <-   data.frame(
      "Finca"                = c(),
      "desde"                = c(),
      "Hasta"                = c(),
      "CI Real"              = c(),
      "CP Real"              = c(),
      "CT Real"              = c(),
      "Var_Comercial"        = c())
    
    if (input$Fr != "Seleccione Finca") {
      
      datvs <- datvs[datvs$Finca == input$Fr,]
      
      
      Vec_Variedad <- unique(datvs$Var_Cultivo)
      
      
      for (i in Vec_Variedad) {
        
        V <- i
        
        datvs <- tball
        
        datvs <- datvs[datvs$Finca == input$Fr,]
        datvs <- datvs[datvs$Var_Cultivo == V,]
        datvs <- na.omit(datvs)
        Vec_fechas <- c(unique(datvs$Periodo))
        Vec_fechas <- sort(Vec_fechas)
        resta <- length(Vec_fechas)-as.numeric(input$Br)
        
        if (resta >= 1 ) {
          
          Aux_Vector <- Vec_fechas[resta:length(Vec_fechas)]
          datvs <- datvs[datvs$Periodo <= max(Aux_Vector) & datvs$Periodo > min(Aux_Vector),]
          
          a <-        data.frame(
            "Finca"                = input$Fr,
            "desde"                = min(datvs$Periodo),
            "hasta"                = max(datvs$Periodo),
            "CI Real"              = round(median(datvs$Ciclo_Inicio_Real)),
            "CP Real"              = round(median(datvs$Ciclo_Pico_Real)),
            "CT Real"              = round(median(datvs$Ciclo_Total_Real)),
            "variedad"             = i,
            "AproV Real"           = median(datvs$Aprovechamiento)
          )
          
          tabla_aux_vs <- rbind(tabla_aux_vs,a)
          
        } else {
          a <-        data.frame(
            
            
            "Finca"                = input$Fr,
            "desde"                = min(datvs$Periodo),
            "hasta"                = max(datvs$Periodo),
            "CI Real"              = round(median(datvs$Ciclo_Inicio_Real)),
            "CP Real"              = round(median(datvs$Ciclo_Pico_Real)),
            "CT Real"              = round(median(datvs$Ciclo_Total_Real)),
            "variedad"             = i,
            "AproV Real"           = median(datvs$Aprovechamiento)
          )
          
          tabla_aux_vs <- rbind(tabla_aux_vs,a)
          
          
        }
        
        
        
      }
    } 
    
    tabla_aux_vs$ID <- paste(tabla_aux_vs$Finca,tabla_aux_vs$variedad, sep = "")
    g <- merge(x=tabla_aux_vs, y=tball_parametros[,c("ID","CI.Forecast","CP.Forecast","CT.Forecast","AprV.Forecast")], by ="ID", all.x = TRUE)
    g$Diferencia_CI <- paste(round(g$CI.Real - g$CI.Forecast,0))
    g$Diferencia_CP <- paste(round(g$CP.Real - g$CP.Forecast,0))
    g$Diferencia_CT <- paste(round(g$CT.Real - g$CT.Forecast,0))
    g$Porcent_Movil <- paste(round(g$AproV.Real/g$AprV.Forecast,2))
    g <- g[,c("Finca","variedad","desde","hasta","CI.Real","CP.Real","CT.Real","AproV.Real","CI.Forecast","CP.Forecast","CT.Forecast","AprV.Forecast","Diferencia_CI","Diferencia_CP","Diferencia_CT","Porcent_Movil")]
    
    
    g
    
    
    
  }, filter = "top")
  
  %>% formatStyle("Porcent_Movil",
                  background = styleColorBar(c(0,1.15), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  %>% formatStyle("Diferencia_CI",
                  background = styleInterval(c(-2.5,0,2),c("#f0a592","#f0ee92","#f0ee92","#f0a592")))
  %>% formatStyle("Diferencia_CI", 
                  background = styleEqual(c(0),c("#98f092")))
  %>% formatStyle("Diferencia_CP",
                  background = styleInterval(c(-2.5,0,2),c("#f0a592","#f0ee92","#f0ee92","#f0a592")))
  %>% formatStyle("Diferencia_CP", 
                  background = styleEqual(c(0),c("#98f092")))
  %>% formatStyle("Diferencia_CT",
                  background = styleInterval(c(-2.5,0,2),c("#f0a592","#f0ee92","#f0ee92","#f0a592")))
  %>% formatStyle("Diferencia_CT", 
                  background = styleEqual(c(0),c("#98f092")))
  
  
  
  ) 
  
  #"Diferencia_CI","Diferencia_CP","Diferencia_CT"
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


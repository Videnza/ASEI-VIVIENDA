# 0. Importing and calling libraries ----
library(tidyverse)
library(foreign)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(mapsPERU)
library(sf)
library(scales)
library(plotly)
library(classInt)
library(readxl)

# 1. Importing the data ----
## First we set the working directory
setwd("C:/Users/User/OneDrive - VIDENZA/1. Proyectos Videnza/1. Proyectos actuales/25. ASEI - Propuestas Vivienda/0. Insumos/CENSO")

## Now we import the dataset that is in an excel format
file_name <- "bd_censo_indicadores.xlsx"
data <- read_excel(file_name)

data <- data %>%
  mutate(ubigeo = as.character(ubigeo))

## Separate database for departamentos, provincias and distritos
departamentos <- data %>%
  filter(substr(ubigeo, 3, 6) == "0000")

provincias <- data %>%
  filter(substr(ubigeo, 5, 6) == "00" & substr(ubigeo, 3, 4) != "00")

distritos <- data %>%
  filter(substr(ubigeo, 5, 6) != "00" & substr(ubigeo, 3, 4) != "00")

## List of the names of departamentos, provincias and distritos
listDpto <- departamentos %>%
  select(departamento)  %>%  
  arrange(departamento) %>% 
  drop_na() %>% 
  pull(departamento)

listDpto <- c("TODOS", listDpto)

listProv <- provincias %>%
  select(provincia)  %>%  
  arrange(provincia) %>% 
  drop_na() %>% 
  pull(provincia)

listProv <- c("TODOS", listProv)

listDist <- distritos %>%
  select(distrito)  %>%  
  arrange(distrito) %>% 
  drop_na() %>% 
  pull(distrito)

listDist <- c("TODOS", listDist)

## List of the variables 
Indicadoresiniciales <- names(data)
Indicadoresexcluidos <- c("ubigeo", "ccdd", "ccpp", "ccdi", "departamento", "provincia", "distrito")

listIndicadores <- setdiff(Indicadoresiniciales, Indicadoresexcluidos)
print(listIndicadores)

#1.  Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Dashboard ASEI", theme = shinytheme("lumen"),
             tabPanel("Mapa interactivo", fluid = TRUE, icon = icon("globe-americas"),
                      tags$head(
                        tags$style(HTML("
                        .container-fluid {
                          padding: 0;
                        }
                        .full-page {
                          height: calc(100vh - 50px); /* Adjusting for navbar height */
                          width: 50vw;
                        }
                      "))),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Seleccione el nivel de análisis"),
                          selectInput(inputId = "Dpto",
                                      label = "Elija el departamento",
                                      choices = listDpto,
                                      selected = "TODOS",
                                      width = "220px"
                          ),
                          uiOutput("uiProv"),
                          uiOutput("uiDist"),
                          selectInput(inputId = "Indicador",
                                      label = "Seleccione el indicador",
                                      choices = listIndicadores,
                                      selected = "deficit_total_porcentaje",
                                      width = "220px"
                          )),
                        mainPanel(
                          div(
                            class = "full-page",
                            plotlyOutput("map1", height = "100%", width = "100%")
                          )
                        ))),
             tabPanel("Ficha técnica", fluid = TRUE, icon = icon("chart-bar")
                      #
             )
             
  ))

## Define server logic required to draw a histogram
server <- function(input, output, session) {

## Present provincias depending on the selected departamento and distritos depending on the selected provincia.
  observeEvent(input$Dpto, {
    if (input$Dpto == "TODOS") {
      prov_choices <- provincias %>%
        select(provincia) %>%
        arrange(provincia) %>%
        drop_na() %>%
        pull(provincia)
    } else {
      ubigeo_dpto <- departamentos %>%
        filter(departamento == input$Dpto) %>%
        pull(ubigeo)
      prov_choices <- provincias %>%
        filter(substr(ubigeo, 1, 2) == substr(ubigeo_dpto, 1, 2)) %>%
        select(provincia) %>%
        arrange(provincia) %>%
        drop_na() %>%
        pull(provincia)
    }
    prov_choices <- c("TODOS", prov_choices)
    
    updateSelectInput(session, "Prov", choices = prov_choices)
  })
  
  observeEvent(input$Prov, {
    if (input$Prov == "TODOS") {
      dist_choices <- distritos %>%
        select(distrito) %>%
        arrange(distrito) %>%
        drop_na() %>%
        pull(distrito)
    } else {
      ubigeo_prov <- provincias %>%
        filter(provincia == input$Prov) %>%
        pull(ubigeo)
      dist_choices <- distritos %>%
        filter(substr(ubigeo, 1, 4) == substr(ubigeo_prov, 1, 4)) %>%
        select(distrito) %>%
        arrange(distrito) %>%
        drop_na() %>%
        pull(distrito)
    }
    dist_choices <- c("TODOS", dist_choices)
    
    updateSelectInput(session, "Dist", choices = dist_choices)
  })
  
  output$uiProv <- renderUI({
    selectInput(inputId = "Prov",
                label = "Elija la provincia",
                choices = c("TODOS"),
                selected = "TODOS",
                width = "220px")
  })
  
  output$uiDist <- renderUI({
    selectInput(inputId = "Dist",
                label = "Elija el distrito",
                choices = c("TODOS"),
                selected = "TODOS",
                width = "220px")
  })
  
  
  ## Elaborate the map for the different indicators
    
  output$map1 <- renderPlotly({
      
      custom_palette <- c("#2dc937", "#e7b416", "#cc3232")
      
     
      if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
        map_peru <- map_DIST  %>%  #Cargamos la base de datos sobre las provincias del Peru
          rename(ubigeo = COD_DISTRITO) %>% #renombramos la variable del DF para el merge por UBIGEO
          st_as_sf
        
        alterna <- data  %>%
          select(ubigeo, input$Indicador) 
        
        colnames(alterna) <- c("ubigeo", "indicador")
        
        map_shiny <- merge(x = map_peru, y = alterna, by = "ubigeo", all.x = TRUE)  %>% 
          mutate(indicador = as.numeric(indicador)) 
        
        # Filtrar solo los distritos de la provincia seleccionada
        
        ubigeo_prov <- provincias %>%
          filter(provincia == input$Prov) %>%
          pull(ubigeo)
        
        map_shiny <- map_shiny %>%
          filter(substr(ubigeo, 1, 4) == substr(ubigeo_prov, 1, 4))
        
        breaks <- classIntervals(map_shiny$indicador, n = 3, style = "jenks")
        map_shiny$jenks_breaks <- cut(map_shiny$indicador, breaks$brks, include.lowest = TRUE)
        
        # Identificar el distrito seleccionado
        
        selected_district <- map_shiny %>% 
          filter(ubigeo == distritos$ubigeo[distritos$distrito == input$Dist])
               
        mapa <- map_shiny %>% 
          ggplot() +
          aes(geometry = geometry) +
          geom_sf(aes(fill = jenks_breaks), linetype = 1, lwd = 0.25) +
          geom_sf(data = selected_district, fill = "red", color = "black", size = 1) +  # Resaltar distrito seleccionado
          geom_sf(fill = NA, color = "black", size = 2) +  # Añadir bordes de distritos
          theme_minimal()+
          theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
          scale_fill_manual(values = custom_palette, name = "Porcentaje de hogares (%)")
        
        ggplotly(mapa)
        
#        ubigeo_prov <- provincias %>%
#          filter(provincia == input$Prov) %>%
#          pull(ubigeo)
        
#        geom_prov <- map_PROV %>%
#          filter(COD_PROVINCIA == ubigeo_prov)
        
#        if (!is.null(geom_prov) && nrow(geom_prov) > 0) {
#          geom_prov <- geom_prov %>%
#            st_as_sf() %>%
#            st_union()
        
        bbox <- st_bbox(selected_district)
        buffer <- 0.1
        mapa <- mapa + coord_sf(xlim = c(bbox["xmin"] - buffer, bbox["xmax"] + buffer), 
                                ylim = c(bbox["ymin"] - buffer, bbox["ymax"] + buffer))
          
          
        #Poner el distrito seleccionado con un color distinto
        
      } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
          print("Zoom al departamento y ponemos el mapa del dpto con divisiones proviciales y distritales")
      } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){
        print("Zoom a la provincia y ponemos el mapa provincia con divisiones distritales")
      } else {
        print("Mapa del Perú por regiones ")
        
        map_peru <- map_DEP  %>%  #Cargamos la base de datos sobre los departamentos del Peru
          rename(ubigeo = COD_DEPARTAMENTO ) %>% #renombramos la variable del DF para el merge por UBIGEO
          st_as_sf()
       
        alterna <- data  %>%
          select(ubigeo, input$Indicador) 
        
        colnames(alterna) <- c("ubigeo", "indicador")
        
        map_shiny <- merge(x = map_peru, y = alterna, by = "ubigeo", all.x = TRUE)  %>% 
          mutate(indicador = as.numeric(indicador)) 
        
        breaks <- classIntervals(map_shiny$indicador, n = 3, style = "jenks")
        map_shiny$jenks_breaks <- cut(map_shiny$indicador, breaks$brks, include.lowest = TRUE)
        
        mapa <- map_shiny %>% 
          ggplot() +
          aes(geometry = geometry) +
          geom_sf(aes(fill = jenks_breaks), linetype = 1,
                  lwd = 0.25) +
          theme_minimal()+
          theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
          scale_fill_manual(values = custom_palette, name = "Porcentaje de hogares (%)")
        
        ggplotly(mapa)

      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

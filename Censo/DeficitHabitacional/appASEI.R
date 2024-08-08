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
          st_as_sf()
        
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
        
        map_shiny <- map_shiny %>%
          mutate(text = paste(distritos$distrito[match(ubigeo, distritos$ubigeo)],"<br>",
                              input$Indicador, ":", round(indicador, 2)))
        
        # Identificar el distrito seleccionado
        
        #selected_district <- map_shiny %>% 
        #  filter(ubigeo == distritos$ubigeo[distritos$distrito == input$Dist])
        
        selected_ubigeo <- distritos$ubigeo[distritos$distrito == input$Dist & 
                                              distritos$provincia == input$Prov & 
                                              distritos$departamento == input$Dep]
        
        # Identificar el distrito seleccionado en el mapa
        selected_district <- map_shiny %>% 
          filter(ubigeo == selected_ubigeo)
               
        mapa <- map_shiny %>% 
          ggplot() +
          aes(geometry = geometry) +
          geom_sf(aes(fill = jenks_breaks, text=text), linetype = 1,
                  lwd = 0.25) +
          geom_sf(data = selected_district, aes(text = text), fill = NA, color = "black", size = 1.5) +  # Añadir bordes de distritos
          theme_minimal()+
          theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
          scale_fill_manual(values = custom_palette, name = input$Indicador)
        
        ggplotly(mapa, tooltip = "text") %>% 
          style(hoverinfo = "text", traces = 1)

      } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
        
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
        
        map_shiny <- map_shiny %>%
          mutate(text = paste(departamentos$departamento[match(ubigeo, departamentos$ubigeo)],"<br>",
                              input$Indicador, ":", round(indicador, 2)))
        
        # Identificar el distrito seleccionado
        
        selected_departamento <- map_shiny %>% 
          filter(ubigeo == departamentos$ubigeo[departamentos$departamento == input$Dpto])
        
        mapa <- map_shiny %>% 
          ggplot() +
          aes(geometry = geometry) +
          geom_sf(aes(fill = jenks_breaks, text=text), linetype = 1,
                  lwd = 0.25) +
          geom_sf(data = selected_departamento, aes(text = text), fill = NA, color = "black", size = 1.5) +  # Resaltar departamento seleccionado
          theme_minimal()+
          theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
          scale_fill_manual(values = custom_palette, name = input$Indicador)
        
        ggplotly(mapa, tooltip = "text") %>% 
          style(hoverinfo = "text", traces = 1)
          
      } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){
        
        map_peru <- map_PROV  %>%  #Cargamos la base de datos sobre las provincias del Peru
          rename(ubigeo = COD_PROVINCIA) %>% #renombramos la variable del DF para el merge por UBIGEO
          st_as_sf()
        
        alterna <- data  %>%
          select(ubigeo, input$Indicador) 
        
        colnames(alterna) <- c("ubigeo", "indicador")
        
        map_shiny <- merge(x = map_peru, y = alterna, by = "ubigeo", all.x = TRUE)  %>% 
          mutate(indicador = as.numeric(indicador)) 
      
        # Filtrar solo las provincias del departamento seleccionado
        
        ubigeo_dpto <- departamentos %>% 
          filter(departamento == input$Dpto) %>% 
          pull(ubigeo)
        
        # Caso de Callao donde solo hay una provincia
        
        if (ubigeo_dpto == "070000") {
          # Incluir Lima (150000) si es Callao
          map_shiny <- map_shiny %>%
            filter(substr(ubigeo, 1, 2) %in% c(substr(ubigeo_dpto, 1, 2), "15"))
        } else {
          map_shiny <- map_shiny %>%
            filter(substr(ubigeo, 1, 2) == substr(ubigeo_dpto, 1, 2))
        }
        
        # Continuación del código para generar los breaks y el mapa
        
        breaks <- classIntervals(map_shiny$indicador, n = 3, style = "jenks")
        map_shiny$jenks_breaks <- cut(map_shiny$indicador, breaks$brks, include.lowest = TRUE)
        
        map_shiny <- map_shiny %>%
          mutate(text = paste(provincias$provincia[match(ubigeo, provincias$ubigeo)],"<br>",
                              input$Indicador, ":", round(indicador, 2)))
        
        # Identificar la provincia seleccionado
        
        selected_provincia <- map_shiny %>% 
          filter(ubigeo == provincias$ubigeo[provincias$provincia == input$Prov])
        
        mapa <- map_shiny %>% 
          ggplot() +
          aes(geometry = geometry) +
          geom_sf(aes(fill = jenks_breaks, text = text), linetype = 1,
                  lwd = 0.25) +
          geom_sf(data = selected_provincia, aes(text = text), fill = NA, color = "black", size = 1.5) + # Resaltar provincia seleccionada
          theme_minimal() +
          theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
          scale_fill_manual(values = custom_palette, name = input$Indicador)
        
        # Convertir el ggplot a un objeto plotly
        ggplotly(mapa, tooltip = "text") %>% 
          style(hoverinfo = "text", traces = 1)
        
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
          scale_fill_manual(values = custom_palette, name = input$Indicador)
        
        ggplotly(mapa)

      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

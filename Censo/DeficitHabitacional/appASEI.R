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
                          selectInput(inputId = "Prov",
                                      label = "Elija la provincia",
                                      choices = listProv,
                                      selected = "TODOS",
                                      width = "220px"
                          ),
                          selectInput(inputId = "Dist",
                                      label = "Elija el distrito",
                                      choices = listDist,
                                      selected = "TODOS",
                                      width = "220px"
                          ),
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

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map1 <- renderPlotly({
      
      custom_palette <- c("#2dc937", "#e7b416", "#cc3232")
      
     
      if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist != "TODOS"){
        print("Zoom a la provincia y ponemos el mapa provincia con divisiones distritales")
        #Poner el distrito seleccionado con un color distinto
        
      } else if (input$Dpto != "TODOS" & input$Prov == "TODOS"){
          print("Zoom al departamento y ponemos el mapa del dpto con divisiones proviciales y distritales")
      } else if (input$Dpto != "TODOS" & input$Prov != "TODOS" & input$Dist == "TODOS"){
        print("Zoom a la provincia y ponemos el mapa provincia con divisiones distritales")
      } else {
        print("Mapa del Perú por regiones ")
        
        map_peru <- map_DEP  %>%  #Cargamos la base de datos sobre los departamentos del Peru
          rename(ubigeo = COD_DEPARTAMENTO ) #renombramos la variable del DF para el merge por UBIGEO
       
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
        

      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(RColorBrewer)
library(stringr)
library(Matrix)
library(plotly)
library(DT)


# for layout choose: https://stackoverflow.com/questions/38939827/footer-alignment-in-shiny-app-dashboard
load('20190131-mouse-eae-data.Robj')
colors_many <- c("#E6194B","#3CB44B","#FFE119","#0082C8","#F58231","#911EB4","#46F0F0","#F032E6","#D2F53C","#FABEBE","#008080","#E6BEFF","#AA6E28","#FFFAC8","#800000","#AAFFC3","#808000","#FFD8B1","#000080","#808080","#FFFFFF","#000000","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5")

shinyUI(fluidPage(

  # Application title
  titlePanel("Mouse EAE single cell RNA-Seq data viewer"),

  # Sidebar with input files and data metrics
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", selected = names(data)[2],
                        list("All cells","Parenchyma & Perivascular Space","Meninges","Choroid Plexus","Blood")), 
      selectizeInput("gene", choices = colnames(data[[1]][[2]]),
                 "Gene Expression Tsne - Enter Gene:", multiple = TRUE, selected = 'Cx3cr1'),
      selectInput("plot_variable", "Choose a variable:",
                  list("Condition","Population","Subpopulation", "Cluster","Compartment")), 
        numericInput("cluster",
                "Differential Gene Expression - Enter Cluster number:", value = 1, min = 1, max = , step=1), #max(as.numeric(data[[input$dataset]][[1]]$Cluster))
      #from url: https://groups.google.com/forum/#!topic/shiny-discuss/JfGnQqAv4RU
      'Upregulated Genes in cluster:',
      div(style='height:600px; overflow-y: scroll',
          dataTableOutput('clusterTableUp')),
      'Downregulated Genes in cluster:',
      div(style='height:600px; overflow-y: scroll',
          dataTableOutput('clusterTableDown'))
      ),

    
    # Show plots from the sc data
    mainPanel(
            fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),  plotOutput("tsnePlot"), #,width="600px",height="400px"
                                plotOutput("tsneExpPlot")) #,width="600px",height="400px"
            ),
            
            fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),  plotlyOutput("tsnePlotVar"), #,width="600px",height="400px"
                                plotOutput("barExpPlot")) #,width="600px",height="400px"
            ),
            
      
        #fluidRow(plotOutput('lineExpPlot')),
        fluidRow(
                plotOutput('MAplot')
        ),
        div(class = "footer",
            ("Created by Roman Sankowski - 2019 - 
             Github: rsankowski
             ")
        )
    )
  )
))


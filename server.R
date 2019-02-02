
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RColorBrewer)
library(tidyr)
library(Matrix)
library(plotly)
library(shinydashboard)

load('20190131-mouse-eae-data.Robj')
colors_many <- c("#E6194B","#3CB44B","#FFE119","#0082C8","#F58231","#911EB4","#46F0F0","#F032E6","#D2F53C","#FABEBE","#008080","#E6BEFF","#AA6E28","#FFFAC8","#800000","#AAFFC3","#808000","#FFD8B1","#000080","#808080","#FFFFFF","#000000","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5")


shinyServer(function(input, output) {
        
        #plot Tsne with cluster number
        output$tsnePlot <- renderPlotly({
        
               tsne <-ggplot(data[[input$dataset]][[1]], aes(V1, V2, fill = Cluster)) +
                        geom_point(pch = 21, size = 2, stroke = 0.1) +
                        theme_void() +
                        scale_fill_manual('',values = rev(colors_many)) +
                        labs(title = 'Clusters')
               ggplotly(tsne)
        })
        
        #Plot Tsne with group labels
        output$tsnePlotVar <- renderPlotly({
                if (is.numeric(data[[input$dataset]][[1]][[input$plot_variable]])) {
                        .type = "seq"
                        .palette = "YlGnBu"
                        variable = data[[input$dataset]][[1]][[input$plot_variable]]
                        vars <- ggplot(data[[input$dataset]][[1]], aes(V1, V2, fill = variable)) +
                                geom_point(pch = 21, size = 2, stroke = 0.1) +
                                theme_void() +
                                scale_fill_distiller('',type = .type, palette = .palette) +
                                labs(title = input$plot_variable)
                        ggplotly(vars)
                }
                else {
                        colors_many <- c("#E6194B","#3CB44B","#FFE119","#0082C8","#F58231","#911EB4","#46F0F0","#F032E6","#D2F53C","#FABEBE","#008080","#E6BEFF","#AA6E28","#FFFAC8","#800000","#AAFFC3","#808000","#FFD8B1","#000080","#808080","#FFFFFF","#000000","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5")
                        variable = data[[input$dataset]][[1]][[input$plot_variable]]
                        ggplot(data[[input$dataset]][[1]], aes(V1, V2, fill = variable)) +
                                geom_point(pch = 21, size = 2, stroke = 0.1) +
                                theme_void() +
                                scale_fill_manual(input$plot_variable,values = colors_many) +
                                labs(title = input$plot_variable)
                }
                
                
        })
       
        #Plot tsne plot Gene expression
        output$tsneExpPlot <- renderPlot({
                         l <- (rowSums(as.matrix(data[[input$dataset]][[2]][, colnames(data[[input$dataset]][[2]]) %in% unique(unlist(str_split(input$gene, c(',',', ', ' ', ' , '))))]))) + 0.1
                        mi <- min(l)
                        ma <- max(l)
                        ColorRamp <- colorRampPalette(c("darkblue","lightblue2","yellow","red2"))(100)
                        ColorLevels <- seq(mi, ma, length = length(ColorRamp))
                        v <- round((l - mi)/(ma - mi) * 99 + 1, 0)
                        
                        kk <- dplyr::bind_cols(data.frame('l'=l), data[[input$dataset]][[1]][,c('V1', 'V2', 'Cluster')]) %>% arrange(l)
                        
                        
                        expr <- ggplot(kk, aes(V1, V2, fill = l)) +
                                geom_point(size = 3, pch = 21, stroke=0.25) +
                                scale_fill_gradientn('', colors = ColorRamp) +
                                theme_void() +
                                labs(title = input$gene)
                
                        expr
        })
        
        
        output$barExpPlot <- renderPlot({
                l <- (rowSums(as.matrix(data[[input$dataset]][[2]][, colnames(data[[input$dataset]][[2]]) %in% unique(unlist(str_split(input$gene, c(',',', ', ' ', ' , '))))]))) + 0.1
                
                kk <- dplyr::bind_cols(data.frame('l'=l), data.frame('Cluster'=data[[input$dataset]][[1]][,c('Cluster')], 'Subpopulation'=data[[input$dataset]][[1]][,c('Subpopulation')], 'Population'=data[[input$dataset]][[1]][,c('Population')]))
                colors_many <- c("#E6194B","#3CB44B","#FFE119","#0082C8","#F58231","#911EB4","#46F0F0","#F032E6","#D2F53C","#FABEBE","#008080","#E6BEFF","#AA6E28","#FFFAC8","#800000","#AAFFC3","#808000","#FFD8B1","#000080","#808080","#FFFFFF","#000000","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5",toupper(c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5')))
                
                
                bars <- ggplot(kk, aes(Cluster, l, fill = Subpopulation)) +
                        stat_summary(fun.y = mean, geom = "bar", color = 'black', lwd=0.25) + 
                        stat_summary(fun.data = mean_cl_normal, geom = "errorbar",width = 0) +
                        scale_fill_manual('Subpopulation', values = rev(colors_many)) +
                        theme_minimal() +
                        labs(title = input$gene) +
                        facet_grid(.~Population,
                                   scales = 'free_x',
                                   drop = T,
                                   space = "free_x") +
                        labs(x='Cluster', y='Mean transcript count')
                
                bars
        })
        
        
        output$MAplot <- renderPlotly({
                a <- data[[input$dataset]][[3]][[as.character(input$cluster)]]
                a <- cbind(gsub('_.*', '',rownames(a)),a)
                colnames(a) <- c('Gene', 'Mean overall', 'Mean in Clust', 'fc', 'p_Value', 'adj_p_Value')
                ma <- ggplot(a, aes(-log10(p_Value), log2(fc))) +
                        geom_point(color = 'red', size=2, alpha=.5) +
                        geom_text(label=as.character(a$Gene)) +
                        labs(title = paste0('Cluster ', input$cluster), x='-log10(p-Value)', y='log2(fold change)') +
                        theme_minimal() +
                        geom_hline(yintercept = 0, size=1, color='red')
                ggplotly(ma)
        })
        
        
        
        #plot upregulated genes
        output$clusterTableUp <- renderTable({
                        a <- data[[input$dataset]][[3]][[as.character(input$cluster)]]
                        a <- cbind(gsub('_.*|\\|.*', '',rownames(a)),a)
                        colnames(a) <- c('Gene', 'Mean overall', 'Mean in Clust', 'fc', 'p-Value', 'adj. p-Value')
                        #a$Gene <- str_trunc(as.character(a$Gene), 15, "right")
                        a[a$fc>1,]
                        

        }, caption=paste("Upregulated Genes"),
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width")
        )
        
        #Plot downregulated genes
        output$clusterTableDown <- renderTable({
                
                a <- data[[input$dataset]][[3]][[as.character(input$cluster)]]
                a <- cbind(gsub('_.*|\\|.*', '',rownames(a)),a)
                colnames(a) <- c('Gene','Mean overall', 'Mean in Clust', 'fc', 'p-Value', 'adj. p-Value')
                #a$Gene <- str_trunc(as.character(a$Gene), 15, "right")
                a[a$fc<1,]
             
                
        }, caption=paste("Downregulated Genes"),
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL)
        )
        
        
      
      
})

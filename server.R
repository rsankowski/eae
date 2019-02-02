
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RColorBrewer)
library(tidyverse)
library(Matrix)
library(plotly)

load('20190131-mouse-eae-data.Robj')

colors_many <- c("#E6194B","#3CB44B","#FFE119","#0082C8","#F58231","#911EB4","#46F0F0","#F032E6","#D2F53C","#FABEBE","#008080","#E6BEFF","#AA6E28","#FFFAC8","#800000","#AAFFC3","#808000","#FFD8B1","#000080","#808080","#FFFFFF","#000000","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5")


shinyServer(function(input, output) {
        
        #plot Tsne with cluster number
        output$tsnePlot <- renderPlot({
        
               tsne <-ggplot(data[[input$dataset]][[1]], aes(V1, V2, fill = Cluster)) +
                        geom_point(pch = 21, size = 3, stroke = 0.25) +
                        theme_void() +
                        scale_fill_manual('Cluster',values = rev(colors_many)) +
                        labs(title = 'Clusters')
               tsne 
               #ggplotly(tsne)
        })
        
        #Plot Tsne with group labels
        output$tsnePlotVar <- renderPlot({
                if (is.numeric(data[[input$dataset]][[1]][[input$plot_variable]])) {
                        .type = "seq"
                        .palette = "YlGnBu"
                        ggplot(data[[input$dataset]][[1]], aes(V1, V2, fill = data[[input$dataset]][[1]][[input$plot_variable]])) +
                                geom_point(pch = 21, size = 3, stroke = 0.25) +
                                theme_void() +
                                scale_fill_distiller(input$plot_variable,type = .type, palette = .palette) +
                                labs(title = input$plot_variable)
                }
                else {
                        colors_many <- c("#E6194B","#3CB44B","#FFE119","#0082C8","#F58231","#911EB4","#46F0F0","#F032E6","#D2F53C","#FABEBE","#008080","#E6BEFF","#AA6E28","#FFFAC8","#800000","#AAFFC3","#808000","#FFD8B1","#000080","#808080","#FFFFFF","#000000","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5")
                        
                        ggplot(data[[input$dataset]][[1]], aes(V1, V2, fill = data[[input$dataset]][[1]][[input$plot_variable]])) +
                                geom_point(pch = 21, size = 3, stroke = 0.25) +
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
                        
                        kk <- bind_cols(data.frame('l'=l), data[[input$dataset]][[1]][,c('V1', 'V2', 'Cluster')]) %>% arrange(l)
                        
                        
                        ggplot(kk, aes(V1, V2, fill = l)) +
                                geom_point(size = 3, pch = 21, stroke=0.25) +
                                scale_fill_gradientn('', colors = ColorRamp) +
                                theme_void() +
                                labs(title = input$gene)
                
                
        })
        
        
        output$barExpPlot <- renderPlot({
                l <- (rowSums(as.matrix(data[[input$dataset]][[2]][, colnames(data[[input$dataset]][[2]]) %in% unique(unlist(str_split(input$gene, c(',',', ', ' ', ' , '))))]))) + 0.1
                
                kk <- bind_cols(data.frame('l'=l), data.frame('Cluster'=data[[input$dataset]][[1]][,c('Cluster')], 'Subpopulation'=data[[input$dataset]][[1]][,c('Subpopulation')], 'Population'=data[[input$dataset]][[1]][,c('Population')]))
                colors_many <- c("#E6194B","#3CB44B","#FFE119","#0082C8","#F58231","#911EB4","#46F0F0","#F032E6","#D2F53C","#FABEBE","#008080","#E6BEFF","#AA6E28","#FFFAC8","#800000","#AAFFC3","#808000","#FFD8B1","#000080","#808080","#FFFFFF","#000000","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5",toupper(c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5')))
                
                
                ggplot(kk, aes(Cluster, l, fill = Subpopulation)) +
                        stat_summary(fun.y = mean, geom = "bar", color = 'black', lwd=0.25) + 
                        stat_summary(fun.data = mean_cl_normal, geom = "errorbar",width = 0) +
                        scale_fill_manual('Cluster', values = rev(colors_many)) +
                        theme_minimal() +
                        labs(title = input$gene) +
                        facet_grid(.~Population,
                                   scales = 'free_x',
                                   drop = T,
                                   space = "free_x") +
                        labs(x='Cluster', y='Mean transcript count')
                
                
        })
        
        
        output$MAplot <- renderPlot({
                a <- data[[input$dataset]][[3]][[as.character(input$cluster)]]
                a <- cbind(gsub('_.*', '',rownames(a)),a)
                colnames(a) <- c('Gene', 'Mean overall', 'Mean in Clust', 'fc', 'p_Value', 'adj_p_Value')
                ggplot(a, aes(-log10(p_Value), log2(fc))) +
                        geom_point(color = 'red', size=3, alpha=.5) +
                        geom_text(label=as.character(a$Gene)) +
                        labs(title = paste0('Cluster ', input$cluster), x='-log10(p-Value)', y='log2(fold change)') +
                        theme_minimal() +
                        geom_hline(yintercept = 0, size=1, color='red')
        })
        
        
        if (F) {output$lineExpPlot <- renderPlot({
                l <- (rowSums(as.matrix(data[[input$dataset]][[2]][, colnames(data[[input$dataset]][[2]]) %in% unique(unlist(str_split(input$gene, c(',',', ', ' ', ' , '))))]))) + 0.1
                
                kk <- bind_cols(data.frame('l'=l), data.frame('Cluster'=data[[input$dataset]][[1]][,c('Cluster')], 'ID'=seq(1:nrow(data[[input$dataset]][[1]]))))
                colors_many <- c("#E6194B","#3CB44B","#FFE119","#0082C8","#F58231","#911EB4","#46F0F0","#F032E6","#D2F53C","#FABEBE","#008080","#E6BEFF","#AA6E28","#FFFAC8","#800000","#AAFFC3","#808000","#FFD8B1","#000080","#808080","#FFFFFF","#000000","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5")
                
                ggplot(kk, aes(x=ID, y=l,color = Cluster)) +
                        geom_bar(stat = 'identity') + 
                        labs(title = input$gene, y = 'Gene Expression', x = 'Cluster') +
                        theme_minimal() +
                        theme(axis.line = element_blank(), 
                              axis.ticks.y = element_blank(), 
                              strip.text.x = element_text(),
                              axis.text.x = element_blank(),
                              strip.background = element_blank(), 
                              panel.grid = element_blank(),
                              panel.spacing.x = unit(0.2, units = 'line'),
                              legend.position = 'None') +
                        scale_color_manual(values = colors_many) +
                        facet_grid(facets = .~Cluster, 
                                   drop = TRUE, 
                                   scales = "free_x", 
                                   switch = "x",
                                   space = "free_x") 
                        
                
                
        })
        }
        
        #plot upregulated genes
        output$clusterTableUp <- renderTable({
                        a <- data[[input$dataset]][[3]][[as.character(input$cluster)]]
                        a <- cbind(gsub('_.*', '',rownames(a)),a)
                        colnames(a) <- c('Gene', 'Mean overall', 'Mean in Clust', 'fc', 'p-Value', 'adj. p-Value')
                        a[a$fc>1,]
                        

        }, caption=paste("Upregulated Genes"),
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width")
        )
        
        #Plot downregulated genes
        output$clusterTableDown <- renderTable({
                
                a <- data[[input$dataset]][[3]][[as.character(input$cluster)]]
                a <- cbind(gsub('_.*', '',rownames(a)),a)
                colnames(a) <- c('Gene','Mean overall', 'Mean in Clust', 'fc', 'p-Value', 'adj. p-Value')
                a[a$fc<1,]
             
                
        }, caption=paste("Downregulated Genes"),
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL)
        )
        
        
        #download plots
        output$downloadExpPlot <- downloadHandler(
                filename = function() { paste(input$cluster, '-tsne.svg', sep='') },
                content = function(file) {
                        ggsave(file,plotInput())
                }
        )
        
      
})

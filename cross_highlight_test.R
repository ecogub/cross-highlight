#goals:
#1. arrange two plots side-by-side
#2. link the plots so that when a point is selected or hovered in one plot,
#   it responds in some way in the other plot. E.g., if you mouseover the dygraph,
#   the corresponding point in the qc plot should light up. you may need both
#   datetime and concentration to uniquely identify the two points that should
#   correspond, so you may have to heavily alter the plot functions. i also
#   haven't thought about this deeply, so if this goal starts to feel
#   incoherent, let us know.

#notes:
#1. this code has been isolated from the macrosheds shiny app. in this form,
#   it's able to run independent of the app. it's just two plot generating
#   functions, both of which return their outputs. Currently, the outputs are
#   being stored in variables (GRAPH_MAIN3a and GRAPH_QC3a). If you execute those
#   variables in the console or from the script in Rstudio, the plots will appear.
#2. the libraries below may be required at some point in your exploration. I'm
#   prettu sure you'll need the ones that are uncommented, and I'm pretty sure
#   you won't need the ones that are. you'll definitely need loon, which I
#   haven't included here.
#3. all the environment variables you'll need (and a lot more) are included in
#   loon_testing_ground.rda, which is loaded below. change the path to wherever
#   you saved the file
#4. i've commented some of the code inside these functions that would normally
#   generate local variables needed to render the plots. in this case, those
#   same variables will be available globally. the commented code has been left
#   for the sake of context
#5. some of the functions called within generate_dygraph and generate_qc are
#   macrosheds functions. these are defined inside helpers.R

# library(V8)
library(feather)
library(plyr)
library(data.table)
# library(shiny)
# library(shinydashboard)
# library(plotly)
library(dygraphs)
# library(ggthemes)
library(jsonlite)
library(lubridate)
library(xts)
# library(leaflet)
library(tidyverse)
library(glue)
# library(shinyjs)

load('~/temp/loon_testing_ground.rda')

generate_dygraph <- function(){

    # sites = na.omit(isolate(input$SITES3[1:3]))
    # varA = isolate(input$VARS3[1])
    # dmns = isolate(get_domains3())
    # conc_flux = isolate(input$CONC_FLUX3)
    # flux_unit = isolate(input$FLUX_UNIT3)
    # conc_unit = isolate(input$CONC_UNIT3)
    # show_pchem = isolate(input$SHOW_PCHEM3)
    # agg = isolate(input$AGG3)
    # dates = isolate(input$DATES3)

    # if(reactive_vals$facet3a == 0) return()

    # if(conc_flux == 'VWC'){
    #     streamdata = volWeightedChem3()
    # } else {
    #     streamdata = dataChem()
    # }
    #
    # if(show_pchem){
    #
    #     if(conc_flux == 'VWC'){
    #         raindata = volWeightedPchem3()
    #     } else {
    #         raindata = dataPchem()
    #     }
    #
    # } else {
    #     raindata = tibble()
    # }

    alldata = prep_mainfacets3(varA, dmns, sites, streamdata, raindata,
                               conc_flux_selection=conc_flux, show_input_concentration=show_pchem)

    rainsites = get_rainsites(raindata, alldata, streamsites=sites,
                              show_input_concentration=show_pchem)

    yunit = ifelse(conc_flux == 'Flux', flux_unit, conc_unit)
    ylab = get_ylab(varA, conc_flux, yunit)

    if(nrow(alldata)){

        displabs = colnames(alldata)[-1]
        dydat = xts(alldata[, displabs], order.by=alldata$datetime, tzone='UTC')
        dimnames(dydat) = list(NULL, displabs)

        is_inst = ifelse(agg == 'Instantaneous', TRUE, FALSE)
        dg = dygraph(dydat, group='nSiteNVar') %>%
            dyOptions(useDataTimezone=FALSE, drawPoints=FALSE,
                      colors=selection_color_match(sites, displabs, linecolors),
                      strokeWidth=2, pointSize=2,
                      retainDateWindow=TRUE, drawGapEdgePoints=TRUE,
                      connectSeparatedPoints=is_inst) %>%
            dyLegend(show='always', labelsSeparateLines=FALSE,
                     labelsDiv='main3a') %>%
            dyAxis('y', label=ylab, labelWidth=16, labelHeight=10,
                   pixelsPerLabel=20, rangePad=10)

        if(show_pchem){

            rain_or_pchem_cols = selection_color_match(paste0('P_', sites),
                                                       paste0('P_', displabs[displabs %in% sites]),
                                                       pchemcolors)

            for(i in 1:length(rainsites)){
                dg = dySeries(dg, name=rainsites[i], color=rain_or_pchem_cols[i],
                              axis='y', drawPoints=FALSE, strokeWidth=2,
                              pointSize=2, strokePattern='dashed')
            }
        }

    } else {

        dg = plot_empty_dygraph(dates, mainlab=colnames(alldata)[-1],
                                maindiv='main3a', plotgroup='nSiteNVar', ylab=ylab, px_per_lab=20)
    }

    return(dg)
}
GRAPH_MAIN3a <- generate_dygraph()

generate_qc <- function(){

    # sites = na.omit(isolate(input$SITES3[1:3]))
    # varA = isolate(input$VARS3[1])
    # dmns = isolate(get_domains3())
    # conc_unit = isolate(input$CONC_UNIT3)
    # show_pchem = isolate(input$SHOW_PCHEM3)
    # agg = isolate(input$AGG3)
    # dates = isolate(input$DATES3)
    #
    # if(reactive_vals$facet3a == 0 || ! show_qc) return()
    #
    # streamdata = dataChem() %>%
    #     select(datetime, site_name, !!varA)
    #
    # dischargedata = dataQ()

    alldata <- inner_join(streamdata,
                          dischargedata,
                          by = c("datetime", "site_name")) %>%
        rename(value = !!varA)

    qc <- ggplot(alldata,
                 aes(x = discharge, y = value, colour = site_name),
                 environment=environment()) +
        geom_point(na.rm = TRUE) +
        scale_colour_manual(values = linecolors,
                            breaks = c(sites)) +
        labs(y = "") +
        ggthemes::theme_few() +
        theme(legend.position = 'none')

    return(qc)
}
GRAPH_QC3a <- generate_qc()

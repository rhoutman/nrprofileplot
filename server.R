# options("tercen.serviceUri"="http://tercen:5400/api/v1/")
# options("tercen.username"="admin")
# options("tercen.password"="admin")
# 
# options("tercen.workflowId"= "66bd7a1b9968e8ab5a45a0cdc4007c24")
# options("tercen.stepId"= "26-9")

library(shiny)
library(shinyjs)
library(tercen)
library(tidyverse)
library(plotly)

shinyServer(function(input, output, session){
  
ctx <- reactive(getCtx(session))

ctxcore <- reactive(getValues(session))

thechoices <- reactive({
  # choicelabels  <- gsub(pattern="[a-zA-Z0-9]*\\.", replacement="", x= ctxcore() %>% colnames() )
  choicelabels  <- ctxcore() %>% colnames() %>% cleannames()
 choicelabels %>% make.names() %>% as.list() %>% setNames(choicelabels)
})


sepoptions <- reactive({
  # thechoices <- function(){choicelabels %>% make.names() %>% as.list() %>% setNames(choicelabels)}
  exclude <-  c("x", "y", "ri", "ci")
  
 thechoices()[!(thechoices() %in% exclude)]
})

output$colfactselect <- renderUI({
 # septoptions <- function(){ thechoices()[!(thechoices() %in% exclude)]}
preselectedcolor <- ctx()$colors %>%  cleannames() %>% make.names()

  pickerInput("selectedcolfact","Color by:", as.list(sepoptions()), options = list(`actions-box` = TRUE),selected=preselectedcolor, multiple = T)

})

colordf <- reactive({
  df <- ctxcore()
  colnames(df) <- colnames(df) %>% make.names()
  
  colfactselect <- input$selectedcolfact
  if (colfactselect  %>% length() > 0) {
    df <- df %>%
      unite("color", colfactselect, remove = F)
  } else {
    df$color <- "all"
  }
  
  return(df)
})

output$test <- renderDataTable({
  print(dataselect())})

output$colorlist <- renderUI({
  thechoices <-  colordf() %>% pull(color) %>% unique()
  pickerInput("selectcolor","Select data", as.list(thechoices), options = list(`actions-box` = TRUE),selected= as.list(thechoices), multiple = T)
})

rcfactors <- reactive({
  rcfactors <- c(ctx()$cnames %>% as.character(),  ctx()$rnames %>% as.character()) %>% unique()
  rcfactors <- rcfactors[!rcfactors==""] %>% cleannames()
  })

output$selectgraph <- renderUI({
  df <- dataselect()
 options <- df  %>% pull(rcfact) %>% unique() 
  
selectInput("graphselect", "select subset", choices= options %>% as.list(), selected = NULL, multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL)
  })

# output$test2 <- renderPrint(input$selectcolor)

dataselect<- reactive({
  df <- colordf() %>% 
    dplyr::filter(color %in% input$selectcolor) 

rcfactors <- rcfactors()    
  if (rcfactors  %>% length() > 0) {
    df <- df %>%
      unite("rcfact", rcfactors, remove = F)
  } else {
    dfrcfactors <- "all"
  }  
return(df)
})

makeplot <- reactive({
  # assess grouping by Color
  group <- ctx()$colors %>% as.character() %>% unique()

  filename = "profile"
  df <- dataselect() %>%
    filter(rcfact==input$graphselect)
 
if(input$colororder !="") {
  colororder <- paste(str_trim(strsplit(input$colororder, split=",")[[1]]),sep=",")
  df$color <- factor(df$color, levels = colororder)
}
  

p <-   ggplot(df, aes(x, y)) +
    geom_line(aes(color=color, group=color), size=input$linesize)

# draw origin
if(input$origin){
  p <-  p +
    expand_limits(y = 0)  +
    geom_hline(aes(yintercept=0), size=0.25, color="black")
  filename <- paste(filename, ".orig", sep="")
}

if(input$point==T) {
  # if(length(group)<1){
  #   if(transparency){
  #     p <- p+
  #       geom_point(aes(alpha=transp), color="orangered2",size=point) +
  #       scale_alpha(range=c(0,1), limits=c(0.5,1), na.value = 0, guide=F) +
  #       theme(
  #         legend.position="none")
  #   }else{
  #     p <- p +
  #       geom_point(color="orangered2",size=input$pointsize) +
  #       theme(
  #         legend.position="none")}
  # }else{
    # if(transparency =="Y"){
      # p <- p +
      #   geom_point(aes(alpha=transp), size=pointsize) +
      #   scale_alpha(range=c(0,1), limits=c(0.5,1), na.value = 0, guide=F)}else{
          p <- p +
            geom_point(aes(color=color),size=input$pointsize)
        # }

    filename <- paste(filename, ".p", sep="")
  # }
}

# define color schemes
if(length(group)>0){
  if(input$colorscheme=="divergent"){

    colnum <- length(unique(df$color))

    if(colnum < 5){
      p <- p +
        scale_color_manual( values=lego.col)
    }

    if(colnum> 4 & colnum < 10){
      p <- p +
        scale_colour_brewer(palette="Set1")
    }
  }

  if(input$colorscheme=="continuous"){

    library(RColorBrewer)
    subblues <- brewer.pal(9,"Blues")[4:9]
    breaks <- length(unique(df$color))
    blues_fun <- colorRampPalette(subblues)
    values <- blues_fun(breaks)
    p <- p +
      scale_colour_manual(values=values)
  }
}

# apply generic figure format for IDs on xaxis
p <- format155bar(p)

# display error bars
if(input$errorbars){
  df$ymin <- df$y - df$error
  df$ymax <- df$y + df$error
  
  p <- p +
    geom_errorbar(data=df, aes(ymin=ymin, ymax=ymax, color=color), size=0.3, width=0.3)
  filename <- paste(filename, ".eb", sep="")
}

# significance asterisks

if(input$displayp){

  df$pstars <- as.character(symnum(df$significance, cutpoints=c(0,0.001,0.01,0.05,1), symbols=c('***', '**', '*', '' ), legend=F))
 
  if(input$antagsignificance){
    controlnames <- c("DMSO", "Ethanol", "EtOH", "Solvent", "Vehicle", "Control")
    
    if(length(as.character(ctx()$colors)) > 2){
      stop(call.=F, "Sorry this only works for significance of one condition vs control" )} else{
        
        setsignperID <- function(df){
          control <- subset(df, color %in% controlnames)
          nocontrol <- subset(df, !(color %in% controlnames))
          
          if(control$y > nocontrol$y) {
            pstars  <- nocontrol$pstars
            df$pstars <- with(df, replace(pstars, color %in% controlnames, pstars)) 
            df$pstar <- with(df, replace(pstars, !(color %in% controlnames), "")) 
          }
          return(df)
        }
        
        if(max(df$y>5 )){
          
          df <- ddply(df, ~ID,setsignperID )
        }  
      }
    }

  if(max(df$y<5 )) {df$yast <- input$astoffset *df$y } else{
    if(input$errorbars=="Y" ){
      df$yast <-input$astoffset(df$y + df$significance + max(df$y)/10)
    }else{
      df$yast <- df$y + max(df$y)/10
    }
  }
  
  if(!(input$rotask)){
    p <- p +
      geom_text(data=df, aes(label=pstars, y=yast ), size=input$astsize, color="black")} else {
        p <- p +
          geom_text(data=df, aes( label=pstars, y=yast ), size=input$astsize,angle =90,vjust=0.8, color="black")  
      }
  
  filename <- paste(filename, ".sign", sep="") 

}

# y axis title
if(input$ytit==""){
  p <- p +
    theme(
      axis.title.y=element_blank()
    )} else {
      p <- p +
        ylab(input$ytit)
    }


#facetting
 # output$uifacetx <- renderUI({
 #   sepoptions <- c("none", sepoptions())
 #   selectInput("facetx", label = "horizontal facets",
 #               choices =as.list(sepoptions), selected = 1)
 # })
 # 
 # output$uifacetx <- renderUI({
 #   sepoptions <- c("none", sepoptions())
 #   selectInput("facety", label = "vertical facets",
 #               choices =as.list(sepoptions), selected = 1)
 # })
 
if(input$facetx !="none" | input$facety !="none"){


  if(input$facetx !="none" & input$facety =="none"){
    ffacet <- paste(". ~", input$facetx)
    filename <- paste(filename, ".xby",input$facetx, sep="")
  }

  if(input$facetx =="none" & input$facety !="none"){
    ffacet <- paste(input$facety, " ~ .")
    filename <- paste(filename, ".yby",input$facety, sep="")
  }

  if(input$facetx !="none" & input$facety !="none"){
    ffacet <- paste(input$facety , "~", input$facetx)
    filename <- paste(filename, ".xby",input$facetx, ".yby",input$facety,sep="")
  }

  p <- p +
    facet_grid(ffacet)

  if(input$yscale=="free"){
    p <- p +
      facet_grid(ffacet, scales="free_y")
    filename <- paste(filename, ".yfree",sep="")
  }
}

#manual y scale
output$yrange <- renderUI({
  
  arange <<- dataselect
  # arange <<- arange
  amin = 1.1*min(arange$y)
 amax = 1.1*max(arange$y)

  
  sliderInput("manualy", "y-axis range:",
              min = 0, max = 1000,
              value = c(0,1000))
})


if(input$yscale=="manually"){

  ymin <- input$manualy[1]
  ymax <- input$manualy[2]
  p <- p +
    ylim(c(ymin,ymax))
  filename <- paste(filename, ".yman",sep="")
}

# legend 

if(input$legopts=="horizontal"){ 
  p <- p +
    theme(
      legend.position=c(input$legx,
                        input$legy)) +
    guides(color=guide_legend(title= input$legtit,
                              title.position="left",                        
                              direction="horizontal"))
}

if(input$legopts=="hide"){ 
  p <- p +
    theme(
      legend.position="none") 
}

if(input$legopts=="separate"){ 
  
  if(colnum < 5){
    p <- p +
      scale_color_manual(title = input$legtit, values=lego.col)
    legwidth <- 2
    legheight <- 2
  }
  
  if(colnum> 4 & colnum < 10){
    p <- p +
      scale_colour_brewer(name = input$legtit, palette="Set1")
    legwidth <- 3
    legheight <- 2 
  }
  
  if(colnum >9){
    p <- p  +
      guides(color= guide_legend(title =input$legtit, title.hjust=0, ncol=ceiling(colnum/20)))
    legwidth <- 5
    legheight <- 5 }
  
  p <- p +
    theme( 
      legend.title = element_blank(),  
      legend.key=element_blank() ,
      legend.background=element_blank())
  
  legend <- extractlegend(p)
  
  p <- p +
    theme(
      legend.position="none") 
  
  
  
}

# main title
asplit <-""
if(asplit !=""){
  p <- p +
    ggtitle(thesplit)
} else{
  if(input$maintit !=""){
    p <- p +
      ggtitle(input$maintit) 
  } 
  p <- p + 
    theme(plot.title = element_text(hjust = 0.5))
}




return(p)
  })

output$mainplot <- renderImage({
  outfile <- tempfile(fileext='.png')

  png(outfile,  width=input$figwidth, height= input$figheight, units="in", res=600)
#   
 print(makeplot() ) 
#   # print(tile)
#   # plotheatmap(tile ,dd.row = clusterdf()$dd.row, dd.col= clusterdf()$dd.col,alegend=  basetile()$alegend,  xvp=input$legendx, yvp= input$legendy,legendsize=input$legendsize, plotfolder = NULL,  filename="heatmap",filetype=".pdf", width=input$figwidth, height=input$figheight)
  dev.off()
#   
#   
  file.copy(from=outfile, to=file.path("plottemp", "plot.png"),overwrite = T)
#   
#   #  Return a list
  list(src = outfile,
  contentType = 'image/png',
       width = input$figwidth *120,
       height = input$figheight*120,
       alt = "This is alternate text")
}, deleteFile = T)
# 
output$saveplot <- downloadHandler(
  filename = "plot.png",
  content = function(file) {
    file.copy(file.path("plottemp", "plot.png"), file)
  }
)
})

# helper functions are below
getCtx = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)

  token = query[["token"]]
  taskId = query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx = tercenCtx(taskId=taskId, authToken=token)
  
  return(ctx)
}

getValues = function(session){

ctx = getCtx(session)

# ctx <- tercenCtx() 

# select the data
df <- ctx$select()

basicselect <- c(".ci",".ri",".x", ".y")

# extract color info
colorfact <- ctx$colors %>% as.character()
if(colorfact %>% length() >0) basicselect <- c(basicselect, colorfact)

# extract significance info
labelfact <- ctx$labels %>% as.character()
if(labelfact %>% length() >0){
  df$significance <- df[[labelfact]]
  basicselect <- c(basicselect, "significance")
  
} 

if(".error" %in% colnames(df)) basicselect <- c(basicselect , ".error")

basicselect <- basicselect %>% unique()

# extract the data
  df <- df %>% select(
    basicselect
      )
# add column data
  columns <-  ctx$cselect() 
  if(colnames(columns) != ".all"){
  columns$.ci <- columns %>% rownames() %>% as.numeric() -1
  df <- df %>% 
    left_join(columns, by=".ci")
  }
  
  
  rows <-  ctx$rselect() 
  if(colnames(rows)  != ".all"){
    rows$.ri <- rows %>% rownames() %>% as.numeric() -1
    df <- df %>% 
      left_join(rows, by=".ri")
  }
  
  colnames(df) <-colnames(df) %>% cleannames()

  return(df)
}

cleannames <- function(x){
  gsub(pattern="[a-zA-Z0-90-9]*\\.", replacement="", x= x)
}

lego.col <- c("orangered2" ,"blue3","chartreuse3","darkgoldenrod1")

format155 <- function(p){
  p1 <- def.format(p) + theme(axis.text.x = element_text(size = 4, 
                                                         angle = 90, hjust = 1, vjust = 0))
  return(p1)
}

format155bar <-function(p){
  p1 <- format155(p) + theme(axis.text.x = element_text(size = 4, 
                                                        angle = 90, hjust = 1, vjust = 0.5))
  return(p1)
}

def.format <- function(p){
  theme_set(theme_bw())
  p1 <- p + theme(axis.ticks.length = unit(0.1, "cm"), axis.title.x = element_blank(), 
                  legend.key = element_blank(), legend.background = element_blank(), 
                  axis.ticks = element_line(size = 0.05)) + scale_x_discrete(expand = c(0.01, 
                                                                                        0.01))
  return(p1)
}

extractlegend <- function(x){
  tmp <- ggplot_gtable(ggplot_build(x)) 
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
legend <- tmp$grobs[[leg]] 
return(legend)}

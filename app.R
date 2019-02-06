#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
# Define UI for application that draws a histogram
ui <- fluidPage(tabsetPanel(
  tabPanel(
    "Fiches France",
    
    # Application title
    titlePanel("Chiffres de la Note"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          "tableau",
          "Tableau",
          c("Fiche Production", "Fiche Investissement", "Fiche Revenu","Fiche Echanges exterieurs","Fiche Consommation et investissement des menages","Fiche PIB France"),
          multiple = F,
          selectize = T
        ),
        uiOutput("liste_gf"),
        uiOutput("liste_gf_old"),
        checkboxInput("digi_fr", "Deuxieme decimale")#,
        #actionButton("export", "Exporter en CSV")#,
        #submitButton("Feu")
      ),
      
      # Show a plot of the generated distribution
      mainPanel("Attention le grisage n'est qu'indicatif et seules les donnees dans les sorties du garde-fou apparaissent. Si rien n'apparait vous n'avez pas acces a O://Special/Special.GFou",htmlOutput("table"))    )
  ),
  tabPanel(
    "Fiches Internationales",titlePanel("Chiffres de la Note"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          "tableau_int",
          "Tableau",
          c("Petrole/Marches FI","Fiche Zone euro", "Fiche Espagne", "Fiche Italie","Fiche Allemagne","Fiche US","Fiche Royaume-Uni","Fiche Japon","Economies emergentes"),
          multiple = F,
          selectize = T
        ),checkboxInput("digi", "Deuxieme decimale")),mainPanel("Attention le grisage n'est qu'indicatif",htmlOutput("table_int"))
   
  ))))


# Partial example

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  ###Les Rdata qui seront utilises dans la fiche
  
  d_fr<<-"N:/GDESE/O-GDESE/Special/Special.GFou/Sorties/syntheseGFOU"
  liste_gf<<-rev(dir(d_fr)[grepl("sortieGFRdata",dir(d_fr))])
  #d_fr<-NULL
  #liste_gf<<-rev(tail(dir()[grepl("sortieGFRdata",dir())]))
  output$liste_gf_old <- renderUI({
  
    selectInput(
        "Gfou_old",
        "Garde-fou a comparer",
        liste_gf,
        multiple = F,
        selectize = T
      )                                    
  })
  output$liste_gf <- renderUI({
    
    selectInput("Gfou", "Garde-fou", liste_gf,
                multiple=F, selectize=T)
  })
  
  
  
  options(stringsAsFactors=FALSE)
  source(file="tabl_prod.R")
  source(file="tabl_inv.R")
  source(file="tabl_rev.R")
  source(file="tabl_comext.R")
  source(file="tabl_conso.R")
  source(file="tabl_pibfr.R")
  ifelse(####choix automatique du dernier trimestre du tableau, si mois de novembre/decembre on change d'annee
    month(Sys.Date() ) %in% 11:12,
    trim <<-
      c(year(Sys.Date()) + 1, 2), trim <<-
      c(
        year(Sys.Date()), ifelse(month(Sys.Date()) %in% 1:3, 2, 4)
      ))
  #trim<<-c(2019,2)#dernier trimestre a apparaitre dans le tableau, fait maintenant automatiquement
  annee<<-trim[1]  ##derniere annee a apparaitre
  source(file="fonctions_pour_fiche.R")
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% rownames(table_noteRch)) return(ts(table_noteRch[acharger,],start=c(as.numeric(substr(names(table_noteRch[acharger,])[1],1,4)),as.numeric(substr(names(table_noteRch[acharger,])[1],6,6))),frequency=4))
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(rownames(table_noteRch))),sep=" "))
    
  }
   output$table <- renderText({
     b_fr<<-paste(d_fr,input$Gfou,sep="//") 
     #b_fr<<-input$Gfou
     b_fr_old<<-paste(d_fr,input$Gfou_old,sep="//")
     #b_fr_old<<-input$Gfou_old
     
     switch(
       input$tableau,
       "Fiche Production" = tabl_prod(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche Investissement" = tabl_inv(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche Revenu" = tabl_rev(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche Echanges exterieurs" = tabl_comext(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche Consommation et investissement des menages"= tabl_conso(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche PIB France"=tabl_pibfr(dig = ifelse(input$digi_fr, 2, 1))
     )
     
   })
   
  
   #d_int<<-"U://M-G140//Usuels.dsc//pRev//previsions"
   d_int<<-"N://GDCJ//N-GDCJ//Echanges.DCJ//DSC//Rdata internationaux"
   
   
   
   
   source(file="tabl_ze.R")
   source(file="tabl_es.R")
   source(file="tabl_us.R")
   source(file="tabl_it.R")
   source(file="tabl_al.R")
   source(file="tabl_uk.R")
   source(file="tabl_jp.R")
   source(file="tabl_em.R")
   source(file="tabl_fi.R")
   output$table_int <- renderText({
    
     switch(
       input$tableau_int,
       "Petrole/Marches FI"=tabl_fi(dig=ifelse(input$digi,2,1)),
       "Fiche Zone euro" = tabl_ze(dig=ifelse(input$digi,2,1)),
       "Fiche Espagne" = tabl_es(dig=ifelse(input$digi,2,1)),
       "Fiche US" = tabl_us(dig=ifelse(input$digi,2,1)),
       "Fiche Italie" = tabl_it(dig=ifelse(input$digi,2,1)),
       "Fiche Allemagne" = tabl_al(dig=ifelse(input$digi,2,1)),
       "Fiche Royaume-Uni"=tabl_uk(dig=ifelse(input$digi,2,1)),
       "Fiche Japon"=tabl_jp(dig=ifelse(input$digi,2,1)),
       "Economies emergentes"=tabl_em(dig=ifelse(input$digi,2,1))
     )
     
   })  
   

}

# Run the application 
shinyApp(ui = ui, server = server)

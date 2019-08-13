
library(tidyverse) #all basic packaged such ggplot2
library(googlesheets) # for loading the GeoPT data from Google drive
library(readxl) # for Excel import
library(shiny)
library(DT)
library(purrr)

#Changes: 
#20190804 IAG logo added
#20190805 Disclaimer added

#Ideas:
#20190804 fix colours for prep.type
#20190804 improve header title page
#20190804 make file upload possible
#20190805 fix sign figures in DT


mytheme <- theme_grey() + theme(plot.title = element_text(colour = "black", size = rel(2))) + theme(axis.title.x = element_text(size = rel(1.8)))+ theme(axis.title.y = element_text(size = rel(1.4))) + theme(axis.text.x = element_text(angle=30, hjust=1, size = rel(1.5))) + theme(axis.text.y = element_text(size = rel(1.2))) + theme(legend.title = element_text(size = rel(1.2))) + theme(legend.text = element_text(size = rel(1))) + theme(strip.text = element_text(colour = "black", size = rel(2)))
unit <- "ng/g"

#loading data

loadData <- function() {
  # Grab the Google Sheet
  sheet <- gs_title("OKUM.MUH.PGE") 
  # Read the data
  gs_read_csv(sheet, col_types = cols(
    lab = col_character(),
    analyte = col_character(),
    method = col_character(),
    prep = col_character(),
    mass = col_double() ,
    measurand = col_double(),
    round = col_character()
  ))
}

OKUM.MUH.PGE <- loadData() # file in Google Drive needs to be named GeoPTall and not GeoPTall.csv
# need to check if headers are available
# need to upload latest version of "~/GitHub/PGEcertification/OKUM.MUH.PGE.xlsx" then open with Google Sheets
# and save as Googlesheet

#OKUM.MUH.PGE <- read_excel("c:/Users/meisel/Documents/GitHub/PGEcertification/OKUM.MUH.PGE.xlsx") # EXCEL import
lab_select <- OKUM.MUH.PGE

RefMatList <- sort(unique(OKUM.MUH.PGE$RM))
AnalList <- c("Ru", "Rh", "Pd","Re", "Os", "Ir", "Pt", "Au", "R.187Os_188Os","Se","Ag","Cd","In", "Te")
LabList <- unique(OKUM.MUH.PGE$lab.)
LabList2 <- unique(OKUM.MUH.PGE$lab)

plotPGE <- function(ref, element, lab.sel, rem) { # plotting ref = Reference Material, element = analyte, lab.sel = lab to be removed, TRUE = remove lab, FALSE = include lab
  
  anal_sub <-lab_select[lab_select$RM %in% ref,c("lab","lab.","scientist","RM", "mass2", element, "prep.type", "year")]
  #anal_sub <- filter(OKUM.MUH.PGE, RM == ref)
  anal_sub <- anal_sub[complete.cases(anal_sub[[element]]),]
  #anal_sub <- na.omit(anal_sub)
  
  if(rem == "TRUE"){ 
    anal_sub <-  subset(anal_sub, anal_sub$lab. != lab.sel)} else {
      anal_sub <-  subset(anal_sub, anal_sub$lab. == lab.sel)
    }
  
  if(element == "R.187Os_188Os"){
    unit <- "ratio"
  } else { unit <- "ng/g"}
  
  if(element %in% c("Se","Ag","Cd","In", "Te")){
    unit <- "µg/g"
  } else { unit <- "ng/g"}
  
  names(anal_sub)[names(anal_sub) == element] <- "anal" # changing the column header with from variable "element" to an universal "anal"
  
  names(anal_sub)[names(anal_sub) == "mass2"] <- "test_port_g" # changing the column header "mass2" to "test portion size" to make the legend better readable
  
  anal_sub$lab. = with(anal_sub, reorder(lab., anal, median)) # ordering the labs by increasing lab median
  
  ggplot(data = anal_sub) +
    geom_boxplot(mapping = aes(x=lab., y=anal, fill=prep.type), outlier.shape = NA, varwidth = FALSE) + facet_wrap(~RM) + 
    geom_jitter(mapping = aes(x=lab., y=anal, colour=test_port_g, size=test_port_g), alpha = 0.3, width = 0.25) + 
    ylab(unit) + labs(title=element) + labs(fill='prep type') + mytheme
}

tabPGE <- function(ref, element) { # plotting ref = Reference Material, element = analyte, lab.sel = lab to be removed, TRUE = remove lab, FALSE = include lab
  
  #ref <- "OKUM"
  #element <- "Ir"
  
  anal_sub <-lab_select[lab_select$RM %in% ref,c("lab","scientist","RM", "mass2", element, "prep.type", "year", "DOI")]
  anal_sub <- anal_sub[complete.cases(anal_sub[[element]]),]
  
  
  # testing for units, isotope ratio does not have one
  if(element == "R.187Os_188Os"){
    unit <- "ratio"
  } else { unit <- "ng/g"}
  
  if(element %in% c("Se","Ag","Cd","In", "Te")){
    unit <- "µg/g"
  } else { unit <- "ng/g"}
  
  anal_sub$unit <- unit
  
  #names(anal_sub)[names(anal_sub) == element] <- "anal" # changing the column header with from variable "element" to an universal "anal"
  
  names(anal_sub)[names(anal_sub) == "mass2"] <- "Test port [g]" # changing the column header "mass2" to "test portion size" to make the legend better readable
  
  #anal_sub$lab. = with(anal_sub, reorder(lab., Ir, median)) # ordering the labs by increasing lab median
  
  datatable(anal_sub) %>%
   formatSignif(element, 3) %>%
    formatRound(element, 3)
  tab <- anal_sub
  tab
}


ui = fluidPage(

  #Title panel
  #titlePanel("PGE and other siderophile elements - literature data"),
  titlePanel(title=div(img(src="IAG_logo.png", height = 60, width = 60), "PGE and other siderophile elements - literature data", style="color:#dd9933"), windowTitle="RMapp"),
 navbarPage("Selection",
             tabPanel("boxplots",
  
  # Typical sidebar layout with text input
  sidebarLayout(
    sidebarPanel(
      selectInput("RefMat", label = "Select RM",
                  choices = RefMatList, selected = "TDB-1"),
      
      #selectInput("Analyte", label = "Select analyte:",
      #            choices = AnalList, selected = "Ir"),
      
      #selectInput("rmLab", label = "Exclude lab:",
      #            choices = LabList, multiple=FALSE, selected = "VSGEI"),
      uiOutput("Analyte"),
      uiOutput("removeLab"),
      uiOutput("lab")

    ),
    # Plot is placed in main panel
    mainPanel(
      plotOutput("PGEplot"),
      plotOutput("PGElab")#,
      #tableOutput("view"),
      #tableOutput("view2")
     )
    )
   ),
  tabPanel("data table",
           h2("tables"),
           DT::dataTableOutput("PGEtable"),
           # Button
           downloadButton("downloadData", "Download data table")
           ),
           
  tabPanel("literature",
              h2("data source"),
           DT::dataTableOutput("lit"),
           # Button
           downloadButton("downloadDataLit", "Download litarature data table")
           ),
  tabPanel("about",
           p(strong("This is RMapp")),
           p("This ShinyApp is based on data collected for geological and environmental reference material (RM). The data sources are literature, IAG GeoPT results and RM certificates. The ShinyApp is hosted by the International Association of Geoanalysts (IAG)", span("http://www.geoanalyst.org/", style = "color:blue")),
           p(strong("Author")),
           p("It was built and maintained by Thomas C. Meisel, General and Analytical Chemistry, Montanuniversität Leoben, Austria as an example to make RM data available to the geoscience community."),
           p(strong("Terms")),
           p("Written by Thomas C. Meisel (thomas.meisel@unileoben.ac.at). Montanuniversität Leoben, 8700 Leoben, AUSTRIA. Written in programming language R (R Development Core Team, 2015. Vienna, Austria. www.r-project.org) version 3.5 (2019).
"),
           p("This app is provided solely for the scientific community and may be taken down at any time with no notice because TCM gets tired of it. It is provided “as is”. use at your own risk and hopefully enjoy :).")
            ),
           p(strong("Disclaimer")),
           p("Although this program is hosted by the IAG (International Association of Geoanalysts, no warranty, expressed or implied, is made by the IAG or the Montanuniversität Leoben as to the accuracy and functioning of the programme and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the IAG and the Montanunivesität in connection therewith.")
        
  )
)


server <- function(input, output){
  # creating controls on the fly with "renderUI". Lab list is created based on RM selection
  # see https://shiny.rstudio.com/articles/dynamic-ui.html
  output$Analyte <- renderUI({
    PGE <- lab_select[lab_select$RM == input$RefMat, c(AnalList)]
    PGE_NA_cols_removed <- PGE[!map_lgl(PGE, ~ all(is.na(.)))]
    Analyte.list <- colnames(PGE_NA_cols_removed)
    selectInput("Analyte", label = "Select analyte:",
                choices = Analyte.list)
  })
  
  
  # creating controls on the fly with "renderUI". Lab list is created based on RM selection
  # see https://shiny.rstudio.com/articles/dynamic-ui.html
  output$lab <- renderUI({
    PGE <- lab_select[lab_select$RM == input$RefMat,] # selecting data based on RM input
    PGE_NA_cols_removed <- PGE[!map_lgl(PGE, ~ all(is.na(.)))] # removing all empty columns
    PGE_NA_cols_removed  <- PGE_NA_cols_removed[complete.cases(PGE[[input$Analyte]]),] # removing incomplete data sets
    lab.list <- unique(PGE_NA_cols_removed$lab.) # extracting the list of remaining labs for choices
    lab.list <- sort(lab.list)
    #lab.list <- c("NA", lab.list)
    selectInput("lab.sel", label = "Select lab for detailed view:",
                choices = lab.list)
  })
  
  # creating controls on the fly with "renderUI". Lab list is created based on RM selection
  # see https://shiny.rstudio.com/articles/dynamic-ui.html
  output$removeLab <- renderUI({
    PGE <- lab_select[lab_select$RM == input$RefMat,] # selecting data based on RM input
    PGE_NA_cols_removed <- PGE[!map_lgl(PGE, ~ all(is.na(.)))] # removing all empty columns
    PGE_NA_cols_removed  <- PGE_NA_cols_removed[complete.cases(PGE[[input$Analyte]]),] # removing incomplete data sets
    lab.list <- unique(PGE_NA_cols_removed$lab.)
    lab.list <- sort(lab.list)
    lab.list <- c("N/A", lab.list)
    selectInput("rmLab", label = "Exclude outlying lab:",
                choices = lab.list)
  })
  

  
  output$PGEplot <- renderPlot({
    
    plotPGE(input$RefMat, input$Analyte, input$rmLab, "TRUE")
  })
  
  output$PGElab <- renderPlot({
    
    plotPGE(input$RefMat, input$Analyte, input$lab.sel, "FALSE")
  })
  
  output$PGEtable <- DT::renderDataTable({
    tabPGE(input$RefMat, input$Analyte)
  })
  
  output$lit <- DT::renderDataTable({
    PGE <- lab_select[lab_select$RM == input$RefMat,] # selecting data based on RM input
    PGE_NA_cols_removed <- PGE[!map_lgl(PGE, ~ all(is.na(.)))] # removing all empty columns
    PGE_NA_cols_removed  <- PGE_NA_cols_removed[complete.cases(PGE[[input$Analyte]]), c("lab", "lab.", "scientist", "year", "DOI")] # removing incomplete data sets
    lit <- unique(PGE_NA_cols_removed) # extracting unique rows only
    lit
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$RefMat,"_data", ".csv", sep = "")
    },
    content = function(file) {
      tab <- tabPGE(input$RefMat, input$Analyte)
      write.csv(tab, file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset ----
  output$downloadDataLit <- downloadHandler(
    filename = function() {
      paste(input$RefMat, "_literature",".csv", sep = "")
    },
    content = function(file) {
      PGE <- lab_select[lab_select$RM == input$RefMat,] # selecting data based on RM input
      PGE_NA_cols_removed <- PGE[!map_lgl(PGE, ~ all(is.na(.)))] # removing all empty columns
      PGE_NA_cols_removed  <- PGE_NA_cols_removed[complete.cases(PGE[[input$Analyte]]), c("lab", "lab.", "scientist", "year", "DOI")] # removing incomplete data sets
      lit <- unique(PGE_NA_cols_removed) # extracting unique rows only
      lit
      write.csv(lit, file, row.names = FALSE)
    }
  )
  output$view <- renderPrint({print(input$lab.sel)})
  
  output$view2 <- renderPrint({print(input$Analyte)})

}



# Run the application 
shinyApp(ui = ui, server = server, 
         options = list(height = 1000))



---
output: html_document
runtime: shiny
---
# Global variables can go here
library(shiny)
library(tidyverse)
library(colorspace)
library(Hmisc)
library(gridExtra)
library(grid)
library(lattice)
library(GGally)
PFAS_raw <- read.csv("/Users/mackenzie/Desktop/Oregon State Documents/Classes/Data Visualization/Assingment_3/PFAS_for_R.csv")
##replace with location on your computer 

#renaming
names(PFAS_raw) <- tolower(names(PFAS_raw))
PFAS <- PFAS_raw %>% 
  rename(lifestage=organism.lifestage, conc.type=conc.1.type..standardized., conc.mean=conc.1.mean..standardized.,
         conc.units=conc.1.units..standardized.,duration= observed.duration..days.) 

class(PFAS$cas.number)
PFAS$cas.number <-as.character(PFAS$cas.number)
PFAS$conc.mean <- as.numeric(PFAS$conc.mean)
PFAS$number.of.doses <- as.factor(PFAS$number.of.doses)
PFAS$publication.year <- as.character(PFAS$publication.year)
PFAS[PFAS == ""] <- NA 

filteredPFAS <- PFAS %>%
                  filter(endpoint == "NOEC"&effect=="Mortality"& !is.na(number.of.doses)) %>%
                  select(c(publication.year, exposure.type, number.of.doses, species.common.name, cas.number,conc.mean))
                  
vars <- (names(filteredPFAS))

# Define the UI
ui <- pageWithSidebar(
  headerPanel(h1("PFAS NOEC values")),
  sidebarPanel(
    selectInput('xvar','Choose a variable to display on X-axis', vars)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)


# Define the server code
server <- function(input, output) { 
  print("server start")
  
output$plot1 <- renderPlot({
      print("renderPlot ran")
      ggplot(filteredPFAS)+
        geom_boxplot(aes(x=filteredPFAS[, input$xvar], y=log2(conc.mean), fill=filteredPFAS[, input$xvar]))+
        geom_point(aes(x=filteredPFAS[, input$xvar], y=log2(conc.mean)))+
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none")+
        labs(title="Mortality NOEC Concentrations by Selected Variable")+
        ylab("Log2 (LC50)")+
        xlab(input$xvar)+
        scale_fill_discrete_sequential("Viridis")
        
       
  })
  print("server end")
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)


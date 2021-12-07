library(shiny)

ui<-fluidPage(
  
  titlePanel("Tipos de Entrada"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput(inputId="text",
                label="1. textInput",
                value = "ENTRADA DE TEXTO"),
      
      radioButtons(inputId="radio",
                   label="2. radioButtons",
                   choices = c("item a", "item b")),
      
      selectInput(inputId="select", 
                  label="3. selectInput",
                  choices = c("ESCOLHA A","ESCOLHA B")),
      
      sliderInput(inputId="slider",
                  label="4. sliderInput",
                  min = 0, max=5, value = 2),
      
      checkboxInput(inputId="checkbox2",
                    label="5. checkboxInput"),
      
      checkboxGroupInput(inputId="checkbox",
                         label="6. checkboxGroupInput",
                         choices = c("item a", "item b")),
      

      numericInput(inputId = "numeric",
                   label="7. numericInput",
                   value=0),
      
      dateInput(inputId="date1",
                label="8. dateInput",
                value="27-02-2018", 
                format="dd/mm/yyyy"),
      
      dateRangeInput(inputId="date2",
                    label="9. dateRangeInput",
                    start="01-02-2018",
                    end  ="28-02-2018",
                    format="dd/mm/yyyy"
                    )

      
    ),
    
    mainPanel()
  )
  
)

server<-function(input,output){
  
}



shinyApp(ui, server)
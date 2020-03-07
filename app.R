library(shiny)
library(shinythemes)
setwd(here::here())
source(paste0(here::here(),"/prep.R"))

shinyApp(
    ui = fluidPage(includeCSS("www/styles.css"),
                   #tags$img(src = "logo.png", width="20%", align="left"),
        selectInput("name", h3("Type university's name"), choices = schools$name),
        downloadButton("report", "Generate custom presentation for similar entities")
    ),
    server = function(input, output) {
        output$report <- downloadHandler(
            # For PDF output, change this to "report.pdf"
            filename = "custom_presentation.pptx",
            content = function(file) {
                # Copy the report file to a temporary directory before processing it, in
                # case we don't have write permissions to the current working dir (which
                # can happen when deployed).
                tempReport <- file.path(tempdir(), "test.Rmd")
                file.copy("test.Rmd", tempReport, overwrite = TRUE)
                
                # Set up parameters to pass to Rmd document
                params <- list(name = input$name)
                
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                                
                )
            }
        )
    }
)

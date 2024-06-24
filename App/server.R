library(shiny)

# Option to allow big files like .fasta files
options(shiny.maxRequestSize = 1000*1024^2)

function(input, output, session) {
  
  observeEvent(input$generate_qc, {
    req(input$evidencefile, input$keysfile)
    output$output_text <- renderText("Working... Please Wait.")
    
    delay(100, {
      if (!input$qc_basic && !input$qc_ext) { 
        output$output_text <- renderText("No QC(s) selected")
        return()
      }
      
      zip_and_download <- function(folder_name) {
        zipfile <- paste0(folder_name, ".zip")
        zip(zipfile, files = folder_name)
        downloadHandler(
          filename = function() { zipfile },
          content = function(file) { file.copy(zipfile, file) },
          contentType = "application/zip"
        )
      }
      
      if (input$qc_basic) {
        artmsQualityControlEvidenceBasic(
          evidence_file = read.table(input$evidencefile$datapath, header = TRUE, sep = "\t"),
          keys_file = read.table(input$keysfile$datapath, header = TRUE, sep = "\t"),
          prot_exp = "APMS"
        )
        
        output$download_basic <- zip_and_download("qc_basic")
        output$download_ui_basic <- renderUI({
          downloadButton("download_basic", "Download Basic QC")
        })
      }
      
      if (input$qc_ext) {
        artmsQualityControlEvidenceExtended(
          evidence_file = read.table(input$evidencefile$datapath, header = TRUE, sep = "\t"),
          keys_file = read.table(input$keysfile$datapath, header = TRUE, sep = "\t"),
          plotPCA = input$perform_pca
        )
        output$download_extended <- zip_and_download("qc_extended")
        output$download_ui_extended <- renderUI({
          downloadButton("download_extended", "Download Extended QC")
        })
      }
      output$output_text <- renderText("Done.")
    })
  })
  
  observeEvent(input$generate_SAINT, {
    req(input$evidencefile, input$keysfile, input$ref)
    output$output_text2 <- renderText("Working... Please Wait.")
    
    delay(10, {
      if (input$msspc) {
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msspc",
          output_file = "msspc.txt"
        )
        output$download_ui2 <- renderUI({
          tagList(
            downloadButton("download_msspc_interactions", "Download msspc SAINT Interactions"),
            downloadButton("download_msspc_baits", "Download msspc SAINT Baits input file"),
            downloadButton("download_msspc_preys", "Download msspc SAINT Preys input file")
          )
        })
      }
      if (input$msint) {
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msint",
          output_file = "msint.txt"
        )
        output$download_ui2 <- renderUI({
          tagList(
            downloadButton("download_msint_interactions", "Download msint SAINT Interactions"),
            downloadButton("download_msint_baits", "Download msint SAINT Baits input file"),
            downloadButton("download_msint_preys", "Download msint SAINT Preys input file")
          )
        })
      }
      
      if (input$msint && input$msspc) {
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msspc",
          output_file = "msspc.txt"
        )
        
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msint",
          output_file = "msint.txt"
        )
        output$download_ui2 <- renderUI({
          tagList(
            downloadButton("download_msspc_interactions", "Download msspc SAINT Interactions"),
            downloadButton("download_msspc_baits", "Download msspc SAINT Baits input file"),
            downloadButton("download_msspc_preys", "Download msspc SAINT Preys input file"),
            downloadButton("download_msint_interactions", "Download msint SAINT Interactions"),
            downloadButton("download_msint_baits", "Download msint SAINT Baits input file"),
            downloadButton("download_msint_preys", "Download msint SAINT Preys input file")
          )
        })
      }
    })
  })
}
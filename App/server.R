library(shiny)

# Option to allow big files like .fasta files
options(shiny.maxRequestSize = 1000*1024^2)

# Install and load packages
if (!requireNamespace("BiocManager", quietly = TRUE))install.packages("BiocManager")
if (!requireNamespace("MSstats", quietly = TRUE)) BiocManager::install("MSstats",force = TRUE)
if (!requireNamespace("artMS", quietly = TRUE)) BiocManager::install("artMS",force = TRUE)
if (!requireNamespace("data.table", quietly = TRUE))install.packages("data.table")
if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) BiocManager::install("ComplexHeatmap")
if (!requireNamespace("ggplot2", quietly = TRUE))install.packages("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE))install.packages("readxl")
if (!requireNamespace("shinyjs", quietly = TRUE))install.packages("shinyjs")
if (!requireNamespace("seqinr", quietly = TRUE))install.packages("seqinr")

library(BiocManager)
library(artMS)
library(MSstats)
library (data.table)
library (ComplexHeatmap)
library (ggplot2)
library(readxl)
library(shinyjs)
library(seqinr)

function(input, output, session) {
  # Generate QC with button
  observeEvent(input$generate_qc, {
    req(input$evidencefile, input$keysfile)
    output$output_text <- renderText({"Working... Please Wait."}) # Message for user patience 
    
    delay(100, { # Delay to display message correctly 
      if (!input$qc_basic && !input$qc_ext) { 
        output$output_text <- renderText({"No QC(s) selected"})
        return()
      }
      
      # Function to zip and download QC folders
      zip_and_download <- function(folder_name) {
        zipfile <- paste0(folder_name, ".zip")
        zip(zipfile, files = folder_name)
        downloadHandler(
          filename = function() {
            zipfile
          },
          content = function(file) {
            file.copy(zipfile, file)
          },
          contentType = "application/zip"
        )
      }
      
      if (input$qc_basic) { # QC basic 
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
      
      if (input$qc_ext) { # QC extended
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
      
      output$output_text <- renderText({"Done."}) # End message
    })
  })
  
  observeEvent(input$generate_SAINT, { # Message for user patience 
    req(input$evidencefile, input$keysfile, input$ref)
    output$output_text2 <- renderText({"Working... Please Wait."})
    
    delay(10, { # Delay to display message correctly 
      if (input$msspc) { # Generate files with spectral count
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msspc",
          output_file = "msspc.txt"
        )
        output$download_ui2 <- renderUI({ # UI to download spectral count files
          tagList(
            downloadButton("download_msspc_interactions", "Download msspc SAINT Interactions"),
            downloadButton("download_msspc_baits", "Download msspc SAINT Baits input file"),
            downloadButton("download_msspc_preys", "Download msspc SAINT Preys input file")
          )
        })
      }
      
      if (input$msint) { # Generate files with intensity
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msint",
          output_file = "msint.txt"
        )
        output$download_ui2 <- renderUI({ # UI to download intensity files
          tagList(
            downloadButton("download_msint_interactions", "Download msint SAINT Interactions"),
            downloadButton("download_msint_baits", "Download msint SAINT Baits input file"),
            downloadButton("download_msint_preys", "Download msint SAINT Preys input file")
          )
        })
      }
      
      if (input$msint && input$msspc) { # Generate both with spectral count then intensity
        # Run with msspc
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msspc",
          output_file = "msspc.txt"
        )
        
        # Run with msint
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msint",
          output_file = "msint.txt"
        )
        output$download_ui2 <- renderUI({ # UI to download all files
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
      
      output$output_text2 <- renderText({ 
        "SAINTexpress files generated successfully." # End message
      }) 
    })
  })
  
  # Download handlers
  output$download_msspc_baits <- downloadHandler(
    filename = function() {
      "msspc-saint-baits.txt"
    },
    content = function(file) {
      file.copy("msspc-saint-baits.txt", file)
    }
  )
  
  output$download_msspc_preys <- downloadHandler(
    filename = function() {
      "msspc-saint-preys.txt"
    },
    content = function(file) {
      file.copy("msspc-saint-preys.txt", file)
    }
  )
  
  output$download_msspc_interactions <- downloadHandler(
    filename = function() {
      "msspc-saint-interactions.txt"
    },
    content = function(file) {
      file.copy("msspc-saint-interactions.txt", file)
    }
  )
  
  output$download_msint_baits <- downloadHandler(
    filename = function() {
      "msint-saint-baits.txt"
    },
    content = function(file) {
      file.copy("msint-saint-baits.txt", file)
    }
  )
  
  output$download_msint_preys <- downloadHandler(
    filename = function() {
      "msint-saint-preys.txt"
    },
    content = function(file) {
      file.copy("msint-saint-preys.txt", file)
    }
  )
  
  output$download_msint_interactions <- downloadHandler(
    filename = function() {
      "msint-saint-interactions.txt"
    },
    content = function(file) {
      file.copy("msint-saint-interactions.txt", file)
    }
  )
}

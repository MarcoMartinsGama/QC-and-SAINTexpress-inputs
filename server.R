library(shiny)
options(shiny.maxRequestSize = 1000*1024^2)
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
suppressPackageStartupMessages(library(ComplexHeatmap))

function(input, output, session) {
  observeEvent(input$generate_qc, {
    output$output_text <- renderText({"Working... Please Wait."})
    
    delay(100, {
      if (!input$qc_basic && !input$qc_ext) { 
        output$output_text <- renderText({"No QC(s) selected"})
        return()
      }
      
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
      
      if (input$qc_basic == TRUE && !input$qc_ext) {
        artmsQualityControlEvidenceBasic(
          evidence_file = read.table(input$evidencefile$datapath, header = TRUE, sep = "\t"),
          keys_file = read.table(input$keysfile$datapath, header = TRUE, sep = "\t"),
          prot_exp = "APMS"
        )
        output$output_text <- renderText({"Done."})
        output$download_basic <- zip_and_download("qc_basic")
        output$download_ui <- renderUI({
          downloadButton("download_basic", "Download Basic QC")
        })
      }
      
      if (input$qc_ext == TRUE && !input$qc_basic) {
        artmsQualityControlEvidenceExtended(
          evidence_file = read.table(input$evidencefile$datapath, header = TRUE, sep = "\t"),
          keys_file = read.table(input$keysfile$datapath, header = TRUE, sep = "\t"),
          plotPCA = FALSE
        )
        output$output_text <- renderText({"Done."})
        output$download_extended <- zip_and_download("qc_extended")
        output$download_ui <- renderUI({
          downloadButton("download_extended", "Download Extended QC")
        })
      }
      
      if (input$qc_basic == TRUE && input$qc_ext == TRUE) {
        output$output_text <- renderText({"Done."})
        artmsQualityControlEvidenceBasic(
          evidence_file = read.table(input$evidencefile$datapath, header = TRUE, sep = "\t"),
          keys_file = read.table(input$keysfile$datapath, header = TRUE, sep = "\t"),
          prot_exp = "APMS"
        )
        
        artmsQualityControlEvidenceExtended(
          evidence_file = read.table(input$evidencefile$datapath, header = TRUE, sep = "\t"),
          keys_file = read.table(input$keysfile$datapath, header = TRUE, sep = "\t"),
          plotPCA = FALSE
        )
        output$download_basic <- zip_and_download("qc_basic")
        output$download_extended <- zip_and_download("qc_extended")
        output$download_ui <- renderUI({
          tagList(
            downloadButton("download_basic", "Download Basic QC"),
            downloadButton("download_extended", "Download Extended QC")
          )
        })
      }
    })
  })
  
  observeEvent(input$generate_SAINT, {
    req(input$evidencefile, input$keysfile, input$ref)
    output$output_text2 <- renderText({"Working... Please Wait."})
    
    delay(10, {
      quant_variable <- c()
      if (input$msspc) quant_variable <- c(quant_variable, "msspc")
      if (input$msint) quant_variable <- c(quant_variable, "msint")
      
      if (length(quant_variable) == 2) {
        # Run with msspc
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msspc",
          output_file = "output.txt"
        )
        
        # Run with msint
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = "msint",
          output_file = "output2.txt"
        )
        
        # Merge interaction files
        o1<- fread("output-saint-interactions.txt")
        o2 <- fread("output2-saint-interactions.txt")
        output_interactions <- cbind(o1,o2[[4]])
        
        
        
        fwrite(output_interactions, "output-saint-interactions.txt", sep = "\t")
        
        
        output$output_text2 <- renderText({
          "SAINTexpress files generated and merged successfully."
        })
        
        output$download_ui2 <- renderUI({
          tagList(
            downloadButton("download_saint_interactions", "Download Merged Interactions"),
            downloadButton("download_saint_baits", "Download SAINT Baits input file"),
            downloadButton("download_saint_preys", "Download SAINT Preys input file")
          )
        })
      } else {
        artmsEvidenceToSaintExpress(
          evidence_file = input$evidencefile$datapath,
          keys_file = input$keysfile$datapath,
          ref_proteome_file = input$ref$datapath,
          quant_variable = quant_variable,
          output_file = "output.txt"
        )
        
        output$output_text2 <- renderText({
          "SAINTexpress files generated successfully."
        })
        
        output$download_ui2 <- renderUI({
          tagList(
            downloadButton("download_saint_interactions", "Download SAINT Interactions"),
            downloadButton("download_saint_baits", "Download SAINT Baits input file"),
            downloadButton("download_saint_preys", "Download SAINT Preys input file")
          )
        })
      }
    })
  })
  
  output$download_saint_baits <- downloadHandler(
    filename = function() {
      "output-saint-baits.txt"
    },
    content = function(file) {
      file.copy("output-saint-baits.txt", file)
    }
  )
  
  output$download_saint_preys <- downloadHandler(
    filename = function() {
      "output-saint-preys.txt"
    },
    content = function(file) {
      file.copy("output-saint-preys.txt", file)
    }
  )
  
  output$download_saint_interactions <- downloadHandler(
    filename = function() {
      "output-saint-interactions.txt"
    },
    content = function(file) {
      file.copy("output-saint-interactions.txt", file)
    }
  )
}


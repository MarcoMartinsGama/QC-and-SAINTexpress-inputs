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
library(BiocManager)
library(artMS)
library(MSstats)
library (data.table)
library (ComplexHeatmap)
library (ggplot2)
library(readxl)
library(shinyjs)
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
      
      # Basic QC
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
      
      # Ext QC
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
      
      # Both QC(s)
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
            downloadButton("download_extended", "Download Extended QC"))
        })
      }
    })
  })
}
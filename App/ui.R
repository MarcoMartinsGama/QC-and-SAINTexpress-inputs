library(shiny)
library(shinyjs)

# Define UI for application
fluidPage(
  useShinyjs(), # Include shinyjs
  titlePanel("QC and SAINT inputs generator"),
  sidebarLayout(
    sidebarPanel(
      # Upload necessary files
      fileInput("evidencefile", "Upload evidence.txt", accept = c("text/plain", ".txt")),
      fileInput("keysfile", "Upload keys.txt", accept = c("text/plain", ".txt")),
      fileInput("ref", "Upload Reference proteome", accept = ".fasta")
    ),
    mainPanel(
      # Check if the users want to do basic, extended or both QC
      checkboxInput("qc_basic", "QC_Basic", value = TRUE),
      checkboxInput("qc_ext", "QC_Extended", value = FALSE),
      conditionalPanel(
        condition = "input.qc_ext == true",
        checkboxInput("perform_pca", "Perform PCA (Might take a long time, be patient)", value = FALSE)
      ),
      
      # Button to generate QC and message confirming the work is in progress
      actionButton("generate_qc", "Generate QC(s)"),
      textOutput("output_text"),
      
      # UI for downloading QCs
      uiOutput("download_ui_basic"),
      uiOutput("download_ui_extended"),
      
      # Check boxes to generate input files for SAINTexpress in spectral count or intensity
      checkboxInput("msspc", "msspc", value = TRUE),
      checkboxInput("msint", "msint", value = FALSE),
      
      # Generate the files 
      actionButton("generate_SAINT", "Generate SAINTexpress input files"),
      textOutput("output_text2"),
      
      # Download the files
      uiOutput("download_ui2")
    )
  )
)

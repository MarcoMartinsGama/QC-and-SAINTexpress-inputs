library(shiny)
fluidPage(
  useShinyjs(),
  titlePanel("QC and SAINT inputs"),
  sidebarLayout(
    sidebarPanel(
      fileInput("evidencefile", "Upload evidence.txt", accept = c("text/plain", ".txt")),
      fileInput("keysfile", "Upload keys.txt", accept = c("text/plain", ".txt")),
      fileInput("ref", "Upload Reference proteome", accept = ".fasta")
    ),
    mainPanel(
      checkboxInput("qc_basic", "QC_Basic", value = TRUE),
      checkboxInput("qc_ext", "QC_Extended", value = FALSE),
      conditionalPanel(
        condition = "input.qc_ext == true",
        checkboxInput("perform_pca", "Perform PCA (Might take a long time, be patient)", value = FALSE)),
      actionButton("generate_qc", "Generate QC(s)"),
      textOutput("output_text"),
      uiOutput("download_ui_basic"),
      uiOutput("download_ui_extended"),
      checkboxInput("msspc", "msspc", value = TRUE),
      checkboxInput("msint", "msint", value = FALSE),
      actionButton("generate_SAINT", "Generate SAINTexpress input files"),
      textOutput("output_text2"),
      uiOutput("download_ui2")
    )
  )
)


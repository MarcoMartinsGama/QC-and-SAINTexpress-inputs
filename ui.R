library(shiny)
fluidPage(
  useShinyjs(),
  titlePanel("QC"),
  sidebarLayout(
    sidebarPanel(
      fileInput("evidencefile", "Input evidence.txt", accept = c("text/plain", ".txt")),
      fileInput("keysfile", "Input keys.txt", accept = c("text/plain", ".txt")),
      checkboxInput("qc_basic", "QC_Basic", value = TRUE),
      checkboxInput("qc_ext", "QC_Extended", value = FALSE),
      actionButton("generate_qc", "Generate QC(s)"),
      textOutput("output_text"),
      uiOutput("download_ui")
    ),
    mainPanel()
  ))
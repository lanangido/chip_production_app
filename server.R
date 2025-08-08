# server.R

server <- function(input, output, session) {
  # Buat koneksi database di awal sesi Shiny
  conn <- get_db_conn()
  # Pastikan koneksi terputus saat sesi Shiny berakhir
  onStop(function() {
    DBI::dbDisconnect(conn)
    message("Database connection closed.")
  })
  
  # Reactive value to store user role
  user_role <- reactiveVal(NULL)
  
  # Inisialisasi reactive_data dan muat data awal dari DB
  reactive_data <- reactiveValues(
    akun = read_and_convert_table(conn, "akun"),
    assembler = read_and_convert_table(conn, "assembler", c("tanggal_start", "tanggal_stop")),
    tester = read_and_convert_table(conn, "tester", c("tanggal_testing")),
    packager = read_and_convert_table(conn, "packager", c("tanggal_packaging"))
  )
  
  # Debugging: Print initial data load status within an observe block
  observe({
    print(paste("Initial data load - Akun rows:", nrow(reactive_data$akun)))
    print(paste("Initial data load - Assembler rows:", nrow(reactive_data$assembler)))
    print(paste("Initial data load - Tester rows:", nrow(reactive_data$tester)))
    print(paste("Initial data load - Packager rows:", nrow(reactive_data$packager)))
  })
  
  # Reactive value to store the start time of assembly process
  assembly_start_timestamp <- reactiveVal(NULL)
  # Reactive value to track if assembly process is currently running
  is_assembly_running <- reactiveVal(FALSE)
  
  # Observe login button click
  observeEvent(input$login_btn, {
    email <- input$email
    password <- input$password
    
    # Ensure reactive_data$akun is available before filtering
    req(reactive_data$akun) 
    user_info <- reactive_data$akun %>%
      filter(email == !!email & password == !!password)
    
    if (nrow(user_info) == 1) {
      user_role(user_info$role)
      print(paste("User logged in with role:", user_info$role)) # Debug print
      shinyjs::hide("email")
      shinyjs::hide("password")
      shinyjs::hide("login_btn")
      shinyjs::show("logout_btn")
      
      # Set session$userData$email_logged_in for admin_akun_server
      session$userData$email_logged_in <- email
      
    } else {
      showModal(modalDialog("Email atau Password salah! Silakan coba lagi.", easyClose = TRUE))
    }
  })
  
  # Observe logout button click
  observeEvent(input$logout_btn, {
    user_role(NULL)
    shinyjs::show("email")
    shinyjs::show("password")
    shinyjs::show("login_btn")
    shinyjs::hide("logout_btn")
    updateTextInput(session, "email", value = "")
    updateTextInput(session, "password", value = "")
    session$userData$email_logged_in <- NULL # Clear logged in email
    showModal(modalDialog("Anda telah berhasil logout.", easyClose = TRUE))
  })
  
  # Render sidebar menu based on user role
  output$sidebar <- renderMenu({
    req(user_role())
    if (user_role() == "Admin") {
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Download Report", tabName = "download", icon = icon("download")),
        menuItem("Admin Dashboard", tabName = "admin_dashboard", icon = icon("user-secret"),
                 menuSubItem("Kelola Data Barang", tabName = "admin_barang"),
                 menuSubItem("Kelola Data Akun", tabName = "admin_akun"),
                 menuSubItem("Analisis Data", tabName = "admin_analisis"))
      )
    } else {
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data Entry", tabName = "entry", icon = icon("edit")),
        menuItem("Download Report", tabName = "download", icon = icon("download"))
      )
    }
  })
  
  # Render main UI content based on user role
  output$main_ui <- renderUI({
    req(user_role())
    
    # Create ns functions for modules
    data_entry_ns_func <- NS(session$ns("data_entry"))
    admin_barang_ns_func <- NS(session$ns("admin_barang"))
    admin_akun_ns_func <- NS(session$ns("admin_akun"))
    admin_analisis_ns_func <- NS(session$ns("admin_analisis"))
    
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard Produksi Chip"),
              fluidRow(
                box(
                  title = "Filter Data", status = "primary", solidHeader = TRUE, width = 12,
                  dateRangeInput("filter_date", "Filter Tanggal (Tanggal Mulai Assembler, Tanggal Testing Tester, Tanggal Packaging Packager)",
                                 start = Sys.Date() - 30, end = Sys.Date(),
                                 format = "dd/mm/yyyy", separator = "to")
                ),
                box(
                  title = "Jumlah Produksi per Tahap", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("role_pie")
                )
              )
      ),
      # PENTING: Modul UI ini sudah mengembalikan 'tabItem', jadi panggil langsung.
      data_entry_ui(user_role, data_entry_ns_func),
      
      tabItem(tabName = "download",
              h2("Download Laporan Produksi"),
              fluidRow(
                box(
                  title = "Unduh Laporan PDF", status = "info", solidHeader = TRUE, width = 6,
                  helpText("Laporan ini akan berisi ringkasan data produksi berdasarkan filter tanggal di Dashboard."),
                  downloadButton("download_report", "Download Laporan PDF", class = "btn-primary")
                )
              )
      ),
      # PENTING: Modul UI ini juga sudah mengembalikan 'tabItem', jadi panggil langsung.
      admin_barang_ui(admin_barang_ns_func),
      admin_akun_ui(admin_akun_ns_func),
      admin_analisis_ui(admin_analisis_ns_func)
    )
  })
  
  # Call data entry server module using moduleServer syntax
  data_entry_server("data_entry", reactive_data, conn, assembly_start_timestamp, is_assembly_running)
  
  # Call admin server modules using moduleServer syntax
  admin_barang_server("admin_barang", reactive_data, conn)
  admin_akun_server("admin_akun", reactive_data, conn)
  admin_analisis_server("admin_analisis", reactive_data)
  
  # --- Dashboard Plot (remains in main server as it uses all reactive_data) ---
  output$role_pie <- renderPlotly({
    req(input$filter_date)
    start_date_filter <- input$filter_date[1]
    end_date_filter <- input$filter_date[2]
    
    # Ensure reactive data is available before plotting
    req(reactive_data$assembler, reactive_data$tester, reactive_data$packager)
    
    assembler_filtered <- reactive_data$assembler %>%
      filter(tanggal_start >= start_date_filter & tanggal_start <= end_date_filter)
    
    tester_filtered <- reactive_data$tester %>%
      filter(tanggal_testing >= start_date_filter & tanggal_testing <= end_date_filter)
    
    packager_filtered <- reactive_data$packager %>%
      filter(tanggal_packaging >= start_date_filter & tanggal_packaging <= end_date_filter)
    
    counts <- data.frame(
      Role = c("Assembler", "Tester", "Packager"),
      Jumlah = c(
        nrow(assembler_filtered),
        nrow(tester_filtered),
        nrow(packager_filtered)
      )
    )
    
    plot_ly(counts, labels = ~Role, values = ~Jumlah, type = 'pie',
            textposition = 'inside', textinfo = 'percent+label',
            marker = list(colors = c('#1f77b4', '#ff7f0e', '#2ca02c'),
                          line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = 'Jumlah Produksi per Tahap',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # FIX: Robust download handler for PDF report
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Laporan_Produksi_Chip_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Show a progress indicator to the user
      withProgress(message = 'Membuat laporan PDF...', value = 0, {
        
        tryCatch({
          # Ensure the report template exists
          report_path <- "reports/report.Rmd"
          if (!file.exists(report_path)) {
            stop("File template laporan 'reports/report.Rmd' tidak ditemukan.")
          }
          
          # Copy the Rmd file to a temporary directory to avoid issues
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy(report_path, tempReport, overwrite = TRUE)
          
          incProgress(0.2, detail = "Menyiapkan data...")
          
          # Set up parameters to pass to the Rmd document
          params <- list(
            assembler_data = reactive_data$assembler,
            tester_data = reactive_data$tester,
            packager_data = reactive_data$packager,
            filter_start_date = input$filter_date[1],
            filter_end_date = input$filter_date[2]
          )
          
          incProgress(0.5, detail = "Merender PDF...")
          
          # Render the report with explicit format to prevent fallback
          rmarkdown::render(tempReport, 
                            output_file = file,
                            output_format = "pdf_document",
                            params = params,
                            envir = new.env(parent = globalenv()))
          
          incProgress(1, detail = "Selesai!")
          
        }, error = function(e) {
          # If an error occurs, show a modal dialog to the user with clear instructions
          showModal(modalDialog(
            title = "Gagal Membuat Laporan PDF",
            HTML(paste0(
              "<p>Gagal membuat laporan PDF. <b>File yang diunduh mungkin berupa file HTML dengan pesan error.</b></p>",
              "<p>Kesalahan ini hampir selalu disebabkan oleh LaTeX yang tidak terinstal atau tidak dapat ditemukan oleh R.</p>",
              "<p><b>Solusi:</b> Harap hubungi administrator sistem untuk memastikan bahwa TinyTeX telah diinstal dengan benar dengan menjalankan <code>tinytex::install_tinytex()</code> di konsol R pada server.</p>",
              "<hr>",
              "<strong>Pesan Error Teknis:</strong><br>",
              "<pre style='font-size: 12px; white-space: pre-wrap; word-wrap: break-word;'>", htmltools::htmlEscape(e$message), "</pre>"
            )),
            easyClose = TRUE
          ))
          # Return NULL to prevent a corrupted download
          return(NULL)
        })
      })
    }
  )
}
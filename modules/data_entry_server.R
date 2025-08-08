# modules/data_entry_server.R

# Definisi modul server menggunakan moduleServer
data_entry_server <- function(id, reactive_data, conn, assembly_start_timestamp, is_assembly_running) {
  moduleServer(id, function(input, output, session) {
    
    # Namespace untuk modul ini
    ns <- session$ns
    
    # Reactive values untuk menyimpan konten barcode/QR code sementara
    rv <- reactiveValues(
      qrcode_info = list(), # Mengubah ini menjadi daftar untuk menyimpan info QR code dari 1 atau 2 paket
      zpl_code = NULL,      # Menyimpan ZPL code yang dihasilkan untuk barcode
      last_no_produksi = NULL # Menyimpan No Produksi terakhir untuk ditampilkan
    )
    
    # Output debug information about reactive_data$assembler rows
    output$debug_output <- renderText({
      paste("Reactive data assembler rows:", nrow(reactive_data$assembler))
    })
    
    # --- Assembler Start/Stop Logic ---
    
    # Initial state for buttons
    observe({
      print("data_entry_server: Initial button state observer triggered.") # Debug print
      shinyjs::disable(ns("stop_assembly"))
      shinyjs::enable(ns("start_assembly"))
    })
    
    # Handle Start Assembly button click
    observeEvent(input$start_assembly, {
      print("Start Assembly button clicked!") # Debug print
      assembly_start_timestamp(Sys.time())
      is_assembly_running(TRUE)
      shinyjs::disable(ns("start_assembly"))
      shinyjs::enable(ns("stop_assembly"))
      output$assembly_status <- renderText({
        paste("Proses dimulai pada:", format(assembly_start_timestamp(), "%Y-%m-%d %H:%M:%S"))
      })
      # Reset barcode content when new assembly starts
      rv$last_no_produksi <- NULL 
      rv$zpl_code <- NULL
    })
    
    # Handle Stop Assembly button click and data submission
    observeEvent(input$stop_assembly, {
      print("Stop Assembly button clicked!") # Debug print
      req(assembly_start_timestamp())
      req(input$pic)
      
      stop_timestamp <- Sys.time()
      
      start_date_only <- as.Date(assembly_start_timestamp())
      stop_date_only <- as.Date(stop_timestamp)
      
      # Kunci untuk reset bulanan: Membuat sufiks unik untuk setiap bulan/tahun (misal: "0825")
      current_month_year_suffix <- format(start_date_only, "%m%y")
      
      # Filter entri yang ada HANYA untuk bulan dan tahun saat ini.
      assembler_current_period <- reactive_data$assembler %>%
        filter(format(tanggal_start, "%m%y") == current_month_year_suffix)
      
      # Jika tidak ada entri untuk bulan/tahun ini, urutan dimulai dari 1.
      if (nrow(assembler_current_period) == 0) {
        next_sequence <- 1
      } else {
        # Jika tidak, temukan nomor urut tertinggi yang ada di periode ini dan tambahkan 1.
        numeric_prefixes <- assembler_current_period$no_produksi %>%
          substr(1, 4) %>%
          as.numeric()
        
        numeric_prefixes <- numeric_prefixes[!is.na(numeric_prefixes)]
        
        if (length(numeric_prefixes) == 0) {
          next_sequence <- 1
        } else {
          max_sequence <- max(numeric_prefixes)
          next_sequence <- max_sequence + 1
        }
      }
      
      formatted_sequence <- sprintf("%04d", next_sequence)
      
      generated_no_produksi <- paste0(formatted_sequence, current_month_year_suffix)
      
      if (generated_no_produksi %in% reactive_data$assembler$no_produksi) {
        showModal(modalDialog(
          paste0("No Produksi '", generated_no_produksi, "' sudah ada. Terjadi duplikasi dalam urutan otomatis."),
          easyClose = TRUE
        ))
        assembly_start_timestamp(NULL)
        is_assembly_running(FALSE)
        shinyjs::enable(ns("start_assembly"))
        shinyjs::disable(ns("stop_assembly"))
        output$assembly_status <- renderText({ "Proses belum dimulai." })
        return()
      }
      
      new_entry <- data.frame(
        no_produksi = generated_no_produksi,
        tanggal_start = start_date_only,
        tanggal_stop = stop_date_only,
        PIC = input$pic
      )
      
      print("Attempting to append new assembler entry to DB.") # Debug print
      DBI::dbAppendTable(conn, "assembler", new_entry)
      print("New assembler entry appended to DB.") # Debug print
      
      # --- Baca ulang data dari DB untuk memastikan reaktivitas ---
      reactive_data$assembler <- read_and_convert_table(conn, "assembler", c("tanggal_start", "tanggal_stop"))
      print(paste("Reactive data assembler rows after update:", nrow(reactive_data$assembler))) # Debug print
      
      # Reset state and update UI
      assembly_start_timestamp(NULL)
      is_assembly_running(FALSE)
      shinyjs::enable(ns("start_assembly"))
      shinyjs::disable(ns("stop_assembly"))
      updateTextInput(session, "pic", value = "")
      output$assembly_status <- renderText({ "Proses belum dimulai." })
      
      # --- ZPL Generation ---
      # Asumsi printer 203 DPI (8 dots/mm)
      # Ukuran label: 31.35mm lebar (~251 dots), 22.86mm tinggi (~183 dots)
      zpl_code <- paste0(
        "^XA",
        "^PW251", # Print Width (lebar label)
        "^LL183", # Label Length (tinggi label)
        "^LH0,0",  # Label Home (posisi 0,0)
        "^FO20,40^BY2", # Field Origin (posisi), Module Width
        "^BCN,80,Y,N,N", # Code 128, Normal, tinggi 80 dots, tampilkan teks
        "^FD", generated_no_produksi, "^FS", # Field Data
        "^XZ"
      )
      rv$zpl_code <- zpl_code # Simpan ZPL di reactive value
      rv$last_no_produksi <- generated_no_produksi # Simpan no produksi untuk UI
      
      shinyjs::delay(200, {
        showModal(modalDialog(paste0("Data Assembler untuk No Produksi '", generated_no_produksi, "' berhasil disimpan! Label ZPL siap dicetak."), easyClose = TRUE))
      })
    })
    
    # Render UI to show the generated production number
    output$assembly_result_ui <- renderUI({
      req(rv$last_no_produksi)
      tagList(
        hr(),
        h4("Produksi Selesai:"),
        p(strong("No Produksi:"), code(rv$last_no_produksi))
      )
    })
    
    # Display assembly status
    output$assembly_status <- renderText({
      if (is_assembly_running()) {
        paste("Proses dimulai pada:", format(assembly_start_timestamp(), "%Y-%m-%d %H:%M:%S"))
      } else {
        "Proses belum dimulai."
      }
    })
    
    # --- ZPL Barcode Printing Logic ---
    observeEvent(input$print_barcode_zpl, {
      req(rv$zpl_code)
      
      showModal(modalDialog(
        title = "Cetak Label ZPL",
        tagList(
          p("Gunakan ZPL di bawah ini atau cetak langsung ke printer jaringan."),
          tags$textarea(rv$zpl_code, rows = 8, style = "width: 100%; font-family: monospace;"),
          hr(),
          h4("Cetak Langsung ke Printer Zebra"),
          textInput(ns("printer_ip"), "Alamat IP Printer Zebra", placeholder = "e.g., 192.168.1.100"),
          actionButton(ns("send_zpl_to_printer"), "Kirim ke Printer", class = "btn-success"),
          # Placeholder for status messages
          br(),br(),
          uiOutput(ns("print_status_ui"))
        ),
        footer = modalButton("Tutup"),
        easyClose = TRUE
      ))
    })
    
    # Observer for the "Send to Printer" button inside the modal
    observeEvent(input$send_zpl_to_printer, {
      req(input$printer_ip, rv$zpl_code)
      
      # Basic IP validation
      if (!grepl("^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$", input$printer_ip)) {
        output$print_status_ui <- renderUI({ tags$p("Format Alamat IP tidak valid.", style = "color:red;") })
        return()
      }
      
      output$print_status_ui <- renderUI({ tags$p("Mengirim data...", style = "color:blue;") })
      
      tryCatch({
        # Open, write, and close the connection
        con <- socketConnection(host = input$printer_ip, port = 9100, open = "w", timeout = 5)
        writeLines(rv$zpl_code, con)
        close(con)
        
        output$print_status_ui <- renderUI({ tags$p("Data ZPL berhasil dikirim ke printer!", style = "color:green;") })
        
      }, error = function(e) {
        
        error_message <- HTML(paste(
          "<p style='color:red;'>Gagal terhubung ke printer. Pastikan:</p>",
          "<ul>",
          "<li>Alamat IP benar dan printer menyala.</li>",
          "<li>Printer terhubung ke jaringan yang sama dengan server Shiny.</li>",
          "<li>Port 9100 tidak diblokir oleh firewall.</li>",
          "</ul>",
          "<p><strong>Detail Error:</strong> ", htmltools::htmlEscape(e$message), "</p>"
        ))
        output$print_status_ui <- renderUI({ error_message })
      })
    })
    
    # --- Data Submission Logic for Tester and Packager (No changes here) ---
    
    # Reactive expression for items available for testing (not yet tested)
    available_for_testing <- reactive({
      all_assembler_nos <- reactive_data$assembler$no_produksi
      already_tested_nos <- reactive_data$tester$no_produksi
      not_yet_tested_nos <- all_assembler_nos[!all_assembler_nos %in% already_tested_nos]
      print(paste("Available for testing:", paste(not_yet_tested_nos, collapse = ", "))) # Debug print
      return(not_yet_tested_nos)
    })
    
    # ObserveEvent for submit_tester:
    observeEvent(input$test_no, {
      # Update test_no choices when reactive_data$assembler or reactive_data$tester changes
      updateSelectInput(session, "test_no", choices = available_for_testing())
    }, ignoreNULL = FALSE, ignoreInit = TRUE) # Update on change, but not on initial load
    
    observeEvent(input$submit_tester, {
      print("Submit Tester button clicked!") # Debug print
      req(input$test_no, input$test_date, input$test_pic, input$status)
      
      if (input$test_no %in% reactive_data$tester$no_produksi) {
        showModal(modalDialog("No Produksi ini sudah di-test. Mohon pilih yang lain atau update data yang sudah ada.", easyClose = TRUE))
        return()
      }
      
      new_entry <- data.frame(
        no_produksi = input$test_no,
        tanggal_testing = input$test_date,
        PIC = input$test_pic,
        status = input$status
      )
      
      print("Attempting to append new tester entry to DB.") # Debug print
      DBI::dbAppendTable(conn, "tester", new_entry)
      print("New tester entry appended to DB.") # Debug print
      
      reactive_data$tester <- read_and_convert_table(conn, "tester", c("tanggal_testing"))
      print(paste("Reactive data tester rows after update:", nrow(reactive_data$tester))) # Debug print
      
      # Clear inputs after submission and update choices *before* showing the modal
      updateSelectInput(session, "test_no", choices = available_for_testing(), selected = character(0))
      updateDateInput(session, "test_date", value = Sys.Date())
      updateTextInput(session, "test_pic", value = "")
      updateSelectInput(session, "status", selected = "Lolos")
      
      shinyjs::delay(200, {
        showModal(modalDialog("Data Tester berhasil disimpan!", easyClose = TRUE))
      })
    })
    
    # FIX: Corrected available_for_packaging logic
    available_for_packaging <- reactive({
      # Get items that passed testing and were NOT rejected
      passed_testing <- reactive_data$tester %>%
        filter(status == "Lolos")
      
      # Get all items that have ever been included in a package
      packaged_items_list <- reactive_data$packager$items_in_package
      
      already_packaged_nos <- character(0)
      if (length(packaged_items_list) > 0 && !all(is.na(packaged_items_list))) {
        # Use the correct `[[1]]` syntax to extract from strsplit's list output
        already_packaged_nos <- unlist(lapply(packaged_items_list, function(x) trimws(strsplit(x, ",")[[1]])))
      }
      
      # Filter the passed items to exclude those already packaged
      available_items <- passed_testing %>%
        filter(!no_produksi %in% already_packaged_nos) %>%
        dplyr::select(no_produksi, tanggal_testing, PIC_testing = PIC) # Select desired columns for display
      
      print(paste("Available for packaging:", paste(available_items$no_produksi, collapse = ", ")))
      return(available_items)
    })
    
    # Observe changes in available_for_packaging to update pickerInput choices
    observe({
      all_available_for_packaging <- available_for_packaging()$no_produksi
      current_selection <- input$pack_nos 
      updatePickerInput(session, "pack_nos", 
                        choices = all_available_for_packaging, 
                        selected = current_selection)
    })
    
    # Simplified process_package function that takes a pre-calculated sequence number
    process_package <- function(selected_nos_subset, package_sequence, conn, input_data) {
      current_date_package <- format(Sys.Date(), "%Y%m%d")
      generated_no_package <- paste0("PKG-", current_date_package, "-", sprintf("%03d", package_sequence))
      
      items_in_package_string <- paste(sort(selected_nos_subset), collapse = ",")
      
      new_entry <- data.frame(
        no_package = generated_no_package,
        tanggal_packaging = input_data$pack_date,
        PIC = input_data$pack_pic,
        items_in_package = items_in_package_string,
        stringsAsFactors = FALSE
      )
      
      print(paste("Appending package:", generated_no_package)) # Debug print
      DBI::dbAppendTable(conn, "packager", new_entry)
      
      qrcode_data_for_package <- paste0(
        "Kode Paket: ", generated_no_package, "\n",
        "Tanggal Packaging: ", format(input_data$pack_date, "%d/%m/%Y"), "\n",
        "Items: ", items_in_package_string
      )
      
      # Robust ZPL generation using Hex codes for newlines
      zpl_qr_data <- gsub("\n", "_0A", qrcode_data_for_package)
      zpl_items_string <- gsub(",", ", ", items_in_package_string)
      
      zpl_string <- paste0(
        "^XA^CI28", # Start, set UTF-8
        "^FO40,35^BQN,2,6^FH^FDMA,", zpl_qr_data, "^FS", # QR Code with Hex data
        "^FO300,60^A0N,35,35^FDPackage: ", generated_no_package, "^FS",
        "^FO300,110^A0N,28,28^FB500,5,0,L,0^FDContains: ", zpl_items_string, "^FS",
        "^XZ" # End
      )
      
      return(list(
        message = paste0("Paket '", generated_no_package, "' berhasil dibuat dengan barang: ", items_in_package_string),
        qrcode_data = qrcode_data_for_package,
        package_no = generated_no_package,
        zpl_string = zpl_string
      ))
    }
    
    observeEvent(input$submit_package, {
      print("Submit Package button clicked!") # Debug print
      req(input$pack_nos, input$pack_date, input$pack_pic)
      
      num_selected_items <- length(input$pack_nos)
      
      if (!(num_selected_items == 5 || num_selected_items == 10)) {
        showModal(modalDialog("Mohon pilih tepat 5 atau 10 barang untuk dikemas dalam satu paket.", easyClose = TRUE))
        return()
      }
      
      rv$qrcode_info <- list() 
      message_text_final <- ""
      
      # Centralized package sequence generation
      current_date_package <- format(Sys.Date(), "%Y%m%d")
      existing_packages_today <- reactive_data$packager %>%
        filter(grepl(paste0("^PKG-", current_date_package), no_package))
      
      if (nrow(existing_packages_today) == 0) {
        next_sequence <- 1
      } else {
        max_seq <- existing_packages_today$no_package %>%
          gsub(paste0("PKG-", current_date_package, "-"), "", .) %>%
          as.numeric() %>%
          max(na.rm = TRUE)
        next_sequence <- if (is.finite(max_seq)) max_seq + 1 else 1
      }
      
      if (num_selected_items == 5) {
        result <- process_package(input$pack_nos, next_sequence, conn, input)
        message_text_final <- result$message
        rv$qrcode_info <- list(list(data = result$qrcode_data, package_no = result$package_no, zpl_string = result$zpl_string))
      } else if (num_selected_items == 10) {
        sorted_pack_nos <- sort(input$pack_nos)
        package1_nos <- sorted_pack_nos[1:5]
        package2_nos <- sorted_pack_nos[6:10]
        
        # Pass unique, incrementing sequence numbers to each function call
        result1 <- process_package(package1_nos, next_sequence, conn, input)
        result2 <- process_package(package2_nos, next_sequence + 1, conn, input)
        
        message_text_final <- paste0(result1$message, "\n", result2$message)
        
        rv$qrcode_info <- list(
          list(data = result1$qrcode_data, package_no = result1$package_no, zpl_string = result1$zpl_string),
          list(data = result2$qrcode_data, package_no = result2$package_no, zpl_string = result2$zpl_string)
        )
      }
      
      reactive_data$packager <- read_and_convert_table(conn, "packager", c("tanggal_packaging"))
      print(paste("Reactive data packager rows after update:", nrow(reactive_data$packager))) # Debug print
      
      updatePickerInput(session, "pack_nos", choices = available_for_packaging()$no_produksi, selected = character(0))
      updateDateInput(session, "pack_date", value = Sys.Date())
      updateTextInput(session, "pack_pic", value = "")
      
      shinyjs::delay(200, {
        showModal(modalDialog(HTML(gsub("\n", "<br>", message_text_final)), easyClose = TRUE))
      })
    })
    
    # --- Dynamic QR Code Display and Download for Packager ---
    output$qrcode_output_area <- renderUI({
      req(length(rv$qrcode_info) > 0) # Pastikan ada data QR code
      
      qr_output_list <- lapply(seq_along(rv$qrcode_info), function(i) {
        qr_data <- rv$qrcode_info[[i]]$data
        qr_package_no <- rv$qrcode_info[[i]]$package_no
        zpl_string_for_download <- rv$qrcode_info[[i]]$zpl_string # Ambil ZPL string
        
        image_id <- paste0("qr_image_", i)
        download_png_id <- paste0("download_qr_png_", i)
        download_zpl_id <- paste0("download_qr_zpl_", i)
        print_id <- paste0("print_qr_", i)
        
        output[[image_id]] <- renderImage({
          outfile <- tempfile(fileext = ".png")
          qr_matrix <- qrcode::qr_code(qr_data)
          png(outfile, width = 200, height = 200, units = "px", res = 72)
          par(mar = c(0, 0, 0, 0))
          image(qr_matrix, col = c("white", "black"), asp = 1, axes = FALSE)
          dev.off()
          
          list(src = outfile,
               contentType = "image/png",
               width = 200,
               height = 200,
               alt = paste("QR Code Paket", qr_package_no))
        }, deleteFile = TRUE)
        
        output[[download_png_id]] <- downloadHandler(
          filename = function() {
            paste0("qrcode_", qr_package_no, ".png")
          },
          content = function(file) {
            qr_matrix <- qrcode::qr_code(qr_data)
            png(file, width = 300, height = 300, units = "px", res = 72)
            par(mar = c(0, 0, 0, 0))
            image(qr_matrix, col = c("white", "black"), asp = 1, axes = FALSE)
            dev.off()
          }
        )
        
        output[[download_zpl_id]] <- downloadHandler(
          filename = function() {
            paste0("label_zpl_", qr_package_no, ".txt")
          },
          content = function(file) {
            writeLines(zpl_string_for_download, file)
          }
        )
        
        observeEvent(input[[print_id]], {
          req(qr_data)
          showModal(modalDialog(
            title = "Cetak Label ZPL",
            HTML(paste0(
              "<p>Untuk mencetak label fisik dengan printer Zebra:</p>",
              "<ol>",
              "<li>Klik tombol <strong>'Unduh ZPL'</strong> di bawah ini untuk mengunduh file ZPL.</li>",
              "<li>Buka <strong>Zebra Setup Utilities</strong> (atau aplikasi pengiriman ZPL lainnya) di komputer yang terhubung ke printer Zebra Anda.</li>",
              "<li>Muat atau tempelkan isi file ZPL yang diunduh ke aplikasi tersebut dan kirimkan ke printer.</li>",
              "</ol>",
              "<p><strong>Isi ZPL untuk Paket ", qr_package_no, ":</strong></p>",
              "<pre style='background-color:#f0f0f0; padding:10px; border-radius:5px; overflow-x:auto;'>",
              htmltools::htmlEscape(zpl_string_for_download),
              "</pre>"
            )),
            easyClose = TRUE,
            footer = tagList(
              downloadButton(ns(download_zpl_id), paste("Unduh ZPL untuk", qr_package_no), class = "btn-primary")
            )
          ))
        }, ignoreInit = TRUE, once = FALSE)
        
        fluidRow(
          box(
            title = paste("QR Code Paket", qr_package_no), status = "info", solidHeader = TRUE, width = 12,
            imageOutput(ns(image_id)),
            fluidRow(
              column(4, downloadButton(ns(download_png_id), paste("Unduh PNG", qr_package_no), style = "width:100%; margin-top: 10px;")),
              column(4, downloadButton(ns(download_zpl_id), paste("Unduh ZPL", qr_package_no), style = "width:100%; margin-top: 10px;")),
              column(4, actionButton(ns(print_id), paste("Cetak ZPL", qr_package_no), class = "btn-info", style = "width:100%; margin-top: 10px;"))
            )
          )
        )
      })
      
      do.call(tagList, qr_output_list)
    })
    
    # --- Data Table Outputs (for regular users) ---
    
    output$assembler_table <- DT::renderDataTable({
      print("Rendering assembler table. Data head:")
      print(head(reactive_data$assembler))
      reactive_data$assembler
    }, options = list(pageLength = 5, scrollX = TRUE))
    
    output$tester_table <- DT::renderDataTable({
      reactive_data$assembler %>%
        filter(!no_produksi %in% reactive_data$tester$no_produksi)
    }, options = list(pageLength = 5, scrollX = TRUE))
    
    output$packager_available_table <- DT::renderDataTable({
      available_for_packaging()
    }, options = list(pageLength = 5, scrollX = TRUE))
    
    output$packages_created_table <- DT::renderDataTable({
      reactive_data$packager %>%
        mutate(
          tanggal_packaging = format(tanggal_packaging, "%d/%m/%Y")
        )
    }, options = list(pageLength = 5, scrollX = TRUE))
  })
}
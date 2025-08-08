# modules/admin_barang_server.R

admin_barang_server <- function(id, reactive_data, conn) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Namespace untuk modul ini
    
    # Render Assembler table for CRUD
    output$admin_assembler_crud_table <- DT::renderDataTable({
      reactive_data$assembler
    }, editable = TRUE, options = list(pageLength = 10, scrollX = TRUE), selection = 'single') # Aktifkan pemilihan baris
    
    # Observe cell edits for Assembler table
    observeEvent(input$admin_assembler_crud_table_cell_edit, {
      info <- input$admin_assembler_crud_table_cell_edit
      row <- info$row
      col <- info$col
      value <- info$value
      colname <- colnames(reactive_data$assembler)[col]
      
      pk_value <- reactive_data$assembler$no_produksi[row]
      
      if (colname %in% c("tanggal_start", "tanggal_stop")) {
        value <- as.Date(value)
      }
      
      reactive_data$assembler[row, col] <- value
      
      sql_query <- sprintf("UPDATE assembler SET %s = %s WHERE no_produksi = %s",
                           DBI::dbQuoteIdentifier(conn, colname),
                           DBI::dbQuoteLiteral(conn, value),
                           DBI::dbQuoteLiteral(conn, pk_value))
      
      tryCatch({
        DBI::dbExecute(conn, sql_query)
        showNotification(paste("Assembler data updated:", pk_value), type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating Assembler data:", e$message), type = "error")
        reactive_data$assembler <- read_and_convert_table(conn, "assembler", c("tanggal_start", "tanggal_stop"))
      })
    })
    
    # Add new Assembler data
    observeEvent(input$admin_add_assembler, {
      req(input$admin_assembler_no_produksi, input$admin_assembler_tanggal_start,
          input$admin_assembler_tanggal_stop, input$admin_assembler_pic)
      
      new_entry <- data.frame(
        no_produksi = input$admin_assembler_no_produksi,
        tanggal_start = input$admin_assembler_tanggal_start,
        tanggal_stop = input$admin_assembler_tanggal_stop,
        PIC = input$admin_assembler_pic,
        stringsAsFactors = FALSE
      )
      
      tryCatch({
        DBI::dbAppendTable(conn, "assembler", new_entry)
        reactive_data$assembler <- read_and_convert_table(conn, "assembler", c("tanggal_start", "tanggal_stop"))
        showNotification("Data Assembler baru berhasil ditambahkan!", type = "message")
        updateTextInput(session, "admin_assembler_no_produksi", value = "")
        updateDateInput(session, "admin_assembler_tanggal_start", value = Sys.Date())
        updateDateInput(session, "admin_assembler_tanggal_stop", value = Sys.Date())
        updateTextInput(session, "admin_assembler_pic", value = "")
      }, error = function(e) {
        showNotification(paste("Error menambahkan data Assembler:", e$message), type = "error")
      })
    })
    
    # Delete Assembler data
    observeEvent(input$admin_delete_assembler, {
      selected_row <- input$admin_assembler_crud_table_rows_selected
      if (length(selected_row) > 0) {
        no_produksi_to_delete <- reactive_data$assembler$no_produksi[selected_row]
        
        # Pastikan tidak ada data terkait di tabel tester atau packager
        # Ini adalah logika bisnis penting untuk mencegah pelanggaran integritas referensial
        if (no_produksi_to_delete %in% reactive_data$tester$no_produksi) {
          showModal(modalDialog(
            "Tidak dapat menghapus data Assembler. Terdapat data terkait di tabel Tester.",
            easyClose = TRUE
          ))
          return()
        }
        
        sql_query <- sprintf("DELETE FROM assembler WHERE no_produksi = %s",
                             DBI::dbQuoteLiteral(conn, no_produksi_to_delete))
        
        tryCatch({
          DBI::dbExecute(conn, sql_query)
          reactive_data$assembler <- read_and_convert_table(conn, "assembler", c("tanggal_start", "tanggal_stop"))
          showNotification(paste("Data Assembler", no_produksi_to_delete, "berhasil dihapus!"), type = "message")
        }, error = function(e) {
          showNotification(paste("Error menghapus data Assembler:", e$message), type = "error")
        })
      } else {
        showModal(modalDialog("Pilih baris yang ingin dihapus terlebih dahulu.", easyClose = TRUE))
      }
    })
    
    
    # Render Tester table for CRUD
    output$admin_tester_crud_table <- DT::renderDataTable({
      reactive_data$tester
    }, editable = TRUE, options = list(pageLength = 10, scrollX = TRUE), selection = 'single') # Aktifkan pemilihan baris
    
    # Observe cell edits for Tester table
    observeEvent(input$admin_tester_crud_table_cell_edit, {
      info <- input$admin_tester_crud_table_cell_edit
      row <- info$row
      col <- info$col
      value <- info$value
      colname <- colnames(reactive_data$tester)[col]
      
      pk_value <- reactive_data$tester$no_produksi[row]
      
      if (colname == "tanggal_testing") {
        value <- as.Date(value)
      }
      
      reactive_data$tester[row, col] <- value
      
      sql_query <- sprintf("UPDATE tester SET %s = %s WHERE no_produksi = %s",
                           DBI::dbQuoteIdentifier(conn, colname),
                           DBI::dbQuoteLiteral(conn, value),
                           DBI::dbQuoteLiteral(conn, pk_value))
      
      tryCatch({
        DBI::dbExecute(conn, sql_query)
        showNotification(paste("Tester data updated:", pk_value), type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating Tester data:", e$message), type = "error")
        reactive_data$tester <- read_and_convert_table(conn, "tester", c("tanggal_testing"))
      })
    })
    
    # Add new Tester data
    observeEvent(input$admin_add_tester, {
      req(input$admin_tester_no_produksi, input$admin_tester_tanggal_testing,
          input$admin_tester_pic, input$admin_tester_status)
      
      new_entry <- data.frame(
        no_produksi = input$admin_tester_no_produksi,
        tanggal_testing = input$admin_tester_tanggal_testing,
        PIC = input$admin_tester_pic,
        status = input$admin_tester_status,
        stringsAsFactors = FALSE
      )
      
      tryCatch({
        DBI::dbAppendTable(conn, "tester", new_entry)
        reactive_data$tester <- read_and_convert_table(conn, "tester", c("tanggal_testing"))
        showNotification("Data Tester baru berhasil ditambahkan!", type = "message")
        updateTextInput(session, "admin_tester_no_produksi", value = "")
        updateDateInput(session, "admin_tester_tanggal_testing", value = Sys.Date())
        updateTextInput(session, "admin_tester_pic", value = "")
        updateSelectInput(session, "admin_tester_status", selected = "Lolos")
      }, error = function(e) {
        showNotification(paste("Error menambahkan data Tester:", e$message), type = "error")
      })
    })
    
    # Delete Tester data
    observeEvent(input$admin_delete_tester, {
      selected_row <- input$admin_tester_crud_table_rows_selected
      if (length(selected_row) > 0) {
        no_produksi_to_delete <- reactive_data$tester$no_produksi[selected_row]
        
        # Pastikan tidak ada data terkait di tabel packager
        if (no_produksi_to_delete %in% unlist(strsplit(reactive_data$packager$items_in_package, ","))) {
          showModal(modalDialog(
            "Tidak dapat menghapus data Tester. Terdapat data terkait di tabel Packager.",
            easyClose = TRUE
          ))
          return()
        }
        
        sql_query <- sprintf("DELETE FROM tester WHERE no_produksi = %s",
                             DBI::dbQuoteLiteral(conn, no_produksi_to_delete))
        
        tryCatch({
          DBI::dbExecute(conn, sql_query)
          reactive_data$tester <- read_and_convert_table(conn, "tester", c("tanggal_testing"))
          showNotification(paste("Data Tester", no_produksi_to_delete, "berhasil dihapus!"), type = "message")
        }, error = function(e) {
          showNotification(paste("Error menghapus data Tester:", e$message), type = "error")
        })
      } else {
        showModal(modalDialog("Pilih baris yang ingin dihapus terlebih dahulu.", easyClose = TRUE))
      }
    })
    
    # Render Packager table for CRUD
    output$admin_packager_crud_table <- DT::renderDataTable({
      reactive_data$packager
    }, editable = TRUE, options = list(pageLength = 10, scrollX = TRUE), selection = 'single') # Aktifkan pemilihan baris
    
    # Observe cell edits for Packager table
    observeEvent(input$admin_packager_crud_table_cell_edit, {
      info <- input$admin_packager_crud_table_cell_edit
      row <- info$row
      col <- info$col
      value <- info$value
      colname <- colnames(reactive_data$packager)[col]
      
      pk_value <- reactive_data$packager$no_package[row]
      
      if (colname == "tanggal_packaging") {
        value <- as.Date(value)
      }
      
      reactive_data$packager[row, col] <- value
      
      sql_query <- sprintf("UPDATE packager SET %s = %s WHERE no_package = %s",
                           DBI::dbQuoteIdentifier(conn, colname),
                           DBI::dbQuoteLiteral(conn, value),
                           DBI::dbQuoteLiteral(conn, pk_value))
      
      tryCatch({
        DBI::dbExecute(conn, sql_query)
        showNotification(paste("Packager data updated:", pk_value), type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating Packager data:", e$message), type = "error")
        reactive_data$packager <- read_and_convert_table(conn, "packager", c("tanggal_packaging"))
      })
    })
    
    # Add new Packager data
    observeEvent(input$admin_add_packager, {
      req(input$admin_packager_no_package, input$admin_packager_tanggal_packaging,
          input$admin_packager_pic, input$admin_packager_items_in_package)
      
      new_entry <- data.frame(
        no_package = input$admin_packager_no_package,
        tanggal_packaging = input$admin_packager_tanggal_packaging,
        PIC = input$admin_packager_pic,
        items_in_package = input$admin_packager_items_in_package,
        stringsAsFactors = FALSE
      )
      
      tryCatch({
        DBI::dbAppendTable(conn, "packager", new_entry)
        reactive_data$packager <- read_and_convert_table(conn, "packager", c("tanggal_packaging"))
        showNotification("Data Packager baru berhasil ditambahkan!", type = "message")
        updateTextInput(session, "admin_packager_no_package", value = "")
        updateDateInput(session, "admin_packager_tanggal_packaging", value = Sys.Date())
        updateTextInput(session, "admin_packager_pic", value = "")
        updateTextInput(session, "admin_packager_items_in_package", value = "")
      }, error = function(e) {
        showNotification(paste("Error menambahkan data Packager:", e$message), type = "error")
      })
    })
    
    # Delete Packager data
    observeEvent(input$admin_delete_packager, {
      selected_row <- input$admin_packager_crud_table_rows_selected
      if (length(selected_row) > 0) {
        no_package_to_delete <- reactive_data$packager$no_package[selected_row]
        
        sql_query <- sprintf("DELETE FROM packager WHERE no_package = %s",
                             DBI::dbQuoteLiteral(conn, no_package_to_delete))
        
        tryCatch({
          DBI::dbExecute(conn, sql_query)
          reactive_data$packager <- read_and_convert_table(conn, "packager", c("tanggal_packaging"))
          showNotification(paste("Data Packager", no_package_to_delete, "berhasil dihapus!"), type = "message")
        }, error = function(e) {
          showNotification(paste("Error menghapus data Packager:", e$message), type = "error")
        })
      } else {
        showModal(modalDialog("Pilih baris yang ingin dihapus terlebih dahulu.", easyClose = TRUE))
      }
    })
  })
}


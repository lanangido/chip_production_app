# modules/admin_akun_server.R

admin_akun_server <- function(id, reactive_data, conn) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Namespace untuk modul ini
    
    # Render Akun table for CRUD
    output$admin_akun_crud_table <- DT::renderDataTable({
      reactive_data$akun
    }, editable = TRUE, options = list(pageLength = 10, scrollX = TRUE), selection = 'single') # Aktifkan pemilihan baris
    
    # Observe cell edits for Akun table
    observeEvent(input$admin_akun_crud_table_cell_edit, {
      info <- input$admin_akun_crud_table_cell_edit
      row <- info$row
      col <- info$col
      value <- info$value
      colname <- colnames(reactive_data$akun)[col]
      
      pk_value <- reactive_data$akun$email[row] # Primary key is email
      
      reactive_data$akun[row, col] <- value
      
      sql_query <- sprintf("UPDATE akun SET %s = %s WHERE email = %s",
                           DBI::dbQuoteIdentifier(conn, colname),
                           DBI::dbQuoteLiteral(conn, value),
                           DBI::dbQuoteLiteral(conn, pk_value))
      
      tryCatch({
        DBI::dbExecute(conn, sql_query)
        showNotification(paste("Akun data updated:", pk_value), type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating Akun data:", e$message), type = "error")
        reactive_data$akun <- read_and_convert_table(conn, "akun")
      })
    })
    
    # Add new Akun data
    observeEvent(input$admin_add_akun, {
      req(input$admin_akun_email, input$admin_akun_password, input$admin_akun_role)
      
      new_entry <- data.frame(
        email = input$admin_akun_email,
        password = input$admin_akun_password, # WARNING: Plain text password!
        role = input$admin_akun_role,
        stringsAsFactors = FALSE
      )
      
      tryCatch({
        DBI::dbAppendTable(conn, "akun", new_entry)
        reactive_data$akun <- read_and_convert_table(conn, "akun")
        showNotification("Akun baru berhasil ditambahkan!", type = "message")
        updateTextInput(session, "admin_akun_email", value = "")
        updateTextInput(session, "admin_akun_password", value = "")
        updateSelectInput(session, "admin_akun_role", selected = "Assembler")
      }, error = function(e) {
        showNotification(paste("Error menambahkan akun:", e$message), type = "error")
      })
    })
    
    # Delete Akun data
    observeEvent(input$admin_delete_akun, {
      selected_row <- input$admin_akun_crud_table_rows_selected
      if (length(selected_row) > 0) {
        email_to_delete <- reactive_data$akun$email[selected_row]
        
        # Dapatkan peran akun yang akan dihapus
        role_of_account_to_delete <- reactive_data$akun %>%
          filter(email == email_to_delete) %>%
          pull(role)
        
        # Pencegahan 1: Jangan biarkan admin menghapus akun Admin terakhir
        if (role_of_account_to_delete == "Admin") {
          num_admin_accounts <- reactive_data$akun %>%
            filter(role == "Admin") %>%
            nrow()
          
          if (num_admin_accounts == 1) {
            showModal(modalDialog("Tidak dapat menghapus akun Admin terakhir. Setidaknya harus ada satu akun Admin.", easyClose = TRUE))
            return()
          }
        }
        
        # Pencegahan 2: Jangan biarkan admin menghapus akunnya sendiri
        # Asumsi: Anda menyimpan email login di session$userData$email_logged_in
        # Pastikan session$userData$email_logged_in diatur saat login di server.R
        if (!is.null(session$userData$email_logged_in) && email_to_delete == session$userData$email_logged_in) {
          showModal(modalDialog("Tidak dapat menghapus akun yang sedang Anda gunakan.", easyClose = TRUE))
          return()
        }
        
        sql_query <- sprintf("DELETE FROM akun WHERE email = %s",
                             DBI::dbQuoteLiteral(conn, email_to_delete))
        
        tryCatch({
          DBI::dbExecute(conn, sql_query)
          reactive_data$akun <- read_and_convert_table(conn, "akun")
          showNotification(paste("Akun", email_to_delete, "berhasil dihapus!"), type = "message")
        }, error = function(e) {
          showNotification(paste("Error menghapus akun:", e$message), type = "error")
        })
      } else {
        showModal(modalDialog("Pilih baris akun yang ingin dihapus terlebih dahulu.", easyClose = TRUE))
      }
    })
  })
}


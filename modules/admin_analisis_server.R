# modules/admin_analisis_server.R

admin_analisis_server <- function(id, reactive_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Namespace untuk modul ini
    
    # Reactive filtered data for analysis plots
    filtered_analysis_data <- reactive({
      start_date <- input$admin_analisis_date_range[1]
      end_date <- input$admin_analisis_date_range[2]
      pic_filter <- input$admin_analisis_pic_filter
      status_filter <- input$admin_analisis_status_filter
      
      assembler_data <- reactive_data$assembler %>%
        filter(tanggal_start >= start_date & tanggal_start <= end_date)
      
      tester_data <- reactive_data$tester %>%
        filter(tanggal_testing >= start_date & tanggal_testing <= end_date)
      
      packager_data <- reactive_data$packager %>%
        filter(tanggal_packaging >= start_date & tanggal_packaging <= end_date)
      
      if (pic_filter != "Semua") {
        assembler_data <- assembler_data %>% filter(PIC == pic_filter)
        tester_data <- tester_data %>% filter(PIC == pic_filter)
        packager_data <- packager_data %>% filter(PIC == pic_filter)
      }
      
      if (status_filter != "Semua") {
        tester_data <- tester_data %>% filter(status == status_filter)
      }
      
      list(assembler = assembler_data, tester = tester_data, packager = packager_data)
    })
    
    # Update PIC filter choices dynamically
    observe({
      all_pics <- unique(c(reactive_data$assembler$PIC, reactive_data$tester$PIC, reactive_data$packager$PIC))
      updateSelectInput(session, "admin_analisis_pic_filter", choices = c("Semua", sort(all_pics)))
    })
    
    # Plot 1: Produksi per Bulan (Assembler)
    output$admin_plot_monthly_assembly <- renderPlotly({
      data <- filtered_analysis_data()$assembler %>%
        mutate(month_year = format(tanggal_start, "%Y-%m")) %>%
        group_by(month_year) %>%
        summarise(count = n()) %>%
        arrange(month_year)
      
      if (nrow(data) == 0) return(NULL)
      
      plot_ly(data, x = ~month_year, y = ~count, type = 'bar', name = 'Jumlah Produksi') %>%
        layout(title = "Jumlah Produksi Assembler per Bulan",
               xaxis = list(title = "Bulan-Tahun"),
               yaxis = list(title = "Jumlah Produksi"))
    })
    
    # Plot 2: Status Testing per PIC
    output$admin_plot_tester_status_pic <- renderPlotly({
      data <- filtered_analysis_data()$tester %>%
        group_by(PIC, status) %>%
        summarise(count = n(), .groups = 'drop')
      
      if (nrow(data) == 0) return(NULL)
      
      plot_ly(data, x = ~PIC, y = ~count, type = 'bar', color = ~status,
              colors = c("Lolos" = "green", "Tidak Lolos" = "red")) %>%
        layout(title = "Status Testing per PIC",
               xaxis = list(title = "PIC Tester"),
               yaxis = list(title = "Jumlah Item"),
               barmode = 'stack')
    })
    
    # Plot 3: Jumlah Paket per PIC (Packager)
    output$admin_plot_packager_count_pic <- renderPlotly({
      data <- filtered_analysis_data()$packager %>%
        group_by(PIC) %>%
        summarise(count = n(), .groups = 'drop')
      
      if (nrow(data) == 0) return(NULL)
      
      plot_ly(data, x = ~PIC, y = ~count, type = 'bar', name = 'Jumlah Paket') %>%
        layout(title = "Jumlah Paket yang Dibuat per PIC",
               xaxis = list(title = "PIC Packager"),
               yaxis = list(title = "Jumlah Paket"))
    })
    
    # Plot 4: Distribusi Waktu Assembly (Durasi dalam Hari)
    output$admin_plot_assembly_duration <- renderPlotly({
      data <- filtered_analysis_data()$assembler %>%
        mutate(duration_days = as.numeric(tanggal_stop - tanggal_start)) %>%
        filter(!is.na(duration_days) & duration_days >= 0) # Filter out invalid durations
      
      if (nrow(data) == 0) return(NULL)
      
      plot_ly(data, x = ~duration_days, type = 'histogram',
              marker = list(color = '#1f77b4', line = list(color = 'white', width = 1))) %>%
        layout(title = "Distribusi Durasi Assembly (Hari)",
               xaxis = list(title = "Durasi (Hari)"),
               yaxis = list(title = "Frekuensi"))
    })
    
    # --- Admin Dashboard: Download CSVs (existing) ---
    output$download_assembler_csv <- downloadHandler(
      filename = function() { paste("assembler_data_", Sys.Date(), ".csv", sep = "") },
      content = function(file) { write_csv(reactive_data$assembler, file) }
    )
    
    output$download_tester_csv <- downloadHandler(
      filename = function() { paste("tester_data_", Sys.Date(), ".csv", sep = "") },
      content = function(file) { write_csv(reactive_data$tester, file) }
    )
    
    output$download_packager_csv <- downloadHandler(
      filename = function() { paste("packager_data_", Sys.Date(), ".csv", sep = "") },
      content = function(file) { write_csv(reactive_data$packager, file) }
    )
    
    output$download_akun_csv <- downloadHandler(
      filename = function() { paste("akun_data_", Sys.Date(), ".csv", sep = "") },
      content = function(file) { write_csv(reactive_data$akun, file) }
    )
  })
}


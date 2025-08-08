# modules/admin_analisis_ui.R


admin_analisis_ui <- function(ns) { 
  tabItem(tabName = "admin_analisis",
          h2("Admin: Analisis Data Mendalam"),
          fluidRow(
            box(title = "Filter Analisis", status = "primary", solidHeader = TRUE, width = 12,
                dateRangeInput(ns("admin_analisis_date_range"), "Rentang Tanggal", # Gunakan ns()
                               start = Sys.Date() - 90, end = Sys.Date(),
                               format = "dd/mm/yyyy", separator = "to"),
                selectInput(ns("admin_analisis_pic_filter"), "Filter PIC (Semua Tahap)", choices = c("Semua")), # Gunakan ns()
                selectInput(ns("admin_analisis_status_filter"), "Filter Status Testing", choices = c("Semua", "Lolos", "Tidak Lolos")) # Gunakan ns()
            )
          ),
          fluidRow(
            box(title = "Produksi per Bulan (Assembler)", status = "info", solidHeader = TRUE, width = 6,
                plotlyOutput(ns("admin_plot_monthly_assembly"))), # Gunakan ns()
            box(title = "Status Testing per PIC", status = "info", solidHeader = TRUE, width = 6,
                plotlyOutput(ns("admin_plot_tester_status_pic"))), # Gunakan ns()
            box(title = "Jumlah Paket per PIC", status = "info", solidHeader = TRUE, width = 6,
                plotlyOutput(ns("admin_plot_packager_count_pic"))), # Gunakan ns()
            box(title = "Distribusi Waktu Assembly (Hari)", status = "info", solidHeader = TRUE, width = 6,
                plotlyOutput(ns("admin_plot_assembly_duration"))) # Gunakan ns()
          )
  )
}


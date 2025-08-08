# modules/admin_akun_ui.R

admin_akun_ui <- function(ns) {
  tabItem(tabName = "admin_akun",
          h2("Admin: Kelola Data Akun Pengguna"),
          fluidRow(
            box(title = "Tambah Akun Baru", status = "warning", solidHeader = TRUE, width = 12,
                textInput(ns("admin_akun_email"), "Email (Wajib, unik)"),
                passwordInput(ns("admin_akun_password"), "Password"),
                selectInput(ns("admin_akun_role"), "Role", choices = c("Assembler", "Tester", "Packager", "Admin")),
                actionButton(ns("admin_add_akun"), "Tambah Akun", class = "btn-success")
            ),
            box(title = "Data Akun (Edit Langsung)", status = "primary", solidHeader = TRUE, width = 12,
                helpText("Klik dua kali pada sel untuk mengedit. Tekan Enter untuk menyimpan. Peringatan: Password disimpan dalam teks biasa!"),
                DT::dataTableOutput(ns("admin_akun_crud_table")),
                actionButton(ns("admin_delete_akun"), "Hapus Akun Terpilih", class = "btn-danger", style = "margin-top: 10px;") # Tombol Hapus
            )
          )
  )
}


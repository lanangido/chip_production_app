# ui.R

ui <- dashboardPage(
  dashboardHeader(title = "Aplikasi Produksi Chip"),
  dashboardSidebar(
    textInput("email", "Masukkan Email"),
    passwordInput("password", "Masukkan Password"),
    actionButton("login_btn", "Login", class = "btn-primary"),
    actionButton("logout_btn", "Logout", class = "btn-danger", style = "margin-top: 15px;"),
    sidebarMenuOutput("sidebar")
  ),
  dashboardBody(
    useShinyjs(),
    # Pustaka JsBarcode tidak lagi diperlukan di sini karena tidak ada rendering SVG langsung
    uiOutput("main_ui")
  )
)
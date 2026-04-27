library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(corrplot)
library(DT)
library(tidyr)
library(psych)
library(rlang)
library(ggsci)
library(GPArotation)
library(polynom)

# ==========================================
# UI (USER INTERFACE)
# ==========================================
ui <- dashboardPage(
  skin = "yellow",
  
  # HEADER
  dashboardHeader(title = "Dashboard Analisis Cluster Multivariat Berbasis Unsupervised Machine Learning", titleWidth = 400),
  
  # SIDEBAR MENU
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Eksplorasi & Preprocessing", tabName = "preprocessing", icon = icon("broom")),
      menuItem("Uji Asumsi & Kelayakan", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Analisis Cluster", tabName = "Cluster", icon = icon("chart-pie")),
      menuItem("Validasi Model", tabName = "validasi", icon = icon("vial")),
      
      tags$hr(), 
      
      # INPUT DATA DI SIDEBAR
      fileInput("file_data", "Unggah Data (.csv atau .xlsx)", accept = c(".csv", ".xlsx")),
      
      # INPUT PARAMETER METODE
      selectInput("metode", "Pilih Metode Clustering:",
                  choices = c("K-Means" = "kmeans",
                              "K-Medoids" = "kmedoids",
                              "Hierarchical" = "hc")),
      
      # INPUT PARAMETER K
      checkboxInput("auto_k", "Auto Cluster (Silhouette)", TRUE),
      
      conditionalPanel(
        condition = "input.auto_k == false",
        sliderInput("k_input", "Input Parameter (Jumlah Cluster):",
                    min = 2, max = 10, value = 5)
      )
    )
  ),
  
  # BODY (ISI KONTEN APLIKASI)
  dashboardBody(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap');
      body {
        font-family: 'Roboto', sans-serif;
      }
      .content-wrapper, .right-side {
        background-color: #FFF8F0;
      }
      .main-header .navbar {
        background-color: #E85D04;
      }
      .main-sidebar {
        background-color: #DC2F02;
      }
      .main-sidebar .sidebar a {
        color: #FFF8F0;
      }
      .box.box-solid.box-primary > .box-header {
        background: #FFBA08;
        color: #2F3E46;
      }
      .box.box-solid.box-info > .box-header {
        background: #F48C06;
        color: #FFFFFF;
      }
      .box.box-solid.box-warning > .box-header {
        background: #E85D04;
        color: #FFFFFF;
      }
      .box.box-solid.box-danger > .box-header {
        background: #DC2F02;
        color: #FFFFFF;
      }
      .box.box-solid.box-success > .box-header {
        background: #FFBA08;
        color: #2F3E46;
      }
      .box-title {
        font-weight: 500;
      }
      .btn {
        font-weight: 500;
      }
    ")),
    tabItems(
      
      # --- MENU 1: HOME ---
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE, title = "Selamat Datang di Dashboard Analisis Cluster Multivariat",
                    h3(strong("Dashboard Analisis Cluster Multivariat Berbasis Unsupervised Machine Learning")),
                    hr(),
                    
                    h4(strong("📊 Manfaat Utama Aplikasi:")),
                    tags$ul(
                      tags$li(
                        strong("Pemetaan Berbasis Data Objektif (Data-Driven Clustering): "),
                        "Aplikasi ini secara otomatis mengelompokkan wilayah atau data observasi berdasarkan kemiripan pola statistiknya, tanpa adanya bias atau tebakan subjektif. Hal ini memudahkan pengguna untuk menemukan \"pola tersembunyi\" dari kumpulan data yang besar dan rumit."
                      ),
                      tags$li(
                        strong("Dukungan Pengambilan Kebijakan (Decision Support): "),
                        "Mengubah tabel angka multivariat menjadi ringkasan profil (tipologi) yang mudah dipahami. Dengan mengetahui karakteristik pasti dari setiap Cluster, pemerintah atau manajemen dapat merancang program, intervensi, atau strategi bisnis yang jauh lebih presisi dan tepat sasaran."
                      ),
                      tags$li(
                        strong("Integrasi Analisis Statistik Menyeluruh (End-to-End Analysis): "),
                        "Pengguna tidak perlu repot menghitung secara manual. Aplikasi ini telah mengotomatisasi seluruh alur kerja komputasi statistika yang baku—mulai dari penyesuaian skala data (Z-Score), uji kelayakan variabel (VIF), penentuan jumlah kelompok terbaik (Silhouette Index), hingga uji validasi kebenaran Cluster (Uji ANOVA)."
                      ),
                      tags$li(
                        strong("Eksplorasi Multi-Algoritma yang Fleksibel: "),
                        "Menyediakan wadah bagi analis untuk bereksperimen dan membandingkan performa tiga metode pengelompokan yang berbeda (K-Means, K-Medoids, dan Hierarchical Clustering). Fleksibilitas ini memastikan pengguna selalu mendapatkan hasil pemetaan yang paling kebal terhadap outlier dan paling sesuai dengan kondisi data aktual."
                      )
                    ),
                    
                    hr(),
                    h4(strong("👥 Tim Penyusun Aplikasi:")),
                    tags$ul(
                      tags$li("Ulinnuha Zaidan Yahya (24050123120006)"),
                      tags$li("Rafi Abdul Rosid (24050123120010)"),
                      tags$li("Tirza Nadya Wibowo (24050123120011)"),
                      tags$li("Novianingsih Dwi Saputri (24050123120026)"),
                      tags$li("Aulora Andini Ikamanda (24050123120027)")
                    ),
                    
                    hr(),
                    p(
                      strong("Program Studi Statistika, Departemen Statistika, Universitas Diponegoro"),
                      br(),
                      em("Tahun 2026")
                    )
                )
              )
      ),
      
      # --- MENU 2: FASE 1 - EKSPLORASI & PREPROCESSING ---
      tabItem(tabName = "preprocessing",
              fluidRow(
                # Highlight Indikator
                valueBoxOutput("box_jumlah_data", width = 4),
                valueBoxOutput("box_metode", width = 4),
                valueBoxOutput("box_k", width = 4)
              ),
              fluidRow(
                box(width = 12, title = "Dataset", status = "primary", DTOutput("tabel_data")),
                box(width = 12, title = "Statistik Deskriptif", status = "info", tableOutput("tabel_deskriptif"))
              ),
              fluidRow(
                box(width = 6, title = "Uji Missing Data", status = "info", tableOutput("tabel_missing")),
                box(width = 6, title = "Ringkasan Missing Data (%)", status = "info", verbatimTextOutput("ringkasan_missing"))
              ),
              fluidRow(
                box(width = 12, title = "Identifikasi Outlier (Boxplot Per Variabel)", status = "warning", plotOutput("plot_boxplot"))
              ),
              fluidRow(
                box(width = 12, title = "Distribusi Data (Histogram Per Variabel)", status = "success", plotOutput("plot_distribusi"))
              ),
              fluidRow(
                box(width = 12, title = "Data Standarisasi (Z-Score)", status = "success", DTOutput("tabel_scaled"))
              )
      ),
      
      # --- MENU 3: FASE 2 - UJI ASUMSI & KELAYAKAN ---
      tabItem(tabName = "asumsi",
              fluidRow(
                box(width = 12, title = "Uji KMO (Kaiser-Meyer-Olkin)", status = "primary",
                    p("Mengukur kecukupan sampel dan kelayakan variabel untuk analisis Cluster."),
                    p(strong("Kriteria: KMO > 0.5 = Layak | KMO ≤ 0.5 = Tidak Layak")),
                    verbatimTextOutput("hasil_kmo"))
              ),
              fluidRow(
                box(width = 7, title = "Matriks Korelasi antar Indikator", status = "info", plotOutput("plot_korelasi")),
                box(width = 5, title = "Uji Multikolinearitas (VIF)", status = "danger", 
                    p(strong("Kriteria: VIF < 10 = Tidak Ada Multikolinearitas")),
                    tableOutput("tabel_vif"))
              )
      ),
      
      # --- MENU 4: FASE 3 - ANALISIS Cluster ---
      tabItem(tabName = "Cluster",
              fluidRow(
                box(width = 12, title = "Penentuan Jumlah Cluster Optimal (Metode Silhouette)", status = "primary",
                    p("Menguji berbagai nilai k untuk menemukan jumlah Cluster terbaik berdasarkan skor Silhouette tertinggi."),
                    plotOutput("plot_silhouette"))
              ),
              fluidRow(
                box(width = 8, title = "Visualisasi Hasil Clustering", status = "success", plotOutput("plot_cluster")),
                box(width = 4, title = "Statistik Cluster", status = "info", verbatimTextOutput("statistik_Cluster"))
              ),
              fluidRow(
                box(width = 12, title = "Tabel Keanggotaan Cluster", status = "info", DTOutput("tabel_hasil"))
              )
      ),
      
      # --- MENU 5: FASE 4 - VALIDASI MODEL ---
      tabItem(tabName = "validasi",
              fluidRow(
                box(width = 12, title = "Evaluasi Cluster (Silhouette Score per Observasi)", status = "primary",
                    p("Mengevaluasi seberapa kuat setiap anggota berada di Clusternya masing-masing."),
                    plotOutput("plot_silhouette_score"))
              ),
              fluidRow(
                box(width = 12, title = "Uji One-Way ANOVA (Perbedaan Antar Cluster)", status = "danger",
                    p("Menguji apakah ada perbedaan nilai rata-rata yang signifikan antar Cluster yang terbentuk."),
                    p("H0: Tidak ada perbedaan karakteristik antar Cluster."),
                    p("H1: Terdapat minimal satu Cluster yang memiliki karakteristik berbeda."),
                    p(strong("Kriteria Uji: Jika p-value < 0.05, Tolak H0.")),
                    hr(),
                    verbatimTextOutput("hasil_anova")
                )
              ),
              fluidRow(
                box(width = 12, title = "Analisis Diskriminan (Validasi Ketepatan Klasifikasi & Fungsi Pembeda)", status = "warning",
                    p("Mengecek akurasi klasifikasi dan mengidentifikasi variabel yang paling kuat membedakan antar Cluster."),
                    verbatimTextOutput("hasil_diskriminan"))
              )
      )
    )
  )
)

# ==========================================
# 2. BAGIAN SERVER (LOGIKA APLIKASI)
# ==========================================
server <- function(input, output, session) {
  
  # Membaca Data Input (Reaktif)
  data_mentah <- reactive({
    req(input$file_data) # Tunggu sampai data diupload
    
    # Deteksi format file berdasarkan extension
    file_path <- input$file_data$datapath
    file_name <- input$file_data$name
    
    # Baca file berdasarkan format
    if(grepl("\\.xlsx$", file_name, ignore.case = TRUE)){
      # Baca file XLSX
      if(!require(readxl, quietly = TRUE)){
        cat("Package 'readxl' dibutuhkan. Install dengan: install.packages('readxl')\n")
        return(NULL)
      }
      readxl::read_excel(file_path)
    } else if(grepl("\\.csv$", file_name, ignore.case = TRUE)){
      # Baca file CSV (coba deteksi separator)
      data <- tryCatch(
        read.csv(file_path, sep = ";"),
        error = function(e) read.csv(file_path, sep = ",")
      )
      data
    } else {
      # Format tidak dikenali
      return(NULL)
    }
  })
  
  # Standardisasi Data Numerik (Z-Score)
  data_scaled <- reactive({
    req(data_mentah())
    df <- data_mentah()
    
    # Ambil kolom numerik saja (skip kolom pertama)
    df_num <- df[, -1, drop = FALSE]
    
    # Konversi semua kolom ke numeric
    df_num <- as.data.frame(lapply(df_num, function(x) {
      # Coba konversi ke numeric
      suppressWarnings(as.numeric(as.character(x)))
    }))
    
    # Hapus kolom yang sepenuhnya NA (gagal dikonversi)
    valid_cols <- !sapply(df_num, function(x) all(is.na(x)))
    df_num <- df_num[, valid_cols, drop = FALSE]
    
    # Jika tidak ada kolom numerik yang valid, kembalikan data kosong
    if(ncol(df_num) == 0) {
      # Kembalikan matrix kosong dengan 1 baris untuk menghindari error
      return(matrix(0, nrow = nrow(df), ncol = 1))
    }
    
    # Jika ada kolom dengan NA, isi dengan mean (imputasi sederhana)
    for(i in 1:ncol(df_num)) {
      if(any(is.na(df_num[, i]))) {
        df_num[is.na(df_num[, i]), i] <- mean(df_num[, i], na.rm = TRUE)
      }
    }
    
    # Standardisasi data
    scale(df_num)
  })
  
  # ================= HIGHLIGHT VALUE BOX =================
  output$box_jumlah_data <- renderValueBox({
    req(data_mentah())
    valueBox(nrow(data_mentah()), "Jumlah Observasi (Wilayah)", icon = icon("map-location-dot"), color = "green")
  })
  
  output$box_metode <- renderValueBox({
    nama_metode <- switch(input$metode, "kmedoids" = "K-Medoids", "kmeans" = "K-Means", "hc" = "Hierarchical")
    valueBox(nama_metode, "Metode Estimasi/Optimasi", icon = icon("gears"), color = "yellow")
  })
  
  output$box_k <- renderValueBox({
    if(input$auto_k){
      valueBox("Auto (Silhouette)", "Parameter K", icon = icon("layer-group"), color = "blue")
    } else {
      valueBox(input$k_input, "Parameter K (Jumlah Cluster)", icon = icon("layer-group"), color = "blue")
    }
  })
  
  # ================= MENU EDA =================
  output$tabel_data <- renderDT({ req(data_mentah()); datatable(data_mentah()) })
  
  output$plot_boxplot <- renderPlot({
    req(data_mentah())
    df <- data_mentah()
    
    # Ambil kolom numerik saja dan konversi
    df_num <- df[, -1, drop = FALSE]
    df_num <- as.data.frame(lapply(df_num, function(x) suppressWarnings(as.numeric(as.character(x)))))
    
    # Hapus kolom yang tidak valid
    valid_cols <- !sapply(df_num, function(x) all(is.na(x)))
    df_num <- df_num[, valid_cols, drop = FALSE]
    
    # Jika tidak ada data valid, tampilkan pesan
    if(ncol(df_num) == 0) {
      plot.new()
      text(0.5, 0.5, "Tidak ada data numerik valid untuk boxplot", cex = 1.2)
      return()
    }
    
    df_long <- df_num %>% pivot_longer(cols = everything(), names_to = "Variabel", values_to = "Nilai")
    ggplot(df_long, aes(x = Variabel, y = Nilai, fill = Variabel)) +
      geom_boxplot() + facet_wrap(~Variabel, scales = "free") +
      theme_bw() + theme(legend.position = "none")
  })
  
  # ================= MENU ASUMSI =================
  output$plot_korelasi <- renderPlot({
    req(data_scaled())
    
    # Cek apakah data valid
    scaled_data <- data_scaled()
    if(is.null(scaled_data) || ncol(scaled_data) < 2) {
      plot.new()
      text(0.5, 0.5, "Tidak ada data yang cukup untuk plot korelasi\nMinimal 2 variabel numerik diperlukan", cex = 1.2)
      return()
    }
    
    tryCatch({
      corrplot(cor(scaled_data), method = "color", addCoef.col = "black", type = "upper", tl.col = "black")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error membuat plot korelasi:\n", e$message), cex = 1.0)
    })
  })
  
  output$tabel_vif <- renderTable({
    req(data_scaled())
    
    # Cek apakah data valid
    scaled_data <- data_scaled()
    if(is.null(scaled_data) || ncol(scaled_data) < 2) {
      return(data.frame(Variabel = "Data tidak cukup", Nilai_VIF = "Minimal 2 variabel diperlukan"))
    }
    
    tryCatch({
      matriks_korelasi <- cor(scaled_data)
      nilai_vif <- diag(solve(matriks_korelasi)) # Menghitung VIF via Invers Matriks Korelasi
      data.frame(
        Indikator = tools::toTitleCase(gsub("[._]", " ", colnames(scaled_data))),
        `Nilai VIF` = nilai_vif, 
        check.names = FALSE 
      )
    }, error = function(e) {
      data.frame(Indikator = "Error", Nilai_VIF = paste("Error menghitung VIF:", e$message))
    })
  })
  
  # ================= MENU ESTIMASI PARAMETER =================
  output$plot_silhouette <- renderPlot({
    req(data_scaled())
    
    # Cek apakah data valid
    scaled_data <- data_scaled()
    if(is.null(scaled_data) || ncol(scaled_data) < 2) {
      plot.new()
      text(0.5, 0.5, "Tidak ada data yang cukup untuk analisis silhouette\nMinimal 2 variabel numerik diperlukan", cex = 1.2)
      return()
    }
    
    tryCatch({
      # Fviz_nbclust otomatis mencari K optimal dengan membandingkan nilai k=2 sampai k=10
      if(input$metode == "kmedoids"){
        fviz_nbclust(scaled_data, pam, method = "silhouette")
      } else if (input$metode == "kmeans"){
        set.seed(123)
        fviz_nbclust(scaled_data, kmeans, method = "silhouette")
      } else {
        fviz_nbclust(scaled_data, hcut, method = "silhouette")
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error membuat plot silhouette:\n", e$message), cex = 1.0)
    })
  })
  
  k_optimal <- reactive({
    req(data_scaled())
    scaled_data <- data_scaled()
    
    tryCatch({
      # Buat vektor kosong untuk menampung hasil dari K=2 sampai K=10
      # Kita gunakan panjang 10 agar index sesuai dengan nilai K
      sil_width <- rep(0, 10) 
      
      for(k in 2:10){
        if(input$metode == "kmedoids"){
          model <- cluster::pam(scaled_data, k = k)
          sil <- cluster::silhouette(model$clustering, dist(scaled_data))
        } else if(input$metode == "kmeans"){
          set.seed(123) # Seed harus sama di setiap iterasi
          model <- kmeans(scaled_data, centers = k, nstart = 25)
          sil <- cluster::silhouette(model$cluster, dist(scaled_data))
        } else {
          hc <- hclust(dist(scaled_data), method = "ward.D2")
          cluster_cut <- cutree(hc, k = k)
          sil <- cluster::silhouette(cluster_cut, dist(scaled_data))
        }
        sil_width[k] <- mean(sil[, 3])
      }
      
      # Mengambil index yang memiliki nilai silhouette tertinggi
      best_k <- which.max(sil_width)
      return(best_k)
      
    }, error = function(e){
      return(2)
    })
  })
  
  hasil_model <- reactive({
    req(data_scaled())
    scaled_data <- data_scaled()
    
    # Ambil K dari fungsi k_optimal() jika auto_k dicentang
    k_final <- if(input$auto_k) k_optimal() else input$k_input
    
    tryCatch({
      if(input$metode == "kmedoids"){
        return(cluster::pam(scaled_data, k = k_final))
        
      } else if (input$metode == "kmeans"){
        set.seed(123) # Seed harus sama dengan yang ada di k_optimal
        # Tambahkan nstart = 25 untuk stabilitas hasil
        return(kmeans(scaled_data, centers = k_final, nstart = 25))
        
      } else {
        res.hc <- hclust(dist(scaled_data), method = "ward.D2")
        return(cutree(res.hc, k = k_final))
      }
      
    }, error = function(e){
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$plot_cluster <- renderPlot({
    req(hasil_model(), data_scaled())
    
    scaled_data <- data_scaled()
    model <- hasil_model()
    
    # Tentukan K untuk keperluan visualisasi (terutama Dendrogram)
    k_final <- if(input$auto_k) k_optimal() else input$k_input
    
    tryCatch({
      if(input$metode == "hc"){
        # Khusus Hierarchical menggunakan fviz_dend
        res.hc <- hclust(dist(scaled_data), method = "ward.D2")
        fviz_dend(res.hc, k = k_final, rect = TRUE, 
                  main = paste("Dendrogram (K =", k_final, ")"))
      } else {
        # K-Means dan K-Medoids menggunakan fviz_cluster
        fviz_cluster(model, data = scaled_data, 
                     geom = "point", 
                     ellipse.type = "convex",
                     main = paste("Visualisasi Cluster (K =", k_final, ")"))
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Gagal memvisualisasikan cluster", cex = 1.2)
    })
  })
  
  output$tabel_hasil <- renderDT({
    req(hasil_model(), data_mentah())
    
    df_hasil <- data_mentah()
    nama_wilayah <- df_hasil[[1]]
    
    if(input$metode %in% c("kmedoids", "kmeans")){
      Cluster_vec <- as.factor(hasil_model()$cluster)
    } else {
      Cluster_vec <- as.factor(hasil_model())
    }
    
    df_temp <- data.frame(Wilayah = nama_wilayah, Cluster = Cluster_vec, stringsAsFactors = FALSE)
    Cluster_labels <- sort(unique(df_temp$Cluster))
    
    anggota_list <- lapply(Cluster_labels, function(k){
      df_temp$Wilayah[df_temp$Cluster == k]
    })
    
    max_len <- max(sapply(anggota_list, length))
    
    anggota_pad <- lapply(anggota_list, function(x){
      c(x, rep("", max_len - length(x)))
    })
    
    tabel_final <- as.data.frame(do.call(cbind, anggota_pad), stringsAsFactors = FALSE)
    colnames(tabel_final) <- paste("Cluster", Cluster_labels)
    tabel_final <- cbind(No = 1:max_len, tabel_final)
    
    datatable(
      tabel_final,
      rownames = FALSE,
      options = list(
        pageLength = -1,      # Menampilkan semua data dalam satu scroll (bukan per halaman)
        lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
        scrollY = "400px",    # Tinggi maksimal tabel sebelum muncul scrollbar vertikal
        scrollX = TRUE,       # Aktifkan scroll horizontal
        scrollCollapse = TRUE, # Tabel menyusut jika data sedikit
        paging = TRUE,        # Tetap aktifkan paging jika ingin navigasi halaman
        autoWidth = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '40px', targets = 0),
          list(width = '200px', targets = 1:(ncol(tabel_final)-1))
        ),
        search = list(regex = TRUE, caseInsensitive = TRUE),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({'background-color': '#FFBA08', 'color': '#2F3E46'});",
          "  var table = this.api();",
          "  $('.dataTables_filter input').off().on('keyup', function() {",
          "    var term = $(this).val().toLowerCase();",
          "    if (term === '') {",
          "       table.columns().visible(true);",
          "       table.search('').draw();",
          "       return;",
          "    }",
          "    table.columns().every(function(index) {",
          "      if(index === 0) return;",
          "      var column = this;",
          "      var colText = column.data().join(' ').toLowerCase();",
          "      if (colText.indexOf(term) !== -1) {",
          "        column.visible(true);",
          "      } else {",
          "        column.visible(false);",
          "      }",
          "    });",
          "  });",
          "}"
        )
      ),
      selection = "none"
    ) %>%
      formatStyle(names(tabel_final), border = '1px solid #ddd')
  })
  
  # ================= FASE 1: PREPROCESSING =================
  # Statistik Deskriptif
  output$tabel_deskriptif <- renderTable({
    req(data_mentah())
    df <- data_mentah()
    
    # Ambil kolom numerik saja dan konversi ke numeric
    df_num <- df[, -1, drop = FALSE]
    df_num <- as.data.frame(lapply(df_num, function(x) suppressWarnings(as.numeric(as.character(x)))))
    
    # Hapus kolom yang tidak bisa dikonversi (semua NA)
    valid_cols <- !sapply(df_num, function(x) all(is.na(x)))
    df_num <- df_num[, valid_cols, drop = FALSE]
    
    # Jika tidak ada kolom valid, return data frame kosong
    if(ncol(df_num) == 0) {
      return(data.frame(Variabel = "Tidak ada data numerik valid"))
    }
    
    # Menghitung statistik deskriptif untuk setiap variabel numerik
    deskriptif <- data.frame(
      Variabel = gsub("\\.", " ", names(df_num)),
      Mean = round(sapply(df_num, mean, na.rm = TRUE), 3),
      Std_Dev = round(sapply(df_num, sd, na.rm = TRUE), 3),
      Min = round(sapply(df_num, min, na.rm = TRUE), 3),
      Q1 = round(sapply(df_num, quantile, probs = 0.25, na.rm = TRUE), 3),
      Median = round(sapply(df_num, median, na.rm = TRUE), 3),
      Q3 = round(sapply(df_num, quantile, probs = 0.75, na.rm = TRUE), 3),
      Max = round(sapply(df_num, max, na.rm = TRUE), 3),
      Skewness = round(sapply(df_num, function(x) {
        if(length(x[!is.na(x)]) > 3) {
          moments::skewness(x, na.rm = TRUE)
        } else {
          NA
        }
      }), 3)
    )
    deskriptif
  })
  
  # Distribusi Data (Histogram)
  output$plot_distribusi <- renderPlot({
    req(data_mentah())
    df <- data_mentah()
    
    # Ambil kolom numerik saja dan konversi
    df_num <- df[, -1, drop = FALSE]
    df_num <- as.data.frame(lapply(df_num, function(x) suppressWarnings(as.numeric(as.character(x)))))
    
    # Hapus kolom yang tidak valid
    valid_cols <- !sapply(df_num, function(x) all(is.na(x)))
    df_num <- df_num[, valid_cols, drop = FALSE]
    
    # Jika tidak ada data valid, tampilkan pesan
    if(ncol(df_num) == 0) {
      plot.new()
      text(0.5, 0.5, "Tidak ada data numerik valid untuk histogram", cex = 1.2)
      return()
    }
    
    df_long <- df_num %>% pivot_longer(cols = everything(), names_to = "Variabel", values_to = "Nilai")
    
    ggplot(df_long, aes(x = Nilai, fill = Variabel)) +
      geom_histogram(bins = 20, alpha = 0.7, color = "black") +
      facet_wrap(~Variabel, scales = "free") +
      theme_bw() +
      labs(title = "Distribusi Data per Variabel", x = "Nilai", y = "Frekuensi") +
      theme(legend.position = "none", strip.text = element_text(size = 10))
  })
  
  # Uji Missing Data
  output$tabel_missing <- renderTable({
    req(data_mentah())
    df <- data_mentah()
    missing_count <- colSums(is.na(df))
    missing_df <- data.frame(
      Variabel = names(missing_count),
      Jumlah_Missing = missing_count,
      Persentase = round(missing_count / nrow(df) * 100, 2)
    )
    missing_df
  })
  
  output$ringkasan_missing <- renderPrint({
    req(data_mentah())
    df <- data_mentah()
    total_missing <- sum(is.na(df))
    cat("Total Missing Data:", total_missing, "\n")
    cat("Total Sel Data:", nrow(df) * ncol(df), "\n")
    if(total_missing == 0){
      cat("\n✓ Tidak ada missing data. Dataset siap dianalisis.\n")
    } else {
      cat("\n⚠ Ada", total_missing, "sel dengan data kosong. Pertimbangkan untuk drop atau imputasi.\n")
    }
  })
  
  # Data Terstandarisasi (Z-Score)
  output$tabel_scaled <- renderDT({
    req(data_scaled())
    scaled_df <- as.data.frame(data_scaled())
    colnames(scaled_df) <- tools::toTitleCase(gsub("\\.", " ", colnames(scaled_df)))
    datatable(scaled_df, options = list(pageLength = 5))
  })
  
  # ================= FASE 2: UJI ASUMSI =================
  # Uji KMO (Kaiser-Meyer-Olkin)
  output$hasil_kmo <- renderPrint({
    req(data_mentah())
    
    # Buat data khusus untuk KMO - pastikan hanya numerik
    df <- data_mentah()
    df_num <- df[, -1, drop = FALSE]  # Skip kolom pertama (nama wilayah)
    
    # Konversi ke numeric dan bersihkan
    df_num <- as.data.frame(lapply(df_num, function(x) {
      suppressWarnings(as.numeric(as.character(x)))
    }))
    
    # Hapus kolom yang sepenuhnya NA
    valid_cols <- !sapply(df_num, function(x) all(is.na(x)))
    df_num <- df_num[, valid_cols, drop = FALSE]
    
    # Hapus kolom dengan variance 0 (semua nilai sama)
    var_cols <- sapply(df_num, function(x) var(x, na.rm = TRUE) > 0)
    df_num <- df_num[, var_cols, drop = FALSE]
    
    # Cek minimal 2 kolom numerik
    if(ncol(df_num) < 2) {
      cat("❌ ERROR: Minimal dibutuhkan 2 kolom data numerik dengan variasi untuk uji KMO\n")
      cat("Kolom yang ditemukan:", ncol(df_num), "\n")
      cat("Solusi: Pastikan dataset memiliki minimal 2 kolom data numerik dengan nilai yang bervariasi\n")
      return()
    }
    
    # Hapus baris yang mengandung NA untuk KMO
    df_clean <- na.omit(df_num)
    
    # Pastikan semua nilai finite
    if(any(!sapply(df_clean, function(x) all(is.finite(x))))) {
      cat("❌ ERROR: Data mengandung nilai tidak finite (Inf, -Inf, atau NaN)\n")
      cat("Solusi: Periksa dan bersihkan data dari nilai ekstrem atau tidak valid\n")
      return()
    }
    
    if(nrow(df_clean) < ncol(df_clean) + 1) {
      cat("❌ ERROR: Data tidak cukup untuk uji KMO\n")
      cat("Jumlah observasi setelah menghapus NA:", nrow(df_clean), "\n")
      cat("Jumlah variabel:", ncol(df_clean), "\n")
      cat("Solusi: Pastikan jumlah observasi > jumlah variabel, atau isi missing values\n")
      return()
    }
    
    # Pasang package psych untuk KMO
    if (!require(psych, quietly = TRUE)) {
      cat("❌ Package 'psych' dibutuhkan. Install dengan: install.packages('psych')\n")
      return()
    }
    
    # Fungsi KMO manual (implementasi dari Python)
    calculate_kmo_manual <- function(df_input) {
      # Menghitung matriks korelasi
      corr <- cor(df_input, use = "complete.obs")
      
      # Menghitung inverse matriks korelasi dengan penanganan singular matrix
      inv_corr <- tryCatch(
        solve(corr),
        error = function(e) solve(corr + diag(ncol(corr)) * 1e-6)
      )
      
      n <- ncol(corr)
      partial_corr <- matrix(0, n, n)
      
      # Menghitung matriks korelasi parsial
      for (i in 1:n) {
        for (j in 1:n) {
          partial_corr[i, j] <- -inv_corr[i, j] / sqrt(inv_corr[i, i] * inv_corr[j, j])
        }
      }
      
      # Menghitung KMO Total
      corr_sq_sum <- sum(corr^2) - sum(diag(corr)^2)
      partial_corr_sq_sum <- sum(partial_corr^2) - sum(diag(partial_corr)^2)
      kmo_total <- corr_sq_sum / (corr_sq_sum + partial_corr_sq_sum)
      
      # Menghitung MSA per variabel
      msa_list <- numeric(n)
      for (i in 1:n) {
        num <- sum(corr[i, ]^2) - corr[i, i]^2
        den <- num + (sum(partial_corr[i, ]^2) - partial_corr[i, i]^2)
        msa_list[i] <- num / den
      }
      
      return(list(overall = kmo_total, MSA = msa_list))
    }
    
    tryCatch({
      # Hitung matriks korelasi
      matriks_korelasi <- cor(df_clean, use = "complete.obs")
      
      # Cek apakah matriks korelasi valid
      if(any(is.na(matriks_korelasi)) || any(!is.finite(matriks_korelasi))) {
        cat("❌ ERROR: Tidak dapat menghitung matriks korelasi\n")
        cat("Penyebab: Data mengandung nilai tidak valid\n")
        return()
      }
      
      # Gunakan implementasi KMO manual untuk menghindari error psych::KMO
      kmo_result <- calculate_kmo_manual(df_clean)
      
      cat("Hasil Uji KMO (Kaiser-Meyer-Olkin):\n")
      cat("=====================================\n")
      cat("Overall KMO Statistic:", round(kmo_result$overall, 4), "\n")
      cat("Jumlah Observasi yang Digunakan:", nrow(df_clean), "\n")
      cat("Jumlah Variabel:", ncol(df_clean), "\n\n")
      
      cat("MSA (Measure of Sampling Adequacy) per Variabel:\n")
      cat("--------------------------------------------------\n")
      msa_df <- data.frame(
        Variabel = names(df_clean),
        MSA_Score = round(kmo_result$MSA, 4)
      )
      print(msa_df)
      cat("\n")
      
      cat("Interpretasi Overall KMO:\n")
      if(kmo_result$overall > 0.5){
        cat("✓ LAYAK untuk analisis Cluster (KMO > 0.5)\n")
      } else {
        cat("✗ TIDAK LAYAK untuk analisis Cluster (KMO ≤ 0.5)\n")
      }
      
      # Tambahan informasi jika ada missing values
      if(nrow(df_clean) != nrow(df_num)) {
        cat("\n⚠️ CATATAN: ", nrow(df_num) - nrow(df_clean), " observasi dengan missing values dihapus untuk perhitungan KMO\n")
      }
      
    }, error = function(e) {
      cat("❌ ERROR saat menghitung KMO:\n")
      cat("Pesan error:", e$message, "\n\n")
      cat("Debugging info:\n")
      cat("- Jumlah kolom asli:", ncol(df), "\n")
      cat("- Jumlah kolom numerik:", ncol(df_num), "\n")
      cat("- Jumlah observasi sebelum clean:", nrow(df_num), "\n")
      cat("- Jumlah observasi setelah clean:", nrow(df_clean), "\n\n")
      cat("Solusi:\n")
      cat("1. Pastikan kolom data berisi angka saja (bukan teks)\n")
      cat("2. Periksa dan isi missing values jika ada\n")
      cat("3. Minimal 2 kolom data numerik diperlukan\n")
      cat("4. Gunakan dataset contoh untuk testing\n")
    })
  })
  
  # ================= FASE 3: ANALISIS Cluster =================
  # Statistik Cluster
  output$statistik_Cluster <- renderPrint({
    req(hasil_model(), data_mentah())
    df <- data_mentah()
    
    if(input$metode %in% c("kmedoids", "kmeans")){
      cluster_label <- hasil_model()$cluster
      n_clusters <- length(unique(cluster_label))
    } else {
      cluster_label <- hasil_model()
      n_clusters <- length(unique(cluster_label))
    }
    
    cat("STATISTIK HASIL Cluster\n")
    cat("=======================\n")
    cat("Metode Cluster:", input$metode, "\n")
    k_final <- if(input$auto_k) k_optimal() else input$k_input
    cat("Jumlah Cluster:", k_final, "\n")
    cat("Jumlah Observasi:", nrow(df), "\n\n")
    cat("Distribusi Anggota per Cluster:\n")
    tabel_dist <- table(cluster_label)
    print(tabel_dist)
    cat("\n")
  })
  
  # ================= FASE 4: VALIDASI MODEL =================
  # Silhouette Plot (Evaluasi per Observasi)
  output$plot_silhouette_score <- renderPlot({
    req(hasil_model(), data_scaled())
    
    # Cek apakah data valid
    scaled_data <- data_scaled()
    model <- hasil_model()
    
    if(is.null(scaled_data) || is.null(model)) {
      plot.new()
      text(0.5, 0.5, "Data atau model tidak valid untuk evaluasi silhouette", cex = 1.2)
      return()
    }
    
    tryCatch({
      if(input$metode == "kmedoids"){
        fviz_silhouette(model)
      } else if(input$metode == "kmeans"){
        sil_km <- silhouette(model$cluster, dist(scaled_data))
        fviz_silhouette(sil_km)
      } else {
        # Untuk hierarchical, tampilkan dendrogram dengan silhouette
        res.hc <- hclust(dist(scaled_data), method = "ward.D2")
        fviz_dend(res.hc, k = input$k_input, rect = TRUE)
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error membuat plot silhouette:\n", e$message), cex = 1.0)
    })
  })
  
  # Uji ANOVA
  output$hasil_anova <- renderPrint({
    req(hasil_model(), data_mentah())
    df_hasil <- data_mentah()
    
    # Ambil label Cluster
    if(input$metode %in% c("kmedoids", "kmeans")){
      Cluster_label <- as.factor(hasil_model()$cluster)
    } else {
      Cluster_label <- as.factor(hasil_model())
    }
    
    # Lakukan uji ANOVA untuk semua variabel numerik
    variabel_numerik <- names(df_hasil)[-1]
    
    cat("UJI ONE-WAY ANOVA (Semua Variabel)\n")
    cat("===================================\n\n")
    
    all_p_values <- c()
    
    for(var in variabel_numerik){
      model_anova <- aov(df_hasil[[var]] ~ Cluster_label)
      anova_summary <- summary(model_anova)[[1]]
      p_val <- anova_summary[["Pr(>F)"]][1]
      all_p_values <- c(all_p_values, p_val)
      
      cat("Variabel:", var, "\n")
      cat("F-Statistic:", round(anova_summary[["F value"]][1], 4), "\n")
      cat("P-Value:", round(p_val, 6), "\n")
      if(p_val < 0.05){
        cat("Status: ✓ SIGNIFIKAN\n")
      } else {
        cat("Status: ✗ TIDAK SIGNIFIKAN\n")
      }
      cat("---\n")
    }
    
    cat("\nKESIMPULAN UMUM:\n")
    sig_count <- sum(all_p_values < 0.05)
    total_var <- length(all_p_values)
    cat("Dari", total_var, "variabel, terdapat", sig_count, "variabel dengan perbedaan signifikan antar Cluster.\n")
    if(sig_count > 0){
      cat("✓ Model Cluster VALID - Terdapat perbedaan karakteristik yang signifikan antar Cluster.\n")
    } else {
      cat("✗ Model Cluster TIDAK VALID - Tidak ditemukan perbedaan signifikan antar Cluster.\n")
    }
  })
  
  # Analisis Diskriminan
  output$hasil_diskriminan <- renderPrint({
    req(hasil_model(), data_mentah())
    
    # Persiapan data
    df <- data_mentah()
    df_num <- df[, -1, drop = FALSE]
    df_num <- as.data.frame(lapply(df_num, function(x) suppressWarnings(as.numeric(as.character(x)))))
    
    if(input$metode %in% c("kmedoids", "kmeans")){
      Cluster_label <- as.factor(hasil_model()$cluster)
    } else {
      Cluster_label <- as.factor(hasil_model())
    }
    
    df_analisis <- cbind(df_num, Cluster = Cluster_label)
    df_analisis <- as.data.frame(df_analisis)
    
    cat("ANALISIS DISKRIMINAN (LDA - Linear Discriminant Analysis)\n")
    cat("===========================================================\n\n")
    
    # Cek if library MASS tersedia untuk LDA
    if (!require(MASS, quietly = TRUE)) {
      cat("Package 'MASS' dibutuhkan. Silakan install: install.packages('MASS')\n")
    } else {
      # Fit LDA model
      lda_model <- lda(Cluster ~ ., data = df_analisis)
      
      # Prediksi dan akurasi
      pred_lda <- predict(lda_model, df_analisis)
      
      # Confusion Matrix
      confusion_matrix <- table(Prediksi = pred_lda$class, Aktual = Cluster_label)
      
      cat("CONFUSION MATRIX:\n")
      cat("==================\n")
      print(confusion_matrix)
      cat("\n")
      
      # Menghitung CCR (Classification Correct Rate) dan APER (Apparent Error Rate)
      diagonal_sum <- sum(diag(confusion_matrix))
      total_observasi <- sum(confusion_matrix)
      off_diagonal_sum <- total_observasi - diagonal_sum
      
      ccr <- (diagonal_sum / total_observasi) * 100
      aper <- (off_diagonal_sum / total_observasi) * 100
      
      cat("PERHITUNGAN KETEPATAN KLASIFIKASI:\n")
      cat("===================================\n")
      cat("CCR (Classification Correct Rate):\n")
      cat("  = ", diagonal_sum, " / ", total_observasi, " × 100%\n")
      cat("  = ", round(ccr, 2), "%\n\n")
      
      cat("APER (Apparent Error Rate):\n")
      cat("  = ", off_diagonal_sum, " / ", total_observasi, " × 100%\n")
      cat("  = ", round(aper, 2), "%\n\n")
      
      cat("KESIMPULAN:\n")
      cat("===========\n")
      cat("Metode:", toupper(input$metode), "\n")
      cat("Ketepatan Klasifikasi (CCR):", round(ccr, 2), "%\n")
      cat("Tingkat Kesalahan (APER):", round(aper, 2), "%\n\n")
      
      if(ccr >= 95){
        cat("★★★ TINGKAT AKURASI SANGAT TINGGI (≥95%)\n")
        cat("→ Model sangat akurat dalam mengklasifikasikan observasi ke Cluster yang benar.\n")
      } else if(ccr >= 90){
        cat("★★★ TINGKAT AKURASI TINGGI (90-95%)\n")
        cat("→ Model akurat dan dapat diandalkan untuk pengklasifikasian.\n")
      } else if(ccr >= 80){
        cat("★★  TINGKAT AKURASI BAIK (80-90%)\n")
        cat("→ Model cukup baik, namun ada beberapa kesalahan klasifikasi.\n")
      } else if(ccr >= 70){
        cat("★   TINGKAT AKURASI CUKUP (70-80%)\n")
        cat("→ Model memiliki akurasi yang dapat diterima.\n")
      } else {
        cat("⚠   TINGKAT AKURASI RENDAH (<70%)\n")
        cat("→ Model perlu perbaikan atau pertimbangan kembali.\n")
      }
      
      cat("\n\nVariabel Pembeda Terkuat (Canonical Discriminant Function):\n")
      cat("-----------------------------------------------------------\n")
      scaling_df <- as.data.frame(as.matrix(lda_model$scaling))
      colnames(scaling_df) <- "Koefisien"
      print(scaling_df)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

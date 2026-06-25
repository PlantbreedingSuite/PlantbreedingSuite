######################################################################
##  COMBINED PLANT BREEDING ANALYTICS SUITE
##  Module 1 : D² Genetic Diversity Analyser
##  Module 2 : MET Analysis (AMMI / GGE / Stability)
##  Module 3 : Multi-Trait Selection Index Suite
##  Developer : Dr. Vijay Kamal Meena
##  Agriculture University Jodhpur | ICAR-ARS 2021
##  vjkamal93@gmail.com | +91 9449509856
######################################################################

`%||%` <- function(a, b) if (!is.null(a)) a else b

pkgs <- c(
  "shiny","shinydashboard","shinyWidgets","shinyjs","DT","plotly",
  "ggplot2","dplyr","tidyverse","readxl","writexl","reshape2",
  "RColorBrewer","ggrepel","scales","viridis","factoextra","FactoMineR",
  "Hmisc","shinycssloaders","biotools","ggdendro","dendextend",
  "metan","corrplot","ggforce","patchwork","tibble","purrr","fmsb","grDevices"
)
new_pkg <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new_pkg)) install.packages(new_pkg, dependencies = TRUE)
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

# ════════════════════════════════════════════════════════════════
#  SHARED GLOBALS
# ════════════════════════════════════════════════════════════════
MET_COLORS <- c(
  "#2D6A4F","#40916C","#52B788","#74C69D","#95D5B2",
  "#1565C0","#1976D2","#42A5F5","#F4A300","#E65100",
  "#6A1B9A","#AD1457","#C62828","#00695C","#F57F17",
  "#33691E","#0277BD","#6D4C41","#4A148C","#BF360C"
)

theme_met <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.title       = element_text(face="bold", size=base_size+2, color="#0D3B2E", margin=margin(b=8)),
      plot.subtitle    = element_text(color="#2D6A4F", size=base_size-1),
      axis.title       = element_text(face="bold", color="#1B4332"),
      axis.text        = element_text(color="#2D3436", size=base_size-1),
      panel.border     = element_rect(color="#2D6A4F", linewidth=0.7),
      panel.grid.major = element_line(color="#D8EDE1", linewidth=0.3),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill="#2D6A4F"),
      strip.text       = element_text(color="white", face="bold"),
      legend.background= element_rect(fill="#F7FBF8", color="#B7E4C7"),
      legend.key       = element_rect(fill="#F7FBF8"),
      plot.background  = element_rect(fill="white", color=NA),
      panel.background = element_rect(fill="#FAFEFE"),
      plot.caption     = element_text(color="#555", size=9, face="italic")
    )
}

save_pdf  <- function(p, file, w=16, h=10) { grDevices::cairo_pdf(file,width=w,height=h); print(p); grDevices::dev.off() }
safe_df   <- function(x) tryCatch(as.data.frame(x), error=function(e) data.frame(Error=e$message))
fmt_dt    <- function(df, pg=15) {
  datatable(df, rownames=FALSE, filter="top", extensions="Buttons",
            options=list(scrollX=TRUE, pageLength=pg, dom="Bfrtip",
                         buttons=list("copy","csv","excel"),
                         initComplete=JS("function(s,j){$(this.api().table().header()).css({'background':'#2D6A4F','color':'white'});}"))) %>%
    formatRound(which(sapply(df, is.numeric)), 3)
}
metan_df  <- function(obj, var_name=NULL) {
  if (is.null(obj)) return(data.frame(Info="No data"))
  x <- obj
  if (!is.null(var_name) && is.list(obj) && !is.data.frame(obj) && var_name %in% names(obj)) x <- obj[[var_name]]
  if (is.list(x) && !is.data.frame(x) && length(x)>0) x <- x[["general"]] %||% x[[1]]
  if (is.null(x)) return(data.frame(Info="No data returned"))
  tryCatch(data.frame(as.list(x), check.names=FALSE, stringsAsFactors=FALSE),
           error=function(e) tryCatch(as.data.frame(x), error=function(e2) data.frame(Error=e2$message)))
}
dl_bar <- function(btn_id, label="⬇  Download HD PDF") {
  div(style="background:#F7FBF8;border:1px solid #D8EDE1;border-radius:8px;padding:7px 12px;margin-bottom:10px;display:flex;align-items:center;gap:10px;",
      span(style="font-size:12px;color:#2D6A4F;font-weight:700;","Export:"),
      downloadButton(btn_id, label, class="btn-primary btn-sm"))
}

# ════════════════════════════════════════════════════════════════
#  COMBINED CSS
# ════════════════════════════════════════════════════════════════
APP_CSS <- tags$style(HTML("
@import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&family=Source+Sans+3:wght@400;600;700&display=swap');
body,.content-wrapper,label,.tab-content,p,td,th{font-family:'Source Sans 3',sans-serif;}
.main-header .logo{font-family:'Playfair Display',serif;}
.main-header .logo{background:linear-gradient(135deg,#071F14,#1B4332)!important;color:#74C69D!important;font-size:12px!important;line-height:1.25!important;padding:8px 14px!important;border-bottom:2px solid #52B788!important;}
.main-header .navbar{background:linear-gradient(90deg,#1B4332,#2D6A4F)!important;border-bottom:2px solid #52B788!important;}
.main-header .navbar .sidebar-toggle{color:#B7E4C7!important;font-size:20px;}
.main-sidebar{background:linear-gradient(180deg,#071F14,#0D3B2E 60%,#1B4332)!important;box-shadow:3px 0 15px rgba(0,0,0,.3)!important;}
.sidebar-menu>li>a{color:#95D5B2!important;font-size:12.5px!important;font-weight:600!important;padding:9px 15px 9px 18px!important;border-left:3px solid transparent;transition:all .2s;}
.sidebar-menu>li:hover>a,.sidebar-menu>li.active>a{background:rgba(82,183,136,.18)!important;border-left:3px solid #52B788!important;color:#fff!important;}
.sidebar-menu>li>a>.fa{color:#52B788!important;width:20px;text-align:center;}
.sidebar-menu .treeview-menu{background:rgba(0,0,0,.25)!important;}
.sidebar-menu .treeview-menu>li>a{color:#74C69D!important;font-size:11.5px!important;font-weight:400!important;padding:6px 10px 6px 30px!important;}
.sidebar-menu .treeview-menu>li.active>a,.sidebar-menu .treeview-menu>li:hover>a{color:#fff!important;background:rgba(82,183,136,.2)!important;}
.suite-divider{background:rgba(82,183,136,.15);border-top:1px solid rgba(82,183,136,.25)!important;padding:5px 15px!important;font-size:10px!important;color:#52B788!important;font-weight:700!important;letter-spacing:1px!important;text-transform:uppercase!important;}
.content-wrapper,.right-side{background:#EBF3EE!important;}
.content{padding:14px 16px;}
.box{border-radius:10px!important;box-shadow:0 3px 14px rgba(0,0,0,.09)!important;border:none!important;overflow:hidden;}
.box-header{background:linear-gradient(90deg,#1B4332,#2D6A4F 60%,#40916C)!important;padding:11px 16px!important;border-radius:10px 10px 0 0!important;}
.box-title{color:#D8F3DC!important;font-weight:700!important;font-size:13.5px!important;}
.box-header .box-tools .btn{color:#B7E4C7!important;}
.box-body{background:white;padding:14px;}
.nav-tabs{border-bottom:2px solid #B7E4C7!important;margin-bottom:12px;}
.nav-tabs>li>a{color:#2D6A4F!important;font-weight:600!important;font-size:11.5px;border-radius:7px 7px 0 0!important;padding:6px 13px;transition:all .2s;}
.nav-tabs>li>a:hover{background:#D8F3DC!important;color:#1B4332!important;}
.nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{background:linear-gradient(135deg,#2D6A4F,#52B788)!important;color:white!important;border-color:transparent!important;border-radius:7px 7px 0 0!important;box-shadow:0 -2px 8px rgba(45,106,79,.25);}
.btn{border-radius:7px!important;font-weight:600!important;letter-spacing:.2px;transition:all .2s;}
.btn-success{background:linear-gradient(135deg,#2D6A4F,#40916C)!important;border:none!important;color:white!important;}
.btn-success:hover{background:linear-gradient(135deg,#1B4332,#2D6A4F)!important;transform:translateY(-1px);box-shadow:0 4px 12px rgba(45,106,79,.35)!important;}
.btn-warning{background:linear-gradient(135deg,#E65100,#F4A300)!important;border:none!important;color:white!important;}
.btn-warning:hover{background:linear-gradient(135deg,#BF360C,#E65100)!important;transform:translateY(-1px);}
.btn-primary{background:linear-gradient(135deg,#0D47A1,#1565C0)!important;border:none!important;color:white!important;}
.btn-primary:hover{background:linear-gradient(135deg,#08306b,#0D47A1)!important;transform:translateY(-1px);}
.btn-info{background:linear-gradient(135deg,#006064,#00838F)!important;border:none!important;color:white!important;}
.btn-block{margin-bottom:5px;}
.small-box{border-radius:10px!important;box-shadow:0 4px 14px rgba(0,0,0,.13)!important;}
.small-box.bg-green{background:linear-gradient(135deg,#1B4332,#2D6A4F)!important;}
.small-box.bg-teal{background:linear-gradient(135deg,#004D40,#00695C)!important;}
.small-box.bg-orange{background:linear-gradient(135deg,#BF360C,#E65100)!important;}
.small-box.bg-purple{background:linear-gradient(135deg,#311B92,#4527A0)!important;}
.small-box h3,.small-box p{font-weight:700!important;}
.dev-card{background:linear-gradient(135deg,#071F14,#0D3B2E 35%,#1B4332 65%,#2D6A4F);border-radius:14px;padding:24px 26px;color:white;box-shadow:0 8px 28px rgba(0,0,0,.3);border:1px solid rgba(82,183,136,.3);position:relative;overflow:hidden;}
.dev-card .dev-name{font-family:'Playfair Display',serif;font-size:20px;color:#95D5B2;margin-bottom:4px;}
.dev-card .dev-role{font-size:13px;color:#74C69D;margin-bottom:16px;font-weight:600;}
.dev-card .cr{display:flex;align-items:flex-start;gap:10px;padding:4px 0;font-size:12.5px;color:#D8F3DC;}
.dev-card .cr .fa{color:#52B788;margin-top:2px;min-width:16px;}
.dev-card .tags{margin-top:14px;}
.dev-card .tag{background:rgba(82,183,136,.2);border:1px solid rgba(82,183,136,.45);color:#B7E4C7;border-radius:20px;padding:3px 11px;font-size:10.5px;display:inline-block;margin:3px 2px;font-weight:600;}
.sec-bar{background:linear-gradient(90deg,#0D3B2E,#1B4332 40%,#2D6A4F);color:#D8F3DC;padding:9px 16px;border-radius:8px;font-weight:700;font-size:14px;margin-bottom:14px;box-shadow:0 3px 10px rgba(13,59,46,.35);}
.home-banner{background:linear-gradient(135deg,#071F14,#0D3B2E 40%,#2D6A4F 75%,#52B788);border-radius:14px;padding:30px;color:white;text-align:center;margin-bottom:20px;box-shadow:0 8px 28px rgba(0,0,0,.22);}
.home-banner h2{font-family:'Playfair Display',serif;font-size:26px;color:#B7E4C7;margin:0 0 8px;}
.home-banner p{font-size:14px;color:#D8F3DC;margin:0;}
.home-banner .vtag{display:inline-block;background:rgba(82,183,136,.25);border:1px solid #52B788;color:#95D5B2;border-radius:20px;padding:4px 16px;font-size:11px;margin-top:10px;font-weight:700;letter-spacing:1px;}
.feat-card{background:white;border-radius:10px;padding:16px;border-left:4px solid #2D6A4F;box-shadow:0 2px 10px rgba(0,0,0,.07);height:100%;transition:all .25s;min-height:100px;}
.feat-card:hover{transform:translateY(-3px);box-shadow:0 8px 20px rgba(45,106,79,.18);border-left-color:#F4A300;}
.feat-card .fi{font-size:26px;margin-bottom:6px;}
.feat-card h4{color:#1B4332;margin:0 0 5px;font-size:13px;font-weight:700;}
.feat-card p{color:#636e72;font-size:11.5px;margin:0;line-height:1.5;}
.well{background:#F7FBF8!important;border:1px solid #D8EDE1!important;border-radius:8px!important;}
.goal-grid{display:grid;grid-template-columns:1fr 1fr;gap:7px;margin-top:6px;}
.goal-row{background:#F7FBF8;border:1px solid #D8EDE1;border-radius:8px;padding:7px 11px;display:flex;align-items:center;justify-content:space-between;gap:8px;}
.goal-row .trait-name{font-weight:700;color:#1B4332;font-size:12px;min-width:60px;}
.app-footer{text-align:center;padding:9px 14px;font-size:11px;color:#555;background:white;border-radius:8px;margin-top:14px;border-top:3px solid #B7E4C7;box-shadow:0 2px 8px rgba(0,0,0,.06);}
.d2-panel{background:#161b22;border:1px solid #30363d;border-radius:10px;padding:20px;margin-bottom:16px;}
.d2-panel-title{font-family:'Playfair Display',serif;font-size:14px;color:#f0f6fc;margin-bottom:12px;padding-bottom:8px;border-bottom:1px solid #30363d;display:flex;align-items:center;gap:8px;}
.d2-stat-row{display:flex;gap:10px;flex-wrap:wrap;margin-bottom:16px;}
.d2-stat-box{flex:1;min-width:110px;background:#0d1117;border:1px solid #30363d;border-radius:8px;padding:12px 14px;text-align:center;}
.d2-stat-num{font-family:'Playfair Display',serif;font-size:26px;color:#56d364;line-height:1;}
.d2-stat-lbl{font-size:10px;color:#7d8590;margin-top:3px;text-transform:uppercase;letter-spacing:.5px;}
.d2-alert{background:rgba(35,134,54,.08);border:1px solid rgba(35,134,54,.25);border-radius:7px;padding:10px 14px;font-size:12.5px;color:#7ee787;margin-bottom:14px;}
.tab-content-d2{background:#0d1117;color:#e6edf3;}
::-webkit-scrollbar{width:5px;height:5px;}
::-webkit-scrollbar-track{background:#F0F4F0;}
::-webkit-scrollbar-thumb{background:#74C69D;border-radius:3px;}
::-webkit-scrollbar-thumb:hover{background:#2D6A4F;}
.dataTables_wrapper{font-size:12px;}
.selectize-control.single .selectize-input{border-color:#B7E4C7!important;border-radius:6px!important;}
.form-control:focus{border-color:#52B788!important;box-shadow:0 0 0 2px rgba(82,183,136,.25)!important;}
"))

# ════════════════════════════════════════════════════════════════
#  D² MODULE — HELPERS
# ════════════════════════════════════════════════════════════════
d2_make_pal <- function(n) colorRampPalette(c("#e63946","#2196f3","#4caf50","#ff9800","#9c27b0","#00bcd4","#f06292","#8d6e63","#607d8b","#cddc39","#ff5722","#009688","#3f51b5","#795548","#ffc107"))(n)
d2_dt_opts  <- list(pageLength=10, scrollX=TRUE, dom='Bfrtip', buttons=c('csv','excel'),
                    initComplete=JS("function(settings,json){$(this.api().table().container()).css({'background':'transparent','color':'#c9d1d9'});}"))

d2_build_scree <- function(pca_res, eig_val) {
  ev <- eig_val; npc <- min(nrow(ev),10)
  df <- data.frame(PC=paste0("PC",seq_len(npc)), Var=ev[1:npc,2], Cum=ev[1:npc,3])
  ggplot(df, aes(x=PC, y=Var)) +
    geom_bar(stat="identity", fill="#238636", colour="#56d364", linewidth=.4) +
    geom_line(aes(group=1), colour="#f39c12", linewidth=1, linetype="dashed") +
    geom_point(colour="#f39c12", size=3) +
    geom_text(aes(label=paste0(round(Var,1),"%")), vjust=-.6, size=3, colour="#c9d1d9") +
    scale_x_discrete(limits=df$PC) +
    labs(title="Scree Plot", x="Principal Component", y="Variance Explained (%)") +
    theme_minimal(base_family="serif") +
    theme(plot.background=element_rect(fill="white",colour=NA), panel.grid=element_line(colour="grey90"),
          axis.text=element_text(colour="grey30"), plot.title=element_text(face="bold",size=13,hjust=.5))
}

d2_build_biplot <- function(pca_res, eig_val) {
  sc  <- max(abs(pca_res$x[,1:2]))/max(abs(pca_res$rotation[,1:2]))*.7
  ind <- data.frame(x=pca_res$x[,1], y=pca_res$x[,2], label=rownames(pca_res$x))
  vr  <- data.frame(x=pca_res$rotation[,1]*sc, y=pca_res$rotation[,2]*sc, label=rownames(pca_res$rotation))
  ggplot() +
    geom_point(data=ind, aes(x=x,y=y), colour="#2196f3", size=2.5, alpha=.85) +
    ggrepel::geom_text_repel(data=ind, aes(x=x,y=y,label=label), size=3, colour="grey30", max.overlaps=20) +
    geom_segment(data=vr, aes(x=0,y=0,xend=x,yend=y), arrow=arrow(length=unit(.2,"cm"),type="closed"), colour="#f39c12", linewidth=.8) +
    ggrepel::geom_text_repel(data=vr, aes(x=x,y=y,label=label), size=3, colour="#f39c12", max.overlaps=20) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey80") +
    geom_vline(xintercept=0, linetype="dashed", colour="grey80") +
    labs(title="PCA Biplot — Genotypes × Traits",
         x=paste0("PC1 (",round(eig_val[1,2],1),"%)"), y=paste0("PC2 (",round(eig_val[2,2],1),"%)")) +
    theme_met()
}

d2_build_network <- function(toc, n_clust, pal) {
  nc     <- n_clust; theta <- seq(pi/2, pi/2+2*pi*(1-1/nc), length.out=nc)
  node_x <- cos(theta); node_y <- sin(theta)
  dist_m <- as.matrix(toc$distClust); intra <- diag(dist_m)
  edges <- data.frame()
  for (i in seq_len(nc-1)) for (j in (i+1):nc)
    edges <- rbind(edges, data.frame(x_from=node_x[i],y_from=node_y[i],x_to=node_x[j],y_to=node_y[j],
                                     dist=round(dist_m[i,j],2), mid_x=(node_x[i]+node_x[j])/2, mid_y=(node_y[i]+node_y[j])/2))
  nodes <- data.frame(cluster=seq_len(nc),x=node_x,y=node_y,intra=round(intra,2),
                      color=pal,lx=node_x*1.22,ly=node_y*1.22,stringsAsFactors=FALSE)
  ggplot() + theme_void() +
    theme(plot.background=element_rect(fill="white",colour=NA), plot.title=element_text(face="bold",size=13,hjust=.5,family="serif"),
          plot.caption=element_text(colour="grey50",size=8,hjust=.5)) +
    geom_segment(data=edges, aes(x=x_from,y=y_from,xend=x_to,yend=y_to), colour="grey70", linewidth=.7) +
    geom_label(data=edges, aes(x=mid_x,y=mid_y,label=dist), size=2.4, fill="white", colour="grey40",
               label.padding=unit(.1,"lines"), label.size=.2) +
    geom_point(data=nodes, aes(x=x,y=y), size=18, colour=nodes$color, alpha=.2) +
    geom_point(data=nodes, aes(x=x,y=y), size=12, colour=nodes$color, fill=nodes$color, shape=21, stroke=2) +
    geom_text(data=nodes, aes(x=x,y=y,label=cluster), size=4.5, fontface="bold", colour="white") +
    geom_label(data=nodes, aes(x=lx,y=ly,label=paste0("C",cluster,"\n",intra)), size=2.8,
               fill=nodes$color, colour="white", fontface="bold", label.padding=unit(.2,"lines"), label.size=0) +
    coord_equal(xlim=c(-1.6,1.6), ylim=c(-1.6,1.6)) +
    labs(title=paste0("Inter/Intra-cluster Distances — ",nc," Tocher Clusters"),
         caption="Node: Cluster / Intra dist  •  Edge: Inter-cluster distance")
}

d2_draw_dendro <- function(dm, n_clust, pal) {
  hc <- hclust(as.dist(dm), method="ward.D2")
  dend <- as.dendrogram(hc) %>%
    dendextend::color_branches(k=n_clust, col=pal) %>%
    dendextend::color_labels(k=n_clust, col=pal) %>%
    dendextend::set("labels_cex", .72) %>%
    dendextend::set("branches_lwd", 2)
  par(bg="white", mar=c(6,5,4,2))
  plot(dend, main="Cluster Dendrogram — Ward's D² Linkage", ylab="Height (D² distance)", cex.main=1.2)
  dendextend::rect.dendrogram(dend, k=n_clust, border=pal, lty=2, lwd=1.8)
  legend("topright", legend=paste("Cluster",seq_len(n_clust)), fill=pal, border=NA, bty="n", cex=.7,
         title=expression(bold("Clusters")))
}

d2_build_corr_heatmap <- function(cor_mat, p_mat, palette) {
  sig <- ifelse(is.na(p_mat),"", ifelse(p_mat<=.001,"***", ifelse(p_mat<=.01,"**", ifelse(p_mat<=.05,"*",""))))
  df  <- reshape2::melt(cor_mat); names(df) <- c("Trait1","Trait2","r")
  df$sig   <- reshape2::melt(sig)$value
  df$label <- paste0(round(df$r,2), df$sig)
  cs <- switch(palette,
    rwb    = scale_fill_gradient2(low="#053061", mid="white", high="#67001f", midpoint=0, limits=c(-1,1), name="r"),
    pastel = scale_fill_gradient2(low="#A6CEE3", mid="white", high="#FB9A99", midpoint=0, limits=c(-1,1), name="r"),
    viridis= scale_fill_viridis_c(option="viridis", limits=c(-1,1), name="r"))
  ggplot(df, aes(x=Trait2, y=Trait1, fill=r)) +
    geom_tile(colour="white", linewidth=.3) +
    geom_text(aes(label=label), size=2.8, colour="grey30") + cs +
    labs(title="Pearson Correlation Matrix", x=NULL, y=NULL) +
    theme_met() +
    theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=9), panel.grid=element_blank())
}

# ════════════════════════════════════════════════════════════════
#  D² MODULE — UI
# ════════════════════════════════════════════════════════════════
d2UI <- function(id) {
  ns <- NS(id)
  tagList(
    # ── Upload ──────────────────────────────────────────────
    tabItem("d2_upload",
      div(class="sec-bar","📁 D² — Data Upload"),
      fluidRow(
        box(title="Upload Files", width=5, status="success", solidHeader=TRUE,
          div(class="d2-alert","Raw data requires columns: REP, GEN, then traits"),
          fileInput(ns("file1"), "Raw Data (CSV or Excel)", accept=c(".csv",".xlsx",".xls")),
          hr(),
          div(class="d2-alert","Genotype means requires columns: GEN, then traits"),
          fileInput(ns("file2"), "Genotype Means (CSV or Excel)", accept=c(".csv",".xlsx",".xls")),
          hr(),
          numericInput(ns("trait_start"), "Trait start column (in raw data)", 3, 1, 20),
          actionButton(ns("btn_load"), "▶  Load & Validate Data", class="btn-success btn-block", icon=icon("play"))
        ),
        box(title="Data Preview", width=7, status="success", solidHeader=TRUE,
          uiOutput(ns("data_stats")), br(),
          h5("Raw Data (first 10 rows)", style="color:#2D6A4F;font-weight:700;"),
          withSpinner(DTOutput(ns("preview1")), color="#56d364"),
          br(),
          h5("Genotype Means (first 10 rows)", style="color:#2D6A4F;font-weight:700;"),
          withSpinner(DTOutput(ns("preview2")), color="#56d364")
        )
      )
    ),
    # ── MANOVA ──────────────────────────────────────────────
    tabItem("d2_manova",
      div(class="sec-bar","📊 D² — MANOVA"),
      fluidRow(
        box(title="Multivariate ANOVA", width=12, status="success", solidHeader=TRUE,
          actionButton(ns("btn_manova"), "▶  Run MANOVA", class="btn-success", icon=icon("play")),
          br(), br(),
          withSpinner(verbatimTextOutput(ns("manova_out")), color="#52B788")
        )
      ),
      fluidRow(
        box(title="Univariate ANOVA per Trait", width=12, status="success", solidHeader=TRUE,
          withSpinner(DTOutput(ns("anova_tbl")), color="#52B788")
        )
      )
    ),
    # ── D² Distances ────────────────────────────────────────
    tabItem("d2_d2",
      div(class="sec-bar","📐 D² — Mahalanobis Distances"),
      fluidRow(
        box(title="D² Distance Matrix", width=12, status="success", solidHeader=TRUE,
          actionButton(ns("btn_d2"), "▶  Compute D² Distances", class="btn-success", icon=icon("play")),
          br(), br(),
          uiOutput(ns("d2_stats")),
          withSpinner(DTOutput(ns("d2_tbl")), color="#52B788")
        )
      ),
      fluidRow(
        box(title="D² Distance Heatmap", width=12, status="success", solidHeader=TRUE,
          dl_bar(ns("dl_heatmap_pdf")),
          withSpinner(plotlyOutput(ns("d2_heatmap"), height="550px"), color="#52B788")
        )
      )
    ),
    # ── Tocher ──────────────────────────────────────────────
    tabItem("d2_tocher",
      div(class="sec-bar","🌐 D² — Tocher Clustering"),
      fluidRow(
        box(title="Tocher Clustering", width=12, status="success", solidHeader=TRUE,
          actionButton(ns("btn_tocher"), "▶  Run Tocher", class="btn-success", icon=icon("play")),
          br(), br(), uiOutput(ns("tocher_stats"))
        )
      ),
      fluidRow(
        box(title="Cluster Membership", width=6, status="success", solidHeader=TRUE,
          withSpinner(DTOutput(ns("tocher_mem")), color="#52B788")),
        box(title="Inter/Intra-cluster Distances", width=6, status="success", solidHeader=TRUE,
          withSpinner(DTOutput(ns("tocher_dist")), color="#52B788"))
      ),
      fluidRow(
        box(title="Cluster Dendrogram", width=6, status="success", solidHeader=TRUE,
          dl_bar(ns("dl_dendro_pdf")),
          withSpinner(plotOutput(ns("dendro_plot"), height="460px"), color="#52B788")),
        box(title="Network Distance Plot", width=6, status="success", solidHeader=TRUE,
          dl_bar(ns("dl_network_pdf")),
          withSpinner(plotOutput(ns("network_plot"), height="460px"), color="#52B788"))
      ),
      fluidRow(
        box(title="Cluster-wise Trait Means", width=12, status="success", solidHeader=TRUE,
          withSpinner(DTOutput(ns("cluster_means")), color="#52B788"))
      )
    ),
    # ── PCA ─────────────────────────────────────────────────
    tabItem("d2_pca",
      div(class="sec-bar","📈 D² — Principal Component Analysis"),
      fluidRow(
        box(title="PCA", width=12, status="success", solidHeader=TRUE,
          actionButton(ns("btn_pca"), "▶  Run PCA", class="btn-success", icon=icon("play")),
          br(), br(), uiOutput(ns("pca_stats"))
        )
      ),
      fluidRow(
        box(title="Scree Plot", width=6, status="success", solidHeader=TRUE,
          dl_bar(ns("dl_scree_pdf")),
          withSpinner(plotOutput(ns("scree_plot"), height="360px"), color="#52B788")),
        box(title="PCA Biplot — Genotypes × Traits", width=6, status="success", solidHeader=TRUE,
          dl_bar(ns("dl_biplot_pdf")),
          withSpinner(plotlyOutput(ns("pca_biplot"), height="360px"), color="#52B788"))
      ),
      fluidRow(
        box(title="Eigenvalues Table", width=6, status="success", solidHeader=TRUE,
          withSpinner(DTOutput(ns("eig_tbl")), color="#52B788")),
        box(title="Variable Loadings", width=6, status="success", solidHeader=TRUE,
          withSpinner(DTOutput(ns("loading_tbl")), color="#52B788"))
      ),
      fluidRow(
        box(title="PCA with Tocher Cluster Overlay", width=12, status="success", solidHeader=TRUE,
          dl_bar(ns("dl_pca_cluster_pdf")),
          withSpinner(plotlyOutput(ns("pca_cluster"), height="480px"), color="#52B788"))
      )
    ),
    # ── Correlation ──────────────────────────────────────────
    tabItem("d2_corr",
      div(class="sec-bar","🔗 D² — Correlation Analysis"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          radioButtons(ns("corr_palette"), "Colour Scheme",
                       choices=c("Red-White-Blue"="rwb","Pastel"="pastel","Viridis"="viridis"), selected="rwb"),
          br(),
          actionButton(ns("btn_corr"), "▶  Compute Correlation", class="btn-success btn-block", icon=icon("play"))
        ),
        box(title="Pearson Correlation Heatmap", width=9, status="success", solidHeader=TRUE,
          dl_bar(ns("dl_corr_pdf")),
          withSpinner(plotlyOutput(ns("corr_heatmap"), height="500px"), color="#52B788")
        )
      ),
      fluidRow(
        box(title="Correlation Matrix (r values)", width=6, status="success", solidHeader=TRUE,
          withSpinner(DTOutput(ns("corr_tbl")), color="#52B788")),
        box(title="Significant Pairs (p ≤ 0.05)", width=6, status="success", solidHeader=TRUE,
          withSpinner(DTOutput(ns("sig_pairs_tbl")), color="#52B788"))
      )
    ),
    # ── Export ──────────────────────────────────────────────
    tabItem("d2_export",
      div(class="sec-bar","📦 D² — Export Results"),
      fluidRow(
        box(title="Download Tables (CSV)", width=6, status="success", solidHeader=TRUE,
          p(style="color:#555;font-size:12px;","Run all analyses first, then download."),
          fluidRow(
            column(6, downloadButton(ns("dl_means"),  "Genotype Means (.csv)",     class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_d2mat"),  "D² Matrix (.csv)",          class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_tocher"), "Tocher Clusters (.csv)",    class="btn-warning btn-block")),
            column(6, downloadButton(ns("dl_corr"),   "Correlation Matrix (.csv)", class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_eig"),    "PCA Eigenvalues (.csv)",    class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_loads"),  "PCA Loadings (.csv)",       class="btn-warning btn-block"))
          ),
          br(),
          fluidRow(
            column(6, downloadButton(ns("dl_scores"),        "PCA Scores (.csv)",              class="btn-warning btn-block")),
            column(6, downloadButton(ns("dl_anova"),         "ANOVA per Trait (.csv)",         class="btn-warning btn-block"))
          ),
          br(),
          fluidRow(
            column(6, downloadButton(ns("dl_cluster_means"), "Cluster-wise Trait Means (.csv)",    class="btn-warning btn-block")),
            column(6, downloadButton(ns("dl_cluster_dist"),  "Inter/Intra Cluster Distances (.csv)", class="btn-warning btn-block"))
          )
        ),
        box(title="Download Plots (PDF)", width=6, status="success", solidHeader=TRUE,
          fluidRow(
            column(6, downloadButton(ns("dl_dendro_pdf_ex"),      "Dendrogram (.pdf)",       class="btn-primary btn-block"), br(),
                      downloadButton(ns("dl_network_pdf_ex"),     "Network Plot (.pdf)",     class="btn-primary btn-block"), br(),
                      downloadButton(ns("dl_heatmap_pdf_ex"),     "D² Heatmap (.pdf)",       class="btn-primary btn-block")),
            column(6, downloadButton(ns("dl_scree_pdf_ex"),       "Scree Plot (.pdf)",       class="btn-primary btn-block"), br(),
                      downloadButton(ns("dl_biplot_pdf_ex"),      "PCA Biplot (.pdf)",       class="btn-primary btn-block"), br(),
                      downloadButton(ns("dl_corr_pdf_ex"),        "Corr. Heatmap (.pdf)",    class="btn-primary btn-block"))
          )
        )
      )
    )
  )
}

# ════════════════════════════════════════════════════════════════
#  D² MODULE — SERVER
# ════════════════════════════════════════════════════════════════
d2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(data1=NULL, data2=NULL, gm=NULL, covar=NULL,
                         d_dist=NULL, dm=NULL, toc=NULL, cluster_vec=NULL,
                         n_clust=NULL, pal=NULL, pca_res=NULL, eig_val=NULL,
                         cor_mat=NULL, p_mat=NULL, mod=NULL, dv=NULL,
                         loaded=FALSE, ran_manova=FALSE)

    read_f <- function(p) {
      ext <- tolower(tools::file_ext(p))
      if (ext %in% c("xlsx","xls")) readxl::read_excel(p) %>% as.data.frame() else read.csv(p, stringsAsFactors=FALSE)
    }

    observeEvent(input$btn_load, {
      req(input$file1, input$file2)
      tryCatch({
        d1 <- read_f(input$file1$datapath); d2 <- read_f(input$file2$datapath)
        rv$data1 <- d1; rv$data2 <- d2
        ts <- input$trait_start
        rv$gm <- aggregate(d1[ts:ncol(d1)], by=list(GEN=d1$GEN), mean)
        rv$loaded <- TRUE
        showNotification("✅ D² data loaded!", type="message")
      }, error=function(e) showNotification(paste("❌",e$message), type="error"))
    })

    output$data_stats <- renderUI({
      req(rv$loaded)
      d1 <- rv$data1
      div(class="d2-stat-row",
          div(class="d2-stat-box", div(class="d2-stat-num", length(unique(d1$GEN))),  div(class="d2-stat-lbl","Genotypes")),
          div(class="d2-stat-box", div(class="d2-stat-num", length(unique(d1$REP))),  div(class="d2-stat-lbl","Replications")),
          div(class="d2-stat-box", div(class="d2-stat-num", ncol(d1)-input$trait_start+1), div(class="d2-stat-lbl","Traits")),
          div(class="d2-stat-box", div(class="d2-stat-num", nrow(d1)),                div(class="d2-stat-lbl","Total Obs.")))
    })
    output$preview1 <- renderDT({ req(rv$data1); datatable(head(rv$data1,10), options=list(scrollX=TRUE,dom='t'), rownames=FALSE, class="display compact") })
    output$preview2 <- renderDT({ req(rv$data2); datatable(head(rv$data2,10), options=list(scrollX=TRUE,dom='t'), rownames=FALSE, class="display compact") })

    observeEvent(input$btn_manova, {
      req(rv$loaded)
      withProgress(message="Running MANOVA…", value=.3, {
        tryCatch({
          d1 <- rv$data1; ts <- input$trait_start
          dv <- as.matrix(d1[,ts:ncol(d1)])
          mod <- manova(dv ~ as.factor(GEN)+as.factor(REP), data=d1)
          ss  <- SSD(mod)
          rv$covar <- ss$SSD/ss$df; rv$mod <- mod; rv$dv <- dv; rv$ran_manova <- TRUE
          setProgress(1); showNotification("✅ MANOVA complete!", type="message")
        }, error=function(e) showNotification(paste("❌",e$message), type="error"))
      })
    })
    output$manova_out <- renderPrint({ req(rv$ran_manova); print(summary(rv$mod)) })
    output$anova_tbl  <- renderDT({
      req(rv$ran_manova)
      al <- summary.aov(rv$mod)
      rows <- lapply(seq_along(al), function(i){ tbl <- as.data.frame(al[[i]]); tbl$Trait <- names(al)[i]; tbl$Source <- rownames(tbl); tbl })
      df <- do.call(rbind, rows)
      df <- df[,c("Trait","Source",setdiff(names(df),c("Trait","Source")))]
      nc <- sapply(df, is.numeric)
      datatable(cbind(df[,!nc,drop=FALSE], round(df[,nc],4)), options=d2_dt_opts, rownames=FALSE, class="display compact")
    })

    observeEvent(input$btn_d2, {
      req(rv$covar, rv$data2)
      withProgress(message="Computing D² distances…", value=.5, {
        tryCatch({
          rv$d_dist <- D2.dist(rv$data2[,-1], rv$covar)
          rv$dm     <- as.matrix(rv$d_dist)
          rownames(rv$dm) <- colnames(rv$dm) <- as.character(rv$data2[,1])
          setProgress(1); showNotification("✅ D² complete!", type="message")
        }, error=function(e) showNotification(paste("❌",e$message), type="error"))
      })
    })
    output$d2_stats <- renderUI({
      req(rv$dm); lt <- rv$dm[lower.tri(rv$dm)]
      div(class="d2-stat-row",
          div(class="d2-stat-box", div(class="d2-stat-num",round(min(lt),2)),  div(class="d2-stat-lbl","Min D²")),
          div(class="d2-stat-box", div(class="d2-stat-num",round(max(lt),2)),  div(class="d2-stat-lbl","Max D²")),
          div(class="d2-stat-box", div(class="d2-stat-num",round(mean(lt),2)), div(class="d2-stat-lbl","Mean D²")),
          div(class="d2-stat-box", div(class="d2-stat-num",round(sd(lt),2)),   div(class="d2-stat-lbl","SD D²")))
    })
    output$d2_tbl <- renderDT({ req(rv$dm); datatable(as.data.frame(round(rv$dm,3)), options=d2_dt_opts, class="display compact") })
    output$d2_heatmap <- renderPlotly({
      req(rv$dm); dm <- rv$dm
      plot_ly(z=dm, x=colnames(dm), y=rownames(dm), type="heatmap",
              colorscale=list(c(0,"#053061"),c(.5,"#d1e5f0"),c(1,"#67001f")),
              hovertemplate="Geno1: %{y}<br>Geno2: %{x}<br>D²: %{z}<extra></extra>") %>%
        layout(paper_bgcolor="white", plot_bgcolor="white",
               xaxis=list(tickangle=-45), yaxis=list(), margin=list(l=70,b=70))
    })

    observeEvent(input$btn_tocher, {
      req(rv$d_dist)
      withProgress(message="Running Tocher…", value=.4, {
        tryCatch({
          rv$toc     <- tocher(rv$d_dist)
          nc         <- length(rv$toc$clusters); rv$n_clust <- nc; rv$pal <- d2_make_pal(nc)
          gn <- as.character(rv$data2[,1])
          cv <- setNames(rep(NA_integer_, length(gn)), gn)
          for (i in seq_len(nc)) {
            idx <- as.integer(rv$toc$clusters[[i]])
            cv[gn[idx]] <- i
          }
          rv$cluster_vec <- cv
          setProgress(1); showNotification("✅ Tocher complete!", type="message")
        }, error=function(e) showNotification(paste("❌",e$message), type="error"))
      })
    })
    output$tocher_stats <- renderUI({
      req(rv$toc); cs <- sapply(rv$toc$clusters, length)
      div(class="d2-stat-row",
          div(class="d2-stat-box", div(class="d2-stat-num",rv$n_clust), div(class="d2-stat-lbl","Clusters")),
          div(class="d2-stat-box", div(class="d2-stat-num",max(cs)),    div(class="d2-stat-lbl","Largest")),
          div(class="d2-stat-box", div(class="d2-stat-num",min(cs)),    div(class="d2-stat-lbl","Smallest")),
          div(class="d2-stat-box", div(class="d2-stat-num",round(mean(cs),1)), div(class="d2-stat-lbl","Avg Size")))
    })
    output$tocher_mem  <- renderDT({
      req(rv$cluster_vec)
      df <- data.frame(Genotype=names(rv$cluster_vec), Cluster=rv$cluster_vec, stringsAsFactors=FALSE)
      datatable(df[order(df$Cluster),], options=list(pageLength=15,dom='frtip',scrollX=TRUE), rownames=FALSE, class="display compact")
    })
    output$tocher_dist <- renderDT({
      req(rv$toc); dm <- as.data.frame(round(as.matrix(rv$toc$distClust),4))
      rownames(dm) <- colnames(dm) <- paste0("C",seq_len(nrow(dm)))
      datatable(dm, options=list(scrollX=TRUE,dom='t'), class="display compact")
    })
    output$dendro_plot  <- renderPlot({ req(rv$dm, rv$n_clust, rv$pal); d2_draw_dendro(rv$dm, rv$n_clust, rv$pal) })
    output$network_plot <- renderPlot({ req(rv$toc, rv$n_clust, rv$pal); print(d2_build_network(rv$toc, rv$n_clust, rv$pal)) })
    output$cluster_means <- renderDT({
      req(rv$cluster_vec, rv$data2)
      d2c <- rv$data2; d2c$Cluster <- rv$cluster_vec[match(as.character(d2c[,1]), names(rv$cluster_vec))]
      cm  <- aggregate(d2c[,-c(1,ncol(d2c))], by=list(Cluster=d2c$Cluster), mean)
      nc  <- sapply(cm, is.numeric)
      datatable(cbind(cm[,!nc,drop=FALSE], round(cm[,nc],3)), options=d2_dt_opts, rownames=FALSE, class="display compact")
    })

    observeEvent(input$btn_pca, {
      req(rv$data2)
      withProgress(message="Running PCA…", value=.5, {
        tryCatch({
          mn <- rv$data2[,-1]; rownames(mn) <- as.character(rv$data2[,1])
          pca <- prcomp(mn, scale.=TRUE)
          rv$pca_res <- pca; rv$eig_val <- get_eigenvalue(pca)
          setProgress(1); showNotification("✅ PCA complete!", type="message")
        }, error=function(e) showNotification(paste("❌",e$message), type="error"))
      })
    })
    output$pca_stats <- renderUI({
      req(rv$eig_val); ev <- rv$eig_val
      div(class="d2-stat-row",
          div(class="d2-stat-box", div(class="d2-stat-num",paste0(round(ev[1,2],1),"%")), div(class="d2-stat-lbl","PC1")),
          div(class="d2-stat-box", div(class="d2-stat-num",paste0(round(ev[2,2],1),"%")), div(class="d2-stat-lbl","PC2")),
          div(class="d2-stat-box", div(class="d2-stat-num",paste0(round(ev[2,3],1),"%")), div(class="d2-stat-lbl","PC1+PC2")),
          div(class="d2-stat-box", div(class="d2-stat-num",sum(ev[,2]>5)),               div(class="d2-stat-lbl","PCs > 5%")))
    })
    output$scree_plot <- renderPlot({ req(rv$pca_res, rv$eig_val); print(d2_build_scree(rv$pca_res, rv$eig_val)) })
    output$pca_biplot <- renderPlotly({
      req(rv$pca_res, rv$eig_val)
      pca <- rv$pca_res; ev <- rv$eig_val
      sc  <- max(abs(pca$x[,1:2]))/max(abs(pca$rotation[,1:2]))*.7
      ind <- data.frame(x=pca$x[,1], y=pca$x[,2], label=rownames(pca$x))
      vr  <- data.frame(x=pca$rotation[,1]*sc, y=pca$rotation[,2]*sc, label=rownames(pca$rotation))
      p <- plot_ly() %>%
        add_trace(data=ind, x=~x, y=~y, type="scatter", mode="markers+text",
                  text=~label, textposition="top center",
                  marker=list(color="#2196f3",size=8,opacity=.8),
                  textfont=list(size=10), name="Genotypes") %>%
        layout(xaxis=list(title=paste0("PC1 (",round(ev[1,2],1),"%)"), gridcolor="#D8EDE1"),
               yaxis=list(title=paste0("PC2 (",round(ev[2,2],1),"%)"), gridcolor="#D8EDE1"),
               title=list(text="PCA Biplot", font=list(size=13)))
      for (i in seq_len(nrow(vr)))
        p <- p %>% add_annotations(x=vr$x[i],y=vr$y[i],xref="x",yref="y",ax=0,ay=0,axref="x",ayref="y",
                                   arrowhead=2,arrowsize=1,arrowwidth=1.5,arrowcolor="#f39c12",
                                   text=vr$label[i],font=list(color="#f39c12",size=9))
      p
    })
    output$pca_cluster <- renderPlotly({
      req(rv$pca_res, rv$cluster_vec, rv$pal)
      pca <- rv$pca_res; ev <- rv$eig_val
      matched_cv <- rv$cluster_vec[match(rownames(pca$x), names(rv$cluster_vec))]
      df  <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], Geno=rownames(pca$x),
                        Cluster=as.factor(matched_cv), stringsAsFactors=FALSE)
      plot_ly(df, x=~PC1, y=~PC2, color=~Cluster, colors=rv$pal, type="scatter", mode="markers+text",
              text=~Geno, textposition="top center", marker=list(size=10,opacity=.85), textfont=list(size=9),
              hovertemplate="<b>%{text}</b><br>PC1: %{x:.2f}<br>PC2: %{y:.2f}<extra></extra>") %>%
        layout(xaxis=list(title=paste0("PC1 (",round(ev[1,2],1),"%)"),gridcolor="#D8EDE1"),
               yaxis=list(title=paste0("PC2 (",round(ev[2,2],1),"%)"),gridcolor="#D8EDE1"),
               title=list(text="PCA with Tocher Cluster Overlay",font=list(size=13)))
    })
    output$eig_tbl <- renderDT({
      req(rv$eig_val); df <- as.data.frame(round(rv$eig_val,4)); df$PC <- rownames(df)
      df <- df[,c("PC",names(df)[1:3])]
      datatable(df, options=list(pageLength=10,dom='t',scrollX=TRUE), rownames=FALSE, class="display compact")
    })
    output$loading_tbl <- renderDT({
      req(rv$pca_res); datatable(as.data.frame(round(rv$pca_res$rotation,4)),
                                  options=list(pageLength=10,dom='t',scrollX=TRUE), class="display compact")
    })

    observeEvent(input$btn_corr, {
      req(rv$data2)
      withProgress(message="Computing correlation…", value=.5, {
        tryCatch({
          mn <- as.matrix(rv$data2[,-1]); cr <- rcorr(mn)
          rv$cor_mat <- cr$r; rv$p_mat <- cr$P
          setProgress(1); showNotification("✅ Correlation complete!", type="message")
        }, error=function(e) showNotification(paste("❌",e$message), type="error"))
      })
    })
    output$corr_heatmap <- renderPlotly({
      req(rv$cor_mat); cm <- rv$cor_mat; pm <- rv$p_mat
      sig <- ifelse(is.na(pm),"", ifelse(pm<=.001,"***", ifelse(pm<=.01,"**", ifelse(pm<=.05,"*",""))))
      txt <- matrix(paste0(round(cm,2),sig), nrow=nrow(cm))
      cs  <- switch(input$corr_palette,
                    rwb=list(c(0,"#053061"),c(.5,"white"),c(1,"#67001f")),
                    pastel=list(c(0,"#A6CEE3"),c(.5,"white"),c(1,"#FB9A99")),
                    viridis=list(c(0,"#440154"),c(.33,"#31688e"),c(.66,"#35b779"),c(1,"#fde725")))
      plot_ly(z=cm, x=colnames(cm), y=rownames(cm), text=txt, type="heatmap",
              colorscale=cs, zmin=-1, zmax=1,
              hovertemplate="Trait1: %{y}<br>Trait2: %{x}<br>r: %{z:.3f}<extra></extra>") %>%
        layout(xaxis=list(tickangle=-45), margin=list(l=80,b=80), title=list(text="Pearson Correlation Matrix"))
    })
    output$corr_tbl <- renderDT({
      req(rv$cor_mat); datatable(as.data.frame(round(rv$cor_mat,4)), options=list(scrollX=TRUE,dom='t'), class="display compact")
    })
    output$sig_pairs_tbl <- renderDT({
      req(rv$cor_mat, rv$p_mat); cm <- rv$cor_mat; pm <- rv$p_mat
      trn <- which(lower.tri(cm), arr.ind=TRUE)
      df  <- data.frame(Trait1=rownames(cm)[trn[,1]], Trait2=colnames(cm)[trn[,2]],
                        r=round(cm[trn],4), p_value=round(pm[trn],4), stringsAsFactors=FALSE)
      df$Significance <- ifelse(df$p_value<=.001,"***", ifelse(df$p_value<=.01,"**", ifelse(df$p_value<=.05,"*","ns")))
      df <- df[df$p_value<=.05,][order(df$p_value[df$p_value<=.05]),]
      datatable(df, options=list(pageLength=15,dom='frtip',scrollX=TRUE), rownames=FALSE, class="display compact")
    })

    # ── D² Download Handlers ─────────────────────────────
    output$dl_means  <- downloadHandler("Genotype_Means.csv",     function(f){ req(rv$gm); df <- rv$gm; nc <- sapply(df, is.numeric); df[nc] <- round(df[nc], 4); write.csv(df, f, row.names=FALSE)})
    output$dl_d2mat  <- downloadHandler("D2_Distance_Matrix.csv", function(f){ req(rv$dm);       write.csv(round(rv$dm,4),f)})
    output$dl_tocher <- downloadHandler("Tocher_Clusters.csv",    function(f){ req(rv$cluster_vec); df <- data.frame(Genotype=names(rv$cluster_vec),Cluster=rv$cluster_vec); write.csv(df[order(df$Cluster),],f,row.names=FALSE)})
    output$dl_cluster_means <- downloadHandler("Cluster_Trait_Means.csv", function(f){ req(rv$cluster_vec, rv$data2); d2c <- rv$data2; d2c$Cluster <- rv$cluster_vec[match(as.character(d2c[,1]), names(rv$cluster_vec))]; cm <- aggregate(d2c[,-c(1,ncol(d2c))], by=list(Cluster=d2c$Cluster), mean); nc <- sapply(cm, is.numeric); cm[nc] <- round(cm[nc], 4); write.csv(cm[order(cm$Cluster),], f, row.names=FALSE) })
    output$dl_cluster_dist  <- downloadHandler("Cluster_Distances.csv",   function(f){ req(rv$toc); dm <- as.data.frame(round(as.matrix(rv$toc$distClust), 4)); rownames(dm) <- colnames(dm) <- paste0("C", seq_len(nrow(dm))); write.csv(dm, f) })
    output$dl_corr   <- downloadHandler("Correlation_Matrix.csv", function(f){ req(rv$cor_mat);  write.csv(round(rv$cor_mat,6),f)})
    output$dl_eig    <- downloadHandler("PCA_Eigenvalues.csv",    function(f){ req(rv$eig_val);  write.csv(round(rv$eig_val,6),f)})
    output$dl_loads  <- downloadHandler("PCA_Loadings.csv",       function(f){ req(rv$pca_res);  write.csv(round(as.data.frame(rv$pca_res$rotation),6),f)})
    output$dl_scores <- downloadHandler("PCA_Scores.csv",         function(f){ req(rv$pca_res);  df <- cbind(Genotype=rownames(rv$pca_res$x),as.data.frame(round(rv$pca_res$x,6))); write.csv(df,f,row.names=FALSE)})
    output$dl_anova  <- downloadHandler("ANOVA_Per_Trait.csv",    function(f){ req(rv$ran_manova); al <- summary.aov(rv$mod); rows <- lapply(seq_along(al),function(i){tbl <- as.data.frame(al[[i]]);tbl$Trait<-names(al)[i];tbl$Source<-rownames(tbl);tbl}); df <- do.call(rbind,rows); write.csv(df,f,row.names=FALSE)})

    for (id_pair in list(c("dl_heatmap_pdf","dl_heatmap_pdf_ex"))) {
      local({ lid <- id_pair
        for (lid_i in lid) local({ lii <- lid_i
          output[[lii]] <- downloadHandler("D2_Heatmap.pdf", function(f){ req(rv$dm); p <- ggplot(reshape2::melt(rv$dm) %>% setNames(c("G1","G2","D")), aes(x=G2,y=G1,fill=D)) + geom_tile(colour="white",linewidth=.3) + scale_fill_gradientn(colours=c("#053061","#2166ac","#d1e5f0","#fddbc7","#d6604d","#67001f"),name="D²") + labs(title="Mahalanobis D² Heatmap",x=NULL,y=NULL) + theme_met() + theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1,size=7),axis.text.y=element_text(size=7),panel.grid=element_blank()); n <- nrow(rv$dm); save_pdf(p,f,max(8,n*.4),max(7,n*.4)) })
        })
      })
    }
    output$dl_dendro_pdf    <- downloadHandler("Dendrogram.pdf",    function(f){ req(rv$dm,rv$n_clust,rv$pal); cairo_pdf(f,14,8); d2_draw_dendro(rv$dm,rv$n_clust,rv$pal); dev.off()})
    output$dl_dendro_pdf_ex <- downloadHandler("Dendrogram.pdf",    function(f){ req(rv$dm,rv$n_clust,rv$pal); cairo_pdf(f,14,8); d2_draw_dendro(rv$dm,rv$n_clust,rv$pal); dev.off()})
    output$dl_network_pdf    <- downloadHandler("Network_Plot.pdf", function(f){ req(rv$toc,rv$n_clust,rv$pal); save_pdf(d2_build_network(rv$toc,rv$n_clust,rv$pal),f,10,10)})
    output$dl_network_pdf_ex <- downloadHandler("Network_Plot.pdf", function(f){ req(rv$toc,rv$n_clust,rv$pal); save_pdf(d2_build_network(rv$toc,rv$n_clust,rv$pal),f,10,10)})
    output$dl_scree_pdf      <- downloadHandler("Scree_Plot.pdf",   function(f){ req(rv$pca_res,rv$eig_val); save_pdf(d2_build_scree(rv$pca_res,rv$eig_val),f,10,6)})
    output$dl_scree_pdf_ex   <- downloadHandler("Scree_Plot.pdf",   function(f){ req(rv$pca_res,rv$eig_val); save_pdf(d2_build_scree(rv$pca_res,rv$eig_val),f,10,6)})
    output$dl_biplot_pdf     <- downloadHandler("PCA_Biplot.pdf",   function(f){ req(rv$pca_res,rv$eig_val); save_pdf(d2_build_biplot(rv$pca_res,rv$eig_val),f,10,8)})
    output$dl_biplot_pdf_ex  <- downloadHandler("PCA_Biplot.pdf",   function(f){ req(rv$pca_res,rv$eig_val); save_pdf(d2_build_biplot(rv$pca_res,rv$eig_val),f,10,8)})
    output$dl_pca_cluster_pdf <- downloadHandler(paste0("PCA_Cluster.pdf"), function(f){ req(rv$pca_res,rv$cluster_vec,rv$pal,rv$eig_val); ev<-rv$eig_val; pca<-rv$pca_res; mcv<-rv$cluster_vec[match(rownames(pca$x),names(rv$cluster_vec))]; df<-data.frame(PC1=pca$x[,1],PC2=pca$x[,2],Geno=rownames(pca$x),Cluster=as.factor(mcv)); p<-ggplot(df,aes(PC1,PC2,colour=Cluster,label=Geno))+geom_point(size=3,alpha=.85)+ggrepel::geom_text_repel(size=2.8,max.overlaps=25)+scale_colour_manual(values=rv$pal)+labs(title="PCA Cluster Overlay",x=paste0("PC1 (",round(ev[1,2],1),"%)"),y=paste0("PC2 (",round(ev[2,2],1),"%)"),colour="Cluster")+theme_met(); save_pdf(p,f,12,9)})
    output$dl_corr_pdf     <- downloadHandler("Correlation_Heatmap.pdf", function(f){ req(rv$cor_mat,rv$p_mat); p <- d2_build_corr_heatmap(rv$cor_mat,rv$p_mat,input$corr_palette); n<-nrow(rv$cor_mat); save_pdf(p,f,max(7,n*.6),max(6,n*.55))})
    output$dl_corr_pdf_ex  <- downloadHandler("Correlation_Heatmap.pdf", function(f){ req(rv$cor_mat,rv$p_mat); p <- d2_build_corr_heatmap(rv$cor_mat,rv$p_mat,input$corr_palette); n<-nrow(rv$cor_mat); save_pdf(p,f,max(7,n*.6),max(6,n*.55))})
  })
}

# ════════════════════════════════════════════════════════════════
#  MET MODULE — UI
# ════════════════════════════════════════════════════════════════
metUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabItem("met_home",
      div(class="home-banner", h2("🌾 Multi-Environment Trial Analysis Suite"),
          p("AMMI · GGE · Stability · ANOVA · Descriptive Statistics"),
          div(class="vtag","VERSION 2.0 | 2025")),
      fluidRow(
        column(4,div(class="feat-card",div(class="fi","📊"),h4("Descriptive Statistics"),p("Summary stats, histograms, boxplots, and interactive GxE heatmaps."))),
        column(4,div(class="feat-card",div(class="fi","🔬"),h4("ANOVA"),p("Individual + pooled ANOVA, GxE decomposition, Bartlett test."))),
        column(4,div(class="feat-card",div(class="fi","📉"),h4("9+ Stability Parameters"),p("Annicchiarico, Ecovalence, Shukla, Eberhart-Russell, Lin & Binns, WAASB.")))
      ), br(),
      fluidRow(
        column(4,div(class="feat-card",div(class="fi","🎯"),h4("AMMI Analysis"),p("Full AMMI model, biplots Types 1–3, IPCA significance, ASV index."))),
        column(4,div(class="feat-card",div(class="fi","🌐"),h4("GGE Biplot"),p("7 GGE biplot types: basic, mean-stability, which-won-where, ranking."))),
        column(4,div(class="feat-card",div(class="fi","📄"),h4("HD PDF + Excel"),p("Every plot exports as HD PDF. All tables download as Excel.")))
      )
    ),
    tabItem("met_data",
      div(class="sec-bar","📂 MET — Data Upload & Inspection"),
      fluidRow(
        box(title="Upload & Configure", width=4, status="success", solidHeader=TRUE,
          fileInput(ns("file1"), "Choose CSV / Excel File", accept=c(".csv",".xlsx",".xls")),
          hr(), h5(style="color:#1B4332;font-weight:700;","Column Mapping"),
          uiOutput(ns("ui_env_col")), uiOutput(ns("ui_gen_col")), uiOutput(ns("ui_rep_col")), hr(),
          actionButton(ns("btn_load"), "⚙️  Load & Process Data", class="btn-success btn-block", icon=icon("play"))
        ),
        box(title="Data Exploration", width=8, status="success", solidHeader=TRUE,
          uiOutput(ns("value_boxes")),
          tabsetPanel(
            tabPanel("📋 Preview",       br(), DTOutput(ns("tbl_preview"))),
            tabPanel("🔎 Structure",     br(), verbatimTextOutput(ns("txt_str"))),
            tabPanel("📦 Outlier Check", dl_bar(ns("dl_plt_outlier")), plotOutput(ns("plt_outlier"), height="340px")),
            tabPanel("📝 Inspect",       br(), verbatimTextOutput(ns("txt_inspect")))
          )
        )
      )
    ),
    tabItem("met_desc",
      div(class="sec-bar","📊 MET — Descriptive Statistics"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_desc_var")),
          actionButton(ns("btn_desc"), "▶  Compute Stats", class="btn-success btn-block", icon=icon("calculator")), hr(),
          downloadButton(ns("dl_desc"), "⬇  Download Excel", class="btn-warning btn-block")
        ),
        box(title="Results", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 Statistics",    br(), DTOutput(ns("tbl_desc"))),
            tabPanel("📊 Histogram",     dl_bar(ns("dl_plt_hist")),  plotOutput(ns("plt_dist"),    height="400px")),
            tabPanel("📦 Boxplot by Env",dl_bar(ns("dl_plt_box")),   plotOutput(ns("plt_box_env"), height="400px")),
            tabPanel("🔥 GxE Heatmap",   dl_bar(ns("dl_plt_heat")),  plotOutput(ns("plt_heat"),    height="400px"))
          )
        )
      )
    ),
    tabItem("met_mean",
      div(class="sec-bar","📈 MET — Mean Performance"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_mean_var")),
          radioGroupButtons(ns("mean_type"), "Group by:",
                            choices=c("Genotype"="GEN","Environment"="ENV","GEN × ENV"="GxE"),
                            selected="GEN", justified=TRUE, size="sm"), br(),
          actionButton(ns("btn_mean"), "▶  Compute Means", class="btn-success btn-block", icon=icon("calculator")), hr(),
          downloadButton(ns("dl_mean"), "⬇  Download Excel", class="btn-warning btn-block")
        ),
        box(title="Mean Performance", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 Table",   br(), DTOutput(ns("tbl_mean"))),
            tabPanel("📊 Bar Plot",dl_bar(ns("dl_plt_bar")), plotlyOutput(ns("plt_mean_bar"), height="400px")),
            tabPanel("📈 GE Plot", dl_bar(ns("dl_plt_ge")),  plotOutput(ns("plt_ge"),         height="400px")),
            tabPanel("🏆 Winners", br(), DTOutput(ns("tbl_winners")))
          )
        )
      )
    ),
    tabItem("met_anova",
      div(class="sec-bar","🔬 MET — Analysis of Variance"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_anova_var")),
          actionButton(ns("btn_anova"), "▶  Run ANOVA", class="btn-success btn-block", icon=icon("play")), hr(),
          downloadButton(ns("dl_anova_ind"),  "⬇  Individual ANOVA", class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_anova_pool"), "⬇  Pooled ANOVA",     class="btn-warning btn-block")
        ),
        box(title="ANOVA Results", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 Individual ANOVA", br(), DTOutput(ns("tbl_ind_anova"))),
            tabPanel("📋 Pooled ANOVA",     br(), DTOutput(ns("tbl_pool_anova"))),
            tabPanel("🧪 Bartlett Test",    br(), verbatimTextOutput(ns("txt_bartlett")))
          )
        )
      )
    ),
    tabItem("met_stab_anova",
      div(class="sec-bar","📉 MET — ANOVA-based Stability"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_stab_anova_var")),
          actionButton(ns("btn_stab_anova"), "▶  Run Analysis", class="btn-success btn-block", icon=icon("play")), hr(),
          downloadButton(ns("dl_ann"), "⬇  Annicchiarico", class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_eco"), "⬇  Ecovalence",    class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_shk"), "⬇  Shukla",        class="btn-warning btn-block")
        ),
        box(title="ANOVA-based Stability", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("Ann. – General",     br(), DTOutput(ns("tbl_ann_gen"))),
            tabPanel("Ann. – Favorable",   br(), DTOutput(ns("tbl_ann_fav"))),
            tabPanel("Ann. – Unfavorable", br(), DTOutput(ns("tbl_ann_unf"))),
            tabPanel("Ecovalence (Wi)",    br(), DTOutput(ns("tbl_eco"))),
            tabPanel("Shukla Variance",    br(), DTOutput(ns("tbl_shukla"))),
            tabPanel("📊 Shukla Plot",     dl_bar(ns("dl_plt_shukla")), plotlyOutput(ns("plt_shukla"), height="400px"))
          )
        )
      )
    ),
    tabItem("met_stab_reg",
      div(class="sec-bar","📈 MET — Regression Stability (Eberhart-Russell)"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_stab_reg_var")),
          actionButton(ns("btn_stab_reg"), "▶  Run Regression", class="btn-success btn-block", icon=icon("play")), hr(),
          downloadButton(ns("dl_stab_reg"), "⬇  Download ANOVA", class="btn-warning btn-block")
        ),
        box(title="Regression Stability", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 ANOVA",          br(), DTOutput(ns("tbl_reg_anova"))),
            tabPanel("📊 Regression Plot",dl_bar(ns("dl_plt_reg")), plotOutput(ns("plt_reg"), height="460px"))
          )
        )
      )
    ),
    tabItem("met_stab_np",
      div(class="sec-bar","🔢 MET — Non-parametric Stability"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_stab_np_var")),
          actionButton(ns("btn_stab_np"), "▶  Run Analysis", class="btn-success btn-block", icon=icon("play")), hr(),
          downloadButton(ns("dl_super"), "⬇  Superiority", class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_fox"),   "⬇  Fox Index",   class="btn-warning btn-block")
        ),
        box(title="Non-parametric Results", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("Lin & Binns",     br(), DTOutput(ns("tbl_superiority"))),
            tabPanel("Fox Top-Third",   br(), DTOutput(ns("tbl_fox"))),
            tabPanel("📊 Superiority Plot", dl_bar(ns("dl_plt_np")), plotlyOutput(ns("plt_np"), height="400px"))
          )
        )
      )
    ),
    tabItem("met_stab_fa",
      div(class="sec-bar","🔠 MET — Factor Analysis of GE"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_stab_fa_var")),
          actionButton(ns("btn_stab_fa"), "▶  Run Factor Analysis", class="btn-success btn-block", icon=icon("play")), hr(),
          downloadButton(ns("dl_fa"), "⬇  Download Results", class="btn-warning btn-block")
        ),
        box(title="Factor Analysis", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("Genotype Scores",  br(), DTOutput(ns("tbl_fa_scores"))),
            tabPanel("Rotated Loadings", br(), DTOutput(ns("tbl_fa_loads"))),
            tabPanel("PCA Eigenvalues",  br(), DTOutput(ns("tbl_fa_pca"))),
            tabPanel("📊 Factor Biplot", dl_bar(ns("dl_plt_fa")), plotOutput(ns("plt_fa"), height="460px"))
          )
        )
      )
    ),
    tabItem("met_stab_wrap",
      div(class="sec-bar","🎁 MET — Comprehensive Stability (ge_stats)"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_wrap_var")),
          actionButton(ns("btn_wrap"), "▶  Compute All", class="btn-success btn-block", icon=icon("cogs")), hr(),
          downloadButton(ns("dl_wrap"),      "⬇  All Parameters", class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_wrap_rank"), "⬇  Rankings",       class="btn-warning btn-block")
        ),
        box(title="All Stability Parameters", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 Parameters", br(), DTOutput(ns("tbl_wrap"))),
            tabPanel("🏆 Rankings",   br(), DTOutput(ns("tbl_wrap_rank"))),
            tabPanel("🔗 Correlation Heatmap", dl_bar(ns("dl_plt_cor")), plotOutput(ns("plt_wrap_cor"), height="480px"))
          )
        )
      )
    ),
    tabItem("met_ammi",
      div(class="sec-bar","🎯 MET — AMMI Analysis"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_ammi_var")),
          numericInput(ns("ammi_naxis"), "IPCA axes:", 2, min=1, max=10),
          actionButton(ns("btn_ammi"), "▶  Run AMMI", class="btn-success btn-block", icon=icon("play")), hr(),
          downloadButton(ns("dl_ammi_anova"), "⬇  ANOVA",      class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_ammi_idx"),   "⬇  AMMI Index", class="btn-warning btn-block")
        ),
        box(title="AMMI Results", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 ANOVA",             br(), DTOutput(ns("tbl_ammi_anova"))),
            tabPanel("📋 IPCA Sig.",          br(), DTOutput(ns("tbl_ipca"))),
            tabPanel("📊 Biplot Type 1",      dl_bar(ns("dl_plt_ammi1")), plotOutput(ns("plt_ammi1"), height="450px")),
            tabPanel("📊 Biplot Type 2",      dl_bar(ns("dl_plt_ammi2")), plotOutput(ns("plt_ammi2"), height="450px")),
            tabPanel("📊 Y × WAAS Biplot",    dl_bar(ns("dl_plt_waas")),  plotOutput(ns("plt_waas"),  height="450px")),
            tabPanel("📋 AMMI Index",          br(), DTOutput(ns("tbl_ammi_idx")))
          )
        )
      )
    ),
    tabItem("met_gge",
      div(class="sec-bar","🌐 MET — GGE Biplot Analysis"),
      fluidRow(
        box(title="Options", width=3, status="success", solidHeader=TRUE,
          uiOutput(ns("ui_gge_var")),
          selectInput(ns("gge_svp"), "SVP:",
                      choices=c("Environment"="environment","Genotype"="genotype","Symmetrical"="symmetrical"),
                      selected="symmetrical"),
          actionButton(ns("btn_gge"), "▶  Run GGE", class="btn-success btn-block", icon=icon("play")), hr(),
          downloadButton(ns("dl_gge"), "⬇  Predictions", class="btn-warning btn-block")
        ),
        box(title="GGE Biplots", width=9, status="success", solidHeader=TRUE,
          tabsetPanel(
            tabPanel("Basic Biplot",           dl_bar(ns("dl_plt_gge1")), plotOutput(ns("plt_gge1"), height="440px")),
            tabPanel("Mean vs Stability",      dl_bar(ns("dl_plt_gge2")), plotOutput(ns("plt_gge2"), height="440px")),
            tabPanel("Which-Won-Where",        dl_bar(ns("dl_plt_gge3")), plotOutput(ns("plt_gge3"), height="440px")),
            tabPanel("Discrim. vs Repres.",    dl_bar(ns("dl_plt_gge4")), plotOutput(ns("plt_gge4"), height="440px")),
            tabPanel("Ranking Genotypes",      dl_bar(ns("dl_plt_gge5")), plotOutput(ns("plt_gge5"), height="440px")),
            tabPanel("Ranking Environments",   dl_bar(ns("dl_plt_gge6")), plotOutput(ns("plt_gge6"), height="440px")),
            tabPanel("Relation Environments",  dl_bar(ns("dl_plt_gge7")), plotOutput(ns("plt_gge7"), height="440px"))
          )
        )
      )
    )
  )
}

# ════════════════════════════════════════════════════════════════
#  MET MODULE — SERVER
# ════════════════════════════════════════════════════════════════
metServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    raw_data       <- reactiveVal(NULL)
    processed_data <- reactiveVal(NULL)

    observeEvent(input$file1, {
      req(input$file1)
      ext <- tools::file_ext(input$file1$name)
      df  <- tryCatch({
        if (tolower(ext)=="csv") read.csv(input$file1$datapath, stringsAsFactors=FALSE)
        else as.data.frame(readxl::read_excel(input$file1$datapath))
      }, error=function(e){ showNotification(paste("❌ File error:",e$message),type="error"); NULL })
      raw_data(df)
    })

    observe({
      req(raw_data()); cols <- names(raw_data())
      g <- function(p) grep(p,cols,ignore.case=TRUE,value=TRUE)[1] %||% cols[1]
      output$ui_env_col <- renderUI(selectInput(session$ns("env_col"),"ENV column:",cols,selected=g("^ENV$|environ")))
      output$ui_gen_col <- renderUI(selectInput(session$ns("gen_col"),"GEN column:",cols,selected=g("^GEN$|geno")))
      output$ui_rep_col <- renderUI(selectInput(session$ns("rep_col"),"REP column:",cols,selected=g("^REP$|repli|block")))
    })

    observeEvent(input$btn_load, {
      req(raw_data(), input$env_col, input$gen_col, input$rep_col)
      df <- raw_data()
      names(df)[names(df)==input$env_col] <- "ENV"
      names(df)[names(df)==input$gen_col] <- "GEN"
      names(df)[names(df)==input$rep_col] <- "REP"
      df$ENV <- factor(df$ENV, levels=unique(df$ENV))
      df$GEN <- factor(df$GEN, levels=unique(df$GEN))
      df$REP <- factor(df$REP, levels=unique(df$REP))
      resp_cols <- setdiff(names(df), c("ENV","GEN","REP"))
      for (col in resp_cols) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      processed_data(df)
      showNotification("✅ MET data loaded!", type="message", duration=3)
    })

    resp_vars <- reactive({ req(processed_data()); setdiff(names(processed_data()), c("ENV","GEN","REP")) })

    output$value_boxes <- renderUI({
      req(processed_data()); df <- processed_data()
      fluidRow(
        valueBox(nlevels(df$ENV),"Environments",icon=icon("leaf"),   color="green", width=3),
        valueBox(nlevels(df$GEN),"Genotypes",   icon=icon("seedling"),color="teal",  width=3),
        valueBox(nlevels(df$REP),"Replications",icon=icon("clone"),  color="orange",width=3),
        valueBox(nrow(df),       "Observations",icon=icon("table"),  color="purple",width=3)
      )
    })
    output$tbl_preview <- renderDT({ req(processed_data()); datatable(processed_data(), options=list(scrollX=TRUE,pageLength=10), rownames=FALSE) })
    output$txt_str     <- renderPrint({ req(processed_data()); str(processed_data()) })
    output$txt_inspect <- renderPrint({ req(processed_data()); tryCatch(inspect(processed_data()), error=function(e) cat("Error:",e$message)) })
    output$plt_outlier <- renderPlot({ req(processed_data(),resp_vars()); v <- sym(resp_vars()[1]); tryCatch(find_outliers(processed_data(),var=!!v,plots=TRUE), error=function(e){ plot.new(); title(paste("Error:",e$message)) }) })
    output$dl_plt_outlier <- downloadHandler(paste0("outlier_",Sys.Date(),".pdf"), function(f){ req(processed_data(),resp_vars()); v<-sym(resp_vars()[1]); cairo_pdf(f,14,9); tryCatch(find_outliers(processed_data(),var=!!v,plots=TRUE),error=function(e){plot.new();title(e$message)}); dev.off() })

    mk_sel <- function(input_id) renderUI({ req(resp_vars()); selectInput(session$ns(input_id),"Response variable:",choices=resp_vars()) })
    output$ui_desc_var       <- mk_sel("desc_var")
    output$ui_mean_var       <- mk_sel("mean_var")
    output$ui_anova_var      <- mk_sel("anova_var")
    output$ui_stab_anova_var <- mk_sel("stab_anova_var")
    output$ui_stab_reg_var   <- mk_sel("stab_reg_var")
    output$ui_stab_np_var    <- mk_sel("stab_np_var")
    output$ui_stab_fa_var    <- mk_sel("stab_fa_var")
    output$ui_wrap_var       <- mk_sel("wrap_var")
    output$ui_ammi_var       <- mk_sel("ammi_var")
    output$ui_gge_var        <- mk_sel("gge_var")

    # Descriptive
    desc_r <- eventReactive(input$btn_desc, { req(processed_data()); tryCatch(desc_stat(processed_data(),stats="all"),error=function(e) NULL) })
    output$tbl_desc <- renderDT({ req(desc_r()); fmt_dt(as.data.frame(desc_r())) })
    output$dl_desc  <- downloadHandler(paste0("descriptive_",Sys.Date(),".xlsx"), function(f){ req(desc_r()); write_xlsx(as.data.frame(desc_r()),f) })

    r_plt_hist <- reactive({ req(processed_data(),input$desc_var); df<-processed_data(); vs<-sym(input$desc_var)
      ggplot(df,aes(x=!!vs,fill=ENV))+geom_histogram(alpha=.75,bins=20,color="white",linewidth=.3)+scale_fill_manual(values=MET_COLORS)+facet_wrap(~ENV,scales="free_y")+labs(title=paste("Distribution of",input$desc_var),x=input$desc_var,y="Count")+theme_met()+theme(legend.position="none") })
    output$plt_dist   <- renderPlot({ req(r_plt_hist()); r_plt_hist() })
    output$dl_plt_hist <- downloadHandler(paste0("histogram_",Sys.Date(),".pdf"), function(f){ req(r_plt_hist()); save_pdf(r_plt_hist(),f,16,11) })

    r_plt_box <- reactive({ req(processed_data(),input$desc_var); df<-processed_data(); vs<-sym(input$desc_var)
      ggplot(df,aes(x=ENV,y=!!vs,fill=ENV))+geom_boxplot(alpha=.8,outlier.color="#C62828")+geom_jitter(width=.15,alpha=.3,size=.8)+scale_fill_manual(values=MET_COLORS)+labs(title=paste("Boxplot of",input$desc_var),x="Environment",y=input$desc_var)+theme_met()+theme(legend.position="none",axis.text.x=element_text(angle=45,hjust=1)) })
    output$plt_box_env <- renderPlot({ req(r_plt_box()); r_plt_box() })
    output$dl_plt_box  <- downloadHandler(paste0("boxplot_",Sys.Date(),".pdf"), function(f){ req(r_plt_box()); save_pdf(r_plt_box(),f,16,10) })

    r_plt_heat <- reactive({ req(processed_data(),input$desc_var); df<-processed_data(); vs<-sym(input$desc_var)
      gm <- df %>% group_by(ENV,GEN) %>% summarise(m=mean(!!vs,na.rm=TRUE),.groups="drop")
      ggplot(gm,aes(x=ENV,y=GEN,fill=m))+geom_tile(color="white",linewidth=.5)+
        scale_fill_gradientn(colors=c("#1B4332","#40916C","#95D5B2","#FFFDE7","#F4A300","#C62828"),name=input$desc_var)+
        geom_text(aes(label=round(m,1)),size=3,color="white",fontface="bold")+
        labs(title=paste("GxE Heatmap –",input$desc_var),x="Environment",y="Genotype")+theme_met()+theme(axis.text.x=element_text(angle=45,hjust=1)) })
    output$plt_heat   <- renderPlot({ req(r_plt_heat()); r_plt_heat() })
    output$dl_plt_heat <- downloadHandler(paste0("GxE_heatmap_",Sys.Date(),".pdf"), function(f){ req(r_plt_heat()); save_pdf(r_plt_heat(),f,16,12) })

    # Mean Performance
    mean_r <- eventReactive(input$btn_mean, {
      req(processed_data(),input$mean_var,input$mean_type); df<-processed_data(); vs<-sym(input$mean_var)
      if (input$mean_type=="GEN")      means_by(df,GEN)
      else if (input$mean_type=="ENV") means_by(df,ENV)
      else df %>% group_by(ENV,GEN) %>% summarise(Mean=mean(!!vs,na.rm=TRUE),.groups="drop")
    })
    output$tbl_mean <- renderDT({ req(mean_r()); fmt_dt(as.data.frame(mean_r())) })
    output$dl_mean  <- downloadHandler(paste0("means_",Sys.Date(),".xlsx"), function(f){ req(mean_r()); write_xlsx(as.data.frame(mean_r()),f) })

    r_plt_bar <- reactive({ req(mean_r(),input$mean_var); df<-as.data.frame(mean_r())
      gc <- if (input$mean_type %in% c("GEN","GxE")) "GEN" else "ENV"
      vc <- if (input$mean_var %in% names(df)) input$mean_var else names(df)[which(sapply(df,is.numeric))[1]]
      req(gc %in% names(df), vc %in% names(df))
      ggplot(df,aes(x=reorder(.data[[gc]],-.data[[vc]]),y=.data[[vc]],fill=.data[[gc]]))+geom_col(color="white",linewidth=.3)+scale_fill_manual(values=MET_COLORS)+labs(title=paste("Mean",vc,"by",gc),x=gc,y=vc)+theme_met()+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none") })
    output$plt_mean_bar <- renderPlotly({ req(r_plt_bar()); ggplotly(r_plt_bar(),tooltip=c("x","y")) })
    output$dl_plt_bar   <- downloadHandler(paste0("barplot_",Sys.Date(),".pdf"), function(f){ req(r_plt_bar()); save_pdf(r_plt_bar(),f,16,10) })

    r_plt_ge <- reactive({ req(processed_data(),input$mean_var); df<-processed_data(); vs<-sym(input$mean_var)
      top_g <- df %>% group_by(GEN) %>% summarise(mv=mean(!!vs,na.rm=TRUE),.groups="drop") %>% slice_max(mv,n=min(15,nlevels(df$GEN)))
      df_s  <- df %>% filter(GEN %in% top_g$GEN)
      ge_plot(df_s,ENV,GEN,!!vs)+theme_met()+labs(title=paste("GE Plot –",input$mean_var)) })
    output$plt_ge    <- renderPlot({ req(r_plt_ge()); r_plt_ge() })
    output$dl_plt_ge <- downloadHandler(paste0("GE_plot_",Sys.Date(),".pdf"), function(f){ req(r_plt_ge()); save_pdf(r_plt_ge(),f,16,10) })
    output$tbl_winners <- renderDT({ req(processed_data(),input$mean_var); vs<-sym(input$mean_var); tryCatch({ w<-ge_winners(processed_data(),ENV,GEN,resp=!!vs); fmt_dt(as.data.frame(w)) },error=function(e) datatable(data.frame(Message=paste("Error:",e$message)))) })

    # ANOVA
    anova_r <- eventReactive(input$btn_anova, {
      req(processed_data(),input$anova_var); df<-processed_data(); vs<-sym(input$anova_var)
      list(ind=tryCatch(anova_ind(df,ENV,GEN,REP,resp=!!vs),error=function(e)NULL),
           pool=tryCatch(anova_joint(df,ENV,GEN,REP,!!vs),error=function(e)NULL),
           bart=tryCatch(bartlett.test(df[[input$anova_var]]~df$ENV),error=function(e)NULL))
    })
    output$tbl_ind_anova  <- renderDT({ req(anova_r()); v<-input$anova_var; fmt_dt(tryCatch(safe_df(anova_r()$ind[[v]]$individual),error=function(e)data.frame(Error=e$message)),20) })
    output$tbl_pool_anova <- renderDT({ req(anova_r()); v<-input$anova_var; fmt_dt(tryCatch(safe_df(anova_r()$pool[[v]]$anova),error=function(e)data.frame(Error=e$message)),20) })
    output$txt_bartlett   <- renderPrint({ req(anova_r()); print(anova_r()$bart) })
    output$dl_anova_ind   <- downloadHandler(paste0("ind_anova_",Sys.Date(),".xlsx"), function(f){ req(anova_r()); v<-input$anova_var; write_xlsx(tryCatch(safe_df(anova_r()$ind[[v]]$individual),error=function(e)data.frame()),f) })
    output$dl_anova_pool  <- downloadHandler(paste0("pool_anova_",Sys.Date(),".xlsx"), function(f){ req(anova_r()); v<-input$anova_var; write_xlsx(tryCatch(safe_df(anova_r()$pool[[v]]$anova),error=function(e)data.frame()),f) })

    # Stability ANOVA-based
    sa_r <- eventReactive(input$btn_stab_anova, {
      req(processed_data(),input$stab_anova_var); df<-processed_data(); vs<-sym(input$stab_anova_var)
      list(ann=tryCatch(Annicchiarico(df,ENV,GEN,REP,!!vs),error=function(e)NULL),
           eco=tryCatch(ecovalence(df,ENV,GEN,REP,!!vs),error=function(e)NULL),
           shukla=tryCatch(Shukla(df,ENV,GEN,REP,!!vs),error=function(e)NULL))
    })
    ann_tab <- function(part) renderDT({ req(sa_r()); v<-input$stab_anova_var
      df <- tryCatch({ sub<-sa_r()$ann; x<-if(v %in% names(sub)) sub[[v]][[part]] else sub[[part]]; data.frame(as.list(x),check.names=FALSE,stringsAsFactors=FALSE) },error=function(e) data.frame(Error=e$message)); fmt_dt(df) })
    output$tbl_ann_gen <- ann_tab("general"); output$tbl_ann_fav <- ann_tab("favorable"); output$tbl_ann_unf <- ann_tab("unfavorable")
    output$tbl_eco    <- renderDT({ req(sa_r()); fmt_dt(tryCatch(metan_df(sa_r()$eco,input$stab_anova_var),error=function(e)data.frame(Error=e$message))) })
    output$tbl_shukla <- renderDT({ req(sa_r()); fmt_dt(tryCatch(metan_df(sa_r()$shukla,input$stab_anova_var),error=function(e)data.frame(Error=e$message))) })
    r_plt_shukla <- reactive({ req(sa_r()); v<-input$stab_anova_var; df<-tryCatch(metan_df(sa_r()$shukla,v),error=function(e)NULL); req(!is.null(df),"GEN" %in% names(df))
      vc<-grep("ShuklaVar|Shukla|shukla|s2_i|Wi|var_i",names(df),value=TRUE,ignore.case=TRUE)[1]
      if(is.na(vc)||is.null(vc)) vc<-names(df)[which(sapply(df,is.numeric)&names(df)!="Y")][1]; req(!is.na(vc))
      ggplot(df,aes(x=reorder(GEN,.data[[vc]]),y=.data[[vc]],fill=.data[[vc]]))+geom_col(color="white",linewidth=.3)+coord_flip()+scale_fill_gradientn(colors=c("#2D6A4F","#95D5B2","#F4A300","#C62828"),name="Value")+labs(title="Shukla Variance by Genotype",subtitle="Lower = more stable",x="Genotype",y="Shukla Variance")+theme_met() })
    output$plt_shukla    <- renderPlotly({ req(r_plt_shukla()); ggplotly(r_plt_shukla()) })
    output$dl_plt_shukla <- downloadHandler(paste0("shukla_",Sys.Date(),".pdf"), function(f){ req(r_plt_shukla()); save_pdf(r_plt_shukla(),f,14,10) })
    output$dl_ann <- downloadHandler(paste0("annicchiarico_",Sys.Date(),".xlsx"), function(f){ req(sa_r()); v<-input$stab_anova_var; r<-sa_r()$ann; req(r); sub<-if(!is.null(v)&&v %in% names(r)) r[[v]] else r; mk<-function(pt) tryCatch({x<-sub[[pt]];data.frame(as.list(x),check.names=FALSE,stringsAsFactors=FALSE)},error=function(e)data.frame(Info=paste(pt,"not available"))); write_xlsx(list(General=mk("general"),Favorable=mk("favorable"),Unfavorable=mk("unfavorable")),f) })
    output$dl_eco <- downloadHandler(paste0("ecovalence_",Sys.Date(),".xlsx"), function(f){ req(sa_r()); write_xlsx(tryCatch(metan_df(sa_r()$eco,input$stab_anova_var),error=function(e)data.frame(Error=e$message)),f) })
    output$dl_shk <- downloadHandler(paste0("shukla_",Sys.Date(),".xlsx"),     function(f){ req(sa_r()); write_xlsx(tryCatch(metan_df(sa_r()$shukla,input$stab_anova_var),error=function(e)data.frame(Error=e$message)),f) })

    # Stability Regression
    sr_r <- eventReactive(input$btn_stab_reg, { req(processed_data(),input$stab_reg_var); vs<-sym(input$stab_reg_var); tryCatch(ge_reg(processed_data(),ENV,GEN,REP,!!vs),error=function(e)NULL) })
    output$tbl_reg_anova <- renderDT({ req(sr_r()); v<-input$stab_reg_var; fmt_dt(tryCatch(safe_df(sr_r()[[v]]$anova),error=function(e)data.frame(Error=e$message))) })
    output$plt_reg       <- renderPlot({ req(sr_r()); tryCatch(plot(sr_r()),error=function(e) ggplot()+annotate("text",x=.5,y=.5,label=e$message)+theme_void()) })
    output$dl_plt_reg    <- downloadHandler(paste0("reg_plot_",Sys.Date(),".pdf"), function(f){ req(sr_r()); cairo_pdf(f,16,10); tryCatch(plot(sr_r()),error=function(e){plot.new();title(e$message)}); dev.off() })
    output$dl_stab_reg   <- downloadHandler(paste0("reg_anova_",Sys.Date(),".xlsx"), function(f){ req(sr_r()); v<-input$stab_reg_var; write_xlsx(tryCatch(safe_df(sr_r()[[v]]$anova),error=function(e)data.frame()),f) })

    # Stability Non-parametric
    np_r <- eventReactive(input$btn_stab_np, { req(processed_data(),input$stab_np_var); df<-processed_data(); vs<-sym(input$stab_np_var)
      list(super=tryCatch(superiority(df,ENV,GEN,!!vs),error=function(e)NULL), fox=tryCatch(Fox(df,ENV,GEN,!!vs),error=function(e)NULL)) })
    output$tbl_superiority <- renderDT({ req(np_r()); fmt_dt(tryCatch(metan_df(np_r()$super,input$stab_np_var),error=function(e)data.frame(Error=e$message))) })
    output$tbl_fox         <- renderDT({ req(np_r()); fmt_dt(tryCatch(metan_df(np_r()$fox,input$stab_np_var),error=function(e)data.frame(Error=e$message))) })
    r_plt_np <- reactive({
      req(np_r()); v <- input$stab_np_var
      df <- tryCatch(metan_df(np_r()$super, v), error=function(e) NULL)
      req(!is.null(df), is.data.frame(df))
      # Normalise genotype column name (metan may return "gen" or "GEN")
      gen_col <- grep("^gen$", names(df), ignore.case=TRUE, value=TRUE)[1]
      req(!is.na(gen_col))
      if (gen_col != "GEN") names(df)[names(df)==gen_col] <- "GEN"
      vc <- grep("^Pi_a$|^Pi$|^W_i$|^Pi_f$|^Pi_u$|general", names(df), value=TRUE, ignore.case=TRUE)[1]
      if (is.na(vc)||is.null(vc)) vc <- names(df)[which(sapply(df, is.numeric))][1]
      req(!is.na(vc))
      ggplot(df, aes(x=reorder(GEN, -.data[[vc]]), y=.data[[vc]], fill=.data[[vc]])) +
        geom_col(color="white", linewidth=.3) +
        coord_flip() +
        scale_fill_gradientn(colors=c("#C62828","#F4A300","#95D5B2","#2D6A4F"), name=vc, labels=scales::comma) +
        labs(title="Lin & Binns Superiority Index", subtitle="Lower Pi = more stable & widely adapted",
             x="Genotype", y=paste("Superiority Index (", vc, ")")) +
        theme_met() + theme(legend.position="none")
    })
    output$plt_np    <- renderPlotly({ req(r_plt_np()); ggplotly(r_plt_np()) })
    output$dl_plt_np <- downloadHandler(paste0("superiority_",Sys.Date(),".pdf"), function(f){ req(r_plt_np()); save_pdf(r_plt_np(),f,14,10) })
    output$dl_super  <- downloadHandler(paste0("superiority_",Sys.Date(),".xlsx"), function(f){ req(np_r()); v<-input$stab_np_var; write_xlsx(tryCatch(metan_df(np_r()$super,v),error=function(e)data.frame(Error=e$message)),f) })
    output$dl_fox    <- downloadHandler(paste0("fox_",Sys.Date(),".xlsx"), function(f){ req(np_r()); write_xlsx(tryCatch(metan_df(np_r()$fox,input$stab_np_var),error=function(e)data.frame(Error=e$message)),f) })

    # Stability Factor Analysis
    fa_r <- eventReactive(input$btn_stab_fa, { req(processed_data(),input$stab_fa_var); vs<-sym(input$stab_fa_var); tryCatch(ge_factanal(processed_data(),ENV,GEN,REP,!!vs),error=function(e)NULL) })
    output$tbl_fa_scores <- renderDT({ req(fa_r()); v<-input$stab_fa_var; fmt_dt(tryCatch(safe_df(fa_r()[[v]]$scores.gen),error=function(e)data.frame(Error=e$message))) })
    output$tbl_fa_loads  <- renderDT({ req(fa_r()); v<-input$stab_fa_var; fmt_dt(tryCatch(safe_df(fa_r()[[v]]$finish.loadings),error=function(e)data.frame(Error=e$message))) })
    output$tbl_fa_pca    <- renderDT({ req(fa_r()); v<-input$stab_fa_var; fmt_dt(tryCatch(safe_df(fa_r()[[v]]$PCA),error=function(e)data.frame(Error=e$message))) })
    output$plt_fa        <- renderPlot({ req(fa_r()); tryCatch(plot(fa_r()),error=function(e) ggplot()+annotate("text",x=.5,y=.5,label=paste("Error:",e$message),size=4)+theme_void()) })
    output$dl_plt_fa     <- downloadHandler(paste0("factor_biplot_",Sys.Date(),".pdf"), function(f){ req(fa_r()); cairo_pdf(f,16,12); tryCatch(plot(fa_r()),error=function(e){plot.new();title(e$message)}); dev.off() })
    output$dl_fa         <- downloadHandler(paste0("factor_analysis_",Sys.Date(),".xlsx"), function(f){ req(fa_r()); v<-input$stab_fa_var; r<-fa_r(); write_xlsx(list(Scores=tryCatch(safe_df(r[[v]]$scores.gen),error=function(e)data.frame()),Loadings=tryCatch(safe_df(r[[v]]$finish.loadings),error=function(e)data.frame()),PCA=tryCatch(safe_df(r[[v]]$PCA),error=function(e)data.frame())),f) })

    # Stability Wrap
    wrap_r <- eventReactive(input$btn_wrap, { req(processed_data(),input$wrap_var); vs<-sym(input$wrap_var)
      m <- tryCatch(ge_stats(processed_data(),ENV,GEN,REP,!!vs),error=function(e)NULL); req(m)
      list(params=tryCatch(get_model_data(m),error=function(e)NULL), ranks=tryCatch(get_model_data(m,"ranks"),error=function(e)NULL)) })
    output$tbl_wrap      <- renderDT({ req(wrap_r(),wrap_r()$params); fmt_dt(as.data.frame(wrap_r()$params),20) })
    output$tbl_wrap_rank <- renderDT({ req(wrap_r(),wrap_r()$ranks);  fmt_dt(as.data.frame(wrap_r()$ranks),20) })
    output$plt_wrap_cor  <- renderPlot({ req(wrap_r(),wrap_r()$params)
      df <- as.data.frame(wrap_r()$params) %>% select(where(is.numeric)) %>% select(any_of(c("Y","CV","Shukla","Wi_g","ASV","WAASB","HMGV","RPGV","HMRPGV","Pi_a","S1","S2","N1","N2","S3","N3","ACV")))
      validate(need(ncol(df)>=2,"Not enough numeric columns."))
      cm <- cor(df, use="complete.obs")
      corrplot(cm,method="color",type="upper",order="hclust",addCoef.col="white",tl.cex=.9,number.cex=.65,col=colorRampPalette(c("#C62828","white","#2D6A4F"))(200),tl.col="#0D3B2E",mar=c(0,0,2,0),title="Stability Parameter Correlations") })
    output$dl_plt_cor    <- downloadHandler(paste0("stability_cor_",Sys.Date(),".pdf"), function(f){ req(wrap_r()); df<-as.data.frame(wrap_r()$params)%>%select(where(is.numeric))%>%select(any_of(c("Y","CV","Shukla","Wi_g","ASV","WAASB","HMGV","RPGV","HMRPGV","Pi_a","S1","S2","N1","N2"))); cm<-cor(df,use="complete.obs"); cairo_pdf(f,14,12); corrplot(cm,method="color",type="upper",order="hclust",addCoef.col="white",tl.cex=1,number.cex=.75,col=colorRampPalette(c("#C62828","white","#2D6A4F"))(200),tl.col="#0D3B2E",mar=c(0,0,3,0),title="Stability Parameter Correlations"); dev.off() })
    output$dl_wrap       <- downloadHandler(paste0("wrap_stability_",Sys.Date(),".xlsx"), function(f){ req(wrap_r()); write_xlsx(as.data.frame(wrap_r()$params),f) })
    output$dl_wrap_rank  <- downloadHandler(paste0("wrap_rankings_",Sys.Date(),".xlsx"),  function(f){ req(wrap_r()); write_xlsx(as.data.frame(wrap_r()$ranks),f) })

    # AMMI
    ammi_r <- eventReactive(input$btn_ammi, { req(processed_data(),input$ammi_var); df<-processed_data(); vs<-sym(input$ammi_var)
      amod <- tryCatch(performs_ammi(df,ENV,GEN,REP,!!vs),error=function(e)NULL); req(amod)
      waas1 <- tryCatch(waas(df,ENV,GEN,REP,!!vs),error=function(e)NULL)
      list(ammi=amod, waas=waas1) })
    output$tbl_ammi_anova <- renderDT({ req(ammi_r()); v<-input$ammi_var; fmt_dt(tryCatch(safe_df(ammi_r()$ammi[[v]]$ANOVA),error=function(e)data.frame(Error=e$message))) })
    output$tbl_ipca       <- renderDT({ req(ammi_r()); fmt_dt(tryCatch(safe_df(get_model_data(ammi_r()$ammi,"ipca_pval")),error=function(e)data.frame(Error=e$message))) })
    bap <- function(type_n, mk="ammi") reactive({ req(ammi_r()); m<-ammi_r()[[mk]]; req(m)
      tryCatch(plot_scores(m,type=type_n)+theme_met()+labs(caption="Dr. Vijay Kamal Meena | AU Jodhpur"),
               error=function(e) ggplot()+annotate("text",x=.5,y=.5,label=paste("Plot unavailable:",e$message),size=4)+theme_void()) })
    r_ammi1<-bap(1,"ammi"); r_ammi2<-bap(2,"ammi"); r_waas<-bap(3,"waas")
    output$plt_ammi1    <- renderPlot({ req(r_ammi1()); r_ammi1() })
    output$plt_ammi2    <- renderPlot({ req(r_ammi2()); r_ammi2() })
    output$plt_waas     <- renderPlot({ req(r_waas());  r_waas()  })
    output$dl_plt_ammi1 <- downloadHandler(paste0("AMMI1_",Sys.Date(),".pdf"), function(f){ req(r_ammi1()); save_pdf(r_ammi1(),f,16,12) })
    output$dl_plt_ammi2 <- downloadHandler(paste0("AMMI2_",Sys.Date(),".pdf"), function(f){ req(r_ammi2()); save_pdf(r_ammi2(),f,16,12) })
    output$dl_plt_waas  <- downloadHandler(paste0("WAAS_",Sys.Date(),".pdf"),  function(f){ req(r_waas()); save_pdf(r_waas(),f,16,12) })
    output$tbl_ammi_idx <- renderDT({ req(ammi_r()); sc<-tryCatch(plot_scores(ammi_r()$ammi),error=function(e)NULL); req(sc,!is.null(sc$data)); df<-sc$data
      if(all(c("PC1","PC2","Y") %in% names(df))){ ss1<-tryCatch({pv<-get_model_data(ammi_r()$ammi,"ipca_pval");as.numeric(pv[1,"percent"])},error=function(e)60); ss2<-100-ss1; df<-df%>%mutate(ASV=sqrt((PC1^2*(ss1/ss2))+PC2^2),Rank_Y=rank(-Y),Rank_Stability=rank(ASV),Combined_Rank=Rank_Y+Rank_Stability)%>%arrange(Combined_Rank) }
      fmt_dt(df,20) })
    output$dl_ammi_anova <- downloadHandler(paste0("ammi_anova_",Sys.Date(),".xlsx"), function(f){ req(ammi_r()); v<-input$ammi_var; write_xlsx(tryCatch(safe_df(ammi_r()$ammi[[v]]$ANOVA),error=function(e)data.frame()),f) })
    output$dl_ammi_idx   <- downloadHandler(paste0("ammi_index_",Sys.Date(),".xlsx"), function(f){ req(ammi_r()); sc<-tryCatch(plot_scores(ammi_r()$ammi),error=function(e)NULL); req(sc); write_xlsx(as.data.frame(sc$data),f) })

    # GGE
    gge_r <- eventReactive(input$btn_gge, { req(processed_data(),input$gge_var,input$gge_svp); df<-processed_data(); vs<-sym(input$gge_var); svp<-input$gge_svp
      list(env=tryCatch(gge(df,ENV,GEN,!!vs,svp="environment"),error=function(e)NULL),
           gen=tryCatch(gge(df,ENV,GEN,!!vs,svp="genotype"),error=function(e)NULL),
           sym=tryCatch(gge(df,ENV,GEN,!!vs,svp="symmetrical"),error=function(e)NULL),
           sel=tryCatch(gge(df,ENV,GEN,!!vs,svp=svp),error=function(e)NULL)) })
    bgp <- function(type_n, mk="sym") reactive({ req(gge_r()); m<-gge_r()[[mk]] %||% gge_r()$sel; req(m)
      tryCatch(plot(m,type=type_n)+theme_met()+labs(caption="Dr. Vijay Kamal Meena | AU Jodhpur"),
               error=function(e) ggplot()+annotate("text",x=.5,y=.5,label=paste("Try different SVP. Error:",e$message),size=3.5)+theme_void()) })
    r_gge1<-bgp(1,"sym"); r_gge2<-bgp(2,"gen"); r_gge3<-bgp(3,"sym"); r_gge4<-bgp(4,"env"); r_gge5<-bgp(8,"gen"); r_gge6<-bgp(6,"env"); r_gge7<-bgp(10,"env")
    output$plt_gge1<-renderPlot({req(r_gge1());r_gge1()}); output$plt_gge2<-renderPlot({req(r_gge2());r_gge2()}); output$plt_gge3<-renderPlot({req(r_gge3());r_gge3()})
    output$plt_gge4<-renderPlot({req(r_gge4());r_gge4()}); output$plt_gge5<-renderPlot({req(r_gge5());r_gge5()}); output$plt_gge6<-renderPlot({req(r_gge6());r_gge6()}); output$plt_gge7<-renderPlot({req(r_gge7());r_gge7()})
    dlgp <- function(r_plt, nm) downloadHandler(paste0(nm,"_",Sys.Date(),".pdf"), function(f){ req(r_plt()); save_pdf(r_plt(),f,16,13) })
    output$dl_plt_gge1<-dlgp(r_gge1,"GGE_basic"); output$dl_plt_gge2<-dlgp(r_gge2,"GGE_mean_stability"); output$dl_plt_gge3<-dlgp(r_gge3,"GGE_WWW")
    output$dl_plt_gge4<-dlgp(r_gge4,"GGE_discrim"); output$dl_plt_gge5<-dlgp(r_gge5,"GGE_rank_gen"); output$dl_plt_gge6<-dlgp(r_gge6,"GGE_rank_env"); output$dl_plt_gge7<-dlgp(r_gge7,"GGE_relation")
    output$dl_gge <- downloadHandler(paste0("gge_predictions_",Sys.Date(),".xlsx"), function(f){ req(gge_r()); v<-input$gge_var; m<-gge_r()$sym%||%gge_r()$env%||%gge_r()$gen; req(m); pred<-tryCatch(predict(m),error=function(e)NULL); req(pred); write_xlsx(tryCatch(safe_df(pred[[v]]),error=function(e)data.frame(Error=e$message)),f) })
  })
}

# ════════════════════════════════════════════════════════════════
#  MT MODULE — HELPERS
# ════════════════════════════════════════════════════════════════
get_selected_n  <- function(total_n, si) max(1, round(total_n*si/100,0))
as_goal_words   <- function(goals) ifelse(goals=="l","decrease","increase")

make_blup_matrix <- function(model_obj, traits) {
  out <- tibble(GEN=as.character(model_obj[[traits[1]]]$BLUPgen$GEN))
  for (tr in traits) { tmp <- model_obj[[tr]]$BLUPgen %>% transmute(GEN=as.character(GEN),!!tr:=Predicted); out <- left_join(out,tmp,by="GEN") }
  out
}
build_table2 <- function(model_obj, raw_data, traits) {
  bind_rows(lapply(traits, function(tr) {
    rnd<-model_obj[[tr]]$random; est<-model_obj[[tr]]$ESTIMATES
    gv<-tryCatch(rnd$Variance[rnd$Group=="GEN"],error=function(e)NA)
    gei<-tryCatch(rnd$Variance[rnd$Group=="GEN:ENV"],error=function(e)NA)
    res<-tryCatch(rnd$Variance[rnd$Group=="Residual"],error=function(e)NA)
    gp<-function(p) tryCatch(est$Values[est$Parameters==p],error=function(e)NA)
    tibble(Trait=tr,Mean=mean(raw_data[[tr]],na.rm=TRUE),GenotypicVar=gv,GEIVar=gei,ResidualVar=res,PhenotypicVar=gp("Phenotypic variance"),h2=gp("Heritability"),GEIr2=gp("GEIr2"),H2mg=gp("h2mg"),Accuracy=gp("Accuracy"),rge=gp("rge"),CVg=gp("CVg"),CVr=gp("CVr"),CVratio=gp("CV ratio"))
  }))
}
selection_differential <- function(blup_mat, selected_geno, goals, method_nm) {
  sel <- blup_mat %>% filter(GEN %in% selected_geno)
  bind_rows(lapply(names(goals), function(tr) {
    xo<-mean(blup_mat[[tr]],na.rm=TRUE); xs<-mean(sel[[tr]],na.rm=TRUE); gn<-xs-xo
    tibble(Method=method_nm,Trait=tr,Goal=as_goal_words(goals[[tr]]),OverallMean=xo,SelectedMean=xs,SD=gn,SDpercent=gn/abs(xo)*100,DesiredGain=ifelse((goals[[tr]]=="h"&gn>0)|(goals[[tr]]=="l"&gn<0),"Yes","No"))
  }))
}
make_sh_index <- function(blup_mat, raw_data, traits, goals, si) {
  pm <- raw_data %>% group_by(GEN) %>% summarise(across(all_of(traits),~mean(.x,na.rm=TRUE)),.groups="drop")
  pcov <- pm%>%select(-GEN)%>%as.matrix()%>%cov(); gcov <- blup_mat%>%select(-GEN)%>%as.matrix()%>%cov()
  weights <- ifelse(goals=="l",-1,1)
  Smith_Hazel(blup_mat%>%column_to_rownames("GEN")%>%as.matrix(),pcov=pcov,gcov=gcov,weights=weights,SI=si)
}
selection_overlap_table <- function(selected_list, total_n) {
  methods <- names(selected_list)
  expand_grid(Method1=methods,Method2=methods) %>%
    mutate(CoincidenceIndex=map2_dbl(Method1,Method2,~tryCatch(coincidence_index(sel1=selected_list[[.x]],sel2=selected_list[[.y]],total=total_n),error=function(e)NA_real_)))
}
selection_membership_table <- function(selected_list, all_genotypes) {
  out <- tibble(GEN=all_genotypes)
  for (nm in names(selected_list)) out[[nm]] <- out$GEN %in% selected_list[[nm]]
  out %>% mutate(MethodsSelected=rowSums(across(-GEN)), Pattern=apply(across(-c(GEN,MethodsSelected)),1,function(x){ hits<-names(x)[as.logical(x)]; if(!length(hits))"None" else paste(hits,collapse=" + ") })) %>% arrange(desc(MethodsSelected),GEN)
}
format_genotype_block <- function(x, per_line=3) {
  x <- sort(unique(as.character(x))); if(!length(x)) return("")
  paste(vapply(split(x,ceiling(seq_along(x)/per_line)),paste,collapse=", ",character(1)),collapse="\n")
}
make_venn_genotype_table <- function(selected_list) {
  membership <- tibble(GEN=Reduce(union,lapply(selected_list,as.character)))
  for (nm in names(selected_list)) membership[[nm]] <- membership$GEN %in% selected_list[[nm]]
  membership %>% rowwise() %>% mutate(Region={hits<-names(selected_list)[c_across(all_of(names(selected_list)))]; if(!length(hits))"None" else paste(hits,collapse="&")}) %>% ungroup() %>% filter(Region!="None") %>% group_by(Region) %>% summarise(Count=n(),Genotypes=format_genotype_block(GEN,per_line=2),.groups="drop")
}
make_venn_region_table <- function(selected_list) {
  membership <- tibble(GEN=Reduce(union,lapply(selected_list,as.character)))
  for (nm in names(selected_list)) membership[[nm]] <- membership$GEN %in% selected_list[[nm]]
  membership %>% rowwise() %>% mutate(Region={hits<-names(selected_list)[c_across(all_of(names(selected_list)))]; if(!length(hits))"None" else paste(hits,collapse="&")}) %>% ungroup() %>% count(Region,name="Count")
}
make_contribution_plot <- function(contrib_tbl, title_text) {
  df <- as.data.frame(contrib_tbl)
  if("GEN" %in% colnames(df)) df <- df %>% rename(Genotype=GEN) else df <- df %>% tibble::rownames_to_column("Genotype")
  nc <- df %>% select(where(is.numeric)) %>% colnames()
  df %>% pivot_longer(cols=all_of(nc),names_to="Factor",values_to="Contribution") %>%
    group_by(Factor) %>% summarise(Contribution=mean(Contribution,na.rm=TRUE)) %>% arrange(desc(Contribution)) %>%
    ggplot(aes(x=reorder(Factor,Contribution),y=Contribution))+geom_col(fill="#2D6A4F",width=.7)+coord_flip()+
    labs(title=title_text,x="Factors",y="Contribution (%)")+theme_met()
}
make_circular_index_plot <- function(index_tbl, genotype_col, value_col, selected_genotypes, title_text, y_lab, lower_is_better=TRUE) {
  plot_tbl <- index_tbl %>% transmute(GEN=as.character(.data[[genotype_col]]),IndexValue=as.numeric(.data[[value_col]]))
  plot_tbl <- if(lower_is_better) arrange(plot_tbl,IndexValue) else arrange(plot_tbl,desc(IndexValue))
  n_gen    <- nrow(plot_tbl)
  plot_tbl <- plot_tbl %>% mutate(Rank=row_number(),Selected=GEN %in% selected_genotypes)
  n_sel    <- sum(plot_tbl$Selected)
  cutpoint <- if(n_sel>0){ if(lower_is_better) max(plot_tbl$IndexValue[plot_tbl$Selected],na.rm=TRUE) else min(plot_tbl$IndexValue[plot_tbl$Selected],na.rm=TRUE) } else NA_real_
  vr       <- diff(range(plot_tbl$IndexValue,na.rm=TRUE)); if(!is.finite(vr)||vr==0) vr<-1
  lbl_size <- max(1.6, min(3.0, 60/n_gen))
  stagger  <- ifelse(seq_len(n_gen) %% 2 == 0, 0.50, 0.75)
  label_tbl <- plot_tbl %>%
    mutate(angle_raw = 90-360*(Rank-.5)/n(),
           angle     = ifelse(angle_raw < -90, angle_raw+180, angle_raw),
           hjust     = ifelse(angle_raw < -90, 1, 0),
           label_y   = if(lower_is_better) min(IndexValue,na.rm=TRUE)-vr*stagger
                       else                max(IndexValue,na.rm=TRUE)+vr*stagger)
  p <- ggplot(plot_tbl,aes(x=Rank,y=IndexValue,group=1)) +
    geom_path(color="grey35",linewidth=.45) +
    geom_point(aes(fill=Selected),shape=21,size=2.5,color="black",stroke=.25) +
    geom_hline(yintercept=cutpoint,color="#D55E00",linewidth=.5,linetype="dashed") +
    geom_text(data=label_tbl,aes(x=Rank,y=label_y,label=GEN,angle=angle,hjust=hjust),
              inherit.aes=FALSE,size=lbl_size,fontface="bold") +
    scale_fill_manual(values=c("FALSE"="grey82","TRUE"="#D55E00")) +
    coord_polar() +
    labs(title=title_text,
         subtitle=paste0("n=",n_gen," genotypes | ",n_sel," selected (orange) | dashed = cut-point"),
         y=y_lab,x=NULL) +
    theme_minimal(base_size=12) +
    theme(legend.position="none",axis.text.x=element_blank(),axis.title.x=element_blank(),
          panel.grid.minor=element_blank(),plot.margin=margin(45,45,45,45),
          plot.title=element_text(face="bold",size=14),
          plot.subtitle=element_text(size=9,color="grey50"),
          axis.title.y=element_text(face="bold",size=11))
  if(lower_is_better) p <- p+scale_y_reverse()
  p
}
make_custom_biplot <- function(model, label_genotypes, title_text, point_color="#2C7FB8", vector_color="#D95F0E") {
  gen_tbl <- as.data.frame(model$coordgen[,1:2]) %>% setNames(c("PC1","PC2")) %>% mutate(label=model$labelgen,highlight=label %in% label_genotypes)
  env_tbl <- as.data.frame(model$coordenv[,1:2]) %>% setNames(c("PC1","PC2")) %>% mutate(label=model$labelenv)
  xl <- paste0(model$labelaxes[1]," (",round(model$varexpl[1],2),"%)"); yl <- paste0(model$labelaxes[2]," (",round(model$varexpl[2],2),"%)")
  ggplot()+geom_hline(yintercept=0,color="grey70",linewidth=.4)+geom_vline(xintercept=0,color="grey70",linewidth=.4)+
    geom_segment(data=env_tbl,aes(x=0,y=0,xend=PC1,yend=PC2),color=vector_color,linewidth=.6,arrow=arrow(length=grid::unit(.18,"cm")))+
    geom_point(data=gen_tbl,aes(PC1,PC2),color=point_color,alpha=.55,size=1.8)+
    geom_point(data=gen_tbl%>%filter(highlight),aes(PC1,PC2),color="#B30000",size=2.2)+
    geom_text_repel(data=gen_tbl%>%filter(highlight),aes(PC1,PC2,label=label),size=3,color="#B30000",max.overlaps=Inf)+
    geom_text_repel(data=env_tbl,aes(PC1,PC2,label=label),size=3.2,color=vector_color,fontface="bold",max.overlaps=Inf)+
    coord_equal()+labs(title=title_text,x=xl,y=yl)+theme_met()
}
make_four_set_venn <- function(selected_list, title_text="Venn Diagram of Selected Genotypes") {
  req(length(selected_list)==4); snv <- names(selected_list)
  region_counts <- make_venn_genotype_table(selected_list)
  rp <- tribble(~Region,~x,~y, snv[1],-2.65,2.15, snv[2],2.65,2.15, snv[3],-2.65,-2.15, snv[4],2.65,-2.15,
    paste(snv[c(1,2)],collapse="&"),0.00,2.65, paste(snv[c(1,3)],collapse="&"),-2.65,0.00,
    paste(snv[c(2,4)],collapse="&"),2.65,0.00, paste(snv[c(3,4)],collapse="&"),0.00,-2.65,
    paste(snv[c(1,4)],collapse="&"),-0.95,0.95, paste(snv[c(2,3)],collapse="&"),0.95,0.95,
    paste(snv[c(1,2,3)],collapse="&"),-0.75,1.55, paste(snv[c(1,2,4)],collapse="&"),0.75,1.55,
    paste(snv[c(1,3,4)],collapse="&"),-0.75,-0.35, paste(snv[c(2,3,4)],collapse="&"),0.75,-0.35,
    paste(snv,collapse="&"),0.00,0.35)
  lt <- rp %>% left_join(region_counts,by="Region") %>% mutate(Count=replace_na(Count,0L),Genotypes=replace_na(Genotypes,""))
  ct <- tibble(x0=c(-1,1,-1,1),y0=c(1,1,-1,-1),r=2,fill=c("#E41A1C","#377EB8","#4DAF4A","#984EA3"),label=snv,lx=c(-2.95,2.95,-2.95,2.95),ly=c(3.2,3.2,-3.2,-3.2))
  ggplot()+geom_circle(data=ct,aes(x0=x0,y0=y0,r=r,fill=fill),color=NA,alpha=.25,inherit.aes=FALSE,show.legend=FALSE)+scale_fill_identity()+geom_text(data=ct,aes(lx,ly,label=label),fontface="bold",size=5)+geom_text(data=lt%>%filter(Genotypes!=""),aes(x,y,label=Genotypes),size=2.7,fontface="bold",lineheight=.92)+coord_fixed(xlim=c(-5,5),ylim=c(-4.5,4.5),clip="off")+labs(title=title_text)+theme_void(base_size=12)+theme(plot.title=element_text(face="bold",size=16,hjust=.5))
}

# ════════════════════════════════════════════════════════════════
#  MT MODULE — UI
# ════════════════════════════════════════════════════════════════
mtUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabItem("mt_home",
      div(class="home-banner",h2("🌿 Multi-Trait Selection Analysis Suite"),p("MTSI · MGIDI · FAI-BLUP · Smith-Hazel · Direct Selection | GT & GYT Biplots | Venn & Radar"),div(class="vtag","VERSION 1.0 | 2025")),
      fluidRow(
        column(3,div(class="feat-card",div(class="fi","🎯"),h4("MTSI"),p("Multi-Trait Stability Index from waasb model."))),
        column(3,div(class="feat-card",div(class="fi","🧬"),h4("MGIDI"),p("Multi-trait Genotype-Ideotype Distance Index."))),
        column(3,div(class="feat-card",div(class="fi","🔢"),h4("FAI-BLUP"),p("Factor Analysis and Ideotype-Design BLUPs."))),
        column(3,div(class="feat-card",div(class="fi","⚖️"),h4("Smith-Hazel"),p("Classical selection index with covariance matrices.")))
      )
    ),
    tabItem("mt_data",
      div(class="sec-bar","📂 MT — Data & Settings"),
      fluidRow(
        box(title="Upload CSV & Column Mapping",width=4,status="success",solidHeader=TRUE,
          fileInput(ns("file1"),"Choose CSV File",accept=".csv"),
          hr(), h5(style="color:#1B4332;font-weight:700;","Column Names"),
          uiOutput(ns("ui_env_col")), uiOutput(ns("ui_gen_col")), uiOutput(ns("ui_rep_col")),
          hr(), h5(style="color:#1B4332;font-weight:700;","Analysis Settings"),
          uiOutput(ns("ui_yield_trait")),
          numericInput(ns("sel_intensity"),"Selection Intensity (%):",15,min=1,max=50), hr(),
          actionButton(ns("btn_load"),"⚙️  Process Data",class="btn-success btn-block",icon=icon("play"))
        ),
        box(title="Trait Goals Configuration",width=5,status="success",solidHeader=TRUE,
          p(style="color:#555;font-size:12px;","Select whether higher or lower values are desired for each trait."),
          uiOutput(ns("ui_trait_goals")), br(), uiOutput(ns("value_boxes"))
        ),
        box(title="Data Preview",width=3,status="success",solidHeader=TRUE, DTOutput(ns("tbl_preview_mini")))
      )
    ),
    tabItem("mt_fit",
      div(class="sec-bar","⚙️ MT — Fit Mixed Models (gamem_met + waasb)"),
      fluidRow(
        box(title="Run Models",width=4,status="success",solidHeader=TRUE,
          div(style="background:#F7FBF8;border:1px solid #D8EDE1;border-radius:8px;padding:11px;margin-bottom:11px;",
              h5(style="color:#1B4332;margin:0 0 7px;","Selected Traits:"), uiOutput(ns("ui_selected_traits_display")),
              h5(style="color:#1B4332;margin:7px 0;","Trait Goals:"), uiOutput(ns("ui_goals_display"))),
          actionButton(ns("btn_fit"),"▶  Fit gamem_met + waasb",class="btn-success btn-block",icon=icon("play")),
          p(style="color:#888;font-size:11px;margin-top:7px;","⚠️ Model fitting may take several minutes."),
          hr(), uiOutput(ns("fit_status"))
        ),
        box(title="Model Output",width=8,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("gamem_met Summary", br(), verbatimTextOutput(ns("txt_gamem"))),
            tabPanel("waasb Summary",     br(), verbatimTextOutput(ns("txt_waasb"))),
            tabPanel("BLUP Matrix",       br(), DTOutput(ns("tbl_blup")))
          )
        )
      )
    ),
    tabItem("mt_varcomp",
      div(class="sec-bar","📊 MT — Variance Components (Table 2)"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_table2"),"▶  Build Table 2",class="btn-success btn-block",icon=icon("table")), hr(),
          downloadButton(ns("dl_table2"),"⬇  Download Excel",class="btn-warning btn-block")
        ),
        box(title="Table 2 — Variance Components",width=9,status="success",solidHeader=TRUE, DTOutput(ns("tbl_varcomp")))
      )
    ),
    tabItem("mt_mtsi",
      div(class="sec-bar","🎯 MT — MTSI"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_mtsi"),"▶  Run MTSI",class="btn-success btn-block",icon=icon("play")), hr(),
          downloadButton(ns("dl_mtsi_idx"),    "⬇  MTSI Index",         class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_mtsi_contrib"),"⬇  Factor Contribution", class="btn-warning btn-block")
        ),
        box(title="MTSI Results",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 MTSI Index",    br(), DTOutput(ns("tbl_mtsi"))),
            tabPanel("📋 Selected",      br(), verbatimTextOutput(ns("txt_mtsi_sel"))),
            tabPanel("📊 Circular Plot",        dl_bar(ns("dl_plt_mtsi_circ")),   plotOutput(ns("plt_mtsi_circ"),height="500px")),
            tabPanel("📊 Strengths & Weaknesses", dl_bar(ns("dl_plt_mtsi_sw")),    plotOutput(ns("plt_mtsi_sw"),height="520px")),
            tabPanel("📊 Contribution",           dl_bar(ns("dl_plt_mtsi_contrib")), plotOutput(ns("plt_mtsi_contrib"),height="400px")),
            tabPanel("📊 metan Default",          dl_bar(ns("dl_plt_mtsi_def")),    plotOutput(ns("plt_mtsi_def"),height="400px"))
          )
        )
      )
    ),
    tabItem("mt_mgidi",
      div(class="sec-bar","🧬 MT — MGIDI"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_mgidi"),"▶  Run MGIDI",class="btn-success btn-block",icon=icon("play")), hr(),
          downloadButton(ns("dl_mgidi_idx"),    "⬇  MGIDI Index",        class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_mgidi_contrib"),"⬇  Factor Contribution", class="btn-warning btn-block")
        ),
        box(title="MGIDI Results",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 MGIDI Index",   br(), DTOutput(ns("tbl_mgidi"))),
            tabPanel("📋 Selected",      br(), verbatimTextOutput(ns("txt_mgidi_sel"))),
            tabPanel("📊 Circular Plot",        dl_bar(ns("dl_plt_mgidi_circ")),   plotOutput(ns("plt_mgidi_circ"),height="500px")),
            tabPanel("📊 Strengths & Weaknesses", dl_bar(ns("dl_plt_mgidi_sw")),    plotOutput(ns("plt_mgidi_sw"),height="520px")),
            tabPanel("📊 Contribution",           dl_bar(ns("dl_plt_mgidi_contrib")), plotOutput(ns("plt_mgidi_contrib"),height="400px")),
            tabPanel("📊 metan Default",          dl_bar(ns("dl_plt_mgidi_def")),    plotOutput(ns("plt_mgidi_def"),height="400px"))
          )
        )
      )
    ),
    tabItem("mt_fai",
      div(class="sec-bar","🔢 MT — FAI-BLUP"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_fai"),"▶  Run FAI-BLUP",class="btn-success btn-block",icon=icon("play")), hr(),
          downloadButton(ns("dl_fai_idx"),"⬇  FAI Index",class="btn-warning btn-block")
        ),
        box(title="FAI-BLUP Results",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 FAI Index",  br(), DTOutput(ns("tbl_fai"))),
            tabPanel("📋 Selected",   br(), verbatimTextOutput(ns("txt_fai_sel"))),
            tabPanel("📊 Circular",   dl_bar(ns("dl_plt_fai_circ")), plotOutput(ns("plt_fai_circ"),height="500px")),
            tabPanel("📊 metan Default", dl_bar(ns("dl_plt_fai_def")), plotOutput(ns("plt_fai_def"),height="400px"))
          )
        )
      )
    ),
    tabItem("mt_sh",
      div(class="sec-bar","⚖️ MT — Smith-Hazel"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_sh"),"▶  Run Smith-Hazel",class="btn-success btn-block",icon=icon("play")), hr(),
          downloadButton(ns("dl_sh_idx"),"⬇  SH Index",class="btn-warning btn-block")
        ),
        box(title="Smith-Hazel Results",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 Index",      br(), DTOutput(ns("tbl_sh"))),
            tabPanel("📋 Selected",   br(), verbatimTextOutput(ns("txt_sh_sel"))),
            tabPanel("📊 Circular",   dl_bar(ns("dl_plt_sh_circ")), plotOutput(ns("plt_sh_circ"),height="500px")),
            tabPanel("📊 metan Default", dl_bar(ns("dl_plt_sh_def")), plotOutput(ns("plt_sh_def"),height="400px"))
          )
        )
      )
    ),
    tabItem("mt_direct",
      div(class="sec-bar","📈 MT — Direct Selection"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_direct"),"▶  Run Direct Selection",class="btn-success btn-block",icon=icon("play")), hr(),
          downloadButton(ns("dl_direct"),"⬇  Download Excel",class="btn-warning btn-block")
        ),
        box(title="Direct Selection Results",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 Selected Genotypes", br(), DTOutput(ns("tbl_direct"))),
            tabPanel("📊 Performance Plot",   dl_bar(ns("dl_plt_direct")), plotOutput(ns("plt_direct"),height="420px"))
          )
        )
      )
    ),
    tabItem("mt_table3",
      div(class="sec-bar","📋 MT — Selection Differentials (Table 3)"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_table3"),"▶  Build Table 3",class="btn-success btn-block",icon=icon("table")), hr(),
          downloadButton(ns("dl_table3"),"⬇  Download Excel",class="btn-warning btn-block")
        ),
        box(title="Selection Differentials",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 All Methods", br(), DTOutput(ns("tbl_table3"))),
            tabPanel("📊 SD Heatmap", dl_bar(ns("dl_plt_sd_heat")), plotOutput(ns("plt_sd_heat"),height="420px"))
          )
        )
      )
    ),
    tabItem("mt_coincidence",
      div(class="sec-bar","🔗 MT — Coincidence Index & Membership"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_coincidence"),"▶  Compute",class="btn-success btn-block",icon=icon("play")), hr(),
          downloadButton(ns("dl_coincidence"),"⬇  Coincidence Excel",class="btn-warning btn-block"), br(),
          downloadButton(ns("dl_membership"), "⬇  Membership Excel", class="btn-warning btn-block")
        ),
        box(title="Overlap Analysis",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📋 Coincidence Index",    br(), DTOutput(ns("tbl_coincidence"))),
            tabPanel("📊 Coincidence Heatmap",  dl_bar(ns("dl_plt_ci_heat")), plotOutput(ns("plt_ci_heat"),height="420px")),
            tabPanel("📋 Membership Table",     br(), DTOutput(ns("tbl_membership")))
          )
        )
      )
    ),
    tabItem("mt_venn",
      div(class="sec-bar","🔷 MT — Venn Diagram (4-way)"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          actionButton(ns("btn_venn"),"▶  Draw Venn",class="btn-success btn-block",icon=icon("play")), hr(),
          downloadButton(ns("dl_venn_tbl"),"⬇  Region Table Excel",class="btn-warning btn-block")
        ),
        box(title="Venn Diagram",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("🔷 Venn Diagram", dl_bar(ns("dl_plt_venn")), plotOutput(ns("plt_venn"),height="500px")),
            tabPanel("📋 Region Summary", br(), DTOutput(ns("tbl_venn")))
          )
        )
      )
    ),
    tabItem("mt_biplots",
      div(class="sec-bar","📉 MT — GT / GYT Biplots"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          numericInput(ns("n_highlight"),"Top genotypes to label:",12,min=1,max=50),
          actionButton(ns("btn_biplot"),"▶  Build Biplots",class="btn-success btn-block",icon=icon("play"))
        ),
        box(title="Biplots",width=9,status="success",solidHeader=TRUE,
          tabsetPanel(
            tabPanel("📊 GT Biplot",      dl_bar(ns("dl_plt_gt")),       plotOutput(ns("plt_gt"),height="460px")),
            tabPanel("📊 GYT Biplot",     dl_bar(ns("dl_plt_gyt")),      plotOutput(ns("plt_gyt"),height="460px")),
            tabPanel("📊 Combined GT+GYT",dl_bar(ns("dl_plt_combined")), plotOutput(ns("plt_combined"),height="460px"))
          )
        )
      )
    ),
    tabItem("mt_radar",
      div(class="sec-bar","🌟 MT — Radar Chart"),
      fluidRow(
        box(title="Options",width=3,status="success",solidHeader=TRUE,
          uiOutput(ns("ui_radar_method")), uiOutput(ns("ui_radar_genotypes")),
          actionButton(ns("btn_radar"),"▶  Draw Radar",class="btn-success btn-block",icon=icon("play")), hr(),
          dl_bar(ns("dl_plt_radar"),"⬇  Download HD PDF")
        ),
        box(title="Radar Chart",width=9,status="success",solidHeader=TRUE, plotOutput(ns("plt_radar"),height="500px"))
      )
    ),
    tabItem("mt_export",
      div(class="sec-bar","💾 MT — Export All Results"),
      fluidRow(
        box(title="Download Complete Package",width=6,status="success",solidHeader=TRUE,
          p("Downloads all tables into a single Excel workbook."), br(),
          downloadButton(ns("dl_all_xlsx"),"📦  Download All Tables (Excel)",class="btn-success btn-block",style="font-size:14px;padding:11px;"),
          br(), br(), p(style="color:#888;font-size:12px;","Note: Run all analyses first.")
        ),
        box(title="Individual Downloads",width=6,status="success",solidHeader=TRUE,
          fluidRow(
            column(6, downloadButton(ns("dl_blup_mat"),"BLUP Matrix",class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_table2_ex"),"Table 2 (Var Comp)",class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_table3_ex"),"Table 3 (Sel Diff)",class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_mtsi_ex"),  "MTSI Index",class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_mgidi_ex"), "MGIDI Index",class="btn-warning btn-block")),
            column(6, downloadButton(ns("dl_fai_ex"),   "FAI-BLUP Index",class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_sh_ex"),    "Smith-Hazel Index",class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_coincid_ex"),"Coincidence Index",class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_member_ex"),"Membership Table",class="btn-warning btn-block"), br(),
                      downloadButton(ns("dl_venn_ex"),  "Venn Regions",class="btn-warning btn-block"))
          )
        )
      )
    )
  )
}

# ════════════════════════════════════════════════════════════════
#  MT MODULE — SERVER
# ════════════════════════════════════════════════════════════════
mtServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    raw_data       <- reactiveVal(NULL)
    processed_data <- reactiveVal(NULL)
    gamem_r        <- reactiveVal(NULL)
    waasb_r        <- reactiveVal(NULL)

    observeEvent(input$file1, {
      req(input$file1)
      df <- tryCatch(read.csv(input$file1$datapath,fileEncoding="UTF-8-BOM",stringsAsFactors=FALSE),
                     error=function(e){ showNotification(paste("❌ File error:",e$message),type="error"); NULL })
      raw_data(df)
    })
    observe({
      req(raw_data()); cols <- names(raw_data())
      g <- function(p) grep(p,cols,ignore.case=TRUE,value=TRUE)[1] %||% cols[1]
      output$ui_env_col <- renderUI(selectInput(session$ns("env_col"),"ENV column:",cols,selected=g("^ENV$|environ")))
      output$ui_gen_col <- renderUI(selectInput(session$ns("gen_col"),"GEN column:",cols,selected=g("^GEN$|geno")))
      output$ui_rep_col <- renderUI(selectInput(session$ns("rep_col"),"REP column:",cols,selected=g("^REP$|repli|block")))
    })
    numeric_trait_cols <- reactive({
      req(raw_data()); cols <- names(raw_data())
      non_trait <- c(input$env_col%||%"",input$gen_col%||%"",input$rep_col%||%"")
      nc <- cols[sapply(raw_data(),is.numeric)]; setdiff(nc,non_trait)
    })
    output$ui_yield_trait <- renderUI({ req(numeric_trait_cols()); selectInput(session$ns("yield_trait"),"Yield / Primary Trait:",choices=numeric_trait_cols(),selected=grep("^SY$|yield|^GY$",numeric_trait_cols(),ignore.case=TRUE,value=TRUE)[1]%||%tail(numeric_trait_cols(),1)) })
    output$ui_trait_goals <- renderUI({
      req(numeric_trait_cols()); trs <- numeric_trait_cols()
      tagList(div(class="goal-grid",lapply(trs,function(tr){
        dg <- if(grepl("^DTF$|^DTM$|days|flower|matur",tr,ignore.case=TRUE))"l" else "h"
        div(class="goal-row",span(class="trait-name",tr),radioButtons(session$ns(paste0("goal_",tr)),NULL,choices=c("↑ Higher"="h","↓ Lower"="l"),selected=dg,inline=TRUE))
      })))
    })
    trait_goal_r <- reactive({
      req(numeric_trait_cols()); trs <- numeric_trait_cols()
      goals <- setNames(sapply(trs,function(tr) input[[paste0("goal_",tr)]]%||%"h"),trs)
      goals[!sapply(goals,is.null)]
    })
    selected_traits_r <- reactive({ req(numeric_trait_cols()); numeric_trait_cols() })
    observeEvent(input$btn_load, {
      req(raw_data(),input$env_col,input$gen_col,input$rep_col)
      df <- raw_data()
      names(df)[names(df)==input$env_col] <- "ENV"
      names(df)[names(df)==input$gen_col] <- "GEN"
      names(df)[names(df)==input$rep_col] <- "REP"
      df$ENV <- factor(df$ENV,levels=unique(df$ENV))
      df$GEN <- factor(df$GEN,levels=unique(df$GEN))
      df$REP <- factor(df$REP,levels=unique(df$REP))
      trs <- selected_traits_r()
      for (tr in trs) if(tr %in% names(df)) df[[tr]] <- suppressWarnings(as.numeric(df[[tr]]))
      processed_data(df)
      showNotification("✅ MT data processed!",type="message",duration=3)
    })
    output$value_boxes <- renderUI({ req(processed_data()); df<-processed_data(); trs<-selected_traits_r()
      fluidRow(valueBox(nlevels(df$ENV),"Environments",icon=icon("leaf"),color="green",width=3),valueBox(nlevels(df$GEN),"Genotypes",icon=icon("seedling"),color="teal",width=3),valueBox(nlevels(df$REP),"Replications",icon=icon("clone"),color="orange",width=3),valueBox(length(trs),"Traits",icon=icon("dna"),color="purple",width=3)) })
    output$tbl_preview_mini   <- renderDT({ req(processed_data()); datatable(head(processed_data(),20),options=list(scrollX=TRUE,pageLength=8,dom="t"),rownames=FALSE) })
    output$ui_selected_traits_display <- renderUI({ req(selected_traits_r()); p(style="color:#555;font-size:12px;",paste(selected_traits_r(),collapse=" · ")) })
    output$ui_goals_display   <- renderUI({ req(trait_goal_r()); g<-trait_goal_r(); p(style="color:#555;font-size:12px;",paste(names(g),ifelse(g=="h","↑","↓"),collapse=" | ")) })

    observeEvent(input$btn_fit, {
      req(processed_data(),selected_traits_r(),trait_goal_r())
      df<-processed_data(); trs<-selected_traits_r(); goals<-trait_goal_r()
      output$fit_status <- renderUI(div(style="color:#F4A300;font-weight:600;","⏳ Fitting models... please wait"))
      withProgress(message="Fitting mixed models...", value=0, {
        incProgress(.1,detail="Starting gamem_met...")
        gm <- tryCatch(gamem_met(df,ENV,GEN,REP,resp=all_of(trs),verbose=FALSE),error=function(e){ showNotification(paste("gamem_met error:",e$message),type="error",duration=8);NULL })
        incProgress(.5,detail="Starting waasb...")
        ideotype_str <- paste(ifelse(goals=="l","min","max"),collapse=",")
        wb <- tryCatch(waasb(df,ENV,GEN,REP,resp=all_of(trs),ideotype=ideotype_str,verbose=FALSE),error=function(e){ showNotification(paste("waasb error:",e$message),type="error",duration=8);NULL })
        incProgress(1,detail="Done!")
      })
      gamem_r(gm); waasb_r(wb)
      if(!is.null(gm)&&!is.null(wb)){ output$fit_status <- renderUI(div(style="color:#2D6A4F;font-weight:600;","✅ Models fitted successfully!")); showNotification("✅ gamem_met + waasb fitted!",type="message",duration=4) }
    })
    blup_mat_r <- reactive({ req(gamem_r(),selected_traits_r()); tryCatch(make_blup_matrix(gamem_r(),selected_traits_r()),error=function(e)NULL) })
    output$txt_gamem <- renderPrint({ req(gamem_r()); tryCatch(print(gamem_r()),error=function(e)cat("Error:",e$message)) })
    output$txt_waasb <- renderPrint({ req(waasb_r()); tryCatch(print(waasb_r()),error=function(e)cat("Error:",e$message)) })
    output$tbl_blup  <- renderDT({ req(blup_mat_r()); fmt_dt(as.data.frame(blup_mat_r())) })

    table2_r <- eventReactive(input$btn_table2, { req(gamem_r(),processed_data(),selected_traits_r()); tryCatch(build_table2(gamem_r(),processed_data(),selected_traits_r()),error=function(e)NULL) })
    output$tbl_varcomp <- renderDT({ req(table2_r()); fmt_dt(as.data.frame(table2_r())) })
    output$dl_table2   <- downloadHandler(paste0("Table2_VarComp_",Sys.Date(),".xlsx"), function(f){ req(table2_r()); write_xlsx(as.data.frame(table2_r()),f) })

    mtsi_r <- eventReactive(input$btn_mtsi, { req(waasb_r()); tryCatch(mtsi(waasb_r(),SI=input$sel_intensity),error=function(e){ showNotification(paste("MTSI error:",e$message),type="error");NULL }) })
    output$tbl_mtsi     <- renderDT({ req(mtsi_r()); fmt_dt(safe_df(mtsi_r()$MTSI)) })
    output$txt_mtsi_sel <- renderPrint({ req(mtsi_r()); cat("Selected genotypes:\n"); print(as.character(mtsi_r()$sel_gen)) })
    r_plt_mtsi_circ <- reactive({ req(mtsi_r()); make_circular_index_plot(mtsi_r()$MTSI,"Genotype","MTSI",as.character(mtsi_r()$sel_gen),"MTSI — Circular Index Plot","Multi-Trait Stability Index",lower_is_better=TRUE) })
    output$plt_mtsi_circ    <- renderPlot({ req(r_plt_mtsi_circ()); r_plt_mtsi_circ() })
    output$dl_plt_mtsi_circ <- downloadHandler(paste0("MTSI_circular_",Sys.Date(),".pdf"), function(f){ req(r_plt_mtsi_circ()); save_pdf(r_plt_mtsi_circ(),f,14,14) })
    r_plt_mtsi_contrib <- reactive({ req(mtsi_r()); make_contribution_plot(mtsi_r()$contri_fac,"MTSI — Factor Contribution") })
    output$plt_mtsi_contrib    <- renderPlot({ req(r_plt_mtsi_contrib()); r_plt_mtsi_contrib() })
    output$dl_plt_mtsi_contrib <- downloadHandler(paste0("MTSI_contribution_",Sys.Date(),".pdf"), function(f){ req(r_plt_mtsi_contrib()); save_pdf(r_plt_mtsi_contrib(),f,14,9) })
    output$plt_mtsi_def    <- renderPlot({ req(mtsi_r()); tryCatch({ p<-plot(mtsi_r(),type="index"); print(p) },error=function(e) print(ggplot()+annotate("text",x=.5,y=.5,label=paste("Error:",e$message),size=4)+theme_void())) })
    output$dl_plt_mtsi_def <- downloadHandler(paste0("MTSI_default_",Sys.Date(),".pdf"), function(f){ req(mtsi_r()); cairo_pdf(f,16,10); tryCatch({ p<-plot(mtsi_r(),type="index"); print(p) },error=function(e){plot.new();title(e$message)}); dev.off() })
    output$plt_mtsi_sw    <- renderPlot({
      req(mtsi_r())
      tryCatch({ p<-plot(mtsi_r(), type="contribution"); print(p) },
               error=function(e) print(ggplot()+annotate("text",x=.5,y=.5,label=paste("Error:",e$message),size=4)+theme_void()))
    })
    output$dl_plt_mtsi_sw <- downloadHandler(paste0("MTSI_SW_",Sys.Date(),".pdf"), function(f){
      req(mtsi_r())
      cairo_pdf(f, 16, 12)
      tryCatch({ p<-plot(mtsi_r(), type="contribution"); print(p) }, error=function(e){ plot.new(); title(e$message) })
      dev.off()
    })
    output$dl_mtsi_idx     <- downloadHandler(paste0("MTSI_index_",Sys.Date(),".xlsx"), function(f){ req(mtsi_r()); write_xlsx(safe_df(mtsi_r()$MTSI),f) })
    output$dl_mtsi_contrib <- downloadHandler(paste0("MTSI_contrib_",Sys.Date(),".xlsx"), function(f){ req(mtsi_r()); write_xlsx(safe_df(mtsi_r()$contri_fac),f) })

    mgidi_r <- eventReactive(input$btn_mgidi, { req(gamem_r(),trait_goal_r()); goals<-trait_goal_r(); tryCatch(mgidi(gamem_r(),ideotype=unname(goals),SI=input$sel_intensity,verbose=FALSE),error=function(e){ showNotification(paste("MGIDI error:",e$message),type="error");NULL }) })
    output$tbl_mgidi     <- renderDT({ req(mgidi_r()); fmt_dt(safe_df(mgidi_r()$MGIDI)) })
    output$txt_mgidi_sel <- renderPrint({ req(mgidi_r()); cat("Selected genotypes:\n"); print(as.character(mgidi_r()$sel_gen)) })
    r_plt_mgidi_circ <- reactive({ req(mgidi_r()); make_circular_index_plot(mgidi_r()$MGIDI,"Genotype","MGIDI",as.character(mgidi_r()$sel_gen),"MGIDI — Circular Index Plot","Genotype-Ideotype Distance Index",lower_is_better=TRUE) })
    output$plt_mgidi_circ    <- renderPlot({ req(r_plt_mgidi_circ()); r_plt_mgidi_circ() })
    output$dl_plt_mgidi_circ <- downloadHandler(paste0("MGIDI_circular_",Sys.Date(),".pdf"), function(f){ req(r_plt_mgidi_circ()); save_pdf(r_plt_mgidi_circ(),f,14,14) })
    r_plt_mgidi_contrib <- reactive({ req(mgidi_r()); make_contribution_plot(mgidi_r()$contri_fac,"MGIDI — Factor Contribution") })
    output$plt_mgidi_contrib    <- renderPlot({ req(r_plt_mgidi_contrib()); r_plt_mgidi_contrib() })
    output$dl_plt_mgidi_contrib <- downloadHandler(paste0("MGIDI_contribution_",Sys.Date(),".pdf"), function(f){ req(r_plt_mgidi_contrib()); save_pdf(r_plt_mgidi_contrib(),f,14,9) })
    output$plt_mgidi_def    <- renderPlot({ req(mgidi_r()); tryCatch({ p<-plot(mgidi_r(),type="index"); print(p) },error=function(e) print(ggplot()+annotate("text",x=.5,y=.5,label=paste("Error:",e$message),size=4)+theme_void())) })
    output$dl_plt_mgidi_def <- downloadHandler(paste0("MGIDI_default_",Sys.Date(),".pdf"), function(f){ req(mgidi_r()); cairo_pdf(f,16,10); tryCatch({ p<-plot(mgidi_r(),type="index"); print(p) },error=function(e){plot.new();title(e$message)}); dev.off() })
    output$plt_mgidi_sw    <- renderPlot({
      req(mgidi_r())
      tryCatch({ p<-plot(mgidi_r(), type="contribution"); print(p) },
               error=function(e) print(ggplot()+annotate("text",x=.5,y=.5,label=paste("Error:",e$message),size=4)+theme_void()))
    })
    output$dl_plt_mgidi_sw <- downloadHandler(paste0("MGIDI_SW_",Sys.Date(),".pdf"), function(f){
      req(mgidi_r())
      cairo_pdf(f, 16, 12)
      tryCatch({ p<-plot(mgidi_r(), type="contribution"); print(p) }, error=function(e){ plot.new(); title(e$message) })
      dev.off()
    })
    output$dl_mgidi_idx     <- downloadHandler(paste0("MGIDI_index_",Sys.Date(),".xlsx"), function(f){ req(mgidi_r()); write_xlsx(safe_df(mgidi_r()$MGIDI),f) })
    output$dl_mgidi_contrib <- downloadHandler(paste0("MGIDI_contrib_",Sys.Date(),".xlsx"), function(f){ req(mgidi_r()); write_xlsx(safe_df(mgidi_r()$contri_fac),f) })

    fai_r <- eventReactive(input$btn_fai, { req(gamem_r(),trait_goal_r()); goals<-trait_goal_r(); di_vec<-ifelse(goals=="l","min","max"); tryCatch(fai_blup(gamem_r(),DI=di_vec,SI=input$sel_intensity,verbose=FALSE),error=function(e){ showNotification(paste("FAI-BLUP error:",e$message),type="error");NULL }) })
    fai_tbl_r   <- reactive({ req(fai_r()); tryCatch(fai_r()$FAI%>%select(Genotype,ID1)%>%arrange(ID1),error=function(e)safe_df(fai_r()$FAI)) })
    fai_selected_r <- reactive({ req(fai_r()); tryCatch(as.character(fai_r()$sel_gen$ID1),error=function(e)character(0)) })
    output$tbl_fai     <- renderDT({ req(fai_tbl_r()); fmt_dt(as.data.frame(fai_tbl_r())) })
    output$txt_fai_sel <- renderPrint({ req(fai_selected_r()); cat("Selected (ID1):\n"); print(fai_selected_r()) })
    r_plt_fai_circ <- reactive({ req(fai_tbl_r(),fai_selected_r()); df<-as.data.frame(fai_tbl_r()); gc<-if("Genotype" %in% names(df))"Genotype" else names(df)[1]; vc<-if("ID1" %in% names(df))"ID1" else names(df)[which(sapply(df,is.numeric))[1]]; make_circular_index_plot(df,gc,vc,fai_selected_r(),"FAI-BLUP — Circular Index Plot","FAI-BLUP Score",lower_is_better=FALSE) })
    output$plt_fai_circ    <- renderPlot({ req(r_plt_fai_circ()); r_plt_fai_circ() })
    output$dl_plt_fai_circ <- downloadHandler(paste0("FAIBLUP_circular_",Sys.Date(),".pdf"), function(f){ req(r_plt_fai_circ()); save_pdf(r_plt_fai_circ(),f,14,14) })
    output$plt_fai_def     <- renderPlot({ req(fai_r()); tryCatch({ p<-plot(fai_r()); print(p) },error=function(e) print(ggplot()+annotate("text",x=.5,y=.5,label=paste("Error:",e$message),size=4)+theme_void())) })
    output$dl_plt_fai_def  <- downloadHandler(paste0("FAIBLUP_default_",Sys.Date(),".pdf"), function(f){ req(fai_r()); cairo_pdf(f,16,10); tryCatch({ p<-plot(fai_r()); print(p) },error=function(e){plot.new();title(e$message)}); dev.off() })
    output$dl_fai_idx      <- downloadHandler(paste0("FAIBLUP_index_",Sys.Date(),".xlsx"), function(f){ req(fai_tbl_r()); write_xlsx(as.data.frame(fai_tbl_r()),f) })

    sh_r <- eventReactive(input$btn_sh, { req(blup_mat_r(),processed_data(),selected_traits_r(),trait_goal_r()); tryCatch(make_sh_index(blup_mat_r(),processed_data(),selected_traits_r(),trait_goal_r(),input$sel_intensity),error=function(e){ showNotification(paste("Smith-Hazel error:",e$message),type="error");NULL }) })
    output$tbl_sh     <- renderDT({ req(sh_r()); fmt_dt(tryCatch(safe_df(sh_r()$index),error=function(e)data.frame(Error=e$message))) })
    output$txt_sh_sel <- renderPrint({ req(sh_r()); cat("Selected genotypes:\n"); print(tryCatch(as.character(sh_r()$sel_gen),error=function(e)"Error extracting")) })
    r_plt_sh_circ <- reactive({ req(sh_r()); df<-tryCatch(as.data.frame(sh_r()$index),error=function(e)NULL); req(df); gc<-if("GEN" %in% names(df))"GEN" else names(df)[1]; vc<-grep("^V1$|index|Score|value",names(df),value=TRUE,ignore.case=TRUE)[1]%||%names(df)[which(sapply(df,is.numeric))[1]]; sel<-tryCatch(as.character(sh_r()$sel_gen),error=function(e)character(0)); make_circular_index_plot(df,gc,vc,sel,"Smith-Hazel — Circular Index Plot","Individual Genetic Worth",lower_is_better=FALSE) })
    output$plt_sh_circ    <- renderPlot({ req(r_plt_sh_circ()); r_plt_sh_circ() })
    output$dl_plt_sh_circ <- downloadHandler(paste0("SmithHazel_circular_",Sys.Date(),".pdf"), function(f){ req(r_plt_sh_circ()); save_pdf(r_plt_sh_circ(),f,14,14) })
    output$plt_sh_def     <- renderPlot({ req(sh_r()); tryCatch({ p<-plot(sh_r()); print(p) },error=function(e) print(ggplot()+annotate("text",x=.5,y=.5,label=paste("Error:",e$message),size=4)+theme_void())) })
    output$dl_plt_sh_def  <- downloadHandler(paste0("SmithHazel_default_",Sys.Date(),".pdf"), function(f){ req(sh_r()); cairo_pdf(f,16,10); tryCatch({ p<-plot(sh_r()); print(p) },error=function(e){plot.new();title(e$message)}); dev.off() })
    output$dl_sh_idx      <- downloadHandler(paste0("SmithHazel_index_",Sys.Date(),".xlsx"), function(f){ req(sh_r()); write_xlsx(safe_df(sh_r()$index),f) })

    direct_r <- eventReactive(input$btn_direct, { req(blup_mat_r(),trait_goal_r(),input$yield_trait); yt<-input$yield_trait; yt_use<-if(yt %in% names(blup_mat_r())) yt else selected_traits_r()[length(selected_traits_r())]; n_sel<-get_selected_n(nrow(blup_mat_r()),input$sel_intensity); goals<-trait_goal_r(); decreasing<-if(yt_use %in% names(goals)) goals[[yt_use]]=="l" else FALSE
      if(decreasing) blup_mat_r()%>%arrange(.data[[yt_use]])%>%slice_head(n=n_sel) else blup_mat_r()%>%arrange(desc(.data[[yt_use]]))%>%slice_head(n=n_sel) })
    output$tbl_direct   <- renderDT({ req(direct_r()); fmt_dt(as.data.frame(direct_r())) })
    r_plt_direct <- reactive({ req(blup_mat_r(),direct_r(),input$yield_trait); df<-as.data.frame(blup_mat_r()); yt<-if(input$yield_trait %in% names(df)) input$yield_trait else names(df)[2]; df$Selected<-df$GEN %in% direct_r()$GEN
      ggplot(df,aes(x=reorder(GEN,-.data[[yt]]),y=.data[[yt]],fill=Selected))+geom_col(color="white",linewidth=.3)+scale_fill_manual(values=c("FALSE"="grey75","TRUE"="#2D6A4F"))+labs(title=paste("Direct Selection on",yt),x="Genotype",y=paste(yt,"(BLUP Predicted)"),fill="Selected")+theme_met()+theme(axis.text.x=element_text(angle=45,hjust=1,size=8)) })
    output$plt_direct    <- renderPlot({ req(r_plt_direct()); r_plt_direct() })
    output$dl_plt_direct <- downloadHandler(paste0("DirectSelection_",Sys.Date(),".pdf"), function(f){ req(r_plt_direct()); save_pdf(r_plt_direct(),f,16,10) })
    output$dl_direct     <- downloadHandler(paste0("DirectSelection_",Sys.Date(),".xlsx"), function(f){ req(direct_r()); write_xlsx(as.data.frame(direct_r()),f) })

    selected_sets_r <- reactive({ out<-list()
      if(!is.null(direct_r())) out$DirectSelection <- as.character(direct_r()$GEN)
      if(!is.null(mtsi_r()))   out$MTSI            <- as.character(mtsi_r()$sel_gen)
      if(!is.null(mgidi_r()))  out$MGIDI           <- as.character(mgidi_r()$sel_gen)
      if(!is.null(sh_r()))     out$SmithHazel      <- tryCatch(as.character(sh_r()$sel_gen),error=function(e)character(0))
      if(!is.null(fai_r()))    out$FAIBLUP         <- fai_selected_r()
      out })

    table3_r <- eventReactive(input$btn_table3, { req(blup_mat_r(),selected_sets_r(),trait_goal_r()); ss<-selected_sets_r(); req(length(ss)>0); bind_rows(lapply(names(ss),function(nm) selection_differential(blup_mat_r(),ss[[nm]],trait_goal_r(),nm))) })
    output$tbl_table3    <- renderDT({ req(table3_r()); fmt_dt(as.data.frame(table3_r())) })
    r_plt_sd_heat <- reactive({ req(table3_r()); df<-table3_r()
      ggplot(df,aes(x=Method,y=Trait,fill=SDpercent))+geom_tile(color="white",linewidth=.5)+geom_text(aes(label=paste0(round(SDpercent,1),"%"),color=ifelse(DesiredGain=="Yes","white","#C62828")),size=3,fontface="bold")+scale_fill_gradientn(colors=c("#C62828","#FFECB3","#2D6A4F"),name="SD%")+scale_color_identity()+labs(title="Selection Differential (%) by Method × Trait",x="Selection Method",y="Trait")+theme_met()+theme(axis.text.x=element_text(angle=30,hjust=1)) })
    output$plt_sd_heat    <- renderPlot({ req(r_plt_sd_heat()); r_plt_sd_heat() })
    output$dl_plt_sd_heat <- downloadHandler(paste0("SelDiff_heatmap_",Sys.Date(),".pdf"), function(f){ req(r_plt_sd_heat()); save_pdf(r_plt_sd_heat(),f,16,10) })
    output$dl_table3      <- downloadHandler(paste0("Table3_SelDiff_",Sys.Date(),".xlsx"), function(f){ req(table3_r()); write_xlsx(as.data.frame(table3_r()),f) })

    coincidence_r <- eventReactive(input$btn_coincidence, { req(blup_mat_r(),selected_sets_r())
      list(ci=tryCatch(selection_overlap_table(selected_sets_r(),nrow(blup_mat_r())),error=function(e)NULL),
           memb=tryCatch(selection_membership_table(selected_sets_r(),blup_mat_r()$GEN),error=function(e)NULL)) })
    output$tbl_coincidence <- renderDT({ req(coincidence_r()); fmt_dt(as.data.frame(coincidence_r()$ci)) })
    output$tbl_membership  <- renderDT({ req(coincidence_r()); fmt_dt(as.data.frame(coincidence_r()$memb)) })
    r_plt_ci_heat <- reactive({ req(coincidence_r(),coincidence_r()$ci); df<-coincidence_r()$ci
      ggplot(df,aes(x=Method1,y=Method2,fill=CoincidenceIndex))+geom_tile(color="white",linewidth=.6)+geom_text(aes(label=round(CoincidenceIndex,2)),size=4,fontface="bold",color="white")+scale_fill_gradientn(colors=c("#0D3B2E","#52B788","#F4A300"),name="CI")+labs(title="Coincidence Index Between Selection Methods",x=NULL,y=NULL)+theme_met()+theme(axis.text.x=element_text(angle=30,hjust=1)) })
    output$plt_ci_heat    <- renderPlot({ req(r_plt_ci_heat()); r_plt_ci_heat() })
    output$dl_plt_ci_heat <- downloadHandler(paste0("CoincidenceIndex_",Sys.Date(),".pdf"), function(f){ req(r_plt_ci_heat()); save_pdf(r_plt_ci_heat(),f,14,10) })
    output$dl_coincidence <- downloadHandler(paste0("CoincidenceIndex_",Sys.Date(),".xlsx"), function(f){ req(coincidence_r()); write_xlsx(as.data.frame(coincidence_r()$ci),f) })
    output$dl_membership  <- downloadHandler(paste0("Membership_",Sys.Date(),".xlsx"), function(f){ req(coincidence_r()); write_xlsx(as.data.frame(coincidence_r()$memb),f) })

    venn_r <- eventReactive(input$btn_venn, { req(selected_sets_r()); ss<-selected_sets_r(); four_methods<-c("MTSI","MGIDI","FAIBLUP","SmithHazel"); avail<-intersect(four_methods,names(ss)); validate(need(length(avail)==4,"Need all 4 methods (MTSI, MGIDI, FAI-BLUP, Smith-Hazel) first.")); ss[avail] })
    r_plt_venn <- reactive({ req(venn_r()); tryCatch(make_four_set_venn(venn_r(),"Venn Diagram: MTSI × MGIDI × FAI-BLUP × Smith-Hazel"),error=function(e) ggplot()+annotate("text",x=.5,y=.5,label=paste("Error:",e$message),size=4)+theme_void()) })
    output$plt_venn    <- renderPlot({ req(r_plt_venn()); r_plt_venn() })
    output$dl_plt_venn <- downloadHandler(paste0("Venn_4way_",Sys.Date(),".pdf"), function(f){ req(r_plt_venn()); save_pdf(r_plt_venn(),f,18,16) })
    output$tbl_venn    <- renderDT({ req(venn_r()); fmt_dt(tryCatch(as.data.frame(make_venn_genotype_table(venn_r())),error=function(e)data.frame(Error=e$message))) })
    output$dl_venn_tbl <- downloadHandler(paste0("Venn_regions_",Sys.Date(),".xlsx"), function(f){ req(venn_r()); write_xlsx(list(Regions=as.data.frame(make_venn_genotype_table(venn_r())),Counts=as.data.frame(make_venn_region_table(venn_r()))),f) })

    biplot_r <- eventReactive(input$btn_biplot, { req(processed_data(),selected_traits_r(),input$yield_trait); df<-processed_data(); trs<-selected_traits_r(); yt<-if(input$yield_trait %in% names(df)) input$yield_trait else trs[length(trs)]; gm<-df%>%group_by(GEN)%>%summarise(across(all_of(trs),~mean(.x,na.rm=TRUE)),.groups="drop"); other_trs<-setdiff(trs,yt); goals<-trait_goal_r(); gyt_ideo<-unname(goals[other_trs])
      list(gt_fit=tryCatch(gtb(gm,gen=GEN,resp=all_of(trs)),error=function(e)NULL),gyt_fit=tryCatch(gytb(gm,gen=GEN,yield=!!sym(yt),traits=all_of(other_trs),ideotype=gyt_ideo),error=function(e)NULL),gen_means=gm,yt=yt) })
    highlight_gen_r <- reactive({ req(biplot_r(),selected_sets_r(),input$n_highlight); memb<-tryCatch(selection_membership_table(selected_sets_r(),biplot_r()$gen_means$GEN),error=function(e)NULL); if(is.null(memb)) return(head(biplot_r()$gen_means$GEN,input$n_highlight)); yt<-biplot_r()$yt; gm<-biplot_r()$gen_means; top<-memb%>%mutate(GEN=as.character(GEN))%>%left_join(gm%>%mutate(GEN=as.character(GEN))%>%select(GEN,!!sym(yt)),by="GEN")%>%mutate(MethodsSelected=replace_na(MethodsSelected,0))%>%arrange(desc(MethodsSelected),desc(.data[[yt]]))%>%slice_head(n=input$n_highlight)%>%pull(GEN); top })
    r_plt_gt       <- reactive({ req(biplot_r(),biplot_r()$gt_fit);  make_custom_biplot(biplot_r()$gt_fit$mod, highlight_gen_r(), "GT Biplot") })
    r_plt_gyt      <- reactive({ req(biplot_r(),biplot_r()$gyt_fit); make_custom_biplot(biplot_r()$gyt_fit$mod,highlight_gen_r(), "GYT Biplot") })
    r_plt_combined <- reactive({ req(r_plt_gt(),r_plt_gyt()); r_plt_gt()+r_plt_gyt()+plot_annotation(title="GT and GYT Biplots",theme=theme(plot.title=element_text(face="bold",color="#0D3B2E",size=16))) })
    output$plt_gt       <- renderPlot({ req(r_plt_gt());       r_plt_gt()       })
    output$plt_gyt      <- renderPlot({ req(r_plt_gyt());      r_plt_gyt()      })
    output$plt_combined <- renderPlot({ req(r_plt_combined()); r_plt_combined() })
    output$dl_plt_gt       <- downloadHandler(paste0("GT_biplot_",Sys.Date(),".pdf"),       function(f){ req(r_plt_gt());       save_pdf(r_plt_gt(),f,14,12) })
    output$dl_plt_gyt      <- downloadHandler(paste0("GYT_biplot_",Sys.Date(),".pdf"),      function(f){ req(r_plt_gyt());      save_pdf(r_plt_gyt(),f,14,12) })
    output$dl_plt_combined <- downloadHandler(paste0("GT_GYT_combined_",Sys.Date(),".pdf"), function(f){ req(r_plt_combined()); save_pdf(r_plt_combined(),f,20,12) })

    output$ui_radar_method    <- renderUI({ req(selected_sets_r()); selectInput(session$ns("radar_method"),"Source method:",choices=names(selected_sets_r()),selected=names(selected_sets_r())[1]) })
    output$ui_radar_genotypes <- renderUI({ req(selected_sets_r(),input$radar_method); genos<-tryCatch(selected_sets_r()[[input$radar_method]],error=function(e)character(0)); checkboxGroupInput(session$ns("radar_genotypes"),"Select genotypes (2-5):",choices=head(genos,10),selected=head(genos,3)) })
    radar_contrib_r <- reactive({ req(input$radar_method,mgidi_r(),mtsi_r()); if(input$radar_method %in% c("MGIDI","DirectSelection")){ req(mgidi_r()); mgidi_r()$contri_fac } else { req(mtsi_r()); mtsi_r()$contri_fac } })
    output$plt_radar <- renderPlot({ req(input$btn_radar>0,isolate(radar_contrib_r()),isolate(input$radar_genotypes)); genos<-isolate(input$radar_genotypes); contrib<-isolate(radar_contrib_r())
      validate(need(length(genos)>=2&&length(genos)<=5,"Please select 2–5 genotypes."))
      tryCatch({ df<-as.data.frame(contrib); if("GEN" %in% colnames(df)) df<-df%>%rename(Genotype=GEN) else df<-df%>%tibble::rownames_to_column("Genotype"); df<-df%>%filter(Genotype %in% genos); df_num<-df%>%select(where(is.numeric)); rownames(df_num)<-df$Genotype; max_v<-apply(df_num,2,max); min_v<-apply(df_num,2,min); radar_df<-rbind(max_v,min_v,df_num); pal<-c("#2D6A4F","#E65100","#1565C0","#AD1457","#4527A0")[seq_len(nrow(df_num))]; par(mar=c(1,1,3,1),bg="white"); fmsb::radarchart(radar_df,axistype=1,pcol=pal,plwd=3,plty=1,pfcol=scales::alpha(pal,.18),cglcol="grey80",cglty=1,axislabcol="#1B4332",vlcex=1.2); title(main=paste("Strength & Weakness —",input$radar_method),col.main="#0D3B2E",font.main=2,cex.main=1.4); legend("bottom",legend=rownames(df_num),col=pal,lty=1,lwd=3,bty="n",cex=1.1,horiz=TRUE) },error=function(e){ plot.new(); text(.5,.5,paste("Radar Error:",e$message),cex=1.2,col="#C62828") }) })
    output$dl_plt_radar <- downloadHandler(paste0("Radar_chart_",Sys.Date(),".pdf"), function(f){ req(radar_contrib_r(),input$radar_genotypes); genos<-input$radar_genotypes; contrib<-radar_contrib_r(); cairo_pdf(f,14,12); tryCatch({ df<-as.data.frame(contrib); if("GEN" %in% colnames(df)) df<-df%>%rename(Genotype=GEN) else df<-df%>%tibble::rownames_to_column("Genotype"); df<-df%>%filter(Genotype %in% genos); df_num<-df%>%select(where(is.numeric)); rownames(df_num)<-df$Genotype; max_v<-apply(df_num,2,max); min_v<-apply(df_num,2,min); radar_df<-rbind(max_v,min_v,df_num); pal<-c("#2D6A4F","#E65100","#1565C0","#AD1457","#4527A0")[seq_len(nrow(df_num))]; par(mar=c(1,1,4,1),bg="white"); fmsb::radarchart(radar_df,axistype=1,pcol=pal,plwd=3,plty=1,pfcol=scales::alpha(pal,.18),cglcol="grey80",cglty=1,axislabcol="#1B4332",vlcex=1.3); title(main=paste("Strength & Weakness —",input$radar_method),col.main="#0D3B2E",font.main=2,cex.main=1.5); legend("bottom",legend=rownames(df_num),col=pal,lty=1,lwd=3,bty="n",cex=1.1,horiz=TRUE) },error=function(e){ plot.new(); text(.5,.5,paste("Error:",e$message)) }); dev.off() })

    mk_safe <- function(x) tryCatch(as.data.frame(x),error=function(e)data.frame(Error=e$message))
    output$dl_all_xlsx <- downloadHandler(paste0("MultiTrait_Analysis_",Sys.Date(),".xlsx"), function(f){ sheets<-list(); if(!is.null(blup_mat_r())) sheets$BLUP_Matrix<-mk_safe(blup_mat_r()); if(!is.null(table2_r())) sheets$Table2_VarComp<-mk_safe(table2_r()); if(!is.null(table3_r())) sheets$Table3_SelDiff<-mk_safe(table3_r()); if(!is.null(mtsi_r())) sheets$MTSI_Index<-mk_safe(mtsi_r()$MTSI); if(!is.null(mgidi_r())) sheets$MGIDI_Index<-mk_safe(mgidi_r()$MGIDI); if(!is.null(fai_r())) sheets$FAIBLUP_Index<-mk_safe(fai_r()$FAI); if(!is.null(sh_r())) sheets$SmithHazel_Index<-mk_safe(sh_r()$index); if(!is.null(coincidence_r()$ci)) sheets$CoincidenceIndex<-mk_safe(coincidence_r()$ci); if(!is.null(coincidence_r()$memb)) sheets$Membership_Table<-mk_safe(coincidence_r()$memb); if(length(sheets)==0) sheets$Info<-data.frame(Message="No analyses completed yet."); write_xlsx(sheets,f) })
    mk_dl <- function(btn_id,get_df,fn_base){ output[[btn_id]] <- downloadHandler(paste0(fn_base,"_",Sys.Date(),".xlsx"),function(f){ df<-get_df(); req(!is.null(df)); write_xlsx(mk_safe(df),f) }) }
    mk_dl("dl_blup_mat",  function()blup_mat_r(),       "BLUP_Matrix")
    mk_dl("dl_table2_ex", function()table2_r(),         "Table2_VarComp")
    mk_dl("dl_table3_ex", function()table3_r(),         "Table3_SelDiff")
    mk_dl("dl_mtsi_ex",   function()mtsi_r()$MTSI,      "MTSI_Index")
    mk_dl("dl_mgidi_ex",  function()mgidi_r()$MGIDI,    "MGIDI_Index")
    mk_dl("dl_fai_ex",    function()fai_r()$FAI,        "FAIBLUP_Index")
    mk_dl("dl_sh_ex",     function()sh_r()$index,       "SmithHazel_Index")
    mk_dl("dl_coincid_ex",function()coincidence_r()$ci, "CoincidenceIndex")
    mk_dl("dl_member_ex", function()coincidence_r()$memb,"Membership_Table")
    mk_dl("dl_venn_ex",   function()make_venn_genotype_table(venn_r()),"Venn_Regions")
  })
}

# ════════════════════════════════════════════════════════════════
#  COMBINED UI
# ════════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "green", title = "Plant Breeding Analytics Suite",

  dashboardHeader(
    title = div(
      div(style="font-family:'Playfair Display',serif;font-size:14px;color:#95D5B2;line-height:1.2;","🌾 Plant Breeding Suite"),
      div(style="font-size:9.5px;color:#74C69D;letter-spacing:.8px;font-weight:600;","D² · MET · MULTI-TRAIT")
    ), titleWidth = 230
  ),

  dashboardSidebar(width = 230,
    sidebarMenu(id = "main_menu",
      # ── D² Analysis ──────────────────────────────────────
      tags$li(class="suite-divider", "🌿 D² Genetic Diversity"),
      menuItem("📁  Data Upload",    tabName="d2_upload",  icon=icon("upload")),
      menuItem("📊  MANOVA",         tabName="d2_manova",  icon=icon("chart-bar")),
      menuItem("📐  D² Distances",   tabName="d2_d2",      icon=icon("ruler-combined")),
      menuItem("🌐  Tocher Cluster", tabName="d2_tocher",  icon=icon("project-diagram")),
      menuItem("📈  PCA",            tabName="d2_pca",     icon=icon("dot-circle")),
      menuItem("🔗  Correlation",    tabName="d2_corr",    icon=icon("link")),
      menuItem("📦  D² Export",      tabName="d2_export",  icon=icon("download")),
      # ── MET Analysis ─────────────────────────────────────
      tags$li(class="suite-divider", "🌾 MET Analysis"),
      menuItem("🏠  MET Home",        tabName="met_home",       icon=icon("home")),
      menuItem("📂  MET Data",        tabName="met_data",       icon=icon("database")),
      menuItem("📊  Descriptive",     tabName="met_desc",       icon=icon("chart-bar")),
      menuItem("📈  Mean Performance",tabName="met_mean",       icon=icon("line-chart")),
      menuItem("🔬  ANOVA",           tabName="met_anova",      icon=icon("flask")),
      menuItem("📉  Stability",       tabName="met_stab_anova", icon=icon("balance-scale"),
        menuSubItem("ANOVA-based",      tabName="met_stab_anova"),
        menuSubItem("Regression",       tabName="met_stab_reg"),
        menuSubItem("Non-parametric",   tabName="met_stab_np"),
        menuSubItem("Factor Analysis",  tabName="met_stab_fa"),
        menuSubItem("Wrap Parameters",  tabName="met_stab_wrap")
      ),
      menuItem("🎯  AMMI",            tabName="met_ammi",  icon=icon("bullseye")),
      menuItem("🌐  GGE",             tabName="met_gge",   icon=icon("globe")),
      # ── Multi-Trait ───────────────────────────────────────
      tags$li(class="suite-divider", "🧬 Multi-Trait Selection"),
      menuItem("🏠  MT Home",         tabName="mt_home",       icon=icon("home")),
      menuItem("📂  Data & Settings", tabName="mt_data",       icon=icon("cog")),
      menuItem("⚙️  Fit Models",      tabName="mt_fit",        icon=icon("cogs")),
      menuItem("📊  Variance Comp.",  tabName="mt_varcomp",    icon=icon("table")),
      menuItem("🎯  MTSI",            tabName="mt_mtsi",       icon=icon("bullseye")),
      menuItem("🧬  MGIDI",           tabName="mt_mgidi",      icon=icon("dna")),
      menuItem("🔢  FAI-BLUP",        tabName="mt_fai",        icon=icon("calculator")),
      menuItem("⚖️  Smith-Hazel",     tabName="mt_sh",         icon=icon("balance-scale")),
      menuItem("📈  Direct Select.",  tabName="mt_direct",     icon=icon("arrow-up")),
      menuItem("🔗  Selection Summary", tabName="mt_table3",  icon=icon("list-check"),
        menuSubItem("Selection Diff. (Table 3)", tabName="mt_table3"),
        menuSubItem("Coincidence Index",         tabName="mt_coincidence"),
        menuSubItem("Membership & Venn",         tabName="mt_venn")
      ),
      menuItem("📉  GT/GYT Biplots",  tabName="mt_biplots",    icon=icon("chart-line")),
      menuItem("🌟  Radar Chart",     tabName="mt_radar",      icon=icon("star")),
      menuItem("💾  MT Export",       tabName="mt_export",     icon=icon("download")),
      # ── About ─────────────────────────────────────────────
      tags$li(class="suite-divider", "ℹ️ Information"),
      menuItem("ℹ️  About",           tabName="about",     icon=icon("info-circle"))
    ),
    div(style="position:absolute;bottom:0;width:100%;padding:8px 12px;background:rgba(0,0,0,.3);font-size:9.5px;color:#52B788;border-top:1px solid rgba(82,183,136,.2);line-height:1.6;",
        "Dr. V.K. Meena | AU Jodhpur")
  ),

  dashboardBody(
    APP_CSS,
    do.call(tabItems, c(
      # ── D² tabs ────────────────────────────────────────
      as.list(d2UI("d2")),
      # ── MET tabs ───────────────────────────────────────
      as.list(metUI("met")),
      # ── MT tabs ────────────────────────────────────────
      as.list(mtUI("mt")),
      # ── About tab ──────────────────────────────────────
      list(tabItem("about",
        div(class="sec-bar","ℹ️ About — Plant Breeding Analytics Suite"),
        fluidRow(
          column(6,
            div(class="dev-card",
              div(class="dev-name","Dr. Vijay Kamal Meena"),
              div(class="dev-role","Assistant Professor (GPB) | Agriculture University Jodhpur"),
              hr(style="border:none;border-top:1px solid rgba(255,255,255,.15);margin:12px 0;"),
              div(class="cr",tags$i(class="fa fa-graduation-cap"),"M.Sc. & Ph.D. — ICAR-IARI, New Delhi"),
              div(class="cr",tags$i(class="fa fa-award"),         "ICAR-ARS 2021"),
              div(class="cr",tags$i(class="fa fa-university"),    "Agri. Research Sub-Station Sumerpur (Pali)"),
              div(class="cr",tags$i(class="fa fa-building"),      "Agriculture University Jodhpur"),
              div(class="cr",tags$i(class="fa fa-envelope"),      "vjkamal93@gmail.com"),
              div(class="cr",tags$i(class="fa fa-envelope"),      "vijaykamal@aujodhpur.ac.in"),
              div(class="cr",tags$i(class="fa fa-phone"),         "+91 9449509856"),
              div(class="tags",
                  span(class="tag","Plant Breeding"),   span(class="tag","Quantitative Genetics"),
                  span(class="tag","GxE Interaction"),  span(class="tag","MET Analysis"),
                  span(class="tag","AMMI"), span(class="tag","GGE Biplot"), span(class="tag","MTSI"),
                  span(class="tag","MGIDI"), span(class="tag","FAI-BLUP"), span(class="tag","D² Analysis"),
                  span(class="tag","R Programming"), span(class="tag","Bioinformatics"))
            )
          ),
          column(6,
            box(title="About This Suite", width=12, status="success", solidHeader=TRUE,
              p("A unified professional Shiny dashboard combining three analytical modules for plant breeding research:"),
              br(),
              h4(style="color:#1B4332;","Module 1 — D² Genetic Diversity Analyser"),
              tags$ul(tags$li("Mahalanobis D² distance matrix"),tags$li("MANOVA & univariate ANOVA"),tags$li("Tocher clustering, dendrogram, network plot"),tags$li("PCA biplot with cluster overlay"),tags$li("Pearson correlation heatmap")),
              br(),
              h4(style="color:#1B4332;","Module 2 — MET Analysis (AMMI/GGE)"),
              tags$ul(tags$li("Descriptive statistics, GxE heatmap"),tags$li("Individual + pooled ANOVA, Bartlett test"),tags$li("ANOVA, regression & non-parametric stability"),tags$li("AMMI: 3 biplot types, ASV, WAAS index"),tags$li("GGE: 7 biplot types (3 SVP options)")),
              br(),
              h4(style="color:#1B4332;","Module 3 — Multi-Trait Selection Suite"),
              tags$ul(tags$li("MTSI, MGIDI, FAI-BLUP, Smith-Hazel"),tags$li("Direct selection on yield trait"),tags$li("Selection differentials (Table 3)"),tags$li("Coincidence index & 4-way Venn diagram"),tags$li("GT/GYT biplots & Radar chart")),
              br(),
              p(strong("Key packages:"),"metan, biotools, FactoMineR, ggplot2, plotly, corrplot, fmsb"),
              div(class="app-footer","Version 1.0 (Combined Suite) | 2025 | Agriculture University Jodhpur")
            )
          )
        )
      ))   # closes tabItem("about",...) + list()
    ))     # closes c(...) + do.call(tabItems, ...)
  )
)

# ════════════════════════════════════════════════════════════════
#  COMBINED SERVER
# ════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  d2Server("d2")
  metServer("met")
  mtServer("mt")
}

# ════════════════════════════════════════════════════════════════
#  LAUNCH
# ════════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)

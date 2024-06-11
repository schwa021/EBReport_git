prop_detail_plot <- function(p){
  
  library(cowplot)
  
  # Spasticity -----
  gspas <- plot_grid(
    p[[1]], p[[2]],
    ncol = 2, rel_widths = c(1,1), rel_heights = c(1,1),
    align = "v")
  
  titspas <- ggdraw() + 
    draw_label(
      "Surgeries to Treat Spasticity",
      fontface = 'bold',
      size = 16,
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  pspas <- 
    plot_grid(
      titspas, gspas,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
  
  # Contracture -----
  gcontr <- plot_grid(
    p[[3]], p[[4]], p[[5]], p[[6]],
    ncol = 2, rel_widths = c(1,1), rel_heights = c(1,1), align = "v")
  
  titcontr <- ggdraw() + 
    draw_label(
      "Surgeries to Treat Contracture",
      fontface = 'bold',
      size = 16,
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  
  pcontr <- 
    plot_grid(
      titcontr, gcontr,
      ncol = 1,
      rel_heights = c(0.05, 1)
    )
  
  # Torsion -----
  gtor <- plot_grid(
    p[[7]], p[[8]],
    ncol = 2, rel_widths = c(1,1), rel_heights = c(1,1),
    align = "v")
  
  tittor <- ggdraw() + 
    draw_label(
      "Surgeries to Treat Femoral and Tibial Torsion",
      fontface = 'bold',
      size = 16,
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  
  ptor <- 
    plot_grid(
      tittor, gtor,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
  
  # Crouch -----
  gcrouch <- plot_grid(
    p[[9]], p[[10]],
    ncol = 2, rel_widths = c(1,1), rel_heights = c(1,1),
    align = "v")
  
  titcrouch <- ggdraw() + 
    draw_label(
      "Surgeries to Treat Crouch (Knee Contracture & Patella Alta)",
      fontface = 'bold',
      size = 16,
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  
  pcrouch <- 
    plot_grid(
      titcrouch, gcrouch,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
  
  # Foot -----
  gfoot <- plot_grid(
    p[[11]], p[[12]],
    ncol = 2, rel_widths = c(1,1), rel_heights = c(1,1),
    align = "v")
  
  titfoot <- ggdraw() + 
    draw_label(
      "Surgeries to Treat Foot Deformity",
      fontface = 'bold',
      size = 16,
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  
  pfoot <- 
    plot_grid(
      titfoot, gfoot,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
  
  
  # Combine all plots
  pprofile <- plot_grid(
    pspas, pcontr, ptor, pcrouch, pfoot, 
    ncol = 1, 
    rel_heights = c(1, 2, 1, 1, 1),
    align = "v",
    scale = .95)
  
  return(list("pspas"=pspas, "ptor"=ptor, "pcontr"=pcontr, "pcrouch"=pcrouch,
              "pfoot"=pfoot, "pprofile"=pprofile))
  
  
}
# GeomPTDIcon draw panel works as expected

    Code
      GeomPTDIcon$draw_panel(data = data.frame(type = c("variation", "assurance"),
      icon = c(system.file("icons", "variation", "improvement_low.png", package = "NHSRplotthedots"),
      system.file("icons", "assurance", "pass.png", package = "NHSRplotthedots"))),
      panel_params = list(), coord = list(transform = function(x, ...) x),
      icons_size = 8L, icons_position = "top right")
    Output
      (rastergrob[GRID.rastergrob.1], rastergrob[GRID.rastergrob.2]) 


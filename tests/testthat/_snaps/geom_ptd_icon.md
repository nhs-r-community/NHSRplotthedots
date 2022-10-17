# GeomPTDIcon draw panel works as expected

    Code
      GeomPTDIcon$draw_panel(data = data.frame(type = c("variation", "assurance"),
      icon = c(system.file("icons", "variation", "improvement_low.svg", package = "NHSRplotthedots"),
      system.file("icons", "assurance", "pass.svg", package = "NHSRplotthedots"))),
      panel_params = list(), coord = list(transform = function(x, ...) x),
      icons_size = 8L, icons_position = "top right")
    Output
      (rastergrob[GRID.rastergrob.1], rastergrob[GRID.rastergrob.2]) 


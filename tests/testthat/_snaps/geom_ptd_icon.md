# GeomPTDIcon draw panel works as expected

    Code
      GeomPTDIcon$draw_panel(data = data.frame(type = c("variation", "assurance"),
      colour = c("red", "green"), text = c("a", "b")), panel_params = list(), coord = list(
        transform = function(x, ...) x))
    Output
      (circle[GRID.circle.1], circle[GRID.circle.2], text[GRID.text.3]) 


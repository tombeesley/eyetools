
# customise your hex sticker
hexSticker::sticker(
  filename = "inst/figures/hex.png",
  # subplot aesthetics
  subplot = "inst/figures/logo.png",
  s_width = 0.95, s_height = .95,
  s_x=1, s_y=1,
  # package name aesthetics
  package = "",
  p_size = 24,
  p_color = "red",
  # hexagon aesthetics
  h_size = 1,
  h_fill = "white",
  h_color = "black",
  # url aesthetics
  url = "tombeesley.github.io/eyetools",
  u_size = 5,
  u_color = "black"
) |> plot() # preview with plot()


library(fresh)

create_theme(
  theme = "default",
  bs_vars_navbar(
    default_bg = "#3f2d54",
    default_color = "#FFFFFF",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#FFFFFF"
  ),
  bs_vars_color(
    gray_base = "#3f2d54",
    brand_primary = "#FFFFFF",
    brand_success = "#FFFFFF",
    brand_info = "#FFFFFF",
    brand_warning = "#FFFFFF",
    brand_danger = "#FFFFFF"
  ),
  bs_vars_state(
    success_text = "#FFF",
    success_bg = "#FFFFFF",
    success_border = "#c9d175",
    info_text = "#FFFFFF",
    info_bg = "#FFFFFF",
    info_border = "#FFFFFF",
    danger_text = "#FFF",
    danger_bg = "#FFFFFF",
    danger_border = "#FFFFFF"
  ),
  bs_vars_wells(
    bg = "#FFF",
    border = "#d175b8"
  ),
  output_file = "www/cbx.css"
)

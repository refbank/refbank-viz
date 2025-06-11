theme_set(theme_bw(base_size = 14) +
            theme(strip.background = element_blank(),
                  panel.grid = element_blank()))

GRP_SIZE_COL_SCALE <- scale_colour_manual(
  values = c("1" = "#dce319", "2" = "#73d055", "3" = "#29af7f", 
             "4" = "#238a8d", "5" = "#33638d", "6" = "#453781", 
             "7" = "#440154")
)
  
STRUCT_COL_SCALE <- scale_colour_manual(
  values = c("thin" = "#fb8072", "medium" = "#80b1d3", 
             "med_thick" = "#b3de69", "thick" = "#fdb462", 
             "network-swap" = "#bebada", "naive-swap" = "#fccde5")
)

OPT_SIZE_COL_SCALE <- scale_colour_manual(
  values = c("4" = "#febb81", "10" = "#d3436e", "12" = "#982d80", 
             "16" = "#221150")
)
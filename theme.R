theme_set(theme_bw(base_size = 14) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank()
  ))

GRP_SIZE_COL_SCALE <- scale_colour_manual( # clearly viridis inspired, but I can't id actual palette
  values = c(
    "1" = "#dce319", "2" = "#73d055", "3" = "#29af7f",
    "4" = "#238a8d", "5" = "#33638d", "6" = "#453781",
    "7" = "#440154"
  )
)

STRUCT_COL_SCALE <- scale_colour_manual(
  values = c(
    "0.5" = "#fbb4ae", "1" = "#fed9a6",
    "1.5" = "#e5d8bd", "2" = "#ccebc5",
    "2.5" = "#b3cde3", "3" = "#decbe4",
    "3.5" = "#fddaec"
  )
)

OPT_SIZE_COL_SCALE <- scale_colour_manual( # viridis magma 10 pt without end points
  values = c(
    "2" = "#fec98d", "4" = "#FD9567", "6" = "#F1605D",
    "8" = "#CD4071", "10" = "#9F2F7F", "12" = "#721F81",
    "14" = "#451077", "16" = "#221150"
  )
)

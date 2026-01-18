
library(magick)

combine_matchup_tables <- function(gt1_path, gt2_path, output_path = "combined_table.png") {
  library(magick)
  img1 <- image_read(gt1_path)
  img2 <- image_read(gt2_path)
  combined <- image_append(c(img1, img2), stack = FALSE)
  image_write(combined, path = output_path, format = "png")
}

# Call the function with your two table images
combine_matchup_tables(
  gt1_path = "matchup_BUD_vs_BES.png",
  gt2_path = "matchup_BES_vs_BUD.png",
  output_path = "matchup_combined_BUD_BES.png"
)



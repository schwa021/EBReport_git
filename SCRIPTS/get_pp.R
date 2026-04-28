get_pp <- function(file_path){
  
  library(pdftools)
  library(magick)
  library(stringr)
  
  # Get path and load pdf
  
  # Convert to Date object
  d <- as.Date(params$Event_Date)
  yr <- format(d, "%Y") # "2026"
  mo <- format(d, "%b") # "Feb"
  dy <- format(d, "%d") # "10"
  # file_path <- glue("L:/Motion Analysis Lab/Electronic Report/{params$MRN}/{params$MRN}{mo}{yr}Printable.pdf")
  # file_path <- "L:\\\\Motion Analysis Lab\\\\Electronic Report\\\\489828\\\\489828Feb2026Printable.pdf"
  info <- pdf_info(file_path)
  toc  <- pdf_toc(file_path) # Get Bookmarks
  
  # 2. Find the Page Number
  # We'll search for the bookmark first, then fallback to text search
  target_text <- "Average Peak Pressures for Left and Right Feet"
  
  full_text <- pdf_text(file_path)
  page_num <- which(str_detect(full_text, target_text))[1]
  
  
  # 3. Extract and Save the Image
  if (!is.na(page_num)) {
    page_image <- pdf_render_page(file_path, page = page_num, dpi = 300)
    img <- image_read(page_image)
    img_trimmed <- image_trim(img)
    border_px <- (5 / 25.4) * 300
    img_final <- image_border(img_trimmed, "white", paste0(border_px, "x", border_px))
    # image_write(img_final, path = "OUTPUT/pp_plot.png", format = "png")
  } else {
    img_final <- NULL
  }
  
  return(img_final)
}
# --- Load required libraries ---
suppressPackageStartupMessages({
  library(terra)
  library(RColorBrewer)
  library(dplyr)
})

# --- Load and prepare the raster ---
image <- rast("08-16-2025.tif")
image <- flip(image)  # flip if upside down

# Print basic info
print(image)
nlyr(image)
names(image) <- paste0("Band_", 1:nlyr(image))  # rename for clarity

# Plot as RGB (adjust bands if needed)
plotRGB(image, r = 1, g = 1, b = 1, stretch = "lin", axes = FALSE,
        main = "RGB Composite")

# --- Clustering for classification ---
vals <- values(image)
vals <- na.omit(vals)  # remove missing data

set.seed(42)
k <- 6
kmeans_result <- kmeans(vals, centers = k, iter.max = 10000)

# Create classified raster
classified <- image[[1]]
values(classified) <- NA
values(classified)[!is.na(values(image[[1]]))] <- kmeans_result$cluster

# Define class labels (adjust visually if needed)
levels(classified) <- data.frame(
  id = 1:6,
  class = c("Not on Map", "Medium Ocean Depth", "Land", "Low Ocean Depth", "Clouds", "Deep Ocean Depth")
)

# Plot classified raster
palette <- brewer.pal(k, "Set1")
plot(classified, col = palette, main = "Classified Map with Labels", axes = FALSE, legend = TRUE)

# --- Frequency and percentages ---
freq_table <- as.data.frame(freq(classified))
colnames(freq_table)[1] <- "class"

total_pixels <- sum(freq_table$count)

# Cloud indices (based on labels)
light_cloud_class <- 5  # "Light Clouds"
heavy_cloud_class <- 6  # "Heavy Cloud"

light_cloud_pixels <- freq_table$count[freq_table$class == light_cloud_class]
heavy_cloud_pixels <- freq_table$count[freq_table$class == heavy_cloud_class]

if (length(light_cloud_pixels) == 0) light_cloud_pixels <- 0
if (length(heavy_cloud_pixels) == 0) heavy_cloud_pixels <- 0

# Cloud percentages
light_cloud_percentage <- (light_cloud_pixels / total_pixels) * 100
heavy_cloud_percentage <- (heavy_cloud_pixels / total_pixels) * 100
total_cloud_percentage <- light_cloud_percentage + heavy_cloud_percentage

cat("\n--- CLOUD COVER ANALYSIS ---\n")
cat("Total pixels:", total_pixels, "\n")
cat("Light cloud percentage:", round(light_cloud_percentage, 2), "%\n")
cat("Heavy cloud percentage:", round(heavy_cloud_percentage, 2), "%\n")
cat("TOTAL CLOUD COVER:", round(total_cloud_percentage, 2), "%\n")

# --- Area statistics ---
pixel_area <- res(classified)[1] * res(classified)[2]

freq_table <- as.data.frame(freq(classified))
colnames(freq_table)[1:3] <- c("layer_id", "class_value", "count")

# Extract class levels
class_levels <- levels(classified)[[1]]

# THE FIX: freq_table$class_value contains CLASS NAMES (strings), not IDs (integers)
# So we need to merge on the "class" column, not the "id" column
area_per_class <- merge(freq_table, class_levels, 
                        by.x = "class_value", 
                        by.y = "class",  # Merge on class names, not IDs!
                        all.x = TRUE)

# Compute area and percentage
area_per_class <- area_per_class %>%
  mutate(
    area_sqm = count * pixel_area,
    percentage = round((count / sum(count)) * 100, 2)
  )

# Keep readable columns
area_per_class <- area_per_class[, c("class_value", "count", "area_sqm", "percentage")]
colnames(area_per_class)[1] <- "class_name"

print("Final area_per_class table:")
print(area_per_class)


# --- Safely extract "Land" percentage ---
land_row <- area_per_class[trimws(tolower(area_per_class$class_name)) == "land", ]

land_pct_value <- if (nrow(land_row) > 0) land_row$percentage else NA

if (!exists("land_percentage")) {
  land_percentage <- list()
}

land_percentage <- append(
  land_percentage,
  list(data.frame(
    file = "08-16-2025.tif",
    land_percentage = land_pct_value
  ))
)

# --- Save outputs ---
writeRaster(classified, "boston_bay_final.tif", overwrite = TRUE)
write.csv(area_per_class, "area_per_class_summary.csv", row.names = FALSE)

print(area_per_class)
cat("\nSaved area_per_class_summary.csv\n")
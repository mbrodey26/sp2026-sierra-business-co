# =============================================================================
# California County Land Cover Analysis
# Source: USGS Annual National Land Cover Database (NLCD)
# Author: Miles Brodey
# Description: Calculates forest, shrubland, and grassland acreage and
#              percentages for each California county from NLCD pixel counts.
# =============================================================================

################# QGIS work explained ################# 

########### Data Sources

### California County Boundaries
# County boundary polygons were downloaded from the US Census Bureau's TIGER/Line 
# Shapefile program, which provides standardized geographic boundary files for 
# administrative units across the United States. The 2023 vintage was used, 
# downloaded directly from the Census Bureau's TIGER database 
# (https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html).
# The national county file was filtered to California using the state FIPS code 
# (STATEFP = 06). In addition to geometry, this file provided key administrative 
# fields including COUNTYFP, GEOID, ALAND (land area in square meters), and AWATER 
# (water area in square meters).

### NLCD Land Cover Raster
# Land cover data was sourced from the USGS Annual National Land Cover Database 
# (NLCD), a nationally consistent 30m resolution land cover classification product 
# produced by the Multi-Resolution Land Characteristics (MRLC) Consortium. The 
# 2024 release was used (Annual_NLCD_LndCov_2024_CU_C1V1.tif), downloaded from 
# the USGS EROS Center 
# (https://www.usgs.gov/centers/eros/science/annual-national-land-cover-database).
# Each pixel in the raster is assigned a numeric land cover code representing 
# its vegetation or surface type (e.g., 41 = Deciduous Forest, 42 = Evergreen 
# Forest, 52 = Shrub/Scrub, 71 = Grassland).


########### Data Processing in QGIS

# Both layers were loaded into QGIS. The county shapefile was 
# filtered to California and reprojected to match the NLCD raster CRS 
# (Albers Equal Area, WGS84). The Zonal Histogram tool was then run with the 
# county polygons as the zone layer and the NLCD raster as the input, producing 
# pixel counts of each land cover class per county. The resulting attribute table 
# was exported as California_Topo_(Cleaned).csv.






################# Rstudio work ################# 

# --- 1. Load Libraries -------------------------------------------------------

library(tidyverse)  
library(openxlsx)   

# --- 2. Load Data ------------------------------------------------------------

df <- read_csv("Data/Raw/California Topo (raw).csv")

# --- 3. Define NLCD Land Cover Codes -----------------------------------------
# NLCD codes used:
#   41 = Deciduous Forest
#   42 = Evergreen Forest
#   43 = Mixed Forest
#   52 = Shrub/Scrub (Shrubland)
#   71 = Grassland/Herbaceous

# --- 4. Constants ------------------------------------------------------------

SQM_PER_PIXEL <- 900       # Each NLCD pixel = 30m x 30m = 900 sq meters
SQM_PER_ACRE  <- 4046.856  # Square meters per acre

# --- 5. Clean and Calculate --------------------------------------------------

ca_land <- df %>%
  # Convert relevant columns to numeric
  mutate(across(c(ALAND, nlcd_41, nlcd_42, nlcd_43, nlcd_52, nlcd_71),
                ~ as.numeric(.))) %>%
  
  # Calculate acreage for each cover type
  # Formula: pixel count * 900 sq m/pixel / 4046.856 sq m/acre
  mutate(
    total_land_acres = ALAND / SQM_PER_ACRE,
    
    forest_acres     = (nlcd_41 + nlcd_42 + nlcd_43) * SQM_PER_PIXEL / SQM_PER_ACRE,
    shrub_acres      = nlcd_52 * SQM_PER_PIXEL / SQM_PER_ACRE,
    grass_acres      = nlcd_71 * SQM_PER_PIXEL / SQM_PER_ACRE,
    
    # Total treatable land (all three combined)
    treated_acres    = forest_acres + shrub_acres + grass_acres,
    
    # Percentages of total county land
    forest_pct       = forest_acres / total_land_acres,
    shrub_pct        = shrub_acres  / total_land_acres,
    grass_pct        = grass_acres  / total_land_acres,
    treated_pct      = treated_acres / total_land_acres
  ) %>%
  
  # Select and rename columns for final output
  select(
    County           = NAME,
    `Total Land (Acres)`         = total_land_acres,
    `Forest (Acres)`             = forest_acres,
    `Forest (% of County)`       = forest_pct,
    `Shrubland (Acres)`          = shrub_acres,
    `Shrubland (% of County)`    = shrub_pct,
    `Grassland (Acres)`          = grass_acres,
    `Grassland (% of County)`    = grass_pct,
    `Total Treated Land (Acres)` = treated_acres,
    `Total Treated (% of County)`= treated_pct
  ) %>%
  
  # Sort alphabetically by county
  arrange(County)

# --- 6. Add California Totals Row --------------------------------------------

ca_totals <- ca_land %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(
    County = "CALIFORNIA TOTAL",
    # Recalculate percentages as weighted totals
    `Forest (% of County)`        = `Forest (Acres)`             / `Total Land (Acres)`,
    `Shrubland (% of County)`     = `Shrubland (Acres)`          / `Total Land (Acres)`,
    `Grassland (% of County)`     = `Grassland (Acres)`          / `Total Land (Acres)`,
    `Total Treated (% of County)` = `Total Treated Land (Acres)` / `Total Land (Acres)`
  )

ca_final <- bind_rows(ca_land, ca_totals)

# --- 7. Quick Summary Preview ------------------------------------------------

cat("\n--- Top 10 Counties by Total Treatable Acres ---\n")
ca_land %>%
  arrange(desc(`Total Treated Land (Acres)`)) %>%
  select(County, `Total Treated Land (Acres)`, `Total Treated (% of County)`) %>%
  slice(1:10) %>%
  mutate(`Total Treated Land (Acres)` = round(`Total Treated Land (Acres)`, 0),
         `Total Treated (% of County)` = scales::percent(`Total Treated (% of County)`, accuracy = 0.1)) %>%
  print()

cat("\n--- Statewide Totals ---\n")
ca_totals %>%
  select(
    `Total Land (Acres)`,
    `Forest (Acres)`,
    `Shrubland (Acres)`,
    `Grassland (Acres)`,
    `Total Treated Land (Acres)`
  ) %>%
  mutate(across(everything(), ~ round(., 0))) %>%
  print()

# --- 8. Export to Excel ------------------------------------------------------

wb <- createWorkbook()

# --- Sheet 1: County Land Cover Summary --------------------------------------
addWorksheet(wb, "County Land Cover Summary")

# Define styles
title_style <- createStyle(
  fontSize = 13, fontColour = "#1F4E79", textDecoration = "bold"
)
source_style <- createStyle(
  fontSize = 8, fontColour = "#595959", textDecoration = "italic"
)
header_style <- createStyle(
  fontSize = 9, fontColour = "white", fgFill = "#1F4E79",
  textDecoration = "bold", halign = "center", valign = "center",
  wrapText = TRUE, border = "TopBottomLeftRight", borderColour = "#BFBFBF"
)
totals_style <- createStyle(
  fontSize = 9, textDecoration = "bold", fgFill = "#D9E1F2",
  border = "TopBottomLeftRight", borderColour = "#BFBFBF"
)
county_style <- createStyle(
  fontSize = 9, border = "TopBottomLeftRight", borderColour = "#BFBFBF"
)
acres_style <- createStyle(
  fontSize = 9, numFmt = "#,##0",
  border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right"
)
pct_style <- createStyle(
  fontSize = 9, numFmt = "0.0%",
  border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right"
)
forest_fill  <- createStyle(fgFill = "#E2EFDA", numFmt = "#,##0",
                            border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right")
forest_pct   <- createStyle(fgFill = "#E2EFDA", numFmt = "0.0%",
                            border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right")
shrub_fill   <- createStyle(fgFill = "#FFF2CC", numFmt = "#,##0",
                            border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right")
shrub_pct    <- createStyle(fgFill = "#FFF2CC", numFmt = "0.0%",
                            border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right")
grass_fill   <- createStyle(fgFill = "#FCE4D6", numFmt = "#,##0",
                            border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right")
grass_pct    <- createStyle(fgFill = "#FCE4D6", numFmt = "0.0%",
                            border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right")
treated_fill <- createStyle(fgFill = "#DAEEF3", numFmt = "#,##0",
                            border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right")
treated_pct  <- createStyle(fgFill = "#DAEEF3", numFmt = "0.0%",
                            border = "TopBottomLeftRight", borderColour = "#BFBFBF", halign = "right")

# Write title and source rows
writeData(wb, "County Land Cover Summary",
          "California County Land Cover Analysis — Forest, Shrubland & Grassland",
          startRow = 1, startCol = 1)
addStyle(wb, "County Land Cover Summary", title_style, rows = 1, cols = 1)
mergeCells(wb, "County Land Cover Summary", cols = 1:10, rows = 1)

writeData(wb, "County Land Cover Summary",
          "Source: USGS Annual National Land Cover Database (NLCD) | Units: Acres and % of Total County Land Area",
          startRow = 2, startCol = 1)
addStyle(wb, "County Land Cover Summary", source_style, rows = 2, cols = 1)
mergeCells(wb, "County Land Cover Summary", cols = 1:10, rows = 2)

# Write data starting at row 4 (row 3 is a spacer)
writeData(wb, "County Land Cover Summary", ca_final, startRow = 4, startCol = 1)

n_rows      <- nrow(ca_final)
data_start  <- 5                        # first data row (after header at row 4)
data_end    <- data_start + n_rows - 1  # last data row
totals_row  <- data_end                 # totals are the last row

# Apply header style
addStyle(wb, "County Land Cover Summary", header_style,
         rows = 4, cols = 1:10, gridExpand = TRUE)

# Apply column styles to data rows (excluding totals row)
body_rows <- data_start:(totals_row - 1)

addStyle(wb, "County Land Cover Summary", county_style,  rows = body_rows, cols = 1)
addStyle(wb, "County Land Cover Summary", acres_style,   rows = body_rows, cols = 2)
addStyle(wb, "County Land Cover Summary", forest_fill,   rows = body_rows, cols = 3)
addStyle(wb, "County Land Cover Summary", forest_pct,    rows = body_rows, cols = 4)
addStyle(wb, "County Land Cover Summary", shrub_fill,    rows = body_rows, cols = 5)
addStyle(wb, "County Land Cover Summary", shrub_pct,     rows = body_rows, cols = 6)
addStyle(wb, "County Land Cover Summary", grass_fill,    rows = body_rows, cols = 7)
addStyle(wb, "County Land Cover Summary", grass_pct,     rows = body_rows, cols = 8)
addStyle(wb, "County Land Cover Summary", treated_fill,  rows = body_rows, cols = 9)
addStyle(wb, "County Land Cover Summary", treated_pct,   rows = body_rows, cols = 10)

# Apply totals row style
addStyle(wb, "County Land Cover Summary", totals_style,
         rows = totals_row, cols = 1:10, gridExpand = TRUE)

# Set column widths
setColWidths(wb, "County Land Cover Summary",
             cols = 1:10,
             widths = c(20, 16, 16, 14, 16, 14, 16, 14, 18, 14))

# Freeze panes below header
freezePane(wb, "County Land Cover Summary", firstActiveRow = 5)

# --- Sheet 2: NLCD Code Reference --------------------------------------------
addWorksheet(wb, "NLCD Code Reference")

ref_data <- tibble(
  Code        = c("41", "42", "43", "52", "71"),
  `Land Cover Class` = c("Deciduous Forest", "Evergreen Forest", "Mixed Forest",
                         "Shrub/Scrub", "Grassland/Herbaceous"),
  `Category Used` = c("Forest", "Forest", "Forest", "Shrubland", "Grassland"),
  Description = c(
    "Areas dominated by trees >5m tall; >20% cover; >75% deciduous",
    "Areas dominated by trees >5m tall; >20% cover; >75% evergreen",
    "Areas dominated by trees >5m tall; 20-75% mix of deciduous/evergreen",
    "Areas dominated by shrubs <5m tall; >20% cover; true shrubs or young trees",
    "Areas dominated by upland grasses and forbs; <20% tree cover"
  )
)

writeData(wb, "NLCD Code Reference", "NLCD Code Reference", startRow = 1)
addStyle(wb, "NLCD Code Reference",
         createStyle(fontSize = 13, fontColour = "#1F4E79", textDecoration = "bold"),
         rows = 1, cols = 1)

writeData(wb, "NLCD Code Reference", ref_data, startRow = 3)
addStyle(wb, "NLCD Code Reference", header_style, rows = 3, cols = 1:4, gridExpand = TRUE)

setColWidths(wb, "NLCD Code Reference", cols = 1:4, widths = c(10, 28, 18, 55))

# --- 9. Save -----------------------------------------------------------------

saveWorkbook(wb, "ca_land_cover_analysis.xlsx", overwrite = TRUE)
cat("\nDone! File saved as ca_land_cover_analysis.xlsx\n")

# ─── Step 1: Setup ─────────────────────────────────────────────
library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

# Load your pre-processed RDS file
all_shots <- readRDS("shiny/data/shot_data.rds")  # Already created from precache_shots.R

# ─── Step 2: Create SQLite DB ──────────────────────────────────
con <- dbConnect(SQLite(), "shiny/data/shot_data.sqlite")

# Optional: remove existing table to overwrite fresh
if (dbExistsTable(con, "shots")) {
  dbRemoveTable(con, "shots")
}

# ─── Step 3: Write to DB ───────────────────────────────────────
dbWriteTable(con, "shots", all_shots)

# ─── Step 4: Close connection ──────────────────────────────────
dbDisconnect(con)

cat("✅ shot_data.sqlite created successfully.\n")

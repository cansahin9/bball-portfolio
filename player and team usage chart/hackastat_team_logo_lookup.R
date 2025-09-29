setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/player and team usage chart")

library(tibble)

team_lookup <- tibble(
  scraped_name = c(
    "Valencia Basket", 
    "Hapoel Tel Aviv", 
    "CB Gran Canaria", 
    "Hapoel Jerusalem BC", 
    "Badalona", 
    "Reyer Venezia", 
    "Besiktas JK ", 
    "Aquila Trento", 
    "Turk Ankara", 
    "BC Lietkabelis", 
    "Bahçeşehir Koleji SK", 
    "Wolves Vilnius", 
    "KK Budućnost", 
    "JL Bourg-en-Bresse",  
    "KK Cedevita Junior", 
    "Aris BC",          
    "ratiopharm Ulm", 
    "Trefl Sopot", 
    "Hamburg Towers", 
    "Cluj-Napoca" 
  ),
  team_code = c(
    "PAM",   
    "HTA", 
    "CAN", 
    "JER", 
    "JOV",
    "VNC", 
    "BES", 
    "TRN",
    "TTK",
    "LKB", 
    "BAH", 
    "WOL",
    "BUD", 
    "BOU",
    "LJU",
    "ARI", 
    "ULM", 
    "TSO",
    "HAM",  
    "CLU"
  )
)

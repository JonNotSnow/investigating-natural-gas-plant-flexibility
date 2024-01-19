##############################################################################################
# Generate BA map
##############################################################################################
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(maps)

shapefile <- read_sf('Control_Areas-shp/Control_Areas.shp')
shapefile <- shapefile %>%
  arrange(NAME)

shapefile_filtered <- shapefile %>%
  # select(-c(OBJECTID, ID, TELEPHONE)) %>%
  filter(NAME %in% c("BONNEVILLE POWER ADMINISTRATION",
                     "DUKE ENERGY PROGRESS EAST",
                     "FLORIDA POWER & LIGHT COMPANY",
                     "ISO NEW ENGLAND INC.",
                     "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..",
                     "NEVADA POWER COMPANY",
                     "CALIFORNIA INDEPENDENT SYSTEM OPERATOR",
                     "NEW YORK INDEPENDENT SYSTEM OPERATOR",
                     "PACIFICORP - EAST",
                     "ARIZONA PUBLIC SERVICE COMPANY",
                     # "PACIFICORP - WEST",
                     "PJM INTERCONNECTION, LLC",
                     "SOUTHERN COMPANY SERVICES, INC. - TRANS",
                     "SALT RIVER PROJECT",
                     "SOUTHWEST POWER POOL",
                     "TENNESSEE VALLEY AUTHORITY",
                     "ASSOCIATED ELECTRIC COOPERATIVE, INC.",
                     "WESTERN AREA POWER ADMINISTRATION - ROCKY MOUNTAIN REGION",
                     "PUBLIC SERVICE COMPANY OF COLORADO",
                     "ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.",
                     "LOS ANGELES DEPARTMENT OF WATER AND POWER"))
# arrange(desc(NAME))
shapefile_filtered <- shapefile_filtered[c(3, 5, 7, 8, 10, 11, 4, 12, 13, 1,
                                           14, 17, 16, 18, 19, 2, 20, 15, 6, 9), ]
shapefile_filtered$NAMEfac <- factor(shapefile_filtered$NAME, levels=unique(as.character(shapefile_filtered$NAME)))

state <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
cities <- data.frame(city = c("Miami", "Las Vegas"),
                     BA = c("FPL", "NEVP"),
                     lat = c(25.7617, 36.1699),
                     lng = c(-80.1918, -115.1398))
cities <- st_as_sf(cities, coords = c("lng", "lat"), remove = FALSE, 
                   crs = 4326, agr = "constant")

ggplot() + # render to 1600*1200
  # geom_sf(data = cities) + 
  geom_sf(data = shapefile_filtered,
          aes(fill = NAMEfac), alpha = 1) +
  geom_sf(data = state, color = "gray40", fill = NA) +
  # geom_text_repel(data = cities, aes(x = lng, y = lat, label = BA),
  #                 fontface = "bold", nudge_x = c(1.5, -10), nudge_y = c(0.25, -0.25)) +
  # coord_sf(xlim = c(-130, -60), ylim = c(23, 50), expand = FALSE) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        legend.position = "none", legend.title = element_blank(), 
        legend.text = element_text(size = 18)) + 
  scale_fill_manual(# breaks = c("BONNEVILLE POWER ADMINISTRATION",
    # "DUKE ENERGY PROGRESS EAST",
    # "FLORIDA POWER & LIGHT COMPANY",
    # "ISO NEW ENGLAND INC.",
    # "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..",
    # "NEVADA POWER COMPANY",
    # "CALIFORNIA INDEPENDENT SYSTEM OPERATOR",
    # "NEW YORK INDEPENDENT SYSTEM OPERATOR",
    # "PACIFICORP - EAST",
    # "ARIZONA PUBLIC SERVICE COMPANY",
    # # "PACIFICORP - WEST",
    # "PJM INTERCONNECTION, LLC",
    # "SOUTHERN COMPANY SERVICES, INC. - TRANS",
    # "SALT RIVER PROJECT",
    # "SOUTHWEST POWER POOL",
    # "TENNESSEE VALLEY AUTHORITY",
    # "ASSOCIATED ELECTRIC COOPERATIVE, INC.",
    # "WESTERN AREA POWER ADMINISTRATION - ROCKY MOUNTAIN REGION",
    # "PUBLIC SERVICE COMPANY OF COLORADO",
    # "ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.",
    # "LOS ANGELES DEPARTMENT OF WATER AND POWER"),
    labels = c("BPAT", "CPLE", "FPL", "ISNE", "MISO",
               "NEVP", "CISO", "NYIS", "PACE", "AZPS",
               "PJM", "SOCO", "SRP", "SWPP", "TVA",
               "AECI", "WACM", "PSCO", "ERCO", "LDWP"),
    # values = c("grey50", "gold1", "green4", "green1", "#FDBF6F",
    #            "deeppink1", "palegreen2", "#6A3D9A", "#FB9A99", "#FF7F00",
    #            "#E31A1C", "darkorange4", "#CAB2D6", "khaki2", "blue1",
    #            "orchid1", "skyblue2", "yellow4", "dodgerblue2", "brown")
    values = c("lightblue4", "gold1", "green4", "darkolivegreen1", "#FDBF6F",
               "peru", "palegreen2", "#6A3D9A", "#FB9A99", "#FF7F00",
               "tomato2", "lightseagreen", "lightslateblue", "khaki2", "royalblue4",
               "orchid1", "skyblue2", "yellow4", "dodgerblue2", "brown")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


ggsave("BAmap2.pdf", width = 12, height = 8, units = "in", dpi = 300)







##############################################################################################
# Generate map for ERCO as an example
##############################################################################################

# ERCO
# ERCO_860 <- eia860_merged %>%
#   filter(Balancing_Authority_Code == "ERCO")
# ERCO_923 <- eia923 %>%
#   filter(Balancing_Authority_Code == "ERCO") %>%
#   select(Plant_Code, Reported_Prime_Mover, Reported_Fuel_Type_Code, Net_Generation_Megawatthours)
# 
# ERCO_merged <- merge(ERCO_923, ERCO_860, by = "Plant_Code", all.x = TRUE, all.y = TRUE)
# ERCO_merged <- ERCO_merged %>%  
#   filter(Reported_Fuel_Type_Code == "NG")

# SWPP
# SWPP_860 <- eia860 %>%
#   filter(Balancing_Authority_Code == "SWPP")
# SWPP_923 <- eia923 %>%
#   filter(Balancing_Authority_Code == "SWPP") %>%
#   select(Plant_Code, Reported_Prime_Mover, Reported_Fuel_Type_Code, Net_Generation_Megawatthours)
# 
# SWPP_merged <- merge(SWPP_923, SWPP_860, by = "Plant_Code") %>%
#   filter(Reported_Fuel_Type_Code == "NG")


# GET MAP DATA
# world_map_data <- ne_countries(scale = "medium", returnclass = "sf")
# state_map_data <- map('state', fill = TRUE, plot = FALSE) %>% st_as_sf()
# shapefile_BA <- read_sf('Control_Areas-shp/Control_Areas.shp')
# shapefile_BA <- shapefile_BA %>%
#   arrange(NAME)
# 
# shapefile_ERCO <- shapefile_BA %>%
#   # select(-c(OBJECTID, ID, TELEPHONE)) %>%
#   filter(NAME == "ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.")
# 
# ggplot() +
#   # geom_sf(data = world_map_data) +
#   # geom_sf(data = state_map_data) +
#   geom_sf(data = state_map_data, color = "gray40", fill = NA) +
#   geom_sf(data = shapefile_ERCO, alpha = 1, fill = "dodgerblue2") +
#   geom_point(data = ERCO_merged, aes(x = Longitude, y = Latitude), color = "red") + 
#   coord_sf(xlim = c(-107, -92), ylim = c(25, 36.5))
###############################################################################
# Topic: Estimating Trump's Tariff Impact on Global Welfare
# Goal: Set variables for the analysis
# Keywords: Tariffs, Global Welfare, Trade Policy, International Trade
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-02
###############################################################################

# Original vector of sectors
raw_sectors <- c(
  "Agriculture, Hunting, Forestry and Fishing",
  "Mining and Quarrying",
  "Food, Beverages and Tobacco",
  "Textiles and Textile Products",
  "Leather, Leather and Footwear",
  "Wood and Products of Wood and Cork",
  "Pulp, Paper, Paper , Printing and Publishing",
  "Coke, Refined Petroleum and Nuclear Fuel",
  "Chemicals and Chemical Products",
  "Rubber and Plastics",
  "Other Non-Metallic Mineral",
  "Basic Metals and Fabricated Metal",
  "Machinery, Nec",
  "Electrical and Optical Equipment",
  "Transport Equipment",
  "Manufacturing, Nec; Recycling",
  "Electricity, Gas and Water Supply",
  "Construction",
  "Sale, Maintenance and Repair of Motor Vehicles and Motorcycles; Retail Sale of Fuel",
  "Wholesale Trade and Commission Trade, Except of Motor Vehicles and Motorcycles",
  "Retail Trade, Except of Motor Vehicles and Motorcycles; Repair of Household Goods",
  "Hotels and Restaurants",
  "Inland Transport",
  "Water Transport",
  "Air Transport",
  "Other Supporting and Auxiliary Transport Activities; Activities of Travel Agencies",
  "Post and Telecommunications",
  "Financial Intermediation",
  "Real Estate Activities",
  "Renting of M&Eq and Other Business Activities",
  "Public Admin and Defence; Compulsory Social Security",
  "Education",
  "Health and Social Work",
  "Other Community, Social and Personal Services",
  "Private Households with Employed Persons"
)

# Mapping vector
clean_sectors <- c(
  "Agriculture", "Mining", "Food", "Textiles", "Leather & Footwear",
  "Wood", "Paper & Printing", "Petroleum & Coke", "Chemicals", "Rubber & Plastics",
  "Non-Metallic Minerals", "Metals", "Machinery", "Electrical & Optical", "Transport Equipment",
  "Other Manufacturing", "Utilities", "Construction", "Motor Vehicle Trade", "Wholesale Trade",
  "Retail Trade", "Hotels & Restaurants", "Inland Transport", "Water Transport", "Air Transport",
  "Transport Support", "Telecom & Post", "Finance", "Real Estate", "Business Services",
  "Public Admin", "Education", "Health & Social Work", "Community Services", "Private Households"
)

#####################################################################
#### Multi-LOB Claims Generation with Cause-of-Loss and Nat-Cat Flagging
#####################################################################

# This script generates claims data separately for each Line of Business (LOB)
# and enriches each LOB with LOB-specific cause-of-loss assignments and
# natural catastrophe (NAT_CAT) flags.

# Load required libraries and parameters from main script
library(SynthETIC)
library(plyr)
library(locfit)
library(dplyr)
library(actuar)
library(tidyr)

# Set up global parameters (same as data simulation.r)
ref_claim <- 1
time_unit <- 1/12
set_parameters(ref_claim=ref_claim, time_unit=time_unit)

# Portfolio and simulation parameters
annual_exposure_growth <- 0.03
settlement_speed_by_lob <- list(
  health = 0.7,
  personal_accident = 1.0,
  motor_liability = 1.2,
  general_liability = 1.5,
  other_motor = 0.9,
  property = 0.8
)
simulation_start_date <- as.Date("2014-12-31")
simulation_start_year <- as.integer(format(simulation_start_date + 1, "%Y"))
years <- 12
I <- years/time_unit

#####################################################################
#### Business Mix Configuration
#####################################################################

# Define the relative business mix (proportion of total premium by LOB)
# Note: Keep values >= 0.40 to avoid SynthETIC edge cases with small exposures
# These proportions represent realistic portfolio composition with stable claim generation.
business_mix <- list(
  health = 0.10,              # 40% relative exposure
  personal_accident = 0.10,   # 40% relative exposure
  general_liability = 0.13,   # 40% relative exposure
  motor_liability = 0.35,     # 50% relative exposure (higher due to frequent claims)
  other_motor = 0.15,         # 40% relative exposure
  property = 0.17             # 40% relative exposure
) #sum(unlist(business_mix))
# Note: Relative multipliers for base exposure vector; not normalized to 1.0.
# They can be tuned per LOB to model realistic business mix and premium distribution.

#####################################################################
#### LOB Configuration
#####################################################################

# Define which claim types belong to each LOB, with type-specific cause probabilities
lob_config <- list(
  health = list(
    types = c(1, 2),
    causes = c("sports", "road_accident", "emergency", "surgery", 
               "chronic_condition", "work_accident", "illness"),
    type_cause_probs = list(
      `1` = c(work_accident=0.30, illness=0.25, road_accident=0.20, emergency=0.15, sports=0.07, surgery=0.03),
      `2` = c(emergency=0.40, surgery=0.35, chronic_condition=0.15, illness=0.10)
    )
  ),
  personal_accident = list(
    types = c(1, 4),
    causes = c("sports", "road_accident", "domestic", "work_accident"),
    type_cause_probs = list(
      `1` = c(sports=0.40, work_accident=0.35, road_accident=0.20, domestic=0.05),
      `4` = c(road_accident=0.40, sports=0.35, domestic=0.20, work_accident=0.05)
    )
  ),
  general_liability = list(
    types = c(2, 5),
    causes = c("property_damage", "product_liability", "professional_negligence", 
               "slip_and_fall", "environmental", "bodily_injury"),
    type_cause_probs = list(
      `2` = c(environmental=0.50, product_liability=0.30, bodily_injury=0.15, professional_negligence=0.05),
      `5` = c(slip_and_fall=0.35, professional_negligence=0.30, property_damage=0.20, bodily_injury=0.10, product_liability=0.05)
    )
  ),
  motor_liability = list(
    types = c(3, 5, 6),
    causes = c("motor_injury", "motor_damage", "hit_and_run", "pedestrian_injury"),
    type_cause_probs = list(
      `3` = c(motor_injury=0.45, motor_damage=0.35, pedestrian_injury=0.15, hit_and_run=0.05),
      `5` = c(motor_damage=0.40, motor_injury=0.35, hit_and_run=0.15, pedestrian_injury=0.10),
      `6` = c(hit_and_run=0.40, motor_injury=0.30, motor_damage=0.20, pedestrian_injury=0.10)
    )
  ),
  other_motor = list(
    types = c(3, 4),
    causes = c("theft_vandalism", "animal", "rear_end", "collision", 
               "pedestrian_cyclist", "hail", "windstorm", "other"),
    type_cause_probs = list(
      `3` = c(collision=0.30, rear_end=0.25, pedestrian_cyclist=0.20, other=0.15, windstorm=0.05, hail=0.03, theft_vandalism=0.02),
      `4` = c(hail=0.35, windstorm=0.30, theft_vandalism=0.20, collision=0.10, other=0.05)
    )
  ),
  property = list(
    types = c(2, 6),
    causes = c("theft", "water_damage", "flood", "fire", "hail", 
               "earthquake", "windstorm", "vandalism"),
    type_cause_probs = list(
      `2` = c(fire=0.40, earthquake=0.30, flood=0.15, water_damage=0.10, other=0.05),
      `6` = c(hail=0.40, windstorm=0.40, water_damage=0.12, theft=0.05, vandalism=0.03)
    )
  )
)

# Define which causes are Natural Catastrophes (NAT_CAT)
nat_cat_causes <- c("windstorm", "hail", "earthquake", "flood", "fire", "environmental")

#####################################################################
#### Generate Claims per LOB
#####################################################################

# Source the main simulation functions
source("./Tools/functions simulation.r")

# Container for all LOB data
all_claims <- data.frame()
all_paid <- data.frame()
all_reopen <- data.frame()

# Track cumulative claim IDs for proper numbering across LOBs
cumulative_id <- 0

# Set seed for reproducibility across LOBs
base_seed <- 1000

for (lob_idx in seq_along(lob_config)) {
  lob_name <- names(lob_config)[lob_idx]
  lob_spec <- lob_config[[lob_idx]]
  lob_mix <- business_mix[[lob_name]]
  
  cat("Generating LOB:", lob_name, "with types:", paste(lob_spec$types, collapse=", "),
      "| Business mix:", lob_mix, "\n")
  
  # Generate claims for this LOB with its specific types and scaled exposure
  lob_data <- data.generation(
    seed = base_seed + lob_idx * 100,
    future_info = FALSE,
    types_to_simulate = lob_spec$types,
    lob_name = lob_name,
    exposure_scale = lob_mix  # Apply business mix scaling
  )
  
  lob_claims <- lob_data$claims
  lob_paid <- lob_data$paid
  lob_reopen <- lob_data$reopen
  
  # Ensure reopen is a dataframe (might be NULL if no reopenings)
  if (is.null(lob_reopen)) {
    lob_reopen <- data.frame()
  }
  
  # Add LOB identifier
  lob_claims$LOB <- lob_name
  lob_paid$LOB <- lob_name
  if (nrow(lob_reopen) > 0) {
    lob_reopen$LOB <- lob_name
  }
  
  # Add Cause-of-Loss: use type-aware probability distribution
  lob_claims$CauseOfLoss <- sapply(1:nrow(lob_claims), function(i) {
    claim_type <- as.character(lob_claims$Type[i])
    type_probs <- lob_spec$type_cause_probs[[claim_type]]
    
    if (!is.null(type_probs)) {
      # Use type-specific probabilities
      sample(names(type_probs), size = 1, prob = type_probs)
    } else {
      # Fallback to uniform if type not in config
      sample(lob_spec$causes, size = 1)
    }
  })
  
  lob_paid$CauseOfLoss <- lob_claims$CauseOfLoss[match(lob_paid$Id, lob_claims$Id)]
  if (nrow(lob_reopen) > 0) {
    lob_reopen$CauseOfLoss <- lob_claims$CauseOfLoss[match(lob_reopen$Id, lob_claims$Id)]
  }
  
  # Add NAT_CAT Flag: 1 if cause is a natural catastrophe, 0 otherwise
  lob_claims$CatFlag <- as.integer(lob_claims$CauseOfLoss %in% nat_cat_causes)
  lob_paid$CatFlag <- lob_claims$CatFlag[match(lob_paid$Id, lob_claims$Id)]
  if (nrow(lob_reopen) > 0) {
    lob_reopen$CatFlag <- lob_claims$CatFlag[match(lob_reopen$Id, lob_claims$Id)]
  }
  
  # Adjust IDs to be cumulative across LOBs
  id_offset <- cumulative_id
  lob_claims$Id <- lob_claims$Id + id_offset
  lob_paid$Id <- lob_paid$Id + id_offset
  lob_reopen$Id <- lob_reopen$Id + id_offset
  
  cumulative_id <- max(lob_claims$Id)
  
  # Append to consolidated datasets (just claims and paid; reopen handled separately)
  all_claims <- rbind(all_claims, lob_claims)
  all_paid <- rbind(all_paid, lob_paid)
  if (nrow(lob_reopen) > 0) {
    all_reopen <- rbind(all_reopen, lob_reopen)
  }
  
  cat("  Generated", nrow(lob_claims), "claims for", lob_name, "\n")
}

# Final sort by occurrence date and ID
all_claims <- all_claims %>% dplyr::arrange(AccDate, Id)
all_paid <- all_paid %>% dplyr::arrange(Id, EventId)
if (nrow(all_reopen) > 0) {
  all_reopen <- all_reopen %>% dplyr::arrange(Id)
}

cat("\n=== CONSOLIDATED CLAIMS SUMMARY ===\n")
cat("Total claims across all LOBs:", nrow(all_claims), "\n")
cat("Total payments:", nrow(all_paid), "\n")
cat("Total reopenings:", nrow(all_reopen), "\n\n")

# Summary by LOB
cat("Claim counts by LOB:\n")
print(all_claims %>%
  dplyr::group_by(LOB) %>%
  dplyr::summarise(
    count = n(),
    avg_ultimate = round(mean(Ultimate), 2),
    min_ultimate = round(min(Ultimate), 2),
    max_ultimate = round(max(Ultimate), 2),
    cat_flag_count = sum(CatFlag)
  ))

cat("\n")

# Summary by LOB and Cause
cat("Claim counts by LOB and Cause:\n")
print(all_claims %>%
  dplyr::group_by(LOB, CauseOfLoss) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::arrange(LOB, desc(count)))

cat("\n")

# Natural Catastrophe summary
nat_cat_summary <- all_claims %>%
  dplyr::filter(CatFlag == 1) %>%
  dplyr::group_by(LOB, CauseOfLoss) %>%
  dplyr::summarise(
    count = n(),
    total_ultimate = round(sum(Ultimate), 2),
    avg_ultimate = round(mean(Ultimate), 2),
    .groups = "drop"
  ) %>%
  dplyr::arrange(LOB, CauseOfLoss)

if (nrow(nat_cat_summary) > 0) {
  cat("Natural Catastrophe claims summary:\n")
  print(nat_cat_summary)
} else {
  cat("No natural catastrophe claims generated in this run.\n")
}

#####################################################################
#### Export consolidated datasets
#####################################################################

# Optionally save the consolidated datasets
# save(all_claims, all_paid, all_reopen, file = "./Data/lob_claims.rda")

cat("\nMulti-LOB data generation complete.\n")
cat("Use: all_claims, all_paid, all_reopen for downstream analysis.\n")

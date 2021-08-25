# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#####################################################################################
# 01_load.R
# script to load provincially and federally mandated species
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 24-Aug-2021
#####################################################################################

.libPaths("C:/Program Files/R/R-4.0.5/library") # to ensure reading/writing libraries from C drive
tz = Sys.timezone() # specify timezone in BC

# Load Packages
list.of.packages <- c("tidyverse", "lubridate","bcdata", "bcmaps","sp","sf")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

###---
# Provincially mandated wildlife (birds)
# Birds of the following species:
#   
# (a)all species of birds described in the American Ornithologists Union Checklist of North America Birds, 7th edition (American Ornithologists Union 1999) or its supplements, which are native to Canada or the United States of America and were not introduced by man;
# 
# (b)Alauda arvensis — skylark;
# 
# (c)Acridotheres cristatellus — crested myna;
# 
# (d)Passer domesticus — house sparrow;
# 
# (e)Sturnus vulgaris — European starling;
# 
# (f)Columba livia — rock dove (except domestic-bred racing pigeon).

# download directly as csv (American Ornithologist checklist)
# http://checklist.americanornithology.org/taxa/
# A accidental/casual in AOU area
# H recorded in AOU area only from Hawaii
# I introduced into AOU area
# N has not bred in AOU area, but occurs regularly as nonbreeding visitor
# † extinct
# * probably misplaced in the current phylogenetic listing, but data indicating proper placement are not yet available

NACC <- read.csv("http://checklist.americanornithology.org/taxa.csv?type=charset%3Dutf-8%3Bsubspecies%3Dno%3B/NACC_list_species.csv",
                 na.strings=c("NA","NaN", ""))
glimpse(NACC)
head(NACC)
NACC <- NACC %>% filter(is.na(status_accidental)) %>% filter(is.na(status_extinct))%>%
  filter(is.na(status_hawaiian)) %>%  filter(is.na(status_introduced))%>%
  filter(is.na(status_misplaced))%>%  filter(is.na(status_nonbreeding))

NACC %>% count(family)

# Families not covered under the migratory bird act
# Accipitridae (osprey, kite, eagles, harrier, hawks)
# Alcedinidae (kingfisher)
# Anhingidae (anhinga)
# Aramidae (limpkin)
# Cathartidae (vultures)
# Corvidae (jays, crows and ravens)
# Falconidae (caracara, falcons)
# Fregatidae (frigatebirds)
# Odontophoridae (quail)
# Passeridae (weaver finches)
# Pelecanidae (pelicans)
# Phaethontidae (tropicbirds)
# Phalacrocoracidae (cormorants)
# Phasianidae (partridges, pheasants, grouse, ptarmigan, sage-grouse, prairie-chicken, turkey)
# Phoenicopteridae (flamingo)
# Prunellidae (accentor)
# Ptilogonatidae (silky-flycatcher)
# Strigidae (owls)
# Sturnidae (starlings, mynas)
# Tytonidae (barn owl)

nonMBA <- c("Accipitridae", "Alcedinidae", "Anhingidae", "Aramidae", "Cathartidae", "Corvidae", "Falconidae",
            "Fregatidae", "Odontophoridae", "Passeridae", "Pelecanidae", "Phaethontidae", "Phalacrocoracidae",
            "Phasianidae", "Phoenicopteridae", "Prunellidae", "Ptilogonatidae", "Strigidae", "Sturnidae",
            "Tytonidae")

NACC %>% filter(!family %in% nonMBA) %>% count(family)

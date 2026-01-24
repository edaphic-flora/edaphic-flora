# Data Sources

This document provides attribution and license information for third-party data sources used in Edaphic Flora.

## Species Taxonomy

- **Source**: World Checklist of Vascular Plants (WCVP)
- **Provider**: Royal Botanic Gardens, Kew
- **License**: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
- **URL**: https://wcvp.science.kew.org/
- **Citation**: Govaerts, R. et al. (2021). The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. *Scientific Data* 8, 215. https://doi.org/10.1038/s41597-021-00997-6
- **Files**: `app/species_accepted.csv`, `app/data/wcvp_species_min.rds`

The WCVP data is used under CC BY 4.0, which permits sharing and adaptation for any purpose with appropriate attribution.

## USDA Plant Characteristics

- **Source**: USDA PLANTS Database
- **Provider**: USDA Natural Resources Conservation Service
- **License**: Public Domain (CC0)
- **URL**: https://plants.usda.gov/
- **Files**: `app/data/raw/usda/usda_traits.csv`, `app/data/processed/ref_usda_traits.csv`, `app/data/processed/ref_taxon.csv`

As a work of the U.S. Government, USDA PLANTS data is not subject to copyright within the United States.

## Wetland Indicators

- **Source**: National Wetland Plant List (NWPL)
- **Provider**: U.S. Army Corps of Engineers
- **License**: Public Domain (U.S. Government Work)
- **URL**: https://wetland-plants.usace.army.mil/
- **Files**: `app/data/raw/nwpl/2022_NWPL_*.xlsx`

The NWPL is an interagency effort led by USACE and supported by EPA, USFWS, and USDA-NRCS. As a federal government product, it is in the public domain.

## Ecoregion Data

- **Source**: EPA Level IV Ecoregions of the United States
- **Provider**: U.S. Environmental Protection Agency
- **License**: Public Domain (U.S. Government Work)
- **URL**: https://www.epa.gov/eco-research/ecoregions
- **R Package**: `ecoregions` (used for spatial lookups)

EPA ecoregion shapefiles are public domain federal data.

## State Boundaries

- **Source**: U.S. Census Bureau TIGER/Line Shapefiles
- **Provider**: U.S. Census Bureau
- **License**: Public Domain (U.S. Government Work)
- **URL**: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
- **R Package**: `tigris` (used to generate `app/data/state_grid.rds`)

Census geographic data is public domain.

## Invasive Species Data

### State Invasive Species Lists

- **Source**: State invasive species councils, departments of agriculture, and natural resource agencies
- **Provider**: Various (50 states)
- **License**: Public Domain (state government works) or as specified by each state
- **Files**: `data/state_invasive_for_import.csv`, database table `ref_noxious_invasive`

Data compiled from official state sources including state Exotic Pest Plant Councils (EPPCs), state departments of agriculture, and invasive species councils. Individual state sources are documented in `scripts/fetch_state_invasive_lists.R`.

### Invasive Plant Atlas

- **Source**: Invasive Plant Atlas of the United States
- **Provider**: University of Georgia Center for Invasive Species and Ecosystem Health
- **License**: Educational/research use
- **URL**: https://www.invasiveplantatlas.org/

Supplementary invasive species distribution data used to enhance state-level coverage.

## Geocoding

- **Source**: OpenStreetMap via Nominatim
- **Provider**: OpenStreetMap contributors
- **License**: [ODbL 1.0](https://opendatacommons.org/licenses/odbl/)
- **URL**: https://www.openstreetmap.org/
- **R Package**: `tidygeocoder` (used for address/coordinate lookups)

Geocoding services are provided by Nominatim using OpenStreetMap data. OpenStreetMap data is available under the Open Database License (ODbL).

---

## License Compatibility Note

Edaphic Flora uses a dual-license model:
- **Code**: AGPL-3.0-or-later
- **User-contributed data**: CC BY-NC 4.0

Third-party data sources retain their original licenses as noted above. The WCVP data (CC BY 4.0) permits commercial use; the NonCommercial restriction in our CC BY-NC 4.0 license applies specifically to user-contributed soil sample data, not to the underlying taxonomy reference data.

---

## Questions?

For licensing inquiries, contact: edaphicflora@gmail.com

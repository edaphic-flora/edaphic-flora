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

## Native Status by State

- **Source**: BONAP Taxonomic Data Center
- **Provider**: Biota of North America Program (John T. Kartesz), Chapel Hill, N.C.
- **License**: Used with permission from BONAP
- **URL**: https://bonap.net/tdc
- **Citation**: Kartesz, J.T., The Biota of North America Program (BONAP). 2015. Taxonomic Data Center. Chapel Hill, N.C. (https://bonap.net/tdc)
- **Files**: `data/bonap_state_nativity_compiled.csv`, database table `ref_state_distribution` (`source = 'BONAP'`)

State-level native and introduced status shown in the app and on the public site (edaphicflora.org) comes from BONAP and is credited on every species page and on the About page. Where BONAP has no record for a species, USDA PLANTS state data fills in and is labeled as USDA.

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

## Wildlife Associations

- **Moths and butterflies**: caterpillar host plant associations from Tallamy, D.W. & Shropshire, K.J. (National Wildlife Federation)
- **Specialist bees**: native bee and host plant associations from Jarrod Fowler
- **Birds**: bird and plant associations adapted from the Audubon native plants database
- **Occurrence data**: GBIF and eBird (Cornell Lab of Ornithology) for state-level wildlife presence in the app's dashboard
- **Files**: compiled source workbooks are proprietary and never committed (`data/wildlife_source/`); loaded into the `ref_wildlife_*` tables

The public site publishes only per-plant counts and highlight flags (moth and butterfly species count, specialist bee count, songbird value, bird food, keystone genus, hummingbird plant) through the `public_site.wildlife_*` views in `site/sql/public_views.sql`. Interaction lists and wildlife species names are not published.

Signed-in people also see My Garden (`/my-garden/`, `site/sql/garden.sql`): for the plants in their own soil tests, how many wildlife species per group and per family those plants support, filtered to their home state with the GBIF and eBird occurrence data, plus native plant genera that would add the most. Like the app's dashboard, it shows counts, family names and plant names only.

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

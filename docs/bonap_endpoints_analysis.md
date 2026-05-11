# BONAP TDC Endpoint Analysis

## Summary
BONAP (bonap.net) is a Knockout.js-based single-page application for querying North American plant distribution and nativity data. The site does not provide official API documentation, but the following endpoints were identified through reverse engineering.

## Discovered Endpoints

### 1. FieldMaps - Genus List Lookup
**URL:** `POST /FieldMaps/Home/GenusList`

**Parameters:**
- `statecode` (string) - Two-letter state code (e.g., "al", "ak", "az")

**Response:**
- JSON object with `success` boolean and genus list data

**Example JavaScript:**
```javascript
$.post('/FieldMaps/Home/GenusList',
  {'statecode': 'ca'},
  function (result) {
    if (result.success) {
      // result contains genus query data
    }
  })
```

### 2. TDC Map Image API
**URL:** `GET /TDC/Image/Map`

**Query Parameters:**
- `taxonType` - "Species" or "Genus"
- `taxonId` - Numeric taxon identifier
- `locationType` - "County" or "State"
- `mapType` - "Normal" (other types unknown)

**Response:** HTML page with map image and distribution data

**Example URLs:**
```
/TDC/Image/Map?taxonType=Species&taxonId=906&locationType=County&mapType=Normal
/TDC/Image/Map?taxonType=Genus&taxonId=1&locationType=State&mapType=Normal
```

### 3. MapList - Location Management
**Base Path:** `/TDC/MapList/`

**Endpoints:**
- `POST /TDC/MapList/FindLocation` - Lookup location by address/zip
- `POST /TDC/MapList/SaveLocation` - Save a "bubble location"
- `POST /TDC/MapList/UpdateTaxonListAtLocation` - Update plant list for location
- `POST /TDC/MapList/TaxonListForLocation` - Retrieve taxon list for location

**Parameters:** Unknown (endpoints identified from JavaScript, not tested)

### 4. Static Map Gallery
**URL Pattern:** `/MapGallery/{locationType}/{taxonType}/{scientific_name}.png`

**Examples:**
```
/MapGallery/County/Genus/Acer.png
/MapGallery/County/Acer rubrum.png
/MapGallery/County/Acer campestre.png
```

**Notes:**
- Species names are space-separated (not URL-encoded in examples)
- Direct image URLs, no API wrapper

## Query Interface Structure

### Available Filters (from TDC Query page)

**Geographic:**
- State (50 US states + territories)
- County (dependent on state selection)
- Zip code
- Custom "bubble location" names

**Taxonomic:**
- Family
- Genus
- Species
- Include/exclude infraspecific taxa

**Nativity Status:**
- Native
- Exotic
- Adventive

**Biological Attributes:**
- Major plant group (pteridophyte, gymnosperm, flowering plant)
- Habit (tree, shrub, forb, grass, cactus, fern, vine)
- Duration (annual, biennial, perennial)
- Flower color (9 options)
- Flowering season (spring, summer, fall, winter)
- Leaf characteristics (evergreen/deciduous)
- Stem texture (herbaceous, woody, semi-woody)
- Habitat types (alpine, aquatic, wetland, etc.)
- Rarity status (endangered, threatened, rare)
- Human use categories
- Toxicity/allergenicity

**Display Options:**
- Results per page (500, 1000, 1500, 3000, 5500, 10000)
- Show/hide authors
- Show/hide common names

## Data Export

### TSV Download
According to BONAP documentation, the TDC allows downloading query results as TSV (tab-separated values) files containing scientific names only.

**Workflow:**
1. Perform query with desired filters
2. Click "Click Here to Download Displayed Species" link
3. Browser "Save As" dialog opens with TSV file

**Endpoint:** Unknown - likely dynamic based on query state

**Limitations:**
- Only species names exported (no distribution, nativity, or other metadata)
- Requires interactive UI workflow (no direct API access discovered)

## Technical Architecture

### Frontend Framework
- **Knockout.js** for MVVM data binding
- jQuery for AJAX requests
- Template syntax using Mustache/Handlebars (`{{variable}}`)

### JavaScript View Models
Main view model object: `_FieldMapsViewModel`

**Observable Properties:**
- `genusQuery` - Current genus query results
- `selectedStateCode` - Currently selected state
- `selectedGenusId` - Currently selected genus
- `stateQuery` - State query configuration (pageSize, etc.)

**State List:**
Hardcoded object mapping state codes to names:
```javascript
self._stateList = {
  "al": {"stateCode": "al", "stateName": "Alabama"},
  // ... all 50 states + territories
}
self._orderedStateCodes = ["al","ak","az",...,"gl","pm"]
```

## Nativity Status Encoding

### Color Codes (from maps)
**State background:**
- Dark green = Native to North America
- Dark blue = Exotic

**County-level:**
- Light green = Native and common in county
- Dark green = Native to state
- Yellow = Rare occurrence
- Brown = Not present
- Dark olive green = Native
- Orange = Native historic
- Teal = Adventive
- Dark navy blue = Exotic

### Status Values
From USDA/BONAP data:
- "Native" - Occurs naturally in area
- "Introduced" / "Exotic" - Moved from native range by humans
- "Adventive" - Self-introduced (natural dispersal to new area)
- "Native, Introduced" - Native to some regions, introduced to others (e.g., native to L48, introduced to HI)

## Missing Information

### Unknown Endpoints
- **Query submission endpoint** - Main search/query submission handler not identified
- **TSV export endpoint** - Direct download URL structure unknown
- **Taxon ID lookup** - How to map scientific names to taxonId values
- **Nomenclator search API** - Species name search endpoint
- **Authentication endpoints** - If any (login module referenced but endpoints not found)

### Unknown Parameters
- MapList endpoint request/response formats
- TDC query submission POST payload structure
- Pagination mechanisms
- Sort parameters
- Additional mapType values besides "Normal"

## Recommendations for Implementation

### Approach 1: Screen Scraping
1. Use FieldMaps genus list endpoint to get genera for a state
2. Navigate to NAPA genus pages at `/Napa/TaxonMaps/Genus/County/{genus}`
3. Parse HTML to extract species lists
4. Use map gallery URLs to get distribution images
5. Manually interpret color codes from images

**Limitations:** No machine-readable nativity status per state

### Approach 2: TDC Interactive Query
1. Automate browser (Selenium/Puppeteer) to interact with TDC query page
2. Submit queries with state + nativity filters
3. Capture TSV download
4. Parse species lists

**Limitations:** Only get species names, no distribution detail

### Approach 3: Request BONAP Permission
Contact BONAP directly to request:
- Official API access
- Bulk data dump
- Database export with distribution + nativity data

**Contact:** BONAP is run by Dr. John Kartesz at North Carolina Botanical Garden

### Approach 4: Alternative Data Sources
Consider using:
- **USDA Plants Database** - Has RESTful API (ref: USDA Plants Database API in R)
- **Flora API** (floraapi.com) - Commercial API with 30,000+ plants, county distribution
- **GBIF** (Global Biodiversity Information Facility) - Open API with occurrence data

## State Codes Reference

States are referenced using lowercase two-letter codes:
```
al=Alabama, ak=Alaska, az=Arizona, ar=Arkansas, ca=California,
co=Colorado, ct=Connecticut, de=Delaware, fl=Florida, ga=Georgia,
hi=Hawaii, id=Idaho, il=Illinois, in=Indiana, ia=Iowa, ks=Kansas,
ky=Kentucky, la=Louisiana, me=Maine, md=Maryland, ma=Massachusetts,
mi=Michigan, mn=Minnesota, ms=Mississippi, mo=Missouri, mt=Montana,
ne=Nebraska, nv=Nevada, nh=New Hampshire, nj=New Jersey, nm=New Mexico,
ny=New York, nc=North Carolina, nd=North Dakota, oh=Ohio, ok=Oklahoma,
or=Oregon, pa=Pennsylvania, ri=Rhode Island, sc=South Carolina,
sd=South Dakota, tn=Tennessee, tx=Texas, ut=Utah, vt=Vermont,
va=Virginia, wa=Washington, wv=West Virginia, wi=Wisconsin, wy=Wyoming
```

Plus territories: "gl" (Greenland?), "pm" (St. Pierre and Miquelon?)

## Sources

- [BONAP TDC Query Page](https://bonap.net/tdc)
- [BONAP FieldMaps](https://bonap.net/fieldmaps)
- [BONAP Downloading Help](http://www.bonap.org/Help/Downloading.htm)
- [BONAP Nativity Documentation](http://www.bonap.org/Help/Nativity.htm)
- [Is it Really Native? BONAP Tutorial](https://vanessagoold.substack.com/p/is-it-really-native-how-to-use-bonap)
- [BONAP North American Plant Atlas](https://grownativemass.org/Great-Resources/databases/BONAP-North-American-Plant-Atlas)

## Next Steps

To complete the endpoint reverse engineering:
1. Use browser DevTools Network tab to capture actual query submissions
2. Test MapList endpoints with various parameters
3. Identify taxonId-to-species mapping mechanism
4. Capture TSV download request structure
5. Document complete request/response formats for all endpoints

**Note:** BONAP does not provide official API documentation. All endpoints identified through observation of client-side JavaScript and HTML structure. Use responsibly and consider contacting BONAP for permission before automated scraping.

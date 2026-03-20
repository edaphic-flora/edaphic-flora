# Edaphic Flora: User Flow Walkthroughs

## About This Document

This document walks through how different user personas interact with Edaphic Flora, a crowdsourced soil database for plants. Each persona represents a real audience segment with distinct goals, comfort levels, and usage patterns. The app has two experience modes: **Casual** (simplified UI, common names, fewer fields) and **Enthusiast/Pro** (full scientific names, all soil parameters, advanced charts).

### How Authentication Works

Edaphic Flora uses polished (Firebase) authentication. The sign-in screen is the very first thing any visitor sees — there is no public-facing content. Users must create an account (Google or email) before they can access the Welcome page or any other part of the app. This is invite-only during alpha.

### How Preferences Work

There is no onboarding wizard or preferences modal. After signing in, users land on the Welcome page. Zip code is entered via a small input field in the navbar's user dropdown menu. When a valid 5-digit zip is entered, the app geocodes it and saves the user's city, state, and coordinates. The experience mode (Casual/Enthusiast) is toggled via a "Pro" switch, also in the navbar dropdown. Both persist across sessions.

---

## Persona 1: Sarah — Weekend Gardener

**Profile:** 34, suburban Minneapolis. Has a perennial border and a few raised beds. Got a soil test through her county extension office after her hydrangeas kept yellowing. Not a plant scientist — she Googles things like "why are my leaves turning yellow."

**Experience mode:** Casual

### First Visit

1. **Signs in.** Sees the branded sign-in page. Creates an account with Google. (During alpha, she'd need an invite.)
2. **Lands on the Welcome page.** Sees the "What is Edaphic Flora?" explanation and the 4-step visual (Submit Data → Add Location → Analyze → Discover). Reads the "Getting Started" section pointing her to Data Entry.
3. **Navigates to Data Entry tab.** Sees the wizard — Step 1: Soil Source.
4. **Lab test confirmation checkbox.** Reads the note about extension offices offering free testing, checks the box. The entry options un-grey and become clickable.
5. **Uploads her PDF soil report** from the University of Minnesota extension lab. The app extracts pH (6.8), organic matter (4.2%), N/P/K, and texture automatically.
6. **Step 2: Review Data.** In Casual mode, she sees pH, OM, and N/P/K — the advanced fields (CEC, micronutrients) are collapsed under "Advanced Fields." She glances at the numbers, confirms they match her report.
7. **Step 3: Add Plants.** Searches "hydrangea" — the common name search shows "Bigleaf Hydrangea (Hydrangea macrophylla)." Selects it. Enters cultivar "Endless Summer," outcome "Struggling," sun "Part Shade," hydrology "Mesic." Adds a second plant: "Echinacea" → "Purple Coneflower (Echinacea purpurea)," outcome "Thriving."
8. **Enters her zip code in the navbar dropdown** (55419). The app resolves it to Minneapolis, MN, and the location badge appears. Her Data Entry location fields auto-fill from this saved zip on future entries.
9. **Submits.** Sees her entries in the "My Data" table below the form. Common names displayed prominently since she's in Casual mode.

### Return Visits

- **Adds a few more plants** over the season using "Reuse previous soil data" — same bed, same soil, different species.
- **Checks the Analysis tab** for Hydrangea macrophylla. Sees the pH distribution chart and notices her pH (6.8) is on the high end compared to other users' thriving specimens. Sees the USDA reference range overlay. Thinks: "Maybe I need to acidify."
- **Reads the Field Guide** section on soil pH after clicking through from Analysis.
- Never touches the Pro toggle. Never exports data. Might share a screenshot with a gardening friend.

### Key Moments

- The common name search is critical — she doesn't know "Hydrangea macrophylla" off the top of her head.
- PDF upload removes the biggest friction point (manually typing 15 soil values).
- Casual mode hides complexity she doesn't need (CEC, micronutrients, texture percentages).

---

## Persona 2: Marcus — Native Plant Restorationist

**Profile:** 52, works for a land trust in central Ohio. Manages prairie and woodland restoration sites. Has dozens of soil tests from different plots. Deeply knowledgeable about native plants — uses Latin names fluently. Cares about native status, invasive species warnings, and ecoregion context.

**Experience mode:** Enthusiast (Pro toggle ON)

### First Visit

1. **Signs in with email.** Creates an account.
2. **Lands on Welcome page.** Reads it carefully — interested in the data model, wants to know what parameters are tracked and how data is aggregated.
3. **Opens the navbar dropdown, enters zip code** (43015, Delaware, OH). Toggles the Pro switch to Enthusiast mode.
4. **Goes straight to Data Entry.** Has a stack of soil reports from different restoration plots.
5. **Confirms lab test checkbox** — all his data is from the Ohio State extension lab.
6. **Uploads first PDF.** Reviews all fields including CEC, micronutrients, and full texture percentages (sand/silt/clay). Enters texture as percentages, not class.
7. **Adds multiple species per sample.** For a single plot's soil test, he enters 5-6 species: Schizachyrium scoparium (Little Bluestem), Ratibida pinnata (Yellow Coneflower), Silphium terebinthinaceum (Prairie Dock), etc. Records outcomes and sun/hydrology for each.
8. **Notices native status badges** — sees "Native to N. America" on his prairie species. Appreciates the invasive warnings when he experimentally checks a non-native.
9. **Enters coordinates precisely** — he has GPS points for each plot, enters lat/long directly in Step 3 rather than relying on zip code geocoding.
10. **Submits, then starts the next plot** with a different soil test.

### Ongoing Use

- **Batch entry sessions.** Comes back with 3-4 soil reports at a time. Uses the wizard efficiently — upload PDF, review, add species, submit, repeat.
- **Heavy Analysis tab user.** Compares species across his sites. Uses the Performance tab to see which prairie species succeed in his clay-heavy plots vs. sandier ones. Checks the Correlations heatmap.
- **Reads the Caveats page** (Help menu) and appreciates the honesty about sample size limitations and geographic bias. Shares the link with colleagues.
- **Exports his data** via the Data Management tab for inclusion in land trust reports.
- **Checks the Find Plants tab** once enough data accumulates — interested in what species the system recommends for his soil conditions.

### Key Moments

- Pro mode is essential — he needs all soil parameters and Latin name search.
- Multi-species per soil sample is his core workflow (one soil test serves an entire planting).
- Ecoregion auto-detection adds value he can't easily get elsewhere.
- He's a data contributor AND consumer — wants both input and analysis tools.

---

## Persona 3: Diane — Master Gardener & Educator

**Profile:** 61, retired teacher, now a certified Master Gardener in Virginia. Runs a community garden program. Helps beginning gardeners interpret soil tests. Uses the app as a teaching tool — "see how your soil compares to what other people are growing in."

**Experience mode:** Starts on Casual to demo for beginners, switches to Enthusiast for her own work.

### First Visit

1. **Signs in.** Creates an account.
2. **Lands on Welcome page.** Reads it, then explores the Help menu before entering any data. Reads the Field Guide cover-to-cover. Reads the FAQ. Reads the Caveats page. She's evaluating whether to recommend this to her Master Gardener group.
3. **Enters zip code** in the navbar dropdown (22901, Charlottesville, VA).
4. **Goes to Data Entry.** Enters data from her community garden's soil test — a comprehensive report from Virginia Tech's extension lab. Uploads the PDF.
5. **Enters several species** from the community garden: tomatoes (Solanum lycopersicum), peppers (Capsicum annuum), basil (Ocimum basilicum). Records outcomes.
6. **Opens the navbar dropdown and toggles Pro mode** to enter her own garden's data — she has detailed records going back years and wants to use all the fields.

### Ongoing Use

- **Demonstrates the app at Master Gardener meetings.** Shows the Data Entry wizard in Casual mode: "See how easy this is? Upload your report, pick your plants, done."
- **Uses Analysis tab as a teaching tool.** Pulls up a common species like tomato and walks through the pH distribution: "See this range? That's what real gardens show us."
- **Encourages her group to contribute data.** The more local data, the more useful the Analysis tab becomes for their region.
- **References the Field Guide's "How to Get a Soil Test" section** when advising gardeners who haven't tested yet. "Look, Virginia Tech does it for free through extension."
- **Uses the Caveats page** to set appropriate expectations: "This is a starting point, not gospel."

### Key Moments

- She needs the app to be approachable enough for beginners (Casual mode) but powerful enough for her own use (Pro mode).
- The Field Guide is a teaching resource, not just reference material.
- She's a multiplier — if she adopts the tool, 20+ gardeners in her network will follow.
- Trust is paramount. The Caveats page and lab-test requirement signal scientific integrity.

---

## Persona 4: James — Landscape Designer

**Profile:** 41, runs a small residential landscape design firm in Portland, OR. Specifies plants for client properties. Already orders soil tests for every project. Wants to cross-reference his plant selections against what actually thrives in similar soil conditions.

**Experience mode:** Enthusiast

### First Visit

1. **Signs in.** Arrives from a colleague's recommendation. Creates an account.
2. **Lands on Welcome page.** Scans it quickly, immediately grasps the value prop: real-world soil-plant data. Clicks over to Data Entry.
3. **Enters zip code** (97214) and **toggles Pro mode** in the navbar dropdown.
4. **Enters data from a current client project.** Uploads the soil report PDF from a commercial lab (Brookside Laboratories). Reviews extracted values — checks that CEC and micronutrients came through correctly.
5. **Enters the plant palette** he specified for the project: 8-10 species with cultivars. Records the planting as "Established" (just installed) with sun and hydrology for the site.
6. **Immediately goes to Analysis tab** to look up a species he's considering for another project. Searches Cornus sericea (Red Osier Dogwood). Looks at pH range, texture distribution, and outcome patterns.

### Ongoing Use

- **Enters data from completed projects** as follow-up — returns to record which plants are thriving vs. struggling a year later. Edits outcomes via the edit button in My Data.
- **Uses Analysis as a design research tool.** Before specifying a plant for a new project, checks what soil conditions other users report success in. Compares against the client's soil test.
- **Particularly values the Performance tab** — the Success Matrix showing which sun/hydrology combos work best helps him match plants to site conditions.
- **Exports data** via Data Management for client reports: "Here's what the crowdsourced data shows for these species in similar soil types."
- **Eventually discovers Find Plants** once data thresholds are met — enters a client's soil test and gets species recommendations ranked by compatibility.

### Key Moments

- Speed matters. He has 10 species per project — the wizard needs to be efficient.
- Cultivar field is important — he specifies named cultivars, not just species.
- He's both contributing professional data (high quality) and using the tool for business research.
- The lab test requirement aligns with his workflow — he already has lab reports for every project.

---

## Persona 5: Alex — Ecology Graduate Student

**Profile:** 27, PhD student studying plant-soil relationships in tallgrass prairie remnants. Has extensive soil data from field plots. Wants to compare their data against a broader dataset. Thinks in terms of statistical distributions, not individual data points.

**Experience mode:** Enthusiast

### First Visit

1. **Signs in.** Creates an account with university email.
2. **Lands on Welcome page, but immediately navigates to Help > Caveats & Limitations.** Wants to understand methodology, data quality controls, and limitations before investing time. Appreciates the transparency about sample size issues and contributor bias.
3. **Enters university zip code** in the navbar dropdown. Toggles Pro mode.
4. **Goes to Data Entry.** Has CSV files from the university lab — enters data manually or considers the import tool in Data Management for bulk upload.
5. **Enters precise GPS coordinates** for each plot. Enters full soil chemistry including micronutrients. Records all species in each plot with detailed outcomes.
6. **Goes deep on the Analysis tab.** Wants the Correlations heatmap, the texture triangle, and the pH vs. Organic Matter scatter plot. Looks for patterns across species.

### Ongoing Use

- **Heavy analysis user.** Compares their prairie species against the broader database. Are the pH ranges in their remnant prairie typical or unusual?
- **Exports raw data** for analysis in R or Python. Uses the CSV export in Data Management.
- **Contributes high-quality, well-documented data** — precise coordinates, complete soil chemistry, careful outcome assessments.
- **May reference the app in publications** — the Caveats page and data quality measures (lab test requirement, stats gating) matter for academic credibility.
- **Eventually frustrated by small sample sizes** for less common prairie species — but understands this is a crowdsourced data limitation.

### Key Moments

- Data quality and transparency are non-negotiable. The lab test gate and caveats build trust.
- Needs raw data export — the visualizations are useful but they also want to run their own analyses.
- Could become a power contributor if the tool proves useful for their research.

---

## Persona 6: Pat — Curious Homeowner (One-Time User)

**Profile:** 45, just bought a house with an overgrown yard. Got a soil test because the extension office was doing a free testing event. Has no idea what the numbers mean. Found Edaphic Flora through a search like "what do soil test results mean."

**Experience mode:** Casual

### First (Possibly Only) Visit

1. **Hits the sign-in page.** Hesitates — do I really want to create an account? Decides to try Google sign-in since it's one click. (During alpha, would need an invite — this is a potential drop-off point.)
2. **Lands on Welcome page.** Reads the "What is Edaphic Flora?" section. Thinks: "Okay, I can enter my soil test and learn something." Clicks over to Data Entry.
3. **Sees the lab test confirmation checkbox.** They did get a real lab test (extension event), so checks it. Entry options appear.
4. **Uploads their soil report.** The PDF extraction fills in all the values. Relieved they don't have to figure out which numbers go where.
5. **Step 2: Review Data.** In Casual mode, sees the simplified view. The numbers are pre-filled. Clicks Next.
6. **Step 3: Add Plants.** Searches for plants in their yard. Doesn't know species — searches "maple" and finds options. Adds a few with rough outcomes.
7. **Submits, then immediately goes to Analysis.** Looks at their maple species. Sees the pH chart. Maybe clicks through to the Field Guide to understand what pH means.
8. **Browses the Field Guide's "How to Get a Soil Test" section** — useful for understanding what their report actually measured, even after the fact.
9. **May or may not return.** If the analysis was useful and easy to understand, might come back when they plant something new.

### Key Moments

- The sign-in requirement is the first hurdle — Pat almost didn't bother. A future public Welcome page could help here.
- The Field Guide is doing double duty: explaining soil science basics AND the app's data model.
- Casual mode and common name search are the difference between "I can use this" and closing the tab.
- PDF upload is essential — without it, this user would abandon at data entry.
- This persona tests whether the app is accessible to non-experts. If Pat can use it, the UX works.

---

## Cross-Persona Flow Summary

| Step | Sarah (Casual) | Marcus (Pro) | Diane (Educator) | James (Designer) | Alex (Researcher) | Pat (One-Timer) |
|------|----------------|--------------|-------------------|-------------------|--------------------|-----------------|
| **Sign-in** | Google, quick | Email | Email | Google, quick | Email (university) | Google, hesitant |
| **First action after Welcome** | Data Entry | Read Welcome, set Pro | Read Help section | Data Entry | Read Caveats | Data Entry |
| **Zip code entry** | After first submit | During setup | During setup | During setup | During setup | Maybe never |
| **Data Entry** | PDF upload, 1-2 species | PDF upload, 5-6 species | PDF upload, demo + real use | PDF upload, 8-10 species | Manual/import, many species | PDF upload, 2-3 species |
| **Search style** | Common name | Latin name | Both (switching modes) | Latin name | Latin name | Common name |
| **Experience mode** | Casual | Enthusiast | Both | Enthusiast | Enthusiast | Casual |
| **Key fields** | pH, OM, N/P/K | All fields + texture % | All fields | All fields + cultivar | All fields + coordinates | pH, OM, N/P/K |
| **Analysis use** | Occasional, specific species | Heavy, comparative | Teaching tool | Design research | Statistical deep-dive | One-time curiosity |
| **Help section use** | Field Guide as reference | Skimmed | Teaching resource | Skimmed | Caveats first | Field Guide as 101 |
| **Return frequency** | Monthly (seasonal) | Weekly (batch sessions) | Weekly (teaching + personal) | Per-project | Weekly (research) | Possibly never |
| **Data quality** | Good (lab test, casual detail) | Excellent (full detail) | Very good | Excellent (professional) | Excellent (research-grade) | Variable |

---

## UX Implications

1. **The sign-in wall is the first test.** Every persona must create an account before seeing anything. For power users (Marcus, Diane, James, Alex) this is fine — they're committed. For casual users (Sarah, Pat), it's a friction point. A future public-facing Welcome page or limited read-only access could lower the barrier.

2. **PDF upload is the single most important feature for adoption.** Every persona uses it. Without it, manual data entry is a wall — especially for casual users.

3. **Common name search is non-negotiable for casual users.** Sarah searches "hydrangea," not "Hydrangea macrophylla." The search must surface common names prominently.

4. **The Pro toggle must be discoverable but not disruptive.** Marcus and James need it immediately; Sarah should never feel like she's missing something by not using it. Currently it lives in the navbar user dropdown — power users find it, casual users don't stumble into complexity.

5. **Zip code entry is easy to miss.** It's a small field inside the navbar dropdown — not part of any onboarding flow. Some users (like Pat) may never find it. A gentle first-visit prompt or tooltip could help without adding a modal.

6. **The lab test confirmation gate adds trust without adding friction.** Every persona either already has a lab test or got one for free through extension. The checkbox takes one second but signals data quality to researchers and educators.

7. **The Field Guide serves three audiences:** reference (Sarah), teaching (Diane), and methodology (Alex). It needs to work for all three without being overwhelming.

8. **Multi-species entry per soil sample is a core workflow** for power users (Marcus, James). The wizard must handle 5-10 species efficiently without feeling tedious.

9. **Data export matters for professional users.** James needs it for client reports; Alex needs it for statistical analysis. This isn't a nice-to-have.

10. **The Caveats page builds institutional trust.** Diane won't recommend the tool to her Master Gardener group, and Alex won't cite it in papers, without honest disclosure of limitations.

11. **Return usage depends on the Analysis tab.** Data entry gets people in the door; analysis brings them back. The visualizations need to reward curiosity.

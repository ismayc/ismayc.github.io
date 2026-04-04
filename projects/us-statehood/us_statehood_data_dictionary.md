# US Statehood Data Dictionary

## File: us_statehood_data.csv

A complete dataset of all 50 US states and the data used in the "Growth of a Nation" interactive statehood map. Each row represents one state. Sorted by admission order.

---

## Column Definitions

### admission_order
- **Type:** Integer (1-50)
- **Description:** The chronological order in which the state was admitted to the Union. Delaware is 1, Hawaii is 50.

### state_name
- **Type:** Text
- **Description:** The full official name of the state (e.g., "New Hampshire", "West Virginia").

### abbreviation
- **Type:** Text (2 characters)
- **Description:** The standard US postal abbreviation for the state (e.g., "NH", "WV").

### fips_code
- **Type:** Text (2 characters, zero-padded)
- **Description:** The Federal Information Processing Standards code used to identify the state in geographic data. This is the key used to match states to their boundary shapes in the US Census Bureau's TopoJSON map files. Examples: "01" = Alabama, "06" = California, "51" = Virginia.

### admission_date
- **Type:** Text (date as "Month Day, Year")
- **Description:** The full date the state was admitted to the Union or ratified the Constitution. For the original 13 colonies, this is the date they ratified the US Constitution.

### admission_year
- **Type:** Integer (1787-1959)
- **Description:** The year extracted from admission_date. Used for timeline positioning and year-gap calculations in the animation.

### land_acquisition
- **Type:** Text
- **Description:** The name of the land acquisition or treaty through which the United States gained sovereignty over the land that became this state. Values include:
  - **Original Territory** - Land held by the US at the time of the Constitution (1787), including the original 13 colonies, the Northwest Territory, and other early claims
  - **Louisiana Purchase** - Acquired from France in 1803
  - **Adams-Onis Treaty** - Florida, acquired from Spain in 1819
  - **Republic of Texas** - Annexed from the independent Republic of Texas in 1845
  - **Oregon Country** - Pacific Northwest, acquired via treaty with Britain in 1846
  - **Mexican Cession** - Acquired from Mexico via the Treaty of Guadalupe Hidalgo in 1848
  - **Alaska Purchase** - Purchased from Russia in 1867
  - **Hawaii Annexation** - Annexed in 1898

### acquisition_year
- **Type:** Integer
- **Description:** The year the US acquired sovereignty over this land. This determines when the state's outline first appears (as faint, unorganized land) on the animated map.

### territory_name
- **Type:** Text
- **Description:** The name of the organized territory (or governing entity) the land belonged to before statehood. For the original colonies, this is "Original Colony." For states carved from existing states, this is "Part of [Parent State]" (e.g., "Part of Virginia" for Kentucky and West Virginia). Other examples include "Northwest Territory", "Orleans Territory", "Dakota Territory", etc.

### territory_organized_year
- **Type:** Integer or blank
- **Description:** The year the territory was formally organized by Congress. Blank for original colonies that went directly to statehood. For states that were part of a parent state (like Kentucky being part of Virginia), this is the year the parent state was organized or admitted. This determines when the state's outline transitions from "acquired/unorganized" to "territory" styling on the map.

### years_as_territory
- **Type:** Integer or blank
- **Description:** Computed field: admission_year minus territory_organized_year. Represents how many years the land existed as an organized territory before achieving statehood. Blank when territory_organized_year is blank. Notable values: New Mexico had 62 years as a territory (the longest), while California had 2 years (it skipped formal territorial status due to the Gold Rush).

### parent_state
- **Type:** Text or blank
- **Description:** For states that were carved out of an existing state, this is the name of the parent state. Used in the map to show these areas as solid-colored parts of their parent state before separation. Only three states have parent values:
  - Kentucky and West Virginia were part of **Virginia**
  - Maine was part of **Massachusetts**

### era
- **Type:** Text
- **Description:** A grouping of states by historical period, used for color-coding on the map. Each era has a distinct color. Values:
  - **Original Thirteen** (orders 1-13) - The colonies that ratified the Constitution
  - **Early Republic** (orders 14-17) - Vermont through Ohio, 1791-1803
  - **Expansion Era** (orders 18-28) - Louisiana through Texas, 1812-1845
  - **Pre-Civil War** (orders 29-34) - Iowa through Kansas, 1846-1861
  - **Civil War & Gilded Age** (orders 35-38) - West Virginia through Colorado, 1863-1876
  - **Western Settlement** (orders 39-45) - North Dakota through Utah, 1889-1896
  - **Early 20th Century** (orders 46-48) - Oklahoma through Arizona, 1907-1912
  - **Modern Additions** (orders 49-50) - Alaska and Hawaii, 1959

### notes
- **Type:** Text
- **Description:** A brief historical note about the state's path to statehood. Used in the info panel when a user hovers over or taps a state on the map.

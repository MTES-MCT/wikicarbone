# Changelog


## [4.0.0](https://github.com/MTES-MCT/ecobalyse/compare/v3.1.0..v4.0.0) (2025-02-19)



### 🚀 Features

- Add pre-treatments and update bleaching process ([#898](https://github.com/MTES-MCT/ecobalyse/issues/898))
- *(textile,ui)* Apply default trims on product category change ([#910](https://github.com/MTES-MCT/ecobalyse/issues/910))
- Add link to docs in trims section. ([#911](https://github.com/MTES-MCT/ecobalyse/issues/911))
- Update finishing ([#906](https://github.com/MTES-MCT/ecobalyse/issues/906))
- *(textile)* Add pre-treatments at the ennobling step. ([#916](https://github.com/MTES-MCT/ecobalyse/issues/916))
- Update aquatic pollution and pre-treatments computations ([#928](https://github.com/MTES-MCT/ecobalyse/issues/928))
- [**breaking**] Replace dyeing medium parameter with dyeing process type. ([#941](https://github.com/MTES-MCT/ecobalyse/issues/941))

### 🪲 Bug Fixes

- Include trims impacts to score without durability. ([#912](https://github.com/MTES-MCT/ecobalyse/issues/912))
- *(security)* Upgrade sentry libs to v8.49.0 ([#918](https://github.com/MTES-MCT/ecobalyse/issues/918))

### 📚 Documentation

- Add FAQ entry about security & self-hosting. ([#919](https://github.com/MTES-MCT/ecobalyse/issues/919))

### ⚙️ Miscellaneous Tasks

- *(data)* Update fast fashion examples nb of references. ([#908](https://github.com/MTES-MCT/ecobalyse/issues/908))
- For bleaching set etf to 0 ([#914](https://github.com/MTES-MCT/ecobalyse/issues/914))
- Check ecobalyse-data sync for PR ([#915](https://github.com/MTES-MCT/ecobalyse/issues/915))
- Sync ecobalyse-data after bw update ([#920](https://github.com/MTES-MCT/ecobalyse/issues/920))
- Doubts on the lamb, hide it for now ([#927](https://github.com/MTES-MCT/ecobalyse/issues/927))
- Upgrade dependencies, 2025, Feb 12. ([#938](https://github.com/MTES-MCT/ecobalyse/issues/938))
- *(textile)* Remove obsolete waste for material ([#940](https://github.com/MTES-MCT/ecobalyse/issues/940))
- Use new deployment stack `scalingo-22` ([#939](https://github.com/MTES-MCT/ecobalyse/issues/939))
- WFLDB export from simapro ([#942](https://github.com/MTES-MCT/ecobalyse/issues/942))


## [3.1.0](https://github.com/MTES-MCT/ecobalyse/compare/v3.0.0..v3.1.0) (2025-01-23)



### 🚀 Features

- Show heat, elec, waste, density in process explorer. ([#901](https://github.com/MTES-MCT/ecobalyse/issues/901))
- Handle component process transforms ([#897](https://github.com/MTES-MCT/ecobalyse/issues/897))
- Render component transforms ([#907](https://github.com/MTES-MCT/ecobalyse/issues/907))

### 🪲 Bug Fixes

- Avoid empty process aliases. ([#899](https://github.com/MTES-MCT/ecobalyse/issues/899))
- *(security)* Upgrade django to 5.1.5. ([#900](https://github.com/MTES-MCT/ecobalyse/issues/900))
- Hide link to food from the homepage. ([#904](https://github.com/MTES-MCT/ecobalyse/issues/904))
- Restrict selectable components to scoped ones. ([#905](https://github.com/MTES-MCT/ecobalyse/issues/905))

### 🚜 Refactor

- Improve YAML gh action legibility. ([#896](https://github.com/MTES-MCT/ecobalyse/issues/896))
- Merge component and process dbs in-memory. ([#903](https://github.com/MTES-MCT/ecobalyse/issues/903))

### ⚙️ Miscellaneous Tasks

- Add python build libs to .gitignore. ([#895](https://github.com/MTES-MCT/ecobalyse/issues/895))
- Update data files ([#889](https://github.com/MTES-MCT/ecobalyse/issues/889))
- Rely on process UUID instead of alias in code. ([#902](https://github.com/MTES-MCT/ecobalyse/issues/902))


## [3.0.0](https://github.com/MTES-MCT/ecobalyse/compare/v2.7.0..v3.0.0) (2025-01-13)



### 🚀 Features

- Generalize density, electricity, heat and waste process fields ([#855](https://github.com/MTES-MCT/ecobalyse/issues/855))
- *(data)* Ensure consistent nullable alias field in all processes files. ([#862](https://github.com/MTES-MCT/ecobalyse/issues/862))
- Add betagouv logo. ([#848](https://github.com/MTES-MCT/ecobalyse/issues/848))
- *(data)* Unified, cross-domain processes file format. ([#866](https://github.com/MTES-MCT/ecobalyse/issues/866))
- *(data)* Validate processes files against a JSON schema. ([#869](https://github.com/MTES-MCT/ecobalyse/issues/869))
- *(data,textile)* Add trim process and components data. ([#824](https://github.com/MTES-MCT/ecobalyse/issues/824))
- *(textile)* Implement trims. ([#873](https://github.com/MTES-MCT/ecobalyse/issues/873))
- *(data,ui)* Add trims to more textile examples, render them in explorer ([#876](https://github.com/MTES-MCT/ecobalyse/issues/876))
- Allow expanding trim details. ([#877](https://github.com/MTES-MCT/ecobalyse/issues/877))
- Allow staff to access detailed impacts from explorer. ([#878](https://github.com/MTES-MCT/ecobalyse/issues/878))

### 🪲 Bug Fixes

- *(food)* [**breaking**] Food processes identifiers are now UUIDs ([#844](https://github.com/MTES-MCT/ecobalyse/issues/844))
- *(data)* [**breaking**] Update textile process ids to use UUID format ([#858](https://github.com/MTES-MCT/ecobalyse/issues/858))
- Data pipeline with new UUIDs ([#857](https://github.com/MTES-MCT/ecobalyse/issues/857))
- Fix api error with old versions ([#851](https://github.com/MTES-MCT/ecobalyse/issues/851))
- Broken homepage after upgrading highcharts ([#863](https://github.com/MTES-MCT/ecobalyse/issues/863))
- *(dev)* Fix npm ci error with `transcrypt` ([#870](https://github.com/MTES-MCT/ecobalyse/issues/870))
- Correct data on trims ([#879](https://github.com/MTES-MCT/ecobalyse/issues/879))
- Warn on session data decoding error. ([#884](https://github.com/MTES-MCT/ecobalyse/issues/884))
- *(textile)* Apply durability to trims impacts. ([#886](https://github.com/MTES-MCT/ecobalyse/issues/886))
- Update PEF score label. ([#887](https://github.com/MTES-MCT/ecobalyse/issues/887))
- Add missing env and allow workflow dispatch for release creation ([#892](https://github.com/MTES-MCT/ecobalyse/issues/892))

### 🚜 Refactor

- Move textile step_usage field to categories. ([#850](https://github.com/MTES-MCT/ecobalyse/issues/850))
- *(data)* Move textile process "correctif" to comment ([#852](https://github.com/MTES-MCT/ecobalyse/issues/852))
- Add encrypted detailed impacts files to the source code ([#840](https://github.com/MTES-MCT/ecobalyse/issues/840))
- Abstract components. ([#872](https://github.com/MTES-MCT/ecobalyse/issues/872))
- Order json keys ([#871](https://github.com/MTES-MCT/ecobalyse/issues/871))

### 📚 Documentation

- Fix openapi food examples ([#867](https://github.com/MTES-MCT/ecobalyse/issues/867))

### ⚙️ Miscellaneous Tasks

- Increase API test timeout ([#853](https://github.com/MTES-MCT/ecobalyse/issues/853))
- *(data)* Remove system_description process field. ([#859](https://github.com/MTES-MCT/ecobalyse/issues/859))
- Upgrade dependencies, December 2024. ([#860](https://github.com/MTES-MCT/ecobalyse/issues/860))
- Remove obsolete/unused info textile process field. ([#861](https://github.com/MTES-MCT/ecobalyse/issues/861))
- *(data)* Merge PastoEco in a single file to speedup imports and fixed linking to AGB ([#833](https://github.com/MTES-MCT/ecobalyse/issues/833))
- Fix score_history workflow for transcrypt ([#864](https://github.com/MTES-MCT/ecobalyse/issues/864))
- Standardize number formatting across codebase ([#804](https://github.com/MTES-MCT/ecobalyse/issues/804))
- Standardize tkm unit ([#868](https://github.com/MTES-MCT/ecobalyse/issues/868))
- Remove obsolete pre-commit command. ([#874](https://github.com/MTES-MCT/ecobalyse/issues/874))
- Update trim api parameter ordering. ([#875](https://github.com/MTES-MCT/ecobalyse/issues/875))
- Remove data directory, now in `ecobalyse-data` repo ([#888](https://github.com/MTES-MCT/ecobalyse/issues/888))
- Update crypto-related docs. ([#890](https://github.com/MTES-MCT/ecobalyse/issues/890))
- *(security)* Upgrade django to >=5.1.4. ([#885](https://github.com/MTES-MCT/ecobalyse/issues/885))
- Readd score_history ([#891](https://github.com/MTES-MCT/ecobalyse/issues/891))


## [2.7.0](https://github.com/MTES-MCT/ecobalyse/compare/v2.6.0..v2.7.0) (2024-12-05)



### 🚀 Features

- *(objects)* Introduce components for objects and veli ([#825](https://github.com/MTES-MCT/ecobalyse/issues/825))
- *(textile)* Update number of references index thresholds. ([#839](https://github.com/MTES-MCT/ecobalyse/issues/839))
- Introduce objects/veli components db and explorer ([#841](https://github.com/MTES-MCT/ecobalyse/issues/841))

### 🚜 Refactor

- Move weaving elec_pppm to textile wellknown. ([#843](https://github.com/MTES-MCT/ecobalyse/issues/843))

### ⚙️ Miscellaneous Tasks

- *(data)* New ingredients ([#814](https://github.com/MTES-MCT/ecobalyse/issues/814))
- *(data)* Add irrigation to the Ecoinvent organic cotton ([#832](https://github.com/MTES-MCT/ecobalyse/issues/832))


## [2.6.0](https://github.com/MTES-MCT/ecobalyse/compare/v2.5.0..v2.6.0) (2024-11-20)



### 🚀 Features

- Add API FAQ page. ([#829](https://github.com/MTES-MCT/ecobalyse/issues/829))
- Intégration Laine woolmark ([#831](https://github.com/MTES-MCT/ecobalyse/issues/831))

### ⚙️ Miscellaneous Tasks

- Upgrade dependencies, Nov. 2024. ([#830](https://github.com/MTES-MCT/ecobalyse/issues/830))
- *(data)* Fixed typo paysane→paysanne ([#836](https://github.com/MTES-MCT/ecobalyse/issues/836))


## [2.5.0](https://github.com/MTES-MCT/ecobalyse/compare/v2.4.0..v2.5.0) (2024-11-07)



### 🚀 Features

- Add bookmarks for objects ([#781](https://github.com/MTES-MCT/ecobalyse/issues/781))
- Add object explorer pages. ([#803](https://github.com/MTES-MCT/ecobalyse/issues/803))
- Distinguish Objects from Veli. ([#813](https://github.com/MTES-MCT/ecobalyse/issues/813))
- Display score without durability ([#815](https://github.com/MTES-MCT/ecobalyse/issues/815))
- Textile export ([#808](https://github.com/MTES-MCT/ecobalyse/issues/808))
- Object export ([#812](https://github.com/MTES-MCT/ecobalyse/issues/812))

### 🪲 Bug Fixes

- Create object encrypted file for versions ([#800](https://github.com/MTES-MCT/ecobalyse/issues/800))
- Improve object simulator. ([#799](https://github.com/MTES-MCT/ecobalyse/issues/799))
- Fix encoded display name field. ([#820](https://github.com/MTES-MCT/ecobalyse/issues/820))

### 🚜 Refactor

- Aggregate in python ([#794](https://github.com/MTES-MCT/ecobalyse/issues/794))
- Turn food process category into a list ([#795](https://github.com/MTES-MCT/ecobalyse/issues/795))
- Aggregate in python ([#807](https://github.com/MTES-MCT/ecobalyse/issues/807))

### ⚙️ Miscellaneous Tasks

- Upgrade dependencies to their latest version, Oct. 2024. ([#801](https://github.com/MTES-MCT/ecobalyse/issues/801))
- Add tolerance to tests comparison ([#810](https://github.com/MTES-MCT/ecobalyse/issues/810))
- *(data)* New export ([#819](https://github.com/MTES-MCT/ecobalyse/issues/819))

### ◀️ Revert

- "refactor: aggregate in python" ([#806](https://github.com/MTES-MCT/ecobalyse/issues/806))


## [2.4.0](https://github.com/MTES-MCT/ecobalyse/compare/v2.3.0..v2.4.0) (2024-10-10)



### 🚀 Features

- Introduce first version of object interface ([#756](https://github.com/MTES-MCT/ecobalyse/issues/756))

### 🪲 Bug Fixes

- Sync food ([#759](https://github.com/MTES-MCT/ecobalyse/issues/759))
- Don't hide version information on staging ([#778](https://github.com/MTES-MCT/ecobalyse/issues/778))
- Reset physical durablility in regulatory mode ([#786](https://github.com/MTES-MCT/ecobalyse/issues/786))
- *(api,food)* Nullable fields weren't nullable anymore. ([#789](https://github.com/MTES-MCT/ecobalyse/issues/789))

### 🚜 Refactor

- Small textile explorer improvements ([#773](https://github.com/MTES-MCT/ecobalyse/issues/773))

### ⚙️ Miscellaneous Tasks

- Don't download draft releases ([#771](https://github.com/MTES-MCT/ecobalyse/issues/771))
- Remove `airTransportRatio` from examples ([#785](https://github.com/MTES-MCT/ecobalyse/issues/785))
- Cleanup package-lock.json. ([#787](https://github.com/MTES-MCT/ecobalyse/issues/787))
- Use builtin python action cache for pipenv ([#796](https://github.com/MTES-MCT/ecobalyse/issues/796))
- Improve changelog by using `git-cliff` ([#768](https://github.com/MTES-MCT/ecobalyse/issues/768))


## [2.3.0](https://github.com/MTES-MCT/ecobalyse/compare/v2.2.0..v2.3.0) (2024-09-25)



### 🚀 Features

- DisplayName in the textile explorer, reordered columns ([#737](https://github.com/MTES-MCT/ecobalyse/issues/737))
- Add link to changelog in app footer. ([#748](https://github.com/MTES-MCT/ecobalyse/issues/748))
- AirTransportRatio should depend on durability ([#757](https://github.com/MTES-MCT/ecobalyse/issues/757))

### 🪲 Bug Fixes

- Encode physicalDurability parameter. ([#751](https://github.com/MTES-MCT/ecobalyse/issues/751))
- Check db integrity after building it ([#753](https://github.com/MTES-MCT/ecobalyse/issues/753))
- Fix github CI python build setup. ([#762](https://github.com/MTES-MCT/ecobalyse/issues/762))
- Stricter validation of POST json body passed to the textile API. ([#760](https://github.com/MTES-MCT/ecobalyse/issues/760))
- *(textile)* Distribution step had no inland road transports added. ([#761](https://github.com/MTES-MCT/ecobalyse/issues/761))
- Decode and validate all optionals. ([#764](https://github.com/MTES-MCT/ecobalyse/issues/764))
- Check uniqueness of JSON db primary keys at build time. ([#766](https://github.com/MTES-MCT/ecobalyse/issues/766))
- Update export outside of EU probability. ([#765](https://github.com/MTES-MCT/ecobalyse/issues/765))
- *(api)* Handle ingredient plane transport in food POST api. ([#769](https://github.com/MTES-MCT/ecobalyse/issues/769))

### 🚜 Refactor

- Removed duplicate identifier column in the food explorer ([#738](https://github.com/MTES-MCT/ecobalyse/issues/738))
- Sort most record properties and constructors. ([#736](https://github.com/MTES-MCT/ecobalyse/issues/736))
- Remove obsolete gitbook markdown parsing code. ([#744](https://github.com/MTES-MCT/ecobalyse/issues/744))
- Express all percentages as splits. ([#770](https://github.com/MTES-MCT/ecobalyse/issues/770))

### ⚙️ Miscellaneous Tasks

- *(data)* Removed recycled viscose (unsafe) ([#750](https://github.com/MTES-MCT/ecobalyse/issues/750))
- Upgrade deps (2024-09-12) ([#746](https://github.com/MTES-MCT/ecobalyse/issues/746))
- Render food api docs conditionally from env. ([#755](https://github.com/MTES-MCT/ecobalyse/issues/755))
- Filter sentry errors by env ([#752](https://github.com/MTES-MCT/ecobalyse/issues/752))
- Add PR template ([#767](https://github.com/MTES-MCT/ecobalyse/issues/767))
- Change waste ratio to input mass ([#711](https://github.com/MTES-MCT/ecobalyse/issues/711))


## [2.2.0](https://github.com/MTES-MCT/ecobalyse/compare/v2.1.1..v2.2.0) (2024-09-12)



### 🚀 Features

- Add app version to openapi docs in the root endpoint. ([#726](https://github.com/MTES-MCT/ecobalyse/issues/726))
- Render app version details in the changelog. ([#725](https://github.com/MTES-MCT/ecobalyse/issues/725))
- Add holistic durability in exploratory mode ([#721](https://github.com/MTES-MCT/ecobalyse/issues/721))

### 🪲 Bug Fixes

- Use fabric processes to compute fabric waste ([#712](https://github.com/MTES-MCT/ecobalyse/issues/712))
- Don't add disabled step impacts to lifecycle totals. ([#719](https://github.com/MTES-MCT/ecobalyse/issues/719))
- Accept custom making complexity for upcycled garments. ([#723](https://github.com/MTES-MCT/ecobalyse/issues/723))
- Make scalingo not segfaulting. ([#728](https://github.com/MTES-MCT/ecobalyse/issues/728))
- Ensure express app is properly monitored by Sentry. ([#729](https://github.com/MTES-MCT/ecobalyse/issues/729))
- *(ci)* Check for ecobalyse-private when extracting the branch name ([#733](https://github.com/MTES-MCT/ecobalyse/issues/733))

### ⚙️ Miscellaneous Tasks

- Upgrade dependencies to their latest stable versions. ([#714](https://github.com/MTES-MCT/ecobalyse/issues/714))
- Security upgrades 2024-09-05 ([#730](https://github.com/MTES-MCT/ecobalyse/issues/730))
- *(data)* New impacts recycled cotton ([#718](https://github.com/MTES-MCT/ecobalyse/issues/718))
- *(data)* Ingredients ([#676](https://github.com/MTES-MCT/ecobalyse/issues/676))
- Optimize scalingo build ([#734](https://github.com/MTES-MCT/ecobalyse/issues/734))


## [2.1.1](https://github.com/MTES-MCT/ecobalyse/compare/v2.1.0..v2.1.1) (2024-09-02)



### 🪲 Bug Fixes

- *(ui)* Hide unreleased entry in production version selector. ([#715](https://github.com/MTES-MCT/ecobalyse/issues/715))


## [2.1.0](https://github.com/MTES-MCT/ecobalyse/compare/v2.0.0..v2.1.0) (2024-09-02)



### 🚀 Features

- Serve multiple app versions ([#627](https://github.com/MTES-MCT/ecobalyse/issues/627))
- Add a button to access material/ingredient details ([#703](https://github.com/MTES-MCT/ecobalyse/issues/703))
- Add a version selector widget. ([#700](https://github.com/MTES-MCT/ecobalyse/issues/700))
- Allow downloading explorer data as CSV. ([#705](https://github.com/MTES-MCT/ecobalyse/issues/705))
- Version selector redirect to current location hash. ([#709](https://github.com/MTES-MCT/ecobalyse/issues/709))
- *(api,textile)* Make all country params optional. ([#713](https://github.com/MTES-MCT/ecobalyse/issues/713))
- Implement upcycling. ([#710](https://github.com/MTES-MCT/ecobalyse/issues/710))

### 🪲 Bug Fixes

- Add missing run command for score history ([#697](https://github.com/MTES-MCT/ecobalyse/issues/697))
- Avoid scrolling to top when using the explorer. ([#702](https://github.com/MTES-MCT/ecobalyse/issues/702))
- Add github token to worklows ([#704](https://github.com/MTES-MCT/ecobalyse/issues/704))
- Broken comparator charts on desynced cache data. ([#706](https://github.com/MTES-MCT/ecobalyse/issues/706))

### 🚜 Refactor

- Use python instead of bash to get data branch ([#685](https://github.com/MTES-MCT/ecobalyse/issues/685))
- Use python to patch files ([#701](https://github.com/MTES-MCT/ecobalyse/issues/701))

### ⚙️ Miscellaneous Tasks

- Serve version file with relative path ([#693](https://github.com/MTES-MCT/ecobalyse/issues/693))
- Upgrade Django to >=5.0.8. ([#695](https://github.com/MTES-MCT/ecobalyse/issues/695))


## [2.0.0](https://github.com/MTES-MCT/ecobalyse/compare/v1.3.2..v2.0.0) (2024-07-30)



### 🚀 Features

- *(textile,api,ui,data)* [**breaking**] Update durability index computation ([#673](https://github.com/MTES-MCT/ecobalyse/issues/673))

### 🪲 Bug Fixes

- Update pull examples data ([#690](https://github.com/MTES-MCT/ecobalyse/issues/690))
- *(food)* Correct default origin for ingredients ([#683](https://github.com/MTES-MCT/ecobalyse/issues/683))

### 📚 Documentation

- Update readme ecobalyse private ([#687](https://github.com/MTES-MCT/ecobalyse/issues/687))

### ⚙️ Miscellaneous Tasks

- *(data,food)* Added Organic tomatos ([#665](https://github.com/MTES-MCT/ecobalyse/issues/665))
- *(ui)* Rename learning tab to exploratory tab. ([#684](https://github.com/MTES-MCT/ecobalyse/issues/684))
- Keep track of data-dir hash ([#686](https://github.com/MTES-MCT/ecobalyse/issues/686))
- Rename business services ([#692](https://github.com/MTES-MCT/ecobalyse/issues/692))


## [1.3.2](https://github.com/MTES-MCT/ecobalyse/compare/v1.3.1..v1.3.2) (2024-07-23)



### 🪲 Bug Fixes

- Data-dir is needed before build ([#681](https://github.com/MTES-MCT/ecobalyse/issues/681))


## [1.3.1](https://github.com/MTES-MCT/ecobalyse/compare/v1.3.0..v1.3.1) (2024-07-23)



### 🪲 Bug Fixes

- Add ecobalyse-private to release please ([#679](https://github.com/MTES-MCT/ecobalyse/issues/679))


## [1.3.0](https://github.com/MTES-MCT/ecobalyse/compare/v1.2.0..v1.3.0) (2024-07-23)



### 🚀 Features

- *(food,ui)* Render agribalyse process name in ingredient selector. ([#659](https://github.com/MTES-MCT/ecobalyse/issues/659))
- Display current version in the footer ([#677](https://github.com/MTES-MCT/ecobalyse/issues/677))

### 🪲 Bug Fixes

- *(api)* Fix invalid openapi format ([#666](https://github.com/MTES-MCT/ecobalyse/issues/666))
- *(api)* Fix material shares sum rounding precision error. ([#670](https://github.com/MTES-MCT/ecobalyse/issues/670))
- Add missing condition in `release-please` workflow ([#671](https://github.com/MTES-MCT/ecobalyse/issues/671))

### 🚜 Refactor

- Remove `processes_impacts` from public repo ([#658](https://github.com/MTES-MCT/ecobalyse/issues/658))

### ⚙️ Miscellaneous Tasks

- Updated brightway, fixed some versions for jupyter, fixed the random issue with ground-beef-organic ([#628](https://github.com/MTES-MCT/ecobalyse/issues/628))
- Remove obsolete adjustable ecotox weighting feat. ([#663](https://github.com/MTES-MCT/ecobalyse/issues/663))
- *(ui)* Rename advanced tab to learning tab. ([#667](https://github.com/MTES-MCT/ecobalyse/issues/667))
- *(data,food)* Mask some ingredients for now ([#669](https://github.com/MTES-MCT/ecobalyse/issues/669))
- Provide a way to synchronise a branch of `ecobalyse-private` with a PR ([#672](https://github.com/MTES-MCT/ecobalyse/issues/672))


## [1.2.0](https://github.com/MTES-MCT/ecobalyse/compare/v1.1.1..v1.2.0) (2024-07-10)



### 🚀 Features

- Add a link to the new product category gform. ([#626](https://github.com/MTES-MCT/ecobalyse/issues/626))
- Clarify ingredients names (bio, conv) and origin (France, EU, Hors-EU) ([#653](https://github.com/MTES-MCT/ecobalyse/issues/653))


## [1.1.1](https://github.com/MTES-MCT/ecobalyse/compare/v1.1.0..v1.1.1) (2024-07-10)



### 🪲 Bug Fixes

- *(ui)* Round Dtex number in the web UI. ([#649](https://github.com/MTES-MCT/ecobalyse/issues/649))
- *(api,ui)* Use custom waste/complexity and product defaults. ([#648](https://github.com/MTES-MCT/ecobalyse/issues/648))
- Rename organic cotton. ([#647](https://github.com/MTES-MCT/ecobalyse/issues/647))
- Avoid building detailed impacts in production. ([#656](https://github.com/MTES-MCT/ecobalyse/issues/656))

### 🚜 Refactor

- Don't use Django anymore to serve the files ([#646](https://github.com/MTES-MCT/ecobalyse/issues/646))

### 🎨 Styling

- Fixed ruff warnings ([#644](https://github.com/MTES-MCT/ecobalyse/issues/644))

### 🧪 Testing

- Use pytest instead of Django builtin ([#654](https://github.com/MTES-MCT/ecobalyse/issues/654))

### ⚙️ Miscellaneous Tasks

- Bump braces from 3.0.2 to 3.0.3 ([#618](https://github.com/MTES-MCT/ecobalyse/issues/618))
- Connect to PG using tunnel ([#643](https://github.com/MTES-MCT/ecobalyse/issues/643))
- Use psycopg2 binary package ([#655](https://github.com/MTES-MCT/ecobalyse/issues/655))


## [1.1.0](https://github.com/MTES-MCT/ecobalyse/compare/1.0.0..v1.1.0) (2024-06-28)



### 🚀 Features

- Store history of scores for example products ([#608](https://github.com/MTES-MCT/ecobalyse/issues/608))
- Show the source of processes/ingredients/materials in the explorer ([#630](https://github.com/MTES-MCT/ecobalyse/issues/630))

### 🪲 Bug Fixes

- Remove fake details ([#622](https://github.com/MTES-MCT/ecobalyse/issues/622))
- Fix fading UI activation status bug. ([#638](https://github.com/MTES-MCT/ecobalyse/issues/638))
- Don't export legacy fake details ([#642](https://github.com/MTES-MCT/ecobalyse/issues/642))
- YarnSize API param wasn't parsed when provided as float. ([#641](https://github.com/MTES-MCT/ecobalyse/issues/641))

### 📚 Documentation

- Enhance README ([#612](https://github.com/MTES-MCT/ecobalyse/issues/612))

### ⚙️ Miscellaneous Tasks

- Add semantic-pr to check PR titles ([#636](https://github.com/MTES-MCT/ecobalyse/issues/636))
- Dont't format `*.md` files ([#639](https://github.com/MTES-MCT/ecobalyse/issues/639))
- Automate release creation ([#632](https://github.com/MTES-MCT/ecobalyse/issues/632))
- Update Pipfile.lock ([#640](https://github.com/MTES-MCT/ecobalyse/issues/640))
- Removed Azadirachtine both on Brightway and SimaPro ([#619](https://github.com/MTES-MCT/ecobalyse/issues/619))


## 1.0.0 (2024-06-20)


### Fixup

- Simplify the previous PR fix-mass-computation-before-spinning ([#388](https://github.com/MTES-MCT/ecobalyse/issues/388))

### GitBook

- [#55] Mise en forme tableau d'étapes

### WIP

- Typing all the amounts with proper units

### WiP

- New country list including processes.

<!-- generated by git-cliff -->

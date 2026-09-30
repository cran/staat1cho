# staat1cho 0.2.0

## Nieuwe functies

* **Vooropleiding en eerstejaars HO**: `combineer_indicatoren()` voegt
  `vooropleiding` (havo, vwo, mbo, ho, buitenlands, overig, onbekend) en
  `eerstejaars_ho` (eerstejaars HO of eerder in HO) toe, uit de 1CHO-kolommen
  `hoogste_vooropleiding_voor_het_ho_omschrijving_vooropleiding` en
  `eerste_jaar_in_het_hoger_onderwijs`. Ontbreken die kolommen, dan is de
  waarde "onbekend". Het dashboard heeft filters en uitsplitsingen van
  rendement en uitval op beide; het benchmarkrapport toont de samenstelling
  (`pct_eerstejaars_ho`, `pct_vooropl_havo`, `pct_vooropl_vwo`,
  `pct_vooropl_mbo`).
* **Peildatum**: het analysebestand krijgt een attribuut `peildatum`
  (1 oktober van het laatste inschrijvingsjaar). Het benchmarkrapport zet de
  peildatum in een eigen kolom, in de metadata en in de toelichting;
  `maak_benchmarkrapport()` heeft een argument `peildatum` om die expliciet op
  te geven. Het dashboard toont de peildatum in de kopbalk.

* `lees_vakhawv()` en `verrijk_met_vakhawv()`: vooropleidingscijfers uit
  VAKHAVW koppelen per student.
* `lees_bekostiging()`, `verrijk_met_bekostiging()` en
  `BEKOSTIGINGSTATUS_CODES`: bekostigingsstatus uit een VLPBEK-bestand
  koppelen, inclusief leesbare reden en of die herstelbaar is.
* `maak_benchmarkrapport()`: geaggregeerd rapport per sector,
  opleidingsvorm, niveau en instroomjaar met privacyonderdrukking.
  `schrijf_benchmarkrapport()` slaat het op als Excel met toelichting,
  validatiepunten en metadata.
* `maak_synthetische_1cho()`: synthetisch 1CHO-bestand met complete
  loopbanen en bekende uitkomsten, voor demo's en validatie (#43).

## Gewijzigd gedrag

* **VAKHAVW-koppeling**: DUO vult het persoonsgebonden nummer in EV aan met
  spaties en in VAKHAVW met nullen, waardoor de koppeling 0% vond.
  `verrijk_met_vakhawv()` negeert nu voorloopnullen bij het koppelen en
  waarschuwt pas onder 10% teruggevonden VAKHAVW-studenten (VAKHAVW bevat
  ook studenten van voor het eerste cohort in de data).
* Het dashboard accepteert bestanden tot 10 GB (was 500 MB).

* **Onvolledige cohorten** (#38): rendement, uitval en studiewissel zijn
  `"Nog niet waarneembaar"` voor cohorten waarvan het meetvenster nog niet
  in de data zit. Die rijen tellen niet mee in percentages. Voorheen kregen
  recente cohorten 0% rendement.
* `bereken_rendement()` heeft een argument `laatste_jaar`;
  `bereken_uitval()` leidt `jaar` af uit de data en geeft een fout als de
  data inschrijvingen na `jaar - 1` bevat.
* **Inlezen** (#39): `maak_basisbestand()` en `lees_vakhawv()` lezen alles
  als tekst en zetten getalkolommen expliciet om. Voorloopnullen in ID's en
  postcodes blijven behouden; leeftijd is een geheel getal (daardoor was
  `gem_leeftijd_instroom` altijd leeg).
* **Benchmarkrapport** (#41): secundaire onderdrukking, celonderdrukking
  (`min_cel`, standaard 5: een percentage is leeg als teller of complement
  kleiner dan 5 is), afronding op hele procenten en een
  `metadata`-attribuut met niveau, drempel en packageversie. Secundaire
  onderdrukking werkt ook per percentage: is in een jaar precies één groep
  leeg voor een percentage, dan wordt het ook in de kleinste zichtbare groep
  leeggemaakt. Het tabblad Metadata vermeldt de geladen optionele bestanden
  en de onderdrukkingsregels; de Toelichting noemt de celonderdrukking.
* **VLPBEK** (#42): studenten zonder BSN krijgen hun onderwijsnummer als
  sleutel, de kolomindeling wordt gecontroleerd, "herstelbaar" vereist dat
  alle redenen herstelbaar zijn, `pd` wordt "deels bekostigd" en er is een
  kolom `bekostiging_jaar`. Beide verrijkingsfuncties melden het
  koppelpercentage.
* **VLPBEK koppelt nog niet** (#42): VLPBEK gebruikt het BSN of
  onderwijsnummer, het EV-bestand een eigen persoonsnummer van DUO, dus de
  koppeling vindt bij echte leveringen (vrijwel) niets terug. README,
  dashboard, `pipeline.R` en de meldingen zeggen dit expliciet. Nieuwe functie `is_gepseudonimiseerd()`;
  een koppeling tussen een gepseudonimiseerd en een niet-gepseudonimiseerd
  bestand (VLPBEK of VAKHAVW) geeft een foutmelding met deze instructie. Het
  koppelpercentage wordt vanuit het bronbestand berekend.
* **Studentniveau** (#45): `combineer_indicatoren()` voegt
  `opleidingscode_diploma` en `diploma_in_instroomopleiding` toe; het
  dashboard legt uit dat filters op opleiding/sector de instroomopleiding
  betreffen.
* `maak_diploma_behaald()` gebruikt op inschrijvingsniveau het verblijfsjaar
  in de opleiding en levert `soort_diploma` en `opleidingscode_diploma`.
  `soortdiploma` in het eindbestand komt nu uit het behaalde diploma (#44).
* `bereken_studiewissel()` weigert cohorten op inschrijvingsniveau;
  `niveau` wordt overal gevalideerd (#44).
* `start_dashboard()` vraagt om ontbrekende dashboard-packages.

# staat1cho 0.1.0

* Eerste CRAN-release.

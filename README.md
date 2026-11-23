# GeoVis – RAM-optimierte Shiny-App

## Dateien

- `app.R`: die neue App. Sie lädt Daten nach Bedarf und verwirft Zwischenspeicher bei Tabwechseln.
- `prepare_geovis.R`: rechnet rechenintensive Modelle **einmal lokal** und speichert kleine Ergebnisdaten.
- `data/geovis_precomputed.rds`: entsteht beim Ausführen von `prepare_geovis.R`. Ohne diese Datei können Slopes/3D nicht angezeigt werden.

## Einmal lokal vorbereiten

1. Den Inhalt dieses Ordners in den vorhandenen GeoVis-Projektordner kopieren. Die dortigen `slopes.rds` und `df_elevation.rds` bitte nicht löschen.
2. In RStudio als Working Directory den GeoVis-Ordner setzen (wo `app.R` liegt).
3. Wenn `slopes.rds` **noch nicht vorhanden** ist, berechnet `prepare_geovis.R` diese Datei lokal aus den SPARTACUS-Monatsdaten. Das kann aufwendig sein und sollte NICHT auf shinyapps.io passieren.
4. `df_elevation.rds` muss vorhanden sein.
5. Einmal ausführen:

```r
source("prepare_geovis.R")
```

Danach existiert `data/geovis_precomputed.rds`.

Zum Testen:

```r
shiny::runApp(".")
```

## Online bereitstellen

Damit nicht versehentlich die großen Eingangsdaten, Berechnungsdateien oder `slopes.rds` hochgeladen werden:

```r
app_files <- c(
  "app.R",
  "data/geovis_precomputed.rds",
  list.files(pattern = "^metadata_(10min|hourly|daily|monthly)\\.json$")
)
rsconnect::deployApp(appDir = ".", appFiles = app_files)
```

Bei fehlenden Metadaten-Dateien holt die App die Metadaten einmal beim Start von GeoSphere. Für schnelleres Starten am besten die vier JSON-Dateien mit hochladen.

## Was verändert wurde

- Keine aufwendige SPARTACUS-Downloadschleife und kein `bam()` beim Online-App-Start bzw. Tabwechsel.
- Slopes-Grafiken und 3D-Plot aus vorberechneten Ergebnissen; 3D-Darstellung zur Speichereinsparung mit 4-km-Darstellungsraster (Analyse bleibt auf 1 km).
- Die `reactiveVal`-Caches für API-Daten und vorberechnete Objekte werden beim Wechsel des Haupttabs geleert; beim Wechsel von Trends- und Slopes-Untertabs werden die betreffenden Caches ebenfalls freigegeben.
- Hitzewellen und Langzeittrend werden getrennt von der API bezogen.
- Zeitindex-Korrektur: Aus den Monats-Steigungen deiner bisherigen `slopes.rds` werden in der Vorbereitung Werte pro Jahr (×12). Damit passen die dargestellten Einheiten °C/Jahr.
- Die österreichweiten Standort-IDs werden deterministisch gewählt statt bei jeder Berechnung neu ausgelost.
- Der Hitzewellenplot für „ÖSTERREICH“ verwendet eine landesweit gemittelte Tagesmaximaltemperatur der ausgewählten Stationen. Das ist keine Definition flächendeckender Hitzewellen und sollte für wissenschaftliche Aussagen gesondert überprüft werden.

## Einschränkungen

Die App wurde ohne R-Laufzeit und ohne deine tatsächlichen `.rds`-Dateien statisch überprüft, aber nicht vollständig in R gestartet oder mit der GeoSphere-API durchgetestet. Sie kann daher in deiner Umgebung weitere Anpassungen erfordern.

Der Speicher wird nicht im Sinne eines vollständigen R-Prozess-Resets geleert: `gc()` kann nur nicht mehr referenzierte Objekte freigeben. Die entscheidende Einsparung entsteht durch Offline-Vorberechnung, den reduzierten 3D-Plot und das Laden nur der nötigen Daten.

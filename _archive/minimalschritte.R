##	>Minimalbeispiel

###	Paket und Projekt erzeugen

# Es gibt viele Möglichkeiten, ein R-Paket zu erzeugen, wir nutzen dafür das Paket „usethis“. Die Erstellung des Pakets ist über wenige Funktionsaufrufe des Pakets `usethis` möglich. Ein Cheatsheet, das die wichtigsten Schritte/Funktionen zur Paketerstellung ist hier [verfügbar](https://rawgit.com/rstudio/cheatsheets/master/package-development.pdf). Ausführlichere Dokumentation gibt es z.B. [hier](https://r-pkgs.org/intro.html) oder [hier](https://r-pkgs.org/intro.html).


.libPaths("/net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/amanMRO/")
.libPaths("/net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/forostar/")
.libPaths()
library(devtools)
library(usethis)
library(available)
library(sinew)


# Am Anfang können wir überprüfen, ob der Name des Tools schon irgendwo
# verwendet wird. Das ist nur wichtig, wenn das Paket z.B. auf CRAN
# veröffentlich werden soll
available::available("genstathelpr") # Suche geht nur mit kleinbuchstaben

# Die Grundstruktur des Pakets wird mit dem Paket 'usethis' erstellt. Der Aufruf
# dieser Funktion wird das neu erstellte R-Projekt öffnen.
usethis::create_package("/net/ifs1/san_projekte/projekte/genstat/07_programme/github//GenstatHelpR")


usethis::use_git()


usethis::use_github(
  private = TRUE, # Projekte können privat, also nur für Mitglieder von GenStat, oder komplett öffentlich sein.
  organisation = "GenStatLeipzig", # das Projekt wird unter der GenStat-Organisation angelegt
  # auth_token = usethis::github_token(), # outdated auf neueren R
  protocol = "https"

) #


usethis::use_roxygen_md()

# Eine Lizenz bestimmt, was mit Open Source Code getan werden darf. Das ist
# für's erste nur ein Dokument, was mit in unserem Projektordner abgelegt wird.
# usethis::use_gpl3_license(name = "GenStat IMISE Uni Leipzig") # altes R 3.x
usethis::use_gpl3_license() # neues R 4.x

# Auf GitHub wird beim Aufruf des Projektes ein Readme im Markdown-Format
# angezeigt. Das wird hiermit erzeugt. Darin sollte eine kurze Einführung in das
# Tool beschrieben werden.
usethis::use_readme_rmd()

# Das README muss manuell über den "Knit" Button aktualisiert werden, wenn darin etwas geändert wurde.

# In eine NEWS-Datei können wichtige Änderungen an dem Tool übersichtlich
# dokumentiert werden. Diese wird hier erzeugt.
usethis::use_news_md()

### Tool Entwicklung
usethis::use_pipe()
devtools::document()


# match_hk() funktion
usethis::use_r("match_hk") # funktion reinkopiert in datei

usethis::use_test(
  name = "match_hk",
  open = FALSE)
# dokumentieren ----
devtools::load_all()

sinew::makeOxygen(match_hk) # davorkopieren, ausfuellen

devtools::document()

usethis::use_package("data.table", type = "Imports") # und weitere in der funktion verwendete pakete

devtools::build()

usethis::use_version("patch") # neue Version: 0.0.1


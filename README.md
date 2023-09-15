# Datenvalidierung_BBD

## BodenBewegungsdienst Deutschland
Stellt mit PSI-Technik verarbeitete S-1 Satelliten Daten zur Verfügung.
MIt PSI sind Deformationen von ca. +− 2mm bis zu einigen Zentimetern pro Jahr detektierbar.

## Zielsetzung
Validierung von Modellen zur Deformationsmessung mit BBD Daten über eine gemeinsame ID.
In der Zeitreihe wird jedes ID-Paar je Aufnahmedatum einzelnd validiert.

## Vorgehensweise
Jedes ID-Paar wird über eine Schleife miteinander validiert. Dafür werden Die Daten zuerst aufbereitet und zusammengeführt.
Pro ID werden lineare Modelle gebildet, um jeweils verschiedene statistische Metriken zu berechnen.

## Ergebnis
Ergebnis ist ein Dataframe, in welchem alle IDs mit den entsprechenden statistischen Metriken gespeichert werden.
Zuletzt werden für bestimmte statistische Metriken Plots erstellt, um die Validierungsergebnisse zusammenzufassen.




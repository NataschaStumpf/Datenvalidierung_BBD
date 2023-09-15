# Datenvalidierung_BBD

Validierung von Modellen, PSI_Prozessierungen von S1-Daten mit BBD Daten über eine gemeinsame ID.
Jedes ID-Paar wird über eine Schleife miteinander validiert. Dafür werden Die Daten zuerst aufbereitet und zusammengeführt.
Pro ID werden lineare Modelle gebildet, um jeweils verschiedene statistischen Metriken zu berechnen.
Ergebnis ist ein Dataframe, in welchem alle IDs mit den entsprechenden statistischen Metriken gespeichert werden.
Zuletzt werden für bestimmte statistische Metriken Plots erstellt, um die Validierungsergebnisse zusammenzufassen.




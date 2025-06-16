Fire Forest Project
Simulation multi-paramétrique et visualisation de la propagation d’un feu de forêt en grille 2D sous différentes conditions météorologiques.

🚀 Fonctionnalités principales
Simulation de la propagation du feu basée sur un modèle physique (inspiré de Rothermel 1972)

Paramètres environnementaux : température, humidité du combustible, vitesse et direction du vent

Recherche automatique des “triplets critiques” : point où le feu passe brutalement de propagation massive à extinction

Export CSV et PNG des états pour analyse ou visualisation graphique

Visualiseur interactif en ScalaFX pour lire les résultats étape par étape

📁 Structure du projet
bash
Copier
Modifier
src/
 └─ main/
     └─ scala/
         ├─ fire/
         │   └─ model/
         │       ├─ Cell.scala         # Définition d'une cellule de la grille
         │       ├─ CellState.scala    # États logiques d'une cellule (Unburned, Heating, ...)
         │       ├─ Grid.scala         # Logique de la grille et évolution à chaque étape
         │       ├─ Meteo.scala        # Paramètres météo (vent, pas de temps)
         │       └─ Simulation.scala   # Orchestration de la simulation
         └─ Simulations/
             ├─ CriticalTripletsFinder.scala  # Recherche automatique des conditions critiques
             └─ ExportSimulation.scala        # Export des états de simulation (CSV, images, ...)
🧪 Modèle de simulation
Grille 2D : chaque case = une cellule (Cell) avec état, température, masse de combustible, humidité, etc.

États d'une cellule : Unburned, Heating, Igniting, Burning, Burned, Torched

Flux physiques calculés à chaque pas : rayonnement, conduction, convection, évaporation de l’eau

Vent : prise en compte de la direction/force dans la propagation (poids directionnels)

Transition d'état selon les équations physiques (température d’ignition, combustion, extinction...)

🔬 Algorithmes
Step : à chaque pas de temps, chaque cellule :

Calcule l’influence de ses voisins (Moore 8, pondérés par le vent)

Actualise sa température, humidité, masse restante

Change d’état si seuil atteint (ignition, extinction, etc.)



🖥️ Visualisation
Visualiseur ScalaFX (interface graphique)

Affiche l’évolution des états de la grille étape par étape (PNG exportés)

Plusieurs vues synchronisées : état logique, température, humidité, masse de combustible

Échelles de couleurs pour chaque variable

Contrôle de la vitesse, de l’étape, du chargement, etc.

📊 Export & Analyse
Export CSV : ratios d’arbres brûlés, étapes critiques, etc.

Export PNG : cartes couleurs pour chaque variable à chaque étape (pour animation ou analyse)

⚙️ Lancer une simulation : 

src/main/scala/Simulations/CriticalTripletsFinder.scala
src/main/scala/Simulations/ExportSimulation.scala
src/main/scala/Simulations/FireSimApp.scala
src/main/scala/Simulations/FireSimApp.scala
src/main/scala/Simulations/FireSimApp.scala

Configurer les paramètres dans le code (taille grille, conditions initiales, météo, etc.)

Compiler et exécuter le module souhaité (par exemple : CriticalTripletsFinder pour l’étude de sensibilité)

Lire les résultats dans le CSV et/ou ouvrir les PNG générés avec le visualiseur ScalaFX

📚 Ressources principales
Rothermel, R. C. (1972). A mathematical model for predicting fire spread in wildland fuels. USDA Forest Service Research Paper INT-115.

Implémentation inspirée des modèles physiques (conduction, convection, rayonnement) adaptés à la grille.

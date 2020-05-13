# Projet PAF

__Description du Dunjeon Crawler:__

Le joueur, incarné par notre héro, doit atteindre le coffre au trésor. De nombreux obstacles l'attente, en effet ce donjon est peuplé de terrible monstres
qui si le héro n'est pas muni d'une épée pourront le tuer, ainsi la partie reset. Les items a disposition du héro seront des épées pour éliminer les monstres
lorsque celui-ci les touchera, en plus des clefs qui permettent d'ouvrir les portes.



__Les choix d'invariants :__
    - Monstre : Un monstre est défini par son espèce, ces coordonnées, sa direction, son compteur de déplacement et son affichage . Ainsi l'invariant vérifie
    que les coordonnées du monstre soit correcte (c'est-à-dire positionné sur une case), le compteur de déplacements et la direction où va se déplacer la monstre.
    
    - Item : Un item est défini par son type et son affichage. Ainsi, l'invariant vérifie que le type de l'item soit différent de ErrorType.
    
    - Carte : Une carte est définie par sa largeur, sa hauteur et son contenu. Ainsi, l'invariant vérifie que la hauteur et la largeur de la carte soit valide par
    rapport au nombre de cases présent, que chaque case existe et qu'elle possède des coordonnées correctes. De plus, la carte doit être entouré de murs et cela 
    doit vérifier qu'il y ait un mur de chaque coté d'une porte selon son orientation.
    
    - GameState: Le gamestate est défini par les coordonnées du héro, les items qu'il possède, sa vitesse, la liste des monstres présent dans le niveau et celle 
    des items, la carte du niveau initiale et la carte courante. Ainsi, l'invariant vérifie l'ensemble de ces points. (cartes correctes, monstres,...)


La vérification des coordonnées consiste à controler si celles-ci soit bien supérieure ou égale à 0 et que ce sont bien des multiples de 50 car notre jeu est
définit par des cases de 50 pixels.
Chaque monstre dispose d'un pattern de direction, ainsi pour vérifier la direction qui reprente l'index du tableau pattern, celle-ci doit est comprise entre 0
et la longueur du tableau.
Le compteur de déplacement correspond au nombre de déplacement a effectué par direction, ainsi nous vérifions s'il est compris entre 0 et le nombre maximum 
défini par défaut pour chaque espece.
    
    
    
__Les extensions :__
    - Déplacement : le personnage peut se deplacer (touche z,q,s,d) et un scroling de la carte est present.
    
    - Monstre : il y a trois differents type de monstre chacun avec ces propres pattern. 
    
    - Actions du personnage : - Le héro peut recupérer une épée.
                              - Le héro peut recupérer une clef.
                              - Le héro peut ouvrir une porte en appuyant sur "k" s'il possède une clef.
                              - Le héro meurt s'il ne possède pas d'épee et qu'il rentre en collision avec un monstre. 
                              - Le héro ne possède donc qu'une vie.
                              - Le héro peut tuer un monstre s'il rentre en collision avec et qu'il possède une épee.
                              
    - Pour quitter la partie, il faut appuyer sur "ESCAP" ou sur la croix de la fenête.

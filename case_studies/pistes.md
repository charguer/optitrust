## Pistes 
## 0. Quantization && Valdidation 
[] Finir la quantization / parallélisation
[] Valider le script en mode 2 
  [] Reorder dims ? 
[] Sucre syntaxique pour décrire les ressources pour les matrices? 

## 1. Passer la validation en mode 3
Dériver la spécification formelle pour valider le script en niveau 3
### A faire: 
[] Formaliser l'inférence des ressources ++ à partir des functions + contrats
[] Implémenter ce formalisme  
[] Formule maths pour l'algo de LLM 
[] Intégrer le niveau 3 aux transformations manquantes 
### Les plus: 
- Expertise d'Arthur 
- Completement dans le cadre de la mt (Peter Mueller) 
- vraie publication scientifique plus accessible
### Questions:  
- Vraiment adapté à ce que demande l'industrie des LLMS ? -> plus les gens compilo 
- Définir la taille du travail à faire : conceptualiser pdt 1 demi journée

## 2. Monter le niveau d'abstraction 
Concevoir une manère d'écrire en fonctionnel les algorithmes de LLM puis de les dériver dans un code C 
### A faire:
[] Concevoir le dit language ~ 2 3 jours  
[] Implémenter la conversion en C 
[] 
### Les plus:
- Plus proche de ce qu'écrivent les gens qui développe des LLMS (Timothée) 
- Permet de faire le lien entre les matheux et les gens qui implementent le backend
### Les moins:
- Perte d'intérêt des transfos déjà réalisées (car déjà faite par PyTorch)
### Questions: 
- Est ce possible d'abstraire ? ~ 2/3 jours
- Est ce que ça répond au besoin des gens qui implémentent des nouvelles features ?  
- Quel type de transformations pour les convaincre ? 
- Est-ce que ça existe déjà ? (Triton) ? Dans quelle mesures 

## 3. Intégrer des transformations plus SOTA
Intégrer la FlashAttention - les experts - le microbatch 
### A Faire: 
[] La Liste d'au dessus
### Les plus: 
- Transformations plus intéressantes pour les gens qui developpent des LLMS 
- Facilité d'implémentation car potentiellement proche de ce qu'on a déjà fait 

### Les moins: 
- Facilité d'implémentation car potentiellement proche de ce qu'on a déjà fait 
- Pas de nouveauté scientifique
### Questions: 
- Est ce une transformation sémantique ou faut-il partir d'un nouveau code de base ? 
- trop SOTA ou ressources en ligne ?
- Y a-t-il des transformations spécifiques pour le Cuda ? 
- Est ce nécessaire de coder cela pour démonter l'intérêt / ou les anciennes transfos sont suffisantes ? 
- Intérêt pour les 10/20 % de performances

### 4. Diversifier les outputs 
Diversifier les output à paritr d'un même script afin de démontrer la rapidité d'OT à s'adapater au HW 
### Questions: 
- Quels transformations sont des bons exemples pour montrer la diversité des outputs et qui démontre des différences ? Y en a t il ? 
- Voir avec Cédric  

## Pistes 
## 0. Quantization && Valdidation 
[] Finir la quantization / parallélisation
[] Valider le script en mode 2 
  [] Reorder dims ? 
[] Sucre syntaxique pour décrire les ressources pour les matrices? 
## 1. Passer la validation en mode 3
Dériver la aspécification formelle pour valider le script en niveau 3
### A faire: 
[] Formaliser l'inférence des ressources ++ à partir des functions + contrats
[] Implémenter ce formalisme  
[] Intégrer le niveau 3 aux transformations manquantes 
### Les plus: 
- Expertise d'Arthur 
- Completement dans le cadre de la mt (Peter Mueller) 
### Questions:  
- Vraiment adapté à ce que demande l'industrie des LLMS ?
- Définir la taille du travail à faire 
## 2. Monter le niveau d'abstraction 
Concevoir une manère d'écrire en fonctionnel les algorithmes de LLM puis de les dériver dans un code C 
### A faire:
[] Concevoir le dit language 
[] Implémenter la conversion en C 
[] 
### Les plus:
- Plus proche de ce qu'écrivent les gens qui développe des LLMS (Timothée) 
- Permet de faire le lien entre les matheux et les gens qui implementent le backend
### Les moins:
- Perte d'intérêt des transfos déjà réalisées (car déjà faite par PyTorch)
### Questions: 
- Est ce que ça répond au besoin des gens qui implémentent des nouvelles features ?  
- Quel type de transformations pour les convaincre ? 
- Est-ce que ça existe déjà ? (Triton) ? Dans quelle mesures 
## 3. Intégrer des transformations plus SOTA
Intégrer la FlashAttention- les experts - le microbatch - le support Cuda
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
### 4. Diversifier les outputs 
Diversifier les output à paritr d'un même script afin de démontrer la rapidité d'OT à s'adapater au HW 
### Questions: 
- Quels transformations sont des bons exemples pour montrer la diversité des outputs ? Y en a t il ? 
- 

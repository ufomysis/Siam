%Projet IA02 - Jeu de SIAM
%Simon Lancelot et Yoann Vandecnocke
%Universite de Technologie de Compiegne

%Prédicat à appeler à la lecture du fichier
:-initialization(menu).

menu :-   write('1 - Jouer une partie contre l\'ordinateur'), nl,
			write('2 - Jouer une partie a deux joueurs'), nl,
			write('3 - Jouer une partie ordinateau contre ordinateur'), nl,
			write('Entrez votre choix : '),
			read(X), nl, lancer_partie(X).
			
lancer_partie(1) :- initialisation_ia_h(P),!.
lancer_partie(2) :- initialisation(P),!.
lancer_partie(3) :- ia(P,[]),!.
lancer_partie(X) :- menu,!.

%%%%%%%%%%%%%%%%%%%%%%
%PARTIE I : INTERFACE%
%%%%%%%%%%%%%%%%%%%%%%

%Affichage des lignes
affiche_lignes(I,P):- I=0,!.
affiche_lignes(I,P):- write('  --------------------------'),nl,write(I), write(' | '),affiche_case(1,I,P),nl,  Nouveau_I is I-1, affiche_lignes(Nouveau_I,P).

%Affichage des cases
affiche_case(6,I,P):-!.
affiche_case(J,I,P):- K is I*10+J,cherche_M(K,P), write(' | '), Nouveau_J is J+1, affiche_case(Nouveau_J,I,P).
affiche_case(J,I,P):- K is I*10+J,cherche_E(K,P), write(' | '), Nouveau_J is J+1, affiche_case(Nouveau_J,I,P).
affiche_case(J,I,P):- K is I*10+J,cherche_R(K,P), write(' | '), Nouveau_J is J+1, affiche_case(Nouveau_J,I,P).
affiche_case(J,I,P):- K is I*10+J,write('  '), write(' | '), Nouveau_J is J+1, affiche_case(Nouveau_J,I,P).

%Affichage du plateau passé en argument
affiche_plateau(P):- affiche_lignes(5,P),write('  --------------------------'),nl,
										 write('     1    2    3    4    5  '),nl.

%Initialisation et affichage du plateau de debut de partie
plateau_initial(P):- P=[[(0,0),(0,0),(0,0),(0,0),(0,0)],[(0,0),(0,0),(0,0),(0,0),(0,0)],[32,33,34],e].
liste_exemple(L):- L=C.

%On verifie si une montagne se trouve en position K et on l'affiche
cherche_M(K,[_,_,L,_]):-member(K,L),write('/\\'),!.
cherche_M(K,[_|Q]) :- verifier_M(K,Q). 

%On verifie si un elephant se trouve en position K et on l'affiche ainsi que son orientation
cherche_E(K,[L,_,_,_]):-member((K,O),L),write('E'),verif_or((K,O)),!.
cherche_E(K,[_|Q]) :- verifier_E(K,Q). 

%On verifie si un rhinoceros se trouve en position K et on l'affiche ainsi que son orientation
cherche_R(K,[_,L,_,_]):-member((K,O),L),write('R'),verif_or((K,O)),!.
cherche_R(K,[_|Q]) :- verifier_E(K,Q). 

verif_or((_,O)):- write(O).

%%%%%%%%%%%%%%%%%%%%%%%%
%PARTIE II : JEU HUMAIN%
%%%%%%%%%%%%%%%%%%%%%%%%

%On verifie si une montagne se trouve en position K
verifier_M(K,[_,_,L,_]):-member(K,L),!.
verifier_M(K,[_|Q]) :- verifier_M(K,Q). 

%On verifie si un elephant se trouve en position K
verifier_E(K,[L,_,_,_]):-member((K,O),L),!.
verifier_E(K,[_|Q]) :- verifier_E(K,Q). 

%On verifie si un rhinoceros se trouve en position K
verifier_R(K,[_,L,_,_]):-member((K,O),L),!.
verifier_R(K,[_|Q]) :- verifier_E(K,Q). 

%case libre vérifie si la case est libre, commentaire inutile ici !
case_libre(P,0).
case_libre(P,K) :- \+verifier_M(K,P), \+verifier_E(K,P), \+verifier_R(K,P).


%verifie si un pion jouable se trouve sur la case K
pion_jouable(P,K) :- verifier_E(K,P).
pion_jouable(P,K) :- verifier_R(K,P).

%verifie si une case est bien une case extérieure
verif_case_ext(C):- 0 is C mod 5,!.
verif_case_ext(C):- 10 is C // 5,!.
verif_case_ext(C):- 1 is C mod 10,!.
verif_case_ext(C):- 1 is C // 10,!.

%verifie que le coup est possible
coup_possible(P,D,D,O) :- D\=0,pion_jouable(P,D), get_orientation(P,D,Or), O\=Or,!.
coup_possible(P,D,A,O) :- D\=0,pion_jouable(P,D), D\=A,get_orientation(P,D,Or), O==Or, deplacement_possible(P,D,A),!.
coup_possible(P,D,A,O) :- D\=0, pion_jouable(P,D), \+deplacement_possible(P,D,A), p_possible(P,D,O,L),!.
coup_possible(P,D,A,O) :- D==0,pion_jouable(P,D), D\=A,deplacement_possible(P,D,A),!.
coup_possible(P,0,A,n) :- pion_jouable(P,0),member(A,[11,12,13,14,15]), \+deplacement_possible(P,0,A), D is A-10, p_possible(P,D,O,L),!.
coup_possible(P,0,A,s) :- pion_jouable(P,0),member(A,[51,52,53,54,55]), \+deplacement_possible(P,0,A), D is A+10, p_possible(P,D,O,L),!.
coup_possible(P,0,A,e) :- pion_jouable(P,0),member(A,[11,21,31,41,51]), \+deplacement_possible(P,0,A), D is A-1, p_possible(P,D,O,L),!.
coup_possible(P,0,A,o) :- pion_jouable(P,0),member(A,[15,25,35,45,55]), \+deplacement_possible(P,0,A), D is A+1, p_possible(P,D,O,L),!.
%ajouter pion_jouable 

%permet de vérifier si on peut se déplacer vers une case libre
deplacement_possible(P,D,A):- case_libre(P,A),A is D+1,!.
deplacement_possible(P,D,A):- case_libre(P,A),A is D+10,!.
deplacement_possible(P,D,A):- case_libre(P,A),A is D-1,!.
deplacement_possible(P,D,A):- case_libre(P,A),A is D-10,!.
deplacement_possible(P,D,0):- verif_case_ext(D),!.
deplacement_possible(P,0,A) :- verif_case_ext(A),case_libre(P,A),!.

%permet de verifier si on peut pousser
%poussee_possible(P,(D,Od),(A,Oa)) :- \+case_libre(P,A),A is D+1, Od=='e',!.
%poussee_possible(P,(D,Od),(A,Oa)) :- \+case_libre(P,A),A is D+10,Od=='n',!.
%poussee_possible(P,(D,Od),(A,Oa)) :- \+case_libre(P,A),A is D-1,Od=='o',!.
%poussee_possible(P,(D,Od),(A,Oa)) :- \+case_libre(P,A),A is D-10,Od=='s',!.
%poussee_possible(P,(D,Od),(A,Oa)) :- verif_case_ext(A), \+case_libre(P,A).

%vérifie l'opposition d'une pièce par rapport à une autre, et retourne yes si les 2 pièces sont opposées 
verif_opposition(o,e).
verif_opposition(e,o).
verif_opposition(n,s).
verif_opposition(s,n).

%verifie si une poussée est possible avec la liste des orientations des acteurs de la poussée
poussee_possible([E,[m|Q]],F,Masse) :- M1 is Masse+1,F>0,!,F>=M1,!,poussee_possible([E,Q],F,M1),!.
poussee_possible([E,[T|Q]],F,Masse) :- verif_egalite(E,T), F1 is F+1,F1>0,!,F1>=Masse,!, poussee_possible([E,Q],F1,Masse),!.
poussee_possible([E,[T|Q]],F,Masse) :- verif_opposition(E,T), F1 is F-1, F1>0,!,F1>=Masse,!, poussee_possible([E,Q],F1,Masse),!.
poussee_possible([E,[T|Q]],F,Masse) :- \+verif_egalite(E,T), \+verif_opposition(E,T),F>0,!,F>=Masse,!,poussee_possible([E,Q],F,Masse),!.
poussee_possible([E,[]],F,Masse) :- !.

%vérifie si les orientations sont les mêmes
verif_egalite(e,e).
verif_egalite(o,o).
verif_egalite(s,s).
verif_egalite(n,n).

%récupération de la liste des orientations des acteurs d'une poussée
recup_liste_or(P,C,Dir,L) :- 	\+successeur(P,C,A,Dir),!.
recup_liste_or(P,C,Dir,L) :- 	successeur(P,C,A,Dir),
						recup_liste_or(P,A,Dir,L1),
						get_orientation(P,A,Or),
						concat([Or],L1,L),!.

%récupération de la liste des acteurs d'une poussée
recup_liste_succ(P,C,Dir,L) :- 	\+successeur(P,C,A,Dir),!.
recup_liste_succ(P,C,Dir,L) :- 	successeur(P,C,A,Dir),
								recup_liste_succ(P,A,Dir,L1),
								concat([A],L1,L),!.						

%test si la poussée est possible								
p_possible(P,C,Dir,L) :- recup_liste_or(P,C,Dir,L),L2=[Dir,L], poussee_possible(L2,1,0),!.



%verifier le successeur pour la poussée
successeur([],D,_,_) :- member(D,[]),!.
successeur([E,R,M,_],D,A,e) :- D2 is D+1,member((D2,_),E), A is D+1,!.
successeur([E,R,M,_],D,A,e) :- D2 is D+1,member((D2,_),R), A is D+1,!.
successeur([E,R,M,_],D,A,e) :- D2 is D+1,member(D2,M), A is D+1,!.
successeur([E,R,M,_],D,A,o) :- D2 is D-1,member((D2,_),E), A is D-1,!.
successeur([E,R,M,_],D,A,o) :- D2 is D-1,member((D2,_),R), A is D-1,!.
successeur([E,R,M,_],D,A,o) :- D2 is D-1,member(D2,M), A is D-1,!.
successeur([E,R,M,_],D,A,n) :- D2 is D+10,member((D2,_),E), A is D+10,!.
successeur([E,R,M,_],D,A,n) :- D2 is D+10,member((D2,_),R), A is D+10,!.
successeur([E,R,M,_],D,A,n) :- D2 is D+10,member(D2,M), A is D+10,!.
successeur([E,R,M,_],D,A,s) :- D2 is D-10,member((D2,_),E), A is D-10,!.
successeur([E,R,M,_],D,A,s) :- D2 is D-10,member((D2,_),R), A is D-10,!.
successeur([E,R,M,_],D,A,s) :- D2 is D-10,member(D2,M), A is D-10,!.

%Vérifie la case suivante dans une direction. Si on sort du plateau, renvoie 0
case_suivante(P,C1,C2,e) :- 0 == C1 mod 5, C2 is 0,!.
case_suivante(P,C1,C2,e) :- C2 is C1+1,!.
case_suivante(P,C1,C2,o) :- 1 == C1 mod 10,C2 is 0,!.
case_suivante(P,C1,C2,o) :- C2 is C1-1,!.
case_suivante(P,C1,C2,n) :- 10 == C1 // 5, C2 is 0,!.
case_suivante(P,C1,C2,n) :- C2 is C1+10,!.
case_suivante(P,C1,C2,s) :- 1 == C // 10, C2 is 0,!.
case_suivante(P,C1,C2,s) :- C2 is C1-10,!.

%renvoie l'orientation d'une pièce donnée
get_orientation([E,R,M,_],C,Or) :- member((C,Or),E),!.
get_orientation([E,R,M,_],C,Or) :- member((C,Or),R),!.
get_orientation([E,R,M,_],C,Or) :- member(C,M),Or = m,!.

%Concaténation de deux chaines dans une troisième
concat(L1,[],L1).
concat([],L2,L2).
concat([T1|Q1],L2,[T1|Q3]) :- concat(Q1,L2,Q3).

%Change le tour de jeu
switch_joueur([E,R,M,e],P2) :- P2 = [E,R,M,r].
switch_joueur([E,R,M,r],P2) :- P2 = [E,R,M,e].


%jouer un coup
jouer_coup(Pi,D,A,Or,Np) :- \+coup_possible(Pi,D,A,Or), Np=Pi,!.
jouer_coup(Pi,D,A,Or,Np) :- case_libre(Pi,A),coup_possible(Pi,D,A,Or),modif_plateau(Pi,D,A,Or,P2), switch_joueur(P2,Np).
jouer_coup(Pi,D,D,Or,Np) :- \+coup_possible(Pi,D,D,Or), Np=Pi,!.
jouer_coup(Pi,D,D,Or,Np) :- coup_possible(Pi,D,D,Or),modif_plateau(Pi,D,D,Or,P2), switch_joueur(P2,Np).
jouer_coup(Pi,D,A,Or,Np) :- D\=0, \+case_libre(Pi,A),coup_possible(Pi,D,A,Or),recup_liste_succ(Pi,D,Or,L), concat([D],L,L1),jouer_poussee(Pi,D,A,Or,L1,P2), continuer(P2), switch_joueur(P2,Np).
jouer_coup(Pi,D,A,Or,Np) :- D\=0, \+case_libre(Pi,A),coup_possible(Pi,D,A,Or),recup_liste_succ(Pi,D,Or,L), concat([D],L,L1),reverse(L1,L2),jouer_poussee(Pi,D,A,Or,L1,Np), \+continuer(Np), qui_gagne(Pi,L2,Or).
jouer_coup(Pi,0,A,Or,Np) :- \+case_libre(Pi,A),coup_possible(Pi,D,A,Or),recup_liste_succ(Pi,A,Or,L), concat([A],L,L1),jouer_poussee(Pi,D,A,Or,L1,P2),modif_plateau(P2,0,A,Or,P3), continuer(P2), switch_joueur(P3,Np).
jouer_coup(Pi,0,A,Or,Np) :- \+case_libre(Pi,A),coup_possible(Pi,D,A,Or),recup_liste_succ(Pi,A,Or,L), concat([A],L,L1),jouer_poussee(Pi,D,A,Or,L1,P2),modif_plateau(P2,0,A,Or,Np), \+continuer(Np), qui_gagne(Pi,L,Or).

jouer_poussee(Pi,_,_,_,[],Pi).
jouer_poussee(Pi,D,A,Or,[T1|Q],Np) :- jouer_poussee(Pi,D,A,Or,Q,Np2),case_suivante(Pi,T1,T2,Or), pousse_plateau(Np2,T1,T2,Or,Np),!.

%modifier un plateau par rapport aux éléphants
modif_plateau([[],_,_,_],_,_,_,_).
modif_plateau([[(T,O)|Q],R,M,e],D,A,Or,P2) :- member((D,_),[(T,O)|Q]),								
										retire_animal([(T,O)|Q],D,N),
										ajoute_animal(N,A,Or,New),nl,
										P2=[New,R,M,e],!.
modif_plateau([[(T,O)|Q],R,M,e],D,A,Or,P2) :- modif_plateau([Q,R,M,e],D,A,Or,P2).


%modifier plateau en fonction des rhino
modif_plateau([_,[],_,_],_,_,_,_).
modif_plateau([E,[(T,O)|Q],M,r],D,A,Or,P2) :- member((D,_),[(T,O)|Q]),									
										retire_animal([(T,O)|Q],D,N),
										ajoute_animal(N,A,Or,New),nl,
										P2=[E,New,M,r],!.
modif_plateau([E,[(T,O)|Q],M,r],D,A,Or,P2) :- modif_plateau([E,Q,M,r],D,A,Or,P2).

%Modification du plateau en fonction de la poussée
%modifier plateau en fonction des éléphants
pousse_plateau([E,R,M,J],D,A,Or,P2) :- member((D,Or2),E), \+member(D,M), 							
										retire_animal(E,D,N),
										ajoute_animal(N,A,Or2,New),nl,
										P2=[New,R,M,J],!.
pousse_plateau([E,R,M,J],D,A,Or,P2) :- member((D,Or2),R), \+member(D,M),						
										retire_animal(R,D,N),
										ajoute_animal(N,A,Or2,New),nl,
										P2=[E,New,M,J],!.
										
pousse_plateau([E,R,M,J],D,A,Or,P2) :- 	member(D,M),									
										retire_montagne(M,D,N),
										ajoute_montagne(N,A,New),
										P2=[E,R,New,J],!.

%ajouter un animal sur le plateau										
ajoute_animal([(0,0)|Q],C,Or,[(C,Or)|Q]):-!.
ajoute_animal([T|Q],C,Or,[T|R]):-ajoute_animal(Q,C,Or,R).

%retirer un animal du plateau
retire_animal([(C,_)|Q], C, [(0,0)|Q]):-!.
retire_animal([T|Q], C, [T|R]):- retire_animal(Q, C, R).

%retirer une montagne sur le plateau
retire_montagne([C|Q], C, [0|Q]):-!.
retire_montagne([T|Q], C, [T|R]):- retire_montagne(Q, C, R).

%ajouter une montagne sur le plateau
ajoute_montagne([0|Q],C,[C|Q]):-!.
ajoute_montagne([T|Q],C,[T|R]):-ajoute_montagne(Q,C,R).

%bouge_montagne([E,R,[D|Q],J],D,A,[E,R,[A|Q],J]).
%bouge_montagne( [E,R,[T|Q],J], D,A, [E,R,[T|Res],J]):- bouge_montagne([E,R,Q,J], D,A, [E,R,Res,J]).

%jouer une partie :
jouer(P) :- \+continuer(P),menu,!.	
jouer(P) :-	affiche_plateau(P),
			tour(P),
			write('Entrez une case depart :'),nl,
			read(X),nl,write('Entrez une case d\'arrivee :'),nl,
			read(Y),nl,write('Entrez une orientation :'),nl,
			read(Or),nl,
			jouer_coup(P,X,Y,Or,Np),Np2 = Np,jouer(Np2),!.


%Tour de jeu : on affiche le joueur qui doit jouer
tour([_,_,_,r]) :- write('Aux rhinoceros de jouer'),nl,!.
tour([_,_,_,e]) :- write('Aux elephants de jouer'),nl,!.
		
initialisation(P) :- plateau_initial(P),jouer(P).

%teste si toutes les montagnes sont sur le jeu. Si ce prédicat est vrai, il n'y a pas de gagnant
continuer([E,R,[],J]).
continuer([E,R,[T|Q],J]) :- member(T,[11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55]),
						continuer([E,R,Q,J]),!.

%affiche le gagnant
qui_gagne([E,R,M,e],[],Or) :- write('Victoire des elephants'), nl,!.
qui_gagne([E,R,M,r],[],Or) :- write('Victoire des rhinoceros'), nl,!.
qui_gagne([E,R,M,e],[T|Q],Or) :- member((T,Or2),E), Or == Or2, write('Victoire des elephants'), nl,!.
qui_gagne([E,R,M,r],[T|Q],Or) :- member((T,Or2),E), Or == Or2, write('Victoire des elephants'), nl,!.
qui_gagne([E,R,M,e],[T|Q],Or) :- member((T,Or2),R), Or == Or2, write('Victoire des rhinoceros'), nl,!.
qui_gagne([E,R,M,r],[T|Q],Or) :- member((T,Or2),R), Or == Or2, write('Victoire des rhinoceros'), nl,!.
qui_gagne([E,R,M,J],[T|Q],Or) :- qui_gagne([E,R,M,J],Q,Or).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PARTIE III : Intelligence artificielle%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%ia(P,D,A,Or) :- plateau_initial(P),bagof((D,A,Or),coup_possible(P,D,A,Or),L),write(L),successeur(P,A,B,Or),ia(P,A,B,Or).

coups_possibles([[],_,_,_],L1,Lcoup) :- write(L1),Lcoup=L1,!.
coups_possibles([[(T,Or)|Q],R,M,J],L1,Lcoup) :- \+successeur([[(T,Or)|Q],R,M,J],T,A,Or),
											
										setof((T,A,Or),succ_libre([[(T,Or)|Q],R,M,J],T,A),L),!,nl,
										
										!,concat(L,L1,L2),coups_possibles([Q,R,M,J],L2,Lcoup),!.

coups_possibles([[(T,Or)|Q],R,M,J],L1,Lcoup) :- successeur([[(T,Or)|Q],R,M,J],T,A,Or),\+case_libre([[(T,Or)|Q],R,M,J],A),
										setof((T,A,Or),coup_possible([[(T,Or)|Q],R,M,J],T,A,Or),L),!,nl,
										
										!,concat(L,L1,L2),coups_possibles([Q,R,M,J],L2,Lcoup),!.
										
coups_possibles([[(0,0)|Q],R,M,J],L1,Lcoup) :- 
											
										setof((0,A,0),verif_case_exterieur([[(0,0)|Q],R,M,J],A),L),!,nl,
										
										!,concat(L,L1,L2),coups_possibles([Q,R,M,J],L2,Lcoup),!.								

									
%coups_possibles([[(T,Or)|Q],R,M,J],L1) :- coups_possibles([Q,R,M,J],L2).
										

										
%coups_possibles([E,[(T,Or)|Q],M,J],L1) :- coups_possibles([E,Q,M,J],L2).

%coups_possibles(P,L) :-  findall((D,A,Or), coup_possible(P,D,A,Or), L).

ia(P,L) :- plateau_initial(P),Np = P, coups_possibles(Np,L,Lcoup),nl,write('ok : '),write(Lcoup),jouer_ia(P,Lcoup).
										
succ_libre(P,D,A) :- A is D+1,case_libre(P,A),member(A,[11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55]).
succ_libre(P,D,A) :- A is D-1,case_libre(P,A),member(A,[11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55]).
succ_libre(P,D,A) :- A is D+10,case_libre(P,A),member(A,[11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55]).
succ_libre(P,D,A) :- A is D-10,case_libre(P,A),member(A,[11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55]).

verif_case_exterieur(P,A):- A is 11,case_libre(P,A),member(A,[11,12,13,14,15]).
verif_case_exterieur(P,A):- A is 12,case_libre(P,A),member(A,[11,12,13,14,15]).
verif_case_exterieur(P,A):- A is 13,case_libre(P,A),member(A,[11,12,13,14,15]).
verif_case_exterieur(P,A):- A is 14,case_libre(P,A),member(A,[11,12,13,14,15]).
verif_case_exterieur(P,A):- A is 15,case_libre(P,A),member(A,[11,12,13,14,15]).
verif_case_exterieur(P,A):- A is 21,case_libre(P,A),member(A,[21,31,41]).
verif_case_exterieur(P,A):- A is 31,case_libre(P,A),member(A,[21,31,41]).
verif_case_exterieur(P,A):- A is 41,case_libre(P,A),member(A,[21,31,41]).
verif_case_exterieur(P,A):- A is 25,case_libre(P,A),member(A,[25,35,45]).
verif_case_exterieur(P,A):- A is 35,case_libre(P,A),member(A,[25,35,45]).
verif_case_exterieur(P,A):- A is 45,case_libre(P,A),member(A,[25,35,45]).
verif_case_exterieur(P,A):- A is 51,case_libre(P,A),member(A,[51,52,53,54,55]).
verif_case_exterieur(P,A):- A is 52,case_libre(P,A),member(A,[51,52,53,54,55]).
verif_case_exterieur(P,A):- A is 53,case_libre(P,A),member(A,[51,52,53,54,55]).
verif_case_exterieur(P,A):- A is 54,case_libre(P,A),member(A,[51,52,53,54,55]).
verif_case_exterieur(P,A):- A is 55,case_libre(P,A),member(A,[51,52,53,54,55]).

%Jeu IA contre IA

jouer_ia(P,_) :- \+continuer(P).

jouer_ia(P,[]).

jouer_ia(P,[(D,A,Or)|Q]) :- continuer(P),write(D),nl,write(A),nl,write(Or),Or==0,Or2 = e,write(Or2),nl,jouer_coup(P,D,A,Or2,Np),affiche_plateau(Np),jouer_ia(Np,Q).

jouer_ia(P,[(D,A,Or)|Q]) :- continuer(P),write(D),nl,write(A),nl,write(Or),Or\=0,jouer_coup(P,D,A,Or,Np),affiche_plateau(Np),jouer_ia(Np,Q).




jouer_ia_humain(P,[(D,A,Or)|Q]) :- continuer(P),write(D),nl,write(A),nl,write(Or),Or==0,Or2 = e,write(Or2),nl,jouer_coup(P,D,A,Or2,Np),jouer2(Np,Q).

jouer_ia_humain(P,[(D,A,Or)|Q]) :- continuer(P),write(D),nl,write(A),nl,write(Or),Or\=0,jouer_coup(P,D,A,Or,Np),jouer2(Np,Q).

initialisation_ia_h(P) :- plateau_initial(P),ia_2(P,[]).

jouer2(P,Q) :-	affiche_plateau(P),
			tour(P),
			write('Entrez une case depart :'),nl,
			read(X),nl,write('Entrez une case d\'arrivee :'),nl,
			read(Y),nl,write('Entrez une orientation :'),
			read(Or),
			jouer_coup(P,X,Y,Or,Np),Np2 = Np,jouer_ia_humain(Np2,Q),!.
			
ia_2(P,L) :- Np = P, coups_possibles(Np,L,Lcoup),jouer_ia_humain(P,Lcoup).
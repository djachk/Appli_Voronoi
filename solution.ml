(* ce module contient les fonctions calculant une solution de coloriage a l'aide d'un sat solver*)

open Printf;;
open Graphics;;
open String;;
open List;;
open Voronoi;;
open Colorier;;

(*cette liste est la liste CNF que l'on va passer au sat solver*)
let gde_liste= ref [[]];;

(* fonction de service...*)
let enlever_liste_vide l= match l with
  []::q->q
  |_ -> l;;

(*cette fonction ajoute les DNF specifiant l'existence d'une couleur pour chaque region;
la variable est un entier -dizaine=region, unite=couleur*)
let existence nbseeds =
  for ind=0 to nbseeds-1 do
    let l=[(true,ind*10);(true,ind*10+1);(true,ind*10+2);(true,ind*10+3)] in
    gde_liste:=List.append !gde_liste [l];
  done;
  gde_liste:=enlever_liste_vide !gde_liste;;

(*cette fonction ajoute les DNF specifiant l'existence d'une couleur unique pour chaque region*)
let unicite nbseeds=
  for ind=0 to nbseeds-1 do
     let l=[ind*10;ind*10+1;ind*10+2;ind*10+3] in
       for i = 0 to 3 do
    	   for j=i+1 to 3  do
             if (i!=j) then let a=(nth l i) and b=(nth l j) in
			           let ll=[(false,a);(false,b)] in gde_liste := append !gde_liste [ll];
	       done
       done
  done;;

(*cette fonction ajoute les DNF specifiant que deux regions adjacentes ont des couleurs differentes*)
let diff_couleurs ind1 ind2=
  for i=0 to 3 do
     gde_liste:= append !gde_liste [[(false, ind1*10+i);(false, ind2*10+i)]];
  done;;

(*cette fonction specifie les couleurs preremplies*)
let initial nbseeds v =
    for ind=0 to nbseeds-1 do
      let coul=v.seeds.(ind).c in
	        if (coul=Some blue) then gde_liste:= append !gde_liste  [[(true, ind*10+0)]]
		      else if (coul=Some yellow) then gde_liste:= append !gde_liste [[(true, ind*10+1)]]
          else if (coul=Some red) then gde_liste:= append !gde_liste [[(true, ind*10+2)]]
		      else if (coul=Some green) then gde_liste:= append !gde_liste [[(true, ind*10+3)]];
    done;;

(*cette fonction calcule une matrice d'adjacence symetrique*)
let calcul_matrice_adjacence_sym nbseeds matrice_adjacence matrice_adjacence_sym=
  for i=0 to nbseeds-1 do
    for j=0 to nbseeds-1 do
      if ( matrice_adjacence.(i).(j)=true || matrice_adjacence.(j).(i)=true) then
                                   	matrice_adjacence_sym.(i).(j)<-true
      else  matrice_adjacence_sym.(i).(j)<-false;
    done
  done;;

(* cette fonction repere les couleurs absentes du voronoi de depart*)
let voir_couleurs_absentes  couleurs_absentes  v  nbseeds =
     for i=0 to 3 do
        couleurs_absentes.(i)<-true;
     done;
     for j=0 to nbseeds-1 do
       if (v.seeds.(j).c = Some blue) then couleurs_absentes.(0)<-false
       else if (v.seeds.(j).c = Some yellow) then couleurs_absentes.(1)<-false
       else if (v.seeds.(j).c = Some red) then couleurs_absentes.(2)<-false
       else if (v.seeds.(j).c = Some green) then couleurs_absentes.(3)<-false;
     done;;

(* cette fonction impose les couleurs absentes du voronoi dans la CNF, qui ne doivent donc pas etre utilisees*)
let constituer_couleurs_absentes  couleurs_absentes nbseeds    =
   for ind=0 to nbseeds-1 do
      for i=0 to 3 do
          if (couleurs_absentes.(i)) then gde_liste:=append !gde_liste [[(false, ind*10+i)]]
      done
   done;;

(*cette fonction constitue la CNF que l'on va passer au sat solver*)
let calcul_grande_liste v nbseeds matrice_adjacence matrice_adjacence_sym couleurs_absentes=
  gde_liste:=[[]];
  existence nbseeds;
  unicite nbseeds;
  initial nbseeds v;
  constituer_couleurs_absentes  couleurs_absentes nbseeds; (*********)
  calcul_matrice_adjacence_sym nbseeds matrice_adjacence matrice_adjacence_sym;
  for i=0 to nbseeds-1 do
    for j=i+1 to nbseeds-1 do
      if (matrice_adjacence_sym.(i).(j)) then diff_couleurs i j;
    done
  done;;

(*on construite le module correspondant a nos variables dont aura besoin le sat solver*)
module Variables=struct
    type t=int
    let compare x y = if x>y then 1 else if x=y then 0 else -1
end;;

(*on construit le module sat solver*)
module Sat=Sat_solver.Make(Variables);;

exception Pas_de_solution of string;;

(*cette fonction lit le resultat du sat solver*)
let decodage_couleurs regions_couleurs = match (Sat.solve !gde_liste) with
    None -> raise (Pas_de_solution "--desole, le solveur n'a rien trouve ")
   |Some res -> let lire_couleurs (b,c)= if b then let region=c/10 and coul=c mod 10
                   in match coul with
             		      0-> regions_couleurs.(region) <- Some blue
		                  |1-> regions_couleurs.(region) <- Some yellow
		                  |2-> regions_couleurs.(region)<- Some red
		                  |3 -> regions_couleurs.(region) <- Some green;
	  in  map lire_couleurs res; ();;

(*cette fonction affiche le resultat trouve par le sat solver*)
let dessiner_solution param width height grille regions_couleurs=
 open_graph param ;
 auto_synchronize false;
 for i=0 to width-1 do
    for j=0 to height-1 do
         let ind=grille.(i).(j).indi in let c=regions_couleurs.(ind) in match c with
	                 None->set_color white;
                   |Some col->set_color col; let ()=
	                     if (noir i j grille width height) then set_color black; in plot i j;
     done
  done;
  synchronize () ;;

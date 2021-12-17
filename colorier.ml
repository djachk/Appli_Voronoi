(* Ce module coontient les fonctions permettant a l'utilisateur de colorier le voronoi choisi
*)

open Printf;;
open Graphics;;
open String;;
open List;;
open Voronoi;;

type point={mutable xp:int;mutable yp:int; mutable germ: seed; mutable coul: color option;mutable indi: int}
let point_neutre={xp=0;yp=0;germ={c=None; x=100; y=100};coul=None;indi=0}
let disteuclid p s= sqrt ((float(p.xp-s.x))**2. +. (float(p.yp-s.y))**2.);;
let taxicab p s=abs_float(float(p.xp-s.x)) +. abs_float(float(p.yp-s.y));;

(* cette fonction trouve le germe le plus proche d'un point en fonction de la distance choisie*)
let rec minseed p l dist=match l with
  [a]->a
  |x::q->let b= (minseed p q dist) in
	 if ((dist p x) < (dist p b)) then x else b;;
 
(* cette fonction initialise la grille avec un point de contenu quelconque pour le moment*)
let grille_init grille width height=
  for i=0 to width-1 do
   for j=0 to height-1 do
   grille.(i).(j)<-{xp=i;yp=j;germ={c=None; x=100; y=100};coul=None;indi=0};
   done
  done;;

(* cette fonction trouve l'indice d'un germe dans le tableau de germes du voronoi*)
let indice_seed s v nbseeds= let res=ref 0 in
   for i=0 to nbseeds-1 do
    if( s=v.seeds.(i)) then res:=i;
  done;
  !res;;

(*cette fonction cree la matrice des regions en affectant a chaque point un record contenant les informations
du germe dont ce point est le plus proche*)
let calcul_matrice_regions grille v width height nbseeds dist=
     for i=0 to width-1 do
      for j=0 to height-1 do
        let g=minseed grille.(i).(j) (Array.to_list v.seeds) dist in
          grille.(i).(j)<-{xp=i;yp=j;germ=g;coul=g.c;indi=(indice_seed g v nbseeds)};
      done
     done;;

let couleur_noire=Some black;;

(*cette fonction calcule si un point est a la frontiere entre deux regions*)
let noir i j grille width height= if ((i=0)||(i=width-1)||(j=0)||(j=height-1)) then false else
  ((grille.(i).(j).germ!=grille.(i-1).(j-1).germ)
                       && (grille.(i-1).(j-1).coul != couleur_noire))
            ||( (grille.(i).(j).germ!=grille.(i).(j-1).germ)
                       && (grille.(i).(j-1).coul != couleur_noire) )
            ||((grille.(i).(j).germ!=grille.(i+1).(j-1).germ)
                       && (grille.(i+1).(j-1).coul != couleur_noire))
            ||((grille.(i).(j).germ!=grille.(i-1).(j).germ)
                       && (grille.(i-1).(j).coul != couleur_noire))
            ||((grille.(i).(j).germ!=grille.(i+1).(j).germ)
                       && (grille.(i+1).(j).coul != couleur_noire))
            ||((grille.(i).(j).germ!=grille.(i-1).(j+1).germ)
                       && (grille.(i-1).(j+1).coul != couleur_noire))
            ||((grille.(i).(j).germ!=grille.(i).(j+1).germ)
                       && (grille.(i).(j+1).coul != couleur_noire))
            ||((grille.(i).(j).germ!=grille.(i+1).(j+1).germ)
                       && (grille.(i+1).(j+1).coul != couleur_noire));;

(*cette fonction dessine le voronoi en affichant les frontieres*)
let draw_voronoi grille width height=
for i=0 to width-1 do
  for j=0 to height-1 do
    let c=grille.(i).(j).coul in let () = match c with
	     None->set_color white;
      |Some col->set_color col; in let ()=
	  if (noir i j grille width height) then set_color black; in
    plot i j;
   done
  done;
synchronize ()
;;

(* cette fonction initialise les regions précoloriees*)
let regions_couleurs_init nbseeds v regions_couleurs=
  for i=0 to nbseeds-1 do
    regions_couleurs.(i)<-v.seeds.(i).c;
  done;;

(*cette fonction initialise le compteur de voisins entre regions a 0*)
let compteur_init nbseeds compteur_voisins=
  for i=0 to nbseeds-1 do
    for j=0 to nbseeds-1 do
       compteur_voisins.(i).(j)<-0;
    done
  done;;

(*cette fonction compte les voisins entre deux regions*)
let compter_voisins grille compteur_voisins width height=
  for i=0 to width-2 do
    for j=0 to height-2 do
      let a=grille.(i).(j) and b=grille.(i+1).(j) in
      if (a.germ!=b.germ) then compteur_voisins.(a.indi).(b.indi)<-
        compteur_voisins.(a.indi).(b.indi) +1;

      let a=grille.(i).(j) and b=grille.(i+1).(j+1) in
      if (a.germ!=b.germ) then compteur_voisins.(a.indi).(b.indi)<-
        compteur_voisins.(a.indi).(b.indi) +1;

      let a=grille.(i).(j) and b=grille.(i).(j+1) in
      if (a.germ!=b.germ) then compteur_voisins.(a.indi).(b.indi)<-
        compteur_voisins.(a.indi).(b.indi) +1;

    done
  done ;;

(*cette fonction construit la matrice d'adjacence en fonction du compteur de voisins entre deux regions*)
let calcul_matrice_adjacence nbseeds compteur_voisins matrice_adjacence=
  for i=0 to nbseeds-1 do
    for j=0 to nbseeds-1 do
      if compteur_voisins.(i).(j)>=2 then
        matrice_adjacence.(i).(j)<-true
      else  matrice_adjacence.(i).(j)<-false;
    done
  done;;

(*cette fonction calcule si une configuration est correcte*)
let config_ok nbseeds matrice_adjacence regions_couleurs= let res=ref true in
for i=0 to nbseeds-1 do
    for j=0 to nbseeds-1 do
      if  (matrice_adjacence.(i).(j)=true
                       && regions_couleurs.(i)=regions_couleurs.(j)) then res:=false;
    done;
done;
!res;;

(*cette fonction calcule si une configuration est completement coloriee*)
let config_pleine nbseeds regions_couleurs= let res=ref true in
  for i=0 to nbseeds-1 do
      if (regions_couleurs.(i)=None)  then res:=false;
  done;!res;;

(********************************************************)

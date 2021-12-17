(* ce module execute la logique du jeu
- l'utilisateur choisit un voronoi dans le fichier exemple, qui doit contenir
quatre voronoi designés v1, v2, v3, v4
- il choisit une distance (euclide ou taxicab)
- il colorie avec la souris
- on lui inique le resultat et on lui propose de reessayer ou bien d'avoir la solution
- on lui propose de rejouer avec un autre voronoi
*)

open Printf;;
open Graphics;;
open String;;
open List;;
open Voronoi;;
open Colorier;;
open Solution;;

let rec jeu ()=
  let ()=printf "Quel voronoi (un chiffre de 1 à 4)?\n" in
  let n=read_int() in let v=match n with
    1-> Examples.v1
    |2-> Examples.v2
    |3-> Examples.v3
    |4-> Examples.v4
    |_-> printf("choix incorrect, je prends 4\n"); Examples.v4  in
  let()=printf "Quelle distance (1 pour Euclide, 2 pour Taxicab)?\n" in
    let m=read_int() in let dist=match m with
    |1->disteuclid
    |2->taxicab   in
  let width=fst v.dim in
  let height=snd v.dim in
  let param=" "^string_of_int(width)^"x"^string_of_int(height) in
  let grille=Array.make_matrix width height point_neutre in
  let nbseeds=Array.length v.seeds in
  let compteur_voisins=Array.make_matrix nbseeds nbseeds 0 in
  let matrice_adjacence=Array.make_matrix nbseeds nbseeds false in
  let regions_couleurs=Array.make nbseeds None in
  let matrice_adjacence_sym=Array.make_matrix nbseeds nbseeds false in
  let couleurs_absentes=Array.make 4 true in   

  let rec rejouer() =
   begin
   try
    open_graph param;
    auto_synchronize false;
    grille_init grille width height;
    compteur_init nbseeds compteur_voisins;
    regions_couleurs_init nbseeds v regions_couleurs;
    voir_couleurs_absentes  couleurs_absentes  v  nbseeds; (******)
    calcul_matrice_regions grille v width height nbseeds dist;
    compter_voisins grille compteur_voisins width height;
    calcul_matrice_adjacence nbseeds compteur_voisins matrice_adjacence;
    draw_voronoi grille width height;
    try
      while (true) do
         let std=wait_next_event [Button_down] in
         let id=std.mouse_x in let jd=std.mouse_y in
         if (id>0&&id<width&&jd>0&&jd<height) then
             let cou=grille.(id).(jd).coul in
             let stu=wait_next_event [Button_up] in
             let iu=stu.mouse_x in let ju=stu.mouse_y in
             if (iu>0&&iu<width&&ju>0&&ju<height) then
                let gup=grille.(iu).(ju).germ in
                	for i=0 to width-1 do
	                  for j=0 to height-1 do
                      if (grille.(i).(j).germ=gup && grille.(i).(j).germ.c=None)
	                    then let ()=grille.(i).(j).coul<-cou;
                              in regions_couleurs.(grille.(i).(j).indi)<-cou;
                	  done
                  done;
              draw_voronoi grille width height;
              synchronize ();
              if (config_pleine nbseeds regions_couleurs) then raise Exit;
        done;
        with Exit->
            let message=if (config_ok nbseeds matrice_adjacence regions_couleurs)
            then "BRAVO!" else "RATE!" in
                   let ()=
                    set_color black;
                    set_text_size 30;
				            moveto (width/4) (height/2);
                    draw_string message;
				            moveto (width/4) (height/3);
                    draw_string("VOULEZ-VOUS REESSAYER?(o/n)");
                    synchronize ();    in
				                let st=wait_next_event[Key_pressed] in
				                close_graph ();
				                if (st.key='o') then rejouer ()
                        else let ()=
				                    calcul_grande_liste v nbseeds matrice_adjacence matrice_adjacence_sym couleurs_absentes;
                            decodage_couleurs regions_couleurs;
				                    dessiner_solution param width height grille regions_couleurs;
				                    set_color black;
				                    set_text_size 30;
				                    moveto (width/4) (height/4);
                            draw_string("TAPEZ UNE TOUCHE");
				                    synchronize ();   in
                               let st=wait_next_event[Key_pressed] in
					                       close_graph ()

  with Pas_de_solution(x)->print_endline x
  end
    in
    rejouer ();
    printf("voulez-vous rejouer? (o pour oui)\n");
	  let c= read_line() in
	  if c="o" then jeu ();
 ;;


jeu ();;

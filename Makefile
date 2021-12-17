all: jouer

jouer: voronoi.cmo examples.cmo sat_solver.cmo colorier.cmo solution.cmo jouer.cmo
	ocamlc -o jouer graphics.cma voronoi.cmo examples.cmo sat_solver.cmo colorier.cmo solution.cmo jouer.cmo

sat_solver.cmo: sat_solver.ml sat_solver.cmi
	ocamlc -c sat_solver.ml

sat_solver.cmi: sat_solver.mli
	ocamlc -c sat_solver.mli

voronoi.cmo: voronoi.ml
	ocamlc -c voronoi.ml

examples.cmo: examples.ml voronoi.cmo
	ocamlc -c examples.ml

colorier.cmo: colorier.ml examples.cmo
	ocamlc -c colorier.ml

solution.cmo: solution.ml examples.cmo
	ocamlc -c solution.ml

jouer.cmo: jouer.ml examples.cmo
	ocamlc -c jouer.ml

clear:
	rm *.cmo *.cmi jouer

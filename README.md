# Kettle
Kettle is a tea steeping timer written in ocaml.

### Requirements

You will need the following libraries:

* [OCaml][] >= 3.12.1

[OCaml]: http://caml.inria.fr/ocaml/release.en.html

### Compilation

To compile kettle, execute

   	$ make 
    
### Usage
	-list   Display the list of availabe types of tea
	-tea    Start the steeping for a specific type of tea
	-help   Display this list of options
	--help  Display this list of options
    
### Example
 	./kettle -tea black
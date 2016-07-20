# AJoCaml
Implementation of the aspect join calculus based on JoCaml 


You need Ocaml (version 4.02.1) and Jocaml (version 4.01.0), compiled with opt option.

To compile ajocaml: 
   run make in the root directory.

To execute the example:
   run (in different terminals)
    ./main.byte 
    ./client.byte 12345 
    ./client.byte 54321 
    ./aspect_repl.byte 12345
    ./aspect_repl.byte 54321

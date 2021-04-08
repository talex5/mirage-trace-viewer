type source = private string

val load : source -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val trace_files : source list Cmdliner.Term.t

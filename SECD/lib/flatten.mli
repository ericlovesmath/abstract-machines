(** Converts minimal lambda calculus to SECD instruction list *)
val flatten : Assign.t -> SECD.instr list

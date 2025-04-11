(** Converts minimal lambda calculus to SECD instruction list *)
val flatten : Assign.t -> Machine.instr list

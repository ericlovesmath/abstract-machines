#!/usr/bin/bash

# Passes
code="print_endline"
parse="Option.get @@ Intro.parse"
assign="Assign.assign_vars @@ $parse"
flatten="Flatten.flatten @@ $assign"
eval="SECD.string_of_value @@ SECD.eval @@ SECD.init @@ $flatten"

run_pass() {
    echo "--- PASS: $1 ---"
    echo ""
    echo "
        open Lib;;
        open $2;;
        $3 \"$CODE\";;
    " | dune utop | sed '1,3d;$d' | sed 's/Lib\.[a-zA-Z]*\.//g'
    echo ""
}

main() {
    case $1 in
        st) run_pass "String"                        "Intro"  "$code" ;;
        pa) run_pass "String -> AST"                 "Intro"  "$parse" ;;
        as) run_pass "AST -> Var to Loc"             "Assign"   "$assign" ;;
        fl) run_pass "Var to Loc -> Flattened"       "SECD"   "$flatten" ;;
        ev) run_pass "Flattened -> SECD Evaluation"  "SECD"   "$eval" ;;

        all) main "st" && main "pa" && main "as" && main "fl" && main "ev" ;;
        -h|--help) echo "Passes: st, pa, as, fl, ev, all" ;;
        *) echo "Unknown Pass, run -h for help"
    esac
}

show_help() {
    echo "Usage: $0 -p PASS [-s CODE_STRING | -f CODE_FILE]"
    echo "Pass options (-p):"
    echo "  st  : String"
    echo "  pa  : String -> AST"
    echo "  as  : AST -> Var to Loc"
    echo "  fl  : Var to Loc -> Flattened"
    echo "  ev  : Flattened -> SECD Evaluation"
    echo "  all : Run all passes"
}

while getopts "p:s:f:h" opt; do
  case $opt in
    p) PASS="$OPTARG" ;;
    s) CODE=$OPTARG ;;
    f) if [ ! -f "$OPTARG" ]; then
         echo "Error: File '$OPTARG' not found" >&2
         exit 1
       fi
       CODE=$(cat "$OPTARG") ;;
    h) show_help; exit 0 ;;
    *) echo "Invalid option: -$OPTARG" >&2; exit 1 ;;
  esac
done

if [ -z "$PASS" ]; then
    echo "Error: No pass specified. Use -p PASS" >&2
    show_help
    exit 1
fi

if [ -z "$CODE" ]; then
    echo "Error: Must provide code input using -s or -f" >&2
    show_help
    exit 1
fi

main $PASS

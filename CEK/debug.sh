#!/usr/bin/bash

# Passes
code="print_endline"
parse_general="Option.get @@ Intro.parse"
parse_cek="Parse.parse @@ $parse_general"
anf="Anf.anf @@ $parse_cek"
eval="CEK.string_of_value @@ CEK.eval @@ $anf"

run_pass() {
    echo "--- PASS: $1 ---"
    echo ""
    echo "
        open Lib;;
        open $2;;
        $3 \"$CODE\";;
    " | dune utop | sed '1,3d;$d'
    echo ""
}

main() {
    case $1 in
        st) run_pass "String"                 "Intro" "$code" ;;
        pg) run_pass "String -> General AST"  "Intro" "$parse_general" ;;
        pc) run_pass "General AST -> CEK AST" "CEK"   "$parse_cek" ;;
        an) run_pass "CEK AST -> ANF"         "CEK"   "$anf" ;;
        ev) run_pass "ANF -> CEK Evaluation"  "CEK"   "$eval" ;;

        all) main "st" && main "pg" && main "pc" && main "an" && main "ev" ;;
        -h|--help) echo "Passes: st, pg, pc, an, ev, all" ;;
        *) echo "Unknown Pass, run -h for help"
    esac
}

show_help() {
    echo "Usage: $0 -p PASS [-s CODE_STRING | -f CODE_FILE]"
    echo "Pass options (-p):"
    echo "  st  : String"
    echo "  pg  : String -> General AST"
    echo "  pc  : General AST -> CEK AST"
    echo "  an  : CEK AST -> ANF"
    echo "  ev  : ANF -> CEK Evaluation"
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

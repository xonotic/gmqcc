#!/bin/bash

expression_depth=5

randomselect_init() {
	randomselect_choice=
	randomselect_count=0
}
randomselect() {
	randomselect_count=$((randomselect_count + 1))
	if [ $((RANDOM % randomselect_count)) -eq 0 ]; then
		randomselect_choice=$*
	fi
}

# runtime expression:
# 1. variables section (needs whitespace)
# float var0;
# vector var1;
# ...
# 2. function creation (no whitespace)
# var0="a";var1=(!var0);
# ...
# 3.
# VARIABLES
# float is_ok() {
#   STATEMENTS
#   return varN == EXPRESSION
# }

# Thus we return:
# EXPRESSION STATEMENTS VARIABLES...
# a constant is:
# 42 var0=42; float var0
# 23 var1=23; float var1
# (42)+(23) var0=42;var1=23;var2=(var0+var1); float var0; float var1; float var2

NL="
"
TAB="	"

newvars() {
	varidx=0
}
nextvar() {
	varidx=$((varidx + 1))
	var=var$varidx
}
build_constant() {
	randomselect_init
	case "$1" in
		float)
			randomselect 0
			randomselect 1
			randomselect -1
			randomselect 3
			randomselect -1073741824
			randomselect 'f'
			;;
		vector)
			randomselect "'0 0 0'"
			randomselect "'0 1 0'"
			randomselect "'-1 0 0'"
			randomselect "'2 2 2'"
			randomselect "'3 2 -1'"
			randomselect "'16777216 -1073741824 0.0625'"
			randomselect 'v'
			;;
		string)
			randomselect '""'
			randomselect '"42"'
			randomselect 'string_null'
			randomselect 's'
			;;
	esac
	nextvar
	compiletime=$randomselect_choice
	runtime="CHECK($1, $var, $randomselect_choice, $randomselect_choice)"
	vars="$1 $var"
}

# Format: "compiletime runtime"
build_random_expression() {
	local depth type op expr1_runtime expr2_runtime expr1_compiletime expr2_compiletime expr1_vars expr2_vars
	depth=$1
	type=$2
	case "$depth" in
		0)
			build_constant "$type"
			return
			;;
	esac
	while :; do
		randomselect_init
		#randomselect "float ** float float"
		randomselect "float ! float"
		randomselect "float ~ float"
		randomselect "float + float"
		randomselect "float - float"
		randomselect "float ! vector"
		randomselect "vector ~ vector"
		randomselect "vector + vector"
		randomselect "vector - vector"
		randomselect "float ! string"
		randomselect "float * float float"
		for binop in '+' '-' '&' '^' '|'; do
			randomselect "float $binop float float"
			randomselect "vector $binop vector vector"
		done
		for binop in '*'; do
			randomselect "vector $binop vector float"
			randomselect "vector $binop float vector"
		done
		randomselect "float / float float"
		randomselect "vector / vector float"
		#randomselect "float % float float"
		randomselect "vector >< vector vector"
		randomselect "float >> float float"
		randomselect "float << float float"
		randomselect "float < float float"
		randomselect "float > float float"
		randomselect "float <=> float float"
		randomselect "float <= float float"
		randomselect "float >= float float"
		for type1 in float vector string; do
			randomselect "float == $type1 $type1"
		done
		for type1 in float vector string; do
			for type2 in float vector string; do
				randomselect "float || $type1 $type2"
				randomselect "float && $type1 $type2"
			done
		done
		thistype=${randomselect_choice%% *}
		if [ x"$type" = x"$thistype" ]; then
			case "$depth" in
				0)
					break
					;;
				*)
					case "$randomselect_choice" in
						*string*)
							;;
						*)
							break
							;;
					esac
					;;
			esac
		fi
	done
	randomselect_choice=${randomselect_choice#* }
	op=${randomselect_choice%% *}
	randomselect_choice=${randomselect_choice#* }
	case "$randomselect_choice" in
		*' '*)
			set -- $randomselect_choice
			build_random_expression "$((depth - 1))" "$1"
			expr1_compiletime=$compiletime
			expr1_runtime=$runtime
			expr1_vars=$vars
			build_random_expression "$((depth - 1))" "$2"
			expr2_compiletime=$compiletime
			expr2_runtime=$runtime
			expr2_vars=$vars
			nextvar
			case "$op" in
				'/'|'%')
					check="if(!${expr2_vars##* }) { print(\"DIVISION BY ZERO\\n\"); return; }$NL$TAB"
					;;
				*)
					check=
					;;
			esac
			compiletime="($expr1_compiletime)$op($expr2_compiletime)"
			runtime="$expr1_runtime;$NL$TAB$expr2_runtime;$NL$TAB$check""CHECK($type, $var, ${expr1_vars##* }$op${expr2_vars##* }, $compiletime)"
			vars="$expr1_vars;$NL$expr2_vars;$NL$type $var"
			;;
		*)
			build_random_expression "$((depth - 1))" "$randomselect_choice"
			nextvar
			compiletime="$op($compiletime)"
			runtime="$runtime;$NL$TAB""CHECK($type, $var, $op${vars##* }, $op($compiletime))"
			vars="$vars;$NL$type $var"
			;;
	esac
}

while :; do
	case "$((RANDOM % 2))" in
		0)
			conv=ftos
			type=float
			;;
		1)
			conv=vtos
			type=vector
			;;
	esac
	newvars
	build_random_expression 1 "$type"
	cat <<EOF >foo.qc
void print(...) = #1;
string ftos(float) = #2;
string vtos(vector) = #5;
float sqrt(float) = #13;
float floor(float) = #14;
noref string string_null;
var string s = "s";
var float f = 134217728;
var vector v = '-134217728 17 0.03125';
$vars;
void check_float(string var_name, string expr_short, string expr_long, float a, float b) {
	print(var_name, " = ", expr_short, "  // ", ftos(a), "\n");
	if (a != b)
		print(var_name, " != ", expr_long, "  // ", ftos(b), "\nFAIL\n");
}
void check_vector(string var_name, string expr_short, string expr_long, vector a, vector b) {
	print(var_name, " = ", expr_short, "  // ", vtos(a), "\n");
	if (a != b)
		print(var_name, " != ", expr_long, "  // ", vtos(b), "\nFAIL\n");
}
void check_string(string var_name, string expr_short, string expr_long, string a, string b) {
	print(var_name, " = ", expr_short, "  // ", a, "\n");
	if (a != b)
		print(var_name, " != ", expr_long, "  // ", b, "\nFAIL\n");
}
#define CHECK(type,var,expr_short,expr_long) \\
	var = (expr_short); \\
	check_##type(#var, #expr_short, #expr_long, var, expr_long)
void main() {
	$runtime;
}
EOF
	if ./gmqcc -std=gmqcc -fftepp -Wall foo.qc; then
		if ./qcvm progs.dat | tee /dev/stderr | grep FAIL >/dev/null; then
			nl foo.qc
			echo "Compiler is broken AGAIN."
			break
		fi
	else
		nl foo.qc
		echo "Compile fail. Maybe this script is broken?"
		break
	fi
done


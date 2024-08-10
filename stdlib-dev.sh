#!/usr/bin/env bash
###############################################################################
# 0. Compiling

(2>&- return 0) || {
	declare -A funcs=()
	compile=()
	compile() {
		declare add declare f func
		while read declare f func
		do [[ ! -v funcs[$func] ]] && funcs[$func]= && [[ -v add ]] && compile+=($func)
		done <<< $(declare -F)
	}
	compile
}

# 0. Compiling
###############################################################################
# 1. Error Handling and I/O

carry_status() { declare status=$?; "$@"; return "$status"; }

bool_status() {
	declare status=$?
	(( $# )) && { "$@"; status=$?; }
	(( status = !status ))
	return "$status"
}

error() { >&2 carry_status echo "$@"; }

errorf() { >&2 carry_status printf "$@"; }

print() { printf %s "$*"; }

println() { print "$*"$'\n'; }

eprint() { >&2 print "$*"; }

eprintln() { >&2 print "$*"$'\n'; }

readin() { IFS= read -rd '' "$@"; }

clearin() { declare REPLY; readin -t .001; }

sudo_write() { command sudo tee "$@" > /dev/null; }

sudo_append() { sudo_write --append "$@"; }

# 1. Error Handling and I/O
###############################################################################
# 2. Testing and Debugging

dryrun() {
	declare dryrun return
	if is_set dryrun
	then eprintln "${*@Q}"; return "${return:-0}";
	else "$@"
	fi
}

# 2. Testing and Debugging
###############################################################################
# 2. Variable Inspection and Manipulation

to_bool() {
	while (( $# ))
	do (( $1 = !!$1 )); shift
	done
}

to_arithmetic() {
	while (( $# ))
	do (( $1 = $1 )); shift
	done
}

attributes() {
	# indirect expansion avoids a subshell, but only works if the expanded
	# varname is set. declare -p works even if the varname is not set, but
	# requires a subshell
	if is_set "$1"
	then declare assignment=${!1@A}
	elif is_declared "$1"
	then declare assignment=$(declare -p "$1")
	else return
	fi
	declare declaration=(${assignment%%=*})
	(( ${#declaration[@]} == 1 )) || print "${declaration[1]##-}"
}

# Sometimes the user should know they've picked a bad varname. Just pick a
# varname that is not the argument to avoid illegal self-reference.
check_varname() {
	[[ $1 != '__' ]] && set -- "$1" __ || set -- "$1" ___
	declare -n "$2=$1"
}

# indirect expansion runs before the command it occurs in. run it in a separate
# command (in this case a function call) so errors can be suppressed.
test_indirect_expansion() { : ${!1}; }

# Reset temporary variables by passing this function's output to `eval`. `eval`
# is necessary to preserve attributes. None of the declaration commands can set
# non-export attributes of a variable in a non-global previous scope.
# TODO should the export attribute be cleared?
reset_temporary() {
	while (( $# ))
	do is_temporary "$1" && set -- "$1" "${1@A}" "${@:2}" && unset "$1" && print "$2;" && shift 2; shift
	done
}

# Once a function has shadowed a variable in a previous scope, it cannot
# restore the visibility of the variable in the previous scope by itself. But a
# subsequent function call can.
unshadow() { unset "$@"; }

# print a varname that is not set
newvar() {
	declare i
	while is_set var$i
	do (( ++i ))
	done
	print var$i
}

# This function resolves varnames if they are namerefs. It chooses an unset
# nameref name using `newvar` to avoid breaking recursive namerefs.
resolve_varname() {
	(( ! $# )) && return
	set -- "$@" "$(newvar)"
	while (( $# > 1 )); do
		! is_nameref "$1" && println "$1" || {
			declare -n "${!#}=$2"
			eval 'println "${!'"$1"'}"'
		}
		shift
	done
}

# use this function's output with `eval` to add assignment statements for
# varnames which are set. e.g. use it to add assignments for function calls
# that use `param` temporary-or-positional parameters.
assign_if_set() {
	while (( $# ))
	do is_set "$1" && print "$1=\$$1 "; shift
	done
}

# pass this function's output to `eval` to declare temporary-or-positional
# parameters. `eval` is necessary because functions cannot access positional
# parameters from previous scopes.
param() {
	declare param
	for param
	do println "declare $param; is_set $param || { (( \$# )) && $param=\$1 && shift; }"
	done
}

# pass this function's output to `eval` to run a command with
# assignment-or-positional parameters
with_params() {
	eval "$(param func)" || return
	declare params; is_set params || params='"$@"'
	assign_if_set "$@"
	print "$func $params"
}

# 2. Variable Inspection and Manipulation
###############################################################################
# 3. Tests
: << 'TESTS'
exists                      ()  [[            -a   $file    ]]
exists                      ()  [[            -e   $file    ]]
is_block_special            ()  [[            -b   $file    ]]
is_character_special        ()  [[            -c   $file    ]]
is_directory                ()  [[            -d   $file    ]]
is_file                     ()  [[            -f   $file    ]]
is_setgid                   ()  [[            -g   $file    ]] # set-group-id bit is set
is_symlink                  ()  [[            -h   $file    ]]
is_symlink                  ()  [[            -L   $file    ]]
is_sticky                   ()  [[            -k   $file    ]] # sticky bit is set
is_fifo                     ()  [[            -p   $file    ]] # is named pipe
is_readable                 ()  [[            -r   $file    ]]
is_nonzero_size             ()  [[            -s   $file    ]]
is_setuid                   ()  [[            -u   $file    ]] # set-user-id bit is set
is_writable                 ()  [[            -w   $file    ]]
is_executable               ()  [[            -x   $file    ]]
is_egid                     ()  [[            -G   $file    ]] # is owned by the effective group id
is_modified_since_last_read ()  [[            -N   $file    ]]
is_euid                     ()  [[            -O   $file    ]] # is owned by the effective user id
is_socket                   ()  [[            -S   $file    ]]
is_terminal                 ()  [[            -t   $fd      ]]
is_option_enabled           ()  [[            -o   $optname ]]
is_set                      ()  [[            -v   $varname ]]
is_nameref                  ()  [[            -R   $varname ]]
is_zero_len                 ()  [[            -z   $string  ]]
is_nonzero_len              ()  [[            -n   $string  ]]
is_nonzero_len              ()  [[                 $string  ]]
is_same_device_and_inode    ()  [[    $file1  -ef  $file2   ]]
is_newer                    ()  [[    $file1  -nt  $file2   ]]
is_older                    ()  [[    $file1  -ot  $file2   ]]
is_arithmetic_equal         ()  [[     expr1  -eq   expr2   ]]
is_arithmetic_equal         ()  ((     expr1   ==   expr2   ))
is_arithmetic_unequal       ()  [[     expr1  -ne   expr2   ]]
is_arithmetic_unequal       ()  ((     expr1   !=   expr2   ))
is_arithmetic_lesser        ()  [[     expr1  -lt   expr2   ]]
is_arithmetic_lesser        ()  ((     expr1   <    expr2   ))
is_arithmetic_greater       ()  [[     expr1  -gt   expr2   ]]
is_arithmetic_greater       ()  ((     expr1   >    expr2   ))
is_arithmetic_lesser_equal  ()  [[     expr1  -le   expr2   ]]
is_arithmetic_lesser_equal  ()  ((     expr1   <=   expr2   ))
is_arithmetic_greater_equal ()  [[     expr1  -ge   expr2   ]]
is_arithmetic_greater_equal ()  ((     expr1   >=   expr2   ))
is_integer_equal            () { [   "$int1"  -eq "$int2"   ]; }
is_integer_unequal          () { [   "$int1"  -ne "$int2"   ]; }
is_integer_lesser           () { [   "$int1"  -lt "$int2"   ]; }
is_integer_greater          () { [   "$int1"  -gt "$int2"   ]; }
is_integer_lesser_equal     () { [   "$int1"  -le "$int2"   ]; }
is_integer_greater_equal    () { [   "$int1"  -ge "$int2"   ]; }
is_string_equal             () { [   "$str1"   == "$str2"   ]; }
is_string_equal             () { [   "$str1"    = "$str2"   ]; }
is_string_equal             ()  [[    $str1    == "$str2"   ]]
is_string_equal             ()  [[    $str1     = "$str2"   ]]
is_string_unequal           ()  [[    $str1    != "$str2"   ]]
is_string_unequal           () { [   "$str1"   != "$str2"   ]; }
is_ascii_lesser             () { [   "$str1"  '<' "$str2"   ]; }
is_ascii_greater            () { [   "$str1"  '>' "$str2"   ]; }
is_lexicographic_lesser     ()  [[    $str1    <   $str2    ]]
is_lexicographic_greater    ()  [[    $str1    >   $str2    ]]
is_string_match             ()  [[    $str1    =~ "$str2"   ]]
is_match                    ()  [[    $string  ==  $pattern ]]
is_not_match                ()  [[    $string  !=  $pattern ]]
is_regex_match              ()  [[    $string  =~  $regex   ]]
TESTS

_stdlib_tests() {

declare -A for=(
	'is_executable file' '[[ -x $file ]]'
	'is_command command' '>&- command -v "$command"'
	'is_bool      value' '[[ $value == [01] ]]'

	'is_numeric   value' '[[ $value && $value != *[!0-9]* ]]'

	# A value is an integer if it passes an integer equality test.
	'is_integer integer' '2>&- [ "$integer" -eq "$integer" ]'

	# A value is a varname if it can be assigned to a nameref. the
	# nameref name is the varname with a prefix to avoid `self
	# references not allowed`. Shadowing and circular references
	# don't matter.
	'is_varname varname' '2>&- declare -n "_$varname=$varname"'

	'is_declared varname' 'declare -p "$varname" &> /dev/null'

	# A value is a parameter if it is a variable name, number (positional
	# parameter), or special parameter character. This can be checked using
	# indirect expansion.
	'is_parameter parameter' '2>&- test_indirect_expansion "$parameter"'

	# A name is a path command if it is found by path search and is executable.
	# Doesn't -P imply -p? I can't find a counter-example...
	'is_path_command name' 'is_executable "$(type -Pp "$name")"'
)

declare -A while=(
	is_set         '[[ -v $1 ]]'
	is_nameref     '[[ -R $1 ]]'
	is_associative '[[ $(attributes "$1") == *A* ]]'
        is_indexed     '[[ $(attributes "$1") == *a* ]]'
        is_arithmetic  '[[ $(attributes "$1") == *i* ]]'

	# A variable is temporary if it is set both before and after being
	# declared. Temporary variables are created by assignment statements in
	# a function call command. They are inherited by subsequent function
	# calls, and cannot be shadowed, until they are unset. See
	# `reset_temporary` for a technique to deal with this.
	is_temporary   '[[ -v $1 ]] && declare "$1" && [[ -v $1 ]]'

	# execute assignment statements. `export` is the only
	# declaration command that can assign variables in non-global
	# previous scopes. `export` can only set or unset the export
	# attribute, not leave it unmodified, so this checks the
	# attribute. assignments can also simply be passed to `eval`,
	# but this function prevents code injection.
	'assign' 'if [[ $(attributes "${1%%=*}") == *x* ]]; then export -- "$1"; else export -n -- "$1"; fi'
)

# variable indirect expansion is automatic for arithmetic expressions.
#
#     $ ! declare -p b && a=b && (( a )) && echo unreachable
#     bash: declare: b: not found
#
# watch out for assignment operators. `b` gets assigned globally.
#
#     $ ! declare -p b && a=--b && (( a )) && declare -p b
#     bash: declare: b: not found
#     declare -- b="-1"
#
# use `test` / `[` for safe integer comparison.

declare -A conditional=(
	is_lexicographic_lesser  '<'
	is_lexicographic_greater '>'
	is_arithmetic_lesser    '-lt'
	is_arithmetic_greater   '-gt'
)

declare -A test=(
	is_ascii_lesser   "'<'"
	is_ascii_greater  "'>'"
	is_integer_lesser  -lt
	is_integer_greater -gt
)

declare funcname command metavar operator declaration
declare eval='eval "$declaration" || error "error $funcname"'

for funcname in "${!for[@]}"; do
	command=${for[$funcname]}
	funcname=($funcname)
	metavar=${funcname[1]}
	declaration="$funcname() { declare $metavar; for $metavar; do $command || return; done; }"
	eval "$eval"
done

for funcname in "${!while[@]}"; do
	command=${while[$funcname]}
	declaration=$funcname'() { while (( $# )); do '$command' && shift || return; done; }'
	eval "$eval"
done

for funcname in "${!conditional[@]}"; do
	operator=${conditional[$funcname]}
	declaration=$funcname'() [[ $1 '$operator' $2 ]]'
	eval "$eval"
done

for funcname in "${!test[@]}"; do
	operator=${test[$funcname]}
	declaration=$funcname'() { [ "$1" '$operator' "$2" ]; }'
	eval "$eval"
done

} && _stdlib_tests && unset -f _stdlib_tests

# 3. Tests
###############################################################################
# 4. Math

log2ceil() {
	declare n log2=0
	for (( n = $1 - 1; n > 0; n >>= 1, ++log2 )); do :; done
	(( n + 1 > 0 )) && echo $log2
}

log2floor() {
	declare n log2=0
	for (( n = $1; n > 1; n >>= 1, ++log2 )); do :; done
	(( n > 0 )) && echo $log2
}

# 4. Math
###############################################################################
# 4. Utilities

timestamp() { date --utc +%Y%m%dT%H%M%SZ; }

download() { wget --output-document=- "$@"; }

tempdownload() {
	declare file=$(mktemp)
	download "$@" > "$file" &&
	print "$file" ||
	carry_status rm -- "$file"
}

tempfd() {
	declare r w temp=$(mktemp)
	exec {r}< "$temp" {w}> "$temp"
	rm -- "$temp"
	set -- $r $w "$@"
	unshadow r w temp
	assign "$3=$1" "$4=$2"
}

ask() {
	# may only be specified as temporary variables, e.g. `default=yes ask`
	declare default echo hide

	eval "$(param prompt)"
	declare replies=("$@")
	declare reply index read=(read -er)
	(( hide )) && read+=(-s)

	if (( echo ))
	then [[ $default ]] && prompt="${prompt:+$prompt }[$default]: "
	else
		(( ${#replies[@]} )) || replies=(yes no)
		is_set default && index=$(array_index replies "$default") && {
			reply=${replies[index]}
			replies[index]=$(style bold underline)$reply$(style) # TODO add color and style
		} || unset default
		prompt="${prompt:+$prompt }($(array_join replies /)): "
		is_set default && replies[index]=$reply
	fi

	while true; do
		"${read[@]}" -p "$prompt" reply
		(( hide )) && [[ $prompt ]] && error
		[[ $reply ]] || { is_set default && reply=$default; } || continue
		(( echo )) && print "$reply" && return
		index=$(array_index replies "$reply") && return "$index"
		error "Unrecognized reply ${reply@Q}: reply one of ${replies[*]@Q}"
	done
}

hidden() {
	eval "$(param value)" || return
	eval "$(param  show)" || show=4
	declare len=${#value} hidden=
	(( show < 0 )) && show=0
	(( show > len )) && show=len
	while (( show++ < len ))
	do hidden+=*; value=${value:1}
	done
	print "$hidden$value"
}

owner_user() {
	eval "$(param path)"
	until [[ ! $path ]] || stat --format=%U "$path" 2> /dev/null
	do path=$(dirname "$path")
	done
}

sudo_user() {
	eval "$(param user)"
	if [[ ! $user || $user == "$USER" ]]
	then "$@"
	else
		is_numeric "$user" && user="#$user"
		sudo --user "$user" "$@"
	fi
}

sudo_owner() {
	eval "$(param path)"
	user=$(owner_user "$path") sudo_user "$@"
}

# Sadly, a subshell is unavoidable because herestrings unavoidably add a
# newline.
owner_write() {
	declare input path
	readin input
	for path
	do print "$input" | sudo_owner "$path" tee -- "$path" > /dev/null
	done
}

owner_append() {
	declare input path
	readin input
	for path
	do print "$input" | sudo_owner "$path" tee --append -- "$path" > /dev/null
	done
}

public_ip() {
	declare ip inet dig=() wget=()
	[[ ${inet:=$1} ]] && {
		declare -A t=([-4]=A [-6]=AAAA)
		[[ -v t[$inet] ]] || {
			error "invalid inet ${inet@Q}: pick one of ${!t[*]}"
			return
		}
		dig=("$inet" -t "${t[$inet]}")
		wget=("$inet")
	}
	# prefer opendns.com, fallback ifconfig.me
	{ ip=$(dig @resolver1.opendns.com -q myip.opendns.com "${dig[@]}" +short) && [[ $ip ]]; } ||
	{ ip=$(download "${wget[@]}" ifconfig.me) && [[ $ip ]]; } &&
	print "$ip"
}

get_redirect_url() {
	declare url curl=(
		curl
		--location
		--proto-default http
		--proto -all,https,+http
		--fail # 4xx, 5xx
		--silent
		--output /dev/null
		--write-out '%{url_effective}'
		--url "$1"
	)
	apt_path_dep curl curl &&
	# prefer HEAD, fallback GET
	{ url=$("${curl[@]}" --head) || url=$("${curl[@]}"); } && print "$url"
}

github_latest_tag() {
	eval "$(param repo)" || return
	declare redirect tag
	redirect=$(get_redirect_url "https://github.com/$repo/releases/latest") &&
	tag=${redirect##*/} &&
	[[ $tag != 'latest' ]] &&
	print "$tag" ||
	error "Could not resolve latest tag for GitHub repo ${repo@Q}"
}

dpkg_version() {
	declare package
	for package
	do dpkg-query --showformat='${Version}' --show "$package" && print ' '
	done
}

pin_package_path() {
	eval "$(param package)" &&
	print "/etc/apt/preferences.d/$package.pref"
}

pin_package_version() {
	eval "$(param package)" || return
	eval "$(param version)" || return
	sudo_write "$(pin_package_path "$package")" <<- EOF
	Package: $package
	Pin: version $version
	Pin-Priority: 1001
	EOF
}

unpin_package() {
	eval "$(param package)" || return
	declare path=$(pin_package_path "$package")
	[[ ! -f $path ]] || sudo rm -- "$path"
}

# 4. Utilities
###############################################################################
# 3. Positional parameter inspection and manipulation

maxlen() {
	declare value maxlen=0
	for value
	do (( maxlen < ${#value} )) && maxlen=${#value}
	done
	print "$maxlen"
}

index() {
	eval "$(param match)" || return
	declare value i=0
	for value
	do [[ $value != "$match" ]] && (( ++i )) || break
	done
	(( i < $# )) && print $i
}

contains() {
	eval "$(param match)" &&
	index "$match" "$@" > /dev/null
}

join() {
	eval "$(param sep)"
	declare value join=
	for value
	do join+=$value$sep
	done
	print "${join%$sep}"
}

# 3. Positional parameter inspection and manipulation
###############################################################################
# 4. Array inspection

array_maxlen() {
	eval "$(param array)" || return
	set -- "$array" "$(newvar)"
	unshadow array
	declare -n "$2=$1" || return
	eval 'maxlen "${'$2'[@]}"'
}

array_index() {
	eval "$(param array)" || return
	eval "$(param match)" || return
	set -- "$array" "$(newvar)" "$match"
	unshadow array match
	declare -n "$2=$1" || return
	eval 'set -- "${!'$2'[@]}" $2 "$3"'
	while (( $# > 2 ))
	do eval '[[ ${!#} == "${'${@: -2:1}'[$1]}" ]]' && print "$1" && return || shift
	done
	return 1
}

array_contains() {
	eval "$(param array)" || return
	set -- "$@" "$array"
	unshadow array
	while (( $# > 1 ))
	do array_index "${!#}" "$1" > /dev/null && shift || return
	done
}

array_contains_any() {
	eval "$(param array)" && check_varname "$array" || return
	set -- "$@" "$array"
	unshadow array
	while (( $# > 1 ))
	do array_index "${!#}" "$1" > /dev/null && return || shift
	done
	return 1
}

# 4. Array inspection
###############################################################################
# 4. Array manipulation

array_join() {
	eval "$(param array)" && check_varname "$array" || return
	eval "$(param sep)"
	set -- "$sep" "$array"
	unshadow array sep
	eval 'join "$1" "${'$2'[@]}"'
}

array_remove() {
	eval "$(param array)" && check_varname "$array" || return
	set -- "$@" "$array"
	unshadow array
	while (( $# > 1 ))
	do set -- "$(array_index "${!#}" "$1")" "${@:2}"; [[ $1 ]] && unset "${!#}[$1]"; shift
	done
}

array_prepend() {
	# Not valid for associative arrays. Destructively re-sequences indexed
	# arrays.
	#
	# TODO do not destructively resequence indices if the minimum index is
	# greater or equal to the positional parameter count
	# TODO collision-free
	declare -n _array=$1; shift
	_array=("$@" "${_array[@]}")
}

array_reverse() {
	# TODO reverse associative array according to sorted indices, like pop
	# TODO param array
	is_varname "$1" || return
	eval 'declare array=("${'"$1"'[@]}")'
	declare swap i=0 j=${#array[@]} k
	while (( k = j - 1 - i, i < j / 2 ))
	do swap=${array[i]} array[i++]=${array[k]} array[k]=$swap
	done
	set -- "$1" "${array[@]}"
	unshadow array swap i j k
	eval "$1"'=("${@:2}")'
}

array_sort() {
	eval "$(param array)" && check_varname "$array" || return
	set -- "$array" "$@"

	# if there are positional arguments, sort those. otherwise sort the
	# array.
	if (( $# > 1 ))
	then array=("${@:2}")
	else unshadow array; eval 'declare array=("${'"$1"'[@]}")'
	fi

	# the comparison command to use
	declare compare
	is_command "${compare:=is_lexicographic_lesser}" || {
		error "compare command ${compare@Q} not found"
		return
	}

	declare vars=(queue beg end lt gt pivot i)
	declare "${vars[@]}"
	vars+=(array compare vars)

	(( ${#array[@]} )) && {
		# iterative quicksort: https://stackoverflow.com/a/30576368
		queue=()
		queue_push queue "$(( ${#array[@]} - 1 ))" 0
		while (( ${#queue[@]} )); do
			queue_pop queue beg
			queue_pop queue end
			lt=() gt=() pivot=${array[beg]}

			for (( i = beg + 1; i <= end; ++i )); do
				if "$compare" "${array[i]}" "$pivot" 2> /dev/null # suppress compare errors
				then lt+=("${array[i]}")
				else gt+=("${array[i]}")
				fi
			done

			(( ${#lt[@]} >= 2 )) && queue_push queue        "$(( beg + ${#lt[@]} - 1 ))" "$beg"
			(( ${#gt[@]} >= 2 )) && queue_push queue "$end" "$(( end - ${#gt[@]} + 1 ))"

			array=("${array[@]:0:beg}" "${lt[@]}" "$pivot" "${gt[@]}" "${array[@]:end+1}")
		done
	}

	set -- "$1" "${array[@]}"
	unshadow "${vars[@]}"
	if [[ $1 ]] && ! is_associative "$1"
	then eval "$1"'=("${@:2}")'
	else shift; declare array=("$@"); declare -p array
	fi
}

queue_push() {
	is_varname "$1" || return
	declare items=("${@:2}")
	array_reverse items
	set -- "$1" "${items[@]}"
	unshadow items
	eval "$1"'+=("${@:2}")'
}

stack_push() {
	is_varname "$1" || return
	eval "$1"'+=("${@:2}")'
}

_stdlib_array() {

declare qs1 qs2
readin qs1 << 'EOF'
	is_varname "$1" || return
	[[ $2 ]] && { is_varname "$2" || return; }
	is_associative "$1" && set -- "$1" "$2" '' || set -- "$1" "$2"
	eval 'declare indices=("${!'"$1"'[@]}") i'
	(( ${#indices[@]} )) || return
	(( $# == 3 )) && {
		declare compare
		compare=$compare array=indices array_sort
		unshadow compare
	}
EOF
readin qs2 << 'EOF'
	set -- "$1" "$2" "${indices[i]}"
	unshadow indices i
	eval 'declare value=("${'"$1"'[$3]}")'
	set -- "$1" "$2" "$3" "$value"
	unshadow value
	if [[ $2 ]]
	then eval "$2=$4"
	else print "$4"
	fi
EOF

declare -A functions=(
	# NOTE For associative arrays, queue_pop and stack_pop function
	# according to ascending sorted key order and accept the `compare`
	# temporary variable to control sorting behaviour. Tip: for descending
	# key order, swap queue and stack.
	queue_peek '{ '"$qs1"' (( i =                  0 )); '"$qs2"' }'
	stack_peek '{ '"$qs1"' (( i = ${#indices[@]} - 1 )); '"$qs2"' }'
	queue_pop  '{ '"$qs1"' (( i =                  0 )); '"$qs2"' unset "$1[$3]"; }'
	stack_pop  '{ '"$qs1"' (( i = ${#indices[@]} - 1 )); '"$qs2"' unset "$1[$3]"; }'
)
declare funcname declaration
for funcname in "${!functions[@]}"; do
	declaration="$funcname() ${functions[$funcname]}"
	eval "$declaration" || error "error $funcname"
done

} && _stdlib_array && unset -f _stdlib_array

# 4. Array manipulation
###############################################################################
# 4. PATH inspection and manipulation

path_index() {
	declare IFS=:
	declare paths=($PATH)
	array_index paths "$1"
}

path_remove() {
	declare IFS=:
	declare paths=($PATH)
	array_remove paths "$@"
	PATH=${paths[*]}
}

path_contains() {
	path_index "$1" > /dev/null
}

path_prepend() {
	path_remove "$@"
	declare path
	for path
	do PATH=$path:$PATH
	done
}

path_append() {
	path_remove "$@"
	declare path
	for path
	do PATH+=:$path
	done
}

# 4. PATH inspection and manipulation
###############################################################################
# 0. Compiling

(2>&- return 0) || {
	add= compile

	declare libr libw
	tempfd libr libw

	# source guard
	>& $libw println 'declare -p _stdlib &> /dev/null && return || declare -r _stdlib'

	# functions
	for func in ${compile[@]}
	do >& $libw declare -f $func
	done

	# TODO maybe declare -p _stdlib_ansi for color/style

	declare stdlib=$(dirname "$0")/stdlib.sh
	> "$stdlib"

	# strip trailing space
	while IFS= read -r line
	do println "${line% }" >> "$stdlib"
	done <& $libr

	exec {libr}<&- {libw}>&-
}

# 0. Compiling
###############################################################################

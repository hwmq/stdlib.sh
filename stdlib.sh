declare -p _stdlib &> /dev/null && return || declare -r _stdlib
array_contains ()
{
    eval "$(param array)" || return;
    set -- "$@" "$array";
    unshadow array;
    while (( $# > 1 )); do
        array_index "${!#}" "$1" > /dev/null && shift || return;
    done
}
array_contains_any ()
{
    eval "$(param array)" && check_varname "$array" || return;
    set -- "$@" "$array";
    unshadow array;
    while (( $# > 1 )); do
        array_index "${!#}" "$1" > /dev/null && return || shift;
    done;
    return 1
}
array_index ()
{
    eval "$(param array)" || return;
    eval "$(param match)" || return;
    set -- "$array" "$(newvar)" "$match";
    unshadow array match;
    declare -n "$2=$1" || return;
    eval 'set -- "${!'$2'[@]}" $2 "$3"';
    while (( $# > 2 )); do
        eval '[[ ${!#} == "${'${@: -2:1}'[$1]}" ]]' && print "$1" && return || shift;
    done;
    return 1
}
array_join ()
{
    eval "$(param array)" && check_varname "$array" || return;
    eval "$(param sep)";
    set -- "$sep" "$array";
    unshadow array sep;
    eval 'join "$1" "${'$2'[@]}"'
}
array_maxlen ()
{
    eval "$(param array)" || return;
    set -- "$array" "$(newvar)";
    unshadow array;
    declare -n "$2=$1" || return;
    eval 'maxlen "${'$2'[@]}"'
}
array_prepend ()
{
    declare -n _array=$1;
    shift;
    _array=("$@" "${_array[@]}")
}
array_remove ()
{
    eval "$(param array)" && check_varname "$array" || return;
    set -- "$@" "$array";
    unshadow array;
    while (( $# > 1 )); do
        set -- "$(array_index "${!#}" "$1")" "${@:2}";
        [[ -n $1 ]] && unset "${!#}[$1]";
        shift;
    done
}
array_reverse ()
{
    is_varname "$1" || return;
    eval 'declare array=("${'"$1"'[@]}")';
    declare swap i=0 j=${#array[@]} k;
    while (( k = j - 1 - i, i < j / 2 )); do
        swap=${array[i]} array[i++]=${array[k]} array[k]=$swap;
    done;
    set -- "$1" "${array[@]}";
    unshadow array swap i j k;
    eval "$1"'=("${@:2}")'
}
array_sort ()
{
    eval "$(param array)" && check_varname "$array" || return;
    set -- "$array" "$@";
    if (( $# > 1 )); then
        array=("${@:2}");
    else
        unshadow array;
        eval 'declare array=("${'"$1"'[@]}")';
    fi;
    declare compare;
    is_command "${compare:=is_lexicographic_lesser}" || {
        error "compare command ${compare@Q} not found";
        return
    };
    declare vars=(queue beg end lt gt pivot i);
    declare "${vars[@]}";
    vars+=(array compare vars);
    (( ${#array[@]} )) && {
        queue=();
        queue_push queue "$(( ${#array[@]} - 1 ))" 0;
        while (( ${#queue[@]} )); do
            queue_pop queue beg;
            queue_pop queue end;
            lt=() gt=() pivot=${array[beg]};
            for ((i = beg + 1; i <= end; ++i ))
            do
                if "$compare" "${array[i]}" "$pivot" 2> /dev/null; then
                    lt+=("${array[i]}");
                else
                    gt+=("${array[i]}");
                fi;
            done;
            (( ${#lt[@]} >= 2 )) && queue_push queue "$(( beg + ${#lt[@]} - 1 ))" "$beg";
            (( ${#gt[@]} >= 2 )) && queue_push queue "$end" "$(( end - ${#gt[@]} + 1 ))";
            array=("${array[@]:0:beg}" "${lt[@]}" "$pivot" "${gt[@]}" "${array[@]:end+1}");
        done
    };
    set -- "$1" "${array[@]}";
    unshadow "${vars[@]}";
    if [[ -n $1 ]] && ! is_associative "$1"; then
        eval "$1"'=("${@:2}")';
    else
        shift;
        declare array=("$@");
        declare -p array;
    fi
}
ask ()
{
    declare default echo hide;
    eval "$(param prompt)";
    declare replies=("$@");
    declare reply index read=(read -er);
    (( hide )) && read+=(-s);
    if (( echo )); then
        [[ -n $default ]] && prompt="${prompt:+$prompt }[$default]: ";
    else
        (( ${#replies[@]} )) || replies=(yes no);
        is_set default && index=$(array_index replies "$default") && {
            reply=${replies[index]};
            replies[index]=$(style bold underline)$reply$(style)
        } || unset default;
        prompt="${prompt:+$prompt }($(array_join replies /)): ";
        is_set default && replies[index]=$reply;
    fi;
    while true; do
        "${read[@]}" -p "$prompt" reply;
        (( hide )) && [[ -n $prompt ]] && error;
        [[ -n $reply ]] || {
            is_set default && reply=$default
        } || continue;
        (( echo )) && print "$reply" && return;
        index=$(array_index replies "$reply") && return "$index";
        error "Unrecognized reply ${reply@Q}: reply one of ${replies[*]@Q}";
    done
}
assign ()
{
    while (( $# )); do
        if [[ $(attributes "${1%%=*}") == *x* ]]; then
            export -- "$1";
        else
            export -n -- "$1";
        fi && shift || return;
    done
}
assign_if_set ()
{
    while (( $# )); do
        is_set "$1" && print "$1=\$$1 ";
        shift;
    done
}
attributes ()
{
    if is_set "$1"; then
        declare assignment=${!1@A};
    else
        if is_declared "$1"; then
            declare assignment=$(declare -p "$1");
        else
            return;
        fi;
    fi;
    declare declaration=(${assignment%%=*});
    (( ${#declaration[@]} == 1 )) || print "${declaration[1]##-}"
}
bool_status ()
{
    declare status=$?;
    (( $# )) && {
        "$@";
        status=$?
    };
    (( status = !status ));
    return "$status"
}
carry_status ()
{
    declare status=$?;
    "$@";
    return "$status"
}
check_varname ()
{
    [[ $1 != '__' ]] && set -- "$1" __ || set -- "$1" ___;
    declare -n "$2=$1"
}
clearin ()
{
    declare REPLY;
    readin -t .001
}
contains ()
{
    eval "$(param match)" && index "$match" "$@" > /dev/null
}
download ()
{
    wget --output-document=- "$@"
}
dpkg_version ()
{
    declare package;
    for package in "$@";
    do
        dpkg-query --showformat='${Version}' --show "$package" && print ' ';
    done
}
dryrun ()
{
    declare dryrun return;
    if is_set dryrun; then
        eprintln "${*@Q}";
        return "${return:-0}";
    else
        "$@";
    fi
}
eprint ()
{
    print "$*" 1>&2
}
eprintln ()
{
    print "$*"'
' 1>&2
}
error ()
{
    carry_status echo "$@" 1>&2
}
errorf ()
{
    carry_status printf "$@" 1>&2
}
get_redirect_url ()
{
    declare url curl=(curl --location --proto-default http --proto -all,https,+http --fail --silent --output /dev/null --write-out '%{url_effective}' --url "$1");
    apt_path_dep curl curl && {
        url=$("${curl[@]}" --head) || url=$("${curl[@]}")
    } && print "$url"
}
github_latest_tag ()
{
    eval "$(param repo)" || return;
    declare redirect tag;
    redirect=$(get_redirect_url "https://github.com/$repo/releases/latest") && tag=${redirect##*/} && [[ $tag != 'latest' ]] && print "$tag" || error "Could not resolve latest tag for GitHub repo ${repo@Q}"
}
hidden ()
{
    eval "$(param value)" || return;
    eval "$(param  show)" || show=4;
    declare len=${#value} hidden=;
    (( show < 0 )) && show=0;
    (( show > len )) && show=len;
    while (( show++ < len )); do
        hidden+=*;
        value=${value:1};
    done;
    print "$hidden$value"
}
index ()
{
    eval "$(param match)" || return;
    declare value i=0;
    for value in "$@";
    do
        [[ $value != "$match" ]] && (( ++i )) || break;
    done;
    (( i < $# )) && print $i
}
is_arithmetic ()
{
    while (( $# )); do
        [[ $(attributes "$1") == *i* ]] && shift || return;
    done
}
is_arithmetic_greater ()
{
    [[ $1 -gt $2 ]]
}
is_arithmetic_lesser ()
{
    [[ $1 -lt $2 ]]
}
is_ascii_greater ()
{
    [ "$1" '>' "$2" ]
}
is_ascii_lesser ()
{
    [ "$1" '<' "$2" ]
}
is_associative ()
{
    while (( $# )); do
        [[ $(attributes "$1") == *A* ]] && shift || return;
    done
}
is_bool ()
{
    declare value;
    for value in "$@";
    do
        [[ $value == [01] ]] || return;
    done
}
is_command ()
{
    declare command;
    for command in "$@";
    do
        command -v "$command" 1>&- || return;
    done
}
is_declared ()
{
    declare varname;
    for varname in "$@";
    do
        declare -p "$varname" &> /dev/null || return;
    done
}
is_executable ()
{
    declare file;
    for file in "$@";
    do
        [[ -x $file ]] || return;
    done
}
is_indexed ()
{
    while (( $# )); do
        [[ $(attributes "$1") == *a* ]] && shift || return;
    done
}
is_integer ()
{
    declare integer;
    for integer in "$@";
    do
        [ "$integer" -eq "$integer" ] 2>&- || return;
    done
}
is_integer_greater ()
{
    [ "$1" -gt "$2" ]
}
is_integer_lesser ()
{
    [ "$1" -lt "$2" ]
}
is_lexicographic_greater ()
{
    [[ $1 > $2 ]]
}
is_lexicographic_lesser ()
{
    [[ $1 < $2 ]]
}
is_nameref ()
{
    while (( $# )); do
        [[ -R $1 ]] && shift || return;
    done
}
is_numeric ()
{
    declare value;
    for value in "$@";
    do
        [[ -n $value && $value != *[!0-9]* ]] || return;
    done
}
is_parameter ()
{
    declare parameter;
    for parameter in "$@";
    do
        test_indirect_expansion "$parameter" 2>&- || return;
    done
}
is_path_command ()
{
    declare name;
    for name in "$@";
    do
        is_executable "$(type -Pp "$name")" || return;
    done
}
is_set ()
{
    while (( $# )); do
        [[ -v $1 ]] && shift || return;
    done
}
is_temporary ()
{
    while (( $# )); do
        [[ -v $1 ]] && declare "$1" && [[ -v $1 ]] && shift || return;
    done
}
is_varname ()
{
    declare varname;
    for varname in "$@";
    do
        declare -n "_$varname=$varname" 2>&- || return;
    done
}
join ()
{
    eval "$(param sep)";
    declare value join=;
    for value in "$@";
    do
        join+=$value$sep;
    done;
    print "${join%$sep}"
}
log2ceil ()
{
    declare n log2=0;
    for ((n = $1 - 1; n > 0; n >>= 1, ++log2 ))
    do
        :;
    done;
    (( n + 1 > 0 )) && echo $log2
}
log2floor ()
{
    declare n log2=0;
    for ((n = $1; n > 1; n >>= 1, ++log2 ))
    do
        :;
    done;
    (( n > 0 )) && echo $log2
}
maxlen ()
{
    declare value maxlen=0;
    for value in "$@";
    do
        (( maxlen < ${#value} )) && maxlen=${#value};
    done;
    print "$maxlen"
}
newvar ()
{
    declare i;
    while is_set var$i; do
        (( ++i ));
    done;
    print var$i
}
owner_append ()
{
    declare input path;
    readin input;
    for path in "$@";
    do
        print "$input" | sudo_owner "$path" tee --append -- "$path" > /dev/null;
    done
}
owner_user ()
{
    eval "$(param path)";
    until [[ ! -n $path ]] || stat --format=%U "$path" 2> /dev/null; do
        path=$(dirname "$path");
    done
}
owner_write ()
{
    declare input path;
    readin input;
    for path in "$@";
    do
        print "$input" | sudo_owner "$path" tee -- "$path" > /dev/null;
    done
}
param ()
{
    declare param;
    for param in "$@";
    do
        println "declare $param; is_set $param || { (( \$# )) && $param=\$1 && shift; }";
    done
}
path_append ()
{
    path_remove "$@";
    declare path;
    for path in "$@";
    do
        PATH+=:$path;
    done
}
path_contains ()
{
    path_index "$1" > /dev/null
}
path_index ()
{
    declare IFS=:;
    declare paths=($PATH);
    array_index paths "$1"
}
path_prepend ()
{
    path_remove "$@";
    declare path;
    for path in "$@";
    do
        PATH=$path:$PATH;
    done
}
path_remove ()
{
    declare IFS=:;
    declare paths=($PATH);
    array_remove paths "$@";
    PATH=${paths[*]}
}
pin_package_path ()
{
    eval "$(param package)" && print "/etc/apt/preferences.d/$package.pref"
}
pin_package_version ()
{
    eval "$(param package)" || return;
    eval "$(param version)" || return;
    sudo_write "$(pin_package_path "$package")" <<-EOF
Package: $package
Pin: version $version
Pin-Priority: 1001
EOF

}
print ()
{
    printf %s "$*"
}
println ()
{
    print "$*"'
'
}
public_ip ()
{
    declare ip inet dig=() wget=();
    [[ -n ${inet:=$1} ]] && {
        declare -A t=([-4]=A [-6]=AAAA);
        [[ -v t[$inet] ]] || {
            error "invalid inet ${inet@Q}: pick one of ${!t[*]}";
            return
        };
        dig=("$inet" -t "${t[$inet]}");
        wget=("$inet")
    };
    {
        ip=$(dig @resolver1.opendns.com -q myip.opendns.com "${dig[@]}" +short) && [[ -n $ip ]]
    } || {
        ip=$(download "${wget[@]}" ifconfig.me) && [[ -n $ip ]]
    } && print "$ip"
}
queue_peek ()
{
    is_varname "$1" || return;
    [[ -n $2 ]] && {
        is_varname "$2" || return
    };
    is_associative "$1" && set -- "$1" "$2" '' || set -- "$1" "$2";
    eval 'declare indices=("${!'"$1"'[@]}") i';
    (( ${#indices[@]} )) || return;
    (( $# == 3 )) && {
        declare compare;
        compare=$compare array=indices array_sort;
        unshadow compare
    };
    (( i =                  0 ));
    set -- "$1" "$2" "${indices[i]}";
    unshadow indices i;
    eval 'declare value=("${'"$1"'[$3]}")';
    set -- "$1" "$2" "$3" "$value";
    unshadow value;
    if [[ -n $2 ]]; then
        eval "$2=$4";
    else
        print "$4";
    fi
}
queue_pop ()
{
    is_varname "$1" || return;
    [[ -n $2 ]] && {
        is_varname "$2" || return
    };
    is_associative "$1" && set -- "$1" "$2" '' || set -- "$1" "$2";
    eval 'declare indices=("${!'"$1"'[@]}") i';
    (( ${#indices[@]} )) || return;
    (( $# == 3 )) && {
        declare compare;
        compare=$compare array=indices array_sort;
        unshadow compare
    };
    (( i =                  0 ));
    set -- "$1" "$2" "${indices[i]}";
    unshadow indices i;
    eval 'declare value=("${'"$1"'[$3]}")';
    set -- "$1" "$2" "$3" "$value";
    unshadow value;
    if [[ -n $2 ]]; then
        eval "$2=$4";
    else
        print "$4";
    fi;
    unset "$1[$3]"
}
queue_push ()
{
    is_varname "$1" || return;
    declare items=("${@:2}");
    array_reverse items;
    set -- "$1" "${items[@]}";
    unshadow items;
    eval "$1"'+=("${@:2}")'
}
readin ()
{
    IFS= read -rd '' "$@"
}
reset_temporary ()
{
    while (( $# )); do
        is_temporary "$1" && set -- "$1" "${1@A}" "${@:2}" && unset "$1" && print "$2;" && shift 2;
        shift;
    done
}
resolve_varname ()
{
    (( ! $# )) && return;
    set -- "$@" "$(newvar)";
    while (( $# > 1 )); do
        ! is_nameref "$1" && println "$1" || {
            declare -n "${!#}=$2";
            eval 'println "${!'"$1"'}"'
        };
        shift;
    done
}
stack_peek ()
{
    is_varname "$1" || return;
    [[ -n $2 ]] && {
        is_varname "$2" || return
    };
    is_associative "$1" && set -- "$1" "$2" '' || set -- "$1" "$2";
    eval 'declare indices=("${!'"$1"'[@]}") i';
    (( ${#indices[@]} )) || return;
    (( $# == 3 )) && {
        declare compare;
        compare=$compare array=indices array_sort;
        unshadow compare
    };
    (( i = ${#indices[@]} - 1 ));
    set -- "$1" "$2" "${indices[i]}";
    unshadow indices i;
    eval 'declare value=("${'"$1"'[$3]}")';
    set -- "$1" "$2" "$3" "$value";
    unshadow value;
    if [[ -n $2 ]]; then
        eval "$2=$4";
    else
        print "$4";
    fi
}
stack_pop ()
{
    is_varname "$1" || return;
    [[ -n $2 ]] && {
        is_varname "$2" || return
    };
    is_associative "$1" && set -- "$1" "$2" '' || set -- "$1" "$2";
    eval 'declare indices=("${!'"$1"'[@]}") i';
    (( ${#indices[@]} )) || return;
    (( $# == 3 )) && {
        declare compare;
        compare=$compare array=indices array_sort;
        unshadow compare
    };
    (( i = ${#indices[@]} - 1 ));
    set -- "$1" "$2" "${indices[i]}";
    unshadow indices i;
    eval 'declare value=("${'"$1"'[$3]}")';
    set -- "$1" "$2" "$3" "$value";
    unshadow value;
    if [[ -n $2 ]]; then
        eval "$2=$4";
    else
        print "$4";
    fi;
    unset "$1[$3]"
}
stack_push ()
{
    is_varname "$1" || return;
    eval "$1"'+=("${@:2}")'
}
sudo_append ()
{
    sudo_write --append "$@"
}
sudo_owner ()
{
    eval "$(param path)";
    user=$(owner_user "$path") sudo_user "$@"
}
sudo_user ()
{
    eval "$(param user)";
    if [[ ! -n $user || $user == "$USER" ]]; then
        "$@";
    else
        is_numeric "$user" && user="#$user";
        sudo --user "$user" "$@";
    fi
}
sudo_write ()
{
    command sudo tee "$@" > /dev/null
}
tempdownload ()
{
    declare file=$(mktemp);
    download "$@" > "$file" && print "$file" || carry_status rm -- "$file"
}
tempfd ()
{
    declare r w temp=$(mktemp);
    exec {r}< "$temp" {w}> "$temp";
    rm -- "$temp";
    set -- $r $w "$@";
    unshadow r w temp;
    assign "$3=$1" "$4=$2"
}
test_indirect_expansion ()
{
    : ${!1}
}
timestamp ()
{
    date --utc +%Y%m%dT%H%M%SZ
}
to_arithmetic ()
{
    while (( $# )); do
        (( $1 = $1 ));
        shift;
    done
}
to_bool ()
{
    while (( $# )); do
        (( $1 = !!$1 ));
        shift;
    done
}
unpin_package ()
{
    eval "$(param package)" || return;
    declare path=$(pin_package_path "$package");
    [[ ! -f $path ]] || sudo rm -- "$path"
}
unshadow ()
{
    unset "$@"
}
with_params ()
{
    eval "$(param func)" || return;
    declare params;
    is_set params || params='"$@"';
    assign_if_set "$@";
    print "$func $params"
}

arg1=""
arg2="$1"

for i in $(seq 1 $(( $(echo "$1" | sed 's/[^N]//g' | wc -m) - 1 )))
do
	arg1=$arg1'\([0-9]\)'
	arg2=$(echo "$arg2" | sed 's/N/\\'$i'/')
done

sed "s/$arg1/$arg2/g"

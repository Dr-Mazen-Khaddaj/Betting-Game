DIR=$1$2
NAME="$DIR"/"$2"
if [ -d "$DIR" ];
then
echo "Deleting "$NAME".vkey"
rm "$NAME".vkey
echo "Deleting "$NAME".skey"
rm "$NAME".skey
echo "Deleting "$NAME"_stake.vkey"
rm "$NAME"_stake.vkey
echo "Deleting "$NAME"_stake.skey"
rm "$NAME"_stake.skey
echo "Deleting "$NAME".addr"
rm "$NAME".addr
echo "Deleting "$DIR
rmdir $DIR
else
	echo "Error: $DIR directory does not exist."
fi
for i in "Alford" "Bolden" "Parker" "Powell" "Hamilton"
do
  echo Initializing $i
  python server.py $i &
done

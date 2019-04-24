echo $(echo 12; echo 12; echo 12; echo 12)

cd "hello"

echo "Privet, $0"
echo "Your args $0 $1 $2 $(echo "...")"


a=p
b=w
c=d
$a$b$c

echo 'This $(echo hello) is $my'
echo "This $(echo hello) is $my"


echo $(echo "hi"; echo "privet")

ac="ho"
ec$ac $ac

ls
cd ../
ls -a
pwd

echo "Enter some vars... (waiting 3)"
read a b c
echo 1: $a
echo 2: $b
echo 3: $c

exit 5

echo "NOOOOOOOOOOO"

###
### flop a grammar (or grammars)
### 1. make tdl copies without docstrings
### 2. flop
### 3. remove tdl copies without docstrings
###
### from trunk, call $ bash pet/compile.bash
###

echo Removing Doc Strings
python3 pet/dedoc.py
mv *for-pet.tdl pet

echo flopping grammar
#ln -s english.set pet/english-for-pet.set
cd pet
flop english-for-pet.tdl
cd ..


echo Removing files without Doc Strings
#rm pet/english-for-pet.set
rm  *-forpet.tdl


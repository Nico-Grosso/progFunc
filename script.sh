#!/bin/bash

./Linter -c ./ejemplos/caso01.mhs > ./ejemplos/misalida/caso01-lint.mhs
./Linter -c ./ejemplos/caso02.mhs > ./ejemplos/misalida/caso02-lint.mhs
./Linter -c ./ejemplos/caso03.mhs > ./ejemplos/misalida/caso03-lint.mhs
./Linter -c ./ejemplos/caso04.mhs > ./ejemplos/misalida/caso04-lint.mhs
./Linter -c ./ejemplos/caso05.mhs > ./ejemplos/misalida/caso05-lint.mhs
./Linter -c ./ejemplos/caso06.mhs > ./ejemplos/misalida/caso06-lint.mhs
./Linter -c ./ejemplos/caso07.mhs > ./ejemplos/misalida/caso07-lint.mhs
./Linter -c ./ejemplos/caso08.mhs > ./ejemplos/misalida/caso08-lint.mhs
./Linter -c ./ejemplos/caso09.mhs > ./ejemplos/misalida/caso09-lint.mhs
./Linter -c ./ejemplos/caso10.mhs > ./ejemplos/misalida/caso10-lint.mhs
./Linter -c ./ejemplos/caso11.mhs > ./ejemplos/misalida/caso11-lint.mhs
./Linter -c ./ejemplos/caso12.mhs > ./ejemplos/misalida/caso12-lint.mhs
./Linter -c ./ejemplos/caso13.mhs > ./ejemplos/misalida/caso13-lint.mhs
./Linter -c ./ejemplos/caso14.mhs > ./ejemplos/misalida/caso14-lint.mhs
./Linter -c ./ejemplos/caso15.mhs > ./ejemplos/misalida/caso15-lint.mhs
./Linter -c ./ejemplos/caso16.mhs > ./ejemplos/misalida/caso16-lint.mhs
./Linter -c ./ejemplos/caso17.mhs > ./ejemplos/misalida/caso17-lint.mhs
./Linter -c ./ejemplos/caso18.mhs > ./ejemplos/misalida/caso18-lint.mhs
./Linter -c ./ejemplos/caso19.mhs > ./ejemplos/misalida/caso19-lint.mhs
./Linter -c ./ejemplos/caso20.mhs > ./ejemplos/misalida/caso20-lint.mhs
./Linter -c ./ejemplos/caso21.mhs > ./ejemplos/misalida/caso21-lint.mhs
./Linter -c ./ejemplos/caso22.mhs > ./ejemplos/misalida/caso22-lint.mhs
./Linter -c ./ejemplos/caso23.mhs > ./ejemplos/misalida/caso23-lint.mhs
./Linter -c ./ejemplos/caso24.mhs > ./ejemplos/misalida/caso24-lint.mhs

./Linter -s ./ejemplos/caso01.mhs > ./ejemplos/misalida/caso01-sug
./Linter -s ./ejemplos/caso02.mhs > ./ejemplos/misalida/caso02-sug
./Linter -s ./ejemplos/caso03.mhs > ./ejemplos/misalida/caso03-sug
./Linter -s ./ejemplos/caso04.mhs > ./ejemplos/misalida/caso04-sug
./Linter -s ./ejemplos/caso05.mhs > ./ejemplos/misalida/caso05-sug
./Linter -s ./ejemplos/caso06.mhs > ./ejemplos/misalida/caso06-sug
./Linter -s ./ejemplos/caso07.mhs > ./ejemplos/misalida/caso07-sug
./Linter -s ./ejemplos/caso08.mhs > ./ejemplos/misalida/caso08-sug
./Linter -s ./ejemplos/caso09.mhs > ./ejemplos/misalida/caso09-sug
./Linter -s ./ejemplos/caso10.mhs > ./ejemplos/misalida/caso10-sug
./Linter -s ./ejemplos/caso11.mhs > ./ejemplos/misalida/caso11-sug
./Linter -s ./ejemplos/caso12.mhs > ./ejemplos/misalida/caso12-sug
./Linter -s ./ejemplos/caso13.mhs > ./ejemplos/misalida/caso13-sug
./Linter -s ./ejemplos/caso14.mhs > ./ejemplos/misalida/caso14-sug
./Linter -s ./ejemplos/caso15.mhs > ./ejemplos/misalida/caso15-sug
./Linter -s ./ejemplos/caso16.mhs > ./ejemplos/misalida/caso16-sug
./Linter -s ./ejemplos/caso17.mhs > ./ejemplos/misalida/caso17-sug
./Linter -s ./ejemplos/caso18.mhs > ./ejemplos/misalida/caso18-sug
./Linter -s ./ejemplos/caso19.mhs > ./ejemplos/misalida/caso19-sug
./Linter -s ./ejemplos/caso20.mhs > ./ejemplos/misalida/caso20-sug
./Linter -s ./ejemplos/caso21.mhs > ./ejemplos/misalida/caso21-sug
./Linter -s ./ejemplos/caso22.mhs > ./ejemplos/misalida/caso22-sug
./Linter -s ./ejemplos/caso23.mhs > ./ejemplos/misalida/caso23-sug
./Linter -s ./ejemplos/caso24.mhs > ./ejemplos/misalida/caso24-sug

# Para los archivos de lint
for i in {01..24}
do
  echo "Procesando archivo caso${i}-lint.mhs"
  diff -w ./ejemplos/caso${i}-lint.mhs ./ejemplos/misalida/caso${i}-lint.mhs > ./ejemplos/diferencias_caso${i}-lint.txt
done

# Para los archivos de sugerencias
for i in {01..24}
do
  echo "Procesando archivo caso${i}-sug"
  diff -w ./ejemplos/caso${i}-sug ./ejemplos/misalida/caso${i}-sug > ./ejemplos/diferencias_caso${i}-sug.txt
done



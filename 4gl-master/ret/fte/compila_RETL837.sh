echo '**********************************************************************'
echo 'Compilando LANZADOR DE LOS ANEXOS 114 Y 115 (programa RETL837.4gl)'
echo 'Comilando formas................'
form4gl RETL8371.per

echo 'Compilando RETL837.4gl'
fglpc   RETL837.4gl
fglpc   RETL837A.4gl
fglpc   RETL837B.4gl

echo 'Generando archivos  RETL837*.4gi'
mv RETL837.4go  RETL837.4gi
mv RETL837A.4go RETL837A.4gi
mv RETL837B.4go RETL837B.4gi

echo 'Moviendo archivos al directorio /safre/ret/exp'
mv RETL837*.frm /safre/ret/exp
mv RETL837.4gi  /safre/ret/exp
mv RETL837A.4gi  /safre/ret/exp
mv RETL837B.4gi  /safre/ret/exp

echo 'Fin de la compilación'
echo '**********************************************************************'
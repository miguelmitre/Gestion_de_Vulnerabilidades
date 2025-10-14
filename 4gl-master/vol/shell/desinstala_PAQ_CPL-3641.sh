echo "Validando ruta CPL-3641"
pwd

echo "Instalando elementos BINARIOS previos CPL-3641"
mv /safre/vol/exp/VOLB014_rp3641.4gi    /safre/vol/exp/VOLB014.4gi

echo "Instalando elementos FUENTE previos CPL-3641"
mv /safre/vol/fte/VOLB014_rp3641.4gl  /safre/vol/fte/VOLB014.4gl

echo "Validando elementos BINARIOS"
ls -ltr /safre/vol/exp/*VOLB014*

echo "Validando elementos FUENTE"
ls -ltr /safre/vol/fte/*VOLB014*

echo "Validando ruta CPL-3641"
pwd

echo "Validando elementos instalados en la ruta base de instalación CPL-3641"
ls -ltr

echo "Validando elementos BINARIOS previos CPL-3641"
ls -ltr /safre/vol/exp/*VOLB014*

echo "Validando elementos FUENTE previos CPL-3641"
ls -ltr /safre/vol/fte/*VOLB014*

echo "Respaldando elementos BINARIOS previos CPL-3641"
mv /safre/vol/exp/VOLB014.4gi     /safre/vol/exp/VOLB014_rp3641.4gi

echo "Respaldando elementos FUENTE previos CPL-3641"
mv /safre/vol/fte/VOLB014.4gl     /safre/vol/fte/VOLB014_rp3641.4gl

echo "Instalando elementos BINARIOS actualizados CPL-3641"
cp VOLB014.4gi     /safre/vol/exp/VOLB014.4gi

echo "Instalando elementos FUENTE actualizados CPL-3641"
cp VOLB014.4gl     /safre/vol/fte/VOLB014.4gl

echo "Validando elementos BINARIOS"
ls -ltr /safre/vol/exp/*VOLB014*

echo "Validando elementos FUENTE"
ls -ltr /safre/vol/fte/*VOLB014*


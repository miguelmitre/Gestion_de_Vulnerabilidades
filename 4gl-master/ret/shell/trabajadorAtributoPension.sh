#!/bin/bash
INFORMIXDIR="/opt/informix"
PATH=$PATH:$INFORMIXDIR"/bin"
ONCONFIG=$INFORMIXDIR"/etc/onconfig.safre"
INFORMIXSERVER="safre_tcp"
INFORMIXSQLHOSTS=$INFORMIXDIR"/etc/sqlhosts"
LANG="en_US"
DBTEMP="/tmp"
DBSPACETEMP="dbs_tmp1,dbs_tmp2"
ODBCINI="/etc/odbcinst.ini"
ODBCHOME="/usr/local/"

LD_LIBRARY_PATH="/usr/local/lib:/opt/informix/lib:/opt/informix/lib/cli:/opt/informix/lib/esql:/opt/informix/lib/client:/opt/informix/lib/dmi:/opt/informix/lib32:/opt/informix/lib32/cli:/opt/informix/lib32/esql:/opt/informix/lib32/client:/opt/informix/lib32/dmi"

LDPATH=$LD_LIBRARY_PATH

export INFORMIXDIR PATH ODBCHOME ODBCINI ONCONFIG INFORMIXSERVER LD_LIBRARY_PATH LDPATH INFORMIXSQLHOSTS DBSPACETEMP DBTEMP

cd /sysx/safre/ret/exp
nohup time fglgo RETP020.4gi 1>/sysx/safre/ret/exp/RETP020.out 2>&1 &

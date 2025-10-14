################################################################################
#INTC06011_29062007.per
#INTC06011_29062007.per
#Proyecto                   : Sistema de AFORES (México)                       #
#Owner                      : E.F.P.                                           #
#Programa TABM131           : 
#Fecha                      : 06 Septiembre de 2007                            #
#Por                        : Salvador Villafranca
#Fecha modifica             : 
#Modifico                   : 
#Sistema                    : TAB                                              #
#Actualiza                  : 
#Fecha Actualiza            : 
################################################################################
DATABASE safre_af

GLOBALS
DEFINE
	reg RECORD
 	    clave_censal   LIKE tab_tel_numeracion.clave_censal,
 	    estado         LIKE tab_tel_numeracion.estado,
 	    cld            LIKE tab_tel_numeracion.cld,
 	    serie          LIKE tab_tel_numeracion.serie,
            nm_final       LIKE tab_tel_numeracion.nm_final,
 	    razon_social   LIKE tab_tel_numeracion.razon_social
        END RECORD,
  
        reg_deta RECORD LIKE tab_tel_numeracion.*,


          l_record  ARRAY[30000] OF RECORD
 	    clave_censal   LIKE tab_tel_numeracion.clave_censal,
 	    estado         LIKE tab_tel_numeracion.estado,
 	    cld            LIKE tab_tel_numeracion.cld,
 	    serie          LIKE tab_tel_numeracion.serie,
            nm_final       LIKE tab_tel_numeracion.nm_final,
 	    razon_social   LIKE tab_tel_numeracion.razon_social
   END RECORD

END GLOBALS
################################################################################
MAIN

   OPTIONS PROMPT LINE LAST,
   INPUT WRAP              ,
   ACCEPT KEY control-o

   DEFER INTERRUPT
   CALL STARTLOG("TABM131.log")
   CALL proceso()

END MAIN
################################################################################
FUNCTION proceso()
DEFINE HOY DATE

   LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 3,3 WITH 20 ROWS, 75 COLUMNS 
   ATTRIBUTE( BORDER)
   DISPLAY        " TABM131           CARGA Y CONSULTA DE NUMERACION TELEFONICA                   " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "ACCION"
      COMMAND KEY(G) "carGa" "Carga la informacion desde un archivo "
         CALL cargar()
      COMMAND "Consulta" "Consulta "
         CALL consulta()
         CLEAR SCREEN
      COMMAND KEY(INTERRUPT)
            EXIT MENU
      COMMAND "Salir" "Salir del Programa "
         EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1

END FUNCTION
################################################################################
FUNCTION Consulta()
DEFINE pos SMALLINT,
        query1  CHAR(100),
        query2  CHAR(250),
        bandera SMALLINT
	
      LET int_flag = FALSE
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1311" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                  (Ctrl-C) Salir       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                         NUMEROS TELEFONICOS                                   " AT 2,1 ATTRIBUTE(REVERSE,BOLD) 

WHILE TRUE
      INITIALIZE reg.*,reg_deta.* TO NULL
      LET bandera=0

      CONSTRUCT BY NAME query1 ON clave_censal,estado,cld,serie,razon_social

         ON KEY (control-m)
            ERROR "PROCESANDO INFORMACION..."
            SLEEP 1
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag THEN
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 1
         ERROR ""
         EXIT WHILE
      END IF

      IF query1 = " 1=1" THEN
         CALL desborda_info()
         CONTINUE WHILE
      END IF

      LET query2 = "SELECT clave_censal,estado,cld,serie,nm_final,",
                   " razon_social ",
                   " FROM tab_tel_numeracion ",
                   " WHERE ", query1 CLIPPED,
	           " ORDER BY 1,3 "

      PREPARE s1 FROM query2
      DECLARE r_cursor_1 CURSOR FOR s1
      LET pos = 1

      FOREACH r_cursor_1 INTO reg.*
         LET l_record[pos].clave_censal= reg.clave_censal
         LET l_record[pos].estado= reg.estado 
         LET l_record[pos].cld= reg.cld
         LET l_record[pos].serie= reg.serie
         LET l_record[pos].nm_final= reg.nm_final
         LET l_record[pos].razon_social= reg.razon_social
         LET pos = pos + 1
         
         IF pos >=30001 THEN  
            CALL desborda_info()
             CONTINUE WHILE
         END IF
      END FOREACH


      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""	
         DISPLAY ARRAY l_record   TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET reg.clave_censal=l_record[pos].clave_censal
               LET reg.estado=l_record[pos].estado
               LET reg.cld=l_record[pos].cld
               LET reg.serie=l_record[pos].serie
               LET reg.nm_final=l_record[pos].nm_final
               LET bandera = 1
               EXIT DISPLAY
            ON KEY (INTERRUPT)
               LET bandera = 0
               EXIT DISPLAY
         END DISPLAY

         IF bandera=0 THEN 
             EXIT WHILE
         ELSE 
             CALL detalle()
             CURRENT WINDOW IS ventana_2

        END IF
      ELSE
         ERROR "NO EXISTE INFORMACION PARA ESE CRITERIO DE BUSQUEDA"
         SLEEP 3
         ERROR ""
         CONTINUE WHILE
   END IF
 CLEAR FORM
 END WHILE
 CLOSE WINDOW ventana_2
END FUNCTION
#****************************************************************************
FUNCTION detalle()
DEFINE resp CHAR(1)

           OPEN WINDOW ventana_3 AT 6,3 WITH FORM "TABM1312" 
           ATTRIBUTE( BORDER)
           DISPLAY "                                                   (Ctrl-C) Salir       " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
           DISPLAY "             DETALLE NUMEROS TELEFONICOS                                   " AT 2,1 ATTRIBUTE(REVERSE,BOLD) 

           SELECT clave_censal,poblacion,municipio,estado,presuscripcion,region,
                  asl,cld,serie,nm_inicial,nm_final,ocupacion,tipo_de_red,
                  modalidad,razon_social,fasignacion,fconsolidacion,fmigracion,
                  cld_anterior 
           INTO   reg_deta .*
           FROM   tab_tel_numeracion
           WHERE  clave_censal = reg.clave_censal AND
                  estado       = reg.estado       AND
                  cld          = reg.cld          AND
                  serie        = reg.serie        AND
                  nm_final     = reg.nm_final 

             DISPLAY BY NAME reg_deta.*
             IF INT_FLAG THEN 
                 CLOSE WINDOW ventana_3
                 LET int_flag = FALSE
                 ERROR "CONSULTA CANCELADA ..."
                 SLEEP 2
                 ERROR ""
                 RETURN
             END IF
           PROMPT "Oprima una tecla para regresar" for CHAR resp
           CLOSE WINDOW ventana_3
END FUNCTION
#**********************************************************************
FUNCTION cargar()
DEFINE ruta,cadena CHAR(100),
       cuenta INTEGER,
       resp CHAR(1)

LET cuenta=0
LET INT_FLAG=0
INITIALIZE resp to NULL

CREATE TEMP TABLE t1 (campo1 CHAR(100))

SELECT ruta_rescate INTO ruta FROM seg_modulo
WHERE modulo_cod="tab"

LET cadena= "ls -f ",ruta CLIPPED, "/pnn_pg.csv > archivo.txt"
#LET cadena2= "ls -f pnn_pg.csv > archivo.txt"
RUN cadena CLIPPED

Load from "archivo.txt" insert into t1

SELECT * FROM t1 

IF STATUS=NOTFOUND THEN
   OPEN WINDOW wnd4 AT 12,10 WITH 5 ROWS,45 COLUMNS ATTRIBUTE (BORDER)
   DISPLAY  "NO EXISTE EL ARCHIVO A CARGAR.VERIFIQUE" at 3,1
   SLEEP 4
   CLOSE WINDOW wnd4 
   DROP TABLE t1
   RETURN
END IF

DROP TABLE t1

IF INT_FLAG THEN
   ERROR "PROCESO CANCELADO"
   SLEEP 2
   ERROR ""
END IF

SELECT COUNT(*) INTO cuenta FROM tab_tel_numeracion

IF cuenta >0 THEN
   OPEN WINDOW wnd5 AT 12,10 WITH 5 ROWS,60 COLUMNS 
   ATTRIBUTE (PROMPT LINE FIRST,BORDER)
   LET resp='%'

   WHILE UPSHIFT(resp) NOT MATCHES "[SN]"
        PROMPT "LA INFORMACION EXISTENTE SERA REEMPLAZADA (S/N)?" FOR CHAR resp
        IF INT_FLAG THEN EXIT WHILE END IF
   END WHILE

   CLOSE WINDOW wnd5 
   IF INT_FLAG THEN RETURN END IF

    IF UPSHIFT(resp) ="S" THEN
       DELETE FROM tab_tel_numeracion where 1=1
       ERROR "INFORMACION ELIMINADA"	
       SLEEP 2
       ERROR ""
    ELSE
        RETURN
    END IF
END IF

LET cadena ="vi ",ruta CLIPPED, "/pnn_pg.csv<cambia.cmd>/dev/null"
RUN cadena CLIPPED 

LET cadena =ruta CLIPPED, "/pnn_pg.csv"
#LOAD FROM "pnn_pg.csv" DELIMITER "," INSERT INTO tab_tel_numeracion
LOAD FROM cadena  DELIMITER "," INSERT INTO tab_tel_numeracion
ERROR "NUEVA INFORMACION CARGADA"	
SLEEP 2
ERROR ""

END FUNCTION
#***************************************************************************
FUNCTION desborda_info()

ERROR ""
OPEN WINDOW wnd6 AT 12,10 WITH 5 ROWS,45 COLUMNS ATTRIBUTE (BORDER)
DISPLAY  "DEMASIADA INFORMACION. DEPURE SU BUSQUEDA" at 3,1
SLEEP 4
CLOSE WINDOW wnd6 

END FUNCTION



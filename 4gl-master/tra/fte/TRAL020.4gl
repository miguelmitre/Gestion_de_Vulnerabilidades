##############################################################################
#Owner             => E.F.P.
#Programa TRAL020  => REPORTE CAPTURA PENDIENTE DE ENVIAR   
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 21 DE FEBRERO DEL 2005 
#Sistema           => TRA-ICE-IMSS   
##############################################################################
DATABASE safre_af
GLOBALS

DEFINE hoy DATE
END GLOBALS
MAIN
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

 CALL ini()
    OPEN WINDOW ventana_1 AT 2,2 WITH 21 ROWS,78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " TRAL020  CONSULTA DE CAPTURA SOLIC. TRA-ICE-AFO IMSS POR ENVIAR               " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "RESUMEN CAPTURA"
    COMMAND "General" "Resumen de Captura"
       CALL general()
    COMMAND "Detalle" "Consulta Por Status"
       CALL detalle() 
    COMMAND "Salir" "Salir del Programa"
          EXIT MENU
  END MENU
        CLOSE WINDOW ventana_1
END MAIN



FUNCTION ini()
#i---------------

LET hoy = TODAY

END FUNCTION

FUNCTION general()
#g----------------

DEFINE cadena CHAR(100)

LET cadena = "fglgo TRAL021"

RUN cadena


END FUNCTION


FUNCTION detalle()
#d----------------

    OPEN WINDOW ventana_2 AT 2,2 WITH 21 ROWS,78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " TRAL020    DETALLE DE CAPTURA PEND. POR ENVIAR POR STATUS IMSS                " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "STATUS" 
    COMMAND "Confirmada" "Solicitudes Confirmadas"
       CALL tipo_detalle(1)
    COMMAND "Rechazo" "Solicitudes recicladas por convivencia"
       CALL tipo_detalle(21) 
    COMMAND "Pendiente" 
            "Solicitudes de trabajador asignado pendiente hasta certificar"
       CALL tipo_detalle(30) 
    COMMAND "Salir" "Salir del Programa"
          EXIT MENU
  END MENU
        CLOSE WINDOW ventana_2
END FUNCTION

FUNCTION tipo_detalle(tipo)
#c-------------------------

DEFINE tipo SMALLINT
DEFINE reg_tipo RECORD 
      n_seguro     CHAR(011),
      nss          CHAR(011),
      n_rfc        CHAR(013),
      rfc          CHAR(013),
      icefa_cod    CHAR(003),
      nro_int_cta  CHAR(030),
      contador     CHAR(009)
    END RECORD

DEFINE i INTEGER
DEFINE arr_reg_tipo ARRAY[1000] OF RECORD 
      n_seguro     CHAR(011),
      nss          CHAR(011),
      n_rfc        CHAR(013),
      rfc          CHAR(013),
      icefa_cod    CHAR(003),
      nro_int_cta  CHAR(030),
      contador     CHAR(009)
    END RECORD

    DEFINE cont1 integer,    
           cont2 integer
  
OPEN WINDOW TRAL0203 AT 2,2 WITH FORM "TRAL0203"  ATTRIBUTE(BORDER)
DISPLAY "                             <Ctrl-c> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)                                            

CASE tipo 
WHEN 1
DISPLAY " TRAL020    DETALLE DE SOL.CONFIRMADAS PENDIENTES DE ENVIAR IMSS               " AT 3,1 ATTRIBUTE(REVERSE)                                          
EXIT CASE
WHEN 21
DISPLAY " TRAL020    DETALLE DE SOL.RECICLADAS RECHAZADAS POR CONV. IMSS                " AT 3,1 ATTRIBUTE(REVERSE)                                          
EXIT CASE
WHEN 30
DISPLAY " TRAL020    DETALLE DE SOL.PENDIENTES EN ESPERA DE CERTIF. IMSS                " AT 3,1 ATTRIBUTE(REVERSE)                                          
EXIT CASE
OTHERWISE
 EXIT CASE
END CASE

DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)                   

SELECT COUNT(*)
INTO cont2
FROM tra_mae_icefa A
WHERE  A.status = tipo

DECLARE cur_tipo CURSOR FOR

SELECT A.n_seguro     ,
       A.nss          ,
       B.n_rfc        ,
       A.rfc          ,
       A.icefa_cod    ,
       A.nro_int_cta  ,
       " " 
FROM   tra_mae_icefa A ,
       afi_mae_afiliado B
WHERE  A.status = tipo 
AND    A.n_seguro = B.n_seguro
ORDER BY 1,5

LET i = 1

FOREACH cur_tipo INTO reg_tipo.*
   LET reg_tipo.contador = i USING"&&&&","/",cont2 USING"&&&&"
   LET reg_tipo.contador = reg_tipo.contador CLIPPED
   LET arr_reg_tipo[i].* = reg_tipo.*
   LET i = i + 1
END FOREACH 

        CALL SET_COUNT (i-1)                                
        DISPLAY ARRAY arr_reg_tipo TO scr_1.*                       
CLOSE WINDOW TRAL0203
END FUNCTION


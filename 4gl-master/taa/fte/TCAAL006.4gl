############################################################
#Proyecto          => Sistema de Afores.( MEXICO )         #
#Programa TCAAL006  => Solicitudes de traspaso              #
#Objetivo de la Mod=> La modificacion consistio en         #
#                     anexarle  la pantalla.               #
#Modificado        => 05 DE MARZO DEL 2003                 #
#Por               => MARCO ANTONIO GONZALEZ ROJAS         #
#Elaboro           => MAURO MUNIZ CABALLERO                #
#Sistema           => TRA                                  #
############################################################

DATABASE safre_af
GLOBALS
    DEFINE  datos        RECORD
            cve_recep          CHAR(03),
	         des_afore          CHAR(45),
            tot_parcial        SMALLINT
    END RECORD

    DEFINE   fecha_presentacion DATE

    DEFINE hoy           DATE
    DEFINE hora          CHAR(8)
    DEFINE aux_pausa     CHAR(1)
    DEFINE g_afore       RECORD LIKE tab_afore_local.*
    DEFINE g_lista       CHAR(200)
    DEFINE g_run         CHAR(200)
    DEFINE g_enter       CHAR(1)
    DEFINE comm          CHAR(100)
    DEFINE tot_total     SMALLINT
    DEFINE g_folio         INTEGER
    DEFINE g_seg_modulo  RECORD LIKE seg_modulo.*
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore   
    DEFINE g_usuario     CHAR(08)
        
END GLOBALS

MAIN

    OPTIONS
    PROMPT LINE LAST
    ---ACCEPT KEY CONTROL-C,
    --- INPUT WRAP

    ---DEFER INTERRUPT

    CALL inicio()
  

    OPEN WINDOW  vent  AT 2,2  WITH FORM "TCAAL0061" ATTRIBUTE (BORDER)
    DISPLAY " < CTRL-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " TCAAL006             REPORTE DE SOLICITUDES DE TRASPASO                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

       INPUT BY NAME g_folio WITHOUT DEFAULTS 
            
          BEFORE FIELD g_folio
             LET g_folio =  NULL 

          AFTER FIELD g_folio
             IF (g_folio = 0  OR g_folio IS NULL) THEN
                ERROR "El folio no puede ir --NULO-- verifique..."
                SLEEP 3
                ERROR "                                          "
                NEXT FIELD g_folio
             END IF
              
       END INPUT
  # PROMPT "ENTER =" FOR g_enter 
    CALL proceso_informacion() #pi

    CLOSE WINDOW vent


END MAIN


FUNCTION proceso_informacion()
#pi---------------------------

    DEFINE tot    SMALLINT
    DEFINE enter  CHAR(01)

    SELECT count(*)
    INTO   tot
    FROM safre_af:taa_cd_det_cedido a 
    WHERE a.folio = g_folio

    IF tot = 0 THEN
       ERROR "NO EXISTEN DATOS" sleep 3
       EXIT PROGRAM 
    ELSE 
       INITIALIZE datos.* TO NULL

       SELECT *, USER
       INTO   g_afore.*, g_usuario 
       FROM   tab_afore_local


       SELECT UNIQUE a.fecha_presentacion
          INTO fecha_presentacion
          FROM safre_af:taa_cd_det_cedido a
       WHERE a.folio = g_folio
     
         

          LET g_lista = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,                        ".sol_trasp.",hoy USING "DDMMYY","_",hora CLIPPED


         ---RUN g_lista

       START REPORT listado TO g_lista

       DECLARE cur1 CURSOR FOR
         SELECT a.cve_recep_cuenta, b.afore_desc, count(*)
         FROM   safre_af:taa_cd_det_cedido a, tab_afore b
         WHERE  a.cve_recep_cuenta = b.afore_cod
           AND  a.folio = g_folio 
         GROUP BY 1,2                                   
         ORDER BY 1

         IF STATUS = NOTFOUND THEN 
            ERROR "NO SE IMPRIME REPORTE" SLEEP 2
         ELSE  
            FOREACH cur1 into datos.*
              LET tot_total = tot_total + datos.tot_parcial         
              OUTPUT TO REPORT listado(datos.*)  #l
            END FOREACH 
         END IF

       FINISH REPORT listado
       ERROR "LISTADO GENERADO =>",g_lista
       SLEEP 3
       ERROR ""
  
       LET  g_lista = "chmod 777 ",g_seg_modulo.ruta_listados CLIPPED,
                                 "/",g_usuario CLIPPED,".sol_trasp.",
                                  hoy USING "DDMMYY","_",hora CLIPPED
       RUN g_lista
       LET    g_run    =  "lp ",g_lista
       RUN    g_run

   --- PROMPT "ARCHIVO IMPRESO => ",g_lista CLIPPED,"  PULSE <enter>" FOR enter
       ERROR  "PROCESO FINALIZADO" SLEEP 2

    END IF

END FUNCTION


REPORT listado(datos)
#l-------------------
    DEFINE L1   CHAR(01)
    DEFINE L5   CHAR(05)
    DEFINE L10  CHAR(10)

    DEFINE  datos        RECORD
            cve_recep    CHAR(03),
            des_afore    CHAR(45),
            tot_parcial  SMALLINT
    END RECORD

    DEFINE  campo        RECORD
            afore_cod    CHAR(03),
            raz_social   CHAR(50) 
    END RECORD

    OUTPUT
    PAGE LENGTH 60
    LEFT MARGIN 0
    RIGHT MARGIN 132
    TOP MARGIN 0
    BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER

    ----PRINT COLUMN 1,'\033e\033(s218T\033(s11H\033(s7B'

    PRINT COLUMN 71,"Pagina:",PAGENO USING "<<<<"
    PRINT COLUMN 02,"TCAAL006","  ","SOLICITUD DE TRASPASO AFORE / AFORE (CEDENTE)","    ","FECHA : "," ",hoy USING "DD-MM-YYYY" 

    SELECT codigo_afore,razon_social
    INTO   campo.afore_cod,campo.raz_social
    FROM   tab_afore_local

    PRINT COLUMN 02,"_________________________________________________________________________________________________________"

    PRINT COLUMN 02,campo.afore_cod,"     ",campo.raz_social               
    PRINT COLUMN 02,"FOLIO:"," ",g_folio
    PRINT COLUMN 02,"FECHA DE PRESENTACION:"," ",fecha_presentacion

    PRINT COLUMN 02,"_________________________________________________________________________________________________________"

    PRINT  

    PRINT COLUMN 02,"_________________________________________________________________________________________________________"
    PRINT COLUMN 02,"CLAVE",
          COLUMN 30,"AFORE",
          COLUMN 50,"AFILIADOS A TRASPASAR" 

    PRINT COLUMN 02,"_________________________________________________________________________________________________________"

    PRINT

ON EVERY ROW

    PRINT
    PRINT COLUMN 02,datos.cve_recep USING "&&&",
          COLUMN 15,datos.des_afore,     
          COLUMN 45,datos.tot_parcial USING "######"
    PRINT 

    IF lineno > 57 THEN
       SKIP TO TOP OF PAGE
    END IF

ON LAST ROW

    PRINT COLUMN 02,"_________________________________________________________________________________________________________"
    
    PRINT COLUMN 02,"TOTAL",
          COLUMN 60,tot_total USING "######"

    PRINT COLUMN 02,"_________________________________________________________________________________________________________"
END REPORT
################################################################################
FUNCTION inicio() 

   LET hora = TIME
   LET hoy  = TODAY

   SELECT codigo_afore,USER
   INTO w_codigo_afore,g_usuario
   FROM tab_afore_local


   SELECT ruta_listados
   INTO  g_seg_modulo.ruta_listados
   FROM seg_modulo
   WHERE modulo_cod = 'taa'


END FUNCTION

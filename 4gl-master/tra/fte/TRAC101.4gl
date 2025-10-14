#############################################################################
#Owner             => E.F.P.
#Programa TRAC101  => LANZA CARGA BDSAR92
#                    
#Fecha creacion    => 14 marzo 2006
#By                => JESUS DAVID YANEZ MORENO
#Fecha de Mod      => 14 marzo 2006
#Sistema           => TRA SAR 92
##############################################################################

DATABASE safre_af

GLOBALS
DEFINE lanza_proceso CHAR(1000)
DEFINE paso  CHAR(200)
DEFINE tot_antes    ,
       tot_sol      ,
       tot_arch     ,
       tot_carga    ,
       tot_ult      DECIMAL(16,0)

DEFINE c_vector  CHAR(008)
DEFINE reporte CHAR(200)
DEFINE usuario CHAR(010)
DEFINE r_tot INTEGER
DEFINE r_unicos INTEGER
DEFINE reg_rep RECORD 
       folio_interno  integer    ,
       cve_ced_cuenta char(003)  ,
       cve_desc       char(010)  ,
       criterio       char(002)  ,
       criterio_desc  char(030)  ,
       tot_criterio   integer    ,
       total          integer    ,
       unicos         integer 
END RECORD 
       
DEFINE txt1 CHAR(250)
     DEFINE banco                  CHAR(015)
     DEFINE v_criterio             CHAR(030)
     DEFINE HOY                    DATE
     DEFINE HORA                   CHAR(008)
     DEFINE v_folio_interno        INTEGER
     DEFINE reg_ruta               RECORD LIKE seg_modulo.*

     DEFINE criterio               CHAR(02) ,
            cuenta                          ,
            cuantos                INTEGER  ,
            ruta_archivo           CHAR(100),
            enter                  CHAR(001)

     DEFINE reg_1                  RECORD 
            nom_archivo            CHAR(020)
     END RECORD

     DEFINE reg_tra_det_bdsar92 RECORD LIKE tra_det_bdsar92.*
          
     DEFINE  g_glob            RECORD
             codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
             razon_social      LIKE safre_af:tab_afore_local.razon_social
                               END RECORD
     DEFINE g_nom_prog         CHAR(07)

END GLOBALS

MAIN
    DEFER INTERRUPT      
    OPTIONS
    ACCEPT KEY CONTROL-I ,
    INPUT WRAP           ,
    PROMPT LINE LAST

   CALL STARTLOG("TRAC101.log")

   CALL init()

   OPEN WINDOW trac1001 AT 4,4 WITH FORM "TRAC1001" ATTRIBUTE(BORDER)
   DISPLAY " TRAC100             CARGA ARCHIVO BDSAR 92  ICEFA-AFORE 	                   " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                              " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg_1.* WITHOUT DEFAULTS

      AFTER FIELD nom_archivo
	       IF reg_1.nom_archivo IS NULL THEN
	           ERROR "Campo NO puede ser NULO"
	           NEXT FIELD nom_archivo
	       END IF
      ON KEY (ESC)

            LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
            reg_1.nom_archivo CLIPPED

            DISPLAY "PROCESANDO INFORMACION " AT 19,2


           LET txt1 = "cd ",reg_ruta.ruta_rescate CLIPPED,";",
           "sed -e /^$/d ",reg_1.nom_archivo CLIPPED ," > ",
           reg_ruta.ruta_rescate CLIPPED,"/PASO"
           RUN txt1

           LET txt1 = "cd ",reg_ruta.ruta_rescate CLIPPED,";mv PASO ",
           ruta_archivo
           
           RUN txt1

   --        LOAD FROM ruta_archivo  DELIMITER "¨"
   --        INSERT INTO sube_registro_ic

   --        DELETE FROM sube_registro_ic
   --        WHERE  reg is NULL

   --        SELECT COUNT(*)
   --        INTO   cuantos 
   --        FROM   sube_registro_ic

           LET cuantos = 1

           IF cuantos = 0 OR 
              cuantos IS NULL THEN
              DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
              AT 19,2 ATTRIBUTE(REVERSE)
              SLEEP 3
              NEXT FIELD nom_archivo
           ELSE 
              EXIT INPUT 
           END IF

       ON KEY (INTERRUPT)
           PROMPT "PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
   END INPUT

   ERROR"Lanzando Proceso por nohup..."
   SLEEP 2

   LET lanza_proceso = "nohup fglgo TRAC100 ",reg_1.nom_archivo CLIPPED

   LET paso = reg_ruta.ruta_envio CLIPPED,
           "/"                 ,
           "nohup_TRAC100."       ,
           HOY USING"YYYYMMDD"

   LET lanza_proceso  = lanza_proceso CLIPPED, " 1>",paso CLIPPED," 2>&1 &"
   RUN lanza_proceso

   ERROR"Saliendo de pantalla...consultar nohup_TRAC100.YYYYMMDD"
   SLEEP 3
   
END MAIN

FUNCTION init()   
#i-------------

    LET HOY                         =                                TODAY
    LET HORA                        =                                TIME
    LET cuantos                     =                                0
    LET cuenta                      =                                0
    LET g_nom_prog                  =                                "TRAC070"
    LET tot_antes                   = 0
    LET tot_sol                     = 0
    LET tot_arch                    = 0
    LET tot_carga                   = 0
    LET tot_ult                     = 0

   
    SELECT codigo_afore,
           razon_social
      INTO g_glob.*
      FROM safre_af:tab_afore_local

    --SQL 
       --CREATE TEMP TABLE sube_registro_ic(reg CHAR(200))
       --FRAGMENT BY ROUND ROBIN  IN dbs_tmp1 , dbs_tmp2
    --END SQL

    SELECT * ,USER
    INTO   reg_ruta.*,usuario
    FROM   seg_modulo   
    WHERE  modulo_cod = "tra"

END FUNCTION

#############################################################################
#Owner             => E.F.P.
#Programa TRAC100  => RECIBE ARCHIVO PARA TRASPASO SAR 92
#                    
#Fecha creacion    => 14 marzo 2006
#By                => JESUS DAVID YANEZ MORENO
#Fecha de Mod      => 14 marzo 2006
#Sistema           => TRA SAR 92
##############################################################################

DATABASE safre_af

GLOBALS

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

   PREPARE qry1 FROM "SET PDQPRIORITY HIGH"
   EXECUTE qry1 

   CALL STARTLOG("TRAC100.log")

   LET  reg_1.nom_archivo = ARG_VAL(1)
   CALL init()

   DISPLAY "TRAC100: CARGA ARCHIVO BDSAR 92  ICEFA-AFORE" 

   DISPLAY "TRAC100: ",HOY USING"DD-MM-YYYY" 

   LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
       reg_1.nom_archivo CLIPPED

       DISPLAY "TRAC100: INICIANDO PROCESO PARA EL ARCHIVO ",reg_1.nom_archivo

       LET txt1 = "cd ",reg_ruta.ruta_rescate CLIPPED,";",
           "sed -e /^$/d ",reg_1.nom_archivo CLIPPED ," > ",
           reg_ruta.ruta_rescate CLIPPED,"/PASO"
       RUN txt1

       LET txt1 = "cd ",reg_ruta.ruta_rescate CLIPPED,";mv PASO ",
                  ruta_archivo
       RUN txt1

       DATABASE safre_tmp
       WHENEVER ERROR CONTINUE

          LOAD FROM ruta_archivo  DELIMITER "¨"
          INSERT INTO sube_registro_ic

       DATABASE safre_af

       DELETE FROM safre_tmp:sube_registro_ic
       WHERE  reg is NULL

       SELECT COUNT(*)
       INTO   cuantos 
       FROM   safre_tmp:sube_registro_ic

       IF cuantos = 0 OR cuantos IS NULL THEN
          DISPLAY  " ERROR: NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
       ELSE
          DISPLAY "TRAC100: FOLIO ",v_folio_interno  
          DISPLAY "TRAC100: A PROCESAR ",cuantos," REGISTROS"
       END IF

       CALL clasifica() 

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

  DATABASE safre_tmp
   WHENEVER ERROR CONTINUE
    DROP TABLE sube_registro_ic
   WHENEVER ERROR STOP
 -- SQL 
    CREATE TABLE sube_registro_ic(reg CHAR(200)) 
 -- FRAGMENT BY ROUND ROBIN IN dbs_tmp1, dbs_tmp2
 -- END SQL
  DATABASE safre_af

    SELECT COUNT(*) 
    INTO tot_antes
    FROM  tra_det_bdsar92

    SELECT COUNT(*)
    INTO   tot_sol
    FROM tra_det_bdsar92 
    WHERE n_seguro IS NOT NULL
    OR    n_seguro not in (""," ")

    SELECT * ,USER
    INTO   reg_ruta.*,usuario
    FROM   seg_modulo   
    WHERE  modulo_cod = "tra"

    SELECT MAX(folio_interno) + 1
    INTO   v_folio_interno
    FROM   tra_folio_interno
  
    INSERT INTO tra_folio_interno VALUES(v_folio_interno,HOY)
 
    DISPLAY "TRAC100: FOLIO: ",v_folio_interno  
 
END FUNCTION

FUNCTION clasifica()
#c------------------
    CALL criterio_1()
    DISPLAY "TRAC100: PROCESO FINALIZADO " 
END FUNCTION


FUNCTION criterio_1()
#c1------------------

     DEFINE v_rowid    INTEGER

     CALL inserta(1)

END FUNCTION

FUNCTION inserta(f_criterio)
#ins------------------------

     DEFINE      registro        CHAR(600)
     DEFINE      f_criterio      SMALLINT
     DEFINE      f_saldo_1       CHAR(011)
     DEFINE      f_saldo_2       CHAR(011)
     DEFINE      f_aport         CHAR(011)
     DEFINE      sw smallint

     SELECT COUNT(*)    
     INTO tot_arch 
     FROM safre_tmp:sube_registro_ic 

LET sw = 1

     DECLARE cur_inserta CURSOR FOR
     SELECT A.* 
     FROM   safre_tmp:sube_registro_ic A

   FOREACH cur_inserta INTO registro
    
     LET reg_tra_det_bdsar92.icefa_cod         = registro[001,003]
     LET reg_tra_det_bdsar92.nss               = registro[004,014]
     LET reg_tra_det_bdsar92.rfc               = registro[015,027]
     LET reg_tra_det_bdsar92.nro_int_cta       = registro[028,057]
     LET reg_tra_det_bdsar92.nombre_ent        = registro[058,177]
     LET f_aport                                  = registro[182,183],"/",
                                                    registro[185,186],"/",
                                                    registro[188,191]
     LET reg_tra_det_bdsar92.fecha_nacimiento  = registro[178,187]
     LET reg_tra_det_bdsar92.estado            = 0 
     LET reg_tra_det_bdsar92.fecha_edo         = " "
     LET reg_tra_det_bdsar92.folio_interno     = v_folio_interno
     LET reg_tra_det_bdsar92.correlativo       = 0
     LET reg_tra_det_bdsar92.usuario           = usuario 


  --   SELECT "OK" 
  --   FROM   tra_det_bdsar92 a
  --   WHERE  a.nss           = reg_tra_det_bdsar92.nss
  --   AND    a.rfc           = reg_tra_det_bdsar92.rfc
     --AND    a.icefa_cod     = reg_tra_det_bdsar92.icefa_cod
     --AND    a.nro_int_cta   = reg_tra_det_bdsar92.nro_int_cta
     --GROUP BY  1

     --IF STATUS = NOTFOUND THEN
        INSERT INTO tra_det_bdsar92 VALUES (reg_tra_det_bdsar92.*)
        LET tot_carga = tot_carga + 1 
     --END IF


   END FOREACH

   SELECT COUNT(*) 
   INTO   tot_ult
   FROM    tra_det_bdsar92

--   LET reporte  = reg_ruta.ruta_listados CLIPPED,"/",usuario CLIPPED,
--                  ".TRAC070."          ,
                  --HOY USING"YYYYMMDD"  ,
 		            --HORA CLIPPED

     --START REPORT rpt_0 TO reporte

DISPLAY "TOTAL ANTES         : "  , tot_antes  
DISPLAY "TOTAL CON SOLICITUD : "  , tot_sol 
DISPLAY "TOTAL EN ARCHIVO    : "  , tot_arch
DISPLAY "TOTAL CARGADOS      : "  , tot_carga
DISPLAY "TOTAL EN BDSAR92    : "  , tot_ult      

END FUNCTION
{
REPORT rpt_0(reg_rep,r_total)
#r0-----------------

DEFINE reg_rep RECORD 
       folio_interno  integer  ,
       cve_ced_cuenta char(003),
       cve_desc       char(010),
       criterio       char(002),
       criterio_desc  char(030),
       tot_criterio   integer  ,
       total          integer ,
       unicos         integer 
END RECORD 


DEFINE r_total integer
OUTPUT 
   PAGE LENGTH 80

FORMAT 
	PAGE HEADER
        PRINT                                                                 
        SKIP 2 LINES                                                         
        PRINT                                                                
            COLUMN 50, TODAY USING "DD/MM/YYYY"
        PRINT
            COLUMN 50, TIME                                

        PRINT                                                                
        PRINT 
           COLUMN 010,g_nom_prog CLIPPED
        PRINT                                                                
           COLUMN 010,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED
	PRINT 
	PRINT
            COLUMN 10,  
               "   REPORTE DE CARGA ARCHIVO CRUCE TRA-ICE-AFORE ISSSTE    "
	PRINT
 	PRINT
        SKIP 2 LINES
        PRINT COLUMN 5,"--------------------------------------------------------------"
        PRINT COLUMN 5,"Folio   : ",reg_rep.folio_interno
        PRINT COLUMN 5,"--------------------------------------------------------------"
        PRINT COLUMN 10,"ICEFA",
              COLUMN 40,"CRITERIO",
              COLUMN 55,"TOTAL"
        PRINT
     ON EVERY ROW 

        PRINT COLUMN 10,reg_rep.cve_ced_cuenta                  ,
              COLUMN 15,reg_rep.cve_desc                        ,
              COLUMN 45,reg_rep.criterio USING"#&"              ,
              COLUMN 55,reg_rep.tot_criterio  USING"#####&"
      AFTER GROUP OF reg_rep.cve_ced_cuenta 
	PRINT COLUMN 54,"--------"   
	PRINT COLUMN 55 ,GROUP SUM(reg_rep.tot_criterio) USING"#####&"
      ON LAST ROW
        PRINT
        PRINT COLUMN 5,"--------------------------------------------------------------"

        PRINT COLUMN 10,"TOTAL  : ",
              COLUMN 55,r_total USING"#####&"
        PRINT
        PRINT COLUMN 5,"--------------------------------------------------------------"

END REPORT
}


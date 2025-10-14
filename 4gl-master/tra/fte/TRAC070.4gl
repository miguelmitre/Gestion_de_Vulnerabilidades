#############################################################################
#Owner             => E.F.P.
#Programa TRAC070  => RECIBE Y CONFRONTA ARCHIVO PARA TRASPASO AUTOMATICO I-A
#                     ISSSTE.
#Fecha creacion    => 10 DE NOVIEMBRE DEL 2005
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Ultima Mod.       => ENERO DEL 2006.
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af

GLOBALS
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

     DEFINE reg_tra_det_atm_issste RECORD LIKE tra_det_atm_issste.*
     DEFINE reg_tra_cza_aut_issste RECORD LIKE tra_cza_aut_issste.*
     DEFINE reg_tra_sum_aut_issste RECORD LIKE tra_sum_aut_issste.*
          
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

   CALL init()

   OPEN WINDOW trac0701 AT 4,4 WITH FORM "TRAC0701" ATTRIBUTE(BORDER)
   DISPLAY " TRAC070     CARGA ARCHIVO CRUCE SAR 92 ICEFA-AFORE ISSSTE	                   " AT 3,1 ATTRIBUTE(REVERSE)

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
            WHENEVER ERROR CONTINUE

            DISPLAY "PROCESANDO INFORMACION " AT 19,2


LET txt1 = "cd ",reg_ruta.ruta_rescate CLIPPED,";",
           "sed -e /^$/d ",reg_1.nom_archivo CLIPPED ," > ",
           reg_ruta.ruta_rescate CLIPPED,"/PASO"
RUN txt1

LET txt1 = "cd ",reg_ruta.ruta_rescate CLIPPED,";mv PASO ",
            ruta_archivo
           
RUN txt1
               LOAD FROM ruta_archivo  DELIMITER "¨"
               INSERT INTO sube_registro_ic

  DELETE FROM sube_registro_ic
  WHERE  registro is NULL

               SELECT COUNT(*)
               INTO   cuantos 
               FROM   sube_registro_ic

               IF cuantos = 0 OR 
                  cuantos IS NULL THEN
                  DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                  AT 19,2 ATTRIBUTE(REVERSE)
                  SLEEP 3
                  NEXT FIELD nom_archivo
               ELSE
		--LET cuantos = cuantos - 2
                DISPLAY "FOLIO                : ",v_folio_interno  AT 16,2
                DISPLAY " REGISTROS A PROCESAR: ",cuantos AT 17,2 
                EXIT INPUT
               END IF
           WHENEVER ERROR STOP

       ON KEY (INTERRUPT)
           PROMPT "PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
   END INPUT
   
  CALL clasifica() 

END MAIN

FUNCTION init()   
#i-------------

    LET HOY                         =                                TODAY
    LET HORA                        =                                TIME
    LET cuantos                     =                                0
    LET cuenta                      =                                0
    LET g_nom_prog                  =                                "TRAC070"

    SELECT codigo_afore,
           razon_social
      INTO g_glob.*
      FROM safre_af:tab_afore_local

    CREATE TEMP TABLE sube_registro_ic(reg CHAR(700))

    SELECT * ,USER
    INTO   reg_ruta.*,usuario
    FROM   seg_modulo   
    WHERE  modulo_cod = "tra"

    SELECT MAX(folio_interno) + 1
    INTO   v_folio_interno
    FROM   tra_folio_interno
  
    INSERT INTO tra_folio_interno VALUES(v_folio_interno,HOY)
 
    DISPLAY "FOLIO : ",v_folio_interno  AT 16,30
 
END FUNCTION

FUNCTION clasifica()
#c------------------
    CALL criterio_1()
    PROMPT " PROCESO FINALIZADO <ENTER> PARA SALIR..." 
    FOR CHAR enter
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
     DEFINE      f_ultapor       CHAR(011)
     DEFINE      f_saldo_1       CHAR(011)
     DEFINE      f_saldo_2       CHAR(011)
     DEFINE      f_aport         CHAR(011)
     DEFINE      sw smallint

LET sw = 1

     DECLARE cur_inserta CURSOR FOR
     SELECT A.* 
     FROM   sube_registro_ic A

   FOREACH cur_inserta INTO registro
    
-- CASE registro[1,2] 
-- WHEN '02'
      LET reg_tra_det_atm_issste.cve_ced_cuenta    = registro[001,003]
      LET reg_tra_det_atm_issste.n_seguro_ent      = registro[004,014]
      LET reg_tra_det_atm_issste.rfc_ent           = registro[015,027]
      LET reg_tra_det_atm_issste.nro_ctrl_icefa    = registro[028,057]
      LET reg_tra_det_atm_issste.curp_ent          = registro[058,075]
      LET reg_tra_det_atm_issste.nombre_ent        = registro[088,207]
      LET reg_tra_det_atm_issste.n_seguro          = " "

      LET f_aport                                  = registro[213,214],"/",
                                                    registro[216,217],"/",
                                                    registro[208,211]
      LET reg_tra_det_atm_issste.fecha_nacimiento  = f_aport
      LET reg_tra_det_atm_issste.marca_viv         = registro[218,218]
      LET reg_tra_det_atm_issste.marca_retiro      = registro[219,219]
      LET reg_tra_det_atm_issste.bimestres_acum    = registro[220,222]
      LET reg_tra_det_atm_issste.rfc_patronal      = registro[223,235]
      LET reg_tra_det_atm_issste.rmo_pag_issste    = registro[236,247]
      LET reg_tra_det_atm_issste.nom_patron        = registro[248,287]
      LET f_ultapor                                = registro[293,294],"/",
                                                    registro[296,297],"/",
                                                    registro[288,291]
      LET reg_tra_det_atm_issste.fecha_ult_apor    = f_ultapor

      LET f_saldo_1 = registro[317,324] , ".",registro[325,326]
      LET reg_tra_det_atm_issste.sar_92_issste     = f_saldo_1
 
      LET f_saldo_1 = registro[327,334] , ".",registro[335,336]
      LET reg_tra_det_atm_issste.viv_92_issste     = f_saldo_1
 
      LET reg_tra_det_atm_issste.nombre_siri       = registro[337,376]
      LET reg_tra_det_atm_issste.paterno_siri      = registro[377,416]
      LET reg_tra_det_atm_issste.materno_siri      = registro[417,456]
      LET reg_tra_det_atm_issste.tipo_criterio     = 0
      LET reg_tra_det_atm_issste.estado            = 0 
      LET reg_tra_det_atm_issste.fecha_edo         = HOY
      LET reg_tra_det_atm_issste.diagnostico       = " "
      LET reg_tra_det_atm_issste.folio_interno     = v_folio_interno
      LET reg_tra_det_atm_issste.correlativo       = 0
      LET reg_tra_det_atm_issste.cad_valida        = "00000000"
      LET reg_tra_det_atm_issste.liga_correlativo  = NULL     
      LET reg_tra_det_atm_issste.fecha_genera      = NULL     
      LET reg_tra_det_atm_issste.usuario           = usuario 


      INSERT INTO tra_det_atm_issste VALUES (reg_tra_det_atm_issste.*)
     
      LET cuenta = cuenta + 1 
         DISPLAY " REGISTROS PROCESADOS: ",cuenta AT 18,2
{
   -- EXIT CASE
   WHEN '01'
     LET reg_tra_cza_aut_issste.folio_interno     = v_folio_interno
     LET reg_tra_cza_aut_issste.ident_servicio     = registro[3,4]
     LET reg_tra_cza_aut_issste.ident_operacion    = registro[5,6]
     LET reg_tra_cza_aut_issste.tipo_ent_origen    = registro[7,8]
     LET reg_tra_cza_aut_issste.cve_ent_origen     = registro[9,11]
     LET reg_tra_cza_aut_issste.tipo_ent_dest      = registro[12,13]
     LET reg_tra_cza_aut_issste.fecha_presentacion = registro[20,27]

     INSERT INTO tra_cza_aut_issste VALUES( reg_tra_cza_aut_issste.*)
    EXIT CASE
   WHEN '09'
      LET reg_tra_sum_aut_issste.folio_interno =  v_folio_interno
      LET reg_tra_sum_aut_issste.tot_regs = registro[3,11]

      INSERT INTO tra_sum_aut_issste VALUES( reg_tra_sum_aut_issste.*)
    EXIT CASE
    OTHERWISE 
    EXIT CASE
    END CASE
}
   END FOREACH

     LET reporte  = reg_ruta.ruta_listados CLIPPED,"/",usuario CLIPPED,
                    ".TRAC070.",
                    HOY USING"YYYYMMDD",
		    HORA CLIPPED
     START REPORT rpt_0 TO reporte


     DECLARE cur_criterio CURSOR FOR
        SELECT a.folio_interno,a.cve_ced_cuenta,a.tipo_criterio,count(*)
        FROM   tra_det_atm_issste a
        WHERE  a.folio_interno = v_folio_interno
        GROUP BY 1,2,3
        ORDER BY 1,2,3


     FOREACH cur_criterio INTO   reg_rep.folio_interno ,
                                 reg_rep.cve_ced_cuenta,
                                 reg_rep.criterio      ,
                                 reg_rep.tot_criterio

     SELECT icefa_desc
     INTO   reg_rep.cve_desc
     FROM   tab_icefa
     WHERE  icefa_cod = reg_rep.cve_ced_cuenta

     SELECT COUNT(*)
     INTO   r_tot
     FROM   tra_det_atm_issste
     WHERE  folio_interno = v_folio_interno

     LET reg_rep.total          = r_tot

     OUTPUT TO REPORT rpt_0 (reg_rep.*,r_tot)
  -- IF reg_rep.criterio = 1 OR
  --    reg_rep.criterio = 2 THEN

        SELECT A.descripcion   
        INTO   reg_rep.criterio_desc
        FROM   tra_tab_valcri A
        WHERE  A.criterio_cod = reg_rep.criterio

   --  INSERT INTO tra_ctr_folio VALUES (reg_rep.folio_interno,reg_rep.cve_desc,reg_rep.criterio,reg_rep.criterio_desc)
  --END IF

  END FOREACH
     FINISH REPORT rpt_0

END FUNCTION

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

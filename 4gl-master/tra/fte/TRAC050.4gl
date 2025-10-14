##############################################################################
#Owner             => E.F.P.
#Programa TRAC050  => RECIBE Y CONFRONTA ARCHIVO PARA TRA-AUTOMATICO ICE-AFO
#Fecha creacion    => 22 DE NOVIEMBRE DE 2001
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 09 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af

GLOBALS

DEFINE reporte CHAR(200)
DEFINE usuario CHAR(010)
DEFINE r_tot INTEGER
DEFINE r_unicos INTEGER
DEFINE reg_rep RECORD 
       folio_interno  integer    ,
       cve_ced_cuenta char(003)  ,
       cve_desc       char(010)  ,
       criterio       char(002)  ,
       criterio_desc  char(010)  ,
       total          integer    ,
       unicos         integer 
END RECORD 
       
DEFINE txt1 CHAR(150)
     DEFINE banco                  CHAR(015)
     DEFINE v_criterio             CHAR(015)
     DEFINE HOY                    DATE
     DEFINE v_folio_interno        INTEGER
     DEFINE reg_ruta               RECORD LIKE seg_modulo.*

     DEFINE criterio               CHAR(02) ,
            cuenta                          ,
            cuantos                INTEGER  ,
            ruta_archivo           CHAR(100),
            enter                  CHAR(001)

     DEFINE reg_1                  RECORD 
            nom_archivo            CHAR(020),
            nss_11                 CHAR(001),
            rfc_10                 CHAR(001),
            rfc_13                 CHAR(001),
            nss11_rfc10            CHAR(001),
            nss11_rfc13            CHAR(001),
            nss_10                 CHAR(001)
     END RECORD

     DEFINE reg_tra_det_automatico RECORD LIKE safre_tmp:tra_det_automatico.*

     DEFINE  g_glob            RECORD
             codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
             razon_social      LIKE safre_af:tab_afore_local.razon_social
                               END RECORD
    DEFINE  g_nom_prog         CHAR(07) 


END GLOBALS

MAIN
    DEFER INTERRUPT      
    OPTIONS
    ACCEPT KEY CONTROL-I ,
    INPUT WRAP           ,
    PROMPT LINE LAST

   CALL init()

   OPEN WINDOW trac0501 AT 4,4 WITH FORM "TRAC0501" ATTRIBUTE(BORDER)
   DISPLAY " TRAC050     CARGA ARCHIVO TRASPASO AUTOMATICO ICE-AFO-IMSS                    " AT 3,1 ATTRIBUTE(REVERSE)


   DISPLAY "                           < Ctrl-C > Salir                                              " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg_1.* WITHOUT DEFAULTS

      AFTER FIELD nom_archivo
	       IF reg_1.nom_archivo IS NULL THEN
	           ERROR "Campo NO puede ser NULO"
	           NEXT FIELD nom_archivo
	       END IF

      AFTER FIELD nss_11
	       IF reg_1.nss_11 <> "X" THEN
	          ERROR "SOLO PUEDE MARCAR CON 'X' "
	          NEXT FIELD nss_11
	       END IF

      AFTER FIELD rfc_10
	       IF reg_1.rfc_10 <> "X" THEN
	          ERROR "SOLO PUEDE MARCAR CON 'X' "
	          NEXT FIELD rfc_10
	       END IF

      AFTER FIELD rfc_13
	       IF reg_1.rfc_13 <> "X" THEN
	          ERROR "SOLO PUEDE MARCAR CON 'X' "
	          NEXT FIELD rfc_13
	       END IF

      AFTER FIELD nss11_rfc10
	         IF reg_1.nss11_rfc10 <> "X" THEN
	            ERROR "SOLO PUEDE MARCAR CON 'X' "
	            NEXT FIELD nss11_rfc10
	         END IF

      AFTER FIELD nss11_rfc13
	         IF reg_1.nss11_rfc13 <> "X" THEN
	            ERROR "SOLO PUEDE MARCAR CON 'X' "
	            NEXT FIELD nss11_rfc13
	         END IF

      AFTER FIELD nss_10
	       IF reg_1.nss_10 <> "X" THEN
	          ERROR "SOLO PUEDE MARCAR CON 'X' "
	          NEXT FIELD nss_10
	       END IF

      ON KEY (ESC)
           IF reg_1.nss_11 = "X" THEN
               LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
                                  reg_1.nom_archivo CLIPPED
               LET criterio      = "01"
           END IF


	        IF reg_1.rfc_10 = "X" THEN
               LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
                                  reg_1.nom_archivo CLIPPED
               LET criterio      = "02"
           END IF

           IF reg_1.rfc_13 = "X" THEN
               LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
                                  reg_1.nom_archivo CLIPPED
               LET criterio      = "03"
           END IF
	        IF reg_1.nss11_rfc10    = "X" THEN
               LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
                                  reg_1.nom_archivo CLIPPED
               LET criterio      = "04"
           END IF

	        IF reg_1.nss11_rfc13 = "X" THEN
               LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
                                  reg_1.nom_archivo CLIPPED
               LET criterio      = "05"
           END IF

	        IF reg_1.nss_10 = "X" THEN
               LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
                                  reg_1.nom_archivo CLIPPED
               LET criterio      = "06"
           END IF

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
               INSERT INTO safre_tmp:sube_registro

  DELETE FROM safre_tmp:sube_registro
  WHERE  registro is NULL

               SELECT COUNT(*)
               INTO   cuantos 
               FROM   safre_tmp:sube_registro

               IF cuantos = 0 OR 
                  cuantos IS NULL THEN
                  DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                  AT 19,2 ATTRIBUTE(REVERSE)
                  SLEEP 3
                  NEXT FIELD nom_archivo
               ELSE
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

    LET HOY     = TODAY
    LET cuantos = 0
    LET cuenta  = 0

    DELETE   FROM  safre_tmp:sube_registro

  

    SELECT * ,USER
    INTO   reg_ruta.*,usuario
    FROM   seg_modulo   
    WHERE  modulo_cod = "tra"

#====== Se Modifico  ya q no existe la tbla ==> safre_tmp:tra_folio_interno===
#====== existe en safre_af:tra_folio_interno
   {SELECT MAX(folio_interno) + 1
    INTO   v_folio_interno
    FROM   safre_tmp:tra_folio_interno
  
    INSERT INTO safre_tmp:tra_folio_interno VALUES(v_folio_interno,HOY)}

    SELECT MAX(folio_interno) + 1
    INTO   v_folio_interno
    FROM   safre_af:tra_folio_interno
  
    INSERT INTO safre_af:tra_folio_interno VALUES(v_folio_interno,HOY)
#=========
 
    DISPLAY "FOLIO : ",v_folio_interno  AT 16,30
    
    SELECT codigo_afore,
           razon_social
      INTO g_glob.*
      FROM safre_af:tab_afore_local

    LET g_nom_prog                 =                   "TRAC050"

 
END FUNCTION

FUNCTION clasifica()
#c------------------
    CASE criterio
        WHEN "01"
            CALL criterio_1()
            EXIT CASE
        WHEN "02"
            CALL criterio_2()
            EXIT CASE
        WHEN "03"
            CALL criterio_3()
            EXIT CASE
        WHEN "04"
            CALL criterio_4() 
            EXIT CASE
        WHEN "05"
            CALL criterio_5()
            EXIT CASE
        WHEN "06"
            CALL criterio_6()
            EXIT CASE
        OTHERWISE 
            EXIT CASE
    END CASE
    PROMPT " PROCESO FINALIZADO <ENTER> PARA SALIR..." 
    FOR CHAR enter
END FUNCTION


FUNCTION criterio_1()
#c1------------------

     DEFINE v_rowid    INTEGER

     CALL inserta(1)

END FUNCTION

FUNCTION criterio_2()
#c2------------------

     DEFINE v_rowid     INTEGER
     DEFINE nss_10      CHAR(010)

     CALL inserta(2)

END FUNCTION

FUNCTION criterio_3()
#c3------------------

     DEFINE v_rowid      INTEGER

     CALL inserta(3)

END FUNCTION

FUNCTION criterio_4()
#c4------------------

     DEFINE v_rowid         INTEGER
     DEFINE rfc_10          CHAR(010)
    
     CALL inserta(4)

END FUNCTION

FUNCTION criterio_5()
#c5------------------

     DEFINE v_rowid         INTEGER
    
     CALL inserta(5)

END FUNCTION

FUNCTION criterio_6()

     DEFINE v_rowid         INTEGER
     DEFINE rfc_10          CHAR(010)
    
     CALL inserta(6)

END FUNCTION

FUNCTION inserta(f_criterio)
#ins------------------------

     DEFINE      registro        CHAR(600)
     DEFINE      f_criterio      SMALLINT
     DEFINE      f_saldo_1       CHAR(011)
     DEFINE      f_saldo_2       CHAR(011)
     DEFINE      f_aport         CHAR(011)
     DEFINE      sw smallint

LET sw = 1

     DECLARE cur_inserta CURSOR FOR
     SELECT A.* 
     FROM   safre_tmp:sube_registro A



     FOREACH cur_inserta INTO registro

     LET reg_tra_det_automatico.cve_ced_cuenta    = registro[001,003]
     LET reg_tra_det_automatico.n_seguro_ent      = registro[004,014]
     LET reg_tra_det_automatico.rfc_ent           = registro[015,027]
     LET reg_tra_det_automatico.nro_ctrl_icefa    = registro[028,057]
     LET reg_tra_det_automatico.nombre_ent        = registro[088,207]
     LET f_aport                                  = registro[213,214],"/",
                                                    registro[216,217],"/",
                                                    registro[208,211]
     LET reg_tra_det_automatico.fecha_nacimiento  = f_aport
     LET reg_tra_det_automatico.marca_viv         = registro[218,218]
     LET reg_tra_det_automatico.marca_retiro      = registro[219,219]
     LET reg_tra_det_automatico.bimestres_acum    = registro[220,223]
     LET reg_tra_det_automatico.rfc_patronal      = registro[224,236]
     LET reg_tra_det_automatico.reg_patronal      = registro[237,247]
     LET reg_tra_det_automatico.nombre_patron     = registro[248,287]
     LET reg_tra_det_automatico.exp_infonavit     = registro[288,296]
     LET f_saldo_1 = registro[297,304] , ".",registro[305,306]
     LET reg_tra_det_automatico.saldo_sar_92      = f_saldo_1
     LET f_saldo_1 = registro[307,314] , ".",registro[315,316]
     LET reg_tra_det_automatico.saldo_viv_92      = f_saldo_1
     LET f_saldo_1 = registro[317,324] , ".",registro[325,326]
     LET reg_tra_det_automatico.sar_92_issste     = f_saldo_1
     LET f_saldo_1 = registro[327,334] , ".",registro[335,336]
     LET reg_tra_det_automatico.viv_92_issste     = f_saldo_1
     LET reg_tra_det_automatico.cve_afore         = registro[337,339]
     LET reg_tra_det_automatico.n_seguro          = registro[340,350]
     LET reg_tra_det_automatico.rfc               = registro[351,363]
     LET reg_tra_det_automatico.tipo_criterio     = f_criterio
     LET reg_tra_det_automatico.estado            = 0 
     LET reg_tra_det_automatico.fecha_edo         = HOY
     LET reg_tra_det_automatico.diagnostico       = " "
     LET reg_tra_det_automatico.folio_interno     = v_folio_interno
     LET reg_tra_det_automatico.correlativo       = 0
     LET reg_tra_det_automatico.cad_valida        = "000"
     LET reg_tra_det_automatico.liga_correlativo = NULL     

     INSERT INTO safre_tmp:tra_det_automatico VALUES (reg_tra_det_automatico.*)

     
    LET cuenta = cuenta + 1 
    DISPLAY " REGISTROS PROCESADOS: ",cuenta AT 18,2
     IF sw = 1 THEN
       SELECT icefa_desc
       INTO   banco
       FROM   tab_icefa 
       WHERE icefa_cod = reg_tra_det_automatico.cve_ced_cuenta

       SELECT criterio_desc
       INTO   v_criterio
       FROM   safre_af:tra_aut_criterio
       WHERE  criterio_cod  = reg_tra_det_automatico.tipo_criterio

       INSERT INTO safre_af:tra_ctr_folio VALUES (v_folio_interno,
                                         banco,
					 reg_tra_det_automatico.tipo_criterio,
                                         v_criterio)
      
     LET sw = 0
     END IF
     END FOREACH

     SELECT COUNT(*) 
     INTO   r_tot
     FROM   safre_tmp:tra_det_automatico
     WHERE  folio_interno = v_folio_interno

     LET reg_rep.folio_interno  = v_folio_interno
     LET reg_rep.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta
     LET reg_rep.cve_desc       = banco
     LET reg_rep.criterio       = f_criterio
     LET reg_rep.criterio_desc  = v_criterio
     LET reg_rep.total          = r_tot
     LET reg_rep.unicos         = r_unicos 

     LET reporte  = reg_ruta.ruta_listados CLIPPED,"/",usuario CLIPPED,
                    ".RCRUCE.C",
                    f_criterio USING"&",
                    ".I",
                    reg_tra_det_automatico.cve_ced_cuenta CLIPPED  ,    
		    ".",
                    HOY USING"YYYYMMDD"

     START REPORT rpt_0 TO reporte
           OUTPUT TO REPORT rpt_0 (reg_rep.*)
     FINISH REPORT rpt_0

END FUNCTION

REPORT rpt_0(reg_rep)
#r0-----------------

DEFINE reg_rep RECORD 
       folio_interno  integer  ,
       cve_ced_cuenta char(003),
       cve_desc       char(010),
       criterio       char(002),
       criterio_desc  char(010),
       total          integer ,
       unicos         integer 
END RECORD 

FORMAT 
	PAGE HEADER
        PRINT                                                                 
        SKIP 2 LINES                                                         
        PRINT                                                                
            COLUMN 02,g_nom_prog  CLIPPED,
            COLUMN 50, TODAY USING "DD/MM/YYYY"
        PRINT

        PRINT COLUMN 030,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED
        
	PRINT 
	PRINT
            COLUMN 14,  
               "REPORTE DE CARGA DE ARCHIVO DE CRUCE SAR 92 IMSS "
	PRINT
 	PRINT
        SKIP 10 LINES
        PRINT COLUMN 5,"----------------------------------------------------"
        PRINT COLUMN 5,"Folio   : ",reg_rep.folio_interno
        PRINT COLUMN 5,"Banco   : ",reg_rep.cve_ced_cuenta," ",reg_rep.cve_desc
        PRINT COLUMN 5,"Criterio: ",reg_rep.criterio," ",reg_rep.criterio_desc
        PRINT COLUMN 5,"----------------------------------------------------"
        PRINT
	     PRINT COLUMN 15,"TOTAL  : ",reg_rep.total
        PRINT
        PRINT COLUMN 5,"----------------------------------------------------"

END REPORT

##############################################################################
#Owner             => E.F.P.
#Programa TRAM062  => CONFRONTA EXCEL TRA-AUTO-ICE-AFO IMSS         
#Fecha creacion    => SIN FECHA DE CREACION
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 21 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS   
##############################################################################
DATABASE safre_af 
GLOBALS
   DEFINE aceptadas ,
	  aceptada_dup,
	  edo_dif  , total  INTEGER

   DEFINE reg_carga_excel RECORD LIKE safre_af:tra_carga_excel.*
   DEFINE ruta_cruce CHAR(300)
   DEFINE txt      CHAR(1000)
   DEFINE ffb      SMALLINT
   DEFINE x_busca  CHAR(100)
   DEFINE usuario  CHAR(010)
   DEFINE v_mod SMALLINT
   DEFINE rr SMALLINT
   DEFINE vtipo SMALLINT
   DEFINE vestado  SMALLINT
   DEFINE ruta char(100)
   DEFINE reg_tra_det_automatico RECORD LIKE safre_tmp:tra_det_automatico.*
   DEFINE v_corr integer
   DEFINE sw smallint
   DEFINE reg_param_tra RECORD LIKE seg_modulo.*
   DEFINE band  CHAR(01)

    DEFINE #glo #char
        enter                 CHAR(1) ,
        HORA                  CHAR(8)
     DEFINE v_folio_interno INTEGER

     DEFINE v_correlativo INTEGER
     DEFINE nom_arch      CHAR(20)

     DEFINE HOY             DATE

     DEFINE b SMALLINT
     DEFINE nomb_prog       CHAR(07)
          
     DEFINE  g_glob            RECORD
             codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
             razon_social      LIKE safre_af:tab_afore_local.razon_social
                               END RECORD
END GLOBALS  

MAIN

    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           , PROMPT LINE LAST     ,
        MESSAGE LINE LAST

     CALL init()
     CALL despliega_folios() 

END MAIN

FUNCTION despliega_folios()
#-------------------------


DEFINE f_b SMALLINT
DEFINE arr_tra_ctr_folio ARRAY[100] OF RECORD LIKE safre_tmp:tra_ctr_folio.*

DEFINE reg_tra_ctr_folio RECORD LIKE safre_tmp:tra_ctr_folio.*

DEFINE arr_c                ,
       i             SMALLINT


LET f_b = 1
    OPEN WINDOW tram0621 AT 2,2 WITH FORM "TRAM0621" ATTRIBUTE(BORDER)
    DISPLAY" TRAM062          CONFRONTA EXCEL TRA-AUTO-ICE-AFO IMSS                        " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY "                          < CTRL-C> Salir                                      " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME nom_arch 

    AFTER FIELD nom_arch
     IF nom_arch IS NULL THEN
	ERROR"NOMBRE DE ARCHIVO NO PUEDE SER NULO..."
	NEXT FIELD nom_arch   
     END IF

    ON KEY (ESC)
     IF nom_arch IS NULL THEN
	ERROR"NOMBRE DE ARCHIVO NO PUEDE SER NULO..."
	NEXT FIELD nom_arch   
     ELSE 
	EXIT INPUT
     END IF
      
     ON KEY (INTERRUPT) 
       EXIT PROGRAM

    END INPUT


LET aceptadas = 0
LET aceptada_dup = 0
LET edo_dif  = 0
LET total  = 0
    
      DISPLAY "PROCESANDO INFORMACION ..." at 19,2 ATTRIBUTE(REVERSE)
      DISPLAY " "

     LET ruta_cruce = reg_param_tra.ruta_rescate CLIPPED,"/",nom_arch CLIPPED

     LOAD FROM  ruta_cruce INSERT INTO safre_tmp:sube_reg

      CALL despliega_confronta()

     DISPLAY BY NAME total

     SELECT "X" 
     FROM rep_aut
     GROUP BY 1
     
     IF STATUS <> NOTFOUND THEN     #SI ENCONTRO INFORM.EN rep_aut

        START REPORT listado_1 TO ruta

        DECLARE cur_rep CURSOR FOR

         SELECT A.*
         FROM  safre_tmp:tra_det_automatico A
         WHERE  A.correlativo IN 
               (SELECT unique H.correlativo
                FROM   rep_aut H)
         AND    A.estado IN (14,15,16,17,50)
         ORDER BY A.estado

         FOREACH cur_rep INTO  reg_tra_det_automatico.*
            OUTPUT TO REPORT listado_1(reg_tra_det_automatico.*)
         END FOREACH 
         FINISH REPORT  listado_1
     DISPLAY "REPORTE GENERADO EN: ",ruta CLIPPED AT 19,1 ATTRIBUTE(REVERSE)
     END IF

     PROMPT "PROCESO FINALIZADO....<ENTER> PARA SALIR.." FOR CHAR ENTER
     CLOSE WINDOW TRAM0621 
     EXIT PROGRAM
END FUNCTION

FUNCTION despliega_confronta()
#dc---------------------------

      DECLARE cur_cruce CURSOR FOR 
      SELECT A.*
      FROM    safre_tmp:sube_reg A
      ORDER  BY A.correlativo

      FOREACH cur_cruce INTO reg_carga_excel.*

      LET total = total + 1

      SELECT A.* 
      INTO   reg_tra_det_automatico.*
      FROM   safre_tmp:tra_det_automatico A
      WHERE  A.correlativo = reg_carga_excel.correlativo


      IF STATUS = NOTFOUND THEN
	  LET edo_dif = edo_dif + 1
	  DISPLAY BY NAME edo_dif
	  INSERT INTO safre_tmp:tra_cruce_excel_edo 
	  VALUES(reg_carga_excel.correlativo   , 
		 100)
          CONTINUE FOREACH
      END IF

      CASE reg_tra_det_automatico.estado
      WHEN 12 
	  EXIT CASE
      OTHERWISE  
	  LET edo_dif = edo_dif + 1
	  DISPLAY BY NAME edo_dif
	  INSERT INTO safre_tmp:tra_cruce_excel_edo 
	  VALUES(reg_tra_det_automatico.correlativo   , 
		 reg_tra_det_automatico.estado)
          CONTINUE FOREACH
         EXIT CASE
      END CASE

        SELECT "OK"
        FROM   safre_tmp:tra_det_automatico  a
        WHERE  a.n_seguro       = reg_tra_det_automatico.n_seguro 
        AND    a.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
        AND    a.rfc_ent        = reg_tra_det_automatico.rfc_ent
        AND    a.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta
        AND    a.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
        AND    a.estado   <> 0
        AND    a.correlativo <>   reg_tra_det_automatico.correlativo
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
     
           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 50
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  reg_tra_det_automatico.correlativo 

           LET v_mod = 50
	  LET aceptadas = aceptadas + 1
	  DISPLAY BY NAME aceptadas
        ELSE

        SELECT "OK"
        FROM   safre_tmp:tra_det_automatico a
        WHERE  a.n_seguro       = reg_tra_det_automatico.n_seguro 
        AND    a.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
        AND    a.rfc_ent        = reg_tra_det_automatico.rfc_ent
        AND    a.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta
        AND    a.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
        AND    a.estado   IN (10,11,14,15,27,30,32,34,36,38,40,50)
        AND    a.correlativo <>   reg_tra_det_automatico.correlativo
        GROUP BY 1

         IF STATUS <> NOTFOUND THEN

           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 15
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  reg_tra_det_automatico.correlativo 

           LET v_mod = 15

	  LET aceptada_dup = aceptada_dup + 1
	  DISPLAY BY NAME aceptada_dup
         ELSE 

           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 50
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  reg_tra_det_automatico.correlativo 

           LET v_mod = 50
	  LET aceptadas = aceptadas + 1
	  DISPLAY BY NAME aceptadas

         END IF
       END IF
      
      INSERT INTO rep_aut 
      VALUES  (reg_tra_det_automatico.correlativo)

      END FOREACH

INSERT INTO safre_af:tra_carga_excel
SELECT * FROM safre_tmp:sube_reg

END FUNCTION

FUNCTION init() 
#i------------- 

LET nomb_prog                          =                  "TRAM062"
LET HOY                                =                   TODAY 
LET HORA                               =                   TIME
LET aceptadas                          =                   0
LET aceptada_dup                       =                   0
LET edo_dif                            =                   0
LET total                              =                   0


SELECT codigo_afore,
       razon_social
INTO g_glob.*
FROM safre_af:tab_afore_local


DATABASE safre_tmp
         create temp table sube_reg(correlativo integer)
DATABASE safre_af

create temp table rep_aut(correlativo integer)

    SELECT *
    INTO   reg_param_tra.*
    FROM  seg_modulo
    WHERE modulo_cod = "tra"

    SELECT user
    INTO   usuario
    FROM   tab_afore_local

    LET ruta = reg_param_tra.ruta_listados       CLIPPED,
    	       "/",usuario CLIPPED ,".MRECH." ,
	       HOY USING "YYYYMMDD"    CLIPPED,".",
               HORA CLIPPED

END FUNCTION

REPORT listado_1(reg_tra_det_automatico)
#l1--------------------------------------

DEFINE reg_tra_det_automatico RECORD LIKE safre_tmp:tra_det_automatico.*
DEFINE i_cont_reg_total integer
DEFINE i_cont_reg_orig integer
    OUTPUT
        PAGE LENGTH 90

    FORMAT

    PAGE HEADER
IF sw = 0 
THEN LET i_cont_reg_total = 0
LET i_cont_reg_orig = 0
LET sw = 1
END IF
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT
            COLUMN 001,HOY USING"DD-MM-YYYY" ,
            COLUMN 155,nomb_prog CLIPPED 
        PRINT
            COLUMN 001,HORA,
            COLUMN 050,"REPORTE DE VALIDACION MANUAL CRUCE EXCEL SAR 92",
                       " TRA-ICE-AFO  IMSS",

             COLUMN 149,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED
       PRINT
       PRINT
            COLUMN 001,"NSS"         ,
            COLUMN 013,"NSS ICEFA"   ,
            COLUMN 025,"RFC ICEFA"   ,
            COLUMN 040,"BANCO"       ,
            COLUMN 049,"NRO.INTERNO" 
            PRINT
            COLUMN 001,"NOMBRE"      
       PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT

    BEFORE GROUP OF reg_tra_det_automatico.estado
        LET i_cont_reg_orig = 0
        PRINT
        PRINT
       PRINT
            COLUMN 001,"----------------------------------------------------",
                       "----------------------------------------------------",
                       "----------------------------------------------------",
                       "-----------------" 
        PRINT
        PRINT
            COLUMN 001,"ULTIMO ESTADO :",
                       reg_tra_det_automatico.estado 
        PRINT
        PRINT
    ON EVERY ROW
        LET i_cont_reg_orig  = i_cont_reg_orig + 1
        PRINT
            COLUMN 001,reg_tra_det_automatico.n_seguro             ,
            COLUMN 013,reg_tra_det_automatico.n_seguro_ent         ,
            COLUMN 025,reg_tra_det_automatico.rfc_ent              ,
            COLUMN 040,reg_tra_det_automatico.cve_ced_cuenta       ,
            COLUMN 049,reg_tra_det_automatico.nro_ctrl_icefa       
            PRINT
            COLUMN 001,reg_tra_det_automatico.nombre_ent[1,40]     ,
            COLUMN 042,reg_tra_det_automatico.nombre_ent[41,80]    ,
            COLUMN 084,reg_tra_det_automatico.nombre_ent[81,120]
        PRINT
            
    AFTER GROUP OF reg_tra_det_automatico.estado
        PRINT
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS :",reg_tra_det_automatico.estado,
                       " :          ",i_cont_reg_orig USING"#########"
        PRINT 	

    ON LAST ROW
      SELECT COUNT(unique correlativo) 
      INTO i_cont_reg_total
      FROM rep_aut
        PRINT	
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS MODIFICADOS MANUALMENTE :       ",
                       i_cont_reg_total USING"#########"
        PRINT      
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
      
END REPORT

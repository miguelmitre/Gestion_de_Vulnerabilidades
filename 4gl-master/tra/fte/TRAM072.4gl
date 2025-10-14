##############################################################################
#Owner             => E.F.P.
#Programa TRAM072  => CONFIRMACION MASIVA DE INVITACION ISSSTE
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 19 DE ENERO DEL 2005
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af
GLOBALS
   DEFINE aceptadas ,
	  aceptada_dup,
	  edo_dif  , total  INTEGER

   DEFINE reg_carga_excel RECORD LIKE tra_carga_excel.*
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
   DEFINE reg_tra_det_aut_issste RECORD LIKE tra_det_aut_issste.*
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
DEFINE arr_tra_ctr_folio ARRAY[100] OF RECORD LIKE tra_ctr_folio.*

DEFINE reg_tra_ctr_folio RECORD LIKE tra_ctr_folio.*

DEFINE arr_c                ,
       i             SMALLINT


LET f_b = 1
    OPEN WINDOW tram0721 AT 2,2 WITH FORM "TRAM0721" ATTRIBUTE(BORDER)
    DISPLAY" TRAM072  CONFIRMACION PLANO DE CORRELATIVOS ICEFA-AFORE ISSSTE                " AT 3,1 ATTRIBUTE(REVERSE)   

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

     LET ruta_cruce = reg_param_tra.ruta_rescate CLIPPED,"/",nom_arch CLIPPED

     LOAD FROM  ruta_cruce INSERT INTO sube_reg

      CALL despliega_confronta()

     DISPLAY BY NAME total

     SELECT "OK" 
     FROM rep_aut
     GROUP BY 1

     IF STATUS <> NOTFOUND THEN

        START REPORT listado_1 TO ruta

        DECLARE cur_rep CURSOR FOR

         SELECT A.*
         FROM  tra_det_aut_issste A
         WHERE  A.correlativo IN 
               (SELECT unique H.correlativo
                FROM   rep_aut H)
        -- AND    A.estado IN (160)
         ORDER BY A.estado

         FOREACH cur_rep INTO  reg_tra_det_aut_issste.*
            OUTPUT TO REPORT listado_1(reg_tra_det_aut_issste.*)
         END FOREACH 
         FINISH REPORT  listado_1
     END IF
     PROMPT "PROCESO FINALIZADO....<ENTER> PARA SALIR.." FOR CHAR ENTER
     CLOSE WINDOW TRAM0721 
     EXIT PROGRAM
END FUNCTION

FUNCTION despliega_confronta()
#dc---------------------------

      DECLARE cur_cruce CURSOR FOR 
      SELECT A.*
      FROM    sube_reg A
      ORDER  BY A.correlativo

      FOREACH cur_cruce INTO reg_carga_excel.*

      LET total = total + 1

      SELECT A.* 
      INTO   reg_tra_det_aut_issste.*
      FROM   tra_det_aut_issste A
      WHERE  A.correlativo = reg_carga_excel.correlativo

      IF STATUS = NOTFOUND THEN
	     LET edo_dif = edo_dif + 1
	     DISPLAY BY NAME edo_dif
	     INSERT INTO tra_cruce_excel_edo VALUES(reg_carga_excel.correlativo,100)
        CONTINUE FOREACH
      END IF

      CASE reg_tra_det_aut_issste.estado
      WHEN 112 
	    EXIT CASE
      WHEN 110
	    EXIT CASE
      OTHERWISE  
	    LET edo_dif = edo_dif + 1
	    DISPLAY BY NAME edo_dif
	    INSERT INTO tra_cruce_excel_edo 
       VALUES(reg_tra_det_aut_issste.correlativo   , 
		        reg_tra_det_aut_issste.estado)
       INSERT INTO rep_aut 
       VALUES  (reg_tra_det_aut_issste.correlativo)
       CONTINUE FOREACH
         EXIT CASE
      END CASE

        SELECT "OK"
        FROM   tra_det_aut_issste  a
        WHERE  a.n_seguro       = reg_tra_det_aut_issste.n_seguro 
        AND    a.n_seguro_ent   = reg_tra_det_aut_issste.n_seguro_ent
        AND    a.rfc_ent        = reg_tra_det_aut_issste.rfc_ent
        AND    a.cve_ced_cuenta = reg_tra_det_aut_issste.cve_ced_cuenta
        AND    a.nro_ctrl_icefa = reg_tra_det_aut_issste.nro_ctrl_icefa
        AND    a.nombre_ent     = reg_tra_det_aut_issste.nombre_ent
        AND    a.estado   <> 0
        AND    a.correlativo <>   reg_tra_det_aut_issste.correlativo
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
     
           UPDATE tra_det_aut_issste
           SET    tra_det_aut_issste.estado          = 160     ,
                  tra_det_aut_issste.cad_valida[4,5] = "88"   ,
                  tra_det_aut_issste.fecha_edo       = TODAY,
                  tra_det_aut_issste.usuario         = usuario
           WHERE  tra_det_aut_issste.correlativo = 
                  reg_tra_det_aut_issste.correlativo 

           LET v_mod = 160
	        LET aceptadas = aceptadas + 1
	        DISPLAY BY NAME aceptadas
        ELSE

        SELECT "OK"
        FROM   tra_det_aut_issste a
        WHERE  a.n_seguro       = reg_tra_det_aut_issste.n_seguro 
        AND    a.n_seguro_ent   = reg_tra_det_aut_issste.n_seguro_ent
        AND    a.rfc_ent        = reg_tra_det_aut_issste.rfc_ent
        AND    a.cve_ced_cuenta = reg_tra_det_aut_issste.cve_ced_cuenta
        AND    a.nro_ctrl_icefa = reg_tra_det_aut_issste.nro_ctrl_icefa
        AND    a.nombre_ent     = reg_tra_det_aut_issste.nombre_ent
        AND    (a.estado   IN (110,160,161,163,164,167,168) OR 
                a.estado IN (SELECT G.estado FROM tra_status G))
        AND    a.correlativo <>   reg_tra_det_aut_issste.correlativo
        GROUP BY 1

         IF STATUS <> NOTFOUND THEN

           UPDATE tra_det_aut_issste
           SET    tra_det_aut_issste.estado      = 163,
                  tra_det_aut_issste.fecha_edo   = TODAY,
                  tra_det_aut_issste.usuario     = usuario
           WHERE  tra_det_aut_issste.correlativo = 
                  reg_tra_det_aut_issste.correlativo 

           LET v_mod        = 163
	        LET aceptada_dup = aceptada_dup + 1
	        DISPLAY BY NAME aceptada_dup

         ELSE 
           UPDATE tra_det_aut_issste
           SET    tra_det_aut_issste.estado          = 160,
                  tra_det_aut_issste.cad_valida[4,5] = "88"   ,
                  tra_det_aut_issste.fecha_edo       = TODAY ,
                  tra_det_aut_issste.usuario         = usuario
           WHERE  tra_det_aut_issste.correlativo = 
                  reg_tra_det_aut_issste.correlativo 

           LET v_mod     = 160
	        LET aceptadas = aceptadas + 1
	        DISPLAY BY NAME aceptadas

         END IF
       END IF

      INSERT INTO rep_aut 
      VALUES  (reg_tra_det_aut_issste.correlativo)

      END FOREACH

INSERT INTO tra_carga_excel
SELECT * FROM sube_reg
END FUNCTION

FUNCTION init() 
#i------------- 

LET HOY  = TODAY 
LET HORA = TIME
LET aceptadas = 0
LET aceptada_dup = 0
LET edo_dif  = 0
LET total  = 0

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
    	       "/",usuario CLIPPED ,".TRAM072." ,
	       HOY USING "YYYYMMDD"    CLIPPED,".",
               HORA CLIPPED

END FUNCTION

REPORT listado_1(reg_tra_det_aut_issste)
#l1--------------------------------------

DEFINE reg_tra_det_aut_issste RECORD LIKE tra_det_aut_issste.*
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
            COLUMN 155,"TRAM071"
        PRINT
            COLUMN 001,HORA,
            COLUMN 050,"REPORTE DE VALIDACION MANUAL CRUCE EXCEL SAR 92 TRAS",
                       "PASO I-A",
            COLUMN 149,"AFORE         "
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

    BEFORE GROUP OF reg_tra_det_aut_issste.estado
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
                       reg_tra_det_aut_issste.estado 
        PRINT
        PRINT
    ON EVERY ROW
        LET i_cont_reg_orig  = i_cont_reg_orig + 1
       -- LET i_cont_reg_total = i_cont_reg_total + 1
        PRINT
            COLUMN 001,reg_tra_det_aut_issste.n_seguro             ,
            COLUMN 013,reg_tra_det_aut_issste.n_seguro_ent         ,
            COLUMN 025,reg_tra_det_aut_issste.rfc_ent              ,
            COLUMN 040,reg_tra_det_aut_issste.cve_ced_cuenta       ,
            COLUMN 049,reg_tra_det_aut_issste.nro_ctrl_icefa       
            PRINT
            COLUMN 001,reg_tra_det_aut_issste.nombre_ent[1,40]     ,
            COLUMN 042,reg_tra_det_aut_issste.nombre_ent[41,80]    ,
            COLUMN 084,reg_tra_det_aut_issste.nombre_ent[81,120]
        PRINT
            
    AFTER GROUP OF reg_tra_det_aut_issste.estado
       -- LET i_cont_reg_total = i_cont_reg_total + i_cont_reg_orig
        PRINT
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS :",reg_tra_det_aut_issste.estado,
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



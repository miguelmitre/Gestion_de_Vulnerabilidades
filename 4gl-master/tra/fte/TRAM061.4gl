##############################################################################
#Owner             => E.F.P.
#Programa TRAM061  => CONFRONTA MANUAL DE TRASPASOS ICEFA_AFORE
#Fecha creacion    => SIN FECHA DE CREACION 
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 21 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af 
GLOBALS
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
   DEFINE v_folio_interno       INTEGER
   DEFINE v_correlativo         INTEGER
   DEFINE HOY                   DATE

   DEFINE b                     SMALLINT

   DEFINE  g_glob               RECORD
           codigo_afore         LIKE safre_af:tab_afore_local.codigo_afore,
           razon_social         LIKE safre_af:tab_afore_local.razon_social
                                END RECORD
   DEFINE g_nom_prog            CHAR(07)
   DEFINE enc_inf_rep           INTEGER

END GLOBALS  

MAIN

    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST

     CALL init()

   OPEN WINDOW trac0552  AT 2,2 WITH FORM "TRAC0552" ATTRIBUTE(BORDER)

    MENU "CONFRONTA MANUAL IMSS"
    COMMAND "Individual" "Confronta individual de registros" 
       CALL despliega_folios()
    COMMAND "Excel" "Confronta manual con archivo de Excel"
       CALL uno() #c
    COMMAND "Salir" "Salir del Programa"
          EXIT MENU
  END MENU

END MAIN

FUNCTION despliega_folios()
#-------------------------


DEFINE f_b SMALLINT
DEFINE arr_tra_ctr_folio ARRAY[100] OF RECORD LIKE safre_af:tra_ctr_folio.*

DEFINE reg_tra_ctr_folio RECORD LIKE safre_af:tra_ctr_folio.*

DEFINE arr_c                ,
       i             SMALLINT


LET f_b = 1
    OPEN WINDOW tram0611 AT 2,2 WITH FORM "TRAM0611" ATTRIBUTE(BORDER)
    DISPLAY" TRAM061   CONFRONTA MANUAL TRA-AUTOMATICOS ICE-AFO-IMSS                 " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY "                          < CTRL-C> Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)


    MESSAGE "<ENTER> Elegir folio ..." ATTRIBUTE(REVERSE)

     DECLARE cur_dsply_folio CURSOR FOR
      
     SELECT A.*      
     FROM   safre_af:tra_ctr_folio A
     ORDER BY A.folio_interno

     LET i = 1

     FOREACH cur_dsply_folio
     INTO reg_tra_ctr_folio.*

          LET  arr_tra_ctr_folio[i].* =
               reg_tra_ctr_folio.*

          LET i = i + 1

     END FOREACH

     CALL SET_COUNT(i-1)

     DISPLAY ARRAY arr_tra_ctr_folio
                TO sr_ctr_folio.*

     ON KEY(RETURN) 
        LET arr_c = ARR_CURR()

        INPUT BY NAME vtipo
        AFTER FIELD vtipo 
          IF vtipo IS NULL THEN 
            ERROR"CAMPO NO PUEDE SER NULO"
            NEXT FIELD vtipo 
          END IF 
          IF (vtipo > 2 OR
             vtipo < 1) THEN
            ERROR"1 ACEPTADAS , 2 RECHAZADAS"
            NEXT FIELD vtipo
          END IF
        ON KEY(ESC)
          IF vtipo IS NULL THEN 
            ERROR"CAMPO NO PUEDE SER NULO"
            NEXT FIELD vtipo 
          END IF 
          IF (vtipo > 2 OR
             vtipo < 1) THEN
            ERROR"1 ACEPTADAS , 2 RECHAZADAS"
            NEXT FIELD vtipo
          END IF

          IF vtipo = 1 THEN
             LET vestado = 10
          ELSE 
             LET vestado = 12
          END IF

          CALL despliega_confronta(arr_tra_ctr_folio[arr_c].folio_interno,
                                   arr_tra_ctr_folio[arr_c].cve_desc,
                                   arr_tra_ctr_folio[arr_c].criterio_cod,
                                   arr_tra_ctr_folio[arr_c].criterio_desc )
          EXIT INPUT 
          END INPUT
     ON KEY(INTERRUPT)
         LET f_b = 0
        EXIT DISPLAY
     END DISPLAY

   IF f_b = 0 THEN

      LET  enc_inf_rep                      =                     0

     SELECT "OK" 
     FROM rep_aut
     GROUP BY 1

     IF STATUS <> NOTFOUND THEN  #Se Encontro inf en FROM rep_aut
        LET enc_inf_rep                     =                      1

        START REPORT listado_1 TO ruta

        DECLARE cur_rep CURSOR FOR

         SELECT A.*
         FROM  safre_tmp:tra_det_automatico A
         WHERE  A.correlativo IN 
               (SELECT unique H.correlativo
                FROM   rep_aut H)
         AND    A.estado IN (14,15,16,17)
         ORDER BY A.estado

         FOREACH cur_rep INTO  reg_tra_det_automatico.*
            OUTPUT TO REPORT listado_1(reg_tra_det_automatico.*)
         END FOREACH 
         FINISH REPORT  listado_1
         IF  (enc_inf_rep                      =             1   )  THEN
            DISPLAY "REPORTE GENERADO EN: ",ruta CLIPPED AT 19,1 ATTRIBUTE(REVERSE) 
         END IF
     END IF

     PROMPT "PROCESO FINALIZADO....<ENTER> PARA SALIR.." FOR CHAR ENTER
     CLOSE WINDOW TRAM0611 
     EXIT PROGRAM
   END IF
END FUNCTION

FUNCTION despliega_confronta(f_folio,f_cve_desc,f_criterio_cod,
                             f_criterio_desc)
#dc----------------------------------

DEFINE f_folio          INTEGER
DEFINE f_cve_desc       CHAR(50)
DEFINE f_criterio_cod   SMALLINT 
DEFINE f_criterio_desc  CHAR(020)
DEFINE f_correlativo    INTEGER
DEFINE f_icefa_cod      CHAR(05) 

DEFINE farr_tra_det_automatico ARRAY[10000] OF RECORD 
       folio_interno  LIKE tra_det_automatico.folio_interno,
       correlativo    LIKE tra_det_automatico.correlativo,
       n_seguro       LIKE tra_det_automatico.n_seguro,
       n_seguro_ent   LIKE tra_det_automatico.n_seguro_ent,
       rfc_ent        LIKE tra_det_automatico.rfc_ent,
       nro_ctrl_icefa LIKE tra_det_automatico.nro_ctrl_icefa,
       paterno        CHAR(040) ,
       materno        CHAR(040) ,
       nombres        CHAR(040) ,
       saldo_sar_92   LIKE tra_det_automatico.saldo_sar_92 ,  
       saldo_viv_92   LIKE tra_det_automatico.saldo_viv_92 ,
       estado         LIKE tra_det_automatico.estado       ,
       estado_desc    CHAR(040)                            ,
       n_seguro_a     like afi_mae_afiliado.n_seguro       ,
       n_rfc_a        like afi_mae_afiliado.n_rfc          ,
       paterno_a      like afi_mae_afiliado.paterno        ,
       materno_a      like afi_mae_afiliado.materno        ,
       nombres_a      like afi_mae_afiliado.nombres        ,
       num_lin        SMALLINT                             
END RECORD

DEFINE freg_tra_det_automatico RECORD 
       folio_interno  LIKE tra_det_automatico.folio_interno,
       correlativo    LIKE tra_det_automatico.correlativo,
       n_seguro       LIKE tra_det_automatico.n_seguro,
       n_seguro_ent   LIKE tra_det_automatico.n_seguro_ent,
       rfc_ent        LIKE tra_det_automatico.rfc_ent,
       nro_ctrl_icefa LIKE tra_det_automatico.nro_ctrl_icefa,
       paterno        CHAR(040) ,
       materno        CHAR(040) ,
       nombres        CHAR(040) ,
       saldo_sar_92   LIKE tra_det_automatico.saldo_sar_92 ,
       saldo_viv_92   LIKE tra_det_automatico.saldo_viv_92 ,
       estado         SMALLINT  ,
       estado_desc    CHAR(040) ,
       n_seguro_a     like afi_mae_afiliado.n_seguro       ,
       n_rfc_a        like afi_mae_afiliado.n_rfc          ,
       paterno_a      like afi_mae_afiliado.paterno        ,
       materno_a      like afi_mae_afiliado.materno        ,
       nombres_a      like afi_mae_afiliado.nombres        ,
       num_lin        SMALLINT                        
END RECORD

DEFINE festado,
       arr_c                ,
       i             SMALLINT,
       tot_reg_pant  SMALLINT 


    OPEN WINDOW tram0612 AT 2,2 WITH FORM "TRAM0612" ATTRIBUTE(BORDER)
    DISPLAY" TRAM0611         CONFRONTA MANUAL TRA-AUTO-ICE-AFO IMSS                       " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY "<F1>ACEPTAR <F2>RECHAZAR <CTRL-C>SALIR                                         " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
    LET ffb = 0
WHILE TRUE
  CLEAR FORM
  CONSTRUCT BY NAME x_busca ON A.n_seguro   

        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
	    LET ffb = 1
            EXIT CONSTRUCT
    END CONSTRUCT
   IF ffb = 1 THEN
       LET ffb = 0
       CLOSE WINDOW tram0612
       EXIT WHILE
   END IF


     SELECT A.icefa_cod
        INTO f_icefa_cod
        FROM tab_icefa A
     WHERE f_cve_desc = icefa_desc  
     GROUP BY 1


     SELECT MIN(A.correlativo)
     INTO   f_correlativo
     FROM   safre_tmp:tra_det_automatico A
     WHERE  A.folio_interno = f_folio
     AND    A.estado        = vestado

LET txt =       
    'SELECT A.folio_interno,                ',
    ' A.correlativo,                  ',
    ' A.n_seguro,                     ',
    ' A.n_seguro_ent ,                ',
    ' A.rfc_ent ,                     ',
    ' A.nro_ctrl_icefa ,              ',
    ' A.nombre_ent[001,040],          ',
    ' A.nombre_ent[041,080],          ',
    ' A.nombre_ent[081,120],          ',
    ' A.saldo_sar_92       ,          ',
    ' A.saldo_viv_92       ,          ',
    ' A.estado             ,          ',
    ' Z.estado_descripcion ,          ',
    ' C.n_seguro          ,           ',
    ' C.n_rfc             ,           ',
    ' C.paterno           ,           ',
    ' C.materno           ,           ',
    ' C.nombres                       ',
    'FROM   safre_tmp:tra_det_automatico A, ',
    '       safre_tmp:tra_aut_estado     Z, ',
    '       safre_af:afi_mae_afiliado    C  ',
    ' WHERE  ',x_busca CLIPPED,
    ' AND    A.folio_interno =  ',f_folio       ,
    ' AND    A.correlativo   >= ',f_correlativo ,
    ' AND    A.estado        =  ',vestado       ,
    ' AND    A.n_seguro      = C.n_seguro      ',
    ' AND    A.estado        = Z.estado_cod    ',
    ' ORDER BY A.correlativo                   '

     LET txt = txt CLIPPED
 
     PREPARE qry1 FROM txt

     DECLARE cur_dsply_detalle CURSOR FOR qry1

     LET i = 1
     LET tot_reg_pant   = 0 
     LET freg_tra_det_automatico.num_lin       = 1

     FOREACH cur_dsply_detalle

     INTO freg_tra_det_automatico.folio_interno    ,
          freg_tra_det_automatico.correlativo      ,
          freg_tra_det_automatico.n_seguro         ,
          freg_tra_det_automatico.n_seguro_ent     ,
          freg_tra_det_automatico.rfc_ent          ,
          freg_tra_det_automatico.nro_ctrl_icefa   ,
          freg_tra_det_automatico.paterno          ,
          freg_tra_det_automatico.materno          ,
          freg_tra_det_automatico.nombres          ,
          freg_tra_det_automatico.saldo_sar_92     ,
          freg_tra_det_automatico.saldo_viv_92     ,
          freg_tra_det_automatico.estado           ,
          freg_tra_det_automatico.estado_desc      ,
          freg_tra_det_automatico.n_seguro_a       ,
          freg_tra_det_automatico.n_rfc_a          , 
          freg_tra_det_automatico.paterno_a        ,
          freg_tra_det_automatico.materno_a        ,
          freg_tra_det_automatico.nombres_a        ,
          freg_tra_det_automatico.num_lin  

          LET  farr_tra_det_automatico[i].* =
               freg_tra_det_automatico.*

          LET freg_tra_det_automatico.num_lin = 
	      freg_tra_det_automatico.num_lin + 1 

          LET i = i + 1
          LET tot_reg_pant = tot_reg_pant + 1
         
                  
          IF i >= 9999 THEN  
             EXIT FOREACH
          END IF
     
     END FOREACH
     

     CALL SET_COUNT(i-1)
    
     DISPLAY "TIPO CRITERIO:"," ",f_criterio_cod  CLIPPED," ",
                             f_criterio_desc CLIPPED AT 4,1 
     DISPLAY "ICEFA        :"," ",f_icefa_cod CLIPPED," ",f_cve_desc CLIPPED AT 5,1

     DISPLAY "REG:"  AT 21,46 ATTRIBUTE(REVERSE)   
     DISPLAY "DE: ["," ",tot_reg_pant USING "#####","]" AT 21, 59

     DISPLAY ARRAY farr_tra_det_automatico
                TO sr_ctr_folio1.*



#############

     ON KEY(F1)

       LET arr_c = ARR_CURR()
       IF (farr_tra_det_automatico[arr_c].estado = 10 OR 
           farr_tra_det_automatico[arr_c].estado = 14) THEN

        LET v_mod = farr_tra_det_automatico[arr_c].estado
         DISPLAY  "ACEPTADA PREVIAMENTE ...                     " AT 21,2
         ATTRIBUTE(REVERSE)
       ELSE 
        SELECT "OK"
        FROM   safre_tmp:tra_det_automatico  a
        WHERE  a.n_seguro       = farr_tra_det_automatico[arr_c].n_seguro 
        AND    a.n_seguro_ent   = farr_tra_det_automatico[arr_c].n_seguro_ent
        AND    a.rfc_ent        = farr_tra_det_automatico[arr_c].rfc_ent
        AND    a.cve_ced_cuenta = f_icefa_cod 
        AND    a.nro_ctrl_icefa = farr_tra_det_automatico[arr_c].nro_ctrl_icefa
        AND    a.estado   <> 0
        AND    a.correlativo <>   farr_tra_det_automatico[arr_c].correlativo
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
     
           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 14
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  farr_tra_det_automatico[arr_c].correlativo 

        LET v_mod = 14
        ELSE

        SELECT "OK"
        FROM   safre_tmp:tra_det_automatico a
        WHERE  a.n_seguro       = farr_tra_det_automatico[arr_c].n_seguro 
        AND    a.n_seguro_ent   = farr_tra_det_automatico[arr_c].n_seguro_ent
        AND    a.rfc_ent        = farr_tra_det_automatico[arr_c].rfc_ent
        AND    a.cve_ced_cuenta = f_icefa_cod 
        AND    a.nro_ctrl_icefa = farr_tra_det_automatico[arr_c].nro_ctrl_icefa
        AND    a.estado   IN (10,11,14,15,30,32,34,36,50)
        AND    a.correlativo <>   farr_tra_det_automatico[arr_c].correlativo
        GROUP BY 1

         IF STATUS <> NOTFOUND THEN

           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 15
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  farr_tra_det_automatico[arr_c].correlativo 
        LET v_mod = 15
         ELSE 
           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 14
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  farr_tra_det_automatico[arr_c].correlativo 
        LET v_mod = 14
         END IF
       END IF
      END IF

           INSERT INTO rep_aut 
           VALUES  (farr_tra_det_automatico[arr_c].correlativo)
           CALL trae_estado(v_mod)    
           RETURNING farr_tra_det_automatico[arr_c].estado,
                     farr_tra_det_automatico[arr_c].estado_desc
        DISPLAY BY NAME farr_tra_det_automatico[arr_c].estado,
                        farr_tra_det_automatico[arr_c].estado_desc

     ON KEY(F2)

       LET arr_c = ARR_CURR() 

       IF (farr_tra_det_automatico[arr_c].estado = 10) THEN
         DISPLAY  "ESTADO NO MODIFICABLE  ...                     " AT 21,2
         ATTRIBUTE(REVERSE)
         LET v_mod = farr_tra_det_automatico[arr_c].estado
       ELSE 
        SELECT "OK"
        FROM   safre_tmp:tra_det_automatico  a
        WHERE  a.n_seguro       = farr_tra_det_automatico[arr_c].n_seguro 
        AND    a.n_seguro_ent   = farr_tra_det_automatico[arr_c].n_seguro_ent
        AND    a.rfc_ent        = farr_tra_det_automatico[arr_c].rfc_ent
        AND    a.cve_ced_cuenta = f_icefa_cod 
        AND    a.nro_ctrl_icefa = farr_tra_det_automatico[arr_c].nro_ctrl_icefa
        AND    a.estado   <> 0
        AND    a.correlativo <>   farr_tra_det_automatico[arr_c].correlativo
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
     
           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 16
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  farr_tra_det_automatico[arr_c].correlativo 
   
          LET v_mod = 16
        ELSE

        SELECT "OK"
        FROM   safre_tmp:tra_det_automatico a
        WHERE  a.n_seguro       = farr_tra_det_automatico[arr_c].n_seguro 
        AND    a.n_seguro_ent   = farr_tra_det_automatico[arr_c].n_seguro_ent
        AND    a.rfc_ent        = farr_tra_det_automatico[arr_c].rfc_ent
        AND    a.cve_ced_cuenta = f_icefa_cod 
        AND    a.nro_ctrl_icefa = farr_tra_det_automatico[arr_c].nro_ctrl_icefa
        AND    a.estado   IN (12,13,16,17)
        AND    a.correlativo <>   farr_tra_det_automatico[arr_c].correlativo
        GROUP BY 1
         IF STATUS <> NOTFOUND THEN
           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 17
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  farr_tra_det_automatico[arr_c].correlativo 
          LET v_mod = 17
         ELSE 
           UPDATE safre_tmp:tra_det_automatico
           SET    safre_tmp:tra_det_automatico.estado = 16
           WHERE  safre_tmp:tra_det_automatico.correlativo = 
                  farr_tra_det_automatico[arr_c].correlativo 
          LET v_mod = 16

         END IF
       END IF
      END IF

           INSERT INTO rep_aut 
           VALUES  (farr_tra_det_automatico[arr_c].correlativo)
           CALL trae_estado(v_mod)    
           RETURNING farr_tra_det_automatico[arr_c].estado,
                     farr_tra_det_automatico[arr_c].estado_desc
           DISPLAY BY NAME farr_tra_det_automatico[arr_c].estado,
                           farr_tra_det_automatico[arr_c].estado_desc

     ON KEY(INTERRUPT)
        EXIT DISPLAY
     END DISPLAY
     END WHILE
END FUNCTION

FUNCTION init() 
#i------------- 

LET HOY                                    =                    TODAY 
LET HORA                                   =                    TIME
LET g_nom_prog                             =                   "TRAM061"

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

    SELECT codigo_afore,
           razon_social
    INTO g_glob.*
    FROM safre_af:tab_afore_local

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
            COLUMN 001,HOY USING"DD-MM-YYYY",
            COLUMN 155,g_nom_prog  CLIPPED
        PRINT
            COLUMN 001,HORA,
            COLUMN 050,"REPORTE DE VALIDACION MANUAL CRUCE SAR 92 TRA-",
                       "ICE-AFO-IMSS"
                 
        PRINT COLUMN 149,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED

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
FUNCTION trae_estado(vestado_cod)
#te-----------------------------


DEFINE vestado_cod SMALLINT
DEFINE v_estado_desc  CHAR(040)

     SELECT a.estado_descripcion
     INTO v_estado_desc
     FROM  safre_tmp:tra_aut_estado a
     WHERE a.estado_cod = vestado_cod      


RETURN vestado_cod,v_estado_desc

END FUNCTION

FUNCTION uno()
#u------------
DEFINE cad CHAR(300)

LET cad = "fglgo TRAM062"
RUN cad

END FUNCTION



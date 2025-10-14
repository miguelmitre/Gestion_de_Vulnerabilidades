##############################################################################
#Owner             => E.F.P.
#Programa TRACI500 => RECIBE Y CONFRONTA ARCHIVO PARA TRASPASO AUTOMATICO I-A
#                     ISSSTE "MASIVO".
#Fecha creacion    => 05 DE OCTUBRE DEL 2007  
#By                => MARCO ANTONIO GONZALEZ ROJAS 
#                     Estados      1  =  "Confirmada"  
#                                  2  =  "Rechazada Confronta"
#                                  3  =  "Duplicada en el Maestro"
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af
GLOBALS

DEFINE #glo #reg_1
        reg_1                 RECORD LIKE seg_modulo.*

DEFINE #glo #reg_1
        reg_tra_masivo        RECORD LIKE tra_det_masivo_issste.*

DEFINE v_correlativo LIKE tra_mae_icefa.correlativo

DEFINE reg RECORD #glo #reg
       nom_archivo           CHAR(18) 
           END RECORD

DEFINE #glo #char
       tmp_pla_trasp2                     ,
       archivo_tipo_trasp    CHAR(200)    ,
       txt_insert            CHAR(100)    ,
       txt_1                 CHAR(500)    ,
       enter                 CHAR(001)    ,
       usuario               CHAR(008)    ,
       c10_fecha             CHAR(010)    ,
       c16_saldo_pares                    ,
       c16_saldo_fovissste                ,
       c16_saldo_sar                      ,
       c16_saldo_viv         CHAR(018)    ,
       c_validacion          CHAR(006)    ,
       g_rfc10               CHAR(010)


DEFINE #date
        HOY                  DATE

DEFINE #glo #smallint
        s_codigo_afore                    ,
        v_folio_interno                   ,
        cuantos               SMALLINT      

DEFINE #integer
        cont                              ,
        cont_acep                         ,
        cont_rech_conf                    ,
        cont_dup_mae_sust                 ,
        cont_dup_mae_opera          INTEGER         

 DEFINE reg_det_trasp RECORD #glo #reg_det_trasp
        bsar_nseguro          CHAR(011)   ,
        bsar_curp             CHAR(018)   ,
        bsar_rfc              CHAR(013)   ,
        bsar_pat                          ,
        bsar_mat                          ,
        bsar_nom              CHAR(040)   ,
        bsar_fecnac           DATE        ,
        bsar_sexo             CHAR(001)   ,
        bsar_ent_nac          CHAR(002)   ,
        bsar_afore            CHAR(003)   ,
        bist_identproc        INTEGER     ,
        bist_curp             CHAR(018)   ,
        bist_nseguro          CHAR(011)   ,
        bist_rfc              CHAR(013)   ,
        bist_numctrlint       CHAR(030)   ,
        bist_pat                          ,
        bist_mat                          ,
        bist_nom              CHAR(040)   ,
        bist_fecnac           DATE        ,
        bist_sdopares                     ,
        bist_sdofoviaivs    DECIMAL(16,6) ,
        bist_sdosar                       ,
        bist_sdofovipesos   DECIMAL(15,2) ,
        bist_nomcompl         CHAR(120)   ,
        bist_icefa            CHAR(003)   ,
        folio                 INTEGER     ,
        estado                SMALLINT    ,
        correlativo           INTEGER     ,
        cad_valida            CHAR(005)   ,
        fecha_genera          DATE        ,
        usuario               CHAR(010)   
                      END RECORD       
      

END GLOBALS

MAIN
    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

   CALL STARTLOG("TRACI500.log")
   CALL init()

   LET tmp_pla_trasp2 =  'xx'
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_pla_trasp2
   WHENEVER ERROR STOP

              CREATE TEMP TABLE xx
                                         (n_registros          CHAR(600))

   OPEN WINDOW traci500 AT 4,4 WITH FORM "TRACI5001" ATTRIBUTE(BORDER)

   DISPLAY " TRACI500  CARGA ARCH ENVIADO POR PROCESAR CRUCE MASIVO ISSSTE                 " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg.* WITHOUT DEFAULTS

       AFTER FIELD nom_archivo
	   IF reg.nom_archivo IS NULL THEN
	       ERROR "Campo NO puede ser NULO"
	       NEXT FIELD nom_archivo
	   END IF

    ON KEY (ESC)

       LET archivo_tipo_trasp = reg_1.ruta_rescate CLIPPED,"/",
                                        reg.nom_archivo CLIPPED
      
       WHENEVER ERROR CONTINUE

               DISPLAY "PROCESANDO INFORMACION " AT 19,2

            -- CALL limpieza(reg_1.ruta_rescate) #l
               LET txt_insert = " INSERT INTO ",tmp_pla_trasp2 CLIPPED
 
               LOAD FROM archivo_tipo_trasp 
               DELIMITER "+" 
               txt_insert 

               LET txt_1 = " SELECT count(*) ",
                           " FROM   ",tmp_pla_trasp2 CLIPPED
             
               PREPARE pre_21 FROM txt_1
               DECLARE cur_21 CURSOR FOR pre_21
               FOREACH cur_21 INTO cuantos
               END FOREACH     

               IF cuantos = 0 THEN
                   DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                   AT 19,2 ATTRIBUTE(REVERSE)
                   SLEEP 3
                   NEXT FIELD nom_archivo
               ELSE
                   EXIT INPUT
               END IF
           WHENEVER ERROR STOP

       ON KEY (INTERRUPT)
           PROMPT "PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
   END INPUT
   
   CALL clasifica() #c

   PROMPT " PROCESO TERMINADO...<ENTER> PARA FINALIZAR"  FOR CHAR enter

END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT MAX(folio_interno) + 1
    INTO   v_folio_interno
    FROM   safre_af:tra_folio_interno
  
    INSERT INTO safre_af:tra_folio_interno VALUES(v_folio_interno,HOY)

    SELECT codigo_afore   ,
           USER 
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT *
    INTO   reg_1.*
    FROM   seg_modulo
    WHERE modulo_cod = "tra"

END FUNCTION

FUNCTION clasifica()
#c------------------
    CALL traspaso_cuentas() #tc
    CALL corre_estadisticas()#ce
    CALL inser_actua_ctas() #iac

    DISPLAY "FOLIO                                              : ",v_folio_interno  AT 10,2
    DISPLAY "TOTAL  DE REGS                                     : ",cuantos    AT 11,2
    DISPLAY "TOTAL  DE REGS ACEPTADOS                           : ",cont_acep  AT 14,2
    DISPLAY "TOTAL  DE REGS RECH.  CONFRONTA                    : ",cont_rech_conf  AT 15,2
    DISPLAY "TOTAL  DE REGS DUPLI. MAESTRO SUSTITUIDO           : ",cont_dup_mae_sust  AT 16,2
    DISPLAY "TOTAL  DE REGS DUPLI. MAESTRO EN PROCESO OPERATIVO : ",cont_dup_mae_opera  AT 17,2

END FUNCTION

FUNCTION traspaso_cuentas()
#tc------------------------
    DEFINE 
        carga_reg            CHAR(600) ,
        c2_carga_reg         CHAR(002)


    LET txt_1 = " SELECT  *  ",
                " FROM   ",tmp_pla_trasp2 CLIPPED
   
    PREPARE pre_3 FROM txt_1
    DECLARE cur_3 CURSOR FOR pre_3

    LET cont = 0
    FOREACH cur_3 INTO carga_reg
           #---layout de Cruce de BDSARISSSTE - BDNSAR---#

            LET cont = cont + 1
            DISPLAY "REGISTROS CARGADOS              : ",cont AT 12,2 

            LET reg_det_trasp.bsar_nseguro       = carga_reg[001,011]
            LET reg_det_trasp.bsar_curp          = carga_reg[012,029]
            LET reg_det_trasp.bsar_rfc           = carga_reg[030,042]
            LET reg_det_trasp.bsar_pat           = carga_reg[043,082]
            LET reg_det_trasp.bsar_mat           = carga_reg[083,122]
            LET reg_det_trasp.bsar_nom           = carga_reg[123,162]
           
            LET c10_fecha                        = carga_reg[168,169],"/",
                                                   carga_reg[171,172],"/",
                                                   carga_reg[163,166]

            LET reg_det_trasp.bsar_fecnac        = c10_fecha         
            LET reg_det_trasp.bsar_sexo          = carga_reg[173]
            LET reg_det_trasp.bsar_ent_nac       = carga_reg[174,175]
            LET reg_det_trasp.bsar_afore         = carga_reg[176,178]

            LET reg_det_trasp.bist_identproc     = carga_reg[179,186]
            LET reg_det_trasp.bist_curp          = carga_reg[187,204]
            LET reg_det_trasp.bist_nseguro       = carga_reg[205,215]
            LET reg_det_trasp.bist_rfc           = carga_reg[216,228]
            LET reg_det_trasp.bist_numctrlint    = carga_reg[229,258]
            LET reg_det_trasp.bist_pat           = carga_reg[259,298]
            LET reg_det_trasp.bist_mat           = carga_reg[299,338]
            LET reg_det_trasp.bist_nom           = carga_reg[339,378]

            LET c10_fecha                        = carga_reg[384,385],"/",
                                                   carga_reg[387,388],"/",
                                                   carga_reg[379,382]

            LET reg_det_trasp.bist_fecnac        = c10_fecha              

            LET c16_saldo_pares                  = carga_reg[389,399],".",
					           carga_reg[400,405]

            LET reg_det_trasp.bist_sdopares      = c16_saldo_pares        

----
            LET c16_saldo_fovissste              = carga_reg[406,416],".",
					           carga_reg[417,422]

            LET reg_det_trasp.bist_sdofoviaivs   = c16_saldo_fovissste    

----
            LET c16_saldo_sar                    = carga_reg[423,437],".",
					           carga_reg[438,439]

            LET reg_det_trasp.bist_sdosar        = c16_saldo_sar          

----
            LET c16_saldo_viv                    = carga_reg[440,454],".",
					           carga_reg[455,456]

            LET reg_det_trasp.bist_sdofovipesos  = c16_saldo_viv          
            LET reg_det_trasp.bist_nomcompl      = carga_reg[457,576]
            LET reg_det_trasp.bist_icefa         = carga_reg[577,579]

------ Variables que no trae el Archivo -----
            LET reg_det_trasp.folio              = v_folio_interno
            LET reg_det_trasp.estado             = 0
            LET reg_det_trasp.correlativo        = 0   #serial
            LET reg_det_trasp.cad_valida         = "000000"
            LET reg_det_trasp.fecha_genera       = TODAY   
            LET reg_det_trasp.usuario            = usuario

            INSERT INTO tra_det_masivo_issste VALUES(
                                                      reg_det_trasp.*     
                                                     )

    END FOREACH

END FUNCTION
FUNCTION inser_actua_ctas()
#tc------------------------
DEFINE l_nseguro         LIKE safre_af:afi_mae_afiliado.n_seguro
DEFINE l_n_unico         LIKE safre_af:afi_mae_afiliado.n_unico
DEFINE l_paterno         LIKE safre_af:afi_mae_afiliado.paterno
DEFINE l_materno         LIKE safre_af:afi_mae_afiliado.materno
DEFINE l_nombre          LIKE safre_af:afi_mae_afiliado.nombres
DEFINE l_n_folio         LIKE safre_af:afi_mae_afiliado.n_folio
DEFINE l_tipo_solicitud  LIKE safre_af:afi_mae_afiliado.tipo_solicitud
DEFINE l_status          LIKE safre_af:tra_mae_icefa_issste.status
DEFINE l_n_rfc           LIKE safre_af:afi_mae_afiliado.n_rfc
DEFINE l_correlativo     LIKE safre_af:tra_mae_icefa_issste.correlativo
DEFINE l_ctosmae         SMALLINT
DEFINE l_patnomcompl     ,
       l_matnomcompl     ,
       l_nomnomcompl     CHAR(40)
  
   CALL initialize()   
 
   DECLARE cur_5  CURSOR FOR

      SELECT A.*
         FROM tra_det_masivo_issste A
      WHERE A.folio       =      v_folio_interno

   LET cont_acep          =      0
   LET cont_rech_conf     =      0
   LET cont_dup_mae_sust  =      0
   LET cont_dup_mae_opera =      0
   LET l_ctosmae          =      0
   LET l_correlativo      =      NULL
   LET l_status           =      NULL

   FOREACH cur_5  INTO  reg_tra_masivo.*
      
      LET g_rfc10                    = reg_tra_masivo.bist_rfc[1,10]
      LET c_validacion               =     "000000"  

           SELECT a.n_seguro       ,
                  a.n_unico        ,
                  a.n_rfc          ,
                  a.paterno        ,
                  a.materno        ,
                  a.nombres        ,
                  a.n_folio        ,
                  a.tipo_solicitud 
           INTO   l_nseguro        ,
                  l_n_unico        , 
                  l_n_rfc          ,
                  l_paterno        ,
                  l_materno        ,
                  l_nombre         ,
                  l_n_folio        ,
                  l_tipo_solicitud 
           FROM   afi_mae_afiliado a
           WHERE  a.n_seguro       =  reg_tra_masivo.bsar_nseguro

      IF   reg_tra_masivo.estado    =    0     AND ( l_nseguro IS NOT NULL 
           OR  l_nseguro <> ""    OR   l_nseguro <> " " ) THEN 

           LET c_validacion[1] = "1"

           IF  reg_tra_masivo.bist_curp      =       l_n_unico    THEN
              LET   c_validacion[3]          =       "1"
           END IF

           IF  g_rfc10                       =       l_n_rfc[1,10]    THEN
              LET   c_validacion[2]          =       "1"
           END IF
          
           IF  reg_tra_masivo.bist_pat       =       l_paterno    THEN
              LET   c_validacion[4]          =       "1"
           END IF

           IF  reg_tra_masivo.bist_mat       =       l_materno    THEN
              LET   c_validacion[5]          =       "1"
           END IF
 
           IF  reg_tra_masivo.bist_nom       =       l_nombre     THEN
              LET   c_validacion[6]          =       "1"
           END IF

           IF  c_validacion  =   "111111"  THEN
         
              #VERIFICA SI EL REG YA EXISTE EN LA TABLA MAESTRA tra_mae_icefa
              #                                                  issste

              LET l_ctosmae = 0

              SELECT a.status,a.correlativo,count(*) 
              INTO   l_status,l_correlativo,l_ctosmae
                 FROM safre_af:tra_mae_icefa_issste a
              WHERE a.nss            = reg_tra_masivo.bist_nseguro
                AND a.rfc            = reg_tra_masivo.bist_rfc
                AND a.icefa_cod      = reg_tra_masivo.bist_icefa
                AND a.nro_int_cta    = reg_tra_masivo.bist_numctrlint
              GROUP BY 1,2

              IF l_ctosmae IS NULL THEN  
                 LET l_ctosmae = 0
              END IF

              IF  ( l_ctosmae =  1 OR  l_ctosmae > 1) THEN # REG YA EXISTENTE en tra_mae_icefa_issste

                 IF l_status = 41 OR 
                    l_status =  7 OR
                    l_status =  8 OR
                    l_status =  2 THEN  #YA EXISTE EN PROCESO OPERATIVO

                    LET cont_dup_mae_opera         =   cont_dup_mae_opera  + 1
                    LET reg_tra_masivo.estado      =   4

                    UPDATE tra_det_masivo_issste
                    SET    tra_det_masivo_issste.usuario     = reg_tra_masivo.usuario ,
                           tra_det_masivo_issste.estado      = reg_tra_masivo.estado ,#"Duplicada en el Maestro en Proceso Operativo"
                           tra_det_masivo_issste.cad_valida  = c_validacion
                    WHERE  tra_det_masivo_issste.correlativo = reg_tra_masivo.correlativo
                      AND  tra_det_masivo_issste.folio       = reg_tra_masivo.folio
                 ELSE
                    LET cont_dup_mae_sust          =   cont_dup_mae_sust   + 1
                    LET reg_tra_masivo.estado      =   3

                    UPDATE tra_det_masivo_issste
                    SET    tra_det_masivo_issste.usuario     = reg_tra_masivo.usuario ,
                           tra_det_masivo_issste.estado      = reg_tra_masivo.estado ,#"Duplicada en el Maestro Sustituido"
                           tra_det_masivo_issste.cad_valida  = c_validacion
                    WHERE  tra_det_masivo_issste.correlativo = reg_tra_masivo.correlativo
                      AND  tra_det_masivo_issste.folio       = reg_tra_masivo.folio
                    DELETE 
                    FROM safre_af:tra_mae_icefa_issste
                    WHERE nss            = reg_tra_masivo.bist_nseguro
                    AND   rfc            = reg_tra_masivo.bist_rfc
                    AND   icefa_cod      = reg_tra_masivo.bist_icefa
                    AND   nro_int_cta    = reg_tra_masivo.bist_numctrlint
                    AND   correlativo    = l_correlativo

                    #---Nombre Completo Segun Icefa---

                    LET l_patnomcompl = reg_tra_masivo.bist_nomcompl[1,40]
                    LET l_matnomcompl = reg_tra_masivo.bist_nomcompl[41,80]
                    LET l_nomnomcompl = reg_tra_masivo.bist_nomcompl[81,120]

                    #---Fin de la Rutina---

                    INSERT INTO tra_mae_icefa_issste
                    VALUES(l_n_folio                      ,#afi_mae_afi n_folio
                           l_tipo_solicitud               ,#afi_mae_afitip_solic
                           l_nseguro                      ,#afi_mae_afi n_seguro
                           reg_tra_masivo.bist_nseguro    ,#bist nss
                           reg_tra_masivo.bist_rfc        ,#bist rfc
                           l_patnomcompl                  ,#bist_nomco paterno
                           l_matnomcompl                  ,#bist_nomco materno
                           l_nomnomcompl                  ,#bist_nomco nombre
                           reg_tra_masivo.correlativo     ,#bist n_folio_tra
                           reg_tra_masivo.bist_icefa      ,#bist icefa_cod    
                           reg_tra_masivo.bist_numctrlint ,#bist nro_int_cta
                           TODAY                          ,#fecha_solic_tra
                           " "                            ,#fecha_comp_icefa
                           0                              ,#saldo_sar_92
                           0                              ,#saldo_viv_92
                           61                             ,#origen_traspaso ??
                           TODAY                          ,#fecha_captura
                           TODAY                          ,#fecha_proceso
                           " "                            ,#lote_genera
                           " "                            ,#fecha_genera
                           1                              ,#status confirmada
                           "3"                            ,#fuente (masivo)
                           0                              ,#correlativo (serial)
                           reg_tra_masivo.usuario         ,#usuario
                           0                              ,#n_envios
                           " "                            ,#diagnostico
                           2                               #cve_sector ??
                           )
  
                 END IF
              ELSE
                 LET cont_acep                            =   cont_acep      + 1

                 #---Nombre Completo Segun Icefa---

                    LET l_patnomcompl = reg_tra_masivo.bist_nomcompl[1,40]
                    LET l_matnomcompl = reg_tra_masivo.bist_nomcompl[41,80]
                    LET l_nomnomcompl = reg_tra_masivo.bist_nomcompl[81,120]

                 #---Fin de la Rutina---

                 INSERT INTO tra_mae_icefa_issste
                 VALUES(l_n_folio                      ,#afi_mae_afi n_folio
                        l_tipo_solicitud               ,#afi_mae_afi tip_solic
                        l_nseguro                      ,#afi_mae_afi n_seguro
                        reg_tra_masivo.bist_nseguro    ,#bist nss
                        reg_tra_masivo.bist_rfc        ,#bist rfc
                        l_patnomcompl                  ,#bist_nomco paterno
                        l_matnomcompl                  ,#bist_nomco materno
                        l_nomnomcompl                  ,#bist_nomco nombre
                        reg_tra_masivo.correlativo     ,#bist n_folio_tra
                        reg_tra_masivo.bist_icefa      ,#bist icefa_cod    
                        reg_tra_masivo.bist_numctrlint ,#bist nro_int_cta
                        TODAY                          ,#fecha_solic_tra
                        " "                            ,#fecha_comp_icefa
                        0                              ,#saldo_sar_92
                        0                              ,#saldo_viv_92
                        61                             ,#origen_traspaso ??
                        TODAY                          ,#fecha_captura
                        TODAY                          ,#fecha_proceso
                        " "                            ,#lote_genera
                        " "                            ,#fecha_genera
                        1                              ,#status confirmada
                        "3"                            ,#fuente (masivo)
                        0                              ,#correlativo (serial)
                        reg_tra_masivo.usuario         ,#usuario
                        0                              ,#n_envios
                        " "                            ,#diagnostico
                        2                               #cve_sector ??
                        )

                 LET reg_tra_masivo.estado                =    1

                 UPDATE tra_det_masivo_issste
                 SET    tra_det_masivo_issste.usuario = reg_tra_masivo.usuario ,
                        tra_det_masivo_issste.estado  = reg_tra_masivo.estado,# CONF ACEPTADO       
                        tra_det_masivo_issste.cad_valida   = c_validacion
                WHERE   tra_det_masivo_issste.correlativo = reg_tra_masivo.correlativo
                  AND   tra_det_masivo_issste.folio       = reg_tra_masivo.folio
            
 
              END IF
           ELSE #  c_validacion <>  "111111"   
              LET cont_rech_conf                       =    cont_rech_conf + 1
              LET reg_tra_masivo.estado                =    2

              UPDATE  tra_det_masivo_issste
              SET     tra_det_masivo_issste.cad_valida  = c_validacion            ,
                      tra_det_masivo_issste.usuario     = reg_tra_masivo.usuario  ,
                      tra_det_masivo_issste.estado      = reg_tra_masivo.estado #"Rech Confronta"
               WHERE   tra_det_masivo_issste.correlativo = reg_tra_masivo.correlativo
           END IF
           ELSE

               LET cont_rech_conf                     =    cont_rech_conf + 1
               LET reg_tra_masivo.estado              =    2
               LET c_validacion                       =    "000000"  
     
               UPDATE  tra_det_masivo_issste
               SET     tra_det_masivo_issste.cad_valida  = c_validacion            ,
                       tra_det_masivo_issste.usuario     = reg_tra_masivo.usuario  ,
                       tra_det_masivo_issste.estado      = reg_tra_masivo.estado #"Rechazada Confronta"
               WHERE   tra_det_masivo_issste.correlativo = reg_tra_masivo.correlativo
                 AND   tra_det_masivo_issste.folio       = reg_tra_masivo.folio
              
      END IF

   END FOREACH

   
END FUNCTION

FUNCTION initialize()
#i-------------------------

   INITIALIZE g_rfc10          TO NULL
   INITIALIZE reg_tra_masivo.* TO NULL

   LET cont_acep          =      0
   LET cont_rech_conf     =      0
   LET cont_dup_mae_sust  =      0
   LET cont_dup_mae_opera =      0

END FUNCTION

FUNCTION corre_estadisticas()
#ce-------------------------

   UPDATE STATISTICS FOR TABLE tra_det_masivo_issste

END FUNCTION

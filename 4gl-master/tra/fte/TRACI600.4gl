##############################################################################
#Owner             => E.F.P.
#Programa TRACI600 => GENERA SOLICITUDES RECHAZADAS POR CONFRONTA MASIVO
#                     TRASPASO AUTOMATICO ICE-AFO-AFO ISSSTE ,PONIENDOLE
#                     STATUS 20 DE CAPTURADA.
#Fecha creacion    => 16 DE OCTUBRE DEL 2007  
#By                => MARCO ANTONIO GONZALEZ ROJAS 
#                     Estados      1  =  "Confirmada"  
#                                  2  =  "Rechazada Confronta"
#                                  3  =  "Duplicada en el Maestro"
#                                  4  =  "Ya Existe En Proceso Operativo"
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
       folio                 INTEGER  
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
       c_validacion          CHAR(006)    


DEFINE #date
        HOY                  DATE

DEFINE #glo #smallint
        s_codigo_afore                    ,
        v_folio_interno                   ,
        cuantos               SMALLINT      

DEFINE #integer
        cont                              ,
        cont_acep                         ,
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

   CALL STARTLOG("TRACI600.log")
   CALL init()


   OPEN WINDOW traci600 AT 4,4 WITH FORM "TRACI6001" ATTRIBUTE(BORDER)

   DISPLAY " TRACI600  GENERA SOLIC. RECHAZADAS POR CRUCE MASIVO ISSSTE                    " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg.* WITHOUT DEFAULTS
       
       AFTER FIELD folio          
	   IF reg.folio  IS NULL THEN
	       ERROR "Campo NO puede ser NULO"
	       NEXT FIELD folio        
	   END IF

    ON KEY (ESC)

       WHENEVER ERROR CONTINUE

               DISPLAY "PROCESANDO INFORMACION " AT 19,2

               SELECT COUNT(*)
               INTO   cuantos
               FROM   safre_af:tra_det_masivo_issste
               WHERE  estado    =  2
               AND    folio     =  reg.folio     

               IF cuantos = 0 THEN
                   DISPLAY  " NO EXISTEN SOLICITUDES A GENERAR CON EL FOLIO:","   ",reg.folio AT 19,2 ATTRIBUTE(REVERSE)
                   SLEEP 3
                   NEXT FIELD folio        
               ELSE
                   EXIT INPUT
               END IF
           WHENEVER ERROR STOP

       ON KEY (INTERRUPT)
           PROMPT "PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
   END INPUT
   
   CALL proceso_principal() #pp

   PROMPT " PROCESO TERMINADO...<ENTER> PARA FINALIZAR"  FOR CHAR enter

END MAIN

FUNCTION init()
#i-------------

    LET HOY = TODAY

    SELECT codigo_afore   ,
           USER 
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

END FUNCTION

FUNCTION proceso_principal()
#c------------------

    CALL inser_actua_ctas() #iac

    DISPLAY "FOLIO                                              : ",reg.folio  AT 10,2
    DISPLAY "TOTAL  DE REGS                                     : ",cuantos    AT 11,2
    DISPLAY "TOTAL  DE REGS ACEPTADOS                           : ",cont_acep  AT 14,2
    DISPLAY "TOTAL  DE REGS DUPLI. MAESTRO SUSTITUIDO           : ",cont_dup_mae_sust  AT 15,2
    DISPLAY "TOTAL  DE REGS DUPLI. MAESTRO EN PROCESO OPERATIVO : ",cont_dup_mae_opera  AT 16,2

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
      WHERE A.folio       =      reg.folio           
        AND A.estado      =      2

   LET cont_acep          =      0
   LET cont_dup_mae_sust  =      0
   LET cont_dup_mae_opera =      0
   LET l_ctosmae          =      0
   LET l_correlativo      =      NULL
   LET l_status           =      NULL

   FOREACH cur_5  INTO  reg_tra_masivo.*
      
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

      IF   reg_tra_masivo.estado    =    2     AND ( l_nseguro IS NOT NULL 
           OR  l_nseguro <> ""    OR   l_nseguro <> " " ) THEN 

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
                           tra_det_masivo_issste.estado      = reg_tra_masivo.estado #"Duplicada en el Maestro en Proceso Operativo"
                    WHERE  tra_det_masivo_issste.correlativo = reg_tra_masivo.correlativo
                      AND  tra_det_masivo_issste.folio       = reg_tra_masivo.folio
                 ELSE  #STATUS MODIFICABLE  <> 41,7,8,2
                    LET cont_dup_mae_sust          =   cont_dup_mae_sust   + 1
                    LET reg_tra_masivo.estado      =   3

                    UPDATE tra_det_masivo_issste
                    SET    tra_det_masivo_issste.usuario     = reg_tra_masivo.usuario ,
                           tra_det_masivo_issste.estado      = reg_tra_masivo.estado  #"Duplicada en el Maestro Sustituido"
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
                           20                             ,#status Capturada  
                           "3"                            ,#fuente (masivo)
                           0                              ,#correlativo (serial)
                           reg_tra_masivo.usuario         ,#usuario
                           0                              ,#n_envios
                           " "                            ,#diagnostico
                           2                               #cve_sector ??
                           )
  
                 END IF
              ELSE  ## REG NO EXISTENTE EN tra_mae_icefa_issste
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
                        20                             ,#status Capturada  
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
                        tra_det_masivo_issste.estado  = reg_tra_masivo.estado# CONF ACEPTADO       
                WHERE   tra_det_masivo_issste.correlativo = reg_tra_masivo.correlativo
                  AND   tra_det_masivo_issste.folio       = reg_tra_masivo.folio
            
 
              END IF
      END IF
   END FOREACH

   
END FUNCTION

FUNCTION initialize()
#i-------------------------

   INITIALIZE reg_tra_masivo.* TO NULL

   LET cont_acep          =      0
   LET cont_dup_mae_sust  =      0
   LET cont_dup_mae_opera =      0

END FUNCTION

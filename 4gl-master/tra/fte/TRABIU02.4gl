##############################################################################
#Owner             => E.F.P.
#Programa TRABIU02 => PROVISIONA TRASPASOS UNI-SAR-ISSSTE   
#Fecha creacion    => 19 DE MARZO DE 1998
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 08 DE OCTUBRE DEL 2008
#Ultima   Mod      => 03 DE MARZO   DEL 2010
#Ultima   Mod      => 22 ABRIL      DEL 2010
#Objetivo de la Mod=> Entrada en Vigor de lo de FOVIIISTE para la Subcuenta
#                     14 Anteriormente solo se Prov. y liquidaban los Pesos
#                     pero con lo de la Entrada en vigor se tienen que Comprar
#                     y Liquidar Acciones y estas se invertirían en la Siefore
#                     12 y ya no en la 0 .
#Objetivo de la Mod --Fecha de Emision: 01/12/2009
#                   --Version : 1:0
#                   --Sustituye a : Ninguna
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af
GLOBALS

DEFINE iden          CHAR(002)
DEFINE pre           CHAR(003)
DEFINE v_id          CHAR(006)
DEFINE sub_inv               ,
       indicador_inv SMALLINT

    DEFINE reg RECORD #glo #reg
        total_saldo_sar_92                  ,
        total_saldo_viv_92                  ,
        total_int_sar_92                    ,
        total_int_viv_92      DECIMAL(15,2)
    END RECORD

    DEFINE reg_4 RECORD #glo #reg_4
        aceptada_icefa                 ,
        devuelta                       ,
        no_atendida                    ,
        rechazada                      ,
        complementarios                ,
        provisionada          SMALLINT 
    END RECORD

    DEFINE #glo #cza #det_sal #det_int #sum
        cza                   RECORD LIKE tra_cza_trasp_issste.*     ,
        det_sal               RECORD LIKE tra_det_trasp_sal_issste.* ,
        det_int               RECORD LIKE tra_det_trasp_int_issste.* 
     #   sum        RECORD LIKE sum_trasp.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        c6_folio_sua          CHAR(06) ,
        enter                 CHAR(01) ,
        aux_pausa             CHAR(01) ,
        usuario               CHAR(08)

    DEFINE #glo #smallint
        s_nro_icefa           SMALLINT

    DEFINE #glo #integer
        num_reg_procesados          ,
        folio                       ,
        total_trasp_sal             ,
        total_trasp_int             ,
        total_trasp           INTEGER

    DEFINE #glo #decimal
           d15_total_importe     DECIMAL(15,2)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS 
        PROMPT LINE LAST

    CALL STARTLOG("TRABIU02.log")

    CALL init() #i

    OPEN WINDOW trabu0021 AT 4,4 WITH FORM "TRABU0021" ATTRIBUTE(BORDER)
    DISPLAY "                            < CTRL-C> Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY" TRABIU02          PROVISIONA TRASPASOS UNI-SAR-ISSSTE                         " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET folio = NULL
    INPUT BY NAME folio WITHOUT DEFAULTS
        AFTER FIELD folio
            IF folio IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD folio
            ELSE

                SELECT "OK"
                FROM   tra_cza_trasp_issste A
                WHERE  A.folio  = folio
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                   ERROR "FOLIO NO EXISTE"
                   NEXT FIELD folio
                ELSE

                    SELECT A.ident_operacion
                    INTO   iden
                    FROM tra_cza_trasp_issste A
                    WHERE  A.folio = folio
                    IF iden = "09" THEN
                       LET pre = "TI-"
                    END IF
                    IF iden = "12" THEN
                       LET pre = "CI-"
                    END IF

                    SELECT "OK"
                    FROM   dis_provision A
                    WHERE  A.folio  = folio 
                    GROUP BY 1

                    IF STATUS <> NOTFOUND THEN
                        ERROR "FOLIO YA FUE PROVISIONADO"
                        NEXT FIELD folio
                    END IF
                END IF

            END IF

            ON KEY (INTERRUPT)
               DISPLAY "                                             " AT 19,2
               DISPLAY "PROCESO CANCELADO" AT 19,2 SLEEP 2
               EXIT PROGRAM
                
        END INPUT

        WHILE TRUE

         PROMPT "DESEA GENERAR EL PROCESO S/N ? " FOR CHAR aux_pausa

            IF (aux_pausa MATCHES "[SsNn]") THEN
                DISPLAY "                               " AT 18,2

                UPDATE tra_cza_trasp_issste
                SET    tra_cza_trasp_issste.estado    =    7
                WHERE  tra_cza_trasp_issste.folio     =    folio

                IF (aux_pausa MATCHES "[Ss]") THEN

                   CALL proceso_principal() #pp
    
                   EXIT WHILE
                ELSE
                   DISPLAY "PROCESO CANCELADO" SLEEP 2
                   EXIT PROGRAM
                END IF

           ELSE #Tecleo <> S(i) o N(o)

                DISPLAY "Solo debe presionar S(i) o N(o)" AT 18,2

           END IF #Fin de aux_pausa MATCHES "[SsNn]"

        END WHILE

        PROMPT "PROCESO FINALIZO NORMALMENTE.... PRESIONE < ENTER > PARA SALIR"         FOR CHAR aux_pausa

    CLOSE WINDOW trabu0021

END MAIN

FUNCTION init()
#i-------------

    LET HOY = TODAY

    SELECT estado
    INTO   reg_4.aceptada_icefa
    FROM   tra_status
    WHERE  des_estado = "ACEPTADA_ICEFA"

    SELECT estado
    INTO   reg_4.complementarios
    FROM   tra_status
    WHERE  des_estado = "COMPLEMENTARIOS"

    SELECT estado
    INTO   reg_4.provisionada
    FROM   tra_status
    WHERE  des_estado = "PROVISIONADA"

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    DEFINE #loc
        c10_coduni_n1         LIKE afi_mae_afiliado.coduni_n1

    SELECT COUNT(*)
    INTO   total_trasp_sal
    FROM   tra_det_trasp_sal_issste A
    WHERE  A.folio  = folio
    AND    A.estado IN(reg_4.aceptada_icefa, reg_4.complementarios)

    SELECT COUNT(*)
    INTO   total_trasp_int
    FROM   tra_det_trasp_int_issste A
    WHERE  A.folio  = folio
    AND    A.estado IN(reg_4.aceptada_icefa, reg_4.complementarios)

    LET total_trasp = total_trasp_sal + total_trasp_int
    DISPLAY "REGISTROS A PROCESAR ",total_trasp AT 15,10

    DECLARE cur_2 CURSOR FOR 

    SELECT tra_cza_trasp_issste.*     ,
           tra_det_trasp_sal_issste.* ,
           USER
    FROM   tra_cza_trasp_issste ,tra_det_trasp_sal_issste
    WHERE  tra_cza_trasp_issste.folio = folio
    AND    tra_cza_trasp_issste.folio = tra_det_trasp_sal_issste.folio

    LET num_reg_procesados     = 0
    LET reg.total_saldo_sar_92 = 0
    LET reg.total_saldo_viv_92 = 0

    FOREACH cur_2 INTO cza.*,det_sal.*,usuario

        LET num_reg_procesados = num_reg_procesados + 1
        DISPLAY "REGISTROS PROCESADOS ",num_reg_procesados AT 17,10

        SELECT afi_mae_afiliado.coduni_n1 
        INTO   c10_coduni_n1
        FROM   afi_mae_afiliado
        WHERE  afi_mae_afiliado.n_seguro = det_sal.n_seguro

        IF STATUS <> NOTFOUND THEN

           LET reg.total_saldo_sar_92 = reg.total_saldo_sar_92 +
                                      det_sal.saldo_sar_92


          LET indicador_inv = 0
          LET sub_inv       = 0

          SELECT b.indicador_inv
          INTO   indicador_inv
          FROM  tra_tab_tiptra b
          WHERE b.origen_traspaso = det_sal.orig_tipo_traspaso 

          CASE  indicador_inv

            WHEN   1
               LET sub_inv = 13 
               EXIT CASE
            OTHERWISE 
              LET sub_inv = 19
              EXIT CASE

          END CASE
      
     CALL dispersa_sal(c10_coduni_n1               ,
                              det_sal.*            ,
                              det_sal.saldo_sar_92 ,
                              sub_inv              ,
                              1
                             )#ds



     LET reg.total_saldo_viv_92 = reg.total_saldo_viv_92 +
                                  det_sal.saldo_viv_92

     CALL dispersa_sal(c10_coduni_n1        ,
                              det_sal.*            ,
                              det_sal.saldo_viv_92 ,
                              14                    ,
                              1
                             )#ds


       UPDATE tra_mae_icefa_issste 
       SET   tra_mae_icefa_issste.saldo_sar_92 = tra_mae_icefa_issste.saldo_sar_92 + 
               det_sal.saldo_sar_92,
              tra_mae_icefa_issste.saldo_viv_92 = tra_mae_icefa_issste.saldo_viv_92 + 
               det_sal.saldo_viv_92
       WHERE  tra_mae_icefa_issste.n_seguro     = det_sal.n_seguro       #AFI
       AND    tra_mae_icefa_issste.nss          = det_sal.n_seguro_ent   #ICE
       AND    tra_mae_icefa_issste.rfc          = det_sal.rfc_ent        #ICE
       AND    tra_mae_icefa_issste.icefa_cod    = det_sal.cve_ced_cuenta #ICE
       AND    tra_mae_icefa_issste.id_procesar  = det_sal.id_procesar    #ICE
       AND    tra_mae_icefa_issste.status IN (7,8,41,17) 


       IF det_sal.tipo_icefa = "N" THEN
    
                CALL actualiza_maeicefa(det_sal.n_seguro         ,  #AFI
                                        det_sal.n_seguro_ent     ,  #ICE
                                        det_sal.rfc_ent          ,  #ICE
                                        det_sal.cve_ced_cuenta   ,  #ICE
                                        det_sal.nro_ctrl_icefa   ,  #ICE
                                        det_sal.id_procesar      ,  #ICE
                                        det_sal.ident_lote_solic ,   
                                        reg_4.aceptada_icefa     ,
                                        reg_4.provisionada
                                       ) #am
            END IF
 
       ELSE

         INSERT INTO excep_trasp_sal VALUES (3,det_sal.*)

       END IF
 
    END FOREACH

    CALL carga_his_saldos( det_sal.fech_presentacion ,
                           det_sal.fech_mov_banxico  ,
                           reg.total_saldo_sar_92    ,
                           reg.total_saldo_viv_92
                           ) #chs

    DECLARE cur_5 CURSOR FOR 

    SELECT C.*  ,
           D.*  ,
           USER
    FROM   tra_cza_trasp_issste C,tra_det_trasp_int_issste D
    WHERE  C.folio = folio
    AND    C.folio = D.folio

    LET reg.total_int_sar_92   = 0
    LET reg.total_int_viv_92   = 0

    FOREACH cur_5 INTO cza.*,det_int.*,usuario

        LET num_reg_procesados = num_reg_procesados + 1
        DISPLAY "REGISTROS PROCESADOS ",num_reg_procesados AT 17,10

        SELECT A.coduni_n1 
        INTO   c10_coduni_n1
        FROM   afi_mae_afiliado A
        WHERE  A.n_seguro = det_int.n_seguro

    IF STATUS <> NOTFOUND THEN

         LET reg.total_int_sar_92 = reg.total_int_sar_92 +
                                    det_int.int_sar_92

         LET indicador_inv = 0
         LET sub_inv       = 0

         SELECT b.indicador_inv
         INTO   indicador_inv
         FROM  tra_tab_tiptra b
         WHERE b.origen_traspaso = det_int.orig_tipo_traspaso 

        CASE  indicador_inv

           WHEN   1
              LET sub_inv = 13 
              EXIT CASE
           OTHERWISE 
              LET sub_inv = 19
              EXIT CASE

        END CASE

        CALL dispersa_int(c10_coduni_n1      ,
                          det_int.*          ,
                          det_int.int_sar_92 ,
                          sub_inv            ,
                          4
                          ) #di


       LET reg.total_int_viv_92 = reg.total_int_viv_92 +
                  det_int.int_viv_92

       CALL dispersa_int(c10_coduni_n1      ,
                         det_int.*          ,
                         det_int.int_viv_92 ,
                         14                  ,
                         4
                        ) #di

       UPDATE tra_mae_icefa_issste 
          SET    tra_mae_icefa_issste.saldo_sar_92 = tra_mae_icefa_issste.saldo_sar_92 + det_int.int_sar_92,
                 tra_mae_icefa_issste.saldo_viv_92 = tra_mae_icefa_issste.saldo_viv_92 + det_int.int_viv_92
       WHERE  tra_mae_icefa_issste.n_seguro     = det_int.n_seguro         #AFI
         AND  tra_mae_icefa_issste.nss          = det_int.n_seguro_ent_ced #ICE
         AND  tra_mae_icefa_issste.rfc          = det_int.rfc_ent_ced      #ICE
         AND  tra_mae_icefa_issste.icefa_cod    = det_int.cve_ced_cuenta   #ICE
         AND  tra_mae_icefa_issste.id_procesar  = det_int.id_procesar      #ICE
         AND  tra_mae_icefa_issste.status       in (7,8,41,17)
       
 ELSE

     INSERT INTO excep_trasp_int VALUES (3,det_int.*)

 END IF

END FOREACH

    CALL carga_his_saldos (det_int.fech_presentacion ,
                           det_int.fech_mov_banxico  ,
                           reg.total_int_sar_92      ,
                           reg.total_int_viv_92
                          ) #chs

    UPDATE tra_det_trasp_int_issste
    SET    tra_det_trasp_int_issste.estado = 7
    WHERE  tra_det_trasp_int_issste.folio  = folio

    UPDATE tra_det_trasp_sal_issste
    SET    tra_det_trasp_sal_issste.estado = 7
    WHERE  tra_det_trasp_sal_issste.folio  = folio
    

END FUNCTION

FUNCTION dispersa_sal(c10_coduni_n1,x_historico,monto,subcuenta,tipomov)
#ds---------------------------------------------------------------------
    DEFINE
        c10_coduni_n1   LIKE afi_mae_afiliado.coduni_n1           ,
        x_historico RECORD LIKE tra_det_trasp_sal_issste.*        ,
        monto     LIKE tra_det_trasp_sal_issste.saldo_sar_92      ,
        g_sie     RECORD LIKE cta_regimen.*

    DEFINE #loc #smallint
       subcuenta           ,
       tipomov      SMALLINT

    DEFINE #loc #decimal
           valor_en_pesos      DECIMAL(16,6)

    DEFINE val_pes_viv         DECIMAL(16,6) #agrego 08102008
 

    DECLARE cur_3 CURSOR FOR

    SELECT *
    FROM   cta_regimen
    WHERE  cta_regimen.nss         = det_sal.n_seguro
    AND    cta_regimen.subcuenta   = subcuenta

    FOREACH cur_3 INTO g_sie.*

        IF monto > 0 THEN
     #######################
     #RECALCULA VALOR PESOS#
     #######################

    IF ( subcuenta = 13 OR subcuenta = 19 ) THEN

       LET c6_folio_sua   = "SAR 92"
       LET valor_en_pesos = 0
       LET valor_en_pesos = (monto * g_sie.porcentaje)/100

    ELSE   #SuBcUeNtA 14 FoViSsStE

       LET c6_folio_sua   = "VIV 92"
       LET valor_en_pesos = 0
       LET valor_en_pesos = (monto * g_sie.porcentaje)/100
       LET val_pes_viv    = valor_en_pesos
       LET valor_en_pesos = val_pes_viv

    END IF

   LET v_id[1,3] = pre
   LET v_id[4,6] = x_historico.cve_ced_cuenta

	
     IF ( subcuenta = 13 OR subcuenta = 19 ) THEN

        INSERT INTO dis_provision VALUES
                      (tipomov                      ,#tipo_movimiento
                      subcuenta                     ,#subcuenta
                      g_sie.codigo_siefore          ,#siefore
                      x_historico.folio             ,#folio
                      x_historico.cont_servicio     ,#folio
                      x_historico.n_seguro          ,#nss
                      x_historico.n_unico           ,#curp
                      c6_folio_sua                  ,#folio_sua
                      x_historico.fech_mov_banxico  ,#fecha_pago
                      x_historico.fech_mov_banxico  ,#fecha_valor
                      ""                            ,#fecha_conversion
                      valor_en_pesos                ,#monto_en_pesos 
                      0                             ,#monto_en_acciones
                      0                             ,#precio_accion
                      0                             ,#dias_cotizados
                      c10_coduni_n1                 ,#sucursal
                      v_id                          ,#id_aportante
                      7                             ,#estado
                      HOY                           ,#fecha_proceso
                      usuario                       ,#usuario
                      x_historico.fech_presentacion ,#fecha_archivo
                      0                              #etiqueta
                         )

     ELSE  #FOVIIISTE

        INSERT INTO dis_provision VALUES
                      (tipomov                      ,#tipo_movimiento
                      subcuenta                     ,#subcuenta
                      g_sie.codigo_siefore          ,#siefore
                      x_historico.folio             ,#folio
                      x_historico.cont_servicio     ,#folio
                      x_historico.n_seguro          ,#nss
                      x_historico.n_unico           ,#curp
                      c6_folio_sua                  ,#folio_sua
                      x_historico.fech_mov_banxico  ,#fecha_pago
                      x_historico.fech_mov_banxico  ,#fecha_valor
                      ""                            ,#fecha_conversion
                      valor_en_pesos                ,#monto_en_pesos #VIV MODIF
                      x_historico.saldo_aport_est   ,#monto_en_acciones
                      0                             ,#precio_accion
                      0                             ,#dias_cotizados
                      c10_coduni_n1                 ,#sucursal
                      v_id                          ,#id_aportante
                      7                             ,#estado
                      HOY                           ,#fecha_proceso
                      usuario                       ,#usuario
                      x_historico.fech_presentacion ,#fecha_archivo
                      0                              #etiqueta
                         )
     END IF

END IF

END FOREACH

END FUNCTION

FUNCTION dispersa_int(c10_coduni_n1,x_historico,monto,subcuenta,tipomov)
#di---------------------------------------------------------------------
    DEFINE
        c10_coduni_n1      LIKE afi_mae_afiliado.coduni_n1          ,
        x_historico        RECORD LIKE tra_det_trasp_int_issste.*   ,
        monto              LIKE tra_det_trasp_int_issste.int_sar_92 ,
        g_sie              RECORD LIKE cta_regimen.*

    DEFINE #loc #smallint
        subcuenta              ,
        tipomov        SMALLINT

    DEFINE #loc #decimal
           valor_en_pesos       DECIMAL(16,6)
 
    DEFINE val_pes_viv          DECIMAL(16,6) #agrego 08102008

    DECLARE cursor_5 CURSOR FOR

    SELECT *
    FROM   cta_regimen
    WHERE  cta_regimen.nss         =     det_int.n_seguro
    AND    cta_regimen.subcuenta   =     subcuenta

    FOREACH cursor_5 INTO g_sie.*

        IF monto > 0 THEN

     #######################
     #RECALCULA VALOR PESOS#
     #######################

    IF ( subcuenta = 13 OR subcuenta = 19 ) THEN

       LET c6_folio_sua   = "SAR 92"
       LET valor_en_pesos = 0
       LET valor_en_pesos = (monto * g_sie.porcentaje)/100

    ELSE   #SuBcUeNtA 14 FoViSsStE

       LET c6_folio_sua   = "VIV 92"
       LET valor_en_pesos = 0
       LET valor_en_pesos = (monto * g_sie.porcentaje)/100
       LET val_pes_viv    = valor_en_pesos
       LET valor_en_pesos = val_pes_viv

    END IF

    LET v_id[1,3] = pre
    LET v_id[4,6] = x_historico.cve_ced_cuenta


     IF ( subcuenta = 13 OR subcuenta = 19 ) THEN

        INSERT INTO dis_provision VALUES
                      (tipomov                      ,#tipo_movimiento
                      subcuenta                     ,#subcuenta
                      g_sie.codigo_siefore          ,#siefore
                      x_historico.folio             ,#folio
                      x_historico.cont_servicio     ,#folio
                      x_historico.n_seguro          ,#nss
                      x_historico.n_unico           ,#curp
                      c6_folio_sua                  ,#folio_sua
                      x_historico.fech_mov_banxico  ,#fecha_pago
                      x_historico.fech_mov_banxico  ,#fecha_valor
                      ""                            ,#fecha_conversion
                      valor_en_pesos                ,#monto_en_pesos 
                      0                             ,#monto_en_acciones
                      0                             ,#precio_accion
                      0                             ,#dias_cotizados
                      c10_coduni_n1                 ,#sucursal
                      v_id                          ,#id_aportante
                      7                             ,#estado
                      HOY                           ,#fecha_proceso
                      usuario                       ,#usuario
                      x_historico.fech_presentacion ,#fecha_archivo
                      0                              #etiqueta
                         )

     ELSE  #FOVIIISTE

        INSERT INTO dis_provision VALUES
                      (tipomov                      ,#tipo_movimiento
                      subcuenta                     ,#subcuenta
                      g_sie.codigo_siefore          ,#siefore
                      x_historico.folio             ,#folio
                      x_historico.cont_servicio     ,#folio
                      x_historico.n_seguro          ,#nss
                      x_historico.n_unico           ,#curp
                      c6_folio_sua                  ,#folio_sua
                      x_historico.fech_mov_banxico  ,#fecha_pago
                      x_historico.fech_mov_banxico  ,#fecha_valor
                      ""                            ,#fecha_conversion
                      valor_en_pesos                ,#monto_en_pesos #VIV MODIF
                      0                             ,#monto_en_acciones
                      0                             ,#precio_accion
                      0                             ,#dias_cotizados
                      c10_coduni_n1                 ,#sucursal
                      v_id                          ,#id_aportante
                      7                             ,#estado
                      HOY                           ,#fecha_proceso
                      usuario                       ,#usuario
                      x_historico.fech_presentacion ,#fecha_archivo
                      0                              #etiqueta
                         )
     END IF

END IF

END FOREACH

END FUNCTION

FUNCTION carga_his_saldos(inf) 
#chs--------------------------
    DEFINE inf                   RECORD
           fecha_archivo         DATE          ,
           fecha_provision       DATE          ,
           total_importe_sar     DECIMAL(16,6) ,
           total_importe_viv     DECIMAL(16,6)
                                 END RECORD

    SELECT "OK"
    FROM   tra_his_dep_icefa A
    WHERE  A.folio     = folio
    and    A.subcuenta = 13
    GROUP BY 1

    IF STATUS = NOTFOUND THEN

        INSERT INTO tra_his_dep_icefa VALUES (folio               ,
          inf.fecha_archivo     ,
          inf.fecha_provision   ,
          ""                    ,
          13                     ,
          inf.total_importe_sar ,
          ""                    ,
          ""                    ,
          usuario               ,
          7
                );
    ELSE

      SELECT A.total_importe
      INTO   d15_total_importe
      FROM   tra_his_dep_icefa A
      WHERE  A.folio     = folio
      AND    A.subcuenta = 13

      LET d15_total_importe = d15_total_importe + inf.total_importe_sar

      UPDATE tra_his_dep_icefa
      SET    tra_his_dep_icefa.total_importe =      d15_total_importe
      WHERE  tra_his_dep_icefa.folio         =      folio
      AND    tra_his_dep_icefa.subcuenta     =      13

    END IF

    SELECT "OK"
    FROM   tra_his_dep_icefa A
    WHERE  A.folio     = folio
    and    A.subcuenta = 14

    IF STATUS = NOTFOUND THEN

        INSERT INTO tra_his_dep_icefa VALUES (folio               ,
          inf.fecha_archivo     ,
          inf.fecha_provision   ,
          ""                    ,
          14                     ,
          inf.total_importe_viv ,
          ""                    ,
          ""                    ,
          usuario               ,
          7
                );
    ELSE

        LET d15_total_importe = 0

        SELECT A.total_importe
        INTO   d15_total_importe
        FROM   tra_his_dep_icefa A
        WHERE  A.folio     = folio
        AND    A.subcuenta = 14

        LET d15_total_importe = d15_total_importe + inf.total_importe_viv

        UPDATE tra_his_dep_icefa
        SET    tra_his_dep_icefa.total_importe = d15_total_importe
        WHERE  tra_his_dep_icefa.folio         = folio
        AND    tra_his_dep_icefa.subcuenta     = 14

   END IF

END FUNCTION

FUNCTION actualiza_maeicefa(reg_3)
#am-------------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        n_seguro              LIKE tra_mae_icefa_issste.n_seguro              ,
        nss                   LIKE tra_mae_icefa_issste.nss                   ,
        rfc                   LIKE tra_mae_icefa_issste.rfc                   ,
        icefa_cod             LIKE tra_mae_icefa_issste.icefa_cod             ,
        nro_int_cta           LIKE tra_mae_icefa_issste.nro_int_cta           ,
        id_procesar           LIKE tra_mae_icefa_issste.id_procesar           ,
        ident_lote_solic      LIKE tra_det_trasp_sal_issste.ident_lote_solic  ,
        estado_actual         SMALLINT                                        ,
        estado_nuevo          SMALLINT  
    END RECORD

    DEFINE reg_4 RECORD #loc #reg_4
        fecha_genera          CHAR(10) ,
        lote_genera           SMALLINT
    END RECORD
   

    LET reg_4.fecha_genera = reg_3.ident_lote_solic[10,11],"/",
                             reg_3.ident_lote_solic[12,13],"/",
                             reg_3.ident_lote_solic[06,09]

    LET reg_4.lote_genera  = reg_3.ident_lote_solic[14,16]

                LET    s_nro_icefa                       = 0

                SELECT COUNT(*)
                INTO   s_nro_icefa
                FROM   tra_mae_icefa_issste
                WHERE  tra_mae_icefa_issste.n_seguro     = reg_3.n_seguro   #AFI
                AND    tra_mae_icefa_issste.nss          = reg_3.nss        #ICE
                AND    tra_mae_icefa_issste.rfc          = reg_3.rfc        #ICE
                AND    tra_mae_icefa_issste.icefa_cod    = reg_3.icefa_cod  #ICE
                AND    tra_mae_icefa_issste.id_procesar  = reg_3.id_procesar#ICE
                AND    tra_mae_icefa_issste.status       = reg_3.estado_actual

                IF s_nro_icefa = 1 THEN

                  UPDATE tra_mae_icefa_issste
                  SET    tra_mae_icefa_issste.status      = reg_3.estado_nuevo
                  WHERE  tra_mae_icefa_issste.n_seguro    = reg_3.n_seguro #AFI
                  AND    tra_mae_icefa_issste.nss         = reg_3.nss      #ICE
                  AND    tra_mae_icefa_issste.rfc         = reg_3.rfc      #ICE
                  AND    tra_mae_icefa_issste.icefa_cod   = reg_3.icefa_cod#ICE
                  AND    tra_mae_icefa_issste.id_procesar = reg_3.id_procesar#IC
                  AND    tra_mae_icefa_issste.status      = reg_3.estado_actual

                    RETURN

                ELSE
     
                   INSERT INTO tra_no_actualiza_iss VALUES(folio       ,
                                                reg_3.estado_nuevo     ,
                                                reg_3.n_seguro         ,#AFI
                                                reg_3.nss              ,#ICE
                                                reg_3.rfc              ,#ICE
                                                reg_3.icefa_cod        ,#ICE
                                                reg_3.nro_int_cta      ,#ICE
                                                reg_3.id_procesar      ,#ICE
                                                reg_3.ident_lote_solic ,
                                                reg_4.fecha_genera     ,
                                                reg_4.lote_genera      ,
                                                s_nro_icefa
                                                       )

                    RETURN

                END IF

END FUNCTION

##############################################################################
#Owner             => E.F.P.
#Programa TRAB002  => PROVISIONA TRASPASOS ICEFA - AFORE    
#Fecha creacion    => 19 DE MARZO DE 1998
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 03 DE FEBRERO DEL 2005
#Ultima Mod        => 22 DE SEPTIEMBRE DEL 2005
#Objetivo de la Mod==> 
#                  La Modificacion consistio en poner a 2 Decimales la 
#                  variable de pesod de VIV para la subcuenta 8
#                  VERSION UNICA PARA TODAS LAS AFORES.
#Sistema           => TRA-ICE-IMSS   
##############################################################################
DATABASE safre_af
GLOBALS
DEFINE iden char(002)
DEFINE pre char(003)
DEFINE v_id char(006)
    DEFINE reg RECORD #glo #reg
        total_saldo_sar_92    DECIMAL(15,2) ,
        total_saldo_viv_92    DECIMAL(15,2) ,
        total_int_sar_92      DECIMAL(15,2) ,
        total_int_viv_92      DECIMAL(15,2)
    END RECORD

    DEFINE reg_4 RECORD #glo #reg_4
        aceptada_icefa        SMALLINT ,
        devuelta              SMALLINT ,
        no_atendida           SMALLINT ,
        rechazada             SMALLINT ,
        complementarios       SMALLINT ,
        provisionada          SMALLINT 
    END RECORD

    DEFINE #glo #cza #det_sal #det_int #sum
        cza                   RECORD LIKE tra_cza_traspaso.*     ,
        det_sal               RECORD LIKE tra_det_trasp_sal.* ,
        det_int               RECORD LIKE tra_det_trasp_int.* 
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
        num_reg_procesados    ,
        folio                 ,
        total_trasp_sal       ,
        total_trasp_int       ,
        total_trasp           INTEGER

    DEFINE #glo #decimal
 d15_total_importe     DECIMAL(15,2)
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS 
        PROMPT LINE LAST

    CALL init() #i
    OPEN WINDOW trab0021 AT 4,4 WITH FORM "TRAB0021" ATTRIBUTE(BORDER)
    DISPLAY "                            < CTRL-C> Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY" TRAB002         PROVISIONA TRASPASOS ICEFA-AFORE IMSS                         " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET folio = NULL
    INPUT BY NAME folio WITHOUT DEFAULTS
        AFTER FIELD folio
            IF folio IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD folio
            ELSE

                SELECT "OK"
                FROM   tra_cza_traspaso A
                WHERE  A.folio  = folio
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                   ERROR "FOLIO NO EXISTE"
                   NEXT FIELD folio
                ELSE

                    SELECT A.ident_operacion
                    INTO   iden
                    FROM tra_cza_traspaso A
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
            IF aux_pausa MATCHES "[SsNn]" THEN
                DISPLAY "                               " AT 18,2

               UPDATE tra_cza_traspaso
               SET    tra_cza_traspaso.estado = 7
               WHERE  tra_cza_traspaso.folio = folio

         IF aux_pausa MATCHES "[Ss]" THEN
             CALL proceso_principal() #pp
    
{
             INSERT INTO excep_trasp_sal
                    SELECT "2",* 
                    FROM   tra_det_trasp_sal A
             WHERE  A.folio = folio
                    AND    A.n_seguro NOT IN (SELECT B.n_seguro
                                              FROM   cta_regimen B
                  WHERE  B.n_seguro = A.n_seguro
                                       AND    B.subcta   IN(7,8)
                                             )

             INSERT INTO excep_trasp_int
                    SELECT "2",* 
                    FROM   tra_det_trasp_int A
             WHERE  A.folio = folio
                    AND    A.n_seguro NOT IN (SELECT B.n_seguro
                                              FROM   cta_regimen B
                  WHERE  B.n_seguro = A.n_seguro
                                       AND    B.subcta   IN(7,8)
                                             )
}
                    EXIT WHILE
         ELSE
                   DISPLAY "PROCESO CANCELADO" SLEEP 2
                   EXIT PROGRAM
         END IF
            ELSE
                DISPLAY "Solo debe presionar S(i) o N(o)" AT 18,2
     END IF
        END WHILE

        PROMPT "PROCESO FINALIZO NORMALMENTE.... PRESIONE < ENTER > PARA SALIR"         FOR CHAR aux_pausa
    CLOSE WINDOW trab0021
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
    FROM   tra_det_trasp_sal A
    WHERE  A.folio  = folio
    AND    A.estado IN(reg_4.aceptada_icefa, reg_4.complementarios)

    SELECT COUNT(*)
    INTO   total_trasp_int
    FROM   tra_det_trasp_int A
    WHERE  A.folio  = folio
    AND    A.estado IN(reg_4.aceptada_icefa, reg_4.complementarios)

    LET total_trasp = total_trasp_sal + total_trasp_int
    DISPLAY "REGISTROS A PROCESAR ",total_trasp AT 15,10

    DECLARE cur_2 CURSOR FOR 
    SELECT tra_cza_traspaso.*     ,
           tra_det_trasp_sal.* ,
           USER
    FROM   tra_cza_traspaso ,tra_det_trasp_sal
    WHERE  tra_cza_traspaso.folio = folio
    AND    tra_cza_traspaso.folio = tra_det_trasp_sal.folio

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
     CALL dispersa_sal(c10_coduni_n1        ,
                              det_sal.*            ,
                              det_sal.saldo_sar_92 ,
                              7                    ,
                              1
                             )#ds
#ff



     LET reg.total_saldo_viv_92 = reg.total_saldo_viv_92 +
                                  det_sal.saldo_viv_92

     CALL dispersa_sal(c10_coduni_n1        ,
                              det_sal.*            ,
                              det_sal.saldo_viv_92 ,
                              8                    ,
                              1
                             )#ds


       UPDATE tra_mae_icefa 
       SET    tra_mae_icefa.saldo_sar_92 = tra_mae_icefa.saldo_sar_92 + 
               det_sal.saldo_sar_92,
               tra_mae_icefa.saldo_viv_92 = tra_mae_icefa.saldo_viv_92 + 
               det_sal.saldo_viv_92
       WHERE  tra_mae_icefa.n_seguro     = det_sal.n_seguro
       AND    tra_mae_icefa.nss          = det_sal.n_seguro_ent
       AND    tra_mae_icefa.rfc          = det_sal.rfc_ent
       AND    tra_mae_icefa.icefa_cod    = det_sal.cve_ced_cuenta
       AND    tra_mae_icefa.nro_int_cta  = det_sal.nro_ctrl_icefa
       AND    tra_mae_icefa.status IN (7,8,41,17) 

 

       IF det_sal.tipo_icefa = "N" THEN
    
                CALL actualiza_maeicefa(det_sal.n_seguro         ,
                                        det_sal.n_seguro_ent     ,
                                        det_sal.rfc_ent          ,
                                        det_sal.cve_ced_cuenta   ,
                                        det_sal.nro_ctrl_icefa   ,
                                        det_sal.ident_lote_solic ,
                                        reg_4.aceptada_icefa     ,
                                        reg_4.provisionada
                                       ) #am
            END IF
 
 ELSE
     INSERT INTO excep_trasp_sal VALUES (3,det_sal.*)
 END IF
 
    END FOREACH

    CALL carga_his_saldos(det_sal.fech_presentacion ,
                          det_sal.fech_mov_banxico  ,
     reg.total_saldo_sar_92    ,
     reg.total_saldo_viv_92
    ) #chs


    DECLARE cur_5 CURSOR FOR 
    SELECT C.*  ,
           D.*  ,
           USER
    FROM   tra_cza_traspaso C,tra_det_trasp_int D
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

     CALL dispersa_int(c10_coduni_n1      ,
                              det_int.*          ,
                              det_int.int_sar_92 ,
                              7                  ,
                              4
                             ) #di


     LET reg.total_int_viv_92 = reg.total_int_viv_92 +
                  det_int.int_viv_92

     CALL dispersa_int(c10_coduni_n1      ,
                              det_int.*          ,
                              det_int.int_viv_92 ,
                              8                  ,
                              4
                             ) #di

       UPDATE tra_mae_icefa 
   SET    tra_mae_icefa.saldo_sar_92 = tra_mae_icefa.saldo_sar_92 + det_int.int_sar_92,
          tra_mae_icefa.saldo_viv_92 = tra_mae_icefa.saldo_viv_92 + det_int.int_viv_92
       WHERE  tra_mae_icefa.n_seguro     = det_int.n_seguro
   AND    tra_mae_icefa.nss          = det_int.n_seguro_ent_ced
   AND    tra_mae_icefa.rfc          = det_int.rfc_ent_ced
   AND    tra_mae_icefa.icefa_cod    = det_int.cve_ced_cuenta
   AND    tra_mae_icefa.nro_int_cta  = det_int.nro_ctrl_icefa
       AND    tra_mae_icefa.status       in (7,8,41,17)
       
 ELSE
     INSERT INTO excep_trasp_int VALUES (3,det_int.*)
 END IF
    END FOREACH

    CALL carga_his_saldos(det_int.fech_presentacion ,
                     det_int.fech_mov_banxico  ,
     reg.total_int_sar_92      ,
     reg.total_int_viv_92
    ) #chs

    UPDATE tra_det_trasp_int
    SET    tra_det_trasp_int.estado = 7
    WHERE  tra_det_trasp_int.folio  = folio
END FUNCTION

FUNCTION dispersa_sal(c10_coduni_n1,x_historico,monto,subcuenta,tipomov)
#ds---------------------------------------------------------------------
    DEFINE
        c10_coduni_n1   LIKE afi_mae_afiliado.coduni_n1         ,
        x_historico RECORD LIKE tra_det_trasp_sal.*     ,
        monto     LIKE tra_det_trasp_sal.saldo_sar_92 ,
        g_sie     RECORD LIKE cta_regimen.*

    DEFINE #loc #smallint
        subcuenta  ,
 tipomov  SMALLINT

    DEFINE #loc #decimal
 valor_en_pesos         DECIMAL(16,6)

    DEFINE val_pes_viv  DECIMAL(16,2) #agrego
 

    DECLARE cur_3 CURSOR FOR
    SELECT *
    FROM   cta_regimen
    WHERE  cta_regimen.nss = det_sal.n_seguro
    AND    cta_regimen.subcuenta   = subcuenta

    FOREACH cur_3 INTO g_sie.*
        IF monto > 0 THEN

     #######################
     #RECALCULA VALOR PESOS#
     
     IF ( subcuenta = 7 ) THEN
        LET c6_folio_sua   = "SAR 92"
        LET valor_en_pesos = 0
        LET valor_en_pesos = (monto * g_sie.porcentaje)/100
     ELSE
        LET c6_folio_sua   = "VIV 92"
        LET valor_en_pesos = 0
        LET valor_en_pesos = (monto * g_sie.porcentaje)/100
        LET val_pes_viv    = valor_en_pesos
        LET valor_en_pesos = val_pes_viv
     END IF

     #######################

     LET v_id[1,3] = pre
     LET v_id[4,6] = x_historico.cve_ced_cuenta


     IF ( subcuenta = 7 ) THEN
        INSERT INTO dis_provision VALUES
                    (tipomov                       ,#tipo_movimiento
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
                     0                              ,#precio_accion
                     0                             ,#dias_cotizados
                     c10_coduni_n1                 ,#sucursal
                     v_id                          ,#id_aportante
                     7                             ,#estado
                     HOY                           ,#fecha_proceso
                     usuario                       ,#usuario
                     x_historico.fech_presentacion ,#fecha_archivo
                     0                              #etiqueta
                     )
     ELSE
        INSERT INTO dis_provision VALUES
                    (tipomov                       ,#tipo_movimiento
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
        c10_coduni_n1         LIKE afi_mae_afiliado.coduni_n1       ,
 x_historico       RECORD LIKE tra_det_trasp_int.*   ,
        monto        LIKE tra_det_trasp_int.int_sar_92 ,
 g_sie        RECORD LIKE cta_regimen.*

    DEFINE #loc #smallint
        subcuenta        ,
 tipomov        SMALLINT

    DEFINE #loc #decimal
 valor_en_pesos       DECIMAL(16,6)
    DEFINE val_pes_viv  DECIMAL(16,2) #agrego
 

    DECLARE cursor_5 CURSOR FOR
    SELECT *
    FROM   cta_regimen
    WHERE  cta_regimen.nss         = det_int.n_seguro
    AND    cta_regimen.subcuenta   = subcuenta

    FOREACH cursor_5 INTO g_sie.*
        IF monto > 0 THEN

     #######################
     #RECALCULA VALOR PESOS#

     IF ( subcuenta = 7 ) THEN
        LET c6_folio_sua   = "SAR 92"
        LET valor_en_pesos = 0
        LET valor_en_pesos = (monto * g_sie.porcentaje)/100
     ELSE
        LET c6_folio_sua   = "VIV 92"
        LET valor_en_pesos = 0
        LET valor_en_pesos = (monto * g_sie.porcentaje)/100
     ##EMPIEZA LA MODIFICACION 
        LET val_pes_viv    = valor_en_pesos
        LET valor_en_pesos = val_pes_viv   
     ## FIN DE LA MODIFICACION
     END IF

     #######################

     LET v_id[1,3] = pre
     LET v_id[4,6] = x_historico.cve_ced_cuenta

     
     IF ( subcuenta = 7 ) THEN
        INSERT INTO dis_provision VALUES
                   (tipomov                        ,#tipo_movimiento
                    subcuenta                      ,#subcuenta
                    g_sie.codigo_siefore              ,#siefore
                    x_historico.folio              ,#folio
                    x_historico.cont_servicio      ,#folio
                    x_historico.n_seguro           ,#nss
                    x_historico.n_unico            ,#curp
                    c6_folio_sua                   ,#folio_sua
                    x_historico.fech_mov_banxico   ,#fecha_pago
                    x_historico.fech_mov_banxico   ,#fecha_valor
                    ""                             ,#fecha_conversion
                    valor_en_pesos                 ,#monto_en_pesos 
                    0                              ,#monto_en_acciones
                    0                              ,#precio_accion
                    0                              ,#dias_cotizados
                    c10_coduni_n1                  ,#sucursal
                    v_id                ,#id_aportante
                    7                              ,#estado
                    HOY                            ,#fecha_proceso
                    usuario                        ,#usuario
                    x_historico.fech_presentacion  ,#fecha_archivo
                    0                               #etiqueta
                        )
     ELSE
        INSERT INTO dis_provision VALUES
                   (tipomov                        ,#tipo_movimiento
                    subcuenta                      ,#subcuenta
                    g_sie.codigo_siefore              ,#siefore
                    x_historico.folio              ,#folio
                    x_historico.cont_servicio      ,#folio
                    x_historico.n_seguro           ,#nss
                    x_historico.n_unico            ,#curp
                    c6_folio_sua                   ,#folio_sua
                    x_historico.fech_mov_banxico   ,#fecha_pago
                    x_historico.fech_mov_banxico   ,#fecha_valor
                    ""                             ,#fecha_conversion
                    valor_en_pesos                 ,#monto_en_pesos #VIVMOD
                    0                              ,#monto_en_acciones
                    0                              ,#precio_accion
                    0                              ,#dias_cotizados
                    c10_coduni_n1                  ,#sucursal
                    v_id                ,#id_aportante
                    7                              ,#estado
                    HOY                            ,#fecha_proceso
                    usuario                        ,#usuario
                    x_historico.fech_presentacion  ,#fecha_archivo
                    0                               #etiqueta
                        )
     END IF

 END IF
    END FOREACH
END FUNCTION

FUNCTION carga_his_saldos(inf) 
#chs--------------------------
    DEFINE inf RECORD
        fecha_archivo         DATE          ,
 fecha_provision       DATE          ,
 total_importe_sar     DECIMAL(16,6) ,
 total_importe_viv     DECIMAL(16,6)
    END RECORD

    SELECT "OK"
    FROM   tra_his_dep_icefa A
    WHERE  A.folio     = folio
    and    A.subcuenta = 7
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        INSERT INTO tra_his_dep_icefa VALUES (folio               ,
          inf.fecha_archivo     ,
          inf.fecha_provision   ,
          ""                    ,
          7                     ,
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
        AND    A.subcuenta = 7

        LET d15_total_importe = d15_total_importe + inf.total_importe_sar

        UPDATE tra_his_dep_icefa
      SET    tra_his_dep_icefa.total_importe = d15_total_importe
      WHERE  tra_his_dep_icefa.folio     = folio
      AND    tra_his_dep_icefa.subcuenta = 7
    END IF

    SELECT "OK"
    FROM   tra_his_dep_icefa A
    WHERE  A.folio     = folio
    and    A.subcuenta = 8

    IF STATUS = NOTFOUND THEN
        INSERT INTO tra_his_dep_icefa VALUES (folio               ,
          inf.fecha_archivo     ,
          inf.fecha_provision   ,
          ""                    ,
          8                     ,
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
 AND    A.subcuenta = 8

   LET d15_total_importe = d15_total_importe + inf.total_importe_viv

   UPDATE tra_his_dep_icefa
 SET    tra_his_dep_icefa.total_importe = d15_total_importe
 WHERE  tra_his_dep_icefa.folio         = folio
 AND    tra_his_dep_icefa.subcuenta     = 8
  END IF

END FUNCTION

FUNCTION actualiza_maeicefa(reg_3)
#am-------------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        n_seguro              LIKE tra_mae_icefa.n_seguro              ,
        nss                   LIKE tra_mae_icefa.nss                   ,
        rfc                   LIKE tra_mae_icefa.rfc                   ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod             ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta           ,
        ident_lote_solic      LIKE tra_det_trasp_sal.ident_lote_solic ,
        estado_actual         SMALLINT                            ,
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

                SELECT COUNT(*)
                INTO   s_nro_icefa
                FROM   tra_mae_icefa
                WHERE  tra_mae_icefa.n_seguro     = reg_3.n_seguro
                AND    tra_mae_icefa.nss          = reg_3.nss
                AND    tra_mae_icefa.rfc          = reg_3.rfc
                AND    tra_mae_icefa.icefa_cod    = reg_3.icefa_cod 
                AND    tra_mae_icefa.nro_int_cta  = reg_3.nro_int_cta
                AND    tra_mae_icefa.fecha_genera = reg_4.fecha_genera
               #AND    tra_mae_icefa.lote_genera  = reg_4.lote_genera
                AND    tra_mae_icefa.status       = reg_3.estado_actual

                IF s_nro_icefa = 1 THEN
                    UPDATE tra_mae_icefa
                    SET    tra_mae_icefa.status = reg_3.estado_nuevo
                    WHERE  tra_mae_icefa.n_seguro     = reg_3.n_seguro
                    AND    tra_mae_icefa.nss          = reg_3.nss
                    AND    tra_mae_icefa.rfc          = reg_3.rfc
                    AND    tra_mae_icefa.icefa_cod    = reg_3.icefa_cod 
                    AND    tra_mae_icefa.nro_int_cta  = reg_3.nro_int_cta
                    AND    tra_mae_icefa.fecha_genera = reg_4.fecha_genera
                   #AND    tra_mae_icefa.lote_genera  = reg_4.lote_genera
                    AND    tra_mae_icefa.status       = reg_3.estado_actual
                    RETURN
                ELSE
               INSERT INTO tra_no_actualiza VALUES(folio           ,
                                                   reg_3.estado_nuevo     ,
                                                   reg_3.n_seguro         ,
                                                   reg_3.nss              ,
                                                   reg_3.rfc              ,
                                                   reg_3.icefa_cod        ,
                                                   reg_3.nro_int_cta      ,
                                                   reg_3.ident_lote_solic ,
                                                   reg_4.fecha_genera     ,
                                                   reg_4.lote_genera      ,
                                                   s_nro_icefa
                                                   )
                    RETURN
                END IF
END FUNCTION

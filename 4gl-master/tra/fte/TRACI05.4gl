############################################################################## 
#Owner             => E.F.P.  
#Programa TRACI05  => LIQUIDACION DE APORTACIONES ICEFAS    
#Fecha creacion    => 25 DE MARZO DE 1998 
#By                => JESUS DAVID YANEZ MORENO 
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS 
#Fecha de Mod      => 28 DE FEBRERO DEL 2005 
#Fecha de Mod      => 27 DE SEPTIEMBRE DEL 2005
#Objetivo Mod      => QUE LAS PARTICIPACIONES LAS TOMARA A 2 DECIMALES.
#                     CALCULE  x_reg.monto_en_pesos = x_reg.monto_en_acciones
#                     * valor_accion
#                     YA NO ASI --LET x_reg.monto_en_acciones = x_reg.monto_en_p
#                     esos/valor_accion
#                     SoLo PaRa SuBcUeNtA =    8 Viv
#Ultima Mod        => 14 DE OCTUBRE DEL 2008
#Objetivo de la Mod=> Entrada en Vigor de lo de FOVIIISTE para la Subcuenta
#                     14 Anteriormente solo se Prov. y liquidaban los Pesos
#                     pero con lo de la Entrada en vigor se tienen que Comprar
#                     y Liquidar Acciones y estas se invertirían en la Siefore
#                     12 y ya no en la 0.
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af

GLOBALS
DEFINE ffol           INTEGER
DEFINE g_siefore_rcv  SMALLINT

DEFINE bandera_udi CHAR(001)
DEFINE vv_corr INTEGER
DEFINE g_param_tra RECORD LIKE seg_modulo.*
DEFINE xxx char(050)
DEFINE xxxx CHAR(002)

DEFINE act  decimal(10,2)

DEFINE reg_llave RECORD 
       n_seguro       char(011),
       n_seguro_ent   char(011),
       rfc_ent        char(013),
       cve_ced_cuenta char(003),
       nro_ctrl_icefa char(030)
END RECORD

DEFINE uu char(008)
DEFINE u1 char(4)
DEFINE u2 char(2)
DEFINE u3 char(2)
DEFINE g_sal ARRAY[500] OF RECORD #glo #g_sal
       folio                 INTEGER       ,
       fecha_archivo         DATE          ,
       total_importe         DECIMAL(16,6) ,
       comision              DECIMAL(16,6) ,
       seleccion             CHAR(01)
                           END RECORD

DEFINE g_reg RECORD #g_reg
       n_seguro             LIKE tra_det_trasp_sal_issste.n_seguro        ,
       cve_ced_cuenta       LIKE tra_det_trasp_sal_issste.cve_ced_cuenta  ,
       nro_ctrl_icefa       LIKE tra_det_trasp_sal_issste.nro_ctrl_icefa
             END RECORD

DEFINE reg_4 RECORD #glo #reg_4
       aceptada              SMALLINT ,
       devuelta              SMALLINT ,
       no_atendida           SMALLINT ,
       rechazada             SMALLINT ,
       complementarios       SMALLINT ,
       provisionada          SMALLINT , 
       liquidada             SMALLINT
END RECORD

DEFINE
       udi1  RECORD LIKE tra_cotiza_udi.* ,
       udi2  RECORD LIKE tra_cotiza_udi.*

DEFINE #date
       HOY           ,
       fecha_parti   ,
       fecha1        ,
       fecha2        DATE

DEFINE #glo #char
       usuario               CHAR(008) ,
       c10_fecha_valor       CHAR(010) ,
       opc                   CHAR(001) ,
       tipo_liquida          CHAR(002) ,
       tipo_comision         CHAR(002) ,
       aux_pausa             CHAR(001) ,
       tipo                  CHAR(001) ,
       uno                   CHAR(001) ,
       dos                   CHAR(001) ,
       enter                 CHAR(001) ,
       G_LISTA               CHAR(100) ,
       ch                    CHAR(100)

DEFINE #glo #smallint
       s_codigo_afore        ,
       salir                 ,
       sigue                 ,
       arr_c                 ,
       arr_l                 SMALLINT

DEFINE #integer
       i                 ,
       cont              ,
       correlativo       INTEGER

DEFINE #decimal
       d16_actualiz_int      DECIMAL(16,6)  ,
       d16_actualiz_udis     DECIMAL(16,6)  ,
       total_accion          DECIMAL(16,6)  ,
---    valor_accion          DECIMAL(16,6)  ,
       valor_accion          DECIMAL(19,14) ,
       importe_liquida       DECIMAL(16,6)  ,
       importe_comision      DECIMAL(16,6)  ,
       importe_total         DECIMAL(16,6)  ,
       totala                DECIMAL(16,2)  ,
       totalc                DECIMAL(16,2)

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT 

 CALL STARTLOG("TRACI05.log")
    
    LET HOY = TODAY
    
LET xxx = "SET PDQPRIORITY 100"
PREPARE qr FROM xxx
EXECUTE qr    

    CALL init()
    OPTIONS PROMPT LINE LAST
    OPEN WINDOW trac0051 AT 3,3 WITH FORM "TRAC0051" ATTRIBUTE(BORDER)
    DISPLAY "TRACI05          LIQUIDACION DE TRASPASO ICEFA-AFORE ISSSTE                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "LIQUIDA"
        COMMAND "LIQUIDAR"
                "LIQUIDACION DE APORTACIONES E INTERESES SAR92 Y VIVIENDA 92"
                 CALL liquida() #l
        COMMAND "Salir" "SALIR DEL PROGRAMA"
                 EXIT PROGRAM
    END MENU
    CLOSE WINDOW trac0051
END MAIN

FUNCTION init()
#i-------------
    SELECT codigo_afore   ,
           USER 
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT estado
    INTO   reg_4.provisionada
    FROM   tra_status
    WHERE  des_estado = "PROVISIONADA"

    SELECT estado
    INTO   reg_4.liquidada
    FROM   tra_status
    WHERE  des_estado = "LIQUIDADA"

    SELECT *
    INTO   g_param_tra.*
    FROM   seg_modulo
    WHERE   modulo_cod = "tra"

CREATE TEMP TABLE temp_rh(nss char(011))

END FUNCTION

FUNCTION liquida()
#l----------------
    INPUT BY NAME uno    ,
                  dos    ,
                  fecha1 ,
                  fecha2

        AFTER FIELD uno
            IF uno = "X" THEN
                CALL despliega_saldos("SAR") #ds
                CALL despliega_deposito("SAR") RETURNING sigue #dd
                NEXT FIELD fecha1
            END IF

        AFTER FIELD dos
            IF dos = "X" THEN
                CALL despliega_saldos("VIV") #ds
                CALL despliega_deposito("VIV") RETURNING sigue #dd
                NEXT FIELD fecha1
            END IF

 BEFORE FIELD fecha1
     LET fecha1 = HOY
     DISPLAY BY NAME fecha1

 AFTER FIELD fecha1
     IF fecha1 IS NULL THEN
         ERROR "INGRESE FECHA DE PAGO"
         NEXT FIELD fecha1
     END IF

 BEFORE FIELD fecha2
     LET fecha2 = HOY
     DISPLAY BY NAME fecha2

 AFTER FIELD fecha2
     IF fecha2 IS NULL THEN
         ERROR "INGRESE FECHA DE LIQUIDACION"
         NEXT FIELD fecha2
     END IF
     IF (DAY(fecha2) <> 1 AND 
	uno = "X") THEN
	CALL opc_udi() RETURNING bandera_udi
     END IF

   ON KEY ( ESC )
            LET salir = FALSE

     IF uno IS NULL AND dos IS NULL THEN
         ERROR " AL MENOS DEBE INGRESAR UNA OPCION"
         NEXT FIELD uno
     END IF
   
     IF fecha1 IS NULL THEN
         ERROR "INGRESE FECHA DE PROCESO"
         NEXT FIELD fecha1
     END IF

     IF fecha2 IS NULL THEN
         ERROR "INGRESE FECHA DE LIQUIDACION"
         NEXT FIELD fecha2
     END IF

     LET cont = 0

     IF uno = "X" THEN
                LET cont = cont + 1 
            END IF

     IF dos = "X" THEN
                LET cont = cont + 1
            END IF

     IF cont > 1 THEN
                ERROR "PUEDE PROCESAR UNA SOLA ACREDITACION A LA VEZ"
         NEXT FIELD uno
     END IF

##MOD 14102008 
        IF uno = "X" THEN

            LET valor_accion = 0

            SELECT precio_del_dia
            INTO   valor_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = fecha2
            AND    codigo_siefore = 1

  ###### IF uno = "X" THEN
            IF STATUS = NOTFOUND THEN
                ERROR "NO EXISTE VALOR DE ACCION SB1 PARA LA FECHA DE ",
                      "LIQUIDACION : ",fecha2 USING "DD/MM/YYYY"
                NEXT FIELD fecha2
            END IF
  ##### END IF
 
            LET valor_accion = 0

            SELECT precio_del_dia
            INTO   valor_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = fecha2
            AND    codigo_siefore = 2

#####    IF uno = "X" THEN
            IF STATUS = NOTFOUND THEN
                ERROR "NO EXISTE VALOR DE ACCION SB2 PARA LA FECHA DE ",
                      "LIQUIDACION : ",fecha2 USING "DD/MM/YYYY"
                NEXT FIELD fecha2
            END IF
        END IF

##### MODIFICACION 14102008
         IF dos = "X" THEN
            LET fecha_parti = MDY(MONTH(fecha2),'01',YEAR(fecha2))

            LET valor_accion = 0

            SELECT precio_del_dia
            INTO   valor_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = fecha_parti
            AND    codigo_siefore = 12   # SIEFORE DE FOVI SUBCTA 14
                     #PARA IMSS siefore 11 SUBCTA 4,8 

            IF STATUS = NOTFOUND THEN
                ERROR "NO EXISTE VALOR DE PARTICIPACION PARA LA FECHA DE ",
                      "LIQUIDACION : ",fecha_parti USING "DD/MM/YYYY"
                NEXT FIELD fecha2
            END IF
         END IF

##### 
            CASE
                WHEN uno = "X" 
                    CALL despliega_deposito("SAR") RETURNING sigue #dd
                    IF NOT sigue THEN
                        NEXT FIELD uno
                    END IF

                WHEN dos = "X"
                    CALL despliega_deposito("VIV") RETURNING sigue
                    IF NOT sigue THEN
                        NEXT FIELD uno
                    END IF
            END CASE
     EXIT INPUT

     ON KEY ( INTERRUPT )
                LET salir = TRUE
         EXIT INPUT
    END INPUT

    IF salir THEN
        RETURN
    END IF
  
    PROMPT "DESEA GENERAR EL PROCESO S/N ? " FOR CHAR aux_pausa

    IF aux_pausa NOT MATCHES "[Ss]" THEN
        CALL Inicializa()
        RETURN
    END IF

    ERROR "PROCESANDO INFORMACION .... ESPERE UN MOMENTO" ATTRIBUTE(BOLD)

   #LOCK TABLE dis_provision IN EXCLUSIVE MODE

    FOR i= 1 to arr_c
        IF g_sal[i].seleccion = "X" THEN
            CALL Proceso_principal(g_sal[i].folio) #pp
        END IF
    END FOR

   #UNLOCK TABLE dis_provision

ERROR ""
PROMPT "PROCESO FINALIZO NORMALMENTE... PRESIONE < ENTER > PARA SALIR" FOR CHAR aux_pausa
END FUNCTION

FUNCTION inicia_saldos()
#is---------------------
    DEFINE 
        i        smallint

    INITIALIZE g_sal TO NULL
    FOR i = 1 TO 50
       #DISPLAY g_sal[i].folio         TO scr_liquida[i].folio 
       #DISPLAY g_sal[i].fecha_archivo TO scr_liquida[i].fecha_archivo 
       #DISPLAY g_sal[i].total_importe TO scr_liquida[i].aportacion 
       #DISPLAY g_sal[i].comision      TO scr_liquida[i].comision 
       #DISPLAY g_sal[i].seleccion     TO scr_liquida[i].seleccion 
    END FOR
END FUNCTION

FUNCTION despliega_saldos(aporte)
#ds------------------------------
    DEFINE
        aporte               CHAR(3)


    OPEN WINDOW trac0052 AT 9,3 WITH FORM "TRAC0052" ATTRIBUTE (BORDER)
    DISPLAY "TRASPASOS PENDIENTES POR LIQUIDAR" AT 1,15
    DISPLAY aporte AT 1,55
    DISPLAY "-------------------------------------------------------------",
            "----------------" AT 2,1

    CALL inicia_saldos() #is

    LET tipo_liquida     = " "
    LET tipo_comision    = " "
    LET importe_liquida  = 0
    LET importe_comision = 0
    LET importe_total    = 0
    LET total_accion     = 0


    CASE
       WHEN  aporte = "SAR"
           LET tipo_liquida  = 13

       WHEN  aporte = "VIV"
           LET tipo_liquida  = 14
    END CASE
 
    DECLARE cur_1 CURSOR FOR
    SELECT folio            ,
           fecha_archivo    ,
           total_importe    ,
           0
    FROM   tra_his_dep_icefa
    WHERE  subcuenta = tipo_liquida 
    AND    estado    = reg_4.provisionada
    ORDER BY 1,2

    LET totala = 0
    LET totalc = 0
    LET i      = 1
    FOREACH cur_1 INTO g_sal[i].*
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    INPUT ARRAY g_sal WITHOUT DEFAULTS FROM scr_liquida.*
        BEFORE FIELD seleccion
            LET arr_c = ARR_CURR()
            LET arr_l = SCR_LINE()

        AFTER FIELD seleccion
            IF g_sal[arr_c].total_importe IS NULL THEN
                LET g_sal[arr_c].total_importe = 0
            END IF

            IF g_sal[arr_c].comision IS NULL THEN
                LET g_sal[arr_c].comision = 0
            END IF

        ON KEY (ESC)
        FOR i = 1 TO ARR_CURR()
                IF g_sal[i].seleccion = "X" THEN
                    LET totala = totala + g_sal[i].total_importe
                    LET totalc = totalc + g_sal[i].comision
                END IF
            END FOR
            DISPLAY BY NAME totala,totalc 

            PROMPT "ES CORRECTA LA SUMA A LIQUIDAR [S/N] ?" FOR opc
            IF opc MATCHES "[Ss]" THEN
                EXIT INPUT
            END IF 

            FOR i = 1 TO arr_c 
                LET g_sal[i].seleccion = NULL
                LET totala = 0
                LET totalc = 0
                DISPLAY  g_sal[i].seleccion TO scr_liquida[i].seleccion
                DISPLAY BY NAME totala,totalc 
            END FOR
    END INPUT
    CLOSE WINDOW trac0052
END FUNCTION

FUNCTION Proceso_principal(vfolio)
#pp-------------------------------
    DEFINE
        vfolio                INTEGER

    LET ffol = vfolio
    CALL separa_nss(vfolio)

    CASE 
        WHEN uno = "X"     # APORTE SAR 92
            CALL proceso_7(vfolio) #p7

        WHEN dos = "X"     # APORTE VIV
            CALL proceso_8(vfolio) #p8
    END CASE
END FUNCTION

FUNCTION proceso_7(vfolio)
#p7-----------------------
    DEFINE #x_reg
        x_reg                 RECORD LIKE dis_provision.*

    DEFINE #loc #smallint
        sw_1                  SMALLINT

    DEFINE
        k                     CHAR(600) ,
        vfolio                INTEGER

    DECLARE cur_2 CURSOR FOR
    SELECT *
    FROM   dis_provision
    WHERE  folio     = vfolio
    AND    estado    = reg_4.provisionada
    AND    subcuenta in (13,19)

    LET cont = 0
    
    LET G_LISTA = g_param_tra.ruta_envio CLIPPED ,"/A"

    START REPORT salida TO G_LISTA
      SELECT * 
        INTO   udi1.*
        FROM   tra_cotiza_udi
      WHERE  tra_cotiza_udi.fecha_valor_udi = fecha2
  
        LET c10_fecha_valor   = fecha2
--      LET c10_fecha_valor   = c10_fecha_valor[01,02],"/01/",
--                              c10_fecha_valor[07,10]
        LET x_reg.fecha_valor = c10_fecha_valor

      SELECT * 
        INTO   udi2.*
        FROM   tra_cotiza_udi
      WHERE  tra_cotiza_udi.fecha_valor_udi = x_reg.fecha_valor

        LET correlativo  = 0
        LET x_reg.estado = 8
        LET x_reg.fecha_proceso = fecha1

 FOREACH cur_2 INTO x_reg.*

     LET x_reg.fecha_valor = c10_fecha_valor

     SELECT a.codigo_siefore
     INTO   g_siefore_rcv
     FROM   cta_regimen a
     WHERE  a.nss = x_reg.nss
     AND    a.subcuenta = x_reg.subcuenta

     LET valor_accion = 0

      SELECT precio_del_dia
        INTO   valor_accion
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = fecha2
        AND    codigo_siefore = x_reg.siefore

  IF x_reg.tipo_movimiento = 1 THEN
     SELECT a.n_seguro      ,
            a.n_seguro_ent  ,
            a.rfc_ent       ,
            a.cve_ced_cuenta,
            a.nro_ctrl_icefa,
	    a.ident_lote_solic[6,13]
     INTO   reg_llave.*,
	    uu
     FROM   tra_det_trasp_sal_issste a
     WHERE  a.folio         = x_reg.folio
     AND    a.cont_servicio = x_reg.consecutivo_lote
     END IF

  IF x_reg.tipo_movimiento = 4 THEN
     SELECT a.n_seguro      ,
            a.n_seguro_ent_ced  ,
            a.rfc_ent_ced       ,
            a.cve_ced_cuenta,
            a.nro_ctrl_icefa ,
	    a.ident_lote_solic[6,13]
     INTO   reg_llave.*,
	    uu
     FROM   tra_det_trasp_int_issste a
     WHERE  a.folio         = x_reg.folio
     AND    a.cont_servicio = x_reg.consecutivo_lote
  END IF
  LET u1 = uu[1,4]
  LET u2 = uu[5,6]
  LET u3 = uu[7,8]

     SELECT A.correlativo
     INTO  vv_corr
     FROM   tra_mae_icefa_issste A 
     WHERE  A.n_seguro    = reg_llave.n_seguro
     AND    A.nss         = reg_llave.n_seguro_ent
     AND    A.rfc         = reg_llave.rfc_ent
     AND    A.icefa_cod   = reg_llave.cve_ced_cuenta
     AND    A.nro_int_cta = reg_llave.nro_ctrl_icefa
     --AND    YEAR(A.fecha_genera) =  u1
     --AND    MONTH(A.fecha_genera) =  u2
     --AND    DAY(A.fecha_genera) =  u3
     AND    A.status in (7,8,17)

     SELECT "OK"
     FROM v_viv A
     WHERE A.n_seguro      = x_reg.nss
     AND   A.cont_servicio = x_reg.consecutivo_lote

     IF STATUS = NOTFOUND  THEN
        CALL f_desmarca_cuenta(x_reg.nss,260,vv_corr)
     END IF

     LET act = 0
     OUTPUT TO REPORT salida(x_reg.*) #s
  IF x_reg.tipo_movimiento = 1 THEN
     SELECT a.n_seguro      ,
            a.n_seguro_ent  ,
            a.rfc_ent       ,
            a.cve_ced_cuenta,
            a.nro_ctrl_icefa
     INTO   reg_llave.*
     FROM   tra_det_trasp_sal_issste a
     WHERE  a.folio         = x_reg.folio
     AND    a.cont_servicio = x_reg.consecutivo_lote
     END IF

  IF x_reg.tipo_movimiento = 4 THEN
     SELECT a.n_seguro      ,
            a.n_seguro_ent_ced  ,
            a.rfc_ent_ced       ,
            a.cve_ced_cuenta,
            a.nro_ctrl_icefa
     INTO   reg_llave.*
     FROM   tra_det_trasp_int_issste a
     WHERE  a.folio         = x_reg.folio
     AND    a.cont_servicio = x_reg.consecutivo_lote
  END IF

     UPDATE tra_mae_icefa_issste 
     SET tra_mae_icefa_issste.saldo_sar_92 = tra_mae_icefa_issste.saldo_sar_92 +
                                         act
     WHERE  tra_mae_icefa_issste.n_seguro    = reg_llave.n_seguro
     AND    tra_mae_icefa_issste.nss         = reg_llave.n_seguro_ent
     AND    tra_mae_icefa_issste.rfc         = reg_llave.rfc_ent
     AND    tra_mae_icefa_issste.icefa_cod   = reg_llave.cve_ced_cuenta
     AND    tra_mae_icefa_issste.nro_int_cta = reg_llave.nro_ctrl_icefa
     AND    tra_mae_icefa_issste.status in (7,8,17)

 END FOREACH

        UPDATE tra_his_dep_icefa
        SET    estado            = reg_4.liquidada ,
               fecha_liquidacion = fecha2      # FECHA VALOR ACCION 
        WHERE  folio     = vfolio
        AND    subcuenta in (13,19)
        AND    estado    = reg_4.provisionada

    FINISH REPORT salida
   
END FUNCTION

FUNCTION proceso_8(vfolio)
#p8-----------------------
    DEFINE #x_reg
        x_reg                 RECORD LIKE dis_cuenta.*

    DEFINE #loc #smallint
        sw_1                  SMALLINT

    DEFINE
        k                     char(600) ,
        vfolio                INTEGER

    DECLARE cur_3 CURSOR FOR 
    SELECT *
    FROM   dis_provision
    WHERE  folio     = vfolio
    AND    estado    = reg_4.provisionada
    AND    subcuenta = 14
    ORDER BY nss

    LET cont = 0
    LET G_LISTA = g_param_tra.ruta_envio CLIPPED ,"/A"
    START REPORT salida TO G_LISTA

        SELECT * 
        INTO   udi1.*
        FROM   tra_cotiza_udi
         WHERE  tra_cotiza_udi.fecha_valor_udi = fecha2
  
        LET fecha_parti = MDY(MONTH(fecha2),'01',YEAR(fecha2))
        LET valor_accion = 0

        SELECT  precio_del_dia
        INTO    valor_accion
        FROM    glo_valor_accion
        WHERE   fecha_valuacion = fecha_parti
          AND   codigo_siefore = 12

        LET c10_fecha_valor    = fecha_parti
        LET c10_fecha_valor    = c10_fecha_valor[01,02],"/01/",
                                 c10_fecha_valor[07,10]
        LET x_reg.fecha_valor  = c10_fecha_valor

        SELECT * 
        INTO   udi2.*
        FROM   tra_cotiza_udi
        WHERE  tra_cotiza_udi.fecha_valor_udi = x_reg.fecha_valor

        LET x_reg.estado = 8
        LET x_reg.fecha_proceso = fecha1


        FOREACH cur_3 INTO x_reg.*

  IF x_reg.tipo_movimiento = 1 THEN

     SELECT a.n_seguro      ,
            a.n_seguro_ent  ,
            a.rfc_ent       ,
            a.cve_ced_cuenta,
            a.nro_ctrl_icefa,
	    a.ident_lote_solic[6,13]
     INTO   reg_llave.*,
	    uu
     FROM   tra_det_trasp_sal_issste a
     WHERE  a.folio         = x_reg.folio
     AND    a.cont_servicio = x_reg.consecutivo_lote
     END IF

  IF x_reg.tipo_movimiento = 4 THEN
     SELECT a.n_seguro      ,
            a.n_seguro_ent_ced  ,
            a.rfc_ent_ced       ,
            a.cve_ced_cuenta,
            a.nro_ctrl_icefa,
	    a.ident_lote_solic[6,13]
     INTO   reg_llave.*,
	    uu
     FROM   tra_det_trasp_int_issste a
     WHERE  a.folio         = x_reg.folio
     AND    a.cont_servicio = x_reg.consecutivo_lote
  END IF
LET u1 = uu[1,4]
LET u2 = uu[5,6]
LET u3 = uu[7,8]

     SELECT A.correlativo
     INTO vv_corr
     FROM   tra_mae_icefa_issste A 
     WHERE  A.n_seguro    = reg_llave.n_seguro
     AND    A.nss         = reg_llave.n_seguro_ent
     AND    A.rfc         = reg_llave.rfc_ent
     AND    A.icefa_cod   = reg_llave.cve_ced_cuenta
     AND    A.nro_int_cta = reg_llave.nro_ctrl_icefa
     AND    A.status in (7,8,17)

     SELECT "OK"
     FROM v_viv A
     WHERE A.n_seguro      = x_reg.nss
     AND   A.cont_servicio = x_reg.consecutivo_lote

      IF STATUS <> NOTFOUND THEN
         CALL f_desmarca_cuenta(x_reg.nss,260,vv_corr)
      END IF

     OUTPUT TO REPORT salida(x_reg.*) #s

 END FOREACH
 
        UPDATE tra_his_dep_icefa
        SET    estado            = reg_4.liquidada ,
               fecha_liquidacion = fecha2      # FECHA VALOR ACCION 
        WHERE  folio     = vfolio
        AND    subcuenta = 14
        AND    estado    = reg_4.provisionada
    FINISH REPORT salida
END FUNCTION

FUNCTION despliega_deposito(aporte)
#dd--------------------------------
    DEFINE
        aporte        CHAR(3)

    DISPLAY aporte AT 7,39 ATTRIBUTE(BOLD)

    LET importe_liquida  = totala
    LET importe_comision = totalc

    ERROR "CONFIRME MONTO TOTAL DE LIQUIDACION VS. DEPOSITO BANCARIO"

    LET importe_total = importe_liquida + importe_comision
### IF aporte <> "VIV" THEN
        LET total_accion  = importe_liquida / valor_accion
### ELSE
###     LET total_accion  = 0
### END IF

    DISPLAY BY NAME importe_liquida  ,
                    importe_comision ,
                    importe_total    
    RETURN TRUE 
END FUNCTION


REPORT salida(x_reg)
#s------------------
    DEFINE l_tipo_proc_tes,l_medio  SMALLINT
    DEFINE l_reg_func_tes           RECORD 
           existe                   SMALLINT ,
           rechazo                  SMALLINT ,
           folio_sol                INTEGER
    END RECORD
    DEFINE ejecuta            CHAR(300)
    DEFINE #x_reg  #x_reg_1
        x_reg                 RECORD LIKE dis_provision.*  ,
        r_mto_pesos_2dec      DECIMAL(22,2) ,
        x_reg_1               RECORD LIKE dis_provision.*

    DEFINE #date  
        fecha_anterior        ,
        fecha_actual          DATE

    DEFINE #decimal
        d16_monto_en_pesos    DECIMAL(16,6) ,
        val_peso              DECIMAL(16,6) ,
        val_accion            DECIMAL(16,6)

    OUTPUT 
        LEFT MARGIN 0
        FORMAT

 ON EVERY ROW
            LET l_tipo_proc_tes   = 7
            LET l_medio = 10
            LET x_reg.etiqueta    = 0
            LET cont              = cont + 1
            LET d16_actualiz_udis = 0

IF (x_reg.subcuenta = 13 OR x_reg.subcuenta = 19 ) THEN
         LET x_reg.fecha_conversion  = fecha2

    IF x_reg.subcuenta = 13 THEN
         LET x_reg.monto_en_acciones = x_reg.monto_en_pesos/valor_accion
         LET x_reg.precio_accion     = valor_accion 
    ELSE 
         LET x_reg.monto_en_acciones = 0
         LET x_reg.precio_accion     = 0
    END IF


      INSERT INTO dis_cuenta VALUES (x_reg.*)
       IF x_reg.siefore <> g_siefore_rcv THEN 
         SELECT "OK"
         FROM safre_af:tes_solicitud a
         WHERE a.nss             = x_reg.nss
         AND a.fecha_solicitud   = x_reg.fecha_conversion
         AND a.tipo_traspaso     = 7 --ind issste
         AND a.grupo_regimen     = 8 -- issste
         AND a.folio_origen      = x_reg.folio
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
{
          LET  ejecuta = "EXECUTE PROCEDURE fn_sol_transf_sie(?,?,?,?,?,?,?)"
          PREPARE qry_tes FROM ejecuta 
          DECLARE cur_tes CURSOR FOR qry_tes
          FOREACH cur_tes USING  x_reg.nss         ,
                                 x_reg.subcuenta   ,
                                 x_reg.siefore     ,
                                 g_siefore_rcv     ,
                                 l_tipo_proc_tes   ,
                                 x_reg.folio       ,
                                 l_medio    
                          INTO l_reg_func_tes.*
          END FOREACH
}
               INSERT INTO safre_af:tes_solicitud VALUES
               (x_reg.nss                ,
                "0"                      ,
                x_reg.fecha_conversion   ,
                "15"                     ,
                "7"                      ,
                "8"                      ,
                x_reg.siefore            ,
                g_siefore_rcv            ,
                "100"                    ,
                ""                       ,
                ""                       ,
                x_reg.folio              )
         END IF                       
        END IF
    IF x_reg.tipo_movimiento = 1 THEN
     SELECT a.n_seguro      ,
            a.n_seguro_ent  ,
            a.rfc_ent       ,
            a.cve_ced_cuenta,
            a.nro_ctrl_icefa
     INTO   reg_llave.*
     FROM   tra_det_trasp_sal_issste a
     WHERE  a.folio         = x_reg.folio
     AND    a.cont_servicio = x_reg.consecutivo_lote
    END IF

    IF x_reg.tipo_movimiento = 4 THEN
     SELECT a.n_seguro      ,
            a.n_seguro_ent_ced  ,
            a.rfc_ent_ced       ,
            a.cve_ced_cuenta,
            a.nro_ctrl_icefa
     INTO   reg_llave.*
     FROM   tra_det_trasp_int_issste a
     WHERE  a.folio         = x_reg.folio
     AND    a.cont_servicio = x_reg.consecutivo_lote
    END IF

     UPDATE tra_mae_icefa_issste 
     SET    tra_mae_icefa_issste.usuario   = usuario,
	    tra_mae_icefa_issste.status    = 8
     WHERE  tra_mae_icefa_issste.n_seguro  = reg_llave.n_seguro
     and    tra_mae_icefa_issste.nss       = reg_llave.n_seguro_ent
     and    tra_mae_icefa_issste.rfc       = reg_llave.rfc_ent
     and    tra_mae_icefa_issste.icefa_cod = reg_llave.cve_ced_cuenta
     and    tra_mae_icefa_issste.nro_int_cta = reg_llave.nro_ctrl_icefa
     and    tra_mae_icefa_issste.status = 7

     -- UPDATE tra_det_aut_issste
     -- set estado = 8

    ###### Aporte por actualizacion de la UDI ######

      IF fecha2 <> x_reg.fecha_valor THEN
        IF bandera_udi = "S" THEN
         LET d16_actualiz_udis = x_reg.monto_en_pesos *
            (udi1.valor_udi/udi2.valor_udi-1)

         LET d16_monto_en_pesos      = x_reg.monto_en_pesos
         LET x_reg.monto_en_pesos    = d16_actualiz_udis
         LET x_reg.tipo_movimiento   = 5
         LET x_reg.monto_en_acciones = x_reg.monto_en_pesos /
                                                  valor_accion


        INSERT INTO dis_cuenta VALUES ( x_reg.* )

        LET act = act +  x_reg.monto_en_pesos

                    ###### Aporte por interes actualizacion UDI ######
        LET d16_actualiz_int        = (d16_monto_en_pesos +
                                       x_reg.monto_en_pesos) *
                                      (fecha2 - x_reg.fecha_valor) *
                                       0.02/360
        LET x_reg.tipo_movimiento   = 3
        LET x_reg.monto_en_pesos    = d16_actualiz_int
        LET x_reg.monto_en_acciones = x_reg.monto_en_pesos /
                                      valor_accion

        INSERT INTO dis_cuenta VALUES ( x_reg.* )
        LET act = act +  x_reg.monto_en_pesos

                    ##################################################
      END IF
       END IF

     ELSE

         LET x_reg.fecha_conversion  = fecha2

{
         # --- Lo comente porque las acciones no vienen bien en el Archivo 
           --- Y ya no saldria bien el monto en pesos se quedaria el mismo
           --- monto en pesos que viene desd archivo Origen.

      -- LET x_reg.monto_en_pesos = x_reg.monto_en_acciones * valor_accion

         LET r_mto_pesos_2dec        = x_reg.monto_en_pesos
         LET x_reg.monto_en_pesos    = r_mto_pesos_2dec

         LET x_reg.precio_accion     = valor_accion

         # ----
}

         LET  x_reg.monto_en_acciones     =   x_reg.monto_en_pesos  / valor_accion
         LET x_reg.precio_accion          =   valor_accion
                                                       

         INSERT INTO dis_cuenta VALUES ( x_reg.* )

         IF  x_reg.tipo_movimiento = 1    THEN

            SELECT a.n_seguro,
                   a.n_seguro_ent,
                   a.rfc_ent,
                   a.cve_ced_cuenta,
                   a.nro_ctrl_icefa
            INTO   reg_llave.*
            FROM   tra_det_trasp_sal_issste a
            WHERE  a.folio         = x_reg.folio
            AND    a.cont_servicio = x_reg.consecutivo_lote

          END IF

          IF x_reg.tipo_movimiento = 4 THEN

              SELECT a.n_seguro,
                     a.n_seguro_ent_ced,
                     a.rfc_ent_ced,
                     a.cve_ced_cuenta,
                     a.nro_ctrl_icefa
              INTO   reg_llave.*
              FROM   tra_det_trasp_int_issste a
              WHERE  a.folio         = x_reg.folio
              AND    a.cont_servicio = x_reg.consecutivo_lote

          END IF
 
          UPDATE tra_mae_icefa_issste 
          SET    tra_mae_icefa_issste.usuario   = usuario  ,
	         tra_mae_icefa_issste.status    = 8
          WHERE  tra_mae_icefa_issste.n_seguro  = reg_llave.n_seguro
          AND    tra_mae_icefa_issste.nss       = reg_llave.n_seguro_ent
          AND    tra_mae_icefa_issste.rfc       = reg_llave.rfc_ent
          AND    tra_mae_icefa_issste.icefa_cod = reg_llave.cve_ced_cuenta
          AND    tra_mae_icefa_issste.nro_int_cta = reg_llave.nro_ctrl_icefa
          AND    tra_mae_icefa_issste.status = 7

     END IF

END REPORT

FUNCTION Inicializa()
#i-------------------

     #LET tipo             = " "
     LET uno              = " "
     LET dos              = " "
     LET fecha1           = " "
     LET fecha2           = " "
     LET tipo_liquida     = 0
     LET tipo_comision    = 0
     LET importe_liquida  = 0
     LET importe_comision = 0
     LET importe_total    = 0
     LET total_accion     = 0

     DISPLAY BY NAME tipo             ,
                     uno              ,
                     dos              ,
                     fecha1           ,
                     fecha2           ,
                     tipo_liquida     ,
                     tipo_comision    ,
                     importe_liquida  ,
                     importe_comision ,
                     importe_total    ,
                     total_accion
                     
   #  DISPLAY "                      " AT 5,39
     #ERROR ""

END FUNCTION


FUNCTION separa_nss(sep_folio)
#tp----------------------------

DEFINE xx              INTEGER 
DEFINE sep_folio       INTEGER 

WHENEVER ERROR CONTINUE
DROP table v_viv 
CREATE TEMP TABLE v_viv(n_seguro char(011),cont_servicio integer)
WHENEVER ERROR STOP

INSERT into v_viv 
SELECT unique a.n_seguro ,a.cont_servicio
FROM   tra_det_trasp_sal_issste a 
WHERE  a.folio = sep_folio 
AND    a.saldo_viv_92 > 0

INSERT into v_viv
SELECT unique a.n_seguro,a.cont_servicio
FROM   tra_det_trasp_int_issste a
WHERE  a.folio = sep_folio
AND    a.int_viv_92 > 0

END FUNCTION

FUNCTION f_desmarca_cuenta(vnss,vmarca_entra,vvvv_corr)
#fd------------------------------------------
DEFINE sal_sar ,
       sal_viv ,
       int_sar ,
       int_viv ,
       tot_s   ,
       tot_v DECIMAL(16,2)

DEFINE ejecuta         CHAR(300),
       vvvv_corr       INTEGER

DEFINE vnss            char(11),
       vmarca_entra    smallint,
       vestado_marca   smallint,
       vcodigo_rechazo smallint,
       vusuario        char(008)

DEFINE xcodigo_marca   smallint,
       xcodigo_rechazo smallint,
       r_corr          integer

LET vestado_marca = 0


SELECT user
INTO   vusuario
FROM   tab_afore_local
GROUP BY 1

SELECT "OK" 
FROM   cta_act_marca
WHERE  nss =  vnss
AND    marca_cod = 140 
GROUP BY 1

IF STATUS <> NOTFOUND THEN

  INSERT INTO temp_rh VALUES(vnss)

  INSERT INTO safre_af:cta_his_inhabilitada
  SELECT a.*
  FROM safre_af:cta_act_marca a
  WHERE a.nss = vnss
  AND   a.marca_cod = 140


  SELECT a.correlativo 
  INTO   r_corr
  FROM   cta_act_marca a
  WHERE  nss       = vnss
  AND    marca_cod = 140

  LET ejecuta = "EXECUTE PROCEDURE desmarca_cuenta(","'",vnss,"'",
  						     ",140,",
						     r_corr ,",",
						     "0,",
						     vmarca_entra,",",
						     "'",vusuario,"')"
LET ejecuta = ejecuta CLIPPED

PREPARE clausula_spl2_1 FROM ejecuta
EXECUTE clausula_spl2_1

END IF

SELECT "OK"
FROM   temp_rh a
WHERE  a.nss = vnss
GROUP 	BY 1

IF STATUS <> NOTFOUND THEN

SELECT a.saldo_sar_92 ,
       a.saldo_viv_92
INTO sal_sar ,
     sal_viv
FROM  safre_af:tra_det_trasp_sal_issste a
WHERE  a.folio = ffol
AND a.n_seguro        = reg_llave.n_seguro
AND a.n_seguro_ent    = reg_llave.n_seguro_ent
AND a.rfc_ent         = reg_llave.rfc_ent
AND a.cve_ced_cuenta  = reg_llave.cve_ced_cuenta
AND a.nro_ctrl_icefa  = reg_llave.nro_ctrl_icefa

SELECT a.int_sar_92 ,
       a.int_viv_92
into   int_sar ,
       int_viv
FROM   tra_det_trasp_int_issste a
WHERE  a.folio = ffol
AND a.n_seguro           = reg_llave.n_seguro
AND a.n_seguro_ent_ced   = reg_llave.n_seguro_ent
AND a.rfc_ent_ced        = reg_llave.rfc_ent
AND a.cve_ced_cuenta     = reg_llave.cve_ced_cuenta
AND a.nro_ctrl_icefa     = reg_llave.nro_ctrl_icefa


LET tot_s = sal_sar + int_sar
LET tot_v = sal_viv + int_viv

INSERT into safre_af:cta_rehabilitada VALUES
(vnss,
0,
0,
0,
0,
0,
tot_s,
tot_v,
TODAY,
260,
TODAY,
0,
vusuario)

END IF

LET ejecuta = "EXECUTE PROCEDURE desmarca_cuenta(","'",vnss,"'",
						    ",",vmarca_entra,",",
						    vvvv_corr,",",
						    vestado_marca,",",
						    vmarca_entra,",",
						    "'",vusuario,"')"

LET ejecuta = ejecuta CLIPPED
WHENEVER ERROR CONTINUE
 PREPARE clausula_spl2 FROM ejecuta
 EXECUTE clausula_spl2
WHENEVER ERROR STOP

END FUNCTION

FUNCTION opc_udi()
#ou---------------

DEFINE opcion_udi CHAR(001)


 OPEN WINDOW trac0053 AT 13,10 WITH FORM "TRAC0053" ATTRIBUTE(BORDER)
    DISPLAY "          CONFIRMACION UDI            " AT 1,1 ATTRIBUTE(REVERSE)


INPUT BY NAME opcion_udi WITHOUT DEFAULTS
AFTER FIELD opcion_udi
    WHILE TRUE
        IF opcion_udi MATCHES "[sSnN]" THEN
            IF opcion_udi MATCHES "[sS]" THEN

                EXIT WHILE
            ELSE
                EXIT WHILE
            END IF
        ELSE
	    NEXT FIELD opcion_udi
	END IF 
    END WHILE

    WHILE TRUE
        PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT INPUT
            ELSE
                NEXT FIELD opcion_udi
            END IF
        END IF
    END WHILE
    
ON KEY (INTERRUPT)
   NEXT FIELD opcion_udi
END INPUT

CLOSE WINDOW trac0053
RETURN opcion_udi

END FUNCTION


#############################################################################
#Proyecto          => SAFRE (Mexico)                                        #
#Propietario       => E.F.P.                                                #
#Programa ACRB057  => LIQUIDACION DEVOLUCION ANUALIDADES GARANTIZADAS       #
#Fecha creacion    => 14 DE ENERO DE 2010                                   #
#Por               => MAURO MUNIZ CBALLERO.                                 #
#Sistema           => ACR                                                   #
#############################################################################

DATABASE safre_af

GLOBALS

   DEFINE
      g_param_taa  RECORD LIKE seg_modulo.*

   DEFINE g_sal ARRAY[500] OF RECORD
      folio          INTEGER,
      fecha_archivo  DATE,
      aportacion     DECIMAL(16,2),
      interes        DECIMAL(16,2),
      seleccion      CHAR(01)
   END RECORD,

   arr_c             ,
   arr_l             ,
   i                 SMALLINT,
   vsaldo_pesos      ,
   totala            ,
   totalc            DECIMAL(16,2)

    DEFINE l_depositos RECORD
        num             ,
        cuenta          SMALLINT,
        valor           DECIMAL(16,6)
    END RECORD

    DEFINE
        HOY             ,
        fecha1          ,
        fecha2          ,
        z_fecha         DATE

    DEFINE
        opc             ,
        aux_pausa       ,
        tipo            ,
        uno             ,
        dos             ,
        tres            ,
        cuatro          ,
        enter           CHAR(1),
        g_usuario       CHAR(8)

    DEFINE
        salir           ,
        cont            ,
        sigue           SMALLINT

    DEFINE
        tipo_desc       CHAR(30),
        g_lista         CHAR(50)

    DEFINE
        aa              ,
        ab              ,
        ac              ,
        ad              DECIMAL(16,6)

    DEFINE
        importe_liquida ,
        importe_interes ,
        importe_total   ,
        valor_accion    ,
        total_accion    DECIMAL(16,6),
        vprecio_accion  DECIMAL(22,14)

    DEFINE
        tipo_liquida    ,
        tipo_comision   CHAR(2)

    DEFINE reg_siefore RECORD
        siefore         CHAR(8),
        precio          DECIMAL(11,6),
        fecha           DATE
    END RECORD

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT 

    LET HOY = TODAY

    CALL inicio() #i

END MAIN

FUNCTION inicio()
#i---------------

    WHENEVER ERROR CONTINUE
        OPTIONS PROMPT LINE LAST
        OPEN WINDOW v1 AT 2,2 WITH FORM "ACRB0071" ATTRIBUTE(BORDER)
        DISPLAY " ACRB057     LIQUIDACION DEVOLUCION ANUALIDADES GARANTIZADAS                   " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

     MENU "LIQUIDA"
          COMMAND "Liquidar" "Liquidacion de devolucion de anualidades garantizadas"
              CALL liquida()
          COMMAND "Salir" "Salir del Programa"
              EXIT PROGRAM
     END MENU

END FUNCTION

FUNCTION liquida()
#l----------------

    SELECT *, user
    INTO   g_param_taa.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'

    LET tipo = 'T'

    INPUT BY NAME tipo,cuatro,fecha1,fecha2
        BEFORE FIELD tipo
            ERROR "Tipo liquidacion por acreditar: (T) = Traspaso Saldos "

        AFTER FIELD tipo
            DISPLAY "                      " AT 5,39

            IF tipo IS NULL THEN
                ERROR "Ingrese Tipo de Liquidacion: (T) Traspaso Saldos"
                NEXT FIELD tipo
            END IF

            LET tipo_desc = "Devolucion de saldos"

            DISPLAY tipo_desc CLIPPED AT 5,39 ATTRIBUTE(REVERSE)
	        ERROR "Marque con 'X' la Acreditacion a Liquidar"

            IF tipo = "T" THEN
                NEXT FIELD cuatro
            END IF

            AFTER FIELD cuatro
                IF cuatro = "X" THEN
                    CALL despliega_saldos(tipo,"ACR",fecha2)
                    CALL despliega_deposito(tipo,"ACR",fecha2) RETURNING sigue
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
                LET fecha2 = TODAY
                DISPLAY BY NAME fecha2

            AFTER FIELD fecha2
                IF fecha2 IS NULL THEN
                    ERROR "INGRESE FECHA DE LIQUIDACION"
                    NEXT FIELD fecha2
                END IF

            ON KEY ( ESC )
                LET salir = FALSE

                IF fecha1 IS NULL THEN
                    ERROR "INGRESE FECHA DE PROCESO"
                    NEXT FIELD fecha1
                END IF

                IF fecha2 IS NULL THEN
                    ERROR "INGRESE FECHA DE LIQUIDACION"
                    NEXT FIELD fecha2
                END IF

                IF cuatro = "X" THEN
                    CALL despliega_deposito(tipo,"ACR",fecha2) RETURNING sigue

                    IF NOT sigue THEN
                        NEXT FIELD cuatro
                    END IF
                END IF

                EXIT INPUT

            ON KEY ( INTERRUPT )
                LET salir = TRUE
                EXIT INPUT

        END INPUT

        IF salir THEN
            RETURN
        END IF

        PROMPT "DESEA GENERAR EL PROCESO [S/N] ? " FOR aux_pausa

        IF aux_pausa NOT MATCHES "[Ss]" THEN
            CALL Inicializa()

            RETURN
        END IF

        ERROR "PROCESANDO INFORMACION .... ESPERE UN MOMENTO" ATTRIBUTE(BOLD)

        FOR i= 1 TO arr_c
           IF g_sal[i].seleccion = "X" THEN
               CALL Proceso_principal(g_sal[i].folio)
               CALL actualiza_estados(g_sal[i].folio)
               CALL rehabilita_cuenta(g_sal[i].folio)
           END IF
        END FOR

        PROMPT "PROCESO FINALIZO NORMALMENTE. PRESIONE [Enter] PARA SALIR"
        FOR aux_pausa

END FUNCTION

FUNCTION inicia_saldos()
#is---------------------

    INITIALIZE g_sal TO NULL

    FOR i = 1 TO 50
        DISPLAY g_sal[i].* TO scr_liquida[i].*
    END FOR

END FUNCTION

FUNCTION despliega_saldos(tipo,aporte,fecha_liquida)
#ds-------------------------------------------------

    DEFINE tipo          CHAR(1)
    DEFINE aporte        CHAR(3)
    DEFINE fecha_liquida DATE
    DEFINE vestado       SMALLINT

    OPEN WINDOW wsaldo AT 9,3 WITH FORM "ACRB0072" ATTRIBUTE (BORDER)
    DISPLAY "TRASPASO DE SALDOS PENDIENTES POR LIQUIDAR" AT 1,15
    DISPLAY aporte AT 1,55
    DISPLAY "-----------------------------------------------------------------------------" AT 2,1

    CALL inicia_saldos()

    LET tipo_liquida     = " "
    LET tipo_comision    = " "
    LET importe_liquida  = 0
    LET importe_interes  = 0
    LET importe_total    = 0
    LET total_accion     = 0

    IF tipo = "T" AND aporte = "ACR" THEN
        LET tipo_liquida  = "58"
    END IF
 
    DECLARE cur_saldo CURSOR FOR
    SELECT  folio,fecha_archivo,impt_aport_acept,impt_inter_acept
    FROM    acr_devol_ag
    WHERE   ident_pago[14,15] = tipo_liquida
    AND     estado in(1,2)
    ORDER BY 1,2

    LET totala = 0
    LET totalc = 0
    LET i = 1

    FOREACH cur_saldo INTO g_sal[i].*
        LET i = i + 1
    END FOREACH

    CALL SET_COUNT(i-1)

    INPUT ARRAY g_sal WITHOUT DEFAULTS FROM scr_liquida.*
        BEFORE FIELD seleccion
            LET arr_c = ARR_CURR()
            LET arr_l = SCR_LINE()

        AFTER FIELD seleccion
            IF g_sal[arr_c].aportacion IS NULL THEN
                LET g_sal[arr_c].aportacion = 0
            END IF

            IF g_sal[arr_c].interes IS NULL THEN
                LET g_sal[arr_c].interes = 0
            END IF

         SELECT a.estado
           INTO vestado
           FROM acr_devol_ag a
          WHERE a.folio = g_sal[arr_c].folio

        IF vestado <> 2 THEN
            ERROR "FOLIO NO PROVISIONADO"
            LET vestado = 0
            LET g_sal[arr_c].seleccion = ''
            DISPLAY g_sal[arr_c].seleccion TO seleccion
            NEXT FIELD seleccion
        END IF

        ON KEY (ESC)
            FOR i = 1 to ARR_CURR()
                IF g_sal[i].seleccion = "X" THEN
                    LET totala = totala + g_sal[i].aportacion
                    LET totalc = ""
                END IF
            END FOR

         DISPLAY BY NAME totala,totalc 

         IF vestado <> 2 THEN
             ERROR "FOLIO NO PROVISIONADO O NO SELECCIONADO"
             LET vestado = 0
             LET g_sal[arr_c].seleccion = ''
             DISPLAY g_sal[arr_c].seleccion TO seleccion
             NEXT FIELD seleccion
         END IF

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

   CLOSE WINDOW wsaldo

END FUNCTION

FUNCTION Proceso_principal(vfolio)
#pp-------------------------------

    DEFINE
        x_reg RECORD LIKE dis_provision.*

    DEFINE
        text    CHAR(500),
        vfolio  INTEGER

    LET text = NULL

    IF tipo = "T" AND cuatro = "X" THEN                    # APORTE ACR
        LET text = "SELECT * FROM dis_provision WHERE folio = ",vfolio,
                   " AND subcuenta in(4,8) ",
                   " AND tipo_movimiento IN (1,4) ",
                   " ORDER BY nss"
    END IF

    PREPARE cur1 FROM text
    DECLARE cursor_1 CURSOR FOR cur1

    LET g_lista = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".AG"

    RUN g_lista

    START REPORT salida TO g_lista 
        FOREACH cursor_1 INTO x_reg.*
            OUTPUT TO REPORT salida(x_reg.*)
        END FOREACH

        UPDATE dis_provision
        SET    estado          = 6
        WHERE  folio           = x_reg.folio

    FINISH REPORT salida

END FUNCTION

FUNCTION despliega_deposito(tipo,aporte,fecha_liquida)
#dd---------------------------------------------------

     DEFINE tipo                CHAR(1)
     DEFINE aporte              CHAR(3)
     DEFINE fecha_liquida       DATE
     DEFINE total_vivienda      DECIMAL(16,6)

     DISPLAY aporte AT 7,39 ATTRIBUTE(BOLD)

       LET importe_liquida = totala
       LET importe_interes = totalc

       ERROR "CONFIRME MONTO TOTAL DE LIQUIDACION vs DEPOSITO BANCARIO"

       LET importe_total = importe_liquida + importe_interes
       LET total_accion  = 0
       LET valor_accion  = vprecio_accion

       DISPLAY BY NAME importe_liquida,
                       importe_interes,
                       importe_total,
                       valor_accion,
                       total_accion
       RETURN TRUE

END FUNCTION

FUNCTION actualiza_estados(vfolio)
#ae-------------------------------

    DEFINE vfolio INTEGER

    UPDATE acr_devol_ag
    SET    estado = 3                  # LIQUIDADO
    WHERE  folio = vfolio
    AND    ident_pago[14,15] = tipo_liquida
    AND    estado = 2

END FUNCTION

REPORT salida(x_reg)
#s------------------

    DEFINE x_reg   RECORD LIKE dis_provision.*
    DEFINE x_reg_1 RECORD LIKE dis_provision.*

    DEFINE fecha_anterior   DATE
    DEFINE fecha_actual     DATE
    DEFINE tot_peso         DECIMAL(16,2)
    DEFINE val_peso         DECIMAL(16,6)
    DEFINE val_accion       DECIMAL(16,6)
    DEFINE x_accion         RECORD LIKE glo_valor_accion.*
    DEFINE x_fecha          DATE
    DEFINE x_fecha_viv      DATE
    DEFINE valor_en_accion  DECIMAL(16,6)
    DEFINE cod_sief         CHAR(8)
    DEFINE mon_sief         DECIMAL(16,6)
    DEFINE v_banx           CHAR(10)
    DEFINE c_banx           CHAR(8)

    OUTPUT
        LEFT MARGIN 0
        FORMAT

        ON EVERY ROW
            SELECT precio_del_dia
            INTO x_reg.precio_accion
            FROM glo_valor_accion
            WHERE fecha_valuacion = x_reg.fecha_conversion
            AND codigo_siefore  = x_reg.siefore

            LET val_peso             = x_reg.monto_en_acciones
                                       * x_reg.precio_accion
            LET tot_peso             = val_peso
            LET x_reg.monto_en_pesos = tot_peso
            LET x_reg.fecha_proceso  = HOY

            INSERT INTO dis_cuenta VALUES (x_reg.*)
    END REPORT

FUNCTION Inicializa()
#i-------------------

     #LET tipo             = " "
     LET uno              = " "
     LET dos              = " "
     LET tres             = " "
     LET cuatro           = " "
     LET fecha1           = " "
     LET fecha2           = " "
     LET tipo_liquida     = 0
     LET tipo_comision    = 0
     LET importe_liquida  = 0
     LET importe_interes  = 0
     LET importe_total    = 0
     LET total_accion     = 0

     DISPLAY BY NAME tipo             ,
                     uno              ,
                     dos              ,
                     tres             ,
                     cuatro           ,
                     fecha1           ,
                     fecha2           ,
                     tipo_liquida     ,
                     tipo_comision    ,
                     importe_liquida  ,
                     importe_interes  ,
                     importe_total    ,
                     total_accion

END FUNCTION

FUNCTION rehabilita_cuenta(vfolio)
#rc-------------------------------

    DEFINE vfolio         INTEGER
    DEFINE nss_cur_dev    CHAR(11)
    DEFINE viv97_cur_dev  DECIMAL(22,14)
    DEFINE viv92_cur_dev  DECIMAL(22,14)
    DEFINE viv97_tot      DECIMAL(22,14)
    DEFINE viv92_tot      DECIMAL(22,14)
    DEFINE v_desmarca     CHAR(70)
    DEFINE r_corr RECORD  LIKE cta_act_marca.*
    DEFINE vprecio_accion DECIMAL(22,14)
    DEFINE vfecha_valor   DATE

    LET vfecha_valor = MDY(MONTH(TODAY),1,YEAR(TODAY))

    SELECT @precio_del_dia
    INTO   vprecio_accion
    FROM   glo_valor_accion
    WHERE  @fecha_valuacion = vfecha_valor
    AND    @codigo_siefore  = 11

    DECLARE cur_dev CURSOR FOR
    SELECT  n_seguro,aivs_v97,aivs_v92
    FROM    acr_det_dev_ag
    WHERE   folio = vfolio

    FOREACH cur_dev INTO nss_cur_dev,viv97_cur_dev,viv92_cur_dev
        INITIALIZE r_corr TO NULL

        SELECT a.*
        INTO   r_corr.*
        FROM   cta_act_marca a
        WHERE  a.nss =  nss_cur_dev
        AND    a.marca_cod = 140

        IF SQLCA.SQLCODE = 0 THEN
            INSERT INTO safre_af:cta_his_inhabilitada
            VALUES (r_corr.*)

            LET viv97_tot = viv97_cur_dev * vprecio_accion
            LET viv92_tot = viv92_cur_dev * vprecio_accion

            INSERT INTO safre_af:cta_rehabilitada VALUES
            (r_corr.nss,
             0,                --monto_retiro
             0,                --monto_cesantía
             0,                --monto_voluntaria
             viv97_tot,        --monto_vivienda97
             0,                --monto_cuota_soc
             0,                --monto_sar
             viv92_tot,        --monto_vivienda92
             HOY,              --fecha_rehabilitada
             r_corr.fecha_ini, --fecha_inhabilitada
             "",               --fecha_actualiza
             0,                --estado
             g_usuario         --usuario
             )

        END IF

        LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

        PREPARE eje_desmarca FROM v_desmarca

        EXECUTE eje_desmarca
        USING   r_corr.nss,
                r_corr.marca_cod,
                r_corr.correlativo,
                r_corr.estado_marca,
                r_corr.marca_causa,
                r_corr.usuario

    END FOREACH

END FUNCTION


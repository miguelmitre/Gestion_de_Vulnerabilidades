#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa TAAB003  => LIQUIDACION APORTACIONES / GENERACION SALDOS CTA.IND  #
#Fecha             => 31 DE ENERO DE 2001                                   #
#Autor             => MAURO MUNIZ CABALLERO.                                #
#Sistema           => TRA                                                   #
#Fecha Modifica    => 06 DE MARZO DE 2008                                   #
#Actualizacion     => JOSUE LISANDRO HUERTA SIERRA                          #
#                  => MULTISIEFORES CIRCULAR 69-2                           #
#REQ: #CPL-1331		 => FSR 03/07/2013
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_sal ARRAY[500] OF RECORD
        folio          INTEGER,
        fecha_archivo  DATE,
        aport_rcv      DECIMAL(16,2),
        aport_viv      DECIMAL(16,2),
        seleccion      CHAR(01)
    END RECORD

    DEFINE
        tipo_trasp    ,
        arr_c         ,
        arr_l         ,
        i             SMALLINT,
        totala        ,
        totalc        DECIMAL(16,2)

    DEFINE
        HOY     ,
        fecha1  ,
        fecha2  DATE

    DEFINE
        opc       ,
        aux_pausa ,
        tipo      ,
        cuatro    CHAR(1)

    DEFINE
        salir ,
        bnd_deposito,
        sigue     SMALLINT

    DEFINE
        tipo_desc CHAR(30),
        g_lista   CHAR(100),
        g_valor   CHAR(100)

    DEFINE
        importe_liquida ,
        importe_vivienda,
        importe_total   DECIMAL(16,6)

    DEFINE
        tipo_liquida    ,
        tipo_comision   CHAR(2),
        txt_cla         CHAR(500)

    DEFINE g_param_taa RECORD LIKE seg_modulo.*
    DEFINE reg_tmp RECORD LIKE safre_tmp:tmp_speic_06.*

    DEFINE g_usuario CHAR(8)

    DEFINE arr_precio_sie ARRAY[10] OF RECORD
         siefore_desc     CHAR(8),
         valor_accion     DECIMAL(11,6)
    END RECORD
    
    DEFINE
        txt_cla1        	CHAR(500),
        xcodigo_marca     SMALLINT ,
        xcodigo_rechazo   SMALLINT

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT 

    LET HOY = TODAY

    CALL STARTLOG('TAAB003.log')
    CALL ERRORLOG ("Versión :CPL-1331")
    CALL inicio()     #i
    CALL inicia_val() #iz
    CALL menu()       #m

END MAIN

FUNCTION menu()
#m-------------

    WHENEVER ERROR CONTINUE
        OPTIONS PROMPT LINE LAST

    OPEN WINDOW v1 AT 2,2 WITH FORM "TAAB0031" ATTRIBUTE(BORDER)

    DISPLAY " TAAB003     LIQUIDACION TRASPASOS AFORE RECIBIDOS                             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "LIQUIDA"
        COMMAND "Liquidar" "Liquidacion Traspaso Saldos a Cuenta Individual"
            CALL liquida()
        COMMAND "Salir" "Salir del Programa"
            EXIT PROGRAM
    END MENU

END FUNCTION

FUNCTION inicia_val()
#iz-----------------

    LET reg_tmp.pret = 0
    LET reg_tmp.pces = 0
    LET reg_tmp.ppat = 0
    LET reg_tmp.pv97 = 0
    LET reg_tmp.psoc = 0
    LET reg_tmp.pest = 0
    LET reg_tmp.psar = 0
    LET reg_tmp.pv92 = 0
    LET reg_tmp.pesp = 0
    LET reg_tmp.pven = 0
    LET reg_tmp.aret = 0
    LET reg_tmp.aces = 0
    LET reg_tmp.apat = 0
    LET reg_tmp.av97 = 0
    LET reg_tmp.asoc = 0
    LET reg_tmp.aest = 0
    LET reg_tmp.asar = 0
    LET reg_tmp.av92 = 0
    LET reg_tmp.aesp = 0
    LET reg_tmp.aven = 0

END FUNCTION

FUNCTION inicio()
#i---------------

    SELECT *, USER
    INTO   g_param_taa.*, g_usuario
    FROM   seg_modulo 
    WHERE  modulo_cod = 'taa'

    DELETE
    FROM   safre_tmp:tmp_speic_06
    WHERE  fecha_traspaso < HOY

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE tmp_rec

        CREATE TABLE tmp_rec
            (nss    CHAR(11) ,
             sub    SMALLINT ,
             pesos  DECIMAL(18,6),
             acc    DECIMAL(18,6))

        DATABASE safre_af
    WHENEVER ERROR STOP

    LET bnd_deposito = FALSE

END FUNCTION

FUNCTION liquida()
#l----------------

    INPUT BY NAME tipo,cuatro,fecha1,fecha2

        BEFORE FIELD tipo
            ERROR "Tipo liquidacion por acreditar: (T) = Traspaso Saldos "
            LET tipo = 'T'

        AFTER FIELD tipo
            DISPLAY "                      " AT 5,39
            IF tipo IS NULL THEN
                ERROR "Ingrese Tipo de Liquidacion: (T) Traspaso Saldos"
                NEXT FIELD tipo
            ELSE
                IF tipo = "T" THEN
                    LET tipo_desc = "Traspaso de Saldos   "
                    DISPLAY tipo_desc CLIPPED AT 5,39 ATTRIBUTE(REVERSE)
                    ERROR "Marque con 'X' la Acreditacion a Liquidar" 
                    NEXT FIELD cuatro
                END IF
            END IF

            AFTER FIELD cuatro
                IF cuatro IS NULL THEN
                    ERROR "SELECCIONE UNA OPCION CORRECTA"
                    SLEEP 3
                    NEXT FIELD cuatro
                ELSE
                    IF cuatro = "X" THEN
                        CALL despliega_saldos(tipo,"TRA",fecha2) RETURNING sigue

                        IF sigue THEN
                            CALL despliega_deposito(tipo,"TRA",fecha2) RETURNING sigue
                        ELSE
                            LET importe_liquida  = 0
                            LET importe_vivienda = 0
                            LET importe_total    = 0

                            DISPLAY BY NAME importe_liquida,
                                            importe_vivienda,
                                            importe_total
                            NEXT FIELD cuatro
                        END IF

                        NEXT FIELD fecha1
                    END IF
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
                ELSE
                    CALL despliega_precio_sie(fecha2) RETURNING sigue
                    IF sigue = 1 THEN
                        LET salir = FALSE
                        EXIT INPUT
                    ELSE
                        IF sigue = 2 THEN
                            ERROR "NO EXISTEN PRECIOS DE SIEFORE PARA LA FECHA DE LIQUIDACION INGRESADA"
                            NEXT FIELD fecha2
                         ELSE
                            LET salir = TRUE
                            EXIT INPUT
                         END IF
                    END IF
                END IF

            ON KEY ( INTERRUPT )
                LET salir = TRUE
                EXIT INPUT

        END INPUT

        IF salir THEN
            CALL inicia_val()
            CALL inicializa()
            CLEAR FORM
            RETURN
        END IF

        WHILE TRUE
            ERROR""
            PROMPT "¿ DESEA GENERAR EL PROCESO [S/N] ? " FOR aux_pausa
            IF aux_pausa <> "S" AND
               aux_pausa <> "s" AND
               aux_pausa <> "n" AND
               aux_pausa <> "N" THEN
                ERROR "SOLO INDIQUE S o N "
                SLEEP 3
                ERROR ""
                CONTINUE WHILE
            ELSE
                IF aux_pausa = "S" OR
                   aux_pausa = "s" THEN
                   EXIT WHILE
                ELSE
                   IF aux_pausa = "N" OR
                   aux_pausa = "n" THEN
                       ERROR "PROCESO CANCELADO."
                       SLEEP 3
                       ERROR ""
                       CALL Inicializa()
                       RETURN
                   END IF
                END IF
            END IF
        END WHILE

        ERROR "PROCESANDO INFORMACION .... ESPERE UN MOMENTO" ATTRIBUTE(BOLD)

        FOR i= 1 TO arr_c
            IF g_sal[i].seleccion = "X" THEN
                UPDATE taa_ctr_traspaso
                   SET ini_liquida = CURRENT,
                       usr_liquida = g_usuario
                 WHERE folio = g_sal[i].folio

                CALL Proceso_principal(g_sal[i].folio)
                CALL actualiza_estados(g_sal[i].folio)

                WHENEVER ERROR CONTINUE
                    DELETE
                    FROM   safre_tmp:tmp_folio_liq
                    WHERE  safre_tmp:tmp_folio_liq.fecha_liq <> hoy2

                    DELETE
                    FROM   safre_tmp:tmp_folio_liq
                    WHERE  safre_tmp:tmp_folio_liq.folio = g_sal[i].folio

                    INSERT INTO safre_tmp:tmp_folio_liq
                    VALUES (g_sal[i].folio, tipo_trasp, fecha2)
                WHENEVER ERROR STOP

                IF tipo_trasp = 3 THEN
                    CALL llena_speic(g_sal[i].folio)
                END IF

                UPDATE taa_ctr_traspaso
                   SET fin_liquida = CURRENT
                 WHERE folio = g_sal[i].folio
            END IF
        END FOR

        PROMPT "PROCESO FINALIZO NORMALMENTE. Presione [Enter] para salir"
        FOR aux_pausa
        ERROR ""
        CALL Inicializa()
        RETURN

END FUNCTION

FUNCTION inicia_saldos()
#is---------------------

    INITIALIZE g_sal TO NULL

    ---FOR i = 1 TO 50
        ---DISPLAY g_sal[i].* TO scr_liquida[i].*
    ---END FOR

END FUNCTION

FUNCTION despliega_saldos(tipo,aporte,fecha_liquida)
#ds-------------------------------------------------

   DEFINE tipo          CHAR(1)
   DEFINE aporte        CHAR(3)
   DEFINE fecha_liquida DATE
   DEFINE vestado       SMALLINT
   DEFINE bnd_liquida   SMALLINT

   OPEN WINDOW wsaldo AT 9,3 WITH FORM "TAAB0032" ATTRIBUTE (BORDER)
   DISPLAY "TRASPASO DE SALDOS PENDIENTES POR LIQUIDAR" AT 1,15
   DISPLAY aporte AT 1,58
   DISPLAY "-----------------------------------------------------------------------------" AT 2,1

   CALL inicia_saldos()

   LET tipo_liquida     = " "
   LET tipo_comision    = " "
   LET importe_liquida  = 0
   LET importe_vivienda = 0
   LET importe_total    = 0

   IF tipo = "T" AND aporte = "TRA" THEN
           LET tipo_liquida  = "57"
   END IF
 
   DECLARE cur_saldo CURSOR FOR
   SELECT  folio,fecha_archivo
   FROM    taa_recepcion_af
   WHERE   ident_pago[14,15] = tipo_liquida
   AND     estado in(1,2)
   ORDER BY 1,2

   LET totala = 0
   LET totalc = 0
   LET i = 1

   FOREACH cur_saldo INTO g_sal[i].folio, g_sal[i].fecha_archivo
       SELECT sum(p.monto_en_pesos)
         INTO g_sal[i].aport_rcv
         FROM dis_provision p
        WHERE p.folio = g_sal[i].folio
          AND p.subcuenta NOT IN (4,8,14,35)

       SELECT sum(p.monto_en_pesos)
         INTO g_sal[i].aport_viv
         FROM dis_provision p
        WHERE p.folio = g_sal[i].folio
          AND p.subcuenta IN (4,8,14,35)

      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   INPUT ARRAY g_sal WITHOUT DEFAULTS FROM scr_liquida.*
      BEFORE FIELD seleccion
         LET arr_c = ARR_CURR()
         LET arr_l = SCR_LINE()

      AFTER FIELD seleccion
         FOR i = 1 to ARR_CURR()
             IF g_sal[i].seleccion = 'X' OR
                g_sal[i].seleccion = 'x' OR
                g_sal[i].seleccion IS  NULL THEN
                 CONTINUE FOR
             ELSE
                 ERROR "OPCION NO VALIDA, INGRESE SOLO 'X'"
                 SLEEP 3
                 ERROR ""
                 NEXT FIELD seleccion
             END IF
         END FOR

         IF g_sal[arr_c].aport_rcv IS NULL THEN
            LET g_sal[arr_c].aport_rcv = 0
         END IF

         IF g_sal[arr_c].aport_viv IS NULL THEN
            LET g_sal[arr_c].aport_viv = 0
         END IF

         SELECT a.estado
           INTO vestado
           FROM taa_recepcion_af a
          WHERE a.folio = g_sal[arr_c].folio

         IF vestado <> 2 THEN
             ERROR "FOLIO NO PROVISIONADO"
             LET vestado = 0
             LET g_sal[arr_c].seleccion = ''
             DISPLAY g_sal[arr_c].seleccion TO seleccion
             NEXT FIELD seleccion
         END IF

      ON KEY (ESC)
         LET bnd_liquida = FALSE
         FOR i = 1 to ARR_CURR()
             IF g_sal[i].seleccion = "X" THEN
                LET totala = totala + g_sal[i].aport_rcv
                LET totalc = totalc + g_sal[i].aport_viv
                LET bnd_liquida  = TRUE
             END IF
         END FOR

         IF bnd_liquida THEN
             DISPLAY BY NAME totala,totalc 

             IF vestado <> 2 THEN
                 ERROR "FOLIO NO PROVISIONADO O NO SELECCIONADO"
                 LET vestado = 0
                 LET g_sal[arr_c].seleccion = ''
                 DISPLAY g_sal[arr_c].seleccion TO seleccion
                 NEXT FIELD seleccion
             END IF

             WHILE TRUE
                LET bnd_deposito = FALSE
                ERROR""
                PROMPT "ES CORRECTA LA SUMA A LIQUIDAR [S/N] ?" FOR opc
                IF opc <> "S" AND
                   opc <> "s" AND
                   opc <> "n" AND
                   opc <> "N" THEN
                    ERROR "SOLO INDIQUE S o N "
                    SLEEP 3
                    ERROR ""
                    LET bnd_deposito = FALSE
                    CONTINUE WHILE
                ELSE
                    IF opc = "S" OR
                       opc = "s" THEN
                       LET bnd_deposito = TRUE
                       EXIT INPUT
                    ELSE
                       IF opc = "N" OR
                       opc = "n" THEN
                           LET bnd_deposito = FALSE
                           FOR i = 1 TO arr_c 
                               LET g_sal[i].seleccion = NULL
                               LET totala = 0
                               LET totalc = 0
                               DISPLAY  g_sal[i].seleccion TO scr_liquida[i].seleccion
                               DISPLAY BY NAME totala,totalc
                           END FOR
                           NEXT FIELD seleccion
                       END IF
                    END IF
                END IF
             END WHILE

         ELSE
             ERROR "FOLIO NO SELECCIONADO.."
             SLEEP 3
             ERROR ""

             FOR i = 1 TO arr_c 
                 LET g_sal[i].seleccion = NULL
                 LET totala = 0
                 LET totalc = 0
                 DISPLAY  g_sal[i].seleccion TO scr_liquida[i].seleccion
                 DISPLAY BY NAME totala,totalc 
             END FOR
             LET bnd_deposito = FALSE
             EXIT INPUT
         END IF

   END INPUT

   CLOSE WINDOW wsaldo

   IF bnd_deposito THEN
       RETURN TRUE
   ELSE
       RETURN FALSE
   END IF

END FUNCTION

FUNCTION Proceso_principal(vfolio)

    DEFINE
        x_reg  RECORD LIKE dis_provision.*

    DEFINE
        text   CHAR(500),
        vfolio INTEGER

    LET text = NULL

    SELECT t.tipo
    INTO   tipo_trasp
    FROM   taa_folio t
    WHERE  t.folio = vfolio

    IF tipo = "T" AND cuatro = "X" THEN                    # APORTE TRA
        LET text = "SELECT * FROM dis_provision WHERE folio = ",vfolio 
    END IF

    PREPARE cur1 FROM text
    DECLARE cursor_1 CURSOR FOR cur1

    LET g_lista = g_param_taa.ruta_exp CLIPPED, "/TAA"

    LET g_valor = 'chmod 777 ',g_lista

    RUN g_valor 

    START REPORT salida TO g_lista
        FOREACH cursor_1 INTO x_reg.*
            OUTPUT TO REPORT salida(x_reg.*)
        END FOREACH

        UPDATE dis_provision
        SET    estado = 6
        WHERE  folio  = x_reg.folio

    FINISH REPORT salida
END FUNCTION

FUNCTION despliega_deposito(tipo,aporte,fecha_liquida)
#dd---------------------------------------------------

    DEFINE tipo                CHAR(1)
    DEFINE aporte              CHAR(3)
    DEFINE fecha_liquida       DATE
    DEFINE total_viv           DECIMAL(22,6)
    DEFINE total_rcv           DECIMAL(22,6)
    DEFINE saldo_viv           DECIMAL(22,6)
    DEFINE saldo_rcv           DECIMAL(22,6)

    DISPLAY aporte AT 7,39 ATTRIBUTE(BOLD)

    LET total_viv = 0
    LET total_rcv = 0

    FOR i= 1 TO arr_c
        IF g_sal[i].seleccion = "X" THEN

            SELECT sum(p.monto_en_pesos)
              INTO saldo_rcv
              FROM dis_provision p
             WHERE p.folio = g_sal[i].folio
               AND p.subcuenta NOT IN (4,8,14,35)

            SELECT sum(p.monto_en_pesos)
              INTO saldo_viv
              FROM dis_provision p
             WHERE p.folio = g_sal[i].folio
               AND p.subcuenta IN (4,8,14,35)

            LET total_rcv = total_rcv + saldo_rcv
            LET total_viv = total_viv + saldo_viv
        END IF
    END FOR

    LET importe_liquida  = total_rcv
    LET importe_vivienda = total_viv
    LET importe_total = importe_liquida + importe_vivienda
    ERROR "CONFIRME MONTO TOTAL DE LIQUIDACION vs DEPOSITO BANCARIO"

    DISPLAY BY NAME importe_liquida,
                    importe_vivienda,
                    importe_total
    RETURN TRUE 

END FUNCTION

FUNCTION despliega_precio_sie(f_liq)

    DEFINE f_liq      DATE
    DEFINE sie_cod    SMALLINT

    DECLARE cur_precio_sie CURSOR FOR
     SELECT precio_del_dia, codigo_siefore
       FROM glo_valor_accion
      WHERE fecha_valuacion = f_liq
        AND codigo_siefore NOT IN (0)
      ORDER BY 2

    LET i = 1

    FOREACH cur_precio_sie INTO arr_precio_sie[i].valor_accion, sie_cod
        SELECT ts.razon_social
          INTO arr_precio_sie[i].siefore_desc
          FROM tab_siefore_local ts
         WHERE ts.codigo_siefore = sie_cod

        LET i = i + 1
    END FOREACH

    IF i = 1 THEN
        RETURN 2
    ELSE
        CALL SET_COUNT(i-1)
        ERROR "[ESC] - Aceptar          [Ctrl-C] Salir "
        DISPLAY ARRAY arr_precio_sie TO scr_precio.*
            ON KEY (ESC)
                RETURN 1

            ON KEY (INTERRUPT)
                RETURN 3

        END DISPLAY
    END IF
END FUNCTION

FUNCTION actualiza_estados(vfolio)
#ae-------------------------

   DEFINE vfolio        INTEGER

   UPDATE taa_recepcion_af
   SET    estado = 3,                  # LIQUIDADO
          fecha_liquidacion = fecha2   # FECHA VALOR ACCION 
   WHERE  folio = vfolio
   AND    ident_pago[14,15] in (tipo_liquida,tipo_comision)
   AND    estado = 2

END FUNCTION

REPORT salida(x_reg)
#s------------------

        DEFINE x_reg            RECORD LIKE dis_provision.*
        DEFINE x_reg_1          RECORD LIKE dis_provision.*

        DEFINE fecha_anterior   DATE
        DEFINE fecha_actual     DATE
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
        DEFINE v_nss            CHAR(11)
        DEFINE v_folio          INTEGER
        DEFINE vcurp            CHAR(18)
        DEFINE vfecha_red_bono  DATE

        OUTPUT 
        LEFT MARGIN 0
        FORMAT

        ############
        ON EVERY ROW
        ############

           LET x_fecha = fecha2
           LET v_banx  = fecha2
           LET c_banx  = v_banx[7,10],v_banx[1,2],v_banx[4,5]


           IF x_reg.subcuenta <> 14 AND  ##SUBCUENTAS RCV
              x_reg.subcuenta <> 35 AND
              x_reg.subcuenta <> 4  AND
              x_reg.subcuenta <> 8  THEN
               SELECT *
               INTO   x_accion.*
               FROM   glo_valor_accion
               WHERE  glo_valor_accion.fecha_valuacion = x_fecha
               AND    glo_valor_accion.codigo_siefore  = x_reg.siefore

               LET x_reg.precio_accion     = x_accion.precio_del_dia
               LET x_reg.fecha_conversion  = fecha2
           ELSE
               LET x_fecha_viv            = MDY(MONTH(fecha2),1,
                                                 YEAR(fecha2))
               LET x_reg.fecha_valor      = x_fecha_viv
               LET x_reg.fecha_conversion = fecha2

               SELECT *
               INTO   x_accion.*
               FROM   glo_valor_accion
               WHERE  glo_valor_accion.fecha_valuacion = x_reg.fecha_valor
               AND    glo_valor_accion.codigo_siefore  = x_reg.siefore

               LET x_reg.precio_accion     = x_accion.precio_del_dia
               LET valor_en_accion         = 0
               LET x_fecha_viv             = MDY(MONTH(fecha2),1,YEAR(fecha2))
               LET x_reg.fecha_valor       = x_fecha_viv
               LET x_reg.fecha_conversion  = fecha2
           END IF

           LET x_reg.estado = 5

           INSERT INTO dis_cuenta VALUES ( x_reg.* )

           IF x_reg.subcuenta = 3 OR
              x_reg.subcuenta = 10 THEN
          
          LET txt_cla = "EXECUTE PROCEDURE crea_saldo_vol(",
                          x_reg.folio,",",'"',x_reg.nss,'"',",",
                          x_reg.siefore,",",
                          x_reg.subcuenta,",",
                      '"',x_reg.fecha_valor,'"',",",
                      '"',x_reg.fecha_conversion,'"',",",
                          x_reg.monto_en_pesos,",",
                          x_reg.monto_en_acciones,",",
                      '"',x_reg.usuario,'"'," )"
          
          {FSR    LET txt_cla = "EXECUTE PROCEDURE crea_saldo_vol(",
                              x_reg.folio,",",
                              '"',x_reg.nss,'"',",",
                              x_reg.siefore,",",
                              x_reg.subcuenta,",",
                              '"',x_reg.fecha_valor,'"',",",
                              '"',x_reg.fecha_conversion,'"',",",
                              x_reg.monto_en_pesos,",",
                              x_reg.monto_en_acciones,",",
                      '"',x_reg.usuario,'"',",", 
                      x_reg.consecutivo_lote," )"
                      
               LET txt_cla = txt_cla CLIPPED

               PREPARE claexe FROM txt_cla
               EXECUTE claexe } # En espera de ser liberado
           END IF
           
           IF x_reg.subcuenta = 36 THEN
           
              LET vcurp           = NULL
              LET vfecha_red_bono = NULL
           
              SELECT n_unico, fecha_red_bono
                INTO vcurp, vfecha_red_bono
                FROM taa_viv_recepcion, afi_mae_afiliado
               WHERE folio             = x_reg.folio
                 AND nss               = x_reg.nss
                 AND nss               = n_seguro
                 AND fecha_mov_banxico = x_reg.fecha_conversion
                 AND importe_bono     <> 0
           
              LET txt_cla1 = "EXECUTE PROCEDURE fn_reg_bono_issste(",
                             '"', x_reg.nss, '",',
                             '"TAR",',
                             '"', vcurp, '",',
                             '"', vfecha_red_bono, '",',
                             '"', x_reg.fecha_conversion, '",',
                             x_reg.monto_en_acciones, ",",
                             x_reg.monto_en_pesos, ",",
                             '"', x_reg.usuario, '")'
                             
              LET txt_cla1 = txt_cla1
              
              PREPARE claexe1 FROM txt_cla1
              EXECUTE claexe1                                                            
                              
           END IF

           INSERT INTO safre_tmp:tmp_rec
           VALUES(x_reg.nss,
                  x_reg.subcuenta,
                  x_reg.monto_en_pesos,
                  x_reg.monto_en_acciones)

END REPORT

FUNCTION Inicializa()

     LET tipo             = " "
     LET cuatro           = " "
     LET fecha1           = " "
     LET fecha2           = " "
     LET importe_liquida  = 0
     LET importe_vivienda = 0
     LET importe_total    = 0
     INITIALIZE arr_precio_sie TO NULL

     DISPLAY BY NAME tipo             ,
                     cuatro           ,
                     fecha1           ,
                     fecha2           ,
                     importe_liquida  ,
                     importe_vivienda ,
                     importe_total

     CLEAR FORM

END FUNCTION

FUNCTION llena_speic(vfolio)
#ls-------------------------

    DEFINE vfolio  INTEGER
    DEFINE tot_liq DECIMAL(18,6)
    DEFINE cta RECORD LIKE cta_ctr_cuenta.*

    DEFINE reg_tmp RECORD
        nss            CHAR(11),
        afore          CHAR(3),
        tipo_traspaso  SMALLINT,
        fecha_traspaso DATE,
        id_garan			 CHAR(1),		#CPL-1331	 
        dias_cotiz		 SMALLINT,		#CPL-1331
        fecha_mov_banxico DATE
    END RECORD

    DEFINE vmarca         CHAR(110)
    DEFINE xnss           CHAR(11)
    DEFINE xmarca         SMALLINT
    DEFINE xrechazo       SMALLINT
    DEFINE vmarca_uni     SMALLINT
    DEFINE vcorrelativo   SMALLINT
    DEFINE vestado_causa  SMALLINT
    DEFINE vrechazo_cod   SMALLINT
    DEFINE vmarca_causa   SMALLINT
    DEFINE vfecha_causa   DATE,
    			 vmarca_estado  SMALLINT, 
    			vcodigo_rechazo SMALLINT
    			    
    #marca de cuentas
    DEFINE edo_proc          SMALLINT ,
    			 edo_mod           SMALLINT ,
        	 edo_acr           SMALLINT 

    INITIALIZE reg_tmp TO NULL #CPL-1331
    
    LET tot_liq = 0
    LET edo_proc        = 237 #CPL-1331
    LET edo_acr         = 230 #CPL-1331

    LET reg_tmp.fecha_traspaso = fecha2
    DECLARE cur_rec CURSOR FOR
    SELECT r.nss, r.cve_ced_cuenta, r.tipo_traspaso, r.ident_garantia,r.dias_pag_cuo_soc, fecha_mov_banxico
    FROM   taa_viv_recepcion r
    WHERE  r.folio = vfolio

    FOREACH cur_rec INTO reg_tmp.nss, reg_tmp.afore, reg_tmp.tipo_traspaso,
    reg_tmp.id_garan, reg_tmp.dias_cotiz, reg_tmp.fecha_mov_banxico #CPL-1331
        IF (reg_tmp.tipo_traspaso = 12 OR 
            reg_tmp.tipo_traspaso = 20) THEN

           LET vmarca_uni = ""

           SELECT "X"
           FROM   uni_unificado
           WHERE  nss_uni = reg_tmp.nss
           GROUP BY 1

           IF STATUS <> NOTFOUND THEN
              LET vmarca_uni = 243
           ELSE
              LET vmarca_uni = 244
           END IF

           LET vmarca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
           PREPARE marcaje FROM vmarca
           LET xmarca        = 0
           LET xrechazo      = 0
           LET vrechazo_cod  = 0
           LET vcorrelativo  = 0
           LET vmarca_causa  = 0
           LET vestado_causa = 0
           LET vfecha_causa  = ""
           DECLARE cur_mar CURSOR FOR marcaje

           OPEN  cur_mar USING
              reg_tmp.nss,      #pnss
              vmarca_uni,       #marca_entra
              vcorrelativo,     #correlativo
              vestado_causa,    #estado_marca
              vrechazo_cod,     #codigo_rechazo
              vmarca_causa,     #marca_causa
              vfecha_causa,     #fecha_causa
              g_usuario         #usuario

           FETCH cur_mar INTO xmarca,
                              xrechazo
           CLOSE cur_mar

        END IF
### CPL-1331		
	LET vmarca_estado   = 0
	LET vcodigo_rechazo = 0
	
	   SELECT b.*
     INTO   cta.*
     FROM   cta_ctr_cuenta b
     WHERE  b.nss = reg_tmp.nss  

     IF SQLCA.SQLCODE = 0 THEN
     		LET cta.fecha_registro = reg_tmp.fecha_mov_banxico
       INSERT INTO cta_his_cuenta VALUES (cta.*)
      END IF

			UPDATE cta_ctr_cuenta 
			SET dias_cotizados     = reg_tmp.dias_cotiz
			WHERE  nss             = reg_tmp.nss    
			
			IF reg_tmp.id_garan = '1' THEN
            SELECT "X"
            FROM   cta_act_marca
            WHERE  marca_cod = edo_proc
            AND    nss       = reg_tmp.nss
            GROUP BY 1
						
            IF SQLCA.SQLCODE <> 0 THEN
                CALL marca_cuenta (reg_tmp.nss, edo_proc, vmarca_estado,
                                   vcodigo_rechazo, g_usuario, vfolio)
            END IF
        END IF

        IF reg_tmp.id_garan = '2' THEN
            SELECT "X"
            FROM   cta_act_marca
            WHERE  marca_cod = edo_acr
            AND    nss       = reg_tmp.nss
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
            	
                CALL marca_cuenta (reg_tmp.nss, edo_acr, vmarca_estado,
                                   vcodigo_rechazo, g_usuario, vfolio)
            END IF
        END IF

        LET reg_tmp.id_garan = '0'

#############################################################        

        SELECT 'X'
          FROM dis_cuenta
         WHERE @folio = vfolio
           AND @nss = reg_tmp.nss
        GROUP BY 1

        IF SQLCA.SQLCODE = 0 THEN
             UPDATE cta_ctr_cuenta
             SET    fecha_pri_general = reg_tmp.fecha_traspaso,
                    fecha_ult_general = reg_tmp.fecha_traspaso
            WHERE   nss               = reg_tmp.nss
        END IF

    END FOREACH

END FUNCTION

FUNCTION marca_cuenta(vnss, vmarca_entra, vmarca_edo, vcodigo_rech,
                      vusuario, vcorrelativo)
#mc---------------------

    DEFINE
        vnss         CHAR(11),
        vmarca_entra SMALLINT,
        vmarca_edo   SMALLINT,
        vcodigo_rech SMALLINT,
        vusuario     CHAR(08),
        vcorrelativo INTEGER,
        vmarca_causa SMALLINT,
        vfecha_causa DATE,
        pmarca_causa SMALLINT,
        ejecuta      CHAR(300)

    LET vmarca_causa = 0
    LET vfecha_causa = ""

---- 168 ---->
          LET pmarca_causa = 230
          LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
          "'",vnss,"'",
          ",",vmarca_entra,
          ",",vcorrelativo,
          ",",vmarca_edo,
          ",",vcodigo_rech,
          ",",pmarca_causa,
          ",","'","'", ",",
          "'",vusuario,"'",")"
				
          LET ejecuta = ejecuta CLIPPED

          PREPARE clausula_spl_230 FROM ejecuta

          DECLARE cursor_marca_230 CURSOR FOR clausula_spl_230

          OPEN cursor_marca_230

          FETCH cursor_marca_230 INTO xcodigo_marca, xcodigo_rechazo

          CLOSE cursor_marca_230

---- 168 <----
{
---- 168 ---->
    PREPARE eje_marca FROM ejecuta

    DECLARE cur_marca CURSOR FOR eje_marca

    OPEN cur_marca

    USING vnss        ,
          vmarca_entra,
          vcorrelativo,
          vmarca_edo  ,
          vcodigo_rech,
          vmarca_causa,
          vfecha_causa,
          vusuario

    FETCH cur_marca
    INTO  xcodigo_marca, xcodigo_rechazo

    CLOSE cur_marca
    FREE  cur_marca
}
---- 168 <----
END FUNCTION
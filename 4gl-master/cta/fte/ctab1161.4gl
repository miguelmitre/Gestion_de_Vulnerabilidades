############################################################################
#Propietario       => E.F.P.                                               #
#Programa CTAB116  => GENERA ARCHIVO CUENTAS INDIVIDUALES SELECC SIEFORE   #
#Fecha creacion    => 24 DE AGOSTO DE 2005                                 #
#Sistema           => CTA                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_seg_modulo      RECORD LIKE seg_modulo.*
    DEFINE g_cta_reg         RECORD LIKE cta_regimen.*
    DEFINE fecha_proceso     DATE
    DEFINE vnom_arch         CHAR(200)  -- Nombre del reporte a emitirse
    DEFINE vnom_limpio       CHAR(200)  -- Nombre del reporte a emitirse

    DEFINE g_mensaje1           ,
           g_mensaje2           ,
           g_mensaje3           ,
           g_mensaje4           ,
           g_mensaje5           CHAR(100),
           g_mensaje6           CHAR(100),
           g_usuario            CHAR(8)  ,
           g_respuesta          CHAR(001),
           g_tipo               CHAR(01),
           enter                CHAR(01),
           comm                 CHAR(500)

    DEFINE g_fecha_corte        DATE
    DEFINE g_fecha_fin          DATE
    DEFINE g_fecha_envio        DATE
    DEFINE g_fecha_cambio       DATE

    DEFINE g_cancela            SMALLINT

    DEFINE HOY               DATE

    DEFINE
        g_ruta                CHAR(100) ,
        v_usuario             CHAR(010)

    DEFINE siefore_1          ,
           siefore_2          CHAR(08)

    DEFINE v_codigo_afore     SMALLINT

    DEFINE vcons_oper         INTEGER    -- Consecutivo por nss
    DEFINE vcmd               CHAR(1000)
    DEFINE p_fecha            DATE

    DEFINE reg_regimen        RECORD
           nss                CHAR(11),
           curp               CHAR(18),
           rfc                CHAR(13),
           tipo_solicitud     CHAR(01),
           indic56            SMALLINT,
           ind_transferencia  SMALLINT,
           subcuenta          CHAR(02),
           siefore_desc       CHAR(08)
    END RECORD

    DEFINE exe_ind     CHAR(100)

END GLOBALS

MAIN

    OPTIONS
        PROMPT LINE LAST

    LET p_fecha = ARG_VAL(1)

    CALL inicio()                   #i
    CALL lee_datos()                #ld
    CALL proceso_principal(g_tipo)  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY           = TODAY
    LET fecha_proceso = TODAY

    CREATE TEMP TABLE arh_tipo3
    (nss   CHAR(11))

    CREATE TEMP TABLE nss_ced3
    (nss   CHAR(11))

    SELECT A.codigo_afore ,
           USER
    INTO   v_codigo_afore ,
           v_usuario
    FROM   tab_afore_local A

    SELECT r.*
    INTO   g_seg_modulo.*
    FROM   seg_modulo r
    WHERE  r.modulo_cod = "cta"

    SELECT siefore_desc
    INTO   siefore_1
    FROM   tab_siefore
    WHERE  afore_cod = v_codigo_afore
    AND    siefore_cod = 1

    SELECT siefore_desc
    INTO   siefore_2
    FROM   tab_siefore
    WHERE  afore_cod = v_codigo_afore
    AND    siefore_cod = 2

    LET exe_ind = "EXECUTE PROCEDURE fn_edad_sin_act(?,?)"

    PREPARE spl_ind FROM exe_ind

END FUNCTION

FUNCTION lee_datos()
#ld-----------------

    OPEN WINDOW  w_menu AT 4,4 WITH FORM "CTAB1161" ATTRIBUTES(BORDER)

    DISPLAY "CTAB116      GENERA ARCHIVO OPERACION 81      <Ctrl-C> SALIR     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    LET g_mensaje1 = "INDIQUE LA FECHA DEL PERIODO:"
    LET g_mensaje2 = "INDIQUE EL TIPO DE ARCHIVO A GENERAR:"
    LET g_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"
    LET g_mensaje4 = "INDIQUE LA RUTA PARA EL ARCHIVO:"
    LET g_mensaje5 = "INDIQUE LA FECHA DE ENVIO   :"
    LET g_mensaje6 = "AL"

    LET g_cancela = FALSE

    INPUT BY NAME g_tipo       ,
                  g_fecha_corte,
                  g_fecha_envio,
                  g_fecha_fin,
                  g_ruta       ,
                  g_respuesta

    BEFORE INPUT
        DISPLAY BY NAME g_mensaje2

    AFTER FIELD g_tipo
        IF g_tipo IS NULL OR
           g_tipo = " " THEN
            ERROR "Tipo de archivo no puede ser nulo"
            NEXT FIELD g_tipo
        END IF

        IF g_tipo <> 1 AND
           g_tipo <> 2 AND
           g_tipo <> 3 THEN
            ERROR "SOLO INDICAR 1, 2 o 3"
            SLEEP 3
            ERROR ""
            NEXT FIELD g_tipo
        ELSE
            IF g_tipo = 3 THEN
                LET g_mensaje1 = "INDIQUE FECHA INICIO PERIODO:"
            ELSE
                LET g_mensaje1 = "INDIQUE LA FECHA DE PERIODO :"
            END IF

            NEXT FIELD g_fecha_corte
        END IF

    BEFORE FIELD g_fecha_corte
        DISPLAY BY NAME g_mensaje1

        IF g_tipo <> 3  THEN
            IF MONTH(TODAY) = 12 THEN
                LET g_fecha_corte = MDY( 12,31,YEAR(TODAY) ) ;
            END IF

            IF MONTH(TODAY) = 1  OR
               MONTH(TODAY) = 2  THEN
                LET g_fecha_corte = MDY( 12,31,YEAR(TODAY) ) ;
                LET g_fecha_corte = g_fecha_corte -1 UNITS YEAR;
            ELSE
                LET g_fecha_corte = MDY( 6,30,YEAR(TODAY) ) ;
            END IF
        END IF

    AFTER FIELD g_fecha_corte
        IF g_fecha_corte IS NULL THEN
            IF g_tipo <> 3 THEN
                ERROR "DEBE INDICAR FECHA DE CORTE"
            ELSE
                ERROR "DEBE INDICAR FECHA INICIO PERIODO"
            END IF
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_corte
        ELSE
            IF g_tipo <> 3 THEN
                IF g_tipo = 1 THEN
                    IF fn_valida_proceso(g_fecha_corte) <> 0 THEN
                        NEXT FIELD g_fecha_corte
                    END IF
                END IF

                NEXT FIELD g_fecha_envio
            ELSE
                NEXT FIELD g_fecha_fin
            END IF
        END IF

    BEFORE FIELD g_fecha_fin
        DISPLAY BY NAME g_mensaje6

    AFTER FIELD g_fecha_fin
        IF g_fecha_fin IS NULL THEN
            ERROR "DEBE INDICAR FECHA FIN PERIODO"
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_fin
        END IF

        IF g_fecha_fin < g_fecha_corte THEN
            ERROR "FECHA FIN PERIODO DEBE SER POSTERIOR A FECHA INICIO PERIODO"
            SLEEP 3
            ERROR ""
            LET g_fecha_fin = ""
            DISPLAY BY NAME g_fecha_fin
            NEXT FIELD g_fecha_fin
        END IF

        NEXT FIELD g_fecha_envio

    BEFORE FIELD g_fecha_envio
        DISPLAY BY NAME g_mensaje5

        IF g_tipo = 1 THEN
            CALL suma_dias_habiles(g_fecha_corte, 1) RETURNING g_fecha_envio
        END IF

    AFTER FIELD g_fecha_envio
        IF g_fecha_envio IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE ENVIO"
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_envio
        END IF

        NEXT FIELD g_ruta

    BEFORE FIELD g_ruta
        LET g_ruta = g_seg_modulo.ruta_envio
        DISPLAY BY NAME g_ruta
        DISPLAY BY NAME g_mensaje4

    AFTER FIELD g_ruta
        IF valida_ruta ( g_ruta ) <> 0 THEN
            NEXT FIELD g_ruta
        END IF

    BEFORE FIELD g_respuesta
        DISPLAY BY NAME g_mensaje3

    AFTER FIELD g_respuesta
        IF g_respuesta <> "S" AND
           g_respuesta <> "s" AND
           g_respuesta <> "N" AND
            g_respuesta <> "n" THEN
            ERROR "SOLO INDIQUE S o N "
            SLEEP 3
            ERROR ""
            NEXT FIELD g_respuesta
        ELSE
            IF g_respuesta = "N" OR
               g_respuesta = "n" THEN
                ERROR "PROCESO CANCELADO."
                SLEEP 3
                ERROR ""
                LET g_tipo    = ""
                LET g_cancela = TRUE
                EXIT PROGRAM
            ELSE
                EXIT INPUT
            END IF
        END IF

    ON KEY (CONTROL-C,INTERRUPT)
        ERROR "PROCESO CANCELADO."
        SLEEP 3
        ERROR ""
        LET g_cancela = TRUE
        LET int_flag  = FALSE
        EXIT INPUT

    END INPUT

END FUNCTION

FUNCTION proceso_principal(p_tipo)
#pp-------------------------------

    DEFINE p_tipo             SMALLINT
    DEFINE v_no_transferencia SMALLINT
    DEFINE v_select           CHAR(4000)
    DEFINE bnd_sdo            SMALLINT

    IF p_tipo = 1 OR
       p_tipo = 3 THEN
        ---CALL genera_nss_cambio()  #gnc
    END IF

    IF p_tipo = 2 THEN
        LET v_select = "SELECT ",
                       " a.n_seguro,  ",
                       " a.n_unico,  ",
                       " a.n_rfc,  ",
                       " DECODE(b.tipo_solicitud,8,'2','0'),  ",
                       " c.ind_edad,",
                       " DECODE(c.ind_transferencia,1,'1','0'), ",
" DECODE(d.subcuenta,1,'01',11,'02',3,'03',15,'04',7,'07',13,'08',16,'09'),",
                       " e.siefore_desc ",
                       " FROM   afi_mae_afiliado a,",
                       " safre_tmp:cuota b,",
                       " cta_ctr_cuenta c,",
                       " cta_regimen d,",
                       " tab_siefore e ",
                       "WHERE  a.n_seguro    = b.n_seguro",
                       "  AND  b.n_seguro    = c.nss",
                       "  AND  b.tipo_solicitud <> 5",
                       "  AND  d.nss         = c.nss",
                       "  AND  d.subcuenta IN (1,3,7,11,13,15,16)",
                       "  AND  e.afore_cod   = ",v_codigo_afore,
                       "  AND  e.siefore_cod = d.codigo_siefore",
                       " ORDER BY 1,7 "

        LET vnom_arch = g_ruta CLIPPED,"/",v_usuario CLIPPED,".BDSELSIE.",
                        HOY USING "DDMMYYYY"
        LET vnom_limpio = v_usuario CLIPPED,".BDSELSIE.",HOY USING "DDMMYYYY"

        CALL genera_cuota( g_fecha_corte )
    END IF

    IF p_tipo = 1 THEN
        LET v_select = "SELECT ",
                       " a.n_seguro,  ",
                       " a.n_unico,  ",
                       " a.n_rfc,  ",
                       " DECODE(a.tipo_solicitud,5,'1','0'),  ",
                       " c.ind_edad,",
                       " DECODE(c.ind_transferencia,1,'1','0'), ",
" DECODE(d.subcuenta,1,'01',11,'02',3,'03',15,'04',7,'07',13,'08',16,'09'),",
                       " e.siefore_desc ",
                       " FROM   afi_mae_afiliado a,",
                       " cta_his_nss_decimo b,",
                       " cta_ctr_cuenta c,",
                       " cta_regimen d,",
                       " tab_siefore e ",
                       "WHERE  a.n_seguro    = b.nss",
                       "  AND  b.nss         = c.nss",
                       "  AND  d.nss         = c.nss",
                       "  AND  b.fecha_corte = '",g_fecha_corte,"'",
                       "  AND  b.proceso_cod   = 1 ",
                       "  AND  d.subcuenta IN (1,3,7,11,13,15,16)",
                       "  AND  e.afore_cod   = ",v_codigo_afore,
                       "  AND  e.siefore_cod = d.codigo_siefore",
               " UNION ",
                       " SELECT ",
                       " a.n_seguro,  ",
                       " a.n_unico,  ",
                       " a.n_rfc,  ",
                       " DECODE(a.tipo_solicitud,5,'1','0'),  ",
                       " c.ind_edad,",
                       " DECODE(c.ind_transferencia,1,'1','0'), ",
" DECODE(d.subcuenta,1,'01',11,'02',3,'03',15,'04',7,'07',13,'08',16,'09'),",
                       " e.siefore_desc ",
                       " FROM   afi_mae_afiliado a,",
                       " cta_ctr_cuenta c,",
                       " cta_regimen d,",
                       " tab_siefore e ",
                       "WHERE  a.n_seguro    = c.nss",
                       "  AND  c.nss         = d.nss",
                       "  AND  a.n_seguro IN(SELECT UNIQUE h.nss ",
                                            "  FROM arh_tipo3 h )",
                       "  AND  d.subcuenta IN (1,3,7,11,13,15,16)",
                       "  AND  e.afore_cod   = ",v_codigo_afore,
                       "  AND  e.siefore_cod = d.codigo_siefore",
                       " ORDER BY 1,7"

        LET vnom_arch = g_ruta CLIPPED,"/",v_usuario CLIPPED,".BDSELSIE-DEC.",
                        HOY USING "DDMMYYYY"

        LET vnom_limpio = v_usuario CLIPPED,".BDSELSIE-DEC.",HOY USING "DDMMYYYY"
    END IF

    IF p_tipo = 3 THEN
        LET v_select = "SELECT ",
                       " a.n_seguro,  ",
                       " a.n_unico,  ",
                       " a.n_rfc,  ",
                       " DECODE(a.tipo_solicitud,5,'1','0'),  ",
                       " c.ind_edad,",
                       " DECODE(c.ind_transferencia,1,'1','0'), ",
" DECODE(d.subcuenta,1,'01',11,'02',3,'03',15,'04',7,'07',13,'08',16,'09'),",
                       " e.siefore_desc ",
                       " FROM   afi_mae_afiliado a,",
                       " cta_ctr_cuenta c,",
                       " cta_regimen d,",
                       " tab_siefore e, ",
                       " safre_tmp:rechazo_op81 r ",
                       "WHERE  a.n_seguro    = c.nss",
                       "  AND  c.nss         = d.nss",
                       --"  AND  a.n_seguro IN(SELECT UNIQUE h.nss ",
                                            --"  FROM arh_tipo3 h )",
                       "  AND  d.subcuenta IN (1,3,7,11,13,15,16)",
                       "  AND  e.afore_cod   = ",v_codigo_afore,
                       "  AND  e.siefore_cod = d.codigo_siefore",
                       "  AND  c.nss         = r.nss",
                       " ORDER BY 1,7"

        LET vnom_arch = g_ruta CLIPPED,"/",v_usuario CLIPPED,".BDSELSIE-CMB.",
                        HOY USING "DDMMYYYY"

        LET vnom_limpio = v_usuario CLIPPED,".BDSELSIE-CMB.",HOY USING "DDMMYYYY"
    END IF

    ERROR "GENERANDO ARCHIVO..."

    -------------------------------------------------------------------

    -- Selecciona los NSS

    PREPARE eje_sel FROM v_select

    DECLARE cur_od CURSOR FOR eje_sel

    LET vcons_oper = 0   -- Consectivo por nss

    START REPORT rep_detalle TO vnom_arch

    FOREACH cur_od INTO reg_regimen.*
        IF reg_regimen.tipo_solicitud  = "1" THEN
            LET reg_regimen.indic56     = 2
            LET reg_regimen.rfc         = "             "
        END IF

        IF reg_regimen.ind_transferencia = 1 THEN
           IF reg_regimen.subcuenta <> "03" AND
              reg_regimen.subcuenta <> "05" THEN
              CALL verifica_saldo ( reg_regimen.nss, 2, g_fecha_corte )
                   RETURNING bnd_sdo

             IF bnd_sdo THEN
                LET reg_regimen.siefore_desc = siefore_2
             ELSE
                LET reg_regimen.siefore_desc = siefore_1
             END IF
           END IF
        END IF

        OUTPUT TO REPORT rep_detalle (reg_regimen.*)

    END FOREACH

    FINISH REPORT rep_detalle

    CALL limpia_nulos ()

    ERROR "ARCHIVO GENERADO : " , vnom_arch
    SLEEP 3

END FUNCTION

FUNCTION genera_nss_cambio()
#gnc------------------------

    DEFINE v_nss      CHAR(11)
    DEFINE v_existe   SMALLINT
    DEFINE v_edad     SMALLINT
    DEFINE v_criterio SMALLINT
    DEFINE v_sba      SMALLINT
    DEFINE v_sbc      SMALLINT

    INSERT INTO nss_ced3
    SELECT c.n_seguro
    FROM   taa_cd_det_cedido c
    WHERE  c.fecha_trasp <= g_fecha_fin
    AND    c.estado IN(103,12)

    INSERT INTO nss_ced3
    SELECT b.nss_cta1
    FROM   uni_unificado b
    WHERE  b.fnotifica <= g_fecha_fin
    AND    b.estado = 100

    CREATE INDEX nss_ced3_1 ON nss_ced3(nss)

    UPDATE STATISTICS FOR TABLE nss_ced3

    INSERT INTO arh_tipo3
    SELECT UNIQUE nss
    FROM   cta_his_regimen
    WHERE  nss IN(SELECT n_seguro
                    FROM afi_mae_afiliado
                   WHERE tipo_solicitud <> 5)
    AND    subcuenta > 0
    AND    factualiza BETWEEN g_fecha_corte AND g_fecha_fin
    AND    nss NOT IN(SELECT nss
                        FROM nss_ced3)

    INSERT INTO arh_tipo3
    SELECT n_seguro
    FROM   afi_mae_afiliado
    WHERE  finicta BETWEEN g_fecha_corte AND g_fecha_fin
    AND    tipo_solicitud <> 5
    AND    n_seguro NOT IN(SELECT nss
                           FROM   arh_tipo3)
    AND    n_seguro NOT IN(SELECT nss
                           FROM   nss_ced3)

    DECLARE cur_mod CURSOR FOR
    SELECT UNIQUE a.n_seguro
    FROM   afi_mae_modifica b, afi_mae_afiliado a
    WHERE  b.fecha_modifica BETWEEN g_fecha_corte AND g_fecha_fin
    AND    b.cod_operacion IN(1,3)
    AND    b.n_seguro NOT IN(SELECT nss
                             FROM arh_tipo3)
    AND    b.n_seguro NOT IN(SELECT nss
                             FROM nss_ced3)
    AND    b.n_seguro       = a.n_seguro
    AND    a.status_interno = 160

    FOREACH cur_mod INTO v_nss
        SELECT "X"
          FROM arh_tipo3 t
         WHERE t.nss = v_nss

        IF STATUS = NOTFOUND THEN
            DECLARE c_edad_ind CURSOR FOR spl_ind

            OPEN c_edad_ind USING v_nss,
                                  HOY

            FETCH c_edad_ind INTO v_existe, v_edad, v_criterio

            CLOSE c_edad_ind

            IF v_edad = 0 THEN
                LET v_sbc = 2
            ELSE
                LET v_sbc = 1
            END IF

            SELECT c.codigo_siefore
              INTO v_sba
              FROM cta_regimen c
             WHERE c.nss       = v_nss
               AND c.subcuenta = 1

            IF v_sba <> v_sbc THEN
                INSERT INTO arh_tipo3 VALUES (v_nss)
            END IF
        END IF
    END FOREACH

    CREATE INDEX arh_tipo3_1 ON arh_tipo3(nss)

    UPDATE STATISTICS FOR TABLE arh_tipo3

END FUNCTION

FUNCTION genera_cuota ( l_fecha_corte )
#gc------------------------------------

    DEFINE l_fecha_corte DATE
    DEFINE v_comando     CHAR(200)

    DATABASE safre_tmp
        LET v_comando = "EXECUTE PROCEDURE amafore('",l_fecha_corte,"')"

        PREPARE eje_cuota FROM v_comando
        ERROR "GENERANDO CUOTA DE MERCADO ..."
        EXECUTE eje_cuota
        ERROR "INFORMACION GENERADA"
        SLEEP 3
        ERROR ""
    DATABASE safre_af

END FUNCTION

FUNCTION valida_ruta ( l_ruta )
#vr----------------------------

    DEFINE l_ruta ,
           v_comando CHAR(100)

    DEFINE v_status  SMALLINT

    LET v_comando = "./valida_ruta.sh ", l_ruta CLIPPED

    RUN v_comando RETURNING v_status

    IF v_status <>  0 THEN
        ERROR "RUTA NO EXISTE O SIN PERMISOS"
        SLEEP 3
        ERROR ""
    END IF

    RETURN v_status

END FUNCTION

FUNCTION fn_valida_proceso( p_fecha_corte )
#fvp---------------------------------------

    DEFINE p_fecha_corte ,
           l_fecha_corte DATE
    DEFINE v_status SMALLINT

    LET v_status = 0

    SELECT fecha_corte
      INTO l_fecha_corte
      FROM cta_ctr_decimo
     WHERE fecha_corte = p_fecha_corte
       AND proceso_cod = 1

    IF l_fecha_corte IS NULL OR
       l_fecha_corte = "12/31/1899" THEN
        ERROR "NO EXISTE INFORMACION PARA ESA FECHA"
        SLEEP 3
        ERROR ""

        LET v_status = 1
    END IF

    RETURN v_status

END FUNCTION

REPORT rep_detalle(reg_regimen)
#rd----------------------------

    DEFINE reg_regimen        RECORD
           nss                CHAR(11),
           curp               CHAR(18),
           rfc                CHAR(13),
           tipo_solicitud     CHAR(01),
           indic56            SMALLINT,
           ind_transferencia  SMALLINT,
           subcuenta          CHAR(02),
           siefore_desc       CHAR(08)
    END RECORD

    OUTPUT
        PAGE   LENGTH  3
        LEFT   MARGIN  0
        RIGHT  MARGIN  0
        TOP    MARGIN  0
        BOTTOM MARGIN  0

    ORDER EXTERNAL BY reg_regimen.nss

    FORMAT
       FIRST PAGE HEADER
           PRINT
               COLUMN 001,"01",
               COLUMN 003,"03",
               COLUMN 005,"81",
               COLUMN 007,"01",
               COLUMN 009,v_codigo_afore USING "&&&",
               COLUMN 012,"03",
               COLUMN 014,"001",
               COLUMN 017,g_fecha_envio USING "YYYYMMDD",
               COLUMN 025,"001",
               COLUMN 028,"02",
               COLUMN 030,121 SPACES

       BEFORE GROUP OF reg_regimen.nss
           PRINT
               COLUMN 001,"02",
               COLUMN 003,"81",
               COLUMN 005,reg_regimen.nss,
               COLUMN 016,reg_regimen.curp,
               COLUMN 034,reg_regimen.rfc,
               COLUMN 047,reg_regimen.indic56 USING "&",
               COLUMN 048,reg_regimen.tipo_solicitud,
               COLUMN 049,reg_regimen.ind_transferencia USING "&",
               COLUMN 050,101 SPACES
       ON EVERY ROW
           PRINT
               COLUMN 001,"03",
               COLUMN 003,reg_regimen.nss,
               COLUMN 014,reg_regimen.curp,
               COLUMN 032,reg_regimen.subcuenta,
               COLUMN 034,reg_regimen.siefore_desc,
               COLUMN 042,109 SPACES
      AFTER GROUP OF reg_regimen.nss
           PRINT
               COLUMN 001,"08",
               COLUMN 003,"000000000000006",
               COLUMN 018,133 SPACES
        LET vcons_oper = vcons_oper + 1
      ON LAST ROW
           PRINT
               COLUMN 001,"09",
               COLUMN 003,"03",
               COLUMN 005,"81",
               COLUMN 007,"01",
               COLUMN 009,v_codigo_afore USING "&&&",
               COLUMN 012,"03",
               COLUMN 014,"001",
               COLUMN 017,g_fecha_envio USING "YYYYMMDD",
               COLUMN 025,vcons_oper USING "&&&&&&&&&&&&&&&",
               COLUMN 040, 111 SPACES

END REPORT

FUNCTION verifica_saldo ( f_nss, f_sie, f_crt )
#vs--------------------------------------------

    DEFINE f_nss    CHAR(11)
    DEFINE f_sie    SMALLINT
    DEFINE f_crt    DATE
    DEFINE vsdo_acc DECIMAL(22,6)
    DEFINE vsdo_pes DECIMAL(22,6)
    DEFINE bnd_sie  SMALLINT

    LET vsdo_acc = 0
    LET vsdo_pes = 0

    SELECT SUM(s.monto_en_acciones), SUM(s.monto_en_pesos)
      INTO vsdo_acc, vsdo_pes
      FROM dis_cuenta s
     WHERE s.nss = f_nss
       AND s.subcuenta NOT IN(3,10,4,8,14)
       AND s.siefore = f_sie
       AND s.fecha_conversion <= f_crt

    IF vsdo_acc <= 0 OR vsdo_acc IS NULL THEN
        LET vsdo_acc = 0
        LET bnd_sie   = FALSE
    ELSE
        LET bnd_sie   = TRUE
    END IF

    RETURN bnd_sie

END FUNCTION

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT


   LET diaHabilSig = diaActual

        WHILE TRUE
            LET feriado   = 0
            LET finSemana = 0
            LET diaSemana = WEEKDAY(diaHabilSig)

            IF diaSemana = 0 OR diaSemana = 6 THEN
                LET finSemana = 1
            END IF

            SELECT *
            FROM   tab_feriado
            WHERE  feria_fecha = diaHabilSig

            IF STATUS <> NOTFOUND THEN
                LET feriado = 1
            END IF

            IF feriado = 1 OR finSemana = 1 THEN
                LET diaHabilSig = diaHabilSig + 1 UNITS DAY
            ELSE
                EXIT WHILE
            END IF
        END WHILE

        RETURN diaHabilSig

END FUNCTION

FUNCTION suma_dias_habiles(diaActual, dia_hab)
#sdh------------------------------------------

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE,
          numDias       SMALLINT,
          dia_hab       SMALLINT

   LET diaTmp = diaActual

   FOR contador = 1 TO dia_hab
        IF contador = 1 THEN
            CALL habil_siguiente (diaTmp) RETURNING diaTmp
        ELSE
            LET diaTmp = diaTmp + 1 UNITS DAY
            CALL habil_siguiente (diaTmp) RETURNING diaTmp
        END IF
   END FOR

   RETURN diaTmp

END FUNCTION

FUNCTION limpia_nulos()
#ln--------------------

    LET comm = "cd " ,g_ruta CLIPPED,
               "/ ; sed -e '/^$/d' ",vnom_limpio CLIPPED,
               " > ",v_usuario CLIPPED,".arh_limpia"

    RUN comm

    LET comm = "cd " ,g_ruta CLIPPED,
               "/ ; mv ",v_usuario CLIPPED, ".arh_limpia ",vnom_limpio CLIPPED
    RUN comm

END FUNCTION


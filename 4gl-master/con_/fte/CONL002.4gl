##############################################################################
#Proyecto          => AFORE ( MEXICO )                                       #
#Propietario       => E.F.P.                                                 #
#Programa CONL002  => CARGA REGISTROS RETIROS TOTALES PARA REPORTE SAT       #
#Fecha creacion    => 23 DE FEBRERO DE 2010                                  #
#Autor             => MAURO MUNIZ CABALLERO                                  #
#Sistema           => CON                                                    #
#CPL-1859          => FSR se realizó cambio para considerar dis_cuenta, en   #
#                     caso de no existir split                               #
##############################################################################

DATABASE safre_af

GLOBALS

    DEFINE hoy             DATE
    DEFINE vfecha_ini      DATE
    DEFINE vfecha_fin      DATE
    DEFINE maxfecha_liq    DATE
    DEFINE vfprom_ini      DATE
    DEFINE vfprom_fin      DATE
    DEFINE pfecha_fin      DATE
    DEFINE vfini           DATE
    DEFINE vffin           DATE

    DEFINE vanio_10        SMALLINT
    DEFINE vanio_800       SMALLINT
    DEFINE vanio_ini       SMALLINT
    DEFINE vanio_fin       SMALLINT
    DEFINE vperiodo        SMALLINT
    DEFINE i               SMALLINT
    DEFINE j               SMALLINT

    DEFINE vgravable       DECIMAL(22,2)
    DEFINE vnogravable     DECIMAL(22,2)
    DEFINE vtotal          DECIMAL(22,2)
    DEFINE vsaldo_promedio DECIMAL(18,6)

    DEFINE enter           CHAR(1)
    DEFINE vanio_c         CHAR(2)
    DEFINE cfini           CHAR(10)
    DEFINE cffin           CHAR(10)
    DEFINE vnombre_tabla   CHAR(21)
    DEFINE eje_sdo         CHAR(300)
    DEFINE sel_his         CHAR(2000)

    DEFINE reg_retencion RECORD
        nss               CHAR(11),
        folio             INTEGER,
        consecutivo_lote  INTEGER,
        grupo_regimen     SMALLINT,
        fecha_conversion  DATE,
        monto_total       DECIMAL(18,2),
        monto_retiro      DECIMAL(18,2),
        monto_gravable    DECIMAL(18,2),
        monto_no_gravable DECIMAL(18,2),
        monto_retenido    DECIMAL(18,2),
        sdo_promedio1     DECIMAL(18,2),
        sdo_promedio2     DECIMAL(18,2),
        sdo_promedio3     DECIMAL(18,2),
        sdo_promedio4     DECIMAL(18,2),
        sdo_promedio5     DECIMAL(18,2),
        sdo_promedio6     DECIMAL(18,2),
        sdo_promedio7     DECIMAL(18,2),
        sdo_promedio8     DECIMAL(18,2),
        sdo_promedio9     DECIMAL(18,2),
        sdo_promedio10    DECIMAL(18,2),
        sdo_promedio11    DECIMAL(18,2),
        sdo_promedio12    DECIMAL(18,2)
    END RECORD

    DEFINE arr_mes ARRAY[12] OF RECORD
        sdo_mes             DECIMAL(18,6)
    END RECORD

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST

    CALL STARTLOG("CONL002.log")

    CALL inicio()
    CALL solicita_parametros()

END MAIN

FUNCTION inicio()
#i---------------

    LET hoy      = today
    LET vperiodo = YEAR(hoy) - 1

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_liquidacion
        DROP TABLE tmp_retencion
        DROP TABLE tmp_nss_ret_tx
        DROP TABLE tmp_subcta_isr
    WHENEVER ERROR STOP

    CREATE TABLE tmp_liquidacion
       (nss                   CHAR(11),
        folio                 INTEGER,
        consecutivo_lote      INTEGER,
        tipo_movimiento       SMALLINT,
        subcuenta             SMALLINT,
        siefore               SMALLINT,
        fecha_conversion      DATE,
        monto_en_pesos        DECIMAL(18,2))

    CREATE TABLE tmp_retencion
        (nss               CHAR(11),
         folio             INTEGER,
         consecutivo_lote  INTEGER,
         grupo_regimen     SMALLINT,
         fecha_conversion  DATE,
         monto_total       DECIMAL(18,2),
         monto_retiro      DECIMAL(18,2),
         monto_gravable    DECIMAL(18,2),
         monto_no_gravable DECIMAL(18,2),
         monto_retenido    DECIMAL(18,2),
         sdo_promedio1     DECIMAL(18,2),
         sdo_promedio2     DECIMAL(18,2),
         sdo_promedio3     DECIMAL(18,2),
         sdo_promedio4     DECIMAL(18,2),
         sdo_promedio5     DECIMAL(18,2),
         sdo_promedio6     DECIMAL(18,2),
         sdo_promedio7     DECIMAL(18,2),
         sdo_promedio8     DECIMAL(18,2),
         sdo_promedio9     DECIMAL(18,2),
         sdo_promedio10    DECIMAL(18,2),
         sdo_promedio11    DECIMAL(18,2),
         sdo_promedio12    DECIMAL(18,2))

    CREATE TABLE tmp_nss_ret_tx
         (nss          CHAR(11), 
          folio        INTEGER,
          consecutivo  DECIMAL(11,0))
    CREATE TABLE tmp_subcta_isr
         (subct_cod    SMALLINT)
    DATABASE safre_af

    INITIALIZE reg_retencion TO NULL

    LET eje_sdo = " EXECUTE PROCEDURE fn_saldo_dia_his (?,?,?,?) "

END FUNCTION

FUNCTION solicita_parametros()
#sp---------------------------

    OPEN WINDOW CONL0021 AT 2,2 WITH FORM "CONL0021" ATTRIBUTE(BORDER)
    DISPLAY "    <Enter> Confirmar Periodo     <Esc> Ejecutar Proceso     <Ctrl-C> Salir    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONL002        CARGA REGISTROS CON RETENCION PARA SAT                         " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    INPUT BY NAME vperiodo WITHOUT DEFAULTS

    AFTER FIELD vperiodo
        IF vperiodo < 2005 OR vperiodo > (YEAR(hoy)-1) THEN
            ERROR "Periodo fuera de rango"
            LET vperiodo = YEAR(hoy) - 1
        ELSE
            LET vfecha_ini = MDY(1,1,vperiodo)
            LET vfecha_fin = MDY(12,31,vperiodo)

            DISPLAY BY NAME vfecha_ini, vfecha_fin
        END IF

        ON KEY(ACCEPT)
            IF vfecha_ini IS NULL OR
               vfecha_ini = '12/31/1899' OR
               vfecha_fin IS NULL OR
               vfecha_fin = '12/31/1899' THEN
                PROMPT "NO HA CONFIRMADO PERIODO A PROCESAR, [Enter] PARA CONTINUAR" FOR ENTER
                NEXT FIELD vperiodo
            END IF

            CALL carga_registros()
            EXIT PROGRAM

        ON KEY(INTERRUPT)
            PROMPT "PROCESO CANCELADO [Enter] PARA SALIR" FOR enter
            EXIT PROGRAM

        EXIT INPUT

    END INPUT

END FUNCTION

FUNCTION carga_registros()
#cr-----------------------

    ERROR "PROCESANDO INFORMACION"
    INSERT INTO safre_tmp:tmp_nss_ret_tx
    SELECT UNIQUE t.nss,
           t.folio,
           t.consecutivo
      FROM safre_af:ret_solicitud_tx t
     WHERE t.estado_solicitud = 8
    --INTO  tmp_nss_ret_tx

---retiros issste
    INSERT INTO safre_tmp:tmp_nss_ret_tx
    SELECT UNIQUE t.nss,
           t.folio,
           t.consecutivo
      FROM safre_af:ret_sol_issste_tx t
     WHERE t.estado_solicitud = 80

---arc retiros voluntarias
    LET vanio_fin = YEAR(vfecha_fin)
    LET cfini     = "01/01/",vanio_fin CLIPPED
    LET vfini     = cfini
    LET cffin     = "12/31/",vanio_fin CLIPPED
    LET vffin     = cffin
    INSERT INTO safre_tmp:tmp_nss_ret_tx
    SELECT UNIQUE t.n_seguro,
           t.n_folio_liq,
           t.consecutivo
    FROM   safre_af:ret_cta_vol t
    WHERE  t.estado = 8
    AND    t.fecha_ult_ret BETWEEN vfini AND vffin
DATABASE safre_tmp
    CREATE INDEX tmp_nss_ret_tx1 ON tmp_nss_ret_tx(folio, nss)
    UPDATE STATISTICS FOR TABLE tmp_nss_ret_tx

    CREATE INDEX tmp_nss_ret_tx2 ON tmp_nss_ret_tx(consecutivo)
    UPDATE STATISTICS FOR TABLE tmp_nss_ret_tx
DATABASE safre_af
    INSERT INTO safre_tmp:tmp_subcta_isr
    SELECT s.subct_cod
      FROM safre_af:tab_subcuenta s,
           safre_af:tab_agrupa_subcta_regimen a
     WHERE s.subct_cod = a.subcuenta
       AND a.grupo_regimen IN(1,2,3,7,8,9,10)
    --INTO TEMP tmp_subcta_isr

    LET sel_his = ""

    LET vanio_ini = 1997
    LET vanio_fin = YEAR(vfecha_fin)

    --FOR vanio_10 = vanio_ini TO vanio_fin
        LET vanio_c = vfecha_ini USING "YY"

        LET vnombre_tabla = "dis_cuenta",vanio_c CLIPPED

        SELECT "X"
          FROM safre_af:systables
         WHERE tabname = vnombre_tabla

        IF SQLCA.SQLCODE = 0 THEN
           LET sel_his =            
                        " SELECT a.nss, a.folio, a.consecutivo_lote, a.tipo_movimiento, ",
                        "        a.subcuenta, a.siefore, a.fecha_conversion, a.monto_en_pesos ",
                        " FROM  ",vnombre_tabla ," a, ",
                        "       safre_tmp:tmp_nss_ret_tx b, ",
                        "       safre_tmp:tmp_subcta_isr c ",
                        " WHERE a.fecha_conversion between '", vfecha_ini,"' and '",vfecha_fin,"'",
                        " AND   a.folio = b.folio ",
                        " AND   a.nss = b.nss ",
                        " AND   a.subcuenta = c.subct_cod ",
                        " AND   a.tipo_movimiento = 10    ",
                        " AND   a.consecutivo_lote = b.consecutivo " 
                        --" UNION ALL "        	
           
           LET sel_his = " INSERT INTO safre_tmp:tmp_liquidacion ", sel_his CLIPPED
           
           PREPARE eje_prioridad FROM "SET PDQPRIORITY HIGH"
           EXECUTE eje_prioridad
           PREPARE eje_sel_10 FROM sel_his
           EXECUTE eje_sel_10                        
        END IF

    --END FOR

    LET sel_his = 
                " SELECT a.nss, a.folio, a.consecutivo_lote, a.tipo_movimiento, ",
                "        a.subcuenta, a.siefore, a.fecha_conversion, a.monto_en_pesos ",
                " FROM dis_cuenta a, ",
                "      safre_tmp:tmp_nss_ret_tx b, ",
                "      safre_tmp:tmp_subcta_isr c ",
                " WHERE a.fecha_conversion between '", vfecha_ini,"' and '",vfecha_fin,"'",
                " AND   a.folio = b.folio ",
                " AND   a.nss = b.nss ",
                " AND   a.subcuenta = c.subct_cod ",
                " AND   a.tipo_movimiento = 10    ",
                " AND   a.consecutivo_lote = b.consecutivo "

    LET sel_his = " INSERT INTO safre_tmp:tmp_liquidacion ", sel_his CLIPPED
    PREPARE eje_sel_11 FROM sel_his
    EXECUTE eje_sel_11

    LET sel_his = ""

    --FOR vanio_800 = vanio_ini TO vanio_fin
        LET vanio_c = vfecha_ini USING "YY"

        LET vnombre_tabla = "dis_cuenta",vanio_c CLIPPED

        SELECT "OK"
          FROM safre_af:systables
         WHERE tabname = vnombre_tabla

        IF SQLCA.SQLCODE = 0 THEN
        
            LET sel_his = sel_his CLIPPED,
                        " SELECT UNIQUE a.nss, a.folio, a.consecutivo_lote, a.tipo_movimiento, ",
                        "               a.subcuenta, a.siefore, a.fecha_conversion, a.monto_en_pesos ",
                        " FROM  ",vnombre_tabla ," a, ",
                        "       safre_tmp:tmp_liquidacion b ",
                        " WHERE a.fecha_conversion = b.fecha_conversion ",
                        " AND   a.folio = b.folio ",
                        " AND   a.nss = b.nss ",
                        " AND   a.subcuenta = b.subcuenta ",
   " AND   a.tipo_movimiento in(490,800,810,815,820,825,830,840,841,850,851,852,853,854,855,856,860,861,862,863,864,865,870,875,876,877,878,880,881,882,883,884,885,886,887,888,889,890,895) ",
                        " AND   a.consecutivo_lote = b.consecutivo_lote " 
                      --  " UNION ALL "
             LET sel_his = " INSERT INTO safre_tmp:tmp_liquidacion ", sel_his CLIPPED
             PREPARE eje_sel_800 FROM sel_his
             EXECUTE eje_sel_800                      
        END IF

    --END FOR

    LET sel_his = 
                " SELECT UNIQUE a.nss, a.folio, a.consecutivo_lote, a.tipo_movimiento, ",
                "               a.subcuenta, a.siefore, a.fecha_conversion, a.monto_en_pesos ",
                " FROM safre_af:dis_cuenta a, ",
                "      safre_tmp:tmp_liquidacion b ",
                " WHERE a.fecha_conversion = b.fecha_conversion ",
                " AND   a.folio = b.folio ",
                " AND   a.nss = b.nss ",
                " AND   a.subcuenta = b.subcuenta ",
   " AND   a.tipo_movimiento in(490,800,810,815,820,825,830,840,841,850,851,852,853,854,855,856,860,861,862,863,864,865,870,875,876,877,878,880,881,882,883,884,885,886,887,888,889,890,895) ",
                " AND   a.consecutivo_lote = b.consecutivo_lote "

    LET sel_his = " INSERT INTO safre_tmp:tmp_liquidacion ", sel_his CLIPPED
    PREPARE eje_sel_801 FROM sel_his
    EXECUTE eje_sel_801

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        CREATE INDEX tmp_liquidacion1 on tmp_liquidacion (nss, subcuenta, consecutivo_lote, fecha_conversion)
        UPDATE STATISTICS FOR TABLE tmp_liquidacion

        CREATE INDEX tmp_liquidacion2 on tmp_liquidacion (folio)
        UPDATE STATISTICS FOR TABLE tmp_liquidacion
    WHENEVER ERROR STOP

    DATABASE safre_af

    DECLARE cur_isr CURSOR FOR
    SELECT UNIQUE t.nss, t.folio, t.consecutivo_lote, t.fecha_conversion
      FROM safre_tmp:tmp_liquidacion t
    ORDER BY t.nss

    FOREACH cur_isr INTO reg_retencion.nss               ,
                         reg_retencion.folio             ,
                         reg_retencion.consecutivo_lote  ,
                         reg_retencion.fecha_conversion

        SELECT 1,
               l.fecha_conversion,
               sum(l.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retiro
          FROM safre_tmp:tmp_liquidacion l
         WHERE l.folio            = reg_retencion.folio
           AND l.nss              = reg_retencion.nss
           AND l.subcuenta        IN(1,2,5,6,9)
           AND l.consecutivo_lote = reg_retencion.consecutivo_lote
           AND l.fecha_conversion = reg_retencion.fecha_conversion
           AND l.tipo_movimiento  <> 10
        GROUP BY 1,2

        SELECT 1,
               r.fecha_conversion,
               sum(r.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retenido
          FROM safre_tmp:tmp_liquidacion r
         WHERE r.folio            = reg_retencion.folio
           AND r.nss              = reg_retencion.nss
           AND r.subcuenta        IN(1,2,5,6,9)
           AND r.consecutivo_lote = reg_retencion.consecutivo_lote
           AND r.fecha_conversion = reg_retencion.fecha_conversion
           AND r.tipo_movimiento  = 10
        GROUP BY 1,2

        IF reg_retencion.monto_retiro <> 0 OR
           reg_retencion.monto_retiro IS NOT NULL THEN
            LET reg_retencion.monto_total       = reg_retencion.monto_retiro + reg_retencion.monto_retenido
            LET reg_retencion.monto_gravable    = reg_retencion.monto_retenido * 5
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_total - reg_retencion.monto_gravable

            LET reg_retencion.monto_retiro      = reg_retencion.monto_retiro * -1
            LET reg_retencion.monto_retenido    = reg_retencion.monto_retenido * -1
            LET reg_retencion.monto_total       = reg_retencion.monto_total * -1
            LET reg_retencion.monto_gravable    = reg_retencion.monto_gravable * -1
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_no_gravable * -1

            IF reg_retencion.monto_no_gravable < 0 THEN
                LET reg_retencion.monto_no_gravable = 0
                LET reg_retencion.monto_gravable    = reg_retencion.monto_total
            END IF

            SELECT MAX(m.fecha_conversion)
              INTO maxfecha_liq
              FROM safre_tmp:tmp_liquidacion m
             WHERE m.nss = reg_retencion.nss
               AND m.subcuenta IN(1,2,5,6,9,3,10,11,12,13,7,17,18,22,23,24,25,30,31,32)

            CALL obtiene_fin_mes(maxfecha_liq) RETURNING pfecha_fin

            LET vfprom_ini = pfecha_fin + 1
            LET vfprom_ini = vfprom_ini - 1 UNITS YEAR

            CALL tmp_cta_his(reg_retencion.nss, 1, vfprom_ini, pfecha_fin)

            CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin

            LET i = 1
            WHILE vfprom_fin <= pfecha_fin
                CALL obtiene_saldo(reg_retencion.nss, vfprom_ini, vfprom_fin, 0, 1)
                     RETURNING vsaldo_promedio

                IF vsaldo_promedio IS NULL OR vsaldo_promedio < 0 THEN
                    LET vsaldo_promedio = 0
                END IF

                LET arr_mes[i].sdo_mes = vsaldo_promedio
                LET vsaldo_promedio    = 0

                LET vfprom_ini = vfprom_fin + 1

                LET i = i + 1

                CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin
            END WHILE

            LET reg_retencion.sdo_promedio1  = arr_mes[1].sdo_mes
            LET reg_retencion.sdo_promedio2  = arr_mes[2].sdo_mes
            LET reg_retencion.sdo_promedio3  = arr_mes[3].sdo_mes
            LET reg_retencion.sdo_promedio4  = arr_mes[4].sdo_mes
            LET reg_retencion.sdo_promedio5  = arr_mes[5].sdo_mes
            LET reg_retencion.sdo_promedio6  = arr_mes[6].sdo_mes
            LET reg_retencion.sdo_promedio7  = arr_mes[7].sdo_mes
            LET reg_retencion.sdo_promedio8  = arr_mes[8].sdo_mes
            LET reg_retencion.sdo_promedio9  = arr_mes[9].sdo_mes
            LET reg_retencion.sdo_promedio10 = arr_mes[10].sdo_mes
            LET reg_retencion.sdo_promedio11 = arr_mes[11].sdo_mes
            LET reg_retencion.sdo_promedio12 = arr_mes[12].sdo_mes

            INSERT INTO safre_tmp:tmp_retencion VALUES (reg_retencion.*)

        END IF

        LET reg_retencion.monto_retiro      = ''
        LET reg_retencion.monto_retenido    = ''
        LET reg_retencion.monto_total       = ''
        LET reg_retencion.monto_gravable    = ''
        LET reg_retencion.monto_no_gravable = ''

        SELECT 7,
               l.fecha_conversion,
               sum(l.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retiro
          FROM safre_tmp:tmp_liquidacion l
         WHERE l.folio            = reg_retencion.folio
           AND l.nss              = reg_retencion.nss
           AND l.subcuenta        = 7
           AND l.consecutivo_lote = reg_retencion.consecutivo_lote
           AND l.fecha_conversion = reg_retencion.fecha_conversion
           AND l.tipo_movimiento  <> 10
        GROUP BY 1,2

        SELECT 7,
               r.fecha_conversion,
               sum(r.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retenido
          FROM safre_tmp:tmp_liquidacion r
         WHERE r.folio            = reg_retencion.folio
           AND r.nss              = reg_retencion.nss
           AND r.subcuenta        = 7
           AND r.consecutivo_lote = reg_retencion.consecutivo_lote
           AND r.fecha_conversion = reg_retencion.fecha_conversion
           AND r.tipo_movimiento  = 10
        GROUP BY 1,2

        IF reg_retencion.monto_retiro <> 0 OR
           reg_retencion.monto_retiro IS NOT NULL THEN
            LET reg_retencion.monto_total       = reg_retencion.monto_retiro + reg_retencion.monto_retenido
            LET reg_retencion.monto_gravable    = reg_retencion.monto_retenido * 5
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_total - reg_retencion.monto_gravable

            LET reg_retencion.monto_retiro      = reg_retencion.monto_retiro * -1
            LET reg_retencion.monto_retenido    = reg_retencion.monto_retenido * -1
            LET reg_retencion.monto_total       = reg_retencion.monto_total * -1
            LET reg_retencion.monto_gravable    = reg_retencion.monto_gravable * -1
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_no_gravable * -1

            IF reg_retencion.monto_no_gravable < 0 THEN
                LET reg_retencion.monto_no_gravable = 0
                LET reg_retencion.monto_gravable    = reg_retencion.monto_total
            END IF

            SELECT MAX(m.fecha_conversion)
              INTO maxfecha_liq
              FROM safre_tmp:tmp_liquidacion m
             WHERE m.nss = reg_retencion.nss
               AND m.subcuenta = 7

            CALL obtiene_fin_mes(maxfecha_liq) RETURNING pfecha_fin

            LET vfprom_ini = MDY(MONTH(pfecha_fin) + 1, 1, YEAR(pfecha_fin) - 1)

            CALL tmp_cta_his(reg_retencion.nss, 7, vfprom_ini, pfecha_fin)

            CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin

            LET j = 1

            WHILE vfprom_fin <= pfecha_fin
                CALL obtiene_saldo(reg_retencion.nss, vfprom_ini, vfprom_fin, 0, 7)
                     RETURNING vsaldo_promedio

                IF vsaldo_promedio IS NULL OR
                   vsaldo_promedio < 0 THEN
                    LET vsaldo_promedio = 0
                END IF

                LET arr_mes[j].sdo_mes = vsaldo_promedio
                LET vsaldo_promedio    = 0

                LET vfprom_ini = vfprom_fin + 1

                LET j = j + 1

                CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin
            END WHILE

            LET reg_retencion.sdo_promedio1  = arr_mes[1].sdo_mes
            LET reg_retencion.sdo_promedio2  = arr_mes[2].sdo_mes
            LET reg_retencion.sdo_promedio3  = arr_mes[3].sdo_mes
            LET reg_retencion.sdo_promedio4  = arr_mes[4].sdo_mes
            LET reg_retencion.sdo_promedio5  = arr_mes[5].sdo_mes
            LET reg_retencion.sdo_promedio6  = arr_mes[6].sdo_mes
            LET reg_retencion.sdo_promedio7  = arr_mes[7].sdo_mes
            LET reg_retencion.sdo_promedio8  = arr_mes[8].sdo_mes
            LET reg_retencion.sdo_promedio9  = arr_mes[9].sdo_mes
            LET reg_retencion.sdo_promedio10 = arr_mes[10].sdo_mes
            LET reg_retencion.sdo_promedio11 = arr_mes[11].sdo_mes
            LET reg_retencion.sdo_promedio12 = arr_mes[12].sdo_mes

            INSERT INTO safre_tmp:tmp_retencion VALUES (reg_retencion.*)

        END IF

        LET reg_retencion.monto_retiro      = ''
        LET reg_retencion.monto_retenido    = ''
        LET reg_retencion.monto_total       = ''
        LET reg_retencion.monto_gravable    = ''
        LET reg_retencion.monto_no_gravable = ''

        SELECT 2,
               l.fecha_conversion,
               sum(l.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retiro
          FROM safre_tmp:tmp_liquidacion l
         WHERE l.folio            = reg_retencion.folio
           AND l.nss              = reg_retencion.nss
           AND l.subcuenta IN(11,12,24,25)
           AND l.consecutivo_lote = reg_retencion.consecutivo_lote
           AND l.fecha_conversion = reg_retencion.fecha_conversion
           AND l.tipo_movimiento  <> 10
        GROUP BY 1,2

        SELECT 2,
               r.fecha_conversion,
               sum(r.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retenido
          FROM safre_tmp:tmp_liquidacion r
         WHERE r.folio            = reg_retencion.folio
           AND r.nss              = reg_retencion.nss
           AND r.subcuenta IN(11,12,24,25)
           AND r.consecutivo_lote = reg_retencion.consecutivo_lote
           AND r.fecha_conversion = reg_retencion.fecha_conversion
           AND r.tipo_movimiento  = 10
        GROUP BY 1,2

        IF reg_retencion.monto_retiro <> 0 OR
           reg_retencion.monto_retiro IS NOT NULL THEN
            LET reg_retencion.monto_total       = reg_retencion.monto_retiro + reg_retencion.monto_retenido
            LET reg_retencion.monto_gravable    = reg_retencion.monto_retenido * 5
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_total - reg_retencion.monto_gravable

            LET reg_retencion.monto_retiro      = reg_retencion.monto_retiro * -1
            LET reg_retencion.monto_retenido    = reg_retencion.monto_retenido * -1
            LET reg_retencion.monto_total       = reg_retencion.monto_total * -1
            LET reg_retencion.monto_gravable    = reg_retencion.monto_gravable * -1
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_no_gravable * -1

            IF reg_retencion.monto_no_gravable < 0 THEN
                LET reg_retencion.monto_no_gravable = 0
                LET reg_retencion.monto_gravable    = reg_retencion.monto_total
            END IF

            SELECT MAX(m.fecha_conversion)
              INTO maxfecha_liq
              FROM safre_tmp:tmp_liquidacion m
             WHERE m.nss = reg_retencion.nss
               AND m.subcuenta IN(11,12,24,25)

            CALL obtiene_fin_mes(maxfecha_liq) RETURNING pfecha_fin

            LET vfprom_ini = MDY(MONTH(pfecha_fin) + 1, 1, YEAR(pfecha_fin) - 1)

            CALL tmp_cta_his(reg_retencion.nss, 7, vfprom_ini, pfecha_fin)

            CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin

            LET j = 1

            WHILE vfprom_fin <= pfecha_fin
                CALL obtiene_saldo(reg_retencion.nss, vfprom_ini, vfprom_fin, 0, 7)
                     RETURNING vsaldo_promedio

                IF vsaldo_promedio IS NULL OR
                   vsaldo_promedio < 0 THEN
                    LET vsaldo_promedio = 0
                END IF

                LET arr_mes[j].sdo_mes = vsaldo_promedio
                LET vsaldo_promedio    = 0

                LET vfprom_ini = vfprom_fin + 1

                LET j = j + 1

                CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin
            END WHILE

            LET reg_retencion.sdo_promedio1  = arr_mes[1].sdo_mes
            LET reg_retencion.sdo_promedio2  = arr_mes[2].sdo_mes
            LET reg_retencion.sdo_promedio3  = arr_mes[3].sdo_mes
            LET reg_retencion.sdo_promedio4  = arr_mes[4].sdo_mes
            LET reg_retencion.sdo_promedio5  = arr_mes[5].sdo_mes
            LET reg_retencion.sdo_promedio6  = arr_mes[6].sdo_mes
            LET reg_retencion.sdo_promedio7  = arr_mes[7].sdo_mes
            LET reg_retencion.sdo_promedio8  = arr_mes[8].sdo_mes
            LET reg_retencion.sdo_promedio9  = arr_mes[9].sdo_mes
            LET reg_retencion.sdo_promedio10 = arr_mes[10].sdo_mes
            LET reg_retencion.sdo_promedio11 = arr_mes[11].sdo_mes
            LET reg_retencion.sdo_promedio12 = arr_mes[12].sdo_mes

            INSERT INTO safre_tmp:tmp_retencion VALUES (reg_retencion.*)
        END IF

        LET reg_retencion.monto_retiro      = ''
        LET reg_retencion.monto_retenido    = ''
        LET reg_retencion.monto_total       = ''
        LET reg_retencion.monto_gravable    = ''
        LET reg_retencion.monto_no_gravable = ''

        SELECT 3,
               l.fecha_conversion,
               sum(l.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retiro
          FROM safre_tmp:tmp_liquidacion l
         WHERE l.folio            = reg_retencion.folio
           AND l.nss              = reg_retencion.nss
           AND l.subcuenta IN(3,10,22,23)
           AND l.consecutivo_lote = reg_retencion.consecutivo_lote
           AND l.fecha_conversion = reg_retencion.fecha_conversion
           AND l.tipo_movimiento  <> 10
        GROUP BY 1,2

        SELECT 3,
               r.fecha_conversion,
               sum(r.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retenido
          FROM safre_tmp:tmp_liquidacion r
         WHERE r.folio            = reg_retencion.folio
           AND r.nss              = reg_retencion.nss
           AND r.subcuenta IN(3,10,22,23)
           AND r.consecutivo_lote = reg_retencion.consecutivo_lote
           AND r.fecha_conversion = reg_retencion.fecha_conversion
           AND r.tipo_movimiento  = 10
        GROUP BY 1,2

        IF reg_retencion.monto_retiro <> 0 OR
           reg_retencion.monto_retiro IS NOT NULL THEN
            LET reg_retencion.monto_total       = reg_retencion.monto_retiro + reg_retencion.monto_retenido
            LET reg_retencion.monto_gravable    = reg_retencion.monto_retenido * 5
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_total - reg_retencion.monto_gravable

            LET reg_retencion.monto_retiro      = reg_retencion.monto_retiro * -1
            LET reg_retencion.monto_retenido    = reg_retencion.monto_retenido * -1
            LET reg_retencion.monto_total       = reg_retencion.monto_total * -1
            LET reg_retencion.monto_gravable    = reg_retencion.monto_gravable * -1
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_no_gravable * -1

            IF reg_retencion.monto_no_gravable < 0 THEN
                LET reg_retencion.monto_no_gravable = 0
                LET reg_retencion.monto_gravable    = reg_retencion.monto_total
            END IF

            SELECT MAX(m.fecha_conversion)
              INTO maxfecha_liq
              FROM safre_tmp:tmp_liquidacion m
             WHERE m.nss = reg_retencion.nss
               AND m.subcuenta IN(3,10,22,23)

            CALL obtiene_fin_mes(maxfecha_liq) RETURNING pfecha_fin

            LET vfprom_ini = MDY(MONTH(pfecha_fin) + 1, 1, YEAR(pfecha_fin) - 1)

            CALL tmp_cta_his(reg_retencion.nss, 7, vfprom_ini, pfecha_fin)

            CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin

            LET j = 1

            WHILE vfprom_fin <= pfecha_fin
                CALL obtiene_saldo(reg_retencion.nss, vfprom_ini, vfprom_fin, 0, 7)
                     RETURNING vsaldo_promedio

                IF vsaldo_promedio IS NULL OR
                   vsaldo_promedio < 0 THEN
                    LET vsaldo_promedio = 0
                END IF

                LET arr_mes[j].sdo_mes = vsaldo_promedio
                LET vsaldo_promedio    = 0

                LET vfprom_ini = vfprom_fin + 1

                LET j = j + 1

                CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin
            END WHILE

            LET reg_retencion.sdo_promedio1  = arr_mes[1].sdo_mes
            LET reg_retencion.sdo_promedio2  = arr_mes[2].sdo_mes
            LET reg_retencion.sdo_promedio3  = arr_mes[3].sdo_mes
            LET reg_retencion.sdo_promedio4  = arr_mes[4].sdo_mes
            LET reg_retencion.sdo_promedio5  = arr_mes[5].sdo_mes
            LET reg_retencion.sdo_promedio6  = arr_mes[6].sdo_mes
            LET reg_retencion.sdo_promedio7  = arr_mes[7].sdo_mes
            LET reg_retencion.sdo_promedio8  = arr_mes[8].sdo_mes
            LET reg_retencion.sdo_promedio9  = arr_mes[9].sdo_mes
            LET reg_retencion.sdo_promedio10 = arr_mes[10].sdo_mes
            LET reg_retencion.sdo_promedio11 = arr_mes[11].sdo_mes
            LET reg_retencion.sdo_promedio12 = arr_mes[12].sdo_mes

            INSERT INTO safre_tmp:tmp_retencion VALUES (reg_retencion.*)

        END IF

        LET reg_retencion.monto_retiro      = ''
        LET reg_retencion.monto_retenido    = ''
        LET reg_retencion.monto_total       = ''
        LET reg_retencion.monto_gravable    = ''
        LET reg_retencion.monto_no_gravable = ''

        SELECT 8,
               l.fecha_conversion,
               sum(l.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retiro
          FROM safre_tmp:tmp_liquidacion l
         WHERE l.folio            = reg_retencion.folio
           AND l.nss              = reg_retencion.nss
           AND l.subcuenta = 13
           AND l.consecutivo_lote = reg_retencion.consecutivo_lote
           AND l.fecha_conversion = reg_retencion.fecha_conversion
           AND l.tipo_movimiento  <> 10
        GROUP BY 1,2

        SELECT 8,
               r.fecha_conversion,
               sum(r.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retenido
          FROM safre_tmp:tmp_liquidacion r
         WHERE r.folio            = reg_retencion.folio
           AND r.nss              = reg_retencion.nss
           AND r.subcuenta = 13
           AND r.consecutivo_lote = reg_retencion.consecutivo_lote
           AND r.fecha_conversion = reg_retencion.fecha_conversion
           AND r.tipo_movimiento  = 10
        GROUP BY 1,2

        IF reg_retencion.monto_retiro <> 0 OR
           reg_retencion.monto_retiro IS NOT NULL THEN
            LET reg_retencion.monto_total       = reg_retencion.monto_retiro + reg_retencion.monto_retenido
            LET reg_retencion.monto_gravable    = reg_retencion.monto_retenido * 5
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_total - reg_retencion.monto_gravable

            LET reg_retencion.monto_retiro      = reg_retencion.monto_retiro * -1
            LET reg_retencion.monto_retenido    = reg_retencion.monto_retenido * -1
            LET reg_retencion.monto_total       = reg_retencion.monto_total * -1
            LET reg_retencion.monto_gravable    = reg_retencion.monto_gravable * -1
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_no_gravable * -1

            IF reg_retencion.monto_no_gravable < 0 THEN
                LET reg_retencion.monto_no_gravable = 0
                LET reg_retencion.monto_gravable    = reg_retencion.monto_total
            END IF

            SELECT MAX(m.fecha_conversion)
              INTO maxfecha_liq
              FROM safre_tmp:tmp_liquidacion m
             WHERE m.nss = reg_retencion.nss
               AND m.subcuenta = 13

            CALL obtiene_fin_mes(maxfecha_liq) RETURNING pfecha_fin

            LET vfprom_ini = MDY(MONTH(pfecha_fin) + 1, 1, YEAR(pfecha_fin) - 1)

            CALL tmp_cta_his(reg_retencion.nss, 7, vfprom_ini, pfecha_fin)

            CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin

            LET j = 1

            WHILE vfprom_fin <= pfecha_fin
                CALL obtiene_saldo(reg_retencion.nss, vfprom_ini, vfprom_fin, 0, 7)
                     RETURNING vsaldo_promedio

                IF vsaldo_promedio IS NULL OR
                   vsaldo_promedio < 0 THEN
                    LET vsaldo_promedio = 0
                END IF

                LET arr_mes[j].sdo_mes = vsaldo_promedio
                LET vsaldo_promedio    = 0

                LET vfprom_ini = vfprom_fin + 1

                LET j = j + 1

                CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin
            END WHILE

            LET reg_retencion.sdo_promedio1  = arr_mes[1].sdo_mes
            LET reg_retencion.sdo_promedio2  = arr_mes[2].sdo_mes
            LET reg_retencion.sdo_promedio3  = arr_mes[3].sdo_mes
            LET reg_retencion.sdo_promedio4  = arr_mes[4].sdo_mes
            LET reg_retencion.sdo_promedio5  = arr_mes[5].sdo_mes
            LET reg_retencion.sdo_promedio6  = arr_mes[6].sdo_mes
            LET reg_retencion.sdo_promedio7  = arr_mes[7].sdo_mes
            LET reg_retencion.sdo_promedio8  = arr_mes[8].sdo_mes
            LET reg_retencion.sdo_promedio9  = arr_mes[9].sdo_mes
            LET reg_retencion.sdo_promedio10 = arr_mes[10].sdo_mes
            LET reg_retencion.sdo_promedio11 = arr_mes[11].sdo_mes
            LET reg_retencion.sdo_promedio12 = arr_mes[12].sdo_mes

            INSERT INTO safre_tmp:tmp_retencion VALUES (reg_retencion.*)

        END IF

        LET reg_retencion.monto_retiro      = ''
        LET reg_retencion.monto_retenido    = ''
        LET reg_retencion.monto_total       = ''
        LET reg_retencion.monto_gravable    = ''
        LET reg_retencion.monto_no_gravable = ''

        SELECT 9,
               l.fecha_conversion,
               sum(l.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retiro
          FROM safre_tmp:tmp_liquidacion l
         WHERE l.folio            = reg_retencion.folio
           AND l.nss              = reg_retencion.nss
           AND l.subcuenta IN(20,21,28,29)
           AND l.consecutivo_lote = reg_retencion.consecutivo_lote
           AND l.fecha_conversion = reg_retencion.fecha_conversion
           AND l.tipo_movimiento  <> 10
        GROUP BY 1,2

        SELECT 9,
               r.fecha_conversion,
               sum(r.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retenido
          FROM safre_tmp:tmp_liquidacion r
         WHERE r.folio            = reg_retencion.folio
           AND r.nss              = reg_retencion.nss
           AND r.subcuenta IN(20,21,28,29)
           AND r.consecutivo_lote = reg_retencion.consecutivo_lote
           AND r.fecha_conversion = reg_retencion.fecha_conversion
           AND r.tipo_movimiento  = 10
        GROUP BY 1,2

        IF reg_retencion.monto_retiro <> 0 OR
           reg_retencion.monto_retiro IS NOT NULL THEN
            LET reg_retencion.monto_total       = reg_retencion.monto_retiro + reg_retencion.monto_retenido
            LET reg_retencion.monto_gravable    = reg_retencion.monto_retenido * 5
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_total - reg_retencion.monto_gravable

            LET reg_retencion.monto_retiro      = reg_retencion.monto_retiro * -1
            LET reg_retencion.monto_retenido    = reg_retencion.monto_retenido * -1
            LET reg_retencion.monto_total       = reg_retencion.monto_total * -1
            LET reg_retencion.monto_gravable    = reg_retencion.monto_gravable * -1
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_no_gravable * -1

            IF reg_retencion.monto_no_gravable < 0 THEN
                LET reg_retencion.monto_no_gravable = 0
                LET reg_retencion.monto_gravable    = reg_retencion.monto_total
            END IF

            SELECT MAX(m.fecha_conversion)
              INTO maxfecha_liq
              FROM safre_tmp:tmp_liquidacion m
             WHERE m.nss = reg_retencion.nss
               AND m.subcuenta IN(20,21,28,29)

            CALL obtiene_fin_mes(maxfecha_liq) RETURNING pfecha_fin

            LET vfprom_ini = MDY(MONTH(pfecha_fin) + 1, 1, YEAR(pfecha_fin) - 1)

            CALL tmp_cta_his(reg_retencion.nss, 7, vfprom_ini, pfecha_fin)

            CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin

            LET j = 1

            WHILE vfprom_fin <= pfecha_fin
                CALL obtiene_saldo(reg_retencion.nss, vfprom_ini, vfprom_fin, 0, 7)
                     RETURNING vsaldo_promedio

                IF vsaldo_promedio IS NULL OR
                   vsaldo_promedio < 0 THEN
                    LET vsaldo_promedio = 0
                END IF

                LET arr_mes[j].sdo_mes = vsaldo_promedio
                LET vsaldo_promedio    = 0

                LET vfprom_ini = vfprom_fin + 1

                LET j = j + 1

                CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin
            END WHILE

            LET reg_retencion.sdo_promedio1  = arr_mes[1].sdo_mes
            LET reg_retencion.sdo_promedio2  = arr_mes[2].sdo_mes
            LET reg_retencion.sdo_promedio3  = arr_mes[3].sdo_mes
            LET reg_retencion.sdo_promedio4  = arr_mes[4].sdo_mes
            LET reg_retencion.sdo_promedio5  = arr_mes[5].sdo_mes
            LET reg_retencion.sdo_promedio6  = arr_mes[6].sdo_mes
            LET reg_retencion.sdo_promedio7  = arr_mes[7].sdo_mes
            LET reg_retencion.sdo_promedio8  = arr_mes[8].sdo_mes
            LET reg_retencion.sdo_promedio9  = arr_mes[9].sdo_mes
            LET reg_retencion.sdo_promedio10 = arr_mes[10].sdo_mes
            LET reg_retencion.sdo_promedio11 = arr_mes[11].sdo_mes
            LET reg_retencion.sdo_promedio12 = arr_mes[12].sdo_mes

            INSERT INTO safre_tmp:tmp_retencion VALUES (reg_retencion.*)

        END IF

        LET reg_retencion.monto_retiro      = ''
        LET reg_retencion.monto_retenido    = ''
        LET reg_retencion.monto_total       = ''
        LET reg_retencion.monto_gravable    = ''
        LET reg_retencion.monto_no_gravable = ''

        SELECT 10,
               l.fecha_conversion,
               sum(l.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retiro
          FROM safre_tmp:tmp_liquidacion l
         WHERE l.folio            = reg_retencion.folio
           AND l.nss              = reg_retencion.nss
           AND l.subcuenta IN(30,31,32)
           AND l.consecutivo_lote = reg_retencion.consecutivo_lote
           AND l.fecha_conversion = reg_retencion.fecha_conversion
           AND l.tipo_movimiento  <> 10
        GROUP BY 1,2

        SELECT 10,
               r.fecha_conversion,
               sum(r.monto_en_pesos)
          INTO reg_retencion.grupo_regimen,
               reg_retencion.fecha_conversion,
               reg_retencion.monto_retenido
          FROM safre_tmp:tmp_liquidacion r
         WHERE r.folio            = reg_retencion.folio
           AND r.nss              = reg_retencion.nss
           AND r.subcuenta IN(30,31,32)
           AND r.consecutivo_lote = reg_retencion.consecutivo_lote
           AND r.fecha_conversion = reg_retencion.fecha_conversion
           AND r.tipo_movimiento  = 10
        GROUP BY 1,2

        IF reg_retencion.monto_retiro <> 0 OR
           reg_retencion.monto_retiro IS NOT NULL THEN
            LET reg_retencion.monto_total       = reg_retencion.monto_retiro + reg_retencion.monto_retenido
            LET reg_retencion.monto_gravable    = reg_retencion.monto_retenido * 5
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_total - reg_retencion.monto_gravable

            LET reg_retencion.monto_retiro      = reg_retencion.monto_retiro * -1
            LET reg_retencion.monto_retenido    = reg_retencion.monto_retenido * -1
            LET reg_retencion.monto_total       = reg_retencion.monto_total * -1
            LET reg_retencion.monto_gravable    = reg_retencion.monto_gravable * -1
            LET reg_retencion.monto_no_gravable = reg_retencion.monto_no_gravable * -1

            IF reg_retencion.monto_no_gravable < 0 THEN
                LET reg_retencion.monto_no_gravable = 0
                LET reg_retencion.monto_gravable    = reg_retencion.monto_total
            END IF

            SELECT MAX(m.fecha_conversion)
              INTO maxfecha_liq
              FROM safre_tmp:tmp_liquidacion m
             WHERE m.nss = reg_retencion.nss
               AND m.subcuenta IN(30,31,32)

            CALL obtiene_fin_mes(maxfecha_liq) RETURNING pfecha_fin

            LET vfprom_ini = MDY(MONTH(pfecha_fin) + 1, 1, YEAR(pfecha_fin) - 1)

            CALL tmp_cta_his(reg_retencion.nss, 7, vfprom_ini, pfecha_fin)

            CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin

            LET j = 1

            WHILE vfprom_fin <= pfecha_fin
                CALL obtiene_saldo(reg_retencion.nss, vfprom_ini, vfprom_fin, 0, 7)
                     RETURNING vsaldo_promedio

                IF vsaldo_promedio IS NULL OR
                   vsaldo_promedio < 0 THEN
                    LET vsaldo_promedio = 0
                END IF

                LET arr_mes[j].sdo_mes = vsaldo_promedio
                LET vsaldo_promedio    = 0

                LET vfprom_ini = vfprom_fin + 1

                LET j = j + 1

                CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin
            END WHILE

            LET reg_retencion.sdo_promedio1  = arr_mes[1].sdo_mes
            LET reg_retencion.sdo_promedio2  = arr_mes[2].sdo_mes
            LET reg_retencion.sdo_promedio3  = arr_mes[3].sdo_mes
            LET reg_retencion.sdo_promedio4  = arr_mes[4].sdo_mes
            LET reg_retencion.sdo_promedio5  = arr_mes[5].sdo_mes
            LET reg_retencion.sdo_promedio6  = arr_mes[6].sdo_mes
            LET reg_retencion.sdo_promedio7  = arr_mes[7].sdo_mes
            LET reg_retencion.sdo_promedio8  = arr_mes[8].sdo_mes
            LET reg_retencion.sdo_promedio9  = arr_mes[9].sdo_mes
            LET reg_retencion.sdo_promedio10 = arr_mes[10].sdo_mes
            LET reg_retencion.sdo_promedio11 = arr_mes[11].sdo_mes
            LET reg_retencion.sdo_promedio12 = arr_mes[12].sdo_mes

            INSERT INTO safre_tmp:tmp_retencion VALUES (reg_retencion.*)

        END IF

        LET reg_retencion.monto_retiro      = ''
        LET reg_retencion.monto_retenido    = ''
        LET reg_retencion.monto_total       = ''
        LET reg_retencion.monto_gravable    = ''
        LET reg_retencion.monto_no_gravable = ''

    END FOREACH

    ERROR ""
    PROMPT "PROCESO TERMINADO, [Enter] PARA SALIR" FOR enter

END FUNCTION

FUNCTION precio_siefore(x_siefore,x_fecha_valuacion)
#ps-------------------------------------------------

    DEFINE x_siefore          SMALLINT
    DEFINE x_fecha_valuacion  DATE
    DEFINE x_precio_accion    DECIMAL(19,14)

   SELECT v.precio_del_dia
     INTO x_precio_accion
     FROM safre_af:glo_valor_accion v
    WHERE v.fecha_valuacion = x_fecha_valuacion
      AND v.codigo_siefore  = x_siefore

    IF SQLCA.SQLCODE <> 0 THEN
        LET x_precio_accion = 0
    END IF

    RETURN x_precio_accion

END FUNCTION

FUNCTION obtiene_fin_mes(xfecha_liq)
#ofm--------------------------------

    DEFINE xfecha_liq DATE
    DEFINE pfin_mes   DATE

    CASE MONTH(xfecha_liq)
        WHEN  1 LET pfin_mes = MDY(MONTH(xfecha_liq),31,YEAR(xfecha_liq))
        WHEN  2 IF YEAR(xfecha_liq) MOD 4 = 0 THEN
                    LET pfin_mes = MDY(MONTH(xfecha_liq),29,YEAR(xfecha_liq))
                ELSE
                    LET pfin_mes = MDY(MONTH(xfecha_liq),28,YEAR(xfecha_liq))
                END IF
        WHEN  3 LET pfin_mes = MDY(MONTH(xfecha_liq),31,YEAR(xfecha_liq))
        WHEN  4 LET pfin_mes = MDY(MONTH(xfecha_liq),30,YEAR(xfecha_liq))
        WHEN  5 LET pfin_mes = MDY(MONTH(xfecha_liq),31,YEAR(xfecha_liq))
        WHEN  6 LET pfin_mes = MDY(MONTH(xfecha_liq),30,YEAR(xfecha_liq))
        WHEN  7 LET pfin_mes = MDY(MONTH(xfecha_liq),31,YEAR(xfecha_liq))
        WHEN  8 LET pfin_mes = MDY(MONTH(xfecha_liq),31,YEAR(xfecha_liq))
        WHEN  9 LET pfin_mes = MDY(MONTH(xfecha_liq),30,YEAR(xfecha_liq))
        WHEN 10 LET pfin_mes = MDY(MONTH(xfecha_liq),31,YEAR(xfecha_liq))
        WHEN 11 LET pfin_mes = MDY(MONTH(xfecha_liq),30,YEAR(xfecha_liq))
        WHEN 12 LET pfin_mes = MDY(MONTH(xfecha_liq),31,YEAR(xfecha_liq))
    END CASE

    RETURN pfin_mes

END FUNCTION

FUNCTION obtiene_saldo(vnss, pfprom_ini, pfprom_fin, vsubcta, vgpo_subcta)
#os-----------------------------------------------------------------------

    DEFINE vnss         CHAR(11)

    DEFINE vsubcta      SMALLINT
    DEFINE vgpo_subcta  SMALLINT
    DEFINE vsubcuenta   SMALLINT
    DEFINE vsiefore     SMALLINT

    DEFINE pfprom_ini   DATE
    DEFINE pfprom_fin   DATE

    DEFINE vsaldo_acc   DECIMAL(18,6)
    DEFINE vsaldo_pes   DECIMAL(18,6)
    DEFINE vsdo_mes     DECIMAL(18,6)
    DEFINE vprecio_sie  DECIMAL(18,6)

    LET vsaldo_acc = 0
    LET vsaldo_pes = 0
    LET vsdo_mes   = 0

    PREPARE eje_obtiene_sdo FROM eje_sdo

    WHILE pfprom_ini <= pfprom_fin
        DECLARE cur_sdo CURSOR FOR eje_obtiene_sdo

        FOREACH cur_sdo USING vnss,
                              vsubcta,
                              vgpo_subcta,
                              pfprom_ini
                        INTO  vsubcuenta ,
                              vsiefore   ,
                              vsaldo_acc ,
                              vsaldo_pes

            IF vsaldo_acc IS NULL OR vsaldo_acc <= 0 THEN
                LET vsaldo_acc = 0
                LET vsaldo_pes = 0
            ELSE
                LET vprecio_sie = precio_siefore (vsiefore, pfprom_fin)

                LET vsaldo_pes  = vsaldo_acc * vprecio_sie

                IF vsaldo_pes IS NULL OR vsaldo_pes <= 0 THEN
                    LET vsaldo_pes = 0
                END IF
            END IF

            LET vsdo_mes   = vsaldo_pes + vsdo_mes

            LET vsaldo_acc = 0
            LET vsaldo_pes = 0
        END FOREACH

        CLOSE cur_sdo
        FREE  cur_sdo

        LET pfprom_ini = pfprom_ini + 1

    END WHILE

    IF vsdo_mes > 0 THEN
        LET vsdo_mes = vsdo_mes / 30
    END IF

    RETURN vsdo_mes

END FUNCTION

FUNCTION tmp_cta_his(p_nss, pgpo_reg, pf_ini, pf_fin)
#tch-------------------------------------------------

    DEFINE pgpo_reg    SMALLINT

    DEFINE pf_ini      DATE
    DEFINE pf_fin      DATE

    DEFINE p_nss       CHAR(11)
    DEFINE vtabla_his  CHAR(20)
    DEFINE sel_his_cta CHAR(10000)

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_cuenta_his;
    WHENEVER ERROR STOP

    DECLARE cur_his_cta CURSOR FOR
    SELECT tabname
    FROM   systables
    WHERE  tabname MATCHES "dis_cuenta??"

    FOREACH cur_his_cta INTO vtabla_his
        LET sel_his_cta = sel_his_cta CLIPPED,
                          " SELECT a.nss, ",
                          "        a.subcuenta, ",
                          "        a.siefore, ",
                          "        a.fecha_conversion, ",
                          "        a.monto_en_acciones, ",
                          "        a.monto_en_pesos ",
                          " FROM   ",vtabla_his CLIPPED," a, ",
                          "        tab_agrupa_subcta_regimen b ",
                          " WHERE  a.nss = ","'",p_nss,"'",
                          " AND    a.subcuenta = b.subcuenta ",
                          " AND    a.fecha_conversion BETWEEN '",pf_ini,"' AND '", pf_fin,"' ",
                          " AND    b.grupo_regimen = ", pgpo_reg ,
                          " UNION ALL "
    END FOREACH
    CLOSE cur_his_cta

    LET sel_his_cta = sel_his_cta CLIPPED,
                      " SELECT a.nss, ",
                      "        a.subcuenta, ",
                      "        a.siefore, ",
                      "        a.fecha_conversion, ",
                      "        a.monto_en_acciones, ",
                      "        a.monto_en_pesos ",
                      " FROM   dis_cuenta a, ",
                      "        tab_agrupa_subcta_regimen b ",
                      " WHERE  a.nss = ","'",p_nss,"'",
                      " AND    a.subcuenta = b.subcuenta ",
                      " AND    a.fecha_conversion BETWEEN '",pf_ini,"' AND '", pf_fin,"' ",
                      " AND    b.grupo_regimen = ", pgpo_reg ,
                      " INTO TEMP tmp_cuenta_his "

    LET sel_his_cta = sel_his_cta CLIPPED
    WHENEVER ERROR CONTINUE
    
       PREPARE eje_sel_his_cta FROM sel_his_cta
       
       IF SQLCA.SQLCODE = 0 THEN 
      ELSE 
      	DISPLAY "nss: ", p_nss
      	DISPLAY "fini: ", pf_ini
      	DISPLAY "ffin: ", pf_fin
      	DISPLAY "pgpo_reg: ", pgpo_reg
      	DISPLAY sel_his_cta
      	EXIT PROGRAM
       END IF 	
    WHENEVER ERROR STOP
       
    EXECUTE eje_sel_his_cta

    CREATE INDEX tmp_cuenta_his1 on tmp_cuenta_his(nss,
                                                   subcuenta,
                                                   fecha_conversion)
    UPDATE STATISTICS FOR TABLE tmp_cuenta_his

END FUNCTION

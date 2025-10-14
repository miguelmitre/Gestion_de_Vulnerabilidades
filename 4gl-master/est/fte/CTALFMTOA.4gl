#***************************************************************
#****** CTALFORMATO_A.4gl
#******
#****** REPORTE DE PERFILES DE INVERSION
#******
#******  Requiere la fecha del corte para convertir a $$
#******
#****** JUNIO 2008 JOSUE LISANDRO HUERTA SIERRA
#******
#***************************************************************

DATABASE safre_tmp

DEFINE v_respuesta        CHAR(1)
DEFINE v_nss_aux          CHAR(11)
DEFINE v_fecha_corte      DATE
DEFINE tot_siefore        SMALLINT

DEFINE v_ruta             CHAR(40),
       v_archivo          CHAR(30),
       v_desc_registro    CHAR(30),
       v_tipo_reporte     SMALLINT,
       v_listado,
       v_listado1         CHAR(150),
       v_imprime          CHAR(150),
       v_usuario          CHAR(08),
       v_hoy              DATE    ,
       v_ctas_sie_reg     INTEGER ,
       v_total_trab       INTEGER

DEFINE total_cuentas      INTEGER
DEFINE total_acciones     DECIMAL(18,6)

DEFINE reg_datos_afore RECORD LIKE safre_af:tab_afore_local.*

DEFINE siefore ARRAY [8] OF RECORD
          codigo_siefore  SMALLINT,
          descripcion     CHAR(8),
          precio_accion   DECIMAL(12,6)
END RECORD

DEFINE arr_cta_siefore ARRAY [6] OF INTEGER
DEFINE tot_ctas_vol INTEGER

MAIN
    CALL STARTLOG ("CTALFMTOA.log")
    DEFER INTERRUPT
    OPTIONS
        PROMPT LINE LAST

    IF NUM_ARGS() = 1  THEN
        LET v_fecha_corte  = ARG_VAL(1)
    ELSE
        ERROR " Ejecute asi : $ nohup fglgo CTALFMTOA 'MM/DD/YYYY' & "
        SLEEP 3
        EXIT PROGRAM
    END IF

    CALL f_lee_reporte()
    CALL f_saldos()

    UPDATE cta_ctr_fmto_a
       SET fecha_fin       = CURRENT,
           fecha_proceso   = TODAY,
           usuario         = USER
     WHERE fecha_corte = v_fecha_corte
       AND paso   = 4 ;

--    LET v_imprime = "lp ",v_listado CLIPPED
--    RUN v_imprime
END MAIN

FUNCTION f_lee_reporte()

    DEFINE i                  SMALLINT

    LET v_hoy          = TODAY 
    LET total_cuentas  = 0
    LET total_acciones = 0

    SELECT ruta_listados,USER
      INTO v_ruta ,v_usuario
      FROM safre_af:seg_modulo
     WHERE modulo_cod = "cta"

    SELECT a.*
      INTO reg_datos_afore.*
      FROM safre_af:tab_afore_local a

    FOR i = 1 TO 8
        SELECT a.codigo_siefore, b.razon_social, a.precio_del_dia
          INTO siefore[i].codigo_siefore, siefore[i].descripcion, siefore[i].precio_accion
          FROM safre_af:glo_valor_accion a, safre_af:tab_siefore_local b
         WHERE a.codigo_siefore   = b.codigo_siefore
           AND a.fecha_valuacion  = v_fecha_corte
           AND a.codigo_siefore   = i
    END FOR

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_nss_fa
        DROP TABLE fmto_a
    WHENEVER ERROR STOP

    CREATE TEMP TABLE tmp_nss_fa
           ( nss CHAR(11))

    CREATE UNIQUE INDEX tmp_nss_fa1 ON tmp_nss_fa ( nss )

    CREATE TABLE fmto_a (  nss                 CHAR(11),
                           ind_edad            SMALLINT,
                           siefore             SMALLINT,
                           grupo_regimen       SMALLINT,
                           monto_en_acciones   DECIMAL(18,6))

    CREATE INDEX fmto_a_1 ON fmto_a (nss, ind_edad, siefore)

    LET v_listado  = v_ruta CLIPPED,"/",v_usuario CLIPPED,".FORMATO-A.",
                     v_fecha_corte USING "MMYYYY",".",v_hoy USING "DDMMYYYY"
    LET v_listado1 = v_ruta CLIPPED,"/",v_usuario CLIPPED,".salida"
END FUNCTION

FUNCTION f_saldos()

    DEFINE i                  SMALLINT
    DEFINE v_prioridad        CHAR(40)
    DEFINE reg_cta_igual     RECORD 
        rcv_imss          INTEGER,
        sar92             INTEGER,
        rcv_issste        INTEGER,
        ahorro_soli       INTEGER,
        aar               INTEGER,
        avol              INTEGER,
        acr               INTEGER,
        alp               INTEGER,
        aplp              INTEGER
    END RECORD
    DEFINE reg_acc_igual     RECORD 
        rcv_imss          DECIMAL(18,6),
        sar92             DECIMAL(18,6),
        rcv_issste        DECIMAL(18,6),
        ahorro_soli       DECIMAL(18,6),
        aar               DECIMAL(18,6),
        avol              DECIMAL(18,6),
        acr               DECIMAL(18,6),
        alp               DECIMAL(18,6),
        aplp              DECIMAL(18,6)
    END RECORD
    DEFINE reg_cta_dif     RECORD 
        rcv_imss          INTEGER,
        sar92             INTEGER,
        rcv_issste        INTEGER,
        ahorro_soli       INTEGER,
        aar               INTEGER,
        avol              INTEGER,
        acr               INTEGER,
        alp               INTEGER,
        aplp              INTEGER
    END RECORD
    DEFINE reg_acc_dif     RECORD 
        rcv_imss          DECIMAL(18,6),
        sar92             DECIMAL(18,6),
        rcv_issste        DECIMAL(18,6),
        ahorro_soli       DECIMAL(18,6),
        aar               DECIMAL(18,6),
        avol              DECIMAL(18,6),
        acr               DECIMAL(18,6),
        alp               DECIMAL(18,6),
        aplp              DECIMAL(18,6)
    END RECORD
    DEFINE reg_paso        RECORD 
        grupo_regimen     SMALLINT,
        acciones          DECIMAL(18,6),
        cuentas           INTEGER
    END RECORD

    LET v_prioridad = "SET PDQPRIORITY HIGH"

    PREPARE eje_prio_ini FROM v_prioridad
    EXECUTE eje_prio_ini

{    INSERT INTO fmto_a
     SELECT t.nss  ,
            t.ind_edad ,
            d.siefore         ,
            c.grupo_regimen   ,
            d.monto_en_acciones
       FROM cta_formato_nss   t,
            OUTER (tmp_saldo_corte d,   safre_af:tab_agrupa_subcta_regimen c)
      WHERE d.nss  = t.nss
        AND d.subcuenta NOT IN (4,8,14,35)
        AND d.subcuenta = c.subcuenta
}

     DELETE
       FROM tmp_saldo_corte
      WHERE monto_en_acciones = 0

     INSERT INTO fmto_a
     SELECT d.nss  ,
            NVL(t.ind_edad,d.siefore),
            d.siefore         ,
            c.grupo_regimen   ,
            d.monto_en_acciones
       FROM tmp_saldo_corte d,
            safre_af:tab_agrupa_subcta_regimen c,
            OUTER( cta_formato_nss   t)
      WHERE d.nss  = t.nss
        AND d.subcuenta NOT IN (4,8,14,35)
        AND d.subcuenta = c.subcuenta

    START REPORT r_saldos TO v_listado

    FOR i = 1 TO 8
        INITIALIZE reg_cta_igual.* TO NULL
        INITIALIZE reg_acc_igual.* TO NULL
        INITIALIZE reg_cta_dif.*   TO NULL
        INITIALIZE reg_acc_dif.*   TO NULL

        DECLARE cur_fmto_eq CURSOR FOR
        SELECT a.grupo_regimen   ,
               SUM(a.monto_en_acciones)
          FROM fmto_a a
         WHERE a.ind_edad = a.siefore
           AND a.siefore  = i
         GROUP BY 1
         ORDER BY 1

         FOREACH cur_fmto_eq INTO reg_paso.grupo_regimen, reg_paso.acciones
             IF reg_paso.grupo_regimen IS NOT NULL THEN
                 SELECT COUNT(unique a.nss)
                   INTO reg_paso.cuentas
                   FROM fmto_a a
                  WHERE a.ind_edad = a.siefore
                    AND a.siefore  = i
                    AND a.grupo_regimen = reg_paso.grupo_regimen

                 CASE reg_paso.grupo_regimen
                      WHEN 1
                          LET reg_cta_igual.rcv_imss = reg_paso.cuentas
                          LET reg_acc_igual.rcv_imss = reg_paso.acciones
                      WHEN 2
                          LET reg_cta_igual.acr = reg_paso.cuentas
                          LET reg_acc_igual.acr = reg_paso.acciones
                      WHEN 3
                          LET reg_cta_igual.avol = reg_paso.cuentas
                          LET reg_acc_igual.avol = reg_paso.acciones
                      WHEN 4
                          LET reg_cta_igual.alp = reg_paso.cuentas
                          LET reg_acc_igual.alp = reg_paso.acciones
                      WHEN 7
                          LET reg_cta_igual.sar92 = reg_paso.cuentas
                          LET reg_acc_igual.sar92 = reg_paso.acciones
                      WHEN 8
                          LET reg_cta_igual.aar = reg_paso.cuentas
                          LET reg_acc_igual.aar = reg_paso.acciones
                      WHEN 9
                          LET reg_cta_igual.aplp = reg_paso.cuentas
                          LET reg_acc_igual.aplp = reg_paso.acciones
                      WHEN 10
                          LET reg_cta_igual.rcv_issste = reg_paso.cuentas
                          LET reg_acc_igual.rcv_issste = reg_paso.acciones
                      WHEN 11
                          LET reg_cta_igual.ahorro_soli = reg_paso.cuentas
                          LET reg_acc_igual.ahorro_soli = reg_paso.acciones
                 END CASE
             END IF
         END FOREACH

        DECLARE cur_fmto_dif CURSOR FOR
        SELECT a.grupo_regimen   ,
               SUM(a.monto_en_acciones)
          FROM fmto_a a
         WHERE a.ind_edad <> a.siefore
           AND a.siefore = i
         GROUP BY 1
         ORDER BY 1

         FOREACH cur_fmto_dif INTO reg_paso.grupo_regimen, reg_paso.acciones

             IF reg_paso.grupo_regimen IS NOT NULL THEN
                 SELECT COUNT(unique a.nss)
                   INTO reg_paso.cuentas
                   FROM fmto_a a
                  WHERE a.ind_edad <> a.siefore
                    AND a.siefore  = i
                    AND a.grupo_regimen = reg_paso.grupo_regimen

                 CASE reg_paso.grupo_regimen
                     WHEN 1
                         LET reg_cta_dif.rcv_imss = reg_paso.cuentas
                         LET reg_acc_dif.rcv_imss = reg_paso.acciones
                     WHEN 2
                         LET reg_cta_dif.acr = reg_paso.cuentas
                         LET reg_acc_dif.acr = reg_paso.acciones
                     WHEN 3
                         LET reg_cta_dif.avol = reg_paso.cuentas
                         LET reg_acc_dif.avol = reg_paso.acciones
                     WHEN 4
                         LET reg_cta_dif.alp = reg_paso.cuentas
                         LET reg_acc_dif.alp = reg_paso.acciones
                     WHEN 7
                         LET reg_cta_dif.sar92 = reg_paso.cuentas
                         LET reg_acc_dif.sar92 = reg_paso.acciones
                     WHEN 8
                         LET reg_cta_dif.aar = reg_paso.cuentas
                         LET reg_acc_dif.aar = reg_paso.acciones
                     WHEN 9
                         LET reg_cta_dif.aplp = reg_paso.cuentas
                         LET reg_acc_dif.aplp = reg_paso.acciones
                     WHEN 10
                         LET reg_cta_dif.rcv_issste = reg_paso.cuentas
                         LET reg_acc_dif.rcv_issste = reg_paso.acciones
                     WHEN 11
                         LET reg_cta_dif.ahorro_soli = reg_paso.cuentas
                         LET reg_acc_dif.ahorro_soli = reg_paso.acciones
                 END CASE
             END IF
         END FOREACH

        IF reg_cta_igual.rcv_imss IS NULL THEN
            LET reg_cta_igual.rcv_imss = 0
            LET reg_acc_igual.rcv_imss = 0
        END IF

        IF reg_cta_igual.sar92 IS NULL THEN
            LET reg_cta_igual.sar92 = 0
            LET reg_acc_igual.sar92 = 0
        END IF

        IF reg_cta_igual.rcv_issste IS NULL THEN
            LET reg_cta_igual.rcv_issste = 0
            LET reg_acc_igual.rcv_issste = 0
        END IF

        IF reg_cta_igual.ahorro_soli IS NULL THEN
            LET reg_cta_igual.ahorro_soli = 0
            LET reg_acc_igual.ahorro_soli = 0
        END IF

        IF reg_cta_igual.aar IS NULL THEN
            LET reg_cta_igual.aar = 0
            LET reg_acc_igual.aar = 0
        END IF

        IF reg_cta_igual.avol IS NULL THEN
            LET reg_cta_igual.avol = 0
            LET reg_acc_igual.avol = 0
        END IF

        IF reg_cta_igual.acr IS NULL THEN
            LET reg_cta_igual.acr = 0
            LET reg_acc_igual.acr = 0
        END IF

        IF reg_cta_igual.alp IS NULL THEN
            LET reg_cta_igual.alp = 0
            LET reg_acc_igual.alp = 0
        END IF

        IF reg_cta_igual.aplp IS NULL THEN
            LET reg_cta_igual.aplp = 0
            LET reg_acc_igual.aplp = 0
        END IF

        IF reg_cta_dif.rcv_imss IS NULL THEN
            LET reg_cta_dif.rcv_imss = 0
            LET reg_acc_dif.rcv_imss = 0
        END IF

        IF reg_cta_dif.sar92 IS NULL THEN
            LET reg_cta_dif.sar92 = 0
            LET reg_acc_dif.sar92 = 0
        END IF

        IF reg_cta_dif.rcv_issste IS NULL THEN
            LET reg_cta_dif.rcv_issste = 0
            LET reg_acc_dif.rcv_issste = 0
        END IF

        IF reg_cta_dif.ahorro_soli IS NULL THEN
            LET reg_cta_dif.ahorro_soli = 0
            LET reg_acc_dif.ahorro_soli = 0
        END IF

        IF reg_cta_dif.aar IS NULL THEN
            LET reg_cta_dif.aar = 0
            LET reg_acc_dif.aar = 0
        END IF

        IF reg_cta_dif.avol IS NULL THEN
            LET reg_cta_dif.avol = 0
            LET reg_acc_dif.avol = 0
        END IF

        IF reg_cta_dif.acr IS NULL THEN
            LET reg_cta_dif.acr = 0
            LET reg_acc_dif.acr = 0
        END IF

        IF reg_cta_dif.alp IS NULL THEN
            LET reg_cta_dif.alp = 0
            LET reg_acc_dif.alp = 0
        END IF

        IF reg_cta_dif.aplp IS NULL THEN
            LET reg_cta_dif.aplp = 0
            LET reg_acc_dif.aplp = 0
        END IF


        IF i = 1 OR
           i = 2 OR
           i = 3 OR
           i = 4 OR
           i = 5 THEN
            OUTPUT TO REPORT r_saldos( siefore[i].*,reg_cta_igual.*, reg_acc_igual.*, 
                                       reg_cta_dif.*, reg_acc_dif.*)
        ELSE
                LET reg_cta_dif.rcv_imss    = 0
                LET reg_cta_dif.sar92       = 0
                LET reg_cta_dif.rcv_issste  = 0
                LET reg_cta_dif.ahorro_soli = 0
                LET reg_cta_dif.aar         = 0
                LET reg_cta_dif.avol        = 0
                LET reg_cta_dif.acr         = 0
                LET reg_cta_dif.alp         = 0
                LET reg_cta_dif.aplp        = 0
 
                LET reg_acc_dif.rcv_imss    = 0
                LET reg_acc_dif.sar92       = 0
                LET reg_acc_dif.rcv_issste  = 0
                LET reg_acc_dif.ahorro_soli = 0
                LET reg_acc_dif.aar         = 0
                LET reg_acc_dif.avol        = 0
                LET reg_acc_dif.acr         = 0
                LET reg_acc_dif.alp         = 0
                LET reg_acc_dif.aplp        = 0
            OUTPUT TO REPORT r_saldos( siefore[i].*,reg_cta_igual.*, reg_acc_igual.*, 
                                       reg_cta_dif.*, reg_acc_dif.*)
        END IF
    END FOR

    FINISH REPORT r_saldos
END FUNCTION

REPORT r_saldos (sie,cta_igual, acc_igual, cta_dif, acc_dif)

    DEFINE cta_igual     RECORD 
        rcv_imss          INTEGER,
        sar92             INTEGER,
        rcv_issste        INTEGER,
        ahorro_soli       INTEGER,
        aar               INTEGER,
        avol              INTEGER,
        acr               INTEGER,
        alp               INTEGER,
        aplp              INTEGER
    END RECORD
    DEFINE acc_igual     RECORD 
        rcv_imss          DECIMAL(18,6),
        sar92             DECIMAL(18,6),
        rcv_issste        DECIMAL(18,6),
        ahorro_soli       DECIMAL(18,6),
        aar               DECIMAL(18,6),
        avol              DECIMAL(18,6),
        acr               DECIMAL(18,6),
        alp               DECIMAL(18,6),
        aplp              DECIMAL(18,6)
    END RECORD
    DEFINE cta_dif     RECORD 
        rcv_imss          INTEGER,
        sar92             INTEGER,
        rcv_issste        INTEGER,
        ahorro_soli       INTEGER,
        aar               INTEGER,
        avol              INTEGER,
        acr               INTEGER,
        alp               INTEGER,
        aplp              INTEGER
    END RECORD
    DEFINE acc_dif     RECORD 
        rcv_imss          DECIMAL(18,6),
        sar92             DECIMAL(18,6),
        rcv_issste        DECIMAL(18,6),
        ahorro_soli       DECIMAL(18,6),
        aar               DECIMAL(18,6),
        avol              DECIMAL(18,6),
        acr               DECIMAL(18,6),
        alp               DECIMAL(18,6),
        aplp              DECIMAL(18,6)
    END RECORD
    DEFINE sie RECORD
        cod_siefore     SMALLINT,
        descripcion     CHAR(8),
        precio_accion   DECIMAL(12,6)
    END RECORD
    DEFINE leyenda_eq       ,
           leyenda_dif      CHAR(10)

    DEFINE tot_cta_eq         INTEGER
    DEFINE tot_cta_dif        INTEGER
    DEFINE total_cuentas_inv  INTEGER
    DEFINE tot_acc_eq         DECIMAL(18,6)
    DEFINE tot_acc_dif        DECIMAL(18,6)
    DEFINE cadena_precio      CHAR(150)
    DEFINE cadena_desc        CHAR(150)
    DEFINE i                  SMALLINT

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH   90

    FORMAT

       PAGE HEADER
           PRINT COLUMN 80, "ANEXO A"
           PRINT COLUMN 72, "Perfiles de Inversion"
           PRINT
           PRINT "MES QUE SE REPORTA:", v_fecha_corte USING "MM/YYYY"
           PRINT "       CLAVE AFORE:", reg_datos_afore.codigo_afore
           PRINT "             AFORE:", reg_datos_afore.razon_social
           PRINT "--------------------------------------------------------------",
                 "--------------------------------------------------------------",
                 "--------------------------------------------------------------",
                 "--------------------------------------------------------------",
                 "-----------------------------------"

           PRINT
           PRINT "PERFIL DEL TRABAJADOR",
               COLUMN 194, "AHORRO VOLUNTARIO"
           PRINT COLUMN 43, "RCV IMSS",
               COLUMN  67, "SEGURO DEL",
               COLUMN  91, "RCV ISSSTE",
               COLUMN 118, "AHORRO",
               COLUMN 141, "AHORRO PARA",
               COLUMN 166, "APORTACIONES",
               COLUMN 188, "COMPLEMENTARIAS",
               COLUMN 214, "DE AHORRO A",
               COLUMN 233, "PERSPECTIVAS INVERSION",
               COLUMN 272, "TOTAL"
           PRINT "SIEFORE",
               COLUMN  12, "EDAD",
               COLUMN  69, "RETIRO",
               COLUMN 116, "SOLIDARIO",
               COLUMN 142, "EL RETIRO",
               COLUMN 166, "VOLUNTARIAS",
               COLUMN 190, "DE RETIRO",
               COLUMN 214, "LARGO PLAZO",
               COLUMN 238, "LARGO PLAZO"
           PRINT "--------------------------------------------------------------",
                 "--------------------------------------------------------------",
                 "--------------------------------------------------------------",
                 "--------------------------------------------------------------",
                 "-----------------------------------"

        ON EVERY ROW
           CASE sie.cod_siefore
               WHEN "1"
                   LET leyenda_eq  = "ASIGNADO"
                   LET leyenda_dif = "ELEGIDO"
               WHEN "2"
                   LET leyenda_eq  = "ASIGNADO"
                   LET leyenda_dif = "ELEGIDO"
               WHEN "3"
                   LET leyenda_eq  = "ASIGNADO"
                   LET leyenda_dif = "ELEGIDO"
               WHEN "4"
                   LET leyenda_eq  = "ASIGNADO"
                   LET leyenda_dif = "ELEGIDO"
               WHEN "5"
                   LET leyenda_eq  = "ASIGNADO"
                   LET leyenda_dif = "ELEGIDO"
               OTHERWISE
                   LET leyenda_eq  = ""
            END CASE


        IF sie.cod_siefore = 1 THEN    #SB 1, SIN SIEFORE ADICIONAL#
            SELECT COUNT(UNIQUE a.nss)
              INTO tot_cta_eq
              FROM fmto_a a
             WHERE (    a.ind_edad = a.siefore
                    AND a.siefore  = sie.cod_siefore)
                OR (a.grupo_regimen = 3)

            LET tot_acc_eq = acc_igual.rcv_imss    +
                             acc_igual.sar92       +
                             acc_igual.rcv_issste  +
                             acc_igual.ahorro_soli +
                             acc_igual.aar         +
                             acc_igual.avol        +
                             acc_dif.avol          +
                             acc_igual.acr         +
                             acc_igual.alp         +
                             acc_igual.aplp

            SELECT COUNT(UNIQUE a.nss)
              INTO tot_cta_dif
              FROM fmto_a a
             WHERE a.ind_edad <> a.siefore
               AND a.siefore  = sie.cod_siefore
               AND a.grupo_regimen <> 3         #DESCRIMINAR EL GRUPO DE AP VOLUNTARIAS COMO ELEGIDAS#

            LET tot_acc_dif = acc_dif.rcv_imss    +
                              acc_dif.sar92       +
                              acc_dif.rcv_issste  +
                              acc_dif.ahorro_soli +
                              acc_dif.aar         +
                              acc_dif.acr         +
                              acc_dif.alp         +
                              acc_dif.aplp

            PRINT sie.descripcion, 
                COLUMN 10, leyenda_eq,
                COLUMN 20, "Cuentas", 
                COLUMN 30,   cta_igual.rcv_imss             USING "#,###,###,###,###,###,##&",
                COLUMN 55,   cta_igual.sar92                USING "#,###,###,###,###,###,##&",
                COLUMN 80,   cta_igual.rcv_issste           USING "#,###,###,###,###,###,##&",
                COLUMN 105,  cta_igual.ahorro_soli          USING "#,###,###,###,###,###,##&",
                COLUMN 130,  cta_igual.aar                  USING "#,###,###,###,###,###,##&",
                COLUMN 155, (cta_igual.avol + cta_dif.avol) USING "#,###,###,###,###,###,##&",
                COLUMN 180,  cta_igual.acr                  USING "#,###,###,###,###,###,##&",
                COLUMN 205,  cta_igual.alp                  USING "#,###,###,###,###,###,##&",
                COLUMN 230,  cta_igual.aplp                 USING "#,###,###,###,###,###,##&",
                COLUMN 255,  tot_cta_eq                     USING "#,###,###,###,###,###,##&"

            PRINT COLUMN 20, "Titulos",
                COLUMN 30,  acc_igual.rcv_imss             USING "##,###,###,###,###.######",
                COLUMN 55,  acc_igual.sar92                USING "##,###,###,###,###.######",
                COLUMN 80,  acc_igual.rcv_issste           USING "##,###,###,###,###.######",
                COLUMN 105, acc_igual.ahorro_soli          USING "##,###,###,###,###.######",
                COLUMN 130, acc_igual.aar                  USING "##,###,###,###,###.######",
                COLUMN 155,(acc_igual.avol + acc_dif.avol) USING "##,###,###,###,###.######",
                COLUMN 180, acc_igual.acr                  USING "##,###,###,###,###.######",
                COLUMN 205, acc_igual.alp                  USING "##,###,###,###,###.######",
                COLUMN 230, acc_igual.aplp                 USING "##,###,###,###,###.######",
                COLUMN 255, tot_acc_eq                     USING "##,###,###,###,###.######"

            PRINT COLUMN 10, leyenda_dif,
                COLUMN 20, "Cuentas",
                COLUMN 30,  cta_dif.rcv_imss    USING "#,###,###,###,###,###,##&",
                COLUMN 55,  cta_dif.sar92       USING "#,###,###,###,###,###,##&",
                COLUMN 80,  cta_dif.rcv_issste  USING "#,###,###,###,###,###,##&",
                COLUMN 105, cta_dif.ahorro_soli USING "#,###,###,###,###,###,##&",
                COLUMN 130, cta_dif.aar         USING "#,###,###,###,###,###,##&",
                COLUMN 179, "0",
                COLUMN 180, cta_dif.acr         USING "#,###,###,###,###,###,##&",
                COLUMN 205, cta_dif.alp         USING "#,###,###,###,###,###,##&",
                COLUMN 230, cta_dif.aplp        USING "#,###,###,###,###,###,##&",
                COLUMN 255, tot_cta_dif         USING "#,###,###,###,###,###,##&" 
                
            PRINT COLUMN 20, "Titulos",
                COLUMN 30,  acc_dif.rcv_imss    USING "##,###,###,###,###.######",
                COLUMN 55,  acc_dif.sar92       USING "##,###,###,###,###.######",
                COLUMN 80,  acc_dif.rcv_issste  USING "##,###,###,###,###.######",
                COLUMN 105, acc_dif.ahorro_soli USING "##,###,###,###,###.######",
                COLUMN 130, acc_dif.aar         USING "##,###,###,###,###.######",
                COLUMN 173, ".000000",
                COLUMN 180, acc_dif.acr         USING "##,###,###,###,###.######",
                COLUMN 205, acc_dif.alp         USING "##,###,###,###,###.######",
                COLUMN 230, acc_dif.aplp        USING "##,###,###,###,###.######",
                COLUMN 255,tot_acc_dif          USING "##,###,###,###,###.######"
            PRINT
            PRINT

            LET total_cuentas =  tot_cta_eq  + tot_cta_dif + total_cuentas
            LET total_acciones = tot_acc_eq  + tot_acc_dif + total_acciones

        ELSE IF sie.cod_siefore = 2 OR
                sie.cod_siefore = 3 OR
                sie.cod_siefore = 4 OR
                sie.cod_siefore = 5 THEN

                 SELECT COUNT(UNIQUE a.nss)
                   INTO tot_cta_eq
                   FROM fmto_a a
                  WHERE a.ind_edad = a.siefore
                    AND a.siefore  = sie.cod_siefore

                 LET tot_acc_eq = acc_igual.rcv_imss    +
                                  acc_igual.sar92       +
                                  acc_igual.rcv_issste  +
                                  acc_igual.ahorro_soli +
                                  acc_igual.aar         +
                                  acc_igual.avol        +
                                  acc_igual.acr         +
                                  acc_igual.alp         +
                                  acc_igual.aplp

                 SELECT COUNT(UNIQUE a.nss)
                   INTO tot_cta_dif
                   FROM fmto_a a
                  WHERE a.ind_edad <> a.siefore
                    AND a.siefore  = sie.cod_siefore

                 LET tot_acc_dif = acc_dif.rcv_imss    +
                                   acc_dif.sar92       +
                                   acc_dif.rcv_issste  +
                                   acc_dif.ahorro_soli +
                                   acc_dif.aar         +
                                   acc_dif.avol        +
                                   acc_dif.acr         +
                                   acc_dif.alp         +
                                   acc_dif.aplp

                 PRINT sie.descripcion, 
                     COLUMN 10, leyenda_eq,
                     COLUMN 20, "Cuentas", 
                     COLUMN 30, cta_igual.rcv_imss     USING "#,###,###,###,###,###,##&",
                     COLUMN 55, cta_igual.sar92        USING "#,###,###,###,###,###,##&",
                     COLUMN 80, cta_igual.rcv_issste   USING "#,###,###,###,###,###,##&",
                     COLUMN 105, cta_igual.ahorro_soli USING "#,###,###,###,###,###,##&",
                     COLUMN 130, cta_igual.aar         USING "#,###,###,###,###,###,##&",
                     COLUMN 155, cta_igual.avol        USING "#,###,###,###,###,###,##&",
                     COLUMN 180, cta_igual.acr         USING "#,###,###,###,###,###,##&",
                     COLUMN 205, cta_igual.alp         USING "#,###,###,###,###,###,##&",
                     COLUMN 230, cta_igual.aplp        USING "#,###,###,###,###,###,##&",
                     COLUMN 255, tot_cta_eq            USING "#,###,###,###,###,###,##&"

                 PRINT COLUMN 20, "Titulos",
                     COLUMN 30, acc_igual.rcv_imss     USING "##,###,###,###,###.######",
                     COLUMN 55, acc_igual.sar92        USING "##,###,###,###,###.######",
                     COLUMN 80, acc_igual.rcv_issste   USING "##,###,###,###,###.######",
                     COLUMN 105, acc_igual.ahorro_soli USING "##,###,###,###,###.######",
                     COLUMN 130, acc_igual.aar         USING "##,###,###,###,###.######",
                     COLUMN 155,acc_igual.avol         USING "##,###,###,###,###.######",
                     COLUMN 180,acc_igual.acr          USING "##,###,###,###,###.######",
                     COLUMN 205,acc_igual.alp          USING "##,###,###,###,###.######",
                     COLUMN 230,acc_igual.aplp         USING "##,###,###,###,###.######",
                     COLUMN 255,tot_acc_eq             USING "##,###,###,###,###.######"

                 PRINT COLUMN 10, leyenda_dif,
                     COLUMN 20, "Cuentas",
                     COLUMN 30,  cta_dif.rcv_imss    USING "#,###,###,###,###,###,##&",
                     COLUMN 55,  cta_dif.sar92       USING "#,###,###,###,###,###,##&",
                     COLUMN 80,  cta_dif.rcv_issste  USING "#,###,###,###,###,###,##&",
                     COLUMN 105, cta_dif.ahorro_soli USING "#,###,###,###,###,###,##&",
                     COLUMN 130, cta_dif.aar         USING "#,###,###,###,###,###,##&",
                     COLUMN 155, cta_dif.avol        USING "#,###,###,###,###,###,##&",
                     COLUMN 180, cta_dif.acr         USING "#,###,###,###,###,###,##&",
                     COLUMN 205, cta_dif.alp         USING "#,###,###,###,###,###,##&",
                     COLUMN 230, cta_dif.aplp        USING "#,###,###,###,###,###,##&",
                     COLUMN 255, tot_cta_dif         USING "#,###,###,###,###,###,##&" 
                     
                 PRINT COLUMN 20, "Titulos",
                     COLUMN 30,  acc_dif.rcv_imss    USING "##,###,###,###,###.######",
                     COLUMN 55,  acc_dif.sar92       USING "##,###,###,###,###.######",
                     COLUMN 80,  acc_dif.rcv_issste  USING "##,###,###,###,###.######",
                     COLUMN 105, acc_dif.ahorro_soli USING "##,###,###,###,###.######",
                     COLUMN 130, acc_dif.aar         USING "##,###,###,###,###.######",
                     COLUMN 155, acc_dif.avol        USING "##,###,###,###,###.######",
                     COLUMN 180, acc_dif.acr         USING "##,###,###,###,###.######",
                     COLUMN 205, acc_dif.alp         USING "##,###,###,###,###.######",
                     COLUMN 230, acc_dif.aplp        USING "##,###,###,###,###.######",
                     COLUMN 255,tot_acc_dif          USING "##,###,###,###,###.######"
                 PRINT
                 PRINT

                 LET total_cuentas =  tot_cta_eq  + tot_cta_dif + total_cuentas
                 LET total_acciones = tot_acc_eq  + tot_acc_dif + total_acciones
             ELSE
                 IF sie.descripcion IS NOT NULL AND
                    sie.descripcion <> '        ' THEN
                     PRINT sie.descripcion, 
                         COLUMN 10, leyenda_eq,
                         COLUMN 20, "Cuentas", 
                         COLUMN 30, cta_igual.rcv_imss     USING "#,###,###,###,###,###,##&",
                         COLUMN 55, cta_igual.sar92        USING "#,###,###,###,###,###,##&",
                         COLUMN 80, cta_igual.rcv_issste   USING "#,###,###,###,###,###,##&",
                         COLUMN 105, cta_igual.ahorro_soli USING "#,###,###,###,###,###,##&",
                         COLUMN 130, cta_igual.aar         USING "#,###,###,###,###,###,##&",
                         COLUMN 155, cta_igual.avol        USING "#,###,###,###,###,###,##&",
                         COLUMN 180, cta_igual.acr         USING "#,###,###,###,###,###,##&",
                         COLUMN 205, cta_igual.alp         USING "#,###,###,###,###,###,##&",
                         COLUMN 230, cta_igual.aplp        USING "#,###,###,###,###,###,##&",
                         COLUMN 255, tot_cta_eq            USING "#,###,###,###,###,###,##&" 
     
                     PRINT COLUMN 20, "Titulos",
                         COLUMN 30, acc_igual.rcv_imss    USING "##,###,###,###,###.######",
                         COLUMN 55, acc_igual.sar92       USING "##,###,###,###,###.######",
                         COLUMN 80, acc_igual.rcv_issste  USING "##,###,###,###,###.######",
                         COLUMN 105,acc_igual.ahorro_soli USING "##,###,###,###,###.######",
                         COLUMN 130,acc_igual.aar         USING "##,###,###,###,###.######",
                         COLUMN 155,acc_igual.avol        USING "##,###,###,###,###.######",
                         COLUMN 180,acc_igual.acr         USING "##,###,###,###,###.######",
                         COLUMN 205,acc_igual.alp         USING "##,###,###,###,###.######",
                         COLUMN 230,acc_igual.aplp        USING "##,###,###,###,###.######",
                         COLUMN 255,tot_acc_eq            USING "##,###,###,###,###.######"
                     PRINT
                     PRINT
     
                     LET total_cuentas  = tot_cta_eq  + total_cuentas
                     LET total_acciones = tot_acc_eq + total_acciones
                 END IF
             END IF
        END IF

        LET cadena_precio = cadena_precio CLIPPED, "      ",  sie.precio_accion USING "$$$&.######" CLIPPED
        LET cadena_desc   = cadena_desc   CLIPPED, "          ", sie.descripcion

        ON LAST ROW

              SELECT a.nss, a.siefore
                FROM fmto_a a
               GROUP BY 1,2
                INTO TEMP cuentas_siefores

              SELECT a.nss, COUNT(*) total
                FROM cuentas_siefores a
               GROUP BY 1
                INTO TEMP sie_tot_ctas

              SELECT COUNT(UNIQUE a.nss)
                INTO tot_ctas_vol
                FROM fmto_a a
               WHERE a.grupo_regimen IN (2,3,4,9)

              FOR i = 1 TO 6
                  SELECT COUNT(*)
                    INTO arr_cta_siefore[i]
                    FROM sie_tot_ctas
                   WHERE total = i
              END FOR

              LET total_cuentas_inv = (arr_cta_siefore[1] + arr_cta_siefore[2] +
                                       arr_cta_siefore[3] + arr_cta_siefore[4] +
                                       arr_cta_siefore[5] )

              PRINT "  TOTAL   AFORE", 
                    COLUMN 20, "Cuentas", 
                    COLUMN 30,  SUM(cta_igual.rcv_imss + cta_dif.rcv_imss)       USING "#,###,###,###,###,###,##&",
                    COLUMN 55,  SUM(cta_igual.sar92 + cta_dif.sar92)             USING "#,###,###,###,###,###,##&",
                    COLUMN 80,  SUM(cta_igual.rcv_issste + cta_dif.rcv_issste)   USING "#,###,###,###,###,###,##&",
                    COLUMN 105, SUM(cta_igual.ahorro_soli + cta_dif.ahorro_soli) USING "#,###,###,###,###,###,##&",
                    COLUMN 130, SUM(cta_igual.aar + cta_dif.aar)                 USING "#,###,###,###,###,###,##&",
                    COLUMN 155, SUM(cta_igual.avol + cta_dif.avol)               USING "#,###,###,###,###,###,##&",
                    COLUMN 180, SUM(cta_igual.acr + cta_dif.acr)                 USING "#,###,###,###,###,###,##&",
                    COLUMN 205, SUM(cta_igual.alp + cta_dif.alp)                 USING "#,###,###,###,###,###,##&",
                    COLUMN 230, SUM(cta_igual.aplp + cta_dif.aplp)               USING "#,###,###,###,###,###,##&",
                    COLUMN 255, total_cuentas                                    USING "#,###,###,###,###,###,##&" 

              PRINT
                    COLUMN 20, "Titulos", 
                    COLUMN 30,  SUM(acc_igual.rcv_imss + acc_dif.rcv_imss)       USING "##,###,###,###,###.######",
                    COLUMN 55,  SUM(acc_igual.sar92 + acc_dif.sar92)             USING "##,###,###,###,###.######",
                    COLUMN 80,  SUM(acc_igual.rcv_issste + acc_dif.rcv_issste)   USING "##,###,###,###,###.######",
                    COLUMN 105, SUM(acc_igual.ahorro_soli + acc_dif.ahorro_soli) USING "##,###,###,###,###.######",
                    COLUMN 130, SUM(acc_igual.aar + acc_dif.aar)                 USING "##,###,###,###,###.######",
                    COLUMN 155, SUM(acc_igual.avol + acc_dif.avol)               USING "##,###,###,###,###.######",
                    COLUMN 180, SUM(acc_igual.acr + acc_dif.acr)                 USING "##,###,###,###,###.######",
                    COLUMN 205, SUM(acc_igual.alp + acc_dif.alp)                 USING "##,###,###,###,###.######",
                    COLUMN 230, SUM(acc_igual.aplp + acc_dif.aplp)               USING "##,###,###,###,###.######",
                    COLUMN 255, total_acciones                                   USING "##,###,###,###,###.######"
               PRINT
               PRINT
               PRINT "     TOTAL DE CUENTAS CON INVERSION EN:",
                    COLUMN 45,  "1 SIEFORE",
                    COLUMN 60,  "2 SIEFORES",
                    COLUMN 75,  "3 SIEFORES",
                    COLUMN 90,  "4 SIEFORES",
                    COLUMN 105, "5 SIEFORES",
                    COLUMN 120, "TOTAL DE CUENTAS"
               PRINT
                    COLUMN 44,  arr_cta_siefore[1] USING "###,###,##&",
                    COLUMN 59,  arr_cta_siefore[2] USING "###,###,##&",
                    COLUMN 74,  arr_cta_siefore[3] USING "###,###,##&",
                    COLUMN 89,  arr_cta_siefore[4] USING "###,###,##&",
                    COLUMN 104, arr_cta_siefore[5] USING "###,###,##&",
                    COLUMN 119, total_cuentas_inv  USING "###,###,##&"
               PRINT
               PRINT "TOTAL DE CUENTAS CON AHORRO VOLUNTARIO:", tot_ctas_vol USING "###,###,##&"
               PRINT
               PRINT
               PRINT COLUMN 70, cadena_desc
               PRINT COLUMN 70, cadena_precio
END REPORT


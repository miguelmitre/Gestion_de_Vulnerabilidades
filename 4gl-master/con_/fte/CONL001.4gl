##############################################################################
#Proyecto          => AFORE ( MEXICO )                                       #
#Propietario       => E.F.P.                                                 #
#Programa CONL001  => CARGA REGISTROS RETIROS TOTALES PARA REPORTE SAT - VOL #
#Fecha creacion    => 23 DE FEBRERO DE 2010                                  #
#Autor             => MAURO MUNIZ CABALLERO                                  #
#Sistema           => CON                                                    #
##############################################################################

DATABASE safre_af

GLOBALS

    DEFINE hoy              DATE
    DEFINE vfecha_ini       DATE
    DEFINE vfecha_fin       DATE
    DEFINE maxfecha_liq     DATE
    DEFINE vfprom_ini       DATE
    DEFINE vfprom_fin       DATE
    DEFINE pfecha_fin       DATE

    DEFINE vperiodo         SMALLINT
    DEFINE vsubcta          SMALLINT
    DEFINE vgpo_subcta      SMALLINT
    DEFINE vsubcuenta       SMALLINT

    DEFINE i                SMALLINT
    DEFINE j                SMALLINT

    DEFINE vprecio_ini      DECIMAl(16,6)
    DEFINE vprecio_fin      DECIMAL(16,6)
    DEFINE rend_real_tot    DECIMAL(16,6)
    DEFINE tasa_real_tot    DECIMAL(16,6)
    DEFINE vrend_nominal    DECIMAL(16,6)
    DEFINE vmto_retencion   DECIMAL(16,6)
    DEFINE iprecio_ini      DECIMAL(16,6)
    DEFINE iprecio_fin      DECIMAL(16,6)
    DEFINE factor_infla     DECIMAL(16,6)
    DEFINE infla_per        DECIMAL(16,6)
    DEFINE mac              DECIMAL(16,6)
    DEFINE ren_real         DECIMAL(16,6)
    DEFINE total_accion     DECIMAL(16,6)
    DEFINE importe_ini      DECIMAL(16,6)
    DEFINE importe_fin      DECIMAL(16,6)
    DEFINE vsaldo_promedio  DECIMAL(18,6)

    DEFINE enter            CHAR(1)
    DEFINE eje_sdo          CHAR(300)
    DEFINE sel_vol          CHAR(1000)
    DEFINE sel_his          CHAR(2000)

    DEFINE reg_reten_vol RECORD
        nss                 CHAR(11),
        interes_real        DECIMAL(18,2),
        monto_retenido      DECIMAL(18,2),
        interes_nominal     DECIMAL(18,2),
        sdo_promedio1       DECIMAL(18,2),
        sdo_promedio2       DECIMAL(18,2),
        sdo_promedio3       DECIMAL(18,2),
        sdo_promedio4       DECIMAL(18,2),
        sdo_promedio5       DECIMAL(18,2),
        sdo_promedio6       DECIMAL(18,2),
        sdo_promedio7       DECIMAL(18,2),
        sdo_promedio8       DECIMAL(18,2),
        sdo_promedio9       DECIMAL(18,2),
        sdo_promedio10      DECIMAL(18,2),
        sdo_promedio11      DECIMAL(18,2),
        sdo_promedio12      DECIMAL(18,2)
    END RECORD

    DEFINE gr_prom RECORD
        fecha_apo           DATE,
        fecha_liq           DATE,
        mto_neto            DECIMAL(16,2),
        mto_retencion       DECIMAL(16,2),
        mto_rendimiento     DECIMAL(16,2)
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

    CALL STARTLOG("CONL001.log")
    CALL inicio()
    CALL solicita_parametros()

END MAIN

FUNCTION inicio()
#i---------------

    LET hoy      = today
    LET vperiodo = YEAR(hoy) - 1

    LET vsubcta         = 0
    LET vgpo_subcta     = 3
    LET vsaldo_promedio = 0

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_reten_vol
        DROP TABLE tmp_nss_ret_vol
    WHENEVER ERROR STOP

    CREATE TABLE tmp_reten_vol
        (nss                CHAR(11),
         interes_real       DECIMAL(18,2),
         monto_retenido     DECIMAL(18,2),
         interes_nominal    DECIMAL(18,2),
         sdo_promedio1      DECIMAL(18,2),
         sdo_promedio2      DECIMAL(18,2),
         sdo_promedio3      DECIMAL(18,2),
         sdo_promedio4      DECIMAL(18,2),
         sdo_promedio5      DECIMAL(18,2),
         sdo_promedio6      DECIMAL(18,2),
         sdo_promedio7      DECIMAL(18,2),
         sdo_promedio8      DECIMAL(18,2),
         sdo_promedio9      DECIMAL(18,2),
         sdo_promedio10     DECIMAL(18,2),
         sdo_promedio11     DECIMAL(18,2),
         sdo_promedio12     DECIMAL(18,2))

    DATABASE safre_af

    INITIALIZE reg_reten_vol.* TO NULL

    LET sel_vol = " SELECT a.fecha_aporte, ",
                         " a.fecha_liquidacion, ",
                         " a.mto_neto * -1, ",
                         " a.mto_retencion * -1, ",
                         " a.mto_rendimiento ",
                  " FROM   safre_af:ret_pago_vol a ",
                  " WHERE  a.nss   = ? ",
                  " AND    a.fecha_liquidacion BETWEEN ? AND ? "

   PREPARE eje_sel_vol  FROM  sel_vol

   LET eje_sdo = " EXECUTE PROCEDURE fn_saldo_dia_his (?,?,?,?)"

END FUNCTION

FUNCTION solicita_parametros()
#sp---------------------------

    OPEN WINDOW CONL0011 AT 2,2 WITH FORM "CONL0021" ATTRIBUTE(BORDER)
    DISPLAY "    <Enter> Confirmar Periodo     <Esc> Ejecutar Proceso     <Ctrl-C> Salir    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONL001   CARGA REGISTROS CON RETENCION PARA SAT (VOLUNTARIAS)                " AT 3,1 ATTRIBUTE(REVERSE)
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

    SELECT UNIQUE t.nss
      FROM safre_af:ret_pago_vol t
     WHERE t.fecha_liquidacion between vfecha_ini and vfecha_fin
    INTO TEMP tmp_nss_ret_vol

    CREATE INDEX tmp_nss_ret_vol1 ON tmp_nss_ret_vol(nss)
    UPDATE STATISTICS FOR TABLE tmp_nss_ret_vol

    LET sel_his = ""

    DECLARE cur_ret_vol CURSOR FOR
    SELECT nss
      FROM tmp_nss_ret_vol

    FOREACH cur_ret_vol INTO reg_reten_vol.nss

    LET rend_real_tot  = 0
    LET tasa_real_tot  = 0
    LET vrend_nominal  = 0
    LET vmto_retencion = 0
    LET i              = 1

        SELECT MAX(m.fecha_liquidacion)
          INTO maxfecha_liq
          FROM safre_af:ret_pago_vol m
         WHERE m.nss = reg_reten_vol.nss

        DECLARE cur_mov CURSOR FOR eje_sel_vol

        FOREACH cur_mov USING reg_reten_vol.nss,
                              vfecha_ini,
                              vfecha_fin
                         INTO gr_prom.*

            LET iprecio_fin  = 0
            LET iprecio_ini  = 0
            LET vprecio_fin  = 0
            LET vprecio_ini  = 0
            LET factor_infla = 0
            LET infla_per    = 0
            LET mac          = 0
            LET ren_real     = 0
            LET total_accion = 0
            LET importe_ini  = 0
            LET importe_fin  = 0

            IF gr_prom.fecha_apo < "01/14/2005" THEN
                LET vprecio_ini = precio_siefore (2,gr_prom.fecha_apo)
            ELSE
                LET vprecio_ini = precio_siefore (1,gr_prom.fecha_apo)
            END IF

            LET vprecio_fin = precio_siefore (1,gr_prom.fecha_liq)

            SELECT precio_del_dia
              INTO iprecio_ini
              FROM glo_valor_accion
             WHERE fecha_valuacion = gr_prom.fecha_apo
               AND codigo_siefore = 13

            SELECT precio_del_dia
              INTO iprecio_fin
              FROM glo_valor_accion
             WHERE fecha_valuacion = gr_prom.fecha_liq
               AND codigo_siefore = 13

            LET total_accion = (gr_prom.mto_neto + gr_prom.mto_retencion) / vprecio_fin
            LET importe_ini  = total_accion * vprecio_ini
            LET importe_fin  = total_accion * vprecio_fin
            LET factor_infla = (iprecio_fin / iprecio_ini) -1

            LET gr_prom.mto_rendimiento = 0
            LET gr_prom.mto_rendimiento = importe_fin - importe_ini

            LET mac = (gr_prom.mto_neto + gr_prom.mto_retencion) - gr_prom.mto_rendimiento

            LET infla_per      = factor_infla * mac
            LET ren_real       = gr_prom.mto_rendimiento - infla_per
            LET rend_real_tot  = rend_real_tot + ren_real
            LET vrend_nominal  = vrend_nominal + gr_prom.mto_rendimiento
            LET vmto_retencion = vmto_retencion + gr_prom.mto_retencion

            INITIALIZE gr_prom.* TO NULL

        END FOREACH

        LET reg_reten_vol.interes_real    = rend_real_tot
        LET reg_reten_vol.monto_retenido  = vmto_retencion
        LET reg_reten_vol.interes_nominal = vrend_nominal

        CALL obtiene_fin_mes(maxfecha_liq) RETURNING pfecha_fin

        LET vfprom_ini = MDY(MONTH(pfecha_fin) + 1, 1, YEAR(pfecha_fin) - 1)

        CALL obtiene_fin_mes(vfprom_ini) RETURNING vfprom_fin

        WHILE vfprom_fin <= pfecha_fin
            CALL obtiene_saldo(reg_reten_vol.nss, vfprom_ini, vfprom_fin)
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

        LET reg_reten_vol.sdo_promedio1  = arr_mes[1].sdo_mes
        LET reg_reten_vol.sdo_promedio2  = arr_mes[2].sdo_mes
        LET reg_reten_vol.sdo_promedio3  = arr_mes[3].sdo_mes
        LET reg_reten_vol.sdo_promedio4  = arr_mes[4].sdo_mes
        LET reg_reten_vol.sdo_promedio5  = arr_mes[5].sdo_mes
        LET reg_reten_vol.sdo_promedio6  = arr_mes[6].sdo_mes
        LET reg_reten_vol.sdo_promedio7  = arr_mes[7].sdo_mes
        LET reg_reten_vol.sdo_promedio8  = arr_mes[8].sdo_mes
        LET reg_reten_vol.sdo_promedio9  = arr_mes[9].sdo_mes
        LET reg_reten_vol.sdo_promedio10 = arr_mes[10].sdo_mes
        LET reg_reten_vol.sdo_promedio11 = arr_mes[11].sdo_mes
        LET reg_reten_vol.sdo_promedio12 = arr_mes[12].sdo_mes

        INSERT INTO safre_tmp:tmp_reten_vol VALUES (reg_reten_vol.*)

        INITIALIZE reg_reten_vol.* TO NULL

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

FUNCTION obtiene_saldo(vnss, pfprom_ini, pfprom_fin)
#os-------------------------------------------------

    DEFINE vnss         CHAR(11)

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


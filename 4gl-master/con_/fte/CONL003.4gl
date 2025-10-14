##############################################################################
#Proyecto          => AFORE ( MEXICO )                                       #
#Propietario       => E.F.P.                                                 #
#Programa CONL003  => GENERA ARCHIVO PARA REPORTE SAT (VOL, RCV y SAR)       #
#Fecha creacion    => 23 DE FEBRERO DE 2010                                  #
#Autor             => MAURO MUNIZ CABALLERO                                  #
#Sistema           => CON                                                    #
##############################################################################

DATABASE safre_af

GLOBALS

    DEFINE hoy                   DATE
    DEFINE vfecha_ini            DATE
    DEFINE vfecha_fin            DATE

    DEFINE vperiodo              SMALLINT

    DEFINE cont1                 INTEGER
    DEFINE cont7                 INTEGER
    DEFINE contt                 INTEGER
    DEFINE cont2                 INTEGER
    DEFINE cont3                 INTEGER
    DEFINE tot_reg_detalle       INTEGER

    DEFINE enter                 CHAR(1)
    DEFINE vhora                 CHAR(8)
    DEFINE vusuario              CHAR(8)
    DEFINE varchivo              CHAR(200)
    DEFINE varch_r               CHAR(200)
    DEFINE varch_d               CHAR(200)
    DEFINE cat                   CHAR(300)

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE rpt_retencion RECORD
        cont_retencion        INTEGER,
        grupo_regimen         SMALLINT,
        rfc                   CHAR(13),
        curp                  CHAR(18),
        nombres               CHAR(50),
        paterno               CHAR(50),
        materno               CHAR(50),
        nss                   CHAR(11),
        int_nominal_gravado   DECIMAL(16,2),
        int_nominal_exento    DECIMAL(16,2),
        int_nominal_total     DECIMAL(16,2),
        ajuste_inflacion      DECIMAL(16,2),
        ajuste_deflacion      DECIMAL(16,2),
        interes_real_gravado  DECIMAL(16,2),
        perdida               DECIMAL(18,2),
        isr_retenido          DECIMAL(18,2),
        sdo_promedio1         DECIMAL(18,2),
        sdo_promedio2         DECIMAL(18,2),
        sdo_promedio3         DECIMAL(18,2),
        sdo_promedio4         DECIMAL(18,2),
        sdo_promedio5         DECIMAL(18,2),
        sdo_promedio6         DECIMAL(18,2),
        sdo_promedio7         DECIMAL(18,2),
        sdo_promedio8         DECIMAL(18,2),
        sdo_promedio9         DECIMAL(18,2),
        sdo_promedio10        DECIMAL(18,2),
        sdo_promedio11        DECIMAL(18,2),
        sdo_promedio12        DECIMAL(18,2)
    END RECORD

    DEFINE rpt_domicilio RECORD
        cont_domicilio        INTEGER,
        cont_retencion        INTEGER,
        rfc                   CHAR(13),
        calle                 CHAR(130),
        numero_ext            CHAR(12),
        numero_int            CHAR(12),
        colonia               CHAR(65),
        cp                    CHAR(6),
        municipio             CHAR(50),
        ent_federativa        CHAR(20)
    END RECORD

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST

    CALL STARTLOG("CONL003.log")
    CALL inicio()
    CALL solicita_parametros()

END MAIN

FUNCTION inicio()
#i---------------

    LET hoy      = today
    LET vhora    = time
    LET vperiodo = YEAR(hoy) - 1

    LET vhora    = vhora[1,2],vhora[4,5],vhora[7,8]

    LET cont2 = 2
    LET cont3 = cont2 + 1

    SELECT count(*)
      INTO cont1
      FROM safre_tmp:tmp_retencion

    SELECT count(*)
      INTO cont7
      FROM safre_tmp:tmp_reten_vol

    LET contt = cont1 + cont7

    SELECT m.*, USER
      INTO g_seg_modulo.*, vusuario
      FROM safre_af:seg_modulo m
     WHERE m.modulo_cod = "con"

    LET varch_r  = g_seg_modulo.ruta_envio CLIPPED,"/retencion"
    LET varchivo = g_seg_modulo.ruta_envio CLIPPED,"/",vusuario CLIPPED, ".DECLARACION_SAT-",vperiodo,".", hoy USING "YYMMDD"

    INITIALIZE rpt_retencion.* TO NULL

END FUNCTION

FUNCTION solicita_parametros()
#sp---------------------------

    OPEN WINDOW CONL0031 AT 2,2 WITH FORM "CONL0021" ATTRIBUTE(BORDER)
    DISPLAY "    <Enter> Confirmar Periodo     <Esc> Ejecutar Proceso     <Ctrl-C> Salir    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONL003        GENERA REPORTE RETENCION SAT -VOL, RCV, SAR-                   " AT 3,1 ATTRIBUTE(REVERSE)
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

    START REPORT listado_retencion TO varch_r

    DECLARE cur_ret_1 CURSOR FOR
    SELECT r.grupo_regimen,
           DECODE(a.tipo_solicitud,5,'XAXX010101000',a.n_rfc),
           a.n_unico,
           a.nombres,
           a.paterno,
           a.materno,
           r.nss,
           r.monto_gravable,
           r.monto_no_gravable,
           r.monto_total,
           0,
           0,
           r.monto_gravable,
           0,
           r.monto_retenido,
           r.sdo_promedio1,
           r.sdo_promedio2,
           r.sdo_promedio3,
           r.sdo_promedio4,
           r.sdo_promedio5,
           r.sdo_promedio6,
           r.sdo_promedio7,
           r.sdo_promedio8,
           r.sdo_promedio9,
           r.sdo_promedio10,
           r.sdo_promedio11,
           r.sdo_promedio12
      FROM safre_af:afi_mae_afiliado a,
           safre_tmp:tmp_retencion r
     WHERE a.n_seguro = r.nss

    FOREACH cur_ret_1 INTO rpt_retencion.grupo_regimen,
                           rpt_retencion.rfc,
                           rpt_retencion.curp,
                           rpt_retencion.nombres,
                           rpt_retencion.paterno,
                           rpt_retencion.materno,
                           rpt_retencion.nss,
                           rpt_retencion.int_nominal_gravado,
                           rpt_retencion.int_nominal_exento,
                           rpt_retencion.int_nominal_total,
                           rpt_retencion.ajuste_inflacion,
                           rpt_retencion.ajuste_deflacion,
                           rpt_retencion.interes_real_gravado,
                           rpt_retencion.perdida,
                           rpt_retencion.isr_retenido,
                           rpt_retencion.sdo_promedio1,
                           rpt_retencion.sdo_promedio2,
                           rpt_retencion.sdo_promedio3,
                           rpt_retencion.sdo_promedio4,
                           rpt_retencion.sdo_promedio5,
                           rpt_retencion.sdo_promedio6,
                           rpt_retencion.sdo_promedio7,
                           rpt_retencion.sdo_promedio8,
                           rpt_retencion.sdo_promedio9,
                           rpt_retencion.sdo_promedio10,
                           rpt_retencion.sdo_promedio11,
                           rpt_retencion.sdo_promedio12

        IF rpt_retencion.materno IS NULL OR
           rpt_retencion.materno MATCHES " *" THEN
            LET rpt_retencion.materno = "N/A"
        END IF

        LET rpt_retencion.cont_retencion = cont2
--arc 140411
        DECLARE cur_dato CURSOR FOR
        SELECT a.calle       ,
               a.numero      ,
               a.depto       ,
               a.colonia     ,
               a.codpos      ,
               b.deleg_desc  ,
               d.estad_desc
          FROM safre_tmp:tmp_retencion t,
               safre_af:afi_mae_afiliado f,
               safre_af:afi_domicilio  a,
         OUTER safre_af:tab_delegacion b,
         OUTER safre_af:tab_estado     d
         WHERE t.nss            = rpt_retencion.nss
           AND t.nss            = f.n_seguro
           AND t.nss            = a.nss
           AND a.n_folio        = f.n_folio
           AND a.tipo_solicitud = f.tipo_solicitud
           AND b.deleg_cod      = a.delega
           AND d.estad_cod      = a.estado
           AND a.marca_envio    = 'X'
           AND t.grupo_regimen  = rpt_retencion.grupo_regimen
        FOREACH cur_dato INTO rpt_domicilio.calle,
                              rpt_domicilio.numero_ext,
                              rpt_domicilio.numero_int,
                              rpt_domicilio.colonia,
                              rpt_domicilio.cp,
                              rpt_domicilio.municipio,
                              rpt_domicilio.ent_federativa
            EXIT FOREACH
        END FOREACH

        IF LENGTH (rpt_domicilio.numero_ext) = 1 THEN
            LET rpt_domicilio.numero_ext = " ",rpt_domicilio.numero_ext CLIPPED
        END IF

        IF rpt_domicilio.calle IS NULL OR
           rpt_domicilio.calle MATCHES " *" THEN
            LET rpt_domicilio.calle          = "INSURGENTES SUR"
            LET rpt_domicilio.numero_ext     = "553"
            LET rpt_domicilio.numero_int     = "6-603"
            LET rpt_domicilio.colonia        = "ESCANDON"
            LET rpt_domicilio.cp             = "11800"
            LET rpt_domicilio.municipio      = "MIGUEL HIDALGO"
            LET rpt_domicilio.ent_federativa = "DISTRITO FEDERAL"
        END IF

        LET rpt_domicilio.cont_domicilio = cont3
        LET rpt_domicilio.cont_retencion = rpt_retencion.cont_retencion

        OUTPUT TO REPORT listado_retencion (rpt_retencion.*, rpt_domicilio.*)

        INITIALIZE rpt_domicilio.* TO NULL

        LET cont2 = cont3 + 1
        LET cont3 = cont2 + 1

    END FOREACH

    INITIALIZE rpt_retencion.* TO NULL

    DECLARE cur_ret_3 CURSOR FOR
    SELECT 3,
           DECODE(a.tipo_solicitud,5,'XAXX010101000',a.n_rfc),
           a.n_unico,
           a.nombres,
           a.paterno,
           a.materno,
           r.nss,
           r.interes_nominal,
           0,
           r.interes_nominal,
           0,
           0,
           r.interes_real,
           0,
           r.monto_retenido,
           r.sdo_promedio1,
           r.sdo_promedio2,
           r.sdo_promedio3,
           r.sdo_promedio4,
           r.sdo_promedio5,
           r.sdo_promedio6,
           r.sdo_promedio7,
           r.sdo_promedio8,
           r.sdo_promedio9,
           r.sdo_promedio10,
           r.sdo_promedio11,
           r.sdo_promedio12
      FROM safre_af:afi_mae_afiliado a,
           safre_tmp:tmp_reten_vol r
     WHERE a.n_seguro = r.nss

    FOREACH cur_ret_3 INTO rpt_retencion.grupo_regimen,
                           rpt_retencion.rfc,
                           rpt_retencion.curp,
                           rpt_retencion.nombres,
                           rpt_retencion.paterno,
                           rpt_retencion.materno,
                           rpt_retencion.nss,
                           rpt_retencion.int_nominal_gravado,
                           rpt_retencion.int_nominal_exento,
                           rpt_retencion.int_nominal_total,
                           rpt_retencion.ajuste_inflacion,
                           rpt_retencion.ajuste_deflacion,
                           rpt_retencion.interes_real_gravado,
                           rpt_retencion.perdida,
                           rpt_retencion.isr_retenido,
                           rpt_retencion.sdo_promedio1,
                           rpt_retencion.sdo_promedio2,
                           rpt_retencion.sdo_promedio3,
                           rpt_retencion.sdo_promedio4,
                           rpt_retencion.sdo_promedio5,
                           rpt_retencion.sdo_promedio6,
                           rpt_retencion.sdo_promedio7,
                           rpt_retencion.sdo_promedio8,
                           rpt_retencion.sdo_promedio9,
                           rpt_retencion.sdo_promedio10,
                           rpt_retencion.sdo_promedio11,
                           rpt_retencion.sdo_promedio12

        IF rpt_retencion.materno IS NULL OR
           rpt_retencion.materno MATCHES " *" THEN
            LET rpt_retencion.materno = "N/A"
        END IF

        LET rpt_retencion.cont_retencion = cont2

        SELECT a.calle       ,
               a.numero      ,
               a.depto       ,
               a.colonia     ,
               a.codpos      ,
               b.deleg_desc  ,
               d.estad_desc
          INTO rpt_domicilio.calle,
               rpt_domicilio.numero_ext,
               rpt_domicilio.numero_int,
               rpt_domicilio.colonia,
               rpt_domicilio.cp,
               rpt_domicilio.municipio,
               rpt_domicilio.ent_federativa
          FROM safre_tmp:tmp_reten_vol t,
               safre_af:afi_mae_afiliado f,
               safre_af:afi_domicilio  a,
         OUTER safre_af:tab_delegacion b,
         OUTER safre_af:tab_estado     d
         WHERE t.nss            = rpt_retencion.nss
           AND t.nss            = f.n_seguro
           AND t.nss            = a.nss
           AND a.n_folio        = f.n_folio
           AND a.tipo_solicitud = f.tipo_solicitud
           AND b.deleg_cod      = a.delega
           AND d.estad_cod      = a.estado
           AND a.marca_envio    = 'X'

        IF LENGTH (rpt_domicilio.numero_ext) = 1 THEN
            LET rpt_domicilio.numero_ext = " ",rpt_domicilio.numero_ext CLIPPED
        END IF

        IF rpt_domicilio.calle IS NULL OR
           rpt_domicilio.calle MATCHES " *" THEN
            LET rpt_domicilio.calle          = "INSURGENTES SUR"
            LET rpt_domicilio.numero_ext     = "553"
            LET rpt_domicilio.numero_int     = "6-603"
            LET rpt_domicilio.colonia        = "ESCANDON"
            LET rpt_domicilio.cp             = "11800"
            LET rpt_domicilio.municipio      = "MIGUEL HIDALGO"
            LET rpt_domicilio.ent_federativa = "DISTRITO FEDERAL"
        END IF

        LET rpt_domicilio.cont_domicilio = cont3
        LET rpt_domicilio.cont_retencion = rpt_retencion.cont_retencion

        OUTPUT TO REPORT listado_retencion (rpt_retencion.*, rpt_domicilio.*)

        INITIALIZE rpt_domicilio.* TO NULL

        LET cont2 = cont3 + 1
        LET cont3 = cont2 + 1

    END FOREACH

    FINISH REPORT listado_retencion

    CALL limpia_blancos(varch_r)

    LET cat = "cat ",g_seg_modulo.ruta_envio CLIPPED,"/retencion > ",
                     g_seg_modulo.ruta_envio CLIPPED,"/",
                     vusuario CLIPPED, ".DECLARACION_SAT-",vperiodo USING "&&&&",".", hoy USING "YYMMDD"

    RUN cat

    ERROR ""
    PROMPT "PROCESO TERMINADO, [Enter] PARA SALIR" FOR enter

END FUNCTION

REPORT listado_retencion (rpt_retencion, rpt_domicilio)
#rlg1--------------------------------------------------

    DEFINE rpt_retencion RECORD
        cont_retencion        INTEGER,
        grupo_regimen         SMALLINT,
        rfc                   CHAR(13),
        curp                  CHAR(18),
        nombres               CHAR(50),
        paterno               CHAR(50),
        materno               CHAR(50),
        nss                   CHAR(11),
        int_nominal_gravado   DECIMAL(16,2),
        int_nominal_exento    DECIMAL(16,2),
        int_nominal_total     DECIMAL(16,2),
        ajuste_inflacion      DECIMAL(16,2),
        ajuste_deflacion      DECIMAL(16,2),
        interes_real_gravado  DECIMAL(16,2),
        perdida               DECIMAL(18,2),
        isr_retenido          DECIMAL(18,2),
        sdo_promedio1         DECIMAL(18,2),
        sdo_promedio2         DECIMAL(18,2),
        sdo_promedio3         DECIMAL(18,2),
        sdo_promedio4         DECIMAL(18,2),
        sdo_promedio5         DECIMAL(18,2),
        sdo_promedio6         DECIMAL(18,2),
        sdo_promedio7         DECIMAL(18,2),
        sdo_promedio8         DECIMAL(18,2),
        sdo_promedio9         DECIMAL(18,2),
        sdo_promedio10        DECIMAL(18,2),
        sdo_promedio11        DECIMAL(18,2),
        sdo_promedio12        DECIMAL(18,2)
    END RECORD

    DEFINE rpt_domicilio RECORD
        cont_domicilio        INTEGER,
        cont_retencion        INTEGER,
        rfc                   CHAR(13),
        calle                 CHAR(130),
        numero_ext            CHAR(12),
        numero_int            CHAR(12),
        colonia               CHAR(65),
        cp                    CHAR(6),
        municipio             CHAR(50),
        ent_federativa        CHAR(20)
    END RECORD

    DEFINE v_total_dif        DECIMAL(16,6)

    DEFINE cve_afore          SMALLINT

    OUTPUT
    PAGE LENGTH    3
    LEFT MARGIN    0
    RIGHT MARGIN   0
    TOP MARGIN     0
    BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER

        SELECT c.codigo_afore
          INTO cve_afore
          FROM safre_af:tab_afore_local c

        PRINT COLUMN 01, "100000001",
              COLUMN 10, cve_afore USING "&&&",
              COLUMN 13, hoy USING "YYYYMMDD",
              COLUMN 21, vperiodo USING "&&&&",
              COLUMN 25, "ACO051202NK7",
              COLUMN 37, "0",
              COLUMN 38, 567 SPACES

        ON EVERY ROW

        LET v_total_dif           = 0
        LET rpt_retencion.perdida = 0

        IF rpt_retencion.grupo_regimen = 3 THEN
            LET v_total_dif = rpt_retencion.int_nominal_gravado - rpt_retencion.interes_real_gravado

            IF v_total_dif < 0 THEN
                LET rpt_retencion.ajuste_deflacion = v_total_dif
            ELSE
                LET rpt_retencion.ajuste_inflacion = v_total_dif
            END IF

            IF rpt_retencion.interes_real_gravado < 0 THEN
                LET rpt_retencion.perdida = rpt_retencion.interes_real_gravado
            END IF
        END IF

        PRINT COLUMN 001,"2",
              COLUMN 002,rpt_retencion.cont_retencion USING "&&&&&&&&",
              COLUMN 010,"1",
              COLUMN 011,rpt_retencion.rfc,
              COLUMN 024,rpt_retencion.curp,
              COLUMN 042,rpt_retencion.nombres,
              COLUMN 092,rpt_retencion.paterno,
              COLUMN 142,rpt_retencion.materno,
              COLUMN 192,"F",
              COLUMN 193,rpt_retencion.nss,
              COLUMN 233,rpt_retencion.int_nominal_gravado  USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 252,rpt_retencion.int_nominal_exento   USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 271,rpt_retencion.int_nominal_total    USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 290,rpt_retencion.ajuste_inflacion     USING "&&&&&&&&&&&&&&.&&",
              COLUMN 307,rpt_retencion.ajuste_deflacion     USING "&&&&&&&&&&&&&&.&&",
              COLUMN 324,rpt_retencion.interes_real_gravado USING "&&&&&&&&&&&&&&.&&",
              COLUMN 341,rpt_retencion.perdida              USING "&&&&&&&&&&&&&&.&&",
              COLUMN 358,rpt_retencion.isr_retenido         USING "&&&&&&&&&&&&&&.&&",
              COLUMN 375,rpt_retencion.sdo_promedio1        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 394,rpt_retencion.sdo_promedio2        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 413,rpt_retencion.sdo_promedio3        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 432,rpt_retencion.sdo_promedio4        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 451,rpt_retencion.sdo_promedio5        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 470,rpt_retencion.sdo_promedio6        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 489,rpt_retencion.sdo_promedio7        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 508,rpt_retencion.sdo_promedio8        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 527,rpt_retencion.sdo_promedio9        USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 546,rpt_retencion.sdo_promedio10       USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 565,rpt_retencion.sdo_promedio11       USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 584,rpt_retencion.sdo_promedio12       USING "-&&&&&&&&&&&&&&&.&&",
              COLUMN 603,"N "

        PRINT COLUMN 001, "3",
              COLUMN 002, rpt_domicilio.cont_domicilio USING "&&&&&&&&",
              COLUMN 010, rpt_domicilio.cont_retencion USING "&&&&&&&&",
              COLUMN 018, rpt_retencion.nss,
              COLUMN 058, rpt_retencion.rfc,
              COLUMN 071, rpt_domicilio.calle,
              COLUMN 201, rpt_domicilio.numero_ext,
              COLUMN 213, rpt_domicilio.numero_int,
              COLUMN 225, rpt_domicilio.colonia,
              COLUMN 290, rpt_domicilio.cp,
              COLUMN 296, rpt_domicilio.municipio,
              COLUMN 346, rpt_domicilio.ent_federativa,
              COLUMN 366, 239 SPACES

    ON LAST ROW
        LET cont3 = rpt_domicilio.cont_domicilio + 1

        PRINT COLUMN 01, "9",
              COLUMN 02, cont3 USING "&&&&&&&&",
              COLUMN 10, contt USING "&&&&&&&&",
              COLUMN 18, "1",
              COLUMN 19, 586 SPACES

END REPORT

FUNCTION limpia_blancos(parchivo)
#flb-----------------------------

    DEFINE parchivo  CHAR(200)
    DEFINE v_comando CHAR(500)

    LET v_comando = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ",parchivo CLIPPED,
               " > ",vusuario CLIPPED,".limpia"

    RUN v_comando

    LET v_comando = "cd ",g_seg_modulo.ruta_envio CLIPPED,
               "; mv ",vusuario CLIPPED,".limpia ",parchivo 

    RUN v_comando

END FUNCTION

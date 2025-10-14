###############################################################################
#Proyecto          => SISTEMA DE AFORES  ( MEXICO )                           #
#Propietario       => E.F.P.                                                  #
#Programa TAAB018  => GENERA ARCHIVO INVERSION RECURSOS TRASPASOS RCV OP. 36  #
#                  => AFORE RECEPTORA                                         #
#Por               => MAURO MUNIZ CABALLERO                                   #
#Fecha creacion    => 29 DE NOVIEMBRE DE 2004                                 #
#Sistema           => TAA                                                     #
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_inv_rcv RECORD
        tipo_registro      CHAR(02),
        ident_servicio     CHAR(02),
        ident_operacion    CHAR(02),
        tipo_ent_origen    CHAR(02),
        cve_ent_origen     CHAR(03),
        tipo_ent_destino   CHAR(02),
        cve_ent_destino    CHAR(03),
        ent_fed_envio_lote CHAR(03),
        fecha_presentacion CHAR(08),
        consec_lote_dia    SMALLINT,
        cve_mod_recepcion  CHAR(02),
        cod_result_operac  CHAR(02),
        mot_rechazo_lote   CHAR(09)
    END RECORD

    DEFINE reg_det_inv_rcv RECORD
        tipo_registro       CHAR(2),
        cont_servicio       DECIMAL(10,0),
        tipo_ent_recep      CHAR(2),
        cve_ent_recep       CHAR(3),
        tipo_ent_ced        CHAR(2),
        cve_ent_ced         CHAR(3),
        fecha_liquidacion   DATE,
        curp                CHAR(18),
        nss                 CHAR(11),
        rfc                 CHAR(13),
        paterno             CHAR(40),
        materno             CHAR(40),
        nombres             CHAR(40),
        cve_sub_01          CHAR(2),
        cve_sie_01          CHAR(8),
        cve_sub_02          CHAR(2),
        cve_sie_02          CHAR(8),
        cve_sub_03          CHAR(2),
        cve_sie_03          CHAR(8),
        cve_sub_04          CHAR(2),
        cve_sie_04          CHAR(8),
        cve_sub_05          CHAR(2),
        cve_sie_05          CHAR(8),
        cve_sub_06          CHAR(2),
        cve_sie_06          CHAR(8),
        cve_sub_07          CHAR(2),
        cve_sie_07          CHAR(8),
        cve_sub_08          CHAR(2),
        cve_sie_08          CHAR(8),
        cve_sub_09          CHAR(2),
        cve_sie_09          CHAR(8),
        cve_sub_10          CHAR(2),
        cve_sie_10          CHAR(8),
        cve_sub_11          CHAR(2),
        cve_sie_11          CHAR(8),
        cve_sub_12          CHAR(2),
        cve_sie_12          CHAR(8),
        cve_sub_13          CHAR(2),
        cve_sie_13          CHAR(8),
        cve_sub_14          CHAR(2),
        cve_sie_14          CHAR(8),
        cve_sub_15          CHAR(2),
        cve_sie_15          CHAR(8),
        cve_sub_16          CHAR(2),
        cve_sie_16          CHAR(8),
        cve_sub_17          CHAR(2),
        cve_sie_17          CHAR(8),
        cve_sub_18          CHAR(2),
        cve_sie_18          CHAR(8),
        cve_sub_19          CHAR(2),
        cve_sie_19          CHAR(8),
        cve_sub_20          CHAR(2),
        cve_sie_20          CHAR(8),
        cve_sub_21          CHAR(2),
        cve_sie_21          CHAR(8),
        cod_res_op          CHAR(2),
        diag_proc           CHAR(15),
        nss_unificado       CHAR(11)
    END RECORD

    DEFINE reg_sum_inv_rcv RECORD
        tipo_registro    CHAR(02),
        cantidad_reg_det DECIMAL(9,0)
    END RECORD

    DEFINE enter       CHAR(1)
    DEFINE iop         CHAR(2)
    DEFINE g_usuario   CHAR(8)
    DEFINE HORA        CHAR(8)
    DEFINE nom_archivo CHAR(30)
    DEFINE vdesc_sie   CHAR(30)
    DEFINE exe_sql     CHAR(100)
    DEFINE exe_ind     CHAR(100)
    DEFINE exe_sql_1   CHAR(100)
    DEFINE g_cza       CHAR(150)
    DEFINE g_det       CHAR(150)
    DEFINE g_sum       CHAR(150)
    DEFINE comma       CHAR(200)
    DEFINE cat         CHAR(300)

    DEFINE contador    INTEGER

    DEFINE vfolio_09   INTEGER
    DEFINE vfolio_12   INTEGER
    DEFINE HOY           DATE
    DEFINE fecha_envio   DATE
    DEFINE fecha_liquida DATE

    DEFINE g RECORD
        cod_sie SMALLINT,
        des_sie CHAR(30),
        tot_sie INTEGER
    END RECORD

    DEFINE g_param_taa RECORD LIKE seg_modulo.*
    DEFINE w_afore     RECORD LIKE tab_afore_local.*

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("TAAB018.log")
    CALL inicio()             #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW TAAB0181 AT 4,4 WITH FORM "TAAB0181" ATTRIBUTE(BORDER)
    DISPLAY " TAAB018  GENERA ARCHIVO INVERSION RECURSOS RCV (OP. 36)                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < CTRL-C > Salir " AT 1,26
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,62 ATTRIBUTE(REVERSE)

    INPUT BY NAME fecha_liquida, fecha_envio,
                  vfolio_09, vfolio_12 WITHOUT DEFAULTS

        AFTER FIELD fecha_liquida
            IF fecha_liquida IS NULL THEN
                ERROR "La fecha de liquidacion NO puede ser nula"
                NEXT FIELD fecha_liquida
            END IF

        AFTER FIELD fecha_envio
            IF fecha_envio IS NULL THEN
                ERROR "La fecha de envio NO puede ser nula"
                NEXT FIELD fecha_envio
            END IF

        AFTER FIELD vfolio_09
            IF vfolio_09 IS NULL THEN
                NEXT FIELD vfolio_12
            ELSE
                SELECT 'X'
                FROM   taa_rcv_recepcion r
                WHERE  r.folio = vfolio_09
                AND    r.ident_operacion = '09'
                GROUP BY 1

                IF SQLCA.SQLCODE = 0 THEN
                    NEXT FIELD vfolio_12
                ELSE
                    ERROR "El folio NO es del proceso de afore receptora traspaso normal"
                    LET vfolio_09 = ''
                    DISPLAY BY NAME vfolio_09
                    NEXT FIELD vfolio_09
                END IF
            END IF

        AFTER FIELD vfolio_12
            IF vfolio_12 IS NULL THEN
                EXIT INPUT
            ELSE
                SELECT 'X'
                FROM   taa_rcv_recepcion r
                WHERE  r.folio = vfolio_12
                AND    r.ident_operacion = '12'
                GROUP BY 1

                IF SQLCA.SQLCODE <> 0 THEN
                    ERROR "El folio NO es del proceso de afore receptora traspaso compl."
                    LET vfolio_12 = ''
                    DISPLAY BY NAME vfolio_12
                    NEXT FIELD vfolio_12
                END IF
            END IF

        EXIT INPUT

        ON KEY ( INTERRUPT )
            EXIT PROGRAM

    END INPUT

    WHILE TRUE
        PROMPT "ESTA SEGURO S/N ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[nN]" THEN
                DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                EXIT PROGRAM
            END IF
            EXIT WHILE
        ELSE
            DISPLAY "PROCESO INTERRUMPIDO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
            EXIT PROGRAM
            EXIT WHILE
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL genera_archivo_inv()   #ad
    CALL impresion_reporte()

    DISPLAY "                       " AT 19,1
    DISPLAY "Archivo generado : ", g_param_taa.ruta_envio CLIPPED,"/",nom_archivo
    PROMPT "PROCESO FINALIZADO, PRESIONE < ENTER > " FOR ENTER

    CLOSE WINDOW TAAB0181

END FUNCTION

FUNCTION inicio()
#i---------------

    LET HOY          = TODAY
    LET HORA         = TIME
    LET contador     = 1
    LET fecha_envio  = HOY

    SELECT *, USER
    INTO   w_afore.*, g_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_taa.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    CREATE TEMP TABLE tot_x_sie
        (cod_sie  SMALLINT,
         des_sie  CHAR(30),
         tot_sie  INTEGER )

    CREATE TEMP TABLE ctr_nss_sie_inv
        (nss       CHAR(11),
         cod_sie   SMALLINT,
         des_sie   CHAR(30))

    SELECT MAX(fecha_mov_banxico)
      INTO fecha_liquida
      FROM taa_rcv_recepcion

    SELECT @folio
      INTO vfolio_09
      FROM taa_rcv_recepcion
     WHERE @fecha_mov_banxico = fecha_liquida
       AND ident_operacion    = '09'
    GROUP BY 1

    IF SQLCA.SQLCODE <> 0 THEN
        LET vfolio_09 = ''
    END IF

    SELECT @folio
      INTO vfolio_12
      FROM taa_rcv_recepcion
     WHERE @fecha_mov_banxico = fecha_liquida
       AND ident_operacion    = '12'
    GROUP BY 1

    IF SQLCA.SQLCODE <> 0 THEN
        LET vfolio_12 = ''
    END IF

END FUNCTION

FUNCTION inicializa()
#iz------------------

    LET reg_det_inv_rcv.curp       = ''
    LET reg_det_inv_rcv.nss        = ''
    LET reg_det_inv_rcv.rfc        = ''
    LET reg_det_inv_rcv.paterno    = ''
    LET reg_det_inv_rcv.materno    = ''
    LET reg_det_inv_rcv.nombres    = ''
    LET reg_det_inv_rcv.cve_sub_01 = ''
    LET reg_det_inv_rcv.cve_sie_01 = ''
    LET reg_det_inv_rcv.cve_sub_02 = ''
    LET reg_det_inv_rcv.cve_sie_02 = ''
    LET reg_det_inv_rcv.cve_sub_03 = ''
    LET reg_det_inv_rcv.cve_sie_03 = ''
    LET reg_det_inv_rcv.cve_sub_04 = ''
    LET reg_det_inv_rcv.cve_sie_04 = ''
    LET reg_det_inv_rcv.cve_sub_05 = ''
    LET reg_det_inv_rcv.cve_sie_05 = ''
    LET reg_det_inv_rcv.cve_sub_06 = ''
    LET reg_det_inv_rcv.cve_sie_06 = ''
    LET reg_det_inv_rcv.cve_sub_07 = ''
    LET reg_det_inv_rcv.cve_sie_07 = ''
    LET reg_det_inv_rcv.cve_sub_08 = ''
    LET reg_det_inv_rcv.cve_sie_08 = ''
    LET reg_det_inv_rcv.cve_sub_09 = ''
    LET reg_det_inv_rcv.cve_sie_09 = ''
    LET reg_det_inv_rcv.cve_sub_10 = ''
    LET reg_det_inv_rcv.cve_sie_10 = ''
    LET reg_det_inv_rcv.cve_sub_11 = ''
    LET reg_det_inv_rcv.cve_sie_11 = ''
    LET reg_det_inv_rcv.cve_sub_12 = ''
    LET reg_det_inv_rcv.cve_sie_12 = ''
    LET reg_det_inv_rcv.cve_sub_13 = ''
    LET reg_det_inv_rcv.cve_sie_13 = ''
    LET reg_det_inv_rcv.cve_sub_14 = ''
    LET reg_det_inv_rcv.cve_sie_14 = ''
    LET reg_det_inv_rcv.cve_sub_15 = ''
    LET reg_det_inv_rcv.cve_sie_15 = ''
    LET reg_det_inv_rcv.cve_sub_16 = ''
    LET reg_det_inv_rcv.cve_sie_16 = ''
    LET reg_det_inv_rcv.cve_sub_17 = ''
    LET reg_det_inv_rcv.cve_sie_17 = ''
    LET reg_det_inv_rcv.cve_sub_18 = ''
    LET reg_det_inv_rcv.cve_sie_18 = ''
    LET reg_det_inv_rcv.cve_sub_19 = ''
    LET reg_det_inv_rcv.cve_sie_19 = ''
    LET reg_det_inv_rcv.cve_sub_20 = ''
    LET reg_det_inv_rcv.cve_sie_20 = ''
    LET reg_det_inv_rcv.cve_sub_21 = ''
    LET reg_det_inv_rcv.cve_sie_21 = ''
    LET reg_det_inv_rcv.nss_unificado = '           '

END FUNCTION

FUNCTION genera_archivo_inv()
#ad--------------------------

    CALL genera_cza_inv_rcv() #gcds
    CALL genera_det_inv_rcv() #gdds
    CALL genera_sum_inv_rcv() #gsds

    LET hora = hora[1,2],hora[4,5],hora[7,8]

    LET nom_archivo = "INV_REC_RCV.",HOY USING "YYMMDD","-",hora

    LET cat = "cat ",g_param_taa.ruta_envio CLIPPED,"/CRT ",
                     g_param_taa.ruta_envio CLIPPED,"/DRT ",
                     g_param_taa.ruta_envio CLIPPED,"/SRT > ",
                     g_param_taa.ruta_envio CLIPPED,"/",
                     nom_archivo

    RUN cat

END FUNCTION

FUNCTION genera_cza_inv_rcv()
#gcds------------------------

    LET reg_cza_inv_rcv.tipo_registro      = "01"
    LET reg_cza_inv_rcv.ident_servicio     = "02"
    LET reg_cza_inv_rcv.ident_operacion    = "36"
    LET reg_cza_inv_rcv.tipo_ent_origen    = "01"
    LET reg_cza_inv_rcv.cve_ent_origen     = w_afore.codigo_afore
    LET reg_cza_inv_rcv.tipo_ent_destino   = "01"
    LET reg_cza_inv_rcv.cve_ent_destino    = "   " 
    LET reg_cza_inv_rcv.ent_fed_envio_lote = "009"
    LET reg_cza_inv_rcv.fecha_presentacion = fecha_envio USING "YYYYMMDD"
    LET reg_cza_inv_rcv.consec_lote_dia    = 1
    LET reg_cza_inv_rcv.cve_mod_recepcion  = "02"
    LET reg_cza_inv_rcv.cod_result_operac  = NULL
    LET reg_cza_inv_rcv.mot_rechazo_lote   = NULL

    LET g_cza = g_param_taa.ruta_envio CLIPPED,"/CRT"

    START REPORT listado_1 TO g_cza
        OUTPUT TO REPORT listado_1(reg_cza_inv_rcv.*) #1
    FINISH REPORT listado_1

END FUNCTION

REPORT listado_1(reg_cza_inv_rcv)
#1-------------------------------

    DEFINE reg_cza_inv_rcv RECORD
        tipo_registro      CHAR(02),
        ident_servicio     CHAR(02),
        ident_operacion    CHAR(02),
        tipo_ent_origen    CHAR(02),
        cve_ent_origen     CHAR(03),
        tipo_ent_destino   CHAR(02),
        cve_ent_destino    CHAR(03),
        ent_fed_envio_lote CHAR(03),
        fecha_presentacion CHAR(08),
        consec_lote_dia    SMALLINT,
        cve_mod_recepcion  CHAR(02),
        cod_result_operac  CHAR(02),
        mot_rechazo_lote   CHAR(09)
    END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
        PRINT
            COLUMN 01,reg_cza_inv_rcv.tipo_registro      ,
                      reg_cza_inv_rcv.ident_servicio     ,
                      reg_cza_inv_rcv.ident_operacion    ,
                      reg_cza_inv_rcv.tipo_ent_origen    ,
                      reg_cza_inv_rcv.cve_ent_origen     ,
                      reg_cza_inv_rcv.tipo_ent_destino   ,
                      reg_cza_inv_rcv.cve_ent_destino    ,
                      reg_cza_inv_rcv.ent_fed_envio_lote ,
                      reg_cza_inv_rcv.fecha_presentacion  ,
                      reg_cza_inv_rcv.consec_lote_dia    USING "&&&",
                      reg_cza_inv_rcv.cve_mod_recepcion  ,
                      reg_cza_inv_rcv.cod_result_operac  ,
                      reg_cza_inv_rcv.mot_rechazo_lote   ,
                      687 spaces

END REPORT

FUNCTION genera_det_inv_rcv()
#gdds------------------------

    DEFINE subcta_proc  CHAR(2)
    DEFINE dsiefore     CHAR(8)

    DEFINE vtipo_sol    SMALLINT
    DEFINE v_existe     SMALLINT
    DEFINE v_edad       SMALLINT
    DEFINE v_criterio   SMALLINT
    DEFINE v_ind_edad   SMALLINT
    DEFINE v_curp       CHAR(18)
    DEFINE v_rfc        CHAR(13)
    DEFINE v_fena       DATE
    DEFINE vsubcta      SMALLINT
    DEFINE vcod_sie     SMALLINT
    DEFINE vtipo_trasp  SMALLINT
    DEFINE i            SMALLINT

    DEFINE vn_folio     DECIMAL(10,0)
    DEFINE vf_banxico   DATE
    DEFINE v_crea_fecha DATE

    LET i          = 0

    LET exe_sql    = "EXECUTE PROCEDURE fn_fnacimiento_sol(?,?,?,?)"
    LET exe_ind    = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
    LET exe_sql_1  = "EXECUTE PROCEDURE fn_valida_edad_sol (?,?) "
    LET g_det      = g_param_taa.ruta_envio CLIPPED,"/DRT"

    PREPARE spl_sol FROM exe_sql
    PREPARE spl_ind FROM exe_ind
    PREPARE spl_fec FROM exe_sql_1
    DECLARE cur_fecha_val CURSOR FOR spl_fec

    CALL llena_rcv()

    LET reg_det_inv_rcv.tipo_registro  = '05'
    LET reg_det_inv_rcv.tipo_ent_recep = '01'
    LET reg_det_inv_rcv.cve_ent_recep  = w_afore.codigo_afore
    LET reg_det_inv_rcv.tipo_ent_ced   = '01'

    DECLARE cur_9 CURSOR FOR
    SELECT UNIQUE t.nss       ,
           t.fecha_mov_banxico,
           t.cve_ced_cuenta   ,
           t.fecha_mov_banxico,
           t.ident_operacion  ,
           t.tipo_traspaso    ,
           t.rfc
    FROM   taa_rcv_recepcion t
    WHERE  t.folio = vfolio_09

    START REPORT listado_2 TO g_det
        FOREACH cur_9 INTO reg_det_inv_rcv.nss,
                           reg_det_inv_rcv.fecha_liquidacion,
                           reg_det_inv_rcv.cve_ent_ced,
                           vf_banxico,
                           iop,
                           vtipo_trasp,
                           reg_det_inv_rcv.rfc 

            LET reg_det_inv_rcv.cont_servicio = contador

            IF vtipo_trasp <> 22 AND
               vtipo_trasp <> 23 THEN
                SELECT a.n_unico          ,
                       --a.n_rfc            ,
                       a.paterno          ,
                       a.materno          ,
                       a.nombres          ,
                       a.n_folio          ,
                       a.tipo_solicitud   
                INTO   reg_det_inv_rcv.curp    ,
                       --reg_det_inv_rcv.rfc     ,
                       reg_det_inv_rcv.paterno ,
                       reg_det_inv_rcv.materno ,
                       reg_det_inv_rcv.nombres ,
                       vn_folio                ,
                       vtipo_sol                
                FROM   afi_solicitud a
                WHERE  a.n_seguro       = reg_det_inv_rcv.nss
                AND    a.status_interno = 75

                OPEN cur_fecha_val USING reg_det_inv_rcv.nss,
                                         vtipo_trasp
                FETCH cur_fecha_val INTO v_crea_fecha
                CLOSE cur_fecha_val
 
                DECLARE c_edad_sol CURSOR FOR spl_sol

                OPEN c_edad_sol USING reg_det_inv_rcv.nss,
                                      vn_folio,
                                      vtipo_sol,
                                      v_crea_fecha

                FETCH c_edad_sol INTO v_existe, 
                                      v_edad, 
                                      v_criterio,
                                      v_ind_edad,
                                      v_curp,
                                      v_rfc,
                                      v_fena

                CLOSE c_edad_sol
            ELSE
                SELECT a.n_unico          ,
                       a.n_rfc            ,
                       a.paterno          ,
                       a.materno          ,
                       a.nombres          ,
                       a.n_folio          ,
                       a.tipo_solicitud   
                INTO   reg_det_inv_rcv.curp    ,
                       reg_det_inv_rcv.rfc     ,
                       reg_det_inv_rcv.paterno ,
                       reg_det_inv_rcv.materno ,
                       reg_det_inv_rcv.nombres ,
                       vn_folio                ,
                       vtipo_sol               
                FROM   afi_mae_afiliado a
                WHERE  a.n_seguro = reg_det_inv_rcv.nss

                OPEN cur_fecha_val USING reg_det_inv_rcv.nss,
                                         vtipo_trasp
                FETCH cur_fecha_val INTO v_crea_fecha
                CLOSE cur_fecha_val

                DECLARE c_edad_ind CURSOR FOR spl_ind
                OPEN c_edad_ind USING reg_det_inv_rcv.nss,
                                      v_crea_fecha
                FETCH c_edad_ind INTO v_existe, 
                                      v_edad, 
                                      v_criterio,
                                      v_ind_edad,
                                      v_curp,
                                      v_rfc,
                                      v_fena
                CLOSE c_edad_ind
            END IF

            DECLARE cur_inv CURSOR FOR
            SELECT UNIQUE b.subcuenta
            FROM   recep_inv a, tab_subcta_taa b
            WHERE  a.nss        = reg_det_inv_rcv.nss
            AND    a.cve_subcta = b.sub_taa

            FOREACH cur_inv INTO vsubcta
                SELECT r.codigo_siefore
                  INTO vcod_sie
                  FROM tab_regimen_inv r
                 WHERE r.ind_edad     = v_ind_edad
                   AND r.tipo_regimen = 0
                   AND r.grupo_regimen = (SELECT a.grupo_regimen
                                            FROM tab_agrupa_subcta_regimen a
                                           WHERE a.subcuenta = vsubcta)

                CALL busca_siefore(vcod_sie, vsubcta)
                     RETURNING dsiefore, subcta_proc

                LET i = i + 1

                CASE i
                    WHEN  1 LET reg_det_inv_rcv.cve_sub_01 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_01 = dsiefore

                    WHEN  2 LET reg_det_inv_rcv.cve_sub_02 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_02 = dsiefore

                    WHEN  3 LET reg_det_inv_rcv.cve_sub_03 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_03 = dsiefore

                    WHEN  4 LET reg_det_inv_rcv.cve_sub_04 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_04 = dsiefore

                    WHEN  5 LET reg_det_inv_rcv.cve_sub_05 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_05 = dsiefore

                    WHEN  6 LET reg_det_inv_rcv.cve_sub_06 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_06 = dsiefore

                    WHEN  7 LET reg_det_inv_rcv.cve_sub_07 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_07 = dsiefore

                    WHEN  8 LET reg_det_inv_rcv.cve_sub_08 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_08 = dsiefore

                    WHEN  9 LET reg_det_inv_rcv.cve_sub_09 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_09 = dsiefore

                    WHEN 10 LET reg_det_inv_rcv.cve_sub_10 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_10 = dsiefore
                END CASE

                INSERT INTO ctr_nss_sie_inv VALUES( reg_det_inv_rcv.nss,
                                                    vcod_sie,
                                                    dsiefore)
            END FOREACH

            LET contador = contador + 1

            OUTPUT TO REPORT listado_2(reg_det_inv_rcv.*) #2

            LET i          = 0

            CALL inicializa()
        END FOREACH

    DECLARE cur_12 CURSOR FOR
    SELECT UNIQUE t.nss       ,
           t.fecha_mov_banxico,
           t.cve_ced_cuenta   ,
           t.fecha_mov_banxico,
           t.ident_operacion  ,
           t.tipo_traspaso    ,
           t.nss_cedente
    FROM   taa_rcv_recepcion t
    WHERE  t.folio = vfolio_12

        FOREACH cur_12 INTO reg_det_inv_rcv.nss,
                            reg_det_inv_rcv.fecha_liquidacion,
                            reg_det_inv_rcv.cve_ent_ced,
                            vf_banxico,
                            iop,
                            vtipo_trasp,
                            reg_det_inv_rcv.nss_unificado

            LET reg_det_inv_rcv.cont_servicio = contador

            SELECT a.n_unico          ,
                   a.n_rfc            ,
                   a.paterno          ,
                   a.materno          ,
                   a.nombres          ,
                   a.n_folio          ,
                   a.tipo_solicitud  
            INTO   reg_det_inv_rcv.curp    ,
                   reg_det_inv_rcv.rfc     ,
                   reg_det_inv_rcv.paterno ,
                   reg_det_inv_rcv.materno ,
                   reg_det_inv_rcv.nombres ,
                   vn_folio                ,
                   vtipo_sol
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = reg_det_inv_rcv.nss

            DECLARE cur_cmp CURSOR FOR
            SELECT UNIQUE b.subcuenta
            FROM   recep_inv a, tab_subcta_taa b
            WHERE  a.nss        = reg_det_inv_rcv.nss
            AND    a.cve_subcta = b.sub_taa

            FOREACH cur_cmp INTO vsubcta
                SELECT r.codigo_siefore
                  INTO vcod_sie
                  FROM cta_regimen r
                 WHERE r.nss       = reg_det_inv_rcv.nss
                   AND r.subcuenta = vsubcta

                CALL busca_siefore(vcod_sie, vsubcta)
                     RETURNING dsiefore, subcta_proc

                LET i = i + 1

                CASE i
                    WHEN  1 LET reg_det_inv_rcv.cve_sub_01 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_01 = dsiefore

                    WHEN  2 LET reg_det_inv_rcv.cve_sub_02 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_02 = dsiefore

                    WHEN  3 LET reg_det_inv_rcv.cve_sub_03 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_03 = dsiefore

                    WHEN  4 LET reg_det_inv_rcv.cve_sub_04 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_04 = dsiefore

                    WHEN  5 LET reg_det_inv_rcv.cve_sub_05 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_05 = dsiefore

                    WHEN  6 LET reg_det_inv_rcv.cve_sub_06 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_06 = dsiefore

                    WHEN  7 LET reg_det_inv_rcv.cve_sub_07 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_07 = dsiefore

                    WHEN  8 LET reg_det_inv_rcv.cve_sub_08 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_08 = dsiefore

                    WHEN  9 LET reg_det_inv_rcv.cve_sub_09 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_09 = dsiefore

                    WHEN 10 LET reg_det_inv_rcv.cve_sub_10 = subcta_proc
                            LET reg_det_inv_rcv.cve_sie_10 = dsiefore
                END CASE

                INSERT INTO ctr_nss_sie_inv VALUES( reg_det_inv_rcv.nss,
                                                    vcod_sie,
                                                    dsiefore)

            END FOREACH

            LET contador = contador + 1

            IF vtipo_trasp = 12 OR
               vtipo_trasp = 20 THEN
                IF reg_det_inv_rcv.nss_unificado IS NULL OR
                   reg_det_inv_rcv.nss_unificado = '           ' THEN
                    LET reg_det_inv_rcv.nss_unificado = ''
                END IF
            ELSE
                LET reg_det_inv_rcv.nss_unificado = ''
            END IF

            OUTPUT TO REPORT listado_2(reg_det_inv_rcv.*) #2

            LET i          = 0

            CALL inicializa()
        END FOREACH

    FINISH REPORT listado_2 #2

    UPDATE STATISTICS FOR TABLE ctr_nss_sie_inv

    SELECT nss, cod_sie, des_sie
      FROM ctr_nss_sie_inv
     GROUP BY 1,2,3
      INTO TEMP ctr_nss_sie_inv_1

    INSERT INTO tot_x_sie
    SELECT cod_sie, des_sie, count(*)
      FROM ctr_nss_sie_inv_1
     GROUP BY 1,2

END FUNCTION

FUNCTION busca_siefore(vcod_sie, vsubcta)
#bs--------------------------------------

    DEFINE vcod_sie SMALLINT
    DEFINE vsubcta  SMALLINT
    DEFINE dsiefore CHAR(8)
    DEFINE vsub_taa CHAR(2)

    SELECT t.razon_social
      INTO dsiefore
      FROM tab_siefore_local t
     WHERE t.codigo_siefore = vcod_sie

    SELECT s.sub_taa
      INTO vsub_taa
      FROM tab_subcta_taa s
     WHERE s.subcuenta = vsubcta

    RETURN dsiefore, vsub_taa

END FUNCTION

REPORT listado_2(reg_det_inv_rcv)
#2-------------------------------

    DEFINE reg_det_inv_rcv RECORD
        tipo_registro       CHAR(2),
        cont_servicio       DECIMAL(10,0),
        tipo_ent_recep      CHAR(2),
        cve_ent_recep       CHAR(3),
        tipo_ent_ced        CHAR(2),
        cve_ent_ced         CHAR(3),
        fecha_liquidacion   DATE,
        curp                CHAR(18),
        nss                 CHAR(11),
        rfc                 CHAR(13),
        paterno             CHAR(40),
        materno             CHAR(40),
        nombres             CHAR(40),
        cve_sub_01          CHAR(2),
        cve_sie_01          CHAR(8),
        cve_sub_02          CHAR(2),
        cve_sie_02          CHAR(8),
        cve_sub_03          CHAR(2),
        cve_sie_03          CHAR(8),
        cve_sub_04          CHAR(2),
        cve_sie_04          CHAR(8),
        cve_sub_05          CHAR(2),
        cve_sie_05          CHAR(8),
        cve_sub_06          CHAR(2),
        cve_sie_06          CHAR(8),
        cve_sub_07          CHAR(2),
        cve_sie_07          CHAR(8),
        cve_sub_08          CHAR(2),
        cve_sie_08          CHAR(8),
        cve_sub_09          CHAR(2),
        cve_sie_09          CHAR(8),
        cve_sub_10          CHAR(2),
        cve_sie_10          CHAR(8),
        cve_sub_11          CHAR(2),
        cve_sie_11          CHAR(8),
        cve_sub_12          CHAR(2),
        cve_sie_12          CHAR(8),
        cve_sub_13          CHAR(2),
        cve_sie_13          CHAR(8),
        cve_sub_14          CHAR(2),
        cve_sie_14          CHAR(8),
        cve_sub_15          CHAR(2),
        cve_sie_15          CHAR(8),
        cve_sub_16          CHAR(2),
        cve_sie_16          CHAR(8),
        cve_sub_17          CHAR(2),
        cve_sie_17          CHAR(8),
        cve_sub_18          CHAR(2),
        cve_sie_18          CHAR(8),
        cve_sub_19          CHAR(2),
        cve_sie_19          CHAR(8),
        cve_sub_20          CHAR(2),
        cve_sie_20          CHAR(8),
        cve_sub_21          CHAR(2),
        cve_sie_21          CHAR(8),
        cod_res_op          CHAR(2),
        diag_proc           CHAR(15),
        nss_unificado       CHAR(11)
    END RECORD


    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW 
        PRINT
            COLUMN 01,reg_det_inv_rcv.tipo_registro     ,
                      reg_det_inv_rcv.cont_servicio     USING "&&&&&&&&&&",
                      reg_det_inv_rcv.tipo_ent_recep    ,
                      reg_det_inv_rcv.cve_ent_recep     ,
                      reg_det_inv_rcv.tipo_ent_ced      ,
                      reg_det_inv_rcv.cve_ent_ced       ,
                      2 SPACES                          ,
                      reg_det_inv_rcv.fecha_liquidacion USING "YYYYMMDD",
                      8 SPACES                          ,
                      reg_det_inv_rcv.curp              ,
                      reg_det_inv_rcv.nss               ,
                      reg_det_inv_rcv.rfc               ,
                      reg_det_inv_rcv.paterno           ,
                      reg_det_inv_rcv.materno           ,
                      reg_det_inv_rcv.nombres           ,
                      reg_det_inv_rcv.cve_sub_01        ,
                      reg_det_inv_rcv.cve_sie_01        ,
                      reg_det_inv_rcv.cve_sub_02        ,
                      reg_det_inv_rcv.cve_sie_02        ,
                      reg_det_inv_rcv.cve_sub_03        ,
                      reg_det_inv_rcv.cve_sie_03        ,
                      reg_det_inv_rcv.cve_sub_04        ,
                      reg_det_inv_rcv.cve_sie_04        ,
                      reg_det_inv_rcv.cve_sub_05        ,
                      reg_det_inv_rcv.cve_sie_05        ,
                      reg_det_inv_rcv.cve_sub_06        ,
                      reg_det_inv_rcv.cve_sie_06        ,
                      reg_det_inv_rcv.cve_sub_07        ,
                      reg_det_inv_rcv.cve_sie_07        ,
                      reg_det_inv_rcv.cve_sub_08        ,
                      reg_det_inv_rcv.cve_sie_08        ,
                      reg_det_inv_rcv.cve_sub_09        ,
                      reg_det_inv_rcv.cve_sie_09        ,
                      reg_det_inv_rcv.cve_sub_10        ,
                      reg_det_inv_rcv.cve_sie_10        ,
                      reg_det_inv_rcv.cve_sub_11        ,
                      reg_det_inv_rcv.cve_sie_11        ,
                      reg_det_inv_rcv.cve_sub_12        ,
                      reg_det_inv_rcv.cve_sie_12        ,
                      reg_det_inv_rcv.cve_sub_13        ,
                      reg_det_inv_rcv.cve_sie_13        ,
                      reg_det_inv_rcv.cve_sub_14        ,
                      reg_det_inv_rcv.cve_sie_14        ,
                      reg_det_inv_rcv.cve_sub_15        ,
                      reg_det_inv_rcv.cve_sie_15        ,
                      reg_det_inv_rcv.cve_sub_16        ,
                      reg_det_inv_rcv.cve_sie_16        ,
                      reg_det_inv_rcv.cve_sub_17        ,
                      reg_det_inv_rcv.cve_sie_17        ,
                      reg_det_inv_rcv.cve_sub_18        ,
                      reg_det_inv_rcv.cve_sie_18        ,
                      reg_det_inv_rcv.cve_sub_19        ,
                      reg_det_inv_rcv.cve_sie_19        ,
                      reg_det_inv_rcv.cve_sub_20        ,
                      reg_det_inv_rcv.cve_sie_20        ,
                      reg_det_inv_rcv.cve_sub_21        ,
                      reg_det_inv_rcv.cve_sie_21        ,
                      2 SPACES                          ,
                      reg_det_inv_rcv.cod_res_op        ,
                      reg_det_inv_rcv.diag_proc         ,
                      reg_det_inv_rcv.nss_unificado     ,
                      288 SPACES 

END REPORT

FUNCTION genera_sum_inv_rcv()
#gstc------------------------

    LET reg_sum_inv_rcv.tipo_registro    = "09"
    LET reg_sum_inv_rcv.cantidad_reg_det = reg_det_inv_rcv.cont_servicio

    LET g_sum = g_param_taa.ruta_envio CLIPPED,"/SRT"

    START REPORT listado_3 TO g_sum
        OUTPUT TO REPORT listado_3(reg_sum_inv_rcv.*) #3
    FINISH REPORT listado_3

END FUNCTION

REPORT listado_3(reg_sum_inv_rcv)
#3-------------------------------

    DEFINE reg_sum_inv_rcv RECORD 
        tipo_registro      CHAR(02),
        cantidad_reg_det   INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_sum_inv_rcv.tipo_registro   ,
                   reg_sum_inv_rcv.cantidad_reg_det   USING "&&&&&&&&&",
                   719 spaces

END REPORT

FUNCTION impresion_reporte()
#imp------------------------

    DEFINE G_IMPRE     CHAR(300)
    DEFINE gimpresion  CHAR(300)

    LET HORA = TIME
    LET HORA = HORA[1,2],HORA[4,5]

    LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".RPT_INV_RCV.",HOY USING "DDMMYY","_",HORA CLIPPED

    START REPORT devol_sol TO G_IMPRE
        DECLARE cur_sie CURSOR FOR
        SELECT *
          FROM tot_x_sie
         ORDER BY 1

        FOREACH cur_sie INTO g.*
            OUTPUT TO REPORT devol_sol (g.*)
        END FOREACH
    FINISH REPORT devol_sol

    LET gimpresion = "chmod 777 ",G_IMPRE
    RUN gimpresion

    LET gimpresion = "lp ",G_IMPRE
     --  LET gimpresion = "vi ",G_IMPRE
    RUN gimpresion

END FUNCTION

REPORT devol_sol(g)
#rds---------------

    DEFINE g RECORD
        cod_sie SMALLINT,
        des_sie CHAR(30),
        tot_sie INTEGER
    END RECORD

    DEFINE total INTEGER

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 60

    FORMAT
        PAGE HEADER

     PRINT COLUMN 01, w_afore.codigo_afore USING '&&&',
           COLUMN 07, w_afore.razon_social,
           COLUMN 68,TODAY USING "dd-mm-yyyy"
     SKIP 2 LINE
     PRINT COLUMN 08,"TOTAL DE NSS POR TIPO DE INVERSION (RCV) "
     SKIP 2 LINE
     PRINT COLUMN 08,"FOLIOS DE LIQUIDACION : ",vfolio_09, " ", vfolio_12
     SKIP 2 LINE
     PRINT COLUMN 05,"SIEFORE",
           COLUMN 40,"TOTAL"
     PRINT COLUMN 02,"--------------------------------------------------------"

   ON EVERY ROW

      PRINT COLUMN 05,g.cod_sie USING "-&",
            COLUMN 10,g.des_sie,
            COLUMN 40,g.tot_sie

    ON LAST ROW
     PRINT COLUMN 02,"--------------------------------------------------------"

        SKIP 4 LINES
        PRINT COLUMN 5, "Total de nss                        : ",
        SUM(g.tot_sie) USING "<<<<<<<"

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar...."

END REPORT

FUNCTION llena_rcv()
#lr-----------------

    WHENEVER ERROR CONTINUE
        DROP TABLE recep_inv
    WHENEVER ERROR STOP

    CREATE TEMP TABLE recep_inv
        (nss         CHAR(11),
         cve_subcta  CHAR(2) ,
         siefore     CHAR(8))

    INSERT INTO recep_inv
    SELECT nss         ,
           cve_subcta_1,
           siefore_1
      FROM taa_rcv_recepcion
     WHERE folio IN (vfolio_09, vfolio_12)
       AND cve_subcta_1 IS NOT NULL

    INSERT INTO recep_inv
    SELECT nss         ,
           cve_subcta_2,
           siefore_2
      FROM taa_rcv_recepcion
     WHERE folio IN (vfolio_09, vfolio_12)
       AND cve_subcta_2 IS NOT NULL

    INSERT INTO recep_inv
    SELECT nss         ,
           cve_subcta_3,
           siefore_3
      FROM taa_rcv_recepcion
     WHERE folio IN (vfolio_09, vfolio_12)
       AND cve_subcta_3 IS NOT NULL

    INSERT INTO recep_inv
    SELECT nss         ,
           cve_subcta_4,
           siefore_4
      FROM taa_rcv_recepcion
     WHERE folio IN (vfolio_09, vfolio_12)
       AND cve_subcta_4 IS NOT NULL

    INSERT INTO recep_inv
    SELECT nss         ,
           cve_subcta_5,
           siefore_5
      FROM taa_rcv_recepcion
     WHERE folio IN (vfolio_09, vfolio_12)
       AND cve_subcta_5 IS NOT NULL

    INSERT INTO recep_inv
    SELECT nss         ,
           cve_subcta_6,
           siefore_6
      FROM taa_rcv_recepcion
     WHERE folio IN (vfolio_09, vfolio_12)
       AND cve_subcta_6 IS NOT NULL

    INSERT INTO recep_inv
    SELECT nss         ,
           cve_subcta_7,
           siefore_7
      FROM taa_rcv_recepcion
     WHERE folio IN (vfolio_09, vfolio_12)
       AND cve_subcta_7 IS NOT NULL

    INSERT INTO recep_inv
    SELECT nss         ,
           cve_subcta_8,
           siefore_8
      FROM taa_rcv_recepcion
     WHERE folio IN (vfolio_09, vfolio_12)
       AND cve_subcta_8 IS NOT NULL

    DELETE 
    FROM   recep_inv
    WHERE  cve_subcta = '  '
    OR     cve_subcta IS NULL

    CREATE INDEX recep_inv2
    ON recep_inv(nss)

    UPDATE STATISTICS FOR TABLE recep_inv

END FUNCTION


################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC902  => RECIBE ARCHIVOS DE RETIROS ISSSTE ENVIADOS POR PROCESAR  #
#Fecha creacion    => 04 DE OCTUBRE DEL 2006                                   #
#By                => DMR                                                      #
#Fecha actualiz.   => 25 DE NOVIEMBRE DE 2007                                  #
#                  => XAVIER TORRES RIOS                                       #
#                  => SE ADAPTO PARA QUE ACEPTARA LOS STATUS 128, 131,134 y 137#
#Modifico          => JAVIER GONZALEZ JERONIMO                                 #
#                  => 17 DE ABRIL DE 2008                                      #
#                     SE AGREGARON CAMBIOS PARA SOPORTE DE MULTISIEFORES       #
#                  => CESAR DAVID CHAVEZ MARTINEZ                              #
#                     18 DE AGOSTO 2008                                        #
#                     MODIFICACIONES PARA LA NUEVA VERSION DE LAYOUT           #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_estado RECORD #glo #reg_02
        recepcionado          LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud ,
        diag_total_op28       LIKE ret_estado.estado_solicitud ,
        diag_total_op31       LIKE ret_estado.estado_solicitud ,
        diag_parcial_op34     LIKE ret_estado.estado_solicitud ,
        diag_parcial_op37     LIKE ret_estado.estado_solicitud
    END RECORD

    --Se van a pasar a locales
    DEFINE #loc #char
        c15_acciones          CHAR(15) ,
        c10_fecha             CHAR(10) ,
        c6_acciones           CHAR(6)

    DEFINE  #reg_cza_lote
        reg_cza_lote          RECORD LIKE ret_cza_lote.*   ,
        reg_19                RECORD LIKE ret_sal_his_rx.*

    DEFINE  #s_modulo
        s_modulo              RECORD LIKE seg_modulo.*

    DEFINE  #reg_sol_total
        reg_sol_total         RECORD LIKE ret_sol_issste_tot.*

    DEFINE  #reg_sol_parcial
        reg_sol_parcial       RECORD LIKE ret_sol_issste_par.*

    DEFINE reg_monto_issste   RECORD
        nss                   LIKE ret_monto_sie_issste.nss            ,
        curp                  LIKE ret_sol_issste_tot.curp             ,
        acciones_sie1         LIKE ret_monto_sie_issste.saldo_acciones ,
        acciones_sie2         LIKE ret_monto_sie_issste.saldo_acciones ,
        acciones_sie3         LIKE ret_monto_sie_issste.saldo_acciones ,
        acciones_sie4         LIKE ret_monto_sie_issste.saldo_acciones ,
        acciones_sie5         LIKE ret_monto_sie_issste.saldo_acciones
    END RECORD

    DEFINE reg_2 RECORD  #glo #reg_2
        nom_archivo           CHAR(20)
    END RECORD

    DEFINE reg_20 RECORD #loc #reg_20
        estado_marca          SMALLINT,
        codigo_rechazo        SMALLINT,
        marca_causa           SMALLINT,
        fecha_causa           DATE
    END RECORD

    DEFINE #glo INTEGER
        cuantos  ,
        cont     ,
        cont_det ,
        cont_01,cont_04,cont_07,
        cont_02,cont_05,cont_08,
        cont_03,cont_06,cont_09,
        ultimo_folio          INTEGER

    DEFINE #glo CHAR
        archivo_retiro        CHAR(200),
        enter                 CHAR(1)  ,
        diag_rechazo          CHAR(3)  ,
        usuario               CHAR(12) ,
        v_desmarca            CHAR(100),
        v_marca               CHAR(100),
        carga_reg             CHAR(1200),
        c19_acciones          CHAR(19) ,
        c6porcentaje_val      CHAR(6)  ,
        sec_pension           CHAR(2)

    DEFINE #glo SMALLINT
        sw_1                  ,
        s_codigo_afore        ,
        v_marca_ent           ,
        v_cod_rechazo         ,
        v_codigo_rechazo      ,
        v_marca_res           SMALLINT


    DEFINE #glo DATE
        HOY                   DATE,
        fecha                 DATE,
        fec_envio             DATE

END GLOBALS


MAIN
    OPTIONS
        PROMPT LINE LAST
        DEFER INTERRUPT
        CALL STARTLOG("RETC902.log")

    CALL init()

    OPEN WINDOW retc9021 AT 4,4 WITH FORM "RETC9021" ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC902          RECEPCIONA ARCHIVOS DE RETIROS ISSSTE                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_2.nom_archivo WITHOUT DEFAULTS

        BEFORE FIELD nom_archivo
            LET reg_2.nom_archivo = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo
            IF reg_2.nom_archivo IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD nom_archivo
            END IF

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = reg_2.nom_archivo
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR "   ARCHIVO YA PROCESADO CON ANTERIORIDAD" ATTRIBUTE(NORMAL)
                NEXT FIELD nom_archivo
            END IF

            -- Se carga el archivo plano en la tabla ret_pla_carga
            LET archivo_retiro = s_modulo.ruta_rescate CLIPPED,"/",
                                 reg_2.nom_archivo CLIPPED

            LOAD FROM archivo_retiro DELIMITER "+"
            INSERT INTO ret_pla_carga

            SELECT count(*)
            INTO   cuantos
            FROM   ret_pla_carga

            IF cuantos = 0 THEN
                ERROR "   NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO" ATTRIBUTE(NORMAL)
                NEXT FIELD nom_archivo
            ELSE
                EXIT INPUT
            END IF

        ON KEY (INTERRUPT)
            PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL primer_paso() #pp --Crea tablas temporales y valida el detalle

    SELECT MAX(folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio

    IF ultimo_folio IS NULL THEN
        LET ultimo_folio = 1
    END IF

    INSERT INTO glo_folio VALUES(ultimo_folio)
    DISPLAY "FOLIO NUMERO  : ",ultimo_folio  AT 18,2

    CALL segundo_paso() #sp

    DISPLAY "FOLIO NUMERO  : ",ultimo_folio  AT 18,2

    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
    FOR CHAR enter

    CLOSE WINDOW retc9021
END MAIN


FUNCTION init()
#i-------------
    LET HOY  = TODAY
    LET sw_1 = 0

    SELECT *
    INTO   s_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT A.estado_solicitud
    INTO   gr_estado.recepcionado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    -- Se almacenan los diagnosticos que deben venir por cada tipo de operacion
    SELECT A.estado_solicitud
    INTO   gr_estado.diag_total_op28
    FROM   ret_estado A
    WHERE  A.descripcion = "RECIBIDO OPER28"

    SELECT A.estado_solicitud
    INTO   gr_estado.diag_total_op31
    FROM   ret_estado A
    WHERE  A.descripcion = "RECIBIDO OPER31"

    SELECT A.estado_solicitud
    INTO   gr_estado.diag_parcial_op34
    FROM   ret_estado A
    WHERE  A.descripcion = "RECIBIDO OPER34"

    SELECT A.estado_solicitud
    INTO   gr_estado.diag_parcial_op37
    FROM   ret_estado A
    WHERE  A.descripcion = "RECIBIDO OPER37"

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM v_desmarca

    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"
    PREPARE eje_marca FROM v_marca

    LET v_marca_ent      = 0
    LET v_cod_rechazo    = 0
    LET v_codigo_rechazo = 0

    WHENEVER ERROR CONTINUE
        CREATE TEMP TABLE ret_pla_carga
        (
            n_registros          CHAR(1200)
        )
    WHENEVER ERROR STOP

END FUNCTION


FUNCTION primer_paso()
#pp-------------------

    CALL crea_tablas_temp()

    LET cont     = 0
    LET cont_det = 0

    DECLARE cur_1 CURSOR FOR
    SELECT  *
    FROM    ret_pla_carga

    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1

        IF carga_reg[1,2] = "03" THEN
            CALL carga_tabla_tmp() #sp  CARGA TABLA TEMPORAL
            CALL compara_det()     #cd  COMPARA DETALLE VS TABLA
        END IF
    END FOREACH

END FUNCTION


FUNCTION segundo_paso()
#pp-------------------
    DEFINE #loc #char
        c10_fecha             CHAR(10)

    LET cont     = 0
    LET cont_det = 0
    LET cont_01  = 0
    LET cont_02  = 0
    LET cont_03  = 0
    LET cont_04  = 0
    LET cont_05  = 0
    LET cont_06  = 0
    LET cont_07  = 0
    LET cont_08  = 0
    LET cont_09  = 0

    DECLARE cur_1p CURSOR FOR
    SELECT  *
    FROM    ret_pla_carga

    FOREACH cur_1p INTO carga_reg
        LET cont = cont + 1
        IF carga_reg[1,2] = "01" THEN

            LET c10_fecha = carga_reg[21,22],"/",
                            carga_reg[23,24],"/",
                            carga_reg[17,20]

            LET reg_cza_lote.fecha_operacion = c10_fecha

            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  nom_archivo = reg_2.nom_archivo
           -- WHERE  fecha_operacion = reg_cza_lote.fecha_operacion

            IF STATUS = NOTFOUND THEN
                INSERT INTO ret_cza_lote
                VALUES (ultimo_folio                  ,
                        reg_cza_lote.fecha_operacion  ,
                        "01/01/0001"                  ,
                        reg_2.nom_archivo             ,
                        HOY                           ,  # fecha_carga
                        0                             ,
                        gr_estado.recepcionado        )
            ELSE
                DISPLAY ""  AT 18,2
                PROMPT " ARCHIVO YA PROCESADO CON ANTERIORIDAD.....<ENTER>",
                       " PARA SALIR" FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF

        DISPLAY " TOTAL REGISTROS PROCESADOS            : ",cont     AT 10,8
        DISPLAY " TOTAL REGISTROS DE DETALLE            : ",cont_det AT 11,8

        DISPLAY " RETIRO 01 : ",cont_01 USING "####&","    RETIRO 04 : ",cont_04 USING "####&","    RETIRO 07 : ",cont_07 USING "####&"   AT 14,8
        DISPLAY " RETIRO 02 : ",cont_02 USING "####&","    RETIRO 05 : ",cont_05 USING "####&","    RETIRO 08 : ",cont_08 USING "####&"   AT 15,8
        DISPLAY " RETIRO 03 : ",cont_03 USING "####&","    RETIRO 06 : ",cont_06 USING "####&","    RETIRO 09 : ",cont_09 USING "####&"   AT 16,8


        IF carga_reg[1,2] = "03" THEN
            CALL carga_det()      #cad DETALLE POR TIPO DE OPERACION
        END IF

        IF carga_reg[1,2] = "09" THEN
            CALL carga_sum_lote() #csl SUMARIO TRANSACCIONAL
        END IF
    END FOREACH
END FUNCTION


FUNCTION carga_det()
#cad-----------------
    DEFINE #loc #char
        c10_fecha             CHAR(10) ,
        cve_destino           CHAR(01) ,
        c6_porcentaje         CHAR(06) ,
        lc_diagnostico        CHAR(03) ,
        lc_tipo_mov           CHAR(02)

    DEFINE #loc #integer
        i_tipo_movimiento     INTEGER

    DEFINE
        li_estado_diag      LIKE ret_estado.estado_solicitud


    LET reg_20.estado_marca   = 0
    LET reg_20.codigo_rechazo = 0
    LET reg_20.marca_causa    = 0


    LET li_estado_diag = 0
    LET lc_tipo_mov    = carga_reg[5,6]

    LET cont_det = cont_det + 1
    DISPLAY " TOTAL REGISTROS DE DETALLE     : ",cont_det AT 11,8

    INITIALIZE reg_20.fecha_causa TO NULL

    -- Carga el detalle para Retiros Totales
    IF lc_tipo_mov = "28" OR lc_tipo_mov = "31" THEN
        LET reg_sol_total.tipo_retiro = carga_reg[191,192]

        CASE reg_sol_total.tipo_retiro
            WHEN "01" # 65 ANOS
               LET cont_01 = cont_01 + 1

            WHEN "02" # PENSION
               LET cont_02 = cont_02 + 1

            WHEN "03" # BENEFICIARIO
               LET cont_03 = cont_03 + 1

            WHEN "06" # PLAN DE PENSIONES
               LET cont_06 = cont_06 + 1

            WHEN "07" # REINGRESO
               LET cont_07 = cont_07 + 1

            WHEN "08" # OBLIGACION ALIMENTICIA
               LET cont_08 = cont_08 + 1

            WHEN "09" # INSTRUCCION DE AUTORIDAD
               LET cont_09 = cont_09 + 1
        END CASE

        LET reg_sol_total.nss_imss = carga_reg[010,020]
        LET reg_sol_total.curp     = carga_reg[032,049]

        IF reg_sol_total.nss_imss IS NULL OR
           reg_sol_total.nss_imss = " " THEN

            CASE lc_tipo_mov
                WHEN "28"
                    SELECT max(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot
                    WHERE  curp             = reg_sol_total.curp
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado

                    SELECT folio               ,
                           consecutivo         ,
                           nss_imss
                    INTO   reg_sol_total.folio       ,
                           reg_sol_total.consecutivo ,
                           reg_sol_total.nss_imss
                    FROM   ret_sol_issste_tot
                    WHERE  curp             = reg_sol_total.curp
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado
                    AND    fecha_envio      = fec_envio
                
                WHEN "31"
                    SELECT max(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot
                    WHERE  curp             = reg_sol_total.curp
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_total_op28

                    SELECT folio               ,
                           consecutivo         ,
                           nss_imss
                    INTO   reg_sol_total.folio       ,
                           reg_sol_total.consecutivo ,
                           reg_sol_total.nss_imss
                    FROM   ret_sol_issste_tot
                    WHERE  curp             = reg_sol_total.curp
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_total_op28
                    AND    fecha_envio      = fec_envio
            END CASE
        ELSE
            CASE lc_tipo_mov
                WHEN "28"
                    SELECT max(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot
                    WHERE  nss_imss         = reg_sol_total.nss_imss
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado

                    SELECT folio               ,
                           consecutivo         
                    INTO   reg_sol_total.folio       ,
                           reg_sol_total.consecutivo 
                    FROM   ret_sol_issste_tot
                    WHERE  nss_imss         = reg_sol_total.nss_imss
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado
                    AND    fecha_envio      = fec_envio
                
                WHEN "31"
                    SELECT max(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot
                    WHERE  nss_imss         = reg_sol_total.nss_imss
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_total_op28

                    SELECT folio               ,
                           consecutivo         
                    INTO   reg_sol_total.folio       ,
                           reg_sol_total.consecutivo 
                    FROM   ret_sol_issste_tot
                    WHERE  nss_imss         = reg_sol_total.nss_imss
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_total_op28
                    AND    fecha_envio      = fec_envio
            END CASE
        END IF

        IF lc_tipo_mov = "28" THEN
            -- Validaciones para operacion 28

            LET li_estado_diag =  gr_estado.diag_total_op28

            LET reg_sol_total.diag_procesar = carga_reg[438,440]

            IF reg_sol_total.diag_procesar IS NULL OR
               reg_sol_total.diag_procesar = "   " THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS  DIAGNOSTICO ... <ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            ELSE
                LET lc_diagnostico = reg_sol_total.diag_procesar
            END IF

            --***********NUEVOS CAMPOS DEL LAYOUT ISSSTE**********************
            --PARA TODOS LOS RETIROS
            LET reg_sol_total.tipo_retiro        = carga_reg[191,192]
            LET reg_sol_total.id_procesar        = carga_reg[517,524]
            LET reg_sol_total.nom_completo_icefa = carga_reg[525,644]

            --PARA RETIROS 3
            IF reg_sol_total.tipo_retiro = 3 THEN
               LET reg_sol_total.nom_beneficiario   = carga_reg[645,764]
            ELSE
            	 INITIALIZE reg_sol_total.nom_beneficiario TO NULL
            END IF

            --PARA RETIROS 2
            IF reg_sol_total.tipo_retiro = 2 THEN
               LET reg_sol_total.rfc_pension        = carga_reg[765,777]
               LET reg_sol_total.paterno_pension    = carga_reg[778,817]
               LET reg_sol_total.materno_pension    = carga_reg[818,857]
               LET reg_sol_total.nombres_pension    = carga_reg[858,897]
            ELSE
               INITIALIZE reg_sol_total.rfc_pension     TO NULL
               INITIALIZE reg_sol_total.paterno_pension TO NULL
               INITIALIZE reg_sol_total.materno_pension TO NULL
               INITIALIZE reg_sol_total.nombres_pension TO NULL
            END IF

            UPDATE ret_sol_issste_tot
            SET    estado_solicitud   = li_estado_diag,
                   diag_procesar      = lc_diagnostico,
                   id_procesar        = reg_sol_total.id_procesar       ,
                   nom_completo_icefa = reg_sol_total.nom_completo_icefa,
                   nom_beneficiario   = reg_sol_total.nom_beneficiario  ,
                   rfc_pension        = reg_sol_total.rfc_pension       ,
                   paterno_pension    = reg_sol_total.paterno_pension   ,
                   materno_pension    = reg_sol_total.materno_pension   ,
                   nombres_pension    = reg_sol_total.nombres_pension
            WHERE  consecutivo        = reg_sol_total.consecutivo

            LET ultimo_folio = reg_sol_total.folio

        ELSE
            -- Validaciones para operacion 31
            LET li_estado_diag =  gr_estado.diag_total_op31

            LET reg_sol_total.status_fondo_viv = carga_reg[443,444]

            IF reg_sol_total.status_fondo_viv IS NULL OR
               reg_sol_total.status_fondo_viv = "  " THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS STATUS FONDO VIVIENDA  <ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF

            LET reg_sol_total.status_sub_ret = carga_reg[445,446]

            IF reg_sol_total.status_sub_ret IS NULL OR
               reg_sol_total.status_sub_ret = "  " THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS STATUS SUBCUENTA RETIRO  <ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF

            UPDATE ret_sol_issste_tot
            SET    estado_solicitud = li_estado_diag                 ,
                   status_fondo_viv = reg_sol_total.status_fondo_viv ,
                   status_sub_ret   = reg_sol_total.status_sub_ret
            WHERE  consecutivo      = reg_sol_total.consecutivo

            IF reg_sol_total.status_sub_ret = "01" OR reg_sol_total.status_fondo_viv = "01" THEN
                LET lc_diagnostico = 1
            ELSE
                LET lc_diagnostico = 0
            END IF

            SELECT MAX(folio) + 1
            INTO   ultimo_folio
            FROM   glo_folio

            IF ultimo_folio IS NULL THEN
                LET ultimo_folio = 1
            END IF

            INSERT INTO glo_folio VALUES(ultimo_folio)

        END IF -- Validaciones por tipo de movimiento

        UPDATE ret_cza_lote
        SET    folio       = ultimo_folio
        WHERE  nom_archivo = reg_2.nom_archivo

        ----- DESMARCA -----

        IF NOT(lc_diagnostico = 400 OR lc_diagnostico = 1) THEN

            SELECT movimiento
            INTO   v_marca_ent
            FROM   tab_retiro_issste
            WHERE  tipo_retiro = reg_sol_total.tipo_retiro

            LET reg_20.estado_marca = 40
            LET reg_20.marca_causa  = 0

            IF reg_sol_total.nss_imss IS NULL OR
               reg_sol_total.nss_imss = " " THEN

                SELECT nss_imss
                INTO   reg_sol_total.nss_imss
                FROM   ret_sol_issste_tot
                WHERE  curp             = reg_sol_total.curp
                AND    estado_solicitud = li_estado_diag
                AND    consecutivo      = reg_sol_total.consecutivo
            END IF

            LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta('",
                             reg_sol_total.nss_imss,"',",v_marca_ent,
                             ",",reg_sol_total.consecutivo,",",
                             reg_20.estado_marca,",",reg_20.marca_causa
                             ,",' ",usuario,"')"
            PREPARE eje_desmar FROM v_desmarca
            EXECUTE eje_desmar
        END IF
    END IF  -- Carga estados para Retiros totales

    -- Carga el detalle para Retiros Parciales
    IF lc_tipo_mov = "34" OR lc_tipo_mov = "37" THEN

        LET reg_sol_parcial.tipo_retiro = carga_reg[183,184]

        CASE reg_sol_parcial.tipo_retiro

            WHEN "04" # INCAPACIDAD
                LET cont_04 = cont_04 + 1

            WHEN "05" # PENSION
                LET cont_05 = cont_05 + 1

       END CASE

       LET reg_sol_parcial.nss_imss           = carga_reg[010,020]
       LET reg_sol_parcial.curp               = carga_reg[032,049]
       LET reg_sol_parcial.rfc                = carga_reg[170,182]

        IF reg_sol_parcial.nss_imss IS NULL OR
           reg_sol_parcial.nss_imss = " " THEN

            CASE lc_tipo_mov
               WHEN "34"
                    SELECT folio        ,
                           consecutivo  ,
                           nss_imss
                    INTO   reg_sol_parcial.folio       ,
                           reg_sol_parcial.consecutivo ,
                           reg_sol_parcial.nss_imss
                    FROM   ret_sol_issste_par
                    WHERE  curp             = reg_sol_parcial.curp
                    AND    estado_solicitud = gr_estado.enviado

                WHEN "37"
                    SELECT MAX(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par
                    WHERE  curp             = reg_sol_parcial.curp
                    AND    estado_solicitud = gr_estado.diag_parcial_op34

                    SELECT folio        ,
                           consecutivo  ,
                           nss_imss
                    INTO   reg_sol_parcial.folio       ,
                           reg_sol_parcial.consecutivo ,
                           reg_sol_parcial.nss_imss
                    FROM   ret_sol_issste_par
                    WHERE  curp             = reg_sol_parcial.curp
                    AND    estado_solicitud = gr_estado.diag_parcial_op34
                    AND    fecha_envio      = fec_envio
            END CASE
        ELSE
            CASE lc_tipo_mov
                WHEN "34"
                    SELECT folio        ,
                           consecutivo
                    INTO   reg_sol_parcial.folio       ,
                           reg_sol_parcial.consecutivo
                    FROM   ret_sol_issste_par
                    WHERE  nss_imss         = reg_sol_parcial.nss_imss
                    AND    estado_solicitud = gr_estado.enviado

                WHEN "37"
                    SELECT MAX(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par
                    WHERE  nss_imss         = reg_sol_parcial.nss_imss
                    AND    estado_solicitud = gr_estado.diag_parcial_op34

                    SELECT folio        ,
                           consecutivo
                    INTO   reg_sol_parcial.folio       ,
                           reg_sol_parcial.consecutivo
                    FROM   ret_sol_issste_par
                    WHERE  nss_imss         = reg_sol_parcial.nss_imss
                    AND    estado_solicitud = gr_estado.diag_parcial_op34
                    AND    fecha_envio      = fec_envio
            END CASE
        END IF

        IF lc_tipo_mov = "34" THEN
            -- Validaciones para operacion 34

            LET li_estado_diag =  gr_estado.diag_parcial_op34

            LET reg_sol_parcial.diag_procesar      = carga_reg[344,346]

            IF reg_sol_parcial.diag_procesar IS NULL OR
               reg_sol_parcial.diag_procesar = "   " THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS  DIAGNOSTICO DE PROCESAR ... <ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            ELSE
                LET lc_diagnostico = reg_sol_parcial.diag_procesar
            END IF

            --***********NUEVOS CAMPOS DEL LAYOUT ISSSTE**********************
            --PARA TODOS LOS RETIROS
            LET reg_sol_parcial.tipo_retiro        = carga_reg[183,184]
            LET reg_sol_parcial.id_procesar        = carga_reg[654,661]
            LET reg_sol_parcial.nom_completo_icefa = carga_reg[662,781]

            --PARA RETIROS 3
            IF reg_sol_parcial.tipo_retiro = 3 THEN
               LET reg_sol_parcial.nom_beneficiario   = carga_reg[782,901]
            ELSE
            	 INITIALIZE reg_sol_parcial.nom_beneficiario TO NULL
            END IF

            --PARA RETIROS 2
            IF reg_sol_parcial.tipo_retiro = 2 THEN
               LET reg_sol_parcial.rfc_pension        = carga_reg[902,914 ]
               LET reg_sol_parcial.paterno_pension    = carga_reg[915,954 ]
               LET reg_sol_parcial.materno_pension    = carga_reg[955,994 ]
               LET reg_sol_parcial.nombres_pension    = carga_reg[995,1034]
            ELSE
               INITIALIZE reg_sol_parcial.rfc_pension     TO NULL
               INITIALIZE reg_sol_parcial.paterno_pension TO NULL
               INITIALIZE reg_sol_parcial.materno_pension TO NULL
               INITIALIZE reg_sol_parcial.nombres_pension TO NULL
            END IF

            UPDATE ret_sol_issste_par
            SET    estado_solicitud = li_estado_diag,
                   diag_procesar    = lc_diagnostico,
                   id_procesar        = reg_sol_parcial.id_procesar       ,
                   nom_completo_icefa = reg_sol_parcial.nom_completo_icefa,
                   nom_beneficiario   = reg_sol_parcial.nom_beneficiario  ,
                   rfc_pension        = reg_sol_parcial.rfc_pension       ,
                   paterno_pension    = reg_sol_parcial.paterno_pension   ,
                   materno_pension    = reg_sol_parcial.materno_pension   ,
                   nombres_pension    = reg_sol_parcial.nombres_pension
            WHERE  consecutivo      = reg_sol_parcial.consecutivo

            LET ultimo_folio = reg_sol_parcial.folio

        ELSE
            -- Validaciones para operacion 37
            LET li_estado_diag =  gr_estado.diag_parcial_op37

            LET reg_sol_parcial.estado_sub_ret    = carga_reg[349,350]

            IF reg_sol_parcial.estado_sub_ret IS NULL OR
               reg_sol_parcial.estado_sub_ret = "  " THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS  ESTADO SUBCUENTA RETIRO <ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            ELSE
                LET lc_diagnostico = reg_sol_parcial.estado_sub_ret
            END IF

            UPDATE ret_sol_issste_par
            SET    estado_solicitud = li_estado_diag,
                   estado_sub_ret   = lc_diagnostico
            WHERE  consecutivo      = reg_sol_parcial.consecutivo

            SELECT MAX(folio) + 1
            INTO   ultimo_folio
            FROM   glo_folio

            IF ultimo_folio IS NULL THEN
                LET ultimo_folio = 1
            END IF

            INSERT INTO glo_folio VALUES(ultimo_folio)
        END IF -- Validaciones por tipo de movimiento

        UPDATE ret_cza_lote
        SET folio         = ultimo_folio
        WHERE nom_archivo = reg_2.nom_archivo

        ----- DESMARCA -----

        IF NOT(lc_diagnostico = 400 OR lc_diagnostico = 01) THEN

            SELECT movimiento
            INTO   v_marca_ent
            FROM   tab_retiro_issste
            WHERE  tipo_retiro = reg_sol_parcial.tipo_retiro

            LET reg_20.estado_marca = 40
            LET reg_20.marca_causa  = 0

            IF reg_sol_parcial.nss_imss IS NULL OR
               reg_sol_parcial.nss_imss = " " THEN
                SELECT nss_imss
                INTO   reg_sol_parcial.nss_imss
                FROM   ret_sol_issste_par
                WHERE  curp             = reg_sol_parcial.curp
                AND    estado_solicitud = li_estado_diag
                AND    consecutivo      = reg_sol_parcial.consecutivo
            END IF

            LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta('",
                               reg_sol_parcial.nss_imss,"',",v_marca_ent,
                               ",",reg_sol_parcial.consecutivo,",",
                               reg_20.estado_marca,",",reg_20.marca_causa
                               ,",' ",usuario,"')"

            PREPARE eje_desmar_2 FROM v_desmarca
            EXECUTE eje_desmar_2
        END IF
    END IF

END FUNCTION


FUNCTION carga_tabla_tmp()
#sp--------------------
    DEFINE #loc #char
        c10_fecha             CHAR(10),
        lc_tipo_mov           CHAR(2)

    INITIALIZE reg_sol_total TO NULL

    LET lc_tipo_mov = carga_reg[5,6]

    ---- CARGA DE ARCHIVO RETIROS TOTALES
    IF lc_tipo_mov = "28" OR lc_tipo_mov = "31" THEN

        LET reg_sol_total.nss_imss    = carga_reg[010,020]
        LET reg_sol_total.curp        = carga_reg[032,049]
        LET reg_sol_total.tipo_retiro = carga_reg[191,192]


        LET cont_det = cont_det + 1
        DISPLAY " TOTAL REGISTROS DE DETALLE     : ",cont_det AT 11,8

        IF reg_sol_total.nss_imss IS NULL OR
           reg_sol_total.nss_imss = " " THEN

            CASE lc_tipo_mov
                WHEN "28"
                    SELECT max(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot
                    WHERE  curp             = reg_sol_total.curp
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado

                    SELECT folio               ,
                           consecutivo         ,
                           nss_imss
                    INTO   reg_sol_total.folio       ,
                           reg_sol_total.consecutivo ,
                           reg_sol_total.nss_imss
                    FROM   ret_sol_issste_tot
                    WHERE  curp             = reg_sol_total.curp
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado
                    AND    fecha_envio      = fec_envio

                WHEN "31"
                    SELECT max(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot
                    WHERE  curp             = reg_sol_total.curp
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_total_op28

                    SELECT folio               ,
                           consecutivo         ,
                           nss_imss
                    INTO   reg_sol_total.folio       ,
                           reg_sol_total.consecutivo ,
                           reg_sol_total.nss_imss
                    FROM   ret_sol_issste_tot
                    WHERE  curp             = reg_sol_total.curp
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_total_op28
                    AND    fecha_envio      = fec_envio
            END CASE
        ELSE
            CASE lc_tipo_mov
                WHEN "28"
                    SELECT max(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot
                    WHERE  nss_imss         = reg_sol_total.nss_imss
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado

                    SELECT folio               ,
                           consecutivo
                    INTO   reg_sol_total.folio ,
                           reg_sol_total.consecutivo
                    FROM   ret_sol_issste_tot
                    WHERE  nss_imss         = reg_sol_total.nss_imss
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado
                    AND    fecha_envio      = fec_envio

                WHEN "31"
                    SELECT max(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot
                    WHERE  nss_imss         = reg_sol_total.nss_imss
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_total_op28

                    SELECT folio               ,
                           consecutivo
                    INTO   reg_sol_total.folio ,
                           reg_sol_total.consecutivo
                    FROM   ret_sol_issste_tot
                    WHERE  nss_imss         = reg_sol_total.nss_imss
                    AND    tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_total_op28
                    AND    fecha_envio      = fec_envio

            END CASE
        END IF

        --Se cargan el archivo plano a la tabla temporal de totales
        CALL carga_archivo_totales(reg_sol_total.tipo_retiro)

    END IF

    ---- CARGA DE ARCHIVO RETIROS PARCIALES
    IF lc_tipo_mov = "34" OR lc_tipo_mov = "37" THEN

        LET reg_sol_parcial.nss_imss    = carga_reg[010,020]
        LET reg_sol_parcial.curp        = carga_reg[032,049]
        LET reg_sol_parcial.tipo_retiro = carga_reg[183,184]

        LET cont_det = cont_det + 1
        DISPLAY " TOTAL REGISTROS DE DETALLE     : ",cont_det AT 11,8

        IF reg_sol_parcial.nss_imss IS NULL OR
           reg_sol_parcial.nss_imss = " " THEN
            CASE lc_tipo_mov
                WHEN "34"
                    SELECT MAX(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par
                    WHERE  curp             = reg_sol_parcial.curp
                    AND    tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado

                    SELECT folio       ,
                           consecutivo ,
                           nss_imss
                    INTO   reg_sol_parcial.folio       ,
                           reg_sol_parcial.consecutivo ,
                           reg_sol_parcial.nss_imss
                    FROM   ret_sol_issste_par
                    WHERE  curp             = reg_sol_parcial.curp
                    AND    tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado
                    AND    fecha_envio      = fec_envio

                WHEN "37"
                    SELECT MAX(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par
                    WHERE  curp             = reg_sol_parcial.curp
                    AND    tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_parcial_op34

                    SELECT folio      ,
                           consecutivo,
                           nss_imss
                    INTO   reg_sol_parcial.folio       ,
                           reg_sol_parcial.consecutivo ,
                           reg_sol_parcial.nss_imss
                    FROM   ret_sol_issste_par
                    WHERE  curp             = reg_sol_parcial.curp
                    AND    tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_parcial_op34
                    AND    fecha_envio      = fec_envio
                END CASE
        ELSE
            CASE lc_tipo_mov
                WHEN "34"
                    SELECT MAX(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par
                    WHERE  curp             = reg_sol_parcial.curp
                    AND    tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado

                    SELECT folio       ,
                           consecutivo
                    INTO   reg_sol_parcial.folio       ,
                           reg_sol_parcial.consecutivo
                    FROM   ret_sol_issste_par
                    WHERE  nss_imss         = reg_sol_parcial.nss_imss
                    AND    tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    estado_solicitud = gr_estado.enviado
                    AND    fecha_envio      = fec_envio

                WHEN "37"
                    SELECT MAX(fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par
                    WHERE  nss_imss         = reg_sol_parcial.nss_imss
                    AND    tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_parcial_op34

                    SELECT folio      ,
                           consecutivo
                    INTO   reg_sol_parcial.folio      ,
                           reg_sol_parcial.consecutivo
                    FROM   ret_sol_issste_par
                    WHERE  nss_imss         = reg_sol_parcial.nss_imss
                    AND    tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    estado_solicitud = gr_estado.diag_parcial_op34
                    AND    fecha_envio      = fec_envio
            END CASE
        END IF

        --Se cargan el archivo plano a la tabla temporal de totales
        CALL carga_archivo_parciales(reg_sol_parcial.tipo_retiro)
    END IF
END FUNCTION

FUNCTION carga_archivo_totales(ls_tipo_retiro)

    DEFINE
        ls_tipo_retiro      SMALLINT

    LET reg_sol_total.nss_issste        = carga_reg[021,031]

    LET reg_sol_total.curp              = carga_reg[032,049]

--    fecha_nac es para retiro 1
    LET c10_fecha                       = carga_reg[187,188],"/",
                                          carga_reg[189,190],"/",
                                          carga_reg[183,186]
    LET reg_sol_total.fecha_nac         = c10_fecha

    LET reg_sol_total.tipo_beneficio    = carga_reg[193,195]

    LET reg_sol_total.n_pensionista     = carga_reg[196,202]

    LET reg_sol_total.entidad_tramite   = carga_reg[203,204]

    IF ls_tipo_retiro = 2 OR ls_tipo_retiro = 7 THEN
        LET c10_fecha                       = carga_reg[209,210],"/",
                                              carga_reg[211,212],"/",
                                              carga_reg[205,208]
        LET reg_sol_total.fecha_ini_pen     = c10_fecha
    END IF

    LET reg_sol_total.cve_doc_prob      = carga_reg[213,213]

    LET c10_fecha                       = carga_reg[218,219],"/",
                                          carga_reg[220,221],"/",
                                          carga_reg[214,217]
    LET reg_sol_total.fec_sol_susp      = c10_fecha

    LET c10_fecha                       = carga_reg[226,227],"/",
                                          carga_reg[228,229],"/",
                                          carga_reg[222,225]
    LET reg_sol_total.fec_sol_reac      = c10_fecha

    LET c10_fecha                       = carga_reg[234,235],"/",
                                          carga_reg[236,237],"/",
                                          carga_reg[230,233]
    LET reg_sol_total.fecha_baja        = c10_fecha

    LET c10_fecha                       = carga_reg[252,253],"/",
                                          carga_reg[254,255],"/",
                                          carga_reg[248,251]
    LET reg_sol_total.fecha_solicitud   = c10_fecha

    LET reg_sol_total.tipo_inversion    = carga_reg[256,257]

    LET c10_fecha                       = carga_reg[262,263],"/",
                                          carga_reg[264,265],"/",
                                          carga_reg[258,261]
    LET reg_sol_total.fecha_val_ahorro  = c10_fecha

    LET c6_acciones                      = carga_reg[266,268],".",
                                           carga_reg[269,270]
    LET reg_sol_total.porcentaje_ahorro  = c6_acciones

    LET c19_acciones                    = carga_reg[355,370],".",
                                          carga_reg[371,372]
    LET reg_sol_total.impt_ahorro_ret   = c19_acciones

    LET reg_sol_total.n_oficio          = carga_reg[391,400]

    LET c10_fecha                       = carga_reg[405,406],"/",
                                          carga_reg[407,408],"/",
                                          carga_reg[401,404]
    LET reg_sol_total.fecha_oficio      = c10_fecha

    LET reg_sol_total.entidad_oficio    = carga_reg[409,410]

    LET c10_fecha                       = carga_reg[415,416],"/",
                                          carga_reg[417,418],"/",
                                          carga_reg[411,414]
    LET reg_sol_total.fecha_val_viv     = c10_fecha

    LET c6_acciones                      = carga_reg[419,421],".",
                                           carga_reg[422,423]
    LET reg_sol_total.porcentaje_fon_viv = c6_acciones

    LET c15_acciones                    = carga_reg[424,431],".",
                                          carga_reg[432,437]

    LET reg_sol_total.impt_fon_viv_liq  = c15_acciones

    LET reg_sol_total.diag_procesar     = carga_reg[438,440]

    LET reg_sol_total.status_fondo_viv  = carga_reg[443,444]

    LET reg_sol_total.status_sub_ret    = carga_reg[445,446]

    LET reg_sol_total.rfc_dependencia   = carga_reg[455,466]

    LET reg_sol_total.nom_dependencia   = carga_reg[467,506]

    LET reg_sol_total.cve_ramo          = carga_reg[507,511]

    LET reg_sol_total.cve_pagaduria     = carga_reg[512,516]

--***********NUEVOS CAMPOS DEL LAYOUT ISSSTE**********************
    --PARA TODOS LOS RETIROS
    LET reg_sol_total.id_procesar        = carga_reg[517,524]
    LET reg_sol_total.nom_completo_icefa = carga_reg[525,644]

    --PARA RETIROS 3
    IF ls_tipo_retiro = 3 THEN
       LET reg_sol_total.nom_beneficiario   = carga_reg[645,764]
    END IF

    --PARA RETIROS 2
    IF ls_tipo_retiro = 2 THEN
       LET reg_sol_total.rfc_pension        = carga_reg[765,777]
       LET reg_sol_total.paterno_pension    = carga_reg[778,817]
       LET reg_sol_total.materno_pension    = carga_reg[818,857]
       LET reg_sol_total.nombres_pension    = carga_reg[858,897]
    END IF

    INSERT INTO sol_issste_tot_temp
    VALUES ( reg_sol_total.* )


    -- Se carga la informacion para los montos de las siefores
    LET reg_monto_issste.nss            = reg_sol_total.nss_imss

    LET reg_monto_issste.curp           = carga_reg[032,049]

    LET c15_acciones                    = carga_reg[271,278],".",
                                          carga_reg[279,284]
    LET reg_monto_issste.acciones_sie1  = c15_acciones

    LET c15_acciones                    = carga_reg[285,292],".",
                                          carga_reg[293,298]
    LET reg_monto_issste.acciones_sie2  = c15_acciones

    LET c15_acciones                    = carga_reg[299,306],".",
                                          carga_reg[307,312]
    LET reg_monto_issste.acciones_sie3  = c15_acciones

    LET c15_acciones                    = carga_reg[313,320],".",
                                          carga_reg[321,326]
    LET reg_monto_issste.acciones_sie4  = c15_acciones

    LET c15_acciones                    = carga_reg[327,334],".",
                                          carga_reg[335,340]
    LET reg_monto_issste.acciones_sie5  = c15_acciones

    INSERT INTO ret_monto_sie_issste_temp
    VALUES (reg_monto_issste.*)

END FUNCTION


FUNCTION carga_archivo_parciales(ls_tipo_retiro)

    DEFINE
        ls_tipo_retiro      SMALLINT

    LET reg_sol_parcial.nss_issste       = carga_reg[021,031]
    LET reg_sol_parcial.curp             = carga_reg[032,049]
    LET reg_sol_parcial.nombres          = carga_reg[050,089]
    LET reg_sol_parcial.paterno          = carga_reg[090,129]
    LET reg_sol_parcial.materno          = carga_reg[130,169]
    LET reg_sol_parcial.rfc              = carga_reg[170,182]
    LET reg_sol_parcial.tipo_beneficio   = carga_reg[185,187]

    LET c10_fecha                        = carga_reg[192,193],"/",
                                           carga_reg[194,195],"/",
                                           carga_reg[188,191]
    LET reg_sol_parcial.fecha_baja       = c10_fecha

    LET c10_fecha                        = carga_reg[210,211],"/",
                                           carga_reg[212,213],"/",
                                           carga_reg[206,209]
    LET reg_sol_parcial.fecha_solicitud  = c10_fecha

    LET reg_sol_parcial.tipo_inversion   = carga_reg[214,215]

    LET c10_fecha                        = carga_reg[220,221],"/",
                                           carga_reg[222,223],"/",
                                           carga_reg[216,219]
    LET reg_sol_parcial.fecha_valoriza   = c10_fecha

    LET c19_acciones                     = carga_reg[308,319],".",
                                           carga_reg[320,325]
    LET reg_sol_parcial.importe_retiro   = c19_acciones

    --aplica para tipo 34
    LET reg_sol_parcial.diag_procesar    = carga_reg[344,346]
    --

    --aplica para tipo 37
    LET reg_sol_parcial.estado_sub_ret   = carga_reg[349,350]
    --

    IF ls_tipo_retiro = 4 THEN
        LET reg_sol_parcial.folio_dictamen   = carga_reg[196,205]

        LET c10_fecha                        = carga_reg[355,356],"/",
                                               carga_reg[357,358],"/",
                                               carga_reg[351,354]
        LET reg_sol_parcial.fecha_entrega_rec= c10_fecha

    END IF


    --***********NUEVOS CAMPOS DEL LAYOUT ISSSTE**********************
    --PARA TODOS LOS RETIROS
    LET reg_sol_parcial.id_procesar        = carga_reg[654,661]
    LET reg_sol_parcial.nom_completo_icefa = carga_reg[662,781]

    --PARA RETIROS 3
    IF ls_tipo_retiro = 3 THEN
       LET reg_sol_parcial.nom_beneficiario   = carga_reg[782,901]
    END IF

    --PARA RETIROS 2
    IF ls_tipo_retiro = 2 THEN
       LET reg_sol_parcial.rfc_pension        = carga_reg[902,914 ]
       LET reg_sol_parcial.paterno_pension    = carga_reg[915,954 ]
       LET reg_sol_parcial.materno_pension    = carga_reg[955,994 ]
       LET reg_sol_parcial.nombres_pension    = carga_reg[995,1034]
    END IF

    INSERT INTO sol_issste_par_temp
    VALUES (reg_sol_parcial.*)

    -- Se carga la informacion para los montos de las siefores
    LET reg_monto_issste.nss            = reg_sol_parcial.nss_imss

    LET reg_monto_issste.curp           = carga_reg[032,049]

    LET c15_acciones                    = carga_reg[224,231],".",
                                          carga_reg[232,237]
    LET reg_monto_issste.acciones_sie1  = c15_acciones

    LET c15_acciones                    = carga_reg[238,245],".",
                                          carga_reg[246,251]
    LET reg_monto_issste.acciones_sie2  = c15_acciones

    LET c15_acciones                    = carga_reg[252,259],".",
                                          carga_reg[260,265]
    LET reg_monto_issste.acciones_sie3  = c15_acciones

    LET c15_acciones                    = carga_reg[266,273],".",
                                          carga_reg[274,279]
    LET reg_monto_issste.acciones_sie4  = c15_acciones

    LET c15_acciones                    = carga_reg[280,287],".",
                                          carga_reg[288,293]
    LET reg_monto_issste.acciones_sie5  = c15_acciones

    INSERT INTO ret_monto_sie_issste_temp
    VALUES (reg_monto_issste.*)

END FUNCTION


FUNCTION compara_det()
#cd-----------------
    DEFINE #loc #r_total_tmp
        r_total_db     RECORD LIKE ret_sol_issste_tot.*,
        r_total_tmp    RECORD LIKE ret_sol_issste_tot.*

    DEFINE #loc #r_parcial_tmp
        r_parcial_db    RECORD LIKE ret_sol_issste_par.*,
        r_parcial_tmp   RECORD LIKE ret_sol_issste_par.*

    DEFINE
        lc_tipo_mov     CHAR(2)

    LET lc_tipo_mov = carga_reg[5,6]

    -- COMPARACION DE RETIROS TOTALES --
    IF lc_tipo_mov = "28" OR lc_tipo_mov = "31" THEN

        LET reg_sol_total.nss_imss    = carga_reg[010,020]
        LET reg_sol_total.curp        = carga_reg[032,049]
        LET reg_sol_total.tipo_retiro = carga_reg[191,192]

        IF reg_sol_total.nss_imss IS NULL OR
           reg_sol_total.nss_imss = " " THEN

            CASE lc_tipo_mov
                WHEN "28"
                    SELECT max(A.fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot A
                    WHERE  A.curp = reg_sol_total.curp
                    AND    A.tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.enviado

                    SELECT A.folio             ,
                           A.consecutivo       ,
                           A.nss_imss
                    INTO   reg_sol_total.folio       ,
                           reg_sol_total.consecutivo ,
                           reg_sol_total.nss_imss
                    FROM   ret_sol_issste_tot A
                    WHERE  A.curp             = reg_sol_total.curp
                    AND    A.tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.enviado
                    AND    A.fecha_envio      = fec_envio

                WHEN "31"
                    SELECT max(A.fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot A
                    WHERE  A.curp = reg_sol_total.curp
                    AND    A.tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.diag_total_op28

                    SELECT A.folio             ,
                           A.consecutivo       ,
                           A.nss_imss
                    INTO   reg_sol_total.folio       ,
                           reg_sol_total.consecutivo ,
                           reg_sol_total.nss_imss
                    FROM   ret_sol_issste_tot A
                    WHERE  A.curp = reg_sol_total.curp
                    AND    A.tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.diag_total_op28
                    AND    A.fecha_envio      = fec_envio
{
                    DISPLAY "reg_sol_total.tipo_retiro:", reg_sol_total.tipo_retiro
                    DISPLAY "fec_envio:", fec_envio
                    DISPLAY "reg_sol_total.curp:", reg_sol_total.curp
}
                    --PROMPT " ARCHIVO " FOR CHAR enter
            END CASE
        ELSE
            CASE lc_tipo_mov
                WHEN "28"
                    SELECT max(A.fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot A
                    WHERE  A.curp = reg_sol_total.curp
                    AND    A.tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.enviado
 
                    SELECT A.folio             ,
                           A.consecutivo
                    INTO   reg_sol_total.folio ,
                           reg_sol_total.consecutivo
                    FROM   ret_sol_issste_tot A
                    WHERE  A.nss_imss         = reg_sol_total.nss_imss
                    AND    A.tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.enviado
                    AND    A.fecha_envio      = fec_envio

                WHEN "31"
                    SELECT max(A.fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_tot A
                    WHERE  A.nss_imss         = reg_sol_total.nss_imss
                    AND    A.tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.diag_total_op28

                    SELECT A.folio             ,
                           A.consecutivo
                    INTO   reg_sol_total.folio ,
                           reg_sol_total.consecutivo
                    FROM   ret_sol_issste_tot A
                    WHERE  A.nss_imss         = reg_sol_total.nss_imss
                    AND    A.tipo_retiro      = reg_sol_total.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.diag_total_op28
                    AND    A.fecha_envio      = fec_envio
            END CASE
        END IF



        IF reg_sol_total.nss_imss IS NULL OR
           reg_sol_total.nss_imss = " " THEN

            SELECT sol_issste_tot_temp.*
            INTO   r_total_tmp.*
            FROM   sol_issste_tot_temp
            WHERE curp = reg_sol_total.curp
        ELSE
            SELECT sol_issste_tot_temp.*
            INTO   r_total_tmp.*
            FROM   sol_issste_tot_temp
            WHERE nss_imss = reg_sol_total.nss_imss
        END IF

        SELECT *
        INTO   r_total_db.*
        FROM   ret_sol_issste_tot
        WHERE  consecutivo = reg_sol_total.consecutivo

        IF r_total_tmp.consecutivo <> r_total_db.consecutivo THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... CONSECUTIVO <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_total_tmp.curp <> r_total_db.curp THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... CURP <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF
        IF r_total_tmp.tipo_retiro <> r_total_db.tipo_retiro THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... TIPO_RETIRO <ENTER> PARA SALIR "
            FOR CHAR enter
{
            DISPLAY "fec_envio:", fec_envio
            DISPLAY "reg_sol_total.curp:", reg_sol_total.curp
            DISPLAY "reg_sol_total.tipo_retiro:", reg_sol_total.tipo_retiro

            DISPLAY "r_total_db.nss_imss:", r_total_db.nss_imss
            DISPLAY "r_total_tmp.tipo_retiro:", r_parcial_tmp.tipo_retiro
            DISPLAY "r_total_db.tipo_retiro:", r_parcial_db.tipo_retiro
            DISPLAY "reg_sol_total.consecutivo:", reg_sol_total.consecutivo
}
            EXIT PROGRAM
        END IF

        IF r_total_tmp.tipo_beneficio <> r_total_db.tipo_beneficio THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... TIPO_BENEFICIO <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF
{
        IF r_total_tmp.n_pensionista <> r_total_db.n_pensionista THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... NUMERO DE PENSIONISTA <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_total_tmp.fecha_ini_pen <> r_total_db.fecha_ini_pen THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... FECHA_INI_PEN <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_total_tmp.entidad_tramite <> r_total_db.entidad_tramite THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... ENTIDAD TRAMITE  <ENTER> PARA SALIR "
            FOR CHAR enter
            --DISPLAY "r_total_tmp.entidad_tramite:", r_total_tmp.entidad_tramite
            EXIT PROGRAM


        END IF

        IF r_total_tmp.cve_doc_prob <> r_total_db.cve_doc_prob THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... CVE DOC PROBATORIO <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_total_tmp.fec_sol_susp <> r_total_db.fec_sol_susp THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... FECHA_SOL_SUSPENSION <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_total_tmp.fec_sol_reac <> r_total_db.fec_sol_reac THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... FECHA SOL REACTIVACION <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_total_tmp.fecha_solicitud <> r_total_db.fecha_solicitud THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... FECHA SOLICITUD  <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_total_tmp.tipo_inversion <> r_total_db.tipo_inversion THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... TIPO INVERSION <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_total_tmp.impt_ahorro_ret <> r_total_db.impt_ahorro_ret THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... IMPORTE AHORRO RETIRO <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF
}
        CALL compara_montos_sie(reg_sol_total.nss_imss, reg_sol_total.curp, lc_tipo_mov)

        IF lc_tipo_mov = "28" THEN
            IF r_total_tmp.diag_procesar = "   " OR r_total_tmp.diag_procesar IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... DIAGNOSTICO DE PROCESAR <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF

{
            --**NUEVOS CAMPOS LAYOUT ISSSTE*****************
            IF r_total_tmp.id_procesar = "   " OR r_total_tmp.id_procesar IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... ID PROCESAR <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF

            IF r_total_tmp.nom_completo_icefa = "   " OR r_total_tmp.nom_completo_icefa IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRE ICEFA <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF

            --PARA RETIRO 2
            IF r_total_tmp.tipo_retiro = 2 THEN
            	 IF r_total_tmp.rfc_pension = "   " OR r_total_tmp.rfc_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... RFC PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_total_tmp.paterno_pension = "   " OR r_total_tmp.paterno_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... PATERNO PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_total_tmp.materno_pension = "   " OR r_total_tmp.materno_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... MATERNO PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_total_tmp.nombres_pension = "   " OR r_total_tmp.nombres_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRES PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF
            END IF

            --PARA RETIRO 3
            IF r_total_tmp.tipo_retiro = 3 THEN
            	 IF r_total_tmp.nom_beneficiario = "   " OR r_total_tmp.nom_beneficiario IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRE BENEFICIARIO <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF
            END IF
}
        ELSE
            IF r_total_tmp.status_fondo_viv = "  " OR r_total_tmp.status_fondo_viv IS NULL THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS... STATUS FONDO VIVIENDA <ENTER> PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
            END IF

            IF r_total_tmp.status_sub_ret = "  " OR r_total_tmp.status_sub_ret IS NULL THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS... STATUS SUBCUENTA RETIRO <ENTER> PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
            END IF

{
            --**NUEVOS CAMPOS LAYOUT ISSSTE*****************
            IF r_total_tmp.id_procesar <> r_total_db.id_procesar IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... ID PROCESAR <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF

            IF r_total_tmp.nom_completo_icefa <> r_total_db.nom_completo_icefa IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRE ICEFA <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF

            --PARA RETIRO 2
            IF r_total_tmp.tipo_retiro = 2 THEN
            	 IF r_total_tmp.rfc_pension <> r_total_db.rfc_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... RFC PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_total_tmp.paterno_pension <> r_total_db.paterno_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... PATERNO PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_total_tmp.materno_pension <> r_total_db.materno_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... MATERNO PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_total_tmp.nombres_pension <> r_total_db.nombres_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRES PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF
            END IF

            --PARA RETIRO 3
            IF r_total_tmp.tipo_retiro = 3 THEN
            	 IF r_total_tmp.nom_beneficiario <> r_total_db.nom_beneficiario IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRE BENEFICIARIO <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF
            END IF
}
        END IF
    END IF -- RETIROS TOTALES

    ---- COMPARACION DE RETIROS PARCIALES ----
    IF lc_tipo_mov  = "34" OR lc_tipo_mov  = "37" THEN

        LET reg_sol_parcial.curp        = carga_reg[032,049]
        LET reg_sol_parcial.tipo_retiro = carga_reg[183,184]

        LET cont_det = cont_det + 1
        DISPLAY " TOTAL REGISTROS DE DETALLE     : ",cont_det AT 11,8

        IF reg_sol_parcial.nss_imss IS NULL OR
           reg_sol_parcial.nss_imss = " " THEN

            CASE lc_tipo_mov
                WHEN "34"
                    SELECT MAX(A.fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par A
                    WHERE  A.curp             = reg_sol_parcial.curp
                    AND    A.tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.enviado

                    SELECT A.folio          ,
                           A.consecutivo    ,
                           A.nss_imss
                    INTO   reg_sol_parcial.folio      ,
                           reg_sol_parcial.consecutivo,
                           reg_sol_parcial.nss_imss
                    FROM   ret_sol_issste_par A
                    WHERE  A.curp             = reg_sol_parcial.curp
                    AND    A.tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.enviado
                    AND    A.fecha_envio      = fec_envio

                WHEN "37"
                    SELECT MAX(A.fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par A
                    WHERE  A.curp             = reg_sol_parcial.curp
                    AND    A.tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.diag_parcial_op34

                    SELECT A.folio          ,
                           A.consecutivo    ,
                           A.nss_imss
                    INTO   reg_sol_parcial.folio       ,
                           reg_sol_parcial.consecutivo ,
                           reg_sol_parcial.nss_imss
                    FROM   ret_sol_issste_par A
                    WHERE  A.curp             = reg_sol_parcial.curp
                    AND    A.tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.diag_parcial_op34
                    AND    A.fecha_envio      = fec_envio
            END CASE
        ELSE
            CASE lc_tipo_mov
                WHEN "34"
                    SELECT MAX(A.fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par A
                    WHERE  A.curp             = reg_sol_parcial.curp
                    AND    A.tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.enviado

                    SELECT A.folio             ,
                           A.consecutivo
                    INTO   reg_sol_parcial.folio ,
                           reg_sol_parcial.consecutivo
                    FROM   ret_sol_issste_par A
                    WHERE  A.nss_imss         = reg_sol_parcial.nss_imss
                    AND    A.tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.enviado
                    AND    A.fecha_envio      = fec_envio

                WHEN "37"
                    SELECT MAX(A.fecha_envio)
                    INTO   fec_envio
                    FROM   ret_sol_issste_par A
                    WHERE  A.nss_imss         = reg_sol_parcial.nss_imss
                    AND    A.tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.diag_parcial_op34

                    SELECT A.folio             ,
                           A.consecutivo
                    INTO   reg_sol_parcial.folio ,
                           reg_sol_parcial.consecutivo
                    FROM   ret_sol_issste_par A
                    WHERE  A.nss_imss         = reg_sol_parcial.nss_imss
                    AND    A.tipo_retiro      = reg_sol_parcial.tipo_retiro
                    AND    A.estado_solicitud = gr_estado.diag_parcial_op34
                    AND    A.fecha_envio      = fec_envio
            END CASE
        END IF

        IF reg_sol_parcial.nss_imss IS NULL OR
           reg_sol_parcial.nss_imss = " " THEN
            SELECT sol_issste_par_temp.*
            INTO   r_parcial_tmp.*
            FROM   sol_issste_par_temp
            WHERE curp = reg_sol_parcial.curp
        ELSE
            SELECT sol_issste_par_temp.*
            INTO   r_parcial_tmp.*
            FROM   sol_issste_par_temp
            WHERE nss_imss = reg_sol_parcial.nss_imss
        END IF

        SELECT *
        INTO   r_parcial_db.*
        FROM   ret_sol_issste_par
        WHERE  consecutivo = reg_sol_parcial.consecutivo



        IF r_parcial_tmp.consecutivo <> r_parcial_db.consecutivo THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... CONSECUTIVO <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_parcial_tmp.curp <> r_parcial_db.curp THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... CURP <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_parcial_tmp.tipo_retiro <> r_parcial_db.tipo_retiro THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... TIPO_RETIRO <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_parcial_tmp.rfc <> r_parcial_db.rfc THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... RFC <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_parcial_tmp.tipo_beneficio <> r_parcial_db.tipo_beneficio THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... TIPO_BENEFICIO <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_parcial_tmp.fecha_solicitud <> r_parcial_db.fecha_solicitud THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... FECHA SOLICITUD  <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF r_parcial_tmp.tipo_inversion <> r_parcial_db.tipo_inversion THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... TIPO INVERSION <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        CALL compara_montos_sie(reg_sol_parcial.nss_imss, reg_sol_parcial.curp, lc_tipo_mov)

        IF r_parcial_tmp.importe_retiro <> r_parcial_db.importe_retiro THEN
            PROMPT " ARCHIVO CON INCONSISTENCIAS... IMPORTE AHORRO RETIRO <ENTER> PARA SALIR"
            FOR CHAR enter
            EXIT PROGRAM
        END IF

        IF lc_tipo_mov = "34" THEN
            IF r_parcial_tmp.diag_procesar = "   " OR r_parcial_tmp.diag_procesar IS NULL THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS... DIAGNOSTICO DE PROCESAR <ENTER> PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
            END IF

{
            --**NUEVOS CAMPOS LAYOUT ISSSTE*****************
            IF r_parcial_tmp.id_procesar = "   " OR r_parcial_tmp.id_procesar IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... ID PROCESAR <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF

            IF r_parcial_tmp.nom_completo_icefa = "   " OR r_parcial_tmp.nom_completo_icefa IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRE ICEFA <ENTER> PARA SALIR"
               FOR CHAR enter
               EXIT PROGRAM
            END IF

            --PARA RETIRO 2
            IF r_parcial_tmp.tipo_retiro = 2 THEN
            	 IF r_parcial_tmp.rfc_pension = "   " OR r_parcial_tmp.rfc_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... RFC PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_parcial_tmp.paterno_pension = "   " OR r_parcial_tmp.paterno_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... PATERNO PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_parcial_tmp.materno_pension = "   " OR r_parcial_tmp.materno_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... MATERNO PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_parcial_tmp.nombres_pension = "   " OR r_parcial_tmp.nombres_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRES PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF
            END IF

            --PARA RETIRO 3
            IF r_parcial_tmp.tipo_retiro = 3 THEN
            	 IF r_parcial_tmp.nom_beneficiario = "   " OR r_parcial_tmp.nom_beneficiario IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRE BENEFICIARIO <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF
            END IF
}

        ELSE
            IF r_parcial_tmp.estado_sub_ret = "  " OR r_parcial_tmp.estado_sub_ret IS NULL THEN
                PROMPT " ARCHIVO CON INCONSISTENCIAS... ESTADO SUBCUENTA RETIRO <ENTER> PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
            END IF

{
            --**NUEVOS CAMPOS LAYOUT ISSSTE*****************
            IF r_parcial_tmp.id_procesar <> r_parcial_db.id_procesar IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... ID PROCESAR <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF

            IF r_parcial_tmp.nom_completo_icefa <> r_parcial_db.nom_completo_icefa IS NULL THEN
               PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRE ICEFA <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
            END IF

            --PARA RETIRO 2
            IF r_parcial_tmp.tipo_retiro = 2 THEN
            	 IF r_parcial_tmp.rfc_pension <> r_parcial_db.rfc_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... RFC PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_parcial_tmp.paterno_pension <> r_parcial_db.paterno_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... PATERNO PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_parcial_tmp.materno_pension <> r_parcial_db.materno_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... MATERNO PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF

               IF r_parcial_tmp.nombres_pension <> r_parcial_db.nombres_pension IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRES PENSION <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF
            END IF

            --PARA RETIRO 3
            IF r_parcial_tmp.tipo_retiro = 3 THEN
            	 IF r_parcial_tmp.nom_beneficiario <> r_parcial_db.nom_beneficiario IS NULL THEN
                  PROMPT " ARCHIVO CON INCONSISTENCIAS... NOMBRE BENEFICIARIO <ENTER> PARA SALIR "
                  FOR CHAR enter
                  EXIT PROGRAM
               END IF
            END IF
}
        END IF
    END IF -- RETIROS PARCIALES --

END FUNCTION

FUNCTION compara_montos_sie(p_nss, p_curp, p_estado)

    DEFINE
        p_nss   LIKE ret_sol_issste_tot.nss_imss,
        p_curp  LIKE ret_sol_issste_tot.curp

    DEFINE
        p_estado    CHAR(03)

    DEFINE
        la_sie_db,
        la_sie_temp     ARRAY [5] OF DECIMAL(16,6)

    DEFINE
        lc_sie          CHAR(1),
        lc_query        CHAR(300),
        lc_tabla        CHAR(30) ,
        lc_where        CHAR(30) ,
        lc_parametro    CHAR(18)

    DEFINE
        ld_monto_acc    DECIMAL(16,6)

    DEFINE
        ls_estado_sol   ,
        ls_sie          ,
        ls_cont         SMALLINT

    CASE p_estado
        WHEN "28"
            LET ls_estado_sol = gr_estado.enviado
            LET lc_tabla      = " , ret_sol_issste_tot B "

        WHEN "31"
            LET ls_estado_sol = gr_estado.diag_total_op28
            LET lc_tabla      = " , ret_sol_issste_tot B "

        WHEN "34"
            LET ls_estado_sol = gr_estado.enviado
            LET lc_tabla      = " , ret_sol_issste_par B "

        WHEN "37"
            LET ls_estado_sol = gr_estado.diag_parcial_op34
            LET lc_tabla      = " , ret_sol_issste_par B "
    END CASE

    LET ls_sie       = 0
    LET ld_monto_acc = 0

    FOR ls_cont = 1 TO 5
        LET la_sie_db[ls_cont] = 0
    END FOR

    IF p_nss IS NULL OR p_nss = " " THEN
        LET lc_parametro = p_curp
        LET lc_where = " WHERE  B.curp = ?"
    ELSE
        LET lc_parametro = p_nss CLIPPED
        LET lc_parametro = lc_parametro CLIPPED
        LET lc_where = " WHERE  A.nss = ?"
    END IF

    -- Obtenemos los valores actuales de ret_monto_sie_issste
    LET lc_query = " SELECT A.siefore, "            ,
                   "        A.saldo_acciones "      ,
                   " FROM   ret_monto_sie_issste A ",
                   lc_tabla                         ,
                   lc_where                         ,
                   " AND    A.nss = B.nss_imss"     ,
                   " AND    A.folio = B.folio"      ,
                   " AND    B.estado_solicitud = ?" ,
                   " AND    A.subcuenta = 13"

    LET lc_query = lc_query CLIPPED
    PREPARE eje_query FROM lc_query

    DECLARE cur_sie CURSOR FOR eje_query

    FOREACH cur_sie USING lc_parametro,
                          ls_estado_sol
                    INTO  ls_sie,
                          ld_monto_acc

        LET la_sie_db[ls_sie] = ld_monto_acc

    END FOREACH

    LET lc_query = " "

    -- Obtenemos los valores almacenados en la ret_monto_sie temporal
    LET lc_query = " SELECT A.acciones_sie1, "      ,
                   "        A.acciones_sie2, "      ,
                   "        A.acciones_sie3, "      ,
                   "        A.acciones_sie4, "      ,
                   "        A.acciones_sie5  "      ,
                   " FROM   ret_monto_sie_issste_temp A ",
                   lc_tabla                         ,
                   lc_where

    LET lc_query = lc_query CLIPPED

    PREPARE eje_query_tmp FROM lc_query

    DECLARE cur_sie_tmp CURSOR FOR eje_query_tmp

    FOREACH cur_sie_tmp USING lc_parametro
                        INTO la_sie_temp[1],
                             la_sie_temp[2],
                             la_sie_temp[3],
                             la_sie_temp[4],
                             la_sie_temp[5]
        EXIT FOREACH

    END FOREACH

    LET lc_query = " "

    FOR ls_cont = 1 TO 5

        IF la_sie_db[ls_cont] <> la_sie_temp[ls_cont] THEN

            LET lc_sie = ls_cont
            LET lc_query = " ARCHIVO CON INCONSISTENCIAS... ACCIONES SIE ", ls_cont,
                           " <ENTER> PARA SALIR "
            PROMPT lc_query CLIPPED FOR CHAR enter
            EXIT PROGRAM
        END IF
    END FOR
END FUNCTION



FUNCTION crea_tablas_temp()

    CREATE TEMP TABLE ret_monto_sie_issste_temp
    (   nss                   CHAR(11)      ,
        curp                  CHAR(18)      ,
        acciones_sie1         DECIMAL(16,6) ,
        acciones_sie2         DECIMAL(16,6) ,
        acciones_sie3         DECIMAL(16,6) ,
        acciones_sie4         DECIMAL(16,6) ,
        acciones_sie5         DECIMAL(16,6)
    )

    CREATE TEMP TABLE sol_issste_tot_temp
    (
     folio integer                  ,
     folio_sol integer              ,
     tipo_id  char(1)               ,
     consecutivo  decimal(11,0)     ,--consecutivo decimal(11,0) not null
     nss_imss char(11)              ,--nss_imss    char(11)      not null
     nss_issste char(11)            ,--nss_issste  char(11)      not null
     curp char(18)                  ,
     fecha_nac date                 ,
     tipo_retiro smallint           ,
     tipo_beneficio smallint        ,
     n_pensionista char(7)          ,
     entidad_tramite char(2)        ,
     fecha_ini_pen date             ,
     cve_doc_prob smallint          ,
     fec_sol_susp date              ,
     fec_sol_reac date              ,
     fecha_baja   date              ,
     fecha_solicitud date           ,
     tipo_inversion smallint        ,
     fecha_val_ahorro date          ,
     porcentaje_ahorro decimal(6,2) ,
     acciones_siefore1 decimal(16,6),
     acciones_siefore2 decimal(16,6),
     impt_ahorro_ret decimal(16,2)  ,
     n_oficio char(10)              ,
     fecha_oficio date              ,
     entidad_oficio char(2)         ,
     fecha_val_viv date             ,
     porcentaje_fon_viv decimal(6,2),
     impt_fon_viv_liq decimal(16,6) ,
     diag_procesar char(3)          ,
     diag_issste char(3)            ,
     status_fondo_viv char(2)       ,
     status_sub_ret   char(2)       ,
     rfc_dependencia  char(12)      ,
     nom_dependencia  char(40)      ,
     cve_ramo char(5)               ,
     cve_pagaduria char(5)          ,
     --
     id_procesar         CHAR(8)    ,
     nom_completo_icefa  CHAR(120)  ,
     nom_beneficiario    CHAR(120)  ,
     rfc_pension         CHAR(13)   ,
     paterno_pension     CHAR(40)   ,
     materno_pension     CHAR(40)   ,
     nombres_pension     CHAR(40)   ,
     --
     estado_solicitud smallint      ,
     fecha_captura date             ,
     fecha_confirma date            ,
     fecha_modifica date            ,
     fecha_liquida date             ,
     fecha_envio date               ,
     usuario_captura char(12)       ,
     usuario_confirma char(12)      ,
     usuario_modifica char(12)      ,
     carta smallint                 ,
     grupo smallint                 ,
     cve_destino char(1)
    )


    CREATE TEMP TABLE sol_issste_par_temp
    (
     nss_imss char(11)                ,
     nss_issste char(11)              ,
     curp char(18)                    ,
     consecutivo decimal(11,0)        ,
     folio integer                    ,
     folio_solicitud integer          ,
     tipo_id char(1)                  ,
     paterno char(40)                 ,
     materno char(40)                 ,
     nombres char(40)                 ,
     rfc char(13)                     ,
     tipo_retiro smallint             ,
     tipo_beneficio smallint          ,
     fecha_baja   date                ,
     folio_dictamen char(10)          ,
     fecha_solicitud date             ,
     fecha_entrega_rec date           ,
     tipo_inversion smallint          ,
     fecha_valoriza date              ,
     acciones_ret_sief1 decimal(14,6) ,
     acciones_ret_sief2 decimal(14,6) ,
     importe_retiro decimal(14,6)     ,
     --
     id_procesar char(8)              ,
     nom_completo_icefa char(120)     ,
     nom_beneficiario char(120)       ,
     rfc_pension char(13)             ,
     paterno_pension char(40)         ,
     materno_pension char(40)         ,
     nombres_pension char(40)         ,
     --
     diag_procesar char(3)            ,
     diag_issste char(3)              ,
     estado_sub_ret char(1)           ,
     fecha_entrega date               ,
     estado_solicitud smallint        ,
     rechazo_cod smallint             ,
     fecha_captura date               ,
     fecha_confirma date              ,
     fecha_modifica date              ,
     fecha_envio date                 ,
     usuario_captura char(12)         ,
     usuario_confirma char(12)        ,
     usuario_modifica char(12)        ,
     carta smallint                   ,
     grupo smallint                   ,
     cve_destino char(1)
    )

END FUNCTION


FUNCTION carga_sum_lote()
#csl------ ---------------
    LET reg_cza_lote.tot_registros = carga_reg[023,028]

    UPDATE ret_cza_lote
    SET    tot_registros   = reg_cza_lote.tot_registros
    WHERE  folio           = ultimo_folio

END FUNCTION


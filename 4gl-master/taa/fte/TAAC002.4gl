#############################################################################
#Proyecto          => AFORE ( MEXICO )                                      #
#Propietario       => E.F.P.                                                #
#Programa TAAC002  => RECIBE RESULTADO VALIDACION IMAGENES, AFORE RECEPTORA #
#Por               => MAURO MUNIZ CABALLERO                                 #
#Fecha creacion    => 31 DE ENERO DE 2001                                   #
#Sistema           => TAA                                                   #
#Modificado        => EDUARDO JOAQUIN RESENDIZ MEDINA (TOTAL DE REGISTROS)  #
#Fecha Modificacion=> 06 DE JULIO DE 2005                                   #
#Modificado        => MAURO MUÑIZ CABALLERO                                 #
#Fecha Modificacion=> 9 DE MAYO DE 2007                                     #
#              Verificación resultado de llamadas telefonicas               #
#Modificado        => JOSUÉ LISANDRO HUERTA SIERRA                          #
#Fecha Modificacion=> 17 DE AGOSTO DE 2007                                  #
#              Actualización de acuerdo a circular 69-1                     #
#Req:648           => JCPV 13/09/2011.                                      #
#Req:744           => JCPV 30/01/2012 Add ts = 15                           #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_taa_cza_devol RECORD LIKE taa_cza_devol.*
    DEFINE reg_taa_det_devol RECORD LIKE taa_det_devol.*
    DEFINE reg_taa_sum_devol RECORD LIKE taa_sum_devol.*
    DEFINE g_seg_modulo      RECORD LIKE seg_modulo.*
    DEFINE i                 RECORD LIKE tab_afore_local.*
    DEFINE reg_carta         RECORD LIKE int_ctr_carta.*

    DEFINE enter      CHAR(1)
    DEFINE g_usuario  CHAR(8)
    DEFINE generar    CHAR(20)
    DEFINE cons_carta CHAR(120)
    DEFINE archivo    CHAR(150)

    DEFINE HOY        DATE
    DEFINE fecha_ver  DATE
    DEFINE fecha_pre  DATE

    DEFINE st_int     SMALLINT
    DEFINE cont_reg   SMALLINT
    DEFINE cuantos    SMALLINT
    DEFINE tipo_solic SMALLINT
    DEFINE st_cap     SMALLINT
    DEFINE tot_acep   SMALLINT
    DEFINE tot_rech   SMALLINT
    DEFINE tot_pend   SMALLINT

    DEFINE g RECORD
        nss            CHAR(11),
        afore          CHAR(3),
        cod_operac     CHAR(2),
        motivo_rechazo CHAR(3),                  #648
        diag_proc      CHAR(3),
        descripcion    CHAR(60),
        cod_edo        SMALLINT,
        #causa          CHAR(2),
        grado_riesgo   CHAR(2),
        vector_riesgo  CHAR(8),
        agte           CHAR(10),
        estado         CHAR(12)
    END RECORD

    DEFINE ban_rech    SMALLINT

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
        PROMPT LINE LAST  ,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG('TAAC002.log')
    CALL inicio()            #i
    CALL proceso_principal() #pp
    CALL impresion_reporte() #ir
    CALL impresion_reporte1() #ir1

    PROMPT  "PROCESO FINALIZADO, PRESIONE ENTER PARA SALIR " FOR enter

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY      = TODAY
    LET cont_reg = 1
    LET tot_acep = 0
    LET tot_rech = 0
    LET tot_pend = 0
    LET ban_rech = 0

    SELECT *, user
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE plano_devueltas
    WHENEVER ERROR STOP

    CREATE TABLE plano_devueltas
        (n_registros          CHAR(730))

    DATABASE safre_af

    CREATE TEMP TABLE tmp_reg_pendientes (nss                CHAR(11),
                                          fecha_presentacion DATE,
                                          cod_result_operac  CHAR(02),
                                          f_recep_sol        DATE);

    INITIALIZE reg_carta.* TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW TAAC0021 AT 4,4 WITH FORM "TAAC0021" ATTRIBUTE(BORDER)
    DISPLAY " TAAC002  RECIBE ARCHIVO RESPUESTA DE VALIDACION IMAGENES                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < CTRL-C > Salir " AT 1,26
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

    INPUT BY NAME generar WITHOUT DEFAULTS
        AFTER FIELD generar
            IF generar IS NULL THEN
                ERROR "NOMBRE DE ARCHIVO NO PUEDE SER NULO"
                NEXT FIELD generar
            END IF

            SELECT "X"
            FROM   taa_ctr_arh a
            WHERE  a.nombre_archivo = generar

            IF SQLCA.SQLCODE = 0 THEN
                PROMPT "ARCHIVO YA PROCESADO,[Enter] p/salir" FOR enter
                EXIT PROGRAM
            END IF

        WHENEVER ERROR CONTINUE
            LET archivo = g_seg_modulo.ruta_rescate CLIPPED,"/",
                          generar CLIPPED

            LOAD FROM archivo INSERT INTO safre_tmp:plano_devueltas

            SELECT count(*)
            INTO   cuantos
            FROM   safre_tmp:plano_devueltas

            IF cuantos = 0 THEN
                DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                AT 19,2 ATTRIBUTE(REVERSE)
                SLEEP 3
                NEXT FIELD generar
            ELSE
                EXIT INPUT
            END IF
        WHENEVER ERROR STOP

        EXIT INPUT

        ON KEY (INTERRUPT)
            ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE)
            SLEEP 2
        EXIT PROGRAM
    END INPUT

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL validacion_previa() #vp
    CALL lee_archivo_plano() #lap

    DISPLAY "REGISTROS ACEPTADOS  : ", tot_acep AT 13,9
    DISPLAY "REGISTROS RECHAZADOS : ", tot_rech AT 14,9

    PROMPT  "PROCESO FINALIZADO, PRESIONE ENTER PARA SALIR " FOR enter

    CLOSE WINDOW TAAC0021

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------

    DEFINE
        cont            SMALLINT,
        cont_fct        SMALLINT,
        vfolio          DECIMAL(10,0),
        srowid          INTEGER

    DEFINE
        vfentcons    DATE,
        vfecha_envio DATE

    DEFINE
        ident_operacion CHAR(2),
        cfecha_8        CHAR(8),
        cfecha_10       CHAR(10),
        carga_reg       CHAR(730),
        cf_recep_sol    CHAR(10),
        f_recep_sol     DATE


    LET cont                          = 0
    LET ident_operacion               = ""
    LET reg_taa_det_devol.f_actualiza = HOY

    DECLARE cur_1 CURSOR FOR
    SELECT  *
    FROM    safre_tmp:plano_devueltas

    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1

                   #---ENCABEZADO SOLICITUD TRASPASO INDIVIDUAL---#

        IF carga_reg[1,2] = "01" AND carga_reg[5,6] = "06" THEN
            LET ident_operacion                     = "01"
            LET reg_taa_cza_devol.tipo_registro     = carga_reg[001,002]
            LET reg_taa_cza_devol.ident_servicio    = carga_reg[003,004]
            LET reg_taa_cza_devol.ident_operacion   = carga_reg[005,006]
            LET cfecha_8                            = carga_reg[020,027]

            LET cfecha_10 = cfecha_8[5,6],"/",cfecha_8[7,8],"/",cfecha_8[1,4]
            LET reg_taa_cza_devol.fecha_presentacion = cfecha_10

            SELECT "X"
            FROM   taa_cza_devol
            WHERE  fecha_presentacion = reg_taa_cza_devol.fecha_presentacion

            IF STATUS = NOTFOUND THEN
                INSERT INTO taa_cza_devol VALUES(reg_taa_cza_devol.*)
            END IF
        END IF

        IF carga_reg[1,2] = "01" AND carga_reg[5,6] <> "06" THEN
            LET ident_operacion = "02"
            PROMPT "EL ARCHIVO NO ES DE DEVOLUCION DE SOLICITUDES, ",
                   "Presione [Enter] para salir" FOR enter
            EXIT PROGRAM
        END IF

                   #---DETALLE SOLICITUD TRASPASO INDIVIDUAL---#

        IF carga_reg[1,2] = "02" AND ident_operacion = "01" THEN
            LET reg_taa_det_devol.tipo_registro      = carga_reg[001,002]
            LET reg_taa_det_devol.cont_servicio      = carga_reg[003,012]
            LET reg_taa_det_devol.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_taa_det_devol.tipo_ced_cuenta    = carga_reg[018,019]
            LET reg_taa_det_devol.cve_ced_cuenta     = carga_reg[020,022]
            LET reg_taa_det_devol.tipo_traspaso      = carga_reg[023,024]
            LET cfecha_8                             = carga_reg[025,032]

            LET reg_taa_det_devol.curp               = carga_reg[033,050]
            LET reg_taa_det_devol.n_seguro           = carga_reg[051,061]
            LET reg_taa_det_devol.rfc                = carga_reg[062,074]
            LET reg_taa_det_devol.paterno            = carga_reg[075,114]
            LET reg_taa_det_devol.materno            = carga_reg[115,154]
            LET reg_taa_det_devol.nombres            = carga_reg[155,194]
            #LET reg_taa_det_devol.cve_sector         = carga_reg[221,221]
            LET reg_taa_det_devol.f_recep_sol        = carga_reg[195,202]
            LET reg_taa_det_devol.ident_lote_solic   = carga_reg[203,218]
            #LET reg_taa_det_devol.nss_cedente        = carga_reg[271,281]
            #LET reg_taa_det_devol.rfc_cedente        = carga_reg[282,294]
            #LET reg_taa_det_devol.paterno_ced        = carga_reg[325,364]
            #LET reg_taa_det_devol.materno_ced        = carga_reg[365,404]
            #LET reg_taa_det_devol.nombres_ced        = carga_reg[405,444]
            #LET reg_taa_det_devol.id_reg_reenv       = carga_reg[581,581]
            LET reg_taa_det_devol.cod_result_operac  = carga_reg[219,220]
            #LET reg_taa_det_devol.diag_proceso       = carga_reg[585,587]
            #LET reg_taa_det_devol.no_llamadas        = carga_reg[588,589]
            #LET reg_taa_det_devol.causa_origen       = carga_reg[590,591]
            #LET reg_taa_det_devol.grado_riesgo       = carga_reg[590,591]
            LET reg_taa_det_devol.motivo_rechazo     = carga_reg[221,223]
            LET reg_taa_det_devol.cod_promotor       = carga_reg[224,233]
            #LET reg_taa_det_devol.diag_imagen        = carga_reg[705,714]
            #LET reg_taa_det_devol.diag_folio         = carga_reg[715,717]
            #LET reg_taa_det_devol.vector_riesgo      = carga_reg[718,725]

            LET cfecha_10 = cfecha_8[5,6],"/",cfecha_8[7,8],"/",cfecha_8[1,4]
            LET reg_taa_det_devol.fecha_presentacion = cfecha_10

            LET cf_recep_sol = reg_taa_det_devol.f_recep_sol[5,6], "/",
                               reg_taa_det_devol.f_recep_sol[7,8], "/",
                               reg_taa_det_devol.f_recep_sol[1,4]
            LET f_recep_sol  = cf_recep_sol

            ---- Tipo traspaso 01 - Individual
            --IF reg_taa_det_devol.tipo_traspaso = '01' THEN
            --   LET tipo_solic = 2
            --ELSE
            --   ---- Tipo traspaso 38 - Traspaso AA Internet
	          --   ---- Tipo traspaso 55 - Traspaso AA por Verificacion electronica de Identidad
            --   LET tipo_solic = 9
            --END IF

            #Obtener NTI y tipo solicitud
            IF reg_taa_det_devol.n_seguro IS NULL OR
            	 reg_taa_det_devol.n_seguro[1] = ' ' THEN

            	 SELECT COUNT(*)
            	 INTO   cont_fct
            	 FROM   afi_solicitud
            	 WHERE  n_unico        = reg_taa_det_devol.curp
            	 AND    fentcons       = f_recep_sol
            	 AND    status_interno IN(50,65,70)

            	 IF cont_fct = 1 THEN
            	 	  SELECT n_seguro      ,
            	 	         tipo_solicitud
            	 	  INTO   reg_taa_det_devol.n_seguro,
            	 	         tipo_solic
            	 	  FROM   afi_solicitud
            	    WHERE  n_unico        = reg_taa_det_devol.curp
            	    AND    fentcons       = f_recep_sol
            	    AND    status_interno IN(50,65,70)
            	 ELSE
            	 	  SELECT MAX(ROWID)
                  INTO   srowid
                  FROM   afi_solicitud
                  WHERE  n_unico        = reg_taa_det_devol.curp
            	    AND    fentcons       = f_recep_sol
            	    AND    status_interno IN(50,65,70)

            	    SELECT n_seguro      ,
            	 	         tipo_solicitud
            	 	  INTO   reg_taa_det_devol.n_seguro,
            	 	         tipo_solic
            	 	  FROM   afi_solicitud
            	    WHERE  ROWID = srowid
            	 END IF
            ELSE
            	 SELECT COUNT(*)
            	 INTO   cont_fct
            	 FROM   afi_solicitud
            	 WHERE  n_seguro       = reg_taa_det_devol.n_seguro
            	 AND    fentcons       = f_recep_sol
            	 AND    status_interno IN(50,65,70)

            	 IF cont_fct = 1 THEN
            	 	  SELECT tipo_solicitud
            	 	  INTO   tipo_solic
            	 	  FROM   afi_solicitud
            	    WHERE  n_seguro       = reg_taa_det_devol.n_seguro
            	    AND    fentcons       = f_recep_sol
            	    AND    status_interno IN(50,65,70)
            	 ELSE
            	 	  SELECT MAX(ROWID)
                  INTO   srowid
                  FROM   afi_solicitud
                  WHERE  n_seguro       = reg_taa_det_devol.n_seguro
            	    AND    fentcons       = f_recep_sol
            	    AND    status_interno IN(50,65,70)

            	    SELECT tipo_solicitud
            	 	  INTO   tipo_solic
            	 	  FROM   afi_solicitud
            	    WHERE  ROWID = srowid
            	 END IF
            END IF

            SELECT "X"
            FROM   taa_det_devol
            WHERE  n_seguro           = reg_taa_det_devol.n_seguro
            AND    fecha_presentacion = reg_taa_det_devol.fecha_presentacion
            IF STATUS = NOTFOUND THEN

               SELECT COUNT(*)
                 INTO cont_fct
                 FROM afi_solicitud
                WHERE @n_seguro = reg_taa_det_devol.n_seguro
                  AND @fentcons = f_recep_sol
               IF cont_fct = 1 THEN
                  SELECT @n_folio, @fentcons, @fecha_envio
                    INTO vfolio, vfentcons, vfecha_envio
                    FROM afi_solicitud
                   WHERE @n_seguro = reg_taa_det_devol.n_seguro
                     AND @fentcons = f_recep_sol
               ELSE
                  SELECT MAX(@fentcons)
                    INTO vfentcons
                    FROM afi_solicitud
                   WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                     AND @status_interno IN(50,65,70)
                     AND @tipo_solicitud = tipo_solic
                  IF STATUS = NOTFOUND THEN
                     LET tot_pend = tot_pend + 1

                     INSERT INTO tmp_reg_pendientes
                     VALUES(reg_taa_det_devol.n_seguro,
                            reg_taa_det.devol.fecha_presentacion,
                            reg_taa_det_devol.cod_result_operac,
                            f_recep_sol)
                  END IF

                  SELECT COUNT(*)
                    INTO cont_fct
                    FROM afi_solicitud
                   WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                     AND @fentcons       = vfentcons
                     AND @status_interno IN(50,65,70)
                     AND @tipo_solicitud = tipo_solic
                  IF cont_fct > 1 THEN
                     SELECT MAX(rowid)
                       INTO srowid
                       FROM afi_solicitud
                      WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                        AND @fentcons       = vfentcons
                        AND @status_interno IN(50,65,70)
                        AND @tipo_solicitud = tipo_solic

                     SELECT @n_folio, @fecha_envio
                       INTO vfolio, vfecha_envio
                       FROM afi_solicitud
                      WHERE @n_seguro = reg_taa_det_devol.n_seguro
                        AND @rowid    = srowid
                  ELSE
                     SELECT @n_folio, @fecha_envio
                       INTO vfolio, vfecha_envio
                       FROM afi_solicitud
                      WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                        AND @fentcons       = vfentcons
                        AND @status_interno IN(50,65,70)
                        AND @tipo_solicitud = tipo_solic
                  END IF
               END IF

               LET reg_taa_det_devol.n_folio = vfolio

               IF reg_taa_det_devol.cod_result_operac = '01' THEN
                  LET st_int                   = 70
                  LET reg_taa_det_devol.estado = 0

                  UPDATE afi_solicitud
                     SET status_interno  = st_int
                   WHERE @n_seguro       = reg_taa_det_devol.n_seguro
                     #AND @status_interno IN(50,65,70)
                     AND @fentcons       = vfentcons
                     AND @n_folio        = vfolio
                     AND @tipo_solicitud = tipo_solic

                  LET tot_acep = tot_acep + 1
               ELSE
                  LET st_int = 40

                  IF reg_taa_det_devol.diag_proceso[1] <> " " THEN
                     SELECT @tipo
                       INTO reg_taa_det_devol.estado
                       FROM tab_dev_taa
                      WHERE @cod_rech  = reg_taa_det_devol.diag_proceso
                        AND @tipo_diag = 1
                     LET st_cap   = reg_taa_det_devol.diag_proceso + 1000
                     LET ban_rech = 1
                  ELSE
                     LET ban_rech = 0
                  END IF

                  IF (reg_taa_det_devol.motivo_rechazo[1] <> " " AND
                      ban_rech = 0)                              THEN

                     LET reg_taa_det_devol.motivo_rechazo =
                         reg_taa_det_devol.motivo_rechazo[1,3]     #648
                     SELECT @tipo
                       INTO reg_taa_det_devol.estado
                       FROM tab_dev_taa
                      WHERE @cod_rech  = reg_taa_det_devol.motivo_rechazo
                        AND @tipo_diag = 2

                     LET st_cap = reg_taa_det_devol.motivo_rechazo + 2000
                     LET ban_rech = 1
                  END IF

                  IF (reg_taa_det_devol.diag_imagen[1] <> " " AND
                      ban_rech = 0)                           THEN

                     LET reg_taa_det_devol.diag_imagen =
                         reg_taa_det_devol.diag_imagen[1,2]
                     SELECT @tipo
                       INTO reg_taa_det_devol.estado
                       FROM tab_dev_taa
                      WHERE @cod_rech  = reg_taa_det_devol.diag_imagen
                        AND @tipo_diag = 2

                     LET st_cap = reg_taa_det_devol.diag_imagen + 2000
                     LET ban_rech = 1
                  END IF

                  IF reg_taa_det_devol.estado = 1 THEN
                     LET reg_carta.docto_cod = 30227
                     LET reg_carta.opera_cod = 'D'
                     LET reg_carta.edo_genera = 10
                  ELSE
                     IF reg_taa_det_devol.estado = 2 THEN
                        LET st_int = 45
                     ELSE
                        LET st_int = 40
                     END IF

                     LET reg_carta.docto_cod  = 30219
                     LET reg_carta.opera_cod  = ' '
                     LET reg_carta.edo_genera = 10
                  END IF

                  UPDATE afi_solicitud
                  SET    status_interno = st_int,
                         status         = 2,
                         status_captura = st_cap
                  WHERE  @n_seguro = reg_taa_det_devol.n_seguro
                  #AND    @status_interno in(50,65,70)
                  AND    @fentcons       = vfentcons
                  AND    @n_folio        = vfolio
                  AND    @tipo_solicitud = tipo_solic

                  LET reg_carta.fecha_registro = reg_taa_det_devol.fecha_presentacion
                  #LET tipo_solic = 2
                  LET ban_rech   = 0

                  IF tipo_solic = 2 
                  OR tipo_solic = 15 THEN                     
                     CALL det_carta(tipo_solic, vfolio) #dc
                  END IF

                  LET tot_rech = tot_rech + 1
               END IF

               IF tipo_solic = 2 
               OR tipo_solic = 15 THEN
                  CALL actualiza_afi_ctr(vfolio, vfecha_envio,
                                         reg_taa_det_devol.n_seguro,st_int,
                                         reg_taa_det_devol.cod_result_operac,
                                         reg_taa_det_devol.fecha_presentacion)
               END IF

               INSERT INTO taa_det_devol VALUES(reg_taa_det_devol.*)

               LET st_cap = ""
            END IF
        END IF

                   #---SUMARIO SOLICITUD TRASPASO INDIVIDUAL---#

        IF carga_reg[1,2] = "09" AND ident_operacion = "01" THEN
            LET ident_operacion = ""
            LET reg_taa_sum_devol.tipo_registro      = carga_reg[001,002]
            LET reg_taa_sum_devol.cantidad_reg_det   = carga_reg[003,011]
            LET reg_taa_sum_devol.fecha_presentacion =
                reg_taa_cza_devol.fecha_presentacion

            SELECT "X"
            FROM   taa_sum_devol
            WHERE  fecha_presentacion = reg_taa_sum_devol.fecha_presentacion

            IF STATUS = NOTFOUND THEN
                INSERT INTO taa_sum_devol VALUES(reg_taa_sum_devol.*)
            END IF
        END IF

        LET cfecha_8  = null
        LET cfecha_10 = null

    END FOREACH

    LET cont_reg = tot_acep + tot_rech

    INSERT INTO taa_ctr_arh
    VALUES (generar, cont_reg, tot_acep, tot_rech, tot_pend, HOY, g_usuario)

END FUNCTION

FUNCTION validacion_previa()
#vp-------------------------

    DEFINE c2_tipo_registro CHAR(2)

    DEFINE
        sw_1  ,
        sw_2  ,
        sw_9  SMALLINT

    DECLARE cur_2 CURSOR FOR
    SELECT UNIQUE(n_registros[1,2])
    FROM   safre_tmp:plano_devueltas

    LET sw_1 = 0
    LET sw_2 = 0
    LET sw_9 = 0

    FOREACH cur_2 INTO c2_tipo_registro
        CASE c2_tipo_registro
            WHEN "01"
                LET sw_1 = 1
            WHEN "02"
                LET sw_2 = 1
            WHEN "09"
                LET sw_9 = 1
        END CASE
    END FOREACH

    IF sw_1 = 0 THEN
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
        EXIT PROGRAM
    END IF

    IF sw_2 = 0 THEN
       PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" FOR enter
       EXIT PROGRAM
    END IF

    IF sw_9 = 0 THEN
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
        EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION impresion_reporte()
#ir-------------------------

    DEFINE w_codigo_afore LIKE safre_af:tab_afore_local.codigo_afore

    DEFINE hora       CHAR(8)
    DEFINE G_IMPRE    CHAR(300)
    DEFINE gimpresion CHAR(300)

    SELECT  codigo_afore
    INTO    w_codigo_afore
    FROM    tab_afore_local

    SELECT MAX(d.f_actualiza)
    INTO   fecha_ver
    FROM   taa_det_devol d

    SELECT s.fecha_presentacion
    INTO   fecha_pre
    FROM   taa_det_devol s
    WHERE  s.f_actualiza = fecha_ver
    GROUP BY 1

    LET hora = TIME
    LET hora = hora[1,2],hora[4,5],hora[7,8]

    LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".DEV_SOL.",hoy USING "DDMMYY", "_",hora CLIPPED

    START REPORT det_dev_sol TO  G_IMPRE

    OUTPUT TO REPORT det_dev_sol(g.*)

    FINISH REPORT det_dev_sol

    --LET gimpresion = "lp ",G_IMPRE
    --LET gimpresion = "vi ",G_IMPRE
    --RUN gimpresion

END FUNCTION

REPORT det_dev_sol(g)
#dvsi----------------

    DEFINE g RECORD
        nss            CHAR(11),
        afore          CHAR(3),
        cod_operac     CHAR(2),
        motivo_rechazo CHAR(3),
        diag_proc      CHAR(3),
        descripcion    CHAR(50),
        cod_edo        SMALLINT,
        #causa          CHAR(2),
        grado_riesgo   CHAR(2),
        vector_riesgo  CHAR(8),
        agte           CHAR(10),
        estado         CHAR(12)

    END RECORD
    DEFINE des_causa CHAR(50)

    DEFINE l_cif       RECORD
           vapuntador  SMALLINT
    END RECORD

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 60
    FORMAT
        PAGE HEADER
        SELECT razon_social, codigo_afore
        INTO i.razon_social, i.codigo_afore
        FROM safre_af:tab_afore_local

        PRINT i.codigo_afore,
              COLUMN 10, i.razon_social,
              COLUMN 64, TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT COLUMN 08,"TOTAL DE SOLICITUDES VALIDACION IMAGENES / CONFIRMACION LLAMADA TELEFONICA"
        PRINT COLUMN 08,"                   TRASPASOS AFORE - AFORE  (RECEPTORA)"
        SKIP 2 LINE
        PRINT COLUMN 08,"ARCHIVO : ",generar
        PRINT COLUMN 04,
    "-------------------------------------------------------------------------------------------------------------------------------- "
        PRINT COLUMN 04,"NSS",
              COLUMN 16,"AFORE",
              COLUMN 23,"ESTADO",
              COLUMN 33,"PROMOTOR",
              COLUMN 45,"CAUSA LLAMADA",
              COLUMN 109,"MOTIVO"
        PRINT COLUMN 04,
    "---------------------------------------------------------------------------------------------------------------------------------"

    ON EVERY ROW

    DECLARE cursor CURSOR FOR
        SELECT dv.n_seguro,
               dv.cve_ced_cuenta,
               dv.cod_result_operac,
               dv.motivo_rechazo,
               dv.diag_proceso,
               #dv.causa_origen,
               dv.grado_riesgo,
               dv.vector_riesgo,
               dv.cod_promotor,
               dv.estado
        FROM   taa_det_devol dv
        WHERE  dv.f_actualiza = fecha_ver
        ORDER BY 2,1

    INITIALIZE g.* TO NULL
    LET l_cif.vapuntador = 0
    FOREACH cursor INTO g.nss, g.afore, g.cod_operac, g.motivo_rechazo, g.diag_proc,
                        g.grado_riesgo, g.vector_riesgo, g.agte, g.cod_edo

    INITIALIZE g.descripcion TO NULL

        IF g.cod_operac = '02' THEN
            SELECT tr.desc_rech
              INTO g.descripcion
              FROM tab_dev_taa tr
             WHERE tr.cod_rech = g.motivo_rechazo

            LET g.estado      = "RECHAZADA"
        ELSE
            LET g.descripcion = "PROCEDENTE"
            LET g.estado      = "ACEPTADA"
        END IF

        LET l_cif.vapuntador = l_cif.vapuntador + 1

        {SELECT descripcion
        INTO   des_causa
        FROM   tab_causa_llamada
        WHERE  causa = g.causa}


        PRINT COLUMN 03,g.nss,
              COLUMN 17,g.afore,
              COLUMN 22,g.estado CLIPPED,
              COLUMN 33,g.agte CLIPPED,
              #COLUMN 45,g.causa,
              #COLUMN 48,des_causa,
              COLUMN 45,g.grado_riesgo,
              COLUMN 48,g.vector_riesgo,
              COLUMN 109,g.descripcion CLIPPED
        LET des_causa = ""
    END FOREACH

    ON LAST ROW
        SKIP 2 LINES
        PRINT COLUMN 04,
    "---------------------------------------------------------------------------------------------------------------------------------"
        PRINT
        PRINT COLUMN 7, "Total de registros aceptados:   ",tot_acep
        PRINT COLUMN 7, "Total de registros rechazados:  ",tot_rech
        PRINT COLUMN 7, "Total de registros encontrados: ",l_cif.vapuntador

    PAGE TRAILER
        SKIP 2 LINE
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
        PAUSE "Presione enter para continuar...."

END REPORT

FUNCTION det_carta(tipo_sol, vfolio)
#dc---------------------------------

   DEFINE fent     DATE
   DEFINE tipo_sol SMALLINT
   DEFINE vfolio   DECIMAL(10,0)

   LET reg_carta.tipo_solicitud = tipo_sol
   LET reg_carta.n_folio        = vfolio

   LET reg_carta.nss            = reg_taa_det_devol.n_seguro
   LET reg_carta.fecha_genera   = TODAY
   LET reg_carta.hora_genera    = TIME
   LET reg_carta.lote_genera    = 0
   LET reg_carta.consecutivo    = 0
   LET reg_carta.id_sepomex     = 0

   SELECT 'X'
     FROM int_ctr_carta
    WHERE @nss            = reg_taa_det_devol.n_seguro
      AND @n_folio        = reg_carta.n_folio
      AND @tipo_solicitud = reg_carta.tipo_solicitud
      AND @fecha_genera   = today
    GROUP BY 1

   IF STATUS = NOTFOUND THEN
     LET cons_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,?,?,?,",
                          "?,?,?)"
     PREPARE sql_exe FROM cons_carta
     EXECUTE sql_exe USING reg_carta.*

     INITIALIZE reg_carta.* TO NULL
   END IF

END FUNCTION

FUNCTION actualiza_afi_ctr(vfolio, vfecha_envio, vnss, st_int, cod_op, vfact)
#aac-------------------------------------------------------------------------

    DEFINE
        vfolio       DECIMAL(10,0),
        vfecha_envio DATE,
        vnss         CHAR(11),
        st_int       SMALLINT,
        cod_op       SMALLINT,
        maxregid     INTEGER,
        vfact        DATE

    DEFINE reg_ctr RECORD LIKE afi_ctr_solicitud.*

    SELECT MAX(@rowid)
      INTO maxregid
      FROM afi_ctr_solicitud
     WHERE @n_seguro         = vnss
       AND @n_folio          = vfolio
       AND @tipo_solicitud  = 2
       AND @fecha_envio      = vfecha_envio

    SELECT *
      INTO reg_ctr.*
      FROM afi_ctr_solicitud
     WHERE @n_seguro       = vnss
       AND @n_folio        = vfolio
       AND @tipo_solicitud = 2
       AND @fecha_envio    = vfecha_envio
       AND @rowid          = maxregid

    LET reg_ctr.fecha_recepcion = TODAY
    LET reg_ctr.cod_operacion   = cod_op
    LET reg_ctr.status_interno  = st_int
    LET reg_ctr.n_folio         = vfolio
    LET reg_ctr.fecha_envio     = vfecha_envio
    LET reg_ctr.tipo_solicitud  = 2
    LET reg_ctr.factualiza      = vfact

    INSERT INTO afi_ctr_solicitud VALUES(reg_ctr.*)
    
    SELECT MAX(@rowid)
      INTO maxregid
      FROM afi_ctr_solicitud
     WHERE @n_seguro         = vnss
       AND @n_folio          = vfolio
       AND @tipo_solicitud  = 15
       AND @fecha_envio      = vfecha_envio

    SELECT *
      INTO reg_ctr.*
      FROM afi_ctr_solicitud
     WHERE @n_seguro       = vnss
       AND @n_folio        = vfolio
       AND @tipo_solicitud = 15
       AND @fecha_envio    = vfecha_envio
       AND @rowid          = maxregid

    LET reg_ctr.fecha_recepcion = TODAY
    LET reg_ctr.cod_operacion   = cod_op
    LET reg_ctr.status_interno  = st_int
    LET reg_ctr.n_folio         = vfolio
    LET reg_ctr.fecha_envio     = vfecha_envio
    LET reg_ctr.tipo_solicitud  = 15
    LET reg_ctr.factualiza      = vfact
    
    INSERT INTO afi_ctr_solicitud VALUES(reg_ctr.*)


END FUNCTION

REPORT det_dev_sol1(g1)
#dvsi----------------

    DEFINE g1            RECORD
      n_seguro           CHAR(11),
      fecha_presentacion DATE,
      cod_result_operac  CHAR(02),
      f_recep_sol        DATE
    END RECORD

    DEFINE l_cif1        RECORD
           vapuntador    SMALLINT
    END RECORD

    OUTPUT
        TOP MARGIN    1
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH   60
    FORMAT
        PAGE HEADER
        SELECT razon_social, codigo_afore
        INTO i.razon_social, i.codigo_afore
        FROM safre_af:tab_afore_local

        PRINT i.codigo_afore,
              COLUMN 10, i.razon_social,
              COLUMN 64, TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE

        PRINT COLUMN 08,"SOL NO ACTUALIZADAS  VALIDACION IMAGENES / CONFIRMACION LLAMADA TELEFONICA"
        PRINT COLUMN 08,"                   TRASPASOS AFORE - AFORE  (RECEPTORA)"
        SKIP 2 LINE

        PRINT COLUMN 08,"ARCHIVO : ",generar
        PRINT COLUMN 04,
    "-------------------------------------------------------------------------------------------------------------------------------- "
        PRINT COLUMN 04,"NSS",
              COLUMN 16,"FECHA PRESENTACION",
              COLUMN 36,"COD RESUL OPERAC",
              COLUMN 52,"FECHA RECEP SOL"
        PRINT COLUMN 04,
    "---------------------------------------------------------------------------------------------------------------------------------"


    ON EVERY ROW

    DECLARE cursor1 CURSOR FOR
        SELECT dv.*
        FROM   tmp_reg_pendientes dv
        ORDER BY 1,3,4

    INITIALIZE g1.* TO NULL

    LET l_cif1.vapuntador = 0

    FOREACH cursor1 INTO g1.*

        LET l_cif1.vapuntador = l_cif1.vapuntador + 1

        PRINT COLUMN 03, g1.n_seguro,
              COLUMN 16, g1.fecha_presentacion,
              COLUMN 36, g1.cod_result_operac   USING "##",
              COLUMN 52, g1.f_recep_sol         USING "DD/MM/YYYY"

    END FOREACH

    ON LAST ROW
        SKIP 2 LINES
        PRINT COLUMN 04,
    "---------------------------------------------------------------------------------------------------------------------------------"
        PRINT
        PRINT COLUMN 7, "Total de registros encontrados: ", l_cif1.vapuntador

    PAGE TRAILER
        SKIP 2 LINE
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
        PAUSE "Presione enter para continuar...."

END REPORT

FUNCTION impresion_reporte1()
#ir-------------------------

    DEFINE g1            RECORD
      n_seguro           CHAR(11),
      fecha_presentacion DATE,
      cod_result_operac  CHAR(02),
      f_recep_sol        DATE
    END RECORD

    DEFINE hora1         CHAR(8)
    DEFINE G_IMPRE1      CHAR(300)
    DEFINE gimpresion1   CHAR(300)

    LET hora1 = TIME
    LET hora1 = hora1[1,2], hora1[4,5], hora1[7,8]

    LET G_IMPRE1 = g_seg_modulo.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
                   ".DEV_SOL_PENDIE.", hoy USING "DDMMYY", "_", hora1 CLIPPED

    START REPORT det_dev_sol1 TO G_IMPRE1

       OUTPUT TO REPORT det_dev_sol1(g1.*)

    FINISH REPORT det_dev_sol1

END FUNCTION

--ALTER TABLE taa_det_devol MODIFY motivo_rechazo CHAR(03)

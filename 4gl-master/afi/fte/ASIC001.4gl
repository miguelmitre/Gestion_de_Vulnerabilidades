###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa ASIC001  => CARGA DE ARCHIVO DE REGISTROS ASIGNADOS             #
#Sistema           => AFI.                                                #
#Autor             => MAURO MUNIZ CABALLERO                               #
#Fecha             => 22 DE JUNIO DE 2001                                 #
#Actualizacion     => EDUARDO RESENDIZ MEDINA  15 MARZO 2006 listado_3    #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 10 DE MARZO DE 2008 (MULTISIEFORES)                 #
###########################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_reg  RECORD
        generar   CHAR(20)
    END RECORD
    DEFINE HOY         DATE
    DEFINE xx_fecha    DATE
    DEFINE diaSig      DATE
    DEFINE enter       CHAR(1)
    DEFINE aux_pausa   CHAR(1)
    DEFINE ejecuta     CHAR(200)
    DEFINE corr        CHAR(100)
    DEFINE carga       CHAR(50)
    DEFINE total_reg   SMALLINT
    DEFINE g_plano1    SMALLINT
    DEFINE aceptar     INTEGER
    DEFINE rechazar    INTEGER
    DEFINE pendiente   INTEGER
    DEFINE aclaracion  INTEGER
    DEFINE max_folio   INTEGER
    DEFINE g_param RECORD LIKE seg_modulo.*

    DEFINE reg_det		RECORD
        tipo_registro        CHAR(02)     ,
        contador_servicio    CHAR(10)     ,
        clave_operacion      CHAR(02)     ,
        nss_solicitud        CHAR(11)     ,
        curp_solicitud       CHAR(18)     ,
        rfc_trabajador       CHAR(13)     ,
        paterno              CHAR(40)     ,
        materno              CHAR(40)     ,
        nombres              CHAR(40)     ,
        fecha_nacimiento     CHAR(08)     ,
        clave_promotor       CHAR(10)     ,
        fec_recp_sol_afore   CHAR(08)     ,
        folio_solicitud      DECIMAL(16,0),
        sexo                 SMALLINT     ,
        entidad_nacimiento   CHAR(02)     ,
        ind_infonavit        CHAR(01)     ,
        nacionalidad         CHAR(03)     ,
        tip_aprob            CHAR(01)     ,
        fol_aprob            CHAR(10)     ,
        doc_aprob            CHAR(16)     ,
        cod_err_ori          CHAR(04)     ,
        cve_afo_ced          CHAR(03)     ,
        folio_edo_cta        CHAR(08)     ,
        cod_operacion        CHAR(02)     ,
        diag_proceso         CHAR(15)     ,
        ident_lote_origen    CHAR(16)     ,
        nss_oficial          CHAR(11)     ,
        ind_nss_modificado   CHAR(01)     ,
        fec_emision_certif   CHAR(08)     ,
        curp_oficial         CHAR(18)     ,
        ind_curp_modif       CHAR(01)     ,
        n_rfc_bd             CHAR(13)     ,
        nombre_bd            CHAR(50)     ,
        nombre_pcanase       CHAR(50)     ,
        fena_bd              CHAR(08)     ,
        codven_bd            CHAR(10)     ,
        sexo_bd              SMALLINT     ,
        estadon_bd           CHAR(02)     ,
        fprimer_afil         CHAR(08)     ,
        falta_actual         CHAR(08)     ,
        cve_afore            CHAR(03)     ,
        nacionalidad_bd      CHAR(03)     ,
        tip_aprob_bd         CHAR(01)     ,
        fol_aprob_bd         CHAR(10)     ,
        doc_aprob_bd         CHAR(16)     ,
        fecha_recep_sello    CHAR(08)     ,
        hora_recep_sello     CHAR(02)     ,
        minuto_recep_sello   CHAR(02)     ,
        seg_recep_sello      CHAR(02)     ,
        cseg_recep_sello     CHAR(02)     ,
        consecutivo_recep    CHAR(08)     ,
        periodo              CHAR(06)     ,
        salario              DECIMAL(9,2) ,
        st_cuenta            CHAR(1)
    END RECORD

    DEFINE
        c_fena              ,
        x_fecha,f1,f2,f3    CHAR(10),
        falta,fpafil,fnbd   DATE

    DEFINE reg_afi RECORD LIKE afi_solicitud.*
    DEFINE reg_mae RECORD LIKE afi_mae_afiliado.*

    DEFINE
        xpaterno ,
        xmaterno ,
        xnombres CHAR(40),
        vnombre  CHAR(50)

    DEFINE comando CHAR(20)

    DEFINE vusuario CHAR(08)

    DEFINE opc CHAR(01)

    DEFINE vtot_act INTEGER
    DEFINE vtot_ina INTEGER
    DEFINE vtot_otr INTEGER

    DEFINE G_LISTA3 CHAR(200)
    DEFINE G_IMPRE3 CHAR(200)
    DEFINE vnombre3 CHAR(200)

    #SALDO CERO
    DEFINE
      ban_cero               SMALLINT

    DEFINE
      v_sql_1                CHAR(50),
      v_sql_2                CHAR(50)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
        PROMPT LINE LAST  ,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG('ASIC001.log')
    CALL inicio()           #i
    CALL inicializa_tablas()
    CALL proceso_principal()  #pp
    CALL prepara_reporte()

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ASIC0011" ATTRIBUTE(BORDER)

   DISPLAY " ASIC001      REGISTRO DE CUENTAS ASIGNADAS A AFORE                            " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

   DISPLAY g_param.ruta_rescate AT 7,10

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*
      AFTER FIELD generar
         IF g_reg.generar IS NULL THEN
            ERROR "Este dato no puede ser nulo"
            NEXT FIELD generar
         END IF

         LET ejecuta = "cd ",g_param.ruta_rescate CLIPPED,"; ls > archivos"
                             CLIPPED
         RUN ejecuta

         WHENEVER ERROR CONTINUE
            DATABASE safre_tmp
            DROP TABLE archivos_afi
         WHENEVER ERROR STOP

         CREATE TABLE archivos_afi
            (campo  CHAR(100))

         DATABASE safre_af

         LET ejecuta = g_param.ruta_rescate CLIPPED,"/archivos" CLIPPED

         LOAD FROM ejecuta INSERT INTO safre_tmp:archivos_afi

         SELECT "X"
         FROM   safre_tmp:archivos_afi
         WHERE  campo = g_reg.generar
         IF STATUS =  100 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO"
            SLEEP 3
            CLEAR FORM
            CLEAR SCREEN
            LET INT_FLAG = TRUE
            NEXT FIELD generar
         END IF

         EXIT INPUT

       ON KEY(INTERRUPT)
          LET INT_FLAG = TRUE
          EXIT INPUT
    END INPUT

    IF INT_FLAG THEN
       LET INT_FLAG = FALSE
       RETURN
    END IF

    PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

    IF opc MATCHES '[Ss]' THEN

        ERROR "Procesando Informacion"

        CALL separa_archivo()
        CALL sube_datos()
        CALL revisa_datos()
        CALL despliega_resultados()
        CALL prepara_reporte()

        PROMPT "Proceso terminado, [Enter] p/salir" FOR enter
    END IF

    CLEAR FORM
    CLEAR SCREEN

END FUNCTION

FUNCTION inicio()
#--------------

-----    LET g_reg.generar = "S"
    LET HOY = TODAY
    LET aceptar    = 0
    LET rechazar   = 0
    LET pendiente  = 0
    LET aclaracion = 0
    LET ban_cero   = 0

    SELECT *,
           USER
    INTO   g_param.*,
           vusuario
    FROM   seg_modulo
    WHERE  modulo_cod = "afi"

    SELECT MAX(n_folio)
    INTO   max_folio
    FROM   afi_det_asignado

    IF SQLCA.SQLCODE <> 0 THEN
       LET max_folio = 0
    END IF

    IF max_folio IS NULL THEN
        LET max_folio = 0
    END IF

    --LET comando = 'sh s_asigna'
    --RUN comando

END FUNCTION

FUNCTION revisa_datos()
#----------------------

    DEFINE sello  CHAR(24)

    DEFINE reg_cza RECORD LIKE afi_cza_asignado.*

    DEFINE
        rechazo_lote	 CHAR(3)                ,
        rechazo_deta	 CHAR(3)                ,
        l_reg		 RECORD LIKE tab_rch_lote.* ,
        x_reg		 RECORD LIKE tab_rdeta.* ,
        aux_pausa	 CHAR(1)

    DEFINE
        rechazo_09	CHAR(02),
        rechazo_001 	CHAR(02),
        rechazo_002 	CHAR(02),
        rechazo_003 	CHAR(02)

    DEFINE
        uno		CHAR(03),
        dos		CHAR(03),
        tre		CHAR(03),
        cua		CHAR(03),
        cin		CHAR(03)

    DEFINE
        i                   SMALLINT,
        l_status_int        SMALLINT,
        aux_status_interno  SMALLINT

    DEFINE
        c_periodo  CHAR(10),
        f_periodo  DATE

    DEFINE v_cae_rech   SMALLINT

    DEFINE
        v_existe      ,
        v_edad        ,
        v_criterio    ,
        v_ind_edad    SMALLINT,
        v_crea_fecha  DATE,
        v_tipo_proc   ,
        v_tipo_trasp  ,
        v_medio       ,
        v_rechazo     SMALLINT,
        v_folioatencion INTEGER

    DEFINE
        v_curp        CHAR(18),
        v_rfc         CHAR(13),
        v_fena        DATE

    LET v_tipo_trasp = 5
    LET v_tipo_proc  = 1
    LET v_medio      = 10

    LET v_sql_1 = "EXECUTE FUNCTION fn_fnacimiento(?,?)"
    LET v_sql_2 = "EXECUTE FUNCTION fn_regimen_inv(?,?,?,?,?,?)"

    PREPARE stmt1 FROM v_sql_1
    PREPARE stmt2 FROM v_sql_2


    # ENCABEZADO #
    SELECT campo1  ,
           campo2  ,
           campo3  ,
           campo4  ,
           campo5  ,
           campo6  ,
           campo7  ,
           campo8  ,
           campo9  ,
           campo10 ,
           campo11
    INTO   reg_cza.tipo_registro,
           reg_cza.ident_servicio,
           reg_cza.ident_operacion,
           reg_cza.tipo_ent_origen,
           reg_cza.cve_ent_origen,
           reg_cza.tipo_ent_destino,
           reg_cza.cve_ent_destino,
           reg_cza.fecha_trans_lote,
           reg_cza.ent_fed_envio,
           reg_cza.consec_dia,
           reg_cza.cve_mod_recep
    FROM safre_tmp:cza_asigna

    INSERT INTO afi_cza_asignado VALUES(reg_cza.*)

    DECLARE cursor_2 CURSOR FOR
    SELECT *
    FROM   safre_tmp:det_asigna
    --ORDER BY contador_servicio

    LET reg_det.salario = 0
    LET f_periodo = NULL

    FOREACH cursor_2 INTO reg_det.*

      LET aceptar = aceptar + 1

      IF reg_det.falta_actual IS NOT NULL THEN
         LET f1    = reg_det.falta_actual[5,6],"/",
                     reg_det.falta_actual[7,8],"/",
                     reg_det.falta_actual[1,4]
      ELSE
         LET f1 = HOY
      END IF

      IF reg_det.fprimer_afil IS NOT NULL THEN
         LET f2   = reg_det.fprimer_afil[5,6],"/",
                    reg_det.fprimer_afil[7,8],"/",
                    reg_det.fprimer_afil[1,4]
      ELSE
         LET f2 = HOY
      END IF

      LET f3 = '01/01/19',reg_det.nss_oficial[5,6]

      LET falta  = f1
      LET fpafil = f2
      LET fnbd   = f3

      CALL habil_siguiente(fpafil) RETURNING diaSig

      LET sello = reg_det.fecha_recep_sello ,
                  reg_det.hora_recep_sello ,
                  reg_det.minuto_recep_sello ,
                  reg_det.seg_recep_sello ,
                  reg_det.cseg_recep_sello,
                  reg_det.consecutivo_recep

      LET vnombre = reg_det.nombre_pcanase

      CALL separa_nombre() RETURNING xpaterno, xmaterno, xnombres

      IF xpaterno IS NULL THEN
         LET xpaterno = 'S/P'
      END IF

      IF reg_det.estadon_bd > 32 THEN
         LET reg_det.estadon_bd = 0
      END IF

      LET reg_det.estadon_bd         = 0
      LET reg_det.sexo_bd            = 0

      LET reg_afi.n_seguro           = reg_det.nss_oficial
      LET reg_afi.n_unico            = reg_det.curp_oficial
      LET reg_afi.n_rfc              = ''
      LET reg_afi.paterno            = xpaterno
      LET reg_afi.materno            = xmaterno
      LET reg_afi.nombres            = xnombres
      LET reg_afi.fena               = fnbd
      LET reg_afi.n_folio            = reg_det.contador_servicio + max_folio
      LET reg_afi.edo_civil          = 0
      LET reg_afi.localn             = ''
      LET reg_afi.estadon            = reg_det.estadon_bd
      LET reg_afi.tiptr              = 0
      LET reg_afi.cod_promotor       = reg_det.codven_bd
      LET reg_afi.sexo               = reg_det.sexo_bd
      LET reg_afi.n_operac           = reg_det.folio_solicitud
      LET reg_afi.frecafor           = fpafil
      LET reg_afi.fentcons           = fpafil
      LET reg_afi.femision           = ''
      LET reg_afi.finitmte           = ''
      LET reg_afi.finicta            = diaSig
      LET reg_afi.status             = 0
      LET reg_afi.agenc_cod          = 0
      LET reg_afi.status_interno     = 100
      LET reg_afi.nacionalidad       = 'MEX'
      LET reg_afi.tip_prob           = 6
      LET reg_afi.fol_prob           = ''
      LET reg_afi.doc_prob           = ''
      LET reg_afi.ind_infonavit      = '0'
      LET reg_afi.documento_1        = ''
      LET reg_afi.documento_2        = ''
      LET reg_afi.documento_3        = ''
      LET reg_afi.documento_4        = ''
      LET reg_afi.documento_5        = ''
      LET reg_afi.documento_6        = ''
      LET reg_afi.envio_dom          = ''
      LET reg_afi.entidad_curp       = ''
      LET reg_afi.asigna_curp        = ''
      LET reg_afi.const_curp         = ''
      LET reg_afi.usuario            = 'cop'
      LET reg_afi.hora               = ''
      LET reg_afi.status_captura     = 0
      LET reg_afi.tipo_solicitud     = 5
      LET reg_afi.fecha_elaboracion  = HOY
      LET reg_afi.lote               = reg_cza.consec_dia
      LET reg_afi.fecha_envio        = ''
      LET reg_afi.cod_esq_comision   = 0
      LET reg_afi.ubicacion          = ''
      LET reg_afi.fecha_1a_afil      = fpafil
      LET reg_afi.indicador_c        = ''
      LET reg_afi.indicador_d        = ''
      LET reg_afi.indicador_e        = ''
      LET reg_afi.cod_error_origen   = ''
      LET reg_afi.folio_edo_cta      = ''
      LET reg_afi.cod_afore_ced      = ''
      LET reg_afi.salario_base_comis = 0.0
      LET reg_afi.salario_actual     = 0.0
      LET reg_afi.fecha_actualiza_sa = ''
      LET reg_afi.coduni_n1          = 0
      LET reg_afi.indicador_comision = 0
      LET reg_afi.codven             = '0000000000'
      LET reg_afi.coor_captura       = ''
      LET reg_afi.lote_captura       = ''
      LET reg_afi.folio_captura      = ''
      LET reg_afi.sello_electronico  = sello

      #SALDO CERO
      SELECT 'X'
      FROM   afi_mae_afiliado
      WHERE  n_seguro = reg_afi.n_seguro

      IF STATUS <> NOTFOUND THEN
         SELECT 'X'
           FROM cta_act_marca
          WHERE nss       = reg_afi.n_seguro
            #AND marca_cod = 150

         IF STATUS <> NOTFOUND THEN
            DELETE FROM cta_act_marca
            WHERE nss       = reg_afi.n_seguro

            UPDATE cta_his_marca
            SET    fecha_fin    = TODAY,
                   usr_desmarca = vusuario
            WHERE  nss          = reg_afi.n_seguro

            INSERT INTO cta_his_cuenta SELECT *
                                       FROM   cta_ctr_cuenta
                                       WHERE  nss = reg_afi.n_seguro

            DELETE FROM cta_nss_regimen
            WHERE  nss = reg_afi.n_seguro

            DELETE FROM cta_regimen
            WHERE  nss = reg_afi.n_seguro

            INSERT INTO afi_his_afiliado SELECT *
                                         FROM afi_mae_afiliado
                                         WHERE n_seguro = reg_afi.n_seguro

            DELETE FROM afi_mae_afiliado
            WHERE n_seguro = reg_afi.n_seguro

            LET ban_cero = 1
         END IF

         UPDATE safre_af:taa_cd_det_cedido
            SET estado   = 99
          WHERE n_seguro = reg_afi.n_seguro
            AND estado  IN (12, 103)
      END IF

      SELECT 'X'
      FROM   afi_mae_afiliado
      WHERE  n_seguro = reg_afi.n_seguro
      AND    tipo_solicitud = 1

      IF SQLCA.SQLCODE <> 0 OR
         ban_cero       = 1 THEN

         INSERT INTO afi_mae_afiliado VALUES(reg_afi.*)

         CASE reg_det.st_cuenta
            WHEN 'A'
               LET reg_det.st_cuenta = '1'
               LET vtot_act          = vtot_act + 1

            WHEN 'I'
               LET reg_det.st_cuenta = '2'
               LET vtot_ina          = vtot_ina + 1

            OTHERWISE
               LET vtot_otr          = vtot_otr + 1
         END CASE

         SELECT "X"
         FROM   cta_ctr_cuenta ccc
         WHERE  ccc.nss = reg_afi.n_seguro

         IF SQLCA.SQLCODE = 0 THEN
            UPDATE cta_ctr_cuenta
            SET    fecha_pri_rcv       = "",
                   fecha_ult_rcv       = "",
                   fecha_pri_general   = "01/01/0001",
                   fecha_ult_general   = "",
                   fecha_vol_pat       = "",
                   fecha_vol_ven       = "",
                   ind_saldo_cero      = 0,
                   fecha_saldo_cero    = "",
                   ind_actividad       = 1,
                   fecha_actividad     = HOY,
                   ind_edad            = "",
                   fecha_edad          = "",
                   criterio_edad       = "",
                   ind_transferencia   = 0,
                   fecha_ind_transf    = "",
                   estado_impresion    = 0,
                   periodo_ult_aporte  = "",
                   dias_cotizados      = 0,
                   ult_sal_integrado   = 0,
                   tipo_informe        = 0,
                   fecha_informe       = "",
                   fecha_registro      = HOY,
                   usuario             = 'cop'
            WHERE  nss                 = reg_afi.n_seguro
         ELSE
            INSERT INTO cta_ctr_cuenta
            VALUES (reg_afi.n_seguro,   #nss
                    "",                 #fecha_pri_rcv
                    "",                 #fecha_ult_rcv
                    "01/01/0001",       #fecha_pri_general
                    "",                 #fecha_ult_general
                    "",                 #fecha_vol_pat
                    "",                 #fecha_vol_ven
                    0,                  #ind_saldo_cero
                    "",                 #fecha_saldo_cero
                    1,                  #ind_actividad
                    HOY,                #fecha_actividad
                    0,                  #ind_edad
                    HOY,                #fecha_edad
                    3,                  #criterio edad
                    0,                  #ind_transferencia
                    HOY,                #fecha transferencia
                    0,                  #estado_impresion,
                    "",                 #periodo_ult_aporte
                    0,                  #dias_cotizados
                    0,                  #ult_sal_integrado
                    0,                  #tipo_informe
                    "",                 #fecha_informe
                    HOY,                #fecha_registro
                    'cop'               #usuario
                    )
         END IF

         LET v_crea_fecha = HOY

         DECLARE curs1 CURSOR FOR stmt1
         OPEN  curs1 USING reg_afi.n_seguro, v_crea_fecha
         FETCH curs1 INTO v_existe, v_edad, v_criterio, v_ind_edad,
                          v_curp, v_rfc, v_fena
         CLOSE curs1

         DECLARE curs2 CURSOR FOR stmt2
         OPEN curs2 USING reg_afi.n_seguro,
                          v_ind_edad,
                          v_ind_edad,
                          v_tipo_proc,
                          v_tipo_trasp,
                          v_medio
         FETCH curs2 INTO v_existe, v_ind_edad, v_rechazo, v_folioatencion
         CLOSE curs2

         IF v_rechazo <> 0 THEN
            INSERT INTO safre_tmp:rch_apertura
            VALUES (reg_afi.n_seguro,v_rechazo)
        END IF

      ELSE
        UPDATE afi_mae_afiliado
        SET    finitmte = fentcons,
               fentcons = reg_afi.fentcons
        WHERE  n_seguro = reg_afi.n_seguro

     END IF

     INSERT INTO afi_det_asignado
     VALUES (reg_afi.n_folio,
             5,
             reg_det.nss_oficial,
             reg_det.clave_operacion,
             reg_det.sexo_bd,
             reg_det.nombre_bd,
             reg_det.nombre_pcanase,
             reg_det.estadon_bd,
             fpafil,
             '',
             0,
             reg_det.st_cuenta)

     LET ban_cero = 0

   END FOREACH

END FUNCTION

FUNCTION despliega_resultados()
#dr----------------------------

    DEFINE total_resp INTEGER

    LET total_resp = aceptar + rechazar + pendiente + aclaracion

    DISPLAY "                TOTAL REGISTROS RECIBIDOS                                      " AT 10,1 ATTRIBUTE ( REVERSE )

    DISPLAY "Total de Registros del lote     : ",
            total_resp USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

    DISPLAY "Cuentas activas            : ",
            vtot_act USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

    DISPLAY "Cuentas inactivas          : ",
            vtot_ina USING "#######&" AT 13,15 ATTRIBUTE ( BOLD )

    DISPLAY "Cuentas sin descripcion    : ",
            vtot_otr USING "#######&" AT 14,15 ATTRIBUTE ( BOLD )

    DISPLAY "                           : ",
            aclaracion USING "#######&" AT 15,15 ATTRIBUTE ( BOLD )


    {DISPLAY "Registros certificados     : ",
            aceptar USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros rechazados       : ",
            rechazar USING "#######&" AT 13,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros pendientes       : ",
            pendiente USING "#######&" AT 14,15 ATTRIBUTE ( BOLD )

    DISPLAY "Registros en aclaracion    : ",
            aclaracion USING "#######&" AT 15,15 ATTRIBUTE ( BOLD ) }

----erm 15 MARZO 2006
  LET G_LISTA3 = g_param.ruta_listados CLIPPED,"/",vusuario CLIPPED,
                 ".REPORTE_TRABASIG." CLIPPED,
                 HOY USING "ddmmyy"

 LET vnombre3 = g_param.ruta_listados CLIPPED, "/ASIC_CIF",
                hoy USING "DDMMYYYY", ".txt"

  START REPORT listado_3 TO G_LISTA3     ---erm 15 Marzo 2006

     OUTPUT TO REPORT listado_3(total_resp,vtot_act,vtot_ina,vtot_otr)

  FINISH REPORT listado_3

  LET G_IMPRE3 = "lp ",G_LISTA3
  RUN G_IMPRE3

    PROMPT "Presione <enter> para continuar " FOR enter

END FUNCTION

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

   DEFINE diaTmp	DATE,
   	  contador	SMALLINT,
	  diaActual	DATE
   
   DEFINE diaHabilSig	DATE,
	  diaSemana	SMALLINT,
	  feriado	SMALLINT,
	  finSemana	SMALLINT
	  
   LET diaHabilSig = diaActual + 1

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

FUNCTION separa_nombre()

   DEFINE
      xpaterno,
      xmaterno,
      xnombres  CHAR(40),
      longitud  CHAR(150),
      i,algo,pos integer,
      opc CHAR(1),
      nom1 CHAR(40),
      nom2 CHAR(40)
      
     LET algo = 0
     LET pos = 0

     LET longitud = LENGTH(vnombre)
     FOR i=1 TO longitud
        IF vnombre[i] = "$" then
           IF algo=0 then
              LET xpaterno = vnombre[1,i-1]
              LET pos = i+1
              LET algo = 1
           ELSE
              IF vnombre[i] ="$" AND vnombre[i-1]="$" THEN 
                 LET xmaterno = ""
                 LET algo = 0
                 LET pos = i+1
              ELSE
                 LET xmaterno = vnombre[pos,i-1]  
                 LET algo = 0
                 LET pos = i+1
              END IF
           END IF
        END IF
     END FOR

     IF pos <> 0 THEN
         LET nom1 = vnombre[pos]
         LET nom2 = vnombre[i-1]

         IF nom1 = " " THEN     
            LET i = i + 1       
         END IF                 

         LET xnombres = vnombre[pos,i-1]
     ELSE
			LET xnombres = 'S/N'
     END IF

     RETURN xpaterno,xmaterno,xnombres

END FUNCTION

FUNCTION separa_archivo()
   LET ejecuta = "head -n 1 ",g_param.ruta_rescate CLIPPED,"/",
                 g_reg.generar CLIPPED," >",
                 g_param.ruta_rescate CLIPPED,"/cza_prueba_asig"
   RUN ejecuta

   LET ejecuta = "sed -e '/^02/!d' ",g_param.ruta_rescate CLIPPED,"/",
                 g_reg.generar CLIPPED," >",
                 g_param.ruta_rescate CLIPPED,"/det_prueba_asig"
   RUN ejecuta

   LET ejecuta = "sed -e '/^09/!d ",g_param.ruta_rescate CLIPPED,"/",
                  g_reg.generar CLIPPED," >",
                  g_param.ruta_rescate CLIPPED,"/sum_prueba_asig"
   RUN ejecuta

END FUNCTION

FUNCTION sube_datos()
   LET ejecuta = "cd ",g_param.ruta_rescate CLIPPED,
       "/;dbload -d safre_tmp -c d_asigna -l det_asigna.log -e 100000  -k "

   RUN ejecuta
  
   LET ejecuta = "cd ",g_param.ruta_rescate CLIPPED,
       "/;dbload -d safre_tmp -c c_asigna -l cza_asigna_log -e 10 -k "

  RUN ejecuta

END FUNCTION

FUNCTION inicializa_tablas()
   WHENEVER ERROR CONTINUE
   DATABASE safre_tmp
   
   DROP TABLE cza_asigna;
   DROP TABLE det_asigna;
   DROP TABLE sum_asigna;

    CREATE TABLE cza_asigna
        (campo1  CHAR(2),
	 campo2  CHAR(2),
	 campo3  CHAR(2),
	 campo4  CHAR(2),
	 campo5  CHAR(3),
	 campo6  CHAR(2),
	 campo7  CHAR(3),
	 campo8  CHAR(8),
	 campo9  CHAR(3),
	 campo10 CHAR(3),
	 campo11 CHAR(2),
	 campo12 CHAR(8),
	 campo13 CHAR(8),
	 campo14 CHAR(8),
	 campo15 CHAR(8),
	 campo16 CHAR(8),
	 campo17 CHAR(1),
	 campo18 CHAR(9),
	 campo19 CHAR(2),
	 campo20 CHAR(3),
	 campo21 CHAR(2),
	 campo22 CHAR(3))
;
    CREATE TABLE det_asigna
        (tipo_registro       CHAR(02)     ,
         contador_servicio   CHAR(10)     ,
         clave_operacion     CHAR(02)     ,
         nss_solicitud       CHAR(11)     ,
         curp_solicitud      CHAR(18)     ,
         rfc_trabajador      CHAR(13)     ,
         paterno             CHAR(40)     ,
         materno             CHAR(40)     ,
         nombres             CHAR(40)     ,
         fecha_nacimiento    CHAR(08)     ,
         clave_promotor      CHAR(10)     ,
         fec_recp_sol_afore  CHAR(08)     ,
         folio_solicitud     INTEGER      ,
         sexo                SMALLINT     ,
         entidad_nacimiento  CHAR(02)     ,
         ind_infonavit       CHAR(01)     ,
         nacionalidad        CHAR(03)     ,
         tip_aprob           CHAR(01)     ,
         fol_aprob           CHAR(10)     ,
         doc_aprob           CHAR(16)     ,
         cod_err_ori         CHAR(04)     ,
         cve_afo_ced         CHAR(03)     ,
         folio_edo_cta       CHAR(08)     ,
         cod_operacion       CHAR(02)     ,
         diag_proceso        CHAR(15)     ,
         ident_lote_origen   CHAR(16)     ,
         nss_oficial         CHAR(11)     ,
         ind_nss_modificado  CHAR(01)     ,
         fec_emision_certif  CHAR(08)     ,
         curp_oficial        CHAR(18)     ,
         ind_curp_modif      CHAR(01)     ,
         n_rfc_bd            CHAR(13)     ,
         nombre_bd           CHAR(50)     ,
         nombre_pcanase      CHAR(50)     ,
         fena_bd             CHAR(08)     ,
         codven_bd           CHAR(10)     ,
         sexo_bd             SMALLINT     ,
         estadon_bd          CHAR(02)     ,
         fprimer_afil        CHAR(08)     ,
         falta_actual        CHAR(08)     ,
         cve_afore           CHAR(03)     ,
         nacionalidad_bd     CHAR(03)     ,
         tip_aprob_bd        CHAR(01)     ,
         fol_aprob_bd        CHAR(10)     ,
         doc_aprob_bd        CHAR(16)     ,
         fecha_recep_sello   CHAR(08)     ,
         hora_recep_sello    CHAR(02)     ,
         minuto_recep_sello  CHAR(02)     ,
         seg_recep_sello     CHAR(02)     ,
         cseg_recep_sello    CHAR(02)     ,
         consecutivo_recep   CHAR(08)     ,
         periodo             CHAR(06)     ,
         salario             DECIMAL(8,2) ,
         st_cta              CHAR(1)
       )
;
    CREATE TABLE sum_asigna
        (campo1  CHAR(2),
	 campo2  CHAR(2),
	 campo3  CHAR(3),
	 campo4  CHAR(8),
	 campo5  CHAR(3),
	 campo6  CHAR(2),
	 campo7  CHAR(2),
	 campo8  CHAR(9),
	 campo9  CHAR(9))

    DATABASE safre_af
    WHENEVER ERROR STOP

END FUNCTION

#########################################################################
FUNCTION prepara_reporte()

  DEFINE g_lista    CHAR(300)
  DEFINE reg_cif    RECORD
         st_cta     CHAR(1),
         st_tot     INTEGER,
         total      INTEGER
                    END RECORD
  DEFINE gimpresion CHAR(300)


  LET g_lista = g_param.ruta_listados CLIPPED, "/ASIC_CIF",
                hoy USING "DDMMYYYY", ".txt"

  START REPORT cifras_ctr TO g_lista

  DECLARE c_cif CURSOR FOR
   SELECT st_cta, count(*)
     FROM safre_tmp:det_asigna
    GROUP BY 1
    ORDER BY 1
  FOREACH c_cif INTO reg_cif.*

     LET reg_cif.total = reg_cif.total + reg_cif.st_tot
     OUTPUT TO REPORT cifras_ctr(reg_cif.*)

  END FOREACH
  FINISH REPORT cifras_ctr

  LET gimpresion = "lp ", g_lista
  RUN gimpresion

END FUNCTION
#########################################################################
REPORT cifras_ctr (lreg_cif)

  DEFINE lreg_cif   RECORD
         st_cta     CHAR(1),
         st_tot     INTEGER,
         total      INTEGER
                    END RECORD

  DEFINE i RECORD LIKE tab_afore_local.*

  OUTPUT
     TOP    MARGIN 1
     BOTTOM MARGIN 0
     LEFT   MARGIN 0
     RIGHT  MARGIN 0
     PAGE   LENGTH 60

  FORMAT
     PAGE HEADER

       SELECT razon_social
         INTO i.razon_social
         FROM tab_afore_local
       
       PRINT COLUMN 01,i.razon_social,
              COLUMN 64,TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT COLUMN 08,"                TOTAL DE REGISTROS ASIGNADOS"
        SKIP 2 LINE
        PRINT COLUMN 08,"ARCHIVO : ",g_reg.generar
        PRINT COLUMN 04,
    "----------------------------------------------------------------------"
    
    ON EVERY ROW
        
        CASE lreg_cif.st_cta
          WHEN 'A' 
             PRINT COLUMN 04, "ACTIVA   : ", lreg_cif.st_tot
          WHEN 'I' 
             PRINT COLUMN 04, "INACTIVAS: ", lreg_cif.st_tot
        END CASE

    ON LAST ROW
        SKIP 5 LINE
        PRINT COLUMN 04, "TOTAL    : ", lreg_cif.total

    PAGE TRAILER
        SKIP 2 LINE
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"

END REPORT
#########################################################################

REPORT listado_3(ltotal_resp,ltot_act,ltot_ina,ltot_otr)    ---erm 13 Marzo 2006

  DEFINE
    ltotal_resp SMALLINT,
    ltot_act    SMALLINT,
    ltot_ina    SMALLINT,
    ltot_otr    SMALLINT


  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
--    PAGE HEADER

    ON LAST ROW

      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="
      PRINT
        COLUMN 15," GENERACION DE ARCHIVO TRABAJADORES ASIGNADOS "
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"---------------------------------------------------"
      SKIP 2 LINE
      PRINT
        COLUMN 03,"Fecha                  : ", TODAY USING "dd-mm-yyyy"
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"Clave Operador         : ", vusuario CLIPPED
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre Archivo Procesar: ","__________________________________"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre de Archivo Procesado: ",g_reg.generar CLIPPED
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"Total Registros del Lote            : ", ltotal_resp USING "#######&"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Total Cuentas Activas               : ", ltot_act USING "#######&"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Total Cuentas Inactivas             : ", ltot_ina USING "#######&"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Total Cuentas sin Descripcion       : ", ltot_otr USING "#######&"
      SKIP 2 LINE
      PRINT
         COLUMN 03,"Nombre y Ruta Reporte a Detalle :"
      PRINT
        COLUMN 05,vnombre3 CLIPPED
      SKIP 2 LINE
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="

END REPORT

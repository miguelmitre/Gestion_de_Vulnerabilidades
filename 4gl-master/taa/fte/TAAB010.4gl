###########################################################################
#Proyecto          => SAFRE  ( MEXICO )                                   #
#Propietario       => E.F.P.                                              #
#Programa TAAB010  => SUBE AFILIADOS POR TRASPASO LIQUIDADOS AL MAESTRO   #
#Sistema           => AFI                                                 #
#Autor             => MAURO MUNIZ CABALLERO                               #
#Fecha actualiz.   => 18 DE ENERO DE 2001                                 #
#Modifico          => EDUARDO JOAQUIN RESENDIZ MEDINA                     #
#Fecha             => 09 DE DICIEMBRE DE 2004                             #
#Modifico          => MAURO MUÑIZ CABALLERO                               #
#Fecha             => 19 DE ABRIL DE 2005                                 #
#                     CRITERIOS APERTURA CUENTA:                          #
#                     < 56 AÑOS S/IND Y 1 SIEFORE POR SB                  #
#                     < 56 AÑOS S/IND Y 2 SIEFORES POR EDAD               #
#                     < 56 AÑOS C/IND Y 1 SIEFORE POR SB                  #
#                     < 56 AÑOS C/IND Y 2 SIEFORES POR EDAD               #
#                     >= 56 AÑOS POR SB1                                  #
#Fecha Modifica    => 30 DE NOVIEMBRE DE 2006 (CUENTAS ADMINISTRADAS)     #
#Actualizacion     => MAURO MUÑIZ CABALLERO   (CUENTAS ADMINISTRADAS)     #
#Fecha Modifica    => 9 DE AAGOSTO DE 2007                                #
#Actualizacion     => MAURO MUÑIZ CABALLERO   (MARCA TRANSF ACRED)        #
#Fecha Modifica    => 06 DE MARZO DE 2008                                 #
#Actualizacion     => JOSUE LISANDRO HUERTA SIERRA                        #
#                  => MULTISIEFORES CIRCULAR 69-2                         #
#Modificacion      => JCPV 25/05/2011                                     #
#REQ:677           => JCPV 31/05/2011 UPD sep_det_reg_sol_reclamante      #
#REQ:750           => JCPV 24/06/2011 Marca cuenta con 280.               #
#REQ:168           => JCPV 04/10/2011 Correct error code -254 (ln 636)    #
#REQ:1243				 	 => FSR  11/04/2013 act finicta en afi_mae_afiliado			#
#REQ:1331					 => FSR  19/06/2013 Se elimina marca 230 y 237 					#
#											colocandolo en la liquidación 											#
###########################################################################

DATABASE safre_af

GLOBALS

    DEFINE HOY       DATE
    DEFINE fecha_bnx DATE
    DEFINE generar   CHAR(1)
    DEFINE HORA      CHAR(8)
    DEFINE g_usuario CHAR(8)
    DEFINE G_LISTA   CHAR(300)
    DEFINE accion    SMALLINT
    DEFINE i         SMALLINT
    DEFINE vfolio    INTEGER
    DEFINE cont_ctas INTEGER
    DEFINE enter     SMALLINT

    DEFINE g_seg_modulo   RECORD LIKE seg_modulo.*

    DEFINE
        vmarca_entra      SMALLINT ,
        vmarca_estado     SMALLINT ,
        vcodigo_rechazo   SMALLINT ,
        ejecuta           CHAR(300),
        xcodigo_marca     SMALLINT ,
        xcodigo_rechazo   SMALLINT ,
        edo_proc          SMALLINT ,
        edo_mod           SMALLINT ,
        edo_acr           SMALLINT ,
        v_desmarca        CHAR(300),
        vmarca            SMALLINT ,
        vcorrelativo      INTEGER  ,
        v_ind_edad        SMALLINT ,
        v_curp            CHAR(18) ,
        v_rfc             CHAR(13) ,
        v_fnacimiento     DATE

    DEFINE exe_sql2          ,
           exq_sql3          CHAR(100)
    DEFINE exe_uni           CHAR(100),
           exe_fnacimiento   CHAR(300)

    DEFINE p_proceso      SMALLINT
    DEFINE p_marca_c      SMALLINT

    DEFINE v_cae_rech          SMALLINT

    DEFINE gs_rechazos    SMALLINT

    DEFINE gc_reporte     CHAR(300)
    DEFINE v_corr              INTEGER              #750
    DEFINE pmarca_entra        SMALLINT             #750
    DEFINE pmarca_causa        SMALLINT             #750
    DEFINE pestado_marca       SMALLINT             #750
    DEFINE pcodigo_rechazo     SMALLINT             #750
END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG("TAAB010.log")
    CALL ERRORLOG ("Versión : CPL-1243")
    CALL inicio()                 #i

    IF accion THEN
        CALL traspasa_datos()     #td
    ELSE
        CALL proceso_principal()  #pp
        DISPLAY "TOTAL DE CUENTAS APERTURADAS :" ,cont_ctas  AT 14,4 ATTRIBUTE(REVERSE)
        DISPLAY "TOTAL DE CUENTAS NO APERTURADAS :" ,gs_rechazos  AT 15,4 ATTRIBUTE(REVERSE)
        DISPLAY "ARCHIVO GENERADO" AT 16,1 ATTRIBUTE(REVERSE)
        DISPLAY gc_reporte CLIPPED AT 17,1 ATTRIBUTE(REVERSE)
        PROMPT" PRESIONE [Enter] PARA SALIR ..." FOR enter
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY    = TODAY
    LET HORA   = TIME
    LET accion = ARG_VAL(1)
    LET vfolio = ARG_VAL(2)

    LET edo_proc        = 237
    LET edo_acr         = 230
    LET edo_mod         = 610
    LET vmarca          = 0
    LET vmarca_estado   = 0
    LET vcodigo_rechazo = 0

    LET p_proceso       = 0
    LET p_marca_c       = 0
    LET cont_ctas       = 0

    LET gs_rechazos     = 0

    SELECT *, USER
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".SUBE_TRASP_MTO" CLIPPED,
                "_",HOY USING "dd-mm-yy","_",HORA CLIPPED

    LET ejecuta    = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"
    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    LET exe_sql2   = " EXECUTE PROCEDURE fn_regimen_inv (?,?,?,?,?,?)"
    LET exe_uni    = " EXECUTE PROCEDURE sp_recupera_marca (?,?,?)"
    LET exe_fnacimiento = "EXECUTE PROCEDURE fn_fnacimiento ( ?, ?)"
    LET exq_sql3   = " EXECUTE FUNCTION fn_valida_edad_sol (?,?) "

    PREPARE prp_fnacimiento FROM exe_fnacimiento
    DECLARE cur_fnacimiento CURSOR FOR prp_fnacimiento

    PREPARE prp_fecha_sol FROM exq_sql3


END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TAAB0101" ATTRIBUTE(BORDER)

    DISPLAY " TAAB010        SUBE AFILIADOS POR TRASPASO AL MAESTRO                         " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                          [ Ctrl-C ] Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio, generar
        AFTER FIELD vfolio
            IF vfolio IS NULL THEN
                ERROR "Folio no puede ser nulo"
                NEXT FIELD vfolio
            ELSE
                NEXT FIELD generar
            END IF
        AFTER FIELD generar
            IF generar NOT MATCHES "[SsNn]" THEN
                ERROR "Opcion solo puede ser S o N"
                NEXT FIELD generar
            ELSE
                IF generar MATCHES "[Nn]" THEN
                    ERROR "PROCESO CANCELADO" SLEEP 2
                    EXIT PROGRAM
                ELSE
                    ERROR "Procesando Informacion... Espere un momento"

                    CALL Traspasa_datos()
                END IF
                EXIT INPUT
            END IF

          ON KEY ( INTERRUPT )
             EXIT PROGRAM
    END INPUT

END FUNCTION

FUNCTION Traspasa_datos()
#td----------------------

    DEFINE afi RECORD LIKE afi_solicitud.*
    DEFINE mae RECORD LIKE afi_mae_afiliado.*
    DEFINE cta RECORD LIKE cta_ctr_cuenta.*

#CPL-1331    DEFINE id_garan     CHAR(1)
#CPL-1331    DEFINE ind_modif    CHAR(1)
    DEFINE v_crea_fecha DATE
    DEFINE HAY          SMALLINT
    DEFINE dias_cotiz   SMALLINT
    DEFINE vtipo_trasp  SMALLINT

    DEFINE v_existe       SMALLINT
    DEFINE v_criterio     SMALLINT
    DEFINE v_tipo_proc    SMALLINT
    DEFINE v_tipo_trasp   SMALLINT
    DEFINE v_medio        SMALLINT
    DEFINE v_edad         SMALLINT
    DEFINE v_rechazo      SMALLINT
    DEFINE vcod_siefore   SMALLINT
    DEFINE v_folioaten    INTEGER

   DEFINE v_query             CHAR(300)
   DEFINE v_tipo_beneficiario INTEGER
   DEFINE v_tramite_ben       INTEGER
   DEFINE v_porcentaje_tot    DECIMAL(5,2)
   DEFINE v_ind_designacion   INTEGER

   DEFINE v_cod_respuesta     CHAR(2)
   DEFINE v_cod_diagnostico   SMALLINT
   DEFINE v_descripcion       VARCHAR(100)

   #CPL-1331 LET id_garan     = 0
   #CPL-1331 LET ind_modif    = 0
    LET v_medio      = 10
    LET v_tipo_proc  = 2
    LET v_tipo_trasp = 5

    PREPARE stmt2 FROM exe_sql2

    IF vfolio IS NULL OR
       vfolio = 0 THEN
        ERROR"EL FOLIO NO PUEDE SER NULO"
        SLEEP 2
        RETURN
    ELSE
        SELECT 'X'
          FROM taa_folio
         WHERE folio = vfolio
           AND tipo in (3,31)
         GROUP BY 1

        IF SQLCA.SQLCODE <> 0 THEN
            ERROR"EL FOLIO NO CORRESPONDE A LA OP 09 TAA RECEPTORA"
            SLEEP 2
            RETURN
        END IF
    END IF

    --CALL archivo()                                         #JCPV

    DECLARE cursor_1 CURSOR FOR
    SELECT A.*,
           tvr.fecha_mov_banxico,
     #CPL-1331      tvr.dias_pag_cuo_soc,
     #CPL-1331      tvr.ident_garantia,
     #CPL-1331      tvr.ind_nom_mod,
           tvr.tipo_traspaso
      FROM afi_solicitud A, taa_viv_recepcion tvr
     WHERE tvr.folio = vfolio
       AND tvr.nss   = A.n_seguro
       AND A.status_interno = 75

    FOREACH cursor_1 INTO afi.*,
                          fecha_bnx,
                          dias_cotiz,
                 #CPL-1331         id_garan,
                 #CPL-1331         ind_modif,
                          vtipo_trasp
display afi.n_seguro
        LET HAY = FALSE

        SELECT COUNT(*)
        INTO   HAY
        FROM   afi_mae_afiliado
        WHERE  n_seguro = afi.n_seguro

        IF HAY THEN
            SELECT *
            INTO   mae.*
            FROM   afi_mae_afiliado ma
            WHERE  ma.n_seguro = afi.n_seguro

            IF SQLCA.SQLCODE = 0 THEN
                SELECT "X"
                FROM   afi_his_afiliado
                WHERE  afi_his_afiliado.n_seguro = mae.n_seguro
                AND    afi_his_afiliado.n_folio        = mae.n_folio
                AND    afi_his_afiliado.tipo_solicitud = mae.tipo_solicitud
                AND    afi_his_afiliado.finicta        = mae.finicta
                 GROUP BY 1

                IF SQLCA.SQLCODE <> 0 THEN
                    INSERT INTO afi_his_afiliado VALUES (mae.*)
                END IF

                SELECT "X"
                  FROM afi_his_afiliado
                 WHERE afi_his_afiliado.n_seguro       = mae.n_seguro
                   AND afi_his_afiliado.n_folio        = mae.n_folio
                   AND afi_his_afiliado.tipo_solicitud = mae.tipo_solicitud
                   AND afi_his_afiliado.finicta        = mae.finicta
                 GROUP BY 1

                IF SQLCA.SQLCODE = 0 THEN
                    DELETE
                    FROM   afi_mae_afiliado
                    WHERE  n_seguro = afi.n_seguro
                END IF
            END IF

            SELECT *
            INTO   cta.*
            FROM   cta_ctr_cuenta cc
            WHERE  cc.nss = afi.n_seguro

            IF cta.nss IS NOT NULL THEN
                INSERT INTO cta_his_cuenta VALUES (cta.*)

                UPDATE cta_ctr_cuenta
                SET    fecha_pri_rcv      = NULL,
                       fecha_ult_rcv      = NULL,
                       fecha_pri_general  = NULL,
                       fecha_ult_general  = NULL,
                       fecha_vol_pat      = NULL,
                       fecha_vol_ven      = NULL,
                       ind_actividad      = 1,
                       fecha_actividad    = HOY,
                       ind_saldo_cero     = 0,
                       fecha_saldo_cero   = NULL,
                       ind_edad           = 0,
                       fecha_edad         = HOY,
                       criterio_edad      = v_criterio,
                       ind_transferencia  = 0,
                       fecha_ind_transf   = HOY,
                       estado_impresion   = 0,
                       periodo_ult_aporte = NULL,
                       dias_cotizados     = dias_cotiz,
                       ult_sal_integrado  = 0,
                       tipo_informe       = 0,
                       fecha_informe      = fecha_informe,
                       fecha_registro     = HOY,
                       usuario            = g_usuario
                WHERE  nss                = afi.n_seguro
            END IF

            DECLARE cur_desmarca CURSOR FOR
            SELECT mc.marca_cod, mc.correlativo
              FROM cta_act_marca mc
             WHERE mc.nss       = afi.n_seguro

            FOREACH cur_desmarca INTO vmarca, vcorrelativo
                CALL desmarca_cuenta ( afi.n_seguro, vmarca, g_usuario,
                                       vcorrelativo)
            END FOREACH

            LET HAY = FALSE
        END IF

        UPDATE taa_cd_det_cedido
           SET estado   = 99
         WHERE n_seguro = afi.n_seguro
           AND estado   IN(12, 103)

        IF NOT HAY THEN
            IF afi.n_unico IS NOT NULL AND
               afi.n_unico <> "                  " AND
               LENGTH(afi.n_unico) = 18 THEN
                LET afi.status_interno = 200
                LET afi.status_captura = 0
            ELSE
                LET afi.status_interno = 100
                LET afi.status_captura = 0
                LET afi.n_unico = NULL
            END IF

            LET afi.status = NULL

            INSERT INTO afi_mae_afiliado VALUES (afi.*)

            INSERT INTO afi_mae_patron    #------- Patrones
            SELECT *
            FROM   afi_patron
            WHERE  n_folio = afi.n_folio
            AND    tipo_solicitud = afi.tipo_solicitud

            WHENEVER ERROR CONTINUE
                INSERT INTO afi_mae_benefici   #------- Beneficiarios
                SELECT *
                FROM   afi_beneficiario
                WHERE  n_folio = afi.n_folio
                AND    tipo_solicitud = afi.tipo_solicitud
                AND    n_seguro = afi.n_seguro
            WHENEVER ERROR STOP

            --Se registra control de designacion para beneficiarios
            LET v_query = "EXECUTE PROCEDURE fn_control_beneficiarios(?,?,?,?,?,?,?)"
            PREPARE exe_control_beneficiarios FROM v_query

            SELECT SUM(porcentaje)
            INTO v_porcentaje_tot
            FROM afi_mae_benefici
            WHERE n_folio = afi.n_folio
            AND tipo_solicitud = afi.tipo_solicitud

            IF v_porcentaje_tot IS NOT NULL AND v_porcentaje_tot > 0 THEN
               LET v_tipo_beneficiario = 2      --Beneficiarios Designados

               LET v_ind_designacion = 1
            ELSE
               LET v_tipo_beneficiario = 5      --SIN Beneficiarios Designados

               LET v_ind_designacion = 0
            END IF

            LET v_tramite_ben = 3      --Traspasos

            DECLARE cur_control_beneficiarios CURSOR FOR exe_control_beneficiarios
            OPEN  cur_control_beneficiarios USING  afi.n_folio,
                                                   afi.tipo_solicitud,
                                                   v_tipo_beneficiario,
                                                   v_tramite_ben,
                                                   v_porcentaje_tot,
                                                   v_ind_designacion,
                                                   g_usuario

            FETCH cur_control_beneficiarios INTO v_cod_respuesta,
                                                 v_cod_diagnostico,
                                                 v_descripcion
            CLOSE cur_control_beneficiarios
      
            EXECUTE prp_fecha_sol USING afi.n_seguro, v_tipo_trasp
                                  INTO v_crea_fecha

            OPEN cur_fnacimiento USING afi.n_seguro, v_crea_fecha
            FETCH cur_fnacimiento INTO v_existe, v_edad, v_criterio, v_ind_edad,
                                       v_curp, v_rfc, v_fnacimiento
            CLOSE cur_fnacimiento

            SELECT "X"
            FROM   cta_ctr_cuenta
            WHERE  cta_ctr_cuenta.nss = afi.n_seguro

            IF SQLCA.SQLCODE = 0 THEN
                UPDATE cta_ctr_cuenta
                SET    ind_edad           = v_edad,
                       fecha_edad         = HOY,
                       criterio_edad      = v_criterio
                WHERE  nss                = afi.n_seguro
            ELSE
                INSERT INTO cta_ctr_cuenta   #------ Control cuenta
                VALUES ( afi.n_seguro,       #nss
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
                         v_edad,             #ind_edad
                         HOY,                #fecha_edad
                         v_criterio,         #criterio_edad
                         0,                  #ind_transferencia
                         HOY,                #fecha_ind_transf
                         0,                  #estado_impresion
                         "",                 #periodo_ult_aporte
                         dias_cotiz,         #dias_cotizados
                         0,                  #ult_sal_integrado
                         0,                  #tipo_informe
                         "",                 #fecha_informe
                         HOY,                #fecha_registro
                         g_usuario           #usuario
                       )
            END IF

            -- CPL-3044
            --
            LET vcod_siefore = 0
            
            SELECT codigo_siefore
              INTO vcod_siefore
              FROM cat_rango_nacimiento
             WHERE id_rango_nacimiento = v_ind_edad
            
            IF SQLCA.SQLCODE = NOTFOUND THEN 
            	 LET vcod_siefore = v_ind_edad 
            END IF 
            --           
            -- CPL-3044            
            
            DECLARE curs2 CURSOR FOR stmt2
            OPEN  curs2 USING afi.n_seguro,v_ind_edad,vcod_siefore,
                              v_tipo_proc,v_tipo_trasp,v_medio

            FETCH curs2 INTO v_existe, v_edad, v_rechazo, v_folioaten

            CLOSE curs2
        END IF

 {       IF id_garan = '1' THEN
            SELECT "X"
            FROM   cta_act_marca
            WHERE  marca_cod = edo_proc
            AND    nss       = afi.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                CALL marca_cuenta (afi.n_seguro, edo_proc, vmarca_estado,
                                   vcodigo_rechazo, g_usuario, vfolio)
            END IF
        END IF

        IF id_garan = '2' THEN
            SELECT "X"
            FROM   cta_act_marca
            WHERE  marca_cod = edo_acr
            AND    nss       = afi.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                CALL marca_cuenta (afi.n_seguro, edo_acr, vmarca_estado,
                                   vcodigo_rechazo, g_usuario, vfolio)
            END IF
        END IF

        LET id_garan = '0'

        IF ind_modif = '1' THEN
            SELECT "X"
            FROM   cta_act_marca
            WHERE  marca_cod = edo_mod
            AND    nss       = afi.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                CALL marca_cuenta (afi.n_seguro, edo_mod, vmarca_estado,
                                   vcodigo_rechazo, g_usuario, vfolio)
            END IF
            LET ind_modif = NULL
        END IF}#CPL-1331

        UPDATE afi_solicitud
        SET    status_interno = 100,
               status_captura = 100,
               finicta        = fecha_bnx
        WHERE  n_seguro       = afi.n_seguro
        AND    n_folio        = afi.n_folio
        AND    tipo_solicitud = afi.tipo_solicitud
        AND    status_interno = 75
        
 				UPDATE afi_mae_afiliado  # CPL-1243
        SET    finicta        = fecha_bnx,
               usuario        = g_usuario
        WHERE  n_seguro       = afi.n_seguro
        AND    n_folio        = afi.n_folio
        AND    tipo_solicitud = afi.tipo_solicitud
        AND    status_interno >= 100

---677--->
--->     Traspaso por separacion de cuentas    <---
        IF  afi.tipo_solicitud = 6  
        OR  afi.tipo_solicitud = 7 THEN
          SELECT "OK"
          FROM sep_det_reg_sol_reclamante a,
               sep_det_solicitud b
          WHERE a.nss = afi.n_seguro
          AND a.estado = 51
          AND a.correlativo = b.idSolicitudSeparacion
          AND b.clasifica_separacion = "B"
          GROUP BY 1

          IF STATUS = NOTFOUND THEN
            UPDATE sep_det_reg_sol_reclamante
            SET estado = 52 -- asociado registrado
            WHERE nss = afi.n_seguro
            AND estado = 51
           ELSE
            UPDATE sep_det_reg_sol_reclamante
            SET estado = 53 -- asociado registrado
            WHERE nss = afi.n_seguro
            AND estado = 51
          END IF
---- 750 ---->
          LET v_corr = 0

          SELECT a.correlativo
           INTO v_corr
          FROM sep_det_reg_sol_reclamante a
          WHERE a.nss = afi.n_seguro
           AND a.estado in (52,53)

          IF v_corr IS NULL THEN
            LET v_corr = 0
          END IF

          LET pmarca_entra = 280
          LET pestado_marca   = 0
          LET pcodigo_rechazo = 0
          LET pmarca_causa = 280
          LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
          "'",afi.n_seguro,"'",
          ",",pmarca_entra,
          ",",v_corr,
          ",",pestado_marca,
          ",",pcodigo_rechazo,
          ",",pmarca_causa,
          ",","'","'", ",",
          "'",g_usuario,"'",")"

          LET ejecuta = ejecuta CLIPPED

          PREPARE clausula_spl_280 FROM ejecuta

          DECLARE cursor_marca_280 CURSOR FOR clausula_spl_280

          OPEN cursor_marca_280

          FETCH cursor_marca_280 INTO xcodigo_marca, xcodigo_rechazo

          CLOSE cursor_marca_280
---- 750 <----
        END IF
---677<---

### LAS MARCAS DE UNIFICACION SE CARGAN EN LA LIQUIDACION###
{        IF vtipo_trasp = 12 OR
           vtipo_trasp = 20 THEN
            PREPARE eje_marca_uni FROM exe_uni

            EXECUTE eje_marca_uni
            USING afi.n_seguro,
                  p_proceso,
                  p_marca_c
        END IF
}
        LET cont_ctas = cont_ctas + 1                    #JCPV
    END FOREACH
    
    CALL archivo()

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
        pmarca_causa SMALLINT

    LET vmarca_causa = 0
    LET vfecha_causa = ""

---- 168 ---->
          LET pmarca_entra = vmarca_entra
          LET pestado_marca   = vmarca_edo
          LET pmarca_causa = 230 
          LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
          "'",vnss,"'",
          ",",pmarca_entra,
          ",",vcorrelativo,
          ",",pestado_marca,
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

FUNCTION desmarca_cuenta (vnss, vmarca, vusuario, vcorrelativo)
#dc------------------------------------------------------------

    DEFINE
        vnss           CHAR(11),
        vmarca         SMALLINT,
        vusuario       CHAR(08),
        vcorrelativo   INTEGER,
        vestado_marca  SMALLINT,
        vmarca_causa   SMALLINT

    LET vestado_marca = 0
    LET vmarca_causa  = 0

    PREPARE eje_desmarca FROM v_desmarca

    EXECUTE eje_desmarca
    USING vnss,
          vmarca,
          vcorrelativo,
          vestado_marca,
          vmarca_causa,
          vusuario

END FUNCTION
################################################################################
FUNCTION archivo()
   DEFINE lar_procesadas ARRAY[100] OF RECORD
   	  tipo_traspaso SMALLINT,
   	  desc_trasp    CHAR(30),
   	  aceptadas     INTEGER,
   	  rechazadas    INTEGER
   END RECORD

   DEFINE ls_cont   ,
          ls_c2     ,                                #JCPV
          ls_tot    SMALLINT

---JCPV--->
    FOR ls_c2 = 1 TO 100
       LET lar_procesadas[ls_c2].tipo_traspaso   = 0
       LET lar_procesadas[ls_c2].desc_trasp      = " "
       LET lar_procesadas[ls_c2].aceptadas       = 0
       LET lar_procesadas[ls_c2].rechazadas      = 0
    END FOR

   DECLARE cur_aceptadas CURSOR FOR
   SELECT tvr.tipo_traspaso,
          c.descripcion,
          count(tvr.nss)
   FROM   afi_mae_afiliado           A,
          taa_viv_recepcion        tvr,
          OUTER tab_tipo_traspaso    c
   WHERE  tvr.folio = vfolio
   AND    tvr.nss   = A.n_seguro
   AND    A.status_interno >= 100                 
   AND    tvr.tipo_traspaso = c.tipo_traspaso
   AND    tvr.ident_operacion = c.id_opera
   AND    tvr.tipo_traspaso NOT IN(83,84,85)
   GROUP BY 1,2
   ORDER BY 1

   DECLARE cur_noapertur CURSOR FOR
   SELECT tvr.tipo_traspaso,
          c.descripcion,
          count(tvr.nss)
   FROM   afi_mae_afiliado           A,
          taa_viv_recepcion        tvr,
          OUTER tab_tipo_traspaso    c
   WHERE  tvr.folio = vfolio
   AND    tvr.nss   = A.n_seguro
   AND    A.status_interno >= 100
   AND    tvr.tipo_traspaso = c.tipo_traspaso
   AND    tvr.ident_operacion = c.id_opera
   AND    tvr.tipo_traspaso  IN(83,84,85)
   GROUP BY 1,2
   ORDER BY 1

--JCPV<---

   LET gc_reporte = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                    ".RPT_TRASP_APT." CLIPPED,
                    vfolio USING "<<<<<<<<<<<<<<<<<<<<<"

   LET ls_cont = 1

   FOREACH cur_aceptadas INTO lar_procesadas[ls_cont].tipo_traspaso,
   	                          lar_procesadas[ls_cont].desc_trasp   ,
   	                          lar_procesadas[ls_cont].aceptadas


   	  LET lar_procesadas[ls_cont].rechazadas = 0

   	  LET ls_cont = ls_cont + 1
   END FOREACH
---JCPV--->
   LET ls_c2 = ls_cont 
   FOREACH cur_noapertur INTO lar_procesadas[ls_c2].tipo_traspaso,
                                  lar_procesadas[ls_c2].desc_trasp   ,
                                  lar_procesadas[ls_c2].rechazadas



          LET ls_c2 = ls_c2 + 1
   END FOREACH
---JCPV<---
   IF  ls_cont > 1  THEN                                          #JCPV
      LET ls_cont = ls_cont - 1                                   #JCPV
   END IF                                                         #JCPV

   START REPORT rpt_cifras TO gc_reporte

   FOR ls_tot = 1 TO ls_cont
          LET cont_ctas     =  cont_ctas                         #JCPV
                            +  lar_procesadas[ls_tot].aceptadas #JCPV
   	  OUTPUT TO REPORT rpt_cifras(lar_procesadas[ls_tot].tipo_traspaso,
   	                              lar_procesadas[ls_tot].desc_trasp   ,
   	                              lar_procesadas[ls_tot].aceptadas    ,
   	                              2
   	                              )
   END FOR

   IF  ls_c2 > 2  THEN                                          #JCPV
      LET ls_c2   = ls_c2   - 1                                   #JCPV
   END IF                                                         #JCPV

   FOR ls_tot = ls_cont + 1 TO ls_c2                                #JCPV 
          LET gs_rechazos   =  gs_rechazos                          #JCPV
                            +  lar_procesadas[ls_tot].rechazadas   #JCPV 

   	  OUTPUT TO REPORT rpt_cifras(lar_procesadas[ls_tot].tipo_traspaso,
   	                              lar_procesadas[ls_tot].desc_trasp   ,
   	                              lar_procesadas[ls_tot].rechazadas   ,
   	                              3
   	                              )
   END FOR

   FINISH REPORT rpt_cifras

END FUNCTION
################################################################################
REPORT rpt_cifras(lr_reporte)
   DEFINE lr_reporte RECORD
   	  tipo_traspaso SMALLINT,
   	  desc_trasp    CHAR(30),
   	  total         INTEGER ,
   	  tipo          SMALLINT
   END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   25

      ORDER EXTERNAL BY lr_reporte.tipo

   FORMAT
   PAGE HEADER
      PRINT COLUMN 001, "                 SUBE AFILIADOS POR TRASPASO AL MAESTRO"
      PRINT COLUMN 001, "PROGRAMA: TAAB010                        FECHA GENERACION:", hoy USING "DD/MM/YYYY"
      PRINT COLUMN 001, "                                         FOLIO: ", vfolio USING "<<<<<<<<<<<<<<<<<<<<<"

   BEFORE GROUP OF lr_reporte.tipo
      CASE lr_reporte.tipo
   	     WHEN 2
   	  	    SKIP 1 LINE
   	  	    PRINT COLUMN 001, "                       CUENTAS APERTURADAS"
   	  	    PRINT COLUMN 001, "ORIGEN   DETALLE                           REGISTROS"

   	  	 WHEN 3
   	  	    SKIP 1 LINE
   	  	    PRINT COLUMN 001, "                       CUENTAS NO APERTURADAS"
   	  	    PRINT COLUMN 001, "ORIGEN   DETALLE                           REGISTROS"
      END CASE

   ON EVERY ROW
      PRINT COLUMN 003, lr_reporte.tipo_traspaso USING "&&",
            COLUMN 010, lr_reporte.desc_trasp              ,
            COLUMN 042, lr_reporte.total         USING "##########&"

   ON LAST ROW
      PRINT COLUMN 001, "TOTAL CUENTAS APERTURADAS.... ", cont_ctas   USING "##########&"
      PRINT COLUMN 001, "TOTAL CUENTAS NO APERTURADAS. ", gs_rechazos USING "##########&"
      PRINT COLUMN 001, "TOTAL GENERAL................ ", cont_ctas + gs_rechazos USING "##########&"

END REPORT
################################################################################
{
                 SUBE AFILIADOS POR TRASPASO AL MAESTRO
PROGRAMA: TAAB010                        FECHA GENERACION:DD/MM/YYYY
                                         FOLIO: ###################&

                       CUENTAS APERTURADAS
ORIGEN   DETALLE                           REGISTROS
  #&     xxxxxxxxxxxxxxxxxxxxxxxxxxxxx   ##########&

                       CUENTAS NO APERTURADAS
ORIGEN   DETALLE                           REGISTROS
  #&     xxxxxxxxxxxxxxxxxxxxxxxxxxxxx   ##########&


TOTAL CUENTAS APERTURADAS     ##########&
TOTAL CUENTAS NO APERTURADAS  ##########&

TOTAL GENERAL                 ##########&
}

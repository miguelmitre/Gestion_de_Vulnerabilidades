###########################################################################
#Proyecto          => SAFRE  ( MEXICO )                                   #
#Propietario       => E.F.P.                                              #
#Programa TAAB024  => SUBE AFIL VIRTUAL DE TRASPASO LIQUIDADOS AL MAESTRO #
#Sistema           => AFI                                                 #
#Autor             => FERNANDO HERRERA HERNANDEZ                          #
#Fecha actualiz.   => 13 DE FEBRERO DE 2009                               #
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
    DEFINE enter               SMALLINT
    DEFINE cont_ctas           INTEGER

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG("TAAB024.log")
    CALL inicio()                 #i

    IF accion THEN
        CALL traspasa_datos()     #td
    ELSE
        CALL proceso_principal()  #pp
        DISPLAY "TOTAL DE CUENTAS APERTURADAS :" ,cont_ctas  AT 14,4 ATTRIBUTE(REVERSE)
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
    LET cont_ctas       = 0

    LET p_proceso       = 0
    LET p_marca_c       = 0

    SELECT *, USER
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".SUBE_TRASP_MTO_VIRT" CLIPPED,
                "_",HOY USING "dd-mm-yy","_",HORA CLIPPED

    LET exe_sql2   = " EXECUTE PROCEDURE fn_regimen_inv_virt (?,?,?,?,?,?)"
    LET exe_fnacimiento = "EXECUTE PROCEDURE fn_fnacimiento_virt ( ?, ?)"
    LET exq_sql3   = " EXECUTE FUNCTION fn_valida_edad_sol_virt (?,?) "

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

    DEFINE id_garan     CHAR(1)
    DEFINE ind_modif    CHAR(1)
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

    LET id_garan     = 0
    LET ind_modif    = 0
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
           AND tipo = 3
         GROUP BY 1

        IF SQLCA.SQLCODE <> 0 THEN
            ERROR"EL FOLIO NO CORRESPONDE A LA OP 09 TAA RECEPTORA"
            SLEEP 2
            RETURN
        END IF
    END IF

    DECLARE cursor_1 CURSOR FOR
    SELECT A.*,
           tvr.fecha_mov_banxico,
           tvr.dias_pag_cuo_soc,
           tvr.ident_garantia,
           tvr.ind_nom_mod,
           tvr.tipo_traspaso
      FROM afi_solicitud A, taa_viv_recepcion tvr
     WHERE tvr.folio = vfolio
       AND tvr.nss   = A.n_seguro
       AND A.status_interno = 75

    FOREACH cursor_1 INTO afi.*,
                          fecha_bnx,
                          dias_cotiz,
                          id_garan,
                          ind_modif,
                          vtipo_trasp

        LET HAY = FALSE

        IF NOT HAY THEN

            INSERT INTO afi_mae_af_virt VALUES (afi.n_seguro,
                                                afi.n_unico,
                                                afi.n_rfc,
                                                afi.fena,
                                                afi.n_folio,
                                                afi.frecafor,
                                                afi.tipo_solicitud,
                                                afi.fecha_elaboracion,
                                                "", --ind_edad      cta_ctr_cuenta
                                                today,  --fecha_edad    cta_ctr_cuenta
                                                "") --criterio_edad cta_ctr_cuenta

            EXECUTE prp_fecha_sol USING afi.n_seguro, vtipo_trasp
                                  INTO v_crea_fecha

            OPEN cur_fnacimiento USING afi.n_seguro, v_crea_fecha
            FETCH cur_fnacimiento INTO v_existe, v_edad, v_criterio, v_ind_edad,
                                       v_curp, v_rfc, v_fnacimiento
            CLOSE cur_fnacimiento
            
            {UPDATE afi_mae_af_virt 
            SET    ind_edad      = v_edad,
                   criterio_edad = v_criterio
            WHERE  n_seguro      = afi.n_seguro}

            DECLARE curs2 CURSOR FOR stmt2

            LET vcod_siefore = v_ind_edad

            OPEN  curs2 USING afi.n_seguro,v_ind_edad,vcod_siefore,
                              v_tipo_proc,vtipo_trasp,v_medio

            FETCH curs2 INTO v_existe, v_edad, v_rechazo, v_folioaten

            CLOSE curs2
        END IF

        LET cont_ctas = cont_ctas + 1
    END FOREACH

END FUNCTION

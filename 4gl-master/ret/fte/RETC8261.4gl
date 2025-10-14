#################################################################################
#Proyecto          => SISTEMA DE AFORES (SAFRE)                                 #
#Propietario       => E.F.P.                                                    #
#Programa RETC8261 => REVERSO. RECEPCION DE ARCHIVO OP.13 RETIRO PARCIAL        #
#Fecha creacion    => 19 DE MARZO DEL 2008                                      #
#By                => FRANCO ESTEBAN ULLOA VIDELA                               #
#Fecha actualiz.   => 9 DE MARZO DE 201O                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Modificaciones para registrar el reverso en las tablas    #
#                     de bitacora                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[4] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE
        gi_folio                INTEGER

    DEFINE
        gs_procesa              SMALLINT
 
    DEFINE
        HOY                     DATE

    DEFINE
        enter                   CHAR(1)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST
    
    CALL STARTLOG("RETC8261.log")
    CALL init()

    CALL f_captura_datos() RETURNING gi_folio, gs_procesa

    IF gs_procesa THEN
        CALL f_reverso(gi_folio) #pp
        CALL f_act_bitacora(gi_folio)
        PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR " FOR CHAR enter
    END IF

    CLOSE WINDOW retc82611
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    
    LET HOY = TODAY
    
    ----- DATOS PARA LA BITACORA DE REVERSOS ----- 
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL"

    LET gr_bitacora.programa      = "RETC8261"
    LET gr_bitacora.desc_tramite  = "CARGA OPERACION 13"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- REVERSA DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE reversa_desmarca(?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION


#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio de carga de archivo con el que se      #
#                   hara el reverso de la carga                             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_lote RECORD LIKE ret_cza_lote.*

    DEFINE
        li_folio        INTEGER

    DEFINE
        ls_procesa      SMALLINT

    DEFINE
        lc_clave        CHAR(003)

    LET ls_procesa = 1

    OPEN WINDOW retc82611 AT 4,4 WITH FORM "RETC82611" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8261  REVERSO DE LA CARGA DE LA OPERACION 13 - PARCIALES                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME li_folio WITHOUT DEFAULTS

        AFTER FIELD li_folio
            IF li_folio IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	            NEXT FIELD li_folio
	        ELSE
	            SELECT *
	            INTO   lr_lote.*
	            FROM   ret_cza_lote
	            WHERE  folio = li_folio

	            DISPLAY "Nombre del archivo   : ", lr_lote.nom_archivo AT 6,16
	            DISPLAY "Fecha de carga   : ", lr_lote.fecha_carga AT 7,20
	        END IF

        ON KEY (ESC)
            IF li_folio IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	            NEXT FIELD li_folio
            ELSE
                CALL f_determina_tipo(li_folio) RETURNING lc_clave

                IF lc_clave = "xxx" THEN
                    ERROR "   EL FOLIO NO CORRESPONDE A LA CARGA DE ARCHIVO" ATTRIBUTE(NORMAL)
                    LET li_folio = 0
                    DISPLAY BY NAME li_folio
	                NEXT FIELD li_folio
                END IF
            END IF
	        EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            LET ls_procesa = 0
            EXIT INPUT
    END INPUT

    IF ls_procesa THEN
        WHILE TRUE
            PROMPT "¿ EJECUTAR REVERSO DE RECEPCION DE LA OP. 13 ? (S/N) : " FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN
                    LET ls_procesa = 1
                ELSE
                    PROMPT "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                    LET ls_procesa = 0
                END IF
                EXIT WHILE
            END IF
        END WHILE
    END IF

    RETURN li_folio, ls_procesa

END FUNCTION

#---------------------------------------------------------------------------#
# f_determina_tipo : Valida que el folio ingresado corresponda a alguno de  #
#                    los tipos de proceso que se cargan                     #
#---------------------------------------------------------------------------#
FUNCTION f_determina_tipo(li_folio)

    DEFINE
        li_folio        INTEGER

    DEFINE
        lc_clave        CHAR(003)

    LET lc_clave = "   "

    -- Verifica si el folio es de parciales
    SELECT "OK"
    FROM   ret_parcial_tx
    WHERE  folio = li_folio
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lc_clave = "xxx"
    END IF

    RETURN lc_clave

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso : Ejecuta el reverso de la operacion solicitada                 #
#---------------------------------------------------------------------------#
FUNCTION f_reverso(pr_datos)

    DEFINE pr_datos RECORD 
        folio           LIKE ret_transf_rx.folio
    END RECORD

    DEFINE 
        li_cont_desm        ,
        li_cont             INTEGER

    DEFINE lr_reg RECORD
        nss         LIKE ret_parcial.nss            ,
        consec      LIKE ret_parcial.consecutivo    ,
        diag_cta    LIKE ret_parcial.diag_cuenta_ind,
        marca_cod   LIKE cta_act_marca.marca_cod    ,
        fecha_ini   LIKE cta_act_marca.fecha_ini
    END RECORD

    LET li_cont_desm    = 0
    LET li_cont         = 0
    -----------------------------------------------------------------------------
        
    DECLARE cur_ptx CURSOR FOR
    SELECT A.nss              , 
           A.consecutivo      ,
           A.diag_cuenta_ind  ,
           B.marca_cod        ,
           B.fecha_ini
    FROM   ret_parcial    A   ,
           cta_his_marca  B   ,
           ret_parcial_tx C
    WHERE  A.nss = B.nss
    AND    A.nss = C.nss
    AND    A.consecutivo = B.correlativo 
    AND    A.consecutivo = C.consecutivo
    AND    C.folio       = pr_datos.folio

    FOREACH cur_ptx INTO lr_reg.*
        IF lr_reg.diag_cta <> 400 THEN

            EXECUTE eje_rev_desmarca USING lr_reg.nss          ,
                                           lr_reg.marca_cod    ,
                                           lr_reg.consec       ,
                                           lr_reg.fecha_ini    

            IF SQLCA.SQLCODE = 0 THEN
                LET li_cont_desm = li_cont_desm + 1
            END IF
        
        END IF
        
        UPDATE ret_parcial
        SET    diag_cuenta_ind = NULL
        WHERE  nss             = lr_reg.nss
        AND    consecutivo     = lr_reg.consec
    
        LET li_cont = li_cont + 1
    
    END FOREACH
  
    DISPLAY "MARCAS REVERSADAS POR FUNCION       : ", li_cont_desm
            USING "<<<,<<&" AT 10,10

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_cont_desm

    -----------------------------------------------------------------------------  

    DISPLAY "R. ACTUALIZADOS EN ret_parcial      : ", li_cont
             USING "<<<,<<&" AT 11,10
    
    LET gar_tablas_rev[2].tabla     = "ret_parcial"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont

    -----------------------------------------------------------------------------  
  
    DELETE
    FROM   ret_parcial_tx
    WHERE  folio = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_parcial_tx     : ", li_cont
            USING "<<<,<<&" AT 12,10

    LET gar_tablas_rev[3].tabla     = "ret_parcial_tx"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_cont

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_cza_lote
    WHERE  folio = pr_datos.folio
   
    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_cza_lote       : ", li_cont
            USING "<<<,<<&" AT 13,10

    LET gar_tablas_rev[4].tabla     = "ret_cza_lote"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_cont

    -----------------------------------------------------------------------------
END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora(p_folio)

    DEFINE p_folio LIKE ret_bitacora_rev.folio
    DEFINE ind_rev LIKE ret_bitacora_rev.id_rev

    DEFINE 
        i       SMALLINT

    LET gr_bitacora.folio       = p_folio
    LET gr_bitacora.fecha_fin   = TODAY
    LET gr_bitacora.hora_fin    = CURRENT HOUR TO SECOND

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR i = 1 TO 4
        IF gar_tablas_rev[i].num_regs > 0 THEN 
            INSERT INTO ret_tablas_rev
            VALUES (ind_rev,
                    gr_bitacora.folio,
                    gar_tablas_rev[i].*
                   )
        END IF
    END FOR
END FUNCTION

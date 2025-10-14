################################################################################
#Owner             => E.F.P.                                                   #
#Programa PENM100  => CAPTURA DE SOLICITUDES DE RETIROS PROGRAMADOS - PENSION  #
#                     MINIMA GARANTIZADA (RETIRO S)                            #
#Version           => CPL                                                      #
#Fecha creacion    => NOVIEMBRE 2013                                           #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => 28 DE ABRIL DE 2011                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Se agrega el campo "importe mensual". Se modifican los   #
#                     criterios de validacion de insuficiencia (Req - EFPS 155)#
#Actualizacion     => ISAI JIMENEZ ROJAS 10-JUNIO-2013                         #
#                  => Se incorpora llamado a programa nuevo de manejo de bene- #
#                  => ficiarios de acuerdo a requerimiento MLM-1893            #
#Actualizacion     => ISAI JIMENEZ ROJAS 25-JUNIO-2013                         #
#                  => Se soluciona falla reportada en MLM-1979 para aceptar    #
#                  => resoluciones con diagnostico 501                         #
#Actualizacion     => ISAI JIMENEZ ROJAS 11-NOV-2013 v1.3                      #
#                  => Se soluciona falla reportada en MLM-1936 y MLM-2128      #
#                  => para permitir la generacion correcta de ultimo pago y    #
#                  => permitir la modificacion de datos bancacarios cuando la  #
#                  => solicitud se encuentre en proceso de pago                #
#Actualizacion     => ISAI JIMENEZ ROJAS 07-ENE-2014 v1.4                      #
#                  => Se modifica calculo de retroactivo con base a la fecha   #
#                  => de pago comentado en incidencia CPL-1414 se genera       #
#                  => propia para CPL                                          #
#Actualizacion     => ISAI JIMENEZ ROJAS 21-MAR-2014 v1.5                      #
#                  => Se corrige procedimiento de captura para nuevas secuencia#
#                  => de contrato sin considerar retroactivo CPL-1574          #
#Sistema           => PEN                                                      #
################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD
        capturado   LIKE pen_estado_pmg.estado_solicitud    ,
        confirmado  LIKE pen_estado_pmg.estado_solicitud    ,
        liquidado   LIKE pen_estado_pmg.estado_solicitud    ,
        rechazado   LIKE pen_estado_pmg.estado_solicitud    ,
        en_proceso  LIKE pen_estado_pmg.estado_solicitud       --MLM-1936, 2128
    END RECORD

    DEFINE gr_param RECORD
        accion      CHAR(001)                           ,
        nss         LIKE pen_solicitud_pmg.nss          ,
        consec      LIKE pen_solicitud_pmg.consecutivo  ,
        tipo_ret    LIKE pen_solicitud_pmg.tipo_retiro        
    END RECORD

    DEFINE
        gs_cod_afore        SMALLINT

    DEFINE
        enter               CHAR(001) ,
        gc_usuario          CHAR(020) ,
        g_tecla             CHAR(1)

    DEFINE
        HOY                 DATE
    DEFINE g_mensaje          CHAR(80)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PENM100.log")
    CALL init()

    OPEN WINDOW main_win AT 2,2 WITH 21 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " PENM100     CAPTURA DE SOLICITUDES - PENSION MINIMA GARANTIZADA                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    IF (gr_param.accion IS NOT NULL) AND (gr_param.accion <> " ") THEN
        CALL f_despliega_datos(gr_param.accion)
    ELSE
        MENU "MANTENEDOR"
            COMMAND "Captura" "Captura una solicitud de PMG"
                CALL f_procesa_captura()
        
            COMMAND "Consulta" "Consulta de solicitudes de PMG"
                CALL f_despliega_datos("C")
        
            COMMAND "Modifica" "Modifica solicitudes capturadas de PMG"
                CALL f_despliega_datos("M")
        
            COMMAND "Elimina" "Elimina solicitudes capturadas de PMG"
                CALL f_despliega_datos("E")
                
            --COMMAND "Datos bancarios" "Modifica Datos Bancarios de una solicitud en proceso"
            --    CALL f_despliega_datos("D")
        
            COMMAND "Salir" "Salir del Programa "
                EXIT MENU
        END MENU
    END IF 

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY = TODAY

    ----- PARAMETROS DE ENTRADA -----
    LET gr_param.accion     = ARG_VAL(1)
    LET gr_param.nss        = ARG_VAL(2)
    LET gr_param.consec     = ARG_VAL(3)
    LET gr_param.tipo_ret   = ARG_VAL(4)
    
    ----- CODIGOS AFORES -----
    SELECT codigo_afore   ,
           USER
    INTO   gs_cod_afore ,
           gc_usuario
    FROM   tab_afore_local

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud                         --MLM-1936, 2128
    INTO   gr_edo.en_proceso                          --MLM-1936, 2128
    FROM   pen_estado_pmg A                           --MLM-1936, 2128
    WHERE  A.descripcion = "EN PROCESO DE PAGO"       --MLM-1936, 2128


    ----- VALIDA SALDOS PMG -----
    LET lc_prepare = " EXECUTE FUNCTION fn_valida_suficiencia_pmg(?,?,?,?) "
    PREPARE eje_valida_pmg FROM lc_prepare

    LET lc_prepare = " "

    ----- VALIDA MONTO CAPTURADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_valida_pago_mensual(?) "
    PREPARE eje_valida_mto FROM lc_prepare

    LET lc_prepare = " "

    ----- DIA DEL PRIMER PAGO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_habil_siguiente(?,?) "
    PREPARE eje_habil_sig FROM lc_prepare

    LET lc_prepare = " "

    ----- SALDO AL DIA  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- MARCAJE -----
    LET lc_prepare = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "
    PREPARE eje_marca FROM lc_prepare

    LET lc_prepare = " "

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

    ----- ULTIMO CONSECUTIVO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_ret_consecutivo() "
    PREPARE eje_consecutivo FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_procesa_captura : Ejecuta los pasos para realizar la captura de         #
#                     solicitud de retiros por PMG                          #
#---------------------------------------------------------------------------#
FUNCTION f_procesa_captura()

    DEFINE lc_nss LIKE pen_solicitud_pmg.nss

    DEFINE
        ls_captura          SMALLINT

        IF f_valida_precio_acc(HOY) <> 0 THEN
            CALL f_tablas_tmp()
            CALL f_captura() RETURNING ls_captura, lc_nss

            IF ls_captura <> 0 THEN
                CALL f_guarda_solicitud(lc_nss)
            END IF
            CLOSE WINDOW penm1001
        END IF

END FUNCTION



#=============================================================================#
# Objetivo : Captura solicitud y guardala en base de datos                    #
#=============================================================================#
FUNCTION f_captura()

    DEFINE lr_solicitud       RECORD
           nss                LIKE pen_solicitud_pmg.nss              ,
           n_rfc              LIKE afi_mae_afiliado.n_rfc             ,
           curp               LIKE pen_solicitud_pmg.curp             ,
           paterno            LIKE afi_mae_afiliado.paterno           ,
           materno            LIKE afi_mae_afiliado.materno           ,
           nombres            LIKE afi_mae_afiliado.nombres           ,
           tipo_retiro        LIKE tab_retiro.tipo_retiro             ,
           descripcion        LIKE tab_retiro.descripcion             ,
           regimen            LIKE ret_det_datamart.regimen           ,
           tipo_seguro        LIKE ret_det_datamart.tipo_seguro       ,
           desc_seguro        CHAR(40)                                ,
           tipo_pension       LIKE ret_det_datamart.tipo_pension      ,
           desc_pension       CHAR(40)                                ,
           tipo_prestacion    LIKE ret_det_datamart.tipo_prestacion   ,
           desc_prestacion    CHAR(40)                                ,
           estado_sub_viv     LIKE ret_det_datamart.estado_sub_viv    ,
           sec_pension        LIKE ret_det_datamart.sec_pension       ,
           importe_mensual    LIKE pen_solicitud_pmg.importe_mensual  ,
           fecha_ini_pen      LIKE ret_det_datamart.fecha_ini_pen     ,
           fec_ini_pago       LIKE ret_det_datamart.fec_ini_pago      ,
           fecha_resolucion   LIKE ret_det_datamart.fecha_resolucion  ,
           fecha_solicitud    LIKE pen_solicitud_pmg.fecha_solicitud  ,
           folio_solicitud    LIKE pen_solicitud_pmg.folio_solicitud  ,
           folio_contrato     LIKE pen_solicitud_pmg.folio_contrato   ,
           sec_contrato       LIKE pen_solicitud_pmg.sec_contrato     ,
           folio_lote         LIKE pen_solicitud_pmg.folio_lote       ,
           fecha_captura      LIKE pen_solicitud_pmg.fecha_captura    ,
           fecha_modifica     LIKE pen_solicitud_pmg.fecha_modifica   ,
           fecha_confirma     LIKE pen_solicitud_pmg.fecha_confirma   ,
           fecha_pago         LIKE pen_solicitud_pmg.fecha_pago       ,
           consecutivo        LIKE pen_solicitud_pmg.consecutivo      ,
           usuario_captura    LIKE pen_solicitud_pmg.usuario_captura  ,
           usuario_modifica   LIKE pen_solicitud_pmg.usuario_modifica ,
           usuario_confirma   LIKE pen_solicitud_pmg.usuario_confirma ,
           estado_solicitud   LIKE pen_solicitud_pmg.estado_solicitud ,
           desc_estado        CHAR(40)
           END RECORD

    DEFINE li_folio_dtm            LIKE ret_det_datamart.folio
    DEFINE lc_cad_msg              CHAR(100)
    DEFINE ls_captura              SMALLINT
    DEFINE ls_procesa              SMALLINT
    DEFINE ls_bandera_fip          SMALLINT
    DEFINE ls_int_flag             SMALLINT

    --------------------------------------------------------------------------

    CALL f_abre_ventana()

    DISPLAY " AGREGA SOLICITUD " AT 1,61 ATTRIBUTE(REVERSE)
    DISPLAY " ESC    : Guarda Solicitud " AT 1,1

    LET ls_captura                      = 1
    LET lr_solicitud.fecha_captura      = HOY
    LET lr_solicitud.fecha_solicitud    = HOY
    LET lr_solicitud.usuario_captura    = gc_usuario
    LET lr_solicitud.tipo_retiro        = "S"
    LET lr_solicitud.estado_solicitud   = gr_edo.capturado

    SELECT descripcion
    INTO   lr_solicitud.desc_estado
    FROM   pen_estado_pmg
    WHERE  estado_solicitud = gr_edo.capturado

    --DESPLIEGA LOS DATOS QUE TENGA LA RESOLUCION 
    DISPLAY BY NAME lr_solicitud.fecha_captura      ,
                    lr_solicitud.fecha_solicitud    ,
                    lr_solicitud.usuario_captura    ,
                    lr_solicitud.tipo_retiro        ,
                    lr_solicitud.estado_solicitud   ,
                    lr_solicitud.desc_estado

     ---------------------------------------------------
     -- CAPTURA DE LOS DATOS PRINCIPALES DE LA SOLICITUD
     ---------------------------------------------------
     INPUT BY NAME lr_solicitud.nss             ,
                 --lr_solicitud.fecha_ini_pen   ,     --MLM-1936
                   lr_solicitud.fecha_solicitud ,
                   lr_solicitud.folio_solicitud ,
                   lr_solicitud.folio_contrato  ,
                   lr_solicitud.importe_mensual WITHOUT DEFAULTS

        -------------------
        AFTER FIELD nss
        -------------------
            IF lr_solicitud.nss IS NULL THEN
                CALL f_error_msg("NSS NO PUEDE SER NULO")
                NEXT FIELD nss
            ELSE
                -- Valida que no exista una solicitud de retiro activa para el nss
                CALL f_valida_sol_previa(lr_solicitud.nss) RETURNING ls_procesa ,
                                                                     lc_cad_msg

                IF ls_procesa <> 0 THEN
                    CALL f_error_msg(lc_cad_msg)
                    NEXT FIELD nss
                END IF

                -- Valida que el nss este afiliado y regresa sus datos
                CALL f_obtiene_nombres(lr_solicitud.nss) RETURNING ls_procesa           ,
                                                                   lc_cad_msg           ,
                                                                   lr_solicitud.paterno ,
                                                                   lr_solicitud.materno ,
                                                                   lr_solicitud.nombres ,
                                                                   lr_solicitud.curp    ,
                                                                   lr_solicitud.n_rfc
                IF ls_procesa <> 0 THEN
                    CALL f_error_msg(lc_cad_msg)
                    NEXT FIELD nss
                ELSE
                    DISPLAY BY NAME lr_solicitud.paterno            ,
                                    lr_solicitud.materno            ,
                                    lr_solicitud.nombres            ,
                                    lr_solicitud.curp               ,
                                    lr_solicitud.n_rfc
                END IF

                CALL f_obtiene_matderecho(lr_solicitud.tipo_retiro)
                    RETURNING lr_solicitud.regimen         ,
                              lr_solicitud.tipo_seguro     ,
                              lr_solicitud.tipo_pension    ,
                              lr_solicitud.tipo_prestacion ,
                              lr_solicitud.desc_seguro     ,
                              lr_solicitud.desc_pension    ,
                              lr_solicitud.desc_prestacion ,
                              lr_solicitud.descripcion

                -- Valida que el nss este en el datamart
                CALL f_obtiene_datamart(lr_solicitud.nss             ,
                                        lr_solicitud.regimen         ,
                                        lr_solicitud.tipo_seguro     ,
                                        lr_solicitud.tipo_pension    ,
                                        lr_solicitud.tipo_prestacion )
                    RETURNING ls_procesa                    ,
                              lc_cad_msg                    ,
                              lr_solicitud.fecha_ini_pen    ,
                              lr_solicitud.fec_ini_pago     ,
                              lr_solicitud.estado_sub_viv   ,
                              lr_solicitud.sec_pension      ,
                              lr_solicitud.fecha_resolucion ,
                              li_folio_dtm

                IF ls_procesa <> 0 THEN
                    CALL f_error_msg(lc_cad_msg)

                    LET lr_solicitud.paterno  = " "
                    LET lr_solicitud.materno  = " "
                    LET lr_solicitud.nombres  = " "
                    LET lr_solicitud.curp     = " "
                    LET lr_solicitud.n_rfc    = " "

                    DISPLAY BY NAME lr_solicitud.paterno            ,
                                    lr_solicitud.materno            ,
                                    lr_solicitud.nombres            ,
                                    lr_solicitud.curp               ,
                                    lr_solicitud.n_rfc

                    NEXT FIELD nss
                ELSE
                    CALL f_valida_tab_insuficiencia(lr_solicitud.nss            ,
                                                    lr_solicitud.sec_pension    ,
                                                    li_folio_dtm                )
                        RETURNING ls_procesa ,
                                  lc_cad_msg

                    IF ls_procesa <> 0 THEN
                        CALL f_error_msg(lc_cad_msg)
                        NEXT FIELD nss
                    ELSE
                        DISPLAY BY NAME lr_solicitud.regimen            ,
                                        lr_solicitud.tipo_seguro        ,
                                        lr_solicitud.tipo_pension       ,
                                        lr_solicitud.tipo_prestacion    ,
                                        lr_solicitud.desc_seguro        ,
                                        lr_solicitud.desc_pension       ,
                                        lr_solicitud.desc_prestacion    ,
                                        lr_solicitud.descripcion

                        DISPLAY BY NAME lr_solicitud.fecha_ini_pen      ,
                                        lr_solicitud.estado_sub_viv     ,
                                        lr_solicitud.sec_pension        ,
                                        lr_solicitud.fecha_resolucion

                    END IF -- Insuficiencia desde Datamart
                END IF -- Nss existe en Datamart
            END IF -- Nss no nulo
            
            --VALIDA EXISTENCIA DE LAS FECHA PARA EFECTO DE CALCULO RETROACTIVO
            IF ( lr_solicitud.fecha_ini_pen IS NULL          OR
                 lr_solicitud.fecha_ini_pen = "01/01/0001"   OR
                 lr_solicitud.fecha_ini_pen = "00/01/0001" )
               AND 
               ( lr_solicitud.fec_ini_pago  IS NULL          OR
                 lr_solicitud.fec_ini_pago  = "01/01/0001"   OR
                 lr_solicitud.fec_ini_pago  = "00/01/0001" ) THEN

               --SE COMENTA POR SI SE REACTIVA REQUERIMIENTO
               --CALL f_captura_fecha_pago() RETURNING lr_solicitud.fec_ini_pago,
               --                                    ls_int_flag
               
               CALL f_notifica_inconsistencia()
 
               CLEAR FORM               
               ERROR "OPERACION CANCELADA"
               NEXT FIELD nss
             END IF
            
{
        ----------------------------
        BEFORE FIELD fecha_ini_pen  --MLM-1936
        ----------------------------
            LET ls_bandera_fip = 0     -- No requiere actualización
            
            IF lr_solicitud.fecha_ini_pen IS NULL        OR 
               lr_solicitud.fecha_ini_pen = "01/01/0001" OR
               lr_solicitud.fecha_ini_pen = "00/01/0001" THEN
               LET ls_bandera_fip = 1  --requiere actualizacion en datamart
            ELSE
               LET ls_bandera_fip = 0
               NEXT FIELD NEXT
            END IF

        ----------------------------
        AFTER FIELD fecha_ini_pen   --MLM-1936
        ----------------------------
            CALL f_valida_fechas(lr_solicitud.fecha_ini_pen)
                RETURNING ls_procesa, lc_cad_msg

            IF ls_procesa <> 0 THEN
                CALL f_error_msg(lc_cad_msg)
                NEXT FIELD fecha_ini_pen
            END IF

            --CONTROLA BANDERA PARA ACTUALIZAR LA FIP DE LA RESOLUCION
            IF ls_bandera_fip = 1 THEN
               --requiere actualizacion en datamart
               WHILE TRUE
                  PROMPT "DESEA ACTUALIZAR LA FECHA EN LA RESOLUCION (S/N): "
                         FOR CHAR g_tecla
                  IF g_tecla MATCHES "[sSnN]" THEN
                     EXIT WHILE
                  END IF
               END WHILE 
               
               IF g_tecla MATCHES "[nN]" THEN
                  INITIALIZE lr_solicitud.fecha_ini_pen TO NULL
                  DISPLAY BY NAME lr_solicitud.fecha_ini_pen
                  LET ls_bandera_fip = 0
                  NEXT FIELD fecha_ini_pen
               ELSE
                  WHENEVER ERROR CONTINUE
                     UPDATE ret_det_datamart 
                     SET    fecha_ini_pen    = lr_solicitud.fecha_ini_pen
                     WHERE  nss              = lr_solicitud.nss
                     AND    sec_pension      = lr_solicitud.sec_pension
                     AND    fecha_resolucion = lr_solicitud.fecha_resolucion
                     AND    tipo_retiro      = "S"
                     
                     IF SQLCA.SQLCODE != 0 THEN
                        ERROR "SE PRESENTO UN ERROR DURANTE LA ACTUALIZACION A DATAMART..."
                        INITIALIZE lr_solicitud.fecha_ini_pen TO NULL
                        DISPLAY BY NAME lr_solicitud.fecha_ini_pen
                        LET ls_bandera_fip = 0
                        NEXT FIELD fecha_ini_pen
                     ELSE
                        LET ls_bandera_fip = 0
                        LET g_mensaje = "SE ACTUALIZO ",
                                        SQLCA.SQLERRD[3] USING "<<&",
                                        " RESOLUCIONES, PRESIONE <ENTER>:"
                       --PROMPT g_mensaje FOR CHAR enter
                        PROMPT "RESOLUCION ACTUALIZADA, PRESIONE <ENTER>: " FOR CHAR enter
                     END IF
                  WHENEVER ERROR STOP
                  
               END IF
            END IF 
}
        
        ----------------------------
        AFTER FIELD fecha_solicitud
        ----------------------------
            IF( FGL_LASTKEY()=FGL_KEYVAL("LEFT") OR 
                FGL_LASTKEY()=FGL_KEYVAL("UP")  
               ) AND ls_bandera_fip = 0              THEN
               NEXT FIELD nss 
            END IF
          
            CALL f_valida_fechas(lr_solicitud.fecha_solicitud)
                RETURNING ls_procesa, lc_cad_msg

            IF ls_procesa <> 0 THEN
                CALL f_error_msg(lc_cad_msg)
                NEXT FIELD fecha_solicitud
            END IF

        ----------------------------
        AFTER FIELD folio_contrato
        ----------------------------
            IF (lr_solicitud.folio_contrato IS NULL) OR (lr_solicitud.folio_contrato = 0) THEN
                CALL f_error_msg("FOLIO CONTRATO NO PUEDE SER NULO")
                NEXT FIELD folio_contrato
            ELSE
                CALL f_obtiene_sec_contrato(lr_solicitud.nss)
                    RETURNING lr_solicitud.sec_contrato

                DISPLAY BY NAME lr_solicitud.sec_contrato
            END IF
        
        ----------------------------
        AFTER FIELD importe_mensual
        ----------------------------
            IF (lr_solicitud.importe_mensual IS NULL) OR (lr_solicitud.importe_mensual = 0) THEN
                CALL f_error_msg("IMPORTE MENSUAL NO PUEDE SER NULO")
                NEXT FIELD importe_mensual
            END IF

        ----------------------------
        ON KEY (ESC)
        ----------------------------
            -- Valida que no salga sin valores nulos
            IF lr_solicitud.nss IS NULL THEN
                CALL f_error_msg("NSS NO PUEDE SER NULO")
                NEXT FIELD nss
            END IF

            CALL f_valida_fechas(lr_solicitud.fecha_solicitud)
                RETURNING ls_procesa, lc_cad_msg

            IF ls_procesa <> 0 THEN
                CALL f_error_msg(lc_cad_msg)
                NEXT FIELD fecha_solicitud
            END IF

            IF (lr_solicitud.folio_contrato IS NULL) OR (lr_solicitud.folio_contrato = 0) THEN
                CALL f_error_msg("FOLIO CONTRATO NO PUEDE SER NULO")
                NEXT FIELD folio_contrato
            END IF

            IF (lr_solicitud.importe_mensual IS NULL) OR (lr_solicitud.importe_mensual = 0) THEN
                CALL f_error_msg("IMPORTE MENSUAL NO PUEDE SER NULO")
                NEXT FIELD importe_mensual
            END IF

            #-- Valida suficiencia de saldos de la PMG
            CALL f_valida_suficiencia(lr_solicitud.nss              ,
                                      lr_solicitud.curp             ,
                                      lr_solicitud.sec_pension      ,
                                      lr_solicitud.sec_contrato     ,
                                      lr_solicitud.fecha_ini_pen    ,
                                      lr_solicitud.importe_mensual
                                     )
                RETURNING ls_procesa, lc_cad_msg

            IF ls_procesa <> 0 THEN
                CALL f_error_msg(lc_cad_msg)
                LET ls_captura = 0
                EXIT INPUT
            END IF

            WHILE TRUE
               PROMPT "¿ GUARDAR SOLICITUD ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
               IF enter MATCHES "[sSnN]" THEN
                  IF enter MATCHES "[sS]" THEN
                     CALL f_inserta_sol_tmp(lr_solicitud.*)
                     LET ls_captura = 1
                     EXIT INPUT
                  ELSE
                     CALL f_error_msg("PROCESO CANCELADO")
                     LET ls_captura = 0
                     EXIT INPUT
                  END IF
               END IF
            END WHILE

        #--
        ON KEY (INTERRUPT, CONTROL-C)
            CALL f_error_msg("PROCESO CANCELADO")
            LET ls_captura = 0
            EXIT INPUT

    END INPUT

    RETURN ls_captura, lr_solicitud.nss

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_captura_fecha_pago()

   DEFINE lc_fec_ini_pago_aux      CHAR(10)
   DEFINE ld_fec_ini_pago_aux      DATE  
   DEFINE ls_int_flag              SMALLINT 
   
   LET ls_int_flag = FALSE

   OPEN WINDOW waux1 AT 10,12 WITH 6 ROWS, 60 COLUMNS ATTRIBUTE(BORDER)
   
      WHILE TRUE

         DISPLAY "LA RESOLUCION CONTIENE UNA FECHA DE PAGO NULA LA CUAL " AT 2,4
         DISPLAY "ES REQUERIDA PARA EL CALCULO DE RETROACTIVO PUEDE     " AT 3,4
         DISPLAY "CAPTURARLA PARA EFECTOS DE CALCULO UNICAMENTE.        " AT 4,4
         
         PROMPT "DESEA CAPTURARLA (S/N):" FOR CHAR g_tecla 
         
         IF INT_FLAG OR g_tecla MATCHES "[nN]" THEN
            LET ls_int_flag         = TRUE
            LET ld_fec_ini_pago_aux = NULL
            LET INT_FLAG            = FALSE
            EXIT WHILE
         END IF

         IF g_tecla MATCHES "[sS]" THEN
            CLEAR WINDOW waux1
            DISPLAY "INGRESE LA FECHA INICIO DE PAGO EN FORMATO DD/MM/AAAA" at 3,2
            PROMPT "FECHA: " FOR lc_fec_ini_pago_aux
            
            IF INT_FLAG THEN
               LET ls_int_flag = TRUE
               LET INT_FLAG    = FALSE
               EXIT WHILE
            END IF

            LET ld_fec_ini_pago_aux = MDY(lc_fec_ini_pago_aux[4,5],
                                          lc_fec_ini_pago_aux[1,2],
                                          lc_fec_ini_pago_aux[7,10])
            
            IF STATUS < 0 OR ld_fec_ini_pago_aux IS NULL THEN
               CLEAR WINDOW waux1
               ERROR "FECHA INVALIDA!!!"
               CONTINUE WHILE
            ELSE
               EXIT WHILE
            END IF
         END IF
      END WHILE
       
   CLOSE WINDOW waux1 
   
   RETURN ld_fec_ini_pago_aux, ls_int_flag

END FUNCTION 

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_notifica_inconsistencia()

   OPEN WINDOW waux1 AT 10,6 WITH 8 ROWS, 70 COLUMNS ATTRIBUTE(BORDER)
   
      DISPLAY "LA RESOLUCION ENCONTRADA TIENE FECHA INICIO DE PENSION y FECHA DE " AT 2,4
      DISPLAY "PAGO NULA o INVALIDA LAS CUALES SON REQUERIDAS PARA EL CALCULO DE " AT 3,4
      DISPLAY "RETROACTIVO.                                                      " AT 4,4
      DISPLAY "LA SOLICITUD NO PUEDE SER CAPTURADA.                              " AT 6,4
      
      PROMPT "PRESIONE ENTER PARA REGRESAR:" FOR CHAR g_tecla 
         
   CLOSE WINDOW waux1 
   
END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_datos : Realiza la consulta para desplegar los datos          #
#                     requeridos por el usuario para realizar la consulta o #
#                     la eliminacion de una solicitud                       #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_datos(pc_tipo_oper)

    DEFINE
        pc_tipo_oper        CHAR(01)

    DEFINE lar_consulta ARRAY[5000] OF RECORD
        nss                 LIKE pen_solicitud_pmg.nss              ,
        n_rfc               LIKE afi_mae_afiliado.n_rfc             ,
        curp                LIKE pen_solicitud_pmg.curp             ,
        paterno             LIKE afi_mae_afiliado.paterno           ,
        materno             LIKE afi_mae_afiliado.materno           ,
        nombres             LIKE afi_mae_afiliado.nombres           ,
        tipo_retiro         LIKE tab_retiro.tipo_retiro             ,
        descripcion         LIKE tab_retiro.descripcion             ,
        regimen             LIKE ret_det_datamart.regimen           ,
        tipo_seguro         LIKE ret_det_datamart.tipo_seguro       ,
        desc_seguro         CHAR(40)                                ,
        tipo_pension        LIKE ret_det_datamart.tipo_pension      ,
        desc_pension        CHAR(40)                                ,
        tipo_prestacion     LIKE ret_det_datamart.tipo_prestacion   ,
        desc_prestacion     CHAR(40)                                ,
        estado_sub_viv      LIKE ret_det_datamart.estado_sub_viv    ,
        sec_pension         LIKE ret_det_datamart.sec_pension       ,
        importe_mensual     LIKE pen_solicitud_pmg.importe_mensual  ,
        fecha_ini_pen       LIKE ret_det_datamart.fecha_ini_pen     ,
        fecha_resolucion    LIKE ret_det_datamart.fecha_resolucion  ,
        fecha_solicitud     LIKE pen_solicitud_pmg.fecha_solicitud  ,
        folio_solicitud     LIKE pen_solicitud_pmg.folio_solicitud  ,
        folio_contrato      LIKE pen_solicitud_pmg.folio_contrato   ,
        sec_contrato        LIKE pen_solicitud_pmg.sec_contrato     ,
        folio_lote          LIKE pen_solicitud_pmg.folio_lote       ,
        fecha_captura       LIKE pen_solicitud_pmg.fecha_captura    ,
        fecha_modifica      LIKE pen_solicitud_pmg.fecha_modifica   ,
        fecha_confirma      LIKE pen_solicitud_pmg.fecha_confirma   ,
        fecha_pago          LIKE pen_solicitud_pmg.fecha_pago       ,
        consecutivo         LIKE pen_solicitud_pmg.consecutivo      ,
        usuario_captura     LIKE pen_solicitud_pmg.usuario_captura  ,
        usuario_modifica    LIKE pen_solicitud_pmg.usuario_modifica ,
        usuario_confirma    LIKE pen_solicitud_pmg.usuario_confirma ,
        estado_solicitud    LIKE pen_solicitud_pmg.estado_solicitud ,
        desc_estado         CHAR(40)
    END RECORD

    DEFINE lr_dat RECORD
        id_err      SMALLINT,
        msg         CHAR(100)
    END RECORD

    DEFINE
        arr_c                 ,
        i                     INTEGER

    DEFINE
        ls_pos                ,
        flag                  SMALLINT

    DEFINE
        txt_1                 CHAR(3000) ,
        x_busca               CHAR(1200)

    ----------------------------------------------------------------------------

    CALL f_abre_ventana()

    CASE pc_tipo_oper
        WHEN "C"
            DISPLAY " CONSULTA SOLICITUD " AT 1,59 ATTRIBUTE(REVERSE)

        WHEN "E"
            DISPLAY " ELIMINA SOLICITUD  " AT 1,59 ATTRIBUTE(REVERSE)

        WHEN "M"
            DISPLAY " MODIFICA SOLICITUD " AT 1,59 ATTRIBUTE(REVERSE)
        
        WHEN "D"
            DISPLAY " MODIFICA DATOS BANCARIOS " AT 1,52 ATTRIBUTE(REVERSE)
    END CASE

    DISPLAY " ESC    : Realiza Consulta " AT 1,1

    
    IF gr_param.accion IS NULL OR gr_param.accion = " " THEN
       --ARMA CRITERIO DE BUSQUEDA
       CONSTRUCT BY NAME x_busca ON A.nss              ,
                                    A.curp             ,
                                    A.fecha_solicitud  ,
                                    A.folio_solicitud  ,
                                    A.folio_contrato   ,
                                    A.folio_lote       ,
                                    A.estado_solicitud
       
         ON KEY (CONTROL-C, INTERRUPT)
             IF flag = 0 THEN
                 LET flag = 1
                 EXIT CONSTRUCT
             END IF
       
         ON KEY (ESC)
             LET INT_FLAG = FALSE
             EXIT CONSTRUCT
       
       END CONSTRUCT
    ELSE
       --ESTABLECE CRITERIO DE BUSQUEDA DE ACUERDO A PARAMETROS 
       LET x_busca = "      nss = '", gr_param.nss, "' " ,
                     " AND  consecutivo = ", gr_param.consec, " ",
                     " AND  tipo_retiro = '", gr_param.tipo_ret, "' "
       
       LET  x_busca = x_busca CLIPPED
    END IF 


    IF flag = 1 THEN
        CALL f_error_msg("BUSQUEDA CANCELADA")
        CLEAR SCREEN
        CLOSE WINDOW penm1001
        RETURN
    END IF

    LET txt_1 = "SELECT A.nss       ," ,
                  " ' '                  ,", -- rfc
                  " ' '                  ,", -- curp
                  " ' '                  ,", -- paterno
                  " ' '                  ,", -- materno
                  " ' '                  ,", -- nombres
                  " A.tipo_retiro        ,",
                  " ' '                  ,", -- descripcion
                  " 0                    ,", -- regimen
                  " ' '                  ,", -- tipo_seguro
                  " ' '                  ,", -- desc_seguro
                  " ' '                  ,", -- tipo_pension
                  " ' '                  ,", -- desc_pension
                  " 0                    ,", -- tipo_prestacion
                  " ' '                  ,", -- desc_prestacion
                  " A.estado_sub_viv     ,",
                  " A.sec_pension        ,",
                  " A.importe_mensual    ,",
                  " A.fecha_ini_pen      ,",
                  " A.fecha_resolucion   ,",
                  " A.fecha_solicitud    ,",
                  " A.folio_solicitud    ,",
                  " A.folio_contrato     ,",
                  " A.sec_contrato       ,",
                  " A.folio_lote         ,",
                  " A.fecha_captura      ,",
                  " A.fecha_modifica     ,",
                  " A.fecha_confirma     ,",
                  " A.fecha_pago         ,",
                  " A.consecutivo        ,",
                  " A.usuario_captura    ,",
                  " A.usuario_modifica   ,",
                  " A.usuario_confirma   ,",
                  " A.estado_solicitud   ,",
                  " B.descripcion         ",
               " FROM pen_solicitud_pmg A , pen_estado_pmg B ",
               " WHERE  ",x_busca CLIPPED,
               " AND A.estado_solicitud = B.estado_solicitud "

    IF pc_tipo_oper <> "C" THEN                                            --MLM-1936
       CASE                                                               --MLM-1936
          WHEN pc_tipo_oper = "M"                                         --MLM-1936
               LET txt_1 = txt_1 CLIPPED,                                 --MLM-1936
                           " AND A.estado_solicitud = ",gr_edo.capturado  --MLM-1936     
          WHEN pc_tipo_oper = "D"                                         --MLM-1936     
               LET txt_1 = txt_1 CLIPPED,                                 --MLM-1936
                           " AND A.estado_solicitud = ",gr_edo.en_proceso --MLM-1936
          OTHERWISE                                                       --MLM-1936 
               LET txt_1 = txt_1 CLIPPED,                                 --MLM-1936
                           " AND A.estado_solicitud = ",gr_edo.capturado  --MLM-1936
       END CASE                                                           --MLM-1936
    END IF

    PREPARE prp_con FROM txt_1
    DECLARE cur_con CURSOR FOR prp_con

    LET i = 1

    FOREACH cur_con INTO lar_consulta[i].*

        CALL f_obtiene_nombres(lar_consulta[i].nss)
            RETURNING lr_dat.*,
                      lar_consulta[i].paterno ,
                      lar_consulta[i].materno ,
                      lar_consulta[i].nombres ,
                      lar_consulta[i].curp    ,
                      lar_consulta[i].n_rfc

        CALL f_obtiene_matderecho(lar_consulta[i].tipo_retiro)
            RETURNING lar_consulta[i].regimen         ,
                      lar_consulta[i].tipo_seguro     ,
                      lar_consulta[i].tipo_pension    ,
                      lar_consulta[i].tipo_prestacion ,
                      lar_consulta[i].desc_seguro     ,
                      lar_consulta[i].desc_pension    ,
                      lar_consulta[i].desc_prestacion ,
                      lar_consulta[i].descripcion

        LET i = i + 1
	    IF i >= 5000 THEN
            CALL f_error_msg("FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO")
	        EXIT FOREACH
	    END IF
    END FOREACH

    IF i = 1 THEN
        CLEAR FORM
        CALL f_error_msg("NO EXISTEN REGISTROS")
        CLEAR SCREEN
        CLOSE WINDOW penm1001
        RETURN
    ELSE
        CASE pc_tipo_oper
            WHEN "D"
                DISPLAY "[CTRL-B] Modifica Datos Bancarios" AT 1,1
            WHEN "C"
                DISPLAY "                           " AT 1,1
                DISPLAY "Ctrl-B : Beneficiarios" AT 2,26
        
            WHEN "E"
                DISPLAY " Ctrl-E : Eliminar Solicitud " AT 1,1
        
            WHEN "M"
                DISPLAY " Ctrl-U : Modificar Solicitud " AT 1,1
                DISPLAY "Ctrl-B : Beneficiarios" AT 2,26

        END CASE
    END IF

    CALL SET_COUNT(i-1)
    
    ------------------------------------------------------
    -- MUESTRA CADA UNA DE LAS SOLICITUDES CONSULTADAS 
    ------------------------------------------------------
    DISPLAY ARRAY lar_consulta TO scr_1.*

        ON KEY (CONTROL-B) 
           LET ls_pos = ARR_CURR() 
           IF pc_tipo_oper = "D" THEN   --Datos bancarios
              --CALL fn_modifica_datos_bancarios(lar_consulta[ls_pos].nss,
              --                                 lar_consulta[ls_pos].consecutivo)
              EXIT DISPLAY 
           ELSE
              IF pc_tipo_oper <> "E" THEN
                  LET ls_pos = ARR_CURR()        
                  CALL f_ejecuta_benef(lar_consulta[ls_pos].nss               ,
                                       lar_consulta[ls_pos].consecutivo       ,
                                       pc_tipo_oper                           ,
                                       lar_consulta[ls_pos].folio_solicitud
                                      )
              END IF 
           END IF
        
        ON KEY (CONTROL-E)
            IF pc_tipo_oper = "E" THEN
                LET ls_pos = ARR_CURR()
                CALL f_elimina(lar_consulta[ls_pos].nss          ,
                               lar_consulta[ls_pos].consecutivo  ,
                               lar_consulta[ls_pos].tipo_retiro
                              )
            END IF

            EXIT DISPLAY

        ------------------------
        -- MODIFICAR SOLICITUD
        ------------------------
        ON KEY (CONTROL-U)
           IF pc_tipo_oper = "M" THEN
              
              LET ls_pos = ARR_CURR()
              
              IF lar_consulta[ls_pos].tipo_retiro = "S" THEN
                 WHILE TRUE
                    PROMPT "SOLO PODRÁ MODIFICAR BENEFICIARIOS, CONTINUAR (S/N): " FOR CHAR g_tecla
                    IF g_tecla MATCHES "[SsNn]" THEN
                       EXIT WHILE
                    END IF
                 END WHILE
                 IF g_tecla MATCHES "[sS]" THEN
                    ERROR " ESTA FUNCION A UN NO HA SIDO IMPLEMENTADAD..." 
                 END IF
              ELSE
                 CALL f_procesa_modifica(lar_consulta[ls_pos].*) 
                 EXIT DISPLAY
              END IF

           END IF 

        ON KEY (CONTROL-C, INTERRUPT)
           EXIT DISPLAY

    END DISPLAY
    
    CLOSE WINDOW penm1001

END FUNCTION

#---------------------------------------------------------------------------#
# f_procesa_modifica : Ejecuta los pasos para realizar la modificacion de   #
#                      solicitud de retiros por PMG                         #
#---------------------------------------------------------------------------#
FUNCTION f_procesa_modifica(pr_pantalla)

    DEFINE pr_pantalla RECORD
        nss                 LIKE pen_solicitud_pmg.nss              ,
        n_rfc               LIKE afi_mae_afiliado.n_rfc             ,
        curp                LIKE pen_solicitud_pmg.curp             ,
        paterno             LIKE afi_mae_afiliado.paterno           ,
        materno             LIKE afi_mae_afiliado.materno           ,
        nombres             LIKE afi_mae_afiliado.nombres           ,
        tipo_retiro         LIKE tab_retiro.tipo_retiro             ,
        descripcion         LIKE tab_retiro.descripcion             ,
        regimen             LIKE ret_det_datamart.regimen           ,
        tipo_seguro         LIKE ret_det_datamart.tipo_seguro       ,
        desc_seguro         CHAR(40)                                ,
        tipo_pension        LIKE ret_det_datamart.tipo_pension      ,
        desc_pension        CHAR(40)                                ,
        tipo_prestacion     LIKE ret_det_datamart.tipo_prestacion   ,
        desc_prestacion     CHAR(40)                                ,
        estado_sub_viv      LIKE ret_det_datamart.estado_sub_viv    ,
        sec_pension         LIKE ret_det_datamart.sec_pension       ,
        importe_mensual     LIKE pen_solicitud_pmg.importe_mensual  ,
        fecha_ini_pen       LIKE ret_det_datamart.fecha_ini_pen     ,
        fecha_resolucion    LIKE ret_det_datamart.fecha_resolucion  ,
        fecha_solicitud     LIKE pen_solicitud_pmg.fecha_solicitud  ,
        folio_solicitud     LIKE pen_solicitud_pmg.folio_solicitud  ,
        folio_contrato      LIKE pen_solicitud_pmg.folio_contrato   ,
        sec_contrato        LIKE pen_solicitud_pmg.sec_contrato     ,
        folio_lote          LIKE pen_solicitud_pmg.folio_lote       ,
        fecha_captura       LIKE pen_solicitud_pmg.fecha_captura    ,
        fecha_modifica      LIKE pen_solicitud_pmg.fecha_modifica   ,
        fecha_confirma      LIKE pen_solicitud_pmg.fecha_confirma   ,
        fecha_pago          LIKE pen_solicitud_pmg.fecha_pago       ,
        consecutivo         LIKE pen_solicitud_pmg.consecutivo      ,
        usuario_captura     LIKE pen_solicitud_pmg.usuario_captura  ,
        usuario_modifica    LIKE pen_solicitud_pmg.usuario_modifica ,
        usuario_confirma    LIKE pen_solicitud_pmg.usuario_confirma ,
        estado_solicitud    LIKE pen_solicitud_pmg.estado_solicitud ,
        desc_estado         CHAR(40)
    END RECORD

    DEFINE lr_cambios_sol RECORD
        nss                 LIKE pen_solicitud_pmg.nss              ,
        consecutivo         LIKE pen_solicitud_pmg.consecutivo      ,
        fecha_solicitud     LIKE pen_solicitud_pmg.fecha_solicitud  ,
        folio_solicitud     LIKE pen_solicitud_pmg.folio_solicitud  ,
        folio_contrato      LIKE pen_solicitud_pmg.folio_contrato   ,
        importe_mensual     LIKE pen_solicitud_pmg.importe_mensual
    END RECORD  

    DEFINE lc_nss LIKE pen_solicitud_pmg.nss

    DEFINE
        ls_modifica         SMALLINT

    CALL f_tablas_tmp()
    CALL f_modifica(pr_pantalla.*) RETURNING ls_modifica     ,
                                             lr_cambios_sol.*
    
    IF ls_modifica <> 0 THEN
        CALL f_modifica_solicitud(lr_cambios_sol.*)
    END IF

END FUNCTION


#---------------------------------------------------------------------------#
# f_modifica : Modifica solicitudes de retiros por PMG que se encuentren    #
#              en estado capturado                                          #
#---------------------------------------------------------------------------#
FUNCTION f_modifica(pr_solicitud)

    DEFINE pr_solicitud RECORD
        nss                 LIKE pen_solicitud_pmg.nss              ,
        n_rfc               LIKE afi_mae_afiliado.n_rfc             ,
        curp                LIKE pen_solicitud_pmg.curp             ,
        paterno             LIKE afi_mae_afiliado.paterno           ,
        materno             LIKE afi_mae_afiliado.materno           ,
        nombres             LIKE afi_mae_afiliado.nombres           ,
        tipo_retiro         LIKE tab_retiro.tipo_retiro             ,
        descripcion         LIKE tab_retiro.descripcion             ,
        regimen             LIKE ret_det_datamart.regimen           ,
        tipo_seguro         LIKE ret_det_datamart.tipo_seguro       ,
        desc_seguro         CHAR(40)                                ,
        tipo_pension        LIKE ret_det_datamart.tipo_pension      ,
        desc_pension        CHAR(40)                                ,
        tipo_prestacion     LIKE ret_det_datamart.tipo_prestacion   ,
        desc_prestacion     CHAR(40)                                ,
        estado_sub_viv      LIKE ret_det_datamart.estado_sub_viv    ,
        sec_pension         LIKE ret_det_datamart.sec_pension       ,
        importe_mensual     LIKE pen_solicitud_pmg.importe_mensual  ,
        fecha_ini_pen       LIKE ret_det_datamart.fecha_ini_pen     ,
        fecha_resolucion    LIKE ret_det_datamart.fecha_resolucion  ,
        fecha_solicitud     LIKE pen_solicitud_pmg.fecha_solicitud  ,
        folio_solicitud     LIKE pen_solicitud_pmg.folio_solicitud  ,
        folio_contrato      LIKE pen_solicitud_pmg.folio_contrato   ,
        sec_contrato        LIKE pen_solicitud_pmg.sec_contrato     ,
        folio_lote          LIKE pen_solicitud_pmg.folio_lote       ,
        fecha_captura       LIKE pen_solicitud_pmg.fecha_captura    ,
        fecha_modifica      LIKE pen_solicitud_pmg.fecha_modifica   ,
        fecha_confirma      LIKE pen_solicitud_pmg.fecha_confirma   ,
        fecha_pago          LIKE pen_solicitud_pmg.fecha_pago       ,
        consecutivo         LIKE pen_solicitud_pmg.consecutivo      ,
        usuario_captura     LIKE pen_solicitud_pmg.usuario_captura  ,
        usuario_modifica    LIKE pen_solicitud_pmg.usuario_modifica ,
        usuario_confirma    LIKE pen_solicitud_pmg.usuario_confirma ,
        estado_solicitud    LIKE pen_solicitud_pmg.estado_solicitud ,
        desc_estado         CHAR(40)
    END RECORD

    DEFINE lr_respaldo RECORD
        fecha_solicitud     LIKE pen_solicitud_pmg.fecha_solicitud  ,
        folio_solicitud     LIKE pen_solicitud_pmg.folio_solicitud  ,
        folio_contrato      LIKE pen_solicitud_pmg.folio_contrato   ,
        importe_mensual     LIKE pen_solicitud_pmg.importe_mensual
    END RECORD 

    DEFINE
        lc_cad_msg          CHAR(100)

    DEFINE
        ls_modifica         ,
        ls_procesa          SMALLINT

    #-- ---------------------------------------------------------------------------------------------

    DISPLAY " ESC    : Guardar Cambios          " AT 1,1
    DISPLAY "                      " AT 2,26
    LET ls_modifica = 1
    
    INPUT BY NAME pr_solicitud.fecha_solicitud    ,
                  pr_solicitud.folio_solicitud    ,
                  pr_solicitud.folio_contrato     ,
                  pr_solicitud.importe_mensual    WITHOUT DEFAULTS

        #--
        BEFORE INPUT
            LET lr_respaldo.fecha_solicitud =  pr_solicitud.fecha_solicitud 
            LET lr_respaldo.folio_solicitud =  pr_solicitud.folio_solicitud 
            LET lr_respaldo.folio_contrato  =  pr_solicitud.folio_contrato  
            LET lr_respaldo.importe_mensual =  pr_solicitud.importe_mensual
        
        #--
        AFTER FIELD fecha_solicitud

            CALL f_valida_fechas(pr_solicitud.fecha_solicitud)
                RETURNING ls_procesa, lc_cad_msg

            IF ls_procesa <> 0 THEN
                CALL f_error_msg(lc_cad_msg)
                LET pr_solicitud.fecha_solicitud = lr_respaldo.fecha_solicitud 
                DISPLAY BY NAME pr_solicitud.fecha_solicitud
                NEXT FIELD fecha_solicitud
            END IF

        #--
        AFTER FIELD folio_contrato
            IF (pr_solicitud.folio_contrato IS NULL) OR (pr_solicitud.folio_contrato = 0) THEN
                CALL f_error_msg("FOLIO CONTRATO NO PUEDE SER NULO")
                LET pr_solicitud.folio_contrato  = lr_respaldo.folio_contrato 
                DISPLAY BY NAME pr_solicitud.folio_contrato
                NEXT FIELD folio_contrato
            ELSE
                CALL f_obtiene_sec_contrato(pr_solicitud.nss)
                    RETURNING pr_solicitud.sec_contrato

                DISPLAY BY NAME pr_solicitud.sec_contrato
            END IF

        #--
        AFTER FIELD importe_mensual
            IF (pr_solicitud.importe_mensual IS NULL) OR (pr_solicitud.importe_mensual = 0) THEN
                CALL f_error_msg("IMPORTE MENSUAL NO PUEDE SER NULO")
                LET pr_solicitud.importe_mensual  = lr_respaldo.importe_mensual 
                DISPLAY BY NAME pr_solicitud.importe_mensual
                NEXT FIELD importe_mensual
            END IF

        #--
        ON KEY (ESC)
            -- Valida que no salga sin valores nulos
            CALL f_valida_fechas(pr_solicitud.fecha_solicitud)
                RETURNING ls_procesa, lc_cad_msg

            IF ls_procesa <> 0 THEN
                CALL f_error_msg(lc_cad_msg)
                NEXT FIELD fecha_solicitud
            END IF

            IF (pr_solicitud.folio_contrato IS NULL) OR (pr_solicitud.folio_contrato = 0) THEN
                CALL f_error_msg("FOLIO CONTRATO NO PUEDE SER NULO")
                NEXT FIELD folio_contrato
            END IF

            IF (pr_solicitud.importe_mensual IS NULL) OR (pr_solicitud.importe_mensual = 0) THEN
                CALL f_error_msg("IMPORTE MENSUAL NO PUEDE SER NULO")
                NEXT FIELD importe_mensual
            END IF


            #-- Si no se modifico ningun campo no se realiza ninguna accion
            IF (lr_respaldo.fecha_solicitud = pr_solicitud.fecha_solicitud) AND
               (lr_respaldo.folio_solicitud = pr_solicitud.folio_solicitud) AND
               (lr_respaldo.folio_contrato  = pr_solicitud.folio_contrato ) AND
               (lr_respaldo.importe_mensual = pr_solicitud.importe_mensual) THEN
                
                LET ls_modifica = 0
                EXIT INPUT
            ELSE
                WHILE TRUE
                    PROMPT "¿ MODIFICAR SOLICITUD ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            LET ls_modifica = 1
                            EXIT INPUT
                        ELSE
                            CALL f_error_msg("PROCESO CANCELADO")
                            LET ls_modifica = 0
                            EXIT INPUT
                        END IF
                    END IF
                END WHILE                
            END IF

        #--
        ON KEY (INTERRUPT, CONTROL-C)
            CALL f_error_msg("PROCESO CANCELADO")
            LET ls_modifica = 0
            EXIT INPUT
    END INPUT

    RETURN ls_modifica                  ,
           pr_solicitud.nss             ,
           pr_solicitud.consecutivo     ,
           pr_solicitud.fecha_solicitud ,
           pr_solicitud.folio_solicitud ,
           pr_solicitud.folio_contrato  ,
           pr_solicitud.importe_mensual
           
END FUNCTION

#---------------------------------------------------------------------------#
# f_elimina : Elimina y desmarca solicitudes de retiros por PMG que se      #
#             encuentren en estado capturado                                #
#---------------------------------------------------------------------------#
FUNCTION f_elimina(pr_elimina)

    DEFINE pr_elimina RECORD
        nss             LIKE pen_solicitud_pmg.nss          ,
        consecutivo     LIKE pen_solicitud_pmg.consecutivo  ,
        tipo_retiro     LIKE pen_solicitud_pmg.tipo_retiro
    END RECORD

    DISPLAY "                      " AT 2,26

    PROMPT "¿ DESEA ELIMINAR ESTA SOLICITUD ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
    IF enter MATCHES "[sSnN]" THEN
        IF enter MATCHES "[sS]" THEN

            CALL f_desmarca_cuenta(pr_elimina.*)

            DELETE
            FROM   pen_solicitud_pmg
            WHERE  nss          = pr_elimina.nss
            AND    consecutivo  = pr_elimina.consecutivo

            DELETE
            FROM   pen_ctr_pago
            WHERE  nss          = pr_elimina.nss
            AND    consecutivo  = pr_elimina.consecutivo

            DELETE
            FROM   pen_ctr_pago_det
            WHERE  nss          = pr_elimina.nss
            AND    consecutivo  = pr_elimina.consecutivo

            DELETE
            FROM   ret_beneficiario
            WHERE  nss          = pr_elimina.nss
            AND    consecutivo  = pr_elimina.consecutivo

            CALL f_error_msg("¡SOLICITUD ELIMINADA CORRECTAMENTE!")
        ELSE
            CALL f_error_msg("PROCESO CANCELADO")
        END IF
    END IF

END FUNCTION


#-- --------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana general del programa con las etiquetas   #
#                  de cada division                                         #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW penm1001 AT 2,2 WITH FORM "PENM1001" ATTRIBUTE (BORDER)

    DISPLAY " Ctrl-C : Salir " AT 2,1
    DISPLAY "v1.5" AT 2,74
    DISPLAY " PENM100                      DATOS DEL AFILIADO                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "             DATOS DE LA SOLICITUD DE PENSION MINIMA GARANTIZADA              " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           DATOS DE CONTROL INTERNO                           " AT 16,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY"  AT 3,68 ATTRIBUTE(REVERSE)
    

END FUNCTION

#---------------------------------------------------------------------------#
# f_guarda_solicitud : Guarda las tablas temporales en las tablas fisicas y #
#                      realiza el marcaje de la cuenta                      #
#---------------------------------------------------------------------------#
FUNCTION f_guarda_solicitud(pc_nss)

    DEFINE pc_nss LIKE pen_solicitud_pmg.nss

    DEFINE ld_ult_consec LIKE pen_solicitud_pmg.consecutivo
    
    DEFINE lc_folio_sol LIKE pen_solicitud_pmg.folio_solicitud

    DEFINE lr_marca RECORD
        marca_cod           SMALLINT,
        desc_rechazo        CHAR(50)
    END RECORD

    DEFINE
        li_cont             INTEGER

    -- ------------------------------------------------------------------------------

    -- Obtenemos el consecutivo para la solicitud y actualizamos las tablas temporales
    LET ld_ult_consec   = f_obtiene_ult_consec()

    UPDATE tmp_solicitud_pmg
    SET    consecutivo = ld_ult_consec
    WHERE  nss = pc_nss

    UPDATE tmp_ctr_pago
    SET    consecutivo = ld_ult_consec
    WHERE  nss = pc_nss

    UPDATE tmp_ctr_pago_det
    SET    consecutivo = ld_ult_consec
    WHERE  nss = pc_nss

    CALL f_marca_cuenta(pc_nss, ld_ult_consec) RETURNING lr_marca.*

    -- Si no ocurrieron errores marcamos la cuenta
    IF lr_marca.marca_cod = 0 THEN

        SELECT folio_solicitud
        INTO   lc_folio_sol
        FROM   tmp_solicitud_pmg
        WHERE  nss          = pc_nss
        AND    consecutivo  = ld_ult_consec

        -- Guardamos la solicitud
        INSERT INTO pen_solicitud_pmg
        SELECT *
        FROM   tmp_solicitud_pmg
        WHERE  nss          = pc_nss
        AND    consecutivo  = ld_ult_consec

        LET li_cont = SQLCA.SQLERRD[3]

        -- Si no ocurrieron errores guardamos las tablas de calculos
        IF li_cont > 0 THEN
            INSERT INTO pen_ctr_pago
            SELECT *
            FROM   tmp_ctr_pago
            WHERE  nss = pc_nss
            AND    consecutivo  = ld_ult_consec

            INSERT INTO pen_ctr_pago_det
            SELECT *
            FROM   tmp_ctr_pago_det
            WHERE  nss = pc_nss
            AND    consecutivo  = ld_ult_consec

            CALL f_ejecuta_benef(pc_nss, ld_ult_consec, "A", lc_folio_sol)
            
            CALL f_error_msg("¡SOLICITUD GUARDADA CORRECTAMENTE!")
        ELSE
            CALL f_error_msg("ERROR - NO PUDO CAPTURARSE LA SOLICITUD")
        END IF
    ELSE
        LET lr_marca.desc_rechazo = "ERROR NSS MARCADO, MOV-",
                                    lr_marca.marca_cod USING "<<<", " ",
                                    lr_marca.desc_rechazo CLIPPED
        CALL f_error_msg(lr_marca.desc_rechazo)
    END IF

END FUNCTION


#---------------------------------------------------------------------------#
# f_marca_cuenta : Ejecuta el script para realizar la marca de la cuenta    #
#---------------------------------------------------------------------------#
FUNCTION f_marca_cuenta(pr_marca)

    DEFINE pr_marca RECORD
        nss         LIKE pen_solicitud_pmg.nss          ,
        consec      LIKE pen_solicitud_pmg.consecutivo
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        cod_rechazo     SMALLINT ,
        marca_causa     SMALLINT ,
        fec_causa       DATE
    END RECORD

    DEFINE
        ls_marca_res        ,
        ls_cod_rech         ,
        ls_movim            SMALLINT

    DEFINE
        lc_desc_rechazo     CHAR(50)

    -- ---------------------------------------------------------------------------------

    LET lc_desc_rechazo     = " "
    LET lr_dat.edo_marca    = 0
    LET lr_dat.cod_rechazo  = 0
    LET lr_dat.marca_causa  = 0
    INITIALIZE lr_dat.fec_causa TO NULL

    SELECT A.movimiento
    INTO   ls_movim
    FROM   tab_retiro A ,
           tmp_solicitud_pmg B
    WHERE  A.tipo_retiro = B.tipo_retiro
    AND    B.nss         = pr_marca.nss

    --MARCAJE--
    EXECUTE eje_marca USING pr_marca.nss            ,--nss
                            ls_movim                ,--marca entrante
                            pr_marca.consec         ,--consecutivo
                            lr_dat.edo_marca        ,--estado_marco
                            lr_dat.cod_rechazo      ,--codigo de rechazo
                            lr_dat.marca_causa      ,--marca_causa
                            lr_dat.fec_causa        ,--fecha_causa
                            gc_usuario               --usuario
                      INTO  ls_marca_res   ,
                            ls_cod_rech

    -- Si existe un error en la marca, regresamos el codigo y el mensaje de error
    IF ls_cod_rech <> 0 THEN
        SELECT rechazo_desc
        INTO   lc_desc_rechazo
        FROM   tab_rch_marca
        WHERE  rechazo_cod = ls_cod_rech
    END IF

    RETURN ls_cod_rech, lc_desc_rechazo

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta cuando esta se elimine                         #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss         LIKE pen_solicitud_pmg.nss          ,
        consec      LIKE pen_solicitud_pmg.consecutivo  ,
        tipo_retiro LIKE pen_solicitud_pmg.tipo_retiro
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE
        ls_movim            SMALLINT

    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    SELECT movimiento
    INTO   ls_movim
    FROM   tab_retiro
    WHERE  tipo_retiro = pr_desmarca.tipo_retiro

    --DESMARCA DE LA CUENTA --
    EXECUTE eje_desmarca USING pr_desmarca.nss      ,--nss
                               ls_movim             ,--marca entrante
                               pr_desmarca.consec   ,--consecutivo
                               lr_dat.edo_marca     ,--estado_marco
                               lr_dat.marca_causa   ,--marca_causa
                               gc_usuario            --usuario

END FUNCTION


#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_solicitud_pmg
        DROP TABLE tmp_ctr_pago
        DROP TABLE tmp_ctr_pago_det
    WHENEVER ERROR STOP

    SELECT *
    FROM   pen_solicitud_pmg
    WHERE  1 = 0
    INTO TEMP tmp_solicitud_pmg

    SELECT *
    FROM   pen_ctr_pago
    WHERE  1 = 0
    INTO TEMP tmp_ctr_pago

    SELECT *
    FROM   pen_ctr_pago_det
    WHERE  1 = 0
    INTO TEMP tmp_ctr_pago_det


END FUNCTION

#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(75)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_ejecuta_benef : Ejecuta el programa RETM810 para la captura de          #
#                   beneficiarios del nss actual                            #
#---------------------------------------------------------------------------#
FUNCTION f_ejecuta_benef(pr_benef)

    DEFINE pr_benef RECORD
        nss             LIKE pen_solicitud_pmg.nss              ,
        consec          LIKE pen_solicitud_pmg.consecutivo      ,
        id_oper         CHAR                                    ,
        folio_sol       LIKE pen_solicitud_pmg.folio_solicitud
    END RECORD

    DEFINE
        lc_ejecuta             CHAR(0200)

    -- Cambio para Metlife. Se modifica el programa de captura de beneficiarios
    -- para insertar los datos de pago para Cash Management 
    
    LET lc_ejecuta = "fglgo RETM810   ", pr_benef.nss       CLIPPED ," ",
                                         pr_benef.consec    CLIPPED ," ",
                                         pr_benef.id_oper   ," ",
                                         pr_benef.folio_sol CLIPPED 
    RUN lc_ejecuta


END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_sol_previa : Valida que el nss tenga o no una solicitud de       #
#                       retiro por pmg previa. En caso de tenerla y no      #
#                       estar en estado liquidado o rechazado, envia un     #
#                       mensaje de error                                    #
#---------------------------------------------------------------------------#
FUNCTION f_valida_sol_previa(pc_nss)

    DEFINE pc_nss LIKE pen_solicitud_pmg.nss

    DEFINE
        ls_estado           SMALLINT

    DEFINE
        lc_mensaje          CHAR(100)
        
    DEFINE
        ld_consecutivo      DECIMAL(11),
        ls_mensualidad      SMALLINT
        
    DEFINE ls_estado_aux    SMALLINT

    -- ----------------------------------------------------------------

    LET ls_estado   = 0
    LET lc_mensaje  = " "

    {SELECT "OK"
    FROM   pen_solicitud_pmg
    WHERE  nss               =  pc_nss
    AND    estado_solicitud NOT IN (gr_edo.liquidado,
                                    gr_edo.rechazado)
    GROUP BY 1}
    
    SELECT consecutivo
    INTO   ld_consecutivo
    FROM   pen_solicitud_pmg
    WHERE  nss =  pc_nss
    AND    estado_solicitud NOT IN (gr_edo.liquidado,
                                    gr_edo.rechazado)
    IF STATUS <> NOTFOUND THEN
       --ENCONTRO EN PROCESO DE PAGO
       
       --RECUPERA LA ULTIMA MENSUALIDAD DE LA SOLICITUD IDENTIFICADA
       SELECT MAX(num_mensualidad) 
       INTO   ls_mensualidad
       FROM   pen_ctr_pago_det
       WHERE  nss         = pc_nss
       AND    consecutivo = ld_consecutivo
     --AND    estado      <> gr_edo.rechazado
      
       --RECUPERA EL ESTADO DE LA ULTIMA MENSUALIDAD
       SELECT estado
       INTO   ls_estado_aux
       FROM   pen_ctr_pago_det
       WHERE  nss             = pc_nss
       AND    consecutivo     = ld_consecutivo
       AND    num_mensualidad = ls_mensualidad
     --AND    estado <> gr_edo.rechazado
       
       IF ls_estado_aux != gr_edo.rechazado THEN --STATUS <> NOTFOUND THEN  
         LET ls_estado   = 1
         LET lc_mensaje  = "YA EXISTE UNA SOLICITUD PREVIA PARA ESTE NSS"
       END IF
    END IF

    RETURN ls_estado ,
           lc_mensaje

END FUNCTION


#---------------------------------------------------------------------------#
# f_valida_fechas : Valida que la fecha capturada no sea nula y que cumpla  #
#                   las condiciones necesarias para ser aceptada            #
#---------------------------------------------------------------------------#
FUNCTION f_valida_fechas(pdt_fecha)

    DEFINE
        pdt_fecha           DATE

    DEFINE
        ls_estado           SMALLINT

    DEFINE
        lc_mensaje          CHAR(100)

    -- ---------------------------------------------------------------------

    LET ls_estado = 0

    IF pdt_fecha IS NULL THEN
        LET lc_mensaje = " LA FECHA NO DEBE SER NULA"
        LET ls_estado = 1
    ELSE
        IF pdt_fecha < "01/01/1900" THEN
            LET lc_mensaje = " FECHA INVALIDA"
            LET ls_estado = 1
        ELSE
            IF pdt_fecha > HOY THEN
                LET lc_mensaje = " LA FECHA NO DEBE SER MAYOR A LA FECHA DEL DIA"
                LET ls_estado = 1
            END IF
        END IF
    END IF

    RETURN ls_estado, lc_mensaje

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_suficiencia : Ejecuta las rutinas de verificacion de suficiencia #
#                        de saldos y llena sus tablas temporales            #
#---------------------------------------------------------------------------#
FUNCTION f_valida_suficiencia(pr_datos)

    DEFINE pr_datos RECORD
        nss                 LIKE ret_det_datamart.nss               ,
        curp                LIKE ret_det_datamart.curp              ,        
        sec_pension         LIKE ret_det_datamart.sec_pension       ,
        sec_contrato        LIKE pen_solicitud_pmg.sec_contrato     ,
        fecha_ini_pen       LIKE ret_det_datamart.fecha_ini_pen     ,
        importe_mensual     LIKE pen_solicitud_pmg.importe_mensual
    END RECORD

    DEFINE lr_pagos     RECORD LIKE pen_ctr_pago.*
    DEFINE lr_det_pagos RECORD LIKE pen_ctr_pago_det.*

    DEFINE lr_datos_val RECORD
        retroactivo         DECIMAL(16,6)   ,
        devengado           DECIMAL(16,6)   ,
        saldo_dia           DECIMAL(16,6)   ,
        id_suficiencia      SMALLINT
    END RECORD

    DEFINE
        ls_dias_hab         ,
        ls_val_mto          ,
        ls_cont_841         SMALLINT,
        ls_estado           SMALLINT

    DEFINE
        ld_primer_pago      ,
        ld_mto_pago         DECIMAL(16,6)

    DEFINE
        lc_mensaje          CHAR(100)

    -- ---------------------------------------------------------------------

    LET ld_primer_pago  = 0
    LET ls_estado       = 0
    LET ls_dias_hab     = 2
    LET lc_mensaje      = " "

    LET lr_pagos.nss                    = pr_datos.nss
    LET lr_pagos.consecutivo            = 1
    LET lr_pagos.folio_lote             = 0
    LET lr_pagos.estado                 = gr_edo.capturado

    LET lr_det_pagos.nss                = pr_datos.nss
    LET lr_det_pagos.consecutivo        = 1
    LET lr_det_pagos.sec_contrato       = pr_datos.sec_contrato
    LET lr_det_pagos.folio_op78         = 0
    LET lr_det_pagos.folio_liquida      = 0
    LET lr_det_pagos.fecha_recibe_op79  = NULL
    LET lr_det_pagos.fecha_liquida      = NULL
    LET lr_det_pagos.motivo_cancela     = NULL
    LET lr_det_pagos.estado             = gr_edo.capturado
    
    EXECUTE eje_valida_mto USING pr_datos.importe_mensual
                           INTO  ld_mto_pago    ,
                                 ls_val_mto

    IF ls_val_mto >= 0 THEN
        
        EXECUTE eje_valida_pmg USING pr_datos.nss           ,
                                     pr_datos.sec_pension   ,
                                     HOY                    ,
                                     ls_val_mto
                               INTO  lr_datos_val.* 
        
        IF lr_datos_val.id_suficiencia = 1 THEN
            
            EXECUTE eje_habil_sig USING HOY         ,
                                        ls_dias_hab
                                  INTO  lr_det_pagos.fecha_pago_estimada
        
            
            LET lr_det_pagos.num_mensualidad    = 1
            
            --VERIFICA LA EXISTENCIA DE MOVIMIENTOS DE LIQUIDACION DE PMG
            SELECT COUNT(*)             --CPL-1574
            INTO ls_cont_841
            FROM dis_cuenta
            WHERE nss = pr_datos.nss
            
            IF ls_cont_841 > 0 THEN   --CPL-1574
               --SI YA TIENE LIQUIDACIONES ASIGNA SOLO MENSUALIDAD
               LET ld_primer_pago = ld_mto_pago
            ELSE
               --ASIGNA MENSUALIDAD Y RETROACTIVO
               LET ld_primer_pago = ld_mto_pago + lr_datos_val.retroactivo + lr_datos_val.devengado
             END IF 

            -- Si el nss no tiene saldo suficiente para pagar todo el primer pago, se paga
            -- lo que tenga en la cuenta
            IF lr_datos_val.saldo_dia > ld_primer_pago THEN
                LET lr_det_pagos.mto_pago_pesos = ld_primer_pago
                
                IF (lr_datos_val.saldo_dia - ld_primer_pago) > ld_mto_pago THEN --MLM-1936
                   LET lr_pagos.num_mensualidades  = (lr_datos_val.saldo_dia - ld_primer_pago)/ld_mto_pago
                ELSE                                                 --MLM-1936
                   LET lr_pagos.num_mensualidades = 1                --MLM-1936
                END IF
                
            ELSE
                LET lr_det_pagos.mto_pago_pesos = lr_datos_val.saldo_dia
                LET lr_pagos.num_mensualidades  = 1
            END IF
        
            LET lr_pagos.pago_mensual_pesos     = ld_mto_pago
            
            IF ls_cont_841 >0 THEN --CPL-1574
               LET lr_pagos.pago_retroactivo_pesos = 0
               LET lr_pagos.devengadas_pesos       = 0
            ELSE
            
               LET lr_pagos.pago_retroactivo_pesos = lr_datos_val.retroactivo
               LET lr_pagos.devengadas_pesos       = lr_datos_val.devengado
            END IF
        ELSE
            LET ls_estado   = 1
            LET lc_mensaje  = " EL NSS NO CUBRE LA PENSION MINIMA GARANTIZADA"
        
            CALL f_inserta_insuficiencia(pr_datos.nss               ,
                                         pr_datos.curp              ,
                                         pr_datos.sec_pension       ,
                                         0                          , -- num mensualidades
                                         pr_datos.fecha_ini_pen
                                        )
        END IF -- Valida suficiencia
    ELSE
        LET ls_estado   = 1
        LET lc_mensaje  = " EL MONTO CAPTURADO NO COINCIDE CON EL VALOR HISTORICO"        
    END IF -- Valida monto capturado

    IF ls_estado = 0 THEN
        INSERT INTO tmp_ctr_pago
        VALUES (lr_pagos.*)

        INSERT INTO tmp_ctr_pago_det
        VALUES (lr_det_pagos.*)
    END IF

    RETURN ls_estado, lc_mensaje

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_tab_insuficiencia : Valida que el nss capturado no haya sido     #
#                              marcado por insuficiencia de saldo en la     #
#                              carga de Datamart o en una captura previa    #
#---------------------------------------------------------------------------#
FUNCTION f_valida_tab_insuficiencia(pr_dat_dtm)

    DEFINE pr_dat_dtm RECORD
        nss            LIKE ret_det_datamart.nss         ,
        sec_pension    LIKE ret_det_datamart.sec_pension ,
        folio          LIKE ret_det_datamart.folio
    END RECORD

    DEFINE
        lc_cadena           CHAR(100)

    DEFINE
        ls_ins_dtm         SMALLINT

    --  ------------------------------------------------------------------------

    LET ls_ins_dtm  = 0
    LET lc_cadena   = " "

    -- Verifica insuficiencia por Datamart
    SELECT "OK"
    FROM   pen_detalle_op70
    WHERE  nss                  = pr_dat_dtm.nss
    AND    sec_pension          = pr_dat_dtm.sec_pension
    AND    folio_datamart       = pr_dat_dtm.folio
    AND    origen_informacion   = 1
    AND    id_tipo_notifica     = 600
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
        LET ls_ins_dtm  = 1
        LET lc_cadena   = "SOLICITUD RECHAZADA POR INSUFICIENCIA EN DATAMART"
    ELSE
        -- Verifica insuficiencia por Captura
        SELECT "OK"
        FROM   pen_detalle_op70
        WHERE  nss                  = pr_dat_dtm.nss
        AND    sec_pension          = pr_dat_dtm.sec_pension
        AND    folio_datamart       = 0
        AND    origen_informacion   = 2
        AND    id_tipo_notifica     = 600
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            LET ls_ins_dtm  = 1
            LET lc_cadena   = "SOLICITUD RECHAZADA POR INSUFICIENCIA EN CAPTURA"
        END IF
    END IF

    RETURN ls_ins_dtm  ,
           lc_cadena

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_precio_acc : Valida que existan los precios de accion para el    #
#                       dia en curso                                        #
#---------------------------------------------------------------------------#
FUNCTION f_valida_precio_acc(pdt_fecha_precios)

    DEFINE
        pdt_fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE
        ls_valida               ,
        ls_sie                  SMALLINT

    LET ls_sie      = 1
    LET ls_valida   = 1


    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET lc_siefore = lr_precio_acc.siefore

            LET lc_mensaje = " FALTA PRECIO DE ACCION: ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                             " SIEFORE ", lc_siefore CLIPPED

            CALL f_error_msg(lc_mensaje)
            LET ls_valida = 0
            EXIT FOREACH
        ELSE
            LET ls_sie = lr_precio_acc.siefore
            LET gar_precio_acc[ls_sie].* = lr_precio_acc.*
        END IF

    END FOREACH

    RETURN ls_valida

END FUNCTION



#-- --------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------#
# f_obtiene_nombres : Determina si el nss dado esta afiliado en la afore.   #
#                     En caso de estarlo se regresan los nombres del        #
#                     afiliado, y en caso contrario regresa un mensaje de   #
#                     error                                                 #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_nombres(pc_nss)

    DEFINE pc_nss LIKE pen_solicitud_pmg.nss

    DEFINE lr_afi RECORD
        ap_paterno      LIKE afi_mae_afiliado.paterno   ,
        ap_materno      LIKE afi_mae_afiliado.materno   ,
        nombres         LIKE afi_mae_afiliado.nombres   ,
        curp            LIKE afi_mae_afiliado.n_unico   ,
        rfc             LIKE afi_mae_afiliado.n_rfc
    END RECORD

    DEFINE
        lc_cadena           CHAR(100)

    DEFINE
        ls_afiliado         SMALLINT

    --  ------------------------------------------------------------------------

    LET ls_afiliado = 0
    LET lc_cadena   = " "

    SELECT paterno  ,
           materno  ,
           nombres  ,
           n_unico  ,
           n_rfc
    INTO   lr_afi.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss

    IF STATUS = NOTFOUND THEN

        LET ls_afiliado = 1

        SELECT "OK"
        FROM   afi_solicitud
        WHERE  n_seguro = pc_nss
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            LET lc_cadena   = "TRABAJADOR NO AFILIADO A LA AFORE"
        ELSE
            LET lc_cadena   = "TRABAJADOR SE ENCUENTRA COMO PREAFILIADO"
        END IF -- No encontrado en afi_solicitud

    END IF -- No encontrado en afi_mae_afiliadp

    RETURN ls_afiliado  ,
           lc_cadena    ,
           lr_afi.*

END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_matderecho : Obtiene la combinacion de la matriz de derechos y  #
#                        sus descripciones usados en la captura             #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_matderecho(pc_tipo_retiro)

    DEFINE pc_tipo_retiro LIKE ret_matriz_derecho.tipo_retiro

    DEFINE lr_mat_derecho RECORD
        regimen             LIKE ret_matriz_derecho.regimen         ,
        tipo_seguro         LIKE ret_matriz_derecho.tipo_seguro     ,
        tipo_pension        LIKE ret_matriz_derecho.tipo_pension    ,
        tipo_prestacion     LIKE ret_matriz_derecho.tipo_prestacion
    END RECORD

    DEFINE lr_desc RECORD
        desc_seguro         CHAR(40)    ,
        desc_pension        CHAR(40)    ,
        desc_prestacion     CHAR(40)    ,
        desc_tipo_retiro    CHAR(40)
    END RECORD

    SELECT regimen          ,
           tipo_seguro      ,
           tipo_pension     ,
           tipo_prestacion
    INTO   lr_mat_derecho.*
    FROM   ret_matriz_derecho
    WHERE  tipo_retiro = pc_tipo_retiro

    SELECT descripcion
    INTO   lr_desc.desc_seguro
    FROM   tab_seguro
    WHERE  clave = lr_mat_derecho.tipo_seguro

    SELECT descripcion
    INTO   lr_desc.desc_pension
    FROM   tab_pension
    WHERE  tipo_pension = lr_mat_derecho.tipo_pension

    SELECT descripcion
    INTO   lr_desc.desc_prestacion
    FROM   tab_prestacion
    WHERE  tipo_prestacion = lr_mat_derecho.tipo_prestacion

    SELECT descripcion
    INTO   lr_desc.desc_tipo_retiro
    FROM   tab_retiro
    WHERE  tipo_retiro = pc_tipo_retiro

    RETURN lr_mat_derecho.* ,
           lr_desc.*

END FUNCTION

#==============================================================================#
# f_obtiene_datamart : Determina si el nss tiene una solicitud en datamart     #
#                      En caso de tenerla se regresan los campos necesarios    #
#                      para llevar la tabla de solicitudes, en caso            #
#                      contrario se regresa un mensaje de error                #
#==============================================================================#
FUNCTION f_obtiene_datamart(pr_matriz)

    DEFINE pr_matriz RECORD
        nss                        LIKE pen_solicitud_pmg.nss              ,
        regimen                    LIKE ret_det_datamart.regimen           ,
        tipo_seguro                LIKE ret_det_datamart.tipo_seguro       ,
        tipo_pension               LIKE ret_det_datamart.tipo_pension      ,
        tipo_prestacion            LIKE ret_det_datamart.tipo_prestacion
        END RECORD

    DEFINE li_max_folio            LIKE ret_det_datamart.folio
    DEFINE lc_max_sec              LIKE ret_det_datamart.sec_pension

    DEFINE lr_dtm RECORD
        fecha_ini_pen              LIKE ret_det_datamart.fecha_ini_pen     ,
        fec_ini_pago               LIKE ret_det_datamart.fec_ini_pago      ,
        estado_sub_viv             LIKE ret_det_datamart.estado_sub_viv    ,
        sec_pension                LIKE ret_det_datamart.sec_pension       ,
        fecha_resolucion           LIKE ret_det_datamart.fecha_resolucion
        END RECORD

    DEFINE lc_cadena               CHAR(100)
    DEFINE ls_datamart             SMALLINT
    DEFINE ld_max_fec_carga_dat    DATE

    --  ------------------------------------------------------------------------

    LET ls_datamart = 0
    LET lc_cadena   = " "

    -- MLM-1979 SE CAMBIA MECANISMO DE BUSQUEDA DE RESOLUCION POR LA 
    -- COMPATIBILIDAD ENTRE RESOLICIONES VIEJAS Y NUEVAS
    
    SELECT MAX(folio)
    INTO   li_max_folio
    FROM   ret_det_datamart
    WHERE  nss              = pr_matriz.nss
    AND    regimen          = pr_matriz.regimen
    AND    tipo_seguro      = pr_matriz.tipo_seguro
    AND    tipo_pension     = pr_matriz.tipo_pension
    AND    tipo_prestacion  = pr_matriz.tipo_prestacion
    AND    diag_datamart    IN (101,300,301,501)
    
    IF li_max_folio IS NULL THEN
       --BUSCA LA RESOLUCION DE LA FORMA ANTERIOR
       SELECT MAX(sec_pension)
       INTO   lc_max_sec
       FROM   ret_det_datamart
       WHERE  nss              = pr_matriz.nss
       AND    regimen          = pr_matriz.regimen
       AND    tipo_seguro      = pr_matriz.tipo_seguro
       AND    tipo_pension     = pr_matriz.tipo_pension
       AND    tipo_prestacion  = pr_matriz.tipo_prestacion
       AND    diag_datamart    IN (101,300,301,501)
       
       IF lc_max_sec IS NULL THEN
          LET ls_datamart = 1
          LET lc_cadena   = "EL NSS NO SE ENCUENTRA EN DATAMART"
       ELSE
          WHENEVER ERROR CONTINUE
          
             --RECUPERA LA FECHA MAS RECIENTE DE LA FECHA DE CARGA EN DATAMART
             SELECT MAX(fec_carga_datamart)
             INTO   ld_max_fec_carga_dat
             FROM   ret_det_datamart
             WHERE  nss              = pr_matriz.nss
             AND    regimen          = pr_matriz.regimen
             AND    tipo_seguro      = pr_matriz.tipo_seguro
             AND    tipo_pension     = pr_matriz.tipo_pension
             AND    tipo_prestacion  = pr_matriz.tipo_prestacion
             AND    diag_datamart    IN (101,300,301,501)
             AND    sec_pension      = lc_max_sec
             
             --RECUPERA LOS DATOS DE LA RESOLUCION 
             SELECT UNIQUE fecha_ini_pen    ,
                    fec_ini_pago     ,
                    estado_sub_viv   ,
                    sec_pension      ,
                    fecha_resolucion
             INTO   lr_dtm.*
             FROM   ret_det_datamart
             WHERE  nss              = pr_matriz.nss
             AND    regimen          = pr_matriz.regimen
             AND    tipo_seguro      = pr_matriz.tipo_seguro
             AND    tipo_pension     = pr_matriz.tipo_pension
             AND    tipo_prestacion  = pr_matriz.tipo_prestacion
             AND    diag_datamart    IN (101,300,301,501)
             AND    sec_pension      = lc_max_sec
             AND    fec_carga_datamart = ld_max_fec_carga_dat
             
            IF SQLCA.SQLCODE < 0 THEN
               LET g_mensaje = "SE ENCONTRARON RESOLUCIONES DUPLICADAS, INFORME A SISTEMAS, PRESIONE <ENTER>:"
               PROMPT g_mensaje FOR CHAR g_tecla
               LET g_mensaje = "RESOLUCION DUPLICADA NSS", pr_matriz.nss," SEC_PENSION: ",  lc_max_sec USING "<<<<<<<<<&"
               CALL ERRORLOG(g_mensaje)
               EXIT PROGRAM
            END IF
          
          WHENEVER ERROR STOP
       END IF
    ELSE
        --SI ENCONTRO POR RESOLUCIONES NUEVAS
        SELECT MAX(sec_pension)
        INTO   lc_max_sec
        FROM   ret_det_datamart
        WHERE  nss              = pr_matriz.nss
        AND    regimen          = pr_matriz.regimen
        AND    tipo_seguro      = pr_matriz.tipo_seguro
        AND    tipo_pension     = pr_matriz.tipo_pension
        AND    tipo_prestacion  = pr_matriz.tipo_prestacion
        AND    diag_datamart    IN (101,300,301,501)
        AND    folio            = li_max_folio

        SELECT fecha_ini_pen    ,
               fec_ini_pago     ,
               estado_sub_viv   ,
               sec_pension      ,
               fecha_resolucion
        INTO   lr_dtm.*
        FROM   ret_det_datamart
        WHERE  nss              = pr_matriz.nss
        AND    regimen          = pr_matriz.regimen
        AND    tipo_seguro      = pr_matriz.tipo_seguro
        AND    tipo_pension     = pr_matriz.tipo_pension
        AND    tipo_prestacion  = pr_matriz.tipo_prestacion
        AND    diag_datamart    IN (101,300,301,501)
        AND    folio            = li_max_folio
        AND    sec_pension      = lc_max_sec
    END IF

    RETURN ls_datamart ,
           lc_cadena   ,
           lr_dtm.*    ,
           li_max_folio

END FUNCTION

#-----------------------------xo----------------------------------------------#
# f_obtiene_sec_contrato : Obtiene la secuencia de contrato correspondiente.#
#                          Si es la primer solicitud del nss la secuencia   #
#                          es 1 y en otro caso es la maxima secuencia       #
#                          existente mas uno                                #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_sec_contrato(pc_nss)

    DEFINE pc_nss LIKE pen_solicitud_pmg.nss

    DEFINE
        ls_sec_contrato         SMALLINT


    SELECT NVL(MAX(sec_contrato),0)
    INTO   ls_sec_contrato
    FROM   pen_solicitud_pmg
    WHERE  nss              = pc_nss
    AND    estado_solicitud IN (gr_edo.liquidado,gr_edo.rechazado)

    LET ls_sec_contrato = ls_sec_contrato + 1

    RETURN ls_sec_contrato

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_ult_consec : Obtiene el ultimo consecutivo a insertarse         #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_ult_consec()

    DEFINE li_consecutivo LIKE pen_solicitud_pmg.consecutivo

    EXECUTE eje_consecutivo INTO li_consecutivo
    RETURN li_consecutivo

END FUNCTION


#---------------------------------------------------------------------------#
# f_inserta_sol_tmp : Almacena los datos capturados en la tabla temporal de #
#                     solicitudes                                           #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_sol_tmp(pr_pantalla)

    DEFINE pr_pantalla RECORD
        nss                 LIKE pen_solicitud_pmg.nss              ,
        n_rfc               LIKE afi_mae_afiliado.n_rfc             ,
        curp                LIKE pen_solicitud_pmg.curp             ,
        paterno             LIKE afi_mae_afiliado.paterno           ,
        materno             LIKE afi_mae_afiliado.materno           ,
        nombres             LIKE afi_mae_afiliado.nombres           ,
        tipo_retiro         LIKE tab_retiro.tipo_retiro             ,
        descripcion         LIKE tab_retiro.descripcion             ,
        regimen             LIKE ret_det_datamart.regimen           ,
        tipo_seguro         LIKE ret_det_datamart.tipo_seguro       ,
        desc_seguro         CHAR(40)                                ,
        tipo_pension        LIKE ret_det_datamart.tipo_pension      ,
        desc_pension        CHAR(40)                                ,
        tipo_prestacion     LIKE ret_det_datamart.tipo_prestacion   ,
        desc_prestacion     CHAR(40)                                ,
        estado_sub_viv      LIKE ret_det_datamart.estado_sub_viv    ,
        sec_pension         LIKE ret_det_datamart.sec_pension       ,
        importe_mensual     LIKE pen_solicitud_pmg.importe_mensual  ,
        fecha_ini_pen       LIKE ret_det_datamart.fecha_ini_pen     ,
        fec_ini_pago        LIKE ret_det_datamart.fec_ini_pago      ,
        fecha_resolucion    LIKE ret_det_datamart.fecha_resolucion  ,
        fecha_solicitud     LIKE pen_solicitud_pmg.fecha_solicitud  ,
        folio_solicitud     LIKE pen_solicitud_pmg.folio_solicitud  ,
        folio_contrato      LIKE pen_solicitud_pmg.folio_contrato   ,
        sec_contrato        LIKE pen_solicitud_pmg.sec_contrato     ,
        folio_lote          LIKE pen_solicitud_pmg.folio_lote       ,
        fecha_captura       LIKE pen_solicitud_pmg.fecha_captura    ,
        fecha_modifica      LIKE pen_solicitud_pmg.fecha_modifica   ,
        fecha_confirma      LIKE pen_solicitud_pmg.fecha_confirma   ,
        fecha_pago          LIKE pen_solicitud_pmg.fecha_pago       ,
        consecutivo         LIKE pen_solicitud_pmg.consecutivo      ,
        usuario_captura     LIKE pen_solicitud_pmg.usuario_captura  ,
        usuario_modifica    LIKE pen_solicitud_pmg.usuario_modifica ,
        usuario_confirma    LIKE pen_solicitud_pmg.usuario_confirma ,
        estado_solicitud    LIKE pen_solicitud_pmg.estado_solicitud ,
        desc_estado         CHAR(40)
    END RECORD

    DEFINE lr_sol_pmg RECORD LIKE pen_solicitud_pmg.*

    -- ------------------------------------------------------------------------
    SELECT grupo
    INTO   lr_sol_pmg.grupo
    FROM   ret_matriz_derecho
    WHERE  tipo_retiro      = pr_pantalla.tipo_retiro
    AND    regimen          = pr_pantalla.regimen
    AND    tipo_seguro      = pr_pantalla.tipo_seguro
    AND    tipo_pension     = pr_pantalla.tipo_pension
    AND    tipo_prestacion  = pr_pantalla.tipo_prestacion

    LET lr_sol_pmg.nss                  = pr_pantalla.nss
    LET lr_sol_pmg.consecutivo          = 1
    LET lr_sol_pmg.sec_contrato         = pr_pantalla.sec_contrato
    LET lr_sol_pmg.folio_contrato       = pr_pantalla.folio_contrato
    LET lr_sol_pmg.folio_lote           = 0
    LET lr_sol_pmg.folio_solicitud      = pr_pantalla.folio_solicitud
    LET lr_sol_pmg.curp                 = pr_pantalla.curp
    LET lr_sol_pmg.sec_pension          = pr_pantalla.sec_pension
    LET lr_sol_pmg.tipo_retiro          = pr_pantalla.tipo_retiro
    LET lr_sol_pmg.regimen              = pr_pantalla.regimen
    LET lr_sol_pmg.tipo_seguro          = pr_pantalla.tipo_seguro
    LET lr_sol_pmg.tipo_pension         = pr_pantalla.tipo_pension
    LET lr_sol_pmg.tipo_prestacion      = pr_pantalla.tipo_prestacion
    LET lr_sol_pmg.importe_mensual      = pr_pantalla.importe_mensual
    LET lr_sol_pmg.fecha_ini_pen        = pr_pantalla.fecha_ini_pen
    LET lr_sol_pmg.fecha_resolucion     = pr_pantalla.fecha_resolucion
    LET lr_sol_pmg.fecha_solicitud      = pr_pantalla.fecha_solicitud
    LET lr_sol_pmg.fecha_pago           = NULL
    LET lr_sol_pmg.estado_sub_viv       = pr_pantalla.estado_sub_viv
    LET lr_sol_pmg.diag_registro        = NULL
    LET lr_sol_pmg.fecha_captura        = pr_pantalla.fecha_captura
    LET lr_sol_pmg.hora_captura         = CURRENT HOUR TO SECOND
    LET lr_sol_pmg.usuario_captura      = pr_pantalla.usuario_captura
    LET lr_sol_pmg.fecha_modifica       = NULL
    LET lr_sol_pmg.hora_modifica        = NULL
    LET lr_sol_pmg.usuario_modifica     = pr_pantalla.usuario_modifica
    LET lr_sol_pmg.fecha_confirma       = NULL
    LET lr_sol_pmg.hora_confirma        = NULL
    LET lr_sol_pmg.usuario_confirma     = pr_pantalla.usuario_confirma
    LET lr_sol_pmg.codigo_rechazo       = NULL
    LET lr_sol_pmg.estado_solicitud     = pr_pantalla.estado_solicitud

    INSERT INTO tmp_solicitud_pmg
    VALUES (lr_sol_pmg.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_insuficiencia : Almacena los registros que se encontraron con   #
#                           insuficiencia de saldos para los tipos de       #
#                           retiro S (PMG)                                  #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_insuficiencia(pr_datos)

    DEFINE pr_datos RECORD
        nss             LIKE ret_det_datamart.nss           ,
        curp            LIKE ret_det_datamart.curp          ,
        sec_pension     LIKE ret_det_datamart.sec_pension   ,
        num_mens        SMALLINT                            ,
        fecha_ini_pen   LIKE ret_det_datamart.fecha_ini_pen
    END RECORD

    DEFINE pr_saldo RECORD
        subcta      SMALLINT        ,
        sie         SMALLINT        ,
        monto_acc   DECIMAL(16,6)   ,
        monto_pes   DECIMAL(16,6)
    END RECORD

    DEFINE lr_insuficiencia RECORD LIKE pen_detalle_op70.*

    DEFINE
        ls_subcta       ,
        ls_grupo        SMALLINT

    -- -----------------------------------------------------------------

    INITIALIZE lr_insuficiencia.* TO NULL
    
    LET ls_subcta   = 0
    LET ls_grupo    = 0

    LET lr_insuficiencia.folio_envio        = 0

    SELECT clave
    INTO   lr_insuficiencia.id_tipo_notifica
    FROM   tab_tipo_notifica_pmg
    WHERE  descripcion MATCHES '*INSUFICIENCIA*'

    LET lr_insuficiencia.nss                = pr_datos.nss
    LET lr_insuficiencia.curp               = pr_datos.curp
    LET lr_insuficiencia.sec_pension        = pr_datos.sec_pension
    LET lr_insuficiencia.folio_datamart     = 0
    LET lr_insuficiencia.tipo_retiro        = "S"

    SELECT regimen          ,
           tipo_seguro      ,
           tipo_pension     ,
           "   "            ,
           tipo_prestacion
    INTO   lr_insuficiencia.regimen             ,
           lr_insuficiencia.tipo_seguro         ,
           lr_insuficiencia.tipo_pension        ,
           lr_insuficiencia.cve_pension         ,
           lr_insuficiencia.tipo_prestacion
    FROM   ret_matriz_derecho
    WHERE  tipo_retiro = lr_insuficiencia.tipo_retiro

    LET lr_insuficiencia.fecha_ini_pen      = pr_datos.fecha_ini_pen
    LET lr_insuficiencia.fecha_primer_pago  = NULL 
    LET lr_insuficiencia.mto_retiro97       = 0
    LET lr_insuficiencia.mto_cv             = 0
    LET lr_insuficiencia.mto_cs             = 0
    LET lr_insuficiencia.mto_viv97          = 0
    LET lr_insuficiencia.num_men_pagadas    = 0
    LET lr_insuficiencia.num_men_calculadas = pr_datos.num_mens
    LET lr_insuficiencia.mto_total_pmg      = 0
    LET lr_insuficiencia.fecha_ultimo_pago  = NULL   
    LET lr_insuficiencia.fecha_agotamiento  = NULL
    LET lr_insuficiencia.fecha_fallecimiento= NULL
    LET lr_insuficiencia.diag_operacion     = NULL
    LET lr_insuficiencia.codigo_rechazo     = 0 
    LET lr_insuficiencia.origen_informacion = 2
    LET lr_insuficiencia.fecha_carga        = HOY
    LET lr_insuficiencia.hora_carga         = CURRENT HOUR TO SECOND
    LET lr_insuficiencia.estado             = gr_edo.capturado
    LET lr_insuficiencia.usuario            = gc_usuario

    LET lr_insuficiencia.saldo_retiro97     = 0
    LET lr_insuficiencia.saldo_cv           = 0
    LET lr_insuficiencia.saldo_cs           = 0
    LET lr_insuficiencia.saldo_viv97        = 0

    DECLARE cur_saldo CURSOR FOR eje_saldo_dia

    FOREACH cur_saldo USING pr_datos.nss    ,
                            ls_subcta       ,
                            ls_grupo        ,
                            HOY
                      INTO  pr_saldo.*

        CASE pr_saldo.subcta
            WHEN 1
                LET lr_insuficiencia.saldo_retiro97 = lr_insuficiencia.saldo_retiro97 + pr_saldo.monto_acc
            
            WHEN 2
                LET lr_insuficiencia.saldo_cv       = lr_insuficiencia.saldo_cv + pr_saldo.monto_acc

            WHEN 6
                LET lr_insuficiencia.saldo_cv       = lr_insuficiencia.saldo_cv + pr_saldo.monto_acc

            WHEN 9
                LET lr_insuficiencia.saldo_cv       = lr_insuficiencia.saldo_cv + pr_saldo.monto_acc

            WHEN 5
                LET lr_insuficiencia.saldo_cs       = lr_insuficiencia.saldo_cs + pr_saldo.monto_acc

            WHEN 4
                LET lr_insuficiencia.saldo_viv97    = lr_insuficiencia.saldo_viv97 + pr_saldo.monto_acc
        END CASE
    
    END FOREACH 

    INSERT INTO pen_detalle_op70
    VALUES (lr_insuficiencia.*)

END FUNCTION 


#---------------------------------------------------------------------------#
# f_modifica_solicitud : Realiza la modificacion de la tabla fisica con los #
#                        valores que se hayan capturado anteriormente       #
#---------------------------------------------------------------------------#
FUNCTION f_modifica_solicitud(pr_update)

    DEFINE pr_update RECORD
        nss                 LIKE pen_solicitud_pmg.nss              ,
        consecutivo         LIKE pen_solicitud_pmg.consecutivo      ,
        fecha_solicitud     LIKE pen_solicitud_pmg.fecha_solicitud  ,
        folio_solicitud     LIKE pen_solicitud_pmg.folio_solicitud  ,
        folio_contrato      LIKE pen_solicitud_pmg.folio_contrato   ,
        importe_mensual     LIKE pen_solicitud_pmg.importe_mensual        
    END RECORD

    DEFINE
        li_cont         SMALLINT

    UPDATE pen_solicitud_pmg
    SET    fecha_solicitud  = pr_update.fecha_solicitud ,
           folio_solicitud  = pr_update.folio_solicitud ,
           folio_contrato   = pr_update.folio_contrato  ,
           importe_mensual  = pr_update.importe_mensual ,
           fecha_modifica   = HOY                       ,
           hora_modifica    = CURRENT HOUR TO SECOND    ,
           usuario_modifica = gc_usuario
    WHERE  nss              = pr_update.nss
    AND    consecutivo      = pr_update.consecutivo

    LET li_cont = SQLCA.SQLERRD[3]

    -- Si no ocurrieron errores guardamos las tablas de calculos
    IF li_cont > 0 THEN
        CALL f_error_msg("¡SOLICITUD MODIFICADA CORRECTAMENTE!")
    ELSE
        CALL f_error_msg("ERROR - NO PUDO MODIFICARSE LA SOLICITUD")
    END IF

END FUNCTION

{==============================================================================}
{ Objetivo : Permitir modificar los datos bancarios de un beneficiario         }
{ Recibe   : NSS y consecutivo, identificador de la solicitud con la que se    }
{          : asociara al beneficiario                                          }
{ Regresa  :                                                                   }
{ Elaboro  : Isai Jimenez Rojas OCTUBRE 2013                                   }
{ Modifico :                                                                   }
{ Implementada para MLM-1936                                                   }
{==============================================================================}
{
FUNCTION fn_modifica_datos_bancarios(p_nss, p_consecutivo)

   DEFINE p_nss                LIKE ret_beneficiario.nss
   DEFINE p_consecutivo        LIKE ret_beneficiario.consecutivo
   
   DEFINE li_contador          INTEGER
   DEFINE ls_tam               SMALLINT
   
   DEFINE lr_benef             RECORD LIKE ret_beneficiario.*
   DEFINE lr_benef2            RECORD
          cod_sucursal         CHAR(4)
          END RECORD  
   
   ---------------------------------------------------------------------------
   
   -- VERIFICA EL NUMERO DE BENEFICIARIOS REGISTRADOS PARA LA SOLICITUD
   SELECT NVL(COUNT(*),0)
   INTO   li_contador
   FROM   ret_beneficiario
   WHERE  nss          = p_nss
   AND    consecutivo  = p_consecutivo
   
   IF li_contador != 1 THEN 
      ERROR "TIENE MAS DE UN BENEFICIARIO O NO TIENE BENEFICIARIOS "
      RETURN
   END IF
   
   --SELECCIONA LOS DATOS DEL BENEFICIARIO
   SELECT *
   INTO   lr_benef.*
   FROM   ret_beneficiario
   WHERE  nss          = p_nss
   AND    consecutivo  = p_consecutivo
         
   OPEN WINDOW w_penm102 AT 2,2 WITH FORM "PENM1002" ATTRIBUTE(BORDER)
      
      DISPLAY " [ESC] Guardar cambios                                      [CTRL-C] Cancelar " AT 1,1
      DISPLAY " PENM100                    DATOS DEL BENEFICIARIO                            " AT 4,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "DD-MM-YYYY"  AT 2,69 ATTRIBUTE(REVERSE)
      DISPLAY "                     DATOS BANCARIOS PARA PAGO DEL RETIRO                     " AT 9,1 ATTRIBUTE(REVERSE)
      DISPLAY "                          DOMICILIO DEL BENEFICIARIO                          " AT 14,1 ATTRIBUTE(REVERSE)

      ------------------------------   
      --CAPTURA DEL BENEFICIARIO
      ------------------------------
      INPUT BY NAME lr_benef.tipo_pago, 
                    lr_benef.banco,
                    lr_benef.tipo_cuenta,
                    lr_benef2.cod_sucursal,
                    lr_benef.clabe, 
                    lr_benef.num_cuenta
                    WITHOUT DEFAULTS

         -----------------
         BEFORE INPUT
         -----------------
       
            -- RECUPERA Y DESPLIEGA DOMICILIO DEL BENEFICIARIO --
            CALL fn_despliega_beneficiario(p_nss, p_consecutivo)

         ------------------------
         AFTER FIELD tipo_pago 
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS 
            END IF
            
            IF lr_benef.tipo_pago IS NULL THEN
               LET lr_benef.tipo_pago = fn_trae_tipo_pago()
               DISPLAY BY NAME lr_benef.tipo_pago
               IF lr_benef.tipo_pago IS NULL THEN
                  ERROR "DEBE SELECCIONAR EL TIPO DE PAGO..."
                  NEXT FIELD tipo_pago
               END IF
            ELSE
               --Checa si es un tipo de pago valido
               SELECT COUNT(*)
               INTO   li_contador
               FROM   tab_pago
               WHERE  tipo_pago  =  lr_benef.tipo_pago
         
               IF li_contador = 0  THEN
                  ERROR "  NO EXISTE TIPO DE PAGO    "
                  NEXT FIELD tipo_pago
               ELSE
                  --AL CAMBIAR EL TIPO DE PAGO SE LIMPIAN TODOS LOS CAMPOS 
                  --DE ACUERDO AL TIPO DE PAGO SELECCIONADO
                  IF lr_benef.tipo_pago = 4 THEN 
                     LET lr_benef.banco = NULL
                  ELSE
                     LET lr_benef.banco = 2
                  END IF
                  
                  LET lr_benef.tipo_cuenta  = NULL
                  LET lr_benef2.cod_sucursal = NULL
                  LET lr_benef.num_cuenta    = NULL
                  LET lr_benef.clabe         = NULL
                              
                  DISPLAY BY NAME lr_benef.banco       
                  DISPLAY BY NAME lr_benef.tipo_cuenta 
                  DISPLAY BY NAME lr_benef2.cod_sucursal
                  DISPLAY BY NAME lr_benef.num_cuenta  
                  DISPLAY BY NAME lr_benef.clabe       
                  
                  --MUEVE EL CONTROL AL CAMPO QUE CORRESPONDA SEGUN TIPO DE PAGO
                  CASE lr_benef.tipo_pago
                       WHEN 1 
                              NEXT FIELD tipo_cuenta
                       WHEN 2 
                              NEXT FIELD tipo_pago
                       WHEN 3 
                              NEXT FIELD tipo_pago
                       WHEN 4
                              NEXT FIELD banco
                  END CASE
               END IF
            END IF

      ------------------------
      BEFORE FIELD banco
      ------------------------
         IF lr_benef.tipo_pago = 1 OR
            lr_benef.tipo_pago = 2 OR
            lr_benef.tipo_pago = 3 THEN

            LET lr_benef.banco = 2
            DISPLAY BY NAME lr_benef.banco

            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            ELSE
               NEXT FIELD NEXT
            END IF

         END IF 

      ------------------------
      AFTER FIELD banco
      ------------------------
         IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
            NEXT FIELD PREVIOUS
         END IF

         IF lr_benef.banco IS NULL  THEN
            LET lr_benef.banco = fn_trae_banco( )
            DISPLAY BY NAME lr_benef.banco
            
            IF lr_benef.banco IS NULL  THEN
               ERROR " DEBE SELECCIONAR EL BANCO "
               NEXT FIELD banco
            END IF 
         END IF
         
         IF lr_benef.banco = 2 AND lr_benef.tipo_pago = 4 THEN
            ERROR "DEBE SELECCIONAR TIPO DE PAGO 1 (TEF) PARA ESTE BANCO "
            NEXT FIELD banco
         END IF

         --Checa si es un banco valido
         SELECT COUNT(*)
         INTO   li_contador
         FROM   tab_banco
         WHERE  banco = lr_benef.banco

         IF li_contador = 0  THEN 
            ERROR " NO EXISTE CLAVE DE BANCO "
            NEXT FIELD banco
         ELSE
            NEXT FIELD clabe
         END IF 

      ------------------------
      AFTER FIELD tipo_cuenta
      ------------------------
         IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
            CASE lr_benef.tipo_pago
                 WHEN 1    NEXT FIELD tipo_pago
                 WHEN 2    NEXT FIELD tipo_pago
                 WHEN 3    NEXT FIELD tipo_pago
                 WHEN 4    NEXT FIELD tipo_pago
            END CASE
         END IF
      
         IF lr_benef.tipo_cuenta IS NULL AND
            lr_benef.tipo_pago = 1 THEN
            ERROR " DEBE PROPORCIONAR TIPO DE CUENTA 1 o 3 "   --IJR
            NEXT FIELD tipo_cuenta
         END IF
      
         CASE lr_benef.tipo_cuenta
              WHEN 1     
                        NEXT FIELD cod_sucursal
              WHEN 3     
                        LET lr_benef.cod_sucursal = NULL 
                        DISPLAY BY NAME lr_benef.cod_sucursal
                        NEXT FIELD num_cuenta
              OTHERWISE 
                        ERROR " TIPO DE CUENTA INVALIDO, DEBE PROPORCIONAR 1 o  3 "
                        NEXT FIELD tipo_cuenta
         END CASE

      ------------------------
      AFTER FIELD cod_sucursal
      ------------------------
          IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
             CASE lr_benef.tipo_pago
                  WHEN 1    NEXT FIELD tipo_cuenta
                  WHEN 2    NEXT FIELD tipo_pago
                  WHEN 3    NEXT FIELD tipo_pago
                  WHEN 4    NEXT FIELD tipo_pago
             END CASE
          END IF
             
          IF lr_benef2.cod_sucursal IS NULL AND
             lr_benef.tipo_cuenta = 1 THEN
             ERROR "  LA  SUCURSAL  NO  PUEDE  SER  NULA  "
             NEXT FIELD cod_sucursal
          END IF
          
          LET ls_tam = LENGTH(lr_benef2.cod_sucursal)
          
          IF ls_tam != 4 THEN
             ERROR " DEBE INGRESAR LA SUCURSAL A 4 DIGITOS MIDE ",ls_tam
             NEXT FIELD cod_sucursal
          END IF
          
          LET lr_benef.cod_sucursal = lr_benef2.cod_sucursal
          
          NEXT FIELD num_cuenta

      ------------------------
      AFTER FIELD num_cuenta
      ------------------------
         IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
            CASE lr_benef.tipo_pago
                 WHEN 1 
                        IF lr_benef.tipo_cuenta = 1 THEN
                           NEXT FIELD cod_sucursal
                        ELSE
                           NEXT FIELD tipo_cuenta
                        END IF
                 WHEN 2 NEXT FIELD tipo_pago
                 WHEN 3 NEXT FIELD tipo_pago
                 WHEN 4 NEXT FIELD banco
            END CASE
         END IF

         IF lr_benef.tipo_pago = 1 THEN
            IF lr_benef.tipo_cuenta = 1 THEN
               IF LENGTH(lr_benef.num_cuenta) != 7 THEN
                  ERROR "DEBE INGRESAR EL NUMERO DE CUENTA A 7 POSICIONES "
                  NEXT FIELD num_cuenta
               END IF
               IF f_valida_numerico(lr_benef.num_cuenta) != 0 THEN 
                  ERROR " CAMPO ERRONEO - NO NUMERICO "
                  NEXT FIELD num_cuenta
               END IF
            ELSE
               IF LENGTH(lr_benef.num_cuenta) != 16 THEN
                  ERROR "DEBE INGRESAR EL NUMERO DE CUENTA A 16 POSICIONES "
                  NEXT FIELD num_cuenta
               END IF
               IF f_valida_numerico(lr_benef.num_cuenta) != 0 THEN 
                  ERROR " CAMPO ERRONEO - NO NUMERICO "
                  NEXT FIELD num_cuenta
               END IF
            END IF
         END IF

      ------------------------
      AFTER FIELD clabe
      ------------------------
         IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
            CASE lr_benef.tipo_pago
                 WHEN 1    NEXT FIELD tipo_pago
                 WHEN 2    NEXT FIELD tipo_pago
                 WHEN 3    NEXT FIELD tipo_pago
                 WHEN 4    NEXT FIELD banco
            END CASE
         END IF

         IF lr_benef.clabe     IS NULL AND 
            lr_benef.tipo_pago = 4 THEN
            ERROR "DEBE PROPORCIONAR CLABE"   --IJR
            NEXT FIELD clabe
         END IF 

         IF LENGTH(lr_benef.clabe) != 18 THEN
            ERROR " DEBE INGRESAR LA CUENTA CLABE A 18 DIGITOS "
            NEXT FIELD clabe
         END IF

         IF f_valida_numerico(lr_benef.clabe) != 0 THEN 
            ERROR " CAMPO ERRONEO - NO NUMERICO "
            NEXT FIELD clabe
         END IF
         
         NEXT FIELD NEXT
      
      -----------
      AFTER INPUT
      -----------
         -- Por si el usuario quiere cancelar la captura
         IF FGL_LASTKEY()= FGL_KEYVAL("INTERRUPT") OR
            FGL_LASTKEY()= FGL_KEYVAL("CONTROL-C") THEN
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
            
         -- No podra cambiar de registro SI los siguientes campos son nulos
         IF lr_benef.tipo_pago  IS NULL OR
            lr_benef.banco      IS NULL OR
            lr_benef.dom_codpos IS NULL THEN
            ERROR "NO PUEDE CAMBIAR DE REGISTRO HABIENDO CAMPOS VACIOS"
            NEXT FIELD tipo_pago
         END IF

      ------------------------
      ON KEY(ESC)
      ------------------------
         --VALIDACION DE DATOS BANCARIOS MLM-1870 IJR
         IF lr_benef.tipo_pago IS NULL  THEN
            ERROR "    EL  TIPO  DE  PAGO  NO  PUEDE  SER  NULO    "
            NEXT FIELD tipo_pago
         END IF 

         IF lr_benef.banco IS NULL  THEN
            NEXT FIELD  banco
            ERROR  "      EL  BANCO  NO  PUEDE SER  NULO    "
         END IF

         IF lr_benef.tipo_cuenta  IS NULL  AND
            lr_benef.tipo_pago    = 1      THEN 
            ERROR "     EL  TIPO DE CUENTA NO  PUEDE  SER  NULO     "
            NEXT FIELD cod_sucursal
         END IF

         IF lr_benef.cod_sucursal IS NULL  AND
            lr_benef.tipo_pago    = "1"    AND 
            lr_benef.tipo_cuenta  = "1"   THEN
            ERROR "     EL  NUMERO  DE  SUCURSAL  NO  PUEDE  SER  NULO     "
            NEXT FIELD cod_sucursal
         END IF

         IF lr_benef.num_cuenta IS NULL  AND
            lr_benef.tipo_pago  = 1 THEN
            ERROR "     EL  NUMERO  DE  CUENTA  NO  PUEDE  SER  NULO     "
            NEXT  FIELD  num_cuenta
         END IF

         IF lr_benef.clabe      IS NULL  AND
            lr_benef.tipo_pago  = 4 THEN
            ERROR "     LA CUENTA CLABE NO PUEDE SER NULA    "
            NEXT  FIELD  num_cuenta
         END IF
         
         EXIT INPUT
 
   END INPUT  --fin de la modificacion de datos bancarios 

   IF INT_FLAG THEN
      --SE CANCELO LA OPERACION 
      LET INT_FLAG = FALSE 
      ERROR " OPERACION CANCELADA POR EL USUARIO "
      CLOSE WINDOW w_penm102
      RETURN
   END IF

   -----------------------------------------
   --VALIDA SI SE QUIERE APLICAR EL CAMBIO
   -----------------------------------------
   OPEN WINDOW wagrega AT 11,13 WITH 2 rows, 55 COLUMNS ATTRIBUTE(BORDER)

      MENU "ESTA SEGURO DE GUARDAR LOS CAMBIOS"
           COMMAND "Si"
                   CALL fn_actualiza_datos_bancarios(p_nss, 
                                                     p_consecutivo,
                                                     lr_benef.tipo_pago,
                                                     lr_benef.banco,
                                                     lr_benef2.cod_sucursal,
                                                     lr_benef.num_cuenta,
                                                     lr_benef.tipo_cuenta,
                                                     lr_benef.clabe
                                                    )
                   EXIT MENU
           COMMAND "No"
                   PROMPT " OPERACION CANCELADA POR EL USUARIO PRESIONE <ENTER>: " ATTRIBUTE(REVERSE) FOR CHAR ENTER
                   EXIT MENU
      END MENU
   
   CLOSE WINDOW wagrega
      
   CLOSE WINDOW w_penm102

END FUNCTION
}
#==============================================================================#
# Despliega los datos generales del beneficiario                               #
# Implementada para MLM-1936                                                   #
#==============================================================================#
{
FUNCTION fn_despliega_beneficiario(p_nss, p_consecutivo)
   
   DEFINE p_nss               LIKE ret_beneficiario.nss
   DEFINE p_consecutivo       LIKE ret_beneficiario.consecutivo
   DEFINE lr_benef            RECORD LIKE ret_beneficiario.*
   DEFINE lr_benef2           RECORD
          consec_benef        LIKE  ret_beneficiario.consec_benef     ,
          nss_benef           LIKE  ret_beneficiario.nss_benef        ,
          rfc_benef           LIKE  ret_beneficiario.rfc_benef        ,
          paterno             LIKE  ret_beneficiario.paterno          ,
          materno             LIKE  ret_beneficiario.materno          ,
          nombres             LIKE  ret_beneficiario.nombres          ,
          paren_cod           LIKE  ret_beneficiario.paren_cod        ,
          paren_des           CHAR(30)                                ,
          porcentaje          LIKE  ret_beneficiario.porcentaje       ,
          tipo_pago           LIKE  ret_beneficiario.tipo_pago        ,
          banco               LIKE  ret_beneficiario.banco            ,
          tipo_cuenta         LIKE  ret_beneficiario.tipo_cuenta      ,
          cod_sucursal        CHAR(4)                                 ,
          clabe               LIKE  ret_beneficiario.clabe            ,
          num_cuenta          LIKE  ret_beneficiario.num_cuenta       ,
          ciudad_cod          INTEGER                                 ,
          monto_en_pesos      LIKE  ret_beneficiario.monto_en_pesos   ,
          fecha_pago          LIKE  ret_beneficiario.fecha_pago       ,
          dom_calle           LIKE  ret_beneficiario.dom_calle        ,
          dom_numero_ext      LIKE  ret_beneficiario.dom_numero_ext   ,
          dom_numero_int      LIKE  ret_beneficiario.dom_numero_int   ,
          dom_codpos          LIKE  ret_beneficiario.dom_codpos       ,
          dom_colonia         LIKE  ret_beneficiario.dom_colonia      ,
          dom_delega          LIKE  ret_beneficiario.dom_delega       ,
          nom_delega          CHAR(30)                                ,
          dom_ciudad_cod      LIKE  ret_beneficiario.dom_ciudad_cod   ,
          nom_ciudad_cod      CHAR(30)                                ,
          dom_estado_cod      LIKE  ret_beneficiario.dom_estado_cod   ,
          nom_estado_cod      CHAR(30)                                ,
          dom_telefono        LIKE  ret_beneficiario.dom_telefono      
          END RECORD
   DEFINE lc_cmd              CHAR(200)
   DEFINE lc_nombre           CHAR(80)
   DEFINE ls_contador         SMALLINT 
   ---------------------------------------------------------------------------
   
   --------------------------------------------
   --OBTIENE Y DESPLIEGA NOMBRE DEL TRABAJADOR
   --------------------------------------------
   LET lc_cmd = "SELECT TRIM(nombres)||' '||TRIM(paterno)||' '||TRIM(materno)", 
                "  FROM   afi_mae_afiliado ",
                " WHERE   n_seguro = ? "

   PREPARE exenom FROM lc_cmd
   EXECUTE exenom USING p_nss INTO lc_nombre
   
   DISPLAY p_nss     TO nss
   DISPLAY lc_nombre TO nombre 
   
   -------------------------
   --RECUPERA BENEFICIARIO
   -------------------------
   SELECT COUNT(*)
   INTO   ls_contador
   FROM   ret_beneficiario
   WHERE  nss          = p_nss
   AND    consecutivo  = p_consecutivo
   
   IF ls_contador != 1 THEN
      ERROR "NSS CON INCONSISTENCIAS, TIENE MAS DE UN BENEFICIARIO O NO TIENE BENEFICIARIOS"
      RETURN
   END IF   

   SELECT *
   INTO   lr_benef.*
   FROM   ret_beneficiario
   WHERE  nss          = p_nss
   AND    consecutivo  = p_consecutivo
   
   DISPLAY BY NAME lr_benef.consec_benef
   DISPLAY BY NAME lr_benef.nss_benef
   DISPLAY BY NAME lr_benef.rfc_benef  
   DISPLAY BY NAME lr_benef.paterno    
   DISPLAY BY NAME lr_benef.materno    
   DISPLAY BY NAME lr_benef.nombres    
   DISPLAY BY NAME lr_benef.paren_cod
   DISPLAY BY NAME lr_benef.porcentaje  
   DISPLAY BY NAME lr_benef.tipo_pago
   DISPLAY BY NAME lr_benef.banco
   DISPLAY BY NAME lr_benef.clabe
   DISPLAY BY NAME lr_benef.num_cuenta

   -----------------------------------
   --RECUPERA DATOS DEL DOMICILIO
   -----------------------------------
   SELECT a.ciudad_cod          ,
          a.monto_en_pesos      ,
          a.fecha_pago          ,
          a.dom_calle           ,
          a.dom_numero_ext      ,
          a.dom_numero_int      ,
          a.dom_codpos          ,
          a.dom_colonia         ,
          a.dom_delega          ,
          b.deleg_desc          ,
          a.dom_ciudad_cod      ,
          c.ciudad_desc         ,
          a.dom_estado_cod      ,
          d.estad_desc          ,
          a.dom_telefono
   INTO   lr_benef2.ciudad_cod      ,
          lr_benef2.monto_en_pesos  ,
          lr_benef2.fecha_pago      ,
          lr_benef2.dom_calle       ,
          lr_benef2.dom_numero_ext  ,
          lr_benef2.dom_numero_int  ,
          lr_benef2.dom_codpos      ,
          lr_benef2.dom_colonia     ,
          lr_benef2.dom_delega      ,
          lr_benef2.nom_delega      ,
          lr_benef2.dom_ciudad_cod  ,
          lr_benef2.nom_ciudad_cod  ,
          lr_benef2.dom_estado_cod  ,
          lr_benef2.nom_estado_cod  ,
          lr_benef2.dom_telefono
   FROM   ret_beneficiario     a,
          afi_mae_afiliado     m,
          OUTER tab_delegacion b,
          OUTER tab_ciudad     c,
          OUTER tab_estado     d
   WHERE  a.nss            = p_nss
   AND    a.nss            = m.n_seguro       
   AND    b.deleg_cod      = a.dom_delega
   AND    c.ciudad_cod     = a.dom_ciudad_cod
   AND    d.estad_cod      = a.dom_estado_cod
   
   IF STATUS = NOTFOUND THEN
      ERROR " NO SE ENCONTRO DOMICILIO "
   ELSE
      --DESPLIEGA DATOS DE DOMICILIO EN LA FORMA
      DISPLAY BY NAME lr_benef2.ciudad_cod
      DISPLAY BY NAME lr_benef2.monto_en_pesos     
      DISPLAY BY NAME lr_benef2.monto_en_pesos  
      DISPLAY BY NAME lr_benef2.dom_calle
      DISPLAY BY NAME lr_benef2.dom_numero_ext
      DISPLAY BY NAME lr_benef2.dom_numero_int
      DISPLAY BY NAME lr_benef2.dom_codpos    
      DISPLAY BY NAME lr_benef2.dom_colonia   
      DISPLAY BY NAME lr_benef2.dom_delega    
      DISPLAY BY NAME lr_benef2.nom_delega    
      DISPLAY BY NAME lr_benef2.dom_ciudad_cod
      DISPLAY BY NAME lr_benef2.nom_ciudad_cod
      DISPLAY BY NAME lr_benef2.dom_estado_cod
      DISPLAY BY NAME lr_benef2.nom_estado_cod
   END IF 
   
END FUNCTION
}
#==============================================================================#
# Actualiza los datos bancarios modificados de un beneficiario                 #
# Implementada para MLM-1936                                                   #
#==============================================================================#
{
FUNCTION fn_actualiza_datos_bancarios(p_nss, 
                                      p_consecutivo,
                                      p_tipo_pago,
                                      p_banco,
                                      p_cod_sucursal,
                                      p_num_cuenta,
                                      p_tipo_cuenta,
                                      p_clabe)

   DEFINE p_nss               LIKE ret_beneficiario.nss
   DEFINE p_consecutivo       LIKE ret_beneficiario.consecutivo
   DEFINE p_tipo_pago         LIKE ret_beneficiario.tipo_pago   
   DEFINE p_banco             LIKE ret_beneficiario.banco       
   DEFINE p_cod_sucursal      LIKE ret_beneficiario.cod_sucursal
   DEFINE p_num_cuenta        LIKE ret_beneficiario.num_cuenta  
   DEFINE p_tipo_cuenta       LIKE ret_beneficiario.tipo_cuenta 
   DEFINE p_clabe             LIKE ret_beneficiario.clabe       

   WHENEVER ERROR CONTINUE

      UPDATE ret_beneficiario
      SET    tipo_pago        = p_tipo_pago     ,
             banco            = p_banco         ,
             cod_sucursal     = p_cod_sucursal  ,
             num_cuenta       = p_num_cuenta    ,
             tipo_cuenta      = p_tipo_cuenta   ,
             clabe            = p_clabe         ,
             usuario_modifica = gc_usuario      ,
             fecha_modifica   = TODAY
      WHERE  nss          = p_nss
      AND    consecutivo  = p_consecutivo

      IF SQLCA.SQLCODE < 0  THEN
         LET g_mensaje = "FALLO ACTUALIZACION, ",
                         " NSS: ",p_nss, 
                         " Consecutivo: ",p_consecutivo,
                         ERR_GET(SQLCA.SQLCODE)
         CALL errorlog(g_mensaje CLIPPED)
         PROMPT "SE PRESENTO UN ERROR AL ACTUALIZAR BENEFICIARIO NOTIFIQUE A SISTEMAS <ENTER>: " FOR CHAR ENTER
      END IF
   WHENEVER ERROR STOP 

END FUNCTION
}
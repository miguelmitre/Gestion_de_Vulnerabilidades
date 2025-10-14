#################################################################################
# Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
# Sistema           => INF                                                      #
# Programa INFM001  => DEVOLUCION DE RECURSOS AL INFONAVIT POR APLICACIONES     #
#                      INDEVIDAS OPER.97-98                                     #
# Creado por        => DAVID HERNANDEZ OYARCE.                                  #
# Fecha creacion    => 02 DE DICIEMBRE DEL 2001                                 #
# Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                              #
# Fecha actualiz.   => 25 DE ENERO DEL 2002                                     #
# Actualizacion     => JOSE LUIS SALDIVAR CARDOSO                               #
# fecha actualiz.   => 03 DE JUNIO DEL 2003                                     #
# Actualizacion     => ISAI JIMENEZ ROJAS                                       #
# fecha actualiz.   => 23 DE SEPTIEMBRE 04 (Integracion de participaciones)     #
#                   => Integracion de participaciones y mod. al layout   17:00  #
# fecha actualiz.   => 13 DE DICIEMBRE DE 2013                                  #
# Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                      - Se optimizo el codigo fuente                           #
#                      - Se aplican librerias especiales al modulo de devolucion#
#################################################################################



DATABASE safre_af

GLOBALS
    DEFINE gar_montos ARRAY[500] OF RECORD
        tipo_viv                LIKE inf_his_oper97.tipo_viv            ,
        fecha_valor_trasp       LIKE inf_his_oper97.fecha_valor_trasp   ,
        fecha_valor_devol       LIKE inf_his_oper97.fecha_valor_devol   ,
        mto_pesos_orig          LIKE inf_his_oper97.mto_pesos_orig      ,
        mto_parti_dev           LIKE inf_his_oper97.mto_parti_dev       ,
        mto_pesos_dev           LIKE inf_his_oper97.mto_pesos_dev       ,
        int_pesos_dev           LIKE inf_his_oper97.int_pesos_dev
    END RECORD

    DEFINE gar_datos ARRAY[50] OF RECORD
        nss                     LIKE inf_his_oper97.nss                 ,
        nombre                  CHAR(38)                                ,
        opera_devolucion        CHAR(02)                                ,
        origen_devolucion       CHAR(03)                                ,
        fecha_afectacion        LIKE inf_his_oper97.fecha_afectacion    ,
        cargo_abono             CHAR(01)
    END RECORD

    DEFINE gr_estado RECORD
        capturado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE g_ultimo_folio LIKE glo_folio.folio

    DEFINE
        HOY                     DATE

    DEFINE
        gc_usuario              CHAR(015)

    DEFINE
        arr_c                   ,
        scr_l                   SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT

    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        MESSAGE LINE LAST-1 ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I,
        NEXT KEY F3         ,
        PREVIOUS KEY F4

    CALL f_lib_abre_log("INFM001")
    CALL init()

    OPEN WINDOW infm0011 AT 4,2 WITH FORM "INFM0011" ATTRIBUTE(BORDER)
    DISPLAY " INFM001          DEVOLUCION AL INFONAVIT OPER.97 O OPER.98                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "MENU"
        COMMAND "Agrega" "Agrega Registros"
            CALL f_agrega_sol()
            CALL inicializa()

        COMMAND "Consulta" "Consulta Registros"
            CALL f_consulta_registro("CONSULTA")

        COMMAND "Modifica" "Modifica Registros"
            CALL f_consulta_registro("MODIFICA")

        COMMAND "Elimina" "Elimina Registros"
            CALL f_consulta_registro("ELIMINA")

        COMMAND "Salir" "Salir del programa"
            CALL inicializa()
            EXIT MENU
    END MENU

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    LET HOY             = TODAY
    LET gc_usuario      = f_lib_obten_user()

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_estado.capturado
    FROM   ret_estado A
    WHERE  A.descripcion = "CAPTURADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_limpia_arr_montos : Inicializa y limpia el screen array principal       #
#---------------------------------------------------------------------------#
FUNCTION f_limpia_arr_montos()

    DEFINE
        ls_cont             SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE gar_montos[1].* TO NULL

    FOR ls_cont = 1 TO 500
        
        LET gar_montos[ls_cont].* = gar_montos[1].*
        
        IF ls_cont <= 5 THEN
            DISPLAY gar_montos[ls_cont].* TO scr_2[ls_cont].*
        END IF 
        
    END FOR
    
END FUNCTION

#---------------------------------------------------------------------------#
# inicializa : Limpia los registros de captura de la pantalla               #
#---------------------------------------------------------------------------#
FUNCTION inicializa()

    DEFINE
        ls_cont                 SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE gar_montos[1].* TO NULL
    INITIALIZE gar_datos[1].* TO NULL

    FOR ls_cont = 1 TO 50        
        LET gar_datos[ls_cont].* = gar_datos[1].*        
        DISPLAY gar_datos[ls_cont].* TO scr.*
    END FOR
    
--    DISPLAY gar_datos[1].* TO scr.*
    
    FOR ls_cont = 1 TO 5
        LET gar_montos[ls_cont].* = gar_montos[1].*        
        DISPLAY gar_montos[ls_cont].* TO scr_1[ls_cont].*
    END FOR

END FUNCTION

#---------------------------------------------------------------------------#
# f_agrega_sol :
#---------------------------------------------------------------------------#
FUNCTION f_agrega_sol()

    DEFINE lr_captura RECORD
        nss                     LIKE inf_his_oper97.nss                 ,
        nombre                  CHAR(40)                                ,
        opera_devolucion        CHAR(02)                                ,
        origen_devolucion       CHAR(03)                                ,
        desc_devolucion         CHAR(60)                                ,
        fecha_afectacion        LIKE inf_his_oper97.fecha_afectacion    ,
        cargo_abono             CHAR(01)
    END RECORD

    DEFINE lr_datos_afi RECORD
        nombres                 CHAR(040)   ,
        paterno                 CHAR(040)   ,
        materno                 CHAR(040)   ,
        curp                    CHAR(018)
    END RECORD

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_captura TO NULL
    DISPLAY BY NAME lr_captura.*

    LET lr_captura.nombre = "                                                     "
    DISPLAY lr_captura.nombre AT 5,35

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,71 ATTRIBUTE(REVERSE)
    DISPLAY " [ Ctrl-c ] Salir sin agregar registros" AT 2,1

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        -------------------
        AFTER FIELD nss
        -------------------
            IF lr_captura.nss IS NULL THEN
                CALL f_lib_error_msg("NUMERO DE SEGURIDAD SOCIAL NO PUEDE SER NULO")
                NEXT FIELD nss
            ELSE
                SELECT nombres   ,
                       paterno   ,
                       materno   ,
                       n_unico
                INTO   lr_datos_afi.*
                FROM   afi_mae_afiliado
                WHERE  n_seguro = lr_captura.nss

                IF STATUS = NOTFOUND THEN
                    CALL f_lib_error_msg("NUMERO DE SEGURIDAD SOCIAL INEXISTENTE")
                    NEXT FIELD nss
                ELSE
                    LET lr_captura.nombre = lr_datos_afi.nombres CLIPPED, " ",
                                            lr_datos_afi.paterno CLIPPED, " ",
                                            lr_datos_afi.materno CLIPPED
                    DISPLAY BY NAME lr_captura.nombre
                END IF
            END IF

        -------------------
        AFTER FIELD opera_devolucion
        -------------------
            IF lr_captura.opera_devolucion IS NULL THEN
                CALL f_lib_error_msg("CAMPO NO PUEDE SER NULO")
                NEXT FIELD opera_devolucion
            ELSE
                IF (lr_captura.opera_devolucion <> '98') AND
                   (lr_captura.opera_devolucion <> '97') THEN
                    CALL f_lib_error_msg("CAMPO UNICAMENTE PARA OP.98 O 97")
                    NEXT FIELD opera_devolucion
                END IF
            END IF

        -------------------
        AFTER FIELD origen_devolucion
        -------------------
            IF lr_captura.origen_devolucion IS NULL THEN
                CALL f_ayuda_tipo_devol() RETURNING lr_captura.origen_devolucion,
                                                    lr_captura.desc_devolucion

                DISPLAY BY NAME lr_captura.origen_devolucion,
                                lr_captura.desc_devolucion

                NEXT FIELD origen_devolucion
            ELSE
                LET lr_captura.origen_devolucion = lr_captura.origen_devolucion USING "&&&"

                SELECT desc
                INTO   lr_captura.desc_devolucion
                FROM   tab_tipo_devol
                WHERE  clave = lr_captura.origen_devolucion

                IF STATUS = NOTFOUND THEN
                    CALL f_lib_error_msg("EL TIPO DE DEVOLUCION NO EXISTE")
                    NEXT FIELD origen_devolucion
                ELSE
                    DISPLAY BY NAME lr_captura.desc_devolucion
                END IF
            END IF

        -------------------
        AFTER FIELD fecha_afectacion
        -------------------
            IF lr_captura.fecha_afectacion IS NULL THEN
                CALL f_lib_error_msg("FECHA AFECTA NO PUEDE SER NULA")
                NEXT FIELD fecha_afectacion
            END IF

            IF lr_captura.fecha_afectacion >= "08/01/2004" THEN
                DISPLAY "PARTICIPACIONES" AT 18,61 ATTRIBUTE(REVERSE)
            ELSE
                DISPLAY " *** PESOS *** " AT 18,61 ATTRIBUTE(REVERSE)
            END IF

        -------------------
        BEFORE FIELD cargo_abono
        -------------------
            LET lr_captura.cargo_abono = "N"

        -------------------
        AFTER FIELD cargo_abono
        -------------------
            IF lr_captura.cargo_abono IS NULL THEN
                CALL f_lib_error_msg("EL CAMPO NO PUEDE SER NULO")
                NEXT FIELD cargo_abono
            END IF

            IF (UPSHIFT(lr_captura.cargo_abono) <> "N")  AND
               (UPSHIFT(lr_captura.cargo_abono) <> "S")  THEN
                CALL f_lib_error_msg("PARA ESTE CAMPO UNICAMENTE PUEDE SER (N/S)")
                NEXT FIELD cargo_abono
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fecha_afectacion
            END IF

            CALL f_ingresa_detalle(lr_datos_afi.curp            ,
                                   lr_captura.nss               ,
                                   lr_captura.opera_devolucion  ,
                                   lr_captura.origen_devolucion ,
                                   lr_captura.cargo_abono       ,
                                   lr_captura.fecha_afectacion 
                                  ) 

            INITIALIZE lr_captura.* TO NULL
            DISPLAY BY NAME lr_captura.*
            NEXT FIELD nss

        -------------------
        ON KEY (CONTROL-C)
        -------------------
            CALL inicializa()
            EXIT INPUT

    END INPUT

END FUNCTION

#---------------------------------------------------------------------------#
# f_ingresa_detalle :
#---------------------------------------------------------------------------#
FUNCTION f_ingresa_detalle(pr_datos_cap)

    DEFINE pr_datos_cap RECORD
        curp                    LIKE inf_his_oper97.n_unico             ,
        nss                     LIKE inf_his_oper97.nss                 ,
        opera_devolucion        LIKE inf_his_oper97.opera_devolucion    ,
        origen_devolucion       LIKE inf_his_oper97.origen_devolucion   ,
        cargo_abono             LIKE inf_his_oper97.cargo_abono         ,
        fecha_afectacion        LIKE inf_his_oper97.fecha_afectacion
    END RECORD 


    DEFINE ld_precio_accion  LIKE glo_valor_accion.precio_del_dia

    DEFINE
        ciclo                   SMALLINT

    DEFINE
        ld_parti_dev            DECIMAL(18,6)

    -- -----------------------------------------------------------------------------

    DISPLAY "[ Esc ] Grabar Registros  [ Ctrl-c ] Salir sin agregar registros" AT 2,1

    INPUT ARRAY gar_montos WITHOUT DEFAULTS FROM scr_1.* ATTRIBUTE(REVERSE)

        -------------------
        BEFORE ROW
        -------------------
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

            --DISPLAY gar_montos[arr_c].* TO scr_1[scr_l].*
            DISPLAY "UD ESTA EN EL REGISTRO (",arr_c USING "&&#",")" AT 18,2

        -------------------
        AFTER FIELD tipo_viv
        -------------------
            IF gar_montos[arr_c].tipo_viv IS NULL OR
               (gar_montos[arr_c].tipo_viv != "92" AND gar_montos[arr_c].tipo_viv != "97")
            THEN
                ERROR "IMPORTE VIVIENDA SOLO PUEDE SER 92 o 97"
                NEXT FIELD tipo_viv
            END IF

        -------------------
        AFTER FIELD fecha_valor_trasp
        -------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD tipo_viv
            END IF

            IF gar_montos[arr_c].fecha_valor_trasp IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_valor_trasp
            END IF

            IF gar_montos[arr_c].fecha_valor_trasp < "07/02/1997" THEN
                ERROR "FECHA QUEBRANTO NO PUEDE SER MENOR QUE EL 02 JULIO DE 1997"
                NEXT FIELD fecha_valor_trasp
            END IF

            --IJR Validar si existe el precio de accion para este dia
            IF gar_montos[arr_c].fecha_valor_trasp >= "08/01/2004" THEN
                IF f_existe_precio(gar_montos[arr_c].fecha_valor_trasp, 11)=FALSE THEN
                    ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
                    NEXT FIELD fecha_valor_trasp
               END IF
            END IF

        -------------------
        AFTER FIELD fecha_valor_devol
        -------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD fecha_valor_trasp
            END IF

            IF gar_montos[arr_c].fecha_valor_devol IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_valor_devol
            END IF
            
            IF gar_montos[arr_c].fecha_valor_devol < "07/02/1997" THEN
                ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR QUE EL 02 JULIO DE 1997"
                NEXT FIELD fecha_valor_devol
            END IF
            
            IF gar_montos[arr_c].fecha_valor_devol < f_primer_dia_mes(TODAY) THEN
                ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR AL PRIMER DIA DEL MES ACTUAL "
                NEXT FIELD fecha_valor_devol
            END IF
                
            --IJR Validar si existe el precio de accion para este dis
            IF f_existe_precio(gar_montos[arr_c].fecha_valor_devol,11)=FALSE THEN
                ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
                NEXT FIELD fecha_valor_devol
            END IF

            IF gar_montos[arr_c].fecha_valor_trasp >= "08/01/2004" THEN
               NEXT FIELD mto_parti_dev
            ELSE
               NEXT FIELD mto_pesos_orig
            END IF

        -------------------
        AFTER FIELD mto_pesos_orig
        -------------------
            -- Se calcula equivalencia en Participaciones a partir de pesos
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFTT") THEN
                NEXT FIELD fecha_valor_devol
            END IF

            CALL f_calcula_participaciones(gar_montos[arr_c].fecha_valor_trasp,
                                           gar_montos[arr_c].fecha_valor_devol,
                                           gar_montos[arr_c].mto_pesos_orig
                                          )
                RETURNING gar_montos[arr_c].mto_parti_dev,
                          gar_montos[arr_c].mto_pesos_dev ,
                          gar_montos[arr_c].int_pesos_dev
            
            NEXT FIELD mto_pesos_dev

        -------------------
        AFTER FIELD mto_parti_dev
        -------------------
            CALL f_calcula_pesos(gar_montos[arr_c].fecha_valor_trasp    ,
                                 gar_montos[arr_c].fecha_valor_devol    ,
                                 gar_montos[arr_c].mto_parti_dev
                                )
                RETURNING gar_montos[arr_c].mto_pesos_orig,
                          gar_montos[arr_c].mto_pesos_dev ,
                          gar_montos[arr_c].int_pesos_dev

            NEXT FIELD mto_pesos_dev

        -------------------
        AFTER ROW
        -------------------
            DISPLAY gar_montos[arr_c].* TO scr_1[scr_l].* ATTRIBUTE(REVERSE)

        -------------------
        ON KEY (ESC)
        -------------------
            IF f_esta_seguro() = TRUE THEN
                MESSAGE " AGREGANDO REGISTROS" 
                SLEEP 1
                
                FOR ciclo = 1 TO 500
                    IF gar_montos[ciclo].tipo_viv           IS NOT NULL AND
                       gar_montos[ciclo].fecha_valor_trasp  IS NOT NULL AND
                       gar_montos[ciclo].fecha_valor_devol  IS NOT NULL AND
                       gar_montos[ciclo].mto_pesos_orig     IS NOT NULL AND
                       gar_montos[ciclo].mto_parti_dev      IS NOT NULL AND
                       gar_montos[ciclo].mto_pesos_dev      IS NOT NULL AND
                       gar_montos[ciclo].int_pesos_dev      IS NOT NULL THEN

                        CALL precio_del_dia(gar_montos[ciclo].fecha_valor_devol,11)
                            RETURNING ld_precio_accion

                        LET ld_parti_dev = ( (gar_montos[ciclo].mto_parti_dev * ld_precio_accion ) - gar_montos[ciclo].mto_pesos_orig) / ld_precio_accion

                        CALL f_inserta_inf_his_oper97(pr_datos_cap.nss                      ,
                                                      pr_datos_cap.curp                     ,
                                                      pr_datos_cap.opera_devolucion         ,
                                                      pr_datos_cap.origen_devolucion        ,
                                                      gar_montos[ciclo].tipo_viv            ,
                                                      gar_montos[ciclo].mto_pesos_orig      ,
                                                      gar_montos[ciclo].mto_pesos_dev       ,
                                                      gar_montos[ciclo].int_pesos_dev       ,
                                                      gar_montos[ciclo].mto_parti_dev       ,
                                                      ld_parti_dev                          ,
                                                      pr_datos_cap.cargo_abono              ,
                                                      pr_datos_cap.fecha_afectacion         ,
                                                      gar_montos[ciclo].fecha_valor_trasp   ,
                                                      gar_montos[ciclo].fecha_valor_devol
                                                     )
                    END IF
                END FOR
                MESSAGE "REGISTROS AGREGADOS CORRECTAMENTE"
            ELSE
                MESSAGE "NO SE AGREGARON REGISTROS"
            END IF
            
            SLEEP 1
            MESSAGE ""
            CALL inicializa()
            EXIT INPUT
        END INPUT

END FUNCTION

#---------------------------------------------------------------------------#
# f_consulta_registro : Permite establecer los criterios de consulta para   #
#                       las opciones de Consultar, Modificar o Eliminar     #
#                       cargando el arreglo con la informacion seleccionada #
#---------------------------------------------------------------------------#
FUNCTION f_consulta_registro(pc_parametro)

    DEFINE
        pc_parametro               CHAR(008)


    DEFINE txt_1     CHAR(400)
    DEFINE x_busca   CHAR(300)
    DEFINE sw_1      SMALLINT
    DEFINE cont_1    SMALLINT

    -- -----------------------------------------------------------------------------

    CALL inicializa()

    OPEN WINDOW infm0012 AT 4,2 WITH FORM "INFM0011" ATTRIBUTE(BORDER)
    DISPLAY " INFM001             DEVOLUCION AL INFONAVIT OPER.97 o OPER.98                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " ",pc_parametro," " AT 1,69 ATTRIBUTE(REVERSE)
    DISPLAY "[ ESC ] Consultar                            [ Ctrl-c ] Cancelar " AT 2,1

    INITIALIZE gar_datos[1].* TO NULL
    
    FOR cont_1 = 2 TO 50
        LET gar_datos[cont_1].*  = gar_datos[1].*
    END FOR 
    
    LET sw_1 = 0
    
    WHILE TRUE
        MESSAGE  "PROPORCIONE SU CRITERIO DE BUSQUEDA"  ATTRIBUTE(REVERSE)
        
        CONSTRUCT BY NAME x_busca ON nss                , 
                                     opera_devolucion   , 
                                     origen_devolucion  ,
                                     fecha_afectacion   ,
                                     cargo_abono
            ON KEY (ESC)
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT

            ON KEY (INTERRUPT)
                LET sw_1 = 1
                EXIT CONSTRUCT
        
        END CONSTRUCT
        
        MESSAGE  ""

        IF sw_1 = 1 THEN
            EXIT WHILE
        END IF

        LET txt_1 = " SELECT nss               ,",
                    "        ''                ,",
                    "        opera_devolucion  ,",
                    "        origen_devolucion ,",
                    "        fecha_afectacion  ,",
                    "        cargo_abono        ",
                    " FROM   inf_his_oper97     ",
                    " WHERE ",x_busca CLIPPED    ,
                    " AND estado = '0' "       ,
                    " GROUP BY 1,2,3,4,5,6"

        PREPARE pre_1 FROM txt_1
        DECLARE cur_2 CURSOR FOR pre_1

        LET cont_1 = 1
        
        FOREACH cur_2 INTO gar_datos[cont_1].*
            LET cont_1 = cont_1 + 1
        END FOREACH

        LET cont_1 = cont_1 - 1

        IF cont_1 = 0 THEN
            MESSAGE "NO HAY INFORMACION CON ESE CRITERIO"
            SLEEP 2
            MESSAGE ""
            CONTINUE WHILE
        END IF

        CALL SET_COUNT(cont_1)

        DISPLAY "[Enter] Ver detalle                          [ Ctrl-c ] Cancelar " AT 2,1

        DISPLAY ARRAY gar_datos TO scr.*

            ON KEY (CONTROL-M)
                LET arr_c = ARR_CURR()
                CASE pc_parametro
                    WHEN "CONSULTA"
                        CALL f_despliega_registro(gar_datos[arr_c].nss                  ,
                                                  gar_datos[arr_c].opera_devolucion     ,
                                                  gar_datos[arr_c].origen_devolucion    ,
                                                  gar_datos[arr_c].fecha_afectacion     ,
                                                  gar_datos[arr_c].cargo_abono
                                                 )
                        IF INT_FLAG THEN
                            LET INT_FLAG = FALSE
                            EXIT DISPLAY
                        END IF

                    WHEN "MODIFICA"
                        CALL f_modifica_detalle(gar_datos[arr_c].nss               ,
                                                gar_datos[arr_c].opera_devolucion  ,
                                                gar_datos[arr_c].origen_devolucion ,
                                                gar_datos[arr_c].fecha_afectacion  ,
                                                gar_datos[arr_c].cargo_abono
                                               )
                        EXIT DISPLAY

                    WHEN "ELIMINA"
                        CALL f_elimina_detalle(gar_datos[arr_c].nss               ,
                                               gar_datos[arr_c].opera_devolucion  ,
                                               gar_datos[arr_c].origen_devolucion ,
                                               gar_datos[arr_c].fecha_afectacion  ,
                                               gar_datos[arr_c].cargo_abono
                                              )
                        EXIT DISPLAY
                END CASE

            ON KEY (INTERRUPT)
                EXIT DISPLAY
        END DISPLAY 
    END WHILE

    CALL inicializa()

    CLOSE WINDOW infm0012

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_registro : Despliega el registro a consultar                  #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_registro(pr_registro)

    DEFINE pr_registro RECORD
        nss                     LIKE inf_his_oper97.nss                 ,
        opera_devolucion        CHAR(02)                                ,
        origen_devolucion       CHAR(03)                                ,
        fecha_afectacion        LIKE inf_his_oper97.fecha_afectacion    ,
        cargo_abono             CHAR(01)
    END RECORD

    DEFINE lar_montos ARRAY[500] OF RECORD                                      
        tipo_viv              LIKE inf_his_oper97.tipo_viv              ,
        fecha_valor_trasp     LIKE inf_his_oper97.fecha_valor_trasp     ,
        fecha_valor_devol     LIKE inf_his_oper97.fecha_valor_devol     ,
        mto_pesos_orig        LIKE inf_his_oper97.mto_pesos_orig        ,
        mto_parti_dev         LIKE inf_his_oper97.mto_parti_dev         ,
        mto_pesos_dev         LIKE inf_his_oper97.mto_pesos_dev         ,
        int_pesos_dev         LIKE inf_his_oper97.int_pesos_dev
    END RECORD

    DEFINE 
        cont_1              ,
        ciclo               SMALLINT

    -- -----------------------------------------------------------------------------

    DECLARE cur_1 CURSOR FOR
    SELECT tipo_viv          ,
           fecha_valor_trasp ,
           fecha_valor_devol ,
           mto_pesos_orig    ,
           mto_parti_dev     ,
           mto_pesos_dev     ,
           int_pesos_dev
    FROM   inf_his_oper97
    WHERE  nss               = pr_registro.nss
    AND    opera_devolucion  = pr_registro.opera_devolucion
    AND    origen_devolucion = pr_registro.origen_devolucion
    AND    fecha_afectacion  = pr_registro.fecha_afectacion

    LET ciclo = 1

    FOREACH cur_1 INTO lar_montos[ciclo].*
        LET ciclo = ciclo + 1
    END FOREACH

    CALL SET_COUNT(ciclo-1)

    DISPLAY "[ Esc ] Salir                                [ Ctrl-c ] Cancelar " AT 2,1

    DISPLAY ARRAY lar_montos TO scr_2.* ATTRIBUTE(REVERSE)
    
        ON KEY ( ESC )
           EXIT DISPLAY
        
        ON KEY ( INTERRUPT )
            EXIT DISPLAY
    
    END DISPLAY

    --Limpia el arreglo de detalles en pantalla
    CALL f_limpia_arr_montos()

    --Restaura el mensaje anterior
    DISPLAY "[Enter] Ver detalle                          [ Ctrl-c ] Cancelar " AT 2,1

END FUNCTION

#---------------------------------------------------------------------------#
# f_modifica_detalle : Modifica un registro capturado                       #
#---------------------------------------------------------------------------#
FUNCTION f_modifica_detalle(pr_modifica)

    DEFINE pr_modifica RECORD
        nss                         LIKE inf_his_oper97.nss               ,
        opera_devolucion            LIKE inf_his_oper97.opera_devolucion  ,
        origen_devolucion           LIKE inf_his_oper97.origen_devolucion ,
        fecha_afectacion            LIKE inf_his_oper97.fecha_afectacion  ,
        cargo_abono                 CHAR(01)
    END RECORD

    DEFINE 
        ls_cont                   SMALLINT

    -- -----------------------------------------------------------------------------
    
    CALL f_limpia_arr_montos()

    DECLARE cur_modifica CURSOR FOR
        SELECT tipo_viv          ,
               fecha_valor_trasp ,
               fecha_valor_devol ,
               mto_pesos_orig    ,
               mto_parti_dev     ,
               mto_pesos_dev     ,
               int_pesos_dev
        FROM   inf_his_oper97
        WHERE  nss               = pr_modifica.nss
        AND    fecha_afectacion  = pr_modifica.fecha_afectacion

    LET ls_cont = 1
    
    FOREACH cur_modifica INTO gar_montos[ls_cont].*
        LET ls_cont = ls_cont + 1
    END FOREACH

    LET ls_cont = ls_cont - 1
    CALL SET_COUNT(ls_cont)

    CALL f_ingresa_modifica(pr_modifica.*)

    CALL f_limpia_arr_montos()

END FUNCTION

#---------------------------------------------------------------------------#
# f_elimina_detalle : Elimina un registro capturado por el usuario          #
#---------------------------------------------------------------------------#
FUNCTION f_elimina_detalle(pr_elimina)

    DEFINE pr_elimina RECORD
        nss                     LIKE inf_his_oper97.nss               ,
        opera_devolucion        LIKE inf_his_oper97.opera_devolucion  ,
        origen_devolucion       LIKE inf_his_oper97.origen_devolucion ,
        fecha_afectacion        LIKE inf_his_oper97.fecha_afectacion  ,
        cargo_abono             CHAR(01)
    END RECORD

    DEFINE 
        sw_1                    ,
        cont_1                  ,
        ciclo                   SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY "[ Enter ] Eliminar Registro  [ Ctrl-c ] Salir sin eliminar registro"
            AT 2,1

    WHILE TRUE  -- Para que sean eliminados tantos registros se requieran
        CALL f_limpia_arr_montos() 

        DECLARE cur_elimina CURSOR FOR
            SELECT tipo_viv          ,
                   fecha_valor_trasp ,
                   fecha_valor_devol ,
                   mto_pesos_orig    ,
                   mto_parti_dev     ,
                   mto_pesos_dev     ,
                   int_pesos_dev
            FROM   inf_his_oper97
            WHERE  nss               = pr_elimina.nss
            AND    opera_devolucion  = pr_elimina.opera_devolucion
            AND    origen_devolucion = pr_elimina.origen_devolucion
            AND    fecha_afectacion  = pr_elimina.fecha_afectacion

        LET ciclo = 1
       
        FOREACH cur_elimina INTO gar_montos[ciclo].*
            LET ciclo = ciclo + 1
        END FOREACH
        
        LET ciclo = ciclo-1
        
        CALL SET_COUNT(ciclo)
        
        IF ciclo = 0 THEN
           MESSAGE  "NO HAY MAS REGISTROS POR ELIMINAR"
           SLEEP 2
           MESSAGE  ""
           EXIT WHILE
        END IF
        
        LET sw_1 = 0
        
        DISPLAY ARRAY gar_montos TO scr_2.* ATTRIBUTE(REVERSE)
            
            ON KEY (CONTROL-M)
                LET arr_c = ARR_CURR()
        
                IF f_esta_seguro() = TRUE THEN
                    --Elimina el Registro Actual
                    DELETE 
                    FROM   inf_his_oper97
                    WHERE  nss              = pr_elimina.nss
                    AND    fecha_afectacion = pr_elimina.fecha_afectacion
                    AND    mto_pesos_orig   = gar_montos[arr_c].mto_pesos_orig
                    AND    mto_parti_dev    = gar_montos[arr_c].mto_parti_dev
                    
                    IF SQLCA.SQLCODE != 0 THEN
                        ERROR "NO PUDO ELIMINARSE EL REGISTRO, VERIFIQUE"
                    ELSE
                        MESSAGE "REGISTRO ELIMINADO ..."
                        SLEEP 2
                        MESSAGE ""
                    END IF
                   
                    INITIALIZE gar_datos TO NULL
                    EXIT DISPLAY
                END IF
            
            ON KEY (INTERRUPT)
                LET sw_1 = 1
                EXIT DISPLAY
        
        END DISPLAY
        
        INITIALIZE gar_montos TO NULL
        
        IF sw_1 = 1 THEN
            EXIT WHILE
        END IF
    END WHILE

    CALL f_limpia_arr_montos() #i3  -- Limpia el arreglo de detalles

END FUNCTION

#---------------------------------------------------------------------------#
# f_ingresa_modifica : Captura los datos que se modificaran en la solicitud #
#---------------------------------------------------------------------------#
FUNCTION f_ingresa_modifica(pr_datos_modif)

    DEFINE pr_datos_modif RECORD
        nss                         LIKE inf_his_oper97.nss               ,
        opera_devolucion            LIKE inf_his_oper97.opera_devolucion  ,
        origen_devolucion           LIKE inf_his_oper97.origen_devolucion ,
        fecha_afectacion            LIKE inf_his_oper97.fecha_afectacion  ,
        cargo_abono                 CHAR(01)
    END RECORD

    DEFINE ld_precio_accion      LIKE glo_valor_accion.precio_del_dia
    DEFINE lc_curp              LIKE inf_his_oper97.n_unico

    DEFINE 
        j                       ,
        ciclo                   SMALLINT

    DEFINE 
        ld_parti_dev            DECIMAL(18,6)

    -- -----------------------------------------------------------------------------

    DISPLAY "[ Esc ] Grabar Registros  [ Ctrl-c ] Salir sin agregar registros"
            AT 2,1

    INPUT ARRAY gar_montos WITHOUT DEFAULTS FROM scr_2.* ATTRIBUTE(REVERSE)
    
        -------------------    
        BEFORE ROW
        -------------------
            LET arr_c = ARR_CURR()
            LET scr_l = SCR_LINE()

            DISPLAY "UD ESTA EN EL REGISTRO (",arr_c USING "&&#",")" AT 18,2

        -------------------
        AFTER FIELD tipo_viv
        -------------------
            IF gar_montos[arr_c].tipo_viv IS NULL OR 
              (gar_montos[arr_c].tipo_viv != "92" AND gar_montos[arr_c].tipo_viv != "97") 
            THEN
                ERROR "IMPORTE VIVIENDA SOLO PUEDE SER 92 o 97"
                NEXT FIELD tipo_viv
            END IF

        -------------------
        AFTER FIELD fecha_valor_trasp
        -------------------
            IF gar_montos[arr_c].fecha_valor_trasp IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_valor_trasp
            END IF
            
            IF gar_montos[arr_c].fecha_valor_trasp < "07/02/1997" THEN
                ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR QUE EL 02 JULIO DE 1997"
                NEXT FIELD fecha_valor_trasp
            END IF
            
            --IJR Validar si existe el precio de accion para este dia
            IF gar_montos[arr_c].fecha_valor_trasp >= "08/01/2004" THEN
                IF f_existe_precio(gar_montos[arr_c].fecha_valor_trasp,11)=FALSE THEN
                    ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
                    NEXT FIELD fecha_valor_trasp
                END IF
            END IF

        -------------------
        AFTER FIELD fecha_valor_devol
        -------------------
            IF gar_montos[arr_c].fecha_valor_devol IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_valor_devol
            END IF
            
            IF gar_montos[arr_c].fecha_valor_devol < "07/02/1997" THEN
                ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR QUE EL 02 JULIO DE 1997"
                NEXT FIELD fecha_valor_devol
            END IF
            
            IF gar_montos[arr_c].fecha_valor_devol < f_primer_dia_mes(TODAY) THEN
                ERROR "FECHA DEVOLUCION NO PUEDE SER MENOR AL PRIMER DIA DEL MES ACTUAL "
                NEXT FIELD fecha_valor_devol
            END IF
            
            --IJR Validar si existe el precio de accion para este dis
            IF f_existe_precio(gar_montos[arr_c].fecha_valor_devol,11) = FALSE THEN
                ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
                NEXT FIELD fecha_valor_devol
            END IF

            IF gar_montos[arr_c].fecha_valor_trasp >= MDY(08,01,2004) THEN
                NEXT FIELD mto_parti_dev
            ELSE
                NEXT FIELD mto_pesos_orig
            END IF

        -------------------
         AFTER FIELD mto_pesos_orig
        -------------------
            -- Se calcula equivalencia en Participaciones a partir de pesos
            CALL f_calcula_participaciones(gar_montos[arr_c].fecha_valor_trasp  ,
                                           gar_montos[arr_c].fecha_valor_devol  ,
                                           gar_montos[arr_c].mto_pesos_orig
                                          )
                RETURNING gar_montos[arr_c].mto_parti_dev,
                          gar_montos[arr_c].mto_pesos_dev ,
                          gar_montos[arr_c].int_pesos_dev

            NEXT FIELD mto_pesos_dev

        -------------------
        AFTER FIELD mto_parti_dev
        -------------------
            -- Se calcula equivalencia en Pesos a partir de Participaciones
            CALL f_calcula_pesos(gar_montos[arr_c].fecha_valor_trasp    , 
                                 gar_montos[arr_c].fecha_valor_devol    ,
                                 gar_montos[arr_c].mto_parti_dev
                                )
                RETURNING gar_montos[arr_c].mto_pesos_orig,
                          gar_montos[arr_c].mto_pesos_dev ,
                          gar_montos[arr_c].int_pesos_dev

            NEXT FIELD mto_pesos_dev

        -------------------
        AFTER ROW
        -------------------
            DISPLAY gar_montos[arr_c].* TO scr_1[scr_l].* ATTRIBUTE(REVERSE)

        -------------------
        ON KEY (ESC)
        -------------------
        IF f_esta_seguro() = TRUE THEN
            MESSAGE " MODIFICANDO REGISTROS" SLEEP 1

            SELECT n_unico
            INTO   lc_curp
            FROM   inf_his_oper97
            WHERE  nss               = pr_datos_modif.nss
            AND    opera_devolucion  = pr_datos_modif.opera_devolucion
            AND    origen_devolucion = pr_datos_modif.origen_devolucion
            AND    fecha_afectacion  = pr_datos_modif.fecha_afectacion
            GROUP BY 1

            DELETE
            FROM   inf_his_oper97
            WHERE  nss               = pr_datos_modif.nss
            AND    opera_devolucion  = pr_datos_modif.opera_devolucion
            AND    origen_devolucion = pr_datos_modif.origen_devolucion
            AND    fecha_afectacion  = pr_datos_modif.fecha_afectacion

            MESSAGE "APLICANDO CAMBIOS"

            FOR ciclo = 1 TO 100
                IF gar_montos[ciclo].tipo_viv           IS NOT NULL AND
                   gar_montos[ciclo].fecha_valor_trasp  IS NOT NULL AND
                   gar_montos[ciclo].fecha_valor_devol  IS NOT NULL AND
                   gar_montos[ciclo].mto_pesos_orig     IS NOT NULL AND
                   gar_montos[ciclo].mto_parti_dev      IS NOT NULL AND
                   gar_montos[ciclo].mto_pesos_dev      IS NOT NULL AND
                   gar_montos[ciclo].int_pesos_dev      IS NOT NULL THEN

                    CALL precio_del_dia(gar_montos[ciclo].fecha_valor_devol,11)
                        RETURNING ld_precio_accion

                    LET ld_parti_dev = gar_montos[ciclo].int_pesos_dev/ld_precio_accion

                    CALL f_inserta_inf_his_oper97(pr_datos_modif.nss                    ,
                                                  lc_curp                               ,
                                                  pr_datos_modif.opera_devolucion       ,
                                                  pr_datos_modif.origen_devolucion      ,
                                                  gar_montos[ciclo].tipo_viv            ,
                                                  gar_montos[ciclo].mto_pesos_orig      ,
                                                  gar_montos[ciclo].mto_pesos_dev       ,
                                                  gar_montos[ciclo].int_pesos_dev       ,
                                                  gar_montos[ciclo].mto_parti_dev       ,
                                                  ld_parti_dev                          ,
                                                  pr_datos_modif.cargo_abono            ,
                                                  pr_datos_modif.fecha_afectacion       ,
                                                  gar_montos[ciclo].fecha_valor_trasp   ,
                                                  gar_montos[ciclo].fecha_valor_devol
                                                 )
                END IF
            END FOR
            
            SLEEP 2
            MESSAGE ""
        ELSE
            MESSAGE "NO SE AGREGARON REGISTROS"
        END IF
        
        SLEEP 1
        MESSAGE ""
        CALL inicializa()
        EXIT INPUT
    
    END INPUT

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_inf_his_oper97 : Inserta un registro en la tabla inf_his_oper97 #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_inf_his_oper97(pr_reg_captura)

    DEFINE pr_reg_captura RECORD 
        nss                 LIKE inf_his_oper97.nss                 ,
        curp                LIKE inf_his_oper97.n_unico             ,
        opera_devolucion    LIKE inf_his_oper97.opera_devolucion    ,
        origen_devolucion   LIKE inf_his_oper97.origen_devolucion   ,          
        tipo_viv            LIKE inf_his_oper97.tipo_viv            ,
        mto_pesos_orig      LIKE inf_his_oper97.mto_pesos_orig      ,
        mto_pesos_dev       LIKE inf_his_oper97.mto_pesos_dev       ,
        int_pesos_dev       LIKE inf_his_oper97.int_pesos_dev       ,
        mto_parti_dev       LIKE inf_his_oper97.mto_parti_dev       ,
        int_parti_dev       LIKE inf_his_oper97.int_parti_dev       ,
        cargo_abono         LIKE inf_his_oper97.cargo_abono         ,   
        fecha_afectacion    LIKE inf_his_oper97.fecha_afectacion    ,   
        fecha_valor_trasp   LIKE inf_his_oper97.fecha_valor_trasp   ,
        fecha_valor_devol   LIKE inf_his_oper97.fecha_valor_devol
    END RECORD 

    DEFINE lr_inf_his_oper97 RECORD LIKE inf_his_oper97.*
        
    -- -----------------------------------------------------------------------------
    
    INITIALIZE lr_inf_his_oper97.* TO NULL        

    LET lr_inf_his_oper97.nss                   = pr_reg_captura.nss                    
    LET lr_inf_his_oper97.n_unico               = pr_reg_captura.curp                     
    LET lr_inf_his_oper97.opera_devolucion      = pr_reg_captura.opera_devolucion          
    LET lr_inf_his_oper97.origen_devolucion     = pr_reg_captura.origen_devolucion          
    LET lr_inf_his_oper97.tipo_viv              = pr_reg_captura.tipo_viv          
    LET lr_inf_his_oper97.mto_pesos_orig        = pr_reg_captura.mto_pesos_orig     
    LET lr_inf_his_oper97.mto_pesos_dev         = pr_reg_captura.mto_pesos_dev     
    LET lr_inf_his_oper97.int_pesos_dev         = pr_reg_captura.int_pesos_dev     
    LET lr_inf_his_oper97.mto_parti_dev         = pr_reg_captura.mto_parti_dev     
    LET lr_inf_his_oper97.int_parti_dev         = pr_reg_captura.int_parti_dev                      
    LET lr_inf_his_oper97.cargo_abono           = pr_reg_captura.cargo_abono            
    LET lr_inf_his_oper97.fecha_afectacion      = pr_reg_captura.fecha_afectacion          
    LET lr_inf_his_oper97.fecha_valor_trasp     = pr_reg_captura.fecha_valor_trasp     
    LET lr_inf_his_oper97.fecha_valor_devol     = pr_reg_captura.fecha_valor_devol     
    LET lr_inf_his_oper97.fecha_ult_proceso     = HOY                                     
    LET lr_inf_his_oper97.estado                = gr_estado.capturado                 
    LET lr_inf_his_oper97.usuario               = gc_usuario                          

    INSERT INTO inf_his_oper97
    VALUES (lr_inf_his_oper97.*)

END FUNCTION



-- REGRESA FECHA CON EL PRIMER DIA DEL MES Y ANO RECIBIDO EN LA FECHA
-- COMO ARGUMENTO

FUNCTION f_primer_dia_mes(vfecha )

   DEFINE vfecha DATE
   DEFINE vdia   SMALLINT
   DEFINE vmes   SMALLINT
   DEFINE vanio  SMALLINT

   LET vdia = 1
   LET vmes = MONTH(vfecha)
   LET vanio= YEAR(vfecha)

   LET vfecha = MDY(vmes,vdia,vanio)

   RETURN vfecha
END FUNCTION

{====================================================================}
{  CALCULA PARTICIPACIONES A PARTIR DE LOS PESOS                     }
{  Entra: pesos                                                      }
{  Sale : participaciones a devolver y pesos a devolver              }
{====================================================================}
FUNCTION f_calcula_participaciones(vfecha_valor_trasp,
                                 vfecha_valor_devol,
                                 vmto_pesos_orig)

   DEFINE vfecha_valor_trasp  LIKE inf_his_oper97.fecha_valor_trasp
   DEFINE vfecha_valor_devol  LIKE inf_his_oper97.fecha_valor_devol
   DEFINE vmto_pesos_orig     LIKE inf_his_oper97.mto_pesos_orig

   DEFINE vmto_parti_dev      LIKE inf_his_oper97.mto_parti_dev
   DEFINE vmto_pesos_dev      LIKE inf_his_oper97.mto_pesos_dev
   DEFINE vint_pesos_dev      LIKE inf_his_oper97.int_pesos_dev

   DEFINE ld_precio_accion     LIKE glo_valor_accion.precio_del_dia
   DEFINE vinst               CHAR(200)
   DEFINE vfecha_aux          DATE

   -- Obtendion del precio de la accion a la fecha de devolucion

   SELECT precio_del_dia
     INTO ld_precio_accion
     FROM glo_valor_accion
    WHERE fecha_valuacion = vfecha_valor_devol
      AND codigo_siefore  = 11

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "NO EXISTE PRECIO DE ACCION PARA ESTA FECHA"
      RETURN 0
   END IF

   --------------------------------------------------------------
   -- PASO 1
   -- Se calcula  correspondiente en participaciones
   -- Desde la fecha de traspaso hasta  el 01 de Agosto del 04
   --------------------------------------------------------------

   LET vfecha_aux = "08/01/2004"
   LET vinst = " EXECUTE PROCEDURE fn_cambia_aporte(?,?,?) "

   PREPARE exe_spl FROM vinst
   DECLARE cur_spl CURSOR FOR exe_spl
   OPEN cur_spl USING vmto_pesos_orig,
                      vfecha_valor_trasp,
                      vfecha_aux

   FETCH cur_spl INTO vmto_parti_dev
   CLOSE cur_spl

   -- Importes a Devolver

   LET vmto_pesos_dev = vmto_parti_dev * ld_precio_accion
   LET vint_pesos_dev = vmto_pesos_dev - vmto_pesos_orig

   RETURN vmto_parti_dev ,
          vmto_pesos_dev  ,
          vint_pesos_dev

END FUNCTION

{====================================================================}
{  CALCULA PESOS A PARTIR DE LAS PARTICIPACIONES                     }
{ Entran: Participaciones                                            }
{ salen : Pesos                                                      }
{====================================================================}
#cp
FUNCTION f_calcula_pesos(vfecha_valor_trasp,
                       vfecha_valor_devol,
                       vmto_parti_dev)

   DEFINE vfecha_valor_trasp  LIKE inf_his_oper97.fecha_valor_trasp
   DEFINE vfecha_valor_devol  LIKE inf_his_oper97.fecha_valor_devol
   DEFINE vmto_parti_dev      LIKE inf_his_oper97.mto_parti_dev

   DEFINE vmto_pesos_orig     LIKE inf_his_oper97.mto_pesos_orig
   DEFINE vmto_pesos_dev      LIKE inf_his_oper97.mto_pesos_dev
   DEFINE vint_pesos_dev      LIKE inf_his_oper97.int_pesos_dev


   LET vmto_pesos_orig = vmto_parti_dev * precio_del_dia(vfecha_valor_trasp,11)
   LET vmto_pesos_dev  = vmto_parti_dev * precio_del_dia(vfecha_valor_devol,11)
   LET vint_pesos_dev  = vmto_pesos_dev - vmto_pesos_orig

   RETURN vmto_pesos_orig ,
          vmto_pesos_dev  ,
          vint_pesos_dev

END FUNCTION

--=======================================================================
-- VALIDA SI EXISTE PRECIO DE LA ACCION PARA EL DIA Y SIEFORE RECIBIDOS
--=======================================================================
FUNCTION f_existe_precio(vfecha,vcodigo_siefore)

   DEFINE vfecha          DATE
   DEFINE vcodigo_siefore LIKE glo_valor_accion.codigo_siefore

   SELECT "X"
     FROM glo_valor_accion
    WHERE fecha_valuacion = vfecha
      AND codigo_siefore  = vcodigo_siefore

   IF SQLCA.SQLCODE = NOTFOUND THEN
      RETURN FALSE
   ELSE
      RETURN TRUE
   END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_esta_seguro : Pregunta si se desea o no realizar una accion             #
#---------------------------------------------------------------------------#
FUNCTION f_esta_seguro()

    DEFINE
        lc_resp             CHAR(1)

    DEFINE
        ls_respuesta        SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_respuesta = TRUE

    WHILE TRUE
        DISPLAY "                                 " AT 18,2
        
        PROMPT " ¿ESTA SEGURO S/N? " FOR lc_resp
        
        IF lc_resp MATCHES "[SsNn]" THEN
            IF lc_resp MATCHES "[Ss]" THEN
                LET ls_respuesta = TRUE
            ELSE
                LET ls_respuesta = FALSE
            END IF
        
            EXIT WHILE
        
        END IF
    
    END WHILE
    
    RETURN ls_respuesta

END FUNCTION



#---------------------------------------------------------------------------#
# f_ayuda_tipo_devol : Despliega una pantalla con los tipos de devolucion   #
#---------------------------------------------------------------------------#
FUNCTION f_ayuda_tipo_devol()

    DEFINE lar_tab_tipo_devol ARRAY[100] OF RECORD LIKE tab_tipo_devol.*

    DEFINE
        li_cont             ,
        li_pos              SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lar_tab_tipo_devol[1].* TO NULL

    FOR li_cont = 2 TO 100
        LET lar_tab_tipo_devol[li_cont].* = lar_tab_tipo_devol[1].*
    END FOR

    LET li_cont = 1

    DECLARE cur_tipo_devol CURSOR FOR
        SELECT *
        FROM   tab_tipo_devol
        ORDER BY 1

    FOREACH cur_tipo_devol INTO lar_tab_tipo_devol[li_cont].*
        LET li_cont = li_cont + 1
    END FOREACH

    LET li_cont = li_cont -1

    IF li_cont = 0 THEN
        RETURN FALSE
    END IF

    OPEN WINDOW INFM0012 AT 5,12 WITH FORM "INFM0012" ATTRIBUTE(FORM LINE 1, BORDER)

    CALL SET_COUNT(li_cont)

    DISPLAY ARRAY lar_tab_tipo_devol TO sa_tab_tipo_devol.*

        ON KEY(ESC)
            LET li_pos = ARR_CURR()
            EXIT DISPLAY

    END DISPLAY

    CLOSE WINDOW INFM0012

    RETURN lar_tab_tipo_devol[li_pos].*

END FUNCTION

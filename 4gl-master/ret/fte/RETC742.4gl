################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC742  => PRELIQUIDACION DE REINVERSION                            #
#Fecha creacion    => 09 DE NOVIEMBRE DE 2011                                  #
#Fecha Nvo Retiro  =>                                                          #
#By                => MAURICIO SANCHEZ VARGAS                                  #
#Fecha actualiz.   => 17-Feb-2012                                              #
#Actualizacion     => Se reestructura programa --Alejandro Chagoya S.          #
#Sistema           =>                                                          #
################################################################################
DATABASE safre_af

GLOBALS

DEFINE gr_edo RECORD
    provisionado,
    preliquidado    LIKE ret_estado_issste.estado_solicitud
END RECORD

DEFINE gr_folios RECORD
    fec_prel    DATE                         ,
    total       LIKE ret_sol_issste_tx.folio ,
    parcial     LIKE ret_sol_issste_tx.folio ,
    transfer    LIKE ret_sol_issste_tx.folio
END RECORD

DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
    estado                SMALLINT     ,
    fecha                 DATE         ,
    siefore               SMALLINT     ,
    precio_dia            DECIMAL(16,6)
END RECORD

DEFINE #glo #date
    HOY                   DATE

DEFINE g_enter                CHAR(001)
DEFINE  enter                 CHAR(001)
DEFINE  gs_usuario            CHAR(015)
DEFINE  gc_query              CHAR(1800)

DEFINE #glo #smallint
    gs_peiss              ,
    gs_flag               ,
    gs_flag_err           ,
    gs_sieviv             ,    
    gs_cod_afore          SMALLINT

DEFINE  gi_proceso            INTEGER
DEFINE  gs_ult_folio          INTEGER

--registro de la consulta de reinversion
DEFINE gr_tot RECORD
    sie                           LIKE dis_cuenta.siefore           ,
    des_sie                       CHAR(17)                          ,
    des_tip                       CHAR(38)                          ,
    des_scta_1                    CHAR(30)                          ,
    des_scta_2                    CHAR(30)                          ,
    des_scta_4                    CHAR(30)                          ,
    des_scta_5                    CHAR(30)                          ,
    des_scta_6                    CHAR(30)                          ,
    des_scta_9                    CHAR(30)                          ,
    des_scta_s                    CHAR(30)                          ,
    accion_1                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_1                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_2                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_2                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_4                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_4                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_5                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_5                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_6                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_6                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_9                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_9                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_s                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_s                       LIKE dis_cuenta.monto_en_pesos    
END RECORD

--arreglo de registros de la consulta de reinversion
DEFINE   gar_tot_com    ARRAY[20]     OF RECORD
    sie                           LIKE dis_cuenta.siefore           ,
    des_sie                       CHAR(17)                          ,
    des_tip                       CHAR(38)                          ,
    des_scta_1                    CHAR(30)                          ,
    des_scta_2                    CHAR(30)                          ,
    des_scta_4                    CHAR(30)                          ,
    des_scta_5                    CHAR(30)                          ,
    des_scta_6                    CHAR(30)                          ,
    des_scta_9                    CHAR(30)                          ,
    des_scta_s                    CHAR(30)                          ,
    accion_1                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_1                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_2                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_2                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_4                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_4                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_5                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_5                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_6                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_6                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_9                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_9                       LIKE dis_cuenta.monto_en_pesos    ,
    accion_s                      LIKE dis_cuenta.monto_en_acciones ,
    pesos_s                       LIKE dis_cuenta.monto_en_pesos    ,
    sie_ri                        LIKE dis_cuenta.siefore           ,
    des_sie_ri                    CHAR(17)                          ,
    des_tip_ri                    CHAR(38)                          ,
    des_scta_1_ri                 CHAR(30)                          ,
    des_scta_2_ri                 CHAR(30)                          ,
    des_scta_4_ri                 CHAR(30)                          ,
    des_scta_5_ri                 CHAR(30)                          ,
    des_scta_6_ri                 CHAR(30)                          ,
    des_scta_9_ri                 CHAR(30)                          ,
    des_scta_s_ri                 CHAR(30)                          ,
    accion_1_ri                   LIKE dis_cuenta.monto_en_acciones ,
    pesos_1_ri                    LIKE dis_cuenta.monto_en_pesos    ,
    accion_2_ri                   LIKE dis_cuenta.monto_en_acciones ,
    pesos_2_ri                    LIKE dis_cuenta.monto_en_pesos    ,
    accion_4_ri                   LIKE dis_cuenta.monto_en_acciones ,
    pesos_4_ri                    LIKE dis_cuenta.monto_en_pesos    ,
    accion_5_ri                   LIKE dis_cuenta.monto_en_acciones ,
    pesos_5_ri                    LIKE dis_cuenta.monto_en_pesos    ,
    accion_6_ri                   LIKE dis_cuenta.monto_en_acciones ,
    pesos_6_ri                    LIKE dis_cuenta.monto_en_pesos    ,
    accion_9_ri                   LIKE dis_cuenta.monto_en_acciones ,
    pesos_9_ri                    LIKE dis_cuenta.monto_en_pesos    ,
    accion_s_ri                   LIKE dis_cuenta.monto_en_acciones ,
    pesos_s_ri                    LIKE dis_cuenta.monto_en_pesos    
END RECORD

-- numero total de registros en el arreglo de la consulta de preliquidacion
DEFINE gi_num_registros SMALLINT

END GLOBALS

{=========================================================================
Clave:
Nombre: main
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:
Funcion principal de preliquidacion de reinversion


Parametros:
Entrada:
Ninguno

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
MAIN
 DEFER INTERRUPT
 OPTIONS
     INPUT WRAP         ,
     PROMPT LINE LAST   ,
     ACCEPT KEY CONTROL-I

 CALL STARTLOG("RETC742.log")

 CALL init() #i

 OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
 DISPLAY " RETC742    PRELIQUIDACION DE REINVERSION          " AT 3,1 ATTRIBUTE(REVERSE)
 DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

 MENU "PRELIQUIDACION"   
   COMMAND "Preliquida" "Ejecuta la Preliquidacion de Reinversión"
     CALL f_ejecuta_preliquidacion()

   COMMAND "Detalle X Siefore" "Consulta de Preliquidación a Reinvertir"
     CALL f_genera_detalle(HOY)
     CLEAR SCREEN
 
   COMMAND "Bitacora" "Consulta la Bitacora de Errores de Preliquidacion"
     CALL f_bitacora_err(0)
     CLEAR SCREEN
 
   COMMAND "Salir" "Salir del Programa "
     EXIT MENU
 END MENU
 
 -- se cierra la ventana
 CLOSE WINDOW main_win 

END MAIN


################################## FUNCIONES DE EJECUCION DE PRELIQUIDACION ##################
{=========================================================================
Clave:
Nombre: f_ejecuta_preliquidacion
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:
Funcion de ejecucion de la preliquidacion de reinversion

Parametros:
Entrada:
Ninguno

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION f_ejecuta_preliquidacion()
DEFINE folio, l_cuenta            INTEGER

LET l_cuenta = 0
 CALL init() #i
 
 -- se abre la ventana del proceso de preliquidacion de reinversion
 OPEN WINDOW RETC7421 AT 4,4 WITH FORM "RETC7421" ATTRIBUTE (BORDER)
 
 -- se despliegan las instrucciones en pantalla para el control del programa
 DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
 DISPLAY " RETC7421       PRELIQUIDACION DE REINVERSION                                  " AT 3,1 ATTRIBUTE(REVERSE)
 DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
 
 DISPLAY HOY TO fecha_preliq
 INPUT BY NAME folio WITHOUT DEFAULTS
 
     AFTER FIELD folio
         IF folio IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD folio
         END IF
        IF f_cuenta(folio) = 0 THEN
           ERROR "NO HAY REGISTROS CON ESTE FOLIO"
           NEXT FIELD folio
        END if	 
     ON KEY (CONTROL-C, INTERRUPT)
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR g_enter
         EXIT PROGRAM

     ON KEY (ESC)
       -- se obtiene el valor del folio  
         IF folio IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD folio
         END IF
       LET l_cuenta = f_cuenta(folio)
       WHILE TRUE
           PROMPT "SE PRELIQUIDARAN ", l_cuenta ," CUENTAS, ESTA SEGURO S/N ? " FOR CHAR g_enter
           IF g_enter MATCHES "[sSnN]" THEN
              IF g_enter MATCHES "[sS]" THEN
              	 CALL f_preliquida(HOY, folio)
                 EXIT INPUT
              ELSE
                 ERROR " PROCESO CANCELADO... "
                 NEXT FIELD folio
              END IF
           ELSE
           	  ERROR " SOLO PRESIONE S o N ..."
           END IF
       END WHILE
 END INPUT

       --registra los movimientos de preliquidacion de retiros
       CALL f_aplica_preliquidacion(folio)

 CLOSE WINDOW RETC7421

END FUNCTION

{=========================================================================
Clave:
Nombre: f_preliquida
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:

Genera la preliquidacion de reinversion para una fecha
y folio dados.

Se leen todos los datos de la tabla de provision que coincidan con el
conjunto de informacion buscado:
- folio asociado
- estatus de solicitud: 102 PROVISIONADO SALDO
- tipo de movimiento: 923 CARGO POR REINVERSION DE RECURSOS
- unicamente las subcuentas (1,2,6,9,5,7):
  1 - RETIRO
  2 - CESANTIA Y VEJEZ
  5 - CUOTA SOCIAL
  6 - ESTATAL
  7 - SEGURO RETIRO SAR 92
  9 - CUOTA ESPECIAL


Se calculan los montos para efectuar la preliquidacion
y los registros se guardan en la tabla temporal de preliquidacion
tmp_preliquida
- folio asociado: el mismo
- estatus de solicitud: 104
- tipo de movimiento: 924 REINVERSION DE RECURSOS
- fechas: fecha de preliquidacion

Parametros:
Entrada:
p_fecha_pre - fecha de la preliquidacion
p_folio     - folio de preliquidacion

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION f_preliquida(p_fecha_pre, p_folio_c)

DEFINE p_folio_c         INTEGER
DEFINE l_folio_a         INTEGER
DEFINE p_fecha_pre       DATE 
DEFINE lr_provi          RECORD LIKE dis_cuenta.*
DEFINE v_precio          LIKE glo_valor_accion.precio_del_dia

 -- se indica en pantalla que el proceso iniciara   
 --MESSAGE "PRELIQUIDANDO..." ATTRIBUTE ( REVERSE )
 
 LET l_folio_a  = ""
 SELECT folio_abono INTO l_folio_a 
 FROM ret_folio
 WHERE folio_cargo = p_folio_c
 AND estado_folio = gr_edo.provisionado
 
 IF STATUS = NOTFOUND THEN
 	   ERROR "ERROR, NO EXISTE FOLIO DE ABONO, VERIFIQUE "
 	    CLOSE WINDOW RETC7421
 	   EXIT PROGRAM
 END IF

DISPLAY "FOLIO ABONO               :", l_folio_a  AT 12,21 ATTRIBUTE(REVERSE)

 DECLARE cur_abono CURSOR FOR
 SELECT * FROM dis_provision
 WHERE folio = l_folio_a
 AND tipo_movimiento = 924

INITIALIZE lr_provi.* TO NULL

    FOREACH cur_abono INTO lr_provi.*
   
      -- ASIGNA DATOS GENERALES PARA REGISTRO DE PRELIQUIDACION
      -- fecha de preliquidacion para cada fecha y el usuario que la ejecuta
      LET lr_provi.fecha_valor        = p_fecha_pre
      LET lr_provi.fecha_pago         = p_fecha_pre
      LET lr_provi.fecha_conversion   = p_fecha_pre
      LET lr_provi.fecha_archivo      = p_fecha_pre
      LET lr_provi.fecha_proceso      = p_fecha_pre
      LET lr_provi.usuario            = gs_usuario
    
      --------------------------------------------------------------------
      -- PREPARA INSERCION DE MOVIMIENTO DE CARGO GENERADO POR LA PRELIQUIDACION
      -------------------------------------------------------------------- 
        --SE ACTUALIZAN LOS PESOS PROVISIONADOS CON EL PRECIO
        --DE LA FECHA DE PRELIQUIDACION
     IF (lr_provi.subcuenta != 4 AND lr_provi.subcuenta != 8) THEN
         LET v_precio = 0
         SELECT precio_del_dia INTO   v_precio
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = p_fecha_pre
         AND    codigo_siefore  = lr_provi.siefore
    
        IF STATUS = NOTFOUND THEN
           ERROR	"NO EXISTE PRECIO DE ACCION PARA LA FECHA: ",p_fecha_pre,
                  " Y LA SIEFORE: ",lr_provi.siefore
           SLEEP 2 ERROR ""
           EXIT PROGRAM
           CLOSE WINDOW RETC7421
        END IF
    
        LET lr_provi.precio_accion  = v_precio
        LET lr_provi.monto_en_acciones = lr_provi.monto_en_pesos / lr_provi.precio_accion
     END IF
      -- inserta el registro de preliquidacion en la tabla temporal
      INSERT INTO tmp_preliquida VALUES (lr_provi.*)
      
    END FOREACH -- Siguiente registro provisionado
 
    DECLARE cur_cargo CURSOR FOR
    SELECT * FROM dis_provision
    WHERE folio = p_folio_c
    AND tipo_movimiento = 923
   
   INITIALIZE lr_provi.* TO NULL 
   
   FOREACH cur_cargo INTO lr_provi.*
      LET lr_provi.fecha_valor        = p_fecha_pre
      LET lr_provi.fecha_pago         = p_fecha_pre
      LET lr_provi.fecha_conversion   = p_fecha_pre
      LET lr_provi.fecha_archivo      = p_fecha_pre
      LET lr_provi.fecha_proceso      = p_fecha_pre
      LET lr_provi.usuario            = gs_usuario

     INSERT INTO tmp_preliquida VALUES (lr_provi.*)

     UPDATE ret_reinversion SET estado_solicitud = gr_edo.preliquidado
     WHERE id_solicitud_saldo = lr_provi.consecutivo_lote
     AND estado_solicitud = gr_edo.provisionado

   END FOREACH
  
END FUNCTION

{=========================================================================
Clave:
Nombre: f_aplica_preliquidacion
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:
Funcion que aplica los montos preliquidados de la tabla temporal
tmp_preliquida en la tabla ret_preliquida

Parametros:
Entrada:
p_fecha_preliq - fecha de preliquidacion
p_folio - folio asociado

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION f_aplica_preliquidacion(p_folio)
DEFINE p_folio           INTEGER

 -- INSERTA MOVIMIENTOS DE PRELIQUIDACION en la tabla final
 INSERT INTO ret_preliquida
 SELECT *
 FROM   tmp_preliquida
 
 UPDATE ret_folio SET estado_folio = gr_edo.preliquidado
 WHERE folio_cargo = p_folio
 AND estado_folio = gr_edo.provisionado

 PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR g_enter
END FUNCTION

{=========================================================================
Clave:
Nombre: f_tablas_tmp
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:
Borra la tabla temporal en caso de que exista, para crearla con la estructura de la tabla dis_cuenta

Parametros:
Entrada:

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION f_tablas_tmp()

-- se borra la tabla temporal de preliquidacion
 WHENEVER ERROR CONTINUE
  DROP TABLE tmp_preliquida
 WHENEVER ERROR STOP

-- se crean la tabla temporal de preliquidacion con la misma estructura
-- que la tabla dis_cuenta
 SELECT * FROM   dis_cuenta
 WHERE  0 = 1
 INTO TEMP tmp_preliquida WITH NO LOG

END FUNCTION

################################### FUNCIONES DE CONSULTA DE PRELIQUIDACION #####################

{=========================================================================
Clave:
Nombre: init
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:

Parametros:
Entrada:
Ninguno

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION init()
#i-------------
DEFINE  lc_prepare           CHAR(300)
DEFINE  ls_dias_hab          SMALLINT

 LET HOY                 =  TODAY
 LET gs_sieviv           =  12
 LET ls_dias_hab         =  2

 SELECT codigo_afore,
        USER
 INTO   gs_cod_afore,
        gs_usuario
 FROM   tab_afore_local

 -- clave de estado de solicitud PROVISIONADO_REINVERSION
 SELECT A.estado_solicitud
 INTO   gr_edo.provisionado
 FROM   ret_estado A
 WHERE  A.descripcion MATCHES "*PROVISIONADO*REINVERSION*"

 -- clave de estado de solicitud PRELIQUIDADO_REINVERSION
 SELECT A.estado_solicitud
 INTO   gr_edo.preliquidado
 FROM   ret_estado A
 WHERE  A.descripcion MATCHES "*PRELIQUIDADO*REINVERSION*"

 LET  gc_query  =   'SELECT  razon_social       ',
                    '  FROM  tab_siefore_local  ',
                    ' WHERE  codigo_siefore = ? '
 PREPARE  p_sel_sie   FROM gc_query  

 LET  gc_query  =   'SELECT  subct_desc         ',
                    '  FROM  tab_subcuenta      ',
                    ' WHERE  subct_cod      = ? '
 PREPARE  p_sel_sub   FROM gc_query

CALL f_tablas_tmp()
END FUNCTION

{=========================================================================
Clave:
Nombre: f_despliega_info
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:

Parametros:
Entrada:
Ninguno

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
{FUNCTION f_despliega_info()
    DEFINE lr_info RECORD
        fecha_preliq        DATE    ,
        sel_tot             CHAR    ,
        sel_par             CHAR    ,
        sel_tran            CHAR    ,
        folio_tot           INTEGER ,
        folio_par           INTEGER ,
        folio_tran          INTEGER
    END RECORD

    DEFINE
        li_tot_prov        INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    OPEN WINDOW RETC7422 AT 4,4 WITH FORM "RETC7422" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7422      PRELIQUIDACION DE SOLICITUDES DE RETIROS                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    LET ls_flag              = 1
    LET lr_info.fecha_preliq = HOY

    -- DISPOSICIONES
    SELECT UNIQUE(folio),
           "x"
    INTO   lr_info.folio_tot,
           lr_info.sel_tot
    FROM   ret_sol_issste_tx
    WHERE  estado_solicitud = gr_edo.recibido

    IF STATUS = NOTFOUND THEN
        LET lr_info.folio_tot = 0
        LET lr_info.sel_tot   = " "
    END IF

    -- PARCIALES  (en desarrollo)
    SELECT UNIQUE(folio),
           "x"
    INTO   lr_info.folio_par,
           lr_info.sel_par
    FROM   ret_parcial_issste
    WHERE  estado_solicitud = gr_edo.recibido

    IF STATUS = NOTFOUND THEN
        LET lr_info.folio_par = 0
        LET lr_info.sel_par   = " "
    END IF

    -- TRANSFERENCIAS
    SELECT UNIQUE(folio),
           "x"
    INTO   lr_info.folio_tran,
           lr_info.sel_tran
    FROM   ret_trans_issste
    WHERE  estado_solicitud = gr_edo.enviado

    IF STATUS = NOTFOUND THEN
        LET lr_info.folio_tran  = 0
        LET lr_info.sel_tran    = " "
    END IF

    INPUT BY NAME lr_info.* WITHOUT DEFAULTS

        BEFORE INPUT
            IF (lr_info.sel_tot <> "x") AND (lr_info.sel_par <> "x") AND (lr_info.sel_tran <> "x") THEN
                PROMPT " NO EXISTEN REGISTROS PARA LIQUIDAR ... < ENTER > PARA SALIR " FOR CHAR enter
                LET ls_flag = 0
                EXIT INPUT
            END IF

        AFTER FIELD fecha_preliq
            IF lr_info.fecha_preliq IS NULL THEN
                ERROR " LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA ..."
                SLEEP 2
                ERROR " "
                NEXT FIELD fecha_preliq
            END IF

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_flag = 0
            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_flag = 0
            EXIT INPUT

        ON KEY (ESC)

            IF lr_info.sel_tot IS NULL THEN
                LET lr_info.sel_tot = " "
            END IF

            IF lr_info.sel_par IS NULL THEN
                LET lr_info.sel_par = " "
            END IF

            IF lr_info.sel_tran IS NULL THEN
                LET lr_info.sel_tran = " "
            END IF

            IF lr_info.sel_tot <> "x" THEN
                LET lr_info.folio_tot = 0
            END IF

            IF lr_info.sel_par <> "x" THEN
                LET lr_info.folio_par = 0
            END IF            

            IF lr_info.sel_tran <> "x" THEN
                LET lr_info.folio_tran = 0
            END IF            
            
            IF (lr_info.sel_tot <> "x") AND (lr_info.sel_par <> "x") AND (lr_info.sel_tran <> "x") THEN
                PROMPT " NO EXISTEN REGISTROS PARA LIQUIDAR ... < ENTER > PARA SALIR " FOR CHAR enter
                LET ls_flag = 0
                EXIT INPUT
            END IF

            IF lr_info.fecha_preliq IS NULL THEN
                ERROR " LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA ..."
                SLEEP 2
                ERROR " "
                NEXT FIELD fecha_preliq
            END IF

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_flag = 1
                        EXIT INPUT
                    ELSE
                        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
                        LET ls_flag = 0
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

    END INPUT
    
    CLOSE WINDOW RETC7422

    RETURN ls_flag              ,
           lr_info.fecha_preliq ,
           lr_info.folio_tot    ,
           lr_info.folio_par    ,
           lr_info.folio_tran

END FUNCTION
}#Se comenta ACS-17-Feb-2012

#------------------------------------------------------------------------------#
# Consulta la informacion de la bitacora de errores, ya sea por medio del menu #
# principal o de forma automatica al presentarse un error en la carga          #
#------------------------------------------------------------------------------#
FUNCTION f_bitacora_err(pi_proceso)

    DEFINE pi_proceso LIKE ret_bitacora_error.id_proceso

    DEFINE lar_bitacora ARRAY[5000] OF RECORD
        programa        LIKE ret_bitacora_error.programa     ,
        folio           LIKE ret_bitacora_error.folio        ,
        id_proceso      LIKE ret_bitacora_error.id_proceso   ,
        fecha_error     LIKE ret_bitacora_error.fecha_error  ,
        hora_error      LIKE ret_bitacora_error.hora_error   ,
        usuario         LIKE ret_bitacora_error.usuario      ,
        nss             LIKE ret_bitacora_error.nss          ,
        curp            LIKE ret_bitacora_error.curp         ,
        tipo_campo      LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo       LIKE ret_bitacora_error.nom_campo    ,
        valor_campo     LIKE ret_bitacora_error.valor_campo  ,
        id_error        LIKE ret_bitacora_error.id_error     ,
        desc_error      LIKE tab_ret_cod_error.descripcion
    END RECORD

    DEFINE  li_pos          INTEGER
    DEFINE  lc_where        CHAR(200)

    OPEN WINDOW RETC7423 AT 4,4 WITH FORM "RETC7423" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7423     BITACORA DE ERRORES DE PRELIQUIDACION                            " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 3,1
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    -- El proceso viene de la carga de archivo, por lo que se hace directa
    -- la carga del arreglo mediante el id de proceso
    IF pi_proceso <> 0 THEN
        LET gc_query =   "SELECT programa    ,",
                         "       folio       ,",
                         "       id_proceso  ,",
                         "       fecha_error ,",
                         "       hora_error  ,",
                         "       usuario     ,",
                         "       nss         ,",
                         "       curp        ,",
                         "       tipo_campo  ,",
                         "       nom_campo   ,",
                         "       valor_campo ,",
                         "       id_error    ,",
                         "       ' '          ",
                         "FROM   ret_bitacora_error ",
                         "WHERE  id_proceso = " , pi_proceso
    ELSE
        -- Se inicia el construct para obtener los datos de consulta
        -- para la bitacora
        LET int_flag = FALSE

        CONSTRUCT BY NAME lc_where ON  folio             ,
                                       fecha_error       ,
                                       usuario
            ON KEY (CONTROL-C)
                LET INT_FLAG = TRUE
                EXIT CONSTRUCT

            ON KEY ( ESC )
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT


        END CONSTRUCT

        IF INT_FLAG = TRUE THEN
            LET INT_FLAG = FALSE
            ERROR "  BUSQUEDA CANCELADA...  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW RETC7423
            RETURN
        END IF

        LET gc_query =   "SELECT programa    ,",
                         "       folio       ,",
                         "       id_proceso  ,",
                         "       fecha_error ,",
                         "       hora_error  ,",
                         "       usuario     ,",
                         "       nss         ,",
                         "       curp        ,",
                         "       tipo_campo  ,",
                         "       nom_campo   ,",
                         "       valor_campo ,",
                         "       id_error    ,",
                         "       ' '          ",
                         " FROM   ret_bitacora_error ",
                         " WHERE ", lc_where CLIPPED ,
                         " AND    programa = 'RETC742' ",
                         " ORDER BY id_proceso DESC "
    END IF

    PREPARE prp_err FROM gc_query
    DECLARE cur_err CURSOR FOR prp_err

    LET li_pos = 1

    FOREACH cur_err INTO lar_bitacora[li_pos].*

        SELECT descripcion
        INTO   lar_bitacora[li_pos].desc_error
        FROM   tab_ret_cod_error
        WHERE  id_error = lar_bitacora[li_pos].id_error

        LET li_pos = li_pos + 1

    END FOREACH

    INITIALIZE lar_bitacora[li_pos].* TO NULL

        IF (li_pos - 1) >= 1 THEN
            CALL SET_COUNT(li_pos - 1)

            DISPLAY ARRAY lar_bitacora TO scr_err.*

                ON KEY (INTERRUPT)
                    EXIT DISPLAY

            END DISPLAY

            CLEAR SCREEN
            CLOSE WINDOW RETX7083

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW RETC7423
        END IF

END FUNCTION

{=========================================================================
Clave:
Nombre: f_genera_detalle
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:


Parametros:
Entrada:
Ninguno

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION f_genera_detalle(pdt_fec_detalle)
DEFINE
 lr_montos        RECORD
   tipo_mov        LIKE dis_cuenta.tipo_movimiento   ,
   acciones        LIKE dis_cuenta.monto_en_acciones ,
   pesos           LIKE dis_cuenta.monto_en_pesos
 END RECORD,
 pdt_fec_detalle  DATE,
 ls_sie           SMALLINT

 -- se invoca la funcion que busca la informacion
 CALL Fnc_Filtro()
 
 -- se abre la ventana para mostrar los resultados
 OPEN WINDOW RETC7424 AT 2,2                                                WITH FORM "RETC7424"  ATTRIBUTE (BORDER)
 DISPLAY " <CTRL-C> SALIR                                                               " AT 1,1  ATTRIBUTE(REVERSE)
 DISPLAY " CONSULTA DE PRELIQUIDACION DE RECURSOS A REINVERTIR               " AT 3,1  ATTRIBUTE(REVERSE)
 DISPLAY HOY USING "DD-MM-YYYY"                                                            AT 3,65 ATTRIBUTE(REVERSE)
  
 IF ( gi_num_registros > 0 ) THEN
 
   -- se asigna el numero de registros encontrados
   CALL SET_COUNT(gi_num_registros)
   
   -- despliega la consulta   
   DISPLAY ARRAY gar_tot_com TO scr_det1.*
 END IF
 
 CLOSE WINDOW RETC7424 

END FUNCTION

{=========================================================================
Clave:
Nombre: fnc_filtro
Fecha Creacion: 08/11/2011
Autor: Ivan Vega Upgenia
Narrativa Del Proceso que Realiza:
Funcion principal del catalogo de dias de servicio

Parametros:
Entrada:
Ninguno

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION Fnc_Filtro()
DEFINE
 ld_fecha_conversion DATE,
 ls_sale             SMALLINT, 
 i                   SMALLINT,
 lar_folios          ARRAY[10] OF RECORD
   folio              LIKE ret_preliquida.folio, -- folio de solicitud
   num_solicitudes    INTEGER
 END RECORD,
 lb_continuar        SMALLINT -- booleana para contiuar la ejecucion de la consulta
 
 -- se asume que no se continuara con la ejecucion
 LET lb_continuar = FALSE

 OPEN WINDOW RETC7425 AT 4,4 WITH FORM "RETC7425" ATTRIBUTE (BORDER)
 DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1   ATTRIBUTE(REVERSE)
 DISPLAY " RETC7425        PRELIQUIDACION DE RECURSOS A REINVERTIR                      " AT 3,1   ATTRIBUTE(REVERSE)
 DISPLAY HOY USING "DD-MM-YYYY"                                                           AT 3,65  ATTRIBUTE(REVERSE)

 -- se toma como fecha por omision la fecha del dia
 LET ld_fecha_conversion = HOY

 -- Input de fecha y validacion 
 INPUT  ld_fecha_conversion  
 WITHOUT DEFAULTS 
 FROM  fecha_conversion
 
   ON KEY ( ESC )
     IF ( ld_fecha_conversion IS NULL ) THEN
       ERROR "Debe capturar una fecha"
       SLEEP 1
       NEXT FIELD fecha_conversion
     END IF
     
     -- se inicia el contador
     LET i = 1
     
     -- se prepara la busqueda de folios para la fecha dada
     LET   gc_query  =  ' SELECT  folio, COUNT(*) '        ,
                        '   FROM  ret_preliquida          ', 
                        '  WHERE  fecha_conversion  =  ?  ',
                        '    AND  tipo_movimiento   =  923',
                        '  GROUP BY 1' 
     PREPARE  p_sel_fecha        FROM  gc_query 
     DECLARE  d_sel_fecha  CURSOR FOR  p_sel_fecha 
     
     -- para cada folio encontrado
     FOREACH  d_sel_fecha USING ld_fecha_conversion
     INTO lar_folios[i].folio, lar_folios[i].num_solicitudes         
       -- se incrementa el indice
       LET  i  =  i + 1
     END FOREACH
     
     -- si no hubo registros
     IF ( (SQLCA.SQLCODE = NOTFOUND) OR (i = 1) ) THEN
       ERROR 'No hay información para la fecha solicitada'
       NEXT  FIELD fecha_conversion
     ELSE 
       CALL SET_COUNT(i - 1)
       LET lb_continuar = TRUE
     END IF
     EXIT  INPUT

   ON KEY (CONTROL-C)
     EXIT INPUT 
 END INPUT
 
 IF ( lb_continuar ) THEN
   DISPLAY ARRAY lar_folios
           TO  scr_det2.*         
   
     ON KEY (CONTROL-C)
        EXIT DISPLAY
   
     ON KEY (ESC) 
        LET  ls_Sale = 1
        -- se obtiene el indice del arreglo en donde estaba el cursor
        LET i = ARR_CURR()
        ERROR "Folio enviado: ", lar_folios[i].folio
        SLEEP 2
        -- se invoca la conformacion de la consulta
        CALL Fnc_Acumula1a(ld_fecha_conversion, lar_folios[i].folio)  
        EXIT DISPLAY
   END DISPLAY
   
 END IF -- lb_continuar
 
 CLOSE WINDOW RETC7425    
END FUNCTION 

{=========================================================================
Clave:
Nombre: Fnc_Acumula1a
Fecha Creacion: 09/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:
Llena el arreglo de despliegue para la consulta de preliquidacion
de reinversion

Parametros:
Entrada:
Fecha y Folio

Salida:
Ninguno

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION Fnc_Acumula1a(ld_fecha, ls_folio)
DEFINE  
 i          SMALLINT,
 ld_fecha   DATE,
 ls_folio   INTEGER,
 ls_sie     INTEGER,
 ls_subcta  INTEGER,
 ls_accion  DECIMAL(16,6),
 ls_pesos   DECIMAL(16,6),
 ls_nss     CHAR (11),
 ls_nss_aux CHAR (11)

 -- se inicia el contador del arreglo
 LET i = 1
 
 --se prepara la consulta de la siefore 
 LET  gc_query  =   "SELECT  UNIQUE siefore\n",
                    "  FROM  ret_preliquida\n",
                    " WHERE  fecha_conversion = '", ld_fecha, "'\n",
                    "   AND  folio = ", ls_folio,"\n",
                    "   AND  tipo_movimiento = 923","\n"
                    --"   AND  siefore = 2\n"
   
 PREPARE prp_siefore FROM gc_query
 DECLARE cur_siefore CURSOR FOR prp_siefore
 FOREACH cur_siefore INTO ls_sie
      
   -- se inicia el registro
   -- seccion tipo movimiento 923
   LET  gar_tot_com[i].sie         =  NULL
   LET  gar_tot_com[i].des_sie     =  NULL              
   LET  gar_tot_com[i].des_tip     =  NULL              
   LET  gar_tot_com[i].des_scta_1  =  NULL   
   LET  gar_tot_com[i].des_scta_2  =  NULL                         
   LET  gar_tot_com[i].des_scta_4  =  NULL   
   LET  gar_tot_com[i].des_scta_5  =  NULL             
   LET  gar_tot_com[i].des_scta_6  =  NULL              
   LET  gar_tot_com[i].des_scta_9  =  NULL   
   LET  gar_tot_com[i].des_scta_s  =  NULL                                                                                                  
   LET  gar_tot_com[i].accion_1    =  0
   LET  gar_tot_com[i].pesos_1     =  0                                 
   LET  gar_tot_com[i].accion_2    =  0
   LET  gar_tot_com[i].pesos_2     =  0               
   LET  gar_tot_com[i].accion_4    =  0       
   LET  gar_tot_com[i].pesos_4     =  0                                                    
   LET  gar_tot_com[i].accion_5    =  0       
   LET  gar_tot_com[i].pesos_5     =  0                                                    
   LET  gar_tot_com[i].accion_6    =  0                             
   LET  gar_tot_com[i].pesos_6     =  0                                                      
   LET  gar_tot_com[i].accion_9    =  0                      
   LET  gar_tot_com[i].pesos_9     =  0           
   LET  gar_tot_com[i].accion_s    =  0  
   LET  gar_tot_com[i].pesos_s     =  0
   
   
   --se llena el arreglo final para el registro 924           
   LET  gar_tot_com[i].sie_ri         =  NULL
   LET  gar_tot_com[i].des_sie_ri     =  NULL
   LET  gar_tot_com[i].des_tip_ri     =  NULL
   LET  gar_tot_com[i].des_scta_1_ri  =  NULL
   LET  gar_tot_com[i].des_scta_2_ri  =  NULL
   LET  gar_tot_com[i].des_scta_4_ri  =  NULL
   LET  gar_tot_com[i].des_scta_5_ri  =  NULL
   LET  gar_tot_com[i].des_scta_6_ri  =  NULL
   LET  gar_tot_com[i].des_scta_9_ri  =  NULL
   LET  gar_tot_com[i].des_scta_s_ri  =  NULL
   LET  gar_tot_com[i].accion_1_ri    =  0
   LET  gar_tot_com[i].pesos_1_ri     =  0
   LET  gar_tot_com[i].accion_2_ri    =  0
   LET  gar_tot_com[i].pesos_2_ri     =  0
   LET  gar_tot_com[i].accion_4_ri    =  0
   LET  gar_tot_com[i].pesos_4_ri     =  0
   LET  gar_tot_com[i].accion_5_ri    =  0
   LET  gar_tot_com[i].pesos_5_ri     =  0
   LET  gar_tot_com[i].accion_6_ri    =  0
   LET  gar_tot_com[i].pesos_6_ri     =  0
   LET  gar_tot_com[i].accion_9_ri    =  0
   LET  gar_tot_com[i].pesos_9_ri     =  0
   LET  gar_tot_com[i].accion_s_ri    =  0
   LET  gar_tot_com[i].pesos_s_ri     =  0
 
   --se prepara la consulta de la subcuenta
   LET  gc_query  =   "SELECT  UNIQUE subcuenta\n",
                      "  FROM  ret_preliquida\n",
                      " WHERE  fecha_conversion = '", ld_fecha, "'\n",
                      "   AND  folio = ", ls_folio,"\n",
                      "   AND  siefore = ", ls_sie,"\n",
                      "   AND  tipo_movimiento = 923\n"
   
   PREPARE prp_subcuenta FROM gc_query
   DECLARE cur_subcuenta CURSOR FOR prp_subcuenta
   FOREACH cur_subcuenta INTO ls_subcta
                                                    
     --se prepara la consulta para el movimiento 923
     LET  gc_query  =  "SELECT  siefore, subcuenta, nss,sum(monto_en_acciones), SUM(monto_en_pesos)\n",
                       "  FROM  ret_preliquida\n",
                       " WHERE  fecha_conversion = '", ld_fecha, "'\n",
                       "   AND  folio = ", ls_folio,"\n",
                       "   AND  tipo_movimiento  =  923\n",
                       "   AND  siefore = ",ls_sie,"\n",
                       "   AND  subcuenta = ", ls_subcta,"\n",
                       " GROUP  BY  1, 2,3\n",
                       " ORDER  BY  3,1, 2\n"            
     
   
       LET gr_tot.accion_1  = 0
       LET gr_tot.pesos_1   = 0
       LET gr_tot.accion_2  = 0
       LET gr_tot.pesos_2   = 0
       LET gr_tot.accion_4  = 0
       LET gr_tot.pesos_4   = 0
       LET gr_tot.accion_5  = 0
       LET gr_tot.pesos_5   = 0
       LET gr_tot.accion_6  = 0
       LET gr_tot.pesos_6   = 0
       LET gr_tot.accion_9  = 0
       LET gr_tot.pesos_9   = 0
       LET gr_tot.accion_s  = 0
       LET gr_tot.pesos_s   = 0 
       LET ls_accion        = 0
       LET ls_pesos         = 0    
     
     PREPARE prp_mov_923 FROM gc_query
     DECLARE cr_mov_923 CURSOR FOR prp_mov_923
          
     --para cada registro del movimiento 923
     FOREACH  cr_mov_923 
     INTO  ls_sie, ls_subcta, ls_nss, ls_accion, ls_pesos       
              
       -- se asigna la SIFORE leida
       LET  gr_tot.sie = ls_sie
       LET  gr_tot.des_sie                = Fnc_Siefore(ls_sie)           
       LET  gr_tot.des_tip                =  '923 - Cargo Reinversión de Recursos ' 
       --se obtienen las descripciones de las subcuentas
       LET  gr_tot.des_scta_1             =  Fnc_Subcta(1)                           
       LET  gr_tot.des_scta_2             =  Fnc_Subcta(2)                           
       LET  gr_tot.des_scta_4             =  Fnc_Subcta(5)                           
       LET  gr_tot.des_scta_5             =  Fnc_Subcta(6)                           
       LET  gr_tot.des_scta_6             =  Fnc_Subcta(9)                           
       LET  gr_tot.des_scta_9             =  Fnc_Subcta(7)                           
       LET  gr_tot.des_scta_s             =  'Subtotal Siefore '                     
       
       --se asignan los importes para cada subcuenta
       IF ls_subcta = 1 THEN
         LET  gr_tot.accion_1               =  ls_accion
         LET  gr_tot.pesos_1                =  ls_pesos
       END IF
       IF ls_subcta = 2 THEN
         LET  gr_tot.accion_2               = ls_accion
         LET  gr_tot.pesos_2                = ls_pesos
       END IF
       IF ls_subcta = 5 THEN
         LET  gr_tot.accion_4               = ls_accion
         LET  gr_tot.pesos_4                = ls_pesos 
       END IF
       IF ls_subcta = 6 THEN  
         LET  gr_tot.accion_5               = ls_accion
         LET  gr_tot.pesos_5                = ls_pesos 
       END IF  
       IF ls_subcta = 9 THEN  
         LET  gr_tot.accion_6               = ls_accion
         LET  gr_tot.pesos_6                = ls_pesos 
       END IF  
       IF ls_subcta = 7 THEN    
         LET  gr_tot.accion_9               = ls_accion
         LET  gr_tot.pesos_9                = ls_pesos 
       END IF  
       LET  gr_tot.accion_s               = gr_tot.accion_s + ls_accion 
       LET  gr_tot.pesos_s                = gr_tot.pesos_s  + ls_pesos
                  
        --se llena el arreglo final para el registro 923
       LET  gar_tot_com[i].sie         =  gr_tot.sie
       LET  gar_tot_com[i].des_sie     =  gr_tot.des_sie                 
       LET  gar_tot_com[i].des_tip     =  gr_tot.des_tip                 
       LET  gar_tot_com[i].des_scta_1  =  gr_tot.des_scta_1   
       LET  gar_tot_com[i].des_scta_2  =  gr_tot.des_scta_2                         
       LET  gar_tot_com[i].des_scta_4  =  gr_tot.des_scta_4   
       LET  gar_tot_com[i].des_scta_5  =  gr_tot.des_scta_5             
       LET  gar_tot_com[i].des_scta_6  =  gr_tot.des_scta_6              
       LET  gar_tot_com[i].des_scta_9  =  gr_tot.des_scta_9   
       LET  gar_tot_com[i].des_scta_s  =  gr_tot.des_scta_s                                                                                                  
       LET  gar_tot_com[i].accion_1    =  gar_tot_com[i].accion_1 + gr_tot.accion_1   
       LET  gar_tot_com[i].pesos_1     =  gar_tot_com[i].pesos_1  + gr_tot.pesos_1                                     
       LET  gar_tot_com[i].accion_2    =  gar_tot_com[i].accion_2 + gr_tot.accion_2   
       LET  gar_tot_com[i].pesos_2     =  gar_tot_com[i].pesos_2  + gr_tot.pesos_2                   
       LET  gar_tot_com[i].accion_4    =  gar_tot_com[i].accion_4 + gr_tot.accion_4          
       LET  gar_tot_com[i].pesos_4     =  gar_tot_com[i].pesos_4  + gr_tot.pesos_4                                                        
       LET  gar_tot_com[i].accion_5    =  gar_tot_com[i].accion_5 + gr_tot.accion_5          
       LET  gar_tot_com[i].pesos_5     =  gar_tot_com[i].pesos_5  + gr_tot.pesos_5                                                        
       LET  gar_tot_com[i].accion_6    =  gar_tot_com[i].accion_6 + gr_tot.accion_6                                
       LET  gar_tot_com[i].pesos_6     =  gar_tot_com[i].pesos_6  + gr_tot.pesos_6                                                          
       LET  gar_tot_com[i].accion_9    =  gar_tot_com[i].accion_9 + gr_tot.accion_9                         
       LET  gar_tot_com[i].pesos_9     =  gar_tot_com[i].pesos_9  + gr_tot.pesos_9               
       LET  gar_tot_com[i].accion_s    =  gar_tot_com[i].accion_s + gr_tot.accion_s     
       LET  gar_tot_com[i].pesos_s     =  gar_tot_com[i].pesos_s  + gr_tot.pesos_s    
     END FOREACH -- siefore, subcuenta, nss MOVIMIENTO 923
       
       
     --se prepara la consulta para el movimiento 924
     LET  gc_query  =  "SELECT  siefore, subcuenta, nss,sum(monto_en_acciones), SUM(monto_en_pesos)\n",
                       "  FROM  ret_preliquida\n",
                       " WHERE  fecha_conversion = '", ld_fecha, "'\n",
                       "   AND  folio = ", ls_folio,"\n",
                       "   AND  tipo_movimiento  =  923\n",
                       "   AND  siefore = ",ls_sie,"\n",
                       "   AND  subcuenta = ", ls_subcta,"\n",
                       " GROUP  BY  1, 2,3\n",
                       " ORDER  BY  3,1, 2\n"            
              
     LET gr_tot.accion_1  = 0
     LET gr_tot.pesos_1   = 0
     LET gr_tot.accion_2  = 0
     LET gr_tot.pesos_2   = 0
     LET gr_tot.accion_4  = 0
     LET gr_tot.pesos_4   = 0
     LET gr_tot.accion_5  = 0
     LET gr_tot.pesos_5   = 0
     LET gr_tot.accion_6  = 0
     LET gr_tot.pesos_6   = 0
     LET gr_tot.accion_9  = 0
     LET gr_tot.pesos_9   = 0
     LET gr_tot.accion_s  = 0
     LET gr_tot.pesos_s   = 0 
     LET ls_accion        = 0
     LET ls_pesos         = 0    
     
     PREPARE prp_mov_924 FROM gc_query
     DECLARE cr_mov_924 CURSOR FOR prp_mov_924
       
     --para cada registro del movimiento 923
     FOREACH  cr_mov_924
     INTO  ls_sie, ls_subcta, ls_nss, ls_accion, ls_pesos

       LET gr_tot.accion_s = 0
       LET gr_tot.pesos_s = 0 
       LET ls_accion = 0
       LET ls_pesos  = 0   
       
       -- la siefore de reinversion es la 10
       LET  gr_tot.sie = 10
       LET  gr_tot.des_sie              =  Fnc_Siefore(10)           
       LET  gr_tot.des_tip              =  '924 - Abono Reinversión de Recursos ' 
       --se obtienen las descripciones de las subcuentas
       LET  gr_tot.des_scta_1           =  Fnc_Subcta(1)                           
       LET  gr_tot.des_scta_2           =  Fnc_Subcta(2)                           
       LET  gr_tot.des_scta_4           =  Fnc_Subcta(5)                           
       LET  gr_tot.des_scta_5           =  Fnc_Subcta(6)                           
       LET  gr_tot.des_scta_6           =  Fnc_Subcta(9)                           
       LET  gr_tot.des_scta_9           =  Fnc_Subcta(7)                           
       LET  gr_tot.des_scta_s           =  'Subtotal Siefore '                     
        --se asignan los importes para cada subcuenta
        
       CALL f_monto_pesos_acciones_siefore10(ld_fecha, ls_folio, ls_subcta, ls_nss)
       RETURNING ls_accion, ls_pesos
        
       IF ls_subcta = 1 THEN
         LET  gr_tot.accion_1               =  ls_accion
         LET  gr_tot.pesos_1                =  ls_pesos
       END IF
       IF ls_subcta = 2 THEN
         LET  gr_tot.accion_2               =  ls_accion
         LET  gr_tot.pesos_2                =  ls_pesos
       END IF
       IF ls_subcta = 5 THEN
         LET  gr_tot.accion_4               =  ls_accion
         LET  gr_tot.pesos_4                =  ls_pesos 
       END IF
       IF ls_subcta = 6 THEN  
         LET  gr_tot.accion_5               =  ls_accion
         LET  gr_tot.pesos_5                =  ls_pesos 
       END IF  
       IF ls_subcta = 9 THEN  
         LET  gr_tot.accion_6               =  ls_accion
         LET  gr_tot.pesos_6                =  ls_pesos 
       END IF  
       IF ls_subcta = 7 THEN    
         LET  gr_tot.accion_9               =  ls_accion
         LET  gr_tot.pesos_9                =  ls_pesos 
       END IF  
       LET  gr_tot.accion_s             =  gr_tot.accion_s + ls_accion
       LET  gr_tot.pesos_s              =  gr_tot.pesos_s + ls_pesos
       
       --se llena el arreglo final para el registro 924           
       LET  gar_tot_com[i].sie_ri         =  gr_tot.sie
       LET  gar_tot_com[i].des_sie_ri     =  gr_tot.des_sie                 
       LET  gar_tot_com[i].des_tip_ri     =  gr_tot.des_tip      
       LET  gar_tot_com[i].des_scta_1_ri  =  gr_tot.des_scta_1   
       LET  gar_tot_com[i].des_scta_2_ri  =  gr_tot.des_scta_2   
       LET  gar_tot_com[i].des_scta_4_ri  =  gr_tot.des_scta_4   
       LET  gar_tot_com[i].des_scta_5_ri  =  gr_tot.des_scta_5   
       LET  gar_tot_com[i].des_scta_6_ri  =  gr_tot.des_scta_6   
       LET  gar_tot_com[i].des_scta_9_ri  =  gr_tot.des_scta_9   
       LET  gar_tot_com[i].des_scta_s_ri  =  gr_tot.des_scta_s                                                                                                  
       LET  gar_tot_com[i].accion_1_ri    =  gar_tot_com[i].accion_1_ri + gr_tot.accion_1   
       LET  gar_tot_com[i].pesos_1_ri     =  gar_tot_com[i].pesos_1_ri  + gr_tot.pesos_1                          
       LET  gar_tot_com[i].accion_2_ri    =  gar_tot_com[i].accion_2_ri + gr_tot.accion_2   
       LET  gar_tot_com[i].pesos_2_ri     =  gar_tot_com[i].pesos_2_ri  + gr_tot.pesos_2                          
       LET  gar_tot_com[i].accion_4_ri    =  gar_tot_com[i].accion_4_ri + gr_tot.accion_4   
       LET  gar_tot_com[i].pesos_4_ri     =  gar_tot_com[i].pesos_4_ri  + gr_tot.pesos_4                          
       LET  gar_tot_com[i].accion_5_ri    =  gar_tot_com[i].accion_5_ri + gr_tot.accion_5   
       LET  gar_tot_com[i].pesos_5_ri     =  gar_tot_com[i].pesos_5_ri  + gr_tot.pesos_5                          
       LET  gar_tot_com[i].accion_6_ri    =  gar_tot_com[i].accion_6_ri + gr_tot.accion_6   
       LET  gar_tot_com[i].pesos_6_ri     =  gar_tot_com[i].pesos_6_ri  + gr_tot.pesos_6                          
       LET  gar_tot_com[i].accion_9_ri    =  gar_tot_com[i].accion_9_ri + gr_tot.accion_9   
       LET  gar_tot_com[i].pesos_9_ri     =  gar_tot_com[i].pesos_9_ri  + gr_tot.pesos_9               
       LET  gar_tot_com[i].accion_s_ri    =  gar_tot_com[i].accion_s_ri + gr_tot.accion_s     
       LET  gar_tot_com[i].pesos_s_ri     =  gar_tot_com[i].pesos_s_ri  + gr_tot.pesos_s                                
       
       
     END FOREACH -- siefore, subcuenta, nss MOVIMIENTO 924
     
   END FOREACH -- siefore y subcuenta
         
   LET i = i + 1      
   
 END FOREACH-- siefore
 
 -- se asigna el numero total de registos que quedaron en el arreglo
 LET gi_num_registros = i - 1

END FUNCTION

{=========================================================================
Clave:
Nombre: f_monto_pesos_acciones_siefore10
Fecha Creacion: 15/11/2011
Autor: Mauricio Sanchez Upgenia
Narrativa Del Proceso que Realiza:
Llena el arreglo de despliegue para la consulta de preliquidacion
de reinversion para la siefore 10

Parametros:
Entrada:
Fecha de proceso, Folio, subcuenta y numero de seguridad social

Salida:
Monto en pesos y monto en acciones para el movimiento 924

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION f_monto_pesos_acciones_siefore10(ld_fecha, ls_folio, ls_subcta, ls_nss)
DEFINE
 ls_accion  DECIMAL(16,6), -- monto en acciones
 ls_pesos   DECIMAL(16,6), -- monto en pesos
 ld_fecha   DATE, -- fecha de consulta
 ls_folio   INTEGER, -- folio de solicitud
 ls_nss     CHAR (11), -- numero de seguridad social
 ls_subcta  INTEGER -- subcuenta de la siefore

 --LET  gc_query = " SELECT siefore, subcuenta, SUM(monto_en_acciones), SUM(monto_en_pesos)\n",
 LET  gc_query = " SELECT SUM(monto_en_acciones), SUM(monto_en_pesos)\n",
                 " FROM  ret_preliquida\n",
                 " WHERE  fecha_conversion = '", ld_fecha, "'\n",
                 " AND  folio = " , ls_folio ,"\n",
                 " AND  tipo_movimiento  =  924\n",
                 " AND  nss = '", ls_nss,"'\n",                           
                 " AND  subcuenta = ", ls_subcta,"\n",
                 " AND  siefore = 10\n"
                  
 PREPARE prp_mov_924m FROM gc_query
 EXECUTE prp_mov_924m INTO  ls_accion, ls_pesos                
       
 -- se devuelven los montos encontrados
 RETURN ls_accion, ls_pesos
END FUNCTION


{=========================================================================
Clave:
Nombre: Fnc_Siefore
Fecha Creacion: 15/11/2011
Obtiene la descripcion de siefore

Parametros:
Entrada:
Clave de la siefore

Salida:
Descripcion de la siefore

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION Fnc_Siefore(ls_cve)
  DEFINE  ls_cve            INTEGER 
  DEFINE  lc_des            CHAR(40)

  EXECUTE p_sel_sie   USING  ls_cve
                       INTO  lc_des
  IF  STATUS = NOTFOUND   THEN 
      LET  lc_des   = 'No Hay SIEFORE'
  END IF 
  RETURN  lc_des
END FUNCTION 
 

{=========================================================================
Clave:
Nombre: Fnc_Subcta
Fecha Creacion: 15/11/2011
Obtiene la descripcion de una subcuenta

Parametros:
Entrada:
clave de la subcuenta

Salida:
Descripcion de la subcuenta

Registro de Modificaciones:
Autor                 Fecha                         Descrip. cambio

=========================================================================}
FUNCTION Fnc_Subcta(ls_cve)
  DEFINE  ls_cve            INTEGER
  DEFINE  lc_des            CHAR(40)

  EXECUTE  p_sel_sub  USING  ls_cve 
                       INTO  lc_des 
  IF  STATUS =  NOTFOUND  THEN
      LET  lc_des  = 'No Hay SUBCUENTA '
  END IF 
  RETURN lc_des 
END FUNCTION 

##############################################################################
#Funcion que cuenta registros a preliquidar
FUNCTION f_cuenta(p_folio)
DEFINE p_folio, l_cuenta INTEGER

LET l_cuenta = 0

SELECT COUNT(UNIQUE consecutivo_lote) INTO l_cuenta
FROM dis_provision a
WHERE folio = p_folio
AND consecutivo_lote IN (SELECT id_solicitud_saldo FROM ret_reinversion
                         WHERE estado_solicitud = gr_edo.provisionado )
AND tipo_movimiento in (923)

 RETURN l_cuenta
END FUNCTION 

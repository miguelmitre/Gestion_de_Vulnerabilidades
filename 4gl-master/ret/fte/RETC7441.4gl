#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC717  => Reinversion Contingente                                   #
#Fecha creacion    => 03-Jul-2012                                               #
#By                => Alejandro Chagoya Salazar                                 #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                                                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        preliquidado        LIKE ret_estado.estado_solicitud    ,
        provisionado        LIKE ret_estado.estado_solicitud    ,
        rechazado           LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gar_precio_acc ARRAY [20] OF RECORD
        estado          SMALLINT     ,
        fecha           DATE         ,
        siefore         SMALLINT     ,
        precio_dia      DECIMAL(16,6)
    END RECORD

    DEFINE #glo #date
        gdt_fecha_viv           ,
        HOY                     DATE

    DEFINE #glo #char
        gc_tipo_ret             CHAR(001) ,
        enter                   CHAR(001) ,
        gs_usuario              CHAR(015)

    DEFINE #glo #smallint
        gs_grupo_sub            ,
        gs_tipo_op              ,
        gs_procesa              ,
        gs_sieviv               ,
        gs_num_siefores         , #-- Indica el numero de siefores que se usan actualmente
        gs_codigo_afore         SMALLINT

    DEFINE  m_folio,
            m_folio_abono,
            m_folio_cargo,
            m_cuantos,
            m_procesa          INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC7441.log")

    CALL f_tablas_tmp()
    CALL init() #i
    CALL f_obtiene_precios_accion(HOY)

    CALL f_despliega_info() RETURNING gs_procesa

    IF gs_procesa THEN
        CALL f_datos()     #-- Inserta datos a la temporal
        CALL f_reinv()     #-- reinversion de solicitudes
    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY
    LET gs_tipo_op          = 43
    LET gs_sieviv           = 11
    LET gs_grupo_sub        = 8     -- Grupo que paga las subcuentas asociadas
    LET m_cuantos           = 0
    LET m_folio             = 0
    LET m_folio_abono       = 0
    LET m_folio_cargo       = 0
    LET m_procesa           = 0

    ----- FECHA DE VIVIENDA -----
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_fecha_val(?) "
    PREPARE eje_fecha_viv FROM lc_prepare
    EXECUTE eje_fecha_viv USING HOY INTO gdt_fecha_viv
    LET lc_prepare = " "

   #PREPARE para desmarcar
   LET lc_prepare =""
   LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
   PREPARE eje_desmarca FROM lc_prepare

   #PREPARE para saldo
   LET lc_prepare =""
   LET lc_prepare = " EXECUTE PROCEDURE fn_saldo_dia ( ?,?,?,?)"
   PREPARE eje_saldo FROM lc_prepare

    #-- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore NOT IN (11,12,13)

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gs_usuario
    FROM   tab_afore_local

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud                     #104
    INTO   gr_edo.preliquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO SALDO"      

    SELECT A.estado_solicitud                     #102
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO SALDO"  

    SELECT A.estado_solicitud                    #110
    INTO   gr_edo.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO SALDO"    

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info :                                                        #
#                                                                           #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()
DEFINE  ls_flag   SMALLINT

  CALL f_abre_ventana()
    
       INPUT m_folio  WITHOUT DEFAULTS FROM folio
       
          AFTER FIELD folio
             IF m_folio IS NULL OR m_folio = 0 THEN
                   ERROR "ERROR, INGRESE UN FOLIO VALIDO"
                   NEXT FIELD folio
             END IF
             IF f_cuenta() = 0 THEN
                 ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
                 NEXT FIELD folio
             END IF
       
          AFTER INPUT
             IF m_folio IS NULL OR m_folio = 0 THEN
                   ERROR "ERROR, INGRESE UN FOLIO VALIDO"
                   NEXT FIELD folio
             END IF
             IF f_cuenta() = 0 THEN
                 ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
                 NEXT FIELD folio
             END IF
       
          ON KEY (ESC)
       
             IF m_folio IS NULL OR m_folio = 0 THEN
                   ERROR "ERROR, INGRESE UN FOLIO VALIDO"
                   NEXT FIELD folio
             END IF
             IF f_cuenta() = 0 THEN
                 ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
                 NEXT FIELD folio
             END IF
             EXIT INPUT
       
          ON KEY (CONTROL-C, INTERRUPT)
             ERROR "PROCESO CANCELADO" SLEEP 2
             ERROR ""
             EXIT PROGRAM
       
       END INPUT

            WHILE TRUE    
                PROMPT "¿EJECUTAR REINVERSION DE CUENTAS? (S/N)  " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_flag = 1
                    ELSE
                        ERROR "PROCESO CANCELADO"
                        SLEEP 2
                        ERROR ""
                        LET ls_flag = 0
                    END IF
                    EXIT WHILE
                 ELSE
                    ERROR "SOLO PRESIONE S o N"
                END IF
            END WHILE 

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Realiza la preliquidacion de las solicitudes del folio dado     #
#---------------------------------------------------------------------------#
FUNCTION f_reinv()
DEFINE lr_saldo RECORD
        nss         CHAR(11),
        subcuenta   LIKE dis_provision.subcuenta,
        acciones    LIKE dis_provision.monto_en_acciones,
        pesos       LIKE dis_provision.monto_en_pesos
    END RECORD,
    lr_dis         RECORD LIKE safre_af:dis_cuenta.*,
    l_id           DECIMAL(11,0),
    l_nss1,
    l_nss2         CHAR(11)

    INITIALIZE lr_saldo.*, l_nss1, l_nss2 TO NULL

    DECLARE cur_reinv CURSOR FOR
    SELECT A.*
    FROM  tmp_nss_1 A
    ORDER BY a.nss

    -- Iniciamos ciclo para cada nss
       FOREACH cur_reinv INTO lr_saldo.*

        INITIALIZE lr_dis.*, l_id TO NULL

        SELECT id_solicitud_saldo INTO l_id
        FROM ret_solicitud_saldo
        WHERE nss = lr_saldo.nss
        AND estado_solicitud = 106 --liquidado

         IF STATUS = NOTFOUND THEN 
           --DISPLAY " NO EXISTE SOLICITUD DE SALDO PARA EL NSS:  ", lr_saldo.nss
           --PROMPT "<ENTER> PARA CONTINUAR " FOR CHAR enter
           CONTINUE FOREACH
         END IF

              LET lr_dis.subcuenta  = lr_saldo.subcuenta
              
            SELECT codigo_siefore INTO  lr_dis.siefore
             FROM   cta_regimen
             WHERE  nss       = lr_saldo.nss
              AND    subcuenta = lr_saldo.subcuenta
              
           IF STATUS = NOTFOUND THEN
              DISPLAY " NO EXISTE SIEFORE PARA EL NSS:  ", lr_saldo.nss,
                     " SUBCUENTA: ",lr_saldo.subcuenta USING "<<<<<"
              PROMPT  " <ENTER> PARA CONTINUAR " FOR CHAR enter
              CONTINUE FOREACH
           END IF

                SELECT precio_del_dia INTO   lr_dis.precio_accion
                 FROM   glo_valor_accion
                 WHERE  fecha_valuacion = HOY
                 AND    codigo_siefore  = lr_dis.siefore

           IF STATUS = NOTFOUND THEN
              DISPLAY " NO EXISTE PRECIO PARA LA SIEFORE:  ", lr_dis.siefore USING "<<<<<"
              PROMPT  " <ENTER> PARA CONTINUAR " FOR CHAR enter
              CONTINUE FOREACH
           END IF

           IF lr_dis.precio_accion <= 0 THEN
              LET lr_dis.precio_accion = 1
           END IF

              LET lr_dis.consecutivo_lote    = l_id
              LET lr_dis.nss                 = lr_saldo.nss
              LET lr_dis.curp                = ""
              LET lr_dis.folio_sua           = ""
              LET lr_dis.dias_cotizados      = 0
              LET lr_dis.sucursal            = NULL
              LET lr_dis.id_aportante        = "REINVER"
              LET lr_dis.estado              = 8
              LET lr_dis.etiqueta            = ""
              LET lr_dis.fecha_valor         = HOY
              LET lr_dis.fecha_pago          = HOY
              LET lr_dis.fecha_conversion    = HOY
              LET lr_dis.fecha_archivo       = HOY
              LET lr_dis.fecha_proceso       = HOY
              LET lr_dis.usuario             = gs_usuario

              #variables del Abono
              LET lr_dis.tipo_movimiento   = 924
              LET lr_dis.folio             = m_folio_abono
              LET lr_dis.monto_en_pesos    = lr_saldo.pesos
              LET lr_dis.monto_en_acciones = lr_saldo.pesos / lr_dis.precio_accion

                 INSERT INTO safre_af:dis_cuenta VALUES(lr_dis.*)
              #Variables del cargo
              LET lr_dis.siefore = 10
              LET lr_dis.tipo_movimiento   = 923
              LET lr_dis.folio             = m_folio_cargo
              LET lr_dis.monto_en_pesos    = (lr_saldo.pesos * -1)
              LET lr_dis.monto_en_acciones = (lr_saldo.acciones * -1)

                INSERT INTO safre_af:dis_cuenta VALUES(lr_dis.*)

            INITIALIZE lr_dis.*, l_id TO NULL

             LET m_procesa = m_procesa + SQLCA.SQLERRD[3]
             DISPLAY "REGISTROS PROCESADOS : " AT 12,1
             DISPLAY m_procesa USING "<<<<<<" AT  12,25 ATTRIBUTE(REVERSE) 

              INSERT INTO nss_tmp VALUES(lr_saldo.nss)

       END FOREACH

         CALL f_marca()

    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW win1

END FUNCTION

#---------------------------------------------------------------------------#
#Verifica si existen registros a 
FUNCTION f_cuenta()

  LET m_cuantos = 0

  SELECT COUNT(UNIQUE nss) INTO m_cuantos
  FROM dis_cuenta a
  WHERE a.folio = m_folio
  AND a.tipo_movimiento IN (800,810,815)

RETURN m_cuantos
END FUNCTION

#---------------------------------------------------------------------------#
#funcion que pasa las solicitudes a la tabla temporal                       #
#---------------------------------------------------------------------------#
FUNCTION f_datos()
DEFINE l_cero    SMALLINT,
       l_nss     CHAR(11),
     lr_saldo RECORD 
        subcuenta SMALLINT,
        siefore   SMALLINT,
        acciones  DECIMAL(13,6),
        pesos     DECIMAL(13,6)
     END RECORD

   LET l_cero = 0

       DECLARE cur_nss CURSOR FOR
          SELECT UNIQUE b.nss FROM ret_trans_imss b 
          WHERE b.folio = m_folio
          AND b.tipo_retiro IN ("A","B","C")
          
          LET l_nss = NULL
       FOREACH cur_nss INTO l_nss

        DECLARE cur_10 CURSOR FOR eje_saldo
          OPEN cur_10 USING l_nss, l_cero, l_cero, HOY
          INITIALIZE lr_saldo.* TO NULL
          FOREACH cur_10 INTO lr_saldo.*
              IF lr_saldo.siefore = 10 THEN
                 INSERT INTO tmp_nss_1 VALUES(l_nss,lr_saldo.subcuenta,
                                              lr_saldo.acciones,lr_saldo.pesos)
              END IF 
          END FOREACH
          CLOSE cur_10
       END FOREACH

--UNLOAD TO "acs.txt"
--SELECT * FROM tmp_nss_1

    SELECT MAX(A.folio) + 1 INTO   m_folio_cargo
    FROM   glo_folio A

    INSERT INTO glo_folio
    VALUES (m_folio_cargo)

    SELECT MAX(A.folio) + 1 INTO   m_folio_abono
    FROM   glo_folio A

    INSERT INTO glo_folio
    VALUES (m_folio_abono)

DISPLAY "FOLIO CARGO: ",m_folio_cargo USING "<<<<<<" AT 6,1 ATTRIBUTE(REVERSE) 
DISPLAY "FOLIO ABONO: ",m_folio_abono USING "<<<<<<" AT 6,45 ATTRIBUTE(REVERSE) 

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana para pedir el folio     de               #
#                  preliquidacion de transferencias                         #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW win1 AT 2,2 WITH FORM "RETC7151" ATTRIBUTE(BORDER)
    DISPLAY " RETC7441               REINVERSION CONTINGENTE VU2.5          ",HOY USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " [Esc]Ejecuta Reinversion                              [Ctrl-C]Salir     " AT 1,1 ATTRIBUTE(REVERSE)  

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion a la fecha dada  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(fecha_precios)

    DEFINE #loc #date
        fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD #loc #lr_precio_acc Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE #loc #smallint
        ls_sie                SMALLINT

    LET ls_sie = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion_isss(?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.siefore = gs_sieviv THEN

            LET ls_sie = gs_sieviv

            SELECT 0                ,
                   fecha_valuacion  ,
                   codigo_siefore   ,
                   precio_del_dia
            INTO   gar_precio_acc[ls_sie].*
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = gdt_fecha_viv
            AND    codigo_siefore  = gs_sieviv

            IF STATUS = NOTFOUND THEN
                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", gdt_fecha_viv USING "DD/MM/yyyy",
                                 " -- SIEFORE ", gs_sieviv CLIPPED
                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            END IF
        ELSE

            IF lr_precio_acc.estado <> 0 THEN
                LET lc_siefore = lr_precio_acc.siefore

                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                                 " -- SIEFORE ", lc_siefore CLIPPED

                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            ELSE
                LET ls_sie                    = lr_precio_acc.siefore
                LET gar_precio_acc[ls_sie].*  = lr_precio_acc.*
            END IF

        END IF
    
    END FOREACH

END FUNCTION

#############################################################
FUNCTION f_marca()
DEFINE l_nss   CHAR(11),
       lr_desmarca RECORD
           nss          CHAR(11),
           marca_entra  SMALLINT,
           correlativo  INTEGER,
           estado_marca SMALLINT,
           marca_causa  SMALLINT,
           usuario      CHAR(08)
       END RECORD,
       l_cuenta,
       l_cuenta1       INTEGER

 INITIALIZE  l_nss, lr_desmarca TO NULL

  LET lr_desmarca.marca_entra   = 921
  LET lr_desmarca.estado_marca  = 0
  LET lr_desmarca.marca_causa   = 921
  LET lr_desmarca.usuario       = gs_usuario
  LET l_cuenta = 0 LET l_cuenta1 = 0

DECLARE cur_marca CURSOR FOR
 SELECT UNIQUE nss FROM  nss_tmp

   FOREACH cur_marca INTO l_nss

    LET lr_desmarca.correlativo = NULL
    SELECT c.correlativo INTO lr_desmarca.correlativo
    FROM cta_act_marca c
    WHERE c.nss = l_nss
    AND c.marca_cod = 921

       LET lr_desmarca.nss           = l_nss

       EXECUTE eje_desmarca USING lr_desmarca.*
       LET l_cuenta = l_cuenta  + 1 

       UPDATE safre_af:ret_solicitud_saldo SET estado_solicitud = 122 --reinvertido
       WHERE nss = l_nss
       AND estado_solicitud = 106 --liquidado

       LET l_cuenta1 = l_cuenta1 + SQLCA.SQLERRD[3]

   END FOREACH

       DISPLAY "REGISTROS DESMARCADOS : " AT 13,1
       DISPLAY l_cuenta USING "<<<<<<"    AT 13,27 ATTRIBUTE(REVERSE) 

       DISPLAY "REGISTROS ACTUALIZADOS : " AT 14,1
       DISPLAY l_cuenta1 USING "<<<<<<" AT 14,27 ATTRIBUTE(REVERSE) 

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

CREATE TEMP TABLE tmp_nss_1 (
nss       CHAR(11),
subcuenta SMALLINT,
acciones  DECIMAL(13,6),
pesos     DECIMAL(13,6)


) WITH NO LOG;


CREATE TEMP TABLE nss_tmp (
nss       CHAR(11)
) WITH NO LOG;

END FUNCTION

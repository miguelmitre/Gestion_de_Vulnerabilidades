###############################################################################
# Proyecto          => SISTEMA DE AFORE( MEXICO )                             #
# Owner             => E.F.P.                                                 #
# Programa RETC836  => RECEPCION DE ARCHIVO A VALIDAR NOMBRES(DIAGNOSTICOS)   #
# Sistema           => RET                                                    #
# Fecha creacion    => 27 DE SEPTIEMBRE DEL 2004 14:02                        #
# Por               => ISAI JIMENEZ ROJAS                                     #
# Actualizacion     => 19/ABRIL/2005 ISAI JIMENEZ ROJAS                       #
#                   => Adecuaciones de acuerdo al Requerimiento 1817 (HSBC)   #
# Version           => v1                                                     #
###############################################################################

DATABASE safre_af
GLOBALS
    DEFINE g_seg_modulo        RECORD LIKE seg_modulo.*    
    DEFINE cuantos             INTEGER
    DEFINE cont                INTEGER
    DEFINE cont_det            INTEGER

    DEFINE archivo_retiro      CHAR(200)
    DEFINE enter               CHAR(1)  
    DEFINE usuario             CHAR(12) 
 
    DEFINE s_codigo_afore      SMALLINT
    DEFINE s_recepcionado      SMALLINT

    DEFINE HOY                 DATE

    DEFINE gr_ret_cza_notifica RECORD LIKE ret_cza_notifica.*
    DEFINE g_nom_archivo       CHAR(50)
    DEFINE g_correlativo       INTEGER

END GLOBALS
{=============================================================================}
{                                                                             }
{=============================================================================}
MAIN
     DEFINE vcontador   SMALLINT
      
     OPTIONS PROMPT LINE LAST
             DEFER INTERRUPT
    
     CALL STARTLOG("RETC836.log")

     WHENEVER ERROR CONTINUE

     CREATE TEMP TABLE tmp_lineas
     (
         linea    CHAR(365)
     )

     WHENEVER ERROR STOP

     CALL init()
     OPEN WINDOW w_8361 AT 4,4 WITH FORM "RETC8361" ATTRIBUTE(BORDER)
     DISPLAY "                             < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
     DISPLAY " RETC836(v1)               RECEPCION DE ARCHIVOS                               " AT 3,1 ATTRIBUTE(REVERSE)
     DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

     -- Captura del Nombre del Archivo a descargar

     INITIALIZE g_nom_archivo TO NULL
     
     INPUT g_nom_archivo WITHOUT DEFAULTS FROM nom_archivo 

        AFTER FIELD nom_archivo
           IF g_nom_archivo IS NULL THEN
              ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
              NEXT FIELD nom_archivo
           END IF

           --VALIDA SI EL ARCHIVO YA FUE PROCESADO
           SELECT COUNT(*)
           INTO   vcontador
           FROM   ret_cza_notifica
           WHERE  nom_archivo = g_nom_archivo

           IF vcontador > 0 THEN
              ERROR " ARCHIVO YA PROCESADO CON ANTERIORIDAD " ATTRIBUTE(NORMAL)
              NEXT FIELD nom_archivo
           END IF

           --VALIDA SI YA SE PROCESO UN ARCHIVO AL DIA
           SELECT COUNT(*)
           INTO   vcontador
           FROM   ret_cza_notifica
           WHERE  fecha_carga = TODAY

           IF vcontador > 0 THEN
              ERROR " SOLO PUEDE PROCESARSE UN ARCHIVO AL DIA "ATTRIBUTE(NORMAL)
              NEXT FIELD nom_archivo
           END IF

           WHENEVER ERROR CONTINUE
           
           SELECT *
           INTO   g_seg_modulo.*
           FROM   seg_modulo
           WHERE  modulo_cod = "ret"

           LET archivo_retiro = g_seg_modulo.ruta_rescate CLIPPED,"/",
                                g_nom_archivo CLIPPED

           LOAD FROM archivo_retiro DELIMITER "+"
           INSERT INTO tmp_lineas
                      
           SELECT count(*)
           INTO   cuantos
           FROM   tmp_lineas
                    
           IF cuantos = 0 THEN
              ERROR "   NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
              ATTRIBUTE(NORMAL)
              NEXT FIELD nom_archivo
           ELSE
              EXIT INPUT
           END IF
            
           WHENEVER ERROR STOP

        ON KEY (INTERRUPT)
             PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
             EXIT PROGRAM
     END INPUT

     DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

     CALL procesa_archivo() #pa

     DISPLAY "FOLIO NUMERO  : ",gr_ret_cza_notifica.folio  AT 18,2

     PROMPT " PROCESO FINALIZADO.... <ENTER> PARA SALIR " FOR CHAR enter

     CLOSE WINDOW w_8361

END MAIN
{=============================================================================}
{                                                                             }
{=============================================================================}
FUNCTION init()   #i

    LET HOY  = TODAY

    SELECT A.estado_solicitud
    INTO   s_recepcionado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local    

    LET g_correlativo = 0

END FUNCTION
{=============================================================================}
{ PROCESA CADA UNO DE LOS DETALLES CARGADOS EN LA TABLA TEMPORAL DE LINEAS    }
{ PROVENIENTES DEL ARCHIVO PLANO                                              }
{=============================================================================}
FUNCTION procesa_archivo()  #pa

    DEFINE lr_ret_det_notifica RECORD LIKE ret_det_notifica.*
    DEFINE cont                SMALLINT
    DEFINE codigo_error        SMALLINT
    DEFINE vlinea              CHAR(365)

    DEFINE vcont_enc           SMALLINT
    DEFINE vcont_det           SMALLINT
    DEFINE vcont_sum           SMALLINT

    -- Se crea temporal para cargar los registros leidos del archivo plano

    SELECT * FROM ret_det_notifica
     WHERE 1=0
      INTO TEMP tmp_ret_det_notifica

    LET cont         = 0
    LET codigo_error = 0

    LET vcont_enc = 0  -- Contador de registros de encabezado
    LET vcont_det = 0  -- Contador de registros de detalle
    LET vcont_sum = 0  -- Contador de registros de sumario

    -- Proceso de cada uno de los detalles cargados
    DECLARE cur_linea CURSOR FOR
    SELECT  * 
      FROM  tmp_lineas

    FOREACH cur_linea INTO vlinea
        LET cont = cont + 1

        CASE vlinea[1,2] 
             WHEN  "01" 
                   --Procesa Encabezado
                   LET vcont_enc = vcont_enc + 1
                   CALL carga_encabezado(vlinea) RETURNING codigo_error 

             WHEN  "03" 
                   --Procesa Encabezado
                   LET vcont_det = vcont_det + 1
                   CALL carga_detalle(vlinea) RETURNING codigo_error

             WHEN  "09" 
                   --Procesa Encabezado
                   LET vcont_sum = vcont_sum + 1
                   CALL carga_sumario(vlinea) RETURNING codigo_error
             OTHERWISE 
                   LET codigo_error = 1
        END CASE

        IF codigo_error != 0 THEN
           EXIT FOREACH
        END IF

    END FOREACH

    CASE codigo_error 
         WHEN 0
                -- Actualiza el No de Registros en ret_cza_lote
                UPDATE ret_cza_notifica
                   SET total_registros = vcont_det
                 WHERE folio = gr_ret_cza_notifica.folio 
                 DISPLAY " REGISTROS PROCESADOS : ", vcont_det USING "<<<,<<&"
                         AT 13,8
         WHEN 1 ERROR "EL ARCHIVO CONTIENE ERRORES EN ENCABEZADO"
                SLEEP 3
         WHEN 2 ERROR "EL ARCHIVO CONTIENE ERRORES EN DETALLES"
                SLEEP 3
         WHEN 3 ERROR "EL ARCHIVO CONTIENE ERRORES EN SUMARIO"
                SLEEP 3

      
    END CASE

    IF vcont_enc = 0 THEN
       ERROR "EL ARCHIVO NO CONTIENE ENCABEZADO"
       SLEEP 3
    END IF

    IF vcont_det = 0 THEN
       ERROR "EL ARCHIVO NO CONTIENE DETALLES"
       SLEEP 3
    END IF

    IF vcont_sum = 0 THEN
       ERROR "EL ARCHIVO NO CONTIENE SUMARIO"
       SLEEP 3
    END IF


END FUNCTION

{========================================================================}
{ Regresa : 0 = No hay errores, 1 = Errores en en encabezado             }
{========================================================================}

FUNCTION carga_encabezado(vlinea)
   DEFINE vlinea    CHAR(365)
   DEFINE vdia      SMALLINT
   DEFINE vmes      SMALLINT
   DEFINE vanio     SMALLINT
   DEFINE vfecha1   DATE

   INITIALIZE gr_ret_cza_notifica.* TO NULL

   -- Se obtiene el Folio a Insertar

   SELECT max(folio)
     INTO gr_ret_cza_notifica.folio
     FROM ret_cza_notifica
 
   -- Se obtiene la fecha de carga en datamart
   LET vanio  = vlinea[17,20]
   LET vmes   = vlinea[21,22]
   LET vdia   = vlinea[23,24]
   LET vfecha1= MDY(vmes,vdia,vanio)
    
   IF gr_ret_cza_notifica.folio IS NULL OR
      gr_ret_cza_notifica.folio =  0    THEN

      LET gr_ret_cza_notifica.folio =  1

   ELSE
      LET gr_ret_cza_notifica.folio =  gr_ret_cza_notifica.folio + 1
   END IF

   LET gr_ret_cza_notifica.nom_archivo        = g_nom_archivo CLIPPED
   LET gr_ret_cza_notifica.fecha_carga        = TODAY
   LET gr_ret_cza_notifica.fec_carga_datamart = vfecha1
   LET gr_ret_cza_notifica.total_registros    = 0
   LET gr_ret_cza_notifica.estado_lote        = 1      --Recepcionado

   -- REGISTRA EL NUEVO FOLIO (Quedando pendiente el No deregs)

   INSERT INTO ret_cza_notifica 
          VALUES (gr_ret_cza_notifica.*)

   IF SQLCA.SQLCODE != 0 THEN
      ERROR "ERROR AL INSERTAR EN ret_cza_notifica"
      SLEEP 3
      ERROR ""
      RETURN 1
   ELSE
      RETURN 0
   END IF

END FUNCTION

{========================================================================}
{ Objetivo: Cargar en la tabla temporar el detalle recibido como param   }
{ Regresa : 0 = No hay errores, 2 = Errores en detalle                   }
{========================================================================}

FUNCTION carga_detalle(vlinea)

   DEFINE vlinea         CHAR(365)
   DEFINE vcodigo_error  SMALLINT
   DEFINE lr_ret_det     RECORD LIKE ret_det_notifica.*

   LET vcodigo_error = 0  -- Inicia con "no hay error"

   -- Arma el registro a ser Insertado en la tabla temporal de detalles

   LET g_correlativo = g_correlativo + 1

   LET lr_ret_det.nss              = vlinea[07,17]
   LET lr_ret_det.sec_pension      = vlinea[18,19]
   LET lr_ret_det.folio            = gr_ret_cza_notifica.folio
   LET lr_ret_det.correlativo      = g_correlativo
   LET lr_ret_det.nombre_datamart  = vlinea[020,069]
   LET lr_ret_det.nombre_procanase = vlinea[070,119]
   LET lr_ret_det.nombre_bdnsar    = vlinea[120,159]
   LET lr_ret_det.paterno_bdnsar   = vlinea[160,199]
   LET lr_ret_det.materno_bdnsar   = vlinea[200,239]
   LET lr_ret_det.nombre_afore     = ""
   LET lr_ret_det.paterno_afore    = ""
   LET lr_ret_det.materno_afore    = ""
   LET lr_ret_det.diag_orig        = vlinea[360,362]
   LET lr_ret_det.diag_afore       = 0                  --vlinea[360,362]
   LET lr_ret_det.estado_registro  = 1                  --Recepcionado

   IF valida_detalle(lr_ret_det.*) = 0 THEN     -- Validacion exitosa

      SELECT nombres, paterno, materno
        INTO lr_ret_det.nombre_afore,
             lr_ret_det.paterno_afore,
             lr_ret_det.materno_afore
        FROM afi_mae_afiliado
       WHERE n_seguro = lr_ret_det.nss

      INSERT INTO ret_det_notifica
          VALUES (lr_ret_det.*)

      IF SQLCA.SQLCODE != 0 THEN
         ERROR "DETALLES CON ERRORES DE ORIGEN"
         SLEEP 3
         ERROR ""
         RETURN 2  -- error en detalle
      ELSE
         RETURN 0
      END IF
   ELSE
      RETURN 2  -- error en detalle
   END IF
   
END FUNCTION

{========================================================================}
{ Objetivo: Procesar el registro de sumario                              }
{ Regresa : 0 = No hay errores, 3 = Errores en sumario                   }
{========================================================================}

FUNCTION carga_sumario(vlinea)

   DEFINE vlinea         CHAR(365)

   RETURN 0

END FUNCTION

{=======================================================================}
{ Objetivo : Validar cada uno de los detalles recibidos en el archivo   }
{            previo a su carga                                          }
{ Regresa  : 0= Exito, 1=Error                                          }
{=======================================================================}
FUNCTION valida_detalle(lr_ret_det_notifica)
   DEFINE lr_ret_det_notifica RECORD LIkE ret_det_notifica.*

   RETURN 0
END FUNCTION
   
   

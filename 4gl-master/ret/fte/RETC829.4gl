################################################################################
# Proyecto          => SISTEMA DE AFORE( MEXICO )                              #
# Owner             => E.F.P.                                                  #
# Programa RETC829  => COMPARADOR DE NOMBRES (RETIRO IV-RT)                    #
# Fecha creacion    => 12 DE ENERO DEL 2000                                    #
# By                => FRANCO ESTEBAN ULLOA VIDELA                             #
# Fecha actualiz.   => 08 DE FEBRERO DEL 2001                                  #
# Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                             #
# Fecha actualiz.   => 15 DE OCTUBRE DEL 2004                                  #
# Actualizacion     => 19/Abril/2005 ISAI JIMENEZ ROJAS                        #
#                   => Ajustes segun Requerimieno 1817                         #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE arr_1 ARRAY[5000] OF RECORD #glo #arr_1
        marca                 CHAR(02) ,
        n_seguro              CHAR(11) ,
        nombre_afore          CHAR(40) ,
        des_afore             CHAR(08) ,
        nombre_oper_03        CHAR(40) ,
        des_procesar          CHAR(08)
    END RECORD

    DEFINE gr_ret_det_notifica   RECORD LIKE ret_det_notifica.*

    DEFINE HOY                   DATE
    DEFINE usuario               CHAR(008)
    DEFINE enter                 CHAR(001)
    DEFINE cont_1                INTEGER
    DEFINE s_codigo_afore        SMALLINT              

    DEFINE g_condicion           CHAR(300)
    DEFINE g_instruccion         CHAR(400)
    DEFINE g_folio               INTEGER
    DEFINE g_bandera_actualiza   SMALLINT

END GLOBALS

MAIN
    DEFINE v_cont          SMALLINT
    
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    

    CALL init() #i

    -- SE ESTABLECEN CRITERIOS DE CONSULTA

    OPEN WINDOW w_8291 AT 4,4 WITH FORM "RETC8291" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> Traer Registros                                      ",
            "<Ctrl-C> Salir " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC829(v1)         VERIFICACION VISUAL DE NOMBRES              ",
            "                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    --OBTIENE EL FOLIO MAS RECIENTE
    SELECT MAX(folio)
    INTO   g_folio
    FROM ret_cza_notifica
    
   
    WHILE TRUE
    
       LET INT_FLAG = FALSE
       
       --CALL limpia_estadisticas()
       
       --ESTABLECE LOS CRITERIOS DE BUSQUEDA    
       CONSTRUCT BY NAME g_condicion ON folio,correlativo,diag_orig,diag_afore
          BEFORE CONSTRUCT
                DISPLAY g_folio TO folio
          AFTER FIELD folio
                LET g_folio = GET_FLDBUF(folio)
                
                IF g_folio IS NULL THEN
                   ERROR " DEBE PROPORCIONAR EL NUMERO DE FOLIO "
                   NEXT FIELD folio
                END IF
                  
                --VERIFICA QUE EXISTA INFORMACION DEL FOLIO PROPORCIONADO
                SELECT COUNT(*)
                INTO   v_cont 
                FROM   ret_cza_notifica
                WHERE  folio = g_folio
              
                IF v_cont = 0 THEN
                   ERROR " NO HAY REGISTROS DEL FOLIO PROPORCIONADO "
                   CALL limpia_estadisticas()
                   NEXT FIELD folio
                END IF
                  
                --VERIFICA QUE HAYA SIDO GENERADO EL LOTE PARA EL FOLIO
                SELECT COUNT(*)
                INTO   v_cont 
                FROM   ret_cza_notifica
                WHERE  folio       = g_folio
                AND    estado_lote = 4  -- ENVIADO
              
                IF v_cont > 0 THEN  -- YA FUE ENVIADO
                   CALL notifica_error() 
                   --NO PERMITIRA LA ACTUALIZACION
                   LET g_bandera_actualiza = FALSE
                ELSE
                   -- PERMITIRA LA ACTUALIZACION
                   LET g_bandera_actualiza = TRUE
                END IF
                  
                --MUESTRA LOS ESTADISTICOS (Solicitados)
                CALL muestra_estadisticas()     

       END CONSTRUCT
       
       IF INT_FLAG THEN 
          --PROMPT  " PRESIONE ...<ENTER> PARA SALIR " FOR CHAR enter
          EXIT WHILE
       END IF

       --VERIFICA SI HAY DATOS CON EL CRITERIO
       IF existen_datos() = TRUE THEN
          CALL primer_paso() #pp  --Consulta y actualiza diagnosticos
          CALL muestra_estadisticas()
       ELSE
          ERROR " NO HAY DATOS CON ESE CRITERIO PARA EL FOLIO PROPORCIONADO "
       END IF

    END WHILE

    CLOSE WINDOW w_8291

END MAIN
{==============================================================================}
{                                                                              }
{                                                                              }
{==============================================================================}
FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local            
END FUNCTION 
{==============================================================================}
{                                                                              }
{                                                                              }
{==============================================================================}
FUNCTION primer_paso()  #pp

    DEFINE lr_notifica          RECORD LIKE ret_det_notifica.*
    
    DEFINE la_detalle           ARRAY[5000] OF RECORD
           nss                  CHAR(11),
           sec_pension          DECIMAL(11,0),
           folio                INTEGER,
           correlativo          INTEGER,
           nombre_datamart      CHAR(50),
           nombre_procanase     CHAR(50),
           nombre_bdnsar        CHAR(50),
           nombre_afore         CHAR(50),
           diag_orig            SMALLINT,
           diag_afore           SMALLINT,
           estado_registro      SMALLINT
           END RECORD
    
    DEFINE vnombre              CHAR(50)
    DEFINE vpaterno             CHAR(50)
    DEFINE vmaterno             CHAR(50)
    DEFINE vtecla               CHAR(1)
    DEFINE vcont                SMALLINT
    DEFINE vpos_arr             SMALLINT
    DEFINE vdiag_afore          SMALLINT

    LET INT_FLAG = FALSE

    -- SELECCION DE REGISTROS RECIBIDOS

    LET vcont = 0
    
    LET g_instruccion = " SELECT * FROM ret_det_notifica ",
                        "  WHERE ", g_condicion CLIPPED,
                        "  AND   folio = ", g_folio

    PREPARE exe_det FROM g_instruccion
    DECLARE cur_det CURSOR FOR exe_det

    -- CARGA DE CADA REGISTRO EN EL ARREGLO
    FOREACH cur_det INTO lr_notifica.*
       
       LET vcont = vcont + 1
       
       LET la_detalle[vcont].nss              = lr_notifica.nss             
       LET la_detalle[vcont].sec_pension      = lr_notifica.sec_pension     
       LET la_detalle[vcont].folio            = lr_notifica.folio           
       LET la_detalle[vcont].correlativo      = lr_notifica.correlativo     
       LET la_detalle[vcont].nombre_datamart  = lr_notifica.nombre_datamart 
       LET la_detalle[vcont].nombre_procanase = lr_notifica.nombre_procanase
       LET la_detalle[vcont].diag_orig        = lr_notifica.diag_orig      
       LET la_detalle[vcont].diag_afore       = lr_notifica.diag_afore     
       LET la_detalle[vcont].estado_registro  = lr_notifica.estado_registro
    
       -- CONCATENACION DEL NOMBRE DE BDNSAR
       LET la_detalle[vcont].nombre_bdnsar = lr_notifica.nombre_bdnsar  CLIPPED,
                                             " ",
                                             lr_notifica.paterno_bdnsar CLIPPED,
                                             " ",
                                             lr_notifica.materno_bdnsar CLIPPED
        
       -- CONCATENACION DEL NOMBRE DE LA AFORE
       SELECT nombres, paterno,  materno
       INTO   vnombre, vpaterno, vmaterno
       FROM   afi_mae_afiliado
       WHERE  n_seguro = la_detalle[vcont].nss

       LET la_detalle[vcont].nombre_afore = vpaterno CLIPPED, " ",
                                            vmaterno CLIPPED, " ",
                                            vnombre  CLIPPED
    END FOREACH
      
    IF vcont = 0 THEN
       ERROR " NO HAY INFORMACION CON ESE CRITERIO DEL FOLIO PROPORCIONADO "
       RETURN
    END IF
      
    -- ABRE VENTANA PARA MOSTRAR DATOS

    OPEN WINDOW w_8292 AT 2,2 WITH FORM "RETC8292" ATTRIBUTE(BORDER)
    
    IF g_bandera_actualiza = TRUE THEN 
       DISPLAY " <ENTER> Modificar Diagnostico                               ",
               " <Ctrl-C> Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    ELSE
       DISPLAY "                                                              ",
               " <Ctrl-C> Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    END IF
      
    DISPLAY " RETM829(v1)           VERIFICACION VISUAL DE NOMBRES           ",
            "               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)   
      
   
    DISPLAY vcont USING "<<<,<<<"," REGISTROS SELECCIONADOS" AT 18,3

    CALL SET_COUNT(vcont)

    -- DESPLEGADO DEL ARREGLO DE LOS DETALLES
    DISPLAY ARRAY la_detalle TO sa_ret_det_notifica.*
       -------------------
       ON KEY (CONTROL-M)
       -------------------
          LET vpos_arr = ARR_CURR()
          
          IF g_bandera_actualiza = TRUE THEN
             
             -- SE CAMBIA MENSAJE Y SE GUARDA VALOR ORIGINAL
             DISPLAY " <ESC> Aceptar Cambio          " AT 1,1 ATTRIBUTE(REVERSE)
             LET vdiag_afore = la_detalle[vpos_arr].diag_afore

             -- PERMITE MODIFICAR EL DIAGNOSTICO DE LA AFORE
             INPUT BY NAME la_detalle[vpos_arr].* WITHOUT DEFAULTS
                   AFTER FIELD diag_afore
                      IF la_detalle[vpos_arr].diag_afore != 1 AND
                         la_detalle[vpos_arr].diag_afore != 2 THEN
                         ERROR " EL DIAGNOSTICO SOLO PUEDE SER 1=ACEPTADO, 2=RECHAZADO "
                         NEXT FIELD diag_afore
                      END IF
             END INPUT
   
             IF INT_FLAG THEN
                --RESTAURA EL DISGNOSTICO ORIGINAL
                LET la_detalle[vpos_arr].diag_afore = vdiag_afore 
                DISPLAY BY NAME la_detalle[vpos_arr].diag_afore 
                LET INT_FLAG = FALSE
             ELSE
                -- ACTUALIZA EL DIAGNOSTICO
                UPDATE ret_det_notifica
                   SET diag_afore  = la_detalle[vpos_arr].diag_afore
                 WHERE nss         = la_detalle[vpos_arr].nss
                   AND sec_pension = la_detalle[vpos_arr].sec_pension
                   AND folio       = la_detalle[vpos_arr].folio
                   AND correlativo = la_detalle[vpos_arr].correlativo
      
                IF SQLCA.SQLCODE != 0 THEN
                   ERROR " ERROR AL ACTUALIZAR DIAGNOSTICO "
                END IF
             END IF
                   
          END IF
    END DISPLAY

    --PROMPT  " PRESIONE ...<ENTER> PARA SALIR " FOR CHAR enter
    
    CLOSE WINDOW w_8292

END FUNCTION
{==============================================================================}
{                                                                              }
{                                                                              }
{==============================================================================}
FUNCTION muestra_estadisticas()

    DEFINE v_aceptados     SMALLINT
    DEFINE v_rechazados    SMALLINT
    DEFINE v_sin_calificar SMALLINT
    DEFINE v_tot_registros SMALLINT

    LET v_aceptados     = 0
    LET v_rechazados    = 0
    LET v_sin_calificar = 0
    LET v_tot_registros = 0
    
    --OBTIENE LOS ESTADISTICOS (Solicitados)
    
    DECLARE cur_est CURSOR FOR
    SELECT  *
    FROM    ret_det_notifica
    WHERE   folio = g_folio
    
    FOREACH cur_est INTO gr_ret_det_notifica.*
       
       LET v_tot_registros = v_tot_registros + 1
       
       IF gr_ret_det_notifica.diag_afore = 0  OR 
          gr_ret_det_notifica.diag_afore IS NULL THEN 
          LET v_sin_calificar = v_sin_calificar + 1
       END IF
         
       IF gr_ret_det_notifica.diag_afore = 1 THEN
          LET v_aceptados = v_aceptados + 1
       END IF
        
       IF gr_ret_det_notifica.diag_afore = 2 THEN
          LET v_rechazados = v_rechazados + 1
       END IF
    END FOREACH
       
    --DESPLIEGA LOS ESTADISTICOS OBTENIDOS
    DISPLAY v_aceptados     TO aceptados     
    DISPLAY v_rechazados    TO rechazados    
    DISPLAY v_sin_calificar TO sin_calificar 
    DISPLAY v_tot_registros TO tot_registros 
    
END FUNCTION
{==============================================================================}
{                                                                              }
{                                                                              }
{==============================================================================}
FUNCTION limpia_estadisticas()

    --DESPLIEGA LOS ESTADISTICOS OBTENIDOS
    DISPLAY " " TO aceptados     
    DISPLAY " " TO rechazados    
    DISPLAY " " TO sin_calificar 
    DISPLAY " " TO tot_registros 
    
END FUNCTION

{==============================================================================}
{                                                                              }
{                                                                              }
{==============================================================================}
FUNCTION existen_datos()
    DEFINE vcont        SMALLINT

    LET vcont = 0
    
    LET g_instruccion = " SELECT COUNT(*) FROM ret_det_notifica ",
                        "  WHERE ", g_condicion CLIPPED,
                        "  AND   folio = ", g_folio

    PREPARE exe_det2 FROM g_instruccion
    
    DECLARE cur_det2 CURSOR FOR exe_det2
    
    OPEN cur_det2
         FETCH cur_det2 INTO vcont
    CLOSE cur_det2
    
    IF vcont = 0 THEN
       RETURN FALSE
    ELSE
       RETURN TRUE
    END IF
    
END FUNCTION


FUNCTION notifica_error()

   DEFINE v_opcion  CHAR(1)

   OPEN WINDOW w_notifica AT 10,12 WITH 7 rows, 55 COLUMNS
        ATTRIBUTE(BORDER, PROMPT LINE LAST -1 )
        
      DISPLAY " LA INFORMACION DEL FOLIO SE ENCUENTRA CON STATUS DE " AT 2,2 
      DISPLAY " < ENVIADO >.  SOLO PODRA CONSULTAR LOS REGISTROS DE " AT 3,2
      DISPLAY " ESTE FOLIO.                                         " AT 4,2
      
      PROMPT "PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR v_opcion

   CLOSE WINDOW w_notifica

END FUNCTION

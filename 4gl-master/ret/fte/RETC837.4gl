################################################################################
# Proyecto          => SISTEMA DE AFORE( MEXICO )                              #
# Owner             => E.F.P.                                                  #
# Programa RETC837  => GENERACION DE ARCHIVO CON RESULTADO DE GIAGNOSTICO      #
# Sistema           => RET                                                     #
# Fecha creacion    => 27 DE SEPTIEMBRE DEL 2004   13:42                       #
# By                => ISAI JIMENEZ ROJAS                                      #
# Modificacion      => 30 DE SEPTIEMBRE DEL 2004   13:34                       #
# By                => ISAI JIMENEZ ROJAS                                      #
#                   => Validar que no se genere mas de una vez el folio y      #
#                   => mandar el valor 2 en los registros que no se calificaron#
# Version           => v1                                                      #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE HOY             DATE
    DEFINE g_usuario       CHAR(8)
    DEFINE g_codigo_afore  SMALLINT              
    DEFINE g_seg_modulo    RECORD LIKE seg_modulo.*

END GLOBALS
{=============================================================================}
{                                                                             }
{=============================================================================}
MAIN
    DEFINE vfolio    INTEGER
    DEFINE vopcion   CHAR(1)
    DEFINE enter     CHAR(1)
    
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT  LINE LAST    ,
        MESSAGE LINE LAST

    CALL init() #i

    -- SE ESTABLECEN CRITERIOS DE CONSULTA

    OPEN WINDOW w_8371 AT 4,4 WITH FORM "RETC8371" ATTRIBUTE(BORDER)
    DISPLAY "                                 <Ctrl-C> Salir                  ",
            "              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC837(v1)         GENERACION DE ARCHIVO DE DIAGNOSTICOS       ",
            "              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)


    -- CAPTURA DEL FOLIO A PROCESAR

    INPUT vfolio FROM folio
       AFTER FIELD folio
          IF vfolio IS NULL THEN
             ERROR "EL FOLIO NO PUEDE SER NULO"
             NEXT FIELD folio
          END IF
          --VALIDA QUE YA HAYA SIDO GENERADO EL ARCHIVO
          SELECT "OK"
          FROM   ret_cza_notifica
          WHERE  folio       = folio
          AND    estado_lote = 4 -- ENVIADO
          
          IF STATUS = 0 THEN --found
             ERROR " EL ARCHIVO PARA ESTE FOLIO YA FUE GENERADO PREVIAMENTE "
             NEXT FIELD folio
          END IF
          
          --VALIDA QUE HAYA INFORMACION PARA EL LOTE
          SELECT "OK"
            FROM ret_cza_notifica
           WHERE folio = vfolio
          
          IF STATUS = NOTFOUND THEN
             ERROR "NO EXISTE INFORMACION DE ESE FOLIO"
             NEXT FIELD folio
          END IF

          PROMPT "ESTA SEGURO (S/N): " FOR CHAR vopcion
          IF vopcion MATCHES "[sS]" THEN
             EXIT INPUT
          ELSE
             INITIALIZE vfolio TO NULL
             DISPLAY vfolio TO folio
             NEXT FIELD folio
          END IF
       ON KEY (INTERRUPT)
          EXIT INPUT
    END INPUT

    IF INT_FLAG THEN 
       PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
    ELSE
       CALL genera_archivo(vfolio) #ga
    END IF
    
    EXIT PROGRAM

END MAIN
{=============================================================================}
{                                                                             }
{=============================================================================}
FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT codigo_afore   ,
           USER
    INTO   g_codigo_afore ,
           g_usuario
    FROM   tab_afore_local            

    SELECT *
      INTO g_seg_modulo.*
      FROM seg_modulo 
     WHERE modulo_cod = "ret"

END FUNCTION 
{===========================================================================}
{                                                                           }
{                                                                           }
{===========================================================================}
FUNCTION genera_archivo(vfolio)  #ga

    DEFINE vfolio               INTEGER
    DEFINE lr_ret_det_notifica  RECORD LIKE ret_det_notifica.*
    DEFINE vtecla               CHAR(1)
    DEFINE vruta                CHAR(100)
    DEFINE varchivo_salida      CHAR(100)
    DEFINE varchivo_paso        CHAR(100)
    DEFINE vcomando             CHAR(200)
    DEFINE vtotal_registros     INTEGER
    
    MESSAGE "GENERANDO ARCHIVO"
    
    LET vruta           = g_seg_modulo.ruta_envio
    LET varchivo_salida = vruta CLIPPED,"/", TODAY USING "YYYYMMDD",".106"
    
    -----------------------------------------
    -- DEFINICION DEL REPORTE ( ENCABEZADO )
    -----------------------------------------
    
    LET varchivo_paso = vruta CLIPPED,"/DIAGENC"
    
    START REPORT rep_encabezado TO varchivo_paso   #re
    
    OUTPUT TO REPORT rep_encabezado(vfolio)        #re
    
    FINISH REPORT rep_encabezado                   #re
    
    LET vcomando = "cat ",varchivo_paso CLIPPED," > ",varchivo_salida CLIPPED
    RUN vcomando

    -------------------------------------
    -- DEFINICION DEL REPORTE ( DETALLE )
    -------------------------------------
    
    LET vtotal_registros = 0
    
    LET varchivo_paso = vruta CLIPPED,"/DIAGDET"
    
    START REPORT rep_detalle TO varchivo_paso
    
    DECLARE cur_detalle CURSOR FOR
      SELECT * FROM ret_det_notifica
       WHERE folio = vfolio
    
    FOREACH cur_detalle INTO lr_ret_det_notifica.*

       SELECT A.paterno ,
              A.materno ,
              A.nombres 
       INTO   lr_ret_det_notifica.paterno_afore ,
              lr_ret_det_notifica.materno_afore ,
              lr_ret_det_notifica.nombre_afore
       FROM   afi_mae_afiliado A
       WHERE  A.n_seguro = lr_ret_det_notifica.nss
    
       LET vtotal_registros = vtotal_registros + 1
       OUTPUT TO REPORT rep_detalle(lr_ret_det_notifica.*)
    END FOREACH
      
    FINISH REPORT rep_detalle
    
    LET vcomando = "cat ",varchivo_paso CLIPPED, " >> ",varchivo_salida CLIPPED
    RUN vcomando
    
    --------------------------------------
    -- DEFINICION DEL REPORTE ( SUMARIO )
    --------------------------------------    
    
    LET varchivo_paso = vruta CLIPPED,"/DIAGSUM"
    
    START REPORT rep_sumario TO varchivo_paso
    
    OUTPUT TO REPORT rep_sumario(vfolio,vtotal_registros)
    
    FINISH REPORT rep_sumario
    
    LET vcomando = "cat ",varchivo_paso CLIPPED, " >> ",varchivo_salida CLIPPED
    RUN vcomando
    
    ------------------------------------
    -- DEPURACION DE ARCHIVOS DE PASO 
    ------------------------------------
    
    LET vcomando = "rm -f ",vruta CLIPPED,"/DIAGENC "
    RUN vcomando
    LET vcomando = "rm -f ",vruta CLIPPED,"/DIAGDET "
    RUN vcomando
    LET vcomando = "rm -f ",vruta CLIPPED,"/DIAGSUM "
    RUN vcomando
    
    DISPLAY "EL ARCHIVO "      AT 10,10
    DISPLAY varchivo_salida    AT 12,10
    DISPLAY "HA SIDO GENERADO" AT 14,10
    
    --ACTUALIZACION DEL ESTADO DEL LOTE
    
    UPDATE ret_cza_notifica
    SET    estado_lote = 4
    WHERE  folio = vfolio
    
    PROMPT "PRESIONE <RETURN> PARA CONTINUAR:" FOR CHAR vtecla
    
END FUNCTION
{===========================================================================}
{                                                                           }
{                                                                           }
{===========================================================================}
REPORT rep_encabezado(vfolio)
   
   DEFINE vfolio              INTEGER
   DEFINE vfec_carga_datamart LIKE ret_cza_notifica.fec_carga_datamart
   
   OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

   FORMAT
      
      ON EVERY ROW
      
         SELECT fec_carga_datamart
           INTO vfec_carga_datamart
           FROM ret_cza_notifica
          WHERE folio = vfolio
          
         PRINT COLUMN 001, "01" ,                      -- Encabezado de lote
               COLUMN 003, "04",                       --
               COLUMN 005, "22",                       -- Ident de operacion
               COLUMN 007, "01",                       --
               COLUMN 009, g_codigo_afore USING "&&&", --
               COLUMN 012, "03",                       --
               COLUMN 014, "001",                      --
               COLUMN 017, vfec_carga_datamart USING "YYYYMMDD",
               COLUMN 025, 341 SPACES
END REPORT
{===========================================================================}
{                                                                           }
{                                                                           }
{===========================================================================}
REPORT rep_detalle(lr_ret_det)
   
   DEFINE lr_ret_det RECORD LIKE ret_det_notifica.*
   DEFINE vnombre    LIKE afi_mae_afiliado.nombres
   DEFINE vpaterno   LIKE afi_mae_afiliado.paterno
   DEFINE vmaterno   LIKE afi_mae_afiliado.materno
   
   OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

   FORMAT
      
      ON EVERY ROW
         IF lr_ret_det.diag_afore IS NULL OR lr_ret_det.diag_afore = 0 THEN
            LET lr_ret_det.diag_afore = 2
         END IF
         
         PRINT COLUMN 001, "03" ,                      -- Tipo de registro
               COLUMN 003, "04",                       -- Ident. de servicio
               COLUMN 005, "22",                       -- Ident. de operacion 
               COLUMN 007, lr_ret_det.nss,             -- NSS
               COLUMN 018, lr_ret_det.sec_pension USING "&&",
               COLUMN 020, lr_ret_det.nombre_datamart, -- Nom/Trab. en datamart
               COLUMN 070, lr_ret_det.nombre_procanase,-- Nom/Trab. en procanase
               COLUMN 120, lr_ret_det.nombre_bdnsar,   -- Nom/Trab. en bdnsar
               COLUMN 160, lr_ret_det.paterno_bdnsar,  -- Nom/Trab. en bdnsar
               COLUMN 200, lr_ret_det.materno_bdnsar,  -- Nom/Trab. en bdnsar
               COLUMN 240, lr_ret_det.nombre_afore,    -- Nom/Trab. en Afore
               COLUMN 280, lr_ret_det.paterno_afore,   -- Nom/Trab. en Afore
               COLUMN 320, lr_ret_det.materno_afore,   -- Nom/Trab. en Afore
               COLUMN 360, lr_ret_det.diag_afore USING "&&&",
               COLUMN 363, 3 SPACES
END REPORT
{===========================================================================}
{                                                                           }
{===========================================================================}
REPORT rep_sumario(vfolio,vtotal_registros)
   
   DEFINE vfolio              INTEGER
   DEFINE vtotal_registros    INTEGER
   DEFINE vfec_carga_datamart LIKE ret_cza_notifica.fec_carga_datamart
   
   OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

   FORMAT
      
      ON EVERY ROW
      
         SELECT fec_carga_datamart
           INTO vfec_carga_datamart
           FROM ret_cza_notifica
          WHERE folio = vfolio
          
         PRINT COLUMN 001, "09" ,                      -- Encabezado de lote
               COLUMN 003, "04",                       --
               COLUMN 005, "22",                       -- Ident. de operacion 
               COLUMN 007, "01",                       --
               COLUMN 009, g_codigo_afore USING "&&&", --
               COLUMN 012, "03",                       --
               COLUMN 014, "001",                      --
               COLUMN 017, vfec_carga_datamart USING "YYYYMMDD",
               COLUMN 025, vtotal_registros    USING "&&&&&&",
               COLUMN 025, 335 SPACES
END REPORT

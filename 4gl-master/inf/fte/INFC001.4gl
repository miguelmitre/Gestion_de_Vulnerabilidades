#################################################################################
# Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
# Sistema           => INF                                                      #
# Programa INFC001  => GENERACION DE LOTE OPERACION 98                          #
# By                => FRANCO ESTEBAN ULLOA VIDELA                              #
# Fecha creacion    => 28 DE ENERO DEL 2002                                     #
# Fecha actualiz.   => 03 DE JUNIO DEL 2003                                     #
# Actualizacion     => JOSE LUIS SALDIVAR CARDOSO                               #
# Sistema           => INF                                                      #
# Actualizacion     => ISAI JIMENEZ ROJAS 04 - OCTUBRE - 2004 :12:15            #
#                   => CAMBIO EN EL LAYOUT DE SALIDA                            #
# fecha actualiz.   => 13 DE DICIEMBRE DE 2013                                  #
# Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                      - Se optimizo el codigo fuente                           #
#                      - Se aplican librerias especiales al modulo de devolucion#
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_estado RECORD
        capturado         LIKE ret_estado.estado_solicitud ,
        procesado         LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE
        c18_nom_archivo         CHAR(018) ,
        RUTA                    CHAR(100) ,
        NOM_ARCHIVO             CHAR(100) ,
        c10_usuario             CHAR(010) ,
        enter                   CHAR(001)

    DEFINE 
        HOY                     ,
        fecha_proceso           DATE

    DEFINE
        gs_cod_afore            SMALLINT

    DEFINE
        ultimo_folio            ,
        i_cont_1                INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
    INPUT WRAP         ,
    PROMPT LINE LAST   ,
    ACCEPT KEY CONTROL-A

    CALL f_lib_abre_log("INFC001")
    CALL init()

    OPEN WINDOW infc0011 AT 2,2 WITH FORM "INFC0011" ATTRIBUTE(BORDER)
    DISPLAY " [Esc] = Aceptar,     [Ctrl-C] = Cancelar                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " INFC001           GENERACION  DE  LOTE  OPERACION  97 o 98                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME fecha_proceso WITHOUT DEFAULTS
        
        AFTER FIELD fecha_proceso
            IF fecha_proceso IS NULL THEN
                ERROR"CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_proceso
            END IF
            
            --Valida que existan Registros a procesar
            SELECT "OK"
            FROM   inf_his_oper97
            WHERE  estado = gr_estado.capturado
            GROUP BY 1
            
            IF STATUS = NOTFOUND THEN
                ERROR " NO EXISTEN REGISTRO PARA PROCESAR...<ENTER> "
                NEXT FIELD fecha_proceso
            END IF
        
        ON KEY (ESC)
            SELECT "OK"
            FROM   inf_his_oper97
            WHERE  estado = gr_estado.capturado
            GROUP BY 1
        
            IF STATUS = NOTFOUND THEN
                PROMPT " NO EXISTEN REGISTRO PARA PROCESAR...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        
            IF esta_seguro() THEN #es
                CALL primer_paso() #pp
        
                DISPLAY "ARCHIVO GENERADO EN LA RUTA :"  AT 10,23
                        DISPLAY gr_seg_modulo.ruta_envio   AT 11,23
        
                DISPLAY "CON EL NOMBRE :"                AT 13,23
                DISPLAY c18_nom_archivo                  AT 14,23
        
                DISPLAY "TOTAL DE REGISTROS          :",
                                i_cont_1 USING "<<<,<<<" AT 16,23
        
                PROMPT " PROCESO FINALIZADO...< ENTER > PARA SALIR"
                                 FOR CHAR enter
                        EXIT PROGRAM
            ELSE
                    PROMPT " PROCESO CANCELADO...< ENTER > PARA SALIR"
                        FOR CHAR enter
                    EXIT PROGRAM
            END IF
        
        ON KEY ( INTERRUPT )
                PROMPT " PROCESO CANCELADO...< ENTER > PARA SALIR"
                    FOR CHAR enter
                EXIT PROGRAM
        
    END INPUT
    CLOSE WINDOW infc0011

END MAIN
{==============================================================================}
{                                                                              }
{==============================================================================}
FUNCTION init()
#i-------------
    LET HOY           = TODAY
    LET fecha_proceso = TODAY

    SELECT A.codigo_afore ,
           USER
    INTO   gs_cod_afore ,
           c10_usuario
    FROM   tab_afore_local A

    SELECT  A.estado_solicitud
    INTO   gr_estado.capturado
    FROM   ret_estado A
    WHERE  descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.procesado
    FROM   ret_estado A
    WHERE  descripcion = "PROCESADO"

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "inf"

    LET RUTA = gr_seg_modulo.ruta_envio CLIPPED,"/"

END FUNCTION
{==============================================================================}
{                                                                              }
{==============================================================================}
FUNCTION primer_paso()
#pp-------------------

    -- Para encabezado Global
    DEFINE cabeza              RECORD
           tipo_registro       CHAR(02),
       ident_servicio      CHAR(02),
       tipo_ent_origen     CHAR(02),
       cve_ent_origen      CHAR(03),
       tipo_ent_destino    CHAR(02),
       cve_ent_destino     CHAR(03),
           fecha_operacion     DATE
           END RECORD

    -- Para encabezado por Tipo de Operacion
    DEFINE cabeza_oper         RECORD
           tipo_registro       CHAR(02)      ,
       ident_servicio      CHAR(02)      ,
       ident_operacion     CHAR(02)      ,
       tipo_ent_origen     CHAR(02)      ,
       cve_ent_origen      CHAR(03)      ,
       tipo_ent_destino    CHAR(02)      ,
       cve_ent_destino     CHAR(03)      ,
           fecha_operacion     DATE          ,
       consecutivo_op      SMALLINT
           END RECORD

    --Para sumario por tipo de Operacion
    DEFINE sumario_oper        RECORD
           tipo_registro       CHAR(02)      ,
           ident_servicio      CHAR(02)      ,
           ident_operacion     CHAR(02)      ,
           tipo_ent_origen     CHAR(02)      ,
       cve_ent_origen      CHAR(03)      ,
       tipo_ent_destino    CHAR(02)      ,
       cve_ent_destino     CHAR(03)      ,
       fecha_operacion     DATE          ,
           origen_devolucion   CHAR(03)      ,
       tot_registros       SMALLINT      ,
       impt_tot_oper       DECIMAL(16,6) ,
       tot_part_viv97      DECIMAL(18,6) ,
       tot_part_viv92      DECIMAL(18,6)
           END RECORD

    --Para sumario Global
    DEFINE sumario             RECORD
           tipo_registro       CHAR(02)      ,
       ident_servicio      CHAR(02)      ,
       tipo_ent_origen     CHAR(02)      ,
       cve_ent_origen      CHAR(03)      ,
       tipo_ent_destino    CHAR(02)      ,
       cve_ent_destino     CHAR(03)      ,
           fecha_operacion     DATE          ,
           tot_registros       SMALLINT      ,
           imp_tot_sum         DECIMAL(15,2) ,
           tot_part_viv97      DECIMAL(18,6) ,
           tot_part_viv92      DECIMAL(18,6)
           END RECORD

    DEFINE lr_inf_his          RECORD LIKE inf_his_oper97.*

    DEFINE vident_oper         CHAR(2)    -- Identificador de tipo de operacion
    DEFINE vcons_oper          SMALLINT   -- Consecutivo por tipo de operacion
    DEFINE vnom_arch           CHAR(200)  -- Nombre del reporte a emitirse
    DEFINE vcmd                CHAR(300)

    LET c18_nom_archivo = "INFDEV.98.",fecha_proceso USING "DDMMYYYY"

    LET NOM_ARCHIVO = RUTA CLIPPED,"INFDEV.98.",fecha_proceso USING "DDMMYYYY"

    -- Genera el Archivo en Blanco

    LET vcmd = "cat /dev/null "," > ",NOM_ARCHIVO
    RUN vcmd

    -------------------------------------------------------------------
    -- GENERA ENCABEZADOS Y DETALLES Y SUMARIOS POR TIPO DE OPERACION
    -- (Un encabezado por cada Tipo de Operacion + sus detalles )
    -------------------------------------------------------------------

    DECLARE cur_od CURSOR FOR  -- Selecciona los tipos de operacion existentes
     SELECT opera_devolucion
       FROM inf_his_oper97
      WHERE estado = gr_estado.capturado
      GROUP BY 1
      ORDER BY 1 ASC

    LET vcons_oper = 0   -- Consectivo por tipo de Operacion

    FOREACH cur_od INTO vident_oper    -- Por cada indentificador de oper.
       LET vcons_oper = vcons_oper + 1

       -------------------------------------------------
       --  E N C A B E Z A D O S (Por Tipo de Operacion)
       -------------------------------------------------
       LET vnom_arch = RUTA CLIPPED,"RETCZA2"

       LET cabeza_oper.tipo_registro    = "02"
       LET cabeza_oper.ident_servicio   = "04"
       LET cabeza_oper.ident_operacion  = vident_oper
       LET cabeza_oper.tipo_ent_origen  = "01"
       LET cabeza_oper.cve_ent_origen   = gs_cod_afore
       LET cabeza_oper.tipo_ent_destino = "04"
       LET cabeza_oper.cve_ent_destino  = "002"
       LET cabeza_oper.fecha_operacion  = fecha_proceso
       LET cabeza_oper.consecutivo_op   = vcons_oper

       START REPORT rep_encabezado_oper TO vnom_arch #reo
        OUTPUT TO REPORT rep_encabezado_oper(cabeza_oper.*) #reo
       FINISH REPORT rep_encabezado_oper

       -- Concatena el encabezado por tipo de operacion
       LET vcmd = "cat ", vnom_arch CLIPPED," >> ",NOM_ARCHIVO
       RUN vcmd
       LET vcmd = "rm -f ", vnom_arch
       RUN vcmd

       ------------------------------------------
       -- D E T A L L E S (Por Tipo de Operacion) #det
       ------------------------------------------

       LET vnom_arch = RUTA CLIPPED,"RETDET1"

       START REPORT rep_detalle TO vnom_arch

       DECLARE cur_det CURSOR FOR
        SELECT *
          FROM inf_his_oper97
         WHERE estado           = gr_estado.capturado
           AND opera_devolucion = vident_oper
         ORDER BY nss ASC

       LET i_cont_1   = 0

       FOREACH cur_det INTO lr_inf_his.*
           LET i_cont_1                = i_cont_1 + 1
       OUTPUT TO REPORT rep_detalle(lr_inf_his.*) #l
       END FOREACH

       FINISH REPORT rep_detalle

       -- Concatena el Detalle generaro por el tipo
       LET vcmd = "cat ", vnom_arch CLIPPED," >> ",NOM_ARCHIVO
       RUN vcmd
       LET vcmd = "rm -f ", vnom_arch
       RUN vcmd

       -----------------------------------------------
       -- S U M A R I O  (Por Tipo de Operacion) #det
       -----------------------------------------------

       LET vnom_arch = RUTA CLIPPED,"RETSUM1"

       LET sumario_oper.tipo_registro     = "04"
       LET sumario_oper.ident_servicio    = "04"
       LET sumario_oper.ident_operacion   = vident_oper
       LET sumario_oper.tipo_ent_origen   = "01"
       LET sumario_oper.cve_ent_origen    = gs_cod_afore
       LET sumario_oper.tipo_ent_destino  = "04"
       LET sumario_oper.cve_ent_destino   = "002"
       LET sumario_oper.fecha_operacion   = fecha_proceso
       LET sumario_oper.origen_devolucion = lr_inf_his.origen_devolucion

       SELECT COUNT(*), SUM(mto_pesos_dev)
         INTO sumario_oper.tot_registros,
              sumario_oper.impt_tot_oper
         FROM inf_his_oper97
        WHERE estado = gr_estado.capturado
          AND opera_devolucion = vident_oper

       SELECT SUM(mto_parti_dev)
         INTO sumario_oper.tot_part_viv97
         FROM inf_his_oper97
        WHERE estado = gr_estado.capturado
          AND opera_devolucion = vident_oper
          AND tipo_viv         = "97"

       SELECT SUM(mto_parti_dev)
         INTO sumario_oper.tot_part_viv92
         FROM inf_his_oper97
        WHERE estado = gr_estado.capturado
          AND opera_devolucion = vident_oper
          AND tipo_viv         = "92"

       START REPORT rep_sumario_oper TO vnom_arch
        OUTPUT TO REPORT rep_sumario_oper(sumario_oper.*) #rs
       FINISH REPORT rep_sumario_oper

       -- Concatena el Sumario por tipo de operacion
       LET vcmd = "cat ", vnom_arch CLIPPED," >> ",NOM_ARCHIVO
       RUN vcmd
       LET vcmd = "rm -f ", vnom_arch
       RUN vcmd

    END FOREACH  -- tipo de operacion

    -------------------------------
    -- OPERACIONES FINALES
    -------------------------------

    LET vcmd = "chmod 777 ",NOM_ARCHIVO
    RUN vcmd

    -- CAMBIA EL ESTATUS DE LOS REGISTROS

    UPDATE inf_his_oper97
    SET    folio             = ultimo_folio        ,
           fecha_ult_proceso = fecha_proceso ,
           estado            = gr_estado.procesado
    WHERE  estado            = gr_estado.capturado

END FUNCTION

{==============================================================================}
{  ENCABEZADO POR TIPO DE OPERACION                                            }
{==============================================================================}
REPORT rep_encabezado_oper(cabeza_oper)
#reo----------------
    DEFINE cabeza_oper      RECORD
           tipo_registro    CHAR(02)      ,
       ident_servicio   CHAR(02)      ,
       ident_operacion  CHAR(02)      ,
       tipo_ent_origen  CHAR(02)      ,
       cve_ent_origen   CHAR(03)      ,
       tipo_ent_destino CHAR(02)      ,
       cve_ent_destino  CHAR(03)      ,
           fecha_operacion  DATE          ,
       consecutivo_op   SMALLINT
           END RECORD

    OUTPUT
        PAGE   LENGTH  1
        LEFT   MARGIN  0
        RIGHT  MARGIN  0
        TOP    MARGIN  0
        BOTTOM MARGIN  0

    FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 001,cabeza_oper.tipo_registro                   ,
            COLUMN 003,cabeza_oper.ident_servicio                  ,
            COLUMN 005,cabeza_oper.ident_operacion                 ,
            COLUMN 007,cabeza_oper.tipo_ent_origen                 ,
            COLUMN 009,cabeza_oper.cve_ent_origen  USING "&&&"     ,
            COLUMN 012,cabeza_oper.tipo_ent_destino                ,
            COLUMN 014,cabeza_oper.cve_ent_destino                 ,
            COLUMN 017,cabeza_oper.fecha_operacion USING "YYYYMMDD",
                COLUMN 025,3 SPACES                                    ,
                COLUMN 028,cabeza_oper.consecutivo_op USING "&&"       ,
                COLUMN 030,291 SPACES

END REPORT

{==============================================================================}
{ D E T A L L E                                                                }
{==============================================================================}

REPORT rep_detalle(reg_oper)
#rd--------------------
    DEFINE reg_oper              RECORD LIKE inf_his_oper97.*
    -- Registro para cada uno de los detalles
    DEFINE detalle RECORD
           part_viv97          DECIMAL(18,6) ,
           part_viv92          DECIMAL(18,6) ,
           precio_part         DECIMAL(11,6) ,
           fecha_afectacion    DATE          ,
           imp_devol_viv97     DECIMAL(16,2) ,
           imp_devol_viv92     DECIMAL(16,2) ,
           int_viv_92          DECIMAL(16,2) ,
           fecha_valor_trasp   DATE          ,
           fecha_valor_devol   DATE          ,
           int_viv_97          DECIMAL(16,2)
           END RECORD

    DEFINE c50_paterno         CHAR(50)
    DEFINE c50_materno         CHAR(50)
    DEFINE c50_nombre          CHAR(50)
    DEFINE c13_rfc             CHAR(13)

    DEFINE c19_part_viv97         CHAR(19),
           c18_part_viv97         CHAR(18),
           c19_part_viv92         CHAR(19),
           c18_part_viv92         CHAR(18),
           c12_precio_part        CHAR(12),
           c11_precio_part        CHAR(11),
       c10_imp_viv97          CHAR(10),
       c11_imp_viv97          CHAR(11),
       c11_imp_viv92          CHAR(11),
       c10_imp_viv92          CHAR(10),
           c11_int_viv97          CHAR(11),
       c10_int_viv97          CHAR(10),
       c11_int_viv92          CHAR(11),
       c10_int_viv92          CHAR(10)

    OUTPUT
        PAGE   LENGTH  1
        LEFT   MARGIN  0
        RIGHT  MARGIN  0
        TOP    MARGIN  0
        BOTTOM MARGIN  0

    FORMAT
    ON EVERY ROW

            SELECT paterno, materno, nombres, n_rfc
              INTO c50_paterno , c50_materno , c50_nombre  , c13_rfc
              FROM afi_mae_afiliado
             WHERE n_seguro = reg_oper.nss

            LET c50_nombre = c50_paterno CLIPPED,"$",
                             c50_materno CLIPPED,"$",
                             c50_nombre  CLIPPED

            IF reg_oper.tipo_viv = "92" THEN
               LET detalle.part_viv92 = reg_oper.mto_parti_dev
               LET detalle.part_viv97 = 0
            ELSE -- 97
               LET detalle.part_viv97 = reg_oper.mto_parti_dev
               LET detalle.part_viv92 = 0
            END IF

            CALL precio_del_dia(reg_oper.fecha_valor_devol,11)
                 RETURNING detalle.precio_part

            IF reg_oper.tipo_viv = "92" THEN
               LET detalle.imp_devol_viv92 = reg_oper.mto_pesos_dev
               LET detalle.int_viv_92      = reg_oper.int_pesos_dev

               LET detalle.imp_devol_viv97 = 0
               LET detalle.int_viv_97      = 0
            ELSE -- 97
               LET detalle.imp_devol_viv97 = reg_oper.mto_pesos_dev
               LET detalle.int_viv_97      = reg_oper.int_pesos_dev

               LET detalle.imp_devol_viv92 = 0
               LET detalle.int_viv_92      = 0
            END IF

        -- Numeros que deben formatearse como cadena
        LET c19_part_viv97 = detalle.part_viv97
                                 USING "&&&&&&&&&&&&.&&&&&&"
        LET c18_part_viv97 = c19_part_viv97[01,12], c19_part_viv97[14,19]

            LET c19_part_viv92 = detalle.part_viv92
                                 USING "&&&&&&&&&&&&.&&&&&&"
        LET c18_part_viv92 = c19_part_viv92[01,12], c19_part_viv92[14,19]

        LET c12_precio_part= detalle.precio_part USING "&&&&&.&&&&&&"
        LET c11_precio_part= c12_precio_part[01,5], c12_precio_part[7,12]

        LET c11_imp_viv97  = detalle.imp_devol_viv97 USING"&&&&&&&&.&&"
        LET c10_imp_viv97  = c11_imp_viv97[01,08],  c11_imp_viv97[10,11]

        LET c11_imp_viv92  = detalle.imp_devol_viv92 USING"&&&&&&&&.&&"
        LET c10_imp_viv92  = c11_imp_viv92[01,08],  c11_imp_viv92[10,11]

        --Intereses
        LET c11_int_viv92  = detalle.int_viv_92 USING"&&&&&&&&.&&"
        LET c10_int_viv92  = c11_int_viv92[01,08],  c11_int_viv92[10,11]

        LET c11_int_viv97  = detalle.int_viv_97 USING"&&&&&&&&.&&"
        LET c10_int_viv97  = c11_int_viv97[01,08],  c11_int_viv97[10,11]

        PRINT
                COLUMN 001,"03"                                      ,
            COLUMN 003,"04"                                      ,
            COLUMN 005,reg_oper.opera_devolucion  USING "&&"     ,
            COLUMN 007,reg_oper.nss                              ,
            COLUMN 018,reg_oper.n_unico                          ,
            COLUMN 036,c50_nombre                                ,
            COLUMN 086,reg_oper.origen_devolucion USING"&&&"     ,
            COLUMN 089,c18_part_viv97                            ,
            COLUMN 107,c18_part_viv92                            ,
            COLUMN 125,c11_precio_part                           ,
            COLUMN 136,10 SPACES                                 ,
            COLUMN 146,"00"                                      ,
            COLUMN 148,10 SPACES                                 ,
            COLUMN 158,reg_oper.fecha_afectacion  USING"YYYYMMDD",
            COLUMN 166,gs_cod_afore USING"&&&"                 ,
            COLUMN 177,c10_imp_viv97                             ,
            COLUMN 187,c10_imp_viv92                             ,
            COLUMN 197,c10_int_viv92                             ,
            COLUMN 207,reg_oper.fecha_valor_trasp USING"YYYYMMDD",
            COLUMN 215,reg_oper.fecha_valor_devol USING"YYYYMMDD",
            COLUMN 223,14 SPACES                                 ,
            COLUMN 237,c10_int_viv97                             ,
            COLUMN 247,26 SPACES                                 ,
        COLUMN 273,c13_rfc                                   ,
                COLUMN 286,35 SPACES

END REPORT

{==============================================================================}
{  SUMARIO POR TIPO DE OPERACION                                               }
{==============================================================================}

REPORT rep_sumario_oper(reg_sum)
#rs------------------------
    DEFINE reg_sum             RECORD
           tipo_registro       CHAR(02)      ,
           ident_servicio      CHAR(02)      ,
           ident_operacion     CHAR(02)      ,
           tipo_ent_origen     CHAR(02)      ,
       cve_ent_origen      CHAR(03)      ,
       tipo_ent_destino    CHAR(02)      ,
       cve_ent_destino     CHAR(03)      ,
       fecha_operacion     DATE          ,
           origen_devolucion   CHAR(03)      ,
       tot_registros       SMALLINT      ,
       impt_tot_oper       DECIMAL(19,6) ,
       tot_part_viv97      DECIMAL(18,6) ,
       tot_part_viv92      DECIMAL(18,6)
           END RECORD

    DEFINE c15_impt_tot_oper   CHAR(15) ,
           c16_impt_tot_oper   CHAR(16)
    DEFINE c18_tot_part_viv97  CHAR(18) ,
           c19_tot_part_viv97  CHAR(19)
    DEFINE c18_tot_part_viv92  CHAR(18) ,
           c19_tot_part_viv92  CHAR(19)

    OUTPUT
        PAGE   LENGTH  1
        LEFT   MARGIN  0
        RIGHT  MARGIN  0
        TOP    MARGIN  0
        BOTTOM MARGIN  0

    FORMAT
    ON EVERY ROW
        -- PREPARACION DE LOS NUMEROS QUE DEBEN IMPRIMIRSE COMO DADENA
            IF reg_sum.impt_tot_oper IS NULL THEN
               LET reg_sum.impt_tot_oper = 0
            END IF

            LET c16_impt_tot_oper = reg_sum.impt_tot_oper
                                USING"&&&&&&&&&&&&&.&&"
            LET c15_impt_tot_oper = c16_impt_tot_oper[01,13],
                    c16_impt_tot_oper[15,16]

        IF reg_sum.tot_part_viv97 IS NULL THEN
           LET reg_sum.tot_part_viv97 = 0
            END IF

        LET c19_tot_part_viv97 = reg_sum.tot_part_viv97
                                 USING"&&&&&&&&&&&&.&&&&&&"
            LET c18_tot_part_viv97 = c19_tot_part_viv97[01,12],
                     c19_tot_part_viv97[14,19]

        IF reg_sum.tot_part_viv92 IS NULL THEN
           LET reg_sum.tot_part_viv92 = 0
            END IF

        LET c19_tot_part_viv92 = reg_sum.tot_part_viv92
                                 USING"&&&&&&&&&&&&.&&&&&&"
            LET c18_tot_part_viv92 = c19_tot_part_viv92[01,12],
                     c19_tot_part_viv92[14,19]

        -- IMPRESION DEL DETALLE

        PRINT
            COLUMN 001,reg_sum.tipo_registro       ,#tipo_registro
            COLUMN 003,reg_sum.ident_servicio      ,#ident_servicio
            COLUMN 005,reg_sum.ident_operacion     ,#ident_operacion
            COLUMN 007,reg_sum.tipo_ent_origen     ,#tipo_ent_origen
            COLUMN 009,reg_sum.cve_ent_origen      ,#clave_ent_origen
            COLUMN 012,reg_sum.tipo_ent_destino    ,#tipo_ent_destin
            COLUMN 014,reg_sum.cve_ent_destino     ,#clave_ent_desti
            COLUMN 017,reg_sum.fecha_operacion     USING"YYYYMMDD",
            COLUMN 025,reg_sum.origen_devolucion   USING"&&&",
                COLUMN 028,reg_sum.tot_registros       USING"&&&&&&",
                COLUMN 034,c15_impt_tot_oper           ,
                COLUMN 049,c18_tot_part_viv97          ,
                COLUMN 067,c18_tot_part_viv92          ,
                COLUMN 085,236 SPACES
END REPORT

FUNCTION Esta_seguro()
#es-------------------
    WHILE TRUE
        PROMPT " ESTA SEGURO S/N" FOR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN

                SELECT MAX(folio)+1
                INTO   ultimo_folio
                FROM   glo_folio

                DISPLAY "FOLIO : ",ultimo_folio AT 18,02
                INSERT INTO glo_folio VALUES (ultimo_folio)

                RETURN TRUE
            ELSE
                RETURN FALSE
            END IF
        END IF
    END WHILE
END FUNCTION

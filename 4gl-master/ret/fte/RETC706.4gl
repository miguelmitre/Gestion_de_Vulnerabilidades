################################################################################
# Proyecto          => Sistema de Afores. ( MEXICO )                           #
# Propietario       => E.F.P.                                                  #
# Programa RETC706  => Reporte de Provision de Recursos a Desinvertir          #
# Fecha             => 25 DE OCTUBRE DEL 2011                                  #
# Por               => ISAI JIMENEZ ROJAS                                      #
# Actualizo         => ISAI JIMENEZ ROJAS                                      #
# Fecha Actualiza   => 04/11/2011 03:53:39 p.m.                                #
# Sistema           => RET                                                     #
#MLM-2180           => Modificaciones al reporte para la nueva plataforma
################################################################################

DATABASE safre_af

GLOBALS
    DEFINE w_codigo_afore     LIKE tab_afore_local.codigo_afore
    DEFINE g_param_dis        RECORD LIKE seg_modulo.*
    DEFINE f_opera            DATE
    DEFINE f_trans            DATE
    DEFINE HOY                DATE

    DEFINE aux_pausa          CHAR(1)
    DEFINE g_enter            CHAR(0001)
    DEFINE g_usuario          CHAR(0008)
    DEFINE G_LISTA            CHAR(0300)
    DEFINE G_IMPRE            CHAR(0300)
    DEFINE impresion          CHAR(0300)
    DEFINE where_clause       CHAR(0250)
    DEFINE g_instruccion      CHAR(1000)

    DEFINE v                  RECORD
           fecha_corte        DATE
           END RECORD

    DEFINE vparametros RECORD
           vfolio             INTEGER ,
           vfecha             DATE
           END RECORD

    DEFINE precio_de_liq   DECIMAL(19,14)

    DEFINE gar_totales ARRAY[13] OF RECORD
       total      ,
       ret97      ,
       cv         ,
       cs         ,
       viv97_pesos,
       viv92_pesos,
       viv97_acc  ,
       viv92_acc  ,
       ret92      DECIMAL(22,6)
    END RECORD

END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN
    DEFER INTERRUPT
          OPTIONS PROMPT LINE LAST,
          MESSAGE LINE LAST-1,
          INPUT WRAP --,
          --ACCEPT KEY CONTROL-O

    CALL startlog("RETC706.log")

    CALL inicializa()

    CALL Consulta()

    PROMPT "  PROCESO FINALIZADO...  <ENTER>  PARA SALIR " FOR CHAR g_enter

END MAIN

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION inicializa()
    DEFINE ls_cont SMALLINT

    SELECT codigo_afore ,
           USER
    INTO   w_codigo_afore ,
           g_usuario
    FROM   tab_afore_local

    SELECT ruta_listados
    INTO   g_param_dis.ruta_listados
    FROM   seg_modulo
    WHERE  modulo_cod='ret'

    LET HOY     = TODAY

    FOR ls_cont = 1 TO 13
       LET gar_totales[ls_cont].total       = 0
       LET gar_totales[ls_cont].ret97       = 0
       LET gar_totales[ls_cont].cv          = 0
       LET gar_totales[ls_cont].cs          = 0
       LET gar_totales[ls_cont].viv97_pesos = 0
       LET gar_totales[ls_cont].viv92_pesos = 0
       LET gar_totales[ls_cont].viv97_acc   = 0
       LET gar_totales[ls_cont].viv92_acc   = 0
       LET gar_totales[ls_cont].ret92       = 0
    END FOR
END FUNCTION

#==============================================================================#
# FUNCTION QUE ABRE LA PANTALLA DE CAPTURA DE CRITERIO
#==============================================================================#
FUNCTION Consulta()

    --DEFINE where_clause          CHAR(250)
    DEFINE v_permisos            CHAR(110)
    DEFINE v_cont_reg            INT
    DEFINE l_record RECORD
           tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro    ,
           nss                   LIKE dis_provision.nss               ,
           folio                 LIKE dis_provision.folio             ,
           consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
           siefore               LIKE dis_provision.siefore
           END RECORD
    DEFINE lr_montos   RECORD
           acciones_ret97        LIKE ret_monto_siefore.acciones_ret97 ,
           acciones_cv           LIKE ret_monto_siefore.acciones_cv    ,
           acciones_cs           LIKE ret_monto_siefore.acciones_cs    ,
           acciones_ret92        LIKE ret_monto_siefore.acciones_ret92 ,
           siefore               LIKE ret_monto_siefore.siefore
           END RECORD
    DEFINE v_diag_registro       LIKE ret_solicitud_saldo.diag_recep_afore
    DEFINE v_mensaje             CHAR(80)
    DEFINE v_contador            INTEGER

    DEFINE lr_montos_tmp         RECORD
           subcuenta             INTEGER,
           siefore               SMALLINT,
           monto_en_acciones     DECIMAL(18,6)
           END RECORD

    DEFINE lc_archivo_det,
           lc_archivo_cza  CHAR(300)

    DEFINE lc_comando_cat,
           lc_comando_rm   CHAR(500)

    ----------------------------------------------------------------------------

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 4,4 WITH FORM "RETC7061" ATTRIBUTE( BORDER)
    DISPLAY "RETC706                       (Ctrl-C) Salir                (ESC) Ejecutar     " AT 3,1 ATTRIBUTE(REVERSE,green)

    LET INT_FLAG = FALSE

    --CRITERIO DE BUSQUEDA POR FOLIO Y FECHA CONVERSION
    CONSTRUCT BY NAME where_clause ON dis_provision.folio,
                      dis_provision.fecha_conversion

       ON KEY(ESC)
           LET int_flag =FALSE
           EXIT  CONSTRUCT

       ON KEY(CONTROL-C)
           LET int_flag = TRUE
           EXIT CONSTRUCT

        AFTER CONSTRUCT
           -- FORZA A QUE SE ESTABLEZCA UN CRITERIO DE BUSQUEDA
           IF NOT INT_FLAG THEN
              IF NOT FIELD_TOUCHED(dis_provision.folio,dis_provision.fecha_conversion) THEN
                 ERROR "DEBE ESBLECER UN CRITERIO DE BUSQUEDA"
                 CONTINUE CONSTRUCT
              END IF
           END IF

    END CONSTRUCT

    IF INT_FLAG THEN
       PROMPT " PROCESO CANCELADO... <ENTER> PARA SALIR " FOR CHAR g_enter
       CLOSE WINDOW ventana_21
       EXIT PROGRAM
    END IF

    -----------------------------------------------------------------
    -- VALIDACION DE LA EXISTENCIA DE INFORMACION DEL CRITERIO
    -----------------------------------------------------------------

    MESSAGE  "VALIDANDO EXISTENCIA DE INFORMACION....." ATTRIBUTE(REVERSE)
    SLEEP 1

    LET g_instruccion = " SELECT COUNT(*) ",
                        " FROM  dis_provision ",
                        " WHERE ",where_clause CLIPPED,
                        " AND   subcuenta IN (1,2,5,6,7,9)                 ",    -- se elimina subcuenta 8
                        " AND tipo_movimiento IN (921) "

    PREPARE cust_stmt1 FROM g_instruccion

    EXECUTE cust_stmt1 INTO v_contador

    IF v_contador = 0 THEN
       MESSAGE "NO EXISTE INFORMACION CON EL CRITERIO SELECCIONADO..." ATTRIBUTE(REVERSE)
       PROMPT  "PRESIONE <ENTER> PARA SALIR: " FOR CHAR g_enter
       EXIT PROGRAM
    END IF

    -----------------------------------------------------------------
    -- PREPARA LA GENERACION DEL REPORTE
    -----------------------------------------------------------------
    #MLM-2180
    LET lc_archivo_det = g_param_dis.ruta_listados CLIPPED,"/",
                         g_usuario CLIPPED, ".RPT_PROV_DES_DET.",
                         HOY USING "YYYYMMDD"

    LET lc_archivo_cza = g_param_dis.ruta_listados CLIPPED,"/",
                         g_usuario CLIPPED, ".RPT_PROV_DES_CZA.",
                         HOY USING "YYYYMMDD"

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED, ".RPT_PROV_DES.",
                  HOY USING "YYYYMMDD"

    LET lc_comando_cat = "cat ", lc_archivo_cza CLIPPED, " ",
                                 lc_archivo_det CLIPPED, " > ",
                                 G_IMPRE CLIPPED

    LET lc_comando_rm = "rm ", lc_archivo_cza CLIPPED, " ",
                               lc_archivo_det CLIPPED


    START REPORT rpt_cza_prov TO lc_archivo_cza
    START REPORT rpt_cuenta_imp TO lc_archivo_det

    MESSAGE  "PROCESANDO INFORMACION... " ATTRIBUTE(REVERSE)
    SLEEP 1

    -----------------------------------------------------------------
    -- SELECCIONA LA INFORNACION REQUERIDA PARA EL REPORTE
    -----------------------------------------------------------------

    LET g_instruccion = "SELECT nss,folio,consecutivo_lote, siefore ",
                        " FROM  dis_provision  ",
                        " WHERE ",where_clause CLIPPED,
                        " AND   subcuenta IN (1,2,5,6,7,9) ",    -- se elimina subcuenta 8
                        " AND   tipo_movimiento IN (921) ",
                        " GROUP BY 1,2,3,4",
                        " ORDER BY 1,2,3,4"

    PREPARE cust_stmt2 FROM g_instruccion

    DECLARE cur_rpt CURSOR FOR cust_stmt2

    LET v_cont_reg = 0

    -- POR CADA UNO DE LOS NSS
    FOREACH cur_rpt INTO l_record.nss              ,
                         l_record.folio            ,
                         l_record.consecutivo_lote ,
                         l_record.siefore

        MESSAGE " PROCESANDO INFORMACION... " ATTRIBUTE(REVERSE)

        LET v_cont_reg = v_cont_reg + 1

        --SELECCION DE DATOS REQUERIDOS DE ret_monto_siefore y ret_solicitud_tx
        --SELECCIONA LOS MONTOS POR SOLICITUD

        MESSAGE " SELECCIONANDO MONTOS... " ATTRIBUTE(REVERSE)

        --OBTIENE LOS MONTOS POR CADA SUBCUENTA A DETALLE DE CADA SOLICITUD
        DECLARE cur_monto CURSOR FOR
        SELECT subcuenta, siefore, SUM(monto_en_acciones)
        FROM   dis_provision
        WHERE  nss              = l_record.nss
        AND    folio            = l_record.folio
        AND    consecutivo_lote = l_record.consecutivo_lote
        AND    siefore          = l_record.siefore
        AND    subcuenta        IN(1,2,6,9,5,7)
        GROUP  BY 1,2

        --INICIALIZA MONTOS
        LET lr_montos.acciones_ret97    = 0
        LET lr_montos.acciones_cv       = 0
        LET lr_montos.acciones_cs       = 0
        LET lr_montos.acciones_ret92    = 0

        FOREACH cur_monto INTO lr_montos_tmp.*

           CASE
              WHEN lr_montos_tmp.subcuenta = 1
                   LET lr_montos.acciones_ret97 = lr_montos.acciones_ret97 + lr_montos_tmp.monto_en_acciones
              WHEN lr_montos_tmp.subcuenta = 2 OR
                   lr_montos_tmp.subcuenta = 6 OR
                   lr_montos_tmp.subcuenta = 9
                   LET lr_montos.acciones_cv    = lr_montos.acciones_cv    + lr_montos_tmp.monto_en_acciones
              WHEN lr_montos_tmp.subcuenta = 5
                   LET lr_montos.acciones_cs    = lr_montos.acciones_cs    + lr_montos_tmp.monto_en_acciones
              WHEN lr_montos_tmp.subcuenta = 7
                   LET lr_montos.acciones_ret92 = lr_montos.acciones_ret92 + lr_montos_tmp.monto_en_acciones
           END CASE

        END FOREACH

        --SELECCION DEL DIAGNOSTICO y  ESTADO DE SUBCUENTA DE VIVIENDA

        LET f_opera = HOY

        SELECT diag_recep_afore
        INTO   v_diag_registro
        FROM   ret_solicitud_saldo
        WHERE  nss                = l_record.nss
        AND    id_solicitud_saldo = l_record.consecutivo_lote

        IF SQLCA.SQLCODE = NOTFOUND THEN
           INITIALIZE v_diag_registro TO NULL
        END IF

        MESSAGE " GENERANDO DETALLE  " ATTRIBUTE(REVERSE)

        IF v_cont_reg = 1 THEN
           OUTPUT TO REPORT rpt_cza_prov(l_record.*   ,
                                         lr_montos.*  ,
                                         v_diag_registro,
                                         f_opera      )
        END IF

        OUTPUT TO REPORT rpt_cuenta_imp(l_record.*   ,
                                        lr_montos.*  ,
                                        v_diag_registro,
                                        f_opera      )

    END FOREACH  -- foreach de cada NSS

    IF v_cont_reg = 0 THEN
         MESSAGE " NO EXISTEN REGISTROS CON ESTE CRITERIO... "
         SLEEP 4
         MESSAGE " "
         EXIT PROGRAM
    END IF

    FINISH REPORT rpt_cza_prov
    FINISH REPORT rpt_cuenta_imp

    MESSAGE ""

    RUN lc_comando_cat
    RUN lc_comando_rm

    LET v_permisos = " chmod 777 ",G_IMPRE
    RUN v_permisos

    LET v_mensaje = "El archivo ",G_IMPRE CLIPPED, " ha sido generado..."

    MESSAGE v_mensaje ATTRIBUTE(REVERSE)

    PROMPT " DESEA GENERAR IMPRESION (S/N)?: " FOR CHAR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
      LET impresion = "lp ",G_IMPRE
      --LET impresion = "vi ",G_IMPRE
      RUN impresion
    END IF

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
REPORT rpt_cuenta_imp(l_record,lr_montos,v_diag_registro,f_opera2)

   DEFINE l_record RECORD
        tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro    ,
        nss                   LIKE dis_provision.nss               ,
        folio                 LIKE dis_provision.folio             ,
        consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
        siefore               LIKE dis_provision.siefore
   END RECORD

   DEFINE lr_montos   RECORD
        acciones_ret97        LIKE ret_monto_siefore.acciones_ret97 ,
        acciones_cv           LIKE ret_monto_siefore.acciones_cv    ,
        acciones_cs           LIKE ret_monto_siefore.acciones_cs    ,
        acciones_ret92        LIKE ret_monto_siefore.acciones_ret92 ,
        siefore               LIKE ret_monto_siefore.siefore
   END RECORD

   DEFINE v_diag_registro     LIKE ret_solicitud_saldo.diag_recep_afore

   DEFINE r_datnss  RECORD
        monto72               LIKE dis_provision.monto_en_pesos  ,
        nombre_afore          LIKE ret_transf_rx.nombre_afore    ,
        paterno_afore         LIKE ret_transf_rx.paterno_afore   ,
        materno_afore         LIKE ret_transf_rx.materno_afore   ,
        diag_registro         LIKE ret_transf_rx.diag_registro
   END RECORD

   DEFINE f_opera2            DATE
   DEFINE hoy                 DATE
   DEFINE nombre_siefore      CHAR(03)
   DEFINE r_nombre            CHAR(60)
   DEFINE r_nom_rep           CHAR(07)
   DEFINE cont_tipo_retiro1   SMALLINT
   DEFINE cont_tipo_retiro2   SMALLINT
   DEFINE ind                 SMALLINT
   DEFINE i                   SMALLINT

   DEFINE rpt_afore RECORD
          codigo_afore        LIKE safre_af:tab_afore_local.codigo_afore ,
          razon_social        LIKE safre_af:tab_afore_local.razon_social
   END RECORD

   DEFINE reg_s1, reg_s2      ARRAY[6] OF RECORD
         tot                  INTEGER,
         tipo_ret             CHAR(1),
         total                DECIMAL(16,6),
         r97                  DECIMAL(16,6),
         cv                   DECIMAL(16,6),
         cs                   DECIMAL(16,6),
         r92                  DECIMAL(16,6)
   END RECORD

   DEFINE reg             ARRAY[6] OF RECORD
        cont_nss_uni      SMALLINT
   END RECORD

   DEFINE lr_tot_fin_sb1 ,
           lr_tot_fin_sb2 RECORD
        total             DECIMAL(16,6),
        ret97             DECIMAL(16,6),
        cv                DECIMAL(16,6),
        cs                DECIMAL(16,6),
        ret92             DECIMAL(16,6)
   END RECORD

   DEFINE
        encabezado           CHAR(30) ,
        nombre_final         CHAR(50) ,
        L1                   CHAR(01) ,
        L2                   CHAR(02) ,
        L5                   CHAR(05) ,
        L10                  CHAR(10)

   DEFINE
        cont_nss_fin         ,
        cont_nss_unicos      SMALLINT

   DEFINE v_total           DECIMAL(16,6)

   OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  200
        PAGE LENGTH   90

   ORDER BY l_record.siefore

   FORMAT

     --------------------
     FIRST PAGE HEADER
     --------------------
           LET ind             = 0
           LET cont_nss_unicos = 0
           LET cont_nss_fin    = 0


           LET  L1  = "\304"
           LET  L2  = "\304\304"
           LET  L5  = "\304\304\304\304\304"
           LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

           PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
           PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'

           SELECT codigo_afore           ,
                  razon_social
           INTO   rpt_afore.codigo_afore ,
                  rpt_afore.razon_social
           FROM safre_af:tab_afore_local

           LET hoy       = TODAY
           LET r_nom_rep = "RETC706"

           IF l_record.folio = 0 THEN
               LET f_opera = f_opera2
           ELSE
               SELECT fecha_genera
               INTO   f_opera
               FROM   ret_ctr_envio_lote
               WHERE  folio = l_record.folio
           END IF

           CALL habil_siguiente(f_opera,3) RETURNING f_trans

           IF rpt_afore.codigo_afore = 532 THEN
               LET encabezado = "SUBDIRECCION DE BENEFICIOS"
           ELSE
               LET encabezado = "   MODULO DE RETIROS      "
           END IF

           PRINT COLUMN 067,encabezado
           SKIP 1 LINE

           PRINT COLUMN 054,"   * REPORTE DE PROVISION DE DISPOSICION DE RECURSOS *"
           SKIP 2 LINE

           PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                                   USING "###","  ",
                                                   rpt_afore.razon_social CLIPPED,
                 COLUMN 126,"PAGINA              :",pageno USING "##"

           PRINT
           PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
                 COLUMN 126,"FECHA VAL. TRANSFER : ",f_trans USING "DD-MM-YYYY"

           PRINT
           PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<<<<<",
                 COLUMN 126,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY"

           PRINT
           PRINT COLUMN 002,"FECHA DE OPERACION  : ",f_opera USING "DD-MM-YYYY"

           PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

           PRINT COLUMN 1,"\332",L10,L1,
                          "\302",L10,L10,L10,L2,L1,
                          "\302",L5,L1,
                                 L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                                 L10,L10,L10,L5,
                          "\302",L5,
                          "\302",L2,L2,
                          "\277"

           PRINT COLUMN 1,"|           |",
                       "                                 |",
                       --"      |", --" TIPO |",
                       --"       |",--" TIPO  |",
                       --"       |",--" TIPO  |",
                       "                                                          MONTOS A LIQUIDAR DE LA CUENTA INDIVIDUAL (ACCIONES)                                                   |",
                       "     |",
                       "    |"
        PRINT COLUMN 1,"|           |",
                       "                                 \303",L5,
                       --"      |",
                       --"       |",
                       --"       \303",L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L5,
                       "\302",L2,L2,"|"

        PRINT COLUMN 1,"|    NSS    |",
                       "       NOMBRE DEL TRABAJADOR     |",
                       --"  DE  |",
                       --"  DE   |",
                       --"  DE   |",
                               "SIEF.|",
                       "     TOTAL     |",
                       "      R97      |",
                       "       CV      |",
                       "      CS       |",
                               "  V I V  97   |",
                               "  V I V  92   |",
                       "P A R T  VIV97 |",
                       "P A R T  VIV92 |",
                       "      R92     |",
                       "  V I V  72   |",
                       "DIAG.|",
                       "REG.|"

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       --"      |",
                       --"       |",
                       --"       |",
                       "     |",
                       "               |",
                       "               |",
                       "               |",
                       "               |",
                       "              |",
                       "              |",
                       "               |",
                       "               |",
                       "              |",
                       "              |",
                       "     |",
                       "    |"

         PRINT COLUMN 1,"|           |",
                        "                                 |",
                        --"SEGURO|",
                        --"PENSION|",
                        --"PRESTA.|",
                        "     |",
                        "    ACCIONES   |",
                        "    ACCIONES   |",
                        "    ACCIONES   |",
                        "    ACCIONES   |",
                        "     PESOS    |",
                        "     PESOS    |",
                        "PARTICIPACIONES|",
                        "PARTICIPACIONES|",
                        "    ACCIONES  |",
                        "     PESOS    |",
                        "     |",
                        "    |"

           PRINT COLUMN 1,"\300",L10,L1,
                          "\301",L10,L10,L10,L2,L1,
                          "\301",L5,L1,
                                 L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L2,L2,
                          "\301",L10,L2,L2,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L2,L2,
                          "\301",L5,L2,L1,
                          "\301",L5,
                          "\301",L2,L2,
                          "\331"

           SKIP 2 LINE

     --------------------
     PAGE HEADER
     --------------------

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
        PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'

        SELECT codigo_afore, razon_social
        INTO rpt_afore.codigo_afore, rpt_afore.razon_social
        FROM safre_af:tab_afore_local

        LET hoy       = TODAY
        LET r_nom_rep = "RETC706"

        IF l_record.folio = 0 THEN
           LET f_opera = f_opera2
        ELSE
           SELECT fecha_genera
           INTO   f_opera
           FROM   ret_ctr_envio_lote
           WHERE  folio = l_record.folio
        END IF

        CALL habil_siguiente(f_opera,3) RETURNING f_trans

        PRINT COLUMN 067,encabezado
        SKIP 1 LINE

        PRINT COLUMN 054,"    REPORTE DE PROVISION DE DESINVERSION DE RECURSOS"
        SKIP 2 LINE

        PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                                USING "###","  ",
                                                rpt_afore.razon_social CLIPPED,
                 COLUMN 126,"PAGINA              :",pageno USING "##"

        PRINT
        PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
              COLUMN 126,"FECHA VAL. TRANSFER : ",f_trans USING "DD-MM-YYYY"

        PRINT
        PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<<<<<",
              COLUMN 126,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY"

        PRINT
        PRINT COLUMN 002,"FECHA DE OPERACION  : ",f_opera USING "DD-MM-YYYY"

        PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

        PRINT COLUMN 1,"\332",L10,L1,
                          "\302",L10,L10,L10,L2,L1,
                          "\302",L5,L1,
                                 L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                                 L10,L10,L10,L5,
                          "\302",L5,
                          "\302",L2,L2,
                          "\277"

           PRINT COLUMN 1,"|           |",
                       "                                 |",
                       --"      |", --" TIPO |",
                       --"       |",--" TIPO  |",
                       --"       |",--" TIPO  |",
                       "                                                          MONTOS A LIQUIDAR DE LA CUENTA INDIVIDUAL (ACCIONES)                                                   |",
                       "     |",
                       "    |"
        PRINT COLUMN 1,"|           |",
                       "                                 \303",L5,
                       --"      |",
                       --"       |",
                       --"       \303",L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L5,
                       "\302",L2,L2,"|"

        PRINT COLUMN 1,"|    NSS    |",
                       "       NOMBRE DEL TRABAJADOR     |",
                       --"  DE  |",
                       --"  DE   |",
                       --"  DE   |",
                               "SIEF.|",
                       "     TOTAL     |",
                       "      R97      |",
                       "       CV      |",
                       "      CS       |",
                               "  V I V  97   |",
                               "  V I V  92   |",
                       "P A R T  VIV97 |",
                       "P A R T  VIV92 |",
                       "      R92     |",
                       "  V I V  72   |",
                       "DIAG.|",
                       "REG.|"

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       --"      |",
                       --"       |",
                       --"       |",
                       "     |",
                       "               |",
                       "               |",
                       "               |",
                       "               |",
                       "              |",
                       "              |",
                       "               |",
                       "               |",
                       "              |",
                       "              |",
                       "     |",
                       "    |"

         PRINT COLUMN 1,"|           |",
                        "                                 |",
                        --"SEGURO|",
                        --"PENSION|",
                        --"PRESTA.|",
                        "     |",
                        "    ACCIONES   |",
                        "    ACCIONES   |",
                        "    ACCIONES   |",
                        "    ACCIONES   |",
                        "     PESOS    |",
                        "     PESOS    |",
                        "PARTICIPACIONES|",
                        "PARTICIPACIONES|",
                        "    ACCIONES  |",
                        "     PESOS    |",
                        "     |",
                        "    |"

           PRINT COLUMN 1,"\300",L10,L1,
                          "\301",L10,L10,L10,L2,L1,
                          "\301",L5,L1,
                                 L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L2,L2,
                          "\301",L10,L2,L2,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L2,L2,
                          "\301",L5,L2,L1,
                          "\301",L5,
                          "\301",L2,L2,
                          "\331"

         SKIP 2 LINE

     ---------------------------------
     --AFTER GROUP  OF lr_montos.siefore
     ON EVERY ROW
     ---------------------------------
          --SE OBTIENE NOMBRE DEL AFILIAD0
          SELECT nombres ,
                 paterno ,
                 materno
          INTO   r_datnss.nombre_afore  ,
                 r_datnss.paterno_afore ,
                 r_datnss.materno_afore
          FROM   afi_mae_afiliado
          WHERE  n_seguro = l_record.nss

          IF SQLCA.SQLCODE = NOTFOUND THEN
               LET r_nombre = NULL
          ELSE
             LET r_nombre = r_datnss.paterno_afore CLIPPED,' ',
                            r_datnss.materno_afore CLIPPED,' ',
                            r_datnss.nombre_afore CLIPPED
          END IF

          --SELECCION DE DESCRIPCION DE SIEFORE
          IF lr_montos.siefore = 1 THEN
              LET  nombre_siefore = "SB1"
              LET  cont_tipo_retiro1 = cont_tipo_retiro1 + 1
          ELSE
              LET  nombre_siefore = "SB2"
              LET  cont_tipo_retiro2 = cont_tipo_retiro2 + 1
          END IF

          LET v_total = lr_montos.acciones_ret97 +
                        lr_montos.acciones_cv  +
                        lr_montos.acciones_cs  +
                        lr_montos.acciones_ret92

          PRINT COLUMN 002,l_record.nss                                           ,
                COLUMN 014,r_nombre[1,35]                                         ,
                COLUMN 49 ,l_record.siefore            USING "###"                ,   --nombre_siefore
                COLUMN 54 ,v_total                     USING "#######&.&&&&&&"    ,
                COLUMN 70 ,lr_montos.acciones_ret97    USING "#######&.&&&&&&"    ,
                COLUMN 86 ,lr_montos.acciones_cv       USING "#######&.&&&&&&"    ,
                COLUMN 102,lr_montos.acciones_cs       USING "#######&.&&&&&&"    ,
                COLUMN 180,lr_montos.acciones_ret92    USING "#######&.&&&&&&"    ,
                COLUMN 211,v_diag_registro             USING "###"

          --SKIP 1 LINE


     {
     -----------------------------------
     BEFORE GROUP OF l_record.nss
     -----------------------------------
         LET cont_nss_unicos = cont_nss_unicos + 1
     }

     -----------------------------------
     --AFTER GROUP OF l_record.tipo_retiro
     --ON EVERY ROW
     --AFTER GROUP  OF l_record.siefore
     -------------------------------------
     --
     --     LET ind = ind + 1    -- Ocurrencia del arreglo
     --
     --     SKIP 1 LINE

          {
          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 1
          LET reg_s1[ind].tot = cont_tipo_retiro1
          IF reg_s1[ind].tot IS NULL OR reg_s1[ind].tot = '' THEN
              LET reg_s1[ind].tot = 0
          END IF

          LET reg_s1[ind].tipo_ret = l_record.tipo_retiro

          LET reg_s1[ind].total    = GROUP SUM (lr_montos.acciones_ret97 +
                                                lr_montos.acciones_cv  +
                                                lr_montos.acciones_cs  +
                                                lr_montos.acciones_ret92)
                                     WHERE l_record.siefore  = 1
          IF reg_s1[ind].total IS NULL OR reg_s1[ind].total = '' THEN
              LET reg_s1[ind].total = 0
          END IF

          LET reg_s1[ind].r97      = GROUP SUM(lr_montos.acciones_ret97)
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].r97 IS NULL OR reg_s1[ind].r97 = '' THEN
              LET reg_s1[ind].r97 = 0
          END IF

          LET reg_s1[ind].cv       = GROUP SUM(lr_montos.acciones_cv   )
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].cv IS NULL OR reg_s1[ind].cv = '' THEN
              LET reg_s1[ind].cv = 0
          END IF

          LET reg_s1[ind].cs       = GROUP SUM(lr_montos.acciones_cs   )
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].cs IS NULL OR reg_s1[ind].cs = '' THEN
              LET reg_s1[ind].cs = 0
          END IF

          LET reg_s1[ind].r92      = GROUP SUM(lr_montos.acciones_ret92)
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].r92 IS NULL OR reg_s1[ind].r92 = '' THEN
              LET reg_s1[ind].r92 = 0
          END IF

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 2
          LET reg_s2[ind].tot = cont_tipo_retiro2
          IF reg_s2[ind].tot IS NULL OR reg_s2[ind].tot = '' THEN
              LET reg_s2[ind].tot = 0
          END IF

          LET reg_s2[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s2[ind].total    = GROUP SUM (lr_montos.acciones_ret97 +
                                                lr_montos.acciones_cv  +
                                                lr_montos.acciones_cs  +
                                                lr_montos.acciones_ret92)
                                     WHERE l_record.siefore  = 2
          IF reg_s2[ind].total IS NULL OR reg_s2[ind].total = '' THEN
              LET reg_s2[ind].total = 0
          END IF

          LET reg_s2[ind].r97      = GROUP SUM(lr_montos.acciones_ret97)
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].r97 IS NULL OR reg_s2[ind].r97 = '' THEN
              LET reg_s2[ind].r97 = 0
          END IF

          LET reg_s2[ind].cv       = GROUP SUM(lr_montos.acciones_cv   )
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].cv IS NULL OR reg_s2[ind].cv = '' THEN
              LET reg_s2[ind].cv = 0
          END IF

          LET reg_s2[ind].cs       = GROUP SUM(lr_montos.acciones_cs   )
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].cs IS NULL OR reg_s2[ind].cs = '' THEN
              LET reg_s2[ind].cs = 0
          END IF

          LET reg_s2[ind].r92      = GROUP SUM(lr_montos.acciones_ret92)
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].r92 IS NULL OR reg_s2[ind].r92 = '' THEN
              LET reg_s2[ind].r92 = 0
          END IF
          }
          -- ACUMULA EN ARREGLO CONTADOR DE NSS UNICOS POR T DE RETIRO

          --LET reg[ind].cont_nss_uni = cont_nss_unicos

          -------------------------------------------------------------
          -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 1
          -------------------------------------------------------------

          --PRINT COLUMN 1,"\332",L10,L1,
          --             "\302",L10,L10,L10,L10,L10,L5,L1,
          --             "\302",L5,
          --             "\302",L10,L5,
          --             "\302",L10,L5,
          --             "\302",L10,L5,
          --             "\302",L10,L5,
          --             "\302",L10,L2,L2,
          --             "\302",L10,L2,L2,
          --             "\302",L10,L5,
          --             "\302",L10,L5,
          --             "\302",L10,L5,
          --             "\302",L10,L2,L2,
          --             "\277"
          --
          --LET v_total = lr_montos.acciones_ret97 +
          --              lr_montos.acciones_cv  +
          --              lr_montos.acciones_cs  +
          --              lr_montos.acciones_ret92
          --
          --IF lr_montos.siefore = 1 THEN
          --    LET  nombre_siefore = "SB1"
          --ELSE
          --    LET  nombre_siefore = "SB2"
          --END IF
          --
          --PRINT COLUMN 001,"|",
          --      COLUMN 015,"TOTALES POR SIEFORE :  ",
          --      COLUMN 070,"|",
          --      COLUMN 072,l_record.siefore                     USING "###",                    --nombre_siefore,
          --      COLUMN 076,"|",
          --      COLUMN 077,GROUP SUM(lr_montos.acciones_ret97 +
          --                           lr_montos.acciones_cv    +
          --                           lr_montos.acciones_cs    +
          --                           lr_montos.acciones_ret92 ) USING "#######&.&&&&&&",
          --      COLUMN 092,"|",
          --      COLUMN 093,GROUP SUM(lr_montos.acciones_ret97)  USING "#######&.&&&&&&",
          --      COLUMN 108,"|",
          --      COLUMN 109,GROUP SUM(lr_montos.acciones_cv)     USING "#######&.&&&&&&",
          --      COLUMN 124,"|",
          --      COLUMN 125,GROUP SUM(lr_montos.acciones_cs)     USING "#######&.&&&&&&",
          --      COLUMN 140,"|",
          --      COLUMN 155,"|",
          --      COLUMN 170,"|",
          --      COLUMN 186,"|",
          --      COLUMN 202,"|",
          --      COLUMN 203,GROUP SUM(lr_montos.acciones_ret92)  USING "#######&.&&&&&&",
          --      COLUMN 218,"|",
          --      COLUMN 233,"|"
          --
          --PRINT COLUMN 1,"\300",L10,L1,
          --               "\301",L10,L10,L10,L10,L10,L5,L1,
          --               "\301",L5,
          --               "\301",L10,L5,
          --               "\301",L10,L5,
          --               "\301",L10,L5,
          --               "\301",L10,L5,
          --               "\301",L10,L2,L2,
          --               "\301",L10,L2,L2,
          --               "\301",L10,L5,
          --               "\301",L10,L5,
          --               "\301",L10,L5,
          --               "\301",L10,L2,L2,
          --               "\331"
          --
          -----------------------------------------------------
          ---- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
          -----------------------------------------------------
          --PRINT COLUMN 1,"\332",L10,L1,
          --             L10,L10,L10,L10,L10,L5,L1,L1,
          --             L5,L1,
          --             L10,L5,L1,
          --             L10,L5,L1,
          --             L10,L5,L1,
          --             L10,L5,L1,
          --             "\302",L10,L2,L2,
          --             "\302",L10,L2,L2,
          --             "\302",L10,L5,
          --             "\302",L10,L5,
          --             "\302",L10,L5,
          --             "\302",L10,L2,L2,
          --             "\277"
          --
          --PRINT COLUMN 001,"|",
          --      COLUMN 015,"TOTAL DE NSS UNICOS :  ",GROUP COUNT(*)  USING "#####",
          --      COLUMN 140,"|",
          --      COLUMN 155,"|",
          --      COLUMN 170,"|",
          --      COLUMN 186,"|",
          --      COLUMN 202,"|",
          --      COLUMN 218,"|",
          --      COLUMN 233,"|"
          --
          --PRINT COLUMN 1,"\300",L10,L1,
          --               L10,L10,L10,L10,L10,L5,L1,L1,
          --               L5,L1,
          --               L10,L5,L1,
          --               L10,L5,L1,
          --               L10,L5,L1,
          --               L10,L5,L1,
          --               "\301",L10,L2,L2,
          --               "\301",L10,L2,L2,
          --               "\301",L10,L5,
          --               "\301",L10,L5,
          --               "\301",L10,L5,
          --               "\301",L10,L2,L2,
          --               "\331"
          --SKIP 1 LINE
          --
          --LET cont_tipo_retiro1 = 0
          --LET cont_tipo_retiro2 = 0
          --LET cont_nss_unicos   = 0

     {
     -----------------------------------
     AFTER GROUP OF l_record.nss
     -----------------------------------
         LET cont_nss_fin = cont_nss_fin + 1
     }

     {
     ---------------------------------
     ON LAST ROW
     ---------------------------------
          NEED 49 LINES

          SKIP 3 LINES

          PRINT "R E S U M E N "
          PRINT

          SKIP 2 LINE

        ---------------------------
        --  TOTALES FINALES DE SB1
        ---------------------------
        LET lr_tot_fin_sb1.total   = SUM(lr_montos.acciones_ret97 +
                                         lr_montos.acciones_cv    +
                                         lr_montos.acciones_cs    +
                                         lr_montos.acciones_ret92)
                                     WHERE lr_montos.siefore = 1
        LET lr_tot_fin_sb1.ret97   = SUM(lr_montos.acciones_ret97)
                                     WHERE lr_montos.siefore = 1
        LET lr_tot_fin_sb1.cv      = SUM(lr_montos.acciones_cv)
                                     WHERE lr_montos.siefore = 1
        LET lr_tot_fin_sb1.cs      = SUM(lr_montos.acciones_cs)
                                     WHERE lr_montos.siefore = 1
        LET lr_tot_fin_sb1.ret92  =  SUM(lr_montos.acciones_ret92)
                                     WHERE lr_montos.siefore = 1

      --------------------------
      --  TOTALES FINALES DE SB2
      --------------------------
      LET lr_tot_fin_sb2.total   = SUM(lr_montos.acciones_ret97 +
                                       lr_montos.acciones_cv    +
                                       lr_montos.acciones_cs    +
                                       lr_montos.acciones_ret92)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.ret97   = SUM(lr_montos.acciones_ret97)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.cv      = SUM(lr_montos.acciones_cv)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.cs      = SUM(lr_montos.acciones_cs)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.ret92  =  SUM(lr_montos.acciones_ret92)
                                   WHERE lr_montos.siefore = 2

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 1
      --------------------------------------------------
      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L10,L10,L5,L1,
                     "\302",L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\302",L10,L2,L2,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\277"

      PRINT
            COLUMN 001,"|",COUNT(*)
                           WHERE l_record.siefore=1 USING "#######",
            COLUMN 013,"|",
            COLUMN 015,"TOTAL  A  LIQUIDAR      :  ",
            COLUMN 070,"|",
            COLUMN 072,"SB1"                                ,
            COLUMN 076,"|",
            COLUMN 077,lr_tot_fin_sb1.total   USING "#######&.&&&&&&" ,

            COLUMN 092,"|",
            COLUMN 093,lr_tot_fin_sb1.ret97   USING "#######&.&&&&&&",
            COLUMN 108,"|",
            COLUMN 109,lr_tot_fin_sb1.cv      USING "#######&.&&&&&&" ,
            COLUMN 124,"|",
            COLUMN 125,lr_tot_fin_sb1.cs      USING "#######&.&&&&&&" ,
            COLUMN 140,"|",
            COLUMN 155,"|",
            COLUMN 170,"|",
            COLUMN 186,"|",
            COLUMN 202,"|",
            COLUMN 203,lr_tot_fin_sb1.ret92   USING "#######&.&&&&&&" ,
            COLUMN 218,"|",
            COLUMN 233,"|"

      PRINT COLUMN 1,"\300",L10,L1,
                     "\301",L10,L10,L10,L10,L10,L5,L1,
                     "\301",L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L2,L2,
                     "\301",L10,L2,L2,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L2,L2,
                     "\331"

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 2
      --------------------------------------------------
      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L10,L10,L5,L1,
                     "\302",L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\302",L10,L2,L2,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\277"
      PRINT
            COLUMN 001,"|",COUNT(*)
                           WHERE l_record.siefore=2 USING "#######",
            COLUMN 013,"|",
            COLUMN 015,"TOTAL  A  LIQUIDAR      :  ",
            COLUMN 070,"|",
            COLUMN 072,"SB2"                                ,
            COLUMN 076,"|",
            COLUMN 077,lr_tot_fin_sb2.total   USING "#######&.&&&&&&" ,

            COLUMN 092,"|",
            COLUMN 093,lr_tot_fin_sb2.ret97   USING "#######&.&&&&&&",
            COLUMN 108,"|",
            COLUMN 109,lr_tot_fin_sb2.cv      USING "#######&.&&&&&&" ,
            COLUMN 124,"|",
            COLUMN 125,lr_tot_fin_sb2.cs      USING "#######&.&&&&&&" ,
            COLUMN 140,"|",
            COLUMN 155,"|",
            COLUMN 170,"|",
            COLUMN 186,"|",
            COLUMN 202,"|",
            COLUMN 203,lr_tot_fin_sb2.ret92   USING "#######&.&&&&&&" ,
            COLUMN 218,"|",
            COLUMN 233,"|"

      PRINT COLUMN 1,"\300",L10,L1,
                     "\301",L10,L10,L10,L10,L10,L5,L1,
                     "\301",L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L2,L2,
                     "\301",L10,L2,L2,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L2,L2,
                     "\331"

             ---------------------------------------------------
             -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
             ---------------------------------------------------
             PRINT COLUMN 1,"\332",L10,L1,
                            L10,L10,L10,L10,L10,L5,L1,L1,
                            L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            "\302",L10,L2,L2,
                            "\302",L10,L2,L2,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\277"

                 PRINT
                     COLUMN 001,"|",
                     COLUMN 015,"TOTAL DE NSS UNICOS : ",cont_nss_fin USING "#####",
                     COLUMN 140,"|",
                     COLUMN 155,"|",
                     COLUMN 170,"|",
                     COLUMN 186,"|",
                     COLUMN 202,"|",
                     COLUMN 218,"|",
                     COLUMN 233,"|"

             PRINT COLUMN 1,"\300",L10,L1,
                            L10,L10,L10,L10,L10,L5,L1,L1,
                            L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            "\301",L10,L2,L2,
                            "\301",L10,L2,L2,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
                            "\331"
     }
END REPORT


#############################################################################
FUNCTION habil_siguiente(diaActual,numDiaHabil)
#hs--------------------------------------------
   DEFINE
       diaTmp                    ,
       diaHabilSig            ,
       diaActual            DATE

   DEFINE
       cont_1                   ,
       numDiaHabil              ,

       diaSemana            ,
       feriado                    ,
       finSemana            SMALLINT

   LET cont_1      = 0
   LET diaHabilSig = diaActual

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilSig)

       IF diaSemana = 0 OR diaSemana = 6 THEN
             LET finSemana = 1
       ELSE
           SELECT *
           FROM   tab_feriado
           WHERE  feria_fecha = diaHabilSig

           IF STATUS <> NOTFOUND THEN
               LET feriado = 1
           END IF
       END IF

       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilSig = diaHabilSig + 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilSig = diaHabilSig + 1 UNITS DAY
           END IF
       END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION






{
        DECLARE cur_montos CURSOR FOR
        SELECT A.acciones_ret97                 ,
               A.acciones_cv                    ,
               A.acciones_cs                    ,
               A.acciones_ret92                 ,
               A.siefore                        ,
        FROM   ret_monto_siefore   A,
               ret_solicitud_saldo B,
               glo_valor_accion    C
        WHERE  A.nss             = l_record.nss               --condicion
        AND    a.consecutivo     = l_record.consecutivo_lote  --condicion
        AND    A.siefore         = l_record.siefore           --condicion
        AND    A.nss             = B.nss                      --join 1
        AND    A.consecutivo     = B.consecutivo              --Join 1
        AND    A.folio           = B.folio                    --Join 1
        AND    C.fecha_valuacion = B.fecha_valor_viv          --Join 2
        AND    C.codigo_siefore  = 11

        FOREACH cur_montos INTO lr_montos.acciones_ret97   ,
                                lr_montos.acciones_cv      ,
                                lr_montos.acciones_cs      ,
                                lr_montos.acciones_ret92   ,
                                lr_montos.siefore          ,
            LET v_bandera = 1 --si encontro solicitudes con saldos

            IF l_record.nss = "11947402993" THEN
               LET lr_montos.siefore = 11
            END IF

            --SELECCION DEL DIAGNOSTICO y  ESTADO DE SUBCUENTA DE VIVIENDA

            SELECT diag_registro
            INTO   lr_sol_rx.diag_registro
            FROM   ret_solicitud_rx
            WHERE  nss         = l_record.nss
            AND    consecutivo = l_record.consecutivo_lote

            IF SQLCA.SQLCODE = NOTFOUND THEN
               INITIALIZE lr_sol_rx.* TO NULL
            END IF

            MESSAGE " GENERANDO DETALLE  " ATTRIBUTE(REVERSE)

            OUTPUT TO REPORT rpt_cuenta_imp(l_record.*   ,
                                            lr_montos.*  ,
                                            lr_sol_rx.*  ,
                                            f_opera      )
        END FOREACH --foreach de montos
}
################################################################################
REPORT rpt_cza_prov(l_record,lr_montos,v_diag_registro,f_opera2)

   DEFINE l_record RECORD
        tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro    ,
        nss                   LIKE dis_provision.nss               ,
        folio                 LIKE dis_provision.folio             ,
        consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
        siefore               LIKE dis_provision.siefore
   END RECORD

   DEFINE lr_montos   RECORD
        acciones_ret97        LIKE ret_monto_siefore.acciones_ret97 ,
        acciones_cv           LIKE ret_monto_siefore.acciones_cv    ,
        acciones_cs           LIKE ret_monto_siefore.acciones_cs    ,
        acciones_ret92        LIKE ret_monto_siefore.acciones_ret92 ,
        siefore               LIKE ret_monto_siefore.siefore
   END RECORD

   DEFINE v_diag_registro     LIKE ret_solicitud_saldo.diag_recep_afore

   DEFINE r_datnss  RECORD
        monto72               LIKE dis_provision.monto_en_pesos  ,
        nombre_afore          LIKE ret_transf_rx.nombre_afore    ,
        paterno_afore         LIKE ret_transf_rx.paterno_afore   ,
        materno_afore         LIKE ret_transf_rx.materno_afore   ,
        diag_registro         LIKE ret_transf_rx.diag_registro
   END RECORD

   DEFINE f_opera2            DATE
   DEFINE hoy                 DATE
   DEFINE nombre_siefore      CHAR(03)
   DEFINE r_nombre            CHAR(60)
   DEFINE r_nom_rep           CHAR(07)
   DEFINE cont_tipo_retiro1   SMALLINT
   DEFINE cont_tipo_retiro2   SMALLINT
   DEFINE ind                 SMALLINT
   DEFINE i                   SMALLINT

   DEFINE rpt_afore RECORD
          codigo_afore        LIKE safre_af:tab_afore_local.codigo_afore ,
          razon_social        LIKE safre_af:tab_afore_local.razon_social
   END RECORD

   DEFINE reg_s1, reg_s2      ARRAY[6] OF RECORD
         tot                  INTEGER,
         tipo_ret             CHAR(1),
         total                DECIMAL(16,6),
         r97                  DECIMAL(16,6),
         cv                   DECIMAL(16,6),
         cs                   DECIMAL(16,6),
         r92                  DECIMAL(16,6)
   END RECORD

   DEFINE reg             ARRAY[6] OF RECORD
        cont_nss_uni      SMALLINT
   END RECORD

   DEFINE lr_tot_fin_sb1 ,
           lr_tot_fin_sb2 RECORD
        total             DECIMAL(16,6),
        ret97             DECIMAL(16,6),
        cv                DECIMAL(16,6),
        cs                DECIMAL(16,6),
        ret92             DECIMAL(16,6)
   END RECORD

   DEFINE
        encabezado           CHAR(30) ,
        nombre_final         CHAR(50) ,
        L1                   CHAR(01) ,
        L2                   CHAR(02) ,
        L5                   CHAR(05) ,
        L10                  CHAR(10)

   DEFINE
        cont_nss_fin         ,
        cont_nss_unicos      SMALLINT

   DEFINE v_total           DECIMAL(16,6)

   DEFINE lc_sql       CHAR(500)
   DEFINE ls_subcuenta SMALLINT
   DEFINE ls_siefore   SMALLINT
   DEFINE ld_pesos     DECIMAL(22,6)
   DEFINE ld_acciones  DECIMAL(22,6)

   DEFINE ls_cont SMALLINT

   OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  200
        PAGE LENGTH   1

   FORMAT

   ON EVERY ROW
      LET r_nom_rep = "RETC706"
      
      SELECT codigo_afore           ,
             razon_social
      INTO   rpt_afore.codigo_afore ,
             rpt_afore.razon_social
      FROM safre_af:tab_afore_local
           
      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'

      IF l_record.folio = 0 THEN
          LET f_opera = f_opera2
      ELSE
          SELECT fecha_genera
          INTO   f_opera
          FROM   ret_ctr_envio_lote
          WHERE  folio = l_record.folio
      END IF

      CALL habil_siguiente(f_opera,3) RETURNING f_trans

      IF rpt_afore.codigo_afore = 532 THEN
          LET encabezado = "SUBDIRECCION DE BENEFICIOS"
      ELSE
          LET encabezado = "   MODULO DE RETIROS      "
      END IF

      PRINT COLUMN 067,encabezado
      SKIP 1 LINE

      PRINT COLUMN 054,"   * REPORTE DE PROVISION DE DISPOSICION DE RECURSOS *"
      SKIP 2 LINE

      PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                              USING "###","  ",
                                              rpt_afore.razon_social CLIPPED,
            COLUMN 126,"PAGINA              : 1"

      PRINT
      PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
            COLUMN 126,"FECHA VAL. TRANSFER : ",f_trans USING "DD-MM-YYYY"

      PRINT
      PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<<<<<",
            COLUMN 126,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY"

      PRINT
      PRINT COLUMN 002,"FECHA DE OPERACION  : ",f_opera USING "DD-MM-YYYY"

      PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

      PRINT COLUMN 1,"\332",L5,
                     "\302",L10,L5,L1,
                     "\302",L10,L5,L1,
                     "\302",L10,L5,L1,
                     "\302",L10,L5,L1,
                     "\302",L10,L5,L1,
                     "\302",L10,L5,L1,
                     "\302",L10,L5,L1,
                     "\302",L10,L5,L1,
                     "\302",L10,L5,L1,
                     "\277"

      PRINT COLUMN 1,"|SIEF.|      TOTAL     |      R97       ",
                     "|        CV      |      CS        |  V I V  97     ",
                     "|  V I V  92     |P A R T  VIV97  |P A R T  VIV92  ",
                     "|      R92       |"

      PRINT COLUMN 1,"|     |     ACCIONES   |    ACCIONES    ",
                     "|     ACCIONES   |    ACCIONES    |     PESOS      ",
                     "|     PESOS      |PARTICIPACIONES |PARTICIPACIONES ",
                     "|    ACCIONES    |"

      PRINT COLUMN 1,"\300",L5,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\331"

      #totales por siefore
      LET lc_sql = " SELECT subcuenta             ,          ",
                   "        siefore               ,          ",
                   "        SUM(monto_en_pesos)   ,          ",
                   "        SUM(monto_en_acciones)           ",
                   " FROM   dis_provision                    ",
                   " WHERE  ", where_clause CLIPPED,
                   " AND    subcuenta       IN (1,2,5,6,7,9) ",
                   " AND    tipo_movimiento IN (921)         ",
                   " GROUP BY 1,2                            ",
                   " ORDER BY 1,2                            "
      PREPARE eje_totales FROM lc_sql
      DECLARE cur_totales CURSOR FOR eje_totales

      FOREACH cur_totales INTO ls_subcuenta,
      	                       ls_siefore  ,
      	                       ld_pesos    ,
      	                       ld_acciones

         CASE
            WHEN ls_subcuenta = 1
               LET gar_totales[ls_siefore].ret97 = gar_totales[ls_siefore].ret97 + ld_acciones
            WHEN ls_subcuenta = 2 OR
                 ls_subcuenta = 6 OR
                 ls_subcuenta = 9
                 LET gar_totales[ls_siefore].cv = gar_totales[ls_siefore].cv + ld_acciones
            WHEN ls_subcuenta = 5
                 LET gar_totales[ls_siefore].cs = gar_totales[ls_siefore].cs + ld_acciones
            WHEN ls_subcuenta = 7
                 LET gar_totales[ls_siefore].ret92 = gar_totales[ls_siefore].ret92 + ld_acciones
         END CASE
      END FOREACH

      FOR ls_cont = 1 TO 13
         LET gar_totales[ls_cont].total = gar_totales[ls_cont].ret97     +
                                          gar_totales[ls_cont].cv        +
                                          gar_totales[ls_cont].cs        +
                                          gar_totales[ls_cont].viv97_acc +
                                          gar_totales[ls_cont].viv92_acc +
                                          gar_totales[ls_cont].ret92
      END FOR

      FOR ls_cont = 1 TO 13
      	 IF gar_totales[ls_cont].total <> 0 THEN
            PRINT COLUMN 003, ls_cont                          USING "##&"             ,
                  COLUMN 008, gar_totales[ls_cont].total       USING "########&.&&&&&&",
                  COLUMN 025, gar_totales[ls_cont].ret97       USING "########&.&&&&&&",
                  COLUMN 042, gar_totales[ls_cont].cv          USING "########&.&&&&&&",
                  COLUMN 059, gar_totales[ls_cont].cs          USING "########&.&&&&&&",
                  COLUMN 076, gar_totales[ls_cont].viv97_pesos USING "########&.&&&&&&",
                  COLUMN 093, gar_totales[ls_cont].viv92_pesos USING "########&.&&&&&&",
                  COLUMN 110, gar_totales[ls_cont].viv97_acc   USING "########&.&&&&&&",
                  COLUMN 127, gar_totales[ls_cont].viv92_acc   USING "########&.&&&&&&",
                  COLUMN 144, gar_totales[ls_cont].ret92       USING "########&.&&&&&&"
         END IF
      END FOR
      
      PRINT COLUMN 1,"\300",L5,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\301",L10,L5,L1,
                     "\331"

END REPORT
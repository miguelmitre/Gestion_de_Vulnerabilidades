################################################################################
# Proyecto          => Sistema de Afores. ( MEXICO )                           #
# Propietario       => E.F.P.                                                  #
# Programa RETL705  => Reporte de Liquidación en Pesos de Recursos a Desinvertir 
# Fecha             => 25 DE OCTUBRE DEL 2011                                  #
# Por               => ISAI JIMENEZ ROJAS                                      #
# Actualizo         => JGHM                                                    #
# Fecha Actualiza   => 02/05/2012                                              #
# Sistema           => Retiros Ventanilla 2.5                                  #
# Actualizo         => JLNJ                                                    #
# Fecha Actualiza   => 19/11/2013                                              #
# Sistema           => Actualización por adecuación al nvo modelo de Servicios #
#                     en Línea MLM-2180                                        #
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
    DEFINE G_IMPRE_NSS        CHAR(0300)
    DEFINE G_IMPRE_SIEF       CHAR(0300)
    DEFINE impresion          CHAR(0300)
    DEFINE where_clause       CHAR(0250)
    DEFINE g_instruccion      CHAR(1000)
    DEFINE ejecuta            CHAR(0500)

    DEFINE v                  RECORD
           fecha_corte        DATE
           END RECORD

    DEFINE vparametros RECORD
           vfolio             INTEGER ,
           vfecha             DATE
           END RECORD

    DEFINE precio_de_liq   DECIMAL(19,14)

    DEFINE  gar_precios             ARRAY[5] OF RECORD
            siefore                 SMALLINT,
            precio                  LIKE glo_valor_accion.precio_del_dia
    END RECORD
    DEFINE   gd_fecha_liquidacion    LIKE  dis_cuenta.fecha_conversion
    
     DEFINE g_tot_siefore ARRAY[15] OF RECORD
           siefore       SMALLINT,
           total         ,
           ret97         ,
           cv            ,
           cs            ,
           viv97         ,
           viv92         ,
           part_viv97    ,
           part_viv92    ,
           ret92         DECIMAL(16,4)
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

    CALL STARTLOG(FGL_GETENV("USER")||".RETL707.log")  
    
    CALL inicializa()
    
    CALL Consulta()

    PROMPT "  PROCESO FINALIZADO...  <ENTER>  PARA SALIR " FOR CHAR g_enter
      
END MAIN

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION inicializa()

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
 
END FUNCTION

#==============================================================================#
# FUNCTION QUE ABRE LA PANTALLA DE CAPTURA DE CRITERIO
#==============================================================================#
FUNCTION Consulta()

    DEFINE where_clause          CHAR(250)
    DEFINE v_permisos            CHAR(110) 
    DEFINE v_cont_reg            INTEGER 
    DEFINE l_record RECORD
           tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro    ,
           nss                   LIKE dis_cuenta.nss               ,
           folio                 LIKE dis_cuenta.folio             ,
           consecutivo_lote      LIKE dis_cuenta.consecutivo_lote  ,
           siefore               LIKE dis_cuenta.siefore           
           END RECORD
    DEFINE lr_montos   RECORD
           pesos_ret97           LIKE dis_cuenta.monto_en_pesos,
           pesos_cv              LIKE dis_cuenta.monto_en_pesos,
           pesos_cs              LIKE dis_cuenta.monto_en_pesos,
           pesos_ret92           LIKE dis_cuenta.monto_en_pesos,
           siefore               LIKE ret_monto_siefore.siefore        
           END RECORD
    DEFINE v_diag_registro       LIKE ret_solicitud_saldo.diag_recep_afore
    DEFINE v_mensaje             CHAR(80)
    DEFINE v_contador            INTEGER
    
    DEFINE lr_montos_tmp         RECORD       
           subcuenta             INTEGER,     
           siefore               SMALLINT,    
           monto_en_pesos        LIKE dis_cuenta.monto_en_pesos
           END RECORD                     
    
    DEFINE l_siefore             ,
           l_subcuenta           ,
           l_cont                SMALLINT,
           l_monto               DECIMAL(18,4)        
    ----------------------------------------------------------------------------
    
    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 4,4 WITH FORM "RETL7051" ATTRIBUTE( BORDER)
    DISPLAY "RETL707                       (Ctrl-C) Salir                (ESC) Ejecutar     " AT 3,1 ATTRIBUTE(REVERSE,green)

    LET INT_FLAG = FALSE

    --CRITERIO DE BUSQUEDA POR FOLIO Y FECHA CONVERSION
    CONSTRUCT BY NAME where_clause ON dis_cuenta.folio,
                                      dis_cuenta.fecha_conversion 

       ON KEY(ESC)
           LET int_flag =FALSE  
           EXIT  CONSTRUCT
       
       ON KEY(CONTROL-C)
           LET int_flag = TRUE
           EXIT CONSTRUCT 
 
        AFTER CONSTRUCT
           -- FORZA A QUE SE ESTABLEZCA UN CRITERIO DE BUSQUEDA
           IF NOT INT_FLAG THEN
              IF NOT FIELD_TOUCHED(dis_cuenta.folio, dis_cuenta.fecha_conversion) THEN
                 ERROR "DEBE ESTABLECER UN CRITERIO DE BUSQUEDA"
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
                        " FROM  safre_af:dis_cuenta ",
                        " WHERE ",where_clause CLIPPED,
                        " AND   subcuenta IN (1,2,5,6,7,9)  "

    PREPARE cust_stmt1 FROM g_instruccion
    EXECUTE cust_stmt1 INTO v_contador

    IF v_contador = 0 THEN
       MESSAGE "NO EXISTE INFORMACION CON EL CRITERIO SELECCIONADO..." ATTRIBUTE(REVERSE)
       PROMPT  "PRESIONE <ENTER> PARA SALIR: " FOR CHAR g_enter
       EXIT PROGRAM
    END IF 
  
    CALL   fn_PrecioAccion(where_clause) 
    -----------------------------------------------------------------
    -- PREPARA LA GENERACION DEL REPORTE
    -----------------------------------------------------------------
    LET G_IMPRE      = g_param_dis.ruta_listados CLIPPED,
                       "/", g_usuario CLIPPED,
                       ".RPT_LIQPES_DES",HOY USING"DDMMYYYY" CLIPPED
                  
    LET G_IMPRE_SIEF = g_param_dis.ruta_listados CLIPPED,
                       "/", g_usuario CLIPPED,
                       ".REC_DESINV_TOT_SIEF"
                       
    LET G_IMPRE_NSS  = g_param_dis.ruta_listados CLIPPED,
                     "/", g_usuario CLIPPED,
                     ".REC_DESINV_NSS"

    START REPORT rpt_total_siefore TO G_IMPRE_SIEF
    START REPORT rpt_cuenta_imp    TO G_IMPRE_NSS

    MESSAGE  "PROCESANDO INFORMACION... " ATTRIBUTE(REVERSE)
    SLEEP 1
    
    # MLM-2180 Se agrega el encabezado para los totales por SIEFORE
    -----------------------------------------------------------------
    -- SELECCIONA LA INFORNACION REQUERIDA PARA EL ENCABEZADO
    -- DE TOTALES POR SIEFORE
    -----------------------------------------------------------------
    LET g_instruccion = " SELECT siefore, subcuenta,sum(monto_en_pesos) ",
                        " FROM   safre_af:dis_cuenta ",
                        " WHERE  ",where_clause CLIPPED,   
                        " AND    subcuenta IN (1,2,5,6,7,9) ",
                        " GROUP  BY 1,2 ",
                        " ORDER  BY 1,2 "
                        
    PREPARE cust_sief FROM g_instruccion
    DECLARE cur_sief  CURSOR FOR cust_sief
    # Inicializando el arreglo
    FOR l_cont = 1 TO 15
       INITIALIZE g_tot_siefore[l_cont].* TO NULL
       
       LET g_tot_siefore[l_cont].total      = 0
       LET g_tot_siefore[l_cont].ret97      = 0
       LET g_tot_siefore[l_cont].cv         = 0
       LET g_tot_siefore[l_cont].cs         = 0
       LET g_tot_siefore[l_cont].viv97      = 0
       LET g_tot_siefore[l_cont].viv92      = 0
       LET g_tot_siefore[l_cont].part_viv97 = 0
       LET g_tot_siefore[l_cont].part_viv92 = 0
    END FOR 
    
    FOREACH cur_sief INTO l_siefore, 
                          l_subcuenta,
                          l_monto
       LET g_tot_siefore[l_siefore].siefore = l_siefore
       LET g_tot_siefore[l_siefore].total   = g_tot_siefore[l_siefore].total + l_monto
       CASE
          WHEN l_subcuenta = 1    # Retiro 91
               LET g_tot_siefore[l_siefore].ret97 = l_monto
          WHEN l_subcuenta = 2 OR # Cesantia y Vejez
               l_subcuenta = 6 OR
               l_subcuenta = 9
               LET g_tot_siefore[l_siefore].cv    = g_tot_siefore[l_siefore].cv + l_monto
          WHEN l_subcuenta = 5    # Cuota Social
               LET g_tot_siefore[l_siefore].cs    = l_monto
          WHEN l_subcuenta = 7    # Retiro 92
               LET g_tot_siefore[l_siefore].ret92 = l_monto
       END CASE
    END FOREACH
    
    OUTPUT TO REPORT rpt_total_siefore(l_record.*)             # MLM-2180 Se agrega el parámetro para saber que se trata de los totales por SIEFORE
    FINISH REPORT rpt_total_siefore  
    # FIN MLM-2180

    -----------------------------------------------------------------
    -- SELECCIONA LA INFORNACION REQUERIDA PARA EL REPORTE
    -----------------------------------------------------------------

    LET g_instruccion = "SELECT nss,folio,consecutivo_lote, siefore ",
                        " FROM  safre_af:dis_cuenta  ",
                        " WHERE ",where_clause CLIPPED,
                        " AND   subcuenta IN (1,2,5,6,7,9) ",
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

        MESSAGE " SELECCIONANDO MONTOS... " ATTRIBUTE(REVERSE)
        
        --OBTIENE LOS MONTOS POR CADA SUBCUENTA A DETALLE DE CADA SOLICITUD
        DECLARE cur_monto CURSOR FOR
        SELECT subcuenta, siefore, SUM(monto_en_pesos)
        FROM   safre_af:dis_cuenta
        WHERE  nss              = l_record.nss
        AND    folio            = l_record.folio
        AND    consecutivo_lote = l_record.consecutivo_lote
        AND    siefore          = l_record.siefore
        AND    subcuenta        IN(1,2,6,9,5,7)
        GROUP  BY 1,2
        
        --INICIALIZA MONTOS
        LET lr_montos.pesos_ret97    = 0
        LET lr_montos.pesos_cv       = 0
        LET lr_montos.pesos_cs       = 0
        LET lr_montos.pesos_ret92    = 0
        
        FOREACH cur_monto INTO lr_montos_tmp.*
        
           CASE
              WHEN lr_montos_tmp.subcuenta = 1
                   LET lr_montos.pesos_ret97 = lr_montos.pesos_ret97 + lr_montos_tmp.monto_en_pesos
              WHEN lr_montos_tmp.subcuenta = 2 OR
                   lr_montos_tmp.subcuenta = 6 OR
                   lr_montos_tmp.subcuenta = 9
                   LET lr_montos.pesos_cv    = lr_montos.pesos_cv    + lr_montos_tmp.monto_en_pesos
              WHEN lr_montos_tmp.subcuenta = 5
                   LET lr_montos.pesos_cs    = lr_montos.pesos_cs    + lr_montos_tmp.monto_en_pesos
              WHEN lr_montos_tmp.subcuenta = 7
                   LET lr_montos.pesos_ret92 = lr_montos.pesos_ret92 + lr_montos_tmp.monto_en_pesos
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

    FINISH REPORT rpt_cuenta_imp

    MESSAGE ""
    
    # Concatenando los reportes de Total por Siefore y Detalle por NSS
    
    LET ejecuta = "cd ",g_param_dis.ruta_rescate  CLIPPED,    
               "; cat ", G_IMPRE_SIEF CLIPPED, " ",
               G_IMPRE_NSS CLIPPED, " > ", G_IMPRE      
    
    RUN ejecuta     
    
    LET ejecuta = "cd ",g_param_dis.ruta_rescate  CLIPPED,    
               "; rm ", G_IMPRE_NSS CLIPPED,
               "; rm ", G_IMPRE_SIEF CLIPPED

    RUN ejecuta
      
    LET v_permisos = " chmod 777 ",G_IMPRE
    RUN v_permisos

    LET v_mensaje = "Se generó el archivo: ",G_IMPRE CLIPPED    
    MESSAGE v_mensaje ATTRIBUTE(REVERSE)
    
    PROMPT " DESEA GENERAR IMPRESION (S/N)?: " FOR CHAR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
      LET impresion = "lp ",G_IMPRE
      RUN impresion
    END IF
    
END FUNCTION


FUNCTION  fn_PrecioAccion(lc_where)
 DEFINE   li_siefore                ,
          li_cont_siefore        SMALLINT,
          ld_precio              LIKE  glo_valor_accion.precio_del_dia

 DEFINE   lc_mensaje             CHAR(100)
 DEFINE   v_contador             INTEGER
 DEFINE   lc_where               CHAR(250)

 ## Obtener fecha de liquidacion 
 LET g_instruccion = " SELECT  fecha_conversion, COUNT(*) ",
                     "   FROM  safre_af:dis_cuenta ",
                     "  WHERE  ", lc_where CLIPPED,
                     "    AND  subcuenta IN (1,2,5,6,7,9)  ",
                     "  GROUP  BY 1 "

 PREPARE  p_SelFec   FROM g_instruccion
 DECLARE  d_SelFec   CURSOR  FOR  p_SelFec
 
 FOREACH  d_SelFec          INTO  gd_fecha_liquidacion, v_contador
     -- Se queda con la ultima   
 END FOREACH 

 #Obtener precios de accion
 DECLARE  cur_precios     CURSOR  FOR
  SELECT  codigo_siefore,
          precio_del_dia
    FROM  glo_valor_accion
   WHERE  fecha_valuacion      =  gd_fecha_liquidacion
     AND  codigo_siefore      IN  (1,2,3,4,5)
   ORDER  BY 1

 LET      li_cont_siefore      =  0
 FOREACH  cur_precios       INTO  li_siefore,
 	                          ld_precio
      LET  gar_precios[li_siefore].siefore =  li_siefore
      LET  gar_precios[li_siefore].precio  =  ld_precio
      LET  li_cont_siefore                 =  li_cont_siefore  +  1
 END FOREACH

 IF   li_cont_siefore    < 5  THEN
      LET    lc_mensaje = "FALTA PRECIOS DE ACCION DEL DIA, DE SIEFORES BASICAS DEL DIA:", gd_fecha_liquidacion USING "DD/MM/YYYY"
      PROMPT lc_mensaje FOR CHAR g_enter
      EXIT PROGRAM
 END IF

END FUNCTION

REPORT rpt_total_siefore(l_record)     
   DEFINE 
           encabezado           CHAR(30) ,
           nombre_final         CHAR(50) ,
           L1                   CHAR(01) ,
           L2                   CHAR(02) ,
           L5                   CHAR(05) ,
           L10                  CHAR(10)
   
   DEFINE hoy                 DATE
   DEFINE f_opera2            DATE
   
   DEFINE r_nom_rep           CHAR(07)
   
   DEFINE 
           cont_nss_fin         ,
           ind                  ,
           l_cont               SMALLINT
   
   DEFINE rpt_afore RECORD
             codigo_afore        LIKE safre_af:tab_afore_local.codigo_afore ,
             razon_social        LIKE safre_af:tab_afore_local.razon_social
   END RECORD  
   
   DEFINE l_record RECORD
        tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro    ,
        nss                   LIKE dis_cuenta.nss               ,
        folio                 LIKE dis_cuenta.folio             ,
        consecutivo_lote      LIKE dis_cuenta.consecutivo_lote  ,
        siefore               LIKE dis_cuenta.siefore           
   END RECORD
   
   OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  200
        PAGE LENGTH   25
         
   FORMAT
        
      --------------------
     FIRST PAGE HEADER
     --------------------
           LET ind             = 0
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
           LET r_nom_rep = "RETL705"
           
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
           
           PRINT COLUMN 054,"  REPORTE DE LIQUIDACION EN PESOS DE RECURSOS A DESINVERTIR "
           SKIP 2 LINE
           
           PRINT COLUMN 002,"AFORE               : ", rpt_afore.codigo_afore   USING "###","  ",
                                                      rpt_afore.razon_social CLIPPED,
                 COLUMN 126,"PAGINA              :",  pageno USING "##"
           
           PRINT
           PRINT COLUMN 002,"PROGRAMA            : ", r_nom_rep,
                 COLUMN 126,"FECHA LIQUIDACION   : ", gd_fecha_liquidacion USING "DD-MM-YYYY"
           
           PRINT
           PRINT COLUMN 002,"FOLIO INTERNO       : ", l_record.folio USING "<<<<<<<<<<",
                 COLUMN 126,"FECHA GENERACION    : ", HOY USING "DD-MM-YYYY"
           
           PRINT
           PRINT COLUMN 002,"FECHA DE OPERACION  : ", f_opera USING "DD-MM-YYYY"
          
           PRINT COLUMN 001, "VALOR ACCION SB1: ", gar_precios[1].precio USING "###&.&&&&&&",
                 COLUMN 047, "VALOR ACCION SB2: ", gar_precios[2].precio USING "###&.&&&&&&",
                 COLUMN 093, "VALOR ACCION SB3: ", gar_precios[3].precio USING "###&.&&&&&&",
                 COLUMN 139, "VALOR ACCION SB4: ", gar_precios[4].precio USING "###&.&&&&&&",
                 COLUMN 185, "VALOR ACCION SB5: ", gar_precios[5].precio USING "###&.&&&&&&",
                             '\033015'
          
           PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'
     
     ---------------------------------
     ON EVERY ROW
     ---------------------------------     
            # MLM-2180 Se anexará una sección de encabezado que contendrá los totales por Siefore  de los recursos que fueron liquidados
           
           PRINT COLUMN 1,"\332",L5,
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
                          "\302"
           
           PRINT COLUMN 1,"|SIEF.|",
                          "    IMPORTE    |",
                          "      R97      |",
                          "       CV      |",
                          "      CS       |",
                          "  V I V  97   |",
                          "  V I V  92   |",
                          "P A R T  VIV97 |",
                          "P A R T  VIV92 |",
                          "      R92     |",
                          "              |"
           
           PRINT COLUMN 1,"\332",L5,
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
                          "\302"
           
           FOR l_cont = 1 TO 15
              IF g_tot_siefore[l_cont].siefore IS NOT NULL THEN
                  PRINT COLUMN 003,g_tot_siefore[l_cont].siefore       USING "###"                  ,  
                        COLUMN 008,g_tot_siefore[l_cont].total         USING "#########&.&&&&&&"    ,
                        COLUMN 024,g_tot_siefore[l_cont].ret97         USING "#########&.&&&&&&"    ,
                        COLUMN 040,g_tot_siefore[l_cont].cv            USING "#########&.&&&&&&"    ,
                        COLUMN 056,g_tot_siefore[l_cont].cs            USING "#########&.&&&&&&"    , 
                        COLUMN 072,g_tot_siefore[l_cont].viv97         USING "#########&.&&&&&&"    ,    
                        COLUMN 087,g_tot_siefore[l_cont].viv92         USING "#########&.&&&&&&"    ,
                        COLUMN 102,g_tot_siefore[l_cont].part_viv97    USING "#########&.&&&&&&"    ,
                        COLUMN 118,g_tot_siefore[l_cont].part_viv92    USING "#########&.&&&&&&"    ,
                        COLUMN 134,g_tot_siefore[l_cont].ret92         USING "#########&.&&&&&&"     
              END IF   
           END FOR 
                          
                          
           PRINT COLUMN 1,"\332",L5,
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
                          "\302"
            # FIN MLM-2180
END REPORT
#==============================================================================#
#                                                                              #
#==============================================================================#
REPORT rpt_cuenta_imp(l_record,lr_montos,v_diag_registro,f_opera2)

   DEFINE l_record RECORD
        tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro    ,
        nss                   LIKE dis_cuenta.nss               ,
        folio                 LIKE dis_cuenta.folio             ,
        consecutivo_lote      LIKE dis_cuenta.consecutivo_lote  ,
        siefore               LIKE dis_cuenta.siefore           
   END RECORD

   DEFINE lr_montos   RECORD
        pesos_ret97           LIKE dis_cuenta.monto_en_pesos,
        pesos_cv              LIKE dis_cuenta.monto_en_pesos,
        pesos_cs              LIKE dis_cuenta.monto_en_pesos,
        pesos_ret92           LIKE dis_cuenta.monto_en_pesos,
        siefore               LIKE ret_monto_siefore.siefore        
   END RECORD

   DEFINE v_diag_registro     LIKE ret_solicitud_saldo.diag_recep_afore

   DEFINE r_datnss  RECORD
        monto72               LIKE dis_cuenta.monto_en_pesos     ,
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
        cont_nss_fin         SMALLINT
        
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
           LET r_nom_rep = "RETL705"
           
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
           
           PRINT COLUMN 054,"  REPORTE DE LIQUIDACION EN PESOS DE RECURSOS A DESINVERTIR "
           SKIP 2 LINE
           
           PRINT COLUMN 002,"AFORE               : ", rpt_afore.codigo_afore   USING "###","  ",
                                                      rpt_afore.razon_social CLIPPED,
                 COLUMN 126,"PAGINA              :",  pageno USING "##"
           
           PRINT
           PRINT COLUMN 002,"PROGRAMA            : ", r_nom_rep,
                 COLUMN 126,"FECHA LIQUIDACION   : ", gd_fecha_liquidacion USING "DD-MM-YYYY"
           
           PRINT
           PRINT COLUMN 002,"FOLIO INTERNO       : ", l_record.folio USING "<<<<<<<<<<",
                 COLUMN 126,"FECHA GENERACION    : ", HOY USING "DD-MM-YYYY"
           
           PRINT
           PRINT COLUMN 002,"FECHA DE OPERACION  : ", f_opera USING "DD-MM-YYYY"
          
           PRINT COLUMN 001, "VALOR ACCION SB1: ", gar_precios[1].precio USING "###&.&&&&&&",
                 COLUMN 047, "VALOR ACCION SB2: ", gar_precios[2].precio USING "###&.&&&&&&",
                 COLUMN 093, "VALOR ACCION SB3: ", gar_precios[3].precio USING "###&.&&&&&&",
                 COLUMN 139, "VALOR ACCION SB4: ", gar_precios[4].precio USING "###&.&&&&&&",
                 COLUMN 185, "VALOR ACCION SB5: ", gar_precios[5].precio USING "###&.&&&&&&",
                             '\033015'
 
           PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'
           
           PRINT COLUMN 1,"\332",L10,L1,
                          "\302",L10,L10,L10,L2,L1,
                          "\302",L5,L1,
                          "\302",L5,L2,
                          "\302",L5,L2,
                          "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                                 L10,L10,L10,L5,L5,L1,
           --             "\302",L5,
           --             "\302",L2,L2,
                          "\277"
           
           PRINT COLUMN 1,"|           |",
                          "                                 |",
                          "      |",
                          "       |",
                          "       |",
                          "                                                      MONTOS LIQUIDADOS DE LA CUENTA INDIVIDUAL (PESOS)                                                       |"
           --             "     |",
           --             "    |"
           PRINT COLUMN 1,"|           |",
                          "                                 |",
                          "      |",
                          "       |",
                          "       \303",L5,
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
                          "\302"
           --             "\302",L5,
           --             "\302",L2,L2,"|"
           
           PRINT COLUMN 1,"|    NSS    |",
                          "       NOMBRE DEL TRABAJADOR     |",
                          "      |",
                          "       |",
                          "       |",
                          "SIEF.|",
                          "    IMPORTE    |",
                          "      R97      |",
                          "       CV      |",
                          "      CS       |",
                          "  V I V  97   |",
                          "  V I V  92   |",
                          "P A R T  VIV97 |",
                          "P A R T  VIV92 |",
                          "      R92     |",
                          "              |"
           --             "DIAG.|",
           --             "REG.|"
           
           PRINT COLUMN 1,"|           |",
                          "                                 |",
                          "      |",
                          "       |",
                          "       |",
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
                          "              |"
           --             "     |",
           --             "    |"
           
           PRINT COLUMN 1,"|           |",
                          "                                 |",
                          "      |",
                          "       |",
                          "       |",
                          "     |",
                          "     BRUTO     |",
                          "     PESOS     |",
                          "     PESOS     |",
                          "     PESOS     |",
                          "     PESOS    |",
                          "     PESOS    |",
                          "PARTICIPACIONES|",
                          "PARTICIPACIONES|",
                          "     PESOS    |",
                          "              |"
           --             "     |",
           --             "    |"
           
           
           PRINT COLUMN 1,"\300",L10,L1,
                          "\301",L10,L10,L10,L2,L1,
                          "\301",L5,L1,
                          "\301",L5,L2,
                          "\301",L5,L2,
                          "\301",L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L2,L2,
                          "\301",L10,L2,L2,
                          "\301",L10,L5,
                          "\301",L10,L5,
                          "\301",L10,L2,L2,
                          "\301",L10,L2,L2,
           --             "\301",L5,
           --             "\301",L2,L2,
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
        LET r_nom_rep = "RETL705"

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

        PRINT COLUMN 054,"  REPORTE DE LIQUIDACION EN PESOS DE RECURSOS A DESINVERTIR "
        SKIP 2 LINE

        PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                                USING "###","  ",
                                                rpt_afore.razon_social CLIPPED,
                 COLUMN 126,"PAGINA              :",pageno USING "##"

        PRINT
        PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
              COLUMN 126,"FECHA LIQUIDACION   : ", gd_fecha_liquidacion USING "DD-MM-YYYY"

        PRINT
        PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<<<<<",
              COLUMN 126,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY"

        PRINT
        PRINT COLUMN 002,"FECHA DE OPERACION  : ",f_opera USING "DD-MM-YYYY"
        PRINT COLUMN 001, "VALOR ACCION SB1: ", gar_precios[1].precio USING "###&.&&&&&&",
              COLUMN 047, "VALOR ACCION SB2: ", gar_precios[2].precio USING "###&.&&&&&&",
              COLUMN 093, "VALOR ACCION SB3: ", gar_precios[3].precio USING "###&.&&&&&&",
              COLUMN 139, "VALOR ACCION SB4: ", gar_precios[4].precio USING "###&.&&&&&&",
              COLUMN 185, "VALOR ACCION SB5: ", gar_precios[5].precio USING "###&.&&&&&&",
                          '\033015'

        PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L10,L5,L5,L1,
        --             "\302",L5,
        --             "\302",L2,L2,
                       "\277"

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       "      |",
                       "       |",
                       "       |",
                       "                                                      MONTOS LIQUIDADOS DE LA CUENTA INDIVIDUAL (PESOS)                                                          |"
        --             "     |",
        --             "    |"
        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       "      |",
                       "       |",
                       "       \303",L5,
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
                       "\302"
        --             "\302",L5,
        --             "\302",L2,L2,"|"
    
        PRINT COLUMN 1,"|    NSS    |",
                   "       NOMBRE DEL TRABAJADOR     |",
               "      |",
               "       |",
               "       |",
                       "SIEF.|",
               "    IMPORTE    |",
               "      R97      |",
               "       CV      |",
               "      CS       |",
                       "  V I V  97   |",
                       "  V I V  92   |",
               "P A R T  VIV97 |",
               "P A R T  VIV92 |",
               "      R92     |",
               "              |"
        --             "DIAG.|",
        --     "REG.|"

        PRINT COLUMN 1,"|           |",
                        "                                 |",
                "      |",
                "       |",
                "       |",
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
                "              |"
         --     "     |",
         --     "    |"

         PRINT COLUMN 1,"|           |",
            "                                 |",
            "      |",
            "       |",
            "       |",
                        "     |",
            "     BRUTO     |",
            "     PESOS     |",
            "     PESOS     |",
            "     PESOS     |",
            "     PESOS    |",
            "     PESOS    |",
            "PARTICIPACIONES|",
            "PARTICIPACIONES|",
            "     PESOS    |",
            "              |"
         -- "     |",
         -- "    |"


         PRINT COLUMN 1,"\300",L10,L1,
                        "\301",L10,L10,L10,L2,L1,
                        "\301",L5,L1,
                        "\301",L5,L2,
                        "\301",L5,L2,
                        "\301",L5,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L2,L2,
                        "\301",L10,L2,L2,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L2,L2,
                        "\301",L10,L2,L2,
          --            "\301",L5,
          --            "\301",L2,L2,
                        "\331"

         SKIP 2 LINE

     ---------------------------------
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
          ELSE
              LET  nombre_siefore = "SB2"
          END IF
          
          LET v_total = lr_montos.pesos_ret97 + 
                        lr_montos.pesos_cv  + 
                        lr_montos.pesos_cs  + 
                        lr_montos.pesos_ret92

          PRINT COLUMN 002,l_record.nss                                           ,
                COLUMN 014,r_nombre[1,35]                                         ,
                COLUMN 072,l_record.siefore            USING "###"                ,   --nombre_siefore
                COLUMN 077,v_total                     USING "#######&.&&"    ,
                COLUMN 093,lr_montos.pesos_ret97       USING "#######&.&&"    ,
                COLUMN 109,lr_montos.pesos_cv          USING "#######&.&&"    ,
                COLUMN 125,lr_montos.pesos_cs          USING "#######&.&&"    , 
                COLUMN 203,lr_montos.pesos_ret92       USING "#######&.&&"    ,
                COLUMN 234,v_diag_registro             USING "###"
          
     
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



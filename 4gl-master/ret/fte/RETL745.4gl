################################################################################
#Proyecto:   => SISTEMA DE AFORES.( MEXICO )                                   #
#Programa:   => RETL745 - REPORTE DE LIQUIDACION DE REINVERSION EN ACCIONES    #
#Autor:      => Daniel Buendia, UPGENIA                                        #
#Fecha:      => Noviembre 18, 2011                                             #
#Sistema:    => RET                                                            #
################################################################################
DATABASE safre_af

DEFINE
 enter                CHAR(1),
 mc_razon_social      CHAR(50),
 md_hoy               DATE,
 mc_archivo           CHAR(200),
 mr_captura           RECORD
   folio              INTEGER,
   fecha_liquida_ini  LIKE dis_cuenta.fecha_conversion,
   fecha_liquida_fin  LIKE dis_cuenta.fecha_conversion
 END RECORD,
 mi_query_ctl SMALLINT -- status que indica que tipo de consulta se realizara
                         -- 1 Se ingreso folio y periodo de liquidacion
                         -- 2 Se ingreso folio
                         -- 3 Se ingreso periodo de liquidacion

{ ==========================================================================
Clave: 
Nombre: 
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion principal del Reporte de Liquidacion de Reinversion en acciones, se
solicita el Folio y/o Periodo de Liquidacion para generar el reporte, se
validan los datos y se invoca la funcion que consulta la informacion de la
base de datos

Registro de modificaciones:

============================================================================
}
MAIN
DEFINE
 li_cont_reg  INTEGER, -- contador de registros
 lv_qryTxt    CHAR(500) -- se asigna sentencia sql a ejecutar   
 
 DEFER INTERRUPT
 
 OPTIONS
   INPUT WRAP,
   PROMPT LINE LAST,
   MESSAGE LINE LAST-1,
   ACCEPT KEY CONTROL-I
 
 -- se genera el achivo log
 CALL STARTLOG("RETL745.log")
 
 -- se invoca la funcion que inicializa variables y prepara las sentencias SQL
 CALL f_inicia_variables()
 
 -- se abre forma, la cual pedira el folio y/o periodo de liquidacion
 OPEN WINDOW w_RETL7451 AT 4,4 WITH FORM "RETL7451" ATTRIBUTES (BORDER)
 DISPLAY "                < Ctrl-C > Salir                (ESC) Ejecutar                 " AT 1,1 ATTRIBUTE(REVERSE)
 DISPLAY " RETL745     REPORTE DE LIQUIDACIÓN DE REINVERSIÓN EN ACCIONES                 " AT 3,1 ATTRIBUTE (REVERSE)
 DISPLAY md_hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)
 
 -- se incializa el registro de captura
 INITIALIZE mr_captura.* TO NULL
 
 INPUT BY NAME mr_captura.* WITHOUT DEFAULTS
   AFTER FIELD folio
     -- se valida el folio en caso de haber ingresado
     IF mr_captura.folio IS NOT NULL THEN
       LET lv_qryTxt = " SELECT COUNT(UNIQUE fecha_conversion)\n",
                       " FROM   dis_cuenta\n",
                       " WHERE  folio = ",mr_captura.folio
       
       PREPARE prp_slctCountUniq_fecConv FROM lv_qryTxt
       EXECUTE prp_slctCountUniq_fecConv INTO li_cont_reg
       
       -- si solo se encontro una fecha se consulta y se desplega
       IF li_cont_reg = 1 THEN
         LET lv_qryTxt = " SELECT UNIQUE fecha_conversion\n",
                         " FROM dis_cuenta\n",
                         " WHERE folio = ",mr_captura.folio,"\n",
                         " AND tipo_movimiento = 924\n",
                         " AND subcuenta IN (1,2,4,5,6,7,8,9)"
         
         PREPARE prp_unique_fec_conv FROM lv_qryTxt
         EXECUTE prp_unique_fec_conv INTO mr_captura.fecha_liquida_ini
         
         -- en caso de encontrar la fecha para el folio dado se despliega en pantalla
         IF mr_captura.fecha_liquida_ini IS NOT NULL THEN
           LET mr_captura.fecha_liquida_fin = mr_captura.fecha_liquida_ini
           DISPLAY BY NAME mr_captura.fecha_liquida_ini, mr_captura.fecha_liquida_fin
           
           NEXT FIELD fecha_liquida_fin
         ELSE
           ERROR "NO EXISTE INFORMACIÓN PARA ESTE FOLIO"
           SLEEP 2
           ERROR ""
           
           NEXT FIELD folio
         END IF
       ELSE
         IF li_cont_reg > 1 THEN
           LET mr_captura.fecha_liquida_ini = NULL
           LET mr_captura.fecha_liquida_fin = NULL
           DISPLAY BY NAME mr_captura.fecha_liquida_ini, mr_captura.fecha_liquida_fin
           
           CONTINUE INPUT
         ELSE
           IF li_cont_reg = 0 THEN
             ERROR "NO EXISTE INFORMACIÓN PARA ESTE FOLIO"
             SLEEP 2
             ERROR ""
             
             NEXT FIELD folio
           END IF
         END IF
       END IF
     END IF
   
   AFTER INPUT
     -- se obtienen los valores ingresados por el usuario
     LET mr_captura.folio = GET_FLDBUF(folio)
     LET mr_captura.fecha_liquida_ini = GET_FLDBUF(fecha_liquida_ini)
     LET mr_captura.fecha_liquida_fin = GET_FLDBUF(fecha_liquida_fin)
     
     -- se valida el folio, este no puede ser nulo
     IF mr_captura.folio IS NULL AND mr_captura.fecha_liquida_ini IS NULL AND mr_captura.fecha_liquida_fin IS NULL THEN
       ERROR "DEBE INGRESAR FOLIO O PERIODO PARA CONTINUAR"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD folio
     END IF
     
     -- se valida el periodo de liquidacion (fecha inicial)
     IF mr_captura.fecha_liquida_ini IS NULL AND mr_captura.fecha_liquida_fin IS NOT NULL THEN
       ERROR "DEBE INGRESAR FECHA DE LIQUIDACIÓN INICIAL"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD fecha_liquida_ini
     END IF
     
     -- se valida el periodo de liquidacion (fecha final)
     IF mr_captura.fecha_liquida_ini IS NOT NULL AND mr_captura.fecha_liquida_fin IS NULL THEN
       ERROR "DEBE INGRESAR FECHA DE LIQUIDACIÓN FINAL"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD fecha_liquida_fin
     END IF
     
     -- se valida el periodo de liquidacion (fecha inicial no puede ser mayor a la final)
     IF mr_captura.fecha_liquida_ini > mr_captura.fecha_liquida_fin THEN
       ERROR "EL PERIODO INICIAL NO PUEDE SER MAYOR AL FINAL"
       SLEEP 2
       ERROR ""
       
       CONTINUE INPUT
     END IF
     
     -- se compueba el tipo de consulta que se realizara
     IF mr_captura.folio IS NOT NULL THEN
       IF mr_captura.fecha_liquida_ini IS NOT NULL AND mr_captura.fecha_liquida_fin IS NOT NULL THEN
         LET mi_query_ctl = 1
       ELSE
         LET mi_query_ctl = 2
       END IF
     ELSE
       IF mr_captura.fecha_liquida_ini IS NOT NULL AND mr_captura.fecha_liquida_fin IS NOT NULL THEN
         LET mi_query_ctl = 3
       ELSE
         ERROR "DEBE INGRESAR FOLIO O PERIODO PARA CONTINUAR"
         SLEEP 2
         ERROR ""
         
         NEXT FIELD folio
       END IF
     END IF
     
     -- verifica si se ingreso folio y periodo
     IF mi_query_ctl = 1 THEN
       -- se cuenta el numero de registros para el folio y la fecha dada
       LET lv_qryTxt = " SELECT COUNT(*)\n",
                       " FROM dis_cuenta\n",
                       " WHERE folio = ",mr_captura.folio,"\n",
                       " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                       " AND tipo_movimiento = 924\n",
                       " AND subcuenta IN (1,2,4,5,6,7,8,9)"
     ELSE
       -- verifica si se ingreso folio
       IF mi_query_ctl = 2 THEN
         -- se cuenta el numero de registros para el folio dado
         LET lv_qryTxt = " SELECT COUNT(*)\n",
                         " FROM dis_cuenta\n",
                         " WHERE folio = ",mr_captura.folio,"\n",
                         " AND tipo_movimiento = 924\n",
                         " AND subcuenta IN (1,2,4,5,6,7,8,9)"
       ELSE
         -- se cuenta el numero de registros para la fecha dada
         LET lv_qryTxt = " SELECT COUNT(*)\n",
                         " FROM dis_cuenta\n",
                         " WHERE fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                         " AND tipo_movimiento = 924\n",
                         " AND subcuenta IN (1,2,4,5,6,7,8,9)"
       END IF
     END IF
     
     PREPARE prp_slctCnt_dicCta1 FROM lv_qryTxt
     EXECUTE prp_slctCnt_dicCta1 INTO li_cont_reg
     
     -- en caso de ocurrir un error al consultar los datos, informar a usuario
     IF SQLCA.SQLCODE < 0 THEN
       ERROR "OCURRIÓ UN ERROR AL REALIZAR CONSULTA A BASE DE DATOS"
       
       CONTINUE INPUT
     END IF
     
     -- si el contador es igual a cero informa que no se encontraron registros
     IF li_cont_reg = 0 THEN
       ERROR "NO EXISTE INFORMACIÓN PARA LOS DATOS INGRESADOS"
       SLEEP 2
       ERROR ""
       
       CONTINUE INPUT
     END IF
     
     EXIT INPUT
   
   ON KEY (ESC)
     -- se obtienen los valores ingresados por el usuario
     LET mr_captura.folio = GET_FLDBUF(folio)
     LET mr_captura.fecha_liquida_ini = GET_FLDBUF(fecha_liquida_ini)
     LET mr_captura.fecha_liquida_fin = GET_FLDBUF(fecha_liquida_fin)
     
     -- se valida el folio, este no puede ser nulo
     IF mr_captura.folio IS NULL AND mr_captura.fecha_liquida_ini IS NULL AND mr_captura.fecha_liquida_fin IS NULL THEN
       ERROR "DEBE INGRESAR FOLIO O PERIODO PARA CONTINUAR"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD folio
     END IF
     
     -- se valida el periodo de liquidacion (fecha inicial)
     IF mr_captura.fecha_liquida_ini IS NULL AND mr_captura.fecha_liquida_fin IS NOT NULL THEN
       ERROR "DEBE INGRESAR FECHA DE LIQUIDACIÓN INICIAL"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD fecha_liquida_ini
     END IF
     
     -- se valida el periodo de liquidacion (fecha final)
     IF mr_captura.fecha_liquida_ini IS NOT NULL AND mr_captura.fecha_liquida_fin IS NULL THEN
       ERROR "DEBE INGRESAR FECHA DE LIQUIDACIÓN FINAL"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD fecha_liquida_fin
     END IF
     
     -- se valida el periodo de liquidacion (fecha inicial no puede ser mayor a la final)
     IF mr_captura.fecha_liquida_ini > mr_captura.fecha_liquida_fin THEN
       ERROR "EL PERIODO INICIAL NO PUEDE SER MAYOR AL FINAL"
       SLEEP 2
       ERROR ""
       
       CONTINUE INPUT
     END IF
     
     -- se compueba el tipo de consulta que se realizara
     IF mr_captura.folio IS NOT NULL THEN
       IF mr_captura.fecha_liquida_ini IS NOT NULL AND mr_captura.fecha_liquida_fin IS NOT NULL THEN
         LET mi_query_ctl = 1
       ELSE
         LET mi_query_ctl = 2
       END IF
     ELSE
       IF mr_captura.fecha_liquida_ini IS NOT NULL AND mr_captura.fecha_liquida_fin IS NOT NULL THEN
         LET mi_query_ctl = 3
       ELSE
         ERROR "DEBE INGRESAR FOLIO O PERIODO PARA CONTINUAR"
         SLEEP 2
         ERROR ""
         
         NEXT FIELD folio
       END IF
     END IF
     
     -- verifica si se ingreso folio y periodo
     IF mi_query_ctl = 1 THEN
       -- se cuenta el numero de registros para el folio y la fecha dada
       LET lv_qryTxt = " SELECT COUNT(*)\n",
                       " FROM dis_cuenta\n",
                       " WHERE folio = ",mr_captura.folio,"\n",
                       " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                       " AND tipo_movimiento = 924\n",
                       " AND subcuenta IN (1,2,4,5,6,7,8,9)"
     ELSE
       -- verifica si se ingreso folio
       IF mi_query_ctl = 2 THEN
         -- se cuenta el numero de registros para el folio dado
         LET lv_qryTxt = " SELECT COUNT(*)\n",
                         " FROM dis_cuenta\n",
                         " WHERE folio = ",mr_captura.folio,"\n",
                         " AND tipo_movimiento = 924\n",
                         " AND subcuenta IN (1,2,4,5,6,7,8,9)"
       ELSE
         -- se cuenta el numero de registros para la fecha dada
         LET lv_qryTxt = " SELECT COUNT(*)\n",
                         " FROM dis_cuenta\n",
                         " WHERE fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                         " AND tipo_movimiento = 924\n",
                         " AND subcuenta IN (1,2,4,5,6,7,8,9)"
       END IF
     END IF
     
     PREPARE prp_slctCnt_dicCta2 FROM lv_qryTxt
     EXECUTE prp_slctCnt_dicCta2 INTO li_cont_reg
     
     -- en caso de ocurrir un error al consultar los datos, informar a usuario
     IF SQLCA.SQLCODE < 0 THEN
       ERROR "OCURRIÓ UN ERROR AL REALIZAR CONSULTA A BASE DE DATOS"
       
       CONTINUE INPUT
     END IF
     
     -- si el contador es igual a cero informa que no se encontraron registros
     IF li_cont_reg = 0 THEN
       ERROR "NO EXISTE INFORMACIÓN PARA LOS DATOS INGRESADOS"
       SLEEP 2
       ERROR ""
       
       CONTINUE INPUT
     END IF
     
     EXIT INPUT
     
   ON KEY (INTERRUPT, CONTROL-C)
     PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
     EXIT PROGRAM
 END INPUT
 --CLOSE WINDOW w_RETL7451
 
 DISPLAY " PROCESANDO INFORMACION... "
 
 CALL f_crea_reporte()
 
 DISPLAY " ARCHIVO:", mc_archivo
 PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
END MAIN

{ ==========================================================================
Clave: 
Nombre: f_inicia_variables
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que inicializa las variables globales que se usaran en el reporte

Registro de modificaciones:

============================================================================
}
FUNCTION f_inicia_variables()
DEFINE
 lc_ruta_listados LIKE seg_modulo.ruta_listados
 
 -- se selecciona de base de datos la razon social
 SELECT razon_social
 INTO   mc_razon_social
 FROM   tab_afore_local
 
 -- s
 LET md_hoy = TODAY
 
 SELECT ruta_listados
 INTO   lc_ruta_listados
 FROM   seg_modulo
 WHERE  modulo_cod = 'ret'
 
 #Asignar ruta y nombre del archivo del reporte
 LET mc_archivo = lc_ruta_listados CLIPPED, "/",md_hoy USING"YYYYMMDD",".745"
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_crea_reporte
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta la informacion requerida por el reporte y se invoca al
reporte

Registro de modificaciones:

============================================================================
}
FUNCTION f_crea_reporte()
DEFINE
 lr_montos RECORD
   folio            LIKE dis_cuenta.folio, -- numero de folio
   nss              LIKE dis_cuenta.nss, -- numero de seguridad social
   consecutivo_lote LIKE dis_cuenta.consecutivo_lote, -- consecutivo lote
   siefore          LIKE dis_cuenta.siefore -- numero de siafore
 END RECORD,
 lr_rpt_liq_reinv            RECORD
   folio             LIKE dis_cuenta.folio,
   nss               CHAR(11),
   consecutivo       INTEGER,
   siefore           SMALLINT,
   regimen           CHAR(2),
   nombre            VARCHAR(100),
   titulos_retencion DECIMAL(22,6),
   titulos_r97       DECIMAL(22,6),
   titulos_cv        DECIMAL(22,6),
   titulos_cs        DECIMAL(22,6),
   titulos_sar_92    DECIMAL(22,6),
   titulos_viv97     DECIMAL(22,6),
   titulos_viv92     DECIMAL(22,6),
   titulos_rcv       DECIMAL(22,6)
 END RECORD,
 li_siefore          SMALLINT,
 ld_precio           LIKE glo_valor_accion.precio_del_dia,
 lv_qryTxt           VARCHAR(255), -- se asigna sentencia sql a ejecutar
 lv_mensaje          VARCHAR(100), -- mensaje a mostrar a usuario
 lv_respuesta        VARCHAR(100), -- respuesta de una confirmacion
 lv_comando          VARCHAR(100)  -- comando unix a ejecutar
 
 -- se genera el reporte
 START REPORT rpt_liquida TO mc_archivo
 
 -- se inicializa el registro de montos
 INITIALIZE lr_montos.* TO NULL
 
 -- se inciializan los titulos
 LET lr_rpt_liq_reinv.titulos_retencion  = 0
 LET lr_rpt_liq_reinv.titulos_r97      = 0
 LET lr_rpt_liq_reinv.titulos_cv      = 0
 LET lr_rpt_liq_reinv.titulos_cs   = 0
 LET lr_rpt_liq_reinv.titulos_sar_92     = 0
 LET lr_rpt_liq_reinv.titulos_viv97      = 0
 LET lr_rpt_liq_reinv.titulos_viv92      = 0
 LET lr_rpt_liq_reinv.titulos_rcv        = 0
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se consultan los datos de dis cuenta para el folio y la fecha dada
   LET lv_qryTxt = " SELECT UNIQUE folio, nss, consecutivo_lote, siefore\n",
                   " FROM dis_cuenta\n",
                   " WHERE folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta IN (1,2,4,5,6,7,8,9)\n",
                   "ORDER BY 1, 4, 2"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se consultan los datos de dis cuenta para el folio dado
     LET lv_qryTxt = " SELECT UNIQUE folio, nss, consecutivo_lote, siefore\n",
                     " FROM dis_cuenta\n",
                     " WHERE folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (1,2,4,5,6,7,8,9)\n",
                     "ORDER BY 1, 4, 2"
   ELSE
     -- se consultan los datos de dis cuenta para la fecha dada
     LET lv_qryTxt = " SELECT UNIQUE folio, nss, consecutivo_lote, siefore\n",
                     " FROM dis_cuenta\n",
                     " WHERE fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (1,2,4,5,6,7,8,9)\n",
                     "ORDER BY 1, 4, 2"
   END IF
 END IF
     
 PREPARE eje_montos FROM lv_qryTxt
 DECLARE cur_montos CURSOR FOR eje_montos
 
 FOREACH cur_montos INTO lr_montos.folio, lr_montos.nss, lr_montos.consecutivo_lote, lr_montos.siefore
   -- se asignan los datos obtenidos de dis cuenta en el registro del reporte
   LET lr_rpt_liq_reinv.folio = lr_montos.folio
   LET lr_rpt_liq_reinv.nss = lr_montos.nss
   LET lr_rpt_liq_reinv.consecutivo = lr_montos.consecutivo_lote
   LET lr_rpt_liq_reinv.siefore = lr_montos.siefore
   LET lr_rpt_liq_reinv.regimen = ""
   
   -- se invoca la funcion que obtiene el nombre del afiliado, para el NSS en proceso
   LET lr_rpt_liq_reinv.nombre = f_obt_nombre_compl(lr_rpt_liq_reinv.nss)
   
   -- se invoca la funcion que obtiene el monto en titulos de la subcuenta R97 (1)
   LET lr_rpt_liq_reinv.titulos_r97 = f_obt_montotitulos_r97(lr_rpt_liq_reinv.nss, lr_rpt_liq_reinv.consecutivo, lr_rpt_liq_reinv.siefore)
   
   -- se invoca la funcion que obtiene el monto en titulos de la subcuenta CV (2,6,9)
   LET lr_rpt_liq_reinv.titulos_cv = f_obt_montotitulos_cv(lr_rpt_liq_reinv.nss, lr_rpt_liq_reinv.consecutivo, lr_rpt_liq_reinv.siefore)
   
   -- se invoca la funcion que obtiene el monto en titulos de la subcuenta VIV97 (4)
   LET lr_rpt_liq_reinv.titulos_viv97 = f_obt_montotitulos_viv97(lr_rpt_liq_reinv.nss, lr_rpt_liq_reinv.consecutivo, lr_rpt_liq_reinv.siefore)
   
   -- se invoca la funcion que obtiene el monto en titulos de la subcuenta VIV92 (8)
   LET lr_rpt_liq_reinv.titulos_viv92 = f_obt_montotitulos_viv92(lr_rpt_liq_reinv.nss, lr_rpt_liq_reinv.consecutivo, lr_rpt_liq_reinv.siefore)
   
   -- se invoca la funcion que obtiene el monto en titulos de la subcuenta CS (5)
   LET lr_rpt_liq_reinv.titulos_cs = f_obt_montotitulos_cs(lr_rpt_liq_reinv.nss, lr_rpt_liq_reinv.consecutivo, lr_rpt_liq_reinv.siefore)
   
   -- se invoca la funcion que obtiene el monto en titulos de la subcuenta R92 (7)
   LET lr_rpt_liq_reinv.titulos_sar_92 = f_obt_montotitulos_r92(lr_rpt_liq_reinv.nss, lr_rpt_liq_reinv.consecutivo, lr_rpt_liq_reinv.siefore)
   
   --  se invoca la funcion que obtiene el monto en titulos de la retencion
   LET lr_rpt_liq_reinv.titulos_retencion = f_obt_montotitulos_retenc(lr_rpt_liq_reinv.nss, lr_rpt_liq_reinv.consecutivo, lr_rpt_liq_reinv.siefore)
   
   #RCV
   LET lr_rpt_liq_reinv.titulos_rcv = lr_rpt_liq_reinv.titulos_r97 + lr_rpt_liq_reinv.titulos_cv + lr_rpt_liq_reinv.titulos_cs
   
   -- envian los datos al reporte
   OUTPUT TO REPORT rpt_liquida(lr_rpt_liq_reinv.*)
 END FOREACH
 
 -- se cierra el reporte
 FINISH REPORT rpt_liquida
 
 -- se asignan los permisos al reporte
 LET lv_comando = "chmod 777 ", mc_archivo CLIPPED
 RUN lv_comando
 
 -- se informa que el archivo del reporte se ha generado
 LET lv_mensaje = "El archivo ",mc_archivo CLIPPED," ha sido generado..."
 MESSAGE lv_mensaje ATTRIBUTE(REVERSE)
 
 -- se pide la confirmacion para imprimir reporte
 PROMPT " DESEA GENERAR IMPRESION (S/N)?: " FOR CHAR lv_respuesta
 IF lv_respuesta MATCHES "[Ss]" THEN
   LET lv_comando = "lp ",mc_archivo CLIPPED
   
   RUN lv_comando
 END IF
END FUNCTION


{ ==========================================================================
Clave: 
Nombre: f_obt_nombre_compl
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el nombre, apellido paterno y apellido materno del
afiliado de la tabla "afi mae afiliado" para el numero de seguridad
social que entra como parametro. Concatena el nombre con los apellido y
envia el nombre completo

Registro de modificaciones:

============================================================================
}
FUNCTION f_obt_nombre_compl(lc_nss)
DEFINE
 lc_nss       LIKE afi_mae_afiliado.n_seguro, -- numero de seguridad social
 lc_nombres   LIKE afi_mae_afiliado.nombres, -- nombre del afiliado
 lc_paterno   LIKE afi_mae_afiliado.paterno, -- apellido paterno del afiliado
 lc_materno   LIKE afi_mae_afiliado.materno, -- apellido materno del afiliado
 lv_nom_compl VARCHAR(100), -- nombre completo del afiliado
 lv_qryTxt    CHAR(500) -- se asigna una sentencia sql a ejecutar
 
 -- se asigna la sentencia sql que obtiene el nombre 
 LET lv_qryTxt = " SELECT UNIQUE paterno, materno, nombres",
                 " FROM afi_mae_afiliado ",
                 " WHERE n_seguro = '",lc_nss CLIPPED,"'"
 
 PREPARE prp_obt_nombre FROM lv_qryTxt
 EXECUTE prp_obt_nombre INTO lc_paterno, lc_materno, lc_nombres
 
 -- se concatena el nombre completo del afiliado
 LET lv_nom_compl = lc_nombres CLIPPED, " ", lc_paterno CLIPPED, " ", lc_materno CLIPPED

 RETURN lv_nom_compl
END FUNCTION


{ ==========================================================================
Clave: 
Nombre: rpt_liquida
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion del Reporte la cual agrupa la informacion seleccionada de base de
datos

Registro de modificaciones:

============================================================================
}
REPORT rpt_liquida(lr_rpt)
DEFINE
 lr_rpt RECORD
   folio            LIKE dis_cuenta.folio,
   nss              CHAR(11),
   consecutivo      DECIMAL(11,0),
   siefore          SMALLINT,
   regimen          CHAR(2),
   nombre           CHAR(30),
   titulos_retencion  DECIMAL(22,6),
   titulos_r97        DECIMAL(22,6),
   titulos_cv         DECIMAL(22,6),
   titulos_cs         DECIMAL(22,6),
   titulos_sar_92     DECIMAL(22,6),
   titulos_viv97      DECIMAL(22,6),
   titulos_viv92      DECIMAL(22,6),
   titulos_rcv        DECIMAL(22,6)
 END RECORD,
 ld_titulos_viv97     DECIMAL(22,6),
 ld_titulos_viv92     DECIMAL(22,6),
 encabezado         CHAR(60),
 L1                 CHAR(01),
 L2                 CHAR(02),
 L3                 CHAR(03),
 L4                 CHAR(04),
 L5                 CHAR(05),
 L6                 CHAR(06),
 L10                CHAR(10),
 lv_folio           VARCHAR(10),
 lv_pagina          VARCHAR(4),
 li_cont            SMALLINT,
 li_siefore         SMALLINT,
 li_page_length     SMALLINT
 
 OUTPUT
 PAGE LENGTH   40
 LEFT MARGIN    0
 RIGHT MARGIN   0
 TOP MARGIN     0
 BOTTOM MARGIN  0
 FORMAT
 
 FIRST PAGE HEADER
   -- se inicializan variables
   LET li_page_length = 40
   LET lv_folio = mr_captura.folio
   LET lv_pagina = PAGENO
   LET L1  = "\304"
   LET L2  = "\304\304"
   LET L3  = "\304\304\304"
   LET L4  = "\304\304\304\304"
   LET L5  = "\304\304\304\304\304"
   LET L6  = "\304\304\304\304\304\304"
   LET L10 = "\304\304\304\304\304\304\304\304\304\304"
   
   LET encabezado = "M Ó D U L O   D E   R E T I R O S"
   
   PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d\033e\033(s23H'
   
   PRINT COLUMN 080,encabezado, '\033015'
   SKIP 1 LINES
   
   PRINT COLUMN 72,"REPORTE DE LIQUIDACIÓN DE REINVERSIÓN EN ACCIONES", '\033015'
   SKIP 2 LINES
   
   PRINT COLUMN 001, "AFORE               : ", mc_razon_social CLIPPED,
         COLUMN 147, "PAGINA              : ", lv_pagina, '\033015'
   
   PRINT COLUMN 001, "PROGRAMA            : RETL745",
         COLUMN 147, "PERIODO LIQUIDACIÓN : ",mr_captura.fecha_liquida_ini USING"DD/MM/YYYY",
         " - ",mr_captura.fecha_liquida_fin USING"DD/MM/YYYY", '\033015'
   
   PRINT COLUMN 001, "FOLIO INTERNO       : ",lv_folio,
         COLUMN 147, "FECHA GENERACIÓN    : ",md_hoy USING "DD/MM/YYYY", '\033015'
 
 PAGE HEADER
   -- se asigna el folio en una variable de tipo varchar
   LET lv_folio = mr_captura.folio
   LET lv_pagina = PAGENO
   
   PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d\033e\033(s23H'
   
   PRINT COLUMN 080,encabezado, '\033015'
   SKIP 1 LINES
   
   PRINT COLUMN 72,"REPORTE DE LIQUIDACIÓN DE REINVERSIÓN EN ACCIONES", '\033015'
   SKIP 2 LINES
   
   PRINT COLUMN 001, "AFORE               : ", mc_razon_social CLIPPED,
         COLUMN 147, "PAGINA              : " ,lv_pagina, '\033015'
   
   PRINT COLUMN 001, "PROGRAMA            : RETL745",
         COLUMN 147, "PERIODO LIQUIDACIÓN : ",mr_captura.fecha_liquida_ini USING"DD/MM/YYYY",
         " - ",mr_captura.fecha_liquida_fin USING"DD/MM/YYYY", '\033015'
   
   PRINT COLUMN 001, "FOLIO INTERNO       : ",lv_folio,
         COLUMN 147, "FECHA GENERACIÓN    : ",md_hoy USING "DD/MM/YYYY", '\033015'
 
 BEFORE GROUP OF lr_rpt.siefore
   NEED 7 LINES
   IF LINENO > li_page_length THEN
     SKIP TO TOP OF PAGE
   END IF
   
   #BLOQUE TITULOS 1
   PRINT COLUMN 001,"\332",L10,L1,              --nss
                    "\302",L10,L10,L10,         --nombre
                    "\302",L3,                 --tipo_seguro
                    "\302",L3,                 --tipo_pension
                    "\302",L2,                 --regimen
                    "\302",L2,                 --siefore
                    "\302",L10,L4,              --total
                    "\302",L10,L6,              --ret97
                    "\302",L10,L6,              --cv
                    "\302",L10,L6,              --cs
                    "\302",L10,L6,              --sar92
                    "\302",L10,L6,              --viv97
                    "\302",L10,L6,              --viv92
                    "\302",L10,L6,              --rcv
                    "\277",
                    '\033015'
   
   #BLOQUE TITULOS 2          --CIERRA
   PRINT COLUMN 001,"|",
         COLUMN 013,"|",      --nss
         COLUMN 044,"|TP.",   --nombre
         COLUMN 048,"|TP.",   --tipo_seguro
         COLUMN 052,"|",      --tipo_pension
         COLUMN 055,"|",      --regimen
         COLUMN 096,"MONTOS LIQUIDADOS DE LA CUENTA INDIVIDUAL (ACCIONES)",
         COLUMN 192,"|", '\033015'
   
   #BLOQUE TITULOS 3                    --CIERRA
   PRINT COLUMN 001,"|",
         COLUMN 013,"|",               --nss
         COLUMN 044,"|",               --nombre
         COLUMN 048,"|",               --tipo_seguro
         COLUMN 052,"|",               --tipo_pension
         COLUMN 055,"\303",L2,         --regimen
         COLUMN 058,"\302",            --siefore
                            L10,L2,L2, --retencion
                    "\302", L10,L5,L1, --comp_ret
                    "\302", L10,L5,L1, --
                    "\302", L10,L5,L1, --sar92
                    "\302", L10,L5,L1, --viv97
                    "\302", L10,L5,L1, --viv92
                    "\302", L10,L5,L1, --rcv
                    "\302", L10,L5,L1, --rcv
                    "\277",
                    '\033015'
   
   #BLOQUE TITULOS 4
   PRINT COLUMN 001,"|    NSS",
         COLUMN 013,"|     NOMBRE DEL TRABAJADOR",
         COLUMN 044,"|D E",
         COLUMN 048,"|D E",
         COLUMN 052,"|RG",
         COLUMN 055,"|SB",
         COLUMN 058,"|   IMPORTE",
         COLUMN 073,"|   RETIRO  97",
         COLUMN 090,"|      CV",
         COLUMN 107,"|      CS",
         COLUMN 124,"|     SAR 92",
         COLUMN 141,"|     VIV97",
         COLUMN 158,"|     VIV92",
         COLUMN 175,"|      RCV",
         COLUMN 192,"|",
         '\033015'
   
   #BLOQUE TITULOS 5
   PRINT COLUMN 001,"|",
         COLUMN 013,"|",
         COLUMN 044,"|SEG",
         COLUMN 048,"|PEN",
         COLUMN 052,"|",
         COLUMN 055,"|",
         COLUMN 058,"|    TOTAL",
         COLUMN 073,"|    ACCIONES",
         COLUMN 090,"|    ACCIONES",
         COLUMN 107,"|    ACCIONES",
         COLUMN 124,"|    ACCIONES",
         COLUMN 141,"|    ACCIONES",
         COLUMN 158,"|    ACCIONES",
         COLUMN 175,"|    ACCIONES",
         COLUMN 192,"|",
         '\033015'
   
   #BLOQUE TITULOS 6                          --CIERRA
   PRINT COLUMN 001,"\300",L10,L1,            --nss
                    "\301",L10,L10,L10,       --nombre
                    "\301", L2,L1,            --tipo_seguro
                    "\301", L2,L1,            --tipo_pension
                    "\301", L2,               --regimen
                    "\301", L2,               --siefore
                    "\301",L10,L2,L2,         --total
                    "\301",L10,L5,L1,         --
                    "\301",L10,L5,L1,         --sar92
                    "\301",L10,L5,L1,         --viv97
                    "\301",L10,L5,L1,         --viv92
                    "\301",L10,L5,L1,         --
                    "\301",L10,L5,L1,         --
                    "\301",L10,L5,L1,         --rcv
                    "\331",
                    '\033015'
 
 ON EVERY ROW
   -- en caso de ser cero no se mostara ningun monto
   IF lr_rpt.titulos_viv97 = 0 THEN
     LET ld_titulos_viv97 = NULL
   END IF
   -- en caso de ser cero no se mostara ningun monto
   IF lr_rpt.titulos_viv92 = 0 THEN
     LET ld_titulos_viv92 = NULL
   END IF
   
   PRINT COLUMN 002, lr_rpt.nss,
         COLUMN 014, lr_rpt.nombre,
         COLUMN 053, lr_rpt.regimen,
         COLUMN 056, lr_rpt.siefore          USING "#&",
         COLUMN 059, lr_rpt.titulos_retencion  USING "##########&.&&",
         COLUMN 074, lr_rpt.titulos_r97        USING "############&.&&",
         COLUMN 091, lr_rpt.titulos_cv         USING "############&.&&",
         COLUMN 108, lr_rpt.titulos_cs         USING "############&.&&",
         COLUMN 125, lr_rpt.titulos_sar_92     USING "############&.&&",
         COLUMN 142, ld_titulos_viv97          USING "############&.&&",
         COLUMN 159, ld_titulos_viv92          USING "############&.&&",
         COLUMN 176, lr_rpt.titulos_rcv        USING "############&.&&",
         '\033015'
   
   LET li_siefore = lr_rpt.siefore
   
 AFTER GROUP OF lr_rpt.siefore
   SKIP 1 LINE
   
   #Verificar que quede espacio en la pagina
	 IF LINENO > li_page_length THEN
	   SKIP TO TOP OF PAGE
   END IF
   
   #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
   PRINT COLUMN 1, "\332",L10,L1,                    --nss
                   "\302",L10,L10,L10,L10,L1,        --totales
                   "\302",L2,                        --siefore
                   "\302",L10,L2,L2,                 --total
                   "\302",L10,L5,L1,                 --retiro08
                   "\302",L10,L5,L1,                 --cv_cs
                   "\302",L10,L5,L1,                 --comp_ret
                   "\302",L10,L5,L1,                 --sar92
                   "\302",L10,L10,L10,L2,L1,         --viviendas
                   "\302",L10,L5,L1,                 --rcv
                   "\277",
                   '\033015'
   
   #BLOQUE 2 TOTALES POR SIEFORE
   PRINT COLUMN 001,"|",
         COLUMN 020,"TOTALES POR SIEFORE",
         COLUMN 058,"|",GROUP SUM(lr_rpt.titulos_retencion)  USING "##########&.&&",
         COLUMN 073,"|",GROUP SUM(lr_rpt.titulos_r97)        USING "############&.&&",
         COLUMN 090,"|",GROUP SUM(lr_rpt.titulos_cv)         USING "############&.&&",
         COLUMN 107,"|",GROUP SUM(lr_rpt.titulos_cs)         USING "############&.&&",
         COLUMN 124,"|",GROUP SUM(lr_rpt.titulos_sar_92)     USING "############&.&&",
         COLUMN 141,"|",--GROUP SUM(lr_rpt.titulos_viv97)      USING "############&.&&",
         COLUMN 158,"|",--GROUP SUM(lr_rpt.titulos_viv92)      USING "############&.&&",
         COLUMN 175,"|",GROUP SUM(lr_rpt.titulos_rcv)        USING "############&.&&",
         COLUMN 192,"|"
   
   #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
   PRINT COLUMN 001, "\300",L10,L1,                    --nss
                     "\301",L10,L10,L10,L10,L1,        --totales
                     "\301",L2,                        --siefore
                     "\301",L10,L2,L2,                 --total
                     "\301",L10,L2,L2,                 --
                     "\301",L10,L5,L1,                 --retiro08
                     "\301",L10,L5,L1,                 --sar92
                     "\301",L10,L10,L10,L2,L1,         --viviendas
                     "\301",L10,L5,L1,                 --rcv
                     "\301",L10,L5,L3,                 --rcv
                     "\331",
                     '\033015'
   SKIP 2 LINE
   
END REPORT

{ ==========================================================================
Clave: 
Nombre: f_obt_montotitulos_r97
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta R97, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:

============================================================================
}
FUNCTION f_obt_montotitulos_r97(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss              LIKE dis_cuenta.nss, -- numero de seguridad social
 li_consecutivo_lote LIKE dis_cuenta.consecutivo_lote, -- consecutivo lote
 li_siefore          LIKE dis_cuenta.siefore, -- numero siefore
 ld_monto_titulos_r97  DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt           CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para R97 para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_cuenta\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 1"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para R97 para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 1"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para R97 para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 1"
   END IF
 END IF
 
 PREPARE prp_sumMontitulos_r97 FROM lv_qryTxt
 EXECUTE prp_sumMontitulos_r97 INTO ld_monto_titulos_r97
 
 -- si el monto obtenido es nulo se asigna cero
 IF ld_monto_titulos_r97 IS NULL THEN
   LET ld_monto_titulos_r97 = 0
 END IF
 
 RETURN ld_monto_titulos_r97
 
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_obt_montotitulos_cv
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta CV, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:

============================================================================
}
FUNCTION f_obt_montotitulos_cv(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss              LIKE dis_cuenta.nss, -- numero de seguridad social
 li_consecutivo_lote LIKE dis_cuenta.consecutivo_lote, -- consecutivo lote
 li_siefore          LIKE dis_cuenta.siefore, -- numero siefore
 ld_monto_titulos_cv   DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt           CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para CV para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_cuenta\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta IN (2,6,9)"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para CV para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (2,6,9)"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para CV para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (2,6,9)"
   END IF
 END IF
 
 PREPARE prp_sumMontitulos_cv FROM lv_qryTxt
 EXECUTE prp_sumMontitulos_cv INTO ld_monto_titulos_cv
 
 -- si el monto obtenido es nulo se asigna cero
 IF ld_monto_titulos_cv IS NULL THEN
   LET ld_monto_titulos_cv = 0
 END IF
 
 RETURN ld_monto_titulos_cv
 
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_obt_montotitulos_viv97
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta VIV97, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:

============================================================================
}
FUNCTION f_obt_montotitulos_viv97(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_cuenta.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_cuenta.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_cuenta.siefore, -- numero siefore
 ld_monto_titulos_viv97 DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para VIV97 para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_cuenta\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 4"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para VIV97 para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 4"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para VIV97 para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 4"
   END IF
 END IF
 
 PREPARE prp_sumMontitulos_viv97 FROM lv_qryTxt
 EXECUTE prp_sumMontitulos_viv97 INTO ld_monto_titulos_viv97
 
 -- si el monto obtenido es nulo se asigna cero
 IF ld_monto_titulos_viv97 IS NULL THEN
   LET ld_monto_titulos_viv97 = 0
 END IF
 
 RETURN ld_monto_titulos_viv97
 
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_obt_montotitulos_viv92
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta VIV92, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:

============================================================================
}
FUNCTION f_obt_montotitulos_viv92(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_cuenta.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_cuenta.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_cuenta.siefore, -- numero siefore
 ld_monto_titulos_viv92 DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para VIV92 para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_cuenta\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 8"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para VIV92 para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 8"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para VIV92 para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 8"
   END IF
 END IF
 
 PREPARE prp_sumMontitulos_viv92 FROM lv_qryTxt
 EXECUTE prp_sumMontitulos_viv92 INTO ld_monto_titulos_viv92
 
 -- si el monto obtenido es nulo se asigna cero
 IF ld_monto_titulos_viv92 IS NULL THEN
   LET ld_monto_titulos_viv92 = 0
 END IF
 
 RETURN ld_monto_titulos_viv92
 
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_obt_montotitulos_cs
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta CS, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:

============================================================================
}
FUNCTION f_obt_montotitulos_cs(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_cuenta.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_cuenta.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_cuenta.siefore, -- numero siefore
 ld_monto_titulos_cs    DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para CS para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_cuenta\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 5"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para CS para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 5"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para CS para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 5"
   END IF
 END IF
 
 PREPARE prp_sumMontitulos_cs FROM lv_qryTxt
 EXECUTE prp_sumMontitulos_cs INTO ld_monto_titulos_cs
 
 -- si el monto obtenido es nulo se asigna cero
 IF ld_monto_titulos_cs IS NULL THEN
   LET ld_monto_titulos_cs = 0
 END IF
 
 RETURN ld_monto_titulos_cs
 
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_obt_montotitulos_r92
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta R92, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:

============================================================================
}
FUNCTION f_obt_montotitulos_r92(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_cuenta.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_cuenta.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_cuenta.siefore, -- numero siefore
 ld_monto_titulos_r92   DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para R92 para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_cuenta\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 7"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para R92 para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 7"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para R92 para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 7"
   END IF
 END IF
 
 PREPARE prp_sumMontitulos_r92 FROM lv_qryTxt
 EXECUTE prp_sumMontitulos_r92 INTO ld_monto_titulos_r92
 
 -- si el monto obtenido es nulo se asigna cero
 IF ld_monto_titulos_r92 IS NULL THEN
   LET ld_monto_titulos_r92 = 0
 END IF
 
 RETURN ld_monto_titulos_r92
 
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_obt_montotitulos_retenc
Fecha creacion: Noviembre 18, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la retencion, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:

============================================================================
}
FUNCTION f_obt_montotitulos_retenc(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_cuenta.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_cuenta.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_cuenta.siefore, -- numero siefore
 ld_monto_titulos_reten DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para retencion para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_cuenta\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta IN (1,2,4,5,6,7,8,9)"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para retencion para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (1,2,4,5,6,7,8,9)"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para retencion para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_cuenta\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_liquida_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_liquida_fin USING "dd/mm/yyyy","'\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (1,2,4,5,6,7,8,9)"
   END IF
 END IF
 
 PREPARE prp_obt_retencion FROM lv_qryTxt
 EXECUTE prp_obt_retencion INTO ld_monto_titulos_reten
 
 IF ld_monto_titulos_reten IS NULL THEN
   LET ld_monto_titulos_reten = 0
 END IF
 
 RETURN ld_monto_titulos_reten
 
END FUNCTION

{
################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Programa RETL922  => REPORTE DE PRELIQUIACION DISPOSICIONES ISSSTE (ACCIONES) #
#By                => CESAR DAVID CHAVEZ MARTINEZ                              #
#Fecha             => 26 DE OCTUBRE 2009                                       #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE gar_precios ARRAY[5] OF RECORD
   	         siefore SMALLINT,
   	         precio  LIKE glo_valor_accion.precio_del_dia
          END RECORD

   DEFINE gr_afore RECORD
   	      codigo_afore   SMALLINT,
          razon_social   CHAR(50)
        END RECORD

   DEFINE hoy DATE

   DEFINE gr_seg_modulo         RECORD LIKE seg_modulo.* ,
          w_tabafore            RECORD LIKE tab_afore_local.*

   DEFINE enter CHAR (1)
   DEFINE gc_archivo CHAR(200)
   DEFINE gc_usuario CHAR(8)

   DEFINE gr_captura RECORD
   	         folio           INTEGER,
   	         fecha_liquida DATE
   END RECORD

   DEFINE gr_disp RECORD
   	         nss                CHAR(11)     ,
             consecutivo        DECIMAL(11,0),
             tipo_retiro        CHAR(1)      ,
             tipo_seguro        CHAR(2)      ,
             tipo_pension       CHAR(2)      ,
             tipo_prestacion    SMALLINT     ,
             siefore            SMALLINT     ,
             regimen            CHAR(2)      ,
             nombre             CHAR(30)     ,

             acciones_retencion     ,
             acciones_bruto         ,
             acciones_neto            ,
             acciones_ret08         ,
             acciones_cv_cs         ,
             acciones_comp_ret      ,
             acciones_sar_92        ,
             acciones_ahorro_sol    ,
             acciones_fov08         ,
             acciones_fov92         ,
             acciones_rcv           DECIMAL(22,6)
   END RECORD

   DEFINE gr_parcial RECORD
          nss              CHAR(11)     ,
          consecutivo      DECIMAL(11,0),
          siefore          SMALLINT     ,
          diag_cuenta_ind  SMALLINT     ,
          tipo_desempleo   CHAR(01)     ,
          tipo_pago        SMALLINT     ,
          pago_desempleo   DECIMAL(22,6),
          nombre           CHAR(30)     ,
          acciones_ret97   DECIMAL(22,6),
          acciones_cv      DECIMAL(22,6),
          acciones_cs      DECIMAL(22,6),
          acciones_est_esp DECIMAL(22,6)
   END RECORD

   DEFINE gd_fecha_genera,
          gd_fecha_operacion DATE

   DEFINE gr_edo RECORD
        preliquidado SMALLINT,
        liquidado    SMALLINT
   END RECORD
END GLOBALS
################################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETL922.log")
    CALL init()

    OPEN WINDOW RETL9221 AT 4,4 WITH FORM "RETL9221" ATTRIBUTE (BORDER)
    DISPLAY "                                < Ctrl-C >                                       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL922   REPORTE PRELIQUIDACION DE DISPOSICION DE RECURSOS ISSSTE ACCIONES   " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INITIALIZE gr_captura.* TO null

    INPUT BY NAME gr_captura.* WITHOUT DEFAULTS
    	  AFTER FIELD folio
    	  	IF gr_captura.folio IS NOT NULL THEN
    	  		 SELECT fecha_conversion
    	  		 INTO   gr_captura.fecha_liquida
    	  		 FROM   safre_tmp:tmp_ret_preliquida
    	  		 WHERE  folio = gr_captura.folio
    	  		 AND    tipo_movimiento IN (851, --A DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
                                        852, --B DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
                                        853, --C DISP.TRAB. BENEF. AMPARO PPP
                                        854, --D DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
                                        855) --E DISP.TRAB. AL AMPARO DE SU EDAD
             GROUP BY 1

             IF SQLCA.SQLCODE = 0 THEN
             	  DISPLAY BY NAME gr_captura.fecha_liquida
             ELSE
             	  ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
    	  		 	  SLEEP 2
    	  		 	  ERROR ""
    	  		 	  NEXT FIELD folio
             END IF
    	    END IF

    	  AFTER FIELD fecha_liquida
    	  	IF gr_captura.fecha_liquida IS NULL THEN
    	  		 IF gr_captura.folio IS NULL THEN
    	  		 	  ERROR "DEBE INDICAR FOLIO O FECHA DE PRELIQUIDACION"
    	  		 	  SLEEP 2
    	  		 	  ERROR ""
    	  		 	  NEXT FIELD folio
    	  		 END IF
    	  	ELSE
    	  		 SELECT folio
    	  		 INTO   gr_captura.folio
    	  		 FROM   safre_tmp:tmp_ret_preliquida
    	  		 WHERE  fecha_conversion = gr_captura.fecha_liquida
    	  		 AND    tipo_movimiento IN (851, --A DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
                                        852, --B DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
                                        853, --C DISP.TRAB. BENEF. AMPARO PPP
                                        854, --D DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
                                        855) --E DISP.TRAB. AL AMPARO DE SU EDAD
             GROUP BY 1

             IF SQLCA.SQLCODE = 0 THEN
             	  DISPLAY BY NAME gr_captura.folio
             ELSE
             	  ERROR "NO EXISTE INFORMACION PARA ESTA FECHA"
    	  		 	  SLEEP 2
    	  		 	  ERROR ""
    	  		 	  NEXT FIELD folio
             END IF
    	    END IF

        ON KEY (ESC)
        	 IF gr_captura.folio IS NULL AND gr_captura.fecha_liquida IS NULL THEN
        	 	  ERROR "DEBE INDICAR FOLIO O FECHA DE PRELIQUIDACION"
    	  		 	SLEEP 2
    	  		 	ERROR ""
    	  		 	NEXT FIELD folio
    	  	 END IF

    	  	 SELECT fecha_conversion
           INTO   gr_captura.fecha_liquida
           FROM   safre_tmp:tmp_ret_preliquida
           WHERE  folio = gr_captura.folio
           AND    tipo_movimiento IN (851, --A DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
                                      852, --B DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
                                      853, --C DISP.TRAB. BENEF. AMPARO PPP
                                      854, --D DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
                                      855) --E DISP.TRAB. AL AMPARO DE SU EDAD

           GROUP BY 1

           IF SQLCA.SQLCODE <> 0 THEN
           	  ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
           	  SLEEP 2
           	  ERROR ""
           	  NEXT FIELD folio
           END IF

    	  	 EXIT INPUT

        ON KEY (INTERRUPT, CONTROL-C)
           PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " --AT 19,1 ATTRIBUTE(REVERSE)
    CALL primer_paso()
    DISPLAY " ARCHIVO:", gc_archivo --CLIPPED AT 18,1 ATTRIBUTE(REVERSE)
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
END MAIN
################################################################################
FUNCTION init()
    DEFINE lc_sql     CHAR(800)

    SELECT codigo_afore,
           razon_social
    INTO   gr_afore.codigo_afore,
           gr_afore.razon_social
    FROM   tab_afore_local

    LET HOY = TODAY

    SELECT *, USER
    INTO   w_tabafore.*, gc_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "LIQUIDADO"

    #OBTENER NOMBRE
    LET lc_sql = " SELECT NVL(TRIM(nombres),' ')||' '||",
                        " NVL(TRIM(paterno),' ')||' '||",
                        " NVL(TRIM(materno),' ') ",
                 " FROM   afi_mae_afiliado ",
                 " WHERE  n_seguro = ? "

    PREPARE get_nombre FROM lc_sql

    #Obtener montos por subcuenta
    LET lc_sql  = " SELECT subcuenta             ,      ",
      	          "        siefore               ,      ",
      	          "        SUM(monto_en_acciones),      ",
      	          "        SUM(monto_en_pesos)          ",
      	          " FROM   safre_tmp:tmp_ret_preliquida ",
      	          " WHERE  nss              =  ?        ",
      	          " AND    consecutivo_lote =  ?        ",
      	          " AND    folio            =  ?        ",
      	          " GROUP BY 1,2                        ",
      	          " ORDER BY 1,2                        "

    PREPARE eje_montos FROM lc_sql
    DECLARE cur_montos CURSOR FOR eje_montos

    #Obtener la retencion
    LET lc_sql  = " SELECT SUM(monto_en_acciones)       ",
      	          " FROM   safre_tmp:tmp_ret_preliquida ",
      	          " WHERE  nss              =  ?        ",
      	          " AND    consecutivo_lote =  ?        ",
      	          " AND    folio            =  ?        ",
      	          " AND    tipo_movimiento  = 10        "

    PREPARE get_retencion FROM lc_sql

    #Asignar ruta y nombre del reporte
    LET gc_archivo = gr_seg_modulo.ruta_listados CLIPPED,
                     "/", gc_usuario CLIPPED,
                     ".RPT_LIQ_",HOY USING"DDMMYYYY",".922"
END FUNCTION
################################################################################
FUNCTION primer_paso()
   DEFINE lr_montos RECORD
   	       subcuenta     SMALLINT,
           siefore       SMALLINT,
           acciones      DECIMAL(22,6),
           pesos         DECIMAL(22,6)
          END RECORD

   DEFINE li_siefore              ,
          li_cont_siefore SMALLINT,
          ld_precio       LIKE glo_valor_accion.precio_del_dia

   DEFINE lc_mensaje CHAR(100)

   #Verificar precios de accion
   SELECT UNIQUE fecha_conversion
   INTO   gr_captura.fecha_liquida
   FROM   safre_tmp:tmp_ret_preliquida
   WHERE  folio       = gr_captura.folio

   #Obtener precios de accion
   DECLARE cur_precios CURSOR FOR
   SELECT codigo_siefore,
          precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = gr_captura.fecha_liquida
   AND    codigo_siefore IN (1,2,3,4,5)
   ORDER  BY 1

   LET li_cont_siefore = 0

   FOREACH cur_precios INTO li_siefore,
   	                        ld_precio

   	  LET gar_precios[li_siefore].siefore = li_siefore
   	  LET gar_precios[li_siefore].precio  = ld_precio

       LET li_cont_siefore = li_cont_siefore + 1
   END FOREACH

   IF li_cont_siefore < 5 THEN
   	  LET    lc_mensaje = "FALTA PRECIOS DE ACCION DEL DIA DE SIEFORES BASICAS DEL DIA:", gd_fecha_genera USING "DD/MM/YYYY"
      PROMPT lc_mensaje FOR CHAR enter
      EXIT PROGRAM
   END IF

   DECLARE cur_liq CURSOR FOR
   SELECT a.nss            ,
          a.consecutivo    ,
          a.tipo_retiro    ,
          a.tipo_seguro    ,
          a.tipo_pension   ,
          a.tipo_prestacion,
          b.siefore        ,
          a.regimen
   FROM   ret_sol_issste_tx a,
          ret_monto_issste  b
   WHERE  a.estado_solicitud IN (gr_edo.preliquidado,
                                 gr_edo.liquidado   )
   AND    a.folio            = gr_captura.folio
   AND    a.curp             = b.curp
   AND    a.consecutivo      = b.consecutivo
   AND    a.folio            = b.folio
   ORDER  BY tipo_retiro    ,
             siefore        ,
             tipo_seguro    ,
             tipo_pension   ,
             tipo_prestacion,
             regimen

   START REPORT rpt_liquida TO gc_archivo

   FOREACH cur_liq INTO  gr_disp.nss            ,
                         gr_disp.consecutivo    ,
                         gr_disp.tipo_retiro    ,
                         gr_disp.tipo_seguro    ,
                         gr_disp.tipo_pension   ,
                         gr_disp.tipo_prestacion,
                         gr_disp.siefore        ,
                         gr_disp.regimen

      INITIALIZE lr_montos.* TO NULL

      #Obtener nombre
      EXECUTE get_nombre USING gr_disp.nss INTO gr_disp.nombre

      LET gr_disp.acciones_retencion  = 0
      LET gr_disp.acciones_bruto      = 0
      LET gr_disp.acciones_neto         = 0
      LET gr_disp.acciones_ret08      = 0
      LET gr_disp.acciones_cv_cs      = 0
      LET gr_disp.acciones_comp_ret   = 0
      LET gr_disp.acciones_sar_92     = 0
      LET gr_disp.acciones_ahorro_sol = 0
      LET gr_disp.acciones_fov08      = 0
      LET gr_disp.acciones_fov92      = 0
      LET gr_disp.acciones_rcv      = 0

      FOREACH cur_montos USING gr_disp.nss        ,
                               gr_disp.consecutivo,
                               gr_captura.folio
                         INTO  lr_montos.subcuenta,
                               lr_montos.siefore  ,
                               lr_montos.acciones ,
                               lr_montos.pesos

         IF lr_montos.acciones IS NULL THEN
         	 LET lr_montos.acciones = 0
         END IF

         IF lr_montos.pesos IS NULL THEN
         	 LET lr_montos.pesos = 0
         END IF

         CASE
         	  WHEN lr_montos.subcuenta = 30
         	  	 LET gr_disp.acciones_ret08 = gr_disp.acciones_ret08 + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 31 OR
         	  	   lr_montos.subcuenta = 32
         	  	 LET gr_disp.acciones_cv_cs = gr_disp.acciones_cv_cs + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 24 OR
         	  	   lr_montos.subcuenta = 25
         	  	 LET gr_disp.acciones_comp_ret = gr_disp.acciones_comp_ret + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 13
         	  	 LET gr_disp.acciones_sar_92 = gr_disp.acciones_sar_92 + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 33 OR
         	  	   lr_montos.subcuenta = 34
         	  	 LET gr_disp.acciones_ahorro_sol = gr_disp.acciones_ahorro_sol + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 35
         	  	 LET gr_disp.acciones_fov08 = gr_disp.acciones_fov08 + lr_montos.acciones
         	  WHEN lr_montos.subcuenta = 14
         	  	 LET gr_disp.acciones_fov92 = gr_disp.acciones_fov92 + lr_montos.acciones
         END CASE
      END FOREACH

      #Obtener retencion
      EXECUTE get_retencion USING gr_disp.nss        ,
                                  gr_disp.consecutivo,
                                  gr_captura.folio
                            INTO  gr_disp.acciones_retencion

      IF gr_disp.acciones_retencion IS NULL THEN
      	 LET gr_disp.acciones_retencion = 0
      END IF

      #Bruto
      LET gr_disp.acciones_bruto = gr_disp.acciones_ret08      +
                                gr_disp.acciones_cv_cs      +
                                gr_disp.acciones_comp_ret   +
                                gr_disp.acciones_sar_92     +
                                gr_disp.acciones_ahorro_sol

      #A pagar
      LET gr_disp.acciones_neto  = gr_disp.acciones_bruto     -
                                gr_disp.acciones_retencion

      #RCV
      LET gr_disp.acciones_rcv = gr_disp.acciones_ret08      +
                              gr_disp.acciones_cv_cs

      OUTPUT TO REPORT rpt_liquida(gr_disp.*)
   END FOREACH

   FINISH REPORT rpt_liquida

   LET lc_mensaje = "chmod 777 ", gc_archivo CLIPPED
   RUN lc_mensaje

   LET lc_mensaje = "lp ", gc_archivo CLIPPED
   RUN lc_mensaje
END FUNCTION
################################################################################
REPORT rpt_liquida(lr_rpt)
   DEFINE lr_rpt RECORD
             nss                CHAR(11)     ,
             consecutivo        DECIMAL(11,0),
             tipo_retiro        CHAR(1)      ,
             tipo_seguro        CHAR(2)      ,
             tipo_pension       CHAR(2)      ,
             tipo_prestacion    SMALLINT     ,
             siefore            SMALLINT     ,
             regimen            CHAR(2)      ,
             nombre             CHAR(30)     ,

             acciones_retencion     ,
             acciones_bruto         ,
             acciones_neto            ,
             acciones_ret08         ,
             acciones_cv_cs         ,
             acciones_comp_ret      ,
             acciones_sar_92        ,
             acciones_ahorro_sol    ,
             acciones_fov08         ,
             acciones_fov92         ,
             acciones_rcv           DECIMAL(22,6)
   END RECORD

   DEFINE
        encabezado            CHAR(60) ,
        var2                  CHAR(10) ,
        var1                  CHAR(10) ,
        L1                    CHAR(01) ,
        L2                    CHAR(02) ,
        L3                    CHAR(03) ,
        L4                    CHAR(04) ,
        L5                    CHAR(05) ,
        L6                    CHAR(06) ,
        L7                    CHAR(07) ,
        L8                    CHAR(08) ,
        L9                    CHAR(09) ,
        L10                   CHAR(10) ,
        L11                   CHAR(11)

   DEFINE lar_total_retiro ARRAY[5] OF RECORD
   	         tipo_retiro            CHAR(01),
             acciones_fov08         ,
             acciones_fov92         DECIMAL(22,6),

   	         lar_total_siefore array[5] OF RECORD
   	         	  siefore             SMALLINT,
   	            nss_tot             INTEGER ,
   	            acciones_retencion     ,
                acciones_bruto         ,
                acciones_neto            ,
                acciones_ret08         ,
                acciones_cv_cs         ,
                acciones_comp_ret      ,
                acciones_sar_92        ,
                acciones_ahorro_sol    ,
                acciones_rcv           DECIMAL(22,6)
             END RECORD
        END RECORD

   DEFINE lar_gran_total_siefore ARRAY[5] OF RECORD
   	      nss_tot             INTEGER ,
   	      siefore             SMALLINT,
   	      acciones_retencion     ,
          acciones_bruto         ,
          acciones_neto            ,
          acciones_ret08         ,
          acciones_cv_cs         ,
          acciones_comp_ret      ,
          acciones_sar_92        ,
          acciones_ahorro_sol    ,
          acciones_fov08         ,
          acciones_fov92         ,
          acciones_rcv           DECIMAL(22,6)
        END RECORD

   DEFINE lar_gran_total_retiro ARRAY[5] OF RECORD
   	      tipo_retiro       CHAR(01),
   	      nss_tot           INTEGER ,

          acciones_fov08      ,
          acciones_fov92      DECIMAL(22,6)
        END RECORD

   DEFINE lr_total_gral RECORD
   	      nss_tot             INTEGER ,
   	      acciones_retencion     ,
          acciones_bruto         ,
          acciones_neto            ,
          acciones_ret08         ,
          acciones_cv_cs         ,
          acciones_comp_ret      ,
          acciones_sar_92        ,
          acciones_ahorro_sol    ,
          acciones_fov08         ,
          acciones_fov92         ,
          acciones_rcv           DECIMAL(22,6)
        END RECORD

   DEFINE li_cont SMALLINT

   DEFINE li_siefore SMALLINT
   DEFINE li_page_length,
          li_page_length_totales SMALLINT

   OUTPUT
        PAGE LENGTH   45
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
    FIRST PAGE HEADER
       #Inicializar acumulador por tipo de retiro y siefore
       FOR li_cont = 1 TO 5

       	 #Inicializar acumulador por tipo de retiro
       	 CASE li_cont
       	 	  WHEN 1 LET lar_total_retiro[li_cont].tipo_retiro = "A" --DISP.TRAB. BENEF. AMPARO NEGATIVA PENSION
       	 	  WHEN 2 LET lar_total_retiro[li_cont].tipo_retiro = "B" --DISP.TRAB. BENEF. AMPARO PENSION AUT. ISSSTE
       	 	  WHEN 3 LET lar_total_retiro[li_cont].tipo_retiro = "C" --DISP.TRAB. BENEF. AMPARO PPP
       	 	  WHEN 4 LET lar_total_retiro[li_cont].tipo_retiro = "D" --DISP.TRAB. BENEF. INSTRUCCION AUTORIDAD
       	 	  WHEN 5 LET lar_total_retiro[li_cont].tipo_retiro = "E" --DISP.TRAB. AL AMPARO DE SU EDAD
       	 END CASE

       	 #Inicializar acumulador por siefore
       	 FOR li_siefore = 1 TO 5
       	 	  LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].siefore          = li_siefore
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot          = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_retencion  = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_bruto      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_neto         = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs      = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret   = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92     = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol = 0
            LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv        = 0
       	 END FOR

         LET lar_total_retiro[li_cont].acciones_fov08      = 0
         LET lar_total_retiro[li_cont].acciones_fov92      = 0
       	 LET lar_gran_total_retiro[li_cont].tipo_retiro    = ""
         LET lar_gran_total_retiro[li_cont].nss_tot        = 0
         LET lar_gran_total_retiro[li_cont].acciones_fov08    = 0
         LET lar_gran_total_retiro[li_cont].acciones_fov92    = 0

         LET lar_gran_total_siefore[li_cont].nss_tot          = 0
         LET lar_gran_total_siefore[li_cont].siefore          = li_cont
         LET lar_gran_total_siefore[li_cont].acciones_retencion  = 0
         LET lar_gran_total_siefore[li_cont].acciones_bruto      = 0
         LET lar_gran_total_siefore[li_cont].acciones_neto         = 0
         LET lar_gran_total_siefore[li_cont].acciones_ret08      = 0
         LET lar_gran_total_siefore[li_cont].acciones_cv_cs      = 0
         LET lar_gran_total_siefore[li_cont].acciones_comp_ret   = 0
         LET lar_gran_total_siefore[li_cont].acciones_sar_92     = 0
         LET lar_gran_total_siefore[li_cont].acciones_ahorro_sol = 0
         LET lar_gran_total_siefore[li_cont].acciones_fov08      = 0
         LET lar_gran_total_siefore[li_cont].acciones_fov92      = 0
         LET lar_gran_total_siefore[li_cont].acciones_rcv        = 0

      END FOR

       LET lr_total_gral.nss_tot          = 0
       LET lr_total_gral.acciones_retencion  = 0
       LET lr_total_gral.acciones_bruto      = 0
       LET lr_total_gral.acciones_neto         = 0
       LET lr_total_gral.acciones_ret08      = 0
       LET lr_total_gral.acciones_cv_cs      = 0
       LET lr_total_gral.acciones_comp_ret   = 0
       LET lr_total_gral.acciones_sar_92     = 0
       LET lr_total_gral.acciones_ahorro_sol = 0
       LET lr_total_gral.acciones_fov08      = 0
       LET lr_total_gral.acciones_fov92      = 0
       LET lr_total_gral.acciones_rcv        = 0

       LET li_page_length         = 45 - 7 --5 titulos, 1 detalle
       LET li_page_length_totales = 45 - 3 --5 titulos, 1 detalle

       LET L1  = "\304"
       LET L2  = "\304\304"
       LET L3  = "\304\304\304"
       LET L4  = "\304\304\304\304"
       LET L5  = "\304\304\304\304\304"
       LET L6  = "\304\304\304\304\304\304"
       LET L7  = "\304\304\304\304\304\304\304"
       LET L8  = "\304\304\304\304\304\304\304\304"
       LET L9 = "\304\304\304\304\304\304\304\304\304"
       LET L10 = "\304\304\304\304\304\304\304\304\304\304"
       LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

       LET encabezado = "M O D U L O   D E   R E T I R O S"

       PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
              '\033e\033(s23H'

       PRINT
            COLUMN 058,encabezado,
            '\033015'

       SKIP 1 LINES

       PRINT COLUMN 55,"REPORTE DE PRE-LIQUIDACION DE DISPOSICION DE RECURSOS ISSSTE",
            '\033015'
       SKIP 2 LINES

       PRINT COLUMN 001, "AFORE               :", gr_afore.razon_social CLIPPED,
             COLUMN 206, "PAGINA              :" ,PAGENO USING "####",
             '\033015'

       PRINT COLUMN 001, "PROGRAMA            :RETL922",
             COLUMN 206, "FECHA LIQUIDACION   :",gr_captura.fecha_liquida USING"DD/MM/YYYY",
             '\033015'

       PRINT COLUMN 001, "FOLIO INTERNO       :",gr_captura.folio USING "#########&",
             COLUMN 206, "FECHA GENERACION    :",HOY USING "DD/MM/YYYY",
             '\033015'


       PRINT COLUMN 001, "VALOR ACCION SB1: ", gar_precios[1].precio USING "###&.&&&&&&",
             COLUMN 047, "VALOR ACCION SB2: ", gar_precios[2].precio USING "###&.&&&&&&",
             COLUMN 093, "VALOR ACCION SB3: ", gar_precios[3].precio USING "###&.&&&&&&",
             COLUMN 139, "VALOR ACCION SB4: ", gar_precios[4].precio USING "###&.&&&&&&",
             COLUMN 185, "VALOR ACCION SB5: ", gar_precios[5].precio USING "###&.&&&&&&",
             '\033015'

    PAGE HEADER
      PRINT COLUMN 001, "FOLIO INTERNO       :", gr_captura.folio USING "#########&",
            COLUMN 055, "REPORTE DE PRE-LIQUIDACION DE DISPOSICION DE RECURSOS ISSSTE",
            COLUMN 206, "PAGINA              :" ,PAGENO USING "####",
            '\033015'

      PRINT COLUMN 001,L10,L10,L10,L10,L10,
                       L10,L10,L10,L10,L10,
                       L10,L10,L10,L10,L10,
                       L10,L10,L10,L10,L10,
                       L10,L10,L10,L5, L2,L5,
                       '\033015'

    BEFORE GROUP OF lr_rpt.tipo_retiro
    	CASE lr_rpt.tipo_retiro
    		 WHEN "A" LET li_cont = 1
    		 WHEN "B" LET li_cont = 2
    		 WHEN "C" LET li_cont = 3
    		 WHEN "D" LET li_cont = 4
    		 WHEN "E" LET li_cont = 5
    	END CASE

   	  IF LINENO > li_page_length THEN
      	 SKIP TO TOP OF PAGE
      END IF

      #BLOQUE TITULOS 1
      PRINT COLUMN 001,"\332",L10,L1,              --nss
                       "\302",L10,L10,L10,         --nombre
                       "\302", L2,L1,              --tipo_seguro
                       "\302", L2,L1,              --tipo_pension
                       "\302", L2,                 --regimen
                       "\302",L10,L10,L10,L10,L10,
                              L10,L10,L10,L10,L10,
                              L10,L10,L10,L10,L10,
                              L10,L10,L10,L5,L2,   --LINEA
                       "\277",
                       '\033015'

      #BLOQUE TITULOS 2          --CIERRA
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",      --nss
            COLUMN 044,"|TP.",   --nombre
            COLUMN 048,"|TP.",   --tipo_seguro
            COLUMN 052,"|",      --tipo_pension
            COLUMN 055,"|",      --regimen
            COLUMN 108,"MONTOS PRE-LIQUIDADOS DE LA CUENTA INDIVIDUAL (ACCIONES) DEL TIPO RETIRO ",lr_rpt.tipo_retiro,
            COLUMN 243,"|",
                       '\033015'

     #BLOQUE TITULOS 3                    --CIERRA
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",               --nss
            COLUMN 044,"|",               --nombre
            COLUMN 048,"|",               --tipo_seguro
            COLUMN 052,"|",               --tipo_pension
            COLUMN 055,"\303",L2,         --regimen
            COLUMN 058,"\302",            --siefore
                               L10,L2,L2, --retencion
                       "\302", L10,L5,L1, --bruto
                       "\302", L10,L5,L1, --neto
                       "\301", L10,L5,L1, --retiro08
                       "\301", L10,L5,L1, --cv_cs
                       "\302", L10,L5,L1, --comp_ret
                       "\302", L10,L5,L1, --sar92
                       "\302", L10,L5,L1, --ahorro_sol
                       "\302", L10,L5,L1, --acciones_fov08
                       "\302", L10,L5,L1, --acciones_fov92
                       "\302", L10,L5,L1, --rcv
                       "\277",
                       '\033015'

      #BLOQUE TITULOS 4
      PRINT COLUMN 001,"|    NSS",
            COLUMN 013,"|     NOMBRE DEL TRABAJADOR",
            COLUMN 044,"|D E",
            COLUMN 048,"|D E",
            COLUMN 052,"|RG",
            COLUMN 055,"|SB",
            COLUMN 058,"|  RETENCION",
            COLUMN 073,"|    IMPORTE",
            COLUMN 090,"|    IMPORTE",
            COLUMN 107,"|  RETIRO  08",
            COLUMN 124,"|    CV / CS",
            COLUMN 141,"| COMP.  RETIRO",
            COLUMN 158,"|     SAR 92",
            COLUMN 175,"|  AHORRO  SOL.",
            COLUMN 192,"| FOVISSSTE 08",
            COLUMN 209,"| FOVISSSTE 92",
            COLUMN 226,"|      RCV",
            COLUMN 243,"|",
            '\033015'

      #BLOQUE TITULOS 5
      PRINT COLUMN 001,"|",
            COLUMN 013,"|",
            COLUMN 044,"|SEG",
            COLUMN 048,"|PEN",
            COLUMN 052,"|",
            COLUMN 055,"|",
            COLUMN 058,"|    TOTAL",
            COLUMN 073,"|     BRUTO",
            COLUMN 090,"|      NETO",
            COLUMN 107,"|    ACCIONES",
            COLUMN 124,"|    ACCIONES",
            COLUMN 141,"|    ACCIONES",
            COLUMN 158,"|    ACCIONES",
            COLUMN 175,"|    ACCIONES",
            COLUMN 192,"|PARTICIPACIONES",
            COLUMN 209,"|PARTICIPACIONES",
            COLUMN 226,"|    ACCIONES",
            COLUMN 243,"|",
            '\033015'

      #BLOQUE TITULOS 6                          --CIERRA
      PRINT COLUMN 001,"\300",L10,L1,            --nss
                       "\301",L10,L10,L10,       --nombre
                       "\301", L2,L1,            --tipo_seguro
                       "\301", L2,L1,            --tipo_pension
                       "\301", L2,               --regimen
                       "\301", L2,               --siefore
                       "\301",L10,L2,L2,         --retencion
                       "\301",L10,L5,L1,         --bruto
                       "\301",L10,L5,L1,         --neto
                       "\301",L10,L5,L1,         --retiro08
                       "\301",L10,L5,L1,         --cv_cs
                       "\301",L10,L5,L1,         --comp_ret
                       "\301",L10,L5,L1,         --sar92
                       "\301",L10,L5,L1,         --ahorro_sol
                       "\301",L10,L5,L1,         --acciones_fov08
                       "\301",L10,L5,L1,         --acciones_fov92
                       "\301",L10,L5,L1,         --rcv
                       "\331",
                       '\033015'
    ON EVERY ROW
      PRINT COLUMN 002, lr_rpt.nss                             ,
            COLUMN 014, lr_rpt.nombre                          ,
            COLUMN 046, lr_rpt.tipo_seguro                     ,
            COLUMN 050, lr_rpt.tipo_pension                    ,
            COLUMN 053, lr_rpt.regimen                         ,
            COLUMN 056, lr_rpt.siefore               USING "#&",

            COLUMN 059, lr_rpt.acciones_retencion       USING "######&.&&&&&&",
            COLUMN 074, lr_rpt.acciones_bruto           USING "########&.&&&&&&",
            COLUMN 091, lr_rpt.acciones_neto            USING "########&.&&&&&&",
            COLUMN 108, lr_rpt.acciones_ret08           USING "########&.&&&&&&",
            COLUMN 125, lr_rpt.acciones_cv_cs           USING "########&.&&&&&&",
            COLUMN 142, lr_rpt.acciones_comp_ret        USING "########&.&&&&&&",
            COLUMN 159, lr_rpt.acciones_sar_92          USING "########&.&&&&&&",
            COLUMN 176, lr_rpt.acciones_ahorro_sol      USING "########&.&&&&&&",
            COLUMN 193, lr_rpt.acciones_fov08           USING "########&.&&&&&&",
            COLUMN 210, lr_rpt.acciones_fov92           USING "########&.&&&&&&",
            COLUMN 227, lr_rpt.acciones_rcv             USING "########&.&&&&&&",
            '\033015'

      LET li_siefore = lr_rpt.siefore

      LET lr_total_gral.nss_tot = lr_total_gral.nss_tot + 1

      IF li_siefore <> 0 THEN
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot          = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot          + 1
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_retencion  = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_retencion  + lr_rpt.acciones_retencion
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_bruto      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_bruto      + lr_rpt.acciones_bruto
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_neto       = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_neto       + lr_rpt.acciones_neto
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08      + lr_rpt.acciones_ret08
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs      = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs      + lr_rpt.acciones_cv_cs
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret   = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret   + lr_rpt.acciones_comp_ret
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92     = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92     + lr_rpt.acciones_sar_92
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol + lr_rpt.acciones_ahorro_sol
        LET lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv        = lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv        + lr_rpt.acciones_rcv
      END IF

      LET lar_gran_total_retiro[li_cont].nss_tot        = lar_gran_total_retiro[li_cont].nss_tot        + 1
      LET lar_total_retiro[li_cont].acciones_fov08      = lar_total_retiro[li_cont].acciones_fov08      + lr_rpt.acciones_fov08
      LET lar_total_retiro[li_cont].acciones_fov92      = lar_total_retiro[li_cont].acciones_fov92      + lr_rpt.acciones_fov92



    AFTER GROUP OF lr_rpt.tipo_retiro
    	SKIP 1 LINE

    	#IMPRIMIR TOTALES POR SIEFORE DEL TIPO RETIRO QUE TERMINA
    	FOR li_siefore = 1 TO 5
    		 IF lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot > 0 THEN

    		 	  #Verificar que quede espacio en la pagina
    		 	  IF LINENO > li_page_length_totales THEN
      	       SKIP TO TOP OF PAGE
            END IF

            #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
            PRINT COLUMN 001, "\332",L10,L1,                    --nss
                              "\302",L10,L10,L10,L10,L1,        --totales
                              "\302",L2,                        --siefore
                              "\302",L10,L2,L2,                 --retencion
                              "\302",L10,L5,L1,                 --bruto
                              "\302",L10,L5,L1,                 --neto
                              "\302",L10,L5,L1,                 --retiro08
                              "\302",L10,L5,L1,                 --cv_cs
                              "\302",L10,L5,L1,                 --comp_ret
                              "\302",L10,L5,L1,                 --sar92
                              "\302",L10,L5,L1,                 --ahorro_sol
                              "\302",L10,L10,L10,L2,L1,         --viviendas
                              "\302",L10,L5,L1,                 --rcv
                              "\277",
                              '\033015'

            #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO Y SIEFORE
            PRINT COLUMN 001, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot  USING "###########&",
                  COLUMN 013, "|TOTALES POR TIPO DE RETIRO ", lar_total_retiro[li_cont].tipo_retiro,
                  COLUMN 055, "|S", li_siefore USING "&",

                  COLUMN 058, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_retencion       USING "######&.&&&&&&",
                  COLUMN 073, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_bruto           USING "########&.&&&&&&",
                  COLUMN 090, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_neto            USING "########&.&&&&&&",
                  COLUMN 107, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08           USING "########&.&&&&&&",
                  COLUMN 124, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs           USING "########&.&&&&&&",
                  COLUMN 141, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret        USING "########&.&&&&&&",
                  COLUMN 158, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92          USING "########&.&&&&&&",
                  COLUMN 175, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol      USING "########&.&&&&&&",
                  COLUMN 192, "|",
                  COLUMN 226, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv             USING "########&.&&&&&&",
                  COLUMN 243, "|",
                  '\033015'

            #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
            PRINT COLUMN 001, "\300",L10,L1,                    --nss
                              "\301",L10,L10,L10,L10,L1,        --totales
                              "\301",L2,                        --siefore
                              "\301",L10,L2,L2,                 --retencion
                              "\301",L10,L5,L1,                 --bruto
                              "\301",L10,L5,L1,                 --neto
                              "\301",L10,L5,L1,                 --retiro08
                              "\301",L10,L5,L1,                 --cv_cs
                              "\301",L10,L5,L1,                 --comp_ret
                              "\301",L10,L5,L1,                 --sar92
                              "\301",L10,L5,L1,                 --ahorro_sol
                              "\301",L10,L10,L10,L2,L1,         --viviendas
                              "\301",L10,L5,L1,                 --rcv
                              "\331",
                              '\033015'
            SKIP 1 LINE
            #Acumular al gran total por tipo de retiro
            LET lar_gran_total_retiro[li_cont].tipo_retiro    = lar_total_retiro[li_cont].tipo_retiro
            LET lar_gran_total_retiro[li_cont].acciones_fov08    = lar_gran_total_retiro[li_cont].acciones_fov08    + lar_total_retiro[li_cont].acciones_fov08
            LET lar_gran_total_retiro[li_cont].acciones_fov92    = lar_gran_total_retiro[li_cont].acciones_fov92    + lar_total_retiro[li_cont].acciones_fov92
    		 END IF
      END FOR

      #IMPRIMIR TOTAL POR TIPO DE RETIRO
      IF lar_gran_total_retiro[li_cont].nss_tot > 0 THEN
    	   #Verificar que quede espacio en la pagina
    	   IF LINENO > li_page_length_totales THEN
            SKIP TO TOP OF PAGE
         END IF

         #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO
         PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L1, --titulo
                           "\302",L10,L5,L2,                 --nss
                           "\302",L10,L10,L10,L10,L10,
                                  L10,L10,L10,L10,L10,
                                  L10,L5,L2,L1,              --espacios
                           "\302",L10,L5,L1,                 --acciones_fov08
                           "\302",L10,L5,L1,                 --acciones_fov92
                           "\302",L10,L5,L1,                 --rcv
                           "\277",
                           '\033015'

         #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO
         PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS POR TIPO DE RETIRO ", lar_gran_total_retiro[li_cont].tipo_retiro,
               COLUMN 055, "|", lar_gran_total_retiro[li_cont].nss_tot      USING "################&",
               COLUMN 073, "|",
               COLUMN 192, "|", lar_gran_total_retiro[li_cont].acciones_fov08           USING "########&.&&&&&&",
               COLUMN 209, "|", lar_gran_total_retiro[li_cont].acciones_fov92           USING "########&.&&&&&&",
               COLUMN 226, "|",
               COLUMN 243, "|",
               '\033015'

         #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
         PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L1, --titulo
                           "\301",L10,L5,L2,                 --nss
                           "\301",L10,L10,L10,L10,L10,
                                  L10,L10,L10,L10,L10,
                                  L10,L5,L2,L1,              --espacios
                           "\301",L10,L5,L1,                 --acciones_fov08
                           "\301",L10,L5,L1,                 --acciones_fov92
                           "\301",L10,L5,L1,                 --rcv
                           "\331",
                           '\033015'
      END IF
      SKIP 2 LINES

    ON LAST ROW
       PRINT COLUMN 001, "R   E   S   U   M   E   N", '\033015'

       #IMPRIMIR TOTALES POR TIPO DE RETIRO Y SIEFORE
       FOR li_cont = 1 TO 5 --TIPO DE RETIRO
       	  FOR li_siefore = 1 TO 5
       	  	 IF lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot > 0 THEN

    		 	      #Verificar que quede espacio en la pagina
    		 	      IF LINENO > li_page_length_totales THEN
      	           SKIP TO TOP OF PAGE
                END IF

                #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
                PRINT COLUMN 001, "\332",L10,L1,                    --nss
                                  "\302",L10,L10,L10,L10,L1,        --totales
                                  "\302",L2,                        --siefore
                                  "\302",L10,L2,L2,                 --retencion
                                  "\302",L10,L5,L1,                 --bruto
                                  "\302",L10,L5,L1,                 --neto
                                  "\302",L10,L5,L1,                 --retiro08
                                  "\302",L10,L5,L1,                 --cv_cs
                                  "\302",L10,L5,L1,                 --comp_ret
                                  "\302",L10,L5,L1,                 --sar92
                                  "\302",L10,L5,L1,                 --ahorro_sol
                                  "\302",L10,L10,L10,L2,L1,         --viviendas
                                  "\302",L10,L5,L1,                 --rcv
                                  "\277",
                                  '\033015'

                #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO Y SIEFORE
                PRINT COLUMN 001, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot  USING "###########&",
                      COLUMN 013, "|TOTALES POR TIPO DE RETIRO ", lar_total_retiro[li_cont].tipo_retiro,
                      COLUMN 055, "|S", li_siefore USING "&",

                      COLUMN 058, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_retencion       USING "######&.&&&&&&",
                      COLUMN 073, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_bruto           USING "########&.&&&&&&",
                      COLUMN 090, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_neto            USING "########&.&&&&&&",
                      COLUMN 107, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08           USING "########&.&&&&&&",
                      COLUMN 124, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs           USING "########&.&&&&&&",
                      COLUMN 141, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret        USING "########&.&&&&&&",
                      COLUMN 158, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92          USING "########&.&&&&&&",
                      COLUMN 175, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol      USING "########&.&&&&&&",
                      COLUMN 192, "|",
                      COLUMN 226, "|", lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv             USING "########&.&&&&&&",
                      COLUMN 243, "|",
                      '\033015'

                #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
                PRINT COLUMN 001, "\300",L10,L1,                    --nss
                                  "\301",L10,L10,L10,L10,L1,        --totales
                                  "\301",L2,                        --siefore
                                  "\301",L10,L2,L2,                 --retencion
                                  "\301",L10,L5,L1,                 --bruto
                                  "\301",L10,L5,L1,                 --neto
                                  "\301",L10,L5,L1,                 --retiro08
                                  "\301",L10,L5,L1,                 --cv_cs
                                  "\301",L10,L5,L1,                 --comp_ret
                                  "\301",L10,L5,L1,                 --sar92
                                  "\301",L10,L5,L1,                 --ahorro_sol
                                  "\301",L10,L10,L10,L2,L1,         --viviendas
                                  "\301",L10,L5,L1,                 --rcv
                                  "\331",
                                  '\033015'

                SKIP 1 LINE
                #Acumular a los totales por siefore
                LET lar_gran_total_siefore[li_siefore].nss_tot          = lar_gran_total_siefore[li_siefore].nss_tot          + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].nss_tot
                LET lar_gran_total_siefore[li_siefore].acciones_retencion  = lar_gran_total_siefore[li_siefore].acciones_retencion  + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_retencion
                LET lar_gran_total_siefore[li_siefore].acciones_bruto      = lar_gran_total_siefore[li_siefore].acciones_bruto      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_bruto
                LET lar_gran_total_siefore[li_siefore].acciones_neto       = lar_gran_total_siefore[li_siefore].acciones_neto       + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_neto
                LET lar_gran_total_siefore[li_siefore].acciones_ret08      = lar_gran_total_siefore[li_siefore].acciones_ret08      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ret08
                LET lar_gran_total_siefore[li_siefore].acciones_cv_cs      = lar_gran_total_siefore[li_siefore].acciones_cv_cs      + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_cv_cs
                LET lar_gran_total_siefore[li_siefore].acciones_comp_ret   = lar_gran_total_siefore[li_siefore].acciones_comp_ret   + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_comp_ret
                LET lar_gran_total_siefore[li_siefore].acciones_sar_92     = lar_gran_total_siefore[li_siefore].acciones_sar_92     + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_sar_92
                LET lar_gran_total_siefore[li_siefore].acciones_ahorro_sol = lar_gran_total_siefore[li_siefore].acciones_ahorro_sol + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_ahorro_sol
                LET lar_gran_total_siefore[li_siefore].acciones_fov08      = lar_gran_total_siefore[li_siefore].acciones_fov08      + lar_total_retiro[li_cont].acciones_fov08
                LET lar_gran_total_siefore[li_siefore].acciones_fov92      = lar_gran_total_siefore[li_siefore].acciones_fov92      + lar_total_retiro[li_cont].acciones_fov92
                LET lar_gran_total_siefore[li_siefore].acciones_rcv        = lar_gran_total_siefore[li_siefore].acciones_rcv        + lar_total_retiro[li_cont].lar_total_siefore[li_siefore].acciones_rcv
             END IF
       	  END FOR --TERMINA DE IMPRIMIR TOTALES POR TIPO DE RETIRO Y SIEFORE

       	  #IMPRIMIR TOTAL POR TIPO DE RETIRO
          IF lar_gran_total_retiro[li_cont].nss_tot > 0 THEN
    	       #Verificar que quede espacio en la pagina
    	       IF LINENO > li_page_length_totales THEN
                SKIP TO TOP OF PAGE
             END IF

             #BLOQUE 1 TITULO TOTALES POR TIPO RETIRO
             PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L1, --titulo
                               "\302",L10,L5,L2,                 --nss
                               "\302",L10,L10,L10,L10,L10,
                                      L10,L10,L10,L10,L10,
                                      L10,L5,L2,L1,              --espacios
                               "\302",L10,L5,L1,                 --acciones_fov08
                               "\302",L10,L5,L1,                 --acciones_fov92
                               "\302",L10,L5,L1,                 --rcv
                               "\277",
                               '\033015'

             #BLOQUE 2 DETALLE TOTALES POR TIPO RETIRO
             PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS POR TIPO DE RETIRO ", lar_gran_total_retiro[li_cont].tipo_retiro,
                   COLUMN 055, "|", lar_gran_total_retiro[li_cont].nss_tot      USING "################&",
                   COLUMN 073, "|",
                   COLUMN 192, "|", lar_gran_total_retiro[li_cont].acciones_fov08           USING "########&.&&&&&&",
                   COLUMN 209, "|", lar_gran_total_retiro[li_cont].acciones_fov92           USING "########&.&&&&&&",
                   COLUMN 226, "|",
                   COLUMN 243, "|",
                   '\033015'

             #BLOQUE 3 TITULO TOTALES POR TIPO RETIRO Y SIEFORE
             PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L1, --titulo
                               "\301",L10,L5,L2,                 --nss
                               "\301",L10,L10,L10,L10,L10,
                                      L10,L10,L10,L10,L10,
                                      L10,L5,L2,L1,              --espacios
                               "\301",L10,L5,L1,                 --acciones_fov08
                               "\301",L10,L5,L1,                 --acciones_fov92
                               "\301",L10,L5,L1,                 --rcv
                               "\331",
                               '\033015'
             SKIP 1 LINE
          END IF
       END FOR --TERMINA DE IMPRIMIR TOTALES POR TIPO DE RETIRO

       SKIP 1 LINE
       #IMPRIMIR TOTALES POR SIEFORE
       FOR li_siefore = 1 TO 5
       	  IF lar_gran_total_siefore[li_siefore].nss_tot > 0 THEN
       	  	 IF LINENO > li_page_length_totales THEN
                SKIP TO TOP OF PAGE
             END IF

             #BLOQUE 1 TITULO TOTALES POR SIEFORE
             PRINT COLUMN 001, "\332",L10,L1,                    --nss
                               "\302",L10,L10,L10,L10,L1,        --totales
                               "\302",L2,                        --siefore
                               "\302",L10,L2,L2,                 --retencion
                               "\302",L10,L5,L1,                 --bruto
                               "\302",L10,L5,L1,                 --neto
                               "\302",L10,L5,L1,                 --retiro08
                               "\302",L10,L5,L1,                 --cv_cs
                               "\302",L10,L5,L1,                 --comp_ret
                               "\302",L10,L5,L1,                 --sar92
                               "\302",L10,L5,L1,                 --ahorro_sol
                               "\302",L10,L10,L10,L2,L1,         --viviendas
                               "\302",L10,L5,L1,                 --rcv
                               "\277",
                               '\033015'

             #BLOQUE 2 DETALLE TOTALES POR SIEFORE
             PRINT COLUMN 001, "|", lar_gran_total_siefore[li_siefore].nss_tot  USING "##########&",
                   COLUMN 013, "|TOTAL POR SIEFORE",
                   COLUMN 055, "|S", li_siefore USING "&",
                   COLUMN 058, "|", lar_gran_total_siefore[li_siefore].acciones_retencion  USING "######&.&&&&&&",
                   COLUMN 073, "|", lar_gran_total_siefore[li_siefore].acciones_bruto      USING "########&.&&&&&&",
                   COLUMN 090, "|", lar_gran_total_siefore[li_siefore].acciones_neto       USING "########&.&&&&&&",
                   COLUMN 107, "|", lar_gran_total_siefore[li_siefore].acciones_ret08      USING "########&.&&&&&&",
                   COLUMN 124, "|", lar_gran_total_siefore[li_siefore].acciones_cv_cs      USING "########&.&&&&&&",
                   COLUMN 141, "|", lar_gran_total_siefore[li_siefore].acciones_comp_ret   USING "########&.&&&&&&",
                   COLUMN 158, "|", lar_gran_total_siefore[li_siefore].acciones_sar_92     USING "########&.&&&&&&",
                   COLUMN 175, "|", lar_gran_total_siefore[li_siefore].acciones_ahorro_sol USING "########&.&&&&&&",
                   COLUMN 192, "|",
                   COLUMN 226, "|", lar_gran_total_siefore[li_siefore].acciones_rcv        USING "########&.&&&&&&",
                   COLUMN 243, "|",
                   '\033015'

             #BLOQUE 3 TITULO TOTALES POR SIEFORE
             PRINT COLUMN 001, "\300",L10,L1,                    --nss
                               "\301",L10,L10,L10,L10,L1,        --totales
                               "\301",L2,                        --siefore
                               "\301",L10,L2,L2,                 --retencion
                               "\301",L10,L5,L1,                 --bruto
                               "\301",L10,L5,L1,                 --neto
                               "\301",L10,L5,L1,                 --retiro08
                               "\301",L10,L5,L1,                 --cv_cs
                               "\301",L10,L5,L1,                 --comp_ret
                               "\301",L10,L5,L1,                 --sar92
                               "\301",L10,L5,L1,                 --ahorro_sol
                               "\301",L10,L10,L10,L2,L1,         --viviendas
                               "\301",L10,L5,L1,                 --rcv
                               "\331",
                               '\033015'
             SKIP 1 LINE
             #Acumular a la ultima linea de totales
             LET lr_total_gral.acciones_retencion  = lr_total_gral.acciones_retencion  + lar_gran_total_siefore[li_siefore].acciones_retencion
             LET lr_total_gral.acciones_bruto      = lr_total_gral.acciones_bruto      + lar_gran_total_siefore[li_siefore].acciones_bruto
             LET lr_total_gral.acciones_neto       = lr_total_gral.acciones_neto       + lar_gran_total_siefore[li_siefore].acciones_neto
             LET lr_total_gral.acciones_ret08      = lr_total_gral.acciones_ret08      + lar_gran_total_siefore[li_siefore].acciones_ret08
             LET lr_total_gral.acciones_cv_cs      = lr_total_gral.acciones_cv_cs      + lar_gran_total_siefore[li_siefore].acciones_cv_cs
             LET lr_total_gral.acciones_comp_ret   = lr_total_gral.acciones_comp_ret   + lar_gran_total_siefore[li_siefore].acciones_comp_ret
             LET lr_total_gral.acciones_sar_92     = lr_total_gral.acciones_sar_92     + lar_gran_total_siefore[li_siefore].acciones_sar_92
             LET lr_total_gral.acciones_ahorro_sol = lr_total_gral.acciones_ahorro_sol + lar_gran_total_siefore[li_siefore].acciones_ahorro_sol
             LET lr_total_gral.acciones_fov08      = lr_total_gral.acciones_fov08      + lar_gran_total_siefore[li_siefore].acciones_fov08
             LET lr_total_gral.acciones_fov92      = lr_total_gral.acciones_fov92      + lar_gran_total_siefore[li_siefore].acciones_fov92
             LET lr_total_gral.acciones_rcv        = lr_total_gral.acciones_rcv        + lar_gran_total_siefore[li_siefore].acciones_rcv
       	  END IF
       END FOR

       #IMPRIMIR TOTAL DE NSS Y TOTALES DE VIVIENDA
       #Verificar que quede espacio en la pagina
    	 IF LINENO > li_page_length_totales THEN
          SKIP TO TOP OF PAGE
       END IF

       #BLOQUE 1 TITULO DE NSS Y TOTALES DE VIVIENDA
       PRINT COLUMN 001, "\332",L10,L10,L10,L10,L10,L2,L1, --titulo
                         "\302",L10,L5,L2,                  --nss
                         "\302",L10,L10,L10,L10,L10,
                                L10,L10,L10,L10,L10,
                                L10,L5,L2,L1,              --espacios
                         "\302",L10,L5,L1,                 --acciones_fov08
                         "\302",L10,L5,L1,                 --acciones_fov92
                         "\302",L10,L5,L1,                 --rcv
                         "\277",
                         '\033015'

       #BLOQUE 2 DETALLE DE NSS Y TOTALES DE VIVIENDA
       PRINT COLUMN 001, "|     TOTAL DE NSS UNICOS",
             COLUMN 055, "|", lr_total_gral.nss_tot               USING "################&",
             COLUMN 073, "|",
             COLUMN 192, "|", lr_total_gral.acciones_fov08           USING "########&.&&&&&&",
             COLUMN 209, "|", lr_total_gral.acciones_fov92           USING "########&.&&&&&&",
             COLUMN 226, "|",
             COLUMN 243, "|",
             '\033015'

       #BLOQUE 3 TITULO DE NSS Y TOTALES DE VIVIENDA
       PRINT COLUMN 001, "\300",L10,L10,L10,L10,L10,L2,L1, --titulo
                         "\301",L10,L5,L2,                  --nss
                         "\301",L10,L10,L10,L10,L10,
                                L10,L10,L10,L10,L10,
                                L10,L5,L2,L1,              --espacios
                         "\301",L10,L5,L1,                 --acciones_fov08
                         "\301",L10,L5,L1,                 --acciones_fov92
                         "\301",L10,L5,L1,                 --rcv
                         "\331",
                         '\033015'
END REPORT
################################################################################}
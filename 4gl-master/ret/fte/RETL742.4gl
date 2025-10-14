#################################################################################
#Proyecto:     => SISTEMA DE AFORES.( MEXICO )                                  #
#Programa:     => RETL742 - REPORTE DE PROVISION DE REINVERSION EN ACCIONES     #
#Autor:        => Daniel Buendia, UPGENIA                                       #
#Fecha:        => Noviembre 23, 2011                                            #
#Sistema:      => RET                                                           #
#################################################################################
DATABASE safre_af

DEFINE
 enter                 CHAR(1),
 mc_razon_social       CHAR(50),
 md_hoy                DATE,
 mc_archivo            CHAR(200),
 mr_captura            RECORD
   folio               INTEGER,
   fecha_provision_ini LIKE dis_provision.fecha_conversion,
   fecha_provision_fin LIKE dis_provision.fecha_conversion
 END RECORD,
 mi_query_ctl          SMALLINT -- status que indica que tipo de consulta se realizara
                                  -- 1 Se ingreso folio y periodo de provision
                                  -- 2 Se ingreso folio
                                  -- 3 Se ingreso periodo de provision

{ ==========================================================================
Clave: 
Nombre: MAIN
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion principal del Reporte de provision de Reinversion en acciones, se
solicita el Folio y/o Periodo de provision para generar el reporte, se
validan los datos y se invoca la funcion que consulta la informacion de la
base de datos

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

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
 CALL STARTLOG("RETL742.log")
 
 -- se invoca la funcion que inicializa variables y prepara las sentencias SQL
 CALL f_inicia_variables()
 
 -- se abre forma, la cual pedira el folio y/o periodo de provision
 OPEN WINDOW w_RETL7421 AT 4,4 WITH FORM "RETL7421" ATTRIBUTES (BORDER)
 DISPLAY "                < Ctrl-C > Salir                (ESC) Ejecutar                 " AT 1,1 ATTRIBUTE(REVERSE)
 DISPLAY " RETL742     REPORTE DE PROVISIÓN DE REINVERSIÓN EN ACCIONES                   " AT 3,1 ATTRIBUTE(REVERSE)
 DISPLAY md_hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)
 
 -- se incializa el registro de captura
 INITIALIZE mr_captura.* TO NULL
 
 INPUT BY NAME mr_captura.* WITHOUT DEFAULTS
   AFTER FIELD folio
     -- se valida el folio en caso de haber ingresado
     IF mr_captura.folio IS NOT NULL THEN
       LET lv_qryTxt = " SELECT COUNT(UNIQUE fecha_conversion)\n",
                       " FROM   dis_provision\n",
                       " WHERE  folio = ",mr_captura.folio
       
       PREPARE prp_slctCountUniq_fecConv FROM lv_qryTxt
       EXECUTE prp_slctCountUniq_fecConv INTO li_cont_reg
       
       -- si solo se encontro una fecha se consulta y se desplega
       IF li_cont_reg = 1 THEN
         LET lv_qryTxt = " SELECT UNIQUE fecha_conversion\n",
                         " FROM dis_provision\n",
                         " WHERE folio = ",mr_captura.folio,"\n",
                         " AND tipo_movimiento = 924\n",
                         " AND subcuenta IN (1,2,4,5,6,7,8,9)"
         
         PREPARE prp_unique_fec_conv FROM lv_qryTxt
         EXECUTE prp_unique_fec_conv INTO mr_captura.fecha_provision_ini
         
         -- en caso de encontrar la fecha para el folio dado se despliega en pantalla
         IF mr_captura.fecha_provision_ini IS NOT NULL THEN
           LET mr_captura.fecha_provision_fin = mr_captura.fecha_provision_ini
           DISPLAY BY NAME mr_captura.fecha_provision_ini, mr_captura.fecha_provision_fin
           
           NEXT FIELD fecha_provision_fin
         ELSE
           ERROR "NO EXISTE INFORMACIÓN PARA ESTE FOLIO"
           SLEEP 2
           ERROR ""
           
           NEXT FIELD folio
         END IF
       ELSE
         IF li_cont_reg > 1 THEN
           LET mr_captura.fecha_provision_ini = NULL
           LET mr_captura.fecha_provision_fin = NULL
           DISPLAY BY NAME mr_captura.fecha_provision_ini, mr_captura.fecha_provision_fin
           
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
     LET mr_captura.fecha_provision_ini = GET_FLDBUF(fecha_provision_ini)
     LET mr_captura.fecha_provision_fin = GET_FLDBUF(fecha_provision_fin)
     
     -- se valida el folio, este no puede ser nulo
     IF mr_captura.folio IS NULL AND mr_captura.fecha_provision_ini IS NULL AND mr_captura.fecha_provision_fin IS NULL THEN
       ERROR "DEBE INGRESAR FOLIO O PERIODO PARA CONTINUAR"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD folio
     END IF
     
     -- se valida el periodo de provision (fecha inicial)
     IF mr_captura.fecha_provision_ini IS NULL AND mr_captura.fecha_provision_fin IS NOT NULL THEN
       ERROR "DEBE INGRESAR FECHA DE PROVISIÓN INICIAL"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD fecha_provision_ini
     END IF
     
     -- se valida el periodo de provision (fecha final)
     IF mr_captura.fecha_provision_ini IS NOT NULL AND mr_captura.fecha_provision_fin IS NULL THEN
       ERROR "DEBE INGRESAR FECHA DE PROVISIÓN FINAL"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD fecha_provision_fin
     END IF
     
     -- se valida el periodo de provision (fecha inicial no puede ser mayor a la final)
     IF mr_captura.fecha_provision_ini > mr_captura.fecha_provision_fin THEN
       ERROR "EL PERIODO INICIAL NO PUEDE SER MAYOR AL FINAL"
       SLEEP 2
       ERROR ""
       
       CONTINUE INPUT
     END IF
     
     -- se compueba el tipo de consulta que se realizara
     IF mr_captura.folio IS NOT NULL THEN
       IF mr_captura.fecha_provision_ini IS NOT NULL AND mr_captura.fecha_provision_fin IS NOT NULL THEN
         LET mi_query_ctl = 1
       ELSE
         LET mi_query_ctl = 2
       END IF
     ELSE
       IF mr_captura.fecha_provision_ini IS NOT NULL AND mr_captura.fecha_provision_fin IS NOT NULL THEN
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
                       " FROM dis_provision\n",
                       " WHERE folio = ",mr_captura.folio,"\n",
                       " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                       " AND tipo_movimiento = 924\n",
                       " AND subcuenta IN (1,2,4,5,6,7,8,9)"
     ELSE
       -- verifica si se ingreso folio
       IF mi_query_ctl = 2 THEN
         -- se cuenta el numero de registros para el folio dado
         LET lv_qryTxt = " SELECT COUNT(*)\n",
                         " FROM dis_provision\n",
                         " WHERE folio = ",mr_captura.folio,"\n",
                         " AND tipo_movimiento = 924\n",
                         " AND subcuenta IN (1,2,4,5,6,7,8,9)"
       ELSE
         -- se cuenta el numero de registros para la fecha dada
         LET lv_qryTxt = " SELECT COUNT(*)\n",
                         " FROM dis_provision\n",
                         " WHERE fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
     LET mr_captura.fecha_provision_ini = GET_FLDBUF(fecha_provision_ini)
     LET mr_captura.fecha_provision_fin = GET_FLDBUF(fecha_provision_fin)
     
     -- se valida el folio, este no puede ser nulo
     IF mr_captura.folio IS NULL AND mr_captura.fecha_provision_ini IS NULL AND mr_captura.fecha_provision_fin IS NULL THEN
       ERROR "DEBE INGRESAR FOLIO O PERIODO PARA CONTINUAR"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD folio
     END IF
     
     -- se valida el periodo de provision (fecha inicial)
     IF mr_captura.fecha_provision_ini IS NULL AND mr_captura.fecha_provision_fin IS NOT NULL THEN
       ERROR "DEBE INGRESAR FECHA DE PROVISIÓN INICIAL"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD fecha_provision_ini
     END IF
     
     -- se valida el periodo de provision (fecha final)
     IF mr_captura.fecha_provision_ini IS NOT NULL AND mr_captura.fecha_provision_fin IS NULL THEN
       ERROR "DEBE INGRESAR FECHA DE PROVISIÓN FINAL"
       SLEEP 2
       ERROR ""
       
       NEXT FIELD fecha_provision_fin
     END IF
     
     -- se valida el periodo de provision (fecha inicial no puede ser mayor a la final)
     IF mr_captura.fecha_provision_ini > mr_captura.fecha_provision_fin THEN
       ERROR "EL PERIODO INICIAL NO PUEDE SER MAYOR AL FINAL"
       SLEEP 2
       ERROR ""
       
       CONTINUE INPUT
     END IF
     
     -- se compueba el tipo de consulta que se realizara
     IF mr_captura.folio IS NOT NULL THEN
       IF mr_captura.fecha_provision_ini IS NOT NULL AND mr_captura.fecha_provision_fin IS NOT NULL THEN
         LET mi_query_ctl = 1
       ELSE
         LET mi_query_ctl = 2
       END IF
     ELSE
       IF mr_captura.fecha_provision_ini IS NOT NULL AND mr_captura.fecha_provision_fin IS NOT NULL THEN
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
                       " FROM dis_provision\n",
                       " WHERE folio = ",mr_captura.folio,"\n",
                       " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                       " AND tipo_movimiento = 924\n",
                       " AND subcuenta IN (1,2,4,5,6,7,8,9)"
     ELSE
       -- verifica si se ingreso folio
       IF mi_query_ctl = 2 THEN
         -- se cuenta el numero de registros para el folio dado
         LET lv_qryTxt = " SELECT COUNT(*)\n",
                         " FROM dis_provision\n",
                         " WHERE folio = ",mr_captura.folio,"\n",
                         " AND tipo_movimiento = 924\n",
                         " AND subcuenta IN (1,2,4,5,6,7,8,9)"
       ELSE
         -- se cuenta el numero de registros para la fecha dada
         LET lv_qryTxt = " SELECT COUNT(*)\n",
                         " FROM dis_provision\n",
                         " WHERE fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
 --CLOSE WINDOW w_RETL7421
 
 DISPLAY " PROCESANDO INFORMACION... "
 
 CALL f_crea_reporte()
 
 DISPLAY " ARCHIVO:", mc_archivo
 PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
END MAIN

{ ==========================================================================
Clave: 
Nombre: f_inicia_variables
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que inicializa las variables globales que se usaran en el reporte

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_inicia_variables()
DEFINE
 lc_ruta_listados LIKE seg_modulo.ruta_listados
 
 -- se selecciona de base de datos la razon social
 SELECT razon_social
 INTO   mc_razon_social
 FROM   tab_afore_local
 
 -- se asigna la fecha de hoy
 LET md_hoy = TODAY
 
 SELECT ruta_listados
 INTO   lc_ruta_listados
 FROM   seg_modulo
 WHERE  modulo_cod = 'ret'
 
 -- se asigna la ruta y nombre del archivo del reporte
 LET mc_archivo = lc_ruta_listados CLIPPED, "/",md_hoy USING"YYYYMMDD",".742"
END FUNCTION

{ ==========================================================================
Clave: 
Nombre: f_crea_reporte
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta la informacion requerida por el reporte y se invoca al
reporte

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_crea_reporte()
DEFINE
 lr_montos RECORD
   folio            LIKE dis_provision.folio, -- numero de folio
   nss              LIKE dis_provision.nss, -- numero de seguridad social
   consecutivo_lote LIKE dis_provision.consecutivo_lote, -- consecutivo lote
   siefore          LIKE dis_provision.siefore -- numero de siafore
 END RECORD,
 lr_rpt_liq_reinv            RECORD
   folio             LIKE dis_provision.folio,
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
 START REPORT rpt_provisionados TO mc_archivo
 
 -- se inicializa el registro de montos
 INITIALIZE lr_montos.* TO NULL
 
 -- se inciializan los titulos
 LET lr_rpt_liq_reinv.titulos_retencion = 0
 LET lr_rpt_liq_reinv.titulos_r97 = 0
 LET lr_rpt_liq_reinv.titulos_cv = 0
 LET lr_rpt_liq_reinv.titulos_cs = 0
 LET lr_rpt_liq_reinv.titulos_sar_92 = 0
 LET lr_rpt_liq_reinv.titulos_viv97 = 0
 LET lr_rpt_liq_reinv.titulos_viv92 = 0
 LET lr_rpt_liq_reinv.titulos_rcv = 0
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se consultan los datos de dis cuenta para el folio y la fecha dada
   LET lv_qryTxt = " SELECT UNIQUE folio, nss, consecutivo_lote, siefore\n",
                   " FROM dis_provision\n",
                   " WHERE folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta IN (1,2,4,5,6,7,8,9)\n",
                   "ORDER BY 1, 4, 2"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se consultan los datos de dis cuenta para el folio dado
     LET lv_qryTxt = " SELECT UNIQUE folio, nss, consecutivo_lote, siefore\n",
                     " FROM dis_provision\n",
                     " WHERE folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (1,2,4,5,6,7,8,9)\n",
                     "ORDER BY 1, 4, 2"
   ELSE
     -- se consultan los datos de dis cuenta para la fecha dada
     LET lv_qryTxt = " SELECT UNIQUE folio, nss, consecutivo_lote, siefore\n",
                     " FROM dis_provision\n",
                     " WHERE fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
   OUTPUT TO REPORT rpt_provisionados(lr_rpt_liq_reinv.*)
 END FOREACH
 
 -- se cierra el reporte
 FINISH REPORT rpt_provisionados
 
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
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el nombre, apellido paterno y apellido materno del
afiliado de la tabla "afi mae afiliado" para el numero de seguridad
social que entra como parametro. Concatena el nombre con los apellido y
envia el nombre completo

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

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
Nombre: rpt_provisionados
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion del Reporte la cual agrupa la informacion seleccionada de base de
datos

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
REPORT rpt_provisionados(lr_rpt)
DEFINE
 lr_rpt RECORD
   folio            LIKE dis_provision.folio,
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
   
   PRINT COLUMN 73,"REPORTE DE PROVISIÓN DE REINVERSIÓN EN ACCIONES", '\033015'
   SKIP 2 LINES
   
   PRINT COLUMN 001, "AFORE             : ", mc_razon_social CLIPPED,
         COLUMN 147, "PAGINA            : ", lv_pagina, '\033015'
   
   PRINT COLUMN 001, "PROGRAMA          : RETL742",
         COLUMN 147, "PERIODO PROVISIÓN : ",mr_captura.fecha_provision_ini USING"DD/MM/YYYY",
         " - ",mr_captura.fecha_provision_fin USING"DD/MM/YYYY", '\033015'
   
   PRINT COLUMN 001, "FOLIO INTERNO     : ",lv_folio,
         COLUMN 147, "FECHA GENERACIÓN  : ",md_hoy USING "DD/MM/YYYY", '\033015'
 
 PAGE HEADER
   -- se asigna el folio en una variable de tipo varchar
   LET lv_folio = mr_captura.folio
   LET lv_pagina = PAGENO
   
   PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d\033e\033(s23H'
   
   PRINT COLUMN 080,encabezado, '\033015'
   SKIP 1 LINES
   
   PRINT COLUMN 73,"REPORTE DE PROVISIÓN DE REINVERSIÓN EN ACCIONES", '\033015'
   SKIP 2 LINES
   
   PRINT COLUMN 001, "AFORE             : ", mc_razon_social CLIPPED,
         COLUMN 147, "PAGINA            : " ,lv_pagina, '\033015'
   
   PRINT COLUMN 001, "PROGRAMA          : RETL742",
         COLUMN 147, "PERIODO PROVISIÓN : ",mr_captura.fecha_provision_ini USING"DD/MM/YYYY",
         " - ",mr_captura.fecha_provision_fin USING"DD/MM/YYYY", '\033015'
   
   PRINT COLUMN 001, "FOLIO INTERNO     : ",lv_folio,
         COLUMN 147, "FECHA GENERACIÓN  : ",md_hoy USING "DD/MM/YYYY", '\033015'
 
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
         COLUMN 096,"MONTOS PROVISIONADOS DE LA CUENTA INDIVIDUAL (ACCIONES)",
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
         COLUMN 013,"|    NOMBRE DEL TRABAJADOR",
         COLUMN 044,"|D E",
         COLUMN 048,"|D E",
         COLUMN 052,"|RG",
         COLUMN 055,"|SB",
         COLUMN 058,"|    TOTAL",
         COLUMN 073,"|   RETIRO  97",
         COLUMN 090,"|       CV",
         COLUMN 107,"|       CS",
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
         COLUMN 058,"|   ACCIONES",
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
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta R97, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_obt_montotitulos_r97(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss              LIKE dis_provision.nss, -- numero de seguridad social
 li_consecutivo_lote LIKE dis_provision.consecutivo_lote, -- consecutivo lote
 li_siefore          LIKE dis_provision.siefore, -- numero siefore
 ld_monto_titulos_r97  DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt           CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para R97 para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_provision\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 1"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para R97 para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 1"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para R97 para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta CV, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_obt_montotitulos_cv(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss              LIKE dis_provision.nss, -- numero de seguridad social
 li_consecutivo_lote LIKE dis_provision.consecutivo_lote, -- consecutivo lote
 li_siefore          LIKE dis_provision.siefore, -- numero siefore
 ld_monto_titulos_cv   DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt           CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para CV para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_provision\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta IN (2,6,9)"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para CV para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (2,6,9)"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para CV para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta VIV97, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_obt_montotitulos_viv97(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_provision.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_provision.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_provision.siefore, -- numero siefore
 ld_monto_titulos_viv97 DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para VIV97 para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_provision\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 4"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para VIV97 para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 4"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para VIV97 para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta VIV92, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_obt_montotitulos_viv92(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_provision.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_provision.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_provision.siefore, -- numero siefore
 ld_monto_titulos_viv92 DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para VIV92 para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_provision\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 8"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para VIV92 para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 8"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para VIV92 para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta CS, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_obt_montotitulos_cs(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_provision.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_provision.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_provision.siefore, -- numero siefore
 ld_monto_titulos_cs    DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para CS para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_provision\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 5"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para CS para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 5"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para CS para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la subcuenta R92, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_obt_montotitulos_r92(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_provision.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_provision.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_provision.siefore, -- numero siefore
 ld_monto_titulos_r92   DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para R92 para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_provision\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta = 7"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para R92 para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta = 7"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para R92 para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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
Fecha creacion: Noviembre 23, 2011
Autor: Daniel Buendia, UPGENIA
Narrativa del proceso que realiza:
Funcion que consulta el monto en titulos de la retencion, para el nss,
consecutivo y siefore que entran como parametro, igualmente considera los
datos ingresados por el usuario

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================
}
FUNCTION f_obt_montotitulos_retenc(lc_nss, li_consecutivo_lote, li_siefore)
DEFINE
 lc_nss               LIKE dis_provision.nss, -- numero de seguridad social
 li_consecutivo_lote  LIKE dis_provision.consecutivo_lote, -- consecutivo lote
 li_siefore           LIKE dis_provision.siefore, -- numero siefore
 ld_monto_titulos_reten DECIMAL(22,6), -- monto en titulos total obtenido
 lv_qryTxt            CHAR(500) -- guarda una sentencia sql a ejecutar
 
 -- verifica si se ingreso folio y periodo
 IF mi_query_ctl = 1 THEN
   -- se crea la sentencia sql que obtiene el monto en titulos para retencion para el folio y la fecha dada
   LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                   " FROM dis_provision\n",
                   " WHERE nss = '",lc_nss,"'\n",
                   " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                   " AND siefore = ",li_siefore,"\n",
                   " AND folio = ",mr_captura.folio,"\n",
                   " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
                   " AND tipo_movimiento = 924\n",
                   " AND subcuenta IN (1,2,4,5,6,7,8,9)"
 ELSE
   -- verifica si se ingreso folio
   IF mi_query_ctl = 2 THEN
     -- se crea la sentencia sql que obtiene el monto en titulos para retencion para el folio dado
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND folio = ",mr_captura.folio,"\n",
                     " AND tipo_movimiento = 924\n",
                     " AND subcuenta IN (1,2,4,5,6,7,8,9)"
   ELSE
     -- se crea la sentencia sql que obtiene el monto en titulos para retencion para la fecha dada
     LET lv_qryTxt = " SELECT SUM(monto_en_acciones)\n",
                     " FROM dis_provision\n",
                     " WHERE nss = '",lc_nss,"'\n",
                     " AND consecutivo_lote = ",li_consecutivo_lote,"\n",
                     " AND siefore = ",li_siefore,"\n",
                     " AND fecha_conversion BETWEEN '",mr_captura.fecha_provision_ini USING "dd/mm/yyyy","' AND '",mr_captura.fecha_provision_fin USING "dd/mm/yyyy","'\n",
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

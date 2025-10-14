############################################################################
#Propietario       => E.F.P.                                               #
#Programa ACRC001  => RECIBE ARCHIVOS DE TRANSFERENCIA DE ACREDITADOS      #
#Fecha creacion    => 30 DE ENERO DE 1998                                  #
#Por               => MAURO MUNIZ CABALLERO                                #
#Fecha actualiz.   => 24 DE MARZO DE 1999                                  #
#Actualizacion .   => MAURO MUNIZ CABALLERO                                #
#Fecha actualiz.   => 17 DE JUNIO 1999.                                    #
#Actualizacion     => MIGUEL ANGEL HERNANDEZ MARTINEZ                      #
#Solucion          => INTEGRACION DE LA FUNCION DEL REPORTE                #
#Fecha actualiz.   => 4 DE AGOSTO DE 2004                                  #
#Actualizacion     => MAURO MUÑIZ CABALLERO                                #
#                     Se adecuo para participaciones                       #
#Modificacion      => EDUARDO JOAQUIN RESENDIZ MEDINA                      #
#Fecha Modifica.   => 9 DE FEBRERO DE 2005                                 #
#Modificacion      => MAURO MUÑIZ CABALLERO                                #
#Fecha Modifica.   => 7 DE AGOSTO DE 2007                                  #
#                     CAMBIO DE MARCA A 232 EN PROCESO DE TRANSF ACRED     #
#Sistema           => ACR                                                  #
#Modificado        => DMR 20 junio 2013  MLM-1977                          #
#Modificacion      => Se agrego la rutina limpieza para cambiar la \ por   #
#                  => otro caracter Ñ para que no truene el programa       #
#                  => en el marcaje y desmarcaje de las cuentas            #
#Modificado        => DMR 08 julio 2013  MLM-1977                          #
#Modificacion      => Se cambio el llamado de la rutina limpieza la cual se#
#                  => adiciono en el ACR_GLOB.4gl para los demas programas.#
#Modificado        => DMR 21 Agosto 2013 MLM-2087 NO archivo RESPALDO y no #
#                  => se pide confirmacion al usuario para sobreescribir   #
#Modificado        => DMR 10 Abril 2014 CPL-1585 Prueba completa modulo ACR#
#                  => NSS sin saldo deben rechazarse;STARTLOG personalizado#
#Modificado        => DMR 22/12/2014 CPL-1657 Se agrego motivo de rechazo 3#
#                     para nss sin saldo en vivienda en reporte del proceso#
############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      HOY                    ,
      d_fecha_cza            ,
      d_fecha_det            DATE

   DEFINE
      enter                  CHAR(1),
      lote_rechazado         CHAR(2),
      c_paso_cza             CHAR(8),
      c_paso_det             CHAR(8),
      c_fecha_cza            CHAR(10),
      c_fecha_det            CHAR(10),
      generar                CHAR(20),
      archivo_traspaso       CHAR(200),
      cat                    CHAR(300),
      hora                   CHAR(8)

   DEFINE
      ban                    ,
      pmarca_causa           ,
      bnd_proc               ,
      edo_proc               ,
      vtot_acep              ,
      vtot_rech              ,
      vcausa_rechazo         , 
      vtot_reg               SMALLINT,
      cuantos                ,
      s_codigo_afore         ,
      cont                   INTEGER
      
   DEFINE
      bnd_proceso            SMALLINT

   DEFINE
       opc                   CHAR(1)  ,
       vnss                  CHAR(11) ,
       vmarca_entra          SMALLINT ,
       vmarca_estado         SMALLINT ,
       vcodigo_rechazo       SMALLINT ,
       g_usuario             CHAR(8)  ,
       ejecuta               CHAR(300),
       xcodigo_marca         SMALLINT ,
       xcodigo_rechazo       SMALLINT

   DEFINE
       vtotal_reg            INTEGER,
       vtotal_aprob          INTEGER,
       vtotal_rech           INTEGER,
       vtotal_pend           INTEGER

   DEFINE
      g_seg_modulo    RECORD LIKE seg_modulo.*
      
   DEFINE
      reg_cza_tra_acr RECORD 
                         tipo_registro          CHAR(02),
                         ident_servicio         CHAR(02),
                         ident_operacion        CHAR(02),
                         tipo_ent_origen        CHAR(02),
                         cve_ent_origen         CHAR(03),
                         tipo_ent_destino       CHAR(02),
                         cve_ent_destino        CHAR(03),
                         ent_fed_envio_lote     CHAR(03),
                         fecha_presentacion     DATE    ,
                         consec_lote_dia        SMALLINT,
                         cve_mod_recepcion      CHAR(02),
                         cod_result_operac      CHAR(02),
                         rechazo                CHAR(09)
                      END RECORD

   DEFINE
      reg_det_tra_acr RECORD
                         tipo_registro          CHAR(002),
                         cont_servicio          DECIMAL(10,0),
                         tipo_recep_cuenta      CHAR(002),
                         cve_recep_cuenta       CHAR(003),
                         tipo_ced_cuenta        CHAR(002),
                         cve_ced_cuenta         CHAR(003),
                         tipo_transferencia     CHAR(002),
                         fecha_presentacion     DATE     ,
                         n_unico_infonavit      CHAR(018),
                         nss_infonavit          CHAR(011),
                         rfc_infonavit          CHAR(013),
                         paterno_infonavit      CHAR(040),
                         materno_infonavit      CHAR(040),
                         nombres_infonavit      CHAR(040),
                         ident_lote_devol       CHAR(016),
                         nss_afore              CHAR(011),
                         rfc_afore              CHAR(013),
                         paterno_afore          CHAR(040),
                         materno_afore          CHAR(040),
                         nombres_afore          CHAR(040),
                         partic_v97             DECIMAL(22,06),
                         ult_apor_viv_97        DECIMAL(15,02),
                         cod_result_operac      CHAR(002),
                         diag_proceso           CHAR(015),
                         nombre_imss            CHAR(050),
                         num_cred_infonavit     DECIMAL(10,0),
                         periodo_pago           CHAR(6)
                      END RECORD

   DEFINE
      reg_sum_tra_acr RECORD
                         tipo_registro          CHAR(02),
                         cant_reg_det           DECIMAL(9,0),
                         sum_partic_v97         DECIMAL(22,6),
                         sum_ult_apor_viv97     DECIMAL(15,2)
                      END RECORD
   
   DEFINE
      g               RECORD
                         total                  INTEGER,
                         tipo_transferencia     CHAR(2),
                         descripcion            CHAR(20)
                      END RECORD

   DEFINE
      reg_bat         RECORD
                         pid                    INTEGER,
                         proceso_cod            INTEGER,
                         opera_cod              INTEGER,
                         nombre_archivo         CHAR(25)
                      END RECORD
END GLOBALS


MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT
   
--   DISPLAY " "
--   DISPLAY ".1"

   CALL STARTLOG(FGL_GETENV("USER")||".ACRC001.log")
   CALL inicio()
   CALL proceso_principal()
   
{
   IF NOT bnd_proceso THEN
      DEFER INTERRUPT
      OPTIONS 
         INPUT WRAP,
         PROMPT LINE LAST  ,
         ACCEPT KEY CONTROL-I

      CALL proceso_principal() #pp
   ELSE
      CALL sube_archivo()
      CALL crea_tablas() #ct
      CALL validacion_previa() #vp
      CALL lee_archivo_plano() #lap
      CALL actualiza_operacion() #ao
   END IF

   CALL impresion_reporte()
   CALL impresion_reporte_fechpres()
}
END MAIN


FUNCTION inicio()
#i---------------
   LET reg_bat.pid            = ARG_VAL(1)
   LET reg_bat.proceso_cod    = ARG_VAL(2)
   LET reg_bat.opera_cod      = ARG_VAL(3)
   LET reg_bat.nombre_archivo = ARG_VAL(4)

   LET bnd_proceso  = 0
   LET edo_proc     = 232
   LET pmarca_causa = 0

   LET vtotal_reg   = 0
   LET vtotal_aprob = 0
   LET vtotal_rech  = 0
   LET vtotal_pend  = 0

   LET HOY          = TODAY
   LET hora = TIME
   LET hora = hora[1,2],hora[4,5]

   IF reg_bat.pid THEN
      DISPLAY "INICIANDO PROCESO"
      LET bnd_proceso = 1
   END IF

   SELECT codigo_afore, USER
   INTO   s_codigo_afore, g_usuario
   FROM   tab_afore_local

   SELECT * 
   INTO   g_seg_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp

      DROP TABLE tmp_pla_acr
      CREATE TABLE tmp_pla_acr
         ( n_registros CHAR(730) )

      DATABASE safre_af
   WHENEVER ERROR STOP
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------
   LET ejecuta = "cd ",g_seg_modulo.ruta_rescate CLIPPED,"; ls > archivo_acrc001" CLIPPED
   RUN ejecuta

   CREATE TEMP TABLE archivo_acrc001 (lista  CHAR(730))

   LET ejecuta = g_seg_modulo.ruta_rescate CLIPPED,"/archivo_acrc001" CLIPPED

   LOAD FROM ejecuta INSERT INTO archivo_acrc001

   LET ejecuta = "rm ",g_seg_modulo.ruta_rescate CLIPPED,"/archivo_acrc001" CLIPPED
   RUN ejecuta
   
   
   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRC0011" ATTRIBUTE(BORDER)
   DISPLAY " ACRC001   RECIBE SOLICITUDES DE TRANSFERENCIA DE ACREDITADOS              " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "      < ESC > Aceptar                                < Ctrl-C > Salir      " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar WITHOUT DEFAULTS
   	
      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO "
            NEXT FIELD generar
         END IF

         SELECT "X"
         FROM   archivo_acrc001
         WHERE  lista = generar

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE "
            NEXT FIELD generar
         END IF                                                 

      ON KEY (ESC)
         IF generar IS NULL THEN
            ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO "
            NEXT FIELD generar
         END IF

         SELECT "a.X"
         FROM   archivo_acrc001 a
         WHERE  a.lista = generar

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE "
            NEXT FIELD generar
         END IF

         SELECT "X"
         FROM   acr_ctr_arh a
         WHERE  a.nombre_archivo = generar

         IF SQLCA.SQLCODE = 0 THEN
            ERROR " NOMBRE DE ARCHIVO YA CARGADO "
            NEXT FIELD generar
         END IF

         CALL limpieza(g_seg_modulo.ruta_rescate,generar)

         DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
         SLEEP 3

         LET archivo_traspaso = g_seg_modulo.ruta_rescate CLIPPED,
                                "/",generar CLIPPED

         LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_acr

         CALL crea_tablas()       #ct
         CALL validacion_previa() #vp
         CALL lee_archivo_plano() #lap
         CALL impresion_reporte(generar) #ip
         CALL impresion_reporte_fechpres(generar)

         SELECT COUNT(*)
         INTO   vtot_reg
         FROM   safre_tmp:det_tra_acr

         SELECT COUNT(*)
         INTO   vtot_acep
         FROM   safre_tmp:det_tra_acr
         WHERE  estado = 0

         SELECT COUNT(*)
         INTO   vtot_rech
         FROM   safre_tmp:det_tra_acr
         WHERE  estado <> 0

         DISPLAY "TOTAL DE REGISTROS : ",vtot_reg AT 11,20
         DISPLAY "TOTAL ACEPTADOS    : ",vtot_acep AT 12,20
         DISPLAY "TOTAL RECHAZADOS   : ",vtot_rech AT 13,20

         DISPLAY "                        " AT 19,1 

         DISPLAY "REPORTES GENERADOS EN LA RUTA: ",g_seg_modulo.ruta_listados CLIPPED AT 15,12
         DISPLAY "CON LOS NOMBRES : ", g_usuario CLIPPED,
                                       ".SOL_ACR.",
                                       HOY USING "DD-MM-YYYY",
                                       "_",hora CLIPPED AT 16,12
                                       
         DISPLAY "                  ", g_usuario CLIPPED,
                                       ".SOL_ACR_FECHPRES.",
                                       HOY USING "DD-MM-YYYY",
                                       "_",hora CLIPPED  AT 17,12     
                  
         PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
         FOR enter

         EXIT INPUT

      ON KEY (INTERRUPT, CONTROL-C)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " 
         FOR enter
         EXIT INPUT
   END INPUT

   CLOSE WINDOW ventana_1
   DROP TABLE archivo_acrc001
   RETURN
END FUNCTION              


FUNCTION sube_archivo()
#sa--------------------
   LET archivo_traspaso = g_seg_modulo.ruta_rescate CLIPPED,"/",
                          reg_bat.nombre_archivo CLIPPED

   SELECT "X"
   FROM   acr_ctr_arh a
   WHERE  a.nombre_archivo = reg_bat.nombre_archivo

   IF SQLCA.SQLCODE = 0 THEN
      DISPLAY  "Program stopped, ARCHIVO YA PROCESADO"
      EXIT PROGRAM
   END IF

   LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_acr

   SELECT count(*)
   INTO   cuantos
   FROM   safre_tmp:tmp_pla_acr

   IF cuantos = 0 OR
      cuantos IS NULL THEN
      DISPLAY  "Program stopped, NOMBRE DE ARCHIVO INCORRECTO O VACIO"
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION crea_tablas()
#ct-------------------
   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp

      DROP TABLE cza_tra_acr
      DROP TABLE det_tra_acr
      DROP TABLE sum_tra_acr
   WHENEVER ERROR STOP

   CREATE TABLE cza_tra_acr
      (tipo_registro         CHAR(02),
       ident_servicio        CHAR(02),
       ident_operacion       CHAR(02),
       tipo_ent_origen       CHAR(02),
       cve_ent_origen        CHAR(03),
       tipo_ent_destino      CHAR(02),
       cve_ent_destino       CHAR(03),
       ent_fed_envio_lote    CHAR(03),
       fecha_presentacion    DATE,
       consec_lote_dia       SMALLINT,
       cve_mod_recepcion     CHAR(02),
       cod_result_operac     CHAR(02),
       rechazo               CHAR(09));

   CREATE TABLE det_tra_acr
      (tipo_registro         CHAR(002),
       cont_servicio         DECIMAL(10,0),
       tipo_recep_cuenta     CHAR(002),
       cve_recep_cuenta      CHAR(003),
       tipo_ced_cuenta       CHAR(002),
       cve_ced_cuenta        CHAR(003),
       tipo_transferencia    CHAR(002),
       fecha_presentacion    DATE     ,
       n_unico_infonavit     CHAR(018),
       nss_infonavit         CHAR(011),
       rfc_infonavit         CHAR(013),
       paterno_infonavit     CHAR(040),
       materno_infonavit     CHAR(040),
       nombres_infonavit     CHAR(040),
       ident_lote_devol      CHAR(016),
       nss_afore             CHAR(011),
       rfc_afore             CHAR(013),
       paterno_afore         CHAR(040),
       materno_afore         CHAR(040),
       nombres_afore         CHAR(040),
       partic_v97            DECIMAL(22,06),
       ult_apor_viv_97       DECIMAL(15,02),
       cod_result_operac     CHAR(002),
       diag_proceso          CHAR(015),
       nombre_imss           CHAR(050),
       num_cred_infonavit    DECIMAL(10,0),
       periodo_pago          CHAR(6),
       estado                SMALLINT,
       causa_rechazo         SMALLINT)

   CREATE TABLE sum_tra_acr
      (tipo_registro         CHAR(02),
       cant_reg_det          DECIMAL(9,0),
       sum_partic_v97        DECIMAL(22,6),
       sum_ult_apor_viv97    DECIMAL(15,2));

   DATABASE safre_af
END FUNCTION


FUNCTION validacion_previa()
#vp-------------------------
   DEFINE
      c2_tipo_registro      CHAR(2)

   DEFINE
      sw_1                  ,
      sw_2                  ,
      sw_9                  SMALLINT

   DECLARE cur_2 CURSOR FOR
   SELECT UNIQUE(n_registros[1,2])
   FROM   safre_tmp:tmp_pla_acr

   LET sw_1 = 0
   LET sw_2 = 0
   LET sw_9 = 0

   FOREACH cur_2 INTO c2_tipo_registro
      CASE c2_tipo_registro
         WHEN "01"
            LET sw_1 = 1
         WHEN "02"
            LET sw_2 = 1
         WHEN "09"
            LET sw_9 = 1
      END CASE
   END FOREACH

   IF sw_1 = 0 THEN
      IF bnd_proceso THEN
         DISPLAY "Program stopped, SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO"
      ELSE
         PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
         EXIT PROGRAM
      END IF
   END IF

   IF sw_2 = 0 THEN
      IF bnd_proceso THEN
         DISPLAY "Program stopped, SE RECHAZA EL LOTE. NO EXISTE DETALLE"
      ELSE
         PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" 
         FOR enter
         EXIT PROGRAM
      END IF
   END IF

   IF sw_9 = 0 THEN
      IF bnd_proceso THEN
         DISPLAY "Program stopped, SE RECHAZA EL LOTE. NO EXISTE SUMARIO"
      ELSE
         PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
         EXIT PROGRAM
      END IF
   END IF
END FUNCTION


FUNCTION lee_archivo_plano()
#lap------------------------
   DEFINE 
      cont                 INTEGER 
 
   DEFINE 
      carga_reg            CHAR(730),
      c2_ident_operacion   CHAR(002)
      
   DEFINE
      saldo_4              DEC(18,6),
      saldo_8              DEC(18,6)

   LET bnd_proc = 0

   DECLARE cur_1 CURSOR FOR
   SELECT  * 
   FROM    safre_tmp:tmp_pla_acr

   LET cont = 0
   LET c2_ident_operacion = ""

   FOREACH cur_1 INTO carga_reg
      LET cont = cont + 1

      #---ENCABEZADO SOLICITUD DE TRANSFERENCIA---#
      IF carga_reg[5,6] = "01" THEN
         LET c2_ident_operacion = "01"
         LET reg_cza_tra_acr.tipo_registro     = carga_reg[001,002]
         LET reg_cza_tra_acr.ident_servicio    = carga_reg[003,004]
         LET reg_cza_tra_acr.ident_operacion   = carga_reg[005,006]
         LET reg_cza_tra_acr.tipo_ent_origen   = carga_reg[007,008]
         LET reg_cza_tra_acr.cve_ent_origen    = carga_reg[009,011]
         LET reg_cza_tra_acr.tipo_ent_destino  = carga_reg[012,013]
         LET reg_cza_tra_acr.cve_ent_destino   = carga_reg[014,016]
         LET reg_cza_tra_acr.ent_fed_envio_lote= carga_reg[017,019]
         LET c_paso_cza                        = carga_reg[020,027]
         LET reg_cza_tra_acr.consec_lote_dia   = carga_reg[028,030]
         LET reg_cza_tra_acr.cve_mod_recepcion = carga_reg[031,032]
         LET reg_cza_tra_acr.cod_result_operac = carga_reg[033,034]
         LET reg_cza_tra_acr.rechazo           = carga_reg[035,043]

         LET c_fecha_cza = c_paso_cza[5,6],"/",
                           c_paso_cza[7,8],"/",
                           c_paso_cza[1,4]

         LET reg_cza_tra_acr.fecha_presentacion = c_fecha_cza

         INSERT INTO safre_tmp:cza_tra_acr
         VALUES(reg_cza_tra_acr.*)
      END IF


      #---DETALLE DE SOLICITUD DE TRANSFERENCIA---#
      IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
         LET reg_det_tra_acr.tipo_registro      = carga_reg[001,002]
         LET reg_det_tra_acr.cont_servicio      = carga_reg[003,012]
         LET reg_det_tra_acr.tipo_recep_cuenta  = carga_reg[013,014]
         LET reg_det_tra_acr.cve_recep_cuenta   = carga_reg[015,017]
         LET reg_det_tra_acr.tipo_ced_cuenta    = carga_reg[018,019]
         LET reg_det_tra_acr.cve_ced_cuenta     = carga_reg[020,022]
         LET reg_det_tra_acr.tipo_transferencia = carga_reg[023,024]
         LET c_paso_det                         = carga_reg[025,032]
         LET reg_det_tra_acr.n_unico_infonavit  = carga_reg[041,058]
         LET reg_det_tra_acr.nss_infonavit      = carga_reg[059,069]
         LET reg_det_tra_acr.rfc_infonavit      = carga_reg[085,097]
         LET reg_det_tra_acr.paterno_infonavit  = carga_reg[098,137]
         LET reg_det_tra_acr.materno_infonavit  = carga_reg[138,177]
         LET reg_det_tra_acr.nombres_infonavit  = carga_reg[178,217]
         LET reg_det_tra_acr.ident_lote_devol   = carga_reg[240,255]
         LET reg_det_tra_acr.nss_afore          = carga_reg[271,281]
         LET reg_det_tra_acr.rfc_afore          = carga_reg[282,294]
         LET reg_det_tra_acr.paterno_afore      = carga_reg[325,364]
         LET reg_det_tra_acr.materno_afore      = carga_reg[365,404]
         LET reg_det_tra_acr.nombres_afore      = carga_reg[405,444]
         LET reg_det_tra_acr.partic_v97         = carga_reg[475,489]
         LET reg_det_tra_acr.ult_apor_viv_97    = carga_reg[490,504]
         LET reg_det_tra_acr.cod_result_operac  = carga_reg[583,584]
         LET reg_det_tra_acr.diag_proceso       = carga_reg[585,599]
         LET reg_det_tra_acr.nombre_imss        = carga_reg[600,649]
         LET reg_det_tra_acr.num_cred_infonavit = carga_reg[650,659]

         LET c_fecha_det = c_paso_det[5,6],"/",
                           c_paso_det[7,8],"/",
                           c_paso_det[1,4]

         LET reg_det_tra_acr.fecha_presentacion  = c_fecha_det
         
         LET vcausa_rechazo = 0

         INSERT INTO safre_tmp:det_tra_acr
         VALUES(reg_det_tra_acr.*,0,vcausa_rechazo)


         #-- MARCAJE DE LA INFORMATIVA --#
         SELECT "X"
         FROM   cta_act_marca
         WHERE  marca_cod = 230
         AND    nss       = reg_det_tra_acr.nss_afore
         GROUP BY 1
         
         LET vmarca_estado   = 0
         LET vcodigo_rechazo = 0
         
         IF STATUS = NOTFOUND THEN
            LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                          "'",reg_det_tra_acr.nss_afore,"'",
                          ",",230,
                          ",",reg_det_tra_acr.cont_servicio,
                          ",",vmarca_estado,
                          ",",vcodigo_rechazo,
                          ",",pmarca_causa,
                          ",","'","'", ",",
                          "'",g_usuario,"'",")"
           
            LET ejecuta = ejecuta CLIPPED
            PREPARE clausula_spl230 FROM ejecuta
            DECLARE cursor_marca230 CURSOR FOR clausula_spl230
            OPEN  cursor_marca230
            FETCH cursor_marca230 INTO xcodigo_marca, xcodigo_rechazo
            CLOSE cursor_marca230
         END IF

         IF reg_det_tra_acr.tipo_transferencia = "03" THEN
            SELECT "X"
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_det_tra_acr.nss_afore
            AND    tipo_solicitud NOT IN (8,14,5) 
            GROUP BY 1
            
            IF SQLCA.SQLCODE <> 0 THEN
               LET vcausa_rechazo  = 1  #-- CUENTA NO ADMINISTRADA POR LA AFORE 
               LET xcodigo_rechazo = 8
            END IF 
         END IF

         IF reg_det_tra_acr.tipo_transferencia = "33" THEN
            SELECT "X"
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_det_tra_acr.nss_afore
            AND    tipo_solicitud = 5 
            GROUP BY 1
            
            IF SQLCA.SQLCODE <> 0 THEN
               LET vcausa_rechazo  = 1            #-- CUENTA NO ADMINISTRADA POR LA AFORE
               LET xcodigo_rechazo = 8
            END IF 
         END IF

         LET saldo_4 = 0                                        #CPL-1585
         
         SELECT NVL(sum(monto_en_acciones),0)
         INTO  saldo_4
         FROM  dis_cuenta
         WHERE nss = reg_det_tra_acr.nss_afore
         AND   subcuenta = 4

         LET saldo_8 = 0
         
         SELECT NVL(sum(monto_en_acciones),0)
         INTO  saldo_8
         FROM  dis_cuenta
         WHERE nss = reg_det_tra_acr.nss_afore
         AND   subcuenta = 8

         IF saldo_4 <= 0 AND saldo_8 <= 0 THEN  #CPL-1657 CUENTA SIN SALDO EN LAS
            LET vcausa_rechazo  = 3             #SUBCUENTAS DE VIVIENDA cod 3 Temp
            LET xcodigo_rechazo = 8
         END IF                                                 #CPL-1585 

         IF vcausa_rechazo <> 0 THEN   
            UPDATE safre_tmp:det_tra_acr
            SET    estado = xcodigo_rechazo,
                   causa_rechazo = vcausa_rechazo
            WHERE  nss_afore = reg_det_tra_acr.nss_afore         
         ELSE
            #-- MARCAJE DE LA OPERATIVA --#
            SELECT "X"
            FROM   cta_act_marca
            WHERE  marca_cod = 232
            AND    nss       = reg_det_tra_acr.nss_afore
            
            IF STATUS = NOTFOUND THEN
               LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                             "'",reg_det_tra_acr.nss_afore,"'",
                             ",",edo_proc,
                             ",",reg_det_tra_acr.cont_servicio,
                             ",",vmarca_estado,
                             ",",vcodigo_rechazo,
                             ",",pmarca_causa,
                             ",","'","'", ",",
                             "'",g_usuario,"'",")"
            
               LET ejecuta = ejecuta CLIPPED
               
               PREPARE clausula_spl232 FROM ejecuta
               DECLARE cursor_marca232 CURSOR FOR clausula_spl232
               OPEN  cursor_marca232
               FETCH cursor_marca232 INTO xcodigo_marca, xcodigo_rechazo
               CLOSE cursor_marca232
                        
               IF xcodigo_rechazo <> 0 THEN
                  LET vcausa_rechazo = 2 #-- CUENTA MARCADA EN PROCESO OPERATIVO
            
                  UPDATE safre_tmp:det_tra_acr
                  SET    estado = xcodigo_rechazo,
                         causa_rechazo = vcausa_rechazo
                  WHERE  nss_afore = reg_det_tra_acr.nss_afore   
               END IF
            END IF               
         END IF
      END IF


      #---SUMARIO DE SOLICITUD DE TRANSFERENCIA---#
      IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
         LET c2_ident_operacion = ""
         LET reg_sum_tra_acr.tipo_registro       = carga_reg[001,002]
         LET reg_sum_tra_acr.cant_reg_det        = carga_reg[003,011]
         LET reg_sum_tra_acr.sum_partic_v97      = carga_reg[042,056]
         LET reg_sum_tra_acr.sum_ult_apor_viv97  = carga_reg[057,071]

         INSERT INTO safre_tmp:sum_tra_acr
         VALUES(reg_sum_tra_acr.*)
      END IF

      LET bnd_proc = 0
   END FOREACH

   SELECT COUNT(*)
   INTO   vtotal_reg
   FROM   safre_tmp:det_tra_acr

   SELECT COUNT(*)
   INTO   vtotal_aprob
   FROM   safre_tmp:det_tra_acr c
   WHERE  c.estado = 0

   SELECT COUNT(*)
   INTO   vtotal_rech
   FROM   safre_tmp:det_tra_acr c
   WHERE  c.estado <> 0

   INSERT INTO acr_ctr_arh
   VALUES(generar,vtotal_reg,vtotal_aprob,vtotal_rech,vtotal_pend,today,g_usuario)
END FUNCTION


FUNCTION impresion_reporte(varchivo)
#ir---------------------------------
   DEFINE
      G_IMPRE        CHAR(300),
      c_impre        CHAR(300),
      gimpresion     CHAR(300),
      varchivo       CHAR(20)

   DEFINE
      w_codigo_afore LIKE tab_afore_local.codigo_afore

   SELECT codigo_afore
   INTO   w_codigo_afore
   FROM   tab_afore_local

   LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".SOL_ACR.",HOY USING "DD-MM-YYYY",
                 "_",hora CLIPPED

   START REPORT det_tra_acr_imp TO  G_IMPRE

      OUTPUT TO REPORT det_tra_acr_imp(g.*,varchivo)

   FINISH REPORT det_tra_acr_imp

   --LET gimpresion = "lp ",G_IMPRE
   --RUN gimpresion

   LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
   RUN c_impre
END FUNCTION


REPORT det_tra_acr_imp(g,varchivo)
#dtai-----------------------------
   DEFINE
      varchivo        CHAR(20)

   DEFINE 
      tot_reg         INTEGER

   DEFINE
      i        RECORD LIKE tab_afore_local.*
    	
   DEFINE
      g        RECORD
                  total               INTEGER,
                  tipo_transferencia  CHAR(2),
                  descripcion         CHAR(20)
               END RECORD

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
        
   FORMAT
      PAGE HEADER
         SELECT razon_social
         INTO i.razon_social
         FROM tab_afore_local
         
         SELECT COUNT(*)
         INTO   tot_reg
         FROM   safre_tmp:det_tra_acr
         
         PRINT COLUMN 01,"ACRC001",
               COLUMN 11,"REGISTROS SOLICITADOS PARA TRANSFERENCIA DE ACREDITADOS",
               COLUMN 69,TODAY USING "dd-mm-yyyy"
         
         PRINT COLUMN 27,"(OPERACION 01)"
         SKIP 2 LINE
         
         PRINT COLUMN 01,"NOMBRE DE ARCHIVO RECIBIDO : ", varchivo
         SKIP 1 LINE 
         
         PRINT COLUMN 01,"-------------------------------------------------------------------------------"
         PRINT COLUMN 09,"NUMERO",
               COLUMN 29,"TIPO DE"
         
         PRINT COLUMN 06,"DE REGISTROS",
               COLUMN 26,"TRANSFERENCIA",
               COLUMN 49,"DESCRIPCION"
         PRINT COLUMN 01,"-------------------------------------------------------------------------------"
         SKIP 1 LINE 
            
      ON EVERY ROW
         DECLARE cursor CURSOR FOR
         SELECT COUNT(*),
                tipo_transferencia
         FROM  safre_tmp:det_tra_acr
         GROUP BY 2
         ORDER BY 2
         
         FOREACH cursor INTO g.total,g.tipo_transferencia
            CASE g.tipo_transferencia
               WHEN "03" LET g.descripcion = "TRANSFERENCIA TOTAL"
               WHEN "33" LET g.descripcion = "TRANSFERENCIA TOTAL"
               WHEN "04" LET g.descripcion = "ULTIMA APORTACION"
               WHEN "34" LET g.descripcion = "ULTIMA APORTACION"
               WHEN "70" LET g.descripcion = "TRANSFERENCIA TOTAL"
               WHEN "71" LET g.descripcion = "ULTIMA APORTACION"
            END CASE
         
            PRINT COLUMN 09,g.total USING "#######",
                  COLUMN 32,g.tipo_transferencia,
                  COLUMN 49,g.descripcion
         END FOREACH

      ON LAST ROW
         SKIP 2 LINE 
         PRINT COLUMN 01,"-------------------------------------------------------------------------------"  
         PRINT COLUMN 1, "TOTALES : ",tot_reg USING "<<<<"

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
END REPORT


FUNCTION actualiza_operacion()
#ao---------------------------
    UPDATE bat_ctr_operacion
    SET    estado_operacion = 4,
           fecha_fin        = CURRENT,
           nom_archivo      = reg_bat.nombre_archivo
    WHERE  pid              = reg_bat.pid
    AND    proceso_cod      = reg_bat.proceso_cod
    AND    opera_cod        = reg_bat.opera_cod
END FUNCTION


FUNCTION impresion_reporte_fechpres(varchivo)
#irf-----------------------------------------
   DEFINE 
      w_codigo_afore LIKE tab_afore_local.codigo_afore

   DEFINE 
      G_IMPRE        CHAR(300),
      c_impre        CHAR(300),
      gimpresion     CHAR(300),
      varchivo       CHAR(20)

   DEFINE
      h       RECORD 
                 nss          CHAR(11),
                 fech_pres    DATE,
                 cod_edo      SMALLINT,
                 desc_edo     CHAR(40)
              END RECORD
  
   SELECT codigo_afore
   INTO   w_codigo_afore
   FROM   tab_afore_local

   LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".SOL_ACR_FECHPRES.",HOY USING "DD-MM-YYYY",
                 "_",hora CLIPPED

   START REPORT det_tra_acr_imp_fechpres TO  G_IMPRE
      DECLARE cur_report2 CURSOR FOR
      SELECT nss_afore,
             fecha_presentacion,
             estado,
             "",
             causa_rechazo
      FROM   safre_tmp:det_tra_acr
      WHERE  estado <> 0
      ORDER BY 1
      
      FOREACH cur_report2 INTO h.*,vcausa_rechazo
         IF vcausa_rechazo = 1 THEN 
            LET h.desc_edo = "CUENTA NO ADMINISTRADA POR LA AFORE"
         END IF
         
         IF vcausa_rechazo = 2 THEN
            LET h.desc_edo = "CUENTA MARCADA EN PROCESO OPERATIVO"
         END IF
         
         IF vcausa_rechazo = 3 THEN
            LET h.desc_edo = "NSS SIN SALDO EN SUBCUENTAS DE VIVIENDA"
         END IF
         
         IF vcausa_rechazo = 0 THEN
            SELECT rechazo_desc 
            INTO   h.desc_edo
            FROM   tab_rch_marca
            WHERE  rechazo_cod = h.cod_edo
         END IF
      
         OUTPUT TO REPORT det_tra_acr_imp_fechpres(h.*,varchivo)
      END FOREACH
   FINISH REPORT det_tra_acr_imp_fechpres

   --LET gimpresion = "lp ",G_IMPRE
   --RUN gimpresion

   LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
   RUN c_impre
END FUNCTION


REPORT det_tra_acr_imp_fechpres(h,varchivo)
#dtai---------------------------------------
   DEFINE
      varchivo       CHAR(20)
      
   DEFINE
      tot_reg        INTEGER

   DEFINE
      i       RECORD LIKE tab_afore_local.*
   
   DEFINE
      h       RECORD
                 nss           CHAR(11),
                 fecha_pres    DATE,
                 cod_edo       SMALLINT,
                 desc_edo      CHAR(40)
              END RECORD
   
   OUTPUT
       TOP MARGIN    1
       BOTTOM MARGIN 0
       LEFT MARGIN   0
       RIGHT MARGIN  0
       PAGE LENGTH  60
       
   FORMAT
      PAGE HEADER
         SELECT razon_social
         INTO i.razon_social
         FROM tab_afore_local
         
         SELECT COUNT(*)
         INTO  tot_reg
         FROM  safre_tmp:det_tra_acr
         WHERE estado <> 0
         
         PRINT COLUMN 01,"ACRC001",
               COLUMN 13,"DEVOLUCION DE REGISTROS DE TRANSFERENCIA DE ACREDITADOS",
               COLUMN 77,TODAY USING "dd-mm-yyyy"       
         SKIP 2 LINE
         
         PRINT COLUMN 01,"NOMBRE DE ARCHIVO RECIBIDO : ", generar
         SKIP 1 LINE
         
         PRINT COLUMN 01,"---------------------------------------------------------------------------------------------"
         PRINT COLUMN 08,"NSS",
               COLUMN 21,"FECHA PRESENTACION",
               COLUMN 44,"ESTADO",
               COLUMN 56,"DESC. ESTADO"
         PRINT COLUMN 01,"---------------------------------------------------------------------------------------------"
         SKIP 1 LINE    
 
      ON EVERY ROW
         PRINT COLUMN 05,h.nss,
               COLUMN 25,h.fecha_pres USING "dd-mm-yyyy",
               COLUMN 46,h.cod_edo USING "##",
               COLUMN 57,h.desc_edo
      
      ON LAST ROW  
         SKIP 2 LINE 
         PRINT COLUMN 01,"---------------------------------------------------------------------------------------------"        
         PRINT COLUMN 1, "TOTALES : ",tot_reg USING "<<<<"
      
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
END REPORT

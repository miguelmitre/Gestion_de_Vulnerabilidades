################################################################################
#Owner             => E.F.P.                                                   #
#Programa UNIC032  => RECIBE ARCHIVOS DE RESPUESTA DE NOTIFICACION             #
#Por               => ARMANDO RODRIGUEZ CASTROPAREDES                          #
#Fecha creacion    => 13 junio 2002                                            #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                          #
#Fecha act         => 3 diciembre 2003                                         #
#Fecha act         => 28 abril 2005                                            #
#Fecha act         => 24 mayo 2007                                             #
#Fecha act         => 28 mayo 2007                                             #
#Modificado por    => CESAR CHÁVEZ MARTÍNEZ                                    #
#Fecha act         => 14 junio 2007                                            #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                          #
#Fecha act         => 22 agosto 2007                                           #
#Fecha act         => 02 enero 2008                                            #
#Sistema           => UNI                                                      #
#Por               => SILVERIA CONTRERAS GARCIA                                #
#Fecha Mod         => 04 Diciembre 2008                                        #
#Modidicacion      => Se valida cuando no existe el archivo, para ke no truene #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_param_uni           RECORD LIKE seg_modulo.* 

   DEFINE HOY,
          vfinicio,
          vfecha1,
          vfecha_recepcion      DATE

   DEFINE carga_reg             CHAR(330),
          usuario               CHAR(008),
          enter                 CHAR(001),
          generar               CHAR(018),
          nombre_archivo        CHAR(018),
          archivo_traspaso      CHAR(200),
          c10_fecha_presenta    CHAR(010),
          c10_fecha_nac_uni     CHAR(010),
          c10_fecha_nac_cta1    CHAR(010) 

   DEFINE s_liquidado,
          s_confrontado,
          s_aceptado,
          s_solicitado,
          cuantos,
          vdia,
          cont,
          cont1,
          cont2                 SMALLINT

   DEFINE ultimo_folio          INTEGER

   DEFINE vcta_administra        CHAR(200),
          vcta_administra_salida CHAR(200)

--- CCM 14/06/07 
   DEFINE vdesmarca           CHAR(100),
          vintra_cta1,
          vintra_uni,
          vextra_cta1,
          vextra_uni          INTEGER,
          vcorrelativo,
          vestado_marca,
          vmarca_causa,
          x_marca_causa       SMALLINT,
          cont_inhab          SMALLINT
          --usuario           CHAR(8),
--- CCM 14/06/07
END GLOBALS
#####################################################################
MAIN
   OPTIONS
      PROMPT LINE LAST

   DEFER INTERRUPT

   CALL STARTLOG("UNIC032.log")

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_pla_trasp

      CREATE TEMP TABLE tmp_pla_trasp
      (n_registros          CHAR(330))
   WHENEVER ERROR STOP

   CALL init()

   OPEN WINDOW unic0011 AT 2,2 WITH FORM "UNIC0011" ATTRIBUTE(BORDER)
   DISPLAY "                          [ Ctrl-C ] Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " UNIC032       RECIBE ARCHIVOS DE RESPUESTA DE NOTIFICACION                                " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar WITHOUT DEFAULTS
      BEFORE FIELD generar
         LET generar = NULL
         CLEAR FORM

      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
         END IF

         SELECT nombre
         INTO   nombre_archivo
         FROM   uni_ctr_archivo
         WHERE  nombre = generar

         IF STATUS <> NOTFOUND THEN
            ERROR "ESTE ARCHIVO YA SE RECIBIO"
            SLEEP 3
            EXIT PROGRAM
         END IF

        WHENEVER ERROR CONTINUE -- SCG 04Dic08 -- 

         SELECT *
         INTO   g_param_uni.*
         FROM   seg_modulo
         WHERE  modulo_cod = "uni"

         LET archivo_traspaso = g_param_uni.ruta_rescate CLIPPED,"/",
                                generar CLIPPED

         LOAD FROM archivo_traspaso DELIMITER "+" INSERT INTO tmp_pla_trasp

         SELECT count(*)
         INTO   cuantos
         FROM   tmp_pla_trasp

         IF cuantos = 0 THEN
            DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO " AT 19,2 ATTRIBUTE(REVERSE)
            SLEEP 2
            NEXT FIELD generar
         ELSE
            EXIT INPUT
         END IF

         WHENEVER ERROR STOP  -- SCG 04Dic08 --

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
        EXIT PROGRAM 
   END INPUT

   DISPLAY " PROCESANDO INFORMACION " AT 21,1 ATTRIBUTE(REVERSE)

   CALL lee_archivo_plano()

   CALL actualiza_aceptados()

   DISPLAY "TOTAL DE CUENTAS INHABILITADAS ", cont_inhab AT 19,8

   INSERT INTO uni_ctr_archivo
   VALUES(generar,
          0,
          vfecha_recepcion,
          22)

   PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
   FOR CHAR enter

   CLOSE WINDOW unic0011
END MAIN
#####################################################################
FUNCTION init()

   LET HOY  = TODAY

   SELECT USER
   INTO   usuario
   FROM   uni_status
   GROUP BY 1

   LET s_confrontado = 20  --- "CONFRONTADO"
   LET s_aceptado    = 25  --- "ACEPTADO CONFRONTA"
   LET s_solicitado  = 30  --- "SOLICITADO"
   LET s_liquidado   = 95  --- "LIQUIDADO"

   LET  vfinicio = HOY
   LET  vdia     = DAY(vfinicio) - 1
   LET  vfinicio = vfinicio - vdia UNITS DAY
   LET  vfecha1  = vfinicio
   LET  vfecha1   = vfecha1 + 1 units month

   LET vfecha1 = habil_siguiente(vfecha1)

--- cuentas administradas
   {LET vcta_administra_salida = " EXECUTE FUNCTION fn_cuenta_saliente (?,?,?,?)" 
   PREPARE cta_administra_salida FROM vcta_administra_salida}
--- cuentas administradas

--- CCM 14/06/07
   LET vdesmarca     = " EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "
   LET vcorrelativo  = 0
   LET vestado_marca = 0
   LET x_marca_causa = 243
   LET vintra_uni    = 241 --- "UNIFICACION INTRA-AFORE"
   LET vintra_cta1   = 242 --- "UNIFICACION INTRA-AFORE U"
   LET vextra_uni    = 243 --- "UNIFICACION EXTRA-AFORE"
   LET vextra_cta1   = 244 --- "UNIFICACION EXTRA-AFORE U"
   LET vmarca_causa  = 0
   LET cont_inhab    = 0
--- CCM 14/06/07
END FUNCTION
#####################################################################
FUNCTION lee_archivo_plano()

   DEFINE motivo_rechazo1     CHAR(3),
          motivo_rechazo2     CHAR(3),
          motivo_rechazo3     CHAR(3),
          vnss                CHAR(11), 
          vresulta            CHAR(02),
          vdiag1              CHAR(02),
          vdiag2              CHAR(02),
          vdiag3              CHAR(02),
          vdiag4              CHAR(02),
          vdiag5              CHAR(02),
          vnss1               CHAR(11),
          vnss2               CHAR(11),
          vresulta1           CHAR(02),
          vdiag11             CHAR(02),
          vdiag21             CHAR(02),
          vdiag31             CHAR(02),
          vdiag41             CHAR(02),
          vdiag51             CHAR(02),
          c10_fecha_rec       CHAR(10),
          c10_fecha_not       CHAR(10),
          vfecha_notifica     DATE,
          vfecha_proxima      DATE,
          f_hoy               DATE,
          dia                 SMALLINT,
          vfolio              INTEGER

   DECLARE cur_1 CURSOR FOR
   SELECT  * 
   FROM    tmp_pla_trasp

   LET cont            = 0
   LET cont1           = 0
   LET cont2           = 0

   FOREACH cur_1 INTO carga_reg
      DISPLAY "TOTAL RECHAZADOS UNIFICADORES : ",cont  AT 15,8
      DISPLAY "TOTAL RECHAZADOS UNIFICADOS   : ",cont1  AT 16,8

      IF carga_reg[01,02] = "01" AND  
         carga_reg[28,29] = "02" THEN
         LET motivo_rechazo1= carga_reg[30,32]
         LET motivo_rechazo2= carga_reg[33,35]
         LET motivo_rechazo3= carga_reg[36,38]

         SELECT  max(folio)
         INTO    vfolio
         FROM    uni_cza_notifica 

         UPDATE  uni_cza_notifica
         SET     motivo_rechazo = motivo_rechazo1
         WHERE   folio = vfolio
         AND     estado = s_aceptado

         DISPLAY "ENCABEZADO CONTIENE ERROR ",motivo_rechazo1 AT 20,8
         SLEEP 3
         EXIT PROGRAM
      END IF

      IF carga_reg[1,2] = "01" THEN  
         LET c10_fecha_rec  = carga_reg[21,22],"/",
                              carga_reg[23,24],"/",
                              carga_reg[17,20]

         LET vfecha_recepcion = c10_fecha_rec

         IF carga_reg[5,6] <> "22" THEN  
            ERROR "EL ARCHIVO NO PERTENECE A RESPUESTA DE NOTIFICACION"
            SLEEP 3
            ERROR " "
            EXIT PROGRAM
         END IF
      END IF

      LET vfecha_notifica = HOY
      LET vfecha_notifica = vfecha_notifica + 1 UNITS MONTH
      LET vfecha_proxima  = MDY(MONTH(vfecha_notifica),1,YEAR(vfecha_notifica))
      LET vfecha_notifica = habil_siguiente(vfecha_proxima)

      IF carga_reg[1,2] = "02" THEN
         LET vnss = ""
         LET cont            = cont + 1 
         LET vnss            = carga_reg[041,051] 
         LET c10_fecha_not   = carga_reg[264,265],"/",
                               carga_reg[266,267],"/",
                               carga_reg[260,263]

         LET vresulta        = carga_reg[269,270] 
         LET vdiag1          = carga_reg[271,273]
         LET vdiag2          = carga_reg[274,276]
         LET vdiag3          = carga_reg[277,279]
         LET vdiag4          = carga_reg[280,282]
         LET vdiag5          = carga_reg[283,285]

         UPDATE  uni_unificador
         SET     estado          = 95,
                 fnotifica       = vfecha_notifica,
                 resul_operacion = vresulta,
                 diag_proceso1   = vdiag1,
                 diag_proceso2   = vdiag2,
                 diag_proceso3   = vdiag3,
                 diag_proceso4   = vdiag4,
                 diag_proceso5   = vdiag5
         WHERE   nss_uni         = vnss
         AND     estado          = 96

         UPDATE  uni_unificado
         SET     estado          = 95,
                 fnotifica       = vfecha_notifica
         WHERE   nss_uni         = vnss
         AND     estado          = 96
      ELSE
         IF carga_reg[1,2] = "03" THEN
            LET cont1    = cont1 + 1 
         END IF

         LET vnss1 = ""
         LET vnss2 = ""
         LET vnss1    = carga_reg[047,057] 
         LET vnss2    = carga_reg[013,023] 
         LET vresulta1= carga_reg[270,271] 
         LET vdiag11  = carga_reg[272,274]
         LET vdiag21  = carga_reg[275,277]
         LET vdiag31  = carga_reg[278,280]
         LET vdiag41  = carga_reg[281,283]
         LET vdiag51  = carga_reg[284,286]

         UPDATE  uni_unificado
         SET     estado = 95,
                 fnotifica         = vfecha_notifica,
                 resulta_operacion = vresulta1,
                 diag_proceso1     = vdiag11,
                 diag_proceso2     = vdiag21,
                 diag_proceso3     = vdiag31,
                 diag_proceso4     = vdiag41,
                 diag_proceso5     = vdiag51
         WHERE   nss_cta1          = vnss1
         AND     estado = 96

         UPDATE  uni_unificado
         SET     estado = 95,
                 fnotifica         = vfecha_notifica
         WHERE   nss_cta1          <> vnss1
         AND     nss_uni           = vnss2
         AND     estado = 96

         UPDATE  uni_unificador
         SET     estado = 95,
                 fnotifica         = vfecha_notifica
         WHERE   nss_uni           = vnss2
         AND     estado = 96
      END IF
   END FOREACH

   IF cont >=1 THEN
      DISPLAY "EL ARCHIVO CONTIENE ",cont CLIPPED,
              " SOLICITUDES RECHAZADAS" AT 18,8 ATTRIBUTE (REVERSE)
      DISPLAY "FECHA DE PROXIMA NOTIFICACION DE REGISTROS RECHAZADOS: ",
              vfecha_notifica AT 19,8 
   ELSE
       DISPLAY "" AT 21,1 ATTRIBUTE(REVERSE)
       DISPLAY "ARCHIVO FUE ACEPTADO" AT 18,8 ATTRIBUTE(REVERSE)
   END IF

END FUNCTION
#####################################################################
FUNCTION actualiza_aceptados()
--- cuentas administradas
   DEFINE x_afore_local     SMALLINT,
          x_accion          SMALLINT,
          xx_tipo_solicitud SMALLINT,
          v_cod_proceso     SMALLINT,
          v_rechazo         SMALLINT,
          xcve_operacion    CHAR(2)

   DEFINE salida RECORD
          nss_cta1         CHAR(11),
          tipo_ent_cta1    CHAR(2),
          cve_ent_cta1     CHAR(3),
          ident_movimiento CHAR(2)
   END RECORD
   
   DEFINE marcar RECORD
          nss_uni          CHAR(11),
          nss_cta1         CHAR(11)
   END RECORD

   DEFINE opc             CHAR(1),
          rpt_ccm_marcaje CHAR(200)

   --- CCM 14/06/07 MARCAJE
   DECLARE cur_marcaje CURSOR FOR
   SELECT  nss_uni,
           nss_cta1
   FROM    uni_unificado
   WHERE   estado = 96

   --LET rpt_ccm_marcaje = "/afore/hsb/safre/uni/fte/ver_uni/rpt_ccm"
   --START REPORT rpt_prueba TO rpt_ccm_marcaje

   FOREACH cur_marcaje INTO marcar.nss_uni,
                            marcar.nss_cta1
      --CALL actualiza_control(marcar.nss_cta1,marcar.nss_uni)
      CALL actualiza_control(marcar.nss_uni,marcar.nss_cta1)

     {      OUTPUT TO REPORT rpt_prueba(marcar.nss_cta1,
                                       vextra_cta1,
                                       vcorrelativo,
                                       vestado_marca,
                                       vmarca_causa,
                                       usuario,
                                       marcar.nss_uni,
                                       vextra_uni
                                       )
                                       }
   END FOREACH
   --FINISH REPORT rpt_prueba
   --- CCM 14/06/07 MARCAJE

   UPDATE uni_unificador
   SET    estado = 100
   WHERE  estado = 96

--- cuentas administradas
   {SELECT codigo_afore
   INTO   x_afore_local
   FROM   tab_afore_local

   DECLARE cur_cta_salida CURSOR FOR
   SELECT  a.nss_cta1,
           a.tipo_ent_cta1,
           a.cve_ent_cta1,
           b.ident_movimiento
   FROM    uni_unificado a, uni_unificador b
   WHERE   a.estado = 96
   AND     b.estado = 100
   AND     a.nss_uni = b.nss_uni
   AND     a.folio   = b.folio

   FOREACH cur_cta_salida INTO salida.nss_cta1,
                               salida.tipo_ent_cta1,
                               salida.cve_ent_cta1,
                               salida.ident_movimiento

      SELECT tipo_solicitud
      INTO   xx_tipo_solicitud
      FROM   afi_mae_afiliado
      WHERE  n_seguro = salida.nss_cta1

      LET x_accion = 1

      IF salida.ident_movimiento = "01" AND salida.tipo_ent_cta1 = "01" THEN
         LET xcve_operacion = "01"
         LET xx_tipo_solicitud = 4
      END IF

      IF salida.ident_movimiento = "02" AND salida.tipo_ent_cta1 = "00" THEN
         LET xcve_operacion = "30"
         LET x_accion = 2
      END IF

      IF salida.ident_movimiento = "02" AND salida.tipo_ent_cta1 = "01" THEN
         LET xcve_operacion = "31"
      END IF

      IF salida.ident_movimiento = "02" AND salida.tipo_ent_cta1 = "59" THEN
         IF salida.cve_ent_cta1 = x_afore_local THEN
            LET xcve_operacion = "33"
         ELSE  
            LET xcve_operacion = "34"
         END IF
         LET x_accion = 2
      END IF

      SELECT a.cod_proceso
      INTO   v_cod_proceso
      FROM   tab_tipo_proceso a
      WHERE  a.tipo_proceso  = xx_tipo_solicitud
      AND    a.cod_operacion = xcve_operacion
      AND    a.accion        = x_accion

      DECLARE cur_cta_salida_1 CURSOR FOR cta_administra_salida

      OPEN  cur_cta_salida_1 USING salida.nss_cta1, #nss
                                   HOY,             #fecha_ingreso
                                   v_cod_proceso,   #cod_proceso
                                   usuario          #usuario

         FETCH cur_cta_salida_1 INTO v_rechazo
      CLOSE cur_cta_salida_1

   END FOREACH}
--- cuentas administradas

   UPDATE uni_unificado
   SET    estado = 100
   WHERE  estado = 96

END FUNCTION
#####################################################################
FUNCTION actualiza_control(nss_u,nss_un)

   DEFINE nss_u            CHAR(11),
          nss_un           CHAR(11),
          marca_unificador SMALLINT,
          marca_unificado  SMALLINT

   LET marca_unificador = 0
   LET marca_unificado  = 0

   #CPL-1680
   DECLARE cur_marca_unificado CURSOR FOR
   SELECT marca_cod
   FROM   cta_act_marca
   WHERE  nss        = nss_un
   AND    marca_cod IN (241,242,243,244)
   
   FOREACH cur_marca_unificado INTO marca_unificado

      IF marca_unificado > 0 THEN
         PREPARE marcaje1 FROM vdesmarca
         EXECUTE marcaje1 USING nss_un,            # nss
                                marca_unificado,   # marca_entra
                                vcorrelativo,      # correlativo
                                vestado_marca,     # estado_marca
                                vmarca_causa,      # marca_causa
                                usuario            # usuario
      END IF
   END FOREACH

   #CPL-1680
   DECLARE cur_marca_unificador CURSOR FOR
   SELECT marca_cod
   FROM   cta_act_marca
   WHERE  nss        = nss_u
   AND    marca_cod IN (241,242,243,244)
   
   FOREACH cur_marca_unificador INTO marca_unificador
   	
      IF marca_unificador > 0 THEN
         PREPARE marcaje FROM vdesmarca
         EXECUTE marcaje USING nss_u,             # nss
                               marca_unificador,  # marca_entra
                               vcorrelativo,      # correlativo
                               vestado_marca,     # estado_marca
                               vmarca_causa,      # marca_causa
                               usuario            # usuario
      END IF
   END FOREACH

   LET cont_inhab = cont_inhab + 1

END FUNCTION
#####################################################################
FUNCTION habil_siguiente(diaActual)

   DEFINE diaTmp        DATE,
          contador      SMALLINT,
          diaActual     DATE

   DEFINE diaHabilSig   DATE,
          diaSemana     SMALLINT,
          feriado       SMALLINT,
          finSemana     SMALLINT

   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)  

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado 
      WHERE  feria_fecha = diaHabilSig

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF 

      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig

END FUNCTION

###########################################################################
REPORT rpt_prueba(lr_prueba_un)

   DEFINE lr_prueba_un RECORD
          nss_un          CHAR(11),
          vextra_cta1     INTEGER,
          vcorrelativo,
          vestado_marca,
          vmarca_causa    SMALLINT,
          usuario         CHAR(08),
          nss_u           CHAR(11),
          vextra_uni      INTEGER
   END RECORD
   
   OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   110

   FORMAT
   PAGE HEADER
{
        10        20        30        40        50        60        70        80        90       100       111
12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
nss_un      vextra_cta1      vcorrelativo   vestado_marca  vmarca_causa   usuario   nss_u       vextra_uni
}  

      PRINT COLUMN 001, "nss_un",
            COLUMN 013, "vextra_cta1",
            COLUMN 030, "vcorrelativo",
            COLUMN 045, "vestado_marca",
            COLUMN 060, "vmarca_causa",
            COLUMN 075, "usuario",
            COLUMN 085, "nss_u",
            COLUMN 097, "vextra_uni"
      
   ON EVERY ROW
      PRINT COLUMN 001, lr_prueba_un.nss_un,
            COLUMN 013, lr_prueba_un.vextra_cta1,
            COLUMN 030, lr_prueba_un.vcorrelativo,
            COLUMN 045, lr_prueba_un.vestado_marca,
            COLUMN 060, lr_prueba_un.vmarca_causa,
            COLUMN 075, lr_prueba_un.usuario,
            COLUMN 085, lr_prueba_un.nss_u,
            COLUMN 097, lr_prueba_un.vextra_uni
            
   PAGE TRAILER
      SKIP 3 LINE
      PRINT COLUMN 01," Pagina : ",PAGENO USING"<<<<<"
      SKIP 2 LINE
END REPORT
###########################################################################

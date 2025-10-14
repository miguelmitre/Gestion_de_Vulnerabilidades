################################################################################
#Proyecto          => SISTEMA DE AFORE.( MEXICO )                              #
#Modulo            => COM                                                      #
#Programa          => COML003                                                  #
#Desscripcion      => GENERA REPORTE DE CAMBIO DE COMISION DE UN PROMOTOR A OTR#
#Autor             => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha             => 29 SEPTIEMBRE 2003.                                      #
#Modifico          => ALEJANDRO RAMIREZ (31 de OCT de 2003)                    #
#Desc.             => Agregue al reporte la informacion de afi_solicitud       #
################################################################################

DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        fecha_hasta DATE 
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        NOM_ARCHIVO           CHAR(200) ,
        seg_usuario           CHAR(008) ,
        RUTA                  CHAR(200) ,
        G_LISTA               CHAR(200) ,
        enter                 CHAR(001)

    DEFINE #loc #integer
        cont_de_registros     INTEGER

    DEFINE r_pro RECORD
      agenc_cod       LIKE pro_mae_promotor.agenc_cod,
      codven          LIKE pro_mae_promotor.codven,
      cod_promotor    LIKE afi_mae_afiliado.cod_promotor,
      nombres_pro     LIKE pro_mae_promotor.nombres,
      paterno_pro     LIKE pro_mae_promotor.paterno,
      materno_pro     LIKE pro_mae_promotor.materno,
      n_seguro        LIKE afi_solicitud.n_seguro,
      n_folio         LIKE afi_solicitud.n_folio,
      frecafor        LIKE afi_solicitud.frecafor,
      tipo_solicitud  LIKE afi_solicitud.tipo_solicitud,
      paterno         LIKE afi_solicitud.paterno,
      materno         LIKE afi_solicitud.materno,
      nombres         LIKE afi_solicitud.nombres,
      salario_actual  LIKE afi_solicitud.salario_actual,
      status_interno  LIKE afi_solicitud.status_interno,
      codven2         LIKE afi_solicitud.codven,
      estado_desc     LIKE tab_status_afi.estado_desc,
      observacion     CHAR(100)
   END RECORD


    DEFINE ejecuta    CHAR(800)
    DEFINE reg_prod          RECORD LIKE com_comis_detalle.*
    DEFINE reg_prod2         RECORD LIKE com_comis_detalle.*
    DEFINE saldo             LIKE dis_cuenta.monto_en_pesos 
    DEFINE total_acciones     DECIMAL(16,6)
    DEFINE valor_accion       DECIMAL(12,6) 
    DEFINE aux2_paterno       CHAR(50),
           aux2_materno       CHAR(50),
           aux2_nombres       CHAR(50)
    DEFINE aux3_paterno       CHAR(50),
           aux3_materno       CHAR(50),
           aux3_nombres       CHAR(50)
    DEFINE vcodven CHAR(10)
    DEFINE vcod_unidad        LIKE pro_mae_promotor.agenc_cod
END GLOBALS


MAIN
   DEFER INTERRUPT
   OPTIONS
       ACCEPT KEY CONTROL-I,
       INPUT WRAP,
       PROMPT LINE LAST

   CALL init()
   
   OPEN WINDOW coml0021 AT 4,4 WITH FORM "COML0031" ATTRIBUTE(BORDER)
   DISPLAY "                             <Ctrl-C> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " COML003     REPORTE CAMBIOS DE PROMOTORES Y ESTADO DEL PAGO                   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg_1.* WITHOUT DEFAULTS
      AFTER FIELD fecha_hasta
         IF reg_1.fecha_hasta IS NULL THEN
            ERROR "CAMPO NO PUEDE SER NULO"
            NEXT FIELD fecha_hasta
         END IF

      ON KEY (ESC)
         EXIT INPUT

      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
         EXIT PROGRAM

   END INPUT

   CLOSE WINDOW coml0021
   CLEAR SCREEN
   DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
   CALL obtenemos_reportes()
   CLEAR SCREEN

END MAIN

FUNCTION init()

   LET HOY = TODAY
   LET reg_1.fecha_hasta = HOY
   
   SELECT ruta_envio ,
          USER
   INTO   RUTA ,
          seg_usuario
   FROM   seg_modulo   
   WHERE  modulo_cod = "com"

--prompt "RUTA ", RUTA for enter

END FUNCTION


##----------------------------------------------------------------------------
## OBTENEMOS LOS REPORTES
##----------------------------------------------------------------------------

FUNCTION obtenemos_reportes()

   DEFINE reg_pro RECORD LIKE pro_mae_promotor.*
   DEFINE reg_afi RECORD LIKE afi_mae_afiliado.*
   DEFINE reg_cta RECORD LIKE dis_cuenta.*
   DEFINE salario_minimo DECIMAL(12,6)
   DEFINE temp_fecha DATE
   DEFINE encabeza CHAR(310)

   SELECT  A.precio_del_dia
   INTO    valor_accion
   FROM    glo_valor_accion A
   WHERE   A.fecha_valuacion = reg_1.fecha_hasta

   SELECT  B.monto_sm
   INTO    salario_minimo
   FROM    tab_salario_minimo B
   WHERE   B.fecha_hasta_sm is null
   AND     B.fecha_desde_sm is not null


   LET saldo = 0 

   DECLARE cur_1 CURSOR FOR 
   SELECT *
   FROM   com_comis_detalle 
   WHERE fentcons  <= reg_1.fecha_hasta
   AND    tipo_solicitud IN (1,2)
   --AND    fecha_corte <= "04/20/2004"
   -----AND    fecha_corte <> "05/31/2004"
   -----AND    fecha_corte <> "04/30/2004"
   AND    fecha_corte < "04/30/2004"  ---ya lo de adelante es nuevo esquema

   ORDER  BY codven,n_folio
 
   LET NOM_ARCHIVO = "ESTADO_PAGO_1_",HOY USING"DDMMYYYY"
   LET G_LISTA = RUTA CLIPPED,"/",NOM_ARCHIVO CLIPPED

   START REPORT listado_1 TO G_LISTA
   LET cont_de_registros = 0

   FOREACH cur_1 INTO reg_prod.*

      --TOTAL_DE ACCIONES
      SELECT SUM(monto_en_acciones)
      INTO   total_acciones
      FROM   dis_cuenta
      WHERE  nss       =   reg_prod.nss
      AND    subcuenta NOT IN (4,8)
      
      IF total_acciones IS NULL THEN
         LET total_acciones = 0
      END IF

      LET saldo  =  total_acciones * valor_accion

      OUTPUT TO REPORT listado_1(reg_prod.*,saldo)


   END FOREACH
   FINISH REPORT listado_1




   ----------------------------------------------------------------------------
   --------- EMITE LISTADO DE PROMOTORES QUE NO TIENEN com_comis_detalle ------
   ----------------------------------------------------------------------------
   DECLARE cur_no CURSOR FOR
   SELECT d.agenc_cod,
          d.codven,
          s.cod_promotor,
          D.nombres,
          D.paterno,
          D.materno,
          s.n_seguro,
          s.n_folio,
       -- s.frecafor,
          s.fecha_elaboracion,
          s.tipo_solicitud,
          s.paterno,
          s.materno,
          s.nombres,
          s.salario_actual,
          s.status_interno,
          s.codven,
          "",
          ""
   FROM   afi_solicitud  s,
          pro_mae_promotor d
   WHERE  s.n_folio NOT IN (SELECT n_folio FROM com_comis_detalle)
   AND    s.cod_promotor    = d.cod_promotor
   AND    s.fecha_elaboracion <= reg_1.fecha_hasta
   AND    s.status_interno < 100


   LET NOM_ARCHIVO = "ESTADO_PAGO_3_",HOY USING"DDMMYYYY"
   LET G_LISTA = RUTA CLIPPED,"/",NOM_ARCHIVO CLIPPED
   START REPORT listado_3 TO G_LISTA

   LET cont_de_registros = 0

   FOREACH cur_no INTO r_pro.*

      -- Obtenemos la descripcion del estado
      SELECT estado_desc INTO r_pro.estado_desc
      FROM tab_status_afi
      WHERE estado_cod = r_pro.status_interno

      IF r_pro.estado_desc IS NULL THEN
         LET r_pro.estado_desc = " "
      END IF

      OUTPUT TO REPORT listado_3(r_pro.*)

   END FOREACH

   FINISH REPORT listado_3                   



   ----------------------------------------------------------------------------
   ----SE FORMA UN SOLO ARCHIVO
   ----------------------------------------------------------------------------


   LET ejecuta="cd ",RUTA CLIPPED,"/",
   "; cat ESTADO_PAGO_1_",HOY USING"DDMMYYYY",
   " ESTADO_PAGO_3_",HOY USING"DDMMYYYY",
   " > ESTADO_PAGO.",HOY USING"DDMMYYYY" CLIPPED

   RUN ejecuta                              

-- DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
   LET ejecuta = "cd ",RUTA CLIPPED,"; rm ESTADO_PAGO_1_",HOY USING"DDMMYYYY"
   RUN ejecuta


-- DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
   LET ejecuta = "cd ",RUTA CLIPPED,"; rm ESTADO_PAGO_3_",HOY USING"DDMMYYYY"
   RUN ejecuta

   LET ejecuta = "cd ",RUTA CLIPPED,"; sort -u ",
                 "ESTADO_PAGO.",HOY USING"DDMMYYYY" CLIPPED,
                 " > paso "
   RUN ejecuta

   -- Concatenamos el ancabezado
   LET encabeza = "AGENTE_COD.'|'NUM_PROMOTOR.'|'COD_PROMOTOR.'|'",
                  "NOMBRE_PROMOTOR.'|'NSS.'|'FOLIO.'|'FEC_FIRMA.'|'",
                  "TIPO_SOLICITUD.'|'NOM_TRABAJADOR.'|'SALARIO.'|'",
                  "FEC_APORTACION.'|'SALDO_TRABAJA.'|'FEC_PAGO.'|'",
                  "STATUS_PAGO'|'MONTO_COMIS.'|'",
                  "DES_STATUS.'|'"


   LET ejecuta = "cd ",RUTA CLIPPED,"/",
                 "; echo ",encabeza CLIPPED,
                 "> paso2" CLIPPED
   RUN ejecuta

   LET ejecuta = "cd ",RUTA CLIPPED,"; cat paso >> paso2" CLIPPED,
                 ";rm paso "
   RUN ejecuta

   LET ejecuta = "cd ",RUTA CLIPPED,"; mv paso2 ",
               "ESTADO_PAGO.",HOY USING"DDMMYYYY" CLIPPED
   RUN ejecuta 



END FUNCTION 




REPORT listado_1(reg_2,sdo)
    DEFINE reg_2              RECORD LIKE com_comis_detalle.*
    DEFINE  sdo               LIKE dis_cuenta.monto_en_pesos

   DEFINE r_edo RECORD
      n_seguro        LIKE afi_solicitud.n_seguro,
      n_folio         LIKE afi_solicitud.n_folio,
      frecafor        LIKE afi_solicitud.frecafor,
      tipo_solicitud  LIKE afi_solicitud.tipo_solicitud,
      paterno         LIKE afi_solicitud.paterno,
      materno         LIKE afi_solicitud.materno,
      nombres         LIKE afi_solicitud.nombres,
      salario_actual  LIKE afi_solicitud.salario_actual,
      status_interno  LIKE afi_solicitud.status_interno,
      estado_desc     LIKE tab_status_afi.estado_desc,
      observacion     CHAR(50)
   END RECORD

   DEFINE desc CHAR(100)
   DEFINE estado_desc CHAR(40)
   DEFINE obser CHAR(100)
   DEFINE status_afi  SMALLINT
   DEFINE temp_felabora DATE
   DEFINE temp_promotor CHAR(10)
   DEFINE temp_codven LIKE pro_mae_promotor.codven

   DEFINE vfecha_rechazo DATE
   DEFINE fec_recep LIKE dis_det_aporte.fecha_recepcion

   DEFINE nombre     CHAR(60), 
          nombre_pro CHAR(60),
          nombre_sol CHAR(60)
   DEFINE nom        CHAR(60),
          ap_paterno CHAR(60),
          ap_materno CHAR(60)

    OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   1

    FORMAT

   ON EVERY ROW

              SELECT MIN(fecha_recepcion)
              INTO fec_recep
              FROM dis_det_aporte
              WHERE n_seguro = reg_2.nss
 
              IF fec_recep IS NULL THEN
                 LET fec_recep = ""
                 LET reg_2.monto_comision = 0
              END IF

              LET ap_paterno=""
              LET ap_materno=""
              LET nom=""
              LET vcod_unidad="" 
              SELECT paterno,
                     materno,
                     nombres,
                     codven,
                     agenc_cod 
              INTO   ap_paterno,
                     ap_materno,
                     nom,
                     vcodven,
                     vcod_unidad
              FROM   pro_mae_promotor
              WHERE  cod_promotor = reg_2.codven   

        LET nombre_pro = ap_paterno CLIPPED," ",
                         ap_materno CLIPPED," ",
                         nom CLIPPED

        LET cont_de_registros = cont_de_registros + 1
   
        LET aux2_paterno = ""
        LET aux2_materno = ""
        LET aux2_nombres = ""
       
        SELECT paterno,
               materno,
               nombres
        INTO aux2_paterno,aux2_materno,aux2_nombres 
        FROM   afi_mae_afiliado
        WHERE  n_folio = reg_2.n_folio
        AND    tipo_solicitud = reg_2.tipo_solicitud

        LET nombre =     aux2_paterno CLIPPED," ",
                         aux2_materno CLIPPED," ",
                         aux2_nombres CLIPPED
        
        SELECT MAX(f_rechazo)
        INTO   vfecha_rechazo
        FROM   afi_rechaza_cert
        WHERE  n_folio        = reg_2.n_folio
        AND    tipo_solicitud = reg_2.tipo_solicitud
        AND    n_seguro       = reg_2.nss

        IF vfecha_rechazo IS NOT NULL THEN
          SELECT MAX(observacion)
          INTO   obser
          FROM   afi_rechaza_cert
          WHERE  n_folio        = reg_2.n_folio
          AND    tipo_solicitud = reg_2.tipo_solicitud
          AND    n_seguro       = reg_2.nss
          AND    f_rechazo      = vfecha_rechazo
        ELSE
          LET obser = ""
        END IF

        LET temp_felabora =""
        SELECT fecha_elaboracion
        INTO temp_felabora
        FROM afi_solicitud
        WHERE n_folio = reg_2.n_folio
        AND n_seguro = reg_2.nss 
        AND tipo_solicitud = reg_2.tipo_solicitud

        -- fecha de firma
        IF temp_felabora IS NOT NULL THEN
           LET reg_2.fentcons = temp_felabora
        END IF

        -- Numero de promotor
        IF (vcodven IS NULL OR vcodven = 0 OR vcodven ="") THEN
           LET vcodven = reg_2.codven
        END IF


        LET desc =""

        PRINT
          --COLUMN 001,reg_2.coduni_n1                         ,"|",--agenc_cod
            COLUMN 001,vcod_unidad                             ,"|",
            COLUMN 012,reg_2.codven                            ,"|",
            COLUMN 023,vcodven                                 ,"|", 
            COLUMN 034,nombre_pro                              ,"|",
            COLUMN 095,reg_2.nss                               ,"|",
            COLUMN 107,reg_2.n_folio                           ,"|",
            COLUMN 114,reg_2.fentcons  USING "DDMMYYYY"        ,"|",
            COLUMN 125,reg_2.tipo_solicitud                    ,"|",
            COLUMN 127,nombre                                  ,"|",
            COLUMN 188,reg_2.salario_base_comis USING "##########&&.&&&&&&","|",
            COLUMN 208,fec_recep USING "DDMMYYYY"              ,"|",
            COLUMN 217,sdo            USING "##########&&.&&&&&&","|",
           COLUMN  237,reg_2.fecha_pago      USING "DDMMYYYY"   ,"|",
           COLUMN  245,reg_2.comis_pagada                       ,"|",
          --COLUMN 237,reg_2.monto_comision USING    "##&&.&&" ,"|",
            COLUMN 247,reg_2.monto_comision USING    "##&&.&&" ,"|",
            COLUMN 255,desc CLIPPED,                            "|"

END REPORT 



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
REPORT listado_3(r_pro)

DEFINE r_pro RECORD
      agenc_cod       LIKE pro_mae_promotor.agenc_cod,
      codven          LIKE pro_mae_promotor.codven,
      cod_promotor    LIKE afi_mae_afiliado.cod_promotor,
      nombres_pro     LIKE pro_mae_promotor.nombres,
      paterno_pro     LIKE pro_mae_promotor.paterno,
      materno_pro     LIKE pro_mae_promotor.materno,
      n_seguro        LIKE afi_solicitud.n_seguro,
      n_folio         LIKE afi_solicitud.n_folio,
    --frecafor        LIKE afi_solicitud.frecafor,
      fecha_elaboracion LIKE afi_solicitud.fecha_elaboracion,
      tipo_solicitud  LIKE afi_solicitud.tipo_solicitud,
      paterno         LIKE afi_solicitud.paterno,
      materno         LIKE afi_solicitud.materno,
      nombres         LIKE afi_solicitud.nombres,
      salario_actual  LIKE afi_solicitud.salario_actual,
      status_interno  LIKE afi_solicitud.status_interno,
      codven2         LIKE afi_solicitud.codven,
      estado_desc     LIKE tab_status_afi.estado_desc,
      observacion     CHAR(50)
   END RECORD

   DEFINE desc CHAR(100)
   DEFINE vfecha_rechazo DATE
   DEFINE temp_codven LIKE pro_mae_promotor.codven
   DEFINE nombre     CHAR(60),
          nombre_pro CHAR(60),
          nombre_sol CHAR(60)

    OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   1

    FORMAT

   ON EVERY ROW 

        LET cont_de_registros = cont_de_registros + 1

        LET nombre =r_pro.paterno CLIPPED," ",
                    r_pro.materno CLIPPED," ", 
                    r_pro.nombres CLIPPED," "


        LET nombre_pro = r_pro.paterno_pro CLIPPED," ",
                         r_pro.materno_pro CLIPPED," ",
                         r_pro.nombres_pro CLIPPED

        SELECT MAX(f_rechazo)
        INTO   vfecha_rechazo
        FROM   afi_rechaza_cert
        WHERE  n_folio        = r_pro.n_folio
        AND    tipo_solicitud = r_pro.tipo_solicitud
        AND    n_seguro       = r_pro.n_seguro

      DECLARE c_12 CURSOR FOR 
        SELECT observacion
        FROM   afi_rechaza_cert
        WHERE  n_folio        = r_pro.n_folio
        AND    tipo_solicitud = r_pro.tipo_solicitud
        AND    n_seguro       = r_pro.n_seguro
        AND    f_rechazo      = vfecha_rechazo
        ORDER BY 1
      FOREACH c_12 INTO r_pro.observacion
         EXIT FOREACH
      END FOREACH

        LET desc = r_pro.estado_desc CLIPPED," ",r_pro.observacion CLIPPED

        -- Numero de promotor
        LET temp_codven = ""
       IF (r_pro.codven IS NULL OR r_pro.codven = 0 OR r_pro.codven ="") THEN
           LET temp_codven = r_pro.codven2
       ELSE
           LET temp_codven = r_pro.codven
       END IF


        PRINT
            COLUMN 001,r_pro.agenc_cod                         ,"|",
          --COLUMN 012,r_pro.codven                            ,"|",
            COLUMN 012,r_pro.cod_promotor                      ,"|",
            COLUMN 023,temp_codven                             ,"|",
            COLUMN 034,nombre_pro                              ,"|",
            COLUMN 095,r_pro.n_seguro                          ,"|",
            COLUMN 107,r_pro.n_folio                           ,"|",
          --COLUMN 114,r_pro.frecafor       USING "DDMMYYYY"   ,"|",
            COLUMN 114,r_pro.fecha_elaboracion USING "DDMMYYYY" ,"|",
            COLUMN 125,r_pro.tipo_solicitud                    ,"|",
            COLUMN 127,nombre                                  ,"|",
            COLUMN 188,r_pro.salario_actual USING "##########&&.&&&&&&","|",
            COLUMN 208,"       "                                ,"|",
            COLUMN 217,0                    USING "##########&&.&&&&&&","|",
           COLUMN 237,"        "                               ,"|",
           COLUMN 245,"N"                                      ,"|",
            COLUMN 247,0                    USING    "##&&.&&" ,"|",
            COLUMN 255,desc CLIPPED,                            "|"



            LET nombre_sol = ""
            LET desc       = ""

END REPORT

######################################################################
#Proyecto     => safre_af                                            #
#Propietario  => E.F.P.                                              #
#Programa     => CTAB007                                             #
#Descripcion  => PANTALLA DE CONSULTA DE ESTADO DE CUENTA            #
#Fecha        => 24 DE JUNIO DE 2005                                 #
#Por          => OMAR SANDOVAL BADILLO                               #
#Sistema      => CTA.                                                #
######################################################################
DATABASE  safre_af
GLOBALS
   DEFINE g_reg      RECORD
          nombre_archivo     CHAR(25)
   END RECORD

   DEFINE g_parametro RECORD LIKE seg_modulo.*

   DEFINE generar            CHAR(100)

   DEFINE hoy                DATE,
          usuario            CHAR(08),
          opc                CHAR(01),
          ejecuta            CHAR(200),
          n_seguro           CHAR(11)
          
END GLOBALS
##############################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("CTAB007.log")

   SELECT s.*,
          USER
   INTO   g_parametro.*,
          usuario
   FROM   seg_modulo s
   WHERE  s.modulo_cod = "cta"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "CTAB0071" ATTRIBUTE(BORDER)
   DISPLAY " CTAB007           PANTALLA DE CONSULTA DE ESTADO DE CUENTA                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "                        LECTURA  DE  ARCHIVO  Y  PROCESO                       " AT 5,1 ATTRIBUTE(REVERSE) 

   MENU "ESTADO DE CUENTA"
      COMMAND "Lectura" "Lectura archvio de estado de cuenta."
         CALL lectura()
      COMMAND "Salida" "Salida del programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN
##############################################
FUNCTION crea_temporal()

   WHENEVER ERROR CONTINUE

   DATABASE safre_tmp
   DROP TABLE tmp_emision_edo_cta

   WHENEVER ERROR STOP

   CREATE TABLE tmp_emision_edo_cta
      (tipo_registro           CHAR(2),
       consecutivo_envio       INTEGER,
       periodo_desde           DATE,   
       periodo_hasta           DATE,   
       nombre_completo         CHAR(120),
       calle_num               CHAR(62),
       colonia                 CHAR(60),
       cp                      INTEGER,
       centro_de_reparto       CHAR(5),
       entidad_federal         CHAR(40),
       munic_delega            CHAR(40),
       fecha_certificacion     CHAR(8),    
       identificador_envio     INTEGER,
       leyenda                 CHAR(1),
       nss                     CHAR(11),
       rfc                     CHAR(13),
       curp                    CHAR(18),
       retiro97_ini            DECIMAL(11,2),
       cv_ini                  DECIMAL(11,2),
       cs_ini                  DECIMAL(11,2),
       sar_imss_ini            DECIMAL(11,2),
       sar_issste_ini          DECIMAL(11,2),
       sub_ini_rcv             DECIMAL(11,2), 
       retiro97_netas          DECIMAL(11,2),
       cv_netas                DECIMAL(11,2),
       cs_netas                DECIMAL(11,2),
       sar_imss_netas          DECIMAL(11,2),
       sar_issste_netas        DECIMAL(11,2),
       sub_netas_rcv           DECIMAL(11,2),
       retiro97_retiros        DECIMAL(11,2),
       cv_retiros              DECIMAL(11,2),
       cs_retiros              DECIMAL(11,2),
       sar_imss_retiros        DECIMAL(11,2),
       sar_issste_retiros      DECIMAL(11,2),
       sub_retiros_rcv         DECIMAL(11,2),
       retiro97_fin            DECIMAL(11,2),
       cv_fin                  DECIMAL(11,2),
       cs_fin                  DECIMAL(11,2),
       sar_imss_fin            DECIMAL(11,2),
       sar_issste_fin          DECIMAL(11,2),
       sub_fin_rcv             DECIMAL(11,2),
       viv_97_imss_ini         DECIMAL(11,2),
       viv_92_imss_ini         DECIMAL(11,2),
       viv_92_issste_ini       DECIMAL(11,2),
       sub_viv_ini             DECIMAL(11,2),
       viv_97_imss_netas       DECIMAL(11,2),
       viv_92_imss_netas       DECIMAL(11,2),
       viv_92_issste_netas     DECIMAL(11,2),
       sub_viv_netas           DECIMAL(11,2),
       viv_97_imss_retiros     DECIMAL(11,2),
       viv_92_imss_retiros     DECIMAL(11,2),
       viv_92_issste_retiros   DECIMAL(11,2),
       sub_viv_retiros         DECIMAL(11,2),
       viv_97_imss_fin         DECIMAL(11,2),
       viv_92_imss_fin         DECIMAL(11,2),
       viv_92_issste_fin       DECIMAL(11,2),
       sub_viv_fin             DECIMAL(11,2),
       vol_ini                 DECIMAL(11,2),
       com_ini                 DECIMAL(11,2),
       sub_vcom_ini            DECIMAL(11,2),
       vol_netas               DECIMAL(11,2),
       com_netas               DECIMAL(11,2),
       sub_vcom_netas          DECIMAL(11,2),
       vol_retiros             DECIMAL(11,2),
       com_retiros             DECIMAL(11,2),
       sub_vcom_retiros        DECIMAL(11,2),
       vol_fin                 DECIMAL(11,2),
       com_fin                 DECIMAL(11,2),
       sub_vcom_fin            DECIMAL(11,2),
       total_general_ini       DECIMAL(11,2),
       total_general_netas     DECIMAL(11,2),
       total_general_retiros   DECIMAL(11,2),
       total_general_fin       DECIMAL(11,2),
       sub_netas_rcv_a         DECIMAL(11,2),
       sub_viv_netas_a         DECIMAL(11,2),
       sub_vcom_netas_a        DECIMAL(11,2),
       total_semestre          DECIMAL(11,2), 
       periodo_pago_1          CHAR(6),
       reg_patron_1            CHAR(11),
       sbc_1                   DECIMAL(11,2),
       dias_cotizados_1        INTEGER,
       periodo_pago_2          CHAR(6),
       reg_patron_2            CHAR(11),
       sbc_2                   DECIMAL(11,2),
       dias_cotizados_2        INTEGER,
       periodo_pago_3          CHAR(6),
       reg_patron_3            CHAR(11),
       sbc_3                   DECIMAL(11,2),
       dias_cotizados_3        INTEGER,
       rendimiento_siefore1    DECIMAL(6,2),  
       rendimiento_siefore2    DECIMAL(6,2),
       rendimiento_adicional   DECIMAL(6,2),
       rendimiento_infonavit   DECIMAL(6,2),
       rendimiento_fovissste   DECIMAL(6,2),
       identificador_comision  INTEGER,
       estructura_flujo1       DECIMAL(6,2),
       comision_flujo1         DECIMAL(11,2),
       estructura_flujo2       DECIMAL(6,2),
       comision_flujo2         DECIMAL(11,2),
       estructura_saldo        DECIMAL(11,2),
       comision_saldo          DECIMAL(11,2),
       total_comisiones        DECIMAL(11,2),
       estructura_adicional    DECIMAL(11,2),
       comision_adicional      DECIMAL(11,2),
       ind_fiscal              CHAR(1),
       fecha_rcv_fiscal        CHAR(8),   
       monto_rcv_fiscal        DECIMAL(11,2),
       imp_rcv_fiscal          DECIMAL(11,2),
       fecha_sar_fiscal        CHAR(8),      
       monto_sar_fiscal        DECIMAL(11,2),
       imp_sar_fiscal          DECIMAL(11,2),
       fecha_sar_issste_fiscal CHAR(8),   
       monto_sar_issste_fiscal DECIMAL(11,2),
       imp_sar_issste_fiscal   DECIMAL(11,2),
       fecha_vol_fiscal        CHAR(8),   
       monto_vol_fiscal        DECIMAL(11,2),
       imp_vol_fiscal          DECIMAL(11,2),
       fecha_com_fiscal        CHAR(8),     
       monto_com_fiscal        DECIMAL(11,2),
       imp_com_fiscal          DECIMAL(11,2),
       interes_real            DECIMAL(11,2),
       interes_nominal         DECIMAL(11,2),
       fondo_retiro_1          DECIMAL(11,2),
       fondo_voluntaria_1      DECIMAL(11,2),
       fondo_retiro_2          DECIMAL(11,2),
       fondo_voluntaria_2      DECIMAL(11,2),
       fondo_adicional_vol     DECIMAL(11,2),
       folio                   INTEGER,
       fecha_emision           CHAR(8),   
       fecha_de_solicitud      CHAR(8),   
       fecha_de_nacimiento     CHAR(8),   
       nacionalidad            CHAR(50),
       telefono                CHAR(15),
       nombre                  CHAR(40),
       desc_afo_recep          CHAR(40),
       fecha_reingreso         CHAR(8)   
     )

END FUNCTION
##############################################
FUNCTION lectura()

   DEFINE x_mes         INTEGER,
          x_ano         INTEGER

   LET hoy = TODAY
   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.nombre_archivo
      AFTER FIELD nombre_archivo
         IF g_reg.nombre_archivo IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD nombre_archivo
         END IF

         EXIT INPUT
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO DE LECTURA CANCELADO ..."
      SLEEP 2
      ERROR " "
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar la lectura del archivo [S/N]? ..." 
   FOR opc

   IF opc MATCHES '[Ss]' THEN
      ERROR "PROCESANDO INFORMACION ..."

      LET ejecuta = "cd ",g_parametro.ruta_envio CLIPPED,
                    "; ls > archivos_cta" CLIPPED
      RUN ejecuta

      WHENEVER ERROR CONTINUE
         DROP TABLE archivos_cta
      WHENEVER ERROR STOP

      CREATE TEMP TABLE archivos_cta
         (campo  CHAR(100))

      LET ejecuta = g_parametro.ruta_envio CLIPPED,"/archivos_cta" CLIPPED
      LOAD FROM ejecuta INSERT INTO archivos_cta

      SELECT "X"
      FROM   archivos_cta
      WHERE  campo = g_reg.nombre_archivo

      IF SQLCA.SQLCODE <> 0 THEN
         PROMPT "NOMBRE DE ARCHIVO INCORRECTO" FOR opc
         CLEAR FORM
         CLEAR SCREEN

         LET ejecuta = "cd ",g_parametro.ruta_envio CLIPPED,"; rm archivos_cta"
         RUN ejecuta
         RETURN
      END IF
      CALL ejecuta_lectura()
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
##############################################
FUNCTION ejecuta_lectura()

   LET generar = g_reg.nombre_archivo

   CALL separa_archivo()      --separa archivo detalle "02"
   CALL crea_temporal()       --crea tabla temporal tmp_emision_edo_cta
   CALL sube_det_cta()        --sube datos del archivo a la tabla creada
   CALL despliega_nss()       --despliega en pantalla los datos de la tabla
{
   LET ejecuta = "cd ",g_parametro.ruta_rescate CLIPPED,
                 "; rm cta_det cta_det1"
   RUN ejecuta  
   LET ejecuta = "cd ",g_parametro.ruta_envio CLIPPED,
                 "; rm archivos_cta_det"
   RUN ejecuta
}

END FUNCTION
##############################################
FUNCTION sube_det_cta()

   LET ejecuta = "cd ",g_parametro.ruta_exp CLIPPED,
                 "/;DBDATE=dmy4;export DBDATE;dbload -d safre_tmp -c sube_cta_det -l /tmp/",usuario CLIPPED,".dbload_cta.log -e 1 -k;"
   RUN ejecuta

END FUNCTION
##############################################
FUNCTION separa_archivo()
   
   LET ejecuta = "sed -e '/^02/!d' ",g_parametro.ruta_envio CLIPPED,"/",
               generar CLIPPED," >",g_parametro.ruta_rescate CLIPPED,"/cta_det1"
   RUN ejecuta

   LET ejecuta = "head -n100 ",g_parametro.ruta_rescate CLIPPED,
                 "/cta_det1 >",g_parametro.ruta_rescate CLIPPED,"/cta_det"
   RUN ejecuta
   
END FUNCTION
##############################################
FUNCTION despliega_nss()

   DEFINE pos         SMALLINT

   DEFINE l_record1   ARRAY[200] OF RECORD
          nss                CHAR(11),
          periodo_desde      DATE,    
          periodo_hasta      DATE,    
          retiro97_ini       DECIMAL(11,2),
          retiro97_netas     DECIMAL(11,2),
          retiro97_retiros   DECIMAL(11,2),
          retiro97_fin       DECIMAL(11,2),
          cv_ini             DECIMAL(11,2),
          cv_netas           DECIMAL(11,2),
          cv_retiros         DECIMAL(11,2),
          cv_fin             DECIMAL(11,2),
          cs_ini             DECIMAL(11,2),
          cs_netas           DECIMAL(11,2),
          cs_retiros         DECIMAL(11,2),
          cs_fin             DECIMAL(11,2),
          sar_imss_ini       DECIMAL(11,2),
          sar_imss_netas     DECIMAL(11,2),
          sar_imss_retiros   DECIMAL(11,2),
          sar_imss_fin       DECIMAL(11,2),
          sar_issste_ini     DECIMAL(11,2),
          sar_issste_netas   DECIMAL(11,2),
          sar_issste_retiros DECIMAL(11,2),
          sar_issste_fin     DECIMAL(11,2),
          sub_ini_rcv        DECIMAL(11,2),
          sub_netas_rcv      DECIMAL(11,2),
          sub_retiros_rcv    DECIMAL(11,2),
          sub_fin_rcv        DECIMAL(11,2),
          viv_97_imss_ini    DECIMAL(11,2),
          viv_97_imss_netas  DECIMAL(11,2),
          viv_97_imss_retiros   DECIMAL(11,2),
          viv_97_imss_fin       DECIMAL(11,2),
          viv_92_imss_ini       DECIMAL(11,2),
          viv_92_imss_netas     DECIMAL(11,2),
          viv_92_imss_retiros   DECIMAL(11,2),
          viv_92_imss_fin       DECIMAL(11,2),
          viv_92_issste_ini     DECIMAL(11,2),
          viv_92_issste_netas   DECIMAL(11,2),
          viv_92_issste_retiros DECIMAL(11,2),
          viv_92_issste_fin     DECIMAL(11,2),
          sub_viv_ini           DECIMAL(11,2),
          sub_viv_netas         DECIMAL(11,2),
          sub_viv_retiros       DECIMAL(11,2),
          sub_viv_fin           DECIMAL(11,2),
          vol_ini               DECIMAL(11,2),
          vol_netas             DECIMAL(11,2),
          vol_retiros           DECIMAL(11,2),
          vol_fin               DECIMAL(11,2),
          com_ini               DECIMAL(11,2),
          com_netas             DECIMAL(11,2),
          com_retiros           DECIMAL(11,2),
          com_fin               DECIMAL(11,2),
          sub_vcom_ini          DECIMAL(11,2),
          sub_vcom_netas        DECIMAL(11,2), 
          sub_vcom_retiros      DECIMAL(11,2),
          sub_vcom_fin          DECIMAL(11,2),
          total_general_ini     DECIMAL(11,2),
          total_general_netas   DECIMAL(11,2),
          total_general_retiros DECIMAL(11,2),
          total_general_fin     DECIMAL(11,2)
   END RECORD
          
   OPEN WINDOW ventana2 AT 2,2 WITH FORM "CTAB0072" ATTRIBUTE(BORDER)
   DISPLAY " CTAB007           PANTALLA DE CONSULTA DE ESTADO DE CUENTA                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "CONTROL : [G]Aportaciones  [F]Fiscal   [I]Ahorro  [V]Rendimiento  [T]Resumen   " AT 2,1
   DISPLAY "                            ESTADO DE CUENTA INDIVIDUAL                        " AT 5,1 ATTRIBUTE(REVERSE) 
   DISPLAY "TOTAL DE TU AHORRO" AT 21,2

   DECLARE cursor_1 CURSOR FOR
   SELECT nss               , 
          periodo_desde     , 
          periodo_hasta     , 
          retiro97_ini      , 
          retiro97_netas    , 
          retiro97_retiros  , 
          retiro97_fin      , 
          cv_ini            , 
          cv_netas          , 
          cv_retiros        , 
          cv_fin            , 
          cs_ini            , 
          cs_netas          , 
          cs_retiros        , 
          cs_fin            , 
          sar_imss_ini      , 
          sar_imss_netas    , 
          sar_imss_retiros  , 
          sar_imss_fin      , 
          sar_issste_ini    , 
          sar_issste_netas  , 
          sar_issste_retiros, 
          sar_issste_fin    , 
          sub_ini_rcv       , 
          sub_netas_rcv     , 
          sub_retiros_rcv   , 
          sub_fin_rcv       ,  
          viv_97_imss_ini   , 
          viv_97_imss_netas , 
          viv_97_imss_retiros  , 
          viv_97_imss_fin      , 
          viv_92_imss_ini      , 
          viv_92_imss_netas    , 
          viv_92_imss_retiros  , 
          viv_92_imss_fin      , 
          viv_92_issste_ini    , 
          viv_92_issste_netas  , 
          viv_92_issste_retiros, 
          viv_92_issste_fin    , 
          sub_viv_ini          , 
          sub_viv_netas        , 
          sub_viv_retiros      ,  
          sub_viv_fin          ,  
          vol_ini              , 
          vol_netas            ,    
          vol_retiros          , 
          vol_fin              ,
          com_ini              ,
          com_netas            ,
          com_retiros          ,
          com_fin              ,
          sub_vcom_ini         ,
          sub_vcom_netas       ,
          sub_vcom_retiros     ,
          sub_vcom_fin         ,
          total_general_ini    ,
          total_general_netas  ,
          total_general_retiros,
          total_general_fin    
   FROM   tmp_emision_edo_cta

   LET pos = 1

   FOREACH cursor_1 INTO l_record1[pos].*
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_record1 TO scr_1.*

         ON KEY(CONTROL-G)
            LET pos = ARR_CURR()
            LET n_seguro = l_record1[pos].nss
            CALL aportaciones()
         ON KEY(CONTROL-F)
            LET pos = ARR_CURR()
            LET n_seguro = l_record1[pos].nss
            CALL retiros()
         ON KEY(CONTROL-I)
            LET pos = ARR_CURR()
            LET n_seguro = l_record1[pos].nss
            CALL ahorro()
         ON KEY(CONTROL-V)
            LET pos = ARR_CURR()
            LET n_seguro = l_record1[pos].nss
            CALL rendimiento()
         ON KEY(CONTROL-T)
            LET pos = ARR_CURR()
            LET n_seguro = l_record1[pos].nss
            CALL resumen_comisiones()
         ON KEY(CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
      CLEAR FORM
      CLOSE WINDOW ventana2
   ELSE
      ERROR "NO SE ENCONTRARON RESGISTROS PARA MOSTRAR ..."
      SLEEP 2
      ERROR ""
      CLEAR FORM
      CLOSE WINDOW ventana2
   END IF

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE tmp_emision_edo_cta
   WHENEVER ERROR STOP

END FUNCTION
##############################################
FUNCTION aportaciones()

   DEFINE pos    SMALLINT

   DEFINE l_record2 ARRAY[200] OF RECORD
          periodo_pago_1        CHAR(6),
          periodo_pago_2        CHAR(6),
          periodo_pago_3        CHAR(6),
          dias_cotizados_1      SMALLINT,
          dias_cotizados_2      SMALLINT,
          dias_cotizados_3      SMALLINT,
          sbc_1                 DECIMAL(11,2),
          sbc_2                 DECIMAL(11,2),
          sbc_3                 DECIMAL(11,2),
          reg_patron_1          CHAR(11),
          reg_patron_2          CHAR(11),
          reg_patron_3          CHAR(11)
   END RECORD

   OPEN WINDOW ventana3 AT 8,15 WITH FORM "CTAB0073" ATTRIBUTE(BORDER)
   DISPLAY "<Ctrl-C> Salir" AT 3,19
   DISPLAY "        Datos de referencia de las ultimas            "
   AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "       aportaciones recibidas en el periodo           "
   AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY "    Periodo    Dias     Salario base    Registro      "
   AT 4,1 ATTRIBUTE(REVERSE)
   DISPLAY "    de pago  cotizados  de cotizacion   patronal      "
   AT 5,1 ATTRIBUTE(REVERSE)

   DECLARE cursor_2 CURSOR FOR
   SELECT periodo_pago_1,
          periodo_pago_2,
          periodo_pago_3,
          dias_cotizados_1,
          dias_cotizados_2,
          dias_cotizados_3,
          sbc_1,
          sbc_2,
          sbc_3,
          reg_patron_1,
          reg_patron_2,
          reg_patron_3
   FROM   tmp_emision_edo_cta
   WHERE  nss = n_seguro

   LET pos = 1

   FOREACH cursor_2 INTO l_record2[pos].*
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_record2 TO scr_2.*
         ON KEY(CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR ... "
      SLEEP 2
      ERROR ""
   END IF 
   
   CLOSE WINDOW ventana3

END FUNCTION
##############################################
FUNCTION ahorro()

   DEFINE pos    SMALLINT

   DEFINE l_record3 ARRAY[200] OF RECORD
          fondo_retiro_1       DECIMAL(11,2),
          fondo_voluntaria_1   DECIMAL(11,2),
          fondo_retiro_2       DECIMAL(11,2),
          fondo_voluntaria_2   DECIMAL(11,2),
          fondo_adicional_vol  DECIMAL(11,2)
   END RECORD

   OPEN WINDOW ventana4 AT 6,19 WITH FORM "CTAB0074" ATTRIBUTE(BORDER)
   DISPLAY "<Ctrl-C> Salir" AT 3,15
   DISPLAY "     Tu Ahorro invertido en tu AFORE     " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY " Concepto     Ahorro          Importe    " AT 4,1 ATTRIBUTE(REVERSE)

   DECLARE cursor_3 CURSOR FOR
   SELECT fondo_retiro_1,
          fondo_voluntaria_1,
          fondo_retiro_2,
          fondo_voluntaria_2,
          fondo_adicional_vol
   FROM   tmp_emision_edo_cta
   WHERE  nss = n_seguro

   LET pos = 1

   FOREACH cursor_3 INTO l_record3[pos].*
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_record3 TO scr_3.*
         ON KEY(CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR ... "
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana4

END FUNCTION
##############################################
FUNCTION rendimiento()

   DEFINE pos    SMALLINT

   DEFINE l_record4 ARRAY[200] OF RECORD
          rendimiento_siefore1    DECIMAL(6,2),
          rendimiento_siefore2    DECIMAL(6,2), 
          rendimiento_adicional   DECIMAL(6,2),
          rendimiento_infonavit   DECIMAL(6,2),
          rendimiento_fovissste   DECIMAL(6,2)
   END RECORD

   OPEN WINDOW ventana5 AT 8,23 WITH FORM "CTAB0075" ATTRIBUTE(BORDER)
   DISPLAY "      Rendimiento por Fondo       " AT 1,1 ATTRIBUTE(REVERSE) 
   DISPLAY "<Ctrl-C> Salir" AT 2,10

   DECLARE cursor_4 CURSOR FOR
   SELECT rendimiento_siefore1,
          rendimiento_siefore2,
          rendimiento_adicional,
          rendimiento_infonavit,
          rendimiento_fovissste
   FROM   tmp_emision_edo_cta
   WHERE  nss = n_seguro

   LET pos = 1

   FOREACH cursor_4 INTO l_record4[pos].*
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_record4 TO scr_4.*
         ON KEY(CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR ... "
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana5

END FUNCTION
##############################################
FUNCTION retiros()

   DEFINE pos    SMALLINT

   DEFINE l_record5 ARRAY[200] OF RECORD
          fecha_rcv_fiscal         CHAR(8),
          monto_rcv_fiscal         DECIMAL(11,2),
          imp_rcv_fiscal           DECIMAL(11,2),
          fecha_sar_fiscal         CHAR(8),
          monto_sar_fiscal         DECIMAL(11,2),
          imp_sar_fiscal           DECIMAL(11,2),
          fecha_sar_issste_fiscal  CHAR(8),
          monto_sar_issste_fiscal  DECIMAL(11,2),
          imp_sar_issste_fiscal    DECIMAL(11,2),
          fecha_vol_fiscal         CHAR(8),   
          monto_vol_fiscal         DECIMAL(11,2),
          imp_vol_fiscal           DECIMAL(11,2),
          fecha_com_fiscal         CHAR(8),   
          monto_com_fiscal         DECIMAL(11,2),
          imp_com_fiscal           DECIMAL(11,2),
          interes_real             DECIMAL(11,2),
          interes_nominal          DECIMAL(11,2)
   END RECORD

   OPEN WINDOW ventana6 AT 6,11 WITH FORM "CTAB0076" ATTRIBUTE(BORDER)
   DISPLAY "<Ctrl-C> Salir" AT 3,24
   DISPLAY "   Concepto               Fecha      Monto     Imp. retenido  "
   AT 4,1 ATTRIBUTE(REVERSE)
   DISPLAY "             Informacion sobre tus retiros que                " 
   AT 1,1 ATTRIBUTE(REVERSE) 
   DISPLAY "          puede ser util para tu declaracion fiscal           "
   AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY "Intereses percibidos durante el periodo conforme la ley ISR   " 
   AT 10,1 ATTRIBUTE(REVERSE)

   DECLARE cursor_5 CURSOR FOR
   SELECT fecha_rcv_fiscal,
          monto_rcv_fiscal,
          imp_rcv_fiscal, 
          fecha_sar_fiscal,   
          monto_sar_fiscal,  
          imp_sar_fiscal,   
          fecha_sar_issste_fiscal, 
          monto_sar_issste_fiscal,
          imp_sar_issste_fiscal, 
          fecha_vol_fiscal,     
          monto_vol_fiscal,    
          imp_vol_fiscal,     
          fecha_com_fiscal,  
          monto_com_fiscal, 
          imp_com_fiscal,  
          interes_real,   
          interes_nominal
   FROM   tmp_emision_edo_cta
   WHERE  nss = n_seguro

   LET pos = 1

   FOREACH cursor_5 INTO l_record5[pos].*
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_record5 TO scr_5.*
         ON KEY(CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR ... "
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana6

END FUNCTION
##############################################
FUNCTION resumen_comisiones()

   DEFINE pos    SMALLINT

   DEFINE l_record6 ARRAY[200] OF RECORD
          estructura_flujo1     DECIMAL(6,2),
          comision_flujo1       DECIMAL(11,2),
          estructura_saldo      DECIMAL(11,2),
          comision_saldo        DECIMAL(11,2),
          total_comisiones      DECIMAL(11,2),
          estructura_adicional  DECIMAL(11,2),
          comision_adicional    DECIMAL(11,2)
   END RECORD

   OPEN WINDOW ventana7 AT 8,20 WITH FORM "CTAB0077" ATTRIBUTE(BORDER)
   DISPLAY "<Ctrl-C> Salir" AT 3,15 
   DISPLAY "Resumen de comisiones cobradas en el periodo " AT 2,1
   ATTRIBUTE(REVERSE)
   DISPLAY " Concepto             Comision     Importe   " AT 4,1 
   ATTRIBUTE(REVERSE)

   DECLARE cursor_6 CURSOR FOR
   SELECT estructura_flujo1,
          comision_flujo1,
          estructura_saldo,
          comision_saldo,
          total_comisiones,
          estructura_adicional,
          comision_adicional
   FROM   tmp_emision_edo_cta
   WHERE  nss = n_seguro

   LET pos = 1

   FOREACH cursor_6 INTO l_record6[pos].*
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_record6 TO scr_6.*
         ON KEY(CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO SE ENCONTRARON REGISTROS PARA MOSTRAR ... "
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana7

END FUNCTION
##############################################

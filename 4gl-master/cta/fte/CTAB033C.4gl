#********************************************************************#
#Proyecto     => safre_af                                            #
#Propietario  => E.F.P.                                              #
#Programa     => CTAB033C                                            #
#Descripcion  => CORTE TRANSVERSAL POR SUBCTA, SIEFORE               #
#Fecha        => 26 de septiembre de 2007.                           #
#Por          => CÉSAR DAVID CHÁVEZ MARTÍNEZ                         #
#Sistema      => CTA.                                                #
#********************************************************************#

DATABASE safre_af
################################################################################
GLOBALS
   DEFINE gar_siefore ARRAY [6] OF RECORD
   	                  cod_siefore   SMALLINT,
                      desc_siefore  CHAR (21),
                      sie_acciones  DECIMAL(18,2),
                      sie_precio    DECIMAL(7,6),
                      sie_pesos     DECIMAL(18,2),
                      x             CHAR(1)
                  END RECORD

   DEFINE gar_subcta ARRAY [11] OF RECORD
                      cod_subcta      SMALLINT,
                      desc_subcta     CHAR(22),
                      subcta_acciones DECIMAL(18,2),
                      subcta_pesos    DECIMAL(18,2)
                  END RECORD

   DEFINE gr_total RECORD
   	                  tot_nss     INTEGER,
                      fecha_corte DATE
                   END RECORD

   DEFINE arr_c,
          arr_l,
          arr_t      SMALLINT,
          USER       CHAR(08),
          hoy        DATE,
          pos        SMALLINT,
          pos_subcta SMALLINT

END GLOBALS
################################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   SELECT usuario
   INTO   USER
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET hoy = TODAY
   CALL Inicializa()
   CALL proceso()
END MAIN
################################################################################
FUNCTION proceso()
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAB0331" ATTRIBUTE(BORDER)

   MENU "CORTE TRANSVERSAL:"
      COMMAND "Consulta" "Consulta de saldos para corte transversal"
         CALL consulta()
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
################################################################################
FUNCTION inicializa()
   DEFINE li_cont  SMALLINT,
          lc_query CHAR (200)

   FOR li_cont = 1 TO 6
   	  INITIALIZE gar_siefore[li_cont].* TO NULL
   END FOR

   FOR li_cont = 1 TO 11
   	  INITIALIZE gar_subcta[li_cont].* TO NULL
   END FOR

   INITIALIZE gr_total.* TO NULL
END FUNCTION
################################################################################
FUNCTION consulta()

-- TOTAL DE NSS Y FECHA DE CORTE
   {SELECT COUNT(*), a.fecha_corte
   INTO   gr_total.*
   FROM   safre_tmp:cta_formato_nss_1 a
   GROUP  BY 2}
   
   SELECT COUNT(*)
   INTO   gr_total.tot_nss
   FROM   safre_tmp:tmp_saldo_corte_1 a
   
   LET gr_total.fecha_corte = "8/31/2007"--MDY(08,31,2007)

   DISPLAY BY NAME gr_total.*

--TOTAL POR SIEFORE
   DECLARE cur_siefore CURSOR FOR
      SELECT a.siefore,
             b.razon_social,
             SUM(a.monto_en_acciones),
             SUM(a.monto_en_pesos)
      FROM   safre_tmp:tmp_saldo_corte_1 a,
             safre_af:tab_siefore_local b
      WHERE  a.siefore = b.codigo_siefore
      AND    a.siefore NOT IN (0,11)
      GROUP BY 1,2
      ORDER BY 1

   LET pos = 1

   ERROR "PROCESANDO INFORMACION..."
   FOREACH cur_siefore INTO gar_siefore[pos].cod_siefore,
   	                        gar_siefore[pos].desc_siefore,
   	                        gar_siefore[pos].sie_acciones,
   	                        gar_siefore[pos].sie_pesos

   	  SELECT a.precio_del_dia
      INTO   gar_siefore[pos].sie_precio
      FROM   glo_valor_accion a
      WHERE  a.codigo_siefore  = gar_siefore[pos].cod_siefore
      AND    a.fecha_valuacion = "08/31/2007"

   	  LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   ERROR ""
   IF pos >= 1 THEN
      CALL SET_COUNT(pos)
         DISPLAY    "Siefores                      Títulos   Precio Vig.       Importe en Pesos" AT 5,1 ATTRIBUTE(REVERSE)
         DISPLAY    "<CTRL-F> GENERAR ARCHIVO" AT 2,51 ATTRIBUTE(REVERSE)
      DISPLAY ARRAY gar_siefore TO scr_sie.*

         ON KEY(CONTROL-M)
         	  LET arr_c = ARR_CURR()
            LET arr_l = SCR_LINE()
            CALL consulta_detalle()

         ON KEY(CONTROL-F)
            CALL reporte(gr_total.tot_nss, gr_total.fecha_corte)
            CLEAR FORM
            DISPLAY "                                                                          " AT 5,1
            DISPLAY "                                                                          " AT 11,1
            DISPLAY "                        " AT 2,51
            EXIT DISPLAY

         ON KEY(INTERRUPT)
            CLEAR FORM
            DISPLAY "                                                                          " AT 5,1
            DISPLAY "                                                                          " AT 11,1
            DISPLAY "                        " AT 2,51
            EXIT DISPLAY
      END DISPLAY

   ELSE
      ERROR "REGISTRO NO ENCONTRADO"
      SLEEP 1
      ERROR ""
      CLEAR SCREEN
      RETURN
   END IF

END FUNCTION
################################################################################
FUNCTION consulta_detalle()
   DEFINE li_pos,
          li_cont SMALLINT

   FOR li_cont = 1 TO 11
   	  INITIALIZE gar_subcta[li_cont].* TO NULL
   END FOR

   DECLARE cur_subcta CURSOR FOR
      SELECT b.subct_prc,
             SUM(a.monto_en_acciones),
             SUM(a.monto_en_pesos)
      FROM   safre_tmp:tmp_saldo_corte_1 a,
             safre_af:tab_subcuenta b
      WHERE  a.subcuenta IN (1,2,3,5,6,7,9,10,11,12,13,15,16)
      AND    a.subcuenta = b.subct_cod
      AND    a.siefore   IN (1,2)  ---= gar_siefore[arr_c].cod_siefore
      GROUP BY 1
      ORDER BY 1

   LET pos_subcta = 1
   ERROR "PROCESANDO DETALLE..."

   FOREACH cur_subcta INTO  gar_subcta[pos_subcta].cod_subcta,
   	                        gar_subcta[pos_subcta].subcta_acciones,
   	                        gar_subcta[pos_subcta].subcta_pesos

   	  CASE gar_subcta[pos_subcta].cod_subcta
   	  	 WHEN  1 LET gar_subcta[pos_subcta].desc_subcta = "Retiro 97"
   	  	 WHEN  2 LET gar_subcta[pos_subcta].desc_subcta = "Cesantía y Vejez"
   	  	 WHEN  3 LET gar_subcta[pos_subcta].desc_subcta = "Cuota Social y Especial"
   	  	 WHEN  5 LET gar_subcta[pos_subcta].desc_subcta = "Aportaciones Voluntarias Ventanilla"
   	  	 WHEN  8 LET gar_subcta[pos_subcta].desc_subcta = "Retiro 92 IMSS"
   	  	 WHEN 13 LET gar_subcta[pos_subcta].desc_subcta = "Aportaciones Voluntarias Patronales"
   	  	 WHEN 15 LET gar_subcta[pos_subcta].desc_subcta = "Aportaciones Complementarias de Retiro Patronales"
   	  	 WHEN 17 LET gar_subcta[pos_subcta].desc_subcta = "Aportaciones Complementarias de Retiro Ventanilla"
   	  	 WHEN 18 LET gar_subcta[pos_subcta].desc_subcta = "Aportaciones a Largo Plazo"
   	  	 WHEN 22 LET gar_subcta[pos_subcta].desc_subcta = "Ahorro para el retiro ISSSTE SIEFORE"
   	  END CASE

   	  LET pos_subcta = pos_subcta + 1
   END FOREACH

   LET pos_subcta = pos_subcta - 1

   ERROR ""

   LET pos_subcta = pos_subcta - 1

   IF pos_subcta >= 1 THEN
   	  DISPLAY    "SubCta                        Títulos                     Importe en Pesos" AT 11,1 ATTRIBUTE(REVERSE)
      FOR li_cont = 1 TO pos_subcta
         DISPLAY gar_subcta[li_cont].* TO scr_cta[li_cont].*
      END FOR
   END IF

END FUNCTION
################################################################################
FUNCTION reporte(li_totnss, ld_fecha_corte)

   DEFINE li_totnss      INTEGER,
          ld_fecha_corte DATE

   DEFINE li_folio_proceso INTEGER

   DEFINE hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          x_tipo_proceso     SMALLINT,
          lc_ejecuta         CHAR(100),
          lc_msg             CHAR(400),
          lc_ok              CHAR(1)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   --FOLIO CORRESPONDIENTE
   SELECT "X"
   FROM cta_folio

   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   li_folio_proceso
      FROM   cta_folio

      UPDATE cta_folio
      SET    folio = li_folio_proceso
   ELSE
      LET li_folio_proceso = 1

      INSERT INTO cta_folio
      VALUES (li_folio_proceso)
   END IF

   --INGRESO A CONTROL-PROCESO
   INSERT INTO dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           "CTAB034C",               -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           "corte transv",          -- parametro1
           ld_fecha_corte,          -- parametro2 --fecha_corte
           li_totnss,               -- parametro3 --tot_nss
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           li_folio_proceso,        -- folio
           NULL,                    -- resultado
           USER,                    -- usuario
           0                        -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   --ENVIO DEL REPORTE POR NOHUP
   ERROR "Ejecutando Reporte por nohup ..."

   LET lc_ejecuta = "nohup time fglgo CTAB034C.4gi " CLIPPED,
                     li_folio_proceso CLIPPED, " ",
                     ld_fecha_corte   CLIPPED, " ",
                     li_totnss        CLIPPED," &"

   RUN lc_ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup ..."

   LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTAB034C"
   PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)
   ERROR ""

END FUNCTION
################################################################################

################################################################################
--/safre/cta/fte/multi_sie
--proceo_cod nomnbre prog
--folio
{
cta_folio
}
################################################################################
#Proyecto            => SAFRE  ( MEXICO )                                      #
#Owner               => E.F.P.                                                 #
#Programa CTAB034C   => ESTADO DE CUENTA                                       #
#Sistema             => CTA                                                    #
#Por                 => CÉSAR DAVID CHÁVEZ MARTÍNEZ                            #
#Fecha               => 26 de Septiembre de 2007                               #
################################################################################

DATABASE safre_af
################################################################################
GLOBALS
  DEFINE  gi_folio,
          gi_registros,
          gi_tot_nss       INTEGER,
          gd_fecha_corte   DATE
  
  DEFINE gr_reporte RECORD
            precio1   DECIMAL(7,6),
            precio2   DECIMAL(7,6)
   END RECORD        

END GLOBALS
################################################################################
MAIN
   LET gi_folio       = ARG_VAL(1)
   LET gd_fecha_corte = ARG_VAL(2)
   LET gi_tot_nss     = ARG_VAL(3)

   DISPLAY "INICIA GENERACIÓN DE ARCHIVO CORTE TRANSV",
           " CON FOLIO:",gi_folio

   CALL STARTLOG("CTAB034C.log")

   CALL Ingresa_etapa(gi_folio,2,"Inicia generación de archivo de corte trnv")
   LET gi_registros = 0
   CALL archivo()
   DISPLAY "ARCHIVO GENERADO:",gi_registros
END MAIN
################################################################################
FUNCTION Ingresa_etapa(li_folio,li_etapa_cod,lc_resultado)
   DEFINE li_folio      INTEGER,
          li_etapa_cod  DECIMAL(2,0),
          lc_resultado  CHAR(50)

   DEFINE hora_inicial       CHAR(08),
          hora_final         CHAR(08)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso
   VALUES
      (TODAY,             -- fecha_proceso
       "CTAB034C",         -- proceso_cod
       li_etapa_cod,      -- etapa_cod
       hora_inicial,      -- hora_inicial
       hora_final,        -- hora_final
       "corte transv",    -- parametro1
       gd_fecha_corte,    -- parametro2  fecha_corte
       gi_tot_nss,        -- parametro3  tot_nss
       NULL,              -- parametro4
       NULL,              -- parametro5
       li_folio,          -- folio
       lc_resultado,      -- resultado
       USER,              -- usuario
       0                  -- consecutivo
      )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso etapa ",
            li_etapa_cod," ",STATUS
      EXIT PROGRAM
   END IF
END FUNCTION
################################################################################
FUNCTION archivo()
   DEFINE lc_nomarch CHAR(80),
          lc_nss     CHAR(11)

   DEFINE lr_reporte   RECORD
             nss       CHAR(11),
             curp      CHAR(18),
             siefore   SMALLINT,
             subcta    SMALLINT,
             acciones1 DECIMAL(12,2),
             precio1   DECIMAL(7,6),
             pesos1    DECIMAL(18,2),
             acciones2 DECIMAL(12,2),
             precio2   DECIMAL(7,6),
             pesos2    DECIMAL(18,2)
          END RECORD

   DEFINE lr_monto_nss  RECORD
             nss        CHAR(11),
             siefore    SMALLINT,
             subcta     SMALLINT,
             acciones   DECIMAL(12,2),
             pesos      DECIMAL(18,2)
          END RECORD

   DEFINE li_tipo_solicitud SMALLINT

   DEFINE li_cont       SMALLINT

   LET li_cont = 1
   LET lc_nomarch = "/safre_prc/cta/envio/corte_trnv_sie_31082007_cta"

   START REPORT transversal TO lc_nomarch
   
   SELECT a.precio_del_dia    --PRECIO DE ACCION SIEFORE 1
   INTO   gr_reporte.precio1
   FROM   glo_valor_accion a
   WHERE  a.codigo_siefore  = 1
   AND    a.fecha_valuacion = "08/31/2007"

   SELECT a.precio_del_dia   --PRECIO DE ACCION SIEFORE 2
   INTO   gr_reporte.precio2
   FROM   glo_valor_accion a
   WHERE  a.codigo_siefore  = 2
   AND    a.fecha_valuacion = "08/31/2007"

   SELECT a.nss
   FROM   safre_af:dis_cuenta a
   WHERE  a.fecha_conversion < "09/01/2007"
   GROUP BY 1
   ORDER BY 1
   INTO temp tmp_nss_corte_1

   CREATE INDEX tmp_nss_corte_1_1 ON tmp_nss_corte_1(nss)
   UPDATE STATISTICS FOR TABLE tmp_nss_corte_1

   DECLARE cur_subcta CURSOR FOR
   SELECT a.nss,
          b.subct_cns,
          a.siefore,
          SUM(a.monto_en_acciones),
          SUM(a.monto_en_pesos)
   FROM   safre_af:tab_subcuenta b,
          safre_af:dis_cuenta a
   WHERE  a.nss IN ( SELECT nss FROM tmp_nss_corte_1)
   AND    a.subcuenta IN (1,2,3,5,6,7,9,10,11,12,13,15,16)
   AND    a.subcuenta = b.subct_cod
   AND    a.fecha_conversion < "09/01/2007"
   AND    a.siefore IN (1,2)
   GROUP BY 1,2,3
   ORDER BY 1,2,3

   FOREACH cur_subcta INTO lr_monto_nss.nss,
                           lr_monto_nss.subcta,
                           lr_monto_nss.siefore,
                           lr_monto_nss.acciones,
                           lr_monto_nss.pesos

      IF lr_monto_nss.siefore = 1 THEN
         LET lr_monto_nss.pesos = lr_monto_nss.acciones * gr_reporte.precio1
      END IF
      IF lr_monto_nss.siefore = 2 THEN
         LET lr_monto_nss.pesos = lr_monto_nss.acciones * gr_reporte.precio2
      END IF

      OUTPUT TO REPORT transversal(lr_monto_nss.*)
   END FOREACH

   CALL Actualiza_etapa(gi_folio,2,gi_registros,"Termina generacion de archivo")

   FINISH REPORT transversal

   CALL Actualiza_etapa(gi_folio,1,gi_registros,lc_nomarch)

END FUNCTION
################################################################################
REPORT transversal(lr_monto_nss)

   DEFINE lr_monto_nss  RECORD
             nss        CHAR(11),
             siefore    SMALLINT,
             subcta     SMALLINT,
             acciones   DECIMAL(12,2),
             pesos      DECIMAL(18,2)
          END RECORD

   DEFINE lr_reporte RECORD
            precio1   DECIMAL(7,6),
            precio2   DECIMAL(7,6),
            curp      CHAR(18)
   END RECORD

   DEFINE lr_reporte_01 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_02 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_03 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_05 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_08 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_13 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_15 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_17 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_18 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   DEFINE lr_reporte_22 RECORD
            acciones1 DECIMAL(12,2),
            pesos1    DECIMAL(18,2),
            acciones2 DECIMAL(12,2),
            pesos2    DECIMAL(18,2),
            subcta    SMALLINT
   END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   ORDER EXTERNAL BY lr_monto_nss.nss,
                     lr_monto_nss.siefore,
                     lr_monto_nss.subcta

   FORMAT

   BEFORE GROUP OF lr_monto_nss.nss
      LET lr_reporte_01.acciones1 = 0
      LET lr_reporte_01.pesos1    = 0
      LET lr_reporte_01.acciones2 = 0
      LET lr_reporte_01.pesos2    = 0
      LET lr_reporte_01.subcta    = 01

      LET lr_reporte_02.acciones1 = 0
      LET lr_reporte_02.pesos1    = 0
      LET lr_reporte_02.acciones2 = 0
      LET lr_reporte_02.pesos2    = 0
      LET lr_reporte_02.subcta    = 02

      LET lr_reporte_03.acciones1 = 0
      LET lr_reporte_03.pesos1    = 0
      LET lr_reporte_03.acciones2 = 0
      LET lr_reporte_03.pesos2    = 0
      LET lr_reporte_03.subcta    = 03

      LET lr_reporte_05.acciones1 = 0
      LET lr_reporte_05.pesos1    = 0
      LET lr_reporte_05.acciones2 = 0
      LET lr_reporte_05.pesos2    = 0
      LET lr_reporte_05.subcta    = 05

      LET lr_reporte_08.acciones1 = 0
      LET lr_reporte_08.pesos1    = 0
      LET lr_reporte_08.acciones2 = 0
      LET lr_reporte_08.pesos2    = 0
      LET lr_reporte_08.subcta    = 08

      LET lr_reporte_13.acciones1 = 0
      LET lr_reporte_13.pesos1    = 0
      LET lr_reporte_13.acciones2 = 0
      LET lr_reporte_13.pesos2    = 0
      LET lr_reporte_13.subcta    = 13

      LET lr_reporte_15.acciones1 = 0
      LET lr_reporte_15.pesos1    = 0
      LET lr_reporte_15.acciones2 = 0
      LET lr_reporte_15.pesos2    = 0
      LET lr_reporte_15.subcta    = 15

      LET lr_reporte_17.acciones1 = 0
      LET lr_reporte_17.pesos1    = 0
      LET lr_reporte_17.acciones2 = 0
      LET lr_reporte_17.pesos2    = 0
      LET lr_reporte_17.subcta    = 17

      LET lr_reporte_18.acciones1 = 0
      LET lr_reporte_18.pesos1    = 0
      LET lr_reporte_18.acciones2 = 0
      LET lr_reporte_18.pesos2    = 0
      LET lr_reporte_18.subcta    = 18

      LET lr_reporte_22.acciones1 = 0
      LET lr_reporte_22.pesos1    = 0
      LET lr_reporte_22.acciones2 = 0
      LET lr_reporte_22.pesos2    = 0
      LET lr_reporte_22.subcta    = 22

      SELECT b.n_unico
      INTO   lr_reporte.curp      --- CURP
      FROM   afi_mae_afiliado b
      WHERE  b.n_seguro       = lr_monto_nss.nss      

   ON EVERY ROW

      IF lr_monto_nss.siefore = 1 THEN
         CASE lr_monto_nss.subcta
            WHEN "01"
               LET lr_reporte_01.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_01.pesos1    = lr_monto_nss.pesos
            WHEN "02"
               LET lr_reporte_02.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_02.pesos1    = lr_monto_nss.pesos
            WHEN "03"
               LET lr_reporte_03.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_03.pesos1    = lr_monto_nss.pesos
            WHEN "05"
               LET lr_reporte_05.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_05.pesos1    = lr_monto_nss.pesos
            WHEN "08"
               LET lr_reporte_08.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_08.pesos1    = lr_monto_nss.pesos
            WHEN "13"
               LET lr_reporte_13.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_13.pesos1    = lr_monto_nss.pesos
            WHEN "15"
               LET lr_reporte_15.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_15.pesos1    = lr_monto_nss.pesos
            WHEN "17"
               LET lr_reporte_17.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_17.pesos1    = lr_monto_nss.pesos
            WHEN "18"
               LET lr_reporte_18.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_18.pesos1    = lr_monto_nss.pesos
            WHEN "22"
               LET lr_reporte_22.acciones1 = lr_monto_nss.acciones
               LET lr_reporte_22.pesos1    = lr_monto_nss.pesos
         END CASE
      ELSE
         CASE lr_monto_nss.subcta
            WHEN "01"
               LET lr_reporte_01.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_01.pesos2    = lr_monto_nss.pesos
            WHEN "02"
               LET lr_reporte_02.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_02.pesos2    = lr_monto_nss.pesos
            WHEN "03"
               LET lr_reporte_03.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_03.pesos2    = lr_monto_nss.pesos
            WHEN "05"
               LET lr_reporte_05.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_05.pesos2    = lr_monto_nss.pesos
            WHEN "08"
               LET lr_reporte_08.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_08.pesos2    = lr_monto_nss.pesos
            WHEN "13"
               LET lr_reporte_13.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_13.pesos2    = lr_monto_nss.pesos
            WHEN "15"
               LET lr_reporte_15.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_15.pesos2    = lr_monto_nss.pesos
            WHEN "17"
               LET lr_reporte_17.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_17.pesos2    = lr_monto_nss.pesos
            WHEN "18"
               LET lr_reporte_18.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_18.pesos2    = lr_monto_nss.pesos
            WHEN "22"
               LET lr_reporte_22.acciones2 = lr_monto_nss.acciones
               LET lr_reporte_22.pesos2    = lr_monto_nss.pesos
         END CASE
      END IF

   AFTER GROUP OF lr_monto_nss.nss
      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_01.subcta    USING "&&", ",",
                      lr_reporte_01.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_01.pesos1    USING "#################&.&&",",",
                      lr_reporte_01.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_01.pesos2    USING "#################&.&&"

      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_02.subcta    USING "&&", ",",
                      lr_reporte_02.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_02.pesos1    USING "#################&.&&",",",
                      lr_reporte_02.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_02.pesos2    USING "#################&.&&"

      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_03.subcta    USING "&&", ",",
                      lr_reporte_03.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_03.pesos1    USING "#################&.&&",",",
                      lr_reporte_03.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_03.pesos2    USING "#################&.&&"

      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_05.subcta    USING "&&", ",",
                      lr_reporte_05.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_05.pesos1    USING "#################&.&&",",",
                      lr_reporte_05.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_05.pesos2    USING "#################&.&&"

      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_08.subcta    USING "&&", ",",
                      lr_reporte_08.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_08.pesos1    USING "#################&.&&",",",
                      lr_reporte_08.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_08.pesos2    USING "#################&.&&"
      
      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_13.subcta    USING "&&", ",",
                      lr_reporte_13.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_13.pesos1    USING "#################&.&&",",",
                      lr_reporte_13.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_13.pesos2    USING "#################&.&&"

      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_15.subcta    USING "&&", ",",
                      lr_reporte_15.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_15.pesos1    USING "#################&.&&",",",
                      lr_reporte_15.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_15.pesos2    USING "#################&.&&"

      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_17.subcta    USING "&&", ",",
                      lr_reporte_17.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_17.pesos1    USING "#################&.&&",",",
                      lr_reporte_17.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_17.pesos2    USING "#################&.&&"

      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_18.subcta    USING "&&", ",",
                      lr_reporte_18.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_18.pesos1    USING "#################&.&&",",",
                      lr_reporte_18.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_18.pesos2    USING "#################&.&&"

      PRINT COLUMN 01,lr_monto_nss.nss        ,",",
                      lr_reporte.curp         ,",",
                      lr_reporte_22.subcta    USING "&&", ",",
                      lr_reporte_22.acciones1 USING "###########&.&&",      ",",
                      gr_reporte.precio1      USING "######&.&&&&&&",       ",",
                      lr_reporte_22.pesos1    USING "#################&.&&",",",
                      lr_reporte_22.acciones2 USING "###########&.&&",      ",",
                      gr_reporte.precio2      USING "######&.&&&&&&",       ",",
                      lr_reporte_22.pesos2    USING "#################&.&&"
      
      LET gi_registros = gi_registros + 1

END REPORT
################################################################################
FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vpos,vresultado)

   DEFINE vfolio       INTEGER,
          vetapa_cod   DECIMAL(2,0),
          vresultado   CHAR(50),
          vpos         INTEGER,
          hoy          DATE

   DEFINE hora_inicial       CHAR(08),
          vhora_final        CHAR(08)

   DEFINE cla_sel            CHAR(400),
          vconsecutivo       INTEGER

   LET hoy = TODAY

   LET cla_sel = " SELECT MAX(consecutivo) ",
                 " FROM   dis_ctrl_proceso ",
                 " WHERE  folio  = ",vfolio,
                 " AND    proceso_cod = 'CTAB034C' ",
                 " AND    etapa_cod = ",vetapa_cod

   PREPARE claexe3 FROM cla_sel

   DECLARE cur_proceso3 CURSOR FOR claexe3

   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vconsecutivo
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_sel = " UPDATE dis_ctrl_proceso ",
                 " SET    hora_final = ","'",vhora_final,"',",
                 "        folio      = ",vfolio,",",
                 "        parametro4 = ",vpos,",",
                 "        resultado  = ","'",vresultado,"'",
                 " WHERE  folio = ",vfolio,
                 " AND    proceso_cod   = 'CTAB034C'",
                 " AND    etapa_cod     = ",vetapa_cod,
                 " AND    consecutivo   = ",vconsecutivo CLIPPED
   PREPARE claexe4 FROM cla_sel

   EXECUTE claexe4

END FUNCTION
################################################################################
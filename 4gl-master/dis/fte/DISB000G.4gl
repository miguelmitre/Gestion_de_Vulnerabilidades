-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB000G                                                  --
-- Descripcion  => PROCESO BATCH DEVOLUCION DE RECURSOS POR CAMBIO DE REGIMEN--
-- Sistema      => DIS                                                       --
-- Por          => DMR                                                       --
-- Fecha        => 21 Jul  2011                                              --
-- Modificado   => Se modifico MENU para uso exclusivo Cambio de Regimen     --
-- Fecha        => 18 Ago  2011                                              --
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE
      hoy                DATE,
      usuario            CHAR(08),
      opc                CHAR(01),
      ejecuta            CHAR(200),
      hora_inicial       CHAR(08),
      hora_final         CHAR(08),
      vsubcuenta         CHAR(06),
      vsub               CHAR(06),
      vtipo              CHAR(01),
      vfecha_recepcion   DATE,
      vrecauda           CHAR(01),
      folio_preliq       INTEGER,
      fecha_preliq       DATE,
      fecha_preliq_bnx   DATE

   DEFINE
      vrecaudax, i       SMALLINT

   DEFINE
      tipo_liquida       CHAR(2)

   DEFINE
      arr_c              ,
      arr_l              SMALLINT

   DEFINE
      cla_sel            CHAR(300)

   DEFINE
      g_param   RECORD LIKE seg_modulo.*

   DEFINE
      g_reg     RECORD
                   nombre_archivo     CHAR(15),
                   fecha_valua        DATE
                END RECORD

   DEFINE
      g_liq     RECORD
                   folio              INTEGER,
                   fecha_proceso      DATE,
                   fecha_liquidacion  DATE,
                   precio_del_dia     DECIMAL(16,6),
                   titulos            DECIMAL(16,6),
                   valor_part         DECIMAL(16,6),
                   monto_vol          DECIMAL(16,6),
                   aporte_rcv         DECIMAL(16,6),
                   aporte_neto_rcv    DECIMAL(16,6),
                   aporte_sar         DECIMAL(16,6),
                   aporte_neto_sar    DECIMAL(16,6),
                   aporte_cs          DECIMAL(16,6),
                   aporte_neto_cs     DECIMAL(16,6),
                   aporte_cls         DECIMAL(16,6),
                   aporte_neto_cls    DECIMAL(16,6),
                   aporte_viv         DECIMAL(16,6),
                   aporte_viv_08      DECIMAL(16,6),
                   aporte_neto_viv    DECIMAL(16,6),
                   interes_rcv        DECIMAL(16,6),
                   interes_neto_rcv   DECIMAL(16,6),
                   interes_sar        DECIMAL(16,6),
                   interes_neto_sar   DECIMAL(16,6),
                   interes_cls        DECIMAL(16,6),
                   interes_neto_cls   DECIMAL(16,6),
                   interes_viv        DECIMAL(16,6),
                   interes_viv_08     DECIMAL(16,6),
                   interes_neto_viv   DECIMAL(16,6),
                   gran_tot           DECIMAL(16,6),
                   gran_tot_neto      DECIMAL(16,6),
                   gran_tot_cls       DECIMAL(16,6),
                   gran_tot_neto_cls  DECIMAL(16,6),
                   gran_tot_viv       DECIMAL(16,6),
                   gran_tot_viv_08    DECIMAL(16,6),
                   gran_tot_neto_viv  DECIMAL(16,6)
                END RECORD

   DEFINE
      g_liq_b   RECORD
                   folio              INTEGER,
                   fecha_proceso      DATE,
                   fecha_liquidacion  DATE,
                   precio_del_dia     DECIMAL(16,6),
                   titulos            DECIMAL(16,6),
                   valor_part         DECIMAL(16,6),
                   monto_vol          DECIMAL(16,6),
                   aporte_bono        DECIMAL(16,6),
                   aporte_neto_bono   DECIMAL(16,6),
                   aporte_dbmx        DECIMAL(16,6),
                   aporte_neto_dbmx   DECIMAL(16,6),
                   interes_bono       DECIMAL(16,6),
                   interes_neto_bono  DECIMAL(16,6),
                   interes_dbmx       DECIMAL(16,6),
                   interes_neto_dbmx  DECIMAL(16,6),
                   gran_tot_bono      DECIMAL(16,6),
                   gran_tot_net_bono  DECIMAL(16,6),
                   gran_tot_dbmx      DECIMAL(16,6),
                   gran_tot_net_dbmx  DECIMAL(16,6)
                END RECORD

   DEFINE
      g_liq_a   RECORD
                   folio              INTEGER,
                   fecha_proceso      DATE,
                   fecha_liquidacion  DATE,
                   precio_del_dia     DECIMAL(16,6),
                   titulos            DECIMAL(16,6),
                   valor_part         DECIMAL(16,6),
                   monto_vol          DECIMAL(16,6),
                   c_bono_pes         DECIMAL(16,6),
                   c_neto_bono_pes    DECIMAL(16,6),
                   c_bono_acc         DECIMAL(16,6),
                   c_neto_bono_acc    DECIMAL(16,6),
                   a_reti_pes         DECIMAL(16,6),
                   a_neto_reti_pes    DECIMAL(16,6)
                END RECORD

   DEFINE
      g_liq_i   RECORD
                   folio              INTEGER,
                   fecha_proceso      DATE,
                   fecha_liquidacion  DATE,
                   aporte_ret         DECIMAL(16,6),
                   aporte_ret_ex      DECIMAL(16,6),
                   aporte_ret_neto    DECIMAL(16,6),
                   aporte_cv          DECIMAL(16,6),
                   aporte_cv_ex       DECIMAL(16,6),
                   aporte_cv_neto     DECIMAL(16,6),
                   aporte_sar         DECIMAL(16,6),
                   aporte_sar_ex      DECIMAL(16,6),
                   aporte_sar_neto    DECIMAL(16,6),
                   aporte_aht         DECIMAL(16,6),
                   aporte_aht_ex      DECIMAL(16,6),
                   aporte_aht_neto    DECIMAL(16,6),
                   aporte_ahd         DECIMAL(16,6),
                   aporte_ahd_ex      DECIMAL(16,6),
                   aporte_ahd_neto    DECIMAL(16,6),
                   itra_ret           DECIMAL(16,6),
                   itra_ret_ex        DECIMAL(16,6),
                   itra_ret_neto      DECIMAL(16,6),
                   itra_cv            DECIMAL(16,6),
                   itra_cv_ex         DECIMAL(16,6),
                   itra_cv_neto       DECIMAL(16,6),
                   itra_sar           DECIMAL(16,6),
                   itra_sar_ex        DECIMAL(16,6),
                   itra_sar_neto      DECIMAL(16,6),
                   itra_aht           DECIMAL(16,6),
                   itra_aht_ex        DECIMAL(16,6),
                   itra_aht_neto      DECIMAL(16,6),
                   itra_ahd           DECIMAL(16,6),
                   itra_ahd_ex        DECIMAL(16,6),
                   itra_ahd_neto      DECIMAL(16,6),
                   tot_rcv            DECIMAL(16,6),
                   tot_rcv_ex         DECIMAL(16,6),
                   tot_rcv_neto       DECIMAL(16,6),
                   tot_sar            DECIMAL(16,6),
                   tot_sar_ex         DECIMAL(16,6),
                   tot_sar_neto       DECIMAL(16,6),
                   tot_aho            DECIMAL(16,6),
                   tot_aho_ex         DECIMAL(16,6),
                   tot_aho_neto       DECIMAL(16,6)
                END RECORD

   DEFINE
      g_sal ARRAY[999] OF RECORD
                             folio           INTEGER,
                             sub             CHAR(06),
                             fecha_archivo   DATE
                          END RECORD
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("DISB000G.log")

   SELECT *, USER
   INTO  g_param.*,
         usuario
   FROM  seg_modulo
   WHERE modulo_cod = "dis"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "DISB000G2" ATTRIBUTE(BORDER)
   DISPLAY " DISB000G       DEVOLUCION DE RECURSOS POR CAMBIO DE REGIMEN                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "     LECTURA DE ARCHIVO, HISTORICOS Y GENERACION DE ARCHIVO DE RESPUESTA            " AT 5,1 ATTRIBUTE(REVERSE)

   MENU "DEVOL. REC."
      COMMAND "Carga_devo" "Carga Archivo Devolucion Recursos Cambio Regimen"
         CALL Carga("N")
      COMMAND "Liquida_devo" "Liquidacion de Devolucion Recursos Cambio Regimen"
         CALL Liquidacion()
      COMMAND "Reverso_devo" "Reverso Arch. Devolucion Recursos Cambio Regimen"
         CALL Reverso_devo("N")
      COMMAND "Reporte_prov" "Reporte montos Provisionados en Cambio de Regimen"
         CALL Ejecuta_repor()
      COMMAND "Salir" "Salida del programa"
         EXIT MENU
   END MENU
   CLOSE WINDOW ventana1
END MAIN


FUNCTION Ejecuta_repor()
   LET ejecuta = " fglgo DISL013G.4gi "
   RUN ejecuta
END FUNCTION


FUNCTION Reverso_devo(tipo_rev)
   DEFINE
      cont          INTEGER,
      band          SMALLINT,
      tipo_carga    CHAR(1),
      fol_rev       INTEGER,
      tipo_rev      CHAR(1)

   OPEN WINDOW ventana1r AT 2,2 WITH FORM "DISB000G1" ATTRIBUTE(BORDER)
   IF tipo_rev = "N" THEN
      DISPLAY " DISB000G         REVERSO DE RECEPCION ARCH. CAMBIO DE REGIMEN                " AT 3,1 ATTRIBUTE(REVERSE)
   END IF

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "    REVERSO DE ARCHIVO, HISTORICOS, INDIVIDUALIZACION Y GENERACION ARCHIVO         " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.nombre_archivo

      AFTER FIELD nombre_archivo
         IF g_reg.nombre_archivo IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO "
            NEXT FIELD nombre_archivo
         ELSE
            IF tipo_rev = "N" THEN
               SELECT UNIQUE folio
               INTO  fol_rev
               FROM  dis_ctrl_proceso
               WHERE parametro1  = g_reg.nombre_archivo
               AND   proceso_cod = "DISB2001G"
               AND   folio IS NOT NULL
               AND   folio <> 0
            END IF

            IF fol_rev IS NULL THEN
               LET fol_rev = 0
            END IF

            SELECT count(*)
            INTO  cont
            FROM  dis_cuenta
            WHERE folio = fol_rev

            IF cont > 0 THEN
               ERROR "ARCHIVO LIQUIDADO ANTERIORMENTE NO SE PUEDE REVERSAR ... "
               NEXT FIELD nombre_archivo
            ELSE
               IF tipo_rev = "N" THEN
                  SELECT "OK"
                  FROM  dis_cza_devorec
                  WHERE folio = fol_rev

                  IF STATUS = NOTFOUND THEN
                     ERROR "ARCHIVO INEXISTENTE COMO PROCESO DE CAMBIO DE REGIMEN ISSSTE..."
                    -- NEXT FIELD nombre_archivo     #temp
                  END IF
               END IF
            END IF
         END IF

      EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      CLOSE WINDOW ventana1r
      LET INT_FLAG = FALSE
      RETURN
   END IF

   LET band = 0

   IF band = 0 THEN
      PROMPT " Desea ejecutar el REVERSO [S/N] ... " FOR opc

      IF opc MATCHES '[Ss]' THEN
         ERROR " Reversando Informacion ..."
         CALL Ejecuta_reverso(fol_rev,tipo_rev,g_reg.nombre_archivo)
      END IF
   END IF

   DISPLAY  "                                  " AT 14,15
   CLOSE WINDOW ventana1r
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Carga(tipo_carga)
   DEFINE
      tipo_carga    CHAR(1),
      cont          SMALLINT,
      band          SMALLINT

   LET vrecauda  = "N"
   LET vrecaudax = 1

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*

      AFTER FIELD nombre_archivo
         IF g_reg.nombre_archivo IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO "
            NEXT FIELD nombre_archivo
         ELSE
            SELECT count(*)
            INTO  cont
            FROM  dis_ctrl_proceso
            WHERE parametro1 = g_reg.nombre_archivo

            IF cont > 0 THEN
               ERROR " ARCHIVO CARGADO ANTERIORMENTE ... "
               NEXT FIELD nombre_archivo
            END IF
         END IF

      AFTER FIELD fecha_valua
         IF g_reg.fecha_valua IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO "
            NEXT FIELD fecha_valua
         ELSE
            SELECT count(*)
            INTO  cont
            FROM  dis_ctrl_proceso
            WHERE parametro1 = g_reg.nombre_archivo

            IF cont > 0 THEN
               ERROR " ARCHIVO CARGADO ANTERIORMENTE ... "
               NEXT FIELD nombre_archivo
            ELSE
               IF tipo_carga = "N" THEN
                  FOR i=1 TO 13 
                     IF i=1 OR i=2 OR i=3 OR i=4 OR i=5 OR i=13 THEN
                        SELECT "OK" 
                        FROM  glo_valor_accion
                        WHERE fecha_valuacion = g_reg.fecha_valua
                        AND codigo_siefore = i

                        IF STATUS = NOTFOUND THEN
                           ERROR "FALTA precio del dia de Siefore ",i USING "##"
                           NEXT FIELD nombre_archivo      #temp
                        END IF
                     END IF
                  END FOR
               END IF
            END IF
         END IF

      LET ejecuta = "cd ",g_param.ruta_rescate CLIPPED,"; ls > archivos" CLIPPED
      RUN ejecuta

      WHENEVER ERROR CONTINUE
         DATABASE safre_tmp
         DROP TABLE archivos_dis_bat
      WHENEVER ERROR STOP

      CREATE TABLE archivos_dis_bat
         (campo  CHAR(100))

      LET ejecuta = g_param.ruta_rescate CLIPPED,"/archivos" CLIPPED

      LOAD FROM ejecuta INSERT INTO archivos_dis_bat

      SELECT "X"
      FROM  safre_tmp:archivos_dis_bat
      WHERE campo = g_reg.nombre_archivo

      IF STATUS =  100 THEN
         ERROR "NOMBRE DE ARCHIVO INCORRECTO"
         SLEEP 3
         CLEAR FORM
         CLEAR SCREEN
         LET INT_FLAG = TRUE
      END IF

      DATABASE safre_af
      EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      RETURN
   END IF

   DISPLAY BY NAME g_reg.nombre_archivo, g_reg.fecha_valua
   LET band = 0

   IF band = 0 THEN
      PROMPT " Desea ejecutar el Proceso [S/N] ... " FOR opc

      IF opc MATCHES '[Ss]' THEN
         ERROR "Procesando Informacion ..."
         CALL Ejecuta_lectura(tipo_carga)

         LET ejecuta = "cd ",g_param.ruta_rescate CLIPPED,";rm archivos" CLIPPED
         RUN ejecuta
      END IF
   END IF

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
 

FUNCTION Ejecuta_lectura(tipo_carga)
   DEFINE
      tipo_carga     CHAR(1)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   CASE
      WHEN tipo_carga = "N"
         INSERT INTO dis_ctrl_proceso VALUES
            (TODAY,                   -- fecha_proceso
             "DISB2001G",             -- proceso_cod
             1,                       -- etapa_cod   -- LECTURA
             hora_inicial,            -- hora_inicial
             hora_final,              -- hora_final
             g_reg.nombre_archivo,    -- parametro1
             vrecauda,                -- parametro2
             g_reg.fecha_valua,       -- parametro3
             NULL,                    -- parametro4
             NULL,                    -- parametro5
             NULL,                    -- folio
             "PROCESANDO",            -- resultado
             usuario,                 -- usuario
             0)

         IF STATUS < 0 THEN
            ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
            SLEEP 4
            EXIT PROGRAM
         END IF
 
         ERROR "Ejecutando Lectura de Archivo por nohup ... "
         LET ejecuta = "nohup time fglgo DISB2001G.4gi 0 0 0 ",
                        g_reg.nombre_archivo CLIPPED," ",
                        vrecaudax," ",
                        g_reg.fecha_valua," ",
                        0,    --v2
                        " &"
         RUN ejecuta
   END CASE

   ERROR "El Proceso se Ejecuto Satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION


FUNCTION Ejecuta_reverso(folio_rev,tipo_rev,nom_archivo)
   DEFINE
      folio_rev      INTEGER,
      vrow           INTEGER,
      cla_upd        CHAR(1500),
      nom_archivo    CHAR(15),
      tipo_rev       CHAR(4)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   IF tipo_rev = "N" THEN
      INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB2001G",             -- proceso_cod
       1,                       -- etapa_cod   -- LECTURA
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       folio_rev,               -- parametro1
       vrecauda,                -- parametro2
       nom_archivo,             -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "REVERSANDO",            -- resultado
       usuario,                 -- usuario
       0)

      IF STATUS < 0 THEN
         ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Reverso Archivo",STATUS
         SLEEP 4
         EXIT PROGRAM
      END IF

      DELETE FROM dis_provision
      WHERE folio = folio_rev

      DELETE FROM dis_cza_devorec
      WHERE folio = folio_rev

      DELETE FROM dis_det_devorec
      WHERE folio = folio_rev

      DELETE FROM dis_dep_devorec
      WHERE folio = folio_rev

      DELETE FROM dis_sum_devorec
      WHERE folio = folio_rev

      SELECT UNIQUE(parametro1)
      INTO   nom_archivo
      FROM   dis_ctrl_proceso
      WHERE  folio = folio_rev
      AND    proceso_cod = "DISB2001G"

      DELETE FROM dis_ctrl_proceso
      WHERE  (parametro1 = nom_archivo OR proceso_cod = "DISB006G")

      LET hora_final = TIME

      LET cla_sel = "SELECT MAX(consecutivo) ",
                    "FROM dis_ctrl_proceso ",
                    "WHERE proceso_cod = 'DISB2001G' ",
                    " AND etapa_cod = 1 " CLIPPED

      PREPARE claexe99 FROM cla_sel
      DECLARE cur_proceso99 CURSOR FOR claexe99
      OPEN cur_proceso99
         FETCH cur_proceso99 INTO vrow
      CLOSE cur_proceso99

      LET cla_upd = "UPDATE dis_ctrl_proceso ",
            " SET   dis_ctrl_proceso.hora_final = ","'",hora_final,"'",",",
            "       dis_ctrl_proceso.etapa_cod  = 8,",
            "       dis_ctrl_proceso.folio      = ",folio_rev,",",
            "       dis_ctrl_proceso.resultado  = 'FOLIO REVERSADO'",
            " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001G' ",
            " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
            " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe11 FROM cla_upd
      EXECUTE claexe11
   END IF

   ERROR "El FOLIO FUE REVERSADO Exitosamente ..."
   SLEEP 3
   ERROR ""
END FUNCTION


FUNCTION Liquidacion()
   OPEN WINDOW vent10 AT 2,2 WITH FORM "DISB000G6" ATTRIBUTE(BORDER)
   DISPLAY " DISB007G          LIQUIDACION LIBERACION TERMINAL ISSSTE                     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   DISPLAY "                 A P O R T A C I O N E S                 TOTAL LIQUIDACION          " AT 7,1 ATTRIBUTE(REVERSE) 
             
   DISPLAY "                INTERESES   EXTEMPORANEOS                TOTAL LIQUIDACION          " AT 13,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                      T O T A L E S                      TOTAL LIQUIDACION          " AT 18,1 ATTRIBUTE(REVERSE) 

   MENU "Liquidacion" 
      COMMAND "Normal" "Liquidacion Normal"
         LET  vtipo = "A"
         CALL Subcuenta()
         CALL Liquidacion_normal()
      COMMAND "Salir"  "Salida"
         EXIT MENU
   END MENU
   CLOSE WINDOW vent10
END FUNCTION


FUNCTION Liquidacion_normal()
   LET INT_FLAG = FALSE

   INPUT BY NAME g_liq.*

      BEFORE FIELD folio
         CALL Despliega_saldos(vtipo,vsubcuenta)

      AFTER FIELD folio
         IF g_liq.folio IS NULL THEN        
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF                                 

         LET g_liq.fecha_proceso = TODAY

         IF vsubcuenta = "RCVI" THEN
            SELECT unique fech_liquidacion
            INTO g_liq.fecha_liquidacion
            FROM dis_dep_devorec
            WHERE folio = g_liq.folio
            AND ident_pago[14,15] IN ("71","72","73","74","75")
         END IF

         DISPLAY BY NAME g_liq.folio,
                         g_liq.fecha_proceso,
                         g_liq.fecha_liquidacion
         DISPLAY BY NAME g_liq.*

         IF vsubcuenta = "RCVI" THEN
            LET cla_sel = "SELECT 'X' ",
               " FROM  dis_dep_devorec ",
               " WHERE folio = ",g_liq.folio,
               " AND   estado = 3 ",
               " AND ident_pago[14,15] in ('71','72','73','74','75')",
               " GROUP  BY 1 "
         END IF

         PREPARE claexe FROM cla_sel
         DECLARE cur1 CURSOR FOR claexe
         OPEN cur1
         FETCH cur1

         IF STATUS <> 100 THEN
            ERROR "La subcuenta de este folio ya esta liquidada"
            SLEEP 3
            CLOSE cur1
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur1

         IF vsubcuenta = "RCVI" THEN
            LET cla_sel= "SELECT 'X' ",
               " FROM  dis_dep_devorec ",
               " WHERE folio = ",g_liq.folio,
               " AND   estado = 2 ",
               " AND ident_pago[14,15] in ('71','72','73','74','75')",
               " GROUP  BY 1 "
         END IF

         PREPARE claexe2 FROM cla_sel
         DECLARE cur2 CURSOR FOR claexe2
         OPEN cur2
         FETCH cur2

         IF STATUS = 100 THEN
            ERROR "La subcuenta de este folio no existe"
            SLEEP 3
            CLOSE cur2
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur2

         SELECT fecha_recepcion
         INTO  vfecha_recepcion
         FROM  dis_cza_devorec
         WHERE folio = g_liq.folio

      AFTER FIELD fecha_proceso                   -- parametro3
         IF g_liq.fecha_proceso IS NULL THEN        
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_proceso
         END IF
                                 
      AFTER FIELD fecha_liquidacion               -- parametro4
         IF g_liq.fecha_liquidacion IS NULL THEN        
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_liquidacion
 ---     ELSE
 ---        IF g_liq.fecha_liquidacion < TODAY THEN
 ---           ERROR " La Fecha de Liquidacion no puede ser MENOR a la Fecha del DIA "
 ---           NEXT FIELD fecha_liquidacion
 ---        END IF
         END IF

         FOR i=1 TO 13
            IF i=1 OR i=2 OR i=3 OR i=3 OR i=4 OR i=5 OR i=13 THEN
               SELECT precio_del_dia
               INTO  g_liq.precio_del_dia
               FROM  glo_valor_accion
               WHERE fecha_valuacion = g_liq.fecha_liquidacion
               AND   codigo_siefore  = i                         ----jerry

               IF STATUS = NOTFOUND THEN
                  ERROR " NO Existe Precio de Accion SIE ",i USING "&&"
                  NEXT FIELD fecha_liquidacion
               END IF
            END IF
         END FOR

         SELECT NVL(sum(monto_en_pesos),0)
         INTO  g_liq.monto_vol
         FROM  dis_provision
         WHERE folio = g_liq.folio
         AND   subcuenta = 10

         DISPLAY BY NAME g_liq.monto_vol

         LET g_liq.valor_part = 1                                     --dmr
         LET g_liq.titulos = g_liq.gran_tot / g_liq.valor_part        --v3

         IF g_liq.titulos IS NULL AND g_liq.monto_vol <= 0 THEN
            ERROR "Los titulos por adquirir no pueden ser nulos"
            NEXT FIELD fecha_liquidacion
         END IF

         LET g_liq.aporte_viv = g_liq.aporte_viv / g_liq.valor_part
         LET g_liq.aporte_neto_viv = g_liq.aporte_neto_viv / g_liq.valor_part
         LET g_liq.gran_tot_viv = g_liq.gran_tot_viv / g_liq.valor_part
         LET g_liq.gran_tot_neto_viv= g_liq.gran_tot_neto_viv / g_liq.valor_part
 
         LET g_liq.valor_part = 0                                --dmr TEMPORAL
         DISPLAY BY NAME g_liq.*
         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el Proceso [S/N] ..." FOR opc
   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_liquidacion("D")     # "D" Dispersion
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Subcuenta()
   OPEN WINDOW vent1 AT 07,02 WITH 2 ROWS, 31 COLUMNS ATTRIBUTE(BORDER)
   MENU "Subcuenta"
      COMMAND "RCVI" "Liquida RCVI,VOL,SAR,SOL y FOV"
         LET vsubcuenta = "RCVI"
         EXIT MENU
   END MENU
   CLOSE WINDOW vent1
END FUNCTION


FUNCTION Despliega_saldos(tipo,aporte)
   DEFINE
      tipo             CHAR(1),
      tipo_liquida2    CHAR(45),
      aporte           CHAR(6),
      fecha_liquida    DATE

   OPEN WINDOW wsaldo AT 9,2 WITH FORM "DISB000G4" ATTRIBUTE (BORDER)
   DISPLAY "DEPOSITOS PENDIENTES POR LIQUIDAR" AT 1,15
   DISPLAY aporte AT 1,55
   DISPLAY "-----------------------------------------------------------------------------" AT 2,1

   LET tipo_liquida     = " "
   LET tipo_liquida2    = " "

   IF vsubcuenta = "RCVI" THEN
      LET tipo_liquida  = "71"
      LET tipo_liquida2 = "'71','72','73','74','75'"
   ELSE
   END IF

   LET vsub    = vsubcuenta

   LET cla_sel = " SELECT folio,",
                      "'",vsub,"'",",",
                        " fecha_archivo ",
                 " FROM   dis_dep_devorec",
                 " WHERE  ident_pago[14,15] IN (",tipo_liquida2 CLIPPED,")",
                 " AND    estado = 2 ",
                 " AND    impt_aport_acept > 0 ",
                 " GROUP  BY 1,2,3 " CLIPPED,    --c22-6
                 " ORDER  BY 1,2 " CLIPPED

   PREPARE claexe3 FROM cla_sel
   DECLARE cur_saldo CURSOR FOR claexe3

   LET i = 1

   FOREACH cur_saldo INTO g_sal[i].*
      IF vsubcuenta = "RCVI" THEN
         SELECT "OK" FROM dis_cuenta
         WHERE folio = g_sal[i].folio
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            LET i = i + 1
         END IF
      ELSE
         LET i = i + 1
      END IF
   END FOREACH

   IF cla_sel IS NULL THEN                 --c22-11
      FOR i = 1 TO 30                      --c22-11
         INITIALIZE g_sal[i].* TO NULL     --c22-11
      END FOR                              --c22-11
   END IF

   CALL SET_COUNT(i-1)

   INPUT ARRAY g_sal WITHOUT DEFAULTS FROM scr_liquida.*

      BEFORE FIELD folio
         LET arr_c = ARR_CURR()
         LET arr_l = SCR_LINE()

      AFTER FIELD folio
         IF g_sal[arr_c].folio IS NULL THEN
            LET g_sal[arr_c].folio = 0
         END IF

      ON KEY (control-m)
         LET g_liq.folio = g_sal[arr_c].folio 
         CASE
            WHEN tipo_liquida = "71" 
               CALL Asigna_montos_aporte()
         END CASE

         EXIT INPUT
   END INPUT

   CLOSE WINDOW wsaldo
END FUNCTION


FUNCTION Asigna_montos_aporte()

-------------------------------- APORTES -----------------------------------

   IF vsub = "RCVI" THEN
      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.monto_vol
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (10)
      AND   tipo_movimiento = 86
      AND   estado          = 5

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_rcv
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (30,31)
      AND   tipo_movimiento IN (89,87)
      AND   estado          = 5

      LET g_liq.aporte_neto_rcv = g_liq.aporte_rcv

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_sar
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (13)
      AND   tipo_movimiento = 87
      AND   estado          = 5

      LET g_liq.aporte_neto_sar = g_liq.aporte_sar

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_cs
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (32)
      AND   tipo_movimiento = 89
      AND   estado          = 5

      LET g_liq.aporte_neto_cs = g_liq.aporte_cs

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_cls
      FROM  dis_provision
      WHERE folio        = g_liq.folio
      AND   subcuenta   IN (11,15,33,34)
      AND   tipo_movimiento IN (87,89)
      AND   estado          = 5

      LET g_liq.aporte_neto_cls = g_liq.aporte_cls

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_viv
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (36)
      AND   tipo_movimiento = 89
      AND   estado          = 5

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_viv_08
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (35)
      AND   tipo_movimiento = 1
      AND   estado          = 5

      LET g_liq.aporte_neto_viv = g_liq.aporte_viv + g_liq.aporte_viv_08

-------------------------------- INTERES -----------------------------------

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.interes_rcv
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (30,31)
      AND   tipo_movimiento = 3
      AND   estado          = 5

      LET g_liq.interes_neto_rcv = g_liq.interes_rcv

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.interes_sar
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (13)
      AND   tipo_movimiento = 3
      AND   estado          = 5

      LET g_liq.interes_neto_sar = g_liq.interes_sar

      LET g_liq.interes_cls      = 0
      LET g_liq.interes_neto_cls = 0

      SELECT NVL(sum(monto_en_pesos),0) 
      INTO  g_liq.interes_viv
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (36)
      AND   tipo_movimiento = 4
      AND   estado          = 5

      SELECT NVL(sum(monto_en_pesos),0) 
      INTO  g_liq.interes_viv_08
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (35)
      AND   tipo_movimiento = 4
      AND   estado          = 5

      LET g_liq.interes_neto_viv = g_liq.interes_viv + g_liq.interes_viv_08
   ELSE
   END IF

-------------------------------- TOTALES -----------------------------------

   LET g_liq.gran_tot      = g_liq.aporte_rcv   + g_liq.interes_rcv +
                             g_liq.aporte_sar   + g_liq.interes_sar +
                             g_liq.aporte_cs

   LET g_liq.gran_tot_neto = g_liq.aporte_neto_rcv + g_liq.interes_neto_rcv +
                             g_liq.aporte_neto_sar + g_liq.interes_neto_sar +
                             g_liq.aporte_neto_cs

   LET g_liq.gran_tot_cls      = g_liq.aporte_cls      + g_liq.interes_cls
   LET g_liq.gran_tot_neto_cls = g_liq.aporte_neto_cls + g_liq.interes_neto_cls 

   LET g_liq.gran_tot_viv      = g_liq.aporte_viv      + g_liq.interes_viv
   LET g_liq.gran_tot_viv_08   = g_liq.aporte_viv_08   + g_liq.interes_viv_08
   LET g_liq.gran_tot_neto_viv = g_liq.aporte_neto_viv + g_liq.interes_neto_viv
END FUNCTION


FUNCTION Ejecuta_liquidacion(tipo_pro)
   DEFINE
      tipo_pro       CHAR(1)
  
   LET hora_inicial = TIME
   LET hora_final   = NULL

   CASE
      WHEN tipo_pro = "D"
         INSERT INTO dis_ctrl_proceso VALUES 
         (TODAY,                   -- fecha_proceso
          "DISB007G",              -- proceso_cod
          2,                       -- etapa_cod     -- LIQUIDACION
          hora_inicial,            -- hora_inicial
          hora_final,              -- hora_final
          g_liq.folio,             -- parametro1
          vtipo,                   -- parametro2
          vsubcuenta,              -- parametro3
          g_liq.fecha_proceso,     -- parametro4
          g_liq.fecha_liquidacion, -- parametro5
          g_liq.folio,             -- folio
          "PROCESANDO",            -- resultado
          usuario,                 -- usuario 
          0)

         IF STATUS < 0 THEN
            ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso Liquidacion ",STATUS
            SLEEP 4
            EXIT PROGRAM
         END IF

         ERROR "Ejecutando Proceso de Liquidacion por nohup ..."
         SLEEP 3
         LET ejecuta = "nohup time fglgo DISB007G.4gi 0 0 0 ",
                              g_liq.folio CLIPPED," ",
                              vtipo, " ",
                              vsubcuenta," ",
                              g_liq.fecha_proceso," ",
                              g_liq.fecha_liquidacion," &"

         RUN ejecuta
   END CASE

   ERROR "El Proceso se Ejecuto Satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION


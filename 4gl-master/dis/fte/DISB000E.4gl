-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB000E                                                  --
-- Descripcion  => PROCESO BATCH DISPERSION Y LIBERACION DE PANTALLA         --
-- Sistema      => DIS                                                       --
-- Por          => DMR                                                       --
-- Fecha        => 10 Ene  2008                                              --
-- Modificado   => DMR                                                       --
-- Fecha        => 12 Ene  2008                                              --
-- Fecha        => 17 Ene  2008 (Comisiones,dias_antiguedad)                 --
-- Fecha        => 24 Ene  2008 (Factores CuoSoc por periodo_pago)           --
-- Modificado   => DMR                                                       --
-- Fecha        => 25 Jun  2008 (NUEVO MPT DE RECAUDACION ISSSTE JUNIO 08)   --
--              =>               TRAE RCV ISSSTE, CUO.SOC ISSSTE Y FOV 08    --
-- Modificado   => DMR                                                       --
-- Fecha        => 15 Jul  2008 (NUEVO MPT DE RECAUDACION ISSSTE JULIO 08)   --
--              =>               TRAE SAR ISSSTE, FOV 92 Y FOV 2008          --
--              => DMR                                                       --
-- Fecha        => 01 Oct  2008 (Mas de 1 archivo al dia,Garantia 45 no Acep.--
-- Modificado   => DMR                                                       --
-- Fecha        => 22 Oct  2008 Recepcion,Carga,Provision y Genera Arch.BONO.--
--                 Pension Usa subcuenta 13 para SAR ISSSTE                  --
-- Fecha        => 23 Oct  2009 Recepcion,Carga,Provision y Genera Arch.Int. --
--                 Transito ISSSTE.                                          --
-- Modificado   => DMR                                                       --
-- Fecha        => 12 Mar  2011 Recepcion,Carga y Genera Arch.BONO a Redimir --
--                 Anualmente                                                --
-- Modificado   => DMR 12 Ene 2015 CPL-1645                                  --
-- Fecha        => STARTLOG PERSONALIZADO y Estandarizacion de versiones     --
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
      vrecauda           CHAR(01)

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
                   nombre_archivo     CHAR(15)
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

   CALL STARTLOG(FGL_GETENV("USER")||".DISB000E.log")       #CPL-1645

   SELECT *, USER
   INTO  g_param.*,
         usuario
   FROM  seg_modulo
   WHERE modulo_cod = "dis"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "DISB000E1" ATTRIBUTE(BORDER)
   DISPLAY " DISB000E          DISPERSION LIBERACION TERMINAL ISSSTE                      " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "    LECTURA DE ARCHIVO, HISTORICOS, INDIVIDUALIZACION Y GENERACION CONFIND         " AT 5,1 ATTRIBUTE(REVERSE)

   MENU "PROCESOS ISSSTE"
      COMMAND "Dispersion" "Procesos de Dispersion"
         CALL Menudisp()
      COMMAND "Intereses" "Procesos de Intereses en Transito"
         CALL Menuinte()
      COMMAND "Bono" "Procesos de Bono"
         CALL Menubono()
      COMMAND "Salir" "Salir del Modulo"
         EXIT MENU
   END MENU
   CLOSE WINDOW ventana1
END MAIN


FUNCTION Menudisp()
   MENU "PROCESOS Dispersion"
      COMMAND "Normal" "Proceso de Dispersion ISSSTE Normal"
         CALL Menunormal()
      COMMAND "Salir" "Salir del Modulo"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION Menubono()
   MENU "PROCESOS BONO"
      COMMAND "Bono de pension" "Proceso de Bono de Pension Acreditacion"
         CALL Menubonop()
      COMMAND "bono Anticipado" "Proceso de Bono Anticipado"
         CALL Menubonoant()
      COMMAND "Redencion anual" "Proceso de Redencion Anual"
         CALL Menureden_anual()
      COMMAND "Salir" "Salir del Modulo"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION Menuinte()
   MENU "PROCESOS Intereses"
      COMMAND "Int. en transito" "Proceso de Intereses en Transito"
         CALL Menuinttran()
      COMMAND "Salir" "Salir del Modulo"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION Menubonoant()
   MENU "BONO ANT."
      COMMAND "Carga_bono_ant" "Carga Archivo Dispersion Bono Anticipado"
         CALL Carga("A")
      COMMAND "Liquida_bono_ant" "Liquidacion de Bono de Pension Anticipado"
         CALL Liquidacion_A()
      COMMAND "Reverso_bono_ant" "Reverso de Bono de Pension Anticipado"
         CALL Reverso_normal_bono("BANT")
      COMMAND "Salir" "Salida del programa"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION Menunormal()
   MENU "DISP. NORMAL"
      COMMAND "Carga_disp_nor" "Carga Archivo Dispersion Normal ISSSTE"
         CALL Carga("N")
      COMMAND "Liquidacion_disp_nor" "Liquidacion de Dispersion Normal ISSSTE"
         CALL Liquidacion()
      COMMAND "Reverso Disp." "Reverso de Archivo de Dispersion Normal"
         CALL Reverso_normal("RCVI")
      COMMAND "Salir" "Salida del programa"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION Menureden_anual()
   MENU "RED. ANUAL"
      COMMAND "Carga_archivo" "Carga Archivo de registros a Redimir"
         CALL Carga("R")
      COMMAND "Lista_reden" "Genera Listados de nss a Redimir por AÑO"
         CALL Listado_red()
      COMMAND "Reverso_reden" "Reverso de Archivo de Bono a Redimir"
         CALL Reverso_normal_bono("BONO")
      COMMAND "reden_Anual" "Redencion Anual del Bono de Pension"
         CALL Redencion_B()
      COMMAND "Salir" "Salida del programa"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION Menubonop()
   MENU "PROCESO BONO"
      COMMAND "Carga_Bono" "Carga Archivo Dispersion Bono"
         CALL Carga("B")
      COMMAND "Liquida_Bono" "Liquidacion de Bono de Pension"
         CALL Liquidacion_B()
      COMMAND "Reverso_bono" "Reverso de Archivo de Bono a Acreditar"
         CALL Reverso_normal_bono("BONB")
      COMMAND "Salir" "Salida del programa"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION Menuinttran()
   MENU "INT. TRANSITO"
      COMMAND "Carga_int_tran" "Carga Archivo de Intereses en Transito ISSSTE"
         CALL Carga("I")
      COMMAND "Liquida_int_tran" "Liquidacion de Intereses en Transito ISSSTE"
         CALL Liquidacion_I()
      COMMAND "Reverso_itra" "Reverso de Intereses en Transito Issste"
         CALL Reverso_normal("ITRA")
      COMMAND "Salir" "Salida del programa"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION Reverso_normal(tipo_rev)
   DEFINE
      cont          INTEGER,
      band          SMALLINT,
      tipo_carga    CHAR(1),
      fol_rev       INTEGER,
      tipo_rev      CHAR(4)

   OPEN WINDOW ventana1r AT 2,2 WITH FORM "DISB000E1" ATTRIBUTE(BORDER)
   IF tipo_rev = "ITRA" THEN
      DISPLAY " DISB000E         REVERSO DE INTERESES EN TRANSITO ISSSTE                     " AT 3,1 ATTRIBUTE(REVERSE)
   ELSE
      DISPLAY " DISB000E          REVERSO DE DISPERSION TERMINAL ISSSTE                      " AT 3,1 ATTRIBUTE(REVERSE)
   END IF

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "    REVERSO DE ARCHIVO, HISTORICOS, INDIVIDUALIZACION Y GENERACION CONFIND         " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.nombre_archivo

      AFTER FIELD nombre_archivo
         IF g_reg.nombre_archivo IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO "
            NEXT FIELD nombre_archivo
         ELSE
            IF tipo_rev = "ITRA" THEN
               SELECT UNIQUE folio
               INTO  fol_rev
               FROM  dis_ctrl_proceso
               WHERE parametro1  = g_reg.nombre_archivo
               AND   proceso_cod = "DISB2001I"
               AND   folio IS NOT NULL
               AND   folio <> 0
            ELSE
               SELECT UNIQUE folio
               INTO  fol_rev
               FROM  dis_ctrl_proceso
               WHERE parametro1  = g_reg.nombre_archivo
               AND   proceso_cod = "DISB2001E"
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
               IF tipo_rev = "ITRA" THEN
                  SELECT "OK"
                  FROM  dis_cza_inttran
                  WHERE folio = fol_rev

                  IF STATUS = NOTFOUND THEN
                     ERROR "ARCHIVO INEXISTENTE COMO PROCESO DE INTERESES EN TRANSITO ISSSTE..."
                     NEXT FIELD nombre_archivo
                  END IF
               ELSE
                  SELECT "OK"
                  FROM  dis_cza_issste
                  WHERE folio = fol_rev

                  IF STATUS = NOTFOUND THEN
                     ERROR "ARCHIVO INEXISTENTE COMO PROCESO DE DISPERSION ISSSTE..."
                     NEXT FIELD nombre_archivo
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


FUNCTION Reverso_normal_bono(tipo_rev)
   DEFINE
      cont          INTEGER,
      band          SMALLINT,
      tipo_carga    CHAR(1),
      fol_rev       INTEGER,
      tipo_rev      CHAR(4)

   OPEN WINDOW ventana1r AT 2,2 WITH FORM "DISB000E1" ATTRIBUTE(BORDER)
   IF tipo_rev = "BONO" THEN
      DISPLAY " DISB000E       REVERSO DEL PROCESO DE BONO PENSION ANUAL                    " AT 3,1 ATTRIBUTE(REVERSE)
   ELSE
      IF tipo_rev = "BONB" THEN
         DISPLAY " DISB000E     REVERSO DEL PROCESO DE BONO PENSION ACREDITACION               " AT 3,1 ATTRIBUTE(REVERSE)
      ELSE
         DISPLAY " DISB000E        REVERSO DEL PROCESO DE BONO ANTICIPADO                      " AT 3,1 ATTRIBUTE(REVERSE)
      END IF
   END IF

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "   REVERSO DE ARCHIVO, HISTORICOS, INDIVIDUALIZACION Y GENERACION BONOCONF        " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.nombre_archivo

      AFTER FIELD nombre_archivo
         IF g_reg.nombre_archivo IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO "
            NEXT FIELD nombre_archivo
         ELSE
            IF tipo_rev = "BONO" THEN
               SELECT UNIQUE folio
               INTO  fol_rev
               FROM  dis_ctrl_proceso
               WHERE parametro1  = g_reg.nombre_archivo
               AND   proceso_cod = "DISB2001R"
               AND   folio IS NOT NULL
               AND   folio <> 0
            ELSE
               IF tipo_rev = "BONB" THEN
                  SELECT UNIQUE folio
                  INTO  fol_rev
                  FROM  dis_ctrl_proceso
                  WHERE parametro1  = g_reg.nombre_archivo
                  AND   proceso_cod = "DISB2001P"
                  AND   folio IS NOT NULL
                  AND   folio <> 0
               ELSE
                  SELECT UNIQUE folio
                  INTO  fol_rev
                  FROM  dis_ctrl_proceso
                  WHERE parametro1  = g_reg.nombre_archivo
                  AND   proceso_cod = "DISB2001A"
                  AND   folio IS NOT NULL
                  AND   folio <> 0
               END IF
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
               IF tipo_rev = "BONO" THEN
                  SELECT "OK"
                  FROM  dis_cza_bono
                  WHERE folio = fol_rev

                  IF STATUS = NOTFOUND THEN
                     ERROR "ARCHIVO INEXISTENTE COMO PROCESO DE BONO DE PENSION ANUAL..."
                     NEXT FIELD nombre_archivo
                  END IF
               ELSE
                  IF tipo_rev = "BONB" THEN
                     SELECT "OK"
                     FROM  dis_cza_bono
                     WHERE folio = fol_rev

                     IF STATUS = NOTFOUND THEN
                        ERROR "ARCHIVO INEXISTENTE COMO PROCESO DE BONO DE PENSION ACREDITACION..."
                        NEXT FIELD nombre_archivo
                     END IF
                  ELSE
                     SELECT "OK"
                     FROM  dis_cza_bono_ant
                     WHERE folio = fol_rev

                     IF STATUS = NOTFOUND THEN
                        ERROR "ARCHIVO INEXISTENTE COMO PROCESO DE BONO ANTICIPADO..."
                        NEXT FIELD nombre_archivo
                     END IF
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
         CALL Ejecuta_reverso_bono(fol_rev,tipo_rev,g_reg.nombre_archivo)
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

            IF tipo_carga = "A" THEN
               PROMPT " Esta cargado el PRECIO de la UDI Fecha Bono ANTICIPADO [S/N] ...? " FOR opc

               IF opc NOT MATCHES '[Ss]' THEN
                  ERROR " Proceso Cancelado ..."
                  NEXT FIELD nombre_archivo
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

   DISPLAY BY NAME g_reg.nombre_archivo
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
             "DISB2001E",             -- proceso_cod
             1,                       -- etapa_cod   -- LECTURA
             hora_inicial,            -- hora_inicial
             hora_final,              -- hora_final
             g_reg.nombre_archivo,    -- parametro1
             vrecauda,                -- parametro2
             0,                       -- parametro3
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
         LET ejecuta = "nohup time fglgo DISB2001E.4gi 0 0 0 ",
                        g_reg.nombre_archivo CLIPPED," ",
                        vrecaudax," ",
                       "01/01/2003"," ",
                        0,    --v2
                        " &"
         RUN ejecuta

      WHEN tipo_carga = "I"
         INSERT INTO dis_ctrl_proceso VALUES
            (TODAY,                   -- fecha_proceso
             "DISB2001I",             -- proceso_cod
             1,                       -- etapa_cod   -- LECTURA
             hora_inicial,            -- hora_inicial
             hora_final,              -- hora_final
             g_reg.nombre_archivo,    -- parametro1
             vrecauda,                -- parametro2
             0,                       -- parametro3
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
         LET ejecuta = "nohup time fglgo DISB2001I.4gi 0 0 0 ",
                        g_reg.nombre_archivo CLIPPED," ",
                        vrecaudax," ",
                       "01/01/2003"," ",
                        0,    --v2
                        " &"
         RUN ejecuta

      WHEN tipo_carga = "B"
         INSERT INTO dis_ctrl_proceso VALUES
            (TODAY,                   -- fecha_proceso
             "DISB2001P",             -- proceso_cod
             1,                       -- etapa_cod   -- LECTURA
             hora_inicial,            -- hora_inicial
             hora_final,              -- hora_final
             g_reg.nombre_archivo,    -- parametro1
             vrecauda,                -- parametro2
             0,                       -- parametro3
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
         LET ejecuta = "nohup time fglgo DISB2001P.4gi 0 0 0 ",
                        g_reg.nombre_archivo CLIPPED," ",
                        vrecaudax," ",
                        "01/01/2003"," ",
                        0,    --v2
                        " &"
         RUN ejecuta

      WHEN tipo_carga = "A"
         INSERT INTO dis_ctrl_proceso VALUES
            (TODAY,                   -- fecha_proceso
             "DISB2001A",             -- proceso_cod
             1,                       -- etapa_cod   -- LECTURA
             hora_inicial,            -- hora_inicial
             hora_final,              -- hora_final
             g_reg.nombre_archivo,    -- parametro1
             vrecauda,                -- parametro2
             0,                       -- parametro3
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
         LET ejecuta = "nohup time fglgo DISB2001A.4gi 0 0 0 ",
                        g_reg.nombre_archivo CLIPPED," ",
                        vrecaudax," ",
                        "01/01/2003"," ",
                        0,    --v2
                        " &"
         RUN ejecuta

      WHEN tipo_carga = "R"
         INSERT INTO dis_ctrl_proceso VALUES
            (TODAY,                   -- fecha_proceso
             "DISB2001R",             -- proceso_cod
             1,                       -- etapa_cod   -- LECTURA
             hora_inicial,            -- hora_inicial
             hora_final,              -- hora_final
             g_reg.nombre_archivo,    -- parametro1
             vrecauda,                -- parametro2
             0,                       -- parametro3
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
         LET ejecuta = "nohup time fglgo DISB2001R.4gi 0 0 0 ",
                        g_reg.nombre_archivo CLIPPED," ",
                        vrecaudax," ",
                        "01/01/2003"," ",
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

   IF tipo_rev = "ITRA" THEN
      INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB2001I",             -- proceso_cod
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

      DELETE FROM dis_cza_inttran
      WHERE folio = folio_rev

      DELETE FROM dis_det_inttran
      WHERE folio = folio_rev

      DELETE FROM dis_dep_inttran
      WHERE folio = folio_rev

      DELETE FROM dis_sum_inttran
      WHERE folio = folio_rev

      SELECT UNIQUE(parametro1)
      INTO   nom_archivo
      FROM   dis_ctrl_proceso
      WHERE  folio = folio_rev
      AND    proceso_cod = "DISB2001I"

      DELETE FROM dis_ctrl_proceso
      WHERE  (parametro1 = nom_archivo OR proceso_cod = "DISB006I")

      LET hora_final = TIME

      LET cla_sel = "SELECT MAX(consecutivo) ",
                    "FROM dis_ctrl_proceso ",
                    "WHERE proceso_cod = 'DISB2001I' ",
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
            " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001I' ",
            " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
            " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

      PREPARE claexe11 FROM cla_upd
      EXECUTE claexe11
   ELSE
      INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB2001E",             -- proceso_cod
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

       DELETE FROM dis_cza_issste
       WHERE folio = folio_rev

       DELETE FROM dis_det_issste
       WHERE folio = folio_rev

       DELETE FROM dis_dep_issste
       WHERE folio = folio_rev

       DELETE FROM dis_sum_issste
       WHERE folio = folio_rev

       SELECT UNIQUE(parametro1)
       INTO   nom_archivo
       FROM   dis_ctrl_proceso
       WHERE  folio = folio_rev
       AND    proceso_cod = "DISB2001E"

       DELETE FROM dis_ctrl_proceso
       WHERE  (parametro1 = nom_archivo OR proceso_cod = "DISB006E")

       LET hora_final = TIME

       LET cla_sel = "SELECT MAX(consecutivo) ",
                     "FROM dis_ctrl_proceso ",
                     "WHERE proceso_cod = 'DISB2001E' ",
                     " AND etapa_cod = 1 " CLIPPED

       PREPARE claexe99i FROM cla_sel
       DECLARE cur_proces99i CURSOR FOR claexe99i
       OPEN cur_proces99i
          FETCH cur_proces99i INTO vrow
       CLOSE cur_proces99i

       LET cla_upd = "UPDATE dis_ctrl_proceso ",
             " SET   dis_ctrl_proceso.hora_final = ","'",hora_final,"'",",",
             "       dis_ctrl_proceso.etapa_cod  = 8,",
             "       dis_ctrl_proceso.folio      = ",folio_rev,",",
             "       dis_ctrl_proceso.resultado  = 'FOLIO REVERSADO'",
             " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001E' ",
             " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
             " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

       PREPARE claexe11i FROM cla_upd
       EXECUTE claexe11i
   END IF

   ERROR "El FOLIO FUE REVERSADO Exitosamente ..."
   SLEEP 3
   ERROR ""
END FUNCTION


FUNCTION Ejecuta_reverso_bono(folio_rev,tipo_rev,nom_archivo)
   DEFINE
      folio_rev      INTEGER,
      vrow           INTEGER,
      cla_upd        CHAR(1500),
      nom_archivo    CHAR(15),
      tipo_rev       CHAR(4)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   IF tipo_rev = "BONO" OR tipo_rev = "BONB" THEN
      IF tipo_rev = "BONO" THEN
         INSERT INTO dis_ctrl_proceso VALUES
         (TODAY,                   -- fecha_proceso
          "DISB2001R",             -- proceso_cod
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
      ELSE
         INSERT INTO dis_ctrl_proceso VALUES
         (TODAY,                   -- fecha_proceso
          "DISB2001P",             -- proceso_cod
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
      END IF

      DELETE FROM dis_provision
      WHERE folio = folio_rev

      DELETE FROM dis_cza_bono
      WHERE folio = folio_rev

      DELETE FROM dis_det_bono
      WHERE folio = folio_rev

      DELETE FROM dis_dep_bono
      WHERE folio = folio_rev

      DELETE FROM dis_sum_bono
      WHERE folio = folio_rev

      IF tipo_rev = "BONO" THEN
         SELECT UNIQUE(parametro1)
         INTO   nom_archivo
         FROM   dis_ctrl_proceso
         WHERE  folio = folio_rev
         AND    proceso_cod = "DISB2001R"

         DELETE FROM dis_ctrl_proceso
         WHERE  (parametro1 = nom_archivo OR proceso_cod = "DISB006R")

         LET hora_final = TIME

         LET cla_sel = "SELECT MAX(consecutivo) ",
                       "FROM dis_ctrl_proceso ",
                       "WHERE proceso_cod = 'DISB2001R' ",
                       " AND etapa_cod = 1 " CLIPPED

         PREPARE claxe99 FROM cla_sel
         DECLARE cur_proces99 CURSOR FOR claxe99
         OPEN cur_proces99
            FETCH cur_proces99 INTO vrow
         CLOSE cur_proces99

         LET cla_upd = "UPDATE dis_ctrl_proceso ",
               " SET   dis_ctrl_proceso.hora_final = ","'",hora_final,"'",",",
               "       dis_ctrl_proceso.etapa_cod  = 8,",
               "       dis_ctrl_proceso.folio      = ",folio_rev,",",
               "       dis_ctrl_proceso.resultado  = 'FOLIO REVERSADO'",
               " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001R' ",
               " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
               " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

         PREPARE claxe11 FROM cla_upd
         EXECUTE claxe11
      ELSE
         SELECT UNIQUE(parametro1)
         INTO   nom_archivo
         FROM   dis_ctrl_proceso
         WHERE  folio = folio_rev
         AND    proceso_cod = "DISB2001P"

         DELETE FROM dis_ctrl_proceso
         WHERE  (parametro1 = nom_archivo OR proceso_cod = "DISB006P")

         LET hora_final = TIME

         LET cla_sel = "SELECT MAX(consecutivo) ",
                       "FROM dis_ctrl_proceso ",
                       "WHERE proceso_cod = 'DISB2001P' ",
                       " AND etapa_cod = 1 " CLIPPED

         PREPARE clxe99p FROM cla_sel
         DECLARE cur_pro99p CURSOR FOR clxe99p
         OPEN cur_pro99p
            FETCH cur_pro99p INTO vrow
         CLOSE cur_pro99p

         LET cla_upd = "UPDATE dis_ctrl_proceso ",
               " SET   dis_ctrl_proceso.hora_final = ","'",hora_final,"'",",",
               "       dis_ctrl_proceso.etapa_cod  = 8,",
               "       dis_ctrl_proceso.folio      = ",folio_rev,",",
               "       dis_ctrl_proceso.resultado  = 'FOLIO REVERSADO'",
               " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001P' ",
               " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
               " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

         PREPARE clax11p FROM cla_upd
         EXECUTE clax11p
      END IF
   ELSE
      INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB2001A",             -- proceso_cod
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

       DELETE FROM dis_cza_bono_ant
       WHERE folio = folio_rev

       DELETE FROM dis_det_bono_ant
       WHERE folio = folio_rev

       DELETE FROM dis_dep_bono_ant
       WHERE folio = folio_rev

       DELETE FROM dis_sum_bono_ant
       WHERE folio = folio_rev

       SELECT UNIQUE(parametro1)
       INTO   nom_archivo
       FROM   dis_ctrl_proceso
       WHERE  folio = folio_rev
       AND    proceso_cod = "DISB2001A"

       DELETE FROM dis_ctrl_proceso
       WHERE  (parametro1 = nom_archivo OR proceso_cod = "DISB006A")

       LET hora_final = TIME

       LET cla_sel = "SELECT MAX(consecutivo) ",
                     "FROM dis_ctrl_proceso ",
                     "WHERE proceso_cod = 'DISB2001A' ",
                     " AND etapa_cod = 1 " CLIPPED

       PREPARE claxe99i FROM cla_sel
       DECLARE cur_procs99i CURSOR FOR claxe99i
       OPEN cur_procs99i
          FETCH cur_procs99i INTO vrow
       CLOSE cur_procs99i

       LET cla_upd = "UPDATE dis_ctrl_proceso ",
             " SET   dis_ctrl_proceso.hora_final = ","'",hora_final,"'",",",
             "       dis_ctrl_proceso.etapa_cod  = 8,",
             "       dis_ctrl_proceso.folio      = ",folio_rev,",",
             "       dis_ctrl_proceso.resultado  = 'FOLIO REVERSADO'",
             " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001A' ",
             " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
             " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

       PREPARE claxe11i FROM cla_upd
       EXECUTE claxe11i
   END IF

   ERROR "El FOLIO FUE REVERSADO Exitosamente ..."
   SLEEP 3
   ERROR ""
END FUNCTION


FUNCTION Liquidacion()
   OPEN WINDOW vent10 AT 2,2 WITH FORM "DISB000E6" ATTRIBUTE(BORDER)
   DISPLAY " DISB007E          LIQUIDACION LIBERACION TERMINAL ISSSTE                     " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   DISPLAY "                 A P O R T A C I O N E S                 TOTAL LIQUIDACION          " AT 7,1 ATTRIBUTE(REVERSE) 
             
   DISPLAY "                INTERESES   EXTEMPORANEOS                TOTAL LIQUIDACION          " AT 13,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                      T O T A L E S                      TOTAL LIQUIDACION          " AT 18,1 ATTRIBUTE(REVERSE) 

   MENU "Liquidacion" 
      COMMAND "Normal" "Liquidacion Normal"
         LET  vtipo = "A"
         CALL Subcuenta()
         CALL Liquidacion_normal()
      COMMAND "43 Bis" "Liquidacion Viv. Gar."
         LET  vtipo = "G"
         LET  vsubcuenta = "GAR"  
         CALL Liquidacion_garantia()
      COMMAND "Salir"  "Salida"
         EXIT MENU
   END MENU
   CLOSE WINDOW vent10
END FUNCTION


FUNCTION Liquidacion_A()
   OPEN WINDOW vent11 AT 2,2 WITH FORM "DISB00E7A" ATTRIBUTE(BORDER)
   DISPLAY " DISB007A      LIQUIDACION LIBERACION BONO PENSION ANTICIPADA                   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   DISPLAY "                   CARGOS DE BONO REDIMIDO             T O T A L E S                " AT 10,1 ATTRIBUTE(REVERSE)
   DISPLAY "                   ABONOS DE BONO REDIMIDO             T O T A L E S                " AT 16,1 ATTRIBUTE(REVERSE)
 
   MENU "Liq._Bono_Anticipado"
      COMMAND "Normal" "Liquidacion Bono Anticipado Normal"
         LET  vtipo = "A"
         LET  vsubcuenta = "BONOA"
         CALL Liquidacion_bono_ant()
      COMMAND "Salir"  "Salida"
         EXIT MENU
   END MENU
   CLOSE WINDOW vent11
END FUNCTION


FUNCTION Liquidacion_B()
   OPEN WINDOW vent11 AT 2,2 WITH FORM "DISB000E7" ATTRIBUTE(BORDER)
   DISPLAY " DISB007P          LIQUIDACION LIBERACION BONO PENSION ISSSTE                 " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   DISPLAY "                 A P O R T A C I O N E S                 TOTAL LIQUIDACION          " AT 7,1 ATTRIBUTE(REVERSE) 
             
   DISPLAY "                INTERESES   EXTEMPORANEOS                TOTAL LIQUIDACION          " AT 12,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                      T O T A L E S                      TOTAL LIQUIDACION          " AT 17,1 ATTRIBUTE(REVERSE) 

   MENU "Liqui_Bono" 
      COMMAND "Normal" "Liquidacion Bono Normal"
         LET  vtipo = "B"
         CALL Liquidacion_bono()
      COMMAND "Salir"  "Salida"
         EXIT MENU
   END MENU
   CLOSE WINDOW vent11
END FUNCTION


FUNCTION Liquidacion_I()
   OPEN WINDOW vent11 AT 2,2 WITH FORM "DISB000E11" ATTRIBUTE(BORDER)
   DISPLAY " DISB007I          LIQUIDACION INTERESES EN TRANSITO ISSSTE                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   DISPLAY "           APORTACIONES            EXTEMPORANEAS         TOTAL LIQUIDACION          " AT 6,1 ATTRIBUTE(REVERSE) 
             
   DISPLAY "        INTERESES TRANSITO         EXTEMPORANEOS         TOTAL LIQUIDACION          " AT 12,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                    T  O  T  A  L  E  S                  TOTAL LIQUIDACION          " AT 18,1 ATTRIBUTE(REVERSE) 

   MENU "Liqui_Intereses" 
      COMMAND "Normal" "Liquidacion Interese en Transito ISSSTE"
         LET  vtipo = "I"
         CALL Liquidacion_inttran()
      COMMAND "Salir"  "Salida"
         EXIT MENU
   END MENU
   CLOSE WINDOW vent11
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
            FROM dis_dep_issste
            WHERE folio = g_liq.folio
            AND ident_pago[14,15] NOT IN ("45","46","48")
         END IF

         IF vsubcuenta = "CSOC" THEN
           {SELECT unique fech_liquidacion
            INTO g_liq.fecha_liquidacion
            FROM dis_dep_issste
            WHERE folio = g_liq.folio
            AND ident_pago[14,15] IN ("48")   DEFASE LIQ. CSOC}

            LET g_liq.fecha_liquidacion = TODAY
         END IF

         DISPLAY BY NAME g_liq.folio,
                         g_liq.fecha_proceso,
                         g_liq.fecha_liquidacion
         DISPLAY BY NAME g_liq.*

         IF vsubcuenta = "RCVI" THEN
            LET cla_sel = "SELECT 'X' ",
               " FROM  dis_dep_issste ",
               " WHERE folio = ",g_liq.folio,
               " AND   estado = 3 ",
               " AND ident_pago[14,15] in ('41','42','43','44','45','46','47','81','82','83')",
               " GROUP  BY 1 "
         END IF

         IF vsubcuenta = "CSOC" THEN
            LET cla_sel = "SELECT 'X' ",
               " FROM  dis_dep_issste ",
               " WHERE folio = ",g_liq.folio,
               " AND   estado = 3 ",
               " AND ident_pago[14,15] in ('48')",
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
               " FROM  dis_dep_issste ",
               " WHERE folio = ",g_liq.folio,
               " AND   estado = 2 ",
               " AND ident_pago[14,15] in ('41','42','43','44','45','46','47','81','82','83')",
               " GROUP  BY 1 "
         END IF

         IF vsubcuenta = "CSOC" THEN
            LET cla_sel= "SELECT 'X' ",
               " FROM  dis_dep_issste ",
               " WHERE folio = ",g_liq.folio,
               " AND   estado = 2 ",
               " AND ident_pago[14,15] in ('48')",
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
         FROM  dis_cza_issste
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

         SELECT precio_del_dia
         INTO  g_liq.precio_del_dia
         FROM  glo_valor_accion
         WHERE fecha_valuacion = g_liq.fecha_liquidacion
         AND   codigo_siefore  = 1                         ----jerry

         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de Accion SIE 1 "
            NEXT FIELD fecha_liquidacion
         END IF

         SELECT precio_del_dia
         INTO  g_liq.precio_del_dia
         FROM  glo_valor_accion
         WHERE fecha_valuacion = g_liq.fecha_liquidacion
         AND   codigo_siefore  = 2                         ----jerry
 
         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de Accion SIE 2 "
            NEXT FIELD fecha_liquidacion
         END IF

         SELECT precio_del_dia
         INTO  g_liq.valor_part
         FROM  glo_valor_accion
         WHERE fecha_valuacion = g_liq.fecha_liquidacion
         AND   codigo_siefore  = 12                        ----jerry

         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de la Aiv, SIE 12 "
            NEXT FIELD fecha_liquidacion
         END IF

         SELECT NVL(sum(monto_en_pesos),0)
         INTO  g_liq.monto_vol
         FROM  dis_provision
         WHERE folio = g_liq.folio
         AND   subcuenta = 3

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


FUNCTION Redencion_B()
   PROMPT "Desea ejecutar el Proceso [S/N] ..." FOR opc
   IF opc MATCHES '[Ss]' THEN
      ERROR " Ejecutando Redencion del Bono ... "
      LET ejecuta = " fglgo DISB007R.4gi "
      RUN ejecuta
   END IF
END FUNCTION


FUNCTION Listado_red()
   PROMPT "Desea ejecutar el Proceso [S/N] ..." FOR opc
   IF opc MATCHES '[Ss]' THEN
      ERROR " Ejecutando Listado de  NSS´s  a Redimir ... "
      LET ejecuta = " fglgo DISL013P.4gi "
      RUN ejecuta
   END IF
END FUNCTION


FUNCTION Liquidacion_garantia()
   LET INT_FLAG = FALSE

   INPUT BY NAME g_liq.*

      BEFORE FIELD folio
         CALL Despliega_saldos(vtipo,vsubcuenta)

      AFTER FIELD folio
         IF g_liq.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         LET g_liq.fecha_proceso     = TODAY
         LET g_liq.fecha_liquidacion = TODAY

         DISPLAY BY NAME g_liq.folio,
                         g_liq.fecha_proceso,
                         g_liq.fecha_liquidacion
         DISPLAY BY NAME g_liq.*

         LET cla_sel = "SELECT 'X' ",
                       " FROM   dis_dep_issste ",
                       " WHERE  folio = ",g_liq.folio,
                       " AND    estado = 3 ",
                       " AND ident_pago[14,15] in ('46','45') ",
                       " GROUP  BY 1 "

         PREPARE claexe1 FROM cla_sel
         DECLARE cur3a CURSOR FOR claexe1
         OPEN cur3a
         FETCH cur3a

         IF STATUS <> 100 THEN
            ERROR "La subcuenta de este folio ya esta liquidada"
            SLEEP 3
            CLOSE cur3a
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur3a

         LET cla_sel= "SELECT 'X' ",
                      " FROM   dis_dep_issste ",
                      " WHERE  folio = ",g_liq.folio,
                      " AND    estado = 2 ",
                      " AND ident_pago[14,15] in ('46','45') ",
                      " GROUP  BY 1 "

         PREPARE claexe2i FROM cla_sel
         DECLARE cur2b CURSOR FOR claexe2i
         OPEN cur2b
         FETCH cur2b

         IF STATUS = 100 THEN
            ERROR "La subcuenta de este folio no existe"
            SLEEP 3
            CLOSE cur2b
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur2b

         SELECT fecha_recepcion
         INTO  vfecha_recepcion
         FROM  dis_cza_issste
         WHERE folio = g_liq.folio

      AFTER FIELD fecha_proceso                   -- parametro3
         IF g_liq.fecha_proceso IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_proceso
         END IF

      AFTER FIELD fecha_liquidacion              -- parametro4
         IF g_liq.fecha_liquidacion IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_liquidacion
 ---     ELSE
 ---        IF g_liq.fecha_liquidacion < TODAY THEN
 ---           ERROR " La Fecha de Liquidacion no puede ser MENOR a la Fecha del DIA "
 ---           NEXT FIELD fecha_liquidacion
 ---        END IF
         END IF

         SELECT precio_del_dia                             ----jerry
         INTO  g_liq.valor_part                            ----jerry
         FROM  glo_valor_accion                            ----jerry
         WHERE fecha_valuacion = g_liq.fecha_liquidacion   ----jerry
         AND   codigo_siefore  = 12                        ----jerry

         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de la Aiv SIE 12 "
            NEXT FIELD fecha_liquidacion
         END IF

         LET g_liq.valor_part = 1
         LET g_liq.titulos = 0
     --  LET g_liq.titulos = g_liq.gran_tot1 / g_liq.valor_part         --v3

         IF g_liq.titulos IS NULL THEN
            ERROR "Los titulos por adquirir no pueden ser nulos"
            NEXT FIELD fecha_acreditacion
         END IF

         LET g_liq.aporte_viv = g_liq.aporte_viv / g_liq.valor_part
         LET g_liq.aporte_neto_viv = g_liq.aporte_neto_viv / g_liq.valor_part
         LET g_liq.gran_tot_viv = g_liq.gran_tot_viv / g_liq.valor_part
         LET g_liq.gran_tot_neto_viv= g_liq.gran_tot_neto_viv / g_liq.valor_part

         LET g_liq.valor_part = 0                                      --dmr
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

   PROMPT "Desea Ejecutar el Proceso [S/N] ..." FOR opc
   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_liquidacion("D")    # "D" Dispersion
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Liquidacion_bono()
   LET INT_FLAG = FALSE

   INPUT BY NAME g_liq_b.*

      BEFORE FIELD folio
         CALL Despliega_saldos_BAI("BONO")

      AFTER FIELD folio
         IF g_liq_b.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         LET g_liq_b.fecha_proceso = TODAY

         SELECT unique fech_liquidacion
         INTO g_liq_b.fecha_liquidacion
         FROM dis_dep_bono
         WHERE folio = g_liq_b.folio

         DISPLAY BY NAME g_liq_b.folio,
                         g_liq_b.fecha_proceso,
                         g_liq_b.fecha_liquidacion
         DISPLAY BY NAME g_liq_b.*

         LET cla_sel = "SELECT 'X' ",
          " FROM  dis_dep_bono ",
          " WHERE folio = ",g_liq_b.folio,
          " AND   estado = 3 ",
          " AND ident_pago[14,15] in ('41','49')",
          " GROUP  BY 1 "
 
         PREPARE claexeb FROM cla_sel
         DECLARE cur1bo CURSOR FOR claexeb
         OPEN cur1bo
         FETCH cur1bo

         IF STATUS <> 100 THEN
            ERROR "La subcuenta de este folio ya esta liquidada"
            SLEEP 3
            CLOSE cur1bo
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur1bo

         LET cla_sel= "SELECT 'X' ",
          " FROM  dis_dep_bono ",
          " WHERE folio = ",g_liq_b.folio,
          " AND   estado = 2 ",
          " AND ident_pago[14,15] in ('41','49')",
          " GROUP  BY 1 "

         PREPARE claexe2b FROM cla_sel
         DECLARE cur2bo CURSOR FOR claexe2b
         OPEN cur2bo
         FETCH cur2bo

         IF STATUS = 100 THEN
            ERROR "La subcuenta de este folio no existe"
            SLEEP 3
            CLOSE cur2bo
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur2bo

         SELECT fecha_recepcion
         INTO  vfecha_recepcion
         FROM  dis_cza_bono
         WHERE folio = g_liq_b.folio

      AFTER FIELD fecha_proceso                   -- parametro3
         IF g_liq_b.fecha_proceso IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_proceso
         END IF
 
      AFTER FIELD fecha_liquidacion               -- parametro4
         IF g_liq_b.fecha_liquidacion IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_liquidacion
 ---     ELSE
 ---        IF g_liq_b.fecha_liquidacion < TODAY THEN
 ---           ERROR " La Fecha de Liquidacion no puede ser MENOR a la Fecha del DIA "
 ---           NEXT FIELD fecha_liquidacion
 ---        END IF
         END IF

         SELECT precio_del_dia
         INTO  g_liq_b.precio_del_dia
         FROM  glo_valor_accion
         WHERE fecha_valuacion = g_liq_b.fecha_liquidacion
         AND   codigo_siefore  = 1                         ----jerry

         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de Accion SIE 1 "
            NEXT FIELD fecha_liquidacion
         END IF

         SELECT precio_del_dia
         INTO  g_liq_b.precio_del_dia
         FROM  glo_valor_accion
         WHERE fecha_valuacion = g_liq_b.fecha_liquidacion
         AND   codigo_siefore  = 2                         ----jerry
 
         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de Accion SIE 2 "
            NEXT FIELD fecha_liquidacion
         END IF

         SELECT precio_del_dia
         INTO  g_liq_b.valor_part
         FROM  glo_valor_accion
         WHERE fecha_valuacion = g_liq_b.fecha_liquidacion
         AND   codigo_siefore  = 12                        ----jerry

         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de la Aiv, SIE 12 "
            NEXT FIELD fecha_liquidacion
         END IF

         LET  g_liq_b.monto_vol = 0

         DISPLAY BY NAME g_liq_b.monto_vol

         LET g_liq_b.valor_part = 1                                       --dmr
         LET g_liq_b.titulos = g_liq_b.gran_tot_dbmx / g_liq_b.valor_part --v3

         IF g_liq_b.titulos IS NULL AND g_liq_b.monto_vol <= 0 THEN
            ERROR "Los titulos por adquirir no pueden ser nulos"
            NEXT FIELD fecha_liquidacion
         END IF

         LET g_liq_b.valor_part = 0                               --dmr TEMPORAL
         DISPLAY BY NAME g_liq_b.*
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
      CALL Ejecuta_liquidacion("B")  # "B"  Bono
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Liquidacion_bono_ant()
   LET INT_FLAG = FALSE

   INPUT BY NAME g_liq_a.*

      BEFORE FIELD folio
         CALL Despliega_saldos_BAI("BONOA")

      AFTER FIELD folio
         IF g_liq_a.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         LET g_liq_a.fecha_proceso = TODAY

         SELECT unique fech_liquidacion
         INTO g_liq_a.fecha_liquidacion
         FROM dis_dep_bono_ant
         WHERE folio = g_liq_a.folio

         DISPLAY BY NAME g_liq_a.folio,
                         g_liq_a.fecha_proceso,
                         g_liq_a.fecha_liquidacion
         DISPLAY BY NAME g_liq_a.*

         LET cla_sel = "SELECT 'X' ",
          " FROM  dis_dep_bono_ant ",
          " WHERE folio = ",g_liq_a.folio,
          " AND   estado = 3 ",
          " AND ident_pago[14,15] = '41'",
          " GROUP  BY 1 "
 
         PREPARE claexeba FROM cla_sel
         DECLARE cur1ba CURSOR FOR claexeba
         OPEN cur1ba
         FETCH cur1ba

         IF STATUS <> 100 THEN
            ERROR "La subcuenta de este folio ya esta liquidada"
            SLEEP 3
            CLOSE cur1ba
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur1ba

         LET cla_sel= "SELECT 'X' ",
          " FROM  dis_dep_bono_ant ",
          " WHERE folio = ",g_liq_a.folio,
          " AND   estado = 2 ",
          " AND ident_pago[14,15] = '41'",
          " GROUP  BY 1 "

         PREPARE claexe2ba FROM cla_sel
         DECLARE cur2ba CURSOR FOR claexe2ba
         OPEN cur2ba
         FETCH cur2ba

         IF STATUS = 100 THEN
            ERROR "La subcuenta de este folio no existe"
            SLEEP 3
            CLOSE cur2ba
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur2ba

         SELECT fecha_recepcion
         INTO  vfecha_recepcion
         FROM  dis_cza_bono_ant
         WHERE folio = g_liq_a.folio

--    AFTER FIELD fecha_proceso                   -- parametro3
         IF g_liq_a.fecha_proceso IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_proceso
         END IF
 
      AFTER FIELD fecha_liquidacion               -- parametro4
         IF g_liq_a.fecha_liquidacion IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_liquidacion
 ---     ELSE
 ---        IF g_liq_a.fecha_liquidacion < TODAY THEN
 ---           ERROR " La Fecha de Liquidacion no puede ser MENOR a la Fecha del DIA "
 ---           NEXT FIELD fecha_liquidacion
 ---        END IF
         END IF

         FOR i=1 TO 5             # Numero de siefores en la afore
            SELECT precio_del_dia
            INTO  g_liq_a.precio_del_dia
            FROM  glo_valor_accion
            WHERE fecha_valuacion = g_liq_a.fecha_liquidacion
            AND   codigo_siefore  = i

            IF STATUS = NOTFOUND THEN
               ERROR " NO Existe Precio de Accion SIE ",i USING "##"
               NEXT FIELD folio
            END IF
         END FOR

         SELECT precio_del_dia
         INTO  g_liq_a.precio_del_dia
         FROM  glo_valor_accion
         WHERE fecha_valuacion = g_liq_a.fecha_liquidacion
         AND   codigo_siefore  = 13

         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de Accion SIE 13 "
            NEXT FIELD folio
         END IF

         LET  g_liq_a.monto_vol = 0

         DISPLAY BY NAME g_liq_a.monto_vol

         LET g_liq_a.valor_part = 1
         LET g_liq_a.titulos = g_liq_a.c_neto_bono_acc

         IF g_liq_a.titulos IS NULL AND g_liq_a.monto_vol <= 0 THEN
            ERROR "Los titulos por adquirir no pueden ser nulos"
            NEXT FIELD folio
         END IF

         LET g_liq_a.a_reti_pes      = g_liq_a.c_bono_pes * (-1)
         LET g_liq_a.a_neto_reti_pes = g_liq_a.c_neto_bono_pes * (-1)
   
         LET g_liq_a.c_bono_pes      = g_liq_a.c_bono_pes
         LET g_liq_a.c_neto_bono_pes = g_liq_a.c_neto_bono_pes

         LET g_liq_a.c_bono_acc      = g_liq_a.c_bono_acc
         LET g_liq_a.c_neto_bono_acc = g_liq_a.c_neto_bono_acc

         DISPLAY BY NAME g_liq_a.*
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
      CALL Ejecuta_liquidacion("A")  # "B"  Bono
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Liquidacion_inttran()
   DEFINE
      precio_dia   LIKE glo_valor_accion.precio_del_dia

   LET INT_FLAG = FALSE

   INPUT BY NAME g_liq_i.*

      BEFORE FIELD folio
         CALL Despliega_saldos_BAI("ITRA")

      AFTER FIELD folio
         IF g_liq_i.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

         LET g_liq_i.fecha_proceso = TODAY
         INITIALIZE g_liq_i.fecha_liquidacion TO NULL

         -- SELECT unique fech_liquidacion
         -- INTO g_liq_i.fecha_liquidacion
         -- FROM dis_dep_inttran
         -- WHERE folio = g_liq_i.folio

         DISPLAY BY NAME g_liq_i.folio,
                         g_liq_i.fecha_proceso,
                         g_liq_i.fecha_liquidacion
         DISPLAY BY NAME g_liq_i.*

         LET cla_sel = "SELECT 'X' ",
          " FROM  dis_dep_inttran ",
          " WHERE folio = ",g_liq_i.folio,
          " AND   estado = 3 ",
          " AND ident_pago[14,15] IN ('41','43','47','51','53','57')",
          " GROUP  BY 1 "
 
         PREPARE claexebi FROM cla_sel
         DECLARE cur1bi CURSOR FOR claexebi
         OPEN cur1bi
         FETCH cur1bi

         IF STATUS <> 100 THEN
            ERROR "La subcuenta de este folio ya esta liquidada"
            SLEEP 3
            CLOSE cur1bi
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur1bi

         LET cla_sel= "SELECT 'X' ",
          " FROM  dis_dep_inttran ",
          " WHERE folio = ",g_liq_i.folio,
          " AND   estado = 2 ",
          " AND ident_pago[14,15] IN ('41','43','47','51','53','57')",
          " GROUP  BY 1 "

         PREPARE claexe2bi FROM cla_sel
         DECLARE cur2bi CURSOR FOR claexe2bi
         OPEN cur2bi
         FETCH cur2bi

         IF STATUS = 100 THEN
            ERROR "La subcuenta de este folio no existe"
            SLEEP 3
            CLOSE cur2bi
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur2bi

         SELECT fecha_recepcion
         INTO  vfecha_recepcion
         FROM  dis_cza_inttran
         WHERE folio = g_liq_i.folio

--    AFTER FIELD fecha_proceso                   -- parametro3
         IF g_liq_i.fecha_proceso IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_proceso
         END IF
 
      AFTER FIELD fecha_liquidacion               -- parametro4
         IF g_liq_i.fecha_liquidacion IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD fecha_liquidacion
 ---     ELSE
 ---        IF g_liq_a.fecha_liquidacion < TODAY THEN
 ---           ERROR " La Fecha de Liquidacion no puede ser MENOR a la Fecha del DIA "
 ---           NEXT FIELD fecha_liquidacion
 ---        END IF
         END IF

         FOR i=1 TO 5             # Numero de siefores en la afore
            SELECT precio_del_dia
            INTO  precio_dia
            FROM  glo_valor_accion
            WHERE fecha_valuacion = g_liq_i.fecha_liquidacion
            AND   codigo_siefore  = i

            IF STATUS = NOTFOUND THEN
               ERROR " NO Existe Precio de Accion SIE ",i USING "##"
               NEXT FIELD folio
            END IF
         END FOR

         SELECT precio_del_dia
         INTO  precio_dia
         FROM  glo_valor_accion
         WHERE fecha_valuacion = g_liq_i.fecha_liquidacion
         AND   codigo_siefore  = 13

         IF STATUS = NOTFOUND THEN
            ERROR " NO Existe Precio de Accion SIE 13 "
            NEXT FIELD folio
         END IF

         DISPLAY BY NAME g_liq_i.*
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
      CALL Ejecuta_liquidacion("I")  # "I"  Intereses Transito
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
      COMMAND "CSOC" "Liquida CUO. SOC."
         LET vsubcuenta = "CSOC"
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

   OPEN WINDOW wsaldo AT 9,2 WITH FORM "DISB000E4" ATTRIBUTE (BORDER)
   DISPLAY "DEPOSITOS PENDIENTES POR LIQUIDAR" AT 1,15
   DISPLAY aporte AT 1,55
   DISPLAY "-----------------------------------------------------------------------------" AT 2,1

   LET tipo_liquida     = " "
   LET tipo_liquida2    = " "

   IF vsubcuenta = "RCVI" THEN
      LET tipo_liquida  = "41"
      LET tipo_liquida2 = "'41','42','43','44','81','82','83','46','47'"
   ELSE
      IF vsubcuenta = "CSOC" THEN
         LET tipo_liquida  = "48"
         LET tipo_liquida2 = "'48'"
      END IF

      IF vsubcuenta = "GAR" THEN
         LET tipo_liquida  = "46"
         LET tipo_liquida2 = "'45','46'"
      END IF
   END IF

   LET vsub             = vsubcuenta

   LET cla_sel = " SELECT folio,",
                      "'",vsub,"'",",",
                        " fecha_archivo ",
                 " FROM   dis_dep_issste ",
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
            WHEN tipo_liquida = "41" 
               CALL Asigna_montos_aporte()
            WHEN tipo_liquida = "46"
               CALL Asigna_montos_aporte()
            WHEN tipo_liquida = "48"
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
      AND   subcuenta IN (3)
      AND   tipo_movimiento = 1
      AND   estado          = 5

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_rcv
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (30,31)
      AND   tipo_movimiento = 1
      AND   estado          = 5

      LET g_liq.aporte_neto_rcv = g_liq.aporte_rcv

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_sar
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (13)
      AND   tipo_movimiento = 1
      AND   estado          = 5

      LET g_liq.aporte_neto_sar = g_liq.aporte_sar

      LET g_liq.aporte_cs = 0 

      LET g_liq.aporte_neto_cs = g_liq.aporte_cs

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_cls
      FROM  dis_provision
      WHERE folio        = g_liq.folio
      AND   subcuenta   IN (11,15,33,34)
      AND   tipo_movimiento = 1
      AND   estado          = 5

      LET g_liq.aporte_neto_cls = g_liq.aporte_cls

      SELECT NVL(sum(monto_en_pesos),0)
      INTO  g_liq.aporte_viv
      FROM  dis_provision
      WHERE folio      = g_liq.folio
      AND   subcuenta IN (14)
      AND   tipo_movimiento = 1
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
      AND   subcuenta IN (14)
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
      IF vsub = "CSOC" THEN
         LET g_liq.monto_vol  = 0
         LET g_liq.aporte_rcv = 0

         LET g_liq.aporte_neto_rcv = g_liq.aporte_rcv

         LET g_liq.aporte_sar = 0

         LET g_liq.aporte_neto_sar = g_liq.aporte_sar

         SELECT NVL(sum(monto_en_pesos),0)
         INTO  g_liq.aporte_cs
         FROM  dis_provision
         WHERE folio      = g_liq.folio
         AND   subcuenta IN (32)
         AND   tipo_movimiento = 1
         AND   estado          = 5

         LET g_liq.aporte_neto_cs = g_liq.aporte_cs

         LET g_liq.aporte_cls = 0

         LET g_liq.aporte_neto_cls = g_liq.aporte_cls

         LET g_liq.aporte_viv = 0

         LET g_liq.aporte_viv_08 = 0

         LET g_liq.aporte_neto_viv = g_liq.aporte_viv + g_liq.aporte_viv_08

         ----------------------------- INTERES ----------------------------

         LET g_liq.interes_rcv = 0

         LET g_liq.interes_neto_rcv = g_liq.interes_rcv

         LET g_liq.interes_sar = 0

         LET g_liq.interes_neto_sar = g_liq.interes_sar

         LET g_liq.interes_cls      = 0
         LET g_liq.interes_neto_cls = 0

         LET g_liq.interes_viv = 0

         LET g_liq.interes_viv_08 = 0

         LET g_liq.interes_neto_viv = g_liq.interes_viv + g_liq.interes_viv_08
      END IF

      IF vsub = "GAR" THEN
         LET g_liq.aporte_rcv = 0
         LET g_liq.aporte_neto_rcv = 0

         LET g_liq.aporte_sar = 0
         LET g_liq.aporte_neto_sar = 0

         LET g_liq.aporte_cs = 0
         LET g_liq.aporte_neto_cs = 0
  
         LET g_liq.aporte_cls = 0
         LET g_liq.aporte_neto_cls = 0

         SELECT NVL(sum(monto_en_pesos),0) 
         INTO  g_liq.aporte_viv
         FROM  dis_provision
         WHERE folio      = g_liq.folio
         AND   subcuenta IN (14)
         AND   tipo_movimiento = 7
         AND   estado          = 5

         SELECT NVL(sum(monto_en_pesos),0) 
         INTO  g_liq.aporte_viv_08
         FROM  dis_provision
         WHERE folio      = g_liq.folio
         AND   subcuenta IN (35)
         AND   tipo_movimiento = 7  
         AND   estado          = 5

         LET g_liq.aporte_neto_viv = g_liq.aporte_viv + g_liq.aporte_viv_08

         LET g_liq.interes_rcv = 0
         LET g_liq.interes_neto_rcv = 0

         LET g_liq.interes_sar = 0
         LET g_liq.interes_neto_sar = 0
 
         LET g_liq.interes_cls = 0
         LET g_liq.interes_neto_cls = 0
        
         LET g_liq.interes_viv = 0
         LET g_liq.interes_neto_viv = 0

         SELECT NVL(sum(monto_en_pesos),0) 
         INTO  g_liq.interes_viv
         FROM  dis_provision
         WHERE folio      = g_liq.folio
         AND   subcuenta IN (14)
         AND   tipo_movimiento = 6
         AND   estado          = 5

         SELECT NVL(sum(monto_en_pesos),0) 
         INTO  g_liq.interes_viv_08
         FROM  dis_provision
         WHERE folio      = g_liq.folio
         AND   subcuenta IN (35)
         AND   tipo_movimiento = 6  
         AND   estado          = 5

         LET g_liq.interes_neto_viv = g_liq.interes_viv + g_liq.interes_viv_08
      END IF
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


FUNCTION Despliega_saldos_BAI(aporte)       # Bono, Anticipado, Intereses (BAI)
   DEFINE
      tipo_liquida2    CHAR(40),
      aporte           CHAR(6),
      xtabla           CHAR(18),
      fecha_liquida    DATE

   OPEN WINDOW wsaldo AT 9,2 WITH FORM "DISB000E4" ATTRIBUTE (BORDER)
   DISPLAY "DEPOSITOS PENDIENTES POR LIQUIDAR" AT 1,15
   DISPLAY aporte AT 1,55
   DISPLAY "-----------------------------------------------------------------------------" AT 2,1

   LET tipo_liquida  = " "
   LET tipo_liquida2 = " "

   CASE aporte
      WHEN "BONOA"
         LET tipo_liquida  = "49"
         LET tipo_liquida2 = "41"
         LET xtabla        = "dis_dep_bono_ant"
         LET vsub          = vsubcuenta

      WHEN "BONO"
         LET tipo_liquida  = "41"
         LET tipo_liquida2 = "'41','49'"
         LET vsubcuenta    = "BONO"
         LET xtabla        = "dis_dep_bono"
         LET vsub          = vsubcuenta
    
      WHEN "ITRA"
         LET tipo_liquida  = "41"
         LET tipo_liquida2 = "'41','43','47','51','53','57'"
         LET vsubcuenta    = "ITRA"
         LET xtabla        = "dis_dep_inttran"
         LET vsub          = vsubcuenta
   END CASE

   LET cla_sel = " SELECT folio,",
                      "'",vsub,"'",",",
                        " fecha_archivo ",
                 " FROM ",xtabla,
                 " WHERE  ident_pago[14,15] IN (",tipo_liquida2 CLIPPED,")",
                 " AND    estado = 2 ",
                 " GROUP  BY 1,2,3 " CLIPPED,    --c22-6
                 " ORDER  BY 1,2 " CLIPPED

   PREPARE claexe3b FROM cla_sel
   DECLARE cur_saldob CURSOR FOR claexe3b

   LET i = 1

   FOREACH cur_saldob INTO g_sal[i].*
      LET i = i + 1
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
         CASE aporte
            WHEN "BONOA"  
               LET g_liq_a.folio = g_sal[arr_c].folio 
               CALL Asigna_montos_aporte_bono_ant()
            WHEN "BONO"
               LET g_liq_b.folio = g_sal[arr_c].folio 
               CALL Asigna_montos_aporte_bono()
            WHEN "ITRA"
               LET g_liq_i.folio = g_sal[arr_c].folio 
               CALL Asigna_montos_inttran()
         END CASE 
         EXIT INPUT
   END INPUT

   CLOSE WINDOW wsaldo
END FUNCTION


FUNCTION Asigna_montos_aporte_bono_ant()

-------------------------------- APORTES -----------------------------------

   LET g_liq_a.monto_vol = 0

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_a.c_bono_pes
   FROM  dis_provision
   WHERE folio           = g_liq_a.folio
   AND   subcuenta       = 36
   AND   tipo_movimiento = 27
   AND   estado          = 5

   LET g_liq_a.c_neto_bono_pes = g_liq_a.c_bono_pes

   SELECT NVL(sum(monto_en_acciones),0)
   INTO  g_liq_a.c_bono_acc
   FROM  dis_provision
   WHERE folio           = g_liq_a.folio
   AND   subcuenta       = 36
   AND   tipo_movimiento = 27
   AND   estado          = 5

   LET g_liq_a.c_neto_bono_acc = g_liq_a.c_bono_acc

-------------------------------- CARGOS -----------------------------------

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_a.a_reti_pes
   FROM  dis_provision
   WHERE folio           = g_liq_a.folio
   AND   subcuenta       = 30
   AND   tipo_movimiento = 33
   AND   estado          = 5

   LET g_liq_a.a_neto_reti_pes = g_liq_a.a_reti_pes
END FUNCTION


FUNCTION Asigna_montos_aporte_bono()

-------------------------------- APORTES -----------------------------------

   LET g_liq_b.monto_vol = 0

   SELECT NVL(sum(monto_en_acciones),0)
   INTO  g_liq_b.aporte_bono
   FROM  dis_provision
   WHERE folio           = g_liq_b.folio
   AND   subcuenta      IN (36)
   AND   tipo_movimiento = 17
   AND   estado          = 5

   LET g_liq_b.aporte_neto_bono = g_liq_b.aporte_bono

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_b.aporte_dbmx
   FROM  dis_provision
   WHERE folio           = g_liq_b.folio
   AND   subcuenta      IN (30)
   AND   tipo_movimiento = 17
   AND   estado          = 5

   LET g_liq_b.aporte_neto_dbmx = g_liq_b.aporte_dbmx


-------------------------------- INTERES -----------------------------------

   LET g_liq_b.interes_bono = 0 

   LET g_liq_b.interes_neto_bono = g_liq_b.interes_bono

   LET g_liq_b.interes_dbmx = 0 

   LET g_liq_b.interes_neto_dbmx = g_liq_b.interes_dbmx


-------------------------------- TOTALES -----------------------------------

   LET g_liq_b.gran_tot_bono = g_liq_b.aporte_bono  +  g_liq_b.interes_bono

   LET g_liq_b.gran_tot_dbmx = g_liq_b.aporte_dbmx  +  g_liq_b.interes_dbmx

   LET g_liq_b.gran_tot_net_bono = g_liq_b.aporte_neto_bono +
                                   g_liq_b.interes_neto_bono

   LET g_liq_b.gran_tot_net_dbmx = g_liq_b.aporte_neto_dbmx +
                                   g_liq_b.interes_neto_dbmx
END FUNCTION


FUNCTION Asigna_montos_inttran()

-------------------------------- APORTES -----------------------------------

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_ret
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (30)
   AND   tipo_movimiento = 1
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_ret_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (30)
   AND   tipo_movimiento = 3
   AND   estado          = 5

   LET g_liq_i.aporte_ret_neto = g_liq_i.aporte_ret + g_liq_i.aporte_ret_ex

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_cv
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (31)
   AND   tipo_movimiento = 1
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_cv_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (31)
   AND   tipo_movimiento = 3
   AND   estado          = 5

   LET g_liq_i.aporte_cv_neto = g_liq_i.aporte_cv + g_liq_i.aporte_cv_ex

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_sar
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (13,19)
   AND   tipo_movimiento = 1
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_sar_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (13,19)
   AND   tipo_movimiento = 3
   AND   estado          = 5

   LET g_liq_i.aporte_sar_neto = g_liq_i.aporte_sar + g_liq_i.aporte_sar_ex

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_aht
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (33)
   AND   tipo_movimiento = 1
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_aht_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (33)
   AND   tipo_movimiento = 3
   AND   estado          = 5

   LET g_liq_i.aporte_aht_neto = g_liq_i.aporte_aht + g_liq_i.aporte_aht_ex

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_ahd
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (34)
   AND   tipo_movimiento = 1
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.aporte_ahd_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (34)
   AND   tipo_movimiento = 3
   AND   estado          = 5

   LET g_liq_i.aporte_ahd_neto = g_liq_i.aporte_ahd + g_liq_i.aporte_ahd_ex


-------------------------------- INTERES TRANSITO -----------------------------

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_ret
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (30)
   AND   tipo_movimiento = 2
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_ret_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (30)
   AND   tipo_movimiento = 4
   AND   estado          = 5

   LET g_liq_i.itra_ret_neto = g_liq_i.itra_ret + g_liq_i.itra_ret_ex

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_cv
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (31)
   AND   tipo_movimiento = 2
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_cv_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (31)
   AND   tipo_movimiento = 4
   AND   estado          = 5

   LET g_liq_i.itra_cv_neto = g_liq_i.itra_cv + g_liq_i.itra_cv_ex

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_sar
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (13,19)
   AND   tipo_movimiento = 2
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_sar_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (13,19)
   AND   tipo_movimiento = 4
   AND   estado          = 5

   LET g_liq_i.itra_sar_neto = g_liq_i.itra_sar + g_liq_i.itra_sar_ex

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_aht
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (33)
   AND   tipo_movimiento = 2
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_aht_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (33)
   AND   tipo_movimiento = 4
   AND   estado          = 5

   LET g_liq_i.itra_aht_neto = g_liq_i.itra_aht + g_liq_i.itra_aht_ex

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_ahd
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (34)
   AND   tipo_movimiento = 2
   AND   estado          = 5

   SELECT NVL(sum(monto_en_pesos),0)
   INTO  g_liq_i.itra_ahd_ex
   FROM  dis_provision
   WHERE folio           = g_liq_i.folio
   AND   subcuenta      IN (34)
   AND   tipo_movimiento = 4
   AND   estado          = 5

   LET g_liq_i.itra_ahd_neto = g_liq_i.itra_ahd + g_liq_i.itra_ahd_ex


-------------------------------- TOTALES -----------------------------------

   LET g_liq_i.tot_rcv     = g_liq_i.aporte_ret    + g_liq_i.aporte_cv +
                             g_liq_i.itra_ret      + g_liq_i.itra_cv

   LET g_liq_i.tot_rcv_ex  = g_liq_i.aporte_ret_ex + g_liq_i.aporte_cv_ex +
                             g_liq_i.itra_ret_ex   + g_liq_i.itra_cv_ex

   LET g_liq_i.tot_rcv_neto= g_liq_i.aporte_ret_neto + g_liq_i.aporte_cv_neto +
                             g_liq_i.itra_ret_neto   + g_liq_i.itra_cv_neto

   LET g_liq_i.tot_sar     = g_liq_i.aporte_sar    + g_liq_i.itra_sar

   LET g_liq_i.tot_sar_ex  = g_liq_i.aporte_sar_ex + g_liq_i.itra_sar_ex

   LET g_liq_i.tot_sar_neto= g_liq_i.aporte_sar_neto + g_liq_i.itra_sar_neto

   LET g_liq_i.tot_aho     = g_liq_i.aporte_aht    + g_liq_i.aporte_ahd +
                             g_liq_i.itra_aht      + g_liq_i.itra_ahd

   LET g_liq_i.tot_aho_ex  = g_liq_i.aporte_aht_ex + g_liq_i.aporte_ahd_ex +
                             g_liq_i.itra_aht_ex   + g_liq_i.itra_ahd_ex

   LET g_liq_i.tot_aho_neto= g_liq_i.aporte_aht_neto + g_liq_i.aporte_ahd_neto +
                             g_liq_i.itra_aht_neto   + g_liq_i.itra_ahd_neto

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
          "DISB007E",              -- proceso_cod
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
         LET ejecuta = "nohup time fglgo DISB007E.4gi 0 0 0 ",
                              g_liq.folio CLIPPED," ",
                              vtipo, " ",
                              vsubcuenta," ",
                              g_liq.fecha_proceso," ",
                              g_liq.fecha_liquidacion," &"

         RUN ejecuta

      WHEN tipo_pro = "B"
         INSERT INTO dis_ctrl_proceso VALUES 
         (TODAY,                     -- fecha_proceso
          "DISB007P",                -- proceso_cod
          2,                         -- etapa_cod     -- LIQUIDACION
          hora_inicial,              -- hora_inicial
          hora_final,                -- hora_final
          g_liq_b.folio,             -- parametro1
          vtipo,                     -- parametro2
          vsubcuenta,                -- parametro3
          g_liq_b.fecha_proceso,     -- parametro4
          g_liq_b.fecha_liquidacion, -- parametro5
          g_liq_b.folio,             -- folio
          "PROCESANDO",              -- resultado
          usuario,                   -- usuario 
          0)

         IF STATUS < 0 THEN
            ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso Liquidacion ",STATUS
            SLEEP 4
            EXIT PROGRAM
         END IF

         ERROR "Ejecutando Proceso de Liquidacion por nohup ..."
         SLEEP 3
         LET ejecuta = "nohup time fglgo DISB007P.4gi 0 0 0 ",
                              g_liq_b.folio CLIPPED," ",
                              vtipo, " ",
                              vsubcuenta," ",
                              g_liq_b.fecha_proceso," ",
                              g_liq_b.fecha_liquidacion," &"

         RUN ejecuta

      WHEN tipo_pro = "A"
         INSERT INTO dis_ctrl_proceso VALUES 
         (TODAY,                     -- fecha_proceso
          "DISB007A",                -- proceso_cod
          2,                         -- etapa_cod     -- LIQUIDACION
          hora_inicial,              -- hora_inicial
          hora_final,                -- hora_final
          g_liq_a.folio,             -- parametro1
          vtipo,                     -- parametro2
          vsubcuenta,                -- parametro3
          g_liq_a.fecha_proceso,     -- parametro4
          g_liq_a.fecha_liquidacion, -- parametro5
          g_liq_a.folio,             -- folio
          "PROCESANDO",              -- resultado
          usuario,                   -- usuario 
          0)

         IF STATUS < 0 THEN
            ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso Liquidacion ",STATUS
            SLEEP 4
            EXIT PROGRAM
         END IF

         ERROR "Ejecutando Proceso de Liquidacion por nohup ..."
         SLEEP 3
         LET ejecuta = "nohup time fglgo DISB007A.4gi 0 0 0 ",
                              g_liq_a.folio CLIPPED," ",
                              vtipo, " ",
                              vsubcuenta," ",
                              g_liq_a.fecha_proceso," ",
                              g_liq_a.fecha_liquidacion," &"

         RUN ejecuta

      WHEN tipo_pro = "I"
         INSERT INTO dis_ctrl_proceso VALUES 
         (TODAY,                     -- fecha_proceso
          "DISB007I",                -- proceso_cod
          2,                         -- etapa_cod     -- LIQUIDACION
          hora_inicial,              -- hora_inicial
          hora_final,                -- hora_final
          g_liq_i.folio,             -- parametro1
          vtipo,                     -- parametro2
          vsubcuenta,                -- parametro3
          g_liq_i.fecha_proceso,     -- parametro4
          g_liq_i.fecha_liquidacion, -- parametro5
          g_liq_i.folio,             -- folio
          "PROCESANDO",              -- resultado
          usuario,                   -- usuario 
          0)

         IF STATUS < 0 THEN
            ERROR "ERROR AL INSERTAR EN TABLA dis_ctrl_proceso Liquidacion ",STATUS
            SLEEP 4
            EXIT PROGRAM
         END IF

         ERROR "Ejecutando Proceso de Liquidacion por nohup ..."
         SLEEP 3
         LET ejecuta = "nohup time fglgo DISB007I.4gi 0 0 0 ",
                              g_liq_i.folio CLIPPED," ",
                              vtipo, " ",
                              vsubcuenta," ",
                              g_liq_i.fecha_proceso," ",
                              g_liq_i.fecha_liquidacion," &"

         RUN ejecuta
   END CASE

   ERROR "El Proceso se Ejecuto Satisfactoriamente por nohup"
   SLEEP 3
   ERROR ""
END FUNCTION


-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB000D                                                  --
-- Descripcion  => PROCESO BATCH DISPERSION Y LIBERACION DE PANTALLA         --
-- Sistema      => DIS                                                       --
-- Por          => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 7 julio 2001.                                             --
-- Modificado   => GERARDO ALFONSO VEGA PAREDES                              --
-- Fecha        => 30 octubre 2001.                                          --
-------------------------------------------------------------------------------
-- Modi.        => Alejandro Ramirez 12 Ago 2005                             --
-- Descr.       => (v2) En la pantalla de liquida los import ya suman la     --
--              => comision tanto de rcv como est                            --
-------------------------------------------------------------------------------
-- Modi.        => Alejandro Ramirez 12 Ago 2005                             --
-- Descr.       => (v3) En la pantalla de liquida los montos de 43bis ya     --
--              => se estan dividiendo con el valor de las acciones          --
-------------------------------------------------------------------------------
-- Modi.        => Alejandro Ramirez 12 Ago 2005                             --
-- Descr.       => (v4) Se agrega la opcion de liquidacion pago 13 (Remanen) --
-------------------------------------------------------------------------------
-- Modi.        => Alejandro Ramirez 11 Nov 2005                             --
-- Descr.       => (v5) Se redondean las paticipaciones a 2 decimales        --
-------------------------------------------------------------------------------
-- Modi.        => Alejandro Ramirez 23 Jun 2006                             --
-- Descr.       => (c22-11) Nueva circular.                                  --
-------------------------------------------------------------------------------
-- Modi.        => DMR 26 mayo 2010                                          --
-- Descr.       => Se modifico para recibir archivos solo con rem y ext ya   --
--              => que las cuentas por cobrar se manejaba con 43             --
-------------------------------------------------------------------------------
-- Modi.        => DMR 17 Enero 2013                                         --
-- Descr.       => Se modifico para estandarizar version en las afores y para--
--              => liquidar por separado RCV y EST por normativa que salio.  --
-- Modi.        => DMR 15 Febrero 2013                                       --
-- Descr.       => Estandarizacion de version afores, SQLCA.SQLCODE y Liq sin--
--              => prioridad en prog liq, se aplica en las rutinas de liq    --
--              => liq_apor_est_esp y liq_apor_rcv_esp                       --
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS
   DEFINE 
      hoy                DATE,
      usuario            CHAR(08),
      opc                CHAR(01),
      ejecuta            CHAR(200),
      comando            CHAR(200),
      hora_inicial       CHAR(08),
      hora_final         CHAR(08),
      aux_subct_desc     CHAR(03),
      hist_fecha_aplica  DATE,
      vsubcuenta         CHAR(06),
      vsub               CHAR(06),
      vtipo              CHAR(01),
      vtipo_liquidacion  CHAR(01),
      vfecha_recepcion   DATE,
      vrecauda           CHAR(01)

   DEFINE
      vrecaudax          SMALLINT,
      vfolio             INTEGER

   DEFINE
      t_v,
      tipo_liquida,
      tipo_comision      CHAR(2)

   DEFINE
      importe_liquida,
      importe_comision,
      total_accion,
      totala,
      totalc,
      v43bis,
      importe_total      DECIMAL(16,6)

   DEFINE
      i                  SMALLINT

   DEFINE
      arr_c              SMALLINT,
      arr_l              SMALLINT

   DEFINE
      cla_sel            CHAR(350)

   DEFINE
      g_param  RECORD LIKE seg_modulo.*

   DEFINE
      g_reg    RECORD
                  nombre_archivo     CHAR(15)
               END RECORD

   DEFINE
      g_liq    RECORD
                  folio              INTEGER,
                  fecha_proceso      DATE,
                  fecha_acreditacion DATE,
                  precio_del_dia     DECIMAL(16,6), 
                  aporte_rcv         DECIMAL(16,6),
                  comision_rcv       DECIMAL(16,6),
                  aporte_neto_rcv    DECIMAL(16,6),
                  aporte_est         DECIMAL(16,6),
                  comision_est       DECIMAL(16,6),
                  aporte_neto_est    DECIMAL(16,6),
                  aporte_viv         DECIMAL(16,6),
                  aporte_neto_viv    DECIMAL(16,6),
                  interes_rcv        DECIMAL(16,6),
                  interes_neto_rcv   DECIMAL(16,6),
                  interes_est        DECIMAL(16,6),
                  interes_neto_est   DECIMAL(16,6),
                  interes_viv        DECIMAL(16,6),
                  interes_neto_viv   DECIMAL(16,6),
                  total_rcv          DECIMAL(16,6),
                  total_neto_rcv     DECIMAL(16,6),
                  total_est          DECIMAL(16,6),
                  total_neto_est     DECIMAL(16,6),
                  gran_tot1          DECIMAL(16,6),
                  gran_tot_com       DECIMAL(16,6),
                  gran_tot_neto1     DECIMAL(16,6),
                  gran_tot2          DECIMAL(16,6),
                  gran_tot_neto2     DECIMAL(16,6),
                  titulos            DECIMAL(16,6),
                  valor_part         DECIMAL(16,6)
               END RECORD

   DEFINE
      g_sal ARRAY[500] OF RECORD
                             folio         INTEGER,
                             sub           CHAR(06),
                             fecha_archivo DATE
                          END RECORD
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("DISB000D.log")

   SELECT *, USER
   INTO  g_param.*, 
         usuario
   FROM  seg_modulo  
   WHERE modulo_cod = "dis"

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "DISB000D1" ATTRIBUTE(BORDER)
   DISPLAY " DISB000D           DISPERSION LIBERACION TERMINAL IMSS                       " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "    LECTURA DE ARCHIVOS, HISTORICOS, INDIVIDUALIZACION Y GENERACION CONFAF      " AT 5,1 ATTRIBUTE(REVERSE) 
              
   MENU "PROCESOS BATCH" 
      COMMAND "Carga" "Carga Archivo Dispersion"
         CALL Carga()
      COMMAND "Liquidaciones" "Liquidacion RCV, VIV, EST y 43 Bis"
         CALL Liquidacion()
      COMMAND "Reverso Disp." "Reverso de Archivo de Dispersion IMSS"
         CALL Reverso_normal("RCV")
      COMMAND "Salir" "Salida del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN


FUNCTION Reverso_normal(tipo_rev)
   DEFINE
      cont        INTEGER,
      band        SMALLINT,
      tipo_carga  CHAR(1),
      fol_rev     INTEGER,
      tipo_rev    CHAR(4)

   OPEN WINDOW ventana1r AT 2,2 WITH FORM "DISB000D1" ATTRIBUTE(BORDER)
   DISPLAY " DISB000D     REVERSO DE DISPERSION NORMAL IMSS RCV, EST Y VIV                     " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "    REVERSO DE ARCHIVO, HISTORICOS, INDIVIDUALIZACION Y GENERACION CONFAC          " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.nombre_archivo

      AFTER FIELD nombre_archivo
         IF g_reg.nombre_archivo IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO "
            NEXT FIELD nombre_archivo
         ELSE
            SELECT UNIQUE folio
            INTO  fol_rev
            FROM  dis_ctrl_proceso
            WHERE parametro1  = g_reg.nombre_archivo
            AND   proceso_cod = "DISB2001B"
            AND   (folio IS NOT NULL AND folio > 0 )

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
               SELECT "OK"
               FROM  dis_cza_aporte
               WHERE folio = fol_rev

               IF SQLCA.SQLCODE = 100 THEN
                  ERROR "ARCHIVO INEXISTENTE COMO PROCESO DE DISPERSION IMSS..."
                  NEXT FIELD nombre_archivo
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
         ERROR " REVERSANDO INFORMACION ..."
         CALL Ejecuta_reverso(fol_rev,tipo_rev,g_reg.nombre_archivo)
      END IF
   END IF

   DISPLAY  "                                    " AT 14,15
   CLOSE WINDOW ventana1r
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Carga()
   DEFINE
      cont     SMALLINT,
      band     SMALLINT

   OPEN WINDOW v1 AT 12,20 WITH 01 ROWS, 37 COLUMNS ATTRIBUTE(BORDER)
   MENU "Recaudacion"
      COMMAND "Normal"
         LET vrecauda = "N"
         LET vrecaudax = 1
         EXIT MENU
      COMMAND "Especial"
         LET vrecauda = "E"
         LET vrecaudax = 2
         EXIT MENU
   END MENU
   CLOSE WINDOW v1

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg.*

      AFTER FIELD nombre_archivo 
         IF g_reg.nombre_archivo IS NULL THEN
            ERROR "ESTE CAMPO NO PUEDE SER NULO"
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
      FROM safre_tmp:archivos_dis_bat
      WHERE campo = g_reg.nombre_archivo

      IF SQLCA.SQLCODE = 100 THEN
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

   LET band = 0

   IF band = 0 THEN
      SELECT max(folio)
      INTO vfolio
      FROM glo_folio

      LET vfolio = vfolio + 1

      DISPLAY "   FOLIO A PROCESAR : ",vfolio AT 14,15

      PROMPT "Desea ejecutar el proceso [S/N] ..." FOR opc

      IF opc MATCHES '[Ss]' THEN
         ERROR "PROCESANDO INFORMACION ..."
         CALL Ejecuta_lectura()
      END IF
   END IF

   DISPLAY "                                    " AT 14,15
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_lectura()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB2001B",             -- proceso_cod
       1,                       -- etapa_cod   -- LECTURA
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_reg.nombre_archivo,    -- parametro1
       vrecauda,                -- parametro2
       NULL,                    -- parametro3
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
        
   ERROR "EJECUTANDO LECTURA DE ARCHIVO POR NOHUP ..."
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB2001B.4gi 0 0 0 ",
                  g_reg.nombre_archivo CLIPPED," ",
                  vrecaudax," ",
                  today," ",
                  " &"
   RUN ejecuta
   ERROR "EL PROCESO SE EJECUTO SATISFACTORIAMENTE POR NOHUP"
   SLEEP 3
   ERROR ""
END FUNCTION


FUNCTION Ejecuta_reverso(folio_rev,tipo_rev,nom_archivo)
   DEFINE
      folio_rev    INTEGER,
      vrow         INTEGER,
      cla_upd      CHAR(1500),
      nom_archivo  CHAR(15),
      tipo_rev     CHAR(4)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB2001B",             -- proceso_cod
       1,                       -- etapa_cod   -- LECTURA
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       vfolio,                  -- parametro1
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

   DELETE FROM dis_cza_aporte
   WHERE folio = folio_rev

   DELETE FROM dis_det_aporte
   WHERE folio = folio_rev

   DELETE FROM dis_det_interes
   WHERE folio = folio_rev

   DELETE FROM dis_dep_aporte
   WHERE folio = folio_rev

   DELETE FROM dis_sum_aporte
   WHERE folio = folio_rev

   SELECT UNIQUE(parametro1)
   INTO   nom_archivo
   FROM   dis_ctrl_proceso
   WHERE  folio = folio_rev
   AND    proceso_cod = "DISB2001B"

   DELETE FROM dis_ctrl_proceso
   WHERE  parametro1  = nom_archivo
   AND    proceso_cod = "DISB2001B"

   LET hora_final = TIME

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 "FROM dis_ctrl_proceso ",
                 "WHERE proceso_cod = 'DISB2001B' ",
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
          " WHERE dis_ctrl_proceso.proceso_cod = 'DISB2001B' ",
          " AND   dis_ctrl_proceso.etapa_cod   = 1 ",
          " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED

   PREPARE claexe11i FROM cla_upd
   EXECUTE claexe11i

   ERROR "El FOLIO FUE REVERSADO Exitosamente ..."
   SLEEP 3
   ERROR ""
END FUNCTION


FUNCTION Liquidacion()
   OPEN WINDOW vent10 AT 2,2 WITH FORM "DISB000D2" ATTRIBUTE(BORDER)
   DISPLAY " DISB000D              LIQUIDACION LIBERACION TERMINAL                         " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

   DISPLAY "          APORTACIONES              COMISION             TOTAL LIQUIDACION         " AT 7,1 ATTRIBUTE(REVERSE)

   DISPLAY "           INTERESES                                     TOTAL LIQUIDACION         " AT 11,1 ATTRIBUTE(REVERSE)

   DISPLAY "             TOTAL                                       TOTAL LIQUIDACION         " AT 15,1 ATTRIBUTE(REVERSE)

   MENU "Liquidacion" 
      COMMAND "Normal" "Liquidacion Normal"
         LET  vtipo = "A"
         CALL Subcuenta()
         CALL Liquidacion_normal()
      COMMAND "Aclaraciones especiales" "Liquidacion Aclaraciones Especiales"
         LET  vtipo = "I"
         CALL Subcuenta()
         CALL Liquidacion_aclaracion()
      COMMAND "43 Bis" "Liquidacion Cargos 43 bis"
         LET  vtipo      = "C"
         LET  vsubcuenta = "GAR"
         CALL Liquidacion_normal()
      COMMAND "Salir" "Salida"
         EXIT MENU
   END MENU
   CLOSE WINDOW vent10
END FUNCTION


FUNCTION Liquidacion_normal()
   LET INT_FLAG = FALSE

   INPUT BY NAME g_liq.*

      BEFORE FIELD folio
--         LET vtipo = "A"
         CALL Despliega_saldos(vtipo,vsubcuenta)

      AFTER FIELD folio
         LET g_liq.fecha_proceso      = TODAY
         LET g_liq.fecha_acreditacion = TODAY

         DISPLAY BY NAME g_liq.folio,
                         g_liq.fecha_proceso,
                         g_liq.fecha_acreditacion
         DISPLAY BY NAME g_liq.*

         IF g_liq.folio IS NULL THEN        
            ERROR "ESTE CAMPO NO PUEDE SER NULO"
            NEXT FIELD folio
         END IF                                 

         CASE 
            WHEN vsubcuenta = "RCV" OR vsubcuenta = "VIV" AND vtipo = "A"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 3 ",
           "AND ident_pago[14,15] IN ('41','43','45','46','47','48','49','40')",
                            " GROUP BY 1 "

            WHEN vsubcuenta = "EST" AND vtipo = "A"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 3 ",
                             " AND   ident_pago[14,15] = '42' ",
                             " GROUP BY 1 "

            WHEN vsubcuenta = "GAR" AND vtipo = "C"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 3 ",
                             " AND   ident_pago[14,15] = '44' ",
                             " GROUP BY 1 "
   
         END CASE
      
         PREPARE claexe FROM cla_sel
         DECLARE cur1 CURSOR FOR claexe
         OPEN cur1
         FETCH cur1

         IF SQLCA.SQLCODE = 0 THEN
            ERROR "LAS SUBCUENTAS DE ESTE FOLIO YA ESTAN LIQUIDADAS !!!"
            SLEEP 3
            CLOSE cur1
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur1

         CASE
            WHEN vsubcuenta = "RCV" OR vsubcuenta = "VIV" AND vtipo = "A"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 2 ",
           "AND ident_pago[14,15] IN ('41','43','45','46','47','48','49','40')",
                             " GROUP BY 1 "

            WHEN vsubcuenta = "EST" AND vtipo = "A"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 2 ",
                             " AND   ident_pago[14,15] = '42' ",
                             " GROUP BY 1 "

            WHEN vsubcuenta = "GAR" AND vtipo = "C"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 2 ",
                             " AND   ident_pago[14,15] = '44' ",
                             " GROUP BY 1 "
         END CASE

         PREPARE claexe2 FROM cla_sel
         DECLARE cur2 CURSOR FOR claexe2
         OPEN cur2
         FETCH cur2

         IF SQLCA.SQLCODE = 100 THEN
            ERROR " LAS SUBCUENTAS DE ESTE FOLIO NO EXISTEN !!!"
            SLEEP 3
            CLOSE cur2
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur2

         SELECT fecha_recepcion
         INTO  vfecha_recepcion
         FROM  dis_cza_aporte
         WHERE folio = g_liq.folio

      AFTER FIELD fecha_proceso                  -- parametro3
         IF g_liq.fecha_proceso IS NULL THEN        
            ERROR "ESTE CAMPO NO PUEDE SER NULO !!!"
            NEXT FIELD fecha_proceso
         END IF                                 
--         IF g_liq.fecha_proceso < vfecha_recepcion THEN
--            ERROR "LA FECHA PROCESO DEBE SER MAYOR AL ... ", 
--            vfecha_recepcion USING 'DD-MM-YYYY ' 
--            NEXT FIELD fecha_proceso
--         END IF

      AFTER FIELD fecha_acreditacion              -- parametro4
         IF g_liq.fecha_acreditacion IS NULL THEN        
            ERROR "ESTE CAMPO NO PUEDE SER NULO !!!"
            NEXT FIELD fecha_acreditacion
         END IF                                 
     
         IF vsubcuenta <> 'GAR' AND vsubcuenta <> 'VIV' THEN          --v4

            ------VALIDACION PREVIA
            FOR i = 1 TO 4
               SELECT "X"                                             --c22-6.12
               FROM  glo_valor_accion                                 --c22-6.12
               WHERE fecha_valuacion = g_liq.fecha_acreditacion       --c22-6.12
               AND   codigo_siefore = i                               --c22-6.12
               AND   precio_del_dia > 0                               --c22-6.12
               GROUP BY 1                                             --c22-6.12

               IF SQLCA.SQLCODE = 100 THEN                              --c22-6.12
                  ERROR "NO EXISTE PRECIO ACCION PARA ESTA FECHA, SIE ",i," !!"
                  NEXT FIELD fecha_acreditacion                       --c22-6.12
               END IF                                                 --c22-6.12
            END FOR

            CALL Despliega_valores(g_liq.folio,g_liq.fecha_acreditacion,
                                   vsubcuenta)                        --c22-6.12
            RETURNING INT_FLAG,g_liq.titulos                          --c22-6.12

            SELECT precio_del_dia                                     --c22-6.12
            INTO  g_liq.valor_part                                    --c22-6.12
            FROM  glo_valor_accion                                    --c22-6.12
            WHERE fecha_valuacion = g_liq.fecha_acreditacion          --c22-6.12
            AND   codigo_siefore  = 11                                --c22-6.12
         ELSE                                                         --c22-6.12
            SELECT precio_del_dia
            INTO  g_liq.precio_del_dia
            FROM  glo_valor_accion
            WHERE fecha_valuacion = g_liq.fecha_acreditacion
            AND   codigo_siefore  = 1

            SELECT precio_del_dia
            INTO  g_liq.valor_part
            FROM  glo_valor_accion
            WHERE fecha_valuacion = g_liq.fecha_acreditacion
            AND   codigo_siefore  = 11

--             LET g_liq.titulos = g_liq.gran_tot1 / g_liq.precio_del_dia   --v3
            LET g_liq.titulos = g_liq.gran_tot1 / g_liq.valor_part          --v3

            IF g_liq.titulos IS NULL THEN
               ERROR "LOS TITULOS POR ADQUIRIR NO PUEDEN SER NULOS !!!"
--                NEXT FIELD fecha_acreditacion
            END IF
 
            IF vsub <> "REMVIV" THEN
               LET g_liq.aporte_viv = g_liq.aporte_viv / g_liq.valor_part
               LET g_liq.aporte_neto_viv=g_liq.aporte_neto_viv/g_liq.valor_part
               LET g_liq.gran_tot2 = g_liq.gran_tot2 / g_liq.valor_part
               LET g_liq.gran_tot_neto2 = g_liq.gran_tot_neto2/g_liq.valor_part
            END IF
         END IF
           
         DISPLAY BY NAME g_liq.*

--         IF g_liq.fecha_acreditacion < vfecha_recepcion THEN
--            ERROR "LA FECHA LIQUIDACION DEBE SER MAYOR AL ... ", 
--            vfecha_recepcion USING 'DD-MM-YYYY ' 
--            NEXT FIELD fecha_acreditacion
--         END IF

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
      CALL Ejecuta_liquidacion()
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Despliega_valores(vfolio,vfecha_acreditacion,vsub)  --c22-6.12
   DEFINE
      vfolio              INTEGER,
      vfecha_acreditacion DATE,
      vsub                CHAR(06),
      aux_sub             CHAR(50),
      total_tit           DECIMAL(16,6)

   DEFINE
      opc                 SMALLINT,
      cod                 DECIMAL(10,0),
      desc                CHAR(60),
      pos                 SMALLINT,
      x_buscar            SMALLINT,  
      x_texto             CHAR(300),
      vtot_titulo         DECIMAL(16,6)

   DEFINE
      l_reg ARRAY[500] OF RECORD
                             tipo_siefore      SMALLINT,
                             subcuenta         SMALLINT,
                             impt_aport_acept  DECIMAL(15,2),
                             precio_del_dia    DECIMAL(16,6),
                             titulos           DECIMAL(16,6)
                          END RECORD

   CASE
{     WHEN vsub = "TODAS"
         LET aux_sub  = "('41','42','45','46','49','40','21','22')" #DMR 220113}
      WHEN vsub = "EST"
         LET aux_sub  = "('42','22')"
      WHEN vsub = "RCV"
         LET aux_sub = "('41','45','46','49','40','21','43')"        --c22-11
      WHEN vsub = "VIV"                                              --v4
         LET aux_sub = "('43','47','48')"                            --c22-11
      OTHERWISE
         LET aux_sub = "('')"
   END CASE

   OPEN WINDOW ventana_2 AT 7,6 WITH FORM "DISB000D3" ATTRIBUTE(BORDER)
   DISPLAY "                         VALORES                                      " AT 2,1 ATTRIBUTE(REVERSE)

   LET vtot_titulo = 0
   LET INT_FLAG    = FALSE

   WHILE TRUE
      LET x_texto = " SELECT tipo_siefore,ident_pago[14,15],impt_aport_acept ",
                    " FROM dis_dep_aporte WHERE folio =",vfolio CLIPPED,
                    " AND  ident_pago[14,15] IN ",aux_sub CLIPPED,
                    " AND  estado = 2 ",
              --    " AND  tipo_siefore > 0 ",
                    " ORDER BY 1,2 "

      PREPARE cur1001a FROM x_texto
      DECLARE cur_8a CURSOR FOR cur1001a

      LET pos = 1

      FOREACH cur_8a INTO l_reg[pos].*
         SELECT precio_del_dia
         INTO  l_reg[pos].precio_del_dia
         FROM  glo_valor_accion
         WHERE fecha_valuacion = vfecha_acreditacion
         AND   codigo_siefore  = l_reg[pos].tipo_siefore

         IF l_reg[pos].precio_del_dia > 0 THEN                      --v5 
            LET l_reg[pos].titulos = l_reg[pos].impt_aport_acept /
                                     l_reg[pos].precio_del_dia
         ELSE                                                       --v5
            LET l_reg[pos].titulos = 0                              --v5
         END IF                                                     --v5

         LET vtot_titulo        = vtot_titulo + l_reg[pos].titulos

         IF l_reg[pos].titulos IS NULL THEN
            LET INT_FLAG = TRUE
            EXIT FOREACH
         END IF

         LET pos = pos + 1
         IF pos >= 500 THEN
            ERROR "FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO !!!"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
          ERROR "NO EXISTE PRECIO DEL TITULO PARA ESTA FECHA !!!"
          LET INT_FLAG = TRUE
      END IF

      LET total_tit = vtot_titulo
      DISPLAY total_tit   TO scr_2.*
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg TO scr_1.*

         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY

         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY

      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
   END WHILE

   CLOSE WINDOW ventana_2
   RETURN INT_FLAG,vtot_titulo
END FUNCTION


FUNCTION Subcuenta()
   OPEN WINDOW vent1 AT 07,02 WITH 2 ROWS, 29 COLUMNS ATTRIBUTE(BORDER)

   MENU "Subcuenta" 
      COMMAND "RCV" "Liquidacion RCV y VIV"
         LET vsubcuenta = "RCV"
         EXIT MENU
      COMMAND "EST" "Liquidacion EST"
         LET vsubcuenta = "EST"
         EXIT MENU
      COMMAND "VIV" "Liqui. VIV Rema Extem"                        --c22-11
         LET vsubcuenta = "VIV"                                        --v4
         EXIT MENU                                                     --v4
   END MENU
   CLOSE WINDOW vent1
END FUNCTION


FUNCTION Tipo_liq()
   OPEN WINDOW vent2 AT 16,02 WITH  2 ROWS, 29 COLUMNS ATTRIBUTE(BORDER)

   MENU "Tipo liquidacion" 
      COMMAND "A" "Aportacion"
         LET vtipo = "A"
         EXIT MENU
      COMMAND "I" "Interes"
         LET vtipo = "I"
         EXIT MENU
   END MENU
   CLOSE WINDOW vent2
END FUNCTION


FUNCTION Despliega_saldos(tipo,aporte)
   DEFINE
      tipo            CHAR(1),
      aporte          CHAR(6),
      fecha_liquida   DATE

   OPEN WINDOW wsaldo AT 9,2 WITH FORM "DISB0073" ATTRIBUTE (BORDER)
   DISPLAY "DEPOSITOS PENDIENTES POR LIQUIDAR" AT 1,15
   DISPLAY aporte AT 1,55
   DISPLAY "-----------------------------------------------------------------------------" AT 2,1

   ---CALL inicia_saldos()

   LET t_v              = " "
   LET tipo_liquida     = " "
   LET tipo_comision    = " "
   LET importe_liquida  = 0
   LET importe_comision = 0
   LET importe_total    = 0
   LET total_accion     = 0

   CASE
      WHEN tipo = "A" AND aporte = "RCV"
         LET tipo_liquida  = "41"
         LET t_v = "43"
         LET tipo_comision = "11"
         LET vsub = "RCVVIV"
      WHEN tipo = "A" AND aporte = "VIV"                              --v4
         LET tipo_liquida  = "41"                                     --v4
         LET vsub = "REMVIV"                                          --v4
      WHEN tipo = "A" AND aporte = "EST"
         LET tipo_liquida  = "42"
         LET tipo_comision = "12"
         LET vsub = "EST"
      WHEN tipo = "I"
--         LET tipo_liquida  = "21"
         LET tipo_comision = "31"

         IF aporte = "RCV" THEN
            LET tipo_liquida  = "41"
            LET vsub = "RCVVIV"
         END IF
         IF aporte = "VIV" THEN
            LET tipo_liquida  = "41"
            LET vsub = "REMVIV"
         END IF
         IF aporte = "EST" THEN
            LET tipo_liquida  = "42"
            LET vsub = "EST"
         END IF

--         LET vsub = "TODAS"
      WHEN tipo = "C" AND aporte = "GAR"
         LET tipo_liquida  = "44"
         LET vsub = "GAR"
   END CASE

   --CONDICION PARA QUE NO SE MUESTREN LOS FOLIOS DE VIVI SI YA SE
   --ENCUENTRAN CON RCV Y SOLO SE MUESTREN EN VIVI SI NO ESTAN EN
   --RCV

   LET cla_sel = NULL                                              --c22-11
   IF vsub = "REMVIV" THEN                                         --c22-11
      WHENEVER ERROR CONTINUE                                      --c22-11
      DROP TABLE tmp_folioss                                       --c22-11
      WHENEVER ERROR STOP                                          --c22-11

      SELECT folio                                                 --c22-11
      FROM  dis_dep_aporte                                         --c22-11
      WHERE ident_pago[14,15] NOT IN ('43','47','48')              --c22-11
      AND   estado = 2                                             --c22-11
      GROUP BY 1                                                   --c22-11
      INTO TEMP tmp_folioss                                        --c22-11

      IF tipo = "I" THEN                       -- DMR ARCH. ACL ESP ident_arch 2
         LET cla_sel = " SELECT folio,",
                            "'",vsub,"'",",",
                            " fecha_archivo ",
                       " FROM  dis_dep_aporte ",
                       " WHERE ident_pago[14,15] IN ('43','47','48') ",
                       " AND   estado = 2 ",
                       " AND   folio NOT IN (SELECT folio FROM tmp_folioss) ",
                       " AND   folio IN (SELECT folio FROM dis_cza_aporte ",
                                       " WHERE  ident_arch = '2') ",
                       " GROUP BY 1,2,3 " CLIPPED,
                       " ORDER BY 1,2 " CLIPPED
      ELSE
         LET cla_sel = " SELECT folio,",
                            "'",vsub,"'",",",
                            " fecha_archivo ",
                       " FROM  dis_dep_aporte ",
                       " WHERE ident_pago[14,15] IN ('43','47','48') ",
                       " AND   estado = 2 ",
                       " AND   folio NOT IN (SELECT folio FROM tmp_folioss) ",
                       " AND   folio NOT IN (SELECT folio FROM dis_cza_aporte ",
                                           " WHERE  ident_arch = '2') ",
                       " GROUP BY 1,2,3 " CLIPPED,
                       " ORDER BY 1,2 " CLIPPED
      END IF
   ELSE
      IF tipo = "I" THEN                       -- DMR ARCH. ACL ESP ident_arch 2
         LET cla_sel = " SELECT folio,",
                            "'",vsub,"'",",", 
                              " fecha_archivo ",
                       " FROM  dis_dep_aporte ",
                   " WHERE ident_pago[14,15] IN ('",tipo_liquida,"','",t_v,"')",
                   " AND   folio IN (SELECT folio FROM dis_cza_aporte ",
                                   " WHERE  ident_arch = '2') ",
                   " AND   estado = 2 ",
                   " GROUP BY 1,2,3 " CLIPPED,
                   " ORDER BY 1,2 " CLIPPED
      ELSE
         IF tipo <> "C" THEN                  --DMR RCV y EST Archivos Normales
            LET cla_sel = " SELECT folio,",
                               "'",vsub,"'",",", 
                                 " fecha_archivo ",
                          " FROM  dis_dep_aporte ",
                   " WHERE ident_pago[14,15] IN ('",tipo_liquida,"','",t_v,"')",
                   " AND   folio NOT IN (SELECT folio FROM dis_cza_aporte ",
                                       " WHERE  ident_arch = '2') ",
                   " AND   estado = 2 ",
                   " GROUP BY 1,2,3 " CLIPPED,
                   " ORDER BY 1,2 " CLIPPED
         ELSE                                 --DMR Garantia NO separa Nor y Esp
            LET cla_sel = " SELECT folio,",
                               "'",vsub,"'",",", 
                                 " fecha_archivo ",
                          " FROM  dis_dep_aporte ",
                   " WHERE ident_pago[14,15] IN ('",tipo_liquida,"','",t_v,"')",
                   " AND   estado = 2 ",
                   " GROUP BY 1,2,3 " CLIPPED,
                   " ORDER BY 1,2 " CLIPPED

         END IF
      END IF
   END IF

   IF cla_sel IS NULL THEN                 --c22-11
   ELSE                                    --c22-11
      PREPARE claexe3 FROM cla_sel
      DECLARE cur_saldo CURSOR FOR claexe3

      LET totala = 0
      LET totalc = 0
      LET i = 1

      FOREACH cur_saldo INTO g_sal[i].*
         LET i = i + 1
      END FOREACH
   END IF                                  --c22-11

   IF cla_sel IS NULL THEN                 --c22-11
      FOR i = 1 TO 30                      --c22-11
         INITIALIZE g_sal[i].* TO NULL     --c22-11
      END FOR                              --c22-11
   END IF                                  --c22-11

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
            WHEN tipo_liquida = "42"
               CALL Asigna_montos_aporte()
            WHEN tipo_liquida = "21"
               CALL Asigna_montos_aporte()         --DMR Acla. Esp. por Separado
--             CALL Asigna_montos_interes()        --Antes TODAS subctas Juntas 
            WHEN tipo_liquida = "43"                                    --v4
               CALL Asigna_montos_cargo()                               --v4
            WHEN tipo_liquida = "44"
               CALL Asigna_montos_cargo()
         END CASE

         EXIT INPUT 
   END INPUT

   CLOSE WINDOW wsaldo
END FUNCTION


FUNCTION Asigna_montos_aporte()
   IF vsub = "RCVVIV" THEN
      SELECT NVL(SUM(impt_aport_acept),0)
      INTO  g_liq.aporte_rcv
      FROM  dis_dep_aporte
      WHERE folio             = g_liq.folio
      AND   ident_pago[14,15] IN ("41","45","46","49","40")
      AND   estado            = 2

      SELECT NVL(SUM(impt_aport_acept),0)
      INTO  g_liq.comision_rcv
      FROM  dis_dep_aporte
      WHERE folio             = g_liq.folio
      AND   ident_pago[14,15] = "11"
      AND   estado            = 2

      LET g_liq.aporte_rcv = g_liq.aporte_rcv + g_liq.comision_rcv * -1
      LET g_liq.aporte_neto_rcv = g_liq.aporte_rcv + g_liq.comision_rcv

      SELECT NVL(SUM(impt_aport_acept),0)
      INTO  g_liq.interes_rcv
      FROM  dis_dep_aporte
      WHERE folio             = g_liq.folio
      AND   ident_pago[14,15] IN ("21","25","26","29","20")
      AND   estado            = 2

      LET g_liq.interes_neto_rcv = g_liq.interes_rcv 

      LET g_liq.aporte_est      = 0
      LET g_liq.comision_est    = 0
      LET g_liq.aporte_neto_est = g_liq.aporte_est + g_liq.comision_est

----      SELECT impt_aport_acept      ----jerry
      SELECT SUM(ROUND(part_viv_acept,2))
      INTO  g_liq.aporte_viv
      FROM  dis_dep_aporte
      WHERE folio             = g_liq.folio
      AND   ident_pago[14,15] = "43"
      AND   estado            = 2

      IF g_liq.aporte_viv IS NULL THEN
         LET g_liq.aporte_viv = 0
      END IF

----      SELECT impt_aport_acept     ----jerry
      SELECT SUM(ROUND(part_viv_acept,2))
      INTO  v43bis
      FROM  dis_dep_aporte
      WHERE folio             = g_liq.folio
      AND   ident_pago[14,15] = "44"
      AND   estado            = 2

      IF v43bis IS NULL THEN
         LET v43bis = 0
      END IF

      LET g_liq.interes_est = 0
      LET g_liq.interes_neto_est = g_liq.interes_est

      LET g_liq.aporte_viv = g_liq.aporte_viv + v43bis

      LET g_liq.aporte_neto_viv = g_liq.aporte_viv 
   ELSE
      IF vsub = "REMVIV" THEN
         SELECT NVL(SUM(impt_aport_acept),0)
         INTO  g_liq.aporte_rcv
         FROM  dis_dep_aporte
         WHERE folio             = g_liq.folio
         AND   ident_pago[14,15] IN ("41","45","46","49","40")
         AND   estado            = 2

         SELECT NVL(SUM(impt_aport_acept),0)
         INTO  g_liq.comision_rcv
         FROM  dis_dep_aporte
         WHERE folio             = g_liq.folio
         AND   ident_pago[14,15] = "11"
         AND   estado            = 2

         LET g_liq.aporte_rcv = g_liq.aporte_rcv + g_liq.comision_rcv * -1
         LET g_liq.aporte_neto_rcv = g_liq.aporte_rcv + g_liq.comision_rcv

         SELECT NVL(SUM(impt_aport_acept),0)
         INTO  g_liq.interes_rcv
         FROM  dis_dep_aporte
         WHERE folio             = g_liq.folio
         AND   ident_pago[14,15] IN ("21","25","26","29","20")
         AND   estado            = 2

         LET g_liq.interes_neto_rcv = g_liq.interes_rcv 

         LET g_liq.aporte_est      = 0
         LET g_liq.comision_est    = 0
         LET g_liq.aporte_neto_est = g_liq.aporte_est + g_liq.comision_est 

----        SELECT impt_aport_acept      ----jerry
         SELECT SUM(ROUND(apli_rem_viv_ace,2))
         INTO  g_liq.aporte_viv
         FROM  dis_dep_aporte
         WHERE folio             = g_liq.folio
         AND   ident_pago[14,15] = "47"
         AND   estado            = 2

         IF g_liq.aporte_viv IS NULL THEN
            LET g_liq.aporte_viv = 0
         END IF

----        SELECT impt_aport_acept     ----jerry
         SELECT SUM(ROUND(apli_ext_viv_ace,2))
         INTO  v43bis
         FROM  dis_dep_aporte
         WHERE folio             = g_liq.folio
         AND   ident_pago[14,15] = "48"
         AND   estado            = 2

         IF v43bis IS NULL THEN
            LET v43bis = 0
         END IF

         LET g_liq.interes_est = 0
         LET g_liq.interes_neto_est = g_liq.interes_est

         LET g_liq.aporte_viv      = g_liq.aporte_viv + v43bis
         LET g_liq.aporte_neto_viv = g_liq.aporte_viv
      ELSE
         LET g_liq.aporte_rcv      = 0
         LET g_liq.comision_rcv    = 0
         LET g_liq.aporte_neto_rcv = g_liq.aporte_rcv + g_liq.comision_rcv
         LET g_liq.interes_rcv = 0 
         LET g_liq.interes_neto_rcv = g_liq.interes_rcv  

         SELECT NVL(SUM(impt_aport_acept),0)
         INTO  g_liq.aporte_est
         FROM  dis_dep_aporte
         WHERE folio             = g_liq.folio
         AND   ident_pago[14,15] = "42"
         AND   estado            = 2

         SELECT NVL(SUM(impt_aport_acept),0)
         INTO  g_liq.comision_est
         FROM  dis_dep_aporte
         WHERE folio             = g_liq.folio
         AND   ident_pago[14,15] = "12"
         AND   estado            = 2

         LET g_liq.aporte_est = g_liq.aporte_est + g_liq.comision_est * -1
         LET g_liq.aporte_neto_est = g_liq.aporte_est + g_liq.comision_est

         SELECT NVL(SUM(impt_aport_acept),0)
         INTO  g_liq.interes_est
         FROM  dis_dep_aporte
         WHERE folio             = g_liq.folio
         AND   ident_pago[14,15] = "22"
         AND   estado            = 2

         LET g_liq.interes_neto_est = g_liq.interes_est

         LET g_liq.aporte_viv = 0
         LET g_liq.aporte_neto_viv = g_liq.aporte_viv
      END IF
   END IF

--   LET g_liq.interes_rcv = 0                       --DMR Acl.Esp. por Separado
--   LET g_liq.interes_neto_rcv = g_liq.interes_rcv  
--   LET g_liq.interes_est = 0
--   LET g_liq.interes_neto_est = g_liq.interes_est  
   LET g_liq.interes_viv      = 0
   LET g_liq.interes_neto_viv = 0

   LET g_liq.total_rcv = g_liq.aporte_rcv + g_liq.interes_rcv

   LET g_liq.total_neto_rcv = g_liq.aporte_neto_rcv + g_liq.interes_neto_rcv

   LET g_liq.total_est = g_liq.aporte_est + g_liq.interes_est

   LET g_liq.total_neto_est = g_liq.aporte_neto_est + g_liq.interes_neto_est

   LET g_liq.gran_tot1 = g_liq.total_rcv + g_liq.total_est

   LET g_liq.gran_tot_com =  g_liq.comision_rcv + g_liq.comision_est

   LET g_liq.gran_tot_neto1 = g_liq.total_neto_rcv + g_liq.total_neto_est

   LET g_liq.gran_tot2 = g_liq.aporte_viv + g_liq.interes_viv

   LET g_liq.gran_tot_neto2 = g_liq.aporte_neto_viv + g_liq.interes_neto_viv
END FUNCTION


FUNCTION inicia_saldos()
   INITIALIZE g_sal TO NULL
   FOR i = 1 TO 500
      DISPLAY g_sal[i].* TO scr_liquida[i].*
   END FOR
END FUNCTION


FUNCTION Ejecuta_liquidacion()
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES 
      (TODAY,                   -- fecha_proceso
       "DISB007B",              -- proceso_cod
       2,                       -- etapa_cod     -- LIQUIDACION
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_liq.folio,             -- parametro1
       vtipo,                   -- parametro2
       vsubcuenta,              -- parametro3
       g_liq.fecha_proceso,     -- parametro4
       g_liq.fecha_acreditacion,-- parametro5
       g_liq.folio,             -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso LIQUIDACION !!! ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   ERROR "EJECUTANDO PROCESO DE LIQUIDACION POR NOHUP !!!"
   SLEEP 3
   LET ejecuta = "nohup time fglgo DISB007B.4gi 0 0 0 ",
                            g_liq.folio CLIPPED," ",
                            vtipo, " ",
                            vsubcuenta," ",
                            g_liq.fecha_proceso," ",
                            g_liq.fecha_acreditacion," &"

   RUN ejecuta
   ERROR "EL PROCESO SE EJECUTO SATISFACTORIAMENTE POR NOHUP !!!"
   SLEEP 3
   ERROR ""
END FUNCTION


FUNCTION Liquidacion_aclaracion()
   LET INT_FLAG = FALSE

   INPUT BY NAME g_liq.*

      BEFORE FIELD folio
--         LET vsubcuenta = "TODAS"
--         LET vtipo = "I"

         CALL Despliega_saldos(vtipo,vsubcuenta)

      AFTER FIELD folio
         LET g_liq.fecha_proceso      = TODAY
         LET g_liq.fecha_acreditacion = TODAY

         DISPLAY BY NAME g_liq.folio,
                         g_liq.fecha_proceso,
                         g_liq.fecha_acreditacion
         DISPLAY BY NAME g_liq.*

         IF g_liq.folio IS NULL THEN        
            ERROR "ESTE CAMPO NO PUEDE SER NULO"
            NEXT FIELD folio
         END IF 

         CASE
            WHEN vsubcuenta = "RCV" OR vsubcuenta = "VIV" AND vtipo = "I"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 3 ",
           "AND ident_pago[14,15] IN ('41','43','45','46','47','48','49','40')",
                            " GROUP BY 1 "

            WHEN vsubcuenta = "EST" AND vtipo = "I"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 3 ",
                             " AND   ident_pago[14,15] = '42' ",
                             " GROUP BY 1 "

            WHEN vsubcuenta = "GAR" AND vtipo = "C"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 3 ",
                             " AND   ident_pago[14,15] = '44' ",
                             " GROUP BY 1 "

         END CASE                                           

--         CASE 
--            WHEN vsubcuenta = "TODAS" AND vtipo = "I"
--               LET cla_sel = "SELECT 'X' ",
--                             " FROM  dis_dep_aporte ",
--                             " WHERE folio = ",g_liq.folio,
--                             " AND   estado = 3 ",
--                             " AND   ident_pago[14,15] = '21' " 
--         END CASE
      
         PREPARE claexe111 FROM cla_sel
         DECLARE cur111 CURSOR FOR claexe111
         OPEN cur111
         FETCH cur111

         IF SQLCA.SQLCODE = 0 THEN
            ERROR "LAS SUBCUENTAS DE ESTE FOLIO YA ESTAN LIQUIDADAS !!!"
            SLEEP 3
            CLOSE cur111
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur111

         CASE
            WHEN vsubcuenta = "RCV" OR vsubcuenta = "VIV" AND vtipo = "I"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 2 ",
           "AND ident_pago[14,15] IN ('41','43','45','46','47','48','49','40')",
                             " GROUP BY 1 "

            WHEN vsubcuenta = "EST" AND vtipo = "I"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 2 ",
                             " AND   ident_pago[14,15] = '42' ",
                             " GROUP BY 1 "

            WHEN vsubcuenta = "GAR" AND vtipo = "C"
               LET cla_sel = "SELECT 'X' ",
                             " FROM  dis_dep_aporte ",
                             " WHERE folio = ",g_liq.folio,
                             " AND   estado = 2 ",
                             " AND   ident_pago[14,15] = '44' ",
                             " GROUP BY 1 "
         END CASE                

--       CASE
--          WHEN vsubcuenta = "TODAS" AND vtipo = "I"
--             LET cla_sel = "SELECT 'X' ",
--                          " FROM  dis_dep_aporte ",
--                          " WHERE folio = ",g_liq.folio,
--                          " AND   estado = 2 ",
--                          " AND   ident_pago[14,15] = '21' "
--       END CASE

         PREPARE claexe222 FROM cla_sel
         DECLARE cur222 CURSOR FOR claexe222
         OPEN cur222
         FETCH cur222

         IF SQLCA.SQLCODE = 100 THEN
            ERROR " LAS SUBCUENTAS DE ESTE FOLIO NO EXISTEN !!!"
            SLEEP 3
            CLOSE cur222
            LET INT_FLAG = TRUE
            EXIT INPUT
         END IF
         CLOSE cur222

         SELECT fecha_recepcion
         INTO  vfecha_recepcion
         FROM  dis_cza_aporte
         WHERE folio = g_liq.folio

      AFTER FIELD fecha_proceso                   -- parametro3
         IF g_liq.fecha_proceso IS NULL THEN        
            ERROR "ESTE CAMPO NO PUEDE SER NULO !!!"
            NEXT FIELD fecha_proceso
         END IF                                 
--         IF g_liq.fecha_proceso < vfecha_recepcion THEN
--            ERROR "LA FECHA PROCESO DEBE SER MAYOR AL ... ", 
--            vfecha_recepcion USING 'DD-MM-YYYY ' 
--            NEXT FIELD fecha_proceso
--         END IF

      AFTER FIELD fecha_acreditacion              -- parametro4
         IF g_liq.fecha_acreditacion IS NULL THEN        
            ERROR "ESTE CAMPO NO PUEDE SER NULO !!!"
            NEXT FIELD fecha_acreditacion
         END IF                                 

         IF vsubcuenta <> 'GAR' AND vsubcuenta <> 'VIV' THEN          --v4

            ------VALIDACION PREVIA
            FOR i = 1 TO 4
               SELECT "X"                                             --c22-6.12
               FROM  glo_valor_accion                                 --c22-6.12
               WHERE fecha_valuacion = g_liq.fecha_acreditacion       --c22-6.12
               AND   codigo_siefore = i                               --c22-6.12
               AND   precio_del_dia > 0                               --c22-6.12
               GROUP BY 1                                             --c22-6.12

               IF SQLCA.SQLCODE = 100 THEN                              --c22-6.12
                  ERROR "NO EXISTE PRECIO ACCION PARA ESTA FECHA, SIE ",i," !!"
                  NEXT FIELD fecha_acreditacion                       --c22-6.12
               END IF                                                 --c22-6.12
            END FOR

            CALL Despliega_valores(g_liq.folio,g_liq.fecha_acreditacion,
                                   vsubcuenta)                        --c22-6.12
            RETURNING INT_FLAG,g_liq.titulos                          --c22-6.12

            SELECT precio_del_dia                                     --c22-6.12
            INTO  g_liq.valor_part                                    --c22-6.12
            FROM  glo_valor_accion                                    --c22-6.12
            WHERE fecha_valuacion = g_liq.fecha_acreditacion          --c22-6.12
            AND   codigo_siefore  = 11                                --c22-6.12
         ELSE
            SELECT precio_del_dia
            INTO  g_liq.precio_del_dia
            FROM  glo_valor_accion
            WHERE fecha_valuacion = g_liq.fecha_acreditacion
            AND   codigo_siefore  = 1                          ----jerry

            SELECT precio_del_dia                              ----jerry
            INTO  g_liq.valor_part                             ----jerry
            FROM  glo_valor_accion                             ----jerry
            WHERE fecha_valuacion = g_liq.fecha_acreditacion   ----jerry
            AND   codigo_siefore  = 11                         ----jerry

--             LET g_liq.titulos = g_liq.gran_tot1 / g_liq.precio_del_dia   --v3
            LET g_liq.titulos = g_liq.gran_tot1 / g_liq.valor_part          --v3

            IF g_liq.titulos IS NULL THEN
               ERROR "LOS TITULOS POR ADQUERIR NO PUEDEN SER NULOS !!!"
--                NEXT FIELD fecha_acreditacion
            END IF

            IF vsub <> "REMVIV" THEN
               LET g_liq.aporte_viv = g_liq.aporte_viv / g_liq.valor_part
               LET g_liq.aporte_neto_viv=g_liq.aporte_neto_viv/g_liq.valor_part
               LET g_liq.gran_tot2 = g_liq.gran_tot2 / g_liq.valor_part
               LET g_liq.gran_tot_neto2 = g_liq.gran_tot_neto2/g_liq.valor_part
            END IF
         END IF

         DISPLAY BY NAME g_liq.*

--         IF g_liq.fecha_acreditacion < vfecha_recepcion THEN
--            ERROR "LA FECHA LIQUIDACION DEBE SER MAYOR AL ... ", 
--            vfecha_recepcion USING 'DD-MM-YYYY ' 
--            NEXT FIELD fecha_acreditacion
--         END IF

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
      CALL Ejecuta_liquidacion()
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Asigna_montos_interes()
   SELECT SUM(impt_aport_acept)
   INTO  g_liq.aporte_rcv
   FROM  dis_dep_aporte
   WHERE folio             = g_liq.folio
   AND   ident_pago[14,15] IN ("41","45","46","49","40")
   AND   estado            = 2

   SELECT SUM(impt_aport_acept)
   INTO  g_liq.comision_rcv
   FROM  dis_dep_aporte
   WHERE folio             = g_liq.folio
   AND   ident_pago[14,15] = "11"
   AND   estado            = 2

   LET g_liq.aporte_neto_rcv = g_liq.aporte_rcv + g_liq.comision_rcv

   SELECT SUM(impt_aport_acept)
   INTO  g_liq.aporte_est
   FROM  dis_dep_aporte
   WHERE folio             = g_liq.folio
   AND   ident_pago[14,15] = "42"
   AND   estado            = 2

   SELECT SUM(impt_aport_acept)
   INTO  g_liq.comision_est
   FROM  dis_dep_aporte
   WHERE folio             = g_liq.folio
   AND   ident_pago[14,15] = "12"
   AND   estado            = 2

   LET g_liq.aporte_neto_est = g_liq.aporte_est + g_liq.comision_est

----   SELECT impt_aport_acept        ----jerry
   SELECT SUM(part_viv_acept)
   INTO  g_liq.aporte_viv
   FROM  dis_dep_aporte
   WHERE folio             = g_liq.folio
   AND   ident_pago[14,15] IN ("43","47","48")
   AND   estado            = 2

   IF g_liq.aporte_viv IS NULL THEN
      LET g_liq.aporte_viv = 0
   END IF

----   SELECT impt_aport_acept        ----jerry
   SELECT SUM(part_viv_acept)
   INTO  v43bis
   FROM  dis_dep_aporte
   WHERE folio             = g_liq.folio
   AND   ident_pago[14,15] = "44"
   AND   estado            = 2

   IF v43bis IS NULL THEN
      LET v43bis = 0
   END IF

   LET g_liq.aporte_viv = g_liq.aporte_viv + v43bis

   LET g_liq.aporte_neto_viv = g_liq.aporte_viv 

   SELECT SUM(impt_aport_acept)
   INTO  g_liq.interes_rcv
   FROM  dis_dep_aporte
   WHERE folio             = g_liq.folio
   AND   ident_pago[14,15] IN ("21","25","26","29","20")
   AND   estado            = 2

   LET g_liq.interes_neto_rcv = g_liq.interes_rcv

   SELECT SUM(impt_aport_acept)
   INTO  g_liq.interes_est
   FROM  dis_dep_aporte
   WHERE folio             = g_liq.folio
   AND   ident_pago[14,15] = "22"
   AND   estado            = 2

   LET g_liq.interes_neto_est = g_liq.interes_est
   LET g_liq.interes_viv      = 0
   LET g_liq.interes_neto_viv = 0

   LET g_liq.total_rcv = g_liq.aporte_rcv + g_liq.interes_rcv

   LET g_liq.total_neto_rcv = g_liq.aporte_neto_rcv + g_liq.interes_neto_rcv

   LET g_liq.total_est = g_liq.aporte_est + g_liq.interes_est

   LET g_liq.total_neto_est = g_liq.aporte_neto_est + g_liq.interes_neto_est

   LET g_liq.gran_tot1 = g_liq.total_rcv + g_liq.total_est

   LET g_liq.gran_tot_com =  g_liq.comision_rcv + g_liq.comision_est

   LET g_liq.gran_tot_neto1 = g_liq.total_neto_rcv + g_liq.total_neto_est

   LET g_liq.gran_tot2 = g_liq.aporte_viv + g_liq.interes_viv

   LET g_liq.gran_tot_neto2 = g_liq.aporte_neto_viv + g_liq.interes_neto_viv
END FUNCTION


FUNCTION limpieza()
   DEFINE 
      comandol CHAR(300)

--------------------------------------------------------------------
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
       "/;sed 's/",ascii 92,ascii 92,"/N/g' ",g_reg.nombre_archivo CLIPPED," > LIMP2 "
   RUN comandol

   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/; mv LIMP2 ",g_reg.nombre_archivo
   RUN comandol
--------------------------------------------------------------------
---   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
---            "/;sed 's/",ascii 92,"//N/g' ",g_reg.nombre_archivo CLIPPED, " > LIMP2 "
---   RUN comandol
   
---   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
---            "/; mv LIMP2 ",g_reg.nombre_archivo
---   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/;sed 's/|/N/g' ",g_reg.nombre_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/; mv LIMP2 ",g_reg.nombre_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/;sed 's/@/N/g' ",g_reg.nombre_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/; mv LIMP2 ",g_reg.nombre_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/;sed 's/,/*/g' ",g_reg.nombre_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/; mv LIMP2 ",g_reg.nombre_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/;sed 's/",ascii 92,"+/*/g' ",g_reg.nombre_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/; mv LIMP2 ",g_reg.nombre_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/;sed 's/",ascii 13,"//g' ",g_reg.nombre_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/; mv LIMP2 ",g_reg.nombre_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/;sed 's/&/N/g' ",g_reg.nombre_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/; mv LIMP2 ",g_reg.nombre_archivo
   RUN comandol
--------------------------------------------------------------------
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/;sed 's/#/N/g' ",g_reg.nombre_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",g_param.ruta_rescate CLIPPED,
            "/; mv LIMP2 ",g_reg.nombre_archivo
   RUN comandol
--------------------------------------------------------------------
END FUNCTION


FUNCTION Asigna_montos_cargo()
   LET g_liq.aporte_rcv      = 0
   LET g_liq.comision_rcv    = 0
   LET g_liq.aporte_neto_rcv = g_liq.aporte_rcv + g_liq.comision_rcv

   LET g_liq.aporte_est      = 0
   LET g_liq.comision_est    = 0
   LET g_liq.aporte_neto_est = g_liq.aporte_est + g_liq.comision_est

--    SELECT impt_aport_acept        ----jerry
--    SELECT SUM(part_viv_acept)     ----jerry   --c22-6  --v3
   SELECT SUM(impt_aport_acept)                           --v3
   INTO   g_liq.aporte_viv
   FROM   dis_dep_aporte
   WHERE  folio             = g_liq.folio
--    AND    ident_pago[14,15] = "44"                     --v4 
   AND    ident_pago[14,15] = tipo_liquida                --v4
   AND    estado            = 2

   LET g_liq.aporte_neto_viv = g_liq.aporte_viv

   LET g_liq.interes_rcv = 0
   LET g_liq.interes_neto_rcv = g_liq.interes_rcv  
   LET g_liq.interes_est = 0
   LET g_liq.interes_neto_est = g_liq.interes_est  
   LET g_liq.interes_viv      = 0
   LET g_liq.interes_neto_viv = 0

   LET g_liq.total_rcv = g_liq.aporte_rcv + g_liq.interes_rcv

   LET g_liq.total_neto_rcv = g_liq.aporte_neto_rcv + g_liq.interes_neto_rcv

   LET g_liq.total_est = g_liq.aporte_est + g_liq.interes_est

   LET g_liq.total_neto_est = g_liq.aporte_neto_est + g_liq.interes_neto_est

   LET g_liq.gran_tot1 = g_liq.total_rcv + g_liq.total_est

   LET g_liq.gran_tot_com =  g_liq.comision_rcv + g_liq.comision_est

   LET g_liq.gran_tot_neto1 = g_liq.total_neto_rcv + g_liq.total_neto_est

   LET g_liq.gran_tot2 = g_liq.aporte_viv + g_liq.interes_viv

   LET g_liq.gran_tot_neto2 = g_liq.aporte_neto_viv + g_liq.interes_neto_viv
END FUNCTION


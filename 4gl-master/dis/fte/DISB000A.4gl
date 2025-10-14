###############################################################################
#Proyecto          => SAFRE                                                   #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS                                                     #
#Programa          => DISB000A                                                #
#Descripcion       => GENERA Archivo de Layout (091301)                       #
#Fecha Inicio      => 10 Junio 2010                                           #
#Elaboro           => DMR                                                     #
#Modificaciones    => METLIFE es diferente ya que la tabla es int_det_vol_ob  #
#Fecha Mod         => 22 Junio 2010                                           #
#                  =>                                                         #
#Modificaciones    =>                                                         #
#Fecha Mod         =>                                                         #
#                  =>                                                         #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE 
      gparam_dis              RECORD LIKE seg_modulo.*,
      vcod_afore              LIKE tab_afore_local.codigo_afore,
      vhora_final             CHAR(08),
      vresultado              CHAR(50),
      vetapa_cod              SMALLINT,
      hoy                     DATE,
      gusuario                CHAR(08),
      vnom_archivo            CHAR(22),
      vreporte                CHAR(200),
      ejecuta                 CHAR(200),
      hora_inicial            CHAR(08),
      hora_final              CHAR(08),
      opc                     CHAR(1)

   DEFINE
      reg_ident RECORD
         folio                INTEGER,
         fecha_recepcion      DATE
   END RECORD

   DEFINE
      reg_sol RECORD
         nss                  CHAR(11),
         curp                 CHAR(18),
         nombre               CHAR(50),
         sdo_bas              DEC(7,2),
         meses09              SMALLINT,
         mon_aho_sol          DEC(10,2),
         folio_resol          INTEGER,
         fecha_resol          DATE
   END RECORD
 
   DEFINE vnum_reg            INTEGER
   DEFINE vfolio              INTEGER
   DEFINE monto_aho_sol       DECIMAL(16,6)
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("DISB000A.log")

   CALL Inicializa()

   OPEN WINDOW win4 AT 2,2 WITH FORM "AHOSOL01" ATTRIBUTE(BORDER)
   DISPLAY " DISB000A  MODULO DE CAPTURA DE SOLICITUDES DE AHORRO SOLIDARIO              " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY  hoy USING "DD/MM/YYYY" AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY " [Esc] Guardar                                               [Ctrl-C] Salir " AT 5,1 ATTRIBUTE(REVERSE)

   MENU "AHORRO SOLIDARIO"
      COMMAND "Captura" "Captura de Solicitudes de Ahorro Solidario"
         CALL Captura()
      COMMAND "Reporte Sol." "Realiza REPORTE de Solicitudes Capturadas"
         CALL rep_sols()
      COMMAND "Liquidacion" "Liquidacion de Solicitudes y Reporte"
         CALL Liquidacion()
      COMMAND "Salir" "Salida del programa"
         EXIT MENU
   END MENU

   CLEAR FORM                 
   CLEAR SCREEN
   CLOSE WINDOW win4
END MAIN


FUNCTION Inicializa()
   LET hoy = TODAY

   LET vnum_reg      = 0
   LET monto_aho_sol = 0
  
   SELECT *,USER
   INTO   gparam_dis.*,gusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   SELECT codigo_afore
   INTO vcod_afore
   FROM tab_afore_local
END FUNCTION


FUNCTION Captura()
   DEFINE 
      opc          CHAR(1)

   DEFINE 
      nombresa     CHAR(40),
      paternoa     CHAR(40),
      maternoa     CHAR(40)

   LET INT_FLAG = FALSE

   INPUT BY NAME reg_sol.*
      AFTER FIELD nss
         IF reg_sol.nss IS NOT NULL THEN
            SELECT "OK"
            FROM afi_mae_afiliado
            WHERE n_seguro = reg_sol.nss
 
            IF STATUS = NOTFOUND THEN
               ERROR " NSS INEXISTENTE !!! "
               NEXT FIELD nss
            ELSE
               SELECT "OK"
               FROM  dis_aho_sol
               WHERE nss = reg_sol.nss
               AND   estado = 0 
 
               IF STATUS <> NOTFOUND THEN
                  ERROR " YA EXISTE SOLICITUD PARA ESTE TRABAJADOR !!! "
                  NEXT FIELD nss
               ELSE
                  SELECT n_unico,nombres,paterno,materno
                  INTO reg_sol.curp,nombresa,paternoa,maternoa
                  FROM afi_mae_afiliado
                  WHERE n_seguro = reg_sol.nss
           
                  LET reg_sol.nombre= nombresa CLIPPED," ",paternoa CLIPPED," ",
                                      maternoa

                  DISPLAY BY NAME reg_sol.curp, reg_sol.nombre
                  NEXT FIELD sdo_bas
               END IF
            END IF
         END IF

      AFTER FIELD curp
         IF reg_sol.curp IS NULL AND reg_sol.nss IS NULL THEN
            ERROR " EL NSS Y CURP NO Pueden ser NULOS !!! "
            NEXT FIELD curp
         ELSE
            SELECT "OK"
            FROM  afi_mae_afiliado
            WHERE n_unico = reg_sol.curp
 
            IF STATUS = NOTFOUND THEN
               ERROR " CURP INEXISTENTE !!! "
               NEXT FIELD curp
            ELSE 
               SELECT "OK"
               FROM  dis_aho_sol
               WHERE curp = reg_sol.curp
               AND   estado = 0 
 
               IF STATUS <> NOTFOUND THEN
                  ERROR " YA EXISTE SOLICITUD PARA ESTE TRABAJADOR !!! "
                  NEXT FIELD curp
               ELSE
                  SELECT n_seguro,nombres,paterno,materno
                  INTO reg_sol.nss,nombresa,paternoa,maternoa
                  FROM afi_mae_afiliado
                  WHERE n_unico = reg_sol.curp
           
                  LET reg_sol.nombre= nombresa CLIPPED," ",paternoa CLIPPED," ",
                                      maternoa 

                  DISPLAY BY NAME reg_sol.nss, reg_sol.nombre
                  NEXT FIELD sdo_bas
               END IF
            END IF
         END IF

      AFTER FIELD sdo_bas
         IF reg_sol.sdo_bas IS NULL OR reg_sol.sdo_bas = 0 THEN
            ERROR " EL SUELDO BASICO NO Puede ser NULO o CERO !!! "
            NEXT FIELD sdo_bas
         END IF

      AFTER FIELD meses09
         IF reg_sol.meses09 IS NULL OR reg_sol.meses09 = 0 THEN
            ERROR " LOS MESES del 2009 no pueden ser NULO o CERO !!! "
            NEXT FIELD meses09
         END IF

      AFTER FIELD mon_aho_sol
         IF reg_sol.mon_aho_sol IS NULL OR reg_sol.mon_aho_sol <= 0 THEN
            ERROR " El monto de AHORRO SOLIDARIO no puede ser NULO o CERO !!! "
            NEXT FIELD mon_aho_sol
         END IF

      AFTER FIELD folio_resol
         IF reg_sol.folio_resol IS NULL OR reg_sol.folio_resol = 0 THEN
            ERROR " EL FOLIO de RESOLUCION NO Puede ser NULO o CERO !!! "
            NEXT FIELD folio_resol
         END IF
      
      AFTER FIELD fecha_resol
         IF reg_sol.fecha_resol IS NULL THEN
            ERROR "Fecha de Resolucion no puede ser NULA !!!"
            NEXT FIELD fecha_resol
         ELSE
            IF reg_sol.fecha_resol > TODAY THEN
               ERROR "Fecha de Resolucion no puede ser MAYOR a la del DIA !!!"
               NEXT FIELD fecha_resol
            END IF
         END IF

      ON KEY(ESC)
         IF reg_sol.curp IS NULL AND reg_sol.nss IS NULL THEN
            ERROR " EL NSS Y CURP NO Pueden ser NULOS !!! "
            NEXT FIELD nss
         END IF

         IF reg_sol.nss IS NOT NULL THEN
            SELECT "OK"
            FROM afi_mae_afiliado
            WHERE n_seguro = reg_sol.nss
 
            IF STATUS = NOTFOUND THEN
               ERROR " NSS INEXISTENTE !!! "
               NEXT FIELD nss
            ELSE
               SELECT "OK"
               FROM  dis_aho_sol
               WHERE nss = reg_sol.nss
               AND   estado = 0 
 
               IF STATUS <> NOTFOUND THEN
                  ERROR " YA EXISTE SOLICITUD PARA ESTE TRABAJADOR !!! "
                  NEXT FIELD nss
               ELSE
                  SELECT n_unico,nombres,paterno,materno
                  INTO reg_sol.curp,nombresa,paternoa,maternoa
                  FROM afi_mae_afiliado
                  WHERE n_seguro = reg_sol.nss
           
                  LET reg_sol.nombre= nombresa CLIPPED," ",paternoa CLIPPED," ",
                                      maternoa 

                  DISPLAY BY NAME reg_sol.curp, reg_sol.nombre
               END IF
            END IF
         END IF

         IF reg_sol.curp IS NULL AND reg_sol.nss IS NULL THEN
            ERROR " EL NSS Y CURP NO Pueden ser NULOS !!! "
            NEXT FIELD curp
         ELSE
            IF reg_sol.curp IS NOT NULL AND reg_sol.curp <> "" THEN
               SELECT "OK"
               FROM  afi_mae_afiliado
               WHERE n_unico = reg_sol.curp
 
               IF STATUS = NOTFOUND THEN
                  ERROR " CURP INEXISTENTE !!! "
                  NEXT FIELD curp
               ELSE 
                  SELECT "OK"
                  FROM  dis_aho_sol
                  WHERE curp = reg_sol.curp
                  AND   estado = 0 
 
                  IF STATUS <> NOTFOUND THEN
                     ERROR " YA EXISTE SOLICITUD PARA ESTE TRABAJADOR !!! "
                     NEXT FIELD curp
                  ELSE
                     SELECT n_seguro,nombres,paterno,materno
                     INTO reg_sol.nss,nombresa,paternoa,maternoa
                     FROM afi_mae_afiliado
                     WHERE n_unico = reg_sol.curp
           
                     LET reg_sol.nombre = nombresa CLIPPED," ",paternoa CLIPPED,                                          " ",maternoa 

                     DISPLAY BY NAME reg_sol.nss, reg_sol.nombre
                  END IF
               END IF
            END IF
         END IF

         IF reg_sol.sdo_bas IS NULL OR reg_sol.sdo_bas = 0 THEN
            ERROR " EL SUELDO BASICO NO Puede ser NULO o CERO !!! "
            NEXT FIELD sdo_bas
         END IF

         IF reg_sol.meses09 IS NULL OR reg_sol.meses09 = 0 THEN
            ERROR " LOS MESES del 2009 no pueden ser NULO o CERO !!! "
            NEXT FIELD meses09
         END IF

         IF reg_sol.mon_aho_sol IS NULL OR reg_sol.mon_aho_sol <= 0  THEN
            ERROR "El monto de AHORRO SOLIDARIO no puede ser NULO o CERO !!!"
            NEXT FIELD mon_aho_sol
         END IF

         IF reg_sol.folio_resol IS NULL OR reg_sol.folio_resol = 0 THEN
            ERROR " EL FOLIO de RESOLUCION NO Puede ser NULO o CERO !!! "
            NEXT FIELD folio_resol
         END IF
      
         IF reg_sol.fecha_resol IS NULL THEN
            ERROR "Fecha de Resolucion no puede ser NULA !!!"
            NEXT FIELD fecha_resol
         ELSE
            IF reg_sol.fecha_resol > TODAY THEN
               ERROR "Fecha de Resolucion no puede ser MAYOR a la del DIA !!!"
               NEXT FIELD fecha_resol
            END IF
         END IF
         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

   END INPUT

   IF INT_FLAG THEN
      ERROR   "PROCESO CANCELADO .."
      SLEEP   1
      LET INT_FLAG = FALSE
   ELSE
      PROMPT " Desea Guardar Solicitud [S/N]  : ... " ATTRIBUTE(REVERSE)
      FOR  opc ATTRIBUTE(REVERSE)

      IF opc MATCHES '[Ss]' THEN
         INSERT INTO dis_aho_sol
         VALUES(0,reg_sol.*,0)

         ERROR " REGISTRO GUARDADO ..."
         SLEEP 1
      ELSE
         ERROR " PROCESO CANCELADO ..."
         SLEEP 1
      END IF
   END IF
   CLEAR FORM
END FUNCTION


FUNCTION Provisiona()
   DEFINE
      reg_pro  RECORD LIKE dis_aho_sol.*,
      reg_cta  RECORD LIKE dis_cuenta.*

   DEFINE
      cont_reg   INTEGER,
      cont_reg2  INTEGER,
      i          SMALLINT

   INITIALIZE reg_pro TO NULL
   INITIALIZE reg_cta TO NULL

   SELECT COUNT(*)
   INTO   cont_reg
   FROM   dis_aho_sol
   WHERE  estado = 0

   SELECT count(*)
   INTO   cont_reg2
   FROM  dis_aho_sol b
   WHERE b.nss NOT IN (SELECT a.nss FROM cta_regimen a
                       WHERE a.nss = b.nss
                       AND   a.subcuenta = 34) 

   IF cont_reg > 0 AND cont_reg2 = 0 THEN

      PROMPT " Desea Provisionar Solicitudes [S/N]  : ... " ATTRIBUTE(REVERSE)
      FOR  opc ATTRIBUTE(REVERSE)

      IF opc MATCHES '[Ss]' THEN

         FOR i = 1 TO 5             # Num de Siefores
             SELECT precio_del_dia
             INTO   reg_cta.precio_accion
             FROM   glo_valor_accion
             WHERE  fecha_valuacion = TODAY
             AND    codigo_siefore  = i                        

             IF STATUS = NOTFOUND THEN
                ERROR " No existe precio de la SIEFORE : ",i
                RETURN
             END IF
         END FOR

         INSERT INTO glo_folio VALUES(0)
         SELECT MAX(folio)
         INTO   reg_cta.folio
         FROM   glo_folio 

         LET cont_reg = 0 
         DECLARE cur_pro CURSOR FOR
         SELECT * FROM dis_aho_sol
         WHERE estado = 0 

         FOREACH cur_pro INTO reg_pro.*
            LET cont_reg = cont_reg + 1
   
            LET reg_cta.tipo_movimiento = 1
            LET reg_cta.subcuenta       = 34
    
            LET reg_cta.siefore = 0
            SELECT codigo_siefore
            INTO   reg_cta.siefore 
            FROM   cta_regimen a
            WHERE  a.nss = reg_pro.nss
            AND    a.subcuenta = 34

            IF reg_cta.siefore IS NULL  OR reg_cta.siefore = 0 THEN
               PROMPT " ERROR siefore INCORRECTA del NSS : ",reg_pro.nss
               FOR opc
            END IF

            LET reg_cta.consecutivo_lote = cont_reg
            LET reg_cta.nss              = reg_pro.nss
            LET reg_cta.curp             = reg_pro.curp
            LET reg_cta.folio_sua        = ""
            LET reg_cta.fecha_pago       = TODAY
            LET reg_cta.fecha_valor      = TODAY
            LET reg_cta.fecha_conversion = TODAY
            
            SELECT precio_del_dia
            INTO   reg_cta.precio_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = TODAY
            AND    codigo_siefore  = reg_cta.siefore
   
            LET reg_cta.monto_en_pesos    = reg_pro.mon_aho_sol
            LET reg_cta.monto_en_acciones = reg_cta.monto_en_pesos / 
                                            reg_cta.precio_accion
        
            LET reg_cta.dias_cotizados    = 0
            LET reg_cta.sucursal          = ""
            LET reg_cta.id_aportante      = "APO_AHO_SOL_TRA"
            LET reg_cta.estado            = 5
            LET reg_cta.fecha_proceso     = TODAY
            LET reg_cta.usuario           = gusuario
            LET reg_cta.fecha_archivo     = TODAY
            LET reg_cta.etiqueta          = 0

            INSERT INTO dis_provision
            VALUES (reg_cta.*)
   
            UPDATE dis_aho_sol
            SET folio  = reg_cta.folio,
                estado = 1
            WHERE estado = 0 
         END FOREACH

         PROMPT " REGISTROS PROVISIONADOS ... ",cont_reg,"                     FOLIO  : ",reg_cta.folio FOR CHAR opc
      ELSE
         ERROR " PROCESO CANCELADO ..."
         SLEEP 1
      END IF
   ELSE
      IF cont_reg = 0 THEN
         ERROR " NO Existen Solicitudes CAPTURADAS ..."
         SLEEP 1
      ELSE 
         IF cont_reg2 > 0 THEN
            ERROR " ERROR En la INFORMACION algun NSS no existe en CTA_REGIMEN..."
            SLEEP 1
         END IF
      END IF
   END IF 
END FUNCTION


FUNCTION Liquidacion()
   DEFINE
      reg_pro  RECORD LIKE dis_aho_sol.*,
      reg_cta  RECORD LIKE dis_cuenta.*

   DEFINE 
      reg_liq  RECORD
                  vfolio   INTEGER
               END RECORD
   DEFINE
      cont_reg             INTEGER,
      cont_reg2            INTEGER,
      cont_reg3            INTEGER,
      i                    SMALLINT,
      nssi                 CHAR(11)

   INITIALIZE reg_pro TO NULL
   INITIALIZE reg_cta TO NULL

   OPEN WINDOW win5 AT 2,2 WITH FORM "AHOSOL02" ATTRIBUTE(BORDER)
   DISPLAY "           MODULO DE CAPTURA DE SOLICITUDES DE AHORRO SOLIDARIO             " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Esc] Guardar                                               [Ctrl-C] Salir " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME reg_liq.vfolio
      AFTER FIELD vfolio
         IF reg_liq.vfolio IS NOT NULL OR reg_liq.vfolio > 0  THEN
            IF vcod_afore = 564 THEN              #MET
               SELECT "OK"
               FROM  int_det_vol_ob
               WHERE folio  = reg_liq.vfolio
               GROUP BY 1
 
               IF STATUS = NOTFOUND THEN
                  ERROR " FOLIO INEXISTENTE !!! "
                  NEXT FIELD vfolio
               ELSE
                  SELECT "OK"
                  FROM  int_det_vol_ob
                  WHERE folio  = reg_liq.vfolio
                  AND   estado = 1
                  GROUP BY 1

                  IF STATUS = NOTFOUND THEN
                     ERROR " NO Existen Solicitudes por LIQUIDAR ..."
                     NEXT FIELD vfolio
                  ELSE
                     SELECT count(*)
                     INTO  cont_reg
                     FROM  dis_aho_sol b
                     WHERE b.nss IN (SELECT a.nss
                                     FROM int_det_vol_ob a
                                     WHERE a.folio = reg_liq.vfolio
                                     AND a.nss     = b.nss
                                     AND a.subcuenta = 34 )
                           AND   b.estado = 0

                     IF cont_reg = 0 THEN
                        ERROR " NO Existen Solicitudes CAPTURADAS o YA se Liquidaron ..."
                        NEXT FIELD vfolio
                     ELSE
                        SELECT count(*)
                        INTO  cont_reg2
                        FROM  dis_aho_sol b
                        WHERE b.nss IN (SELECT a.nss
                                        FROM int_det_vol_ob a
                                        WHERE a.folio = reg_liq.vfolio
                                        AND a.nss   = b.nss
                                        AND a.subcuenta = 34 )
                        AND  b.nss NOT IN (SELECT a.nss FROM cta_regimen a
                                           WHERE a.nss = b.nss
                                           AND   a.subcuenta = 34)

                        IF cont_reg2 > 0 THEN
                           ERROR " ERROR En la INFORMACION algun NSS no existe en CTA_REGIMEN..." 
                           NEXT FIELD vfolio
                        END IF
                     END IF
                  END IF
               END IF
            ELSE
               SELECT "OK"
               FROM  int_det_voluntaria
               WHERE folio  = reg_liq.vfolio
               GROUP BY 1
 
               IF STATUS = NOTFOUND THEN
                  ERROR " FOLIO INEXISTENTE !!! "
                  NEXT FIELD vfolio
               ELSE
                  SELECT "OK"
                  FROM  int_det_voluntaria
                  WHERE folio  = reg_liq.vfolio
                  AND   estado = 1
                  GROUP BY 1
 
                  IF STATUS = NOTFOUND THEN
                     ERROR " NO Existen Solicitudes por LIQUIDAR ..."
                     NEXT FIELD vfolio
                  ELSE  
                     CASE vcod_afore 
                        WHEN 562                   #INV
                           SELECT count(*)
                           INTO  cont_reg
                           FROM  dis_aho_sol b
                           WHERE b.nss IN (SELECT a.nss
                                           FROM int_det_voluntaria a
                                           WHERE a.folio = reg_liq.vfolio
                                           AND a.nss       = b.nss
                                           AND a.subcuenta = 34 )
                           AND   b.estado = 0

                        WHEN 568                   #COP
                           SELECT count(*)
                           INTO  cont_reg
                           FROM  dis_aho_sol b
                           WHERE b.nss IN (SELECT a.nss
                                           FROM int_det_voluntaria a
                                           WHERE a.folio = reg_liq.vfolio
                                           AND a.nss   = b.nss
                                           AND a.tipo_aportacion = "A")
                           AND   b.estado = 0

                        WHEN 516                   #XXI
                           SELECT count(*)
                           INTO  cont_reg
                           FROM  dis_aho_sol b
                           WHERE b.nss IN (SELECT a.nss
                                           FROM int_det_voluntaria a
                                           WHERE a.folio = reg_liq.vfolio
                                           AND a.nss   = b.nss
                                           AND a.tipo_vol = "A" )
                           AND   b.estado = 0   
                     END CASE
 
                     IF cont_reg = 0 THEN
                        ERROR " NO Existen Solicitudes CAPTURADAS o YA se Liquidaron ..."
                        NEXT FIELD vfolio
                     ELSE
                        CASE vcod_afore 
                           WHEN 562                   #INV
                              SELECT count(*)
                              INTO  cont_reg2
                              FROM  dis_aho_sol b
                              WHERE b.nss IN (SELECT a.nss
                                              FROM int_det_voluntaria a
                                              WHERE a.folio = reg_liq.vfolio
                                              AND a.nss       = b.nss
                                              AND a.subcuenta = 34 )
                              AND  b.nss NOT IN (SELECT a.nss FROM cta_regimen a
                                                 WHERE a.nss = b.nss
                                                 AND   a.subcuenta = 34)

                           WHEN 568                   #COP
                              SELECT count(*)
                              INTO  cont_reg2
                              FROM  dis_aho_sol b
                              WHERE b.nss IN (SELECT a.nss
                                           FROM int_det_voluntaria a
                                           WHERE a.folio = reg_liq.vfolio
                                           AND a.nss   = b.nss
                                           AND a.tipo_aportacion = "A")
                              AND  b.nss NOT IN (SELECT a.nss FROM cta_regimen a
                                                 WHERE a.nss = b.nss
                                                 AND   a.subcuenta = 34)

                           WHEN 516                   #XXI
                              SELECT count(*)
                              INTO  cont_reg2
                              FROM  dis_aho_sol b
                              WHERE b.nss IN (SELECT a.nss
                                              FROM int_det_voluntaria a
                                              WHERE a.folio = reg_liq.vfolio
                                              AND a.nss   = b.nss
                                              AND a.tipo_vol = "A" )
                              AND  b.nss NOT IN (SELECT a.nss FROM cta_regimen a
                                                 WHERE a.nss = b.nss
                                                 AND   a.subcuenta = 34) 
 
                        END CASE
  
                        IF cont_reg2 > 0 THEN
                           ERROR " ERROR En la INFORMACION algun NSS no existe en CTA_REGIMEN..." 
                           NEXT FIELD vfolio
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         ELSE
            ERROR " FOLIO INCORRECTO NO PUEDE SER NULO NI CERO !!! "
            NEXT FIELD vfolio
         END IF

      ON KEY(ESC)
         IF reg_liq.vfolio IS NULL OR reg_liq.vfolio = 0  THEN
            ERROR " FOLIO INCORRECTO NO PUEDE SER NULO NI CERO !!! "
            NEXT FIELD vfolio
         ELSE
            IF vcod_afore = 564 THEN
               SELECT "OK"
               FROM  int_det_vol_ob
               WHERE folio = reg_liq.vfolio
               AND subcuenta = 34
               GROUP BY 1
 
               IF STATUS = NOTFOUND THEN
                  ERROR " FOLIO INEXISTENTE !!! "
                  NEXT FIELD vfolio
               END IF
            ELSE
               SELECT "OK"
               FROM  int_det_voluntaria
               WHERE folio = reg_liq.vfolio
               GROUP BY 1
 
               IF STATUS = NOTFOUND THEN
                  ERROR " FOLIO INEXISTENTE !!! "
                  NEXT FIELD vfolio
               END IF
            END IF
         END IF
         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

   END INPUT

   IF INT_FLAG THEN
      ERROR   "PROCESO CANCELADO .."
      SLEEP   1
      LET INT_FLAG = FALSE
   ELSE
      PROMPT " Desea Liquidar Solicitudes [S/N]  : ... " ATTRIBUTE(REVERSE)
      FOR  opc ATTRIBUTE(REVERSE)

      IF opc MATCHES '[Ss]' THEN
         FOR i = 1 TO 5             # Num de Siefores
             SELECT precio_del_dia
             INTO   reg_cta.precio_accion
             FROM   glo_valor_accion
             WHERE  fecha_valuacion = TODAY
             AND    codigo_siefore  = i                        

             IF STATUS = NOTFOUND THEN
                ERROR " No existe precio de la SIEFORE : ",i
                CLOSE WINDOW win5
                RETURN
             END IF
         END FOR

         CASE vcod_afore
            WHEN 562                   #INV
               DECLARE cur_inv CURSOR FOR
               SELECT UNIQUE nss 
               FROM  int_det_voluntaria
               WHERE folio     = reg_liq.vfolio
               AND   subcuenta = 34
          
               FOREACH cur_inv INTO nssi
                  UPDATE dis_aho_sol
                  SET estado = 1
                  WHERE nss  = nssi
               END FOREACH

            WHEN 564                   #MET
               DECLARE cur_met CURSOR FOR
               SELECT UNIQUE nss
               FROM  int_det_vol_ob
               WHERE folio     = reg_liq.vfolio
               AND   subcuenta = 34

               FOREACH cur_met INTO nssi
                  UPDATE dis_aho_sol
                  SET estado = 1
                  WHERE nss  = nssi
               END FOREACH

            WHEN 568                   #COP
               DECLARE cur_cop CURSOR FOR
               SELECT UNIQUE nss
               FROM  int_det_voluntaria
               WHERE folio           = reg_liq.vfolio
               AND   tipo_aportacion = "A"

               FOREACH cur_cop INTO nssi
                  UPDATE dis_aho_sol
                  SET estado = 1
                  WHERE nss  = nssi
               END FOREACH

            WHEN 516                   #XXI
               DECLARE cur_xxi CURSOR FOR
               SELECT UNIQUE nss
               FROM  int_det_voluntaria
               WHERE folio     = reg_liq.vfolio
               AND   tipo_vol  = "A"

               FOREACH cur_xxi INTO nssi
                  UPDATE dis_aho_sol
                  SET estado = 1
                  WHERE nss  = nssi
               END FOREACH 
         END CASE

         INSERT INTO glo_folio VALUES(0)
         SELECT MAX(folio)
         INTO   reg_cta.folio
         FROM   glo_folio 

         LET cont_reg = 0 
         DECLARE cur_liq CURSOR FOR
         SELECT * FROM dis_aho_sol
         WHERE estado = 1 

         FOREACH cur_liq INTO reg_pro.*
            LET cont_reg = cont_reg + 1
   
            LET reg_cta.tipo_movimiento = 1
            LET reg_cta.subcuenta       = 34
    
            LET reg_cta.siefore = 0
            SELECT codigo_siefore
            INTO   reg_cta.siefore 
            FROM   cta_regimen a
            WHERE  a.nss = reg_pro.nss
            AND    a.subcuenta = 34

            IF reg_cta.siefore IS NULL  OR reg_cta.siefore = 0 THEN
               PROMPT " ERROR siefore INCORRECTA del NSS : ",reg_pro.nss
               FOR opc
            END IF

            LET reg_cta.consecutivo_lote = cont_reg
            LET reg_cta.nss              = reg_pro.nss
            LET reg_cta.curp             = reg_pro.curp
            LET reg_cta.folio_sua        = ""
            LET reg_cta.fecha_pago       = TODAY
            LET reg_cta.fecha_valor      = TODAY
            LET reg_cta.fecha_conversion = TODAY
            
            SELECT precio_del_dia
            INTO   reg_cta.precio_accion
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = TODAY
            AND    codigo_siefore  = reg_cta.siefore
   
            LET reg_cta.monto_en_pesos    = reg_pro.mon_aho_sol
            LET reg_cta.monto_en_acciones = reg_cta.monto_en_pesos / 
                                            reg_cta.precio_accion
        
            LET reg_cta.dias_cotizados    = 0
            LET reg_cta.sucursal          = ""
            LET reg_cta.id_aportante      = "APO_AHOSOL_TRA"
            LET reg_cta.estado            = 5
            LET reg_cta.fecha_proceso     = TODAY
            LET reg_cta.usuario           = gusuario
            LET reg_cta.fecha_archivo     = TODAY
            LET reg_cta.etiqueta          = 0

            INSERT INTO dis_cuenta
            VALUES (reg_cta.*)
   
            UPDATE dis_aho_sol
            SET folio  = reg_cta.folio,
                estado = 2
            WHERE estado = 1 

            CASE vcod_afore
               WHEN 562                   #INV
                  UPDATE int_det_voluntaria
                  SET estado = 2
                  WHERE folio = reg_liq.vfolio
                  AND   nss   = reg_cta.nss
                  AND   subcuenta = 34
          
               WHEN 564                   #MET
                  UPDATE int_det_vol_ob
                  SET estado = 2
                  WHERE folio = reg_liq.vfolio
                  AND   nss   = reg_cta.nss
                  AND   subcuenta = 34

               WHEN 568                   #COP
                  UPDATE int_det_voluntaria
                  SET estado = 2
                  WHERE folio           = reg_liq.vfolio
                  AND   tipo_aportacion = "A"

                WHEN 516                   #XXI
                  UPDATE int_det_voluntaria
                  SET estado = 2
                  WHERE folio = reg_liq.vfolio
                  AND   nss   = reg_cta.nss
                  AND   tipo_vol  = "A"

            END CASE
         END FOREACH
     
         CALL Genera_salidas(reg_cta.folio)

         PROMPT " REGISTROS LIQUIDADOS ... ",cont_reg USING "#######","      FOLIO  : ",reg_cta.folio USING "#######", "     ENTER PARA SALIR " FOR CHAR opc
      ELSE
         ERROR " PROCESO CANCELADO ..."
         SLEEP 1
      END IF
   END IF 
   CLOSE WINDOW win5
END FUNCTION


FUNCTION Genera_salidas(vfolio)
   DEFINE
      vfolio                 INTEGER
 
   DEFINE reg_cza RECORD
      tipo_registro          CHAR(02),
      ident_servicio         CHAR(02),
      ident_operacion        CHAR(02),
      tipo_ent_origen        CHAR(02),
      clave_ent_origen       CHAR(03),
      tipo_ent_destino       CHAR(02),
      clave_ent_destino      CHAR(03),
      fech_trans             DATE,
      consecutivo_lote       CHAR(03),
      result_operacion       CHAR(02),
      diagnostico1           CHAR(03),
      diagnostico2           CHAR(03),
      diagnostico3           CHAR(03)
   END RECORD
  
   DEFINE reg_det RECORD
      tipo_registro          CHAR(2),
      consecutivo_lote       INTEGER,
      n_unico                CHAR(18),
      nss                    CHAR(11),
      paterno                CHAR(40),
      materno                CHAR(40),
      nombres                CHAR(40),
      sueldo_base_cot        DECIMAL(16,2),
      num_per_cotiza         SMALLINT,
      impt_aho_sol           DECIMAL(16,2),
      fecha_emi_resol        DATE,
      fecha_recep_afo        DATE,
      fol_resol              CHAR(8),
      result_operacion       CHAR(2),
      diagnostico1           CHAR(3),
      diagnostico2           CHAR(3),
      diagnostico3           CHAR(3)
   END RECORD

   DEFINE reg_sum RECORD
      tipo_registro          CHAR(02),
      tot_reg_archi          INTEGER,
      tot_aho_sol            DECIMAL(17,2),
      result_operacion       CHAR(2),
      diagnostico1           CHAR(3),
      diagnostico2           CHAR(3),
      diagnostico3           CHAR(3)
   END RECORD


   -------------------- GENERA RESPUESTA cza -----------------

   LET reg_cza.tipo_registro     = "01"
   LET reg_cza.ident_servicio    = "09"
   LET reg_cza.ident_operacion   = "13"
   LET reg_cza.tipo_ent_origen   = "01"
   LET reg_cza.clave_ent_origen  = vcod_afore
   LET reg_cza.tipo_ent_destino  = "03"
   LET reg_cza.clave_ent_destino = "001"
   LET reg_cza.fech_trans        = TODAY + 1
   LET reg_cza.consecutivo_lote  = 1
   LET reg_cza.result_operacion  = "00"
   LET reg_cza.diagnostico1      = "000"
   LET reg_cza.diagnostico2      = "000"
   LET reg_cza.diagnostico3      = "000"

   DISPLAY " GENERANDO CZA ..." #---AT 10,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/czaa"
   START REPORT rep_cza TO vreporte
      OUTPUT TO REPORT rep_cza(reg_cza.*)
   FINISH REPORT rep_cza


   -------------------- GENERA RESPUESTA det -----------------

   DECLARE cur_det CURSOR FOR
   SELECT
      "02",
      a.consecutivo_lote,
      b.n_unico,
      a.nss,
      b.paterno,
      b.materno,
      b.nombres,
      "",
      "",
      a.monto_en_pesos,
      "",
      "",
      "",
      "00",
      "000",
      "000",
      "000"
   FROM  dis_cuenta a, afi_mae_afiliado b
   WHERE a.folio = vfolio
   AND   a.nss   = b.n_seguro
   ORDER BY a.consecutivo_lote

   DISPLAY " GENERANDO DET..." #---AT 12,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/deta"

   START REPORT rep_det TO vreporte
      FOREACH cur_det INTO reg_det.*
         SELECT sdo_bas, meses09, folio_resol, fecha_resol, TODAY
         INTO reg_det.sueldo_base_cot, reg_det.num_per_cotiza, 
              reg_det.fol_resol,reg_det.fecha_emi_resol,reg_det.fecha_recep_afo
         FROM dis_aho_sol
         WHERE folio = vfolio
         AND nss = reg_det.nss
 
         OUTPUT TO REPORT rep_det(reg_det.*)
      END FOREACH
   FINISH REPORT rep_det


   --------------- GENERA RESPUESTA SUM       ---------------------

   DISPLAY " GENERANDO SUM..." #----AT 16,15
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/suma"
   START REPORT rep_sum TO vreporte

      LET reg_sum.tipo_registro     = "09"
      LET reg_sum.tot_reg_archi     = vnum_reg            
      LET reg_sum.tot_aho_sol       = monto_aho_sol
      LET reg_sum.result_operacion  = "00"
      LET reg_sum.diagnostico1      = "000"
      LET reg_sum.diagnostico2      = "000"
      LET reg_sum.diagnostico3      = "000"

      OUTPUT TO REPORT rep_sum(reg_sum.*)

   FINISH REPORT rep_sum


   ---------------- Generacion del Archivo -------------------------

   LET vnom_archivo = TODAY USING "YYYYMMDD" CLIPPED,
                      ".AHOSOL" CLIPPED

   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; cat czaa deta suma > ",vnom_archivo CLIPPED
   RUN ejecuta
   DISPLAY " ARCHIVO GENERADO...",gparam_dis.ruta_envio CLIPPED,"/",vnom_archivo
   DISPLAY ""
   LET ejecuta = "cd ",gparam_dis.ruta_envio CLIPPED,"/",
                 "; rm czaa deta suma "
   RUN ejecuta
END FUNCTION


FUNCTION rep_sols()
   DEFINE
      reg_sol  RECORD LIKE dis_aho_sol.*

   DISPLAY " GENERANDO REPORTE..." #----AT 16,15
   SLEEP 3
   DECLARE cur_rr CURSOR FOR 
   SELECT * FROM dis_aho_sol
   ORDER BY estado,nss
   
   LET vreporte = gparam_dis.ruta_envio CLIPPED,"/repsol_ahosol.txt"
   START REPORT rep_sol TO vreporte
      FOREACH cur_rr INTO reg_sol.*
         OUTPUT TO REPORT rep_sol(reg_sol.*)
      END FOREACH
   FINISH REPORT rep_sol
   PROMPT " REPORTE GENERADO EN /safre_prc/dis/envio/repsol_ahosol.txt" FOR CHAR opc
END FUNCTION


REPORT rep_cza(reg_cza)
   DEFINE reg_cza RECORD
      tipo_registro      CHAR(02),
      ident_servicio     CHAR(02),
      ident_operacion    CHAR(02),
      tipo_ent_origen    CHAR(02),
      clave_ent_origen   CHAR(03),
      tipo_ent_destino   CHAR(02),
      clave_ent_destino  CHAR(03),
      fech_trans         DATE,
      consecutivo_lote   CHAR(03),
      result_operacion   CHAR(02),
      diagnostico1       CHAR(03),
      diagnostico2       CHAR(03),
      diagnostico3       CHAR(03)
   END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         PRINT COLUMN 1,reg_cza.tipo_registro,
                        reg_cza.ident_servicio,
                        reg_cza.ident_operacion,
                        reg_cza.tipo_ent_origen,
                        reg_cza.clave_ent_origen,
                        reg_cza.tipo_ent_destino,
                        reg_cza.clave_ent_destino,
                        reg_cza.fech_trans       USING "YYYYMMDD",
                        reg_cza.consecutivo_lote USING "&&&",
                        reg_cza.result_operacion,
                        reg_cza.diagnostico1,
                        reg_cza.diagnostico2,
                        reg_cza.diagnostico3,
                        262 SPACES
END REPORT


REPORT rep_det(reg_det)
   DEFINE reg_det RECORD
      tipo_registro      CHAR(2),
      consecutivo_lote   INTEGER,
      n_unico            CHAR(18),
      nss                CHAR(11),
      paterno            CHAR(40),
      materno            CHAR(40),
      nombres            CHAR(40),
      sueldo_base_cot    DECIMAL(16,2),
      num_per_cotiza     SMALLINT,
      impt_aho_sol       DECIMAL(16,2),
      fecha_emi_resol    DATE,
      fecha_recep_afo    DATE,
      fol_resol          CHAR(8),
      result_operacion   CHAR(2),
      diagnostico1       CHAR(3),
      diagnostico2       CHAR(3),
      diagnostico3       CHAR(3)
   END RECORD,

   csdo_base_cot         CHAR(08),
   xsdo_base_cot         CHAR(07),
   cimpt_aho_sol         CHAR(13),
   ximpt_aho_sol         CHAR(12) 

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW

   LET vnum_reg = vnum_reg + 1
 
   LET csdo_base_cot  = reg_det.sueldo_base_cot USING '&&&&&.&&'
   LET xsdo_base_cot  = csdo_base_cot[1,5] , csdo_base_cot[7,8]

   LET cimpt_aho_sol  = reg_det.impt_aho_sol USING '&&&&&&&&&&.&&'
   LET ximpt_aho_sol  = cimpt_aho_sol[1,10] , cimpt_aho_sol[12,13]

   LET monto_aho_sol  = monto_aho_sol + reg_det.impt_aho_sol 

   IF reg_det.nss[1] = "I" THEN
      LET reg_det.nss = "00000000000"
   END IF

         PRINT COLUMN 1,reg_det.tipo_registro,
                     reg_det.consecutivo_lote USING "&&&&&&&&",
                     reg_det.n_unico,
                     reg_det.nss,
                     reg_det.paterno,
                     reg_det.materno, 
                     reg_det.nombres,
                     xsdo_base_cot,
                     reg_det.num_per_cotiza USING "&&&&&&",
                     ximpt_aho_sol,
                     reg_det.fecha_emi_resol USING "YYYYMMDD",
                     reg_det.fecha_recep_afo USING "YYYYMMDD",
                     reg_det.fol_resol USING "&&&&&&&&",
                     reg_det.result_operacion,
                     reg_det.diagnostico1,
                     reg_det.diagnostico2,
                     reg_det.diagnostico3,
                     81 SPACES
END REPORT


REPORT rep_sum(reg_sum)
   DEFINE
      reg_sum RECORD
      tipo_registro      CHAR(02),
      tot_reg_archi      INTEGER,
      tot_aho_sol        DECIMAL(15,2),
      result_operacion   CHAR(2),
      diagnostico1       CHAR(3),
      diagnostico2       CHAR(3),
      diagnostico3       CHAR(3)
   END RECORD,

   c16_tot_aho_sol       CHAR(16),
   c15_tot_aho_sol       CHAR(15)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW

         IF reg_sum.tot_aho_sol IS NULL THEN
            LET reg_sum.tot_aho_sol = 0
         END IF

         LET c16_tot_aho_sol= reg_sum.tot_aho_sol USING "&&&&&&&&&&&&&.&&"
         LET c15_tot_aho_sol= c16_tot_aho_sol[1,13] , c16_tot_aho_sol[15,16]
 
         PRINT
            COLUMN 1,reg_sum.tipo_registro,
                     reg_sum.tot_reg_archi   USING "&&&&&&&&",
                     c15_tot_aho_sol,
                     reg_sum.result_operacion,
                     reg_sum.diagnostico1,
                     reg_sum.diagnostico2,
                     reg_sum.diagnostico3,
                     264 spaces
END REPORT


REPORT rep_sol(reg_sol)
   DEFINE
      reg_sol RECORD LIKE dis_aho_sol.*

   OUTPUT
      LEFT MARGIN    0
      RIGHT MARGIN   0
      TOP MARGIN     0
      BOTTOM MARGIN  0
      PAGE LENGTH   60

   FORMAT
      PAGE HEADER
         PRINT COLUMN 23,"REPORTE DE SOLICITUDES CAPTURADAS DE AHORRO SOLIDARIO"
         SKIP 2 LINES
      
      ON EVERY ROW
         PRINT COLUMN  1,reg_sol.folio USING "########",
               COLUMN 10,reg_sol.nss,
               COLUMN 23,reg_sol.curp,
               COLUMN 43,reg_sol.nombre,
               COLUMN 75,reg_sol.mon_aho_sol USING "#########.##",
               COLUMN 97,reg_sol.estado
END REPORT


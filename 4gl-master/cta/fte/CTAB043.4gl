-------------------------------------------------------------------------------
-- Proyecto      AFORES
-- Propietario   EFP
-- Programa      CTAB043
-- Descripcion   BATCH DE PRELIQUIDACION IDENTIFICACION POR EDAD
-- Fecha         26 agosto 2010
-- hecho por     ARMANDO RODRIGUEZ CASTROPAREDES
-- Sistema       CTA
-------------------------------------------------------------------------------
DATABASE  safre_af

GLOBALS
   DEFINE
      g_reg RECORD
         folio_min          INTEGER,
         folio_max          INTEGER
      END RECORD

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE 
      hoy               DATE,
      usuario           CHAR(08),
      opc               CHAR(01),
      vdesmarca         CHAR(100),
      ejecuta           CHAR(200),
      comando           CHAR(200),
      hora_inicial      CHAR(08),
      hora_final        CHAR(08),
      aux_subct_desc    CHAR(03),
      hist_fecha_aplica DATE,
      vsubcuenta        CHAR(03),
      vtipo             CHAR(01),
      vfecha_liquida    DATE
   DEFINE vrecaudax,
          vestado,
          vmarca  ,
          vgeneracion   SMALLINT

   DEFINE vfolio_liquida INTEGER

END GLOBALS

MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
   DEFER INTERRUPT

   SELECT *,
          USER
   INTO   g_param.*, 
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET vdesmarca     = " EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "
   LET hoy = TODAY
   LET vfecha_liquida = hoy

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "CTAB0431" ATTRIBUTE(BORDER)

   DISPLAY " CTAB043           LIQUIDACION BATCH DE IDENTIFICACION POR EDAD                " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "IDENTIFICACION POR EDAD"
      COMMAND "Preliquida" "Genera Preliquidacion de Identificacion por edad"
         CALL preliquida()
      COMMAND "Liquida" "Liquida la Identificacion por edad"
         CALL liquida()
      COMMAND "Salir" "Salida del programa"
         EXIT MENU
   END MENU
END MAIN

FUNCTION preliquida()

   INPUT BY NAME vfecha_liquida WITHOUT DEFAULTS

      AFTER FIELD vfecha_liquida 
         IF vfecha_liquida IS NULL THEN
            ERROR "DEBE PONER ALGUNA FECHA PARA EJECUTAR"
            SLEEP 2
            NEXT FIELD vfecha_liquida
         END IF

         #IF vfecha_liquida >= hoy THEN
         IF vfecha_liquida <= hoy THEN

         SELECT "X"
         FROM   safre_tmp:tmp_saldo_edad
         WHERE  fecha_conversion = vfecha_liquida
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            ERROR "NO HAY SALDOS PARA LA LIQUIDACION"
            SLEEP 3
            EXIT PROGRAM
         END IF

            LET vmarca = 0

            SELECT "X"
            FROM   tes_solicitud
            WHERE  tipo_traspaso = 13
            AND    estado  = 100
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               LET vmarca = 1
            END IF

            IF vmarca = 0  THEN
               ERROR "NO HAY CUENTAS PENDIENTES DE TRANSFERENCIA POR EDAD"
               SLEEP 3
               EXIT PROGRAM
            END IF

            SELECT MAX(folio) + 1
            INTO   vfolio_liquida
            FROM   glo_folio

            IF vfolio_liquida IS NULL THEN
               LET vfolio_liquida = 1
            END IF

            INSERT INTO glo_folio VALUES(vfolio_liquida)
            DISPLAY "Folio : ", vfolio_liquida AT 18,2 
            SLEEP 3
            ERROR "Procesando informacion ..."

            CALL envia_batch()

         END IF
         EXIT INPUT
      ON KEY(INTERRUPT)
      ERROR "PROCESO CANCELADO"
      SLEEP 2
      EXIT PROGRAM

   END INPUT

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
   
FUNCTION liquida()
DEFINE vfolio   INTEGER

        #DISPLAY "                                                                          " AT  6,1 ATTRIBUTE(REVERSE)
        OPEN WINDOW v16 AT   9,12 WITH FORM "CTAB0432"
        DISPLAY "      LIQUIDACION IDENTIFICACION EDAD      " AT 1,1 ATTRIBUTE (REVERSE) 

   INPUT BY NAME vfolio WITHOUT DEFAULTS

      AFTER FIELD vfolio 
         IF vfolio IS NULL THEN
            ERROR "DEBE PONER EL FOLIO PARA EJECUTAR"
            SLEEP 2
            NEXT FIELD vfolio
         END IF

         SELECT "X"
         FROM    dis_ctrl_proceso a
         WHERE   a.folio = vfolio
         AND     a.hora_final IS NULL
         GROUP BY 1

         IF STATUS =  NOTFOUND THEN
            ERROR "NO HA CONCLUIDO LA PRELIQUIDACION"
            SLEEP 3
            EXIT PROGRAM
         END IF

         CALL reg_liquida(vfolio)

         EXIT INPUT
      ON KEY(INTERRUPT)
      ERROR "PROCESO CANCELADO"
      SLEEP 2
      EXIT PROGRAM

   END INPUT

   CLEAR FORM
   CLEAR SCREEN

END FUNCTION

FUNCTION reg_liquida(rfolio)

DEFINE  rfolio     INTEGER
DEFINE  xnss       CHAR(11)
DEFINE  xgrupo     SMALLINT
DEFINE  xsolicitud SMALLINT

   ERROR "Carga liquidacion..."

   INSERT INTO dis_cuenta
   SELECT *
   FROM   dis_provision
   WHERE  folio = rfolio

   ERROR "Cierra marcas..."

   DECLARE cur_1 CURSOR FOR
   SELECT nss,
          grupo_regimen,
          folio_solicitud
   FROM   tes_solicitud
   WHERE  folio   = rfolio
   AND    tipo_traspaso = 13
   AND    estado = 102
   ORDER BY 1

   FOREACH cur_1 INTO  xnss,
                       xgrupo,
                       xsolicitud
      CALL actualiza_marca(xnss,
                           xgrupo,
                           xsolicitud)

   END FOREACH

   UPDATE tes_solicitud
   SET    estado = 103
   WHERE  folio  = rfolio
   AND    tipo_traspaso = 13
   AND    estado = 102

END FUNCTION

FUNCTION envia_batch()
   DEFINE xfolio_dia  SMALLINT

   LET hora_inicial = TIME
   LET hora_final   = NULL
   LET xfolio_dia   =  0

   INSERT INTO dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           "CTAC043",               -- proceso_cod
           1,                       -- etapa_cod   -- EJECUTA NOHUP
           hora_inicial,            -- hora_inicial
           hora_final,              -- hora_final
           vfecha_liquida,          -- parametro1
           NULL,                    -- parametro2
           NULL,                    -- parametro3
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           vfolio_liquida,          -- folio
           NULL,                    -- resultado
           USER,                    -- usuario
           0                        -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR NO INSERTO EN LA TABLA DE CONTROL",STATUS
      SLEEP 3
      EXIT PROGRAM
   END IF

   ERROR "Ejecutando nueva sesion de estado de cuenta por nohup ..."
   SLEEP 3

   LET ejecuta = "nohup time fglgo CTAC043.4gi    " CLIPPED,
                  vfolio_liquida CLIPPED," ", vfecha_liquida CLIPPED," &"

   RUN ejecuta

   ERROR "El proceso se ejecuto satisfactoriamente por nohup"

   SLEEP 2
   ERROR ""
   EXIT PROGRAM

END FUNCTION
-----------------------------------------------------------------
FUNCTION actualiza_marca(reg_7)

   DEFINE reg_7 RECORD
      nss               CHAR(11),
      grupo             SMALLINT,
      correlativo       INTEGER
   END RECORD
   DEFINE xmarca        SMALLINT
   DEFINE xmarca_causa  SMALLINT
   DEFINE vestado_marca SMALLINT

      LET xmarca_causa   = 0
      LET vestado_marca  = 0

      SELECT marca_cod
      INTO   xmarca
      FROM   tab_grupo_regimen 
      WHERE  grupo_regimen = reg_7.grupo

      PREPARE marcaje_ret FROM vdesmarca
      EXECUTE marcaje_ret USING reg_7.nss,         # nss
                                xmarca,            # marca_entra
                                reg_7.correlativo, # correlativo
                                vestado_marca,     # estado_marca
                                xmarca_causa,      # marca_causa
                                usuario            # usuario

END FUNCTION

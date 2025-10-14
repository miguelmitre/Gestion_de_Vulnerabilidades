-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB097                                                   --
-- Descripcion  => PROCESO BATCH GENERA ARCHIVO DE CONCILIACION SALDOS FOV.  -- 
-- Sistema      => DIS                                                       --
-- Por          => DMR                                                       --
-- Fecha        => 22 Febrero 2008.                                          --
-- Fecha Mod    => 04 Noviembre 2008. (Pesos y Aivs subctas 14 y 35)         --
-------------------------------------------------------------------------------
DATABASE  safre_af

GLOBALS
   DEFINE g_reg RECORD
      fecha_aplicacion   DATE
   END RECORD

   DEFINE g_param RECORD LIKE seg_modulo.*

   DEFINE vcod_afore     LIKE tab_afore_local.codigo_afore

   DEFINE 
      hoy               DATE,
      fecha_1mes        DATE,
      usuario           CHAR(08),
      opc               CHAR(01),
      ejecuta           CHAR(200),
      hora_inicial      CHAR(08),
      xhora_final       CHAR(08)

   DEFINE cont_reg      INTEGER
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
   DEFER INTERRUPT

   CALL STARTLOG("DISB097.log")
   SELECT *,
          USER
   INTO   g_param.*,
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   SELECT codigo_afore
   INTO vcod_afore
   FROM tab_afore_local

   LET hoy = TODAY

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "DISB0971" ATTRIBUTE(BORDER)
   DISPLAY " DISB097      GENERACION ARCHIVO DE CONCILIACION SALDOS FOV.                       " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "PRINCIPAL"
      COMMAND "Generar"                  "Generar Archivo"
         CALL Generar_archivo()
      COMMAND "Salida"                   "Regresa menu anterior"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN


FUNCTION Generar_archivo()
   LET INT_FLAG = FALSE
   INPUT BY NAME g_reg.*

      AFTER FIELD fecha_aplicacion
         IF g_reg.fecha_aplicacion IS NULL THEN
            ERROR "Este campo no puede ser NULO"
            NEXT FIELD fecha_aplicacion
         ELSE
            LET fecha_1mes = MDY(MONTH (g_reg.fecha_aplicacion) USING "##",01,YEAR (g_reg.fecha_aplicacion) USING "####")

            SELECT "OK"
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = fecha_1mes
            AND    codigo_siefore  = 12
            GROUP BY 1
     
            IF STATUS = NOTFOUND THEN
               ERROR " NO existe precio de AIVS FOVISSSTE ",fecha_1mes USING "DD/MM/YYYY"
               NEXT FIELD fecha_aplicacion
            END IF
         END IF

      ON KEY (ESC)
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

   PROMPT "Desea ejecutar el Proceso [S/N] ... " FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL Ejecuta_generar_arch()
   END IF
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION


FUNCTION Ejecuta_generar_arch()
   LET hora_inicial = TIME
   LET xhora_final  = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB097",               -- proceso_cod
       1,                       -- etapa_cod
       hora_inicial,            -- hora_inicial
       xhora_final,             -- hora_final
       "14",                    -- parametro1
       g_reg.fecha_aplicacion,  -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Generar_Arc" ,STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   CALL Proceso_principal()
        
   RUN ejecuta
   ERROR "El Proceso se ejecuto Satisfactoriamente "
   SLEEP 3
   ERROR ""
   
   LET xhora_final = TIME
  
   UPDATE dis_ctrl_proceso
   SET   hora_final  = xhora_final
   WHERE proceso_cod = "DISB097"
   AND   parametro2  = g_reg.fecha_aplicacion
END FUNCTION


FUNCTION Proceso_principal()
#cp-------------------
    DEFINE ejecuta               CHAR(200)
    DEFINE ch                    CHAR(100)
    DEFINE nssp                  CHAR(11)
    DEFINE G_LISTA               CHAR(200)
    DEFINE G_LISTA2              CHAR(200)
    DEFINE G_LISTA3              CHAR(200)
    DEFINE vrepor_cza            CHAR(200)
    DEFINE vrepor_det            CHAR(200)
    DEFINE vrepor_sum            CHAR(200)
    DEFINE interes_fov           DECIMAL(16,6)
    DEFINE val_aiv_fov           LIKE glo_valor_accion.precio_del_dia

    DEFINE reg_1  RECORD
                     curp         CHAR(18), 
                     nss          CHAR(11),
                     saldo_viv97  DECIMAL(16,2),
                     saldo_viv92  DECIMAL(16,2),
                     saldo_fovi92 DECIMAL(16,2),
                     saldo_fovi08 DECIMAL(16,2),
                     aivs_viv97   DECIMAL(20,6),
                     aivs_viv92   DECIMAL(20,6),
                     aivs_fovi92  DECIMAL(20,6),
                     aivs_fovi08  DECIMAL(20,6)
                  END RECORD   

    DEFINE reg_s  RECORD
                     tot_sdo_viv97  DECIMAL(16,2),
                     tot_sdo_viv92  DECIMAL(16,2),
                     tot_sdo_fovi92 DECIMAL(16,2),
                     tot_sdo_fovi08 DECIMAL(16,2),
                     tot_aiv_viv97  DECIMAL(24,6),
                     tot_aiv_viv92  DECIMAL(24,6),
                     tot_aiv_fovi92 DECIMAL(24,6),
                     tot_aiv_fovi08 DECIMAL(24,6)
                  END RECORD
 
    LET reg_s.tot_sdo_viv97  = 0
    LET reg_s.tot_sdo_viv92  = 0
    LET reg_s.tot_sdo_fovi92 = 0
    LET reg_s.tot_sdo_fovi08 = 0
    LET reg_s.tot_aiv_viv97  = 0
    LET reg_s.tot_aiv_viv92  = 0
    LET reg_s.tot_aiv_fovi92 = 0
    LET reg_s.tot_aiv_fovi08 = 0
    LET interes_fov          = 0

    WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
    
    DROP TABLE nss_uni_fovi
    CREATE TABLE nss_uni_fovi
    (
     nss   CHAR(11)
    )
   
    DATABASE safre_af
    WHENEVER ERROR STOP

    LET cont_reg = 0
    LET G_LISTA = g_param.ruta_envio CLIPPED,"/",g_reg.fecha_aplicacion USING "DDMMYYYY",".CONCILIA" 

    LET G_LISTA2= g_param.ruta_envio CLIPPED,"/","NSS_FOVI.TXT"

    UNLOAD TO G_LISTA2
    SELECT nss
    FROM   dis_cuenta
    WHERE  nss IS NOT NULL
    AND    subcuenta IN (14,35)

    LET ejecuta = "sort -u ",g_param.ruta_envio CLIPPED,"/NSS_FOVI.TXT ",
                  " > ",g_param.ruta_envio CLIPPED,"/NSS_UNI_FOV.TXT "
    run ejecuta

    LET G_LISTA3 = g_param.ruta_envio CLIPPED,"/","NSS_UNI_FOV.TXT"

    LOAD FROM G_LISTA3
    INSERT INTO safre_tmp:nss_uni_fovi

    DATABASE safre_tmp
    CREATE index indc1 ON nss_uni_fovi(nss) 
    DATABASE safre_af

-------------------------------------------------------------  ENCABEZADO
    DISPLAY "GENERANDO CZA_CONCI..."    #---AT 10,15
    LET vrepor_cza = g_param.ruta_envio CLIPPED,"/cza_conci"
    START REPORT rep_cza TO vrepor_cza
       OUTPUT TO REPORT rep_cza()
    FINISH REPORT rep_cza

-------------------------------------------------------------  DETALLE
    DISPLAY "GENERANDO DET_CONCI..."    #---AT 10,15
    LET vrepor_det = g_param.ruta_envio CLIPPED,"/det_conci"
    START REPORT rep_det TO vrepor_det
       DECLARE cur CURSOR FOR
       SELECT nss FROM safre_tmp:nss_uni_fovi
--     ORDER BY 1

       FOREACH cur INTO nssp
          INITIALIZE reg_1 TO NULL

          SELECT n_unico
          INTO   reg_1.curp
          FROM   afi_mae_afiliado
          WHERE  n_seguro = nssp

          LET reg_1.nss = nssp

          LET reg_1.saldo_viv97   = 0          -- saldo en cero
          LET reg_s.tot_sdo_viv97 = 0
 
          LET reg_1.saldo_viv92   = 0          -- saldo en cero
          LET reg_s.tot_sdo_viv92 = 0

          LET reg_1.aivs_viv97    = 0          -- saldo en cero
          LET reg_s.tot_aiv_viv97 = 0

          LET reg_1.aivs_viv92    = 0          -- saldo en cero
          LET reg_s.tot_aiv_viv92 = 0

          LET fecha_1mes = MDY(MONTH (g_reg.fecha_aplicacion) USING "##",01,YEAR(g_reg.fecha_aplicacion) USING "####")

          SELECT NVL(precio_del_dia,0)
          INTO   val_aiv_fov
          FROM   glo_valor_accion   --gvp 100908 antes glo_valor_acc_fov
          WHERE  fecha_valuacion = fecha_1mes
          and    codigo_siefore  = 12

          SELECT NVL(sum(monto_en_acciones), 0)
          INTO   reg_1.aivs_fovi92
          FROM   dis_cuenta
          WHERE  nss       = nssp
          AND    subcuenta = 14
          AND    fecha_conversion <= g_reg.fecha_aplicacion

          LET reg_1.saldo_fovi92 = reg_1.aivs_fovi92 * val_aiv_fov

          SELECT NVL(sum(monto_en_acciones), 0)
          INTO   reg_1.aivs_fovi08
          FROM   dis_cuenta
          WHERE  nss       = nssp
          AND    subcuenta = 35
          AND    fecha_conversion <= g_reg.fecha_aplicacion

          LET reg_1.saldo_fovi08 = reg_1.aivs_fovi08 * val_aiv_fov

          IF reg_1.aivs_fovi92 <> 0 OR reg_1.aivs_fovi08 <> 0 THEN

             LET reg_s.tot_sdo_fovi92 = reg_s.tot_sdo_fovi92 +
                                        (reg_1.saldo_fovi92 * 100)

             LET reg_s.tot_aiv_fovi92 = reg_s.tot_aiv_fovi92 +
                                        (reg_1.aivs_fovi92 * 1000000)

             LET reg_s.tot_sdo_fovi08 = reg_s.tot_sdo_fovi08 +
                                        (reg_1.saldo_fovi08 * 100)

             LET reg_s.tot_aiv_fovi08 = reg_s.tot_aiv_fovi08 +
                                        (reg_1.aivs_fovi08 * 1000000)

             LET cont_reg = cont_reg + 1
             OUTPUT TO REPORT rep_det(reg_1.*)
          END IF
       END FOREACH
    FINISH REPORT rep_det

-------------------------------------------------------------  SUMARIO
    DISPLAY "GENERANDO SUM_CONCI..."    #---AT 10,15
    LET vrepor_sum = g_param.ruta_envio CLIPPED,"/sum_conci"
    START REPORT rep_sum TO vrepor_sum
       OUTPUT TO REPORT rep_sum(reg_s.*)
    FINISH REPORT rep_sum

-------------------------------------------------------------  GENERAR ARCHIVO

    LET ejecuta = "cd ",g_param.ruta_envio CLIPPED,"/",
                  "; cat cza_conci det_conci sum_conci > ",G_LISTA CLIPPED
    RUN ejecuta

    LET ch = "chmod 777 ",G_LISTA
    run ch
    
--  LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",G_LISTA_2 CLIPPED
--  run borra_lineas
END FUNCTION


REPORT rep_cza()
   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         PRINT
            COLUMN 001,"01"                      , -- Encabezado
            COLUMN 003,"12"                      , -- Saldos de Vivienda
            COLUMN 005,"01"                      , -- Saldos
            COLUMN 007,"01"                      , -- Afore
            COLUMN 009,vcod_afore USING"&&&"     , -- Clave entidad origen
            COLUMN 012,"03"                      , -- Tipo entidad destino
            COLUMN 014,"001"                     , -- Clave entidad destino
            COLUMN 017,HOY USING "YYYYMMDD"      , -- Fecha de operacion
            COLUMN 025,"001"                     , -- Consecutivo dia 
            COLUMN 028,"  "                      , -- Resultado Operacion
            COLUMN 030,"   "                     , -- Motivo Rechazo1 lote
            COLUMN 033,"   "                     , -- Motivo Rechazo2 lote
            COLUMN 036,"   "                     , -- Motivo Rechazo3 lote
            COLUMN 039, 162 SPACES
END REPORT


REPORT rep_det(reg_2)
#ld---------------------------
   DEFINE reg_2 RECORD #loc #reg_2
                   curp         CHAR(18), 
                   nss          CHAR(11),
                   saldo_viv97  DECIMAL(16,2),
                   saldo_viv92  DECIMAL(16,2),
                   saldo_fovi92 DECIMAL(16,2),
                   saldo_fovi08 DECIMAL(16,2),
                   aivs_viv97   DECIMAL(20,6),
                   aivs_viv92   DECIMAL(20,6),
                   aivs_fovi92  DECIMAL(20,6),
                   aivs_fovi08  DECIMAL(20,6)
                END RECORD

   DEFINE c15_saldo_fovi92      CHAR(15)
   DEFINE c18_aivs_fovi92       CHAR(18)
   DEFINE c15_saldo_fovi08      CHAR(15)
   DEFINE c18_aivs_fovi08       CHAR(18)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
         LET reg_2.saldo_viv97 = reg_2.saldo_viv97 * 100
         LET reg_2.saldo_viv92 = reg_2.saldo_viv92 * 100

         LET reg_2.saldo_fovi92 = reg_2.saldo_fovi92 * 100

         IF reg_2.saldo_fovi92 < 0 THEN
            LET c15_saldo_fovi92 = "-",reg_2.saldo_fovi92 USING "&&&&&&&&&&&&&&"
         ELSE
            LET c15_saldo_fovi92 = reg_2.saldo_fovi92 USING "&&&&&&&&&&&&&&&"
         END IF

         LET reg_2.aivs_viv97 = reg_2.aivs_viv97 * 1000000
         LET reg_2.aivs_viv92 = reg_2.aivs_viv92 * 1000000

         LET reg_2.aivs_fovi92= reg_2.aivs_fovi92 * 1000000

         IF reg_2.aivs_fovi92 < 0 THEN
            LET c18_aivs_fovi92= "-",reg_2.aivs_fovi92 USING "&&&&&&&&&&&&&&&&&"
         ELSE
            LET c18_aivs_fovi92= reg_2.aivs_fovi92 USING "&&&&&&&&&&&&&&&&&&"
         END IF


         LET reg_2.saldo_fovi08 = reg_2.saldo_fovi08 * 100

         IF reg_2.saldo_fovi08 < 0 THEN
            LET c15_saldo_fovi08 = "-",reg_2.saldo_fovi08 USING "&&&&&&&&&&&&&&"
         ELSE
            LET c15_saldo_fovi08 = reg_2.saldo_fovi08 USING "&&&&&&&&&&&&&&&"
         END IF

         LET reg_2.aivs_fovi08 = reg_2.aivs_fovi08 * 1000000

         IF reg_2.aivs_fovi08 < 0 THEN
            LET c18_aivs_fovi08= "-",reg_2.aivs_fovi08 USING "&&&&&&&&&&&&&&&&&"
         ELSE
            LET c18_aivs_fovi08= reg_2.aivs_fovi08 USING "&&&&&&&&&&&&&&&&&&"
         END IF

         PRINT
            COLUMN 001,"02"                                     ,-- Detalle
            COLUMN 003,reg_2.curp                               ,-- CURP
            COLUMN 021,reg_2.nss                                ,-- NSS
            COLUMN 032,reg_2.saldo_viv97 USING "&&&&&&&&&&&&&&&",-- Saldo Viv97
            COLUMN 047,reg_2.saldo_viv92 USING "&&&&&&&&&&&&&&&",-- Saldo Viv92
            COLUMN 062,c15_saldo_fovi92                         ,-- Saldo Fovi92
            COLUMN 077,c15_saldo_fovi08                         ,-- Saldo Fovi08
            COLUMN 092,reg_2.aivs_viv97 USING "&&&&&&&&&&&&&&&&&&" ,-- Aivs_97
            COLUMN 110,reg_2.aivs_viv92 USING "&&&&&&&&&&&&&&&&&&" ,-- Aivs_92
            COLUMN 128,c18_aivs_fovi92                             ,--Aivs_Fov92
            COLUMN 146,c18_aivs_fovi08                             ,--Aivs_Fov08
            COLUMN 164,"  "                                     ,-- Resul. Ope.
            COLUMN 166,"               "                        ,-- Diag. Proc.
            COLUMN 181,20 SPACES
END REPORT


REPORT rep_sum(reg_t)
   DEFINE reg_t RECORD
                   tot_sdo_viv97  DECIMAL(16,2),
                   tot_sdo_viv92  DECIMAL(16,2),
                   tot_sdo_fovi92 DECIMAL(16,2),
                   tot_sdo_fovi08 DECIMAL(16,2),
                   tot_aiv_viv97  DECIMAL(24,6),
                   tot_aiv_viv92  DECIMAL(24,6),
                   tot_aiv_fovi92 DECIMAL(24,6),
                   tot_aiv_fovi08 DECIMAL(24,6)
                END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
      ON EVERY ROW
        PRINT
            COLUMN 001,"09"                        ,-- Sumario
            COLUMN 003,cont_reg USING "&&&&&&&&&"  ,-- Total de Registros
            COLUMN 012,reg_t.tot_sdo_viv97 USING "&&&&&&&&&&&&&&&" ,-- Tot Viv97
            COLUMN 027,reg_t.tot_sdo_viv92 USING "&&&&&&&&&&&&&&&" ,-- Tot Viv92
            COLUMN 042,reg_t.tot_sdo_fovi92 USING "&&&&&&&&&&&&&&&",-- Tot Fov92
            COLUMN 057,reg_t.tot_sdo_fovi08 USING "&&&&&&&&&&&&&&&",-- Tot Fov08
            COLUMN 072,reg_t.tot_aiv_viv97 USING "&&&&&&&&&&&&&&&&&&&&&" ,--aivs
            COLUMN 093,reg_t.tot_aiv_viv92 USING "&&&&&&&&&&&&&&&&&&&&&" ,--aivs
            COLUMN 114,reg_t.tot_aiv_fovi92 USING "&&&&&&&&&&&&&&&&&&&&&",--aivs
            COLUMN 135,reg_t.tot_aiv_fovi08 USING "&&&&&&&&&&&&&&&&&&&&&",--aivs
            COLUMN 156,45 SPACES
END REPORT


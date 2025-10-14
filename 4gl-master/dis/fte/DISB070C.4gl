-------------------------------------------------------------------------------
-- Proyecto     => AFORES                                                    --
-- Propietario  => E.F.P.                                                    --
-- Programa     => DISB070C                                                  --
-- Descripcion  => PROGRAMA DE CONTROL DE CONSULTAS SDO CERO                 --
-- Fecha        => 10 Ene  2005.                                             --
-- By           => ALEJANDRO RAMIREZ                                         --
-- Sistema      => DIS                                                       --
-------------------------------------------------------------------------------
DATABASE  safre_af

 GLOBALS
      DEFINE opc       CHAR(1)
      DEFINE ENTER     CHAR(1)
      DEFINE opc2      CHAR(1)
      DEFINE forma1    CHAR(50)
      DEFINE comando   CHAR(100)
      DEFINE g_usuario      CHAR (08)
      DEFINE g_param_dis    RECORD LIKE dis_parametro.*
      DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore
      DEFINE HOY            DATE

      DEFINE regi ARRAY[1] OF RECORD
             registros      INTEGER
      END RECORD




      DEFINE
          sw_periodo        SMALLINT

      DEFINE v RECORD
              fecha_proceso DATE,
              nss           CHAR(11)
      END RECORD

      DEFINE l_record_lper   ARRAY[10000] OF RECORD
              nss               CHAR(11),
              fecha_corte       DATE,
              fecha_proceso     DATE,
              tipo_nss          CHAR(2)
      END RECORD 
                 
      DEFINE l_record_per   ARRAY[10000] OF RECORD
              nss               CHAR(11),
              fecha_corte       DATE,
              fecha_proceso     DATE,
              tipo_nss          CHAR(2),
              desc              CHAR(40)
      END RECORD                  

      DEFINE nuevo RECORD 
              fecha_transf      DATE,
              consec_reg_lote   INTEGER,
              nss               CHAR(11),
              rfc_trab          CHAR(13),
              curp_trab         CHAR(18),
              nombre_trab       CHAR(50),
              status_cta        CHAR(2),
              status            CHAR(2),
              fecha_modif       DATE,
              status_envio      CHAR(1),
              periodo           CHAR(6),
              des_causa         CHAR(23)
      END RECORD                  
     
      DEFINE sql_text   CHAR(350) 
      DEFINE hora       CHAR (08)
      DEFINE G_IMPRE    CHAR(300)
      DEFINE impresion  CHAR(300)
      DEFINE 
        mes_env         CHAR(02),
        anio_env        INTEGER,
        dia_ult         CHAR(02)

      DEFINE pos        SMALLINT
      DEFINE ejecuta    CHAR(300)
      DEFINE contador   INTEGER 
 END GLOBALS 

MAIN

 DEFER INTERRUPT
 CALL inicializa()     

 OPEN WINDOW w_1 AT 3,4
    WITH 3 ROWS,72 COLUMNS
    ATTRIBUTE( BORDER)
    DISPLAY "                          CONSULTA DE CUENTAS " AT 3,1
    DISPLAY " DISB070C " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)
    LET sql_text        = null

    MENU "MENU "
      COMMAND "Ctas Sdo cero " "Consulta las cuentas con sdo cero"
        CALL Consulta1()
  --  COMMAND "Ctas con sdo"
  --    CALL Consulta2()
      COMMAND "Ctas rehabilitadas" "Consulta las cuentas rehabilitadas"
  --    CALL Consulta3()   TRAC011
      LET ejecuta = "fglgo TRAC011.4gi" 
      RUN ejecuta
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
    END MENU

    CLOSE WINDOW w_1 

 END MAIN

--*****************************************************************************
-- Inicializa
--*****************************************************************************

FUNCTION inicializa()
    OPTIONS
        INPUT WRAP,
        MESSAGE LINE LAST,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I,
        FORM LINE 3
        LET opc = NULL

 LET hora = TIME
 LET HOY  = TODAY
 LET contador = 0

 SELECT  ruta_listados
 INTO    g_param_dis.ruta_spool
 FROM    seg_modulo
 WHERE   modulo_cod='dis'


 SELECT  codigo_afore,USER
 INTO    w_codigo_afore,g_usuario
 FROM    tab_afore_local

 LET regi[1].registros = 0
 --DISPLAY registros TO scr_2.*

 WHENEVER ERROR CONTINUE
   DROP TABLE tt_arch_excel

 CREATE TEMP TABLE tt_arch_excel
 (
     nss               CHAR(11),
     fecha_corte       DATE,
     fecha_proceso     DATE,
     tipo_nss          CHAR(2),
     desc              CHAR(40)
 )

 WHENEVER ERROR STOP

 END FUNCTION


--*****************************************************************************
-- Consulta 1
--*****************************************************************************

FUNCTION Consulta1()

DEFINE
    nss_pru      CHAR(11),
    cont         integer

    OPEN WINDOW ventana_21 AT 9,4 WITH FORM "DISB070C1" ATTRIBUTE( BORDER)
    DISPLAY "              (Ctrl-C) Salir      ENTER Y ESC (Ejecutar)                " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag= FALSE

    INPUT BY NAME v.*

      ON KEY (ESC)

      IF v.nss IS NULL AND v.fecha_proceso IS NULL THEN
          ERROR "Al menos un dato debe accesarse"
          NEXT FIELD fecha_proceso
      END IF 


      ERROR "      PROCESANDO INFORMACION ...."
      SLEEP 2
      EXIT INPUT

      ON KEY (CONTROL-C) 
        LET int_flag = TRUE
        EXIT INPUT
    END INPUT


    IF int_flag = TRUE THEN
       LET int_flag = FALSE
       CLEAR FORM 
       CLEAR SCREEN
       CLOSE WINDOW ventana_21
       RETURN
    END IF


    WHENEVER ERROR CONTINUE
    DROP TABLE tt_cuenta

    LET sw_periodo = 0

    CASE WHEN v.fecha_proceso IS NOT NULL AND
              v.nss           IS NOT NULL 

         SELECT * 
         FROM   cta_nss_sdo_cero
         WHERE  nss           = v.nss
         AND    fecha_proceso = v.fecha_proceso
         INTO   TEMP tt_cuenta

        WHEN v.fecha_proceso IS NOT NULL AND
             v.nss           IS NULL 

          SELECT *
          FROM   cta_nss_sdo_cero
          WHERE  fecha_proceso = v.fecha_proceso
          INTO   TEMP tt_cuenta

        WHEN v.fecha_proceso IS NULL AND
             v.nss           IS NOT NULL 

          SELECT *
          FROM   cta_nss_sdo_cero
          WHERE  nss = v.nss
          INTO   TEMP tt_cuenta

        OTHERWISE
 
          ERROR "Proporcinar al menos un valor"
       
    END CASE




    LET pos =1
    INITIALIZE l_record_lper[pos].* TO NULL


    DECLARE cur_per CURSOR FOR
        SELECT * 
        FROM   tt_cuenta 
    FOREACH cur_per INTO l_record_lper[pos].*

         LET regi[1].registros = regi[1].registros + 1

         LET l_record_per[pos].nss            = l_record_lper[pos].nss
         LET l_record_per[pos].fecha_corte    = l_record_lper[pos].fecha_corte
         LET l_record_per[pos].fecha_proceso  = l_record_lper[pos].fecha_proceso
         LET l_record_per[pos].tipo_nss       = l_record_lper[pos].tipo_nss


         SELECT desc_tipo
         INTO   l_record_per[pos].desc
         FROM   tab_tipo_nss
         WHERE  cod_tipo_nss = l_record_lper[pos].tipo_nss
 
         --PARA EL ARCHIVO
         INSERT INTO tt_arch_excel
         VALUES(l_record_per[pos].nss,l_record_per[pos].fecha_corte,
                l_record_per[pos].fecha_proceso,l_record_per[pos].tipo_nss,
                l_record_per[pos].desc)


         LET pos = pos + 1
    END FOREACH


    INITIALIZE l_record_per[pos].*, l_record_lper[pos].* TO NULL
   

    IF (pos-1) >= 1 THEN

      CALL  SET_COUNT(pos-1)  --pepe
      OPEN WINDOW ventana_2 AT 9,4 WITH FORM "DISB070C2" ATTRIBUTE( BORDER)
      DISPLAY " CONSULTA " AT 1,40 ATTRIBUTE(REVERSE,green)
      DISPLAY " (Ctrl-C) Salir      (Ctrl-P) Imprime      (Ctrl-F) Archivo             " AT 1,1 ATTRIBUTE(REVERSE,green)

      DISPLAY " DISB070C2                                                              " AT 2,1 ATTRIBUTE(REVERSE,green)


      DISPLAY regi[1].registros TO scr_2.*  --ojo
      ERROR ""

      DISPLAY ARRAY l_record_per TO scr_1.*


         ON KEY (CONTROL-P)
            ERROR "          PROCESANDO IMPRESION...."
            SLEEP 2
            CALL imprime()

         ON KEY (CONTROL-F)
            ERROR "          PROCESANDO ARCHIVO...."
            SLEEP 2
            CALL archivo()

        ON KEY (INTERRUPT)
           EXIT DISPLAY


      END DISPLAY


      CLEAR SCREEN
      CLEAR WINDOW ventana_2
      CLOSE WINDOW ventana_2    --ojo
      CLOSE WINDOW ventana_21   --ojo
    ELSE
      ERROR "      ARCHIVO ... VACIO."
      CLOSE WINDOW ventana_21   --ojo2
      SLEEP 2
    END IF


END FUNCTION

FUNCTION imprime()

 DEFINE G_IMPRE   CHAR(300)
 DEFINE i         integer 
 DEFINE nuevo  RECORD
        nss               CHAR(11),
        fecha_corte       DATE,
        fecha_proceso     DATE,
        tipo_nss          CHAR(2),
        desc              CHAR(40)
 END RECORD

   LET G_IMPRE = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                                                     ".IMP_CERO_",
                               HOY USING "dd-mm-yyyy","_",hora CLIPPED

   -- PARA IMPRESION
    START REPORT rpt_cuenta_imp TO G_IMPRE

   FOR i=1 TO pos

       LET nuevo.nss            = l_record_per[i].nss
       LET nuevo.fecha_corte    = l_record_per[i].fecha_corte
       LET nuevo.fecha_proceso  = l_record_per[i].fecha_proceso
       LET nuevo.tipo_nss       = l_record_per[i].tipo_nss
       LET nuevo.desc           = l_record_per[i].desc
 
       IF nuevo.nss IS NULL THEN
           EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_cuenta_imp(nuevo.*)

   END FOR 
   

 FINISH REPORT rpt_cuenta_imp
 ERROR "IMPRESION GENERADA...."
 SLEEP 2

 LET impresion = "lp ",G_IMPRE
-- RUN impresion

END FUNCTION

FUNCTION archivo()

 DEFINE comando   CHAR(300)
 DEFINE columnas  CHAR(200)
 DEFINE G_LISTA   CHAR(300)

   LET G_LISTA = g_param_dis.ruta_spool CLIPPED,"/",g_usuario CLIPPED,
                                                     ".ARC_CERO_",
                               HOY USING "dd-mm-yyyy","_",hora CLIPPED

   UNLOAD TO G_LISTA
   SELECT * FROM tt_arch_excel
   ORDER BY 1

   LET columnas = "NUM SS'|'FECHA CORTE'|'FECHA PROCESO'|'TIPO NSS'|'DESC'|'"

   LET comando ="cd ",g_param_dis.ruta_spool CLIPPED,"; echo ",columnas CLIPPED,"> alguno2" CLIPPED
   RUN comando

   LET comando ="cd ",g_param_dis.ruta_spool CLIPPED,"; cat ",G_LISTA CLIPPED,">> alguno2" CLIPPED,";mv alguno2 ",G_LISTA CLIPPED
   RUN comando

END FUNCTION


REPORT rpt_cuenta_imp(ynuevo)

   DEFINE ynuevo  RECORD
        nss               CHAR(11),
        fecha_corte       DATE,
        fecha_proceso     DATE,
        tipo_nss          CHAR(2),
        desc              CHAR(40)
   END RECORD

  OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

  FORMAT
      PAGE HEADER
         PRINT '\033e\033(s1p11v0s0b4168T\033(s8H\033(s7B'
         PRINT COLUMN 01,  "DISB070C",
               COLUMN 35,  "Reporte de Saldo Cero",
               COLUMN 100, TODAY USING "mm-dd-yyyy"
         SKIP 2 LINE

         PRINT '\033e\033(s1p8v0s0b4168T\033(s8H\033(s7B'
         PRINT COLUMN 20,  "NSS.Traba.",
               COLUMN 40,  "Fec corte",
               COLUMN 60,  "Fec proceso",
               COLUMN 80,  "Tipo nss",
               COLUMN 100, "Descrip."
               
         SKIP 1 LINE

         PRINT COLUMN 01,'______________________________________________________________________________________________________________________________'
         SKIP 1 LINE

        ON EVERY ROW

        PRINT COLUMN 19,ynuevo.nss CLIPPED,
              COLUMN 38,ynuevo.fecha_corte CLIPPED,
              COLUMN 63,ynuevo.fecha_proceso CLIPPED,
              COLUMN 83,ynuevo.tipo_nss CLIPPED,
              COLUMN 100,ynuevo.desc CLIPPED

        LET contador = contador + 1

    ON LAST ROW

        PRINT COLUMN 20,'_______________'
        SKIP 1 LINE
        PRINT COLUMN 01,"TOTAL DE REG.:",contador


END REPORT


FUNCTION Dia_ultimo(mes_in,anio_in)

  DEFINE
     mes_in   CHAR(02),
     anio_in  INTEGER,
     w_dias   CHAR(02)

 CASE mes_in

     WHEN '01'
       LET w_dias  = '31'

     WHEN '02'
       IF anio_in MOD 4 = 0 THEN
         LET w_dias  = '29'
       ELSE
         LET w_dias  = '28'
       END IF

     WHEN '03'
       LET w_dias  = '31'
 WHEN '04'
       LET w_dias  = '30'

     WHEN '05'
       LET w_dias  = '31'

     WHEN '06'
      LET w_dias  = '30'

     WHEN '07'
       LET w_dias  = '31'

     WHEN '08'
       LET w_dias  = '31'

     WHEN '09'
       LET w_dias  = '30'

     WHEN '10'
       LET w_dias  = '31'

     WHEN '11'
       LET w_dias  = '30'

     WHEN '12'
       LET w_dias  = '31'

    END CASE

    RETURN w_dias

END FUNCTION

################################################################################
#Proyecto          => SISTEMA SAFRE ( MEXICO )                                 #
#Sistema           => PRO                                                      #
#Programa PROC003  => GENERACION DE ARCHIVO PARA LA CONSAR PROMOTORES DADOS DE #
#                     BAJA                                                     #
#Fecha             => 24 DE ENERO DEL 2001                                     #
#ELABORADO POR     => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Actualizada => 29 DE MARZO DEL 2004                                     #
#ELABORADO POR     => LAURA EUGENIA CORTES GUZMAN                              #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 17-09-2009                                               #
#Observacion       => Se actualiza de acuerdo a layout del 27/07/09            #
#                  => (v10)                                                    #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo #paramgrales
        parametro      RECORD LIKE seg_modulo.*,

        HOY                DATE,
        borra_lineas       CHAR(200) ,
        enter              CHAR(01) ,
        G_LISTA_1          CHAR(300) ,
        G_LISTA_2          CHAR(300) ,
        tot_regis_env      ,
        tot_registros      ,
        s_codigo_afore     SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

   CALL STARTLOG("PROC003.log")
   CALL init() #i
   OPEN WINDOW proc0031 AT 4,4 WITH FORM "PROC0031" ATTRIBUTE(BORDER)
   DISPLAY "                           < Ctrl-C > Sa",
           "lir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " PROC003     GENERACION DE ARCHIVO PROMOTORES DADOS ",
           "DE BAJA                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   WHILE TRUE
       PROMPT " ESTA SEGURO  S/N " FOR CHAR enter
       IF enter MATCHES "[SsnN]" THEN
           IF enter MATCHES "[Ss]" THEN
               SELECT "OK"
               FROM   pro_mae_promotor A
               WHERE  A.motivo_suspende MATCHES "2*"
               AND    A.motivo_suspende      <> "2F"
--               AND    A.status                =  2    --(v10)
               AND    A.status                in (2,3)    --(v10)
               AND    A.status_interno        = 0

               OR     A.motivo_suspende MATCHES "3*"
               AND    A.status                =  3
               AND    A.status_interno        = 0
               GROUP BY 1

               IF STATUS = NOTFOUND THEN
                   PROMPT " NO EXISTEN REGISTROS ...<ENTER> PARA SALIR "
                   FOR CHAR enter
                   EXIT PROGRAM
               END IF
               EXIT WHILE
           ELSE
               PROMPT " PROCESO CANCELADO... <ENTER> PARA SALIR "
               FOR CHAR enter
               EXIT PROGRAM
           END IF
       END IF
   END WHILE

   DISPLAY " PROCESANDO INFORMACION... " AT 19,2 ATTRIBUTE(REVERSE)

   CALL primer_paso() #pp

   CLOSE WINDOW proc0031

   OPEN WINDOW proc0031 AT 4,4 WITH FORM "PROC0031" ATTRIBUTE(BORDER)
   DISPLAY "                           < Ctrl-C > Sa",
           "lir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " PROC003     GENERACION DE ARCHIVO PROMOTORES DADOS ",
           "DE BAJA                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   DISPLAY " TOTAL DE REGISTROS PROCESADOS : ",tot_registros AT 09,18
   DISPLAY " TOTAL DE REGISTROS ENVIADOS   : ",tot_regis_env AT 11,18
   PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter

   CLOSE WINDOW proc0031
END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY

    SELECT codigo_afore INTO s_codigo_afore FROM tab_afore_local

    SELECT *
    INTO   parametro.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pro"
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE reg_303 RECORD #loc #reg_303
        cod_promotor           CHAR(10) ,
        motivo_suspende       CHAR(2)  ,
        fecha_baja             DATE
    END RECORD

    DEFINE vfecha_baja         DATE,
           vfecha_baja2        DATE

    LET tot_registros = 0

    DECLARE cursor_2 CURSOR FOR
       SELECT A.cod_promotor    ,
              A.motivo_suspende ,
              A.fecha_baja
       FROM   pro_mae_promotor A
       WHERE  A.motivo_suspende MATCHES "2*"
       AND    A.motivo_suspende      <> "2F"
       AND    A.motivo_suspende      <> "2C"
--       AND    A.status                = 2         --(v10)
       AND    A.status                in (2,3)      --(v10)
       AND    A.status_interno        = 0
       OR     A.motivo_suspende MATCHES "3*"
       AND    A.status                = 3
       AND    A.status_interno        = 0

       LET G_LISTA_1 = parametro.ruta_envio CLIPPED,"/","baj"
       LET G_LISTA_2 = parametro.ruta_envio CLIPPED,"/","BAJ"

       START REPORT listado_1 TO G_LISTA_1
       LET tot_regis_env = 0

    FOREACH cursor_2 INTO reg_303.*

      LET tot_registros = tot_registros + 1

      LET vfecha_baja = reg_303.fecha_baja + 29 UNITS DAY

      LET tot_regis_env = tot_regis_env + 1

      DISPLAY " TOTAL DE REGISTROS PROCESADOS : ",tot_registros AT 09,18
      DISPLAY " TOTAL DE REGISTROS ENVIADOS   : ",tot_regis_env AT 11,18

      OUTPUT TO REPORT listado_1(reg_303.*) #l1

      INSERT INTO pro_envio_scb VALUES (HOY,reg_303.*,1,"")

      UPDATE pro_mae_promotor
      SET    pro_mae_promotor.status_interno = 1
      WHERE  pro_mae_promotor.status                = 2
      AND    pro_mae_promotor.status_interno        = 0
      AND    pro_mae_promotor.cod_promotor          = reg_303.cod_promotor
      OR     pro_mae_promotor.motivo_suspende MATCHES "3*"
      AND    pro_mae_promotor.status                = 3
      AND    pro_mae_promotor.status_interno        = 0
      AND    pro_mae_promotor.cod_promotor          = reg_303.cod_promotor

    END FOREACH
    FINISH REPORT listado_1

    LET borra_lineas = "sed -e '/^$/d' ",
                       G_LISTA_1 CLIPPED ,
                       " > ",
                       G_LISTA_2 CLIPPED
    RUN borra_lineas

    LET G_LISTA_2 = "chmod 777 ",
                    parametro.ruta_envio CLIPPED,"/",
                    "BAJ"
    RUN G_LISTA_2

    IF tot_regis_env <> 0 THEN
       INSERT INTO pro_ctr_envio VALUES (HOY,"BAJ",2,"",tot_registros,"")
    END IF
END FUNCTION

REPORT listado_1(reg_303)
#l1----------------------
    DEFINE reg_303 RECORD #loc #reg_303
               cod_promotor       CHAR(10) ,
               motivo_suspende    CHAR(2)  ,
               fecha_baja         DATE
           END RECORD,

           c8_fecha_proceso       CHAR(08),
           nro_lote               DECIMAL(9,0)

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 001,"303"                              ,#tipo_registro
            COLUMN 004,reg_303.cod_promotor               ,
            COLUMN 014,reg_303.motivo_suspende            ,
            COLUMN 016,reg_303.fecha_baja USING"YYYYMMDD" ,
            COLUMN 024,427 SPACES
END REPORT

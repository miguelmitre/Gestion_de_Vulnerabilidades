#***************************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa          => AFIB041                                               #
#Descripcion       => REVERSO DEL ARCHIVO OP 19 (BAJA CURP)                 #
#Sistema           => AFI.                                                  #
#Autor             => Eduardo Joaquin Resendiz Medina                       #
#Fecha             => 30 DE ABRIL DE 2010                                   #
#***************************************************************************#
DATABASE safre_af

GLOBALS
    DEFINE borrado         INTEGER,
           cuantos         INTEGER,
           cuanto_cza      INTEGER,
           tot_arch        INTEGER,
           usuario         CHAR(8),
           HOY             DATE,
           total           INTEGER,

           reg        RECORD
           fecha_proceso      DATE
           END RECORD,

           f_fecha_proceso    CHAR(10),

           estan      RECORD
           n_seguro        CHAR(11),
           n_folio         INTEGER,
           tipo_solicitud  SMALLINT,
           fecha_solicitud DATE
           END RECORD,


           fecha_l         DATE,
           ban             INTEGER,
           enter           CHAR(1)
END GLOBALS

MAIN
   DEFINE valor     SMALLINT

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

   CALL STARTLOG("AFIB041.log")

   LET borrado  = 0
   LET tot_arch = 0
   LET cuantos  = 0
   LET cuanto_cza  = 0

   LET HOY     = TODAY
   LET total   = 0
   INITIALIZE reg.*, estan.*, fecha_l TO NULL

   OPEN WINDOW AFIB041 AT 2,2 WITH FORM "AFIB0411" ATTRIBUTE(BORDER)
   DISPLAY " <Esc> Ejecutar                                              <",
           "Ctrl-C> Salir    " AT 1,1  ATTRIBUTE(REVERSE)
   DISPLAY " AFIB041          REVERSO  SOLICITUD  BAJA  CURP  OP  19 ",
           "                       " AT 3,1  ATTRIBUTE(REVERSE)
   DISPLAY HOY  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

   INPUT reg.fecha_proceso
      FROM FORMONLY.fecha_proceso

      AFTER FIELD FORMONLY.fecha_proceso
         IF reg.fecha_proceso IS NULL OR
            reg.fecha_proceso  = " "  THEN
            ERROR "DIGITE CORRECTAMENTE LA FECHA DEL PROCESO "
            NEXT FIELD FORMONLY.fecha_proceso
         ELSE
            LET f_fecha_proceso = reg.fecha_proceso USING "mm/dd/yyyy"
         END IF

      ON KEY(ESC)

         IF reg.fecha_proceso IS NULL OR
            reg.fecha_proceso  = " "  THEN
            ERROR "DIGITE CORRECTAMENTE LA FECHA DEL LOTE "
            NEXT FIELD FORMONLY.fecha_proceso
         END IF


         CALL borra()

         LET ban = 0
         EXIT INPUT

      ON KEY(CONTROL-C,INTERRUPT)
         LET ban = 1
         EXIT INPUT
   END INPUT

   IF ban = 1 THEN
      PROMPT "PROCESO CANCELADO... < ENTER > PARA CONTINUAR..." FOR enter
      EXIT PROGRAM
   END IF
   CLOSE WINDOW AFIB041
END MAIN


FUNCTION borra()
   DISPLAY "PROCESANDO INFORMACION..... ESPERE UNOS MOMENTOS..." AT 18,1

    SELECT COUNT(*)
    INTO   cuantos
    FROM   afi_mae_afiliado a, afi_mae_modifica b,
           afi_ctr_curp c
    WHERE  a.status_interno in(250)
    AND    a.n_seguro       = b.n_seguro
    AND    a.n_folio        = b.n_folio
    AND    a.tipo_solicitud = b.tipo_solicitud
    AND    b.cod_operacion  = 0
    AND    b.status_interno = 250
    AND    a.n_seguro       = c.nss
    AND    c.cve_operacion  = "19"
    AND    c.fecha_envio    = reg.fecha_proceso
    AND    c.fecha_respuesta IS NULL

   WHENEVER ERROR CONTINUE

   DECLARE app CURSOR FOR
        SELECT a.n_seguro,
               a.n_folio,
               a.tipo_solicitud,
               c.fecha_envio
        FROM   afi_mae_afiliado a, afi_mae_modifica b,
               afi_ctr_curp c
        WHERE  a.status_interno IN(250) # STATUS MODIFICADOS
        AND    a.n_seguro       = b.n_seguro
        AND    a.n_folio        = b.n_folio
        AND    a.tipo_solicitud = b.tipo_solicitud
        AND    b.cod_operacion  = 0
        AND    b.status_interno = 250
        AND    a.n_seguro       = c.nss
        AND    c.cve_operacion  = "19"
        AND    c.fecha_envio    = reg.fecha_proceso
        AND    c.fecha_respuesta IS NULL 

      FOREACH app INTO estan.*
            UPDATE afi_mae_afiliado
            SET status_interno   = 240
            WHERE n_seguro       = estan.n_seguro
            AND   n_folio        = estan.n_folio
            AND   tipo_solicitud = estan.tipo_solicitud
            AND   status_interno = 250

            UPDATE afi_mae_modifica
            SET status_interno   = 240
            WHERE n_seguro       = estan.n_seguro
            AND   n_folio        = estan.n_folio
            AND   tipo_solicitud = estan.tipo_solicitud
            AND   status_interno = 250

            UPDATE afi_ctr_curp
            SET   fecha_envio    = NULL
            WHERE nss            = estan.n_seguro
            AND   n_folio        = estan.n_folio
            AND   tipo_solicitud = estan.tipo_solicitud
            AND   fecha_envio    = reg.fecha_proceso
            AND   cve_operacion  = '19'

            LET borrado = borrado + 1

      END FOREACH
      PROMPT "Se reversaron ",borrado USING "####&"," DE ",cuantos USING "####&",
             " Enter para Continuar" FOR enter

      WHENEVER ERROR STOP
      DISPLAY "" AT 18,1
      DISPLAY " REGISTROS BORRADOS  : ",borrado USING "#######" AT 18,1
      PROMPT "PROCESO FINALIZADO... < ENTER > PARA CONTINUAR..." FOR enter 
END FUNCTION


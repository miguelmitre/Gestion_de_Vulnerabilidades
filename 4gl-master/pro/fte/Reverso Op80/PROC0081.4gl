################################################################################
#Proyecto          => SISTEMA DE safre_af.( MEXICO )                           #
#Sistema           => PRO.                                                     #
#Programa PROC0081 => REVERSO DE ACTUALIZACION DEL DATOS DEL PROMOTOR          #
#Fecha             => 07 DE MAYO DEL 2002                                      #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Modificacion=> 29 DE MARZO DEL 2004                                     #
#Modificado Por    => LAURA EUGENIA CORTES GUZMAN                              #
################################################################################
DATABASE safre_af
GLOBALS

   DEFINE reg_1 RECORD
      fecha_genera   LIKE pro_capacitacion.fecha_genera
   END RECORD

   DEFINE
      HOY            DATE,
      enter          CHAR(001)

END GLOBALS


MAIN
   DEFER INTERRUPT
   OPTIONS
      INPUT WRAP          ,
      PROMPT LINE LAST    ,
      ACCEPT KEY CONTROL-I

   CALL STARTLOG("PROC0081.log")    

   CALL init() #i

   OPEN WINDOW proc00811 AT 4,4 WITH FORM "PROC00811" ATTRIBUTE(BORDER)
   DISPLAY "                             <Ctrl-C> Salir                                      " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " PROC0081     REVERSO ENVIO ACTUALIZACION DATOS DEL PROMOTOR                   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg_1.fecha_genera WITHOUT DEFAULTS
      AFTER FIELD fecha_genera
         IF reg_1.fecha_genera IS NULL THEN
            ERROR " CAMPO NO PUEDE SER NULO "
            NEXT FIELD fecha_genera
         ELSE
            SELECT UNIQUE "OK"
            FROM   pro_ctr_envio A
            WHERE  A.fecha_genera = reg_1.fecha_genera
            AND    A.estado = 4
            AND    A.tipo_operacion = "MOD"

            IF STATUS <> NOTFOUND THEN
               PROMPT "NO SE PUEDE REALIZAR LA  OPERACION,",
                      "LOTE YA GENERADO...<ENTER> PARA SALIR " 
               FOR CHAR enter

               EXIT PROGRAM
            END IF
         END IF

      ON KEY (ESC)
         IF reg_1.fecha_genera IS NULL THEN
            ERROR " CAMPO NO PUEDE SER NULO "
            NEXT FIELD fecha_genera
         ELSE
            SELECT UNIQUE "OK"
            FROM   pro_ctr_envio A
            WHERE  A.fecha_genera = reg_1.fecha_genera
            AND    A.estado = 4
            AND    A.tipo_operacion = "MOD"

            IF STATUS <> NOTFOUND THEN
               PROMPT "NO SE PUEDE REALIZAR LA  OPERACION,",
                      "LOTE YA GENERADO <ENTER> PARA SALIR "
               FOR CHAR enter

               EXIT PROGRAM
            END IF
         END IF

         WHILE TRUE
            PROMPT " ESTA SEGURO S/N " FOR CHAR enter
            IF enter MATCHES "[SsNn]" THEN
               IF enter MATCHES "[Ss]" THEN
                  EXIT INPUT
               ELSE
                  PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR "
                  FOR CHAR enter 
                  EXIT PROGRAM
               END IF
            END IF
         END WHILE

      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
         EXIT PROGRAM
   END INPUT

   DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

   CALL primer_paso() #pp

   PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
   EXIT PROGRAM

   CLOSE WINDOW proc00811

END MAIN


FUNCTION init()
#i-------------
   LET HOY                = TODAY
   LET reg_1.fecha_genera = HOY
END FUNCTION


FUNCTION primer_paso()
#pp-------------------
   DEFINE #loc #char
      c10_cod_promotor      CHAR(10)

   DECLARE cur_1 CURSOR FOR
   SELECT A.cod_promotor
   FROM   pro_envio_mod A
   WHERE  A.fenvio = reg_1.fecha_genera
   AND    A.status_interno = 1

   FOREACH cur_1 INTO c10_cod_promotor
      UPDATE pro_mae_promotor
      SET    pro_mae_promotor.status_interno = 6 ,
             pro_mae_promotor.fenvio         = ""
      WHERE  pro_mae_promotor.cod_promotor = c10_cod_promotor
   END FOREACH

   DELETE
   FROM  pro_envio_mod
   WHERE fenvio = reg_1.fecha_genera
   AND   @status_interno = 1

   DELETE
   FROM   pro_ctr_envio
   WHERE  fecha_genera   = reg_1.fecha_genera
   AND    tipo_operacion = "MOD"
   AND    estado         = 2

END FUNCTION

############################################################################
#Proyecto               :       AFORE ( MEXICO )                           #
#Propietario            :       E.F.P.                                     #
#Programa TAAC011       :       Recibe archivo del valor de accion para la #
#                               siefore.                                   #
#Fecha creacion         :       06 de Junio de 2003                        #
#Autor                  :       Fernando Herrera Hernandez                 #
#Modifica               :       Josue Lisandro Huerta Sierra               #
#Fecha                  :       5 de Marzo de 2008                         #
#               Adecuacion para multisiefores circular 69-2                #
#Sistema                :                                                  #
############################################################################
DATABASE safre_af
   GLOBALS
      DEFINE g_param_taa                        RECORD LIKE seg_modulo.*
      DEFINE reg_acc_sie                        RECORD  
             afore                              SMALLINT,
             siefore                            CHAR(08),
             titulos                            DECIMAL(18,6),
             valor                              DECIMAL(18,6),
             importe                            DECIMAL(18,6),
             fecha                              DATE
                                                END RECORD

      DEFINE hoy                                DATE,
             cuantos                            INTEGER,
             cont_acept                         INTEGER,
             cont_rech                          INTEGER,
             contador                           INTEGER,
             g_usuario                          CHAR(8),
             archivo_traspaso                   CHAR(200)

      DEFINE g_afore                            RECORD LIKE tab_afore_local.*

      DEFINE generar                            CHAR(020),
             enter                              CHAR(001)

   END GLOBALS
############################################################################
MAIN

   DISPLAY " "
   DISPLAY ".1"

   CALL STARTLOG('TAAC011.log')
   CALL inicio() #i

   DEFER INTERRUPT
   OPTIONS
      INPUT   WRAP,
      MESSAGE LINE LAST,
      PROMPT  LINE LAST,
      ERROR   LINE LAST

      CALL proceso_principal() #pp

END MAIN
############################################################################
FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "TAAC011" 
   ATTRIBUTE(BORDER)  
   DISPLAY "                            < CTRL - C > Salir                                 " AT 1,1 
   ATTRIBUTE(REVERSE)
   DISPLAY " TAAC011              ARCHIVO DE VALOR DE ACCION (SIEFORE)                     " AT 3,1
   ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,65
   ATTRIBUTE(REVERSE)
   DISPLAY "Total de registros en el archivo: ", contador   AT 13,05
   DISPLAY "Total de registros aceptados    : ", cont_acept AT 14,05
   DISPLAY "Total de registros rechazados   : ", cont_rech  AT 15,05     

   INPUT BY NAME generar

      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
         END IF

         WHENEVER ERROR CONTINUE
            LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED, "/",
                                   generar CLIPPED

            LOAD FROM archivo_traspaso DELIMITER "," 
            INSERT INTO safre_tmp:tmp_acc_siefore

            SELECT COUNT(*)
              INTO cuantos
              FROM safre_tmp:tmp_acc_siefore 

            IF cuantos = 0 THEN
               DISPLAY " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
               AT 19,2
               ATTRIBUTE(REVERSE)
               SLEEP 3
               NEXT FIELD generar
            ELSE
               EXIT INPUT
            END IF
         WHENEVER ERROR STOP

         ON KEY (INTERRUPT)
            ERROR "PROCESO CANCELADO" 
            ATTRIBUTE(REVERSE)
            SLEEP 2
            EXIT PROGRAM 

   END INPUT

   ERROR "PROCESANDO INFORMACION "

   CALL validacion_previa() #vp

   ERROR ""

   PROMPT "Presione <enter> para finalizar " FOR enter

END FUNCTION
############################################################################
FUNCTION inicio()
#i---------------

   LET contador    = 0
   LET cuantos     = 0
   LET cont_rech   = 0
   LET cont_acept  = 0


   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE tmp_acc_siefore
   WHENEVER ERROR STOP

   CREATE TABLE tmp_acc_siefore
      (afore    SMALLINT,
       siefore  CHAR(08),
       titulos  DECIMAL(18,6),
       valor    DECIMAL(18,6),
       importe  DECIMAL(18,6),
       fecha    DATE)   

   DATABASE safre_af

   LET hoy = TODAY

   SELECT *, USER
     INTO g_param_taa.*, g_usuario
     FROM seg_modulo 
    WHERE modulo_cod = 'taa'

END FUNCTION
############################################################################
FUNCTION validacion_previa()
#vp-------------------------

   DEFINE v_ins         SMALLINT
   DEFINE desc_sie      CHAR(8)
   DEFINE cod_sie       SMALLINT

   LET v_ins = 0

   DECLARE c_1 CURSOR FOR
   SELECT  * 
   FROM    safre_tmp:tmp_acc_siefore
   FOREACH c_1 INTO reg_acc_sie.*

      LET contador = contador + 1

      ---- Valida clave afore ----
      SELECT 'X'
        FROM tab_afore
       WHERE @afore_cod = reg_acc_sie.afore

      IF STATUS =  NOTFOUND THEN
         LET cont_rech = cont_rech + 1
         LET v_ins     = 0
         CONTINUE FOREACH
      ELSE
         ---- Valida descripcion de la siefore ----
         SELECT @siefore_desc, @siefore_cod
           INTO desc_sie, cod_sie
           FROM tab_siefore
          WHERE @afore_cod    = reg_acc_sie.afore
            AND @siefore_desc = reg_acc_sie.siefore

         IF STATUS <> NOTFOUND THEN
            LET v_ins = 1
         ELSE
            LET cont_rech = cont_rech + 1
            LET v_ins     = 0
            CONTINUE FOREACH
         END IF
      END IF 

       ---- Valida duplicidad ----
       SELECT 'X'
         FROM taa_accion_siefore
        WHERE @codigo_afore    = reg_acc_sie.afore
          AND @codigo_siefore  = cod_sie
          AND @desc_siefore    = reg_acc_sie.siefore
          AND @precio_del_dia  = reg_acc_sie.valor
          AND @fecha_valuacion = reg_acc_sie.fecha
       GROUP BY 1

       IF STATUS <> NOTFOUND THEN
          LET cont_rech = cont_rech + 1
          LET v_ins     = 0
          CONTINUE FOREACH
       END IF 

      IF v_ins = 1 THEN
         INSERT INTO taa_accion_siefore VALUES (reg_acc_sie.afore,
                                                cod_sie,
                                                reg_acc_sie.siefore,
                                                reg_acc_sie.valor,
                                                reg_acc_sie.fecha,
                                                g_usuario)
         LET cont_acept = cont_acept + 1
         LET v_ins      = 0
      END IF

   END FOREACH

   DISPLAY "Total de registros en el archivo: ", contador   AT 13,05
   DISPLAY "Total de registros aceptados    : ", cont_acept AT 14,05
   DISPLAY "Total de registros rechazados   : ", cont_rech  AT 15,05     

END FUNCTION
############################################################################

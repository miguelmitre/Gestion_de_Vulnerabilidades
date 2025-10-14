################################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                             #
#Owner             => E.F.P.      . 					       #
#Programa COMB013  => ACTUALIZACION EAQUEMA COMISION EN afi_mae_afiliado               #
#Fecha             => 27 marzo 1999.      				       #
#By                => GERARDO ALFONSO VEGA PAREDES.      		       #
#Sistema           => COM. 					               #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_record RECORD
      n_folio           DECIMAL(8,0),
      codven            DECIMAL(10,0),
      nivel             SMALLINT,
      fentcons          DATE
   END RECORD
   DEFINE vn_folio DECIMAL(8,0)
   DEFINE 
      hoy		DATE,
      vcod_esq_viejo    SMALLINT,
      vcod_esq_comision SMALLINT,
      cla_where         CHAR(300),
      cla_where2        CHAR(300),
      opc		CHAR(01)
END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMB0131" ATTRIBUTE(BORDER)
   DISPLAY " COMB013    ACTUALIZACION ESQUEMA COMISION EN AFILIACIONES                  " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 2,65 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   INPUT BY NAME vn_folio,vcod_esq_viejo,vcod_esq_comision
      AFTER FIELD vn_folio
         IF vn_folio IS NULL THEN
            ERROR "Este campo no puede ser NULO"
            NEXT FIELD vn_folio
         END IF
         SELECT cod_esq_comision
         INTO   vcod_esq_viejo
         FROM   afi_mae_afiliado
         WHERE  n_folio = vn_folio

         DISPLAY BY NAME vcod_esq_viejo

      AFTER FIELD vcod_esq_comision
         IF vcod_esq_comision IS NULL THEN
            ERROR "Este campo no puede ser NULO"
            NEXT FIELD vcod_esq_comision
         END IF

         EXIT INPUT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT
   IF INT_FLAG = TRUE THEN
      ERROR "Operacion abortada"
      SLEEP 2
      LET INT_FLAG = FALSE
      EXIT PROGRAM 
   END IF
   PROMPT "Esta suguro de cambiar las afiliaciones con el nuevo esquema [S/N]..." FOR opc
   IF opc MATCHES '[Ss]' THEN
      CALL genera_proceso()
   ELSE
      ERROR "Operacion abortada"
      SLEEP 2
   END IF
END MAIN

FUNCTION genera_proceso()
   DEFINE cla_upd CHAR(200),
          cla_sel CHAR(200),
          vcodven CHAR(10)

         LET cla_upd = "UPDATE afi_mae_afiliado ",
                          "SET cod_esq_comision = ",vcod_esq_comision,
                       " WHERE  n_folio = ",vn_folio CLIPPED

      PREPARE exe_up FROM cla_upd
      EXECUTE exe_up

   ERROR "Actualizacion completada"
   SLEEP 3   
END FUNCTION

################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Owner             => E.F.P.                                                   #
#Programa COMB006  => PROCESO BATCH REVERSA CALCULO COMISIONES RESUMEN Y DETALLE
#Sistema           => COM.                                                     #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha             => 23 agostos 1997.                                         #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha             => 17 enero 2001.                                           #
################################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_record RECORD
      fecha_corte DATE
   END RECORD

   DEFINE g_param_com RECORD LIKE com_parametro.*

   DEFINE opc      CHAR(1),
          hoy      DATE,
          vusuario CHAR(08),
          cla_del  CHAR(500),
          cla_upd  CHAR(500)

   DEFINE vn_folio        DECIMAL(8,0),
          vtipo_solicitud SMALLINT,
          vfecha_proceso  CHAR(10)

END GLOBALS

MAIN
   OPTIONS 
   PROMPT LINE LAST,
   INPUT WRAP,
   ACCEPT KEY control-o

   DEFER INTERRUPT

   LET hoy = DATE

   SELECT *,
          USER 
   INTO   g_param_com.*,
          vusuario 
   FROM com_parametro

   OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMB0061" ATTRIBUTE( BORDER)

   DISPLAY " COMB006                REVERSEO DE COMISIONES                             " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-c] P/Salir " AT 1,1 

   LET INT_FLAG = FALSE
  
   INPUT BY NAME g_record.*
      AFTER FIELD fecha_corte
         IF g_record.fecha_corte IS NULL THEN
            ERROR "Fecha NO puede ser NULA"
            NEXT FIELD fecha_corte
         END IF

         DECLARE cur_fecha CURSOR FOR
         SELECT "G"
         FROM   com_reverso
         WHERE  fecha_corte = g_record.fecha_corte

         OPEN cur_fecha
         FETCH cur_fecha
         IF STATUS = NOTFOUND THEN
            ERROR "No existe esta fecha para Reversar"
            NEXT FIELD fecha_corte
         END IF

         EXIT INPUT

      ON KEY (ESC)
         LET INT_FLAG = TRUE
         EXIT INPUT
    
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      ERROR "Proceso Cancelado..."
      SLEEP 2
      EXIT PROGRAM
   END IF
      
   PROMPT "Deseas emitir reverseo [S/N]...." for opc

   IF opc MATCHES '[Ss]' THEN
      ERROR "REVERSANDO..."
      CALL Proceso_principal()
      ERROR ""
      PROMPT "Proceso Finalizo Normalmente..Presione < ENTER > para Salir"
      FOR CHAR opc
   ELSE
      ERROR "Proceso Cancelado..." SLEEP 2
   END IF

END MAIN

FUNCTION Proceso_principal()

   DECLARE cur_detalle CURSOR FOR
   SELECT n_folio,
          tipo_solicitud
   FROM   com_reverso
   WHERE  fecha_corte = g_record.fecha_corte
   GROUP  BY 1,2

   FOREACH cur_detalle INTO vn_folio,vtipo_solicitud

      LET cla_upd = "UPDATE afi_mae_afiliado ",
                    "SET    indicador_comision = 0 ",
                    "WHERE  n_folio        = ",vn_folio,
                    "AND    tipo_solicitud = ",vtipo_solicitud CLIPPED
      PREPARE claupd1 FROM cla_upd
      EXECUTE claupd1

      LET cla_upd = "UPDATE afi_solicitud ",
                    "SET    indicador_comision = 0 ",
                    "WHERE  n_folio        = ",vn_folio,
                    "AND    tipo_solicitud = ",vtipo_solicitud CLIPPED
      PREPARE claupd2 FROM cla_upd
      EXECUTE claupd2

   END FOREACH

   LET cla_del = "DELETE ",
                 "FROM   com_comis_resumen ",
                 "WHERE  fecha_corte = ","'",g_record.fecha_corte,"'" CLIPPED

   PREPARE cladel1 FROM cla_del
   EXECUTE cladel1
  
   LET cla_del = "DELETE ",
                 "FROM   com_comis_detalle ",
                 "WHERE  fecha_corte = ","'",g_record.fecha_corte,"'" CLIPPED
  
   PREPARE cladel2 FROM cla_del
   EXECUTE cladel2

   LET vfecha_proceso = g_record.fecha_corte

   LET cla_del = "DELETE ",
                 "FROM   com_ctrl_proceso ",
                 "WHERE  parametro1 = ","'",vfecha_proceso,"'"  CLIPPED
  
   PREPARE cladel3 FROM cla_del
   EXECUTE cladel3

   LET cla_del = "DELETE ",
                 "FROM   com_reverso ",
                 "WHERE  fecha_corte = ","'",g_record.fecha_corte,"'" CLIPPED
  
   PREPARE cladel4 FROM cla_del
   EXECUTE cladel4
END FUNCTION

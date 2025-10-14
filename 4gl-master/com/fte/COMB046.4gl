#############################################################################
#Proyecto          => Sistema de AFORE.( MEXICO )                           #
#Propietario       => E.F.P.                                                #
#Programa          => COMB046                                               #
#Descripcion       => Sustituye en numero interno por nomina                #
#Fecha             => 18 Abril 2005                                         #
#Por               => ISABEL FONSECA FRIAS                                  #
#Sistema           => COM                                                   #
#############################################################################
database safre_af

GLOBALS
   DEFINE g_param_com    RECORD LIKE seg_modulo.*

   DEFINE hoy            DATE


   DEFINE g_reg record
      folio    integer,
      nomina   char(10),
      tipo_sol smallint
   END RECORD

   DEFINE   ejecuta       CHAR(200)
   DEFINE aux_pausa       CHAR(1)
   DEFINE contador integer

END GLOBALS

MAIN

   SELECT ruta_rescate
   INTO   g_param_com.ruta_rescate
   FROM   seg_modulo
   where modulo_cod = "com"

   LET hoy = ARG_VAL(1)

#----------------
   CALL proceso_especial() 

   CALL principal()
END MAIN

FUNCTION proceso_especial() 
#----------------

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE tmp_nomina 
   WHENEVER ERROR STOP

   CREATE TABLE tmp_nomina 
      (folio INTEGER,
       nomina    CHAR(10),
       tipo_solicitud SMALLINT);

   CREATE INDEX tmp_nomina_1 on tmp_nomina (folio,nomina);

       LET ejecuta = "cd ",g_param_com.ruta_rescate CLIPPED,
                     "/;dbload -d safre_tmp -c sube_nomina -l dbload.log"

       RUN ejecuta

END FUNCTION

FUNCTION principal()
#----------------

   DECLARE cur1 cursor for
   SELECT folio,
          nomina,
          tipo_solicitud
   FROM   safre_tmp:tmp_nomina
   ORDER  by 1

   LET contador = 0
   FOREACH cur1 into g_reg.*

      LET contador = contador + 1

      UPDATE safre_af:com_comis_detalle
      SET    codven         = g_reg.nomina
      WHERE  n_folio        = g_reg.folio
      AND    tipo_solicitud = g_reg.tipo_sol
      AND    fecha_corte    = hoy 

   END FOREACH

END FUNCTION

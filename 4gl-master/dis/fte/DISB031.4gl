###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#                  => E.F.P.                                                  #
#Programa          => DISB031                                                 #
#Descripcion       => Genera fecha_pri_general en cta_ctr_cuenta              #
#Sistema           => DIS.                                                    #
#Fecha Inicio      => 30 mayo 2002.                                           #
#Fecha Termino     =>                                                         #
#By                => GERARDO ALFONSO VEGA PAREDES                            #
#Fecha ult. modif. =>                                                         #
#By                =>                                                         #
#                  =>                                                         #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE cla_sel  CHAR(800)

   DEFINE vnss       CHAR(11),
          vfecha_pri DATE

END GLOBALS

MAIN

   CALL define_cursores()

   CALL Genera_fecha()

END MAIN

FUNCTION define_cursores()

   LET cla_sel = " SELECT nss ",
                 " FROM   cta_ctr_cuenta ",
                 " WHERE  fecha_pri_general IS NULL ", 
                 " OR     fecha_pri_general = '01/01/0001' ",
                 " FOR UPDATE " CLIPPED

   PREPARE claexe1 FROM cla_sel
   DECLARE cur_fecha CURSOR FOR claexe1



   LET cla_sel = " SELECT MIN(fecha_conversion) ",
                 " FROM   dis_cuenta ",
                 " WHERE  nss = ? " CLIPPED

   PREPARE claexe2 FROM cla_sel
   DECLARE cur_cuenta CURSOR FOR claexe2

END FUNCTION

FUNCTION Genera_fecha()

   FOREACH cur_fecha INTO vnss

      OPEN cur_cuenta USING vnss

      FETCH cur_cuenta INTO vfecha_pri

      IF STATUS <> NOTFOUND THEN
         UPDATE cta_ctr_cuenta
         SET    fecha_pri_general = vfecha_pri
         WHERE  CURRENT OF cur_fecha         
      END IF

      CLOSE cur_cuenta

   END FOREACH

END FUNCTION

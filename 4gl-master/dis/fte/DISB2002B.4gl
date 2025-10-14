###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#                  => E.F.P.                                                  #
#Programa          => DISB2002B                                               #
#Descripcion       => INSERTA REGISTROS DE dis_tmp_aporte a dis_det_aporte y  #
#                  => dis_tmp_interes a dis_det_interes                       #
#Sistema           => DIS.                                                    #
#Fecha Inicio      => 25 octubre 2001.                                        #
#Fecha Termino     => 25 octubre 2001.                                        #
#By                => GERARDO ALFONSO VEGA PAREDES                            #
#Fecha ult. modif. =>                                                         #
#By                =>                                                         #
#                  =>                                                         #
###############################################################################

DATABASE safre_af

MAIN
   CALL Inserta_aporte()
END MAIN

FUNCTION Inserta_aporte()

   ERROR "Insertando registros en dis_det_aporte y dis_det_interes"

   INSERT INTO dis_det_aporte
   SELECT * 
   FROM   safre_tmp:dis_tmp_aporte
 
   INSERT INTO dis_det_interes
   SELECT *
   FROM   safre_tmp:dis_tmp_interes

   ERROR "Finalizo insert en dis_det_aporte y dis_det_interes"

END FUNCTION

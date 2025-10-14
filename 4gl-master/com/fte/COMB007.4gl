################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Owner             => E.F.P                                                    #
#Programa COMB007  => PROCESO BATCH PROVISIONA COMISIONES EJECUTIVOS(RESUMEN)  #
#Sistema           => COM.                                                     #
#Fecha             => 25 agosto 1997.                                          #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha Actualiza   => 25 Agosto 1997.                                          #
#By                => HECTOR FERNANDEZ ARCINIEGA.                              #
#Fecha             => 20 octubre 1997.                                         #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha             => 4 mayo 1998.                                             #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha             => 1 mayo 2002.                                             #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
################################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_reg RECORD LIKE com_comis_resumen.*

   DEFINE g_met RECORD
      meta_afi1        LIKE tab_puesto.meta_afi1,
      meta_afi2        LIKE tab_puesto.meta_afi2,
      meta_afi3        LIKE tab_puesto.meta_afi3,
      meta_tra1        LIKE tab_puesto.meta_tra1
   END RECORD

   DEFINE g_param_com RECORD LIKE com_parametro.*

   DEFINE vmeta_afilia DECIMAL(12,2),
          vmeta_calida DECIMAL(12,2),
          vmeta_sumasm DECIMAL(12,2),
          vcalidad_cta DECIMAL(12,2)

   DEFINE g_record RECORD
      fecha_desde DATE,
      fecha_hasta DATE
   END RECORD

   DEFINE aux_pausa,
          g_opcion  CHAR(1),
          g_usuario CHAR(8),
          seltxt    CHAR(400)

   DEFINE hoy         DATE,
          vcomando    SMALLINT,
          opc         CHAR(01),
          vencontrado SMALLINT

   DEFINE hora_inicial CHAR(8),  
          hora_final   CHAR(8),  
          vhora_max    CHAR(8),  
          vhora_final  CHAR(8),  
          cla_sel      CHAR(900),
          cla_upd      CHAR(450),
          vmensaje     CHAR(50)

   DEFINE g_bat RECORD LIKE com_ctrl_proceso.*

END GLOBALS

MAIN
   DISPLAY "INICIO CALCULO A RESPONSABLE"

   CALL STARTLOG("COMB007.log")

   LET hoy = ARG_VAL(1)

   SELECT *,
          USER 
   INTO   g_param_com.*,
          g_usuario 
   FROM   com_parametro

   LET cla_sel = "SELECT MAX(hora_inicial) ",       
                 "FROM   com_ctrl_proceso ",        
                 "WHERE  proceso_cod = 'COMB007' ", 
                " AND    etapa_cod   = 4" CLIPPED   
                                                    
   PREPARE claexe9 FROM cla_sel                     
   DECLARE cur_proceso9 CURSOR FOR claexe9          
   OPEN cur_proceso9                                
      FETCH cur_proceso9 INTO vhora_max             
   CLOSE cur_proceso9                               
                                                    
   SELECT *
   INTO   g_bat.*
   FROM   com_ctrl_proceso
   WHERE  com_ctrl_proceso.proceso_cod  = "COMB007"
   AND    com_ctrl_proceso.etapa_cod    = 4
   AND    com_ctrl_proceso.hora_inicial = vhora_max

   CALL proceso_principal()

   LET vhora_final = TIME

   LET vmensaje = "CALCULO A RESPONSABLES TERMINADO"
                                                                   
   LET cla_upd = "UPDATE com_ctrl_proceso ",
                 "SET    hora_final   = ","'",vhora_final,"'",",",
                 "       resultado    = ","'",vmensaje,"'",
                " WHERE  proceso_cod  = 'COMB007' ",
                " AND    etapa_cod    = 4 ",
                " AND    hora_inicial = ","'",vhora_max,"'" CLIPPED
                                                                   
   PREPARE claupdexe FROM cla_upd
   EXECUTE claupdexe
                                                                   
   DISPLAY "FIN CALCULO A RESPONSABLES"

END MAIN

FUNCTION Proceso_principal()

   LET cla_sel = "SELECT a.*,",
                        "b.meta_afi1,",
                        "b.meta_afi2,",
                        "b.meta_afi3,",
                        "b.meta_tra1 ",
                 "FROM   com_comis_resumen a,tab_puesto b ",
                 "WHERE  estado_comision = 0 ",
                " AND    nivel <> 1 ",        --significa pago responsables
                " AND    b.cod_puesto = a.cod_tipo_prom " CLIPPED
       
   PREPARE claexe1 FROM cla_sel
   DECLARE cursor_1 CURSOR FOR claexe1

   LET vencontrado = FALSE

   FOREACH cursor_1 INTO g_reg.*,g_met.*

      LET vmeta_afilia = g_reg.total_afiliados * 100 / g_met.meta_afi1
      LET vmeta_calida = g_reg.promedio_sm     * 100 / g_met.meta_afi2
      LET vmeta_sumasm = g_reg.total_sm        * 100 / g_met.meta_afi3
      LET vcalidad_cta = (vmeta_afilia + vmeta_calida + vmeta_sumasm) / 3

      IF vcalidad_cta >= g_met.meta_tra1 THEN

         LET g_reg.pago = 1                         -- Si se paga comision

         IF vcalidad_cta > 100 THEN
            LET vcalidad_cta = 100
         END IF

         LET g_reg.total_comision = vcalidad_cta * g_reg.comis_calculada / 100
      ELSE
         LET g_reg.pago = 0                         -- No se paga comsion
         LET g_reg.total_comision = 0
      END IF

      UPDATE com_comis_resumen 
      SET    estado_comision = 10,    --Calculo de Metas a Responsables
             pago            = g_reg.pago,
             total_comision  = g_reg.total_comision
      WHERE  estado_comision = 0
      AND    codven          = g_reg.codven  

   END FOREACH
   
END FUNCTION

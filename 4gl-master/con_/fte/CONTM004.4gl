############################################################################## #Proyecto          => Sistema de Afores.( MEXICO )                             # #Owner             => E.F.P. 					               #
#Programa CONT004  => GENERACION DE TRANSACCIONES CONTABLES (ICEFAS)
#Fecha             => 16 Diciembre 1997                                        #
#By                => JOSE MANUEL VIZCAINO                                     #
#Sistema           => CONTABILIDAD 		                               #
################################################################################
DATABASE safre_af
    GLOBALS
          DEFINE vcomida                   DECIMAL(16,6)
          DEFINE vnombre                   CHAR(11)
	  DEFINE G_LISTA                   CHAR(200)         
	  DEFINE vreporte                  INTEGER
	  DEFINE vreferencia               CHAR(10)  
	  DEFINE vfolio3                   CHAR(10)
	  DEFINE vpregunta                 CHAR(1)
	  DEFINE venvio                    CHAR(2)         
	  DEFINE vtran                     CHAR(2)
	  DEFINE vusuario                  CHAR(10)
	  DEFINE vfecha_proceso            DATE
	  DEFINE vvalor_accion             DECIMAL(16,6)
	  DEFINE vsuma_todo                DECIMAL(16,6)
	  DEFINE vtransaccion              CHAR(8)
          DEFINE vtransacc    CHAR(2)
	  DEFINE vprecio_accion            DECIMAL(16,6),
		 vmonto_en_pesos           DECIMAL(16,6)
	  DEFINE vtran1207                 CHAR(5) 

	  DEFINE gr_3101 RECORD 
                 transaccion               CHAR(4),
                 monto_en_pesos            DECIMAL(16,6)
          END RECORD
          
	  DEFINE gr_3102 RECORD 
                 transaccion               CHAR(4),
                 monto_en_pesos            DECIMAL(16,6)
          END RECORD

          DEFINE gr_3201 RECORD 
                 transaccion               CHAR(4),
                 monto_en_pesos            DECIMAL(16,6),
		 siefore                   CHAR(3),
		 subcuenta                 CHAR(1),
		 precio_accion             DECIMAL(16,6)
	  END RECORD

	  DEFINE gr_3202 RECORD 
                 transaccion               CHAR(4),
                 subcuenta                 CHAR(1),
                 monto_en_pesos            DECIMAL(16,6)
          END RECORD
          
          DEFINE gr_3204 RECORD
		 transaccion               DECIMAL(16,6),
                 fraccion                  DECIMAL(16,6),
                 siefore                   CHAR(1)
          END RECORD

          DEFINE gr_3203 RECORD 
                 transaccion               CHAR(4),
                 fraccion                  DECIMAL(16,6),
                 siefore                   CHAR(3)
          END RECORD
 
          DEFINE gr_1207 RECORD 
                 transaccion               CHAR(4),
                 monto_en_pesos            DECIMAL(16,6),
                 siefore                   CHAR(3),
                 subcuenta                 CHAR(1)
          END RECORD
           
          DEFINE gr_1208 RECORD 
                 transaccion               CHAR(4),
                 monto_en_pesos            DECIMAL(16,6),
                 siefore                   CHAR(3),
                 subcuenta                 CHAR(1)
          END RECORD
          
        
          DEFINE gr_1214 RECORD 
                 transaccion               CHAR(4),
                 monto_en_pesos            DECIMAL(16,6),
                 siefore                   CHAR(3),
                 subcuenta                 CHAR(1)
          END RECORD

          DEFINE gr_3205 RECORD
                 transaccion               CHAR(5),
                 precio_accion             DECIMAL(16,6),
	         monto_en_pesos            DECIMAL(16,6),
                 siefore                   CHAR(3),
		 subcuenta                 CHAR(1)
	  END RECORD

          DEFINE gr_3208 RECORD
                 transaccion               CHAR(4),
		 monto_en_acciones         DECIMAL(16,6),
                 siefore                   CHAR(3)                   
          END RECORD
 
          DEFINE gr_3206 RECORD
                 transaccion               CHAR(4),
                 monto_en_pesos            DECIMAL(16,6),
                 siefore                   CHAR(3)                   
          END RECORD
         
          DEFINE gr_3207 RECORD
                 transaccion               CHAR(4),
                 precio_accion             DECIMAL(16,6),
	         monto_en_pesos            DECIMAL(16,6),
                 siefore                   CHAR(3),
		 subcuenta                 CHAR(1)
	  END RECORD
 
          DEFINE gr_3209 RECORD
	         transaccion               CHAR(3),
                 precio_accion             DECIMAL(16,6),
                 monto_en_pesos            DECIMAL(16,6),
		 siefore                   CHAR(3)
	  END RECORD

          DEFINE vfolio1      INTEGER 
          DEFINE vfolio2      INTEGER 
          DEFINE vfecha                    CHAR(20)
          DEFINE vtransa_cod               CHAR(5) --jmv
          DEFINE vtransa_desc              CHAR(35)
          DEFINE vdebito_credito           CHAR(1)
          DEFINE vprovi_acred              CHAR(1)
          DEFINE hoy                       DATE
          DEFINE vperiodo                  CHAR(7)
          DEFINE vanio                     CHAR(4)
          DEFINE vfecha_calculo            DATE  
          DEFINE vfecha_pago               DATE
          DEFINE vhoy                      DATE
          DEFINE vtipo_registro            CHAR(1)
          DEFINE vconstante                CHAR(4)
          DEFINE vcodigo_analisis1         CHAR(15)
          DEFINE vcodigo_analisis2         CHAR(15)
          DEFINE vfolio_interno            CHAR(10)
          DEFINE vval_porcentaje           DECIMAL(5,2)
          DEFINE resp                      CHAR(1)
          DEFINE resp1                     CHAR(1)

 END GLOBALS

     MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o

        WHENEVER ERROR STOP
   
   	DEFER INTERRUPT

	LET HOY = TODAY
	OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CONTM001" ATTRIBUTE (BORDER)
	DISPLAY " CONTM004   GENERADOR ARCHIVO tmp_pla CONTABILIDAD (ICEFAS)                         " AT 3,1 
        ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 
        ATTRIBUTE(REVERSE)
        CALL startlog("icefas.log")

           MENU "Contabilidad Icefas"

             COMMAND "Generar" "Generar Contabilidad Icefas"
                      CALL inicializa_tablas()
		      CALL Genera_contabilidad()
             COMMAND "Salir" "Salir del Menu Actual"
                      EXIT MENU

         END MENU
      END MAIN
############################################################################
     FUNCTION Genera_contabilidad()
############################################################################

       -- inicializa con_pla_archivo
        DELETE FROM con_pla_archivo
        
	SELECT usuario_cod INTO vusuario FROM seg_usuario
	       WHERE usuario_cod = user

        IF vfecha_proceso IS NULL THEN
	   ERROR "FECHA PROCESO NO PUEDE SER NULO"
	   ATTRIBUTE (REVERSE)
	   SLEEP 2
	   ERROR " "
           RETURN
        END IF

        INPUT BY NAME vtransacc, vfolio1, vfolio2
             BEFORE FIELD vtransacc
              ERROR "SELECCIONE UNA TRANSACCION"
              ATTRIBUTE (REVERSE)
              SLEEP 2
              ERROR " "
             
	 AFTER FIELD vtransacc
	      IF vtransacc = 1 THEN 
                 LET vnombre = "IP"
		 NEXT FIELD vfolio1
              END IF
              
	      IF vtransacc = 2 THEN
                  LET vnombre = "IL"
                  PROMPT "Proporcione La Fecha a Procesar : "
	          ATTRIBUTE (REVERSE)
	          FOR vfecha_proceso
       	          ATTRIBUTE (REVERSE)
		  NEXT FIELD vfolio2
	      END IF

         AFTER FIELD vfolio1
	      LET vfolio3 = vfolio1
	      LET vreferencia = vfolio3, vreporte

	      SELECT folio FROM con_his_archivo
		  WHERE folio = vfolio1
		    AND transaccion MATCHES "11*" 
               GROUP BY 1
	       IF STATUS <> NOTFOUND THEN
                  ERROR "FOLIO YA PROVISIONADO .. VERIFIQUE"
		  ATTRIBUTE (REVERSE)
		  SLEEP 2
		  ERROR " "
		  NEXT FIELD vfolio1
               END IF

	       PROMPT "Desea Generar Provision Aportaciones ? S/N                                "
               ATTRIBUTE (REVERSE)
	       FOR resp
                IF resp MATCHES"[Ss]" THEN 
	           LET venvio = "11"
		   ERROR "PROCESANDO PROVISION APORTACIONES ...."
		   ATTRIBUTE (REVERSE)
		   SLEEP 2
		   
		   CALL provision_aportaciones_subcuenta(vfolio1)  --3101
                   CALL provision_aportaciones_vivienda(vfolio1)   --3102
		   CALL actualiza_hist_contab(venvio, vfolio1)
		   CALL reporte()
		   PROMPT "Provision Finalizada Presione  < ENTER > Para Continuar       "
                   ATTRIBUTE (REVERSE)
		   FOR vpregunta
                   ATTRIBUTE (REVERSE)
		   RETURN
		  
		  ELSE
                   ERROR "Provision Cancelada "
	           ATTRIBUTE (REVERSE)
                   SLEEP 2
                   ERROR " "
	           CLEAR FORM
		   RETURN
		END IF
	 
	 AFTER FIELD vfolio2
	       LET vfolio3 = vfolio2
	       LET vreferencia = vfolio2, vreporte

	       SELECT folio FROM con_his_archivo
		  WHERE folio = vfolio2
		    AND transaccion MATCHES "12*" 
               GROUP BY 1

	       PROMPT "Desea Generar Liquidacion Aportaciones ? S/N                                     "
               ATTRIBUTE (REVERSE)
	       FOR resp
                IF resp MATCHES"[Ss]" THEN 
		  LET venvio = "12" 
		  ERROR "PROCESANDO LIQUIDACION APORTACIONES ...."
		  ATTRIBUTE (REVERSE)
		  SLEEP 2
		  
		  CALL liquidacion_aportaciones(vfolio2)             --3201
		  CALL asignacion_titulos_posicion_propia(vfolio2)   --3208
		  CALL cancelacion_provision_aportaciones(vfolio2)   --3202
                  CALL compra_fraccion(vfolio2)                      --3204
                  CALL asignacion_recursos_posicion_propia(vfolio2)  --3203
                  ##CALL cancelacion_asignacion_recursos1(vfolio2) 
                  CALL cancelacion_asignacion_recursos(vfolio2)      --3206
                  CALL asignacion_titulos_siefore(vfolio2)           --3205
                  CALL asignacion_titulos_trabajadores1(vfolio2)     --3207
                  CALL cancelacion_titulos_trabajadores(vfolio2)     --3209
		  CALL actualiza_hist_contab(venvio, vfolio2)
		  CALL reporte()
		  
		  PROMPT "Liquidacion Finalizada Presione  < ENTER > Para Continuar       "
                   ATTRIBUTE (REVERSE)
		   FOR vpregunta
                   ATTRIBUTE (REVERSE)
		   RETURN
		 ELSE
                   ERROR "Provision Cancelada "
	           ATTRIBUTE (REVERSE)
                   SLEEP 2
                   ERROR " "
	           CLEAR FORM
		   RETURN
		END IF
     END INPUT    
  END FUNCTION


 ############################################################################
    FUNCTION provision_aportaciones_subcuenta(vfol_rec)  --3101
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vvsaldo_sar_92    DECIMAL(16,6)
      DEFINE vint_sar_92       DECIMAL(16,6)
      DEFINE vmonto_en_pesos   DECIMAL(16,6)
 
      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME
      LET vvsaldo_sar_92 = 0
      LET vmonto_en_pesos = 0

  -- 3101 --  Registro Provision Aportaciones SAR Anterior.

         SELECT sum(int_sar_92) INTO vint_sar_92 
            FROM tra_det_trasp_int
           WHERE folio = vfol_rec

         DECLARE c_cur CURSOR FOR
         SELECT sum(saldo_sar_92) --INT0 vvsaldo_sar_92 
            FROM tra_det_trasp_sal
           WHERE tra_det_trasp_sal.folio = vfol_rec   
         FOREACH c_cur into vvsaldo_sar_92    
         END FOREACH
 
      LET vmonto_en_pesos = vvsaldo_sar_92 +  vint_sar_92
 
      SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3101"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
         LET vtransa_desc= "PROVISION POR SUBCUENTA"

         LET vtransaccion = "31017"
	 
	 INSERT INTO con_pla_archivo 
            VALUES (vtransaccion,                  -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    vmonto_en_pesos,               -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "7",                           -- Codigo Analisis 1    
                    " ",                           -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    "    " )                       -- Codigo Analisis 4
        
	INITIALIZE gr_3101.* TO NULL
    END FUNCTION

 ############################################################################
    FUNCTION provision_aportaciones_vivienda(vfol_rec)
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vsaldo_viv_92     DECIMAL(16,6)
      
      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

      -- 3102 --  Registro de Aportaciones Vivienda

      SELECT sum(saldo_viv_92) INTO vsaldo_viv_92 
            FROM tra_det_trasp_sal
             WHERE folio = vfol_rec

      SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3102"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
      
         INSERT INTO con_pla_archivo 
            VALUES (vtransa_cod,                   -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    vsaldo_viv_92,                 -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    " ",                           -- Codigo Analisis 1    
                    " ",                           -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    "  ")                          -- Codigo Analisis 4
    END FUNCTION
 
 ############# --- Se inicia generacion de Liquidaciones  -- ###############

 ############################################################################
    FUNCTION liquidacion_aportaciones(vfol_rec) --3201
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      
      WHENEVER ERROR CONTINUE
      DROP TABLE con_ctr_subcuenta
      CREATE TABLE con_ctr_subcuenta 
      (
       subcuenta               CHAR(1),
       monto_en_pesos          DECIMAL(16,6),
       siefore                 CHAR(1),
       precio_accion           DECIMAL(16,6)
      );

      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

  -- 3201 --  Liquidacion Aportaciones

      DECLARE c_3201 CURSOR FOR
      SELECT "3201", sum(monto_en_pesos), siefore, subcuenta, precio_accion
             FROM dis_cuenta
            WHERE folio = vfol_rec   
              AND tipo_movimiento IN (1,2,3,4,5)
              AND subcuenta IN (7)
              AND fecha_conversion = vfecha_proceso
		GROUP BY 1,3,4,5
                  -- ORDER BY 2
      FOREACH c_3201 INTO gr_3201.*
              
	      INSERT INTO con_reg_subcuenta
                      values(gr_3201.subcuenta)
   
              INSERT INTO con_ctr_subcuenta
		     VALUES (gr_3201.subcuenta, gr_3201.monto_en_pesos,
			     gr_3201.siefore, gr_3201.precio_accion)

      
         SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3201"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
        
         LET vtransa_cod = gr_3201.transaccion, gr_3201.subcuenta

	 INSERT INTO con_pla_archivo 
            VALUES (vtransa_cod,                   -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    gr_3201.monto_en_pesos,        -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    gr_3201.siefore,               -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    "  " )                         -- Codigo Analisis 4
       
	 END FOREACH
    END FUNCTION

 ############################################################################
    FUNCTION cancelacion_provision_aportaciones(vfol_rec)  -- 3202
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vcuenta           CHAR(5)
      DEFINE vsubcta           CHAR(1)
     
      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME
    
  -- 3202 --  Cancelacion Provision Aportaciones

    DECLARE c_cur3202 CURSOR FOR 
         SELECT "3202", subcuenta, sum(monto_en_pesos)
            FROM dis_cuenta
              WHERE folio = vfol_rec   
               AND tipo_movimiento IN (1,2,4)
               AND subcuenta IN (7)
          GROUP BY 1,2
    FOREACH c_cur3202 INTO gr_3202.*

  	 SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3202"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
         LET vcuenta = "3202", gr_3202.subcuenta
 
         INSERT INTO con_pla_archivo 
            VALUES (vcuenta,                       -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    gr_3202.monto_en_pesos,        -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    gr_3202.subcuenta,             -- Codigo Analisis 1    
                    " ",                           -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    "  ")                          -- Codigo Analisis 4
 	  END FOREACH
    END FUNCTION

 ############################################################################
    FUNCTION compra_fraccion(vfol_rec) -- 3204
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vvfraccion        DECIMAL(16,6)
      DEFINE vvalor_accion     DECIMAL(16,6)
      DEFINE vmultiplica       DECIMAL(16,6)           


      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

  -- 3204 --  Compra Fraccion

       SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO gr_3204.transaccion, vtransa_desc, vdebito_credito, 
		vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3204"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"

	 SELECT fraccion, valor_accion INTO vvfraccion, vvalor_accion 
		 FROM con_ctr_fraccion
	
	 LET vvfraccion = vvfraccion * vvalor_accion

         INSERT INTO con_pla_archivo 
            VALUES ("3204",                        -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    vvfraccion,                      -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "  ",                          -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                          -- Codigo Analisis 3
                    gr_3204.siefore )              -- Codigo Analisis 4
 
    END FUNCTION

 ############################################################################
    FUNCTION asignacion_recursos_posicion_propia(vfol_rec) --3203
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vvfraccion        DECIMAL(16,6)
      DEFINE vvalor_accion     DECIMAL(16,6)
      DEFINE vmultiplica       DECIMAL(16,6)           


      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

  -- 3203 --  Asignacion Recursos Posicion Propia

       SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO gr_3203.transaccion, vtransa_desc, vdebito_credito, 
		vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3203"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
         

	 SELECT fraccion, valor_accion INTO vvfraccion, vvalor_accion 
		 FROM con_ctr_fraccion
	
	LET vvfraccion = vvfraccion * vvalor_accion

         INSERT INTO con_pla_archivo 
            VALUES ("3203",                        -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    vvfraccion,                    -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "  ",                          -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    gr_3203.siefore )              -- Codigo Analisis 4
 
    END FUNCTION
 
 ############################################################################
    FUNCTION asignacion_titulos_siefore(vfol_rec)  -- 3205
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vprecio_accion    DECIMAL(16,6)


      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

  -- 3205 --  Asignacion Titulos Siefore.
      
      SELECT precio_accion INTO vprecio_accion
            FROM con_ctr_subcuenta
	    GROUP BY 1

       
       SELECT "3205", monto INTO gr_3205.transaccion, gr_3205.monto_en_pesos
            FROM con_ctr_1211

       SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3205"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"

      LET gr_3205.monto_en_pesos = gr_3205.monto_en_pesos / vprecio_accion

         INSERT INTO con_pla_archivo 
            VALUES (gr_3205.transaccion,           -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    gr_3205.monto_en_pesos,        -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "  ",                          -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    gr_3205.siefore )              -- Codigo Analisis 4
 
    END FUNCTION

 ############################################################################
    FUNCTION asignacion_titulos_posicion_propia(vfol_rec) --3208
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vsuma           decimal(16,6)
      DEFINE vprecio1        decimal(16,6)
      DEFINE vfraccion       decimal(16,6)
      DEFINE vfraccion_tot   decimal(16,6)
      DEFINE vprecio         decimal(16,6)
      DEFINE vsuma1          decimal(16,6)
      
      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

         WHENEVER ERROR CONTINUE
         create table con_ctr_monto 
         (
                monto decimal(16,6),
                monto1 decimal(16,6)
         );
              
         create table con_ctr_monto1 
         (
                monto decimal(16,6)
         );

         create table con_ctr_monto2 
         (
                monto1 decimal(16,6),
                monto2 decimal(16,6)
         );

         create table con_ctr_monto2 
         (
                monto1 decimal(16,6),
                monto2 decimal(16,6)
         );


    -- 3208 --  Asignacion Titulos Posicion Propia

     SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
     WHERE transa_cod = "3208"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
         SELECT transa_cod INTO gr_3208.transaccion FROM tab_transaccion
		WHERE transa_cod = "3208"
	

         WHENEVER ERROR CONTINUE
	 
         create table con_ctr_monto 
         (
                monto decimal(16,6),
                monto1 decimal(16,6)
         );
              
         create table con_ctr_monto1 
         (
                monto decimal(16,6)
         );

         create table con_ctr_monto2 
         (
                monto1 decimal(16,6),
                monto2 decimal(16,6)
         );

         create table con_ctr_monto2 
         (
                monto1 decimal(16,6),
                monto2 decimal(16,6)
         );
	 

         WHENEVER ERROR STOP
	 
	 SELECT precio_accion, sum(monto_en_pesos)
             INTO vsuma, vprecio
	     FROM dis_cuenta
            WHERE folio = vfol_rec
	      AND subcuenta IN (7)
	      AND tipo_movimiento > 0
              AND fecha_conversion = vfecha_proceso
             group by 1

         LET vsuma_todo = vsuma

            INSERT INTO con_ctr_monto 
	      values (vsuma, vprecio)
          

          SELECT monto, monto1 INTO vprecio1, vsuma1
	      FROM con_ctr_monto

          SELECT monto, monto1 INTO vprecio, vsuma 
	      FROM con_ctr_monto

          LET vprecio1 = vsuma1 / vprecio1
          LET vprecio  = vsuma / vprecio

	    INSERT INTO con_ctr_monto1
		 VALUES (vprecio1)

          INSERT INTO con_ctr_monto2
		 VALUES (vprecio, vprecio1)

          SELECT trunc(monto2) + 1 INTO vprecio
                 FROM con_ctr_monto2

	  INSERT INTO con_ctr_monto3
		 VALUES (vprecio, vprecio1)
	  
	  SELECT monto1 INTO vprecio
		 FROM con_ctr_monto3
	  
          SELECT (monto1 - monto2)  INTO vfraccion_tot
                 FROM con_ctr_monto3

         INSERT INTO con_ctr_fraccion VALUES(vfraccion_tot, vsuma_todo)

         INSERT INTO con_pla_archivo 
            VALUES (gr_3208.transaccion,           -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    vfraccion_tot,                 -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "  ",                          -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    gr_3208.siefore)               -- Codigo Analisis 4
    
    END FUNCTION


 ############################################################################
    FUNCTION cancelacion_asignacion_recursos(vfol_rec) -- 3206
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vconcepto1        DECIMAL(16,6)    
      DEFINE vconcepto2        DECIMAL(16,6)    

      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

  -- 3206 --  Asignacion Titulos Trabajadores

     WHENEVER ERROR CONTINUE
     DROP TABLE con_ctr_1211
     CREATE TABLE con_ctr_1211 
            (
             transaccion     CHAR(4),
             monto           DECIMAL(16,6)
            );

     WHENEVER ERROR STOP
     SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
     WHERE transa_cod = "3206"

     LET gr_3206.transaccion = "3206"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
         
	 SELECT sum(monto) INTO vconcepto1
		FROM con_pla_archivo
		WHERE codigo_cuenta MATCHES "3201*"

	 SELECT monto INTO vconcepto2
		FROM con_pla_archivo
		WHERE codigo_cuenta = "3203"

	 LET vconcepto1 = vconcepto1 + vconcepto2
	 
         INSERT INTO con_ctr_1211
                 VALUES ("1211", vconcepto1)
	 
         INSERT INTO con_pla_archivo 
            VALUES ("3206",                        -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    vconcepto1,                    -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "  ",                          -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    " " )                          -- Codigo Analisis 4
       
    END FUNCTION
 
{
 ############################################################################
    FUNCTION cancelacion_asignacion_recursos(vfol_rec)
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
      DEFINE vconcepto1        DECIMAL(16,6)    
      DEFINE vconcepto2        DECIMAL(16,6)    

      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "TEST"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

  -- 1211 --  Asignacion Titulos Trabajadores

     SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
     WHERE transa_cod = "1211"

   --LET gr_1211.monto_en_pesos = gr_1204.monto_en_pesos+gr_1206.monto_en_pesos
     LET gr_1211.transaccion = "1211"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
        
         DECLARE c_actual CURSOR FOR
             SELECT "1211", monto FROM con_pla_archivo
                    WHERE codigo_cuenta[1,4] IN ("1204", "1205")
         FOREACH c_actual INTO gr_1211.transaccion, 
                               gr_1211.monto_en_pesos

	 INSERT INTO con_pla_archivo 
            VALUES (gr_1211.transaccion,           -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    gr_1211.monto_en_pesos,        -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "  ",                          -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    gr_1211.siefore )              -- Codigo Analisis 4
       
       END FOREACH
    END FUNCTION
}
 ############################################################################
    FUNCTION asignacion_titulos_trabajadores1(vfol_rec) -- 3207
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
 
      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

  -- 3207 --  Asignacion Titulos Trabajadores

       DELETE FROM con_ctr_accion
       DECLARE c_3207 CURSOR FOR
       SELECT "3207", precio_accion, monto_en_pesos, siefore, subcuenta
           FROM con_ctr_subcuenta
	   ORDER BY 4,5
       FOREACH c_3207 INTO gr_3207.*

       INSERT INTO con_ctr_accion
	      VALUES (gr_3207.precio_accion, gr_3207.monto_en_pesos,
		      gr_3207.subcuenta)
      
       SELECT monto1, monto2 INTO vprecio_accion, vmonto_en_pesos
	      FROM con_ctr_accion
             WHERE subcuenta = gr_3207.subcuenta

       LET vmonto_en_pesos = vmonto_en_pesos / vprecio_accion

       SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3207"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
	
	 LET vtransaccion = gr_3207.transaccion, gr_3207.subcuenta
	 
         INSERT INTO con_pla_archivo 
            VALUES (vtransaccion,                  -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    vmonto_en_pesos,               -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "  ",                          -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    gr_3207.siefore )              -- Codigo Analisis 4
         
	 END FOREACH
    END FUNCTION

 ############################################################################
    FUNCTION cancelacion_titulos_trabajadores(vfol_rec) --3209
 ############################################################################

      DEFINE vfecha_calculo    CHAR(7)
      DEFINE vhoy              DATE
      DEFINE vanio             CHAR(4)
      DEFINE vfol_rec          INTEGER   
 
      LET vhoy  = today
      LET vanio = year(vhoy)
      LET vtipo_registro = "M"
      LET vconstante = "71157"
      LET vfecha_calculo = today
      LET vfecha_pago = today
      LET vfolio_interno = "REFERENCIA"
      LET vfecha = TODAY,  TIME

  -- 3209 --  Asignacion Titulos Siefore

       
       SELECT "3209", monto INTO gr_3209.transaccion, gr_3209.monto_en_pesos
            FROM con_pla_archivo
        WHERE codigo_cuenta = "3205"     
 
       SELECT transa_cod, transa_desc, debito_credito, provi_acred
           INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
           FROM tab_transaccion
         WHERE transa_cod = "3209"

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
	 
         INSERT INTO con_pla_archivo 
            VALUES (vtransa_cod,                   -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_pago,                   -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    gr_3209.monto_en_pesos,        -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vfolio_interno,                -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    "  ",                          -- Codigo Analisis 1    
                    "  ",                          -- Codigo Analisis 2
                    vfol_rec,                      -- Codigo Analisis 3
                    gr_3209.siefore )              -- Codigo Analisis 4
 
    END FUNCTION

#############################################################################
        FUNCTION calc_dias(vdias)
#############################################################################
        
        DEFINE vdias     DATE
        DEFINE vvdias    SMALLINT
        DEFINE vacum     SMALLINT
        DEFINE vmonth    SMALLINT

        LET vvdias = day(vdias)
        LET vmonth  = month(vdias)

        CASE
             WHEN vmonth = 1 
               LET vacum = vvdias
             WHEN vmonth = 2 
               LET vacum = 31 + vvdias
             WHEN vmonth = 3 
               LET vacum = 59 + vvdias
             WHEN vmonth = 4 
               LET vacum = 90 + vvdias
             WHEN vmonth = 5 
               LET vacum = 120 + vvdias
             WHEN vmonth = 6 
               LET vacum = 151 + vvdias
             WHEN vmonth = 7 
               LET vacum = 181 + vvdias
             WHEN vmonth = 8 
               LET vacum = 212 + vvdias
             WHEN vmonth = 9 
               LET vacum = 243 + vvdias
             WHEN vmonth = 10 
               LET vacum = 273 + vvdias
             WHEN vmonth = 11 
               LET vacum = 304 + vvdias
             WHEN vmonth = 12 
               LET vacum = 334 + vvdias
          END CASE
      
     RETURN vacum 
END FUNCTION

 #########################################################################
  FUNCTION REPORTE()
 #########################################################################
  
   DEFINE gr_report RECORD
          codigo_cuenta char(15),
          periodo_contab char(7),
          fecha_transacc date,
          tipo_registro char(1),
          monto decimal(16,2),
          debito_credito char(1),
          tipo_diario char(5),
          referencia char(21),
          desc_conc_mov char(25),
          codigo_analisis1 char(15),
          codigo_analisis2 char(15),
          codigo_analisis3 char(15),
          codigo_analisis4 char(15)
    END RECORD

--  LET G_LISTA =  g_paramgrales.ruta_spool CLIPPED,"/",g_usuario CLIPPED,".SOL_RECH_CURP_",hoy USING "dd-mm-yy","_",hora CLIPPED
	    

   --LET G_LISTA =  "/u/FTE/CENVIO/", vnombre CLIPPED, vfolio3 CLIPPED, ":",  HOY USING "YYYYMMDD"
   LET G_LISTA =  "/LISTADOS/", vnombre CLIPPED, vfolio3 CLIPPED, ":",  HOY USING "YYYYMMDD"

   DECLARE c_contab CURSOR FOR
        SELECT codigo_cuenta, periodo_contab, fecha_transacc, tipo_registro,
	       round(monto, 2), debito_credito, tipo_diario, referencia,
	       desc_conc_mov, codigo_analisis1, codigo_analisis2,
	       codigo_analisis3, codigo_analisis4
	FROM con_pla_archivo ORDER BY 1 
        START REPORT contab to G_LISTA
   FOREACH c_contab INTO gr_report.*
        OUTPUT TO REPORT contab(gr_report.*)
   END FOREACH
        ERROR "GENERANDO REPORTE ARCHIVO PLANO"
        ATTRIBUTE (REVERSE)
        SLEEP 2
        ERROR " "
   
    FINISH REPORT contab
  END FUNCTION

 #########################################################################
    report contab(rpt)
 #########################################################################
      
    DEFINE rpt RECORD 
           codigo_cuenta char(15),
           periodo_contab char(7),
           fecha_transacc date,
           tipo_registro char(1),
           monto decimal(16,2),
           debito_credito char(1),
           tipo_diario char(5),
           referencia char(10),    ---char(21)
           desc_conc_mov char(15),
           codigo_analisis1 char(15),
           codigo_analisis2 char(15),
           codigo_analisis3 char(15),
           codigo_analisis4 char(15)
    END RECORD

    OUTPUT
        PAGE LENGTH 200
        LEFT MARGIN 0
        TOP MARGIN 0

    FORMAT
        PAGE HEADER
     
      SKIP 0 LINES
  
      ON EVERY ROW 
 
       print
                   
            COLUMN 01, rpt.codigo_cuenta,      -- Account Code
            COLUMN 16, rpt.periodo_contab,     -- Accounting Period
            COLUMN 23, rpt.fecha_transacc USING "yyyymmdd", -- Transaction Date   
            COLUMN 31, "  ",                   -- Blank 
            COLUMN 33, rpt.tipo_registro,      -- Record Type
            COLUMN 34, "  ",                    -- SunBusiness Journal Number
            COLUMN 36, "     ",
            COLUMN 41, "     ",                 -- Line Number
            COLUMN 46, "  ",                    -- BlanK
            --COLUMN 48, rpt.monto USING "#################0",  -- Amount
            COLUMN 48, rpt.monto USING "##############.&&&",  -- Amount
            COLUMN 66, rpt.debito_credito,      -- Debit/Credit Marker
            COLUMN 67, " ",                     -- Allocation Indicador
            COLUMN 68,  "TEST",                 -- Journal Type
            COLUMN 73,  "    ",                 -- Journal Source
            COLUMN 78,  vreferencia,            -- Transaction Reference 
            COLUMN 93,  rpt.desc_conc_mov,      -- Descripction
            COLUMN 242, "              ",       -- Analysis Code 0
            COLUMN 257, rpt.codigo_analisis1,   -- Analysis Code 1
            COLUMN 272, rpt.codigo_analisis3,   -- Analysis Code 2
            COLUMN 287, rpt.codigo_analisis2,   -- Analysis Code 3
            COLUMN 302, vfolio3,                -- Analysis Code 4
            COLUMN 317, vreporte                -- Analysis Code 5
end report

 #########################################################################
    FUNCTION inicializa_tablas()    
 #########################################################################
        
	--delete from contab_plano;
        --delete from hist_contab;
        delete from ctrl_acciones;
        delete from ctrl_fraccion;
        delete from ctrl_monto;
        delete from ctrl_monto1;
        delete from ctrl_monto2;
        delete from ctrl_monto3;
    
    END FUNCTION

  #########################################################################
     FUNCTION actualiza_hist_contab(vtransa, vvfolio)
  #########################################################################

        DEFINE vcodigo_cuenta       CHAR(5),
	       vfecha_transacc      DATE,
               vmonto               DECIMAL(16,6),
	       vfolio               CHAR(4),
               vstatus              SMALLINT,
	       vusuario             CHAR(10),
	       vtransa              CHAR(2),
	       vvfolio              CHAR(4)

      SELECT usuario_cod INTO vusuario FROM seg_usuario
               WHERE usuario_cod = user
      
      IF vtransacc = "1" THEN
         LET vfecha_proceso = TODAY
         DECLARE hist_contab1 CURSOR FOR
             SELECT codigo_cuenta, fecha_transacc, monto,
	      codigo_analisis3[1,2] 
              FROM con_pla_archivo
	     WHERE codigo_cuenta MATCHES"31*"

         FOREACH hist_contab1 INTO vcodigo_cuenta, 
	      vfecha_transacc, vmonto, vfolio

           INSERT INTO con_his_archivo
              VALUES (vfecha_transacc,  vcodigo_cuenta, vmonto,
      	      vvfolio, "", vusuario, vfecha_proceso)
         END FOREACH  
      END IF
 
  
 
      IF vtransacc = "2" THEN
         DECLARE hist_contab2 CURSOR FOR
             SELECT codigo_cuenta, fecha_transacc, monto,
	      codigo_analisis3[1,2] 
              FROM con_pla_archivo
	     WHERE codigo_cuenta MATCHES"32*"

         FOREACH hist_contab2 INTO vcodigo_cuenta, 
	      vfecha_transacc, vmonto, vfolio
           

           INSERT INTO con_his_archivo
              VALUES (vfecha_transacc,  vcodigo_cuenta, vmonto,
      	      vvfolio, "", vusuario, vfecha_proceso)
         END FOREACH  
           --DELETE FROM con_pla_archivo
      END IF

   END FUNCTION

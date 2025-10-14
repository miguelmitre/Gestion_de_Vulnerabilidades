###############################################################################
#Proyecto          => AFORE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  #
#Programa TAAC004  => REVERSA PROVISION Y/O LIQUIDACION TRASPASOS RECIBIDOS   #
#                  => AFORE RECEPTORA                                         #
#Autor             => MAURO MUNIZ CABALLERO                                   #
#Fecha             => 31 DE ENERO DE 2001                                     #
#Modifiacion       => JOSUE LISANDRO HUERTA SIERRA                            #
#Fecha             => 2 DE OCTUBRE DE 2007                                    #
#   A LA FUNCION reversa_liquidacion SE AGREGO EL DELETE DE LA TABLA          #
#   cta_saldo_devol Y LA DESMARCA DE 243 Y 244                                #
#Sistema           => TAA                                                     #
#Req: 168          => JCPV 30/09/2011. opc 5 vs opc 6                         # 
#Req: 1038         => JCPV 15/11/2012 Apertura de Cuentas Certificadas.       #
#CPL-1507          => FSR  11/02/2014 Se actualiza reverso de carga           #
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_param_taa    RECORD LIKE seg_modulo.*
    DEFINE s_codigo_afore LIKE tab_afore_local.codigo_afore

    DEFINE
        opcion       SMALLINT,
        vfolio       INTEGER,
        vfecha       DATE,
        hoy          DATE,
        enter        CHAR(1),
        vfile_name   CHAR(25),
        vfecha_recep DATE,                                             #1038
        vtipo_solic INTEGER									#1142

    DEFINE
        vproceso_cod CHAR(5)

    DEFINE
        txt_cla1     CHAR(500)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION inicio()
#i-------------

    LET hoy = today

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_taa.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    LET vproceso_cod = '00009'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW TAAC0041 AT 4,4 WITH FORM "TAAC0041" ATTRIBUTE(BORDER)
    DISPLAY "TAAC004         REVERSO DE OPERACIONES  TAAR " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio, vfile_name,opcion,vfecha_recep,vtipo_solic

      AFTER FIELD vfolio
         IF vfolio IS NULL THEN
            NEXT FIELD opcion      
         ELSE 
            SELECT "X"
              FROM con_transaccion
             WHERE @folio       = vfolio
               AND @proceso_cod = vproceso_cod
               AND @estado      = 40
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               PROMPT " FOLIO PROCESADO EN EL AREA DE CONTABILIDAD,",
                      " [Enter] continuar "
               ATTRIBUTES (REVERSE) FOR enter
               NEXT FIELD vfolio
            END IF

            SELECT "X"
            FROM   taa_cza_recepcion h
            WHERE  h.folio = vfolio

            IF SQLCA.SQLCODE <> 0 THEN
         PROMPT " EL FOLIO NO ES DEL PROCESO DE TRASPASO, [Enter] continuar "
         ATTRIBUTES (REVERSE) FOR enter
                NEXT FIELD vfolio     
            END IF

            NEXT FIELD opcion
         END IF
      AFTER FIELD opcion
         IF opcion IS NULL THEN
            ERROR "Campo opcion reverso NO puede ser NULO"
            NEXT FIELD opcion
         ELSE
            IF opcion  > 7 THEN                                       #1038
                ERROR "REVERSAR: 0=> CARGA, 1=> PROVISION, 2=> LIQUIDACION, 3=> PROV/LIQ, 4=>PROCESO, 5=>HISTORICO 6=>APERTURA"
                SLEEP 3
                ERROR "7=>APERTURA CTAS. CERTIFICADAS"
                NEXT FIELD opcion
            ELSE
             IF   opcion <> 0  
             AND  opcion <> 7  THEN
               IF vfolio IS NULL THEN
                 LET opcion = NULL
                 NEXT FIELD vfolio
                ELSE
                 EXIT INPUT                        
               END IF
              ELSE
               IF  opcion = 0  THEN                                   #1038
                NEXT FIELD vfile_name
               ELSE
                NEXT FIELD vfecha_recep                               #1038
               END IF
             END IF
            END IF
         END IF

      AFTER FIELD vfile_name 
         IF vfile_name IS NULL
         OR vfile_name = ' ' THEN
           ERROR "Teclear nombre del archivo"
           NEXT FIELD vfile_name
          ELSE
           LET vfolio = null
           DISPLAY BY NAME vfolio
           EXIT INPUT
         END IF

     AFTER FIELD vfecha_recep                                         #1038
         IF vfecha_recep IS NULL
         OR vfecha_recep = ' ' THEN
           ERROR "Teclear fecha de recepcion"
           NEXT FIELD vfecha_recep
         ELSE
          NEXT FIELD vtipo_solic	
         END IF
        AFTER FIELD vtipo_solic
         IF vtipo_solic IS NULL THEN
            ERROR "Tipo Solicitud  NO puede ser NULO"
            NEXT FIELD vtipo_solic
         ELSE
           LET vfolio = null
           DISPLAY BY NAME vfolio
           EXIT INPUT
         END IF
         
        ON KEY ( INTERRUPT )
            EXIT PROGRAM

    END INPUT

    WHILE TRUE
        PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN 
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
                SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    #DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    ERROR "PROCESANDO INFORMACION " 

    CASE opcion 
        WHEN 0 CALL reversa_carga()            
        WHEN 1 CALL reversa_provision()  #rp
               CALL actualiza_hist_af()     
        WHEN 2 CALL reversa_liquidacion() #rl
               CALL actualiza_histdep()     
        WHEN 3 CALL reversa_provision() #rp
               CALL reversa_liquidacion() #rl
               CALL actualiza_hist_af()     
               CALL actualiza_histdep()     
        WHEN 4 CALL reversa_provision() #rp
               CALL reversa_liquidacion() #rl
               CALL reversa_historico() #rl
               CALL reversa_ctr_arh() #rca
        WHEN 5 CALL reversa_historico() #rl
        WHEN 6 CALL reversa_apertura() #ra        
        WHEN 7 CALL reversa_ap_ctas_cert()         
    END CASE

    PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE) 
    FOR enter

    CLOSE WINDOW TAAC0041

END FUNCTION

FUNCTION reversa_carga()

DEFINE r_afi_solicitud RECORD LIKE afi_solicitud.*

INITIALIZE r_afi_solicitud.* TO NULL

    SELECT "X"
    FROM taa_ctr_traspaso
    WHERE nombre_archivo = vfile_name
    GROUP BY 1
    IF  SQLCA.SQLCODE <> 0 THEN
      ERROR "Nombre del archivo incorrecto, verifique"
      SLEEP 4
      ERROR "Opcion de reverso no procede"              
      SLEEP 4
    ELSE
      UPDATE safre_af:afi_solicitud
       SET status_interno = 70,
           finicta        = NULL
       WHERE n_seguro in(select n_seguro from safre_tmp:det_tra_viv)
         AND status_interno  = 100                                    #1038 =75
    
      UPDATE safre_af:afi_solicitud
       SET status_interno = 70,
           finicta        = NULL
       WHERE n_unico in(select n_unico from safre_tmp:det_tra_viv)
         AND status_interno  = 100                                    #1038 =75
       
      #CPL-1507 INI
       DELETE 
       FROM afi_solicitud
       WHERE tipo_solicitud IN (13,17)
       AND status_interno = 70
       AND n_seguro IN (SELECT n_seguro from safre_tmp:det_tra_viv); 
  
      DECLARE cur_solic CURSOR FOR 
        SELECT n_seguro       , 
         n_unico              ,
         n_rfc                ,
         paterno              ,
         materno              ,
         nombres              ,
         fena                 ,
         n_folio              ,
         edo_civil            ,
         localn               ,
         estadon              ,
         tiptr                ,
         cod_promotor         ,
         sexo                 ,
         n_operac             ,
         frecafor             ,
         fentcons             ,
         femision             ,
         finitmte             ,
         finicta              ,
         @status              ,
         agenc_cod            ,
         status_interno       ,
         nacionalidad         ,
         tip_prob             ,
         fol_prob             ,
         doc_prob             ,
         ind_infonavit        ,
         documento_1          ,
         documento_2          ,
         documento_3          ,
         documento_4          ,
         documento_5          ,
         documento_6          ,
         envio_dom            ,
         entidad_curp         ,
         asigna_curp          ,
         const_curp           ,
         usuario              ,
         hora                 ,
         status_captura       ,
         tipo_solicitud       ,
         fecha_elaboracion    ,
         lote                 ,
         fecha_envio          ,
         cod_esq_comision     ,
         ubicacion            ,
         fecha_1a_afil        ,
         indicador_c          ,
         indicador_d          ,
         indicador_e          ,
         cod_error_origen     ,
         folio_edo_cta        ,
         cod_afore_ced        ,
         salario_base_comis   ,
         salario_actual       ,
         fecha_actualiza_sa   ,
         coduni_n1            ,
         indicador_comision   ,
         codven               ,
         coor_captura         ,
         lote_captura         ,
         folio_captura        ,
         sello_electronico   
        FROM afi_his_solicitud 
        WHERE nombre_archivo = vfile_name
        
        FOREACH cur_solic INTO r_afi_solicitud.*
          INSERT INTO afi_solicitud VALUES (	r_afi_solicitud.*)
        END FOREACH
        
        DELETE 
        FROM afi_his_solicitud
        WHERE nombre_archivo = vfile_name;
       
       #CPL-1507 FIN 
       
       
       DELETE FROM safre_af:taa_ctr_traspaso
        WHERE nombre_archivo = vfile_name
    END IF

END FUNCTION     #reversa_carga()

FUNCTION reversa_provision()
#rp-------------------------

    DELETE
    FROM   dis_provision
    WHERE  folio = vfolio

    UPDATE taa_ctr_traspaso
    SET    ini_provision = NULL,
           fin_provision = NULL,
           usr_provision = NULL
    WHERE  folio         = vfolio

END FUNCTION

FUNCTION reversa_liquidacion()
#rp---------------------------

    DEFINE vnss               CHAR(11)
    DEFINE vtipo_traspaso     SMALLINT
    DEFINE vmarca_cod         SMALLINT
    DEFINE qry_rev_marca      CHAR(200)#CPL-1331
    DEFINE vfecha_mov_banxico DATE,
    			 vident_garantia    SMALLINT, #CPL-1331
    			 vcorrelativo				INTEGER,
    			 vdias_cotizados		SMALLINT	
    			 

    DELETE 
    FROM   dis_cuenta
    WHERE  folio = vfolio

    UPDATE dis_provision
    SET    estado = 5
    WHERE  folio = vfolio

    DELETE
    FROM   cta_saldo_vol
    WHERE  folio = vfolio

		
		#CPL-1331 Para desmarcar las cuentas marcadas durante el proceso
		LET qry_rev_marca = "EXECUTE PROCEDURE reversa_marca ( ?, ?, ?)"
    PREPARE prp_rev_marca FROM qry_rev_marca
    
    DECLARE cur_desmarca CURSOR FOR
    SELECT t.nss, t.tipo_traspaso, t.fecha_mov_banxico, t.ident_garantia
    FROM   taa_viv_recepcion t
    WHERE  t.folio = vfolio

    FOREACH cur_desmarca INTO vnss, vtipo_traspaso, vfecha_mov_banxico, vident_garantia
		    #Atualizamos dias cotizados 
		    SELECT dias_cotizados
		    INTO vdias_cotizados
		    FROM cta_his_cuenta
		    WHERE nss = vnss 
		    AND fecha_registro =   vfecha_mov_banxico
		    
		    UPDATE cta_ctr_cuenta 
			  SET dias_cotizados     = vdias_cotizados
			  WHERE  nss             = vnss
			  
			  DELETE FROM  cta_his_cuenta
	      WHERE nss = vnss 
		    AND fecha_registro =   vfecha_mov_banxico
					
			#Obtenemos todas las cuentas marcas del folio (con marca 243, 244, 237, 230 para desmarcar	
			
			DECLARE cur_des CURSOR FOR 
					SELECT marca_cod, correlativo
					FROM cta_act_marca
					WHERE nss=  vnss
					AND marca_cod IN (243,244,237,230)
					AND correlativo = vfolio
					
			FOREACH cur_des INTO vmarca_cod, vcorrelativo		
			  EXECUTE prp_rev_marca USING 	 vnss,
                                       vmarca_cod,
                                       vcorrelativo		 	
		   
		    END FOREACH
        LET txt_cla1 = "EXECUTE PROCEDURE fn_rev_reg_bono_issste(",
                       '"', vnss, '",',
                       '"TAR",',
                       '"', vfecha_mov_banxico, '")'
                                                    
        LET txt_cla1 = txt_cla1
              
        PREPARE claexe1 FROM txt_cla1
        EXECUTE claexe1                                                            
        
      END FOREACH
        {UPDATE cta_his_bono
           SET fecha_reingreso = NULL
         WHERE nss             = vnss
           AND fecha_reingreso = vfecha_mov_banxico
           
        DELETE 
          FROM cta_act_bono
         WHERE nss             = vnss
           AND fecha_registro  = vfecha_mov_banxico}
        


    UPDATE taa_ctr_traspaso
    SET    ini_liquida = NULL,
           fin_liquida = NULL,
           usr_liquida = NULL
    WHERE  folio       = vfolio

END FUNCTION

FUNCTION actualiza_hist_af()
#aha-----------------------

    UPDATE taa_cza_recepcion
    SET    estado = 1
    WHERE  folio = vfolio

    UPDATE taa_rcv_recepcion 
    SET    estado = 1
    WHERE  folio = vfolio

    UPDATE taa_viv_recepcion 
    SET    estado = 1
    WHERE  folio = vfolio

    UPDATE taa_sum_recepcion 
    SET    estado = 1
    WHERE  folio = vfolio

END FUNCTION

FUNCTION actualiza_histdep()
#aha-----------------------

    UPDATE taa_recepcion_af 
    SET    estado = 2
    WHERE  folio = vfolio

END FUNCTION

FUNCTION reversa_historico()

     DEFINE vnss            CHAR(11)                                  #1038
----( 168 )--[->]

    WHILE TRUE
    PROMPT "Va a reversar la Apertura de Cuentas tambien.... ?" FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
              ERROR "Entonces debe seleccionar primero esa opcion " 
               SLEEP 3
               RETURN                     
             ELSE
              EXIT WHILE
            END IF
        END IF
    END WHILE

    WHILE TRUE
    PROMPT "Despues no podra reversar la Apertura de Cuentas, desea continuar ?" FOR enter
      IF enter MATCHES "[sS]" THEN
              EXIT WHILE
       ELSE   
        ERROR  "No se ejecuta el reverso seleccionado"
        SLEEP 3
        RETURN       
      END IF
    END WHILE
----( 168 )--[<-]   

    ERROR "Ejecutandose reversa_historico, espere "
    SLEEP 3

    DELETE FROM taa_cza_recepcion
    WHERE  folio = vfolio
    
    DECLARE csr01 CURSOR FOR                                          #1038
     SELECT  tvr.nss                                                  #1038
     FROM    taa_viv_recepcion tvr                                    #1038
     WHERE   folio = vfolio                                           #1038
    FOREACH  csr01 INTO vnss                                          #1038
     UPDATE  afi_solicitud                                            #1038
      SET    finicta = ' '                                            #1038
     WHERE   n_seguro    =   vnss                                     #1038
      AND    status_interno  = 100                                    #1038
    END FOREACH                                                       #1038
    
    DELETE FROM taa_rcv_recepcion 
    WHERE  folio = vfolio

    DELETE FROM taa_viv_recepcion 
    WHERE  folio = vfolio

    DELETE FROM taa_sum_recepcion 
    WHERE  folio = vfolio

    DELETE FROM taa_recepcion_af 
    WHERE  folio = vfolio

    UPDATE taa_ctr_traspaso
    SET    ini_incorpora = NULL,
           fin_incorpora = NULL,
           usr_incorpora = NULL,
           reg_incorpora = NULL,
           folio         = NULL 
    WHERE  folio       = vfolio

END FUNCTION     #reversa_historico()

FUNCTION reversa_ctr_arh() #rca

    DELETE FROM taa_ctr_traspaso
    WHERE  folio = vfolio

END FUNCTION

FUNCTION reversa_apertura()
#ra----------------------------

    DEFINE l_ejecuta          CHAR(200)
    DEFINE cont_reg_liq       INTEGER
    DEFINE cont_reg_prov      INTEGER
    DEFINE bnd_apertura       SMALLINT
    DEFINE fecha_liquidacion  DATE

    LET bnd_apertura = FALSE
    LET bnd_apertura = TRUE     #TEMP

    SELECT fecha_mov_banxico
      INTO fecha_liquidacion
      FROM taa_viv_recepcion
     WHERE folio = vfolio
     GROUP BY 1

    SELECT COUNT(*)
      INTO cont_reg_liq
      FROM dis_cuenta
     WHERE folio = vfolio

    SELECT COUNT(*)
      INTO cont_reg_prov
      FROM dis_provision
     WHERE folio = vfolio
###TEMP
{
    IF cont_reg_liq > 0 OR
       cont_reg_prov > 0 THEN
        IF cont_reg_liq > 0 THEN
            PROMPT " FOLIO LIQUIDADO, NO PUEDE REVERSAR APERTURA,",
                   " [Enter] continuar " 
               FOR enter
            LET bnd_apertura = FALSE
        ELSE
            PROMPT " FOLIO PROVISIONADO, NO PUEDE REVERSAR APERTURA,",
                   " [Enter] continuar " 
               FOR enter
            LET bnd_apertura = FALSE
        END IF
    ELSE
        IF hoy > fecha_liquidacion THEN
            LET bnd_apertura = FALSE
            PROMPT " NO PUEDE REVERSAR APERTURA FECHA LIQUIDACION MENOR,",
                   " [Enter] continuar " 
               FOR enter
        ELSE
            LET bnd_apertura = TRUE
        END IF
    END IF
}
###TEMP
    IF bnd_apertura THEN
        LET l_ejecuta = " fglgo TAAC016 1 ", vfolio, "  ", fecha_liquidacion
        RUN l_ejecuta
    ELSE
        EXIT PROGRAM
    END IF
END FUNCTION        #reversa_apertura()

FUNCTION reversa_ap_ctas_cert()
#ra----------------------------

    DEFINE l_ejecuta          CHAR(200)
    DEFINE cont_reg_liq       INTEGER
    DEFINE cont_reg_prov      INTEGER
    DEFINE bnd_apertura       SMALLINT
    DEFINE fecha_liquidacion  DATE
    DEFINE vtipo_solic				INTEGER

    LET bnd_apertura = FALSE
    LET bnd_apertura = TRUE     #TEMP

     
    IF bnd_apertura THEN
        LET l_ejecuta = " fglgo TAAC016A 1 ", vfecha_recep, vtipo_solic
        RUN l_ejecuta
    ELSE
        EXIT PROGRAM
    END IF
END FUNCTION       #reversa_ap_ctas_cert()
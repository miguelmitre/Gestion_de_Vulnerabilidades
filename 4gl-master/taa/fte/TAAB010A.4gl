###########################################################################
#Proyecto          => SAFRE  ( MEXICO )                                   #
#Propietario       => E.F.P.                                              #
#Programa TAAB010A => SUBE AFILIADOS POR TRASPASO AL MAESTRO              #
#Sistema           => TAA                                                 #
#Autor             => MAURO MUNIZ CABALLERO                               #
#Clonó             => Juandelacruz Peral Villaraos                        #
#Fecha             => Noviembre de 2012                                   #
#Modifico          => MAURO MUÑIZ CABALLERO                               #
#Fecha             => 19 DE ABRIL DE 2005                                 #
#Ult. Mod.         => 13 ENERO 2017 CPL-2468                              #
#                     05 DE AGOSTO DEL 2016                               #
#REQ:1038          => Apertura de Cuentas Certificadas                    #
#REQ-1142		=> Agregar filtro tipo solicitud							                #
#CPL-1875   => El status_interno se modifica de 70 a 100                  #
#CPL-2372   => Quitar los Updates  o Asignacion de Variables al Campo     #
#              finicta antes de realizarle la Insercion a safre_af:afi_mae#
#              _afiliado,por tanto se dejara la finicta que traiga origi- #
#              nalmente la tabla  afi_solicitud                           #
###########################################################################

DATABASE safre_af

GLOBALS

    DEFINE HOY       DATE
    DEFINE fecha_bnx DATE
    DEFINE generar   CHAR(1)
    DEFINE HORA      CHAR(8)
    DEFINE g_usuario CHAR(8)
    DEFINE G_LISTA   CHAR(300)
    DEFINE accion    SMALLINT
    DEFINE i         SMALLINT
    DEFINE vfolio    INTEGER
    DEFINE cont_ctas INTEGER
    DEFINE enter     SMALLINT
    DEFINE vfecha_liq DATE
    DEFINE vtipo_solic INTEGER #1142
    DEFINE ctipo_solic VARCHAR(5)


    DEFINE g_seg_modulo   RECORD LIKE seg_modulo.*

    DEFINE
        vmarca_entra      SMALLINT ,
        vmarca_estado     SMALLINT ,
        vcodigo_rechazo   SMALLINT ,
        ejecuta           CHAR(300),
        xcodigo_marca     SMALLINT ,
        xcodigo_rechazo   SMALLINT ,
        edo_proc          SMALLINT ,
        edo_mod           SMALLINT ,
        edo_acr           SMALLINT ,
        v_desmarca        CHAR(300),
        vmarca            SMALLINT ,
        vcorrelativo      INTEGER  ,
        v_ind_edad        SMALLINT ,
        v_curp            CHAR(18) ,
        v_rfc             CHAR(13) ,
        v_fnacimiento     DATE

    DEFINE exe_sql2          ,
           exq_sql3          CHAR(100)
    DEFINE exe_uni           CHAR(100),
           exe_fnacimiento   CHAR(300)

    DEFINE p_proceso      SMALLINT
    DEFINE p_marca_c      SMALLINT

    DEFINE v_cae_rech          SMALLINT

    DEFINE gs_rechazos    SMALLINT

    DEFINE gc_reporte     CHAR(300)
    DEFINE v_corr              INTEGER              #750
    DEFINE pmarca_entra        SMALLINT             #750
    DEFINE pmarca_causa        SMALLINT             #750
    DEFINE pestado_marca       SMALLINT             #750
    DEFINE pcodigo_rechazo     SMALLINT             #750
    DEFINE vctas_apert         INTEGER              #1038
    DEFINE vctas_noapert        INTEGER             #1038
END GLOBALS

-------------------------------------------------------------------------------

MAIN

    OPTIONS INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG(FGL_GETENV("USER")||".TAAB010A.log")
    
    CALL inicio()                 #i

    IF accion THEN
        CALL traspasa_datos()     #td
    ELSE
        CALL proceso_principal()  #pp
        DISPLAY "TOTAL DE CUENTAS APERTURADAS :" ,vctas_apert
                AT 14,4 ATTRIBUTE(REVERSE)                            #1038
        DISPLAY "TOTAL DE CUENTAS NO APERTURADAS :" ,vctas_noapert
                AT 15,4 ATTRIBUTE(REVERSE)                            #1038
        DISPLAY "ARCHIVO GENERADO" AT 16,1 ATTRIBUTE(REVERSE)
        DISPLAY gc_reporte CLIPPED AT 17,1 ATTRIBUTE(REVERSE)
        PROMPT" PRESIONE [Enter] PARA SALIR ..." FOR enter
    END IF

END MAIN

-------------------------------------------------------------------------------

FUNCTION inicio()
#i---------------

    LET HOY    = TODAY
    LET HORA   = TIME
    LET accion = ARG_VAL(1)
    LET vfolio = ARG_VAL(2)
    LET vfolio = 0    

    LET edo_proc        = 237
    LET edo_acr         = 230
    LET edo_mod         = 610
    LET vmarca          = 0
    LET vmarca_estado   = 0
    LET vcodigo_rechazo = 0

    LET p_proceso       = 0
    LET p_marca_c       = 0
    LET cont_ctas       = 0

    LET gs_rechazos     = 0
    LET vctas_apert     = 0
    LET vctas_noapert   = 0

    SELECT *, USER
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".SUBE_TRASP_MTO" CLIPPED,
                "_",HOY USING "dd-mm-yy","_",HORA CLIPPED

    LET ejecuta    = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"
    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    LET exe_sql2   = " EXECUTE PROCEDURE fn_regimen_inv (?,?,?,?,?,?)"
    LET exe_uni    = " EXECUTE PROCEDURE sp_recupera_marca (?,?,?)"
    LET exe_fnacimiento = "EXECUTE PROCEDURE fn_fnacimiento ( ?, ?)"
    LET exq_sql3   = " EXECUTE FUNCTION fn_valida_edad_sol (?,?) "

    PREPARE prp_fnacimiento FROM exe_fnacimiento
    DECLARE cur_fnacimiento CURSOR FOR prp_fnacimiento

    PREPARE prp_fecha_sol FROM exq_sql3   


END FUNCTION

-------------------------------------------------------------------------------

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TAAB010A1" ATTRIBUTE(BORDER)

    DISPLAY " TAAB010A       SUBE AFILIADOS AL MAESTRO                         " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                          [ Ctrl-C ] Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfecha_liq, vtipo_solic,generar
    	
        AFTER FIELD vfecha_liq
            IF vfecha_liq IS NULL THEN
                ERROR "Fecha no puede ser nula"
                NEXT FIELD vfecha_liq
            ELSE
                NEXT FIELD vtipo_solic
            END IF
            
        AFTER FIELD vtipo_solic
         IF vtipo_solic IS NULL THEN
            ERROR "Tipo Solicitud  NO puede ser NULO"
            NEXT FIELD vtipo_solic
         ELSE 
            NEXT FIELD generar
         END IF
            
        AFTER FIELD generar
            IF generar NOT MATCHES "[SsNn]" THEN
                ERROR "Opcion solo puede ser S o N"
                NEXT FIELD generar
            ELSE
                IF generar MATCHES "[Nn]" THEN
                    ERROR "PROCESO CANCELADO" SLEEP 2
                    EXIT PROGRAM
                ELSE
                    ERROR "Procesando Informacion... Espere un momento"

                    CALL Traspasa_datos()
                    ERROR "Proceso Terminado "
                END IF
                EXIT INPUT
            END IF

          ON KEY ( INTERRUPT )
             EXIT PROGRAM
    END INPUT

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION Traspasa_datos()

  DEFINE afi              RECORD LIKE afi_solicitud.*
  DEFINE mae              RECORD LIKE afi_mae_afiliado.*
  DEFINE cta              RECORD LIKE cta_ctr_cuenta.*

  DEFINE id_garan         CHAR(1)
  DEFINE ind_modif        CHAR(1)
  DEFINE v_crea_fecha     DATE
  DEFINE HAY              SMALLINT
  DEFINE dias_cotiz       SMALLINT
  DEFINE vtipo_trasp      SMALLINT

  DEFINE v_existe         SMALLINT
  DEFINE v_criterio       SMALLINT
  DEFINE v_tipo_proc      SMALLINT
  DEFINE v_tipo_trasp     SMALLINT
  DEFINE v_medio          SMALLINT
  DEFINE v_edad           SMALLINT
  DEFINE v_rechazo        SMALLINT
  DEFINE vcod_siefore     SMALLINT
  DEFINE v_folioaten      INTEGER
  DEFINE vcuantos         INTEGER      
  
  DEFINE lc_comando       CHAR(4000)   
  DEFINE v_exi_afihis     SMALLINT 
  
  DEFINE v_finicta        VARCHAR(10)  -- CPL-3245
  DEFINE v_largo          SMALLINT     -- CPL-3245
  DEFINE v_act_solicitud  SMALLINT     -- CPL-3245
  DEFINE v_hoy            DATE         -- CPL-3245 

  DEFINE v_query             CHAR(300)
  DEFINE v_tipo_beneficiario INTEGER
  DEFINE v_tramite_ben       INTEGER
  DEFINE v_porcentaje_tot    DECIMAL(5,2)
  DEFINE v_ind_designacion   INTEGER

  DEFINE v_cod_respuesta     CHAR(2)
  DEFINE v_cod_diagnostico   SMALLINT
  DEFINE v_descripcion       VARCHAR(100)
  
  LET v_hoy        = TODAY             -- CPL-3245 
  LET id_garan     = 0
  LET ind_modif    = 0
  LET v_medio      = 10
  LET v_tipo_proc  = 2
  LET v_tipo_trasp = 5

  PREPARE stmt2 FROM exe_sql2

  IF vfecha_liq IS NULL THEN
     ERROR"LA FECHA NO PUEDE SER NULA"
     SLEEP 2
     RETURN     
  END IF

  LET lc_comando = '',
                   '\n SELECT count(*)                            ',
                   '\n   FROM afi_solicitud                       ',                     
                   '\n  WHERE fentcons       = "',vfecha_liq,'" ',
                   '\n    AND tipo_solicitud =  ',vtipo_solic,' ', 	# 1142
                   '\n    AND status_interno = 65               '  #CERT PROC APT CTAS      
  -- DISPLAY lc_comando CLIPPED                 
  PREPARE qrya FROM lc_comando                  
  EXECUTE qrya INTO vcuantos
  
  IF vcuantos = 0 OR vcuantos IS NULL THEN
     ERROR "No existen cuentas por aperturar para los criterios dados"
     SLEEP 4
     EXIT PROGRAM
  ELSE
     ERROR "Cuentas a aperturar.... ", vcuantos USING "####,##&"
     SLEEP 3
  END IF

  LET lc_comando = '',  
                   '\n SELECT A.*,                                ',
                   '\n        " ",                                ',  #vfecha_liq
                   '\n        0  ,                                ',  #tvr.dias_pag_cuo_soc,
                   '\n        0  ,                                ',  #tvr.ident_garantia,
                   '\n        0  ,                                ',  #tvr.ind_nom_mod,
                   '\n        0                                   ',  #tvr.tipo_traspaso                   
                   '\n   FROM afi_solicitud A                     ',
                   '\n  WHERE A.fentcons       = "',vfecha_liq,'" ',
                   '\n    AND A.tipo_solicitud =  ',vtipo_solic,' ',	# 1142
                   '\n    AND A.status_interno = 65               '   # CERT PROC APT CTAS                           
  -- DISPLAY lc_comando CLIPPED                                    
  PREPARE qryb FROM lc_comando                          
  DECLARE cursor_1 CURSOR FOR qryb
  
  FOREACH cursor_1 INTO afi.*, fecha_bnx,  dias_cotiz, id_garan, ind_modif, vtipo_trasp

    LET fecha_bnx = vfecha_liq
    LET HAY       = FALSE
    
    -- CPL-3245
    LET v_act_solicitud = 0
    LET v_finicta = afi.finicta
    LET v_finicta = v_finicta CLIPPED
    LET v_largo = LENGTH(v_finicta) 
    
    IF v_largo = 0 THEN 
       LET afi.finicta     = v_hoy 
       LET v_act_solicitud = 1 
    END IF               
    -- CPL-3245
              
    LET lc_comando = '',              
                     '\n SELECT COUNT(*)                      ',    
                     '\n   FROM afi_mae_afiliado              ',
                     '\n  WHERE n_seguro = "',afi.n_seguro,'" '
    -- DISPLAY lc_comando CLIPPED                 
    PREPARE qryc FROM lc_comando                          
    EXECUTE qryc INTO HAY

    IF HAY > 0 THEN  #Ya Existe AFI_MAE_AFILIADO APERTURADO *REGRESO DE CUENTA

       LET lc_comando = '',              
                        '\n SELECT *                             ',    
                        '\n   FROM afi_mae_afiliado              ',
                        '\n  WHERE n_seguro = "',afi.n_seguro,'" '
       -- DISPLAY lc_comando CLIPPED                 
       PREPARE qryd FROM lc_comando                          
       EXECUTE qryd INTO mae.* 	
    	
       IF SQLCA.SQLCODE = 0 THEN #SE ENCONTRO
       	
          LET lc_comando = '',              
                           '\n SELECT COUNT(*)                                 ',
                           '\n   FROM afi_his_afiliado                         ',
                           '\n  WHERE n_seguro       = "',mae.n_seguro,'"      ',
                           '\n    AND n_folio        =  ',mae.n_folio,'        ',
                           '\n    AND tipo_solicitud =  ',mae.tipo_solicitud,' '
          -- DISPLAY lc_comando CLIPPED                 
                           
           #AND    afi_his_afiliado.finicta        = mae.finicta #CPL-2468 Se comenta ya
                                                                 #se estan encontrando Nulos tanto en afi_mae_afiliado
                                                                 #como en afi_his_afiliado y En esta tabla no acepta Duplis
                                                                 #por NSS ( Los NULLS traen problemas en las busquedas)              
          PREPARE qrye FROM lc_comando                          
          EXECUTE qrye INTO v_exi_afihis	

          IF v_exi_afihis = 0 THEN #NO SE ENCONTRO
             INSERT INTO afi_his_afiliado VALUES (mae.*)
          END IF

          -- CONFIRMAR QUE SE INSERTO 
          EXECUTE qrye INTO v_exi_afihis	
          
          IF v_exi_afihis > 0 THEN 
             LET lc_comando = '',              
                              '\n DELETE FROM afi_mae_afiliado         ',
                              '\n  WHERE n_seguro = "',afi.n_seguro,'" '
             -- DISPLAY lc_comando CLIPPED                 
             PREPARE qryf FROM lc_comando
             EXECUTE qryf                              
          END IF
          
       END IF

       LET lc_comando = '',            
                        '\n SELECT *                           ',
                        '\n   FROM cta_ctr_cuenta cc           ',
                        '\n  WHERE cc.nss = "',afi.n_seguro,'" '
       -- DISPLAY lc_comando CLIPPED                 
       PREPARE qryg FROM lc_comando 
       EXECUTE qryg INTO cta.*
   
       IF cta.nss IS NOT NULL THEN
       	
          INSERT INTO cta_his_cuenta VALUES (cta.*)
          
          LET lc_comando = '',
              '\n  UPDATE cta_ctr_cuenta                           ',
              '\n     SET fecha_pri_rcv      = NULL,               ',
              '\n         fecha_ult_rcv      = NULL,               ',
              '\n         fecha_pri_general  = NULL,               ',
              '\n         fecha_ult_general  = NULL,               ',
              '\n         fecha_vol_pat      = NULL,               ',
              '\n         fecha_vol_ven      = NULL,               ',
              '\n         ind_actividad      = 1,                  ',
              '\n         fecha_actividad    = "',HOY,'",          ',
              '\n         ind_saldo_cero     = 0,                  ',
              '\n         fecha_saldo_cero   = NULL,               ',
              '\n         ind_edad           = 0,                  ',
              '\n         fecha_edad         = "',HOY,'",          ',
              '\n         criterio_edad      = ',v_criterio,',     ',
              '\n         ind_transferencia  = 0,                  ',
              '\n         fecha_ind_transf   = "',HOY,'",          ',
              '\n         estado_impresion   = 0,                  ',
              '\n         periodo_ult_aporte = NULL,               ',
              '\n         dias_cotizados     = ',dias_cotiz,',     ',
              '\n         ult_sal_integrado  = 0,                  ',
              '\n         tipo_informe       = 0,                  ',
--            '\n         fecha_informe      = "',fecha_informe,'" ',
              '\n         fecha_registro     = "',HOY,'",          ',
              '\n         usuario            = "',g_usuario,'"     ',
              '\n   WHERE nss                = "',afi.n_seguro,'"  '
          -- DISPLAY lc_comando CLIPPED                 
          PREPARE qryh FROM lc_comando  
          EXECUTE qryh
             
       END IF
        
       LET lc_comando = '',
                        '\n SELECT mc.marca_cod, mc.correlativo ',
                        '\n   FROM cta_act_marca mc             ',
                        '\n  WHERE mc.nss = "',afi.n_seguro,'"  '                         
       -- DISPLAY lc_comando CLIPPED                 
       PREPARE qryi FROM lc_comando
       DECLARE cur_desmarca CURSOR FOR qryi                 

       FOREACH cur_desmarca INTO vmarca, vcorrelativo
             
         -- CALL desmarca_cuenta ( afi.n_seguro, vmarca, g_usuario,  vcorrelativo)
            
         LET lc_comando = '',
                          '\n EXECUTE PROCEDURE desmarca_cuenta ("',afi.n_seguro,'", ',
                          '\n ',vmarca,', ',vcorrelativo,', 0, 0, "',g_usuario,'")   '
         -- DISPLAY lc_comando CLIPPED                 
         PREPARE eje_desm FROM lc_comando                           
         EXECUTE eje_desm                
         
       END FOREACH

       LET HAY = FALSE
       
    END IF

    LET lc_comando = '',
                     '\n UPDATE taa_cd_det_cedido             ',
                     '\n     SET estado  = 99                 ',
                     '\n  WHERE n_seguro = "',afi.n_seguro,'" ',
                     '\n    AND estado   IN (12, 103)         '
    -- DISPLAY lc_comando CLIPPED                 
    PREPARE qryj FROM lc_comando
    EXECUTE qryj                   

    IF NOT HAY THEN
        IF afi.n_unico IS NOT NULL AND
           afi.n_unico <> "                  " AND
           LENGTH(afi.n_unico) = 18 THEN
            LET afi.status_interno = 200
            LET afi.status_captura = 0
        ELSE
            LET afi.status_interno = 100
            LET afi.status_captura = 0
            LET afi.n_unico = NULL
        END IF

        LET afi.status = NULL
        
        -- CPL-3245 
        IF v_act_solicitud = 1 THEN 
        	
        	 LET afi.finicta = v_hoy 
        	 
       	   LET lc_comando = '',       
                            '\n UPDATE afi_solicitud                             ',
                            '\n    SET finicta        = "',v_hoy,'"              ',
                            '\n  WHERE n_folio        =  ',afi.n_folio,'         ',
                            '\n    AND tipo_solicitud =  ',afi.tipo_solicitud,'  '
           PREPARE act_cuatro FROM lc_comando
           EXECUTE act_cuatro        	         	 
        	 
        END IF 
        -- CPL-3245 
        
        INSERT INTO afi_mae_afiliado VALUES (afi.*)

        INSERT INTO afi_mae_patron    #------- Patrones
        SELECT *
        FROM   afi_patron
        WHERE  n_folio = afi.n_folio
        AND    tipo_solicitud = afi.tipo_solicitud

        WHENEVER ERROR CONTINUE
            INSERT INTO afi_mae_benefici   #------- Beneficiarios
            SELECT *
            FROM   afi_beneficiario
            WHERE  n_folio = afi.n_folio
            AND    tipo_solicitud = afi.tipo_solicitud
            AND    n_seguro = afi.n_seguro
        WHENEVER ERROR STOP

        --Se registra control de designacion para beneficiarios
         LET v_query = "EXECUTE PROCEDURE fn_control_beneficiarios(?,?,?,?,?,?,?)"
         PREPARE exe_control_beneficiarios FROM v_query

         SELECT SUM(porcentaje)
         INTO v_porcentaje_tot
         FROM afi_mae_benefici
         WHERE n_folio = afi.n_folio
         AND tipo_solicitud = afi.tipo_solicitud

         IF v_porcentaje_tot IS NOT NULL AND v_porcentaje_tot > 0 THEN
            LET v_tipo_beneficiario = 2      --Beneficiarios Designados

            LET v_ind_designacion = 1
         ELSE
            LET v_tipo_beneficiario = 5      --SIN Beneficiarios Designados

            LET v_ind_designacion = 0
         END IF

         LET v_tramite_ben = 3      --Traspasos

         DECLARE cur_control_beneficiarios CURSOR FOR exe_control_beneficiarios
         OPEN  cur_control_beneficiarios USING  afi.n_folio,
                                                afi.tipo_solicitud,
                                                v_tipo_beneficiario,
                                                v_tramite_ben,
                                                v_porcentaje_tot,
                                                v_ind_designacion,
                                                g_usuario

         FETCH cur_control_beneficiarios INTO v_cod_respuesta,
                                              v_cod_diagnostico,
                                              v_descripcion
         CLOSE cur_control_beneficiarios
        
        EXECUTE prp_fecha_sol USING afi.n_seguro, v_tipo_trasp 
                              INTO v_crea_fecha                

        OPEN cur_fnacimiento USING afi.n_seguro, v_crea_fecha
        FETCH cur_fnacimiento INTO v_existe, v_edad, v_criterio, v_ind_edad,
                                   v_curp, v_rfc, v_fnacimiento
        CLOSE cur_fnacimiento

        SELECT "X"
        FROM   cta_ctr_cuenta
        WHERE  cta_ctr_cuenta.nss = afi.n_seguro

        IF SQLCA.SQLCODE = 0 THEN
            UPDATE cta_ctr_cuenta
            SET    ind_edad           = v_edad,
                   fecha_edad         = HOY,
                   criterio_edad      = v_criterio
            WHERE  nss                = afi.n_seguro
        ELSE
            INSERT INTO cta_ctr_cuenta   #------ Control cuenta
            VALUES ( afi.n_seguro,       #nss
                     "",                 #fecha_pri_rcv
                     "",                 #fecha_ult_rcv
                     "01/01/0001",       #fecha_pri_general
                     "",                 #fecha_ult_general
                     "",                 #fecha_vol_pat
                     "",                 #fecha_vol_ven
                     0,                  #ind_saldo_cero
                     "",                 #fecha_saldo_cero
                     1,                  #ind_actividad
                     HOY,                #fecha_actividad
                     v_edad,             #ind_edad
                     HOY,                #fecha_edad
                     v_criterio,         #criterio_edad
                     0,                  #ind_transferencia
                     HOY,                #fecha_ind_transf
                     0,                  #estado_impresion
                     "",                 #periodo_ult_aporte
                     dias_cotiz,         #dias_cotizados
                     0,                  #ult_sal_integrado
                     0,                  #tipo_informe
                     "",                 #fecha_informe
                     HOY,                #fecha_registro
                     g_usuario           #usuario
                   )
        END IF

        DECLARE curs2 CURSOR FOR stmt2

        -- CPL-3044
        --
        LET vcod_siefore = 0
        
        SELECT codigo_siefore
          INTO vcod_siefore
          FROM cat_rango_nacimiento
         WHERE id_rango_nacimiento = v_ind_edad
        
        IF SQLCA.SQLCODE = NOTFOUND THEN 
        	 LET vcod_siefore = v_ind_edad 
        END IF 
        --           
        -- CPL-3044    

        OPEN  curs2 USING afi.n_seguro,v_ind_edad,vcod_siefore,
                          v_tipo_proc,v_tipo_trasp,v_medio

        FETCH curs2 INTO v_existe, v_edad, v_rechazo, v_folioaten

        CLOSE curs2
    END IF

    IF id_garan = '1' THEN
        SELECT "X"
        FROM   cta_act_marca
        WHERE  marca_cod = edo_proc
        AND    nss       = afi.n_seguro
        GROUP BY 1

        IF SQLCA.SQLCODE <> 0 THEN
            CALL marca_cuenta (afi.n_seguro, edo_proc, vmarca_estado,
                               vcodigo_rechazo, g_usuario, vfolio)
        END IF
    END IF

    IF id_garan = '2' THEN
        SELECT "X"
        FROM   cta_act_marca
        WHERE  marca_cod = edo_acr
        AND    nss       = afi.n_seguro
        GROUP BY 1

        IF SQLCA.SQLCODE <> 0 THEN
            CALL marca_cuenta (afi.n_seguro, edo_acr, vmarca_estado,
                               vcodigo_rechazo, g_usuario, vfolio)
        END IF
    END IF

    LET id_garan = '0'

    IF ind_modif = '1' THEN
        SELECT "X"
        FROM   cta_act_marca
        WHERE  marca_cod = edo_mod
        AND    nss       = afi.n_seguro
        GROUP BY 1

        IF SQLCA.SQLCODE <> 0 THEN
            CALL marca_cuenta (afi.n_seguro, edo_mod, vmarca_estado,
                               vcodigo_rechazo, g_usuario, vfolio)
        END IF
        LET ind_modif = NULL
    END IF
   
---SACTUALIZA afi_solicitud  1038   Apertura de Cuentas de Registro 
    IF  afi.tipo_solicitud = 1           -- REGISTRO PURO
    OR  afi.tipo_solicitud = 6           -- REGISTRO POR SEPARACION 
    OR  afi.tipo_solicitud = 11          -- REGISTRO INTERNET 
    OR  afi.tipo_solicitud = 12  THEN    -- REGISTRO INTERNET NO AFILIADOS 
    	
    	 #CPL-2372        	 
    	 #Se Comenta los Updates de finicta tanto para afi_solicitud como
    	 #afi_mae_afiliado.

       -- CPL-3245    	 
    	 LET lc_comando = '',        
                        '\n UPDATE afi_solicitud                            ',
                        '\n    SET status_interno = 100,                    ',  #CPL-1875 antes quedaba en 70 ahora pasa a 100
                        '\n        usuario        = "',g_usuario CLIPPED,'" ',
                        '\n  WHERE n_seguro       = "',afi.n_seguro,'"      ',
                        '\n    AND n_folio        =  ',afi.n_folio,'        ',
                        '\n    AND tipo_solicitud =  ',vtipo_solic,'        ',
                        '\n    AND status_interno =  65                     ',
                        '\n    AND fentcons       = "',vfecha_liq,'"        '
       PREPARE act_cero FROM lc_comando
       EXECUTE act_cero                 
      
       IF v_act_solicitud = 1 THEN  
       	  LET lc_comando = '',       
                           '\n UPDATE afi_mae_afiliado                    ',
                           '\n    SET finicta        = "',v_hoy,'"        ',
                           '\n  WHERE n_seguro       = "',afi.n_seguro,'" ',
                           '\n    AND n_folio        =  ',afi.n_folio,'   ',
                           '\n    AND tipo_solicitud =  ',vtipo_solic,'   ',
                           '\n    AND status_interno =  65                ',
                           '\n    AND fentcons       = "',vfecha_liq,'"   '
          PREPARE act_uno FROM lc_comando
          EXECUTE act_uno                 
       END IF       
       -- CPL-3245 
        
    ELSE
    
       --CPL-3245  
       LET lc_comando = '',       
                        '\n UPDATE afi_solicitud                            ',
                        '\n    SET status_interno = 100,                    ', #CPL-1875 antes quedaba en 70 ahora pasa a 100
                        '\n        usuario        = "',g_usuario CLIPPED,'" ',
                        '\n  WHERE n_seguro       = "',afi.n_seguro,'"      ',
                        '\n    AND n_folio        =  ',afi.n_folio,'        ',
                        '\n    AND tipo_solicitud =  ',vtipo_solic,'        ',
                        '\n    AND status_interno =  65                     ',
                        '\n    AND fentcons       = "',vfecha_liq,'"        '
       PREPARE act_dos FROM lc_comando
       EXECUTE act_dos                  
       
       IF v_act_solicitud = 1 THEN   
       	  LET lc_comando = '',       
                           '\n UPDATE afi_solicitud                       ',
                           '\n    SET finicta        = "',v_hoy,'"        ',
                           '\n  WHERE n_seguro       = "',afi.n_seguro,'" ',
                           '\n    AND n_folio        =  ',afi.n_folio,'   ',
                           '\n    AND tipo_solicitud =  ',vtipo_solic,'   ',
                           '\n    AND status_interno =  65                ',
                           '\n    AND fentcons       = "',vfecha_liq,'"   '
          PREPARE act_tres FROM lc_comando
          EXECUTE act_tres                                            
       END IF       
       --CPL-3245  
       
    END IF
    
    LET    vctas_apert    =  vctas_apert + 1                      #1038
       
--77->
--   Traspaso por separacion de cuentas    <---
    IF  afi.tipo_solicitud = 6  
    OR  afi.tipo_solicitud = 7 THEN
      SELECT "OK"
      FROM sep_det_reg_sol_reclamante a,
           sep_det_solicitud b
      WHERE a.nss = afi.n_seguro
      AND a.estado = 51
      AND a.correlativo = b.idSolicitudSeparacion
      AND b.clasifica_separacion = "B"
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
        UPDATE sep_det_reg_sol_reclamante
        SET estado = 52 -- asociado registrado
        WHERE nss = afi.n_seguro
        AND estado = 51
       ELSE
        UPDATE sep_det_reg_sol_reclamante
        SET estado = 53 -- asociado registrado
        WHERE nss = afi.n_seguro
        AND estado = 51
      END IF
-- 7 ---->
      LET v_corr = 0

      SELECT a.correlativo
       INTO v_corr
      FROM sep_det_reg_sol_reclamante a
      WHERE a.nss = afi.n_seguro
       AND a.estado in (52,53)

      IF v_corr IS NULL THEN
        LET v_corr = 0
      END IF

      LET pmarca_entra = 280
      LET pestado_marca   = 0
      LET pcodigo_rechazo = 0
      LET pmarca_causa = 280
      LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
      "'",afi.n_seguro,"'",
      ",",pmarca_entra,
      ",",v_corr,
      ",",pestado_marca,
      ",",pcodigo_rechazo,
      ",",pmarca_causa,
      ",","'","'", ",",
      "'",g_usuario,"'",")"

      LET ejecuta = ejecuta CLIPPED

      PREPARE clausula_spl_280 FROM ejecuta

      DECLARE cursor_marca_280 CURSOR FOR clausula_spl_280

      OPEN cursor_marca_280

      FETCH cursor_marca_280 INTO xcodigo_marca, xcodigo_rechazo

      CLOSE cursor_marca_280
----50 <----
      END IF
---6<---
      
  END FOREACH
    
  CALL archivo()

END FUNCTION       #Traspasa_datos()

-------------------------------------------------------------------------------

FUNCTION marca_cuenta(vnss, vmarca_entra, vmarca_edo, vcodigo_rech,
                      vusuario, vcorrelativo)

  DEFINE
      vnss         CHAR(11),
      vmarca_entra SMALLINT,
      vmarca_edo   SMALLINT,
      vcodigo_rech SMALLINT,
      vusuario     CHAR(08),
      vcorrelativo INTEGER,
      vmarca_causa SMALLINT,
      vfecha_causa DATE
  
  LET vmarca_causa = 0
  LET vfecha_causa = ""
  
  LET pmarca_entra = vmarca_entra
  LET pestado_marca   = vmarca_edo
  LET pcodigo_rechazo = vcodigo_rech
  LET pmarca_causa = 230
  LET v_corr       = vcorrelativo
  LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
  "'",vnss,"'",
  ",",pmarca_entra,
  ",",v_corr,
  ",",pestado_marca,
  ",",pcodigo_rechazo,
  ",",pmarca_causa,
  ",","'","'", ",",
  "'",vusuario,"'",")"
  
  LET ejecuta = ejecuta CLIPPED
  
  PREPARE clausula_spl_230 FROM ejecuta
  
  DECLARE cursor_marca_230 CURSOR FOR clausula_spl_230
  
  OPEN cursor_marca_230
  
  FETCH cursor_marca_230 INTO xcodigo_marca, xcodigo_rechazo
  
  CLOSE cursor_marca_230

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION desmarca_cuenta (vnss, vmarca, vusuario, vcorrelativo)

  DEFINE
      vnss           CHAR(11),
      vmarca         SMALLINT,
      vusuario       CHAR(08),
      vcorrelativo   INTEGER,
      vestado_marca  SMALLINT,
      vmarca_causa   SMALLINT
  
  LET vestado_marca = 0
  LET vmarca_causa  = 0
  
  PREPARE eje_desmarca FROM v_desmarca
  
  EXECUTE eje_desmarca
  USING vnss,
        vmarca,
        vcorrelativo,
        vestado_marca,
        vmarca_causa,
        vusuario

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION archivo()

  DEFINE lar_procesadas ARRAY[100] OF RECORD
  	  tipo_traspaso SMALLINT,
  	  desc_trasp    CHAR(30),
  	  aceptadas     INTEGER,
  	  rechazadas    INTEGER
  END RECORD

  DEFINE ls_cont   ,
         ls_c2     ,                                #JCPV
         ls_tot    SMALLINT

   FOR ls_c2 = 1 TO 100
      LET lar_procesadas[ls_c2].tipo_traspaso   = 0
      LET lar_procesadas[ls_c2].desc_trasp      = " "
      LET lar_procesadas[ls_c2].aceptadas       = 0
      LET lar_procesadas[ls_c2].rechazadas      = 0
   END FOR

  DECLARE cur_aceptadas CURSOR FOR
  SELECT 1,                           #tvr.tipo_traspaso
         c.descripcion,
         count(A.n_seguro)
  FROM   afi_mae_afiliado           A,
         afi_solicitud              B,
         tab_tipo_traspaso          c
  WHERE  A.fentcons        = vfecha_liq
  AND 	 A.tipo_solicitud	 = vtipo_solic
  AND		 A.tipo_solicitud  = B.tipo_solicitud
  AND    A.status_interno >= 100  
  AND    A.n_seguro        = B.n_seguro
  AND    B.status_interno  = 100 #CPL-1875 antes quedaba en 70 ahora pasa a 100               
  AND    1                 = c.tipo_traspaso
  AND    c.id_opera        = '09'
  GROUP BY 1,2
  ORDER BY 1


  LET ctipo_solic = vtipo_solic
  LET gc_reporte = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                   ".RPT_APT_CTAS_CERT." CLIPPED, ctipo_solic CLIPPED,
                   vfecha_liq USING "DD-MM-YYYY"

  LET ls_cont = 1

  FOREACH cur_aceptadas INTO lar_procesadas[ls_cont].tipo_traspaso,
  	                          lar_procesadas[ls_cont].desc_trasp   ,
  	                          lar_procesadas[ls_cont].aceptadas


  	  LET lar_procesadas[ls_cont].rechazadas = 0

  	  LET ls_cont = ls_cont + 1
  END FOREACH

  IF  ls_cont > 1  THEN                                          #JCPV
     LET ls_cont = ls_cont - 1                                   #JCPV
  END IF                                                         #JCPV

  START REPORT rpt_cifras TO gc_reporte

  FOR ls_tot = 1 TO ls_cont
         LET cont_ctas     =  cont_ctas                         #JCPV
                           +  lar_procesadas[ls_tot].aceptadas #JCPV
  	  OUTPUT TO REPORT rpt_cifras(lar_procesadas[ls_tot].tipo_traspaso,
  	                              lar_procesadas[ls_tot].desc_trasp   ,
  	                              lar_procesadas[ls_tot].aceptadas    ,
  	                              2
  	                              )
  END FOR

  IF  ls_c2 > 2  THEN                                          #JCPV
     LET ls_c2   = ls_c2   - 1                                   #JCPV
  END IF                                                         #JCPV

  FOR ls_tot = ls_cont + 1 TO ls_c2                                #JCPV 
         LET gs_rechazos   =  gs_rechazos                          #JCPV
                           +  lar_procesadas[ls_tot].rechazadas   #JCPV 

  	  OUTPUT TO REPORT rpt_cifras(lar_procesadas[ls_tot].tipo_traspaso,
  	                              lar_procesadas[ls_tot].desc_trasp   ,
  	                              lar_procesadas[ls_tot].rechazadas   ,
  	                              3
  	                              )
  END FOR

  FINISH REPORT rpt_cifras

END FUNCTION

-------------------------------------------------------------------------------

REPORT rpt_cifras(lr_reporte)

   DEFINE lr_reporte RECORD
   	  tipo_traspaso SMALLINT,
   	  desc_trasp    CHAR(30),
   	  total         INTEGER ,
   	  tipo          SMALLINT
   END RECORD

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   25

      ORDER EXTERNAL BY lr_reporte.tipo

   FORMAT
   PAGE HEADER
      PRINT COLUMN 001, "                 SUBE AFILIADOS POR TRASPASO AL MAESTRO"
      PRINT COLUMN 001, "PROGRAMA: TAAB010A                       FECHA GENERACION:", hoy USING "DD/MM/YYYY"
      PRINT COLUMN 001, "                                         FOLIO: ", vfolio USING "<<<<<<<<<<<<<<<<<<<<<"

   BEFORE GROUP OF lr_reporte.tipo
      CASE lr_reporte.tipo
   	     WHEN 2
   	  	    SKIP 1 LINE
   	  	    PRINT COLUMN 001, "                       CUENTAS APERTURADAS"
   	  	    PRINT COLUMN 001, "ORIGEN   DETALLE                           REGISTROS"

   	  	 WHEN 3
   	  	    SKIP 1 LINE
   	  	    PRINT COLUMN 001, "                       CUENTAS NO APERTURADAS"
   	  	    PRINT COLUMN 001, "ORIGEN   DETALLE                           REGISTROS"
      END CASE

   ON EVERY ROW
      PRINT COLUMN 003, lr_reporte.tipo_traspaso USING "&&",
            COLUMN 010, lr_reporte.desc_trasp              ,
            COLUMN 042, lr_reporte.total         USING "##########&"

   ON LAST ROW
      PRINT COLUMN 001, "TOTAL CUENTAS APERTURADAS.... ", cont_ctas   USING "##########&"
      PRINT COLUMN 001, "TOTAL CUENTAS NO APERTURADAS. ", gs_rechazos USING "##########&"
      PRINT COLUMN 001, "TOTAL GENERAL................ ", cont_ctas + gs_rechazos USING "##########&"

END REPORT

-------------------------------------------------------------------------------

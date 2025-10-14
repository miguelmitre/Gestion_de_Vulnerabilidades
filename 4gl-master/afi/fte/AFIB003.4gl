#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIB003  => TRASPASA AFILIADOS APROBADOS POR AL MAESTRO           #
#Sistema           => AFI.                                                  #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 17 DE ENERO DE 2001                                   #
#Autor             => MAURO MUNIZ CABALLERO (Proceso batch)                 #
#Modificado        => ISABEL FONSECA                                        #
#Fecha             => 24 DE NOVIEMBRE DE 2004                               #
#Modifico          => EDUARDO RESENDIZ MEDINA                               #
#fECHA             => 07  DE DICIEMBRE DE 2004                              #
#Ultima version    => 15 DE MARZO DEL 2005 -> FERNANDO HERRERA HERNANDEZ    #
#Actualizacion     => EDUARDO RESENDIZ MENDINA 14 MARZO 2006 listado_3      #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ 08 MARZO 2008 (MULTISIEF)  #
#############################################################################

DATABASE safre_af

GLOBALS

  DEFINE enter            CHAR(1),
         generar          CHAR(1),
         aux_pausa        CHAR(1),
         opc              CHAR(1),
         g_usuario        CHAR(8),
         HORA             CHAR(8),
         vnss             CHAR(11),
         operacion        CHAR(40),
         v_sql_1          CHAR(50),
         v_sql_2          CHAR(50),
         HOY              DATE,
         f_ini_tmte       DATE,
         v_query               CHAR(300)

  DEFINE w_aux            RECORD
                            n_seguro        LIKE afi_solicitud.n_seguro,
                            n_unico         LIKE afi_solicitud.n_unico ,
                            n_rfc           LIKE afi_solicitud.n_rfc   ,
                            paterno         LIKE afi_solicitud.paterno ,
                            materno         LIKE afi_solicitud.materno ,
                            nombres         LIKE afi_solicitud.nombres ,
                            fena            LIKE afi_solicitud.fena    ,
                            sexo            LIKE afi_solicitud.sexo    ,
                            frecafor        LIKE afi_solicitud.frecafor,
                            status_interno  SMALLINT
                          END RECORD

  DEFINE g_afore          RECORD LIKE tab_afore_local.*
  DEFINE g_paramgrales    RECORD LIKE seg_modulo.*
  DEFINE g_aficefa        RECORD LIKE afi_icefa.*
  DEFINE gr_ctanssreg     RECORD LIKE cta_nss_regimen.*
                          
  DEFINE reg_bat          RECORD
                            pid             INTEGER,
                            proceso_cod     INTEGER,
                            opera_cod       INTEGER,
                            nombre_archivo  CHAR(25)
                          END RECORD

  DEFINE bnd_proceso      SMALLINT ,
         pestado_marca    SMALLINT ,
         pcodigo_rechazo  SMALLINT ,
         ejecuta          CHAR(300),
         xcodigo_marca    SMALLINT ,
         xcodigo_rechazo  SMALLINT ,
         pmarca_entra     SMALLINT ,
         con_curp         SMALLINT , 
         sin_curp         SMALLINT ,
         pmarca_causa     SMALLINT ,
         pfecha_causa     SMALLINT ,
         xmarca_estado    SMALLINT ,
         edo_proc         SMALLINT

  DEFINE consulta_carta   CHAR(120)
  
  -- CPL-3165
  DEFINE lv_tipo_solic    VARCHAR(20)
  DEFINE lc_comando       CHAR(2000) 
  DEFINE ls_menores       SMALLINT 
  -- CPL-3165
  
  DEFINE reg_carta        RECORD LIKE int_ctr_carta.*
  DEFINE afi              RECORD LIKE afi_solicitud.*
                          
  DEFINE G_IMPRE3         CHAR(200)
  DEFINE G_NOMBRE         CHAR(200)
  DEFINE gimpresion3      CHAR(200)
  
  DEFINE v_desmarca       CHAR(300),
         vmarca           SMALLINT,
         vcorrelativo     INTEGER

END GLOBALS

-------------------------------------------------------------------------------

MAIN

  DEFINE lc_opcion     CHAR(1)     -- CPL-3165
  DEFINE lc_opcion2    CHAR(2)     -- CPL-3165

  -- DISPLAY " "
  -- DISPLAY ".1"
  
  -- ASIGNAR POR DEFAUL UNA OPCION DEL MENU
  LET lc_opcion  = '3'          --CPL-3165
  LET ls_menores = 0            --CPL-3165

  CALL STARTLOG("AFIB003.log")

  CALL inicio()              #i

  IF NOT bnd_proceso THEN
  	
     DEFER INTERRUPT
     OPTIONS INPUT WRAP,
         PROMPT LINE LAST,
         ACCEPT KEY CONTROL-I


     --------------------------
     --CPL-3165           
     CALL pantalla_previa()
          RETURNING lc_opcion, lc_opcion2 
          
     -- DISPLAY "OPCION ELEGIDA ",lc_opcion     
          
     CASE     
       WHEN lc_opcion = "1" -- REGISTRO  
       
            LET lv_tipo_solic = "(1)"            
            LET ls_menores    = 0 
            CALL proceso_principal(lv_tipo_solic)
            
            PROMPT "Proceso finalizado, [Enter] para salir"
            FOR enter
            
       WHEN lc_opcion = "2" -- ACTIVACION DE MENORES DE EDAD
 
            CASE
              WHEN lc_opcion2 = "1"
                   LET lv_tipo_solic = "(33)"            
              WHEN lc_opcion2 = "2"
                   LET lv_tipo_solic = "(34)"            
              WHEN lc_opcion2 = "3"
                   LET lv_tipo_solic = "(32)"            
            END CASE 
            
            LET ls_menores    = 1 
            CALL proceso_principal(lv_tipo_solic)

            PROMPT "Proceso finalizado, [Enter] para terminar"
            FOR enter

       WHEN lc_opcion = "3" -- SALIR 

       
     END CASE     
     
  ELSE
     
      LET lc_opcion     = "1"
      LET lv_tipo_solic = "(1)"
      LET ls_menores    = 0 
      
      CALL traspasa_datos(lv_tipo_solic)   -- CPL-3165 
      CALL actualiza_bat_f(0) 
            
      -- DISPLAY "Proceso finalizado"
      
  END IF
        
  IF lc_opcion = "1" OR lc_opcion = "2" THEN
    CALL despliega_resultados()
  END IF   

END MAIN

-------------------------------------------------------------------------------

FUNCTION inicio()

  LET reg_bat.pid            = ARG_VAL(1)
  LET reg_bat.proceso_cod    = ARG_VAL(2)
  LET reg_bat.opera_cod      = ARG_VAL(3)

  LET bnd_proceso = 0

  IF reg_bat.pid THEN
      -- DISPLAY "INICIANDO PROCESO ..."
      LET bnd_proceso = 1
  END IF

  WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE safre_tmp:afi_2
  WHENEVER ERROR STOP

  CREATE TABLE safre_tmp:afi_2
  (n_seguro        CHAR(11) ,
   n_unico         CHAR(18) ,
   n_rfc           CHAR(13) ,
   paterno         CHAR(40) ,
   materno         CHAR(40) ,
   nombres         CHAR(40) ,
   fena            DATE     ,
   sexo            SMALLINT ,
   frecafor        DATE     ,
   status_interno  SMALLINT);

  DATABASE safre_af

  SELECT *
    INTO g_paramgrales.*
    FROM seg_modulo 
   WHERE modulo_cod = 'afi'

  SELECT *, USER
    INTO g_afore.*, g_usuario
    FROM tab_afore_local

  LET HOY  = TODAY
  LET HORA = TIME

  LET pmarca_entra    = 605
  LET pestado_marca   = 0
  LET pcodigo_rechazo = 0
  LET pmarca_causa    = 0
  LET pfecha_causa    = ""

  LET operacion = 'ALTA EN MAESTRO DE AFILIADOS'

  INITIALIZE reg_carta.*    TO NULL  
  INITIALIZE gr_ctanssreg.* TO NULL

  LET v_sql_1    = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
  LET v_sql_2    = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"  -- no aplica a activacion de menores
  LET v_desmarca = "EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "

  PREPARE stmt1 FROM v_sql_1
  PREPARE stmt2 FROM v_sql_2

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION pantalla_previa()

  DEFINE sigue      SMALLINT
  DEFINE sigue2     SMALLINT
  
  DEFINE confirma   CHAR(1)
  DEFINE confirma2  CHAR(1)

  LET confirma  = ""
  LET confirma2 = "" 
  
  OPEN WINDOW v1 AT 2,2 WITH 21 ROWS , 78 COLUMNS ATTRIBUTE(BORDER)

    DISPLAY " AFIB003          TRASPASA AFILIADOS APROBADOS AL MAESTRO                      " AT 3, 1 ATTRIBUTE(REVERSE)
    DISPLAY "                                < Ctrl-C > Salir                               " AT 1, 1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy"                                                             AT 3,63 ATTRIBUTE(REVERSE)
    
    LET   sigue = 1
    WHILE sigue = 1
  
      DISPLAY "OPCIONES DE TIPO DE APERTURA A REALIZAR " AT  7,5
      DISPLAY "                                        " AT  8,5
      DISPLAY "1.  REGISTRO                            " AT  9,5
      DISPLAY "2.  ACTIVACION DE MENORES DE EDAD       " AT 10,5
      DISPLAY "                                        " AT 11,5
      DISPLAY "3.  SALIR                               " AT 12,5 
      DISPLAY "                                        " AT 13,5
      DISPLAY "                                        " AT 14,5
    
      -- Elegir una de las 3 opciones
      PROMPT "     OPCION [1|2|3]: " FOR confirma
    
      IF (confirma MATCHES "[123]") THEN
      	
         LET sigue = 0 
         
         IF confirma = "2" THEN 
         
            LET confirma2 = ""
            
            DISPLAY "OPCIONES ACTIVACION DE MENORES DE EDAD  " AT  7,5
            DISPLAY "                                        " AT  8,5
            DISPLAY "1.  IMSS (33)                           " AT  9,5
            DISPLAY "2.  ISSSTE (34)                         " AT 10,5
            DISPLAY "3.  INDEPENDIENTE (32)                  " AT 11,5 
            DISPLAY "                                        " AT 12,5
            DISPLAY "4.  REGRESAR                            " AT 13,5 
            DISPLAY "                                        " AT 14,5
            
            LET   sigue2 = 1
            WHILE sigue2 = 1
            
              -- Elegir una de las 3 opciones
              PROMPT "     OPCION [1|2|3|4]: " FOR confirma2
            
              IF (confirma2 MATCHES "[1234]") THEN
                 LET sigue2 = 0 
              ELSE
                 ERROR "ELEGIR UNA DE LAS 4 OPCIONES"
                 LET sigue2 = 1
              END IF # confirma N
               
            END WHILE
         
         END IF 

      ELSE
         ERROR "ELEGIR UNA DE LAS 3 OPCIONES"
         LET sigue = 1
      END IF # confirma N
      
      IF confirma2 = "4" THEN 
      	 LET sigue     = 1
      	 LET confirma2 = ""      	 
      END IF  	 
       
    END WHILE
        
  CLOSE WINDOW v1

  RETURN confirma, confirma2
    
END FUNCTION 

-------------------------------------------------------------------------------

FUNCTION proceso_principal(pv_tipo_solic)

  DEFINE pv_tipo_solic VARCHAR(20)  

  OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0031" ATTRIBUTE(BORDER)
  DISPLAY " AFIB003          TRASPASA AFILIADOS APROBADOS AL MAESTRO                      " AT 3,1 ATTRIBUTE(REVERSE)

  DISPLAY "                                < Ctrl-C > Salir                               " AT 1,2 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

  INPUT BY NAME generar
  AFTER FIELD generar
      IF generar NOT MATCHES "[SsNn]" THEN
          ERROR "Opcion solo puede ser [S / N]"
      ELSE
          IF generar MATCHES "[Nn]" THEN
              ERROR "PROCESO CANCELADO"
              SLEEP 2
              EXIT INPUT
          ELSE
              ERROR "Procesando Informacion... Espere un momento"
              CALL traspasa_datos(pv_tipo_solic)  -- CPL-3165
          END IF

          EXIT INPUT
      END IF

      ON KEY ( INTERRUPT )
          EXIT INPUT

  END INPUT

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION traspasa_datos(pv_tipo_solic)     

  DEFINE pv_tipo_solic   VARCHAR(20)  -- CPL-3165

  DEFINE pat RECORD      LIKE afi_patron.* ,
         ben RECORD      LIKE afi_beneficiario.*,
         mae RECORD      LIKE afi_mae_afiliado.*,
         cta RECORD      LIKE cta_ctr_cuenta.*
                         
  DEFINE mensaje         CHAR(050),
         G_LISTA         CHAR(300),
         G_IMPRE         CHAR(300)

  DEFINE i               SMALLINT,
         cont            SMALLINT,
         HAY             SMALLINT,
         v_porc          SMALLINT,
         v_sql_1         CHAR(50),
         v_existe        SMALLINT,        
         v_edad          SMALLINT,        
         v_criterio      SMALLINT,        
         v_ind_edad      SMALLINT,        
         v_crea_fecha    DATE    ,        
         v_tipo_proc     SMALLINT,
         v_tipo_trasp    SMALLINT,
         v_medio         SMALLINT,
         v_cve_siefore   SMALLINT,
         v_cve_sief_i    SMALLINT,
         v_cve_sief_f    SMALLINT,
         v_rechazo       SMALLINT,
         v_folioatencion INTEGER

  DEFINE v_curp          CHAR(18),
         v_rfc           CHAR(13),
         v_fena          DATE    

  DEFINE regrowid        RECORD 
                           v_rowid   DECIMAL(10,0)
                         END RECORD  
  	
  DEFINE v_afisolreg     RECORD
                           nss   LIKE afi_solicitud_regimen.nss,
                           fol   LIKE afi_solicitud_regimen.n_folio,
                           ts    LIKE afi_solicitud_regimen.tipo_solicitud,
                           edo   LIKE afi_solicitud_regimen.estado
                         END RECORD

  DEFINE v_ctareg        RECORD LIKE cta_regimen.*
  DEFINE v_siefore       SMALLINT  -- CPL-3044     
  DEFINE v_reg_act_men   RECORD LIKE afi_act_menor_edad.*    -- CPL-3165
  DEFINE v_curp_var      VARCHAR(18)
  DEFINE v_longitud      SMALLINT

   DEFINE v_tipo_beneficiario INTEGER
   DEFINE v_tramite_ben       INTEGER
   DEFINE v_porcentaje_tot    DECIMAL(6,2)
   DEFINE v_ind_designacion   INTEGER

   DEFINE v_cod_respuesta     CHAR(2)
   DEFINE v_cod_diagnostico   SMALLINT
   DEFINE v_descripcion       VARCHAR(100)
	
  LET mensaje = "NO SE HICIERON TRASPASOS AL MAESTRO DE AFILIADOS"

  LET v_tipo_trasp = 5
  LET v_tipo_proc  = 1
  LET v_medio      = 10
  LET con_curp     = 0     --CPL-3165
  LET sin_curp     = 0     --CPL-3165

  LET lc_comando = '',
                   '\n SELECT rowid, A.*                            ',
                   '\n   FROM afi_solicitud A                       ',
                   '\n  WHERE A.status_interno = 60                 ',
                   '\n    AND A.tipo_solicitud IN ',pv_tipo_solic,' '
  -- DISPLAY lc_comando CLIPPED 
  -- EXIT PROGRAM 
                     
  PREPARE con_afi FROM lc_comando
  DECLARE cursor_1 CURSOR FOR con_afi 
  FOREACH cursor_1 INTO  regrowid.v_rowid, afi.*

    LET v_curp_var = afi.n_unico CLIPPED    --CPL-3165
    LET v_longitud = LENGTH(v_curp_var)     --CPL-3165     
    
    -- DISPLAY '>',afi.n_seguro,'<   >',afi.n_unico,'<'
    -- IF (afi.n_unico IS NULL OR afi.n_unico = '                  ') THEN
    
    IF v_longitud = 0 THEN 
       LET sin_curp = sin_curp + 1
    ELSE
       LET con_curp = con_curp + 1
    END IF
    
    LET HAY = FALSE

    SELECT COUNT(*)
      INTO HAY
      FROM afi_mae_afiliado m
     WHERE m.n_seguro = afi.n_seguro
     
    -- display "HAY ", hay
    
    IF HAY THEN
    	
       SELECT *
         INTO mae.*
         FROM afi_mae_afiliado ma
        WHERE ma.n_seguro = afi.n_seguro

       IF SQLCA.SQLCODE = 0 THEN
       	
          INSERT INTO afi_his_afiliado VALUES (mae.*)

          IF SQLCA.SQLCODE = 0 THEN
             DELETE
               FROM afi_mae_afiliado
              WHERE n_seguro = afi.n_seguro
          END IF

          SELECT b.*
            INTO cta.*
            FROM cta_ctr_cuenta b
           WHERE b.nss = afi.n_seguro

          IF SQLCA.SQLCODE = 0 THEN
             SELECT "X"
               FROM cta_ctr_cuenta
              WHERE cta_ctr_cuenta.nss = afi.n_seguro
              
             IF SQLCA.SQLCODE = 0 THEN
                INSERT INTO cta_his_cuenta VALUES (cta.*)
             END IF

          END IF

          IF mae.tipo_solicitud = 5 THEN
             LET afi.finitmte = mae.fentcons
             LET afi.fentcons = afi.fentcons

             UPDATE afi_det_asignado
                SET fecha_afiliacion = afi.fentcons,
                    estado_asignado  = 100
              WHERE n_seguro = mae.n_seguro
                AND n_folio  = mae.n_folio
                AND tipo_solicitud = mae.tipo_solicitud
          END IF

          LET HAY = FALSE
          
       END IF

       UPDATE safre_af:taa_cd_det_cedido
          SET estado   = 99
        WHERE n_seguro = afi.n_seguro
          AND estado  IN (12, 103)
          
    END IF


    LET afi.status_interno = 100
    LET afi.status_captura = 100

    IF NOT HAY THEN
    	
       IF afi.n_unico IS NOT NULL AND
          afi.n_unico <> "                  " AND
          LENGTH(afi.n_unico) = 18 THEN
          LET afi.status_interno = 200
          LET afi.status_captura = 0
       ELSE
          LET afi.status_interno = 100
          LET afi.status_captura = 0
       END IF

       LET afi.status = NULL 
       
       -----------------------------------
       ----- ACTIVACION DE MENORES -------
       -----------------------------------
       -- CPL-3165
       IF (afi.tipo_solicitud = 32) OR (afi.tipo_solicitud = 33) OR 
          (afi.tipo_solicitud = 34) THEN   
          	                           
          LET lc_comando = '',
                           '\n  SELECT FIRST 1 *                  ',
                           '\n    FROM afi_act_menor_edad	        ',
                           '\n   WHERE curp = "',afi.n_unico,'"   ',
                           '\n   ORDER BY id_act_menor_edad  DESC ' 
                           
          PREPARE act_sel FROM lc_comando 
          EXECUTE act_sel INTO v_reg_act_men.*
          
          IF SQLCA.SQLCODE = NOTFOUND THEN
          	 -- NO MODIFICAR LOS DATOS 
          ELSE 
             -- Fecha que se recibio en el Id 8 Fecha de transferencia de lote
             -- del archivo de activacion de menores            
             LET afi.finicta = v_reg_act_men.fecha_transf_lote  
          END IF  	
          	          	
       END IF    	
       -- CPL-3165
       -----------------------------------
       ----- ACTIVACION DE MENORES -------
       -----------------------------------       
       
       -- display "interno", afi.status_interno
       
       INSERT INTO afi_mae_afiliado VALUES(afi.*)

       IF SQLCA.SQLCODE <> 0 THEN
          INSERT INTO safre_tmp:nss_dup VALUES (afi.n_seguro)
       END IF

       INSERT INTO afi_mae_patron      -------- Patrones
       SELECT *
         FROM afi_patron
        WHERE n_folio = afi.n_folio
          AND tipo_solicitud = afi.tipo_solicitud

       INSERT INTO afi_mae_benefici    -------- Beneficiarios
       SELECT *
         FROM afi_beneficiario
        WHERE n_seguro = afi.n_seguro
          AND n_folio = afi.n_folio

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

      LET v_tramite_ben = 1      --Registro

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

       SELECT "X"
         FROM cta_ctr_cuenta
        WHERE cta_ctr_cuenta.nss = afi.n_seguro

       IF SQLCA.SQLCODE = 0 THEN
          UPDATE cta_ctr_cuenta
             SET fecha_pri_rcv      = NULL,
                 fecha_ult_rcv      = NULL,
                 fecha_pri_general  = NULL,
                 fecha_ult_general  = NULL,
                 fecha_vol_pat      = NULL,
                 fecha_vol_ven      = NULL,
                 ind_actividad      = 1,
                 fecha_actividad    = HOY,
                 ind_edad           = 0,
                 fecha_edad         = HOY,
                 criterio_edad      = 0,
                 ind_transferencia  = 0,
                 fecha_ind_transf   = HOY,
                 ind_saldo_cero     = 0,
                 fecha_saldo_cero   = NULL,
                 estado_impresion   = 0,
                 periodo_ult_aporte = NULL,
                 dias_cotizados     = 0,
                 ult_sal_integrado  = 0,
                 tipo_informe       = 0,
                 fecha_informe      = NULL,
                 fecha_registro     = HOY,
                 usuario            = g_usuario
           WHERE nss                = afi.n_seguro
       ELSE
           INSERT INTO cta_ctr_cuenta     #------ Control cuenta
           VALUES (afi.n_seguro,       #nss
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
                   0,                  #ind_edad
                   HOY,                #fecha_edad
                   0,                  #criterio_edad
                   0,                  #ind_transferencia
                   HOY,                #fecha_ind_transf
                   0,                  #estado_impresion,
                   "",                 #periodo_ult_aporte
                   0,                  #dias_cotizados
                   0,                  #ult_sal_integrado
                   0,                  #tipo_informe
                   "",                 #fecha_informe
                   HOY,                #fecha_registro
                   g_usuario           #usuario
                   )
       END IF

       -->CPL-1134
       LET vmarca       = 0
       LET vcorrelativo = 0
       
       DECLARE cur_desmarca CURSOR FOR
        SELECT mc.marca_cod, mc.correlativo
          FROM cta_act_marca mc
         WHERE mc.nss = afi.n_seguro
         
       FOREACH cur_desmarca INTO vmarca, vcorrelativo
         # CPL-3460 Se agrega la marca 120 Inhabilitaci�n por traspaso Afore-Afore para desmarca
         IF vmarca = 150 OR vmarca = 151 OR vmarca = 120 THEN 
            CALL desmarca_cuenta (afi.n_seguro, vmarca, g_usuario, vcorrelativo)
         END IF
       END FOREACH
       --CPL-1134
       
       LET v_crea_fecha = HOY           

       DECLARE curs1 CURSOR FOR stmt1
       OPEN  curs1 USING afi.n_seguro, v_crea_fecha
       FETCH curs1 INTO v_existe, v_edad, v_criterio, v_ind_edad, v_curp, v_rfc, v_fena
       CLOSE curs1
       
       -- CPL-3044
       --
       LET v_siefore = 0
       
       SELECT codigo_siefore
         INTO v_siefore
         FROM cat_rango_nacimiento
        WHERE id_rango_nacimiento = v_ind_edad
       
       IF SQLCA.SQLCODE = NOTFOUND THEN 
       	  LET v_siefore = v_ind_edad 
       END IF 
       --           
       -- CPL-3044
       
       IF ls_menores = 1 THEN                         -- CPL-3165
       	  -- NO APLICA EL CAMBIO DE REGIMEN EN LA ACTIVACION DE MENORES 
       	  -- HACER UNA COPIA PARA EL TIPO 33 [ CTA_NSS_REGIMEN y CTA_REGIMEN ]
       	  -- CON EL NSS ANTERIOR CREAR UNA COPIA CON EL NUEVO NSS 
          IF (afi.tipo_solicitud = 33) THEN 
          	
       	     CALL fn_copia_regimen(v_reg_act_men.n_folio,
       	                           v_reg_act_men.tipo_solicitud,
       	                           v_reg_act_men.nti_anterior,
       	                           afi.n_seguro)
       	  END IF    
       	  
       ELSE 	                                        -- CPL-3165
          DECLARE curs2 CURSOR FOR stmt2
             OPEN curs2 USING afi.n_seguro,
                              v_ind_edad,
	                            v_siefore,              -- CPL-3044
                              v_tipo_proc,
                              v_tipo_trasp,
                              v_medio
            FETCH curs2 INTO v_existe, v_ind_edad, v_rechazo, v_folioatencion
          CLOSE curs2
            
          IF v_rechazo <> 0 THEN
	           INSERT INTO safre_tmp:rch_apertura
	           VALUES (afi.n_seguro,v_rechazo)
	        END IF
       END IF                                         -- CPL-3165

    END IF
    -- NO HAY 

    IF afi.status_interno = 100 THEN
    	
       LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                     "'",afi.n_seguro,"'",
                     ",",pmarca_entra,
                     ",",afi.n_folio,
                     ",",pestado_marca,
                     ",",pcodigo_rechazo,
                     ",",pmarca_causa,
                     ",","'","'", ",",
                     "'",g_usuario,"'",")"     
       LET ejecuta = ejecuta CLIPPED
    
       PREPARE clausula_spl FROM ejecuta
       DECLARE cursor_marca CURSOR FOR clausula_spl
          OPEN cursor_marca
         FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
         CLOSE cursor_marca
       
    END IF

    LET HORA = TIME

    INSERT INTO afi_ctr_logico
    VALUES (afi.n_folio,
            afi.tipo_solicitud,
            afi.n_seguro,
            afi.status_interno,
            g_usuario,
            HOY,
            HORA,
            operacion)

    INSERT INTO safre_tmp:afi_2
    VALUES (afi.n_seguro       ,
            afi.n_unico        ,
            afi.n_rfc          ,
            afi.paterno        ,
            afi.materno        ,
            afi.nombres        ,
            afi.fena           ,
            afi.sexo           ,
            afi.frecafor       ,
            afi.status_interno
            )

    UPDATE afi_solicitud 
       SET afi_solicitud.status         = 100 ,
           afi_solicitud.status_interno = 100 ,
           afi_solicitud.status_captura = 100
     WHERE afi_solicitud.n_seguro       = afi.n_seguro
       AND afi_solicitud.n_folio        = afi.n_folio
       AND afi_solicitud.tipo_solicitud = afi.tipo_solicitud

    IF afi.finitmte IS NULL THEN
       LET reg_carta.docto_cod = 30201 
       CALL det_carta() #dc
    ELSE
       LET reg_carta.docto_cod = 30202
       CALL det_carta() #dc
    END IF

    SELECT "X"
      FROM safre_af:rec_solicitud mr
     WHERE mr.n_seguro   = afi.n_seguro
       AND mr.origen_rec <> 1
     GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN
       UPDATE safre_af:rec_solicitud
          SET safre_af:rec_solicitud.origen_rec  =  1
        WHERE safre_af:rec_solicitud.n_seguro    =  afi.n_seguro
          AND safre_af:rec_solicitud.origen_rec <>  1
    END IF      
    
  END FOREACH
  FREE cursor_1

  SELECT "X"
    FROM safre_tmp:afi_2
   GROUP BY 1

  LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".ENVIA_REG_MAESTRO." CLIPPED,
                HOY USING "dd-mm-yy","_",HORA CLIPPED

  IF SQLCA.SQLCODE = NOTFOUND THEN 
     START REPORT listado_1 TO G_LISTA
       OUTPUT TO REPORT listado_1(mensaje)
     FINISH REPORT listado_1
  ELSE
     START REPORT listado_2 TO G_LISTA
       DECLARE cur_1 CURSOR FOR
        SELECT *
          FROM safre_tmp:afi_2

       FOREACH cur_1 INTO w_aux.*
         OUTPUT TO REPORT listado_2(w_aux.*)
       END FOREACH
     FINISH REPORT listado_2
  END IF 

  LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,
	              "/",g_usuario CLIPPED,".ENVIA_REG_MAESTRO." CLIPPED,
                HOY USING "dd-mm-yy","_",HORA CLIPPED
  RUN G_LISTA

  LET G_IMPRE = "lp ",g_paramgrales.ruta_listados CLIPPED,
	              "/",g_usuario CLIPPED,".ENVIA_REG_MAESTRO." CLIPPED,
                HOY USING "dd-mm-yy","_",HORA CLIPPED
  -- RUN G_IMPRE

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION despliega_resultados()

  DEFINE total_resp SMALLINT

  LET total_resp = con_curp + sin_curp

  ERROR ""
  IF bnd_proceso THEN
  	
     DISPLAY "          NSS INCORPORADOS AL MAESTRO DE AFILIADOS           "
     DISPLAY "Total de Registros Incorporados : ", total_resp USING "#####&"
     DISPLAY "Registros con curp asignada     : ", con_curp   USING "#####&"
     DISPLAY "Registros sin curp asignada     : ", sin_curp   USING "#####&"
     
  ELSE
  
     DISPLAY "          NSS INCORPORADOS AL MAESTRO DE AFILIADOS           "  AT 10,1 ATTRIBUTE(REVERSE)
     DISPLAY "Total de Registros Incorporados : ", total_resp USING "#####&"  AT 11,15
     DISPLAY "Registros con curp asignada     : ", con_curp   USING "#####&"  AT 12,15
     DISPLAY "Registros sin curp asignada     : ", sin_curp   USING "#####&"  AT 13,15
     PROMPT "Presione [Enter] para terminar" FOR enter
     
  END IF

  --->erm 14 Marzo 2006
  LET HORA = TIME
  LET G_IMPRE3 = g_paramgrales.ruta_listados  CLIPPED,"/",g_usuario CLIPPED,
                 ".REP_APE_CTA1." CLIPPED,
                 hoy USING "dd-mm-yy",".",hora CLIPPED

  LET G_NOMBRE = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".ENVIA_REG_MAESTRO." CLIPPED,
                 HOY USING "dd-mm-yy","_",HORA CLIPPED

  START REPORT listado_3 TO G_IMPRE3
    OUTPUT TO REPORT listado_3(total_resp,con_curp,sin_curp)
  FINISH REPORT listado_3

  -- LET gimpresion3 = "lp ",G_IMPRE3
  -- RUN gimpresion3
  ---<erm 14 Marzo 2006

END FUNCTION

-------------------------------------------------------------------------------

REPORT listado_1(mensaje)

  DEFINE mensaje             CHAR(50)

  OUTPUT
	  LEFT MARGIN   0
	  RIGHT MARGIN  0
	  TOP MARGIN    0
	  BOTTOM MARGIN 0

  FORMAT
  PAGE HEADER
       PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
            COLUMN 20," TRASPASO  AL  MAESTRO  DE  AFILIADOS "
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"
        PRINT
            COLUMN 01,"N.S.S. "  ,
            COLUMN 13,"CURP   "  ,
            COLUMN 33,"R.F.C. "  ,
            COLUMN 48,"Paterno"  ,
            COLUMN 68,"Materno"  ,
            COLUMN 88,"Nombres"       
        PRINT
            COLUMN 05,"Fecha Nac."     ,
            COLUMN 17,"Sexo"           ,
            COLUMN 28,"Fecha Frecafor" ,
            COLUMN 43,"Edo. Afiliado" 
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT 
        PRINT 
        PRINT 
            COLUMN 15,mensaje
            
END REPORT

-------------------------------------------------------------------------------

REPORT listado_2(w_aux)

  DEFINE w_aux     RECORD
                     n_seguro            LIKE afi_solicitud.n_seguro,
                     n_unico             LIKE afi_solicitud.n_unico ,
                     n_rfc               LIKE afi_solicitud.n_rfc   ,
                     paterno             LIKE afi_solicitud.paterno ,
                     materno             LIKE afi_solicitud.materno ,
                     nombres             LIKE afi_solicitud.nombres ,
                     fena                LIKE afi_solicitud.fena    ,
                     sexo                LIKE afi_solicitud.sexo    ,
                     frecafor            LIKE afi_solicitud.frecafor,
                     status_interno      SMALLINT
                   END RECORD

  DEFINE l_estado  CHAR(16) ,
         aux_sexo  CHAR(10)

  DEFINE cont      INTEGER

  OUTPUT
	  LEFT MARGIN   0
  	RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
    PAGE HEADER
         PRINT
             COLUMN 03,"========================================",
             COLUMN 40,"==================================="
         PRINT
             COLUMN 20," TRASPASO  AL  MAESTRO  DE  AFILIADOS "
         PRINT
             COLUMN 03,"----------------------------------------",
             COLUMN 40,"-----------------------------------"
         PRINT
             COLUMN 01,"N.S.S  "       ,
             COLUMN 13,"CURP   "       ,
             COLUMN 33,"R.F.C. "       ,
             COLUMN 48,"Paterno"       ,
             COLUMN 68,"Materno"       ,
             COLUMN 88,"Nombres"       
         PRINT
             COLUMN 05,"Fecha Nac."    ,
             COLUMN 17,"Sexo"          ,
             COLUMN 28,"Fecha Frecafor",
             COLUMN 43,"Edo. Afiliado" 
         PRINT
             COLUMN 03,"========================================",
             COLUMN 40,"==================================="
    ON EVERY ROW
         IF w_aux.n_unico IS NULL OR w_aux.n_unico = " " THEN
	          LET l_estado = NULL
	       END IF
	       
         CASE w_aux.status_interno
	         WHEN   0 LET l_estado = "CAPTURADO"
	         WHEN  10 LET l_estado = "IMCOMPLETO"
	         WHEN  20 LET l_estado = "COMPLETO"
	         WHEN  30 LET l_estado = "ENIVIADO"
	         WHEN  40 LET l_estado = "RECHAZADO"
	         WHEN  50 LET l_estado = "PENDIENTE"
	         WHEN  55 LET l_estado = "ACLARACION"
	         WHEN  60 LET l_estado = "APROBADO"
	         WHEN 100 LET l_estado = "REGISTRADO"
         END CASE

	       SELECT sexo_desc 
           INTO aux_sexo 
           FROM tab_sexo
          WHERE sexo_cod = w_aux.sexo

         PRINT
             COLUMN 01,w_aux.n_seguro                   ,
             COLUMN 13,w_aux.n_unico                    ,
             COLUMN 33,w_aux.n_rfc                      ,
             COLUMN 48,w_aux.paterno CLIPPED            ,
             COLUMN 68,w_aux.materno CLIPPED            ,
             COLUMN 88,w_aux.nombres CLIPPED
         PRINT
             COLUMN 05,w_aux.fena  USING "dd-mm-yyyy"   ,
             COLUMN 17,aux_sexo    ,
             COLUMN 28,w_aux.frecafor USING "dd-mm-yyyy",
             COLUMN 43,l_estado CLIPPED
    
    ON LAST ROW
    
         SELECT COUNT(*)
           INTO   cont
           FROM   safre_tmp:afi_2

         PRINT
         PRINT
         PRINT
             COLUMN 03,"----------------------------------------",
             COLUMN 40,"-----------------------------------"

         PRINT
         PRINT
             COLUMN 03,"NUMERO TOTAL DE REGISTROS ---> ",cont
             
END REPORT

-------------------------------------------------------------------------------

FUNCTION actualiza_bat_f(v_folio)

  DEFINE v_cat          CHAR(600)
  DEFINE vv_fecha_log   CHAR(030)
  DEFINE vv_prog        CHAR(010)
  DEFINE paso           CHAR(100)

  DEFINE v_fecha_log    DATETIME YEAR TO SECOND

  DEFINE v_folio        INTEGER 
  DEFINE reg_ruta       RECORD LIKE seg_modulo.*

  SELECT A.*
    INTO reg_ruta.*
    FROM seg_modulo A
   WHERE modulo_cod = "bat"
 
  UPDATE bat_ctr_operacion
     SET folio       = NULL ,      
         estado_cod  = 4    ,
         fecha_fin   = CURRENT
   WHERE pid         = reg_bat.pid
     AND proceso_cod = reg_bat.proceso_cod
     AND opera_cod   = reg_bat.opera_cod

  UPDATE bat_ctr_proceso
     SET folio       = NULL ,      
         estado_cod  = 4    ,
         fecha_fin   = CURRENT
   WHERE pid         = reg_bat.pid
     AND proceso_cod = reg_bat.proceso_cod
  
  UPDATE bat_tmp_predecesor
     SET bandera_ejecuta  = 1
   WHERE pid_prod         = reg_bat.pid
     AND proceso_cod_prod = reg_bat.proceso_cod
     AND opera_cod_prod   = reg_bat.opera_cod
  
  LET v_fecha_log  = CURRENT
  LET vv_fecha_log = v_fecha_log
  
  SELECT A.programa_cod 
    INTO vv_prog 
    FROM bat_ctr_operacion A
   WHERE A.pid         = reg_bat.pid
     AND A.proceso_cod = reg_bat.proceso_cod
     AND A.opera_cod   = reg_bat.opera_cod
  
  LET paso  = "nohup:"            ,
              reg_bat.pid         USING"&&&&&",":",
              reg_bat.proceso_cod USING"&&&&&",":",
              reg_bat.opera_cod   USING"&&&&&"
            
  LET v_cat = "echo '"                ,
              vv_fecha_log[1,4]       ,   
              vv_fecha_log[6,7]       ,  
              vv_fecha_log[9,10]      ,  
              vv_fecha_log[12,13]     ,   
              vv_fecha_log[15,16]     ,    
              vv_fecha_log[18,19]     ,
              "|"                    ,
              vv_prog  CLIPPED        ,
              "|"                     ,
              "FINOK"                ,
              "|"                     ,
              reg_ruta.ruta_listados CLIPPED,  
              "/"                     ,
              paso CLIPPED            ,
              "'"                     ,
              " >> "                  ,
              reg_ruta.ruta_envio CLIPPED ,
              "/"                     ,
              "aad_safre.log"

  LET v_cat = v_cat CLIPPED
  RUN v_cat
  
END FUNCTION

-------------------------------------------------------------------------------

FUNCTION det_carta() #dc

  LET reg_carta.nss            = afi.n_seguro 
  LET reg_carta.n_folio        = afi.n_folio
  LET reg_carta.tipo_solicitud = afi.tipo_solicitud
  LET reg_carta.fecha_registro = afi.fentcons
  LET reg_carta.opera_cod      = NULL
  LET reg_carta.edo_genera     = '10'
  LET reg_carta.fecha_genera   = TODAY
  LET reg_carta.hora_genera    = TIME
  LET reg_carta.lote_genera    = 0
  LET reg_carta.consecutivo    = 0
  LET reg_carta.id_sepomex     = 0

  LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,",
                       "?,?,?,?,?,?)"
                       
  PREPARE exe_sql FROM consulta_carta
  EXECUTE exe_sql USING reg_carta.* 

  INITIALIZE reg_carta.* TO NULL

END FUNCTION

-------------------------------------------------------------------------------

REPORT listado_3(ltotal_resp, lccurp, lscurp)    ---erm 13 Marzo 2006

  DEFINE cont_reg     INTEGER,
         l_sol        CHAR(12),
         vtip_sol     SMALLINT,
         vsolv_env    SMALLINT

  DEFINE ltotal_resp  SMALLINT
  DEFINE lccurp       SMALLINT
  DEFINE lscurp       SMALLINT
  DEFINE lrechazar    SMALLINT
  DEFINE lpendiente   SMALLINT
  DEFINE laclaracion  SMALLINT
  DEFINE lpend_90     SMALLINT
  DEFINE lafore       CHAR(50)

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
    ON LAST ROW

      PRINT COLUMN 03,"========================================",
            COLUMN 40,"==================================================="
      PRINT COLUMN 15,"  CIFRAS CONTROL APERTURA DE LA CUENTA "
      PRINT COLUMN 03,"----------------------------------------",
            COLUMN 40,"---------------------------------------------------"            
      SKIP 2 LINE
      
      PRINT COLUMN 03,"Fecha                     : ", TODAY USING "dd-mm-yyyy"      
      SKIP 1 LINE
      
      PRINT COLUMN 03,"Tipo                      : ", "1", "  ","REGISTRO"
      SKIP 1 LINE
      
      PRINT COLUMN 03,"Clave Operador            : ", g_usuario CLIPPED
      SKIP 1 LINE
      
      PRINT COLUMN 03,"Nombre Archivo Procesar   : ", "_______________________________"
      SKIP 1 LINE
      
      PRINT COLUMN 03,"Hora de Generacion Proceso: ",HORA CLIPPED
      SKIP 1 LINE
      
      PRINT COLUMN 03,"No. Registros Aperturados               : ",ltotal_resp USING "#######&"
      PRINT COLUMN 03,"No. Registros con CURP asignada         : ",lccurp USING "#######&"
      PRINT COLUMN 03,"No. Registros con CURP no asignada      : ",lscurp USING "#######&"
      PRINT COLUMN 03,"Registros con Marca de 56 anios         : ",""
      PRINT COLUMN 03,"Registros sin Marca de 56 anios         : ",""
      PRINT COLUMN 03,"Nombre y Ruta Reporte a Detalle :"
      PRINT COLUMN 05,G_NOMBRE CLIPPED
      SKIP 2 LINE
      
      PRINT COLUMN 03,"========================================",
            COLUMN 40,"==================================================="

END REPORT

-------------------------------------------------------------------------------

FUNCTION desmarca_cuenta (vnss, vmarca, vusuario, vcorrelativo)

  DEFINE vnss           CHAR(11),
         vmarca         SMALLINT,
         vusuario       CHAR(08),
         vcorrelativo   INTEGER,
         vestado_marca  SMALLINT,
         vmarca_causa   SMALLINT

  LET vestado_marca = 0
  LET vmarca_causa  = 0

  PREPARE eje_desmarca FROM v_desmarca

  EXECUTE eje_desmarca USING vnss,
                             vmarca,
                             vcorrelativo,
                             vestado_marca,
                             vmarca_causa,
                             vusuario

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION fn_copia_regimen(pfolio, psolicitud, pnti, pnss)

  DEFINE pfolio      DECIMAL(10)
  DEFINE psolicitud  SMALLINT 
  DEFINE pnti        CHAR(11) 
  DEFINE pnss        CHAR(11)
  
  DEFINE v_cta_nss   RECORD LIKE cta_nss_regimen.*
  DEFINE v_cta_reg   RECORD LIKE cta_regimen.*

  -- CREAR TABLAS TEMPORALES        	  
  WHENEVER ERROR CONTINUE
    DROP TABLE cta_nss_regimen_menor 
  WHENEVER ERROR STOP 
  
  WHENEVER ERROR CONTINUE
    DROP TABLE cta_regimen_menor 
  WHENEVER ERROR STOP 
   
  -- SELECCIONAR LOS REGISTROS Y CREAR TEMPORALES 
  LET lc_comando = '',
                   '\n   SELECT * FROM cta_nss_regimen     ',
                   '\n    WHERE nss = "',pnti,'"           ',
                   '\n     INTO TEMP cta_nss_regimen_menor '
  PREPARE tab1 FROM lc_comando
  EXECUTE tab1 
   
  LET lc_comando = '',
                   '\n   SELECT * FROM cta_regimen         ',
                   '\n    WHERE nss = "',pnti,'"           ',
                   '\n     INTO TEMP cta_regimen_menor     '
  PREPARE tab2 FROM lc_comando
  EXECUTE tab2
  
  -- PASAR REGISTROS CON NUEVO NSS 
  DECLARE tab1_cur CURSOR FOR 
   SELECT * FROM cta_nss_regimen_menor
  FOREACH tab1_cur INTO v_cta_nss.* 
     LET v_cta_nss.nss = pnss              -- ASIGNAR EL NUEVO NSS 
     INSERT INTO cta_nss_regimen VALUES(v_cta_nss.*)
  END FOREACH
  FREE tab1_cur 
  
  DECLARE tab2_cur CURSOR FOR 
   SELECT * FROM cta_regimen_menor
  FOREACH tab2_cur INTO v_cta_reg.* 
     LET v_cta_reg.nss = pnss              -- ASIGNAR EL NUEVO NSS 
     INSERT INTO cta_regimen  VALUES(v_cta_reg.*)
  END FOREACH
  FREE tab2_cur

END FUNCTION 

-------------------------------------------------------------------------------
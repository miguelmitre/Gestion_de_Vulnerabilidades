-------------------------------------------------------------------------------
-- Proyecto      = Sistema de Afores.( MEXICO ) 
-- Propietario   = EFP                         
-- Programa      = AFIB054
-- Descripcion   = ACTIVACION DE MENORES
-------------------------------------------------------------------------------

DATABASE safre_af

-------------------------------------------------------------------------------

GLOBALS 

 DEFINE vusu               VARCHAR(10)
 DEFINE vsubtitulo         VARCHAR(45)
 DEFINE vprog              VARCHAR(15)
 DEFINE vbasura            CHAR(1)
 DEFINE v_arch             VARCHAR(60)
 DEFINE v_ruta_envio       VARCHAR(40)
 DEFINE v_ruta_rescate     VARCHAR(40)
 DEFINE v_ruta_listados    VARCHAR(40)
 DEFINE v_ruta_exp         VARCHAR(40)
 DEFINE lc_comando         CHAR(3000) 
 DEFINE lc_pausa           CHAR(1)

 DEFINE vtot_rme_locali    INTEGER  
 DEFINE vtot_rme_noloca    INTEGER 
 DEFINE vtot_rme_acepta    INTEGER 
 DEFINE vtot_rme_rechaz    INTEGER  
 DEFINE vtot_rme_regist    INTEGER
 
 DEFINE v_ren              SMALLINT 
 DEFINE v_col              SMALLINT   
 
END GLOBALS 

-------------------------------------------------------------------------------

MAIN 

  CALL STARTLOG(FGL_GETENV("USER")||"activacion.log")  

  OPTIONS INPUT WRAP,
          PROMPT  LINE LAST,
          ERROR   LINE LAST,
          COMMENT LINE LAST,
          MESSAGE LINE LAST

  DEFER INTERRUPT

  -- POSICION DE VENTANA DE MENSAJES AL USUARIO 
  LET v_ren = 9
  LET v_col = 7 
  
  LET INT_FLAG = FALSE

  CALL fn_get_usuario()
       RETURNING vusu

  -- RUTAS   
  LET v_ruta_envio     = ""
  LET v_ruta_rescate   = ""
  LET v_ruta_listados  = ""
     
  SELECT ruta_envio,    ruta_rescate, 
         ruta_listados, ruta_exp
    INTO v_ruta_envio,    v_ruta_rescate, 
         v_ruta_listados, v_ruta_exp 
    FROM safre_af:seg_modulo
   WHERE modulo_cod = "afi"

  LET vsubtitulo   = "ACTIVACION DE MENORES" 
  LET vprog        = "AFIB054" 

  OPEN WINDOW v1 AT 2,2 WITH 21 ROWS , 78 COLUMNS ATTRIBUTE(BORDER)

    CALL fn_encabezado(vsubtitulo,vprog,3)
      
    MENU ""
           
      COMMAND "Cargar Archivo" "Cargar Archivo Activacion Menores"
              CALL carga()
                        
      COMMAND "Generar Archivo" "Generar Archivo de Respuesta"
              CALL pide_folio("ARCHIVO")
                        
      COMMAND "Reporte" "Reporte"
              CALL pide_folio("REPORTE")
                                        
      COMMAND "Salir" "Salir"
              EXIT MENU
              
    END MENU
    CLOSE WINDOW v1

END MAIN 

-------------------------------------------------------------------------------

FUNCTION carga()

  DEFINE vpausa           CHAR(1)
  DEFINE ejecuta          CHAR(200) 
  DEFINE ls_status        SMALLINT 

  --------------------------------------------
  
  LET v_arch = ""

  LET vtot_rme_locali = 0 
  LET vtot_rme_noloca = 0
  LET vtot_rme_acepta = 0
  LET vtot_rme_rechaz = 0 
  LET vtot_rme_regist = 0 

  LET ejecuta = "cd ",v_ruta_rescate CLIPPED,"; ls *.ACTMEN > ",v_ruta_rescate CLIPPED,"/archivos_lst" CLIPPED
  RUN ejecuta

  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_archivos_lst
  WHENEVER ERROR STOP

  CREATE TEMP TABLE tmp_archivos_lst (lista  CHAR(100))

  LET ejecuta = v_ruta_rescate CLIPPED,"/archivos_lst" CLIPPED

  LOAD FROM ejecuta INSERT INTO tmp_archivos_lst

  LET ejecuta = "rm ",v_ruta_rescate CLIPPED,"/archivos_lst" CLIPPED
  RUN ejecuta
  
  --------------------------------------------

  OPEN WINDOW v2 AT 6,2 WITH FORM "AFIB0541" ATTRIBUTE(BORDER, MESSAGE LINE LAST -1)
    
    --CAPTURA EL NOMBRE DE ARCHIVO A PROCESAR
    INPUT BY NAME v_arch WITHOUT DEFAULTS
    	
       AFTER FIELD v_arch
          IF v_arch IS NULL OR v_arch = "" THEN
             ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO "    
             LET v_arch = ""
             DISPLAY BY NAME v_arch
             NEXT FIELD v_arch
          END  IF
    
          LET lc_comando = '',
                           '\n SELECT lista                        ',
                           '\n   FROM safre_af:tmp_archivos_lst    ',
                           '\n  WHERE lista = "',v_arch CLIPPED,'" '
          -- DISPLAY lc_comando CLIPPED                           
          PREPARE carch1 FROM lc_comando
          EXECUTE carch1                 
    
          IF SQLCA.SQLCODE = NOTFOUND THEN
             ERROR " NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE "
             LET v_arch = ""
             DISPLAY BY NAME v_arch
             NEXT FIELD v_arch
          END IF
          
          IF archivo_cargado(v_arch) THEN 
             ERROR " EL ARCHIVO YA FUE CARGADO ANTERIORMENTE..."
             LET v_arch = ""
             DISPLAY BY NAME v_arch
             NEXT FIELD v_arch
          END IF 	
    
       ON KEY (ESC)
          IF v_arch IS NULL OR v_arch = "" THEN
             ERROR " NOMBRE DE ARCHIVO NO PUEDE SER NULO "
             LET v_arch = ""
             DISPLAY BY NAME v_arch
             NEXT FIELD v_arch
          END IF
             
          LET lc_comando = '',
                           '\n SELECT lista                        ',
                           '\n   FROM safre_af:tmp_archivos_lst    ',
                           '\n  WHERE lista = "',v_arch CLIPPED,'" '
          -- DISPLAY lc_comando CLIPPED                           
          PREPARE carch2 FROM lc_comando
          EXECUTE carch2             
              
          IF SQLCA.SQLCODE = NOTFOUND THEN
             ERROR " NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE "
             LET v_arch = ""
             DISPLAY BY NAME v_arch
             NEXT FIELD v_arch
          END IF
          
          IF archivo_cargado(v_arch) THEN 
             ERROR " EL ARCHIVO YA FUE CARGADO ANTERIORMENTE..."
             LET v_arch = ""
             DISPLAY BY NAME v_arch
             NEXT FIELD v_arch
          END IF 	
          
          EXIT INPUT 
    
       ON KEY (INTERRUPT, CONTROL-C)
          PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR lc_pausa
          EXIT INPUT
            
    END INPUT
    
    IF INT_FLAG = TRUE THEN 
       LET INT_FLAG = FALSE
    ELSE
       --CARGA LA INFORMACION DEL ARCHIVO PLANO A TABLA TEMPORAL 
       CALL carga_datos() 
            RETURNING ls_status
          
       MESSAGE ""    
    END IF 
    
    -- BORRAR ARCHIVOS RESIDUALES 
    LET ejecuta = "cd    ",v_ruta_rescate CLIPPED,"/;",
                  "touch ",v_ruta_rescate CLIPPED,"/det_activa_menores;",
                  "rm    ",v_ruta_rescate CLIPPED,"/det_activa_menores"
    RUN ejecuta
  
  CLOSE WINDOW v2    
  DROP TABLE tmp_archivos_lst
  
  RETURN

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION pide_folio(pc_opcion)

  DEFINE pc_opcion        VARCHAR(10)

  DEFINE ls_existe_folio  SMALLINT 
  DEFINE li_folio_lote    DECIMAL(10,0) 
  DEFINE ls_status        SMALLINT 

  OPEN WINDOW v3 AT 6,2 WITH FORM "AFIB0541" ATTRIBUTE(BORDER, MESSAGE LINE LAST -1)
    
    --CAPTURA EL NOMBRE DE ARCHIVO A PROCESAR
    INPUT BY NAME li_folio_lote WITHOUT DEFAULTS
    	
       AFTER FIELD li_folio_lote
       
          LET ls_existe_folio = 0 
          
          IF li_folio_lote IS NULL OR li_folio_lote = 0 OR li_folio_lote = "" THEN
             ERROR " FOLIO LOTE NO PUEDE SER NULO O CERO "    
             INITIALIZE li_folio_lote TO NULL
             DISPLAY BY NAME li_folio_lote
             NEXT FIELD li_folio_lote
          END  IF
    
          LET lc_comando = '',
                           '\n SELECT COUNT(*)                                 ',
                           '\n   FROM safre_af:afi_act_menor_edad              ',
                           '\n  WHERE folio_lote = "',li_folio_lote CLIPPED,'" '
          -- DISPLAY lc_comando CLIPPED                           
          PREPARE gen01 FROM lc_comando
          EXECUTE gen01 INTO ls_existe_folio                 
    
          IF ls_existe_folio = 0 THEN
             ERROR " EL FOLIO LOTE... NO EXISTE "
             INITIALIZE li_folio_lote TO NULL
             DISPLAY BY NAME li_folio_lote
             NEXT FIELD li_folio_lote
          END IF
    
       ON KEY (ESC)
          IF li_folio_lote IS NULL OR li_folio_lote = 0 OR li_folio_lote = "" THEN
             ERROR " EL FOLIO LOTE NO PUEDE SER NULO O CERO"
             INITIALIZE li_folio_lote TO NULL
             DISPLAY BY NAME li_folio_lote
             NEXT FIELD li_folio_lote
          END IF
             
          LET lc_comando = '',
                           '\n SELECT COUNT(*)                                 ',
                           '\n   FROM safre_af:afi_act_menor_edad              ',
                           '\n  WHERE folio_lote = "',li_folio_lote CLIPPED,'" '
          -- DISPLAY lc_comando CLIPPED                           
          PREPARE gen02 FROM lc_comando
          EXECUTE gen02 INTO ls_existe_folio                 
    
          IF ls_existe_folio = 0 THEN
             ERROR " EL FOLIO LOTE... NO EXISTE "
             INITIALIZE li_folio_lote TO NULL
             DISPLAY BY NAME li_folio_lote
             NEXT FIELD li_folio_lote
          END IF
          
          EXIT INPUT 
    
       ON KEY (INTERRUPT, CONTROL-C)
          PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR lc_pausa
          EXIT INPUT
            
    END INPUT
    
    IF INT_FLAG = TRUE THEN 
       LET INT_FLAG = FALSE
    ELSE    
       CASE 
         WHEN pc_opcion = "ARCHIVO"
              -- GENERAR EL ARCHIVO DE RESPUESTA 
              CALL genera_archivo(li_folio_lote) 
         WHEN pc_opcion = "REPORTE"
              -- GENERAR EL REPORTE DE CARGA ARCHIVO DE RESPUESTA 
              CALL genera_reporte(li_folio_lote) 
       END CASE   
       MESSAGE ""    
    END IF 
    
  CLOSE WINDOW v3  
  
  RETURN

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION genera_reporte(pi_folio_lote) 

  DEFINE pi_folio_lote  DECIMAL(10,0)
  
  DEFINE li_folio_lote  INTEGER  
  DEFINE lv_folio_lote  VARCHAR(10)
  
  DEFINE lr_reporte     VARCHAR(100)
  DEFINE lr_nombre_rep  VARCHAR(050)

  DEFINE lv_afore_local CHAR(03)
  DEFINE ld_fecha       DATE 
  DEFINE lc_fecha_10    CHAR(10)
  DEFINE lc_fecha_08    CHAR(08) 
  DEFINE ls_registros   SMALLINT
  DEFINE ls_tipo_sol    SMALLINT 
  
  DEFINE lr_reg         RECORD   
                          id_folio_lote   VARCHAR(10),                    
                          id_04_nss       VARCHAR(11), --
                          id_05_curp      VARCHAR(18), --
                          id_06_rfc       VARCHAR(13), --XXXX999999XXX
                          id_07_paterno   VARCHAR(40), --NO BLANCOS
                          id_08_materno   VARCHAR(40), --NO BLANCOS o N/A
                          id_09_nombre    VARCHAR(40), --NO BLANCOS
                          id_10_fec_nac   VARCHAR(08), --AAAAMMDD
                          id_11_sexo      VARCHAR(01), --1=M, 2=F
                          id_12_ent_nac   VARCHAR(02), --
                          id_13_nacion    VARCHAR(03), --
                          id_14_fec_act   VARCHAR(08), --AAAAMMDD
                          id_15_tip_afi   VARCHAR(01), --2, 4 o 5
                          id_16_cla_afo   VARCHAR(03), --568 COPPEL
                          id_17_mar_reg   VARCHAR(04), --
                          id_18_ori_act   VARCHAR(01), --1,2,3,4
                          id_19_res_afo   VARCHAR(02) --01
                        END RECORD  
                        
  -- CREAR TABLA TEMPORAL 
  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_crea_reporte_menores
  WHENEVER ERROR STOP

  CREATE TABLE tmp_crea_reporte_menores
  ( 
   renglon  SMALLINT   ,
   registro VARCHAR(20),
   folio    VARCHAR(20), 
   tip_sol  VARCHAR(20),
   nss      VARCHAR(20),
   curp     VARCHAR(20),
   rfc      VARCHAR(20),
   paterno  VARCHAR(40),
   materno  VARCHAR(40),
   nombres  VARCHAR(40),
   fec_act  VARCHAR(20),
   tip_afi  VARCHAR(20),
   cla_afo  VARCHAR(20),
   mar_reg  VARCHAR(20),
   ori_act  VARCHAR(20)
  )

  -- AFORE LOCAL                         
  SELECT codigo_afore INTO lv_afore_local 
    FROM tab_afore_local

  -- NOMBRE DEL ARCHIVO RESPUESTA 
  LET li_folio_lote = pi_folio_lote 
  LET lv_folio_lote = li_folio_lote 
  LET lr_nombre_rep = "REPORTE_act_men_FOLIO_",lv_folio_lote CLIPPED,".txt"
  LET lr_reporte    = v_ruta_listados CLIPPED,"/",lr_nombre_rep CLIPPED
    
  -- FECHA DE HOY 
  LET ld_fecha    = TODAY 
  LET lc_fecha_10 = ld_fecha     
  LET lc_fecha_08 = lc_fecha_10[07,10],lc_fecha_10[01,02],lc_fecha_10[04,05]

  -- ENCABEZADO
  INSERT INTO tmp_crea_reporte_menores VALUES                                     
  (0                 , "REGISTRO "        , "FOLIO LOTE"    ,                                       
   "TIPO SOLICITUD"  , "NSS"              , "CURP"          , "RFC",              
   "APELLIDO PATERNO", "APELLIDO MATERNO" , "NOMBRES"       , "FECHA ACTIVACION", 
   "TIPO AFILIACION" , "CLAVE AFORE"      , "MARCA REGIMEN" , "ORIGEN ACTIVACION")

  -- DETALLE    
  LET ls_registros = 0  
  DECLARE reporte_cur CURSOR FOR 
      SELECT folio_lote                        ,
             nss                               ,
             curp                              ,
             rfc                               ,
             paterno                           ,
             materno                           ,
             nombres                           ,
             TO_CHAR(fecha_nacimiento,"%Y%m%d"),
             sexo                              ,
             LPAD(entidad_nacimiento,2,'0')    ,
             nacionalidad                      ,
             TO_CHAR(fecha_activacion,"%Y%m%d"),
             tipo_afiliacion                   ,
             clave_afore                       ,
             marca_regimen                     ,
             origen_activacion                 ,
             '01'                                 -- RESPUESTA DE LA AFORE 
        FROM afi_act_menor_edad
       WHERE folio_lote = pi_folio_lote

  FOREACH reporte_cur INTO lr_reg.*   
    
    LET ls_registros = ls_registros + 1 
    
    CASE 
      WHEN lr_reg.id_15_tip_afi = '2'
           LET ls_tipo_sol = 33  -- IMSS  
      WHEN lr_reg.id_15_tip_afi = '4'
           LET ls_tipo_sol = 34  -- ISSSTE  
      WHEN lr_reg.id_15_tip_afi = '5'
           LET ls_tipo_sol = 32  -- INDEPENDIENTE  
    END CASE

    INSERT INTO tmp_crea_reporte_menores VALUES 
    (ls_registros        , ls_registros        , lr_reg.id_folio_lote,
     ls_tipo_sol         , lr_reg.id_04_nss    , lr_reg.id_05_curp   , lr_reg.id_06_rfc    ,
     lr_reg.id_07_paterno, lr_reg.id_08_materno, lr_reg.id_09_nombre , lr_reg.id_14_fec_act,
     lr_reg.id_15_tip_afi, lr_reg.id_16_cla_afo, lr_reg.id_17_mar_reg, lr_reg.id_18_ori_act)

  END FOREACH
  FREE reporte_cur
  
  -- GENERAR EL ARCHIVO MEDIANTE UN UNLOAD  
  UNLOAD TO lr_reporte
  SELECT TRIM(registro), TRIM(folio  ), TRIM(tip_sol), TRIM(nss    ),
         TRIM(curp    ), TRIM(rfc    ), TRIM(paterno), TRIM(materno), 
         TRIM(nombres ), TRIM(fec_act), TRIM(tip_afi), TRIM(cla_afo), 
         TRIM(mar_reg ), TRIM(ori_act)                                
    FROM tmp_crea_reporte_menores   
   ORDER BY renglon   
  
  -- AVISO AL USUARIO   
  OPEN WINDOW wauxi01 AT v_ren,v_col WITH 7 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
     DISPLAY  "SE GENERO REPORTE DEL FOLIO ",lv_folio_lote        AT 2,2
     DISPLAY  "RUTA      : ",v_ruta_listados  CLIPPED             AT 3,2
     DISPLAY  "NOMBRE    : ",lr_nombre_rep CLIPPED                AT 4,2
     DISPLAY  "REGISTROS : ",ls_registros  CLIPPED                AT 5,2
     PROMPT " PRESIONE <ENTER> PARA CONTINUAR " FOR CHAR lc_pausa        
  CLOSE WINDOW wauxi01

  -- BORRAR TABLA TEMPORAL 
  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_crea_reporte_menores
  WHENEVER ERROR STOP

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION genera_duplicados() 
                        
  DEFINE lr_reporte       VARCHAR(100)
  DEFINE lr_nombre_rep    VARCHAR(050)
                          
  DEFINE lv_afore_local   CHAR(03)
  DEFINE ls_registros     SMALLINT
  DEFINE ls_tipo_sol      SMALLINT 
                          
  DEFINE lr_reg           RECORD   
                            id_curp         VARCHAR(18),                    
                            id_total        VARCHAR(10)
                          END RECORD  
                        
  -- CREAR TABLA TEMPORAL 
  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_crea_reporte_duplicados
  WHENEVER ERROR STOP

  CREATE TABLE tmp_crea_reporte_duplicados
  ( 
   renglon  SMALLINT   ,
   numero   VARCHAR(10),
   curp     VARCHAR(18),
   total    VARCHAR(10)
  )

  -- AFORE LOCAL                         
  SELECT codigo_afore INTO lv_afore_local 
    FROM tab_afore_local

  -- NOMBRE DEL ARCHIVO RESPUESTA 
  LET lr_nombre_rep = "CURPS_DUPLICADOS.txt"
  LET lr_reporte    = v_ruta_listados CLIPPED,"/",lr_nombre_rep CLIPPED
    
  -- ENCABEZADO
  INSERT INTO tmp_crea_reporte_duplicados VALUES                                     
  (0, "NUM","CURP","TOTAL")

  -- DETALLE  
  LET ls_registros = 0  
  
  DECLARE recorre_cur CURSOR FOR 
   SELECT * FROM safre_tmp:tmp_reg_activa_duplicados   

  FOREACH recorre_cur INTO lr_reg.*   
    
    LET ls_registros = ls_registros + 1 
    
    INSERT INTO tmp_crea_reporte_duplicados VALUES 
    (ls_registros, ls_registros, lr_reg.id_curp, lr_reg.id_total)

  END FOREACH
  FREE recorre_cur
  
  -- GENERAR EL ARCHIVO MEDIANTE UN UNLOAD  
  UNLOAD TO lr_reporte
  SELECT TRIM(numero), TRIM(curp  ), TRIM(total)
    FROM tmp_crea_reporte_duplicados   
   ORDER BY renglon   
  

  -- BORRAR TABLA TEMPORAL 
  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_crea_reporte_duplicados
  WHENEVER ERROR STOP
  
  RETURN lr_reporte

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION genera_archivo(pi_folio_lote) 

  DEFINE pi_folio_lote  DECIMAL(10,0)
  DEFINE li_folio_lote  INTEGER  
  DEFINE lv_folio_lote  VARCHAR(10)
  
  DEFINE lr_reporte     VARCHAR(100)
  DEFINE lr_nombre_rep  VARCHAR(050)

  DEFINE lv_afore_local CHAR(03)
  DEFINE ld_fecha       DATE 
  DEFINE lc_fecha_10    CHAR(10)
  DEFINE lc_fecha_08    CHAR(08) 
  
  DEFINE lc_linea       CHAR(220)
  DEFINE lc_linea_e     CHAR(220)
  DEFINE lc_linea_s     CHAR(220)
  
  DEFINE lc_registros   CHAR(09)
  DEFINE ls_registros   SMALLINT 
  DEFINE ld_act_menor   DECIMAL(10,0) 
  
  DEFINE lr_reg         RECORD 
                          id_01_tip_reg   CHAR(02), --02
                          id_02_conta     CHAR(10), --consecutivo
                          id_03_clave     CHAR(02), --20
                          id_04_nss       CHAR(11), --
                          id_05_curp      CHAR(18), --
                          id_06_rfc       CHAR(13), --XXXX999999XXX
                          id_07_paterno   CHAR(40), --NO BLANCOS
                          id_08_materno   CHAR(40), --NO BLANCOS o N/A
                          id_09_nombre    CHAR(40), --NO BLANCOS
                          id_10_fec_nac   CHAR(08), --AAAAMMDD
                          id_11_sexo      CHAR(01), --1=M, 2=F
                          id_12_ent_nac   CHAR(02), --
                          id_13_nacion    CHAR(03), --
                          id_14_fec_act   CHAR(08), --AAAAMMDD
                          id_15_tip_afi   CHAR(01), --2, 4 o 5
                          id_16_cla_afo   CHAR(03), --568 COPPEL
                          id_17_mar_reg   CHAR(04), --
                          id_18_ori_act   CHAR(01), --1,2,3,4
                          id_19_res_afo   CHAR(02) --01
                        END RECORD 

  -- AFORE LOCAL                         
  SELECT codigo_afore INTO lv_afore_local 
    FROM tab_afore_local

  -- NOMBRE DEL ARCHIVO RESPUESTA 
  LET li_folio_lote = pi_folio_lote 
  LET lv_folio_lote = li_folio_lote 
  LET lr_nombre_rep = "respuesta_act_men_",lv_afore_local,"_",lv_folio_lote CLIPPED,".txt"
  LET lr_reporte    = v_ruta_envio CLIPPED,"/",lr_nombre_rep CLIPPED
    
  -- FECHA DE HOY 
  LET ld_fecha    = TODAY 
  LET lc_fecha_10 = ld_fecha     
  LET lc_fecha_08 = lc_fecha_10[07,10],lc_fecha_10[01,02],lc_fecha_10[04,05]

  -- ENCABEZADO
  LET lc_linea_e = "01012001",lv_afore_local,"03001",lc_fecha_08 
  
  -- SUMARIO   
  SELECT COUNT(*) 
    INTO lc_registros
    FROM afi_act_menor_edad
   WHERE folio_lote = pi_folio_lote
   
  LET lc_comando = '', 
                   '\n SELECT LPAD("',lc_registros CLIPPED,'",9,"0") ',
                   '\n   FROM tab_afore_local                        ' 
  PREPARE gen03 FROM lc_comando
  EXECUTE gen03 INTO lc_registros 
  
  LET lc_linea_s = "09",lc_registros
  
  -- INICIA CREACION DE REPORTE
  START REPORT arch_respuesta TO lr_reporte
  
    -- ENCABEZADO
    OUTPUT TO REPORT arch_respuesta(lc_linea_e)
    
    LET ls_registros = 0 
    
    DECLARE resp_cur CURSOR FOR 
      SELECT id_act_menor_edad                 ,
             tipo_registro                     ,
             LPAD(contador_servicio,10,'0')    ,
             clave_operacion                   ,
             nss                               ,
             curp                              ,
             rfc                               ,
             paterno                           ,
             materno                           ,
             nombres                           ,
             TO_CHAR(fecha_nacimiento,"%Y%m%d"),
             sexo                              ,
             LPAD(entidad_nacimiento,2,'0')    ,
             nacionalidad                      ,
             TO_CHAR(fecha_activacion,"%Y%m%d"),
             tipo_afiliacion                   ,
             clave_afore                       ,
             marca_regimen                     ,
             origen_activacion                 ,
             '01'                                 -- RESPUESTA DE LA AFORE 
        FROM afi_act_menor_edad
       WHERE folio_lote   = pi_folio_lote
         AND edo_registro = 10                    -- REGISTRADO O CARGADO
    
    FOREACH resp_cur INTO ld_act_menor, lr_reg.*
    
      LET ls_registros = ls_registros + 1 
       
      LET lc_linea  =  lr_reg.id_01_tip_reg,
                       lr_reg.id_02_conta  ,
                       lr_reg.id_03_clave  ,
                       lr_reg.id_04_nss    ,
                       lr_reg.id_05_curp   ,
                       lr_reg.id_06_rfc    ,
                       lr_reg.id_07_paterno,
                       lr_reg.id_08_materno,
                       lr_reg.id_09_nombre ,
                       lr_reg.id_10_fec_nac,
                       lr_reg.id_11_sexo   ,
                       lr_reg.id_12_ent_nac,
                       lr_reg.id_13_nacion ,
                       lr_reg.id_14_fec_act,
                       lr_reg.id_15_tip_afi,
                       lr_reg.id_16_cla_afo,
                       lr_reg.id_17_mar_reg,
                       lr_reg.id_18_ori_act,
                       lr_reg.id_19_res_afo
      -- DETALLE                  
      OUTPUT TO REPORT arch_respuesta(lc_linea)
      
      -- ACTUALIZAR INFORMACION DEL REGSTRO 
      UPDATE afi_act_menor_edad
         SET nom_archivo_envio = lr_nombre_rep, 
             fecha_envio       = TODAY        ,   
             edo_registro      = 30               -- ENVIADO 
       WHERE id_act_menor_edad = ld_act_menor
         AND folio_lote        = pi_folio_lote     
       
    END FOREACH
    FREE resp_cur
    
    -- SUMARIO  
    OUTPUT TO REPORT arch_respuesta(lc_linea_s)
  
  FINISH REPORT arch_respuesta
  
  -- AVISO AL USUARIO   
  IF ls_registros = 0 THEN 
     OPEN WINDOW wauxj01 AT v_ren,v_col WITH 7 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
        DISPLAY  "NO SE GENERO ARCHIVO RESPUESTA DEL FOLIO ",lv_folio_lote AT 2,2
        DISPLAY  "RAZON:                                                 " AT 3,2 
        DISPLAY  "SI EXISTE EL FOLIO, PERO LOS REGISTROS YA NO TIENEN EL " AT 4,2
        DISPLAY  "ESTADO REQUERIDO QUE ES (REGISTRADO O CARGADO)         " AT 5,2
        PROMPT " PRESIONE <ENTER> PARA CONTINUAR " FOR CHAR lc_pausa        
     CLOSE WINDOW wauxj01
  ELSE    
     OPEN WINDOW wauxh01 AT v_ren,v_col WITH 7 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
        DISPLAY  "SE GENERO ARCHIVO RESPUESTA DEL FOLIO ",lv_folio_lote    AT 2,2
        DISPLAY  "RUTA      : ",v_ruta_envio  CLIPPED                      AT 3,2
        DISPLAY  "NOMBRE    : ",lr_nombre_rep CLIPPED                      AT 4,2
        DISPLAY  "REGISTROS : ",lc_registros  CLIPPED                      AT 5,2
        PROMPT " PRESIONE <ENTER> PARA CONTINUAR " FOR CHAR lc_pausa        
     CLOSE WINDOW wauxh01
  END IF 

END FUNCTION

-------------------------------------------------------------------------------

REPORT arch_respuesta(p_linea)

  DEFINE p_linea CHAR(220)
  
  OUTPUT         
      PAGE   LENGTH 1
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      RIGHT  MARGIN 0
      LEFT   MARGIN 0

  FORMAT
      ON EVERY ROW
      
      PRINT COLUMN 001, p_linea

END REPORT 

-------------------------------------------------------------------------------

FUNCTION fn_encabezado(subtitulo,prog,linea)

  -- PONER EL fn_encabezado ESTANDAR EN LAS VENTANAS QUE SE 
  -- UTILICEN DENTRO DE LOS MODULOS DE SAFRE

  DEFINE subtitulo  CHAR(45)
  DEFINE prog       CHAR(15)
  DEFINE linea      SMALLINT 

  DEFINE hoy_hoy    DATE
  DEFINE i          SMALLINT 

  LET hoy_hoy = TODAY
	   
  -- CENTRAR EL TITULO
  LET i = LENGTH(subtitulo CLIPPED)
  LET i = ((80 - i) / 2) + 1

  DISPLAY "                                                                              "
       AT linea,1 ATTRIBUTE(REVERSE)
  DISPLAY hoy_hoy   USING "DD-MM-YYYY " AT linea,69 ATTRIBUTE(REVERSE)
  DISPLAY prog      CLIPPED             AT linea,01 ATTRIBUTE(REVERSE) 
  DISPLAY subtitulo CLIPPED             AT linea,i  ATTRIBUTE(REVERSE) 

  DISPLAY " <ESC> Procesar "            AT 4,1  
  DISPLAY " <Ctrl-C> Salir "            AT 4,63 

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION fn_get_usuario()

  -- OBTENER EL LOGIN DEL USUARIO QUE ESTA EJECUTANDO EL PROCESO ACTUAL 

  DEFINE v_usuario   VARCHAR(10) 
  
  SELECT MAX(USER) 
    INTO v_usuario
    FROM tab_afore_local
	
  RETURN v_usuario 

END FUNCTION 

-------------------------------------------------------------------------------

FUNCTION carga_datos() 

  DEFINE ejecuta             CHAR(500)
  DEFINE ls_resultado        SMALLINT 
  DEFINE ls_cargados         SMALLINT    
  DEFINE ls_encabezado       SMALLINT 
  DEFINE ls_sumario          SMALLINT 
  
  DEFINE lc_fec_transfe      CHAR(08)
  DEFINE lv_fec_transfe      CHAR(10)
  DEFINE ld_fec_transfe      DATE 
  
  DEFINE ls_servicio         SMALLINT 
  DEFINE lc_servicio         CHAR(02)
  
  DEFINE ls_operacion        SMALLINT
  DEFINE lc_operacion        CHAR(02) 
  DEFINE ls_duplicados       SMALLINT 
  DEFINE lc_curp_duplicado   CHAR(18)
  
  DEFINE lc_curp_validando   VARCHAR(18)
  DEFINE ls_curp_longitud    SMALLINT 
  
  DEFINE lc_nss_validando    VARCHAR(11)
  DEFINE ls_nss_longitud     SMALLINT 
  
  DEFINE lc_feac_validando   VARCHAR(08)
  DEFINE ls_feac_longitud    SMALLINT 
  
  DEFINE lc_repe_validando   VARCHAR(04)
  DEFINE ls_repe_longitud    SMALLINT 
  
  DEFINE ls_tiene_curp       SMALLINT 
  DEFINE ls_tiene_nss        SMALLINT 
  DEFINE ls_tiene_fec_act    SMALLINT 
  DEFINE ls_tiene_reg_pen    SMALLINT
  
  DEFINE ls_exi_reg_pen      SMALLINT  
  DEFINE ls_exi_curp_bd      SMALLINT 
  DEFINE ls_tip_sol_val      SMALLINT 
  DEFINE lc_tipo_afil        CHAR(1)
  DEFINE ls_reg_aceptado     SMALLINT 
  DEFINE lc_rechazo          VARCHAR(60)
  DEFINE ls_realiza_carga    SMALLINT 
  DEFINE li_folio_lote       DECIMAL(10) 
  DEFINE l_folio             INTEGER 
  DEFINE ls_reg_rechazos     INTEGER 
  DEFINE lc_reg_rechazos     VARCHAR(10)
  
  DEFINE lv_paterno          VARCHAR(40)
  DEFINE lv_materno          VARCHAR(40)
  DEFINE lv_nombres          VARCHAR(40)
  DEFINE lv_origen           CHAR(1) 
  DEFINE ls_entidad_valida   SMALLINT 
  DEFINE ls_nacion_valida    SMALLINT 
  DEFINE ls_activa_valida    SMALLINT 
  DEFINE ls_rechaza_registro SMALLINT 
  
  DEFINE lc_nombre_reporte   VARCHAR(100)
  DEFINE lc_ruta_y_nombre    VARCHAR(100)
  DEFINE v_reporte_duplicado VARCHAR(100)  
  
  DEFINE lr_afi_mae          RECORD LIKE afi_mae_afiliado.* 
  DEFINE lr_afiliacion       RECORD 
                               imss_issste    SMALLINT,   
                               independiente  SMALLINT
                             END RECORD 
 
  DEFINE lr_reg              RECORD 
                               id_01_tip_reg   CHAR(02), --02
                               id_02_conta     CHAR(10), --consecutivo
                               id_03_clave     CHAR(02), --20
                               id_04_nss       CHAR(11), --
                               id_05_curp      CHAR(18), --
                               id_06_rfc       CHAR(13), --XXXX999999XXX
                               id_07_paterno   CHAR(40), --NO BLANCOS
                               id_08_materno   CHAR(40), --NO BLANCOS o N/A
                               id_09_nombre    CHAR(40), --NO BLANCOS
                               id_10_fec_nac   CHAR(08), --AAAAMMDD
                               id_11_sexo      CHAR(01), --1=M, 2=F
                               id_12_ent_nac   CHAR(02), --
                               id_13_nacion    CHAR(03), --
                               id_14_fec_act   CHAR(08), --AAAAMMDD
                               id_15_tip_afi   CHAR(01), --2, 4 o 5
                               id_16_cla_afo   CHAR(03), --568 COPPEL
                               id_17_mar_reg   CHAR(04), --
                               id_18_ori_act   CHAR(01), --1,2,3,4
                               id_19_res_afo   CHAR(02) --01
                             END RECORD 
                             
  --------------------------------
  DATABASE safre_tmp
  --------------------------------

  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_det_activa_menores
  WHENEVER ERROR STOP

  CREATE TABLE tmp_det_activa_menores
  ( 
   linea CHAR(220)
  )

  --------------------------------
  
  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_reg_activa_menores
  WHENEVER ERROR STOP

  CREATE TABLE tmp_reg_activa_menores
  ( 
    id_01_tip_reg   CHAR(02), 
    id_02_conta     CHAR(10),
    id_03_clave     CHAR(02),
    id_04_nss       CHAR(11),
    id_05_curp      CHAR(18),
    id_06_rfc       CHAR(13),
    id_07_paterno   CHAR(40),
    id_08_materno   CHAR(40),
    id_09_nombre    CHAR(40),
    id_10_fec_nac   CHAR(08),
    id_11_sexo      CHAR(01),
    id_12_ent_nac   CHAR(02),
    id_13_nacion    CHAR(03),
    id_14_fec_act   CHAR(08),
    id_15_tip_afi   CHAR(01),
    id_16_cla_afo   CHAR(03),
    id_17_mar_reg   CHAR(04),
    id_18_ori_act   CHAR(01),
    id_19_res_afo   CHAR(02)
  )

  --------------------------------
  
  WHENEVER ERROR CONTINUE
    DROP TABLE tmp_reporte_rechazos_menores
  WHENEVER ERROR STOP

  CREATE TABLE tmp_reporte_rechazos_menores
  ( 
   orden     SMALLINT,
   registro  VARCHAR(18),
   curp      VARCHAR(18),
   paterno   VARCHAR(40),
   materno   VARCHAR(40),
   nombre    VARCHAR(40),
   rechazo   VARCHAR(60)
  )
  
  --------------------------------
  DATABASE safre_af
  --------------------------------

  -- BORRAR ARCHIVOS ANTERIORES                  
  -- LET ejecuta = "rm ",v_ruta_rescate CLIPPED,"/det_activa_menores* > ",v_ruta_rescate CLIPPED,"/basura"
  -- DISPLAY ejecuta
  -- RUN ejecuta
  
  LET ejecuta = "chmod 777 ",v_ruta_rescate CLIPPED,"/basura"
  -- DISPLAY ejecuta
  RUN ejecuta
  
  -- RENOMBRAR EL ARCHIVO INDICADO AL NOMBRE UTILIZADO EN EL PROGRAMA
  LET ejecuta = "cp ",v_ruta_rescate CLIPPED,"/", v_arch CLIPPED,"  ",v_ruta_rescate CLIPPED,"/det_activa_menores > basura"
  -- DISPLAY ejecuta
  RUN ejecuta
  
  LET ejecuta = "chmod 777 ",v_ruta_rescate CLIPPED,"/basura"
  -- DISPLAY ejecuta
  RUN ejecuta    
  
  -- LO QUE SE HACE ES CARGAR EL ARCHIVO EN LA TABLA CREADA
  LET ejecuta = "dbaccess safre_tmp ",v_ruta_exp CLIPPED,"/sube_activa_menores.sql"
  -- DISPLAY ejecuta
  RUN ejecuta 
  
  -----------------------------------------------------------------------------
  
  LET ls_resultado    = 0      -- VALIDACION DE CARGA DEL ARCHIVO
  LET ls_reg_rechazos = 1      -- SE ASIGNA 1 POR DEFAULT COMO CANDADO 
  LET ls_cargados     = 0      -- REGISTROS CARGADOS EN LA TABLA TEMPORAL
  
  LET ls_encabezado   = 0 
  LET ls_sumario      = 0 
  
  LET ls_servicio     = 0 
  LET lc_servicio     = ""
  
  LET ls_operacion    = 0 
  LET lc_operacion    = ""
  
  LET ls_duplicados   = 0      -- VALIDACION DE REGISTROS DUPLICADOS 
  
  -----------------------------------------------------------------------------

  -- VALIDAR QUE TENGA ENCABEZADO
  SELECT COUNT(*) INTO ls_encabezado
    FROM safre_tmp:tmp_det_activa_menores 
   WHERE linea[01,02] = "01" 
   
  IF ls_encabezado = 1 THEN 
  	
  	 SELECT linea[03,04], linea[05,06], linea[17,24] 
  	   INTO lc_servicio, lc_operacion, lc_fec_transfe 
       FROM safre_tmp:tmp_det_activa_menores 
      WHERE linea[01,02] = "01" 
      
     IF lc_servicio  = "01" THEN LET ls_servicio  = 1 END IF 
     IF lc_operacion = "20" THEN LET ls_operacion = 1 END IF  
     
     LET lv_fec_transfe = lc_fec_transfe[05,06],"/", 
                          lc_fec_transfe[07,08],"/", 
                          lc_fec_transfe[01,04]
                          
     --  FECHA DE TRANSFERENCIA DE LOTE                      
     LET ld_fec_transfe = lv_fec_transfe                      
                          
  END IF  
     
  -- VALIDAR QUE TENGA SUMARIO
  SELECT COUNT(*) INTO ls_sumario
    FROM safre_tmp:tmp_det_activa_menores 
   WHERE linea[01,02] = "09" 
  
  -- VER CUANTOS DETALLE DE REGISTROS SE CARGARON                    
  SELECT COUNT(*) INTO ls_cargados
    FROM safre_tmp:tmp_det_activa_menores 
   WHERE linea[01,02] = "02" 
  
  CASE 
    WHEN ls_encabezado = 0 
    
         OPEN WINDOW wauxa1 AT v_ren,v_col WITH 6 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
           DISPLAY  '                                                              ' AT 2,2
           DISPLAY  '   EL ARCHIVO NO TIENE ENCABEZADO, VERIFIQUE ARCHIVO.         ' AT 3,2
           DISPLAY  '                                                              ' AT 4,2
           PROMPT " PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR lc_pausa
         CLOSE WINDOW wauxa1
         LET ls_resultado = 1
         
    WHEN ls_sumario = 0 
    
         OPEN WINDOW wauxb1 AT v_ren,v_col WITH 6 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
           DISPLAY  '                                                              ' AT 2,2
           DISPLAY  '   EL ARCHIVO NO TIENE SUMARIO, VERIFIQUE ARCHIVO.            ' AT 3,2
           DISPLAY  '                                                              ' AT 4,2
           PROMPT " PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR lc_pausa
         CLOSE WINDOW wauxb1
         LET ls_resultado = 1
  
    WHEN ls_servicio = 0 
    
         OPEN WINDOW wauxc1 AT v_ren,v_col WITH 6 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
           DISPLAY  '                                                              ' AT 2,2
           DISPLAY  ' IDENTIFICADOR DE SERVICIO INCORRECTO, VERIFIQUE ARCHIVO.     ' AT 3,2
           DISPLAY  '                                                              ' AT 4,2
           PROMPT " PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR lc_pausa
         CLOSE WINDOW wauxc1
         LET ls_resultado = 1
         
    WHEN ls_operacion = 0 
    
         OPEN WINDOW wauxd1 AT v_ren,v_col WITH 6 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
           DISPLAY  '                                                              ' AT 2,2
           DISPLAY  ' IDENTIFICADOR DE OPERACION INCORRECTO, VERIFIQUE ARCHIVO.    ' AT 3,2
           DISPLAY  '                                                              ' AT 4,2
           PROMPT " PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR lc_pausa
         CLOSE WINDOW wauxd1
         LET ls_resultado = 1

    OTHERWISE
    
         -- CARGAR EL ARCHIVO A TABLA TEMPORAL PARA REVISAR SUS REGISTROS 
         
         LET lr_afiliacion.imss_issste   = 0
         LET lr_afiliacion.independiente = 0
         
         DECLARE uno_cur CURSOR FOR 
           SELECT linea[001,002] AS id_01,   -- tipo_registro
                  linea[003,012] AS id_02,   -- contador de servicio
                  linea[013,014] AS id_03,   -- clave de la operacion
                  linea[015,025] AS id_04,   -- nss 
                  linea[026,043] AS id_05,   -- curp
                  linea[044,056] AS id_06,   -- rfc
                  linea[057,096] AS id_07,   -- patero
                  linea[097,136] AS id_08,   -- materno
                  linea[137,176] AS id_09,   -- nombres 
                  linea[177,184] AS id_10,   -- fecha de nacimiento 
                  linea[185,185] AS id_11,   -- sexo
                  linea[186,187] AS id_12,   -- entidad de nacimiento 
                  linea[188,190] AS id_13,   -- nacionalidad 
                  linea[191,198] AS id_14,   -- fecha de activacion
                  linea[199,199] AS id_15,   -- tipo de afiliacion
                  linea[200,202] AS id_16,   -- clave de afore de afiliacion 
                  linea[203,206] AS id_17,   -- marca de regimen pensionario
                  linea[207,207] AS id_18,   -- origen de la activacion
                  linea[208,209] AS id_19    -- respuesta de la afore 
             FROM safre_tmp:tmp_det_activa_menores 
            WHERE linea[01,02] = "02"
             
         FOREACH uno_cur INTO lr_reg.*
          
           INSERT INTO safre_tmp:tmp_reg_activa_menores VALUES(lr_reg.*) 
            
           -- VER QUE EN EL ARCHIVO SOLO VENGA TIPO DE AFILIACION(5) 
           -- O QUE EN EL ARCHIVO VENGAN TIPO DE AFILIACION (2 y 4)            
           CASE 
              WHEN lr_reg.id_15_tip_afi = '2'
                   LET lr_afiliacion.imss_issste   = lr_afiliacion.imss_issste   + 1 
                   
              WHEN lr_reg.id_15_tip_afi = '4'
                   LET lr_afiliacion.imss_issste   = lr_afiliacion.imss_issste   + 1  
                   
              WHEN lr_reg.id_15_tip_afi = '5'
                   LET lr_afiliacion.independiente = lr_afiliacion.independiente + 1
           END CASE 
  
         END FOREACH  
         FREE uno_cur 

         -- INICIA REVISION DE REGISTROS DUPLICADOS 
         WHENEVER ERROR CONTINUE
           DROP TABLE tmp_reg_activa_duplicados
         WHENEVER ERROR STOP
          
         SELECT id_05_curp, COUNT(*) AS total
           FROM safre_tmp:tmp_reg_activa_menores
          GROUP BY 1
         HAVING COUNT(*) > 1
         INTO TEMP tmp_reg_activa_duplicados
          
         SELECT COUNT(*) INTO ls_duplicados
           FROM safre_tmp:tmp_reg_activa_duplicados
            
         IF ls_duplicados > 0 THEN 

         	 LET lc_comando = '', 
         	                  '\n SELECT FIRST 1 id_05_curp                    ',
         	                  '\n   FROM safre_tmp:tmp_reg_activa_duplicados   '
         	 PREPARE reg_dup FROM lc_comando
         	 EXECUTE reg_dup INTO lc_curp_duplicado
         	 
         	 CALL genera_duplicados()
         	      RETURNING v_reporte_duplicado
         	
           OPEN WINDOW wauxe1 AT v_ren,v_col WITH 6 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
             DISPLAY  ' EXISTEN REGISTROS DUPLICADOS, VERIFIQUE ARCHIVO DE CARGA.    ' AT 2,2
             DISPLAY  ' SE GENERO EL SIGUIENTE REPORTE INDICANDO LOS CURPS DUPLICADOS' AT 3,2
             DISPLAY  ' ',v_reporte_duplicado,''                                       AT 4,2 
             PROMPT " PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR lc_pausa
           CLOSE WINDOW wauxe1
           LET ls_resultado = 1
                    	
         END IF 
         -- TERMINA REVISION DE REGISTROS DUPLICADOS 
          
         -- INICIA VALIDAR QUE EN EL ARCHIVO VENGAN AFILIACIONES (2 y 4) o SOLO ( 5 )           
         CASE 
            WHEN (lr_afiliacion.imss_issste > 0) AND (lr_afiliacion.independiente = 0)
                 -- SE ACEPTA ARCHIVO
            WHEN (lr_afiliacion.imss_issste = 0) AND (lr_afiliacion.independiente > 0)
                 -- SE ACEPTA ARCHIVO
            OTHERWISE      
                 -- NO SE ACEPTA EL ARCHIVO 
                 OPEN WINDOW wauxf1 AT v_ren,v_col WITH 6 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
                   DISPLAY  ' NO SE ACEPTA EL ARCHIVO, POR LOS TIPOS DE AFILIACION         ' AT 2,2
                   DISPLAY  ' SOLO PUEDE TENER REGISTROS DE AFILIACION (5) EN UN ARCHIVO o ' AT 3,2
                   DISPLAY  ' AFILICACION (2 y 4), PERO NO COMBINADOS AL MISMO TIEMPO      ' AT 4,2
                   PROMPT " PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR lc_pausa
                 CLOSE WINDOW wauxf1
                 LET ls_resultado = 1          
         END CASE 
         -- TERMINA VALIDAR QUE EN EL ARCHIVO VENGAN AFILIACIONES (2 y 4) o SOLO ( 5 )   
          
  END CASE 
  
  -- SI NO HUBO PROBLEMA CON LAS VALICACIONES ANTERIORES
  -- INICIA VALIDACION DE LOS REGISTROS DETALLE 
  IF ls_resultado = 0 THEN 
  	  	
  	 -- DISPLAY "ENTRE A ls_resultado = 0" 	  	  	
     LET vtot_rme_locali = 0       -- LOCALIZADOS 
     LET vtot_rme_noloca = 0       -- NO LOCALIZADOS
     LET vtot_rme_acepta = 0       -- ACEPTADOS 
     LET vtot_rme_rechaz = 0       -- RECHAZADOS 
     LET vtot_rme_regist = 0       -- TOTAL 
     LET ls_reg_rechazos = 0       -- SE CAMBIA A 0 EL VALOR  
  	
  	 DECLARE dos_cur CURSOR FOR 
       SELECT * 
         FROM safre_tmp:tmp_reg_activa_menores
              
  	 FOREACH dos_cur INTO lr_reg.*
  	 
  	   LET lc_rechazo = ""
  	 
  	   CALL rechaza_registro(lr_reg.*)
  	        RETURNING lc_rechazo 
  	        
  	   IF lc_rechazo = "" OR lc_rechazo IS NULL THEN      

          LET ls_tiene_curp     = 0    -- VALIDAR CURP 
          LET lc_curp_validando = ""
          LET ls_curp_longitud  = 0 
                 
          LET ls_tiene_nss      = 0    -- VALIDAR NSS 
          LET lc_nss_validando  = ""
          LET ls_nss_longitud   = 0
                 
          LET ls_tiene_fec_act  = 0    -- FECHA DE ACTIVACION
          LET lc_feac_validando = ""
          LET ls_feac_longitud  = 0 
                 
          LET ls_tiene_reg_pen  = 0    -- REGIMEN PENSIONARIO
          LET ls_exi_reg_pen    = 0 
          LET lc_repe_validando = ""
          LET ls_repe_longitud  = 0
          
          LET ls_exi_curp_bd    = 0    -- EXISTE CURP EN AFI_MAE_AFILIADO
          LET ls_tip_sol_val    = 0    -- TIPO SOLICITUD DEL REGISTRO VALIDA
          
          LET ls_reg_aceptado   = 0    -- ACEPTAR O RECHAZAR EL REGISTRO
          LET lc_rechazo        = ""   -- MOTIVO DE RECHAZO 
   	      
  	      -- VALIDAR CURP 
  	      LET lc_curp_validando = lr_reg.id_05_curp CLIPPED 
  	      -- DISPLAY lc_curp_validando
  	      LET ls_curp_longitud  = LENGTH(lc_curp_validando)
  	      IF ls_curp_longitud   = 18 THEN 
  	      	  LET ls_tiene_curp = 1 
  	      END IF 	   
          
          -- VALIDAR NSS 
          LET lc_nss_validando = lr_reg.id_04_nss CLIPPED
          LET ls_nss_longitud  = LENGTH(lc_nss_validando)
          IF ls_nss_longitud = 11 THEN 
          	 LET ls_tiene_nss = 1 
          END IF 
          
          -- VALIDAR FECHA DE ACTIVACION 
          LET lc_feac_validando = lr_reg.id_14_fec_act CLIPPED 
          LET ls_feac_longitud = LENGTH(lc_feac_validando)
          IF ls_feac_longitud = 8 THEN 
          	 LET ls_tiene_fec_act = 1 
          END IF
          
          -- VALIDAR REGIMEN PENSIONARIO
          LET lc_repe_validando = lr_reg.id_17_mar_reg CLIPPED
          LET ls_repe_longitud = LENGTH(lc_repe_validando)  
          
          IF ls_repe_longitud = 4 THEN 
          	
          	  LET ls_tiene_reg_pen = 1 
          	  -- REVISAR QUE SEA UNA MARCA DE REGIMEN PENSIONARIO VALIDA 
          	  LET lc_comando = '', 
          	                   '\n SELECT COUNT(*)                                 ',
          	                   '\n   FROM safre_af:tab_mar_reg_pension             ',
          	                   '\n  WHERE id_reg_pension = "',lc_repe_validando,'" ',
          	                   '\n    AND ind_activo     = 1                       '
             PREPARE com_uno FROM lc_comando
             EXECUTE com_uno INTO ls_exi_reg_pen
             
             IF ls_exi_reg_pen = 0 THEN 
                LET ls_tiene_reg_pen = 0    -- NO ES UNA MARCA VALIDA 
             END IF    
             
          END IF   
          
          -- IDENTIFICAR EL TIPO DE AFILIACION (2,4,5)
          LET lc_tipo_afil = lr_reg.id_15_tip_afi  
          
          -- VALIDAR SI LA CURP EXISTE EN AFI_MAE_AFILIADO
          IF ls_tiene_curp = 1 THEN        
          	
             LET lc_comando = '',
                              '\n SELECT FIRST 1 *                         ',
                              '\n   FROM safre_af:afi_mae_afiliado         ',
                              '\n  WHERE n_unico = "',lc_curp_validando,'" '
                              
             -- DISPLAY lc_comando CLIPPED
             
             PREPARE com_dos FROM lc_comando
             EXECUTE com_dos INTO lr_afi_mae.* 
             
             IF SQLCA.SQLCODE = NOTFOUND THEN 
             	
             	  LET vtot_rme_noloca = vtot_rme_noloca + 1        -- NO LOCALIZADO
                LET ls_exi_curp_bd  = 0 
                LET ls_tip_sol_val  = 0
                
             ELSE
             
                LET vtot_rme_locali = vtot_rme_locali + 1        -- LOCALIZADO     
                LET ls_exi_curp_bd  = 1 
                                                          
                -- VALIDR EL TIPO DE SOLICITUD, ACORDE AL TIPO DE AFILIACION 
                LET ls_tip_sol_val  = 0 
                CASE 
                   WHEN lc_tipo_afil = '5' AND lr_afi_mae.tipo_solicitud = 26  
                        LET ls_tip_sol_val = 1 
                        
                   WHEN (lc_tipo_afil = '2' OR lc_tipo_afil = '4') AND 
                        (lr_afi_mae.tipo_solicitud = 26 OR lr_afi_mae.tipo_solicitud = 32)
                        LET ls_tip_sol_val = 1 
                END CASE         
                
             END IF 
             
             -------------------------------
             -- INICIA VALIDACION DE REGISTROS EN BASE A LAS TABLAS DE CASOS VARIABLES 
             -------------------------------
             -- YA SE VALIDO QUE TENGA CURP DE 18 POSICIONES 
             -------------------------------
             
             CASE
               -- QUE SEA TRABAJADOR INDEPENDIENTE  
               WHEN (lc_tipo_afil = '5')             
                    CASE
                      -- CASO 1
                      WHEN (ls_tiene_nss   = 0) AND (ls_tiene_fec_act = 1) AND 
                           (ls_exi_curp_bd = 1) AND (ls_tip_sol_val   = 1) 
                           LET vtot_rme_acepta = vtot_rme_acepta + 1 
                           LET lc_rechazo = ""
                      -- CASO 2                        
                      WHEN (ls_tiene_nss   = 0) AND (ls_tiene_fec_act = 1) AND 
                           (ls_exi_curp_bd = 1) AND (ls_tip_sol_val   = 0) 
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1                         
                           LET lc_rechazo = "INDEPENDIENTE - TIPO SOLICITUD DIFERENTE A 26"
                      -- CASO 3                        
                      WHEN (ls_tiene_nss   = 0) AND (ls_tiene_fec_act = 1) AND 
                           (ls_exi_curp_bd = 0) 
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1                         
                           LET lc_rechazo = "INDEPENDIENTE - NO EXISTE CURP EN MAESTRO DE AFILIADOS"
                      -- CASO 4                        
                      WHEN (ls_tiene_nss   = 0) AND (ls_tiene_fec_act = 0)
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1    
                           LET lc_rechazo = "INDEPENDIENTE - NO TIENE FECHA DE ACTIVACION"
                      -- CASO 5 
                      WHEN (ls_tiene_nss   = 1)
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1     
                           LET lc_rechazo = "INDEPENDIENTE - EL REGISTRO TRAE NSS"
                      OTHERWISE 
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1     
                           LET lc_rechazo = "INDEPENDIENTE - NO CUMPLE CONDICIONES"                             
                    END CASE
          
               -- TRABAJADOR IMSS 
               WHEN (lc_tipo_afil = '2')
                    CASE 
                      -- CASO 1 
                      WHEN (ls_tiene_nss   = 1) AND (ls_tiene_fec_act = 1) AND 
                           (ls_exi_curp_bd = 1) AND (ls_tip_sol_val   = 1) 
                           LET vtot_rme_acepta = vtot_rme_acepta + 1 
                           LET lc_rechazo = ""
                      -- CASO 2 
                      WHEN (ls_tiene_nss   = 1) AND (ls_tiene_fec_act = 1) AND 
                           (ls_exi_curp_bd = 1) AND (ls_tip_sol_val   = 0) 
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "IMSS - TIPO SOLICITUD DIFERENTE A 26 o 32"
                      -- CASO 3 
                      WHEN (ls_tiene_nss   = 1) AND (ls_tiene_fec_act = 1) AND 
                           (ls_exi_curp_bd = 0)
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "IMSS - NO EXISTE CURP EN MAESTRO DE AFILIADOS"
                      -- CASO 4 
                      WHEN (ls_tiene_nss   = 1) AND (ls_tiene_fec_act = 0)
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "IMSS - NO TIENE FECHA DE ACTIVACION"
                      -- CASO 5 
                      WHEN (ls_tiene_nss   = 0)
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "IMSS - EL REGISTRO NO TRAE NSS"
                      OTHERWISE 
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1     
                           LET lc_rechazo = "IMSS - NO CUMPLE CONDICIONES"      
                    END CASE 
          
               WHEN (lc_tipo_afil = '4')
                    CASE 
                      -- CASO 1 
                      WHEN (ls_tiene_nss   = 1)
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "ISSSTE - EL REGISTRO TRAE NSS"
                      -- CASO 2 
                      WHEN (ls_tiene_nss     = 0) AND (ls_tiene_fec_act = 1) AND 
                           (ls_tiene_reg_pen = 1) AND (ls_exi_curp_bd   = 1) AND 
                           (ls_tip_sol_val   = 1) 
                           LET vtot_rme_acepta = vtot_rme_acepta + 1 
                           LET lc_rechazo = ""
                      -- CASO 3 
                      WHEN (ls_tiene_nss     = 0) AND (ls_tiene_fec_act = 1) AND 
                           (ls_tiene_reg_pen = 1) AND (ls_exi_curp_bd   = 1) AND 
                           (ls_tip_sol_val   = 0) 
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "ISSSTE - TIPO SOLICITUD DIFERENTE A 26 o 32"
                      -- CASO 4 
                      WHEN (ls_tiene_nss     = 0) AND (ls_tiene_fec_act = 1) AND 
                           (ls_tiene_reg_pen = 1) AND (ls_exi_curp_bd   = 0)
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "ISSSTE - NO EXISTE CURP EN MAESTRO DE AFILIADOS"
                      -- CASO 5 
                      WHEN (ls_tiene_nss     = 0) AND (ls_tiene_fec_act = 1) AND 
                           (ls_tiene_reg_pen = 0) 
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "ISSSTE - NO TIENE MARCA DE REGIMEN PENSIONARIO O ES INVALIDA"
                      -- CASO 6
                      WHEN (ls_tiene_nss     = 0) AND (ls_tiene_fec_act = 0)
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                           LET lc_rechazo = "ISSSTE - NO TIENE FECHA DE ACTIVACION"                        
                      OTHERWISE 
                           LET vtot_rme_rechaz = vtot_rme_rechaz + 1     
                           LET lc_rechazo = "ISSSTE - NO CUMPLE CONDICIONES" 
                    END CASE 
          
               OTHERWISE 
                  LET vtot_rme_rechaz = vtot_rme_rechaz + 1 
                  LET lc_rechazo = "TIPO DE AFILIACION NO VALIDA"
             
             END CASE 
                       
             -------------------------------
             -- TERMINA VALIDACION DE REGISTROS EN BASE A LAS VARIABLES 
             -------------------------------
             
          ELSE 
          
             LET vtot_rme_noloca = vtot_rme_noloca + 1           -- NO LOCALIZADO       
             LET vtot_rme_rechaz = vtot_rme_rechaz + 1           -- RECHAZADO
             LET lc_rechazo = "LA CURP DEL REGISTRO, NO ES VALIDA"
             
          END IF
          -- ls_tiene_curp
       
       ELSE -- rechaza_registro(lr_reg.*)
         
          LET vtot_rme_rechaz = vtot_rme_rechaz + 1           -- RECHAZADO
          
  	      -- VALIDAR CURP 
  	      LET lc_curp_validando = lr_reg.id_05_curp CLIPPED 
  	      LET ls_curp_longitud = LENGTH(lc_curp_validando)
  	      
  	      IF ls_curp_longitud = 18 THEN 
             -- VER SI EXISTE EN AFI_MAE_AFILIADO
             LET lc_comando = '',
                              '\n SELECT FIRST 1 *                         ',
                              '\n   FROM safre_af:afi_mae_afiliado         ',
                              '\n  WHERE n_unico = "',lc_curp_validando,'" ',
                              '\n    AND tipo_solicitud IN (26,32)         '
             
             PREPARE com_tres FROM lc_comando
             EXECUTE com_tres INTO lr_afi_mae.* 
             
             IF SQLCA.SQLCODE = NOTFOUND THEN 
                LET vtot_rme_noloca = vtot_rme_noloca + 1        -- NO LOCALIZADO
             ELSE
                LET vtot_rme_locali = vtot_rme_locali + 1        -- LOCALIZADO     
             END IF           
          ELSE 
             LET vtot_rme_noloca = vtot_rme_noloca + 1        -- NO LOCALIZADO
  	      END IF 
       
       END IF 
       -- rechaza_registro(lr_reg.*)
       
       -- GENERACION DEL REPORTE DE REGISTROS RECHAZADOS o NO LOCALIZADOS
       IF lc_rechazo = "" OR lc_rechazo IS NULL THEN 
       	  -- REGISTRO ACEPTADO
       ELSE         
          LET ls_reg_rechazos = ls_reg_rechazos + 1  
          LET lc_reg_rechazos = ls_reg_rechazos
                                   
          IF ls_reg_rechazos = 1 THEN           	   	
       	     -- INSERTAR LOS TITULOS DEL REPORTE 
             INSERT INTO safre_tmp:tmp_reporte_rechazos_menores VALUES 
             (1,"REGISTRO","CURP","APELLIDO PATERNO","APELLIDO MATERNO","NOMBRE","DESCRIPCION DEL RECHAZO");         
             INSERT INTO safre_tmp:tmp_reporte_rechazos_menores VALUES 
             (2,lc_reg_rechazos,      lr_reg.id_05_curp,   lr_reg.id_07_paterno, 
                lr_reg.id_08_materno, lr_reg.id_09_nombre, lc_rechazo);             
          ELSE
       	     -- INSERTAR REGISTROS RECHAZADOS POR ALGUN MOTIVO
             INSERT INTO safre_tmp:tmp_reporte_rechazos_menores VALUES 
             (2,lc_reg_rechazos,      lr_reg.id_05_curp,   lr_reg.id_07_paterno, 
                lr_reg.id_08_materno, lr_reg.id_09_nombre, lc_rechazo);               
          END IF                
            
       END IF 
       -- TERMINA GENERACION DEL REPORTE DE REGISTROS RECHAZADOS o NO LOCALIZADOS
     
     END FOREACH 
     FREE dos_cur
     
     -- DISPLAY "REGISTROS CARGADOS ",ls_cargados
     LET vtot_rme_regist = vtot_rme_acepta + vtot_rme_rechaz
     
     -- MOSTRAR CIFRAS RESULTADO DE LA CARGA DEL ARCHIVO 
     DISPLAY BY NAME vtot_rme_locali
     DISPLAY BY NAME vtot_rme_noloca
     DISPLAY BY NAME vtot_rme_acepta
     DISPLAY BY NAME vtot_rme_rechaz
     DISPLAY BY NAME vtot_rme_regist
     
  END IF 
  
  -----------------------------------------------------------------------------   
  -- TERMINA VALIDACION DE LOS REGISTROS DETALLE 
  -----------------------------------------------------------------------------   
  
  ---------------------------
  DATABASE safre_af
  ---------------------------
  -- DISPLAY " ls_resultado    = ",ls_resultado,"<"
  -- DISPLAY " ls_reg_rechazos = ",ls_reg_rechazos,"<"
  
  CASE 
    WHEN ls_resultado = 0 AND ls_reg_rechazos = 0
  	    
  	     -- GENERAR FOLIO GLOBAL  	     
         LET lc_comando = "EXECUTE FUNCTION fn_obten_glo_folio()"
         PREPARE eje_glo_folio FROM lc_comando 
         EXECUTE eje_glo_folio INTO li_folio_lote  
  	     
  	     LET l_folio = li_folio_lote
  	     DISPLAY l_folio TO li_folio_lote   
  	     PROMPT " SE MUESTRAN CIFRAS Y FOLIO... <ENTER> PARA CONTINUAR " FOR lc_pausa
  	       
  	     -- INSERTAR A TABLA DEFINITIVA LOS REGISTROS 
  	     DECLARE tres_cur CURSOR FOR 
           SELECT * FROM safre_tmp:tmp_reg_activa_menores
                  
  	     FOREACH tres_cur INTO lr_reg.*
           CALL inserta_registro(li_folio_lote, ld_fec_transfe, lr_reg.*)
  	     END FOREACH 
  	     FREE tres_cur
  	     
         OPEN WINDOW wauxh1 AT v_ren,v_col WITH 6 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
            DISPLAY  '  SE HA CARGADO EL ARCHIVO ',v_arch CLIPPED,''                           AT 2,2
            DISPLAY  '  CON ',ls_cargados USING "<<<<<",' REGISTROS'                           AT 3,2
            DISPLAY  '                                                              '          AT 4,2
            PROMPT " PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR lc_pausa
         CLOSE WINDOW wauxh1
         LET ls_resultado = 0
       	     
    WHEN ls_resultado = 1 AND ls_reg_rechazos = 0 
    
         PROMPT " SE MUESTRAN CIFRAS... <ENTER> PARA CONTINUAR " FOR lc_pausa
         -- NO MOSTRAR MAS MENSAJES y REGRESAR AL MENU 
     
    WHEN ls_resultado = 0 AND ls_reg_rechazos > 0 
   
         PROMPT " SE MUESTRAN CIFRAS... <ENTER> PARA CONTINUAR " FOR lc_pausa
   
         -- GENERAR REPORTE DE RECHAZON Y DAR AVISO AL USUARIO  
         LET lc_nombre_reporte = "Reporte_Rechazos_Activacion_Menores.txt" 
         LET lc_ruta_y_nombre  = v_ruta_listados CLIPPED,"/",lc_nombre_reporte CLIPPED
         
         -- REALZAR LA DESCARGA DE REGISTROS DE LA TABLA DE RECHAZOS 
         UNLOAD TO lc_ruta_y_nombre
         SELECT registro, curp, TRIM(paterno), TRIM(materno), TRIM(nombre), TRIM(rechazo)
           FROM safre_tmp:tmp_reporte_rechazos_menores
          ORDER BY orden  
              
         OPEN WINDOW wauxg01 AT v_ren,v_col WITH 7 rows, 68 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST )
            DISPLAY  "NO SE CARGO EL ARCHIVO Y SE GENERO EL SIGUIENTE REPORTE       " AT 2,2
            DISPLAY  "RUTA      : ",v_ruta_listados CLIPPED                             AT 3,2
            DISPLAY  "NOMBRE    : ",lc_nombre_reporte CLIPPED                         AT 4,2
            DISPLAY  "REGISTROS : ",ls_reg_rechazos                                   AT 5,2
            PROMPT " PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR lc_pausa        
         CLOSE WINDOW wauxg01
         LET ls_resultado = 1
     
  END CASE

  ---------------------------
  DATABASE safre_af
  ---------------------------

  RETURN ls_resultado
    
END FUNCTION

-------------------------------------------------------------------------------

FUNCTION rechaza_registro(pr_reg)

  DEFINE pr_reg             RECORD 
                              id_01_tip_reg   CHAR(02), --02
                              id_02_conta     CHAR(10), --consecutivo
                              id_03_clave     CHAR(02), --20
                              id_04_nss       CHAR(11), --
                              id_05_curp      CHAR(18), --
                              id_06_rfc       CHAR(13), --XXXX999999XXX
                              id_07_paterno   CHAR(40), --NO BLANCOS
                              id_08_materno   CHAR(40), --NO BLANCOS o N/A
                              id_09_nombre    CHAR(40), --NO BLANCOS
                              id_10_fec_nac   CHAR(08), --AAAAMMDD
                              id_11_sexo      CHAR(01), --1=M, 2=F
                              id_12_ent_nac   CHAR(02), --
                              id_13_nacion    CHAR(03), --
                              id_14_fec_act   CHAR(08), --AAAAMMDD
                              id_15_tip_afi   CHAR(01), --2, 4 o 5
                              id_16_cla_afo   CHAR(03), --568 COPPEL
                              id_17_mar_reg   CHAR(04), --
                              id_18_ori_act   CHAR(01), --1,2,3,4
                              id_19_res_afo   CHAR(02) --01
                            END RECORD 
                            
  DEFINE ls_continua        SMALLINT 
                            
  DEFINE lv_paterno         VARCHAR(40)
  DEFINE lv_materno         VARCHAR(40)
  DEFINE lv_nombres         VARCHAR(40)
  DEFINE lv_origen          CHAR(1) 
  
  DEFINE lv_fecha           VARCHAR(8)
  DEFINE ls_lon_fec         SMALLINT 
  
  DEFINE lv_ano             VARCHAR(04)
  DEFINE lv_mes             VARCHAR(02)
  DEFINE lv_dia             VARCHAR(02)
  
  DEFINE ls_ano             SMALLINT 
  DEFINE ls_mes             SMALLINT 
  DEFINE ls_dia             SMALLINT 
  
  DEFINE ls_paterno_valido  SMALLINT 
  DEFINE ls_materno_valido  SMALLINT 
  DEFINE ls_nombres_valido  SMALLINT
  
  DEFINE ls_afore_valida    SMALLINT 
  DEFINE ls_afore_existe    SMALLINT
   
  DEFINE ls_origen_valido   SMALLINT 
  DEFINE ls_origen_existe   SMALLINT 
  
  DEFINE ls_entidad_valida  SMALLINT 
  DEFINE ls_entidad_existe  SMALLINT 
  
  DEFINE ls_nacion_valida   SMALLINT 
  DEFINE ls_nacion_existe   SMALLINT 
  
  DEFINE ls_afilia_valida   SMALLINT
  DEFINE ls_afilia_existe   SMALLINT 
  
  DEFINE lc_cadena          VARCHAR(60)
  
  ----------------------------------
  
  LET ls_continua        = 1 
  LET lc_cadena          = ""
  
  -- id_07 VALIDAR APELLIDO PATERNO
  IF ls_continua = 1 THEN  
     LET lv_paterno = pr_reg.id_07_paterno CLIPPED  
     IF lv_paterno = "" OR lv_paterno IS NULL THEN 
  	    LET lc_cadena = "APELLIDO PATERNO"  
  	    LET ls_continua = 0 
  	 END IF    
  END IF 	  
  
  -- id_08 VALIDAR APELLIDO MATERNO
  IF ls_continua = 1 THEN 
     LET lv_materno = pr_reg.id_08_materno CLIPPED 
     IF lv_materno = "" OR lv_materno IS NULL THEN 
  	    LET lc_cadena = "APELLIDO MATERNO"
  	    LET ls_continua = 0 
     END IF
  END IF    
  
  -- id_09 VALIDAR NOMBRE   	
  IF ls_continua = 1 THEN 
     LET lv_nombres = pr_reg.id_09_nombre CLIPPED
     IF lv_nombres = "" OR lv_nombres IS NULL THEN 
     	  LET lc_cadena = "NOMBRE(S)"
        LET ls_continua = 0 
     END IF 
  END IF 
  
  -- id_10 FECHA NACIMIENTO 	
  IF ls_continua = 1 THEN 
     LET lv_fecha = pr_reg.id_10_fec_nac CLIPPED 
     LET ls_lon_fec = LENGTH(lv_fecha)
     IF ls_lon_fec = 8 THEN 
          
  	    LET lv_ano = pr_reg.id_10_fec_nac[1,4]
  	    LET lv_mes = pr_reg.id_10_fec_nac[5,6]
  	    LET lv_dia = pr_reg.id_10_fec_nac[7,8]
  	    
  	    LET ls_ano = lv_ano
  	    LET ls_mes = lv_mes 
  	    LET ls_dia = lv_dia 
  	    
  	    IF (ls_ano >= 1900 AND ls_ano <= 2100) AND
  	       (ls_mes >= 1    AND ls_mes <= 12  ) AND 
  	       (ls_dia >= 1    AND ls_dia <= 31  ) THEN 
  	    ELSE 
        	 LET lc_cadena = "FECHA NACIMIENTO"
           LET ls_continua = 0    	
  	    END IF 
  	 ELSE 
        LET lc_cadena = "FECHA NACIMIENTO"
        LET ls_continua = 0    	
  	 END IF      	 
  END IF 	

  -- id_11 SEXO
  IF ls_continua = 1 THEN 
  	 IF pr_reg.id_11_sexo = "1" OR pr_reg.id_11_sexo = "2" THEN 
  	 ELSE 
     	  LET lc_cadena = "SEXO"
        LET ls_continua = 0 
  	 END IF
  END IF 
  
  -- id_12 ENTIDAD DE NACIMIENTO
  IF ls_continua = 1 THEN 
     LET ls_entidad_valida = pr_reg.id_12_ent_nac 
     SELECT COUNT(*) INTO ls_entidad_existe
       FROM safre_af:tab_estado
      WHERE estad_cod = ls_entidad_valida
     
     IF ls_entidad_existe = 0 THEN 
     	  LET lc_cadena = "ENTIDAD DE NACIMIENTO"
        LET ls_continua = 0 
     END IF
  END IF  
  
  -- id_13 NACIONALIDAD 
  IF ls_continua = 1 THEN 
     LET lc_comando = '',
                      '\n SELECT COUNT(*)                             ',
                      '\n   FROM safre_af:tab_pais                    ',
                      '\n  WHERE pais_cod = "',pr_reg.id_13_nacion,'" '
     PREPARE con_pais FROM lc_comando
     EXECUTE con_pais INTO ls_nacion_existe
     
     IF ls_nacion_existe = 0 THEN 
        LET lc_cadena = "NACIONALIDAD"
        LET ls_continua = 0 
     END IF
  END IF 

  -- id_14 FECHA ACTIVACION 
  IF ls_continua = 1 THEN  
     LET lv_fecha = pr_reg.id_14_fec_act CLIPPED 
     LET ls_lon_fec = LENGTH(lv_fecha)
     IF ls_lon_fec = 8 THEN 
          
  	    LET lv_ano = pr_reg.id_14_fec_act[1,4]
  	    LET lv_mes = pr_reg.id_14_fec_act[5,6]
  	    LET lv_dia = pr_reg.id_14_fec_act[7,8]
  	    
  	    LET ls_ano = lv_ano
  	    LET ls_mes = lv_mes 
  	    LET ls_dia = lv_dia 
  	    
  	    IF (ls_ano >= 1900 AND ls_ano <= 2100) AND
  	       (ls_mes >= 1    AND ls_mes <= 12  ) AND 
  	       (ls_dia >= 1    AND ls_dia <= 31  ) THEN 
  	    ELSE 
        	  LET lc_cadena = "FECHA ACTIVACION"
           LET ls_continua = 0    	
  	    END IF 
  	 ELSE 
        LET lc_cadena = "FECHA ACTIVACION"
        LET ls_continua = 0    	
  	 END IF      	 
  END IF 	

  -- id_15 TIPO DE AFILIACION   
  IF ls_continua = 1 THEN 
     LET ls_afilia_valida = pr_reg.id_15_tip_afi 
     SELECT COUNT(*) INTO ls_afilia_existe
       FROM safre_af:tab_tpo_afi_menor
      WHERE id_cat_tpo_afi = ls_afilia_valida
     
     IF ls_afilia_existe = 0 THEN 
     	  LET lc_cadena = "TIPO DE AFILIACION"
        LET ls_continua = 0 
     END IF 
  END IF 
  
  -- id_16 CLAVE AFORE    
  IF ls_continua = 1 THEN 
     LET ls_afore_valida = pr_reg.id_16_cla_afo 
     SELECT COUNT(*) INTO ls_afore_existe
       FROM safre_af:tab_afore
      WHERE afore_cod = ls_afore_valida
     
     IF ls_afore_existe = 0 THEN 
     	  LET lc_cadena = "CLAVE AFORE AFILIACION"
        LET ls_continua = 0 
     END IF 
  END IF   
  
  -- id_18 ORIGEN DE ACTIVACION  
  IF ls_continua = 1 THEN 
     LET ls_origen_valido = pr_reg.id_18_ori_act
     SELECT COUNT(*) INTO ls_origen_existe
       FROM safre_af:tab_origen_act_menor
      WHERE id_tpo_origen = ls_origen_valido  
      
     IF ls_origen_existe = 0 THEN
      	 LET lc_cadena = "ORIGEN DE ACTIVACION"
         LET ls_continua = 0 
     END IF 	     
  END IF 

  IF ls_continua = 0 THEN 
     LET lc_cadena = "EL DATO ",lc_cadena CLIPPED," NO ES VALIDO"
  END IF    
  
  RETURN lc_cadena   	

END FUNCTION 
  
-------------------------------------------------------------------------------

FUNCTION inserta_registro(pi_folio_lote, pd_fec_transfe, pr_reg)

  DEFINE pi_folio_lote      DECIMAL(10,0)
  DEFINE pd_fec_transfe     DATE 
  
  DEFINE pr_reg             RECORD 
                              id_01_tip_reg   CHAR(02), --02
                              id_02_conta     CHAR(10), --consecutivo
                              id_03_clave     CHAR(02), --20
                              id_04_nss       CHAR(11), --
                              id_05_curp      CHAR(18), --
                              id_06_rfc       CHAR(13), --XXXX999999XXX
                              id_07_paterno   CHAR(40), --NO BLANCOS
                              id_08_materno   CHAR(40), --NO BLANCOS o N/A
                              id_09_nombre    CHAR(40), --NO BLANCOS
                              id_10_fec_nac   CHAR(08), --AAAAMMDD
                              id_11_sexo      CHAR(01), --1=M, 2=F
                              id_12_ent_nac   CHAR(02), --
                              id_13_nacion    CHAR(03), --
                              id_14_fec_act   CHAR(08), --AAAAMMDD
                              id_15_tip_afi   CHAR(01), --2, 4 o 5
                              id_16_cla_afo   CHAR(03), --568 COPPEL
                              id_17_mar_reg   CHAR(04), --
                              id_18_ori_act   CHAR(01), --1,2,3,4
                              id_19_res_afo   CHAR(02)  --01
                            END RECORD 
  DEFINE lc_fecha           CHAR(10)
  DEFINE ld_fecha_nac       DATE 
  DEFINE ld_fecha_act       DATE 
  DEFINE lv_calle           VARCHAR(60)
  DEFINE lv_telefono        VARCHAR(40)   
  DEFINE lv_correo          VARCHAR(100)
  
  DEFINE ls_exi_afi_sol     SMALLINT    -- VALIDAR QUE NO EXISTA EL REGISTRO
  DEFINE ls_dom_cod         SMALLINT    -- PARA LOS TIPOS DE DOMICILIO
  DEFINE ls_tel_cod         SMALLINT    -- PARA LOS TIPOS DE TELEFONO
  DEFINE lr_afi_mae         RECORD LIKE afi_mae_afiliado.*
  DEFINE lr_afi_sol         RECORD LIKE afi_solicitud.*
  DEFINE lr_afi_dom         RECORD LIKE afi_domicilio.*
  DEFINE lr_afi_tel         RECORD LIKE afi_telefono.*
  DEFINE lr_afi_cor         RECORD LIKE afi_correo_elect.*
  DEFINE ld_id_act_menor    DECIMAL(10,0)
  
  DEFINE v_marca            SMALLINT 
  DEFINE v_codigo_rechazo   SMALLINT 
  DEFINE v_marca_causa      SMALLINT
  DEFINE v_fecha_causa      DATE 
  DEFINE v_correlativo      INTEGER     
  DEFINE v_estado_marca     SMALLINT 
   
  ----------------------
  -- FECHA DE NACIMIENTO 
  ----------------------
  
  LET lc_fecha     = pr_reg.id_10_fec_nac[05,06],"/",
                     pr_reg.id_10_fec_nac[07,08],"/",
                     pr_reg.id_10_fec_nac[01,04]
  LET ld_fecha_nac = lc_fecha
  
  ----------------------
  -- FECHA DE ACTIVACION 
  ----------------------
  
  LET lc_fecha     = pr_reg.id_14_fec_act[05,06],"/",
                     pr_reg.id_14_fec_act[07,08],"/",
                     pr_reg.id_14_fec_act[01,04]
  LET ld_fecha_act = lc_fecha
  
  -- CARGAR ARCHIVO A TABLAS DEFINITIVAS 
  INSERT INTO safre_af:afi_act_menor_edad VALUES 
  (
   seq_afi_act_menor_edad.NEXTVAL,
   pi_folio_lote                 ,
   pd_fec_transfe                ,
   0                             ,
   0                             ,
   ""                            ,
   pr_reg.id_01_tip_reg          ,
   pr_reg.id_02_conta            ,
   pr_reg.id_03_clave            ,
   pr_reg.id_04_nss              ,
   pr_reg.id_05_curp             ,
   pr_reg.id_06_rfc              ,
   pr_reg.id_07_paterno          ,
   pr_reg.id_08_materno          ,
   pr_reg.id_09_nombre           ,
   ld_fecha_nac                  ,
   pr_reg.id_11_sexo             ,
   pr_reg.id_12_ent_nac          ,
   pr_reg.id_13_nacion           ,
   ld_fecha_act                  ,
   pr_reg.id_15_tip_afi          ,
   pr_reg.id_16_cla_afo          ,
   pr_reg.id_17_mar_reg          ,
   pr_reg.id_18_ori_act          ,
   pr_reg.id_19_res_afo          ,
   0                             ,  -- edo_liquidacion 
   v_arch                        ,  -- nombre_archivo_carga
   ""                            ,  -- nombre_archivo_envio 
   10                            ,  -- edo_registro
   TODAY                         ,  -- fecha_carga
   ""                            ,  -- fecha_envio
   vusu                          
  )
  
  -- SI SE INSERTO CORRECTAMENTE, INSERTAR EN LAS DEMAS TABLAS 
  IF SQLCA.SQLERRD[3] > 0  THEN 

     -- AFI_SOLICITUD 
     -- AFI_DOMICILIO
     -- AFI_TELEFONO
     -- AFI_CORREO ELECT   
     
     -- DISPLAY "CURP :",pr_reg.id_05_curp    

     -- OBTENER VALOR UNICO DEL REGISTRO  
     LET ld_id_act_menor = 0      
     LET lc_comando = '',
                      '\n SELECT id_act_menor_edad                    ',
                      '\n   FROM safre_af:afi_act_menor_edad          ',
                      '\n  WHERE folio_lote =  ',pi_folio_lote,'      ',
                      '\n    AND curp       = "',pr_reg.id_05_curp,'" '
     PREPARE sel_id FROM lc_comando                    
     EXECUTE sel_id INTO ld_id_act_menor
  
     -- AFI_SOLICITUD 
     LET lc_comando = '',
                      '\n SELECT FIRST 1 *                         ',
                      '\n   FROM safre_af:afi_mae_afiliado         ',
                      '\n  WHERE n_unico = "',pr_reg.id_05_curp,'" ',
                      '\n    AND tipo_solicitud IN (26,32)         '
     PREPARE com_cuatro FROM lc_comando
     EXECUTE com_cuatro INTO lr_afi_mae.*
     
     IF SQLCA.SQLCODE = NOTFOUND THEN 
     	  -- NO INSERTAR EN AFI_SOLICITUD
     	  -- DISPLAY "NO SE INSERTO EN AFI_SOLICITUD, NO EXISTE AFI_MAE_AFILIADO"
     ELSE
     
        CALL asigna_valores(lr_afi_mae.*)
             RETURNING lr_afi_sol.* 

        LET lr_afi_sol.status_interno = 60 
        CASE 
          WHEN pr_reg.id_15_tip_afi = 2
               LET lr_afi_sol.tipo_solicitud = 33  -- IMSS  
               LET lr_afi_sol.n_seguro       = pr_reg.id_04_nss  
               
          WHEN pr_reg.id_15_tip_afi = 4
               LET lr_afi_sol.tipo_solicitud = 34  -- ISSSTE  
               
          WHEN pr_reg.id_15_tip_afi = 5
               LET lr_afi_sol.tipo_solicitud = 32  -- INDEPENDIENTE                 
        END CASE
           
        -- VERIFICAR QUE NO EXISTA EL REGISTRO PREVIAMENTE.        
        SELECT COUNT(*) 
          INTO ls_exi_afi_sol
          FROM safre_af:afi_solicitud
         WHERE n_folio         = lr_afi_sol.n_folio 
           AND tipo_solicitud  = lr_afi_sol.tipo_solicitud
           
        IF ls_exi_afi_sol > 0 THEN     
        	
        	 -- YA EXISTE EL REGISTRO, NO INSERTARLO 
        	 -- DISPLAY "YA EXISTE REGISTRO CON "
        	 -- DISPLAY "n_folio         ",lr_afi_sol.n_folio
        	 -- DISPLAY "tipo_solicitud  ",lr_afi_sol.tipo_solicitud
        	 -- DISPLAY "SE BORRA Y DE INSERTARA NUEVAMENTE"
 
           -- BORRAR DE afi_solicitud
           LET lc_comando = '',
                            '\n DELETE FROM safre_af:afi_solicitud                      ',
                            '\n  WHERE n_folio         =  ',lr_afi_sol.n_folio,'        ', 
                            '\n    AND tipo_solicitud  =  ',lr_afi_sol.tipo_solicitud,' '   
           PREPARE borra_sol FROM lc_comando
           EXECUTE borra_sol                 
              
           -- BORRAR DE afi_domicilio
           LET lc_comando = '',
                            '\n DELETE FROM safre_af:afi_domicilio                      ',
                            '\n  WHERE nss             = "',lr_afi_sol.n_seguro,'"      ',
                            '\n    AND n_folio         =  ',lr_afi_sol.n_folio,'        ',
                            '\n    AND tipo_solicitud  =  ',lr_afi_sol.tipo_solicitud,' '
           PREPARE borra_dom FROM lc_comando
           EXECUTE borra_dom                               
                                       
           -- BORRAR DE afi_telefono
           LET lc_comando = '',
                            '\n DELETE FROM safre_af:afi_telefono                       ',
                            '\n  WHERE nss             = "',lr_afi_sol.n_seguro,'"      ',
                            '\n    AND n_folio         =  ',lr_afi_sol.n_folio,'        ',
                            '\n    AND tipo_solicitud  =  ',lr_afi_sol.tipo_solicitud,' '
           PREPARE borra_tel FROM lc_comando
           EXECUTE borra_tel   
           
           -- BORRAR DE afi_correo_elect    
           LET lc_comando = '',
                            '\n DELETE FROM safre_af:afi_correo_elect                   ',
                            '\n  WHERE nss             = "',lr_afi_sol.n_seguro,'"      ',
                            '\n    AND n_folio         =  ',lr_afi_sol.n_folio,'        ',
                            '\n    AND tipo_solicitud  =  ',lr_afi_sol.tipo_solicitud,' '
           PREPARE borra_cor FROM lc_comando
           EXECUTE borra_cor   
        
        END IF
        
        -- ACTUALIZAR afi_act_menor_edad (n_folio, tipo_solicitud, nti_anterior)         
        UPDATE safre_af:afi_act_menor_edad
           SET n_folio        = lr_afi_sol.n_folio, 
               tipo_solicitud = lr_afi_sol.tipo_solicitud,
               nti_anterior   = lr_afi_mae.n_seguro
         WHERE id_act_menor_edad  = ld_id_act_menor
           AND folio_lote         = pi_folio_lote
           
        IF SQLCA.SQLERRD[3] > 0 THEN 
        	 -- SE ACTUALIZO EL REGISTRO 
        END IF 	    
        
        -- INSERTAR EN TABLAS DEFINITIVAS                          	
        INSERT INTO safre_af:afi_solicitud VALUES (lr_afi_sol.*)
        
        IF SQLCA.SQLERRD[3] > 0 THEN
        	
           -- DISPLAY "SE INSERTO EN afi_solicitud"
           
           --------------------------
           -- INICIO MARCAR LA CUENTA
           -------------------------- 
           LET v_marca          =  622
           LET v_estado_marca   =  0
           LET v_marca_causa    =  0
           LET v_codigo_rechazo =  0
           LET v_marca_causa    =  0
           LET v_fecha_causa    =  ""
           LET v_correlativo    =  0
           
           LET lc_comando = '',
                            '\n SELECT MAX(correlativo)                 ',
                            '\n   FROM cta_act_marca                    ',
                            '\n  WHERE nss  = "',lr_afi_sol.n_seguro,'" '
           PREPARE mar_sel FROM lc_comando
           EXECUTE mar_sel INTO v_correlativo
           
           IF (SQLCA.SQLCODE = NOTFOUND) OR (v_correlativo IS NULL) OR (v_correlativo = 0) THEN 
           	  LET v_correlativo = 1 
           END IF 	                     

           ----------------------
           -- MARCA CUENTA 
           ----------------------
           CALL fn_marca_cuenta (lr_afi_sol.n_seguro, 
                                 v_marca            , 
                                 v_correlativo      , 
                                 v_estado_marca     , 
                                 v_codigo_rechazo   , 
                                 v_marca_causa      , 
                                 v_fecha_causa      , 
                                 vusu               )
                                                
           -------------------------------------                                   
           -- CASO ESPECIAL PARA IMSS 
           -- CON LOS DATOS DE AFI_MAE_AFILIADO 
           -------------------------------------
           IF lr_afi_sol.tipo_solicitud = 33 THEN  
           	
              LET v_marca          =  621
              LET v_estado_marca   =  0
              LET v_marca_causa    =  0
              LET v_codigo_rechazo =  0
              LET v_marca_causa    =  0
              LET v_fecha_causa    =  ""
              LET v_correlativo    =  0
              
              LET lc_comando = '',
                               '\n SELECT MAX(correlativo)                 ',
                               '\n   FROM cta_act_marca                    ',
                               '\n  WHERE nss  = "',lr_afi_mae.n_seguro,'" '
              PREPARE mar_sel2 FROM lc_comando
              EXECUTE mar_sel2 INTO v_correlativo
              
              IF (SQLCA.SQLCODE = NOTFOUND) OR (v_correlativo IS NULL) OR (v_correlativo = 0) THEN 
              	  LET v_correlativo = 1 
              END IF 	
                            
              CALL fn_marca_cuenta (lr_afi_mae.n_seguro, 
                                    v_marca            , 
                                    v_correlativo      , 
                                    v_estado_marca     , 
                                    v_codigo_rechazo   , 
                                    v_marca_causa      , 
                                    v_fecha_causa      , 
                                    vusu               )
              
           END IF 	
           ------------------------                                   
           -- FIN MARCAR LA CUENTA 
           ------------------------
        
           -- CONSIDERAR LOS CODIGOS DE DOMICILIO 1 y 2      
        	 -- AFI_DOMICILIO
           FOR ls_dom_cod = 1 TO 2 
           
             LET lc_comando = '',
                              '\n SELECT FIRST 1 *                                        ',
                              '\n   FROM safre_af:afi_domicilio                           ',
                              '\n  WHERE nss             = "',lr_afi_mae.n_seguro,'"      ',
                              '\n    AND n_folio         =  ',lr_afi_mae.n_folio,'        ',
                              '\n    AND tipo_solicitud  =  ',lr_afi_mae.tipo_solicitud,' ',
                              '\n    AND dom_cod         =  ',ls_dom_cod,'                ',
                              '\n    AND marca_envio     =  "X"                           ',
                              '\n  ORDER BY factualiza DESC                               '
             PREPARE com_cinco FROM lc_comando
             EXECUTE com_cinco INTO lr_afi_dom.*
             
             IF SQLCA.SQLCODE = NOTFOUND THEN 
             	  -- NO SE ENCONTRO REGISTRO
                -- DISPLAY "NO SE INSERTO EN afi_domicilio con dom_cod = ",ls_dom_cod," NO EXISTE"
             	  INITIALIZE lr_afi_dom.* TO NULL 
             ELSE 
                LET lv_calle = lr_afi_dom.calle CLIPPED 
                
                IF lv_calle = "" OR lv_calle IS NULL THEN 
                   -- NO INSERTAR EL REGISTRO
                   -- DISPLAY "NO SE INSERTO EN afi_domicilio con dom_cod = ",ls_dom_cod," SIN CALLE"
                ELSE    	                	 
                   LET lr_afi_dom.tipo_solicitud = lr_afi_sol.tipo_solicitud
                   LET lr_afi_dom.nss            = lr_afi_sol.n_seguro
                   INSERT INTO safre_af:afi_domicilio VALUES (lr_afi_dom.*) 
                   
                   IF SQLCA.SQLERRD[3] > 0 THEN
                   	  -- DISPLAY "SE INSERTO EN afi_domicilio con dom_cod = ",ls_dom_cod,""
                   END IF 	 
                   INITIALIZE lr_afi_dom.* TO NULL 
                END IF 
             END IF 	  
             
           END FOR         
        	 -- TERMINA AFI_DOMICILIO
        
           -- AFI_TELEFONO
           FOR ls_tel_cod = 1 TO 4
           
             IF ls_tel_cod = 3 THEN
             	  -- NO CONSIDERAR ESTE VALOR 
             ELSE
                LET lc_comando = '',
                                 '\n SELECT FIRST 1 *                                        ',
                                 '\n   FROM safre_af:afi_telefono                            ',
                                 '\n  WHERE nss             = "',lr_afi_mae.n_seguro,'"      ',
                                 '\n    AND n_folio         =  ',lr_afi_mae.n_folio,'        ',
                                 '\n    AND tipo_solicitud  =  ',lr_afi_mae.tipo_solicitud,' ',
                                 '\n    AND tel_cod         =  ',ls_tel_cod,'                ',
                                 '\n  ORDER BY factualiza DESC                               '
                PREPARE com_seis FROM lc_comando
                EXECUTE com_seis INTO lr_afi_tel.*
                
                IF SQLCA.SQLCODE = NOTFOUND THEN 
                	  -- NO SE ENCONTRO REGISTRO
                    -- DISPLAY "NO SE INSERTO EN afi_telefono con tel_cod = ",ls_tel_cod," NO EXISTE"
                	  INITIALIZE lr_afi_tel.* TO NULL 
                ELSE 
                   LET lv_telefono = lr_afi_tel.telefono CLIPPED 
                   
                   IF lv_telefono = "" OR lv_telefono IS NULL THEN 
                      -- NO INSERTAR EL REGISTRO
                      -- DISPLAY "NO SE INSERTO EN afi_telefono con tel_cod = ",ls_tel_cod," SIN TELEFONO"
                   ELSE    	                	 
                      LET lr_afi_tel.tipo_solicitud = lr_afi_sol.tipo_solicitud
                      LET lr_afi_tel.nss            = lr_afi_sol.n_seguro
                      INSERT INTO safre_af:afi_telefono VALUES (lr_afi_tel.*) 
                      
                      IF SQLCA.SQLERRD[3] > 0 THEN
                      	 -- DISPLAY "SE INSERTO EN afi_telefono con tel_cod = ",ls_tel_cod,""
                      END IF 	 
                      INITIALIZE lr_afi_tel.* TO NULL 
                   END IF 
                END IF 	  
             END IF  
             
           END FOR  
           -- TERMINA AFI_TELEFONO
           
           -- AFI_CORREO_ELECT
           LET lc_comando = '',
                            '\n SELECT FIRST 1 *                                        ',
                            '\n   FROM safre_af:afi_correo_elect                        ',
                            '\n  WHERE nss             = "',lr_afi_mae.n_seguro,'"      ',
                            '\n    AND n_folio         =  ',lr_afi_mae.n_folio,'        ',
                            '\n    AND tipo_solicitud  =  ',lr_afi_mae.tipo_solicitud,' ',
                            '\n  ORDER BY factualiza DESC                               '
           PREPARE com_siete FROM lc_comando
           EXECUTE com_siete INTO lr_afi_cor.*
           
           IF SQLCA.SQLCODE = NOTFOUND THEN 
           	  -- NO SE ENCONTRO REGISTRO
           	  -- DISPLAY "NO SE INSERTO EN afi_correo_elect NO EXISTE"
           	  INITIALIZE lr_afi_cor.* TO NULL 
           ELSE 
              LET lv_correo = lr_afi_cor.correo_elect CLIPPED 
              
              IF lv_correo = "" OR lv_correo IS NULL THEN 
                 -- NO INSERTAR EL REGISTRO
                 -- DISPLAY "SE INSERTO EN afi_correo_elect SIN CORREO"
              ELSE    	                	 
                 LET lr_afi_cor.tipo_solicitud = lr_afi_sol.tipo_solicitud
                 LET lr_afi_cor.nss            = lr_afi_sol.n_seguro
                 INSERT INTO safre_af:afi_correo_elect VALUES (lr_afi_cor.*) 
                 
                 IF SQLCA.SQLERRD[3] > 0 THEN
                 	  -- DISPLAY "SE INSERTO EN afi_correo_elect"
                 END IF 	 
                 INITIALIZE lr_afi_cor.* TO NULL 
              END IF 
           END IF 	  
           -- TERMINA AFI_CORREO_ELECT              
        ELSE 
           -- NO INSERTAR EN LAS TABLAS RESTANTES 
           -- DISPLAY "NO SE INSERTO EN afi_solicitud"
           -- DISPLAY "NO SE INSERTO EN LAS TABLAS RESTANTES"
        END IF 
        -- INSERT INTO afi_solicitud                    
        
     END IF 	 
     -- SELECT EN AFI_MAE_AFILIADO
                           
  END IF
  -- INSERTAR EN AFI_ACT_MENOR_EDAD

END FUNCTION    

-------------------------------------------------------------------------------

FUNCTION asigna_valores(pr_afimae)

  DEFINE pr_afimae    RECORD LIKE afi_mae_afiliado.*
  
  DEFINE pr_afisol    RECORD LIKE afi_solicitud.* 

  -- FUE NECESARIO HACER ESTA FUNCION POR LAS DIFERENCIAS
  -- DE LOS SIGUIENTES CAMPOS y NO FUE POSIBLE HACER UNA 
  -- ASIGNACION DIRECTA DE TODO EL REGISTRO 
  -- afi_mae_afiliado               afi_solicitud
  -- integer        n_operac        decimal(10)
  -- smallint       status_captura  char(15)   
  -- char(15)       indicador_c     char(5)  
  -- char(15)       indicador_d     char(5)  
  -- char(15)       indicador_e     char(5)  

  LET pr_afisol.n_seguro            = pr_afimae.n_seguro          
  LET pr_afisol.n_unico             = pr_afimae.n_unico           
  LET pr_afisol.n_rfc               = pr_afimae.n_rfc             
  LET pr_afisol.paterno             = pr_afimae.paterno           
  LET pr_afisol.materno             = pr_afimae.materno           
  LET pr_afisol.nombres             = pr_afimae.nombres           
  LET pr_afisol.fena                = pr_afimae.fena              
  LET pr_afisol.n_folio             = pr_afimae.n_folio           
  LET pr_afisol.edo_civil           = pr_afimae.edo_civil         
  LET pr_afisol.localn              = pr_afimae.localn            
  LET pr_afisol.estadon             = pr_afimae.estadon           
  LET pr_afisol.tiptr               = pr_afimae.tiptr             
  LET pr_afisol.cod_promotor        = pr_afimae.cod_promotor      
  LET pr_afisol.sexo                = pr_afimae.sexo              
  LET pr_afisol.n_operac            = pr_afimae.n_operac          
  LET pr_afisol.frecafor            = pr_afimae.frecafor          
  LET pr_afisol.fentcons            = pr_afimae.fentcons          
  LET pr_afisol.femision            = pr_afimae.femision          
  LET pr_afisol.finitmte            = pr_afimae.finitmte          
  LET pr_afisol.finicta             = pr_afimae.finicta           
  LET pr_afisol.status              = pr_afimae.status            
  LET pr_afisol.agenc_cod           = pr_afimae.agenc_cod         
  LET pr_afisol.status_interno      = pr_afimae.status_interno    
  LET pr_afisol.nacionalidad        = pr_afimae.nacionalidad      
  LET pr_afisol.tip_prob            = pr_afimae.tip_prob          
  LET pr_afisol.fol_prob            = pr_afimae.fol_prob          
  LET pr_afisol.doc_prob            = pr_afimae.doc_prob          
  LET pr_afisol.ind_infonavit       = pr_afimae.ind_infonavit     
  LET pr_afisol.documento_1         = pr_afimae.documento_1       
  LET pr_afisol.documento_2         = pr_afimae.documento_2       
  LET pr_afisol.documento_3         = pr_afimae.documento_3       
  LET pr_afisol.documento_4         = pr_afimae.documento_4       
  LET pr_afisol.documento_5         = pr_afimae.documento_5       
  LET pr_afisol.documento_6         = pr_afimae.documento_6       
  LET pr_afisol.envio_dom           = pr_afimae.envio_dom         
  LET pr_afisol.entidad_curp        = pr_afimae.entidad_curp      
  LET pr_afisol.asigna_curp         = pr_afimae.asigna_curp       
  LET pr_afisol.const_curp          = pr_afimae.const_curp        
  LET pr_afisol.usuario             = pr_afimae.usuario           
  LET pr_afisol.hora                = pr_afimae.hora              
  LET pr_afisol.status_captura      = pr_afimae.status_captura    
  LET pr_afisol.tipo_solicitud      = pr_afimae.tipo_solicitud    
  LET pr_afisol.fecha_elaboracion   = pr_afimae.fecha_elaboracion 
  LET pr_afisol.lote                = pr_afimae.lote              
  LET pr_afisol.fecha_envio         = pr_afimae.fecha_envio       
  LET pr_afisol.cod_esq_comision    = pr_afimae.cod_esq_comision  
  LET pr_afisol.ubicacion           = pr_afimae.ubicacion         
  LET pr_afisol.fecha_1a_afil       = pr_afimae.fecha_1a_afil     
  LET pr_afisol.indicador_c         = pr_afimae.indicador_c       
  LET pr_afisol.indicador_d         = pr_afimae.indicador_d       
  LET pr_afisol.indicador_e         = pr_afimae.indicador_e       
  LET pr_afisol.cod_error_origen    = pr_afimae.cod_error_origen  
  LET pr_afisol.folio_edo_cta       = pr_afimae.folio_edo_cta     
  LET pr_afisol.cod_afore_ced       = pr_afimae.cod_afore_ced     
  LET pr_afisol.salario_base_comis  = pr_afimae.salario_base_comis
  LET pr_afisol.salario_actual      = pr_afimae.salario_actual    
  LET pr_afisol.fecha_actualiza_sa  = pr_afimae.fecha_actualiza_sa
  LET pr_afisol.coduni_n1           = pr_afimae.coduni_n1         
  LET pr_afisol.indicador_comision  = pr_afimae.indicador_comision
  LET pr_afisol.codven              = pr_afimae.codven            
  LET pr_afisol.coor_captura        = pr_afimae.coor_captura      
  LET pr_afisol.lote_captura        = pr_afimae.lote_captura      
  LET pr_afisol.folio_captura       = pr_afimae.folio_captura     
  LET pr_afisol.sello_electronico   = pr_afimae.sello_electronico 

  RETURN pr_afisol.*

END FUNCTION 

-------------------------------------------------------------------------------

FUNCTION fn_marca_cuenta(p_n_seguro    , p_marca         , p_correlativo,
                         p_estado_marca, p_codigo_rechazo, p_marca_causa,
                         p_fecha_causa , p_usu           )

  DEFINE p_n_seguro        CHAR(11)  -- pnss          
  DEFINE p_marca           SMALLINT  -- pmarca_entra  
  DEFINE p_correlativo     INTEGER   -- pcorrelativo  
  DEFINE p_estado_marca    SMALLINT  -- pestado_marca 
  DEFINE p_codigo_rechazo  SMALLINT  -- pcodigo_rechazo
  DEFINE p_marca_causa     SMALLINT  -- pmarca_causa  
  DEFINE p_fecha_causa     DATE      -- pfecha_causa  
  DEFINE p_usu             CHAR(08)  -- pusuario    
  
  DEFINE v_marca_causa     SMALLINT 
  DEFINE v_codigo_rechazo  SMALLINT 
  
  LET lc_comando = '',
                   '\n EXECUTE PROCEDURE marca_cuenta ( "',p_n_seguro,'",      ',
                   '\n                                   ',p_marca,',          ',
                   '\n                                   ',p_correlativo,',    ',
                   '\n                                   ',p_estado_marca,',   ',
                   '\n                                   ',p_codigo_rechazo,', ',
                   '\n                                   ',p_marca_causa,',    ',
                   '\n                                  "',p_fecha_causa,'",   ',
                   '\n                                  "',p_usu,'")           '                   
   
   -- DISPLAY lc_comando CLIPPED  
   -- DISPLAY "MARCAJE DE CUENTA ",p_n_seguro              
   -- DISPLAY "MARCA ",p_marca
   
   PREPARE eje_mar FROM lc_comando
   DECLARE cur_mar CURSOR FOR eje_mar
      OPEN cur_mar
     FETCH cur_mar INTO v_marca_causa, v_codigo_rechazo
     CLOSE cur_mar 
     
   -- DISPLAY "RESPUESTA ",v_marca_causa," - ",v_codigo_rechazo  

END FUNCTION

-------------------------------------------------------------------------------

FUNCTION archivo_cargado(parch)

  DEFINE parch   VARCHAR(60)
  DEFINE vexis   SMALLINT 
  
  LET vexis = 0 
  
  LET lc_comando = '',
                   '\n SELECT COUNT(*)                                  ',
                   '\n   FROM afi_act_menor_edad                        ',
                   '\n  WHERE nom_archivo_carga  = "',parch CLIPPED,'"  '
  PREPARE revisa_carga FROM lc_comando
  EXECUTE revisa_carga INTO vexis
  
  IF vexis = 0 THEN 
  	 RETURN FALSE 
  ELSE 
     RETURN TRUE 
  END IF 	                     
  
END FUNCTION 

-------------------------------------------------------------------------------

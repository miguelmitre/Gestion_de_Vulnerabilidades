# CARGA MARCA Y DESMARCA
DATABASE safre_af

GLOBALS
   DEFINE
     vg_usuario            CHAR(8)
    ,vg_hoy                DATE
    ,vg_archivo_marca_desmar  CHAR(200)
    ,vg_regs_total        INTEGER
    ,vg_regs_rech         INTEGER
    ,vg_regs_marcados     INTEGER
    ,vg_regs_desmarcados  INTEGER
    ,vg_cuantos           INTEGER
    ,vg_carga_reg         CHAR(35)
    ,vg_opc               CHAR(1)
    ,vg_MarcaCuenta       SMALLINT
    ,vg_marca_res         SMALLINT
    ,vg_cod_rechazo       SMALLINT
    ,vg_Marca             CHAR(200)
    ,vg_impre             CHAR(300)
    ,enter              CHAR(001)

   DEFINE rg_parametro          RECORD LIKE seg_modulo.* 
   DEFINE archivo_carga         CHAR(25)

END GLOBALS


MAIN

   OPTIONS
      PROMPT LINE LAST
     DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV('USER')||'.RETM022.log')

   CALL Inicio()
   CALL Proceso()

END MAIN


FUNCTION Inicio()
   SELECT *
        ,USER
     INTO rg_parametro.*
           ,vg_usuario
    FROM seg_modulo
   WHERE modulo_cod = "ret"

   LET vg_MarcaCuenta   = 935
   LET vg_regs_total    = 0
   LET vg_hoy           = TODAY
   LET vg_regs_rech     = 0
   LET vg_regs_marcados = 0
   LET vg_regs_desmarcados  = 0
--        (n_registros CHAR(35))
   WHENEVER ERROR CONTINUE
      CREATE TEMP TABLE tmp_marca_desmarca
            (v_nss  CHAR(11),
             v_curp CHAR(18),
             v_tipo_marca CHAR(01)
             )
   WHENEVER ERROR STOP


   WHENEVER ERROR CONTINUE
      CREATE TEMP TABLE tmp_rechazo
        (v_nss  CHAR(11),
         v_curp CHAR(18),
         motivo_rechazo CHAR(40)
         )
   WHENEVER ERROR STOP

   LET vg_Marca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
   PREPARE eje_marca FROM vg_Marca   

    LET vg_Marca = "EXECUTE PROCEDURE desmarca_cuenta( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM vg_Marca 

    LET  vg_Marca =       " SELECT FIRST 1 *                "
                         ," FROM cta_act_marca              "
                         ," WHERE nss = ?                   "
                         ," AND marca_cod = ?               "
                         ," AND correlativo = ?             "
                         ," ORDER BY fecha_ini DESC         "
    PREPARE p_marca_sql FROM  vg_Marca 
END FUNCTION


FUNCTION Proceso()

   OPEN WINDOW RETM0221 AT 2,2 WITH FORM "RETM0221" ATTRIBUTE(BORDER)
       DISPLAY "     [Enter] Iniciar                                       [Ctrl-C]  Salir     " AT 1,1 ATTRIBUTE(REVERSE)
       DISPLAY " RETM022    TRABAJADOR CON ATRIBUTO DE PENSION                        " AT 3,1 ATTRIBUTE(REVERSE)
       DISPLAY vg_hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

       CALL fn_carga_archivo()

   CLOSE WINDOW RETM0221

END FUNCTION


FUNCTION fn_carga_archivo()

    LET vg_cuantos = 0
     
    CLEAR FORM
    DISPLAY "" AT 2,1
    DISPLAY "[Enter] Iniciar                                               [Ctrl-C]  Salir     " AT 1,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME archivo_carga WITHOUT DEFAULTS
      BEFORE FIELD archivo_carga
         LET archivo_carga = NULL
         CLEAR FORM

      AFTER FIELD archivo_carga
         IF archivo_carga IS NULL THEN
            ERROR " EL CAMPO NO PUEDE SER NULO ..."
            SLEEP 1
            NEXT FIELD archivo_carga
         END IF

         LET vg_archivo_marca_desmar = rg_parametro.ruta_rescate CLIPPED,"/",archivo_carga CLIPPED

         WHENEVER ERROR CONTINUE
            LOAD FROM vg_archivo_marca_desmar INSERT INTO tmp_marca_desmarca;      
         WHENEVER ERROR CONTINUE

         
         SELECT COUNT(*) INTO vg_cuantos
           FROM tmp_marca_desmarca 

         IF vg_cuantos = 0 THEN
            ERROR " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO... FAVOR DE VERIFICAR "
            SLEEP 1
            NEXT FIELD archivo_carga
         END IF

         WHILE TRUE
             PROMPT " ESTA SEGURO DE CARGA EL ARCHIVO S/N ? " FOR CHAR enter
             IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN
                    EXIT WHILE
                ELSE
                    PROMPT" PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
                    FOR CHAR enter
                    EXIT PROGRAM
                END IF
             END IF
         END WHILE
         EXIT INPUT 

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT PROGRAM
   END INPUT

   IF fn_valida_archivo(archivo_carga) = FALSE THEN 
      EXIT PROGRAM 
   END IF 

      DISPLAY " TOTAL REGISTROS            : " AT 11,8
     DISPLAY vg_regs_total                    AT 11,39
      DISPLAY " TOTAL REGISTROS RECHAZADOS : " AT 12,8
     DISPLAY vg_regs_rech                     AT 12,39
      DISPLAY " TOTAL REGISTROS MARCADOS   : " AT 13,8
     DISPLAY vg_regs_marcados                 AT 13,39
      DISPLAY " TOTAL REGISTROS DESMARCADOS: " AT 14,8
     DISPLAY vg_regs_desmarcados              AT 14,39

     CALL fn_procesa_marca_desmarca()
     CALL Reporte()

   ERROR " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
   PROMPT "" FOR vg_opc  

END FUNCTION 


FUNCTION msg_Error(p_cad)
   DEFINE
      p_cad    CHAR(100)

   PROMPT p_cad
   FOR CHAR vg_opc

END FUNCTION


FUNCTION fn_procesa_marca_desmarca()

   DEFINE
     vl_cont_curp          SMALLINT

   ,v_nss                  CHAR(11)
   ,v_curp                 CHAR(18)
   ,v_tipo_marca           CHAR(01)
   ,vl_cad                 CHAR(50)
   ,v_estado               SMALLINT 
   ,v_reg_cta_act_marca    RECORD LIKE cta_act_marca.* 
   ,reg_ret_atributo_pension RECORD LIKE ret_trabajador_atributo_pension.*  
   
   DECLARE cur_1 CURSOR FOR
   SELECT *
   FROM tmp_marca_desmarca

   FOREACH cur_1 INTO  v_nss,v_curp,v_tipo_marca  

      --Valida estructura 
      IF LENGTH(v_nss CLIPPED)<> 11 THEN
          LET vl_cad = 'NSS INVALIDO, VALIDAR ESTRUTURA DE ARCHIVO'
          CALL msg_Error(vl_cad)
          EXIT FOREACH
      END IF 

      IF LENGTH(v_curp CLIPPED)<> 18 THEN
          LET vl_cad = 'CURP INVALIDO, VALIDAR ESTRUTURA DE ARCHIVO'
          CALL msg_Error(vl_cad)
          EXIT FOREACH
      END IF 

       IF LENGTH(v_tipo_marca CLIPPED)<> 1 THEN
          LET vl_cad = 'MARCA O DESMARCA INVALIDO, VALIDAR ESTRUTURA DE ARCHIVO'
          CALL msg_Error(vl_cad)
          EXIT FOREACH
      END IF 

      IF v_tipo_marca <> '0' AND v_tipo_marca <> '1' THEN
          LET vl_cad = 'MARCA O DESMARCA INVALIDO, VALIDAR ESTRUTURA DE ARCHIVO'
          CALL msg_Error(vl_cad)
          EXIT FOREACH
      END IF 
      
       -- Existe nss 
      LET vl_cont_curp = 0

      SELECT COUNT(n_unico)INTO vl_cont_curp
      FROM afi_mae_afiliado
      WHERE n_unico = v_nss 

      IF vl_cont_curp = 0 OR vl_cont_curp IS NULL THEN 
         LET vg_regs_rech       = vg_regs_rech + 1
         INSERT INTO tmp_rechazo VALUES (v_nss,v_curp,'NO EXISTE CUENTA')
         CONTINUE FOREACH 
      END IF 

      INITIALIZE v_reg_cta_act_marca.* TO NULL
      INITIALIZE reg_ret_atributo_pension.* TO NULL  

       DECLARE cur_nss CURSOR FOR
       SELECT UNIQUE *
       FROM ret_trabajador_atributo_pension 
       WHERE nss = v_nss 

       FOREACH cur_nss INTO reg_ret_atributo_pension.*

              --Existe marca 
              EXECUTE p_marca_sql USING v_nss,vg_MarcaCuenta,reg_ret_atributo_pension.id_ret_trab_atrib_pension  
                     INTO v_reg_cta_act_marca.* 
              
              --Marcar 
              IF  v_tipo_marca = '1'  THEN

                  IF v_reg_cta_act_marca.NSS IS NULL THEN 

                      LET v_estado = fn_marcacuenta(v_nss,reg_ret_atributo_pension.id_ret_trab_atrib_pension )

                      IF v_estado == 0 THEN
                            UPDATE ret_trabajador_atributo_pension SET fecha_inicio_marca = TODAY 
                            WHERE nss = v_nss 
                            AND   id_ret_trab_atrib_pension = v_reg_cta_act_marca.correlativo
                            
                            LET vg_regs_marcados = vg_regs_marcados + 1 
                      ELSE
                            LET vg_regs_rech = vg_regs_rech + 1
                            INSERT INTO tmp_rechazo VALUES (v_nss,v_curp,'ERROR EN MARCAR LA CUENTA');
                      END IF 
                      
                  ELSE 
                     LET vg_regs_rech = vg_regs_rech + 1
                     INSERT INTO tmp_rechazo VALUES (v_nss,v_curp,'CUENTA SE ENCUENTRA MARCADA')
                  END IF  

              END IF 

              --DesMarcar 
              IF  v_tipo_marca = '0'  THEN 

                 IF v_reg_cta_act_marca.marca_cod = vg_MarcaCuenta THEN 
                      CALL fn_desmarca_cuenta(v_nss,reg_ret_atributo_pension.id_ret_trab_atrib_pension )

                      UPDATE ret_trabajador_atributo_pension SET fecha_fin_marca = TODAY 
                      WHERE nss = v_nss 
                      AND   id_ret_trab_atrib_pension = v_reg_cta_act_marca.correlativo

                      LET vg_regs_desmarcados = vg_regs_desmarcados + 1 
                 ELSE 
                     INSERT INTO tmp_rechazo VALUES (v_nss,v_curp,'CUENTA DESMARCADA ANTERIORMENTE') 
                     LET vg_regs_rech = vg_regs_rech + 1
                 END IF 

              END IF 
         LET v_nss = NULL
         LET v_curp = NULL
         LET v_tipo_marca = NULL   
     END FOREACH 
     
     DISPLAY vg_regs_total                    AT 11,39 
     DISPLAY vg_regs_rech                     AT 12,39
     DISPLAY vg_regs_marcados                 AT 13,39
     DISPLAY vg_regs_desmarcados              AT 14,39

     LET vg_regs_total = vg_regs_rech + vg_regs_marcados + vg_regs_desmarcados  

   END FOREACH

   LET vg_regs_total = vg_regs_rech + vg_regs_marcados + vg_regs_desmarcados
   DISPLAY vg_regs_total                    AT 11,39 
   DISPLAY vg_regs_rech                     AT 12,39
   DISPLAY vg_regs_marcados                 AT 13,39
   DISPLAY vg_regs_desmarcados              AT 14,39

END FUNCTION


FUNCTION fn_marcacuenta(p_nss,p_consecutivo)
   DEFINE
       vl_Correlativo SMALLINT
      ,vl_Nulo        SMALLINT
      ,p_nss          CHAR(11)
      ,p_consecutivo  DECIMAL(11,0) 

   LET vl_Correlativo = 0
   LET vl_Nulo = NULL

   -- Pone marca correspondiente a la cuenta
   DECLARE cur_sp CURSOR FOR eje_marca
   OPEN cur_sp USING p_nss
                ,vg_MarcaCuenta
                ,p_consecutivo
                ,vl_Correlativo
                ,vl_Correlativo
                ,vl_Correlativo
                ,vl_Nulo
                ,vg_Usuario

   FETCH cur_sp INTO vg_marca_res
                ,vg_cod_rechazo
   CLOSE cur_sp

   RETURN vg_cod_rechazo
END FUNCTION


FUNCTION Reporte()
   DEFINE v_nss_rech   CHAR(11)
   DEFINE v_curp_rech  CHAR(18)
   DEFINE v_motivo_rechazo CHAR(40)
   DEFINE v_ruta           CHAR(100) 
   DEFINE v_tot_report     INTEGER 

   LET v_tot_report = 0

   LET v_ruta = "ls -l ",rg_parametro.ruta_envio CLIPPED,"/","ATRIBUTOREC",vg_hoy USING "ddmmyy","*  | wc -l "
   RUN v_ruta RETURNING v_tot_report 

   LET v_tot_report = v_tot_report + 1 
   LET vg_impre = rg_parametro.ruta_envio CLIPPED,"/","ATRIBUTOREC",vg_hoy USING "ddmmyy",'C',v_tot_report USING '&&&','.txt'

   DISPLAY " Nombre reporte: ", vg_impre AT 16,8

   START REPORT rpt_archivo TO vg_impre

   DECLARE cur_Detalle CURSOR FOR
      SELECT * FROM tmp_rechazo

   FOREACH cur_Detalle INTO v_nss_rech,v_curp_rech,v_motivo_rechazo

      OUTPUT TO REPORT rpt_archivo(v_nss_rech,v_curp_rech,v_motivo_rechazo)

   END FOREACH

   FINISH REPORT rpt_archivo

END FUNCTION


REPORT rpt_archivo(p_nss_rech,p_curp_rech,p_motivo_rechazo)

   DEFINE p_nss_rech   CHAR(11)
   DEFINE p_curp_rech  CHAR(18)
   DEFINE p_motivo_rechazo CHAR(40)

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60


   FORMAT
      PAGE HEADER

         PRINT COLUMN 001, "NSS"
              ,COLUMN 013, "CURP"
              ,COLUMN 033, "Motivo de Rechazo"

      ON EVERY ROW
         PRINT COLUMN 001, p_nss_rech CLIPPED
              ,COLUMN 013, p_curp_rech CLIPPED
              ,COLUMN 033, p_motivo_rechazo CLIPPED

END REPORT



FUNCTION fn_valida_archivo(p_nombre_archivo_carga)
DEFINE p_nombre_archivo_carga      CHAR(25)
DEFINE dia, mes , ano,consecutivo  INTEGER
DEFINE vl_cad                      CHAR(60)
DEFINE arctxt                      CHAR(5) 

   LET dia = 0
   LET mes = 0 
   LET ano = 0
   LET consecutivo = 0    

   LET dia =  p_nombre_archivo_carga[11,12]
   LET mes =  p_nombre_archivo_carga[13,14]
   LET ano =  p_nombre_archivo_carga[15,16]
   LET consecutivo =  p_nombre_archivo_carga[18,20]

   IF dia = 0 OR dia > 31 THEN 
      LET vl_cad = 'ERROR EN EL NOMBRE DEL ARCHIVO'
      CALL msg_Error(vl_cad)
      RETURN FALSE 
   END IF 

   IF mes = 0 OR mes > 12 THEN 
      LET vl_cad = 'ERROR EN EL NOMBRE DEL ARCHIVO'
      CALL msg_Error(vl_cad)
      RETURN FALSE 
   END IF 

   IF ano = 0 THEN 
      LET vl_cad = 'ERROR EN EL NOMBRE DEL ARCHIVO'
      CALL msg_Error(vl_cad)
      RETURN FALSE 
   END IF
   LET arctxt =  p_nombre_archivo_carga[22,24]

   IF p_nombre_archivo_carga[22,24] <> 'txt' THEN
      LET vl_cad = 'ERROR EN LA EXTENSION DEL ARCHIVO'
      CALL msg_Error(vl_cad)
      RETURN FALSE 
   END IF  

   RETURN TRUE 
END FUNCTION 



FUNCTION fn_desmarca_cuenta(v_nss,v_consecutivo)

    DEFINE lr_desmarca         RECORD
           nss                 LIKE cta_his_marca.nss          ,
           marca_cod           LIKE cta_his_marca.marca_cod    ,
           consecutivo         LIKE cta_his_marca.correlativo  ,
           edo_causa           LIKE cta_his_marca.estado_marca ,
           marca_causa         LIKE cta_his_marca.marca_causa
    END RECORD
    DEFINE v_nss               CHAR(11)
    DEFINE v_consecutivo       DECIMAL(11,0)

    LET lr_desmarca.nss         = v_nss
    LET lr_desmarca.marca_cod   = vg_MarcaCuenta
    LET lr_desmarca.consecutivo = v_consecutivo 
    LET lr_desmarca.edo_causa   = 0
    LET lr_desmarca.marca_causa = 0
    
    EXECUTE eje_desmarca USING lr_desmarca.*, vg_usuario

END FUNCTION
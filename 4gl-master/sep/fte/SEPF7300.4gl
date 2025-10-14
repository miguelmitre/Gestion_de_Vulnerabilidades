##########################################################################
#MODULO      : SEP
#PROGRAMA    : SEPF030 
#DESCRIPCION : LISTA PAREJAS ESTADO 3 DIAGNOSTICADO EN ESPERA DE ENVIARSE 
#            : DIAGNOSTICO DE OP27 (REGISTRO EN TABLA PUENTE PARA WS)
#FECHA       : 25/05/2022 CPL-3611
###########################################################################

DATABASE safre_af

GLOBALS 
  define xcodigo                  integer
  define r_seg_modulo             record like seg_modulo.*
  define 
         g_enter               char(001) ,
         g_ejecuta             char(100) 

  define hoy                   date

  define cont_1                integer

  define arr_1 ARRAY[2000]      of record
	 idbitacorawsresultadoconfronta smallint , 
         idsolicitudseparacion dec(10,0),
         nssInvadidoREQ        char(11) ,
         fechaalta             date     ,
         estado_sol            smallint ,
         des_estado_sol        char(15) ,
         estado_sep            smallint ,
         des_estado_sep        char(15)
  end record

  define arr_pendientes ARRAY[2000]      of record
	 arc_1                 smallint , 
         correlativo           dec(10,0),
         n_seguro              char(11),
         nss                   char(11),
         estado_sep            smallint,
         des_estado_sep        char(15),
         diag_confronta        CHAR(2),
         calsifica_separacion  CHAR(1)
  end record




  define txt_nss               CHAR(1000) , 
         txt_count             CHAR(1000) ,
         cad_construct         CHAR(0050) ,
         x_nss                 CHAR(0011)

  define arc_1                 ,
         total_pa              ,
         tot_folio_ts          INTEGER

  define band                  ,
         band_c                smallint

  define v_idsolicitudseparacion dec(10,0);
  define v_idbitacorawsresultadoconfronta  dec(10,0);

END GLOBALS

MAIN

 OPTIONS INPUT WRAP,
 PROMPT LINE LAST  ,
 ACCEPT KEY CONTROL-I
 DEFER INTERRUPT

 CALL STARTLOG("SEPF500.log")
 LET hoy = TODAY
 CALL inicio()

   OPEN WINDOW ventana_menu AT 2,2 WITH 21 ROWS,78 COLUMNS ATTRIBUTE(BORDER)
   DISPLAY " SEPF7300            GESTION DE ENVIO DE RESULTADO DE CONFRONTA                  " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

   MENU "ENVIO CONFRONTA"
   COMMAND "Pendientes" "Parejas diagnosticadas pendientes de enviar"
      CALL pendientes()
   COMMAND "Peticiones WS" "Gestion de peticiones"
      CALL consulta()
   COMMAND "Salir" "Salir del Programa"
      EXIT MENU
   END MENU


 END MAIN

#--------------------
FUNCTION pendientes()
#--------------------

    DEFINE xresult SMALLINT
    DEFINE g       SMALLINT

    LET xresult = FALSE

    OPEN WINDOW ventana_pendientes AT 2,2 WITH FORM "SEPF7302" ATTRIBUTE(BORDER)


    DISPLAY "                                                                                 " AT 1,1 
    DISPLAY "                                                                                 " AT 2,1 
    DISPLAY "  ESC Aceptar  Ctrl-c Salir                                                      " AT 1,1 

    DISPLAY " SEPF7000            PAREJAS DIAGNOSTICADAS PENDIENTES DE ENVIAR                 " AT 3,1 ATTRIBUTE(REVERSE)
    
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    CONSTRUCT  cad_construct ON a.n_seguro             , --invadido
                                a.nss                  , --asociado
                                b.diag_confronta       , --dignostico 
                                b.clasifica_separacion --clasifica
                           FROM n_seguro               ,
                                nss                    ,
                                diag_confronta         ,
                                clasifica_separacion
                               
     ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    ON KEY(INTERRUPT)
       LET band_c = 0
       EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT int_flag THEN 

          LET txt_nss = 
          " SELECT unique '',a.correlativo        ,",
                         " a.n_seguro             ,",
                         " a.nss                  ,",
                         " a.estado               ,",
                         " d.des_estado           ,",
                         " b.diag_confronta       ,",
                         " b.clasifica_separacion  ", 
          " FROM sep_det_reg_sol_reclamante a , ",
                " sep_det_solicitud          b , ",
                " sep_estado_separacion      d   ", 
          " WHERE ",cad_construct CLIPPED ,
          " AND   a.correlativo = b.idsolicitudseparacion  " ,
          " AND   a.estado      = d.estado " ,
          " AND   a.estado      = 3 "

         WHILE TRUE

           PREPARE qry_pendientes FROM txt_nss 
           DECLARE cur_pendientes CURSOR FOR qry_pendientes
   
           LET cont_1 = 1
   
           FOREACH cur_pendientes INTO arr_pendientes[cont_1].*
                   LET arr_pendientes[cont_1].arc_1 = cont_1
                   LET cont_1              = cont_1 + 1
           END FOREACH
  
           IF cont_1 = 1 THEN
              ERROR ""
              CLEAR FORM
              PROMPT " No Existen Registros...<ENTER> para Salir " 
              ATTRIBUTE(REVERSE) FOR CHAR g_enter
	      CLOSE WINDOW ventana_pendientes
              EXIT WHILE
	   ELSE  


           LET txt_count = 
           " SELECT COUNT(*) " ,
           " FROM sep_det_reg_sol_reclamante a , ",
                " sep_det_solicitud          b , ",
                " sep_estado_separacion      d   ", 
           " WHERE ",cad_construct CLIPPED ,
           " AND   a.correlativo = b.idsolicitudseparacion  " ,
           " AND   a.estado      = d.estado " ,
           " AND   a.estado      = 3 "

             PREPARE qry_pendientes_count FROM txt_count
             EXECUTE qry_pendientes_count INTO tot_folio_ts
             LET total_pa = cont_1 - 1 
	     DISPLAY BY NAME total_pa
	     DISPLAY BY NAME tot_folio_ts

             CALL SET_COUNT(cont_1-1)
            
             LET arc_1 = 0

             DISPLAY " Ctrl-i Reg Sep     Ctrl-b Request                             Ctrl-c Salir  " AT 1,1 

             DISPLAY ARRAY arr_pendientes TO  scr_pendientes.*


             ON KEY(CONTROL-B) --reenvio de resultado de confronta 

                  LET g = ARR_CURR()

                  IF ( arr_pendientes[g].estado_sep = 3) THEN

                        WHILE TRUE
                           PROMPT "CONFIRMAR ENVIO RESULTADO DE CONFRONTA S/N ? " FOR CHAR g_enter
                           IF g_enter MATCHES "[sSnN]" THEN
                              IF g_enter MATCHES "[sS]" THEN

                                 LET xresult = 0
                                 CALL fws_resultado_confronta(arr_pendientes[g].n_seguro ,arr_pendientes[g].correlativo)
                                 RETURNING xresult
                     
                                 IF xresult THEN

                                 LET arr_pendientes[g].estado_sep = 4

                                 SELECT a.des_estado
                                 INTO   arr_pendientes[g].des_estado_sep
                                 FROM   sep_estado_separacion  a
                                 WHERE  a.estado = arr_pendientes[g].estado_sep

                                    UPDATE sep_det_reg_sol_reclamante
                                    SET    sep_det_reg_sol_reclamante.estado         = 4  , --en sep_bitacora_wsresultado_confronta
                                           sep_det_reg_sol_reclamante.fecha_proceso  = TODAY
                                    WHERE  sep_det_reg_sol_reclamante.correlativo =
                                           arr_pendientes[g].correlativo
                     
                                    ERROR ""
                                    PROMPT " REGISTRO PARA ENVIO DE RESULTADO DE CONFRONTA SOLICITADO..<Enter>..." FOR char g_enter
                                 ELSE
                                    PROMPT "ERROR AL REGISTRAR EN BITACORA..<Enter>..." FOR char g_enter
                                 END IF

                                 DISPLAY arr_pendientes[g].estado_sep  TO scr_pendientes.estado_sep
                                 DISPLAY arr_pendientes[g].des_estado_sep TO scr_pendientes.des_estado_sep
                                 --DISPLAY arr_1_c[g].fecha_proceso TO scr_1.fecha_proceso

                                 EXIT WHILE
                              ELSE
                                 ERROR"ENVIO CANCELADO..."
                                 SLEEP 2
                                 ERROR" "
                                 EXIT WHILE
                              END IF
                           END IF
                        END WHILE
                  ELSE
                     ERROR"ESTADO NO MODIFICABLE"
                     SLEEP 2
                     ERROR" "
                  END IF

                ON KEY (CONTROL-I) -- consulta registro separacion de cuentas

		   LET arc_1 = ARR_CURR()
                   LET g_ejecuta = "fglgo SEPM100 ","1"," ","'",
                                   arr_pendientes[arc_1].n_seguro,"'"
                   RUN g_ejecuta
                   LET band = 1
                   --EXIT DISPLAY
                ON KEY (INTERRUPT)
	           LET band = 0
                   EXIT DISPLAY
             END DISPLAY 

             IF band = 0 THEN
                PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
                FOR CHAR g_enter
	        CLOSE WINDOW ventana_pendientes
                EXIT WHILE
             END IF
           END IF

       END WHILE

      ELSE 
	      CLOSE WINDOW ventana_pendientes
      END IF

END FUNCTION 

FUNCTION consulta()

    OPEN WINDOW ventana_nss AT 2,2 WITH FORM "SEPF7300" ATTRIBUTE(BORDER)


    DISPLAY "                                                                                 " AT 1,1 
    DISPLAY "                                                                                 " AT 2,1 
    DISPLAY "  ESC Aceptar  Ctrl-c Salir                                                      " AT 1,1 

    DISPLAY " SEPF7000      GESTION DE PETICIONES DE RESULTADOS DE CONFRONTA O27              " AT 3,1 ATTRIBUTE(REVERSE)
    
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    CONSTRUCT  cad_construct ON a.nssInvadidoREQ ,
                                a.fechaalta   ,
                                a.estado 
                           FROM nssInvadido   ,
                                fechaalta     ,
                                des_estado_sol 
                               
     ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    ON KEY(INTERRUPT)
       LET band_c = 0
       EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT int_flag THEN 

          LET txt_nss = 
          " SELECT unique  a.idbitacorawsresultadoconfronta,a.idsolicitudseparacion , ",
                           " a.nssInvadidoREQ        , ",
                           " a.fechaalta             , ", 
                           " a.estado                , ",
                           " b.descripcion           , ",
                           " c.estado                , ",
                           " d.des_estado              ",
          " FROM sep_bitacora_wsresultadoconfronta a , ",
               " sep_estado_solicitud_marca b , ", 
               " sep_det_reg_sol_reclamante c , ",
               " sep_estado_separacion      d   ",
          " WHERE ",cad_construct CLIPPED ,
          " AND a.idsolicitudseparacion = c.correlativo ",
          " AND a.estado                = b.estado      ",
          " AND c.estado                = d.estado      ",
          " ORDER BY a.idbitacorawsresultadoconfronta desc, a.estado,a.nssInvadidoREQ "

         WHILE TRUE

           PREPARE qry_nss FROM txt_nss 
           DECLARE cur_nss CURSOR FOR qry_nss
   
           LET cont_1 = 1
   
           FOREACH cur_nss INTO arr_1[cont_1].*
                   LET cont_1              = cont_1 + 1
           END FOREACH
  
           IF cont_1 = 1 THEN
              ERROR ""
              CLEAR FORM
              PROMPT " No Existen Registros...<ENTER> para Salir " 
              ATTRIBUTE(REVERSE) FOR CHAR g_enter
	      CLOSE WINDOW ventana_nss
              EXIT WHILE
	   ELSE  


              LET txt_count = 
              " SELECT COUNT(unique idsolicitudseparacion) ",
              " FROM sep_bitacora_wsresultadoconfronta a ,    ",
                   " sep_estado_solicitud_marca b , ", 
                   " sep_det_reg_sol_reclamante c , ",
                   " sep_estado_separacion      d   ",
              " WHERE ",cad_construct CLIPPED ,
              " AND a.idsolicitudseparacion = c.correlativo ",
              " AND a.estado                = b.estado      ",
              " AND c.estado                = d.estado      "


             PREPARE qry_txt_count FROM txt_count
             EXECUTE qry_txt_count INTO tot_folio_ts
             LET total_pa = cont_1 - 1 
	     DISPLAY BY NAME total_pa
	     DISPLAY BY NAME tot_folio_ts

             CALL SET_COUNT(cont_1-1)
            
             LET arc_1 = 0

             DISPLAY " Ctrl-i Reg Sep     Ctrl-e Response                            Ctrl-c Salir  " AT 1,1 
             DISPLAY " Ctrl-w Detalle ws                                                           " AT 2,1 

             DISPLAY ARRAY arr_1 TO  scr_1.*

                ON KEY (CONTROL-E)    -- recibe respuesta del ws       
                   LET arc_1 = ARR_CURR()
                   LET v_idsolicitudseparacion = arr_1[arc_1].idsolicitudseparacion
                   LET v_idbitacorawsresultadoconfronta  = arr_1[arc_1].idbitacorawsresultadoconfronta

                   IF arr_1[arc_1].estado_sol <> 2 THEN
                      PROMPT " Estado no viable para recuperar response...<Enter> para continuar... " 
                      ATTRIBUTE(REVERSE) FOR CHAR g_enter
                   ELSE
                      CALL fn_recibe_ws_response(v_idbitacorawsresultadoconfronta,v_idsolicitudseparacion) 
                      RETURNING xcodigo 
                      ERROR ""

                      IF xcodigo = 0 THEN PROMPT " RESPONSE RECIBIDO Y REGISTRO DE SEP ACTUALIZADO...<Enter>..." FOR CHAR g_enter END IF 
                      IF xcodigo = 1 THEN PROMPT " RESPONSE RECIBIDO y RESGISTRO DE SEP NO ENCONTRADO...<Enter>..." FOR CHAR g_enter END IF

                      LET band = 1
                      EXIT DISPLAY
                   END IF

                ON KEY (CONTROL-W)   -- consulta detalle request y response de tabla puente
                   LET arc_1 = ARR_CURR()
                   LET v_idsolicitudseparacion = arr_1[arc_1].idsolicitudseparacion
                   LET v_idbitacorawsresultadoconfronta  = arr_1[arc_1].idbitacorawsresultadoconfronta

                   CALL despliega_detalle_wssolmarca("sep_bitacora_wsresultadoconfronta",v_idbitacorawsresultadoconfronta)

                ON KEY (CONTROL-I) -- consulta registro separacion de cuentas

		   LET arc_1 = ARR_CURR()
		   --LET x_nss = arr_1[arc_1].asociado
                   LET g_ejecuta = "fglgo SEPM100 ","1"," ","'",
                                   arr_1[arc_1].nssInvadidoREQ,"'"
                   RUN g_ejecuta
                   LET band = 1
                   --EXIT DISPLAY
                ON KEY (INTERRUPT)
	           LET band = 0
                   EXIT DISPLAY
             END DISPLAY 

             IF band = 0 THEN
                PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
                FOR CHAR g_enter
	        CLOSE WINDOW ventana_nss
                EXIT WHILE
             END IF
           END IF

       END WHILE

      ELSE 
	      CLOSE WINDOW ventana_nss
      END IF

END FUNCTION 

#################
FUNCTION inicio()

   SELECT * 
   INTO  r_seg_modulo.*
   FROM seg_modulo
   WHERE modulo_cod = "sep"

END FUNCTION


############################################################
FUNCTION despliega_detalle_wssolmarca(p_tabla_consulta,v_corr)

DEFINE v_nss char(11)
DEFINE v_id_cliente dec(10,0)

DEFINE p_tabla       VARCHAR(50),
       p_tabla_consulta VARCHAR(50),
       p_id_registro DECIMAL(9,0),
       p_id_registro_consulta DECIMAL(9,0),
       p_id_maquinaria SMALLINT,
       p_tmp_detalle_solicitud RECORD
          v_ind        SMALLINT,
          v_diag       CHAR(3),
          v_sql_error  INTEGER,
          v_isam_error INTEGER,
          v_msg_error  VARCHAR(100),
          v_etiqueta   VARCHAR(50),
          v_valor      VARCHAR(80)
       END RECORD,
       v_detalle ARRAY[100] OF RECORD
          v_etiqueta VARCHAR(50),
          v_valor    VARCHAR(80)
       END RECORD,
       reg  SMALLINT ,
       vnss CHAR(11) ,
       vnombre CHAR(80)

   DEFINE v_corr     DEC(10,0)
   DEFINE v_consulta CHAR(200)
   DEFINE l_fecha    date
   DEFINE l_txt      char(1000)

  --OPEN WINDOW sepf7001 AT 03,26 WITH FORM "SEPF7001" ATTRIBUTE(BORDER)
  OPEN WINDOW sepf7001 AT 7,2 WITH FORM "SEPF7001" ATTRIBUTE(BORDER)
  DISPLAY " Ctrl-c Salir                                                                  " AT 1,1
  DISPLAY "                               DETALLE SERVICIO                                " AT 2,1 ATTRIBUTE(REVERSE)


  LET v_consulta = " EXECUTE FUNCTION fn_glo_recupera_etiquetas(?,?) "

  PREPARE prp_rec_detalle_solicitud FROM v_consulta

   DECLARE cur_rec_detalle_solicitud CURSOR FOR prp_rec_detalle_solicitud

   LET reg = 1

   FOREACH cur_rec_detalle_solicitud USING p_tabla_consulta ,
                                           v_corr
                                    INTO p_tmp_detalle_solicitud.*
      LET v_detalle[reg].v_etiqueta = p_tmp_detalle_solicitud.v_etiqueta
      LET v_detalle[reg].v_valor    = p_tmp_detalle_solicitud.v_valor
      LET reg = reg + 1
   END FOREACH

  LET l_txt = 
  "SELECT a.fechaalta " ,
  " FROM ",p_tabla_consulta clipped, " a",
  " WHERE a.idbitacorawsresultadoconfronta = ",v_corr 

  LET l_txt = l_txt clipped

  PREPARE qry_pp FROM l_txt
  DECLARE cur_pp CURSOR FOR qry_pp

  FOREACH cur_pp INTO l_fecha
  END FOREACH

  LET v_detalle[1].v_etiqueta = "Fecha Solicitud:"
  LET v_detalle[1].v_valor     = l_fecha USING "DD-MM-YYYY"

  SELECT nssInvadidoREQ
   INTO  vnss 
  FROM sep_bitacora_wsresultadoconfronta
  WHERE idbitacorawsresultadoconfronta = v_corr
 
  DISPLAY BY NAME vnss

  CALL SET_COUNT(reg-1)

  DISPLAY ARRAY v_detalle TO scr_2.* ATTRIBUTE(REVERSE)
    ON KEY (INTERRUPT)
       EXIT DISPLAY
  END DISPLAY

  CLOSE WINDOW sepf7001

END FUNCTION


#####################################
FUNCTION fn_recibe_ws_response(p_idbitacorawsresultadoconfronta,p_idsolicitudseparacion)

define codigo integer
DEFINE v_txt CHAR(200)

DEFINE p_idbitacorawsresultadoconfronta       ,
       p_idsolicitudseparacion      dec(10,0)


LET v_txt = "fglgo ",r_seg_modulo.ruta_exp CLIPPED,"/SEPC7300 ",p_idbitacorawsresultadoconfronta," ",p_idsolicitudseparacion," 1>sal 2>&1 "

RUN v_txt RETURNING codigo
let codigo = codigo / 256   
UPDATE sep_bitacora_wsresultadoconfronta 
SET     estado = 3
WHERE   idbitacorawsresultadoconfronta = p_idbitacorawsresultadoconfronta
RETURN codigo

END FUNCTION


########################################################
FUNCTION fws_resultado_confronta(vnss,dd_corr)

        DEFINE vnss                        CHAR(1)
        DEFINE dd_corr                     DEC(10,0)

        DEFINE reg_2 RECORD
            folio                 INTEGER,
            tipo_registro         CHAR(2),
            cont_servicio         INTEGER,
            n_seguro              CHAR(11),
            rfc                   CHAR(13),
            curp                  CHAR(18),
            tipo_ent_admon        CHAR(2),
            clave_admon           CHAR(3),
            paterno               CHAR(40),
            materno               CHAR(40),
            nombre                CHAR(40),
            fecha_nacimiento      DATE,
            ent_nacimiento        CHAR(2),
            sexo                  CHAR(1),
            nombre_procanase      CHAR(40),
            fecha_afiliacion      DATE,
            fecha_marca           DATE,
            diag_confronta        CHAR(2),
            clasifica_separacion  CHAR(1),
            credito               char(1),
            resulta_operacion     CHAR(2),
            diag_proceso1         CHAR(3),
            diag_proceso2         CHAR(3),
            diag_proceso3         CHAR(3),
            traspaso_previo       char(02),
            idSolicitudSeparacion integer
        END RECORD

        DEFINE
            vcodigo_afore        CHAR(3)   ,
            l_reclasifica        SMALLINT  ,
            l_clas_verif         CHAR(1)   , 
            p_correlativo        DEC(10,0) ,
            l_usuario            CHAR(20)  ,
            l_nss_asociado       CHAR(11)  ,
            l_ind_cambio_asociado char(1)  ,
            l_asociado_cambiado  char(11)  ,
            c_fecha              char(8)

        SELECT codigo_afore 
        INTO   vcodigo_afore
        FROM   tab_afore_local

        SELECT user 
        INTO  l_usuario
        FROM  tab_afore_local

        SELECT b.* ,
               a.nss
        INTO   reg_2.*  ,
               l_nss_asociado
        FROM   sep_det_reg_sol_reclamante       a ,
               sep_det_solicitud                b 
        WHERE  a.correlativo = dd_corr
        AND    a.correlativo = b.idSolicitudSeparacion
        AND    a.estado = 3 --diagnositcado

        LET l_reclasifica = NULL

        SELECT id INTO l_reclasifica
          FROM sep_reclasifica
        WHERE id = reg_2.idSolicitudSeparacion

        LET l_clas_verif = " "

        IF l_reclasifica = reg_2.idSolicitudSeparacion THEN
           LET l_clas_verif = "1"
        END IF
{
        IF reg_2.diag_confronta = '02' THEN

           SELECT a.correlativo
           INTO v_corr
           FROM cta_act_marca a
           WHERE a.nss = reg_2.nss
           AND   a.marca_cod = 280

           LET ejecuta =
           'EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?) '
           LET ejecuta = ejecuta CLIPPED
           PREPARE qry_desmarca FROM ejecuta

           LET pmarca_entra  = 280
           LET pestado_marca = 30
           LET pmarca_causa  = 280

           EXECUTE qry_desmarca USING reg_2.nss     ,
                                      pmarca_entra  ,
                                      v_corr        ,
                                      pestado_marca ,
                                      pmarca_causa  ,
                                      pusuario

        END IF

}
        LET c_fecha = reg_2.fecha_nacimiento  using "YYYYMMDD"

        INSERT INTO sep_bitacora_wsresultadoconfronta        
        VALUES (
            TODAY                       , --fechaalta    
            --REQUEST
            "01"                                     , --origensolicitudREQ
            reg_2.clave_admon                        , --claveAforeREQ
            reg_2.n_seguro                           , --nssInvadidoREQ
            reg_2.rfc                                , --rfcREQ
            reg_2.curp                               , --curpREQ
            reg_2.paterno                            , --apellidopaternoREQ
            reg_2.materno                            , --apellidomaternoREQ
            reg_2.nombre                             , --nombreTrabajadorREQ
            c_fecha                                  , --fechanacimientoREQ
            reg_2.ent_nacimiento                     , --entidadfederativanacimientoREQ
            reg_2.sexo                               , --sexoREQ
            ""                                       , --indicadorunificacionpreviaREQ
            reg_2.diag_confronta                     , --diagnosticoconfrontaREQ
            reg_2.clasifica_separacion               , --clasificacionseparacionREQ
            l_clas_verif                             , --indicadorcambioclasificacionREQ
            l_nss_asociado                           , --idnegocioasociadoREQ 
            l_ind_cambio_asociado                    , --indicadorcambionssasociadoREQ
            l_asociado_cambiado                      , --idnegocioasociadonuevoREQ 
            --RESPONSE EN BLANCO
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            --CTRL
            seq_sep_bitacora_wsresultadoconfronta.nextval  , --idbitacorawsresultadoconfronta
            dd_corr                                  , --idsolicitudseparacion
            "1"                                            , --estado
            l_usuario                                      , --usuario
            "0"                                              --omitido uso coppel
            )


            -- inicio actualizacion de nulos a vacio
   
            update sep_bitacora_wsresultadoconfronta set rfcREQ = ""  
               where idsolicitudseparacion = dd_corr and rfcREQ is null
            
            update sep_bitacora_wsresultadoconfronta set curpREQ = ""  
               where idsolicitudseparacion = dd_corr and curpREQ is null
            
            update sep_bitacora_wsresultadoconfronta set apellidopaternoREQ = ""  
               where idsolicitudseparacion = dd_corr and apellidopaternoREQ is null
            
            update sep_bitacora_wsresultadoconfronta set apellidomaternoREQ = ""  
               where idsolicitudseparacion = dd_corr and apellidomaternoREQ is null
            
            update sep_bitacora_wsresultadoconfronta set nombretrabajadorREQ = ""  
               where idsolicitudseparacion = dd_corr and nombretrabajadorREQ is null
            
            update sep_bitacora_wsresultadoconfronta set fechanacimientoREQ  = ""  
               where idsolicitudseparacion = dd_corr and fechanacimientoREQ is null
            
            update sep_bitacora_wsresultadoconfronta set entidadfederativanacimientoREQ = ""  
               where idsolicitudseparacion = dd_corr and entidadfederativanacimientoREQ is null
            
            update sep_bitacora_wsresultadoconfronta set sexoREQ = ""  
               where idsolicitudseparacion = dd_corr and sexoREQ is null
            
            update sep_bitacora_wsresultadoconfronta set indicadorunificacionpreviaREQ = ""  
               where idsolicitudseparacion = dd_corr and indicadorunificacionpreviaREQ is null
            
            update sep_bitacora_wsresultadoconfronta set clasificacionseparacionREQ = ""  
               where idsolicitudseparacion = dd_corr and clasificacionseparacionREQ is null

            update sep_bitacora_wsresultadoconfronta set indicadorcambioclasificacionREQ = ""  
               where idsolicitudseparacion = dd_corr and indicadorcambioclasificacionREQ is null

            update sep_bitacora_wsresultadoconfronta set idnegocioasociadoREQ = ""  
               where idsolicitudseparacion = dd_corr and idnegocioasociadoREQ is null

            update sep_bitacora_wsresultadoconfronta set indicadorcambionssasociadoREQ = ""  
               where idsolicitudseparacion = dd_corr and indicadorcambionssasociadoREQ is null

            update sep_bitacora_wsresultadoconfronta set idnegocioasociadonuevoREQ = ""  
               where idsolicitudseparacion = dd_corr and idnegocioasociadonuevoREQ is null

            -- fin actualiza nulos

            IF SQLCA.SQLCODE = 0 THEN
               RETURN 1
            ELSE
               RETURN 0
            END IF 

END FUNCTION

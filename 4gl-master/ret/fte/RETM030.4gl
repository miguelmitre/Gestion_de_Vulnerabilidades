#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RET016  => DEVOLUCION AV POR DOMICILIACION INDEBIDA APP               #
#Fecha creacion    => 12/02/2020                                      #
#By                => CARLOS BENITEZ                                 #
#Sistema           => APP                                                       #
#################################################################################

DATABASE safre_af

   DEFINE
       md_date          DATE
      ,mc_usuario       CHAR(10)
      ,mc_enter         CHAR(1)
      ,mc_reporte       CHAR(100)
      ,mr_modulo        RECORD LIKE seg_modulo.*
   DEFINE mar_precio_acc ARRAY [99] OF RECORD
        estado          SMALLINT     ,
        fecha           DATE         ,
        siefore         SMALLINT     ,
        precio_dia      DECIMAL(16,6)
    END RECORD

################################################################################
MAIN

   OPTIONS INPUT WRAP,
   PROMPT LINE LAST,
   MESSAGE LINE LAST,
   ERROR LINE LAST,
   ACCEPT KEY CONTROL-I
   DEFER INTERRUPT
   CALL f_lib_abre_log("RETM030")
   CALL f_init()
   CALL f_menu()


END MAIN
################################################################################

FUNCTION f_init()

   DEFINE
      lc_txt         CHAR(200)

   LET md_date = TODAY
   LET mc_usuario = f_lib_obten_user()

   SELECT *
   INTO mr_modulo.*
   FROM  seg_modulo
   WHERE modulo_cod = 'ret'
{
     LET lc_txt = ""
     LET lc_txt = "EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"
     LET lc_txt = lc_txt CLIPPED

     PREPARE eje_saldo_dia FROM lc_txt
     
         ----- DESMARCA -----
    LET lc_txt = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_txt
    
    LET lc_txt = " "
   
}
END FUNCTION
################################################################################
FUNCTION f_menu()

OPEN WINDOW frm_menu AT 2,2  WITH 21 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
DISPLAY " RETM030     DEVOLUCION AV POR DOMICILIACION INDEBIDA APP        ",md_date USING "DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)

MENU "Devolucion A.V."
         COMMAND "Selecionar" "Aportaciones Voluntarias a devolver "
               CALL f_seleccion(1)
         COMMAND "Provisionar" "Desmarca NSS Tramite Judicial"
               CALL f_seleccion(2)
         COMMAND "Liquidar" "Desmarca NSS Tramite Judicial"
               CALL f_liquida()
         COMMAND "Reporte" "Desmarca NSS Tramite Judicial"
               CALL f_reporte()
         COMMAND "Salir" "Salir del Programa"
            EXIT MENU
       END MENU

END FUNCTION

FUNCTION f_seleccion(ls_selec)
   DEFINE
      lc_curp           CHAR(18),
      lc_nss            CHAR(11) ,
      lc_busca          CHAR(100),
      lc_busca_mae      CHAR(100),
      v_bandera         SMALLINT ,
      lc_txt            CHAR(500),
      ls_status         SMALLINT,
      ls_selec          SMALLINT,
      li_consec_liquida   DECIMAL (10,0)

  # Se definen estas variables para escapar el valor del NSS y CURP  del lc_busca que genera el CONSTRUCT   
  DEFINE 
     c_curp           CHAR(18),
     c_nss            CHAR(11)



   OPEN WINDOW win1 AT 2,2 WITH FORM "RETM0301"  ATTRIBUTE (BORDER)
   DISPLAY " < Esc > Ejecutar                                            < Ctrl-C > Salir  "   AT 2,1 ATTRIBUTE(REVERSE)
   IF ls_selec = 1 THEN
      DISPLAY " RETM030                SELECCION DE APORTACIONES VOLUNTARIAS                  " AT 3,1 ATTRIBUTE (REVERSE)
   ELSE
      DISPLAY " RETM030                PROVISION DE APORTACIONES VOLUNTARIAS                  " AT 3,1 ATTRIBUTE (REVERSE)
   END IF

   DISPLAY md_date USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE (REVERSE)

   INITIALIZE lc_busca TO NULL
   INITIALIZE lc_busca_mae TO NULL

   LET INT_FLAG= FALSE
   LET v_bandera= TRUE

   WHILE (v_bandera = TRUE)
      CONSTRUCT lc_busca ON b.nss            ,
                            b.curp
                         FROM n_seguro         ,
                              n_unico
         AFTER FIELD n_seguro
               LET c_nss = GET_FLDBUF(n_seguro)
             
          AFTER FIELD n_unico
               LET c_curp = GET_FLDBUF(n_unico)
             
               

         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            LET v_bandera=FALSE
            EXIT CONSTRUCT

         ON KEY (INTERRUPT)
            LET int_flag = TRUE
            LET v_bandera=FALSE
            EXIT CONSTRUCT

         ON KEY(ESC)
            LET int_flag = FALSE
            LET c_nss = GET_FLDBUF(n_seguro)
            LET c_curp = GET_FLDBUF(n_unico)
            EXIT CONSTRUCT

      END CONSTRUCT
              
     

      #valida que la variable del construct no vaya vacia 
      LET lc_busca = lc_busca CLIPPED
      IF lc_busca = " 1=1 " OR lc_busca IS NULL AND v_bandera = TRUE THEN
         CALL f_lib_error_msg("DEBE INGRESAR AL MENOS UN DATO DE BUSQUEDA VALIDO")
      ELSE
         LET v_bandera= FALSE
      END IF

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO"
      CLEAR SCREEN
      CLOSE WINDOW win1
      RETURN
   END IF

   {DISPLAY "c_curp",c_curp
   DISPLAY "c_nss",c_nss

   DISPLAY "ls_selec",ls_selec
   DISPLAY "lc_busca",lc_busca}

   CALL f_dis_cuenta_tmp(lc_busca)

   IF ls_selec = 2 THEN
      # Se utiliza este CASE para utilizar  los valores del CURP y el NSS escapados del construct 
      CASE
            WHEN (LENGTH(c_curp CLIPPED)<>0) AND (LENGTH(c_nss CLIPPED)<>0) 
                LET lc_busca_mae = "b.nss='",c_nss,"' AND a.n_unico='",c_curp,"'"
            WHEN (LENGTH(c_curp CLIPPED)==0) AND (LENGTH(c_nss CLIPPED)<>0) 
                LET lc_busca_mae = "b.nss='",c_nss,"'" 
            WHEN (LENGTH(c_curp CLIPPED)<>0) AND (LENGTH(c_nss CLIPPED)==0) 
                LET lc_busca_mae = "a.n_unico='",c_curp,"'"
      END CASE 

      #DISPLAY "lc_busca_mae",lc_busca_mae

    #CABC FALTA CREAR LAMARCA NUEVO 494

    LET lc_txt = " SELECT b.correlativo,a.n_unico    \n",
            " FROM   cta_act_marca b, \n",
            "        afi_mae_afiliado  a  \n",
            " WHERE  a.n_seguro  = b.nss    \n",
            " AND   marca_cod = 494   \n",
            " AND "  , lc_busca_mae CLIPPED

    #DISPLAY "lc_txt",lc_txt

   PREPARE exe_cor FROM  lc_txt

   EXECUTE exe_cor INTO li_consec_liquida,lc_curp

   IF SQLCA.SQLCODE = 100 THEN
      CALL f_lib_error_msg("NO EXISTEN REGISTROS SELECCIONADOS")
      CLEAR SCREEN
      CLOSE WINDOW win1
      RETURN
   END IF

   SELECT UNIQUE 1
   FROM   ret_dev_ap_vol
   WHERE  consec_liquida = li_consec_liquida
   AND    fecha_provision  IS NULL

   IF SQLCA.SQLCODE = 100 THEN
      CALL f_lib_error_msg("REGISTROS YA PROVISIONADOS ")
      CLEAR SCREEN
      CLOSE WINDOW win1
      RETURN
   END IF

      CALL f_obtiene_precios_accion(md_date)
      CALL f_provision (li_consec_liquida , lc_curp)
      CALL f_finaliza_prov()
      LET v_bandera=FALSE
      CLOSE WINDOW win1
      RETURN
   ELSE
      LET lc_txt= " SELECT unique a.nss                        \n",
                 #" FROM    dis_cuenta a,                     \n",
                  " FROM   safre_tmp:tmp_dis_cta a,           \n",
                  "         int_det_vol_rc b                  \n",
                  " WHERE   a.nss  = b.nss                    \n",
                  " AND     a.folio = b.folio                 \n",
                  " AND     a.consecutivo_lote = b.consecutivo\n",
                  " AND     a.tipo_movimiento in (123, 310, 311) \n",
                  " AND " ,  lc_busca CLIPPED

      PREPARE bus_2 FROM lc_txt
      EXECUTE  bus_2 INTO lc_nss

      # se realiza un conteo de los registros en el for each
       # se realiza un conteo de los registros en el for each                    
       IF SQLCA.SQLCODE = 100 THEN
          SELECT 1
          FROM afi_mae_afiliado a
          WHERE a.tipo_solicitud IN (2, 7, 13, 15, 17, 28, 29) 
            AND a.n_seguro=lc_nss
         
          IF SQLCA.SQLCODE = 0 THEN           
             PROMPT "CUENTA RECIBIDA POR TRASPASO, REALIZAR DEVOLUCIÓN DE AV? (S/N)" FOR CHAR mc_enter
             IF mc_enter MATCHES "[SsNn]" THEN
                IF mc_enter MATCHES "[Ss]" THEN
                   CALL f_dev_aportacion(lc_nss, lc_curp)
                   EXIT WHILE
                ELSE
                   ERROR "PROCESO CANCELADO" SLEEP 2
                   ERROR ""
                   EXIT WHILE
                END IF
             ELSE
                ERROR "SOLO PRESIONE S o N"
             END IF       	
          END IF 
       END IF

      LET ls_status  = f_verifica_convivencia(lc_nss)

      IF ls_status = 1 THEN

         SELECT 'x'
         FROM   cta_act_marca
         WHERE  nss = lc_nss
         AND    marca_cod IN (120,130)

         IF SQLCA.SQLCODE = 0 THEN
            CALL f_lib_error_msg("CUENTA INHABILITADA ")
            CLEAR SCREEN
            CLOSE WINDOW win1
            RETURN
         END IF

         SELECT 'x'
         FROM   cta_act_marca
         WHERE  nss = lc_nss
         AND    marca_cod IN (150)

         IF SQLCA.SQLCODE = 0 THEN
            CALL f_lib_error_msg("CUENTA CANCELADA ")
            CLEAR SCREEN
            CLOSE WINDOW win1
            RETURN
         END IF

         CALL f_lib_error_msg("CUENTA EN PROCESO OPERATIVO")
         CLEAR SCREEN
         CLOSE WINDOW win1
         RETURN
      END IF

      LET lc_txt = " SELECT 1 \n ",
                   " FROM   afi_mae_afiliado  b \n ",
                   " WHERE  b.tipo_solicitud = 14                    \n ",
                   " AND   b.n_seguro = '",lc_nss,"'" CLIPPED
      --DISPLAY lc_txt
      PREPARE bus_3 FROM lc_txt
      EXECUTE  bus_3 INTO ls_status

      IF SQLCA.SQLCODE = 0 THEN
         CALL f_lib_error_msg("CUENTA INACTIVA OPERATIVO")
         CLEAR SCREEN
         CLOSE WINDOW win1
         RETURN
      END IF

   END IF

   END WHILE
   CLOSE WINDOW win1
 {     IF ls_selec = 1 THEN
      # Se utiliza este CASE para utilizar  los valores del CURP y el NSS escapados del construct 
      CASE
            WHEN (LENGTH(c_curp CLIPPED)<>0) AND (LENGTH(c_nss CLIPPED)<>0) 
                LET lc_busca_mae = "nss = '",c_nss,"' AND curp = '",c_curp,"'"
                CALL f_dis_cuenta_tmp(lc_busca_mae)
            WHEN (LENGTH(c_curp CLIPPED)==0) AND (LENGTH(c_nss CLIPPED)<>0) 
                LET lc_busca_mae = "nss = '",c_nss,"'" 
                CALL f_dis_cuenta_tmp(lc_busca_mae)
            WHEN (LENGTH(c_curp CLIPPED)<>0) AND (LENGTH(c_nss CLIPPED)==0) 
                LET lc_busca_mae = "curp = '",c_curp,"'"
                
      END CASE
         
     END IF }
   
   CALL muestra_arreglo(lc_busca)
END FUNCTION

FUNCTION f_dev_aportacion(lc_nss, lc_curp)

    DEFINE
       lc_nss            CHAR(11),
       lc_curp           CHAR(18),
       li_folio          INTEGER,
       lc_nombre         CHAR(15),
       lc_ape_paterno    CHAR(40),
       lc_ape_materno    CHAR(40),
       lf_pago           DATE, 
       lf_aportacion     DATE, 
       lc_clave          CHAR(03), 
       ld_monto_aport    DECIMAL(7,2), 
       li_tipo_aport     INTEGER, 
       lc_curp_anterior  CHAR(18), 
       lf_traspaso       DATE
       
    DEFINE lr_datos RECORD
       estado            SMALLINT,
       mensaje           CHAR(100)
    END RECORD 
    
    DEFINE v_fecha_pago  DATE, 
           v_fecha_valor DATE
           
    DEFINE lc_txt        CHAR(500)

OPEN WINDOW win5 AT 2,2 WITH FORM "RETM0305"  ATTRIBUTE (BORDER)
    DISPLAY " < Ctrl-E > Capturar Aportacion                            < Ctrl-C > Salir "   AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETM030               CAPTURA APORTACIONES A DEVOLVER                      "   AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY md_date USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE (REVERSE)

    INPUT  lc_nss, lc_curp, li_folio, lc_nombre, lc_ape_paterno, lc_ape_materno, lf_pago, lf_aportacion, lc_clave, ld_monto_aport, li_tipo_aport, lc_curp_anterior  
  	WITHOUT DEFAULTS FROM nss, curp, folio, nombre, paterno, materno, f_pago, f_aportacion, clave, monto_aport, tipo_aport, curp_anterior
  	

        AFTER FIELD folio
            IF li_folio IS NULL OR li_folio = 0 THEN
                ERROR "ERROR, INGRESE UN FOLIO VALIDO"
                NEXT FIELD folio
            ELSE 
                SELECT UNIQUE 1
                FROM dis_provision
                WHERE  folio = li_folio
            
                IF SQLCA.SQLCODE = 0 THEN
                    ERROR "REGISTRO INGRESADO PREVIAMENTE"
                    NEXT FIELD folio
                END IF
            
                SELECT UNIQUE 1
                FROM dis_cuenta
                WHERE  folio = li_folio
            
                IF SQLCA.SQLCODE = 0 THEN
                    ERROR "FOLIO YA SE ENCUENTRA LIQUIDADO"
                    NEXT FIELD folio
                END IF      
            END IF
           
        AFTER FIELD nombre
            IF lc_nombre IS NULL THEN 
            	   ERROR "ERROR, INGRESE UN NOMBRE VALIDO"
                 NEXT FIELD nombre
            END IF
           
        AFTER FIELD paterno
            IF lc_ape_paterno IS NULL THEN 
            	   ERROR "ERROR, INGRESE UN APELLIDO PAETERNO VALIDO"
            	   NEXT FIELD paterno
            END IF 
           
        AFTER FIELD f_pago      
            CALL f_lib_valida_fechas(lf_pago) RETURNING lr_datos.*
           	  
            IF  lr_datos.estado = 1 THEN 
                CALL f_lib_error_msg(lr_datos.mensaje)
            	  NEXT FIELD f_pago
            ELSE  
           	   LET lc_txt = " SELECT FIRST 1 tr.fecha_mov_banxico ",       	         
                            " FROM           taa_rcv_recepcion tr, ", 
                            "                afi_mae_afiliado am ", 
                            " WHERE          tr.fecha_mov_banxico > am.finicta ", 
                            " AND            tr.ident_operacion = '09' ", 
                            " AND            am.tipo_solicitud IN (2, 7, 13, 15, 17, 28, 29) ", 
                            " AND            am.n_seguro = tr.nss ", 
                            " AND            am.n_seguro = '",lc_nss,"'",
                            " ORDER BY       fecha_mov_banxico DESC "                       
               
               PREPARE exe_mov FROM lc_txt
               EXECUTE exe_mov INTO lf_traspaso
               
               IF lf_pago > lf_traspaso THEN
              	    ERROR "ERROR, APORTACIÓN REALIZADA POSTERIOR AL TRASPASO"
              	    NEXT FIELD f_pago 
               END IF	
           END IF
           
        AFTER FIELD f_aportacion
            CALL f_lib_valida_fechas(lf_aportacion) RETURNING lr_datos.*
            	  
            IF  lr_datos.estado = 1 THEN 
                CALL f_lib_error_msg(lr_datos.mensaje)
            	  NEXT FIELD f_aportacion
            END IF
           
        AFTER FIELD clave          
            IF lc_clave IS NULL THEN 
                ERROR "ERROR, INGRESE CLAVE DE RED COMERCIAL VALIDA"
                NEXT FIELD clave
            ELSE
            	  SELECT 1
            	  FROM tab_vol_red_com
            	  WHERE cve_rc = lc_clave
            	  
            	  IF SQLCA.SQLCODE = 100 THEN 
            	      ERROR "ERROR, CLAVE INEXISTENTE"	
            	      NEXT FIELD clave
            	  END IF
            END IF
           
        AFTER FIELD monto_aport      
            IF ld_monto_aport IS NULL THEN 
                ERROR "ERROR, INGRESE MONTO DE APORTACIÓN VOLUNTARIA VALIDA"
                NEXT FIELD monto_aport
            END IF
           
        AFTER FIELD tipo_aport      
            IF li_tipo_aport IS NULL THEN 
                ERROR "ERROR, INGRESE TIPO DE APORTACIÓN VALIDA"
                NEXT FIELD tipo_aport
            ELSE 
            	  SELECT count(tipo_apor)
            	  FROM tab_vol_redcom_subcta
            	  WHERE tipo_apor = li_tipo_aport 
            	  
            	  IF SQLCA.SQLCODE = 100 THEN 
                    ERROR "ERROR, TIPO DE APORTACIÓN INEXISTENTE"          	      	
                    NEXT FIELD tipo_aport
            	  END IF
            END IF
        
           	
        ON KEY (CONTROL-E)        	  
            PROMPT "¿CUENTA RECIBIDA POR TRASPASO, REALIZAR DEVOLUCION DE AV? (S/N)" FOR CHAR mc_enter                      
            IF mc_enter MATCHES "[SsNn]" THEN
                IF mc_enter MATCHES "[Ss]" THEN             	
                    	
                  {  SELECT 1 
                    FROM   cli_afiliado b, 
                           afi_ctr_certificacion a  
                    WHERE  a.id_afi_solicitud = b.id_afi_solicitud 
                    AND    a.tpo_solicitud IN (2, 7, 13, 15, 17, 28, 29) 
                    AND    a.nss = lc_nss}

                    SELECT 1
                    FROM afi_mae_afiliado a
                    WHERE a.tipo_solicitud IN (2, 7, 13, 15, 17, 28, 29) 
                    AND a.n_seguro=lc_nss
                            
            
                    IF SQLCA.SQLCODE = 100 THEN 
                        CALL f_lib_error_msg("LA CUENTA NO FUE RECIBIDA POR TRASPASO")
                        CLEAR SCREEN
                        CLOSE WINDOW win5
                        RETURN
                    END IF
                    	
                    SELECT UNIQUE 1
                    FROM   ret_dev_ap_vol
                    WHERE  folio =   li_folio
                    AND    nss   =   lc_nss                    
                    AND    fecha_liquida IS NOT NULL
                        
                    IF SQLCA.SQLCODE = 0 THEN
                    	  CALL f_lib_error_msg("FOLIO YA EXISTENTE")
                        CLEAR SCREEN
                        CLOSE WINDOW win5
                        RETURN
                    END IF
                        
                        
                    SELECT fecha_pago, 
                           fecha_valor
                    INTO   v_fecha_pago, 
                           v_fecha_valor
                    FROM   int_det_vol_rc  
                    WHERE  folio = li_folio                    
                    AND    nss = lc_nss
                    
                    IF SQLCA.SQLCODE = 100 THEN                        
                        CALL f_lib_error_msg("NO HAY INFORMACIÓN EN REDES COMERCIALES")
                        CLEAR SCREEN
                        CLOSE WINDOW win5
                        RETURN  
                    END IF  
                      
                    
                    INSERT INTO ret_dev_ap_vol
                        VALUES (li_folio,           --folio           
                                1,                  --consecutivo     
                                lc_nss,             --nss             
                                lc_curp,            --curp            
                                TODAY,              --fecha_seleccion 
                                ld_monto_aport,     --monto           
                                li_tipo_aport,      --tpo_apor        
                                0,                  --consec_liquida  
                                '',                 --folio_liquida   
                                USER,               --usuario_marca   
                                '',                 --usuario_prov    
                                '',                 --usuario_liquida 
                                '',                 --fecha_provision 
                                '');                --fecha_liquida   
                                                                                                    
                    CALL f_lib_error_msg("SE HA GUARDAO CORRECTAMENTE LA INFORMACIÓN")
                    EXIT INPUT
                    CLEAR SCREEN                    
                    CLOSE WINDOW win5
                    RETURN  
                ELSE
                    ERROR "PROCESO CANCELADO" SLEEP 2
                    ERROR ""
                    EXIT INPUT
                END IF
            ELSE
                ERROR "SOLO PRESIONE S o N"
            END IF   
                     
            EXIT INPUT
                       
        
       ON KEY (CONTROL-C, INTERRUPT)
          ERROR "PROCESO CANCELADO" SLEEP 2
          ERROR ""
          EXIT INPUT
          	
    END INPUT

CLOSE WINDOW win5

END FUNCTION
#==============================================================================#
# Retorna: 0 = Convive, <>0 = No convive                                       #
#==============================================================================#
FUNCTION f_verifica_convivencia(p_nss)

   DEFINE p_nss          CHAR(11),
          p_marca        SMALLINT,
          l_marca_cod    SMALLINT,
          l_rechazo      SMALLINT

   DEFINE lc_status      SMALLINT

   -----------------------------------------------------------------------------

   LET lc_status  = 0 --OK
   LET p_marca = 494

   DECLARE cur_mar CURSOR FOR
   SELECT m.marca_cod
   FROM   safre_af:cta_act_marca m
   WHERE  m.nss = p_nss

   FOREACH cur_mar INTO l_marca_cod

      SELECT c.rechazo_cod INTO   l_rechazo
      FROM   safre_af:cta_convivencia c
      WHERE  c.marca_entra  = p_marca
      AND    c.marca_activa = l_marca_cod
      IF l_rechazo != 0 THEN
          LET lc_status = 1
          EXIT FOREACH
      END IF

   END FOREACH

   RETURN lc_status

END FUNCTION

FUNCTION muestra_arreglo(v_condicion)

--DECLARACION DE VARIABLES
DEFINE v_condicion            CHAR(500)
DEFINE v_comando              CHAR(1000)
DEFINE i                      SMALLINT
DEFINE la_sol                 ARRAY[2000] OF RECORD
       marca                  CHAR(1),
       folio                  INTEGER,
       nss                    CHAR(11),
       consecutivo            DECIMAL(11,0),
       fecha_conversion       DATE,
       cve_rc                 CHAR(3)
       END RECORD
DEFINE la_monto                 ARRAY[2000] OF RECORD
       monto                  DECIMAL(13,0),
       tpo_apor               SMALLINT ,
       curp                   CHAR(18)
       END RECORD

DEFINE v_cont                 INTEGER
      ,l_case                 SMALLINT
      ,v_pos_arr              INTEGER
      ,v_pos_scr              INTEGER
      ,v_cont_inp             SMALLINT
      ,ls_rechazo             SMALLINT
      ,ls_cont                SMALLINT
      ,li_consec_liquida        DECIMAL(10,0)


    ---------------------------------------------
    --SELECCION DE LA INFORMACION A CONSULTAR
    ---------------------------------------------

   MESSAGE " PROCESANDO INFORMACION " ATTRIBUTE(REVERSE)

   LET v_comando =   " SELECT '',                     \n",
                     "        a.folio ,               \n",
                     "        a.nss ,                 \n",
                     "        b.consecutivo      ,    \n",
                     "        a.fecha_conversion ,    \n",
                     "        b.cve_rc          ,     \n",
                     "        b.monto           ,     \n",
                     "        B.tipo_apor       ,      \n",
                     "        b.curp                  \n",
                    #" FROM   dis_cuenta a       ,    \n",
                     " FROM   safre_tmp:tmp_dis_cta a, \n",
                     "        int_det_vol_rc   b      \n",
                     " WHERE   a.nss  = b.nss         \n",
                     " AND     a.folio = b.folio      \n",
                     " AND     a.consecutivo_lote = b.consecutivo  \n",  
                     " AND  ", v_condicion


   WHENEVER ERROR CONTINUE
      CLOSE WINDOW panta3
      LET v_comando = v_comando CLIPPED
      PREPARE exe1 FROM v_comando

      IF SQLCA.SQLCODE != 0 THEN
         CLEAR SCREEN
         DISPLAY SQLCA.SQLCODE," - ",v_comando
         CALL ERRORLOG(v_comando)
         CALL f_lib_error_msg("ERROR INTERNO NOTIFIQUE A SISTEMAS")
         EXIT PROGRAM
      END IF

   WHENEVER ERROR STOP

   LET v_cont = 1
   --SELECCION DE LA INFORMACION A PROCESAR
   DECLARE cur1 CURSOR FOR exe1
   FOREACH cur1 INTO la_sol[v_cont].*, la_monto[v_cont].*
   LET v_cont = v_cont + 1

      IF v_cont = 2000 THEN
         ERROR "SOLO SE MUESTRAN LOS PRIMEROS 2000 REGISTROS DE LA CONSULTA"
         SLEEP 2
         EXIT FOREACH
      END IF
   END FOREACH

   LET v_cont = v_cont - 1
   MESSAGE " "

   IF v_cont = 0 THEN
      ERROR "NO HAY INFORMACION CON EL CRITERIO SOLICITADO"
      RETURN
   END IF

   -------------------------------------------
   --DESPLEGADO DEL RESULTADO DE LA CONSULTA
   OPEN WINDOW panta3 AT 2,2 WITH FORM "RETM0302" ATTRIBUTE (BORDER)
   DISPLAY " < Esc > Ejecutar                                            < Ctrl-C > Salir  "   AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " <Ctrl-E> Seleccionar Todo               <Ctrl-F> Eliminar Seleccion      v1.0"AT 2,1
   DISPLAY " RETM030                SELECCION DE APORTACIONES VOLUNTARIAS                  " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY md_date USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE (REVERSE)


   --DESPLIEGA EL ARREGLO CON LA INFORMACION CONSULTADA

   CALL SET_COUNT(v_cont)
   LET l_case = 0
   LET i = 0
   LET v_cont_inp = TRUE
   WHILE (v_cont_inp = TRUE)
      CASE l_case
         WHEN 0 #se desmarcan todos los REGISTROS
            FOR i = 1 TO v_cont STEP 1
               LET la_sol[i].marca =" "
            END FOR
         WHEN 1 #se marcan todos los registros
            FOR i = 1 TO v_cont STEP 1
               SELECT UNIQUE 1
               FROM   ret_dev_ap_vol
               WHERE  folio =   la_sol[i].folio      
               AND    nss   =   la_sol[i].nss        
               AND    consecutivo =  la_sol[i].consecutivo
               AND    fecha_liquida IS NOT NULL
            
               IF SQLCA.SQLCODE = 0 THEN   
                  LET la_sol[i].marca =" "
               ELSE
                  LET la_sol[i].marca = "X"
               END IF 
               
            END FOR
      END CASE

      INPUT ARRAY la_sol WITHOUT DEFAULTS FROM scr_1.* ATTRIBUTES (CURRENT ROW DISPLAY = "REVERSE") HELP 8

         BEFORE ROW
            LET v_pos_arr = ARR_CURR()
            LET v_pos_scr = SCR_LINE()

            IF (v_pos_arr = v_cont + 1) THEN
               PROMPT "NO HAY MAS REGISTROS QUE MOSTRAR, DESEA SALIR (S/N) " FOR mc_enter
                  IF mc_enter MATCHES "[SsNn]" THEN
                     IF mc_enter MATCHES "[Ss]" THEN
                        LET v_cont_inp = FALSE
                        EXIT INPUT
                     END IF
                  ELSE
                     ERROR "SOLO PRESIONE S o N"
                  END IF
            ELSE
               # se mandan valores para la informacion adicional de la ventana
              CALL f_despliega_detalle( la_sol[v_pos_arr].folio       ,
                                        la_sol[v_pos_arr].nss        ,
                                        la_sol[v_pos_arr].consecutivo )
            END IF
         
         AFTER ROW
            
            SELECT UNIQUE 1
            FROM   ret_dev_ap_vol
            WHERE  folio =   la_sol[v_pos_arr].folio      
            AND    nss   =   la_sol[v_pos_arr].nss        
            AND    consecutivo =  la_sol[v_pos_arr].consecutivo
            AND    fecha_liquida IS NOT NULL
            
            IF SQLCA.SQLCODE = 0 THEN   
               CALL f_lib_error_msg("REGISTRO DEVUELTO")
               LET la_sol[v_pos_arr].marca =" "
            END IF 
               
         ON KEY (INTERRUPT,CONTROL-C)
            LET v_cont_inp = FALSE
            EXIT INPUT

          #selleccionar todos los registros
         ON KEY (CONTROL-E)
            LET l_case = 1
            LET v_cont_inp = TRUE
            EXIT INPUT

           #Quitar seleccion de todos los registros
         ON KEY (CONTROL-F)
            LET l_case = 0
            LET v_cont_inp = TRUE
            EXIT INPUT

         ON KEY (ESC)
            OPEN WINDOW aviso AT 11,16   WITH 1 ROWS, 50 COLUMNS ATTRIBUTE(BORDER,BLINK)
            PROMPT " DESEA SELECCIONAR PARA PROVISION ? (S/N)" FOR CHAR mc_enter
            CLOSE WINDOW aviso
               IF mc_enter MATCHES "[SsNn]" THEN
                  IF mc_enter MATCHES "[Ss]" THEN
                     #Envio por FOLIO
                        LET i= 0
                        LET ls_cont = 0


                        FOR i=1 TO v_cont STEP 1
                           IF la_sol[i].marca= "X" THEN
                           LET ls_rechazo = 0
                              CALL marca_cuenta(la_sol[i].nss) RETURNING ls_rechazo, li_consec_liquida
                              IF ls_rechazo <> 0 THEN
                                 OPEN WINDOW aviso AT 11,16   WITH 1 ROWS, 60 COLUMNS ATTRIBUTE(BORDER,BLINK)
                                 CALL f_lib_error_msg("CUENTA EN PROCESO OPERATIVO")
                                 LET v_cont_inp = FALSE
                                 CLOSE WINDOW aviso
                                 EXIT INPUT

                              END IF
                              INSERT INTO ret_dev_ap_vol
                              VALUES (la_sol[i].folio,
                                      la_sol[i].consecutivo,
                                      la_sol[i].nss,
                                      la_monto[i].curp,
                                      TODAY,
                                      la_monto[i].monto,
                                      la_monto[i].tpo_apor,
                                      li_consec_liquida,
                                      '',
                                      USER,
                                      '','','','')
                              LET ls_cont = ls_cont + 1
                           END IF
                        END FOR
                     OPEN WINDOW aviso AT 11,20   WITH 3 ROWS, 50 COLUMNS ATTRIBUTE(BORDER)
                     DISPLAY "REGISTROS SELECCIONADOS PARA PROVISION : " , ls_cont AT 2,4
                     CALL f_lib_error_msg("PROCESO TERMINADO ")
                     CLOSE WINDOW aviso
                     LET v_cont_inp = FALSE
                     EXIT INPUT

                  ELSE
                     LET v_cont_inp = FALSE
                      EXIT INPUT
                  END IF
               ELSE
                   ERROR "SOLO PRESIONE S o N"
               END IF
            LET v_cont_inp = TRUE
      END INPUT
   END WHILE
   CLOSE WINDOW panta3
   CALL f_seleccion(1)

END FUNCTION
#################################################################################

FUNCTION f_despliega_detalle(p_id_folio,p_id_nss,p_id_consecutivo)

DEFINE p_id_folio          INTEGER,
       p_id_nss            CHAR(11),
       p_id_consecutivo    DECIMAL(11,0)
   ,lr_datos         RECORD
      nombre         CHAR(120),
      curp           CHAR(18),
      fecha_pago     DATE,
      fecha_valor    DATE,
      monto          DECIMAL(10,2),
      tipo_apor      INTEGER,
      curp_ant       CHAR(18)
      END RECORD,
      lc_txt         CHAR(500)

   LET lc_txt = " SELECT trim(a.paterno)||' '||trim(a.materno)||' '||trim(a.nombre) nombre,",
                "        a.curp         ,  ",
                "        a.fecha_pago   ,  ",
                "        a.fecha_valor  ,  ",
                "        a.monto        ,  ",
                "        a.tipo_apor    ,  ",
                "        a.curp_ant        ",
                " FROM  int_det_vol_rc a   ",
                " WHERE a.nss  = ?         ",
                " AND   a.folio = ?        ",
                " AND   a.consecutivo = ?  "
   PREPARE exe_datos FROM  lc_txt

   EXECUTE exe_datos INTO lr_datos.*
                     USING  p_id_nss,
                            p_id_folio,
                            p_id_consecutivo
   DISPLAY BY NAME  lr_datos.*

END FUNCTION

FUNCTION f_provision (li_consec_liquida , lc_curp)

DEFINE   lc_nss               CHAR(11),
         lc_curp              CHAR(18),
         li_folio             INTEGER,
         li_consecutivo       DECIMAL (10,0),
         li_consec_liquida      DECIMAL (10,0),
         lc_txt               CHAR(100),
         lr_datos    RECORD
            folio             INTEGER,
            consecutivo       DECIMAL(11,0),
            nss               CHAR(11),
            fecha_seleccion   DATE ,
            monto             DECIMAL(13,2),
            tpo_apor          SMALLINT,
            consec_liquida      DECIMAL(10,0)
         END RECORD,
         lar_montos_tot   ARRAY[5] OF RECORD
            monto_pes           DECIMAL(16,6),
            monto_acc           DECIMAL(16,6)
         END RECORD,
         lr_saldo          RECORD
            subcta      SMALLINT,
            siefore     SMALLINT,
            acciones    DECIMAL(16,6),
            pesos       DECIMAL(16,2)    --se cambia a 2 decimales
         END RECORD,
         i              SMALLINT,
         ls_sie         SMALLINT,
         ls_cero        SMALLINT,
         ls_sub         SMALLINT,
         v_monto_acciones_tmp DECIMAL(22,6)

  WHENEVER ERROR CONTINUE
        DROP TABLE tmp_provision
  WHENEVER ERROR STOP

    SELECT *
    FROM   dis_provision
    WHERE 1=2 INTO TEMP tmp_provision

MESSAGE "CALCULANDO MONTOS ...."

INITIALIZE lr_datos.* TO NULL

FOR i= 1 TO 5 STEP 1
   LET lar_montos_tot[i].monto_acc = 0
   LET lar_montos_tot[i].monto_pes = 0
END FOR
LET ls_cero = 0

DECLARE cur_prov CURSOR FOR
SELECT   folio          ,
         consecutivo    ,
         nss            ,
         fecha_seleccion,
         monto          ,
         tpo_apor       ,
         consec_liquida
FROM   ret_dev_ap_vol
WHERE  consec_liquida = li_consec_liquida

FOREACH cur_prov INTO lr_datos.*

      LET i = lr_datos.tpo_apor
      LET lar_montos_tot[i].monto_pes=0
      
      LET lar_montos_tot[i].monto_pes = lar_montos_tot[i].monto_pes + lr_datos.monto

      DISPLAY "lr_datos.tpo_apor",lr_datos.tpo_apor

    CASE lr_datos.tpo_apor
      WHEN 1
         SELECT codigo_siefore
         INTO ls_sie
         FROM   cta_regimen
         WHERE  nss = lr_datos.nss
         AND    subcuenta = 16

         LET lar_montos_tot[i].monto_acc = lar_montos_tot[i].monto_pes / mar_precio_acc[ls_sie].precio_dia
         LET ls_sub=16
         
      WHEN 2
         SELECT codigo_siefore
         INTO ls_sie
         FROM   cta_regimen
         WHERE  nss = lr_datos.nss
         AND    subcuenta = 10

         LET lar_montos_tot[i].monto_acc = lar_montos_tot[i].monto_pes / mar_precio_acc[ls_sie].precio_dia
         LET ls_sub=10
         
      WHEN 3
         SELECT codigo_siefore
         INTO ls_sie
         FROM   cta_regimen
         WHERE  nss = lr_datos.nss
         AND    subcuenta = 12

         LET lar_montos_tot[i].monto_acc = lar_montos_tot[i].monto_pes / mar_precio_acc[ls_sie].precio_dia
         LET ls_sub=12
         
      WHEN 4
         SELECT codigo_siefore
         INTO ls_sie
         FROM   cta_regimen
         WHERE  nss = lr_datos.nss
         AND    subcuenta = 23
         LET lar_montos_tot[i].monto_acc = lar_montos_tot[i].monto_pes / mar_precio_acc[ls_sie].precio_dia
         LET ls_sub=23
         
      OTHERWISE
         SELECT codigo_siefore
         INTO ls_sie
         FROM   cta_regimen
         WHERE  nss = lr_datos.nss
         AND    subcuenta = 10
         LET lar_montos_tot[i].monto_acc = lar_montos_tot[i].monto_pes / mar_precio_acc[ls_sie].precio_dia
         LET ls_sub=10
         
   END CASE

   LET v_monto_acciones_tmp=lar_montos_tot[i].monto_acc
     LET lc_txt = ""
     LET lc_txt = "EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"
     LET lc_txt = lc_txt CLIPPED

     PREPARE eje_saldo_dia FROM lc_txt
       DECLARE cur_sdo CURSOR FOR eje_saldo_dia
       FOREACH cur_sdo USING lr_datos.nss,
                             ls_sub,
                             ls_cero,
                             md_date
                       INTO  lr_saldo.subcta, 
                             lr_saldo.siefore,
                             lr_saldo.acciones,
                             lr_saldo.pesos                          
       
           IF lr_saldo.acciones < v_monto_acciones_tmp  THEN
               LET v_monto_acciones_tmp = lr_saldo.acciones
           END IF
                      
           IF v_monto_acciones_tmp > 0 THEN 
               CALL  f_provisiona_subcta(lc_curp,
                                         lr_datos.nss,
                                         lr_saldo.subcta,
                                         li_consec_liquida,
                                         v_monto_acciones_tmp,
                                         v_monto_acciones_tmp*mar_precio_acc[ls_sie].precio_dia,
                                         md_date
                                         )	
           END IF

           LET v_monto_acciones_tmp=v_monto_acciones_tmp-lr_saldo.acciones
       END FOREACH 

END FOREACH

END FUNCTION

FUNCTION marca_cuenta(vl_nss)
#mc------------------------------------------------------
    DEFINE #loc #smallint
        vl_marca_ent                     ,
        vl_marca_res                     ,
        vl_convive_cod                   ,
        vl_cod_rechazo                   SMALLINT

    DEFINE #loc #char
        vl_nss                           CHAR(011)

    DEFINE #loc #integer
        vl_consecutivo                   INTEGER


    DEFINE #loc #reg_20
        reg_20 RECORD
              estado_marca          SMALLINT,
              codigo_rechazo        SMALLINT,
              marca_causa           SMALLINT,
              fecha_causa           DATE
    END RECORD
   DEFINE
      v_marca                       CHAR(150)
    LET vl_marca_ent = 494

     LET v_marca = " EXECUTE PROCEDURE fn_obten_ret_consecutivo() "
    PREPARE eje_concec FROM v_marca
    LET v_marca =""

    EXECUTE eje_concec INTO vl_consecutivo

    LET reg_20.estado_marca   = 0
    LET reg_20.codigo_rechazo = 0
    LET reg_20.marca_causa    = 0
    INITIALIZE reg_20.fecha_causa TO NULL

    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? ) "
    PREPARE eje_marca FROM v_marca

    WHENEVER ERROR CONTINUE
    DECLARE cur_sp CURSOR FOR eje_marca
    OPEN cur_sp USING vl_nss                , # nss
                      vl_marca_ent          , # marca entrante
                      vl_consecutivo        , # correlativo
                      reg_20.estado_marca   , # estado_marca
                      reg_20.codigo_rechazo , # codigo de rechazo
                      reg_20.marca_causa    , # marca_causa
                      reg_20.fecha_causa    , # fecha_causa
                      mc_usuario              # usuario

     FETCH cur_sp INTO vl_marca_res   , # misma marca si convive o
                      vl_cod_rechazo    # marca activa que rechaza
                                        # codigo de rechazo

     CLOSE cur_sp

     --DISPLAY vl_marca_res , " ",vl_cod_rechazo
     RETURN vl_cod_rechazo, vl_consecutivo
     WHENEVER ERROR STOP
END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion a la fecha dada  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(ldt_precios)

    DEFINE
        ldt_precios             DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_precios_acc          CHAR(100) ,
        lc_mensaje              CHAR(100) ,
        lc_siefore              CHAR(002)

    DEFINE
        ls_sie                  SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1
    --LET ldt_precios = HOY

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion(?)"
    PREPARE eje_precios_accion FROM lc_precios_acc

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING ldt_precios
                      INTO lr_precio_acc.*

            IF lr_precio_acc.estado <> 0 THEN
                LET lc_siefore = lr_precio_acc.siefore

                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: DIA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                                 " SIEFORE: ", lc_siefore CLIPPED

                CALL f_lib_error_msg(lc_mensaje)
                EXIT PROGRAM
            ELSE
                LET ls_sie                    = lr_precio_acc.siefore
                LET mar_precio_acc[ls_sie].*  = lr_precio_acc.*
            END IF
    END FOREACH

END FUNCTION

FUNCTION f_provisiona_subcta(pr_provi)

    DEFINE pr_provi RECORD
        curp        LIKE ret_trans_issste.curp          ,
        nss         LIKE ret_trans_issste.nss           ,
        subcta      SMALLINT                            ,
        consec      LIKE ret_trans_issste.consecutivo   ,
        acciones    DECIMAL(16,6)                       ,
        pesos       DECIMAL(16,6)                       ,
        fecha_proc  DATE
    END RECORD

    DEFINE
        ps_sie              SMALLINT

    DEFINE ld_precio_acc      DECIMAL (22,6)

    DEFINE
        si_provisiono       SMALLINT

    DEFINE
        ld_acc              ,
        ld_pesos            DECIMAL(16,6)

    DEFINE
        ldt_fec_proc        DATE


    DEFINE #loc #char
        folio_sua             CHAR(06) ,
        id_aporte             CHAR(12)

    -- -----------------------------------------------------------------------------

    LET si_provisiono = 0
    LET folio_sua     = ""
    LET id_aporte     = "CONTRACARGO"

    MESSAGE "INSERTANDO REGISTROS "

    SELECT codigo_siefore
    INTO   ps_sie
    FROM   cta_regimen
    WHERE  nss = pr_provi.nss
    AND    subcuenta = pr_provi.subcta

    LET ld_acc          = -pr_provi.acciones
    LET ld_pesos        = -pr_provi.pesos
    LET ld_precio_acc   = mar_precio_acc[ps_sie].precio_dia

    INSERT INTO tmp_provision
        VALUES (
         494      ,-- tipo_movimiento
         pr_provi.subcta        ,-- subcuenta
         ps_sie                 ,-- siefore
         1                      ,-- folio (temporal)
         pr_provi.consec        ,-- consecutivo_lote
         pr_provi.nss           ,-- nss
         pr_provi.curp          ,-- curp
         NULL                   ,-- folio_sua
         TODAY                    ,-- fecha_pago
         TODAY           ,-- fecha_valor
         TODAY                    ,-- fecha_conversion
         ld_pesos               ,-- monto_en_pesos
         ld_acc                 ,-- monto_en_acciones
         ld_precio_acc          ,-- precio_accion
         0                      ,-- dias_cotizados
         ""                     ,-- sucursal
         id_aporte              ,-- id_aportante
         6                      ,-- estado
         TODAY                    ,-- fecha_proceso
         mc_usuario             ,-- usuario
         TODAY                    ,-- fecha_archivo
         1                        -- etiqueta
        )

END FUNCTION


FUNCTION f_finaliza_prov()

   DEFINE li_folio            INTEGER

    LET li_folio = f_ultimo_folio()

    MESSAGE "PROVISIONANDO CUENTAS ...."
    DISPLAY "FOLIO PROVISION    : ", li_folio AT 17,13

    -- Copiamos la provision de la tabla temporal a la definitiva
    UPDATE tmp_provision
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO dis_provision
    SELECT *
    FROM   tmp_provision
    WHERE  folio = li_folio

    UPDATE ret_dev_ap_vol
    SET  folio_liquida   = li_folio,
         usuario_prov    = USER ,
         fecha_provision = md_date
    WHERE  consec_liquida IN (SELECT consecutivo_lote
                            FROM tmp_provision)

    CALL f_archivo(1)
    MESSAGE ""

   CALL f_lib_error_msg("PROVISION FINALIZADA")

END FUNCTION
#---------------------------------------------------------------------------#
# f_ultimo_folio : Obtiene el ultimo folio que se usara para procesar los   #
#                  registros de disposiciones                               #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_ult_folio     INTEGER


    SELECT MAX(A.folio) + 1
    INTO   li_ult_folio
    FROM   glo_folio A

    INSERT INTO glo_folio
    VALUES (li_ult_folio)

    RETURN li_ult_folio


END FUNCTION
FUNCTION f_archivo(ls_tipo)

   DEFINE
      lc_nom_arc          CHAR(100),
      lc_nom_arc_cza          CHAR(100),
      lc_txt              CHAR(3000),
      ls_tipo              SMALLINT,
      gc_comando CHAR(500)

MESSAGE " GENERANDO ARCHIVO . . . "
   CASE ls_tipo
      WHEN 1
         LET lc_nom_arc  = mr_modulo.ruta_listados CLIPPED, "/PROV_AV_CONTRA_DET_", TODAY USING "yymmdd_",mc_usuario CLIPPED,".txt"
         LET lc_nom_arc_cza  = mr_modulo.ruta_listados CLIPPED, "/PROV_AV_CONTRA_", TODAY USING "yymmdd_",mc_usuario CLIPPED,".txt"
         UNLOAD TO lc_nom_arc
         SELECT TO_CHAR(a.fecha_conversion,'%d/%m/%Y'),
                a.folio,
                a.curp,
                a.nss,
                SUM(b.monto),
                b.tpo_apor,
                a.Siefore,
                a.Subcuenta,
                a.tipo_movimiento ,
                a.Id_aportante,
                a.monto_en_pesos
         FROM   dis_provision a,
                ret_dev_ap_vol b
         WHERE  a.nss = b.nss
         AND    a.consecutivo_lote = b. consec_liquida
         AND    b.fecha_provision = TODAY 
         GROUP BY 1,2,3,4,6,7,8,9,10,11
      WHEN 2
         LET lc_nom_arc  = mr_modulo.ruta_listados CLIPPED, "/LIQ_AV_CONTRA_DET_", TODAY USING "yymmdd_",mc_usuario CLIPPED,".txt"
         LET lc_nom_arc_cza  = mr_modulo.ruta_listados CLIPPED, "/LIQ_AV_CONTRA_", TODAY USING "yymmdd_",mc_usuario CLIPPED,".txt"
         UNLOAD TO lc_nom_arc
         SELECT a.curp,
                a.nss,
                SUM(b.monto),
                b.tpo_apor,
                a.folio,
                TO_CHAR(a.fecha_conversion,'%d/%m/%Y'),
                a.Siefore,
                a.Subcuenta,
                a.tipo_movimiento ,
                a.monto_en_pesos ,
                a.monto_en_acciones
         FROM   dis_cuenta a,
                ret_dev_ap_vol b
         WHERE  a.nss = b.nss
         AND    a.consecutivo_lote = b. consec_liquida
         AND    b.fecha_liquida = TODAY 
         GROUP BY 1,2,4,5,6,7,8,9,10,11
      WHEN 3
         WHENEVER ERROR CONTINUE
            DROP TABLE tmp_desc_030
         WHENEVER ERROR STOP
         LET lc_nom_arc  = mr_modulo.ruta_listados CLIPPED, "/RPT_AV_CONTRA_DET_", TODAY USING "yymmdd_",mc_usuario CLIPPED,".txt" 
         LET lc_nom_arc_cza  =  mr_modulo.ruta_listados CLIPPED, "/RPT_AV_CONTRA_", TODAY USING "yymmdd_",mc_usuario CLIPPED,".txt"  
         LET lc_nom_arc = lc_nom_arc CLIPPED
         LET lc_txt = --" UNLOAD TO ",  lc_nom_arc ,
                      " SELECT b.curp,                                      \n",
                      "        b.nss   ,                                    \n",
                      "        trim(b.nombre) nombre ,                      \n",
                      "        trim(b.paterno) paterno,                     \n",
                      "        TRIM(b.materno) materno,                     \n",
                      "        to_char(b.fecha_pago,'%d/%m/%Y') fec_pago,   \n",
                      "        to_char(b.fecha_valor,'%d/%m/%Y') fec_valor,  \n",
                      "        b.cve_rc ,                                   \n",
                      "        b.monto     ,                                \n",
                      "        b.tipo_apor ,                                \n",
                      "        b.curp_ant  ,                                \n",
                      "       ( SELECT UNIQUE TO_CHAR(z.fecha_conversion ,'%d/%m/%Y')    \n",
                      "        FROM   dis_cuenta z                          \n",
                      "        WHERE  z.nss = c.nss                         \n",
                      "        AND    z.folio = c.folio                     \n",
                      "        AND    z.consecutivo_lote = c.consecutivo) fec_apor , \n",
                      "        a.folio    ,                                 \n",
                      "        a.subcuenta   ,                              \n",
                      "        a.tipo_movimiento   ,                        \n",
                      "        c.monto monto_liq   ,                              \n",
                      "        to_char(a.fecha_conversion,'%d/%m/%Y') fec_conv,                          \n",
                      "        a.monto_en_pesos   ,                         \n",
                      "        a.monto_en_acciones ,                        \n",
                      "        a.id_aportante                               \n",
                      " FROM   dis_cuenta a       ,                         \n",
                      "        int_det_vol_rc   b ,                         \n",
                      "        ret_dev_ap_vol    c                          \n",
                      " WHERE   c.nss  = b.nss                              \n",
                      " AND     c.folio = b.folio                           \n",
                      " AND     c.consecutivo = b.consecutivo               \n",
                      " AND     c.consec_liquida = a.consecutivo_lote       \n",
                      " AND     c.folio_liquida = a.folio                   \n",
                      " AND   ", mc_reporte, "\n ",
                      "into temp tmp_desc_030"
               #DISPLAY lc_txt
               PREPARE exe_rpt FROM lc_txt
               EXECUTE exe_rpt
               
               UNLOAD TO lc_nom_arc
               SELECT *
               FROM   tmp_desc_030

   END CASE

   #se llama el reporte de la generacion del encabezado
   START REPORT rpt_cza TO lc_nom_arc_cza
   OUTPUT TO REPORT rpt_cza(ls_tipo)
   FINISH REPORT rpt_cza

    #se concatena en un solo archivo el encabezado y el detalle
   LET gc_comando = "cat ", lc_nom_arc     CLIPPED, " >> ",
                            lc_nom_arc_cza     CLIPPED
   RUN gc_comando
   #se borra el archivo generado del detalle
   LET gc_comando = "rm ", lc_nom_arc CLIPPED
   RUN gc_comando


DISPLAY "ARCHIVO GENERADO   : " AT 18,13
DISPLAY lc_nom_arc  AT 19,13

END FUNCTION

FUNCTION f_liquida()

DEFINE
   li_folio          INTEGER  


  OPEN WINDOW win3 AT 2,2 WITH FORM "RETM0303"  ATTRIBUTE (BORDER)
   DISPLAY " < Esc > Ejecutar                                            < Ctrl-C > Salir  "   AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY " RETM030                  LIQUIDACION  DE CONTRACARGOS                         " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY md_date USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE (REVERSE)
 
  INPUT li_folio WITHOUT DEFAULTS FROM folio 

   AFTER FIELD folio
   
      IF li_folio IS NULL OR li_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF

      SELECT UNIQUE 1
      FROM dis_provision
      WHERE  folio = li_folio

      IF SQLCA.SQLCODE = 100 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF

      SELECT UNIQUE 1
      FROM dis_cuenta
      WHERE  folio = li_folio

      IF SQLCA.SQLCODE = 0 THEN
          ERROR "FOLIO YA SE ENCUENTRA LIQUIDADO"
          NEXT FIELD folio
      END IF

   AFTER INPUT
      IF li_folio IS NULL OR li_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF

      SELECT UNIQUE 1
      FROM dis_provision
      WHERE  folio = li_folio

      IF SQLCA.SQLCODE = 100 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF

      SELECT UNIQUE 1
      FROM dis_cuenta
      WHERE  folio = li_folio

      IF SQLCA.SQLCODE = 0 THEN
          ERROR "FOLIO YA SE ENCUENTRA LIQUIDADO"
          NEXT FIELD folio
      END IF

   ON KEY (ESC)
      LET li_folio = GET_FLDBUF(folio)
      IF li_folio IS NULL OR li_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF

      SELECT UNIQUE 1
      FROM dis_provision
      WHERE  folio = li_folio
      
      IF SQLCA.SQLCODE = 100 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF

      SELECT UNIQUE 1
      FROM dis_cuenta
      WHERE  folio = li_folio
      AND tipo_movimiento = 494

      IF SQLCA.SQLCODE = 0 THEN
          ERROR "FOLIO YA SE ENCUENTRA LIQUIDADO"
          NEXT FIELD folio
      END IF

      EXIT INPUT

   ON KEY (CONTROL-C, INTERRUPT)
      ERROR "PROCESO CANCELADO" SLEEP 2
      ERROR ""
      EXIT INPUT
END INPUT

 WHILE NOT INT_FLAG
         PROMPT "¿DESEA EJECUTAR LA LIQUIDACION ? (S/N)" FOR CHAR mc_enter
         IF mc_enter MATCHES "[SsNn]" THEN
            IF mc_enter MATCHES "[Ss]" THEN
               CALL f_liquida_proc(li_folio)
               CALL f_finaliza_liq(li_folio)
               EXIT WHILE
            ELSE
               ERROR "PROCESO CANCELADO" SLEEP 2
               ERROR ""
               EXIT WHILE
            END IF
         ELSE
            ERROR "SOLO PRESIONE S o N"
         END IF
      END WHILE

CLOSE WINDOW win3

END FUNCTION


FUNCTION f_liquida_proc(li_folio)

DEFINE
         li_folio             INTEGER,
         lr_datos_liq    RECORD
            folio             INTEGER,
            consecutivo       DECIMAL(11,0),
            nss               CHAR(11),
            curp              CHAR(18),
            fecha_seleccion   DATE ,
            monto             DECIMAL(13,2),
            tpo_apor          SMALLINT,
            consec_liquida      DECIMAL(10,0)
         END RECORD,
         lar_montos_tot   ARRAY[5] OF RECORD
            monto_pes           DECIMAL(16,6),
            monto_acc           DECIMAL(16,6)
         END RECORD,
         lr_saldo          RECORD
            subcta      SMALLINT,
            siefore     SMALLINT,
            acciones    DECIMAL(16,6),
            pesos       DECIMAL(16,2)    --se cambia a 2 decimales
         END RECORD,
         i              SMALLINT,
         ls_sie         SMALLINT,
         ls_cero        SMALLINT,
         ls_sub         SMALLINT,
         lc_txt         CHAR(100)

  WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cuenta#tmp_provision
  WHENEVER ERROR STOP

    SELECT *
    FROM   dis_cuenta
    WHERE 1=2 INTO TEMP tmp_dis_cuenta

    CALL f_obtiene_precios_accion(md_date)

MESSAGE "CALCULANDO MONTOS ...."
INITIALIZE lr_datos_liq.* TO NULL

FOR i= 1 TO 5 STEP 1
   LET lar_montos_tot[i].monto_acc = 0
   LET lar_montos_tot[i].monto_pes = 0
END FOR
LET ls_cero = 0

 DECLARE cur_liq CURSOR FOR
SELECT   folio          ,
         consecutivo    ,
         nss            ,
         curp           ,
         fecha_seleccion,
         monto          ,
         tpo_apor       ,
         consec_liquida
FROM   ret_dev_ap_vol
WHERE  folio_liquida = li_folio

   FOREACH cur_liq INTO lr_datos_liq.*
   
   LET i = lr_datos_liq.tpo_apor
     LET lc_txt = "EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"
     LET lc_txt = lc_txt CLIPPED

     PREPARE eje_saldo_dia1 FROM lc_txt
      DECLARE cur_liq_sdo CURSOR FOR eje_saldo_dia1
      FOREACH  cur_liq_sdo USING lr_datos_liq.nss,
                                   ls_cero,
                                   ls_cero,
                                   md_date INTO lr_saldo.*

            CASE
               WHEN  lr_datos_liq.tpo_apor = 1 and lr_saldo.subcta = 16
                  IF lr_saldo.pesos < lr_datos_liq.monto   THEN
                     LET lr_datos_liq.monto = lr_saldo.pesos
                  END IF

                  LET ls_sub = 16 
                  EXIT FOREACH
               WHEN lr_datos_liq.tpo_apor = 2 and lr_saldo.subcta = 10
                   IF lr_saldo.pesos < lr_datos_liq.monto   THEN
                     LET lr_datos_liq.monto = lr_saldo.pesos
                  END IF

                  LET ls_sub = 10
                  EXIT FOREACH
               WHEN lr_datos_liq.tpo_apor = 3 and lr_saldo.subcta = 12
                   IF lr_saldo.pesos < lr_datos_liq.monto   THEN
                     LET lr_datos_liq.monto = lr_saldo.pesos
                  END IF

                  LET ls_sub = 12
                  EXIT FOREACH
               WHEN lr_datos_liq.tpo_apor = 4 and lr_saldo.subcta = 23
                   IF lr_saldo.pesos < lr_datos_liq.monto   THEN
                     LET lr_datos_liq.monto = lr_saldo.pesos
                  END IF

                  LET ls_sub = 23
                  EXIT FOREACH
               WHEN lr_datos_liq.tpo_apor = 5 and lr_saldo.subcta = 10
                   IF lr_saldo.pesos < lr_datos_liq.monto   THEN
                     LET lr_datos_liq.monto = lr_saldo.pesos
                  END IF

                  LET ls_sub = 10
                  EXIT FOREACH
            END CASE

          END FOREACH

   IF lr_datos_liq.monto > 0 THEN

      CALL  f_liquida_subcta(   lr_datos_liq.curp ,
                                lr_datos_liq.nss,
                                ls_sub,
                                lr_datos_liq.consec_liquida,
                                0 ,
                                lr_datos_liq.monto,
                                md_date,
                                li_folio
                                )
   END IF

END FOREACH
   CALL f_desmarca_cuenta(lr_datos_liq.nss  ,
                          lr_datos_liq.consec_liquida )

END FUNCTION

FUNCTION f_liquida_subcta(pr_provi)

    DEFINE pr_provi RECORD
        curp        CHAR(18)           ,
        nss         CHAR(11)           ,
        subcta      SMALLINT           ,
        consec      DECIMAL(10,0)      ,
        acciones    DECIMAL(16,6)      ,
        pesos       DECIMAL(16,6)      ,
        fecha_proc  DATE               ,
        folio        INTEGER
    END RECORD

    DEFINE
        ps_sie              SMALLINT

    DEFINE ld_precio_acc      DECIMAL (22,6)

    DEFINE
        lc_curp       CHAR(18)

    DEFINE
        ld_acc              ,
        ld_pesos            DECIMAL(16,6)

    DEFINE
        ldt_fec_proc        DATE


    DEFINE #loc #char
        folio_sua             CHAR(06) ,
        id_aporte             CHAR(12)

    -- -----------------------------------------------------------------------------

    LET folio_sua     = ""
    LET id_aporte     = "CONTRACARGO"

    MESSAGE "INSERTANDO REGISTROS "

    SELECT codigo_siefore
    INTO   ps_sie
    FROM   cta_regimen
    WHERE  nss = pr_provi.nss
    AND    subcuenta = pr_provi.subcta

    LET ld_acc          = -pr_provi.pesos / mar_precio_acc[ps_sie].precio_dia
    LET ld_pesos        = -pr_provi.pesos
    LET ld_precio_acc   = mar_precio_acc[ps_sie].precio_dia

    INSERT INTO tmp_dis_cuenta
        VALUES (
         494      ,-- tipo_movimiento
         pr_provi.subcta        ,-- subcuenta
         ps_sie                 ,-- siefore
         pr_provi.folio         ,-- folio (temporal)
         pr_provi.consec        ,-- consecutivo_lote
         pr_provi.nss           ,-- nss
         pr_provi.curp          ,-- curp
         NULL                   ,-- folio_sua
         TODAY                  ,-- fecha_pago
         TODAY                  ,-- fecha_valor
         TODAY                  ,-- fecha_conversion
         ld_pesos               ,-- monto_en_pesos
         ld_acc                 ,-- monto_en_acciones
         ld_precio_acc          ,-- precio_accion
         0                      ,-- dias_cotizados
         ""                     ,-- sucursal
         id_aporte              ,-- id_aportante
         6                      ,-- estado
         TODAY                    ,-- fecha_proceso
         mc_usuario             ,-- usuario
         TODAY                    ,-- fecha_archivo
         1                        -- etiqueta
        )


END FUNCTION

FUNCTION f_finaliza_liq(li_folio)

   DEFINE li_folio            INTEGER


    MESSAGE "LIQUIDANDO  CUENTAS ...."

    -- Copiamos la provision de la tabla temporal a la definitiva

    INSERT INTO dis_cuenta
    SELECT *
    FROM   tmp_dis_cuenta
    WHERE  folio = li_folio

    UPDATE ret_dev_ap_vol
    SET  folio_liquida   = li_folio,
         usuario_liquida    = USER ,
         fecha_liquida = md_date
    WHERE  folio_liquida = li_folio

    CALL f_archivo(2)

    CALL f_lib_error_msg("LIQUIDACION FINALIZADA")

END FUNCTION

FUNCTION f_reporte()

   DEFINE
   lr_datos       RECORD
      nss            CHAR(11),
      curp           CHAR(18),
      fecha_ini      DATE     ,
      fecha_fin      DATE
      END RECORD,
      ls_bnd_fecha      SMALLINT,
      lc_cad_msg        CHAR(100)

    OPEN WINDOW win4 AT 2,2 WITH FORM "RETM0304"  ATTRIBUTE (BORDER)
   DISPLAY " < Esc > Ejecutar                                            < Ctrl-C > Salir  "   AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY " RETM030                  REPORTE      DE CONTRACARGOS                         " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY md_date USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE (REVERSE)
   
   LET lr_datos.fecha_ini = md_date
   LET lr_datos.fecha_fin = md_date
   
  INPUT lr_datos.*  WITHOUT DEFAULTS FROM nss, curp, fecha_ini, fecha_fin

   AFTER FIELD nss
      
      IF lr_datos.nss IS NOT NULL THEN 
         SELECT UNIQUE 1
         FROM ret_dev_ap_vol
         WHERE  nss = lr_datos.nss

         IF SQLCA.SQLCODE = 100 THEN
             CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
             NEXT FIELD nss
         END IF
      END IF 

   AFTER FIELD curp
      IF lr_datos.curp IS NOT NULL THEN 
         SELECT UNIQUE 1
         FROM ret_dev_ap_vol
         WHERE  curp = lr_datos.curp

         IF SQLCA.SQLCODE = 100 THEN
             CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
             NEXT FIELD curp
         END IF
      END IF 

   AFTER FIELD fecha_ini
            CALL f_lib_valida_fechas(lr_datos.fecha_ini) RETURNING ls_bnd_fecha, lc_cad_msg

            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_ini
            END IF

   AFTER FIELD fecha_fin
            CALL f_lib_valida_fechas(lr_datos.fecha_fin) RETURNING ls_bnd_fecha, lc_cad_msg

            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_fin
            ELSE
                IF (lr_datos.fecha_ini > lr_datos.fecha_fin) THEN
                    LET lr_datos.fecha_ini = NULL
                    CALL f_lib_error_msg("ERROR: FECHA INICIAL MAYOR A LA FINAL")
                    NEXT FIELD fecha_ini
                END IF

               SELECT UNIQUE 1
               FROM ret_dev_ap_vol
               WHERE  fecha_liquida BETWEEN lr_datos.fecha_ini AND lr_datos.fecha_fin

               IF SQLCA.SQLCODE = 100 THEN
                   CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
                   NEXT FIELD nss
               END IF
            END IF

   AFTER INPUT

       
      IF lr_datos.nss IS NOT NULL THEN 
         SELECT UNIQUE 1
         FROM ret_dev_ap_vol
         WHERE  nss = lr_datos.nss

         IF SQLCA.SQLCODE = 100 THEN
             CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
             NEXT FIELD nss
         END IF
      END IF 
      IF lr_datos.curp IS NOT NULL THEN 
         SELECT UNIQUE 1
         FROM ret_dev_ap_vol
         WHERE  curp = lr_datos.curp

         IF SQLCA.SQLCODE = 100 THEN
             CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
             NEXT FIELD curp
         END IF
      END IF 
      
       CALL f_lib_valida_fechas(lr_datos.fecha_ini) RETURNING ls_bnd_fecha, lc_cad_msg

            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_ini
            END IF
       CALL f_lib_valida_fechas(lr_datos.fecha_fin) RETURNING ls_bnd_fecha, lc_cad_msg

         IF ls_bnd_fecha = 1 THEN
             CALL f_lib_error_msg(lc_cad_msg)
             NEXT FIELD fecha_fin
         ELSE
             IF (lr_datos.fecha_ini > lr_datos.fecha_fin) THEN
                 LET lr_datos.fecha_ini = NULL
                 CALL f_lib_error_msg("ERROR: FECHA INICIAL MAYOR A LA FINAL")
                 NEXT FIELD fecha_ini
             END IF

            SELECT UNIQUE 1
            FROM ret_dev_ap_vol
            WHERE  fecha_liquida BETWEEN lr_datos.fecha_ini AND lr_datos.fecha_fin

            IF SQLCA.SQLCODE = 100 THEN
                CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
                NEXT FIELD fecha_ini
            END IF
         END IF

   ON KEY (ESC)

            
      IF lr_datos.nss IS NOT NULL THEN
         SELECT UNIQUE 1
         FROM ret_dev_ap_vol
         WHERE  nss = lr_datos.nss

         IF SQLCA.SQLCODE = 100 THEN
             CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
             NEXT FIELD nss
         END IF
      END IF 
      IF lr_datos.curp IS NOT NULL THEN 
         SELECT UNIQUE 1
         FROM ret_dev_ap_vol
         WHERE  curp = lr_datos.curp

         IF SQLCA.SQLCODE = 100 THEN
             CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
             NEXT FIELD curp
         END IF
      END IF 

       CALL f_lib_valida_fechas(lr_datos.fecha_ini) RETURNING ls_bnd_fecha, lc_cad_msg

            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_ini
            END IF
       CALL f_lib_valida_fechas(lr_datos.fecha_fin) RETURNING ls_bnd_fecha, lc_cad_msg

         IF ls_bnd_fecha = 1 THEN
             CALL f_lib_error_msg(lc_cad_msg)
             NEXT FIELD fecha_fin
         ELSE
             IF (lr_datos.fecha_ini > lr_datos.fecha_fin) THEN
                 LET lr_datos.fecha_ini = NULL
                 CALL f_lib_error_msg("ERROR: FECHA INICIAL MAYOR A LA FINAL")
                 NEXT FIELD fecha_ini
             END IF

            SELECT UNIQUE 1
            FROM ret_dev_ap_vol
            WHERE  fecha_liquida BETWEEN lr_datos.fecha_ini AND lr_datos.fecha_fin

            IF SQLCA.SQLCODE = 100 THEN
                CALL f_lib_error_msg("NO EXISTE INFORMACION PARA ESTE NSS")
                NEXT FIELD fecha_ini
            END IF
         END IF

      EXIT INPUT

   ON KEY (CONTROL-C, INTERRUPT)
      ERROR "PROCESO CANCELADO" SLEEP 2
      ERROR ""
      EXIT INPUT
END INPUT

   IF lr_datos.nss IS NOT NULL THEN
      LET mc_reporte = "c.nss = '", lr_datos.nss,"'"
   ELSE
      IF lr_datos.curp IS NOT NULL THEN
         LET mc_reporte = "c.curp = '", lr_datos.curp,"'"
      ELSE
         LET mc_reporte = "c.fecha_liquida BETWEEN '", lr_datos.fecha_ini , "' AND '", lr_datos.fecha_fin,"'"
      END IF
   END IF

WHILE NOT INT_FLAG
         PROMPT "¿DESEA GENERAR EL REPORTE ? (S/N)" FOR CHAR mc_enter
         IF mc_enter MATCHES "[SsNn]" THEN
            IF mc_enter MATCHES "[Ss]" THEN
               CALL f_archivo(3)
               CALL f_lib_error_msg("REPORTE FINALIZADO")
               EXIT WHILE
            ELSE
               ERROR "PROCESO CANCELADO" SLEEP 2
               ERROR ""
               EXIT WHILE
            END IF
         ELSE
            ERROR "SOLO PRESIONE S o N"
         END IF
      END WHILE

CLOSE WINDOW win4

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta cuando esta se elimine                         #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consec          LIKE ret_solicitud_tx.consecutivo   
    END RECORD,
         ls_marca       SMALLINT

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD
    DEFINE lc_txt CHAR(100)
    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0
    LET ls_marca = 494

    --DESMARCA DE LA CUENTA --
    LET lc_txt = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_txt
    EXECUTE eje_desmarca USING pr_desmarca.nss          ,--nss
                              ls_marca                  ,--marca entrante
                               pr_desmarca.consec       ,--consecutivo
                               lr_dat.edo_marca         ,--estado_marco
                               lr_dat.marca_causa       ,--marca_causa
                               mc_usuario                --usuario

END FUNCTION

################################################################################
#########codigo para la generaion del encabezado del reporte ###################
REPORT rpt_cza(p_tipo)
    DEFINE p_tipo SMALLINT
    
   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
    CASE p_tipo
        WHEN 1
           PRINT
          'FECHA CONVERSION'                    ,'|'
          ,'FOLIO'            ,'|'
          ,'CURP'                       ,'|'
          ,'NSS'                      ,'|'
          ,'MONTO'                   ,'|'
          ,'TIPO APORTACION'                   ,'|'
          ,'SIEFORE'                   ,'|'
          ,'SUBCUENTA'    ,'|'
          ,'TIPO MOVIMIENTO','|'
          ,'ID APORTANTE'            ,'|'
          ,'MONTO PESOS'        
        WHEN 2
           PRINT
          'CURP'                       ,'|'
          ,'NSS'                      ,'|'
          ,'MONTO'                   ,'|'
          ,'TIPO APORTACION'                   ,'|'
          ,'FOLIO'                   ,'|',
          'FECHA CONVERSION'                    ,'|'
          ,'SIEFORE'                   ,'|'
          ,'SUBCUENTA'    ,'|'
          ,'TIPO MOVIMIENTO','|'
          ,'MONTO PESOS' ,'|',
           'MONTO ACCIONES'
        WHEN 3
            PRINT
          'CURP'                       ,'|'
          ,'NSS'                      ,'|'
          ,'NOMBRE'                      ,'|'
          ,'PATERNO'                      ,'|'
          ,'NOMBRE'                      ,'|'
          ,'FECHA PAGO'                      ,'|'
          ,'FECHA VALOR'                      ,'|'
          ,'CLAVE'                      ,'|'
          ,'MONTO'                   ,'|'
          ,'TIPO APORTACION'                   ,'|'
          ,'CURP ANTERIOR'                      ,'|',
          'FECHA CONVERSION'                    ,'|'
          ,'FOLIO'                      ,'|'
          ,'SUBCUENTA'    ,'|'
          ,'TIPO MOVIMIENTO','|'
          ,'MONTO LIQUIDADO' ,'|',
          'FECHA CONVERSION'                    ,'|',
          'MONTO PESOS'                    ,'|',
           'MONTO ACCIONES'
    END CASE


END REPORT

FUNCTION f_dis_cuenta_tmp(v_condicion)

    DEFINE v_condicion CHAR(100)
    DEFINE v_query CHAR(500)
    DEFINE lc_dis_cuenta CHAR(60)
    #DISPLAY "v_condicion: ",v_condicion
   WHENEVER ERROR CONTINUE
   
   DATABASE safre_tmp
   
   DROP TABLE tmp_dis_cta
   
   CREATE TABLE tmp_dis_cta
   (
   tipo_movimiento     SMALLINT, 
   subcuenta           SMALLINT,
   siefore             SMALLINT,
   folio               INTEGER, 
   consecutivo_lote    INTEGER, 
   nss                 CHAR(11), 
   curp                CHAR(18), 
   folio_sua           CHAR(6), 
   fecha_pago          DATE, 
   fecha_valor         DATE, 
   fecha_conversion    DATE, 
   monto_en_pesos      DECIMAL(22,6), 
   monto_en_acciones   DECIMAL(22,6), 
   precio_accion       DECIMAL(22,6), 
   dias_cotizados      SMALLINT, 
   sucursal            CHAR(10), 
   id_aportante        CHAR(11), 
   estado              SMALLINT, 
   fecha_proceso       DATE, 
   usuario             CHAR(8), 
   fecha_archivo       DATE, 
   etiqueta            SMALLINT
   )   

   DATABASE safre_af

   #DISPLAY "ENTRA TEMP"
   

        LET v_query =  " SELECT tabname                         ",
                       " FROM   SYSTABLES                       ",
                       " WHERE  tabname MATCHES 'dis_cuenta??'  ",
                       " AND    tabname <> 'dis_cuenta_2'       ",
                       " OR     tabname =  'dis_cuenta'         ",
                       " ORDER BY 1                             "
     #DISPLAY "v_query1",v_query
     
   PREPARE sql_dis FROM v_query
   DECLARE c_dis CURSOR FOR sql_dis
   FOREACH c_dis INTO lc_dis_cuenta

     LET v_query =     " INSERT INTO safre_tmp:tmp_dis_cta ",
                       " SELECT   *                             ",
                       " FROM     ",lc_dis_cuenta CLIPPED,"   b ",
                       " WHERE    ", v_condicion CLIPPED

      #DISPLAY "v_query2",v_query
      PREPARE sql_dis_cuenta FROM v_query
      EXECUTE sql_dis_cuenta

   END FOREACH

   CREATE INDEX tmp_dis_cta_ret1 on safre_tmp:tmp_dis_cta(consecutivo_lote)
   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_dis_cta
   CREATE INDEX tmp_dis_cta_ret2 on safre_tmp:tmp_dis_cta(nss)
   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_dis_cta
   CREATE INDEX tmp_dis_cta_ret3 on safre_tmp:tmp_dis_cta(fecha_conversion)
   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_dis_cta
   CREATE INDEX tmp_dis_cta_ret4 on safre_tmp:tmp_dis_cta(subcuenta)
   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_dis_cta

END FUNCTION

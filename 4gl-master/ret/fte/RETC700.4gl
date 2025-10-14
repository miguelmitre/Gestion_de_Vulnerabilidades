################################################################################
# Proyecto          => SISTEMA DE AFORE( SAFRE )                               #
# Owner             => E.F.P.                                                  #
# Programa RETC700  => CONSULTA DE SOLICITUDES DE SALDOS PREVIOS               #
# Fecha creacion    => 13 DE OCTUBRE 2011                                      #
# By                => ISAI JIMENEZ ROJAS                                      #
# Revision          => ISAI JIMENEZ ROJAS 22/11/2011 10:34:13 a.m.             #
# Actualizacion     => ISAI JIMENEZ ROJAS 06/01/2012 12:23:36 p.m.             #
#                   => Se cambian campos fecha en Detalle                      #
#                   => 120112 - Se cambia diagnostico rechazo de 501 a 1       #
# Sistema           => RET                                                     #
#------------------------------------------------------------------------------#
#Modificacion      => MLMVUA-6                                                 #
#Fecha y Autor     => 20-11-2012 Alejandro Chagoya Salazar                     #
#Descripcion       => Se agregan campos de indicador de portabilidad y         #
#                  => fecha de vigencia al resultado de la consulta            #
################################################################################
#------------------------------------------------------------------------------#
#Modificacion      => MLM-2180                                                 #
#Fecha y Autor     => Noviembre, 06, 2013 Jairo Palafox Sanchez                #
#Descripcion       => Se modifica la consulta de saldos previos se incluye:    #
#                  => Número ISSSTE,Tipo de Trámite y Entidad Origen           #
#MLM-2359          => FSR se cambia la_sol.ip de interget a  CHAR(1)           #
################################################################################

DATABASE safre_af
GLOBALS

    DEFINE HOY                   DATE
    DEFINE f_fecha_valor_viv     DATE
    DEFINE enter                 CHAR(1)
    DEFINE g_tecla               CHAR(1)

END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN

    DEFINE v_condicion            CHAR(500)
    DEFINE v_comando              CHAR(1000)
    DEFINE v_cont                 INTEGER
    DEFINE v_descripcion          CHAR(50)
    DEFINE lr_criterio            RECORD
           nss                    LIKE ret_solicitud_saldo.nss                ,
           curp                   LIKE ret_solicitud_saldo.curp               ,
           f_recep_procesar_ini   DATE                                        ,
           f_recep_procesar_fin   DATE                                        ,
           f_vencimiento_ini      DATE                                        ,
           f_vencimiento_fin      DATE                                        ,           
           diag_recep_afore       LIKE ret_solicitud_saldo.diag_recep_afore   ,
           diag_envio_procesar    LIKE ret_solicitud_saldo.diag_envio_procesar,
           tipo_tramite           SMALLINT                                    ,
           estado_solicitud       LIKE ret_solicitud_saldo.estado_solicitud
           END RECORD
    DEFINE v_bandera              SMALLINT
    
    
    
    -- se crea la tabla temporal para la solicitud del tramite de saldos previos
    CREATE TEMP TABLE c_tramite_procesar ( estado_tramite SMALLINT, descripcion CHAR(400))
    
    -- se insertan los campos para la solicitud de saldos previos
    
    INSERT INTO c_tramite_procesar VALUES(1,"Solicitud Saldo Previo");
    INSERT INTO c_tramite_procesar VALUES(2,"Ampliación de Vigencia Solicitud Saldo Previo");


    DEFER INTERRUPT
    OPTIONS INPUT WRAP         ,
            PROMPT LINE LAST   ,
            ACCEPT KEY CONTROL-I,
            MESSAGE LINE LAST -1

    CALL STARTLOG("RETC700.log")
    
    LET HOY = TODAY
    
    ---------------------------------------
    -- CAPTURA DEL CRITERIO DE BUSQUEDA
    ---------------------------------------
      
    OPEN WINDOW w1 AT 2,2 WITH FORM "RETC7001" ATTRIBUTE(BORDER)
    DISPLAY " RETC700                CONSULTA DE SOLICITUDES                                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < ESC > Ejecutar Consulta     GENERAL RETIRO                < Ctrl-C > Salir  " AT 1,1 ATTRIBUTE(REVERSE)    
    --DISPLAY "                                               GENERAL RETIRO  " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    WHILE TRUE

       INITIALIZE lr_criterio.* TO NULL
       INITIALIZE v_condicion   TO NULL
       LET v_bandera = 0    -- para saber si hubo o no quiterio de busqueda

       #MLMVUA-6 INI --se despliega today como fechas al inicio
       LET lr_criterio.f_recep_procesar_ini = ""
       LET lr_criterio.f_recep_procesar_fin = ""
       #MLM-2120 FIN
       -- se inicializan las fechas de vencimiento como consultas actuales
       LET lr_criterio.f_vencimiento_ini = ""
       LET lr_criterio.f_vencimiento_fin = ""
       

       INPUT BY NAME  lr_criterio.* WITHOUT DEFAULTS 
           
           BEFORE INPUT 
                  DISPLAY " " TO descripcion        -- limpia la descripcion
                  DISPLAY " " TO descripcion_recep  -- limpia la descripcion
                  DISPLAY " " TO descripcion_envio  -- limpia la descripcion
                  DISPLAY " " TO desc_tpo_tramite   -- limpia la descripcion
                  
           -- fecha de recepcion
           AFTER FIELD f_recep_procesar_fin
                  IF (lr_criterio.f_recep_procesar_ini IS NULL AND 
                      lr_criterio.f_recep_procesar_fin IS NULL ) THEN 
                      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                         NEXT FIELD PREVIOUS
                      ELSE
                         NEXT FIELD NEXT
                      END IF
                  ELSE
                      IF lr_criterio.f_recep_procesar_ini IS NULL THEN      
                         ERROR "DEBE INGRESAR LA FECHA INICIAL DEL PERIODO"
                         NEXT FIELD f_recep_procesar_ini
                      END IF
                      IF lr_criterio.f_recep_procesar_fin IS NULL THEN
                         ERROR "DEBE INGRESAR LA FECHA FINAL DEL PERIODO"
                         NEXT FIELD f_recep_procesar_fin
                      END IF
                  END IF
           -- fecha de vencimiento       
           AFTER FIELD f_vencimiento_fin
                  IF (lr_criterio.f_vencimiento_ini IS NULL AND 
                      lr_criterio.f_vencimiento_fin IS NULL ) THEN 
                      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                         NEXT FIELD PREVIOUS
                      ELSE
                         NEXT FIELD NEXT
                      END IF
                  ELSE
                      IF lr_criterio.f_vencimiento_ini IS NULL THEN      
                         ERROR "DEBE INGRESAR LA FECHA INICIAL DEL VENCIMIENTO"
                         NEXT FIELD f_vencimiento_ini
                      END IF
                      IF lr_criterio.f_vencimiento_fin IS NULL THEN
                         ERROR "DEBE INGRESAR LA FECHA FINAL DEL VENCIMIENTO"
                         NEXT FIELD f_vencimiento_fin
                      END IF
                  END IF
           
          
           
           AFTER FIELD diag_recep_afore
                  IF lr_criterio.diag_recep_afore IS NULL THEN 
                     DISPLAY " " TO descripcion_recep  -- limpia la descripcion
                  ELSE
                     IF lr_criterio.diag_recep_afore = 0 THEN 
                        DISPLAY "RECHAZO" TO descripcion_recep  -- limpia la descripcion
                     ELSE
                        LET v_descripcion = f_busca_desc_diagnostico(lr_criterio.diag_recep_afore,"recepcion")
                        IF v_descripcion IS NULL THEN
                           ERROR "VALOR INVALIDO"
                           NEXT FIELD diag_recep_afore
                        ELSE
                           DISPLAY v_descripcion TO descripcion_recep
                        END IF
                     END IF 
                  END IF

           AFTER FIELD diag_envio_procesar
                  IF lr_criterio.diag_envio_procesar IS NULL THEN 
                     DISPLAY " " TO descripcion_envio  -- limpia la descripcion
                  ELSE
                     IF lr_criterio.diag_envio_procesar = 0 THEN 
                        DISPLAY "RECHAZO" TO descripcion_envio  -- limpia la descripcion
                     ELSE
                        LET v_descripcion = f_busca_desc_diagnostico(lr_criterio.diag_envio_procesar,"envio")
                        IF v_descripcion IS NULL THEN
                           ERROR "VALOR INVALIDO"
                           NEXT FIELD diag_envio_procesar
                        ELSE
                           DISPLAY v_descripcion TO descripcion_envio
                        END IF
                     END IF 
                  END IF
            -- se valida el tipo de tramite      
            AFTER FIELD tipo_tramite
             IF lr_criterio.tipo_tramite IS NULL THEN 
               
               DISPLAY " " TO desc_tpo_tramite  -- limpia la descripcion
             ELSE
               LET v_descripcion = f_busca_desc_tpo_tramite(lr_criterio.tipo_tramite)
               DISPLAY v_descripcion TO desc_tpo_tramite
                                                        
             END IF --tipo_tramite nulo   
                          
            AFTER FIELD estado_solicitud
             IF lr_criterio.estado_solicitud IS NULL THEN 
                DISPLAY " " TO descripcion  -- limpia la descripcion
             ELSE
                LET v_descripcion = f_busca_desc_estado(lr_criterio.estado_solicitud)
                DISPLAY v_descripcion TO descripcion
             END IF                                           
             
           ON KEY (CONTROL-C)
                   LET int_flag=TRUE
                   EXIT INPUT
   
           ON KEY (INTERRUPT)
                   LET int_flag=TRUE
                   EXIT INPUT
   
           ON KEY (ESC)
               LET int_flag = FALSE
               EXIT INPUT
                  
           ------------
           -- AYUDA
           ------------
           ON KEY (CONTROL-W)
              --AYUDA PARA ESTADO
              IF INFIELD(estado_solicitud) THEN
                 LET lr_criterio.estado_solicitud = f_ayuda_estado()
                 IF INT_FLAG = TRUE THEN 
                    LET INT_FLAG = FALSE
                 ELSE
                    IF lr_criterio.estado_solicitud IS NOT NULL THEN
                       LET v_descripcion = f_busca_desc_estado(lr_criterio.estado_solicitud)
                       DISPLAY BY NAME lr_criterio.estado_solicitud
                       DISPLAY v_descripcion TO descripcion     
                       NEXT FIELD NEXT 
                    END IF
                 END IF
              END IF
              
              --AYUDA PARA DIAGNOSTICO DE RECEPCION
              IF INFIELD(diag_recep_afore) THEN
                 LET lr_criterio.diag_recep_afore = f_ayuda_diagnostico("recepcion")
                 IF INT_FLAG = TRUE THEN 
                    LET INT_FLAG = FALSE
                 ELSE
                    IF lr_criterio.diag_recep_afore IS NOT NULL THEN
                       LET v_descripcion = f_busca_desc_diagnostico(lr_criterio.diag_recep_afore,"recepcion")
                       DISPLAY BY NAME lr_criterio.diag_recep_afore
                       DISPLAY v_descripcion TO descripcion_recep
                       NEXT FIELD NEXT 
                    END IF
                 END IF
              END IF

              --AYUDA PARA DIAGNOSTICO DE ENVIO
              IF INFIELD(diag_envio_procesar) THEN
                 LET lr_criterio.diag_envio_procesar = f_ayuda_diagnostico("envio")
                 IF INT_FLAG = TRUE THEN 
                    LET INT_FLAG = FALSE
                 ELSE
                    IF lr_criterio.diag_envio_procesar IS NOT NULL THEN
                       LET v_descripcion = f_busca_desc_diagnostico(lr_criterio.diag_envio_procesar,"envio")
                       DISPLAY BY NAME lr_criterio.diag_envio_procesar
                       DISPLAY v_descripcion TO descripcion_envio
                       NEXT FIELD NEXT 
                    END IF
                 END IF
              END IF
              --AYUDA tipo de tramite
              IF INFIELD(tipo_tramite) THEN
                 LET lr_criterio.tipo_tramite = f_ayuda_tpo_tramite()
                 -- INT_FLAG = TRUE THEN 
                    --ERROR "INT_FLAG"
                    -- se limpian las variables por si queda basura
                    --LET lr_criterio.tipo_tramite = NULL
                    LET INT_FLAG = FALSE
                -- ELSE
                    IF lr_criterio.tipo_tramite IS NOT NULL THEN
                       LET v_descripcion = f_busca_desc_tpo_tramite(lr_criterio.tipo_tramite)
                       DISPLAY BY NAME lr_criterio.tipo_tramite
                       DISPLAY v_descripcion TO desc_tpo_tramite                            
                       NEXT FIELD NEXT                   
                    END IF
                 --END IF
              END IF
               
       END INPUT

       IF INT_FLAG THEN
          EXIT WHILE
       ELSE

          --ARMA CRITERIO DE BUSQUEDA DE ACUERDO A LOS DATOS PROPORCIONADOS

          IF lr_criterio.nss IS NOT NULL THEN
             LET v_condicion = " ret_solicitud_saldo.nss = '", lr_criterio.nss,"'"
          END IF

          IF lr_criterio.curp IS NOT NULL THEN
             IF LENGTH(v_condicion) > 0 THEN
                LET v_condicion = v_condicion CLIPPED, " AND curp = '", lr_criterio.curp,"'"
             ELSE
                LET v_condicion = " ret_solicitud_saldo.curp = '", lr_criterio.curp,"'"
             END IF
          END IF
          IF lr_criterio.f_recep_procesar_ini IS NOT NULL AND 
             lr_criterio.f_recep_procesar_ini IS NOT NULL THEN
             IF LENGTH(v_condicion) > 0 THEN
                LET v_condicion = v_condicion CLIPPED,
                                  " AND DATE(f_recep_procesar) BETWEEN '",
                                  lr_criterio.f_recep_procesar_ini,"' AND '",
                                  lr_criterio.f_recep_procesar_fin,"'"
             ELSE
                LET v_condicion = " DATE(f_recep_procesar) BETWEEN '",
                                  lr_criterio.f_recep_procesar_ini,"' AND '",
                                  lr_criterio.f_recep_procesar_fin,"'"
             END IF
          END IF
          
       
          -- fecha de vencimiento  
          IF lr_criterio.f_vencimiento_ini IS NOT NULL AND 
            lr_criterio.f_vencimiento_ini IS NOT NULL THEN
            IF LENGTH(v_condicion) > 0 THEN
              LET v_condicion = v_condicion CLIPPED,
                                " AND DATE(fecha_vencimiento) BETWEEN '",
                                lr_criterio.f_vencimiento_ini,"' AND '",
                                lr_criterio.f_vencimiento_fin,"'"
            ELSE
              LET v_condicion = " DATE(fecha_vencimiento) BETWEEN '",
                                lr_criterio.f_vencimiento_ini,"' AND '",
                                lr_criterio.f_vencimiento_fin,"'"
            END IF
          END IF              
          --EVALUA CONDICION SOBRE diag_recep_afore
          IF lr_criterio.diag_recep_afore IS NOT NULL THEN
             IF LENGTH(v_condicion) > 0 THEN
                IF lr_criterio.diag_recep_afore = 0 THEN
                   LET v_condicion = v_condicion CLIPPED," AND diag_recep_afore != 1 "
                ELSE
                   LET v_condicion = v_condicion CLIPPED," AND diag_recep_afore = ", lr_criterio.diag_recep_afore
                END IF 
             ELSE
                IF lr_criterio.diag_recep_afore = 0 THEN
                   LET v_condicion = " diag_recep_afore != 1 "
                ELSE
                   LET v_condicion = " diag_recep_afore = ", lr_criterio.diag_recep_afore
                END IF
             END IF
          END IF

          --EVALUA CONDICION SOBRE diag_envio_procesar
          IF lr_criterio.diag_envio_procesar IS NOT NULL THEN
             IF LENGTH(v_condicion) > 0 THEN
                IF lr_criterio.diag_envio_procesar = 0 THEN
                   LET v_condicion = v_condicion CLIPPED, " AND diag_envio_procesar != 501 "
                ELSE
                   LET v_condicion = v_condicion CLIPPED, " AND diag_envio_procesar = ", lr_criterio.diag_envio_procesar
                END IF
             ELSE
                IF lr_criterio.diag_envio_procesar = 0 THEN
                   LET v_condicion = " diag_envio_procesar != 501 "
                ELSE
                   LET v_condicion = " diag_envio_procesar = ", lr_criterio.diag_envio_procesar
                END IF
             END IF
          END IF
          
   
          --EVALUA CONDICION SOBRE estado_solicitud
          IF lr_criterio.estado_solicitud IS NOT NULL THEN
            IF LENGTH(v_condicion) > 0 THEN
               LET v_condicion = v_condicion CLIPPED," AND ret_solicitud_saldo.estado_solicitud = ", lr_criterio.estado_solicitud
            ELSE
               LET v_condicion = " ret_solicitud_saldo.estado_solicitud = ", lr_criterio.estado_solicitud
            END IF
          END IF
          
          --EVALUA CONDICION SOBRE tipo_tramite
          IF lr_criterio.tipo_tramite IS NOT NULL THEN
            IF LENGTH(v_condicion) > 0 THEN
               LET v_condicion = v_condicion CLIPPED," AND ret_solicitud_saldo.tipo_tramite = ", lr_criterio.tipo_tramite
            ELSE
               LET v_condicion = " ret_solicitud_saldo.tipo_tramite = ", lr_criterio.tipo_tramite
            END IF
          END IF
          
          IF LENGTH(v_condicion) = 0 THEN
             LET v_condicion = " 1=1 "          
          END IF
          --DISPLAY v_condicion SLEEP 3
          --MUESTRA LOS DATOS RESULTANTES DE LA CONSULTA
          CALL f_muestra_arreglo(v_condicion)
        END IF
    
    END WHILE
    
    CLOSE WINDOW w1
    
END MAIN

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_muestra_arreglo(v_condicion)
    
    --DECLARACION DE VARIABLES
    DEFINE v_condicion            CHAR(500)
    DEFINE v_comando              CHAR(1000)
    DEFINE la_sol                 ARRAY[1000] OF RECORD 
           ip                     CHAR(1),
           id_solicitud_saldo     LIKE ret_solicitud_saldo.id_solicitud_saldo ,
           nss                    LIKE ret_solicitud_saldo.nss                ,
           f_recep_procesar       DATE,
           fecha_vigencia         DATE,
           diag_recep_afore       LIKE ret_solicitud_saldo.diag_recep_afore   ,
           diag_envio_procesar    LIKE ret_solicitud_saldo.diag_envio_procesar,
           estado_solicitud       LIKE ret_solicitud_saldo.estado_solicitud   ,
           desc_estado            CHAR(15),
           marca                  CHAR(1)
           END RECORD
    DEFINE v_cont                 INTEGER
    DEFINE v_pos_arr              INTEGER
    DEFINE v_pos_scr              INTEGER
    DEFINE v_cont_inp             SMALLINT
    DEFINE v_opcion               CHAR(1)
     
    ---------------------------------------------
    --SELECCION DE LA INFORMACION A CONSULTAR
    ---------------------------------------------
    
    MESSAGE " PROCESANDO INFORMACION " ATTRIBUTE(REVERSE)
    LET v_comando = " SELECT ret_solicitud_saldo.ind_portabilidad, \n",
                    " ret_solicitud_saldo.id_solicitud_saldo, \n",
                    " ret_solicitud_saldo.nss, \n",
                    " DATE(ret_solicitud_saldo.f_recep_procesar), \n",
                    " DATE(ret_folio_vigencia.fecha_vigencia), \n",
                    " ret_solicitud_saldo.diag_recep_afore, \n",
                    " ret_solicitud_saldo.diag_envio_procesar, \n",
                    " ret_solicitud_saldo.estado_solicitud, \n",
                    " ret_estado.descripcion \n",
                    " FROM ret_solicitud_saldo, ret_estado, OUTER ret_folio_vigencia \n",
                    " WHERE ret_solicitud_saldo.estado_solicitud = ret_estado.estado_solicitud \n",
                    " AND ret_solicitud_saldo.nss = ret_folio_vigencia.nss \n",
                    " AND ret_solicitud_saldo.folio_cargo = ret_folio_vigencia.folio_anterior \n",
                    " AND ",v_condicion CLIPPED, "\n",
                    " ORDER BY 4 DESC"
    --DISPLAY "v_comando", v_comando
    WHENEVER ERROR CONTINUE
    LET v_comando = v_comando CLIPPED
    PREPARE exe1 FROM v_comando
    
    IF SQLCA.SQLCODE != 0 THEN
       CLEAR SCREEN
       DISPLAY SQLCA.SQLCODE," - ",v_comando
       CALL ERRORLOG(v_comando)
       PROMPT " ERROR INTERNO NOTIFIQUE A SISTEMAS " FOR enter
       EXIT PROGRAM 
    END IF
    
    WHENEVER ERROR STOP
    
    LET v_cont = 1
        
    --SELECCION DE LA INFORMACION A PROCESAR 
       
    DECLARE cur1 CURSOR FOR exe1
    FOREACH cur1 INTO la_sol[v_cont].*
       LET v_cont = v_cont + 1

       IF v_cont = 1000 THEN
          ERROR "SOLO SE MUESTRAN LOS PRIMEROS 1000 REGISTROS DE LA CONSULTA"
          SLEEP 2
          EXIT FOREACH
       END IF
    END FOREACH

    LET v_cont = v_cont - 1

    MESSAGE " "

    IF v_cont = 0 THEN
       ERROR "NO HAY INFORMACION CON EL CRITERIO SOLICITADO"
       --SLEEP 3
       RETURN 
    END IF

    -------------------------------------------
    --DESPLEGADO DEL RESULTADO DE LA CONSULTA
    -------------------------------------------

    CURRENT WINDOW IS SCREEN

    OPEN WINDOW w2 AT 1,1 WITH FORM "RETC7002" 
    DISPLAY " RETC700                  CONSULTA DE SOLICITUDES                             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " <Ctrl-C> Salir                GENERAL RETIRO               <CTRL-P> Imprimir " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    --DESPLIEGA EL ARREGLO CON LA INFORMACION CONSULTADA

    LET v_cont_inp = TRUE

    DISPLAY v_cont TO FORMONLY.regs
    CALL SET_COUNT(v_cont)
    WHILE (v_cont_inp = TRUE)

       --MESSAGE "PRESIONE <CONTROL-P> PARA IMPRIMIR"   

       INPUT ARRAY la_sol WITHOUT DEFAULTS FROM scr_1.* 

          AFTER ROW
              LET v_pos_arr = ARR_CURR()
              LET v_pos_scr = SCR_LINE()

              DISPLAY la_sol[v_pos_arr].* TO scr_1[v_pos_scr].* ATTRIBUTE(NORMAL)

          BEFORE ROW
              LET v_pos_arr = ARR_CURR()
              LET v_pos_scr = SCR_LINE()

              IF (v_pos_arr = v_cont + 1) THEN
                  EXIT INPUT
              ELSE
                  DISPLAY la_sol[v_pos_arr].* TO scr_1[v_pos_scr].* ATTRIBUTE(REVERSE)
                  CALL f_despliega_detalle(la_sol[v_pos_arr].id_solicitud_saldo)
                  --DISPLAY la_sol[v_pos_arr].* TO scr_1[v_pos_scr].* ATTRIBUTE(REVERSE)
              END IF

          ON KEY (INTERRUPT,CONTROL-C,ESC)
              LET v_cont_inp = FALSE 
              EXIT INPUT

          ON KEY (CONTROL-P)
              --CONTROLA LA GENERACION DEL REPORTE
              CALL f_imprime_listado(v_condicion)

       END INPUT
    
    END WHILE

    CLOSE WINDOW w2

    CURRENT WINDOW IS w1

END FUNCTION

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#

FUNCTION f_despliega_detalle(p_id_solicitud_saldo)
   
   DEFINE p_id_solicitud_saldo      INTEGER
   DEFINE lr_ret_solicitud_saldo    RECORD LIKE ret_solicitud_saldo.*
   DEFINE v_descipcion              LIKE ret_estado.descripcion
   DEFINE v_fecha_sol_trabajador    DATE 
   DEFINE v_fecha_ini_pen           DATE 
   DEFINE v_fecha_vencimiento       DATE 

   SELECT *
   INTO   lr_ret_solicitud_saldo.*
   FROM   ret_solicitud_saldo
   WHERE  id_solicitud_saldo = p_id_solicitud_saldo
   --ORDER BY fecha_sol_trabajador
   
   LET v_fecha_sol_trabajador   = DATE(lr_ret_solicitud_saldo.fecha_sol_trabajador)
   LET v_fecha_ini_pen          = DATE(lr_ret_solicitud_saldo.fecha_ini_pen)
   LET v_fecha_vencimiento      = DATE(lr_ret_solicitud_saldo.fecha_vencimiento)

   DISPLAY BY NAME lr_ret_solicitud_saldo.nombre                 ,
                   lr_ret_solicitud_saldo.apellido_paterno       ,
                   lr_ret_solicitud_saldo.apellido_materno       ,
                   lr_ret_solicitud_saldo.curp                   ,
                   lr_ret_solicitud_saldo.num_issste             ,
                   lr_ret_solicitud_saldo.tipo_tramite           ,
                   lr_ret_solicitud_saldo.entidad_origen         ,
                   lr_ret_solicitud_saldo.ind_saldo_fip          ,
                   lr_ret_solicitud_saldo.ind_trasp_post_fip     ,
                   v_fecha_sol_trabajador                        ,
                   v_fecha_ini_pen                               ,
                   v_fecha_vencimiento

END FUNCTION 

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_busca_desc_estado(v_estado_solicitud)
   DEFINE v_estado_solicitud INTEGER
   DEFINE v_descripcion      CHAR(20)
   
   SELECT descripcion
   INTO   v_descripcion 
   FROM   ret_estado
   WHERE  estado_solicitud = v_estado_solicitud
   
   RETURN v_descripcion

END FUNCTION
#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_busca_desc_tpo_tramite(v_estado_tpo_tramite)
   DEFINE v_estado_tpo_tramite INTEGER
   DEFINE v_descripcion      CHAR(20)
   
   SELECT descripcion
   INTO   v_descripcion 
   FROM   c_tramite_procesar
   WHERE  estado_tramite = v_estado_tpo_tramite
   
   RETURN v_descripcion

END FUNCTION

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_busca_desc_diagnostico(v_diagnostico,v_parametro)
   
   DEFINE v_diagnostico      INTEGER
   DEFINE v_parametro        CHAR(20)
   DEFINE v_descripcion      CHAR(50)
   DEFINE v_instruccion      CHAR(100) 
   
   IF v_parametro = "recepcion" THEN
      LET v_instruccion = "SELECT descripcion FROM tab_diag_sol_recep ",
                          " WHERE  diag_recep_afore = ",v_diagnostico
   ELSE
      LET v_instruccion = "SELECT descripcion FROM tab_diag_sol_envio ",
                          " WHERE  diag_envio_procesar = ",v_diagnostico
   END IF

   PREPARE exe_diag2 FROM v_instruccion
   
   EXECUTE exe_diag2 INTO v_descripcion 
   
   RETURN v_descripcion

END FUNCTION

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_nombre_afore()

   DEFINE v_desc    CHAR(20)
   DEFINE rpt_afore RECORD
          codigo_afore          LIKE safre_af:tab_afore_local.codigo_afore ,
          razon_social          LIKE safre_af:tab_afore_local.razon_social
   END RECORD


   SELECT codigo_afore,razon_social
   INTO   rpt_afore.codigo_afore,
          rpt_afore.razon_social
   FROM   safre_af:tab_afore_local
   
   LET v_desc =  rpt_afore.codigo_afore USING "&&&", " - ",rpt_afore.razon_social 
   RETURN v_desc

END FUNCTION

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_imprime_listado(v_condicion)

    DEFINE v_condicion            CHAR(500)
    DEFINE v_comando              CHAR(1000)
    DEFINE lr_ret_solicitud_saldo RECORD LIKE ret_solicitud_saldo.*
    DEFINE lr_ret_estado          RECORD LIKE ret_estado.*
    DEFINE v_nom_rep              CHAR(200)
    DEFINE v_mensaje              CHAR(80)
    DEFINE m_usuario              CHAR(20)

    MESSAGE " PROCESANDO INFORMACION " ATTRIBUTE(REVERSE)
    
    
    LET v_comando = " SELECT * FROM ret_solicitud_saldo",
                    " WHERE  ",v_condicion

    WHENEVER ERROR CONTINUE
    
    PREPARE exe2 FROM v_comando
    
    IF SQLCA.SQLCODE != 0 THEN
       CLEAR SCREEN
       DISPLAY SQLCA.SQLCODE," - ",v_comando
       PROMPT " ERROR INTERNO NOTIFIQUE A SISTEMAS, PRESIONE <ENTER> " FOR enter
       EXIT PROGRAM 
    END IF
    
    WHENEVER ERROR STOP
    
    --INICIALIZA EL REPORTE
    
    SELECT ruta_listados
    INTO   v_nom_rep
    FROM   seg_modulo 
    WHERE  modulo_cod="ret"
    
    
    -- se obtiene el usuario activo
    LET v_comando = "SELECT USER FROM systables WHERE tabid = 1\n"   -- obtiene el usuario de informix activo
             
    PREPARE pre_usuario FROM v_comando
    EXECUTE pre_usuario  INTO m_usuario
    
    FREE pre_usuario
       
    LET v_nom_rep = v_nom_rep CLIPPED,"/",m_usuario CLIPPED,".RPT_SOL_SALPREV.", TODAY USING "DDMMYYYY"   
    
    
               
    START REPORT rpt_solicitudes TO v_nom_rep
    
    --SELECCION DE LA INFORMACION A ENVIAR A REPORTE
    DECLARE cur2 CURSOR FOR exe2
    FOREACH cur2 INTO lr_ret_solicitud_saldo.*
      
      SELECT *
      INTO   lr_ret_estado.*
      FROM   ret_estado
      WHERE  estado_solicitud = lr_ret_solicitud_saldo.estado_solicitud
      
      OUTPUT TO REPORT rpt_solicitudes(lr_ret_solicitud_saldo.*, lr_ret_estado.*)
    END FOREACH

    --FINALIZA EL REPORTE
    FINISH REPORT rpt_solicitudes
    
    LET v_mensaje = "EL REPORTE ",v_nom_rep CLIPPED," HA SIDO GENERADO"    
    MESSAGE v_mensaje ATTRIBUTE(REVERSE )
    
    PROMPT "PRESIONE <ENTER> PARA CONTINUAR:" FOR CHAR g_tecla
    MESSAGE ""
   
END FUNCTION

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_ayuda_estado()

   DEFINE la_ret_estado        ARRAY[50] OF RECORD LIKE ret_estado.*
   DEFINE v_estado_solicitud   INTEGER
   DEFINE v_posicion           INTEGER

   OPTIONS FORM LINE 1
      
   DECLARE cur_edo CURSOR FOR
      SELECT * FROM ret_estado
      WHERE  estado_solicitud >= 100

   LET v_posicion = 1
   
   FOREACH cur_edo INTO la_ret_estado[v_posicion].*
      LET v_posicion = v_posicion + 1
   END FOREACH
   
   LET v_posicion = v_posicion -1
   
   CALL SET_COUNT(v_posicion)
   
   --DESPLEGADO DEL ARREGLO
   OPEN WINDOW w_ayuda AT 8,50 WITH FORM "RETC7003" ATTRIBUTE(BORDER)

      DISPLAY ARRAY la_ret_estado TO scr_1.*
         ON KEY (INTERRUPT)
            INITIALIZE v_estado_solicitud TO NULL
            EXIT DISPLAY
            
         ON KEY (CONTROL-c)
            INITIALIZE v_estado_solicitud TO NULL
            EXIT DISPLAY
            
         ON KEY (esc)
            LET v_posicion = ARR_CURR()
            LET v_estado_solicitud = la_ret_estado[v_posicion].estado_solicitud
            EXIT DISPLAY
      END DISPLAY

   CLOSE WINDOW w_ayuda

   OPTIONS FORM LINE 3
                 
   RETURN v_estado_solicitud
   
END FUNCTION

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_ayuda_diagnostico(v_parametro)
   
   DEFINE v_parametro         CHAR(20)
   DEFINE la_diag             ARRAY[50] OF RECORD 
          diagnostico         INTEGER,
          descripcion         CHAR(50)
          END RECORD
   DEFINE v_instruccion       CHAR(80)
   DEFINE v_diagnostico       INTEGER
   DEFINE v_posicion          INTEGER

   OPTIONS FORM LINE 1
      
   IF v_parametro = "recepcion" THEN
      LET v_instruccion = "SELECT * FROM tab_diag_sol_recep"
   ELSE
      LET v_instruccion = "SELECT * FROM tab_diag_sol_envio"
   END IF
      
   PREPARE exe_diag FROM v_instruccion
   
   DECLARE cur_diagnostico CURSOR FOR exe_diag

   LET v_posicion = 1
   
   FOREACH cur_diagnostico INTO la_diag[v_posicion].*
      LET v_posicion = v_posicion + 1
   END FOREACH
   
   LET v_posicion = v_posicion -1
   
   CALL SET_COUNT(v_posicion)
   
   --DESPLEGADO DEL ARREGLO
   OPEN WINDOW w_diag AT 8,38 WITH FORM "RETC7004" ATTRIBUTE(BORDER)

      DISPLAY ARRAY la_diag TO scr_1.*
         ON KEY (INTERRUPT)
            INITIALIZE v_diagnostico TO NULL
            EXIT DISPLAY
            
         ON KEY (CONTROL-c)
            INITIALIZE v_diagnostico TO NULL
            EXIT DISPLAY
            
         ON KEY (esc)
            LET v_posicion = ARR_CURR()
            LET v_diagnostico = la_diag[v_posicion].diagnostico
            EXIT DISPLAY
      END DISPLAY

   CLOSE WINDOW w_diag

   OPTIONS FORM LINE 3
                 
   RETURN v_diagnostico
   
END FUNCTION

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#
FUNCTION f_ayuda_tpo_tramite()

   DEFINE la_ret_tpo_tramite  ARRAY[50] OF RECORD 
          tipo_tramite         INTEGER,
          descripcion          CHAR(50)
          END RECORD
   DEFINE v_estado_tpo_tramite   INTEGER
   DEFINE v_posicion             INTEGER

   OPTIONS FORM LINE 1
     
   DECLARE cur_tpo_tramite CURSOR FOR 
     SELECT estado_tramite, descripcion 
     FROM c_tramite_procesar      
     
   -- se incia el contador de registros
   LET v_posicion = 1
   
   FOREACH cur_tpo_tramite INTO la_ret_tpo_tramite[v_posicion].*
     -- se incrementa el contador
     LET v_posicion = v_posicion + 1
     --DISPLAY "la_ret_tpo_tramite.*,",la_ret_tpo_tramite[v_posicion].*
   
   END FOREACH
   -- se decrementa el contador
   LET v_posicion = v_posicion -1
   -- verifica que por los menos se imprima el primer registro del arreglo
   CALL SET_COUNT(v_posicion)
   
   --DISPLAY "v_posicion",v_posicion 
   --DESPLEGADO DEL ARREGLO
   OPEN WINDOW w_ayuda_tpo_tram AT 8,45 WITH FORM "RETC7005" ATTRIBUTE(BORDER)

      DISPLAY ARRAY la_ret_tpo_tramite TO scr_1.*
         ON KEY (INTERRUPT)
            INITIALIZE v_estado_tpo_tramite TO NULL
            EXIT DISPLAY
            
         ON KEY (CONTROL-c)
            INITIALIZE v_estado_tpo_tramite TO NULL
            EXIT DISPLAY
            
         ON KEY (esc)
            LET v_posicion = ARR_CURR()
            LET v_estado_tpo_tramite = la_ret_tpo_tramite[v_posicion].tipo_tramite
            EXIT DISPLAY
      END DISPLAY

   CLOSE WINDOW w_ayuda_tpo_tram

   OPTIONS FORM LINE 3
   RETURN v_estado_tpo_tramite
   
END FUNCTION

#==============================================================================#
#                                                                              #
#                                                                              #
#==============================================================================#

REPORT rpt_solicitudes(lr_ret_solicitud_saldo,lr_ret_estado)

   DEFINE
       lr_ret_solicitud_saldo RECORD LIKE ret_solicitud_saldo.*,
       lr_ret_estado          RECORD LIKE ret_estado.*,
       v_linea                CHAR(130)       
       
   OUTPUT
       PAGE LENGTH    60
       LEFT MARGIN     0
       RIGHT MARGIN  150
       TOP MARGIN      0
       BOTTOM MARGIN   0

   FORMAT
       
     PAGE HEADER 
         PRINT COLUMN 1, 
           "NSS"             , "|",
           "CURP"            , "|",
           "TPO_TRAMITE"     , "|",
           "FOLIO_PROCESAR"  , "|",
           "NUM_ISSSTE"      , "|",
           "FEC_VENCIMIENTO" , "|",
           "IMP_RET97"       , "|",
           "IMP_CS"          , "|",
           "IMP_CES_VEJ"     , "|",
           "IMP_VIV97"       , "|",
           "S_VIV"           , "|",
           "FEC_SOLICITUD"   , "|",
           "DIAG_SOL"        , "|",
           "DIAG_AFORE"      , "|",
           "EDO_SOL"         , "|"
                               
     ON EVERY ROW
        PRINT COLUMN  1, 
          lr_ret_solicitud_saldo.nss                                         , "|",
          lr_ret_solicitud_saldo.curp                                      , "|",
          lr_ret_solicitud_saldo.tipo_tramite                              , "|",          
          lr_ret_solicitud_saldo.folio_t_procesar CLIPPED                  , "|",                    
          lr_ret_solicitud_saldo.num_issste                                , "|",                    
          lr_ret_solicitud_saldo.fecha_vencimiento                         , "|",  
          lr_ret_solicitud_saldo.pes_ret97_post         USING "#######&.&&", "|",
          lr_ret_solicitud_saldo.pes_cs_post            USING "#######&.&&", "|",
          lr_ret_solicitud_saldo.pes_cv_post            USING "#######&.&&", "|",
          lr_ret_solicitud_saldo.pes_viv97_post         USING "#######&.&&", "|",
          lr_ret_solicitud_saldo.estado_vivienda        USING "##&"        , "|",
          DATE(lr_ret_solicitud_saldo.f_recep_procesar) USING "dd/mm/yyyy" , "|",
          lr_ret_solicitud_saldo.diag_recep_afore       USING "&&&"        , "|",
          lr_ret_solicitud_saldo.diag_envio_procesar    USING "&&&"        , "|",
          lr_ret_solicitud_saldo.estado_solicitud       USING "&&&"        , "|"         
              
     ON LAST ROW
        SKIP 1 LINES
                       
        PRINT COLUMN  1," TOTALES: "                                            , "|", 
                                                                                  "|", 
                                                                                  "|",
                                                                                  "|",
                                                                                  "|",
                                                                                  "|",
          SUM(lr_ret_solicitud_saldo.pes_ret97_post)         USING "#######&.&&", "|",
          SUM(lr_ret_solicitud_saldo.pes_cs_post)            USING "#######&.&&", "|",
          SUM(lr_ret_solicitud_saldo.pes_cv_post)            USING "#######&.&&", "|",
          SUM(lr_ret_solicitud_saldo.pes_viv97_post)         USING "#######&.&&", "|"                
          
        PRINT COLUMN  1,"TOTAL DE REGISTROS: ", COUNT(*) USING "<<<,<<<,<<&",     "|"
END REPORT

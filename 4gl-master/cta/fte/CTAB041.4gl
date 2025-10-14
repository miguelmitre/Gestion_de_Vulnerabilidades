################################################################################
#Proyecto     => safre_af                                                      #
#Propietario  => E.F.P.                                                        #
#Programa     => CTAB041                                                       #
#Descripcion  => IDENTIFICACION POR EDAD                                       #
#Por          => MIGUEL ANGEL HERNANDEZ MARTINEZ                               #
#Fecha        => 22 de Julio de 2008.                                          #
#Fecha Mod    => 7 de agosto de 2008.                                          #
#Fecha Mod    => 18 de Diciembre de 2008.                                      #
#Sistema      => CTA.                                                          #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE gc_mensaje1           ,
          gc_mensaje2           ,
          gc_mensaje3           CHAR(100),
          gc_mensaje4           CHAR(100),
          gc_usuario            CHAR(8)  ,
          gc_respuesta          CHAR(001),
          permisos              CHAR(100)

   DEFINE g_cancela             SMALLINT

   DEFINE gd_fecha_corte,
          gd_fecha_valuacion    DATE

   DEFINE arr_c,
          arr_l,
          arr_t                 SMALLINT,
          GUSER                 CHAR(08),
          hoy                   DATE,
          pos                   SMALLINT,
          pos_subcta            SMALLINT

   DEFINE gi_siefore,
          gi_estatus,
          gi_siefore_ced,
          gi_siefore_rec        SMALLINT

   DEFINE gi_folio_liq INTEGER

   DEFINE gar_consulta_liq      ARRAY[400] OF RECORD
             folio              INTEGER,
             subcuenta          SMALLINT,
             siefore_ced        SMALLINT,
             pesos_ced          DECIMAL(22,6),
             acciones_ced       DECIMAL(22,6),
             siefore_rec        SMALLINT,
             pesos_rec          DECIMAL(22,6),
             acciones_rec       DECIMAL(22,6)
      END RECORD

   DEFINE gr_reporte            RECORD
          afore_desc            CHAR(14),
          precio1               DECIMAL(7,6),
          precio2               DECIMAL(7,6),
          precio3               DECIMAL(7,6),
          precio4               DECIMAL(7,6),
          precio5               DECIMAL(7,6),
          siefore1              CHAR(9),
          siefore2              CHAR(9),
          siefore3              CHAR(9),
          siefore4              CHAR(9),
          siefore5              CHAR(9)
      END RECORD

   DEFINE gar_regimen           ARRAY[20] OF RECORD
          codigo_siefore        SMALLINT,
          desc_siefore          CHAR(30),
          total                 INTEGER
      END RECORD

   DEFINE f_orden_sel           CHAR(200),
          f_act_ind_eda         CHAR(200),
          v_comando             CHAR(1000),
          f_sdo_edad            CHAR(200)

   DEFINE reg                   RECORD
          fecha_corte           DATE,
          fecha_edad            DATE
      END RECORD

   DEFINE hay_reg               INTEGER
   DEFINE lc_query              CHAR(3000)

  DEFINE   lc_Proceso                  CHAR(10)
  DEFINE   ls_Etapa                    SMALLINT 
  DEFINE   li_Etapas                   SMALLINT 
  DEFINE   lc_Comenta                  CHAR(50)
  DEFINE   ld_FechaInicio              DATE
  DEFINE   li_Consecutivo              INTEGER
  DEFINE   ls_Control                  SMALLINT

  DEFINE   gr_CifEdad                  ARRAY[10] OF RECORD   
           gr_ley                      CHAR(25),
           gr_Total                    INTEGER,
           gr_NoCamR                   INTEGER,
           gr_CamReg                   INTEGER,
           gr_Proced                   INTEGER, 
           gr_NoProc                   INTEGER,
           gr_Desmar                   INTEGER
           END RECORD 
           
  DEFINE   gc_opc                      CHAR(01)
          
  DEFINE   gr_CifSie                   ARRAY[10,10] OF RECORD 
           afilia                      INTEGER,
           accion                      DECIMAL(22,6),
           pesos                       DECIMAL(22,6) 
           END RECORD 

  DEFINE    gr_todos1                  ARRAY[10] OF RECORD   
            gr_ley                     CHAR(10),
            gr_Cue01                   INTEGER,
            gr_Cue02                   INTEGER,
            gr_Cue03                   INTEGER,
            gr_Cue04                   INTEGER,
            gr_Cue05                   INTEGER,
            gr_Acc01                   DECIMAL(22,6),
            gr_Acc02                   DECIMAL(22,6),
            gr_Acc03                   DECIMAL(22,6),
            gr_Acc04                   DECIMAL(22,6),
            gr_Acc05                   DECIMAL(22,6),
            gr_Pes01                   DECIMAL(22,6),
            gr_Pes02                   DECIMAL(22,6),
            gr_Pes03                   DECIMAL(22,6),
            gr_Pes04                   DECIMAL(22,6),
            gr_Pes05                   DECIMAL(22,6)
            END RECORD 
            
END GLOBALS

################################################################################
MAIN

   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
   
   DEFER    INTERRUPT

   CALL     inicio()
   CALL     menu()

END MAIN

################################################################################
FUNCTION inicio()

   SELECT USER
   INTO   GUSER
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   LET hoy = TODAY
   LET     f_orden_sel     =  " EXECUTE FUNCTION fn_transfiere_ie (?,?,?,?,?,?,?) "
   PREPARE p_orden_sel   FROM f_orden_sel

   LET     f_act_ind_eda   =  " EXECUTE FUNCTION fn_actualiza_ind_edad (?,?,?) "
   PREPARE p_act_ind_eda FROM f_act_ind_eda

   LET     lc_query        =  'UPDATE STATISTICS FOR TABLE safre_af:cta_transf_edad ; '
   PREPARE p_UpdSta1                      FROM  lc_query 

   LET     lc_query        =  'UPDATE STATISTICS FOR TABLE safre_af:cta_transf_ed_gpo '
   PREPARE p_UpdSta2                      FROM  lc_query 

   LET     lc_query  =
           "  SELECT  COUNT(*)                  ",
           "    FROM  cta_transf_edad           ",
           "   WHERE  fecha_corte = ?           "
   PREPARE p_VerTrans    FROM lc_query 

END FUNCTION

################################################################################
FUNCTION menu()

   OPEN WINDOW win1 AT 2,2 WITH FORM "CTAB0411" ATTRIBUTE(BORDER)
   DISPLAY " CTAB041                   IDENTIFICACION POR EDAD                          " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

   MENU "CAMBIO SIEFORE ANUAL"
      COMMAND "IdentificaPrevia " "Identifica PREVIA Afiliados para Cambio de Regimen por Edad"
         CALL  f_ident_regs(1)

      COMMAND "IdentificaDefinitiva " "Identifica DEFINITIVA Afiliados para Cambio de Regimen por Edad"
         CALL  f_ident_regs(2)

      COMMAND "Reporte " "Genera Archivo Plano de Afiliados para Cambio de Regimen por Edad"
       CALL  f_m_gen_reporte()

      COMMAND "salDos" "Prepara saldos de cuentas a transferir"
         CALL  fn_obtiene_saldos()

      COMMAND "Cambio Regimen " "Cambio de Regimen y Crea Solicitudes de Transferencia"
         CALL proceso(1)

      COMMAND "soLicitudes " "Prepara Solicitudes de Transferecia de Recursos " 
         CALL  cambio_estado()
         
      COMMAND "Preliquidacion " "Preliquidacion de Transferencia de Recursos"
         CALL preliquida()

      COMMAND "Liquidacion " "Liquidacion de Transferencia de Recursos"
         CALL liquida()         

      COMMAND "Archivo" "Generar archivo Op 81"
         CALL  lee_fecha()
         
      COMMAND 'Verifica Proceso' 'Permite revisar Inicio-Final de los procesos que se ejcutan'
         LET   lc_Proceso  = 'CTAB041' 
         LET   li_Etapas   = 10         
         CALL  f_ConsultaProceso( lc_Proceso, li_Etapas)
         
      COMMAND 'Cifras'           ' Permite revisar cifras generadas por el proceso ' 
         CALL  f_Cifras() 
         
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU
   CLOSE WINDOW win1
END FUNCTION

################################################################################
FUNCTION f_ident_regs(p_tipo)

   DEFINE   p_tipo                      SMALLINT,    #1 = previa, 2 = definitiva
            ls_etapa                    SMALLINT     #Etapa 10 previa
   DEFINE   lc_Comenta1                 CHAR(50)   
   DEFINE   ld_FechaInicio              DATE
   DEFINE   ls_resultado                SMALLINT
   DEFINE   ld_HoraInicio               CHAR(08)
   DEFINE   ld_HoraTermino              CHAR(08)
   DEFINE   l_db                        CHAR(15)          #variable para base de datos
   DEFINE   l_folio                     DECIMAL(16)

   DEFINE   x_tipo_proceso              SMALLINT,
            opc                         CHAR(1),
            ls_hay_reg                  INTEGER
   DEFINE   lc_elproc                   CHAR(200)
   DEFINE   GUSE                        CHAR(10)

   LET INT_FLAG = FALSE

   
--   OPEN WINDOW win4 AT 5,2 WITH FORM "CTAB0414"
--   DISPLAY "[Esc] Ejecutar  [Ctrl-C] Salir " AT 1,1 ATTRIBUTE(REVERSE)
   
   OPEN WINDOW win4 AT 2,2 WITH FORM "CTAB0414" ATTRIBUTE(BORDER)
   DISPLAY "  [Esc] Ejecutar                                          [Ctrl-C] Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "CTAB041                IDENTIFICACION POR EDAD                 ",HOY USING"DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)

 
    IF p_tipo = 1 THEN      # previa
       LET l_db = "safre_tmp"
       LET ls_Etapa = 10
       LET lc_Comenta     =  'Identificacion Previa por Edad'
    ELSE                    #definitiva
       LET l_db = "safre_af"
       LET ls_Etapa = 1
       LET lc_Comenta     =  'Identificacion Definitiva por Edad'
    END IF

    LET l_db = l_db CLIPPED

    DATABASE l_db

   DISPLAY "BASE_ASIGNADA: ", l_db

   LET     lc_query  =
           "  SELECT  COUNT(*)                  ",
           "    FROM  cta_transf_edad           ",
           "   WHERE  fecha_corte = ?           "
   PREPARE p_VerTrans2    FROM lc_query 

   INPUT BY NAME  reg.fecha_corte,
                  reg.fecha_edad

      AFTER FIELD fecha_corte
         IF reg.fecha_corte   IS NULL THEN
            ERROR "Fecha de Corte Edad no puede quedar vacia ..."
            NEXT FIELD fecha_corte
         END IF

         EXECUTE  p_VerTrans2    USING  reg.fecha_corte
                                 INTO   ls_hay_reg
         IF ls_hay_reg > 0 THEN
            IF p_tipo = 1 THEN     # previa
               PROMPT  "YA EXISTE INFORMACION PARA ESTA FECHA, ¿DESEA ELIMINARLA? (S/N) " ATTRIBUTE(REVERSE) FOR opc
               IF opc MATCHES '[Ss]' THEN
                  CALL f_del_ident(reg.fecha_corte)
                  NEXT FIELD fecha_edad
               ELSE
                  NEXT FIELD fecha_corte    
               END IF
            ELSE
               PROMPT  "YA EXISTE INFORMACION PARA ESTA FECHA, VERIFIQUE " ATTRIBUTE(REVERSE) FOR opc
               NEXT FIELD fecha_corte
            END IF
         END IF
         
      AFTER FIELD fecha_edad
         IF reg.fecha_edad    IS NULL THEN
              ERROR "Fecha para calculo de Saldos no puede quedar vacia ..."
              NEXT FIELD fecha_edad
         END IF

      ON KEY(ESC)
         IF reg.fecha_corte   IS NULL THEN
            ERROR "Fecha de Corte Edad no puede quedar vacia ..."
            NEXT FIELD fecha_corte
         END IF
         EXECUTE  p_VerTrans2    USING  reg.fecha_corte
                                 INTO   ls_hay_reg
        IF ls_hay_reg > 0 THEN
            IF p_tipo = 1 THEN     # previa
               PROMPT  "YA EXISTE INFORMACION PARA ESTA FECHA, ¿DESEA ELIMINARLA? (S/N) " ATTRIBUTE(REVERSE) FOR opc
               IF opc MATCHES '[Ss]' THEN
                  CALL f_del_ident(reg.fecha_corte)
                  NEXT FIELD fecha_edad
               ELSE
                  NEXT FIELD fecha_corte    
               END IF
            ELSE
               PROMPT  "YA EXISTE INFORMACION PARA ESTA FECHA, VERIFIQUE " ATTRIBUTE(REVERSE) FOR opc
               NEXT FIELD fecha_corte
            END IF
         END IF

         IF reg.fecha_edad    IS NULL THEN
              ERROR "Fecha para Saldos no puede quedar vacia ..."
              NEXT FIELD fecha_edad
         END IF

         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
         ERROR   "PROCESO CANCELADO .."
   ELSE 
         PROMPT  "Desea Ejecutar el Proceso [S/N] ... " ATTRIBUTE(REVERSE)
            FOR  opc ATTRIBUTE(REVERSE)

            IF opc MATCHES '[Ss]' THEN
         --      CALL    f_quita_indic0es()
               LET     lc_Proceso     =  'CTAB041'
--               LET     lc_Comenta     =  'Identificacion Previa por Edad'
               LET     ld_FechaInicio =  reg.fecha_corte
               LET     ls_Etapa       =  1
               LET     ls_Control     =  0
               LET     li_Consecutivo =  0
            
              CALL   f_VerProc(lc_Proceso, ls_Etapa) RETURNING ls_resultado,
                                                             ld_FechaInicio,
                                                             ld_HoraInicio,
                                                             ld_HoraTermino,
                                                             lc_Comenta1,
                                                             li_Consecutivo
        --DISPLAY "RESULTADO: ", ld_HoraTermino
             IF ld_HoraTermino IS NULL OR ld_HoraTermino = "" THEN  
              ### Existe algo ejecutando
                PROMPT ' No se puede EJECUTAR, EXISTE un proceso que aún no termina ', 
                      lc_Proceso CLIPPED, '-', ls_Etapa, 
                    ' REVISE .. ' FOR opc ATTRIBUTE(REVERSE)
                 LET     ls_Control   = 0
                 RETURN
             ELSE          
               SELECT  USER
               INTO  GUSER
               FROM  safre_af:seg_modulo
               WHERE  modulo_cod = "cta"

               IF p_tipo = 1 THEN
                  LET     lc_elproc  =  "\"EXECUTE PROCEDURE sp_transf_edad_tmp( '",
                                     reg.fecha_corte,      "', '",
                                     reg.fecha_edad,       "') \" "
                  LET     lc_elproc  =  "echo ", lc_elproc CLIPPED, " > ", GUSER CLIPPED, ".eje_transf_edad_tmp.sql"

                  LET     v_comando  =  "nohup time dbaccess safre_tmp ",
                                        GUSER CLIPPED, ".eje_transf_edad_tmp.sql "

                  LET ls_Etapa = 10

               ELSE
                  LET     lc_elproc  =  "\"EXECUTE PROCEDURE sp_transferencia_edad( '",
                                     reg.fecha_corte,      "', '",
                                     reg.fecha_edad,       "') \" "
                  LET     lc_elproc  =  "echo ", lc_elproc CLIPPED, " > ", GUSER CLIPPED, ".eje_transf_edad.sql"

                  LET     v_comando  =  "nohup time dbaccess safre_tmp ",
                                         GUSER CLIPPED, ".eje_transf_edad.sql "
                  LET ls_Etapa = 1  

               END IF --DE TIPO DEFINITIVA O PREVIA
               SLEEP 3
               RUN     lc_elproc
              
               LET     v_comando  =  v_comando CLIPPED

               CALL    f_GeneraProceso(lc_Proceso,
                                       lc_Comenta,
                                       ld_FechaInicio,
                                       ls_Etapa,
                                       v_comando,
                                       reg.fecha_corte,
                                       reg.fecha_edad)  RETURNING ls_Control,
                                                                  li_Consecutivo

                                                                  
                                                                  
                                                                  
                  IF      li_Consecutivo    > 0  THEN
                       PROMPT    "Presione ENTER para Continuar"  ATTRIBUTE(REVERSE)
                               FOR opc ATTRIBUTE (REVERSE)
                         
                  ELSE
                        ERROR ' NO FUE POSIBLE registrar el proceso .. ', lc_Proceso CLIPPED,'-',ls_Etapa
                        SLEEP 5
                        ERROR '    '
                  END IF

                  WHENEVER ERROR CONTINUE
                       INSERT INTO safre_tmp:tmp_fechas_transferencia
                       VALUES (TODAY, reg.fecha_corte, reg.fecha_edad, p_tipo)

                  WHENEVER ERROR STOP 
                  
             END IF --IF LRESULTADO
          ELSE
               ERROR  "PROCESO CANCELADO .."
          END IF --IF SI
   END IF --INT_FLAG

   CLEAR FORM
   CLEAR SCREEN
   CLOSE WINDOW win4
   RETURN
END FUNCTION

################################################################################
FUNCTION proceso(x_tipo_proceso)

 DEFINE   x_tipo_proceso              SMALLINT,
          opc                         CHAR(1)
 DEFINE   lc_elproc                   CHAR(200)
 DEFINE   GUSE                        CHAR(10)
 DEFINE   ls_Continua                 SMALLINT 
 DEFINE   lc_Proceso                  CHAR(10)
 DEFINE   ls_Etapa                    SMALLINT
 DEFINE   ls_resultado                SMALLINT
 DEFINE   ld_FechaInicio              DATE
 DEFINE   ld_HoraInicio               CHAR(08)
 DEFINE   ld_HoraTermino              CHAR(08)
 DEFINE   lc_Comenta1                 CHAR(50)
 DEFINE   li_Consecutivo              INTEGER

 LET   ls_Continua        =    0
 --- Verificar que no exista en ejcucuión algun paso 1 o 2 

 LET    lc_Proceso              =  'CTAB041'
 LET    ls_Etapa                =  1
 FOR    ls_Etapa                =  1 TO 2 
     LET    ls_Etapa            =  1
     LET    ls_resultado        =  0
     CALL   f_VerProc(lc_Proceso, ls_Etapa)     RETURNING  ls_resultado,
                                                           ld_FechaInicio,
                                                           ld_HoraInicio,
                                                           ld_HoraTermino,
                                                           lc_Comenta1,
                                                           li_Consecutivo

     IF ld_HoraTermino IS NULL OR ld_HoraTermino = "" THEN  
            ### En ejecucion paso 1
            ERROR  'No se puede EJECUTAR, EXISTE un proceso que aún no termina ', lc_Proceso CLIPPED, '-', ls_Etapa,
                   'REVISE .. '
            SLEEP  10
            ERROR  ' '
            LET    ls_Continua   = 1
     END IF
     LET    ls_Etapa             =  ls_Etapa + 1
 END FOR

 IF    ls_Continua        =    0     THEN  
      LET INT_FLAG = FALSE
      OPEN WINDOW win2 AT 5,2 WITH FORM "CTAB0412"
      DISPLAY "[Ctrl-C] Salir " AT 1,1 ATTRIBUTE(REVERSE)

      INPUT BY NAME reg.fecha_corte
   
        BEFORE FIELD fecha_corte 
            ---   busque y sugiera ultima fecha procesada ... 
            SELECT  MAX(fecha_corte)
              INTO  reg.fecha_corte 
              FROM  cta_transf_edad

            DISPLAY reg.fecha_corte TO fecha_corte 
   
         AFTER FIELD  fecha_corte
            IF reg.fecha_corte IS NULL THEN
               ERROR  "Fecha de Corte no puede ser vacia .."
               NEXT   FIELD fecha_corte
            END IF

            LET     hay_reg          =   0 
            IF      x_tipo_proceso   =   1  THEN   
                 SELECT  COUNT(*)
                   INTO  hay_reg
                   FROM  cta_transf_edad
                  WHERE  fecha_corte = reg.fecha_corte
                    AND  cod_rechazo = 2   
            END IF
   
            IF hay_reg       =   0  THEN
               ERROR "No hay Información para la Fecha de Corte que solicita"
               NEXT FIELD fecha_corte
            END IF
   
            EXIT INPUT
   
         ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT INPUT
   
      END INPUT

      IF INT_FLAG THEN
         LET INT_FLAG = FALSE
         CLEAR FORM
         CLEAR SCREEN
         CLOSE WINDOW win2
      ELSE
          LET     opc =  '' 
          PROMPT "Desea ejecutar el proceso [S/N] ... " ATTRIBUTE (REVERSE)
              FOR opc ATTRIBUTE (REVERSE)
    
          IF opc MATCHES '[Ss]' THEN
             IF x_tipo_proceso = 1 THEN
                CALL cambio_regimen()
             END IF
       
             IF x_tipo_proceso = 2 THEN
                CALL reintento_cambio_regimen()
             END IF
    
             IF x_tipo_proceso = 3 THEN
                CALL cambio_indicador_edad()
             END IF
       
             PROMPT "SELECCIONE ENTER PARA SALIR"
                     FOR opc
          ELSE
             ERROR "PROCESO CANCELADO .."
          END IF
    
          CLEAR FORM
          CLEAR SCREEN
          CLOSE WINDOW win2
      END IF
 END IF
 RETURN
END FUNCTION

#####################################################################
FUNCTION cambio_regimen()

   DEFINE  vnss            CHAR(11),
           sw_procesados   INTEGER ,
           vind_edad       SMALLINT

   DEFINE  r RECORD
           nss             CHAR(11),
           siefore         SMALLINT,
           grupo_regimen   SMALLINT
   END RECORD

   DEFINE  x_existe        SMALLINT,
           x_ind_edad      SMALLINT,
           x_rechazo       SMALLINT,
           x_folio         INTEGER,
           x_tipo_traspaso SMALLINT,
           x_medio         SMALLINT,
           x_tipo_proceso  SMALLINT,
           sw_aceptado     INTEGER,
           sw_rechazado    INTEGER,
           opc             CHAR(1)

   LET     lc_Proceso     =  'CTAB041'
   LET     lc_Comenta     =  'Actualización de Regimen por Edad'
   LET     ld_FechaInicio =  reg.fecha_corte
   LET     ls_Etapa       =  2
   LET     ls_Control     =  0
   LET     li_Consecutivo =  0

   SELECT  USER
     INTO  GUSER
     FROM  safre_af:seg_modulo
    WHERE  modulo_cod = "cta"

   ---- fecha corte      ----- la fecha de corte
   ---- estado           ----- registros a considerar .. en que estado deben estar según maquinaria ... (2) 

   LET     v_comando    =  "nohup time fglgo CTAB042 ",
                           "'", reg.fecha_corte, "' ",
                           "2 " 
   LET     v_comando    =  v_comando CLIPPED

   CALL    f_GeneraProceso(lc_Proceso,
                           lc_Comenta,
                           ld_FechaInicio,
                           ls_Etapa,
                           v_comando,
                           reg.fecha_corte,
                           reg.fecha_edad)  RETURNING ls_Control,
                                                      li_Consecutivo
  
   IF      li_Consecutivo    > 0  THEN
        PROMPT    "Presione ENTER para Continuar"  ATTRIBUTE(REVERSE)
           FOR opc ATTRIBUTE (REVERSE)
   ELSE
        ERROR ' NO FUE POSIBLE registrar el proceso .. ', lc_Proceso CLIPPED,'-',ls_Etapa
        SLEEP 5
        ERROR '    '
   END IF

END FUNCTION

################################################################################
FUNCTION reintento_cambio_regimen()

   DEFINE vnss            CHAR(11),
          sw_procesados   INTEGER ,
          vind_edad       SMALLINT

   DEFINE r RECORD
          nss             CHAR(11),
          ind_edad        SMALLINT,
          siefore         SMALLINT,
          grupo_regimen   SMALLINT
   END RECORD

   DEFINE x_existe        SMALLINT,
          x_ind_edad      SMALLINT,
          x_rechazo       SMALLINT,
          x_folio         INTEGER,
          x_tipo_traspaso SMALLINT,
          x_medio         SMALLINT,
          x_tipo_proceso  SMALLINT,
          sw_aceptado     INTEGER,
          sw_rechazado    INTEGER

   LET    x_tipo_traspaso     = 13
   LET    x_medio             = 10
   LET    x_tipo_proceso      = 2

   LET    sw_procesados       = 0
   LET    sw_aceptado         = 0
   LET    sw_rechazado        = 0

   ERROR "PROCESANDO INFORMACION"

   DECLARE cur_3 CURSOR FOR
   SELECT  c.nss,
           c.ind_edad,
           cg.siefore,
           cg.grupo_regimen
   FROM    cta_transf_ed_gpo cg,
           cta_transf_edad c
   WHERE   cg.nss = c.nss
   AND     cg.fecha_corte  = reg.fecha_corte
   AND     c.fecha_corte   = cg.fecha_corte
   AND     cg.estado       = 2
   ORDER BY 1,2

   FOREACH cur_3 INTO r.*
      LET sw_procesados = sw_procesados + 1

      DISPLAY "PROCESADOS:",sw_procesados AT 09,3 ATTRIBUTE(REVERSE)

      DECLARE cur_4 CURSOR FOR p_orden_sel          
      OPEN    cur_4 USING r.nss,             --- nss
                          r.ind_edad,         --- ind_edad
                          r.grupo_regimen,   --- grupo_regimen
                          r.siefore,         --- siefore
                          x_tipo_proceso,    --- tipo_proceso
                          x_tipo_traspaso,   --- tipo_traspaso
                          x_medio            --- medio

         FETCH cur_4 INTO x_existe,
                          x_ind_edad,
                          x_rechazo,
                          x_folio

         IF x_rechazo = 0 THEN
            LET sw_aceptado = sw_aceptado + 1
         ELSE
            LET sw_rechazado = sw_rechazado + 1
         END IF
      CLOSE   cur_4

      INSERT  INTO cta_his_transf_ed_gpo
      SELECT  nss,
              grupo_regimen,
              siefore_ant,
              siefore,
              folio_transf,
              estado,
              intento,
              fecha_corte,
              factualiza,
              today,
              usuario
        FROM  cta_transf_ed_gpo
       WHERE  nss = r.nss
         AND  grupo_regimen = r.grupo_regimen
         AND  fecha_corte = reg.fecha_corte
         AND  siefore = r.siefore

      UPDATE  cta_transf_ed_gpo
         SET  estado = x_rechazo,
              folio_transf = x_folio,
              intento = intento + 1,
              factualiza = TODAY
       WHERE  nss = r.nss
         AND  grupo_regimen = r.grupo_regimen
         AND  fecha_corte = reg.fecha_corte
         AND  siefore = r.siefore

      DISPLAY "ACEPTADOS :",sw_aceptado AT 11,3 ATTRIBUTE(REVERSE)
      DISPLAY "RECHAZADOS:",sw_rechazado AT 13,3 ATTRIBUTE(REVERSE)

   END FOREACH

   ERROR "PROCESO TERMINADO..."
END FUNCTION

################################################################################
FUNCTION cambio_estado()

   DEFINE   arr_1              ARRAY[20] OF RECORD
            grupo_regimen      INTEGER,
            total              INTEGER
   END RECORD

   DEFINE   i                  SMALLINT,
            total_total        INTEGER,
            opc                CHAR(1),
            ls_Control         SMALLINT 

   DISPLAY "[Ctrl-C] Cancelar  [Enter] Aceptar "                                            AT 4,1 ATTRIBUTE(REVERSE)
   OPEN WINDOW voc92 AT 7,3 WITH FORM "CTAB0413" ATTRIBUTE(BORDER)
   DISPLAY "     GRUPO REGIMEN                TOTAL REGISTROS                             " AT 1,1 ATTRIBUTE(REVERSE)

   LET         ls_Control           =  0 
   WHILE TRUE 
        LET         i               =  1
        DECLARE cur_5 CURSOR FOR
             SELECT  grupo_regimen, COUNT(*)
               FROM  tes_solicitud
              WHERE  tipo_traspaso  =  13
                AND  estado         =  10
              GROUP  BY 1
              ORDER  BY 1
        FOREACH cur_5              INTO arr_1[i].grupo_regimen,
                                        arr_1[i].total
            LET     total_total = total_total    +     arr_1[i].total
            LET     i = i + 1
        END FOREACH
     
        LET         i = i - 1
        IF      i   >=   1 THEN
            CALL    SET_COUNT(i)
            DISPLAY ARRAY arr_1      TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET   ls_Control  =  1
                    EXIT DISPLAY
      
                 ON KEY ( CONTROL-M )
                    PROMPT  "Desea Ejecutar el Proceso [S/N] ... " ATTRIBUTE(REVERSE)
                       FOR  opc ATTRIBUTE(REVERSE)
   
                    IF opc MATCHES '[Ss]' THEN
                               CALL actualiza_tes_solicitud()
                               PROMPT " TOTAL REGISTROS ACTUALIZADOS: ",
                                      total_total,
                                      " ENTER PARA SALIR" ATTRIBUTE(REVERSE)
                                  FOR opc
                               EXIT DISPLAY
                    END IF
            END DISPLAY
        ELSE
            LET        ls_Control  = 1
            ERROR      " NO HAY SOLICITUDES " ATTRIBUTE(NORMAL)
        END IF

        IF   ls_Control            = 1  THEN 
             EXIT WHILE
        END IF
   END WHILE
   CLOSE WINDOW voc92
END FUNCTION

################################################################################
FUNCTION actualiza_tes_solicitud()

   UPDATE  tes_solicitud
      SET  estado        = 100
    WHERE  tipo_traspaso = 13
      AND  estado        = 10

END FUNCTION

################################################################################
FUNCTION cambio_indicador_edad()

   DEFINE vnss            CHAR(11),
          sw_procesados   INTEGER ,
          vind_edad       SMALLINT

   DEFINE r1 RECORD
          nss             CHAR(11),
          ind_edad        SMALLINT,
          criterio        SMALLINT
   END RECORD

   DEFINE x_existe        SMALLINT,
          x_ind_edad      SMALLINT,
          x_rechazo       SMALLINT,
          sw_aceptado     INTEGER,
          sw_rechazado    INTEGER

   LET sw_procesados = 0
   LET sw_aceptado = 0
   LET sw_rechazado = 0

   ERROR "PROCESANDO INFORMACION DE ACTUALIZACION DE INDICADOR EDAD"

   DECLARE cur_6 CURSOR FOR
   SELECT c.nss,
          c.ind_edad,
          c.criterio
   FROM   cta_transf_ed_gpo cg,
          cta_transf_edad c
   WHERE  cg.nss = c.nss
   AND    c.fecha_corte = reg.fecha_corte
   AND    c.fecha_corte = cg.fecha_corte
   AND    cg.estado = 0
   ORDER BY 1,2

   FOREACH cur_6 INTO r1.nss,
                      r1.ind_edad,
                      r1.criterio

      LET sw_procesados = sw_procesados + 1

      DISPLAY "REGISTROS PROCESADOS PARA ACTUALIZAR INDICADOR EDAD:",sw_procesados AT 13,3 ATTRIBUTE(REVERSE)

      DECLARE cur_7 CURSOR FOR p_act_ind_eda
      OPEN    cur_7 USING r1.nss,             --- nss
                          r1.ind_edad,        --- ind_edad
                          r1.criterio         --- criterio

         FETCH cur_7 INTO x_rechazo

         IF x_rechazo = 1 THEN
            LET sw_aceptado = sw_aceptado + 1
         ELSE
            LET sw_rechazado = sw_rechazado + 1
         END IF
      CLOSE   cur_7

      DISPLAY "REGISTROS ACEPTADOS DE ACTUALIZACION INDICADOR EDAD :",sw_aceptado AT 14,3 ATTRIBUTE(REVERSE)
      DISPLAY "REGISTROS RECHAZADOS DE ACTUALIZACION INDICADOR EDAD:",sw_rechazado AT 15,3 ATTRIBUTE(REVERSE)

   END FOREACH

END FUNCTION
################################################################################
FUNCTION lee_fecha()

   DEFINE opc       CHAR(1)
   DEFINE ls_accion SMALLINT

   DISPLAY "                 GENERACION   DEL   ARCHIVO   OPERACION 81                     " AT 5,1 ATTRIBUTE(REVERSE)

   LET INT_FLAG = FALSE

   OPEN WINDOW ventana2 AT 7,2 WITH FORM "CTAB0412"

   INPUT BY NAME reg.fecha_corte

      AFTER FIELD fecha_corte
         IF reg.fecha_corte IS NULL THEN
            ERROR "Fecha de corte de periodo no puede ser nulo.."
            NEXT FIELD fecha_corte
         END IF

         SELECT COUNT(*)
         INTO   hay_reg
         FROM   cta_transf_edad
         WHERE  fecha_corte = reg.fecha_corte

         IF hay_reg = 0 THEN
            ERROR "No hay registros para la Fecha de corte..."
            NEXT FIELD fecha_corte
         END IF

         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLEAR FORM
      CLEAR SCREEN
      CLOSE WINDOW ventana2
      RETURN
   END IF

   LET ls_accion = 0

   WHILE TRUE
      PROMPT "ESTA SEGURO DE CONTINUAR EJECUCION [S/N]:" FOR opc

      IF opc  MATCHES "[SsNn]" THEN
         IF opc MATCHES "[Ss]" THEN
                  LET ls_accion = 1
                  EXIT WHILE
         ELSE
                  LET ls_accion = 0
            EXIT WHILE
         END IF
      ELSE
         ERROR "SOLO INDIQUE S o N "
         SLEEP 2
         ERROR ""
         CONTINUE WHILE
      END IF
   END WHILE

   IF ls_accion = 1 THEN
          CALL ejecuta_archivo(reg.fecha_corte)
   ELSE
          ERROR "PROCESO CANCELADO."
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana2
   CLEAR FORM
   CLEAR SCREEN
   RETURN
END FUNCTION

################################################################################
FUNCTION ejecuta_archivo(ld_fecha_corte)
   DEFINE ld_fecha_corte DATE

   DEFINE li_totnss,
          li_folio_proceso INTEGER

   DEFINE lc_comando CHAR(1000)
   DEFINE lc_msg     CHAR(100)
   DEFINE lc_ok      CHAR(001)

   CALL inserta_paso_reporte(ld_fecha_corte, "CTAC126") RETURNING li_totnss,
                                                                  li_folio_proceso

   LET lc_comando = "nohup time fglgo CTAC126.4gi " CLIPPED,
                     li_folio_proceso CLIPPED, " ",
                     ld_fecha_corte   CLIPPED, " ",
                     li_totnss        CLIPPED, " ",

                     "1> ", GUSER CLIPPED, "_op81.salida ",
                     "2> ", GUSER CLIPPED, "_op81.error &"

   ERROR "EJECUTANDO PROCESO POR NOHUP."
   SLEEP 2
   RUN lc_comando
   {DISPLAY "lc_comando: ", lc_comando CLIPPED
   SLEEP 8}
   ERROR "VERIFIQUE ARCHIVOS:", GUSER CLIPPED, "_op81.salida  Y ", GUSER CLIPPED, "_op81.error"
   SLEEP 3
   ERROR ""

   LET lc_msg = "Folio: ",li_folio_proceso CLIPPED, " Proc Cod: CTAC126"
   PROMPT lc_msg CLIPPED FOR CHAR lc_ok ATTRIBUTE (REVERSE)

END FUNCTION

################################################################################
FUNCTION inserta_paso_reporte(ld_fecha_corte, lc_proceso_cod)
   DEFINE li_folio_proceso INTEGER,
          lc_hora_inicial  CHAR(08),
          li_totnss        INTEGER,
          ld_fecha_corte   DATE,
          lc_proceso_cod   CHAR(10)

   LET lc_hora_inicial  = TIME

   SELECT "X"
   FROM safre_af:cta_folio

   IF SQLCA.SQLCODE = 0 THEN
      SELECT folio + 1
      INTO   li_folio_proceso
      FROM   safre_af:cta_folio

      UPDATE safre_af:cta_folio
      SET    folio = li_folio_proceso
   ELSE
      LET li_folio_proceso = 1

      INSERT INTO safre_af:cta_folio
      VALUES (li_folio_proceso)
   END IF

   SELECT COUNT(*)
   INTO   li_totnss
   FROM   cta_transf_edad
   WHERE  fecha_corte = ld_fecha_corte

   INSERT INTO safre_af:dis_ctrl_proceso
   VALUES (TODAY,                   -- fecha_proceso
           lc_proceso_cod,          -- proceso_cod
           1,                       -- etapa_cod   -- LECTURA
           lc_hora_inicial,         -- hora_inicial
           NULL,                    -- hora_final
           "Op. 81",                -- parametro1
           ld_fecha_corte,          -- parametro2 --fecha_corte
           li_totnss,               -- parametro3 --tot_nss
           NULL,                    -- parametro4
           NULL,                    -- parametro5
           li_folio_proceso,        -- folio
           NULL,                    -- resultado
           GUSER,                   -- usuario
           0                        -- consecutivo
          )

   IF SQLCA.SQLCODE <> 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA safre_af:dis_ctrl_proceso ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

   RETURN li_totnss, li_folio_proceso

END FUNCTION


################################################################################
FUNCTION f_crea_indices()
 DEFINE   ls_ind                      SMALLINT
 DEFINE   lc_Proceso                  CHAR(10)
 DEFINE   ls_Etapa                    SMALLINT
 DEFINE   ls_resultado                SMALLINT
 DEFINE   ld_FechaInicio              DATE
 DEFINE   ld_HoraInicio               CHAR(08)
 DEFINE   ld_HoraTermino              CHAR(08)
 DEFINE   lc_Comenta1                 CHAR(50)
 DEFINE   li_Consecutivo              INTEGER

 LET    lc_Proceso          =  'CTAB041' 
 LET    ls_Etapa            =  1
 LET    ls_resultado        =  0
 CALL   f_VerProc(lc_Proceso, ls_Etapa)     RETURNING  ls_resultado,
                                                       ld_FechaInicio,
                                                       ld_HoraInicio,
                                                       ld_HoraTermino,
                                                       lc_Comenta1,
                                                       li_Consecutivo
 IF ld_HoraTermino IS NULL OR ld_HoraTermino = "" THEN  
        ### En ejecucion paso 1
 ELSE
        LET     ls_ind   =   0 
        SELECT  COUNT(*)
          INTO  ls_ind 
          FROM  sysindices
         WHERE  idxname = 'ix_cta_transf_edad1'
       
        IF     ls_ind   =  0   THEN   
              SQL LOCK TABLE cta_transf_edad IN EXCLUSIVE MODE ;
              END SQL
              SQL CREATE INDEX 'safre'.ix_cta_transf_edad1                          
                              ON 'safre'.cta_transf_edad (nss,ind_edad,etapa)         
                              IN  cta_dbs1;                                           
              END SQL
              SQL UPDATE STATISTICS FOR TABLE cta_transf_edad ; 
              END SQL 
              SQL UNLOCK TABLE cta_transf_edad;
              END SQL
        END IF
       
        LET     ls_ind   =   0 
        SELECT  COUNT(*)
          INTO  ls_ind 
          FROM  sysindices
         WHERE  idxname = 'ix_cta_transf_edad2'
       
        IF     ls_ind   =  0   THEN   
              SQL LOCK TABLE cta_transf_edad IN EXCLUSIVE MODE ;
              END SQL
              SQL CREATE INDEX 'safre'.ix_cta_transf_edad2                          
                              ON 'safre'.cta_transf_edad (fecha_corte)                
                              IN  cta_dbs1;                                           
              END SQL
              SQL UPDATE STATISTICS FOR TABLE cta_transf_edad ; 
              END SQL 
              SQL UNLOCK TABLE cta_transf_edad;
              END SQL
        END IF
            
        LET     ls_ind   =   0 
        SELECT  COUNT(*)
          INTO  ls_ind 
          FROM  sysindices
         WHERE  idxname = 'ix_cta_transf_ed_gpo_1'
       
        IF     ls_ind   =  0   THEN   
              SQL LOCK TABLE cta_transf_ed_gpo IN EXCLUSIVE MODE ;
              END SQL
              SQL CREATE INDEX 'safre'.ix_cta_transf_ed_gpo_1                       
                              ON 'safre'.cta_transf_ed_gpo (nss,grupo_regimen)        
                              IN  cta_dbs1;                                          
              END SQL
              SQL UPDATE STATISTICS FOR TABLE cta_transf_ed_gpo ; 
              END SQL 
              SQL UNLOCK TABLE cta_transf_ed_gpo;
              END SQL
        END IF
       
        LET     ls_ind   =   0
        SELECT  COUNT(*)
          INTO  ls_ind
          FROM  sysindices
         WHERE  idxname = 'ix_cta_transf_ed_gpo_2'
       
        IF     ls_ind   =  0   THEN
              SQL LOCK TABLE cta_transf_ed_gpo IN EXCLUSIVE MODE ;
              END SQL
              SQL CREATE INDEX 'safre'.ix_cta_transf_ed_gpo_2                       
                              ON 'safre'.cta_transf_ed_gpo (siefore_ant,siefore)      
                              IN  cta_dbs1;                                          
              END SQL
              SQL UPDATE STATISTICS FOR TABLE cta_transf_ed_gpo ; 
              END SQL 
              SQL UNLOCK TABLE cta_transf_ed_gpo;
              END SQL
        END IF
 END IF
        
END FUNCTION

################################################################################
FUNCTION f_quita_indices()
 DEFINE   ls_ind                      SMALLINT
 DEFINE   lc_Proceso                  CHAR(10)
 DEFINE   ls_Etapa                    SMALLINT
 DEFINE   ls_resultado                SMALLINT
 DEFINE   ld_FechaInicio              DATE
 DEFINE   ld_HoraInicio               CHAR(08)
 DEFINE   ld_HoraTermino              CHAR(08)
 DEFINE   lc_Comenta1                 CHAR(50)
 DEFINE   li_Consecutivo              INTEGER

 LET    lc_Proceso          =  'CTAB041'
 LET    ls_Etapa            =  1
 LET    ls_resultado        =  0
 CALL   f_VerProc(lc_Proceso, ls_Etapa)     RETURNING  ls_resultado,
                                                       ld_FechaInicio,
                                                       ld_HoraInicio,
                                                       ld_HoraTermino,
                                                       lc_Comenta1,
                                                       li_Consecutivo
 IF ld_HoraTermino IS NULL OR ld_HoraTermino = "" THEN  
        ### En ejecucion paso 1
 ELSE
        LET     ls_ind   =   0
        SELECT  COUNT(*)
          INTO  ls_ind
          FROM  sysindices
         WHERE  idxname = 'ix_cta_transf_edad1'

        IF     ls_ind   >  0   THEN
              SQL LOCK TABLE cta_transf_edad IN EXCLUSIVE MODE ;
              END SQL
              SQL DROP INDEX ix_cta_transf_edad1;
              END SQL
              SQL UNLOCK TABLE cta_transf_edad;
              END SQL
        END IF

        LET     ls_ind   =   0
        SELECT  COUNT(*)
          INTO  ls_ind
          FROM  sysindices
         WHERE  idxname = 'ix_cta_transf_edad2'

        IF     ls_ind   >  0   THEN
              SQL LOCK TABLE cta_transf_edad IN EXCLUSIVE MODE ;
              END SQL
              SQL DROP INDEX ix_cta_transf_edad2
              END SQL
              SQL UNLOCK TABLE cta_transf_edad;
              END SQL
        END IF

        LET     ls_ind   =   0
        SELECT  COUNT(*)
          INTO  ls_ind
          FROM  sysindices
         WHERE  idxname = 'ix_cta_transf_ed_gpo_1'

        IF     ls_ind   >  0   THEN
              SQL LOCK TABLE cta_transf_ed_gpo IN EXCLUSIVE MODE ;
              END SQL
              SQL DROP INDEX ix_cta_transf_ed_gpo_1
              END SQL
              SQL UNLOCK TABLE cta_transf_ed_gpo;
              END SQL
        END IF

        LET     ls_ind   =   0
        SELECT  COUNT(*)
          INTO  ls_ind
          FROM  sysindices
         WHERE  idxname = 'ix_cta_transf_ed_gpo_2'

        IF     ls_ind   >  0   THEN
              SQL LOCK TABLE cta_transf_ed_gpo IN EXCLUSIVE MODE ;
              END SQL
              SQL DROP INDEX ix_cta_transf_ed_gpo_2
              END SQL
              SQL UNLOCK TABLE cta_transf_ed_gpo;
              END SQL
        END IF
 END IF

END FUNCTION


################################################################################
FUNCTION f_Cifras()

 DEFINE   x_tipo_proceso              SMALLINT,
          opc                         CHAR(1)
 DEFINE   lc_elproc                   CHAR(200)
 DEFINE   GUSE                        CHAR(10)
 DEFINE   ls_Continua                 SMALLINT
 DEFINE   lc_Proceso                  CHAR(10)
 DEFINE   ls_Etapa                    SMALLINT
 DEFINE   ls_resultado                SMALLINT
 DEFINE   ld_FechaInicio              DATE
 DEFINE   ld_HoraInicio               CHAR(08)
 DEFINE   ld_HoraTermino              CHAR(08)
 DEFINE   lc_Comenta1                 CHAR(50)
 DEFINE   li_Consecutivo              INTEGER

 LET   ls_Continua        =    0
 --- Verificar que no exista en ejcucuión algun paso 1 

 LET    lc_Proceso              =  'CTAB041'
 LET    ls_Etapa                =  1
 FOR    ls_Etapa                =  1 TO 1
     LET    ls_Etapa            =  1
     LET    ls_resultado        =  0
     CALL   f_VerProc(lc_Proceso, ls_Etapa)     RETURNING  ls_resultado,
                                                           ld_FechaInicio,
                                                           ld_HoraInicio,
                                                           ld_HoraTermino,
                                                           lc_Comenta1,
                                                           li_Consecutivo

     IF ld_HoraTermino IS NULL OR ld_HoraTermino = "" THEN  
            ### En ejecucion paso 1
            ERROR  'No se puede EJECUTAR, EXISTE un proceso que aún no termina ', lc_Proceso CLIPPED, '-', ls_Etapa,
                   'REVISE .. '
            SLEEP  10
            ERROR  ' '
            LET    ls_Continua   = 1
     END IF
     LET    ls_Etapa             =  ls_Etapa + 1
 END FOR

 IF    ls_Continua        =    0     THEN
      LET INT_FLAG = FALSE
      OPEN WINDOW win5 AT 5,2 WITH FORM "CTAB0412"
      DISPLAY "[Ctrl-C] Salir " AT 1,1 ATTRIBUTE(REVERSE)

      INPUT BY NAME reg.fecha_corte

        BEFORE FIELD fecha_corte
            ---   busque y sugiera ultima fecha procesada ...
            SELECT  MAX(fecha_corte)
              INTO  reg.fecha_corte
              FROM  cta_transf_edad

            DISPLAY reg.fecha_corte TO fecha_corte

         AFTER FIELD  fecha_corte
            IF reg.fecha_corte IS NULL THEN
               ERROR  "Fecha de Corte no puede ser vacia .."
               NEXT   FIELD fecha_corte
            END IF

            LET     hay_reg          =   0
            IF      x_tipo_proceso   =   1  THEN
                 SELECT  COUNT(*)
                   INTO  hay_reg
                   FROM  cta_transf_edad
                  WHERE  fecha_corte = reg.fecha_corte
                    AND  cod_rechazo = 2
            END IF

--            IF hay_reg       =   0  THEN
--               ERROR "No hay Información para la Fecha de Corte que solicita"
--               NEXT FIELD fecha_corte
--            END IF

            EXIT INPUT

         ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT INPUT

      END INPUT

      IF INT_FLAG THEN
           LET INT_FLAG = FALSE
           CLEAR FORM
           CLEAR SCREEN
           CLOSE WINDOW win5
      ELSE
           CALL f_CifrasMenu()
           CLEAR FORM
           CLEAR SCREEN
           CLOSE WINDOW win5
      END IF
 END IF
 RETURN
END FUNCTION


################################################################################
FUNCTION f_CifrasMenu()

   MENU "CIFRAS"
      COMMAND "Tipo" "Cuentas Administradas por Tipo de Cuenta"
         CALL  f_CifrasTipo() 
      COMMAND "Edad" "Cuentas Administradas por Edad"
         CALL  f_CifrasEdad() 
      COMMAND "Régimen" "Transferencias por Siefore"
         CALL  f_CifrasSief() 
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU
END FUNCTION


################################################################################
FUNCTION f_CifrasTipo()
  DEFINE    li_x                       SMALLINT 
  DEFINE    li_y                       SMALLINT 
  DEFINE    lr_todos                   RECORD   
            ls_tipo_sol                SMALLINT,
            li_cuantos                 INTEGER
            END RECORD 
 
  FOR li_x  =  1 TO 10
      LET     gr_CifEdad[li_x].gr_ley                      = ''         
      LET     gr_CifEdad[li_x].gr_Total                    = 0
      LET     gr_CifEdad[li_x].gr_NoCamR                   = 0
      LET     gr_CifEdad[li_x].gr_CamReg                   = 0
      LET     gr_CifEdad[li_x].gr_Proced                   = 0
      LET     gr_CifEdad[li_x].gr_NoProc                   = 0
      LET     gr_CifEdad[li_x].gr_Desmar                   = 0
  END FOR
 
  --- Todos en cuota de mercado
  LET     lc_query        =  ' SELECT  tipo_solicitud, COUNT(*)       ',
                             '   FROM  safre_tmp:tmp_cuota_multisie   ',
                             '  GROUP  BY 1                           '
  PREPARE p_SelTipoTod                   FROM  lc_query 
  DECLARE d_SelTipoTod             CURSOR FOR  p_SelTipoTod

  --- No cambian
  LET     lc_query        =  ' SELECT  a.tipo_solicitud, COUNT(*)                                 ',
                             '   FROM  safre_tmp:tmp_cuota_multisie a                             ',
                             '  WHERE  a.n_seguro   NOT IN ( SELECT  nss                          ',
                             '                                 FROM  safre_af:cta_transf_edad     ',
                             '                                WHERE  fecha_corte = ? )            ',
                             '  GROUP  BY 1                                                       ' 
  PREPARE p_SelTipoNoC                   FROM  lc_query 
  DECLARE d_SelTipoNoC             CURSOR FOR  p_SelTipoNoC

  --- Cambian
  LET     lc_query        =  ' SELECT  a.tipo_solicitud, COUNT(*)                                 ',
                             '   FROM  safre_tmp:tmp_cuota_multisie a                             ',
                             '  WHERE  a.n_seguro       IN ( SELECT  nss                          ',
                             '                                 FROM  safre_af:cta_transf_edad     ',
                             '                                WHERE  fecha_corte = ? )            ',
                             '  GROUP  BY 1                                                       '
  PREPARE p_SelTipoCam                   FROM  lc_query 
  DECLARE d_SelTipoCam             CURSOR FOR  p_SelTipoCam

  --- Proceden
  LET     lc_query        =  ' SELECT  tipo_solicitud, COUNT(*)   ',
                             '   FROM  safre_af:cta_transf_edad   ',
                             '  WHERE  fecha_corte     =   ?      ',
                             '    AND  cod_rechazo     IN  (0,2)  ',
                             '  GROUP  BY 1                       '
  PREPARE p_SelTipoPro                   FROM  lc_query 
  DECLARE d_SelTipoPro             CURSOR FOR  p_SelTipoPro

  --- No Proceden 
  LET     lc_query        =  ' SELECT  tipo_solicitud, COUNT(*)   ',
                             '   FROM  safre_af:cta_transf_edad   ',
                             '  WHERE  fecha_corte     =   ?      ',
                             '    AND  cod_rechazo     =   8      ',
                             '  GROUP  BY 1                       ' 
  PREPARE p_SelTipoNop                   FROM  lc_query 
  DECLARE d_SelTipoNop             CURSOR FOR  p_SelTipoNop


  --- Desmarcados
  LET     lc_query        =  ' SELECT  tipo_solicitud, COUNT(*)   ',
                             '   FROM  safre_af:cta_transf_edad   ',
                             '  WHERE  fecha_corte     =   ?      ',
                             '    AND  cod_rechazo     =   10     ',
                             '  GROUP  BY 1                       '
  PREPARE p_SelTipoDes                   FROM  lc_query 
  DECLARE d_SelTipoDes             CURSOR FOR  p_SelTipoDes

  ERROR " Procesando Información ... " 

  LET     gr_CifEdad[1].gr_ley    =  'AFILIADOS     .. '
  LET     gr_CifEdad[2].gr_ley    =  'ASIGNADOS     .. '
  LET     gr_CifEdad[3].gr_ley    =  'NO AFILIADOS  .. '
  LET     gr_CifEdad[4].gr_ley    =  'TOTAL         .. '

  FOREACH d_SelTipoTod                        INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(1,lr_todos.ls_tipo_sol)
       LET  gr_CifEdad[li_x].gr_Total            =  gr_CifEdad[li_x].gr_Total + lr_todos.li_cuantos
       LET  gr_CifEdad[4].gr_Total               =  gr_CifEdad[4].gr_Total    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelTipoNoc                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(1,lr_todos.ls_tipo_sol)
       LET  gr_CifEdad[li_x].gr_NoCamR           =  gr_CifEdad[li_x].gr_NoCamR + lr_todos.li_cuantos
       LET  gr_CifEdad[4].gr_NoCamR              =  gr_CifEdad[4].gr_NoCamR    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelTipoCam                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(1,lr_todos.ls_tipo_sol)
       LET  gr_CifEdad[li_x].gr_CamReg           =  gr_CifEdad[li_x].gr_CamReg + lr_todos.li_cuantos
       LET  gr_CifEdad[4].gr_CamReg              =  gr_CifEdad[4].gr_CamReg    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelTipoPro                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(1,lr_todos.ls_tipo_sol)
       LET  gr_CifEdad[li_x].gr_Proced           =  gr_CifEdad[li_x].gr_Proced + lr_todos.li_cuantos
       LET  gr_CifEdad[4].gr_Proced              =  gr_CifEdad[4].gr_Proced    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelTipoNoP                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(1,lr_todos.ls_tipo_sol)
       LET  gr_CifEdad[li_x].gr_NoProc           =  gr_CifEdad[li_x].gr_NoProc + lr_todos.li_cuantos
       LET  gr_CifEdad[4].gr_NoProc              =  gr_CifEdad[4].gr_NoProc    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelTipoDes                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(1,lr_todos.ls_tipo_sol)
       LET  gr_CifEdad[li_x].gr_Desmar           =  gr_CifEdad[li_x].gr_Desmar + lr_todos.li_cuantos
       LET  gr_CifEdad[4].gr_Desmar              =  gr_CifEdad[4].gr_Desmar    + lr_todos.li_cuantos
  END FOREACH
 
  OPEN WINDOW win7                                                  AT 7,3 WITH FORM "CTAB0415"  ATTRIBUTE(BORDER)
  DISPLAY "[Ctrl-I] Reporte "                                       AT 1,1                       ATTRIBUTE(REVERSE)
  DISPLAY "[Ctrl-C] Salir "                                         AT 2,1                       ATTRIBUTE(REVERSE)
  DISPLAY "            SIN     CAMBIO             NO           "    AT 1,27                      ATTRIBUTE(REVERSE)
  DISPLAY "   TOTAL   CAMBIO  REGIMEN PROCEDEN PROCEDEN DESMAR "    AT 2,27                      ATTRIBUTE(REVERSE)

  CALL    SET_COUNT(4)
  DISPLAY ARRAY                      gr_CifEdad TO scr_1.*
       ON KEY ( INTERRUPT )
          EXIT DISPLAY
       ON KEY (CONTROL-I) 
          CALL f_CifrasTipoRep(1) 
          PROMPT    "Reporte Generado, Presione ENTER para Continuar"  ATTRIBUTE(REVERSE)
                     FOR gc_opc ATTRIBUTE (REVERSE)
  END DISPLAY

  CLOSE WINDOW win7  
END FUNCTION

 
################################################################################
FUNCTION f_VerInd(ls_cve, ls_tipo_sol)
  DEFINE     ls_cve                               SMALLINT
  DEFINE     ls_tipo_sol                          SMALLINT
  DEFINE     li_x                                 SMALLINT

  IF    ls_cve    =     1   THEN  
        LET   li_x                       =  0
        IF    ls_tipo_sol                =  1
         OR   ls_tipo_sol                =  2
         OR   ls_tipo_sol                =  3
         OR   ls_tipo_sol                =  4
         OR   ls_tipo_sol                =  6
         OR   ls_tipo_sol                =  7
         OR   ls_tipo_sol                =  9
         OR   ls_tipo_sol                =  10
         OR   ls_tipo_sol                =  11  THEN
              LET  li_x                  =  1
        ELSE
              IF   ls_tipo_sol           =  5   THEN
                   LET  li_x             =  2
              ELSE
                   LET  li_x             =  3
              END IF
        END IF
  ELSE
        IF    ls_tipo_sol    IS NULL       
         OR   ls_tipo_sol    =  0     THEN
              LET  li_x                  =  7
        ELSE
              LET  li_x                  =  ls_tipo_sol
        END IF
  END IF

  RETURN li_x
END FUNCTION 


################################################################################
FUNCTION f_CifrasTipoRep(ls_tipo) 
  DEFINE       lc_NomRep             CHAR(280)
  DEFINE       lc_titulo             CHAR(120)
  DEFINE       lc_linbla             CHAR(120)
  DEFINE       lc_linea              CHAR(120)
  DEFINE       li_x                  SMALLINT 
  DEFINE       ls_tipo               SMALLINT 

  IF        ls_tipo                  =  1  THEN 
        --  LET       lc_NomRep                =  '/safre_prc/cta/envio/'||'EdadTipo-'||
        --                                        DAY(TODAY)||MONTH(TODAY)||YEAR(TODAY)||'.rep'
        LET       lc_NomRep                =  'EdadTipo-'||
                                              DAY(TODAY)||MONTH(TODAY)||YEAR(TODAY)||'.rep'
  ELSE
        IF        ls_tipo            =  2  THEN 
            --  LET       lc_NomRep                =  '/safre_prc/cta/envio/'||'EdadRegi-'||
            --                                        DAY(TODAY)||MONTH(TODAY)||YEAR(TODAY)||'.rep'
            LET       lc_NomRep                =  'EdadRegi-'||
                                                  DAY(TODAY)||MONTH(TODAY)||YEAR(TODAY)||'.rep'
        ELSE
            --  LET       lc_NomRep                =  '/safre_prc/cta/envio/'||'EdadSief-'||
            --                                        DAY(TODAY)||MONTH(TODAY)||YEAR(TODAY)||'.rep'
            LET       lc_NomRep                =  'EdadSief-'||
                                                  DAY(TODAY)||MONTH(TODAY)||YEAR(TODAY)||'.rep'
        END IF
  END IF
  LET       lc_linbla                =  '  '
  LET       lc_NomRep                =  lc_NomRep CLIPPED
  START     REPORT f_Report         TO  lc_NomRep
  IF        ls_tipo         =   1  
   OR       ls_tipo         =   2   THEN 
       LET       lc_titulo                =  'CONCEPTO,AFILIADOS TOTAL, SIN CAMBIO, CAMBIO REGIMEN, '||
                                             'CUENTAS PROCEDEN, CUENTAS NO PROCEDEN, CUENTAS DESMARCADAS '
       OUTPUT TO REPORT f_Report(lc_titulo)
       OUTPUT TO REPORT f_Report(lc_linbla)
       LET       li_x                     =  1
       FOR li_x  =  1 TO 10
           IF    gr_CifEdad[li_x].gr_ley   <>  ' '   THEN 
                 LET   lc_linea =  gr_CifEdad[li_x].gr_ley||','||gr_CifEdad[li_x].gr_Total||','||
                                   gr_CifEdad[li_x].gr_NoCamR||','||gr_CifEdad[li_x].gr_CamReg||','||
                                   gr_CifEdad[li_x].gr_Proced||','||gr_CifEdad[li_x].gr_NoProc||','||
                                   gr_CifEdad[li_x].gr_Desmar
                 OUTPUT TO REPORT  f_Report(lc_linea)  
           END IF
       END FOR 
  ELSE
       LET       lc_titulo                =  'ORIGEN-DESTINO, SIE-1, SIE-2, SIE-3, SIE-4, SIE-5 '          
       OUTPUT TO REPORT f_Report(lc_titulo)
       OUTPUT TO REPORT f_Report(lc_linbla)
       LET       li_x                     =  1
       FOR li_x  =  1 TO 10
           IF    gr_todos1[li_x].gr_ley   <>  ' '   THEN 
                 LET   lc_linea =  gr_todos1[li_x].gr_ley||','||gr_todos1[li_x].gr_Cue01||','||
                                   gr_todos1[li_x].gr_Cue02||','||gr_todos1[li_x].gr_Cue03||','||
                                   gr_todos1[li_x].gr_Cue04||','||gr_todos1[li_x].gr_Cue05
                 OUTPUT TO REPORT  f_Report(lc_linea)  

                 LET   lc_linea =  '     ACCIONES       '||','||gr_todos1[li_x].gr_Acc01||','||
                                   gr_todos1[li_x].gr_Acc02||','||gr_todos1[li_x].gr_Acc03||','||
                                   gr_todos1[li_x].gr_Acc04||','||gr_todos1[li_x].gr_Acc05
                 OUTPUT TO REPORT  f_Report(lc_linea)  

                 LET   lc_linea =  '     PESOS          '||','||gr_todos1[li_x].gr_Pes01||','||
                                   gr_todos1[li_x].gr_Pes02||','||gr_todos1[li_x].gr_Pes03||','||
                                   gr_todos1[li_x].gr_Pes04||','||gr_todos1[li_x].gr_Pes05
                 OUTPUT TO REPORT  f_Report(lc_linea)  
           END IF
       END FOR 
  END IF
  FINISH REPORT f_Report
     
END FUNCTION


### Función general imprime una linea en el reporte que sea
REPORT f_Report(lc_DetReg)

  DEFINE       lc_DetReg             CHAR(120)     --- Registro a tratar

  OUTPUT
     PAGE      LENGTH    1
     LEFT      MARGIN    0
     RIGHT     MARGIN    0
     TOP       MARGIN    0
     BOTTOM    MARGIN    0

  FORMAT
      ON EVERY ROW
         PRINT COLUMN 001, lc_DetReg 

END REPORT


################################################################################
FUNCTION f_CifrasEdad()
  DEFINE    li_x                       SMALLINT
  DEFINE    li_y                       SMALLINT
  DEFINE    lr_todos                   RECORD
            ls_ind_edad                SMALLINT,
            li_cuantos                 INTEGER
            END RECORD

  FOR li_x  =  1 TO 10
      LET     gr_CifEdad[li_x].gr_ley                      = ''         
      LET     gr_CifEdad[li_x].gr_Total                    = 0
      LET     gr_CifEdad[li_x].gr_NoCamR                   = 0
      LET     gr_CifEdad[li_x].gr_CamReg                   = 0
      LET     gr_CifEdad[li_x].gr_Proced                   = 0
      LET     gr_CifEdad[li_x].gr_NoProc                   = 0
      LET     gr_CifEdad[li_x].gr_Desmar                   = 0
  END FOR
 
  --- Todos en cuota de mercado
  LET     lc_query        =  ' SELECT  b.ind_edad, COUNT(*)                  ',
                             '   FROM  safre_tmp:tmp_cuota_multisie a,       ',
                             '  OUTER  safre_af:cta_ctr_cuenta b             ',
                             '  WHERE  a.n_seguro     = b.nss                ',
                             '  GROUP  BY 1                                  '
  PREPARE p_SelEdadTod                   FROM  lc_query
  DECLARE d_SelEdadTod             CURSOR FOR  p_SelEdadTod

  --- No cambian
  LET     lc_query        =  ' SELECT  b.ind_edad, COUNT(*)                                     ',
                             '   FROM  safre_tmp:tmp_cuota_multisie a,                          ',
                             '  OUTER  safre_af:cta_ctr_cuenta      b                           ',
                             '  WHERE  a.n_seguro     = b.nss                                   ',
                             '    AND  a.n_seguro   NOT IN ( SELECT  nss                        ',
                             '                                 FROM  safre_af:cta_transf_edad   ',
                             '                                WHERE  fecha_corte =   ? )        ',
                             '  GROUP  BY 1                                                     '
  PREPARE p_SelEdadNoC                   FROM  lc_query
  DECLARE d_SelEdadNoC             CURSOR FOR  p_SelEdadNoC

  --- Cambian
  LET     lc_query        =  ' SELECT  b.ind_edad, count(*)                                     ',
                             '   FROM  safre_tmp:tmp_cuota_multisie a,                          ',
                             '         safre_af:cta_ctr_cuenta      b                           ',
                             '  WHERE  a.n_seguro         = b.nss                               ',
                             '    AND  a.n_seguro       IN ( SELECT  nss                        ',
                             '                                 FROM  safre_af:cta_transf_edad   ',
                             '                                WHERE  fecha_corte =   ? )        ', 
                             '  GROUP  BY 1                                                     '
  PREPARE p_SelEdadCam                   FROM  lc_query
  DECLARE d_SelEdadCam             CURSOR FOR  p_SelEdadCam

  --- Proceden
  LET     lc_query        =  ' SELECT  b.ind_edad, COUNT(*)                 ',
                             '   FROM  safre_af:cta_transf_edad a,          ',
                             '         safre_af:cta_ctr_cuenta  b           ',
                             '  WHERE  a.nss              = b.nss           ',
                             '    AND  a.fecha_corte      =   ?             ',
                             '    AND  a.cod_rechazo      IN  (0,2)         ',
                             '  GROUP  BY 1                                 '
  PREPARE p_SelEdadPro                   FROM  lc_query
  DECLARE d_SelEdadPro             CURSOR FOR  p_SelEdadPro

  --- No Proceden
  LET     lc_query        =  ' SELECT  b.ind_edad, count(*)                 ',
                             '   FROM  safre_af:cta_transf_edad a,          ',
                             '         safre_af:cta_ctr_cuenta  b           ',
                             '  WHERE  a.nss              = b.nss           ',
                             '    AND  a.fecha_corte      =   ?             ',
                             '    AND  a.cod_rechazo      =   8             ',
                             '  GROUP  BY 1                                 '
  PREPARE p_SelEdadNoP                   FROM  lc_query
  DECLARE d_SelEdadNoP             CURSOR FOR  p_SelEdadNoP

  --- Desmarcadas
  LET     lc_query        =  ' SELECT  b.ind_edad, count(*)                 ',
                             '   FROM  safre_af:cta_transf_edad a,          ',
                             '         safre_af:cta_ctr_cuenta  b           ',
                             '  WHERE  a.nss              = b.nss           ',
                             '    AND  a.fecha_corte      =   ?             ',
                             '    AND  a.cod_rechazo      =   10            ',
                             '  GROUP  BY 1                                 '
  PREPARE p_SelEdadDes                  FROM  lc_query
  DECLARE d_SelEdadDes            CURSOR FOR  p_SelEdadDes

  ERROR " Procesando Información ... " 
  LET     gr_CifEdad[1].gr_ley    =  'MAYORES 55 AÑOS   .. '
  LET     gr_CifEdad[2].gr_ley    =  '46 AÑOS A 55 AÑOS .. '
  LET     gr_CifEdad[3].gr_ley    =  '37 AÑOS A 45 AÑOS .. '
  LET     gr_CifEdad[4].gr_ley    =  '27 AÑOS A 36 AÑOS .. ' 
  LET     gr_CifEdad[5].gr_ley    =  'MENORES A 26 AÑOS .. ' 
  LET     gr_CifEdad[6].gr_ley    =  'TOTALES           .. ' 
  LET     gr_CifEdad[7].gr_ley    =  'SIN IDENTIFICA    .. ' 

  FOREACH d_SelEdadTod                        INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(2,lr_todos.ls_ind_edad)
       LET  gr_CifEdad[li_x].gr_Total            =  gr_CifEdad[li_x].gr_Total + lr_todos.li_cuantos
       LET  gr_CifEdad[6].gr_Total               =  gr_CifEdad[6].gr_Total    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelEdadNoc                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(2,lr_todos.ls_ind_edad)
       LET  gr_CifEdad[li_x].gr_NoCamR           =  gr_CifEdad[li_x].gr_NoCamR + lr_todos.li_cuantos
       LET  gr_CifEdad[6].gr_NoCamR              =  gr_CifEdad[6].gr_NoCamR    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelEdadCam                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(2,lr_todos.ls_ind_edad)
       LET  gr_CifEdad[li_x].gr_CamReg           =  gr_CifEdad[li_x].gr_CamReg + lr_todos.li_cuantos
       LET  gr_CifEdad[6].gr_CamReg              =  gr_CifEdad[6].gr_CamReg    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelEdadPro                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(2,lr_todos.ls_ind_edad)
       LET  gr_CifEdad[li_x].gr_Proced           =  gr_CifEdad[li_x].gr_Proced + lr_todos.li_cuantos
       LET  gr_CifEdad[6].gr_Proced              =  gr_CifEdad[6].gr_Proced    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelEdadNoP                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(2,lr_todos.ls_ind_edad)
       LET  gr_CifEdad[li_x].gr_NoProc           =  gr_CifEdad[li_x].gr_NoProc + lr_todos.li_cuantos
       LET  gr_CifEdad[6].gr_NoProc              =  gr_CifEdad[6].gr_NoProc    + lr_todos.li_cuantos
  END FOREACH

  FOREACH d_SelEdadDes                       USING  reg.fecha_corte
                                              INTO  lr_todos.*
       LET  li_x                                 =  f_VerInd(2,lr_todos.ls_ind_edad)
       LET  gr_CifEdad[li_x].gr_Desmar           =  gr_CifEdad[li_x].gr_Desmar + lr_todos.li_cuantos
       LET  gr_CifEdad[6].gr_Desmar              =  gr_CifEdad[6].gr_Desmar    + lr_todos.li_cuantos
  END FOREACH

  OPEN WINDOW win7                                                  AT 7,3 WITH FORM "CTAB0415"  ATTRIBUTE(BORDER)
  DISPLAY "[Ctrl-I] Reporte "                                       AT 1,1                       ATTRIBUTE(REVERSE)
  DISPLAY "[Ctrl-C] Salir "                                         AT 2,1                       ATTRIBUTE(REVERSE)
  DISPLAY "            SIN     CAMBIO             NO           "    AT 1,27                      ATTRIBUTE(REVERSE)
  DISPLAY "   TOTAL   CAMBIO  REGIMEN PROCEDEN PROCEDEN DESMAR "    AT 2,27                      ATTRIBUTE(REVERSE)

  CALL    SET_COUNT(7)
  DISPLAY ARRAY                      gr_CifEdad TO scr_1.*
       ON KEY ( INTERRUPT )
          EXIT DISPLAY
       ON KEY (CONTROL-I)
          CALL f_CifrasTipoRep(2)
          PROMPT    "Reporte Generado, Presione ENTER para Continuar"  ATTRIBUTE(REVERSE)
                     FOR gc_opc ATTRIBUTE (REVERSE)
  END DISPLAY

  CLOSE WINDOW win7
END FUNCTION


################################################################################
FUNCTION f_CifrasSief()
  DEFINE    li_x                       SMALLINT 
  DEFINE    li_y                       SMALLINT 
  DEFINE    lr_todos                   RECORD   
            sie_ori                    SMALLINT,
            sie_new                    SMALLINT,
            afilia                     INTEGER, 
            accion                     DECIMAL(22,6),
            pesos                      DECIMAL(22,6) 
            END RECORD 
 
  FOR li_x  =  1 TO 10
      FOR   li_y   =  1  TO  10
            LET     gr_CifSie[li_x, li_y].afilia                       = 0          
            LET     gr_CifSie[li_x, li_y].accion                       = 0
            LET     gr_CifSie[li_x, li_y].pesos                        = 0
      END FOR
      LET    gr_todos1[li_x].gr_Cue01            =  0
      LET    gr_todos1[li_x].gr_Cue02            =  0
      LET    gr_todos1[li_x].gr_Cue03            =  0
      LET    gr_todos1[li_x].gr_Cue04            =  0
      LET    gr_todos1[li_x].gr_Cue05            =  0
      LET    gr_todos1[li_x].gr_Acc01            =  0
      LET    gr_todos1[li_x].gr_Acc02            =  0
      LET    gr_todos1[li_x].gr_Acc03            =  0
      LET    gr_todos1[li_x].gr_Acc04            =  0
      LET    gr_todos1[li_x].gr_Acc05            =  0
      LET    gr_todos1[li_x].gr_Pes01            =  0
      LET    gr_todos1[li_x].gr_Pes02            =  0
      LET    gr_todos1[li_x].gr_Pes03            =  0
      LET    gr_todos1[li_x].gr_Pes04            =  0
      LET    gr_todos1[li_x].gr_Pes05            =  0
  END FOR
 
  CALL      f_VerSaldos() 
  --- Acumulado por siefore de ed_gpo

  LET     lc_query        =  ' SELECT  a.siefore_ant, a.siefore,             ',
                             '         COUNT(*),                             ',
                             '         SUM(c.monto_en_acciones),             ',
                             '         SUM(c.monto_en_pesos)                 ',
                             '   FROM  cta_transf_ed_gpo         a,          ',
                             '         cta_transf_edad           b,          ',
                             '         safre_tmp:tmp_saldo_edad  c,          ',
                             '         tab_agrupa_subcta_regimen d           ',
                             '  WHERE  a.nss             =  b.nss            ',
                             '    AND  a.fecha_corte     =  ?                ',
                             '    AND  a.estado          =  0                ',
                             '    AND  b.cod_rechazo    IN  (0,2)            ',
                             '    AND  a.fecha_corte     =  b.fecha_corte    ',
                             '    AND  a.nss             =  c.nss            ',
                             '    AND  a.siefore_ant     =  c.siefore        ',
                             '    AND  c.subcuenta       =  d.subcuenta      ',
                             '    AND  d.grupo_regimen   =  a.grupo_regimen  ',
                             '  GROUP  BY 1, 2                               '
  PREPARE p_SelTipoSie                   FROM  lc_query 
  DECLARE d_SelTipoSie             CURSOR FOR  p_SelTipoSie

  ERROR " Procesando Información ... " 
  LET     gr_todos1[1].gr_ley    =  'SIEFORE-1  .. '
  LET     gr_todos1[2].gr_ley    =  'SIEFORE-2  .. '
  LET     gr_todos1[3].gr_ley    =  'SIEFORE-3  .. '
  LET     gr_todos1[4].gr_ley    =  'SIEFORE-4  .. ' 
  LET     gr_todos1[5].gr_ley    =  'SIEFORE-5  .. ' 
  LET     gr_todos1[6].gr_ley    =  'TOTAL      .. ' 

  FOREACH   d_SelTipoSie                      USING  reg.fecha_corte
                                               INTO  lr_todos.*

       LET  li_x                                  =  lr_todos.sie_ori
       LET  li_y                                  =  lr_todos.sie_new
       LET  gr_CifSie[li_x, li_y].afilia          =  gr_CifSie[li_x, li_y].afilia  +  lr_todos.afilia
       LET  gr_CifSie[li_x, li_y].accion          =  gr_CifSie[li_x, li_y].accion  +  lr_todos.accion
       LET  gr_CifSie[li_x, li_y].pesos           =  gr_CifSie[li_x, li_y].pesos   +  lr_todos.pesos 
       LET  gr_CifSie[6,    li_y].afilia          =  gr_CifSie[6,    li_y].afilia  +  lr_todos.afilia
       LET  gr_CifSie[6,    li_y].accion          =  gr_CifSie[6,    li_y].accion  +  lr_todos.accion
       LET  gr_CifSie[6,    li_y].pesos           =  gr_CifSie[6,    li_y].pesos   +  lr_todos.pesos 
       LET  gr_CifSie[li_x, 6].afilia             =  gr_CifSie[li_x, 6].afilia     +  lr_todos.afilia
       LET  gr_CifSie[li_x, 6].accion             =  gr_CifSie[li_x, 6].accion     +  lr_todos.accion
       LET  gr_CifSie[li_x, 6].pesos              =  gr_CifSie[li_x, 6].pesos      +  lr_todos.pesos  
       LET  gr_CifSie[6,    6].afilia             =  gr_CifSie[6,    6].afilia     +  lr_todos.afilia
       LET  gr_CifSie[6,    6].accion             =  gr_CifSie[6,    6].accion     +  lr_todos.accion
       LET  gr_CifSie[6,    6].pesos              =  gr_CifSie[6,    6].pesos      +  lr_todos.pesos  
  END FOREACH

  OPEN WINDOW win8                                                  AT 5,2 WITH FORM "CTAB0416"  ATTRIBUTE(BORDER)
  DISPLAY "[Ctrl-I] Reporte "                                       AT 1,1                       ATTRIBUTE(REVERSE)
  DISPLAY "[Ctrl-C] Salir "                                         AT 2,1                       ATTRIBUTE(REVERSE)
  DISPLAY "SIE-1      SIE-2       SIE-3      SIE-4       SIE-5   "  AT 1,24                      ATTRIBUTE(REVERSE)
  DISPLAY "           CASOS / ACCIONES / PESOS                   "  AT 2,24                      ATTRIBUTE(REVERSE)

  FOR     li_x  =  1 TO 10
       LET    gr_todos1[li_x].gr_Cue01            =  gr_CifSie[li_x, 1].afilia
       LET    gr_todos1[li_x].gr_Cue02            =  gr_CifSie[li_x, 2].afilia
       LET    gr_todos1[li_x].gr_Cue03            =  gr_CifSie[li_x, 3].afilia
       LET    gr_todos1[li_x].gr_Cue04            =  gr_CifSie[li_x, 4].afilia
       LET    gr_todos1[li_x].gr_Cue05            =  gr_CifSie[li_x, 5].afilia
       LET    gr_todos1[li_x].gr_Acc01            =  gr_CifSie[li_x, 1].accion
       LET    gr_todos1[li_x].gr_Acc02            =  gr_CifSie[li_x, 2].accion
       LET    gr_todos1[li_x].gr_Acc03            =  gr_CifSie[li_x, 3].accion
       LET    gr_todos1[li_x].gr_Acc04            =  gr_CifSie[li_x, 4].accion
       LET    gr_todos1[li_x].gr_Acc05            =  gr_CifSie[li_x, 5].accion
       LET    gr_todos1[li_x].gr_Pes01            =  gr_CifSie[li_x, 1].pesos 
       LET    gr_todos1[li_x].gr_Pes02            =  gr_CifSie[li_x, 2].pesos 
       LET    gr_todos1[li_x].gr_Pes03            =  gr_CifSie[li_x, 3].pesos 
       LET    gr_todos1[li_x].gr_Pes04            =  gr_CifSie[li_x, 4].pesos 
       LET    gr_todos1[li_x].gr_Pes05            =  gr_CifSie[li_x, 5].pesos 
  END FOR
 
  ERROR " " 
  CALL    SET_COUNT(6)
  DISPLAY ARRAY                      gr_todos1    TO scr_1.*
       ON KEY ( INTERRUPT )
          EXIT DISPLAY
       ON KEY (CONTROL-I)
          CALL f_CifrasTipoRep(3)
          PROMPT    "Reporte Generado, Presione ENTER para Continuar"  ATTRIBUTE(REVERSE)
                     FOR gc_opc ATTRIBUTE (REVERSE)
  END DISPLAY

  CLOSE WINDOW win8

END FUNCTION
 
### Si no existen saldos los crea 
FUNCTION f_VerSaldos()
 DEFINE     lc_query            CHAR(500)
 DEFINE     ls_existe           SMALLINT

 DATABASE   safre_tmp 

 LET        ls_existe   =  0
 LET        lc_query    =  " SELECT  COUNT(*)                      ",
                           "   FROM  systables                     ",
                           "  WHERE  tabname = 'tmp_saldo_edad'    " 
 PREPARE    p_CreSald          FROM  lc_query 

 LET        lc_query    =  " EXECUTE PROCEDURE safre_tmp:sp_saldo_edad(?)    "
 PREPARE    p_EjeSald          FROM  lc_query 

 EXECUTE    p_CreSald          INTO  ls_existe  
 IF         ls_existe            IS  NULL
  OR        ls_existe             =  0      THEN 
     LET       ls_existe =    0 
 ELSE 
     PROMPT  "Existen saldos, desea Actualizar [S/N] ... " ATTRIBUTE(REVERSE)
        FOR  gc_opc ATTRIBUTE(REVERSE)
     IF      gc_opc MATCHES '[Ss]' THEN
         LET       ls_existe =    2 
     ELSE
         LET       ls_existe =    1
     END IF
 END IF

 IF         ls_existe             =  0
  OR        ls_existe             =  2      THEN
    ERROR   ' Creando tabla de saldos en Temporal .. '

    ---WHENEVER ERROR CONTINUE 
    
    IF     ls_existe              =  2      THEN
         DROP   TABLE tmp_saldo_edad;
    END IF

    CREATE TABLE tmp_saldo_edad
      (
        nss                 CHAR(11),
        subcuenta           SMALLINT,
        siefore             SMALLINT,
        fecha_conversion    DATE,
        monto_en_acciones   DECIMAL(22,6),
        monto_en_pesos      DECIMAL(22,6)
      );

    ERROR  ' Ejecutando procedimiento de creación de saldos .. '

    EXECUTE  p_EjeSald        USING  reg.fecha_corte
    ---WHENEVER ERROR STOP 
    
 END IF 

 DATABASE safre_af 
END FUNCTION 

FUNCTION fn_obtiene_saldos()
#fos------------------------
   DEFINE   x_tipo_proceso              SMALLINT,
            opc                         CHAR(1),
            ls_hay_reg                  INTEGER
   DEFINE   lc_elproc                   CHAR(200)
   DEFINE   GUSE                        CHAR(10)

   LET INT_FLAG = FALSE

   OPEN WINDOW win_sdo AT 5,2 WITH FORM "CTAB0416"
   DISPLAY "[Esc] Ejecutar  [Ctrl-C] Salir " AT 1,1 ATTRIBUTE(REVERSE)

   LET reg.fecha_corte = TODAY

   INPUT BY NAME  reg.fecha_corte WITHOUT DEFAULTS

      AFTER FIELD fecha_corte
         IF reg.fecha_corte   IS NULL THEN
            ERROR "Fecha de saldos no puede quedar vacia ..."
            NEXT FIELD fecha_corte
         END IF
{svera

         EXECUTE  p_VerTrans    USING  reg.fecha_corte
                                 INTO   ls_hay_reg
         IF   ls_hay_reg     > 0 THEN
              ERROR "Existe información para la fecha que solicita ..."
              NEXT FIELD fecha_corte
         END IF
}
      ON KEY(ESC)
         IF reg.fecha_corte   IS NULL THEN
            ERROR "Fecha de saldos no puede quedar vacia ..."
            NEXT FIELD fecha_corte
         END IF
{svera
         EXECUTE  p_VerTrans    USING  reg.fecha_corte
                                 INTO   ls_hay_reg
         IF   ls_hay_reg     > 0 THEN
              ERROR "Existe información para la fecha que solicita ..."
              NEXT FIELD fecha_corte
         END IF
}
         EXIT INPUT

      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
         ERROR   "PROCESO CANCELADO .."
   ELSE 
         PROMPT  "Desea Ejecutar el Proceso [S/N] ... " ATTRIBUTE(REVERSE)
            FOR  opc ATTRIBUTE(REVERSE)
   
         IF opc MATCHES '[Ss]' THEN
               LET     lc_Proceso     =  'CTAB041'
               LET     lc_Comenta     =  'Obtiene saldos para Cambio de Regimen por Edad'
               LET     ld_FechaInicio =  reg.fecha_corte
               --LET     ls_Etapa       =  1
               LET     ls_Control     =  0
               LET     li_Consecutivo =  0

               CALL crea_tablas_saldo()

         --      CALL    f_quita_indices()
               SELECT  USER
                 INTO  GUSER
                 FROM  safre_af:seg_modulo
                WHERE  modulo_cod = "cta"

               LET     lc_elproc  =  "\"EXECUTE PROCEDURE safre_tmp:sp_saldo_edad( '", reg.fecha_corte, "') \" "
               LET     lc_elproc  =  "echo ", lc_elproc CLIPPED, " > ", GUSER CLIPPED, ".eje_saldo_edad.sql"
               RUN     lc_elproc

               LET     v_comando  =  "nohup time dbaccess safre_tmp ",
                                     GUSER CLIPPED, ".eje_saldo_edad.sql "
               LET     v_comando  =  v_comando CLIPPED

               CALL    f_GeneraProceso(lc_Proceso,
                                       lc_Comenta,
                                       ld_FechaInicio,
                                       ls_Etapa,
                                       v_comando,
                                       reg.fecha_corte,
                                       reg.fecha_edad)  RETURNING ls_Control,
                                                                  li_Consecutivo
               IF      li_Consecutivo    > 0  THEN
                    PROMPT    "Presione ENTER para Continuar"  ATTRIBUTE(REVERSE)
                               FOR opc ATTRIBUTE (REVERSE)
               ELSE
                    ERROR ' NO FUE POSIBLE registrar el proceso .. ', lc_Proceso CLIPPED,'-',ls_Etapa
                    SLEEP 5
                    ERROR '    '
               END IF
         ELSE
               ERROR  "PROCESO CANCELADO .."


         END IF
   END IF

   CLEAR FORM
   CLEAR SCREEN
   CLOSE WINDOW win_sdo
   RETURN
END FUNCTION

FUNCTION crea_tablas_saldo()
DATABASE safre_tmp
    WHENEVER ERROR CONTINUE

        DROP TABLE tmp_saldo_edad;
        DROP TABLE tmp_saldo_grupo;

    WHENEVER ERROR STOP

        CREATE TABLE tmp_saldo_edad
            (nss char(11),
             subcuenta smallint,
             siefore smallint,
             fecha_conversion date,
             monto_en_acciones decimal(22,6),
             monto_en_pesos decimal(22,6))

        IN tmp_dbs1;

        CREATE TABLE tmp_saldo_grupo
            (nss char(11),
             grupo_regimen smallint,
             siefore smallint,
             fecha_conversion date,
             monto_en_acciones decimal(22,6),
             monto_en_pesos decimal(22,6))

        IN tmp_dbs1;   
        
END FUNCTION

####################################################################################
#Menu para reporte previo o definitivo
FUNCTION f_m_gen_reporte()
   OPEN WINDOW frm_men_rep AT 2,2 WITH 22 ROWS, 73 COLUMNS ATTRIBUTE(BORDER)
   DISPLAY "CTAB041        REPORTE DE IDENTIFICACION POR EDAD         ",HOY USING"DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)

   MENU "REPORTES"
      COMMAND "Previo" "Genera Reporte de Identificacion Previa"
         CALL genera_reporte(1)

      COMMAND "Definitivo" "Genera Reporte de Identificacion Definitiva"
         CALL genera_reporte(2)

      COMMAND "Salir" "Regresa Al Menu Anterior"
         EXIT MENU
   END MENU
   CLOSE WINDOW frm_men_rep

END FUNCTION

####################################################################################
####################################################################################
FUNCTION genera_reporte(p_tipo)

DEFINE p_tipo,            #1-Previo 2-Definitivo
       ls_etapa         SMALLINT	
DEFINE
   enter             CHAR(1),
   lanza_proceso     CHAR(200),
   lc_elproc           CHAR(200),
   vsp_transf_edad     CHAR(100),
   vruta_rescate       CHAR(40)
      
DEFINE vmarca_cod          ,
      ws                  SMALLINT
  
DEFINE
   vfecha_ini        ,
   vfecha_corte      ,
   l_fecha                    DATE,
   l_parametro1, l_parametro2 CHAR(10)


LET ls_Etapa = 0
IF p_tipo = 1 THEN      # previo
   LET ls_Etapa       =  10
ELSE                    #definitivo
   LET ls_Etapa       =  1
END IF

   LET ws     = 0

   OPEN WINDOW win6 AT 2,2 WITH FORM "CTAB0417" ATTRIBUTE(BORDER)
   DISPLAY "CTAB041          REPORTE IDENTIFICACION POR EDAD               ",HOY USING"DD-MM-YYYY" AT 3,1 ATTRIBUTE(REVERSE)

     INITIALIZE l_fecha TO NULL
     SELECT MAX(fecha_proceso) INTO l_fecha
     FROM safre_af:dis_ctrl_proceso
     WHERE proceso_cod = "CTAB041"
     AND etapa_cod = ls_Etapa
     AND hora_final IS NOT NULL

    IF l_fecha IS NULL THEN
       PROMPT "NO EXISTE FECHA PARA EL REPORTE <ENTER> PARA SALIR" ATTRIBUTE(REVERSE)
       FOR CHAR enter
       CLOSE WINDOW win6
       RETURN
    END IF 

     LET l_parametro1 = "" LET l_parametro2 = ""

     SELECT max(fecha_corte), max(fecha_edad) 
     INTO l_parametro1,l_parametro2
     FROM safre_tmp:tmp_fechas_transferencia
     WHERE tipo = p_tipo

     LET vfecha_ini = MDY(l_parametro1[1,2],l_parametro1[4,5],l_parametro1[7,10])
     LET vfecha_corte = MDY(l_parametro2[1,2],l_parametro2[4,5],l_parametro2[7,10])

     DISPLAY vfecha_ini,vfecha_corte TO FORMONLY.fecha_corte, FORMONLY.fecha_edad 

   PROMPT " DESEA GENERAR EL ARCHIVO S/N ? " ATTRIBUTE(REVERSE)
   FOR enter


   IF enter MATCHES "[Ss]" THEN

      DISPLAY " GENERANDO ARCHIVO " AT 17,1 ATTRIBUTE(REVERSE)
  
    WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
         DROP TABLE tmp_saldo_edad;
         DROP TABLE tmp_saldo_grupo;
         DROP TABLE tmp_reporte_tes;
    WHENEVER ERROR STOP
  
      CALL crea_tablas_saldo()

      WHENEVER ERROR CONTINUE
         DATABASE safre_tmp
      WHENEVER ERROR STOP

      LET vsp_transf_edad = "EXECUTE PROCEDURE safre_tmp:sp_saldo_edad('",vfecha_corte, "')"

      PREPARE eje_sp_transf_edad FROM vsp_transf_edad
      EXECUTE eje_sp_transf_edad

      CREATE TABLE tmp_reporte_tes
      (nss               CHAR(11),
       fecha_calculo     DATE,
       tipo_solicitud    SMALLINT,
       siefore_ant       SMALLINT,
       siefore           SMALLINT,
       subcuenta         SMALLINT,
       monto_en_acciones DECIMAL(16,6),
       estado            SMALLINT,
       desc_estado       CHAR(150),
       cod_rechazo       SMALLINT)

       IN tmp_dbs1;

      LET     lc_elproc  =  "\"EXECUTE PROCEDURE sp_reporte_tes( '", 
                            vfecha_ini,       "', ", p_Tipo, ") \" "
      LET     lc_elproc  =  "echo ", lc_elproc CLIPPED, " > ", GUSER CLIPPED, ".eje_reporte_tes.sql"
      RUN     lc_elproc

      LET     v_comando  =  "nohup time dbaccess safre_tmp ",
                            GUSER CLIPPED, ".eje_reporte_tes.sql "
      LET     v_comando  =  v_comando CLIPPED
      RUN     v_comando

      DATABASE safre_af

      SELECT ruta_rescate
      INTO   vruta_rescate
      FROM   safre_af:seg_modulo
      WHERE  modulo_cod = "cta"

      LET lanza_proceso = "nohup fglgo CTAB041_1 '",vfecha_ini, "' ", p_Tipo

	    LET lanza_proceso  = lanza_proceso CLIPPED, " 1>"," ctab041_1" CLIPPED," 2> ctab041_1.err &"
      RUN lanza_proceso
 
      LET permisos = "chmod 777 ","ctab041_1"
	    RUN permisos

     DISPLAY "REPORTES GENERADOS EN LA RUTA: ",vruta_rescate CLIPPED AT 11,10
     DISPLAY "CON LOS NOMBRES : ","REPORTE_TES_ACEP_",HOY  USING "YYYYMMDD",".txt" AT 13,10
     DISPLAY "REPORTE_TES_ACEPRCV_",HOY  USING "YYYYMMDD",".txt" AT 14,28
     DISPLAY "REPORTE_TES_RECH_",HOY  USING "YYYYMMDD",".txt" AT 15,28

     DATABASE safre_af
     
     PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR enter

   ELSE
      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
      FOR enter
      LET ws = 1
   END IF

   IF ws = 1 THEN
     CLOSE WINDOW win6
     RETURN
   END IF

   CLOSE WINDOW win6

END FUNCTION


FUNCTION preliquida()
#--------------------

   DEFINE 
      lanza_proceso     CHAR(200)
      
   LET lanza_proceso = "fglgo CTALT01 "
   RUN lanza_proceso

END FUNCTION


FUNCTION liquida()
#--------------------

   DEFINE 
    lanza_proceso     CHAR(200)
      
   LET lanza_proceso = "fglgo CTALT06 "
   RUN lanza_proceso

END FUNCTION
####################################################################################
#Funcion que borra una identificacion previa
FUNCTION f_del_ident(p_fec_corte)
DEFINE p_fec_corte DATE,
       l_parametro CHAR(10),
       l_msj       CHAR(500)

LET l_msj = ""

LET l_parametro = p_fec_corte

ERROR "ELIMINANDO INFORMACION"

WHENEVER ERROR CONTINUE
   DELETE FROM safre_tmp:cta_transf_edad
   WHERE fecha_corte = p_fec_corte

     IF SQLCA.SQLCODE < 0 THEN
        LET l_msj = "ERROR AL BORRAR cta_transf_edad, FECHA: ", l_parametro
        CALL ERRORLOG(l_msj CLIPPED)
     END IF

   DELETE FROM safre_tmp:cta_transf_ed_gpo
   WHERE fecha_corte = p_fec_corte

     IF SQLCA.SQLCODE < 0 THEN
        LET l_msj = "ERROR AL BORRAR cta_transf_ed_gpo, FECHA: ", l_parametro
        CALL ERRORLOG(l_msj CLIPPED)
     END IF

   DELETE FROM safre_af:dis_ctrl_proceso
   WHERE proceso_cod = "CTAB041"
   AND etapa_cod = 10
   AND hora_final IS NOT NULL

     IF SQLCA.SQLCODE < 0 THEN
        LET l_msj = "ERROR AL BORRAR dis_ctrl_proceso, FECHA: ", l_parametro
        CALL ERRORLOG(l_msj CLIPPED)
     END IF
     
   DELETE FROM safre_tmp:tmp_fechas_transferencia
   WHERE fecha_corte = p_fec_corte
   AND tipo = 1
   
     IF SQLCA.SQLCODE < 0 THEN
        LET l_msj = "ERROR AL BORRAR tmp_fechas_transferencia, FECHA: ", l_parametro
        CALL ERRORLOG(l_msj CLIPPED)
     END IF
         
WHENEVER ERROR STOP

PROMPT  "IDENTIFICACION PREVIA ELIMINADA <ENTER> PARA CONTINUAR" 
         ATTRIBUTE(REVERSE) FOR CHAR gc_opc 

ERROR ""
END FUNCTION

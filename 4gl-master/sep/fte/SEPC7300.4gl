################################################################################
#Owner             => E.F.P                                                    #
#Programa SEPC007  => RECIBE RESPUESTA OP 27                                   #
#Fecha creacion    => 12 DE ABRIL DEL 2005                                     #
#Por               => JESUS YAÑEZ MORENO                                       #
#Sistema           => SEP                                                      #
################################################################################
#Actualizacion     => Se agrega log de errores. 06Jun2012 Alejandro Chagoya    #
################################################################################
#Actualizacion     => Se agrega funcion para insertar en sep_bitacora          #
#                  => un historial de los cambios de estado que se hacen       #
#                  => en sep_det_reg_sol_reclamante                            #
#Autor             => Alejandro Chagoya Salazar                                #
#Fecha             => 14 Marzo 2013                                            #
#Requerimiento     => INV-1872                                                 #
################################################################################
#Actualizacion     => Se valida que solo cuando esta la solicitud en eestado 4 #
#                  => actualiza a los estado 52 o 53                           #
#Autor             => Alejandro Chagoya Salazar                                #
#Fecha             => 04 Junio 2013                                            #
#Requerimiento     => INV-2115                                                 #
################################################################################
#Modificacion      => Se actualizan registros improcedentes                    #
#Autor             => Alejandro Chagoya Salazar    => 29 Octubre 2014          #
#Requerimiento     => INV-2797                                                 #
################################################################################
#Modificacion      => Se hace actualizacion de la cuo                          #
#                     registros que no tengan E.I. se rechazan con estado 7    #
#Autor             => Cristian Morales Roblero  Fecha : 27 febrero  2015       #
#Requerimiento     => INV-3169 MLM-3028 CPL-1878                               #
################################################################################
#Modificacion      => Se valida la fecha en que la solicitud de separacion fue #
#                     capturada para entrar o no en el proceso de la CUO       #
#Autor             => Cristian Morales Roblero  Fecha : 27 Mayo  2015          #
#Requerimiento     => INV-3415                                                 #
################################################################################
#Req CPL-2061 => CMR 20/08/2015 Se quita la mecanica realizada por la cuo      #
################################################################################
################################################################################
#Req CPL-3611 => 25/05/2022 cambia a modo batch por cambio a servicios rest    #
################################################################################

DATABASE safre_af
GLOBALS
   DEFINE g_xcodigo           smallint
   DEFINE xmarca              ,
          xresult             smallint
   DEFINE g_enter             char(1)

   DEFINE reg_sep_bitacora_wsresultadoconfronta RECORD LIKE sep_bitacora_wsresultadoconfronta.*

   DEFINE reg_det_separa RECORD
          tipo_registro         CHAR(2),
          cont_servicio         INTEGER,
          nss_asociado          CHAR(11),
          tipo_entidad_nss_involucrado char(02),
          clave_entidad_involucrado char(03),
          resulta_operacion     CHAR(2),
          diag_proceso1         CHAR(3),
          diag_proceso2         CHAR(3),
          diag_proceso3         CHAR(3)
   END RECORD

   DEFINE
          carga_reg              CHAR(330),
          usuario                VARCHAR(20),
          enter                  CHAR(001),
          motivo_rechazo_1       CHAR(003),
          motivo_rechazo_2       CHAR(003),
          motivo_rechazo_3       CHAR(003),
          resulta_oper           CHAR(002),
          generar                CHAR(020),
          nombre_archivo         CHAR(020),
          archivo_separa         CHAR(200),
          c10_fecha_presenta     CHAR(010),
          c10_fecha_nac_sep      CHAR(010),
          c10_fecha_afilia       CHAR(010),
           c10_fecha_marca       CHAR(010)

   DEFINE s_recibido,
          s_rechazado,
          cont_det   ,
          cuantos   ,
          cont                   SMALLINT

   DEFINE ultimo_folio           INTEGER
   DEFINE gr_modulo              RECORD LIKE seg_modulo.*
   DEFINE gs_afore               SMALLINT
   DEFINE gc_comando             CHAR(1000)

   DEFINE m_estado_ant INTEGER,
          m_edo_impro  SMALLINT,        #improcedente
          m_cadena     CHAR(300)

   DEFINE vid_ws                    DEC(10,0),
          vidsolicitudseparacion    DEC(10,0)
  
END GLOBALS

 GLOBALS "SEPRPTS.4gl"

MAIN

   LET vid_ws                   = ARG_VAL(1)
   LET vidsolicitudseparacion   = ARG_VAL(2)

   CALL init()

   LET msg = gr_modulo.ruta_listados CLIPPED,'/',usuario CLIPPED,'.SEPC7300.',vid_ws using "&&&&&&&&&&",'.log'

 LET msg = msg clipped

   CALL STARTLOG(msg)

   LET HORA = TIME
   LET msg = "SEPC7000 ",HOY," ",HORA," INICIA RECEPCION DE RESPONSE Resultado Confronta"
   CALL ERRORLOG(msg)

   CALL fn_procesa_ws_response() #lap

   CALL asigna_globales()

   LET HORA = TIME
   LET msg = "SEPC7000 ",HOY," ",HORA," PROCESO FINALIZADO..."
   CALL ERRORLOG(msg)

   IF g_xcodigo = 1 THEN EXIT PROGRAM 0 ELSE EXIT PROGRAM 1 END IF

END MAIN



#--------------
FUNCTION init()
#--------------

   LET HOY  = TODAY

   SELECT   estado
            ,USER
   INTO     s_recibido
            ,usuario
   FROM     sep_estado_separacion
   WHERE    des_estado = "RECIBIDO OP 27"

   SELECT   estado
   INTO     s_rechazado
   FROM     sep_estado_separacion
   WHERE    des_estado = "RECHAZO OP 27"

   SELECT   estado
   INTO     m_edo_impro
   FROM     sep_estado_separacion
   WHERE    des_estado
   MATCHES "*HISTORICO IMPROCEDEN*"

   SELECT *
   INTO   gr_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'sep'

   SELECT   codigo_afore
   INTO     gs_afore
   FROM     safre_af:tab_afore_local
   GROUP BY 1

   LET   m_cadena =  " "
   LET   m_cadena =  "EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?)"
   LET   m_cadena =  m_cadena CLIPPED
   PREPARE pre_desmarca FROM m_cadena

END FUNCTION

#---------------------------------
FUNCTION fn_procesa_ws_response()
#---------------------------------

   DEFINE l_diag_confronta       CHAR(2),
          l_clasifica_separacion CHAR(1),
          lidSolicitudSeparacion INTEGER

   SELECT a.*
   INTO   reg_sep_bitacora_wsresultadoconfronta.*
   FROM   sep_bitacora_wsresultadoconfronta a
   WHERE  a.idbitacorawsresultadoconfronta =  vid_ws

   LET HORA = TIME

   LET msg = "SEPC7300 ",HOY," ",HORA," INVADIDO       : ",reg_sep_bitacora_wsresultadoconfronta.nssInvadido
   CALL ERRORLOG(msg)

   LET msg = "SEPC7300 ",HOY," ",HORA," DIAG CONFRONTA : ",reg_sep_bitacora_wsresultadoconfronta.diagnosticoconfrontaREQ
   CALL ERRORLOG(msg)

   LET msg = "SEPC7300 ",HOY," ",HORA," CLASIFICACION  : ",reg_sep_bitacora_wsresultadoconfronta.clasificacionseparacionREQ
   CALL ERRORLOG(msg)

   LET msg = "SEPC7300 ",HOY," ",HORA," RESULTADO OP   : ",reg_sep_bitacora_wsresultadoconfronta.resultadooperacion
   CALL ERRORLOG(msg)

   LET msg = "SEPC7300 ",HOY," ",HORA," DIAGNOSTICO    : ",reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
   CALL ERRORLOG(msg)

   INITIALIZE l_diag_confronta        , 
              l_clasifica_separacion  ,
              lidSolicitudSeparacion TO NULL

   IF reg_sep_bitacora_wsresultadoconfronta.diagnosticoconfrontaREQ = "02" THEN
      IF reg_sep_bitacora_wsresultadoconfronta.resultadooperacion = "01" THEN
            --rechazado en confronta aceptado se desmarca

            UPDATE sep_det_reg_sol_reclamante
            SET    estado    = m_edo_impro  --improcedente aceptado
            WHERE  correlativo = lidSolicitudSeparacion
            LET g_xcodigo = 1
  
            UPDATE sep_det_solicitud
            SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                   diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
            WHERE  idsolicitudseparacion = vidsolicitudseparacion
 
            CALL f_desmarca_cuenta(reg_sep_bitacora_wsresultadoconfronta.nssinvadidoreq)   #desmarca invadido

            LET HORA = TIME
            LET msg = "SEPC7300 ",HOY," ",HORA," INVADIDO DESMARCADO y ACTUALIZADO A ESTADO IMPROCEDENTE..."
            CALL ERRORLOG(msg)

      ELSE
            UPDATE sep_det_reg_sol_reclamante
            SET    estado    = s_rechazado
            WHERE  correlativo = lidSolicitudSeparacion
            LET g_xcodigo = 1

            UPDATE sep_det_solicitud
            SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                   diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
            WHERE  idsolicitudseparacion = vidsolicitudseparacion
      END IF
   ELSE 
    IF reg_sep_bitacora_wsresultadoconfronta.resultadooperacion = "01" THEN

      INITIALIZE ultimo_folio TO NULL

      SELECT   a.folio        ,
               a.correlativo
      INTO     ultimo_folio
               , lidSolicitudSeparacion
      FROM     sep_det_reg_sol_reclamante a
      WHERE    a.correlativo = vidsolicitudseparacion
      AND      a.estado in (4,6)

      IF SQLCA.SQLCODE = 0 THEN #modificacion cuo
   
      INSERT INTO sep_det03_solicitud
      VALUES(ultimo_folio                                                 ,
             "03"                                                         , --tipo_registro
             2                                                            , -- constante 2 por ws ya no llega 
             reg_sep_bitacora_wsresultadoconfronta.nssasociado            ,
             reg_sep_bitacora_wsresultadoconfronta.tipoentidadnssasociado ,
             reg_sep_bitacora_wsresultadoconfronta.claveentidadnssasociado,
             reg_sep_bitacora_wsresultadoconfronta.resultadooperacion     ,
             reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso     ,
             ""                                                           ,
             ""                                                           ,
             vidsolicitudseparacion
            )
      END IF

      SELECT   a.diag_confronta  ,        #CPL-2076
               a.clasifica_separacion
      INTO     l_diag_confronta ,
               l_clasifica_separacion
      FROM     sep_det_solicitud  a
      WHERE    a.idSolicitudSeparacion = lidSolicitudSeparacion

      SELECT   "ok"
      FROM     afi_mae_afiliado a
      WHERE    a.n_seguro  =  reg_sep_bitacora_wsresultadoconfronta.nssasociado
      AND      a.tipo_solicitud <> 5
      GROUP BY 1


      IF STATUS <> NOTFOUND THEN
         SELECT   "ok"
         FROM     cta_act_marca a
         WHERE    a.nss = reg_sep_bitacora_wsresultadoconfronta.nssasociado
         AND      a.marca_cod = 120
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN

            #modificacion cuo
            SELECT   estado
            INTO     m_estado_ant
            FROM     safre_af:sep_det_reg_sol_reclamante
            WHERE    correlativo = lidSolicitudSeparacion
            #modificacion cuo

            UPDATE   sep_det_reg_sol_reclamante
            SET      estado = s_recibido
            WHERE    correlativo = lidSolicitudSeparacion
            AND      estado = 4
            LET g_xcodigo = 1

         ELSE

            IF (l_diag_confronta = "01") THEN
               IF (l_clasifica_separacion = "B") THEN
                  # INV-1872 INI                                 --#
                  LET m_estado_ant= NULL

                  SELECT   estado
                  INTO     m_estado_ant
                  FROM     safre_af:sep_det_reg_sol_reclamante
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------
                  #INV-1872 FIN                                 --#

                  UPDATE   sep_det_reg_sol_reclamante
                  SET      estado = 53
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------
                  LET g_xcodigo = 1

               END IF

               IF (l_clasifica_separacion = "C") THEN
                  # INV-1872 INI                                 --#
                  LET m_estado_ant= NULL

                  SELECT   estado
                  INTO     m_estado_ant
                  FROM     safre_af:sep_det_reg_sol_reclamante
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------#
                  #INV-1872 INI                                 --#

                  UPDATE   sep_det_reg_sol_reclamante
                  SET      estado = 52
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------#        
                  LET g_xcodigo = 1

                  UPDATE sep_det_solicitud
                  SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                         diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
                  WHERE   idsolicitudseparacion = vidsolicitudseparacion

               END IF

               IF (l_clasifica_separacion = "D") THEN

                  # INV-1872 INI                                 --#
                  SELECT   estado
                  INTO     m_estado_ant
                  FROM     safre_af:sep_det_reg_sol_reclamante
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4 #-----------------INV-2115-----------------#
                  #   INV-1872 FIN                                 --#

                  UPDATE   sep_det_reg_sol_reclamante
                  SET      estado = 52
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND     estado = 4 #-----------------INV-2115-----------------#
                  LET g_xcodigo = 1

                  UPDATE sep_det_solicitud
                  SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                         diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
                  WHERE   idsolicitudseparacion = vidsolicitudseparacion

               END IF
               #INI CPL-2076
               IF (l_clasifica_separacion = "E") THEN

                  LET m_estado_ant= NULL

                  SELECT   estado
                  INTO     m_estado_ant
                  FROM     safre_af:sep_det_reg_sol_reclamante
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4


                  UPDATE   sep_det_reg_sol_reclamante
                  SET      estado = 52
                  WHERE    correlativo = lidSolicitudSeparacion
                  AND      estado = 4
                  LET g_xcodigo = 1

                  UPDATE sep_det_solicitud
                  SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                         diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
                  WHERE   idsolicitudseparacion = vidsolicitudseparacion

               END IF
               #fin CPL-2076

               --marca nss asociado para diag=01
                 IF (reg_sep_bitacora_wsresultadoconfronta.nssAsociado <> "00000000000") THEN
                    IF (reg_sep_bitacora_wsresultadoconfronta.nssAsociado IS NOT NULL     )  THEN
                     IF (reg_sep_bitacora_wsresultadoconfronta.nssAsociado <> ""           )   THEN
                      IF (reg_sep_bitacora_wsresultadoconfronta.nssAsociado <> reg_sep_bitacora_wsresultadoconfronta.nssInvadido) THEN

                         CALL f_marca_cuenta(reg_sep_bitacora_wsresultadoconfronta.nssAsociado,lidSolicitudSeparacion)
                         RETURNING xmarca,xresult
                         IF xresult = 0 THEN
                            LET HORA = TIME
                            LET msg = "SEPC7000 ",HOY," ",HORA," PROBLEMA AL MARCAR LA CUENTA"
                            CALL ERRORLOG(msg)
                            LET HORA = TIME
                            LET msg = "SEPC7000 ",HOY," ",HORA," XMARCA: ",xmarca," XRESULT: ",xresult
                            CALL ERRORLOG(msg)
                         ELSE
                            LET HORA = TIME
                            LET msg = "SEPC7000 ",HOY," ",HORA," PROBLEMA AL MARCAR LA CUENTA"
                            CALL ERRORLOG(msg)
                            LET HORA = TIME
                            LET msg =  "SEPC7000 ",HOY," ",HORA," XMARCA: ",xmarca," XRESULT: ",xresult
                            CALL ERRORLOG(msg)
                         END IF
                      END IF
                     END IF
                    END IF
                 END IF
            ELSE
               #INV-1872 INI                                 --#
               SELECT   estado
               INTO     m_estado_ant
               FROM     safre_af:sep_det_reg_sol_reclamante
               WHERE    correlativo = lidSolicitudSeparacion
               #INV-1872 FIN                                 --#

               UPDATE   sep_det_reg_sol_reclamante
               SET      estado = 5
               WHERE    correlativo = lidSolicitudSeparacion
               AND      estado <> 7    # cuo
               LET g_xcodigo = 1

               UPDATE sep_det_solicitud
               SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                      diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
               WHERE   idsolicitudseparacion = vidsolicitudseparacion

            END IF
         END IF
      ELSE

         SELECT   estado
         INTO     m_estado_ant
         FROM     safre_af:sep_det_reg_sol_reclamante
         WHERE    correlativo = lidSolicitudSeparacion

         IF (l_clasifica_separacion = "E") THEN

            UPDATE  sep_det_reg_sol_reclamante
            SET     estado = 52
            WHERE   correlativo = lidSolicitudSeparacion
            AND     estado = 4
            LET g_xcodigo = 1

            UPDATE sep_det_solicitud
            SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                   diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
            WHERE   idsolicitudseparacion = vidsolicitudseparacion

         ELSE
            UPDATE  sep_det_reg_sol_reclamante
            SET     estado = s_recibido
            WHERE   correlativo = lidSolicitudSeparacion
            AND     estado = 4
            LET g_xcodigo = 1

            UPDATE sep_det_solicitud
            SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                   diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
            WHERE   idsolicitudseparacion = vidsolicitudseparacion

         END IF
      END IF
    ELSE --RECHAZADOS resultadooperacion=02

     LET msg = "RECHAZO: ",vidsolicitudseparacion
     CALL ERRORLOG(msg)

     LET msg = "RESULTADO OPERACION: ",reg_sep_bitacora_wsresultadoconfronta.resultadooperacion
     CALL ERRORLOG(msg)

     LET msg = "DIAGNOSTICO: ",reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
     CALL ERRORLOG(msg)

         UPDATE  sep_det_reg_sol_reclamante
         SET     estado = s_rechazado
         WHERE   correlativo = vidsolicitudseparacion
         LET g_xcodigo = 1

         UPDATE sep_det_solicitud
         SET    resultado_operacion = reg_sep_bitacora_wsresultadoconfronta.resultadooperacion,
                diag_proc1 = reg_sep_bitacora_wsresultadoconfronta.diagnosticoproceso
         WHERE   idsolicitudseparacion = vidsolicitudseparacion

    END IF --FIN RESULTADO OPERACION
   END IF

END FUNCTION

#-------------------------
FUNCTION asigna_globales()
#-------------------------

#== ASIGNACION DE VARIABLES QUE SE UTILIZA EL PROGRAMA SEPREPGRAL.4gl===

   LET  g_tabname      =  "sep_det_solicitud a"
   LET  g_tabname_1    =  "sep_det03_solicitud b"
   LET  g_nombre_prog  =  "SEPC7300"
   LET  g_tipo_desc1   =  "REPORTE DE RESPONSE OP(27)"
   let g_total_cuentas = cont

END FUNCTION

#---------------------------------
FUNCTION f_desmarca_cuenta(p_nss)
#---------------------------------

   DEFINE p_nss CHAR(11)
   DEFINE lr_act  RECORD LIKE safre_af:cta_act_marca.*
   DEFINE lr_marca RECORD
          nss            CHAR(11),
          marca_entra    SMALLINT,
          correlativo    INTEGER,
          estado         SMALLINT,
          marca_causa    SMALLINT,
          usuario        CHAR(08)
     END RECORD

   INITIALIZE lr_act.*, lr_marca.* TO NULL

   SELECT * INTO lr_act.*
   FROM cta_act_marca
   WHERE nss = p_nss
   AND marca_cod = 280   #marca de separacion

   IF SQLCA.SQLCODE = 100 THEN   # no esta marcada
      RETURN
   END IF

   LET lr_marca.nss          = p_nss
   LET lr_marca.marca_entra  = 280
   LET lr_marca.correlativo  = lr_act.correlativo
   LET lr_marca.estado       = 30
   LET lr_marca.marca_causa  = 280
   LET lr_marca.usuario      = usuario

   EXECUTE pre_desmarca USING lr_marca.*

END FUNCTION

FUNCTION f_marca_cuenta(vnss,dd_corr)
#--marca la cuenta asociada en proceso de separacion de cuentas
-----------------------------------------------------

DEFINE vnss     char(011)
DEFINE dd_corr  integer
DEFINE vmarcaentra smallint
DEFINE lmarca char(100)
DEFINE vestadomarca smallint
DEFINE vrechazocod  smallint
DEFINE vmarcacausa smallint
DEFINE vfechacausa date
DEFINE vusuario char(010)
DEFINE xmarca smallint
DEFINE xrechazo smallint

LET vestadomarca = 0
LET vmarcaentra = 590 --separacion nss asociado
LET vrechazocod = 0
LET vmarcacausa = 0
LET vfechacausa = ""

SELECT USER
INTO   vusuario
FROM   tab_afore_local
group by 1

LET lmarca = "EXECUTE PROCEDURE marca_cuenta(?,?,?,?,?,?,?,?)"

PREPARE marcaje FROM lmarca

DECLARE cur_marca CURSOR FOR marcaje

FOREACH cur_marca USING vnss        ,
                        vmarcaentra ,
                        dd_corr     ,
                        vestadomarca ,
                        vrechazocod ,
                        vmarcacausa ,
                        vfechacausa ,
                        vusuario
                  INTO xmarca    ,
                       xrechazo
END FOREACH

if xrechazo <> 0 THEN
   LET HORA = TIME
   LET msg = "Marca 590 rechazada por convivencia : ",xrechazo
   CALL ERRORLOG(msg)
else
   LET HORA = TIME
   LET msg = "Marca 590 procedente ..."
   CALL ERRORLOG(msg)
end if

return xmarca ,xrechazo

END FUNCTION

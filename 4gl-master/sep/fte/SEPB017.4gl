##############################################################################
#MODULO        : SEP                                                         #
#PROGRAMA      : SEPB017                                                     #
#DESCRIPCION   : Liquida  las parejas de Separacion de Cuentas que se        #
#                encuentran en estado 55 (liquidadas)                        #
#AUTOR         : JESUS YAÑEZ MORENO                                          #
#FECHA         : 3 abr 2010                                                  #
#------------------------------MODIFICACIONES--------------------------------#
#Actualizacion : Se actualiza codigo con version de XXI 14jun2012            #
#              : Alejandro Chagoya                                           #
##############################################################################
#Requerimiento     => INV-1524                                               #
#Fecha y Autor     => 09-Ago-2012    Alejandro Chagoya Salazar               #
#Descripcion       => Se cambia  fn_sol_transf_sie por fn_sep_transf_sie     #
#                  => se cambia metio de TES de 0 a 10                       #
##############################################################################
#Requerimiento     => INV-1812                                               #
#Fecha y Autor     => 11-febrero-2013   Alejandro Chagoya Salazar            #
#Descripcion       => Se valida el regimen por subcuenta de el invadido      #
#                  => para hacer el cargo y el abono correspondiente         #
#                  => Se agrega log de errores por usuario                   #
##############################################################################

DATABASE safre_af

GLOBALS
DEFINE  g_folio          INTEGER
DEFINE  g_total          INTEGER

DEFINE  g_enter          SMALLINT,
        g_confirma       SMALLINT

DEFINE  g_today          ,
        g_fecha_max      ,
        g_fecha_habil    ,
        g_primer_natural ,
        g_primer_habil   DATE

END GLOBALS

####################################################################
MAIN

 OPTIONS INPUT WRAP,
 PROMPT LINE LAST  ,
 ACCEPT KEY CONTROL-I
 DEFER INTERRUPT

CALL STARTLOG(FGL_GETENV("USER")||".SEPB017.log")

 LET g_today = TODAY

 OPEN WINDOW v_provisiona at 2,2 with form "SEPB016" attribute(border)

    DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " SEPB017          LIQUIDACION DE SEPARACON DE CUENTAS                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY g_today USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

 LET g_today = TODAY

 -- se validan las fechas de ejecucion del proceso
  CALL valida_fecha()

  -- se muestran datos iniciales informativos
   SELECT COUNT(*) INTO g_total
   FROM   sep_det_reg_sol_reclamante a
   WHERE  a.estado = 55 -- provisionadas

 DISPLAY BY NAME g_total,g_primer_habil,g_today

 -- se pide confirmacion de liquidacion
 CALL pregunta("Confirmar Liquidacion ? [s/n]...") RETURNING g_confirma

-- si no se confirma se regresa a la pantalla de lista
   IF NOT g_confirma THEN
      ERROR "Cancelando Liquidacion..."
      SLEEP  2
      EXIT PROGRAM
   ELSE
      ERROR "Procesando Liquidacion.."
   END IF

   -- liquidacion confirmada
   CALL liquidar()

   PROMPT "Liquidacion Concluida <enter> para Continuar..." FOR CHAR g_enter

END MAIN

######################################################
#calcula el dia habil dado en ciclo
FUNCTION cal_fecha_avant(xfecha,ndias)
DEFINE xfecha,fhabil     DATE
DEFINE ndias        ,
       cuenta       ,
       dia_semana      SMALLINT

     LET fhabil = xfecha
     LET cuenta = 1

 WHILE cuenta <= ndias
    LET dia_semana = weekday(fhabil)
    IF  dia_semana = 0 OR dia_semana = 6 THEN
        LET fhabil = fhabil + 1
        CONTINUE WHILE
    ELSE
       SELECT "ok"
       FROM tab_feriado
       WHERE  feria_fecha = fhabil
       IF STATUS <> NOTFOUND THEN
          LET fhabil = fhabil + 1
          CONTINUE WHILE
       ELSE
          IF ndias = 1 THEN EXIT WHILE END IF
          LET cuenta = cuenta + 1
          IF cuenta <= ndias THEN
             LET fhabil = fhabil + 1
          END IF
       END IF
    END IF
 END WHILE

RETURN fhabil

END FUNCTION

#####################################################################
# valida que la fecha sea valida para provisionar
FUNCTION valida_fecha()

 CALL cal_fecha_avant(g_today,1) RETURNING g_fecha_habil

 IF g_today <> g_fecha_habil THEN
    PROMPT "Dia inhabil, no se puede liquidar...<enter>" FOR CHAR g_enter
    EXIT PROGRAM
 END IF

 LET g_primer_natural = MDY(MONTH(g_today),"01",YEAR(g_today))

 CALL cal_fecha_avant(g_primer_natural,8) RETURNING g_fecha_max

 IF g_today > g_fecha_max THEN
    PROMPT "Fecha fuera de tiempo para liquidar...<enter>" FOR CHAR g_enter
    EXIT PROGRAM
 END IF

 CALL cal_fecha_avant(g_primer_natural,1) RETURNING g_primer_habil

END FUNCTION

#################################################################
#ciclo while para preguntas tipo s/n
FUNCTION pregunta(largumento)
DEFINE largumento   CHAR(60) ,
       lrespuesta   SMALLINT ,
       lenter       CHAR(001)

   LET lrespuesta = 0

  WHILE TRUE
       PROMPT largumento CLIPPED FOR CHAR lenter
       IF lenter MATCHES "[SsNn]" THEN
          IF lenter MATCHES "[Ss]" THEN
             LET lrespuesta = 1
             EXIT WHILE
          ELSE
             LET lrespuesta = 0
             EXIT WHILE
          END IF
       END IF
  END WHILE

 RETURN lrespuesta

END FUNCTION

#########################################################################
# liquida los movimientos de separacion de cuentas provisionados
FUNCTION liquidar()
DEFINE    l_importe_2d       DEC(12,2)
DEFINE    l_qry              CHAR(300),
          l_qry_tes          CHAR(300),
          lprecio            DEC(16,6),
          lfechav            DATE,
          sie_tes,
          tipo_tes,
          medio_tes,
          lexiste,lrechazo   SMALLINT ,
          lfolio integer
   DEFINE l_provisionadas     RECORD
              idSolicitudSeparacion INTEGER,
              folio                 INTEGER,
              invadido              CHAR(11),
              asociado              CHAR(11)
        END RECORD
   DEFINE l_dis_provision record LIKE safre_af:dis_provision.*

#INV-1524_INI
 LET tipo_tes  = 11 --tipo de trasnferencia entre siefores(Separacion)
 LET medio_tes = 10 --medios de trasnferencia entre siefores

  LET l_qry_tes =
      "EXECUTE PROCEDURE fn_sep_transf_sie(?,?,?,?,?,?,?,?)"
  PREPARE qry_tes FROM l_qry_tes
  DECLARE cur_tes CURSOR FOR qry_tes

   DECLARE cur_provisionadas CURSOR FOR

   SELECT a.correlativo ,
           a.folio       ,
           a.n_seguro    ,   --invadido
           a.nss
   FROM sep_det_reg_sol_reclamante a
   WHERE  a.estado = 55 -- provisionadas
   ORDER BY a.correlativo

   LET l_qry = " SELECT a.* " ,
               " FROM   dis_provision a " ,
               " WHERE  a.folio = ? " ,
               " AND    a.consecutivo_lote = ? "

   PREPARE qry FROM l_qry
   DECLARE cur_dis_provision CURSOR FOR qry

   FOREACH cur_provisionadas into l_provisionadas.*

       FOREACH cur_dis_provision USING l_provisionadas.folio,
                                       l_provisionadas.idSolicitudSeparacion
                                  INTO l_dis_provision.*

           IF l_dis_provision.siefore = 11 THEN
               LET lfechav  = g_primer_natural
               SELECT a.precio_del_dia INTO lprecio
               FROM  glo_valor_accion a
               WHERE  a.fecha_valuacion = g_primer_natural
               AND    a.codigo_siefore  = l_dis_provision.siefore
           ELSE

               #-- INV-1812 INI
               SELECT a.codigo_siefore INTO l_dis_provision.siefore
                 FROM cta_regimen a
                WHERE a.nss = l_provisionadas.invadido
                  AND a.subcuenta = l_dis_provision.subcuenta
               #-- INV-1812 FIN 

               LET lfechav  = g_primer_habil
               SELECT a.precio_del_dia INTO lprecio
               FROM glo_valor_accion a
               WHERE  a.fecha_valuacion = g_primer_habil
               AND    a.codigo_siefore  = l_dis_provision.siefore
           END IF

             LET     l_dis_provision.monto_en_pesos    =
                     l_dis_provision.monto_en_acciones  * lprecio
             IF  l_dis_provision.siefore  =  11    THEN
                 LET l_importe_2d = l_dis_provision.monto_en_pesos
                 LET l_dis_provision.monto_en_pesos  =  l_importe_2d
             END IF

           LET l_dis_provision.fecha_pago = lfechav
           LET l_dis_provision.fecha_valor = lfechav
           LET l_dis_provision.fecha_conversion = g_primer_habil
           LET l_dis_provision.estado  = 8 -- liquidado
           
           IF l_dis_provision.monto_en_pesos <> 0 THEN
              INSERT INTO dis_cuenta values(l_dis_provision.*)
           END IF

           ---- verifica si se debe generar una sol de tes
           IF l_dis_provision.nss = l_provisionadas.asociado THEN

              SELECT a.codigo_siefore INTO sie_tes
              FROM cta_regimen a
              WHERE a.nss = l_provisionadas.asociado
              AND   a.subcuenta = l_dis_provision.subcuenta

              IF sie_tes <> l_dis_provision.siefore  THEN
                  FOREACH cur_tes USING l_provisionadas.asociado ,
                                        l_dis_provision.subcuenta ,
                                        l_dis_provision.siefore   ,
                                        sie_tes                   ,
                                        tipo_tes                  ,
                                        l_dis_provision.folio     ,
                                        medio_tes                 ,
                                        l_dis_provision.fecha_conversion
                                  INTO  lexiste ,
                                        lrechazo ,
                                        lfolio
                  END FOREACH
              END IF
           END IF
       END FOREACH  -- fin liquidacion

#INV-1524 FIN
     UPDATE sep_det_reg_sol_reclamante
     SET    estado = 8  --liquidado
     WHERE  correlativo = l_provisionadas.idSolicitudSeparacion

 END FOREACH  -- fin provisionadas

END FUNCTION

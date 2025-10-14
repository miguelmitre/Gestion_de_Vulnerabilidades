################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Sistema           => RET                                                      #
#Owner             => E.F.P                                                    #
#Programa RETM011  => MANTENEDOR DE RETIROS COMPLEMENTARIOS                    #
#By                => ISAI JIMENEZ ROJAS                                       #
#                  => COPIA DEL PROGRAMA RETM011 DE ACUERDO AL ANALISIS DE SVP #
#Fecha creacion    => 01-OCT-2014                  v1.0                        #
#Actualizacion     => 22-OCT-2014                  v1.1     MLM-1886           #
#                  => Se implementa fn_calcula_isr_vol                         #
#                  => recibe: fecha del aporte                                 #
#                  =>         fecha de liquidacion del retiro                  #
#                  =>         monto del aporte (monto a retirar) en acciones   #
#                  =>         siefore del aporte                               #
#                  => regresa: rendimiendo en pesos (interes real)             #
#                  =>          isr 6% en pesos                                 #
#                  =>          retencion 20% en pesos                          #
#                  =>          periodo del aporte utilizado(inpc)              #
#                  =>          periodo del retiro utilizado(inpc)              #
#                  =>          badera de aporte cambiado (inpc)                #
#                  =>          badera de retiro cambiado (inpc)                #
#Actualizacion     => ISAI JIMENEZ ROJAS 12-dic-14       v1.1       MLM-1886   #
#                  => Implementacion de voluntarias subcuentas (22 y 23) CBF   #
#------------------------------------------------------------------------------#
#Requerimiento     => MLM-3140    15-Abr-2015   Alejandro Chagoya Salazar      #
#Actualizacion     => Se Agrega validacion en cta_saldo_vol con traspaso y UNI #
################################################################################
# fecha Actuali   => MLM-3150 13/05/2015                                       #
# Autor           => Cristian  Morales Roblero                                 #
# Actualizacion   => Se Hace la hace la integracion de los retiros voluntarias #
#                    sin beneficio fiscal subctas 3y 10 tipos de retiros 1 y 2 #
################################################################################
# fecha Actuali   => MLM-3270 26/06/2015                                       #
# Autor           => Cristian  Morales Roblero                                 #
# Actualizacion   => se cambia el consecutivo_lote de los movimientos de isr   #
#                    se queda el mismo consecutivo que la solicitud            #
################################################################################
#Req:MLM-3289  => ACS la fecha del trasp ced, como fecha de primera aport v1.2 #
################################################################################
#Req_MLM-3370  => CMR se cambia la variable par al identificacion del mov      #
################################################################################
#Req:MLM-2929  => CMR 04/08/2015 se agrega retiro tipo 13  de reinversion      #
################################################################################
#Req:MLM-3420  => CMR 04/08/2015 se agrega retiro tipo 6 ret. vol mixto total  #
#Req:MLM-3444  => CMR 10/09/2015 se corrigue la manera de calcular el imp. neto#
#Req:MLM-3455  => CMR 14/09/2015 se se agrupan las fechas de TAA               #
#Req:MLM-3682  => CMR 07/01/2016 se actualiza tasa de retencion de isr         #
################################################################################

-- Tipo retiro:
--    1 - Retiro Parcial voluntarias SBF
--    2 - Retiro Total   voluntarias SBF
--    3 - Retiro Parcial voluntarias CBf
--    4 - Retiro Total   voluntarias CBF
--    6 - RETIRO TOTAL VOLUNTARIAS MIXTO
--    7 - Retiro Parcial complementarias SBF
--    8 - Retiro Total   complementarias SBF
--    9 - Retiro Parcial complementarias CBf
--   10 - Retiro Total   complementarias CBF
--   11 - Retiro Parcual mixto
--   12 - Retiro Total   mixto
--   13 - Retiro Reinversion

--   13 - RETIRO PARCIAL LARGO PLAZO SBF   ##CPL-3203
--   14 - RETIRO TOTAL LARGO PLAZO   SBF   ##CPL-3203
--   15 - RETIRO TOTAL BENEFICIARIOS 

--------------------------------------------------------------------------------

DATABASE safre_af

GLOBALS
    DEFINE gr_estados         RECORD
           capturado          SMALLINT ,
           provisionado       SMALLINT ,
           liquidado          SMALLINT ,
           rechazado          SMALLINT ,
           confirmado         SMALLINT         -- CPL-2824
           END RECORD

    DEFINE reg_1              RECORD
           n_folio            LIKE afi_mae_afiliado.n_folio     ,
           n_seguro           LIKE afi_mae_afiliado.n_seguro    ,
           n_rfc              LIKE afi_mae_afiliado.n_rfc       ,
           n_unico            LIKE afi_mae_afiliado.n_unico     ,
           fentcons           LIKE afi_mae_afiliado.fentcons    ,
           paterno            LIKE afi_mae_afiliado.paterno     ,
           materno            LIKE afi_mae_afiliado.materno     ,
           nombres            LIKE afi_mae_afiliado.nombres
       END RECORD

    DEFINE reg_2_array        ARRAY[5000] OF RECORD
           var_nula           CHAR(1)                           ,
           n_folio            LIKE afi_mae_afiliado.n_folio     ,
           n_seguro           LIKE afi_mae_afiliado.n_seguro    ,
           n_rfc              LIKE afi_mae_afiliado.n_rfc       ,
           n_unico            LIKE afi_mae_afiliado.n_unico     ,
           fentcons           LIKE afi_mae_afiliado.fentcons    ,
           paterno            LIKE afi_mae_afiliado.paterno     ,
           materno            LIKE afi_mae_afiliado.materno     ,
           nombres            LIKE afi_mae_afiliado.nombres     ,

           acc_sbf_sb1        LIKE dis_cuenta.monto_en_acciones ,
           acc_sbf_s6         LIKE dis_cuenta.monto_en_acciones ,
           pes_sbf            LIKE dis_cuenta.monto_en_pesos    ,
           acc_cbf_sb1        LIKE dis_cuenta.monto_en_acciones ,
           acc_cbf_s6         LIKE dis_cuenta.monto_en_acciones ,
           pes_cbf            LIKE dis_cuenta.monto_en_pesos    ,
           pes                LIKE dis_cuenta.monto_en_pesos    ,

           fecha_ult_ret      LIKE ret_cta_vol.fecha_ult_ret    ,
           n_folio_sol        LIKE ret_cta_vol.n_folio_sol      ,
           tipo_ret           LIKE ret_cta_vol.tipo_ret         ,
           des_tipo_ret       CHAR(40)                          ,
           mto_solic          LIKE ret_cta_vol.mto_solic        ,
           porcentaje_solic   LIKE ret_cta_vol.porcentaje_solic ,
           edad               LIKE ret_cta_vol.edad             ,
           fecha_solic        LIKE ret_cta_vol.fecha_solic      ,
           fecha_captura      LIKE ret_cta_vol.fecha_captura    ,
           ultimo_proceso     LIKE ret_cta_vol.ultimo_proceso   ,
           pension_invalidez  LIKE ret_cta_vol.pension_invalidez,
           deduccion          LIKE ret_cta_vol.deduccion        ,
           cod_rechazo_ent    LIKE ret_cta_vol.cod_rechazo_ent  ,   #* Rech. *
           consecutivo        INTEGER                           ,
           usuario            LIKE ret_cta_vol.usuario          ,
           estado             LIKE ret_cta_vol.estado           ,
           desc_estado        CHAR(30)
       END RECORD

    DEFINE gr_general         RECORD
           mto_acc_sb1        LIKE dis_cuenta.monto_en_acciones ,
           mto_acc_s6         LIKE dis_cuenta.monto_en_acciones ,
           mto_acc_12         LIKE dis_cuenta.monto_en_acciones ,
           mto_acc_11         LIKE dis_cuenta.monto_en_acciones ,
           monto_en_pesos     LIKE dis_cuenta.monto_en_pesos    ,

           fecha_ult_ret      LIKE ret_cta_vol.fecha_ult_ret    ,
           n_folio_sol        LIKE ret_cta_vol.n_folio_sol      ,
           tipo_ret           LIKE ret_cta_vol.tipo_ret         ,
           des_tipo_ret       CHAR(40)                          ,
           mto_solic          LIKE ret_cta_vol.mto_solic        ,
           porcentaje_solic   LIKE ret_cta_vol.porcentaje_solic ,
           tipo_pago          LIKE ret_cta_vol.tipo_pago        ,
           des_tipo_pago      CHAR(40)                          ,
           edad               LIKE ret_cta_vol.edad             ,
           fecha_solic        LIKE ret_cta_vol.fecha_solic      ,
           fecha_captura      LIKE ret_cta_vol.fecha_captura    ,
           ultimo_proceso     LIKE ret_cta_vol.ultimo_proceso   ,
           pension_invalidez  LIKE ret_cta_vol.pension_invalidez,
           deduccion          LIKE ret_cta_vol.deduccion        ,
           cod_rechazo_ent		LIKE ret_cta_vol.cod_rechazo_ent  ,   #* Rech. *
           consecutivo        INTEGER                           ,
           usuario            LIKE ret_cta_vol.usuario          ,
           estado             LIKE ret_cta_vol.estado
           END RECORD

    DEFINE reg_20             RECORD
           estado_marca       SMALLINT ,
           codigo_rechazo     SMALLINT ,
           marca_causa        SMALLINT ,
           fecha_causa        DATE
       END RECORD

    DEFINE HOY                 DATE

    DEFINE aux_pausa           CHAR(01)
    DEFINE c8_usuario          CHAR(08)
    DEFINE v_desmarca          CHAR(100)
    DEFINE v_desmarca2         CHAR(100)
    DEFINE v_marca             CHAR(100)

    DEFINE s_codigo_afore      SMALLINT
    DEFINE v_cod_rechazo       SMALLINT
    DEFINE v_marca_res         SMALLINT
    DEFINE i                   SMALLINT

    DEFINE g_ultimo_folio      INTEGER
    DEFINE i_tipo_movimiento   INTEGER

    DEFINE aux_monto_en_pesos  DECIMAL(16,6)

    DEFINE monto_en_acciones   DECIMAL(16,6)
    DEFINE precio_dia_sb1      DECIMAL(11,6)
    DEFINE precio_dia_s6       DECIMAL(11,6)
    DEFINE v_ejecuta           CHAR(120)

    DEFINE g_usuario           CHAR(8)
    DEFINE gc_mensaje          CHAR(200)
    DEFINE g_marca_ent         SMALLINT
    DEFINE enter               CHAR(01)
    
DEFINE mc_comando    CHAR(5000)

END GLOBALS

#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN

   DEFER INTERRUPT

   OPTIONS
       INPUT WRAP       ,
       PROMPT LINE LAST ,
       MESSAGE LINE LAST


   CALL init() #Inicializa variables globales

   CALL STARTLOG(g_usuario CLIPPED||".RETM011.log")

   OPEN WINDOW RETM0111 AT 2,2 WITH FORM "RETM0111" ATTRIBUTE( BORDER)
   DISPLAY " RETM011            DATOS DEL REGISTRO DE AFILIACION                               " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "               MANTENIMIENTO RETIRO AP COMPLEMENTARIAS Y VOL                       " AT 8,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

   CALL inicializa()

   MENU "AP COMPL Y VOL v1.2"
      COMMAND "Agrega"
              "AGREGA SOLICITUD DE RETIRO AP COMPLEMENTARIAS Y VOL "
              CALL f_agrega()
              CALL inicializa()

      COMMAND "Consulta"
              "CONSULTA SOLICITUDES DE RETIRO DE AP COMPLEMENTARIAS Y VOL"
              CALL f_consulta(1)

      COMMAND "Modifica"
              "MODIFICA SOLICITUDES DE RETIRO DE AP COMPLEMENTARIAS Y VOL "
              CALL f_modifica()

      COMMAND "Elimina"
              "ELIMINA SOLICITUDES DE RETIRO DE AP COMPLEMENTARIAS Y VOL"
              CALL f_elimina()

      COMMAND "Rechaza"
              "RECHAZA SOLICITUDES DE RETIRO DE AP COMPLEMENTARIAS Y VOL"
              CALL f_consulta( 2 )

      COMMAND "Liquida" "LIQUIDA AP COMPLEMENTARIAS Y VOL "
              CALL f_liquida()

      COMMAND KEY("F") --"Folios liquidados" "MUESTRA UNA LISTA DE FOLIOS LIQUIDADOS"
               CALL f_lista_folios_liquidados()

      COMMAND "Reversa Liquidacion" "REVERSA UN FOLIO LIQUIDADO"
               CALL f_reversa_liquidacion()

      COMMAND "Salir" "SALIR DEL PROGRAMA"
              EXIT MENU

   END MENU

END MAIN

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION init()
   DEFINE lc_prepare   CHAR(300)

   LET HOY = TODAY
   

   SELECT codigo_afore   ,
          USER
   INTO   s_codigo_afore ,
          g_usuario
   FROM   tab_afore_local

   LET gr_estados.capturado    = 0
   LET gr_estados.provisionado = 7
   LET gr_estados.liquidado    = 8
   LET gr_estados.rechazado    = 20
   LET gr_estados.confirmado   = 3 

   LET v_marca = "EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
   PREPARE eje_marca FROM v_marca
   
   ----- INSERTA MAESTRO DE AFILIADOS ------
   LET lc_prepare = " EXECUTE FUNCTION fn_control_beneficiarios(?,?,?,?,?,?,?) "
   PREPARE eje_maestro_benef FROM lc_prepare
   
   LET v_cod_rechazo     = 0

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION inicializa()

    INITIALIZE reg_1.* TO NULL
    CLEAR FORM

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_agrega()

   DEFINE lr_saldos  RECORD
          acc_sbf_sb1       DECIMAL(16,6),
          acc_sbf_s6        DECIMAL(16,6),
          pes_sbf           DECIMAL(16,6),
          acc_cbf_sb1       DECIMAL(16,6),
          acc_cbf_s6        DECIMAL(16,6),
          pes_cbf           DECIMAL(16,6),
          pes               DECIMAL(16,6)
      END RECORD

   DEFINE ld_fec_aux          DATE
   DEFINE ld_fec_aux2         DATE
   DEFINE ld_fecha_compara    DATE
   DEFINE ld_vmax_fec_banxico DATE
   DEFINE ld_suma_acciones    DECIMAL(18,6)
   DEFINE ls_cont             SMALLINT
   DEFINE ls_evalua_fecha     SMALLINT
   DEFINE lc_comando          CHAR(300)
   DEFINE lc_condicion        CHAR(500)

   -----------------------------------------------------------------------------

   LET INT_FLAG = FALSE

   WHILE TRUE
      CLEAR FORM
      DISPLAY "" AT 2,1
      DISPLAY " <Ctrl-C> Salir                           ",
              "                                          "   AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " AGREGA " AT 1,71 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY " <Esc> Buscar Afiliado          " AT 2, 1

      INITIALIZE ls_cont    TO NULL
      INITIALIZE lc_comando TO NULL

      #CONSTRUYE EL CRITERIO DE BUSQUEDA EN LA TABLA
      CONSTRUCT BY NAME lc_condicion ON afi_mae_afiliado.n_folio ,
                                        afi_mae_afiliado.n_seguro,
                                        afi_mae_afiliado.n_rfc   ,
                                        afi_mae_afiliado.n_unico

      #CAMBIA LA BANDERA PARA EXIT DEL WHILE
      IF INT_FLAG != 0 THEN
         LET INT_FLAG = FALSE
         ERROR "OPERACION CANCELADA!"
         EXIT WHILE
      END IF

      #ARMA QUERY DONDE CUENTA LOS REGISTROS CON EL CRITERIO ANTERIOR
      WHENEVER ERROR CALL f_errores

      LET lc_comando = " SELECT COUNT (*)        ",
                       " FROM   afi_mae_afiliado ",
                       " WHERE                   ",lc_condicion CLIPPED

      PREPARE exe_contador FROM lc_comando
      EXECUTE exe_contador INTO ls_cont

      WHENEVER ERROR STOP

      #SE VALIDA EL CONTADOR DE REGISTROS RETORNADOS POR LA INSTRUCCION PREPARA
      IF ls_cont != 1 THEN
         ERROR " NO HAY CRITERIOS PARA BUSCAR AL AFILIADO O REGRESA MAS DE UN SOLO REGISTRO "
         CONTINUE WHILE
      END IF

      #SE PREPARA LA INSTRUCCION QUE DEVOLVERA LOS DATOS DEL TRABAJADOR FILTRADO
      WHENEVER ERROR CALL f_errores

      LET lc_comando = " SELECT n_folio         ,",
                       "        n_seguro        ,",
                       "        n_rfc           ,",
                       "        n_unico         ,",
                       "        fentcons        ,",
                       "        paterno         ,",
                       "        materno         ,",
                       "        nombres          ",
                       " FROM   afi_mae_afiliado ",
                       " WHERE                   ",lc_condicion CLIPPED

      PREPARE exe_qry FROM lc_comando
      EXECUTE exe_qry INTO reg_1.n_folio ,
                           reg_1.n_seguro,
                           reg_1.n_rfc   ,
                           reg_1.n_unico ,
                           reg_1.fentcons,
                           reg_1.paterno ,
                           reg_1.materno ,
                           reg_1.nombres

      WHENEVER ERROR STOP

      IF SQLCA.SQLCODE < 0 THEN
         PROMPT "SE PRESENTO UN ERROR AL RECUPERAR SOLICITUDES, PRESIONE <ENTER> " FOR CHAR enter
      END IF

      #PINTA LOS DATOS DEL NSS FILTRADO Y CONTROL DE GUARDADO
      DISPLAY BY NAME reg_1.n_folio, reg_1.n_seguro, reg_1.n_rfc, reg_1.n_unico,
                      reg_1.fentcons,reg_1.paterno, reg_1.materno, reg_1.nombres

      INITIALIZE gr_general.* TO NULL

      DISPLAY gr_estados.capturado TO estado
      DISPLAY "CAPTURADO"          TO desc_estado

      LET gr_general.fecha_solic = HOY
      LET ls_cont                = 0

      #BUSCA SOLICITUD PREVIA
      SELECT COUNT(*)
      INTO   ls_cont
      FROM   ret_cta_vol   A
      WHERE  A.n_seguro    = reg_1.n_seguro
      AND    A.estado      = gr_estados.capturado
      AND    A.tipo_ret    IN(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) #MLM-3150 ##CPL-3203 (Agregar 13 y 14 )

      #SI CUENTA CON SOLICITUDES CAPTURADAS REGRESA A CAPTURA
      IF ls_cont > 0 THEN
         PROMPT " TRABAJADOR YA CUENTA CON UNA SOLICITUD CAPTURADA, PRESIONE <ENTER> " FOR CHAR enter
         RETURN
         CONTINUE WHILE
      END IF

      #VERIFICA EL SALDO (CON/SIN BENEFICIO FISCAL)
      WHENEVER ERROR CALL f_errores

      SELECT SUM(NVL(A.monto_en_acciones,0))
      INTO   ld_suma_acciones
      FROM   dis_cuenta A
      WHERE  A.nss = reg_1.n_seguro
      AND    A.subcuenta IN(3,10,11,12,15,16,20,21,22,23,24,25,26,27,28,29) --Voluntarias CBF y complementarias #MLM-3150 ##CPL-3203 (15 y 16)

      WHENEVER ERROR STOP

      IF SQLCA.SQLCODE < 0 THEN
         ERROR "ERROR EN CONSULTA DE SALDOS NOTIFIQUE A SISTEMAS"
         EXIT PROGRAM
      ELSE
         IF ld_suma_acciones = 0 THEN
            PROMPT " TRABAJADOR CON SALDO CERO EN APORTACIONES, PRESIONE <ENTER> " FOR CHAR enter
            CONTINUE WHILE 
         END IF
      END IF


      --OBTIENE UN NUEVO CONSECUTIVO PARA LA SOLICITUD
      SELECT MAX(A.consecutivo) + 1
      INTO   gr_general.consecutivo
      FROM   ret_consecutivo A

      IF gr_general.consecutivo IS NULL THEN
         LET gr_general.consecutivo = 1
      END IF

      INSERT INTO ret_consecutivo VALUES (gr_general.consecutivo)

      DISPLAY BY NAME gr_general.consecutivo

      ----------------------------------------------
      --CAPTURA LOS DATOS GENERALES DELA SOLICITUD
      ----------------------------------------------
      INPUT BY NAME gr_general.tipo_ret         ,
                    gr_general.n_folio_sol      ,
                    gr_general.mto_solic        ,
                    gr_general.porcentaje_solic ,
                    gr_general.fecha_solic      ,
                    gr_general.pension_invalidez,
                    gr_general.deduccion        WITHOUT DEFAULTS

         --------------
         BEFORE INPUT
         --------------
            LET gr_general.fecha_captura  = HOY
            LET gr_general.usuario        = c8_usuario
            LET gr_general.ultimo_proceso = HOY
            LET gr_general.fecha_solic    = HOY

            #OBTIENE LA FECHA EDAD DEL TRABAJADOR
            LET gr_general.edad = f_obten_edad(reg_1.n_seguro)

            DISPLAY BY NAME gr_general.fecha_ult_ret
            DISPLAY BY NAME gr_general.fecha_captura
            DISPLAY BY NAME gr_general.usuario
            DISPLAY BY NAME gr_general.ultimo_proceso
            DISPLAY BY NAME gr_general.edad
            DISPLAY BY NAME gr_general.fecha_solic

            DISPLAY " <ESC> Guardar registro         " AT 2, 1

         ---------------------
         AFTER FIELD tipo_ret
         ---------------------
            IF gr_general.tipo_ret IS NULL THEN
               #SELECCIONA EL TIPO DE PAGO
               CALL f_selecciona_tipo_ret()
                   RETURNING gr_general.tipo_ret, gr_general.des_tipo_ret

               IF gr_general.tipo_ret IS NULL THEN
                  NEXT FIELD  tipo_ret
               END IF
            END IF

            --RECUPERA LA DESCRIPCION DEL TIPO DE PAGO CAPTURADO
            SELECT des_tipo_ret INTO gr_general.des_tipo_ret
            FROM   tab_retiro_old
            WHERE  tipo_ret = gr_general.tipo_ret
            AND    tipo_ret IN (1,2,3,4,6,7,8,9,10,11,12,13,14,15)  --se excluye 5   #MLM-3150 MLM-2929 MLM-3420 ##CPL-3203(13 y 14)

            IF STATUS = NOTFOUND THEN
               ERROR " TIPO DE RETIRO INVALIDO "
               NEXT FIELD tipo_ret
            ELSE
               DISPLAY BY NAME gr_general.tipo_ret
               DISPLAY BY NAME gr_general.des_tipo_ret
            END IF


            --VALIDA ANTIGUEDAD DEL ULTIMO RETIRO CON BASE A LA SIEFORE DE SU REGIMEN
            MESSAGE "VALIDANDO ULTIMO RETIRO, POR FAVOR ESPERE..."

            IF f_valida_ult_retiro(reg_1.n_seguro,gr_general.tipo_ret) = FALSE THEN
               #RETURN
               EXIT WHILE
            END IF

            MESSAGE ""

            --VALIDA ANTIGUEDAD DEL PRIMER APORTE CON BASE A LA SIEFORE DE SU REGIMEN
            MESSAGE "VALIDANDO PRIMER APORTE, POR FAVOR ESPERE..."
            IF f_valida_primer_aporte(reg_1.n_seguro,gr_general.tipo_ret) = FALSE THEN
               #RETURN
               EXIT WHILE
            END IF

            MESSAGE ""

            --RECUPERA Y DESPLIEGA EL SALDOS DE ACUERDO AL TIPO DE RETIRO
            CALL f_obten_saldos_comp2(reg_1.n_seguro,gr_general.tipo_ret) RETURNING lr_saldos.*

            DISPLAY BY NAME lr_saldos.*

            IF f_valida_cuadre(reg_1.n_seguro, gr_general.tipo_ret) = FALSE THEN
               #LET INT_FLAG = TRUE
               #EXIT INPUT
               #RETURN
               EXIT WHILE
            END IF

            --DESPLIEGA EL MONTO QUE CORRESPONDA AL TIPO DE RETIRO ##TOTALES
            IF gr_general.tipo_ret = 2   OR       --TOTAL VOL SBF #MLM-3150
               gr_general.tipo_ret = 4   OR       --TOTAL VOL CBF
               gr_general.tipo_ret = 6   OR       --TOTAL VOL CBF MIX #MLM-3420
               gr_general.tipo_ret = 8   OR       --TOTAL COM SBF
               gr_general.tipo_ret = 10  OR       --TOTAL COM CBF
               gr_general.tipo_ret = 12  OR       --TOTAL COM MIX
               gr_general.tipo_ret = 14 THEN      --TOTAL LARGO PLAZO ##CPL-3203
               --RETIRO TOTAL
               LET gr_general.porcentaje_solic = 100

               CASE gr_general.tipo_ret
                    WHEN 2  LET gr_general.mto_solic = lr_saldos.pes_sbf #MLM-3150
                    WHEN 4  LET gr_general.mto_solic = lr_saldos.pes_cbf
                    WHEN 6  LET gr_general.mto_solic = lr_saldos.pes_cbf + lr_saldos.pes_sbf   #MLM-3420
                    WHEN 8  LET gr_general.mto_solic = lr_saldos.pes_sbf
                    WHEN 10 LET gr_general.mto_solic = lr_saldos.pes_cbf
                    WHEN 12 LET gr_general.mto_solic = lr_saldos.pes
                    WHEN 14 LET gr_general.mto_solic = lr_saldos.pes_sbf ##CPL-3203
               END CASE

               DISPLAY BY NAME gr_general.mto_solic        ATTRIBUTE(BOLD)
               DISPLAY BY NAME gr_general.porcentaje_solic ATTRIBUTE(BOLD)
            ELSE
               #RETIRO PARCIAL
               --LET gr_general.porcentaje_solic = ""
               --LET gr_general.mto_solic = 0

               DISPLAY gr_general.mto_solic        TO mto_solic ATTRIBUTE(BOLD)
               DISPLAY gr_general.porcentaje_solic TO porcentaje_solic
            END IF

            #VALIDA SALDOS CON BENEFICIO FISCAL
            IF lr_saldos.pes = 0 THEN
               PROMPT "EL NSS ", reg_1.n_seguro, " NO CUENTA CON SALDO PARA REALIZAR EL TRAMITE. " ATTRIBUTE(REVERSE) FOR CHAR enter
               #RETURN
               #EXIT PROGRAM
               EXIT WHILE
            END IF

            --VALIDA SALDOS SIN BENEFICIO FISCAL
            IF(gr_general.tipo_ret  = 1   OR                         #MLM-3150
               gr_general.tipo_ret  = 2   OR                         #MLM-3150
               gr_general.tipo_ret  = 7   OR
               gr_general.tipo_ret  = 8   OR
               gr_general.tipo_ret  = 13  OR                         #CPL-3203(13 Y 14)
               gr_general.tipo_ret  = 14 ) AND lr_saldos.pes_sbf    = 0   THEN
               PROMPT "EL NSS ", reg_1.n_seguro, " NO CUENTA CON SALDO PARA REALIZAR EL TRAMITE.. " ATTRIBUTE(REVERSE) FOR CHAR enter
               #RETURN
               EXIT WHILE
            END IF

            IF (gr_general.tipo_ret = 6 OR gr_general.tipo_ret = 15)    AND               #MLM-3420
               lr_saldos.pes_cbf + lr_saldos.pes_sbf = 0 THEN
               PROMPT "EL NSS ", reg_1.n_seguro, " NO CUENTA CON SALDO PARA REALIZAR EL TRAMITE.. " ATTRIBUTE(REVERSE) FOR CHAR enter
               #RETURN
               EXIT WHILE
            END IF

            --VALIDA SALDOS CON BENEFICIO FISCAL
            IF(gr_general.tipo_ret = 3    OR
               gr_general.tipo_ret = 4    OR
               gr_general.tipo_ret = 9    OR
               gr_general.tipo_ret = 10 ) AND lr_saldos.pes_cbf   = 0    THEN
               PROMPT "EL NSS ", reg_1.n_seguro, " NO CUENTA CON SALDO PARA REALIZAR EL TRAMITE... " ATTRIBUTE(REVERSE) FOR CHAR enter
               #RETURN
               EXIT WHILE
            END IF

         -------------------------
         AFTER FIELD n_folio_sol
         -------------------------
            IF gr_general.n_folio_sol IS NULL THEN
               ERROR " EL CAMPO FOLIO NO PUEDE SER NULO "
               NEXT FIELD n_folio_sol
            ELSE
               #VALIDA LA EXISTENCIA DEL FOLIO CAPTURADO
               SELECT "OK"
               FROM   ret_cta_vol
               WHERE  n_folio_sol = gr_general.n_folio_sol
               GROUP BY 1

               IF STATUS <> NOTFOUND THEN
                  ERROR " FOLIO YA HA SIDO INGRESADO "
                  NEXT FIELD n_folio_sol
               END IF
            END IF

         -----------------------
         BEFORE FIELD mto_solic
         -----------------------
            #SI ES RETIRO TOTAL NO SE PERMITE LA CAPTURA DEL MONTO o %  (TOTALES)
            IF gr_general.tipo_ret = 2   OR    #MLM-3150
               gr_general.tipo_ret = 4   OR
               gr_general.tipo_ret = 6   OR    #MLM-3420
               gr_general.tipo_ret = 8   OR
               gr_general.tipo_ret = 10  OR
               gr_general.tipo_ret = 12  OR
               gr_general.tipo_ret = 14 THEN   ##CPL-3203
               #RETIRO TOTAL
               NEXT FIELD fecha_solic
            END IF

         ----------------------
         AFTER FIELD mto_solic
         ----------------------
            IF gr_general.mto_solic IS NOT NULL THEN
               #VERIFICA QUE NO SEA MAYOR AL SALDO SIN BENEFICIO FISCAL
               IF (gr_general.tipo_ret = 7 OR gr_general.tipo_ret = 8 OR
                   gr_general.tipo_ret = 1 OR gr_general.tipo_ret = 2 OR         #MLM-3150   
                   gr_general.tipo_ret = 13 OR gr_general.tipo_ret = 14) AND     ##CPL-3203
                   gr_general.mto_solic > lr_saldos.pes_sbf THEN
                   ERROR " MONTO INVALIDO, NO PUEDE SER MAYOR AL SALDO. "
                   NEXT FIELD mto_solic
               END IF

               IF (gr_general.tipo_ret = 6 OR gr_general.tipo_ret = 15 ) AND       #MLM-3420
                   gr_general.mto_solic > (lr_saldos.pes_sbf + lr_saldos.pes_cbf)  THEN
                   ERROR " MONTO INVALIDO, NO PUEDE SER MAYOR AL SALDO. "
                   NEXT FIELD mto_solic
               END IF

               #VERIFICA QUE NO SEA MAYOR AL SALDO CON BENEFICIO FISCAL
               IF (gr_general.tipo_ret = 9 OR gr_general.tipo_ret = 10 OR
                   gr_general.tipo_ret = 3 OR gr_general.tipo_ret = 4) AND
                   gr_general.mto_solic > lr_saldos.pes_cbf THEN
                   ERROR " MONTO INVALIDO, NO PUEDE SER MAYOR AL SALDO.. "
                   NEXT FIELD mto_solic
               END IF

               IF gr_general.mto_solic > lr_saldos.pes THEN
                  ERROR " MONTO INVALIDO, NO PUEDE SER MAYOR AL SALDO... "
                  NEXT FIELD mto_solic
               END IF

               #VERIFICA QUE NO SEA UN MONTO INVALIDO
               IF gr_general.mto_solic <= 0 THEN
                  ERROR " MONTO INVALIDO "
                  NEXT FIELD mto_solic
               END IF

               #CALCULA PORCENTAJE CON BASE AL MONTO SOLICITADO SEGUN T. RET
               CASE
                  WHEN (gr_general.tipo_ret = 7  OR gr_general.tipo_ret = 8 OR  #SbF
                        gr_general.tipo_ret = 1  OR gr_general.tipo_ret = 2)    #MLM-3150
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)
                                                       / lr_saldos.pes_sbf )
                  WHEN (gr_general.tipo_ret = 6)
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)     #MLM-3420
                                                       / (lr_saldos.pes_sbf + lr_saldos.pes_cbf) )
                  WHEN (gr_general.tipo_ret = 9  OR gr_general.tipo_ret = 10 OR #CBF
                        gr_general.tipo_ret = 3  OR gr_general.tipo_ret = 4)
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)
                                                       / lr_saldos.pes_cbf )
                  WHEN (gr_general.tipo_ret = 11 OR gr_general.tipo_ret = 12)   #MIXTO
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)
                                                       / lr_saldos.pes )    
                  WHEN (gr_general.tipo_ret = 13 OR gr_general.tipo_ret = 14)   #LARGO PLAZO SBF  ##CPL-3203 
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)/ lr_saldos.pes )                  
               END CASE

               DISPLAY BY NAME gr_general.mto_solic
               DISPLAY BY NAME gr_general.porcentaje_solic

               NEXT FIELD fecha_solic

            END IF

         ------------------------------
         BEFORE FIELD porcentaje_solic
         ------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               #SI ES RETIRO TOTAL NO SE PERMITE LA CAPTURA DEL MONTO o %
               IF gr_general.tipo_ret = 2   OR    #MLM-3150
                  gr_general.tipo_ret = 4   OR
                  gr_general.tipo_ret = 6   OR    #MLM-3420
                  gr_general.tipo_ret = 8   OR
                  gr_general.tipo_ret = 10  OR
                  gr_general.tipo_ret = 12  OR
                  gr_general.tipo_ret = 14 THEN   ##CPL-3203
                   #RETIRO TOTAL
                   NEXT FIELD n_folio_sol
               END IF
            END IF

         -----------------------------
         AFTER FIELD porcentaje_solic
         -----------------------------
            IF gr_general.mto_solic        IS NULL AND
               gr_general.porcentaje_solic IS NULL THEN
                ERROR " DEBE INGRESAR MONTO O PORCENTAJE "
                NEXT FIELD  mto_solic
            END IF

            IF gr_general.porcentaje_solic <= 0 OR
               gr_general.porcentaje_solic >100 THEN
                ERROR " PORCENTAJE INVALIDO "
                NEXT FIELD  mto_solic
            END IF

            #CALCULA MONTO CON BASE AL PORCENTAJE SEGUN T. RET
            CASE
               WHEN (gr_general.tipo_ret = 1 OR gr_general.tipo_ret = 2 OR      #MLM-3150
                     gr_general.tipo_ret = 7 OR gr_general.tipo_ret = 8)
                    LET gr_general.mto_solic = ( lr_saldos.pes_sbf *
                                   ( gr_general.porcentaje_solic / 100 ) )

               WHEN (gr_general.tipo_ret = 3 OR gr_general.tipo_ret = 4 OR
                     gr_general.tipo_ret = 9 OR gr_general.tipo_ret = 10)
                    LET gr_general.mto_solic = ( lr_saldos.pes_cbf *
                                   ( gr_general.porcentaje_solic / 100 ) )

               WHEN (gr_general.tipo_ret = 11 OR gr_general.tipo_ret = 12)
                    LET gr_general.mto_solic = ( lr_saldos.pes *
                                   ( gr_general.porcentaje_solic / 100 ) )
               WHEN (gr_general.tipo_ret = 13 OR gr_general.tipo_ret = 14) ##CPL-3203
                    LET gr_general.mto_solic = ( lr_saldos.pes_sbf *( gr_general.porcentaje_solic / 100 ) )                    
            END CASE

            DISPLAY BY NAME gr_general.mto_solic

         -------------------------
         AFTER FIELD fecha_solic
         -------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gr_general.fecha_solic IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD fecha_solic
            END IF

            IF gr_general.fecha_solic > HOY THEN
               ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
               NEXT FIELD fecha_solic
            END IF
         ------------------------------
         AFTER FIELD pension_invalidez
         ------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               NEXT FIELD fecha_solic
            END IF

            IF gr_general.pension_invalidez IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD pension_invalidez
            ELSE
               IF gr_general.pension_invalidez NOT MATCHES "[SsNn]" THEN
                  ERROR " CAMPO DEBE SER S o N "
                  NEXT FIELD pension_invalidez
               END IF
            END IF

            #SOLICITA CONFIRMACION
            IF gr_general.pension_invalidez MATCHES"[Ss]" THEN
               PROMPT "CONFIRMA QUE NO SE APLICARA RETENCION: (S/N) " FOR CHAR enter
               IF enter NOT MATCHES "[Ss]" THEN
                  NEXT FIELD pension_invalidez
               END IF
            END IF

        ------------------------
        AFTER FIELD deduccion
        ------------------------
                    IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                        NEXT FIELD pension_invalidez
                    END IF

                    IF gr_general.deduccion IS NULL THEN
                        ERROR ""
                        ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE( NORMAL )
                        NEXT FIELD deduccion
                    ELSE
                        CASE gr_general.deduccion
                            WHEN  "S"
                            WHEN  "N"
                        OTHERWISE
                            ERROR ""
                            ERROR "   CAMPO DEBE SER S o N " ATTRIBUTE( NORMAL )
                            NEXT FIELD deduccion
                        END CASE


                            NEXT FIELD tipo_ret

                    END IF

         ------------
         AFTER INPUT
         ------------
            IF gr_general.n_folio_sol IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD n_folio_sol
            END IF

            IF gr_general.tipo_ret IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD tipo_ret
            END IF

            IF gr_general.edad IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD edad
            END IF

            IF gr_general.fecha_solic IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD fecha_solic
            END IF

            IF gr_general.fecha_solic > HOY THEN
               ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
               NEXT FIELD fecha_solic
            END IF

            IF gr_general.pension_invalidez IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD pension_invalidez
            ELSE
               CASE gr_general.pension_invalidez
                  WHEN  "S"

                  WHEN  "N"

                  OTHERWISE
                     ERROR " CAMPO DEBE SER S o N "
                     NEXT FIELD pension_invalidez
               END CASE
            END IF

            #INSERTA SOLICITUD DE RETIRO DE COMPLEMENTARIAS
            INSERT INTO ret_cta_vol
               VALUES(reg_1.n_folio               ,
                      gr_general.n_folio_sol      ,
                      "S"                         ,
                      ""                          , #n_folio_liq
                      reg_1.n_seguro              ,
                      reg_1.n_unico               ,
                      reg_1.n_rfc                 ,
                      reg_1.paterno               ,
                      reg_1.materno               ,
                      reg_1.nombres               ,
                      gr_general.edad             ,
                      gr_general.tipo_ret         ,
                      gr_general.deduccion        , #gr_general.deduccion,
                      gr_general.pension_invalidez,
                      0                           , #mto_en_acc_3
                      0                           , #mto_en_acc_10
                      NULL                        , #gr_general.mto_deducido,
                      gr_general.mto_solic        ,
                      gr_general.porcentaje_solic ,
                      NULL                        , #gr_general.tipo_pago,
                      ""                          , #tipo_identif
                      ""                          ,
                      gr_general.fecha_solic      ,
                      gr_general.fecha_captura    ,
                      gr_general.fecha_ult_ret    ,
                      NULL                        , #gr_general.fecha_deduccion,
                      gr_general.ultimo_proceso   ,
                      ""                          , #diagnostico_pago
                      gr_estados.capturado        , #estado
                      c8_usuario                  , #usuario
                      gr_general.consecutivo      ,
                      ""                          ,
                      ""                          ,
                      ""
                      )

            IF SQLCA.SQLCODE < 0 THEN
               LET gc_mensaje = " ERROR AL INSERTAR SOLICITUD NOTIFIQUE A SISTEMAS "
               ERROR gc_mensaje
               LET gc_mensaje = gc_mensaje CLIPPED," NSS:",reg_1.n_seguro," ",
                               ERR_GET(SQLCA.SQLCODE)
               CALL ERRORLOG(gc_mensaje)
               NEXT FIELD NEXT
            END IF

            #INSERTA INFORMACION COMPLEMENTARIA A LA SOLICITUD
            INSERT INTO ret_cta_vol_comp
               VALUES (reg_1.n_seguro , gr_general.consecutivo )

            IF SQLCA.SQLCODE < 0 THEN
               LET gc_mensaje = " ERROR AL INSERTAR COMPLEMENTO NOTIFIQUE A SISTEMAS "
               ERROR gc_mensaje
               LET gc_mensaje = gc_mensaje CLIPPED," NSS:",reg_1.n_seguro," ",
                               ERR_GET(SQLCA.SQLCODE)
               CALL ERRORLOG(gc_mensaje)
               NEXT FIELD NEXT
            END IF

            #VERIFICA LA CAPTURA DE BENEFICIARIOS
            SQL
               SELECT FIRST 1 1 FROM ret_beneficiario
               WHERE  nss         = $reg_1.n_seguro
               AND    consecutivo = $gr_general.consecutivo
            END SQL

            #EJECUTA EL RETM810 PARA CAPTURAR BENEFICIARIOS
            IF STATUS = NOTFOUND THEN
               LET v_ejecuta = "fglgo RETM810 ", reg_1.n_seguro CLIPPED,
                               " ",gr_general.consecutivo CLIPPED,
                               " ", 'A', gr_general.n_folio_sol
               RUN v_ejecuta
            ELSE
               ERROR " YA SE HAN CAPTURADO BENEFICIARIOS  "
            END IF

            SQL
               SELECT FIRST 1 1
               FROM ret_beneficiario
               WHERE  nss         = $reg_1.n_seguro
               AND    consecutivo = $gr_general.consecutivo
            END SQL

            IF STATUS = NOTFOUND THEN
               ERROR " NO SE PUEDE CAPTURAR LA SOLICITUD SIN BENEFICIARIOS "

               #ELIMINA INFORMACION DE COMPLEMENTARIA
               DELETE FROM ret_cta_vol_comp
               WHERE  n_seguro    = reg_1.n_seguro
               AND    consecutivo = gr_general.consecutivo

               #ELIMINA SOLICITUD DE COMPLEMENTARIA
               DELETE FROM ret_cta_vol
               WHERE  n_seguro    = reg_1.n_seguro
               AND    consecutivo = gr_general.consecutivo

               NEXT FIELD n_folio_sol
            END IF

            #Hasta aqui Nueva Captura de Beneficiarios
          --  IF gr_general.tipo_ret  <= 6 THEN
          --     LET i_tipo_movimiento = 490
          --  ELSE
          --     LET i_tipo_movimiento = 897
          --  END IF

            --LET i_tipo_movimiento     = 897
            LET reg_20.estado_marca   = 0
            LET reg_20.codigo_rechazo = 0
            LET reg_20.marca_causa    = 0
            LET reg_20.fecha_causa    = NULL

            IF gr_general.tipo_ret = 1  OR
               gr_general.tipo_ret = 2  OR
               gr_general.tipo_ret = 3  OR
               gr_general.tipo_ret = 4  OR
               gr_general.tipo_ret = 6  OR     #MLM-3420
               gr_general.tipo_ret = 13 OR     #MLM-3150 
               gr_general.tipo_ret = 14 THEN   ##CPL-3203 (13 y 14)
               LET i_tipo_movimiento = 490
            ELSE
               LET i_tipo_movimiento = 897
            END IF

            #MARCA LA CUENTA
            DECLARE cur_sp CURSOR FOR eje_marca
            OPEN cur_sp USING reg_1.n_seguro        , # nss
                              i_tipo_movimiento     , # marca entrante
                              gr_general.consecutivo, # correlativo
                              reg_20.estado_marca   ,
                              reg_20.codigo_rechazo ,
                              reg_20.marca_causa    ,
                              reg_20.fecha_causa    ,
                              g_usuario

            FETCH cur_sp INTO v_marca_res,       # misma marca si convive o
                              v_cod_rechazo      # marca_activa que rechaza
                                                 # codigo de rechazo

            ERROR " SOLICITUD INSERTADA "

            CLOSE cur_sp

            INITIALIZE gr_general.* TO NULL
            INITIALIZE reg_1.* TO NULL
            EXIT INPUT

         -------------------
         ON KEY (INTERRUPT)
         -------------------
            DELETE FROM  ret_consecutivo
            WHERE  consecutivo = gr_general.consecutivo

            ERROR " OPERACION CANCELADA. "

            INITIALIZE gr_general.* TO NULL
            EXIT INPUT
      END INPUT

      IF INT_FLAG THEN
         LET INT_FLAG = FALSE
         ERROR "OPERACION CANCELADA!!"
      ELSE
         ERROR " REGISTRO GUARDADO EXITOSAMENTE. "
         INITIALIZE gr_general.* TO NULL
         CLEAR FORM
         EXIT WHILE
      END IF

   END WHILE

END FUNCTION
#------------------------------------------------------------------------------#
#  SE AGREGA VALIDACION DE ULTIMO RETIRO                                       #
#------------------------------------------------------------------------------#
FUNCTION f_valida_ult_retiro(pc_nss, ps_tipo_ret)

   DEFINE pc_nss                  CHAR(11)
   DEFINE ps_tipo_ret             SMALLINT   --tipo de retiro seleccionado

   DEFINE ld_fecha_cta_saldo_vol  DATE    ,
          ld_fecha_ret_cta_vol    DATE    ,
          ls_siefore              SMALLINT,
          ls_dif_mes_vol          SMALLINT,
          ls_dif_mes_dis          SMALLINT

   DEFINE ls_nss_cod_sief   SMALLINT
   DEFINE ld_fecha_ul_vol   DATE
   DEFINE ld_fecha_aux      DATE
   DEFINE ld_fecha_primera_vol      DATE

   LET ls_siefore              = 0
   LET ld_fecha_cta_saldo_vol  = NULL
   LET ld_fecha_ret_cta_vol    = NULL


   WHENEVER ERROR CONTINUE

      CASE
          WHEN (gr_general.tipo_ret = 1 OR gr_general.tipo_ret = 2)                                  #MLM-3150
               --SE OBTIENE LA SIEFORE DEL NSS DE ACUERDO AL REGIMEN y AL TIPO DE RETIRO SELECCIONADO
               SELECT UNIQUE codigo_siefore
               INTO ls_nss_cod_sief
               FROM cta_regimen
               WHERE nss = pc_nss
               AND subcuenta IN (3,10)

          WHEN (gr_general.tipo_ret = 3 OR gr_general.tipo_ret = 4)
               --SE OBTIENE LA SIEFORE DEL NSS DE ACUERDO AL REGIMEN y AL TIPO DE RETIRO SELECCIONADO
               SELECT UNIQUE codigo_siefore
               INTO ls_nss_cod_sief
               FROM cta_regimen
               WHERE nss = pc_nss
               AND subcuenta IN (22,23)
          WHEN (gr_general.tipo_ret = 6)      #MLM-3420
               --SE OBTIENE LA SIEFORE DEL NSS DE ACUERDO AL REGIMEN y AL TIPO DE RETIRO SELECCIONADO
               SELECT UNIQUE codigo_siefore
               INTO ls_nss_cod_sief
               FROM cta_regimen
               WHERE nss = pc_nss
               AND subcuenta IN (3,10,22,23)

          WHEN (gr_general.tipo_ret = 7 OR gr_general.tipo_ret = 8)
               --SE OBTIENE LA SIEFORE DEL NSS DE ACUERDO AL REGIMEN y AL TIPO DE RETIRO SELECCIONADO
               SELECT UNIQUE codigo_siefore
               INTO ls_nss_cod_sief
               FROM cta_regimen
               WHERE nss = pc_nss
               AND subcuenta IN (11,12)

          WHEN (gr_general.tipo_ret = 9 OR gr_general.tipo_ret = 10)
               --SE OBTIENE LA SIEFORE DEL NSS DE ACUERDO AL REGIMEN y AL TIPO DE RETIRO SELECCIONADO
               SELECT UNIQUE codigo_siefore
               INTO ls_nss_cod_sief
               FROM cta_regimen
               WHERE nss = pc_nss
               AND subcuenta IN (24,25)

          WHEN (gr_general.tipo_ret = 11 OR gr_general.tipo_ret = 12)
               --SE OBTIENE LA SIEFORE DEL NSS DE ACUERDO AL REGIMEN y AL TIPO DE RETIRO SELECCIONADO
               SELECT UNIQUE codigo_siefore
               INTO ls_nss_cod_sief
               FROM cta_regimen
               WHERE nss = pc_nss
               AND subcuenta IN (11,12,24,25)   
          WHEN (gr_general.tipo_ret = 13 OR gr_general.tipo_ret = 14) ##CPL-3203
               --SE OBTIENE LA SIEFORE DEL NSS DE ACUERDO AL REGIMEN y AL TIPO DE RETIRO SELECCIONADO
               SELECT UNIQUE codigo_siefore
               INTO ls_nss_cod_sief
               FROM cta_regimen
               WHERE nss = pc_nss
               AND subcuenta IN (15,16)     
      END CASE

      IF SQLCA.SQLCODE < 0 THEN
         PROMPT "SE PRESENTO UN ERROR AL CONSULTAR REGIMEN, NOTIFIQUE A SISTEMAS: " FOR CHAR ENTER
         CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
         EXIT PROGRAM
      END IF

      IF ls_nss_cod_sief IS NULL OR SQLCA.SQLCODE = NOTFOUND THEN
          PROMPT "LA CUENTA NO TIENE REGIMEN, NOTIFIQUE A SISTEMAS: " FOR CHAR ENTER
          CALL ERRORLOG("LA CUENTA "||pc_nss||" NO TIENE REGIMEN")
          EXIT PROGRAM
      END IF

   WHENEVER ERROR STOP

   --SE OBTIENE LA FECHA DEL ULTIMO RETIRO CON BASE A LA SIEFORE
   LET ld_fecha_ul_vol = f_fecha_ult_voluntaria(pc_nss, ls_nss_cod_sief,gr_general.tipo_ret)

   --SE OBTIENE LA FECHA DEL PRIMER RETIRO CON BASE A LA SIEFORE

   --DISPLAY "ld_fecha_ul_vol", ld_fecha_ul_vol  --DEBUG

   IF ls_nss_cod_sief = 6 THEN
      --SE VALIDAN 2 MESES
      LET ld_fecha_aux = HOY - 2 UNITS MONTH
      IF ld_fecha_aux < ld_fecha_ul_vol THEN
         OPEN WINDOW w_aux3 AT 10,8 WITH 6 ROWS, 63 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST)
         DISPLAY "      NO SE PUEDE CAPTURAR LA SOLICITUD DE RETIRO PARA EL    "             AT 2,1 ATTRIBUTE(NORMAL)
         DISPLAY " NSS ", pc_nss CLIPPED, ", NO HAN PASADO 2 MESES DESDE EL ULTIMO RETIRO  " AT 4,1 ATTRIBUTE(NORMAL)
         PROMPT  "                         <ACEPTAR>                            " ATTRIBUTE (REVERSE) FOR CHAR enter
         CLOSE WINDOW w_aux3
         
         RETURN FALSE
      END IF
   ELSE
   
      --SE VALIDAN 6 MESES
      LET ld_fecha_aux = HOY - 6 UNITS MONTH
      IF ld_fecha_aux < ld_fecha_ul_vol THEN
         OPEN WINDOW w_aux3 AT 10,8 WITH 6 ROWS, 63 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST)
         DISPLAY "      NO SE PUEDE CAPTURAR LA SOLICITUD DE RETIRO PARA EL    "             AT 2,1 ATTRIBUTE(NORMAL)
         DISPLAY " NSS ", pc_nss CLIPPED, ", NO HAN PASADO 6 MESES DESDE EL ULTIMO RETIRO  " AT 4,1 ATTRIBUTE(NORMAL)
         PROMPT  "                         <ACEPTAR>                            " ATTRIBUTE (REVERSE) FOR CHAR enter
         CLOSE WINDOW w_aux3
         RETURN FALSE
      END IF

      --SE VALIDAN 5 AÑOS DE PERMANENCIA 
      IF gr_general.tipo_ret = 13 OR gr_general.tipo_ret = 14 THEN 
         --SE OBTIENE PRIMERA FECHA
         LET ld_fecha_primera_vol = f_fecha_primera_voluntaria(pc_nss, ls_nss_cod_sief,gr_general.tipo_ret)
         LET ld_fecha_aux = HOY - 60 UNITS MONTH
         IF ld_fecha_aux < ld_fecha_primera_vol THEN
             OPEN WINDOW w_aux3 AT 10,8 WITH 6 ROWS, 63 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST)
             DISPLAY "      NO SE PUEDE CAPTURAR LA SOLICITUD DE RETIRO PARA EL    "             AT 2,1 ATTRIBUTE(NORMAL)
             DISPLAY " NSS ", pc_nss CLIPPED, ", NO HAN PASADO 5 AÑOS DESDE DE PERMANENCIA  " AT 4,1 ATTRIBUTE(NORMAL)
             PROMPT  "                         <ACEPTAR>                            " ATTRIBUTE (REVERSE) FOR CHAR enter
             CLOSE WINDOW w_aux3
             RETURN FALSE
          END IF
      END IF 
      
   END IF

   RETURN TRUE

END FUNCTION
#==============================================================================#
#  SE AGREGA VALIDACION DE PRIMER APORTE                                       #
#==============================================================================#
FUNCTION f_valida_primer_aporte(pc_nss,ps_tipo_ret)

   DEFINE pc_nss                  CHAR(11)
   DEFINE ps_tipo_ret             SMALLINT

   DEFINE ld_fecha_primer_aporte  DATE
   DEFINE ld_fecha_aux            DATE
   DEFINE ls_nss_cod_sief         SMALLINT
#MLM-3140 INI
   DEFINE lc_cad                  CHAR(1500),    #variable de prepare
          lc_sub                  CHAR(100)     #variable para subcuentas

      LET lc_sub = ""

       --SE OBTIENE LA SIEFORE DEL NSS DE ACUERDO AL REGIMEN y AL TIPO DE RETIRO SELECCIONADO
      CASE
          WHEN (gr_general.tipo_ret = 1 OR gr_general.tipo_ret = 2) #MLM-3150
               LET lc_sub = "(3,10)"
          WHEN (gr_general.tipo_ret = 3 OR gr_general.tipo_ret = 4)
               LET lc_sub = "(22,23)"
          WHEN (gr_general.tipo_ret = 6)    #MLM-3420
               LET lc_sub = "(3,10,22,23)"
          WHEN (gr_general.tipo_ret = 7 OR gr_general.tipo_ret = 8)
               LET lc_sub = "(11,12)"
          WHEN (gr_general.tipo_ret = 9 OR gr_general.tipo_ret = 10)
               LET lc_sub = "(24,25)"
          WHEN (gr_general.tipo_ret = 11 OR gr_general.tipo_ret = 12)
               LET lc_sub = "(11,12,24,25)"
          WHEN (gr_general.tipo_ret = 13 OR gr_general.tipo_ret = 14) ##CPL-3203
               LET lc_sub = "(15,16)"    
          WHEN ps_tipo_ret = 15   ##CPL-3345
               LET lc_sub = "(3,10,11,12,15,16,20,21,22,23,24,25,26,27,28,29)"                   
      END CASE

    WHENEVER ERROR CONTINUE

      LET lc_cad = ""
      LET lc_cad = " SELECT UNIQUE codigo_siefore \n ",
                   " FROM cta_regimen \n",
                   " WHERE nss = '", pc_nss, "'\n",
                   " AND subcuenta IN ", lc_sub CLIPPED
#DISPLAY "sief ",lc_cad
      LET lc_cad = lc_cad CLIPPED
      PREPARE p_sief FROM lc_cad
      EXECUTE p_sief INTO ls_nss_cod_sief

      IF SQLCA.SQLCODE < 0 OR ls_nss_cod_sief IS NULL THEN
         PROMPT "ERROR AL CONSULTAR REGIMEN EN PRIMER APORTE, NOTIFIQUE A SISTEMAS: " FOR CHAR ENTER
         CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
         EXIT PROGRAM
      END IF

      --RECUPERA EL PRIMER APORTE DEL NSS EN SUBCUENTAS VOLUNTARIAS

      LET lc_cad = ""
      LET lc_cad = " SELECT MIN(fecha_conversion) \n ",
                   " FROM cta_saldo_vol \n",
                   " WHERE nss = '", pc_nss, "'\n",
                   " AND subcuenta IN ", lc_sub CLIPPED, " \n",
                   " AND folio NOT IN (SELECT folio_liquida                                  \n",
                   "                   FROM uni_unificado                                    \n",
                   "                   WHERE nss_uni = '", pc_nss,                       "' )\n",   #MLM-3113
                   " AND folio NOT IN (SELECT MAX(folio)                                     \n",
                   "                   FROM taa_viv_recepcion                                \n",
                   "                   WHERE nss = '", pc_nss, "' \n",
                   "                   AND fecha_mov_banxico = (SELECT MAX(fecha_mov_banxico) \n ",
                   "                                            FROM taa_viv_recepcion \n",
                   "                                            WHERE nss = '", pc_nss,"' ))"   #MLM-3140
#DISPLAY "fec ",lc_cad
      LET lc_cad = lc_cad CLIPPED
      PREPARE p_primer_fec FROM lc_cad
   #MLM-3289 INI       #-- VE SI VIENE DE TRASPASPASO --#
   LET ld_fecha_primer_aporte = NULL

   SELECT fecha_vol_pat
   INTO   ld_fecha_primer_aporte
   FROM   taa_rcv_recepcion A
   WHERE  A.nss = pc_nss
   AND    A.fecha_mov_banxico = (SELECT MAX(fecha_mov_banxico)
                                 FROM   taa_rcv_recepcion
                                 WHERE  nss = pc_nss
                                 AND    ident_operacion = "09"   #-- TRASPASO NORMAL --#
                                 AND    (fecha_vol_pat IS NOT NULL AND fecha_vol_pat <> "01010001"))
   AND    (fecha_vol_pat IS NOT NULL AND fecha_vol_pat <> "01010001")
   GROUP BY 1     #MLM-3455

      IF SQLCA.SQLCODE < 0 THEN
      ERROR "SE PRESENTO UN ERROR AL CONSULTAR PRIMER APORTE TAA "
      PROMPT "NOTIFIQUE AL AREA DE SISTEMAS Y PRESIONE <ENTER> PARA CONTINUAR:" FOR CHAR enter
      EXIT PROGRAM
   END IF

   IF ld_fecha_primer_aporte IS NULL  THEN
      EXECUTE p_primer_fec INTO ld_fecha_primer_aporte    #MLM-3140 FIN

      IF SQLCA.SQLCODE < 0 THEN
         ERROR "SE PRESENTO UN ERROR AL CONSULTAR PRIMER APORTE "
         PROMPT "NOTIFIQUE AL AREA DE SISTEMAS Y PRESIONE <ENTER> PARA CONTINUAR:" FOR CHAR enter
         EXIT PROGRAM
      END IF
   END IF #MLM-3289 FIN

   WHENEVER ERROR STOP

   --DISPLAY "ld_fecha_primer_aporte=", ld_fecha_primer_aporte  --DEBUG

   IF ls_nss_cod_sief = 6 THEN
      --SE VALIDAN 2 MESES
      LET ld_fecha_aux = HOY - 2 UNITS MONTH
      IF ld_fecha_aux < ld_fecha_primer_aporte THEN
         OPEN WINDOW w_aux3 AT 10,8 WITH 6 ROWS, 63 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST)
         DISPLAY "      NO SE PUEDE CAPTURAR LA SOLICITUD DE RETIRO PARA EL    "             AT 2,1
         DISPLAY " NSS ", pc_nss CLIPPED, ", NO HAN PASADO 2 MESES DESDE EL PRIMER APORTE  " AT 4,1
         PROMPT  "                         <ACEPTAR>                            " ATTRIBUTE (REVERSE) FOR CHAR enter
         CLOSE WINDOW w_aux3
         RETURN FALSE
      END IF
   ELSE
      --SE VALIDAN 6 MESES
      LET ld_fecha_aux = HOY - 6 UNITS MONTH
      IF ld_fecha_aux < ld_fecha_primer_aporte THEN
         OPEN WINDOW w_aux3 AT 10,8 WITH 6 ROWS, 63 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST)
         DISPLAY "      NO SE PUEDE CAPTURAR LA SOLICITUD DE RETIRO PARA EL    "             AT 2,1
         DISPLAY " NSS ", pc_nss CLIPPED, ", NO HAN PASADO 6 MESES DESDE EL PRIMER APORTE  " AT 4,1
         PROMPT  "                         <ACEPTAR>                            " ATTRIBUTE (REVERSE) FOR CHAR enter
         CLOSE WINDOW w_aux3
         RETURN FALSE
      END IF
   END IF

   RETURN TRUE

END FUNCTION
#------------------------------------------------------------------------------#
#                                                                              #
#------------------------------------------------------------------------------#
FUNCTION f_fecha_ult_voluntaria(p_nss, p_siefore,ls_tipo_ret)

   DEFINE p_siefore       SMALLINT
   DEFINE p_nss           CHAR(0011)
   DEFINE v_tabname       CHAR(0050)
   DEFINE v_comando       CHAR(3000)
   DEFINE v_max_fecha     DATE
   DEFINE ls_cont_anio    SMALLINT
   DEFINE ls_tipo_ret     SMALLINT #MLM-3150
   DEFINE ls_tipo_mov     SMALLINT#MLM-3150
   #MLM-3150
   IF ls_tipo_ret <5 THEN
         LET ls_tipo_mov = 490
      ELSE
         LET ls_tipo_mov = 897
   END IF
   #MLM-3150
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta
      #MLM-3150

      LET v_comando = " SELECT * FROM dis_cuenta ",
                      " WHERE nss = '",p_nss CLIPPED,"'",
                      " AND   tipo_movimiento = ", ls_tipo_mov , #MLM-3150
                      " AND   siefore = ",p_siefore CLIPPED

      --PREPARA LA SELECCION PARA BARRER TODAS LAS TABLAS DE DIS_CUENTA
      LET ls_cont_anio = 0

      DECLARE cur_dis_cta CURSOR FOR
      SELECT tabname
      FROM SYSTABLES
      WHERE  tabname MATCHES "dis_cuenta?*"

      FOREACH cur_dis_cta INTO v_tabname
         LET ls_cont_anio = ls_cont_anio + 1
         LET v_comando = v_comando CLIPPED, " UNION ",
                         " SELECT * FROM ",v_tabname CLIPPED,
                         " WHERE nss = '",p_nss,"'",
                         " AND   tipo_movimiento = ", ls_tipo_mov , #MLM-3150
                         " AND   siefore = ",p_siefore CLIPPED
         IF ls_cont_anio = 2 THEN   --solo busca en los ultimos 2 respaldos
            EXIT FOREACH
         END IF
      END FOREACH

      LET v_comando = v_comando CLIPPED, " INTO TEMP tmp_dis_cuenta "

      PREPARE exe_dis_cuenta FROM v_comando

      IF SQLCA.SQLCODE < 0 THEN
         PROMPT "SE PRESENTO UN ERROR AL PREPARAR CONSULTA A DIS_CUENTA :" FOR CHAR ENTER
         CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
         CALL ERRORLOG(v_comando CLIPPED)
         EXIT PROGRAM
      END IF

      EXECUTE exe_dis_cuenta

      IF SQLCA.SQLCODE < 0 THEN
         CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
         CALL ERRORLOG(v_comando CLIPPED)
--         DISPLAY "v_comando:",v_comando
         EXIT PROGRAM
      END IF

      SELECT MAX(fecha_conversion)
      INTO v_max_fecha
      FROM tmp_dis_cuenta

   WHENEVER ERROR STOP

   RETURN v_max_fecha

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_consulta(ls_tipo)

   DEFINE lr_saldos   RECORD
          acc_sbf_sb1                 ,
          acc_sbf_s6                  ,
          pes_sbf                     ,
          acc_cbf_sb1                 ,
          acc_cbf_s6                  ,
          pes_cbf                     ,
          pes         DECIMAL(16,6)
      END RECORD

   DEFINE c9des_estado_solic CHAR(009),
          hace_el_input               ,
          hace_el_select     CHAR(500),
          s_tot_registros             ,
          arr_c                       ,
          pos                         ,
          ls_tipo             SMALLINT

   DEFINE
      lc_texto             CHAR(200)
      ,lc_sql                 CHAR(2000)


-------------------------------------------------------------------------------
   CLEAR FORM

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1

    --DISPLAY " <Esc> Consultar                   <Ctrl-C> Salir  "        AT 1,1
    --DISPLAY " CONSULTA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
   IF( ls_tipo = 1 ) THEN
      DISPLAY " <Esc> Consultar                   <Ctrl-C> Salir  "        AT 1,1
      DISPLAY " CONSULTA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
   ELSE
      DISPLAY " <ESC> Buscar                      <Ctrl-C> Salir  " AT 1, 1
      DISPLAY " RECHAZA " AT 1, 69 ATTRIBUTE( REVERSE, BOLD )
   END IF

   LET INT_FLAG      = FALSE
   LET hace_el_input = NULL

   INITIALIZE reg_1.* TO NULL

   #DEFINE CRITERIO DE BUSQUEDA
   CONSTRUCT hace_el_input ON A.n_folio ,
                              A.n_seguro,
                              A.n_rfc   ,
                              A.n_unico ,
                              A.paterno ,
                              A.materno ,
                              A.nombres ,
                              B.estado
                         FROM n_folio   ,
                              n_seguro  ,
                              n_rfc     ,
                              n_unico   ,
                              paterno   ,
                              materno   ,
                              nombres   ,
                              estado

      --------------
       ON KEY (ESC)
      --------------
          ERROR ""
          ERROR " PROCESANDO INFORMACION... "
          LET INT_FLAG = FALSE
          EXIT CONSTRUCT

        -------------------
        ON KEY (CONTROL-C)
        -------------------
           LET INT_FLAG = TRUE
           CLEAR FORM
           EXIT CONSTRUCT
   END CONSTRUCT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      ERROR " BUSQUEDA CANCELADA..."
      CLEAR SCREEN
      RETURN
   END IF

   DISPLAY " <Ctrl-B> Consulta beneficiarios   " AT 2,1

   #ARMA QUERY DE SELECCION
   LET hace_el_select = " SELECT A.n_folio  ,",
                        "        A.n_seguro ,",
                        "        A.n_rfc    ,",
                        "        A.n_unico  ,",
                        "        A.fentcons ,",
                        "        A.paterno  ,",
                        "        A.materno  ,",
                        "        A.nombres  ,",
                        "        USER        ",
                        " FROM   afi_mae_afiliado A,  ",
                        "        ret_cta_vol      B  ",
                        --"        ret_cta_vol_comp C   ",
                        " WHERE ",hace_el_input CLIPPED,
                        " AND    A.n_seguro = B.n_seguro ",
                        --" AND   (B.n_seguro = C.n_seguro AND   ",
                        --"        B.consecutivo = C.consecutivo)",
                        " GROUP BY 1,2,3,4,5,6,7,8,9 ",
                        " ORDER BY 2,3" CLIPPED

   LET pos = 1

   PREPARE cur1 FROM hace_el_select
   DECLARE cursor_1 CURSOR FOR cur1

   --SELECCIONA CADA SOLUCITUD CONSULTADA
   FOREACH cursor_1 INTO reg_1.*,c8_usuario
   #1902
   LET lc_texto = " "

   IF( ls_tipo = 2 ) THEN    #* Rech. *
      LET lc_texto  = "AND A.estado IN ( 0, 3 , -1 ) AND cod_rechazo_ent IS NULL"
   END IF


      LET lc_sql =
      " SELECT 0                                   ,"
      ,"        0                                  ,"
      ,"        0                                  ,"
      ,"        0                                  ,"
      ,"        0                                  ,"
      ,"        A.fecha_ult_ret                    ,"              #fecha_ult_ret
      ,"        A.n_folio_sol                      ,"              #n_folio_sol
      ,"        A.tipo_ret                         ,"              #tipo_ret
      ,"        B.des_tipo_ret                     ,"              #des_tipo_ret
      ,"        A.mto_solic                        ,"              #mto_solic
      ,"        A.porcentaje_solic                 ,"              #porcentaje_solic
      ,"        A.tipo_pago                        ,"              #tipo_pago
      ,"        ''                                 ,"              #des_tipo_pago
      ,"        A.edad                             ,"              #edad
      ,"        A.fecha_solic                      ,"              #fecha_solic
      ,"        A.fecha_captura                    ,"              #fecha_captura
      ,"        A.ultimo_proceso                   ,"              #ultimo_proceso
      ,"        A.pension_invalidez                ,"              #pension_invalidez
      ,"        A.deduccion                        ,"              #deduccion
      ,"        A.cod_rechazo_ent                  ,"              #* Rech. *
      ,"        A.consecutivo                      ,"              #consecutivo
      ,"        A.usuario                          ,"              #usuario
      ,"        A.estado                            "              #estado
      ," FROM   ret_cta_vol A, tab_retiro_old B     "
      ," WHERE  A.n_seguro  ='", reg_1.n_seguro,"'  "
      ," AND    A.tipo_ret  = B.tipo_ret            "
      ,lc_texto CLIPPED,                       "    "
      ,"ORDER BY fecha_captura                      "

      LET lc_sql = lc_sql CLIPPED
      PREPARE exe_sql FROM lc_sql
      DECLARE cursor_con_2 CURSOR FOR exe_sql
      FOREACH cursor_con_2 INTO gr_general.*
         --------------------------------------------------------------------
         --RECUPERA EL SALDO DE LAS SUBCUENTAS DE COMPLEMENTARIAS
         --------------------------------------------------------------------
         --CALL f_obten_saldos_comp(reg_1.n_seguro) RETURNING lr_saldos.*

         LET reg_2_array[pos].var_nula              =  " "
         LET reg_2_array[pos].n_folio               =  reg_1.n_folio
         LET reg_2_array[pos].n_seguro              =  reg_1.n_seguro
         LET reg_2_array[pos].n_rfc                 =  reg_1.n_rfc
         LET reg_2_array[pos].n_unico               =  reg_1.n_unico
         LET reg_2_array[pos].fentcons              =  reg_1.fentcons
         LET reg_2_array[pos].paterno               =  reg_1.paterno
         LET reg_2_array[pos].materno               =  reg_1.materno
         LET reg_2_array[pos].nombres               =  reg_1.nombres

         LET reg_2_array[pos].acc_sbf_sb1           =  lr_saldos.acc_sbf_sb1
         LET reg_2_array[pos].acc_sbf_s6            =  lr_saldos.acc_sbf_s6
         LET reg_2_array[pos].pes_sbf               =  lr_saldos.pes_sbf
         LET reg_2_array[pos].acc_cbf_sb1           =  lr_saldos.acc_cbf_sb1
         LET reg_2_array[pos].acc_cbf_s6            =  lr_saldos.acc_cbf_s6
         LET reg_2_array[pos].pes_cbf               =  lr_saldos.pes_cbf
         LET reg_2_array[pos].pes                   =  lr_saldos.pes

         LET reg_2_array[pos].fecha_ult_ret         =  gr_general.fecha_ult_ret
         LET reg_2_array[pos].n_folio_sol           =  gr_general.n_folio_sol
         LET reg_2_array[pos].tipo_ret              =  gr_general.tipo_ret
         LET reg_2_array[pos].des_tipo_ret          =  gr_general.des_tipo_ret
         LET reg_2_array[pos].mto_solic             =  gr_general.mto_solic
         LET reg_2_array[pos].porcentaje_solic      =  gr_general.porcentaje_solic
         LET reg_2_array[pos].edad                  =  gr_general.edad
         LET reg_2_array[pos].fecha_solic           =  gr_general.fecha_solic
         LET reg_2_array[pos].fecha_captura         =  gr_general.fecha_captura
         LET reg_2_array[pos].ultimo_proceso        =  gr_general.ultimo_proceso
         LET reg_2_array[pos].pension_invalidez     =  gr_general.pension_invalidez
         LET reg_2_array[pos].deduccion             =  gr_general.deduccion
       --LET reg_2_array[pos].fecha_deduccion       =  gr_general.fecha_deduccion
       --LET reg_2_array[pos].mto_deducido          =  gr_general.mto_deducido
         LET reg_2_array[pos].cod_rechazo_ent       =  gr_general.cod_rechazo_ent
         LET reg_2_array[pos].consecutivo           =  gr_general.consecutivo
         LET reg_2_array[pos].usuario               =  gr_general.usuario
         LET reg_2_array[pos].estado                =  gr_general.estado

         LET pos = pos + 1
      END FOREACH
   END FOREACH

   CALL SET_COUNT(pos-1)

   LET s_tot_registros = pos - 1

   IF(pos-1) >= 1 THEN
      INPUT ARRAY reg_2_array WITHOUT DEFAULTS FROM scr_3.* ATTRIBUTES(MAXCOUNT=s_tot_registros)
         BEFORE ROW
            LET arr_c = ARR_CURR()

            IF arr_c = s_tot_registros THEN
               ERROR " NO HAY MAS REGISTROS HACIA ABAJO "
            END IF

            --SELECCIONA Y DESPLIEGA DESCRIPCION DEL ES
            SELECT descripcion
            INTO   c9des_estado_solic
            FROM   ret_estado A
            WHERE  A.estado_solicitud = reg_2_array[arr_c].estado

            DISPLAY c9des_estado_solic TO desc_estado -- 20, 41

            CALL f_obten_saldos_comp2(reg_2_array[arr_c].n_seguro,reg_2_array[arr_c].tipo_ret) RETURNING lr_saldos.*

            DISPLAY BY NAME lr_saldos.*

            IF( ls_tipo = 2 ) THEN    #* Rech., Solo opcion Rechaza *
               NEXT FIELD cod_rechazo_ent
            END IF

            AFTER FIELD cod_rechazo_ent
               IF( ls_tipo = 2 ) THEN    #* Rech., Solo opcion Rechaza *
                  IF( reg_2_array[arr_c].cod_rechazo_ent IS NOT NULL ) THEN
                     SELECT   1
                     FROM     ret_rechazo_grl
                     WHERE    cod_rechazo_ent = reg_2_array[arr_c].cod_rechazo_ent
                     AND      entidad = 1
                     AND      tipo_retiro IN ( "D", "G" )

                     IF SQLCA.SQLCODE = NOTFOUND THEN
                        ERROR "NO EXISTE ESTE CODIGO DE RECHAZO"
                        LET reg_2_array[arr_c].cod_rechazo_ent = NULL

                        NEXT FIELD cod_rechazo_ent
                     END IF
                  ELSE
                     -- Se despliega ventana de ayuda de codigo de rechazo
                     CALL despliega_cod_rechazo_ent( reg_2_array[arr_c].tipo_ret )
                     RETURNING reg_2_array[arr_c].cod_rechazo_ent

                     IF( reg_2_array[arr_c].cod_rechazo_ent <= 0 ) THEN
                        ERROR "NO EXISTE ESTE CODIGO DE RECHAZO "
                        LET reg_2_array[arr_c].cod_rechazo_ent = NULL
                        NEXT FIELD cod_rechazo_ent
                     END IF
                  END IF

                  DISPLAY BY NAME reg_2_array[arr_c].cod_rechazo_ent
                  PROMPT "CONFIRMA RECHAZO DE SOLICITUD (S/N)" FOR CHAR enter

                  IF enter MATCHES "[NnSs]" THEN
                     CASE
                        WHEN enter MATCHES "[Ss]"
                           UPDATE   ret_cta_vol
                           SET      cod_rechazo_ent   = reg_2_array[arr_c].cod_rechazo_ent,
                                    estado            = gr_estados.rechazado
                           WHERE    n_seguro          = reg_2_array[arr_c].n_seguro
                           AND      consecutivo       = reg_2_array[arr_c].consecutivo

                           IF reg_2_array[arr_c].tipo_ret <= 6 OR
                              reg_2_array[arr_c].tipo_ret = 13 OR 
                              reg_2_array[arr_c].tipo_ret = 14 THEN
                              LET g_marca_ent = 490
                           ELSE
                              LET g_marca_ent = 897
                           END IF
                           CALL fn_desmarca_cuenta ( reg_2_array[arr_c].n_seguro,reg_2_array[arr_c].consecutivo)
                          
                           DISPLAY "RECHAZO CONFIRMADO... " AT 21, 2
                              ATTRIBUTE( REVERSE )
                           SLEEP 3

                        WHEN enter MATCHES "[Nn]"
                           DISPLAY "RECHAZO CANCELADO... " AT 21, 2
                              ATTRIBUTE( REVERSE )
                           SLEEP 3
                     END CASE
                  END IF
                  EXIT INPUT
               END IF


         ON KEY(CONTROL-B)
            LET arr_c = ARR_CURR()

            LET v_ejecuta = "fglgo RETM810 ", reg_2_array[arr_c].n_seguro CLIPPED,
               " ", reg_2_array[arr_c].consecutivo CLIPPED, " ", 'A', reg_2_array[pos].n_folio_sol
            RUN v_ejecuta

         ON KEY (INTERRUPT)
            --DISPLAY "Total Registros               " AT 20,1
            --DISPLAY "         " AT 19, 41
            EXIT INPUT

         ON KEY (CONTROL-C)
            --DISPLAY "Total Registros               " AT 19, 56
            --DISPLAY "         " AT 19, 41
            EXIT INPUT
      END INPUT
   ELSE
      ERROR " NO EXISTEN REGISTROS "
   END IF

   MESSAGE ""
   CLEAR FORM

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_modifica()

   DEFINE lr_saldos   RECORD
          acc_sbf_sb1              ,
          acc_sbf_s6               ,
          pes_sbf                  ,
          acc_cbf_sb1              ,
          acc_cbf_s6               ,
          pes_cbf                  ,
          pes         DECIMAL(16,6)
      END RECORD

   DEFINE c9des_estado_solic  CHAR(009),
          hace_el_input       CHAR(300),
          hace_el_select      CHAR(500),
          s_tot_registros              ,
          arr_c                        ,
          pos                          ,
          ban                 SMALLINT

--------------------------------------------------------------------------------
   CLEAR FORM

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " <Ctrl-C> Regresa al menu     "   AT 1,1
   DISPLAY "  <ESC>   Despliega Registros "   AT 2,1
   DISPLAY " MODIFICA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)

   LET ban           = 0
   LET INT_FLAG      = FALSE
   LET hace_el_input = NULL

   ---------------------------------------------------
   --ARMA CRITERIO DE BUSQUEDA PARA LA MODIFICACION
   ---------------------------------------------------
   CONSTRUCT hace_el_input ON A.n_folio  ,
                              A.n_seguro ,
                              A.n_rfc    ,
                              A.n_unico  ,
                              A.paterno  ,
                              A.materno  ,
                              A.nombres
                         FROM n_folio    ,
                              n_seguro   ,
                              n_rfc      ,
                              n_unico    ,
                              paterno    ,
                              materno    ,
                              nombres

       ON KEY (ESC)
           LET INT_FLAG = FALSE
           EXIT CONSTRUCT

       ON KEY (CONTROL-C)
           LET INT_FLAG = TRUE
           CLEAR FORM
           ERROR " OPERACION CANCELADA "
           EXIT CONSTRUCT
   END CONSTRUCT

   MESSAGE " PROCESANDO INFORMACION... " ATTRIBUTE(REVERSE)

   IF INT_FLAG = TRUE THEN
       LET INT_FLAG  = FALSE
       CLEAR SCREEN
       ERROR " OPERACION CANCELADA... "
       RETURN
   END IF

    DISPLAY " <Ctrl-V> Consulta Beneficiarios " AT 2,35
    DISPLAY " <ENTER> Modificar Registro      " AT 1,35
    DISPLAY " <Ctrl-C> Cancelar               " AT 1,1
    DISPLAY "                                 " AT 2,1
   ----------------------------------------------------
   --ARMA CRITERIO DE BUSQUEDA DE REGISTROS A MODIFICAR
   ----------------------------------------------------
   LET hace_el_select = " SELECT A.n_folio  ,",
                        "        A.n_seguro ,",
                        "        A.n_rfc    ,",
                        "        A.n_unico  ,",
                        "        A.fentcons ,",
                        "        A.paterno  ,",
                        "        A.materno  ,",
                        "        A.nombres  ,",
                        "        USER        ",
                        " FROM   afi_mae_afiliado A  ",
                        "        ,ret_cta_vol      B  ",
                        --"        ,ret_cta_vol_comp C   ",
                        " WHERE ",hace_el_input CLIPPED,
                        " AND    A.n_seguro    = B.n_seguro ",
                        --" AND   (B.n_seguro    = C.n_seguro AND    ",
                        --"        B.consecutivo = C.consecutivo) ",
                        " GROUP BY 1,2,3,4,5,6,7,8,9 ",
                        " ORDER BY 2,3" CLIPPED

   WHENEVER ERROR CALL f_errores

   PREPARE pre_8 FROM hace_el_select

   IF SQLCA.SQLCODE < 0 THEN
      ERROR " ERROR AL PREPARAR CONSULTA, VERIFIQUE CRITERIO o NOTIFIQUE A SISTEMAS "
   END IF

   WHENEVER ERROR STOP

   DECLARE cur_8 CURSOR FOR pre_8

   LET pos = 1

   FOREACH cur_8 INTO reg_1.*,c8_usuario
       LET ban = 0

       DECLARE cur_9 CURSOR FOR
       SELECT
            --0                   ,
            --0                   ,
            --0                   ,
            --0                   ,
              0                   ,
              0                   ,
              0                   ,
              0                   ,
              0                   ,
              A.fecha_ult_ret     ,
              A.n_folio_sol       ,
              A.tipo_ret          ,
              B.des_tipo_ret      ,
              A.mto_solic         ,
              A.porcentaje_solic  ,
              A.tipo_pago         ,
              ""                  ,#des_tipo_pago
              A.edad              ,
              A.fecha_solic       ,
              A.fecha_captura     ,
              A.ultimo_proceso    ,
              A.pension_invalidez ,#pension_invalidez
              A.deduccion         ,#deduccion
            --A.fecha_deduccion   ,#fecha_deduccion
            --A.mto_deducido      ,#mto_deducido
              A.cod_rechazo_ent  ,#* Rech. *
              A.consecutivo       ,
              A.usuario           ,
              A.estado
       FROM   ret_cta_vol A, tab_retiro_old B
       WHERE  A.n_seguro  = reg_1.n_seguro
       AND    A.tipo_ret  = B.tipo_ret
       ORDER BY fecha_captura

       FOREACH cur_9 INTO gr_general.*
           --------------------------------------------------------------------
           --RECUPERA EL SALDO DE LAS SUBCUENTAS DE COMPLEMENTARIAS
           --------------------------------------------------------------------
           --CALL f_obten_saldos_comp(reg_1.n_seguro) RETURNING lr_saldos.*


           LET reg_2_array[pos].var_nula              =  " "
           LET reg_2_array[pos].n_folio               =  reg_1.n_folio
           LET reg_2_array[pos].n_seguro              =  reg_1.n_seguro
           LET reg_2_array[pos].n_rfc                 =  reg_1.n_rfc
           LET reg_2_array[pos].n_unico               =  reg_1.n_unico
           LET reg_2_array[pos].fentcons              =  reg_1.fentcons
           LET reg_2_array[pos].paterno               =  reg_1.paterno
           LET reg_2_array[pos].materno               =  reg_1.materno
           LET reg_2_array[pos].nombres               =  reg_1.nombres

           LET reg_2_array[pos].acc_sbf_sb1           =  lr_saldos.acc_sbf_sb1
           LET reg_2_array[pos].acc_sbf_s6            =  lr_saldos.acc_sbf_s6
           LET reg_2_array[pos].pes_sbf               =  lr_saldos.pes_sbf
           LET reg_2_array[pos].acc_cbf_sb1           =  lr_saldos.acc_cbf_sb1
           LET reg_2_array[pos].acc_cbf_s6            =  lr_saldos.acc_cbf_s6
           LET reg_2_array[pos].pes_cbf               =  lr_saldos.pes_cbf
           LET reg_2_array[pos].pes                   =  lr_saldos.pes

           LET reg_2_array[pos].fecha_ult_ret         =  gr_general.fecha_ult_ret
           LET reg_2_array[pos].n_folio_sol           =  gr_general.n_folio_sol
           LET reg_2_array[pos].tipo_ret              =  gr_general.tipo_ret
           LET reg_2_array[pos].des_tipo_ret          =  gr_general.des_tipo_ret
           LET reg_2_array[pos].mto_solic             =  gr_general.mto_solic
           LET reg_2_array[pos].porcentaje_solic      =  gr_general.porcentaje_solic
           LET reg_2_array[pos].edad                  =  gr_general.edad
           LET reg_2_array[pos].fecha_solic           =  gr_general.fecha_solic
           LET reg_2_array[pos].fecha_captura         =  gr_general.fecha_captura
           LET reg_2_array[pos].ultimo_proceso        =  gr_general.ultimo_proceso
           LET reg_2_array[pos].pension_invalidez     =  gr_general.pension_invalidez
           LET reg_2_array[pos].deduccion             =  gr_general.deduccion
           LET reg_2_array[pos].cod_rechazo_ent       =  gr_general.cod_rechazo_ent
           LET reg_2_array[pos].consecutivo           =  gr_general.consecutivo
           LET reg_2_array[pos].usuario               =  gr_general.usuario
           LET reg_2_array[pos].estado                =  gr_general.estado

           LET pos = pos + 1
       END FOREACH
   END FOREACH

   LET s_tot_registros = pos - 1

   CALL SET_COUNT(s_tot_registros)

   IF (s_tot_registros) >= 1 THEN

       --GESTIONA LA MODIFICACION DE CADA SOLICITUD CONSULTADA
       INPUT ARRAY reg_2_array WITHOUT DEFAULTS FROM scr_3.* ATTRIBUTE(MAXCOUNT=s_tot_registros)
           --------------------
           BEFORE ROW
           --------------------
               LET arr_c = ARR_CURR()
               IF arr_c >= s_tot_registros AND s_tot_registros > 1 THEN
                  ERROR " NO HAY MAS REGISTROS HACIA ABAJO "
               END IF

               SELECT descripcion
               INTO   c9des_estado_solic
               FROM   ret_estado A
               WHERE  A.estado_solicitud = reg_2_array[arr_c].estado

               --DISPLAY  "Total Registros: ",s_tot_registros USING "<<<&" AT 20,2
               DISPLAY "Total Registros: ",s_tot_registros USING "<<<&" AT 20,56
               DISPLAY c9des_estado_solic TO desc_estado

                CALL f_obten_saldos_comp2(reg_2_array[arr_c].n_seguro,reg_2_array[arr_c].tipo_ret) RETURNING lr_saldos.*

                DISPLAY BY NAME lr_saldos.*

           --------------------
           ON KEY(CONTROL-M)
           --------------------
               LET arr_c = ARR_CURR()

               IF reg_2_array[arr_c].estado = gr_estados.capturado THEN
                  ------------------------------------
                  --MODIFICA EL REGISTRO SELECCIONADO
                  ------------------------------------
                   CALL modifica_seleccion(reg_2_array[arr_c].*) #ms
                   ERROR ""
                   EXIT INPUT
               ELSE
                   PROMPT" ESTA SOLICITUD NO SE PUEDE MODIFICAR...<ENTER>",
                         " PARA CONTINUAR" FOR CHAR enter
                   EXIT INPUT
               END IF
           --------------------
           ON KEY(CONTROL-V)
           --------------------
              LET arr_c = ARR_CURR()

              LET v_ejecuta = "fglgo RETM810 ",
                 reg_2_array[arr_c].n_seguro CLIPPED, " ",
                 reg_2_array[arr_c].consecutivo CLIPPED, " ", 'C', 0 CLIPPED
              RUN v_ejecuta
           --------------------
           ON KEY (INTERRUPT)
           --------------------
               --DISPLAY "Total Registros               " AT 19,56
               DISPLAY "" AT 20,1
               EXIT INPUT

           --------------------
           ON KEY (CONTROL-C)
           --------------------
               --DISPLAY "Total Registros               " AT 19,56
               DISPLAY "" AT 20,1
               EXIT INPUT
       END INPUT
   ELSE
       PROMPT "NO HAY SOLICITUDES...<ENTER> PARA CONTINUAR" FOR CHAR enter
   END IF

   CLEAR FORM

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_elimina()

    DEFINE lr_saldos               RECORD
           acc_sbf_sb1             DECIMAL(16,6),
           acc_sbf_s6              DECIMAL(16,6),
           pes_sbf                 DECIMAL(16,6),
           acc_cbf_sb1             DECIMAL(16,6),
           acc_cbf_s6              DECIMAL(16,6),
           pes_cbf                 DECIMAL(16,6),
           pes                     DECIMAL(16,6)
       END RECORD

    DEFINE c9des_estado_solic      CHAR(009),
           hace_el_input           CHAR(300),
           hace_el_select          CHAR(500),
           s_tot_registros                  ,
           arr_c                            ,
           scr_l                            ,
           pos                     SMALLINT

--------------------------------------------------------------------------------
    CLEAR FORM

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " <Ctrl-C> Regresa al Menu " AT 1,1
    DISPLAY "   <ESC>  Despliega Registros " AT 2,1
    DISPLAY " ELIMINA " AT 1,70 ATTRIBUTE(REVERSE,BOLD)

    LET INT_FLAG      = FALSE
    LET hace_el_input = NULL

    INITIALIZE reg_1.* TO NULL

    ----------------------------------------------------------
    --ARMA CRITERIO DE BUSQUEDA PARA SELECCION DE SOLICITUDES
    ----------------------------------------------------------
    CONSTRUCT hace_el_input ON A.n_folio  ,
                               A.n_seguro ,
                               A.n_rfc    ,
                               A.n_unico  ,
                               A.paterno  ,
                               A.materno  ,
                               A.nombres
                          FROM n_folio  ,
                               n_seguro ,
                               n_rfc    ,
                               n_unico  ,
                               paterno  ,
                               materno  ,
                               nombres

        ON KEY (ESC)
            ERROR ""
            ERROR " PROCESANDO INFORMACION... "
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
            CLEAR FORM
            LET INT_FLAG = TRUE
            EXIT CONSTRUCT
    END CONSTRUCT

    DISPLAY " <Ctrl-V> Consulta Beneficiarios" AT 2,35
    DISPLAY " <ENTER> Eliminar Registro      " AT 1,35
    DISPLAY " <Ctrl-C> Cancelar              " AT 1,1
    DISPLAY "                                " AT 2,1

    IF INT_FLAG = TRUE THEN
        LET INT_FLAG = FALSE
        CLEAR SCREEN
        ERROR " BUSQUEDA CANCELADA..."
        RETURN
    END IF

    LET hace_el_select = " SELECT A.n_folio  ,",
                         "        A.n_seguro ,",
                         "        A.n_rfc    ,",
                         "        A.n_unico  ,",
                         "        A.fentcons ,",
                         "        A.paterno  ,",
                         "        A.materno  ,",
                         "        A.nombres  ,",
                         "        USER        ",
                         " FROM   afi_mae_afiliado A, ",
                         "        ret_cta_vol      B  ",
                         " WHERE ",hace_el_input CLIPPED,
                         " AND    A.n_seguro = B.n_seguro ",
                       --" AND   (B.n_seguro    = C.n_seguro AND ",
                       --"        B.consecutivo = C.consecutivo) ",
                         " AND    B.tipo_ret IN(1,2,3,4,6,7,8,9,10,11,12,13,14)  ",    #MLM-3150
                         " GROUP BY 1,2,3,4,5,6,7,8,9 ",
                         " ORDER BY 2,3" CLIPPED

    LET pos = 1
    PREPARE pre_13 FROM hace_el_select
    DECLARE cur_13 CURSOR FOR pre_13

    FOREACH cur_13 INTO reg_1.*,c8_usuario
        DECLARE cur_14 CURSOR FOR
        SELECT
               0                              ,  --mto_acc_sb1
               0                              ,  --mto_acc_s6
               0                              ,  --mto_acc_12
               0                              ,  --mto_acc_11
               0                              ,  --monto_en_pesos
               A.fecha_ult_ret                ,  --fecha_ult_ret
               A.n_folio_sol                  ,  --n_folio_sol
               A.tipo_ret                     ,  --tipo_ret
               B.des_tipo_ret                 ,  --des_tipo_ret
               A.mto_solic                    ,  --mto_solic
               A.porcentaje_solic             ,  --porcentaje_solic
               A.tipo_pago                    ,  --tipo_pago
               ""                             ,  --des_tipo_pago
               A.edad                         ,  --edad
               A.fecha_solic                  ,  --fecha_solic
               A.fecha_captura                ,  --fecha_captura
               A.ultimo_proceso               ,  ---ultimo_proceso
               A.pension_invalidez            ,  ---pension_invalidez
               A.deduccion                    ,  --deduccion                  #cpl-1902
               A.cod_rechazo_ent              ,  --codigo de rechazo
               A.consecutivo                  ,  --consecutivo
               A.usuario                      ,  --usuario
               A.estado                          --estado
        FROM   ret_cta_vol A, tab_retiro_old B
        WHERE  A.n_seguro  = reg_1.n_seguro
        AND    A.tipo_ret  = B.tipo_ret
        ORDER BY fecha_captura

        FOREACH cur_14 INTO gr_general.*

            LET aux_monto_en_pesos          = 0
            LET precio_dia_sb1              = 0
            LET precio_dia_s6               = 0

            CALL precio_accion(gr_general.fecha_captura,1) RETURNING precio_dia_sb1

            LET reg_2_array[pos].var_nula          =  " "
            LET reg_2_array[pos].n_folio           =  reg_1.n_folio
            LET reg_2_array[pos].n_seguro          =  reg_1.n_seguro
            LET reg_2_array[pos].n_rfc             =  reg_1.n_rfc
            LET reg_2_array[pos].n_unico           =  reg_1.n_unico
            LET reg_2_array[pos].fentcons          =  reg_1.fentcons
            LET reg_2_array[pos].paterno           =  reg_1.paterno
            LET reg_2_array[pos].materno           =  reg_1.materno
            LET reg_2_array[pos].nombres           =  reg_1.nombres

            LET reg_2_array[pos].acc_sbf_sb1           =  0 --gr_general.mto_acc_sb1
            LET reg_2_array[pos].acc_sbf_s6            =  0 --gr_general.mto_acc_s6
            LET reg_2_array[pos].pes_sbf               =  0 --gr_general.mto_acc_s6
            LET reg_2_array[pos].acc_cbf_sb1           =  0 --gr_general.mto_acc_sb1
            LET reg_2_array[pos].acc_cbf_s6            =  0 --gr_general.mto_acc_s6
            LET reg_2_array[pos].pes_cbf               =  0 --gr_general.mto_acc_s6
            LET reg_2_array[pos].pes                   =  0 --gr_general.monto_en_pesos

            LET reg_2_array[pos].fecha_ult_ret     =  gr_general.fecha_ult_ret
            LET reg_2_array[pos].n_folio_sol       =  gr_general.n_folio_sol
            LET reg_2_array[pos].tipo_ret          =  gr_general.tipo_ret
            LET reg_2_array[pos].des_tipo_ret      =  gr_general.des_tipo_ret
            LET reg_2_array[pos].mto_solic         =  gr_general.mto_solic
            LET reg_2_array[pos].porcentaje_solic  =  gr_general.porcentaje_solic
            LET reg_2_array[pos].edad              =  gr_general.edad
            LET reg_2_array[pos].fecha_solic       =  gr_general.fecha_solic
            LET reg_2_array[pos].fecha_captura     =  gr_general.fecha_captura
            LET reg_2_array[pos].ultimo_proceso    =  gr_general.ultimo_proceso
            LET reg_2_array[pos].pension_invalidez =  gr_general.pension_invalidez
            LET reg_2_array[pos].deduccion         =  gr_general.deduccion
            LET reg_2_array[pos].cod_rechazo_ent   =  gr_general.cod_rechazo_ent
            LET reg_2_array[pos].consecutivo       =  gr_general.consecutivo
            LET reg_2_array[pos].usuario           =  gr_general.usuario
            LET reg_2_array[pos].estado            =  gr_general.estado

            LET pos = pos + 1
        END FOREACH
    END FOREACH

    LET s_tot_registros = pos - 1

    CALL SET_COUNT(s_tot_registros)

    IF (s_tot_registros) >= 1 THEN
        INPUT ARRAY reg_2_array WITHOUT DEFAULTS FROM scr_3.* ATTRIBUTE(MAXCOUNT = s_tot_registros)
            BEFORE ROW
                LET arr_c = ARR_CURR()
                IF arr_c >= s_tot_registros THEN
                   ERROR "   NO HAY MAS REGISTROS HACIA ABAJO "
                END IF

                SELECT descripcion
                INTO   c9des_estado_solic
                FROM   ret_estado A
                WHERE  A.estado_solicitud = reg_2_array[arr_c].estado

                DISPLAY "Total Registros ",s_tot_registros AT 20,56
                DISPLAY c9des_estado_solic TO desc_estado --AT 20,41

                CALL f_obten_saldos_comp2(reg_2_array[arr_c].n_seguro,reg_2_array[arr_c].tipo_ret) RETURNING lr_saldos.*

                DISPLAY BY NAME lr_saldos.*

            ON KEY (CONTROL-M)
                IF reg_2_array[arr_c].estado = gr_estados.capturado THEN
                   WHILE TRUE
                      PROMPT " ESTA SEGURO S/N " FOR CHAR aux_pausa

                      IF aux_pausa MATCHES "[Ss]" THEN
                         DELETE
                         FROM   ret_cheque
                         WHERE  n_seguro    = reg_2_array[arr_c].n_seguro
                         AND    consecutivo = reg_2_array[arr_c].consecutivo

                         DELETE
                         FROM   ret_abono_cuenta
                         WHERE  n_seguro    = reg_2_array[arr_c].n_seguro
                         AND    consecutivo = reg_2_array[arr_c].consecutivo

                         DELETE
                         FROM   ret_cta_vol
                         WHERE  consecutivo = reg_2_array[arr_c].consecutivo

                         --DELETE
                         --FROM   ret_folio_pago
                         --WHERE  nss         = reg_2_array[arr_c].n_seguro
                         --AND    consecutivo = reg_2_array[arr_c].consecutivo

                         DELETE
                         FROM   ret_beneficiario
                         WHERE  nss         = reg_2_array[arr_c].n_seguro
                         AND    consecutivo = reg_2_array[arr_c].consecutivo

                           IF gr_general.tipo_ret = 1  OR
                              gr_general.tipo_ret = 2  OR
                              gr_general.tipo_ret = 3  OR
                              gr_general.tipo_ret = 4  OR
                              gr_general.tipo_ret = 6  OR     #MLM-3420
                              gr_general.tipo_ret = 13 OR     #MLM-3150 
                              gr_general.tipo_ret = 14 THEN   ##CPL-3203
                              LET g_marca_ent = 490
                        ELSE
                              LET g_marca_ent = 897
                        END IF
                        CALL fn_desmarca_cuenta (reg_2_array[arr_c].n_seguro,reg_2_array[arr_c].consecutivo)
                         
                         ERROR"   REGISTRO ELIMINADO "
                         CLEAR FORM
                         EXIT INPUT
                      ELSE
                         PROMPT " ELIMINACION CANCELADA...<ENTER> PARA",
                                " SALIR " FOR CHAR aux_pausa
                         CLEAR FORM
                         CLEAR SCREEN
                         EXIT INPUT

                      END IF
                   END WHILE
                ELSE
                  PROMPT" REGISTRO LIQUIDADO, NO SE PUEDE ELIMINAR",
                        "...<ENTER> PARA CONTINUAR" FOR CHAR enter
                  EXIT INPUT
                END IF

            ON KEY(CONTROL-V)
               LET arr_c = ARR_CURR()

               LET v_ejecuta = "fglgo RETM810 ",
                  reg_2_array[arr_c].n_seguro CLIPPED, " ",
                  reg_2_array[arr_c].consecutivo CLIPPED, " ", 'C', 0 CLIPPED
               RUN v_ejecuta

            ON KEY (INTERRUPT)
                DISPLAY "" AT 20,1
                CLEAR FORM
                EXIT INPUT

            ON KEY (CONTROL-C)
                DISPLAY "" AT 20,1
                CLEAR FORM
                EXIT INPUT
        END INPUT
    ELSE
        ERROR " NO EXISTEN REGISTROS "
    END IF

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_liquida()

   DEFINE la_liq              ARRAY[100] OF RECORD
          marca               CHAR(01)     ,
          n_seguro            CHAR(11)     ,
          consecutivo         CHAR(10)     ,
          nombre              CHAR(43)     ,
          fecha_solic         DATE         ,
          monto_en_pesos      DECIMAL(22,6),
          tipo_ret            SMALLINT    
          END RECORD
  DEFINE la_cons              ARRAY[100] OF RECORD
          consecutivo         DECIMAL(10,0)
  END RECORD

   DEFINE lr_reg_vol_sel      RECORD
          n_seguro            CHAR(11)     ,#n_seguro
          consecutivo         DECIMAL(10,0),
          paterno             CHAR(40)     ,#paterno
          materno             CHAR(40)     ,#materno
          nombres             CHAR(40)     ,#nombres
          tipo_ret            SMALLINT     ,#tipo_ret
          fecha_solic         DATE         ,#fecha_solic
          mto_solic           DECIMAL(16,6) #mto_solic
          END RECORD
   DEFINE ld_mes
         ,ld_anio              SMALLINT     # se agregan variables para validacion de inpc MLM-1886

   DEFINE ld_fecha_liquida    DATE

   DEFINE ls_pos                    INTEGER
   DEFINE arr_c                     INTEGER
   DEFINE li_consecutivo            INTEGER
   DEFINE ls_tipo_ret               SMALLINT
   DEFINE ls_contador               SMALLINT
   DEFINE ls_cont_total             SMALLINT
   DEFINE ls_cont_parcial           SMALLINT
   DEFINE ls_liquida                SMALLINT
   DEFINE ls_marcados               SMALLINT
   DEFINE ld_monto_total_acc        DECIMAL(16,6)
   DEFINE ls_pos_scr                SMALLINT
   DEFINE ls_estado_liq             SMALLINT
   DEFINE v_existe_voluntaria_app   SMALLINT  #CPL-3212
   DEFINE v_existe_mov_123          SMALLINT
   DEFINE v_nss_paso                CHAR(11)
   DEFINE v_mensaje_debug           CHAR(200)

   DEFINE v_n_folio                 LIKE afi_mae_afiliado.n_folio    
   DEFINE v_tipo_solicitud          LIKE afi_mae_afiliado.tipo_solicitud
   DEFINE v_respuesta               SMALLINT
   DEFINE v_diagnostico             SMALLINT
   DEFINE vl_descripcion            CHAR(100)
   DEFINE v_porcentaje              LIKE afi_ctr_beneficiario.porcentaje_total
   DEFINE v_tipo_benef              LIKE afi_ctr_beneficiario.tipo_beneficiario    
   DEFINE v_tramite                 LIKE afi_ctr_beneficiario.tramite      
   DEFINE v_ind_designacion         LIKE afi_ctr_beneficiario.ind_designacion   

   -----------------------------------------------------------------------------

   INITIALIZE lr_reg_vol_sel.* TO NULL

   LET INT_FLAG           = FALSE
   LET ld_monto_total_acc = 0

   LET ls_pos = 0

   MESSAGE " BUSCANDO INFORMACION... POR FAVOR ESPERE!" ATTRIBUTE(REVERSE)


   -- Valida que si existen solicitudes de retiro por beneficiarios sumen los porcentajes el 100%
   CALL f_valida_porcentajes_beneficiarios()
   --------------------------------------------------------------------------
   --SELECCIONA EN ARREGLO SOLICITUDES DE VOL CBF COMPLEMENTARIAS A LIQUIDAR
   --------------------------------------------------------------------------
   DECLARE cur_1 CURSOR FOR
   SELECT  n_seguro    ,   #n_seguro
           consecutivo ,
           paterno     ,   #paterno
           materno     ,   #materno
           nombres     ,   #nombres
           tipo_ret    ,   #tipo_ret
           fecha_solic ,   #fecha_solic
           mto_solic       #mto_solic
   FROM    ret_cta_vol
   WHERE   tipo_ret  IN(1,2,3,4,6,7,8,9,10,11,12,13,14,15)  #tramites complementarias + Vol CBF # MLM-3150  ###CPL-3203 (13 y 14)
--   AND   estado    =   gr_estados.capturado                                -- CPL-2824   
   AND     estado    IN (gr_estados.capturado, gr_estados.confirmado)        -- CPL-2824
   ORDER BY n_seguro, consecutivo
     

   FOREACH cur_1 INTO lr_reg_vol_sel.*

       LET ls_pos = ls_pos + 1

       LET la_liq[ls_pos].n_seguro       = lr_reg_vol_sel.n_seguro
       LET la_liq[ls_pos].nombre[1,26]   = lr_reg_vol_sel.paterno CLIPPED," ",
                                           lr_reg_vol_sel.materno CLIPPED," ",
                                           lr_reg_vol_sel.nombres CLIPPED
       LET la_liq[ls_pos].tipo_ret       = lr_reg_vol_sel.tipo_ret
       LET la_liq[ls_pos].consecutivo    = lr_reg_vol_sel.consecutivo
       LET la_cons[ls_pos].consecutivo   = lr_reg_vol_sel.consecutivo

      IF lr_reg_vol_sel.tipo_ret  = 2 THEN
         CALL f_calcula_mto_solic(lr_reg_vol_sel.n_seguro,lr_reg_vol_sel.tipo_ret,lr_reg_vol_sel.mto_solic)
         RETURNING la_liq[ls_pos].monto_en_pesos
      ELSE
         IF lr_reg_vol_sel.tipo_ret  = 15 THEN
            -- Obtiene el porcentaje que le corresponde por beneficiario
            CALL f_calcula_mto_solic_benef(lr_reg_vol_sel.n_seguro,lr_reg_vol_sel.consecutivo) 
            RETURNING la_liq[ls_pos].monto_en_pesos
         ELSE
            LET  la_liq[ls_pos].monto_en_pesos = lr_reg_vol_sel.mto_solic
         END IF
      END IF

       LET la_liq[ls_pos].fecha_solic    = lr_reg_vol_sel.fecha_solic

   END FOREACH

   MESSAGE ""

   IF ls_pos = 0 THEN
      ERROR " NO HAY SOLICITUDES DE COMPLEMENTARIAS A LIQUIDAR "
      RETURN
   END IF

   CLEAR FORM

   OPEN WINDOW wliquida AT 2,2 WITH FORM "RETM0112" ATTRIBUTE(BORDER)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " <ESC> Liquidar          <Ctrl-B> Detalle De Acciones          <Ctrl-C> Salir "
                                    AT 1,1  ATTRIBUTE(REVERSE)
   DISPLAY " RETM011          LIQUIDACION DE APORTACIONES COMPLEMENTARIAS Y VOL "
                                    AT 3,1  ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY "   AT 3,68 ATTRIBUTE(REVERSE)
   DISPLAY "FECHA DE LIQUIDACION :" AT 19,9

   --DESPLIEGA MONTO TOTAL A LIQUIDAR EN CEROS
   DISPLAY ld_monto_total_acc TO total_en_acciones

   CALL SET_COUNT(ls_pos)

   -------------------------------------------------------
   --DESPLIEGA SOLICITUDES CAPTURADAS Y PERMITE SELECCION
   -------------------------------------------------------
   INPUT ARRAY la_liq WITHOUT DEFAULTS FROM scr_1.*           ATTRIBUTES(MAXCOUNT = ls_pos      ,
                                                              CURRENT ROW DISPLAY = "REVERSE")
      ----------
      BEFORE ROW
      ----------
         LET arr_c = ARR_CURR()
         LET gc_mensaje = "-->",f_obten_desc_retiro(la_liq[arr_c].tipo_ret)
         MESSAGE gc_mensaje CLIPPED ATTRIBUTE(REVERSE)

      -----------------
      AFTER FIELD marca
      -----------------
         LET arr_c = ARR_CURR()
         LET ld_monto_total_acc = 0

         --ACTUALIZA EL MONTO TOTAL EN PANTALLA
         FOR  i = 1 TO ls_pos
            IF la_liq[i].marca IS NOT NULL THEN
                LET ld_monto_total_acc = ld_monto_total_acc + la_liq[i].monto_en_pesos
            END IF
         END FOR

         DISPLAY ld_monto_total_acc TO total_en_acciones

      ----------
      AFTER ROW
      ----------
         LET arr_c      = ARR_CURR()
         LET ls_pos_scr = SCR_LINE()

         IF la_liq[arr_c].marca = "X" THEN
            --VALIDA QUE LAS CUENTAS DE COMPLEMENTARIAS ESTEN CUADRADAS
            IF f_valida_cta_saldo_comp(la_liq[arr_c].n_seguro, la_liq[arr_c].tipo_ret) = FALSE THEN
               PROMPT "LA CUENTA NO SE PUEDE LIQUIDAR POR NO ENCONTRARSE CUADRADA, PRESIONE ENTER:" FOR CHAR ENTER
               LET la_liq[arr_c].marca = ""
               DISPLAY la_liq[arr_c].marca TO scr_1[ls_pos_scr].marca
            END IF
         END IF
         MESSAGE ""

      -------------------
      ON KEY (CONTROL-B)
      -------------------
         CALL detalle_liq_acc() #dal

      -----------------
      ON KEY(INTERRUPT)
      -----------------
         EXIT INPUT

      ---------------
      ON KEY(ACCEPT)
      ---------------
         --VALIDA SI HAY AL MENOS UN NSS MARCADO
         LET ls_marcados = 0

         FOR i=1 TO ls_pos
             IF la_liq[i].marca = "X" THEN
                LET ls_marcados = ls_marcados + 1
                CONTINUE FOR
             END IF
         END FOR

         IF ls_marcados = 0 THEN
            ERROR " NO HA SELECCIONADO REGISTROS A LIQUIDAR "
            CONTINUE INPUT
         ELSE
            EXIT INPUT
         END IF

   END INPUT

   ----------------------------------------
   --SE VALIDA LA CANCELACION/INTERRUPCION
   ----------------------------------------
   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLOSE WINDOW wliquida
      ERROR " OPERACION CANCELADA POR EL USUARIO "
      RETURN
   END IF

   ---------------------------------------------------------------------
   -- CAPTURA FECHA DE LIQUIDACION Y LE ASIGNA FECHA ACTUAL PARA LIQUIDAR
   ---------------------------------------------------------------------
   LET ld_fecha_liquida = HOY

   INPUT ld_fecha_liquida WITHOUT DEFAULTS FROM fecha_liquidacion

      ------------------------------
      AFTER FIELD fecha_liquidacion
      ------------------------------
         IF ld_fecha_liquida IS NULL THEN
             ERROR " LA FECHA DE LIQUIDACION NO PUDE SER NULA "
             NEXT FIELD fecha_liquidacion
         END IF

      -------------
      ON KEY (ESC)
      -------------
         IF ld_fecha_liquida IS NULL THEN
            ERROR " LA FECHA DE LIQUIDACION NO PUDE SER NULA "
            NEXT FIELD fecha_liquidacion

         END IF

         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      CLOSE WINDOW wliquida
      ERROR " OPERACION CANCELADA "
      RETURN
   END IF

   --------------------------------------------------------
   --SE CONFIRMA LA EJECUCION DEL PROCESO DE LIQUIDACION
   --------------------------------------------------------

   WHILE TRUE

      LET gc_mensaje = " ",ls_marcados USING "<<<<" ," CUENTA(S) SELECCIONADA(S), DESEA APLICAR LA LIQUIDACION (S/N):"

      PROMPT gc_mensaje CLIPPED FOR CHAR enter
      CASE
         WHEN enter MATCHES "[Ss]"
              EXIT WHILE
         WHEN enter MATCHES "[Nn]"
              CLOSE WINDOW wliquida
              RETURN
     END CASE
   END WHILE

   ----------------------------------------
   --OBTIENE NUEVO FOLIO PARA LIQUIDACION
   ----------------------------------------

   SELECT nvl(MAX(folio)+ 1,1)
   INTO   g_ultimo_folio
   FROM   glo_folio

   IF SQLCA.SQLCODE = 0 THEN
     INSERT INTO glo_folio VALUES (g_ultimo_folio)
   ELSE
     PROMPT "NO SE ASIGNO CORRECTAMENTE EL FOLIO PARA LIQUIDACION, VERIFIQUE CON EL AREA DE SISTEMAS" FOR CHAR enter
     EXIT PROGRAM
   END IF

   LET ls_contador     = 0
   LET ls_cont_total   = 0
   LET ls_cont_parcial = 0
   LET v_nss_paso      = "X"

   ------------------------------------------------------------------------------
   --SELECCIONA CADA UNA DE LAS SOLICITUDES MARCADAS EN EL ARREGLO PARA LIQUIDAR
   ------------------------------------------------------------------------------
   FOR i = 1 TO ls_pos
      IF la_liq[i].marca = "X" THEN
--         LET v_mensaje_debug = "                                     INFO -     Procesando el nss: ", la_liq[i].n_seguro, " consecutivo:", la_liq[i].consecutivo
--         CALL ERRORLOG (v_mensaje_debug)
         UPDATE ret_cta_vol
         SET    ret_cta_vol.estado      = 7
         WHERE  ret_cta_vol.n_seguro    = la_liq[i].n_seguro
         AND    ret_cta_vol.consecutivo = la_cons[i].consecutivo
         AND    ret_cta_vol.estado IN( 0, 3 )  ------------------CAPTURADO
         IF la_liq[i].tipo_ret = 15 THEN 
             UPDATE ret_cta_vol
             SET    ret_cta_vol.mto_solic   = la_liq[i].monto_en_pesos
             WHERE  ret_cta_vol.n_seguro    = la_liq[i].n_seguro
             AND    ret_cta_vol.consecutivo = la_cons[i].consecutivo
         END IF

        --CPL-3212 SE VALIDA SI ES UN RETIRO POR APP
        SELECT COUNT(*)
        INTO v_existe_voluntaria_app
        FROM ret_detalle_av_op19
        WHERE consecutivo = la_cons[i].consecutivo
        AND nss = la_liq[i].n_seguro
        AND codigo_resultado = '01'

--        SELECT COUNT(*)
--        INTO v_existe_mov_123
--        FROM   dis_cuenta      a,
--               int_det_vol_rc  b
--        WHERE  b.nss = la_liq[i].n_seguro
--        AND    a.nss              = b.nss
--        AND    a.tipo_movimiento  = 123
--        AND    a.folio            = b.folio
        --AND    a.consecutivo_lote = b.consecutivo
--        AND    b.cve_rc           = '032'
--        AND    a.monto_en_pesos   = b.monto

         --RESPALDA LOS APORTES DEL NSS ANTES DE REALIZAR CUALQUIER OPERACION
--         LET v_mensaje_debug = "                                     INFO -     NSS: ", la_liq[i].n_seguro, " consecutivo:", la_liq[i].consecutivo, " Tipo ret:",la_liq[i].tipo_ret, " NSS paso:",v_nss_paso
--         CALL ERRORLOG (v_mensaje_debug)
         
         IF la_liq[i].tipo_ret <> 15 OR (la_liq[i].tipo_ret = 15 AND la_liq[i].n_seguro <> v_nss_paso) THEN
--             LET v_mensaje_debug = "                                     INFO -     Creando Historia el nss: ", la_liq[i].n_seguro, " consecutivo:", la_liq[i].consecutivo
--             CALL ERRORLOG (v_mensaje_debug)
 
             CALL f_respalda_aportes(la_liq[i].n_seguro,g_ultimo_folio,la_liq[i].tipo_ret)
             CALL  f_historia(la_liq[i].n_seguro)     #cpl-2112
             LET v_nss_paso = la_liq[i].n_seguro 
         END IF

         --CONFORME AL RETIRO SELECCIONA LA RUTINA DE LIQUIDACION CORRESPONDIENTE
         CASE
            WHEN la_liq[i].tipo_ret = 1  OR #MLM-3150
                 la_liq[i].tipo_ret = 3  OR
                 la_liq[i].tipo_ret = 7  OR
                 la_liq[i].tipo_ret = 9  OR
                 la_liq[i].tipo_ret = 11 OR 
                 la_liq[i].tipo_ret = 13 OR   ##CPL-3203
                 la_liq[i].tipo_ret = 15
                     LET v_mensaje_debug = "                                     INFO -     liquidando parcial el nss: ", la_liq[i].n_seguro, " consecutivo:", la_liq[i].consecutivo, " tipo_retiro = ",la_liq[i].tipo_ret
                     CALL ERRORLOG (v_mensaje_debug)

                 IF v_existe_voluntaria_app > 0 THEN
                    --LIQUIDA RETIRO PARCIAL APP -- se manda llamar la misma de retiros normales
                    -- porque ya no importa el origen de la aportación CPL-3645
                     LET ls_estado_liq   = f_liquida_parciales(la_liq[i].n_seguro  ,
                                                               la_liq[i].tipo_ret  ,
                                                               ld_fecha_liquida,
                                                               la_liq[i].consecutivo)

                 ELSE 
                     -- LIQUIDA RETIRO PARCIAL
--                     LET v_mensaje_debug = "                                     INFO -     liquidando parcial el nss: ", la_liq[i].n_seguro, " consecutivo:", la_liq[i].consecutivo
--                     CALL ERRORLOG (v_mensaje_debug)
                     
                     LET ls_estado_liq   = f_liquida_parciales(la_liq[i].n_seguro  ,
                                                               la_liq[i].tipo_ret  ,
                                                               ld_fecha_liquida,
                                                               la_liq[i].consecutivo)
                 END IF
                 
                 IF ls_estado_liq = TRUE THEN
                    LET ls_cont_parcial = ls_cont_parcial + 1
                 END IF

            WHEN la_liq[i].tipo_ret = 2  OR       #MLM-3150
                 la_liq[i].tipo_ret = 4  OR
                 la_liq[i].tipo_ret = 6  OR       #MLM-3420
                 la_liq[i].tipo_ret = 8  OR
                 la_liq[i].tipo_ret = 10 OR
                 la_liq[i].tipo_ret = 12 OR
                 la_liq[i].tipo_ret = 14          ##CPL-3203

                 -- LIQUIDA RETIRO TOTAL
                 LET ls_estado_liq = f_liquida_totales(la_liq[i].n_seguro      ,
                                                       la_liq[i].tipo_ret      ,
                                                       ld_fecha_liquida)
                 IF ls_estado_liq = TRUE THEN
                    LET ls_cont_total = ls_cont_total + 1
                 END IF
         END CASE

--         LET v_mensaje_debug = "                                     INFO -     Regresa de la liquidación nss: ", la_liq[i].n_seguro, " consecutivo:", la_liq[i].consecutivo, " Estado ", ls_estado_liq
--         CALL ERRORLOG (v_mensaje_debug)

         IF ls_estado_liq = FALSE THEN
            OPEN WINDOW w_aux1 AT 10,11 WITH 6 ROWS, 60 COLUMNS ATTRIBUTE(BORDER)
               DISPLAY "    NO SE PUDO LIQUIDAR LA SOLICITUD DEL NSS ",
                       la_liq[i].n_seguro, "    " AT 2,1  ATTRIBUTE(REVERSE)
               DISPLAY "          POR FAVOR, NOTIFIQUE AL AREA DE SISTEMAS          "
                                                  AT 4,1 ATTRIBUTE(REVERSE)
               PROMPT  "                         <ACEPTAR> " FOR CHAR enter
            CLOSE WINDOW w_aux1
         ELSE
            
            --ACTULIZA SOLICITUD A ESTADO LIQUIDADO
            UPDATE ret_cta_vol
            SET    ret_cta_vol.n_folio_liq = g_ultimo_folio ,
                   ret_cta_vol.estado      = gr_estados.liquidado,
                   ret_cta_vol.fecha_ult_ret = TODAY                     #mlm-3150
            WHERE  ret_cta_vol.n_seguro    = la_liq[i].n_seguro
            AND    ret_cta_vol.consecutivo = la_liq[i].consecutivo
            AND    ret_cta_vol.estado      = 7

            IF SQLCA.SQLCODE < 0 THEN
               LET gc_mensaje = "NO SE PUDO ACTUALIZAR SOLICITUD DEL NSS ",
                                la_liq[i].n_seguro
               CALL ERRORLOG(gc_mensaje CLIPPED)
               CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
               PROMPT gc_mensaje FOR CHAR ENTER
               EXIT PROGRAM
            END IF
            -- ACTUALIZA EL FOLIO DE LAS LIQUIDADAS DE BENEFICIARIOS
            IF la_liq[i].tipo_ret = 15 THEN
                UPDATE ret_ctr_benef
                SET    folio_liquida = g_ultimo_folio
                WHERE  nss               = la_liq[i].n_seguro
                AND    consecutivo_solic = la_liq[i].consecutivo
                -- Se obtiene el tipo de beneficiario para determinar si se inserta en el maestro o no
                SELECT tipo_benef
                INTO   v_tipo_benef
                FROM   ret_ctr_benef
                WHERE  nss               = la_liq[i].n_seguro
                AND    consecutivo_solic = la_liq[i].consecutivo
                IF v_tipo_benef = 1 THEN
                    LET v_tipo_benef = 3
                    LET v_tramite = 4
                    LET v_porcentaje = 100
                    LET v_ind_designacion = NULL
                    SELECT n_folio, tipo_solicitud
                    INTO   v_n_folio, v_tipo_solicitud
                    FROM   afi_mae_afiliado
                    WHERE  n_seguro = la_liq[i].n_seguro
                    EXECUTE eje_maestro_benef USING v_n_folio,
                                              v_tipo_solicitud,
                                              v_tipo_benef,
                                              v_tramite,
                                              v_porcentaje,
                                              v_ind_designacion,
                                              g_usuario
                                        INTO  v_respuesta, v_diagnostico, vl_descripcion
                
                END IF
            END IF 
            CALL fn_desmarca_cuenta (la_liq[i].n_seguro,la_cons[i].consecutivo)
            IF la_liq[i].tipo_ret = 15 THEN
                CALL f_inserta_detalle_benef(la_liq[i].n_seguro,la_cons[i].consecutivo,g_ultimo_folio) 
                RETURNING ls_estado_liq  
            END IF 
         END IF

      END IF

   END FOR  --SIGUIENTE REGISTRO DE LA LISTA

   CLEAR WINDOW wliquida

   ---------------------------------
   --IMPRESION DE CIFRAS DE CONTROL
   ---------------------------------
   DISPLAY "                   LIQUIDACION APORTACIONES COMPLEMENTARIAS                   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                              CIFRAS DE CONTROL                               " AT 3,1 ATTRIBUTE(REVERSE)

   LET ls_contador = ls_cont_parcial + ls_cont_total

   IF ls_contador = 0 THEN
      ----NO HUBO LIQUIDACION SE DEPURA INFORMACION Y SE LIBERA FOLIO
      WHENEVER ERROR CONTINUE

         DELETE FROM glo_folio
         WHERE  FOLIO =  g_ultimo_folio
      WHENEVER ERROR STOP

      PROMPT " PROCESO FINALIZADO, PRESIONE <ENTER> PARA CONTINUAR: " FOR CHAR enter
   ELSE
      DISPLAY "FOLIO GENERADO                       : ",g_ultimo_folio    AT 8, 5
      DISPLAY "SOLICITUDES RETIRO PARCIAL LIQUIDADAS: ",ls_cont_parcial   AT 10,5
      DISPLAY "SOLICITUDES RETIRO TOTAL LIQUIDADAS  : ",ls_cont_total     AT 12,5
      DISPLAY "SOLICITUDES TOTALES PROCESADAS       : ",ls_contador       AT 16,5

      PROMPT " PROCESO FINALIZADO, DESEA VER RESUMEN (S/N): " FOR CHAR enter

      IF enter MATCHES "[sS]" THEN
         --MUESTRA EL RESUMEN DEL FOLIO
         CALL f_resumen_liquidacion(g_ultimo_folio)
      END IF
   END IF

   CLOSE WINDOW wliquida

END FUNCTION
#==============================================================================#
# Rutina principal para liquidacion de solicitudes de retiro total ya sea SBF  #
# o CBF                                                                        #
#==============================================================================#
FUNCTION f_liquida_totales(pc_nss, ps_tipo_ret, pd_fecha_liquidacion)

   DEFINE pc_nss                CHAR(11)
   DEFINE ps_tipo_ret           SMALLINT
   DEFINE pd_fecha_liquidacion  DATE

   DEFINE lr_cta_saldo_vol      RECORD LIKE cta_saldo_vol.*
   DEFINE lr_ret_cta_vol        RECORD LIKE ret_cta_vol.*


   DEFINE ls_cod_sief_subta03   SMALLINT
   DEFINE ls_cod_sief_subta10   SMALLINT
   DEFINE ls_cod_sief_subta22   SMALLINT
   DEFINE ls_cod_sief_subta23   SMALLINT
   DEFINE ls_cod_sief_subta11   SMALLINT
   DEFINE ls_cod_sief_subta12   SMALLINT
   DEFINE ls_cod_sief_subta24   SMALLINT
   DEFINE ls_cod_sief_subta25   SMALLINT
   DEFINE ls_cod_sief_subta15   SMALLINT

   DEFINE lc_instruccion        CHAR(5000)
   DEFINE lc_sub                CHAR(100)
   DEFINE ls_siefore_act         SMALLINT

   -----------------------------------------------------------------------------

   --RECUPERA LAS SIEFORES DE LAS SUBCUENTAS DE COMPLEMENTARIAS DE LA CUENTA
   CALL f_obten_siefores_comp(pc_nss) RETURNING ls_cod_sief_subta03,    #MLM-3150
                                                ls_cod_sief_subta10,
                                                ls_cod_sief_subta22,
                                                ls_cod_sief_subta23,
                                                ls_cod_sief_subta11,
                                                ls_cod_sief_subta12,
                                                ls_cod_sief_subta24,
                                                ls_cod_sief_subta25,
                                                ls_cod_sief_subta15

   -----------------------------------
   -- SE RECUPERA SOLICITUD DE RETIRO
   -----------------------------------
   WHENEVER ERROR CONTINUE

   SELECT *
   INTO   lr_ret_cta_vol.*
   FROM   ret_cta_vol
   WHERE  n_seguro = pc_nss
     AND  tipo_ret = ps_tipo_ret
--   AND  estado   = 0   --capturada                  -- CPL-2824
     AND  estado   = 7 -- capturada, confirmadoa -- CPL-2824

   IF SQLCA.SQLCODE < 0 THEN
      CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
      PROMPT "SE PRESENTO UN ERROR EN LA CONSULTA DE SOLICITUD, NOTIFIQUE A SISTEMAS:" FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   IF SQLCA.SQLCODE = NOTFOUND THEN
      LET gc_mensaje = "NO SE LOCALIZO LA SOLICITUD PARA EL NSS ", pc_nss
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   WHENEVER ERROR STOP

   -------------------------------------------------------------------
   --DETERMINA LAS SUBCUENTAS A SELECCIONAR SEGUN EL TIPO DE RETIRO
   -------------------------------------------------------------------

   CASE
          WHEN ps_tipo_ret = 2    #MLM-3150
               LET lc_sub = "(3,10)"
               LET ls_siefore_act = ls_cod_sief_subta03
          WHEN ps_tipo_ret = 4
               LET lc_sub = "(22,23)"
               LET ls_siefore_act = ls_cod_sief_subta23
          WHEN ps_tipo_ret = 6      #MLM-3420
               LET lc_sub = "(3,10,22,23)"
               LET ls_siefore_act = ls_cod_sief_subta10
          WHEN ps_tipo_ret = 8
               LET lc_sub = "(11,12)"
               LET ls_siefore_act = ls_cod_sief_subta11
          WHEN ps_tipo_ret = 10
               LET lc_sub = "(24,25)"
               LET ls_siefore_act = ls_cod_sief_subta24
          WHEN ps_tipo_ret = 12
               LET lc_sub = "(11,12,24,25)"
               LET ls_siefore_act = ls_cod_sief_subta12
          WHEN ps_tipo_ret = 14
               LET lc_sub = "(15,16)"     
               LET ls_siefore_act = ls_cod_sief_subta15
          OTHERWISE
           PROMPT "TIPO DE RETIRO INCORRECTO PARA LA RUTINA <ENTER>:" FOR CHAR ENTER
           EXIT PROGRAM
      END CASE

      LET lc_instruccion = " SELECT   folio             ,  \n",
                           "          consecutivo_lote  ,  \n",
                           "          nss               ,  \n",
                           "          siefore           ,  \n",
                           "          subcuenta         ,  \n",
                           "          fecha_valor       ,  \n",
                           "          fecha_conversion  ,  \n",
                           "          monto_en_pesos    ,  \n",
                           "          monto_en_acciones ,  \n",
                           "          saldo_acciones    ,  \n",
                           "          fecha_conversion  ,  \n",
                           "          usuario              \n",
                           " FROM     cta_saldo_vol_tmp    \n",
                           "WHERE    nss            =  ?        \n",
                           "AND      saldo_acciones > 0         \n",
                           "AND      subcuenta in ",lc_sub CLIPPED,"       \n",
                           "ORDER BY fecha_conversion ASC       \n"
   -----------------------------------------------------------------------
   -- SELECCION LOS APORTES DE CTA_SALDO_VOL CON SALDO PARA HACER EL PAGO
   -----------------------------------------------------------------------
   PREPARE exe_saldo_vol FROM lc_instruccion

   DECLARE cur_aporte CURSOR FOR exe_saldo_vol

   FOREACH cur_aporte USING pc_nss INTO lr_cta_saldo_vol.*
      --SE APLICA LIQUIDACION TOTAL AL APORTE
      IF f_liquida_aporte_total(lr_cta_saldo_vol.*,lr_ret_cta_vol.*,pd_fecha_liquidacion,ls_siefore_act) = FALSE THEN
         CALL f_reversa_liquidacion_nss(pc_nss,g_ultimo_folio,lr_ret_cta_vol.consecutivo)
           RETURN FALSE

      END IF

   END FOREACH

   RETURN TRUE

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_liquida_aporte_total(pr_cta_saldo_vol, pr_ret_cta_vol, pd_fecha_liquidacion,ls_siefore_act)

   DEFINE pr_cta_saldo_vol         RECORD LIKE cta_saldo_vol.*   --APORTE
   DEFINE pr_ret_cta_vol           RECORD LIKE ret_cta_vol.*     --SOLICITUD
   DEFINE pd_fecha_liquidacion     DATE                          --FECHA DE LIQUIDACION

   DEFINE ld_precio_accion         DECIMAL(16,6)


   DEFINE ld_monto_bruto_acciones  DECIMAL(16,6)
   DEFINE ld_monto_neto_acciones   DECIMAL(16,6)

   DEFINE ld_monto_bruto_pesos     DECIMAL(16,6)
   DEFINE ld_monto_neto_pesos      DECIMAL(16,6)

   --DATOS RETORNADOS POR EL SP
   DEFINE ld_monto_rend_pesos      DECIMAL(16,6)
   DEFINE ld_monto_isr_pesos       DECIMAL(16,6)
   DEFINE ld_monto_ret20_pesos     DECIMAL(16,6)
   DEFINE lc_periodo_apo           CHAR(6)  --PERIODO DE INPC APLICADO EN APORTE
   DEFINE lc_periodo_ret           CHAR(6)  --PERIODO DE INPC APLICADO EN RETIRO
   DEFINE ls_bandera_apo           SMALLINT --0 encontro aporte, 1 selecciono otro
   DEFINE ls_bandera_ret           SMALLINT --0 encontro aporte, 1 selecciono otro
   DEFINE ps_tipo_ret              SMALLINT
   DEFINE ld_monto_rend_acciones   DECIMAL(16,6)
   DEFINE ld_monto_isr_acciones    DECIMAL(16,6)   --ISR 06%
   DEFINE ld_monto_ret20_acciones  DECIMAL(16,6)   --ISR 20%
   DEFINE ld_monto_ret35_acciones  DECIMAL(16,6)   --ISR 35%

   DEFINE lc_instruccion           CHAR(100)
   DEFINE ls_bandera_pension       CHAR(003)
   DEFINE ls_bandera_deduccion     CHAR(003)

   DEFINE ls_tipo_movimiento        SMALLINT
   DEFINE ls_tipo_isr               SMALLINT
   DEFINE ls_siefore_act            SMALLINT
   DEFINE ls_edad                   SMALLINT
   DEFINE ls_valida_5anios          SMALLINT
   DEFINE v_porcentaje_isr          DECIMAL(2,2) 
   DEFINE v_descripcion_porcentaje  CHAR(8)
   DEFINE v_mensaje_porcentaje      CHAR(2)
   DEFINE v_existe_voluntaria_app   CHAR(1)
   DEFINE v_existe_mov_123          CHAR(1)
   DEFINE v_id_aportante            CHAR(11)

   -----------------------------------------------------------------------------

   LET ld_monto_bruto_acciones = 0
   LET ld_monto_bruto_pesos    = 0
   LET ld_monto_neto_acciones  = 0
   LET ld_monto_neto_pesos     = 0
   LET ld_monto_rend_acciones  = 0
   LET ld_monto_rend_pesos     = 0
   LET ld_monto_isr_acciones   = 0
   LET ld_monto_isr_pesos      = 0
   LET ld_monto_ret20_acciones = 0
   LET ld_monto_ret20_pesos    = 0

   -----------------------------------------------------------------------------
   WHENEVER ERROR CONTINUE

   --OBTIENE EL PRECIO_ACCION DE LA SIEFORE Q CORRESPONDE AL APORTE A LIQ
   SELECT precio_del_dia
   INTO   ld_precio_accion
   FROM   glo_valor_accion
   WHERE  codigo_siefore  = ls_siefore_act
   AND    fecha_valuacion = pd_fecha_liquidacion

   IF SQLCA.SQLCODE <0 THEN
      LET gc_mensaje = "NO SE PUDO RECUPERAR PRECIO SIE ", ls_siefore_act USING "<<",
                       " FECHA ",pd_fecha_liquidacion USING "dd/mm/yyyy", " PRESIONE ENTER:"
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   --SE OBTIENE BRUTO EN ACCIONES
   LET ld_monto_bruto_acciones  = pr_cta_saldo_vol.saldo_acciones

   --SE CALCULAN BRUTO EN PESOS
   LET ld_monto_bruto_pesos = ld_monto_bruto_acciones * ld_precio_accion

   IF (pr_cta_saldo_vol.subcuenta = 22   OR 
       pr_cta_saldo_vol.subcuenta = 23 ) OR   --VOL CBF
      (pr_cta_saldo_vol.subcuenta = 24   OR 
       pr_cta_saldo_vol.subcuenta = 25 ) THEN --COM CBF 
      --LET ls_tipo_isr = 2

      LET ls_edad =  f_obten_edad(pr_cta_saldo_vol.nss) ## Se obtiene Edad del NSS
      IF ls_edad >= 65 THEN -- CPL-3221
        LET ls_tipo_isr = 1 --SBF
      ELSE 
        LET ls_tipo_isr = 2 --CBF
        LET v_porcentaje_isr   = 0.20
      END IF 
   ELSE
      LET ls_tipo_isr = 1
   END IF 

   #INICIO CPL-3203
   IF (pr_cta_saldo_vol.subcuenta = 15   OR 
       pr_cta_saldo_vol.subcuenta = 16) THEN

      LET ls_valida_5anios = f_valida_anios5(pr_cta_saldo_vol.fecha_conversion)

      IF ((ls_valida_5anios = 1) OR (ls_valida_5anios IS NULL)) AND pr_ret_cta_vol.tipo_ret <> 15 THEN 
         LET gc_mensaje = "NO CUENTAN CON LA PERMANENCIA DE 5 AÑOS ", ls_siefore_act USING "<<",
                         " FECHA ",pd_fecha_liquidacion USING "dd/mm/yyyy", " PRESIONE ENTER:"
         PROMPT gc_mensaje FOR CHAR ENTER
         EXIT PROGRAM  
      END IF 

      ##Porcentaje para las subcuentas 15 y 16 con 5 años permanencia es de 35% 
      LET ls_tipo_isr = 2 
      LET v_porcentaje_isr   = 0.35
   END IF 
   #FIN    CPL-3203

   CALL f_calculo_ISR(       pr_cta_saldo_vol.fecha_conversion ,
                             pd_fecha_liquidacion     ,
                             pr_cta_saldo_vol.saldo_acciones   ,
                             pr_cta_saldo_vol.saldo_acciones   ,
                             pr_cta_saldo_vol.siefore           ,
                             ls_siefore_act,
                             ls_tipo_isr,
                             v_porcentaje_isr      ##CPL-3203
                            )#cisr
            RETURNING ld_monto_isr_pesos ,
                      ld_monto_rend_pesos
   --SE VALIDA SI APLICARA EL ISR (0.006)
    ## INIcio CPL-3203
    IF ls_tipo_isr = 1 THEN --SBF (Sin Beneficio Fiscal)
        --SOLO SE RESTA ISR
        LET ld_monto_isr_acciones   = ld_monto_isr_pesos    / ld_precio_accion
        LET ld_monto_neto_acciones = ld_monto_bruto_acciones - ld_monto_isr_acciones
        LET ld_monto_neto_pesos    = ld_monto_neto_acciones * ld_precio_accion
    END IF  

    IF ls_tipo_isr = 2 THEN --CBF (Con Beneficio Fiscal)
        --SE APLICA RET "ld_monto_ret20_pesos" el ISR puede ser de (20% y 35%)
        LET ld_monto_ret20_pesos    = ld_monto_isr_pesos
        LET ld_monto_ret20_acciones = ld_monto_ret20_pesos / ld_precio_accion
        LET ld_monto_neto_acciones = ld_monto_bruto_acciones - ld_monto_ret20_acciones
        LET ld_monto_neto_pesos    = ld_monto_neto_acciones * ld_precio_accion 
    END IF
    ## FIN CPL-3203

   -----------------------------------
   --PREPARA LOS MONTOS A REGISTRAR
   -----------------------------------

   LET ld_monto_neto_pesos  = ld_monto_neto_pesos  * -1
   LET ld_monto_isr_pesos   = ld_monto_isr_pesos   * -1
   LET ld_monto_ret20_pesos = ld_monto_ret20_pesos * -1

   LET ld_monto_neto_acciones  = ld_monto_neto_acciones  * -1
   LET ld_monto_isr_acciones   = ld_monto_isr_acciones   * -1
   LET ld_monto_ret20_acciones = ld_monto_ret20_acciones * -1

   IF FGL_GETENV("DEBUG")="1" THEN                                               --DEBUG
      OPEN WINDOW waux AT 2,2 WITH 20 rows, 70 COLUMNS ATTRIBUTE(BORDER)         --DEBUG
         DISPLAY "Funcion liquida aporte total....."                     AT  1,5
         DISPLAY "subcuenta...............= ",pr_cta_saldo_vol.subcuenta AT  3,5 --DEBUG
         DISPLAY "ld_monto_bruto_pesos    = ",ld_monto_bruto_pesos       AT  4,5 --DEBUG
         DISPLAY "ld_monto_neto_pesos     = ",ld_monto_neto_pesos        AT  5,5 --DEBUG
         DISPLAY "ld_monto_isr_pesos      = ",ld_monto_isr_pesos         AT  6,5 --DEBUG
         DISPLAY "ld_monto_ret20_pesos    = ",ld_monto_ret20_pesos       AT  7,5 --DEBUG
         DISPLAY "ld_monto_rend_pesos     = ",ld_monto_rend_pesos        AT  8,5 --DEBUG
                                                                                 --DEBUG
         DISPLAY "ld_monto_bruto_acciones = ",ld_monto_bruto_acciones    AT  9,5 --DEBUG
         DISPLAY "ld_monto_neto_acciones  = ",ld_monto_neto_acciones     AT 10,5 --DEBUG
         DISPLAY "ld_monto_isr_acciones   = ",ld_monto_isr_acciones      AT 11,5 --DEBUG
         DISPLAY "ld_monto_ret20_acciones = ",ld_monto_ret20_acciones    AT 12,5 --DEBUG
         DISPLAY "ld_monto_rend_acciones  = ",ld_monto_isr_acciones      AT 13,5 --DEBUG
         DISPLAY " fecha aporte             = ",pr_cta_saldo_vol.fecha_conversion  AT 14,5
         DISPLAY " siefore aporte           = ",pr_cta_saldo_vol.siefore AT 15,5
         DISPLAY "siefore XCTUAL            = ",ls_siefore_act          AT 16,5
         PROMPT "PRESIONA enter:" FOR CHAR ENTER                                 --DEBUG
      CLOSE WINDOW waux                                                          --DEBUG
   END IF                                                                        --DEBUG


   -----------------------------------
   --INSERTA MOVIMIENTO EN PAGO VOL
   -----------------------------------

   INSERT INTO ret_pago_vol
        VALUES(g_ultimo_folio                    ,    --folio
               pr_cta_saldo_vol.nss              ,    --nss
               pr_ret_cta_vol.consecutivo        ,    --consecutivo
               ld_monto_neto_pesos               ,    --mto_neto en PESOS
               ld_monto_isr_pesos                ,    --mto_retencion en PESOS
               ld_monto_rend_pesos               ,    --mto_rendimiento
               g_usuario                         ,    --usuario
               pr_cta_saldo_vol.fecha_conversion ,    --fecha_aporte
               pd_fecha_liquidacion              )    --fecha_liquidacion

   IF SQLCA.SQLCODE < 0 THEN
      LET gc_mensaje = "NO SE PUDO INSERTAR PAGO VOL nss ", pr_cta_saldo_vol.nss
      CALL ERRORLOG(gc_mensaje CLIPPED)
      CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   -------------------------------------------------------------
   --DETERMINA EL TIPO DE MOVIMIENTO CON BASE AL TIPO DE RETIRO
   -------------------------------------------------------------
   CASE             #mlm-2929
      WHEN pr_ret_cta_vol.tipo_ret = 1 OR
           pr_ret_cta_vol.tipo_ret = 2 OR
           pr_ret_cta_vol.tipo_ret = 3 OR
           pr_ret_cta_vol.tipo_ret = 4 OR       #MLM-3420
           pr_ret_cta_vol.tipo_ret = 6 OR
           pr_ret_cta_vol.tipo_ret =13 OR
           pr_ret_cta_vol.tipo_ret =14 OR
           pr_ret_cta_vol.tipo_ret =15          ##CPL-3203 (13 y 14)

           #Se agrega validacion para determinar si se trata de un retiro voluntario normal o por APP
            
           SELECT 'X'
           INTO v_existe_voluntaria_app
           FROM ret_detalle_av_op19
           WHERE consecutivo = pr_ret_cta_vol.consecutivo
           AND nss = pr_cta_saldo_vol.nss
           AND codigo_resultado = '01'
           GROUP BY 1          

--           SELECT DISTINCT 'X'
--           INTO v_existe_mov_123
--           FROM   dis_cuenta      a,
--                  int_det_vol_rc  b
--           WHERE  b.nss = pr_cta_saldo_vol.nss
--           AND    a.nss              = b.nss
--           AND    a.tipo_movimiento  = 123
--           AND    a.folio            = b.folio
--           --AND    a.consecutivo_lote = b.consecutivo
--           AND    b.cve_rc           = '032'
--           AND    a.monto_en_pesos   = b.monto

           IF v_existe_voluntaria_app ='X' THEN
                #es un retiro voluntario APP
                LET ls_tipo_movimiento = 493
                LET v_id_aportante='RET-APPVOL'
           ELSE
                IF pr_ret_cta_vol.tipo_ret =15 AND 
                   (pr_cta_saldo_vol.subcuenta = 11 OR
                    pr_cta_saldo_vol.subcuenta = 12 OR
                    pr_cta_saldo_vol.subcuenta = 24 OR
                    pr_cta_saldo_vol.subcuenta = 25) THEN 
                   LET ls_tipo_movimiento = 897
                   LET v_id_aportante='RETIRO'
                ELSE 
                   #retiro voluntario normal
                   LET ls_tipo_movimiento = 490
                   LET v_id_aportante='RETIRO'
                END IF
           END IF

      WHEN pr_ret_cta_vol.tipo_ret = 7  OR
           pr_ret_cta_vol.tipo_ret = 8  OR
           pr_ret_cta_vol.tipo_ret = 9  OR
           pr_ret_cta_vol.tipo_ret = 10 OR
           pr_ret_cta_vol.tipo_ret = 11 OR
           pr_ret_cta_vol.tipo_ret = 12
           LET ls_tipo_movimiento = 897
           LET v_id_aportante='RETIRO'

   END CASE

   -------------------------------------------------
   -- INSERTA (liquida) MONTO NETO MOVIMIENTO 897
   -------------------------------------------------

   INSERT INTO dis_cuenta                    --INSERTA MOVIMIENTO DE RETIRO
        VALUES(ls_tipo_movimiento                 ,--tipo_movimiento
               pr_cta_saldo_vol.subcuenta        ,--subcuenta
               ls_siefore_act                    ,--siefore
               g_ultimo_folio                    ,--folio
               pr_ret_cta_vol.consecutivo        ,--consecutivo      -----------CAMBIAR EBN UISR
               pr_cta_saldo_vol.nss              ,--nss
               pr_ret_cta_vol.n_unico            ,--n_unico
               NULL                              ,--folio_sua
               pd_fecha_liquidacion              ,--fecha_pago
               pd_fecha_liquidacion              ,--fecha_valor
               pd_fecha_liquidacion              ,--fecha_conversion
               ld_monto_neto_pesos               ,--monto_en_pesos
               ld_monto_neto_acciones            ,--monto_en_acciones
               ld_precio_accion                  ,--precio_accion
               NULL                              ,--dias_cotizados
               NULL                              ,--sucursal
               v_id_aportante                    ,--id_aportante
               gr_estados.liquidado              ,--estado
               HOY                               ,--fecha_proceso
               g_usuario                         ,--usuario
               HOY                               ,--fecha_archivo
               0                                 )--etiqueta

   IF SQLCA.SQLCODE < 0 THEN
      LET gc_mensaje = "NO SE PUDO INSERTAR MOVIMIENTO NETO PARA EL NSS ",
                       pr_cta_saldo_vol.nss
      CALL ERRORLOG(gc_mensaje CLIPPED)
      CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   ---------------------------------------------------
   --REGISTRA MOVIMIENTO DE ISR 6%    (MOVIMIENTO 10)
   ---------------------------------------------------
   IF ld_monto_isr_acciones != 0 OR ld_monto_ret20_acciones != 0 THEN

      --MANEJO DE ISR 6%
      IF ld_monto_isr_acciones != 0  THEN
         INSERT INTO dis_cuenta              --INSERTA MOVIMIENTO DE RETIRO
              VALUES("10"                              , --tipo_movimiento
                     pr_cta_saldo_vol.subcuenta        , --subcuenta
                     ls_siefore_act                    , --siefore
                     g_ultimo_folio                    , --folio
                     pr_ret_cta_vol.consecutivo        , --consecutivo
                     pr_cta_saldo_vol.nss              , --nss
                     pr_ret_cta_vol.n_unico            , --n_unico
                     NULL                              , --folio_sua
                     pd_fecha_liquidacion              , --fecha_pago
                     pd_fecha_liquidacion              , --fecha_valor
                     pd_fecha_liquidacion              , --fecha_conversion
                     ld_monto_isr_pesos                , --monto_isr_pesos
                     ld_monto_isr_acciones             , --monto_isr_acciones
                     ld_precio_accion                  , --precio_accion
                     NULL                              , --dias_cotizados
                     NULL                              , --sucursal
                     v_id_aportante                    , --id_aportante
                     gr_estados.liquidado              , --estado
                     HOY                               , --fecha_proceso
                     g_usuario                         , --usuario
                     HOY                               , --fecha_archivo
                     0                                 ) --etiqueta

         IF SQLCA.SQLCODE < 0 THEN
            LET gc_mensaje = "NO SE PUDO INSERTAR MOVIMIENTO DE ISR, NSS ",
                              pr_cta_saldo_vol.nss
            CALL ERRORLOG(gc_mensaje CLIPPED)
            CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
            PROMPT gc_mensaje FOR CHAR ENTER
            EXIT PROGRAM
         END IF
      END IF

      --MANEJO DE RETENCION 20% o 35%
      IF ld_monto_ret20_acciones != 0 AND
        (pr_cta_saldo_vol.subcuenta = 22 OR pr_cta_saldo_vol.subcuenta = 23   OR
         pr_cta_saldo_vol.subcuenta = 24 OR pr_cta_saldo_vol.subcuenta = 25   OR 
         pr_cta_saldo_vol.subcuenta = 15 OR pr_cta_saldo_vol.subcuenta = 16) THEN  --CBF ##CPL-3203

         CASE v_porcentaje_isr
            WHEN 0.20  --20% ISR
                LET v_descripcion_porcentaje =  "RETIRO20"
                LET v_mensaje_porcentaje     =  "20%"
            WHEN 0.35  --35% ISR  
                LET v_descripcion_porcentaje =  "RETIRO35"
                LET v_mensaje_porcentaje     =  "35%"
         END CASE 
         --------------------------------------------------
         --INSERTA DIS_CUENTA CON MOV 10 MONTOS DE RET20% O RET35%
         --------------------------------------------------
         INSERT INTO dis_cuenta              --INSERTA MOVIMIENTO DE RETIRO
              VALUES("10"                              , --tipo_movimiento
                     pr_cta_saldo_vol.subcuenta        , --subcuenta
                     ls_siefore_act                    , --siefore
                     g_ultimo_folio                    , --folio
                     pr_ret_cta_vol.consecutivo        , --consecutivo
                     pr_cta_saldo_vol.nss              , --nss
                     pr_ret_cta_vol.n_unico            , --n_unico
                     NULL                              , --folio_sua
                     pd_fecha_liquidacion              , --fecha_pago
                     pd_fecha_liquidacion              , --fecha_valor
                     pd_fecha_liquidacion              , --fecha_conversion
                     ld_monto_ret20_pesos              , --monto_isr_pesos
                     ld_monto_ret20_acciones           , --monto_isr_acciones
                     ld_precio_accion                  , --precio_accion
                     NULL                              , --dias_cotizados
                     NULL                              , --sucursal
                     v_id_aportante                    , --id_aportante
                     gr_estados.liquidado              , --estado
                     HOY                               , --fecha_proceso
                     g_usuario                         , --usuario
                     HOY                               , --fecha_archivo
                     0                                 ) --etiqueta

         IF SQLCA.SQLCODE < 0 THEN
            LET gc_mensaje = "NO SE PUDO INSERTAR MOVIMIENTO DE RETENCION ",v_mensaje_porcentaje,", NSS ",
                              pr_cta_saldo_vol.nss
            CALL ERRORLOG(gc_mensaje CLIPPED)
            CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
            PROMPT gc_mensaje FOR CHAR ENTER
            EXIT PROGRAM
         END IF

      END IF

   END IF

   RETURN TRUE

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_liquida_parciales(pc_nss, ps_tipo_ret, pd_fecha_liquidacion,pd_consecutivo)

   DEFINE pc_nss                   CHAR(11)
   DEFINE ps_tipo_ret              SMALLINT
   DEFINE pd_fecha_liquidacion     DATE
   DEFINE pd_consecutivo           DECIMAL(10,0)

   DEFINE lr_cta_saldo_vol         RECORD LIKE cta_saldo_vol.*
   DEFINE lr_ret_cta_vol           RECORD LIKE ret_cta_vol.*

   DEFINE lc_instruccion           CHAR(2000)
   DEFINE lc_sub                   CHAR(100)

   DEFINE ld_pendiente_pesos       DECIMAL(18,6)
   DEFINE ld_pendiente_acciones    DECIMAL(18,6)
   DEFINE ld_mto_aporte_pesos      DECIMAL(18,6)
   DEFINE ls_siefore_act           SMALLINT
   DEFINE ls_grupo_reg             SMALLINT
   DEFINE ls_id_aporte             INTEGER
   DEFINE v_mensaje_debug          CHAR(200)

   DEFINE v_existe_voluntaria_app  SMALLINT

   DEFINE ld_precio_retiro         LIKE glo_valor_accion.precio_del_dia

   LET lc_sub = ""
   -----------------------------------------------------------------------------
   MESSAGE "LIQUIDANDO RETIRO PARCIAL..." SLEEP 1

   ------------------------------------
   -- SE RECUPERA SOLICITUD DE RETIRO
   ------------------------------------
   WHENEVER ERROR CONTINUE

   SELECT *
   INTO   lr_ret_cta_vol.*
   FROM   ret_cta_vol
   WHERE  n_seguro    = pc_nss
   AND    tipo_ret    = ps_tipo_ret
   AND    consecutivo = pd_consecutivo
   AND    estado      = 7


   IF SQLCA.SQLCODE < 0 THEN
      CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
      PROMPT "SE PRESENTO UN ERROR EN LA CONSULTA DE SOLICITUD, NOTIFIQUE A SISTEMAS:" FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   IF SQLCA.SQLCODE = NOTFOUND THEN
      LET gc_mensaje = "NO SE LOCALIZO LA SOLICITUD PARA EL NSS ", pc_nss
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

--    LET v_mensaje_debug = "                                     INFO -     En liquida parciales: ", pc_nss
--    CALL ERRORLOG (v_mensaje_debug)

   WHENEVER ERROR STOP
   
   IF ps_tipo_ret <=6 THEN
      LET ls_grupo_reg  = 3
   ELSE
       IF ps_tipo_ret = 13 OR ps_tipo_ret = 14 THEN
          LET ls_grupo_reg  = 4
       ELSE
          LET ls_grupo_reg = 2
       END IF
   END IF 
   SELECT codigo_siefore              
   INTO   ls_siefore_act
   FROM   cta_nss_regimen             
   WHERE  nss           = pc_nss       
   AND    grupo_regimen = ls_grupo_reg

   --LET v_mensaje_debug = "                                     INFO -     Siefore Encontrada: ", ls_siefore_act, " tipo_retiro ", ps_tipo_ret, " grupo recuperado ", ls_grupo_reg 
   --CALL ERRORLOG (v_mensaje_debug)
   
   -------------------------------------------------------------------
   --DETERMINA LAS SUBCUENTAS A SELECCIONAR SEGUN EL TIPO DE RETIRO
   -------------------------------------------------------------------
      CASE
          WHEN ps_tipo_ret = 1    #MLM-3150
               LET lc_sub = "(3,10)"
          WHEN ps_tipo_ret = 3
               SELECT COUNT(*)
               INTO v_existe_voluntaria_app
               FROM ret_detalle_av_op19
               WHERE consecutivo = pd_consecutivo
               AND nss = pc_nss
               AND codigo_resultado = '01'
               IF v_existe_voluntaria_app > 0 THEN 
                   LET lc_sub = "(23)"
               ELSE
                   LET lc_sub = "(22,23)"
               END IF 
          WHEN ps_tipo_ret = 7
               LET lc_sub = "(11,12)"
          WHEN ps_tipo_ret = 9
               LET lc_sub = "(24,25)"
          WHEN ps_tipo_ret = 11
               LET lc_sub = "(11,12,24,25)"
          WHEN ps_tipo_ret = 13   ##CPL-3203
               LET lc_sub = "(15,16)"    
          WHEN ps_tipo_ret = 15   ##CPL-3345
               LET lc_sub = "(3,10,22,23,11,12,24,25,15,16)"    
          OTHERWISE
           PROMPT "TIPO DE RETIRO INCORRECTO PARA LA RUTINA <ENTER>:" FOR CHAR ENTER
           EXIT PROGRAM
      END CASE

      LET lc_instruccion = " SELECT   folio             ,  \n",
                           "          consecutivo_lote  ,  \n",
                           "          nss               ,  \n",
                           "          siefore           ,  \n",
                           "          subcuenta         ,  \n",
                           "          fecha_valor       ,  \n",
                           "          fecha_conversion  ,  \n",
                           "          monto_en_pesos    ,  \n",
                           "          monto_en_acciones ,  \n",
                           "          saldo_acciones    ,  \n",
                           "          fecha_conversion  ,  \n",
                           "          usuario           ,  \n",
                           "          id_aporte            \n",
                           " FROM     cta_saldo_vol_tmp    \n",
                           "WHERE    nss            =  ?        \n",
                           "AND      saldo_acciones > 0         \n",
                           "AND      subcuenta in ",lc_sub CLIPPED,"       \n",
                           "ORDER BY fecha_conversion ASC       \n"

   PREPARE exe_saldo_par FROM lc_instruccion

   --DETERMINA EL MONTO EN PESOS POR APLICAR (INDICADO EN LA SOLICITUD)
   LET ld_pendiente_pesos = lr_ret_cta_vol.mto_solic

   -----------------------------------------------------------------------
   -- SELECCION LOS APORTES DE CTA_SALDO_VOL CON SALDO PARA HACER EL PAGO
   -----------------------------------------------------------------------

   DECLARE cur_aporte_par CURSOR FOR exe_saldo_par

   FOREACH cur_aporte_par USING pc_nss INTO lr_cta_saldo_vol.*, ls_id_aporte
      --RECUPERA EL PRECIO DE ACCION DE LA SIEFORE DEL APORTE A LA FECHA DEL RETIRO
      SELECT precio_del_dia
      INTO   ld_precio_retiro
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = pd_fecha_liquidacion
      AND    codigo_siefore  = ls_siefore_act
     --   LET v_mensaje_debug = "                                     INFO -     Saldo acciones: ", lr_cta_saldo_vol.saldo_acciones, " subcuenta:", lr_cta_saldo_vol.subcuenta, " precio: ",ld_precio_retiro, " siefore:", ls_siefore_act, " fecha:", pd_fecha_liquidacion
     --   CALL ERRORLOG (v_mensaje_debug)

      --OBTIENE EL IMPORTE ACTUALIZADO EN PESOS DEL APORTE
      LET ld_mto_aporte_pesos = lr_cta_saldo_vol.saldo_acciones * ld_precio_retiro

     -- LET v_mensaje_debug = "                                     INFO -     pendiente pesos: ", ld_pendiente_pesos, " ld_mto_aporte_pesos: ",ld_mto_aporte_pesos
     -- CALL ERRORLOG (v_mensaje_debug)

      IF ld_pendiente_pesos >  ld_mto_aporte_pesos THEN
         --SE REQUIERE APLICAR EL APORTE COMPLETO
         IF f_liquida_aporte_total(lr_cta_saldo_vol.*,lr_ret_cta_vol.*,pd_fecha_liquidacion,ls_siefore_act) = FALSE THEN
            CALL f_reversa_liquidacion_nss(pc_nss,g_ultimo_folio,lr_ret_cta_vol.consecutivo)
--            LET v_mensaje_debug = "                                     INFO -     se reversa total aportacion: ", pc_nss
--            CALL ERRORLOG (v_mensaje_debug)
           RETURN FALSE

         END IF
         IF lr_ret_cta_vol.tipo_ret = 15 THEN
            --Actualiza cta_saldo_vol_tmp para que no sean considerados con el siguiente beneficiario
            UPDATE cta_saldo_vol_tmp
            SET    saldo_acciones = 0
            WHERE  id_aporte = ls_id_aporte;
--            LET v_mensaje_debug = "                                     INFO -     actualiza cta_saldo_vol_tmp id_aporte: ", ls_id_aporte
--            CALL ERRORLOG (v_mensaje_debug)
         END IF

         --RESTA EL IMPORTE DEL APORTE AL PENDIENTE POR APLICAR
         LET ld_pendiente_pesos = ld_pendiente_pesos - ld_mto_aporte_pesos
      ELSE
         --DETERMINA LA CANTIDAD DE ACCIONES POR APLICAR
         LET ld_pendiente_acciones  = ld_pendiente_pesos / ld_precio_retiro

         --APLICA LA CANTIDAD NECESARIA DEL APORTE INDICADO
         IF f_liquida_aporte_parcial(lr_cta_saldo_vol.*    ,
                                       lr_ret_cta_vol.*      ,
                                       pd_fecha_liquidacion  ,
                                       ld_pendiente_acciones ,   --pendiente por aplicar
                                       ls_siefore_act
                                       ) = FALSE THEN
            CALL f_reversa_liquidacion_nss(pc_nss,g_ultimo_folio,lr_ret_cta_vol.consecutivo)
--            LET v_mensaje_debug = "                                     INFO -     se reversa parcial aportacion: ", pc_nss
--            CALL ERRORLOG (v_mensaje_debug)
            RETURN FALSE
         END IF
         IF lr_ret_cta_vol.tipo_ret = 15 THEN
            --Actualiza cta-saldo_vol_tmp para que no sean considerados con el siguiente beneficiario
            UPDATE cta_saldo_vol_tmp
            SET    saldo_acciones = saldo_acciones - ld_pendiente_acciones
            WHERE  id_aporte = ls_id_aporte;
--            LET v_mensaje_debug = "                                     INFO -     actualiza parcial cta_saldo_vol_tmp id_aporte: ", ls_id_aporte
--            CALL ERRORLOG (v_mensaje_debug)
            
         END IF
        LET ld_pendiente_pesos = 0
      END IF

      IF ld_pendiente_pesos =  0 THEN
         EXIT FOREACH
      END IF

   END FOREACH

   MESSAGE ""

   RETURN TRUE

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_liquida_aporte_parcial( pr_cta_saldo_vol          ,     --aporte
                                   pr_ret_cta_vol            ,     --solicitud
                                   pd_fecha_liquidacion      ,     --fecha retiro
                                   pd_mto_parcial_acciones   ,      --mto ret parcial
                                   ls_siefore_act
                                 )

   DEFINE pr_cta_saldo_vol         RECORD LIKE cta_saldo_vol.*   --APORTE
   DEFINE pr_ret_cta_vol           RECORD LIKE ret_cta_vol.*     --SOLICITUD
   DEFINE pd_fecha_liquidacion     DATE                          --fecha retiro
   DEFINE pd_mto_parcial_acciones  LIKE cta_saldo_vol.monto_en_acciones

   -----------------------------------------------------------------------------
   --NOTA:
   --    la variable  pd_mto_parcial_acciones se aplica para retiros parciales
   --    en caso contrario se aplic pr_cta_saldo_vol.saldo_acciones
   --    (cuando se trata de retiros totales)
   -----------------------------------------------------------------------------

   DEFINE ld_precio_accion         DECIMAL(16,6)

   DEFINE ld_monto_bruto_acciones  DECIMAL(16,6)
   DEFINE ld_monto_neto_acciones   DECIMAL(16,6)

   DEFINE ld_monto_bruto_pesos     DECIMAL(16,6)
   DEFINE ld_monto_neto_pesos      DECIMAL(16,6)

   --DATOS RETORNADOS POR EL SP
   DEFINE ld_monto_rend_acciones   DECIMAL(16,6)
   DEFINE ld_monto_isr_acciones    DECIMAL(16,6)   --ISR 06%
   DEFINE ld_monto_ret20_acciones  DECIMAL(16,6)   --ISR 20%
   DEFINE lc_periodo_apo           CHAR(6)  --PERIODO DE INPC APLICADO EN APORTE
   DEFINE lc_periodo_ret           CHAR(6)  --PERIODO DE INPC APLICADO EN RETIRO
   DEFINE ls_bandera_apo           SMALLINT --0 encontro aporte, 1 selecciono otro
   DEFINE ls_bandera_ret           SMALLINT --0 encontro aporte, 1 selecciono otro

   DEFINE ld_monto_rend_pesos      DECIMAL(16,6)
   DEFINE ld_monto_isr_pesos       DECIMAL(16,6)
   DEFINE ld_monto_ret20_pesos     DECIMAL(16,6)

   DEFINE lc_instruccion           CHAR(100)
   DEFINE lc_bandera_sp            CHAR(003)
   DEFINE ls_bandera_pension       CHAR(003)
   DEFINE ls_tipo_movimiento       SMALLINT
   DEFINE ls_tipo_isr              SMALLINT
   DEFINE ls_siefore_act           SMALLINT
   DEFINE ls_edad                  SMALLINT 
   DEFINE v_porcentaje_isr         DECIMAL(2,2)
   DEFINE ls_valida_5anios         SMALLINT
   DEFINE v_descripcion_porcentaje CHAR(8)
   DEFINE v_mensaje_porcentaje     CHAR(2)
   DEFINE v_existe_voluntaria_app  CHAR(1)
   DEFINE v_id_aportante           CHAR(11)
   DEFINE v_existe_mov_123         CHAR(1)

   -----------------------------------------------------------------------------

   LET ld_monto_bruto_acciones = 0
   LET ld_monto_bruto_pesos    = 0
   LET ld_monto_neto_acciones  = 0
   LET ld_monto_neto_pesos     = 0
   LET ld_monto_rend_acciones  = 0
   LET ld_monto_rend_pesos     = 0
   LET ld_monto_isr_acciones   = 0
   LET ld_monto_isr_pesos      = 0
   LET ld_monto_ret20_acciones = 0
   LET ld_monto_ret20_pesos    = 0

   -----------------------------------------------------------------------------

   WHENEVER ERROR CONTINUE

   --OBTIENE EL PRECIO_ACCION DE LA SIEFORE Q CORRESPONDE AL APORTE A LIQ
   SELECT precio_del_dia
   INTO   ld_precio_accion
   FROM   glo_valor_accion
   WHERE  codigo_siefore  = ls_siefore_act
   AND    fecha_valuacion = pd_fecha_liquidacion

   IF SQLCA.SQLCODE <0 THEN
      LET gc_mensaje = "NO SE PUDO RECUPERAR PRECIO SIE ", ls_siefore_act USING "<<",
                       " FECHA ",pd_fecha_liquidacion USING "dd/mm/yyyy", " PRESIONE ENTER:"
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   --SE OBTIENE BRUTO EN ACCIONES DEPENDIENDO DEL TIPO DE RETIRO
   IF pr_ret_cta_vol.tipo_ret =  1 OR
      pr_ret_cta_vol.tipo_ret =  3 OR
      pr_ret_cta_vol.tipo_ret =  7 OR
      pr_ret_cta_vol.tipo_ret =  9 OR
      pr_ret_cta_vol.tipo_ret = 11 OR
      pr_ret_cta_vol.tipo_ret = 13 OR
      pr_ret_cta_vol.tipo_ret = 15 THEN  ##CPL-3203

      --SE TOMA EL MONTO PARCIAL RECIBIDO COMO ARGUMENTO
      LET ld_monto_bruto_acciones  = pd_mto_parcial_acciones
   ELSE
      LET ld_monto_bruto_acciones  = pr_cta_saldo_vol.saldo_acciones
   END IF

   --SE CALCULAN BRUTO EN PESOS
   LET ld_monto_bruto_pesos = ld_monto_bruto_acciones * ld_precio_accion
   #MLM-3150 inicia

   LET ls_edad =  f_obten_edad(pr_cta_saldo_vol.nss) ## Se obtiene Edad del NSs

   IF (pr_cta_saldo_vol.subcuenta = 22 OR pr_cta_saldo_vol.subcuenta = 23 ) OR   --VOL CBF
      (pr_cta_saldo_vol.subcuenta = 24 OR pr_cta_saldo_vol.subcuenta = 25 ) THEN --COM CBF
      --LET ls_tipo_isr = 2
      
      LET ls_edad =  f_obten_edad(pr_cta_saldo_vol.nss) ## Se obtiene Edad del NSS
      IF ls_edad >= 65 THEN -- CPL-3221
        LET ls_tipo_isr = 1
      ELSE 
        LET ls_tipo_isr = 2
        LET v_porcentaje_isr = 0.20  
      END IF
   ELSE
      LET ls_tipo_isr = 1
   END IF 

   #INICIO CPL-3203
   IF (pr_cta_saldo_vol.subcuenta = 15   OR 
       pr_cta_saldo_vol.subcuenta = 16) THEN

      LET ls_valida_5anios = f_valida_anios5(pr_cta_saldo_vol.fecha_conversion)

      IF ((ls_valida_5anios = 1) OR (ls_valida_5anios IS NULL)) AND pr_ret_cta_vol.tipo_ret <> 15 THEN 
         LET gc_mensaje = "NO CUENTAN CON LA PERMANENCIA DE 5 AÑOS ", ls_siefore_act USING "<<",
                         " FECHA ",pd_fecha_liquidacion USING "dd/mm/yyyy", " PRESIONE ENTER:"
         PROMPT gc_mensaje FOR CHAR ENTER
         EXIT PROGRAM  
      END IF 
      
      ##Porcentaje para las subcuentas 15 y 16 con 5 años permanencia es de 35% 
      LET ls_tipo_isr = 2 
      LET v_porcentaje_isr   = 0.35
   END IF 
   #FIN    CPL-3203

   CALL f_calculo_ISR( pr_cta_saldo_vol.fecha_conversion ,
                             pd_fecha_liquidacion     ,
                             ld_monto_bruto_acciones   ,
                             pr_cta_saldo_vol.saldo_acciones   ,
                             pr_cta_saldo_vol.siefore           ,
                             ls_siefore_act,
                             ls_tipo_isr,
                             v_porcentaje_isr      ##CPL-3203  (20% y 35%)
                            )#cisr
            RETURNING ld_monto_isr_pesos ,
                      ld_monto_rend_pesos


   --SE VALIDA SI APLICARA EL ISR (0.006)
 
     { --SE DEBE APLICAR ISR
      IF ls_tipo_isr = 1 THEN 
         LET ld_monto_isr_acciones  = ld_monto_isr_pesos    / ld_precio_accion
      ELSE
         ##ls_tipo_isr = 2
         LET ld_monto_ret20_pesos     = ld_monto_isr_pesos
         LET  ld_monto_ret20_acciones =  ld_monto_ret20_pesos / ld_precio_accion
      END IF 
      
      IF (pr_cta_saldo_vol.subcuenta = 22 OR pr_cta_saldo_vol.subcuenta = 23  OR
          pr_cta_saldo_vol.subcuenta = 24 OR pr_cta_saldo_vol.subcuenta = 25 ) THEN  --CBF
         --SE APLICA RET 20% e ISR -- CPL-3221
         --LET ld_monto_neto_acciones = ld_monto_bruto_acciones - ld_monto_ret20_acciones
         --LET ld_monto_neto_pesos    = ld_monto_neto_acciones * ld_precio_accion
         
         IF ls_edad >= 65 THEN -- CPL-3221
            --SOLO SE RESTA ISR      
            ## ls_tipo_isr = 1
            LET ld_monto_neto_acciones = ld_monto_bruto_acciones - ld_monto_isr_acciones
            LET ld_monto_neto_pesos    = ld_monto_neto_acciones * ld_precio_accion
         ELSE 
            --SE APLICA RET 20% e ISR
            ## ls_tipo_isr = 2  
            LET ld_monto_neto_acciones = ld_monto_bruto_acciones - ld_monto_ret20_acciones
            LET ld_monto_neto_pesos    = ld_monto_neto_acciones * ld_precio_accion
         END IF

      ELSE
         --SOLO SE RESTA ISR      ## ls_tipo_isr = 1
         LET ld_monto_neto_acciones = ld_monto_bruto_acciones - ld_monto_isr_acciones
         LET ld_monto_neto_pesos    = ld_monto_neto_acciones * ld_precio_accion
     END IF}

    ## INIcio CPL-3203
    IF ls_tipo_isr = 1 THEN --SBF (Sin Beneficio Fiscal)
        --SOLO SE RESTA ISR
        LET ld_monto_isr_acciones  = ld_monto_isr_pesos    / ld_precio_accion
        LET ld_monto_neto_acciones = ld_monto_bruto_acciones - ld_monto_isr_acciones
        LET ld_monto_neto_pesos    = ld_monto_neto_acciones * ld_precio_accion
    END IF  

    IF ls_tipo_isr = 2 THEN --CBF (Con Beneficio Fiscal)
        --SE APLICA RET "ld_monto_ret20_pesos" el ISR puede ser de (20% y 35%)
        LET ld_monto_ret20_pesos    = ld_monto_isr_pesos
        LET ld_monto_ret20_acciones = ld_monto_ret20_pesos / ld_precio_accion
        LET ld_monto_neto_acciones = ld_monto_bruto_acciones - ld_monto_ret20_acciones
        LET ld_monto_neto_pesos    = ld_monto_neto_acciones * ld_precio_accion 
    END IF
    ## FIN CPL-3203

   -----------------------------------
   --PREPARA LOS MONTOS A REGISTRAR
   -----------------------------------

   LET ld_monto_neto_pesos  = ld_monto_neto_pesos  * -1
   LET ld_monto_isr_pesos   = ld_monto_isr_pesos   * -1
   LET ld_monto_ret20_pesos = ld_monto_ret20_pesos * -1

   LET ld_monto_neto_acciones  = ld_monto_neto_acciones  * -1
   LET ld_monto_isr_acciones   = ld_monto_isr_acciones   * -1
   LET ld_monto_ret20_acciones = ld_monto_ret20_acciones * -1

   IF FGL_GETENV("DEBUG")="1" THEN                                               --DEBUG
      OPEN WINDOW waux AT 2,2 WITH 20 rows, 70 COLUMNS ATTRIBUTE(BORDER)         --DEBUG
         DISPLAY "---Funcion: f_liquida_aporacion"                          AT  1,5 --DEBUG
         DISPLAY "- lc_bandera_sp...........= ",lc_bandera_sp              AT  2,5 --DEBUG
         DISPLAY "- subcuenta...............= ",pr_cta_saldo_vol.subcuenta AT  3,5 --DEBUG
         DISPLAY "- ld_monto_bruto_pesos    = ",ld_monto_bruto_pesos       AT  4,5 --DEBUG
         DISPLAY "- ld_monto_neto_pesos     = ",ld_monto_neto_pesos        AT  5,5 --DEBUG
         DISPLAY "- ld_monto_isr_pesos      = ",ld_monto_isr_pesos         AT  6,5 --DEBUG
         DISPLAY "- ld_monto_ret20_pesos    = ",ld_monto_ret20_pesos       AT  7,5 --DEBUG
         DISPLAY "- ld_monto_rend_pesos     = ",ld_monto_rend_pesos        AT  8,5 --DEBUG
                                                                                 --DEBUG
         DISPLAY "- ld_monto_bruto_acciones = ",ld_monto_bruto_acciones    AT  9,5 --DEBUG
         DISPLAY "- ld_monto_neto_acciones  = ",ld_monto_neto_acciones     AT 10,5 --DEBUG
         DISPLAY "- ld_monto_isr_acciones   = ",ld_monto_isr_acciones      AT 11,5 --DEBUG
         DISPLAY "- ld_monto_ret20_acciones = ",ld_monto_ret20_acciones    AT 12,5 --DEBUG
         DISPLAY "- ld_monto_rend_acciones  = ",ld_monto_isr_acciones      AT 13,5 --DEBUG
         DISPLAY " fecha aporte             = ",pr_cta_saldo_vol.fecha_conversion  AT 14,5
         DISPLAY " siefore aporte           = ",pr_cta_saldo_vol.siefore AT 15,5
         DISPLAY "siefore XCTUAL            = ",ls_siefore_act          AT 16,5
         DISPLAY ""
         PROMPT "PRESIONA enter:" FOR CHAR ENTER                                 --DEBUG
      CLOSE WINDOW waux                                                          --DEBUG
   END IF                                                                        --DEBUG


   -----------------------------------
   --INSERTA MOVIMIENTO EN PAGO VOL
   -----------------------------------

   INSERT INTO ret_pago_vol
        VALUES(g_ultimo_folio                    ,    --folio
               pr_cta_saldo_vol.nss              ,    --nss
               pr_ret_cta_vol.consecutivo        ,    --consecutivo
               ld_monto_neto_pesos               ,    --mto_neto en PESOS
               ld_monto_isr_pesos                ,    --mto_retencion en PESOS
               ld_monto_rend_pesos               ,    --mto_rendimiento
               g_usuario                         ,    --usuario
               pr_cta_saldo_vol.fecha_conversion ,    --fecha_aporte
               pd_fecha_liquidacion              )    --fecha_liquidacion

   IF SQLCA.SQLCODE < 0 THEN
      LET gc_mensaje = "NO SE PUDO INSERTAR PAGO VOL nss ", pr_cta_saldo_vol.nss
      CALL ERRORLOG(gc_mensaje CLIPPED)
      CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   -------------------------------------------------------------
   --DETERMINA EL TIPO DE MOVIMIENTO CON BASE AL TIPO DE RETIRO
   -------------------------------------------------------------
   CASE             #mlm-2929
      WHEN pr_ret_cta_vol.tipo_ret = 1 OR
           pr_ret_cta_vol.tipo_ret = 2 OR
           pr_ret_cta_vol.tipo_ret = 3 OR
           pr_ret_cta_vol.tipo_ret = 4 OR
           pr_ret_cta_vol.tipo_ret = 6 OR
           pr_ret_cta_vol.tipo_ret =13 OR
           pr_ret_cta_vol.tipo_ret =14 OR
           pr_ret_cta_vol.tipo_ret =15          ##CPL-3203 (13 y 14)

            #Se agrega validacion para determinar si se trata de un retiro voluntario normal o por APP
           
           SELECT 'X'
           INTO v_existe_voluntaria_app
           FROM ret_detalle_av_op19
           WHERE consecutivo = pr_ret_cta_vol.consecutivo
           AND nss = pr_cta_saldo_vol.nss
           AND codigo_resultado='01'
           GROUP BY 1

--           SELECT DISTINCT 'X'
--           INTO v_existe_mov_123
--           FROM   dis_cuenta      a,
--                  int_det_vol_rc  b
--           WHERE  b.nss = pr_cta_saldo_vol.nss
--           AND    a.nss              = b.nss
--           AND    a.tipo_movimiento  = 123
--           AND    a.folio            = b.folio
--           --AND    a.consecutivo_lote = b.consecutivo
--           AND    b.cve_rc           = '032'
--           AND    a.monto_en_pesos   = b.monto

           IF v_existe_voluntaria_app ='X' THEN
                #es un retiro voluntario APP
                LET ls_tipo_movimiento = 493
                LET v_id_aportante='RET-APPVOL'
           ELSE
                IF pr_ret_cta_vol.tipo_ret =15 AND 
                   (pr_cta_saldo_vol.subcuenta = 11 OR
                    pr_cta_saldo_vol.subcuenta = 12 OR
                    pr_cta_saldo_vol.subcuenta = 24 OR
                    pr_cta_saldo_vol.subcuenta = 25) THEN 
                   LET ls_tipo_movimiento = 897
                   LET v_id_aportante='RETIRO'
                ELSE 
                   #retiro voluntario normal
                   LET ls_tipo_movimiento = 490
                   LET v_id_aportante='RETIRO'
                END IF           
           END IF

      WHEN pr_ret_cta_vol.tipo_ret = 7  OR
           pr_ret_cta_vol.tipo_ret = 8  OR
           pr_ret_cta_vol.tipo_ret = 9  OR
           pr_ret_cta_vol.tipo_ret = 10 OR
           pr_ret_cta_vol.tipo_ret = 11 OR
           pr_ret_cta_vol.tipo_ret = 12
           LET ls_tipo_movimiento = 897
           LET v_id_aportante='RETIRO'

   END CASE
   -------------------------------------------------
   -- INSERTA (liquida) MONTO NETO MOVIMIENTO 897
   -------------------------------------------------

   INSERT INTO dis_cuenta                    --INSERTA MOVIMIENTO DE RETIRO
        VALUES(ls_tipo_movimiento                ,--tipo_movimiento
               pr_cta_saldo_vol.subcuenta        ,--subcuenta
               ls_siefore_act                    ,--siefore
               g_ultimo_folio                    ,--folio
               pr_ret_cta_vol.consecutivo        ,--consecutivo      -----------CAMBIAR EBN UISR
               pr_cta_saldo_vol.nss              ,--nss
               pr_ret_cta_vol.n_unico            ,--n_unico
               NULL                              ,--folio_sua
               pd_fecha_liquidacion              ,--fecha_pago
               pd_fecha_liquidacion              ,--fecha_valor
               pd_fecha_liquidacion              ,--fecha_conversion
               ld_monto_neto_pesos               ,--monto_en_pesos
               ld_monto_neto_acciones            ,--monto_en_acciones
               ld_precio_accion                  ,--precio_accion
               NULL                              ,--dias_cotizados
               NULL                              ,--sucursal
               v_id_aportante                    ,--id_aportante
               gr_estados.liquidado              ,--estado
               HOY                               ,--fecha_proceso
               g_usuario                         ,--usuario
               HOY                               ,--fecha_archivo
               0                                 )--etiqueta

   IF SQLCA.SQLCODE < 0 THEN
      LET gc_mensaje = "NO SE PUDO INSERTAR MOVIMIENTO NETO PARA EL NSS ",
                       pr_cta_saldo_vol.nss
      CALL ERRORLOG(gc_mensaje CLIPPED)
      CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   ---------------------------------------------------
   --REGISTRA MOVIMIENTO DE ISR 6%    (MOVIMIENTO 10)
   ---------------------------------------------------
   IF ld_monto_isr_acciones != 0 OR ld_monto_ret20_acciones !=0 THEN

      --APLICACION DE ISR 6%
      IF ld_monto_isr_acciones != 0 THEN
         INSERT INTO dis_cuenta              --INSERTA MOVIMIENTO DE RETIRO
              VALUES("10"                              , --tipo_movimiento
                     pr_cta_saldo_vol.subcuenta        , --subcuenta
                     ls_siefore_act                    , --siefore
                     g_ultimo_folio                    , --folio
                     pr_ret_cta_vol.consecutivo        , --consecutivo
                     pr_cta_saldo_vol.nss              , --nss
                     pr_ret_cta_vol.n_unico            , --n_unico
                     NULL                              , --folio_sua
                     pd_fecha_liquidacion              , --fecha_pago
                     pd_fecha_liquidacion              , --fecha_valor
                     pd_fecha_liquidacion              , --fecha_conversion
                     ld_monto_isr_pesos                , --monto_isr_pesos
                     ld_monto_isr_acciones             , --monto_isr_acciones
                     ld_precio_accion                  , --precio_accion
                     NULL                              , --dias_cotizados
                     NULL                              , --sucursal
                     v_id_aportante                    , --id_aportante
                     gr_estados.liquidado              , --estado
                     HOY                               , --fecha_proceso
                     g_usuario                         , --usuario
                     HOY                               , --fecha_archivo
                     0                                 ) --etiqueta

         IF SQLCA.SQLCODE < 0 THEN
            LET gc_mensaje = "NO SE PUDO INSERTAR MOVIMIENTO DE ISR, NSS ",
                              pr_cta_saldo_vol.nss
            CALL ERRORLOG(gc_mensaje CLIPPED)
            CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
            PROMPT gc_mensaje FOR CHAR ENTER
            EXIT PROGRAM
         END IF
      END IF

      --MANEJO DE RETENCION 20% O 35%
      IF ld_monto_ret20_acciones != 0 AND
        (pr_cta_saldo_vol.subcuenta = 22 OR pr_cta_saldo_vol.subcuenta = 23   OR
         pr_cta_saldo_vol.subcuenta = 24 OR pr_cta_saldo_vol.subcuenta = 25   OR
         pr_cta_saldo_vol.subcuenta = 15 OR pr_cta_saldo_vol.subcuenta = 16)  OR
         pr_ret_cta_vol.deduccion = "S"  THEN  --CBF

         CASE v_porcentaje_isr
            WHEN 0.20  --20% ISR
                LET v_descripcion_porcentaje =  "RETIRO20"
                LET v_mensaje_porcentaje     =  "20%"
            WHEN 0.35  --35% ISR  
                LET v_descripcion_porcentaje =  "RETIRO35"
                LET v_mensaje_porcentaje     =  "35%"
         END CASE 
         --------------------------------------------------
         --INSERTA DIS_CUENTA CON MOV 10 MONTOS DE RET20% O RET35%
         --------------------------------------------------
         INSERT INTO dis_cuenta              --INSERTA MOVIMIENTO DE RETIRO
              VALUES("10"                              , --tipo_movimiento
                     pr_cta_saldo_vol.subcuenta        , --subcuenta
                     ls_siefore_act                    , --siefore
                     g_ultimo_folio                    , --folio
                     pr_ret_cta_vol.consecutivo        , --consecutivo
                     pr_cta_saldo_vol.nss              , --nss
                     pr_ret_cta_vol.n_unico            , --n_unico
                     NULL                              , --folio_sua
                     pd_fecha_liquidacion              , --fecha_pago
                     pd_fecha_liquidacion              , --fecha_valor
                     pd_fecha_liquidacion              , --fecha_conversion
                     ld_monto_ret20_pesos              , --monto_isr_pesos
                     ld_monto_ret20_acciones           , --monto_isr_acciones
                     ld_precio_accion                  , --precio_accion
                     NULL                              , --dias_cotizados
                     NULL                              , --sucursal
                     v_id_aportante                    , --id_aportante
                     gr_estados.liquidado              , --estado
                     HOY                               , --fecha_proceso
                     g_usuario                         , --usuario
                     HOY                               , --fecha_archivo
                     0                                 ) --etiqueta

         IF SQLCA.SQLCODE < 0 THEN
            LET gc_mensaje = "NO SE PUDO INSERTAR MOVIMIENTO DE RETENCION ",v_mensaje_porcentaje,", NSS ",
                              pr_cta_saldo_vol.nss
            CALL ERRORLOG(gc_mensaje CLIPPED)
            CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
            PROMPT gc_mensaje FOR CHAR ENTER
            EXIT PROGRAM
         END IF

      END IF

   END IF
   RETURN TRUE
END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_reversa_liquidacion()

   DEFINE li_folio                 INTEGER
   DEFINE lc_opcion                CHAR(1)
   DEFINE li_contador              INTEGER
   DEFINE li_contador1             INTEGER
   DEFINE li_contador2             INTEGER
   DEFINE li_contador3             INTEGER
   DEFINE li_contador4             INTEGER
   DEFINE lr_cta_saldo_vol_rev     RECORD LIKE cta_saldo_vol_rev.*
      ,lc_nss                      CHAR(11)
      ,ls_tipo_ret                 SMALLINT
      ,li_correlativo              INTEGER
      ,ld_fecha_ini                DATE
      ,rev_desmarca                CHAR(100)
      ,lc_txt                      CHAR(1000)
      ,lc_marca_ent                SMALLINT
   DEFINE v_consec_unico           INTEGER
   DEFINE v_saldo_acciones         DECIMAL(22,6)

   -----------------------------------------------------------------------------

   OPEN WINDOW wrev AT 2,2 WITH 21 rows, 78 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST -1)

      DISPLAY "                                REVERSO DE LIQUIDACION                                " AT 3,1 ATTRIBUTE(REVERSE)

      PROMPT "INGRESA EL FOLIO A LIQUIDAR: " FOR li_folio

      -------------------------------------------------
      --VALIDA LA EXISTENCIA DE INFORMACION A REVERSAR
      -------------------------------------------------
      SELECT COUNT(UNIQUE nss)
      INTO   li_contador
      FROM   dis_cuenta
      WHERE  folio           = li_folio
      AND    tipo_movimiento IN(493,490, 897)

      IF li_contador = 0 THEN
         PROMPT "NO HAY INFORMACION DEL FOLIO INDICADO: " FOR CHAR enter
         CLOSE WINDOW wrev
         RETURN
      END IF

      LET gc_mensaje = "EL FOLIO A REVERSAR CUENTA CON ", li_contador USING "<<<<<<<<<<<"," CUENTA(S)"
      DISPLAY gc_mensaje AT 10,2

      PROMPT "ESTA SEGURO DE EJECUTAR EL REVERSO (S/N): " FOR CHAR lc_opcion

      DISPLAY "" AT 10,2 --limpia mensaje de notificacion

      IF lc_opcion MATCHES "[Ss]" THEN

         --ELIMINA MOVIMIENTOS DE LIQUIDACION
         DELETE FROM dis_cuenta
         WHERE  folio     = li_folio
         AND    subcuenta IN (3,10,22,23,11,12,24,25,15,16)   --complementarias #MLM-3150 ##CPL-3203 (15 y 16)

         LET li_contador1 = sqlca.sqlerrd[3]

         --ELIMINA INFORMACION DE PAGO
         DELETE FROM ret_pago_vol
         WHERE  folio = li_folio
         AND    nss IN (SELECT n_seguro
                        FROM   ret_cta_vol
                        WHERE  n_folio_liq = li_folio )

         DELETE FROM ret_ctr_benef_det
         WHERE  folio_liquida = li_folio

         LET li_contador2 = sqlca.sqlerrd[3]

         DECLARE cur_saldo_vol_rev CURSOR FOR
             SELECT   saldo_acciones,
                      consec_unico
             FROM     cta_saldo_vol_rev
             WHERE    folio_retiro = li_folio



        FOREACH cur_saldo_vol_rev INTO v_saldo_acciones, v_consec_unico
            UPDATE cta_saldo_vol
            SET    saldo_acciones = v_saldo_acciones
            WHERE  rowid = v_consec_unico
        END FOREACH

        DECLARE cur_rev_marca CURSOR FOR
            SELECT   n_seguro
                    ,tipo_ret
                    ,consecutivo
            FROM     ret_cta_vol
            WHERE    n_folio_liq = li_folio

        FOREACH cur_rev_marca INTO lc_nss,ls_tipo_ret,li_correlativo
        
            LET lc_marca_ent = 140
            LET rev_desmarca = "EXECUTE PROCEDURE reversa_marca('",
                                                                lc_nss       ,"',",
                                                                lc_marca_ent      ,",",
                                                                li_correlativo,
                                                              ")"
            
            PREPARE eje_revmarca140 FROM rev_desmarca
            EXECUTE eje_revmarca140
            LET rev_desmarca = ""
            
            SELECT  fecha_ini    , marca_cod 
            INTO    ld_fecha_ini , g_marca_ent
            FROM    cta_his_marca
            WHERE   nss = lc_nss
            AND     correlativo = li_correlativo

            LET rev_desmarca = "EXECUTE PROCEDURE reversa_desmarca ",
                               "('",lc_nss,
                               "',",g_marca_ent,
                               ",",li_correlativo,
                               ",'",ld_fecha_ini,"')"
                               
            LET rev_desmarca = rev_desmarca CLIPPED

            SLEEP 1

            PREPARE des_desmarca FROM rev_desmarca
            EXECUTE des_desmarca
        END FOREACH

        --ACTUALIZA ESTADO DE LA SOLICITUD
        UPDATE ret_ctr_benef
        SET    folio_liquida = 0
        WHERE  folio_liquida = li_folio
         
        --ACTUALIZA ESTADO DE LA SOLICITUD
        UPDATE ret_cta_vol
        SET    n_folio_liq = NULL,
               estado      = 0   ,
               fecha_ult_ret = NULL  
        WHERE  n_folio_liq = li_folio
        AND    estado      = 8

        LET li_contador3 = sqlca.sqlerrd[3]


        UPDATE cta_saldo_vol_rev
        SET    fecha_reverso    = TODAY
        WHERE  folio_retiro     = li_folio

        SELECT COUNT(*)
        INTO  li_contador4
        FROM  cta_saldo_vol_rev
        WHERE folio_retiro  =   li_folio

        MESSAGE ""

        DISPLAY "REGISTROS ELIMINADOS   EN dis_cuenta   : ", li_contador1 USING "<<<<<<<<" AT 10,10
        DISPLAY "REGISTROS ELIMINADOS   EN ret_pago_vol : ", li_contador2 USING "<<<<<<<<" AT 12,10
        DISPLAY "REGISTROS ACTUALIZADOS EN ret_cta_vol  : ", li_contador3 USING "<<<<<<<<" AT 14,10
        DISPLAY "REGISTROS ACTUALIZADOS EN cta_saldo_vol: ", li_contador4 USING "<<<<<<<<" AT 16,10

        PROMPT "PRESIONE ENTER PARA CONTINUAR:" FOR CHAR ENTER

    END IF

    CLOSE WINDOW wrev

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_errores()

      LET gc_mensaje = ERR_GET(SQLCA.SQLCODE)
      DISPLAY gc_mensaje CLIPPED AT 2,1
      ERROR "SE HA PRESENTADO UN ERROR, NOTIFIQUE A SISTEMAS"
      PROMPT "PRESIONE ENTER PARA CONTINUAR: " FOR CHAR enter
      EXIT PROGRAM

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#

#==============================================================================#
#                                                                              #
#==============================================================================#
#==============================================================================#
#                                                                              #
#==============================================================================#
#==============================================================================#
#                                                                              #
#==============================================================================#

#==============================================================================#
#                                                                              #
#==============================================================================#
#==============================================================================#
#                                                                              #
#==============================================================================#
#==============================================================================#
#                                                                              #
#==============================================================================#
#==============================================================================#
#                                                                              #
#==============================================================================#


#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION precio_accion(p_fecha_valuacion,p_siefore)

   DEFINE p_fecha_valuacion       DATE
   DEFINE p_siefore               SMALLINT
   DEFINE ld_precio_del_dia       LIKE glo_valor_accion.precio_del_dia

   --OBTIENE EL PRECIO DEL DIA DEL CATALOGO
   SELECT precio_del_dia
   INTO   ld_precio_del_dia
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = p_fecha_valuacion
   AND    codigo_siefore  = p_siefore

   IF STATUS = NOTFOUND OR ld_precio_del_dia IS NULL THEN
      PROMPT " NO EXISTE PRECIO DE ACCION PARA LA SIEFORE ",p_siefore USING "<<&", " AL DIA DE ", p_fecha_valuacion USING "DD/MM/YYYY"
      FOR CHAR enter
      EXIT PROGRAM
   END IF

   RETURN ld_precio_del_dia

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION modifica_seleccion(gr_general)

    DEFINE gr_general              RECORD #loc #gr_general
           var_nula                CHAR(1)                             ,
           n_folio                 LIKE afi_mae_afiliado.n_folio       ,
           n_seguro                LIKE afi_mae_afiliado.n_seguro      ,
           n_rfc                   LIKE afi_mae_afiliado.n_rfc         ,
           n_unico                 LIKE afi_mae_afiliado.n_unico       ,
           fentcons                LIKE afi_mae_afiliado.fentcons      ,
           paterno                 LIKE afi_mae_afiliado.paterno       ,
           materno                 LIKE afi_mae_afiliado.materno       ,
           nombres                 LIKE afi_mae_afiliado.nombres       ,

           acc_sbf_sb1             LIKE dis_cuenta.monto_en_acciones   ,
           acc_sbf_s6              LIKE dis_cuenta.monto_en_acciones   ,
           pes_sbf                 LIKE dis_cuenta.monto_en_pesos      ,
           acc_cbf_sb1             LIKE dis_cuenta.monto_en_acciones   ,
           acc_cbf_s6              LIKE dis_cuenta.monto_en_acciones   ,
           pes_cbf                 LIKE dis_cuenta.monto_en_pesos      ,
           pes                     LIKE dis_cuenta.monto_en_pesos      ,

           fecha_ult_ret           LIKE ret_cta_vol.fecha_ult_ret      ,
           n_folio_sol             LIKE ret_cta_vol.n_folio_sol        ,
           tipo_ret                LIKE ret_cta_vol.tipo_ret           ,
           des_tipo_ret            CHAR(40)                            ,
           mto_solic               LIKE ret_cta_vol.mto_solic          ,
           porcentaje_solic        LIKE ret_cta_vol.porcentaje_solic   ,
         --tipo_pago               LIKE ret_cta_vol.tipo_pago          ,
           edad                    LIKE ret_cta_vol.edad               ,
         --des_tipo_pago           CHAR(40)                            ,
           fecha_solic             LIKE ret_cta_vol.fecha_solic        ,
           fecha_captura           LIKE ret_cta_vol.fecha_captura      ,
           ultimo_proceso          LIKE ret_cta_vol.ultimo_proceso     ,
           pension_invalidez       LIKE ret_cta_vol.pension_invalidez  ,
           deduccion               LIKE ret_cta_vol.deduccion          ,
         --fecha_deduccion         LIKE ret_cta_vol.fecha_deduccion    ,
         --mto_deducido            LIKE ret_cta_vol.mto_deducido       ,
           cod_rechazo_ent    LIKE ret_cta_vol.cod_rechazo_ent         ,   #* Rech. *
           consecutivo             INTEGER                             ,
           usuario                 LIKE ret_cta_vol.usuario            ,
           estado                  LIKE ret_cta_vol.estado             ,
           desc_estado             CHAR(30)
           END RECORD

    DEFINE reg_9                   RECORD
           var_nula                CHAR(1)                             ,
           n_folio                 LIKE afi_mae_afiliado.n_folio       ,
           n_seguro                LIKE afi_mae_afiliado.n_seguro      ,
           n_rfc                   LIKE afi_mae_afiliado.n_rfc         ,
           n_unico                 LIKE afi_mae_afiliado.n_unico       ,
           fentcons                LIKE afi_mae_afiliado.fentcons      ,
           paterno                 LIKE afi_mae_afiliado.paterno       ,
           materno                 LIKE afi_mae_afiliado.materno       ,
           nombres                 LIKE afi_mae_afiliado.nombres       ,

           acc_sbf_sb1             LIKE dis_cuenta.monto_en_acciones   ,
           acc_sbf_s6              LIKE dis_cuenta.monto_en_acciones   ,
           pes_sbf                 LIKE dis_cuenta.monto_en_pesos      ,
           acc_cbf_sb1             LIKE dis_cuenta.monto_en_acciones   ,
           acc_cbf_s6              LIKE dis_cuenta.monto_en_acciones   ,
           pes_cbf                 LIKE dis_cuenta.monto_en_pesos      ,
           pes                     LIKE dis_cuenta.monto_en_pesos      ,

           fecha_ult_ret           LIKE ret_cta_vol.fecha_ult_ret      ,
           n_folio_sol             LIKE ret_cta_vol.n_folio_sol        ,
           tipo_ret                LIKE ret_cta_vol.tipo_ret           ,
           des_tipo_ret            CHAR(40)                            ,
           mto_solic               LIKE ret_cta_vol.mto_solic          ,
           porcentaje_solic        LIKE ret_cta_vol.porcentaje_solic   ,
         --tipo_pago               LIKE ret_cta_vol.tipo_pago          ,
           edad                    LIKE ret_cta_vol.edad               ,
         --des_tipo_pago           CHAR(40)                            ,
           fecha_solic             LIKE ret_cta_vol.fecha_solic        ,
           fecha_captura           LIKE ret_cta_vol.fecha_captura      ,
           ultimo_proceso          LIKE ret_cta_vol.ultimo_proceso     ,
           pension_invalidez       LIKE ret_cta_vol.pension_invalidez  ,
           deduccion               LIKE ret_cta_vol.deduccion          ,
         --fecha_deduccion         LIKE ret_cta_vol.fecha_deduccion    ,
         --mto_deducido            LIKE ret_cta_vol.mto_deducido       ,
           cod_rechazo_ent    LIKE ret_cta_vol.cod_rechazo_ent         ,   #* Rech. *
           consecutivo             INTEGER                             ,
           usuario                 LIKE ret_cta_vol.usuario            ,
           estado                  LIKE ret_cta_vol.estado,
           desc_estado             CHAR(30)
    END RECORD

    DEFINE lr_saldos               RECORD
           acc_sbf_sb1             DECIMAL(16,6),
           acc_sbf_s6              DECIMAL(16,6),
           pes_sbf                 DECIMAL(16,6),
           acc_cbf_sb1             DECIMAL(16,6),
           acc_cbf_s6              DECIMAL(16,6),
           pes_cbf                 DECIMAL(16,6),
           pes                     DECIMAL(16,6)
           END RECORD

    DEFINE cont_1                  SMALLINT
    DEFINE sw_6                    SMALLINT
    DEFINE sw_2                    SMALLINT
    DEFINE monto_paso              LIKE dis_cuenta.monto_en_pesos

    ---------------------------------------------------------------------------

    LET reg_9.* = gr_general.*
    LET sw_2    = 0
    LET sw_6    = 0

    ---------------------------------------------------------------------------

    --OBTIENE SALDOS DE LAS SUBCUENTAS DE APORTACIONES COMPLEMENTARIAS
    CALL f_obten_saldos_comp2(reg_1.n_seguro,reg_9.tipo_ret) RETURNING lr_saldos.*   #MLM-3150

    DISPLAY BY NAME lr_saldos.*

    ------------------------------------------
    --MODIFICA DATOS PREVIAMENTE CARGADOS
    ------------------------------------------
    INPUT BY NAME gr_general.tipo_ret         ,
                  gr_general.n_folio_sol      ,
                  gr_general.mto_solic        ,
                  gr_general.porcentaje_solic ,
                  gr_general.fecha_solic      ,
                  gr_general.pension_invalidez
                  WITHOUT DEFAULTS

        --------------------------
        AFTER FIELD n_folio_sol
        --------------------------
            IF gr_general.n_folio_sol IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD n_folio_sol
            ELSE
                SELECT "OK"
                FROM   ret_cta_vol
                WHERE  n_folio_sol <> reg_9.n_folio_sol
                AND    n_folio_sol  = gr_general.n_folio_sol
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    ERROR "   SOLICITUD YA INGRESADA " ATTRIBUTE(NORMAL)
                    NEXT FIELD n_folio_sol
                END IF
            END IF

        --------------------------
        AFTER FIELD tipo_ret
        --------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_folio_sol
            END IF

            IF gr_general.tipo_ret IS NULL THEN
               --SELECCIONA EL TIPO DE PAGO
               CALL f_selecciona_tipo_ret()
                    RETURNING gr_general.tipo_ret, gr_general.des_tipo_ret

               DISPLAY BY NAME gr_general.tipo_ret
               DISPLAY BY NAME gr_general.des_tipo_ret

               IF gr_general.tipo_ret IS NULL THEN
                  NEXT FIELD  tipo_ret
               END IF
            ELSE
               --RECUPERA LA DESCRIPCION DEL TIPO DE PAGO CAPTURADO
               SELECT des_tipo_ret
               INTO   gr_general.des_tipo_ret
               FROM   tab_retiro_old
               WHERE  tipo_ret = gr_general.tipo_ret
               AND    tipo_ret IN (1,2,3,4,6,7,8,9,10,11,12,13,14,15)      #MLM-3150  MLM-2929 #MLM-3420

               IF STATUS = NOTFOUND THEN
                  ERROR " TIPO DE RETIRO INVALIDO "
                  NEXT FIELD tipo_ret
               ELSE
                  DISPLAY BY NAME gr_general.tipo_ret
                  DISPLAY BY NAME gr_general.des_tipo_ret
               END IF
            END IF

            CALL f_obten_saldos_comp2(reg_1.n_seguro,gr_general.tipo_ret) RETURNING lr_saldos.*
            DISPLAY BY NAME lr_saldos.*

            IF f_valida_cuadre(reg_1.n_seguro, gr_general.tipo_ret) = FALSE THEN
               PROMPT "EL NSS ", reg_1.n_seguro, " NO ESTAN CUADRADAS SUS CUENTAS. " ATTRIBUTE(REVERSE) FOR CHAR enter
            END IF

            --DESPLIEGA EL MONTO QUE CORRESPONDA AL TIPO DE PAGO
            IF gr_general.tipo_ret = 2   OR     #MLM-3150
               gr_general.tipo_ret = 4   OR
               gr_general.tipo_ret = 8   OR
               gr_general.tipo_ret = 10  OR
               gr_general.tipo_ret = 12  OR
               gr_general.tipo_ret = 14 THEN    ##CPL-3203
               --RETIRO TOTAL
               LET gr_general.porcentaje_solic = 100

               CASE gr_general.tipo_ret
                    WHEN 2  LET gr_general.mto_solic = lr_saldos.pes_sbf #MLM-3150
                    WHEN 4  LET gr_general.mto_solic = lr_saldos.pes_cbf
                    WHEN 6  LET gr_general.mto_solic = lr_saldos.pes_cbf    #MLM-3420
                    WHEN 8  LET gr_general.mto_solic = lr_saldos.pes_sbf
                    WHEN 10 LET gr_general.mto_solic = lr_saldos.pes_cbf
                    WHEN 12 LET gr_general.mto_solic = lr_saldos.pes
                    WHEN 14 LET gr_general.mto_solic = lr_saldos.pes_sbf  ##CPL-3203
               END CASE

               DISPLAY BY NAME gr_general.mto_solic        ATTRIBUTE(BOLD)
               DISPLAY BY NAME gr_general.porcentaje_solic ATTRIBUTE(BOLD)
            ELSE
               --RETIRO PARCIAL
               --LET gr_general.porcentaje_solic = ""
               --LET gr_general.mto_solic        = 0
               DISPLAY gr_general.mto_solic        TO mto_solic ATTRIBUTE(BOLD)
               DISPLAY gr_general.porcentaje_solic TO porcentaje_solic
            END IF

            --VALIDA SALDOS CON BENEFICIO FISCAL
            IF lr_saldos.pes = 0 THEN
               ERROR " NO CUENTA CON SALDO PARA REALIZAR EL TRAMITE! "
               NEXT FIELD tipo_ret
            END IF

            --VALIDA SALDOS SIN BENEFICIO FISCAL
            IF(gr_general.tipo_ret  = 1    OR               #MLM-3150
               gr_general.tipo_ret  = 2    OR               #MLM-3150
               gr_general.tipo_ret  = 7    OR
               gr_general.tipo_ret  = 8    OR
               gr_general.tipo_ret  = 13   OR
               gr_general.tipo_ret  = 14 ) AND              ##CPL-3203 (13 y 14)
               lr_saldos.pes_sbf = 0 THEN
               ERROR " NO CUENTA CON SALDO PARA REALIZAR EL TRAMITE!! "
               NEXT FIELD tipo_ret
            END IF

            IF gr_general.tipo_ret = 6    AND               #MLM-3420
               lr_saldos.pes_cbf + lr_saldos.pes_sbf = 0 THEN
               ERROR " NO CUENTA CON SALDO PARA REALIZAR EL TRAMITE!!! "
               NEXT FIELD tipo_ret
            END IF

            --VALIDA SALDOS CON BENEFICIO FISCAL
            IF(gr_general.tipo_ret = 3    OR
               gr_general.tipo_ret = 4    OR
               gr_general.tipo_ret = 9    OR
               gr_general.tipo_ret = 10 ) AND
               lr_saldos.pes_cbf = 0 THEN
               ERROR " NO CUENTA CON SALDO PARA REALIZAR EL TRAMITE!!! "
               NEXT FIELD tipo_ret
            END IF

        -------------------------
        BEFORE FIELD mto_solic
        -------------------------
           --SI ES RETIRO TOTAL NO SE PERMITE LA CAPTURA DEL MONTO o %
           IF gr_general.tipo_ret = 2   OR      #MLM-3150
              gr_general.tipo_ret = 4   OR      #MLM-3150
              gr_general.tipo_ret = 6   OR      #MLM-3420
              gr_general.tipo_ret = 8   OR
              gr_general.tipo_ret = 10  OR
              gr_general.tipo_ret = 12  OR
              gr_general.tipo_ret = 14 THEN     ##CPL-3203
              --RETIRO TOTAL
              NEXT FIELD fecha_solic
           END IF

        --------------------------
        AFTER FIELD mto_solic
        --------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_ret
            END IF

            IF gr_general.mto_solic IS NOT NULL THEN

               --VERIFICA QUE NO SEA MAYOR AL SALDO SIN BENEFICIO FISCAL
               IF (gr_general.tipo_ret = 1 OR gr_general.tipo_ret = 2 OR        #MLM-3150
                   gr_general.tipo_ret = 7 OR gr_general.tipo_ret = 8 AND
                   gr_general.tipo_ret = 13 OR gr_general.tipo_ret = 14) AND     ##CPL-3203
                   gr_general.mto_solic > lr_saldos.pes_sbf THEN
                   ERROR " MONTO INVALIDO, NO PUEDE SER MAYOR AL SALDO "
                   NEXT FIELD mto_solic
               END IF

               --VERIFICA QUE NO SEA MAYOR AL SALDO CON BENEFICIO FISCAL
               IF (gr_general.tipo_ret = 3 OR gr_general.tipo_ret = 4 OR
                   gr_general.tipo_ret = 9 OR gr_general.tipo_ret = 10) AND
                   gr_general.mto_solic > lr_saldos.pes_cbf THEN
                   ERROR " MONTO INVALIDO, NO PUEDE SER MAYOR AL SALDO "
                   NEXT FIELD mto_solic
               END IF

               IF (gr_general.tipo_ret = 6 ) AND       #MLM-3420
                   gr_general.mto_solic > (lr_saldos.pes_sbf + lr_saldos.pes_cbf)  THEN
                   ERROR " MONTO INVALIDO, NO PUEDE SER MAYOR AL SALDO. "
                   NEXT FIELD mto_solic
               END IF

               IF gr_general.mto_solic > lr_saldos.pes THEN
                  ERROR " MONTO INVALIDO, NO PUEDE SER MAYOR AL SALDO "
                  NEXT FIELD mto_solic
               END IF

               --VERIFICA QUE NO SEA UN MONTO INVALIDO
               IF gr_general.mto_solic <= 0 THEN
                  ERROR " MONTO INVALIDO "
                  NEXT FIELD mto_solic
               END IF

              --CALCULA PORCENTAJE CON BASE AL MONTO SOLICITADO SEGUN T. RET
              CASE
                  WHEN (gr_general.tipo_ret = 7  OR gr_general.tipo_ret = 8 OR
                        gr_general.tipo_ret = 1  OR gr_general.tipo_ret = 2)    
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)/ lr_saldos.pes_sbf ) #MLM-3150
                       
                  WHEN (gr_general.tipo_ret = 9  OR gr_general.tipo_ret = 10 OR
                        gr_general.tipo_ret = 3  OR gr_general.tipo_ret = 4)
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)/ lr_saldos.pes_cbf )
                       
                  WHEN (gr_general.tipo_ret = 6)
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)/ (lr_saldos.pes_sbf + lr_saldos.pes_cbf) )  #MLM-3420
                       
                  WHEN (gr_general.tipo_ret = 11 OR gr_general.tipo_ret = 12)
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)/ lr_saldos.pes )  
                       
                  WHEN (gr_general.tipo_ret = 13 OR gr_general.tipo_ret = 14)   #LARGO PLAZO SBF  ##CPL-3203 
                       LET gr_general.porcentaje_solic=((gr_general.mto_solic*100)/ lr_saldos.pes_sbf )                                     
               END CASE

              DISPLAY BY NAME gr_general.mto_solic
              DISPLAY BY NAME gr_general.porcentaje_solic

              NEXT FIELD fecha_solic

            END IF

        -----------------------------
        AFTER FIELD porcentaje_solic
        -----------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD mto_solic
            END IF

            IF gr_general.porcentaje_solic <= 0 THEN
                ERROR ""
                ERROR "   PORCENTAJE INVALIDO" ATTRIBUTE(NORMAL)
            ELSE
                LET monto_paso = (gr_general.pes *
                                 (gr_general.porcentaje_solic / 100))

                LET gr_general.mto_solic = monto_paso

                DISPLAY gr_general.mto_solic TO mto_solic
            END IF

        -------------------------------
        AFTER FIELD fecha_solic
        -------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD PREVIOUS --tipo_pago
            END IF

            IF gr_general.fecha_solic IS NULL THEN
               ERROR "   CAMPO NO PUEDE SER NULO "
               NEXT FIELD fecha_solic
            END IF

            IF gr_general.fecha_solic > HOY THEN
               ERROR "   FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
               NEXT FIELD fecha_solic
            END IF

        -----------------------------
        AFTER FIELD pension_invalidez
        -----------------------------
           IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
              NEXT FIELD fecha_solic
           END IF

           IF gr_general.pension_invalidez IS NULL THEN
              ERROR " CAMPO NO PUEDE SER NULO "
              NEXT FIELD pension_invalidez
           ELSE
              IF gr_general.pension_invalidez NOT MATCHES "[SsNn]" THEN
                 ERROR "   CAMPO DEBE SER S o N "
                 NEXT FIELD pension_invalidez
              END IF
           END IF

           --SOLICITA CONFIRMACION
           IF gr_general.pension_invalidez MATCHES"[Ss]" THEN
               PROMPT "CONFIRMA QUE NO SE APLICARA RETENCION (S/N):" FOR CHAR enter
               IF enter NOT MATCHES "[Ss]" THEN
                  NEXT FIELD pension_invalidez
               END IF
           END IF

        -------------------------------
        AFTER INPUT
        -------------------------------
            IF gr_general.n_folio_sol IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD n_folio_sol
            ELSE
               SELECT "OK"
               FROM   ret_cta_vol
               WHERE  n_folio_sol <> reg_9.n_folio_sol
               AND    n_folio_sol  = gr_general.n_folio_sol
               GROUP BY 1

               IF STATUS <> NOTFOUND THEN
                  ERROR " SOLICITUD YA INGRESADA "
                  NEXT FIELD n_folio_sol
               END IF
            END IF

            IF gr_general.tipo_ret IS NULL THEN
               ERROR " CAMPO NO PUEDE SER NULO "
               NEXT FIELD tipo_ret
            ELSE
               SELECT des_tipo_ret
               INTO   gr_general.des_tipo_ret
               FROM   tab_retiro_old
               WHERE  tipo_ret = gr_general.tipo_ret

               IF STATUS = NOTFOUND THEN
                   ERROR " TIPO DE RETIRO INEXISTENTE "
                   NEXT FIELD tipo_ret
               END IF
            END IF

            --IF gr_general.tipo_pago IS NULL THEN
            --   ERROR " CAMPO NO PUEDE SER NULO "
            --   NEXT FIELD tipo_pago
            --ELSE
            --    SELECT descripcion
            --    INTO   gr_general.des_tipo_pago
            --    FROM   tab_pago
            --    WHERE  tipo_pago = gr_general.tipo_pago
            --
            --    IF STATUS = NOTFOUND THEN
            --        ERROR " TIPO DE PAGO INEXISTENTE "
            --        NEXT FIELD tipo_pago
            --    END IF
            --    DISPLAY gr_general.des_tipo_pago TO des_tipo_pago
            --END IF

            IF gr_general.fecha_solic IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD fecha_solic
            ELSE
                IF gr_general.fecha_solic > HOY THEN
                    ERROR " FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                    NEXT FIELD fecha_solic
                END IF
            END IF

            -----------------------------------------------
            --ACTUALIZACION DE LA SOLICITUD MODIFICADA
            -----------------------------------------------
            UPDATE ret_cta_vol
            SET    n_folio_sol       = gr_general.n_folio_sol      ,
                   tipo_ret          = gr_general.tipo_ret         ,
                   mto_solic         = gr_general.mto_solic        ,
                   porcentaje_solic  = gr_general.porcentaje_solic ,
                 --tipo_pago         = gr_general.tipo_pago        ,
                   fecha_solic       = gr_general.fecha_solic      ,
                   fecha_captura     = gr_general.fecha_captura    ,
                   ultimo_proceso    = gr_general.ultimo_proceso   ,
                   estado            = gr_estados.capturado   ,
                   usuario           = c8_usuario
            WHERE  consecutivo       = gr_general.consecutivo

            ERROR " SOLICITUD MODIFICADA "
            SLEEP 2
            LET sw_2 = 0
            INITIALIZE gr_general.*  TO NULL
            INITIALIZE reg_1.*  TO NULL

            EXIT INPUT

        -----------------------
        ON KEY (CONTROL-C)
        -----------------------
            LET gr_general.* = reg_9.*

            DISPLAY BY NAME gr_general.*

            EXIT INPUT

        -----------------------
        ON KEY (INTERRUPT)
        -----------------------
            LET gr_general.* = reg_9.*
            DISPLAY BY NAME gr_general.*
            EXIT INPUT
    END INPUT

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
#==============================================================================#
# Observacion:                                                                 #
#   la validacion de las subcuentas se realiza sobre las siefores 1 y 6        #
#==============================================================================#
FUNCTION f_valida_cta_saldo_comp(p_nss,p_tipo_ret)

    DEFINE p_nss                   CHAR(11)
    DEFINE p_tipo_ret              SMALLINT
    DEFINE ls_grupo_reg            SMALLINT
    DEFINE ls_siefore              SMALLINT

    --PARA DIS_CUENTA
    DEFINE v_sldo_sub11_sief1      DECIMAL(16,6)
    DEFINE v_sldo_sub11_sief6      DECIMAL(16,6)
    DEFINE v_sldo_sub12_sief1      DECIMAL(16,6)
    DEFINE v_sldo_sub12_sief6      DECIMAL(16,6)
    DEFINE v_sldo_sub24_sief1      DECIMAL(16,6)
    DEFINE v_sldo_sub24_sief6      DECIMAL(16,6)
    DEFINE v_sldo_sub25_sief1      DECIMAL(16,6)
    DEFINE v_sldo_sub25_sief6      DECIMAL(16,6)
    DEFINE v_sldo_sub23_sief1      DECIMAL(16,6)
    DEFINE v_sldo_sub23_sief6      DECIMAL(16,6)
    DEFINE v_sldo_sub03_sief1      DECIMAL(16,6)      #MLM-3150
    DEFINE v_sldo_sub03_sief6      DECIMAL(16,6)      #MLM-3150
    DEFINE v_sldo_sub10_sief1      DECIMAL(16,6)      #MLM-3150
    DEFINE v_sldo_sub10_sief6      DECIMAL(16,6)      #MLM-3150

    --PARA CTA_SALDO_VOL
    DEFINE v2_sldo_sub11_sief1     DECIMAL(16,6)
    DEFINE v2_sldo_sub11_sief6     DECIMAL(16,6)
    DEFINE v2_sldo_sub12_sief1     DECIMAL(16,6)
    DEFINE v2_sldo_sub12_sief6     DECIMAL(16,6)
    DEFINE v2_sldo_sub24_sief1     DECIMAL(16,6)
    DEFINE v2_sldo_sub24_sief6     DECIMAL(16,6)
    DEFINE v2_sldo_sub25_sief1     DECIMAL(16,6)
    DEFINE v2_sldo_sub25_sief6     DECIMAL(16,6)
    DEFINE v2_sldo_sub23_sief1     DECIMAL(16,6)
    DEFINE v2_sldo_sub23_sief6     DECIMAL(16,6)
    DEFINE v2_sldo_sub03_sief1     DECIMAL(16,6)      #MLM-3150
    DEFINE v2_sldo_sub03_sief6     DECIMAL(16,6)      #MLM-3150
    DEFINE v2_sldo_sub10_sief1     DECIMAL(16,6)      #MLM-3150
    DEFINE v2_sldo_sub10_sief6     DECIMAL(16,6)      #MLM-3150
    DEFINE v2_sldo_sub15_sief1     DECIMAL(16,6)      ##CPL-3203
    DEFINE v2_sldo_sub16_sief1     DECIMAL(16,6)      ##CPL-3203 

    DEFINE v_saldo                 DECIMAL(16,6)
    DEFINE v_sldo_sub15_sief1      DECIMAL(16,6)      ##CPL-3203
    DEFINE v_sldo_sub16_sief1      DECIMAL(16,6)      ##CPL-3203

    ---------------------------------------------------------------------------

    LET v_sldo_sub11_sief1   = 0
    LET v_sldo_sub11_sief6   = 0
    LET v_sldo_sub12_sief1   = 0
    LET v_sldo_sub12_sief6   = 0
    LET v_sldo_sub23_sief1   = 0
    LET v_sldo_sub23_sief6   = 0
    LET v_sldo_sub24_sief1   = 0
    LET v_sldo_sub24_sief6   = 0
    LET v_sldo_sub25_sief1   = 0
    LET v_sldo_sub25_sief6   = 0
    LET v_sldo_sub03_sief1   = 0       #MLM-3150
    LET v_sldo_sub03_sief6   = 0       #MLM-3150
    LET v_sldo_sub10_sief1   = 0       #MLM-3150
    LET v_sldo_sub10_sief6   = 0       #MLM-3150

    LET v2_sldo_sub11_sief1  = 0
    LET v2_sldo_sub11_sief6  = 0
    LET v2_sldo_sub12_sief1  = 0
    LET v2_sldo_sub12_sief6  = 0
    LET v2_sldo_sub23_sief1  = 0
    LET v2_sldo_sub23_sief6  = 0
    LET v2_sldo_sub24_sief1  = 0
    LET v2_sldo_sub24_sief6  = 0
    LET v2_sldo_sub25_sief1  = 0
    LET v2_sldo_sub25_sief6  = 0
    LET v2_sldo_sub03_sief1  = 0       #MLM-3150
    LET v2_sldo_sub03_sief6  = 0       #MLM-3150
    LET v2_sldo_sub10_sief1  = 0       #MLM-3150
    LET v2_sldo_sub10_sief6  = 0       #MLM-3150

   IF p_tipo_ret <=6 THEN
      LET ls_grupo_reg  = 3
   ELSE
       IF p_tipo_ret = 13 OR p_tipo_ret = 14 THEN
          LET ls_grupo_reg  = 4
       ELSE
          LET ls_grupo_reg = 2
       END IF
   END IF   
   SELECT codigo_siefore
   INTO   ls_siefore
   FROM   cta_nss_regimen
   WHERE  nss           = p_nss
   AND    grupo_regimen = ls_grupo_reg
    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 11 SIEFORE 1 y 6
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub11_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 11
    AND    A.siefore   = ls_siefore
   

    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 12 SIEFORE 1 y 6
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub12_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 12
    AND    A.siefore   = ls_siefore


    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 24 SIEFORE 1 y 6
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub24_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 24
    AND    A.siefore   = ls_siefore

    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 25 SIEFORE 1 y 6
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub25_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 25
    AND    A.siefore   = ls_siefore


    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 23 SIEFORE 1 y 6
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub23_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 23
    AND    A.siefore   = ls_siefore

    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 3 SIEFORE 1 y 6 #MLM-3150
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub03_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 3
    AND    A.siefore   = ls_siefore

    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 10 SIEFORE 1 y 6 #MLM-3150
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub10_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 10
    AND    A.siefore   = ls_siefore    
    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 15 SIEFORE 1 y 6 #MLM-3150
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub15_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 15
    AND    A.siefore   = ls_siefore
    ---------------------------------------------
    --RECUPERA SALDO SUBCUENTA 15 SIEFORE 1 y 6 #MLM-3150
    ---------------------------------------------
    SELECT NVL(SUM(A.monto_en_acciones),0)
    INTO   v_sldo_sub16_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 16
    AND    A.siefore   = ls_siefore
    
    ----------------------------------------------------------------
    --VALIDA SALDOS EN CERO SUBCUENTA 11 SIEFORE 1 y 6
    ----------------------------------------------------------------
    CASE
      WHEN  p_tipo_ret = 7 OR p_tipo_ret = 8
      IF v_sldo_sub11_sief1 = 0 AND
         v_sldo_sub12_sief1 = 0 THEN
         PROMPT "SALDO EN CERO NSS:",p_nss
         FOR CHAR enter
         RETURN FALSE
      END IF
      WHEN p_tipo_ret = 9 OR p_tipo_ret = 10
      IF (v_sldo_sub24_sief1 = 0 OR v_sldo_sub24_sief1 < 0)AND
         (v_sldo_sub25_sief1 = 0 OR v_sldo_sub25_sief1 < 0)THEN
         PROMPT "SALDO EN CERO NSS:",p_nss
         FOR CHAR enter
         RETURN FALSE
      END IF
   WHEN p_tipo_ret = 3 OR p_tipo_ret = 4
      IF (v_sldo_sub23_sief1 = 0 OR v_sldo_sub23_sief1 < 0)AND
         (v_sldo_sub23_sief6 = 0 OR v_sldo_sub23_sief6 < 0)THEN
         PROMPT "SALDO EN CERO NSS:",p_nss
         FOR CHAR enter
         RETURN FALSE
      END IF
   WHEN p_tipo_ret = 1 OR p_tipo_ret = 2
      IF (v_sldo_sub03_sief1 = 0 OR v_sldo_sub03_sief1 < 0) AND
         (v_sldo_sub03_sief6 = 0 OR v_sldo_sub03_sief6 < 0)AND
         (v_sldo_sub10_sief1 = 0 OR v_sldo_sub10_sief1 < 0)AND
         (v_sldo_sub10_sief6 = 0 OR v_sldo_sub10_sief6 < 0)THEN
         PROMPT "SALDO EN CERO NSS:",p_nss
         FOR CHAR enter
         RETURN FALSE
      END IF
   WHEN p_tipo_ret = 13 OR p_tipo_ret = 14  
      IF (v_sldo_sub15_sief1 = 0 OR v_sldo_sub15_sief1 < 0) AND
         (v_sldo_sub16_sief1 = 0 OR v_sldo_sub16_sief1 < 0) THEN 
         PROMPT "SALDO EN CERO NSS:",p_nss
         FOR CHAR enter
         RETURN FALSE
      END IF

   END CASE

    ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 11 SIEFORE 1 y 6
    ----------------------------------------------------------------

    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub11_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 11
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub11_sief1 <> v2_sldo_sub11_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta. 11 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
    END IF

     ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 12 SIEFORE 1 y 6
    ----------------------------------------------------------------

    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub12_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 12
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub12_sief1 <> v2_sldo_sub12_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta.12 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
        --RETURN
    END IF

   
   ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 23 SIEFORE 1 y 6
    ----------------------------------------------------------------

    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub23_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 23
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub23_sief1 <> v2_sldo_sub23_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta. 23 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
    END IF


   ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 24 SIEFORE 1 y 6
    ----------------------------------------------------------------
    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub24_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 24
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub24_sief1 <> v2_sldo_sub24_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta.24 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
        --RETURN
    END IF

   
    ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 25 SIEFORE 1 y 6
    ----------------------------------------------------------------
    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub25_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 25
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub25_sief1 <> v2_sldo_sub25_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta.25 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
        --RETURN
    END IF

  
     ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 3 SIEFORE 1 y 6 #MLM-3150
    ----------------------------------------------------------------

    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub03_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 3
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub03_sief1 <> v2_sldo_sub03_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta. 3 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
    END IF

     ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 10 SIEFORE 1 y 6 #MLM-3150
    ----------------------------------------------------------------

    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub10_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 10
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub10_sief1 <> v2_sldo_sub10_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta. 10 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
    END IF

    ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 15 SIEFORE 1 y 6 ##CPL-3203
    ----------------------------------------------------------------

    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub15_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 15
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub15_sief1 <> v2_sldo_sub15_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta. 15 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
    END IF

    ----------------------------------------------------------------
    --VALIDA SALDOS CONTRA CTA_SALDO_VOL SUBCUENTA 16 SIEFORE 1 y 6 ##CPL-3203
    ----------------------------------------------------------------

    SELECT NVL(SUM(A.saldo_acciones),0)
    INTO   v2_sldo_sub16_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss       = p_nss
    AND    A.subcuenta = 16
    AND    A.siefore   = ls_siefore

    IF v_sldo_sub16_sief1 <> v2_sldo_sub16_sief1 THEN
        PROMPT "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta. 16 y sief.",ls_siefore,") NSS:",p_nss
        FOR CHAR enter
        RETURN FALSE
        --EXIT PROGRAM
    END IF

    RETURN TRUE

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION valida_suficiencia(p_nss, p_consecutivo, p_tipo_ret)

    DEFINE p_nss                   CHAR(11)
    DEFINE p_consecutivo           INTEGER
    DEFINE p_tipo_ret              SMALLINT

    DEFINE v_prec_acc_sief1        LIKE glo_valor_accion.precio_del_dia
    DEFINE v_prec_acc_sief6        LIKE glo_valor_accion.precio_del_dia
    DEFINE v_sldo_pesos_sief1      DECIMAL(16,6)
    DEFINE v_sldo_pesos_sief6      DECIMAL(16,6)
    DEFINE v_tot_sldo_disp         DECIMAL(16,6)
    DEFINE ld_mto_solicitado       DECIMAL(16,6)
    DEFINE ld_mto_comp             DECIMAL(16,6)
    DEFINE v2_sldo_acc_sief1       DECIMAL(16,6)
    DEFINE v2_sldo_acc_sief6       DECIMAL(16,6)

    DEFINE v_ban                    SMALLINT
    DEFINE v2_ind_sief1             SMALLINT
    DEFINE v2_ind_sief6             SMALLINT

    DEFINE lr_saldos               RECORD
           acc_sbf_sb1             DECIMAL(16,6),
           acc_sbf_s6              DECIMAL(16,6),
           pes_sbf                 DECIMAL(16,6),
           acc_cbf_sb1             DECIMAL(16,6),
           acc_cbf_s6              DECIMAL(16,6),
           pes_cbf                 DECIMAL(16,6),
           pes                     DECIMAL(16,6)    --PESOS TOTALES
           END RECORD

    ---------------------------------------------------------------------------

    LET v_prec_acc_sief1   = 0
    LET v_prec_acc_sief6   = 0
    LET v_sldo_pesos_sief1 = 0
    LET v_sldo_pesos_sief6 = 0
    LET v_tot_sldo_disp    = 0
    LET ld_mto_solicitado  = 0
    LET v2_sldo_acc_sief1  = 0
    LET v2_sldo_acc_sief6  = 0

    ---------------------------------------------------------------------------

    --OBTIENE LOS SALDOS DE LAS SUBCUENTAS DE COMPLEMENTARIAS
    CALL f_obten_saldos_comp2(p_nss,p_tipo_ret) RETURNING lr_saldos.*


    --RECUPERA MONTO SOLICITADO
    SELECT mto_solic
    INTO   ld_mto_solicitado
    FROM   ret_cta_vol
    WHERE  n_seguro    = p_nss
    AND    consecutivo = p_consecutivo


    CASE p_tipo_ret
         WHEN  7 LET ld_mto_comp = lr_saldos.pes_sbf
         WHEN  8 LET ld_mto_comp = lr_saldos.pes_sbf
         WHEN  9 LET ld_mto_comp = lr_saldos.pes_cbf
         WHEN 10 LET ld_mto_comp = lr_saldos.pes_cbf
         WHEN 11 LET ld_mto_comp = lr_saldos.pes
         WHEN 12 LET ld_mto_comp = lr_saldos.pes
    END CASE

    IF FGL_GETENV("DEBUGVOL")="1" THEN                    --DEBUG
--       DISPLAY "Valida_suficiencia()"                     --DEBUG
--       DISPLAY "ld_mto_solicitado => ",ld_mto_solicitado  --DEBUG
--       DISPLAY "ld_mto_comp       => ",ld_mto_comp        --DEBUG
    END IF                                                --DEBUG

    --COMPARA MONTO SOLICITADO CONTRA DISPONIBLE
    IF ld_mto_solicitado > ld_mto_comp THEN
       PROMPT " RETIRO IMPROCEDENTE MTO. SOLIC MAYOR A MTO. EN LA CUENTA " ATTRIBUTE(NORMAL)
       FOR CHAR enter
       EXIT PROGRAM
    END IF

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION mes_letras(mes)

    DEFINE mes       CHAR(02)
    DEFINE vmes      CHAR(10)

    CASE mes
        WHEN "01"
            LET vmes = "ENERO"
        WHEN "02"
            LET vmes = "FEBRERO"
        WHEN "03"
            LET vmes=  "MARZO"
        WHEN "04"
            LET vmes = "ABRIL"
        WHEN "05"
            LET vmes = "MAYO"
        WHEN "06"
            LET vmes = "JUNIO"
        WHEN "07"
            LET vmes = "JULIO"
        WHEN "08"
            LET vmes = "AGOSTO"
        WHEN "09"
            LET vmes = "SEPTIEMBRE"
        WHEN "10"
            LET vmes = "OCTUBRE"
        WHEN "11"
            LET vmes = "NOVIEMBRE"
        WHEN "12"
            LET vmes = "DICIEMBRE"
    END CASE
    RETURN  vmes
END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION detalle_liq_acc()
    DEFINE arr_4 ARRAY[100] OF RECORD
           n_seguro               CHAR(11) ,
           nombre                 CHAR(43) ,
           mto_acc_sb1            DECIMAL(22,6),
           mto_acc_s6             DECIMAL(22,6)
           END RECORD

    DEFINE reg_17 RECORD
           n_seguro               CHAR(11) ,
           paterno                CHAR(40) ,
           materno                CHAR(40) ,
           nombres                CHAR(40) ,
           tipo_ret               SMALLINT ,
           mto_solic              DECIMAL(16,6)
           END RECORD

    DEFINE M1                     DECIMAL(22,6)
    DEFINE vmto_acc_sb1           DECIMAL(22,6)
    DEFINE vmto_acc_s6            DECIMAL(22,6)
    DEFINE vmto_pesos_sb1         DECIMAL(22,6)
    DEFINE vmto_pesos_s6          DECIMAL(22,6)
    DEFINE vprec_dia_sb1          DECIMAL(11,6)
    DEFINE vprec_dia_s6           DECIMAL(11,6)
    DEFINE tot_acc_sb1            DECIMAL(22,6)
    DEFINE tot_acc_s6             DECIMAL(22,6)
    DEFINE lc_instruccion         CHAR(500)
    DEFINE lc_sub                 CHAR(100)

    DEFINE pos                    INTEGER
    DEFINE arr_c                  INTEGER

    --------------------------------------------------------------------------

    OPEN WINDOW RETM0114 AT 2,2 WITH FORM "RETM0114" ATTRIBUTE(BORDER)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY "                              <Ctrl-C> Salir",
            "                                    " AT 1,1 ATTRIBUTE (REVERSE)
    DISPLAY " RETM011     LIQUIDACION DE APORTACIONES COMPLEMENTARIAS Y VOL     "
                                                   AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY " AT 3,68 ATTRIBUTE(REVERSE)

    INITIALIZE reg_17.* TO NULL
    LET vprec_dia_sb1  = 0
    LET vprec_dia_s6   = 0
    LET vmto_acc_sb1   = 0
    LET vmto_acc_s6    = 0
    LET vmto_pesos_sb1 = 0
    LET vmto_pesos_s6  = 0
    LET tot_acc_sb1    = 0
    LET tot_acc_s6     = 0
    LET pos            = 0

    FOR pos = 1 TO 100
        INITIALIZE arr_4[pos].* TO NULL
    END FOR

    LET pos     = 0

    --RECUPERA PRECIOS DEL DIA SIEFORE 1 y 6
    CALL precio_accion(HOY,1) RETURNING vprec_dia_sb1
    CALL precio_accion(HOY,3) RETURNING vprec_dia_s6

    --SELECCIONA SOLICITUDES
    DECLARE cur_15 CURSOR FOR
    SELECT A.n_seguro ,
           A.paterno  ,
           A.materno  ,
           A.nombres  ,
           A.tipo_ret ,
           A.mto_solic
    FROM   ret_cta_vol      A,
           OUTER ret_cta_vol_comp B
    WHERE  A.n_seguro    = B.n_seguro
    AND    A.consecutivo = B.consecutivo
    AND    A.estado      = gr_estados.capturado --IN (0, 3)

    --PARA CADA SOLICITUD
    FOREACH cur_15 INTO  reg_17.*

        LET pos = pos + 1

        LET arr_4[pos].n_seguro = reg_17.n_seguro
        LET arr_4[pos].nombre[1,26] = reg_17.paterno CLIPPED," ",
                                      reg_17.materno CLIPPED," ",
                                      reg_17.nombres CLIPPED
      CASE
          WHEN (reg_17.tipo_ret = 1 OR reg_17.tipo_ret = 2) #MLM-3150
               LET lc_sub = "(3,10)"
          WHEN (reg_17.tipo_ret = 3 OR reg_17.tipo_ret = 4)
               LET lc_sub = "(22,23)"
          WHEN (reg_17.tipo_ret = 6)
               LET lc_sub = "(3,10,22,23)"
          WHEN (reg_17.tipo_ret = 7 OR reg_17.tipo_ret = 8)
               LET lc_sub = "(11,12)"
          WHEN (reg_17.tipo_ret = 9 OR reg_17.tipo_ret = 10)
               LET lc_sub = "(24,25)"
          WHEN (reg_17.tipo_ret = 11 OR reg_17.tipo_ret = 12)
               LET lc_sub = "(11,12,24,25)"
          WHEN (reg_17.tipo_ret = 13 OR reg_17.tipo_ret = 14) ##CPL-3203
               LET lc_sub = "(15,16)"
          WHEN (reg_17.tipo_ret = 15) ##CPL-3203
               LET lc_sub = "(3,10,22,23,11,12,24,25,15,16)"
      END CASE

        LET lc_instruccion =  "SELECT NVL(SUM(A.monto_en_acciones),0) ",
                              "FROM   dis_cuenta A                    ",
                              "WHERE  A.nss       =  ? "               ,
                              "AND    A.subcuenta IN ",lc_sub CLIPPED  ,
                              "AND    A.siefore   = 1                 "

        PREPARE exe_acc_s1 FROM lc_instruccion



        LET lc_instruccion =  "SELECT NVL(SUM(A.monto_en_acciones),0) ",
                              "FROM   dis_cuenta A                    ",
                              "WHERE  A.nss       =  ? " ,
                              "AND    A.subcuenta IN ",lc_sub CLIPPED  ,
                              "AND    A.siefore   = 3                 "

        PREPARE exe_acc_s6 FROM lc_instruccion



        IF reg_17.tipo_ret = 2 OR reg_17.tipo_ret = 4 OR
           reg_17.tipo_ret = 8 OR reg_17.tipo_ret = 10 OR
           reg_17.tipo_ret = 12 OR reg_17.tipo_ret = 14 THEN   -- RETIRO TOTAL -- ##CPL-3203 (tipo ret =14) 
            --RECUPERA SALDO EN ACCIONES SB1
            SELECT NVL(SUM(A.monto_en_acciones),0)
            INTO   arr_4[pos].mto_acc_sb1
            FROM   dis_cuenta A
            WHERE  A.nss       = reg_17.n_seguro
            AND    A.subcuenta IN (11,12)
            AND    A.siefore   = 1

            --RECUPERA SALDO EN ACCIONES SB6
            SELECT NVL(SUM(A.monto_en_acciones),0)
            INTO   arr_4[pos].mto_acc_s6
            FROM   dis_cuenta A
            WHERE  A.nss       = reg_17.n_seguro
            AND    A.subcuenta IN (11,12)
            AND    A.siefore   = 3

            EXECUTE exe_acc_s1 USING reg_17.n_seguro INTO arr_4[pos].mto_acc_sb1
            EXECUTE exe_acc_s6 USING reg_17.n_seguro INTO arr_4[pos].mto_acc_s6

            LET tot_acc_sb1 = tot_acc_sb1 + arr_4[pos].mto_acc_sb1
            LET tot_acc_s6  = tot_acc_s6  + arr_4[pos].mto_acc_s6

        ELSE -- RETIRO PARCIAL --

            --RECUPERA SALDO EN ACCIONES SB1
            SELECT NVL(SUM(A.monto_en_acciones),0),
                   NVL(SUM(A.monto_en_acciones),0) * vprec_dia_sb1
            INTO   vmto_acc_sb1 ,
                   vmto_pesos_sb1
            FROM   dis_cuenta A
            WHERE  A.nss       = reg_17.n_seguro
            AND    A.subcuenta IN (11,12)
            AND    A.siefore   = 1

            --RECUPERA SALDO EN ACCIONES SB6
            SELECT NVL(SUM(A.monto_en_acciones),0),
                   NVL(SUM(A.monto_en_acciones),0) * vprec_dia_s6
            INTO   vmto_acc_s6 ,
                   vmto_pesos_s6
            FROM   dis_cuenta A
            WHERE  A.nss       = reg_17.n_seguro
            AND    A.subcuenta IN (11,12)
            AND    A.siefore   = 3

            EXECUTE exe_acc_s1 USING reg_17.n_seguro INTO vmto_acc_sb1
            EXECUTE exe_acc_s6 USING reg_17.n_seguro INTO vmto_acc_s6
            LET vmto_pesos_sb1 = vmto_acc_sb1 * vprec_dia_sb1
            LET vmto_pesos_s6  = vmto_acc_s6  * vprec_dia_s6

            LET M1 = reg_17.mto_solic - vmto_pesos_sb1

            IF M1 > 0 THEN
                LET arr_4[pos].mto_acc_sb1 = vmto_acc_sb1

                LET arr_4[pos].mto_acc_s6 = (M1 * vmto_acc_s6) / vmto_pesos_s6

            ELSE
                LET arr_4[pos].mto_acc_sb1 = (reg_17.mto_solic * vmto_acc_sb1 ) / vmto_pesos_sb1

                LET arr_4[pos].mto_acc_s6 = 0
            END IF

            LET tot_acc_sb1 = tot_acc_sb1 + arr_4[pos].mto_acc_sb1

            LET tot_acc_s6 = tot_acc_s6 + arr_4[pos].mto_acc_s6

        END IF
    END FOREACH


    DISPLAY tot_acc_sb1 to tot_acc_sb1

    DISPLAY tot_acc_s6 to tot_acc_s6

    CALL SET_COUNT(pos)

    --DESPLIEGA ARREGLO CON EL DETALLE EN ACCIONEs
    DISPLAY ARRAY arr_4 TO scr_1.* --ATTRIBUTES(maxcount=pos)
        ON KEY (INTERRUPT)
            LET pos = 0
            EXIT DISPLAY

        ON KEY (CONTROL-C)
            LET pos = 0
            EXIT DISPLAY

    END DISPLAY
    CLOSE WINDOW RETM0114
END FUNCTION

#==============================================================================#
# Muestra arreglo de tipos de retiro permitiendo su seleccion y retornando el  #
# tipo de retiro seleccionado                                                  #
#==============================================================================#
FUNCTION f_selecciona_tipo_ret()

   DEFINE lr_retiro          ARRAY[100] OF RECORD LIKE tab_retiro_old.*
   DEFINE lc_instruccion     CHAR(5000)
   DEFINE ls_pos             SMALLINT

   --PREPARA SELECCION DE BUSQUEDA

   LET lc_instruccion = " SELECT * FROM tab_retiro_old ",
                        " WHERE tipo_ret IN(1,2,3,4,6,7,8,9,10,11,12,13,14,15) "  #MLM-3150 #CPL-3203 (13 y 14)

   PREPARE pre_4 FROM lc_instruccion
   DECLARE cur_4 CURSOR FOR pre_4

   --CARGA EN ARREGLO DATOS SELECCIONADOS
   LET ls_pos = 1

   FOREACH cur_4 INTO lr_retiro[ls_pos].*
       LET ls_pos = ls_pos + 1
       IF ls_pos > 100 THEN
          ERROR " HA SIDO SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO "
          EXIT FOREACH
       END IF
   END FOREACH

   LET ls_pos = ls_pos -1

   --DESPLIEGA EL ARREGLO CON LOS DATOS SELECCIONADOS
   IF ls_pos = 0 THEN
      ERROR "EL CATALOGO ESTA VACIO, NOTIFIQUE A SISTEMAS <ENTER>:"
   ELSE
      OPEN WINDOW wretm0115 AT 05,12 WITH FORM "RETM0115" ATTRIBUTE(BORDER)
         DISPLAY " <ESC> Aceptar                         <CTRL-C> Cancelar " AT 1,1
         DISPLAY "             TRAMITES APORTACIONES VOLUNTARIAS           " AT 3,1  ATTRIBUTE(REVERSE)
         DISPLAY "       <ENTER> PARA SELECCIONAR EL TIPO DE RETIRO        " AT 13,1 ATTRIBUTE(REVERSE)

         CALL SET_COUNT(ls_pos)

         DISPLAY ARRAY lr_retiro TO scr_1.*  ATTRIBUTES(CURRENT ROW DISPLAY = "REVERSE")
            ON KEY (ESC, CONTROL-M)
               LET ls_pos = ARR_CURR()
               EXIT DISPLAY
         END DISPLAY

         IF INT_FLAG THEN
            LET INT_FLAG = FALSE
            LET ls_pos = 1
            INITIALIZE lr_retiro[ls_pos].* TO NULL
         END IF

      CLOSE WINDOW wretm0115
   END IF

   RETURN lr_retiro[ls_pos].*

END FUNCTION

#==============================================================================#
# Determina la cantidad de acciones y pesos por aplicar de la sie1 y sie6      #
#==============================================================================#
#==============================================================================#
# OBTIENE SALDOS EXISTENTES EN SUBCUENTAS DE APORTACIONES COMPLEMENTARIAS      #
#==============================================================================#
FUNCTION f_obten_saldos_comp(p_nss)

    DEFINE p_nss                   CHAR(11)
    DEFINE lr_saldos               RECORD
           acc_sbf_sb1             DECIMAL(16,6),
           acc_sbf_s6              DECIMAL(16,6),
           pes_sbf                 DECIMAL(16,2),
           acc_cbf_sb1             DECIMAL(16,2),
           acc_cbf_s6              DECIMAL(16,6),
           pes_cbf                 DECIMAL(16,2),
           pes                     DECIMAL(16,2)    --PESOS TOTALES
           END RECORD

    DEFINE ls_subcuenta            SMALLINT
    DEFINE ls_siefore              SMALLINT
    DEFINE ld_acciones             DECIMAL(16,6)
    DEFINE ld_precio               LIKE glo_valor_accion.precio_del_dia
    DEFINE ld_pesos                DECIMAL(16,6)
    DEFINE lc_instruccion          CHAR(5000)

   -----------------------------------------------------------------------------
    LET lr_saldos.acc_sbf_sb1 = 0
    LET lr_saldos.acc_sbf_s6  = 0
    LET lr_saldos.pes_sbf     = 0
    LET lr_saldos.acc_cbf_sb1 = 0
    LET lr_saldos.acc_cbf_s6  = 0
    LET lr_saldos.pes_cbf     = 0         --saldo con beneficio fiscal en pesos
    LET lr_saldos.pes         = 0         --saldo total en pesos

    --------------------------------------------------------------------------
    --PREPARA LA SELECCION DE LOS SALDOS QUE TENGA EL TRABAJADOR EN DIS_CUENTA
    --------------------------------------------------------------------------

    LET lc_instruccion = "SELECT siefore, subcuenta , NVL(monto_en_acciones,0)",
                         " FROM   dis_cuenta ",
                         " WHERE  nss = ? ",
                         " AND    subcuenta IN (11,12,24,25) "

    PREPARE exe01 FROM lc_instruccion

    DECLARE cur_sdo_com CURSOR FOR exe01

    FOREACH cur_sdo_com USING p_nss INTO ls_siefore, ls_subcuenta, ld_acciones

       --RECUPERA EL PRECIO DE LA SIEFORE SEGUN EL APORTE QUE SE RECUPERE
       CALL precio_accion(HOY,ls_siefore) RETURNING ld_precio

       --DETERMINA LOS PESOS
       LET ld_pesos = ld_acciones * ld_precio

       --ACUMULA ACCIONES Y PESOS SEGUN SUBCUENTA Y SIEFORE
       IF ls_subcuenta = 11 OR ls_subcuenta = 12 THEN
          --SIN BENEFICIO FISCAL
          IF ls_siefore = 6 THEN
             --SIEFORE 6
             LET lr_saldos.acc_sbf_s6  = lr_saldos.acc_sbf_s6 + ld_acciones
          ELSE
             --SIEFORE BASICA
             LET lr_saldos.acc_sbf_sb1 = lr_saldos.acc_sbf_sb1 + ld_acciones
          END IF

          LET lr_saldos.pes_sbf = lr_saldos.pes_sbf + ld_pesos
       ELSE
          --CON BENEFICIO FISCAL
          IF ls_siefore = 6 THEN
            --SIEFORE 6
            LET lr_saldos.acc_cbf_s6    = lr_saldos.acc_cbf_s6 + ld_acciones
          ELSE
             --SIEFORE BASICA 1
             LET lr_saldos.acc_cbf_sb1 = lr_saldos.acc_cbf_sb1 + ld_acciones
          END IF

          LET ld_pesos = ld_acciones * ld_precio
          LET lr_saldos.pes_cbf = lr_saldos.pes_cbf + ld_pesos

       END IF

       LET lr_saldos.pes = lr_saldos.pes + ld_pesos

    END FOREACH

    RETURN lr_saldos.*

END FUNCTION



#==============================================================================#
# OBTIENE SALDOS EXISTENTES EN SUBCUENTAS DE APORTACIONES COMPLEMENTARIAS      #
#==============================================================================#
FUNCTION f_obten_saldos_comp2(p_nss, p_tipo_ret)

    DEFINE p_nss                   CHAR(11)
    DEFINE p_tipo_ret              SMALLINT

    DEFINE lr_saldos               RECORD
           acc_sbf_sb1             DECIMAL(16,6),
           acc_sbf_s6              DECIMAL(16,6),
           pes_sbf                 DECIMAL(16,6),
           acc_cbf_sb1             DECIMAL(16,6),
           acc_cbf_s6              DECIMAL(16,6),
           pes_cbf                 DECIMAL(16,6),
           pes                     DECIMAL(16,6)    --PESOS TOTALES
           END RECORD

    DEFINE ls_subcuenta            SMALLINT
    DEFINE ls_siefore              SMALLINT
    DEFINE ld_acciones             DECIMAL(16,6)
    DEFINE ld_precio               LIKE glo_valor_accion.precio_del_dia
    DEFINE ld_pesos                DECIMAL(16,6)
    DEFINE lc_instruccion          CHAR(5000)
    DEFINE lc_sub                  CHAR (100)

   -----------------------------------------------------------------------------
    LET lr_saldos.acc_sbf_sb1 = 0
    LET lr_saldos.acc_sbf_s6  = 0
    LET lr_saldos.pes_sbf     = 0
    LET lr_saldos.acc_cbf_sb1 = 0
    LET lr_saldos.acc_cbf_s6  = 0
    LET lr_saldos.pes_cbf     = 0         --saldo con beneficio fiscal en pesos
    LET lr_saldos.pes         = 0         --saldo total en pesos

    --------------------------------------------------------------------------
    --PREPARA LA SELECCION DE LOS SALDOS QUE TENGA EL TRABAJADOR EN DIS_CUENTA
    --------------------------------------------------------------------------

    CASE
          WHEN (p_tipo_ret = 1 OR p_tipo_ret = 2) #MLM-3150
               LET lc_sub = "(3,10)"
          WHEN (p_tipo_ret = 3 OR p_tipo_ret = 4)
               LET lc_sub = "(22,23)"
          WHEN (p_tipo_ret = 6)
               LET lc_sub = "(3,10,22,23)"     #MLM-3420
          WHEN (p_tipo_ret = 7 OR p_tipo_ret = 8)
               LET lc_sub = "(11,12)"
          WHEN (p_tipo_ret = 9 OR p_tipo_ret = 10)
               LET lc_sub = "(24,25)"
          WHEN (p_tipo_ret = 11 OR p_tipo_ret = 12)
               LET lc_sub = "(11,12,24,25)"  
         WHEN (p_tipo_ret = 13 OR p_tipo_ret = 14) #CPL-3203
               LET lc_sub = "(15,16)"   
          WHEN (p_tipo_ret = 15)   ##CPL-3345
               LET lc_sub = "(3,10,22,23,11,12,24,25,15,16)"    
      END CASE

       LET lc_instruccion = " SELECT siefore, subcuenta , NVL(monto_en_acciones,0)",
                            " FROM   dis_cuenta ",
                            " WHERE  nss = ? ",
                            " AND    subcuenta IN ", lc_sub CLIPPED     #MLM-3150
    PREPARE exe02 FROM lc_instruccion

    DECLARE cur_sdo_com2 CURSOR FOR exe02

    FOREACH cur_sdo_com2 USING p_nss INTO ls_siefore, ls_subcuenta, ld_acciones

       --RECUPERA EL PRECIO DE LA SIEFORE SEGUN EL APORTE QUE SE RECUPERE
       CALL precio_accion(HOY,ls_siefore) RETURNING ld_precio

       --DETERMINA LOS PESOS
       LET ld_pesos = ld_acciones * ld_precio

       --ACUMULA ACCIONES Y PESOS SEGUN SUBCUENTA Y SIEFORE
       IF ls_subcuenta = 11 OR ls_subcuenta = 12 OR
          ls_subcuenta = 3  OR ls_subcuenta = 10 OR #MLM-3150
          ls_subcuenta = 15 OR ls_subcuenta = 16 THEN ##CPL-3203 (15 Y 16)
          --SIN BENEFICIO FISCAL
          IF ls_siefore = 3 THEN
             --SIEFORE 3
             LET lr_saldos.acc_sbf_s6  = lr_saldos.acc_sbf_s6 + ld_acciones
          ELSE
             --SIEFORE BASICA
             LET lr_saldos.acc_sbf_sb1 = lr_saldos.acc_sbf_sb1 + ld_acciones
          END IF

          LET lr_saldos.pes_sbf = lr_saldos.pes_sbf + ld_pesos
       ELSE
          --CON BENEFICIO FISCAL
          IF ls_siefore = 3 THEN
            --SIEFORE 3
            LET lr_saldos.acc_cbf_s6    = lr_saldos.acc_cbf_s6 + ld_acciones
          ELSE
             --SIEFORE BASICA 1
             LET lr_saldos.acc_cbf_sb1 = lr_saldos.acc_cbf_sb1 + ld_acciones
          END IF

          LET ld_pesos = ld_acciones * ld_precio
          LET lr_saldos.pes_cbf = lr_saldos.pes_cbf + ld_pesos

       END IF

       LET lr_saldos.pes = lr_saldos.pes + ld_pesos

    END FOREACH

    RETURN lr_saldos.*

END FUNCTION
#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_obten_edad(p_nss)

   DEFINE p_nss               CHAR(11)

   DEFINE r_existe            SMALLINT
   DEFINE r_edad              SMALLINT
   DEFINE r_criterio          SMALLINT
   DEFINE r_ind_edad          SMALLINT
   DEFINE v_curp              CHAR(18)
   DEFINE v_rfc               CHAR(13)
   DEFINE v_fena              DATE

   DEFINE lc_instruccion      CHAR(5000)

   LET lc_instruccion = "EXECUTE FUNCTION fn_fnacimiento(?,?)"
   PREPARE exe_fnac FROM lc_instruccion

   --EXECUTE exe_fnac USING reg_1.n_seguro,HOY INTO r_existe    ,
   EXECUTE exe_fnac USING p_nss,HOY INTO r_existe    ,
                                                  r_edad      ,
                                                  r_criterio  ,
                                                  r_ind_edad  ,
                                                  v_curp      ,
                                                  v_rfc       ,
                                                  v_fena
   RETURN r_edad

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_obten_desc_retiro(p_tipo_ret)

   DEFINE p_tipo_ret          SMALLINT
   DEFINE lr_tab_retiro       RECORD LIKE tab_retiro_old.*

   SELECT *
   INTO   lr_tab_retiro.*
   FROM   tab_retiro_old
   WHERE  tipo_ret = p_tipo_ret

   RETURN lr_tab_retiro.des_tipo_ret

END FUNCTION

#==============================================================================#
# Valida cuandre entre dis_cuenta y cta_saldo_vol                              #
# Retorna: TRUE si esta cuadrada la subcuenta, FALSE si no esta cuadrada       #
#==============================================================================#
FUNCTION f_valida_cuadre(p_nss, p_tipo_ret)

    DEFINE p_nss                   CHAR(11)
    DEFINE p_tipo_ret              SMALLINT

    DEFINE ld_saldo_dis            DECIMAL(16,6)
    DEFINE ld_saldo_cta            DECIMAL(16,6)

    ------------------------------------------------
    -- VALIDACION DE SUBCUENTAS SIN BENEFICIO FISCAL
    ------------------------------------------------
    IF p_tipo_ret=7 OR p_tipo_ret=8 OR p_tipo_ret=11 OR p_tipo_ret=12 THEN
       CALL f_obten_saldos_cuadre(p_nss, 11, 1)
            RETURNING ld_saldo_dis, ld_saldo_cta
       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 11 y sief. 1):"
          FOR CHAR enter
          RETURN FALSE
       END IF
       -------------------------------------------------------------------
       CALL f_obten_saldos_cuadre(p_nss, 11, 3)
            RETURNING ld_saldo_dis, ld_saldo_cta
       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 11 y sief. 6):"
          FOR CHAR enter
          RETURN FALSE
       END IF
       -------------------------------------------------------------------
       CALL f_obten_saldos_cuadre(p_nss, 12, 1)
            RETURNING ld_saldo_dis, ld_saldo_cta
       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 12 y sief. 1):"
          FOR CHAR enter
          RETURN FALSE
       END IF
       -------------------------------------------------------------------
       CALL f_obten_saldos_cuadre(p_nss, 12, 3)
            RETURNING ld_saldo_dis, ld_saldo_cta
       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 12 y sief. 6):"
          FOR CHAR enter
          RETURN FALSE
       END IF
    END IF

    ------------------------------------------------
    -- VALIDACION DE SUBCUENTAS SIN BENEFICIO FISCAL
    ------------------------------------------------
    IF p_tipo_ret=9 OR p_tipo_ret=10 OR p_tipo_ret=11 OR p_tipo_ret=12 THEN
       CALL f_obten_saldos_cuadre(p_nss, 24, 1)
            RETURNING ld_saldo_dis, ld_saldo_cta
       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 24 y sief. 1):"
          FOR CHAR enter
          RETURN FALSE
       END IF
       CALL f_obten_saldos_cuadre(p_nss, 24, 3)
            RETURNING ld_saldo_dis, ld_saldo_cta
       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 24 y sief. 6):"
          FOR CHAR enter
          RETURN FALSE
       END IF
       -------------------------------------------------------------------
       CALL f_obten_saldos_cuadre(p_nss, 25, 1)
            RETURNING ld_saldo_dis, ld_saldo_cta
       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 25 y sief. 1):"
          FOR CHAR enter
          RETURN FALSE
       END IF
       CALL f_obten_saldos_cuadre(p_nss, 25, 3)
            RETURNING ld_saldo_dis, ld_saldo_cta
       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 25 y sief. 6):"
          FOR CHAR enter
          RETURN FALSE
       END IF
    END IF

    -------------------------------------------------------------
    -- VALIDACION DE SUBCUENTAS VOLUNTARIAS CON BENEFICIO FISCAL
    -------------------------------------------------------------
    IF p_tipo_ret=3 OR p_tipo_ret=4 OR p_tipo_ret = 6 THEN

       CALL f_obten_saldos_cuadre(p_nss, 22, 1)
            RETURNING ld_saldo_dis, ld_saldo_cta

       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 22 y sief. 1):"
          FOR CHAR enter
          RETURN FALSE
       END IF

       CALL f_obten_saldos_cuadre(p_nss, 22, 3)
            RETURNING ld_saldo_dis, ld_saldo_cta

       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 22 y sief. 6):"
          FOR CHAR enter
          RETURN FALSE
       END IF

       -------------------------------------------------------------------
       CALL f_obten_saldos_cuadre(p_nss, 23, 1)
            RETURNING ld_saldo_dis, ld_saldo_cta

       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 23 y sief. 1):"
          FOR CHAR enter
          RETURN FALSE
       END IF

       CALL f_obten_saldos_cuadre(p_nss, 23, 3)
            RETURNING ld_saldo_dis, ld_saldo_cta

       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 23 y sief. 6):"
          FOR CHAR enter
          RETURN FALSE
       END IF
    END IF

    -------------------------------------------------------------
    -- VALIDACION DE SUBCUENTAS VOLUNTARIAS SIN BENEFICIO FISCAL
    -------------------------------------------------------------
    IF p_tipo_ret=1 OR p_tipo_ret=2 OR p_tipo_ret = 6 THEN

       CALL f_obten_saldos_cuadre(p_nss, 3, 1)
            RETURNING ld_saldo_dis, ld_saldo_cta

       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 3 y sief. 1):"
          FOR CHAR enter
          RETURN FALSE
       END IF

       CALL f_obten_saldos_cuadre(p_nss, 3, 3)
            RETURNING ld_saldo_dis, ld_saldo_cta

       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 3 y sief. 6):"
          FOR CHAR enter
          RETURN FALSE
       END IF

       -------------------------------------------------------------------
       CALL f_obten_saldos_cuadre(p_nss, 10, 1)
            RETURNING ld_saldo_dis, ld_saldo_cta

       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 10 y sief. 1):"
          FOR CHAR enter
          RETURN FALSE
       END IF

       CALL f_obten_saldos_cuadre(p_nss, 10, 3)
            RETURNING ld_saldo_dis, ld_saldo_cta

       IF ld_saldo_dis != ld_saldo_cta THEN
          PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 10 y sief. 6):"
          FOR CHAR enter
          RETURN FALSE
       END IF
    END IF

    -------------------------------------------------------------
    -- VALIDACION DE SUBCUENTAS SIN BENEFICIO FISCAL
    -------------------------------------------------------------  
    IF p_tipo_ret=13 OR p_tipo_ret=14 THEN ##CPL-3203
        ------------------------------subcta 15-------------------------------------
        CALL f_obten_saldos_cuadre(p_nss, 15, 1) RETURNING ld_saldo_dis, ld_saldo_cta
        IF ld_saldo_dis != ld_saldo_cta THEN
            PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 15 y sief. 1):"
            FOR CHAR enter
            RETURN FALSE
        END IF
        
        CALL f_obten_saldos_cuadre(p_nss, 15, 3) RETURNING ld_saldo_dis, ld_saldo_cta
        IF ld_saldo_dis != ld_saldo_cta THEN
            PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 15 y sief. 3):"
            FOR CHAR enter
            RETURN FALSE
        END IF
        ------------------------------subcta 16-------------------------------------
        CALL f_obten_saldos_cuadre(p_nss, 16, 1) RETURNING ld_saldo_dis, ld_saldo_cta
        IF ld_saldo_dis != ld_saldo_cta THEN
            PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 16 y sief. 1):"
            FOR CHAR enter
            RETURN FALSE
        END IF

        CALL f_obten_saldos_cuadre(p_nss, 16, 3) RETURNING ld_saldo_dis, ld_saldo_cta
        IF ld_saldo_dis != ld_saldo_cta THEN
            PROMPT "NO CUADRA cta_saldo_vol VS dis_cuenta (subcta. 16 y sief. 3):"
            FOR CHAR enter
            RETURN FALSE
        END IF
    END IF 

    RETURN TRUE

END FUNCTION

#==============================================================================#
# RECUPERA SALDOS DE dis_cuenta Y cta_saldo_vol para validar cuadraje          #
#==============================================================================#
FUNCTION f_obten_saldos_cuadre(p_nss, p_subcuenta, p_siefore)

   DEFINE p_nss               CHAR(11)
   DEFINE p_subcuenta         SMALLINT
   DEFINE p_siefore           SMALLINT

   DEFINE ld_saldo_acc1       DECIMAL(18,6)
   DEFINE ld_saldo_acc2       DECIMAL(18,6)

   --------------------------------
   -- RECUPERA SALDO DE DIS_CUENTA
   --------------------------------
   SELECT NVL(SUM(A.monto_en_acciones),0)
   INTO   ld_saldo_acc1
   FROM   dis_cuenta A
   WHERE  A.nss       = p_nss
   AND    A.subcuenta = p_subcuenta
   AND    A.siefore   = p_siefore

   --------------------------------
   -- RECUPERA SALDO DE CTA_SALDO_VOL
   --------------------------------
   SELECT NVL(SUM(A.saldo_acciones),0)
   INTO   ld_saldo_acc2
   FROM   cta_saldo_vol A
   WHERE  A.nss       = p_nss
   AND    A.subcuenta = p_subcuenta
   AND    A.siefore   = p_siefore

   RETURN ld_saldo_acc1, ld_saldo_acc2

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_lista_folios_liquidados()

   DEFINE la_folios_liq       ARRAY[50] OF RECORD
          folio               INTEGER  ,
          fecha               DATE     ,
          contador            INTEGER
          END RECORD
   DEFINE ls_cont             SMALLINT
   DEFINE ls_pos              SMALLINT

   -----------------------------------------------------------------------------

   MESSAGE "BUSCANDO INFORMACION, POR FAVOR ESPERE..." ATTRIBUTE(REVERSE)

   DECLARE curfolliq CURSOR FOR
   SELECT folio           ,
          fecha_conversion,
          COUNT(DISTINCT nss)
   FROM   dis_cuenta
   WHERE  tipo_movimiento IN (493,490,897)
   GROUP  BY 1,2
   ORDER  BY folio DESC

   LET ls_cont = 1

   FOREACH curfolliq INTO la_folios_liq[ls_cont].folio,
                          la_folios_liq[ls_cont].fecha,
                          la_folios_liq[ls_cont].contador
      LET ls_cont = ls_cont + 1
      IF ls_cont > 50 THEN
         ERROR "SOLO SE MOSTRARAN 50 REGISTROS"
         EXIT FOREACH
      END IF
   END FOREACH

   MESSAGE ""

   --ERROR "se mostraran","registros"

   --DESPLEGADO DE DATOS RECUPERADOS
   OPEN WINDOW wfl AT 2,2 WITH FORM "RETM0116" ATTRIBUTE(BORDER, FORM LINE 1)

      DISPLAY "         CONSULTA DE FOLIOS LIQUIDADOS DE VOLUNTARIAS Y COMPLEMENTARIAS        " AT 2,1 ATTRIBUTE(REVERSE)

      CALL SET_COUNT(ls_cont-1)

      DISPLAY ARRAY la_folios_liq TO sr_folios_liq.* ATTRIBUTES (CURRENT ROW DISPLAY = "REVERSE")
         ON KEY (CONTROL-M)
            LET ls_pos = ARR_CURR()

            CALL f_resumen_liquidacion(la_folios_liq[ls_pos].folio)
      END DISPLAY

   CLOSE WINDOW wfl

END FUNCTION

#==============================================================================#
# Realiza un respaldo integro del aporte candidato a liquidacion adjuntando    #
# informacion del folio de liquidacion de retiro asociado y la fecha de carga  #
#==============================================================================#
FUNCTION f_respalda_aportes(pc_nss, pi_folio,ps_tipo_ret)

   DEFINE pc_nss              CHAR(11)
   DEFINE pi_folio            INTEGER     --folio con el que se asopciaran
   DEFINE ps_tipo_ret         SMALLINT    --tipo de retiro

   DEFINE lr_cta_saldo_vol    RECORD LIKE cta_saldo_vol.*
   DEFINE lc_instruccion      CHAR(300)
   DEFINE consec_unico        INTEGER

   WHENEVER ERROR CONTINUE

   --SELECCIONA CADA UNO DE LOS APORTES DE LA CUENTA A RESPALDAR
   CASE
      WHEN ps_tipo_ret = 1 OR
           ps_tipo_ret = 2 OR
           ps_tipo_ret = 3 OR
           ps_tipo_ret = 4 OR        #MLM-3150
           ps_tipo_ret = 6             #mlm-3420
      --PREPARA SELECCION DE APORTES VOLUNTARIOS CON SALDO
      LET lc_instruccion = "SELECT *,rowid                        \n",
                           "FROM   cta_saldo_vol                  \n",
                           "WHERE  nss            = ?             \n",
                           "AND    subcuenta      IN(3,10,22,23)       \n", #MLM-3150
                           "AND    saldo_acciones > 0             \n"
      WHEN ps_tipo_ret = 7 OR
           ps_tipo_ret = 8 OR
           ps_tipo_ret = 9 OR
           ps_tipo_ret = 10 OR
           ps_tipo_ret = 11 OR
           ps_tipo_ret = 12
      --PREPARA SELECCION DE APORTES COMPLEMENTARIOS CON SALDO
      LET lc_instruccion = "SELECT *,rowid                        \n",
                           "FROM   cta_saldo_vol                  \n",
                           "WHERE  nss            = ?             \n",
                           "AND    subcuenta      IN(11,12,24,25) \n",
                           "AND    saldo_acciones > 0             \n"

     WHEN ps_tipo_ret = 13 OR ps_tipo_ret = 14 ##CPL-3203
       --PREPARA SELECCION DE APORTES LARGO PLAZO
      LET lc_instruccion = "SELECT *,rowid                        \n",
                           "FROM   cta_saldo_vol                  \n",
                           "WHERE  nss            = ?             \n",
                           "AND    subcuenta      IN(15,16,26,27) \n",
                           "AND    saldo_acciones > 0             \n"
     WHEN ps_tipo_ret = 15 ##CPL-3345
       --PREPARA SELECCION DE APORTES LARGO PLAZO
      LET lc_instruccion = "SELECT *,rowid                        \n",
                           "FROM   cta_saldo_vol                  \n",
                           "WHERE  nss            = ?             \n",
                           "AND    subcuenta      IN(3,10,22,23,11,12,24,25,15,16) \n",
                           "AND    saldo_acciones > 0             \n"
                           
   END CASE

   PREPARE exe_res FROM lc_instruccion

   DECLARE cur_res CURSOR FOR exe_res

   FOREACH cur_res USING pc_nss INTO lr_cta_saldo_vol.*,consec_unico
      --INSERTA APORTE EN TABLA DE RESPALO
      INSERT INTO cta_saldo_vol_rev
             VALUES(pi_folio                            ,    --folio_retiro
                    lr_cta_saldo_vol.folio              ,    --folio
                    lr_cta_saldo_vol.consecutivo_lote   ,    --consecutivo_lote
                    lr_cta_saldo_vol.nss                ,    --nss
                    lr_cta_saldo_vol.siefore            ,    --siefore
                    lr_cta_saldo_vol.subcuenta          ,    --subcuenta
                    lr_cta_saldo_vol.fecha_valor        ,    --fecha_valor
                    lr_cta_saldo_vol.fecha_conversion   ,    --fecha_conversion
                    lr_cta_saldo_vol.monto_en_pesos     ,    --monto_en_pesos
                    lr_cta_saldo_vol.monto_en_acciones  ,    --monto_en_acciones
                    lr_cta_saldo_vol.saldo_acciones     ,    --saldo_acciones
                    lr_cta_saldo_vol.fecha_saldo        ,    --fecha_saldo
                    lr_cta_saldo_vol.usuario            ,    --usuario
                    TODAY                               ,    --fecha_registro
                    NULL                                ,    --fecha_reverso
                    consec_unico                             --ROWID de cta_saldo_vol    
                    )
      IF SQLCA.SQLCODE < 0 THEN
         --HUBO UN ERROR
         LET gc_mensaje = "ERROR AL INSERTAR RESPALDO DE APORTE NSS:",pc_nss
         CALL ERRORLOG(gc_mensaje)
         CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
         PROMPT "NO SE PUDO RESPALDAR APORTE, INFORME A SISTEMAS, PRESIONE <ENTER>:" FOR CHAR ENTER
         EXIT PROGRAM
      END IF

   END FOREACH

   WHENEVER ERROR STOP

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_obten_siefores_comp(pc_nss)

   DEFINE pc_nss                CHAR(11)
   DEFINE ls_cod_sief_subta03   SMALLINT
   DEFINE ls_cod_sief_subta10   SMALLINT
   DEFINE ls_cod_sief_subta22   SMALLINT
   DEFINE ls_cod_sief_subta23   SMALLINT
   DEFINE ls_cod_sief_subta11   SMALLINT
   DEFINE ls_cod_sief_subta12   SMALLINT
   DEFINE ls_cod_sief_subta24   SMALLINT
   DEFINE ls_cod_sief_subta25   SMALLINT
   DEFINE ls_cod_sief_subta15   SMALLINT
   DEFINE ls_bandera            SMALLINT

   LET ls_bandera = 0

   -----------------------------------------------------------------------------
   WHENEVER ERROR CONTINUE

   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 03 #mlm-3150
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta03
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 3

   IF SQLCA.SQLCODE = NOTFOUND OR SQLCA.SQLCODE < 0 THEN #MLM-3150
      LET ls_bandera = 1
   END IF

   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 10
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta10
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 10

   IF SQLCA.SQLCODE = NOTFOUND OR SQLCA.SQLCODE < 0 THEN
      LET ls_bandera = 1
   END IF

   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 22
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta22
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 22

   IF SQLCA.SQLCODE = NOTFOUND OR SQLCA.SQLCODE < 0 THEN
      LET ls_bandera = 1
   END IF

   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 23
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta23
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 23


   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 11
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta11
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 11

   IF SQLCA.SQLCODE = NOTFOUND OR SQLCA.SQLCODE < 0 THEN
      LET ls_bandera = 1
   END IF

   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 12
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta12
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 12

   IF SQLCA.SQLCODE = NOTFOUND OR SQLCA.SQLCODE < 0 THEN
      LET ls_bandera = 1
   END IF

   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 24
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta24
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 24

   IF SQLCA.SQLCODE = NOTFOUND OR SQLCA.SQLCODE < 0 THEN
      LET ls_bandera = 1
   END IF

   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 25
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta25
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 25

   IF SQLCA.SQLCODE = NOTFOUND OR SQLCA.SQLCODE < 0 THEN
      LET ls_bandera = 1
   END IF

   --OBTIENE CODIGO SIEFORE DE LA SUBCUENTA 15
   SELECT codigo_siefore
   INTO   ls_cod_sief_subta15
   FROM   cta_regimen
   WHERE  nss       = pc_nss
   AND    subcuenta = 15 ##CPL-3203

   IF SQLCA.SQLCODE = NOTFOUND OR SQLCA.SQLCODE < 0 THEN
      LET ls_bandera = 1
   END IF

   IF ls_bandera = 1 THEN
      LET gc_mensaje = "NO ENCONTRO REGIMEN PARA SUBCUENTAS DE COMPLEMENTARIAS NSS ",pc_nss
      CALL ERRORLOG(gc_mensaje)
      LET gc_mensaje = gc_mensaje CLIPPED, "<ENTER>:"
      PROMPT gc_mensaje CLIPPED FOR CHAR enter
      EXIT PROGRAM
   END IF

   WHENEVER ERROR STOP

   RETURN ls_cod_sief_subta03,
          ls_cod_sief_subta10,
          ls_cod_sief_subta22,
          ls_cod_sief_subta23,
          ls_cod_sief_subta11,
          ls_cod_sief_subta12,
          ls_cod_sief_subta24,
          ls_cod_sief_subta25,
          ls_cod_sief_subta15
END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_resumen_liquidacion(pi_folio_liquida)

   DEFINE pi_folio_liquida    INTEGER
   DEFINE ls_sql              CHAR(4000)
   DEFINE la_resumen          ARRAY[200] OF RECORD
          nss                 LIKE dis_cuenta.nss,
          subcuenta           LIKE dis_cuenta.subcuenta,
          siefore             LIKE dis_cuenta.siefore,
          monto_neto          DECIMAL(10,2),
          monto_isr           DECIMAL(10,2),
          monto_isr_20        DECIMAL(10,2),
          monto_isr_35        DECIMAL(10,2),
          monto_total         DECIMAL(10,2)
          END RECORD
   DEFINE ld_total            DECIMAL(10,2)
   DEFINE ls_pos              SMALLINT

   -----------------------------------------------------------------------------

   IF pi_folio_liquida IS NULL THEN
      ERROR "FOLIO DE LIQUIDACION NULO...."
      RETURN
   END IF

   --INICIALIZACIONES
   LET ls_pos   = 1
   LET ld_total = 0

   --PROCESO
   MESSAGE "RECUPERANDO INFORMACION, ESPERE UN MOMENTO..."

   --PREPARA LA INFORMACION A CONSULTAR
   LET ls_sql = "SELECT nss,                                                            \n",
                "       subcuenta,                                                      \n",
                "       siefore,                                                        \n",
                "       SUM(CASE                                                        \n",
                "           WHEN (tipo_movimiento=490 OR tipo_movimiento=897)           \n",
                "                 THEN monto_en_pesos                                   \n",
                "                 ELSE 0                                                \n",
                "           END),                                                       \n",
                "       SUM(CASE                                                        \n",
                "           WHEN (tipo_movimiento=10 AND id_aportante='RETIRO')         \n",
                "                 THEN monto_en_pesos                                   \n",
                "                 ELSE 0                                                \n",
                "           END),                                                       \n",
                "       SUM(case                                                        \n",
                "           WHEN (tipo_movimiento=10 and id_aportante='RETIRO20')       \n",
                "                 THEN monto_en_pesos                                   \n",
                "                 ELSE 0                                                \n",
                "           END),                                                       \n",
                "       SUM(case                                                        \n",
                "           WHEN (tipo_movimiento=10 and id_aportante='RETIRO35')       \n",
                "                 THEN monto_en_pesos                                   \n",
                "                 ELSE 0                                                \n",
                "           END),                                                       \n",
                "       SUM(case                                                        \n",
                "           WHEN (tipo_movimiento=493 and id_aportante='RET-APPVOL')    \n",
                "                 THEN monto_en_pesos                                   \n",
                "                 ELSE 0                                                \n",
                "           END),                                                       \n",
                "       SUM(monto_en_pesos)                                             \n",
                " FROM dis_cuenta                                                       \n",
                " WHERE folio = ?                                                       \n",
                " GROUP BY 1,2,3                                                        \n",
                " ORDER BY 1,2 "

   PREPARE exe_resumen FROM ls_sql

   IF SQLCA.SQLCODE < 0 THEN
      ERROR "errror al preparar"
      -- DISPLAY ls_sql
      EXIT PROGRAM
   END IF

   DECLARE cur_resumen CURSOR FOR exe_resumen

   FOREACH cur_resumen USING pi_folio_liquida INTO la_resumen[ls_pos].*

      LET ld_total = ld_total + la_resumen[ls_pos].monto_total
      LET ls_pos = ls_pos + 1

      IF ls_pos > 200 THEN
         ERROR "SOLO SE VISUALIZARAN 200 REGISTROS"
         EXIT FOREACH
      END IF

   END FOREACH

   LET ls_pos = ls_pos -1

   CALL SET_COUNT(ls_pos)

   ----------------------------------------
   --DESPLEGADO DE LA INFORMACION LIQUIDADA
   ----------------------------------------
   OPEN WINDOW wresumen AT 2,2 WITH FORM "RETM0117" ATTRIBUTE(BORDER, FORM LINE 1)

      LET gc_mensaje = "                     CONSULTA DE LIQUIDACION FOLIO ",pi_folio_liquida USING "<<<<<<<<<<"
      DISPLAY gc_mensaje AT 2,1 ATTRIBUTE(REVERSE)
      DISPLAY ld_total TO total ATTRIBUTE(REVERSE)  --total general
      DISPLAY ARRAY la_resumen TO sa_resumen.*
   CLOSE WINDOW wresumen 

   RETURN

END FUNCTION
################################################################################
######
################################################################################
FUNCTION f_reversa_liquidacion_nss(lc_nss,li_folio,p_consecutivo)

   DEFINE li_folio                 INTEGER
   DEFINE lc_nss                   CHAR(11)
   DEFINE p_consecutivo            DECIMAL(10,0)
   DEFINE lc_opcion                CHAR(1)
   DEFINE li_contador              INTEGER
   DEFINE li_contador1             INTEGER
   DEFINE li_contador2             INTEGER
   DEFINE li_contador3             INTEGER
   DEFINE li_contador4             INTEGER
   DEFINE lr_cta_saldo_vol_rev     RECORD LIKE cta_saldo_vol_rev.*

      -------------------------------------------------
      --VALIDA LA EXISTENCIA DE INFORMACION A REVERSAR
      -------------------------------------------------
      SELECT COUNT(UNIQUE nss)
      INTO   li_contador
      FROM   dis_cuenta
      WHERE  folio            = li_folio
      AND    nss              = lc_nss
      AND    consecutivo_lote = p_consecutivo
      AND    tipo_movimiento IN(493,490, 897)

         --ELIMINA MOVIMIENTOS DE LIQUIDACION
         DELETE FROM dis_cuenta
         WHERE  folio            = li_folio
         AND    NSS              = lc_nss
         AND    consecutivo_lote = p_consecutivo
         AND    subcuenta IN (3,10,22,23,11,12,24,25,15,16)   --complementarias #MLM-3150  ##CPL-3203(15 y 16)

         --ELIMINA INFORMACION DE PAGO
         DELETE FROM ret_pago_vol
         WHERE  folio = li_folio
         AND    nss = (SELECT n_seguro
                        FROM   ret_cta_vol
                        WHERE  n_folio_liq = li_folio
                        AND    n_seguro = lc_nss)
         AND    consecutivo = p_consecutivo
         --ACTUALIZA ESTADO DE LA SOLICITUD
         UPDATE ret_cta_vol
         SET    n_folio_liq = NULL,
                estado      = 0
         WHERE  n_folio_liq = li_folio
         AND    n_seguro    = lc_nss
         AND    consecutivo = p_consecutivo
         AND    estado      = 7

         ------------------------------------------------
         --RECUPERA LOS MONTOS DE CADA UNO DE LOS APORTES
         ------------------------------------------------
         DECLARE cur_apo_rev1 CURSOR FOR
         SELECT *
         FROM   cta_saldo_vol_rev
         WHERE  folio_retiro = li_folio
         AND    nss          = lc_nss

         FOREACH cur_apo_rev1 INTO lr_cta_saldo_vol_rev.*

            --REGRESA AL APORTE EL SALDO ACCIONES RESPALDADO
            UPDATE cta_saldo_vol
            SET    saldo_acciones   = lr_cta_saldo_vol_rev.saldo_acciones
            WHERE  folio            = lr_cta_saldo_vol_rev.folio
            AND    consecutivo_lote = lr_cta_saldo_vol_rev.consecutivo_lote
            AND    nss              = lr_cta_saldo_vol_rev.nss
            AND    siefore          = lr_cta_saldo_vol_rev.siefore
            AND    subcuenta        = lr_cta_saldo_vol_rev.subcuenta

            IF SQLCA.SQLCODE < 0 THEN
               --HUBO UN ERROR
               LET gc_mensaje = "ERROR AL ACTUALIZAR APORTE EN DE REVERSO NSS:",lr_cta_saldo_vol_rev.nss
               CALL ERRORLOG(gc_mensaje)
               CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
               PROMPT "ERROR AL ACTUALIZAR FECHA DE REVERSO, INFORME A SISTEMAS, PRESIONE <ENTER>:" FOR CHAR ENTER
               EXIT PROGRAM
            END IF

            --REGISTRA LA FECHA DE APLICACION DEL REVERSO
            UPDATE cta_saldo_vol_rev
            SET    fecha_reverso    = TODAY
            WHERE  folio_retiro     = lr_cta_saldo_vol_rev.folio_retiro
            AND    nss              = lr_cta_saldo_vol_rev.nss
            AND    folio            = lr_cta_saldo_vol_rev.folio
            AND    consecutivo_lote = lr_cta_saldo_vol_rev.consecutivo_lote
            AND    siefore          = lr_cta_saldo_vol_rev.siefore
            AND    subcuenta        = lr_cta_saldo_vol_rev.subcuenta

            IF SQLCA.SQLCODE < 0 THEN
               --HUBO UN ERROR
               LET gc_mensaje = "ERROR AL ACTUALIZAR FECHA DE REVERSO AL APORTE NSS:",lr_cta_saldo_vol_rev.nss
               CALL ERRORLOG(gc_mensaje)
               CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
               PROMPT "ERROR AL ACTUALIZAR FECHA DE REVERSO, INFORME A SISTEMAS, PRESIONE <ENTER>:" FOR CHAR ENTER
               EXIT PROGRAM
            END IF

           LET li_contador4 = li_contador4 + 1

         END FOREACH
END FUNCTION
################################################################################
FUNCTION f_calculo_ISR(reg_11,ls_tipo_isr,v_porcentaje_isr)
#cisr-----------------------
    DEFINE reg_11 RECORD #loc #reg_11
        fecha_ini             DATE          ,
        fecha_fin             DATE          ,
        monto_en_acciones     DECIMAL(16,6) ,
        tot_accion            DECIMAL(16,6) ,
        siefore               SMALLINT      ,
        reg_inv               SMALLINT
    END RECORD

    DEFINE #loc #decimal
        precio_accion_ini                   ,
        precio_accion_fin     DECIMAL(16,6) ,
        rend_en_pesos         DECIMAL(16,6) ,
        mto_rete_accion       DECIMAL(16,6) ,
        --mto_neto              DECIMAL(16,6),
       valor_retencion        DECIMAL(16,6) ,
       monto_tot_isr          DECIMAL(16,6) ,
       monto_en_accion_pa     DECIMAL(16,6) ,
       monto_accion_base      DECIMAL(16,6)

    DEFINE   ld_dias          SMALLINT,
         ld_monto_factor      DECIMAL (16,6),
         ld_factor_anio       DECIMAL(16,6),
         ld_aporte_pesos      DECIMAL (16,6),
         ls_tipo_isr          SMALLINT
         
    DEFINE v_porcentaje_isr   DECIMAL(2,2) #Porcentaje 0.20 y 0.35 ##CPL-3203
         

    LET valor_retencion    = 0
    LET monto_tot_isr      = 0
    LET precio_accion_ini  = 0
    LET precio_accion_fin  = 0
    LET mto_rete_accion    = 0
    LET rend_en_pesos      = 0
    LET valor_retencion    = 0.0008    #cpl2461 #CPL-2701  #CPL-3270  #CPL-3485
    LET monto_en_accion_pa = reg_11.tot_accion
    LET monto_tot_isr      = 0
    LET monto_accion_base  = 0
    LET rend_en_pesos      = 0

    CALL precio_accion(reg_11.fecha_ini,reg_11.siefore) RETURNING precio_accion_ini
    CALL precio_accion(reg_11.fecha_fin,reg_11.reg_inv) RETURNING precio_accion_fin

    LET rend_en_pesos = (reg_11.monto_en_acciones - monto_accion_base)* precio_accion_fin

    IF monto_en_accion_pa >= reg_11.monto_en_acciones THEN
       LET ld_aporte_pesos = precio_accion_fin * reg_11.monto_en_acciones
       LET monto_accion_base = reg_11.monto_en_acciones * (precio_accion_ini/precio_accion_fin)
       
       LET ld_monto_factor = valor_retencion * ld_aporte_pesos

       IF (reg_11.fecha_fin - reg_11.fecha_ini) >= 365 THEN 
            LET ld_dias =  365
       ELSE
            LET ld_dias = (reg_11.fecha_fin - reg_11.fecha_ini)
       END IF 
         
       LET ld_factor_anio =  ld_monto_factor / 365

       IF ls_tipo_isr = 1 THEN 
            LET monto_tot_isr = ld_factor_anio * ld_dias
       ELSE
            LET monto_tot_isr  = (precio_accion_fin * reg_11.monto_en_acciones) * v_porcentaje_isr --0.20
       END IF  
       LET rend_en_pesos = (reg_11.monto_en_acciones - monto_accion_base)* precio_accion_fin
       LET monto_en_accion_pa = monto_en_accion_pa - reg_11.monto_en_acciones
    ELSE
       LET monto_accion_base = monto_en_accion_pa *(precio_accion_ini / precio_accion_fin)
       LET ld_monto_factor = valor_retencion * (precio_accion_fin * monto_en_accion_pa)

       IF (reg_11.fecha_fin - reg_11.fecha_ini) >= 365 THEN 
            LET ld_dias =  365
       ELSE
            LET ld_dias = (reg_11.fecha_fin - reg_11.fecha_ini)
       END IF 
         
       LET ld_factor_anio =  ld_monto_factor / 365

       IF ls_tipo_isr = 1 THEN 
            LET monto_tot_isr = ld_factor_anio * ld_dias
       ELSE
            LET monto_tot_isr  = (precio_accion_fin * monto_en_accion_pa) * v_porcentaje_isr  --0.20
       END IF  
       LET rend_en_pesos = (monto_en_accion_pa - monto_accion_base) * precio_accion_fin
       LET monto_en_accion_pa = 0
    END IF
    LET mto_rete_accion =  monto_tot_isr

    RETURN mto_rete_accion, rend_en_pesos

END FUNCTION
################################################################################
FUNCTION f_calcula_mto_solic( lc_nss,ls_tipo_ret,ld_mto_solic )
#cms----------------------------------------------------------
   DEFINE #loc #char
      lc_nss             CHAR(11)
      ,lc_sub                 CHAR(100)
      ,lc_instruccion         CHAR(1000)

   DEFINE
      ls_tipo_ret
      ,ls_cod_sief            SMALLINT

   DEFINE #loc #decimal
      ld_mto_acc
      ,ld_mto_pesos
      ,ld_tot_mto_pesos
      ,ld_mto_solic
      ,ld_prec_dia            DECIMAL(16,6)

   CASE
      WHEN ls_tipo_ret <= 6
         SELECT codigo_siefore
         INTO   ls_cod_sief
         FROM   cta_nss_regimen
         WHERE  nss           =  lc_nss
         AND    grupo_regimen =  3
      WHEN ls_tipo_ret >= 7 AND ls_tipo_ret <= 12
         SELECT codigo_siefore
         INTO   ls_cod_sief
         FROM   cta_nss_regimen
         WHERE  nss           =  lc_nss
         AND    grupo_regimen =  2  
      WHEN ls_tipo_ret = 13 OR ls_tipo_ret = 14  ##CPL-3203
         SELECT codigo_siefore
         INTO   ls_cod_sief
         FROM   cta_nss_regimen
         WHERE  nss           =  lc_nss
         AND    grupo_regimen =  4 
   END CASE

   CALL precio_accion(TODAY,ls_cod_sief)
   RETURNING ld_prec_dia

   CASE
      WHEN (ls_tipo_ret = 1 OR ls_tipo_ret = 2) #MLM-3150
           LET lc_sub = "(3,10)"
      WHEN (ls_tipo_ret = 3 OR ls_tipo_ret = 4)
           LET lc_sub = "(22,23)"
      WHEN (ls_tipo_ret = 7 OR ls_tipo_ret = 8)
           LET lc_sub = "(11,12)"
      WHEN (ls_tipo_ret = 9 OR ls_tipo_ret = 10)
           LET lc_sub = "(24,25)"
      WHEN (ls_tipo_ret = 11 OR ls_tipo_ret = 12)
           LET lc_sub = "(11,12,24,25)" 
      WHEN (ls_tipo_ret = 13 OR ls_tipo_ret = 14) ##CPL-3203
           LET lc_sub = "(15,16)"   
   END CASE

   LET lc_instruccion   =   " SELECT   NVL(SUM(A.monto_en_acciones),0) ,"
                           ,"          NVL(SUM((A.monto_en_acciones)     "
                           ,"          * ?),0)                 "
                           ," FROM     dis_cuenta A                      "
                           ," WHERE    A.nss       = ?                   "
                           ," AND      A.siefore   = ?                   "
                           ," AND      A.subcuenta in  ", lc_sub CLIPPED

   LET lc_instruccion = lc_instruccion CLIPPED
   PREPARE  sdo_al_dia   FROM lc_instruccion

   EXECUTE sdo_al_dia   USING ld_prec_dia, lc_nss     , ls_cod_sief
                        INTO  ld_mto_acc , ld_mto_pesos

   LET ld_mto_acc       = (ld_mto_solic * ld_mto_acc ) / ld_mto_pesos
   LET ld_tot_mto_pesos = ld_mto_acc  * ld_prec_dia

   RETURN   ld_tot_mto_pesos

END FUNCTION
################################################################################
FUNCTION f_calcula_mto_solic_benef(p_nss, p_consecutivo)
    DEFINE p_nss             CHAR(11)
    DEFINE p_consecutivo     DECIMAL(10,0) 
    DEFINE v_cantidad        SMALLINT
    DEFINE ld_mto_acc        DECIMAL(16,6) 
    DEFINE ld_mto_pesos      DECIMAL(16,6)
    DEFINE rec_saldo_benef   RECORD LIKE ret_saldo_benef.*
    
    -- Buscamos registro en ret_saldo_benef, si no se encuentra lo insertamos
    -- BUSCAMOS LAS SUBCUENTAS QUE TENGAN SALDO DEL TRABAJADOR
    LET gc_mensaje = "Buscando en ret_saldo_benef :",p_nss
    CALL ERRORLOG(gc_mensaje)

    DECLARE cur_saldo_vol CURSOR FOR SELECT nss, TODAY, subcuenta, siefore, 
                                            SUM(A.monto_en_acciones),
                                            SUM(A.monto_en_acciones * B.precio_del_dia)
                                     FROM   dis_cuenta A,
                                            glo_valor_accion B
                                     WHERE  A.nss             = p_nss
                                     AND    A.siefore         = B.codigo_siefore
                                     AND    B.fecha_valuacion = TODAY
                                     AND    A.subcuenta       IN  (3,10,22,23,11,12,24,25,15,16)
                                     GROUP BY 1,2,3,4
                                     HAVING SUM(A.monto_en_acciones) > 0
    FOREACH cur_saldo_vol INTO rec_saldo_benef.*
        SELECT COUNT(*) 
        INTO   v_cantidad
        FROM   ret_saldo_benef
        WHERE  nss = rec_saldo_benef.nss
        AND    subcuenta = rec_saldo_benef.subcuenta
        AND    siefore   = rec_saldo_benef.siefore
        IF v_cantidad = 0 THEN
            INSERT INTO ret_saldo_benef VALUES (rec_saldo_benef.*)
        END IF 
    END FOREACH

    SELECT SUM(A.monto_acc * (B.porcentaje/100)),
           SUM(A.monto_acc * (B.porcentaje/100) * C.precio_del_dia)
    INTO   ld_mto_acc , ld_mto_pesos
    FROM   ret_saldo_benef A,
           ret_ctr_benef B,
           glo_valor_accion C
    WHERE  A.nss               = p_nss
    AND    B.nss               = p_nss
    AND    B.consecutivo_solic = p_consecutivo
    AND    A.siefore           = c.codigo_siefore
    AND    C.fecha_valuacion   = TODAY
    AND    A.subcuenta         IN  (3,10,22,23,11,12,24,25,15,16)
    AND    A.fecha_saldo       = (SELECT MAX(fecha_saldo)
                                  FROM   ret_saldo_benef
                                  WHERE  nss = p_nss)
    LET gc_mensaje = "El saldo a regresar es :",ld_mto_pesos
    CALL ERRORLOG(gc_mensaje)    

   RETURN   ld_mto_pesos

END FUNCTION
########################################################################################
FUNCTION despliega_cod_rechazo_ent( tipo_retiro )
   DEFINE
      l_reg                ARRAY[100] OF RECORD
         cod_rechazo_ent   SMALLINT,
         des_corta         CHAR(60)
      END RECORD,
      x_x                  CHAR(200),
      x_buscar             CHAR(030),
      codigo, pos          SMALLINT,
      descripcion          CHAR(60),
      tipo_retiro, tr      CHAR(1)

   LET tr = tipo_retiro
   OPEN WINDOW retm0064 AT 5, 10 WITH FORM "RETM0064" ATTRIBUTE( BORDER )
   DISPLAY "                      TIPOS DE RECHAZOS                  "
      AT 2, 1 ATTRIBUTE( REVERSE )
   INPUT BY NAME x_buscar
      BEFORE FIELD x_buscar
         LET x_buscar = "*"

      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "DESCRIPCION A BUSCAR NO PUEDE SER NULA"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
   END INPUT
   WHILE TRUE
      LET x_x = " SELECT cod_rechazo_ent, des_larga FROM ret_rechazo_grl ",
         "WHERE des_corta MATCHES ", '"', x_buscar CLIPPED, '" ',
         --"AND tipo_retiro = '", tr CLIPPED, "' ",
         "AND tipo_retiro IN ( 'D', 'G' ) ",
         "AND entidad = 1 ",
         "ORDER BY 1 "

      PREPARE pre_9 FROM x_x
      DECLARE cur_91 CURSOR FOR pre_9
      LET pos = 1
      FOREACH cur_91 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF( pos - 1 ) < 1 THEN
         ERROR "CATALOGO DE RECHAZOS VACIO "
      END IF

      CALL SET_COUNT( pos - 1 )
      DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY ( CONTROL-C )
            LET pos = 0
            EXIT DISPLAY

         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY

         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            LET codigo = l_reg[pos].cod_rechazo_ent
            LET descripcion = l_reg[pos].des_corta
            EXIT DISPLAY
      END DISPLAY
      IF pos <> 0 THEN
         EXIT WHILE
      END IF
   END WHILE
   CLOSE WINDOW retm0064
   RETURN codigo   --, descripcion
END FUNCTION
################################################################################
FUNCTION fn_desmarca_cuenta(lc_nss, li_consecutivo)

   DEFINE   lc_nss            CHAR(11)
   DEFINE   li_consecutivo    DECIMAL(10,0)
   DEFINE   lc_prepare        CHAR(200)
   DEFINE   ls_marca          ,
            ls_edo_marca      ,
            ls_marca_causa    SMALLINT

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

   SELECT marca_cod 
   INTO   ls_marca
   FROM   cta_act_marca
   WHERE  nss         = lc_nss
   AND    correlativo = li_consecutivo
   LET ls_edo_marca    = 0
   LET ls_marca_causa  = 0
   
    EXECUTE eje_desmarca USING lc_nss           ,   -- nss
                               ls_marca         ,   -- marca entrante
                               li_consecutivo   ,   -- consecutivo
                               ls_edo_marca     ,   -- estado_marco
                               ls_marca_causa   ,   -- marca_causa
                               g_usuario                   -- usuario
   
END FUNCTION 
################################################################################

FUNCTION f_historia(lc_nss)         #cpl-2112
   DEFINE
      lc_nss               CHAR(11),
      archivo              CHAR(100)
      
CALL f_tabla_tmp()
CALL f_recupera (lc_nss)
CALL f_construye (lc_nss)

--CALL f_actualiza (lc_nss)
END FUNCTION
################################################################################
FUNCTION f_recupera(lc_nss)		#cpl-2112

DEFINE lc_dis_cuenta           CHAR(12)
      ,lc_tabla_ini           CHAR(12)
      ,lc_nss                 CHAR(11)
      ,ld_fecha               DATE
   --DISPLAY " "
   --DISPLAY " INICIO DEL PROCESO DE RECUPERACION DE TABLAS "

   SELECT   MIN(fecha_conversion)
   INTO     ld_fecha
   FROM     dis_cuenta
   WHERE    nss         =  lc_nss
   AND      subcuenta   IN (SELECT subcuenta FROM tab_agrupa_subcta_regimen
                            WHERE grupo_regimen IN (2,3,4))

   --CALL f_fecha_dis(ld_fecha) RETURNING lc_tabla_ini
   --DISPLAY "tabla inicio ", lc_tabla_ini
   LET mc_comando =  " SELECT tabname                         \n",
                     " FROM   SYSTABLES                       \n",
                     " WHERE  tabname MATCHES 'dis_cuenta??'  \n",
                    -- " AND    tabname >= ?                  \n",
                     " AND    tabname <> 'dis_cuenta_2'       \n",
                     " OR     tabname =  'dis_cuenta'         \n",
                     " ORDER BY 1                             \n"

   WHENEVER ERROR CONTINUE

      LET mc_comando = mc_comando CLIPPED
      PREPARE sql_dis FROM mc_comando

      IF SQLCA.SQLCODE != 0 THEN
         DISPLAY " ERROR AL EXTRAER INFORMACION VER LOG DE ERRORES"
         CALL ERRORLOG(SQLCA.SQLCODE|| mc_comando)
         RETURN
      END IF

   --SELECCION DE LA INFORMACION A PROCESAR
   DECLARE cur_discuenta CURSOR FOR sql_dis
   FOREACH cur_discuenta INTO lc_dis_cuenta --USING lc_tabla_ini
      --       CALL ERRORLOG (" VALIDANDO REGISTROS "|| lc_dis_cuenta )
      --DISPLAY " "
      --DISPLAY " VALIDANDO REGISTROS ", lc_dis_cuenta
      --
      --DISPLAY " PROCESANDO TOTALES"

      LET mc_comando =  " INSERT INTO cta_saldo_vol_tmp                 \n",
                        " SELECT * FROM (SELECT   0                   , \n",
                        "          folio                              , \n", 
                        "          consecutivo_lote                   , \n",
                        "          nss                                , \n",
                        "          tipo_movimiento                    , \n",
                        "          siefore                            , \n",
                        "          subcuenta                          , \n",
                        "          fecha_valor                        , \n",
                        "          fecha_conversion                   , \n",
                        "          monto_en_pesos                     , \n",
                        "          monto_en_acciones                  , \n",
                        "          monto_en_acciones                  , \n",
                        "          precio_accion                      , \n",
                        "          usuario                              \n",
                        " FROM     ",lc_dis_cuenta CLIPPED ,           "\n",
                        " WHERE    nss         =                   ?    \n",
                        " AND      subcuenta   IN (SELECT a.subcuenta    \n",
                        "                           FROM tab_agrupa_subcta_regimen a  \n",
                        "                          WHERE a.grupo_regimen IN (2,3,4))    \n",
                        " AND       monto_en_acciones  > 0                            \n",
                        " AND       tipo_movimiento not in( 999)                      \n",
                        " ORDER BY fecha_conversion DESC, folio DESC, consecutivo_lote DESC)  "

      LET mc_comando = mc_comando CLIPPED
      PREPARE sql_totales FROM mc_comando
      --CALL ERRORLOG(mc_comando)
      IF SQLCA.SQLCODE != 0 THEN
         DISPLAY " ERROR AL preparar la LA INFORMACION VER LOG DE ERRORES"
         CALL ERRORLOG(SQLCA.SQLCODE||" ERROR AL IPREPARAR LA FUNCION " || mc_comando)
         RETURN
      END IF

      EXECUTE  sql_totales USING lc_nss
      IF SQLCA.SQLCODE != 0 THEN
         DISPLAY " ERROR AL INSERTAR LA INFORMACION VER LOG DE ERRORES"
         CALL ERRORLOG(SQLCA.SQLCODE||" ERROR AL INSERTAR EN LA TABLA TEMPORAL " ||lc_dis_cuenta||" GENERACION DE TOTALES")
         RETURN
      END IF

      --DISPLAY " "
      --DISPLAY " VALIDANDO REGISTROS ", lc_dis_cuenta

      --DISPLAY " PROCESANDO TOTALES"

      LET mc_comando =  " INSERT INTO cta_saldo_vol_tmp_ret                                         \n",
                        " SELECT * FROM (SELECT   0                                               , \n",
                        "          folio                                                          , \n",
                        "          consecutivo_lote                                               , \n",
                        "          nss                                                            , \n",
                        "          tipo_movimiento                                                , \n",
                        "          siefore                                                        , \n",
                        "          subcuenta                                                      , \n",
                        "          fecha_conversion                                               , \n",
                        "          monto_en_pesos                                                 , \n",
                        "          monto_en_acciones                                              , \n",
                        "          monto_en_acciones                                              , \n",
                        "          precio_accion                                                    \n",
                        " FROM     ",lc_dis_cuenta CLIPPED ,                                       "\n",
                        " WHERE    nss         =                   ?                                \n",
                        " AND      subcuenta   IN (SELECT subcuenta FROM tab_agrupa_subcta_regimen  \n",
                        "                          WHERE grupo_regimen IN (2,3,4))                    \n",
                        " AND       monto_en_acciones < 0                                          \n",
                        " ORDER BY fecha_conversion DESC)                                           "


      LET mc_comando = mc_comando CLIPPED
      PREPARE sql_retiros FROM mc_comando

      IF SQLCA.SQLCODE != 0 THEN
         DISPLAY " ERROR AL preparar la LA INFORMACION VER LOG DE ERRORES"
         CALL ERRORLOG(SQLCA.SQLCODE||" ERROR AL IPREPARAR LA FUNCION " || mc_comando)
         RETURN
      END IF

      EXECUTE  sql_retiros USING lc_nss
      IF SQLCA.SQLCODE != 0 THEN
         DISPLAY " ERROR AL INSERTAR LA INFORMACION VER LOG DE ERRORES"
         CALL ERRORLOG(SQLCA.SQLCODE||" ERROR AL INSERTAR EN LA TABLA TEMPORAL " ||lc_dis_cuenta||" GENERACION DE TOTALES")
         RETURN
      END IF

   END FOREACH
   WHENEVER ERROR STOP
--   DELETE FROM   cta_saldo_vol_tmp_ret
--   WHERE  folio  IN (SELECT folio 
--                     FROM   tes_solicitud
--                     WHERE  nss = lc_nss
--                     AND    tipo_traspaso = 14)
--                       
--   DELETE FROM   cta_saldo_vol_tmp
--   WHERE  folio  IN (SELECT folio 
--                     FROM   tes_solicitud
--                     WHERE  nss = lc_nss
--                     AND    tipo_traspaso = 14)       


END FUNCTION

FUNCTION f_fecha_dis(ld_fecha)

   DEFINE
      lc_anio                 CHAR(02)
      ,lc_tabla_dis            CHAR(12)
      ,ld_fecha               DATE


      LET lc_anio  = (YEAR(ld_fecha)) MOD 100 USING "&&"
      LET lc_tabla_dis = "dis_cuenta"||lc_anio

      RETURN lc_tabla_dis

END FUNCTION

FUNCTION f_tabla_tmp()
   WHENEVER ERROR CONTINUE

   DROP  TABLE cta_saldo_vol_tmp;

   IF SQLCA.SQLCODE != 0 AND SQLCA.SQLCODE != -206 THEN
      CALL ERRORLOG (SQLCA.SQLCODE||" ERROR AL BORRAR LA TEMPORAL APORT ")
   END IF

   CREATE TEMP TABLE cta_saldo_vol_tmp
     (
       id_aporte           SERIAL         ,	      #CPL-2276
       folio               INTEGER        ,
       consecutivo_lote    INTEGER        ,
       nss                 CHAR(11)       ,
       tipo_movimiento     SMALLINT       ,
       siefore             SMALLINT       ,
       subcuenta           SMALLINT       ,
       fecha_valor         DATE           ,
       fecha_conversion    DATE           ,
       monto_en_pesos      DECIMAL(16,6)  ,
       monto_en_acciones   DECIMAL(16,6)  ,
       saldo_acciones      DECIMAL(16,6)  ,
       precio_accion       DECIMAL(16,6)  ,
       usuario             CHAR(12)
     )


   DROP TABLE cta_saldo_vol_tmp_ret;

   IF SQLCA.SQLCODE != 0 AND SQLCA.SQLCODE != -206 THEN
      CALL ERRORLOG (SQLCA.SQLCODE||" ERROR AL BORRAR LA TEMPORAL RET ")
   END IF

   CREATE TEMP  TABLE cta_saldo_vol_tmp_ret
     (
       id_retiro           SERIAL         ,     #CPL-2276
       folio               INTEGER        ,
       consecutivo_lote    INTEGER        ,
       nss                 CHAR(11)       ,
       tipo_movimiento     SMALLINT       ,
       siefore             SMALLINT       ,
       subcuenta           SMALLINT       ,
       fecha_conversion    DATE           ,
       monto_en_pesos      DECIMAL(22,6)  ,
       monto_en_acciones   DECIMAL(22,6)  ,
       saldo_acciones      DECIMAL(22,6)  ,
       precio_accion       DECIMAL(22,6)
     )

   WHENEVER ERROR STOP

END FUNCTION
################################################################################
FUNCTION f_construye(lc_nss)
   DEFINE
      lc_nss                        CHAR(11)
      ,ls_siefore
      ,ls_siefore_movimiento
      ,ls_siefore_abono      
      ,ls_subcuenta
      ,ls_tipo_movimiento           
      ,ls_regimen        
      ,v_bandera      
      ,ls_error                        SMALLINT
      ,ld_monto_acc_ret
      ,ld_saldo_acciones
      ,ld_precio
      ,ld_precio_ret
      ,ld_acciones_actuales_sie
      ,ld_total_acc
      ,ld_acciones_act
      ,ld_saldo                     DECIMAL(22,6)
      ,li_consecutivo_lote
      ,li_folio                     INTEGER
      ,ld_fecha_ret                 DATE
      ,ld_fecha_conversion          DATE
      ,ls_aporte                    INTEGER     #CPL-2276
      ,ld_valida_saldo_subcta_sief  DECIMAL(22,6)
      ,ld_suma_cargos               DECIMAL(22,6)
      ,ld_pesos                     DECIMAL(22,6)  
      ,ls_subcuenta_proceso         SMALLINT
      ,ls_bandera_sin_saldo         SMALLINT
      ,ld_folio_lote_ord            INTEGER,
      v_mensaje_debug               CHAR(200)      
   WHENEVER ERROR CONTINUE
  
   LET ls_subcuenta_proceso = 0
   LET ls_bandera_sin_saldo = 0
   LET mc_comando    = " SELECT NVL(SUM(monto_en_acciones),0)  \n",
                       " FROM   dis_cuenta                     \n",
                       " WHERE  nss       = ?                  \n",
                       " AND    subcuenta = ?                    "

   LET mc_comando = mc_comando CLIPPED
   PREPARE pre_saldo FROM mc_comando
   
   LET mc_comando    = " SELECT   folio             ,             \n",
                       "          siefore           ,             \n",
                       "          subcuenta         ,             \n",
                       "          tipo_movimiento   ,             \n",
                       "          ABS(monto_en_acciones) ,         \n",
                       "          precio_accion     ,             \n",
                       "          fecha_conversion                \n",
                       " FROM     cta_saldo_vol_tmp_ret \n",
                       " ORDER BY subcuenta, fecha_conversion DESC \n"

   LET mc_comando = mc_comando CLIPPED
   LET ld_monto_acc_ret = 0
   LET ld_total_acc = 0
   PREPARE pre_cur FROM mc_comando

   IF SQLCA.SQLCODE != 0 THEN
      CALL ERRORLOG (SQLCA.SQLCODE ||mc_comando)
   END IF

   WHENEVER ERROR STOP
   DECLARE cur_ret CURSOR FOR pre_cur
   FOREACH cur_ret INTO li_folio             ,
                        ls_siefore           ,
                        ls_subcuenta         ,
                        ls_tipo_movimiento   ,
                        ld_monto_acc_ret     ,
                        ld_precio_ret        ,
                        ld_fecha_ret

      IF  ls_tipo_movimiento = 905  THEN

         LET mc_comando =
         " SELECT   NVL(saldo_acciones,0)                          \n",
         " FROM     cta_saldo_vol_tmp   a                          \n",
         " WHERE     tipo_movimiento    = 914                      \n",--55
         " AND       subcuenta         = ",ls_subcuenta        ,"  \n",
         " AND       fecha_conversion  = '",ld_fecha_ret       ,"' \n",
         " AND       folio             = ",li_folio            ,"  \n"

         LET mc_comando = mc_comando CLIPPED
         PREPARE sql_acc_act FROM mc_comando
         IF SQLCA.SQLCODE != 0 THEN
            CALL ERRORLOG(SQLCA.SQLCODE||mc_comando)
         END IF

         EXECUTE sql_acc_act  INTO     ld_acciones_actuales_sie

         IF SQLCA.SQLCODE = 0 THEN
            UPDATE   cta_saldo_vol_tmp
            SET      saldo_acciones    =  0
            WHERE    tipo_movimiento   =  914 --55
            AND      subcuenta         =  ls_subcuenta
            AND      folio             =  li_folio         
          CALL fn_actualiza_anteriores_corte(ld_acciones_actuales_sie,ld_monto_acc_ret,ls_subcuenta,ls_siefore,ld_fecha_ret) RETURNING v_bandera
            IF v_bandera <> 0 THEN 
               LET v_mensaje_debug = "                                     INFO -     Problemas al actualizar anteriores: ", v_bandera
               CALL ERRORLOG (v_mensaje_debug)
            ELSE 
               LET v_mensaje_debug = "                                     INFO -     Anteriores actualizados: ", v_bandera
               CALL ERRORLOG (v_mensaje_debug)
            END IF 
         ELSE
            CONTINUE FOREACH
         END IF
         --LET ld_monto_acc_ret = 0 
      ELSE

         LET mc_comando =
         " SELECT   NVL(saldo_acciones,0)                                             \n ",
         " FROM     cta_saldo_vol_tmp                                                 \n ",
         " WHERE    folio       in  (SELECT   b.folio                                  \n ",
         "                          FROM     tes_solicitud  b                         \n ",
         "                          WHERE    b.folio =          ", li_folio CLIPPED," \n ",
         "                          AND      b.fecha_traspaso ='", ld_fecha_ret,    "'\n ",
         "                          AND      b.grupo_regimen IN (2,3,4)                 \n ",
         "                          AND      b.nss             = '",lc_nss,"'    )     \n ",
         " AND      saldo_acciones > 0                                                \n ",
         " AND      subcuenta         =     ", ls_subcuenta,"                         \n "

         LET mc_comando = mc_comando CLIPPED
         PREPARE sql_tes_sol   FROM mc_comando
         IF SQLCA.SQLCODE != 0 THEN
                  CALL ERRORLOG(SQLCA.SQLCODE||mc_comando)
         END IF

         EXECUTE  sql_tes_sol INTO  ld_acciones_actuales_sie
         WHENEVER ERROR STOP
         
         IF SQLCA.SQLCODE = 0 THEN
            UPDATE   cta_saldo_vol_tmp
            SET      saldo_acciones    =  0
            WHERE    folio             =  li_folio
            AND      subcuenta         =  ls_subcuenta
            AND      fecha_conversion  =  ld_fecha_ret
            CALL fn_actualiza_anteriores_corte(ld_acciones_actuales_sie,ld_monto_acc_ret,ls_subcuenta,ls_siefore,ld_fecha_ret) RETURNING v_bandera
            IF v_bandera <> 0 THEN 
            
               LET v_mensaje_debug = "                                     INFO -     Problemas al actualizar anteriores transferencia: ", v_bandera
               CALL ERRORLOG (v_mensaje_debug)
            ELSE 
               LET v_mensaje_debug = "                                     INFO -     Anteriores transferencia actualizados: ", v_bandera
               CALL ERRORLOG (v_mensaje_debug)
            END IF 
            --LET ld_monto_acc_ret= 0
         END IF

      END IF
    END FOREACH
   --- Obtenemos el saldo de cada una de las subcuentas

   LET ls_regimen = 0
   LET mc_comando =  " SELECT subcuenta                  \n",
                     " FROM   tab_agrupa_subcta_regimen  \n",
                     " WHERE  grupo_regimen IN (2,3,4)       "
   PREPARE pre_subctas FROM mc_comando

   WHENEVER ERROR STOP
   DECLARE cur_subctas CURSOR FOR pre_subctas
   FOREACH cur_subctas INTO ls_subcuenta_proceso
      --- Se obtiene el saldo de la subcuenta para comenzar la actualización de las aportaciones
      EXECUTE pre_saldo  USING lc_nss, ls_subcuenta_proceso INTO ld_saldo
--   LET v_mensaje_debug = "                                     INFO -     Saldo de la subcuenta : ", ls_subcuenta_proceso, " - ",ld_saldo, " NSS ",lc_nss
--   CALL ERRORLOG (v_mensaje_debug)

      IF ld_saldo IS NOT NULL AND ld_saldo > 0 THEN
      
         DECLARE cur_apor CURSOR FOR
         SELECT  saldo_acciones     ,
                 consecutivo_lote   ,
                 precio_accion      ,
                 id_aporte          ,                     #CPL-2276
                 fecha_conversion   ,
                 siefore            ,
                 folio
         FROM    cta_saldo_vol_tmp
         WHERE   saldo_acciones > 0
         AND     subcuenta = ls_subcuenta_proceso
         ORDER BY fecha_conversion DESC, folio DESC, consecutivo_lote DESC, id_aporte            #CPL-2276
         FOREACH cur_apor INTO   ld_saldo_acciones    ,
                                 li_consecutivo_lote  ,
                                 ld_precio            ,
                                 ls_aporte            ,       #CPL-2276
                                 ld_fecha_conversion  ,
                                 ls_siefore_movimiento, 
                                 ld_folio_lote_ord
                                 
            IF ld_saldo_acciones <= ld_saldo THEN 
               LET ld_saldo = ld_saldo - ld_saldo_acciones
            ELSE 
               UPDATE cta_saldo_vol_tmp       ---- Para cuando son varios registros para la misma fecha y mismo folio 
               SET    saldo_acciones   = ld_saldo        ---- Primero se inicializan los que restan del folio 
               WHERE  nss              = lc_nss
               AND    subcuenta        = ls_subcuenta_proceso
               AND    consecutivo_lote = li_consecutivo_lote
               AND    fecha_conversion = ld_fecha_conversion
               AND    folio            = ld_folio_lote_ord
               AND    siefore          = ls_siefore_movimiento
               AND    id_aporte        = ls_aporte

               UPDATE cta_saldo_vol_tmp       ---- Para cuando son varios registros para la misma fecha y mismo folio 
               SET    saldo_acciones   = 0        ---- Primero se inicializan los que restan del folio 
               WHERE  nss              = lc_nss
               AND    subcuenta        = ls_subcuenta_proceso
               AND    fecha_conversion <= ld_fecha_conversion
               AND    folio            <=  ld_folio_lote_ord
               --AND    consecutivo_lote < li_consecutivo_lote
               AND    id_aporte        > ls_aporte
               
               UPDATE cta_saldo_vol_tmp
               SET    saldo_acciones   = 0
               WHERE  nss              = lc_nss
               AND    subcuenta        = ls_subcuenta_proceso
               AND    fecha_conversion <= ld_fecha_conversion
               AND    folio            <  ld_folio_lote_ord
               LET ld_saldo = 0
               EXIT FOREACH
            END IF
         END FOREACH
         IF ld_saldo > 0 THEN
             --Ajusta remanente
              UPDATE cta_saldo_vol_tmp       
              SET    saldo_acciones   = saldo_acciones + ld_saldo
              WHERE  nss              = lc_nss
              AND    subcuenta        = ls_subcuenta_proceso
              AND    consecutivo_lote = li_consecutivo_lote
              AND    fecha_conversion = ld_fecha_conversion
              AND    folio            = ld_folio_lote_ord
              AND    siefore          = ls_siefore_movimiento
              AND    id_aporte        = ls_aporte
         END IF                         
      ELSE 
         UPDATE cta_saldo_vol_tmp
         SET    saldo_acciones   = 0
         WHERE  nss              = lc_nss
         AND    subcuenta        = ls_subcuenta_proceso
      END IF 
   END FOREACH

END FUNCTION

FUNCTION f_actualiza(lc_nss)

DEFINE 
       v_id_aporte        SMALLINT, 
       v_fecha_conversion DATE,
       v_saldo_acciones   DECIMAL(22,6),
       v_siefore_origen   SMALLINT, 
       v_siefore_destino  SMALLINT,
       v_subcuenta        SMALLINT,
       lc_nss             CHAR(11),
       f_corte            DATE,
       precio_sf_origen   DECIMAL(22,6),
       precio_sf_destino  DECIMAL(22,6),
       nuevos_pesos       DECIMAL(22,6), 
       nuevas_acciones    DECIMAL(22,6)

    
    LET f_corte = "12/16/2019"

    DECLARE cur_actualiza CURSOR FOR
    SELECT id_aporte,
           fecha_conversion , 
           saldo_acciones   ,
           subcuenta        ,
           siefore           
    FROM  cta_saldo_vol_tmp
    WHERE saldo_acciones > 0 
    ORDER BY id_aporte, fecha_conversion  ASC
    
    FOREACH cur_actualiza INTO   v_id_aporte       ,            
                                 v_fecha_conversion,            
                                 v_saldo_acciones  ,
                                 v_subcuenta       ,
                                 v_siefore_origen         

        SELECT precio_del_dia 
        INTO   precio_sf_origen
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = v_fecha_conversion
        AND    codigo_siefore = v_siefore_origen

        LET nuevos_pesos = v_saldo_acciones * precio_sf_origen

        SELECT codigo_siefore
        INTO  v_siefore_destino
        FROM cta_regimen 
        WHERE nss = lc_nss
        AND   subcuenta = v_subcuenta

        SELECT precio_del_dia 
        INTO precio_sf_destino
        FROM glo_valor_accion 
        WHERE fecha_valuacion = f_corte
        AND codigo_siefore = v_siefore_destino

        LET nuevas_acciones = nuevos_pesos / precio_sf_destino

        UPDATE cta_saldo_vol_tmp
        SET saldo_acciones = nuevas_acciones
        WHERE id_aporte = v_id_aporte

    END FOREACH 

END FUNCTION 

FUNCTION fn_actualiza_anteriores_corte(p_acciones_saldo,p_acciones_ret,p_subcuenta,p_siefore,p_fecha_ret)
DEFINE p_acciones_saldo       DECIMAL(22,6)
DEFINE p_acciones_ret         DECIMAL(22,6)
DEFINE p_subcuenta            SMALLINT
DEFINE p_siefore              SMALLINT 
DEFINE p_fecha_ret            DATE
DEFINE ls_bandera             SMALLINT

   LET ls_bandera = 0
   
   WHENEVER ERROR CONTINUE
            UPDATE   cta_saldo_vol_tmp
            SET      saldo_acciones    =  ((saldo_acciones * p_acciones_saldo)/p_acciones_ret)
            WHERE    subcuenta         =  p_subcuenta
            AND      siefore           =  p_siefore
            AND      fecha_conversion  <  p_fecha_ret;

   IF SQLCA.SQLCODE != 0 THEN
      CALL ERRORLOG (SQLCA.SQLCODE||" ERROR AL actualizar temporal")
      LET ls_bandera = SQLCA.SQLCODE
   END IF
   WHENEVER ERROR STOP
   
   RETURN ls_bandera

END FUNCTION 

#################################################################################
#+ Descripción General: Valida años
#+
#+ Descripción Amplia: Realiza la validacion de los años de la aportacion
#+
#+ @code
#+ DEFINE ld_fec_aporte     DATE
#+ DEFINE ls_band           SMALLINT
#+ CALL f_valida_anios5(ld_fec_aporte)
#+
#+ @param ld_fec_aporte     : Fecha de aportacion
#+
#+ @returnType SMALLINT 
#+ @return ls_band : 0= Mayor o igual a 5 años. 1=Menor a 5 años 
#+
FUNCTION f_valida_anios5(ld_fec_aporte)
#----------------------------------
DEFINE
   ld_fec_aporte           DATE,
   ls_dias_trans ,
   ls_band                 SMALLINT,
   li_anios_trans          DECIMAL(5,2)

   LET ls_band = 1

   LET ls_dias_trans = TODAY - ld_fec_aporte

   LET li_anios_trans = ls_dias_trans / 365

   IF li_anios_trans >= 5 THEN
        LET ls_band = 0
   END IF

   RETURN ls_band
END FUNCTION

#################################################################################
#+ Descripción General Función: Busca la primea fecha de dis_cuenta 
#+
#+ @code
#+ DEFINE p_nss           CHAR(0011)
#+ DEFINE p_siefore       SMALLINT
#+ DEFINE ls_tipo_ret     SMALLINT
#+ DEFINE v_min_fecha     DATE
#+ CALL f_fecha_primera_voluntaria(p_nss, p_siefore,ls_tipo_ret) RETURNIG v_min_fecha
#+
#+ @param p_nss       : NSS
#+ @param p_siefore   : Siefore
#+ @param ls_tipo_ret : Tipo retiro 
#+
#+ @returnType DATE
#+ @return v_min_fecha  : Primera fecha 
#+
FUNCTION f_fecha_primera_voluntaria(p_nss, p_siefore,ls_tipo_ret)

   DEFINE p_siefore       SMALLINT
   DEFINE p_nss           CHAR(0011)
   DEFINE v_tabname       CHAR(0050)
   DEFINE v_comando       CHAR(20000)
   DEFINE v_min_fecha     DATE
   DEFINE ls_cont_anio    SMALLINT
   DEFINE ls_tipo_ret     SMALLINT #MLM-3150
   DEFINE ls_tipo_mov     SMALLINT#MLM-3150
   #MLM-3150
   IF ls_tipo_ret = 13 OR ls_tipo_ret = 14  THEN
         LET ls_tipo_mov = 490
      ELSE
         LET ls_tipo_mov = 897
   END IF
   #MLM-3150
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta
      #MLM-3150
      LET v_comando = " SELECT * FROM dis_cuenta ",
                      " WHERE nss = '",p_nss CLIPPED,"'",
                      " AND   tipo_movimiento = ", ls_tipo_mov , #MLM-3150
                      " AND   siefore = ",p_siefore CLIPPED

      --PREPARA LA SELECCION PARA BARRER TODAS LAS TABLAS DE DIS_CUENTA
      LET ls_cont_anio = 0

      DECLARE cur_dis_cta_1 CURSOR FOR
      SELECT tabname
      FROM SYSTABLES
      WHERE  tabname MATCHES "dis_cuenta?*"
      ORDER BY 1

      FOREACH cur_dis_cta_1 INTO v_tabname
         LET ls_cont_anio = ls_cont_anio + 1
         LET v_comando = v_comando CLIPPED, "\n UNION ",
                         " SELECT * FROM ",v_tabname CLIPPED,
                         " WHERE nss = '",p_nss,"'",
                         " AND   tipo_movimiento = ", ls_tipo_mov , #MLM-3150
                         " AND   siefore = ",p_siefore CLIPPED
         IF ls_cont_anio = 18 THEN   --solo busca en los 18 respaldos
            EXIT FOREACH
         END IF
      END FOREACH

      LET v_comando = v_comando CLIPPED, " INTO TEMP tmp_dis_cuenta "

      PREPARE exe_dis_cuenta_1 FROM v_comando

      IF SQLCA.SQLCODE < 0 THEN
         PROMPT "SE PRESENTO UN ERROR AL PREPARAR CONSULTA A DIS_CUENTA :" FOR CHAR ENTER
         CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
         CALL ERRORLOG(v_comando CLIPPED)
         EXIT PROGRAM
      END IF

      EXECUTE exe_dis_cuenta_1

      IF SQLCA.SQLCODE < 0 THEN
         CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
         CALL ERRORLOG(v_comando CLIPPED)
         EXIT PROGRAM
      END IF

      SELECT MIN(fecha_conversion)
      INTO v_min_fecha
      FROM tmp_dis_cuenta

   WHENEVER ERROR STOP

   RETURN v_min_fecha

END FUNCTION


#==============================================================================#
#CPL-3212 LIQUIDACION PARCIAL APP                                              #
#==============================================================================#
FUNCTION f_liquida_parciales_app(pc_nss, ps_tipo_ret, pd_fecha_liquidacion)

   DEFINE pc_nss                    CHAR(11)
   DEFINE ps_tipo_ret               SMALLINT
   DEFINE pd_fecha_liquidacion      DATE

   DEFINE lr_cta_saldo_vol          RECORD LIKE cta_saldo_vol.*
   DEFINE lr_ret_cta_vol            RECORD LIKE ret_cta_vol.*

   DEFINE lc_instruccion            CHAR(2000)
   DEFINE v_subcuenta               CHAR(100)
   DEFINE v_total                   INTEGER 

   DEFINE ld_pendiente_pesos        DECIMAL(18,6)
   DEFINE ld_pendiente_acciones     DECIMAL(18,6)
   DEFINE ld_mto_aporte_pesos       DECIMAL(18,6)
   DEFINE ls_siefore_act            SMALLINT
   DEFINE ls_grupo_reg              SMALLINT 
   DEFINE v_query                   CHAR(5000)
   DEFINE v_tabname                 CHAR(20)
   DEFINE v_consecutivo_lote_existe INTEGER
   DEFINE folio_existe              INTEGER
   DEFINE nss_existe                CHAR(11)
   DEFINE tipo_movimiento_existe    SMALLINT
   DEFINE subcuenta_existe          SMALLINT
   DEFINE v_monto_en_acciones_existe DECIMAL(22,7)
   DEFINE v_query2                  CHAR(5000)
   DEFINE v_dis_cuenta_consecutivo_lote INTEGER
   DEFINE v_dis_cuenta_nss          CHAR(11)
   DEFINE v_dis_cuenta_subcuenta    SMALLINT
   DEFINE v_dis_cuenta_siefore      SMALLINT
   DEFINE v_contador_tablas         SMALLINT
   DEFINE v_dis_cuenta_folio        INTEGER   
   DEFINE v_query3                  CHAR(20000)
   DEFINE ls_consec_ret             SMALLINT

   DEFINE ld_precio_retiro         LIKE glo_valor_accion.precio_del_dia

   -----------------------------------------------------------------------------
   MESSAGE "LIQUIDANDO RETIRO PARCIAL APP..." SLEEP 1

   ------------------------------------
   -- SE RECUPERA SOLICITUD DE RETIRO
   ------------------------------------
   WHENEVER ERROR CONTINUE

   SELECT *
   INTO   lr_ret_cta_vol.*
   FROM   ret_cta_vol
   WHERE  n_seguro = pc_nss
   AND    tipo_ret = ps_tipo_ret
   AND    estado   = 7


   IF SQLCA.SQLCODE < 0 THEN
      CALL ERRORLOG(ERR_GET(SQLCA.SQLCODE))
      PROMPT "SE PRESENTO UN ERROR EN LA CONSULTA DE SOLICITUD, NOTIFIQUE A SISTEMAS:" FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   IF SQLCA.SQLCODE = NOTFOUND THEN
      LET gc_mensaje = "NO SE LOCALIZO LA SOLICITUD PARA EL NSS ", pc_nss
      PROMPT gc_mensaje FOR CHAR ENTER
      EXIT PROGRAM
   END IF

   WHENEVER ERROR STOP
   
   -------------------------------------------------------------------
   --DETERMINA LAS SUBCUENTAS A SELECCIONAR SEGUN EL TIPO DE RETIRO
   -------------------------------------------------------------------
      CASE
          WHEN ps_tipo_ret = 1    #MLM-3150
               LET v_subcuenta = "(3,10)"
          WHEN ps_tipo_ret = 3
               LET v_subcuenta = "(23)"
          OTHERWISE
           PROMPT "TIPO DE RETIRO INCORRECTO PARA LA RUTINA <ENTER>:" FOR CHAR ENTER
           EXIT PROGRAM
      END CASE

   --DETERMINA EL MONTO EN PESOS POR APLICAR (INDICADO EN LA SOLICITUD)
   LET ld_pendiente_pesos = lr_ret_cta_vol.mto_solic

   -----------------------------------------------------------------------
   -- SELECCION LOS APORTES DE CTA_SALDO_VOL CON SALDO PARA HACER EL PAGO
   -----------------------------------------------------------------------

   #se busca en todas las tablas de dis_cuenta
    DECLARE cur_dis_cta_dis_historico_parcial CURSOR FOR
    SELECT tabname
    FROM SYSTABLES
    WHERE  (tabname MATCHES "dis_cuenta[0-9][0-9]"
        OR tabname='dis_cuenta')

    LET v_total = 0
    LET v_contador_tablas = 0

    FOREACH cur_dis_cta_dis_historico_parcial INTO v_tabname

        LET v_query= '',
                     '\n SELECT DISTINCT                       ',
                     '\n        consecutivo_lote,              ',
                     '\n        nss,                           ',
                     '\n        subcuenta,                     ',
                     '\n        siefore,                       ',
                     '\n        folio                          ',
                     '\n   FROM ',v_tabname CLIPPED,'          ',
                     '\n  WHERE tipo_movimiento IN (310,311)   ',   -- PST-3660
                     '\n    AND subcuenta IN  ',v_subcuenta,'  ',   -- PST-3660
                     '\n    AND nss        = "',pc_nss,'"      ',
                     
                     '\n UNION ',
                     
                     '\n SELECT DISTINCT                         ',
                     '\n        a.consecutivo_lote,              ',
                     '\n        a.nss,                           ',
                     '\n        a.subcuenta,                     ',
                     '\n        a.siefore,                       ',
                     '\n        a.folio                          ',
                     '\n   FROM ',v_tabname CLIPPED,' a,         ',
                     '\n        int_det_vol_rc b                 ',
                     '\n  WHERE a.nss              = b.nss       ',
                     '\n    AND a.folio            = b.folio     ',
                     '\n    AND a.monto_en_pesos   = b.monto     ',
                     '\n    AND a.tipo_movimiento  = 123         ',
                     '\n    AND b.cve_rc           = "032"       ',
                     '\n    AND a.subcuenta IN ',v_subcuenta,'   ',
                     '\n    AND a.nss        = "',pc_nss,'"      '
                     
        PREPARE prp_obtiene_datos_dis_cuenta FROM v_query
        DECLARE cur_obtiene_datos_dis_cuenta CURSOR FOR prp_obtiene_datos_dis_cuenta

        FOREACH cur_obtiene_datos_dis_cuenta
           INTO v_dis_cuenta_consecutivo_lote,
                v_dis_cuenta_nss,
                v_dis_cuenta_subcuenta,
                v_dis_cuenta_siefore,
                v_dis_cuenta_folio

            {
            DISPLAY "ENTRAMOS:"
            DISPLAY "v_dis_cuenta_consecutivo_lote: ",v_dis_cuenta_consecutivo_lote
            DISPLAY "v_dis_cuenta_nss: ",v_dis_cuenta_nss
            DISPLAY "v_dis_cuenta_subcuenta: ",v_dis_cuenta_subcuenta
            DISPLAY "v_dis_cuenta_siefore: ",v_dis_cuenta_siefore
            DISPLAY "v_dis_cuenta_folio: ",v_dis_cuenta_folio
            DISPLAY "v_contador_tablas: ",v_contador_tablas
            }

            IF v_contador_tablas=0 THEN
                LET v_query3="SELECT vol.folio,\n",
                              " vol.consecutivo_lote,\n",    
                              " vol.nss,\n",    
                              " vol.siefore,\n",    
                              " vol.subcuenta,\n",    
                              " vol.fecha_valor,\n",    
                              " vol.fecha_conversion,\n",    
                              " vol.monto_en_pesos,\n",    
                              " vol.monto_en_acciones,\n",    
                              " vol.saldo_acciones,\n",    
                              " vol.fecha_conversion,\n",    
                              " vol.usuario\n",    
                              " FROM cta_saldo_vol_tmp vol ",
                              " WHERE vol.saldo_acciones > 0 ",    
                                " AND vol.consecutivo_lote =",v_dis_cuenta_consecutivo_lote,
                                " AND vol.nss ='",v_dis_cuenta_nss,"' ",
                                " AND vol.subcuenta =",v_dis_cuenta_subcuenta,
                                " AND vol.siefore =",v_dis_cuenta_siefore,
                                " AND vol.folio =",v_dis_cuenta_folio

                LET v_contador_tablas=1
            ELSE
                LET v_query3= v_query3 CLIPPED, " UNION ",
                                 "SELECT vol.folio,\n",    
                                  " vol.consecutivo_lote,\n",
                                  " vol.nss,\n",    
                                  " vol.siefore,\n",    
                                  " vol.subcuenta,\n",    
                                  " vol.fecha_valor,\n",    
                                  " vol.fecha_conversion,\n",    
                                  " vol.monto_en_pesos,\n",    
                                  " vol.monto_en_acciones,\n",    
                                  " vol.saldo_acciones,\n",    
                                  " vol.fecha_conversion,\n",    
                                  " vol.usuario \n",    
                                  " FROM cta_saldo_vol_tmp vol ",
                                 " WHERE vol.saldo_acciones > 0 ",                   
                                   " AND vol.consecutivo_lote =",v_dis_cuenta_consecutivo_lote,
                                   " AND vol.nss ='",v_dis_cuenta_nss,"' ",
                                   " AND vol.subcuenta =",v_dis_cuenta_subcuenta,
                                   " AND vol.siefore =",v_dis_cuenta_siefore,
                                   " AND vol.folio =",v_dis_cuenta_folio
            END IF

            --DISPLAY "V_QERY4:",v_query3

            LET v_total = v_total + 1

        END FOREACH
        --DISPLAY "SALIO"
    END FOREACH

    LET v_query3 = v_query3," ORDER BY vol.fecha_conversion ASC "

    --DISPLAY "V:_QUERY11:",v_query
    --DISPLAY "V:_QUERY33:",v_query3
    
    IF v_total > 0 THEN

        LET ls_consec_ret = 0

        --DISPLAY "CONSULTA: ",v_query3
    
        PREPARE exe_saldo_par_app FROM v_query3
        DECLARE cur_saldo_par_app CURSOR FOR exe_saldo_par_app
        FOREACH cur_saldo_par_app INTO lr_cta_saldo_vol.*
        
           --RECUPERA EL PRECIO DE ACCION DE LA SIEFORE DEL APORTE A LA FECHA DEL RETIRO

           SELECT codigo_siefore
           INTO   ls_siefore_act
           FROM   cta_regimen
           WHERE  nss = lr_cta_saldo_vol.nss
           AND    subcuenta = lr_cta_saldo_vol.subcuenta
              
           SELECT precio_del_dia
           INTO   ld_precio_retiro
           FROM   glo_valor_accion
           WHERE  fecha_valuacion = pd_fecha_liquidacion
           AND    codigo_siefore  = ls_siefore_act
              
           --DISPLAY "ld_precio_retiro",ld_precio_retiro

           LET ls_consec_ret = ls_consec_ret + 1
           --OBTIENE EL IMPORTE ACTUALIZADO EN PESOS DEL APORTE
           
           LET ld_mto_aporte_pesos = lr_cta_saldo_vol.saldo_acciones * ld_precio_retiro

          IF ld_pendiente_pesos >  ld_mto_aporte_pesos THEN
             --SE REQUIERE APLICAR EL APORTE COMPLETO
             IF f_liquida_aporte_total(lr_cta_saldo_vol.*,lr_ret_cta_vol.*,pd_fecha_liquidacion,ls_siefore_act) = FALSE THEN
                CALL f_reversa_liquidacion_nss(pc_nss,g_ultimo_folio,lr_ret_cta_vol.consecutivo)
                CALL ERRORLOG (" *** Reversa Liquidacion en f_liquida_aporte_total ")
               RETURN FALSE

             END IF

             --RESTA EL IMPORTE DEL APORTE AL PENDIENTE POR APLICAR
             LET ld_pendiente_pesos = ld_pendiente_pesos - ld_mto_aporte_pesos
          ELSE          
             --DETERMINA LA CANTIDAD DE ACCIONES POR APLICAR
             LET ld_pendiente_acciones  = ld_pendiente_pesos / ld_precio_retiro

             --APLICA LA CANTIDAD NECESARIA DEL APORTE INDICADO
             IF f_liquida_aporte_parcial(lr_cta_saldo_vol.*    ,
                                           lr_ret_cta_vol.*      ,
                                           pd_fecha_liquidacion  ,
                                           ld_pendiente_acciones ,   --pendiente por aplicar
                                           ls_siefore_act
                                           ) = FALSE THEN
                CALL f_reversa_liquidacion_nss(pc_nss,g_ultimo_folio,lr_ret_cta_vol.consecutivo)
                CALL ERRORLOG (" *** Reversa Liquidacion en f_liquida_aporte_parcial ")
                RETURN FALSE
             END IF

            LET ld_pendiente_pesos = 0
          END IF

          IF ld_pendiente_pesos =  0 THEN
             EXIT FOREACH
          END IF

       END FOREACH
   ELSE
      CALL ERRORLOG (" *** Regresa falso por v_total = "|| v_total)
      RETURN FALSE
   END IF

   MESSAGE ""

   IF ls_consec_ret = 0 THEN 
      CALL ERRORLOG (" *** Regresa falso por ls_consec_ret = "|| ls_consec_ret)
      RETURN FALSE
   END IF

   RETURN TRUE

END FUNCTION

FUNCTION f_inserta_detalle_benef(pc_nss, pd_consecutivo, pd_folio)
    DEFINE pc_nss                 CHAR(11)
    DEFINE pd_consecutivo         DECIMAL(10,0)
    DEFINE pd_folio               DECIMAL(10,0)
    DEFINE lc_query               CHAR(5000)
    DEFINE rec_benef_det     RECORD
           nss                     CHAR(11), 
           consecutivo_lote        DECIMAL(10,0),
           folio                   DECIMAL(10,0),
           subcuenta               SMALLINT,
           monto_total             DECIMAL(22,6),
           acciones_bruto          DECIMAL(22,6),
           pesos_neto              DECIMAL(22,6),
           pesos_isr               DECIMAL(22,6),
           siefore                 SMALLINT,
           precio_accion           DECIMAL(14,6),
           fecha_conversion        DATE
    END RECORD

    LET lc_query = " INSERT INTO ret_ctr_benef_det                                                                                           \n",
                   " SELECT A.nss, A.consecutivo_lote,A.folio,A.subcuenta,A.pesos+B.pesos,A.acciones+B.acciones,                             \n",
                   "            A.pesos,B.pesos,A.siefore,A.precio_accion,A.fecha_conversion                                                 \n",
                   "     FROM   (SELECT nss, consecutivo_lote,folio,subcuenta,siefore, precio_accion, fecha_conversion,                      \n",
                   "                    SUM(ABS(monto_en_acciones)) AS acciones, SUM(ABS(monto_en_pesos)) as pesos                           \n",
                   "             FROM   dis_cuenta                                                                                           \n",
                   "             WHERE  nss   = ",pc_nss,"                                                                                   \n",
                   "             AND    folio = ",pd_folio,"                                                                                 \n",
                   "             AND    consecutivo_lote = ",pd_consecutivo,"                                                                \n",
                   "             AND    tipo_movimiento <> 10                                                                                \n",
                   "             GROUP BY nss, consecutivo_lote,folio,subcuenta,siefore, precio_accion, fecha_conversion) A                  \n",
                   "            LEFT OUTER JOIN (SELECT nss, consecutivo_lote,folio,subcuenta,siefore, precio_accion, fecha_conversion,      \n",
                   "                                    SUM(ABS(monto_en_acciones)) AS acciones, SUM(ABS(monto_en_pesos)) as pesos           \n",
                   "                             FROM   dis_cuenta                                                                           \n",
                   "                             WHERE  nss   = ",pc_nss,"                                                                   \n",
                   "                             AND    folio = ",pd_folio,"                                                                 \n",
                   "                             AND    consecutivo_lote = ",pd_consecutivo,"                                                \n",
                   "                             AND    tipo_movimiento = 10                                                                 \n",
                   "                             GROUP BY nss, consecutivo_lote,folio,subcuenta,siefore, precio_accion, fecha_conversion) B  \n",
                   "                         ON  A.nss = B.nss                                                                               \n",
                   "                        AND  A.consecutivo_lote = B.consecutivo_lote                                                     \n",
                   "                        AND  A.folio = B.folio                                                                           \n",
                   "                        AND  A.subcuenta = B.subcuenta                                                                   \n",
                   "                        AND  A.siefore = B.siefore                                                                       \n",
                   "                        AND  A.precio_accion = B.precio_accion                                                           \n",
                   "                        AND  A.fecha_conversion = B.fecha_conversion                                                     \n"

    PREPARE prp_benef_det FROM lc_query
    EXECUTE prp_benef_det 
    RETURN 0
   
END FUNCTION

FUNCTION f_valida_porcentajes_beneficiarios()
    DEFINE ld_porcentaje            DECIMAL(10,2)
    DEFINE rec_beneficiarios    RECORD
        nss             CHAR(11),
        consecutivo     DECIMAL(10,0)
    END RECORD

    DECLARE cur_benef_por CURSOR FOR SELECT n_seguro, consecutivo
                                     FROM   ret_cta_vol
                                     WHERE  tipo_ret = 15
                                     AND    estado IN (0,3)
    FOREACH cur_benef_por INTO rec_beneficiarios.*
        SELECT SUM(a.porcentaje)
        INTO   ld_porcentaje
        FROM   ret_ctr_benef a,
               ret_cta_vol b
        WHERE  a.nss               = rec_beneficiarios.nss
        AND    a.nss               = b.n_seguro
        AND    a.consecutivo_solic = b.consecutivo
        AND    b.tipo_ret          = 15
        AND    b.estado            IN (0,3)
        IF ld_porcentaje > 100 THEN
            -- Se actualizan todas las solicitudes por liquidar a rechazadas
            UPDATE ret_cta_vol
            SET    estado = 20,
                   cod_rechazo_ent = "-1"
            WHERE  n_seguro    = rec_beneficiarios.nss
            AND    consecutivo = rec_beneficiarios.consecutivo
            AND    tipo_ret    = 15
            AND    estado      IN (0,3)
            
        END IF
    END FOREACH

END FUNCTION
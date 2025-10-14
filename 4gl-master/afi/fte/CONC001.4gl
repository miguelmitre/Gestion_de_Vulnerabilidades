###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa          => GENERA REGISTRO CONTABLE                                #
#Fecha             => 18 octubre 2001                                         #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha actualiza   => 17 enero 2005   multisiefores                           #
#Actualizado       => EDUARDO JOAQUIN RESENDIZ MEDINA                         #
#Fecha Actualiza   => 20 Julio 2005 - 15 Agosto 2005 - 13 Septiembre 2005     #
#                     26 Octubre 2005 - 08 Noviembre 2005 - 11 Nov 2005       #
#                     03 Mzo 2006  27 Mzo 2006                                #
#                     15 Junio-24 Julio 2006 Traspaso Afore-Afore Ced-Recep   #
#                     20 Jun/10 Ago Transferencia/Siefores  23 Ago Guber      #
#                     12 Sep 2006 00008 voluntarias por fecha                 #
#                     07 Feb 2007 00060,00061,00062 RETIROS A DETALLE         #
#                     19 Abr 2007 00048 Voluntarias Vent. LP                  #
###############################################################################
DATABASE safre_af
GLOBALS
      DEFINE g_reg   RECORD 
          folio             INTEGER,
          fecha_emision     DATE,
          fecha_valor       DATE,
          identificador     SMALLINT,
          transaccion_cod   INTEGER,
          siefore           SMALLINT,
          importe           DECIMAL(15,2),
          proceso_cod       CHAR(05),
          fecha_actualiza   DATE,
          usuario           CHAR(08),
          estado            SMALLINT
      END RECORD

      DEFINE g_reg2  RECORD 
          proceso_cod       CHAR(05),
          descripcion       CHAR(80)
      END RECORD

      DEFINE reg_3  RECORD 
          fecha_emision     DATE,
          fecha_valor       DATE,
          identificador     SMALLINT,
          transaccion_cod   INTEGER,
          importe           DECIMAL(15,2),
          proceso_cod       CHAR(05)
      END RECORD

      DEFINE #glo #integer
          cont           ,
          vsolicitado    ,
          vaceptado      ,
          vconfronta     ,
          vcodigo_afore  ,
          vlote          ,
          vfolio         ,
          vfolio1        ,
          tot_registros  INTEGER

      DEFINE #glo #date
          HOY            ,
          vfecha         ,
          f_1habil       ,
          f_hoy          DATE

      DEFINE #glo #smallint
          x_liq          ,
          dia            SMALLINT

      DEFINE #glo #char
          G_LISTA        CHAR(100),
          borra          CHAR(200),
          vclave_entidad CHAR(3),
          vproceso       CHAR(5),
          vproceso1      CHAR(5),
          vdescripcion   CHAR(80),
          vusuario       CHAR(8),
          aux_pausa      CHAR(1),
          vpregunta      ,
          enter          CHAR(1)

      DEFINE v_femi      DATE   ---erm

      DEFINE g_paramgrales    RECORD LIKE seg_modulo.*

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST
    ---> svp ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG('CONC001.log')
    CALL inicio()  #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET v_femi = ""  ---erm
    LET vfecha = ""
    LET vfolio = ""
    LET vproceso = ""
    LET vdescripcion = ""
    LET vpregunta = ""
    LET HOY = TODAY
    LET f_hoy = HOY
    LET dia   = DAY(f_hoy) - 1
    LET f_hoy = f_hoy - dia UNITS DAY
    LET f_1habil = f_hoy - 1 UNITS DAY
    CALL habil_siguiente(f_1habil) RETURNING f_1habil

    SELECT codigo_afore,
           user
    INTO   vcodigo_afore,
           vusuario
    FROM   tab_afore_local

    LET  vclave_entidad = "09 "

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

       OPEN WINDOW wnd1 AT 2,2 WITH 22 ROWS,78 COLUMNS
       ATTRIBUTE(BORDER)

       MENU "CONTABILIDAD "
           COMMAND "Ingresa" "Ingresa transacciones por folio"
              OPEN WINDOW ventana_1 AT 2,2 
              WITH FORM "CONC0011" ATTRIBUTE(BORDER)
              DISPLAY "          [ Esc ] Iniciar                            [ Ctrl-C ] Salir          " AT 3,1 ATTRIBUTE(REVERSE)
              DISPLAY "CONC001         GENERA REGISTRO CONTABLE POR PROCESO                           " AT 5,1 ATTRIBUTE(REVERSE)
              DISPLAY HOY USING "dd-mm-yyyy" AT 5,67 ATTRIBUTE(REVERSE)
                   CALL ingresa() #in
                   IF (g_reg.proceso_cod = "00041" OR 
                       g_reg.proceso_cod = "00016" OR 
                       g_reg.proceso_cod = "00021" OR 
                       g_reg.proceso_cod = "00026" OR 
                       g_reg.proceso_cod = "00033" OR
                       g_reg.proceso_cod = "00054" OR
                       g_reg.proceso_cod = "00055") THEN
                       IF g_reg.fecha_emision IS NULL OR
                          g_reg.fecha_emision = '12/31/1899' THEN
                           DISPLAY "Proceso No Registrado " AT 19,16 
                           ATTRIBUTE(REVERSE) SLEEP 3
                       ELSE
                          DISPLAY "Fecha de Registro: ", 
                          g_reg.fecha_emision USING "DD/MM/YYYY" AT 19,16 
                          ATTRIBUTE(REVERSE) SLEEP 3
                       END IF
                   ELSE
                     IF g_reg.fecha_emision IS NULL OR
                          g_reg.fecha_emision = '12/31/1899' THEN
                           DISPLAY "Proceso No Registrado " AT 19,16 
                           ATTRIBUTE(REVERSE) SLEEP 3
                       ELSE
                           DISPLAY "Fecha de Conciliacion: ", 
                           g_reg.fecha_emision USING "DD/MM/YYYY" AT 19,16 
                           ATTRIBUTE(REVERSE) SLEEP 3
                       END IF
                   END IF
                      ERROR "" 
                      CLEAR FORM
                      DISPLAY "                                                     " AT 19,16 


                      CLOSE WINDOW ventana_1
           COMMAND "Concilia" "Ingresa movimientos de conciliacion"
                   CALL concilia() #co
                   ERROR "" 
                   # CLEAR FORM

           COMMAND "Salir" "Salir de Programa"
                   EXIT MENU
       END MENU
       CLOSE WINDOW wnd1
END FUNCTION

FUNCTION ingresa()
#in---------------
  LET  vproceso     = ""
  LET  vdescripcion = ""
  LET  vfolio       = ""
  LET  vfecha       = ""
  LET  vpregunta    = ""
  INPUT BY NAME vproceso,
                vdescripcion,
                vfolio,
                vfecha,
                vpregunta WITHOUT DEFAULTS

      AFTER  FIELD vproceso

         IF vproceso IS NULL THEN
             CALL despliega_tipo()
             RETURNING vproceso,vdescripcion

            IF vproceso IS NULL THEN
               NEXT FIELD vproceso
            END IF

         ELSE
            --DISPLAY vproceso SLEEP 3
             SELECT "X"
             FROM   tab_proceso
             WHERE  proceso_cod = vproceso
             AND proceso_cod NOT IN (00012,00013,00014,00015,00018,         ---erm 3 Mzo 2006
                                     00020,00031,00032,00034,00036,00037,   ---erm 3 Mzo 2006
                                     00049)                                 ---erm 25 Sep 2007

             IF STATUS <> NOTFOUND THEN
                 SELECT descripcion
                 INTO   vdescripcion
                 FROM   tab_proceso
                 WHERE  proceso_cod = vproceso
             ELSE
                 ERROR "No existe el tipo de proceso" 
                 SLEEP 3
                 NEXT FIELD vproceso
             END IF
         END IF

         DISPLAY "Descripcion del proceso" AT 10,16 ATTRIBUTE(REVERSE)
         DISPLAY BY NAME vproceso,
                         vdescripcion

         IF (vproceso = "00022"   OR 
             #vproceso = "00008"   OR
             vproceso = "00027")  THEN
             LET vfecha = f_hoy
             DISPLAY BY NAME vfecha
             NEXT FIELD vfecha
         END IF

         IF (vproceso = "00008") THEN
            NEXT FIELD vfolio 
         END IF

----->erm 25-Oct-2005
         IF  vproceso = "00041" THEN
             NEXT FIELD vpregunta
         END IF
-----<erm 25-Oct-2005

         IF vproceso = "00033"  THEN
             LET vfecha = f_1habil
             SELECT "X"
             FROM   con_transaccion
             WHERE  proceso_cod = vproceso
             AND    fecha_emision = vfecha
             GROUP BY 1

             IF STATUS <> NOTFOUND THEN
                ERROR "YA ESTA REGISTRADO EL PROCESO EN EL HISTORICO ",
                vfecha USING "dd/mm/yyyy"
                SLEEP 3
                EXIT PROGRAM
             END IF
             DISPLAY BY NAME vfecha
             NEXT FIELD vfecha
         END IF
         NEXT FIELD vfolio

      AFTER FIELD vfolio

          SELECT "X"
          FROM   con_transaccion
          WHERE  folio       = vfolio
          AND    proceso_cod = vproceso
          --AND    proceso_cod <> '00023'
          GROUP BY 1
          IF STATUS <> NOTFOUND THEN

              IF (vproceso = "00008" OR 
                  vproceso = "00055") THEN
                 DISPLAY "Fecha de Revalorizacion " AT 16,16
                 NEXT FIELD vfecha
              END IF

{
              IF (vproceso = "00013"   OR
	          vproceso = "00018")  THEN
                  LET vfecha = f_hoy
                  DISPLAY BY NAME vfecha
                  NEXT FIELD vfecha
              END IF
}
              ERROR "YA ESTA REGISTRADO EL PROCESO EN EL HISTORICO"
              SLEEP 3
              EXIT PROGRAM
          END IF
{
          IF (vproceso = "00013"   OR
	      vproceso = "00018")  THEN
              LET vfecha = f_hoy
              DISPLAY BY NAME vfecha
              NEXT FIELD vfecha
          END IF
}

          
         IF (vproceso = "00008") THEN
            IF vfolio IS NULL THEN
               NEXT FIELD vfecha
            ELSE
               NEXT FIELD vfecha
            END IF
         END IF

          NEXT FIELD vpregunta

      AFTER FIELD vfecha
          IF (vproceso = "00033" OR
              vproceso = "00055") THEN 
             #vproceso = "00008"  THEN
              SELECT "X"
              FROM   con_transaccion
              WHERE  proceso_cod = vproceso
              AND    fecha_emision = vfecha
              GROUP BY 1

              IF STATUS <> NOTFOUND THEN
                 ERROR "YA ESTA REGISTRADO EL PROCESO EN EL HISTORICO ",
                 vfecha USING "dd/mm/yyyy"
                 SLEEP 3
                 EXIT PROGRAM
              END IF
          END IF

          IF (vproceso = "00022"   OR
              vproceso = "00027")  THEN
              SELECT "X"
              FROM   cta_tasa_viv
              WHERE  fecha_aplica = vfecha
              GROUP BY 1
              IF STATUS = NOTFOUND THEN
                  ERROR "NO HA CONCLUIDO EL PROCESO DE GENERACION DE INTERESES"
                  SLEEP 3
                  EXIT PROGRAM
              END IF
          END IF

          SELECT "X"
          FROM   con_transaccion
          WHERE  proceso_cod   = vproceso
          AND    fecha_emision = vfecha
          GROUP BY 1
          IF STATUS <> NOTFOUND THEN
              IF (vproceso = "00022"   OR
                  vproceso = "00027")  THEN
                  ERROR "YA ESTAN CONTABILIZADOS LOS INTERESES DEL PERIODO"
                  SLEEP 3
                  EXIT PROGRAM
              ELSE
                  SELECT "X"
                  FROM   con_transaccion
                  WHERE  proceso_cod   = vproceso
                  AND    folio         = vfolio
                  AND    fecha_emision = vfecha
                  GROUP BY 1
                  IF STATUS <> NOTFOUND THEN
                      ERROR "YA ESTA REGISTRADO ESTE RETIRO"
                      SLEEP 3
                      EXIT PROGRAM
                  END IF
              END IF
          END IF
          NEXT FIELD vpregunta

      AFTER FIELD vpregunta

     IF vpregunta = "S"
     OR vpregunta = "s"  THEN

         ERROR" PROCESANDO INFORMACION "
         IF vproceso >= "00001" AND
            vproceso <= "00004" THEN

             LET x_liq = ""

             CASE vproceso
                 WHEN "00001"
                     SELECT  "X"
                     FROM    dis_dep_aporte
                     WHERE   folio = vfolio
                     AND     ident_pago[14,15] = "11"
                     AND     estado = 2
                     GROUP BY 1

                     IF STATUS = NOTFOUND THEN
                         ERROR "La dispersion ya fue liquidada"
                         SLEEP 3
                         LET x_liq = 1
                     END IF

                 WHEN "00002"
                     SELECT  "X"
                     FROM    dis_dep_aporte
                     WHERE   folio = vfolio
                     AND     ident_pago[14,15] = "12"
                     AND     estado = 2
                     GROUP BY 1

                     IF STATUS = NOTFOUND THEN
                     ERROR "El archivo de Aclaraciones RCV esta liquidado"
                          SLEEP 3
                          LET x_liq = 1
                     END IF

                 WHEN "00003"
                     SELECT  "X"
                     FROM    dis_dep_aporte
                     WHERE   folio = vfolio
                     AND     ident_pago[14,15] = "21"
                     AND     estado = 2
                     GROUP BY 1

                     IF STATUS = NOTFOUND THEN
                     ERROR "El archivo de Aclaraciones RCV esta liquidado"
                          SLEEP 3
                          LET x_liq = 1
                     END IF

                 WHEN "00004"
                     SELECT  "X"
                     FROM    dis_dep_aporte
                     WHERE   folio = vfolio
                     AND     ident_pago[14,15] = "23"
                     AND     estado = 2
                     GROUP BY 1

                     IF STATUS = NOTFOUND THEN
                   ERROR "Archivo de Aclaraciones Gubernamental liquidado"
                         SLEEP 3
                         LET x_liq = 1
                     END IF
             END CASE
             IF x_liq = 1 THEN
                 ERROR "PROCESANDO INFORMACION"
                 CALL obtiene_informacion() #oi
             ELSE
                 ERROR "PROCESANDO INFORMACION"
                 CALL obtiene_provision()   #op
             END IF
         ELSE
             CASE vproceso
                 WHEN "00005"
                     CALL icefa_afore()         #ia
                 WHEN "00007"
                     CALL interes_transito()    #it
                 WHEN "00008"
                     CALL obtiene_voluntarias() #ov
                 WHEN "00011"
                     CALL reverso_icefa()       #ri
                 WHEN "00012"
                     CALL retiros_ivrt()        #rivrt
                 WHEN "00013"
                     CALL retiros_diversos()    #rd
                 WHEN "00014"
                     CALL retiros_diversos()    #rd
                 WHEN "00015"
                     CALL retiros_diversos()    #rd
                 WHEN "00016"
                     CALL obtiene_acreditados() #oa
                 WHEN "00018"
                     CALL retiros_diversos()    #rd
                 WHEN "00019"
                     CALL retiros_diversos()    #rd
                 WHEN "00020"
                     CALL retiros_diversos()    #rd
                 WHEN "00021"
                     CALL unificacion()         #du
                 WHEN "00022"
                     CALL intereses()           #i
                 WHEN "00023"
                     CALL devolucion_pagos()    #dp
                 WHEN "10023"
                     CALL devolucion_pagos_viv()    #dp
                 WHEN "00026"
                     CALL obtiene_acreditados() #oa
                 WHEN "00027"
                     CALL intereses()           #i
                 WHEN "00029"
                     CALL credito_garantia()    #cg
                 WHEN "00030"
                     CALL credito_garantia()    #cg
                 WHEN "00031"
                     CALL retiros_diversos()    #rd
                 WHEN "00032"
                     CALL retiros_diversos()    #rd
                 WHEN "00033"
                     CALL aportacion_subsecuente()    #rd
                 WHEN "00034"
                     CALL retiros_diversos()    #rd
                 WHEN "00037"
                     CALL retiros_diversos()    #rd

                 #fhh se anexo 02-Dic-2004
                 WHEN "00038"
                     CALL icefa_afore()         #ia
                 #erm 10 Ago 2006
                 WHEN "00039"
                     CALL trans_sief()          #ts
                 #fhh se anexo 02-Dic-2004
                 #erm => 24-oct-2005
                 WHEN "00041"
                     CALL intereses_fovissste()  #if
                 WHEN "00047"
                     CALL recaudacion_issste()       ---erm 18 Diciembre 2007
                 WHEN "00048"
                     CALL obtiene_informacion()      ---erm 19 Abril 2007
                 WHEN "00049"
                     CALL retiros_diversos()         ---erm 12 Julio 2007
                 WHEN "00054"
                     CALL bono_pension()         ---erm 21 Nov 2008
                 WHEN "00055"
                     CALL bono_pension_diaria()         ---erm 21 Nov 2008
                 WHEN "00060"
                     CALL retiros_diversos()         #rd
                 WHEN "00061"
                     CALL retiros_diversos()         #rd
                 WHEN "00062"
                     CALL retiros_diversos()         #rd
                 WHEN "00063"
                     CALL retiros_diversos()         #rd
                 WHEN "00064"
                     CALL retiros_diversos()         #rd
                 WHEN "00065"
                     CALL retiros_diversos()         #rd
                 WHEN "00066"
                     CALL retiros_diversos()         #rd
                 WHEN "00067"
                     CALL retiros_diversos()         #rd
                 WHEN "00068"
                     CALL retiros_diversos()         #rd
                 WHEN "00069"
                     CALL retiros_diversos()         #rd
                 WHEN "00070"
                     CALL retiros_diversos()         #rd
                 WHEN "00071"
                     CALL retiros_diversos()         #rd
                 WHEN "00072"
                     CALL retiros_diversos()         #rd
                 #erm <= 

                 OTHERWISE
                     CALL obtiene_informacion() #oi
             END CASE
         END IF

         IF g_reg.fecha_emision IS NULL OR
             g_reg.fecha_emision = '12/31/1899' THEN
              ERROR "REGISTRO NO INGRESADO" 
              SLEEP 3
              EXIT INPUT
              ERROR ""
         ELSE
              ERROR" REGISTRO INGRESADO "
              SLEEP 2
              EXIT INPUT
              ERROR ""
         END IF
     ELSE
         ERROR" PROCESO CANCELADO "
         SLEEP 2
              CALL inicio()
         EXIT INPUT
         ERROR ""
     END IF

      ON KEY (CONTROL-C)
          ERROR " PROCESO CANCELADO "
          SLEEP 2
          # CALL inicio()
          EXIT INPUT

    END INPUT

END FUNCTION

FUNCTION pregunta()
    ERROR ""

    PROMPT " DESEA GENERAR EL ARCHIVO  S/N  ?  " for CHAR aux_pausa
END FUNCTION                                                         

FUNCTION obtiene_informacion()
#oi---------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    #DEFINE subcta          CHAR(05)
    DEFINE vsiefore        SMALLINT
    DEFINE vcomision       SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE xpesos          DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    #DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


     DECLARE con_1 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_1 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_acciones = ""
         LET  total_pesos    = ""
         LET  precio         = ""
         LET  f_conversion   = ""
         LET  f_proceso      = ""

         IF (subcta = 4    OR
             subcta = 8    OR
             subcta = 14 ) THEN #fhh se anexo 02-Dic-2004
              LET principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                              " fecha_conversion, siefore ",
                              " FROM   dis_cuenta ",
                              " WHERE  folio =","'", g_reg.folio,"'",
                              " AND  (subcuenta in(","'", subcta,"'",")",
                              " OR   subcuenta in(","'", subcta1,"'","))",
                              " AND    tipo_movimiento not in(888,999) ",
                             " GROUP BY 2,3 "
              PREPARE con_11 FROM  principal
              DECLARE viv_2 CURSOR FOR con_11
              FOREACH viv_2 INTO total_pesos,f_conversion,vsiefore
                                  --,f_proceso

                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_pesos
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.identificador = 1

                 IF g_reg.proceso_cod = "00009" THEN
                     SELECT MAX(fecha_conversion)
                     INTO   f_conversion
                     FROM   dis_cuenta
                     WHERE  folio = g_reg.folio

                     LET g_reg.fecha_emision = f_conversion
                     LET g_reg.fecha_valor   = f_conversion
                 END IF
                 {IF g_reg.proceso_cod = "00001" OR
                    g_reg.proceso_cod = "00003" THEN
                     IF g_reg.transaccion_cod  = 24001 THEN
                         LET  xpesos = 0

                         SELECT SUM(monto_en_pesos)
                         INTO   xpesos
                         FROM   dis_provision
                         WHERE  folio           = g_reg.folio
                         AND    subcuenta       = 4
                         AND    tipo_movimiento = 3

                         IF xpesos IS NULL THEN
                             LET  xpesos = 0
                         END IF

                         LET g_reg.importe = g_reg.importe + xpesos

                         LET  xpesos = 0

                         SELECT SUM(monto_en_pesos)
                         INTO   xpesos
                         FROM   dis_cuenta
                         WHERE  folio           = g_reg.folio
                         AND    subcuenta       = 4
                         AND    tipo_movimiento = 7

                         IF xpesos IS NULL THEN
                             LET  xpesos = 0
                         END IF
                         LET g_reg.importe = g_reg.importe - xpesos

                     END IF
                 END IF}

                 IF g_reg.importe <> 0 THEN
                     CALL registra_historico(g_reg.*) #rh
                 END IF

              END FOREACH

         ELSE

           #arc modificar el sum(acciones)
           #fhh se anexo 02-Dic-2004

             LET vcomision = 0

             CASE vtransaccion
             WHEN 90093
               LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                " ROUND(SUM(a.monto_en_pesos),2),",
                                " a.fecha_conversion, a.siefore ",
                                " FROM   dis_cuenta a ",
                                " WHERE  a.folio =","'", g_reg.folio,"'",
                                " AND    a.subcuenta = 13 ", 
                                " AND    a.tipo_movimiento in(1,4,220,295,293) ",
--		" AND    a.id_aportante MATCHES 'TI-*' ",
                                " GROUP BY 3,4 "
              PREPARE issste_1 FROM  principal
              DECLARE c_issste CURSOR FOR issste_1
              FOREACH c_issste INTO total_acciones,total_pesos,f_conversion,
                                    vsiefore
                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_acciones
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 2

                 IF g_reg.importe <> 0 THEN
                     IF vcomision = 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                     END IF

                     LET g_reg.importe       = total_pesos
                     LET g_reg.identificador = 1
                      CALL registra_historico(g_reg.*) #rh
                 END IF
              END FOREACH
	      #fhh se anexo 02-Dic-2004

-->ejrm inicio agrega transaccion 90094 TRASPASO SAR-ISSSTE
             WHEN 90094
               LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                " ROUND(SUM(a.monto_en_pesos),2),",
                                " a.fecha_conversion, a.siefore ",
                                " FROM   dis_cuenta a ",
                                " WHERE  a.folio =","'", g_reg.folio,"'",
                                " AND    a.subcuenta = 13 ", 
                                " AND    a.tipo_movimiento = 220 ",
--                              " AND    a.id_aportante MATCHES 'TI-*' ",
                                " GROUP BY 3,4 "
              PREPARE issste_2 FROM  principal
              DECLARE c_issste2 CURSOR FOR issste_2
              FOREACH c_issste2 INTO total_acciones,total_pesos,f_conversion,
                                     vsiefore
                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_acciones
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 2

                 IF g_reg.importe <> 0 THEN
                 IF vcomision = 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                 END IF

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1
                    CALL registra_historico(g_reg.*) #rh
                 END IF
              END FOREACH
         #ejrm se anexo 12-sep-2005
--<ejrm fin de afrega transaccion 90094

-->ejrm 19 Abril 2007 inicio agrega transaccion 26006 Voluntarias Vent LP
             WHEN 26006
               LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                " ROUND(SUM(a.monto_en_pesos),2),",
                                " a.fecha_conversion, a.siefore ",
                                " FROM   dis_cuenta a ",
                                " WHERE  a.folio =","'", g_reg.folio,"'",
                                " AND    a.subcuenta = 16 ", 
                                " AND    a.tipo_movimiento = 1 ",
                                " GROUP BY 3,4 "
              PREPARE vol_lp FROM  principal
              DECLARE c_lp CURSOR FOR vol_lp
              FOREACH c_lp INTO total_acciones,total_pesos,f_conversion,
                                     vsiefore
                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_acciones
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 2

                 IF g_reg.importe <> 0 THEN
                 IF vcomision = 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                 END IF

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1
                    CALL registra_historico(g_reg.*) #rh
                 END IF
              END FOREACH
--<ejrm fin de afrega transaccion 26006

             WHEN 21615
               LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2)*(-1),",
                                " ROUND(SUM(monto_en_pesos),2)*(-1),",
                                " precio_accion, ",
                                " fecha_conversion, siefore ",
                                " FROM   dis_cuenta ",
                                " WHERE  folio =","'", g_reg.folio,"'",
                                " AND   (subcuenta in(","'", subcta,"'",")",
                                " OR    subcuenta in(","'", subcta1,"'",")",
                                " OR    subcuenta in(","'", subcta2,"'","))",
                                " AND tipo_movimiento BETWEEN 100 AND 114 ",
                                " GROUP BY 3,4,5 "

               PREPARE con_13 FROM  principal
               DECLARE rcv_2 CURSOR FOR con_13
               FOREACH rcv_2 INTO total_acciones,
                                  total_pesos,
                                  precio,
                                  f_conversion,
                                  vsiefore
                  LET g_reg.siefore       = vsiefore
                  LET g_reg.importe       = total_acciones
                  LET g_reg.fecha_valor   = f_conversion
                  LET g_reg.fecha_emision = f_conversion
                  LET g_reg.identificador = 2

                  IF g_reg.importe <> 0 THEN
                     IF vcomision = 0 THEN
                         IF  (vproceso <> "00001" OR   #erm
                              vproceso <> "00002" OR
                              vproceso <> "00003")THEN #erm
                         ELSE                          #erm
                            CALL registra_historico(g_reg.*) #rh
                         END IF
                     END IF
                     LET g_reg.importe       = total_pesos
                     LET g_reg.identificador = 1
                     CALL registra_historico(g_reg.*) #rh
                  END IF
               END FOREACH

             OTHERWISE  
               IF vtransaccion <> 20115 THEN 
                  LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   " ROUND(SUM(monto_en_pesos),2),",
                                   " precio_accion, ",
                                   " fecha_conversion, siefore ",
                                   " FROM   dis_cuenta ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND   (subcuenta in(","'", subcta,"'",")",
                                   " OR    subcuenta in(","'", subcta1,"'",")",
                                   " OR    subcuenta in(","'", subcta2,"'","))",
                                   " AND    tipo_movimiento not in(888,999) ",
                                   " GROUP BY 3,4,5 "
                  PREPARE con_12 FROM  principal
                  DECLARE rcv_1 CURSOR FOR con_12
                  FOREACH rcv_1 INTO total_acciones,total_pesos,
                                     precio,f_conversion,vsiefore
                     LET g_reg.siefore       = vsiefore
                     LET g_reg.importe       = total_acciones
                     LET g_reg.fecha_valor   = f_conversion
                     LET g_reg.fecha_emision = f_conversion
                     LET g_reg.identificador = 2

                     IF g_reg.importe <> 0 THEN
                         IF vcomision = 0 THEN
                                 CALL registra_historico(g_reg.*) #rh
                         END IF
                         LET g_reg.importe       = total_pesos
                         LET g_reg.identificador = 1
                         CALL registra_historico(g_reg.*) #rh
                     END IF
                  END FOREACH
              END IF     ---comentado 09 Dic 2005
{#inicia TRANSFERENCIA ENTRE SIEFORES erm 09 dic 2005
	   ELSE
               LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                " ROUND(SUM(monto_en_pesos),2),",
                                " precio_accion, ",
                                " fecha_conversion, siefore ",
                                " FROM   dis_cuenta ",
                                " WHERE  folio =","'", g_reg.folio,"'",
                                " AND   (subcuenta in(","'", subcta,"'",")",
                                " OR    subcuenta in(","'", subcta1,"'",")",
                                " OR    subcuenta in(","'", subcta2,"'","))",
                                " AND    tipo_movimiento not in(888,999) ",
                                " GROUP BY 3,4,5 "
              PREPARE con_erm FROM  principal
              DECLARE rcv_erm CURSOR FOR con_erm
              FOREACH rcv_erm INTO total_acciones,total_pesos,precio,f_conversion,
                                 vsiefore
                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_acciones
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 2

                 IF g_reg.importe <> 0 THEN
                   IF vcomision = 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                   END IF
                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1
                    CALL registra_historico(g_reg.*) #rh
                 END IF
              END FOREACH
           --END IF
         END IF
#termina TRANSFERENCIA ENTRE SIEFORES erm 09 Dic 2005}
               LET  vcomision = 0

              SELECT "X"
              FROM   tab_transaccion
              WHERE  transaccion_cod = g_reg.transaccion_cod
              AND    descripcion_1 MATCHES "COMISION*"
              GROUP BY 1

              IF STATUS <> NOTFOUND THEN
                 LET total_acciones = 0
                 LET total_pesos    = 0
                 LET precio         = 0
                 LET f_conversion   = ""
                 LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2)*",
                                  " (-1),",
                                  " ROUND(SUM(monto_en_pesos),2)*(-1),",
                                  " precio_accion, ",
                                  " fecha_conversion, siefore ",
                                  " FROM   dis_cuenta ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
                                  " AND   (subcuenta in(","'", subcta,"'",")",
                                  " OR    subcuenta in(","'", subcta1,"'",")",
                                  " OR    subcuenta in(","'", subcta2,"'","))",
                                  " AND tipo_movimiento BETWEEN 100 AND 114 ",
                                  " GROUP BY 3,4,5 "
                  PREPARE con_13_com FROM  principal
                  DECLARE rcv_2com CURSOR FOR con_13_com
                  FOREACH rcv_2com INTO total_acciones,
                                        total_pesos,
                                        precio,
                                        f_conversion,
                                        vsiefore
                     LET g_reg.siefore       = vsiefore
                     LET g_reg.importe       = total_acciones
                     LET g_reg.fecha_valor   = f_conversion
                     LET g_reg.fecha_emision = f_conversion
                     LET g_reg.identificador = 2

                     IF g_reg.importe <> 0 THEN
                         IF vcomision = 0 THEN
                             IF  (vproceso <> "00001" OR    #erm 11 Nov 2005
                                  vproceso <> "00002" OR
                                  vproceso <> "00003") THEN #erm 11 Nov 2005
                             ELSE                           #erm
                                CALL registra_historico(g_reg.*) #rh
                             END IF
                         END IF
                         LET g_reg.importe       = total_pesos
                         LET g_reg.identificador = 1
                         CALL registra_historico(g_reg.*) #rh
                     END IF
                     LET  vcomision     = 1
                  END FOREACH

              END IF
           END CASE
         END IF
         
     END FOREACH
END FUNCTION
FUNCTION obtiene_provision()
#op-------------------------
    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE vcomision       SMALLINT
    DEFINE vsiefore        SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10

     LET  precio      = ""
     LET  f_conversion= ""
     CALL habil_siguiente(HOY) RETURNING f_conversion

     SELECT precio_del_dia
     INTO   precio
     FROM   glo_valor_accion
     WHERE  fecha_valuacion = f_conversion
     AND    codigo_siefore = 1

     IF precio IS NULL THEN
         ERROR "NO EXISTE EL PRECIO DE LA ACCION DEL ",f_conversion USING "DD/MM/YYYY"
         SLEEP 3
         ERROR ""
         RETURN 
     END IF 

     DECLARE con_2 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_2 INTO vtransaccion,subcta,subcta1

         LET  g_reg.transaccion_cod = vtransaccion
         LET  total_pesos = ""

         IF (subcta = 4 OR
             subcta = 8 OR
             subcta = 14) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2)",
                               " FROM   dis_provision ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND    subcuenta in(","'", subcta,"'",")",
                               " AND    tipo_movimiento not in(888,999)"
              PREPARE con_15 FROM  principal
              DECLARE viv_3 CURSOR FOR con_15
              FOREACH viv_3 INTO total_pesos

              LET g_reg.importe       = total_pesos
              LET g_reg.fecha_valor   = f_conversion
              LET g_reg.fecha_emision = f_conversion
              LET g_reg.identificador = 1
              LET g_reg.siefore       = 11

              IF g_reg.importe <> 0 THEN
                  CALL registra_historico(g_reg.*) #rh
              END IF
              END FOREACH

         ELSE
               LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                   " siefore ",
                                " FROM   dis_provision ",
                                " WHERE  folio =","'", g_reg.folio,"'",
                                " AND   (subcuenta in(","'", subcta,"'",")",
                                " OR    subcuenta in(","'", subcta1,"'","))",
                                " AND    tipo_movimiento not in(888,999) ",
                                   " GROUP BY 2 "
              PREPARE con_16 FROM  principal
              DECLARE rcv_3 CURSOR FOR con_16
              FOREACH rcv_3 INTO total_pesos,vsiefore
                 LET g_reg.importe       = total_pesos
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 1
		 LET g_reg.siefore       = vsiefore

                 IF g_reg.importe <> 0 THEN
                     CALL registra_historico(g_reg.*) #rh


                     SELECT precio_del_dia
                     INTO   precio
                     FROM   glo_valor_accion
                     WHERE  fecha_valuacion = f_conversion
                     AND    codigo_siefore = g_reg.siefore

                     IF precio IS NULL THEN
                        ERROR "NO EXISTE EL PRECIO DE LA ACCION DEL ",f_conversion USING "DD/MM/YYYY"
                     SLEEP 5
                     ERROR ""
                     RETURN
                  END IF

                     LET g_reg.importe       = total_pesos / precio
                     LET g_reg.identificador = 2
                     IF vcomision = 0 THEN
                         CALL registra_historico(g_reg.*) #rh
                     END IF
                 END IF
              END FOREACH

	      LET vcomision = 0
              SELECT "X"
              FROM   tab_transaccion
              WHERE  transaccion_cod = g_reg.transaccion_cod
              AND    descripcion_1 MATCHES "COMISION*"
              GROUP BY 1

              IF STATUS <> NOTFOUND THEN
               LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2)*(-1),",
                                   " siefore ",
                                   " FROM   dis_provision ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND   (subcuenta in(","'", subcta,"'",")",
                                   " OR    subcuenta in(","'", subcta1,"'","))",
                                   " AND  tipo_movimiento BETWEEN 100 and 109 ",
                                   " GROUP BY 2 "
                  PREPARE con_17 FROM  principal
                  DECLARE rcv_4 CURSOR FOR con_17
                  FOREACH rcv_4 INTO total_pesos,vsiefore
                     LET g_reg.importe       = total_pesos
                     LET g_reg.fecha_valor   = f_conversion
                     LET g_reg.fecha_emision = f_conversion
                     LET g_reg.identificador = 1
                     LET g_reg.siefore       = vsiefore

                     IF g_reg.importe <> 0 THEN
                         CALL registra_historico(g_reg.*) #rh
                         LET g_reg.importe       = total_pesos / precio
                         LET g_reg.identificador = 2
                         IF vcomision = 0 THEN
                             IF  (vproceso <> "00001" OR    #erm 11 Nov 2005
                                  vproceso <> "00002" OR
                                  vproceso <> "00003") THEN #erm
                             ELSE                           #erm
                                CALL registra_historico(g_reg.*) #rh
                             END IF
                         END IF
                     END IF
                     LET vcomision = 1
                  END FOREACH

              END IF

         END IF
         
     END FOREACH
END FUNCTION                                                         
FUNCTION obtiene_acreditados()
#oa---------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE vfecha_sig      DATE
    DEFINE cont2           SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE vdia            SMALLINT
    DEFINE vsiefore        SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


     DECLARE con_18 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_18 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_pesos  = ""
         LET  precio       = ""
         LET  f_conversion = ""
         LET  vfecha_sig   = ""

         IF (g_reg.transaccion_cod = 24019  OR
             g_reg.transaccion_cod = 29019) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion, siefore ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND    subcuenta in(","'", subcta,"'",")",
                               " AND    tipo_movimiento in(1)",
                               " AND    id_aportante = 'DEV. INF.' ",
                             " GROUP BY 2,3 "
             PREPARE con_19 FROM  principal
             DECLARE viv_5 CURSOR FOR con_19
             FOREACH viv_5 INTO total_pesos,f_conversion,vsiefore
             END FOREACH
         END IF
         IF (g_reg.transaccion_cod = 24029  OR
             g_reg.transaccion_cod = 29029) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion, siefore ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND    subcuenta in(","'", subcta,"'",")",
                               " AND    tipo_movimiento = 4  ",
                               #" AND    id_aportante = 'DEV-INF' ",
                               " AND    id_aportante = 'DEV. INF.' ",
                             " GROUP BY 2,3 "
             PREPARE con_20 FROM  principal
             DECLARE viv_6 CURSOR FOR con_20
             FOREACH viv_6 INTO total_pesos,f_conversion,vsiefore
             END FOREACH
         END IF
         IF g_reg.transaccion_cod = 24319 THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion, siefore ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND  (subcuenta in(","'", subcta,"'",")",
                               " OR   subcuenta in(","'", subcta1,"'","))",
                               " AND    tipo_movimiento = 230  ",
                               " AND    id_aportante = 'ACR-ULT' ",
                             " GROUP BY 2,3 "
             PREPARE con_21 FROM  principal
             DECLARE viv_7 CURSOR FOR con_21
             FOREACH viv_7 INTO total_pesos,f_conversion,vsiefore
             END FOREACH
         END IF
         IF (g_reg.transaccion_cod = 24353 OR
             g_reg.transaccion_cod = 29353 ) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion, siefore ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND  (subcuenta in(","'", subcta,"'",")",
                               " OR   subcuenta in(","'", subcta1,"'","))",
                               " AND    tipo_movimiento = 230  ",
                               " AND    id_aportante = 'ACR-TRA' ",
                             " GROUP BY 2,3 "
             PREPARE con_22 FROM  principal
             DECLARE viv_8 CURSOR FOR con_22
             FOREACH viv_8 INTO total_pesos,f_conversion,vsiefore
             END FOREACH
         END IF
         IF (g_reg.transaccion_cod = 24355 OR
             g_reg.transaccion_cod = 29355 ) THEN
              LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                               " fecha_conversion, siefore ",
                               " FROM   dis_cuenta ",
                               " WHERE  folio =","'", g_reg.folio,"'",
                               " AND  (subcuenta in(","'", subcta,"'",")",
                               " OR   subcuenta in(","'", subcta1,"'","))",
                               " AND    tipo_movimiento = 235  ",
                               " AND    id_aportante = 'ACR-TRA' ",
                             " GROUP BY 2,3 "
             PREPARE con_23 FROM  principal
             DECLARE viv_9 CURSOR FOR con_23
             FOREACH viv_9 INTO total_pesos,f_conversion,vsiefore
             END FOREACH
         END IF
         LET g_reg.importe       = total_pesos
         LET g_reg.fecha_valor   = f_conversion
         --LET g_reg.fecha_emision = f_conversion      ---erm 07 Julio 2006
         LET g_reg.fecha_emision = HOY      ---erm 28 Agosto 2007
         LET g_reg.identificador = 1
         LET g_reg.siefore       = 11

         #Envia fecha valor y emision al quinto dia habil
         #comentado el día 13-julio-2005        
         {LET vfecha_sig = f_conversion
         LET vdia       = DAY(vfecha_sig)
         LET vfecha_sig = vfecha_sig - vdia UNITS DAY
         LET cont2 = 1
         FOR cont2 = 1 TO 5
             CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig
         END FOR
         LET g_reg.fecha_emision = vfecha_sig
         LET g_reg.fecha_valor   = vfecha_sig}
---se comenta con {} el día 07 Julio 2006
{
---=>    #Envia fecha valor y emision al 1er dia habil
         #21-octubre-2005        
         LET vfecha_sig = f_conversion
         LET vdia       = DAY(vfecha_sig)
         LET vfecha_sig = vfecha_sig - vdia UNITS DAY
             CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig
--->erm mes siguiente
         LET vfecha_sig = vfecha_sig + 1 UNITS MONTH          ---mas 1 mes
         LET vfecha_sig = vfecha_sig - 1 UNITS DAY
             CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig
---<erm
         LET g_reg.fecha_emision = vfecha_sig
         LET g_reg.fecha_valor   = vfecha_sig
}
         IF g_reg.fecha_emision IS NULL THEN
            SELECT UNIQUE(fecha_emision)
            INTO   v_femi
            FROM   con_transaccion
            WHERE  folio = g_reg.folio
            AND    estado = 20
              IF SQLCA.SQLCODE <> NOTFOUND THEN
                 LET g_reg.fecha_emision = v_femi
              END IF
         END IF
---<=
         IF g_reg.importe <> 0 THEN
             CALL registra_historico(g_reg.*) #rh
         END IF

     END FOREACH
END FUNCTION                                                         
FUNCTION retiros_ivrt()
#rivrt--------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE vsiefore        SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


    DECLARE con_24 CURSOR FOR

        SELECT transaccion_cod,
               subcuenta,
               subcuenta1,
               subcuenta2
        FROM   tab_transaccion
        WHERE  proceso_cod = g_reg.proceso_cod
        AND    descripcion_1 NOT MATCHES "*FRACCION"

    FOREACH con_24 INTO vtransaccion,subcta,subcta1,subcta2

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos = ""
        LET  precio      = ""
        LET  f_conversion = ""

        IF subcta = 4 THEN
            LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                             " fecha_conversion, siefore ",
                             " FROM   dis_cuenta ",
                             " WHERE  folio =","'", g_reg.folio,"'",
                             " AND  tipo_movimiento = 800 ",
                             " AND  subcuenta = 4 ",
                             " GROUP BY 2,3 "
            PREPARE con_26 FROM  principal
            DECLARE viv_11 CURSOR FOR con_26
            FOREACH viv_11 INTO total_pesos,f_conversion, vsiefore

               LET g_reg.importe       = total_pesos
               LET g_reg.fecha_valor   = f_conversion
               LET g_reg.fecha_emision = f_conversion
               LET g_reg.identificador = 1
               LET g_reg.siefore       = vsiefore

               IF g_reg.importe <> 0 THEN
                   CALL registra_historico(g_reg.*) #rh
               END IF
            END FOREACH

        ELSE
            CASE vtransaccion
                WHEN 91001
		    ---------  Invalidez
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " a.precio_accion, ",
                                  " a.fecha_conversion, a.siefore ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.nss in ( ",
                                  " SELECT b.nss ",
                                  " FROM   ret_transf_rx b ",
                                  " WHERE  b.folio =","'", g_reg.folio,"'",
                                  " AND    b.tipo_retiro = 'A' ",
                                  " AND    b.tipo_pension = 'IN') ",
                                  " AND    a.subcuenta in(1,2,5,6,9) ",
                                  " AND    a.tipo_movimiento = 800 ",
                                  " GROUP BY 2,3,4 "
                    PREPARE con_27 FROM  principal
                    DECLARE rcv_5 CURSOR FOR con_27
                    FOREACH rcv_5 INTO total_pesos,precio,f_conversion,vsiefore

                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2
	               LET g_reg.siefore       = vsiefore

                       IF g_reg.importe <> 0 THEN
		          IF g_reg.transaccion_cod >= 91001  AND
                             g_reg.transaccion_cod <= 91003  THEN
                          ELSE
                             CALL registra_historico(g_reg.*) #rh
                          END IF

                          LET g_reg.importe       = total_pesos * precio
                          LET g_reg.identificador = 1

                          CALL registra_historico(g_reg.*) #rh
                       END IF
                   END FOREACH

                 WHEN 91002
     ---------  Vida
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                  " precio_accion, ",
                                  " fecha_conversion, siefore ",
                                  " FROM   dis_cuenta ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
                                  " AND    nss in ( ",
                                  " SELECT nss ",
                                  " FROM   ret_transf_rx ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
                                  " AND    tipo_retiro = 'A' ",
                                  " AND    tipo_pension not in('IN','IP')) ",
                                  " AND    subcuenta in(1,2,5,6,9) ",
                                  " AND    tipo_movimiento = 800 ",
                                  " GROUP BY 2,3,4 "
                    PREPARE con_28 FROM  principal
                    DECLARE rcv_6 CURSOR FOR con_28
                    FOREACH rcv_6 INTO total_pesos,precio,f_conversion,vsiefore
                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2
                       LET g_reg.siefore       = vsiefore

                       IF g_reg.importe <> 0 THEN
                          IF g_reg.transaccion_cod >= 91001  AND
                             g_reg.transaccion_cod <= 91003  THEN
                          ELSE
                             CALL registra_historico(g_reg.*) #rh
                          END IF

                          LET g_reg.importe       = total_pesos * precio
                          LET g_reg.identificador = 1

                          CALL registra_historico(g_reg.*) #rh
                       END IF
                    END FOREACH

                 WHEN 91003
     ---------  Riesgo de Trabajo
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                  " precio_accion, ",
                                  " fecha_conversion, siefore ",
                                  " FROM   dis_cuenta ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
                                  " AND    nss in ( ",
                                  " SELECT nss ",
                                  " FROM   ret_transf_rx ",
                                  " WHERE  folio =","'", g_reg.folio,"'",
                                  " AND    tipo_retiro = 'A' ",
                                  " AND    tipo_pension = 'IP') ",
                                  " AND    subcuenta in(1,2,5,6,9) ",
                                  " AND    tipo_movimiento = 800 ",
                                  " GROUP BY 2,3,4 "
                    PREPARE con_29 FROM  principal
                    DECLARE rcv_7 CURSOR FOR con_29
                    FOREACH rcv_7 INTO total_pesos,precio,f_conversion,vsiefore
                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2
                       LET g_reg.siefore       = vsiefore

                       IF g_reg.importe <> 0 THEN
                          IF g_reg.transaccion_cod >= 91001  AND
                             g_reg.transaccion_cod <= 91003  THEN
                          ELSE
                             CALL registra_historico(g_reg.*) #rh
                          END IF

                          LET g_reg.importe       = total_pesos * precio
                          LET g_reg.identificador = 1

                          CALL registra_historico(g_reg.*) #rh
                       END IF
                    END FOREACH

                 OTHERWISE
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   " precio_accion, ",
                                   " fecha_conversion, siefore ",
                                   " FROM   dis_cuenta ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND   tipo_movimiento  =  800 ",
                                   " AND   (subcuenta in(","'", subcta,"'",")",
                                   " OR    subcuenta in(","'", subcta1,"'",")",
                                   " OR    subcuenta in(","'", subcta2,"'","))",
                                   " GROUP BY 2,3,4 "
                     PREPARE con_30 FROM  principal
                     DECLARE rcv_8 CURSOR FOR con_30
                     FOREACH rcv_8 INTO total_pesos,precio,f_conversion,vsiefore
                        LET g_reg.importe       = total_pesos
                        LET g_reg.fecha_valor   = f_conversion
                        LET g_reg.fecha_emision = f_conversion
                        LET g_reg.identificador = 2
                        LET g_reg.siefore       = vsiefore

                      IF g_reg.importe <> 0 THEN
                        IF g_reg.transaccion_cod >= 91001  AND
                           g_reg.transaccion_cod <= 91003  THEN
                        ELSE
                           CALL registra_historico(g_reg.*) #rh
                        END IF

                        LET g_reg.importe       = total_pesos * precio
                        LET g_reg.identificador = 1

                        CALL registra_historico(g_reg.*) #rh
                      END IF
                     END FOREACH

              END CASE

         END IF
         
    END FOREACH
END FUNCTION

FUNCTION retiros_diversos()
#rd------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE vsiefore        SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE vimporte_dif    DECIMAL(15,2)
    DEFINE vaccion_dif     DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_acciones  DECIMAL(15,2)
    DEFINE vsubcuenta      SMALLINT

    DEFINE vtrans_tip_mov  INTEGER    ---nueva por plantilla erm
    DEFINE vtip_mov        SMALLINT   ---nueva por plantilla erm
    DEFINE principal_mov   CHAR(400)  ---nueva por plantilla erm

    DEFINE vtipo_pension   CHAR(2)    ---#RETIROS A DETALLE

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


    DECLARE con_31 CURSOR FOR

        SELECT transaccion_cod,
               subcuenta,
               subcuenta1,
               subcuenta2
        FROM   tab_transaccion
        WHERE  proceso_cod = g_reg.proceso_cod
        AND    descripcion_1 NOT MATCHES "*FRACCION"

    FOREACH con_31 INTO vtransaccion,subcta,subcta1,subcta2

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos    = ""
        LET  total_acciones = ""
        LET  precio         = ""
        LET  f_conversion   = ""


-------> erm inicia 
        LET principal_mov = "SELECT transaccion_cod,tipo_movimiento ",
                            "FROM  tab_con_movimiento ",
                            "WHERE proceso_cod = ","'",g_reg.proceso_cod,"'",
                            " AND   transaccion_cod = ",vtransaccion
        PREPARE erm_1    FROM principal_mov
        DECLARE c_tipmov CURSOR FOR erm_1
        FOREACH c_tipmov INTO vtrans_tip_mov,vtip_mov

            DECLARE c_subcuenta CURSOR FOR
            SELECT subcuenta
            FROM   tab_transaccion
            WHERE  transaccion_cod = vtransaccion
            AND    proceso_cod     = vproceso

            FOREACH c_subcuenta INTO vsubcuenta
---movimientos de retiros
               IF vtip_mov >= 800 THEN
                   IF vtransaccion = 24351 OR
                      vtransaccion = 29022 OR
                      vtransaccion = 29351 THEN 

                        LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                         " fecha_conversion, siefore, subcuenta ",
                                         " FROM   dis_cuenta ",
                                         " WHERE  folio =","'", g_reg.folio,"'",
                                         " AND    tipo_movimiento = ",vtip_mov,
                                         " AND   (subcuenta in(","'", subcta,"'",")",
                                         " OR    subcuenta in(","'", subcta1,"'",")",
                                         " OR    subcuenta in(","'", subcta2,"'","))",
                                         " GROUP BY 2,3,4 "
                        PREPARE con_32 FROM  principal
                        DECLARE viv_12 CURSOR FOR con_32
                        FOREACH viv_12 INTO total_pesos,f_conversion,vsiefore

                           LET g_reg.importe       = total_pesos
                           LET g_reg.fecha_valor   = f_conversion
                           LET g_reg.fecha_emision = f_conversion
                           LET g_reg.identificador = 1
                           LET g_reg.siefore       = vsiefore

                           IF g_reg.importe <> 0 THEN
                               CALL registra_historico(g_reg.*) #rh
                           END IF
                        END FOREACH
                   ELSE                                        ---- pesos y acciones
                       LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                        " ROUND(SUM(monto_en_pesos),2), ",
                                        " fecha_conversion, siefore, subcuenta ",
                                        " FROM   dis_cuenta ",
                                        " WHERE  folio =","'", g_reg.folio,"'",
                                        " AND    tipo_movimiento = ",vtip_mov,
                                        " AND   (subcuenta in(","'", subcta,"'",")",
                                        " OR    subcuenta in(","'", subcta1,"'",")",
                                        " OR    subcuenta in(","'", subcta2,"'","))",
                                        " GROUP BY 3,4,5 "
                       PREPARE con_321 FROM  principal
                       DECLARE viv_123 CURSOR FOR con_321
                       FOREACH viv_123 INTO total_acciones,total_pesos,f_conversion,
                                            vsiefore

                          LET g_reg.importe       = total_acciones
                          LET g_reg.fecha_valor   = f_conversion
                          LET g_reg.fecha_emision = f_conversion
                          LET g_reg.identificador = 2
                          LET g_reg.siefore       = vsiefore

                              IF g_reg.importe <> 0 THEN
                                  CALL registra_historico(g_reg.*) #rh

                                  LET g_reg.importe       = total_pesos
                                  LET g_reg.identificador = 1

                                  CALL registra_historico(g_reg.*) #rh
                              END IF
                       END FOREACH
                   END IF
               END IF
-----------------------------------------------------------------------------------------
#RETIROS A DETALLE 
{                   CASE vtransaccion
                       WHEN 24351
--                   IF vtransaccion = 24351 OR
--                      vtransaccion = 29022 OR
--                      vtransaccion = 29351 THEN   ----solo pesos
                           LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                            " fecha_conversion, siefore, subcuenta ",
                                            " FROM   dis_cuenta ",
                                            " WHERE  folio =","'", g_reg.folio,"'",
                                            " AND    tipo_movimiento = ",vtip_mov,
                                            " AND   (subcuenta in(","'", subcta,"'",")",
                                            " OR    subcuenta in(","'", subcta1,"'",")",
                                            " OR    subcuenta in(","'", subcta2,"'","))",
                                            " GROUP BY 2,3,4 "
                           PREPARE con_3224351 FROM  principal
                           DECLARE viv_1224351 CURSOR FOR con_3224351
                           FOREACH viv_1224351 INTO total_pesos,f_conversion,vsiefore,
                                                    vsubcuenta

                              LET g_reg.importe       = total_pesos
                              LET g_reg.fecha_valor   = f_conversion
                              LET g_reg.fecha_emision = f_conversion
                              LET g_reg.identificador = 1
                              LET g_reg.siefore       = vsiefore

                              IF g_reg.importe <> 0 THEN
                                  CALL registra_historico(g_reg.*) #rh
                              END IF
                           END FOREACH
                       WHEN 29022
                           LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                            " fecha_conversion, siefore, subcuenta ",
                                            " FROM   dis_cuenta ",
                                            " WHERE  folio =","'", g_reg.folio,"'",
                                            " AND    tipo_movimiento = ",vtip_mov,
                                            " AND   (subcuenta in(","'", subcta,"'",")",
                                            " OR    subcuenta in(","'", subcta1,"'",")",
                                            " OR    subcuenta in(","'", subcta2,"'","))",
                                            " GROUP BY 2,3,4 "
                           PREPARE con_3229022 FROM  principal
                           DECLARE viv_1229022 CURSOR FOR con_3229022
                           FOREACH viv_1229022 INTO total_pesos,f_conversion,vsiefore,
                                                    vsubcuenta

                              LET g_reg.importe       = total_pesos
                              LET g_reg.fecha_valor   = f_conversion
                              LET g_reg.fecha_emision = f_conversion
                              LET g_reg.identificador = 1
                              LET g_reg.siefore       = vsiefore

                              IF g_reg.importe <> 0 THEN
                                  CALL registra_historico(g_reg.*) #rh
                              END IF
                           END FOREACH
                       WHEN 29351
                           LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                            " fecha_conversion, siefore, subcuenta ",
                                            " FROM   dis_cuenta ",
                                            " WHERE  folio =","'", g_reg.folio,"'",
                                            " AND    tipo_movimiento = ",vtip_mov,
                                            " AND   (subcuenta in(","'", subcta,"'",")",
                                            " OR    subcuenta in(","'", subcta1,"'",")",
                                            " OR    subcuenta in(","'", subcta2,"'","))",
                                            " GROUP BY 2,3,4 "
                           PREPARE con_3229351 FROM  principal
                           DECLARE viv_1229351 CURSOR FOR con_3229351
                           FOREACH viv_1229351 INTO total_pesos,f_conversion,vsiefore,
                                                    vsubcuenta

                              LET g_reg.importe       = total_pesos
                              LET g_reg.fecha_valor   = f_conversion
                              LET g_reg.fecha_emision = f_conversion
                              LET g_reg.identificador = 1
                              LET g_reg.siefore       = vsiefore

                              IF g_reg.importe <> 0 THEN
                                  CALL registra_historico(g_reg.*) #rh
                              END IF
                           END FOREACH
                       WHEN 21315
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " FROM   dis_cuenta  ",
                                           " WHERE  folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " GROUP BY 3,4,5"
                          PREPARE con_3211 FROM  principal
                          DECLARE viv_1231 CURSOR FOR con_3211
                          FOREACH viv_1231 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore
                                 IF g_reg.importe <> 0 THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                 END IF
                          END FOREACH
                       WHEN 22351
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " FROM   dis_cuenta  ",
                                           " WHERE  folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " GROUP BY 3,4,5"
                          PREPARE con_3212 FROM  principal
                          DECLARE viv_1232 CURSOR FOR con_3212
                          FOREACH viv_1232 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore
                                 
                                 IF g_reg.importe <> 0 THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                 END IF
                          END FOREACH
                       WHEN 23351
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " FROM   dis_cuenta  ",
                                           " WHERE  folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " GROUP BY 3,4,5"
                          PREPARE con_3213 FROM  principal
                          DECLARE viv_1233 CURSOR FOR con_3213
                          FOREACH viv_1233 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                 END IF
                          END FOREACH
                       WHEN 29400
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_3214 FROM  principal
                          DECLARE viv_1234 CURSOR FOR con_3214
                          FOREACH viv_1234 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF (vtipo_pension = 'IP' OR 
                                        vtipo_pension = 'IN') THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
                       WHEN 29401
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_3215 FROM  principal
                          DECLARE viv_1235 CURSOR FOR con_3215
                          FOREACH viv_1235 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF (vtipo_pension = 'VI' OR
                                        vtipo_pension = 'VO') THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
                       WHEN 29402
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_3216 FROM  principal
                          DECLARE viv_1236 CURSOR FOR con_3216
                          FOREACH viv_1236 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF vtipo_pension = 'RT' THEN

                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
                       WHEN 29411
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_32115 FROM  principal
                          DECLARE viv_12315 CURSOR FOR con_32115
                          FOREACH viv_12315 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF (vtipo_pension = 'IP' OR 
                                        vtipo_pension = 'IN') THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
                       WHEN 29412
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_32116 FROM  principal
                          DECLARE viv_12316 CURSOR FOR con_32116
                          FOREACH viv_12316 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF (vtipo_pension = 'VI' OR
                                        vtipo_pension = 'VO') THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
                       WHEN 29413
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_32117 FROM  principal
                          DECLARE viv_12317 CURSOR FOR con_32117
                          FOREACH viv_12317 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF vtipo_pension = 'RT' THEN

                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
--->TIPO B
                       WHEN 29403
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_3217 FROM  principal
                          DECLARE viv_1237 CURSOR FOR con_3217
                          FOREACH viv_1237 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF vtipo_pension = 'CE' THEN

                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
                       WHEN 29404
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_3218 FROM  principal
                          DECLARE viv_1238 CURSOR FOR con_3218
                          FOREACH viv_1238 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF vtipo_pension = 'CE' THEN

                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
                       WHEN 29405
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_3219 FROM  principal
                          DECLARE viv_1239 CURSOR FOR con_3219
                          FOREACH viv_1239 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF vtipo_pension = 'VE' THEN

                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH
                       WHEN 29406
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_32110 FROM  principal
                          DECLARE viv_12310 CURSOR FOR con_32110
                          FOREACH viv_12310 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF vtipo_pension = 'VE' THEN

                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH

                       WHEN 29407
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_32111 FROM  principal
                          DECLARE viv_12311 CURSOR FOR con_32111
                          FOREACH viv_12311 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF (vtipo_pension = 'IP' OR
                                        vtipo_pension = 'IN') THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH

                       WHEN 29408
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_32112 FROM  principal
                          DECLARE viv_12312 CURSOR FOR con_32112
                          FOREACH viv_12312 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF (vtipo_pension = 'IP' OR
                                        vtipo_pension = 'IN') THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH

                       WHEN 29409
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_32113 FROM  principal
                          DECLARE viv_12313 CURSOR FOR con_32113
                          FOREACH viv_12313 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF (vtipo_pension = 'VI' OR
                                        vtipo_pension = 'VO') THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH

                       WHEN 29410
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " ,b.tipo_pension ",
                                           " FROM   dis_cuenta a,ret_transf_rx b  ",
                                           " WHERE  a.folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " AND    a.folio = b.folio ",
                                           " AND    a.nss   = b.nss ",
                                           " GROUP BY 3,4,5,6"
                          PREPARE con_32114 FROM  principal
                          DECLARE viv_12314 CURSOR FOR con_32114
                          FOREACH viv_12314 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta,vtipo_pension

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                    IF (vtipo_pension = 'VI' OR
                                        vtipo_pension = 'VO') THEN
                                        CALL registra_historico(g_reg.*) #rh

                                        LET g_reg.importe       = total_pesos
                                        LET g_reg.identificador = 1

                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                 END IF
                          END FOREACH

                       OTHERWISE                                        ---- pesos y acciones
                          LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                           " ROUND(SUM(monto_en_pesos),2), ",
                                           " fecha_conversion, siefore, subcuenta ",
                                           " FROM   dis_cuenta ",
                                           " WHERE  folio =","'", g_reg.folio,"'",
                                           " AND    tipo_movimiento = ",vtip_mov,
                                           " AND   (subcuenta in(","'", subcta,"'",")",
                                           " OR    subcuenta in(","'", subcta1,"'",")",
                                           " OR    subcuenta in(","'", subcta2,"'","))",
                                           " GROUP BY 3,4,5 "
                          PREPARE con_321 FROM  principal
                          DECLARE viv_123 CURSOR FOR con_321
                          FOREACH viv_123 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore,vsubcuenta

                             LET g_reg.importe       = total_acciones
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.identificador = 2
                             LET g_reg.siefore       = vsiefore

                                 IF g_reg.importe <> 0 THEN
                                     CALL registra_historico(g_reg.*) #rh

                                     LET g_reg.importe       = total_pesos
                                     LET g_reg.identificador = 1

                                     CALL registra_historico(g_reg.*) #rh
                                 END IF
                          END FOREACH
                      END CASE 
                   END IF}
-----------------------------------------------------------------------------------------
            END FOREACH
--        END FOREACH


---- movimiento de retencion
        IF vtip_mov = 10 AND 
           (vtransaccion = 26510 OR
           vtransaccion = 26520 OR 
           vtransaccion = 26530 OR                       ---transacciones nuevas proceso 00063 10 Mayo 2007
           vtransaccion = 26540 OR                       ---transacciones nuevas proceso 00063 10 Mayo 2007
           vtransaccion = 26550) THEN                    ---transacciones nuevas proceso 00063 10 Mayo 2007
            LET  principal = --" SELECT ROUND(SUM(monto_en_pesos),2),",            ---erm 08 Mayo 2007
                             " SELECT ROUND(SUM(monto_en_acciones),2),",           ---erm 08 Mayo 2007
                             " ROUND(SUM(monto_en_pesos),2),",                     ---erm 08 Mayo 2007
                             " fecha_conversion, siefore, subcuenta ",
                             " FROM   dis_cuenta ",
                             " WHERE  folio =","'", g_reg.folio,"'",
                             " AND    tipo_movimiento = ",vtip_mov,
                             " AND   (subcuenta in(","'", subcta,"'",")",
                             " OR    subcuenta in(","'", subcta1,"'",")",
                             " OR    subcuenta in(","'", subcta2,"'","))",
                             --" GROUP BY 2,3,4 "                                  ---erm 08 Mayo 2007
                             " GROUP BY 3,4,5 "
            PREPARE con_322 FROM  principal
            DECLARE viv_122 CURSOR FOR con_322
            --FOREACH viv_122 INTO total_pesos,f_conversion,vsiefore               ---erm 08 Mayo 2007
            FOREACH viv_122 INTO total_acciones,total_pesos,f_conversion,vsiefore

               --LET g_reg.importe       = total_pesos                             ---erm 08 Mayo 2007
               LET g_reg.importe       = total_acciones                            ---erm 08 Mayo 2007
               LET g_reg.fecha_valor   = f_conversion
               LET g_reg.fecha_emision = f_conversion
               --LET g_reg.identificador = 1                                      ---erm 08 Mayo 2007
               LET g_reg.identificador = 2                                        ---erm 08 Mayo 2007
               LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos                      ---erm 08 Mayo 2007
                       LET g_reg.identificador = 1                                ---erm 08 Mayo 2007

                       CALL registra_historico(g_reg.*) #rh                       ---erm 08 Mayo 2007
                   END IF
            END FOREACH
        END IF
        END FOREACH

--comentado

        CASE vtransaccion 
            {WHEN  24315
                LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 4 ",
                                 " AND    tipo_movimiento in(820,850,880) ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                             " GROUP BY 2,3 "
                PREPARE con_32a FROM  principal
                DECLARE viv_12a CURSOR FOR con_32a
                FOREACH viv_12a INTO total_pesos,f_conversion,vsiefore
 
                   LET g_reg.importe       = total_pesos
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 1
		   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN  29301
                LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    tipo_movimiento between 820 and 880 ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                             " GROUP BY 2,3 "
                PREPARE con_33 FROM  principal
                DECLARE viv_13 CURSOR FOR con_33
                FOREACH viv_13 INTO total_pesos,f_conversion,vsiefore

                   LET g_reg.importe       = total_pesos
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 1
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN  22315
                LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                 " ROUND(SUM(a.monto_en_pesos),2), ",
                                 " a.fecha_conversion, a.siefore  ",
                                 " FROM   dis_cuenta a ",
                                 " WHERE  a.folio =","'", g_reg.folio,"'",
                                 " AND    a.subcuenta in(2,6,9) ",
                                 " AND    a.tipo_movimiento in(820,850,880) ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                                " GROUP BY 3,4 "
                PREPARE con_34 FROM  principal
                DECLARE rcv_9 CURSOR FOR con_34
                FOREACH rcv_9 INTO total_acciones,total_pesos,f_conversion,
				   vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
   
                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 23315
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5 ",
                                 " AND    tipo_movimiento in(820,850,880) ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3,4 "
                PREPARE con_35 FROM  principal
                DECLARE rcv_10 CURSOR FOR con_35
                FOREACH rcv_10 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 21315
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento in(820,850,880) ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3,4 "
                PREPARE con_36 FROM  principal
                DECLARE rcv_11 CURSOR FOR con_36
                FOREACH rcv_11 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN  26510
                LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                 " ROUND(SUM(a.monto_en_pesos),2), ",
                                 " a.fecha_conversion, a.siefore ",
                                 " FROM   dis_cuenta a ",
                                 " WHERE  a.folio =","'", g_reg.folio,"'",
                                 " AND    a.nss in ( ",
                                 " SELECT b.nss ",
                                 " FROM   ret_solicitud_tx b ",
                                 " WHERE  b.tipo_retiro in('D','G','J') ",
                                 " AND    b.folio =","'", g_reg.folio,"'",")",
                                 " AND    a.subcuenta = 1 ",
                                 " AND    a.tipo_movimiento = 10 ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3,4 "
                PREPARE con_69 FROM  principal
                DECLARE rcv_28 CURSOR FOR con_69
                FOREACH rcv_28 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
   
                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN  26511
                LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                 " ROUND(SUM(a.monto_en_pesos),2), ",
                                 " a.fecha_conversion, a.siefore ",
                                 " FROM   dis_cuenta a ",
                                 " WHERE  a.folio =","'", g_reg.folio,"'",
                                 " AND    a.nss in ( ",
                                 " SELECT b.nss ",
                                 " FROM   ret_solicitud_tx b ",
                                 " WHERE  b.tipo_retiro in('D','G','J') ",
                                 " AND    b.folio =","'", g_reg.folio,"'",")",
                                 " AND    a.subcuenta = 5 ",
                                 " AND    a.tipo_movimiento = 10 ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3,4 "
                PREPARE con_70 FROM  principal
                DECLARE rcv_29 CURSOR FOR con_70
                FOREACH rcv_29 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH
            WHEN  26512
                LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                 " ROUND(SUM(a.monto_en_pesos),2), ",
                                 " a.fecha_conversion, a.siefore ",
                                 " FROM   dis_cuenta a ",
                                 " WHERE  a.folio =","'", g_reg.folio,"'",
                                 " AND    a.nss in ( ",
                                 " SELECT b.nss ",
                                 " FROM   ret_solicitud_tx b ",
                                 " WHERE  b.tipo_retiro in('D','G','J') ",
                                 " AND    b.folio =","'", g_reg.folio,"'",")",
                                 " AND    a.subcuenta in(2,6,9) ",
                                 " AND    a.tipo_movimiento = 10 ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3,4 "
                PREPARE con_71 FROM  principal
                DECLARE rcv_30 CURSOR FOR con_71
                FOREACH rcv_30 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1
   
                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH}

            WHEN 26305
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_cta_vol ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    tipo_movimiento = 490 ",
                                 " GROUP BY 3,4 "
                PREPARE con_37 FROM  principal
                DECLARE rcv_12 CURSOR FOR con_37
                FOREACH rcv_12 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
   
                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 26307
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_cta_vol ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    tipo_movimiento = 490 ",
                                 " GROUP BY 3,4 "
                PREPARE con_43 FROM  principal
                DECLARE rcv_18 CURSOR FOR con_43
                FOREACH rcv_18 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH
#arc
            WHEN 26500
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_cta_vol ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 #" AND    subcuenta in(","'", subcta,"'",")",
                                 " AND    subcuenta in(3) ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3,4 "
                PREPARE con_67 FROM  principal
                DECLARE rcv_26 CURSOR FOR con_67
                FOREACH rcv_26 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore
                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 26501
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    nss in ( ",
                                 " SELECT n_seguro ",
                                 " FROM   ret_cta_vol ",
                                 " WHERE  folio =","'", g_reg.folio,"'",")",
                                 " AND    subcuenta in(10) ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3,4 "
                PREPARE con_72 FROM  principal
                DECLARE rcv_31 CURSOR FOR con_72
                FOREACH rcv_31 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore
                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH


            {WHEN 23301
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2), ",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5 ",
                                 #" AND    tipo_movimiento = 487 ",
				 #actualizacion 31-5
                                 " AND    tipo_movimiento = 870 ",
                                 " GROUP BY 3,4 "
                PREPARE con_38 FROM  principal
                DECLARE rcv_13 CURSOR FOR con_38
                FOREACH rcv_13 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 21303
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore  ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento = 875 ",
                                 " GROUP BY 3,4 "
                PREPARE con_39 FROM  principal
                DECLARE rcv_14 CURSOR FOR con_39
                FOREACH rcv_14 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 22303
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore  ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(2,6,9) ",
                                 #" AND    tipo_movimiento = 486 ",
				 #actualizacion 31-5
                                 " AND    tipo_movimiento = 875 ",
                                 " GROUP BY 3,4 "
                PREPARE con_40 FROM  principal
                DECLARE rcv_15 CURSOR FOR con_40
                FOREACH rcv_15 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
   
                       LET g_reg.importe       = total_pesos
                       #LET g_reg.importe       = total_pesos * precio
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 23303
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5 ",
                                 " AND    tipo_movimiento = 875 ",
                                 " GROUP BY 3,4 "
                PREPARE con_41 FROM  principal
                DECLARE rcv_16 CURSOR FOR con_41
                FOREACH rcv_16 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH
		LET vaccion_dif  = 0
		LET vimporte_dif = 0

            WHEN 28301
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 7 ",
                                 " AND    tipo_movimiento BETWEEN 820 AND 880 ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3,4 "
                PREPARE con_42 FROM  principal
                DECLARE rcv_17 CURSOR FOR con_42
                FOREACH rcv_17 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 26520
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 7 ",
                                 " AND    tipo_movimiento = 10 ",
                                 #" AND    fecha_conversion = ","'",vfecha,"'",
                                 " GROUP BY 3,4 "
                PREPARE con_68 FROM  principal
                DECLARE rcv_27 CURSOR FOR con_68
                FOREACH rcv_27 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 21500
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento = 840 ",
                                 " GROUP BY 3,4 "
                PREPARE con_78 FROM  principal
                DECLARE rcv_36 CURSOR FOR con_78
                FOREACH rcv_36 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
   
                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1
   
                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 22500
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(2,6,9) ",
                                 " AND    tipo_movimiento = 840 ",
                                 " GROUP BY 3,4 "
                PREPARE con_80 FROM  principal
                DECLARE rcv_37 CURSOR FOR con_80
                FOREACH rcv_37 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
   
                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 23500
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5  ",
                                 " AND    tipo_movimiento = 840  ",
                                 " GROUP BY 3,4 "
                PREPARE con_81 FROM  principal
                DECLARE rcv_38 CURSOR FOR con_81
                FOREACH rcv_38 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 24550
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 4  ",
                                 " AND    tipo_movimiento = 840 ",
                                 " GROUP BY 3,4 "
                PREPARE con_82 FROM  principal
                DECLARE rcv_39 CURSOR FOR con_82
                FOREACH rcv_39 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_pesos
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 1
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 26530
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    nss in( ",
                                 " SELECT nss ",
                                 " FROM   ret_solicitud_tx ",
				 " WHERE  folio =","'",g_reg.folio,"'",
				 " AND    tipo_retiro = 'F' )",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3,4 "
                PREPARE con_83 FROM  principal
                DECLARE rcv_40 CURSOR FOR con_83
                FOREACH rcv_40 INTO total_acciones,total_pesos,f_conversion,
				    vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 26531
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 5 ",
                                 " AND    nss in( ",
                                 " SELECT nss ",
                                 " FROM   ret_solicitud_tx ",
				 " WHERE  folio =","'",g_reg.folio,"'",
				 " AND    tipo_retiro = 'F') ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3,4 "
                PREPARE con_84 FROM  principal
                DECLARE rcv_41 CURSOR FOR con_84
                FOREACH rcv_41 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 26532
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(2,6,9) ",
                                 " AND    nss in( ",
                                 " SELECT nss ",
                                 " FROM   ret_solicitud_tx ",
				 " WHERE  folio =","'",g_reg.folio,"'",
				 " AND    tipo_retiro = 'F' )",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3,4 "
                PREPARE con_85 FROM  principal
                DECLARE rcv_42 CURSOR FOR con_85
                FOREACH rcv_42 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            ------- retiro 2%
            WHEN 21332
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento = 830 ",
                                 " GROUP BY 3,4 "
                PREPARE ret2_1 FROM  principal
                DECLARE ret2_2 CURSOR FOR ret2_1
                FOREACH ret2_2 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 21520
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
				 " AND    nss in( ",
				 " SELECT nss ",
				 " FROM   ret_solicitud_tx ",
				 " WHERE  folio = ","'", g_reg.folio,"'",
				 " AND    tipo_retiro = 'E') ",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento = 10 ",
                                 " GROUP BY 3,4 "
                PREPARE ret2_3 FROM  principal
                DECLARE ret2_4 CURSOR FOR ret2_3
                FOREACH ret2_4 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            ------------ ley 73 00032
            WHEN 22332
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(2,6,9)  ",
                                 " AND    tipo_movimiento in(810) ",
                                 " GROUP BY 3,4 "
                PREPARE ret2_5 FROM  principal
                DECLARE ret2_6 CURSOR FOR ret2_5
                FOREACH ret2_6 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
   
                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 23332
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(5)  ",
                                 " AND    tipo_movimiento in(810) ",
                                 " GROUP BY 3,4 "
                PREPARE ret2_7 FROM  principal
                DECLARE ret2_8 CURSOR FOR ret2_7
                FOREACH ret2_8 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 24332
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(4)  ",
                                 " AND    tipo_movimiento in(810) ",
                                 " GROUP BY 3,4 "
                PREPARE ret2_9 FROM  principal
                DECLARE ret2_10 CURSOR FOR ret2_9
                FOREACH ret2_10 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_pesos
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 1
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            ------------ pension garantizada 00034
            WHEN 21334
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta = 1 ",
                                 " AND    tipo_movimiento in(815) ",
                                 " GROUP BY 3,4 "
                PREPARE ret3_3 FROM  principal
                DECLARE ret3_4 CURSOR FOR ret3_3
                FOREACH ret3_4 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 22334
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(2,6,9)  ",
                                 " AND    tipo_movimiento in(815) ",
                                 " GROUP BY 3,4 "
                PREPARE ret3_5 FROM  principal
                DECLARE ret3_6 CURSOR FOR ret3_5
                FOREACH ret3_6 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 23334
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(5)  ",
                                 " AND    tipo_movimiento in(815) ",
                                 " GROUP BY 3,4 "
                PREPARE ret3_7 FROM  principal
                DECLARE ret3_8 CURSOR FOR ret3_7
                FOREACH ret3_8 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_acciones
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 2
                   LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_pesos
                       LET g_reg.identificador = 1

                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH

            WHEN 24334
                LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                 " ROUND(SUM(monto_en_pesos),2),",
                                 " fecha_conversion, siefore ",
                                 " FROM   dis_cuenta ",
                                 " WHERE  folio =","'", g_reg.folio,"'",
                                 " AND    subcuenta in(4)  ",
                                 " AND    tipo_movimiento in(815) ",
                                 " GROUP BY 3,4 "
                PREPARE ret3_9 FROM  principal
                DECLARE ret3_10 CURSOR FOR ret3_9
                FOREACH ret3_10 INTO total_acciones,total_pesos,f_conversion,
		vsiefore

                   LET g_reg.importe       = total_pesos
                   LET g_reg.fecha_valor   = f_conversion
                   LET g_reg.fecha_emision = f_conversion
                   LET g_reg.identificador = 1
                   LET g_reg.siefore       = vsiefore
   
                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                   END IF
                END FOREACH}
        END CASE
    END FOREACH
END FUNCTION                                                         

FUNCTION unificacion()
#ou-----------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE max_fecha       DATE
    #DEFINE subcta          CHAR(05)
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE tipo_mov        SMALLINT
    DEFINE i               SMALLINT
    DEFINE vdia            SMALLINT
    DEFINE vsiefore        SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE pesos           DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


     DECLARE con_44 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_44 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_pesos = ""
         LET  precio      = ""
         LET  f_conversion = ""

         IF (subcta = 4 OR
             subcta = 8 OR
             subcta = 14) THEN
             LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                              " fecha_conversion, siefore ",
                              " FROM   dis_cuenta ",
                              " WHERE  folio =","'", g_reg.folio,"'",
                              " AND   ( subcuenta in(","'", subcta,"'",")",
                              " OR    subcuenta in(","'", subcta1,"'","))",
                              " AND    tipo_movimiento in(1,4)",
                             " GROUP BY 2,3 "
             PREPARE con_45 FROM  principal
             DECLARE viv_10 CURSOR FOR con_45
             FOREACH viv_10 INTO total_pesos,f_conversion, vsiefore
                 LET g_reg.importe       = total_pesos
                 LET i                   = 1
                 LET max_fecha = f_conversion                 --erm oct
---erm comentado con {} el día 10 Julio 2006
{
                 LET vdia      = DAY(max_fecha)               --erm oct
                 LET max_fecha = max_fecha - vdia UNITS DAY   --erm oct
---                 LET max_fecha           = f_conversion -1 UNITS DAY
---                 FOR i = 1 TO 5      ---comentado erm 21-10-2005
                      CALL habil_siguiente(max_fecha) RETURNING max_fecha
---                 END FOR             ---comentado erm 21-10-2005
--->erm mes siguiente
                 LET max_fecha = max_fecha + 1 UNITS MONTH          ---mas 1 mes
                 LET max_fecha = max_fecha - 1 UNITS DAY
                     CALL habil_siguiente(max_fecha) RETURNING max_fecha
---<erm
}
                 --LET g_reg.fecha_emision = max_fecha
                 LET g_reg.fecha_emision = HOY             ---28 Agosto 2007
                 LET g_reg.fecha_valor   = max_fecha
                 LET g_reg.siefore       = vsiefore
                 LET g_reg.identificador = 1

--->erm 26-Oct-2005
                IF g_reg.fecha_emision IS NULL THEN
                   SELECT UNIQUE(fecha_emision)
                   INTO   v_femi
                   FROM   con_transaccion
                   WHERE  folio = g_reg.folio
                   AND    estado = 20
                     IF SQLCA.SQLCODE <> NOTFOUND THEN
                        LET g_reg.fecha_emision = v_femi
                     END IF
                END IF
---<erm 26-Oct-2005

                 IF g_reg.importe > 0 THEN
                     CALL registra_historico(g_reg.*) #rh
                 END IF
             END FOREACH
         ELSE
             LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2), ",
                              " fecha_conversion, siefore ",
                              " FROM   dis_cuenta ",
                              " WHERE  folio =","'", g_reg.folio,"'",
                              " AND   (subcuenta in(","'", subcta,"'",")",
                              " OR    subcuenta in(","'", subcta1,"'",")",
                              " OR    subcuenta in(","'", subcta2,"'","))",
                              " AND    tipo_movimiento = 1 ",
                             " GROUP BY 2,3 "
            PREPARE con_46 FROM  principal
            DECLARE rcv_19 CURSOR FOR con_46
            FOREACH rcv_19 INTO pesos,f_conversion,vsiefore
                LET g_reg.importe       = pesos
                LET g_reg.fecha_valor   = f_conversion
                --LET g_reg.fecha_emision = f_conversion
                LET g_reg.fecha_emision = HOY                 ---28 Agosto 2007
                LET g_reg.siefore       = vsiefore
                LET g_reg.identificador = 1
---erm comentado con {} el día 07 Julio 2006
{
         #erm Envia fecha valor y emision al quinto dia habil
         #erm comentado el día 21-octubre-2005 con esto lo pasa al 1er dia habil
          ---      LET i = 1
                LET max_fecha = f_conversion
                LET vdia      = DAY(max_fecha)
                LET max_fecha = max_fecha - vdia UNITS DAY
          ---  FOR i = 1 TO 5
                   CALL habil_siguiente(max_fecha) RETURNING max_fecha
          ---       END FOR
--->erm mes siguiente
                 LET max_fecha = max_fecha + 1 UNITS MONTH          ---mas 1 mes
                 LET max_fecha = max_fecha - 1 UNITS DAY
                     CALL habil_siguiente(max_fecha) RETURNING max_fecha
---<erm
                LET g_reg.fecha_valor   = max_fecha
                LET g_reg.fecha_emision = max_fecha
}
--->erm 26-Oct-2005
                IF g_reg.fecha_emision IS NULL THEN
                   SELECT UNIQUE(fecha_emision)
                   INTO   v_femi
                   FROM   con_transaccion
                   WHERE  folio = g_reg.folio
                   AND    estado = 20
                     IF SQLCA.SQLCODE <> NOTFOUND THEN
                        LET g_reg.fecha_emision = v_femi
                     END IF
                END IF
---<erm 26-Oct-2005

                IF g_reg.importe > 0 THEN
                    CALL registra_historico(g_reg.*) #rh
                    LET g_reg.importe       = pesos
                    LET g_reg.identificador = 1
                END IF
            END FOREACH
         END IF
     END FOREACH
END FUNCTION

FUNCTION reverso_icefa()
#ri---------------------
    DEFINE vtransaccion       INTEGER
    DEFINE f_conversion       DATE
    DEFINE f_conversion_viv   DATE
    DEFINE fhoy               DATE
    DEFINE fhoy1              DATE
    #DEFINE subcta            CHAR(05)
    DEFINE vsiefore           SMALLINT
    DEFINE dia                SMALLINT
    DEFINE mes                SMALLINT
    DEFINE subcta             SMALLINT
    DEFINE subcta1            SMALLINT
    DEFINE subcta2            SMALLINT
    DEFINE tipo_mov           SMALLINT
    DEFINE principal          CHAR(350)
    DEFINE precio             DECIMAL(10,6)
    DEFINE importe            DECIMAL(15,2)
    DEFINE total_pesos        DECIMAL(15,2)
    DEFINE vviv_pesos         DECIMAL(15,2)
    DEFINE vsar_comision      DECIMAL(15,2)
    DEFINE vsar_comision_ant  DECIMAL(15,2)
    DEFINE vsar_pesos         DECIMAL(15,2)
    DEFINE vsar_pesos_ant     DECIMAL(15,2)
    DEFINE vsar_accion        DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10
    LET mes                   = 1
    LET vviv_pesos            = 0
    LET vsar_pesos            = 0
    LET vsar_pesos_ant        = 0
    LET vsar_accion           = 0


     DECLARE con_50 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_50 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion
         LET  total_pesos           = ""
         LET  precio                = ""
         LET  f_conversion          = ""
         IF subcta = 8 THEN
             SELECT SUM(saldo_viv_92),
                    fecha_devol
             INTO   vviv_pesos,
                    f_conversion
             FROM   dev_det_normal
             WHERE  folio_devol  = g_reg.folio
             GROUP BY 2

             IF vviv_pesos IS NULL THEN
                 LET vviv_pesos = 0
             END IF

             LET g_reg.importe       = vviv_pesos * -1
             LET g_reg.fecha_valor   = f_conversion
             LET g_reg.fecha_emision = f_conversion
             LET g_reg.identificador = 1
	     LET g_reg.siefore       = 11

             IF g_reg.importe <> 0 THEN
                CALL registra_historico(g_reg.*) #rh
             END IF

         ELSE
             IF g_reg.transaccion_cod < 21515 THEN
                 SELECT SUM(saldo_sar_pagar),
                        ROUND(SUM(acciones_sar_92),2),
                        ROUND(SUM(saldo_sar_92_orig),2),
                        fecha_devol
                 INTO   vsar_pesos,
                        vsar_accion,
                        vsar_pesos_ant,
                        f_conversion
                 FROM   dev_det_normal
                 WHERE  folio_devol  = g_reg.folio
                 GROUP BY 4

                 IF vsar_pesos IS NULL THEN
                     LET vsar_pesos = 0
                 END IF
                 IF vsar_accion IS NULL THEN
                     LET vsar_accion = 0
                 END IF
                 IF vsar_pesos_ant IS NULL THEN
                     LET vsar_pesos_ant = 0
                 END IF

                 LET g_reg.importe       = vsar_accion * -1
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 2
                 LET g_reg.siefore       = vsiefore
	         LET vsar_comision       = 0

		 SELECT ROUND(SUM(monto_en_pesos),2)
		 INTO   vsar_comision
		 FROM   dis_cuenta
		 WHERE  folio = g_reg.folio
		 AND    tipo_movimiento = 110

                 IF STATUS = NOTFOUND THEN
                     LET vsar_comision = 0
                 END IF

                 IF g_reg.importe <> 0 THEN
                     IF g_reg.transaccion_cod <> 21512  THEN
                         CALL registra_historico(g_reg.*) #rh
        
                         LET g_reg.importe       = vsar_pesos * -1
                         LET g_reg.identificador = 1
	                 LET  g_reg.importe      = g_reg.importe +
					           vsar_comision

                         CALL registra_historico(g_reg.*) #rh
                     ELSE
                         LET g_reg.importe       = vsar_pesos_ant * -1
                         LET g_reg.identificador = 1

                         CALL registra_historico(g_reg.*) #rh
                     END IF
                 END IF
             ELSE
	         LET vsar_comision       = 0
	         LET vsar_comision_ant   = 0
                 LET g_reg.siefore       = vsiefore

                 SELECT fecha_devol
                 INTO   f_conversion
                 FROM   dev_det_normal
                 WHERE  folio_devol  = g_reg.folio
                 GROUP BY 1

                 IF g_reg.transaccion_cod = 21515 THEN
		     SELECT ROUND(SUM(comis_actual),2)
                     INTO   vsar_comision
                     FROM   safre_tmp:dev_detalle_comis 
		     WHERE  folio_devol = g_reg.folio

		     IF vsar_comision IS NULL THEN
		         LET vsar_comision = 0
		     END IF
                     LET g_reg.importe = vsar_comision * -1
                 ELSE
		     SELECT ROUND(SUM(comis_anterior),2)
                     INTO   vsar_comision_ant
                     FROM   safre_tmp:dev_detalle_comis 
		     WHERE  folio_devol = g_reg.folio

		     IF vsar_comision_ant IS NULL THEN
		         LET vsar_comision_ant = 0
		     END IF
                     LET g_reg.importe = vsar_comision_ant * -1
                 END IF
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 1
                 LET g_reg.siefore       = vsiefore

                 IF g_reg.importe <> 0 THEN
                      CALL registra_historico(g_reg.*) #rh
                 END IF
             END IF 
         END IF
     END FOREACH
END FUNCTION

FUNCTION intereses()
#i------------------

    DEFINE vtransaccion    INTEGER
    DEFINE xfolio          INTEGER
    DEFINE f_conversion    DATE
    DEFINE vfecha_sig      DATE
    DEFINE vfecha_fin      DATE
    DEFINE xfecha_ini      DATE
    DEFINE xfecha_fin      DATE
    DEFINE f_hoy           DATE
    DEFINE vid             CHAR(11)
    DEFINE vsiefore        SMALLINT
    DEFINE dia             SMALLINT
    DEFINE vdia            SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE cont2           SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)
    DEFINE xpesos          DECIMAL(15,2)
    DEFINE xpesos1         DECIMAL(15,2)

    LET  g_reg.folio           = vfolio
    LET  g_reg.proceso_cod     = vproceso
    LET  g_reg.fecha_actualiza = HOY
    LET  g_reg.usuario         = vusuario
    LET  g_reg.estado          = 10

    LET  f_hoy       = ""
    LET  xfecha_ini  = ""
    LET  xfecha_fin  = ""
    LET  f_hoy       = HOY
    LET  dia         = DAY(f_hoy)
    LET  xfecha_fin  = f_hoy - dia UNITS DAY
    LET  dia         = DAY(xfecha_fin) -1
    LET  xfecha_ini  = xfecha_fin - dia UNITS DAY
    LET  total_pesos = ""
    LET  precio      = ""
    LET  f_conversion = vfecha

    DECLARE con_10 CURSOR FOR
        SELECT ROUND(SUM(monto_en_pesos),2),
               subcuenta,
               id_aportante
        FROM   safre_tmp:dis_cuenta_int
        WHERE  nss > 0
        AND    subcuenta in(4,8)
	#AND    tipo_movimiento <> 7
        AND    fecha_valor     <= vfecha
        AND    fecha_conversion = vfecha
        GROUP BY 2,3
    FOREACH con_10 INTO total_pesos,subcta,vid
        CASE subcta
            WHEN 4
                IF vid = "INFONAVIT" THEN
                    LET g_reg.transaccion_cod = 24500
		    LET xfolio = ""
		    LET xpesos = ""
		    LET xpesos1 = 0
		    DECLARE int_1 CURSOR FOR
		       SELECT folio
		       FROM   con_transaccion
		       WHERE  proceso_cod  = "00003"
		       AND    fecha_emision BETWEEN xfecha_ini
					    AND     xfecha_fin
                       GROUP BY 1
		    FOREACH int_1 INTO xfolio

		       SELECT SUM(monto_en_pesos)
		       INTO   xpesos
		       FROM   dis_provision
		       WHERE  folio           = xfolio
		       AND    subcuenta       = 4
		       AND    tipo_movimiento = 3

                       IF xpesos is null then
                           LET xpesos = 0
                       END IF
                       LET xpesos1 = xpesos1 + xpesos
		    END FOREACH
		    LET total_pesos = total_pesos - xpesos1
                ELSE
                    LET g_reg.transaccion_cod = 24502
                END IF
            WHEN 8
                IF vid = "INFONAVIT" THEN
                    LET g_reg.transaccion_cod = 24510
                ELSE
                    LET g_reg.transaccion_cod = 24503
                END IF
        END CASE 

        LET g_reg.siefore       = 11
        LET g_reg.importe       = total_pesos
        LET g_reg.fecha_valor   = f_conversion
        LET g_reg.fecha_emision = f_conversion
        LET g_reg.identificador = 1

        LET vfecha_sig = f_conversion
        LET vfecha_fin = f_conversion
        LET vdia       = DAY(vfecha_sig)
        LET vfecha_sig = vfecha_sig - vdia UNITS DAY
        LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
        LET cont2 = 1
        FOR cont2 = 1 TO 5
            CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig
        END FOR
        LET vfecha_fin = vfecha_fin - 1 UNITS DAY
        CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin
        LET g_reg.fecha_emision = vfecha_sig
        LET g_reg.fecha_valor   = vfecha_sig

        IF g_reg.importe <> 0 THEN
            CALL registra_historico(g_reg.*) #rh
            LET g_reg.proceso_cod     = "00027"
            LET g_reg.importe         = g_reg.importe * -1
            LET g_reg.fecha_emision   = vfecha_fin
            LET g_reg.fecha_valor     = vfecha_fin
            CALL registra_historico(g_reg.*) #rh
            LET g_reg.proceso_cod     = vproceso
        END IF
    END FOREACH

END FUNCTION

FUNCTION icefa_afore()
#ia-------------------
    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE vsiefore        SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos1    DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


    DECLARE con_52 CURSOR FOR

        SELECT transaccion_cod,
               subcuenta,
               subcuenta1,
               subcuenta2
        FROM   tab_transaccion
        WHERE  proceso_cod = g_reg.proceso_cod
        AND    descripcion_1 NOT MATCHES "*FRACCION"

    FOREACH con_52 INTO vtransaccion,subcta,subcta1,subcta2

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos           = ""
        LET  total_pesos1          = ""
        LET  precio                = ""
        LET  f_conversion          = ""

        IF  subcta = 8  OR
	    subcta = 14 THEN
            LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                             " fecha_conversion, siefore ",
                             " FROM   dis_cuenta ",
                             " WHERE  folio =","'", g_reg.folio,"'",
                             " AND  (subcuenta in(","'", subcta,"'",")",
                             " OR   subcuenta in(","'", subcta1,"'","))",
                             " AND    tipo_movimiento not in(888,999) ",
                             " GROUP BY 2,3 "
            PREPARE con_60 FROM  principal
            DECLARE viv_15 CURSOR FOR con_60
            FOREACH viv_15 INTO total_pesos,f_conversion,vsiefore

               LET g_reg.importe       = total_pesos
               LET g_reg.fecha_valor   = f_conversion
               LET g_reg.fecha_emision = f_conversion
               LET g_reg.identificador = 1
	       LET g_reg.siefore       = vsiefore

               IF g_reg.importe <> 0 THEN
                   CALL registra_historico(g_reg.*) #rh
               END IF
            END FOREACH

        ELSE
            CASE vtransaccion
                WHEN 90091
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion, a.siefore ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta in(","'", subcta,"'",")",
                                  " AND    a.tipo_movimiento = 3 ",
                                  " GROUP BY 3,4 "
                    PREPARE con_61 FROM  principal
                    DECLARE rcv_20 CURSOR FOR con_61
                    FOREACH rcv_20 INTO total_pesos,total_pesos1,f_conversion,
		    vsiefore
                       LET g_reg.siefore       = vsiefore
                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2

                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           LET g_reg.importe       = total_pesos1
                           LET g_reg.identificador = 1
                           CALL registra_historico(g_reg.*) #rh
                       END IF
                    END FOREACH

                WHEN 90092
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion, a.siefore ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta in(","'", subcta,"'",")",
                                  " AND    a.tipo_movimiento = 5 ",
                                 " GROUP BY 3,4 "
                    PREPARE con_62 FROM  principal
                    DECLARE rcv_21 CURSOR FOR con_62
                    FOREACH rcv_21 INTO total_pesos,total_pesos1,f_conversion,
		    vsiefore
                       LET g_reg.siefore       = vsiefore
                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2

                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           LET g_reg.importe       = total_pesos1
                           LET g_reg.identificador = 1
                           CALL registra_historico(g_reg.*) #rh
                       END IF
                    END FOREACH

                #fhh se anexo 02-Dic-2004
                WHEN 90093
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                  " ROUND(SUM(a.monto_en_pesos),2), ",
                                  " a.fecha_conversion, a.siefore ",
                                  " FROM   dis_cuenta a ",
                                  " WHERE  a.folio =","'", g_reg.folio,"'",
                                  " AND    a.subcuenta = 13 ",
                                  " AND    a.tipo_movimiento in (1,4) ",
--		  " AND    a.id_aportante MATCHES 'TI-*' ",
                                 " GROUP BY 3,4 "
                    PREPARE con_63a FROM  principal
                    DECLARE rcv_22a CURSOR FOR con_63a
                    FOREACH rcv_22a INTO total_pesos,total_pesos1,f_conversion,
		    vsiefore
                       LET g_reg.siefore       = vsiefore
                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2
         
                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           LET g_reg.importe       = total_pesos1
                           LET g_reg.identificador = 1
                           CALL registra_historico(g_reg.*) #rh
                       END IF
                    END FOREACH
                 #fhh se anexo 02-Dic-2004

                 OTHERWISE
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   " ROUND(SUM(monto_en_pesos),2), ",
                                   " fecha_conversion, siefore ",
                                   " FROM   dis_cuenta ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND    subcuenta in(","'", subcta,"'",")",
                                   " AND    tipo_movimiento in(1,4) ",
                                   " GROUP BY 3,4 "
                     PREPARE con_63 FROM  principal
                     DECLARE rcv_22 CURSOR FOR con_63
                     FOREACH rcv_22 INTO total_pesos,total_pesos1,f_conversion,
		     vsiefore
                       LET g_reg.siefore       = vsiefore
                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2
         
                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           LET g_reg.importe       = total_pesos1
                           LET g_reg.identificador = 1
                           CALL registra_historico(g_reg.*) #rh
                       END IF
                     END FOREACH

              END CASE
         END IF
         
    END FOREACH
END FUNCTION                                                         
FUNCTION interes_transito()
#ia-------------------
    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE vsiefore        SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos1    DECIMAL(15,2)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


    DECLARE int_t1 CURSOR FOR

        SELECT transaccion_cod,
               subcuenta,
               subcuenta1,
               subcuenta2
        FROM   tab_transaccion
        WHERE  proceso_cod = g_reg.proceso_cod
        AND    descripcion_1 NOT MATCHES "*FRACCION"

    FOREACH int_t1 INTO vtransaccion,subcta,subcta1,subcta2

        LET  g_reg.transaccion_cod = vtransaccion
        LET  total_pesos   = ""
        LET  total_pesos1  = ""
        LET  precio        = ""
        LET  f_conversion  = ""

            CASE vtransaccion
                WHEN 21001
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                   " ROUND(SUM(a.monto_en_pesos),2), ",
                                   " a.fecha_conversion, a.siefore ",
                                   " FROM   dis_cuenta a ",
                                   " WHERE  a.folio =","'", g_reg.folio,"'",
                                   " AND    a.subcuenta = 1 ",
                                   " AND    a.tipo_movimiento = 3 ",
                                   " GROUP BY 3,4 "
                    PREPARE int_t2 FROM  principal
                    DECLARE int_t3 CURSOR FOR int_t2
                    FOREACH int_t3 INTO total_pesos,total_pesos1,f_conversion,
		    vsiefore
                        LET g_reg.importe       = total_pesos
                        LET g_reg.fecha_valor   = f_conversion
                        LET g_reg.fecha_emision = f_conversion
                        LET g_reg.siefore       = vsiefore
                        LET g_reg.identificador = 2
                        IF g_reg.importe <> 0 THEN
                            CALL registra_historico(g_reg.*) #rh
                            LET g_reg.importe       = total_pesos1
                            LET g_reg.identificador = 1
                            CALL registra_historico(g_reg.*) #rh
                        END IF
                    END FOREACH

                WHEN 22001
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                   " ROUND(SUM(a.monto_en_pesos),2), ",
                                   " a.fecha_conversion, a.siefore ",
                                   " FROM   dis_cuenta a ",
                                   " WHERE  a.folio =","'", g_reg.folio,"'",
                                   " AND    a.subcuenta in(2,6,9) ",
                                   " AND    a.tipo_movimiento = 3 ",
                                   " GROUP BY 3,4 "
                    PREPARE int_t4 FROM  principal
                    DECLARE int_t5 CURSOR FOR int_t4
                    FOREACH int_t5 INTO total_pesos,total_pesos1,f_conversion,
		    vsiefore
                        LET g_reg.importe       = total_pesos
                        LET g_reg.fecha_valor   = f_conversion
                        LET g_reg.fecha_emision = f_conversion
                        LET g_reg.siefore       = vsiefore
                        LET g_reg.identificador = 2
                        IF g_reg.importe <> 0 THEN
                            CALL registra_historico(g_reg.*) #rh
                            LET g_reg.importe       = total_pesos1
                            LET g_reg.identificador = 1
                            CALL registra_historico(g_reg.*) #rh
                        END IF
                    END FOREACH

                 WHEN 23001
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                     " ROUND(SUM(monto_en_pesos),2), ",
                                     " fecha_conversion, siefore ",
                                     " FROM   dis_cuenta ",
                                     " WHERE  folio =","'", g_reg.folio,"'",
                                     " AND    subcuenta = 5 ",
                                     " AND    tipo_movimiento = 3  ",
                                     " GROUP BY 3,4 "
                     PREPARE int_t6 FROM  principal
                     DECLARE int_t7 CURSOR FOR int_t6
                     FOREACH int_t7 INTO total_pesos,total_pesos1,f_conversion,
		     vsiefore
                        LET g_reg.importe       = total_pesos
                        LET g_reg.fecha_valor   = f_conversion
                        LET g_reg.fecha_emision = f_conversion
                        LET g_reg.siefore       = vsiefore
                        LET g_reg.identificador = 2
                        IF g_reg.importe <> 0 THEN
                            CALL registra_historico(g_reg.*) #rh
                            LET g_reg.importe       = total_pesos1
                            LET g_reg.identificador = 1
                            CALL registra_historico(g_reg.*) #rh
                        END IF
                     END FOREACH

                 WHEN 26003
                    LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                     " ROUND(SUM(monto_en_pesos),2), ",
                                     " fecha_conversion, siefore ",
                                     " FROM   dis_cuenta ",
                                     " WHERE  folio =","'", g_reg.folio,"'",
                                     " AND    subcuenta = 3 ",
                                     " AND    tipo_movimiento = 3  ",
                                     " GROUP BY 3,4 "
                     PREPARE int_t8 FROM  principal
                     DECLARE int_t9 CURSOR FOR int_t8
                     FOREACH int_t9 INTO total_pesos,total_pesos1,f_conversion,
		     vsiefore
                        LET g_reg.importe       = total_pesos
                        LET g_reg.fecha_valor   = f_conversion
                        LET g_reg.fecha_emision = f_conversion
                        LET g_reg.siefore       = vsiefore
                        LET g_reg.identificador = 2
                        IF g_reg.importe <> 0 THEN
                            CALL registra_historico(g_reg.*) #rh
                            LET g_reg.importe       = total_pesos1
                            LET g_reg.identificador = 1
                            CALL registra_historico(g_reg.*) #rh
                        END IF
                     END FOREACH
--->02 Oct 2006 ---19 Abril 2007 
                 WHEN 26010
                     LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                      " ROUND(SUM(monto_en_pesos),2),",
                                      " precio_accion, ",
                                      " fecha_conversion, siefore ",
                                      " FROM   dis_cuenta ",
                                      " WHERE  folio =","'", g_reg.folio,"'",
                                      " AND   (subcuenta in(","'", subcta,"'",")",
                                      " OR    subcuenta in(","'", subcta1,"'",")",
                                      " OR    subcuenta in(","'", subcta2,"'","))",
                                      " AND    tipo_movimiento = 3 ",
                                      " GROUP BY 3,4,5 "
                     PREPARE con_00007 FROM  principal
                     DECLARE rcv_00007 CURSOR FOR con_00007
                     FOREACH rcv_00007 INTO total_pesos,total_pesos1,
                                        precio,f_conversion,vsiefore
                        LET g_reg.siefore       = vsiefore
                        LET g_reg.importe       = total_pesos
                        LET g_reg.fecha_valor   = f_conversion
                        LET g_reg.fecha_emision = f_conversion
                        LET g_reg.identificador = 2
                     
                        IF g_reg.importe <> 0 THEN
                                    CALL registra_historico(g_reg.*) #rh
                            LET g_reg.importe       = total_pesos1
                            LET g_reg.identificador = 1
                            CALL registra_historico(g_reg.*) #rh
                        END IF
                     END FOREACH
---<
              END CASE

    END FOREACH
END FUNCTION
FUNCTION devolucion_pagos()
#dp------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE vf_conversion   DATE
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    #DEFINE subcta          CHAR(05)
    DEFINE vsiefore        SMALLINT
    DEFINE vcomision       SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    #DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos1    DECIMAL(22,6)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)
    DEFINE vproc12         CHAR(5)
    DEFINE vtran12         SMALLINT
    DEFINE vcuenta12       CHAR(9)
    DEFINE vtipo12         CHAR(1)
    DEFINE vfecha_sig      DATE
    DEFINE vfecha_fin      DATE
    DEFINE vdia            SMALLINT
    DEFINE cont2           SMALLINT
    DEFINE vtransacc12     CHAR(500)

    DEFINE vtransacc       RECORD LIKE con_transaccion.*
    DEFINE vtransacc2      RECORD LIKE con_transaccion.*

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10

     DECLARE con_79 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_79 INTO vtransaccion,subcta,subcta1,subcta2

#############=> ejrm 24-Oct-2005

         DECLARE c_tran12 CURSOR FOR
         SELECT proceso_cod,transaccion_cod,cuenta,tipo
         FROM   con_traductor
         WHERE  transaccion_cod = vtransaccion
         AND    proceso_cod = g_reg.proceso_cod

         FOREACH c_tran12 INTO vproc12,vtran12,vcuenta12,vtipo12
-------------------------------------------------------------------------------

            LET vtransacc12 = "SELECT * ",
                              "FROM  con_transaccion ",
                              "WHERE transaccion_cod    = 21150 ",
                              --"OR transaccion_cod = 21151) ",
                              "AND   folio               = ", vfolio,
                              " AND   fecha_emision       = ","'",vfecha_sig,"'",
                              --" AND   fecha_emision       = ","'",vfecha,"'",
                              " AND   siefore IN (1,2)"
            PREPARE confirma FROM vtransacc12
            DECLARE c_transacc CURSOR FOR confirma
            FOREACH c_transacc INTO vtransacc.*
            END FOREACH
            IF vtransacc.folio  <> vfolio  THEN  

                IF vtran12   = 21150 AND
                   --vcuenta12 = "713401333" AND
                   vcuenta12 = "713401" AND
                   vtipo12   = "A" THEN

                         LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                          " ROUND(SUM(a.monto_en_pesos),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  a.folio =","'", g_reg.folio,"'",
                                          " AND    a.subcuenta = 1 ",
                                         #" AND    a.tipo_movimiento = 540 ",
                                          " AND a.tipo_movimiento between 540 and 555 ",
                                          " GROUP BY 3,4 "
                           PREPARE con_741 FROM  principal
                           DECLARE rcv_321 CURSOR FOR con_741
                           FOREACH rcv_321 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore
                               LET g_reg.siefore       = vsiefore
                               LET g_reg.importe       = total_pesos
--                               LET g_reg.fecha_valor   = vf_conversion
--                               LET g_reg.fecha_emision = vf_conversion
                               LET g_reg.fecha_valor   = f_conversion
                               LET g_reg.fecha_emision = f_conversion
                               LET g_reg.identificador = 1

                               LET g_reg.transaccion_cod = vtran12

                               LET vfecha_sig = f_conversion
                               LET vfecha_fin = f_conversion
                               LET vdia       = DAY(vfecha_sig)
                               LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                               LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                               LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                               LET cont2      = 1

                                   CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                               LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                               CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                               LET g_reg.fecha_emision = vfecha_sig
                               --LET g_reg.fecha_valor   = vfecha_sig
                               LET g_reg.fecha_valor   = f_conversion       ---23 Ago 2006

                               IF g_reg.importe <> 0 THEN
                                   CALL registra_historico12(g_reg.*) #rh
                               END IF

                           END FOREACH
                END IF
{
                           IF g_reg.importe <> 0 THEN
                                CALL registra_historico(g_reg.*) #rh
                                LET g_reg.importe       = total_acciones
                                LET g_reg.identificador = 2
                                IF  (g_reg.transaccion_cod = 21152 OR
                                     g_reg.transaccion_cod = 21153) THEN
                                ELSE
                                    CALL registra_historico(g_reg.*) #rh
                                END IF
                            END IF
                        END FOREACH
}
------------    ----------------------------------------------------------------------
                IF vtran12   = 21150 AND
                   --vcuenta12 = "723401333" AND
                   vcuenta12 = "723401" AND
                   vtipo12   = "C" THEN

                         LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                          " ROUND(SUM(a.monto_en_pesos),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  a.folio =","'", g_reg.folio,"'",
                                          " AND    a.subcuenta = 1 ",
                                         #" AND    a.tipo_movimiento = 540 ",
                                          " AND a.tipo_movimiento between 540 and 555 ",
                                          " GROUP BY 3,4 "
                           PREPARE con_742 FROM  principal
                           DECLARE rcv_322 CURSOR FOR con_742
                           FOREACH rcv_322 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore
                               LET g_reg.siefore       = vsiefore
                               LET g_reg.importe       = total_pesos
--                               LET g_reg.fecha_valor   = vf_conversion
--                               LET g_reg.fecha_emision = vf_conversion
                               LET g_reg.fecha_valor   = f_conversion
                               LET g_reg.fecha_emision = f_conversion
                               LET g_reg.identificador = 1

                               LET g_reg.transaccion_cod = vtran12

                               LET vfecha_sig = f_conversion
                               LET vfecha_fin = f_conversion
                               LET vdia       = DAY(vfecha_sig)
                               LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                               LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                               LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                               LET cont2      = 1

                                   CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                               LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                               CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                               LET g_reg.fecha_emision = vfecha_sig
                               --LET g_reg.fecha_valor   = vfecha_sig
                               LET g_reg.fecha_valor  = f_conversion       ---23 Ago 2006

                               IF g_reg.importe <> 0 THEN
                                   CALL registra_historico12(g_reg.*) #rh
                               END IF

                           END FOREACH
                END IF

------------    -----------------------------------------------------------------------
                IF vtran12   = 21150 AND
                   --vcuenta12 = "B00902" AND
                   vcuenta12 = "110201" AND
                   vtipo12   = "A" THEN

                         LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                          " ROUND(SUM(a.monto_en_pesos),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  a.folio =","'", g_reg.folio,"'",
                                          " AND    a.subcuenta = 1 ",
                                         #" AND    a.tipo_movimiento = 540 ",
                                          " AND a.tipo_movimiento between 540 and 555 ",
                                          " GROUP BY 3,4 "
                           PREPARE con_743 FROM  principal
                           DECLARE rcv_323 CURSOR FOR con_743
                           FOREACH rcv_323 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore
                               LET g_reg.siefore       = vsiefore
                               LET g_reg.importe       = total_pesos
--                               LET g_reg.fecha_valor   = vf_conversion
--                               LET g_reg.fecha_emision = vf_conversion
                               LET g_reg.fecha_valor   = f_conversion
                               LET g_reg.fecha_emision = f_conversion
                               LET g_reg.identificador = 1

                               LET g_reg.transaccion_cod = vtran12

                               LET vfecha_sig = f_conversion
                               LET vfecha_fin = f_conversion
                               LET vdia       = DAY(vfecha_sig)
                               LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                               LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                               LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                               LET cont2      = 1

                                   CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                               LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                               CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                               LET g_reg.fecha_emision = vfecha_sig
                               --LET g_reg.fecha_valor   = vfecha_sig
                               LET g_reg.fecha_valor   = f_conversion       ---23 Ago 2006

                               IF g_reg.importe <> 0 THEN
                                   CALL registra_historico12(g_reg.*) #rh
                               END IF

                           END FOREACH
                END IF

------------    ------------------------------------------------------------------------------
                IF vtran12   = 21150 AND
                   --vcuenta12 = "210101333" AND
                   vcuenta12 = "21010001" AND
                   vtipo12   = "C" THEN

                         LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                          " ROUND(SUM(a.monto_en_pesos),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  a.folio =","'", g_reg.folio,"'",
                                          " AND    a.subcuenta = 1 ",
                                         #" AND    a.tipo_movimiento = 540 ",
                                          " AND a.tipo_movimiento between 540 and 555 ",
                                          " GROUP BY 3,4 "
                           PREPARE con_744 FROM  principal
                           DECLARE rcv_324 CURSOR FOR con_744
                           FOREACH rcv_324 INTO total_acciones,total_pesos,f_conversion,
                                               vsiefore
                               LET g_reg.siefore       = vsiefore
                               LET g_reg.importe       = total_pesos
--                               LET g_reg.fecha_valor   = vf_conversion
--                               LET g_reg.fecha_emision = vf_conversion
                               LET g_reg.fecha_valor   = f_conversion
                               LET g_reg.fecha_emision = f_conversion
                               LET g_reg.identificador = 1

                               LET g_reg.transaccion_cod = vtran12

                               LET vfecha_sig = f_conversion
                               LET vfecha_fin = f_conversion
                               LET vdia       = DAY(vfecha_sig)
                               LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                               LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                               LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                               LET cont2      = 1

                                   CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                               LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                               CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                               LET g_reg.fecha_emision = vfecha_sig
                               --LET g_reg.fecha_valor   = vfecha_sig
                               LET g_reg.fecha_valor   = f_conversion       ---23 Ago 2006

                               IF g_reg.importe <> 0 THEN
                                   CALL registra_historico12(g_reg.*) #rh
                               END IF

                           END FOREACH
                END IF
------------    ------------------------------------------------------------------------

            ELSE

                LET vtransacc12 = "SELECT * ",
                                  "FROM  con_transaccion ",
                                  "WHERE transaccion_cod    = 21151 ",
                                  --"OR transaccion_cod = 21151) ",
                                  "AND   folio               = ", vfolio,
                                  " AND   fecha_emision       = ","'",vfecha_sig,"'",
                                  --" AND   fecha_emision       = ","'",vfecha,"'",
                                  " AND   siefore IN (1,2)"
                PREPARE confirma2 FROM vtransacc12
                DECLARE c_transacc2 CURSOR FOR confirma2
                FOREACH c_transacc2 INTO vtransacc2.*
                END FOREACH
                IF vtransacc2.folio  <> vfolio  THEN  

                     IF vtran12   = 21151 AND
                       --vcuenta12 = "713401333" AND
                       vcuenta12 = "713401" AND
                       vtipo12   = "A" THEN

                          LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                          " ROUND(SUM(a.monto_en_pesos),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  a.folio =","'", g_reg.folio,"'",
                                          " AND    a.subcuenta in(2,6,9) ",
                                          " AND a.tipo_movimiento between 540 and 555 ",
                                         " GROUP BY 3,4 "
                            PREPARE con_755 FROM  principal
                            DECLARE rcv_335 CURSOR FOR con_755
                            FOREACH rcv_335 INTO total_acciones,total_pesos,f_conversion,
                                                vsiefore
                                LET g_reg.siefore       = vsiefore
                                LET g_reg.importe       = total_pesos
--                                LET g_reg.fecha_valor   = vf_conversion
--                                LET g_reg.fecha_emision = vf_conversion
                                LET g_reg.fecha_valor   = f_conversion
                                LET g_reg.fecha_emision = f_conversion
                                LET g_reg.identificador = 1

                                LET g_reg.transaccion_cod = vtran12

                                LET vfecha_sig = f_conversion
                                LET vfecha_fin = f_conversion
                                LET vdia       = DAY(vfecha_sig)
                                LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                                LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                                LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                                LET cont2      = 1

                                    CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                                LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                                CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                                LET g_reg.fecha_emision = vfecha_sig
                                --LET g_reg.fecha_valor   = vfecha_sig
                                LET g_reg.fecha_valor   = f_conversion       ---23 Ago 2006

                                IF g_reg.importe <> 0 THEN
                                    CALL registra_historico12(g_reg.*) #rh
                                END IF

                            END FOREACH
                    END IF

{                                IF g_reg.importe <> 0 THEN
                                    CALL registra_historico(g_reg.*) #rh
                                    LET g_reg.importe       = total_acciones
                                    LET g_reg.identificador = 2
                                    IF  (g_reg.transaccion_cod = 21152 OR
                                         g_reg.transaccion_cod = 21153) THEN
                                    ELSE
                                        CALL registra_historico(g_reg.*) #rh
                                    END IF
                                END IF
                            END FOREACH
}
------------        -----------------------------------------------------------------------
                    IF vtran12   = 21151 AND
                       --vcuenta12 = "723401333" AND
                       vcuenta12 = "723401" AND
                       vtipo12   = "C" THEN

                          LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                          " ROUND(SUM(a.monto_en_pesos),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  a.folio =","'", g_reg.folio,"'",
                                          " AND    a.subcuenta in(2,6,9) ",
                                          " AND a.tipo_movimiento between 540 and 555 ",
                                         " GROUP BY 3,4 "
                            PREPARE con_756 FROM  principal
                            DECLARE rcv_336 CURSOR FOR con_756
                            FOREACH rcv_336 INTO total_acciones,total_pesos,f_conversion,
                                                vsiefore
                                LET g_reg.siefore       = vsiefore
                                LET g_reg.importe       = total_pesos
--                                LET g_reg.fecha_valor   = vf_conversion
--                                LET g_reg.fecha_emision = vf_conversion
                                LET g_reg.fecha_valor   = f_conversion
                                LET g_reg.fecha_emision = f_conversion
                                LET g_reg.identificador = 1

                                LET g_reg.transaccion_cod = vtran12

                                LET vfecha_sig = f_conversion
                                LET vfecha_fin = f_conversion
                                LET vdia       = DAY(vfecha_sig)
                                LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                                LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                                LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                                LET cont2      = 1

                                    CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                                LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                                CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                                LET g_reg.fecha_emision = vfecha_sig
                                --LET g_reg.fecha_valor   = vfecha_sig
                                LET g_reg.fecha_valor   = f_conversion       ---23 Ago 2006

                                IF g_reg.importe <> 0 THEN
                                    CALL registra_historico12(g_reg.*) #rh
                                END IF

                            END FOREACH
                    END IF

------------        ------------------------------------------------------------------------------
                    IF vtran12   = 21151 AND
                       --vcuenta12 = "B00902" AND
                       vcuenta12 = "110201" AND
                       vtipo12   = "A" THEN

                          LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                          " ROUND(SUM(a.monto_en_pesos),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  a.folio =","'", g_reg.folio,"'",
                                          " AND    a.subcuenta in(2,6,9) ",
                                          " AND a.tipo_movimiento between 540 and 555 ",
                                         " GROUP BY 3,4 "
                            PREPARE con_757 FROM  principal
                            DECLARE rcv_337 CURSOR FOR con_757
                            FOREACH rcv_337 INTO total_acciones,total_pesos,f_conversion,
                                                vsiefore
                                LET g_reg.siefore       = vsiefore
                                LET g_reg.importe       = total_pesos
--                                LET g_reg.fecha_valor   = vf_conversion
--                                LET g_reg.fecha_emision = vf_conversion
                                LET g_reg.fecha_valor   = f_conversion
                                LET g_reg.fecha_emision = f_conversion
                                LET g_reg.identificador = 1

                                LET g_reg.transaccion_cod = vtran12

                                LET vfecha_sig = f_conversion
                                LET vfecha_fin = f_conversion
                                LET vdia       = DAY(vfecha_sig)
                                LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                                LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                                LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                                LET cont2      = 1

                                    CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                                LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                                CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                                LET g_reg.fecha_emision = vfecha_sig
                                --LET g_reg.fecha_valor   = vfecha_sig
                                LET g_reg.fecha_valor   = f_conversion       ---23 Ago 2006

                                IF g_reg.importe <> 0 THEN
                                    CALL registra_historico12(g_reg.*) #rh
                                END IF

                            END FOREACH
                    END IF

------------        ----------------------------------------------------------------------------------
                    IF vtran12   = 21151 AND
                       --vcuenta12 = "210101333" AND
                       vcuenta12 = "21010001" AND
                       vtipo12   = "C" THEN

                          LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                          " ROUND(SUM(a.monto_en_pesos),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  a.folio =","'", g_reg.folio,"'",
                                          " AND    a.subcuenta in(2,6,9) ",
                                          " AND a.tipo_movimiento between 540 and 555 ",
                                         " GROUP BY 3,4 "
                            PREPARE con_758 FROM  principal
                            DECLARE rcv_338 CURSOR FOR con_758
                            FOREACH rcv_338 INTO total_acciones,total_pesos,f_conversion,
                                                vsiefore
                                LET g_reg.siefore       = vsiefore
                                LET g_reg.importe       = total_pesos
--                                LET g_reg.fecha_valor   = vf_conversion
--                                LET g_reg.fecha_emision = vf_conversion
                                LET g_reg.fecha_valor   = f_conversion
                                LET g_reg.fecha_emision = f_conversion
                                LET g_reg.identificador = 1

                                LET g_reg.transaccion_cod = vtran12

                                LET vfecha_sig = f_conversion
                                LET vfecha_fin = f_conversion
                                LET vdia       = DAY(vfecha_sig)
                                LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                                LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                                LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                                LET cont2      = 1

                                    CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                                LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                                CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                                LET g_reg.fecha_emision = vfecha_sig
                                --LET g_reg.fecha_valor   = vfecha_sig
                                LET g_reg.fecha_valor   = f_conversion       ---23 Ago 2006

                                IF g_reg.importe <> 0 THEN
                                    CALL registra_historico12(g_reg.*) #rh
                                END IF

                            END FOREACH
                     END IF

                ELSE
                EXIT FOREACH
                END IF 
            END IF
         END FOREACH

##############<= erm 24-Oct-2005

            LET  g_reg.transaccion_cod = vtransaccion

            LET  total_acciones = ""
            LET  total_pesos    = ""
            LET  precio         = ""
            LET  f_conversion   = ""
            LET  f_proceso      = ""

            DECLARE c_vf CURSOR FOR
               SELECT fecha_conversion
               --INTO   vf_conversion
               FROM   dis_cuenta
               WHERE  folio = g_reg.folio
               GROUP BY 1
            FOREACH c_vf INTO vf_conversion

               IF subcta = 4 THEN
#vivienda se liquida el 1er día habil del mes con el precio de la accion del 1er día natural
                    LET principal = " SELECT SUM(ROUND(monto_en_pesos,2)),",
                                    " fecha_conversion, siefore ",
                                    --" FROM   dis_provision ",
                                    " FROM   dis_cuenta ",
                                    " WHERE  folio =","'", g_reg.folio,"'",
                                    " AND  (subcuenta in(","'", subcta,"'",")",
                                    " OR   subcuenta in(","'", subcta1,"'","))",
                                    " GROUP BY 2,3 "
                    PREPARE con_73 FROM  principal
                    DECLARE viv_17 CURSOR FOR con_73
                    FOREACH viv_17 INTO total_pesos,f_conversion,vsiefore

                       LET g_reg.importe       = total_pesos
--                       LET g_reg.fecha_emision = vf_conversion
--                       LET g_reg.fecha_valor   = vf_conversion
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 1
                       LET g_reg.siefore       = vsiefore

                    IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                       END IF
                    END FOREACH

---> erm 19 ene 2007 para enviar VIVIENDA al 1er dia habil mes siguiente
{coment erm 21 Mayo 2007                        LET vfecha_sig = f_conversion
                        LET vfecha_fin = f_conversion
                        LET vdia       = DAY(vfecha_sig)
                        LET vfecha_sig = vfecha_sig + 1 UNITS MONTH    ---23 Ago 2006
                        LET vfecha_sig = vfecha_sig - vdia UNITS DAY
                        LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
                        LET cont2      = 1

                            CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig

                        LET vfecha_fin = vfecha_fin - 1 UNITS DAY

                          CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

                        LET g_reg.fecha_emision = vfecha_sig
                        --LET g_reg.fecha_valor   = vfecha_sig
                        LET g_reg.fecha_valor   = f_conversion       ---23 Ago 2006

                        IF g_reg.importe <> 0 THEN
                            CALL registra_historico12(g_reg.*) #rh
                        END IF

                    END FOREACH}       #coment erm 21 Mayo 2007
---<
               ELSE
                  CASE vtransaccion
                      WHEN 21150
                        LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                         " ROUND(SUM(a.monto_en_pesos),2), ",
                                         " a.fecha_conversion, a.siefore ",
                                         " FROM   dis_cuenta a ",
                                         " WHERE  a.folio =","'", g_reg.folio,"'",
                                         " AND    a.subcuenta = 1 ",
                                        #" AND    a.tipo_movimiento = 540 ",
                                         " AND a.tipo_movimiento between 540 and 555 ",
                                         " GROUP BY 3,4 "
                          PREPARE con_74 FROM  principal
                          DECLARE rcv_32 CURSOR FOR con_74
                          FOREACH rcv_32 INTO total_acciones,total_pesos,f_conversion,
                                              vsiefore
                              LET g_reg.siefore       = vsiefore
                              LET g_reg.importe       = total_pesos
--                              LET g_reg.fecha_valor   = vf_conversion
--                              LET g_reg.fecha_emision = vf_conversion
                              LET g_reg.fecha_valor   = f_conversion
                              LET g_reg.fecha_emision = f_conversion
                              LET g_reg.identificador = 1

                              IF g_reg.importe <> 0 THEN
                                  CALL registra_historico(g_reg.*) #rh
                                  LET g_reg.importe       = total_acciones
                                  LET g_reg.identificador = 2
                                  IF  (g_reg.transaccion_cod = 21152 OR
                                       g_reg.transaccion_cod = 21153) THEN
                                  ELSE
                                      CALL registra_historico(g_reg.*) #rh
                                  END IF
                              END IF
                          END FOREACH

                      WHEN 21151
                        LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                        " ROUND(SUM(a.monto_en_pesos),2), ",
                                        " a.fecha_conversion, a.siefore ",
                                        " FROM   dis_cuenta a ",
                                        " WHERE  a.folio =","'", g_reg.folio,"'",
                                        " AND    a.subcuenta in(2,6,9) ",
                                        " AND a.tipo_movimiento between 540 and 555 ",
                                       " GROUP BY 3,4 "
                          PREPARE con_75 FROM  principal
                          DECLARE rcv_33 CURSOR FOR con_75
                          FOREACH rcv_33 INTO total_acciones,total_pesos,f_conversion,
                                              vsiefore
                              LET g_reg.siefore       = vsiefore
                              LET g_reg.importe       = total_pesos
--                              LET g_reg.fecha_valor   = vf_conversion
--                              LET g_reg.fecha_emision = vf_conversion
                              LET g_reg.fecha_valor   = f_conversion
                              LET g_reg.fecha_emision = f_conversion
                              LET g_reg.identificador = 1

                              IF g_reg.importe <> 0 THEN
                                  CALL registra_historico(g_reg.*) #rh
                                  LET g_reg.importe       = total_acciones
                                  LET g_reg.identificador = 2
                                  IF  (g_reg.transaccion_cod = 21152 OR
                                       g_reg.transaccion_cod = 21153) THEN
                                  ELSE
                                      CALL registra_historico(g_reg.*) #rh
                                  END IF
                              END IF
                          END FOREACH

                       WHEN 21152
                          LET  principal = " SELECT SUM(monto_ret+monto_act_ret) ",
                                           " FROM   exc_det_exceso ",
                                           " WHERE  folio =","'", g_reg.folio,"'",
                                           " AND    result_operacion = 01 "
                           PREPARE con_76 FROM  principal
                           DECLARE rcv_34 CURSOR FOR con_76
                           FOREACH rcv_34 INTO total_pesos
--->23 Ago
                           LET total_pesos1 = 0

                           SELECT SUM(ROUND(monto_comi_ret,2))
                           INTO   total_pesos1
                           FROM   exc_exceso_comis
                           WHERE  folio    = g_reg.folio

                           IF total_pesos1 IS NULL THEN
                               LET total_pesos1 = 0
                           END IF
                           LET total_pesos = total_pesos - total_pesos1
                           LET total_pesos = total_pesos * -1
---<23 Ago
                              LET g_reg.siefore       = vsiefore
                              LET g_reg.importe       = total_pesos
                              LET g_reg.fecha_valor   = vf_conversion
                              LET g_reg.fecha_emision = vf_conversion
                              --LET g_reg.fecha_valor   = f_conversion
                              --LET g_reg.fecha_emision = f_conversion
                              LET g_reg.identificador = 1

                              IF g_reg.importe <> 0 THEN
                                  CALL registra_historico(g_reg.*) #rh
                                  LET g_reg.importe       = total_acciones
                                  LET g_reg.identificador = 2
                                  IF  (g_reg.transaccion_cod = 21152 OR
                                       g_reg.transaccion_cod = 21153) THEN
                                  ELSE
                                      CALL registra_historico(g_reg.*) #rh
                                  END IF
                              END IF
                           END FOREACH
{
                       LET total_pesos1 = 0

                       SELECT SUM(ROUND(monto_comi_ret,2))
                       INTO   total_pesos1
                       FROM   exc_exceso_comis
                       WHERE  folio    = g_reg.folio

                       IF total_pesos1 IS NULL THEN
                           LET total_pesos1 = 0
                       END IF
                       LET total_pesos = total_pesos + total_pesos1
                       LET total_pesos = total_pesos * -1
}
                       WHEN 21153
                          LET  principal = " SELECT SUM(monto_ces_vej_pat+monto_act_ces_vej) ",
                                           " FROM   exc_det_exceso ",
                                           " WHERE  folio =","'", g_reg.folio,"'",
                                           " AND    result_operacion = 01 "
                           PREPARE con_77 FROM  principal
                           DECLARE rcv_35 CURSOR FOR con_77
                           FOREACH rcv_35 INTO total_pesos
--->erm 23 Ago 2006
                           LET total_pesos1 = 0

                           SELECT SUM(ROUND(monto_comi_ces_vej,2))
                           INTO   total_pesos1
                           FROM   exc_exceso_comis
                           WHERE  folio    = g_reg.folio

                           IF total_pesos1 IS NULL THEN
                               LET total_pesos1 = 0
                           END IF
                           LET total_pesos = total_pesos - total_pesos1
                           LET total_pesos = total_pesos * -1
--                           END CASE
---<
                              LET g_reg.siefore       = vsiefore
                              LET g_reg.importe       = total_pesos
                              LET g_reg.fecha_valor   = vf_conversion
                              LET g_reg.fecha_emision = vf_conversion
                              --LET g_reg.fecha_valor   = f_conversion
                              --LET g_reg.fecha_emision = f_conversion
                              LET g_reg.identificador = 1

                              IF g_reg.importe <> 0 THEN
                                  CALL registra_historico(g_reg.*) #rh
                                  LET g_reg.importe       = total_acciones
                                  LET g_reg.identificador = 2
                                  IF  (g_reg.transaccion_cod = 21152 OR
                                       g_reg.transaccion_cod = 21153) THEN
                                  ELSE
                                      CALL registra_historico(g_reg.*) #rh
                                  END IF
                              END IF
                           END FOREACH
{
                       LET total_pesos1 = 0

                       SELECT SUM(ROUND(monto_comi_ces_vej,2))
                       INTO   total_pesos1
                       FROM   exc_exceso_comis
                       WHERE  folio    = g_reg.folio

                       IF total_pesos1 IS NULL THEN
                           LET total_pesos1 = 0
                       END IF
                       LET total_pesos = total_pesos + total_pesos1
                       LET total_pesos = total_pesos * -1}
                       END CASE

               END IF
--            END IF
            END FOREACH
         ---END FOREACH
     END FOREACH
END FUNCTION
FUNCTION credito_garantia()
#cg------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE vf_conversion   DATE
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    DEFINE vfecha_sig      DATE
    DEFINE vfecha_fin      DATE
    #DEFINE subcta          CHAR(05)
    DEFINE vsiefore        SMALLINT
    DEFINE vcomision       SMALLINT
    DEFINE vdia            SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE cont2           SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    #DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos1    DECIMAL(22,6)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 20

     DECLARE cg_1 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH cg_1 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_acciones = ""
         LET  total_pesos    = ""
         LET  precio         = ""
         LET  f_conversion   = ""
         LET  f_proceso      = ""

         LET principal = " SELECT SUM(ROUND(monto_en_pesos,2)),",
                         " fecha_conversion, siefore ",
                         " FROM   dis_provision ",
                         " WHERE  folio =","'", g_reg.folio,"'",
                         " AND  subcuenta in(","'", subcta,"'",")",
                         " GROUP BY 2,3 "
         PREPARE cg_2 FROM  principal
         DECLARE cg_viv CURSOR FOR cg_2
         FOREACH cg_viv INTO total_pesos,f_conversion,vsiefore

            LET g_reg.importe       = total_pesos
            --LET g_reg.fecha_emision = f_conversion
            LET g_reg.fecha_emision = HOY              ---28 Agosto 2007
            LET g_reg.fecha_valor   = f_conversion
            LET g_reg.identificador = 1
            LET g_reg.siefore       = vsiefore
---comentado con {} el día 07 Julio 2006
{
            LET vfecha_sig = f_conversion
            LET vfecha_fin = f_conversion
            LET vdia       = DAY(vfecha_sig)
            LET vfecha_sig = vfecha_sig - vdia UNITS DAY
            LET vfecha_fin = vfecha_fin + 1 UNITS MONTH
            LET cont2      = 1
---            FOR cont2      = 1 TO 5          ---comentado erm 21-ocubre-2005
                CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig
---            END FOR                          ---comentado erm 21-ocubre-2005
--->erm mes siguiente
            LET vfecha_sig = vfecha_sig + 1 UNITS MONTH          ---mas 1 mes
            LET vfecha_sig = vfecha_sig - 1 UNITS DAY

                CALL habil_siguiente(vfecha_sig) RETURNING vfecha_sig
---<erm
            LET vfecha_fin = vfecha_fin - 1 UNITS DAY

            CALL habil_anterior(vfecha_fin) RETURNING vfecha_fin

            LET g_reg.fecha_emision = vfecha_sig
            LET g_reg.fecha_valor   = vfecha_sig
}
--->erm 26-Oct-2005
            IF g_reg.fecha_emision IS NULL THEN
               SELECT UNIQUE(fecha_emision)
               INTO   v_femi
               FROM   con_transaccion
               WHERE  folio = g_reg.folio
               AND    estado = 20
                 IF SQLCA.SQLCODE <> NOTFOUND THEN
                    LET g_reg.fecha_emision = v_femi
                 END IF
            END IF
---<erm 26-Oct-2005

            IF g_reg.importe <> 0 THEN
                CALL registra_historico(g_reg.*) #rh
            END IF

         END FOREACH

     END FOREACH
END FUNCTION
FUNCTION aportacion_subsecuente()
#as------------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE vf_conversion   DATE
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    DEFINE vfecha_sig      DATE
    DEFINE vfecha_fin      DATE
    #DEFINE subcta          CHAR(05)
    DEFINE vsiefore        SMALLINT
    DEFINE vcomision       SMALLINT
    DEFINE vdia            SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE cont2           SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE importe         DECIMAL(15,2)
    #DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos1    DECIMAL(22,6)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)
    DEFINE max_fecha       DATE

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 20

    DECLARE as_1 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH as_1 INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_acciones = ""
         LET  total_pesos    = ""
         LET  precio         = ""
         LET  f_conversion   = vfecha
         LET  f_proceso      = ""

         LET principal = " SELECT SUM(ROUND(monto_en_pesos,2)), ",
                         " siefore  ",
                         " FROM   dis_cuenta ",
                         " WHERE  subcuenta =","'", subcta,"'",
                         " AND  tipo_movimiento = 7 ",
                         " AND  fecha_conversion = ","'",vfecha,"'",
			 " GROUP BY 2 "
         PREPARE as_2 FROM  principal
         DECLARE as_viv CURSOR FOR as_2
         FOREACH as_viv INTO total_pesos,vsiefore

            LET g_reg.importe       = total_pesos
            --LET g_reg.fecha_emision = f_conversion
            LET g_reg.fecha_emision = HOY              ---28 Agosto 2007
            LET g_reg.fecha_valor   = f_conversion
            LET g_reg.siefore       = vsiefore
            LET g_reg.identificador = 1
---comentado con {} el día 07 Julio 2006
{
     #erm Envia fecha valor y emision al 1er dia habil
     #erm  21-octubre-2005
            LET max_fecha = f_conversion
            LET vdia      = DAY(max_fecha)
            LET max_fecha = max_fecha - vdia UNITS DAY
               CALL habil_siguiente(max_fecha) RETURNING max_fecha
--->erm mes siguiente
            LET max_fecha = max_fecha + 1 UNITS MONTH          ---mas 1 mes
            LET max_fecha = max_fecha - 1 UNITS DAY
                CALL habil_siguiente(max_fecha) RETURNING max_fecha
---<erm
            LET g_reg.fecha_valor   = max_fecha
            LET g_reg.fecha_emision = max_fecha
}
--->erm 26-Oct-2005
            IF g_reg.fecha_emision IS NULL THEN
               SELECT UNIQUE(fecha_emision)
               INTO   v_femi
               FROM   con_transaccion
               WHERE  folio = g_reg.folio
               AND    estado = 20
                 IF SQLCA.SQLCODE <> NOTFOUND THEN
                    LET g_reg.fecha_emision = v_femi
                 END IF
            END IF
---<erm 26-Oct-2005

            IF g_reg.importe <> 0 THEN
               CALL registra_historico(g_reg.*) #rh
            END IF

         END FOREACH

     END FOREACH

END FUNCTION
FUNCTION registra_historico(g_reg5)
#rp----------------------------
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
	siefore           SMALLINT,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
	usuario           CHAR(08),
        estado            SMALLINT
    END RECORD

    LET   g_reg5.estado  = 10

    IF (g_reg5.proceso_cod = "00016" OR
        g_reg5.proceso_cod = "00021" OR
        g_reg5.proceso_cod = "00022" OR
        g_reg5.proceso_cod = "00026" OR
        g_reg5.proceso_cod = "00027" OR
        g_reg5.proceso_cod = "00029" OR
        g_reg5.proceso_cod = "00030" OR
        g_reg5.proceso_cod = "00033" OR
        g_reg5.proceso_cod = "00041" OR
        g_reg5.proceso_cod = "00054" OR
        g_reg5.proceso_cod = "00055") THEN
        LET   g_reg5.estado  = 20
    END IF
    {
    IF g_reg5.proceso_cod = "00002" THEN
        LET g_reg5.proceso_cod = "00001"
    END IF
    IF g_reg5.proceso_cod = "00004" THEN
        LET g_reg5.proceso_cod = "00003"
    END IF
    }
--->erm se agrega 23 Abril 2007 solo para subcuentas 4 o 8
    IF  g_reg5.proceso_cod = "00063" AND 
       (g_reg5.transaccion_cod = 24351 OR
        g_reg5.transaccion_cod = 29351) THEN
        LET   g_reg5.estado  = 20
    END IF

    IF  g_reg5.proceso_cod = "00023" AND 
        g_reg5.transaccion_cod = 24150 THEN
        LET   g_reg5.estado  = 20
    END IF
---<
    INSERT INTO con_transaccion VALUES(g_reg5.*)

END FUNCTION

FUNCTION registra_historico12(g_reg5)
#rp----------------------------
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        siefore           SMALLINT,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
        usuario           CHAR(08),
        estado            SMALLINT
    END RECORD

    LET   g_reg5.estado  = 20

    INSERT INTO con_transaccion VALUES(g_reg5.*)

END FUNCTION

FUNCTION  despliega_tipo()
    DEFINE pos        INTEGER
    DEFINE cla_where  CHAR(200)
    DEFINE sel_where  CHAR(200)
    DEFINE l_record  ARRAY[1000] OF RECORD
        codigo         CHAR(05),
        descripcion    CHAR(80)
    END RECORD

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,18 WITH FORM "CONC0012" ATTRIBUTE( BORDER)
      DISPLAY "    (Enter) Seleccionar                (Ctrl-C) Salir      " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "        Escoja con < ENTER > seleccionar el tipo           " AT 2,1
      DISPLAY "                                                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON proceso_cod 
                          FROM proceso_cod

         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         # CLEAR SCREEN
         CLOSE WINDOW ventana_2
         INITIALIZE l_record[1].codigo  TO NULL
         INITIALIZE l_record[1].descripcion  TO NULL
         RETURN l_record[pos].codigo, l_record[pos].descripcion
      END IF

      LET sel_where = "SELECT * FROM tab_proceso WHERE ",cla_where CLIPPED,
                      " AND proceso_cod NOT IN (00012,00013,00014,00015,",    ---erm 3 Mzo 2006
                      "00018,00020,00031,00032,00034,00036,00037,00049)",           ---erm 3 Mzo 2006
                      "ORDER BY 1,2 "
  
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1) 
          DISPLAY ARRAY l_record TO scr_1.* 

             ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                LET vproceso     = l_record[pos].codigo
                LET vdescripcion = l_record[pos].descripcion
                EXIT DISPLAY

             ON KEY (CONTROL-C)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

             ON KEY (INTERRUPT)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "EL PROCESO NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          INITIALIZE l_record[1].codigo  TO NULL
          INITIALIZE l_record[1].descripcion  TO NULL
          RETURN l_record[pos].codigo, l_record[pos].descripcion
       END IF

    END IF

    RETURN l_record[pos].codigo, l_record[pos].descripcion
END FUNCTION

REPORT listado_3(reg_3)
#l3--------------------
        DEFINE reg_3 RECORD #glo #reg_3
            fecha_emision     DATE,
            fecha_valor       DATE,
            identificador     SMALLINT,
            transaccion_cod   INTEGER,
            importe           DECIMAL(15,2),
            proceso_cod       CHAR(05)
        END RECORD
   
    OUTPUT
        PAGE LENGTH 1
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"001",                                 #origen
            COLUMN 003,",",                                   #separador
            COLUMN 004,reg_3.fecha_emision USING"MM/DD/YYYY", #fecha_emision
            COLUMN 015,",",                                   #separador
            COLUMN 016,reg_3.fecha_valor   USING"MM/DD/YYYY", #fecha_emision
            COLUMN 026,",",                                   #separador
            COLUMN 027,reg_3.identificador USING"&",          #identificador
            COLUMN 028,",",                                   #separador
            COLUMN 029,reg_3.transaccion_cod USING"&&&&&",    #transaccion
            COLUMN 034,",",                                   #separador
            COLUMN 035,reg_3.importe*100 USING"&&&&&&&&&&&&&&&",  #importe
            COLUMN 050,",",                                   #separador
            COLUMN 051,reg_3.proceso_cod,                     #proceso
            COLUMN 056,",",                                   #separador
            COLUMN 057,"0000000000",
            COLUMN 067,",",                                   #separador
            COLUMN 068,"0",
            COLUMN 069,",",                                   #separador
            COLUMN 070,"0",
            COLUMN 071,",",                                   #separador
            COLUMN 072,"0"
END REPORT

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------
   DEFINE diaTmp	DATE,
   	  contador	SMALLINT,
	  diaActual	DATE
   
   DEFINE diaHabilSig	DATE,
	  diaSemana	SMALLINT,
	  feriado	SMALLINT,
	  finSemana	SMALLINT
	  

   LET diaHabilSig = diaActual + 1 UNITS DAY

	WHILE TRUE
   	    LET feriado   = 0
   	    LET finSemana = 0
   	    LET diaSemana = WEEKDAY(diaHabilSig)  

   	    IF diaSemana  = 0 OR diaSemana = 6 THEN
      	        LET finSemana = 1
   	    END IF
  	     
   	    SELECT *
   	    FROM   tab_feriado 
   	    WHERE  feria_fecha = diaHabilSig
    	
   	    IF STATUS <> NOTFOUND THEN
       	        LET feriado = 1
   	    END IF 
		
   	    IF feriado = 1 OR finSemana = 1 THEN
       	        LET diaHabilSig = diaHabilSig + 1 UNITS DAY
   	    ELSE
       	        EXIT WHILE
   	    END IF
	END WHILE

	RETURN diaHabilSig

END FUNCTION #habil_siguiente

FUNCTION habil_anterior(diaActual)
#ha--------------------------------

   DEFINE diaTmp	      DATE,
   	  contador	      SMALLINT,
          diaActual	      DATE,
          diaHabilAnt	      DATE,
          diaSemana	      SMALLINT,
	  feriado	      SMALLINT,
	  finSemana	      SMALLINT

   LET diaHabilAnt = diaActual

	WHILE TRUE
   	    LET feriado   = 0
   	    LET finSemana = 0
   	    LET diaSemana = WEEKDAY(diaHabilAnt)  

   	    IF diaSemana = 0 OR diaSemana = 6 THEN
      	        LET finSemana = 1
   	    END IF
  	     
   	    SELECT *
   	    FROM   tab_feriado 
   	    WHERE  feria_fecha = diaHabilAnt
    	
   	    IF STATUS <> NOTFOUND THEN
       	        LET feriado = 1
   	    END IF 
		
   	    IF feriado = 1 OR finSemana = 1 THEN
       	        LET diaHabilAnt = diaHabilAnt - 1 UNITS DAY
   	    ELSE
       	        EXIT WHILE
   	    END IF
	END WHILE

	RETURN diaHabilAnt
END FUNCTION #habil_anterior
---->svp FUNCTION

FUNCTION concilia()
DEFINE
   reg RECORD
    vproceso CHAR(5),
    vfolio   INTEGER,
    vfecha   DATE
  END RECORD,
  salida_var SMALLINT, #variable utilizada para controlar la salida del INPUT 
                       # ARRAY, ya que el INT_FLAG se manipula para mantenerlo
                       #dentro del ciclo WHILE 
paso decimal(7,2),
  reg_arr ARRAY[99] OF RECORD
      siefore          SMALLINT,
      accion_safre     DECIMAL(15,2),
      accion_tesoreria DECIMAL(15,2),
      precio_safre     DECIMAL(15,2),
      precio_tesoreria DECIMAL(15,2),
      dif_concilia     DECIMAL(10,2),
      identificador    SMALLINT,
      entero           INTEGER,
      dif_fraccion   DECIMAL(7,2)
  END RECORD,

 cadena1 CHAR(1000), #Cadenas de caracteres utilizadas para generar un SELECT
                     # dinamico, en base a condiciones dadas.
 cadena2,cadena3 CHAR(250),

 aux_proc     CHAR(5), # Variable que almacena un tipo de proceso -00009 o
                       # 00010- para con un solo SELECT tener el control de 
                       # la transferencia
 resp         CHAR(1),
 aux_edo      SMALLINT,
 a,i          SMALLINT, # Indices utilizados para la manipulacion del arreglo

 precio_dia   LIKE glo_valor_accion.precio_del_dia,

 suma_pesos,
 suma_accion DECIMAL(15,2), # Almacenan el acumulado del importe para acciones
                            # y pesos de con_transaccion
 movimiento   SMALLINT,

 indx_arr,
 tot_arr,
 indx_scr     SMALLINT,
 vdescripcion CHAR(80),
 mensaje      CHAR(200) # Almacena un mensaje para ser desplegado en varios
                        # puntos de la funcion

#Despliegue de la ventana con los datos para hacer la consulta: proceso,folio
# y fecha
OPEN WINDOW wnd_conc AT 5,2 WITH FORM "CONC001B" 
ATTRIBUTE (BORDER,FORM LINE FIRST)

WHILE TRUE # Se mantiene dentro de un ciclo a menos que el usuario aborte la
           # Operacion

      DISPLAY "[ Esc ]Iniciar                                               [ Ctrl-C ]Salir" AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "CONC001         CONCILIACION ENTRE TESORERIA Y SAFRE                           " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

      LET INT_FLAG = 0
      INITIALIZE reg.*,vdescripcion TO NULL
      DISPLAY BY NAME vdescripcion

      FOR a= 1 TO 99
          LET reg_arr[a].siefore = 0
          LET reg_arr[a].identificador = 0
          LET reg_arr[a].accion_safre = 0
          LET reg_arr[a].precio_safre = 0
          LET reg_arr[a].accion_tesoreria = 0
          LET reg_arr[a].precio_tesoreria = 0
          LET reg_arr[a].dif_concilia = 0
          LET reg_arr[a].entero= 0
          LET reg_arr[a].dif_fraccion= 0
      END FOR    

    #Captura de los datos para realizar la busqueda  
    INPUT BY NAME reg.* WITHOUT DEFAULTS
      
      AFTER FIELD vproceso
         IF reg.vproceso IS NULL THEN
             CALL despliega_tipo()
             RETURNING reg.vproceso,vdescripcion
             DISPLAY BY NAME reg.vproceso,vdescripcion
         ELSE
            SELECT descripcion
            INTO vdescripcion
            FROM   tab_proceso
            WHERE  proceso_cod = reg.vproceso
            
            IF STATUS <> NOTFOUND THEN
                DISPLAY BY NAME reg.vproceso,vdescripcion
            ELSE
                ERROR "No existe este tipo de proceso" 
                SLEEP 3
                ERROR ""
                NEXT FIELD vproceso
            END IF
          END IF

      BEFORE FIELD vfolio
            # Para estos procesos (00001,00002,00003,00004) se captura folio
            # para cualquier otro no se captura este dato
            IF reg.vproceso >= "00001"  AND reg.vproceso <= "00004"  THEN
               IF reg.vfolio IS NULL THEN
                  SELECT MAX(folio)  # Se recupera el valor Maximo pero 
                                     # se da oportunidad de cambiar el numero 
                                     # de Folio
	          INTO   reg.vfolio
	          FROM   con_transaccion
	          WHERE  proceso_cod = reg.vproceso
	          AND    estado = 10

	          IF STATUS = NOTFOUND THEN
                    LET reg.vfolio = 0
	          END IF
                  DISPLAY BY NAME reg.vfolio
                  NEXT FIELD vfolio
              END IF
            ELSE
               NEXT FIELD vfecha
            END IF

      AFTER  FIELD vfolio
                IF (reg.vfolio IS NULL OR reg.vfolio = 0 ) THEN
                    ERROR "El folio no puede ser nulo o cero"
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD vfolio
                ELSE
                    SELECT "X"
                    FROM   con_transaccion
                    WHERE  proceso_cod   = reg.vproceso
                    AND    folio         = reg.vfolio
                    AND    estado        = 10
                    GROUP BY 1
           
                    IF STATUS = NOTFOUND THEN
		        ERROR "El folio no existe o ya esta conciliado"
		        SLEEP 2
		        ERROR ""
		        NEXT FIELD vfolio
                    END IF
                 END IF
        
      AFTER  FIELD vfecha
         IF reg.vfecha IS NULL THEN
             ERROR "La fecha no puede ser nula"
             SLEEP 2
             ERROR ""
             NEXT FIELD vfecha
         ELSE
             SELECT "X"
             FROM   con_transaccion
             WHERE  proceso_cod = reg.vproceso
             AND    fecha_emision = reg.vfecha
             AND    estado = 10
             GROUP BY 1
             
             IF STATUS = NOTFOUND THEN
                 ERROR "No existe proceso por conciliar" 
                 SLEEP 3
                 NEXT FIELD vfecha
             END IF
         END IF
    
   AFTER INPUT
       IF INT_FLAG THEN EXIT INPUT END IF 
   END INPUT

   IF INT_FLAG THEN 
      ERROR "CONCILIACION CANCELADA"	
      SLEEP 2	
      ERROR ""
      EXIT WHILE 
   END IF

   # Se selecciona el tipo de movimientos par determinar si es un egreso(-1) 
   # o un ingreso(1)
   SELECT @movimiento INTO movimiento
   FROM tab_proceso 
   WHERE proceso_cod=reg.vproceso

   LET i=1
   LET suma_pesos=0
   LET suma_accion  =0

   DECLARE r_cursor_1 CURSOR FOR
   SELECT codigo_siefore FROM tab_siefore_local 
   WHERE codigo_siefore <> 0 AND  # Recupera todas las siefores excepto el de 
                                  # COMISION Y VIVIENDA
         codigo_siefore <> 11
   ORDER BY 1

   #SE recorre para cada siefore encontrada
   FOREACH r_cursor_1 INTO reg_arr[i].siefore
		IF INT_FLAG THEN EXIT FOREACH END IF
	
    LET a=0
    FOR a= 1 TO 2
       #CAdena general para calculo del importe		
        LET cadena1 ="SELECT SUM(importe)FROM con_transaccion ",
                    " WHERE proceso_cod =", reg.vproceso,
                    " AND   fecha_emision ='",reg.vfecha,"'",
                    " AND   identificador=", a,
                    " AND   siefore=",reg_arr[i].siefore,
                    " AND transaccion_cod NOT IN (SELECT transaccion_cod FROM tab_transaccion ",
                    "WHERE (descripcion_1 MATCHES \"COMIS*\" OR ",
                    "       descripcion_1 MATCHES \"*VIV*\"))"

       # Manejo de las excepciones para el calculo de importe para procesos 
       # con folio
        IF reg.vproceso >="00001" AND reg.vproceso <= "00004" THEN
           LET cadena2=" AND folio=", reg.vfolio
        ELSE
            LET cadena2=""
        END IF

        # Y excepcion segun si es un egreso o ingreso para codigos de 
        # transaccion especificos
        IF movimiento = -1 THEN
           CASE reg.vproceso
             WHEN "00012"
                 LET cadena3=" AND transaccion_cod NOT IN (91001,91002,91003) "
             WHEN "00023"
                 LET cadena3=" AND transaccion_cod NOT IN (21200,21152,21153) "
             WHEN "00011"
                 LET cadena3=" AND transaccion_cod NOT IN (21510) "
             OTHERWISE
                 LET cadena3=""
           END CASE
        ELSE
           CASE reg.vproceso	
             WHEN "00047"
                LET cadena3=" AND transaccion_cod NOT IN (22615) "
              OTHERWISE
                LET cadena3=""
          END CASE
        END IF

        # SE junntan las 3 cadenas en una para PREPARARLA y mandarla a 
        # ejecutarse.
        LET cadena1=cadena1 CLIPPED,cadena2 CLIPPED,cadena3 CLIPPED

        PREPARE s1 FROM cadena1
        DECLARE r_cursor_2 CURSOR FOR s1

        OPEN r_cursor_2
 
        CASE a
           WHEN 1
              FETCH r_cursor_2 INTO reg_arr[i].precio_safre
              IF reg_arr[i].precio_safre IS NULL THEN
                 LET reg_arr[i].precio_safre= 0
              END IF
            WHEN 2
              FETCH r_cursor_2 INTO reg_arr[i].accion_safre
              IF reg_arr[i].accion_safre IS NULL THEN
                 LET reg_arr[i].accion_safre= 0
              END IF
         END CASE   
        CLOSE r_cursor_2

      # Se realiza en acumuladopara pesos y acciones
      IF movimiento =-1 THEN
            LET suma_pesos=suma_pesos + reg_arr[i].precio_safre
      ELSE
            LET suma_accion=suma_accion + reg_arr[i].accion_safre
      END IF

      LET reg_arr[i].identificador = a # Se guarda el tipo de identificador
                                       # 1 para pesos y 2 para acciones. 

      END FOR
      LET i=i+1 # Se incrementa el indice para los datos del ARRAY
   END FOREACH

   IF INT_FLAG THEN EXIT WHILE END IF
   
   IF ((suma_pesos =0 OR suma_pesos IS NULL)
   AND (suma_accion=0 OR suma_accion IS NULL)) THEN
      ERROR "No hay movimiento de pesos en el proceso"
      SLEEP 2
      ERROR ""
      CONTINUE WHILE
   END IF

   #Encabezado para indicar en el ARRAY q desplazarse es atraves de las flechas
      DISPLAY "[ Esc ]Iniciar  [Flecha Arriba/Flecha Abajo para Avanzar]    [ Ctrl-C ]Salir" AT 1,1 ATTRIBUTE(REVERSE)
    ----DISPLAY "[ Esc ] Iniciar     [Flecha Arriba/Flecha Abajo para avanzar]  [ Ctrl-C ] Salir" AT 1,1 ATTRIBUTE(REVERSE)

    #VEntana para la captura de los precios y las acciones
    OPEN WINDOW wnd_arr AT 13,2 WITH FORM "CONC001C" 
    ATTRIBUTE(FORM LINE FIRST)

    #Captura de los datos de tesoreria. Se utiliza otro WHILE para asegurar 
    # que solo se capturen los datos con valor y que al llegar al ultimo
    # renglon vuelva a posicionarse en el primero.
 WHILE TRUE
   LET INT_FLAG=0
   LET salida_var=0

   CALL SET_COUNT(i-1)
   INPUT ARRAY reg_arr WITHOUT DEFAULTS FROM scr_arr.*
   ---ATTRIBUTE (CURRENT ROW DISPLAY= "REVERSE")
   
    BEFORE ROW
       LET indx_arr = ARR_CURR()
       LET indx_scr = SCR_LINE()
       LET tot_arr = ARR_COUNT()
   
       DISPLAY BY NAME indx_arr,tot_arr

       IF indx_arr > tot_arr THEN
          EXIT INPUT
       END IF
		
    BEFORE FIELD precio_tesoreria
       #EN Ingreso solo se capturan acciones
       IF movimiento= 1 THEN
          NEXT FIELD accion_tesoreria
      END IF

    BEFORE FIELD accion_tesoreria
       # EN egresos solo se capturan valores a pesos
       IF movimiento= -1 THEN
         NEXT FIELD precio_tesoreria  
       END IF

  AFTER FIELD precio_tesoreria
    #Para asegurar que solo capturen valores en pesos cuando el importe 
    # por el sistema SAFRE es difernte de cero y no es NULO
    IF reg_arr[indx_arr].precio_safre IS NOT NULL AND 
       reg_arr[indx_arr].precio_safre <> 0 THEN
      IF reg_arr[indx_arr].precio_tesoreria=0 OR 
      reg_arr[indx_arr].precio_tesoreria IS NULL THEN
         ERROR "EL valor no puede ser cero o estar vacío"
         SLEEP 2
         ERROR ""
         NEXT FIELD precio_tesoreria
      ELSE
         # Regresa el valor de la accion para el dia
         CALL get_precio_dia(reg.vfecha,reg_arr[indx_arr].siefore)
         RETURNING precio_dia
         IF INT_FLAG THEN
            ERROR "No existe un valor  de tasa de precio para la fecha dada"
            SLEEP 3
            ERROR ""
            LET salida_var=1
            EXIT INPUT
         END IF
      END IF
      
      #Calculo del valor por calculo para los egresos de accion_tesoreria
      LET reg_arr[indx_arr].accion_tesoreria = (precio_dia /reg_arr[indx_arr].precio_tesoreria)
      
      #Se igualan los signos de precio y accion para tesoreria
      IF (reg_arr[indx_arr].precio_safre <0 
      AND reg_arr[indx_arr].precio_tesoreria >0) THEN
         LET reg_arr[indx_arr].precio_tesoreria= 
         reg_arr[indx_arr].precio_tesoreria * -1
      END IF
      
      IF (reg_arr[indx_arr].precio_safre >0 
      AND reg_arr[indx_arr].precio_tesoreria< 0) THEN
         LET reg_arr[indx_arr].precio_tesoreria= 
         reg_arr[indx_arr].precio_tesoreria * -1
      END IF

      DISPLAY reg_arr[indx_arr].precio_tesoreria TO 
      scr_arr[indx_scr].precio_tesoreria

      DISPLAY reg_arr[indx_arr].accion_tesoreria TO 
      scr_arr[indx_scr].accion_tesoreria

      LET reg_arr[indx_arr].dif_concilia= reg_arr[indx_arr].precio_tesoreria - 
                                          reg_arr[indx_arr].precio_safre
      DISPLAY reg_arr[indx_arr].dif_concilia TO scr_arr[indx_scr].dif_concilia

      #Verifica que no exceda los 3 puntos
      IF (reg_arr[indx_arr].dif_concilia >3 
      OR reg_arr[indx_arr].dif_concilia < -3) THEN	
          ERROR "Lo diferencia no puede ser mayor a 3 ni menor a -3"
          SLEEP 2
          ERROR ""
          NEXT FIELD precio_tesoreria
      END IF
     ELSE
       IF reg_arr[indx_arr].precio_tesoreria <> 0 THEN
          LET reg_arr[indx_arr].precio_tesoreria =0
          DISPLAY reg_arr[indx_arr].precio_tesoreria TO 
          scr_arr[indx_scr].precio_tesoreria
          ERROR "No se puede capturar pues no hubo movimientos"
          SLEEP 2
          ERROR ""
        END IF
     END IF

    AFTER FIELD accion_tesoreria
    #Para asegurar que solo capturen valores en acciones cuando el importe 
    # por el sistema SAFRE es difernte de cero y no es NULO
    IF reg_arr[indx_arr].accion_safre IS NOT NULL AND 
       reg_arr[indx_arr].accion_safre <> 0 THEN
      IF reg_arr[indx_arr].accion_tesoreria=0 
      OR reg_arr[indx_arr].accion_tesoreria IS NULL THEN
         ERROR "EL valor no puede ser cero o estar vacío"
         SLEEP 2
         ERROR ""
         NEXT FIELD accion_tesoreria
      ELSE
         # Regresa el valor de la accion para el dia
         CALL get_precio_dia(reg.vfecha,reg_arr[indx_arr].siefore)
         RETURNING precio_dia
         IF INT_FLAG THEN
            ERROR "No existe un valor  de tasa de precio para la fecha dada"
            SLEEP 3
            ERROR ""
            LET salida_var=1
            EXIT INPUT
         END IF
      END IF
      
      #Calculo del valor por calculo para los ingresos de precio_tesoreria
      LET reg_arr[indx_arr].precio_tesoreria = (precio_dia *       ---(TENIA /)
                                             reg_arr[indx_arr].accion_tesoreria)
      
      #Se igualan los signos de precio y accion para tesoreria
      IF (reg_arr[indx_arr].precio_safre <0 
      AND reg_arr[indx_arr].precio_tesoreria >0) THEN
         LET reg_arr[indx_arr].precio_tesoreria= 
         reg_arr[indx_arr].precio_tesoreria * -1
      END IF
      
      IF (reg_arr[indx_arr].precio_safre >0 
      AND reg_arr[indx_arr].precio_tesoreria< 0) THEN
         LET reg_arr[indx_arr].precio_tesoreria= 
         reg_arr[indx_arr].precio_tesoreria * -1
      END IF

      DISPLAY reg_arr[indx_arr].precio_tesoreria 
      TO scr_arr[indx_scr].precio_tesoreria

      DISPLAY reg_arr[indx_arr].accion_tesoreria 
      TO scr_arr[indx_scr].accion_tesoreria

      LET reg_arr[indx_arr].dif_concilia= reg_arr[indx_arr].accion_tesoreria - 
                                          reg_arr[indx_arr].accion_safre

      DISPLAY reg_arr[indx_arr].dif_concilia 
      TO scr_arr[indx_scr].dif_concilia

      #Verifica que no exceda los 3 puntos
      IF (reg_arr[indx_arr].dif_concilia >3 
      OR reg_arr[indx_arr].dif_concilia < -3) THEN	
          ERROR "Lo diferencia no puede ser mayor a 3 ni menor a -3"
          SLEEP 2
          ERROR ""
          NEXT FIELD accion_tesoreria
      END IF
     ELSE
       IF reg_arr[indx_arr].accion_tesoreria <> 0 THEN
          LET reg_arr[indx_arr].accion_tesoreria =0
          DISPLAY reg_arr[indx_arr].accion_tesoreria TO 
          scr_arr[indx_scr].accion_tesoreria
          ERROR "No se puede capturar pues no hubo movimientos"
          SLEEP 2
          ERROR ""
      END IF
    END IF
   
   ON KEY(ACCEPT)
    # SE barre todo el registro para verificar que no haya algun valor
    # capturado igula a cero cuando existe un importe obtenido por 
    # acumulacion de los valores en SAFRE
    FOR a = 1 TO (i-1)
      IF movimiento = -1 THEN 
       IF reg_arr[a].precio_safre IS NOT NULL AND 
          reg_arr[a].precio_safre <> 0 THEN
         IF reg_arr[a].precio_tesoreria=0 OR 
         reg_arr[a].precio_tesoreria IS NULL THEN
            ERROR "Existe un valor cero o nulo con importe calculado"
            SLEEP 2
            ERROR ""
            CONTINUE INPUT
         END IF
       END IF
     ELSE
       IF reg_arr[a].accion_safre IS NOT NULL AND 
          reg_arr[a].accion_safre <> 0 THEN
         IF reg_arr[a].accion_tesoreria=0 
         OR reg_arr[a].accion_tesoreria IS NULL THEN
            ERROR "Existe un valor cero o nulo con importe calculadoo"
            SLEEP 2
            ERROR ""
            CONTINUE INPUT
         END IF
      END IF
     END IF
    END FOR

   LET INT_FLAG=1
   LET salida_var =0
   EXIT INPUT

  ON KEY(INTERRUPT)
   LET INT_FLAG=1
   LET salida_var =1
   EXIT INPUT

  AFTER INPUT
    IF INT_FLAG THEN EXIT INPUT END IF
  END INPUT 

  IF INT_FLAG THEN EXIT WHILE END IF
 END WHILE

IF salida_var=0 THEN # Si se presiono ESC se hace el valor de INT_FLAG=0
                     # para que no abandone el sistema
   LET INT_FLAG=0
END IF

 IF INT_FLAG THEN 
    ERROR "CONCILIACION CANCELADA"	
    SLEEP 2	
    ERROR ""  
    CLOSE WINDOW wnd_arr
    EXIT WHILE
 END IF

#SE mantiene en el  ciclo del input si el valor excede los 3 puntos o es 0 
# o nulo, en caso contraro si se acepta la Conciliacion se ejecuta 
# ingresa_ajuste

  WHILE TRUE
    PROMPT "Desea Conciliar el Registro S/N ? " FOR CHAR resp
    IF UPSHIFT (resp) MATCHES "[SN]" THEN	
       EXIT WHILE
    END IF
  END WHILE

 IF UPSHIFT(resp)="S" THEN
    FOR a= 1 TO (i-1)
      IF reg_arr[a].dif_concilia <> 0 THEN
         CALL ingresa_ajuste(reg.vfecha,reg.vproceso,
          reg_arr[a].dif_concilia,reg_arr[a].identificador,reg_arr[a].siefore)

      --ELSE  
      END IF
         IF movimiento=1 THEN
            IF reg_arr[a].accion_safre IS NULL OR
            reg_arr[a].accion_safre= 0 THEN
                CONTINUE FOR
            END IF
         ELSE
            IF reg_arr[a].precio_safre IS NULL OR
            reg_arr[a].precio_safre= 0 THEN
                CONTINUE FOR
            END IF
         END IF

         IF (reg.vproceso ="00039" OR reg.vproceso="00046") THEN
           IF movimiento =1 THEN 
             LET reg_arr[a].entero= reg_arr[a].accion_safre - (1 * movimiento)
           END IF
         ELSE
           LET reg_arr[a].entero= reg_arr[a].accion_safre + (1 * movimiento)
         END IF
 
         # Para egreso se usa la accion_safre y para ingreso la 
         # accion de tesoreria
         IF movimiento =1 THEN
             LET reg_arr[a].dif_fraccion = reg_arr[a].entero - 
                                           reg_arr[a].accion_tesoreria
         ELSE
            LET reg_arr[a].dif_fraccion = (reg_arr[a].entero - 
                                          reg_arr[a].accion_safre)
         END IF
      
         IF reg_arr[a].dif_fraccion <>  0 THEN
            CALL ingresa_fraccion (reg.vfecha,reg.vproceso, 
            reg_arr[a].dif_fraccion,reg_arr[a].identificador,reg_arr[a].siefore)
         END IF
      --END IF
         
     IF reg.vproceso >= "00001" AND reg.vproceso <= "00004" THEN
     # Solo para estos procesos existe folio en caso contrario solo se
     # toma el proceso y la fecha de emision
           UPDATE con_transaccion
           SET estado=20
           WHERE proceso_cod =reg.vproceso
           AND   fecha_emision = reg.vfecha
           AND   folio         = reg.vfolio
    ELSE	
          UPDATE con_transaccion
          SET estado=20
          WHERE proceso_cod =reg.vproceso
          AND   fecha_emision = reg.vfecha

     END IF
  END FOR

 #Para los procesos 000009 y el 00010 se verifica la exitencia del otro
 # lo cual indicaria la prescencia de un traspaso
 IF reg.vproceso = "00009" OR reg.vproceso="00010" THEN
    CASE reg.vproceso
      WHEN "00009"
           LET aux_proc= "00010"
           LET mensaje="Falta conciliar el Traspaso A-A como Cedente"
      WHEN "00010"
           LET aux_proc= "00009"
           LET mensaje="Falta conciliar el Folio Traspaso A-A como Receptora"
    END CASE

    LET aux_edo=0
    LET cadena1=" SELECT estado FROM con_transaccion ",
                " WHERE proceso_cod = '", aux_proc CLIPPED,"' ",
                " AND fecha_emision ='",reg.vfecha, "' "

    PREPARE s3 FROM cadena1
    DECLARE r_cursor_3 CURSOR FOR s3
    OPEN r_cursor_3
         FETCH r_cursor_3 INTO aux_edo
    CLOSE r_cursor_3

    CASE aux_edo 
         WHEN 10 
              ERROR mensaje
              SLEEP 2
              ERROR ""
         WHEN 20 
              ERROR "REALIZANDO NETEO"
              SLEEP 2
              ERROR ""
              CALL registra_neteo(reg.vfecha, "00028")
     END CASE
 END IF
  ERROR "CONCILIACION EFECTUADA"	
  SLEEP 2	
  ERROR ""
 ELSE
    ERROR "CONCILIACION CANCELADA"	
    SLEEP 2	
    ERROR ""
    CLOSE WINDOW wnd_arr
    EXIT WHILE
 END IF

 CLOSE WINDOW wnd_arr
END WHILE

CLOSE WINDOW wnd_conc
END FUNCTION

#*************************************************************
FUNCTION get_precio_dia(fecha,siefore)
 DEFINE precio_dia   LIKE glo_valor_accion.precio_del_dia,
        fecha        DATE,
        siefore      SMALLINT

  LET precio_dia = 0
  LET INT_FLAG=0

  SELECT precio_del_dia INTO precio_dia
  FROM glo_valor_accion
  WHERE fecha_valuacion = fecha
  AND   codigo_siefore  = siefore
 
  IF STATUS = NOTFOUND THEN
     LET INT_FLAG =1
  END IF

  RETURN precio_dia
END FUNCTION
----<
FUNCTION ingresa_ajuste(reg_a)
#ia---------------------------
    DEFINE reg_a   RECORD 
        fecha             DATE,
        proceso           CHAR(05),
        diferencia        DECIMAL(15,2),
        tipo              SMALLINT,
        siefore           SMALLINT
    END RECORD
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        siefore           SMALLINT,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
	usuario           CHAR(08),
        estado            SMALLINT
    END RECORD
    DEFINE  vprecio       DECIMAL(10,6)
    DEFINE  vtransaccion  INTEGER
    DEFINE  xfolio        INTEGER

        SELECT MIN(transaccion_cod)
        INTO   vtransaccion
        FROM   tab_transaccion
        WHERE  proceso_cod = reg_a.proceso

        SELECT MAX(folio)
        INTO   xfolio
        FROM   con_transaccion
        WHERE  proceso_cod     = reg_a.proceso
        AND    transaccion_cod = vtransaccion
        AND    fecha_emision   = reg_a.fecha
        AND    siefore         = reg_a.siefore

        IF vproceso >= "00001"  AND
           vproceso <= "00004"  THEN
	   LET xfolio = vfolio
        END IF

        SELECT *
        INTO   g_reg5.*
        FROM   con_transaccion
        WHERE  fecha_emision   = reg_a.fecha
        AND    identificador   = reg_a.tipo
        AND    transaccion_cod = vtransaccion
        AND    proceso_cod     = reg_a.proceso
        AND    folio           = xfolio
        AND    siefore         = reg_a.siefore

        LET g_reg5.importe         = reg_a.diferencia
	LET g_reg5.estado          = 20
	LET g_reg5.usuario         = vusuario
	LET g_reg5.fecha_actualiza = HOY

--->erm 15 Junio 2006
{    IF reg_a.proceso = '00010' THEN
       IF g_reg5.importe < 0 THEN
          LET g_reg5.importe = g_reg5.importe * -1
       END IF
    END IF}
---<

    INSERT INTO con_transaccion VALUES(g_reg5.*)

END FUNCTION
FUNCTION ingresa_fraccion(reg_a)
#ia---------------------------
    DEFINE reg_a   RECORD 
        fecha             DATE,
        proceso           CHAR(05),
        diferencia        DECIMAL(15,2),
        tipo              SMALLINT,
        siefore           SMALLINT
    END RECORD
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        siefore           SMALLINT,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
	usuario           CHAR(08),
        estado            SMALLINT
    END RECORD
    DEFINE  vprecio       DECIMAL(10,6)
    DEFINE  vtransaccion  INTEGER

    SELECT transaccion_cod
    INTO   g_reg5.transaccion_cod
    FROM   tab_transaccion
    WHERE  proceso_cod = reg_a.proceso  
    AND    descripcion_1 MATCHES "*FRACCION"

    SELECT MAX(folio)
    INTO   g_reg5.folio
    FROM   con_transaccion
    WHERE  proceso_cod   = reg_a.proceso  
    AND    fecha_emision = reg_a.fecha

#arc
    IF vproceso >= "00001"  AND
       vproceso <= "00004"  THEN
       LET g_reg5.folio = vfolio
    END IF

    SELECT precio_del_dia
    INTO   vprecio
    FROM   glo_valor_accion
    WHERE  fecha_valuacion = reg_a.fecha
    AND    codigo_siefore  = reg_a.siefore

    LET g_reg5.siefore         = reg_a.siefore
    LET g_reg5.importe         = reg_a.diferencia
    LET g_reg5.fecha_emision   = reg_a.fecha
    LET g_reg5.fecha_valor     = reg_a.fecha
    LET g_reg5.usuario         = vusuario
    LET g_reg5.identificador   = reg_a.tipo
    LET g_reg5.proceso_cod     = reg_a.proceso
    LET g_reg5.fecha_actualiza = HOY
    LET g_reg5.estado          = 20

--->erm 15 Junio 2006
{    IF reg_a.proceso = '00010' THEN
       IF g_reg5.importe < 0 THEN
          LET g_reg5.importe = g_reg5.importe * -1
       END IF
    END IF}
---<

    INSERT INTO con_transaccion VALUES(g_reg5.*)

    LET g_reg5.importe         = reg_a.diferencia * vprecio
    LET g_reg5.identificador   = 1

--->erm 15 Junio 2006
{    IF reg_a.proceso = '00010' THEN
       IF g_reg5.importe < 0 THEN
          LET g_reg5.importe = g_reg5.importe * -1
       END IF
    END IF}
---<

    INSERT INTO con_transaccion VALUES(g_reg5.*)

END FUNCTION
FUNCTION verifica_diferencia(reg_a)
#ia---------------------------
    DEFINE reg_a   RECORD 
        folio             INTEGER,
        tipo_mov          SMALLINT,
        proceso           CHAR(05)
    END RECORD
    DEFINE  vaccion_dif   DECIMAL(15,2)
    DEFINE  vimporte_dif  DECIMAL(15,2)
    DEFINE  vimporte      DECIMAL(15,2)

    IF (reg_a.tipo_mov = 486 OR 
        reg_a.tipo_mov = 487) THEN
        SELECT ROUND(SUM(monto_en_acciones),2)
        INTO   vaccion_dif
        FROM   dis_cuenta
        WHERE  folio           = reg_a.folio
        AND    tipo_movimiento = reg_a.tipo_mov
        AND    subcuenta in(1,2,5,6,9)
    ELSE
        SELECT ROUND(SUM(monto_en_acciones),2)
        INTO   vaccion_dif
        FROM   dis_cuenta
        WHERE  folio           = reg_a.folio
        AND    tipo_movimiento not in(486,487)
        AND    subcuenta in(1,2,5,6,9)
    END IF
    IF vaccion_dif IS NULL THEN 
        LET vaccion_dif = 0
    END IF
    SELECT SUM(importe)
    INTO   vimporte_dif
    FROM   con_transaccion
    WHERE  folio         = reg_a.folio
    AND    proceso_cod   = reg_a.proceso
    AND    identificador = 2

    IF vimporte_dif IS NULL THEN 
        LET vimporte_dif = 0
    END IF

    LET vimporte = 0
    LET vimporte = vaccion_dif - vimporte_dif
    RETURN vimporte
END FUNCTION
FUNCTION registra_neteo(reg_a)
#ia---------------------------
    DEFINE reg_a   RECORD 
        fecha             DATE,
        proceso           CHAR(05)
    END RECORD
    DEFINE g_reg5  RECORD 
        folio             INTEGER,
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        siefore           SMALLINT,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
        fecha_actualiza   DATE,
	usuario           CHAR(08),
        estado            SMALLINT
    END RECORD
    DEFINE  vimporte      DECIMAL(15,2)
    DEFINE  ident         SMALLINT
    DEFINE  vsiefore      SMALLINT
    DEFINE  vtransaccion  INTEGER

    SELECT transaccion_cod
    INTO   g_reg5.transaccion_cod
    FROM   tab_transaccion
    WHERE  proceso_cod = reg_a.proceso  
    AND    descripcion_1 = "NETEO"

    DECLARE cur_net CURSOR FOR
        SELECT sum(importe),identificador,siefore
        FROM   con_transaccion
        WHERE  proceso_cod in("00009","00010")
        AND    fecha_emision = reg_a.fecha
        AND    transaccion_cod NOT IN(24011,29011,29022,24351,29351,99991)
        AND    estado = 20
        GROUP BY 2,3
	ORDER BY 2,3
    FOREACH cur_net INTO vimporte,ident,vsiefore

	 CASE ident
	    WHEN 1
           LET g_reg5.folio           = ""
           LET g_reg5.importe         = vimporte
	        LET g_reg5.fecha_emision   = reg_a.fecha
	        LET g_reg5.fecha_valor     = reg_a.fecha
	        LET g_reg5.usuario         = vusuario
	        LET g_reg5.identificador   = ident
	        LET g_reg5.siefore         = vsiefore
	        LET g_reg5.proceso_cod     = reg_a.proceso
	        LET g_reg5.fecha_actualiza = HOY
	        LET g_reg5.estado          = 20

           INSERT INTO con_transaccion VALUES(g_reg5.*)

	    WHEN 2
              LET g_reg5.folio           = ""
              LET g_reg5.importe         = vimporte
	      LET g_reg5.fecha_emision   = reg_a.fecha
	      LET g_reg5.fecha_valor     = reg_a.fecha
	      LET g_reg5.usuario         = vusuario
	      LET g_reg5.siefore         = vsiefore
	      LET g_reg5.identificador   = ident
	      LET g_reg5.proceso_cod     = reg_a.proceso
	      LET g_reg5.fecha_actualiza = HOY
	      LET g_reg5.estado          = 20

	      IF vimporte > 0 THEN
                 INSERT INTO con_transaccion VALUES(g_reg5.*)
 	      END IF

        END CASE
    END FOREACH

END FUNCTION

FUNCTION intereses_fovissste()

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    DEFINE vsiefore        SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE xpesos          DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE vfecha_sig      DATE
    DEFINE vfecha_fin      DATE
    DEFINE xfecha_ini      DATE
    DEFINE xfecha_fin      DATE
    DEFINE f_hoy           DATE
    DEFINE xfecha_mes_sig  DATE

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 20

    LET  f_hoy       = ""
    LET  xfecha_ini  = ""
    LET  xfecha_fin  = ""
    LET  f_hoy       = HOY
    LET  dia         = DAY(f_hoy)
    LET  xfecha_fin  = f_hoy - dia UNITS DAY
    LET  dia         = DAY(xfecha_fin) -1
    LET  xfecha_ini  = xfecha_fin - dia UNITS DAY
    LET  total_pesos = ""
    LET  f_conversion = vfecha

     DECLARE con_fovi CURSOR FOR
         SELECT transaccion_cod,subcuenta
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 MATCHES "*FOVISSSTE"

     FOREACH con_fovi INTO vtransaccion,subcta

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_pesos    = ""
         LET  f_conversion   = ""
         LET  f_proceso      = ""
--display xfecha_ini, " ", xfecha_fin sleep 10
         IF   subcta = 14  THEN
              LET principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                              " subcuenta, siefore ",
                              " FROM   dis_cuenta ",
--                              " WHERE  folio =","'", g_reg.folio,"'",
                              " WHERE ",
--                              " AND    subcuenta =","'", subcta,"'",
                              " subcuenta =","'", subcta,"'",
                              " AND    tipo_movimiento = 3 ",
                              " AND    fecha_conversion BETWEEN ","'",xfecha_ini,"'",
                              " AND ","'",xfecha_fin,"'",
                              " GROUP BY 2,3 "
              PREPARE con_111 FROM  principal
              DECLARE viv_21 CURSOR FOR con_111
              FOREACH viv_21 INTO total_pesos,subcta,vsiefore

--->erm mes siguiente
                 LET xfecha_mes_sig = xfecha_ini + 1 UNITS MONTH          ---mas 1 mes
                 LET xfecha_mes_sig = xfecha_mes_sig - 1 UNITS DAY
                     CALL habil_siguiente(xfecha_mes_sig) RETURNING xfecha_mes_sig
---<erm

                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_pesos
                 --LET g_reg.fecha_emision = xfecha_mes_sig
                 --LET g_reg.fecha_valor   = xfecha_mes_sig
                 LET g_reg.fecha_emision = HOY                  ---28 Agosto 2007
                 LET g_reg.fecha_valor   = xfecha_mes_sig
                 LET g_reg.identificador = 1

                 LET g_reg.folio         = ""

                 IF g_reg.importe <> 0 THEN
                     CALL registra_historico(g_reg.*) #rh
                 END IF

              END FOREACH
         END IF
     END FOREACH
END FUNCTION

--->erm de hsb 20 Junio 2006
FUNCTION trans_sief()
#oi---------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    DEFINE vsiefore        SMALLINT
    DEFINE vcomision       SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(500)
    DEFINE precio          DECIMAL(10,6)
    DEFINE xpesos          DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)

    DEFINE vtrans_tip_mov  INTEGER
    DEFINE vtip_mov        SMALLINT
    DEFINE principal_mov   CHAR(400)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10

    DECLARE con_10_tf CURSOR FOR
    SELECT transaccion_cod,
           subcuenta,
           subcuenta1,
           subcuenta2
    FROM   tab_transaccion
    WHERE  proceso_cod = g_reg.proceso_cod
    AND    descripcion_1 NOT MATCHES "*FRACCION"
    FOREACH con_10_tf INTO vtransaccion,subcta,subcta1,subcta2
      LET g_reg.transaccion_cod = vtransaccion

      LET total_acciones = ""
      LET total_pesos    = ""
      LET precio         = ""
      LET f_conversion   = ""
      LET f_proceso      = ""

      LET principal_mov = "SELECT transaccion_cod, tipo_movimiento",
                          " FROM  tab_con_movimiento ",
                          " WHERE proceso_cod = ","'",g_reg.proceso_cod,"'",
                          " AND   transaccion_cod = ",vtransaccion
      PREPARE mov_10    FROM principal_mov
      DECLARE c_tipmov10 CURSOR FOR mov_10
      FOREACH c_tipmov10 INTO vtrans_tip_mov,vtip_mov
      IF vtransaccion = 90093 OR
           vtransaccion = 90693 THEN
           LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                            " ROUND(SUM(a.monto_en_pesos),2),",
                            " a.fecha_conversion, a.siefore ",
                            " FROM   dis_cuenta a ",
                            " WHERE  a.folio =","'", g_reg.folio,"'",
                            " AND    a.subcuenta = ", subcta,
                            " AND    a.tipo_movimiento = ", vtip_mov,
                            " AND    a.id_aportante MATCHES 'TI-*' ",
                            " GROUP BY 3,4 "
           PREPARE issste_10 FROM  principal
           DECLARE c_issste10 CURSOR FOR issste_10
           FOREACH c_issste10 INTO total_acciones,total_pesos,f_conversion,
                                   vsiefore
             LET g_reg.siefore       = vsiefore
             LET g_reg.importe       = total_acciones
             LET g_reg.fecha_valor   = f_conversion
             LET g_reg.fecha_emision = f_conversion
             LET g_reg.identificador = 2

             IF g_reg.importe <> 0 THEN
                IF vcomision = 0 THEN
                   CALL registra_historico(g_reg.*) #rh
                END IF
                LET g_reg.importe       = total_pesos
                LET g_reg.identificador = 1
                CALL registra_historico(g_reg.*) #rh
             END IF
           END FOREACH
      END IF
{
-------->retencion
        IF vtransaccion = 26510 OR
           vtransaccion = 26511 THEN 
            LET  principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                             " ROUND(SUM(monto_en_acciones),2),",
                             " fecha_conversion, siefore, subcuenta ",
                             " FROM   dis_cuenta ",
                             " WHERE  folio =","'", g_reg.folio,"'",
                             " AND    tipo_movimiento = 10",
                             " AND   (subcuenta in(","'", subcta,"'",")",
                             " OR    subcuenta in(","'", subcta1,"'",")",
                             " OR    subcuenta in(","'", subcta2,"'","))",
                             " GROUP BY 3,4,5 "
            PREPARE con_322_tf FROM  principal
            DECLARE viv_122_tf CURSOR FOR con_322_tf
            FOREACH viv_122_tf INTO total_pesos,total_acciones,
                                 f_conversion,vsiefore

               LET g_reg.importe       = total_pesos
               LET g_reg.fecha_valor   = f_conversion
               LET g_reg.fecha_emision = f_conversion
               LET g_reg.identificador = 1
               LET g_reg.siefore       = vsiefore

                   IF g_reg.importe <> 0 THEN
                       CALL registra_historico(g_reg.*) #rh

                       LET g_reg.importe       = total_acciones
                       LET g_reg.identificador = 2

                       CALL registra_historico(g_reg.*) #rh

                   END IF
            END FOREACH
        END IF
---<}
	LET  vcomision = 0

        SELECT "X"
        FROM   tab_transaccion
        WHERE  transaccion_cod = g_reg.transaccion_cod
        AND    descripcion_1 MATCHES "COMISION*"
        GROUP BY 1
        IF STATUS <> NOTFOUND THEN
           LET total_acciones = 0
           LET total_pesos    = 0
           LET precio         = 0
           LET f_conversion   = ""
           LET  vcomision     = 1
           LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2)*(-1),",
                            " ROUND(SUM(monto_en_pesos),2)*(-1),",
                            " precio_accion, ",
                            " fecha_conversion, siefore ",
                            " FROM   dis_cuenta ",
                            " WHERE  folio =","'", g_reg.folio,"'",
                            " AND   (subcuenta in(","'", subcta,"'",")",
                            " OR    subcuenta in(","'", subcta1,"'",")",
                            " OR    subcuenta in(","'", subcta2,"'","))",
                            " AND tipo_movimiento BETWEEN 100 AND 106 ",
                            " GROUP BY 3,4,5 "
           PREPARE con_130 FROM  principal
           DECLARE rcv_20_tf CURSOR FOR con_130
           FOREACH rcv_20_tf INTO total_acciones,total_pesos, precio, f_conversion, 
                              vsiefore
             LET g_reg.siefore       = vsiefore
             LET g_reg.importe       = total_acciones
             LET g_reg.fecha_valor   = f_conversion
             LET g_reg.fecha_emision = f_conversion

             IF g_reg.importe <> 0 THEN
                LET g_reg.importe       = total_pesos
                LET g_reg.identificador = 1
                CALL registra_historico(g_reg.*) #rh
             END IF
           END FOREACH
        ELSE
           LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                            " ROUND(SUM(monto_en_pesos),2),",
                            " precio_accion, ",
                            " fecha_conversion, siefore ",
                            " FROM   dis_cuenta ",
                            " WHERE  folio =","'", g_reg.folio,"'",
                            " AND   (subcuenta in(","'", subcta,"'",")",
                            " OR    subcuenta in(","'", subcta1,"'",")",
                            " OR    subcuenta in(","'", subcta2,"'","))",
                           #" AND    tipo_movimiento not in(888,999) ",
                            " AND    tipo_movimiento = ", vtip_mov,
                            " GROUP BY 3,4,5 "
           PREPARE con_120 FROM  principal
           DECLARE rcv_100 CURSOR FOR con_120
           FOREACH rcv_100 INTO total_acciones,total_pesos,precio,f_conversion,
                                vsiefore
             LET g_reg.siefore       = vsiefore
             LET g_reg.importe       = total_acciones
             LET g_reg.fecha_valor   = f_conversion
             LET g_reg.fecha_emision = f_conversion
             LET g_reg.identificador = 2

             IF g_reg.importe <> 0 THEN
                IF vcomision = 0 THEN
                   CALL registra_historico(g_reg.*) #rh
                END IF
                LET g_reg.importe       = total_pesos
                LET g_reg.identificador = 1
                CALL registra_historico(g_reg.*) #rh
             END IF
           END FOREACH
        END IF
      END FOREACH
    END FOREACH

END FUNCTION
---<erm de hsb 20 Junio 2006
--->erm 20 Sep 2006
FUNCTION obtiene_voluntarias()
#ov---------------------------

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    DEFINE vsiefore        SMALLINT
    DEFINE vcomision       SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE precio          DECIMAL(10,6)
    DEFINE xpesos          DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10


     DECLARE con_1vol CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_1vol INTO vtransaccion,subcta,subcta1,subcta2

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_acciones = ""
         LET  total_pesos    = ""
         LET  precio         = ""
         LET  f_conversion   = ""
         LET  f_proceso      = ""

         IF (subcta = 4    OR
             subcta = 8    OR
             subcta = 14 ) THEN #fhh se anexo 02-Dic-2004
              LET principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                              " fecha_conversion, siefore ",
                              " FROM   dis_cuenta ",
                              " WHERE  fecha_conversion =","'",vfecha,"'",
                              " AND  (subcuenta in(","'", subcta,"'",")",
                              " OR   subcuenta in(","'", subcta1,"'","))",
                              " AND    tipo_movimiento not in(888,999) ",
                             " GROUP BY 2,3 "
              PREPARE con_vol1 FROM  principal
              DECLARE viv_vol1 CURSOR FOR con_vol1
              FOREACH viv_vol1 INTO total_pesos,f_conversion,vsiefore
                                  --,f_proceso

                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_pesos
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.identificador = 1

                 IF g_reg.proceso_cod = "00009" THEN
                     SELECT MAX(fecha_conversion)
                     INTO   f_conversion
                     FROM   dis_cuenta
                     WHERE  folio = g_reg.folio

                     LET g_reg.fecha_emision = f_conversion
                     LET g_reg.fecha_valor   = f_conversion
                 END IF
                 IF g_reg.proceso_cod = "00001" OR
                    g_reg.proceso_cod = "00003" THEN
                     IF g_reg.transaccion_cod  = 24001 THEN
                         LET  xpesos = 0

                         SELECT SUM(monto_en_pesos)
                         INTO   xpesos
                         FROM   dis_provision
                         WHERE  folio           = g_reg.folio
                         AND    subcuenta       = 4
                         AND    tipo_movimiento = 3

                         IF xpesos IS NULL THEN
                             LET  xpesos = 0
                         END IF

                         LET g_reg.importe = g_reg.importe + xpesos

                         LET  xpesos = 0

                         SELECT SUM(monto_en_pesos)
                         INTO   xpesos
                         FROM   dis_cuenta
                         WHERE  fecha_conversion           = vfecha
                         AND    subcuenta       = 4
                         AND    tipo_movimiento = 7

                         IF xpesos IS NULL THEN
                             LET  xpesos = 0
                         END IF
                         LET g_reg.importe = g_reg.importe - xpesos

                     END IF
                 END IF

                 IF g_reg.importe <> 0 THEN
                     CALL registra_historico(g_reg.*) #rh
                 END IF

              END FOREACH

         ELSE

           #arc modificar el sum(acciones)
           #fhh se anexo 02-Dic-2004

             LET vcomision = 0

             CASE vtransaccion
             WHEN 90093
               LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                " ROUND(SUM(a.monto_en_pesos),2),",
                                " a.fecha_conversion, a.siefore ",
                                " FROM   dis_cuenta a ",
                                " WHERE  a.fecha_conversion =","'", vfecha,"'",
                                " AND    a.subcuenta = 13 ", 
                                " AND    a.tipo_movimiento in(1,4) ",
--		" AND    a.id_aportante MATCHES 'TI-*' ",
                                " GROUP BY 3,4 "
              PREPARE issste_vol FROM  principal
              DECLARE c_issste_vol CURSOR FOR issste_vol
              FOREACH c_issste_vol INTO total_acciones,total_pesos,f_conversion,
                                    vsiefore
                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_acciones
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 2

                 IF g_reg.importe <> 0 THEN
                     IF vcomision = 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                     END IF

                     LET g_reg.importe       = total_pesos
                     LET g_reg.identificador = 1
                      CALL registra_historico(g_reg.*) #rh
                 END IF
              END FOREACH
	      #fhh se anexo 02-Dic-2004

-->ejrm inicio agrega transaccion 90094 TRASPASO SAR-ISSSTE
             WHEN 90094
               LET  principal = " SELECT ROUND(SUM(a.monto_en_acciones),2),",
                                " ROUND(SUM(a.monto_en_pesos),2),",
                                " a.fecha_conversion, a.siefore ",
                                " FROM   dis_cuenta a ",
                                " WHERE  a.fecha_conversion =","'", vfecha,"'",
                                " AND    a.subcuenta = 13 ", 
                                " AND    a.tipo_movimiento = 220 ",
--                              " AND    a.id_aportante MATCHES 'TI-*' ",
                                " GROUP BY 3,4 "
              PREPARE issste_vol2 FROM  principal
              DECLARE c_issste2_vol CURSOR FOR issste_vol2
              FOREACH c_issste2_vol INTO total_acciones,total_pesos,f_conversion,
                                     vsiefore
                 LET g_reg.siefore       = vsiefore
                 LET g_reg.importe       = total_acciones
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.identificador = 2

                 IF g_reg.importe <> 0 THEN
                 IF vcomision = 0 THEN
                       CALL registra_historico(g_reg.*) #rh
                 END IF

                    LET g_reg.importe       = total_pesos
                    LET g_reg.identificador = 1
                    CALL registra_historico(g_reg.*) #rh
                 END IF
              END FOREACH
         #ejrm se anexo 12-sep-2005
--<ejrm fin de afrega transaccion 90094

             WHEN 21615
               LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2)*(-1),",
                                " ROUND(SUM(monto_en_pesos),2)*(-1),",
                                " precio_accion, ",
                                " fecha_conversion, siefore ",
                                " FROM   dis_cuenta ",
                                " WHERE  fecha_conversion =","'", vfecha,"'",
                                " AND   (subcuenta in(","'", subcta,"'",")",
                                " OR    subcuenta in(","'", subcta1,"'",")",
                                " OR    subcuenta in(","'", subcta2,"'","))",
                                " AND tipo_movimiento BETWEEN 100 AND 114 ",
                                " GROUP BY 3,4,5 "

               PREPARE con_vol13 FROM  principal
               DECLARE rcv_vol2 CURSOR FOR con_vol13
               FOREACH rcv_vol2 INTO total_acciones,
                                  total_pesos,
                                  precio,
                                  f_conversion,
                                  vsiefore
                  LET g_reg.siefore       = vsiefore
                  LET g_reg.importe       = total_acciones
                  LET g_reg.fecha_valor   = f_conversion
                  LET g_reg.fecha_emision = f_conversion
                  LET g_reg.identificador = 2

                  IF g_reg.importe <> 0 THEN
                     IF vcomision = 0 THEN
                         IF  (vproceso <> "00001" OR   #erm
                              vproceso <> "00002" OR
                              vproceso <> "00003")THEN #erm
                         ELSE                          #erm
                            CALL registra_historico(g_reg.*) #rh
                         END IF
                     END IF
                     LET g_reg.importe       = total_pesos
                     LET g_reg.identificador = 1
                     CALL registra_historico(g_reg.*) #rh
                  END IF
               END FOREACH

             OTHERWISE  
               IF vtransaccion <> 20115 THEN 
                  IF vfolio IS NULL THEN
                  LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   " ROUND(SUM(monto_en_pesos),2),",
                                   " precio_accion, ",
                                   " fecha_conversion, siefore ",
                                   " FROM   dis_cuenta ",
                                   " WHERE  fecha_conversion =","'", vfecha,"'",
                                   " AND   (subcuenta in(","'", subcta,"'",")",
                                   " OR    subcuenta in(","'", subcta1,"'",")",
                                   " OR    subcuenta in(","'", subcta2,"'","))",
                                   " AND    tipo_movimiento not in(888,999) ",
                                   " GROUP BY 3,4,5 "
                  ELSE
                  IF (vfolio IS NOT NULL) AND
                     (vfecha IS NULL)     THEN 
                  LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   " ROUND(SUM(monto_en_pesos),2),",
                                   " precio_accion, ",
                                   " fecha_conversion, siefore ",
                                   " FROM   dis_cuenta ",
                                   #" WHERE  fecha_conversion =","'", vfecha,"'",
                                   " WHERE folio = ", vfolio,
                                   " AND   (subcuenta in(","'", subcta,"'",")",
                                   " OR    subcuenta in(","'", subcta1,"'",")",
                                   " OR    subcuenta in(","'", subcta2,"'","))",
                                   " AND    tipo_movimiento not in(888,999) ",
                                   " GROUP BY 3,4,5 "
                  END IF
                  IF (vfolio IS NOT NULL) AND
                     (vfecha IS NOT NULL) THEN 
                  LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2),",
                                   " ROUND(SUM(monto_en_pesos),2),",
                                   " precio_accion, ",
                                   " fecha_conversion, siefore ",
                                   " FROM   dis_cuenta ",
                                   " WHERE  fecha_conversion =","'", vfecha,"'",
                                   " AND    folio = ", vfolio,
                                   " AND   (subcuenta in(","'", subcta,"'",")",
                                   " OR    subcuenta in(","'", subcta1,"'",")",
                                   " OR    subcuenta in(","'", subcta2,"'","))",
                                   " AND    tipo_movimiento not in(888,999) ",
                                   " GROUP BY 3,4,5 "
                  END IF
                  END IF
                  PREPARE con_vol12 FROM  principal
                  DECLARE rcv_vol1 CURSOR FOR con_vol12
                  FOREACH rcv_vol1 INTO total_acciones,total_pesos,
                                     precio,f_conversion,vsiefore
                     LET g_reg.siefore       = vsiefore
                     LET g_reg.importe       = total_acciones
                     LET g_reg.fecha_valor   = f_conversion
                     LET g_reg.fecha_emision = f_conversion
                     LET g_reg.identificador = 2

                     IF g_reg.importe <> 0 THEN
                         IF vcomision = 0 THEN
                                 CALL registra_historico(g_reg.*) #rh
                         END IF
                         LET g_reg.importe       = total_pesos
                         LET g_reg.identificador = 1
                         CALL registra_historico(g_reg.*) #rh
                     END IF
                  END FOREACH
              END IF     ---comentado 09 Dic 2005

               LET  vcomision = 0

              SELECT "X"
              FROM   tab_transaccion
              WHERE  transaccion_cod = g_reg.transaccion_cod
              AND    descripcion_1 MATCHES "COMISION*"
              GROUP BY 1

              IF STATUS <> NOTFOUND THEN
                 LET total_acciones = 0
                 LET total_pesos    = 0
                 LET precio         = 0
                 LET f_conversion   = ""
                 LET  principal = " SELECT ROUND(SUM(monto_en_acciones),2)*",
                                  " (-1),",
                                  " ROUND(SUM(monto_en_pesos),2)*(-1),",
                                  " precio_accion, ",
                                  " fecha_conversion, siefore ",
                                  " FROM   dis_cuenta ",
                                  " WHERE  fecha_conversion =","'", vfecha,"'",
                                  " AND   (subcuenta in(","'", subcta,"'",")",
                                  " OR    subcuenta in(","'", subcta1,"'",")",
                                  " OR    subcuenta in(","'", subcta2,"'","))",
                                  " AND tipo_movimiento BETWEEN 100 AND 114 ",
                                  " GROUP BY 3,4,5 "
                  PREPARE con_13_volcom FROM  principal
                  DECLARE rcv_2volcom CURSOR FOR con_13_volcom
                  FOREACH rcv_2volcom INTO total_acciones,
                                        total_pesos,
                                        precio,
                                        f_conversion,
                                        vsiefore
                     LET g_reg.siefore       = vsiefore
                     LET g_reg.importe       = total_acciones
                     LET g_reg.fecha_valor   = f_conversion
                     LET g_reg.fecha_emision = f_conversion
                     LET g_reg.identificador = 2

                     IF g_reg.importe <> 0 THEN
                         IF vcomision = 0 THEN
                             IF  (vproceso <> "00001" OR    #erm 11 Nov 2005
                                  vproceso <> "00002" OR
                                  vproceso <> "00003") THEN #erm 11 Nov 2005
                             ELSE                           #erm
                                CALL registra_historico(g_reg.*) #rh
                             END IF
                         END IF
                         LET g_reg.importe       = total_pesos
                         LET g_reg.identificador = 1
                         CALL registra_historico(g_reg.*) #rh
                     END IF
                     LET  vcomision     = 1
                  END FOREACH

              END IF
           END CASE
         END IF
         
     END FOREACH
END FUNCTION

FUNCTION devolucion_pagos_viv()

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    #DEFINE subcta          CHAR(05)
    DEFINE vsiefore        SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE importe         DECIMAL(15,2)
    #DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)
    DEFINE vdia            SMALLINT
    DEFINE cont2           SMALLINT

    LET g_reg.folio           = vfolio
    --LET g_reg.proceso_cod     = vproceso
    LET g_reg.proceso_cod     = '10023'
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 20

     DECLARE con_viv_1 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_viv_1 INTO vtransaccion,subcta,subcta1,subcta2

            LET  g_reg.transaccion_cod = vtransaccion

            LET  total_acciones = ""
            LET  total_pesos    = ""
            LET  f_conversion   = ""

               IF subcta = 4 THEN
#vivienda se liquida el 1er día habil del mes con el precio de la accion del 1er día natural
                    LET principal = " SELECT SUM(ROUND(monto_en_pesos,2)),",
                                    " fecha_conversion, siefore ",
                                    --" FROM   dis_provision ",
                                    " FROM   dis_cuenta ",
                                    " WHERE  folio =","'", g_reg.folio,"'",
                                    " AND  (subcuenta in(","'", subcta,"'",")",
                                    " OR   subcuenta in(","'", subcta1,"'","))",
                                    " GROUP BY 2,3 "
                    PREPARE con_viv_2 FROM  principal
                    DECLARE viv_17_viv CURSOR FOR con_viv_2
                    FOREACH viv_17_viv INTO total_pesos,f_conversion,vsiefore

                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 1
                       LET g_reg.siefore       = vsiefore
                       LET g_reg.proceso_cod   = '00023'

                    IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                       END IF
                    END FOREACH
              END IF 

     END FOREACH
END FUNCTION

--->erm 18 Diciembre 2007
FUNCTION recaudacion_issste()

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    DEFINE f_proceso       DATE
    DEFINE vsiefore        SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE principal_2     CHAR(350)
    DEFINE xpesos          DECIMAL(15,2)
    DEFINE importe         DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)
    DEFINE total_pesos_com DECIMAL(22,6)
    DEFINE total_acciones_com DECIMAL(22,6)
    DEFINE vfecha_sig      DATE
    DEFINE vfecha_fin      DATE
    DEFINE xfecha_ini      DATE
    DEFINE xfecha_fin      DATE
    DEFINE f_hoy           DATE
    DEFINE xfecha_mes_sig  DATE

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 10

     DECLARE con_rec_issste CURSOR FOR
         SELECT transaccion_cod,subcuenta
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH con_rec_issste INTO vtransaccion,subcta

         LET  g_reg.transaccion_cod = vtransaccion

         LET  total_pesos        = ""
         LET  total_acciones     = ""
         LET  total_pesos_com    = ""
         LET  total_acciones_com = ""
         LET  f_conversion       = ""
         LET  f_proceso          = ""

            CASE vtransaccion
                WHEN 90093

         LET  principal = " SELECT ROUND(SUM(a.monto_en_pesos),2), ",
                                   "        ROUND(SUM(a.monto_en_acciones),2), ",
                                   " a.fecha_conversion, a.siefore ",
                                   " FROM   dis_cuenta a ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND    subcuenta =","'", subcta,"'",
                                   " AND    a.tipo_movimiento in (1,2,3,4) ",
                                   " GROUP BY 3,4 "
                    PREPARE rec_iss FROM  principal
                    DECLARE rec_isss2 CURSOR FOR rec_iss
                    FOREACH rec_isss2 INTO total_pesos,total_acciones,f_conversion,vsiefore

                    LET  principal_2 = " SELECT ROUND(SUM(a.monto_en_pesos),2), ",
                                          "        round(sum(a.monto_en_acciones),2), ",
                                          " a.fecha_conversion, a.siefore ",
                                          " FROM   dis_cuenta a ",
                                          " WHERE  folio =","'", g_reg.folio,"'",
                                          " AND    subcuenta =","'", 13,"'",
                                          " AND tipo_movimiento BETWEEN 100 AND 114 ",
                                          " AND siefore = ",vsiefore,
                                          " GROUP BY 3,4 "
                          PREPARE rec_is2 FROM  principal_2
                          DECLARE rec_is3 CURSOR FOR rec_is2
                          FOREACH rec_is3 INTO total_pesos_com,total_acciones_com,f_conversion,vsiefore

                             LET g_reg.importe       = total_pesos + total_pesos_com
                             LET g_reg.fecha_valor   = f_conversion
                             LET g_reg.fecha_emision = f_conversion
                             LET g_reg.siefore       = vsiefore
                             LET g_reg.identificador = 1

                             IF g_reg.importe <> 0 THEN
                                 CALL registra_historico(g_reg.*) #rh
                                 LET g_reg.importe       = total_acciones + total_acciones_com
                                 LET g_reg.identificador = 2
                                 CALL registra_historico(g_reg.*) #rh
                             END IF
                          END FOREACH

                    IF total_pesos_com IS NOT NULL OR
                       total_acciones_com IS NOT NULL THEN
                       CONTINUE FOREACH
                    ELSE

                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.siefore       = vsiefore
                       LET g_reg.identificador = 1

                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           LET g_reg.importe       = total_acciones
                           LET g_reg.identificador = 2
                           CALL registra_historico(g_reg.*) #rh
                       END IF
                    END IF

                    END FOREACH

                 WHEN 22615  #comisiones
                  LET  principal = " SELECT ROUND(SUM(a.monto_en_pesos),2), ",
                                   " a.fecha_conversion, a.siefore ",
                                   " FROM   dis_cuenta a ",
                                   " WHERE  folio =","'", g_reg.folio,"'",
                                   " AND    subcuenta =","'", subcta,"'",
                                   " AND tipo_movimiento BETWEEN 100 AND 114 ",
                                   " GROUP BY 2,3 "
                     PREPARE rec_iss2 FROM  principal
                     DECLARE rec_isss3 CURSOR FOR rec_iss2
                     FOREACH rec_isss3 INTO total_pesos,f_conversion,vsiefore
                        LET g_reg.importe       = total_pesos
                        LET g_reg.fecha_valor   = f_conversion
                        LET g_reg.fecha_emision = f_conversion
                        LET g_reg.siefore       = vsiefore
                        LET g_reg.identificador = 1
                        IF g_reg.importe <> 0 THEN
                            CALL registra_historico(g_reg.*) #rh
                        END IF
                     END FOREACH
              END CASE

         IF   subcta = 14  THEN
              LET principal = " SELECT ROUND(SUM(monto_en_pesos),2),",
                              " subcuenta, siefore,fecha_conversion ",
                              " FROM   dis_cuenta ",
                              " WHERE  folio =","'", g_reg.folio,"'",
                              " AND    subcuenta =","'", subcta,"'",
                              " AND    tipo_movimiento in (1,3) ",
                               " GROUP BY 2,3,4 "
              PREPARE rec_iss3 FROM  principal
              DECLARE rec_isss4 CURSOR FOR rec_iss3
              FOREACH rec_isss4 INTO total_pesos,subcta,vsiefore,f_conversion

                 LET g_reg.siefore       = 0
                 LET g_reg.importe       = total_pesos
                 LET g_reg.fecha_emision = f_conversion
                 LET g_reg.fecha_valor   = f_conversion
                 LET g_reg.identificador = 1

                 IF g_reg.importe <> 0 THEN
                     CALL registra_historico(g_reg.*) #rh
                 END IF

              END FOREACH
         END IF
     END FOREACH
END FUNCTION
---<erm 18 Diciembre 2007

FUNCTION bono_pension()

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    #DEFINE subcta          CHAR(05)
    DEFINE vsiefore        SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE importe         DECIMAL(15,2)
    #DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)
    DEFINE vdia            SMALLINT
    DEFINE cont2           SMALLINT

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 20

     DECLARE bono_pen_1 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH bono_pen_1 INTO vtransaccion,subcta,subcta1,subcta2

            LET  g_reg.transaccion_cod = vtransaccion

            LET  total_acciones = ""
            LET  total_pesos    = ""
            LET  f_conversion   = ""

               --IF subcta = 36 THEN
               IF vtransaccion = 29023 THEN

                    LET principal = " SELECT ROUND(SUM(saldo_ini_pesos_udis),2), ",
                                    "        ROUND(SUM(saldo_ini_udis),2), ",
                                    " fecha_actualiza ",
                                    " FROM   sdo_ini_bono ",
                                    " WHERE  folio =","'", g_reg.folio,"'",
                                    " GROUP BY 3 "
                    PREPARE bono_pen FROM  principal
                    DECLARE bono_pens_ini CURSOR FOR bono_pen
                    FOREACH bono_pens_ini INTO total_pesos,total_acciones,f_conversion

                       LET g_reg.importe       = total_acciones
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2
                       LET g_reg.siefore       = 13
                       LET g_reg.proceso_cod   = vproceso

                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           #registra los pesos menos las udis y se contailiza en la cuenta 710403
                           LET g_reg.importe       = total_pesos
                           LET g_reg.identificador = 1
                           LET g_reg.transaccion_cod = 29025
                           CALL registra_historico(g_reg.*) #rh
                       END IF
                    END FOREACH
              END IF 

     END FOREACH
END FUNCTION

FUNCTION bono_pension_diaria()

    DEFINE vtransaccion    INTEGER
    DEFINE f_conversion    DATE
    #DEFINE subcta          CHAR(05)
    DEFINE vsiefore        SMALLINT
    DEFINE subcta          SMALLINT
    DEFINE subcta1         SMALLINT
    DEFINE subcta2         SMALLINT
    DEFINE principal       CHAR(350)
    DEFINE importe         DECIMAL(15,2)
    #DEFINE total_pesos     DECIMAL(15,2)
    DEFINE total_pesos     DECIMAL(22,6)
    DEFINE total_acciones  DECIMAL(22,6)
    DEFINE vdia            SMALLINT
    DEFINE cont2           SMALLINT

    LET g_reg.folio           = vfolio
    LET g_reg.proceso_cod     = vproceso
    LET g_reg.fecha_actualiza = HOY
    LET g_reg.usuario         = vusuario
    LET g_reg.estado          = 20

     DECLARE bono_pen_2 CURSOR FOR

         SELECT transaccion_cod,
                subcuenta,
                subcuenta1,
                subcuenta2
         FROM   tab_transaccion
         WHERE  proceso_cod = g_reg.proceso_cod
         AND    descripcion_1 NOT MATCHES "*FRACCION"

     FOREACH bono_pen_2 INTO vtransaccion,subcta,subcta1,subcta2

            LET  g_reg.transaccion_cod = vtransaccion

            LET  total_acciones = ""
            LET  total_pesos    = ""
            LET  f_conversion   = ""

               --IF subcta = 36 THEN
            CASE vtransaccion 
               WHEN  29023
                    LET principal = " SELECT ROUND(SUM(mov_dia_710401),2), ",
                                    " fecha_actualiza ",
                                    " FROM   act_dia_bono ",
                                    " WHERE  folio =","'", g_reg.folio,"'",
                                    " AND fecha_actualiza = ","'",vfecha,"'",
                                    " GROUP BY 2 "
                    PREPARE bono_dia1 FROM  principal
                    DECLARE bono_pens CURSOR FOR bono_dia1
                    FOREACH bono_pens INTO total_acciones,f_conversion

                       LET g_reg.importe       = total_acciones
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2
                       LET g_reg.siefore       = 13
                       LET g_reg.proceso_cod   = vproceso

                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           #registra pesos tal cual no se contabiliza en ninguna cuenta
                       END IF
                    END FOREACH

               WHEN  29024
                    LET principal = " SELECT ROUND(SUM(mov_dia_710402),2), ",
                                    " fecha_actualiza ",
                                    " FROM   act_dia_bono ",
                                    " WHERE  folio =","'", g_reg.folio,"'",
                                    " AND fecha_actualiza = ","'",vfecha,"'",
                                    " GROUP BY 2 "
                    PREPARE bono_dia2 FROM  principal
                    DECLARE bono_pens1 CURSOR FOR bono_dia2
                    FOREACH bono_pens1 INTO total_acciones,f_conversion

                       LET g_reg.importe       = total_acciones
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 2
                       LET g_reg.siefore       = 13
                       LET g_reg.proceso_cod   = vproceso

                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           #registra pesos tal cual no se contabiliza en ninguna cuenta
                       END IF
                    END FOREACH

               WHEN  29025
                    LET principal = " SELECT ROUND(SUM(mov_dia_710403),2), ",
                                    " fecha_actualiza ",
                                    " FROM   act_dia_bono ",
                                    " WHERE  folio =","'", g_reg.folio,"'",
                                    " AND fecha_actualiza = ","'",vfecha,"'",
                                    " GROUP BY 2 "
                    PREPARE bono_dia3 FROM  principal
                    DECLARE bono_pens2 CURSOR FOR bono_dia3
                    FOREACH bono_pens2  INTO total_pesos,f_conversion

                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 1
                       LET g_reg.siefore       = 13
                       LET g_reg.proceso_cod   = vproceso

                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           #registra pesos tal cual no se contabiliza en ninguna cuenta
                       END IF
                    END FOREACH

               WHEN  29026
                    LET principal = " SELECT ROUND(SUM(mov_dia_710404),2), ",
                                    " fecha_actualiza ",
                                    " FROM   act_dia_bono ",
                                    " WHERE  folio =","'", g_reg.folio,"'",
                                    " AND fecha_actualiza = ","'",vfecha,"'",
                                    " GROUP BY 2 "
                    PREPARE bono_dia4 FROM  principal
                    DECLARE bono_pens3 CURSOR FOR bono_dia4
                    FOREACH bono_pens3 INTO total_pesos,f_conversion

                       LET g_reg.importe       = total_pesos
                       LET g_reg.fecha_valor   = f_conversion
                       LET g_reg.fecha_emision = f_conversion
                       LET g_reg.identificador = 1
                       LET g_reg.siefore       = 13
                       LET g_reg.proceso_cod   = vproceso

                       IF g_reg.importe <> 0 THEN
                           CALL registra_historico(g_reg.*) #rh
                           #registra pesos tal cual no se contabiliza en ninguna cuenta
                       END IF
                    END FOREACH
              END CASE

     END FOREACH
END FUNCTION

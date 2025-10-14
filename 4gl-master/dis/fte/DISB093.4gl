###############################################################################
#Proyecto          => Safre                                                   #
#Propietario       => E.F.P.                                                  #
#Modulo            => DIS.                                                    #
#Programa          => DISB093                                                 #
#Descripcion       => Genara Saldos de la cuentas individual(OFICIO consar272)#
#                  => Lanzador.                                               #
#Fecha Inicio      => 30-agosto-2006                                          #
#Fecha Termino     =>                                                         #
#Autor             => JOSE ALEJANDRO RAMIREZ LARA                             #
#Modificacion      => En lugar de tomar los datos de tmp_saldo_estadistica,   #
#                  => tomara de la tabla tmp_saldo_corte (formato A)          #
###############################################################################

DATABASE safre_tmp

GLOBALS
 DEFINE
   gparam_dis    RECORD LIKE safre_af:seg_modulo.*,
   gusuario      CHAR(08),
   vcod_afore    LIKE safre_af:tab_afore_local.codigo_afore,
   v_curp         CHAR(18),
   v_interes     DECIMAL(15,2),
   v_total_nss   INTEGER,
   vfecha_corte  ,
   HOY           ,
   f_ini         ,
   f_fin         DATE,
   f_proximo     DATE,
   v_listado     CHAR(300),
   v_saldo_cero  ,
   v_total_saldos SMALLINT,
   hora_final    CHAR(08),
   hora_inicial  CHAR(08),
   ejecuta       CHAR(500)

DEFINE opc       CHAR(01)
DEFINE f_valida  DATE
DEFINE r_saldos RECORD
       nss       CHAR(11),
       subcuenta SMALLINT,
       pesos     DECIMAL(15,2),
       fecha     DATE
       END RECORD

DEFINE vvalor_accion LIKE safre_af:glo_valor_accion.precio_del_dia

END GLOBALS

MAIN
  DEFINE ltime CHAR(10)

  OPTIONS INPUT WRAP,
          PROMPT LINE LAST,
          ACCEPT KEY CONTROL-C

  LET ltime = time
  CALL STARTLOG("DISB093.log")
--let vfecha_corte = '20060831'

  LET HOY = today

  OPEN WINDOW ventana_1 AT 2,2 WITH FORM "DISB0931" ATTRIBUTE(BORDER)
  DISPLAY " DISB093           GENERA ESTADISTICA DE SALDOS MENSUALES                     " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

  LET int_flag= FALSE

  LET vfecha_corte = MDY(MONTH(today),1,YEAR(today))-1
  DISPLAY BY NAME vfecha_corte

  PROMPT "Desea ejecutar los saldos [S/N] ? " FOR opc
  IF opc MATCHES "[Ss]" THEN

--  SELECT "x"                                                       --v4
--  FROM   safre_af:dis_ctrl_proceso
--  WHERE  proceso_cod='DISB093'
--  AND    parametro2=vfecha_corte
--  GROUP BY 1

--  IF STATUS <> NOTFOUND THEN                                       --v44
--       ERROR "Esta fecha ya fue calculada, dar enter para salir"   --v44
--       prompt '' for opc                                           --v44
--       exit program                                                --v44
--  ELSE                                                             --v44 
        SELECT "x"
        FROM cta_ctr_fmto_a
        WHERE fecha_corte=vfecha_corte
        AND paso=3
        AND fecha_ini is not null

        IF STATUS = NOTFOUND THEN
           ERROR "La fecha ingresada aun no corre para el formato A, dar enter para salir"
           prompt '' for opc
           exit program
        END IF
--  END IF                                                           --v44

    ERROR "Procesando Informacion ..."
    ERROR ""
    CLEAR FORM
    CLEAR SCREEN
    CLOSE WINDOW ventana_1
  ELSE
    CLOSE WINDOW ventana_1
    EXIT PROGRAM
  END IF

  ERROR "Procesando Informacion ..."
  CALL Inicializa()

  --Verifica si existe precio del primer dia natural del siguiente mes
  LET f_proximo = MDY(MONTH(f_fin),1,YEAR(f_fin))+1 

  SELECT precio_del_dia
  INTO   vvalor_accion
  FROM   safre_af:glo_valor_accion
  WHERE  fecha_valuacion = f_proximo
  AND    codigo_siefore  = 11

  IF vvalor_accion IS NULL THEN
     ERROR "NO EXISTE PRECIO DE ACCION DE LA SIEFORE 11,PARA:",f_proximo
     EXIT PROGRAM
  END IF

  CALL Ingresa_ctrl_proceso(1,"Obtiene saldos")
  LET ejecuta = "nohup time fglgo DISB094.4gi ",vfecha_corte CLIPPED," &"
  RUN ejecuta

  ERROR "FINALIZA LA EJECUCION"
  
END MAIN

FUNCTION Inicializa()

   DEFINE v_mes SMALLINT

   LET v_total_nss = 0

   SELECT *,USER
   INTO   gparam_dis.*,gusuario
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "dis"

   SELECT codigo_afore
   INTO vcod_afore
   FROM safre_af:tab_afore_local

   LET v_mes = MONTH(vfecha_corte)

   LET f_ini = MDY(MONTH(vfecha_corte),1,YEAR(vfecha_corte))

   IF v_mes = 12 THEN
      LET f_fin = MDY(1,1,YEAR(vfecha_corte)+1)-1
   ELSE
      LET f_fin = MDY(MONTH(vfecha_corte)+1,1,YEAR(vfecha_corte))-1
   END IF

END FUNCTION

FUNCTION Ingresa_ctrl_proceso(vetapa,vproceso)

   DEFINE vetapa INTEGER
   DEFINE vproceso CHAR(15)

   --INSERTA REGISTRO EN dis_ctrl_proceso
   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO safre_af:dis_ctrl_proceso VALUES
      (HOY,                     -- fecha_proceso
       "DISB093",               -- proceso_cod
       vetapa,                  -- etapa_cod   -- LECTURA
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       vproceso,                -- parametro1
       vfecha_corte,            -- parametro2
       f_ini,                   -- parametro3
       f_fin,                   -- parametro4
       NULL,                    -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       gusuario,
       0)                       -- usuario
   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso Lectura Archivo",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF

END FUNCTION


#############################################################################
#Proyecto            => SAFRE ( MEXICO )                                    #
#Propietario         => E.F.P.                                              #
#Programa            => CTA_OFICIO                                          #
#Descripcion         => OFICIO CONSAR 492-06                                #
#Por                 => OMAR SABNDOVAL BADILLO                              #
#Fecha               => 21 de noviembre de 2006                             #
#Sistema             => CTA.                                                #
#############################################################################
DATABASE safre_af
GLOBALS
    DEFINE   g_param_cta           RECORD LIKE seg_modulo.* 

    DEFINE reg_res RECORD
        nss                      CHAR(11)
    END RECORD                   
                                 
    DEFINE HOY                   DATE
    DEFINE vfolio                INTEGER
     
    DEFINE carga_reg             CHAR(330),
           vcodigo_afore         SMALLINT,
           vrazon_social         CHAR(050),
           usuario               CHAR(008),
           enter    	         CHAR(001),      
           generar               CHAR(020),
           opc                   CHAR(001),
           archivo_carga         CHAR(200),
           G_LISTA               CHAR(200) 

    DEFINE cont                  SMALLINT,   
           cuantos               SMALLINT   

END GLOBALS
#######################################################################
MAIN
    OPTIONS
        PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST
        DEFER INTERRUPT

    SELECT *
    INTO   g_param_cta.*
    FROM   seg_modulo
    WHERE  modulo_cod = "cta"

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_pla_oficio
        CREATE TEMP TABLE tmp_pla_oficio
        (
         n_registros          CHAR(30)
        )
    WHENEVER ERROR STOP

    LET HOY  = TODAY

    CALL STARTLOG("CTA_OFI.log")

    SELECT codigo_afore,
           razon_social
    INTO   vcodigo_afore,
           vrazon_social
    FROM   tab_afore_local

    CALL proceso()

END MAIN
#######################################################################
FUNCTION proceso()

    OPEN WINDOW ventana_01 AT 2,2 WITH FORM "CTA_OFI" ATTRIBUTE(BORDER)
    DISPLAY "       [Enter]  Iniciar                                  [Ctrl-C]  Salir       " AT 1,1 ATTRIBUTE(REVERSE)  
    DISPLAY " OFICIO 492-06  RECIBE ARCHIVO DE GENERACION DE OFICIO 492-06                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL carga()

   CLOSE WINDOW ventana_01
END FUNCTION
#######################################################################
FUNCTION carga()

   DISPLAY "INGRESE EL NOMBRE DEL ARCHIVO A PROCESAR :" AT 7,9

   INPUT BY NAME generar WITHOUT DEFAULTS
      BEFORE FIELD generar
         LET generar = NULL
         CLEAR FORM
         DISPLAY  "                                              "
         AT 19,2 

      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
         END IF

         WHENEVER ERROR CONTINUE
            LET archivo_carga = g_param_cta.ruta_rescate CLIPPED,"/",
                                generar CLIPPED

            LOAD FROM archivo_carga DELIMITER "+" INSERT INTO tmp_pla_oficio
                  
            SELECT count(*)
            INTO   cuantos
            FROM   tmp_pla_oficio
                
            IF cuantos = 0 THEN
                DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                AT 19,2 ATTRIBUTE(REVERSE)
                SLEEP 2
                NEXT FIELD generar
            ELSE
                EXIT INPUT
            END IF
         WHENEVER ERROR STOP

      ON KEY(INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM
   END INPUT

   ERROR " PROCESANDO INFORMACION ..."
   SLEEP 3

   CALL lee_archivo_plano()

   ERROR ""
   PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
   FOR CHAR enter

END FUNCTION
#######################################################################
FUNCTION lee_archivo_plano()

    DEFINE v_arch_oficio         CHAR(100),
           permisos              CHAR(200)

    DEFINE l_detalle RECORD
       nss                     CHAR(11),
       fecha_movimiento        DATE,
       subcuenta               CHAR(2),
       monto_en_pesos          DECIMAL(18,2),
       monto_en_acciones       DECIMAL(10,6),
       siefore                 SMALLINT,
       tipo_movimiento_s       CHAR(3),
       tipo_movimiento_p       CHAR(2),
       desc_movimiento         CHAR(60)
    END RECORD

    LET v_arch_oficio = g_param_cta.ruta_envio CLIPPED,"/",
                        "OFICIO_492-06.",TODAY USING "DDMMYY"

    START REPORT r_report TO v_arch_oficio

    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    tmp_pla_oficio
   
    LET cont = 0

    FOREACH cur_1 INTO reg_res.nss

       LET cont = cont + 1
       DISPLAY " TOTAL NSS : ",cont AT 16,8

       CALL genera_tmp_cuenta(reg_res.nss)

       DECLARE cur_2 CURSOR FOR
       SELECT a.nss,
              a.fecha_conversion,
              b.procesar_sub,
              a.monto_en_pesos,
              a.monto_en_acciones,
              a.siefore,
              a.tipo_movimiento,
              c.procesar_mov,
              UPPER(c.descripcion)
       FROM   tmp_dis_cuenta a,
              safre_tmp:tmp_equ_subcuenta b,
              safre_tmp:tmp_equ_movimiento c
       WHERE  a.subcuenta       = b.safre_sub
       AND    a.tipo_movimiento = c.safre_mov
       ORDER BY 1,2,3

       FOREACH cur_2 INTO l_detalle.*

          IF l_detalle.siefore = 11 THEN
             LET l_detalle.siefore = 0
          END IF

          OUTPUT TO REPORT r_report(l_detalle.*)
       END FOREACH

       INITIALIZE  reg_res.*  TO NULL
    END FOREACH

    FINISH REPORT r_report

    DISPLAY "Archivo generado en: ",v_arch_oficio CLIPPED AT 19,2 

    LET permisos = "chmod 777 ",v_arch_oficio CLIPPED
    RUN permisos

END FUNCTION
#######################################################################
FUNCTION genera_tmp_cuenta (p_nss)

   DEFINE p_nss           CHAR(11)

   DEFINE v_nombre_tabla  CHAR(20) ,
          v_anio                   ,
          v_anio_ini               ,
          v_anio_fin      SMALLINT

   DEFINE v_anio_c        CHAR(01) ,
          sel_his         CHAR(1000)

   DEFINE v_fecha         DATE,
          fecha_afiliado  DATE

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   SELECT fentcons
   INTO   fecha_afiliado
   FROM   afi_mae_afiliado
   WHERE  n_seguro = p_nss

   LET v_anio_ini = YEAR( fecha_afiliado )
   LET v_anio_fin = YEAR( HOY )


   FOR v_anio = v_anio_ini TO v_anio_fin

      LET v_fecha  = MDY(1,1,v_anio)
      LET v_anio_c = v_fecha USING "YY"

      LET v_nombre_tabla = "dis_cuenta",v_anio_c CLIPPED

      SELECT "tabla"
      FROM   systables
      WHERE  tabname = v_nombre_tabla

      IF SQLCA.SQLCODE = 0 THEN
         LET sel_his = sel_his CLIPPED,
                     " SELECT nss,",
                             "fecha_conversion,",
                             "subcuenta,",
                             "monto_en_pesos,",
                             "monto_en_acciones,",
                             "siefore,",
                             "tipo_movimiento",
                     " FROM  ",v_nombre_tabla,
                     " WHERE nss = ","'",p_nss,"'",
                     " UNION ALL "
      END IF
   END FOR

   LET sel_his = sel_his CLIPPED,
               " SELECT nss,",
                       "fecha_conversion,",
                       "subcuenta,",
                       "monto_en_pesos,",
                       "monto_en_acciones,",
                       "siefore,",
                       "tipo_movimiento",
               " FROM   dis_cuenta ",
               " WHERE  nss = ","'",p_nss,"'",
               " INTO TEMP tmp_dis_cuenta "

   PREPARE eje_sel_his FROM sel_his
   EXECUTE eje_sel_his

   CREATE INDEX tmp_dis_cuenta1 on tmp_dis_cuenta ( tipo_movimiento )
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

   DELETE 
   FROM  tmp_dis_cuenta
   WHERE tipo_movimiento = 999

   CREATE INDEX tmp_dis_cuenta2 on tmp_dis_cuenta (fecha_conversion)
   CREATE INDEX tmp_dis_cuenta3 on tmp_dis_cuenta (subcuenta)
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION
#######################################################################
REPORT r_report(l_reg_oficio)

   DEFINE l_reg_oficio RECORD
      nss                     CHAR(11),
      fecha_movimiento        DATE,
      subcuenta               CHAR(2),
      monto_en_pesos          DECIMAL(18,2),
      monto_en_acciones       DECIMAL(10,6),
      siefore                 CHAR(1),
      tipo_movimiento_s       CHAR(3),
      tipo_movimiento_p       CHAR(2),
      desc_movimiento         CHAR(60)
   END RECORD

   DEFINE desc_movimiento     CHAR(60)

   OUTPUT
      PAGE LENGTH   1 
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT
      ON EVERY ROW
         IF l_reg_oficio.tipo_movimiento_p = 99 THEN
            SELECT descripcion
            INTO   l_reg_oficio.desc_movimiento
            FROM   tab_movimiento
            WHERE  codigo = l_reg_oficio.tipo_movimiento_s
            GROUP BY 1
         END IF

   PRINT l_reg_oficio.nss                                       ,"	",
         l_reg_oficio.fecha_movimiento USING "YYYYMMDD"         ,"	",
         l_reg_oficio.subcuenta                                 ,"	",
         l_reg_oficio.monto_en_pesos USING "###############&.##","	",
         l_reg_oficio.monto_en_acciones USING "###&.######"     ,"	",
         l_reg_oficio.siefore                                   ,"	",
         l_reg_oficio.tipo_movimiento_p                         ,"	",
         l_reg_oficio.desc_movimiento
END REPORT

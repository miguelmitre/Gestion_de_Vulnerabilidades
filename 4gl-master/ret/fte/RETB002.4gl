###############################################################################
#Propietario         => E.F.P.                                                #
#Programa            => GENERA ARCHIVO DE RETIROS SAR 92 EFECTUADOS POR MEDIO #
#                       DEL ANEXO "A" (PLAN PRIVADO DE PENSION)               #
#Fecha               => 13 DE FEBRERO DEL 2007                                #
#Realizado           => CIRILO DE LA CRUZ                                     #
#Fecha actualizacion =>                                                       #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE
        g_seg_modulo           RECORD LIKE seg_modulo.*

    DEFINE reg_1 RECORD #glo #reg_1
        folio                 INTEGER  ,
        nss                   CHAR(11) ,
        subcuenta             SMALLINT ,
        fecha_conversion      DATE     ,
        monto_en_pesos        DECIMAL(16,6)
    END RECORD

    DEFINE #glo #char
        c12_usuario           CHAR(012) ,
        reporte_ppp           CHAR(100) ,
        nom_archivo           CHAR(015) ,
        G_LISTA               CHAR(100) ,
        enter                 CHAR(001)

    DEFINE #glo #smallint
        s_codigo_afore        SMALLINT

    DEFINE 
        HOY                   ,
        vfecha                DATE
    DEFINE
        g_paramgrales  RECORD LIKE seg_modulo.*

END GLOBALS


MAIN
 DEFINE vdfecha DATE

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST
---    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT
   

    CALL init()  #i
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "RETB0021" ATTRIBUTE(BORDER)
    DISPLAY "RETB001      GENERA ARCHIVO DE LOS RETIROS DE SAR 92 (ANEXO A)               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "    [ Esc ] Iniciar                                       [ Ctrl-C ] Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfecha WITHOUT DEFAULTS

      AFTER FIELD vfecha
         IF vfecha IS NULL THEN 
            ERROR "DEBE CAPTURAR UNA FECHA VALIDA"
            NEXT FIELD vfecha
         END IF

  ---   ON KEY (ESC)  

    ---     EXIT INPUT
      ---    ERROR " PROCESANDO INFORMACION " 

        ---  CALL primer_paso()   #gr

     ---- PROMPT " PROCESO FINALIZADO ...[ENTER] PARA CONTINUAR "
     --- FOR enter

      ---EXIT INPUT

     --- ON KEY (INTERRUPT)
     ----    ERROR " PROCESO CANCELADO "
    ---     SLEEP 2
    ---     EXIT PROGRAM

    END INPUT
    
    IF NOT int_flag THEN
          ERROR " PROCESANDO INFORMACION " 

          CALL primer_paso()   #pp

          DISPLAY " EL ARCHIVO HA SIDO GENERADO EN LA RUTA : " AT 11,20
          DISPLAY G_LISTA CLIPPED AT 13,21
          DISPLAY " CON EL NOMBRE : ",nom_archivo AT 15,20

          PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter
    ELSE
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM
 
    END IF  

END MAIN


FUNCTION init()
#i-------------

    LET HOY    = TODAY
    LET vfecha = HOY

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           c12_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    LET G_LISTA     = g_seg_modulo.ruta_envio
    LET nom_archivo = "PPP",HOY USING"YYYYMMDD",".txt"
    LET reporte_ppp = G_LISTA CLIPPED,"/",nom_archivo

END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    ---determinar si aun no se ha ejecutado el proceso ----

       CALL valida_datos_ret_his_sar()
       CALL segundo_paso() #sp
     
END FUNCTION

#############################################################################
FUNCTION genera_tmp_cuenta(p_fecha_fin )

   DEFINE p_fecha_ini             ,
          p_fecha_fin     DATE

   DEFINE v_nombre_tabla  CHAR(20) ,
          v_anio                   ,
          v_anio_ini               ,
          v_anio_fin      SMALLINT

   DEFINE v_anio_c        CHAR(02) ,
          sel_his         CHAR(2000)

   DEFINE v_fecha         DATE

   WHENEVER ERROR CONTINUE
      DROP TABLE paso_dis_cuenta;
   WHENEVER ERROR STOP

   LET v_anio_ini = 1997
   LET v_anio_fin = YEAR( p_fecha_fin )


   FOR v_anio = v_anio_ini TO v_anio_fin

      LET v_fecha  = MDY(1,1,v_anio)
      LET v_anio_c = v_fecha USING "YY"

      LET v_nombre_tabla = "dis_cuenta",v_anio_c CLIPPED

      SELECT "tabla"
      FROM   systables
      WHERE  tabname = v_nombre_tabla

      IF SQLCA.SQLCODE = 0 THEN

         LET sel_his = sel_his CLIPPED,
                       " SELECT folio    ,",
                       "        nss      ,",
                       "        subcuenta,",
                       "        fecha_conversion ,'",vfecha ,"' fecha_proceso,",
                       " sum(monto_en_pesos) monto_en_pesos ",
                       " FROM   ", v_nombre_tabla,
                       " WHERE  subcuenta IN (7,8)  ",
                       " AND    tipo_movimiento IN(433,434,499,496,498,840)",
                       " GROUP BY 1,2,3,4,5 " ,
                       " UNION ALL "
      END IF
   END FOR

   LET sel_his = sel_his CLIPPED,
      " SELECT folio    ,",
      "        nss      ,",
      "        subcuenta,",
      "        fecha_conversion ,'",vfecha ,"' fecha_proceso,",
      " sum(monto_en_pesos) monto_en_pesos ",
      " FROM   dis_cuenta   ",
      " WHERE  subcuenta IN (7,8)  ",
      " AND    tipo_movimiento IN(433,434,499,496,498,840)",
      " GROUP BY 1,2,3,4,5 ",
      " INTO TEMP paso_dis_cuenta "

   PREPARE eje_prioridad FROM "SET PDQPRIORITY HIGH"
   EXECUTE eje_prioridad

   PREPARE eje_sel_his FROM sel_his
   EXECUTE eje_sel_his
   UPDATE STATISTICS FOR TABLE ret_his_sar

   INSERT INTO ret_his_sar 
   SELECT A.folio            ,
          A.nss              ,
          A.subcuenta        ,
          A.fecha_conversion ,
          A.monto_en_pesos   ,
          A.fecha_proceso    ,
          USER
   FROM paso_dis_cuenta A

   
   {CREATE INDEX ret_his_sar1 on ret_his_sar (folio,nss,subcuenta,fecha_proceso)
   UPDATE STATISTICS FOR TABLE ret_his_sar
   }
   

END FUNCTION


FUNCTION genera_tmp_cuenta_10(p_fecha_fin )

   DEFINE p_fecha_ini             ,
          p_fecha_fin     DATE

   DEFINE v_nombre_tabla  CHAR(20) ,
          v_anio                   ,
          v_anio_ini               ,
          v_anio_fin      SMALLINT

   DEFINE v_anio_c        CHAR(02) ,
          sel_his         CHAR(2000)

   DEFINE v_fecha         DATE

   WHENEVER ERROR CONTINUE
      DROP TABLE paso_dis_cuenta;
   WHENEVER ERROR STOP

   LET v_anio_ini = 1997
   LET v_anio_fin = YEAR( p_fecha_fin )


   FOR v_anio = v_anio_ini TO v_anio_fin

      LET v_fecha  = MDY(1,1,v_anio)
      LET v_anio_c = v_fecha USING "YY"

      LET v_nombre_tabla = "dis_cuenta",v_anio_c CLIPPED

      SELECT "tabla"
      FROM   systables
      WHERE  tabname = v_nombre_tabla

      IF SQLCA.SQLCODE = 0 THEN

         LET sel_his = sel_his CLIPPED,
                       " SELECT A.folio    ,",
                       "        A.nss      ,",
                       "        A.subcuenta,",
                       "        A.fecha_conversion ,'",vfecha ,"' fecha_proceso,",
                       "        sum(A.monto_en_pesos) monto_en_pesos ",
                       " FROM   ", v_nombre_tabla," A,"," ret_his_sar B",
                       " WHERE  A.nss              = B.nss ",
                       " AND    A.subcuenta        = B.subcuenta ",
                       " AND    A.fecha_conversion = B.fecha_conversion ",
                       " AND    A.folio            = B.folio ",
                       " AND    A.tipo_movimiento IN(10)",
                       " AND    B.fecha_proceso    = '",vfecha,"'",
                       " GROUP BY 1,2,3,4,5 " ,
                       " UNION ALL "
      END IF
   END FOR

   LET sel_his = sel_his CLIPPED,
      " SELECT A.folio    ,",
      "        A.nss      ,",
      "        A.subcuenta,",
      "        A.fecha_conversion ,'",vfecha ,"' fecha_proceso,",
      "        sum(A.monto_en_pesos) monto_en_pesos ",
      " FROM   dis_cuenta A, ret_his_sar B             ",
      " WHERE  A.nss              = B.nss              ",
      " AND    A.subcuenta        = B.subcuenta        ",
      " AND    A.fecha_conversion = B.fecha_conversion ",
      " AND    A.folio            = B.folio            ",
      " AND    A.tipo_movimiento IN(10)                ",
      " AND    B.fecha_proceso    = '",vfecha,"'",
      " GROUP BY 1,2,3,4,5 ",
      " INTO TEMP paso_dis_cuenta "

   {
   PREPARE eje_prioridad_10 FROM "SET PDQPRIORITY HIGH"
   EXECUTE eje_prioridad_10
   }

#ff
   PREPARE eje_sel_his_10 FROM sel_his
   EXECUTE eje_sel_his_10
  #UPDATE STATISTICS FOR TABLE ret_his_sar

   INSERT INTO ret_his_sar 
   SELECT A.folio            ,
          A.nss              ,
          A.subcuenta        ,
          A.fecha_conversion ,
          A.monto_en_pesos   ,
          A.fecha_proceso    ,
          USER
   FROM   paso_dis_cuenta A

   
   {CREATE INDEX ret_his_sar1 on ret_his_sar (folio,nss,subcuenta,fecha_proceso)
   UPDATE STATISTICS FOR TABLE ret_his_sar
   }
   

END FUNCTION

FUNCTION valida_datos_ret_his_sar()
#vdrhs----------------------------
    DEFINE #loc #char
        procesar_sn           CHAR(1)


   SELECT "OK"
   FROM   ret_his_sar
   WHERE  fecha_proceso = vfecha
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
       -- llamar a la funcion e carga  ---
       DISPLAY "                             " AT 21,1
       ERROR ""
       DISPLAY "EJECUTANDO PROCESO"   AT 21,1 ATTRIBUTE (REVERSE)
       CALL genera_tmp_cuenta(vfecha)
       CALL genera_tmp_cuenta_10(vfecha )
   ELSE
      ------ ya se ejecuto el proceso  ---
      PROMPT "EL PROCESO YA HA SIDO EJECUTADO, ¿DESEAS REPROCESARLO S/N?"
      FOR CHAR procesar_sn

      IF procesar_sn MATCHES "[sS]"  THEN
           --- EJECUTAR NUEVAMENTE EL PROCESO ----

        DISPLAY "                             " AT  21,1
        ERROR ""
         display "EJECUTANDO PROCESO.."  AT 21,1   ATTRIBUTE (REVERSE)

           ---eliminar datos anteriores para evitar duplicidad de datos ---
         DELETE FROM ret_his_sar
         WHERE fecha_proceso=vfecha

          ---- hacer nuevamente la carga de datos ---
         CALL genera_tmp_cuenta(vfecha)
         CALL genera_tmp_cuenta_10(vfecha )
    
      ELSE 
          ERROR "PROCESO CANCELADO"

      END IF
   END IF
END FUNCTION

FUNCTION segundo_paso()
#sp-------------------
    DEFINE reg_2 RECORD #loc #reg_2
        folio                 INTEGER              ,
        nss                   LIKE ret_his_sar.nss ,
        fecha_conversion      DATE
    END RECORD

    DEFINE reg_3 RECORD #loc #reg_3
        monto_pesos_sub7      DECIMAL(10,2) ,
        monto_pesos_sub8      DECIMAL(10,2)

    END RECORD
    DEFINE
        fecha_conversion_ant  LIKE ret_his_sar.fecha_conversion ,
        subcuenta_ant         LIKE ret_his_sar.subcuenta   

    DEFINE #loc #dec
        d10_2_monto_sub7      DECIMAL(10,2) ,
        monto_subcuenta7      ,
        monto_subcuenta8      LIKE ret_his_sar.monto_en_pesos 

    DEFINE #loc #integer
        ndatos_generados      INTEGER

    DEFINE #loc #decimal
        dmonto_subcuenta7,
        dmonto_subcuenta8     DECIMAL(8,2)

  
    #ff
    DECLARE cur_retiros_sar92 CURSOR FOR
    SELECT A.folio              ,
           A.nss                ,
           A.fecha_conversion
    FROM   ret_his_sar A
    WHERE  A.fecha_proceso = vfecha 
    GROUP BY 1,2,3

    START REPORT rep_ret_sar92 TO reporte_ppp
        FOREACH cur_retiros_sar92 INTO reg_2.*

            SELECT NVL(SUM(monto_en_pesos),0)
            INTO   reg_3.monto_pesos_sub7
            FROM   ret_his_sar
            WHERE  fecha_proceso = vfecha
            AND    nss           = reg_2.nss
            AND    subcuenta     = 7
            AND    folio         = reg_2.folio

            SELECT NVL(SUM(monto_en_pesos),0)
            INTO   reg_3.monto_pesos_sub8
            FROM   ret_his_sar
            WHERE  fecha_proceso = vfecha
            AND    nss           = reg_2.nss
            AND    subcuenta     = 8
            AND    folio         = reg_2.folio

        OUTPUT TO REPORT rep_ret_sar92(reg_2.nss              ,
                                       reg_2.fecha_conversion ,
                                       reg_3.monto_pesos_sub7 ,
                                       reg_3.monto_pesos_sub8
                                      )
        END FOREACH
    FINISH REPORT rep_ret_sar92
END FUNCTION

REPORT rep_ret_sar92(reg_10)
#rrs-----------------------
    DEFINE reg_10 RECORD #loc #reg_10
        nss                   LIKE ret_his_sar.nss ,
        fecha_conversion      DATE                 ,
        monto_pesos_sub7      DECIMAL(10,2)        ,
        monto_pesos_sub8      DECIMAL(10,2)
    END RECORD

    DEFINE reg_11 RECORD #loc #reg_11
        rfc                   CHAR(13) ,
        curp                  CHAR(18) ,
        paterno               CHAR(40) ,
        materno               CHAR(40) ,
        nombre                CHAR(40)
    END RECORD

    DEFINE #loc #char
        c11_monto_pesos_sub7  CHAR(11) ,
        c10_monto_pesos_sub7  CHAR(10) ,
        c11_monto_pesos_sub8  CHAR(11) ,
        c10_monto_pesos_sub8  CHAR(10)

    OUTPUT 
        PAGE LENGTH   1 
        LEFT MARGIN   0
        RIGHT MARGIN  0 
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
         SELECT n_rfc   ,
                n_unico ,
                paterno ,
                materno ,
                nombres 
         INTO   reg_11.*
         FROM   afi_mae_afiliado
         WHERE  n_seguro = reg_10.nss

         PRINT
             COLUMN 001,s_codigo_afore USING"&&&"                        ,
             COLUMN 004,reg_10.nss                                       ,
             COLUMN 015,reg_11.paterno                                   ,
             COLUMN 055,reg_11.materno                                   ,
             COLUMN 095,reg_11.nombre                                    ,
             COLUMN 135,reg_11.rfc                                       ,
             COLUMN 148,reg_11.curp                                      ,
             COLUMN 166,reg_10.fecha_conversion USING "yyyymmdd"         ,
             COLUMN 174,reg_10.monto_pesos_sub7 * 100 USING "&&&&&&&&&&" ,
             COLUMN 184,reg_10.monto_pesos_sub8 * 100 USING "&&&&&&&&&&" ,
             COLUMN 190,22 SPACES

END REPORT

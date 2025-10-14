##############################################################################
#Owner             => E.F.P.
#Programa TRAC055  => TRASPASO AL MAESTRO DE ICEFAS         
#Fecha creacion    => 22 DE NOVIEMBRE DE 2001
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 23 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af  
GLOBALS
    DEFINE estado                    SMALLINT
    DEFINE v_format                  CHAR(030)
    DEFINE k                         INTEGER 
    DEFINE u                         INTEGER
    DEFINE g_raz_social              LIKE    tab_afore_local.razon_social
    DEFINE g_cod_afore               LIKE    tab_afore_local.codigo_afore
    DEFINE x_busca                   CHAR(500)
    DEFINE c8_usuario                CHAR(008)
    DEFINE cont                      INTEGER
    DEFINE cont_asi                  INTEGER
    DEFINE cont_acep                 INTEGER
    DEFINE cont_dev                  INTEGER
    DEFINE cont_dup                  INTEGER
    DEFINE cont1                     INTEGER
    DEFINE reg_tra_det_automatico    RECORD LIKE safre_tmp:tra_det_automatico.*
    DEFINE reg_afi_mae_afiliado      RECORD LIKE afi_mae_afiliado.*
    DEFINE reg_ruta                  RECORD LIKE seg_modulo.*
    DEFINE RUTA                      CHAR(300)
    DEFINE RUTA_F                    CHAR(300)
    DEFINE HOY                       DATE
    DEFINE reg_1                     RECORD 
           cve_ced_cuenta            CHAR(003),
           asignados                 CHAR(001),
           t_lote                    INTEGER 
                                     END RECORD
    DEFINE enter                     CHAR(001)
    DEFINE v_row                     INTEGER
    DEFINE HORA                      CHAR(008)
    DEFINE sw                        SMALLINT
    DEFINE cod_afore                 LIKE tab_afore_local.codigo_afore
    DEFINE raz_social                LIKE tab_afore_local.razon_social
    DEFINE g_nom_prog                CHAR(07)
    DEFINE hay_regs_rpt1             INTEGER
    DEFINE hay_regs_rpt2             INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT      
    OPTIONS
    ACCEPT KEY CONTROL-I ,
    INPUT WRAP           ,
    PROMPT LINE LAST


   CALL init()

   OPEN WINDOW trac0552  AT 4,4 WITH FORM "TRAC0552" ATTRIBUTE(BORDER)

    MENU "TRASPASO AL MAESTRO DE ICEFAS IMSS"
    COMMAND "Validacion" "Validacion automatica y manual" 
       CALL init()
       CALL uno()
    COMMAND "Con Reverso" "Aceptadas con un reverso previo"
       CALL rev() #c
    COMMAND "Salir" "Salir del Programa"
          EXIT MENU
  END MENU
END MAIN

FUNCTION uno()
#u------------

   OPEN WINDOW trac0551  AT 4,4 WITH FORM "TRAC0551" ATTRIBUTE(BORDER)
   DISPLAY " TRAC055      TRASPASO AL MAESTRO DE SOLIC. ICEFA-AFORE IMSS                   " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                              " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)


  CONSTRUCT BY NAME x_busca ON a.folio_interno      ,
                    a.cve_ced_cuenta     ,
                    a.tipo_criterio      ,
                    a.saldo_sar_92       ,
                    a.saldo_viv_92       
        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
            RETURN

    END CONSTRUCT
{  LET estado = NULL
   INPUT BY NAME estado,reg_1.t_lote WITHOUT DEFAULTS
       AFTER FIELD estado 
	 IF (estado                      =                                10 OR 
 	     estado                      =                                14 OR
	     estado                      =                                50 OR
	     estado  IS NULL) THEN
         ELSE
	    ERROR "ESTADO NO PUEDE SER DISTINTO DE 10,14,50 O NULO"
	    NEXT FIELD estado
	    END IF
}

   INPUT BY NAME reg_1.t_lote WITHOUT DEFAULTS

       AFTER FIELD t_lote
  
     WHILE TRUE
       PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
       IF enter MATCHES "[sSnN]" THEN
        IF enter MATCHES "[sS]" THEN
           EXIT INPUT
        ELSE
         DISPLAY "                                                                               " at 19,1
         DISPLAY"PROCESO CANCELADO " AT 19,2 ATTRIBUTE(REVERSE) SLEEP 2
         SLEEP 2
         DISPLAY "                                                                               " at 19,1
         EXIT PROGRAM 
        END IF
       END IF
     END WHILE

   ON KEY(INTERRUPT)
      PROMPT "PROCESO CANCELADO <ENTER> PARA SALIR..." for char enter
      EXIT PROGRAM

   END INPUT

   DISPLAY "PROCESANDO INFORMACION ...." AT 19,2 ATTRIBUTE(REVERSE)

   CALL primer_paso()
   CALL genera_reporte()

   PROMPT "PROCESO FINALIZADO...<ENTER> PARA CONCLUIR .."
   FOR char enter
   CLOSE WINDOW trac0551
END FUNCTION

FUNCTION init()   
#i-------------
    LET HORA                              =                            TIME
    LET HOY                               =                            TODAY
    LET reg_1.t_lote                      =                            0
    LET reg_1.cve_ced_cuenta              =                            NULL
    LET reg_1.asignados                   =                            NULL
    LET cont                              =                            0
    LET cont_dup                          =                            0
    LET cont_asi                          =                            0
    LET cont_acep                         =                            0
    LET cont_dev                          =                            0
    LET sw                                =                            0
    LET g_nom_prog                        =                            "TRAC055"
    LET hay_regs_rpt1                     =                            0 
    LET hay_regs_rpt2                     =                            0 

    SELECT USER
    INTO c8_usuario
    FROM tab_afore_local

    SELECT * 
    INTO   reg_ruta.*
    FROM   seg_modulo   
    WHERE  modulo_cod = "tra"

    LET RUTA     = reg_ruta.ruta_listados CLIPPED,
                   "/",c8_usuario CLIPPED,".TMAESTRO.",
                   HOY USING"YYYYMMDD",".",HORA CLIPPED

    LET RUTA_F   = reg_ruta.ruta_listados CLIPPED ,
                   "/",c8_usuario CLIPPED,".RFORM.",
                   HOY USING"YYYYMMDD",".",HORA CLIPPED

    START  REPORT salida TO RUTA_F 

    SELECT  codigo_afore,razon_social
    INTO    g_cod_afore,g_raz_social
    FROM    tab_afore_local

WHENEVER ERROR CONTINUE 
DROP TABLE paso_maestro
WHENEVER ERROR STOP

CREATE TEMP TABLE paso_maestro
  (
    cve_ced_cuenta char(3),
    n_seguro_ent char(11),
    rfc_ent char(13),
    nro_ctrl_icefa char(30),
    nombre_ent char(120),
    fecha_nacimiento date,
    marca_viv char(1),
    marca_retiro char(1),
    bimestres_acum smallint,
    rfc_patronal char(13),
    reg_patronal char(11),
    nombre_patron char(120),
    exp_infonavit char(9),
    saldo_sar_92 decimal(10,2),
    saldo_viv_92 decimal(10,2),
    sar_92_issste decimal(10,2),
    viv_92_issste decimal(10,2),
    cve_afore char(3),
    n_seguro char(11),
    rfc char(13),
    tipo_criterio smallint,
    estado smallint,
    fecha_edo date,
    diagnostico smallint,
    folio_interno integer,
    correlativo serial not null ,
    cad_valida char(5),
    liga_correlativo integer
  )

END FUNCTION

FUNCTION primer_paso()
#pp-------------------

DEFINE paterno           CHAR(40)
DEFINE materno           CHAR(40)
DEFINE nombre            CHAR(40)
DEFINE qry               CHAR(500)    
DEFINE edo               CHAR(30)

IF ( estado is NULL  OR estado = " "  OR estado = 0)  THEN
  LET edo = ' IN (10,14,50)'
END IF

IF estado = 10 THEN
   LET edo = ' = 10'
END IF

IF estado = 14 THEN
   LET edo = ' = 14'
END IF

IF estado = 50 THEN
   LET edo = ' = 50'
END IF


        LET qry = "SELECT a.rowid,a.*,b.* ",
                  " FROM safre_tmp:tra_det_automatico a ," ,
                  "      safre_af:afi_mae_afiliado   b  " ,
                  "WHERE  ",x_busca CLIPPED ,
                  " AND    a.n_seguro = b.n_seguro ",
                  " AND    a.estado   ",edo CLIPPED
 
      LET qry = qry CLIPPED
      PREPARE qry_sql FROM qry
      DECLARE cur_1 CURSOR FOR qry_sql 
    
      FOREACH cur_1 INTO v_row,reg_tra_det_automatico.*,reg_afi_mae_afiliado.* 

      LET cont = cont + 1

      IF reg_1.t_lote > 0 THEN
         IF cont_acep >= reg_1.t_lote THEN
            EXIT FOREACH
         END IF
      END IF

      DISPLAY "REGISTROS PROCESADOS...",cont AT 14,2

      IF reg_afi_mae_afiliado.tipo_solicitud = 5 THEN
         LET cont_asi = cont_asi + 1
         DISPLAY "REGISTROS ASIGNADOS ...",cont_asi AT 15,2
         LET reg_tra_det_automatico.estado = 27    

         UPDATE safre_tmp:tra_det_automatico
         SET safre_tmp:tra_det_automatico.estado = 27
         WHERE safre_tmp:tra_det_automatico.rowid = v_row

         INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
         CONTINUE FOREACH
      END IF 

        SELECT "OK"
        FROM  tra_mae_icefa a
        WHERE a.n_seguro    = reg_tra_det_automatico.n_seguro
        AND   a.nss         = reg_tra_det_automatico.n_seguro_ent
        AND   a.rfc         = reg_tra_det_automatico.rfc_ent
        AND   a.icefa_cod   = reg_tra_det_automatico.cve_ced_cuenta
        AND   a.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
        GROUP BY 1

  IF STATUS = NOTFOUND THEN

       LET paterno = reg_tra_det_automatico.nombre_ent[1,40] 
       LET materno = reg_tra_det_automatico.nombre_ent[41,80] 
       LET nombre = reg_tra_det_automatico.nombre_ent[81,120] 

              CALL formatea(reg_tra_det_automatico.*)
              RETURNING reg_tra_det_automatico.* 

               SELECT "OK"
               FROM  safre_af:tra_mae_icefa a
               WHERE a.n_seguro    = reg_tra_det_automatico.n_seguro 
               AND   a.nss         = reg_tra_det_automatico.n_seguro_ent
               AND   a.rfc         = reg_tra_det_automatico.rfc_ent
               AND   a.icefa_cod   = reg_tra_det_automatico.cve_ced_cuenta
               AND   a.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
               GROUP BY 1

               IF STATUS = NOTFOUND THEN

                 UPDATE safre_tmp:tra_det_automatico 
                 SET safre_tmp:tra_det_automatico.estado = 30
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                  INSERT INTO tra_mae_icefa
                  VALUES(reg_afi_mae_afiliado.n_folio          ,
                         reg_afi_mae_afiliado.tipo_solicitud   ,
                         reg_afi_mae_afiliado.n_seguro         ,
                         reg_tra_det_automatico.n_seguro_ent ,
                         reg_tra_det_automatico.rfc_ent      ,
                         paterno   ,
                         materno   ,
                         nombre    ,
                         "0",
                         reg_tra_det_automatico.cve_ced_cuenta ,
                         reg_tra_det_automatico.nro_ctrl_icefa ,
                         TODAY,
                         "",
                         "",
                         0                     ,#saldo_sar_92
                         0                     ,#saldo_viv_92
                         TODAY,
                         TODAY,
                         ""                     ,#lote_genera
                         ""                     ,#fecha_genera
                         1                      ,#status
                         3                      ,
                         0                      ,#correlativo
                         c8_usuario         ,
                         ""                     ,#n_envios
                         ""                      #diagnostico
                        )

            LET reg_tra_det_automatico.estado = 30
            INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
            LET cont_acep = cont_acep + 1 
            DISPLAY "REGISTROS ACEPTADOS...",cont_acep AT 16,2
            CONTINUE FOREACH
          ELSE
######           
           SELECT "OK"
           FROM   tra_mae_icefa A
           WHERE A.n_seguro    = reg_tra_det_automatico.n_seguro
           AND   A.nss         = reg_tra_det_automatico.n_seguro_ent
           AND   A.rfc         = reg_tra_det_automatico.rfc_ent
           AND   A.icefa_cod   = reg_tra_det_automatico.cve_ced_cuenta
           AND   A.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
           AND   A.status IN (2,4,7,8,10,15,16,17,41)
	   GROUP BY 1
           
           IF STATUS <> NOTFOUND THEN
              SELECT "OK"
              FROM dev_det_normal  A
              WHERE A.n_seguro       = reg_tra_det_automatico.n_seguro
              AND   A.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
              AND   A.rfc_ent        = reg_tra_det_automatico.rfc_ent
              AND   A.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta
              AND   A.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
              AND   A.status         = 1
              GROUP BY 1

              IF STATUS <> NOTFOUND THEN
  
                 UPDATE safre_tmp:tra_det_automatico
                 SET safre_tmp:tra_det_automatico.estado =  36
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                 LET reg_tra_det_automatico.estado = 36
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_dev = cont_dev + 1
                 DISPLAY "REGISTROS ACEPTADOS CON REVERSO...",cont_dev AT 16,2
                 CONTINUE FOREACH
               ELSE 
                 UPDATE  safre_tmp:tra_det_automatico
                 SET     safre_tmp:tra_det_automatico.estado = 32
                 WHERE   safre_tmp:tra_det_automatico.rowid = v_row

                 LET reg_tra_det_automatico.estado = 32
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_dup = cont_dup + 1
                 DISPLAY "REGISTROS DUPLICADOS EN MAESTRO...",cont_dup AT 17,2
                 CONTINUE FOREACH
               END IF
           ELSE

              SELECT "ok"
              FROM tra_mae_icefa a
              WHERE a.n_seguro    = reg_tra_det_automatico.n_seguro
              AND   a.nss         = reg_tra_det_automatico.n_seguro_ent
              AND   a.rfc         = reg_tra_det_automatico.rfc_ent
              AND   a.icefa_cod   = reg_tra_det_automatico.cve_ced_cuenta
              AND   a.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
              AND   a.fuente      = 3

              IF STATUS = NOTFOUND THEN

              DELETE FROM tra_mae_icefa
              WHERE tra_mae_icefa.n_seguro    = reg_tra_det_automatico.n_seguro
              AND   tra_mae_icefa.nss     = reg_tra_det_automatico.n_seguro_ent
              AND   tra_mae_icefa.rfc    = reg_tra_det_automatico.rfc_ent
        AND   tra_mae_icefa.icefa_cod = reg_tra_det_automatico.cve_ced_cuenta
        AND   tra_mae_icefa.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa

              LET paterno = reg_tra_det_automatico.nombre_ent[1,40] 
              LET materno = reg_tra_det_automatico.nombre_ent[41,80] 
              LET nombre = reg_tra_det_automatico.nombre_ent[81,120] 

                 UPDATE safre_tmp:tra_det_automatico 
                 SET   safre_tmp:tra_det_automatico.estado = 30
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                  INSERT INTO tra_mae_icefa
                  VALUES(reg_afi_mae_afiliado.n_folio          ,
                         reg_afi_mae_afiliado.tipo_solicitud   ,
                         reg_afi_mae_afiliado.n_seguro         ,
                         reg_tra_det_automatico.n_seguro_ent ,
                         reg_tra_det_automatico.rfc_ent      ,
                         paterno   ,
                         materno   ,
                         nombre    ,
                         "0",
                         reg_tra_det_automatico.cve_ced_cuenta ,
                         reg_tra_det_automatico.nro_ctrl_icefa ,
                         TODAY,
                         "",
                         "",
                         0                     ,#saldo_sar_92
                         0                     ,#saldo_viv_92
                         TODAY,
                         TODAY,
                         ""                     ,#lote_genera
                         ""                     ,#fecha_genera
                         1                      ,#status
                         3                      ,
                         0                      ,#correlativo
                         c8_usuario         ,
                         ""                     ,#n_envios
                         ""                      #diagnostico
                        )

                 LET reg_tra_det_automatico.estado = 30
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_acep = cont_acep + 1
                 DISPLAY "REGISTROS ACEPTADOS...",cont_acep AT 15,2
                 CONTINUE FOREACH

                ELSE
                 LET reg_tra_det_automatico.estado = 32
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)

                 UPDATE safre_tmp:tra_det_automatico 
                 SET   safre_tmp:tra_det_automatico.estado = 32
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                 LET cont_dup = cont_dup + 1
                 DISPLAY "REGISTROS DUPLICADOS EN MAESTRO...",cont_dup AT 17,2
                 CONTINUE FOREACH
                END IF
             END IF
#####
          END IF
          ELSE

           SELECT "OK"
           FROM   tra_mae_icefa A
           WHERE A.n_seguro    = reg_tra_det_automatico.n_seguro
           AND   A.nss         = reg_tra_det_automatico.n_seguro_ent
           AND   A.rfc         = reg_tra_det_automatico.rfc_ent
           AND   A.icefa_cod   = reg_tra_det_automatico.cve_ced_cuenta
           AND   A.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
           AND   A.status IN (2,4,7,8,10,15,16,17,41)
           GROUP BY 1

           IF STATUS <> NOTFOUND THEN
              SELECT "OK"
              FROM dev_det_normal  A
              WHERE A.n_seguro       = reg_tra_det_automatico.n_seguro
              AND   A.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
              AND   A.rfc_ent        = reg_tra_det_automatico.rfc_ent
              AND   A.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta
              AND   A.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
              AND   A.status         = 1
              GROUP BY 1
             
              IF STATUS <> NOTFOUND THEN
  
                 UPDATE safre_tmp:tra_det_automatico
                 SET safre_tmp:tra_det_automatico.estado =  36
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                 LET reg_tra_det_automatico.estado = 36
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_dev = cont_dev + 1
                 DISPLAY "REGISTROS ACEPTADOS CON REVERSO...",cont_dev AT 16,2
                 CONTINUE FOREACH
               ELSE 

                 UPDATE  safre_tmp:tra_det_automatico
                 SET     safre_tmp:tra_det_automatico.estado = 32
                 WHERE   safre_tmp:tra_det_automatico.rowid = v_row

                 LET reg_tra_det_automatico.estado = 32
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_dup = cont_dup + 1
                 DISPLAY "REGISTROS DUPLICADOS EN MAESTRO...",cont_dup AT 17,2
                 CONTINUE FOREACH
               END IF
           ELSE
              SELECT "ok"
              FROM tra_mae_icefa a
              WHERE a.n_seguro    = reg_tra_det_automatico.n_seguro
              AND   a.nss         = reg_tra_det_automatico.n_seguro_ent
              AND   a.rfc         = reg_tra_det_automatico.rfc_ent
              AND   a.icefa_cod   = reg_tra_det_automatico.cve_ced_cuenta
              AND   a.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
              AND   a.fuente      = 3

              IF STATUS = NOTFOUND THEN

              DELETE FROM tra_mae_icefa
              WHERE tra_mae_icefa.n_seguro    = reg_tra_det_automatico.n_seguro
              AND   tra_mae_icefa.nss     = reg_tra_det_automatico.n_seguro_ent
              AND   tra_mae_icefa.rfc    = reg_tra_det_automatico.rfc_ent
        AND   tra_mae_icefa.icefa_cod = reg_tra_det_automatico.cve_ced_cuenta
        AND   tra_mae_icefa.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa

              LET paterno = reg_tra_det_automatico.nombre_ent[1,40] 
              LET materno = reg_tra_det_automatico.nombre_ent[41,80] 
              LET nombre = reg_tra_det_automatico.nombre_ent[81,120] 

              CALL formatea(reg_tra_det_automatico.*)
              RETURNING reg_tra_det_automatico.* 

               SELECT "OK"
               FROM  tra_mae_icefa a
               WHERE a.n_seguro = reg_tra_det_automatico.n_seguro 
               AND   a.nss = reg_tra_det_automatico.n_seguro_ent
               AND   a.rfc = reg_tra_det_automatico.rfc_ent
               AND   a.icefa_cod = reg_tra_det_automatico.cve_ced_cuenta
               AND   a.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
               GROUP BY 1

               IF STATUS = NOTFOUND THEN
                 UPDATE safre_tmp:tra_det_automatico 
                 SET   safre_tmp:tra_det_automatico.estado = 30
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                  INSERT INTO tra_mae_icefa
                  VALUES(reg_afi_mae_afiliado.n_folio          ,
                         reg_afi_mae_afiliado.tipo_solicitud   ,
                         reg_afi_mae_afiliado.n_seguro         ,
                         reg_tra_det_automatico.n_seguro_ent ,
                         reg_tra_det_automatico.rfc_ent      ,
                         paterno   ,
                         materno   ,
                         nombre    ,
                         "0",
                         reg_tra_det_automatico.cve_ced_cuenta ,
                         reg_tra_det_automatico.nro_ctrl_icefa ,
                         TODAY,
                         "",
                         "",
                         0                     ,#saldo_sar_92
                         0                     ,#saldo_viv_92
                         TODAY,
                         TODAY,
                         ""                     ,#lote_genera
                         ""                     ,#fecha_genera
                         1                      ,#status
                         3                      ,
                         0                      ,#correlativo
                         c8_usuario         ,
                         ""                     ,#n_envios
                         ""                      #diagnostico
                        )

                 LET reg_tra_det_automatico.estado = 30
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_acep = cont_acep + 1
                 DISPLAY "REGISTROS ACEPTADOS...",cont_acep AT 15,2
                 CONTINUE FOREACH
                ELSE
#####
           SELECT "OK"
           FROM   tra_mae_icefa A
           WHERE A.n_seguro    = reg_tra_det_automatico.n_seguro
           AND   A.nss         = reg_tra_det_automatico.n_seguro_ent
           AND   A.rfc         = reg_tra_det_automatico.rfc_ent
           AND   A.icefa_cod   = reg_tra_det_automatico.cve_ced_cuenta
           AND   A.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
           AND   A.status IN (2,4,7,8,10,15,16,17,41)
	   GROUP BY 1
           
           IF STATUS <> NOTFOUND THEN
              SELECT "OK"
              FROM dev_det_normal  A
              WHERE A.n_seguro       = reg_tra_det_automatico.n_seguro
              AND   A.n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
              AND   A.rfc_ent        = reg_tra_det_automatico.rfc_ent
              AND   A.cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta
              AND   A.nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
              AND   A.status         = 1
              GROUP BY 1

              IF STATUS <> NOTFOUND THEN
  
                 UPDATE safre_tmp:tra_det_automatico
                 SET safre_tmp:tra_det_automatico.estado =  36
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                 LET reg_tra_det_automatico.estado = 36
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_dev = cont_dev + 1
                 DISPLAY "REGISTROS ACEPTADOS CON REVERSO...",cont_dev AT 16,2
                 CONTINUE FOREACH
               ELSE 
                 UPDATE  safre_tmp:tra_det_automatico
                 SET     safre_tmp:tra_det_automatico.estado = 32
                 WHERE   safre_tmp:tra_det_automatico.rowid = v_row

                 LET reg_tra_det_automatico.estado = 32
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_dup = cont_dup + 1
                 DISPLAY "REGISTROS DUPLICADOS EN MAESTRO...",cont_dup AT 17,2
                 CONTINUE FOREACH
               END IF
           ELSE

              SELECT "ok"
              FROM tra_mae_icefa a
              WHERE a.n_seguro    = reg_tra_det_automatico.n_seguro
              AND   a.nss         = reg_tra_det_automatico.n_seguro_ent
              AND   a.rfc         = reg_tra_det_automatico.rfc_ent
              AND   a.icefa_cod   = reg_tra_det_automatico.cve_ced_cuenta
              AND   a.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa
              AND   a.fuente      = 3

              IF STATUS = NOTFOUND THEN

              DELETE FROM tra_mae_icefa
              WHERE tra_mae_icefa.n_seguro    = reg_tra_det_automatico.n_seguro
              AND   tra_mae_icefa.nss     = reg_tra_det_automatico.n_seguro_ent
              AND   tra_mae_icefa.rfc    = reg_tra_det_automatico.rfc_ent
        AND   tra_mae_icefa.icefa_cod = reg_tra_det_automatico.cve_ced_cuenta
        AND   tra_mae_icefa.nro_int_cta = reg_tra_det_automatico.nro_ctrl_icefa

              LET paterno = reg_tra_det_automatico.nombre_ent[1,40] 
              LET materno = reg_tra_det_automatico.nombre_ent[41,80] 
              LET nombre = reg_tra_det_automatico.nombre_ent[81,120] 

                 UPDATE safre_tmp:tra_det_automatico 
                 SET   safre_tmp:tra_det_automatico.estado = 30
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                  INSERT INTO tra_mae_icefa
                  VALUES(reg_afi_mae_afiliado.n_folio          ,
                         reg_afi_mae_afiliado.tipo_solicitud   ,
                         reg_afi_mae_afiliado.n_seguro         ,
                         reg_tra_det_automatico.n_seguro_ent ,
                         reg_tra_det_automatico.rfc_ent      ,
                         paterno   ,
                         materno   ,
                         nombre    ,
                         "0",
                         reg_tra_det_automatico.cve_ced_cuenta ,
                         reg_tra_det_automatico.nro_ctrl_icefa ,
                         TODAY,
                         "",
                         "",
                         0                     ,#saldo_sar_92
                         0                     ,#saldo_viv_92
                         TODAY,
                         TODAY,
                         ""                     ,#lote_genera
                         ""                     ,#fecha_genera
                         1                      ,#status
                         3                      ,
                         0                      ,#correlativo
                         c8_usuario         ,
                         ""                     ,#n_envios
                         ""                      #diagnostico
                        )

                 LET reg_tra_det_automatico.estado = 30
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 LET cont_acep = cont_acep + 1
                 DISPLAY "REGISTROS ACEPTADOS...",cont_acep AT 15,2
                 CONTINUE FOREACH

                ELSE
                 LET reg_tra_det_automatico.estado = 32
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)

                 UPDATE safre_tmp:tra_det_automatico 
                 SET   safre_tmp:tra_det_automatico.estado = 32
                 WHERE safre_tmp:tra_det_automatico.rowid = v_row

                 LET cont_dup = cont_dup + 1
                 DISPLAY "REGISTROS DUPLICADOS EN MAESTRO...",cont_dup AT 17,2
                 CONTINUE FOREACH
                END IF
             END IF
######
                END IF

              ELSE
                 LET reg_tra_det_automatico.estado = 32
                 INSERT INTO paso_maestro VALUES(reg_tra_det_automatico.*)
                 UPDATE safre_tmp:tra_det_automatico 
                 SET    safre_tmp:tra_det_automatico.estado = 32
                 WHERE  safre_tmp:tra_det_automatico.rowid = v_row
                 LET cont_dup = cont_dup + 1
                 DISPLAY "REGISTROS DUPLICADOS EN MAESTRO...",cont_dup AT 17,2
                 CONTINUE FOREACH
                END IF
             END IF
           END IF
    END FOREACH

    IF cont = 0 THEN
       ERROR "NO HAY REGISTROS PARA TRASPASO A MAESTRO DE ICEFAS..."
       SLEEP 3
    END IF
   FINISH REPORT salida
END FUNCTION

FUNCTION genera_reporte()
#gr----------------------

  START REPORT r_1 TO RUTA  

  DECLARE cur_r CURSOR FOR
  SELECT a.cve_ced_cuenta,
         a.n_seguro_ent,
         a.rfc_ent,
         a.nro_ctrl_icefa,
         a.nombre_ent,
         a.fecha_nacimiento,
         a.marca_viv,
         a.marca_retiro,
         a.bimestres_acum,
         a.rfc_patronal,
         a.reg_patronal,
         a.nombre_patron,
         a.exp_infonavit,
         a.saldo_sar_92,
         a.saldo_viv_92,
         a.sar_92_issste,
         a.viv_92_issste,
         a.cve_afore,
         a.n_seguro,
         a.rfc,
         a.tipo_criterio,
         a.estado,
         a.fecha_edo,
         a.diagnostico,
         a.folio_interno,
         a.correlativo,
         a.cad_valida,
         a.liga_correlativo 
  FROM  paso_maestro a
  ORDER BY a.folio_interno,a.cve_ced_cuenta,a.tipo_criterio,a.estado


  FOREACH cur_r INTO reg_tra_det_automatico.*

      LET hay_regs_rpt1                      =                             1

   OUTPUT TO REPORT r_1(reg_tra_det_automatico.*)

      IF  ( hay_regs_rpt1               =                  1 ) THEN
         DISPLAY "REPORTE  1 GENERADO EN: ",RUTA CLIPPED AT 16,2
      END IF
      
  END FOREACH 
FINISH REPORT r_1
END FUNCTION
################################################################################

REPORT r_1(reg_tra_det_automatico)
#r1-------------------------------

DEFINE reg_tra_det_automatico RECORD LIKE safre_tmp:tra_det_automatico.*
DEFINE cve_desc CHAR(015)
    OUTPUT

        PAGE LENGTH 90
    FORMAT

    PAGE HEADER
    SELECT codigo_afore,razon_social
    INTO cod_afore,raz_social
    FROM tab_afore_local

       PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
       PRINT COLUMN 160,"Pagina:",PAGENO USING "<<<<"
       PRINT
            COLUMN 001,"____________________________________________________",
                       "____________________________________________________",
                       "____________________________________________________",
                       "_________________" 
       PRINT
       PRINT
            COLUMN 001,g_nom_prog,
            COLUMN 060,"TRASPASO AL MAESTRO DE SOLIC. ICEFA-AFORE IMSS",
            COLUMN 160,HOY USING"DD-MM-YYYY" 
       PRINT
             COLUMN 160,HORA
       PRINT COLUMN 001,cod_afore USING "&&&","  ",raz_social CLIPPED
       PRINT
 
       BEFORE GROUP OF reg_tra_det_automatico.estado
       PRINT
            COLUMN 001,"____________________________________________________",
                       "____________________________________________________",
                       "____________________________________________________",
                       "_________________" 
       PRINT COLUMN 001,"FOLIO: ",reg_tra_det_automatico.folio_interno 
             USING "#####",
             COLUMN 015,"BANCO: ",reg_tra_det_automatico.cve_ced_cuenta 
             USING "###",
             COLUMN 027,"TIPO CRITERIO: ",reg_tra_det_automatico.tipo_criterio 
             USING "#",
             COLUMN 046,"ESTADO: ",reg_tra_det_automatico.estado USING "##"
      PRINT COLUMN 001,"____________________________________________________",
                       "____________________________________________________",
                       "____________________________________________________",
                       "_________________" 
      PRINT COLUMN 001,"NSS"         ,
            COLUMN 013,"NSS ICEFA"   ,
            COLUMN 025,"RFC ICEFA"   ,
            COLUMN 040,"NRO.INTERNO" ,
            COLUMN 140,"ESTADO     "
      PRINT COLUMN 001,"NOMBRE"      
      PRINT COLUMN 001,"____________________________________________________",
                       "____________________________________________________",
                       "____________________________________________________",
                       "_________________" 
       PRINT
        ON EVERY ROW
            PRINT
                 COLUMN 001,reg_tra_det_automatico.n_seguro             ,
                 COLUMN 013,reg_tra_det_automatico.n_seguro_ent         ,
                 COLUMN 025,reg_tra_det_automatico.rfc_ent              ,
                 COLUMN 040,reg_tra_det_automatico.nro_ctrl_icefa       
           PRINT
                 COLUMN 001,reg_tra_det_automatico.nombre_ent[1,40]     ,
                 COLUMN 042,reg_tra_det_automatico.nombre_ent[41,80]    ,
                 COLUMN 084,reg_tra_det_automatico.nombre_ent[81,120]   ,
                 COLUMN 140,reg_tra_det_automatico.estado USING"&&"
           PRINT

       AFTER GROUP OF reg_tra_det_automatico.estado
            
          PRINT COLUMN 080,"TOTALES:",GROUP COUNT(*) USING "###,###,##&"

    ON LAST ROW
       PRINT
            COLUMN 001,"____________________________________________________",
                       "____________________________________________________",
                       "____________________________________________________",
                       "_________________" 


       PRINT  COLUMN 080,"TOTAL DE REGISTROS PROCESADOS :       ",
                         COUNT(*) USING"###,###,##&"
      
                                                       
END REPORT

FUNCTION formatea(g_reg)
#f-----------------------
DEFINE nro_ctrl_anterior         CHAR(030)
DEFINE g_ant                     RECORD LIKE safre_tmp:tra_det_automatico.*
DEFINE g_reg                     RECORD LIKE safre_tmp:tra_det_automatico.*,
       g_icefa                   LIKE safre_tmp:tra_icefa_ref.icefa,
       g_icefa_envio             LIKE safre_tmp:tra_icefa_ref.icefa_envio,
       g_formato_icefa           LIKE    safre_tmp:tra_icefa_ref.formato_icefa,
       g_tipo_formato            LIKE    safre_tmp:tra_icefa_ref.tipo_formato ,
       g_raz_social              LIKE    tab_afore_local.razon_social,
       g_cod_afore               LIKE    tab_afore_local.codigo_afore,
       g_formato                 CHAR(30),
       g_num_linea               INTEGER,
       si_formatea               SMALLINT,
       x                         SMALLINT,
       y                         SMALLINT

DEFINE  reg                    RECORD 
                               linea        CHAR(01)
                               END   RECORD

DEFINE  p_nro char(003)

    LET nro_ctrl_anterior = g_reg.nro_ctrl_icefa
    LET g_num_linea =  0
    LET si_formatea =  0

   ############ 
   #Inicia Rutina para Formaterar Numero de Control Icefa 
   ############

   SELECT a.icefa         ,
          a.icefa_envio   ,
          a.formato_icefa ,
          a.tipo_formato
   INTO   g_icefa         ,
          g_icefa_envio   ,
          g_formato_icefa ,
          g_tipo_formato 
   FROM safre_tmp:tra_icefa_ref a
   WHERE  a.icefa = reg_tra_det_automatico.cve_ced_cuenta

       CASE g_tipo_formato
        WHEN 1

            SELECT "OK" 
	    FROM   safre_tmp:tra_icefa_ref a
	    WHERE  a.formato_icefa = g_reg.nro_ctrl_icefa
	    AND    a.tipo_formato  = 1
	    GROUP BY 1

	    IF STATUS  <> NOTFOUND THEN
	       EXIT CASE
	    ELSE 

              LET si_formatea = 1

              LET p_nro[1,3] = g_reg.nro_ctrl_icefa[2,4]
              SELECT "OK" 
  	      FROM   safre_tmp:tra_icefa_ref a
  	      WHERE  a.formato_icefa[2,4] = p_nro
	      AND    a.tipo_formato  = 1
	      GROUP BY 1

              IF STATUS <> NOTFOUND THEN
                  LET g_formato[1] = "0"
		  LET g_formato[2,4] = p_nro
		  LET g_formato[5,8] = "0000"
                  LET g_formato[9,30] = " "
	          LET g_reg.nro_ctrl_icefa = g_formato
              ELSE 
               LET g_formato[1,8]  = g_formato_icefa
               LET g_formato[9,30] = " "
	       LET g_reg.nro_ctrl_icefa = g_formato
              END IF
            END IF
          EXIT CASE
        WHEN 2
            IF g_reg.nro_ctrl_icefa[1,18] = "000000000000000000" THEN
               FOR x = 19 TO 30
                 IF g_reg.nro_ctrl_icefa[x] = " "  THEN
                    LET si_formatea      =  1
                    EXIT FOR
                 END IF
               END FOR
            ELSE
               LET si_formatea = 1
            END IF
	       LET u = 1
            IF si_formatea THEN
		FOR k = 1 TO 30 
		   IF g_reg.nro_ctrl_icefa[k] <> " " THEN
                      LET v_format[u] = g_reg.nro_ctrl_icefa[k] 
			  LET u = u + 1
                   END IF
                END FOR

                LET g_formato[1,18]  =  "000000000000000000"
                LET g_formato[19,30] =  v_format[1,12] 
                                       USING "&&&&&&&&&&&&"
	        LET g_reg.nro_ctrl_icefa = g_formato
            END IF
        WHEN  3
            IF g_icefa = "071" THEN
              IF g_reg.nro_ctrl_icefa[1,24] = "                        " THEN
                 FOR x = 25 TO 30
                  IF g_reg.nro_ctrl_icefa[x] = " "  THEN
                     LET si_formatea = 1
                     EXIT  FOR
                  END IF
                 END FOR
               ELSE
                 LET si_formatea = 1
               END IF
               IF si_formatea THEN
                  LET g_formato[1,24]  = "                        "
                  LET g_formato[25,30] = g_reg.nro_ctrl_icefa[1,6]
                                         USING   "&&&&&&"
	          LET g_reg.nro_ctrl_icefa = g_formato
               END IF
             ELSE
               IF g_reg.nro_ctrl_icefa[10,30] = "                     " THEN
                  FOR x = 1 TO 9
                    IF g_reg.nro_ctrl_icefa[x] = " " THEN
                       LET si_formatea = 1
                       EXIT  FOR
                    END IF
                  END FOR
               ELSE
                  LET si_formatea = 1
               END IF
               IF si_formatea THEN
                  LET g_formato[1,9]   = g_reg.nro_ctrl_icefa[1,9]
                                         USING     "&&&&&&&&&"
                  LET g_formato[10,30] = "                     "
	          LET g_reg.nro_ctrl_icefa = g_formato
               END IF
             END IF

         WHEN 4
             IF g_icefa = "021" THEN
               IF g_reg.nro_ctrl_icefa[1,30] <> 
                  "                              "  THEN
                  LET g_formato[1,30] = "                              "
                  LET si_formatea = 1
	          LET g_reg.nro_ctrl_icefa = g_formato
               END IF
             ELSE
               IF g_reg.nro_ctrl_icefa[9,30] = "                      " THEN
                  FOR x = 1 TO 8
                    IF g_reg.nro_ctrl_icefa[x] = " " THEN
                      LET si_formatea =  1
                      EXIT  FOR
                    END IF
                  END FOR
               ELSE
                  LET si_formatea =  1
               END IF
             IF si_formatea                THEN
                LET g_formato[1,8] = g_reg.nro_ctrl_icefa[1,8]
                                     USING     "&&&&&&&&"
                LET g_formato[9,30]= " "
	        LET g_reg.nro_ctrl_icefa = g_formato
             END IF
           END IF
        WHEN  5
           IF g_reg.nro_ctrl_icefa[1,30] <> 
              "                              "  THEN
              LET g_formato[1,30] = " "
              LET si_formatea     =  1
	      LET g_reg.nro_ctrl_icefa = g_formato
           END IF
        WHEN  6
	    EXIT CASE
	    OTHERWISE 
	      EXIT CASE
        END CASE


   ##########   
   #Termina  Rutina   para   Formaterar   Numero de Control Icefa  ####
   ##########   

           IF si_formatea THEN

              LET g_ant.* = g_reg.*

	      LET g_ant.nro_ctrl_icefa = nro_ctrl_anterior

              INSERT INTO safre_tmp:tra_det_planchada VALUES(g_ant.*);

   UPDATE   safre_tmp:tra_det_automatico
   SET      safre_tmp:tra_det_automatico.cve_ced_cuenta      =  g_icefa_envio,
            safre_tmp:tra_det_automatico.nro_ctrl_icefa      =  g_formato 
   WHERE    safre_tmp:tra_det_automatico.n_seguro            =  g_reg.n_seguro
   AND safre_tmp:tra_det_automatico.n_seguro_ent        =  g_reg.n_seguro_ent
   AND safre_tmp:tra_det_automatico.cve_ced_cuenta      =  g_reg.cve_ced_cuenta 
   AND safre_tmp:tra_det_automatico.nro_ctrl_icefa      =  nro_ctrl_anterior


          LET hay_regs_rpt2               =                         1

       OUTPUT TO REPORT  salida(g_reg.*,g_icefa,g_icefa_envio,g_formato_icefa,
                         g_tipo_formato,g_raz_social,g_cod_afore,
                         g_formato,g_num_linea,si_formatea,x,y,reg.*,
			 nro_ctrl_anterior)
           END IF

           IF  ( hay_regs_rpt2               =                  1 ) THEN
              DISPLAY "REPORTE  1 GENERADO EN: ",RUTA_F CLIPPED AT 16,2
           END IF

    RETURN    g_reg.*

END FUNCTION


REPORT salida(g_reg,g_icefa,g_icefa_envio,g_formato_icefa,
g_tipo_formato,g_raz_social,g_cod_afore,g_formato,g_num_linea,
si_formatea,x,y,l_rep,nro_ctrl_anterior)
#s-----------------------------------------------------------

DEFINE g_reg             RECORD  LIKE    safre_tmp:tra_det_automatico.*,
       g_icefa           LIKE    safre_tmp:tra_icefa_ref.icefa,
       g_icefa_envio     LIKE    safre_tmp:tra_icefa_ref.icefa_envio,
       g_formato_icefa   LIKE    safre_tmp:tra_icefa_ref.formato_icefa,
       g_tipo_formato    LIKE    safre_tmp:tra_icefa_ref.tipo_formato ,
       g_raz_social      LIKE    tab_afore_local.razon_social,
       g_cod_afore       LIKE    tab_afore_local.codigo_afore,
       g_formato         CHAR(30),
       g_num_linea       INTEGER,
       si_formatea       SMALLINT,
       x                 SMALLINT,
       y                 SMALLINT

DEFINE    l_rep          RECORD 
          linea          CHAR(01)
                         END   RECORD

DEFINE    nro_ctrl_anterior      CHAR(030)
DEFINE    r_nombre               CHAR(60),
          l_coloca_espacio       SMALLINT

    OUTPUT
        TOP      MARGIN  1
        BOTTOM   MARGIN  0
        LEFT     MARGIN  0
        RIGHT    MARGIN  0
        PAGE     LENGTH  60

    FORMAT
        PAGE HEADER

        PRINT  '\033e\033(10U\033&l1O\033&k2S\033&l12d'      
        PRINT  COLUMN  152,"Pagina: ",PAGENO USING "<<<<"
        PRINT  COLUMN   65,"REPORTE  DE  ICEFAS  PLANCHADAS ",
               COLUMN  148,"fecha_proc: ",
               COLUMN  160,TODAY USING "dd-mm-yyyy"
        PRINT  COLUMN   65,"TRASPASOS ICEFA-AFORE IMSS  "
        PRINT  COLUMN   01, "_________________________________________________________________________________________________________________________________________________________________________"
        PRINT  COLUMN   01,g_cod_afore               USING  "&&&","  ",
                        g_raz_social    CLIPPED,
               COLUMN   65,"Folio:",g_reg.folio_interno USING  "#####",
               COLUMN   85,"Criterio:",g_reg.tipo_criterio     USING  "##"
        PRINT  COLUMN   01, "_________________________________________________________________________________________________________________________________________________________________________"

        PRINT  COLUMN   01,"LINEA",
               COLUMN   11,"N_SEGURO",
               COLUMN   24,"N_SEGURO_ENT",
               COLUMN   38,"N O M B R E ",
               COLUMN   85,"RFC_ENT",
               COLUMN  101,"ICEFA",
               COLUMN  108,"NUM_CONTROL_ICEFA",
               COLUMN  147,"SAR_92",
               COLUMN  164,"VIV_92"
        PRINT  COLUMN   01, "_________________________________________________________________________________________________________________________________________________________________________"

    ON EVERY ROW
        LET       g_num_linea             =  g_num_linea      +  1
        LET       r_nombre                =  " "
        LET       y                       =  0
        LET       l_coloca_espacio        =  0
   #####         Inicia   Rutina   para   Reducir   Nombre       #####

        FOR  x =  1 TO 120
              IF (g_reg.nombre_ent[x] <>  " ")     THEN
                      LET  y                =  y + 1
                      LET  l_coloca_espacio =  1
		      IF y = 0 THEN LET y = 1 END IF
		      IF x = 0 THEN LET x = 1 END IF
                      LET  r_nombre[y]      =  g_reg.nombre_ent[x]
                  ELSE
                   IF (y > 0 AND l_coloca_espacio) THEN
                        LET  y                   =  y   +  1
                        LET  l_coloca_espacio    =  0
                   END IF
              END IF
        END FOR
   #####         Termina  Rutina   para   Reducir   Nombre       #####

        PRINT  COLUMN   01,g_num_linea         USING  "#######",
               COLUMN   11,g_reg.n_seguro,
               COLUMN   24,g_reg.n_seguro_ent,
               COLUMN   38,r_nombre,
               COLUMN   85,g_reg.rfc_ent,
               COLUMN  101,g_reg.cve_ced_cuenta,
               COLUMN  108,nro_ctrl_anterior,
               COLUMN  141,g_reg.saldo_sar_92  USING   "#,###,###.&&",
               COLUMN  158,g_reg.saldo_viv_92  USING   "#,###,###.&&"

    ON LAST ROW
        SKIP 4 LINES
        PRINT  COLUMN    2,  "Total de registros enviados: ",
        COUNT(*) USING "<<<<"

    PAGE TRAILER
        SKIP 2 LINE
        #PAUSE "Presione enter para continuar...."

END REPORT                      
FUNCTION rev()
#r------------
DEFINE eje CHAR(100)

LET eje = "fglgo TRAM0099.4gi"
RUN eje

END FUNCTION

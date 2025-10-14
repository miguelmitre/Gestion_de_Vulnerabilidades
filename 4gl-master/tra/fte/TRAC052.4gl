##############################################################################
#Owner             => E.F.P.
#Programa TRAC052  => CONFRONTA AFORE-ICEFA (MASIVOS)         
#Fecha creacion    => 28 DE MARZO DE 2000
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 22 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af

GLOBALS
    DEFINE sw smallint

    DEFINE reg_nombre RECORD 
           paterno char(040),
           materno char(040),
           nombres char(040) 
                      END RECORD

    DEFINE v_rowid INTEGER
    DEFINE #glo #reg_2
        reg_2                 RECORD LIKE tra_det_automatico.*

    DEFINE reg_1 RECORD #glo #reg_1
        cve_ced_cuenta        SMALLINT
    END RECORD

    DEFINE reg_4 RECORD #glo #reg_4
        marcada               CHAR(003) ,
        enviada               CHAR(003) 
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        c3_cve_ced_cuenta     CHAR(3),
        RUTA                  CHAR(100) ,
        borra_lineas          CHAR(200) ,
        G_LISTA_1             CHAR(100) ,
        enter                 CHAR(001)

    DEFINE #glo #integer
        i_correlativo         ,
        ultimo_folio          INTEGER
    
    DEFINE g_usuario          CHAR(08)

    DEFINE g_nom_prog         CHAR(07)
   
    DEFINE  g_glob            RECORD
            codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
            razon_social      LIKE safre_af:tab_afore_local.razon_social
                              END RECORD
    DEFINE r1      CHAR(100)

    DEFINE r2      CHAR(100)

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST ,
    INPUT WRAP               ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL init() #i
    LET sw = 0
    OPEN WINDOW TRAC0521 AT 4,4 WITH FORM "TRAC0521" ATTRIBUTE(BORDER)

    DISPLAY "                             <Ctrl-c> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " TRAC052     CONFRONTA TRASPASO ICEFA-AFORE IMSS (MASIVOS)                     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    WHILE TRUE
        PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] : ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
             PROMPT "Proceso Cancelado...<ENTER> para Salir" for char enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

                SELECT "OK" 
                FROM   tra_det_automatico
                WHERE  estado         IN (10,14)
                GROUP BY 1

             IF STATUS = NOTFOUND THEN

               ERROR "NO EXISTEN REGISTROS PARA MANDAR A CONFRONTA..."
               SLEEP 4
               EXIT PROGRAM
             END IF


           CALL primer_paso()
           CALL genera_reporte()

              LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED," > ",
                                  RUTA CLIPPED,"/CM.",HOY USING"DDMMYYYY"

  DISPLAY "ARCHIVO PLANO PARA ENVIAR: ",RUTA CLIPPED,"/","SALIDA1"  AT 15,5 ATTRIBUTE (REVERSE)

  DISPLAY "ARCHIVO INTERNO PARA LA AFORE: ",r1  CLIPPED,"/",g_usuario CLIPPED,".EC.",HOY USING"YYYYMMDD"  AT 17,5 ATTRIBUTE (REVERSE)

  PROMPT " PROCESO FINALIZADO...PRESIONE <ENTER> PARA CONTINUAR" FOR CHAR enter
  RUN borra_lineas

   CLOSE WINDOW TRAC0521
END MAIN

FUNCTION init()
#i-------------
    LET HOY                               =                         TODAY
    LET g_nom_prog                        =                         "TRAC052"

    SELECT ruta_envio
    INTO   RUTA #ruta PARA ARCHIVO PLANO A ENVIAR A PROCESAR
    FROM   safre_af:seg_modulo
    WHERE modulo_cod = "tra"

    LET G_LISTA_1                          =            RUTA CLIPPED,"/SALIDA1"

    DELETE FROM safre_tmp:tra_tmp_aut_envio

    SELECT USER
    INTO g_usuario
    FROM safre_af:tab_afore_local

    SELECT codigo_afore,
           razon_social
    INTO g_glob.*
    FROM safre_af:tab_afore_local

    SELECT A.ruta_listados 
    INTO   r1 #ruta para ARCHIVO INTERNO PARA LA AFORE
    FROM   seg_modulo A
    WHERE  A.modulo_cod = "tra"

    LET r2 = r1 CLIPPED,"/",g_usuario CLIPPED,".EC.",HOY USING"YYYYMMDD"

END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE #loc #integer
        cont_1                INTEGER


    DECLARE cur_1 CURSOR FOR
    SELECT rowid,*
    FROM   tra_det_automatico
    WHERE  estado       IN (10,14)

    LET cont_1 = 0

    START REPORT listado_1 TO G_LISTA_1

        FOREACH cur_1 INTO v_rowid,reg_2.*

           SELECT paterno,
                  materno,
                  nombres
           INTO reg_nombre.*
           FROM safre_af:afi_mae_afiliado
           WHERE n_seguro = reg_2.n_seguro

              LET cont_1 = cont_1 + 1
              IF cont_1 > 50000 THEN
                 EXIT FOREACH
               END IF
               DISPLAY "TOTAL REGISTROS PROCESADOS  ",cont_1 AT 12,20

               OUTPUT TO REPORT listado_1(reg_2.*,reg_nombre.*)

                  UPDATE tra_det_automatico
                  SET    estado        = "20"
                  WHERE  rowid         = v_rowid

                  INSERT INTO safre_tmp:tra_tmp_aut_envio VALUES (reg_2.*)

        END FOREACH
    FINISH REPORT listado_1
END FUNCTION

REPORT listado_1(reg_2,reg_nombre)  
#l1--------------------
    DEFINE #loc #reg_2
        reg_2                 RECORD LIKE tra_det_automatico.* 

     DEFINE reg_nombre RECORD 
              paterno char(040),
              materno char(040),
              nombres char(040)
     END RECORD

    DEFINE #loc #char
        c9_icefa_receptora    CHAR(09) ,
        c15_saldo_viv_92      CHAR(15) ,
        c16_saldo_viv_92      CHAR(16) ,
        c15_saldo_sar_92      CHAR(15) ,
        c16_saldo_sar_92      CHAR(16)

    DEFINE #loc #integer
        nro_secuencia         INTEGER

    OUTPUT 
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
       #PAGE LENGTH 1

#ff
    FORMAT
    FIRST PAGE HEADER
        LET nro_secuencia = 0
     ON EVERY ROW

        LET nro_secuencia = nro_secuencia + 1
  
        PRINT
            COLUMN 001,"02",
            COLUMN 009,"90"                                 ,
            COLUMN 011,HOY USING"YYMMDD"                    ,
            COLUMN 017,"OO0005160"                          ,
            COLUMN 026,"000000"                             ,
            COLUMN 032,reg_2.cve_ced_cuenta                 ,
            COLUMN 052,reg_2.rfc_ent                        ,
            COLUMN 065,reg_2.n_seguro_ent                   ,
            COLUMN 076,reg_2.nro_ctrl_icefa                 ,
            COLUMN 106,reg_2.nombre_ent                     ,
            COLUMN 226,reg_2.rfc                            ,
            COLUMN 239,reg_2.n_seguro                       ,
            COLUMN 250,reg_nombre.paterno                   ,
            COLUMN 290,reg_nombre.materno                    ,
            COLUMN 330,reg_nombre.nombres                    ,
           # COLUMN 370,"000000000000000"                    ,
           # COLUMN 385,"000000000000000"                    ,
           # COLUMN 416,"00"                                 ,
            COLUMN 418,nro_secuencia      USING"&&&&&&&&&"   ,
            COLUMN 427,44 SPACES
END REPORT

FUNCTION genera_reporte()
#gr----------------------
    DEFINE reg_rep RECORD     
           cve_ced_cuenta LIKE tra_det_automatico.cve_ced_cuenta,
           tipo_criterio  LIKE tra_det_automatico.tipo_criterio,
           estado         LIKE tra_det_automatico.estado,
           total          INTEGER
    END RECORD


    START REPORT rpt_0 TO r2 

    DECLARE cur_rep CURSOR FOR 

     SELECT A.cve_ced_cuenta,
            A.tipo_criterio,
            A.estado        ,
            count(*)
     FROM  safre_tmp:tra_tmp_aut_envio A
     GROUP BY 1,2,3
     ORDER BY 1,2,3


    FOREACH cur_rep INTO reg_rep.*
      OUTPUT TO REPORT rpt_0 (reg_rep.*)
    END FOREACH
    FINISH REPORT rpt_0

END FUNCTION

REPORT rpt_0(reg_rep)
#l1------------------

    DEFINE i_cont_reg_total INTEGER
    DEFINE i_cont_reg_orig  INTEGER 

    DEFINE reg_rep RECORD     
           cve_ced_cuenta LIKE tra_det_automatico.cve_ced_cuenta,
           tipo_criterio  LIKE tra_det_automatico.tipo_criterio,
           estado         LIKE tra_det_automatico.estado,
           total          INTEGER
    END RECORD

    DEFINE reg_rep1       RECORD 
           cve_desc       CHAR(010),
           criterio_desc  CHAR(010),
           estado_desc    CHAR(010)
    END RECORD

    DEFINE reg_res ARRAY[15] OF RECORD
           cve_ced_cuenta LIKE tra_det_automatico.cve_ced_cuenta ,
           cve_desc       CHAR(010),
           total          INTEGER 
    END RECORD

    DEFINE j,f SMALLINT
 
    OUTPUT
        PAGE LENGTH 90

    FORMAT

    PAGE HEADER

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT
            COLUMN 001,HOY USING"DD-MM-YYYY" ,
            COLUMN 155,g_nom_prog CLIPPED
        PRINT
            COLUMN 050,"ENVIO DE CONFRONTA CRUCE SAR 92 TRASPASO ICEFA-AFORE", 
                       " IMSS",

            COLUMN 149,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED
       PRINT
       PRINT
            COLUMN 010,"ICEFA"         ,
            COLUMN 023,""              ,
            COLUMN 035,"CRITERIO"      ,
            COLUMN 050,""              ,
            COLUMN 059,"SITUACION"     ,
            COLUMN 140,"TOTAL"
       PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT

    BEFORE GROUP OF reg_rep.cve_ced_cuenta
      IF sw = 0 THEN
        LET i_cont_reg_total = 0
        LET f = 0
        LET j = 0
        LET sw = 1
      END IF
        LET i_cont_reg_orig = 0

    ON EVERY ROW
        LET i_cont_reg_total = i_cont_reg_total + reg_rep.total
        LET i_cont_reg_orig  = i_cont_reg_orig + reg_rep.total

        SELECT A.icefa_desc
        INTO  reg_rep1.cve_desc
        FROM   tab_icefa A
        WHERE A.icefa_cod = reg_rep.cve_ced_cuenta 

       SELECT A.criterio_desc
       INTO   reg_rep1.criterio_desc
       FROM   safre_af:tra_aut_criterio A
       WHERE  A.criterio_cod  = reg_rep.tipo_criterio

       SELECT A.estado_descripcion
       INTO   reg_rep1.estado_desc
       FROM   tra_aut_estado A
       WHERE  A.estado_cod = reg_rep.estado

        PRINT
          # COLUMN 001,i_cont_reg_total USING "&&&&&&"     ,
            COLUMN 010,reg_rep.cve_ced_cuenta USING"&&&"   ,
            COLUMN 023,reg_rep1.cve_desc                   ,
            COLUMN 035,reg_rep.tipo_criterio USING"&&"     ,
            COLUMN 050,reg_rep1.criterio_desc              ,
            COLUMN 059,reg_rep.estado        USING"&&"     ,
            COLUMN 065,reg_rep1.estado_desc                ,
            COLUMN 140,reg_rep.total
        PRINT
    
    AFTER GROUP OF reg_rep.cve_ced_cuenta
            LET f = f + 1
            LET reg_res[f].cve_ced_cuenta = reg_rep.cve_ced_cuenta
            LET reg_res[f].cve_desc       = reg_rep1.cve_desc
            LET reg_res[f].total          = i_cont_reg_orig
    ON LAST ROW
        PRINT	
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        SKIP 2 lines
        PRINT 
              COLUMN 001,"ICEFA",
              COLUMN 025,"TOTAL"  
        PRINT	
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 

        FOR j = 1 TO f
          PRINT COLUMN 001, reg_res[j].cve_ced_cuenta     ,
                COLUMN 006, reg_res[j].cve_desc           ,
                COLUMN 025, reg_res[j].total USING"&&&&&&"  
        END FOR
       
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS PROCESADOS :       ",
                       i_cont_reg_total USING"#########"
        PRINT      
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
END REPORT

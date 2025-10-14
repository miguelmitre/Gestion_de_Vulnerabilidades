##############################################################################
#Owner             => E.F.P.
#Programa TRAC054  => CARGA ARCH. RESP. CONFRONTA MASIVOS TRA-ICE IMSS
#Fecha creacion    => 22 DE NOVIEMBRE DE 2001
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 23 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af

GLOBALS
    DEFINE #loc #integer
        i_cont_reg_total      ,
        i_cont_reg_orig       INTEGER


    DEFINE                    HORA    CHAR(008)
    DEFINE                    RUTA    CHAR(200)
    DEFINE                    paterno CHAR(40)
    DEFINE                    materno CHAR(40)
    DEFINE                    nombre  CHAR(40)

    DEFINE c8_usuario char(008)

    DEFINE reg_2 RECORD LIKE tra_mae_icefa.*

    DEFINE reg_1_a RECORD #glo #reg_1
        n_folio               LIKE afi_mae_afiliado.n_folio        ,
        tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud ,
        n_seguro              LIKE afi_mae_afiliado.n_seguro       ,
        n_rfc                 LIKE afi_mae_afiliado.n_rfc          ,
        n_unico               LIKE afi_mae_afiliado.n_unico        ,
        fentcons              LIKE afi_mae_afiliado.fentcons      ,
        paterno               LIKE afi_mae_afiliado.paterno        ,
        materno               LIKE afi_mae_afiliado.materno        ,
        nombres               LIKE afi_mae_afiliado.nombres
    END RECORD       


     DEFINE txt1                   CHAR(150)
     DEFINE banco                  CHAR(015)
     DEFINE v_criterio             CHAR(015)
     DEFINE HOY                    DATE
     DEFINE v_folio_interno        INTEGER
     DEFINE reg_ruta               RECORD LIKE seg_modulo.*

     DEFINE criterio               CHAR(02) ,
            cuenta                          ,
            cuantos                INTEGER  ,
            ruta_archivo           CHAR(100),
            enter                  CHAR(001)

     DEFINE reg_1                  RECORD 
            nom_archivo            CHAR(020)
     END RECORD

     DEFINE reg_tra_det_automatico RECORD LIKE tra_det_automatico.*

     DEFINE g_usuario              CHAR(08)
  
     DEFINE g_nom_prog             CHAR(07)

     DEFINE g_glob                RECORD
            codigo_afore          LIKE safre_af:tab_afore_local.codigo_afore,
            razon_social          LIKE safre_af:tab_afore_local.razon_social
                                  END RECORD
     DEFINE hay_reg_rep           INTEGER 

END GLOBALS

MAIN
    DEFER INTERRUPT      
    OPTIONS
    ACCEPT KEY CONTROL-I ,
    INPUT WRAP           ,
    PROMPT LINE LAST

   CALL init()

   OPEN WINDOW trac0541  AT 4,4 WITH FORM "TRAC0541" ATTRIBUTE(BORDER)
   DISPLAY " TRAC054   CARGA ARCH. RESP. CONFRONTA MASIVOS TRA-ICE IMSS                    " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                              " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME reg_1.* WITHOUT DEFAULTS

      AFTER FIELD nom_archivo
	       IF reg_1.nom_archivo IS NULL THEN
	           ERROR "Campo NO puede ser NULO"
	           NEXT FIELD nom_archivo
	       END IF

      ON KEY (ESC)
         LET ruta_archivo = reg_ruta.ruta_rescate CLIPPED,"/",
                            reg_1.nom_archivo CLIPPED
         LET criterio      = "01"

         DISPLAY "PROCESANDO INFORMACION " AT 19,2

         LET txt1 = "cd ",reg_ruta.ruta_rescate CLIPPED,";",
                    "sed -e /^$/d ",reg_1.nom_archivo CLIPPED ," > ",
                    reg_ruta.ruta_rescate CLIPPED,"/PASO"
         RUN txt1

         LET txt1 = "cd ",reg_ruta.ruta_rescate CLIPPED,";mv PASO ",
                    ruta_archivo
         RUN txt1

         LOAD FROM ruta_archivo 
         INSERT INTO safre_tmp:sube_registro #Esta tblaconti registro char(500)

         DELETE FROM safre_tmp:sube_registro
         WHERE  registro is NULL

         SELECT COUNT(*)
         INTO   cuantos 
         FROM   safre_tmp:sube_registro

         IF cuantos = 0 OR 
            cuantos IS NULL THEN
             DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                      AT 19,2 ATTRIBUTE(REVERSE)
             SLEEP 3
             NEXT FIELD nom_archivo
         ELSE
             DISPLAY "REGISTROS A PROCESAR: ",cuantos AT 17,2 
             EXIT INPUT
         END IF

       ON KEY (INTERRUPT)
           PROMPT "PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
           EXIT PROGRAM
   END INPUT
   
   CALL  clasifica() 
   CALL  genera_reporte()

    PROMPT " PROCESO FINALIZADO <ENTER> PARA SALIR..." 
    FOR CHAR enter
END MAIN

FUNCTION init()   
#i-------------

    LET HORA                              =                           TIME
    LET HOY                               =                           TODAY
    LET cuantos                           =                           0
    LET cuenta                            =                           0
    LET g_nom_prog                        =                          "TRAC054"
    LET hay_reg_rep                       =                           0       

    DELETE   FROM  safre_tmp:sube_registro

    SELECT * 
    INTO   reg_ruta.*
    FROM   seg_modulo   
    WHERE  modulo_cod = "tra"

    SELECT USER
    INTO g_usuario
    FROM safre_af:tab_afore_local

    SELECT codigo_afore,
           razon_social
    INTO g_glob.*
    FROM safre_af:tab_afore_local

    LET RUTA = reg_ruta.ruta_listados CLIPPED,"/",g_usuario CLIPPED,".RCONF.",
               HOY USING"YYYYMMDD"

    CREATE temp TABLE temp_auto
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

FUNCTION clasifica()
#c------------------

    CALL criterio_1()

END FUNCTION


FUNCTION criterio_1()
#c1------------------

     CALL inserta()

END FUNCTION


FUNCTION inserta()
#ins--------------

     DEFINE      registro        CHAR(600)
     DEFINE      f_saldo_1       CHAR(009)
     DEFINE      f_saldo_2       CHAR(011)
     DEFINE      f_aport         CHAR(010)
     DEFINE      sw              SMALLINT

LET sw = 1

     DECLARE cur_inserta CURSOR FOR

     SELECT A.* 
     FROM   safre_tmp:sube_registro A

     FOREACH cur_inserta INTO registro

     LET reg_tra_det_automatico.cve_ced_cuenta    = registro[032,034]
     LET reg_tra_det_automatico.n_seguro_ent      = registro[065,075]
     LET reg_tra_det_automatico.rfc_ent           = registro[052,064]
     LET reg_tra_det_automatico.nro_ctrl_icefa    = registro[076,105]
     LET reg_tra_det_automatico.nombre_ent        = registro[106,225]
     LET reg_tra_det_automatico.n_seguro          = registro[239,249]
     LET f_saldo_1 = registro[370,382] , ".",registro[383,384]
     LET reg_tra_det_automatico.saldo_sar_92      = f_saldo_1
     LET f_saldo_1 = registro[385,397] , ".",registro[398,399]
     LET reg_tra_det_automatico.saldo_viv_92      = f_saldo_1
     LET reg_tra_det_automatico.diagnostico       = registro[416,417]
     CASE reg_tra_det_automatico.diagnostico
     WHEN "01"

        LET reg_tra_det_automatico.estado = 22
        UPDATE tra_det_automatico
        SET    estado = 22 ,
               diagnostico  = reg_tra_det_automatico.diagnostico,
               saldo_sar_92 = reg_tra_det_automatico.saldo_sar_92,
               saldo_viv_92 = reg_tra_det_automatico.saldo_viv_92
        WHERE  n_seguro       = reg_tra_det_automatico.n_seguro
        AND    n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
        AND    rfc_ent        = reg_tra_det_automatico.rfc_ent
        AND    cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta
        AND    nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
        AND    nombre_ent     = reg_tra_det_automatico.nombre_ent
        AND    estado         = 20

          INSERT INTO temp_auto VALUES(reg_tra_det_automatico.*)
        EXIT CASE
     OTHERWISE
        UPDATE tra_det_automatico
        SET    estado = 24,
               diagnostico = reg_tra_det_automatico.diagnostico
        WHERE  n_seguro       = reg_tra_det_automatico.n_seguro
        AND    n_seguro_ent   = reg_tra_det_automatico.n_seguro_ent
        AND    rfc_ent        = reg_tra_det_automatico.rfc_ent
        AND    cve_ced_cuenta = reg_tra_det_automatico.cve_ced_cuenta
        AND    nro_ctrl_icefa = reg_tra_det_automatico.nro_ctrl_icefa
        AND    nombre_ent     = reg_tra_det_automatico.nombre_ent
        AND    estado         = 20
            LET reg_tra_det_automatico.estado = 24
            INSERT INTO temp_auto VALUES(reg_tra_det_automatico.*)
        EXIT CASE
    END CASE

    LET cuenta = cuenta + 1 
    DISPLAY "REGISTROS PROCESADOS: ",cuenta AT 18,2
   END FOREACH
END FUNCTION


FUNCTION genera_reporte()
#gr----------------------



  START REPORT r_1 TO RUTA  

  DECLARE cur_r CURSOR FOR
  SELECT * 
  FROM  temp_auto
  ORDER BY estado,cve_ced_cuenta,n_seguro


  FOREACH cur_r INTO reg_tra_det_automatico.*

      LET hay_reg_rep                   =                  1

   OUTPUT TO REPORT r_1(reg_tra_det_automatico.*)
  END FOREACH 

      IF  ( hay_reg_rep                 =                  1 ) THEN
         DISPLAY "REPORTE GENERADO EN: ",RUTA CLIPPED AT 16,2
      END IF 

FINISH REPORT r_1
END FUNCTION

REPORT r_1(reg_tra_det_automatico)
#l1--------------------------------------

DEFINE reg_tra_det_automatico RECORD LIKE tra_det_automatico.*
DEFINE ed                     CHAR(020)

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
            COLUMN 155,g_nom_prog
        PRINT
            COLUMN 001,HORA,
            COLUMN 050,"RESPUESTA DE CONFRONTA CRUCE SAR 92 TRASPASO ",
                       "ICE-AFO IMSS",

            COLUMN 149,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED
       PRINT
       PRINT
            COLUMN 001,"NSS"         ,
            COLUMN 013,"NSS ICEFA"   ,
            COLUMN 025,"RFC ICEFA"   ,
            COLUMN 040,"BANCO"       ,
            COLUMN 049,"NRO.INTERNO" ,
            COLUMN 140,"DIAGNOSTICO"
            PRINT
            COLUMN 001,"NOMBRE"      
       PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT

    BEFORE GROUP OF reg_tra_det_automatico.estado
        SELECT estado_descripcion
        INTO ed
        FROM tra_aut_estado
        WHERE estado_cod = reg_tra_det_automatico.estado
        LET i_cont_reg_orig = 0
        PRINT
        PRINT
       PRINT
            COLUMN 001,"----------------------------------------------------",
                       "----------------------------------------------------",
                       "----------------------------------------------------",
                       "-----------------" 
        PRINT
        PRINT
            COLUMN 001,"ULTIMO ESTADO : ", ed
                       
        PRINT
        PRINT
    ON EVERY ROW
        LET i_cont_reg_orig  = i_cont_reg_orig + 1
        LET i_cont_reg_total = i_cont_reg_total +1
        PRINT
            COLUMN 001,reg_tra_det_automatico.n_seguro             ,
            COLUMN 013,reg_tra_det_automatico.n_seguro_ent         ,
            COLUMN 025,reg_tra_det_automatico.rfc_ent              ,
            COLUMN 040,reg_tra_det_automatico.cve_ced_cuenta       ,
            COLUMN 049,reg_tra_det_automatico.nro_ctrl_icefa       
            PRINT
            COLUMN 001,reg_tra_det_automatico.nombre_ent[1,40]     ,
            COLUMN 042,reg_tra_det_automatico.nombre_ent[41,80]    ,
            COLUMN 084,reg_tra_det_automatico.nombre_ent[81,120]   ,
            COLUMN 140,reg_tra_det_automatico.diagnostico USING"&&"
        PRINT
            
    AFTER GROUP OF reg_tra_det_automatico.estado
        PRINT
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS :",ed,
                       " :          ",i_cont_reg_orig USING"#########"
        PRINT 	

    ON LAST ROW
        PRINT	
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS PROCESADOS :       ",
                       i_cont_reg_total USING"#########"
        PRINT      
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
      
                                                       
END REPORT

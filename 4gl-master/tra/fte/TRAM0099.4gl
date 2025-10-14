##############################################################################
#Owner             => E.F.P.
#Programa TRAM0099 => MANTENIMIENTO DE ICEFAS(Con Reverso)   
#Fecha creacion    => 10 ABRIL 2003        
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 23 DE FEBRERO DEL 2005 
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af

GLOBALS
DEFINE    txt_2                   CHAR(200)
DEFINE    x_busca                 CHAR(200)
DEFINE    txt_1                   CHAR(2000)
DEFINE    v_estado                CHAR(002)
DEFINE    v_estado_descripcion    CHAR(100)

DEFINE    v                       RECORD
          icefa_cod               CHAR(03),
          icefa_desc              CHAR(24)
                                  END  RECORD

DEFINE    arr        ARRAY[1000]  OF  RECORD 
          x                  CHAR(01) ,
          cve_ced_cuenta     LIKE safre_tmp:tra_det_automatico.cve_ced_cuenta  ,
          icefa_desc         LIKE safre_af:tab_icefa.icefa_desc     ,
          n_folio            LIKE afi_mae_afiliado.n_folio        ,
          tipo_solicitud     LIKE afi_mae_afiliado.tipo_solicitud ,
          n_seguro           LIKE afi_mae_afiliado.n_seguro       ,
          n_rfc              LIKE afi_mae_afiliado.n_rfc          ,
          n_unico            LIKE afi_mae_afiliado.n_unico        ,
          fentcons           LIKE afi_mae_afiliado.fentcons       ,
          paterno            LIKE afi_mae_afiliado.paterno        ,
          materno            LIKE afi_mae_afiliado.materno        ,
          nombres            LIKE afi_mae_afiliado.nombres        ,
          estado             LIKE safre_tmp:tra_det_automatico.estado  ,
          estado_descripcion LIKE safre_tmp:tra_aut_estado.estado_descripcion, 
          n_seguro_ent       LIKE safre_tmp:tra_det_automatico.n_seguro_ent ,
          rfc_ent            LIKE safre_tmp:tra_det_automatico.rfc_ent  ,
          nro_ctrl_icefa     LIKE safre_tmp:tra_det_automatico.nro_ctrl_icefa ,
          paterno2           CHAR(40),
          materno2           CHAR(40),
          nombres2           CHAR(40),
          saldo_sar_92       LIKE safre_tmp:tra_det_automatico.saldo_sar_92 ,
          saldo_viv_92       LIKE safre_tmp:tra_det_automatico.saldo_viv_92,
          fuente             CHAR(4),
          correlativo        INTEGER,
          registro           INTEGER
                                  END  RECORD

DEFINE    arr1               RECORD 
          x                  CHAR(01) ,
          cve_ced_cuenta     LIKE safre_tmp:tra_det_automatico.cve_ced_cuenta ,
          icefa_desc         LIKE  safre_af:tab_icefa.icefa_desc ,
          n_folio            LIKE  afi_mae_afiliado.n_folio     ,
          tipo_solicitud     LIKE  afi_mae_afiliado.tipo_solicitud ,
          n_seguro           LIKE  afi_mae_afiliado.n_seguro       ,
          n_rfc              LIKE  afi_mae_afiliado.n_rfc          ,
          n_unico            LIKE  afi_mae_afiliado.n_unico        ,
          fentcons           LIKE  afi_mae_afiliado.fentcons       ,
          paterno            LIKE  afi_mae_afiliado.paterno        ,
          materno            LIKE  afi_mae_afiliado.materno        ,
          nombres            LIKE  afi_mae_afiliado.nombres        ,
          estado             LIKE  safre_tmp:tra_det_automatico.estado,
          estado_descripcion LIKE  safre_tmp:tra_aut_estado.estado_descripcion,
          n_seguro_ent       LIKE  safre_tmp:tra_det_automatico.n_seguro_ent,
          rfc_ent            LIKE  safre_tmp:tra_det_automatico.rfc_ent,
          nro_ctrl_icefa     LIKE  safre_tmp:tra_det_automatico.nro_ctrl_icefa,
          paterno2           CHAR(40),
          materno2           CHAR(40),
          nombres2           CHAR(40),
          saldo_sar_92       LIKE  safre_tmp:tra_det_automatico.saldo_sar_92,
          saldo_viv_92       LIKE  safre_tmp:tra_det_automatico.saldo_viv_92,
          fuente             CHAR(4),
          correlativo        INTEGER,
          registro           INTEGER
                             END  RECORD

DEFINE   paso_tram0099       RECORD   
         n_seguro            CHAR(11)    ,
         n_seguro_ent        CHAR(11)    ,
         nombre              CHAR(120)   ,
         rfc_ent             CHAR(18)    ,
         icefa_cod           CHAR(03)    ,
         nro_ctrl_icefa      CHAR(30)    
                             END  RECORD

DEFINE   n_registro          INTEGER,
         i                   SMALLINT,
         fin_icefa           SMALLINT,
         t_registro          INTEGER,
         g_usuario           CHAR(08),
         g_num_linea         INTEGER ,
         g_raz_social        LIKE    tab_afore_local.razon_social ,
         g_cod_afore         LIKE    tab_afore_local.codigo_afore ,
         g_param_tra         RECORD  LIKE seg_modulo.* ,
         g_ruta              CHAR(100) ,
         enter               CHAR(01),
         HOY                 DATE

DEFINE   reg                 RECORD
         linea               CHAR(01)
                             END  RECORD
DEFINE   g_genera_reporte    SMALLINT
DEFINE   g_nom_prog          CHAR(08) 
DEFINE   hay_regs            INTEGER
END GLOBALS

MAIN
    OPTIONS PROMPT LINE LAST ,
    INPUT WRAP               
    DEFER INTERRUPT

    #####
    UPDATE tra_mae_icefa
    SET icefa_cod = 2
    WHERE tra_mae_icefa.fecha_captura = today
    AND tra_mae_icefa.icefa_cod IS NULL
    ####

    CALL f_001_inicio()
    CALL f_010_proceso()

    IF   g_genera_reporte     THEN
         LET  hay_regs                        =                               0
         CALL f_100_genera_reporte()
    END IF

    DISPLAY  "                                                           " 
    AT    18,1

    PROMPT  "   PROCESO CONCLUIDO TECLEE <Enter> PARA SALIR.. " ATTRIBUTE(REVERSE,BLINK)
    FOR CHAR  enter

   #CLOSE     WINDOW   TRAM0099

END MAIN

FUNCTION  f_001_inicio()
#f001-------------------


    WHENEVER  ERROR CONTINUE
       DROP TABLE paso_tram0099
    WHENEVER ERROR STOP

    CREATE TEMP TABLE paso_tram0099
                 (
                 n_seguro             CHAR(11)    ,
                 n_seguro_ent         CHAR(11)    ,
                 nombre               CHAR(120)   ,
                 rfc_ent              CHAR(18)    ,
                 icefa_cod            CHAR(03)    ,
                 nro_ctrl_icefa       CHAR(30)    
                 )

    LET HOY =  TODAY

    OPEN WINDOW TRAM0099 AT 2,2 WITH FORM "TRAM0099" ATTRIBUTE( BORDER)
    DISPLAY " TRAM099             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [Ctrl-c]Salir [Esc]Bloques de 100 Regs. [F2]Inserta  "
            AT 1,1 ATTRIBUTE(BOLD)
 
    DISPLAY "                   DATOS DEL REGISTRO DE ICEFA-AFORE IMSS                      " AT 10,1 ATTRIBUTE(REVERSE)
    DISPLAY " CONSULTA  Y  GRABA " AT 1,55 ATTRIBUTE(REVERSE)
 
    CALL f_002_inicializa()

END FUNCTION

FUNCTION  f_002_inicializa()
#f002-----------------------

    INITIALIZE arr TO NULL

END FUNCTION

FUNCTION  f_010_proceso()
#f10---------------------

    DEFINE   busca     CHAR(100)
    DEFINE   l_icefa   CHAR(03)

    WHILE   TRUE
    CONSTRUCT BY NAME x_busca ON B.n_seguro,
			         B.cve_ced_cuenta
	 ON KEY (ESC )
	         LET INT_FLAG = FALSE
		 EXIT CONSTRUCT
	 ON KEY (INTERRUPT )
	         LET INT_FLAG = FALSE
	         RETURN
    END CONSTRUCT

    CALL f_011_trata_icefa()
       LET INT_FLAG                      =                                FALSE
       LET  g_genera_reporte             =                                1
       CALL f_002_inicializa()
    END  WHILE
END FUNCTION

FUNCTION  f_011_trata_icefa()
#f011------------------------

LET txt_1 = 
    ' SELECT  " "                     ,',
    ' B.cve_ced_cuenta        ,',
    ' " "                     ,',
    ' A.n_folio               ,',
    ' A.tipo_solicitud        ,',
    ' A.n_seguro              ,',
    ' A.n_rfc                 ,',
    ' A.n_unico               ,',
    ' A.fentcons              ,',
    ' A.paterno   [1,25]      ,',
    ' A.materno   [1,25]      ,',
    ' A.nombres   [1,25]      ,',
    ' B.estado                ,',
    ' " "                     ,',
    ' B.n_seguro_ent          ,',
    ' B.rfc_ent               ,',
    ' B.nro_ctrl_icefa        ,',
    ' B.nombre_ent [1,40]     ,',
    ' B.nombre_ent [41,80]    ,',
    ' B.nombre_ent [81,120]   ,',
    ' B.saldo_sar_92          ,',
    ' B.saldo_viv_92          ,',
    ' "3"                     ,',
    ' B.correlativo           ,',
    '   0',
    ' FROM    afi_mae_afiliado A , safre_tmp:tra_det_automatico  B ',
    ' WHERE   ',x_busca CLIPPED,
    ' AND     A.n_seguro          =  B.n_seguro  ',
    ' AND     B.estado            =  36 ',
    ' ORDER   BY 3 '

LET txt_1 = txt_1 CLIPPED
    PREPARE cur1 FROM txt_1
    DECLARE c_1_f_11 CURSOR FOR  cur1

LET txt_2 = 
 ' SELECT COUNT(*) ' ,
 ' FROM safre_tmp:tra_det_automatico B',
 ' WHERE ',x_busca CLIPPED ,
 ' AND B.estado = 36 '
PREPARE cur_prueba FROM txt_2
DECLARE cur_pp CURSOR FOR cur_prueba
FOREACH cur_pp INTO t_registro
END FOREACH

DISPLAY BY NAME t_registro

     LET       n_registro             =  1
     LET fin_icefa =  0
     LET i         =  1

     LET fin_icefa =  0
     FOREACH c_1_f_11 INTO arr[i].*,g_usuario
          SELECT estado_descripcion
          INTO   arr[i].estado_descripcion
          FROM   safre_tmp:tra_aut_estado
          WHERE  estado_cod = arr[i].estado

	  SELECT    A.icefa_desc
	    INTO      arr[i].icefa_desc
	    FROM      tab_icefa   A
	   WHERE     A.icefa_cod = arr[i].cve_ced_cuenta

          IF fin_icefa THEN
             EXIT    FOREACH 
          ELSE  
            LET arr[i].registro = n_registro
            IF i = 100 THEN
               CALL f_013_display_array()
               LET i =  0
            ELSE 
              IF i = t_registro THEN
               CALL f_013_display_array()
               LET i =  0
              END IF
            END IF 
    
            LET n_registro =  n_registro +  1
            LET i          =  i          +  1
          END IF
     END FOREACH

     INITIALIZE v.* TO  NULL
     CALL f_002_inicializa()
     CLEAR     FORM
END FUNCTION

FUNCTION  f_013_display_array()
#f013--------------------------


     CALL SET_COUNT(i)

     DISPLAY ARRAY arr TO  scr_1.*
          ON   KEY      (ESC)
               CALL      f_002_inicializa()
               EXIT      DISPLAY
          ON   KEY      (INTERRUPT)
               CALL      f_002_inicializa()
               LET       fin_icefa    =  1
               CLEAR     FORM
               EXIT      DISPLAY
          ON   KEY      (F2)
               LET       i            =  ARR_CURR()
               CALL      f_014_insert()
               DISPLAY BY NAME arr[i].estado
               DISPLAY BY NAME arr[i].estado_descripcion
     END DISPLAY
END FUNCTION

FUNCTION  f_014_insert()
#f014-------------------

    DEFINE    ya_existe          SMALLINT,
              l_nombre_120       CHAR(120)

    LET ya_existe =  0
    LET enter     =  "X"

    LET ya_existe = 0

    SELECT  COUNT(*)
    INTO    ya_existe
    FROM    tra_mae_icefa a
    WHERE   a.n_seguro           =  arr[i].n_seguro
    AND     a.nss                =  arr[i].n_seguro_ent
    AND     a.rfc                =  arr[i].rfc_ent
    AND     a.icefa_cod          =  arr[i].cve_ced_cuenta
    AND     a.nro_int_cta        =  arr[i].nro_ctrl_icefa
    AND     a.fuente             =  3

    IF ya_existe IS NULL THEN
       LET ya_existe = 0
    END IF

    IF ya_existe > 0 THEN

              LET arr[i].estado = 40 

           UPDATE safre_tmp:tra_det_automatico
           SET    estado = 40
          WHERE  safre_tmp:tra_det_automatico.correlativo = arr[i].correlativo

              SELECT estado_descripcion 
              INTO   arr[i].estado_descripcion
              FROM   safre_tmp:tra_aut_estado
              WHERE  estado_cod = arr[i].estado

              ERROR "  YA  HAY  REGISTROS  ",ya_existe  USING "###"
     ELSE
######

      CALL formatea(arr[i].*)
      RETURNING arr1.*

    LET ya_existe = 0

    SELECT  COUNT(*)
    INTO    ya_existe
    FROM    tra_mae_icefa a
    WHERE   a.n_seguro           =  arr1.n_seguro
    AND     a.nss                =  arr1.n_seguro_ent
    AND     a.rfc                =  arr1.rfc_ent
    AND     a.icefa_cod          =  arr1.cve_ced_cuenta
    AND     a.nro_int_cta        =  arr1.nro_ctrl_icefa
    AND     a.fuente             =  3

    IF ya_existe IS NULL THEN
       LET ya_existe = 0
    END IF

    IF ya_existe > 0 THEN

              LET arr[i].estado = 40 

           UPDATE safre_tmp:tra_det_automatico
           SET    estado = 40
          WHERE  safre_tmp:tra_det_automatico.correlativo = arr[i].correlativo

              SELECT estado_descripcion 
              INTO   arr[i].estado_descripcion
              FROM   safre_tmp:tra_aut_estado
              WHERE  estado_cod = arr[i].estado

              ERROR "  YA  HAY  REGISTROS  ",ya_existe  USING "###"
     ELSE
########
       WHILE TRUE
        PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
         IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN

              INSERT INTO tra_mae_icefa
                        VALUES(arr[i].n_folio          ,
                               arr[i].tipo_solicitud   ,
                               arr[i].n_seguro         ,
                               arr[i].n_seguro_ent     ,
                               arr[i].rfc_ent         ,
                               arr[i].paterno2         ,
                               arr[i].materno2         ,
                               arr[i].nombres2         ,
                               "0",
                               arr[i].cve_ced_cuenta ,
                               arr1.nro_ctrl_icefa ,
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
                               g_usuario              ,
                               ""                     ,#n_envios
                               ""                      #diagnostico
                               )

                      LET l_nombre_120 [01,40]  =  arr[i].paterno2
                      LET l_nombre_120 [41,80]  =  arr[i].materno2
                      LET l_nombre_120 [81,120] =  arr[i].nombres2

               INSERT INTO paso_tram0099
                      VALUES  (
                               arr[i].n_seguro         ,
                               arr[i].n_seguro_ent     ,
                               l_nombre_120            ,
                               arr[i].rfc_ent          ,
                               arr[i].cve_ced_cuenta   ,
                               arr1.nro_ctrl_icefa
                               )

               LET  arr[i].estado = 38

               UPDATE safre_tmp:tra_det_automatico 
               SET    estado = 38
               WHERE  correlativo = arr[i].correlativo

               ERROR   "  REG. INSERTADO AL MAESTRO DE ICEFAS "
	       SLEEP 2
        
               EXIT WHILE
             ELSE
                EXIT WHILE
            END IF
         END IF
       END WHILE
       END IF
     END IF
END FUNCTION

FUNCTION  f_100_genera_reporte()
#f100---------------------------

   CLEAR     FORM
   DISPLAY    "                                                                               " AT 10,1 ATTRIBUTE(REVERSE)


   SELECT    *
   INTO    g_param_tra.*
   FROM    seg_modulo
   WHERE    modulo_cod            =  "tra"

   SELECT  codigo_afore,razon_social
   INTO    g_cod_afore,g_raz_social
   FROM    tab_afore_local

   LET       g_nom_prog   =   "TRAM0099"
   LET       g_ruta       =   g_param_tra.ruta_listados CLIPPED ,"/ACEPDEV.",
                              HOY USING"YYYYMMDD"

   START     REPORT     salida     TO  g_ruta

   DECLARE   C_0            CURSOR   FOR
   SELECT    *
   FROM    paso_tram0099
   ORDER    BY  5,1
    
   FOREACH   C_0              INTO paso_tram0099.*
      LET   hay_regs                        =                                 1 
      OUTPUT          TO   REPORT  salida(reg.*)
   END FOREACH
   FINISH  REPORT    salida

   IF ( hay_regs                            =                           1) THEN
      DISPLAY    "   REPORTE GENERADO (RUTA):   ",g_ruta CLIPPED AT  19,1     ATTRIBUTE(REVERSE)
   END IF

END FUNCTION

REPORT salida(l_rep)
    DEFINE    l_rep                         RECORD
              linea                         CHAR(01)
                                      END   RECORD,
              r_nombre                      CHAR(45),
              l_coloca_espacio              SMALLINT,
              x                             SMALLINT,
              y                             SMALLINT

    OUTPUT
        TOP      MARGIN  1
        BOTTOM   MARGIN  0
        LEFT     MARGIN  0
        RIGHT    MARGIN  0
        PAGE     LENGTH  60

    FORMAT
        PAGE HEADER

      # PRINT   '\033e\033(10U\033&l1O\033&k2S\033&l12d'      
        PRINT    COLUMN   1,"\033e\033(0U\033&k2S\033&l12d"
        PRINT    COLUMN   01,g_nom_prog CLIPPED,
                 COLUMN  125,"Pagina: ",PAGENO USING "<<<<"
        PRINT    COLUMN   45,"REPORTE DE TRANSFERIDAS TRASPASO ICEFA-AFORE IMSS",
                 COLUMN  110,"FECHA PROCESO: ",
                 COLUMN  125,TODAY USING "dd-mm-yyyy"
        PRINT    COLUMN   01, "________________________________________________________________________________________________________________________________________"
        PRINT    COLUMN   01,g_cod_afore  USING  "&&&","  ",
                             g_raz_social CLIPPED
        PRINT    COLUMN   01, "________________________________________________________________________________________________________________________________________"

        PRINT    COLUMN    1,"LINEA",
                 COLUMN    7,"N_SEGURO",
                 COLUMN   20,"N_SEGURO_ENT",
                 COLUMN   33,"N O M B R E ",
                 COLUMN   80,"RFC_ENT",
                 COLUMN   99,"ICEFA",
                 COLUMN  106,"NUM_CONTROL_ICEFA"
        PRINT    COLUMN   01, "________________________________________________________________________________________________________________________________________"

    ON EVERY ROW
        LET       g_num_linea             =  g_num_linea      +  1
        LET       r_nombre                =  " "
        LET       y                       =  0
        LET       l_coloca_espacio        =  0
   #####          Inicia   Rutina   para   Localizar  Nombre       #####
        FOR       x                       =  1          TO  120
                  IF        paso_tram0099.nombre[x]         <>  " "     THEN
                            LET      y                   =  y   +  1
                            LET      l_coloca_espacio    =  1
                            LET      r_nombre   [y]      = 
                                     paso_tram0099.nombre[x]
                  ELSE
                  IF        y                            >  0   AND
                            l_coloca_espacio                    THEN
                            LET      y                   =  y   +  1
                            LET      l_coloca_espacio    =  0
                  END IF
                  END IF
        END FOR
   #####          Termina  Rutina   para   Localizar  Nombre       #####

        PRINT  COLUMN   01,g_num_linea         USING  "#####",
               COLUMN   07,paso_tram0099.n_seguro,
               COLUMN   20,paso_tram0099.n_seguro_ent,
               COLUMN   33,r_nombre,
               COLUMN   80,paso_tram0099.rfc_ent,
               COLUMN   99,paso_tram0099.icefa_cod,
               COLUMN  106,paso_tram0099.nro_ctrl_icefa

    ON LAST ROW
        SKIP      4    LINES
        PRINT     COLUMN    2,  "Total de registros enviados: ",
        COUNT    (*)   USING  "<<<<"

    PAGE TRAILER
        SKIP      2    LINE
        PAUSE    "Presione enter para continuar...."

END REPORT                      

FUNCTION formatea(g_reg)
#f-----------------------

    DEFINE    g_reg       RECORD 
      x                   CHAR(01) ,
      cve_ced_cuenta      LIKE safre_tmp:tra_det_automatico.cve_ced_cuenta  ,
      icefa_desc          LIKE  safre_af:tab_icefa.icefa_desc     ,
      n_folio             LIKE  afi_mae_afiliado.n_folio        ,
      tipo_solicitud      LIKE  afi_mae_afiliado.tipo_solicitud ,
      n_seguro            LIKE  afi_mae_afiliado.n_seguro       ,
      n_rfc               LIKE  afi_mae_afiliado.n_rfc          ,
      n_unico             LIKE  afi_mae_afiliado.n_unico        ,
      fentcons            LIKE  afi_mae_afiliado.fentcons       ,
      paterno             LIKE  afi_mae_afiliado.paterno        ,
      materno             LIKE  afi_mae_afiliado.materno        ,
      nombres             LIKE  afi_mae_afiliado.nombres        ,
      estado              LIKE  safre_tmp:tra_det_automatico.estado       ,
      estado_descripcion  LIKE  safre_tmp:tra_aut_estado.estado_descripcion, 
      n_seguro_ent        LIKE  safre_tmp:tra_det_automatico.n_seguro_ent ,
      rfc_ent             LIKE  safre_tmp:tra_det_automatico.rfc_ent          ,
      nro_ctrl_icefa      LIKE  safre_tmp:tra_det_automatico.nro_ctrl_icefa   ,
      paterno2            CHAR(40),
      materno2            CHAR(40),
      nombres2            CHAR(40),
      saldo_sar_92        LIKE  safre_tmp:tra_det_automatico.saldo_sar_92     ,
      saldo_viv_92        LIKE  safre_tmp:tra_det_automatico.saldo_viv_92,
      fuente              CHAR(4),
      correlativo         INTEGER,
      registro            INTEGER
                          END  RECORD

DEFINE nro_ctrl_anterior  CHAR(030)
DEFINE g_ant              RECORD LIKE safre_tmp:tra_det_automatico.*
DEFINE u                  SMALLINT
DEFINE k                  SMALLINT
DEFINE v_format           CHAR(030)

DEFINE 
      g_icefa             LIKE    safre_tmp:tra_icefa_ref.icefa,
      g_icefa_envio       LIKE    safre_tmp:tra_icefa_ref.icefa_envio,
      g_formato_icefa     LIKE    safre_tmp:tra_icefa_ref.formato_icefa,
      g_tipo_formato      LIKE    safre_tmp:tra_icefa_ref.tipo_formato ,
      g_raz_social        LIKE    tab_afore_local.razon_social,
      g_cod_afore         LIKE    tab_afore_local.codigo_afore,
      g_formato           CHAR(30),
      g_num_linea         INTEGER,
      si_formatea         SMALLINT,
      x                   SMALLINT,
      y                   SMALLINT

DEFINE  reg               RECORD 
        linea             CHAR(01)
                          END   RECORD

DEFINE p_nro              CHAR(003)

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
   WHERE  a.icefa = g_reg.cve_ced_cuenta

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
           END IF

    RETURN    g_reg.*

END FUNCTION

###############################################################################	
#Proyecto	       => SISTEMA DE SAFRE ( MEXICO )
#Owner                 => E.F.P
#                  ** V E R S I O N      E S P E C I A L **
#Programa  ESTB099     => ESTE ARCHIVO CONTIENE LA INFORMACION DE LOS MOV.
#                         DIARIOS QUE SE REALIZAN EN LOS PROCESOS OPERATIVOS
#                         DE LAS ADMINISTRADORAS.
#Observaciones         => ESTE PROGRAMA ES LLAMADO  POR EL PROGRAMA ESTB099
#                         DEL CUAL RECIBE LOS PARAMETROS DE:FECHA GENERA.
#By                    => MARCO ANTONIO GONZALEZ ROJAS
#                      => VERSION PARA TODAS LAS AFORES
#Fecha de Creacion     => 10 DE JULIO DEL 2008.
#Ultima Modificacion   => 16 JULIO DEL 2009   .
#Objetivo de la Modif  => De Acuerdo a Oficio CONSAR DOO/220/1642/2008
#Sistema               => EST
################################################################################
DATABASE safre_af
GLOBALS

DEFINE  g_cat             CHAR(300)
DEFINE  g_num_regs        INTEGER
DEFINE  g_enter           CHAR(001)

DEFINE  g_repdet                        ,
        g_repcbza               CHAR(600)
DEFINE  g_ruta  RECORD LIKE safre_af:seg_modulo.*
DEFINE  g_codigo_afore          CHAR(003)
DEFINE  g_usuario               CHAR(010) 
DEFINE  g_fecha_env             ,       
        g_dia_ant               ,
        g_fecha_gen             DATE
DEFINE  g_fecha_h               DATETIME YEAR TO SECOND
DEFINE  g_txt                   CHAR(5000)
DEFINE  g_nomarch               CHAR(120)


DEFINE g_rec      RECORD
                  sie           SMALLINT        ,
                  id_ctasub     CHAR(04)        ,
                  ent           DECIMAL (16,2 ) ,
                  sal           DECIMAL (16,2 ) ,
                  neto          DECIMAL (16,2 ) 
                  END RECORD

#--- FIN DE DEFINICION DE VARIABLES
END GLOBALS

MAIN        

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

   CALL STARTLOG("ESTB099.log")


   CALL  init()
   CALL  habil_anterior( g_fecha_gen ) RETURNING  g_dia_ant
   CALL  crea_tbls_temp()


   DISPLAY " "
   DISPLAY "PROGRAMA: ESTB099 "
   DISPLAY "FECHA DE LA OPERACION QUE SE REPORTA  : ",g_dia_ant
   DISPLAY g_fecha_h,"INICIANDO PROCESO..."

   CALL  llena_det_64()
   CALL  Act_subcta_00()
   CALL  gen_rep() #grpt


   LET g_fecha_h = CURRENT
   DISPLAY g_fecha_h,"ESTADISTICA GENERADA..."

END MAIN

FUNCTION gen_rep()
#pp-------------------------

DEFINE l_siefore    SMALLINT,
       l_id_ctasub  CHAR(04),
       l_entradas   ,
       l_salidas    ,
       l_neto       DECIMAL ( 16,2 )



    LET g_nomarch    =    g_ruta.ruta_envio CLIPPED,"/",
                      --  g_fecha_env USING"YYYYMMDD",
                          g_fecha_gen USING"YYYYMMDD",
                          "_AF_",g_codigo_afore,"_000_veresp.1110"


  
    START REPORT rpt_1 TO g_nomarch

    LET g_txt  =
               "SELECT  FIRST 1 1 ",
               "FROM    det_64 ",
               "WHERE  id_ctasub  <> '0000' ",
               "AND ( entradas <> 0  OR salidas <> 0 ) " 

    PREPARE idsql01 FROM g_txt
    EXECUTE idsql01
 
    IF SQLCA.SQLCODE       =     NOTFOUND   THEN    # NO HuBo OpEraCion 
       #---Total de Registros---#
        CASE  g_codigo_afore
           WHEN   574  #AFORE SCOTIA  ( CON SIEFORE ADICIONAL )
              LET g_num_regs      = 185       
           WHEN   564  #AFORE METLIFE ( CON SIEFORE ADICIONAL )
              LET g_num_regs      = 217
           OTHERWISE   #DEMAS  AFORES
              LET g_num_regs      = 181
        END CASE

       #* Imprime  Encabezado *
       OUTPUT TO REPORT rpt_1( 'E', 0, '    ',0,0,0 )
       #* Imprime  Detalles y Subencabezado *
          DECLARE cursor10 CURSOR FOR

                  SELECT * 
                  FROM   det_64    
                  WHERE  id_ctasub  <> "0000"
                  ORDER BY siefore,id_ctasub

          FOREACH cursor10 INTO l_siefore    ,
                                l_id_ctasub  ,
                                l_entradas   ,
                                l_salidas    ,
                                l_neto

                  OUTPUT TO REPORT rpt_1( 'D',l_siefore,l_id_ctasub,l_entradas,
                                              l_salidas,l_neto )

          END FOREACH
    ELSE 
       #---Total de Registros---#
        CASE  g_codigo_afore
           WHEN   574  #AFORE SCOTIA  ( CON SIEFORE ADICIONAL )
              LET g_num_regs      = 185       
           WHEN   564  #AFORE METLIFE ( CON SIEFORE ADICIONAL )
              LET g_num_regs      = 217
           OTHERWISE   #DEMAS  AFORES
              LET g_num_regs      = 181
        END CASE
         
        #* Imprime  Encabezado *
        OUTPUT TO REPORT rpt_1( 'E', 0, '    ',0,0,0 ) # SI HuBo OpEraCion
       
        #* Imprime  Detalles y Subencabezado *
        DECLARE cursor01 CURSOR FOR

                SELECT * 
                FROM   det_64    
                WHERE  id_ctasub  <> "0000"
                ORDER BY siefore,id_ctasub

        FOREACH cursor01 INTO l_siefore    ,
                              l_id_ctasub  ,
                              l_entradas   ,
                              l_salidas    ,
                              l_neto

                OUTPUT TO REPORT rpt_1( 'D',l_siefore,l_id_ctasub,l_entradas,
                                            l_salidas,l_neto )

        END FOREACH

    END IF


    FINISH REPORT rpt_1              

END FUNCTION

REPORT rpt_1( tipo, l_siefore,l_id_ctasub,l_entradas,l_salidas,l_neto )
#gr-------------------------

DEFINE l_siefore    SMALLINT,
       l_id_ctasub  CHAR(04),
       l_entradas   ,
       l_salidas    ,
       l_neto       DECIMAL ( 16,2 )

DEFINE
      tipo                 CHAR( 1 ),
      FormatoE                      ,
      FormatoS                      ,
      FormatoN              CHAR(16)

DEFINE 
      tipo_entidad          CHAR(03),
      entidad_sief          SMALLINT,
      subtipo_ent           CHAR(03)
  

   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      ORDER EXTERNAL BY l_siefore

   FORMAT

   ON EVERY ROW
      IF( tipo = 'E' ) THEN    #* Encabezado *
         PRINT
           COLUMN 001,"000",
           COLUMN 004,"1110",
           COLUMN 008,"001",
           COLUMN 011,g_codigo_afore,
           COLUMN 014,g_dia_ant  USING"YYYYMMDD",
           COLUMN 022,"055",
           COLUMN 025,g_num_regs USING"&&&&&&&&&&&&&&&&&&&&&&&&&&",
           COLUMN 051,5 SPACES
      ELSE                #* Detalle    *
         LET l_entradas   = l_entradas * 100
         LET l_salidas    = l_salidas  * 100
         LET l_neto       = l_neto     * 100

         LET FormatoE = '&&&&&&&&&&&&&&&&'           #* Cantidades Positivas *
         LET FormatoS = '&&&&&&&&&&&&&&&&'           #* Cantidades Negativas *
         LET FormatoN = '&&&&&&&&&&&&&&&&'           #* Cantidades Posi-Negs *

         IF( l_neto    < 0 ) THEN
            LET FormatoN[1] = '-'                    #* Cantidades Negativas *
         END IF

         #--DETALLE 1: FLUJO DIARIO OPERATIVO --

         PRINT COLUMN 01, "301",                     #* Tipo de registro *
               COLUMN 04, l_id_ctasub USING "&&&&" , #* IdCta Id SubCta  *
               COLUMN 08, l_entradas USING FormatoE, #* Entradas
               COLUMN 24, l_salidas  USING FormatoS, #* Salidas
               COLUMN 40, l_neto     USING FormatoN  #* Neto
      END IF

   BEFORE GROUP OF l_siefore

      IF( tipo <> 'E' ) THEN
          
            CASE l_siefore

               WHEN 1
                  LET tipo_entidad =	"002"
                  LET entidad_sief =    g_codigo_afore
                  LET subtipo_ent  =    "001"
               WHEN 2
                  LET tipo_entidad =    "002"  
                  LET entidad_sief =    g_codigo_afore
                  LET subtipo_ent  =    "002" 

               WHEN 3
                  LET tipo_entidad =    "002" 
                  LET entidad_sief =    g_codigo_afore
                  LET subtipo_ent  =    "003" 

               WHEN 4
                  LET tipo_entidad =    "002"  
                  LET entidad_sief =    g_codigo_afore
                  LET subtipo_ent  =    "004" 

               WHEN 5
                  LET tipo_entidad =   "002" 
                  LET entidad_sief =   g_codigo_afore
                  LET subtipo_ent  =   "005" 

               WHEN 6
                  CASE g_codigo_afore

                     WHEN 574 # AFORE SCOTIA  ( CON SIEFORE ADICIONAL )
                              #SUBCTA 017 SIEF AP VOL( 3,10,22,23,28,29 )
                        LET tipo_entidad =   "017" 
                        LET entidad_sief =   g_codigo_afore + 100
                        LET subtipo_ent  =   "001" 
                     WHEN 564 #AFORE METLIFE ( CON SIEFORE ADICIONAL )
                              #SUBCTA 003 SIEF AP COMPL ( 11,12 )
                        LET tipo_entidad =   "003" 
                        LET entidad_sief =   g_codigo_afore - 200
                        LET subtipo_ent  =   "001" 
                     OTHERWISE

                  END  CASE

            END CASE

      PRINT COLUMN 01, "101",                    #* Tipo de registro *
            COLUMN 04, tipo_entidad USING "&&&", #* Clave Tipo Tipo Ent Sief *
            COLUMN 07, entidad_sief USING "&&&", #* Clave Entidad Siefore  *
            COLUMN 10, subtipo_ent  USING "&&&", #* Clave SubTipo Ent. Sief  *
            COLUMN 13, 43 SPACES
      END IF

END REPORT
 
  
FUNCTION init()
#i-----------------------

   LET g_fecha_env         =      TODAY 
   LET g_fecha_gen         =      ARG_VAL(1) #Parametro Rec del Prog ESTB099.4gl
   LET g_fecha_h           =      CURRENT

   SELECT a.* 
   INTO   g_ruta.*
   FROM   safre_af:seg_modulo a
   WHERE  a.modulo_cod = 'est'

   SELECT a.codigo_afore
   INTO   g_codigo_afore
   FROM   tab_afore_local a

   SELECT USER
   INTO   g_usuario
   FROM   safre_af:tab_afore_local

END FUNCTION

FUNCTION habil_anterior(diaActual)
#ha----------------------

   DEFINE diaTmp          DATE,
          contador        SMALLINT,
          diaActual       DATE,
          diaHabilAnt     DATE,
          diaSemana       SMALLINT,
          feriado         SMALLINT,
          finSemana       SMALLINT

   LET diaHabilAnt = diaActual - 1 UNITS DAY

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

FUNCTION crea_tbls_temp()
#ctt---------------------
DEFINE    i            , 
          l_fin        SMALLINT

   LET       l_fin        =     0

   CREATE TEMP TABLE  det_64    ( siefore            SMALLINT        ,
                                  id_ctasub          CHAR(04)        ,
                                  entradas           DECIMAL ( 16,2 ),
                                  salidas            DECIMAL ( 16,2 ),
                                  neto               DECIMAL ( 16,2 ) )

   CASE g_codigo_afore 
      WHEN  574   #574 AFORE SCOTIA (CON SIE ADICIONAL ) INSERTA << SCOTIA >>
         LET     l_fin           =    5

         FOR     i  =  1   TO   l_fin   #Se inserta Exclusivamente para las sief
                                        #1,2,3,4,5.(No se pone el Idcuenta 0102
                                        #0401 y 0402 ).

            INSERT INTO det_64 VALUES  ( i,"0100",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0101",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0103",0,0,0 )	
            INSERT INTO det_64 VALUES  ( i,"0104",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0106",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0200",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0201",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0202",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0203",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0205",0,0,0 )	
            INSERT INTO det_64 VALUES  ( i,"0300",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0301",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0500",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0501",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0502",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0600",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0601",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0602",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0700",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0701",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0702",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0703",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0800",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0801",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0803",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0804",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0805",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0900",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"0901",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"1000",0,0,0 )
            INSERT INTO det_64 VALUES  ( i,"1001",0,0,0 )
   
          END FOR 

          # Se Inserta para la Siefore Adicional es decir la 6 .

            INSERT INTO det_64 VALUES  ( 6,"0100",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0102",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0104",0,0,0 )	
            INSERT INTO det_64 VALUES  ( 6,"0106",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0200",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0201",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0202",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0203",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0205",0,0,0 )	
            INSERT INTO det_64 VALUES  ( 6,"0400",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0401",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0402",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0500",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0501",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0502",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0600",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0601",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0602",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0700",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0701",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"0702",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"1000",0,0,0 )
            INSERT INTO det_64 VALUES  ( 6,"1001",0,0,0 )

       OTHERWISE
          CASE  g_codigo_afore
             WHEN  564 #564 AFORE METLIFE ( CON SIEFORE ADICIONAL )
                LET     l_fin           =    6
             OTHERWISE #DEMAS AFORES ,CON AFORES QUE SOLO TIENEN 5 SIEFORES
                LET     l_fin           =    5
          END CASE

          FOR     i    =      1   TO   l_fin

             INSERT INTO det_64 VALUES  ( i,"0100",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0101",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0102",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0103",0,0,0 )	
             INSERT INTO det_64 VALUES  ( i,"0104",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0106",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0200",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0201",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0202",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0203",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0205",0,0,0 )	
             INSERT INTO det_64 VALUES  ( i,"0300",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0301",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0400",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0401",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0402",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0500",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0501",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0502",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0600",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0601",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0602",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0700",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0701",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0702",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0703",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0800",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0801",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0803",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0804",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0805",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0900",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0901",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"1000",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"1001",0,0,0 )

          END FOR 

   END CASE

END FUNCTION

FUNCTION llena_det_64()
#lld6---------------------


#01    00    RECAUDACION 
 
CALL inicializa()

   CASE  g_codigo_afore

      WHEN 564  # SOLO AFORE METLIFE
         
        LET g_txt    =

        "SELECT  A.siefore, ",
             "CASE WHEN A.subcuenta IN ( 1,2 ) AND A.tipo_movimiento IN (1,2,3,4) ",
                  "AND  B.ident_arch      <> 3  AND A.id_aportante <> 'BANXICO' ",
                  "THEN '0101' ",
                  "WHEN A.subcuenta IN ( 3,11 ) AND A.tipo_movimiento  IN ( 1,3 ) ",
                  "AND  B.ident_arch      <> 3 AND A.id_aportante <> 'BANXICO' ",
                  "THEN '0102' ",
                  "WHEN A.subcuenta IN (5,6,9)   AND A.tipo_movimiento IN (1,3 ) ",
                  "AND  B.ident_arch      <> 3  AND A.id_aportante <> 'BANXICO' ",
                  "THEN '0103' ",
                  "WHEN A.subcuenta IN ( 1,2,5,6,9,3,11 ) AND A.tipo_movimiento = 3 ",
                  "AND  B.ident_arch      <> 3  AND A.id_aportante =  'BANXICO' ",
                  "THEN '0104' ",
                  "WHEN A.subcuenta IN ( 1,2,3,5,6,9,11 ) AND A.tipo_movimiento IN ",
"(1,2,3,4 ) ",
                  "AND  B.ident_arch      =  3 ",
                  "THEN '0106' ",
                  "ELSE '0000' ",
                  "END CASE , ",
                  "NVL(SUM(A.monto_en_pesos),0),0 ",
          "FROM dis_cuenta A , dis_cza_aporte B ",
          "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
          "AND   A.folio             = B.folio ",
          "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 ",
          "ORDER BY 1,2 " 


      OTHERWISE # DE+ AFORES

         LET g_txt    =

         "SELECT  A.siefore, ",
             "CASE WHEN A.subcuenta IN ( 1,2 ) AND A.tipo_movimiento IN (1,2,3,4) ",
                  "AND  B.ident_arch      <> 3  AND A.id_aportante <> 'BANXICO' ",
                  "THEN '0101' ",
                  "WHEN A.subcuenta IN ( 3,11 ) AND A.tipo_movimiento  = 1 ",
                  "AND  B.ident_arch      <> 3 ",
                  "THEN '0102' ",
                  "WHEN A.subcuenta IN (5,6,9)   AND A.tipo_movimiento IN (1,3 ) ",
                  "AND  B.ident_arch      <> 3  AND A.id_aportante <> 'BANXICO' ",
                  "THEN '0103' ",
                  "WHEN A.subcuenta IN ( 1,2,5,6,9,3,11 ) AND A.tipo_movimiento = 3 ",
                  "AND  B.ident_arch      <> 3  AND A.id_aportante =  'BANXICO' ",
                  "THEN '0104' ",
                  "WHEN A.subcuenta IN ( 1,2,3,5,6,9,11 ) AND A.tipo_movimiento IN ",
"(1,2,3,4 ) ",
                  "AND  B.ident_arch      =  3 ",
                  "THEN '0106' ",
                  "ELSE '0000' ",
                  "END CASE , ",
                  "NVL(SUM(A.monto_en_pesos),0),0 ",
          "FROM dis_cuenta A , dis_cza_aporte B ",
          "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
          "AND   A.folio             = B.folio ",
          "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 ",
          "ORDER BY 1,2 " 

   END  CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry1 FROM g_txt
   DECLARE c1 CURSOR FOR qry1

   FOREACH c1 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                   g_rec.sal

     CALL Act_tbl ( g_rec.* ) 

   END FOREACH 

   #02    00    TRASPASOS   

CALL inicializa()

   CASE g_codigo_afore

       WHEN 516  # SOLO AFORE XXI     
           
          LET g_txt    =

              "SELECT  A.siefore, ",
                   "CASE WHEN C.tipo_traspaso IN (1,3) ",
                        "THEN '0201' ",
                        "WHEN C.tipo_traspaso  = 2 ",
                        "THEN '0202' ",
                   "ELSE '0000' ",
                   "END CASE ,0, ",
                   "NVL(SUM(A.monto_en_pesos),0) ",
              "FROM    dis_cuenta A , taa_cd_ctr_folio C ",
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",
                                "WHERE A.tipo_movimiento   =  B.marca_cod ) ",
                "AND   A.tipo_movimiento   BETWEEN  200 AND 299 ",
                "AND   A.folio             = C.folio ",
                "AND   NOT EXISTS ( SELECT 1 FROM taa_cd_indebidos d WHERE A.nss = ",
      "                            d.nss  AND d.estado = 103 ",
      "                            AND A.fecha_conversion = d.fecha_liquidacion ) ",
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
   "          GROUP BY 1,2 ",
             "UNION ",
             "SELECT  A.siefore, ",
                     "CASE WHEN EXISTS (SELECT 1 FROM taa_viv_recepcion C ",
                          "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ) ",
                          "THEN '0201' ",
                          "WHEN EXISTS (SELECT 1 FROM taa_viv_recepcion C ",
                          "WHERE A.folio =  C.folio AND C.ident_operacion  = '12' ) ",
                          "THEN '0202' ",
                    "ELSE '0000' ",
                    "END CASE , ",
                    "NVL(SUM(A.monto_en_pesos),0),0 ",
             "FROM   dis_cuenta  A ",
            "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
              "AND   A.tipo_movimiento   =  1 ",
              "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
            "GROUP BY 1,2 ",
            "UNION ",
            "SELECT   A.siefore, ",
                     "'0203', ",
                      "NVL(SUM(A.monto_en_pesos),0),0 ",
             "FROM    dis_cuenta  A ",
             "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.tipo_movimiento    = 779 ",
               "AND  EXISTS ( SELECT 1 FROM taa_cd_indebidos d WHERE A.nss = ",
               "    d.nss  AND d.estado = 103 ",
               "    AND A.fecha_conversion = d.fecha_liquidacion ) ",	
               "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
            "GROUP BY 1,2 "

       OTHERWISE # DE+ AFORES

          LET g_txt    =

            "SELECT  A.siefore, ",
                 "CASE WHEN C.tipo_traspaso IN (1,3) ",
                      "THEN '0201' ",
                      "WHEN C.tipo_traspaso  = 2 ",
                      "THEN '0202' ",
                 "ELSE '0000' ",
                 "END CASE ,0, ",
                 "NVL(SUM(A.monto_en_pesos),0) ",
            "FROM    dis_cuenta A , taa_cd_ctr_folio C ",
            "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
              "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",
                              "WHERE A.tipo_movimiento   =  B.marca_cod ) ",
              "AND   A.tipo_movimiento   BETWEEN  200 AND 299 ",
              "AND   A.folio             = C.folio ",
              "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
           "GROUP BY 1,2 ",
           "UNION ",
           "SELECT  A.siefore, ",
                   "CASE WHEN EXISTS (SELECT 1 FROM taa_viv_recepcion C ",
                        "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ) ",
                        "THEN '0201' ",
                        "WHEN EXISTS (SELECT 1 FROM taa_viv_recepcion C ",
                        "WHERE A.folio =  C.folio AND C.ident_operacion  = '12' ) ",
                        "THEN '0202' ",
                  "ELSE '0000' ",
                  "END CASE , ",
                  "NVL(SUM(A.monto_en_pesos),0),0 ",
           "FROM   dis_cuenta  A ",
          "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
            "AND   A.tipo_movimiento   =  1 ",
            "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 ",
          "UNION ",
          "SELECT   A.siefore, ",
                   "'0203', ",
                    "NVL(SUM(A.monto_en_pesos),0),0 ",
           "FROM    dis_cuenta  A ",
           "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
             "AND   A.tipo_movimiento    = 779 ",
             "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 "

   END CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry2 FROM g_txt
   DECLARE c2 CURSOR FOR qry2

   FOREACH c2 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                   g_rec.sal

     CALL Act_tbl ( g_rec.* ) 

    CASE g_codigo_afore         

       WHEN  516  # SOLO AFORE XXI 

          IF (g_rec.id_ctasub = "0203") THEN

              LET g_rec.sal = -g_rec.ent
              LET g_rec.ent = 0
              CALL Act_tbl ( g_rec.* )

           END IF

       OTHERWISE # DE+ AFORES

    END CASE

  END FOREACH 


   #03    00    SAR 92            

CALL inicializa()
LET g_txt    =

   "SELECT    A.siefore, ",
             "'0301', ",
             "NVL(SUM(A.monto_en_pesos),0) ,0 ",
   "FROM      dis_cuenta  A ",
   "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
   "AND       A.subcuenta          IN   ( 7,8 ) ",
   "AND       A.tipo_movimiento    IN   ( 1,4 ) ",
   "AND       A.id_aportante      MATCHES '[CT]I-*' ",
   "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
   "GROUP BY 1,2 ",
   "ORDER BY 1,2 " 

  LET g_txt = g_txt CLIPPED
  PREPARE qry3 FROM g_txt
  DECLARE c3 CURSOR FOR qry3

  FOREACH c3 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                   g_rec.sal

     CALL Act_tbl ( g_rec.* ) 

   END FOREACH 

   #04    00    APORTACIONES VOLUNTARIAS

CALL inicializa()

   CASE  g_codigo_afore

      WHEN 516  # SOLO AFORE XXI
          
         LET g_txt    =

         "SELECT A.siefore, ",
         "CASE ",
                 "WHEN A.subcuenta IN ( 3,10,22,23,28,29 ) ",  #ENT 0401
                         "THEN '0401' ",
                 "WHEN A.subcuenta IN ( 11,12 ) THEN '0402' ", #ENT 0402
                 "ELSE '0000' ",
         "END CASE , ",
         "NVL(SUM(A.monto_en_pesos),0) ,0 ",
         "FROM      dis_cuenta  A ",
         "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND       A.subcuenta          IN   ( 3,10,11,12,22,23,28,29) ",
         "AND       A.id_aportante      MATCHES 'VE*' ",
         "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
         "AND       A.tipo_movimiento = 1 ",
         "GROUP BY 1,2 ",
         "UNION ",
         "SELECT A.siefore, '0401', ",
         "0, NVL(SUM(A.monto_en_pesos),0) ",#SAL 0401
         "FROM      dis_cuenta  A ",
         "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND       A.subcuenta          IN   ( 3,10,22,23,28,29) ",
         "AND       A.tipo_movimiento IN ( 10,25,490 ) ",
         "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
         "GROUP BY 1,2 ",
         # T.M 25  RETENCION    I D E por el momento ligado a la subcta 10
         "UNION ",
         "SELECT A.siefore, '0402', ",
         "0, NVL(SUM(A.monto_en_pesos),0) ",#SAL 0402
         "FROM      dis_cuenta  A ",
         "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND       A.subcuenta          =   12 ",
         "AND       A.tipo_movimiento IN ( 10,803 ) ",
         "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
         "GROUP BY 1,2 ",
         "ORDER BY 1,2 " 
         # T.M 803 Retiro de aportaciones Complementarias (tipo de Retiro T)
         # + T.M  10 ( Retencion de Imp )
 
      WHEN 564  # SOLO AFORE METLIFE
          
         LET g_txt    =
 
            "SELECT A.siefore, ", 
                "CASE ",
                    "WHEN A.subcuenta IN ( 3,10,22,23,28,29 ) ",
                            "THEN '0401' ",
                    "WHEN A.subcuenta IN ( 11,12,16 ) THEN '0402' ",
                    "ELSE '0000' ",
                 "END CASE, ",
            "NVL(SUM(A.monto_en_pesos),0) ,0 ",
            "FROM      dis_cuenta  A ",
            "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
            "AND       A.subcuenta          IN   ( 3,10,11,12,16,22,23,28,29) ",
            "AND      (A.id_aportante      MATCHES 'VE*' OR ",
                      "A.id_aportante      MATCHES 'OMNI*') ",
            "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
            "AND       A.tipo_movimiento = 1 ",
            "GROUP BY 1,2 ",
            "UNION ",           # ISR DE PVI Subcuenta 16 y Tipo Mov 10
            "SELECT A.siefore, ",
                             "CASE ",
                             "WHEN A.tipo_movimiento = 801  OR ",
                                "( A.subcuenta = 16  AND A.tipo_movimiento IN ( 10,802 ) ) ",
                                  "THEN '0402' ",
                             "WHEN A.id_aportante NOT MATCHES 'OMNI*' ",
                              "THEN '0401' ",
                             "ELSE '0000' ",
                          "END CASE, ",
                     "0, NVL(SUM(A.monto_en_pesos),0) ",
                     "FROM      dis_cuenta  A ",
                     "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
                     "AND       A.subcuenta          IN   ( 3,10,16,22,23,28,29) ",
                     "AND       A.tipo_movimiento IN ( 10, 490 ,801,802 ) ",
                     "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
                     "GROUP BY 1,2 ",
                     "ORDER BY 1,2,3 "
    
      WHEN 568  # SOLO AFORE COPPEL 
          
         LET g_txt    =

         "SELECT A.siefore, ",
         "CASE ",
                 "WHEN A.subcuenta IN ( 3,10,16,22,23,28,29 ) ",
                         "THEN '0401' ",
                 "WHEN A.subcuenta IN ( 11,12 ) THEN '0402' ",
                 "ELSE '0000' ",
         "END CASE , ",
         "NVL(SUM(A.monto_en_pesos),0) ,0 ",
         "FROM      dis_cuenta  A ",
         "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND       A.subcuenta          IN   ( 3,10,11,12,16,22,23,28,29) ",
         "AND       A.id_aportante      MATCHES 'VE*' ",
         "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
         "AND       A.tipo_movimiento = 1 ",
         "GROUP BY 1,2 ",
         "UNION ",
         "SELECT A.siefore, '0401', ",
         "0, NVL(SUM(A.monto_en_pesos),0) ",
         "FROM      dis_cuenta  A ",
         "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND       A.subcuenta          IN   ( 3,10,16,22,23,28,29) ",
         "AND       A.tipo_movimiento IN ( 10, 490 ) ",
         "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
         "GROUP BY 1,2 ",
         "ORDER BY 1,2 " 

      OTHERWISE # DE+ AFORES
          
         LET g_txt    =

         "SELECT A.siefore, ",
         "CASE ",
                 "WHEN A.subcuenta IN ( 3,10,22,23,28,29 ) ",
                         "THEN '0401' ",
                 "WHEN A.subcuenta IN ( 11,12 ) THEN '0402' ",
                 "ELSE '0000' ",
         "END CASE , ",
         "NVL(SUM(A.monto_en_pesos),0) ,0 ",
         "FROM      dis_cuenta  A ",
         "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND       A.subcuenta          IN   ( 3,10,11,12,22,23,28,29) ",
         "AND       A.id_aportante      MATCHES 'VE*' ",
         "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
         "AND       A.tipo_movimiento = 1 ",
         "GROUP BY 1,2 ",
         "UNION ",
         "SELECT A.siefore, '0401', ",
         "0, NVL(SUM(A.monto_en_pesos),0) ",
         "FROM      dis_cuenta  A ",
         "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND       A.subcuenta          IN   ( 3,10,22,23,28,29) ",
         "AND       A.tipo_movimiento IN ( 10, 490 ) ",
         "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
         "GROUP BY 1,2 ",
         "ORDER BY 1,2 " 

   END CASE  
 

  LET g_txt = g_txt CLIPPED
  PREPARE qry4 FROM g_txt
  DECLARE c4 CURSOR FOR qry4

  FOREACH c4 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                  g_rec.sal

     CALL Act_tbl ( g_rec.* ) 

   END FOREACH 

   #05    00    CORRECCION DE CUENTAS 

CALL inicializa()
LET g_txt    =
  
  "SELECT  A.siefore,'0501', ",
          "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
          "NVL(SUM ( CASE ",
                           "WHEN A.tipo_movimiento <> 1 THEN A.monto_en_pesos ",
                    "ELSE 0 ",
                    "END ),0) ",
  "FROM   dis_cuenta  A ",
  "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
  "AND     A.id_aportante[1,3] MATCHES  'U[CN]-' ",
  "AND     A.tipo_movimiento   IN ( 1,241,242 ) ",
  "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",
  "GROUP BY 1,2 ",
  "UNION ",
  "SELECT  A.siefore,'0502', ",
         "NVL(SUM ( CASE ",
                          "WHEN A.monto_en_pesos > 0 THEN A.monto_en_pesos ",
                    "ELSE 0 ",
                    "END ),0), ",
         "NVL(SUM ( CASE ",
                          "WHEN A.monto_en_pesos < 0 THEN A.monto_en_pesos ",
                    "ELSE 0 ",
                    "END ),0) ",
  "FROM     dis_cuenta  A ,sep_det_reg_sol_reclamante B ",
    "WHERE  A.fecha_proceso           = '", g_dia_ant , "' ",
    "AND    B.estado                  = 9 ",
    "AND    A.folio                   = B.folio ",
    "AND    ( A.nss = B.n_seguro  OR  A.nss  = B.nss ) ",
    "AND    A.siefore           IN ( 1,2,3,4,5,6 ) ",
    "AND    A.subcuenta    NOT IN (4,8,14) ",
  "GROUP BY 1,2 "
 
  LET g_txt = g_txt CLIPPED
  PREPARE qry5 FROM g_txt
  DECLARE c5 CURSOR FOR qry5

  FOREACH c5 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
	                  g_rec.sal

     CALL Act_tbl ( g_rec.* ) 

   END FOREACH 

   #06    00    TRANSFERENCIA ENTRE SIEFORES    

#-----------------

CALL inicializa()

CASE  g_codigo_afore

   WHEN 516  # SOLO AFORE XXI
     
      LET g_txt    =
  
        "SELECT A.siefore,'0601', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
               "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",
         "FROM   dis_cuenta  A,tes_solicitud B ",
         "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
           "AND   A.tipo_movimiento   IN ( 1,210 ) ",
           "AND   A.folio = B.folio ",
           "AND   B.estado = 103 AND B.tipo_traspaso =   13 ",
           "AND   B.folio_solicitud = A.consecutivo_lote ",
           "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
        "GROUP BY 1,2 ",
        "UNION ",
        "SELECT A.siefore,'0602', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
               "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",
        "FROM   dis_cuenta  A ",
        "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
        "AND     A.tipo_movimiento   IN ( 1,210 ) ",
        "AND     EXISTS ( SELECT  1 ",
                             "FROM tes_solicitud B ",
                         "WHERE  B.fecha_traspaso  = '", g_dia_ant , "' ",
                           "AND  B.estado          =  103 ",
                           "AND  B.tipo_traspaso   <> 13  ",
                           "AND  A.folio  = B.folio )     ",
        "AND     A.siefore           IN ( 1,2,3,4,5,6 )   ",
        "GROUP BY 1,2 ",
        "ORDER BY 1,2 "

   WHEN 574  # SOLO AFORE SCOTIA

      LET g_txt    =
  
        "SELECT A.siefore,'0601', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
               "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",
         "FROM   dis_cuenta  A,tes_solicitud B ",
         "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
           "AND   A.tipo_movimiento   IN ( 1,210 ) ",
           "AND   A.folio = B.folio ",
           "AND   B.estado = 103 AND B.tipo_traspaso IN ( 5,13 ) ",
           "AND   B.folio_solicitud = A.consecutivo_lote ",
           "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
        "GROUP BY 1,2 ",
        "UNION ",
        "SELECT A.siefore,'0602', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
               "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",
        "FROM   dis_cuenta  A,tes_solicitud B ",
        "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
        "AND     A.tipo_movimiento   IN ( 1,210 ) ",
        "AND     A.folio = B.folio ",
        "AND     B.estado = 103 AND B.tipo_traspaso IN (1,3,4,6,7,8,9) ",
        "AND     B.folio_solicitud = A.consecutivo_lote  ",
        "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",
        "GROUP BY 1,2 ",
        "ORDER BY 1,2 "

    WHEN 562  # SOLO AFORE INVERCAP 

       LET g_txt    =
       
          "SELECT A.siefore,'0601', ",
                 "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
                 "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",
           "FROM   dis_cuenta  A,tes_solicitud B ",
           "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
             "AND   A.tipo_movimiento   IN ( 1,210 ) ",
             "AND   A.folio = B.folio ",
             "AND   B.estado = 103 AND B.tipo_traspaso =   13 ",
             "AND   B.folio_solicitud = A.consecutivo_lote ",
             "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 ",
          "UNION ",
          "SELECT A.siefore,'0602', ",
                 "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
                 "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",
          "FROM   dis_cuenta  A ",
          "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
          "AND     A.tipo_movimiento   IN ( 1,210 ) ",
          "AND     EXISTS ( SELECT  1 ",
                               "FROM tes_solicitud B ",
                           "WHERE  B.fecha_traspaso  = '", g_dia_ant , "' ",
                             "AND  B.estado          =  103 ",
                             "AND  B.tipo_traspaso IN (1,3,4,5,6,7,8,9)  ",
                             "AND  A.folio  = B.folio )     ",
          "AND     A.siefore           IN ( 1,2,3,4,5,6 )   ",
          "GROUP BY 1,2 ",
          "ORDER BY 1,2 "

    OTHERWISE # DE+ AFORES 

       LET g_txt    =

          "SELECT A.siefore,'0601', ",
                 "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
                 "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",
           "FROM   dis_cuenta  A,tes_solicitud B ",
           "WHERE   A.fecha_conversion  =   '09/28/2009'   ",
             "AND   A.tipo_movimiento   IN ( 1,210 ) ",
             "AND   A.folio = B.folio ",
             "AND   B.estado = 103 AND B.tipo_traspaso =   13 ",
             "AND   B.folio_solicitud = A.consecutivo_lote ",
             "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 ",
          "UNION ",
          "SELECT A.siefore,'0602', ",
                 "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",
                 "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",
          "FROM   dis_cuenta  A,tes_solicitud B ",
          "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
          "AND     A.tipo_movimiento   IN ( 1,210 ) ",
          "AND     A.folio = B.folio ",
          "AND     B.estado = 103 AND B.tipo_traspaso IN (1,3,4,5,6,7,8,9,14) ",
          "AND     B.folio_solicitud = A.consecutivo_lote  ",
          "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 ",
          "ORDER BY 1,2 "


END CASE

  LET g_txt = g_txt CLIPPED
  PREPARE qry6 FROM g_txt
  DECLARE c6 CURSOR FOR qry6

  FOREACH c6 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
	                  g_rec.sal

     CALL Act_tbl ( g_rec.* ) 

   END FOREACH 

#------------

   #07    00    ISSSTE             
   #      01
   #      02
   #09    00    DEVOLUCION DE PAGOS SIN JUSTIFICACION LEGAL
   #      01        


CALL inicializa()

          LET g_txt    =

            "SELECT  A.siefore, ",
                  "CASE WHEN A.subcuenta IN ( 13,30,31,32,33,34,35,14 ) AND A.tipo_movimiento IN( 1,4,17,3 ) AND A.id_aportante NOT  MATCHES '[CT]I-*' AND A.folio IN ( SELECT  B.folio FROM dis_cza_issste B WHERE   A.folio  =  B.folio ) THEN '0701' ",
                       "WHEN A.subcuenta IN ( 13,14,19 ) AND A.tipo_movimiento IN( 1,4 ) ",
                       "AND       A.id_aportante      MATCHES '[CT]I-*' ",
                       "THEN '0702' ",
                       "ELSE '0000' ",
                  "END CASE , ",
                  "NVL(SUM(A.monto_en_pesos),0),0 ",
             "FROM dis_cuenta A ",
             "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",           
               "AND   A.subcuenta IN ( 13,14,19,30,31,32,33,34,35 ) ",
               "AND   A.tipo_movimiento IN ( 1,4,17,3 ) ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
             "GROUP BY 1,2 ",
             "UNION ",
             "SELECT  A.siefore,'0901',0,NVL(SUM(A.monto_en_pesos),0) ",
             "FROM dis_cuenta A ",
             "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.subcuenta IN ( 1,2 ) ",
               "AND   A.tipo_movimiento IN ( 540,545,550,555 ) ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
             "GROUP BY 1,2 ",
             "ORDER BY 1,2 " 

   LET g_txt = g_txt CLIPPED
   PREPARE qry7 FROM g_txt
   DECLARE c7 CURSOR FOR qry7

   FOREACH c7 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
 	                  g_rec.sal

      CALL Act_tbl ( g_rec.* ) 

   END FOREACH 

   #07    00    ISSSTE                       
   #      03     

   #08    00    RETIROS       
   #      01    RETIRO TOTAL
   #      03    TRANSFERENCIA AL GOBIERNO FEDERAL
   #      04    RETIRO PARCIAL POR DESEMPLEO
   #      05    RETIRO PARCIAL POR MATRIMONIO
   #10    00    0TROS  CONCEPTOS
   #10    01    FONDOS DE PREVISION SOCIAL


#Nuevos T.M para la Subctaid 08 04  Dispocision parcial Desempleo
        --876( ret parcial por desempleo tipo A )
        --877( ret parcial por desempleo tipo B )
        --878( ret parcial por desempleo pago complementario )		

CALL inicializa()

   CASE g_codigo_afore

      WHEN 564  # SOLO AFORE METLIFE

         LET g_txt    =

           "SELECT A.siefore, ",
                   "CASE WHEN A.tipo_movimiento BETWEEN 881 AND 889 ",
                        "THEN '0703' ",
                        "WHEN A.tipo_movimiento IN ( 825,820,830,840,850,860,880,10 ) AND A.subcuenta NOT IN (3,10,16,23) ",
                        "THEN '0801' ",
                        "WHEN A.tipo_movimiento IN ( 800,810,815 ) ",
                        "THEN '0803' ",
                        "WHEN A.tipo_movimiento IN ( 875,876,877,878 ) ",
                        "THEN '0804' ",
                        "WHEN A.tipo_movimiento = 870  ",
                        "THEN '0805' ",
                        "ELSE '0000' ",
                   "END CASE , ",
                   "0,NVL(SUM(A.monto_en_pesos),0) ",
         "FROM      dis_cuenta  A ",
         "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
         "AND       A.tipo_movimiento IN ( 800,881,882,883,884,885,886,887,888, ",
                                            "889,825,820,830,840,850,860,880, ",
                                            "870,875,876,877,878,810,815,10 ) ",
         "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
         "GROUP BY 1,2 ",
         "ORDER BY 1,2 " 
      
      WHEN 568  # SOLO AFORE COPPEL  

         LET g_txt    =

           "SELECT A.siefore, ",
                   "CASE WHEN A.tipo_movimiento BETWEEN 881 AND 889 ",
                        "THEN '0703' ",
                        "WHEN A.tipo_movimiento IN ( 825,820,830,840,850,860,880,10 ) AND A.subcuenta NOT IN (3,10,16,23) ",
                        "THEN '0801' ",
                        "WHEN A.tipo_movimiento IN ( 800,810,815 ) ",
                        "THEN '0803' ",
                        "WHEN A.tipo_movimiento IN ( 875,876,877,878 ) ",
                        "THEN '0804' ",
                        "WHEN A.tipo_movimiento = 870  ",
                        "THEN '0805' ",
                        "ELSE '0000' ",
                   "END CASE , ",
                   "0,NVL(SUM(A.monto_en_pesos),0) ",
         "FROM      dis_cuenta  A ",
         "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
         "AND       A.tipo_movimiento IN ( 800,881,882,883,884,885,886,887,888, ",
                                           "889,825,820,830,840,850,860,880, ",
                                           "870,875,876,877,878,810,815,10 ) ",
         "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
         "GROUP BY 1,2 ",
         "ORDER BY 1,2 " 
      
      WHEN 516  # SOLO AFORE XXI        

         LET g_txt    =
             
            "SELECT A.siefore, ",
                    "CASE WHEN ( ( A.tipo_movimiento BETWEEN 881 AND 889 ) OR ",
                         "( A.tipo_movimiento = 10 AND A.subcuenta = 13 ) ) ",
                         "THEN '0703' ",
                         "WHEN A.tipo_movimiento IN ( 825,820,830,840,850,860,880,10 ) AND A.subcuenta NOT IN ( 3,10,12,13,23 ) ",
                         "THEN '0801' ",
                         "WHEN A.tipo_movimiento IN ( 800,810,815 ) ",
                         "THEN '0803' ",
                         "WHEN A.tipo_movimiento IN ( 875,876,877,878 ) ",
                         "THEN '0804' ",
                         "WHEN A.tipo_movimiento = 870  ",
                         "THEN '0805' ",
                         "ELSE '0000' ",
                    "END CASE , ",
                    "0,NVL(SUM(A.monto_en_pesos),0) ",
          "FROM      dis_cuenta  A ",
          "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
          "AND       A.tipo_movimiento IN ( 800,881,882,883,884,885,886,887,888, ",
                                            "889,825,820,830,840,850,860,880, ",
                                            "870,875,876,877,878,810,815,10 ) ",
          "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 ",
          "ORDER BY 1,2 " 

      OTHERWISE # DE+ AFORES

         LET g_txt    =
             
            "SELECT A.siefore, ",
                    "CASE WHEN A.tipo_movimiento BETWEEN 881 AND 889 ",
                         "THEN '0703' ",
                         "WHEN A.tipo_movimiento IN ( 825,820,830,840,850,860,880,10 ) AND A.subcuenta NOT IN (3,10,23) ",
                         "THEN '0801' ",
                         "WHEN A.tipo_movimiento IN ( 800,810,815 ) ",
                         "THEN '0803' ",
                         "WHEN A.tipo_movimiento IN ( 875,876,877,878 ) ",
                         "THEN '0804' ",
                         "WHEN A.tipo_movimiento = 870  ",
                         "THEN '0805' ",
                         "ELSE '0000' ",
                    "END CASE , ",
                    "0,NVL(SUM(A.monto_en_pesos),0) ",
          "FROM      dis_cuenta  A ",
          "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
          "AND       A.tipo_movimiento IN ( 800,881,882,883,884,885,886,887,888, ",
                                            "889,825,820,830,840,850,860,880, ",
                                            "870,875,876,877,878,810,815,10 ) ",
          "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
          "GROUP BY 1,2 ",
          "ORDER BY 1,2 " 

   END CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry8 FROM g_txt
   DECLARE c8 CURSOR FOR qry8

   FOREACH c8 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
	           g_rec.sal

      CALL Act_tbl ( g_rec.* ) 

   END FOREACH 

#10    00    0TROS  CONCEPTOS
#-----------------

CALL inicializa()

#-- 620 ABONO POR AJUSTE OPERATIVO (PAC)          ENTRADA
#-- 610 CARGO POR AJUSTE OPERATIVO (PAC)          SALIDA

CASE  g_codigo_afore


   WHEN 516  # SOLO AFORE XXI

     
      LET g_txt    =
  
        "SELECT A.siefore,'1000', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,620 ,A.monto_en_pesos ,0)),0), ",
               "NVL(SUM (DECODE (A.tipo_movimiento,610 ,A.monto_en_pesos,0)),0) ",
         "FROM   dis_cuenta  A ",
         "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
           "AND   A.tipo_movimiento   IN ( 610,620 ) ",
           "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
        "GROUP BY 1,2 ",
        "UNION ",
        "SELECT    A.siefore, ",
                   "'1000', ",
                  "NVL(SUM(A.monto_en_pesos),0) ,0 ",
        "FROM      dis_cuenta  A ",
        "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
        "AND       A.tipo_movimiento   =  570        ",
        "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
        "GROUP BY 1,2 "

   WHEN 562  # SOLO AFORE INVERCAP
     
      LET g_txt    =
  
        "SELECT A.siefore,'1000', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,620 ,A.monto_en_pesos ,0)),0), ",
               "NVL(SUM (DECODE (A.tipo_movimiento,610 ,A.monto_en_pesos,0)),0) ",
         "FROM   dis_cuenta  A ",
         "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
           "AND   A.tipo_movimiento   IN ( 610,620 ) ",
           "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
        "GROUP BY 1,2 "

END CASE

  LET g_txt = g_txt CLIPPED
  PREPARE qry10 FROM g_txt
  DECLARE c10 CURSOR FOR qry10

  FOREACH c10 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
	           g_rec.sal

     CALL Act_tbl ( g_rec.* ) 

  END FOREACH 

END FUNCTION 
#------------
   
FUNCTION inicializa()
#i-----------------------

    INITIALIZE g_rec.*  TO NULL

END FUNCTION 

FUNCTION Act_tbl(A_rec)
#i-----------------------

 DEFINE A_rec      RECORD
                   sie           SMALLINT        ,
                   id_ctasub     CHAR(04)        ,
                   ent           DECIMAL (16,2 ) ,
                   sal           DECIMAL (16,2 ) ,
                   neto          DECIMAL (16,2 ) 
                  END RECORD

     LET  A_rec.sal           =  A_rec.sal * -1  

     CASE
       WHEN A_rec.ent   <> 0  AND A_rec.sal <> 0 
        UPDATE
            det_64
            SET    entradas   =  A_rec.ent ,
                   salidas    =  A_rec.sal  ,
                   neto       =  A_rec.ent   -  A_rec.sal
            WHERE  siefore    =  A_rec.sie
             AND  id_ctasub   =  A_rec.id_ctasub
       WHEN A_rec.ent  <> 0  AND A_rec.sal = 0
        UPDATE
            det_64
            SET    entradas   =  A_rec.ent ,
                   neto       =  A_rec.ent   -  salidas  
            WHERE  siefore    =  A_rec.sie
              AND  id_ctasub  =  A_rec.id_ctasub
       WHEN A_rec.ent  =  0  AND A_rec.sal <> 0
        UPDATE
            det_64
            SET    salidas    =  A_rec.sal ,
                   neto       =  entradas    -  A_rec.sal
            WHERE  siefore    =  A_rec.sie
              AND  id_ctasub  =  A_rec.id_ctasub
       WHEN A_rec.ent  <> 0  AND A_rec.sal = 0
       OTHERWISE RETURN
     END CASE  

END FUNCTION 

FUNCTION Act_subcta_00()
#As----------------------
DEFINE   l_sief     SMALLINT
DEFINE   l_id_cta   CHAR(02)
DEFINE   l_sent     ,
         l_ssal     ,
         l_neto     DECIMAL(16,2)

LET  l_sief       =    NULL
LET  l_id_cta     =    NULL
LET  l_sent       =    0
LET  l_ssal       =    0
LET  l_neto       =    0


DECLARE d CURSOR FOR

   SELECT A.siefore,A.id_ctasub[1,2],
          NVL(SUM(A.entradas),0),NVL(SUM(A.salidas),0)
   FROM   det_64 A
   WHERE  id_ctasub  <> '0000'
   GROUP BY 1,2
   ORDER BY 1,2

   FOREACH d INTO l_sief,l_id_cta,l_sent,l_ssal

         UPDATE
            det_64
            SET    entradas        =  l_sent    ,
                   salidas         =  l_ssal    ,
                   neto            =  l_sent   -  l_ssal   
            WHERE  siefore         =  l_sief       
              AND  id_ctasub[1,2]  =  l_id_cta             
              AND  id_ctasub[3,4]  =  '00'

   END FOREACH


END FUNCTION

###############################################################################	
#Proyecto	             => SISTEMA DE SAFRE ( MEXICO )
#Owner                 => E.F.P
#                  ** V E R S I O N      U N I C A **
#Programa  ESTB110     => ESTE ARCHIVO CONTIENE LA INFORMACION DE LOS MOV.
#                         DIARIOS QUE SE REALIZAN EN LOS PROCESOS OPERATIVOS
#                         DE LAS ADMINISTRADORAS.
#Observaciones         => ESTE PROGRAMA ES LLAMADO  POR EL PROGRAMA ESTB109
#                         DEL CUAL RECIBE LOS PARAMETROS DE:FECHA GENERA.
#By                    => MARCO ANTONIO GONZALEZ ROJAS
#                      => VERSION PARA TODAS LAS AFORES CON FORMATEO PESOS 2DEC
#FeCHa CrEaCiOn        => ENERO 2015  CPL-1808.
#                         
#Objetivo de la Modif  => Oficio Num DOO/220/0247/2010   
#                         Circular Consar 19-9 publicado en el Diario Oficial
#                         de la federacion del 22 de enero del 2010
#                         22 y 29 de Marzo del 2010 de las 11 a la 13:00 pm
#                                          
#                         La modificacion consistio en Agrandar el Tamaño de la
#                         Variable de Vivienda  para el Detalle 2.
#Ultima Modificacion  ==> Oficio DOO/220/1745/2011 Emitido por CONSAR.
#                                                    
#  LA MODIFICACIÒN CONSISTIRÀ EN GUARDAR LA INFORMACIÒN EN UNA TABLA FISICA.
#  PARA PODERLA EXPLOTAR EN CUALQUIER MOMENTO. 
#                                          
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
DEFINE  g_dd                              , 
        g_mm                    CHAR(02)  ,
        g_yy                    CHAR(04)
        
DEFINE g_rec      RECORD
                  sie           SMALLINT        ,
                  id_ctasub     CHAR(04)        ,
                  ent           DECIMAL (22,6 ) ,
                  sal           DECIMAL (22,6 ) ,
                  neto          DECIMAL (22,6 ) 
                  END RECORD

DEFINE g_viv              ,
       g_ultimo   SMALLINT
       
#Var para Checar si existen o No Registros Cedente y Receptora.
DEFINE g_hayinf_rec_ced   CHAR(01)
DEFINE g_sumaregscedrec          ,
       g_numreg_recep            ,
       g_numreg_ced       SMALLINT

DEFINE g_prest_1erdhab    DATE    #JIRA INV-2283

#--- FIN DE DEFINICION DE VARIABLES
END GLOBALS

MAIN        

   OPTIONS INPUT WRAP,
   PROMPT LINE LAST,
   ACCEPT KEY CONTROL-I
   DEFER INTERRUPT

   CALL STARTLOG("ESTB110.log")


   CALL  init()
   CALL  habil_anterior( g_fecha_gen ) RETURNING  g_dia_ant
   
   CALL  act_n_envios()
   
   CALL  crea_tbls_temp()
   CALL  valida_folLiqRecep_Ced()


   DISPLAY " "
   DISPLAY "PROGRAMA: ESTB109 "
   DISPLAY "FECHA DE LA OPERACION QUE SE REPORTA  : ",g_dia_ant
   DISPLAY g_fecha_h,"INICIANDO PROCESO..."

   CALL  llena_det_64()
   CALL  llena_det_65()   
   CALL  Act_subcta_00()
   
   CALL  llena_tblfisiviv11_12()
   
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
       l_neto       DECIMAL ( 22,6 )

    
    LET g_nomarch    =    g_ruta.ruta_envio CLIPPED,"/",
                      --  g_fecha_env USING"YYYYMMDD",
                          g_fecha_gen USING"YYYYMMDD",
                          "_AF_",g_codigo_afore,"_000.1110"


  
    START REPORT rpt_1 TO g_nomarch

    LET g_txt  =
               "SELECT  FIRST 1 1 ",
               "FROM    det_64 ",
               "WHERE   id_ctasub  <> '0000' ",
               "AND     siefore    in (1,2,3,4,5,6) ", 
               "AND ( entradas <> 0  OR salidas <> 0 ) " 

    PREPARE idsql01 FROM g_txt
    EXECUTE idsql01
 
    IF SQLCA.SQLCODE       =     NOTFOUND   THEN    # NO HuBo OpEraCion 
       #---Total de Registros---#
        CASE  g_codigo_afore
           WHEN   564  #AFORE METLIFE ( CON SIEFORE ADICIONAL )
              LET g_num_regs      = 567 
           OTHERWISE   #DEMAS  AFORES
              LET g_num_regs      = 480 
        END CASE

       #* Imprime  Encabezado *
       OUTPUT TO REPORT rpt_1( 'E', 0, '    ',0,0,0 )
       #* Imprime  Detalles y Subencabezado *
          DECLARE cursor10 CURSOR FOR

                  SELECT * 
                  FROM   det_64    
                  WHERE  id_ctasub  <> "0000"
                  AND    siefore    in (1,2,3,4,5,6) 
                  ORDER BY siefore,id_ctasub

          FOREACH cursor10 INTO l_siefore    ,
                                l_id_ctasub  ,
                                l_entradas   ,
                                l_salidas    ,
                                l_neto

                  OUTPUT TO REPORT rpt_1( 'D',l_siefore,l_id_ctasub,l_entradas,
                                              l_salidas,l_neto )

          END FOREACH
    ELSE #Si hubo Operacion 
       #---Total de Registros---#
        CASE  g_codigo_afore
           WHEN   564  #AFORE METLIFE ( CON SIEFORE ADICIONAL )
              LET g_num_regs      = 567
           OTHERWISE   #DEMAS  AFORES
              LET g_num_regs      = 480 
        END CASE
         
        #* Imprime  Encabezado *
        OUTPUT TO REPORT rpt_1( 'E', 0, '    ',0,0,0 ) # SI HuBo OpEraCion
       
        #* Imprime  Detalles y Subencabezado *
        DECLARE cursor01 CURSOR FOR

                SELECT * 
                FROM   det_64    
                WHERE  id_ctasub  <> "0000"
                AND    siefore    in (1,2,3,4,5,6)  
                ORDER BY siefore,id_ctasub

        FOREACH cursor01 INTO l_siefore    ,
                              l_id_ctasub  ,
                              l_entradas   ,
                              l_salidas    ,
                              l_neto

                #-- SE INSERTARÀN LOS DATOS A LA TABLA FISICA CREADA safre_af:est_det_64 --#
                #-- SOLO SE INSERTARÀN LAS SIEFORES BÀSICAS (1,2,3,4,5,6)   --#
                
                
                
                #--VALIDA QUE SE INSERTEN ID CTAS ID SUBCUENTAS SOLO < CON MONTO >   --#
                    
                IF ( l_entradas <> 0   OR l_salidas <> 0  ) THEN                   
                	                
                   INSERT INTO safre_af:est_det_64 VALUES ( g_dia_ant            ,
                                                            g_ultimo             ,
                                                            l_siefore            ,
                                                            l_id_ctasub          ,
                                                            l_entradas           ,
                                                            l_salidas            ,
                                                            l_neto               ,
                                                            g_usuario )
                                                       
                END IF
                
                
                
                               
                OUTPUT TO REPORT rpt_1( 'D',l_siefore,l_id_ctasub,l_entradas,
                                            l_salidas,l_neto )
                                            

        END FOREACH

    END IF


    FINISH REPORT rpt_1              

END FUNCTION

REPORT rpt_1( tipo,r_siefore,r_id_ctasub,r_entradas_pes,
              r_salidas_pes,r_neto_pes )
#gr-------------------------

DEFINE r_siefore               SMALLINT,
       r_id_ctasub             CHAR(04),
       r_entradas_pes                  ,
       r_salidas_pes                   ,
       r_neto_pes         DECIMAL (16,2)

#---DEFINICION DE VARIABLES DE PESOS( SIEFORES BAS 1,2,3,4,5,6 ) "Y"
#---ACCCIONES( VIVIENDA SIEFORES 11 'o' 12 )---#

DEFINE
      tipo                      CHAR(1)

DEFINE 
       FormatoE_pes                        ,
       FormatoS_pes                        ,
       FormatoN_pes                CHAR(16),
       r_siefore_acc               SMALLINT,
       r_id_ctasub_acc             CHAR(04),
       r_entradas_acc                      ,
       r_salidas_acc                       ,
       r_neto_acc          DECIMAL ( 22,6 ),
       for_r_entradas_acc                  ,
       for_r_salidas_acc                   ,
       for_r_neto_acc      DECIMAL ( 22,2 ),
       FormatoE_viv                        ,
       FormatoS_viv                        ,
       FormatoN_viv                 CHAR(20)

DEFINE 
      entidad_sief            SMALLINT,
      tipo_entidad                    ,
      subtipo_ent             CHAR(03)


   OUTPUT
      PAGE LENGTH   1
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      ORDER EXTERNAL BY r_siefore

   FORMAT

   ON EVERY ROW
      IF( tipo = 'E' ) THEN    #* Encabezado *
         PRINT
           COLUMN 001,"000",
           COLUMN 004,"1110",
           COLUMN 008,"001",
           COLUMN 011,g_codigo_afore,
           COLUMN 014,g_dia_ant  USING"YYYYMMDD",
           COLUMN 022,"070",
           COLUMN 025,g_num_regs USING"&&&&&&&&&&&&&&&&&&&&&&&&&&",
           COLUMN 051,20 SPACES
      ELSE                #* Detalle    *
         LET r_entradas_pes   = r_entradas_pes * 100
         LET r_salidas_pes    = r_salidas_pes  * 100
         LET r_neto_pes       = r_neto_pes     * 100

                            #Formateo a 16 Caracteres#
         LET FormatoE_pes = '&&&&&&&&&&&&&&&&'       #* Cantidades Positivas *
         LET FormatoS_pes = '&&&&&&&&&&&&&&&&'       #* Cantidades Negativas *
         LET FormatoN_pes = '&&&&&&&&&&&&&&&&'       #* Cantidades Posi-Negs *

         IF( r_neto_pes  < 0 ) THEN
            LET FormatoN_pes[1] = '-'                #* Cantidades Negativas *
         END IF

         #--DETALLE 1: FLUJO DIARIO OPERATIVO --

         PRINT COLUMN 01, "301",                     #* Tipo de registro *
               COLUMN 04, r_id_ctasub USING "&&&&" , #* IdCta Id SubCta  *
               COLUMN 08, r_entradas_pes USING FormatoE_pes, #* Entradas
               COLUMN 24, r_salidas_pes  USING FormatoS_pes, #* Salidas
               COLUMN 40, r_neto_pes     USING FormatoN_pes, #* Neto
               COLUMN 56, 15 SPACES
         
      END IF

   BEFORE GROUP OF r_siefore

      IF( tipo <> 'E' ) THEN
          
            CASE r_siefore

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
            COLUMN 13, 58 SPACES
      END IF

      ON LAST ROW    #IMPRIME LA PARTE DE VIVIENDA

          LET    r_siefore_acc          =         NULL
          LET    r_id_ctasub_acc        =         NULL
          LET    r_entradas_acc         =         0
          LET    r_salidas_acc          =         0
          LET    r_neto_acc             =         0
          LET    for_r_entradas_acc     =         0
          LET    for_r_salidas_acc      =         0
          LET    for_r_neto_acc         =         0


          DECLARE cur10 CURSOR FOR

          SELECT *
          FROM   det_64 A
          WHERE  A.id_ctasub  <> "0000"          
          AND    A.siefore         =           g_viv    --issa

          ORDER BY siefore,id_ctasub

          FOREACH cur10    INTO r_siefore_acc    ,
                                r_id_ctasub_acc  ,
                                r_entradas_acc   ,
                                r_salidas_acc    ,
                                r_neto_acc
             
                          
             LET for_r_entradas_acc   = r_entradas_acc * 1000000
             LET for_r_salidas_acc    = r_salidas_acc  * 1000000
             LET for_r_neto_acc       = r_neto_acc     * 1000000


                            #Formateo a 20 Caracteres#
             LET FormatoE_viv = '&&&&&&&&&&&&&&&&&&&&' #* Cantidades Positivas *
             LET FormatoS_viv = '&&&&&&&&&&&&&&&&&&&&' #* Cantidades Negativas *
             LET FormatoN_viv = '&&&&&&&&&&&&&&&&&&&&' #* Cantidades Posi-Negs *

             IF( r_neto_acc    < 0 ) THEN
                LET FormatoN_viv[1] = '-'              #* Cantidades Negativas *
             END IF

             #--DETALLE 2: FLUJO DIARIO OPERATIVO --

             PRINT COLUMN 01, "302",                       #* Tipo de registro *
                   COLUMN 04, r_id_ctasub_acc USING "&&&&" ,#* IdCta Id SubCta*
                   COLUMN 08, for_r_entradas_acc USING FormatoE_viv, #* Entradas
                   COLUMN 28, for_r_salidas_acc  USING FormatoS_viv, #* Salidas
                   COLUMN 48, for_r_neto_acc     USING FormatoN_viv, #* Neto
                   COLUMN 68, 3 SPACES


            LET    r_siefore_acc          =         NULL
            LET    r_id_ctasub_acc        =         NULL
            LET    r_entradas_acc         =         0
            LET    r_salidas_acc          =         0
            LET    r_neto_acc             =         0
            LET    for_r_entradas_acc     =         0
            LET    for_r_salidas_acc      =         0
            LET    for_r_neto_acc         =         0

         END FOREACH

END REPORT
 
  
FUNCTION init()
#i-----------------------

   LET g_fecha_env         =      TODAY 
   LET g_fecha_gen         =      ARG_VAL(1) #Parametro Rec del Prog ESTB109.4gl
   LET g_fecha_h           =      CURRENT

   LET g_viv               =      11

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
   
   
   LET g_prest_1erdhab           =               MDY( MONTH(g_fecha_env),1,YEAR(g_fecha_env) )#JIRA INV-2283
   

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
                                  entradas           DECIMAL ( 22,6 ), 	
                                  salidas            DECIMAL ( 22,6 ),
                                  neto               DECIMAL ( 22,6 ) )
                                  
          CASE  g_codigo_afore
             WHEN  564 #564 AFORE METLIFE ( CON SIEFORE ADICIONAL )
                LET     l_fin           =    6
             OTHERWISE #DEMAS AFORES ,CON AFORES QUE SOLO TIENEN 5 SIEFORES
                LET     l_fin           =    5
          END CASE

          FOR     i    =      1   TO   l_fin

             INSERT INTO det_64 VALUES  ( i,"0100",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0101",0,0,0 )            
             INSERT INTO det_64 VALUES  ( i,"0103",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0104",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0107",0,0,0 )               
             INSERT INTO det_64 VALUES  ( i,"0109",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0110",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0111",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0112",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0119",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0120",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0121",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0122",0,0,0 )   

             INSERT INTO det_64 VALUES  ( i,"0200",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0201",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0202",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0203",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0206",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"0207",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0208",0,0,0 )  
	           INSERT INTO det_64 VALUES  ( i,"0217",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0218",0,0,0 )   
	        
             

             INSERT INTO det_64 VALUES  ( i,"0400",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0401",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0402",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0403",0,0,0 ) 
             INSERT INTO det_64 VALUES  ( i,"0404",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0405",0,0,0 )   

             INSERT INTO det_64 VALUES  ( i,"0500",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0501",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0502",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0503",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0504",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0505",0,0,0 ) 
	           INSERT INTO det_64 VALUES  ( i,"0510",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0511",0,0,0 ) 

             INSERT INTO det_64 VALUES  ( i,"0600",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0601",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0602",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0603",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0604",0,0,0 )   

             INSERT INTO det_64 VALUES  ( i,"0800",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0801",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0803",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0804",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0805",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0806",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0807",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0808",0,0,0 )
	           INSERT INTO det_64 VALUES  ( i,"0811",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0816",0,0,0 )	     


             INSERT INTO det_64 VALUES  ( i,"0900",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0901",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"0902",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"0903",0,0,0 )     


             INSERT INTO det_64 VALUES  ( i,"1000",0,0,0 )
             INSERT INTO det_64 VALUES  ( i,"1002",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1003",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1004",0,0,0 )   

             INSERT INTO det_64 VALUES  ( i,"1100",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"1101",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1102",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1103",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"1104",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1105",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"1106",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1107",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1108",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"1109",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1110",0,0,0 )
	           INSERT INTO det_64 VALUES  ( i,"1114",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1115",0,0,0 )
	     

             INSERT INTO det_64 VALUES  ( i,"1200",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1201",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1202",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"1203",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1204",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1205",0,0,0 )   

             INSERT INTO det_64 VALUES  ( i,"1300",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1301",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1302",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1303",0,0,0 )   
             INSERT INTO det_64 VALUES  ( i,"1304",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"1305",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"1306",0,0,0 )  
             INSERT INTO det_64 VALUES  ( i,"1307",0,0,0 ) 

          END FOR

           --SE INSERTA LOS ID CTA Y SUBCTA DE VIVIENDA  11 y 12--  
                             
           FOR   i       =      11   TO   12    
           	  
          #LET   i       =         g_viv
              
              INSERT INTO det_64 VALUES  ( i,"0100",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"0113",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0114",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0115",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0116",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0117",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0118",0,0,0 )
              
              INSERT INTO det_64 VALUES  ( i,"0200",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"0209",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0210",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0211",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0219",0,0,0 )
              
              
              INSERT INTO det_64 VALUES  ( i,"0500",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"0506",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0507",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0508",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0509",0,0,0 )
              
              INSERT INTO det_64 VALUES  ( i,"0800",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"0810",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0812",0,0,0 ) 
              INSERT INTO det_64 VALUES  ( i,"0813",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0814",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0815",0,0,0 )
              
              INSERT INTO det_64 VALUES  ( i,"0900",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"0904",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"0905",0,0,0 )
              
              INSERT INTO det_64 VALUES  ( i,"1000",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"1005",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1006",0,0,0 )
              
              INSERT INTO det_64 VALUES  ( i,"1100",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"1111",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1112",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1113",0,0,0 )
              
              INSERT INTO det_64 VALUES  ( i,"1200",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"1207",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1208",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1209",0,0,0 )
              
              INSERT INTO det_64 VALUES  ( i,"1300",0,0,0 ) #Se lo puse yo
              INSERT INTO det_64 VALUES  ( i,"1308",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1309",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1310",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1311",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1312",0,0,0 )
              INSERT INTO det_64 VALUES  ( i,"1313",0,0,0 )

           END  FOR
           	
END FUNCTION

FUNCTION llena_det_64()
#lld6---------------------

   #    ---EN ESTA PARTE SE INSERTA LAS SIFORES  BÁSICAS DE LA 1 A LA 6
   
   
   #01    00    RECAUDACION     RECAUDACION       RECAUDACION   RECAUDACION
   
   #01 01 RCV (Cuotas Obrero-patronales)
   #01 02 Voluntarias y complementarias  patronal 
   #01 03 Cuotas gubernamentales (Cuota Social,Estatal, etc)
   #01 04 Intereses en tránsito
 
   CALL inicializa()
   
      INITIALIZE g_txt  TO NULL
      
      CASE  g_codigo_afore
   
         WHEN 800  # AFORE FICTICIA
            
         OTHERWISE # DE+ AFORES    
   
            #01 01 RCV (Cuotas Obrero-patronales)
            #01 02 Voluntarias y complementarias  patronal 
            #01 03 Cuotas gubernamentales (Cuota Social,Estatal, etc)
            #01 04 Intereses en tránsito
            
            #Antes 0 Dispersion    Ordinaria  Ahora  ident_arch  =   0  con ident_separacion  =  '  '                  
            #Antes 1 Aclaraciones  Ordinarias Ahora  ident_arch  =   0  con ident_separacion <>  '  '                  
            #Antes 2 Aclaraciones  Especiales Ahora  ident_arch  =   2  con ident_separacion <>  '  '                  
            #Antes 3 Dispersion    Asignacion         De Momento no Hay Cambios Definidos y al parecer no van a llegar 
            #Antes 4 Dispersion    Por Sep. de Ctas.  De Momento no Hay Cambios Definidos y al parecer no van a llegar 
            
            LET g_txt    =
   
               "SELECT  A.siefore, ",             
                   "CASE WHEN A.subcuenta IN ( 1,2 ) AND A.tipo_movimiento IN ( 1,2,3,4 ) ",
                        "AND  B.ident_arch = 0  AND A.id_aportante <> 'BANXICO' ",
                        "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                        "              WHERE A.folio =  C.folio ",     
                        "              AND   A.nss   =  C.n_seguro ",                   
                        "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                        "              AND   C.ident_separacion = '  '  )",
                        "THEN '0101' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO
                        "WHEN A.subcuenta IN ( 5,6,9 )   AND A.tipo_movimiento IN ( 1,3 ) ",
                        "AND  B.ident_arch  = 1  AND A.id_aportante <> 'BANXICO' ",
                        "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                        "              WHERE A.folio =  C.folio ",     
                        "              AND   A.nss   =  C.n_seguro ",                   
                        "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                        "              AND   C.ident_separacion = '  '  )",
                        "THEN '0103' ",  
                        "ELSE '0000' ",
                        "END CASE , ",
                        "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS
                "FROM dis_cuenta A , dis_cza_aporte B ",
                "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   A.folio             = B.folio ",
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 "
   
      END  CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry1 FROM g_txt
   DECLARE c1 CURSOR FOR qry1
   
   FOREACH c1 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                   g_rec.sal
   
     CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH
   
   #----------------------------- 
   #01    04   INTERESES EN TRANSITO

   CALL inicializa()

      CASE  g_codigo_afore
      	
      	 WHEN 800  # AFORE FICTICIA
          
         OTHERWISE # DE+ AFORES  COPPEL  INVERCAP METLIFE JIRA MLM-2221 | Se Añade el segundo
                   # Qry del 0104 como esta en Coppel y Invercap
          	
          	 LET g_txt    =
              
                "SELECT  A.siefore, ",               
                "CASE WHEN A.subcuenta IN ( 1,2,5,6,9,3,11,15 ) AND A.tipo_movimiento = 3 ",#JIRA CPL-1332 Nuevo Modulo Intereses en Transito Individualizado.
                      "AND  A.id_aportante =  'BANXICO' ",
                      "     AND EXISTS ( SELECT 1 FROM dis_cza_iti D WHERE A.folio = D.folio )",
                      "THEN '0104' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Agrego Subcuenta 11                 
                      "WHEN A.subcuenta IN ( 1,2,3,5,6,9,11 )  AND A.tipo_movimiento IN ( 3,4 ) AND A.id_aportante <>  'BANXICO'  ",#AGREGUE EL T.M = 4 JIRA 1051
                      "    AND EXISTS ( SELECT 1 FROM dis_cza_aporte B  WHERE A.folio = B.folio  ",
                      "    AND  B.ident_arch  IN ( 0,1,2 )  ) ",
                      "    AND  EXISTS ( SELECT 1 FROM dis_det_interes C ",
                      "    WHERE A.folio =  C.folio ",
                      "    AND   A.consecutivo_lote =   C.consec_reg_lote  )",
                      "THEN '0104' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Agrego ident_arch = 0 
                     "ELSE '0000' ",
                     "END CASE , ",
                     "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS
                "FROM dis_cuenta A ",
                "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",          
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 "
            
      END CASE

      
   LET g_txt = g_txt CLIPPED
   PREPARE qry1530 FROM g_txt
   DECLARE c1530 CURSOR FOR qry1530

   FOREACH c1530 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #--------Qrys Nuevos Para la Nueva Version ---
   
   #01 06 Asignación                                           INACTIVO
   #01 07 Aclaraciones Cuotas  Obrero-patronales
   #01 08 Aclaraciones Voluntarias y Complementarias Patronal
   #01 09 Aclaraciones Cuotas Gubernamentales
   #01 10 Asignación RCV
   #01 11 Asignación Voluntarias y Complementarias Patronal
   #01 12 Asignación Cuotas Gubernamentales	

   CALL inicializa()
    
      CASE  g_codigo_afore
         
         WHEN 800  # AFORE FICTICIA    
         	
         OTHERWISE # DE+ AFORES COPPEL METLIFE INVERCAP
         
            #Se quitan los Intereses en el ID 01 09 NOV 2011
            #Se quitan los Intereses en el ID 01 07 NOV 2011
            
            # ident_arch  = 0   DISPERSION Antes 1,2 (Archivos de Aclaraciones)
          	# ident_separacion <> '  '  Aclaracion
          	 
          	#Antes 0 Dispersion    Ordinaria  Ahora  ident_arch  =   0  con ident_separacion  =  '  '                  
          	#Antes 1 Aclaraciones  Ordinarias Ahora  ident_arch  =   0  con ident_separacion <>  '  '                  
          	#Antes 2 Aclaraciones  Especiales Ahora  ident_arch  =   2  con ident_separacion <>  '  '                  
          	#Antes 3 Dispersion    Asignacion         De Momento no Hay Cambios Definidos y al parecer no van a llegar 
         	  #Antes 4 Dispersion    Por Sep. de Ctas.  De Momento no Hay Cambios Definidos y al parecer no van a llegar 
            
            LET g_txt    =

               "SELECT  A.siefore, ",
                    "CASE WHEN A.subcuenta IN ( 1,2 ) AND A.tipo_movimiento IN ( 1,2 ) ",
                         " AND  B.ident_arch IN ( 0,2 )  AND A.id_aportante <> 'BANXICO' ",
                         " AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                         "               WHERE A.folio =  C.folio ",     
                         "               AND   A.nss   =  C.n_seguro ",                   
                         "               AND   A.consecutivo_lote =   C.consec_reg_lote ",
                         "               AND   C.ident_separacion <> '  '  )",
                         "THEN '0107' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO                       
                         "WHEN A.subcuenta IN ( 5,6,9 )   AND A.tipo_movimiento = 1 ",
                         "AND  B.ident_arch IN ( 1,2 )  AND A.id_aportante <> 'BANXICO' ",
                         "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                         "               WHERE A.folio =  C.folio ",     
                         "               AND   A.nss   =  C.n_seguro ",                   
                         "               AND   A.consecutivo_lote =   C.consec_reg_lote ",
                         "               AND   C.ident_separacion <> '  '  )",
                         "THEN '0109' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Agrego ident_arch = 1 (Aclaraciones)
                         "WHEN A.subcuenta IN ( 1,2 ) AND A.tipo_movimiento IN ( 1,2,3,4 ) ",
                         "AND  B.ident_arch    = 3  AND A.id_aportante <>  'BANXICO' ",
                         "THEN '0110' ",
                         "WHEN A.subcuenta IN ( 3,11 ) AND A.tipo_movimiento IN ( 1 ) ",
                         "AND  B.ident_arch     =  3 AND A.id_aportante <>  'BANXICO' ",
                         "THEN '0111' ",
                         "WHEN A.subcuenta IN ( 5,6,9 ) AND A.tipo_movimiento IN ( 1,3 ) ",
                         "AND  B.ident_arch     =  3  AND A.id_aportante <>  'BANXICO' ",
                         "THEN '0112' ",
                         "ELSE '0000' ",
                         "END CASE , ",
                         "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS
               "FROM dis_cuenta A , dis_cza_aporte B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
                
      END  CASE 

   LET g_txt = g_txt CLIPPED
   PREPARE qry50 FROM g_txt
   DECLARE c50 CURSOR FOR qry50

   FOREACH c50 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                       g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH


   #QRYS NUEVOS      NUEVOS      NUEVOS       NUEVOS       NUEVOS     NUEVOS
   #01 19 ACR IMSS                                                                   
   #01 20 Voluntarias IMSS                                                           
   #01 21 Aclaraciones ACR IMSS                                                      
   #01 22 Aclaraciones Voluntarias IMSS                                              
   
   CALL inicializa()
   
      CASE  g_codigo_afore
   
         WHEN 564  # SOLO AFORE METLIFE
            
            LET g_txt    =
            
   	           #01 19 ACRIMSS
   	           
               #Se quitan Intereses del Id 01 Id 21 
               #Se quitan Intereses del Id 01 Id 22
               
               #Antes 0 Dispersion    Ordinaria  Ahora  ident_arch  =   0  con ident_separacion  =  '  '                  
               #Antes 1 Aclaraciones  Ordinarias Ahora  ident_arch  =   0  con ident_separacion <>  '  '                  
               #Antes 2 Aclaraciones  Especiales Ahora  ident_arch  =   2  con ident_separacion <>  '  '                  
               #Antes 3 Dispersion    Asignacion         De Momento no Hay Cambios Definidos y al parecer no van a llegar 
               #Antes 4 Dispersion    Por Sep. de Ctas.  De Momento no Hay Cambios Definidos y al parecer no van a llegar 
               
               "SELECT  A.siefore, ",
                    "CASE WHEN A.subcuenta = 11  AND A.tipo_movimiento IN ( 1,3 ) ",
                         "AND  B.ident_arch = 0  AND A.id_aportante <> 'BANXICO' ",
                         "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                         "              WHERE A.folio =  C.folio ",     
                         "              AND   A.nss   =  C.n_seguro ",                   
                         "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                         "              AND   C.ident_separacion = '  '  )",
                         "THEN '0119' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0
                         "WHEN A.subcuenta  = 3  AND A.tipo_movimiento  IN ( 1,3 ) ", 
                         "AND  B.ident_arch = 0 AND A.id_aportante <> 'BANXICO' ",
                         "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                         "              WHERE A.folio =  C.folio ",     
                         "              AND   A.nss   =  C.n_seguro ",                   
                         "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                         "              AND   C.ident_separacion = '  '  )",
                         "THEN '0120' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0
   		                  "WHEN A.subcuenta  =  11 AND A.tipo_movimiento  = 1 ", 
                         "AND  B.ident_arch IN ( 0,2 ) AND A.id_aportante <> 'BANXICO' ", 
                         "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                         "              WHERE A.folio =  C.folio ",     
                         "              AND   A.nss   =  C.n_seguro ",                   
                         "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                         "              AND   C.ident_separacion <> '  '  )",
                         "THEN '0121' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0 
                         "WHEN A.subcuenta  = 3  AND A.tipo_movimiento  = 1 ", 
                         "AND  B.ident_arch IN ( 0,2 ) AND A.id_aportante <> 'BANXICO' ", 
                         "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                         "              WHERE A.folio =  C.folio ",     
                         "              AND   A.nss   =  C.n_seguro ",                   
                         "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                         "              AND   C.ident_separacion <> '  '  )",
                         "THEN '0122' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0 
                         "ELSE '0000' ",
                         "END CASE , ",
                         "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS
                 "FROM dis_cuenta A , dis_cza_aporte B ",
                 "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
                 "AND   A.folio             = B.folio ",
                 "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                 "GROUP BY 1,2 "
   
         OTHERWISE # DE+ AFORES
   
            LET g_txt    =
             
   	           #01 19 ACRIMSS
               
               #Se quitan Intereses del Id 01 Id 21 
               #Se quitan Intereses del Id 01 Id 22 
               
               #Antes 0 Dispersion    Ordinaria  Ahora  ident_arch  =   0  con ident_separacion  =  '  '                  
               #Antes 1 Aclaraciones  Ordinarias Ahora  ident_arch  =   0  con ident_separacion <>  '  '                  
               #Antes 2 Aclaraciones  Especiales Ahora  ident_arch  =   2  con ident_separacion <>  '  '                  
               #Antes 3 Dispersion    Asignacion         De Momento no Hay Cambios Definidos y al parecer no van a llegar 
               #Antes 4 Dispersion    Por Sep. de Ctas.  De Momento no Hay Cambios Definidos y al parecer no van a llegar 
               
               
               "SELECT  A.siefore, ",
                   "CASE WHEN A.subcuenta = 11  AND A.tipo_movimiento = 1 ",
                        "AND  B.ident_arch = 0 ",
                        "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                        "              WHERE A.folio =  C.folio ",     
                        "              AND   A.nss   =  C.n_seguro ",                   
                        "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                        "              AND   C.ident_separacion = '  '  )",
                        "THEN '0119' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0
                        "WHEN A.subcuenta  = 3  AND A.tipo_movimiento  = 1 ",    
                        "AND  B.ident_arch = 0 ",
                        "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                        "              WHERE A.folio =  C.folio ",     
                        "              AND   A.nss   =  C.n_seguro ",                   
                        "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                        "              AND   C.ident_separacion = '  '  )",
                        "THEN '0120' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0
   		                  "WHEN A.subcuenta  = 11 AND A.tipo_movimiento  = 1 ",    
                        "AND  B.ident_arch IN ( 0,2 ) ",                             
                        "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                        "              WHERE A.folio =  C.folio ",     
                        "              AND   A.nss   =  C.n_seguro ",                   
                        "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                        "              AND   C.ident_separacion <> '  '  )",
                        "THEN '0121' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0 
                        "WHEN A.subcuenta  = 3  AND A.tipo_movimiento  = 1 ",    
                        "AND  B.ident_arch IN ( 0,2 ) ",                             
                        "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                        "              WHERE A.folio =  C.folio ",     
                        "              AND   A.nss   =  C.n_seguro ",                   
                        "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                        "              AND   C.ident_separacion <> '  '  )",
                        "THEN '0122' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0 
                        "ELSE '0000' ",
                        "END CASE , ",
                        "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS
               "FROM dis_cuenta A , dis_cza_aporte B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
   
      END  CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry750 FROM g_txt
   DECLARE c750 CURSOR FOR qry750
   
   FOREACH c750 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                   g_rec.sal
   
     CALL Act_tbl ( g_rec.* ) 
      
   END FOREACH 

   #----------------------
   
   #02 00  TRASPASOS TRASPASOS TRASPASOS TRASPASOS TRASPASOS TRASPASOS  
   #02 01  Traspasos ordinarios
   #02 02  Traspasos complementarios
   #02 03  Resarcimientos 

   CALL inicializa()

      INITIALIZE g_txt TO NULL
      
      CASE g_codigo_afore
      
          WHEN 568  # SOLO AFORE COPPEL
          	 	
          	 #NUM REQ CPL-757 
          	 #Para los TIPOS DE TRASPASOS = 71 (Subcuentas IMSS) TANTO COMO RECEPTORA COMO CEDENTE
          	 #ARCHIVOS NORMALES Y COMPLEMENTARIOS aunque el layout no especifica
          	 #en que id cta id subcuenta tiene que ir la AFORE SOLICITO QUE SE PINTE EN 0201 Y 0202
          	 
          	 #CPL-1782 Nov13 Se Agrega Tipo Traspaso 52 (TRASPASO A-A POR REASIGNACION) 
          	 #         Tipo de Movimiento 298
          	 
             LET g_txt    =
      
                "SELECT  A.siefore, ",
                     "CASE WHEN C.tipo_traspaso IN (1,4) ",       # 1=POR PROMOTOR | 4=DIVERSOS
                          "THEN '0201' ",
                          "WHEN C.tipo_traspaso  =  2 ",          # 2=COMPLEMENTARIO
                          "THEN '0202' ",
                     "ELSE '0000' ",
                     "END CASE ,0, ",
                     "NVL(SUM(A.monto_en_pesos),0) ",
                "FROM    dis_cuenta A , taa_cd_ctr_folio C ",#SALIDAS
                "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                  "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",
                                  "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                                  "AND   B.tipo_traspaso IN  ( 01,02,38,51,52,55,57,71,72,74 ) ) ",            #20 NOV2012 NUM. REQ. JIRA. CPL-1064              
                  "AND   A.subcuenta   IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS 
                  "AND   A.tipo_movimiento IN ( 220,271,290,295,297,293,294,272,248,298 ) ",   
                  "AND   A.folio             = C.folio ",
                  "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 ",        
                "UNION ",               
                #****  Query OPTIMIZADO Y PROBADO.**** 
                #-----  Modificacion CPL-757  Se cambia tabla donde            
                # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se  
                # Agrega Filtro A.consecutivo_lote  = C.cont_servicio
                "SELECT  A.siefore, ",
                    "CASE WHEN  C.ident_operacion  = '09' ", "AND C.tipo_traspaso IN  ( 01,02,38,55,71,72,74 )",    #20 NOV2012 NUM. REQ. JIRA. CPL-1064
                           "THEN '0201' ",
                          "WHEN C.ident_operacion  = '09' ", "AND   C.tipo_traspaso IN ( 51,57 ) ",                        
                               "AND   C.cve_ced_cuenta  NOT IN ( 517,531 ) ",
                          "THEN '0201' ",
                          "WHEN  C.ident_operacion  = '12' ","AND   C.tipo_traspaso IN  ( 01,02,38,55,71,72,74 ) ", #20 NOV2012 NUM. REQ. JIRA. CPL-1064
                          "THEN '0202' ",
                          "WHEN C.ident_operacion  = '12' ", "AND   C.tipo_traspaso IN ( 51,57 ) ",
                               "AND   C.cve_ced_cuenta  NOT IN ( 517,531 ) ",                
                          "THEN '0202' ",                                                    
                    "ELSE '0000' ", 
                    "END CASE , ",
                    "SUM(A.monto_en_pesos),0 ",
                "FROM  dis_cuenta  A , ",   #ENTRADAS  ENTRADAS RECEPTORA
                      "taa_rcv_recepcion C ",
                "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   A.subcuenta   IN ( 1,2,3,5,6,7,9,10,11,12,15,16, ",                   
                                        "17,18,20,21,22,23,24,25,26,27,28,29 ) ",            
                "AND   A.tipo_movimiento   =  1 ",                                           
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                      
                "AND   A.folio =  C.folio ",                                                 
                "AND   A.nss  = C.nss ",                                                     
                "AND   A.consecutivo_lote  = C.cont_servicio ",                              
                "GROUP BY 1,2 ",  
                "UNION ",
                "SELECT   A.siefore, ",
                         "'0203', ",
                          "NVL(SUM(A.monto_en_pesos),0),0 ",
                 "FROM    dis_cuenta  A ",#RESARCIMIENTO  ENTRADA
                 "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                   "AND   A.tipo_movimiento    = 779 ",
                   "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 ",           
                "UNION ",
                "SELECT  A.siefore, ",                                                 
                   "CASE WHEN A.subcuenta IN ( 1,2,3,5,6,7,9,10,11,12,15,16, ",#Todas las Subctas  ",
                                           "17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ",
                          "AND A.tipo_movimiento     = 225 ",                  #Tipo de Traspaso = 21  
                          "THEN '0203' ",                                               
                          "WHEN  A.subcuenta IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22 ",
                                              ",23,24,25,26,27,28,29 ) ",      #Puras Sucuentas IMSS
                          "AND A.tipo_movimiento     = 273 ",                  #Tipo de Traspaso = 73  
                          "THEN '0203' ", 
                   "END CASE , ",
                   "0,NVL(SUM(A.monto_en_pesos),0) ",                          #SALIDAS REV TRA INDEBIDOS                                       
                "FROM    dis_cuenta  A ,taa_cd_ctr_folio C ",                                                              
                "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                       
                "AND     A.folio               = C.folio ",                                                                  
                "AND     C.tipo_traspaso       =  2 ",                         #Complementarios    
                "AND     A.tipo_movimiento     IN(  225,273 ) ",               #Tipo de Traspaso = 21 y Tipo de Traspaso = 73                                              
                "AND     A.subcuenta   IN ( 1,2,3,5,6,7,9,10,11,12,15,16, ",   #Todas las Subctas                                             
                                        "17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ",                                          
                "AND   A.siefore     IN ( 1,2,3,4,5,6 ) ",                                                                 
                "GROUP BY 1,2 "                                                                                          
          
          WHEN 562  # SOLO AFORE INVERCAP
          	  
          	 #-- Modificaciòn de Acuerdo al Jira INV-1183
          	 
          	 LET g_txt    =
             
                "SELECT  A.siefore, ",
                     "CASE WHEN C.tipo_traspaso IN (1,4) ",# 1=POR PROMOTOR | 4=DIVERSOS
                          "THEN '0201' ",
                          "WHEN C.tipo_traspaso  =  2 ",    # 2=COMPLEMENTARIO
                          "THEN '0202' ",
                     "ELSE '0000' ",
                     "END CASE ,0, ",
                     "NVL(SUM(A.monto_en_pesos),0) ",
                "FROM    dis_cuenta A , taa_cd_ctr_folio C ",#SALIDAS
                "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                  "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",
                                  "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                                  "AND   B.tipo_traspaso IN  ( 01,02,38,55,57  ) ) ",                
                  "AND   A.subcuenta   IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS 
                  "AND   A.tipo_movimiento IN ( 220,257,295,293,294 ) ",   
                  "AND   A.folio             = C.folio ",
                  "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 ", 
                "UNION ", #JIRA INV-1504     Modificacion 21 Septiembre del 2012                              
                "SELECT  A.siefore, ",                                                       
                    "CASE WHEN  C.ident_operacion  = '09' ",                  #NORMAL        
                               "AND C.tipo_traspaso IN  ( 01,02,38,55,57 )",                    
                               "AND C.cve_ced_cuenta NOT IN ( 517,531 ) ",    #Modificacion JIRA 777 Agosto 2013 se discrimina
                          "THEN '0201' ",                                     #Claves de Prestadora para que no las lea se leeran en el 0217
                          "WHEN  C.ident_operacion  = '12' ",                 #COMPLEMENTARIO
                               "AND   C.tipo_traspaso IN  ( 01,02,38,55,57 ) ", 
                               "AND C.cve_ced_cuenta NOT IN ( 517,531 ) ",                
                          "THEN '0202' ",                                                    
                    "ELSE '0000' ",                                                          
                    "END CASE , ",                                                           
                    "SUM(A.monto_en_pesos),0 ",                                              
                "FROM  dis_cuenta  A , ",   #ENTRADAS  ENTRADAS RECEPTORA                    
                      "taa_rcv_recepcion C ",                                                
                "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                           
                "AND   A.subcuenta   IN ( 1,2,3,5,6,7,9,10,11,12,15,16, ",                   
                                        "17,18,20,21,22,23,24,25,26,27,28,29 ) ",            
                "AND   A.tipo_movimiento   =  1 ",                                           
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                      
                "AND   A.folio =  C.folio ",                                                 
                "AND   A.nss  = C.nss ",                                                     
                "AND   A.consecutivo_lote  = C.cont_servicio ",                              
                "GROUP BY 1,2 ",  
                "UNION ",
                "SELECT   A.siefore, ",
                         "'0203', ",
                          "NVL(SUM(A.monto_en_pesos),0),0 ",
                 "FROM    dis_cuenta  A ",#RESARCIMIENTO  ENTRADA
                 "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                   "AND   A.tipo_movimiento    = 779 ",
                   "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 "              
          	
          WHEN 564  # SOLO AFORE METLIFE #MODIFICACIÓN  JIRA MLM-1086 71,74
          	                             #A petición de la Afore se Agrega 
          	                             #tipo_traspaso 71,74 = T.M CED 271,248 
          	                          
          	                             #MLM-2745  S O L O    C E D E N T E  Julio - Ago                              
          	                             #          Se Agregá el Tipo de traspaso 72 (INDEB.  POR CURP-PROMOTOR)       
          	                             #          Asi como la Subcuenta 37( APORTACIONES LARGO PLAZO VENT 218 )      
          	                             #          02 01  Traspasos Ordinarios                                        
          	                             #          02 02  Traspasos Complementarios                                   
          	                                      
          	  LET g_txt    =                      
      
                 "SELECT  A.siefore, ",
                      "CASE WHEN C.tipo_traspaso IN (1,4) ",# 1=POR PROMOTOR | 4=DIVERSOS
                           "THEN '0201' ",
                           "WHEN C.tipo_traspaso  =  2 ",    # 2=COMPLEMENTARIO
                           "THEN '0202' ",
                      "ELSE '0000' ",
                      "END CASE ,0, ",
                      "NVL(SUM(A.monto_en_pesos),0) ",
                 "FROM    dis_cuenta A , taa_cd_ctr_folio C ",#SALIDAS  C E D E N T E
                 "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                   "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",
                                   "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                                   "AND   B.tipo_traspaso IN  ( 01,02,38,55,71,72,74 ) ) ",                
                   "AND   A.subcuenta   IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37 ) ",#PURAS SUBCTA IMSS 
                   "AND   A.tipo_movimiento IN ( 220,295,293,294,271,272,248 ) ",   
                   "AND   A.folio             = C.folio ",
                   "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                 "GROUP BY 1,2 ",       
                 "UNION ", #MLM-2745 Se Agrego el Tipo de Traspasos 72 y Subcuenta 37             
                 "SELECT  A.siefore, ", 
                         "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",
                              "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ", #NORMAL
                              "AND   C.tipo_traspaso IN  ( 01,02,38,55,71,72,74 ) AND A.nss  = C.nss  ", 
                              "AND   A.consecutivo_lote  = C.cont_servicio ) ",
                              "THEN '0201' ",
                              "WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",
                              "WHERE A.folio =  C.folio AND C.ident_operacion  = '12' ", #COMPLEMENTARIO   
                              "AND   C.tipo_traspaso IN  ( 01,02,38,55,71,72,74 )  AND A.nss  = C.nss ",
                              "AND   A.consecutivo_lote  = C.cont_servicio ) ",
                              "THEN '0202' ",
                        "ELSE '0000' ",
                        "END CASE , ",
                        "NVL(SUM(A.monto_en_pesos),0),0 ",
                 "FROM   dis_cuenta  A ",#ENTRADAS  ENTRADAS RECEPTORA
                 "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                 "AND     A.subcuenta   IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37 ) ",#PURAS SUBCTA IMSS
                   "AND   A.tipo_movimiento   =  1 ",
                   "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                 "GROUP BY 1,2 ",
                 "UNION ",
                 "SELECT   A.siefore, ",
                          "'0203', ",
                           "NVL(SUM(A.monto_en_pesos),0),0 ",
                  "FROM    dis_cuenta  A ",#RESARCIMIENTO  ENTRADA
                  "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                    "AND   A.tipo_movimiento    = 779 ",
                    "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
                 "GROUP BY 1,2 "
          		
          OTHERWISE # DE+ AFORES
             
      
      END CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry2 FROM g_txt
   DECLARE c2 CURSOR FOR qry2

   FOREACH c2 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                   g_rec.sal

     CALL Act_tbl ( g_rec.* )

   END FOREACH 

   #--------Qrys Nuevos---  

   #0205 Aclaraciones                           Inactiva
   #0206 Reverso de Traspasos Indebidos
   #0207 Devolucion de Comisiones <NO HAY SALIDAS> ,<NO HAY ENTRADAS>---
   #0208 Traspasos   SAR 92 (IMSS-Afore)


   CALL inicializa() 
   
      CASE  g_codigo_afore
      
         WHEN 800  # AFORE FICTICIA        
         
         WHEN 568  # SOLO AFORE COPPEL
         	
         	  LET g_txt    =
         	                                                      #16NOV2012 NUM. REQ. JIRA. CPL-1064
         	     "SELECT   A.siefore, ",                          #JIRA SOLIC.  CPL-920 folios complementarios que es donde viajan los resarcimientos
                       "'0206', ",                                   
                       "0,NVL(SUM(A.monto_en_pesos),0) ",       #SALIDAS  CEDENTE     
               "FROM    dis_cuenta  A ,taa_cd_ctr_folio C ",         
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",  
               "AND     A.folio               = C.folio ",      #TODAS LAS SUBCUENTAS     
               "AND     C.tipo_traspaso       <>  2 ",          #TRASPASO NORMALES     
               "AND     A.tipo_movimiento   IN ( 225,273 ) ",   #TIPO  TRASPASO  21 Y 73             
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",    
               "GROUP BY 1,2 ",       
               "UNION ",              
               "SELECT  A.siefore, ",  
                       "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",
                            "WHERE A.folio =  C.folio AND C.ident_operacion IN ( '09', '12' ) ",
                            "AND C.tipo_traspaso IN ( 21,73 )  AND A.nss  =  C.nss  ",#20 NOV2012 NUM. REQ. JIRA. CPL-1064
                            "AND A.consecutivo_lote  = C.cont_servicio ) ",
                            "THEN '0206' ",#ENTRADAS REV TRA INDEBIDOS
                       "ELSE '0000' ",
                       "END CASE , ",
                       "NVL(SUM(A.monto_en_pesos),0),0 ",
                   "FROM   dis_cuenta  A ", #TODAS LAS SUBCUENTAS
                  "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                    "AND   A.tipo_movimiento   =  1 ",
                    "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                  "GROUP BY 1,2 ",
                "UNION ",
               "SELECT    A.siefore, ",
               "'0208', ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS TRA SAR 92 IMSS
               "FROM     dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta          IN   ( 7 ) ",
               "AND       A.tipo_movimiento    IN   ( 1,4 ) ",
               "AND       A.id_aportante      MATCHES '[CT]I-*' ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
       
         WHEN 562  # SOLO AFORE INVERCAP
         	
            LET g_txt    =           
              
               "SELECT   A.siefore, ",                               
                        "'0206', ",                                  
                        "0,NVL(SUM(A.monto_en_pesos),0) ",           #SALIDAS REV TRA INDEBIDOS
               "FROM    dis_cuenta  A ,taa_cd_ctr_folio C ",         
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",   #tipo_traspaso   21  225 
               "AND     A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ", # TODAS LAS SUBCUENTAS                      
               "AND     A.tipo_movimiento  IN ( 225,227 ) ",          #tipo_traspaso   23  227                        
               "AND     A.folio            = C.folio ",               #tipo_traspaso   22  225 ????            
               "AND     A.siefore          IN ( 1,2,3,4,5,6 ) ",      
               "GROUP BY 1,2 ",        
               "UNION ",    
               "SELECT A.siefore, '0206', ",                                                                                   
               "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS  RECEPTORA   QRY SUPER OPTIMIZADO                                                               
               "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                    
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
               "AND   A.folio =  C.folio ",                                                                                    
               "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ",#TODAS LAS SUBCTS
               "AND   A.tipo_movimiento = 1 ",                                                                                 
               "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                   
               "AND   C.tipo_traspaso IN ( 21,22,23 ) ",                                                                          
               "AND   A.nss  = C.nss ",   
               "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                     
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                          
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT    A.siefore, ",
               "'0208', ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS TRA SAR 92 IMSS
               "FROM     dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta          IN   ( 7 ) ",
               "AND       A.tipo_movimiento    IN   ( 1,4 ) ",
               "AND       A.id_aportante      MATCHES '[CT]I-*' ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
              
         OTHERWISE # METLIFE
         
             #MLM-2644 Se Incorporara el Tipo de Traspaso 73 Tipo Movimiento
             #         273.
      
             LET g_txt    =           
             
                "SELECT   A.siefore, ",                               
                         "'0206', ",                                  
                         "0,NVL(SUM(A.monto_en_pesos),0) ",           #SALIDAS REV TRA INDEBIDOS
                "FROM    dis_cuenta  A ,taa_cd_ctr_folio C ",         
                "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",  
                "AND   A.folio               = C.folio ",             
                "AND   A.tipo_movimiento     IN ( 225,273 ) ",#MLM-2644 Se Incorporo el Tipo de Movimiento 273                 
                "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",      
                "GROUP BY 1,2 ",        # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde 
                "UNION ",               # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se      
                "SELECT  A.siefore, ",  # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
                        "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",
                             "WHERE A.folio =  C.folio AND C.ident_operacion IN ( '09', '12' ) ",
                             "AND C.tipo_traspaso IN ( 21,73 )  AND A.nss  =  C.nss  ",#MLM-2644 Se Incorporo el Tipo de Movimiento 273                 
                             "AND A.consecutivo_lote  = C.cont_servicio ) ",
                             "THEN '0206' ",#ENTRADAS REV TRA INDEBIDOS
                        "ELSE '0000' ",
                        "END CASE , ",
                        "NVL(SUM(A.monto_en_pesos),0),0 ",
                    "FROM   dis_cuenta  A ",
                   "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                     "AND   A.tipo_movimiento   =  1 ",
                     "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                   "GROUP BY 1,2 ",
                 "UNION ",
                "SELECT    A.siefore, ",
                "'0208', ",
                "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS TRA SAR 92 IMSS
                "FROM     dis_cuenta  A ",
                "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND       A.subcuenta          IN   ( 7 ) ",
                "AND       A.tipo_movimiento    IN   ( 1,4 ) ",
                "AND       A.id_aportante      MATCHES '[CT]I-*' ",
                "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 "
      
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry51 FROM g_txt
   DECLARE c51 CURSOR FOR qry51
   
   FOREACH c51 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                      g_rec.sal
   
        CALL Act_tbl ( g_rec.* )
   
   END FOREACH

   #02 17 Asignacion de Cuentas   QRYS NUEVOS NUEVOS NUEVOS NUEVOS
   #      Se esta Desarrollando A finales de Noviembre se estaría Liquidando PACO CPL-693
          
   CALL inicializa() 
       
      #---Inicializa Variables --#

      INITIALIZE   g_dd   TO NULL
      
      #--                      --#
    
      LET g_dd       =  DAY(g_dia_ant)   USING "&&" 

      CASE  g_codigo_afore
         
         #TIPO DE TRASPASO 51 TRASPASO A-A   POR ASIGNACION DE CUENTAS
         #TIPO DE TRASPASO 57 TRASPASO A-A   POR ASIGNACION ATRAVES DE INTERNET
         #TIPO DE TRASPASO = 1 POR PROMOTOR  TIPO DE TRASPASO  = 2 COMPLEMENTARIO   TIPO DE TRASPASO = 4 DIVERSOS
         
         #-----  E N T R A D A S  DE :  PRESTADADORA  A:  < AFORE >   -------#
         #SE REGISTRA COMO ENTRADA LOS MONTOS TRANSFERIDOS DE LAS CUENTAS INDIVIDUALES 
         #ASIGNADAS, PROVENIENTES DE LA PRESTADORA DE SERVICIOS.   
         
         #----- S A L I D A S   DE :   AFORE   A: PRESTADORA DE SERVICIOS -------#
         #SE REGISTRA COMO SALIDA LAS CUENTAS ASIGNADAS INACTIVAS
         #CUYO MONTO DE DEVOLVERÁ A BANCO DE MEXICO.
         
                  	
         WHEN 562  # SOLO AFORE INVERCAP
          	       # JIRA INV-1504 Modificacion 21 Septiembre del 2012
          	       # JIRA INV-2283 Agosto del 2013
          	        
          	IF ( g_dd = '01' OR
          		   g_dd = '02' OR
          		   g_dd = '03' OR
          		   g_dd = '04' OR
          		   g_dd = '05'   ) THEN #SE  LIQUIDA PRESTADORA PRIMER DÍA HABIL DEL MES
          		   	                    #PRESTADORA  SOLO RECEPTORA.
               LET g_txt    =  	
          	    
          	      "SELECT  A.siefore, ",
                     "CASE WHEN C.tipo_traspaso IN ( 2,4 ) ",
                        "THEN '0217' ",                   
                        "ELSE '0000' ",
                        "END CASE ,0, ",
                        "NVL(SUM(A.monto_en_pesos),0) ",                                
                  "FROM    dis_cuenta A , taa_cd_ctr_folio C ",     #SALIDAS   SUPER OPTIMIZADO 
                  "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                  "AND     A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ", #TODAS LAS SUBCUENTAS
                  "AND     A.tipo_movimiento   =  290  ",                          
                  "AND     EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                                 "WHERE A.tipo_movimiento  =  B.marca_cod  ",
                                 "AND   B.tipo_traspaso = 51 ) ",
                  "AND    A.folio             = C.folio ",                                     
                  "AND    A.siefore  IN ( 1,2,3,4,5,6 ) ",
                  "GROUP BY 1,2 ",               
                  "UNION ",# JIRA INV-777  Modificación Agosto 2013. 
                  "SELECT A.siefore, '0217', ",                                                                                   
                  "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS    RECEPTORA JUAN  SUPER OPTIMIZADO                                                               
                  "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                    
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",     
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 )  ",#PURAS SUBCTA IMSS                                                         
                  "AND   A.tipo_movimiento = 1 ",                                                                                 
                  "AND   A.folio =  C.folio ",                                                                                                              
                  "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                   
                  "AND   C.tipo_traspaso  IN ( 51,01,02,38,55,57 ) ",                                                                          
                  "AND   C.cve_ced_cuenta IN ( 517,531 ) ",#SOLO SE EXTRAERÁ INFORMACIÓN DE LA PRESTADORA DE SERVICIOS.
                  "AND   A.nss  = C.nss ",   
                  "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                     
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                          
                  "GROUP BY 1,2 "      
             
            ELSE#NO SE LIQUIDO PRESTADORA ENTONCES SE REPORTA AFORE-AFORE NORMAL.
            	
               LET g_txt    =  	
          	    
          	      "SELECT  A.siefore, ",
                     "CASE WHEN C.tipo_traspaso IN ( 2,4 ) ",
                        "THEN '0217' ",                   
                        "ELSE '0000' ",
                        "END CASE ,0, ",
                        "NVL(SUM(A.monto_en_pesos),0) ",                                
                  "FROM    dis_cuenta A , taa_cd_ctr_folio C ",     #SALIDAS   SUPER OPTIMIZADO 
                  "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                  "AND     A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ", #TODAS LAS SUBCUENTAS
                  "AND     A.tipo_movimiento   =  290  ",                          
                  "AND     EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                                 "WHERE A.tipo_movimiento  =  B.marca_cod  ",
                                 "AND   B.tipo_traspaso = 51 ) ",
                  "AND    A.folio             = C.folio ",                                     
                  "AND    A.siefore  IN ( 1,2,3,4,5,6 ) ",
                  "GROUP BY 1,2 ",               
                  "UNION ",# JIRA-2283  Modificación Agosto 2013. 
                  "SELECT A.siefore, '0217', ",                                                                                   
                  "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS    RECEPTORA JUAN  SUPER OPTIMIZADO                                                               
                  "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                    
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",     
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 )  ",#PURAS SUBCTA IMSS                                                         
                  "AND   A.tipo_movimiento = 1 ",                                                                                 
                  "AND   A.folio =  C.folio ",                                                                                                              
                  "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                   
                  "AND   C.tipo_traspaso = 51 ",
                  "AND   A.nss  = C.nss ",   
                  "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                     
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                          
                  "GROUP BY 1,2 "
            	
            END IF#FIN DE SE LIQUIDO O NO PRESTADORA.
             
         WHEN 564  # SOLO AFORE METLIFE
          	
          	       #MODIFICACION 14JUN2012 JIRA MLM-1086 SE COMENTO ESTA LINEA
          	       #"AND   A.id_aportante[1,6]  MATCHES  'TCREA-' ",       
          	 
          	LET g_txt    =  	
          	
          	  "SELECT  A.siefore, ",
                 "CASE WHEN C.tipo_traspaso IN ( 2,4 ) ",
                    "THEN '0217' ",                   
                    "ELSE '0000' ",
                    "END CASE ,0, ",
                    "NVL(SUM(A.monto_en_pesos),0) ",                                
              "FROM    dis_cuenta A , taa_cd_ctr_folio C ",     
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
              "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                             "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                             "AND   B.tipo_traspaso IN ( 51,57 ) ) ",
              "AND   A.tipo_movimiento IN ( 290,297 ) ",             
              "AND   A.folio             = C.folio ",             
             #"AND   A.id_aportante[1,6]  MATCHES  'TCREA-' ",       
              "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
              "GROUP BY 1,2 ",         
              "UNION ",                    #MODIFICACION 14JUN2012 JIRA MLM-1086 SE COMENTO ESTA LINEA             
              "SELECT  A.siefore, ",       # AND   C.cve_ced_cuenta IN ( 517,531 )  
                      "CASE WHEN EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",
                           "WHERE  A.folio =  C.folio AND C.ident_operacion  IN ( '09','12' ) ",
                           " AND   C.tipo_traspaso  IN ( 51,57 ) AND A.nss  = C.nss ",
                           #" AND   C.cve_ced_cuenta IN ( 517,531 )  ",              #517 PRESTADORA DE SERVICIOS axxi  
                           " AND   A.consecutivo_lote  = C.cont_servicio ) ",
                           "THEN '0217' ",                                          #531 PRESTADORA DE SERVICIOS BANORT        
                     "ELSE '0000' ",                            
                     "END CASE , ",                             
                     "NVL(SUM(A.monto_en_pesos),0),0 ",                             #ENTRADAS RECEPTORA JUAN
              "FROM   dis_cuenta  A ",                          
             "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.tipo_movimiento   =  1 ",                       
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",          
             "GROUP BY 1,2 "
            
         OTHERWISE # COPPEL      
          
            LET g_txt    =  	
          	 
               "SELECT  A.siefore, ",
                  "CASE WHEN C.tipo_traspaso IN ( 2,4 ) ",
                     "THEN '0217' ",                   
                     "ELSE '0000' ",
                     "END CASE ,0, ",
                     "NVL(SUM(A.monto_en_pesos),0) ",                                
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ",     #SALIDAS << UNICO EVENTO >> CEDENTE PACO 
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                              "AND   B.tipo_traspaso IN ( 51,57 ) ) ",
               "AND   A.tipo_movimiento IN ( 290,297 ) ",             
               "AND   A.folio             = C.folio ",             
               "AND   A.id_aportante[1,6]  MATCHES  'TCREA-' ",             
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",         # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde                                      
               "UNION ",                # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se
               "SELECT  A.siefore, ",   # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
                       "CASE WHEN EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",
                            "WHERE  A.folio =  C.folio AND C.ident_operacion  IN ( '09','12' ) ",
                            " AND   C.tipo_traspaso  IN ( 51,57 ) AND A.nss  = C.nss ",
                            " AND   C.cve_ced_cuenta IN ( 517,531 )  ",              #517 PRESTADORA DE SERVICIOS axxi  
                            " AND   A.consecutivo_lote  = C.cont_servicio ) ",
                            "THEN '0217' ",                                          #531 PRESTADORA DE SERVICIOS BANORT        
                      "ELSE '0000' ",                            
                      "END CASE , ",                             
                      "NVL(SUM(A.monto_en_pesos),0),0 ",                             #ENTRADAS RECEPTORA JUAN
               "FROM   dis_cuenta  A ",                          
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   A.tipo_movimiento   =  1 ",                       
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",          
              "GROUP BY 1,2 " 
            
      END CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry5111 FROM g_txt
   DECLARE c5111 CURSOR FOR qry5111
 
   FOREACH c5111 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                      g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
    
   #02 18 REASIGNACION DE CUENTAS QRYS NUEVOS NUEVOS NUEVOS NUEVOS
         
   CALL inicializa() 

      CASE  g_codigo_afore
      
         #TIPO DE TRASPASO 51 TRASPASO A-A  POR ASIGNACION DE CUENTAS
         #TIPO DE TRASPASO 57 TRASPASO A-A  POR ASIGNACION ATRAVES DE INTERNET
          
         #-----  E N T R A D A S  DE :  AFORE   A:  < AFORE >   -------#
         #SE REGISTRA COMO ENTRADA LOS MONTOS TRANSFERIDOS DE LAS CUENTAS INDIVIDUALES 
         #ASIGNADAS PREVIAMENTE EN ALGUNA AFORE ESTO ES LOS RECURSOS SE ENCONTRABAN 
         #EN SIEFORES.
         
         #----- S A L I D A S   DE :   AFORE   A: < AFORE >   -------#
         #SE REGISTRA COMO SALIDA LOS MONTOS DE LAS CUENTA INDIVIDUALES REASIGNADOS
         #A UNA NUEVA ADMINISTRADORA.
         
         WHEN 562  # SOLO AFORE INVERCAP
         	
         	  LET g_txt    =
         	  
         	     "SELECT A.siefore, '0218', ",                                                                                                                           
               "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS CEDENTE                                                                                         
               "FROM  dis_cuenta  A,taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                                   
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                  
               "AND   A.folio =  C.folio ",                                                                                                        
               "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 )",# TODAS LAS SUBCTAS QRY OPTIMIZADO
               "AND   A.tipo_movimiento   = 298 ",                                                                                         
               "AND   A.tipo_movimiento   =  B.marca_cod ",                                                                                        
               "AND   B.tipo_traspaso = 52 ",                                                                                              
               "AND   C.tipo_traspaso IN ( 2,4 ) ",                                                                                                
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                                              
               "GROUP BY 1,2 " , 
               "UNION ",                                                                                                                            
               "SELECT A.siefore, '0218', ",                                                                                                                                                                                                   
               "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS                                                                                                                                                                 
               "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                                                                                                                
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                                                                                          
               "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ",#TODAS LAS SUBCTAS QRY OPTIMIZADO                                                           
               "AND   A.tipo_movimiento = 1 ",                                                                                                                                                                             
               "AND   A.folio =  C.folio ",                                                                                                                                                                                
               "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                                                                                                               
               "AND   C.tipo_traspaso = 52 ",                                                                                                                                   
               "AND   A.nss  = C.nss ",                                                                                                                                                                                    
               "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                                                                                             
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                                                                                                                      
               "GROUP BY 1,2 " 
         	
         OTHERWISE # DE+ AFORES COPPEL METLIFE	
          
            LET g_txt    = 
          	  
          	   "SELECT  A.siefore, ",
               "CASE WHEN C.tipo_traspaso IN ( 2,4 ) ",                
                     "THEN '0218' ",                   
                     "ELSE '0000' ",
                     "END CASE ,0, ",
                     "NVL(SUM(A.monto_en_pesos),0) ",
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ",                             #SALIDAS CEDENTE SE DIFERENCIA POR EL ID APORTANTE
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                      #DEL 02 17 DE ASIGNACION PRESTADORA .
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",                 
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                              "AND   B.tipo_traspaso = 52  ) ",
               "AND   A.tipo_movimiento   = 298 ",             
               "AND   A.folio             = C.folio ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",       # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
               "UNION ",              # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se     
               "SELECT  A.siefore, ", # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
                       "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",
                            "WHERE  A.folio =  C.folio AND C.ident_operacion  IN ( '09','12' ) ",
                            " AND   C.tipo_traspaso = 52  AND A.nss  = C.nss ",
                            " AND   C.cve_ced_cuenta NOT IN ( 517,531 )  ",             #<> 517 PRESTADORA DE SERVICIOS axxi  
                            " AND   A.consecutivo_lote  = C.cont_servicio) ",
                             "THEN '0218' ",                                              #<> 531 PRESTADORA DE SERVICIOS BANORT        
                      "ELSE '0000' ",                            
                      "END CASE , ",                             
                      "NVL(SUM(A.monto_en_pesos),0),0 ",                                 #ENTRADAS RECEPTORA JUAN
               "FROM   dis_cuenta  A ",                          
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                 "AND   A.tipo_movimiento   =  1 ",                       
                 "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",          
               "GROUP BY 1,2 "
         
      END CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry5112 FROM g_txt
   DECLARE c5112 CURSOR FOR qry5112
 
   FOREACH c5112 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                      g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
   
   #----------------------

   #03 00 SAR92                                           INACTIVO
   #03 01 Traspasos IMSS-Afore                            INACTIVO

   #----------------------
   #MAGR
   #04 00  APORTACIONES VOLUNTARIAS APORTACIONES VOLUNTARIAS
   #04 01  VOLUNTARIAS ( Incluye trabajores Independientes )
   #04 02  COMPLEMENTARIAS DE RETIRO
   #04 03  APORT.DE AHORRO A LARGO PLAZO INCLUYE TRAB IND

   #04 04  VOLUNTARIAS CON PERSPECTIVA A LARGO PLAZO
         --NO EXIS AUN 'NI ENT NI SAL'
   #04 05  FONDOS DE PREVISION SOCIAL 
         --NO EXIS AUN 'NI ENT NI SAL'                                                      
   
   #MLM-2485 Mayo 2014 Se Implementará Algoritmo en el Id Cuenta  04 Id Subcuenta 01 Voluntarias
   #        ( Incluye trabajores Independientes )  para que se Incluya el Nuevo Concepto de
   #        Redes Comerciales ( Que son los que Entraran por Oxo etc.. se Ingresaran con 
   #        un Tipo de Movimiento Nuevo el 123 ( APORTACION VOLUNTARIA REDES COMERCIALES )
   #        y solo Afectará la Subcuenta 10.
   
   CALL inicializa()

      CASE  g_codigo_afore
               
         WHEN 564  # SOLO AFORE METLIFE
         	  
         	  #REQ. JIRA MLM-1886 CONFIG. 0402  ==SOLO SALIDA==
            #Complementarias con y sin beneficio fiscal DESARROLADO POR ISAI.
            #El Tipo de Mov 10 Id Aportante RETIRO 6% ISR y Tipo de Mov 10 RETIRO20 20%.
            
            #Subcuenta    11 APORTACIONES COMPLEMENTARIAS PATRON
            #             12 APORTACIONES COMPLEMENTARIAS VENTANILLA
            #             24 APORT COMPLEMENTARIA PATRON CON BF 176
            #             25 APORT COMPLEMENTARIAS VENT CON BF 176
         
            LET g_txt    =
         
               "SELECT A.siefore, ",
                   "CASE ",
                       "WHEN A.subcuenta IN ( 10,3 )  THEN '0401' ",                     #ENT 0401
                       "WHEN A.subcuenta = 10 AND A.tipo_movimiento = 123 THEN '0401' ", #ENT 0401  Redes Comerciales                     
                       "WHEN A.subcuenta =  12        THEN '0402' ",                     #ENT 0402
                       "WHEN A.subcuenta IN (15,16)   THEN '0403' ",                     #ENT 0403
                       "WHEN A.subcuenta IN ( 22,23,24,25,27,37 ) THEN '0404' ",         #ENT 0404
                       "ELSE '0000' ",
                    "END CASE, ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#E N T R A D A S
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta  IN   ( 3,10,11,12,15,16,22,23,24,25,27,37 ) ",                          
               "AND      (A.id_aportante   MATCHES  'VE*'          OR ",
                         "A.id_aportante   MATCHES  'OMNI*'        OR ",
                         "A.id_aportante   MATCHES  'VOL-ISSSTE'   OR ",
                        #"A.id_aportante   MATCHES  'COMPL.ISSST'  OR ",
                         "A.id_aportante   MATCHES  'VE-REDCO'     OR ",
                         "A.id_aportante   MATCHES  'LPLAZO-ISSS' )   ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "AND       A.tipo_movimiento IN ( 1,123 ) ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, ",#REQ. JIRA MLM-1886 CONFIG. 0402  ==SOLO SALIDA==
                   "CASE ",
                       "WHEN A.subcuenta IN ( 3,10 )  THEN '0401' ",       #SAL 0401
                       "WHEN A.subcuenta IN ( 11,12,24,25 ) ",             #SAL 0402
                       "AND   EXISTS ( SELECT 1 FROM safre_af:ret_pago_vol C ",
                       "              WHERE A.folio =  C.folio ) THEN '0402' ", 
                       "ELSE '0000' ",
                    "END CASE, ",
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS 
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta  IN   ( 3,10,11,12,24,25 ) ",
               "AND       A.siefore    IN ( 1,2,3,4,5,6 ) ",
               "AND       A.tipo_movimiento IN ( 10,490,897 )  ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, ",
                       "CASE ",
                          "WHEN A.tipo_movimiento = 801  OR ",
                           "( A.subcuenta = 16  AND A.tipo_movimiento IN ( 10,802 ) ) ",
                          "THEN '0403' ",                                #SAL 0403
                          "ELSE '0000' ",
                       "END CASE, ",
                        "0, NVL(SUM(A.monto_en_pesos),0) ",
               "FROM   dis_cuenta  A ",
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND    A.subcuenta = 16  ",
               "AND    A.tipo_movimiento IN ( 10,801,802 ) ",
               "AND    A.siefore         IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, ",
                       "CASE ",
                          "WHEN A.tipo_movimiento = 801  OR ",
                           "( A.subcuenta IN ( 22,23,24,25,27,37 ) ",
                           " AND A.tipo_movimiento IN ( 10,802 ) ) ",
                          "THEN '0404' ",                                #SAL 0404
                          "ELSE '0000' ",
                       "END CASE, ",
                        "0, NVL(SUM(A.monto_en_pesos),0) ",
               "FROM   dis_cuenta  A ",
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND    A.subcuenta  IN ( 22,23,24,25,27,37 ) ",
               "AND    A.tipo_movimiento IN ( 10,801,802 ) ",
               "AND    A.siefore         IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
               
         WHEN 568  # SOLO AFORE COPPEL 
         
            #CPL-1536 Mayo 2014 Se Implementará Algoritmo en el Id Cuenta  04 Id Subcuenta 01 Voluntarias
            #        ( Incluye trabajores Independientes )  para que se Incluya el Nuevo Concepto de
            #        Redes Comerciales ( Que son los que Entraran por Oxo etc.. se Ingresaran con 
            #        un Tipo de Movimiento Nuevo el 123 ( APORTACION VOLUNTARIA REDES COMERCIALES )
            #        y solo Afectará la Subcuenta 10.          
               
            LET g_txt    =
                 
               "SELECT A.siefore, ",
               "CASE ",
                       "WHEN A.subcuenta IN ( 3,10,23 ) ",                    
                               "THEN '0401' ",                              #ENT 0401
                       "WHEN A.subcuenta = 10 AND A.tipo_movimiento = 123 THEN '0401' ", #ENT 0401  Redes Comerciales                                                           
                       "WHEN A.subcuenta IN ( 11,12,24,25 ) THEN '0402' ",  #ENT 0402                 
                       "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ", # E N T R A D A S
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta          IN   ( 3,10,11,12,23,24,25 ) ",
               "AND      (A.id_aportante   MATCHES  'VE*'          OR ",
                         "A.id_aportante   MATCHES  'VOL-ISSSTE'   OR ",
                         "A.id_aportante   MATCHES  'VE-REDCO'     OR ",
                         "A.id_aportante   MATCHES  'COMPL.ISSST'  ) ",                   
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "AND       A.tipo_movimiento IN ( 1,123 ) ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, ",
               "CASE ",
                       "WHEN  A.subcuenta =  16 ",                   #ENT 0403                         
                               "THEN '0403' ",   
                        "WHEN A.subcuenta =  15  THEN '0403' ",      #ENT 0403                         
                       "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#E N T R A D A S
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta         IN ( 15,16 ) ",
               "AND      (A.id_aportante     MATCHES 'VE*' OR ",
               "          A.id_aportante     MATCHES  'LPLAZO-ISSS' ) ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "AND       A.tipo_movimiento = 1 ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, '0401', ",
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta          IN   ( 3,10,16,22,23 ) ",
               "AND       A.tipo_movimiento IN ( 10, 490 ) ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
        
         WHEN 562  # SOLO AFORE INVERCAP
               
            LET g_txt    =
              
               #INV-2597 Mayo 2014 Se Implementará Algoritmo en el Id Cuenta  04 Id Subcuenta 01 Voluntarias
               #        ( Incluye trabajores Independientes )  para que se Incluya el Nuevo Concepto de
               #        Redes Comerciales ( Que son los que Entraran por Oxo etc.. se Ingresaran con 
               #        un Tipo de Movimiento Nuevo el 123 ( APORTACION VOLUNTARIA REDES COMERCIALES )
               #        y solo Afectará la Subcuenta 10.  
               
               #T.M 58   ABONO POR REINVERSION DE RECURSOS NO COBRADOS            
               "SELECT A.siefore, ",
               "CASE ",
                       "WHEN A.subcuenta IN ( 10,23 ) ",                       
                               "THEN '0401' ",                                            #ENT 0401
                       "WHEN A.subcuenta = 10 AND A.tipo_movimiento = 58 ",                                  
                               "THEN '0401' ",                                            #ENT 0401 17-OCT-2012 INV-1645
                       "WHEN A.subcuenta = 10 AND A.tipo_movimiento = 123 THEN '0401' ", #ENT 0401  Redes Comerciales
                      #"WHEN A.subcuenta IN ( 11,12,24,25 ) THEN '0402' ",     #ENT 0402
                       "WHEN A.subcuenta IN ( 12,24,25 ) THEN '0402' ",        #19jun2012 MOD JIRA INV-1432 Se omitio subcta 11
                       "ELSE '0000' ",                                         #con el id_aportante  'COMPL.ISSST'
               "END CASE , ",                                                  #Esto se mostrara en el id cta 11 id subcta 14
               "NVL(SUM(A.monto_en_pesos),0) ,0 ", #ENTRADAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta          IN   ( 10,12,23,24,25 ) ",                  
               "AND      (A.id_aportante   MATCHES  'VE*'  OR ",
                         "A.id_aportante   MATCHES  'VE-REDCO'     OR ",
                         "A.id_aportante   MATCHES  'VOL-ISSSTE' ) ",
                        #"A.id_aportante   MATCHES  'COMPL.ISSST'  ) ",       #19jun2012 MOD JIRA INV-1432 Se omitio                              
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "AND       A.tipo_movimiento IN ( 1,58,123 ) ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, ",   #MODIFICACION JIRA INV - 1510 ENT- Subcta 21 Aportaciones vol 
               "CASE ",                #Con perspectiva a largo Plazo.
                       "WHEN A.subcuenta =  16 ",
                               "THEN '0403' ",                      #ENT 0403                                                     
                       "WHEN A.subcuenta =  15  THEN '0403' ",      #ENT 0403 
                       "WHEN A.subcuenta =  21  THEN '0404' ",      #ENT 0404                                            
                       "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta         IN ( 15,16,21 ) ",
               "AND      (A.id_aportante     MATCHES 'VE*' OR ",        
               "          A.id_aportante     MATCHES  'LPLAZO-ISSS' ) ",         
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "AND       A.tipo_movimiento = 1 ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, '0401', ",#REQ. JIRA INV-2889 28 Agosto 2014 Se Quita la Subcta 16
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta          IN   ( 3,10,22,23 ) ",
               "AND       A.tipo_movimiento IN ( 10,490 ) ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ",                    #REQ. JIRA INV-1570 28 Agosto 2012
               "SELECT A.siefore, '0402', ",#REQ. JIRA INV-2889 28 Agosto 2014 Se Agrega Subcta 11 T.M 10,490
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta IN ( 11,12 ) ",
               "AND       A.tipo_movimiento IN ( 10,490,897 ) ",#MOD JIRA INV-1983 SE AÑADIO EL T.M = 10
               "AND       A.siefore   IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, '0403', ",#REQ. JIRA INV-2889 28 Agosto 2014 Se Agrega la Subcta 16 Tipo Mov. 10,490
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta = 16 ",
               "AND       A.tipo_movimiento IN ( 10,490 ) ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ", 
               "SELECT A.siefore, '0404', ",
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.subcuenta         = 21 ",
               "AND       A.tipo_movimiento IN ( 10,898 ) ",      #MOD JIRA INV-1983 SE AÑADIO EL T.M = 10
               "AND       A.siefore   IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
      
      END CASE  
 

   LET g_txt = g_txt CLIPPED
   PREPARE qry88 FROM g_txt
   DECLARE c88 CURSOR FOR qry88
   
   FOREACH c88 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
   
      CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH    

   #0500    CORRECCION DE CUENTAS CORRECCION DE CUENTAS CORRECCION DE CUENTA
   
   CALL inicializa()
   
      CASE  g_codigo_afore  
        
                   #TIPO DE TRASPASO 24 TRASPASO A-A POR SEPARACION DE CUENTAS                     
                   # MOD INV-2450 Ajustes para separar Importe IMSS de ISSSTE 
                   
         WHEN 562  # SOLO AFORE INVERCAP	 
          # INV-2897  September 2014 Nuevas Configuraciones Separacion
          #           Issste.
          #           No se Detalla las Subcuentas en las Entradas de Separacion
          #           Issste porque puede quedar Abierto las Subcuentas que liquiden.
          #           el Id Aportante Varia por tanto no es fijo.

            LET g_txt    =          
                                               
               "SELECT A.siefore, ",   
               "CASE ",                                                                                                                                                                       
                       "WHEN A.tipo_movimiento = 88 AND A.fecha_conversion  = '", g_dia_ant , "' THEN '0504' ", #ENTRADA 0504 SEPARACION ISSSTE                            
                       "WHEN A.tipo_movimiento = 1 AND EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",              #ENTRADA 0504 RECEPTORA
                       "                                        WHERE  A.folio =  C.folio ",                                                                                                  
                       "                                        AND    C.ident_operacion  IN ( '09','12' ) ",                                                                                 
                       "                                        AND    C.tipo_traspaso  = 24 ",         
                       "                                        AND    A.subcuenta IN (  13,30,31,32,33,34,39 ) ",                                                                                     
                       "                                        AND    A.nss  = C.nss  ",                                                                                                    
                       "                                        AND    A.consecutivo_lote  = C.cont_servicio )",
                       "AND A.fecha_conversion  = '", g_dia_ant , "' THEN '0504' ",                                                                                           
                       "ELSE '0000' ",                                                                                                                                                        
               "END CASE , ",                                                                                                                                                                 
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#E N T R A D A S                                                                                                                            
               "FROM      dis_cuenta  A ",                                                                                                                         
               "WHERE     A.subcuenta IN (  13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE                                                                                                        
               "AND       A.tipo_movimiento IN ( 1,88 ) ",                                                                                                                                    
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                                                                                                                          
               "GROUP BY 1,2 ",                                                                                                                                             
               "UNION ",# SALIDA INV-2897  September 2014 Nuevas Configuraciones Separacion
               "SELECT A.siefore, ",                                                                                                                                                          
               "CASE ",
                       "WHEN A.tipo_movimiento = 288 AND A.fecha_conversion  = '", g_dia_ant , "' THEN '0504' ",  #SALIDA 0504 SEPARACION ISSSTE             
                            
                       "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                                                             
                       "               WHERE  A.tipo_movimiento   =  B.marca_cod  ",                                                                                                          
                       "                AND   B.tipo_traspaso     = 24  ",                                                                                                                    
                       "                AND   A.tipo_movimiento   = 291  ",                                                                                                                   
                       "                AND   A.folio             = C.folio ",                                                                                                                
                       "                AND   C.tipo_traspaso IN ( 2,4 ) ", 
                       "AND A.fecha_conversion  = '", g_dia_ant , "' ) THEN '0504' ",#SALIDA 0504 CEDENTE                                                                      
                       "ELSE '0000' ",                                                                                                                                                        
               "END CASE , ",                                                                                                                                                                 
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDA 0504                                                                                                                               
               "FROM      dis_cuenta  A ",
               "WHERE     A.subcuenta IN (  13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE                                                                                                        
               "AND       A.tipo_movimiento IN ( 288,291 ) ",                                                                                                                                          
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                                                                                                                          
               "GROUP BY 1,2 " 
                
         WHEN 568  # SOLO AFORE COPPEL
         	
         	  #39 CV ISSSTE PATRON REQ CPL-757 SE INCORPORARÁ LA SUBCUENTA 39 CV ISSSTE PATRON   Entradas y salidas
         	  
         	  LET g_txt    =              # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde                                                                                                                   
                                         # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se                                                                                                                                                                       
               "SELECT A.siefore, ",   # Agrega Filtro A.consecutivo_lote  = C.cont_servicio                                                                                                                                                             
               "CASE ",                                                                                                                                                                       
                       "WHEN A.subcuenta = 13 AND A.tipo_movimiento = 88  AND A.id_aportante = 'SEP-ISS' ",          #ENT 0504 SEPARACION ISSSTE < LO QUE SE VENIA REPORTANDO >               
                               "THEN '0504' ",                                                                                                                                                
                       "WHEN A.tipo_movimiento = 1 AND EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",                 #ENT 0504 RECEPTORA
                       "                                        WHERE  A.folio =  C.folio ",                                                                                                  
                       "                                        AND    C.ident_operacion  IN ( '09','12' ) ",                                                                                 
                       "                                        AND    C.tipo_traspaso  = 24 ",                                                                                               
                       "                                        AND    A.nss  = C.nss  ",                                                                                                    
                       "                                        AND    A.consecutivo_lote  = C.cont_servicio )",
                       "THEN '0504' ",                                                                                                                                                        
                       "ELSE '0000' ",                                                                                                                                                        
               "END CASE , ",                                                                                                                                                                 
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#E N T R A D A S                                                                                                                            
               "FROM      dis_cuenta  A ",                                                                                                                                                    
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                                                         
               "AND       A.subcuenta IN (  13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE                                                                                                        
               "AND       A.tipo_movimiento IN ( 1,88 ) ",                                                                                                                                    
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                                                                                                                          
               "GROUP BY 1,2 ",                                                                                                                                                               
               "UNION ",                                                                                                                                                                      
               "SELECT A.siefore, ",                                                                                                                                                          
               "CASE ",                                                                                                                                                                       
                       "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                                                             
                       "               WHERE  A.tipo_movimiento   =  B.marca_cod  ",                                                                                                          
                       "                AND   B.tipo_traspaso     = 24  ",                                                                                                                    
                       "                AND   A.tipo_movimiento   = 291  ",                                                                                                                   
                       "                AND   A.folio             = C.folio ",                                                                                                                
                       "                AND   C.tipo_traspaso IN ( 2,4 ) ) ",                                                                                                                 
                       "THEN '0504' ",                                        #SALIDA 0504 CEDENTE                      
                       "WHEN  A.tipo_movimiento = 288 AND A.subcuenta = 13 AND A.id_aportante = 'SEP-ISS' ",
                       "THEN '0504' ",#SALIDAS 288 ( CARGO SEPARACION DE CUENTAS ISSSTE )          
                       "ELSE '0000' ",                                                                                                                                                        
               "END CASE , ",                                                                                                                                                                 
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDA 0504                                                                                                                               
               "FROM      dis_cuenta  A ",                                                                                                                                                    
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",  
               "AND       A.subcuenta IN (  13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE                                                                                                        
               "AND       A.tipo_movimiento IN ( 288,291 ) ",                                                                                                                                          
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                                                                                                                          
               "GROUP BY 1,2 "
         	
         OTHERWISE # DE+ AFORES	METLIFE
         
            LET g_txt    =             # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde                                                                                                                   
                                       # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se                                                                                                                                                                       
               "SELECT A.siefore, ",   # Agrega Filtro A.consecutivo_lote  = C.cont_servicio                                                                                                                                                             
               "CASE ",                                                                                                                                                                       
                       "WHEN A.subcuenta = 13 AND A.tipo_movimiento = 88  AND A.id_aportante = 'SEP-ISS' ",          #ENT 0504 SEPARACION ISSSTE < LO QUE SE VENIA REPORTANDO >               
                               "THEN '0504' ",                                                                                                                                                
                       "WHEN A.tipo_movimiento = 1 AND EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",                 #ENT 0504 RECEPTORA
                       "                                        WHERE  A.folio =  C.folio ",                                                                                                  
                       "                                        AND    C.ident_operacion  IN ( '09','12' ) ",                                                                                 
                       "                                        AND    C.tipo_traspaso  = 24 ",                                                                                               
                       "                                        AND    A.nss  = C.nss  ",                                                                                                    
                       "                                        AND    A.consecutivo_lote  = C.cont_servicio )",
                       "THEN '0504' ",                                                                                                                                                        
                       "ELSE '0000' ",                                                                                                                                                        
               "END CASE , ",                                                                                                                                                                 
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#E N T R A D A S                                                                                                                            
               "FROM      dis_cuenta  A ",                                                                                                                                                    
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                                                         
               "AND       A.subcuenta IN (  13,30,31,32,33,34 ) ",#PURAS SUBCTA ISSSTE                                                                                                        
               "AND       A.tipo_movimiento IN ( 1,88 ) ",                                                                                                                                    
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                                                                                                                          
               "GROUP BY 1,2 ",                                                                                                                                                               
               "UNION ",                                                                                                                                                                      
               "SELECT A.siefore, ",                                                                                                                                                          
               "CASE ",                                                                                                                                                                       
                       "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                                                             
                       "               WHERE  A.tipo_movimiento   =  B.marca_cod  ",                                                                                                          
                       "                AND   B.tipo_traspaso     = 24  ",                                                                                                                    
                       "                AND   A.tipo_movimiento   = 291  ",                                                                                                                   
                       "                AND   A.folio             = C.folio ",                                                                                                                
                       "                AND   C.tipo_traspaso IN ( 2,4 ) ) ",                                                                                                                 
                       "THEN '0504' ",                                        #SALIDA 0504 CEDENTE                                           
                       "ELSE '0000' ",                                                                                                                                                        
               "END CASE , ",                                                                                                                                                                 
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDA 0504                                                                                                                               
               "FROM      dis_cuenta  A ",                                                                                                                                                    
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",  
               "AND       A.subcuenta IN (  13,30,31,32,33,34 ) ",#PURAS SUBCTA ISSSTE                                                                                                        
               "AND       A.tipo_movimiento = 291  ",                                                                                                                                          
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                                                                                                                          
               "GROUP BY 1,2 "
      END CASE
    
    
   PREPARE qry733 FROM g_txt                              
   DECLARE c733 CURSOR FOR qry733                          
                                                         
   FOREACH c733 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent, 
                     g_rec.sal                                                
                                                                             
      CALL Act_tbl ( g_rec.* )                                               
                                                                             
   END FOREACH                                                                  
   
   #0505    REVERSO       ISSSTE    < NO EXISTE AUN >   
   #0501    UNIFICACION   IMSS
     
   CALL inicializa() 

   #---Inicializa Variables --#
   
   INITIALIZE   g_dd   TO NULL
   INITIALIZE   g_mm   TO NULL
   INITIALIZE   g_yy   TO NULL
        
   #--                      --#
   
   LET g_dd       =  DAY(g_dia_ant)   USING "&&"
   LET g_mm       =  MONTH(g_dia_ant) USING "&&"
   LET g_yy       =  YEAR(g_dia_ant)  USING "&&&&"                                                        
                                                                          
      CASE  g_codigo_afore                                                   
                                                                             
         WHEN 562  # SOLO AFORE INVERCAP
         	
         	  #TIPO DE TRASPASO 24 TRASPASO SEPARACION NORMAL
             
            #SOLO MOSTRARÁ LAS CIFRAS DEL QUERY DE CEDENTE Y RECEPTORA CUANDO:
            #Se haya Liquidado folios de Cedente y Receptora  y
            #que sea el día de reporte de la Estadistica diferente  a los 
            #Primeros 5 días Habiles.
            
            IF ( g_dd = '01' OR
            	   g_dd = '02' OR
         		     g_dd = '03' OR
         		     g_dd = '04' OR
         		     g_dd = '05' ) THEN #SE LEERA QRY PARA BUSCAR LOS MONTOS
         		                        #DE  U N I F I C A C I O N     I M S S 
               LET g_txt    = 
            	  
           	      #-- UNIFICACION INTRA -EXTRA AFORE
                  #---ENTRADAS Este Proceso se liquida + "o" - los Primeros 5 días Naturales                                                               
                  "SELECT A.siefore, '0501', ",                            
                  "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS
                  "FROM  dis_cuenta  A ",                              
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",   
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS
                  "AND   A.tipo_movimiento = 1 ",           
                  "AND   A.id_aportante[1,3] MATCHES  'U[CN]-'",
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",    
                  "GROUP BY 1,2 ",
                  "UNION ",                                          
                  #-- UNIFICACION INTRA -EXTRA AFORE                                                                                
                  #---SALIDAS Este Proceso se liquida + "o" - los Primeros 5 días Naturales                                                  	                                                                                                                           
            	     "SELECT A.siefore, '0501', ",                                                                                   
            	     "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDA                                                                       
            	     "FROM  dis_cuenta  A ",                                                                                         
            	     "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
            	     "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS
            	     "AND   A.tipo_movimiento IN ( 241,242 ) ",                                                                      
            	     "AND   A.id_aportante[1,3] MATCHES  'U[CN]-'",                                                                  
            	     "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                          
            	     "GROUP BY 1,2 "
            
            ELSE  #RANGO DE FECHAS  > 5 HASTA FIN DE MES SE PUEDE LIQUIDAR                                                           
            	    #RECEPTORA Y CEDENTE
            	     
               LET g_txt    =                     
               
                   # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
                   # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se     
                   # Agrega Filtro A.consecutivo_lote  = C.cont_servicio    
                                                                                                                                                                       
                   #-- RECEPTORA                                                                                                   
                   #---ENTRADAS Este Proceso se liquida + "o" - Entre 25 y 31                                                     
                   "SELECT A.siefore, '0501', ",                                                                                   
                   "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS                                                                     
                   "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                    
                   "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
                   "AND   A.folio =  C.folio ",                                                                                    
                   "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS
                   "AND   A.tipo_movimiento = 1 ",                                                                                 
                   "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                   
                   "AND   C.tipo_traspaso  IN ( 12,83 )",                                                                          
                   "AND   A.nss  = C.nss ",   
                   "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                     
                   "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                          
                   "GROUP BY 1,2 ",
                   "UNION ",                                                                                                                    
                   #-- CEDENTE                                                                                                           
                   #---SALIDAS Este Proceso se liquida + "o" - Entre 25 y 31                                                             
                   "SELECT A.siefore, '0501', ",                                                                                      
                   "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS                                                                         
                   "FROM  dis_cuenta  A,taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                  
                   "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                 
                   "AND   A.folio =  C.folio ",                                                                                       
                   "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS   
                   "AND   A.tipo_movimiento IN ( 240,283 ) ",                                                                         
                   "AND   A.tipo_movimiento   =  B.marca_cod ",                                                                       
                   "AND   B.tipo_traspaso IN ( 12,83 ) ",                                                                             
                   "AND   C.tipo_traspaso IN ( 2,4 ) ",                                                                               
                   "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                             
                   "GROUP BY 1,2 "
            	        
            END IF
                                                                             
         WHEN 568  # SOLO AFORE COPPEL CPL-1413
         
            #TIPO DE TRASPASO 24 TRASPASO SEPARACION NORMAL
            
            #SOLO MOSTRARÁ LAS CIFRAS DEL QUERY DE CEDENTE Y RECEPTORA CUANDO:
            #Se haya Liquidado folios de Cedente y Receptora  y
            #que sea el día de reporte de la Estadistica diferente  a los 
            #Primeros 5 días Habiles.
            
            IF ( g_dd = '01' OR
            	   g_dd = '02' OR
         		     g_dd = '03' OR
         		     g_dd = '04' OR
         		     g_dd = '05' ) THEN #SE LEERA QRY PARA BUSCAR LOS MONTOS
         		                       #DE  U N I F I C A C I O N     I M S S 
               LET g_txt    = 
            	  
            	    "SELECT  A.siefore,'0501', ", 
                  "NVL(SUM ( CASE ",
                            "WHEN A.tipo_movimiento = 1 THEN A.monto_en_pesos ",    #ENTRADAS 0501
                           "ELSE 0 ",
                           "END ),0), ", 
                  "NVL(SUM ( CASE ",
                  "WHEN A.tipo_movimiento <> 1 THEN A.monto_en_pesos ",#SALIDAS 0501
                  "ELSE 0 ",
                  "END ),0) ",
                  "FROM  dis_cuenta A ",
                  "WHERE A.folio IN ( ",
                  "SELECT UNIQUE B.folio  ",
                  "FROM   dis_cuenta B ",
                  "WHERE  B.tipo_movimiento IN (241,242) ",
                  "AND    B.fecha_conversion = '", g_dia_ant , "' ",
                  " ) ",
                  "AND siefore IN (1,2,3,4,5) ",
                  "GROUP BY 1,2 "
                             
            ELSE  #RANGO DE FECHAS  > 5 HASTA FIN DE MES SE PUEDE LIQUIDAR                                                           
            	    #RECEPTORA Y CEDENTE
            	     
               LET g_txt    =                     
               
                  # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
                  # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se     
                  # Agrega Filtro A.consecutivo_lote  = C.cont_servicio    
                                                                                                                                                                      
                  #-- RECEPTORA                                                                                                   
                  #---ENTRADAS Este Proceso se liquida + "o" - Entre 25 y 31                                                     
                  "SELECT A.siefore, '0501', ",                                                                                   
                  "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS                                                                     
                  "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                    
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
                  "AND   A.folio =  C.folio ",                                                                                    
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS
                  "AND   A.tipo_movimiento = 1 ",                                                                                 
                  "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                   
                  "AND   C.tipo_traspaso  IN ( 12,83 )",                                                                          
                  "AND   A.nss  = C.nss ",   
                  "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                     
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                          
                  "GROUP BY 1,2 ",
                  "UNION ",                                                                                                                    
                  #-- CEDENTE                                                                                                           
                  #---SALIDAS Este Proceso se liquida + "o" - Entre 25 y 31                                                             
                  "SELECT A.siefore, '0501', ",                                                                                      
                  "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS                                                                         
                  "FROM  dis_cuenta  A,taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                  
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                 
                  "AND   A.folio =  C.folio ",                                                                                       
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS   
                  "AND   A.tipo_movimiento IN ( 240,283 ) ",                                                                         
                  "AND   A.tipo_movimiento   =  B.marca_cod ",                                                                       
                  "AND   B.tipo_traspaso IN ( 12,83 ) ",                                                                             
                  "AND   C.tipo_traspaso IN ( 2,4 ) ",                                                                               
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                             
                  "GROUP BY 1,2 "
                  
            END IF            
         
         OTHERWISE # DE+ AFORES  METLIFE MLM-2221 Se Agregará Consulta para Extraer                OTHERWISE # DE+ AFORES  METLIFE MLM-2221 Se Agregará Consulta para Extraer                
                   #                              Las Entradas y Salidas de las Unificaciones                #                              Las Entradas y Salidas de las Unificaciones
                   #                              Intra-Extra Afore ISSSTE.                                  #                              Intra-Extra Afore ISSSTE.                  
         
            #TIPO DE TRASPASO 24 TRASPASO SEPARACION NORMAL                                                                                                           
                                                                                                                                                                      
            #SOLO MOSTRARÁ LAS CIFRAS DEL QUERY DE CEDENTE Y RECEPTORA CUANDO:                                                                                        
            #Se haya Liquidado folios de Cedente y Receptora  y                                                                                                       
            #que sea el día de reporte de la Estadistica diferente  a los                                                                                             
            #Primeros 5 días Habiles.                                                                                                                                 
                                                                                                                                                                      
            IF ( g_dd = '01' OR                                                                                                                                       
            	   g_dd = '02' OR                                                                                                                                       
                 g_dd = '03' OR                                                                                                                                       
                 g_dd = '04' OR                                                                                                                                       
                 g_dd = '05' ) THEN #SE LEERA QRY PARA BUSCAR LOS MONTOS                                                                                              
                                   #DE  U N I F I C A C I O N     I M S S                                                                                             
               LET g_txt    =                                                                                                                                         
            	                       
                  "SELECT A.siefore, ",                
                  "CASE ",
                         "WHEN A.subcuenta IN ( 1,2,3,5,6,7,9,10,11,12,16,22,23,24,25,27,37 ) ",
                         "AND  A.tipo_movimiento = 1 ",#TM 1 APORTACION
                         "AND  A.folio IN ( SELECT UNIQUE B.folio FROM dis_cuenta B WHERE B.tipo_movimiento IN (241,242) ",#TM 241 UNIFICACION INTRA-AFORE,242 UNIFICACION EXTRA-AFORE                                                                                                                          
                         "AND  B.fecha_conversion = '", g_dia_ant , "' AND B.folio = A.folio ) ",#MLM-2811
                         "THEN '0501' ",
                         "WHEN  A.tipo_movimiento = 265  AND A.subcuenta = 10 ", #JIRA MLM-2654 265  'ABONO UNIFICACION INTRA-AFORE IMSS-ISSSTE'
                         "THEN '0501' ",
                         "ELSE '0000' ",
                  "END CASE , ",
                  "NVL(SUM(A.monto_en_pesos),0) ,0 ",    #ENTRADAS        
                  "FROM      dis_cuenta  A ",
                  "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                 
                  "AND       A.subcuenta IN (   1,2,3,5,6,7,9,10,11,12,16,22,23,24,25,27,37 ) ",  #PURAS SUBCTAS ISSSTE
                  "AND       A.tipo_movimiento IN (1,265) ",
                  "AND       A.siefore  IN ( 1,2,3,4,5,6 ) ",            
                  "GROUP BY 1,2 ", 
                  "UNION ",
                  "SELECT A.siefore, ",                
                     "CASE ",
                         "WHEN A.subcuenta IN ( 1,2,3,5,6,7,9,10,11,12,16,22,23,24,25,27,37 ) ",
                         "AND  A.tipo_movimiento IN ( 241,242 ) ",#TM 241 UNIFICACION INTRA-AFORE,242 UNIFICACION EXTRA-AFORE
                         "AND  A.folio IN ( SELECT UNIQUE B.folio FROM dis_cuenta B WHERE B.tipo_movimiento IN (241,242) ",                                                                                                                          
                         "AND  B.fecha_conversion = '", g_dia_ant , "' AND B.folio = A.folio ) ",#MLM-2811
                         "THEN '0501' ", 
                         "WHEN  A.tipo_movimiento = 264  AND A.subcuenta = 10 ", #JIRA MLM-2654 264, 'CARGO UNIFICACION INTRA-AFORE IMSS-ISSSTE'
                         "THEN '0501' ",
                         "ELSE '0000' ",
                  "END CASE , ",
                  "0, NVL(SUM(A.monto_en_pesos),0) ", #SALIDAS
                  "FROM      dis_cuenta  A ",
                  "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                 
                  "AND       A.subcuenta IN (   1,2,3,5,6,7,9,10,11,12,16,22,23,24,25,27,37 ) ",  #PURAS SUBCTAS ISSSTE
                  "AND       A.tipo_movimiento IN ( 241,242,264 ) ",
                  "AND       A.siefore  IN ( 1,2,3,4,5,6 ) ",            
                  "GROUP BY 1,2 "
                                                                                                                                                                      
            ELSE  #RANGO DE FECHAS  > 5 HASTA FIN DE MES SE PUEDE LIQUIDAR                                                                                            
            	    #RECEPTORA Y CEDENTE                                                                                                                                
            	                                                                                                                                                        
               LET g_txt    =                                                                                                                                         
                                                                                                                                                                      
                  # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde                                                                                         
                  # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se                                                                                              
                  # Agrega Filtro A.consecutivo_lote  = C.cont_servicio                                                                                               
                                                                                                                                                                      
                  #-- RECEPTORA                                                                                                                                       
                  #---ENTRADAS Este Proceso se liquida + "o" - Entre 25 y 31                                                                                          
                  "SELECT A.siefore, '0501', ",                                                                                                                       
                  "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS                                                                                                         
                  "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                                                        
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                                  
                  "AND   A.folio =  C.folio ",                                                                                                                        
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37) ",#PURAS SUBCTA IMSS MLM-2745 MLM-2745 Se Agrega Subcuenta 37                                
                  "AND   A.tipo_movimiento = 1 ",                                                                                                                     
                  "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                                                       
                  "AND   C.tipo_traspaso  IN ( 12,83 )",                                                                                                              
                  "AND   A.nss  = C.nss ",                                                                                                                            
                  "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                                     
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                                                              
                  "GROUP BY 1,2 ",                                                                                                                                    
                  "UNION ",                                                                                                                                           
                  #-- CEDENTE                                                                                                                                         
                  #---SALIDAS Este Proceso se liquida + "o" - Entre 25 y 31                                                                                           
                  "SELECT A.siefore, '0501', ",                                                                                                                       
                  "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS                                                                                                          
                  "FROM  dis_cuenta  A,taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                                                   
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                                  
                  "AND   A.folio =  C.folio ",                                                                                                                        
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37 ) ",#PURAS SUBCTA IMSS  MLM-2745 Se Agrega Subcuenta 37                                  
                  "AND   A.tipo_movimiento IN ( 240,283 ) ",                                                                                                          
                  "AND   A.tipo_movimiento   =  B.marca_cod ",                                                                                                        
                  "AND   B.tipo_traspaso IN ( 12,83 ) ",                                                                                                              
                  "AND   C.tipo_traspaso IN ( 2,4 ) ",                                                                                                                
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                                                              
                  "GROUP BY 1,2 "                                                                                                                                     
                                                                                                                                                                      
            END IF 
         
      END CASE      
   
   LET g_txt = g_txt CLIPPED                                              
   PREPARE qry33 FROM g_txt                                              
   DECLARE c33 CURSOR FOR qry33                                         
                                                                          
   FOREACH c33 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,                 
                    g_rec.sal                                             
                                                                          
      CALL Act_tbl ( g_rec.* )                                            
   
   END FOREACH
   
   #0502    SEPARACION    IMSS
   
   CALL inicializa() 
   
   
      #---Inicializa Variables --#
      
      INITIALIZE   g_dd   TO NULL
      INITIALIZE   g_mm   TO NULL
      INITIALIZE   g_yy   TO NULL
           
      #--                      --#
      
      LET g_dd       =  DAY(g_dia_ant)   USING "&&"
      LET g_mm       =  MONTH(g_dia_ant) USING "&&"
      LET g_yy       =  YEAR(g_dia_ant)  USING "&&&&"                                                        
                                                                             
      CASE  g_codigo_afore                                                                                                                               
         
         WHEN 568  # SOLO AFORE COPPEL 
         	
                	 #TIPO DE TRASPASO 24 TRASPASO SEPARACION NORMAL
                   #CPL-1413
            
            IF ( g_dd = '01' OR                                                                                                               
                 g_dd = '02' OR                                                                                                               
                 g_dd = '03' OR                                                                                                                
                 g_dd = '04' OR                                                                                                                
                 g_dd = '05' OR                                                                                                                
                 g_dd = '06' OR                                                                                                                
                 g_dd = '07' OR                                                                                                                
                 g_dd = '08' OR                                                                                                                
                 g_dd = '09' OR        	                                                                                                       
                 g_dd = '10' OR                                                                                                                
                 g_dd = '11' OR                                                                                                                
                 g_dd = '12' )   THEN  #DÍAS QUE NO SE LIQUIDA CEDENTE NI RECEPTORA  ENTONCES QUE LEEA UNIFICACION  NORMALITA INTRA-EXTRA AFORE
                                        #RANGO DE BUSQUEDA ENTRE DIA 1 AL DIA 5 AL PRINCIPIO DE MES                                             
                                        #SEPARACION NORMALITA.                                                                                 
            
               LET g_txt    = 
            	  
            	     "SELECT  A.siefore,'0502', ",
                            "NVL(SUM ( CASE ",
                                        "WHEN A.monto_en_pesos > 0 THEN A.monto_en_pesos ",
                                      "ELSE 0 ",
                                      "END ),0), ",
                            "NVL(SUM ( CASE ",
                                          "WHEN A.monto_en_pesos < 0 THEN A.monto_en_pesos ",
                                       "ELSE 0 ",
                                       "END ),0) ",
                   "FROM     dis_cuenta  A ,sep_det_reg_sol_reclamante B ",#ENT y SAL
                   "WHERE  A.fecha_proceso           = '", g_dia_ant , "' ",
                   "AND    B.estado                  = 8 ",
                   "AND    A.folio                   = B.folio ",
                   "AND    ( A.nss = B.n_seguro  OR  A.nss  = B.nss ) ",
                   "AND    A.siefore           IN ( 1,2,3,4,5,6 ) ",
                   "AND    A.subcuenta    NOT IN (4,8,14) ",
                   "GROUP BY 1,2 "
            
            ELSE #RANGO DE FECHAS  > 15 HASTA FIN DE MES SE PUEDE LIQUIDAR 
            	    #RECEPTORA Y CEDENTE.
            	    
            	 LET g_txt    =                                  # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
                                                              # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se      
                  "SELECT A.siefore, ",                        # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
                  "CASE ",                                                                                     #ENT 0502 SEPARACION IMSS < LO QUE SE VENIA REPORTANDO >
                         "WHEN A.tipo_movimiento = 1 AND EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",         #ENT 0502 OJO FALTA LEER PURAS SUBCTA IMSS???  POR ARCHIVO DE RECEPTORA
                         "                                        WHERE  A.folio =  C.folio ",
                         "                                        AND    C.ident_operacion  IN ( '09','12' ) ",
                         "                                        AND    C.tipo_traspaso = 24 ",
                         "                                        AND    A.nss  = C.nss ",                     
                         "                                        AND    A.consecutivo_lote  = C.cont_servicio) ",
                         "THEN '0502' ",                     
                         "ELSE '0000' ",
                  "END CASE , ",
                  "NVL(SUM(A.monto_en_pesos),0) ,0 ",#E N T R A D A S 
                  "FROM      dis_cuenta  A ",
                  "WHERE     A.fecha_conversion  = '", g_dia_ant , "'  ",
                  "AND       A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ", #PURAS SUBCUENTAS IMSS                          
                  "AND       A.tipo_movimiento    =  1 ",
                  "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
                  "GROUP BY 1,2 ",
                  "UNION ",
                  "SELECT A.siefore, ",
                  "CASE ",                      
                          "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",
                          "               WHERE  A.tipo_movimiento    =  B.marca_cod  ",                     
                          "                AND   B.tipo_traspaso      =  24      ",
                          "                AND   A.tipo_movimiento    =  291    ",
                          "                AND   A.folio              =  C.folio ",
                          "                AND   C.tipo_traspaso IN ( 2,4 ) )",                                            
                          "THEN '0502' ",                                                             
                          "ELSE '0000' ",
                  "END CASE , ",
                  "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDA 0502
                  "FROM      dis_cuenta  A ",
                  "WHERE     A.fecha_conversion  = '", g_dia_ant , "'  ",
                  "AND       A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ", #PURAS SUBCUENTAS IMSS              
                  "AND       A.tipo_movimiento = 291  ",
                  "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
                  "GROUP BY 1,2 "
                  
            END IF
         	
         WHEN 562  # SOLO AFORE INVERCAP	
         	
         	         #TIPO DE TRASPASO 24 TRASPASO SEPARACION NORMAL
            
                   #SOLO MOSTRARÁ LAS CIFRAS DEL QUERY DE CEDENTE Y RECEPTORA CUANDO:
                   #Se haya Liquidado folios de Cedente y Receptora  y
                   #que sea el día de reporte de la Estadistica diferente  a los 
                   #Primeros 5 días Habiles.
            	
            IF  ( g_dd = '01' OR
                  g_dd = '02' OR
         	        g_dd = '03' OR
         	        g_dd = '04' OR
         	        g_dd = '05' OR
         	        g_dd = '06' OR
         	        g_dd = '07' OR
         	        g_dd = '08' OR
         	        g_dd = '09' OR        	       
         	        g_dd = '10' OR
         	        g_dd = '11' OR
         	        g_dd = '12' )   THEN  #DÍAS QUE NO SE LIQUIDA CEDENTE NI RECEPTORA  ENTONCES QUE LEEA UNIFICACION  NORMALITA INTRA-EXTRA AFORE
         	                              #RANGO DE BUSQUEDA ENTRE DIA 1 AL DIA 5 AL PRINCIPIO DE MES
                                        #SEPARACION NORMALITA.           	                         
               #MOD INV-2450 Ajustes para separar Importe IMSS de ISSSTE                        
               LET g_txt    = 
            	  
            	     "SELECT  A.siefore,'0502', ",
                           "NVL(SUM ( CASE ",
                                       "WHEN A.monto_en_pesos > 0 THEN A.monto_en_pesos ",
                                     "ELSE 0 ",
                                     "END ),0), ",
                           "NVL(SUM ( CASE ",
                                         "WHEN A.monto_en_pesos < 0 THEN A.monto_en_pesos ",
                                      "ELSE 0 ",
                                      "END ),0) ",
                  "FROM     dis_cuenta  A ,sep_det_reg_sol_reclamante B ",#ENT y SAL
                  "WHERE  A.fecha_proceso           = '", g_dia_ant , "' ",
                  "AND    B.estado                  = 9 ",
                  "AND    A.folio                   = B.folio ",
                  "AND    ( A.nss = B.n_seguro  OR  A.nss  = B.nss ) ",
                  "AND    A.siefore           IN ( 1,2,3,4,5,6 ) ",
                  "AND    A.subcuenta    NOT IN (4,8,14,13,30,31,32,33,34,39) ",#SE EXCLUYEN SUBCUENTAS ISSSTE
                  "GROUP BY 1,2 "
                  
            ELSE #RANGO DE FECHAS  > 15 HASTA FIN DE MES SE PUEDE LIQUIDAR 
            	    #RECEPTORA Y CEDENTE.
            	    
               LET g_txt    =                                 
               
                  "SELECT A.siefore, '0502', ",                                                                                   
                  "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS                                                                     
                  "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                    
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
                  "AND   A.folio =  C.folio ",                                                                                    
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS  QRY OPTIMIZADO
                  "AND   A.tipo_movimiento = 1 ",                                                                                 
                  "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                   
                  "AND   C.tipo_traspaso  = 24",                                                                          
                  "AND   A.nss  = C.nss ",   
                  "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                     
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                          
                  "GROUP BY 1,2 ",
                  "UNION ",                   
                  "SELECT A.siefore, '0502', ",                                                                                      
                  "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS                                                                         
                  "FROM  dis_cuenta  A,taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                  
                  "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                 
                  "AND   A.folio =  C.folio ",                                                                                       
                  "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 )",#PURAS SUBCTA IMSS  QRY OPTIMIZADO 
                  "AND   A.tipo_movimiento = 291 ",                                                                         
                  "AND   A.tipo_movimiento   =  B.marca_cod ",                                                                       
                  "AND   B.tipo_traspaso = 24 ",                                                                             
                  "AND   C.tipo_traspaso IN ( 2,4 ) ",                                                                               
                  "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                             
                  "GROUP BY 1,2 "
                  
            END IF
            
         WHEN 564  # SOLO AFORE METLIFE  
         	
         	  IF g_hayinf_rec_ced  = "S"  THEN  # DÍAS EN QUE PROBABLEMENTE SE LIQUIDA CEDENTE Y RECEPTORA
         	  	    
                                                                              
                  LET g_txt    =                                  # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
                                                                  # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se      
                     "SELECT A.siefore, ",                        # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
                     "CASE ",                                                                                     #ENT 0502 SEPARACION IMSS < LO QUE SE VENIA REPORTANDO >
                            "WHEN A.tipo_movimiento = 1 AND EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",         #ENT 0502 OJO FALTA LEER PURAS SUBCTA IMSS???  POR ARCHIVO DE RECEPTORA
                            "                                        WHERE  A.folio =  C.folio ",
                            "                                        AND    C.ident_operacion  IN ( '09','12' ) ",
                            "                                        AND    C.tipo_traspaso = 24 ",
                            "                                        AND    A.nss  = C.nss ",                     
                            "                                        AND    A.consecutivo_lote  = C.cont_servicio) ",
                            "THEN '0502' ",                     
                            "ELSE '0000' ",
                     "END CASE , ",
                     "NVL(SUM(A.monto_en_pesos),0) ,0 ",#E N T R A D A S 
                     "FROM      dis_cuenta  A ",
                     "WHERE     A.fecha_conversion  = '", g_dia_ant , "'  ",
                     "AND       A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37 ) ", #PURAS SUBCUENTAS IMSS  MLM-2745 Se Agrega Subcuenta 37
                     "AND       A.tipo_movimiento    =  1 ",
                     "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
                     "GROUP BY 1,2 ",
                     "UNION ",
                     "SELECT A.siefore, ",
                     "CASE ",                      
                             "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",
                             "               WHERE  A.tipo_movimiento    =  B.marca_cod  ",                     
                             "                AND   B.tipo_traspaso      =  24      ",
                             "                AND   A.tipo_movimiento    =  291    ",
                             "                AND   A.folio              =  C.folio ",
                             "                AND   C.tipo_traspaso IN ( 2,4 ) )",                                            
                             "THEN '0502' ",                                                             
                             "ELSE '0000' ",
                     "END CASE , ",
                     "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDA 0502
                     "FROM      dis_cuenta  A ",
                     "WHERE     A.fecha_conversion  = '", g_dia_ant , "'  ",
                     "AND       A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37 ) ", #PURAS SUBCUENTAS IMSS MLM-2745 Se Agrega Subcuenta 37
                     "AND       A.tipo_movimiento = 291  ",
                     "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
                     "GROUP BY 1,2 "
                
             ELSE # DÍAS QUE NO SE LIQUIDA CEDENTE NI RECEPTORA  ENTONCES QUE LEEA SEPARACION NORMALITA           	    
             	
             	  LET g_txt    = 
             	     
             	     "SELECT A.siefore, '0502', ",     #SEPARACION IMSS   NUM. REQUERIMIENTO JIRA  MLM-1433                       
                   "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS                                                                                         
                   "FROM  dis_cuenta  A ",                                                                                        
                   "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                   
                   "AND   A.tipo_movimiento   = 590 ", 
                   "AND   A.id_aportante      = 'SEPARACION' ",                                    
                   "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                                              
                   "GROUP BY 1,2 ",                                                                                                                    
                   "UNION ",                                                                                                                                
                   "SELECT A.siefore, '0502', ",
                   "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS 0502
                   "FROM  dis_cuenta  A ",
                   "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
                   "AND   A.tipo_movimiento   = 280 ",
                   "AND   A.id_aportante      = 'SEPARACION' ",
                   "AND   A.siefore            IN ( 1,2,3,4,5,6 ) ",
                   "GROUP BY 1,2 "
                
             END IF
         		
         OTHERWISE # DE+ AFORES  
            
      END CASE 
     
   LET g_txt = g_txt CLIPPED                                              
   PREPARE qry3332 FROM g_txt                                              
   DECLARE c3332 CURSOR FOR qry3332                                         
                                                                          
   FOREACH c3332 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,                 
                    g_rec.sal                                             
                                                                          
      CALL Act_tbl ( g_rec.* )                                            
   
   END FOREACH
     
   #-- 05 03   UNIFICACION ISSSTE ( RCV ISSSTE )
                                          
   CALL inicializa()                                                         
                                                                             
      CASE  g_codigo_afore                                                   
                           
         WHEN 562  # SOLO AFORE INVERCAP
         	
         	  LET g_txt    =                       # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
                                                 # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se     
               "SELECT A.siefore, ",                # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
                  "CASE ",
                      "WHEN A.subcuenta = 13 AND A.tipo_movimiento IN ( 1,3 ) AND A.id_aportante = 'UNI_ISSSTE' ",
                              "AND A.subcuenta IN ( 13,30,31,32,33,34,39 ) THEN '0503' ",                                                                #ENTRADAS 0503 UNIFICACION ISSSTE < LO QUE SE VENIA REPORTANDO >
                      "WHEN A.tipo_movimiento = 1 AND EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",          #ENTRADAS 0503 ARCHIVO RECEPTORA
                                                              "WHERE  A.folio =  C.folio ",
                                                              "AND    C.ident_operacion  IN ( '09','12' ) ",
                                                              "AND    C.tipo_traspaso  IN ( 12,83 ) ",
                                                              "AND    A.nss  = C.nss  ",
                                                              "AND    A.consecutivo_lote  = C.cont_servicio ",
                                                              "AND    A.subcuenta IN ( 13,30,31,32,33,34,39 ) ) ",                                                        
                      "THEN '0503' ",                                     
                      "WHEN  A.tipo_movimiento = 1 AND A.id_aportante[1,3] MATCHES  'U[CN]-' ",            #UNIFICACION INTRA -EXTRA AFORE                    
                      "AND   A.subcuenta IN ( 13,30,31,32,33,34,39 )",
                      "THEN  '0503' ",
                      "WHEN  A.tipo_movimiento = 265  AND A.subcuenta IN ( 10,13,30,31,32,33,34,39 ) ",      #JIRA INV-1930 265 'ABONO UNIFICACION INTRA-AFORE IMSS-ISSSTE'    
                      "THEN '0503' ",                                                                   
                      "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",    #ENTRADAS        
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                 
               "AND       A.subcuenta IN (  10,13,30,31,32,33,34,39 ) ",  #PURAS SUBCTAS ISSSTE
               "AND       A.tipo_movimiento IN ( 1,3,265 ) ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",            
               "GROUP BY 1,2 ", 
               "UNION ",
               "SELECT A.siefore, ",
               "CASE ",
                      "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",
                                      "WHERE A.tipo_movimiento   =  B.marca_cod ",
                                      "AND   A.subcuenta IN ( 13,30,31,32,33,34,39 ) ",                       
                                      "AND   B.tipo_traspaso IN ( 12,83 ) ",
                                      "AND   A.tipo_movimiento IN ( 240,283 ) ", 
                                      "AND   A.folio             = C.folio ",
                                      "AND   C.tipo_traspaso IN ( 2,4 ) ) ",                    
                      "THEN '0503' ",
                      "WHEN A.tipo_movimiento IN ( 241,242 ) AND A.id_aportante[1,3] MATCHES  'U[CN]-' ", 
                      "AND  A.subcuenta IN ( 13,30,31,32,33,34,39 )  ", 
                      "THEN '0503' ",
                      "WHEN  A.tipo_movimiento = 264  AND A.subcuenta IN ( 10,13,30,31,32,33,34,39 ) ", #JIRA INV-1930 264, 'CARGO UNIFICACION INTRA-AFORE IMSS-ISSSTE'
                      "THEN '0503' ",
                      "ELSE '0000' ",
               "END CASE , ",
               "0, NVL(SUM(A.monto_en_pesos),0) ", #SALIDAS CEDENTE
               "FROM      dis_cuenta A ",              
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",  
               "AND       A.subcuenta IN (  10,13,30,31,32,33,34,39 ) ",  #PURAS SUBCTAS ISSSTE                 
               "AND       A.tipo_movimiento IN ( 240,241,242,264,283 ) ",
               "AND       A.siefore         IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "

      WHEN 568  # SOLO AFORE COPPEL
      	  
      	 #CPL-1808 E N T R A D A S (Se Omite el Algoritmo  de Unificaciones Mixta).
      	 #         WHEN A.subcuenta = 13 AND A.tipo_movimiento IN ( 1,3 ) AND A.id_aportante = 'UNI_ISSSTE'    
      	 #         Solo se reportará en el Id Cuenta 13 Id Subcuenta 02.   	 
      	 
      	 #39 CV ISSSTE PATRON REQ CPL-757 SE INCORPORARÁ LA SUBCUENTA 39 CV ISSSTE PATRON   Entradas y salidas 
      	
      	 IF g_hayinf_rec_ced  = "S" THEN # DÍAS EN QUE SE LIQUIDA CEDENTE Y RECEPTORA
      	 	    
      	
      	    LET g_txt    =                # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
                                          # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se     
               "SELECT A.siefore, ",      # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
                  "CASE ",                      
                      "WHEN A.tipo_movimiento = 1 AND EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",          #ENTRADAS 0503 ARCHIVO RECEPTORA
                                                              "WHERE  A.folio =  C.folio ",
                                                              "AND    C.ident_operacion  IN ( '09','12' ) ",
                                                              "AND    C.tipo_traspaso  IN ( 12,83 ) ",
                                                              "AND    A.nss  = C.nss  ",
                                                              "AND    A.consecutivo_lote  = C.cont_servicio) ",
                      "THEN '0503' ",                  
                      "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",        
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                 
               "AND       A.subcuenta IN ( 30,31,32,33,34,39 ) ",  #PURAS SUBCTAS ISSSTE
               "AND       A.tipo_movimiento = 1 ",
               "AND       A.siefore  IN ( 1,2,3,4,5,6 ) ",            
               "GROUP BY 1,2 ", 
               "UNION ",
               "SELECT A.siefore, ",
               "CASE ",
                      "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",
                                      "WHERE A.tipo_movimiento   =  B.marca_cod ",                      
                                      "AND   B.tipo_traspaso IN ( 12,83 ) ",
                                      "AND   A.tipo_movimiento IN ( 240,283 ) ", 
                                      "AND   A.folio             = C.folio ",
                                      "AND   C.tipo_traspaso IN ( 2,4 ) ) ",                    
                      "THEN '0503' ",
                      "ELSE '0000' ",
               "END CASE , ",
               "0, NVL(SUM(A.monto_en_pesos),0) ", #SALIDAS CEDENTE
               "FROM      dis_cuenta A ",              
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",  
               "AND       A.subcuenta IN (  13,30,31,32,33,34,39 ) ",  #PURAS SUBCTAS ISSSTE                 
               "AND       A.tipo_movimiento IN ( 240,283 ) ",
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "   
      	 
      	 ELSE # DÍAS QUE NO SE LIQUIDA CEDENTE NI RECEPTORA  ENTONCES QUE LEEA UNIFICACION  NORMALITA INTRA-EXTRA AFORE 
      	
         END IF
        
      OTHERWISE # DE+ AFORES  METLIFE MLM-2221 Se Agregará Consulta para Extraer
                #                              Las Entradas y Salidas de las Unificaciones
                #                              Intra-Extra Afore ISSSTE.
              
         LET g_txt    =              # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
                                     # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se     
            "SELECT A.siefore, ",    # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
               "CASE ",
                   "WHEN A.subcuenta = 13 AND A.tipo_movimiento IN ( 1,3 ) AND A.id_aportante = 'UNI_ISSSTE' ",
                           "THEN '0503' ",                                                                #ENTRADAS 0503 UNIFICACION ISSSTE < LO QUE SE VENIA REPORTANDO >
                   "WHEN A.tipo_movimiento = 1 AND EXISTS ( SELECT 1 FROM taa_rcv_recepcion C ",          #ENTRADAS 0503 ARCHIVO RECEPTORA
                                                           "WHERE  A.folio =  C.folio ",
                                                           "AND    C.ident_operacion  IN ( '09','12' ) ",
                                                           "AND    C.tipo_traspaso  IN ( 12,83 ) ",
                                                           "AND    A.nss  = C.nss  ",
                                                           "AND    A.consecutivo_lote  = C.cont_servicio) ",
                   "THEN '0503' ",
                   "WHEN  A.tipo_movimiento = 1 AND A.id_aportante[1,3] MATCHES  'U[CN]-' ", #ENTRADAS MLM-2221 OCTUBRE 2013 UNIFICACION INTRA -EXTRA AFORE                    
                   "AND   A.subcuenta IN ( 13,30,31,32,33,34 )",
                   "THEN  '0503' ",
                   "WHEN  A.tipo_movimiento = 265  AND A.subcuenta IN ( 13,30,31,32,33,34,39 ) ", #JIRA MLM-2654 265  'ABONO UNIFICACION INTRA-AFORE IMSS-ISSSTE'
                   "THEN  '0503' ",                   
                   "ELSE '0000' ",
            "END CASE , ",
            "NVL(SUM(A.monto_en_pesos),0) ,0 ",        
            "FROM      dis_cuenta  A ",
            "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                 
            "AND       A.subcuenta IN (  13,30,31,32,33,34,39 ) ",  #ENTRADAS PURAS SUBCTAS ISSSTE
            "AND       A.tipo_movimiento IN ( 1,3,265 ) ",
            "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",            
            "GROUP BY 1,2 ", 
            "UNION ",
            "SELECT A.siefore, ",
            "CASE ",
                   "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",
                                   "WHERE A.tipo_movimiento   =  B.marca_cod ",                      
                                   "AND   B.tipo_traspaso IN ( 12,83 ) ",
                                   "AND   A.tipo_movimiento IN ( 240,283 ) ", 
                                   "AND   A.folio             = C.folio ",
                                   "AND   C.tipo_traspaso IN ( 2,4 ) ) ",                    
                   "THEN '0503' ",
                   "WHEN A.tipo_movimiento IN ( 241,242 ) AND A.id_aportante[1,3] MATCHES  'U[CN]-' ", #SALIDAS MLM-2221 OCTUBRE 2013 UNIFICACION INTRA -EXTRA AFORE                    
                   "AND  A.subcuenta IN ( 13,30,31,32,33,34 )  ", 
                   "THEN '0503' ",
                   "WHEN  A.tipo_movimiento = 264  AND A.subcuenta IN ( 13,30,31,32,33,34,39 ) ", #JIRA MLM-2654 264, 'CARGO UNIFICACION INTRA-AFORE IMSS-ISSSTE'
                   "THEN '0503' ",
                   "ELSE '0000' ",
            "END CASE , ",
            "0, NVL(SUM(A.monto_en_pesos),0) ", #SALIDAS CEDENTE
            "FROM      dis_cuenta A ",              
            "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",  
            "AND       A.subcuenta IN (  13,30,31,32,33,34,39 ) ",  #PURAS SUBCTAS ISSSTE                 
            "AND       A.tipo_movimiento IN ( 240,241,242,264,283 ) ",
            "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
            "GROUP BY 1,2 "
              
      END CASE                                                               
      
   LET g_txt = g_txt CLIPPED                                              
   PREPARE qry135 FROM g_txt                                              
   DECLARE c135 CURSOR FOR qry135                                         
                                                                          
   FOREACH c135 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,                 
                     g_rec.sal                                             
                                                                          
      CALL Act_tbl ( g_rec.* )                                            
         
   END FOREACH               
   
   #-- 0510 Unificacion Asignados IMSS   -NUEVO QRY NUEVO NUEVO NUEVO NUEVO NUEVO NUEVO NUEVO NUEVO
   #-- Origenes de Traspasos 20,84 y 85 (Solo recursos IMSS.) 
   #-- Entre dos o más Afores.
   
   #TIPO DE TRASPASO 20 TRASPASO ( ASIGNADO-ADMINISTRADORA) POR UNIFICACION
   #TIPO DE TRASPASO 84 TRASPASO POR REGISTRO ASIGNADO MISMA     ADMINISTRADORA POR UNIFICACION
   #TIPO DE TRASPASO 85 TRASPASO POR REGISTRO ASIGNADO DIFERENTE ADMINISTRADORA POR UNIFICACION
   
   #TIPO DE TRASPASO = 1 POR PROMOTOR  TIPO DE TRASPASO  = 2 COMPLEMENTARIO   TIPO DE TRASPASO = 4 DIVERSOS
         
   CALL inicializa() 

      CASE  g_codigo_afore
            
         WHEN 564  # SOLO AFORE METLIFE  
         
            LET g_txt    =
            
               "SELECT  A.siefore, ",
                  "CASE WHEN C.tipo_traspaso IN ( 2,4 )",
                     "THEN '0510' ",                       
                     "ELSE '0000' ",
                     "END CASE ,0, ",
                     "NVL(SUM(A.monto_en_pesos),0) ",
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ", #SALIDAS CEDENTE 
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                              "AND   B.tipo_traspaso IN ( 20,84,85 ) ) ",
               "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37 ) ",#PURAS SUBCTA IMSS  MLM-2745 Se Agrega Subcuenta 37
               "AND   A.tipo_movimiento IN ( 247,284,285 ) ",                 
               "AND   A.folio  = C.folio ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ", #MODIFICACION 29MARZO2012
               "SELECT    A.siefore, ",
               "'0510', ",#JIRA MLM-1223 07-AGO-2012 CONFERENCIA CARO ,PIDIO CONSIDERE SUBCTAS TMB DE ISSSTE AUNQUE
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS RECEPTORA                   NO LO ESPECIFIQUE EL LAYOUT.
               "FROM   dis_cuenta  A       , ",
                      "taa_rcv_recepcion C ",
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND    A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,37 ) ",#MLM-2745 Se Agrega Subcuenta 37
               "AND    A.tipo_movimiento   =  1 ",
               "AND    A.folio             =  C.folio ",
               "AND    C.ident_operacion  IN ( '09','12' ) ",
               "AND    C.tipo_traspaso    IN ( 20,84,85 ) ",
               "AND    A.nss               = C.nss ",
               "AND    A.consecutivo_lote  = C.cont_servicio ",
               "AND    A.siefore  IN ( 1,2,3,4,5,6 ) ",  
               "GROUP BY 1,2 " 
                
         WHEN 562  # SOLO AFORE INVERCAP
         	
         	  #JIRA INV-1504     Modificacion 21 Septiembre del 2012
         	  LET g_txt    =
            
               "SELECT  A.siefore, ",
                  "CASE WHEN C.tipo_traspaso IN ( 2,4 )",
                     "THEN '0510' ",                       
                     "ELSE '0000' ",
                     "END CASE ,0, ",
                     "NVL(SUM(A.monto_en_pesos),0) ",
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ", #SALIDAS CEDENTE 
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                              "AND   B.tipo_traspaso IN ( 20,84,85 ) ) ",
               "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39  ) ",#PURAS SUBCTA IMSS  E ISSSTE                             
               "AND   A.tipo_movimiento IN ( 247,284,285 ) ",                 
               "AND   A.folio  = C.folio ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ", 
               "SELECT    A.siefore, ",
               "'0510', ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS RECEPTORA 
               "FROM   dis_cuenta  A       , ",
                      "taa_rcv_recepcion C ",
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND    A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39  ) ",
               "AND    A.tipo_movimiento   =  1 ",
               "AND    A.folio             =  C.folio ",
               "AND    C.ident_operacion  IN ( '09','12' ) ",
               "AND    C.tipo_traspaso    IN ( 20,84,85 ) ",
               "AND    A.nss               = C.nss ",
               "AND    A.consecutivo_lote  = C.cont_servicio ",
               "AND    A.siefore  IN ( 1,2,3,4,5,6 ) ",  
               "GROUP BY 1,2 "
         	         
         OTHERWISE # DE+ AFORES                  COPPEL
            
            LET g_txt    =

               "SELECT  A.siefore, ",
                  "CASE WHEN C.tipo_traspaso IN ( 2,4 )",
                     "THEN '0510' ",                       
                     "ELSE '0000' ",
                     "END CASE ,0, ",
                     "NVL(SUM(A.monto_en_pesos),0) ",
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ", #SALIDAS CEDENTE JIRA 	CPL-1064
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                              "AND   B.tipo_traspaso IN ( 20,84,85 ) ) ",
               "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#PURAS SUBCTA IMSS                               
               "AND   A.tipo_movimiento IN ( 247,284,285 ) ",                 
               "AND   A.folio  = C.folio ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ", #MODIFICACION 29MARZO2012
               "SELECT    A.siefore, ",
               "'0510', ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS RECEPTORA 
               "FROM   dis_cuenta  A       , ",
                      "taa_rcv_recepcion C ",
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND    A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",
               "AND    A.tipo_movimiento   =  1 ",
               "AND    A.folio             =  C.folio ",
               "AND    C.ident_operacion  IN ( '09','12' ) ",
               "AND    C.tipo_traspaso    IN ( 20,84,85 ) ",
               "AND    A.nss               = C.nss ",
               "AND    A.consecutivo_lote  = C.cont_servicio ",
               "AND    A.siefore  IN ( 1,2,3,4,5,6 ) ",  
               "GROUP BY 1,2 "         
               
      END CASE
    
   PREPARE qry5122 FROM g_txt
   DECLARE c5122 CURSOR FOR qry5122
 
   FOREACH c5122 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                      g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
   #-----------------
    
   #-- 0511 Separación Asignados IMSS   -NUEVO QRY NUEVO NUEVO NUEVO NUEVO NUEVO NUEVO NUEVO NUEVO
   #-- Origenes de Traspasos 25 ( Solo recursos IMSS ). Entre dos o Más Afores
   
   
   #TIPO DE TRASPASO 25 TRASPASO ( ASIGNADO-ADMINISTRADORA) POR SEPARACION   
   #TIPO DE TRASPASO = 1 POR PROMOTOR  TIPO DE TRASPASO  = 2 COMPLEMENTARIO   TIPO DE TRASPASO = 4 DIVERSOS
         
   CALL inicializa() 
   
      CASE  g_codigo_afore
      	
         WHEN 562  # SOLO AFORE INVERCAP
         	 
         	  LET g_txt    =
         	
               "SELECT  A.siefore, ",
                  "CASE WHEN C.tipo_traspaso IN ( 2,4 ) ",
                     "THEN '0511' ",                       
                     "ELSE '0000' ",
                     "END CASE ,0, ",
                     
                     "NVL(SUM(A.monto_en_pesos),0) ",
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ",#SALIDAS CEDENTE 
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                              "AND   B.tipo_traspaso = 25 ) ",
               "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ",#PURAS SUBCTA IMSS                                                               
               "AND   A.tipo_movimiento = 292 ",                 
               "AND   A.folio  = C.folio ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",        
               "UNION ",	             
               "SELECT  A.siefore, ",  
                       "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",
                            "WHERE A.folio =  C.folio AND C.ident_operacion  IN ( '09','12' ) ",
                            "AND   C.tipo_traspaso = 25 AND A.nss  = C.nss ",
                            "AND   A.consecutivo_lote  = C.cont_servicio) ",
                            "THEN '0511' ",                                                    
                      "ELSE '0000' ",                            
                      "END CASE , ",                             
                      "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS RECEPTORA 
                "FROM   dis_cuenta  A ",                          
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                 "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,13,30,31,32,33,34,39 ) ",#PURAS SUBCTA IMSS                               
                 "AND   A.tipo_movimiento   =  1 ",               
                 "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",          
               "GROUP BY 1,2 "
         	
         	 OTHERWISE # DE+ AFORES           METLIFE   COPPEL
            
              LET g_txt    =
              
                 "SELECT  A.siefore, ",
                    "CASE WHEN C.tipo_traspaso IN ( 2,4 ) ",
                       "THEN '0511' ",                       
                       "ELSE '0000' ",
                       "END CASE ,0, ",
                       "NVL(SUM(A.monto_en_pesos),0) ",
                 "FROM    dis_cuenta A , taa_cd_ctr_folio C ",#SALIDAS CEDENTE 
                 "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                 "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",
                                "WHERE A.tipo_movimiento   =  B.marca_cod  ",
                                "AND   B.tipo_traspaso = 25 ) ",
                 "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37 ) ",#PURAS SUBCTA IMSS #MLM-2745 Se Agrega Subcuenta 37                                                              
                 "AND   A.tipo_movimiento = 292 ",                 
                 "AND   A.folio  = C.folio ",
                 "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                 "GROUP BY 1,2 ",        # Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde
                 "UNION ",	             # Apunta de taa_viv_recepcion a taa_rcv_recepcion y se  
                 "SELECT  A.siefore, ",  # Agrega Filtro A.consecutivo_lote  = C.cont_servicio      
                         "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",
                              "WHERE A.folio =  C.folio AND C.ident_operacion  IN ( '09','12' ) ",
                              "AND   C.tipo_traspaso = 25 AND A.nss  = C.nss ",
                              "AND   A.consecutivo_lote  = C.cont_servicio) ",
                              "THEN '0511' ",                                                    
                        "ELSE '0000' ",                            
                        "END CASE , ",                             
                        "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS RECEPTORA 
                  "FROM   dis_cuenta  A ",                          
                 "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                   "AND   A.subcuenta  IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29,37 ) ",#PURAS SUBCTA IMSS MLM-2745 Se Agrega Subcuenta 37
                   "AND   A.tipo_movimiento   =  1 ",               
                   "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",          
                 "GROUP BY 1,2 "
        
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry5222 FROM g_txt
   DECLARE c5222 CURSOR FOR qry5222
   
   FOREACH c5222 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                      g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
   
   #----------------------

   #06    00    TRANSFERENCIA ENTRE SIEFORES TRANSFERENCIA ENTRE SIEFORES

   #06    01    POR EDAD
   #06    02    POR ELECCION
   #06    03    POR CORRECCION
   #06    04    POR REGISTRO DE ASIGNADOS

         
   CALL inicializa()
   
      CASE  g_codigo_afore
        
         #Se Modifica el T.M 1 por el 55(ABONO) y el
         #T.M 210 por el 255(CARGO) Exclusivamente para el TIPO DE TRASPASO
         #13(TRANSFERENCIA POR EDAD).
         
         #-- Tipo traspaso = 15 solo COPPEL Por Modificacion de Curp
         
         WHEN 562  # SOLO AFORE INVERCAP
         	   
         	  #INV-2702 *08May2014* Se Agregara al Id Cuenta 06 Id Subcuenta 03  
         	  #         (Por Correccion) para que se reporte el Nuevo Tipo de 
         	  #          Traspaso 17(Solicitud liq. indebida reinversión recursos
         	  #          no cobrados) El cual manejará Los ABONOS con T.M = 58
         	  #          en Lugar de 1 y los CARGOS conservaria el Mismo Tipo de Mov.
         	  #          en este caso el T.M 210.
          
            #Se Modifica el T.M 1 por el 55 (ABONO) y el
            #T.M 210 por el 255 (CARGO ) Exclusivamente para el TIPO DE TRASPASO
            #13(TRANSFERENCIA POR EDAD)
            
            LET g_txt    =
  
               "SELECT A.siefore,'0601', ",#06 01    POR EDAD
                      "NVL(SUM (DECODE (A.tipo_movimiento,55 ,A.monto_en_pesos ,0)),0), ",#ENT
                      "NVL(SUM (DECODE (A.tipo_movimiento,255 ,A.monto_en_pesos,0)),0) ",#SAL
                "FROM   dis_cuenta  A,tes_solicitud B ",
                "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                  "AND   A.tipo_movimiento   IN ( 55,255 ) ",
                  "AND   A.folio = B.folio ",
                  "AND   B.folio_solicitud = A.consecutivo_lote ",
                  "AND   B.estado = 103 AND B.tipo_traspaso IN ( 13 ) ",
                  "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",            #Se reportara el 14 Nov 2012 y 23 Nov 2012 por unica Ocación
               "UNION ",                   #Transferencia de recursos de la SB5 a la SB4 .
               "SELECT A.siefore,'0601', ",#06 01    POR EDAD                            
               "NVL(SUM (DECODE (A.tipo_movimiento,926 ,A.monto_en_pesos,0)),0), ",#ENT
               "NVL(SUM (DECODE (A.tipo_movimiento,925 ,A.monto_en_pesos,0)),0)  ",#SAL 
               "FROM   dis_cuenta A ",                                
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                    
               "AND    A.tipo_movimiento   IN ( 925,926 ) ",                                          
               "AND    A.siefore           IN ( 1,2,3,4,5,6 ) ",                        
               "GROUP BY 1,2 ",
               "UNION ", 
               "SELECT A.siefore,'0602', ",#06    02    POR ELECCION
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",#ENT
                      "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",#SAL
               "FROM   dis_cuenta  A,tes_solicitud B ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.tipo_movimiento   IN ( 1,210 ) ",
               "AND     A.folio = B.folio ",
               "AND     B.folio_solicitud = A.consecutivo_lote  ",
               "AND     B.estado = 103 AND B.tipo_traspaso IN (1,2,3,4,6,7,8,9,10,11) ",
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ",               #INV-2702 *08May2014*
               "SELECT A.siefore, ",
                   "CASE ",
                       "WHEN A.tipo_movimiento = 1  THEN '0603' ",#06 03 POR CORRECCION(APORTE)
                       "WHEN A.tipo_movimiento = 58 THEN '0603' ",#06 03 POR CORRECCION(APORTE) Exclusivo Tip. Tra 17
                       "ELSE '0000' ",
                    "END CASE, ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#E N T R A D A S
               "FROM    dis_cuenta  A,tes_solicitud B ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.tipo_movimiento   IN ( 1,58 ) ",
               "AND     A.folio = B.folio ",
               "AND     B.folio_solicitud = A.consecutivo_lote  ",
               "AND     B.estado = 103 AND B.tipo_traspaso IN (12,14,17) ", #New Traspaso 17(Solicitud liq. indebida 
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",      	  
               "GROUP BY 1,2 ", 
               "UNION ",               
               "SELECT A.siefore, ",
                   "CASE ",
                       "WHEN A.tipo_movimiento = 210  THEN '0603' ",#06 03 POR CORRECCION(SALIDA)
                       "ELSE '0000' ",
                    "END CASE, ",
               "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS 
               "FROM    dis_cuenta  A,tes_solicitud B ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.tipo_movimiento   = 210 ",
               "AND     A.folio = B.folio ",
               "AND     B.folio_solicitud = A.consecutivo_lote  ",
               "AND     B.estado = 103 AND B.tipo_traspaso IN (12,14,17) ", #New Traspaso 17(Solicitud liq. indebida 
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",           #reinversión recursos no cobrados)            	  
               "GROUP BY 1,2 ",
               "UNION ", 
               "SELECT A.siefore,'0604', ",#06  04  POR REGISTRO DE ASIGNADOS
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",#ENT
                      "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",#SAL
               "FROM   dis_cuenta  A,tes_solicitud B ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.tipo_movimiento   IN ( 1,210 ) ",
               "AND     A.folio = B.folio ",
               "AND     B.folio_solicitud = A.consecutivo_lote  ",
               "AND     B.estado = 103 AND B.tipo_traspaso IN ( 5,16 ) ",
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
         
         WHEN 568  # SOLO AFORE COPPEL    
             
            LET g_txt    =                                                                                                                  
                                                                                                                                             
               "SELECT A.siefore,'0601', ",#06 01    POR EDAD                                                                                
                      "NVL(SUM (DECODE (A.tipo_movimiento,55 ,A.monto_en_pesos ,0)),0), ",#ENT                                               
                      "NVL(SUM (DECODE (A.tipo_movimiento,255 ,A.monto_en_pesos,0)),0) ",#SAL                                                
                "FROM   dis_cuenta  A,tes_solicitud B ",                                                                                     
                "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                                         
                  "AND   A.tipo_movimiento   IN ( 55,255 ) ",                                                                                
                  "AND   A.folio = B.folio ",                                                                                                
                  "AND   B.folio_solicitud = A.consecutivo_lote ",                                                                           
                  "AND   B.estado = 103 AND B.tipo_traspaso IN ( 0,10,13 ) ",                                                                
                  "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",                                                                           
               "GROUP BY 1,2 ",            #Se reportara el 14 Nov 2012 y 23 Nov 2012 por unica Ocación                                      
               "UNION ",                   #Transferencia de recursos de la SB5 a la SB4 .                                                   
               "SELECT A.siefore,'0601', ",#06 01    POR EDAD                                                                                
               "NVL(SUM (DECODE (A.tipo_movimiento,926 ,A.monto_en_pesos,0)),0), ",#ENT                                                      
               "NVL(SUM (DECODE (A.tipo_movimiento,925 ,A.monto_en_pesos,0)),0)  ",#SAL                                                      
               "FROM   dis_cuenta A ",                                                                                                       
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                                                                           
               "AND    A.tipo_movimiento   IN ( 925,926 ) ",                                                                                 
               "AND    A.siefore           IN ( 1,2,3,4,5,6 ) ",                                                                             
               "GROUP BY 1,2 ",                                                                                                              
               "UNION ",                                                                                                                     
               "SELECT A.siefore,'0602', ",#06    02    POR ELECCION                                                                         
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",#ENT                                                
                      "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",#SAL                                                
               "FROM   dis_cuenta  A,tes_solicitud B ",                                                                                      
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                                          
               "AND     A.tipo_movimiento   IN ( 1,210 ) ",                                                                                  
               "AND     A.folio = B.folio ",                                                                                                 
               "AND     B.folio_solicitud = A.consecutivo_lote  ",                                                                           
               "AND     B.estado = 103 AND B.tipo_traspaso IN ( 1,9 ) ",                                                                     
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",                                                                            
               "GROUP BY 1,2 ",                                                                                                              
               "UNION ",                                                                                                                     
               "SELECT A.siefore,'0603', ",#06    03    POR CORRECCION                                                                       
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",#ENT                                                
                      "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",#SAL                                                
               "FROM   dis_cuenta  A,tes_solicitud B ",                                                                                      
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                                          
               "AND     A.tipo_movimiento   IN ( 1,210 ) ",                                                                                  
               "AND     A.folio = B.folio ",                                                                                                 
               "AND     B.folio_solicitud = A.consecutivo_lote  ",                                                                           
               "AND     B.estado = 103 AND B.tipo_traspaso IN ( 3,4,6,7,8,11,12,14,15 ) ",#--CPL-1677 03 JUL2014 Se Añade Tipo Traspaso 11 Separación de Cuentas 
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",                                                                            
               "GROUP BY 1,2 ",                                                                                                              
               "UNION ",                                                                                                                     
               "SELECT A.siefore,'0604', ",#06  04  POR REGISTRO DE ASIGNADOS                                                                
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",#ENT                                                
                      "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",#SAL                                                
               "FROM   dis_cuenta  A,tes_solicitud B ",                                                                                      
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                                          
               "AND     A.tipo_movimiento   IN ( 1,210 ) ",                                                                                  
               "AND     A.folio = B.folio ",                                                                                                 
               "AND     B.folio_solicitud = A.consecutivo_lote  ",                                                                           
               "AND     B.estado = 103 AND B.tipo_traspaso IN ( 5,16 ) ",                                                                    
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",                                                                            
               "GROUP BY 1,2 "  
               
         OTHERWISE # DE+ AFORES SOLO METLIFE   
         	
         	  LET g_txt    =
            
               "SELECT A.siefore,'0601', ",#06 01    POR EDAD
                      "NVL(SUM (DECODE (A.tipo_movimiento,55 ,A.monto_en_pesos ,0)),0), ",#ENT
                      "NVL(SUM (DECODE (A.tipo_movimiento,255 ,A.monto_en_pesos,0)),0) ",#SAL
                "FROM   dis_cuenta  A,tes_solicitud B ",
                "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                  "AND   A.tipo_movimiento   IN ( 55,255 ) ",
                  "AND   A.folio = B.folio ",
                  "AND   B.folio_solicitud = A.consecutivo_lote ",
                  "AND   B.estado = 103 AND B.tipo_traspaso IN ( 0,10,13 ) ",
                  "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",            #Se reportara el 14 Nov 2012 y 23 Nov 2012 por unica Ocación
               "UNION ",                   #Transferencia de recursos de la SB5 a la SB4 .
               "SELECT A.siefore,'0601', ",#06 01    POR EDAD                            
               "NVL(SUM (DECODE (A.tipo_movimiento,926 ,A.monto_en_pesos,0)),0), ",#ENT
               "NVL(SUM (DECODE (A.tipo_movimiento,925 ,A.monto_en_pesos,0)),0)  ",#SAL 
               "FROM   dis_cuenta A ",                                
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                    
               "AND    A.tipo_movimiento   IN ( 925,926 ) ",                                          
               "AND    A.siefore           IN ( 1,2,3,4,5,6 ) ",                        
               "GROUP BY 1,2 ",
               "UNION ", 
               "SELECT A.siefore,'0602', ",#06    02    POR ELECCION
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",#ENT
                      "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",#SAL
               "FROM   dis_cuenta  A,tes_solicitud B ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.tipo_movimiento   IN ( 1,210 ) ",
               "AND     A.folio = B.folio ",
               "AND     B.folio_solicitud = A.consecutivo_lote  ",
               "AND     B.estado = 103 AND B.tipo_traspaso IN ( 1,9 ) ",
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore,'0603', ",#06    03    POR CORRECCION
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",#ENT
                      "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",#SAL
               "FROM   dis_cuenta  A,tes_solicitud B ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.tipo_movimiento   IN ( 1,210 ) ",
               "AND     A.folio = B.folio ",
               "AND     B.folio_solicitud = A.consecutivo_lote  ",
               "AND     B.estado = 103 AND B.tipo_traspaso IN ( 3,4,6,7,8,12,14,15 ) ",
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore,'0604', ",#06  04  POR REGISTRO DE ASIGNADOS
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_pesos ,0)),0), ",#ENT
                      "NVL(SUM (DECODE (A.tipo_movimiento,210 ,A.monto_en_pesos,0)),0) ",#SAL
               "FROM   dis_cuenta  A,tes_solicitud B ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.tipo_movimiento   IN ( 1,210 ) ",
               "AND     A.folio = B.folio ",
               "AND     B.folio_solicitud = A.consecutivo_lote  ",
               "AND     B.estado = 103 AND B.tipo_traspaso IN ( 5,16 ) ",
               "AND     A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "               

      END  CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry103 FROM g_txt
   DECLARE c103 CURSOR FOR qry103

   FOREACH c103 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
       
END FUNCTION #Fin llena_det_64

FUNCTION llena_det_65()     

   #----------------------

   #07 00  ISSSTE                            INACTIVA
   #07 01  RECAUDACION                       INACTIVA
   #07 02  TRASPASOS                         INACTIVA
   #07 03  RETIROS                           INACTIVA

   #----------------------

      
   #08    00    RETIROS     RETIROS     RETIROS   RETIROS    RETIROS  RETIROS 
   #      01    DISPOSICION TOTAL DE RECURSOS
   #      03    TRANSFERENCIAS AL IMSS Y  GOBIERNO FEDERAL
   #      04    DISPOCISION  PARCIAL POR DESEMPLEO
   #      05    DISPOCISION PARCIAL POR AYUDA DE MATRIMONIO
   #      06    RETIROS  SAR 92

   #      08    TRANSFERENCIA  A 'ASEGURADORAS'       >< AUN NO HAY ><


   #Nuevos T.M para la Subcta id 08 04  Dispocision parcial Desempleo
           --876( ret parcial por desempleo tipo A )
           --877( ret parcial por desempleo tipo B )
           --878( ret parcial por desempleo pago complementario )		
   
   #-----------------
   
   
   #08    04    REINTEGRO DE SEMANAS COTIZADAS 

   CALL inicializa()
   
      CASE  g_codigo_afore      
         
         WHEN 564  # SOLO AFORE METLIFE
         	  #Comment - 14/may/12 11:25 AM 
         	  #MLM-1048  16MAY2012  Movimiento de Reintegro de Semanas Cotizadas se 
         	  #                     debe alojar en el ID 08-04 Disposición Parcial por Desempleo 
         	  #                     T.M = 83 ( REINTEGRO DE SEMANAS COTIZADAS )
         	  #<< SON UN INGRESO >>
         	 
            LET g_txt    =
   
               "SELECT  A.siefore, ",
               "'0804', ",
               "NVL(SUM(A.monto_en_pesos),0) ,0 ", #ENTRADA Reintegro de Semanas Cotizadas
               "FROM   dis_cuenta  A  ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento     = 83  ",
               "AND    A.siefore IN ( 1,2,3,4,5,6 ) ",
               "GROUP  BY 1,2 "
   
         OTHERWISE # DE+ AFORES
   
      END CASE
   
      
   LET g_txt = g_txt CLIPPED
   PREPARE qry1450 FROM g_txt
   DECLARE c1450 CURSOR FOR qry1450
   
   FOREACH c1450 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
      
   #-----------------
   
   CALL inicializa()
   
      CASE g_codigo_afore
   
         WHEN 564  # SOLO AFORE METLIFE
         	
            LET g_txt    =
            
               "SELECT A.siefore, ",
                       "CASE WHEN A.tipo_movimiento IN ( 825,820,830,850,880,10 ) AND A.subcuenta IN ( 1,2,5,6,9 ) ", #SE REPORTARA PURO RCV   		                
                            "THEN '0801' ",
                            "WHEN A.tipo_movimiento IN ( 810,815 ) ",
                            "THEN '0803' ",
                            "WHEN A.tipo_movimiento IN ( 875,876,877,878 ) ",
                            "THEN '0804' ",
                            "WHEN A.tipo_movimiento = 870  ",
                            "THEN '0805' ",
                            "WHEN A.tipo_movimiento IN ( 825,820,830,850,860,880 ) AND A.subcuenta = 7   ", #SE REPORTARA PURO SAR
                            "THEN '0806' ",
                            "WHEN A.tipo_movimiento = 10 AND EXISTS ( SELECT * ",
                                                                     " FROM ret_solicitud_tx B ",
                                                                     " WHERE A.folio = B.folio ",
                                                                     " AND   B.tipo_retiro IN ( 'D','E','G','H','J','M' ) ) ",                        
                            "AND  A.subcuenta =  7 ",                                         
                            "THEN '0806' ",
                            "ELSE '0000' ",
                       "END CASE , ",
                       "0,NVL(SUM(A.monto_en_pesos),0) ",
               "FROM      dis_cuenta  A ",
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
               "AND       A.tipo_movimiento IN (  825,820,830,850,860,880, ", 
                                                 "870,875,876,877,878,810,815,10  ",
                                                " ) ",
               "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 " 
             
         WHEN 568  # SOLO AFORE COPPEL 
                   #Se excluye T.M 800 (TIPO A) 
   
            LET g_txt    =
   
               #NUEVO  OCTUBRE 2011 AL ID CTA 08 IDSUBCTA O1 SE QUITAN T.M 840,860 
               #NUEVO  OCTUBRE 2011 AL ID CTA 08 IDSUBCTA O3 SE AGREGA EL TIPO DE RETIRO 'A´ T.M = 800
               
               "SELECT A.siefore, ",
                     "CASE  WHEN A.tipo_movimiento IN ( 825,820,830,850,880,10 ) AND A.subcuenta NOT IN (3,10,13,16,23) ",
                            "AND NOT EXISTS (SELECT 1 FROM ret_solicitud_tx C ",
                            "WHERE A.folio =  C.folio AND C.tipo_retiro IN ( 'H','F' ) ",
                            "AND A.nss  =  C.nss ) ",
                            "THEN '0801' ",
                            "WHEN  A.tipo_movimiento IN ( 810,815 ) ",
                            "THEN '0803' ",                                                
                            "WHEN A.tipo_movimiento = 800 ",
                            "     AND EXISTS ( SELECT 1 FROM ret_transf_rx C ",
                            "                  WHERE A.folio =  C.folio ",
                            "                  AND   A.nss   =  C.nss ",                        
                            "                  AND   C.cve_destino <> 'A' ) ",
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
               "AND       A.tipo_movimiento IN ( 800,825,820,830,850,880, ", 
                                                 "870,875,876,877,878,810,815,10  ",
                                                 " ) ", 
               "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
   
         WHEN 562  # SOLO AFORE INVERCAP
         	         #NUEVO  OCTUBRE 2011 AL ID CTA 08 IDSUBCTA O1 SE QUITAN T.M 840,860 
   
            LET g_txt    =
            
               "SELECT  A.siefore,'0803',NVL(SUM(A.monto_en_pesos),0),0 ",
               "FROM    dis_cuenta A ",#ENTRADA IMSS  Devolución contingente Aseguradoras
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.id_aportante      = 'IMSS' ",
               "AND     A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ",        
               "SELECT A.siefore, ",
                  "CASE WHEN A.tipo_movimiento IN ( 825,820,830,850,880,10 ) AND A.subcuenta IN ( 1,2,5,6,9 ) ", #SE REPORTARA PURO RCV
                       "THEN '0801' ",
                       "WHEN A.tipo_movimiento IN ( 810,815 ) ",                      
                       "THEN '0803' ",
                       "WHEN A.tipo_movimiento IN ( 875,876,877,878 ) ",
                       "THEN '0804' ",
                       "WHEN A.tipo_movimiento = 870  ",
                       "THEN '0805' ",
                       "WHEN A.tipo_movimiento IN ( 825,820,830,850,860,880 ) AND A.subcuenta = 7   ", #SE REPORTARA PURO SAR
                       "THEN '0806' ",
                       "WHEN A.tipo_movimiento = 10 AND EXISTS ( SELECT * ",
                                                                " FROM ret_solicitud_tx B ",
                                                                " WHERE A.folio = B.folio ",
                                                                " AND   B.tipo_retiro IN ( 'D','E','G','H','J','M' ) ) ",                        
                       "AND  A.subcuenta =  7 ",                                        
                       "THEN '0806' ",
                       "ELSE '0000' ",
                  "END CASE , ",
                  "0,NVL(SUM(A.monto_en_pesos),0) ",
               "FROM      dis_cuenta  A ",
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
               "AND       A.tipo_movimiento IN ( 825,820,830,850,860,880, ", 
                                                 "870,875,876,877,878,810,815,10  ",
                                                " ) ",
               "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
         	
         OTHERWISE # DE+ AFORES
               
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry8 FROM g_txt
   DECLARE c8 CURSOR FOR qry8
   
   FOREACH c8 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
            g_rec.sal
   
      CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH    
   #------------------------------
   
   #08    06    RETIROS  SAR -92

   CALL inicializa()

      CASE  g_codigo_afore

         WHEN 568  # SOLO AFORE COPPEL
         	
            LET g_txt    =

               "SELECT  A.siefore, ",
               "CASE WHEN EXISTS (SELECT 1 FROM ret_solicitud_tx C ",
                  "WHERE A.folio =  C.folio AND C.tipo_retiro = 'H' ",
                  "AND A.nss  =  C.nss ) ",
                  "THEN '0806' ",
               "ELSE '0000' ",
               "END CASE , ",
               "0,NVL(SUM(A.monto_en_pesos),0) ",
               "FROM     dis_cuenta  A ",
               "WHERE    A. fecha_conversion  = '", g_dia_ant , "' ", 
               "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
              
         OTHERWISE # DE+ AFORES
              
      END CASE

      
   LET g_txt = g_txt CLIPPED
   PREPARE qry765 FROM g_txt
   DECLARE c765 CURSOR FOR qry765

   FOREACH c765 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
      
   #-----------------------------

   #08    07    RETIROS  PROGRAMADOS

   CALL inicializa()

      CASE  g_codigo_afore
  
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES

            LET g_txt    =

               "SELECT  A.siefore, ",
               "'0807', ",
               "0,NVL(SUM(A.monto_en_pesos),0) ",     #SALIDAS RETIROS PROGRAMADOS
               "FROM   dis_cuenta  A  ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento     = 841 ",
               "AND    A.siefore IN ( 1,2,3,4,5,6 ) ",#SOLO LIQ.SIF 1
               "GROUP  BY 1,2 "

      END CASE

      
   LET g_txt = g_txt CLIPPED
   PREPARE qry150 FROM g_txt
   DECLARE c150 CURSOR FOR qry150

   FOREACH c150 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

      
   #08    08  TRANSFERENCIA  A 'ASEGURADORAS' POR EL MOMENTO SOLO COPPEL

   CALL inicializa()

      CASE  g_codigo_afore
         
         WHEN 800  # AFORE FICTICIA
         
         WHEN 568  # SOLO AFORE COPPEL

            #JIRA CPL-965
            
            LET g_txt    =

               "SELECT  A.siefore,'0808',NVL(SUM(A.monto_en_pesos),0),0 ",               
               "FROM    dis_cuenta A ",#ENTRADA IMSS  Devolución contingente Aseguradoras
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                      
               "AND     A.id_aportante   MATCHES 'IMSS*' ", 
               "AND     A.tipo_movimiento  = 640 ",                          
               "AND     A.siefore  IN ( 1,2,3,4,5,6 ) ",                                 
               "GROUP BY 1,2 ",              
               "UNION ",
               "SELECT  A.siefore, ",
               "'0808', ",
               "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS TRANSFERENCIA A 'ASEGURADORAS
               "FROM   dis_cuenta  A  ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND    EXISTS ( SELECT 1 FROM ret_transf_rx C ",
               "       WHERE  A.folio =  C.folio ",
               "       AND    A.nss   =  C.nss  ",
               "       AND    C.cve_destino = 'A') ",
               "AND    A.tipo_movimiento    = 800 ",
               "AND    A.siefore IN ( 1,2,3,4,5,6 ) ",
               "GROUP  BY 1,2 "
          
            #Tipo de Movimiento 800 (TIPO A) TRANSFERENCIA A 'ASEGURADORAS'
            #NUEVO  OCTUBRE 2011 AL ID CTA 08 IDSUBCTA O8  T.M = 800
         
         WHEN 562  # SOLO AFORE INVERCAP
         	
         	  LET g_txt    =

               "SELECT  A.siefore,'0808',NVL(SUM(A.monto_en_pesos),0),0 ",               
               "FROM    dis_cuenta A ",#ENTRADA IMSS  Devolución contingente Aseguradoras
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                      
               "AND     A.id_aportante      = 'ASEGURADORA' ",                           
               "AND     A.siefore  IN ( 1,2,3,4,5,6 ) ",                                 
               "GROUP BY 1,2 ",              
               "UNION ",
               "SELECT  A.siefore, ",
               "'0808', ",
               "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS TRANSFERENCIA A 'ASEGURADORAS
               "FROM   dis_cuenta  A  ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento     = 800 ",
               "AND    A.siefore IN ( 1,2,3,4,5,6 ) ",
               "GROUP  BY 1,2 "
         	
         OTHERWISE # DE+ AFORES
         	
         	  LET g_txt    =

              "SELECT  A.siefore, ",
              "'0808', ",
              "0,NVL(SUM(A.monto_en_pesos),0) ",#SALIDAS TRANSFERENCIA A 'ASEGURADORAS
              "FROM   dis_cuenta  A  ",
              "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
              "AND    A.tipo_movimiento     = 800 ",
              "AND    A.siefore IN ( 1,2,3,4,5,6 ) ",
              "GROUP  BY 1,2 "

      END CASE

   
   LET g_txt = g_txt CLIPPED
   PREPARE qry350 FROM g_txt
   DECLARE c350 CURSOR FOR qry350

   FOREACH c350 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #QRYS NUEVOS      NUEVOS      NUEVOS       NUEVOS       NUEVOS     NUEVOS
   #08 11 Plan Privado de Pension
 
 
   CALL inicializa()

      CASE  g_codigo_afore
  
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES

            LET g_txt    =
            
               "SELECT  A.siefore, ",
               "CASE WHEN A.tipo_movimiento = 840 ",
               "     THEN '0811' 	",
               "     WHEN A.tipo_movimiento = 10 AND EXISTS ( SELECT * ",
               "                        FROM ret_solicitud_tx B ",
               "                       WHERE A.folio = B.folio ",
               "                       AND   B.tipo_retiro = 'F' ) ",
               "   THEN '0811' ",
               "   ELSE '0000' ",
               "   END CASE , ",
               "    0,NVL(SUM(A.monto_en_pesos),0) ",
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.tipo_movimiento IN ( 10,840 ) ",
               "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "

      END CASE

      
   LET g_txt = g_txt CLIPPED
   PREPARE qry752 FROM g_txt
   DECLARE c752 CURSOR FOR qry752

   FOREACH c752 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

        
   #-----------------------------           -----------------------------#
   #-----------------------------           -----------------------------#
   
   #QRYS NUEVOS      NUEVOS      NUEVOS       NUEVOS       NUEVOS     NUEVOS
   #0816 SALDOS PREVIOS DE RETIROS  27-ABR-2012
   #Desinversion de recursos de las cuentas individuales de
   #trabajadores por una prospectación de pensión del IMSS,
   #recibida en la Administradora. Se registra un ingreso
   #en los casos de reintegros alas cuentas individuales derivado de:
   #     ° Derecho NO Otorgado durante los 30 días de vigencia
   #       del tramite.
   #     ° Pensión Garantizada con Modalidad de Pago en la AFORE.
   #     ° Negativas de Pensión por Cesantía e Invalidez.
   #     ° Pensiones por Invalidez( Aportaciones posteriores al
   #       bimestre de la Fecha Inicio de Pensión.
   #     ° Pensiones por Incapacidad Permanente con porcentaje de
   #       valuación menor  al 50 %
   
   # Las transferencias o dispocisiones que resulten de las resoluciones
   # emitidas por el IMSS,se registrarán para efectos de este reporte
   # en los apartados de los retiros que correspondan. 
   
         
   #-- JIRA MLM-1032  PURAS SALIDAS | FALTAN LAS ENTRADAS QRY EN CORDINACION CON PEIS
   #-- Comento el Usuario Adriana que no se reportara para las Salidas la Sie = 6
        
   CALL inicializa()
   
      #T.M 921    =  ABONO POR FUSION
      
      
      CASE  g_codigo_afore
      
         WHEN 800  # AFORE FICTICIA
      
         OTHERWISE # DE+ AFORES
      
            LET g_txt    =
               
               "SELECT  A.siefore, ",                                    
               "CASE WHEN A.tipo_movimiento = 924 ",  # ENTRADAS REINVERSION DE RECURSOS
               "   THEN '0816' 	",                                                   
               "   ELSE '0000' ",                                        
               "END CASE , ",                                         
               " NVL(SUM(A.monto_en_pesos),0),0 ",    
               "FROM      dis_cuenta  A ",                               
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",    
               "AND       A.tipo_movimiento IN ( 924 ) ",             
               "AND       A.siefore         IN ( 1,2,3,4,5 ) ",      
               "GROUP BY 1,2 ",                                                         
               "UNION ",
               "SELECT  A.siefore, ",                                    
               "CASE WHEN A.tipo_movimiento = 921 ",  # DESINVERSION DE RECURSOS
               "   THEN '0816' 	",                                                   
               "   ELSE '0000' ",                                        
               "END CASE , ",                                         
               "    0,NVL(SUM(A.monto_en_pesos),0) ",  #SALIDAS SALDOS PREVIOS DE RETIROS                  
               "FROM      dis_cuenta  A ",                               
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",    
               "AND       A.tipo_movimiento IN ( 921 ) ",             
               "AND       A.siefore         IN ( 1,2,3,4,5 ) ",      
               "GROUP BY 1,2 "
               
      END CASE
     
   LET g_txt = g_txt CLIPPED
   PREPARE qry1555 FROM g_txt
   DECLARE c1555 CURSOR FOR qry1555

   FOREACH c1555 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #-----------------------------           -----------------------------#
   #-----------------------------           -----------------------------#
   
   #09    00    DEVOLUCION DE PAGOS  SIN JUSTIFICACION LEGAL
   #      01    IMSS
   #      02    ISSSTE
   #      03    SARISSSTE                      ????????


   CALL inicializa()

      LET g_txt    =

         "SELECT  A.siefore,'0901',0,NVL(SUM(A.monto_en_pesos),0) ",
         "FROM dis_cuenta A ",#SALIDA IMSS
         "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
           "AND   A.subcuenta IN ( 1,2 ) ",
           "AND   A.tipo_movimiento IN ( 540,545,550,555 ) ",
           "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
         "GROUP BY 1,2 "

   LET g_txt = g_txt CLIPPED
   PREPARE qry44 FROM g_txt
   DECLARE c44 CURSOR FOR qry44

   FOREACH c44 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                   g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

   CALL inicializa()

      LET g_txt    =

         "SELECT  A.siefore,'0902',0,NVL(SUM(A.monto_en_pesos),0) ",
         "FROM dis_cuenta A ",#SALIDA ISSSTE
         "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
           "AND   A.subcuenta IN ( 13,30,31,32 ) ",
           "AND   A.tipo_movimiento IN ( 543,544,553,554 ) ",
           "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
         "GROUP BY 1,2 "

   LET g_txt = g_txt CLIPPED
   PREPARE qry23 FROM g_txt
   DECLARE c23 CURSOR FOR qry23
   
   FOREACH c23 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH

   #10    00    OTROS CONCEPTOS         
   #      01    Fondos de Prevision Social                 ????
   #      02    Otros IMSS
   #      03    Otros ISSSTE                               ????
   #      04    Otros Trabajadores Independientes          ????
   #      05    0tros Movimientos  Infonavit               ????
   #      06    Otros Movimientos Fovissste                ????


   #-- 620 ABONO POR AJUSTE OPERATIVO (PAC)          ENTRADA
   #-- 610 CARGO POR AJUSTE OPERATIVO (PAC)          SALIDA
   
   CASE  g_codigo_afore
   
      WHEN 562  # SOLO AFORE INVERCAP
        
         ## T.M 83 NUEVO ( REINTEGRO DE SEMANAS COTIZADAS )
         
         LET g_txt    =  
         
            #JIRA INV - 1779 DIC2012
            "SELECT A.siefore,'1002', ",
                  "NVL(SUM (DECODE (A.tipo_movimiento,620 ,A.monto_en_pesos ,0)),0), ", #ENTRADA
                  "NVL(SUM (DECODE (A.tipo_movimiento,610 ,A.monto_en_pesos,0)),0) ",   #SALIDA
            "FROM   dis_cuenta  A ",
            "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
              "AND   A.tipo_movimiento   IN ( 610,620 ) ",
              "AND   A.subcuenta   IN ( 1,2,3,5,6,7,9,10,11,12,15,16,17,18,20,21,22,23,24,25,26,27,28,29 ) ",#SOLO SUBCTAS IMSS
              "AND   A.siefore     IN ( 1,2,3,4,5,6 ) ",
            "GROUP BY 1,2 ",
            "UNION ",
            "SELECT    A.siefore, ",
                       "'1002', ",
                      "NVL(SUM(A.monto_en_pesos),0) ,0 ", #ENTRADA
            "FROM      dis_cuenta  A ",
            "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
            "AND       A.tipo_movimiento   =  83        ",
            "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
            "GROUP BY 1,2 ",
            "UNION ", #MODIFICACION JIRA INV-1797
            "SELECT    A.siefore, ",
                       "'1002', ",
                      "NVL(SUM(A.monto_en_pesos),0) ,0 ", #ENTRADA
            "FROM      dis_cuenta  A ",
            "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
            "AND       A.tipo_movimiento   =  1        ",
            "AND       A.subcuenta IN ( 1,2,5,6,7,9 ) ",
            "AND       A.id_aportante = 'DEVOLUCION' ",
            "AND       A.siefore   IN ( 1,2,3,4,5,6 ) ",
            "GROUP BY 1,2 "
           
      WHEN 568  # SOLO AFORE COPPEL
       
         ## T.M 83 NUEVO ( REINTEGRO DE SEMANAS COTIZADAS )
        
         LET g_txt    =
         
            "SELECT A.siefore,'1002', ",
                   "NVL(SUM (DECODE (A.tipo_movimiento,620 ,A.monto_en_pesos ,0)),0), ",
                   "NVL(SUM (DECODE (A.tipo_movimiento,610 ,A.monto_en_pesos,0)),0) ",
             "FROM   dis_cuenta  A ",
             "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.tipo_movimiento   IN ( 610,620 ) ",
               "AND   A.siefore           IN ( 1,2,3,4,5,6 ) ",
            "GROUP BY 1,2 ",
            "UNION ",       
            "SELECT  A.siefore, ",                                    
            "CASE WHEN A.tipo_movimiento = 83 ",  # ENTRADA Ya estaba esta Consulta antes de la Modif CPL-927
            "     THEN '1002' 	",                                                    
                " WHEN A.tipo_movimiento = 590 AND A.subcuenta = 7  ", #JIRA CPL-927 Recuperación  de Recursos  SAR92
                "      AND A.id_aportante MATCHES 'REC SAR*' ",        #Y VIVIENDA 92,OFICIO 
                " THEN '1002' 	",                                       
            "   ELSE '0000' ",                                        
            "END CASE , ",                                         
            " NVL(SUM(A.monto_en_pesos),0),0 ",    
            "FROM      dis_cuenta  A ",                               
            "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",    
            "AND       A.tipo_movimiento IN ( 83,590  ) ",             
            "AND       A.siefore         IN ( 1,2,3,4,5 ) ",      
            "GROUP BY 1,2 "
            
     END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry10 FROM g_txt
   DECLARE c10 CURSOR FOR qry10
   
   FOREACH c10 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
             g_rec.sal
   
      CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH 
      
   #10      03    Otros ISSSTE                               
 
   #--------Qrys Nuevos---
  
   CALL inicializa()
    
      CASE  g_codigo_afore  
            
         WHEN 568  # SOLO AFORE COPPEL
         	
         	  LET g_txt    = 
         	     
         	     #16NOV2012 NUM. REQ. JIRA. CPL-1064       
            
               "SELECT  A.siefore, ",                                              
               "CASE WHEN A.subcuenta = 13  AND A.tipo_movimiento = 86 ",          
                         "AND EXISTS ( SELECT 1 FROM  safre_af:dis_cza_devorec C ",
                         "WHERE A.folio =  C.folio ) ",                            
                     "THEN '1003' ",                                               
                     "ELSE '0000' ",                                               
               "END CASE , ",                                                      
               "NVL(SUM(A.monto_en_pesos),0),0 ",                 #ENTRADAS        
               "FROM dis_cuenta A ",                                               
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                  
               "AND   A.subcuenta IN ( 13 ) ",                                     
               "AND   A.tipo_movimiento IN ( 86 ) ",                               
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                              
               "GROUP BY 1,2 ",                                                    
               "UNION ", 
               "SELECT A.siefore, '1003', ",                                                                                   
               "NVL(SUM(A.monto_en_pesos),0),0 ",                #ENTRADAS RECEPTORA                                                                    
               "FROM  dis_cuenta  A, taa_rcv_recepcion C ",                                                                    
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
               "AND   A.folio =  C.folio ",                                                                                    
               "AND   A.subcuenta  IN ( 13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE  QRY OPTIMIZADO
               "AND   A.tipo_movimiento = 1 ",                                                                                 
               "AND   C.ident_operacion  IN ( '09','12' ) ",                                                                   
               "AND   C.tipo_traspaso IN ( 20,25 ) ",            #20 NOV2012 NUM. REQ. JIRA. CPL-1064                                                                        
               "AND   A.nss  = C.nss ",   
               "AND   A.consecutivo_lote  = C.cont_servicio ",                                                                                     
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                          
               "GROUP BY 1,2 ",
               "UNION ",               
               "SELECT  A.siefore, ",                                                           
               "CASE  WHEN A.subcuenta IN ( 30,31,32 ) AND A.tipo_movimiento IN (  87,89 ) ",
                          "AND EXISTS ( SELECT 1 FROM  safre_af:dis_cza_devorec C ",         
                          "WHERE A.folio =  C.folio ) ",                                     
                      "THEN '1003' ",                                                        
                     "ELSE '0000' ",                                                         
                   "END CASE , ",                                                            
               "0,NVL(SUM(A.monto_en_pesos),0) ",                 #SALIDAS                   
               "FROM dis_cuenta A ",                                                         
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                            
               "AND   A.subcuenta IN ( 30,31,32 ) ",                                         
               "AND   A.tipo_movimiento IN ( 87,89 ) ",                                      
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                        
               "GROUP BY 1,2 ",                                                                 
               "UNION ",   
               "SELECT A.siefore, '1003', ",                     #16NOV2012 NUM. REQ. JIRA. CPL-1064                                                                                    
               "0,NVL(SUM(A.monto_en_pesos),0) ",                #SALIDAS   CEDENTE                                                                       
               "FROM  dis_cuenta  A,taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",                                                  
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                                                 
               "AND   A.folio =  C.folio ",                                                                                       
               "AND   A.subcuenta  IN ( 13,30,31,32,33,34,39 )",#PURAS SUBCTA IMSS  QRY OPTIMIZADO 
               "AND   A.tipo_movimiento IN ( 247,292 ) ",                                                                         
               "AND   A.tipo_movimiento   =  B.marca_cod ",                                                                       
               "AND   B.tipo_traspaso IN ( 20,25 )  ",                                                                             
               "AND   C.tipo_traspaso IN ( 2,4 ) ",                                                                               
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                                             
               "GROUP BY 1,2 "
        
         WHEN 564  # SOLO AFORE METLIFE      
         	 
         	  LET g_txt    = 
            
               "SELECT  A.siefore, ",                                                                               
               "CASE WHEN A.subcuenta = 13  AND A.tipo_movimiento = 86 ",                                     
                         "AND EXISTS ( SELECT 1 FROM  safre_af:dis_cza_devorec C ",                           
                         "WHERE A.folio =  C.folio ) ",                                                       
                     "THEN '1003' ",              #ENTRADAS  DEVOLUCION CAMBIO DE REGIMEN proceso de David                       
                     "ELSE '0000' ",                                                                          
               "END CASE , ",                                                                                 
               "NVL(SUM(A.monto_en_pesos),0),0 ",                                                             
               "FROM dis_cuenta A ",                                                                          
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                             
               "AND   A.subcuenta IN ( 13 ) ",                                                                
               "AND   A.tipo_movimiento IN ( 86 ) ",                                                          
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                         
               "GROUP BY 1,2 ",                                                                               
               "UNION ",                                                                                      
               "SELECT  A.siefore, ",                                                                         
               "CASE  WHEN A.subcuenta IN ( 30,31,32 ) AND A.tipo_movimiento IN (  87,89 ) ",                   
                          "AND EXISTS ( SELECT 1 FROM  safre_af:dis_cza_devorec C ",                          
                          "WHERE A.folio =  C.folio ) ",                                                      
                      "THEN '1003' ",               #SALIDAS  DEVOLUCION CAMBIO DE REGIMEN proceso de David                                                                                              
                     "ELSE '0000' ",                                                                         
                   "END CASE , ",                                                                            
               "0,NVL(SUM(A.monto_en_pesos),0) ",                                                            
               "FROM dis_cuenta A ",                                                                         
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                            
               "AND   A.subcuenta IN ( 30,31,32 ) ",                                                         
               "AND   A.tipo_movimiento IN ( 87,89 ) ",                                                      
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                        
               "GROUP BY 1,2 "

         WHEN 562  # SOLO AFORE INVERCAP
         	
         	  LET g_txt    =  
         	  
         	     #JIRA INV - 1779 DIC2012
               "SELECT A.siefore,'1003', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,620 ,A.monto_en_pesos ,0)),0), ", #ENTRADA
               "NVL(SUM (DECODE (A.tipo_movimiento,610 ,A.monto_en_pesos,0)),0) ",   #SALIDA
               "FROM     dis_cuenta  A ",
               "WHERE    A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND      A.tipo_movimiento   IN ( 610,620 ) ",
               "AND      A.subcuenta IN (  13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE
               "AND      A.siefore IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 ",
               "UNION ",
         	     "SELECT  A.siefore, ",                                                                               
               "CASE WHEN A.subcuenta = 13  AND A.tipo_movimiento = 86 ",                                     
                         "AND EXISTS ( SELECT 1 FROM  safre_af:dis_cza_devorec C ",                           
                         "WHERE A.folio =  C.folio ) ",                                                       
                     "THEN '1003' ",              #ENTRADAS  DEVOLUCION CAMBIO DE REGIMEN proceso de David                                                                                                  
                     "ELSE '0000' ",                                                                          
               "END CASE , ",                                                                                 
               "NVL(SUM(A.monto_en_pesos),0),0 ",                                                             
               "FROM dis_cuenta A ",                                                                          
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                             
               "AND   A.subcuenta IN ( 13 ) ",                                                                
               "AND   A.tipo_movimiento IN ( 86 ) ",                                                          
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                         
               "GROUP BY 1,2 ",                                                                               
               "UNION ",          
               #--  02 ABRIL 2012
               #--  288 ( CARGO SEPARACION DE CUENTAS ISSSTE ) 
               "SELECT  A.siefore, ",                                                                         
               "CASE  WHEN A.subcuenta IN ( 30,31,32 ) AND A.tipo_movimiento IN (  87,89 ) ",                   
                          "AND EXISTS ( SELECT 1 FROM  safre_af:dis_cza_devorec C ",                          
                          "WHERE A.folio =  C.folio ) ",                                                      
                     "THEN '1003' ",               #SALIDAS  DEVOLUCION CAMBIO DE REGIMEN proceso de David                                                                                               
                     "WHEN  A.tipo_movimiento = 288 AND A.subcuenta = 13 AND A.id_aportante = 'SEP-ISS' ",
                     "THEN '1003' ",#SALIDAS 288 ( CARGO SEPARACION DE CUENTAS ISSSTE )                  
                     "ELSE '0000' ",                                                                         
                   "END CASE , ",                                                                            
               "0,NVL(SUM(A.monto_en_pesos),0) ",                                                            
               "FROM dis_cuenta A ",                                                                         
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                            
               "AND   A.subcuenta IN ( 13,30,31,32 ) ",                                                         
               "AND   A.tipo_movimiento IN ( 87,89,288 ) ",                                                      
               "AND   A.siefore IN ( 1,2,3,4,5,6 ) ",                                                        
               "GROUP BY 1,2 " 

      END  CASE 

   LET g_txt = g_txt CLIPPED
   PREPARE qry652 FROM g_txt
   DECLARE c652 CURSOR FOR qry652

   FOREACH c652 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
 
   #--------------------------------------------------------------------
   #11   00    RECAUDACION ISSSTE  RECAUDACION ISSSTE RECAUDACION ISSSTE    
   
   #11   01    RCV
   #11   02    SARISSSTE (SAR 92)
   #11   03    Cuotas gubernamentales
   #11   05    Ahorro solidario                 
   #11   06    Redención de bono
   
   #--------Qrys Nuevos---
  
   CALL inicializa()
    
      CASE  g_codigo_afore
          
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES 
             
            LET g_txt    =

               "SELECT    A.siefore, ",
                           "'1101', ",
                           "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENT
               "FROM dis_cuenta A , dis_det_issste B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   A.subcuenta    IN ( 30,31 ) ",
               "AND   A.tipo_movimiento IN( 1,4,17,3 ) ",
               "AND   A.id_aportante NOT  MATCHES '[CT]I-*' ",
               #"AND   B.ind_trab_bono     <> 1 ",
               "AND   B.ident_tipo_aport  = '01' ",
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
    
     END  CASE 

   LET g_txt = g_txt CLIPPED
   PREPARE qry52 FROM g_txt
   DECLARE c52 CURSOR FOR qry52

   FOREACH c52 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
 
   CALL inicializa()
    
      CASE  g_codigo_afore
          
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES 
             
            LET g_txt    =

               "SELECT    A.siefore, ",
                           "'1102', ",
                           "NVL(SUM(A.monto_en_pesos),0) ,0 ",
               "FROM dis_cuenta A , dis_det_issste B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   A.subcuenta         = 13  ",
               "AND   A.tipo_movimiento IN( 1,4,17,3 ) ",
               "AND   A.id_aportante NOT  MATCHES '[CT]I-*' ",
               #AND   B.ind_trab_bono     = ' ' ",
               "AND   B.ident_tipo_aport  = '01' ",
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 " 

      END  CASE 

   LET g_txt = g_txt CLIPPED
   PREPARE qry53 FROM g_txt
   DECLARE c53 CURSOR FOR qry53

   FOREACH c53 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

         
   #11   04    Intereses en tránsito  

   CALL inicializa()
     
      CASE  g_codigo_afore

         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES

            LET g_txt    = 
    
               "SELECT  A.siefore, ",
               "'1104', ",
               " NVL(SUM(A.monto_en_pesos),0),0 ",#ENT. INT. EN TRANSITO ISSTE
               "FROM   dis_cuenta  A, dis_cza_inttran C  ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND    A.folio               = C.folio ",
               "AND    A.tipo_movimiento     IN ( 2,4) ",
               "AND    A.siefore             IN ( 1,2,3,4,5,6 ) ",
               "AND    A.subcuenta           IN (13,30,31,33,34) ",
               "GROUP  BY 1,2 "
      END CASE
 
   LET g_txt = g_txt CLIPPED
   PREPARE qry74 FROM g_txt
   DECLARE c74 CURSOR FOR qry74

   FOREACH c74 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

   CALL inicializa()
   
      CASE  g_codigo_afore
         
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES 
            
            LET g_txt    =              
                                                                                                              
               "SELECT  A.siefore, ",                                                               
                   "CASE WHEN A.subcuenta = 32  AND A.tipo_movimiento IN ( 1,4,17,3 ) ",            
                        "AND A.id_aportante NOT  MATCHES '[CT]I-*' ",                               
                        "AND EXISTS  ( SELECT 1 FROM dis_det_issste B ",                            
                        "              WHERE  B.ident_tipo_aport  = '01' ",                         
                        "              AND  A.consecutivo_lote  =  B.consec_reg_lote ",             
                        "              AND  A.folio   = B.folio ) ",                                
                        "THEN '1103' ",                                                             
                        "WHEN A.subcuenta IN ( 33,34 )  AND A.tipo_movimiento IN ( 1,4,17,3 ) ",    
                        "AND A.id_aportante NOT  MATCHES '[CT]I-*' ",                               
                        "AND EXISTS  ( SELECT 1 FROM dis_det_issste B ",                            
                        "              WHERE  B.ident_tipo_aport  = '01' ",                         
                        "              AND  A.consecutivo_lote  =  B.consec_reg_lote ",             
                        "              AND  A.folio   = B.folio ) ",                                
                        "THEN '1105' ",                                                             
                        "WHEN A.subcuenta = 34 AND A.tipo_movimiento = 1 ",                         
                        "AND A.id_aportante MATCHES 'APO_AHOSOL*' ",                                
                        "THEN '1105' ",#ENT AHORRO-SOL-VENTANILLA                                                                                                                 
                        "ELSE '0000' ",                                                             
                        "END CASE , ",                                                              
                        "NVL(SUM(A.monto_en_pesos),0),0 ",                                          
                "FROM dis_cuenta A  ",                                                              
                "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",                                  
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                             
                "GROUP BY 1,2 "                              
              
      END  CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry54 FROM g_txt
   DECLARE c54 CURSOR FOR qry54

   FOREACH c54 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #--#--#--#
   
   #11   06    ENT REDENCION DE BONO

   #Subcuenta = 30    RETIRO ISSSTE
   #T.M       = 33    ABONO REDENCION ANTICIPADA
   #Me comento David 28-02-2011 QUE ASÍ, VA APLICAR PARA TODAS LAS AFORES.    
    
   CALL inicializa()
     
      CASE  g_codigo_afore

         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES

            LET g_txt    = 
    
               "SELECT  A.siefore, ",
               "'1106', ",
               " NVL(SUM(A.monto_en_pesos),0),0 ",#ENT REDENCION DE BONO
               "FROM   dis_cuenta  A ",
               "WHERE  A.fecha_proceso       = '", g_dia_ant , "' ",                 
               "AND    A.tipo_movimiento     = 33 ",
               "AND    A.siefore             IN ( 1,2,3,4,5,6 ) ",
               "AND    A.subcuenta           =  30 ",
               "AND    A.id_aportante  MATCHES  'REDEN*' ",
               "GROUP  BY 1,2 ",                 
               "UNION ",    
               "SELECT  A.siefore, ",
               "'1106', ",
               " NVL(SUM(A.monto_en_pesos),0),0 ",#ENT REDENCION DE BONO
               "FROM   dis_cuenta  A ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",                 
               "AND    A.tipo_movimiento     = 1 ",
               "AND    A.siefore             IN ( 1,2,3,4,5,6 ) ",
               "AND    A.subcuenta           =  30 ",
               "AND    A.id_aportante  MATCHES  'REDEN BONO' ",
               "GROUP  BY 1,2 ",
               "UNION ",                                                            
               "SELECT  A.siefore, ",   #Jueves 24 May 2012 Liquido Coppel                                            
               "'1106', ",                                                          
               "NVL(SUM(A.monto_en_pesos),0),0 ",#Revalorización Anticipada de BONO
               "FROM   dis_cuenta  A ",                                             
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                  
               "AND    A.tipo_movimiento     = 33 ",                                
               "AND    A.siefore             IN ( 1,2,3,4,5,6 ) ",                  
               "AND    A.subcuenta           =  30 ",                               
               "AND    A.id_aportante  MATCHES  'BONO PENS-A' ",                    
               "GROUP  BY 1,2 "
               
      END CASE
 
   LET g_txt = g_txt CLIPPED
   PREPARE qry474 FROM g_txt
   DECLARE c474 CURSOR FOR qry474

   FOREACH c474 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

 
   #--#--#--#
   
   #11 07    Aclaraciones RCV 
       
   CALL inicializa()
   
      CASE  g_codigo_afore
       
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES 
          
            LET g_txt    =

               "SELECT    A.siefore, ",
                           "'1107', ",
                           "NVL(SUM(A.monto_en_pesos),0) ,0 ",
               "FROM dis_cuenta A , dis_det_issste B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   A.subcuenta    IN ( 30,31 ) ",
               "AND   A.tipo_movimiento IN( 1,4,17,3 ) ",
               "AND   A.id_aportante NOT  MATCHES '[CT]I-*' ",
               #"AND   B.ind_trab_bono     = 1 ",
               "AND   B.ident_tipo_aport  = '02' ",
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "

      END  CASE 

   LET g_txt = g_txt CLIPPED
   PREPARE qry55 FROM g_txt
   DECLARE c55 CURSOR FOR qry55
   
   FOREACH c55 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
      
   #11 08    Aclaraciones SARISSSTE

   CALL inicializa()
   
      CASE  g_codigo_afore
     
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES 
         
            LET g_txt    =

               "SELECT    A.siefore, ",
                           "'1108', ",
                           "NVL(SUM(A.monto_en_pesos),0) ,0 ",
               "FROM dis_cuenta A , dis_det_issste B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   A.subcuenta         = 13  ",
               "AND   A.tipo_movimiento IN( 1,4,17,3 ) ",
               "AND   A.id_aportante NOT  MATCHES '[CT]I-*' ",
               #"AND   B.ind_trab_bono       <> 1 ",
               "AND   B.ident_tipo_aport  = '02' ",
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
	
      END  CASE 

   LET g_txt = g_txt CLIPPED
   PREPARE qry56 FROM g_txt
   DECLARE c56 CURSOR FOR qry56

   FOREACH c56 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
 
      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #11 09    Aclaraciones Cuotas Gubernamentales 
   #11 10    Aclaraciones Ahorro Solidario	

   CALL inicializa()
   
      CASE  g_codigo_afore
   
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES 
       
            LET g_txt    =

               "SELECT  A.siefore, ",
                   "CASE WHEN A.subcuenta = 32  AND A.tipo_movimiento IN ( 1,4,17,3 ) ",
                        "AND A.id_aportante NOT  MATCHES '[CT]I-*' ",
                        "THEN '1109' ",
                        "WHEN A.subcuenta IN ( 33,34 )  AND A.tipo_movimiento IN ( 1,4,17,3 ) ", 
                        "AND A.id_aportante NOT  MATCHES '[CT]I-*' ", 
                        "THEN '1110' ",
                        "ELSE '0000' ",
                        "END CASE , ",
                        "NVL(SUM(A.monto_en_pesos),0),0 ",
                "FROM dis_cuenta A , dis_det_issste B ",
                "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   A.folio             = B.folio ",
                "AND   B.ident_tipo_aport  = '02' ",
                "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 "

      END  CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry57 FROM g_txt
   DECLARE c57 CURSOR FOR qry57

   FOREACH c57 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal

      CALL Act_tbl ( g_rec.* )


   END FOREACH


   #QRYS NUEVOS      NUEVOS      NUEVOS       NUEVOS       NUEVOS     NUEVOS
   
   #11 14 ACR ISSSTE               
   #11 15 Voluntarias ISSSTE   
   
   CALL inicializa()
   
      CASE  g_codigo_afore
      
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES 
       
            LET g_txt    =

               "SELECT    A.siefore, ",                            
               "'1114', ",                             
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",     
               "FROM dis_cuenta A , dis_det_issste B ",            
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",  
               "AND   A.folio             = B.folio ",             
               "AND   A.subcuenta         = 11  ",                 
               "AND   A.tipo_movimiento   = 1  ",          
               "AND   A.id_aportante NOT  MATCHES '[CT]I-*' ",                 
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",  
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",             
               "GROUP BY 1,2 "
      END  CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry751 FROM g_txt
   DECLARE c751 CURSOR FOR qry751

   FOREACH c751 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

      CALL Act_tbl ( g_rec.* )


   END FOREACH

   CALL inicializa()
   
      CASE  g_codigo_afore
      
         WHEN 800  # AFORE FICTICIA
         	
         OTHERWISE # DE+ AFORES 
       
            LET g_txt    =
                
		           "SELECT    A.siefore, ",                            
               "'1115', ",                             
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",     
               "FROM dis_cuenta A , dis_det_issste B ",            
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",  
               "AND   A.folio             = B.folio ",             
               "AND   A.subcuenta         = 3  ",
               "AND   A.tipo_movimiento IN( 1 ) ",          
               "AND   A.id_aportante NOT  MATCHES '[CT]I-*' ",                 
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",  
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",             
               "GROUP BY 1,2 "
               
      END  CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry7521 FROM g_txt
   DECLARE c7521 CURSOR FOR qry7521

   FOREACH c7521 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

      CALL Act_tbl ( g_rec.* )


   END FOREACH

   #12 00 RETIROS ISSSTE  RETIOS ISSSTE  RETIROS ISSSTE RETIROS ISSSTE    

   #12 01 RETIROS TOTALES
   #12 02 RETIROS PARCIALES POR DESEMPLEO
   #12 03 RETIRO  SAR 92 
   #12 04 RETIROS PROGRAMADOS
   #12 05 TRANSFERENCIA A ASEGURADORAS

   CALL inicializa()

      CASE  g_codigo_afore
            
         WHEN 568  # SOLO AFORE COPPEL 
            
            LET g_txt    = 
             
               "SELECT A.siefore, ",                                                                                                       
               "CASE WHEN (  A.tipo_movimiento IN ( 851,852,853,854,858 ) ",
               "OR ( A.tipo_movimiento = 10 AND A.subcuenta = 13 AND NOT EXISTS ( SELECT 1 FROM  ",
                      "ret_sol_issste_tx B WHERE A.folio = B.folio AND A.nss = B.nss ",
                      "AND  B.tipo_retiro = 'E' ) ) )",                                        
                      "THEN '1201' ",                                                                                                
                       "WHEN A.tipo_movimiento = 864 AND A.subcuenta IN ( 30,31,32,33,34 ) ",#CPL-1359 liquidación de un retiro ISSSTE tipo 'I'
                      "     AND  EXISTS ( SELECT 1 FROM  ret_sol_issste_tx B ",
                      "    WHERE A.folio = B.folio AND A.nss = B.nss AND  B.tipo_retiro = 'I' ) ",
                      "THEN '1201' ",                 
                      "WHEN A.tipo_movimiento = 856 ",                                                                               
                      "THEN '1202' ",           
                      "WHEN ( A.tipo_movimiento IN ( 855 ) OR ( A.tipo_movimiento = 10 AND A.subcuenta = 13  ",
                      "AND  EXISTS ( SELECT 1 FROM  ret_sol_issste_tx C WHERE A.folio = C.folio AND A.nss = C.nss  ",
                      "AND  C.tipo_retiro = 'E'  ) ) ) ",
                      "THEN '1203' ",
                      "WHEN A.tipo_movimiento = 864 AND A.subcuenta = 13  ",               
                      "     AND  EXISTS ( SELECT 1 FROM  ret_sol_issste_tx B ",                      
                      "    WHERE A.folio = B.folio AND A.nss = B.nss AND  B.tipo_retiro = 'I' ) ",   
                      "THEN '1203' ",
                      "WHEN A.tipo_movimiento = 857 ",                                                                               
                      "THEN '1204' ",                                                                                                
                      "WHEN A.tipo_movimiento IN ( 862,866 ) ",                                                                      
                      "THEN '1205' ",                                                                                                
                      "ELSE '0000' ",                                                                                                
                 "END CASE , ",                                                                                                      
                 "0,NVL(SUM(A.monto_en_pesos),0) ",#Salidas                                                                                  
               "FROM      dis_cuenta  A ",                                                                                                   
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                                                                       
               "AND       A.tipo_movimiento IN ( 851,852,853,854,855,856,857,858, ",                                                         
                                                 "862,864,866,10 ) ",                                                                            
               "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",                                                                          
               "GROUP BY 1,2 "  
                                
         WHEN 562  # SOLO AFORE INVERCAP     
    
            #INV-2121  1201 como salida con tipo de movimiento 864 en donde debe de considerar las subcuentas 13,30,31,32,33,34 
            
            LET g_txt    =

               "SELECT A.siefore, ",
                     "CASE WHEN (  A.tipo_movimiento IN ( 851,852,853,854,855,858 ) OR ( A.tipo_movimiento = 10 AND A.subcuenta = 13 ) ) ",
                            "THEN '1201' ",
                            "WHEN  A.tipo_movimiento = 864 AND A.subcuenta IN ( 13,30,31,32,33,34 ) ",
                            " AND  EXISTS ( SELECT 1 FROM  ret_sol_issste_tx B ",
                            "    WHERE A.folio = B.folio AND A.nss = B.nss AND  B.tipo_retiro = 'I' ) ",
                            "THEN '1201' ",
                            "WHEN A.tipo_movimiento = 856 ",
                            "THEN '1202' ",
                            "WHEN A.tipo_movimiento = 857 ",
                            "THEN '1204' ",
                            "WHEN A.tipo_movimiento IN ( 862,866 ) ",
                            "THEN '1205' ",
                            "ELSE '0000' ",
                       "END CASE , ",
                       "0,NVL(SUM(A.monto_en_pesos),0) ", #SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
               "AND       A.tipo_movimiento IN ( 851,852,853,854,855,856,857,858, ",
                                                 "862,864,866,10 ) ", 
               "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
            
         WHEN 564  # SOLO AFORE METLIFE
         	
         	  LET g_txt    =
         	  
         	     "SELECT A.siefore, ",
                    "CASE WHEN A.tipo_movimiento IN ( 851,852,853,854,855,858,864 ) AND A.subcuenta IN ( 30,31,32,33,34 ) ",#SE REPORTARÁ PURO RCV  MLM-2221 Se Agrego Tipo de Mov. 864(DISP. ASEGURADORA POR PAGO DE PENSION) tipo_retiro I
                           "THEN '1201' ",
                           "WHEN A.tipo_movimiento = 856 ",
                           "THEN '1202' ",                           
                           "WHEN A.tipo_movimiento IN ( 851,852,853,854,855,858,864,10 ) AND A.subcuenta = 13 ",#SE REPORTARÁ PURO SAR  MLM-2221 Se Agrego Tipo de Mov. 864(DISP. ASEGURADORA POR PAGO DE PENSION) tipo_retiro I
                           "THEN '1203' ",
                           "WHEN A.tipo_movimiento = 857 ",
                           "THEN '1204' ",
                           "WHEN A.tipo_movimiento IN ( 862,866 ) ",
                           "THEN '1205' ",
                           "ELSE '0000' ",
                      "END CASE , ",
                      "0,NVL(SUM(A.monto_en_pesos),0) ",
               "FROM      dis_cuenta  A ",
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
               "AND       A.tipo_movimiento IN ( 851,852,853,854,855,856,857,858, ",
                                                 "862,864,866,10 ) ", 
               "AND       A.siefore           IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "

      END CASE
          
   LET g_txt = g_txt CLIPPED
   PREPARE qry37  FROM g_txt
   DECLARE c37    CURSOR FOR qry37

   FOREACH c37 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
 	            g_rec.sal

      CALL Act_tbl ( g_rec.* ) 

   END FOREACH 

   #  TRASPASOS ISSSTE   TRASPASOS ISSSTE   TRASPASOS ISSSTE  TRASPASOS ISSSTE  

   #13  00 TRASPASOS ISSSTE 
   #13  01 TRASPASOS AFORE-AFORE
   #13 	02 TRASPASOS PENSIONISSSTE INACTIVA - AFORE ( Unificacion Sar-Issste )
   #13  03 TRASPASOS PENSIONISSSTE INACTIVA - PENSIONISSSTE ACTIVA
   #13  04 TRASPASOS ICEFA - AFORE o PENSIONISSSTE
   #13  05 TRASPASOS  COMPLEMENTARIOS
   
   
   #------------------  
   #13  01 TRASPASOS AFORE-AFORE  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO 
   
   CALL inicializa()                                                                                                              
                                                                                                                               
      CASE g_codigo_afore                                                                                                         
          
         #MODIFICACION JIRA MLM-1086 SE AGREGO EL TIPO DE TRASPASO 74 AUNQUE NO ESTE INCLUIDO
         #EN EL CATALOGO DE MOVIMIENTOS DE RECURSOS.
         
         #MLM-2644 Se Incorporara el Tipo de Traspaso 72 Tipo Movimiento
         #         272.
         
         WHEN 564  # SOLO AFORE METLIFE	     
         
            LET g_txt    =                                                                                                       
                                                                                                                                 
               "SELECT  A.siefore, ",                                                                                            
                  "CASE WHEN C.tipo_traspaso IN (1,4) ", # 1=POR PROMOTOR | 4=DIVERSOS                                           
                     "THEN '1301' ",                                                                                                                
                     "ELSE '0000' ",                                                                                             
                     "END CASE ,0, ",                                                                                            
                     "NVL(SUM(A.monto_en_pesos),0) ",                                                                            
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ",#SALIDAS                                                             
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",                                                         
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",                                                      
                              "AND   B.tipo_traspaso IN  ( 01,02,38,55,71,72,74 ) ) ", #MLM-2644 Se Incorporo el Tipo de Traspaso 72                                                 
               "AND   A.subcuenta IN (  13,30,31,32,33,34,39  ) ",#PURAS SUBCTA ISSSTE       
               "AND   A.tipo_movimiento IN ( 220,295,293,294,271,272,248 ) ",#MLM-2644 Se Incorporo el Tipo de Movimiento 272                                                                                                           
               "AND   A.folio             = C.folio ",                                                                           
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                           
               "GROUP BY 1,2 ",                                                                                                  
               "UNION ",	             
               "SELECT  A.siefore, ",  
                       "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",  
                            "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ", #NORMAL                                   
                            " AND   C.tipo_traspaso IN  ( 01,02,38,55,71,72,74 )  AND A.nss  = C.nss ",#MLM-2644 Se Incorporo el Tipo de Traspaso 72                                 
                            " AND A.consecutivo_lote  = C.cont_servicio ) ",
                            "THEN '1301' ",                                                                                                                                                                              
                      "ELSE '0000' ",                                                                                            
                      "END CASE , ",                                                                                             
                      "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS #ENTRADAS RECEP                                                
               "FROM   dis_cuenta  A ",                                                                                          
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                               
                "AND   A.subcuenta IN (  13,30,31,32,33,34,39  ) ",#PURAS SUBCTA ISSSTE       
                "AND   A.tipo_movimiento   =  1 ",                                                                               
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                          
              "GROUP BY 1,2 "
         
         WHEN 562  # SOLO AFORE INVERCAP
         	   
         	   #JIRA INV-1504     Modificacion 21 Septiembre del 2012
         	   #13 01   Traspasos Afore - Afore
         	  
         	   #01 Abr 2013 INV-1983 
         	   
         	   LET g_txt    =                                                                                                       
                                                                                                                                 
                "SELECT A.siefore, ",
                    "CASE ",                      
                            "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",
                            "               WHERE  A.tipo_movimiento    =  B.marca_cod  ",                     
                            "                AND   B.tipo_traspaso IN  ( 01,02,38,55,57 ) ",
                            "                AND   A.subcuenta IN ( 13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE 
                            "                AND   A.tipo_movimiento IN ( 220,257,293,294,295 ) ",
                            "                AND   A.folio              =  C.folio ",
                            "                AND   C.tipo_traspaso IN ( 1,4 ) )", # 1=POR PROMOTOR | 4=DIVERSOS
                            "THEN '1301' ",
                            "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",   #25 Jul 2013 INV-2257  Se agrega subcta 12 y 16  
                            "               WHERE  A.tipo_movimiento    =  B.marca_cod  ",                     
                            "                AND   B.tipo_traspaso IN  ( 71,72,73,74 ) ",
                            "                AND   A.subcuenta IN ( 10,12,13,16,23,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE 
                            "                AND   A.tipo_movimiento IN ( 248,271,272,273 ) ",          #Subcta 10 y Subcta = 23 
                            "                AND   A.folio              =  C.folio ",           #Pueden Ser Subctas IMSS é ISSSTE
                            "                AND   C.tipo_traspaso IN ( 1,4 ) )",# 1=POR PROMOTOR | 4=DIVERSOS                      
                            "THEN '1301' ",                                                             
                        "ELSE '0000' ",
                "END CASE , ",
                "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDA CEDENTE
                "FROM      dis_cuenta  A ",
                "WHERE     A.fecha_conversion  = '", g_dia_ant , "'  ",
                "AND       A.subcuenta  IN ( 10,12,13,16,23,30,31,32,33,34,39 ) ", # PURAS SUBCTA ISSSTE  Subcta 10 y Subcta = 23 Pueden Ser Subctas IMSS é ISSSTE
                "AND       A.tipo_movimiento IN ( 220,257,293,294,295,248,271,272,273 )  ",
                "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 ",
                "UNION ",	                                                                                                                                                  
                "SELECT  A.siefore, ",                                                                                                                   
                   "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",                                                                               
                      "WHERE A.folio =  C.folio AND C.ident_operacion  = '09'", #NORMAL                                                                  
                      " AND   C.tipo_traspaso IN  ( 01,02,38,55,57 ) ",                                                                                  
                      " AND   A.subcuenta IN ( 13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE                                                             
                      " AND   A.nss  = C.nss ",                                                                                                          
                      " AND   A.consecutivo_lote  = C.cont_servicio   ) ",                                                                               
                      "THEN '1301' ",                                         #25 Jul 2013 INV-2257  Se agrega subcta 12 y 16                                                                                   
                      "WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",                                                                                 
                      "WHERE A.folio =  C.folio AND C.ident_operacion  = '09'",#NORMAL                                                              
                      " AND   C.tipo_traspaso IN  ( 71,72,73,74 ) ",                                                                                           
                      " AND   A.subcuenta IN ( 10,12,13,16,23,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE                                                          
                      " AND   A.nss  = C.nss ",                            #Subcta 10 y Subcta = 23                                                      
                      " AND   A.consecutivo_lote  = C.cont_servicio   ) ", #Pueden Ser Subctas IMSS é ISSSTE                                              
                      "THEN '1301' ",                                                                                                                    
                   "ELSE '0000' ",                                                                                                                       
                   "END CASE , ",                                                                                                                        
                "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS #ENTRADAS RECEP                                                                              
                "FROM   dis_cuenta  A ",                                                                                                                 
                "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                      
                "AND    A.subcuenta  IN ( 10,12,13,16,23,30,31,32,33,34,39 ) ", # PURAS SUBCTA ISSSTE  Subcta 10 y Subcta = 23 Pueden Ser Subctas IMSS é ISSSTE
                "AND    A.tipo_movimiento   =  1 ",                                                                                                      
                "AND    A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                                                 
                "GROUP BY 1,2 " 
         		
         OTHERWISE # DE+ AFORES        #39 CV ISSSTE PATRON    #COPPEL         
                                       #REQ CPL-757 SE INCORPORARÁ LA SUBCUENTA 39 CV ISSSTE PATRON   Entradas y salidas
                                                                                
            LET g_txt    =                                                                                                       
                                                                                                                                 
               "SELECT  A.siefore, ",                                                                                                   
                  "CASE WHEN C.tipo_traspaso IN (1,4) ",                                          # 1=POR PROMOTOR | 4=DIVERSOS                                           
                     "THEN '1301' ",                                                                                                                
                     "ELSE '0000' ",                                                                                             
                     "END CASE ,0, ",                                                                                            
                     "NVL(SUM(A.monto_en_pesos),0) ",                                                                            
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ",                                       #SALIDAS                                                             
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",                                                         
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",                                                      
                              "AND   B.tipo_traspaso IN  ( 01,02,38,55,71,51,57,72,74,84,85 ) ) ", #20 NOV2012 NUM. REQ. JIRA. CPL-1064                                                    
               "AND   A.subcuenta IN (  13,30,31,32,33,34,39  ) ",                                 #PURAS SUBCTA ISSSTE       
               "AND   A.tipo_movimiento IN ( 220,295,293,294,271,290,297,272,248,284,285 ) ",                                                                
               "AND   A.folio             = C.folio ",                                                                           
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                           
               "GROUP BY 1,2 ",                                                                                                  
               "UNION ",	             
               "SELECT  A.siefore, ",  
                       "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",  
                            "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ", #NORMAL                                   
                            " AND   C.tipo_traspaso IN  ( 01,02,38,55,71,51,57,72,74,84,85 )  AND A.nss  = C.nss ", #20 NOV2012 NUM. REQ. JIRA. CPL-1064                                
                            " AND A.consecutivo_lote  = C.cont_servicio ) ",
                            "THEN '1301' ",                                                                                                                                                                              
                      "ELSE '0000' ",                                                                                            
                      "END CASE , ",                                                                                             
                      "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS #ENTRADAS RECEP                                                
               "FROM   dis_cuenta  A ",                                                                                          
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                               
                "AND   A.subcuenta IN (  13,30,31,32,33,34,39  ) ",#PURAS SUBCTA ISSSTE       
                "AND   A.tipo_movimiento   =  1 ",                                                                               
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                          
              "GROUP BY 1,2 "                                                                                                   
         
      END CASE                                                            
                                                                                                                                                                  
   LET g_txt = g_txt CLIPPED                                          
   PREPARE qry2334 FROM g_txt                                            
   DECLARE c2334 CURSOR FOR qry2334                                         
                                                               
   FOREACH c2334 INTO   g_rec.sie,g_rec.id_ctasub,g_rec.ent,               
                        g_rec.sal                                          
                                                                
      CALL Act_tbl ( g_rec.* )                                         
                                                                
                                                                     
   END FOREACH   
    
   #------------------
   CALL inicializa() 
     
      #ID CTA 13 ID SUBCUENTA 02  UNIFICACION DE TRASPASOS PENSIONISSSTE-AFORE
      #Movimiento de recursos de la Cuenta de PensionISSSTE que se encuentra
      #en Banco de Mexico hacia una Administradora,como resultado de un proceso
      #de unificacion de recursos  a una Administradora y se invierten los 
      #Recursos en Siefore(Incluyendo los recursos SARISSSTE)
      
      CASE  g_codigo_afore           
                                      
         WHEN 564  # SOLO AFORE METLIFE 
               
         WHEN 568  # SOLO AFORE COPPEL
         
            #CPL-987 SE INGRESO A PETICION DE LA AFORE   
            #ENTRADAS TRA-ICE-AFO-PENSIONISSTE           
            #E N T R A D A S                             
            #UNIFICACION DE TRASPASOS PENSIONISSSTE-AFORE
            
            #CPL-1466 Se Añade a la primera consulta del Id Cuenta 13 Id Subcuenta 02
            #         El verificar que solo reporte lo liquidado del Proceso de Unificacion
            #         Sar - ISSSTE para eso verifica que el folio se encuentre en tra_det_trasp_sal_issste		
            
            LET g_txt    =
               
               "SELECT  A.siefore, ",                         
               "CASE WHEN A.id_aportante  MATCHES '[CT]I-*' ",   
                    "AND  EXISTS ( SELECT 1 FROM safre_af:tra_det_trasp_sal_issste C ",             
                          "              WHERE A.folio =  C.folio )",
               "     THEN '1302' ",                           
               "     WHEN A.id_aportante = 'UNI_ISSSTE' ",    
               "     THEN '1302' ",
               "     ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_pesos),0),0 ",   #ENTRADAS 
               "FROM   dis_cuenta  A ",
               "WHERE  A. fecha_conversion  = '", g_dia_ant , "' ", 
               "AND    A.subcuenta IN ( 13,19 ) ",         
               "AND    A.tipo_movimiento IN ( 1,4 ) ",
               "AND    A.siefore     IN ( 1,2,3,4,5,6 ) ",
               "GROUP BY 1,2 "
         
         OTHERWISE # DE+ AFORES             
                      
         
            LET g_txt    =                                                           
                      
               "SELECT    A.siefore, ",                                                                    
               "'1302', ",                                                            
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS TRA-ICE-AFO-PENSIONISSTE  
               "FROM     dis_cuenta  A ",                                             
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                 
               "AND       A.subcuenta          IN   ( 13,19 ) ",                      
               "AND       A.tipo_movimiento    IN   ( 1,4 ) ",                        
               "AND       A.id_aportante      MATCHES '[CT]I-*' ",                    
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                  
               "GROUP BY 1,2 ",                                                                         
               "UNION ",                                                                               
               "SELECT    A.siefore, ",                                                                
               "'1302', ",                                                                             
               "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS UNIFICACION SAR-ISSSTE                     
               "FROM     dis_cuenta  A ",                                                              
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                  
               "AND       A.subcuenta          IN   ( 13,19 ) ",                                       
               "AND       A.tipo_movimiento    IN   ( 1,4 ) ",                                         
               "AND       A.id_aportante      MATCHES '[CT]UI-*' ",                                    
               "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                                   
               "GROUP BY 1,2 "                                                       
                               
      END CASE
                               
   LET g_txt = g_txt CLIPPED
   PREPARE qry199 FROM g_txt
   DECLARE c199 CURSOR FOR qry199
        
   FOREACH c199 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
        
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
      
   #13 04  Traspasos Icefa - Afore o PENSIONISSSTE - Afore
   
   CALL inicializa() 
   
      CASE  g_codigo_afore           
                                  
         WHEN 564  # SOLO AFORE METLIFE 
         
             LET g_txt    =
         
                  "SELECT    A.siefore, ",                                                                
                  "'1304', ",                                                                             
                  "NVL(SUM(A.monto_en_pesos),0) ,0 ",#ENTRADAS UNIFICACION SAR-ISSSTE                     
                  "FROM     dis_cuenta  A ",                                                              
                  "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                                  
                  "AND       A.subcuenta          IN   ( 13,19 ) ",                                       
                  "AND       A.tipo_movimiento    IN   ( 1,4 ) ",                                         
                  "AND       A.id_aportante      MATCHES '[CT]I-*' ",                                    
                  "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",                                   
                  "GROUP BY 1,2 "                            
                 
         OTHERWISE # DE+ AFORES 
         
      END CASE
        
   LET g_txt = g_txt CLIPPED
   PREPARE qry99 FROM g_txt
   DECLARE c99 CURSOR FOR qry99
   
   FOREACH c99 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
      
   #------------------
   #13  05 TRASPASOS  Complementarios  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO  NUEVO 
   
   CALL inicializa()                                                                                                              
                                                                                                                               
      CASE g_codigo_afore  
      	
         WHEN 800  # AFORE FICTICIA                                                                                           
      
         WHEN 568  # SOLO AFORE COPPEL
         	
         	         #OJO FALTA LA PARTE DE CEDENTE LO REFERENTE A   REQ CPL-757 Comment 29/mar/12 05:22 PM 
         	         #EN MI CORREO ESTA EL DETALLE DE ESTO EN LA CARPETA DE amigos_out_trab
         	         
         	         #39 CV ISSSTE PATRON REQ CPL-757 SE INCORPORARÁ LA SUBCUENTA 39 CV ISSSTE PATRON   Entradas y salidas
         	         
         	  LET g_txt    =  
         	    
         	     "SELECT  A.siefore, ",                                                                                            
                "CASE WHEN C.tipo_traspaso = 2 ", # 2= COMPLEMENTARIOS                                           
                   "THEN '1305' ",                                                                                                                
                   "ELSE '0000' ",                                                                                             
                   "END CASE ,0, ",                                                                                            
                    "NVL(SUM(A.monto_en_pesos),0) ",                                                                            
                "FROM    dis_cuenta A , taa_cd_ctr_folio C ",#SALIDAS                                                             
                "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
                "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",                                                         
                               "WHERE A.tipo_movimiento   =  B.marca_cod  ",                                                      
                               "AND   B.tipo_traspaso IN  ( 01,02,38,55,71,51,57,72,74,84,85 ) ) ",  #JIRA SOLICITUD CPL 920 SE AGREGAN 51,57                                                
                "AND   A.subcuenta IN (  13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE       
                "AND   A.tipo_movimiento IN ( 220,295,293,294,271,290,297,272,248,284,285 ) ",                                                                
                "AND   A.folio             = C.folio ",                                                                           
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                           
                "GROUP BY 1,2 ",         
         	      "UNION ", #MODIFICACION NUM REQ CPL-757 Comment 29/mar/12 05:22 PM
         	      "SELECT  A.siefore, ", #no comentado   03 ABRIL 2012 SE LES SOLICITO NO NORMATIVO
                         "CASE WHEN  C.ident_operacion  = '12' ", " AND   C.tipo_traspaso IN  ( 01,02,38,55,71,72,74,84,85 )", #20 NOV2012 NUM. REQ. JIRA. CPL-1064
                               "THEN '1305' ",
                               "WHEN  C.ident_operacion  = '12' ", " AND   C.tipo_traspaso IN  ( 51,57 )", #JIRA SOLICITUD  CPL-920
                               "AND  C.cve_ced_cuenta  NOT IN ( 517,531 ) ",
                              "THEN '1305' ",
                         "ELSE '0000' ",                                                                                            
                         "END CASE , ",                                                                                             
                         "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS #ENTRADAS RECEP                                                
                "FROM   dis_cuenta  A , ",#ENTRADAS  ENTRADAS RECEPTORA                                                                                          
                "       taa_rcv_recepcion C ",
                "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                                                               
                "AND    A.subcuenta IN (  13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE       
                "AND    A.tipo_movimiento   =  1 ",                                                                               
                "AND    A.siefore   IN ( 1,2,3,4,5,6 ) ",                                                         
                "AND    A.folio =   C.folio ",
                "AND    A.nss   =   C.nss ",
                "AND    A.consecutivo_lote  = C.cont_servicio ",
                "GROUP BY 1,2 " 
         
         WHEN 564  # SOLO AFORE METLIFE
         	
         	#MLM-2644 Se Incorporara el Tipo de Traspaso 72 Tipo Movimiento  
         	#         272.                                                   
         	
         	 LET g_txt    =                                                                                                       
                                                                                                                                 
               "SELECT  A.siefore, ",                                                                                            
                  "CASE WHEN C.tipo_traspaso = 2 ", # 2= COMPLEMENTARIOS                                           
                     "THEN '1305' ",                                                                                                                
                     "ELSE '0000' ",                                                                                             
                     "END CASE ,0, ",                                                                                            
                     "NVL(SUM(A.monto_en_pesos),0) ",                                                                            
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ",#SALIDAS                                                             
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                              
               "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B  ",                                                         
                              "WHERE A.tipo_movimiento   =  B.marca_cod  ",                                                      
                              "AND   B.tipo_traspaso IN  ( 01,02,38,55,71,72,74 ) ) ",#MLM-2644 Se Incorporo el Tipo de Traspaso 72                                                     
               "AND   A.subcuenta IN (  13,30,31,32,33,34 ) ",#PURAS SUBCTA ISSSTE       
               "AND   A.tipo_movimiento IN ( 220,295,293,294,271,272,248 ) ", #MLM-2644 Se Incorporo el Tipo de Movimiento 272                                                               
               "AND   A.folio             = C.folio ",                                                                           
               "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                           
               "GROUP BY 1,2 ",                                                                                                  
               "UNION ",# Modificacion 23FEB2012 JIRA CPL-757 Se cambia tabla donde	                                                                                                       
               "SELECT  A.siefore, ",# Apunta de taa_viv_recepcion a taa_rcv_recepcion y se                                                                                            
                       "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ", # Agrega Filtro A.consecutivo_lote  = C.cont_servicio                                                     
                            "WHERE A.folio =  C.folio AND C.ident_operacion  = '12' ", #COMPLEMENTARIOS                                   
                            " AND   C.tipo_traspaso IN  ( 01,02,38,55,71,72,74 )  AND A.nss  = C.nss ", #MLM-2644 Se Incorporo el Tipo de Traspaso 72                                 
                            " AND A.consecutivo_lote  = C.cont_servicio ) ",
                            "THEN '1305' ",
                            "ELSE '0000' ", 
                      "END CASE , ",   
                      "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS #ENTRADAS RECEP 
               "FROM   dis_cuenta  A ",                                               
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   A.subcuenta IN (  13,30,31,32,33,34 ) ",#PURAS SUBCTA ISSSTE       
                "AND   A.tipo_movimiento   =  1 ",                                                                               
                "AND   A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                          
               "GROUP BY 1,2 "
         	
         WHEN 562  # SOLO AFORE INVERCAP                                        	                                                             
              	   # JIRA INV-1504     Modificacion 21 Septiembre del 2012    
                   # 13 05   Traspasos Complementarios
                   # 01 Abr 2013 INV-1983
         	   
         	  LET g_txt    =                                                                                                       
                                                                                                                                
                "SELECT A.siefore, ",
                    "CASE ",                      
                            "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ",
                            "               WHERE  A.tipo_movimiento    =  B.marca_cod  ",                     
                            "                AND   B.tipo_traspaso IN  ( 01,02,38,55,57 ) ",
                            "                AND   A.subcuenta IN ( 13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE 
                            "                AND   A.tipo_movimiento IN ( 220,257,293,294,295 ) ",
                            "                AND   A.folio              =  C.folio ",
                            "                AND   C.tipo_traspaso = 2  )", # 2 = Complementarios
                            "THEN '1305' ",
                            "WHEN EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B,taa_cd_ctr_folio C ", #25 Jul 2013 INV-2257  Se agrega subcta 12 y 16  
                            "               WHERE  A.tipo_movimiento    =  B.marca_cod  ",                     
                            "                AND   B.tipo_traspaso IN  ( 71,72,73,74 ) ",
                            "                AND   A.subcuenta IN ( 10,12,13,16,23,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE 
                            "                AND   A.tipo_movimiento IN ( 248,271,272,273 ) ",          #Subcta 10 y Subcta = 23 
                            "                AND   A.folio              =  C.folio ",           #Pueden Ser Subctas IMSS é ISSSTE
                            "                AND   C.tipo_traspaso = 2  )",# 2 = Complementarios                     
                            "THEN '1305' ",                                                             
                        "ELSE '0000' ",
                "END CASE , ",
                "0, NVL(SUM(A.monto_en_pesos),0) ",#SALIDA CEDENTE
                "FROM      dis_cuenta  A ",
                "WHERE     A.fecha_conversion  = '", g_dia_ant , "'  ",
                "AND       A.subcuenta  IN ( 10,12,13,16,23,30,31,32,33,34,39 ) ", # PURAS SUBCTA ISSSTE  Subcta 10 y Subcta = 23 Pueden Ser Subctas IMSS é ISSSTE
                "AND       A.tipo_movimiento IN ( 220,257,293,294,295,248,271,272,273 )  ",
                "AND       A.siefore            IN ( 1,2,3,4,5,6 ) ",
                "GROUP BY 1,2 ",
                "UNION ",	                                                                                                                                                  
                "SELECT  A.siefore, ",                                                                                                                   
                   "CASE WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",                                                                               
                      "WHERE A.folio =  C.folio AND C.ident_operacion  = '12'", #COMPLEMENTARIOS                                                                 
                      " AND   C.tipo_traspaso IN  ( 01,02,38,55,57 ) ",                                                                                  
                      " AND   A.subcuenta IN ( 13,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE                                                             
                      " AND   A.nss  = C.nss ",                                                                                                          
                      " AND   A.consecutivo_lote  = C.cont_servicio   ) ",                                                                               
                      "THEN '1305' ",                                           #25 Jul 2013 INV-2257  Se agrega subcta 12 y 16                                                                                  
                      "WHEN EXISTS (SELECT 1 FROM taa_rcv_recepcion C ",                                                                                 
                      "WHERE A.folio =  C.folio AND C.ident_operacion  = '12'", #COMPLEMENTARIOS                                                              
                      " AND   C.tipo_traspaso IN  ( 71,72,73,74 ) ",                                                                                           
                      " AND   A.subcuenta IN ( 10,12,13,16,23,30,31,32,33,34,39 ) ",#PURAS SUBCTA ISSSTE                                                          
                      " AND   A.nss  = C.nss ",                            #Subcta 10 y Subcta = 23                                                      
                      " AND   A.consecutivo_lote  = C.cont_servicio   ) ", #Pueden Ser Subctas IMSS é ISSSTE                                              
                      "THEN '1305' ",                                                                                                                    
                   "ELSE '0000' ",                                                                                                                       
                   "END CASE , ",                                                                                                                        
                "NVL(SUM(A.monto_en_pesos),0),0 ",#ENTRADAS #ENTRADAS RECEP                                                                              
                "FROM   dis_cuenta  A ",                                                                                                                 
                "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                                                                                      
                "AND    A.subcuenta  IN ( 10,12,13,16,23,30,31,32,33,34,39 ) ", # PURAS SUBCTA ISSSTE  Subcta 10 y Subcta = 23 Pueden Ser Subctas IMSS é ISSSTE
                "AND    A.tipo_movimiento   =  1 ",                                                                                                      
                "AND    A.siefore  IN ( 1,2,3,4,5,6 ) ",                                                                                                 
                "GROUP BY 1,2 "                                                                                                
      
      END CASE                                                            
                                                                                                                                                                  
   LET g_txt = g_txt CLIPPED                                          
   PREPARE qry2335 FROM g_txt                                            
   DECLARE c2335 CURSOR FOR qry2335                                         
                                                               
   FOREACH c2335 INTO   g_rec.sie,g_rec.id_ctasub,g_rec.ent,               
                        g_rec.sal                                          
                                                             
      CALL Act_tbl ( g_rec.* )                                         
                                                             
                                                                  
   END FOREACH   
    
   #------------------     
   
   #13  06 RESARCIMIENTOS POR DEVOLUCION DE COMISIONES
   
   #13  07 RESARCIMIENTOS DIFERENCIAL EN RENDIMIENTOS

   CALL inicializa()
     
      CASE  g_codigo_afore
         
         WHEN 568  # SOLO AFORE COPPEL
    
            LET g_txt    =
                                                                       #16NOV2012 NUM. REQ. JIRA. CPL-1064
               "SELECT   A.siefore, ",                                                                                    
                       "'1307', ",                       
                       "0,NVL(SUM(A.monto_en_pesos),0) ",              #Salidas Cedente
               "FROM    dis_cuenta  A ,taa_cd_ctr_folio C ",                                                              
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                                                       
               "AND     A.folio               = C.folio ",                                                                  
               "AND     C.tipo_traspaso       =  2 ",                  # Complementarios
               "AND     A.tipo_movimiento   IN ( 225,273 ) ",          # Tipo de Traspaso = 21 Y 73.                                                               
               "AND     A.subcuenta   IN ( 13,30,31,32,33,34,39 ) ",   ##PURAS SUBCTA ISSSTE  QRY OPTIMIZADO                                                                   
               "AND     A.siefore     IN ( 1,2,3,4,5,6 ) ",                                                                 
               "GROUP BY 1,2 "
               
         OTHERWISE # DE+ AFORES                 
   
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry123 FROM g_txt
   DECLARE c123 CURSOR FOR qry123
   
   FOREACH c123 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
   
   CALL Act_tbl ( g_rec.* )
   
   END FOREACH

#---------------- VIVIENDA                     VIVIENDA ----------------------#
#---------------- VIVIENDA                     VIVIENDA ----------------------#
#---------------- VIVIENDA                     VIVIENDA ----------------------#
#---------------- VIVIENDA                     VIVIENDA ----------------------#
#---------------- VIVIENDA                     VIVIENDA ----------------------#
#---------------- VIVIENDA      VIVIENDA       VIVIENDA ----------------------#
#--Se llena la parte de VIVIENDA para el flujo diario de Movimientos de Vivienda
 


   #RECAUDACION RECAUDACION VIVIENDA VIVIENDA VIVIENDA
   #0113 Cuota Patronal Infonavit                         VIVIENDA
   #0114 Aclaraciones Cuotas Patronal Infonavit           VIVIENDA
   #0115 Asignación Infonavit                             VIVIENDA
   
   CALL inicializa()
    
      CASE  g_codigo_afore
   
         WHEN 800  # AFORE FICTICIA
             
         OTHERWISE # DE+ AFORES
             
            #Antes 0 Dispersion    Ordinaria  Ahora  ident_arch  =   0  con ident_separacion  =  '  '                  
            #Antes 1 Aclaraciones  Ordinarias Ahora  ident_arch  =   0  con ident_separacion <>  '  '                  
            #Antes 2 Aclaraciones  Especiales Ahora  ident_arch  =   2  con ident_separacion <>  '  '                  
            #Antes 3 Dispersion    Asignacion         De Momento no Hay Cambios Definidos y al parecer no van a llegar 
            #Antes 4 Dispersion    Por Sep. de Ctas.  De Momento no Hay Cambios Definidos y al parecer no van a llegar 
            
             
             LET g_txt    =
   
                "SELECT  A.siefore, ",
                   "CASE WHEN A.subcuenta IN ( 4,8 ) AND A.tipo_movimiento IN ( 1,3,4 ) ",
                   "AND  B.ident_arch = 0 ",
                   "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                   "              WHERE A.folio =  C.folio ",     
                   "              AND   A.nss   =  C.n_seguro ",                   
                   "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                   "              AND   C.ident_separacion = '  '  )",
                   "THEN '0113' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Agrego  ident_arch = 0
                   "WHEN A.subcuenta IN ( 4,8 )  AND A.tipo_movimiento IN ( 1,3,4 ) ",
                   "AND  B.ident_arch IN ( 0,2 ) ",
                   "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                   "              WHERE A.folio =  C.folio ",     
                   "              AND   A.nss   =  C.n_seguro ",                   
                   "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                   "              AND   C.ident_separacion <> '  '  )",
                   "THEN '0114' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Agrego ident_arch = 0
                   "WHEN A.subcuenta IN ( 4,8 )  AND A.tipo_movimiento IN ( 1,3,4 ) ",
                   "AND  B.ident_arch      =  3   ",
                   "THEN '0115' ",#No cambia ya no llegarian este tipo de Archivos
                   "ELSE '0000' ",
             "END CASE , ",
             "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS
             "FROM dis_cuenta A , dis_cza_aporte B ",
             "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
             "AND   A.folio             = B.folio ",
             "AND   A.siefore           =  11 ",
             "GROUP BY 1,2 "
             
      END CASE
   
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry100 FROM g_txt
   DECLARE c100 CURSOR FOR qry100
   
   FOREACH c100 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
   
 
   #--------------                                           ------------------#        
   #0116 43 Bis (Ordinarias)                         VIVIENDA #SALIDA
   #0117 Aclaraciones 43 Bis                         VIVIENDA #SALIDA
   #0118 Asignación 43 Bis                           VIVIENDA #SALIDA           
   #--------------                                           ------------------#  
   
   #Se va por fecha proceso por lo siguiente:
   #Fecha Pago       JUEVES 1ERO DIA INHABIL  
   #Fecha Valor      JUEVES 1ERO DIA INHABIL
   #Fecha Conversion JUEVES 1ERO DIA INHABIL
   #Fecha Proceso    MARTES 06 DE ABRIL ( Se Inserto este dia en dis_cuenta )
   #MIERCOLES 07 DE ABRIL ( Reporto lo del Martes )
    
   #Antes 0 Dispersion    Ordinaria  Ahora  ident_arch  =   0  con ident_separacion  =  '  '                    
   #Antes 1 Aclaraciones  Ordinarias Ahora  ident_arch  =   0  con ident_separacion <>  '  '                  
   #Antes 2 Aclaraciones  Especiales Ahora  ident_arch  =   2  con ident_separacion <>  '  '                  
   #Antes 3 Dispersion    Asignacion         De Momento no Hay Cambios Definidos y al parecer no van a llegar 
   #Antes 4 Dispersion    Por Sep. de Ctas.  De Momento no Hay Cambios Definidos y al parecer no van a llegar 
   
   CALL inicializa() 
   
      CASE  g_codigo_afore
        
         #ident_arch = 0           DISPERSION RCV
         #ident_separacion  = '  '  DISPERSION ORDINARIA
         #ident_separacion <> '  '  ACLARACIONES    
         #ident_arch  = 1  Gubernamentales  
         #ident_arch  = 2  Aclaraciones Esp.
   
         WHEN 800  # AFORE FICTICIA
         	
         OTHERWISE # DE+ AFORES
   
             LET g_txt    =
   
                "SELECT  A.siefore, ",
                     "CASE WHEN A.subcuenta IN ( 4,8 ) AND A.tipo_movimiento IN ( 7,8,38 ) ",
                          "AND  B.ident_arch = 0 ",  
                          "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                          "              WHERE A.folio =  C.folio ",     
                          "              AND   A.nss   =  C.n_seguro ",                   
                          "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                          "              AND   C.ident_separacion = '  '  )",                
                          "THEN '0116' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0                                   
                          "WHEN A.subcuenta IN ( 4,8 ) AND A.tipo_movimiento IN ( 7,8,38 ) ",
                          "AND  B.ident_arch IN ( 0,2 ) ",  
                          "AND  EXISTS ( SELECT 1 FROM safre_af:dis_det_aporte C ",   
                          "              WHERE A.folio =  C.folio ",     
                          "              AND   A.nss   =  C.n_seguro ",                   
                          "              AND   A.consecutivo_lote =   C.consec_reg_lote ",
                          "              AND   C.ident_separacion <> '  '  )", 
                          "THEN '0117' ",#JIRA MLM-1955 26JUN2013 RECAUDACION PARALELO Se Añadio ident_arch = 0                                                                       
                          "WHEN A.subcuenta IN ( 4,8 ) AND A.tipo_movimiento IN ( 7,8,38 ) ",
                          "AND  B.ident_arch      =  3 ",
                          "THEN '0118' ",#No Cambia queda Igual.
                      "ELSE '0000' ",
                    "END CASE , ",
                "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS  
                "FROM dis_cuenta A ,dis_cza_aporte B ",
                "WHERE A.fecha_proceso     = '", g_dia_ant , "' ",             
                "AND   A.folio             =  B.folio  ",
                "AND   A.siefore           =  11 ",
                "GROUP BY 1,2 "
                
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry300 FROM g_txt
   DECLARE c300 CURSOR FOR qry300
   
   FOREACH c300 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
   

   # TRASPASOS TRASPASOS  TRASPASOS  VIVIENDA VIVIENDA VIVIENDA  VIVIENDA    
   
   #0209 Traspasos Ordinarios Infonavit            VIVIENDA
   #0210 Traspasos Complementarios Infonavit       VIVIENDA
   #0211 Traspasos Vivienda 92 Infonavit - Afore   VIVIENDA

   
   CALL inicializa()
   
      CASE g_codigo_afore
   
         WHEN 800  # AFORE FICTICIA          	
   
         WHEN 564  # SOLO AFORE METLIFE 
         	
         	 LET g_txt    =
         	 
         	    "SELECT  A.siefore, ",
                   "CASE WHEN C.tipo_traspaso IN (1,3,4) ",
                        "THEN '0209' ",#TRA ORDINARIOS INFONAVIT
                        "WHEN C.tipo_traspaso  = 2 ",
                        "THEN '0210' ",#TRA COMPLEMENTARIOS INFONAVIT
                   "ELSE '0000' ",
                   "END CASE ,0, ",
                   "NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
              "FROM    dis_cuenta A , taa_cd_ctr_folio C ",
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",
                                "WHERE A.tipo_movimiento   =  B.marca_cod ) ",
                "AND   A.tipo_movimiento   BETWEEN  200 AND 299 ",
                "AND   A.folio             = C.folio ",
                "AND   NOT EXISTS ( SELECT 1 FROM taa_cd_indebidos d WHERE A.nss = ",
                "                   d.nss  AND d.estado = 103 ",
                "AND A.fecha_conversion = d.fecha_liquidacion ) ",
                "AND   A.siefore  = 11  ",
                "GROUP BY 1,2 ",
              "UNION ",            #SOLO RECEPTORA
       	      "SELECT  A.siefore, ",
              "CASE WHEN EXISTS ( SELECT 1 FROM taa_viv_recepcion C ",
                         "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ) ",                        
                   "THEN '0209' ",#TRA ORDINARIOS INFONAVIT                  
                   "WHEN EXISTS ( SELECT 1 FROM taa_viv_recepcion C ",               # Se sumarà la Parte de los Traspasos
                        "WHERE A.folio =  C.folio AND C.ident_operacion  = '12' ",   # Complementarios solo aquellos folios
                        "AND   C.cve_ced_cuenta IN ( 517,531 ) ) ",                  # que Vengan como Prestadora de Servicios JIRA MLM-915.                             
                   "THEN '0209' ",#TRA COMPLEMENTARIOS INFONAVIT
                   "WHEN EXISTS ( SELECT 1 FROM taa_viv_recepcion C ",
                        "WHERE A.folio =  C.folio AND C.ident_operacion  = '12'  ",
                         "AND   C.cve_ced_cuenta NOT IN ( 517,531 ) ) ",       # Se omiten aquellas cuentas de Presradora JIRA MLM-915                       
                   "THEN '0210' ",#TRA COMPLEMENTARIOS INFONAVIT
                   "ELSE '0000' ",
              "END CASE , ",
              "NVL(SUM(A.monto_en_acciones),0),0 ", #ENTRADAS
              "FROM   dis_cuenta  A ",
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
              "AND    A.subcuenta IN ( 4,8 ) ",
              "AND    A.tipo_movimiento  =   1 ",
              "AND    A.siefore   = 11 ",
              "GROUP BY 1,2 "
   
         OTHERWISE # DE+ AFORES
          
            LET g_txt    =
   
               "SELECT  A.siefore, ",
                    "CASE WHEN C.tipo_traspaso IN (1,3,4) ",
                         "THEN '0209' ",#TRA ORDINARIOS INFONAVIT
                         "WHEN C.tipo_traspaso  = 2 ",
                         "THEN '0210' ",#TRA COMPLEMENTARIOS INFONAVIT
                    "ELSE '0000' ",
                    "END CASE ,0, ",
                    "NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM    dis_cuenta A , taa_cd_ctr_folio C ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                 "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",
                                 "WHERE A.tipo_movimiento   =  B.marca_cod ) ",
                 "AND   A.tipo_movimiento   BETWEEN  200 AND 299 ",
                 "AND   A.folio             = C.folio ",
                 "AND   NOT EXISTS ( SELECT 1 FROM taa_cd_indebidos d WHERE A.nss = ",
                 "                   d.nss  AND d.estado = 103 ",
                 "AND A.fecha_conversion = d.fecha_liquidacion ) ",
                 "AND   A.siefore  = 11  ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT  A.siefore, ",
               "CASE WHEN EXISTS ( SELECT 1 FROM taa_viv_recepcion C ",
                          "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ) ",
                          "AND A.fecha_conversion  = '", g_dia_ant , "' ", 
                    "THEN '0209' ",#TRA ORDINARIOS INFONAVIT
                    "WHEN A.subcuenta = 8 AND A.tipo_movimiento IN ( 1,4 ) ",
                          "AND  A.id_aportante  MATCHES '[CT]I-*' ",
                          "AND  A.fecha_proceso  = '", g_dia_ant , "' ", 
                    "THEN '0209' ",#ENTRADAS VIV TRA-ICE-ICE-AFO-AFO -IMSS
                    "WHEN EXISTS ( SELECT 1 FROM taa_viv_recepcion C ",
                         "WHERE A.folio =  C.folio AND C.ident_operacion  = '12' ) ",
                         "AND A.fecha_conversion  = '", g_dia_ant , "' ", 
                    "THEN '0210' ",#TRA COMPLEMENTARIOS INFONAVIT
                    "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_acciones),0),0 ", #ENTRADAS
               "FROM   dis_cuenta  A ",
               "WHERE    A.subcuenta IN ( 4,8 ) ",
               "AND   A.tipo_movimiento   IN ( 1,4 ) ",
               "AND   A.siefore   = 11 ",
               "GROUP BY 1,2 "
               
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry101 FROM g_txt
   DECLARE c101 CURSOR FOR qry101
   
   FOREACH c101 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
                     
     CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH 
   
   CALL inicializa()
   
      CASE  g_codigo_afore
      
         WHEN 564  # SOLO AFORE METLIFE 
         	  
         	 LET g_txt    =
      
            "SELECT A.siefore, ",
            "CASE  WHEN EXISTS (SELECT 1 FROM acr_cza_devuelto C ",                         #ENTRADAS MTOS EXCEDENTES INFONAVIT antes 0213
                "               WHERE A.folio =  C.folio  AND A.id_aportante = 'DEV. INF.' ",
		             "               AND   A.tipo_movimiento   =  1 ) ",
                "THEN '0211' ",                                  
                "WHEN EXISTS (SELECT 1 FROM  acr_det_dev_ag C ", 
                "             WHERE A.folio =  C.folio AND A.id_aportante = 'DEV. INF.' ",  #ENTRADAS MTOS EXCEDENTES ANUALIDADES GARANTIZADAS antes 0213
                "             AND   A.tipo_movimiento   =  1 ) ",
                "THEN '0211' ",                                  
                "WHEN  A.tipo_movimiento  = '570' AND  A.id_aportante  = 'DINF' ",         #antes 0214
                "THEN '0211' ", 
                "WHEN A.tipo_movimiento   = '1' AND  A.id_aportante = 'DEV-INF' ",
		             "            AND   EXISTS  ( SELECT 1 FROM acr_cza_dev_cred B ",
                "            WHERE A.folio =  B.folio ) ",
                "THEN '0211' ",                                                            #antes 0216              
                "WHEN A.subcuenta = 8  AND A.tipo_movimiento IN ( 1,4 ) ",
                "AND  A.id_aportante      MATCHES '[CT]I-*' ",
                "THEN '0211' ",
                "ELSE '0000' ",
            "END CASE, ",
            "NVL(SUM(A.monto_en_acciones),0),0 ",                                           #ENTRADAS 
            "FROM dis_cuenta A ",         
            "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
	          "AND    A.subcuenta = 8 ",
            "AND    A.tipo_movimiento IN ( 1,4,570) ",
            "AND    A.siefore = 11 ",
            "GROUP BY 1,2 ",
            "UNION ", 
            "SELECT A.siefore, ",
            "CASE WHEN A.subcuenta = 8  AND A.tipo_movimiento = '230' ", 
                      "AND  A.id_aportante       = 'ACR-TRA' ",        #TRANSFERENCIA DE ACREDITADOS                                                              
                      "THEN '0211'", #antes 0212
                  "WHEN A.tipo_movimiento = 234 ",  
                      "     AND  A.id_aportante    = 'ACR-TRA-AG' ",  #ANUALIDADES GARANTIZADAS                           
                      "THEN '0211' ", #antes 0212
                  "WHEN  A.subcuenta = 8 AND  A.tipo_movimiento    = '236'  ",
		                   "      AND  A.id_aportante  = 'USO-CRED' ",
                      "AND   EXISTS  ( SELECT 1 FROM acr_cza_garantia B ",
                      "                WHERE A.folio =  B.folio ) ",
                      "THEN '0211' ", #antes 0215         
                 "ELSE '0000' ",
            "END CASE, ",                                  
            "0,NVL(SUM(A.monto_en_acciones),0) ",                           #SALIDAS 
            "FROM dis_cuenta A ",         
            "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
	          "AND    A.subcuenta = 8 ",
            "AND    A.tipo_movimiento IN ( 230,234,236 ) ",
            "AND    A.siefore = 11 ",
            "GROUP BY 1,2 "
             
         WHEN 568  # SOLO AFORE COPPEL 	
                	
            LET g_txt    =
            
               "SELECT A.siefore, ",
                   "CASE  WHEN EXISTS (SELECT 1 FROM acr_cza_devuelto C ",                         #ENTRADAS MTOS EXCEDENTES INFONAVIT antes 0213
                      "               WHERE A.folio =  C.folio  AND A.id_aportante = 'DEV. INF.' ",
	       	            "               AND   A.tipo_movimiento   =  1 ) ",
                      "THEN '0211' ",                                  
                      "WHEN EXISTS (SELECT 1 FROM  acr_det_dev_ag C ", 
                      "             WHERE A.folio =  C.folio AND A.id_aportante = 'DEV. INF.' ",  #ENTRADAS MTOS EXCEDENTES ANUALIDADES GARANTIZADAS antes 0213
                      "             AND   A.tipo_movimiento   =  1 ) ",
                      "THEN '0211' ",                                                       #SE REPORTA EN EL ID CTA 10 ID SUBCUENTA 05
                      "WHEN A.tipo_movimiento   = '1' AND  A.id_aportante = 'DEV-INF' ",
	       	            "            AND   EXISTS  ( SELECT 1 FROM acr_cza_dev_cred B ",
                      "            WHERE A.folio =  B.folio ) ",
                      "THEN '0211' ",                                                            #antes 0216              
                      "ELSE '0000' ",
               "END CASE, ",
               "NVL(SUM(A.monto_en_acciones),0),0 ",                                             #ENTRADAS 
               "FROM dis_cuenta A ",         
               "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
	             "AND    A.subcuenta = 8 ",
               "AND    A.tipo_movimiento IN ( 1,570) ",
               "AND    A.siefore = 11 ",
               "GROUP BY 1,2 ",
               "UNION ", 
               "SELECT A.siefore, ",
               "CASE WHEN A.subcuenta = 8  AND A.tipo_movimiento = '230' ", 
                         "AND  A.id_aportante       = 'ACR-TRA' ",        #TRANSFERENCIA DE ACREDITADOS                           
                         "THEN '0211'", #antes 0212
                     "WHEN A.tipo_movimiento = 234 ",  
                         "     AND  A.id_aportante    = 'ACR-TRA-AG' ",  #ANUALIDADES GARANTIZADAS                           
                         "THEN '0211' ", #antes 0212
                     "WHEN  A.subcuenta = 8 AND  A.tipo_movimiento    = '236'  ",
	       		            "      AND  A.id_aportante  = 'USO-CRED' ",
                         "AND   EXISTS  ( SELECT 1 FROM acr_cza_garantia B ",
                         "                WHERE A.folio =  B.folio ) ",
                         "THEN '0211' ", #antes 0215         
                    "ELSE '0000' ",
               "END CASE, ",                                  
               "0,NVL(SUM(A.monto_en_acciones),0) ",                           #SALIDAS 
               "FROM dis_cuenta A ",         
               "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
	              "AND    A.subcuenta = 8 ",
               "AND    A.tipo_movimiento IN ( 230,234,236 ) ",
               "AND    A.siefore = 11 ",
               "GROUP BY 1,2 "
             
         OTHERWISE # DE+ AFORES  INVERCAP
      
            LET g_txt    =
            
               "SELECT A.siefore, ",
               "CASE  WHEN EXISTS (SELECT 1 FROM acr_cza_devuelto C ",                         #ENTRADAS MTOS EXCEDENTES INFONAVIT antes 0213
                   "               WHERE A.folio =  C.folio  AND A.id_aportante = 'DEV. INF.' ",
		                "               AND   A.tipo_movimiento   =  1 ) ",
                   "THEN '0211' ",                                  
                   "WHEN EXISTS (SELECT 1 FROM  acr_det_dev_ag C ", 
                   "             WHERE A.folio =  C.folio AND A.id_aportante = 'DEV. INF.' ",  #ENTRADAS MTOS EXCEDENTES ANUALIDADES GARANTIZADAS antes 0213
                   "             AND   A.tipo_movimiento   =  1 ) ",
                   "THEN '0211' ",                                  
                   "WHEN  A.tipo_movimiento  = '570' AND  A.id_aportante  = 'DINF' ",         #antes 0214
                   "THEN '0211' ", 
                   "WHEN A.tipo_movimiento   = '1' AND  A.id_aportante = 'DEV-INF' ",
		                "             AND   EXISTS  ( SELECT 1 FROM acr_cza_dev_cred B ",
                   "            WHERE A.folio =  B.folio ) ",
                   "THEN '0211' ",                                                            #antes 0216              
                   "ELSE '0000' ",
               "END CASE, ",
               "NVL(SUM(A.monto_en_acciones),0),0 ",                                           #ENTRADAS 
               "FROM dis_cuenta A ",         
               "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
	             "AND    A.subcuenta = 8 ",
               "AND    A.tipo_movimiento IN ( 1,570) ",
               "AND    A.siefore = 11 ",
               "GROUP BY 1,2 ",
               "UNION ", 
               "SELECT A.siefore, ",
               "CASE WHEN A.subcuenta = 8  AND A.tipo_movimiento = '230' ", 
                         "AND  A.id_aportante       = 'ACR-TRA' ",        #TRANSFERENCIA DE ACREDITADOS                            
                         "THEN '0211'", #antes 0212
                     "WHEN A.tipo_movimiento = 234 ",  
                         "     AND  A.id_aportante    = 'ACR-TRA-AG' ",  #ANUALIDADES GARANTIZADAS                           
                         "THEN '0211' ", #antes 0212
                     "WHEN  A.subcuenta = 8 AND  A.tipo_movimiento    = '236'  ",
		                      "      AND  A.id_aportante  = 'USO-CRED' ",
                         "AND   EXISTS  ( SELECT 1 FROM acr_cza_garantia B ",
                         "                WHERE A.folio =  B.folio ) ",
                         "THEN '0211' ", #antes 0215         
                    "ELSE '0000' ",
               "END CASE, ",                                  
               "0,NVL(SUM(A.monto_en_acciones),0) ",                           #SALIDAS 
               "FROM dis_cuenta A ",         
               "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
	             "AND    A.subcuenta = 8 ",
               "AND    A.tipo_movimiento IN ( 230,234,236 ) ",
               "AND    A.siefore = 11 ",
               "GROUP BY 1,2 "
       
      END CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry102 FROM g_txt
   DECLARE c102 CURSOR FOR qry102

   FOREACH c102 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #NUEVOS QRYS   NUEVOS NUEVOS   NUEVOS   NUEVOS  NUEVOS  NUEVOS NUEVOS
   #02 19 Traspasos Vivienda 97 Infonavit - Afore

   CALL inicializa()
   
      CASE  g_codigo_afore
      
         WHEN 800  # AFORE FICTICIA
         
         OTHERWISE # DE+ AFORES
         
            LET g_txt    =
         
               "SELECT A.siefore, ",
               "CASE WHEN EXISTS ( SELECT 1 FROM acr_cza_devuelto C ",                           #ENTRADAS MTOS EXCEDENTES INFONAVIT antes 0213
                   "              WHERE A.folio =  C.folio  AND A.id_aportante = 'DEV. INF.' ",
	  	              "              AND A.tipo_movimiento   =  1 ) ",
                   "THEN '0219' ",                                  
                   "WHEN EXISTS (SELECT 1 FROM  acr_det_dev_ag C ", 
                   "             WHERE A.folio =  C.folio AND A.id_aportante = 'DEV. INF.' ",    #ENTRADAS MTOS EXCEDENTES ANUALIDADES GARANTIZADAS antes 0213
                   "             AND A.tipo_movimiento   =  1 ) ",
                   "THEN '0219' ",
                   "WHEN A.tipo_movimiento   = '1' AND  A.id_aportante = 'DEV-INF' ",
	  	              "       AND   EXISTS  ( SELECT 1 FROM acr_cza_dev_cred B ",
                   "                      WHERE A.folio =  B.folio ) ",
                   "THEN '0219' ",                                                                #antes 0216              
                   "ELSE '0000' ",
               "END CASE, ",
               "NVL(SUM(A.monto_en_acciones),0),0 ",                #ENTRADAS 
               "FROM dis_cuenta A ",         
               "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
	             "AND    A.subcuenta = 4 ",
               "AND    A.tipo_movimiento IN (1) ",
               "AND    A.siefore = 11 ",
               "GROUP BY 1,2 ",
               "UNION ", 
               "SELECT A.siefore, ",
               "CASE WHEN A.subcuenta = 4  AND A.tipo_movimiento = '230' ", 
                           "AND  A.id_aportante       = 'ACR-TRA' ",    #TRANSFERENCIA DE ACREDITADOS                                                                 
                           "THEN '0219'",  #Antes 0212
                           "WHEN A.tipo_movimiento = 234 ",  
                           "     AND  A.id_aportante    = 'ACR-TRA-AG' ",#ANUALIDADES GARANTIZADAS                           
                           "THEN '0219' ",  #Antes 0212
                           "WHEN A.subcuenta = 4 AND  A.tipo_movimiento    = '236'  ",
	  		                    "AND  A.id_aportante  = 'USO-CRED' ",                             
                           "THEN '0219' ", #Antes 0215
                           "ELSE '0000' ",
               "END CASE, ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",                #SALIDAS 
               "FROM dis_cuenta A ",         
               "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
	             "AND    A.subcuenta = 4 ",
               "AND    A.tipo_movimiento IN ( 230,234,236 ) ",
               "AND    A.siefore = 11 ",
               "GROUP BY 1,2 "
           
      END CASE
            
   LET g_txt = g_txt CLIPPED
   PREPARE qry6500 FROM g_txt
   DECLARE c6500 CURSOR FOR qry6500
   
   FOREACH c6500 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                      g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
   
   #CORRECCION DE CUENTAS   CORRECCION DE CUENTAS  VIVIENDA VIVIENDA VIVIENDA
   
   #0506   UNIFICACION  INFONAVIT                       VIVIENDA
   #0507   SEPARACION   INFONAVIT                       VIVIENDA
   
   #0508   UNIFICACION  FOVISSTE  VIVIENDA

   CALL inicializa()

      CASE  g_codigo_afore  
        
         WHEN 568  # SOLO AFORE COPPEL
         	
            LET g_txt    =         
             
               "SELECT  A.siefore, ",                                                                                         
                  "CASE WHEN    A.tipo_movimiento = 1 ",                                                                      
                          "AND  A.id_aportante[1,3] MATCHES  'U[CN]-' ",  # UNIFICACION  FOVISSTE  VIVIENDA                     
                          "AND  A.siefore =  12 ",                                                                            
                          "THEN '0508'",                           
                          "ELSE '0000' ",                                                                                     
                  "END CASE , ",                                                                                              
                  "NVL(SUM(A.monto_en_acciones),0),0 ",                  # ENTRADAS                                                          
               "FROM dis_cuenta A ",                                                                                          
               "WHERE A.fecha_conversion      = '", g_dia_ant , "' ",                                                         
               "AND   A.siefore               = 12  ",                                                                        
               "AND   A.tipo_movimiento  IN ( 1,3 ) ",                                                                        
               "AND   A.subcuenta    IN ( 14,35 ) ",                                                                             
               "GROUP BY 1,2 ",                                                                                               
               "UNION ",                                                                                                      
               "SELECT  A.siefore, ",                                                                                         
                  "CASE WHEN    A.tipo_movimiento IN ( 241,242 ) ",                                                           
                          "AND  A.id_aportante[1,3] MATCHES  'U[CN]-' ", #UNIFICACION  FOVISSTE  VIVIENDA                     
                          "AND  A.siefore =  12 ",                                                                            
                          "THEN '0508'",                                                                                      
                          "ELSE '0000' ",                                                                                     
                  "END CASE , ",                                                                                              
                  "0, NVL(SUM(A.monto_en_acciones),0) ",   # SALIDAS                                                          
               "FROM dis_cuenta A ",                                                                                          
               "WHERE A.fecha_conversion      = '", g_dia_ant , "' ",                                                         
               "AND   A.siefore               = 12  ",                                                                        
               "AND   A.tipo_movimiento  IN ( 241,242 ) ",                                                                    
               "GROUP BY 1,2 "                                                                                                      
          
         OTHERWISE # DE+ AFORES
         	
            LET g_txt    =         
          
               "SELECT  A.siefore, ",                                                                                         
                  "CASE WHEN    A.tipo_movimiento = 1 ",                                                                      
                          "AND  A.id_aportante[1,3] MATCHES  'U[CN]-' ",  # UNIFICACION  FOVISSTE  VIVIENDA                     
                          "AND  A.siefore =  12 ",                                                                            
                          "THEN '0508'",                                                                                      
                        "WHEN   A.subcuenta IN ( 14,35 ) AND A.tipo_movimiento IN   ( 1,3 ) ",                                
                          "AND  A.id_aportante    = 'UNI_ISSSTE' ",      # Unificaciones de cuentas ISSSTE(Activas) con Cuentas IMSS
                          "AND  A.siefore         = 12 ",                                                                     
                          "THEN '0508' ",   
                          "WHEN A.tipo_movimiento = 265 AND A.subcuenta IN ( 14,35 ) ", #INV -1930 265 'ABONO UNIFICACION INTRA-AFORE IMSS-ISSSTE'                                                            
                          "THEN '0508' ",   
                          "ELSE '0000' ",                                                                                     
                  "END CASE , ",                                                                                              
                  "NVL(SUM(A.monto_en_acciones),0),0 ",                  # ENTRADAS                                                          
               "FROM dis_cuenta A ",                                                                                          
               "WHERE A.fecha_conversion      = '", g_dia_ant , "' ",                                                         
               "AND   A.siefore               = 12  ",                                                                        
               "AND   A.tipo_movimiento  IN ( 1,3,265 ) ",                                                                        
               "AND   A.subcuenta    IN ( 14,35 ) ",                                                                             
               "GROUP BY 1,2 ", 
               "UNION ",                                                                                                      
               "SELECT  A.siefore, ",                                                                                         
                  "CASE WHEN    A.tipo_movimiento IN ( 241,242 ) ",                                                           
                          "AND  A.id_aportante[1,3] MATCHES  'U[CN]-' ", #UNIFICACION  FOVISSTE  VIVIENDA                     
                          "AND  A.siefore =  12 ",                                                                            
                          "THEN '0508'", 
                          "WHEN A.tipo_movimiento = 264 AND A.subcuenta IN ( 14,35 ) ",#INV-1930 264 'CARGO UNIFICACION INTRA-AFORE IMSS-ISSSTE' 
                          "THEN '0508'",                                                                     
                          "ELSE '0000' ",                                                                                     
                  "END CASE , ",                                                                                              
                  "0, NVL(SUM(A.monto_en_acciones),0) ",   # SALIDAS                                                          
               "FROM dis_cuenta A ",                                                                                          
               "WHERE A.fecha_conversion      = '", g_dia_ant , "' ",                                                         
               "AND   A.siefore               = 12  ",                                                                        
               "AND   A.tipo_movimiento  IN ( 241,242,264 ) ",                                                                    
               "GROUP BY 1,2 " 
          	
      END CASE
 
   LET g_txt = g_txt CLIPPED
   PREPARE qry166 FROM g_txt
   DECLARE c166 CURSOR FOR qry166

   FOREACH c166 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

       CALL Act_tbl ( g_rec.* ) 

   END FOREACH 


   #0509   SEPARACION   FOVISSTE   VIVIENDA
   #REQ  JIRA INV-1925 01marzo2013
   
   # SOLO AFORE INVERCAP	                                                     
   # INV-2897  September 2014 Nuevas Configuraciones Separacion               
   #           Issste.                                                        
   #           No se Detalla las Subcuentas en las Entradas de Separacion     
   #           Issste porque puede quedar Abierto las Subcuentas que liquiden.
   #           el Id Aportante Varia por tanto no
   
   CALL inicializa()  
   
      CASE  g_codigo_afore
         #MOD INV-2450 Ajustes para separar Importe IMSS de ISSSTE
         WHEN 562  # SOLO AFORE INVERCAP
         	  
         	  LET g_txt    = 
         	                  
         	     "SELECT A.siefore, ",   
               "CASE ",                                                                                                                                                                       
                       "WHEN A.tipo_movimiento = 88 AND A.fecha_proceso  = '", g_dia_ant , "' THEN '0509' ",  #ENTRADA 0509 SEPARACION FOVISSSTE                                           
                       "ELSE '0000' ",                                                                                                                                                        
               "END CASE , ",                                                                                                                                                                 
               "NVL(SUM(A.monto_en_acciones),0) ,0 ",#SALIDAS 0509                                                                                                                           
               "FROM      dis_cuenta  A ",  
               "WHERE     A.fecha_proceso   = '", g_dia_ant , "' ",                  
               "AND       A.tipo_movimiento = 88 ",                                                                                                                                    
               "AND       A.siefore         = 12 ",                                                                                                                          
               "GROUP BY 1,2 ",                                                                                                                                             
               "UNION ",# SALIDA INV-2897  September 2014 Nuevas Configuraciones Separacion
               "SELECT A.siefore, ",                                                                                                                                                          
               "CASE ",
                       "WHEN A.tipo_movimiento = 288 AND A.fecha_proceso  = '", g_dia_ant , "' THEN '0509' ",  #SALIDA 0509 SEPARACION FOVISSSTE                                            
                       "ELSE '0000' ",                                                                                                                                                        
               "END CASE , ",                                                                                                                                                                 
               "0, NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS 0509                                                                                                                               
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_proceso   = '", g_dia_ant , "' ",                  
               "AND       A.tipo_movimiento = 288 ",                                                                                                                                    
               "AND       A.siefore         = 12 ",                                                                                                                       
               "GROUP BY 1,2 " 
         
         OTHERWISE # DE+ AFORES             
                                   
            LET g_txt    =  
                
               "SELECT    A.siefore, ",
               "'0509', ",
               "NVL(SUM(A.monto_en_acciones),0),0 ",
               "FROM     dis_cuenta  A  ",#ENTRADAS
               "WHERE    A.fecha_conversion  = '", g_dia_ant , "' ",        
               "AND      A.subcuenta          IN (  14,35 ) ",
               "AND      A.tipo_movimiento    =  88  ",
               "AND      A.id_aportante       = 'SEP-ISS' ",
               "AND      A.siefore            =  12  ",
               "GROUP BY 1,2 "
                
      END CASE
      
   LET g_txt = g_txt CLIPPED                                    
   PREPARE qry310 FROM g_txt                                    
   DECLARE c310 CURSOR FOR qry310                               
                                                                
   FOREACH c310 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,       
                     g_rec.sal                                  
                                                   
       CALL Act_tbl ( g_rec.* )                                 
                                                                
   END FOREACH                                                  
           
   
   CALL inicializa()
   
      CASE  g_codigo_afore
      	  
      	 WHEN 568  # SOLO AFORE COPPEL 	
      	 	
      	 	  LET g_txt    =
           
               "SELECT  A.siefore,'0506', ",
                       "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_acciones ,0)),0), ",
                       "NVL(SUM ( CASE ",
                                        "WHEN A.tipo_movimiento <> 1 THEN A.monto_en_acciones ",
                                 "ELSE 0 ",
                                 "END ),0) ",
               "FROM   dis_cuenta  A ",
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.id_aportante[1,3] MATCHES  'U[CN]-'    ",
               "AND     A.tipo_movimiento   IN ( 1,241,242 )     ",
               "AND     A.siefore           =   11               ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT  A.siefore,'0507', ",
                      "NVL(SUM ( CASE ",
                                       "WHEN A.monto_en_acciones > 0 THEN A.monto_en_acciones ",
                                 "ELSE 0 ",
                                 "END ),0), ",
                      "NVL(SUM ( CASE ",
                                       "WHEN A.monto_en_acciones < 0 THEN A.monto_en_acciones ",
                               "ELSE 0 ",
                               "END ),0) ",
               "FROM     dis_cuenta  A ,sep_det_reg_sol_reclamante B ",
               "WHERE  A.fecha_proceso           = '", g_dia_ant , "' ",
               "AND    B.estado                  = 8              ",
               "AND    A.folio                   = B.folio        ",
               "AND    ( A.nss = B.n_seguro  OR  A.nss  = B.nss ) ",
               "AND    A.siefore           = 11                   ",
               "GROUP BY 1,2 "   
      	 	
      	 OTHERWISE # DE+ AFORES
           
           LET g_txt    =
           
              "SELECT  A.siefore,'0506', ",
                      "NVL(SUM (DECODE (A.tipo_movimiento,1 ,A.monto_en_acciones ,0)),0), ",
                      "NVL(SUM ( CASE ",
                                       "WHEN A.tipo_movimiento <> 1 THEN A.monto_en_acciones ",
                                "ELSE 0 ",
                                "END ),0) ",
              "FROM   dis_cuenta  A ",
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
              "AND     A.id_aportante[1,3] MATCHES  'U[CN]-'    ",
              "AND     A.tipo_movimiento   IN ( 1,241,242 )     ",
              "AND     A.siefore           =   11               ",
              "GROUP BY 1,2 ",
              "UNION ",
              "SELECT  A.siefore,'0507', ",
                     "NVL(SUM ( CASE ",
                                      "WHEN A.monto_en_acciones > 0 THEN A.monto_en_acciones ",
                                "ELSE 0 ",
                                "END ),0), ",
                     "NVL(SUM ( CASE ",
                                      "WHEN A.monto_en_acciones < 0 THEN A.monto_en_acciones ",
                              "ELSE 0 ",
                              "END ),0) ",
              "FROM     dis_cuenta  A ,sep_det_reg_sol_reclamante B ",
              "WHERE  A.fecha_proceso           = '", g_dia_ant , "' ",
              "AND    B.estado                  = 9              ",
              "AND    A.folio                   = B.folio        ",
              "AND    ( A.nss = B.n_seguro  OR  A.nss  = B.nss ) ",
              "AND    A.siefore           = 11                   ",
              "GROUP BY 1,2 "   
                      
      END CASE 
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry110 FROM g_txt
   DECLARE c110 CURSOR FOR qry110
   
   FOREACH c110 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
   
       CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH 
   
   # RETIROS IMSS   RETIROS IMSS  RETIROS IMSS  VIVIENDA VIVIENDA VIVIENDA   
        
   # SE QUITA  CAMBIOS OCTUBRE DEL 2011  08 09 DISPOCISION   DE RECURSOS INFONAVIT       
   # 08 10 TRANSFERENCIA DE RECURSOS INFONAVIT AL GOBIERNO FEDERAL
        
   #-- T.M 841 (PAGO PENSION RETIROS PROGRAMADOS ) SUBCTA 4 SIEFORE  11
    
   CALL inicializa()
        
      #NUEVO  OCTUBRE 2011 AL ID CTA 08 IDSUBCTA 10 SE AGREGA EL TIPO DE RETIRO 'A´ T.M = 800
        
      CASE g_codigo_afore
        
         WHEN 564  # SOLO AFORE METLIFE
        
            LET g_txt    =
        
               "SELECT A.siefore, ",                      
                            "CASE WHEN A.tipo_movimiento IN ( 810,815 ) ",   
                            "THEN '0810' ",
                            "ELSE '0000' ",
                       "END CASE , ",
                       "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                
               "AND       A.tipo_movimiento IN ( 810,815 ) ",                                           
               "AND       A.subcuenta       IN ( 4,8 ) ",
               "AND       A.siefore         =  11  ",
               "GROUP BY 1,2 "
              
         WHEN 568  # SOLO AFORE COPPEL  
   
            # SE QUITA  CAMBIOS OCTUBRE DEL 2011  08 09 DISPOCISION   DE RECURSOS INFONAVIT
            #NUEVO  OCTUBRE 2011 AL ID CTA 08 IDSUBCTA 10 SE AGREGA EL TIPO DE RETIRO 'A´ T.M = 800
   
            LET g_txt    =
   
               "SELECT A.siefore, ",                                                           
                     "CASE WHEN A.tipo_movimiento IN ( 810,815 ) ",
                            "THEN '0810' ",                                                
                            "WHEN A.tipo_movimiento = 800 ",
                            "     AND EXISTS ( SELECT 1 FROM ret_transf_rx C ",
                            "                  WHERE A.folio =  C.folio ",
                            "                  AND   A.nss   =  C.nss ",                        
                            "                  AND   C.cve_destino <> 'A' ) ",
                            "THEN '0810' ",                        
                            "ELSE '0000' ",
                       "END CASE , ",
                       "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",                         
               "AND       A.tipo_movimiento IN ( 800,810,815 ) ",
               "AND       A.subcuenta       IN ( 4,8 ) ",
               "AND       A.siefore         =  11  ",
               "GROUP BY 1,2 "
               
         WHEN 562  # SOLO AFORE INVERCAP 
         	
         	  #SE QUITA  CAMBIOS OCTUBRE DEL 2011  08 09 DISPOCISION   DE RECURSOS INFONAVIT
            #NUEVO  OCTUBRE 2011 AL ID CTA 08 IDSUBCTA 10 SE AGREGA EL TIPO DE RETIRO 'A´ T.M = 800
            
            #"CASE WHEN A.tipo_movimiento IN ( 825,820,830,840,841,850,860,880,10 ) AND A.subcuenta NOT IN (3,10,23) ",
            #"THEN '0809' ",
   
            LET g_txt    =
                
               "SELECT  A.siefore,'0810',NVL(SUM(A.monto_en_acciones),0),0 ",
               "FROM    dis_cuenta A ",#ENTRADA IMSS  Devolución contingente Aseguradoras
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND     A.id_aportante      = 'IMSS' ",
               "AND     A.subcuenta IN ( 4,8 ) ",    
               "AND     A.siefore  = 11 ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, ",                         
                   "CASE WHEN A.tipo_movimiento IN ( 810,815 ) ",                          
                         "THEN '0810' ", 
                         "ELSE '0000' ",
                    "END CASE , ",
                    "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM      dis_cuenta A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",          
               "AND       A.tipo_movimiento IN ( 810,815 ) ",                                           
               "AND       A.subcuenta       IN ( 4,8 ) ",
               "AND       A.siefore         =  11  ",
               "GROUP BY 1,2 "
               
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry128 FROM g_txt
   DECLARE c128 CURSOR FOR qry128
   
   FOREACH c128 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                g_rec.sal
   
      CALL Act_tbl ( g_rec.* ) 
   
   
   END FOREACH 
   

   #QRYS NUEVOS      NUEVOS      NUEVOS       NUEVOS       NUEVOS     NUEVOS
   #08 12 Dispocicion de Recursos Infonavit Vivienda 92
   
   CALL inicializa()
   
      INITIALIZE  g_txt  TO NULL
      
      CASE  g_codigo_afore
   
         WHEN 800  # AFORE FICTICIA
       		
         OTHERWISE #   DE+ AFORES
                   #-- Modificacion  JIRA INV-1254
                   #-- DEBIDO A LA MODIFICACION A TRANSFERENCIAS Y DISPOSICIONES IMMS (REGIMEN 73)
                   #-- SE AÑADIO T.M  = 817  RETIRO DISPOSICION VIVIENDA 97 RET-E REG 73 
                   #-- CPL-1251 SE AÑADIO T.M  = 818  CANCELACION DE INTERESES DE VIVIENDA
                   
            LET g_txt    =
	      
               "SELECT A.siefore, ",                   
		            "CASE WHEN A.tipo_movimiento IN ( 817,818,825,820,830,840,850,860,880,10  ) AND A.subcuenta = 8 ", #SE REPORTARA PURO RCV
                            "THEN '0812' ",                       
                            "ELSE '0000' ",
                       "END CASE , ",
                        "0,NVL(SUM(A.monto_en_acciones),0) ",
               "FROM      dis_cuenta  A ",
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
               "AND       A.tipo_movimiento IN ( 817,818,825,820,830,840,850,860,880,10  ) ", 
       	       #"AND       A.subcuenta =    8    ",				              
               "AND       A.siefore   =   11    ",
               "GROUP BY 1,2 "
             
      END CASE
      
   LET g_txt = g_txt CLIPPED
   PREPARE qry753 FROM g_txt
   DECLARE c753 CURSOR FOR qry753
   
   FOREACH c753 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
      
   #-----------------------------

   #QRYS NUEVOS      NUEVOS      NUEVOS       NUEVOS       NUEVOS     NUEVOS
   #08 13 Dispocicion de Recursos Infonavit Vivienda 97

   CALL inicializa()
   
      INITIALIZE  g_txt  TO NULL
   
      CASE  g_codigo_afore
   
         WHEN 800  # AFORE FICTICIA
       		
         OTHERWISE # DE+ AFORES
            
            #-- Modificacion  JIRA INV-1254                                       
            #-- DEBIDO A LA MODIFICACION A TRANSFERENCIAS Y DISPOSICIONES IMMS (RE
            #-- SE AÑADIO T.M  = 817  RETIRO DISPOSICION VIVIENDA 97 RET-E REG 73 
            #-- CPL-1251 SE AÑADIO T.M  = 818  CANCELACION DE INTERESES DE VIVIENDA
   
            LET g_txt    =
	      
               "SELECT A.siefore, ",                   
		            "CASE WHEN A.tipo_movimiento IN ( 817,818,825,820,830,840,850,860,880,10  ) AND A.subcuenta = 4 ", #SE REPORTARA PURO RCV
                            "THEN '0813' ",                       
                            "ELSE '0000' ",
                       "END CASE , ",
                       "0,NVL(SUM(A.monto_en_acciones),0) ",
               "FROM      dis_cuenta  A ",
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
               "AND       A.tipo_movimiento IN ( 817,818,825,820,830,840,850,860,880,10  ) ", 
	             #"AND       A.subcuenta =    4    ",				              
               "AND       A.siefore   =   11    ",
               "GROUP BY 1,2 "
             
      END CASE
   
      
   LET g_txt = g_txt CLIPPED
   PREPARE qry754 FROM g_txt
   DECLARE c754 CURSOR FOR qry754
   
   FOREACH c754 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
      
   #-----------------------------
 
   #QRYS NUEVOS      NUEVOS      NUEVOS       NUEVOS       NUEVOS     NUEVOS
   #0814  Retiros de recursos Infonavit por retiros Programados

   CALL inicializa()
   
      CASE  g_codigo_afore
   
         WHEN 800  # AFORE FICTICIA
       		
         OTHERWISE # DE+ AFORES
   
            LET g_txt    =
   
               "SELECT  A.siefore, ",
               "'0814', ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS RETIROS PROGRAMADOS
               "FROM   dis_cuenta  A  ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento     = 841 ",
               "AND    A.subcuenta = 4 ",
               "AND    A.siefore   = 11",             
               "GROUP  BY 1,2 "
    
      END CASE
   
      
   LET g_txt = g_txt CLIPPED
   PREPARE qry756 FROM g_txt
   DECLARE c756 CURSOR FOR qry756
   
   FOREACH c756 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
      
   #-----------------------------
 
   #QRYS NUEVOS      NUEVOS      NUEVOS       NUEVOS       NUEVOS     NUEVOS
   #0815  Retiros de recursos Infonavit por Transferencia de Aseguraforas


   CALL inicializa()
   
      CASE  g_codigo_afore
   
         WHEN 800  #AFORE FICTICIA
       		 
     		 WHEN 568  #SOLO AFORE COPPEL	
                   #JIRA  CPL-965
                   
            LET g_txt    =            
            
            
               "SELECT  A.siefore,'0815',NVL(SUM(A.monto_en_acciones),0),0 ",               
               "FROM    dis_cuenta A ",#ENTRADA IMSS  Devolución contingente Aseguradoras
               "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                      
               "AND     A.id_aportante   MATCHES 'IMSS*' ",                              
               "AND     A.tipo_movimiento  = 640 ",                                      
               "AND     A.siefore          = 11  ",                                 
               "GROUP BY 1,2 ",                                                          
               "UNION ",
               "SELECT  A.siefore, ",
               "'0815', ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS TRANSFERENCIA A 'ASEGURADORAS
               "FROM   dis_cuenta  A  ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento = 800 ",
               "AND    EXISTS ( SELECT 1 FROM ret_transf_rx C ",
               "       WHERE  A.folio =  C.folio ",
               "       AND    A.nss   =  C.nss  ",
               "       AND    C.cve_destino = 'A') ",
               "AND    A.subcuenta = 4 ",
               "AND    A.siefore   = 11",               
               "GROUP  BY 1,2 ",               
               "UNION ",               
               "SELECT  A.siefore, ",
               "'0815', ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS TRANSFERENCIA A 'ASEGURADORAS CONTINGENTE
               "FROM   dis_cuenta  A  ",
               "WHERE  A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento = 800 ",
               "AND    A.subcuenta = 4 ",
               "AND    A.siefore   = 11",               
               "GROUP  BY 1,2 "
               
       	 WHEN 562  # SOLO AFORE INVERCAP
       	 	
       	   LET g_txt    =
       	   
       	      "SELECT  A.siefore,'0815',NVL(SUM(A.monto_en_acciones),0),0 ",
              "FROM    dis_cuenta A ",#ENTRADA IMSS  Devolución contingente Aseguradoras
              "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
              "AND     A.id_aportante      = 'ASEGURADORA' ",
              "AND     A.subcuenta IN ( 4,8 ) ",    
              "AND     A.siefore  = 11 ",
              "GROUP BY 1,2 ",
              "UNION ",
              "SELECT  A.siefore, ",
              "'0815', ",
              "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS TRANSFERENCIA A 'ASEGURADORAS
              "FROM    dis_cuenta  A  ",
              "WHERE   A.fecha_conversion    = '", g_dia_ant , "' ",
              "AND     A.tipo_movimiento     = 800 ",                 
              "AND    A.subcuenta = 4 ",
              "AND    A.siefore   = 11",   
              "GROUP  BY 1,2 "
               
       	 OTHERWISE # DE+ AFORES  METLIFE
       	 	  
     	 	    LET g_txt    =
   
               "SELECT  A.siefore, ",
               "'0815', ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS TRANSFERENCIA A 'ASEGURADORAS
               "FROM    dis_cuenta  A  ",
               "WHERE   A.fecha_conversion    = '", g_dia_ant , "' ",
               "AND     A.tipo_movimiento     = 800 ",                                  
               "AND    A.subcuenta = 4 ",
               "AND    A.siefore   = 11",   
               "GROUP  BY 1,2 "
	        
      END CASE
      
   LET g_txt = g_txt CLIPPED
   PREPARE qry757 FROM g_txt
   DECLARE c757 CURSOR FOR qry757
   
   FOREACH c757 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
   
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
	
   # DEVOLUCION DE PAGOS SIN JUSTIF LEGAL VIVIENDA VIVIENDA VIVIENDA VIVIENDA VIVIE
   
   
   # 09 04 CUOTAS EN EXCESO INFONAVIT
   # 09 05 CUOTAS EN EXCESO FOVISSSTE
   
   CALL inicializa()
   
      LET g_txt    =
   
         "SELECT  A.siefore,'0904',0,NVL(SUM(A.monto_en_acciones),0) ",
         "FROM dis_cuenta A ",#SALIDAS
         "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND   A.subcuenta IN ( 4,8 ) ",
         "AND   A.tipo_movimiento IN ( 540,545,550,555 ) ",
         "AND   A.siefore  = 11  ",
         "GROUP BY 1,2 "
   
      LET g_txt = g_txt CLIPPED
      PREPARE qry22 FROM g_txt
      DECLARE c22 CURSOR FOR qry22
   
      FOREACH c22 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                       g_rec.sal
   
         #LET  g_rec.sie        =        g_viv
   
         CALL Act_tbl ( g_rec.* )
   
      END FOREACH
   
   CALL inicializa()
   
      LET g_txt    =
      
         "SELECT  A.siefore,'0905',0,NVL(SUM(A.monto_en_acciones),0) ",
         "FROM dis_cuenta A ",#SALIDAS
         "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
         "AND   A.subcuenta IN ( 14,35 ) ",
         "AND   A.tipo_movimiento IN ( 543,544,553,554 ) ",
         "AND   A.siefore  = 12  ",
         "GROUP BY 1,2 "
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry80 FROM g_txt
   DECLARE c80 CURSOR FOR qry80
   
   FOREACH c80 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal     
      
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
   
   # RECAUDACION ISSSTE   RECAUDACION ISSSTE VIVIENDA VIVIENDA VIVIENDA  VIVIENDA
   
   # 11 11 CUOTA PATRONAL FOVISSSTE 

   CALL inicializa()
   
      CASE  g_codigo_afore
         
         WHEN 568  #SOLO AFORE COPPEL 
         	 
            LET g_txt    =

               "SELECT  A.siefore, ",
                  "CASE WHEN A.subcuenta IN ( 14,35 )  AND A.tipo_movimiento IN ( 1,4 ) ",
                       "THEN '1111' ",
                       "ELSE '0000' ",
                       "END CASE , ",
                       "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS
               "FROM dis_cuenta A , dis_det_issste B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   B.ident_tipo_aport  = '01' ",
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
               "AND   B.ident_viv_garantia = 0	",
               "AND   A.siefore  =  12  ",
               "GROUP BY 1,2 "
               
         OTHERWISE # DE+ AFORES METLIFE INVERCAP   
         
            #REQUERIMIENTO JIRA MLM-1048 08MAY2012 se comento la siguiente linea 
         	  # AND   B.ident_viv_garantia = 0	  ya que quieren que se reporte
         	  # tanto B.ident_viv_garantia = 0 como B.ident_viv_garantia = 1
         	  
         	  LET g_txt    =
            
                "SELECT  A.siefore, ",
                   "CASE WHEN A.subcuenta IN ( 14,35 )  AND A.tipo_movimiento IN ( 1,4 ) ",
                        "THEN '1111' ",
                        "ELSE '0000' ",
                        "END CASE , ",
                        "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS
                "FROM dis_cuenta A , dis_det_issste B ",
                "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   A.folio             = B.folio ",
                "AND   B.ident_tipo_aport  = '01' ",
                "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
                #"AND   B.ident_viv_garantia = 0	",
                "AND   A.siefore  =  12  ",
                "GROUP BY 1,2 "
         
      END  CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry58 FROM g_txt
   DECLARE c58 CURSOR FOR qry58

   FOREACH c58  INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #--          
   # 11 12  ACLARACIONES CUOTAS PATRONAL FOVISSSTE
   
   CALL inicializa()
   
      CASE  g_codigo_afore
       
         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES 
          
            LET g_txt    =

               "SELECT  A.siefore, ",
                   "CASE WHEN A.subcuenta IN ( 14,35 )  AND A.tipo_movimiento IN ( 1,4 ) ",
                        "THEN '1112' ",
                       "ELSE '0000' ",
                       "END CASE , ",
                       "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS
               "FROM dis_cuenta A , dis_det_issste B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   B.ident_tipo_aport  = '02' ",
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
               "AND   A.siefore  =  12 ",
               "GROUP BY 1,2 "

      END  CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry104 FROM g_txt
   DECLARE c104 CURSOR FOR qry104

   FOREACH c104 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
 
   #11 13 Asignacion Fovissste NO EXISTE AUN     


   #RETIROS ISSSTE   RETIROS ISSSTE  RETIROS ISSSTE  VIVIENDA VIVIENDA  VIVIENDA
   
   #SE  QUITA CAMBIOS DE OCTUBRE DEL 2011 12 06 Dispocision de recursos Fovissste
   #12 07 Transferencia de recursos Fovissste al Gobierno Federal


   CALL inicializa()
      
      # SE  QUITA CAMBIOS DE OCTUBRE DEL 2011 12 06 Dispocision de recursos Fovissste
      CASE  g_codigo_afore
      	
         WHEN 564  # SOLO AFORE METLIFE
	                 # SE  QUITA CAMBIOS DE OCTUBRE DEL 2011 12 06 Dispocision de recursos Fovissste
      
            LET g_txt    =
      
               "SELECT A.siefore, ",                    
                            "CASE WHEN A.tipo_movimiento IN ( 861,862 )  AND A.subcuenta IN ( 14,35 ) ",
                            "THEN '1207' ",
                            "ELSE '0000' ",
                       "END CASE , ",
                       "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDA
               "FROM      dis_cuenta  A ",
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.tipo_movimiento IN ( 851,852,853,854,855,858,861,862 ) ",
               "AND       A.siefore            =  12  ",
               "GROUP BY 1,2 "
               
         OTHERWISE # DE+ AFORES        
                   # SE  QUITA CAMBIOS DE OCTUBRE DEL 2011 12 06 Dispocision de recursos Fovissste
      
             LET g_txt    =
      
                "SELECT A.siefore, ",
                             "CASE WHEN A.tipo_movimiento = 861  AND A.subcuenta IN ( 14,35 ) ",
                             "THEN '1207' ",
                             "ELSE '0000' ",
                        "END CASE , ",
                          "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDA
                "FROM      dis_cuenta  A ",
                "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
                "AND       A.tipo_movimiento IN ( 851,852,853,854,855,858,861 ) ",
                "AND       A.siefore            =  12  ",
                "GROUP BY 1,2 "
             
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry87  FROM g_txt
   DECLARE c87    CURSOR FOR qry87
   
   FOREACH c87 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
   
      CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH 
   
   #QUERYS  NUEVOS  NUEVOS NUEVOS NUEVOS NUEVOS NUEVOS NUEVOS
   #12 08 Dispocision de Recursos Fovissste 92
 
   CALL inicializa()
            
      CASE  g_codigo_afore
     
         WHEN 800  # AFORE FICTICIA
   
         OTHERWISE # DE+ AFORES
       
            LET g_txt    =
            
               "SELECT A.siefore, ",
                     "CASE WHEN  A.tipo_movimiento IN ( 851,852,853,854,855,858,864 ) AND A.subcuenta = 14 ",
                            "THEN '1208' ",
                            "ELSE '0000' ",
                        "END CASE , ",
                        "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDA
               "FROM      dis_cuenta  A ",
               "WHERE     A. fecha_conversion  = '", g_dia_ant , "' ",                
               "AND       A.tipo_movimiento IN ( 851,852,853,854,855,858,864 ) ",
               "AND       A.siefore            =  12  ",
               "GROUP BY 1,2 "
   
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry755  FROM g_txt
   DECLARE c755    CURSOR FOR qry755
   
   FOREACH c755 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                g_rec.sal
   
      CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH 

   #QUERYS  NUEVOS  NUEVOS NUEVOS NUEVOS NUEVOS NUEVOS NUEVOS
   #12 09 Dispocision de Recursos Fovissste 08
 
    CALL inicializa()	
      
      CASE  g_codigo_afore
      	
         WHEN 564  # SOLO AFORE METLIFE
         	
            LET g_txt    =
            
               "SELECT A.siefore, ",
               "CASE WHEN  A.tipo_movimiento IN ( 851,852,853,854,855,858,864 ) AND A.subcuenta = 35 ",#MLM-2654  SE OMITE EL TIPO DE  MOVIMIENTO  862 YA QUE 
               "     THEN '1209' ",
               "     ELSE '0000' ",                                                                         #SE ESTA INCLUYENDO CORRECTAMENTE EN EL ID CUENTA 12 ID SUBCUENTA 07
               "END CASE , ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDA
               "FROM  dis_cuenta  A ",
               "WHERE A. fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.tipo_movimiento IN ( 851,852,853,854,855,858,864 ) ",
               "AND   A.siefore            =  12  ",
               "GROUP BY 1,2 "
         	
         	
         OTHERWISE # DE+ AFORES
                   #INV -1771    tipo_retiro      I
                   #             movimiento       862
                   #             descripcion      TRANSF. ASEGURADORA POR PAGO DE PENSION
                   
             LET g_txt    =
         
                "SELECT A.siefore, ",
                       "CASE WHEN  A.tipo_movimiento IN ( 851,852,853,854,855,858,862,864 ) AND A.subcuenta = 35 ",
                       "    THEN '1209' ",
                       "    ELSE '0000' ",
                      "END CASE , ",
                      "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDA
                "FROM  dis_cuenta  A ",
                "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
                "AND   A.tipo_movimiento IN ( 851,852,853,854,855,858,862,864 ) ",
                "AND   A.siefore            =  12  ",
                "GROUP BY 1,2 "
                
      END CASE
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry7561  FROM g_txt
   DECLARE c7561    CURSOR FOR qry7561
   
   FOREACH c7561 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                      g_rec.sal
   
      CALL Act_tbl ( g_rec.* ) 
   
   END FOREACH 

   # TRASPASOS ISSSTE TRASPASOS ISSSTE TRASPASOS ISSSTE  VIVIENDA VIVIENDA  VIVIEND
   
   #1308 TRASPASOS ORDINARIOS FOVISSSTE
   #1309 TRASPASOS COMPLEMENTARIOS FOVISSSTE
   #Se reporta todo tipo de Traspasos Complementarios
   #de Trabajadores con Saldo en la Subcuenta de 
   #Vivienda FOVISSSTE.
    
   CALL inicializa()

      CASE g_codigo_afore
      	 
      	 WHEN 568  # SOLO AFORE COPPEL
           	
           	LET g_txt    =                                                                       
           	                                                                                  
           	   "SELECT  A.siefore, ",                                                  
           	        "CASE WHEN C.tipo_traspaso IN (1,3,4) ",                           
           	             "THEN '1308' ",#TRA ORDINARIOS INFONAVIT                      
           	             "WHEN C.tipo_traspaso  = 2 ",                                 
           	             "THEN '1309' ",#TRA COMPLEMENTARIOS INFONAVIT                 
           	        "ELSE '0000' ",                                                    
           	        "END CASE ,0, ",                                                   
           	       "NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS                        
           	   "FROM    dis_cuenta A , taa_cd_ctr_folio C ",                          
           	   "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                   
           	   "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",              
           	                   "WHERE A.tipo_movimiento   =  B.marca_cod ) ",        
           	   "AND   A.tipo_movimiento   BETWEEN  200 AND 299 ",                    
           	   "AND   A.folio             = C.folio ",                               
           	   "AND   A.siefore  =  12  ",                                           
           	   "GROUP BY 1,2 ",                                                         
           	   "UNION ",                                                                
           	   "SELECT  A.siefore, ",                                                   
           	           "CASE WHEN EXISTS (SELECT 1 FROM taa_viv_recepcion C ",          
           	                "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ) ",
           	                "THEN '1308' ",#TRA ORDINARIOS INFONAVIT                    
           	                "WHEN EXISTS (SELECT 1 FROM taa_viv_recepcion C ",          
           	                "WHERE A.folio =  C.folio AND C.ident_operacion  = '12' ) ",
           	                "THEN '1309' ",#TRA COMPLEMENTARIOS INFONAVIT  
           	                "WHEN  A.id_aportante = 'UNI_ISSSTE' ",
           	                "THEN '1309'", #Ingreso CPL-987        
           	           "ELSE '0000' ",                                                  
           	           "END CASE , ",                                                   
           	             "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS                 
           	   "FROM    dis_cuenta  A ",                                                 
           	   "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",                     
           	   "AND     A.tipo_movimiento   =  1 ",                                      
           	   "AND     A.siefore  = 12 ",                                               
           	   "GROUP BY 1,2 "  
         
           OTHERWISE # DE+ AFORES
      
                     # DE ACUERDO AL NUMERO DE REQUERIMIENTO CPL-491 Y DE ACUERDO A LO QUE ME COMENTA
                     # PACO EL ESPECIALISTA DE CEDENTE SE TIENE QUE AGREGAR AL ID CTA 13 ID SUBCUENTA 08
                     # El tipo de Traspaso = 4.( ya que me comenta que entrarón Nvos Tipos 31-mar-2011.
      
              LET g_txt    =
              
                 "SELECT  A.siefore, ",
                      "CASE WHEN C.tipo_traspaso IN (1,3,4) ",
                           "THEN '1308' ",#TRA ORDINARIOS INFONAVIT
                           "WHEN C.tipo_traspaso  = 2 ",
                           "THEN '1309' ",#TRA COMPLEMENTARIOS INFONAVIT
                      "ELSE '0000' ",
                      "END CASE ,0, ",
                      "NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
                  "FROM    dis_cuenta A , taa_cd_ctr_folio C ",
                  "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                   "AND   EXISTS  ( SELECT 1 FROM taa_cd_tipo_traspaso B ",
                                   "WHERE A.tipo_movimiento   =  B.marca_cod ) ",
                   "AND   A.tipo_movimiento   BETWEEN  200 AND 299 ",
                   "AND   A.folio             = C.folio ",
                   "AND   A.siefore  =  12  ",
                 "GROUP BY 1,2 ",
                 "UNION ",
                 "SELECT  A.siefore, ",
                         "CASE WHEN EXISTS (SELECT 1 FROM taa_viv_recepcion C ",
                              "WHERE A.folio =  C.folio AND C.ident_operacion  = '09' ) ",
                              "THEN '1308' ",#TRA ORDINARIOS INFONAVIT
                              "WHEN EXISTS (SELECT 1 FROM taa_viv_recepcion C ",
                              "WHERE A.folio =  C.folio AND C.ident_operacion  = '12' ) ",
                              "THEN '1309' ",#TRA COMPLEMENTARIOS INFONAVIT
                         "ELSE '0000' ",
                         "END CASE , ",
                           "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS
                 "FROM   dis_cuenta  A ",
                 "WHERE   A.fecha_conversion  = '", g_dia_ant , "' ",
                  "AND   A.tipo_movimiento   =  1 ",
                  "AND   A.siefore  = 12 ",
                 "GROUP BY 1,2 " 
                 
      END CASE

   LET g_txt = g_txt CLIPPED
   PREPARE qry111 FROM g_txt
   DECLARE c111 CURSOR FOR qry111

   FOREACH c111 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

     CALL Act_tbl ( g_rec.* ) 

   END FOREACH 
   

   #13  10  TRASPASOS FOVISSSTE 92

   CALL inicializa() 
   
      CASE  g_codigo_afore
      
         WHEN 562  # SOLO AFORE INVERCAP
         	
            LET g_txt    =
            
               "SELECT    A.siefore, ",
               "'1310', ",
               "NVL(SUM(A.monto_en_acciones),0) ,0 ",#ENT TRA-ICE-AFO-PENSIONISSTE 
               "FROM     dis_cuenta  A ",
               "WHERE     A.fecha_proceso      = '", g_dia_ant , "' ",
               "AND       A.subcuenta          =    14        ",
               "AND       A.tipo_movimiento    IN   ( 1,4 ) ",
               "AND       A.id_aportante      MATCHES '[CT]I-*' ",
               "AND       A.siefore            =    12    ",
               "GROUP BY 1,2 "
               
         OTHERWISE # DE+ AFORES 
         
            #CPL-1466 Se Añade a la primera consulta del Id Cuenta 13 Id Subcuenta 02
            #         El verificar que solo reporte lo liquidado del Proceso de Unificacion
            #         Sar - ISSSTE para eso verifica que el folio se encuentre en tra_det_trasp_sal_issste
            
            LET g_txt    =                                                         
                                                                                   
               "SELECT    A.siefore, ",                                            
               "'1310', ",                                                         
               "NVL(SUM(A.monto_en_acciones),0) ,0 ",#ENT TRA-ICE-AFO-PENSIONISSTE 
               "FROM     dis_cuenta  A ",                                          
               "WHERE     A.fecha_conversion   = '", g_dia_ant , "' ",             
               "AND       A.subcuenta          =    14        ",                   
               "AND       A.tipo_movimiento    IN   ( 1,4 ) ",                     
               "AND       A.id_aportante      MATCHES '[CT]I-*' ", 
               "AND  EXISTS ( SELECT 1 FROM safre_af:tra_det_trasp_sal_issste C ",  
               "              WHERE A.folio =  C.folio )",
               "AND       A.siefore            =    12    ",                       
               "GROUP BY 1,2 "
               
      END CASE               
                  
   LET g_txt = g_txt CLIPPED
   PREPARE qry28 FROM g_txt
   DECLARE c28 CURSOR FOR qry28
   
   FOREACH c28 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                    g_rec.sal
	 
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH

   #13  11  NOTIFICACION DE SALDO POR CREDITO FOVISSSTE

   #Se va por fecha proceso por lo siguiente:
   #Fecha Pago       JUEVES 1ERO DIA INHABIL  
   #Fecha Valor      JUEVES 1ERO DIA INHABIL
   #Fecha Conversion JUEVES 1ERO DIA INHABIL
   #Fecha Proceso    MARTES 06 DE ABRIL ( Se Inserto este dia en dis_cuenta )
   #MIERCOLES 07 DE ABRIL ( Reporto lo del Martes )
   
   CALL inicializa() 
   
      LET g_txt    =
   
         "SELECT    A.siefore, ",
         "'1311', ",
         "0,NVL(SUM(A.monto_en_acciones),0) ",#SAL  
         "FROM     dis_cuenta  A ",
         -- "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
         "WHERE     A.fecha_proceso     = '", g_dia_ant , "' ",
         "AND       A.tipo_movimiento    = 231  ",
         "AND       A.siefore            =  12  ",
         "AND   EXISTS  ( SELECT 1 FROM acr_det_ced_issste B ",
         "                WHERE A.folio =  B.folio ) ",
         "GROUP BY 1,2 "
   
   LET g_txt = g_txt CLIPPED
   PREPARE qry133 FROM g_txt
   DECLARE c133 CURSOR FOR qry133
   
   FOREACH c133 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
                     
      CALL Act_tbl ( g_rec.* )
   
   END FOREACH
   
   #13  12 APORTACIONES SUBSECUENTES FOVISSSTE

   #Se va por fecha proceso por lo siguiente:
   #Fecha Pago       JUEVES 1ERO DIA INHABIL  
   #Fecha Valor      JUEVES 1ERO DIA INHABIL
   #Fecha Conversion JUEVES 1ERO DIA INHABIL
   #Fecha Proceso    MARTES 06 DE ABRIL ( Se Inserto este dia en dis_cuenta )
   #MIERCOLES 07 DE ABRIL ( Reporto lo del Martes )

   CALL inicializa()
   
      # Tipo Movimiento 6   =    INT EXT SUBSECUENTES GARANTIA  CARGO ( SALIDAS )
      # Tipo Movimiento 7   =    APORTACIONES SUBSECUENTES      CARGO ( SALIDAS )
      
      # Tipo Movimiento 1   =    APORTACION       ABONO ( ENTRADAS )
      # Tipo Movimiento 4   =    INTERES RECIBIDO ABONO ( ENTRADAS )
      
      CASE  g_codigo_afore
               
         WHEN 568  # SOLO AFORE COPPEL   
         
            # Este Proceso  consiste en :
            # ENTRADAS son Aquellos Aportes que realiza el Patron al trabajador con respecto 
            # a la VIVIENDA ,pero dichos Trabajadores ya tienen un Prestamo de INFONAVIT
            # por lo tanto el trabajador en algún momento tiene que devolver o ceder esos
            # recursos y lo hará mediante un Cargo (Salida ) EL 1ER DÍA NATURAL DEL SIGUIENTE
            # MES.
            
            LET g_txt    =  
            
               "SELECT  A.siefore, ",
               "CASE WHEN A.subcuenta IN ( 14,35 )  AND A.tipo_movimiento IN ( 1,4 ) ",
                    "THEN '1312' ",
                    "ELSE '0000' ",
                    "END CASE , ",
                    "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS
               "FROM dis_cuenta A , dis_det_issste B ",
               "WHERE A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND   A.folio             = B.folio ",
               "AND   B.ident_tipo_aport  = '01' ",
               "AND   A.consecutivo_lote  =  B.consec_reg_lote ",
               "AND   B.ident_viv_garantia = 1	",
               "AND   A.siefore  =  12  ",
               "GROUP BY 1,2 " ,  
               "UNION ",
               "SELECT  A.siefore, ",
               "'1312', ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM   dis_cuenta  A ,dis_det_issste C ",               
               "WHERE     A.fecha_proceso     = '", g_dia_ant , "' ",
               "AND    A.folio               = C.folio ",
               "AND    A.consecutivo_lote    = C.consec_reg_lote ",
               "AND    A.tipo_movimiento     IN ( 6,7 ) ",  
               "AND    A.siefore             IN ( 12 ) ",
               "AND    C.ident_viv_garantia  = '1' ",
               "GROUP  BY 1,2 "
                
         OTHERWISE # DE+ AFORES
         
                   # Este Proceso  consiste en :
                   # ENTRADAS son Aquellos Aportes que realiza el Patron al trabajador con respecto 
                   # a la VIVIENDA ,pero dichos Trabajadores ya tienen un Prestamo de INFONAVIT
                   # por lo tanto el trabajador en algún momento tiene que devolver o ceder esos
                   # recursos y lo hará mediante un Cargo (Salida ) EL 1ER DÍA NATURAL DEL SIGUIENTE
                   # MES.
                   
            LET g_txt    =
                                                  
               "SELECT  A.siefore, ",
               "'1312', ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM   dis_cuenta  A ,dis_det_issste C ",
               -- "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "WHERE     A.fecha_proceso     = '", g_dia_ant , "' ",
               "AND    A.folio               = C.folio ",
               "AND    A.consecutivo_lote    = C.consec_reg_lote ",
               "AND    A.tipo_movimiento     IN ( 6,7 ) ",  
               "AND    A.siefore             IN ( 12 ) ",
               "AND    C.ident_viv_garantia  = '1' ",
               "GROUP  BY 1,2 "
            
      END CASE


   LET g_txt = g_txt CLIPPED
   PREPARE qry122 FROM g_txt
   DECLARE c122 CURSOR FOR qry122
   
   FOREACH c122 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
   
      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #----------------------

   #13  13  DEVOLUCION DE MONTOS EXEDENTES FOVISSSTE 

   #Se va por fecha proceso por lo siguiente:
   #Fecha Pago       JUEVES 1ERO DIA INHABIL  
   #Fecha Valor      JUEVES 1ERO DIA INHABIL
   #Fecha Conversion JUEVES 1ERO DIA INHABIL
   #Fecha Proceso    MARTES 06 DE ABRIL ( Se Inserto este dia en dis_cuenta )
   #MIERCOLES 07 DE ABRIL ( Reporto lo del Martes )

   CALL inicializa()

      CASE  g_codigo_afore		

         WHEN 800  # AFORE FICTICIA

         OTHERWISE # DE+ AFORES

            LET g_txt    =

               "SELECT  A.siefore, ",
               "'1313', ",
               "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS
               "FROM   dis_cuenta  A ",
                -- "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",
               "WHERE  A.fecha_proceso     = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento     = 1 ",
               "AND    A.siefore             = 12 ",
               "AND    A.id_aportante        = 'DEV-FOV' ",
               "AND    EXISTS  ( SELECT 1 FROM acr_det_dev_issste B ",
               "                 WHERE A.folio =  B.folio ) ",
               "GROUP  BY 1,2 "

      END CASE


   LET g_txt = g_txt CLIPPED
   PREPARE qry170 FROM g_txt
   DECLARE c170 CURSOR FOR qry170

   FOREACH c170 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
                     
      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #----------------------
   #--  RUBRO OTROS CONCEPTOS ---
   #----------------------

   #10  05  OTROS MOVIMIENTOS INFONAVIT   VIVIENDA VIVIENDA VIVIENDA

   #Se va por fecha proceso por lo siguiente:
   #Fecha Pago       JUEVES 1ERO DIA INHABIL  
   #Fecha Valor      JUEVES 1ERO DIA INHABIL
   #Fecha Conversion JUEVES 1ERO DIA INHABIL
   #Fecha Proceso    MARTES 06 DE ABRIL ( Se Inserto este dia en dis_cuenta )
   #MIERCOLES 07 DE ABRIL ( Reporto lo del Martes )

   CALL inicializa()

      #-- Qry para reflejar las ANUALIDADES GARANTIZADAS (SALIDA)

      CASE  g_codigo_afore		

         WHEN 564  # SOLO AFORE METLIFE
         	
         	 LET g_txt    =
         	  
         	    "SELECT A.siefore, ",
               "CASE ",
                  "WHEN A.id_aportante MATCHES 'DIN*' AND A.tipo_movimiento IN ( 560,570 ) ",                            
                             "THEN '1005' ",#operacion 98 de devolucion de recursos indebidos a infonavit por aplicaciones indebidas  (SALIDA)                                      
                  "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_acciones),0) ,0 ",#E N T R A D A S 
               "FROM      dis_cuenta  A ",                 
               "WHERE     A.fecha_conversion = '",	 g_dia_ant , "' ",   
               "AND       A.subcuenta       IN  ( 4,8 ) ",                          
               "AND       A.tipo_movimiento IN  ( 560,570 ) ", 
               "AND       A.siefore            = 11 ",             
               "GROUP BY 1,2 ",                                    
               "UNION ",
               "SELECT A.siefore, ",
               "CASE ",                 
                  "WHEN A.id_aportante MATCHES 'DIN*' AND A.tipo_movimiento IN ( 565,575 ) ",                         
                        "THEN '1005' ",#operacion 98 de devolucion de recursos indebidos a infonavit por aplicaciones indebidas  (SALIDA)                                      
                  "ELSE '0000' ",
               "END CASE , ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion = '", g_dia_ant , "' ",
               "AND       A.subcuenta       IN  ( 4,8 ) ",             
               "AND       A.tipo_movimiento IN  ( 565,575 ) ",
               "AND       A.siefore            = 11 ",             
               "GROUP BY 1,2 " 
           
         WHEN 568  # SOLO AFORE COPPEL   

            LET g_txt    =
            
               "SELECT A.siefore, ",
               "CASE ",
                  "WHEN A.id_aportante MATCHES 'DIN*' AND A.tipo_movimiento IN ( 560,570 ) ",
                             "AND A.fecha_conversion = '",	 g_dia_ant , "' ",   
                             "THEN '1005' ",#operacion 98 de devolucion de recursos indebidos a infonavit por aplicaciones indebidas  (SALIDA)                                      
                  "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_acciones),0) ,0 ",#E N T R A D A S 
               "FROM      dis_cuenta  A ",   
               "WHERE     A.subcuenta       IN  ( 4,8 ) ",                          
               "AND       A.tipo_movimiento IN  ( 560,570 ) ", 
               "AND       A.siefore            = 11 ",             
               "GROUP BY 1,2 ",                                    
               "UNION ",
               "SELECT A.siefore, ",
               "CASE ",
                  "WHEN A.id_aportante MATCHES 'DIN*' AND A.tipo_movimiento IN ( 565,575 ) ",
                        "AND A.fecha_conversion = '", g_dia_ant , "' ",   
                        "THEN '1005' ",#operacion 98 de devolucion de recursos indebidos a infonavit por aplicaciones indebidas  (SALIDA)                                      
                  "ELSE '0000' ",
               "END CASE , ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.subcuenta       IN  ( 4,8 ) ",
               "AND       A.tipo_movimiento IN  ( 565,575 ) ",
               "AND       A.siefore            = 11 ",             
               "GROUP BY 1,2 " ,
               "UNION ",              #Derivado de JIRA CPL-1274 David Macay
               "SELECT A.siefore, ",
               "CASE ",
                  "WHEN A.id_aportante MATCHES 'PC-*' AND A.tipo_movimiento = 610 ",#CARGO POR AJUSTE OPERATIVO (PAC)
                        "AND A.fecha_conversion = '", g_dia_ant , "' ",   
                        "THEN '1005' ",#Correccion AIVS
                  "ELSE '0000' ",
               "END CASE , ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.subcuenta         =    4 ",#VIVIENDA                           
               "AND       A.tipo_movimiento   =  610 ",#CARGO POR AJUSTE OPERATIVO (PAC)
               "AND       A.siefore           =   11 ",             
               "GROUP BY 1,2 "
              
         WHEN 562  # SOLO AFORE INVERCAP 
         	
         	 LET g_txt    =
         	 
         	    #JIRA INV - 1779 DIC2012            	
         	    "SELECT A.siefore,'1005', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,620 ,A.monto_en_acciones ,0)),0), ", #ENTRADA
               "NVL(SUM (DECODE (A.tipo_movimiento,610 ,A.monto_en_acciones,0)),0) ",   #SALIDA
               "FROM   dis_cuenta  A ",
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento   IN ( 610,620 ) ",
               "AND    A.subcuenta   IN ( 4,8 ) ",
               "AND    A.siefore        = 11 ",      
               "GROUP BY 1,2 ",
         	    "UNION ",
         	    "SELECT A.siefore, ",
               "CASE ",
                  "WHEN A.id_aportante MATCHES 'DIN*' AND A.tipo_movimiento IN ( 560,570 ) ",                            
                             "THEN '1005' ",#operacion 98 de devolucion de recursos indebidos a infonavit por aplicaciones indebidas  (SALIDA)                                      
                  "ELSE '0000' ",
               "END CASE , ",
               "NVL(SUM(A.monto_en_acciones),0) ,0 ",#E N T R A D A S 
               "FROM      dis_cuenta  A ",                 
               "WHERE     A.fecha_conversion = '",	 g_dia_ant , "' ",   
               "AND       A.subcuenta       IN  ( 4,8 ) ",                          
               "AND       A.tipo_movimiento IN  ( 560,570 ) ", 
               "AND       A.siefore            = 11 ",             
               "GROUP BY 1,2 ",                                                     
               "UNION ", #MODIFICACION JIRA INV-1797
               "SELECT    A.siefore, ",
                         "'1005', ",
                         "NVL(SUM(A.monto_en_acciones),0) ,0 ", #ENTRADA
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND       A.tipo_movimiento   =  1 ",                
               "AND       A.id_aportante = 'DEVOLUCION' ",
               "AND       A.siefore      = 11 ",
               "GROUP BY 1,2 ",
               "UNION ",
               "SELECT A.siefore, ",
               "CASE ",                 
                  "WHEN A.id_aportante MATCHES 'DIN*' AND A.tipo_movimiento IN ( 565,575 ) ",                         
                        "THEN '1005' ",#operacion 98 de devolucion de recursos indebidos a infonavit por aplicaciones indebidas  (SALIDA)                                      
                  "ELSE '0000' ",
               "END CASE , ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS
               "FROM      dis_cuenta  A ",
               "WHERE     A.fecha_conversion = '", g_dia_ant , "' ",
               "AND       A.subcuenta       IN  ( 4,8 ) ",             
               "AND       A.tipo_movimiento IN  ( 565,575 ) ",
               "AND       A.siefore            = 11 ",             
               "GROUP BY 1,2 " 

      END CASE


   LET g_txt = g_txt CLIPPED
   PREPARE qry172 FROM g_txt
   DECLARE c172 CURSOR FOR qry172

   FOREACH c172 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal
                    
      CALL Act_tbl ( g_rec.* )

   END FOREACH

   #10  06  OTROS MOVIMIENTOS FOVISSSTE FOVISSSTE FOVISSSTE FOVISSSTE
                                                                                              
   CALL inicializa()                                                                                                                                                                        
                                                                                                     
      CASE  g_codigo_afore	                                       
               
         WHEN 562  # SOLO AFORE INVERCAP            	 
         	        #--  02 ABRIL 2012 
                   #--  288 ( CARGO SEPARACION DE CUENTAS ISSSTE ) 
         	  
            LET g_txt    =    
            
               #JIRA INV - 1779 DIC2012        
          	    "SELECT A.siefore,'1006', ",
               "NVL(SUM (DECODE (A.tipo_movimiento,620 ,A.monto_en_acciones ,0)),0), ", #ENTRADA
               "NVL(SUM (DECODE (A.tipo_movimiento,610 ,A.monto_en_acciones,0)),0) ",   #SALIDA
               "FROM   dis_cuenta  A ",
               "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",
               "AND    A.tipo_movimiento   IN ( 610,620 ) ",  
               "AND    A.subcuenta         IN ( 14,35 ) ",                
               "AND    A.siefore           = 12 ",      
               "GROUP BY 1,2 ",
          	    "UNION ",
          	    "SELECT  A.siefore, ",
               "'1006', ",
               "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS  
               "FROM   dis_cuenta  A ",
               "WHERE  A.fecha_proceso  = '", g_dia_ant , "' ",                
               "AND    A.tipo_movimiento     =  288 ",
               "AND    A.subcuenta           IN ( 14,35 ) ", #JIRA INV-1338 03 DE MAY2012 SE INCLUYO SUBCTA 35
               "AND    A.id_aportante        = 'SEP-ISS' ",
               "AND    A.siefore             =  12 ",                                   
               "GROUP  BY 1,2 "
                                    
         WHEN 568  # SOLO AFORE COPPEL                           

             LET g_txt    =                             

          	     "SELECT  A.siefore, ",                                      
          	     "'1006', ",                                                 
          	     "NVL(SUM(A.monto_en_acciones),0),0 ",#ENTRADAS              
          	     "FROM   dis_cuenta  A ",                                    
          	     "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                     	   
          	     "AND    A.tipo_movimiento     = 86 ",                            
          	     "AND    A.subcuenta           = 14 ",
          	     "AND    A.siefore             = 12 ",                                       	   
          	     "GROUP  BY 1,2 ",                                                    	   
         	     "UNION ",
                "SELECT  A.siefore, ",
                "'1006', ",
                "0,NVL(SUM(A.monto_en_acciones),0) ",#SALIDAS  
                "FROM   dis_cuenta  A ",
                "WHERE  A.fecha_conversion  = '", g_dia_ant , "' ",                
                "AND    A.tipo_movimiento     =  87 ",
                "AND    A.subcuenta           =  35 ",
                "AND    A.siefore             =  12 ",                 
                "GROUP  BY 1,2 "
                
      END CASE
              

   LET g_txt = g_txt CLIPPED
   PREPARE qry201 FROM g_txt
   DECLARE c201 CURSOR FOR qry201

   FOREACH c201 INTO g_rec.sie,g_rec.id_ctasub,g_rec.ent,
                     g_rec.sal

      CALL Act_tbl ( g_rec.* )

   END FOREACH
                 

END FUNCTION     
                 
FUNCTION inicializa()
#i-----------------------
                 
    INITIALIZE g_rec.*  TO NULL
                 
END FUNCTION     
                 
FUNCTION Act_tbl(A_rec)
#i-----------------------
                 
 DEFINE A_rec      RECORD
                   sie           SMALLINT        ,
                   id_ctasub     CHAR(04)        ,
                   ent           DECIMAL ( 22,6 ) ,
                   sal           DECIMAL ( 22,6 ) ,
                   neto          DECIMAL ( 22,6 ) 
                  END RECORD
                  
  DEFINE Pes_rec    RECORD                           
                  sie           SMALLINT        ,  
                  id_ctasub     CHAR(04)        ,  
                  ent           DECIMAL ( 22,2 ) , 
                  sal           DECIMAL ( 22,2 ) , 
                  neto          DECIMAL ( 22,2 )   
                 END RECORD                        
                 
                 
     
    #IF ( A_rec.sie <> 11 ) THEN # SIEFORE SIN VIVIENDA PUROS PESOS
     IF ( A_rec.sie <> 11 AND A_rec.sie <> 12  ) THEN # SIEFORE SIN VIVIENDA PUROS PESOS
        
        LET Pes_rec.sie           =       A_rec.sie       
        LET Pes_rec.id_ctasub     =       A_rec.id_ctasub
        LET Pes_rec.ent           =       A_rec.ent       
        LET Pes_rec.sal           =       A_rec.sal       
        LET Pes_rec.neto          =       A_rec.neto      
                                      

        LET A_rec.sie             =       Pes_rec.sie                                             
        LET A_rec.id_ctasub       =       Pes_rec.id_ctasub                                                       
        LET A_rec.ent             =       Pes_rec.ent                                                      
        LET A_rec.sal             =       Pes_rec.sal                                                             
        LET A_rec.neto            =       Pes_rec.neto       
        
     END IF
     
     
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

     INITIALIZE A_rec.* TO NULL 
     INITIALIZE Pes_rec.* TO NULL 

END FUNCTION 

FUNCTION Act_subcta_00()
#As----------------------
DEFINE   l_sief_pes     SMALLINT
DEFINE   l_id_cta_pes   CHAR(02)
DEFINE   l_sent_pes     ,
         l_ssal_pes     ,
         l_neto_pes     DECIMAL(22,2)


DEFINE   l_sief_acc     SMALLINT      #--quitar
DEFINE   l_id_cta_acc   CHAR(02)      #--quitar
DEFINE   l_sent_acc     ,             #--quitar
         l_ssal_acc     ,             #--quitar 
         l_neto_acc     DECIMAL(22,6) #--quitar
         
DEFINE   l_idctasub      CHAR(02) 
DEFINE   l_ent                       ,
         l_sal           DECIMAL(22,6)
         

LET  l_sief_pes       =    NULL
LET  l_id_cta_pes     =    NULL
LET  l_sent_pes       =    0
LET  l_ssal_pes       =    0


DECLARE d CURSOR FOR     # PESOS


   SELECT A.siefore,A.id_ctasub[1,2],
          NVL(SUM(A.entradas),0),NVL(SUM(A.salidas),0)
   FROM   det_64 A
   WHERE  A.id_ctasub  <> '0000'
   AND    A.siefore IN ( 1,2,3,4,5,6 ) 
   GROUP BY 1,2
   ORDER BY 1,2

   FOREACH d INTO l_sief_pes,l_id_cta_pes,l_sent_pes,l_ssal_pes

         UPDATE
            det_64
            SET    entradas        =  l_sent_pes    ,
                   salidas         =  l_ssal_pes    ,
                   neto            =  l_sent_pes   -  l_ssal_pes   
            WHERE  siefore         =  l_sief_pes       
              AND  id_ctasub[1,2]  =  l_id_cta_pes            
              AND  id_ctasub[3,4]  =  '00'

   END FOREACH


#----LECTURA PARTE VIVIENDA-----#

#--- SOLO SE TRATARÁ EL  NETO DE VIVIENDA DE LAS SIGUIENTES CUENTAS SUBCUENTAS
#--- LAS CUALES TIENEN LA PECULIARIDAD DE QUE TIENEN CTAS SUBCTAS DE VIVIENDA
#--- INFONAVIT Y FOVISSSTE.

#--- 05 00 CORRECCION DE CUENTAS 
#--  09 00 DEVOLUCION DE PAGOS SIN JUSTIFICACION LEGAL
#--  10 00 OTROS CONCEPTOS

LET    l_idctasub           =  NULL
LET    l_ent                =  0
LET    l_sal                =  0

DECLARE z_1   CURSOR FOR

  SELECT A.id_ctasub[1,2]         ,
         NVL(SUM(A.entradas),0)   ,
         NVL(SUM(A.salidas),0)
  FROM   det_64 A
  WHERE  A.id_ctasub  <> '0000' 
  AND    A.id_ctasub[1,2] IN ( '05','09','10' )      
  AND    A.siefore  IN ( 11,12 )
  GROUP BY 1
  ORDER BY 1  
  
FOREACH z_1 INTO l_idctasub  ,
	               l_ent       ,
	               l_sal 
	 
	              
   UPDATE
   det_64
   SET    entradas        =  l_ent          ,
          salidas         =  l_sal          ,
          neto            =  l_ent     -  l_sal   
   WHERE  siefore         =  g_viv
   AND    id_ctasub[1,2]  =  l_idctasub            
   AND    id_ctasub[3,4]  =  '00'
	               
END FOREACH	  

#-----------------                        -----------------#
#--   SE SACARÁ EL NETEO  DE TODAS AQUELLAS CTAS SUBCUENTAS 
#--   DIFERENTES A:
#---  05 00 CORRECCION DE CUENTAS 
#--   09 00 DEVOLUCION DE PAGOS SIN JUSTIFICACION LEGAL
#--   10 00 OTROS CONCEPTOS



LET  l_sief_acc       =    NULL
LET  l_id_cta_acc     =    NULL 
LET  l_sent_acc       =    0    
LET  l_ssal_acc       =    0   

DECLARE z CURSOR FOR     # ACC S


   SELECT A.siefore,A.id_ctasub[1,2],
          NVL(SUM(A.entradas),0),NVL(SUM(A.salidas),0)
   FROM   det_64 A   
   WHERE  A.id_ctasub  <> '0000'    
   AND    A.id_ctasub[1,2] NOT IN ( '05','09','10' )
   AND    A.siefore      IN    ( 11,12 )  
   GROUP BY 1,2
   ORDER BY 1,2

   FOREACH z INTO l_sief_acc,l_id_cta_acc,l_sent_acc,l_ssal_acc

         UPDATE
            det_64
            SET    entradas        =  l_sent_acc    ,
                   salidas         =  l_ssal_acc    ,
                   neto            =  l_sent_acc   -  l_ssal_acc   
            WHERE  siefore         =  l_sief_acc       
              AND  id_ctasub[1,2]  =  l_id_cta_acc            
              AND  id_ctasub[3,4]  =  '00'

   END FOREACH

END FUNCTION

FUNCTION act_n_envios()            
#ane-------------------------  


  LET     g_ultimo           =           0                
   
  SELECT  MAX(n_envios) + 1
  INTO    g_ultimo	
  FROM    safre_af:est_det_64
  WHERE   fec_dia_habil_ant  =   g_dia_ant
  
  
  
  IF ( g_ultimo   =   0 OR
       g_ultimo IS NULL OR
       g_ultimo   = " " OR
       g_ultimo   = ""  )  THEN       
	               	         	                	
  	 LET g_ultimo =   1             	               	 
    UPDATE
       safre_af:est_det_64
       SET  n_envios           =      g_ultimo
    WHERE   fec_dia_habil_ant  =      g_dia_ant
  
  END IF     

END FUNCTION

FUNCTION llena_tblfisiviv11_12()            
#pp-------------------------  
DEFINE  v_siefore_acc               SMALLINT, 
        v_id_ctasub_acc             CHAR(04), 
        v_entradas_acc                      , 
        v_salidas_acc                       , 
        v_neto_acc          DECIMAL ( 22,6 ) 


   INITIALIZE   v_siefore_acc      TO  NULL    
   INITIALIZE   v_id_ctasub_acc    TO  NULL    
   INITIALIZE   v_entradas_acc     TO  NULL
   INITIALIZE   v_salidas_acc      TO  NULL
   INITIALIZE   v_neto_acc         TO  NULL
   
   DECLARE cur444 CURSOR FOR
   

   SELECT *
   FROM   det_64 A
   WHERE  A.id_ctasub  <> "0000"          
   AND    A.siefore    IN   ( 11,12 )

   ORDER BY siefore,id_ctasub

   FOREACH cur444    INTO v_siefore_acc    ,
                          v_id_ctasub_acc  ,
                          v_entradas_acc   ,
                          v_salidas_acc    ,
                          v_neto_acc

  
      #-- SE INSERTARÀN LOS DATOS A LA TABLA FISICA CREADA safre_af:est_det_64 --#
      #-- SOLO INSERTARA LA SIEFORES DE VIVIENDA 11(SUBCTA 4,8) Y 12(SUBCTA 14,35)       
      
      
      #--VALIDA QUE SE INSERTEN ID CTAS ID SUBCUENTAS SOLO < CON MONTO >   --#
          
      IF ( v_entradas_acc <> 0   OR v_salidas_acc <> 0  ) THEN                   
      	                
         INSERT INTO safre_af:est_det_64 VALUES ( g_dia_ant            ,
                                                  g_ultimo             ,
                                                  v_siefore_acc        ,
                                                  v_id_ctasub_acc      ,
                                                  v_entradas_acc       ,
                                                  v_salidas_acc        ,
                                                  v_neto_acc           ,
                                                  g_usuario )
         
         IF ( v_siefore_acc = 12 ) THEN
         	
         	  UPDATE
         	     det_64
         	     SET   entradas       =    v_entradas_acc                ,
         	           salidas        =    v_salidas_acc                 ,
         	           neto           =    v_neto_acc                    
         	  WHERE    siefore        =    11
         	    AND    id_ctasub      =    v_id_ctasub_acc
         	     
         	  
         END IF                            
                 
      END IF

   END FOREACH 
   
   DELETE
          FROM det_64
   WHERE  siefore        =         12
   
END FUNCTION

FUNCTION valida_folLiqRecep_Ced()
#hs--------------------------------             

   
#Las Siguientes Consultas validarán 
#si Existen folios Liquidados de los
#procesos de Cedente y Receptora.
#Tanto para las Corridas Bimestrales como
#para las Nuevas Corridas Mensuales.
#
#Con esto nos cersioraremos que solo busque
#La Información de las Consultas cuando haya
#que buscarla tanto para Cedente como Receptora
#esto ayudará a que no se alente el proceso
#diariamiente.

   LET   g_hayinf_rec_ced  = "N"  #BANDERA APAGADA DE HAY REGISTROS LIQUIDADOS
                                  #YA SEA DE CEDENTE O RECEPTORA. 
                                  

   LET  g_sumaregscedrec        =   0

   #P R O C E S O     DE     R E C E P T O R A                                            
   
   LET    g_numreg_recep        =   0
   SELECT COUNT(*)                                        
   INTO   g_numreg_recep
   FROM   taa_ctr_traspaso A                              
   WHERE  DATE(A.fin_liquida) = g_dia_ant                 
   AND    EXISTS ( SELECT 1                               
                   FROM  dis_cuenta B                     
                   WHERE A.folio =   B.folio              
                   AND   B.fecha_conversion  = g_dia_ant )
   
   IF ( g_numreg_recep IS NULL  OR
   	    g_numreg_recep  = ""    OR
   	    g_numreg_recep  = " " ) THEN
   
   
      LET g_numreg_recep  = 0
   	    	
   END IF  	
   
                                                     
   #P R O C E S O     DE     C E D E N T E
   
   LET    g_numreg_ced        =   0
   SELECT COUNT(*)   
   INTO   g_numreg_ced                                   
   FROM   taa_cd_ctr_folio A                              
   WHERE  A.fecha_liquidacion = g_dia_ant                    
   AND    A.estado            = 103 --liquidado              
   AND    EXISTS ( SELECT 1                               
                   FROM  dis_cuenta B                       
                   WHERE A.folio =   B.folio              
                   AND   B.fecha_conversion  = g_dia_ant )
                   
   IF ( g_numreg_ced IS NULL  OR
   	    g_numreg_ced  = ""    OR
   	    g_numreg_ced  = " " ) THEN
   
   
      LET g_numreg_ced  = 0
   	    	
   END IF  	    	

   #----------------                     ---------------#
   #----------------                     ---------------#
   
   LET  g_sumaregscedrec        =  g_numreg_recep  +
                                   g_numreg_ced
                                   
   IF g_sumaregscedrec  >  0 THEN #SE PRENDE BANDERA HAY REGISTROS LIQUIDADOS
   	                              #YA SEA DE CEDENTE O RECEPTORA.

      LET g_hayinf_rec_ced  = "S"  	                              
   	
   END IF


                   
                   
END FUNCTION       
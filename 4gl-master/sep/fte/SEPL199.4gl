DATABASE   safre_af
GLOBALS  
   DEFINE  g_nss                  CHAR(11),
           g_sql                  CHAR(500),
           g_tabname              CHAR(30),
           g_enter                CHAR,
           g_arr_curr                      ,
           g_scr_line                      ,
           g_reg                           ,
           g_scta                          ,
           g_arr_count                     ,
           g_cargos                        ,
           g_tot_reg              SMALLINT


   DEFINE   arr_movtos          ARRAY[2000]   OF   RECORD
            subcuenta                       LIKE  dis_cuenta.subcuenta,
            siefore                         LIKE  dis_cuenta.siefore,
            fecha_conversion                LIKE  dis_cuenta.fecha_conversion,
            tipo_movimiento                 LIKE  dis_cuenta.tipo_movimiento,
            id_aportante                    LIKE  dis_cuenta.id_aportante,
            acciones                        LIKE  dis_cuenta.monto_en_acciones,
            pesos                           LIKE  dis_cuenta.monto_en_pesos
                                      END   RECORD

   DEFINE   l_movtos                  RECORD
            subcuenta                       LIKE  dis_cuenta.subcuenta,
            siefore                         LIKE  dis_cuenta.siefore,
            fecha_conversion                LIKE  dis_cuenta.fecha_conversion,
            tipo_movimiento                 LIKE  dis_cuenta.tipo_movimiento,
            id_aportante                    LIKE  dis_cuenta.id_aportante,
            acciones                        LIKE  dis_cuenta.monto_en_acciones,
            pesos                           LIKE  dis_cuenta.monto_en_pesos
                                      END   RECORD
 
END  GLOBALS
MAIN
   OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
#           DEFER    INTERRUPT
  LET       g_reg                       =  1
  LET       g_cargos                    =  1
  OPEN     WINDOW    PREPBIM        AT  2,2
            WITH      FORM     "SEPL199"  ATTRIBUTE(BORDER)
   DISPLAY  "<<SEPL199>>      SEPARACION  DE  CUENTAS                   ",
            "                   "    AT  2,2   ATTRIBUTE(REVERSE)
   DISPLAY  "   MUESTRA  LOS  MOVIMIENTOS  HISTORICOS  DEL  TRABAJADOR     ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)
   PROMPT   "       TECLEE  NUMERO DE CUENTA  ...?     "  for  g_nss
   PROMPT   "       TECLEE  1= Cargos  2= Todos los Movtos....?  "  for g_cargos
   PROMPT   "       TECLEE  subcuenta                            "  for g_scta
   DISPLAY  "   MUESTRA  LOS  MOVIMIENTOS  HISTORICOS  DEL  TRABAJADOR: ",g_nss,
            "            "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  "   PROCESANDO  INFORMACION....."    AT  21,2
   LET       g_enter         =  0

   DECLARE   c_tab       CURSOR  FOR
    SELECT   nombre_tabla
      FROM   safre_af:taa_cd_tab_cuenta
     
   FOREACH   c_tab     INTO   g_tabname
      IF        g_cargos        =  1   THEN
         LET      g_sql                    =
                  ' SELECT   subcuenta,siefore,fecha_conversion,',
                  '          tipo_movimiento,',
                  '          id_aportante,monto_en_acciones,monto_en_pesos  ',
                  '   FROM  ',  g_tabname      CLIPPED ,' ' ,
                  '  WHERE    nss             =    ','"',g_nss,'"',
                  '    AND    subcuenta       =    ','"',g_scta,'"',
                  '    AND    monto_en_pesos       <  0  ',
                  '    AND    tipo_movimiento  NOT  BETWEEN 100  AND  110  ',
                  '  ORDER    BY  1,2,3,4 ;' 
      ELSE
         LET      g_sql                    =
                  ' SELECT   subcuenta,siefore,fecha_conversion,',
                  '          tipo_movimiento,',
                  '          id_aportante,monto_en_acciones,monto_en_pesos  ',
                  '   FROM  ',  g_tabname      CLIPPED ,' ' ,
                  '  WHERE    nss             =    ','"',g_nss,'"',
                  '    AND    subcuenta       =    ','"',g_scta,'"',
                  '  ORDER    BY  1,2,3,4 ;' 
      END IF
         
         PREPARE  sql_m         FROM   g_sql
         DECLARE  c_movtos      CURSOR     FOR  sql_m
         FOREACH  c_movtos      INTO   l_movtos.*
              LET      arr_movtos[g_reg].*   =  l_movtos.*
              LET      g_reg                 =  g_reg      +  1
         END FOREACH
   END FOREACH
   LET      g_reg             =  g_reg      -  1
   CALL     SET_COUNT(g_reg)
   DISPLAY  ARRAY    arr_movtos        TO scr_movtos.*

            ON KEY (INTERRUPT)
                 EXIT  PROGRAM
   END DISPLAY

END MAIN


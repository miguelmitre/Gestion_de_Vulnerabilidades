DATABASE   safre_af
GLOBALS
   DEFINE  g_nss                  CHAR(11),
           g_nss_invadido         CHAR(11),
           g_nss_reclamante       CHAR(11),
           g_sql_42               CHAR(300),
           l_grupo                         ,
           g_scta                          ,
           g_cargos                        ,
           cero                            ,
           g_siefore                       ,
           g_enter                SMALLINT ,
           g_acciones                      ,
           g_pesos                DEC(16,6),
           g_folio                DEC(10,0),
           g_fecha_conversion              ,
           g_today                DATE     
   DEFINE  g_cta      RECORD   LIKE   safre_af:dis_cuenta.*
END  GLOBALS
MAIN
      OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
#           DEFER    INTERRUPT
     CALL      f_010_inicio()
     CALL      f_100_proceso()
    
END MAIN
FUNCTION    f_010_inicio()
   LET      g_today                     =  TODAY  -  1
   SELECT   UNIQUE( folio )
     INTO   g_folio
     FROM   safre_af:dis_cuenta
    WHERE   nss                 =  g_nss_invadido
      AND   fecha_conversion    =  g_today
      AND   tipo_movimiento     =  280;
   IF       g_folio           IS   NULL    OR
            g_folio             =  0    THEN
            DISPLAY  "  ANTES DEBE LIQUIDAR LA SEPARACION PARA PODER AJUSTAR...."           AT    10,5
           PROMPT   "   TECLEE  ENTER PARA SALIR ...."  FOR  g_enter
           EXIT  PROGRAM
   END IF
   DISPLAY  "     FOLIO  DE  OPERACION ====> : ",g_folio   AT  10,5
   SELECT   nss_invadido
     INTO   g_nss_invadido
     FROM   safre_tmp:sep_ajuste;
   SELECT   nss
     INTO   g_nss_reclamante
     FROM   safre_af:sep_det_reg_sol_reclamante  
    WHERE   folio                       =  g_folio
      AND   nss                         =  g_nss_invadido;
   LET      g_enter                     =  0
   OPEN     WINDOW    PREPBIM        AT  2,2
            WITH      FORM     "SEPC0198"  ATTRIBUTE(BORDER)
   DISPLAY  "<<SEPC0198>>      SEPARACION  DE  CUENTAS                   ",
            "                   "    AT  2,2   ATTRIBUTE(REVERSE)
   DISPLAY  "   MUESTRA  LOS  MOVIMIENTOS  HISTORICOS  DEL  TRABAJADOR     ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  "  TRASPASA  LOS  SALDOS AL DIA  DEL  NSS INVADIDO AL SEPARADOR ",
            "            "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  "  NSS INVADIDO : ",g_nss_invadido, "    NSS  SEPARADOR: ",g_nss_reclamante   AT    6,5
   PROMPT   "   TECLEE  1  PARA  EFECTUAR  LA SEPARACION...."  FOR  g_enter
   IF       g_enter             IS  NULL   OR
            g_enter             <>  1  THEN
            EXIT   PROGRAM
   END  IF
   DISPLAY  "   PROCESANDO  INFORMACION....."    AT  20,2

END FUNCTION
FUNCTION    F_100_proceso()
   CALL     F_130_ajusta()
   DELETE   FROM    safre_af:dis_cuenta
   WHERE    folio                        =  g_folio
     AND    nss                          =  g_nss_invadido
     AND    sucursal               NOT  IN("SEPC0198");
   DELETE   FROM    safre_af:dis_cuenta
   WHERE    folio                        =  g_folio
     AND    nss                          =  g_nss_reclamante
     AND    sucursal               NOT  IN("SEPC0198");
   UPDATE   safre_af:dis_cuenta
      SET   sucursal                     = "0"
   WHERE    folio                        =  g_folio
     AND    nss                          =  g_nss_invadido
     AND    tipo_movimiento              =  280
     AND    sucursal               NOT  IN("SEPC0198");

   UPDATE   safre_af:dis_cuenta
      SET   sucursal                     = "0"
   WHERE    folio                        =  g_folio
     AND    nss                          =  g_nss_reclamante
     AND    tipo_movimiento              =  590
     AND    sucursal               NOT  IN("SEPC0198");
   PROMPT   "   FIN DE PROCESO TECLEE ENTER PARA  SALIR ...."  FOR  g_enter
END FUNCTION

FUNCTION    F_130_ajusta()
   DEFINE   l_grupo                             SMALLINT
   LET      g_sql_42                      =
            ' EXECUTE  FUNCTION  fn_saldo_dia (?,?,?,?) ;  '
   PREPARE  sql_42          FROM     g_sql_42   # nuevo para saldos
   LET      cero                          =  0
   LET      l_grupo                       =  0
   LET      g_acciones                    =  0
   LET      g_pesos                       =  0
   DECLARE  c_saldo       CURSOR     FOR  sql_42
   FOREACH  c_saldo
            USING     g_nss_invadido,
                      cero        ,   #  subcuenta
                      l_grupo     ,
                      g_today
             INTO     g_scta      ,
                      g_siefore   ,
                      g_acciones  ,
                      g_pesos
           UPDATE    safre_af:dis_cuenta
              SET    monto_en_acciones      =  g_acciones   *  -1,
                     monto_en_pesos         =  g_pesos      *  -1,
                     sucursal               =  "SEPC0198"
            WHERE    folio                  =  g_folio
             AND     nss                    =  g_nss_invadido
             AND     tipo_movimiento        =  280
             AND     subcuenta              =  g_scta
             AND     siefore                =  g_siefore
           UPDATE    safre_af:dis_cuenta
              SET    monto_en_acciones      =  g_acciones,
                     monto_en_pesos         =  g_pesos   ,
                     sucursal               =  "SEPC0198"
            WHERE    folio                  =  g_folio
             AND     nss                    =  g_nss_reclamante
             AND     tipo_movimiento        =  590
             AND     subcuenta              =  g_scta
             AND     siefore                =  g_siefore
   END  FOREACH
END FUNCTION

#######################################################################
#PROGRAMA:SEPL011                                                     #
#OBJETIVO:SEPARA MONTOS DE TRASPASOS HISTORICO DE INGRESO             #
#RERECHOS RESERVADOS:EFP                                              #
#AUTOR:JOSE FRANCISCO LUGO CORNEJO                                    #
#DESARROLLADO :  13/MARZO/2009                                        #
#######################################################################
DATABASE   safre_af
GLOBALS  
   DEFINE  g_nss                  CHAR(11),
           g_sql                  CHAR(500),
           g_tabname              CHAR(30),
           g_usuario              CHAR(08),
           g_today                DATE,
           g_enter                CHAR,
           g_arr_curr                      ,
           g_scr_line                      ,
           g_reg                           ,
           g_arr_count                     ,
           i                               ,
           g_lastkey                       ,
           g_tot_reg              SMALLINT,
           g_folio                DEC(10,0)


   DEFINE   arr_movtos          ARRAY[100]   OF   RECORD
            porc_sep                              SMALLINT,
            fecha_conversion                LIKE  dis_cuenta.fecha_conversion,
            subcuenta                       LIKE  dis_cuenta.subcuenta,
            tipo_movimiento                 LIKE  dis_cuenta.tipo_movimiento,
            id_aportante                    LIKE  dis_cuenta.id_aportante,
            acciones                        LIKE  dis_cuenta.monto_en_acciones,
            pesos                           LIKE  dis_cuenta.monto_en_pesos
                                      END   RECORD

   DEFINE   g_movtos                  RECORD
            porc_sep                              SMALLINT,
            fecha_conversion                LIKE  dis_cuenta.fecha_conversion,
            subcuenta                       LIKE  dis_cuenta.subcuenta,
            tipo_movimiento                 LIKE  dis_cuenta.tipo_movimiento,
            id_aportante                    LIKE  dis_cuenta.id_aportante,
            acciones                        LIKE  dis_cuenta.monto_en_acciones,
            pesos                           LIKE  dis_cuenta.monto_en_pesos
                                      END   RECORD
 
   DEFINE   g_dividir                 RECORD
            porc_sep                              SMALLINT,
            fecha_conversion                LIKE  dis_cuenta.fecha_conversion,
            subcuenta                       LIKE  dis_cuenta.subcuenta,
            tipo_movimiento                 LIKE  dis_cuenta.tipo_movimiento,
            id_aportante                    LIKE  dis_cuenta.id_aportante,
            acciones                        LIKE  dis_cuenta.monto_en_acciones,
            pesos                           LIKE  dis_cuenta.monto_en_pesos
                                      END   RECORD
END  GLOBALS
MAIN
   CALL STARTLOG ("SEPL011.log")
   OPTIONS  INPUT    WRAP,
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
#           DEFER    INTERRUPT
   CALL     F_010_inicio()
   CALL     F_100_proceso()
END MAIN

FUNCTION    F_010_inicio()
   DEFINE   l_pat                        ,
            l_mat                        ,
            l_nom                        CHAR(80)
   DEFINE   l_registros                  ,
            l_no_existe                  ,
            l_separado                   SMALLINT
   DEFINE   l_ano                        CHAR(04)
   DEFINE   l_prov        RECORD         LIKE   dis_provision.*
   DEFINE   l_f_banxico                  DATE
   LET      g_today                     =   TODAY
   LET      g_reg                       =  1
   OPEN     WINDOW    PREPBIM        AT  2,2
            WITH      FORM     "SEPL011"  ATTRIBUTE(BORDER)
   DISPLAY  "<<SEPL011>>            SEPARACION  DE  CUENTAS            ",
     g_today  USING  "DD/MM/YYYY", "         "    AT  2,2   ATTRIBUTE(REVERSE)
   DISPLAY  "     SEPARACION DE MONTOS POR PORCENTAJE DE INGRESO POR TRASPASO  ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  " <CTRL-C>     PARA  CANCELAR     O   <ESC>  PARA  SEPARAR  " AT 5,2
   INPUT  BY  NAME  g_nss
         AFTER   FIELD   g_nss
         IF      g_nss                         IS  NULL   THEN
                 ERROR  " NSS  NO PUEDE SER NULO   "
                 NEXT  FIELD    g_nss
         ELSE
                 SELECT  1     INTO    l_no_existe
                   FROM  safre_af:sep_det_solicitud
                  WHERE  n_seguro        =  g_nss
                   AND   diag_confronta           =  "01"
                   AND   clasifica_separacion     =  "C";
                 IF      l_no_existe           IS  NULL   OR
                         l_no_existe             <>  1     THEN
                         ERROR   "   SOLO PUEDE SEPARAR LOS CASOS  01 C    "
                         LET     g_nss            =  NULL
                         NEXT  FIELD    g_nss
                 END  IF
         END IF
         ON      KEY    (ESC)
                 IF      g_nss                         IS  NULL   THEN
                         ERROR  " NSS  NO PUEDE SER NULO   "
                         NEXT  FIELD    g_nss
                 END IF
                 EXIT   INPUT
         ON KEY ( INTERRUPT )
                 EXIT PROGRAM
   END INPUT

   DISPLAY  "   MUESTRA  LOS  MOVIMIENTOS  HISTORICOS  DEL  TRABAJADOR: ",
            "            "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  "   PROCESANDO  INFORMACION....."    AT  22,2
   LET       g_enter                       =  0
   LET       g_tabname                     =  "dis_cuenta"
   SELECT    MAX(folio),user
     INTO    g_folio,g_usuario
     FROM    safre_af:taa_viv_recepcion
   WHERE     nss                           =  g_nss
     AND     ident_operacion               =  "09";
   IF        g_folio           IS  NULL    OR
             g_folio            =  0    THEN
             ERROR    "   NO  EXISTE  EN  RECEPTORA      "
             PROMPT  "  TECLEE  ENTER  PARA  SALIR   "  FOR  g_enter
             EXIT  PROGRAM
   END  IF
   SELECT    fecha_mov_banxico,paterno_cedente,materno_cedente,
             nombres_cedente
     INTO    l_f_banxico,l_pat,l_mat,l_nom
     FROM    safre_af:taa_viv_recepcion
   WHERE     nss                           =  g_nss
     AND     folio                         =  g_folio;
   LET       l_nom           =  l_nom  CLIPPED," ",
             l_pat  CLIPPED," ",l_mat  CLIPPED
   DISPLAY   l_nom        TO  nombre
   DISPLAY   g_folio      TO  folio
   LET       l_registros                   =  0
   SELECT    COUNT(*)
     INTO    l_registros
     FROM    safre_af:dis_cuenta
    WHERE    nss                           =  g_nss
      AND    folio                         =  g_folio;
   DELETE    FROM    safre_af:dis_provision
    WHERE    nss                           =  g_nss
      AND    folio                         =  g_folio;
   IF        l_registros                  IS  NULL   OR
             l_registros                   <  1      THEN
             LET       l_ano               =  l_f_banxico   USING  "YYYY"
             LET       g_tabname           =  "dis_cuenta",l_ano[3,4] 
   END IF

   LET       g_sql                    =
            ' SELECT   *  ',
            ' FROM ', g_tabname  CLIPPED ,
            '  WHERE    nss                 =  ','"',g_nss,'"',
            '    AND    tipo_movimiento     =  1 ',
            '    AND    folio               =  ',g_folio,
            '  ORDER    BY  1,2,3 ;' 
   PREPARE  sql_m         FROM   g_sql
   DECLARE  c_movtos      CURSOR     FOR  sql_m
   FOREACH  c_movtos      INTO   l_prov.*
         LET      l_separado            =  0
         SELECT   1
           INTO   l_separado
           FROM   sep_his_traspasos
          WHERE   nss                   =  g_nss
            AND   folio                 =  g_folio
            AND   subcuenta             =  l_prov.subcuenta;
         IF       l_separado            =  1     THEN
                  CONTINUE   FOREACH
         END  IF
         INSERT        INTO   safre_af:dis_provision
                       VALUES  (l_prov.*)
         LET      g_movtos.porc_sep             =  0
         LET      g_movtos.fecha_conversion     =  l_prov.fecha_conversion
         LET      g_movtos.subcuenta            =  l_prov.subcuenta
         LET      g_movtos.tipo_movimiento      =  l_prov.tipo_movimiento
         LET      g_movtos.id_aportante         =  l_prov.id_aportante
         LET      g_movtos.acciones             =  l_prov.monto_en_acciones
         LET      g_movtos.pesos                =  l_prov.monto_en_pesos
         LET      arr_movtos[g_reg].*           =  g_movtos.*
         LET      g_reg                         =  g_reg      +  1
   END FOREACH
   CALL     SET_COUNT(g_reg -1)
END  FUNCTION

FUNCTION    F_100_proceso()
   DEFINE   l_cur                           ,
            l_arr_count                     ,
            l_scr_line                      ,
            l_perfil_cod                    ,
            l_arr_curr                      SMALLINT
   INPUT   ARRAY    arr_movtos  WITHOUT DEFAULTS  FROM  scr_movtos.*
           BEFORE  ROW
                   LET      l_arr_curr         =  ARR_CURR()
                   LET      l_arr_count        =  ARR_COUNT() + 1
                   LET      l_scr_line         =  SCR_LINE()
                   IF       l_arr_curr         >  l_arr_count    THEN
                            EXIT INPUT
                   END IF
                   DISPLAY  arr_movtos[l_arr_curr].*   TO
                            scr_movtos[l_scr_line].*
                                ATTRIBUTE(REVERSE)
           AFTER    FIELD    porc_sep
                   IF       l_arr_curr     >=  (l_arr_count)  THEN
                            LET      g_lastkey     =   FGL_LASTKEY()
                            IF     ((g_lastkey     =
                                     FGL_KEYVAL("down"))      OR
                                    (g_lastkey     =
                                     FGL_KEYVAL("return"))    OR
                                    (g_lastkey     =
                                     FGL_KEYVAL("tab"))       OR
                                    (g_lastkey     =
                                      FGL_KEYVAL("right"))) THEN
                                    ERROR    "    NO HAY MAS REGISTROS",
                                    " EN ESTA DIRECCION ........      "
                                    NEXT    FIELD     porc_sep
                            END IF
                   END IF
           AFTER   ROW
                   LET      l_arr_curr         =  ARR_CURR()
                   LET      l_scr_line         =  SCR_LINE()
                   DISPLAY  arr_movtos[l_arr_curr].*   TO
                            scr_movtos[l_scr_line].*

           ON KEY (ESC)
                 CALL     F_120_divide_montos()
                 PROMPT  "  SEPARACION  EXITOSA  TECLEE <ENTER> PARA SALIR "
                          FOR   g_enter
                 EXIT     PROGRAM

   END INPUT
END FUNCTION

FUNCTION    F_120_divide_montos()
   DEFINE   l_prov           RECORD   LIKE   dis_provision.*
   DEFINE   g_current              DATETIME   YEAR  TO  SECOND
   DEFINE   l_porc_acc              DEC(10,6)
   LET      g_sql                       =
            '  INSERT     INTO ', g_tabname  CLIPPED ,
            '  VALUES  (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ; '
   PREPARE  sql_ins         FROM     g_sql
   LET      g_current              =  CURRENT
   FOR      i     =  1           TO  50  
            IF     arr_movtos[i].subcuenta         >  0     THEN
                   IF     arr_movtos[i].porc_sep         =  0     THEN
                          DELETE    FROM   dis_provision
                           WHERE    nss          =  g_nss
                             AND    folio        =  g_folio
                             AND    subcuenta    =  arr_movtos[i].subcuenta
                          INITIALIZE      arr_movtos[i].*  TO  null
                    END IF
            END IF
   END FOR
   DECLARE   c_prov       CURSOR    FOR
   SELECT    *
     FROM    dis_provision
    WHERE    nss                      =  g_nss
      AND    folio                    =  g_folio
   FOREACH   c_prov                INTO  l_prov.*
         INSERT    INTO   sep_his_traspasos
            VALUES   (g_nss,
                      g_folio         ,
                      l_prov.subcuenta,
                      l_prov.monto_en_acciones,
                      l_prov.monto_en_pesos,
                      g_today ,
                      g_current,
                      g_usuario)
         LET       g_sql                    =
                  ' DELETE   FROM ', g_tabname  CLIPPED ,
                  '  WHERE    nss                 =  ','"',g_nss,'"',
                  '    AND    subcuenta           =  ',l_prov.subcuenta,
                  '    AND    folio               =  ',g_folio
         PREPARE  sql_del         FROM     g_sql
         EXECUTE  sql_del
         
         FOR       i            =  1    TO  100
                   IF     l_prov.subcuenta  =  arr_movtos[i].subcuenta  THEN
                          EXIT   FOR
                   END  IF
         END  FOR
         LET       l_porc_acc   =  l_prov.monto_en_acciones
                                *  arr_movtos[i].porc_sep  / 100
         LET       l_prov.monto_en_acciones    =  l_prov.monto_en_acciones
                                               -  l_porc_acc
         LET       l_prov.monto_en_pesos       =  l_prov.monto_en_acciones
                                               *  l_prov.precio_accion
         EXECUTE   sql_ins      USING
                   l_prov.tipo_movimiento    ,
                   l_prov.subcuenta          ,
                   l_prov.siefore            ,
                   l_prov.folio              ,
                   l_prov.consecutivo_lote   ,
                   l_prov.nss                ,
                   l_prov.curp               ,
                   l_prov.folio_sua          ,
                   l_prov.fecha_pago         ,
                   l_prov.fecha_valor        ,
                   l_prov.fecha_conversion   ,
                   l_prov.monto_en_pesos     ,
                   l_prov.monto_en_acciones  ,
                   l_prov.precio_accion      ,
                   l_prov.dias_cotizados     ,
                   l_prov.sucursal           ,
                   l_prov.id_aportante       ,
                   l_prov.estado             ,
                   l_prov.fecha_proceso      ,
                   l_prov.usuario            ,
                   l_prov.fecha_archivo      ,
                   l_prov.etiqueta       

         LET       l_prov.monto_en_acciones    =  l_porc_acc
         LET       l_prov.monto_en_pesos       =  l_porc_acc
                                               *  l_prov.precio_accion
         EXECUTE   sql_ins      USING
                   l_prov.tipo_movimiento    ,
                   l_prov.subcuenta          ,
                   l_prov.siefore            ,
                   l_prov.folio              ,
                   l_prov.consecutivo_lote   ,
                   l_prov.nss                ,
                   l_prov.curp               ,
                   l_prov.folio_sua          ,
                   l_prov.fecha_pago         ,
                   l_prov.fecha_valor        ,
                   l_prov.fecha_conversion   ,
                   l_prov.monto_en_pesos     ,
                   l_prov.monto_en_acciones  ,
                   l_prov.precio_accion      ,
                   l_prov.dias_cotizados     ,
                   l_prov.sucursal           ,
                   l_prov.id_aportante       ,
                   l_prov.estado             ,
                   l_prov.fecha_proceso      ,
                   l_prov.usuario            ,
                   l_prov.fecha_archivo      ,
                   l_prov.etiqueta       

   END FOREACH
END FUNCTION



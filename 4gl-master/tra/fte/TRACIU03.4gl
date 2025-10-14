###########################################################################
# Solicitante Afores 
# /Siefore Origen trapaso
# Origen trapaso
#       61            SAR ISSSTE afore (siefore trabajadores afiliados por
#                     NSS  llave nss/curp) Tipo Admon Siefore.
#
#
#       63            SAR ISSSTE afore (siefore trabajadores no afiliados por
#                     CURP llave curp) Tipo Admon Siefore.             
#---------------------------------------------------------------------------
# Solicitante PENSION  
# ISSSTE  BANXICO 
# Origen trapaso
#       
#       62            SAR ISSSTE-afore (Banxico ) para trabajadores no   
#                     afiliados por CURP llave CURP Tipo de Admon 
#                     Banxico.
#Fecha Creacion:      22 - FEBRERO -2011
#Req:627              => JCPV 08/11/2011. Corr. more than one record ln 275
###########################################################################
DATABASE safre_af

DEFINE   g_enter            CHAR(01)

GLOBALS  "TRACIU02g.4gl"


FUNCTION  f_pide_folio(li_folio)

  DEFINE    li_folio       INTEGER

  CALL init()


  CALL f_actualiza(li_folio)  
  
  DISPLAY "----------------------------------------------------------------------------"  AT 18,1 
  
  PROMPT "PROCESO FINALIZADO...PRESIONE <ENTER> PARA SALIR.."
            FOR CHAR g_enter  
    
  EXIT PROGRAM
  

END FUNCTION

#-----i
FUNCTION init()

LET  g_siremp              =          0
LET  g_noremp              =          0
LET  g_noectllave          =          0
LET  g_nuevos              =          0

INITIALIZE  g_reg.*  TO  NULL

CALL act_ests()

LET    g_icefa_cod                 =                    178
LET    g_usuario                   =                    NULL
LET    g_status_afi                =                    2 

SELECT USER
INTO   g_usuario
FROM   safre_af:seg_modulo
WHERE  modulo_cod                  =                     "tra"

SELECT codigo_afore
  INTO gc_codigo_afore
  FROM tab_afore_local
   

   WHENEVER ERROR CONTINUE

   DATABASE safre_tmp 
      
      DELETE FROM safre_tmp:tmp_afi_dup   
      DELETE FROM safre_tmp:tmp_ctr_dup   
      DELETE FROM safre_tmp:tmp_afi_maee
     
      DROP TABLE safre_tmp:tmp_afi_dup
      DROP TABLE safre_tmp:tmp_ctr_dup
      DROP TABLE safre_tmp:tmp_afi_maee

     
      CREATE TABLE safre_tmp:tmp_afi_dup( curp CHAR(18)   ,
                                          nss  CHAR(11)   ,
                                          cuant INTEGER   )

      CREATE TABLE safre_tmp:tmp_ctr_dup( curp CHAR(18)   ,
                                          nss  CHAR(11)   ,
                                          cuant INTEGER   )

      CREATE TABLE safre_tmp:tmp_afi_maee( n_folio      DECIMAL(10,0)   ,
                                          n_seguro     CHAR(11)        ,
                                          tipo_sol     SMALLINT        ,
                                          n_unico      CHAR(18)        ,
                                          status       SMALLINT       )
           
     
   WHENEVER ERROR STOP

   CALL  f_prepa_1()

   CALL  f_afiliado_pre()

   DATABASE safre_af 

END FUNCTION

FUNCTION f_prepa() 
    DEFINE   lc_query                CHAR(2000)

    LET      lc_query   = NULL

 -- Selecciona todos los registros cargados por folio de tra_issste_unifica 
    LET      lc_query  = "  SELECT B.*                            ",
                         "    FROM safre_af:tra_issste_unifica B, ",
                         "         safre_tmp:tmp_afi_maee      T  ",
                         "   WHERE  B.folio    =    ?             ",
                         "   AND    B.curp     =    T.n_unico     "
    PREPARE p_SelUni               FROM  lc_query
    DECLARE d_SelUni          CURSOR FOR  p_SelUni

 
 -- Busca informacion del nss en tmp_afi_maee  (tabla temporal)
    LET      lc_query  = "  SELECT g.n_folio                     ,",
                         "         g.n_seguro                    ,",
                         "         g.tipo_sol                    ,",
                         "         g.status                       ",
                         "    FROM safre_tmp:tmp_afi_maee g       ",
                         "   WHERE g.n_unico   =    ?             "
                         
    PREPARE p_SelAfi                FROM  lc_query
    DECLARE d_SelAfi               CURSOR FOR  p_SelAfi       #627
    	
           LET      lc_query   = "  SELECT min(g.fecha_proceso)           ",
                                 "    FROM safre_af:cta_ctr_reg_ind  g    ",
                                 "   WHERE g.nti       =    ?             "
           PREPARE p_SelCtrF               FROM  lc_query

    IF gc_codigo_afore = 564 THEN ---- METLIFE 
        -- Busca informacion del nss en cta_ctr_reg_ind, afo 
           LET      lc_query  = "  SELECT g.tipo_trab_afo                ",
                                "    FROM safre_af:cta_ctr_reg_ind  g    ",
                                "   WHERE g.nti             =  ?         ", 
                                "   AND   g.fecha_proceso   =  ?         " 
           PREPARE p_SelCtrA               FROM  lc_query
    ELSE
        -- Busca informacion del nss en cta_ctr_reg_ind, ind 
           LET      lc_query  = "  SELECT g.tipo_trab_ind                ",
                                "    FROM safre_af:cta_ctr_reg_ind  g    ",
                                "   WHERE g.nti             =  ?         ", 
                                "   AND   g.fecha_proceso   =  ?         " 
           PREPARE p_SelCtrI               FROM  lc_query
    END IF

 -- Busca maximo folio asignado en tra_mae_icefa_issste
    LET      lc_query  = "  SELECT MAX(n_folio_tra) + 1                  ",
                         "    FROM safre_af:tra_mae_icefa_issste         ",
                         "  WHERE n_folio_tra[1]  NOT MATCHES  '[A-Z]'   ", 
                         "    AND n_folio_tra[2]  NOT MATCHES  '[A-Z]'   ",
                         "    AND n_folio_tra[3]  NOT MATCHES  '[A-Z]'   ",
                         "    AND n_folio_tra[4]  NOT MATCHES  '[A-Z]'   ",
                         "    AND n_folio_tra[5]  NOT MATCHES  '[A-Z]'   ",
                         "    AND n_folio_tra[6]  NOT MATCHES  '[A-Z]'   ",
                         "    AND n_folio_tra[7]  NOT MATCHES  '[A-Z]'   ",
                         "    AND n_folio_tra[8]  NOT MATCHES  '[A-Z]'   ",
                         "    AND n_folio_tra     NOT MATCHES  '|'   "


    PREPARE p_SelIce                FROM  lc_query

  -- Busca en tra_mae_icefa_issste el status de la solicitud 

     LET     lc_query   =  " SELECT    a.status                ",
                           " FROM      tra_mae_icefa_issste a  ",   
                           " WHERE     a.n_seguro    =  ?      ",  --ACTIVO
                           " AND       a.nss         =  ?      ",  --INACTIVO
                           " AND       a.rfc         =  ?      ",  --INACTIVO
                           " AND       a.icefa_cod   =  ?      ",  -- 178 
                           " AND       a.id_procesar =  ?      "   --INACTIVO

    PREPARE p_SelSta                FROM  lc_query

 -- Inserta registro nuevo en tra_mae_icefa_issste
    LET      lc_query  = "  INSERT  INTO safre_af:tra_mae_icefa_issste       ",
             " VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE p_InsIce                FROM  lc_query

 -- Actualiza la f_actualiza con la fecha del día en tra_issste_unifica
   LET       lc_query  = "  UPDATE  safre_af:tra_issste_unifica   ",
                         "     SET  f_actualiza  =   ?            ",
                         "   WHERE  folio        =   ?            "
    PREPARE p_UpdUni                FROM  lc_query


-- Actualiza status_mae_ice con el status de tra_mae_icefa_isssste

   LET       lc_query  = "  INSERT INTO safre_af:tra_ctr_unisar            ", 
                         "  VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    PREPARE p_InsCrt                FROM  lc_query

-- Borra registro de tra_mae_icefa_issste 
   LET       lc_query  = " DELETE  tra_mae_icefa_issste      ", 
                         " WHERE   n_seguro        =  ?      ",  --ACTIVO
                         " AND     nss             =  ?      ",  --INACTIVO
                         " AND     rfc             =  ?      ",  --INACTIVO
                         " AND     icefa_cod       =  ?      ",  -- 178 
                         " AND     id_procesar     =  ?      "   --INACTIVO

    PREPARE p_DelTra                FROM  lc_query

END FUNCTION 
#-----fc
FUNCTION f_actualiza(li_folio)

DEFINE  reg_a                 RECORD LIKE safre_af:afi_mae_afiliado.*
DEFINE  l_n_folio             LIKE safre_af:afi_mae_afiliado.n_folio
DEFINE  l_tipo_solicitud      LIKE safre_af:afi_mae_afiliado.tipo_solicitud
DEFINE  l_n_seguro            LIKE safre_af:afi_mae_afiliado.n_seguro
DEFINE  l_origen_traspaso     LIKE safre_af:tra_mae_icefa_issste.origen_traspaso
DEFINE  l_paterno             ,
        l_materno             ,
        l_nombre              CHAR(040)
DEFINE  l_n_folio_tra         LIKE safre_af:tra_mae_icefa_issste.n_folio_tra
DEFINE  li_folio                     ,
        li_leidos                    ,
        li_tratados                  ,
        li_noe_afi             INTEGER

DEFINE  l_cve_sector                  ,
        l_status                      ,
        l_ctos_mae                    , 
        l_tipo_trab_ind       SMALLINT,
        l_fecha_proceso       DATE

DEFINE  lc_null               CHAR(005)
DEFINE  ln_cero                       ,
        ln_cuatro             INTEGER  
DEFINE  ld_fecha              DATE

   LET  l_status                          =              NULL
   LET  l_n_folio                         =              NULL
   LET  l_tipo_solicitud                  =              NULL
   LET  l_n_seguro                        =              NULL
   LET  l_origen_traspaso                 =              NULL
   LET  li_leidos                         =              0
   LET  li_tratados                       =              0
   LET  li_noe_afi                        =              0

   INITIALIZE  g_reg.*  TO  NULL

   LET  lc_null                           =              ""
   LET  ln_cero                           =              0
   LET  ln_cuatro                         =              4
   LET  ld_fecha                          =              TODAY

   CALL  f_prepa()


   LET g_siremp       =      0
   LET g_noremp       =      0
   LET g_noectllave   =      0
   LET g_nuevos       =      0


   FOREACH d_SelUni           USING  li_folio 
                               INTO  g_reg.*

        LET  l_n_seguro                        =              NULL
         
        LET   li_leidos   =  li_leidos + 1
        --EXECUTE p_SelAfi              USING   g_reg.curp     #627
        FOREACH d_SelAfi              USING   g_reg.curp       #627
                                      INTO    l_n_folio              ,
                                              l_n_seguro             ,
                                              l_tipo_solicitud       ,
                                              l_status           

        LET g_status_mae_ice  =    0        

        EXECUTE p_SelSta              USING   l_n_seguro,
                                              g_reg.nss_icefa,
                                              g_reg.rfc_icefa,
                                              g_icefa_cod,
                                              g_reg.id_procesar   
                                      INTO    g_status_mae_ice 

        IF (  g_status_mae_ice   =   0     OR     -- No Existe ( Registro Nuevo)
              g_status_mae_ice   =   1     OR     -- Confirmada 
              g_status_mae_ice   =   20    OR     -- Capturadas  
              g_status_mae_ice   =   21    OR     -- Rechazados por Convivencia
              g_status_mae_ice   =   77  ) THEN   -- Cargados por Archivo Layout
                                                  -- 098301 BDNSAR Cuentas Inact

           LET l_fecha_proceso              =          NULL
           LET l_tipo_trab_ind              =          NULL
           LET l_cve_sector                 =          NULL

           IF g_status_mae_ice = 0 THEN
              LET g_nuevos          =   g_nuevos + 1
           ELSE 
              LET g_siremp          =   g_siremp + 1 
           END IF  

                
          EXECUTE p_SelCtrF             USING   l_n_seguro
                                        INTO    l_fecha_proceso

           CASE  gc_codigo_afore
 
              WHEN 564  # SOLO AFORE METLIFE

                 IF ( l_tipo_solicitud = 8 OR l_tipo_solicitud = 12 ) THEN

                    LET l_origen_traspaso       =       63  #TRAB IND.
                    LET l_cve_sector            =       2   #Sector PRIVADO

                 ELSE # <> 8
   
                    LET l_origen_traspaso        =      61
                    LET l_cve_sector             =      1  #Sector PUBLICO
   
                 END IF
   
   
              OTHERWISE # DEMAS AFORES
   
                 IF ( l_tipo_solicitud = 8 OR l_tipo_solicitud = 12 ) THEN
   
                    LET l_origen_traspaso       =       63  #TRAB IND.
                    LET l_cve_sector            =       2   #Sector PRIVADO

                 ELSE # <> 8
   
                    LET l_origen_traspaso        =      61
                    LET l_cve_sector             =      1  #Sector PUBLICO
   
                 END IF


           END CASE
   
            LET  l_paterno               =              NULL
            LET  l_materno               =              NULL
            LET  l_nombre                =              NULL
   
            LET  l_paterno               =  g_reg.nombre_completo[01,40]
            LET  l_materno               =  g_reg.nombre_completo[41,80]
            LET  l_nombre                =  g_reg.nombre_completo[81,120]
   
            LET  l_n_folio_tra           =               NULL

            EXECUTE p_SelIce                INTO   l_n_folio_tra

            IF ( l_n_folio_tra = 0  OR  l_n_folio_tra  IS NULL  ) THEN

               LET l_n_folio_tra         =               1

            END IF

            IF (l_n_seguro = " "  OR l_n_seguro = ""   OR 
                l_n_seguro IS NULL ) THEN
    
                LET l_n_seguro = " " 

            END IF 


            IF (g_reg.nss_icefa = " "  OR g_reg.nss_icefa = ""   OR 
                g_reg.nss_icefa IS NULL ) THEN
    
                LET g_reg.nss_icefa = " " 

            END IF 


            IF (g_reg.rfc_icefa = " "  OR g_reg.rfc_icefa = ""   OR
                g_reg.rfc_icefa IS NULL ) THEN

                LET g_reg.rfc_icefa = " " 

            END IF

           #Borra  Registro de TRA MAE ICEFA ISSSTE
            EXECUTE p_DelTra              USING   l_n_seguro,
                                                  g_reg.nss_icefa,
                                                  g_reg.rfc_icefa,
                                                  g_reg.icefa,
                                                  g_reg.id_procesar   

            CASE  gc_codigo_afore 

               WHEN 564  # SOLO AFORE METLIFE

                  LET   l_ctos_mae               =             0

                     SELECT    COUNT(*)
                     INTO      l_ctos_mae
                     FROM      tra_mae_icefa_issste a  
                     WHERE     a.nss            =  g_reg.nss_icefa
                     AND       a.rfc            =  g_reg.rfc_icefa
                     AND       a.icefa_cod      =  g_reg.icefa
                     AND       a.id_procesar    =  g_reg.id_procesar

                     IF ( l_ctos_mae >=  1 ) THEN # Encontrado por campo llave
            
                        LET      g_noectllave      = g_noectllave + 1 

                        EXECUTE  p_InsCrt                      USING  
                                 g_reg.nss                     ,#nss 
                                 g_reg.curp                    ,#curp
                                 g_reg.rfc                     ,#rfc
                                 g_reg.paterno                 ,#paterno
                                 g_reg.materno                 ,#materno
                                 g_reg.nombre                  ,#nombre
                                 g_reg.f_nacimiento            ,#f_nacimiento
                                 g_reg.sexo                    ,#sexo
                                 g_reg.ent_nacimiento          ,#ent_nacimiento
                                 g_reg.afore                   ,#afore
                                 g_status_afi                  ,#tatus_afi 
                                 g_status_mae_ice              ,#status_mae_ice
                                 ld_fecha                      ,#f_carga
                                 g_usuario                      #usuario 

                     ELSE #registro no encontrado por campo llave se puede
                          #insertar sin problemas

                        LET    l_status     =   1 # Se pone asi para que los
                                                  # tome la operacion 01 se 
                                                  # sustituye por el 77.

                        EXECUTE p_InsIce             USING  
                                l_n_folio            ,#n_folio          --AFI
                                l_tipo_solicitud     ,#tipo_solicitud   --AFI
                                l_n_seguro           ,#n_seguro         --AFI
                                g_reg.nss_icefa      ,#nss              --ICE
                                g_reg.rfc_icefa      ,#rfc              --ICE
                                l_paterno            ,#paterno          --ICE
                                l_materno            ,#materno          --ICE
                                l_nombre             ,#nombres          --ICE
                                l_n_folio_tra        ,#n_folio_tra
                                g_reg.icefa          ,#icefa_cod        --ICE
                                g_reg.id_procesar    ,#id_procesar      --ICE
                                g_reg.nti            ,#nro_int_cta      --ICE
                                ld_fecha             ,#fecha_solic_tra
                                lc_null              ,#fecha_comp_icefa
                                ln_cero              ,#saldo_sar_92
                                ln_cero              ,#saldo_viv_92
                                l_origen_traspaso    ,#origen_traspaso  --ICE
                                ld_fecha             ,#fecha_captura
                                ld_fecha             ,#fecha_proceso
                                lc_null              ,#lote_genera
                                lc_null              ,#fecha_genera
                                l_status             ,#status
                                ln_cuatro            ,#fuente
                                ln_cero              ,#correlativo
                                g_usuario            ,#usuario
                                ln_cero              ,#n_envios
                                lc_null              ,#diagnostico
                                l_cve_sector          #cve_sector -Sector Publi
                     END IF
 
 

               OTHERWISE # DEMAS AFORES

                  LET    l_status     =   1 # Se pone asi para que los
                                            # tome la operacion 01 se 
                                            # sustituye por el 77.

                  EXECUTE p_InsIce             USING  
                          l_n_folio            ,#n_folio          --AFI
                          l_tipo_solicitud     ,#tipo_solicitud   --AFI
                          l_n_seguro           ,#n_seguro         --AFI
                          g_reg.nss_icefa      ,#nss              --ICE
                          g_reg.rfc_icefa      ,#rfc              --ICE
                          l_paterno            ,#paterno          --ICE
                          l_materno            ,#materno          --ICE
                          l_nombre             ,#nombres          --ICE
                          l_n_folio_tra        ,#n_folio_tra
                          g_reg.icefa          ,#icefa_cod        --ICE
                          g_reg.id_procesar    ,#id_procesar      --ICE
                          g_reg.nti            ,#nro_int_cta      --ICE
                          ld_fecha             ,#fecha_solic_tra
                          lc_null              ,#fecha_comp_icefa
                          ln_cero              ,#saldo_sar_92
                          ln_cero              ,#saldo_viv_92
                          l_origen_traspaso    ,#origen_traspaso  --ICE
                          ld_fecha             ,#fecha_captura
                          ld_fecha             ,#fecha_proceso
                          lc_null              ,#lote_genera
                          lc_null              ,#fecha_genera
                          l_status             ,#status
                          ln_cuatro            ,#fuente
                          ln_cero              ,#correlativo
                          g_usuario            ,#usuario
                          ln_cero              ,#n_envios
                          lc_null              ,#diagnostico
                          l_cve_sector          #cve_sector -Sector Publi

            END  CASE  

        ELSE   #Status <> "A"   0,1,20,21

            LET      g_noremp      = g_noremp + 1 

            EXECUTE  p_InsCrt                      USING  
                     g_reg.nss                     ,#nss 
                     g_reg.curp                    ,#curp
                     g_reg.rfc                     ,#rfc
                     g_reg.paterno                 ,#paterno
                     g_reg.materno                 ,#materno
                     g_reg.nombre                  ,#nombre
                     g_reg.f_nacimiento            ,#f_nacimiento
                     g_reg.sexo                    ,#sexo
                     g_reg.ent_nacimiento          ,#ent_nacimiento
                     g_reg.afore                   ,#afore
                     g_status_afi                  ,#tatus_afi 
                     g_status_mae_ice              ,#status_mae_ice
                     ld_fecha                      ,#f_carga
                     g_usuario                      #usuario 
                                           
      END IF
    END FOREACH     #d_SelAfi                                             #627
   END FOREACH      #d_SelUni

   EXECUTE  p_UpdUni                        USING  ld_fecha,
                                                   li_folio

   LET li_tratados             =   SQLCA.SQLERRD[3]


   DISPLAY "TOT.  REGS. LEIDOS PARA ACTUALIZAR                     : ", li_tratados  AT 7,8 ATTRIBUTE (REVERSE)


   DISPLAY "REGS. ENCONTRADOS EN AFILIACION                        : ", li_leidos    AT 9,8



   LET    li_noe_afi        = li_tratados  -  li_leidos        
   
   DISPLAY "REGS. < NO > ENCONTRADOS EN AFILIACION                 : ", li_noe_afi  AT 10,8

   DISPLAY "DETALLE DE LOS REGISTROS ENCONTRADOS EN AFILIACION       "  AT 12,8 ATTRIBUTE (REVERSE)

   DISPLAY " REGS. NUEVOS  INGRESADOS  EN B.D. UNI-SAR-ISS         : ", g_nuevos     AT 14,8 

   DISPLAY " REGS. EXIST. EN B.D. UNI-SAR-ISS REEMPLAZADOS         : ", g_siremp     AT 15,8 

   DISPLAY " REGS. EXIST. EN B.D. UNI-SAR-ISS < NO > REEMPLAZADOS  : ", g_noremp     AT 16,8 
   DISPLAY " REGS. EXIST. EN  MAEICE  CAMPO LLAVE                  : ", g_noectllave  AT 17,8 

   DISPLAY "                                                            "  AT 19,8

END FUNCTION
--fp1
FUNCTION f_prepa_1() 
    DEFINE   lc_query                CHAR(2000)

    LET      lc_query   = NULL

 -- Selecciona todos los registros cargados en safre_tmp:tmp_afi_dup

    LET      lc_query  = "  SELECT a.curp,a.nss                   ",
                         "    FROM safre_tmp:tmp_afi_dup   a       "

    PREPARE p_SelAD               FROM  lc_query
    DECLARE d_SelAD               CURSOR FOR p_SelAD 

 -- Selecciona todos los registros cargados en safre_tmp:tmp_afi_dup

    LET      lc_query  = "  SELECT a.curp,a.nss                   ",
                         "    FROM safre_tmp:tmp_ctr_dup   a       "

    PREPARE p_SelCtrDup               FROM  lc_query
    DECLARE d_SelCtrDup              CURSOR FOR p_SelCtrDup 

 

 -- Busca informacion del safre_af:afi_mae_afiliado 
    LET      lc_query  = "  SELECT b.n_folio                     ,",
                         "         b.tipo_solicitud               ",
                         "    FROM safre_af:afi_mae_afiliado b    ",
                         "   WHERE b.n_unico   =    ?             ",
                         "     AND b.n_seguro  =    ?             "
    PREPARE p_SelAfiMae       FROM  lc_query

 -- Obtiene la sumatoria del monto en acciones de dis_cuenta 
    LET      lc_query  = "  SELECT SUM(b.monto_en_acciones)       ",
                         "    FROM safre_af:dis_cuenta b          ",
                         "   WHERE b.nss       =    ?             "
    PREPARE p_SelDisCta       FROM  lc_query



 -- Busca informacion del nss en safre_af:afi_mae_afiliado   
    LET      lc_query  = "  SELECT b.n_seguro                    ",
                         "    FROM safre_af:afi_mae_afiliado b   ",
                         "   WHERE b.n_unico   =    ?            "
    PREPARE p_SelAfiMae1       FROM  lc_query
    DECLARE d_SelAfiMae1           CURSOR FOR p_SelAfiMae1 



 -- Inserta registro nuevo en tra_mae_icefa_issste
    LET      lc_query  = "  INSERT  INTO safre_tmp:tmp_afi_maee     ",
             " VALUES(?,?,?,?,?) "
    PREPARE p_InstmpAfi                FROM  lc_query

-- Borra registro de safre_af:tra_ctr_unisar 
   LET       lc_query  = " DELETE  safre_af:tra_ctr_unisar   ",
                         " WHERE   nss         =  ?      ",  
                         " AND     curp        =  ?      ",  
                         " AND     paterno     =  ?      ",  
                         " AND     materno     =  ?      ",  
                         " AND     nombre      =  ?      ",  
                         " AND     f_carga     =  ?      "  

    PREPARE p_DelTraCtr                FROM  lc_query

-- Borra registro de safre_af:tra_ctr_unisar 
   LET       lc_query  = " DELETE  safre_af:tra_ctr_unisar   ",
                         " WHERE   nss         =  ?      ",  
                         " AND     curp        =  ?      "  

    PREPARE p_DelTraCtr1                FROM  lc_query




--------------------------------------------------------------------
END FUNCTION
#-----fa
FUNCTION f_afiliado_pre()


   DISPLAY " PROCESANDO INFORMACION ESPERE POR FAVOR .. " AT 19,8 ATTRIBUTE     (REVERSE)

-- Inserta Duplicados en tmp_afi_dup (tabla temporal)

    INSERT  INTO safre_tmp:tmp_afi_dup        
    SELECT  A.curp , 
            A.nss  ,
            COUNT(*)     
    FROM    safre_af:tra_issste_unifica A,    
            safre_af:afi_mae_afiliado B               
    WHERE   A.curp             = B.n_unico            
    AND     A.paterno          = B.paterno            
    AND     A.materno          = B.materno            
    AND     A.nombre           = B.nombres            
    AND     A.rfc              = B.n_rfc         
    AND     A.f_nacimiento     = B.fena
    GROUP by 1,2                              
    HAVING COUNT(*) > 1                      
    
-- Inserta Regs Unicos de los duplicados en tmp_afi_maee (tabla temporal)


      FOREACH d_SelAD      INTO  g_reg5.*     

               LET   g_fol                =            NULL
               LET   g_tipo_sol           =            NULL

        EXECUTE p_SelAfiMae           USING   g_reg5.curp,
                                              g_reg5.nss  
                                      INTO    g_fol                  ,
                                              g_tipo_sol  


            IF STATUS <> NOTFOUND THEN

               LET g_status               =            1 

               EXECUTE p_InstmpAfi                  USING
                                                    g_fol,                  
                                                    g_reg5.nss     ,
                                                    g_tipo_sol    ,
                                                    g_reg5.curp    ,
                                                    g_status  
            ELSE

               DECLARE z CURSOR FOR 

                  SELECT a.n_seguro
                    FROM safre_af:afi_mae_afiliado a
                   WHERE a.n_unico            =           g_reg5.curp
                           
                   FOREACH  z INTO g_reg2.*
                       
                      LET   g_mont_acc   = 0

                      SELECT SUM(b.monto_en_acciones)
                        INTO g_mont_acc
                        FROM safre_af:dis_cuenta b
                       WHERE b.nss       =    g_reg2.nss

                      IF ( g_mont_acc  > 0 )      THEN                           

                         LET g_status               =            1 

                         EXECUTE p_InstmpAfi                  USING
                                                              g_fol,
                                                              g_reg2.nss     ,
                                                              g_tipo_sol    ,
                                                              g_reg5.curp    ,
                                                              g_status  

                         EXIT FOREACH 

                      END IF

                   END FOREACH

            END IF              

      END FOREACH

-- Inserta < NO > duplicados en tmp_afi_maee (tabla temporal)

   INSERT  INTO safre_tmp:tmp_afi_maee 
   SELECT  a.n_folio,a.n_seguro,a.tipo_solicitud,a.n_unico,1
   FROM    safre_af:afi_mae_afiliado   a,            
           safre_af:tra_issste_unifica t             
   WHERE   a.nombres        =          t.nombre                      
   AND     a.paterno        =          t.paterno                      
   AND     a.materno        =          t.materno                      
   AND     a.n_unico        =          t.curp                         
   AND     a.n_rfc          =          t.rfc             
   AND     a.fena           =          t.f_nacimiento 
   AND     t.curp NOT IN ( SELECT d.curp            
                            FROM   safre_tmp:tmp_afi_dup  d )   

-- Inserta los registros < NO > encontrados por Curp,paterno,materno y
-- nombre en afi_mae_afiliado 
  
   INSERT  INTO safre_af:tra_ctr_unisar
   SELECT     t.nss ,
              t.curp ,
              t.rfc  ,
              t.paterno ,
              t.materno ,
              t.nombre ,
              t.f_nacimiento ,
              t.sexo ,
              t.ent_nacimiento ,
              t.afore ,
              1,
              ' ',
              t.f_carga ,
              t.usuario 
   FROM       safre_af:tra_issste_unifica T 
   WHERE      NOT EXISTS ( SELECT 1
                           FROM safre_tmp:tmp_afi_maee A 
                           WHERE A.n_unico    =      T.curp     )

-- Inserta los registros duplicados encontrados por Curp de  la tabla     
-- tra_ctr_unisar ( Tabla que se lleno con los regs que no coincidieron
-- por la llave principal es decir Curp,Paterno,Materno y Nombre ) .

   INSERT INTO safre_tmp:tmp_ctr_dup
   SELECT  A.curp  ,
           A.nss   ,
           COUNT(*)
   FROM    safre_af:tra_ctr_unisar A,
           safre_af:afi_mae_afiliado B
   WHERE   A.curp     = B.n_unico
   GROUP BY 1,2
   HAVING COUNT(*) > 1

   #----- Codigo para depurar los registros  que no estan en afi_mae_afiliado
   #      por n_unico,paterno,materno,nombres ,pero si deben estar 
   #      por n_unico por lo menos  y coincidir en paterno o materno o nombre.
   #      Tabla   == > safre_af  tra_ctr_unisar
  
   INITIALIZE  g_reg1.*  TO NULL

   LET         g_validacion         =        NULL
   LET         g_validacion         =        "0000"

   DECLARE  f  CURSOR FOR 

      SELECT   t.nss                  ,
               t.paterno              ,
               t.materno              ,
               t.nombre               ,
               a.n_unico              ,
               a.n_seguro             ,
               a.n_folio              ,
               a.tipo_solicitud       ,
               a.paterno              ,
               a.materno              ,
               a.nombres              ,
               t.f_carga              
       FROM    safre_af:tra_ctr_unisar      t  ,
               safre_af:afi_mae_afiliado    a
      WHERE    t.curp              =            a.n_unico
        AND    a.n_unico NOT IN  ( SELECT  r.curp
                                   FROM safre_tmp:tmp_ctr_dup r )
   FOREACH  f  INTO g_reg1.* 

      LET   g_validacion[1,4]         =         "1000"
 
      IF ( g_reg1.pat_ctr     =      g_reg1.pat_afi  )   THEN
            
         LET  g_validacion[2]    =         "1"

      END IF           


      IF ( g_reg1.mat_ctr     =      g_reg1.mat_afi  )   THEN
            
         LET  g_validacion[3]    =         "1"

      END IF           


      IF ( g_reg1.nom_ctr     =      g_reg1.nom_afi  )   THEN
          
         LET  g_validacion[4]    =         "1"

      END IF           


      #--- Verifica  g_validacion para ver si se procede a insertar en 
      #--- la tabla  safre_tmp:tmp_afi_maee


      IF  ( g_validacion = "1011" OR
            g_validacion = "1101" OR 
            g_validacion = "1110" OR 
            g_validacion = "1100" OR 
            g_validacion = "1001" OR 
            g_validacion = "1010" ) THEN 


      #---Inserta registro en safre_tmp:tmp_afi_maee ---#
            --LET g_status               =            20 
            LET g_status               =            1 

            EXECUTE p_InstmpAfi                  USING
                                                 g_reg1.fol_afi  ,
                                                 g_reg1.nss_ctr  ,
                                                 g_reg1.sol_afi  ,
                                                 g_reg1.curp_afi ,
                                                 g_status 



      #-- Borra registro en tabla tra_ctr_unisar.
      #-- Nota: Este registro fue el que previamente se inserto en 
      #--       La tabla safre_tmp:tra_ctr_unisar


            EXECUTE p_DelTraCtr           USING   g_reg1.nss_ctr,
                                                  g_reg1.curp_afi, 
                                                  g_reg1.pat_ctr, 
                                                  g_reg1.mat_ctr, 
                                                  g_reg1.nom_ctr, 
                                                  g_reg1.fcarga_ctr


      END IF
 
    
   END FOREACH

   -- Leemos la  tabla safre_tmp:tmp_ctr_dup la cual contiene
   -- los registros duplicados encontrados por Curp de  la tabla     
   -- tra_ctr_unisar ( Tabla que se lleno con los regs que no coincidieron
   -- por la llave principal es decir Curp,Paterno,Materno y Nombre ) .

     
      FOREACH d_SelCtrDup     INTO  g_reg3.*

               LET   g_fol3               =            NULL
               LET   g_tipo_sol3          =            NULL

        EXECUTE p_SelAfiMae           USING   g_reg3.curp,
                                              g_reg3.nss
                                      INTO    g_fol                  ,
                                              g_tipo_sol


            IF STATUS <> NOTFOUND THEN
               LET g_status               =            1 

               EXECUTE p_InstmpAfi                  USING
                                                    g_fol3,
                                                    g_reg3.nss     ,
                                                    g_tipo_sol3    ,
                                                    g_reg3.curp    ,
                                                    g_status  

               EXECUTE p_DelTraCtr1           USING   g_reg3.nss,
                                                      g_reg3.curp


               EXIT FOREACH 

            ELSE

                   FOREACH d_SelAfiMae1   USING g_reg3.curp
                                           INTO g_reg4.*

                       
                      LET   g_mont_acc4  = 0

                   EXECUTE p_SelDisCta         USING   g_reg4.nss
                                                INTO   g_mont_acc4              


                      IF (  g_mont_acc4  > 0  )      THEN                           
                         #---Inserta registro en safre_tmp:tmp_afi_maee ---#

                         LET g_status               =            1 

                         EXECUTE p_InstmpAfi           USING
                                                       g_fol3          ,
                                                       g_reg4.nss      ,
                                                       g_tipo_sol3     ,
                                                       g_reg3.curp     ,
                                                       g_status  

                         EXIT FOREACH 

                      END IF

                   END FOREACH

                END IF              

         END FOREACH 

   #----- Fin de la depuracion 

    CREATE INDEX tmp_afi_dup_1 ON tmp_afi_dup (curp,nss)

    CREATE INDEX tmp_ctr_dup_1 ON tmp_ctr_dup (curp,nss)

    CREATE INDEX tmp_afi_maee_1 ON tmp_afi_maee (n_folio,n_seguro,tipo_sol)

    UPDATE STATISTICS FOR TABLE tmp_afi_maee 

    LET    g_cuan_no  = 0  

--Obtiene el total de registros no encontro en afi_mae_afiliados

    SELECT  count(*),"not_existe_en_mae"
      INTO  g_cuan_no 
      FROM  safre_af:tra_issste_unifica T 
     WHERE  NOT EXISTS ( SELECT 1
                    FROM safre_tmp:tmp_afi_maee A 
                   WHERE A.n_unico = T.curp )

-- Para cuando n_seguro de tra_mae_icefa_issste es blanco  

    UPDATE  safre_af:tra_mae_icefa_issste 
       SET  n_seguro = " " 
     WHERE  fuente   = 4  
       AND  n_seguro IS NULL

-- Para cuando n_seguro de tra_mae_icefa_issste es blanco  

    UPDATE  safre_af:tra_mae_icefa_issste 
       SET  nss      = " " 
     WHERE  fuente   = 4  
       AND  nss      IS NULL

-- Para cuando n_seguro de tra_mae_icefa_issste es blanco  

    UPDATE  safre_af:tra_mae_icefa_issste 
       SET  rfc      = " "
     WHERE  fuente   = 4  
       AND  rfc      IS NULL

END FUNCTION

FUNCTION act_ests()

UPDATE STATISTICS FOR TABLE tra_mae_icefa_issste 

END FUNCTION

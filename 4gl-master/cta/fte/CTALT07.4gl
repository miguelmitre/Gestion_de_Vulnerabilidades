-----------------------------------------
--Programa : CTALT07 
--Descripcion : Liquida TES X EDAD
--Autor    : JESUS DAVID YAÑEZ MORENO
--Fecha    : 01-09-2010   
-----------------------------------------

DATABASE safre_af

GLOBALS

    DEFINE g_reg_tes_tipo_id RECORD LIKE tes_tipo_id_aportante.*

    DEFINE g_ruta_log            CHAR(200)
  
    DEFINE reg_tes_ctr_folio  RECORD LIKE tes_ctr_folio.*

    DEFINE reg_aboctas           RECORD LIKE dis_provision.*

    DEFINE s_codigo_afore        LIKE tab_afore_local.codigo_afore
    DEFINE gprecio_acc           LIKE safre_af:glo_valor_accion.precio_del_dia
    DEFINE gprecio_parti         LIKE safre_af:glo_valor_accion.precio_del_dia

    DEFINE
        xcodigo_marca           ,
        xcodigo_rechazo         ,  
        g_provisionada          ,  
        g_liquidada             ,  
        g_cero                  SMALLINT

    DEFINE
        contar                  ,
        g_procesados            INTEGER

    DEFINE
        g_fecha_liquidacion ,
        vfecha              DATE

    DEFINE
        g_nom_tab_tes_solicitud   CHAR(040),  
        g_enter             CHAR(001),
        hora                CHAR(005),
        desc_tipo           CHAR(023),
        g_lista             CHAR(100),
        comando             CHAR(100),
        ejecuta             CHAR(300),
        g_txt               CHAR(1000),
        G_IMPRE_TM          CHAR(300),
        G_IMPRE_SUB         CHAR(300)

    DEFINE registro RECORD
        siefore             LIKE dis_cuenta.siefore, 
        subcuenta           LIKE dis_cuenta.subcuenta,
        monto_en_acciones   LIKE dis_cuenta.monto_en_acciones,
        monto_en_pesos      LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE fecha_valuacion  DATE

    DEFINE g_pdq CHAR(100)

END GLOBALS

GLOBALS "GLOB_REPS.4gl"

MAIN


    LET reg_tes_ctr_folio.folio = ARG_VAL(1)
    LET fecha_valuacion         = ARG_VAL(2)

    DISPLAY "CTALT07: INICIA LIQUIDACION FOLIO ",
            reg_tes_ctr_folio.folio  USING  "#######"

    CALL inicio()            #i

    LET g_ruta_log = g_seg_modulo.ruta_envio CLIPPED,"/CTALT07.log"  

    CALL STARTLOG(g_ruta_log)

    CALL proceso_principal() #pp


END MAIN

FUNCTION inicio()
#i---------------

    LET hoy            = TODAY
    LET hora           = TIME
    LET g_cero         = 0

    SELECT  codigo_afore    ,    
            user
      INTO  s_codigo_afore  , 
            g_usuario
      FROM  tab_afore_local

    SELECT  *
      INTO  g_seg_modulo.*
      FROM  seg_modulo
     WHERE  modulo_cod = 'cta' -- se modifica por liq x edad

    SELECT  a.estado
      INTO  g_provisionada
      FROM  safre_af:taa_cd_edo_cedente  a
     WHERE  a.descripcion    = 'PROVISIONADA'
       AND  a.tipo           = 3;

    SELECT  a.estado
      INTO  g_liquidada
      FROM  safre_af:taa_cd_edo_cedente  a
     WHERE  a.descripcion    = 'LIQUIDADA'
       AND  a.tipo           = 3;

       INSERT INTO safre_af:tes_ctr_folio 
       VALUES (reg_tes_ctr_folio.folio,
               13,
               fecha_valuacion ,
               fecha_valuacion ,
               fecha_valuacion ,
               102             ,
               g_usuario)

       UPDATE  tes_solicitud
       set     folio           = reg_tes_ctr_folio.folio, 
               estado          = 102 ,
               fecha_traspaso  = fecha_valuacion
       WHERE   tipo_traspaso = 13
       AND     estado        = 100

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    DEFINE    l_registros         INTEGER
    DEFINE    l_texto_query       CHAR(500)

    DISPLAY "CTALT07: ",
       hoy USING "DD-MM-YYYY" 

    SELECT  a.*     
    INTO   reg_tes_ctr_folio.*
      FROM  tes_ctr_folio a
     WHERE  a.folio                   =  reg_tes_ctr_folio.folio
       AND  a.estado                  =  g_provisionada

    IF STATUS = NOTFOUND    THEN
       DISPLAY "CTALT07: Program stopped"
       DISPLAY "CTALT07: ERROR NO HAY FOLIO PENDIENTE DE LIQUIDAR "
    END IF

    SELECT a.*
    INTO   g_reg_tes_tipo_id.*
    FROM   tes_tipo_id_aportante a
    WHERE  a.tipo_traspaso = reg_tes_ctr_folio.tipo_traspaso

    LET   g_nom_tab_tes_solicitud      =  "tes_solicitud"

    LET   l_texto_query      = 
          'SELECT  COUNT(*) ',
          'FROM   ', g_nom_tab_tes_solicitud CLIPPED ,' ',
          'WHERE  folio   = ',reg_tes_ctr_folio.folio  CLIPPED,' ',
          'AND  estado  =  ',g_provisionada

    PREPARE  query_count   FROM  l_texto_query
    EXECUTE  query_count   INTO  l_registros

    CALL     trae_valor_accion()

    DISPLAY "CTALT07: FOLIO A LIQUIDAR           :",
       reg_tes_ctr_folio.folio              USING"#######"
    DISPLAY "CTALT07: FECHA DE LIQUIDACION       :",
       reg_tes_ctr_folio.fecha_liquidacion  USING"DD-MM-YYYY"
    DISPLAY "CTALT07: REGISTROS A PROCESAR       :",
       l_registros                          USING "######"
        
    DISPLAY  "CTALT07: PROCESANDO FOLIO ", 
       reg_tes_ctr_folio.folio              USING"#######"

    CALL    liquida()

    DISPLAY "CTALT07: TERMINA INDIVIDUALIZACION DE TRASPASO SIE X EDAD "

    UPDATE cta_ctrl_tes_ied_liq
    SET    estado1      = "OK"
    WHERE  tipo_proceso = 2
    AND    fecha        = fecha_valuacion 

    DISPLAY "CTALT07: INICIA DESMARCA DE CUENTAS  "

    UPDATE cta_ctrl_tes_ied_liq
    SET    accion2      = "OK"
    WHERE  tipo_proceso = 2
    AND    fecha        = fecha_valuacion 

    CALL    desmarca()

    DISPLAY "CTALT07: FIN DESMARCA DE CUENTAS  "

    UPDATE cta_ctrl_tes_ied_liq
    SET    estado2      = "OK"
    WHERE  tipo_proceso = 2
    AND    fecha        = fecha_valuacion 

    UPDATE  tes_ctr_folio
       SET  tes_ctr_folio.estado = g_liquidada
     WHERE  tes_ctr_folio.folio  = reg_tes_ctr_folio.folio
       AND  tes_ctr_folio.estado = g_provisionada

    UPDATE  tes_solicitud
       SET  estado         = g_liquidada ,
            fecha_traspaso = reg_tes_ctr_folio.fecha_liquidacion
     WHERE  folio          = reg_tes_ctr_folio.folio
       AND  estado         = g_provisionada

    DISPLAY "CTALT07: INICIA GENERACION DE REPORTE DE CIFRAS DE LIQUIDACION  "

    UPDATE cta_ctrl_tes_ied_liq
    SET    accion3      = "OK"
    WHERE  tipo_proceso = 2
    AND    fecha        = fecha_valuacion 

    CALL    asigna_globales()

    CALL    genera_reporte()

    DISPLAY "CTALT07: FINALIZA GENERACION DE REPORTE DE CIFRAS DE LIQUIDACION  "

    UPDATE cta_ctrl_tes_ied_liq
    SET    estado3      = "OK"
    WHERE  tipo_proceso = 2
    AND    fecha        = fecha_valuacion 

            DISPLAY "CTALT07: REGISTROS PROCESADOS       :",
               g_procesados USING "#######"

            DISPLAY "CTALT07: LIQUIDACION FOLIO ",
               reg_tes_ctr_folio.folio USING"#######", " CONCLUIDA"
        
            DISPLAY "---------------------------------------------------" 
            DISPLAY ""

END FUNCTION
{
FUNCTION liquida()
#l----------------

    DEFINE  l_nss                 CHAR(11)
    DEFINE  l_texto_query         CHAR(500)
    DEFINE  l_texto_liq           CHAR(500)
    DEFINE  i_subct               SMALLINT
    DEFINE  verifica_liq          SMALLINT
    DEFINE  l_tot_subcuentas      SMALLINT

    LET l_texto_liq = ' EXECUTE FUNCTION fn_liquida(?,?,?,?,?) '
    PREPARE qry_liq             FROM  l_texto_liq

    LET     g_procesados          = 0

    LET     l_texto_query  =  ' SELECT  unique nss ',
         ' FROM  ', g_nom_tab_tes_solicitud CLIPPED ,
         ' WHERE folio = ',reg_tes_ctr_folio.folio      CLIPPED,
         ' AND   estado = ',g_provisionada

    PREPARE   query_ced            FROM  l_texto_query
    DECLARE   cur_ced       CURSOR  FOR  query_ced

    FOREACH   cur_ced  INTO  l_nss

    LET g_procesados = g_procesados + 1

    LET l_tot_subcuentas = 0

    SELECT count(*) 
    INTO   l_tot_subcuentas 
    FROM   tab_subcuenta a

    FOR i_subct = 1 TO l_tot_subcuentas

    DECLARE cur_liq             CURSOR FOR qry_liq

    FOREACH cur_liq USING reg_tes_ctr_folio.folio               ,
                          l_nss                                 ,
                          i_subct                               ,
                          reg_tes_ctr_folio.fecha_liquidacion   ,
                          hoy
                    INTO  verifica_liq

    END FOREACH

    END FOR

    END FOREACH

END FUNCTION
}

FUNCTION liquida()
#l----------------

   LET g_pdq = "EXECUTE PROCEDURE fn_liquida_tied(?)"
   PREPARE q_pdq FROM g_pdq
   EXECUTE q_pdq USING reg_tes_ctr_folio.folio

END FUNCTION

FUNCTION trae_valor_accion()
#tva----------------------------
DEFINE g_fecha_parti date
DEFINE l_sie  SMALLINT
DEFINE tot_siefores SMALLINT

SELECT COUNT(*) 
INTO   tot_siefores
FROM   tab_siefore_local
where  codigo_siefore not in (0,11)

FOR l_sie = 1 TO  tot_siefores

    SELECT a.precio_del_dia
    INTO   gprecio_acc
    FROM   safre_af:glo_valor_accion a
    WHERE  a.fecha_valuacion = reg_tes_ctr_folio.fecha_liquidacion
      AND  a.codigo_siefore       = l_sie

    IF gprecio_acc      =  0   THEN
       DISPLAY "CTALT07: Program stopped"
       DISPLAY "CTALT07: ERROR NO HAY PRECIO DE ACCION DE HOY :",
               reg_tes_ctr_folio.fecha_liquidacion
       EXIT  PROGRAM
    ELSE
       DISPLAY "CTALT07: PRECIO DE ACCION SIEFORE ",l_sie," :",
           gprecio_acc USING "###.######"
    END IF
END FOR

END FUNCTION

FUNCTION cal_fecha_retro(x_fecha,ciclo)
#cf-------------------------------

    DEFINE cc         SMALLINT
    DEFINE x_fecha    DATE
    DEFINE ciclo      SMALLINT

    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT

    LET sig_fecha  = x_fecha
    LET cc = 1

    WHILE cc <= ciclo

        LET sig_fecha  = sig_fecha - 1

        LET dia_semana = WEEKDAY(sig_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
           CONTINUE WHILE
        ELSE
           SELECT "ok"
           FROM   tab_feriado
           WHERE  feria_fecha = sig_fecha

           IF STATUS <> NOTFOUND THEN
              CONTINUE WHILE
           ELSE
              LET cc = cc + 1
           END IF
        END IF
    END WHILE
    RETURN sig_fecha
END FUNCTION

FUNCTION desmarca()        
#d-----------------

DEFINE vnss         CHAR(011) 
DEFINE vmarca_entra SMALLINT
DEFINE l_tes_solicitud RECORD LIKE tes_solicitud.*

    DECLARE cur_desmarca CURSOR FOR 
    SELECT a.* 
    FROM   tes_solicitud a
    WHERE  a.folio  = reg_tes_ctr_folio.folio
    AND    a.estado = g_provisionada

    FOREACH cur_desmarca INTO l_tes_solicitud.*

           SELECT a.marca_cod 
           INTO   vmarca_entra 
           FROM   tab_grupo_regimen a
           WHERE  a.grupo_regimen = l_tes_solicitud.grupo_regimen 

           LET ejecuta = "EXECUTE PROCEDURE desmarca_cuenta(",
                            "?,?,?,?,?,?)"

           LET ejecuta = ejecuta  CLIPPED

           PREPARE  clausula_spl1   FROM   ejecuta
           EXECUTE  clausula_spl1   USING 
                    l_tes_solicitud.nss             ,
                    vmarca_entra                    ,
                    l_tes_solicitud.folio_solicitud ,
                    g_cero                          ,
                    vmarca_entra                    ,
                    g_usuario
   END FOREACH
END FUNCTION

FUNCTION asigna_globales()
#ag-----------------------

    LET     g_tabname     = 'dis_cuenta b '
    LET     g_folio       = reg_tes_ctr_folio.folio

    SELECT COUNT(*)
    INTO   g_total_cuentas
    FROM   safre_af:tes_solicitud
    WHERE  folio  = reg_tes_ctr_folio.folio
      AND  estado = 103

    LET    g_fecha_accion = reg_tes_ctr_folio.fecha_liquidacion
    LET    g_fecha_parti  = g_fecha_accion

    LET    g_tipo_desc1    = g_reg_tes_tipo_id.descripcion CLIPPED ,
                            ' LIQUIDACION POR TIPO DE TRASPASO'
    LET    g_tipo_desc2    = g_reg_tes_tipo_id.descripcion CLIPPED ,
                            ' LIQUIDACION POR SUBCUENTA'
    LET    g_nombre_programa = "CTALT08" 
    LET    g_tip_rep       ='TL', g_reg_tes_tipo_id.tipo_traspaso USING"##"

END FUNCTION

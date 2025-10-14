DATABASE safre_af 
FUNCTION RETM814(f_nss)
#r813----------------------
    DEFINE vv                ,
           v_row             ,
           t_row             INTEGER

    DEFINE enter             CHAR(001)
    
    DEFINE reg_act_id_tramite RECORD 
        v_row                INTEGER                   ,
        id_tramite           LIKE ret_aporte.id_tramite
    END RECORD 
    
    DEFINE cla_where         CHAR(0600),
           txt_814           CHAR(1000),
           txt_814_2         CHAR(1000),
           f_nss             CHAR(0011)
    
    DEFINE hoy               DATE 
    
    DEFINE lastkey           ,
           band              ,
           i                 ,
           arr_c             ,
           total_pa          ,
           linea             SMALLINT
    
    DEFINE reg_814 RECORD 
           id_tramite        LIKE ret_aporte.id_tramite    ,
           id_aporte         LIKE ret_aporte.id_aporte     ,
           consec_tramite    LIKE ret_aporte.consec_tramite,
           periodo_pago      LIKE ret_aporte.periodo_pago  ,
           folio_sua         LIKE dis_cuenta.folio_sua     ,
           id_arc            CHAR(001)                     ,
           id_viv            CHAR(001)                     ,
           descripcion       LIKE ret_estado.descripcion  
    END RECORD
    
    DEFINE a_v_row ARRAY[1000] OF RECORD
           v_row             LIKE ret_aporte.id_tramite
    END RECORD
    
    DEFINE a_reg_814 ARRAY[1000] OF RECORD 
           id_tramite        LIKE ret_aporte.id_tramite    ,
           id_aporte         LIKE ret_aporte.id_aporte     ,
           consec_tramite    LIKE ret_aporte.consec_tramite,
           periodo_pago      LIKE ret_aporte.periodo_pago  ,
           folio_sua         LIKE dis_cuenta.folio_sua     ,
           id_arc            CHAR(001)                     ,
           id_viv            CHAR(001)                     ,
           descripcion       LIKE ret_estado.descripcion  
    END RECORD
    
    DEFINE reg_814_1 RECORD
           fecha_conversion  LIKE ret_aporte.fecha_conversion ,
           folio             LIKE ret_aporte.folio            ,
           consecutivo_lote  LIKE ret_aporte.consecutivo_lote ,
           fecha_envio       LIKE ret_extemporanea.fecha_envio,
           importe_rcv       LIKE ret_aporte.importe_rcv      , 
           importe_viv       LIKE ret_aporte.importe_viv97      
    END RECORD
    
    DEFINE a_reg_814_1 ARRAY[100] OF RECORD
           fecha_conversion  LIKE ret_aporte.fecha_conversion ,
           folio             LIKE ret_aporte.folio            ,
           consecutivo_lote  LIKE ret_aporte.consecutivo_lote ,
           fecha_envio       LIKE ret_extemporanea.fecha_envio,
           importe_rcv       LIKE ret_aporte.importe_rcv      , 
           importe_viv       LIKE ret_aporte.importe_viv97       
    END RECORD
    
    
    DEFINE reg_814_2 RECORD 
           n_seguro          LIKE afi_mae_afiliado.n_seguro,
           n_rfc             LIKE afi_mae_afiliado.n_rfc   ,
           n_unico           LIKE afi_mae_afiliado.n_unico ,
           nombre_afiliado   CHAR(45)                      ,
           a_n_rfc           LIKE dis_det_aporte.n_rfc     ,
           a_n_unico         LIKE dis_det_aporte.n_unico   ,
           nombre_aporte     CHAR(45)                
    END RECORD
    
    DEFINE a_reg_814_2 ARRAY[1] OF RECORD 
           n_rfc             LIKE afi_mae_afiliado.n_rfc   ,
           n_unico           LIKE afi_mae_afiliado.n_unico ,
           nombre_afiliado   CHAR(45)                      ,
           a_n_rfc           LIKE dis_det_aporte.n_rfc     ,
           a_n_unico         LIKE dis_det_aporte.n_unico   ,
           nombre_aporte     CHAR(45)                
    END RECORD
    
    
    WHENEVER ERROR CONTINUE
        DROP TABLE ret_act_id_tramite
    WHENEVER ERROR STOP
    
    CREATE TEMP TABLE ret_act_id_tramite (v_row      INTEGER ,
                                          id_tramite SMALLINT)
    
    LET hoy = TODAY
    
    OPEN WINDOW retm8141 AT 2,2 WITH FORM "RETM8141" ATTRIBUTE(BORDER) 
    DISPLAY "                           <Ctrl-C> SALIR                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETM814            TRABAJADORES CON APORTACION EXTEMPORANEA                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING"DD-MM-YY" AT 3,70 ATTRIBUTE(REVERSE)
    
    
    CALL dibuja_pantalla(6,9,18)
    CALL dibuja_pantalla(6,9,37)
    CALL dibuja_pantalla(11,15,9)
    CALL dibuja_pantalla(11,15,20)
    CALL dibuja_pantalla(11,15,32)
    CALL dibuja_pantalla(11,15,44)
    CALL dibuja_pantalla(11,15,54)
    CALL dibuja_pantalla(11,15,61)
    CALL dibuja_pantalla(11,15,68)
    CALL dibuja_pantalla(17,19,13)
    CALL dibuja_pantalla(17,19,22)
    CALL dibuja_pantalla(17,19,35)
    CALL dibuja_pantalla(17,19,48)
    CALL dibuja_pantalla(17,19,64)
    
    DECLARE cur_814 CURSOR FOR
    SELECT a.id_tramite       , 
           a.id_aporte        , 
           a.consec_tramite   , 
           a.periodo_pago     , 
           " "                , 
           " "                , 
           " "                , 
           b.descripcion      ,
           a.fecha_conversion ,
           a.folio            ,
           a.consecutivo_lote ,
           c.fecha_envio      ,
           a.importe_rcv      ,
           a.importe_viv97    ,
           a.rowid
    FROM   ret_aporte a      , 
           ret_estado b      , 
           ret_extemporanea c 
    WHERE  a.nss             = f_nss
    AND    a.nss             = c.nss 
    AND    c.estado_registro = b.estado_solicitud 
    UNION
    SELECT 2                  ,
           0                  ,
           a.consec_tramite   ,
           ""                ,
           ""                ,
           ""                ,
           ""                ,
           c.descripcion      ,
           b.fecha_recepcion  ,
           a.folio            ,
           0                  ,
           b.fecha_envio     ,
           0                  ,
           0                  ,
           0
    FROM   ret_especial a    ,
           ret_cza_especial b,
           ret_estado c
#    WHERE  a.nss             = "32624454776"
    WHERE  a.nss             = f_nss
    AND    a.folio           = b.folio
    AND    c.estado_solicitud= 0
         
    LET i = 1
    
    FOREACH cur_814 INTO reg_814.*  ,
                         reg_814_1.*,
                         v_row
    
        LET a_reg_814[i].*   = reg_814.*
        LET a_reg_814_1[i].* = reg_814_1.*
        LET a_v_row[i].v_row = v_row
        LET i                = i + 1
          
    END FOREACH
    
    IF i = 1 THEN
        DISPLAY "  NO SE ENCONTRARON REGISTROS ...  " AT 21,2 
        SLEEP 2
    END IF
    
    CALL SET_COUNT(i-1)
     
    LET INT_FLAG = FALSE
    
    INPUT ARRAY a_reg_814 WITHOUT DEFAULTS FROM scr_814.*
        BEFORE ROW
            LET arr_c    = ARR_CURR()
            LET total_pa = ARR_COUNT()
            LET linea    = SCR_LINE()
            
            IF arr_c > total_pa THEN
               LET band = TRUE
               EXIT INPUT 
            END IF
            
            DISPLAY a_reg_814[arr_c].id_tramite
            TO scr_814[linea].id_tramite ATTRIBUTE(REVERSE)
            
            DISPLAY a_reg_814[arr_c].id_aporte
            TO scr_814[linea].id_aporte ATTRIBUTE(REVERSE) 
            
            DISPLAY a_reg_814[arr_c].consec_tramite
            TO scr_814[linea].consec_tramite ATTRIBUTE(REVERSE) 
            
            DISPLAY a_reg_814[arr_c].periodo_pago
            TO scr_814[linea].periodo_pago ATTRIBUTE(REVERSE) 
            
            DISPLAY a_reg_814[arr_c].folio_sua
            TO scr_814[linea].folio_sua ATTRIBUTE(REVERSE) 
            
            DISPLAY a_reg_814[arr_c].id_arc
            TO scr_814[linea].id_arc ATTRIBUTE(REVERSE) 
            
            DISPLAY a_reg_814[arr_c].id_viv
            TO scr_814[linea].id_viv ATTRIBUTE(REVERSE) 
            
            DISPLAY a_reg_814[arr_c].descripcion
            TO scr_814[linea].descripcion ATTRIBUTE(REVERSE) 
            
            DISPLAY a_reg_814_1[arr_c].* TO scr_814_1.*
            
            LET txt_814_2 =
                ' SELECT a.n_seguro    , ',
                       ' a.n_rfc       , ',
                       ' a.n_unico     , ',
                       ' TRIM(a.paterno) || " " || TRIM(a.materno) || " " || TRIM(nombres), ',
                       ' b.n_rfc       , ',
                       ' b.n_unico     , ',
                       ' REPLACE(nom_trabajador,"$"," ") ',
                ' FROM   afi_mae_afiliado a , ',
                       ' dis_det_aporte   b   ',
                ' WHERE  a.n_seguro        = ','"',f_nss,'"'                       , 
                ' AND    a.n_seguro        = b.n_seguro '                          ,
                ' AND    b.folio           = ',a_reg_814_1[arr_c].folio            ,
                ' AND    b.consec_reg_lote = ',a_reg_814_1[arr_c].consecutivo_lote ,
                ' AND    b.periodo_pago    = ',a_reg_814[arr_c].periodo_pago 
            
            PREPARE qry_814_2 FROM txt_814_2
            EXECUTE qry_814_2 INTO reg_814_2.*
            DISPLAY BY NAME reg_814_2.*
    
    AFTER FIELD id_tramite 
       
        IF (a_reg_814[arr_c].id_tramite < 0 OR
            a_reg_814[arr_c].id_tramite > 1    ) THEN 
        
            ERROR"    VALOR INVALIDO TECLEAR 0/1  "
            NEXT FIELD id_tramite
    
        ELSE
             LET t_row = 0
    
             SELECT count(*)
             INTO   t_row
             FROM   ret_act_id_tramite a
             WHERE  a.v_row = a_v_row[arr_c].v_row
    
             IF t_row = 0 THEN
                INSERT INTO safre_af:ret_act_id_tramite 
                VALUES (a_v_row[arr_c].v_row,a_reg_814[arr_c].id_tramite)
             ELSE 
                UPDATE safre_af:ret_act_id_tramite
                SET    safre_af:ret_act_id_tramite.id_tramite = a_reg_814[arr_c].id_tramite
                WHERE  safre_af:ret_act_id_tramite.v_row      = a_v_row[arr_c].v_row
             END IF
    
        END IF
        
        IF arr_c >= (total_pa) THEN
    
             LET t_row = 0
    
             SELECT count(*)
             INTO   t_row
             FROM   ret_act_id_tramite
             WHERE  v_row = a_v_row[total_pa].v_row
    
             IF (t_row = 0  OR  t_row IS NULL)  THEN
    
                INSERT INTO safre_af:ret_act_id_tramite 
                VALUES (a_v_row[total_pa].v_row       ,
                        a_reg_814[total_pa].id_tramite)
             ELSE 
                UPDATE safre_af:ret_act_id_tramite
                SET    safre_af:ret_act_id_tramite.id_tramite = a_reg_814[total_pa].id_tramite
                WHERE  safre_af:ret_act_id_tramite.v_row      = a_v_row[total_pa].v_row
      
             END IF
    
           LET lastkey = FGL_LASTKEY()
             IF ((lastkey = FGL_KEYVAL("down"))
              OR (lastkey = FGL_KEYVAL("return"))
              OR (lastkey = FGL_KEYVAL("tab"))
              OR (lastkey = FGL_KEYVAL("right")))
             THEN 
               ERROR "  NP HAY MAS OPCIONES EN ESA DIRECCION  "
               NEXT FIELD id_tramite
             END IF
        END IF
    
    AFTER ROW
        LET arr_c = ARR_CURR()
        LET linea = SCR_LINE()
    
        DISPLAY a_reg_814[arr_c].* TO scr_814[linea].*
    
    ON KEY (INTERRUPT)
       EXIT INPUT
    
    
    ON KEY (ESC)
        WHILE TRUE
            PROMPT "  DESEA CONSERVAR LAS MODIFICACIONES S/N ?  " FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN
    
                 DECLARE cur_1 CURSOR FOR 
                   SELECT a.*
                   FROM   ret_act_id_tramite a
                   
                   FOREACH cur_1 INTO reg_act_id_tramite.*
                       UPDATE ret_aporte
                       SET    id_tramite = reg_act_id_tramite.id_tramite
                       WHERE  rowid      = reg_act_id_tramite.v_row
                   END FOREACH
    
                   EXIT WHILE
                ELSE
                    DISPLAY "  PROCESO CANCELADO...  " AT 21,2  SLEEP 2
                    EXIT INPUT
                END IF
            END IF
        END WHILE
        DISPLAY "  ACTUALIZACION CONCLUIDA...  " AT 21,2
        SLEEP 2
        EXIT INPUT
    
    END INPUT
    
    CLOSE WINDOW retm8141

END FUNCTION

